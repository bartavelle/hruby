import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import System.Environment

getBuildInfo :: LocalBuildInfo -> BuildInfo
getBuildInfo l = case library (localPkgDescr l) of
                     Just x -> libBuildInfo x
                     Nothing -> error "Could not find the buildinfo!"

setBuildInfo :: LocalBuildInfo -> BuildInfo -> LocalBuildInfo
setBuildInfo l b = l { localPkgDescr = lpd' }
    where
        lpd = localPkgDescr l
        Just li = library lpd
        li' = li { libBuildInfo = b }
        lpd' = lpd { library = Just li' }

main :: IO ()
main = defaultMainWithHooks myhooks
    where
        myhooks    = simpleUserHooks { confHook = (\a b -> configure a b >>= myConfHook) }
        myConfHook h = do
            let buildinfos = getBuildInfo h
            -- hunt for libraries
            let rubies = ["ruby2.1", "ruby2.0", "ruby1.9", "ruby1.8", "ruby"]
                pathes = ["/usr/lib", "/usr/local/lib"]
                inc21 = ["/usr/include/ruby-2.1.0", "/usr/include/x86_64-linux-gnu/ruby-2.1.0", "/usr/include/ruby-2.1.0/x86_64-linux"]
                inc20 = ["/usr/include/ruby-2.0.0", "/usr/include/x86_64-linux-gnu/ruby-2.0.0", "/usr/include/ruby-2.0.0/x86_64-linux"]
                inc19 = ["/usr/lib/ruby/1.9/x86_64-linux"]
                inc18 = ["/usr/lib/ruby/1.8/x86_64-linux"]
                r21 = ("ruby2.1", inc21, ["-DRUBY2","-DRUBY21"])
                r20 = ("ruby2.0", inc20, ["-DRUBY2"])
                r19 = ("ruby1.9", inc19, [])
                r18 = ("ruby1.8", inc18, [])
            f <- findFirstFile (\(p,r) -> p ++ "/lib" ++ r ++ ".so") [(p,r) | p <- pathes, r <- rubies]
            let rubystring = maybe "" snd f
                lg s = notice normal ("Auto detected " ++ s)
            (e,i,c) <- case fmap snd f of
                           Just "ruby2.1" -> lg rubystring >> return r21
                           Just "ruby2.0" -> lg rubystring >> return r20
                           Just "ruby1.9" -> lg rubystring >> return r19
                           Just "ruby1.8" -> lg rubystring >> return r18
                           Just "ruby" -> do
                                is21 <- findFirstFile id inc21
                                is20 <- findFirstFile id inc20
                                is19 <- findFirstFile id inc19
                                is18 <- findFirstFile id inc18
                                case filter ((/= Nothing) . fst) [(is21,r21), (is20,r20), (is19,r19), (is18,r18)] of
                                    ((_,(n,inc,flgs)):_) -> lg n >> return ("ruby",inc,flgs)
                                    _ -> die "Could not find the ruby library"
                           Just x -> die $ "Did find this (this should not happen): " ++ x
                           _ -> die "Could not find the ruby library"
            let bi = buildinfos { extraLibs = e : extraLibs buildinfos
                                , includeDirs = includeDirs buildinfos ++ i
                                , ccOptions = ccOptions buildinfos ++ c
                                }
            return (setBuildInfo h bi)
