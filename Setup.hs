import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import System.Environment
import Data.List (partition,stripPrefix,foldl')
import Data.Maybe (mapMaybe)

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

validflags :: [String]
validflags = ["ruby18", "ruby19", "ruby20", "ruby21"]

can'tFindRuby :: String
can'tFindRuby = unlines $ [ "Could not find the ruby library. Ensure that it is present on your system (on Debian/Ubuntu, make sure you installed the ruby1.8-dev package)."
                          , "If you know it to be installed, please install hruby in the following way (example for nix):"
                          , ""
                          , "$ cabal install hruby -p --configure-option=\"--rubyversion=19 --rubylib=ruby --rubyinc=/nix/store/v0w14mdpcy9c0qwvhqa7154qsv53ifqn-ruby-1.9.3-p484/include/ruby-1.9.1 --rubyinc=/nix/store/v0w14mdpcy9c0qwvhqa7154qsv53ifqn-ruby-1.9.3-p484/include/ruby-1.9.1/x86_64-linux' --extra-lib-dirs=$HOME/.nix-profile/lib/\""
                          , ""
                          , " --rubylib : Should be the name of the library passed to the linker (ruby for libruby.so)."
                          , " --rubyinc : There can be several instances of this flag. Should be the path of the various ruby header files."
                          , " --rubyversion : Mandatory for ruby 2.0 and 2.1, should have the values 20 or 21."
                          ]

type InstallInfo = (String, [String], [String])

inc18,inc19,inc20,inc21 :: [String]
inc21 = ["/usr/include/ruby-2.1.0", "/usr/include/x86_64-linux-gnu/ruby-2.1.0", "/usr/include/ruby-2.1.0/x86_64-linux"]
inc20 = ["/usr/include/ruby-2.0.0", "/usr/include/x86_64-linux-gnu/ruby-2.0.0", "/usr/include/ruby-2.0.0/x86_64-linux"]
inc19 = ["/usr/lib/ruby/1.9/x86_64-linux"]
inc18 = ["/usr/lib/ruby/1.8/x86_64-linux"]

r18,r19,r20,r21 :: InstallInfo
r21 = ("ruby2.1", inc21, ["-DRUBY2","-DRUBY21"])
r20 = ("ruby2.0", inc20, ["-DRUBY2"])
r19 = ("ruby1.9", inc19, [])
r18 = ("ruby1.8", inc18, [])

defaultLib :: InstallInfo -> InstallInfo
defaultLib (_,i,c) = ("ruby",i,c)

myConfHook :: LocalBuildInfo -> IO LocalBuildInfo
myConfHook h = do
    -- hunt for libraries
    let rubies = ["ruby2.1", "ruby2.0", "ruby1.9", "ruby1.8", "ruby"]
        pathes = ["/usr/lib", "/usr/local/lib"]
    f <- findFirstFile (\(p,r) -> p ++ "/lib" ++ r ++ ".so") [(p,r) | p <- pathes, r <- rubies]
    let rubystring = maybe "" snd f
        lg s = notice normal ("Auto detected " ++ s)
    case fmap snd f of
        Just "ruby2.1" -> hookWith r21 h
        Just "ruby2.0" -> hookWith r20 h
        Just "ruby1.9" -> hookWith r19 h
        Just "ruby1.8" -> hookWith r18 h
        Just "ruby" -> do
            is21 <- findFirstFile id inc21
            is20 <- findFirstFile id inc20
            is19 <- findFirstFile id inc19
            is18 <- findFirstFile id inc18
            case filter ((/= Nothing) . fst) [(is21,r21), (is20,r20), (is19,r19), (is18,r18)] of
                ((_,(n,inc,flgs)):_) -> hookWith ("ruby",inc,flgs) h
                _ -> warn normal can'tFindRuby >> return h
        Just x -> die $ "Did find this (this should not happen): " ++ x
        _ -> warn normal can'tFindRuby >> return h

hookWith (e,i,c) h =
    let buildinfos = getBuildInfo h
        bi = buildinfos { extraLibs = e : extraLibs buildinfos
                        , includeDirs = includeDirs buildinfos ++ i
                        , ccOptions = ccOptions buildinfos ++ c
                        }
    in putStrLn ("Detected parameters: " ++ show (e,i,c)) >> return (setBuildInfo h bi)

parseFlags :: [String] -> LocalBuildInfo -> IO LocalBuildInfo
parseFlags flags h = return $ setBuildInfo h $ foldl' parseFlags' bbi flags
    where
        bbi = getBuildInfo h
        parseFlags' bi fl = case (stripPrefix "--rubylib=" fl, stripPrefix "--rubyinc=" fl, stripPrefix "--rubyversion=" fl) of
                                (Just l, _, _)    -> bi { extraLibs   = l : extraLibs bi }
                                (_, Just l, _)    -> bi { includeDirs = l : includeDirs bi }
                                (_, _, Just "20") -> bi { ccOptions   = "-DRUBY2" : ccOptions bi }
                                (_, _, Just "21") -> bi { ccOptions   = "-DRUBY2" : "-DRUBY21" : ccOptions bi }
                                _                 -> bi

main :: IO ()
main = do
    args <- getArgs
    let flags = mapMaybe (stripPrefix "--configure-option=") args
    let hook = if null flags
                   then myConfHook
                   else parseFlags (concatMap words flags)
    defaultMainWithHooks $ simpleUserHooks { confHook = (\a b -> configure a b >>= hook) }

