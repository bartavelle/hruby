{-# LANGUAGE ScopedTypeVariables #-}
import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription

import System.Environment
import System.Process
import System.Exit

import Control.Applicative
import Control.Exception

import Data.List (partition,stripPrefix,foldl')
import Data.Maybe (mapMaybe)

-- --rubyversion=20 --rubylib=ruby-2.0 --rubyinc=/usr/include/ruby-2.0.0 --rubyinc=/usr/include/x86_64-linux-gnu/ruby-2.0.0
-- {rbVersion = (2,0,0), rbInstallName = "ruby2.0", rbIncludes = ["/usr/include/ruby-2.0.0","/usr/include/ruby-2.0.0/x86_64-linux-gnu"], rbLib = "/usr/lib", rbLibName = "ruby-2.0"}

type RubyVersion = (Int, Int, Int)
data RubyInfo = RubyInfo { rbVersion     :: RubyVersion
                         , rbInstallName :: String
                         , rbIncludes    :: [String]
                         , rbLib         :: String
                         , rbLibName     :: String
                         } deriving (Eq, Show, Read)

evalRuby :: String            -- expression to evaluate
         -> IO (Maybe String) -- stdout, if successfull
evalRuby exp = do
    let getruby [] = return (ExitFailure 3, "beuh", undefined)
        getruby (x:xs) = readProcessWithExitCode x ["-e", exp] "" `catch` \ (_ :: IOException) -> getruby xs
    (exitCode, out, err) <- getruby [ "ruby2.1", "ruby2.0", "ruby2", "ruby1.8", "ruby"]
    return $ if exitCode == ExitSuccess
               then Just out
               else Nothing

getRubyInfo :: IO (Maybe RubyInfo)
getRubyInfo = do
    version     <- fmap (fmap read) (evalRuby "print '('+RUBY_VERSION.gsub('.', ',')+')'")
    case version of
        Nothing -> return Nothing
        Just (1,9,_) -> error $ unlines [ "Ruby 1.9 cannot be integrated with the GHC runtime. Tough luck :("
                                        , "On Ubuntu 14.04, you can try this:"
                                        , "  apt-get install ruby2.0 libruby2.0 ruby2.0-dev"
                                        , "  cabal install hruby -p --configure-option=\"--rubyversion=20 --rubylib=ruby-2.0 --rubyinc=/usr/include/ruby-2.0.0 --rubyinc=/usr/include/x86_64-linux-gnu/ruby-2.0.0\""
                                        ]
        Just v@(1,8,_) -> return $ Just $ RubyInfo v
                                                   "/usr/lib/ruby/1.8"
                                                   ["/usr/lib/ruby/1.8/x86_64-linux","/usr/lib/ruby/1.8/x86_64-linux","/usr/lib64/ruby/1.8/x86_64-linux"]
                                                   "/usr/lib"
                                                   "ruby1.8"
        Just v -> do
            installName <- evalRuby "print RbConfig::CONFIG['RUBY_INSTALL_NAME']"
            headerDir   <- evalRuby "print RbConfig::CONFIG['rubyhdrdir']"
            headerDir'  <- evalRuby "print RbConfig::CONFIG['rubyhdrdir'] + File::Separator + 'ruby'"
            archDir     <- evalRuby "print RbConfig::CONFIG['rubyarchhdrdir']"
            libDir      <- evalRuby "print RbConfig::CONFIG['libdir']"
            td          <- evalRuby "print RbConfig::CONFIG['topdir']"
            libName     <- evalRuby "print RbConfig::CONFIG['LIBRUBY_SO'].sub(/^lib/,'').sub(/\\.(so|dll|dylib)([.0-9]+)?$/,'')"
            return $ RubyInfo <$> pure v
                              <*> installName
                              <*> sequence [headerDir, headerDir', archDir, td]
                              <*> libDir
                              <*> libName

defsFor :: RubyInfo -> [String]
defsFor info =
    case rbVersion info of
        (2, 0, _) -> ["-DRUBY2"]
        (2, _, _) -> ["-DRUBY2", "-DRUBY21"]
        (a, b, _) -> ["-DRUBY" ++ show a ++ show b]

cantFindRuby :: String
cantFindRuby = unlines [ "Could not find the ruby library. Ensure that it is present on your system (on Debian/Ubuntu, make sure you installed the ruby1.8-dev package)."
                       , "If you know it to be installed, please install hruby in the following way (example for nix):"
                       , ""
                       , "$ cabal install hruby -p --configure-option=\"--rubyversion=19 --rubylib=ruby --rubyinc=/nix/store/v0w14mdpcy9c0qwvhqa7154qsv53ifqn-ruby-1.9.3-p484/include/ruby-1.9.1 --rubyinc=/nix/store/v0w14mdpcy9c0qwvhqa7154qsv53ifqn-ruby-1.9.3-p484/include/ruby-1.9.1/x86_64-linux' --extra-lib-dirs=$HOME/.nix-profile/lib/\""
                       , ""
                       , " --rubylib : Should be the name of the library passed to the linker (ruby for libruby.so)."
                       , " --rubyinc : There can be several instances of this flag. Should be the path of the various ruby header files."
                       , " --rubyversion : Mandatory for ruby 2.0 and 2.1, should have the values 20 or 21."
                       ]

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

myConfHook :: LocalBuildInfo -> IO LocalBuildInfo
myConfHook h = do
    mrubyInfo <- getRubyInfo
    case mrubyInfo of
        Just info ->
            let buildinfos = getBuildInfo h
                bi = buildinfos { extraLibs    = [rbLibName info]
                                , extraLibDirs = rbLib info : extraLibDirs buildinfos
                                , includeDirs  = includeDirs buildinfos ++ rbIncludes info
                                , ccOptions    = ccOptions buildinfos ++ defsFor info
                                }
                in putStrLn ("Detected ruby: " ++ show info ++ " cc:" ++ show (ccOptions bi)) >> return (setBuildInfo h bi)
        _ -> warn normal cantFindRuby >> return h

parseFlags :: [String] -> LocalBuildInfo -> IO LocalBuildInfo
parseFlags flags h = return $ setBuildInfo h $ foldl' parseFlags' bbi flags
    where
        bbi = getBuildInfo h
        addOptions (Just "20") bi = bi { ccOptions   = "-DRUBY2" : ccOptions bi }
        addOptions (Just "21") bi = bi { ccOptions   = "-DRUBY2" : "-DRUBY21" : ccOptions bi }
        addOptions (Just x) bi = bi { ccOptions = ("-DRUBY" ++ take 2 x) : ccOptions bi }
        addOptions Nothing bi = bi
        parseFlags' bi fl = addOptions (stripPrefix "--rubyversion=" fl) $ case (stripPrefix "--rubylib=" fl, stripPrefix "--rubyinc=" fl) of
                                        (Just l, _)    -> bi { extraLibs   = l : filter (/= "ruby") (extraLibs bi) }
                                        (_, Just l)    -> bi { includeDirs = l : includeDirs bi }
                                        _              -> bi

main :: IO ()
main = do
    args <- getArgs
    let flags = mapMaybe (stripPrefix "--configure-option=") args
    let hook = if null flags
                   then myConfHook
                   else parseFlags (concatMap words flags)
    defaultMainWithHooks $ simpleUserHooks { confHook = \a b -> configure a b >>= hook }
