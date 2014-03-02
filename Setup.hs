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

import Data.List (partition,stripPrefix,foldl')
import Data.Maybe (mapMaybe)


type RubyVersion = (Int, Int, Int)
data RubyInfo = RubyInfo { rbVersion     :: RubyVersion
                         , rbInstallName :: String
                         , rbIncludes    :: [String]
                         , rbLib         :: String
                         , rbSoName      :: String
                         , rbLibName     :: String
                         } deriving (Eq, Show, Read)

evalRuby :: String            -- expression to evaluate
         -> IO (Maybe String) -- stdout, if successfull
evalRuby exp = do
    (exitCode, out, err) <- readProcessWithExitCode "ruby" ["-e", exp] ""
    return $ if exitCode == ExitSuccess
               then Just out
               else Nothing

getRubyInfo :: IO (Maybe RubyInfo)
getRubyInfo = do
    version     <- evalRuby "print '('+RUBY_VERSION.gsub('.', ',')+')'" >>= (return . fmap read)
    installName <- evalRuby "print RbConfig::CONFIG['RUBY_INSTALL_NAME']"
    headerDir   <- evalRuby "print RbConfig::CONFIG['rubyhdrdir']"
    archDir     <- evalRuby "print RbConfig::CONFIG['rubyhdrdir'] + File::Separator + RbConfig::CONFIG['arch']"
    libDir      <- evalRuby "print RbConfig::CONFIG['libdir']"
    soName      <- evalRuby "print RbConfig::CONFIG['LIBRUBY_SO']"
    libName     <- evalRuby "print RbConfig::CONFIG['LIBRUBY_SO'].sub(/^lib/,'').sub(/\\.(so|dll|dylib)$/,'')"
    return $ RubyInfo <$> version
                      <*> installName
                      <*> sequence [headerDir, archDir]
                      <*> libDir
                      <*> soName
                      <*> libName

defsFor :: RubyInfo -> [String]
defsFor info =
    case rbVersion info of
        (1, _, _) -> []
        (2, 0, _) -> ["-DRUBY2"]
        (2, _, _) -> ["-DRUBY2", "-DRUBY21"]

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
        Just (info) ->
            let buildinfos = getBuildInfo h
                bi = buildinfos { extraLibs    = rbLibName info : extraLibs buildinfos
                                , extraLibDirs = rbLib info : extraLibDirs buildinfos
                                , includeDirs  = includeDirs buildinfos ++ rbIncludes info
                                , ccOptions    = ccOptions buildinfos ++ defsFor info
                                }
                in putStrLn ("Detected ruby: " ++ show info) >> return (setBuildInfo h bi)
        _ -> warn normal can'tFindRuby >> return h

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
