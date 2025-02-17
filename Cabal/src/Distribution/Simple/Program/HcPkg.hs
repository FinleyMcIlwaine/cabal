{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Program.HcPkg
-- Copyright   :  Duncan Coutts 2009, 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @hc-pkg@ program.
-- Currently only GHC and GHCJS have hc-pkg programs.
module Distribution.Simple.Program.HcPkg
  ( -- * Types
    HcPkgInfo (..)
  , RegisterOptions (..)
  , defaultRegisterOptions

    -- * Actions
  , init
  , invoke
  , register
  , unregister
  , recache
  , expose
  , hide
  , dump
  , describe
  , list

    -- * Program invocations
  , initInvocation
  , registerInvocation
  , unregisterInvocation
  , recacheInvocation
  , exposeInvocation
  , hideInvocation
  , dumpInvocation
  , describeInvocation
  , listInvocation
  ) where

import Distribution.Compat.Prelude hiding (init)
import Prelude ()

import Distribution.InstalledPackageInfo
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Simple.Compiler
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils
import Distribution.Types.ComponentId
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Verbosity

import Data.List (stripPrefix)
import System.FilePath as FilePath (isPathSeparator, joinPath, splitDirectories, splitPath, (<.>), (</>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified System.FilePath.Posix as FilePath.Posix

-- | Information about the features and capabilities of an @hc-pkg@
--   program.
data HcPkgInfo = HcPkgInfo
  { hcPkgProgram :: ConfiguredProgram
  , noPkgDbStack :: Bool
  -- ^ no package DB stack supported
  , noVerboseFlag :: Bool
  -- ^ hc-pkg does not support verbosity flags
  , flagPackageConf :: Bool
  -- ^ use package-conf option instead of package-db
  , supportsDirDbs :: Bool
  -- ^ supports directory style package databases
  , requiresDirDbs :: Bool
  -- ^ requires directory style package databases
  , nativeMultiInstance :: Bool
  -- ^ supports --enable-multi-instance flag
  , recacheMultiInstance :: Bool
  -- ^ supports multi-instance via recache
  , suppressFilesCheck :: Bool
  -- ^ supports --force-files or equivalent
  }

-- | Call @hc-pkg@ to initialise a package database at the location {path}.
--
-- > hc-pkg init {path}
init :: HcPkgInfo -> Verbosity -> Bool -> FilePath -> IO ()
init hpi verbosity preferCompat path
  | not (supportsDirDbs hpi)
      || (not (requiresDirDbs hpi) && preferCompat) =
      writeFile path "[]"
  | otherwise =
      runProgramInvocation verbosity (initInvocation hpi verbosity path)

-- | Run @hc-pkg@ using a given package DB stack, directly forwarding the
-- provided command-line arguments to it.
invoke :: HcPkgInfo -> Verbosity -> PackageDBStack -> [String] -> IO ()
invoke hpi verbosity dbStack extraArgs =
  runProgramInvocation verbosity invocation
  where
    args = packageDbStackOpts hpi dbStack ++ extraArgs
    invocation = programInvocation (hcPkgProgram hpi) args

-- | Additional variations in the behaviour for 'register'.
data RegisterOptions = RegisterOptions
  { registerAllowOverwrite :: Bool
  -- ^ Allows re-registering \/ overwriting an existing package
  , registerMultiInstance :: Bool
  -- ^ Insist on the ability to register multiple instances of a
  -- single version of a single package. This will fail if the @hc-pkg@
  -- does not support it, see 'nativeMultiInstance' and
  -- 'recacheMultiInstance'.
  , registerSuppressFilesCheck :: Bool
  -- ^ Require that no checks are performed on the existence of package
  -- files mentioned in the registration info. This must be used if
  -- registering prior to putting the files in their final place. This will
  -- fail if the @hc-pkg@ does not support it, see 'suppressFilesCheck'.
  }

-- | Defaults are @True@, @False@ and @False@
defaultRegisterOptions :: RegisterOptions
defaultRegisterOptions =
  RegisterOptions
    { registerAllowOverwrite = True
    , registerMultiInstance = False
    , registerSuppressFilesCheck = False
    }

-- | Call @hc-pkg@ to register a package.
--
-- > hc-pkg register {filename | -} [--user | --global | --package-db]
register
  :: HcPkgInfo
  -> Verbosity
  -> PackageDBStack
  -> InstalledPackageInfo
  -> RegisterOptions
  -> IO ()
register hpi verbosity packagedbs pkgInfo registerOptions
  | registerMultiInstance registerOptions
  , not (nativeMultiInstance hpi || recacheMultiInstance hpi) =
      die' verbosity $
        "HcPkg.register: the compiler does not support "
          ++ "registering multiple instances of packages."
  | registerSuppressFilesCheck registerOptions
  , not (suppressFilesCheck hpi) =
      die' verbosity $
        "HcPkg.register: the compiler does not support "
          ++ "suppressing checks on files."
  -- This is a trick. Older versions of GHC do not support the
  -- --enable-multi-instance flag for ghc-pkg register but it turns out that
  -- the same ability is available by using ghc-pkg recache. The recache
  -- command is there to support distro package managers that like to work
  -- by just installing files and running update commands, rather than
  -- special add/remove commands. So the way to register by this method is
  -- to write the package registration file directly into the package db and
  -- then call hc-pkg recache.
  --
  | registerMultiInstance registerOptions
  , recacheMultiInstance hpi =
      do
        let pkgdb = registrationPackageDB packagedbs
        writeRegistrationFileDirectly verbosity hpi pkgdb pkgInfo
        recache hpi verbosity pkgdb
  | otherwise =
      runProgramInvocation
        verbosity
        (registerInvocation hpi verbosity packagedbs pkgInfo registerOptions)

writeRegistrationFileDirectly
  :: Verbosity
  -> HcPkgInfo
  -> PackageDB
  -> InstalledPackageInfo
  -> IO ()
writeRegistrationFileDirectly verbosity hpi (SpecificPackageDB dir) pkgInfo
  | supportsDirDbs hpi =
      do
        let pkgfile = dir </> prettyShow (installedUnitId pkgInfo) <.> "conf"
        writeUTF8File pkgfile (showInstalledPackageInfo pkgInfo)
  | otherwise =
      die' verbosity $ "HcPkg.writeRegistrationFileDirectly: compiler does not support dir style package dbs"
writeRegistrationFileDirectly verbosity _ _ _ =
  -- We don't know here what the dir for the global or user dbs are,
  -- if that's needed it'll require a bit more plumbing to support.
  die' verbosity $ "HcPkg.writeRegistrationFileDirectly: only supports SpecificPackageDB for now"

-- | Call @hc-pkg@ to unregister a package
--
-- > hc-pkg unregister [pkgid] [--user | --global | --package-db]
unregister :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId -> IO ()
unregister hpi verbosity packagedb pkgid =
  runProgramInvocation
    verbosity
    (unregisterInvocation hpi verbosity packagedb pkgid)

-- | Call @hc-pkg@ to recache the registered packages.
--
-- > hc-pkg recache [--user | --global | --package-db]
recache :: HcPkgInfo -> Verbosity -> PackageDB -> IO ()
recache hpi verbosity packagedb =
  runProgramInvocation
    verbosity
    (recacheInvocation hpi verbosity packagedb)

-- | Call @hc-pkg@ to expose a package.
--
-- > hc-pkg expose [pkgid] [--user | --global | --package-db]
expose :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId -> IO ()
expose hpi verbosity packagedb pkgid =
  runProgramInvocation
    verbosity
    (exposeInvocation hpi verbosity packagedb pkgid)

-- | Call @hc-pkg@ to retrieve a specific package
--
-- > hc-pkg describe [pkgid] [--user | --global | --package-db]
describe :: HcPkgInfo -> Verbosity -> PackageDBStack -> PackageId -> IO [InstalledPackageInfo]
describe hpi verbosity packagedb pid = do
  output <-
    getProgramInvocationLBS
      verbosity
      (describeInvocation hpi verbosity packagedb pid)
      `catchIO` \_ -> return mempty

  case parsePackages output of
    Left ok -> return ok
    _ ->
      die' verbosity $
        "failed to parse output of '"
          ++ programId (hcPkgProgram hpi)
          ++ " describe "
          ++ prettyShow pid
          ++ "'"

-- | Call @hc-pkg@ to hide a package.
--
-- > hc-pkg hide [pkgid] [--user | --global | --package-db]
hide :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId -> IO ()
hide hpi verbosity packagedb pkgid =
  runProgramInvocation
    verbosity
    (hideInvocation hpi verbosity packagedb pkgid)

-- | Call @hc-pkg@ to get all the details of all the packages in the given
-- package database.
dump :: HcPkgInfo -> Verbosity -> PackageDB -> IO [InstalledPackageInfo]
dump hpi verbosity packagedb = do
  output <-
    getProgramInvocationLBS
      verbosity
      (dumpInvocation hpi verbosity packagedb)
      `catchIO` \e ->
        die' verbosity $
          programId (hcPkgProgram hpi)
            ++ " dump failed: "
            ++ displayException e

  case parsePackages output of
    Left ok -> return ok
    _ ->
      die' verbosity $
        "failed to parse output of '"
          ++ programId (hcPkgProgram hpi)
          ++ " dump'"

parsePackages :: LBS.ByteString -> Either [InstalledPackageInfo] [String]
parsePackages lbs0 =
  case traverse parseInstalledPackageInfo $ splitPkgs lbs0 of
    Right ok -> Left [setUnitId . maybe id mungePackagePaths (pkgRoot pkg) $ pkg | (_, pkg) <- ok]
    Left msgs -> Right (NE.toList msgs)
  where
    splitPkgs :: LBS.ByteString -> [BS.ByteString]
    splitPkgs = checkEmpty . doSplit
      where
        -- Handle the case of there being no packages at all.
        checkEmpty [s] | BS.all isSpace8 s = []
        checkEmpty ss = ss

        isSpace8 :: Word8 -> Bool
        isSpace8 9 = True -- '\t'
        isSpace8 10 = True -- '\n'
        isSpace8 13 = True -- '\r'
        isSpace8 32 = True -- ' '
        isSpace8 _ = False

        doSplit :: LBS.ByteString -> [BS.ByteString]
        doSplit lbs = go (LBS.findIndices (\w -> w == 10 || w == 13) lbs)
          where
            go :: [Int64] -> [BS.ByteString]
            go [] = [LBS.toStrict lbs]
            go (idx : idxs) =
              let (pfx, sfx) = LBS.splitAt idx lbs
               in case foldr (<|>) Nothing $ map (`lbsStripPrefix` sfx) separators of
                    Just sfx' -> LBS.toStrict pfx : doSplit sfx'
                    Nothing -> go idxs

            separators :: [LBS.ByteString]
            separators = ["\n---\n", "\r\n---\r\n", "\r---\r"]

lbsStripPrefix :: LBS.ByteString -> LBS.ByteString -> Maybe LBS.ByteString
#if MIN_VERSION_bytestring(0,10,8)
lbsStripPrefix pfx lbs = LBS.stripPrefix pfx lbs
#else
lbsStripPrefix pfx lbs
    | LBS.isPrefixOf pfx lbs = Just (LBS.drop (LBS.length pfx) lbs)
    | otherwise              = Nothing
#endif

mungePackagePaths :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
mungePackagePaths pkgroot pkginfo =
  pkginfo
    { importDirs = mungePaths (importDirs pkginfo)
    , includeDirs = mungePaths (includeDirs pkginfo)
    , libraryDirs = mungePaths (libraryDirs pkginfo)
    , libraryDirsStatic = mungePaths (libraryDirsStatic pkginfo)
    , libraryDynDirs = mungePaths (libraryDynDirs pkginfo)
    , frameworkDirs = mungePaths (frameworkDirs pkginfo)
    , haddockInterfaces = mungePaths (haddockInterfaces pkginfo)
    , haddockHTMLs = mungeUrls (haddockHTMLs pkginfo)
    }
  where
    mungePaths = map mungePath
    mungeUrls = map mungeUrl

    mungePath p = case stripVarPrefix "${pkgroot}" p of
      Just p' -> pkgroot </> p'
      Nothing -> p

    mungeUrl p = case stripVarPrefix "${pkgrooturl}" p of
      Just p' -> toUrlPath pkgroot p'
      Nothing -> p

    toUrlPath r p =
      "file:///"
        -- URLs always use posix style '/' separators:
        ++ FilePath.Posix.joinPath (r : FilePath.splitDirectories p)

    stripVarPrefix var p =
      case splitPath p of
        (root : path') -> case stripPrefix var root of
          Just [sep] | isPathSeparator sep -> Just (joinPath path')
          _ -> Nothing
        _ -> Nothing

-- Older installed package info files did not have the installedUnitId
-- field, so if it is missing then we fill it as the source package ID.
-- NB: Internal libraries not supported.
setUnitId :: InstalledPackageInfo -> InstalledPackageInfo
setUnitId
  pkginfo@InstalledPackageInfo
    { installedUnitId = uid
    , sourcePackageId = pid
    }
    | unUnitId uid == "" =
        pkginfo
          { installedUnitId = mkLegacyUnitId pid
          , installedComponentId_ = mkComponentId (prettyShow pid)
          }
setUnitId pkginfo = pkginfo

-- | Call @hc-pkg@ to get the source package Id of all the packages in the
-- given package database.
--
-- This is much less information than with 'dump', but also rather quicker.
-- Note in particular that it does not include the 'UnitId', just
-- the source 'PackageId' which is not necessarily unique in any package db.
list
  :: HcPkgInfo
  -> Verbosity
  -> PackageDB
  -> IO [PackageId]
list hpi verbosity packagedb = do
  output <-
    getProgramInvocationOutput
      verbosity
      (listInvocation hpi verbosity packagedb)
      `catchIO` \_ -> die' verbosity $ programId (hcPkgProgram hpi) ++ " list failed"

  case parsePackageIds output of
    Just ok -> return ok
    _ ->
      die' verbosity $
        "failed to parse output of '"
          ++ programId (hcPkgProgram hpi)
          ++ " list'"
  where
    parsePackageIds = traverse simpleParsec . words

--------------------------
-- The program invocations
--

initInvocation :: HcPkgInfo -> Verbosity -> FilePath -> ProgramInvocation
initInvocation hpi verbosity path =
  programInvocation (hcPkgProgram hpi) args
  where
    args =
      ["init", path]
        ++ verbosityOpts hpi verbosity

registerInvocation
  :: HcPkgInfo
  -> Verbosity
  -> PackageDBStack
  -> InstalledPackageInfo
  -> RegisterOptions
  -> ProgramInvocation
registerInvocation hpi verbosity packagedbs pkgInfo registerOptions =
  (programInvocation (hcPkgProgram hpi) (args "-"))
    { progInvokeInput = Just $ IODataText $ showInstalledPackageInfo pkgInfo
    , progInvokeInputEncoding = IOEncodingUTF8
    }
  where
    cmdname
      | registerAllowOverwrite registerOptions = "update"
      | registerMultiInstance registerOptions = "update"
      | otherwise = "register"

    args file =
      [cmdname, file]
        ++ packageDbStackOpts hpi packagedbs
        ++ [ "--enable-multi-instance"
           | registerMultiInstance registerOptions
           ]
        ++ [ "--force-files"
           | registerSuppressFilesCheck registerOptions
           ]
        ++ verbosityOpts hpi verbosity

unregisterInvocation
  :: HcPkgInfo
  -> Verbosity
  -> PackageDB
  -> PackageId
  -> ProgramInvocation
unregisterInvocation hpi verbosity packagedb pkgid =
  programInvocation (hcPkgProgram hpi) $
    ["unregister", packageDbOpts hpi packagedb, prettyShow pkgid]
      ++ verbosityOpts hpi verbosity

recacheInvocation
  :: HcPkgInfo
  -> Verbosity
  -> PackageDB
  -> ProgramInvocation
recacheInvocation hpi verbosity packagedb =
  programInvocation (hcPkgProgram hpi) $
    ["recache", packageDbOpts hpi packagedb]
      ++ verbosityOpts hpi verbosity

exposeInvocation
  :: HcPkgInfo
  -> Verbosity
  -> PackageDB
  -> PackageId
  -> ProgramInvocation
exposeInvocation hpi verbosity packagedb pkgid =
  programInvocation (hcPkgProgram hpi) $
    ["expose", packageDbOpts hpi packagedb, prettyShow pkgid]
      ++ verbosityOpts hpi verbosity

describeInvocation
  :: HcPkgInfo
  -> Verbosity
  -> PackageDBStack
  -> PackageId
  -> ProgramInvocation
describeInvocation hpi verbosity packagedbs pkgid =
  programInvocation (hcPkgProgram hpi) $
    ["describe", prettyShow pkgid]
      ++ packageDbStackOpts hpi packagedbs
      ++ verbosityOpts hpi verbosity

hideInvocation
  :: HcPkgInfo
  -> Verbosity
  -> PackageDB
  -> PackageId
  -> ProgramInvocation
hideInvocation hpi verbosity packagedb pkgid =
  programInvocation (hcPkgProgram hpi) $
    ["hide", packageDbOpts hpi packagedb, prettyShow pkgid]
      ++ verbosityOpts hpi verbosity

dumpInvocation :: HcPkgInfo -> Verbosity -> PackageDB -> ProgramInvocation
dumpInvocation hpi _verbosity packagedb =
  (programInvocation (hcPkgProgram hpi) args)
    { progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args =
      ["dump", packageDbOpts hpi packagedb]
        ++ verbosityOpts hpi silent

-- We use verbosity level 'silent' because it is important that we
-- do not contaminate the output with info/debug messages.

listInvocation :: HcPkgInfo -> Verbosity -> PackageDB -> ProgramInvocation
listInvocation hpi _verbosity packagedb =
  (programInvocation (hcPkgProgram hpi) args)
    { progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args =
      ["list", "--simple-output", packageDbOpts hpi packagedb]
        ++ verbosityOpts hpi silent

-- We use verbosity level 'silent' because it is important that we
-- do not contaminate the output with info/debug messages.

packageDbStackOpts :: HcPkgInfo -> PackageDBStack -> [String]
packageDbStackOpts hpi dbstack
  | noPkgDbStack hpi = [packageDbOpts hpi (registrationPackageDB dbstack)]
  | otherwise = case dbstack of
      (GlobalPackageDB : UserPackageDB : dbs) ->
        "--global"
          : "--user"
          : map specific dbs
      (GlobalPackageDB : dbs) ->
        "--global"
          : ("--no-user-" ++ packageDbFlag hpi)
          : map specific dbs
      _ -> ierror
  where
    specific (SpecificPackageDB db) = "--" ++ packageDbFlag hpi ++ "=" ++ db
    specific _ = ierror
    ierror :: a
    ierror = error ("internal error: unexpected package db stack: " ++ show dbstack)

packageDbFlag :: HcPkgInfo -> String
packageDbFlag hpi
  | flagPackageConf hpi =
      "package-conf"
  | otherwise =
      "package-db"

packageDbOpts :: HcPkgInfo -> PackageDB -> String
packageDbOpts _ GlobalPackageDB = "--global"
packageDbOpts _ UserPackageDB = "--user"
packageDbOpts hpi (SpecificPackageDB db) = "--" ++ packageDbFlag hpi ++ "=" ++ db

verbosityOpts :: HcPkgInfo -> Verbosity -> [String]
verbosityOpts hpi v
  | noVerboseFlag hpi =
      []
  | v >= deafening = ["-v2"]
  | v == silent = ["-v0"]
  | otherwise = []
