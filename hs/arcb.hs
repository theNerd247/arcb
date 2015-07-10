{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Yaml as YAML
import Control.Applicative
import Data.Typeable
import Data.Data
import GHC.Generics
import System.Directory
import System.Process

data Dependency = Dependency
  {
  -- | file path to install the dependency. If the dependency is a project then
  -- simply use the project name
  storageDir :: FilePath
  -- | The url to the server location of the dependency 
  ,url     :: String
  -- | the branch or tag from which to pull.
  ,branch     :: String
  } deriving (Data,Typeable,Generic,Show)

instance YAML.ToJSON Dependency
instance YAML.FromJSON Dependency

-- | TODO: change this to by system agnostic (preferably using the gitlib2)
pullRepo :: Dependency -> IO ()
pullRepo dep = do
  system $ "cd "
    ++ (storageDir dep)
    ++ "; git pull"
  return ()
  

-- | TODO: change this to by system agnostic (preferably using the gitlib2)
cloneRepo :: Dependency -> IO ()
cloneRepo dep = do
  system $ "git clone -b" 
    ++ (branch dep ) 
    ++ " " ++ (url dep) 
    ++ " " ++ (storageDir dep)
  return ()

installDep :: Dependency -> IO ()
installDep dep = do
  exists <- doesDirectoryExist $ storageDir dep
  runGitCommand exists
    where
      runGitCommand e = case e of
          True -> pullRepo dep
          False -> cloneRepo dep

installDeps :: [Dependency] -> IO ()
installDeps = sequence_ . fmap installDep 

configFilePath = "dependency.yaml"

main = do
  parseOut <- YAML.decodeFileEither configFilePath
  either printParseError installDeps parseOut
    where
      printParseError = putStrLn . YAML.prettyPrintParseException

