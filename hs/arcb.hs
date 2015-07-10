{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Yaml as YAML
import Control.Applicative
import Data.Typeable
import Data.Data
import GHC.Generics
import System.Process

data Dependency = Dependency
  {
  -- | file path to install the dependency. If the dependency is a project then
	-- simply use the project name
  storageDir :: FilePath
  -- | The url to the server location of the dependency (currently only git is
  -- supported)
  ,url     :: String
  -- | the branch or tag from which to pull.
  ,branch     :: String
  } deriving (Data,Typeable,Generic,Show)

instance YAML.ToJSON Dependency
instance YAML.FromJSON Dependency

installDep :: Dependency -> IO ()
installDep = undefined

installDeps :: [Dependency] -> IO ()
installDeps = sequence_ . fmap installDep 

configFilePath = "dependencies.yaml"

main = do
  parseOut <- YAML.decodeFileEither configFilePath
  either printParseError installDeps parseOut
    where
      printParseError = putStrLn . YAML.prettyPrintParseException


