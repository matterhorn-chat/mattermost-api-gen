{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Main (main) where

import           Control.Lens

import           GHC.Stack (HasCallStack)
-- import           Data.CallStack
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Aeson as A
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict as HM

import System.FilePath as FP

import Text.Show.Pretty

version_name :: String
version_name = "v4"

type Section = String

sectionFileMap :: HM.HashMap Section FilePath
sectionFileMap = HM.fromList $
  map (\n -> (n, "reference" </> version_name </> n FP.<.> "json"))
  [ "brand"
  , "channels"
  , "cluster"
  , "commands"
  , "compliance"
  , "definitions"
  , "emoji"
  , "files"
  , "introduction"
  , "ldap"
  , "oauth"
  , "posts"
  , "preferences"
  , "reactions"
  , "saml"
  , "status"
  , "system"
  , "teams"
  , "users"
  , "webhooks"
  ]

-- | This section corresponds to the entry point
-- for the specification. We find important
-- misc. data in this section.
topSection :: Section
topSection = "introduction"

readSection :: HasCallStack => FilePath -> IO A.Value
readSection fp = do
  cs <- BS.readFile fp
  let mb_json = A.eitherDecode' cs
  case mb_json of
    Left err   -> fail $ "Parse error in: '" ++ fp ++ "'\n" ++
                         "Error: " ++ err
    Right json -> return json

readSections :: HasCallStack
             => HM.HashMap Section FilePath -> IO (HM.HashMap Section A.Value)
readSections sMap = do
  let foldlM a m f = HM.foldlWithKey' f a m
  foldlM (return mempty)
         sMap $ \a k v -> do
           m  <- a
           json <- readSection v
           return $! HM.insert k json m

-- TODO: this would be a lot easier if we converted each endpoint to
-- an Endpoint type and then did the rest with that.
printEndpoint :: HasCallStack => T.Text -> (T.Text, A.Value) -> IO ()
printEndpoint basePath (path, v) = do
  let route = basePath `T.append` path
  putStrLn (T.unpack route)

  case v ^? key "post" . nonNull of
    Just v'  -> printEndpointDetails "post" v'
    Nothing  -> case v ^? key "put" . nonNull of
      Just v' -> printEndpointDetails "put" v'
      Nothing -> printEndpointDetails "get" (v ^?! key "get")

printEndpointDetails :: HasCallStack => String -> A.Value -> IO ()
printEndpointDetails typ v = do
  putStrLn typ
  let parameters = v ^?! key "parameters" . nth 0 ^@.. members
  pPrint parameters
  return ()


main :: HasCallStack => IO ()
main = do
  putStrLn "mattermost-api-gen"
  top <- readSection (sectionFileMap HM.! topSection)
  -- print sectionJSONMap

  -- first order of business is to extract
  -- the base route
  let basePath = top ^?! key "basePath" . _String
      version  = top ^?! key "info" ^?! key "version" . _String
      endpoints :: [Section]
      endpoints = top ^?! key "x-tagGroups" . _Array . folded .
                  -- Extract the Endpoints group
                  filtered (\k -> k^?! key "name" == "Endpoints") ^?!
                  -- Extract the names of the end point sections
                  key "tags" . _Array ^.. folded . _String &
                  -- the file names are all stored lower cased,
                  -- but the names in the yaml are mixed case
                  map T.toLower &
                  -- For better or worse, probably worse,
                  -- we're using Section (aka String) for these
                  map T.unpack
  print basePath
  print version
  print endpoints
  let endpointsFileMap = HM.intersection sectionFileMap
                                         (HM.fromList (map (,()) endpoints))
  sectionJSONMap <- readSections endpointsFileMap
  -- mapM_ print sectionJSONMap
  -- let userEndpoints = sectionJSONMap HM.! "users" ^@.. members
  let foldlM' a m f = HM.foldl' f a m
  foldlM' (return ()) sectionJSONMap $ \_ v -> do
    mapM_ (printEndpoint basePath) (v ^@.. members)

  return ()
