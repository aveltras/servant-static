{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.Static
  ( mkApp
  ) where

import           Control.Monad              (forM_, join)
import           Crypto.Hash                (MD5, hash)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.FileEmbed             (getDir, makeRelativeToProject)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Instances.TH.Lift          ()
import           Language.Haskell.TH        (Type, appE, appT, clause, conT,
                                             funD, litT, mkName, normalB, sigD,
                                             strTyLit, tySynD, varE)
import           Language.Haskell.TH.Syntax (Dec, Q, addDependentFile, runIO)
import           Network.HTTP.Types         (hCacheControl, hContentType, ok200)
import           Network.Mime               (MimeType, defaultMimeLookup)
import           Network.Wai                (responseLBS)
import           Servant                    ((:<|>), (:>), Application, Server,
                                             Tagged (..), serve)
import           Servant.API                ((:<|>) ((:<|>)), Raw, toUrlPiece)
import           Servant.Links
import           System.FilePath            (replaceBaseName, splitDirectories,
                                             takeBaseName, (</>))

type Entry = (FilePath, FilePath, BS.ByteString, MimeType, FilePath)

mkApp :: String -> FilePath -> Q [Dec]
mkApp apiName staticDir = do
  entries <- fetchEntries =<< makeRelativeToProject staticDir
  mconcat <$> sequenceA [ mkAPI apiName entries
                        , mkServer apiName entries
                        , mkServerReload apiName entries
                        , mkLinks apiName entries
                        , mkApp'
                        , mkAppReload'
                        ]
  where
    mkApp' = sequence [appSig, appFunc]
    appSig = sigD (mkName $ "appFor" <> apiName) [t|Application|]
    appFunc = funD (mkName $ "appFor" <> apiName) [clause [] (normalB [e|serve (Proxy :: Proxy $(conT $ mkName apiName)) $(varE (mkName $ "serverFor" <> apiName))|]) []]

    mkAppReload' = sequence [appSigReload, appFuncReload]
    appSigReload = sigD (mkName $ "appReloadFor" <> apiName) [t|Application|]
    appFuncReload = funD (mkName $ "appReloadFor" <> apiName) [clause [] (normalB [e|serve (Proxy :: Proxy $(conT $ mkName apiName)) $(varE (mkName $ "serverReloadFor" <> apiName))|]) []]

mkAPI :: String -> [Entry] -> Q [Dec]
mkAPI apiName entries = fmap pure $ tySynD (mkName apiName) [] foldedAPI
  where
    foldedAPI :: Q Type
    foldedAPI = foldl1 (appT . appT [t|(:<|>)|]) $ entryType <$> entries

    entryType :: Entry -> Q Type
    entryType (_, hashedName, _, _, _) = pathPiecesToType $ splitDirectories hashedName

mkLinks :: String -> [Entry] -> Q [Dec]
mkLinks apiName entries = mconcat <$> sequenceA [ sequence [linksSig, linksFunc], join <$> traverse linkQ entries ]
  where
    linksName = mkName $ "linksFor" <> apiName
    linksFunc = funD linksName [clause [] (normalB [|safeLink' toUrlPiece (Proxy :: Proxy $(conT (mkName apiName)))|]) []]
    linksSig = sigD linksName [t|forall endpoint. (IsElem endpoint $(conT (mkName apiName)), HasLink endpoint) => Proxy endpoint -> MkLink endpoint Text |]

    linkQ :: Entry -> Q [Dec]
    linkQ (fileName, hashedName, _, _, _) = sequence [endpointSig, endpointFunc]
      where
        endpointLinkType = pathPiecesToType $ splitDirectories hashedName
        endpointSig = sigD endpointName [t|MkLink $(endpointLinkType) Text|]
        endpointFunc = funD endpointName [clause [] (normalB [|$(varE linksName) (Proxy :: Proxy $(endpointLinkType))|]) []]
        endpointName = mkName $ f <$> fileName
          where f '.' = '_'
                f '/' = '_'
                f c   = c

mkServer :: String -> [Entry] -> Q [Dec]
mkServer apiName entries = sequence [serverSig, serverFunc]
  where
    serverName = mkName $ "serverFor" <> apiName
    serverSig = sigD serverName [t|Server $(conT $ mkName apiName)|]
    serverFunc = funD serverName [clause [] (normalB serverExp) []]
    serverExp = (foldl1 $ appE . appE [e|(:<|>)|]) $ flip fmap entries $
      \(_, _, fileContent, mimeType, _) ->
        let lazyBS = LBS.fromStrict fileContent
        in [e| Tagged $ \_ respond -> respond $ responseLBS ok200 [ (hContentType, mimeType)
                                                                  , (hCacheControl, C8.pack "public, max-age=31536000") ] lazyBS |]

mkServerReload :: String -> [Entry] -> Q [Dec]
mkServerReload apiName entries = sequence [serverSig, serverFunc]
  where
    serverName = mkName $ "serverReloadFor" <> apiName
    serverSig = sigD serverName [t|Server $(conT $ mkName apiName)|]
    serverFunc = funD serverName [clause [] (normalB serverExp) []]
    serverExp = (foldl1 $ appE . appE [e|(:<|>)|]) $ flip fmap entries $
      \(_, _, _, mimeType, file) ->
        [e| Tagged $ \_ respond -> LBS.readFile file >>= \lazyBS -> respond $ responseLBS ok200 [ (hContentType, mimeType)
                                                                                                    , (hCacheControl, C8.pack "no-store") ] lazyBS |]


pathPiecesToType :: [FilePath] -> Q Type
pathPiecesToType []     = [t|Raw|]
pathPiecesToType (x:xs) = [t|$(litT $ strTyLit x) :> $(pathPiecesToType xs)|]

fetchEntries :: FilePath -> Q [Entry]
fetchEntries staticDir = do
  files <- runIO $ getDir staticDir
  forM_ files $ addDependentFile . (</>) staticDir . fst
  pure $ (flip fmap) files $
    \(fileName, fileContent) -> ( fileName
                                , replaceBaseName fileName (takeBaseName fileName <> "-" <> (show $ hash @_ @MD5 fileContent))
                                , fileContent
                                , defaultMimeLookup $ T.pack fileName
                                , staticDir </> fileName
                                )
