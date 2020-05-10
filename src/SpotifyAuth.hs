{-# LANGUAGE FlexibleContexts #-}

module SpotifyAuth (
      loadCredentials
    , spotifyID
    , spotifySecret 
    , castToURI 
    , getSpotifyAuthLink
) where



import Network.OAuth.OAuth2
import URI.ByteString as UR
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Except


data SpotifyCredentials = SpotifyCredentials String String deriving (Show)

packStrToBS :: String -> BS.ByteString
packStrToBS = encodeUtf8 . T.pack

loadCredentials :: String -> IO SpotifyCredentials
loadCredentials path = do
    contents <- readFile path
    let creds = lines contents
        in return (SpotifyCredentials (head creds) (creds !! 1))

spotifyID :: SpotifyCredentials -> String
spotifyID (SpotifyCredentials id _) = id

spotifySecret:: SpotifyCredentials -> String
spotifySecret (SpotifyCredentials _ secret) = secret


getSpotifyAuthLink :: SpotifyCredentials -> URIRef Absolute
getSpotifyAuthLink creds = let oauth = getSpotifyConfig creds 
                           in authorizationUrl oauth



-- |This casts a string to a URI, fails if invalid
castToURI :: String -> URI
castToURI str =  let bs = packStrToBS str
                     uri = UR.parseURI strictURIParserOptions bs
                     res = case uri of
                          Right ur -> ur
                          Left err -> Prelude.error (show err)

                 in res


getSpotifyConfig :: SpotifyCredentials -> OAuth2 
getSpotifyConfig creds = let id = spotifyID creds
                             secret = spotifySecret creds
                             authuri = castToURI "https://accounts.spotify.com/authorize"
                             tokenuri = castToURI "https://accounts.spotify.com/api/token"
                             callback = castToURI "http://localhost:8000/callback"
                             auth = OAuth2 (T.pack id) (Just (T.pack secret)) authuri tokenuri (Just callback)
                         in auth
