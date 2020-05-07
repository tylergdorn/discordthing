module Spotify (getSpotifyLink) where

import Text.Regex.PCRE
import qualified Data.Text as T
import Data.Text.Encoding as E

getGroups :: (String, String, String, [String]) -> [String]
getGroups (_, _, _, g) = g

-- | getSpotifyLink returns the part of a spotify url after open.spotify.com
getSpotifyLink :: T.Text -> Maybe T.Text
getSpotifyLink str = 
    let regex = ".*open.spotify.com\\/([^\\s]*)" 
        -- this might be a bad idea to go back and forth for the regex
        text = T.unpack str
        x = (text =~ regex) :: (String, String, String, [String])
        groups = getGroups x
        elem = if null groups then Nothing else Just (T.pack (head groups))
    in elem
