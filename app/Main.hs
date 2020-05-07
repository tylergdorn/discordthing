{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf
import Spotify
import System.IO

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = startHandler
                        , discordOnEnd = putStrLn "Ended"
                        -- , discordOnEvent = eventHandler
                        , discordOnEvent = spotifyEventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  threadDelay (1 `div` 10 * 10^6)
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandle -> IO ()
startHandler dis = do
  Right partialGuilds <- restCall dis R.GetCurrentUserGuilds

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall dis $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall dis $ R.GetGuildChannels (guildId guild)
    case filter (\x -> isTextChannel x && channelName x == "music" ) chans of
      (c:_) -> do _ <- restCall dis $ R.CreateMessage (channelId c) "Hello! I will reply to pings with pongs"
                  pure ()
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
-- eventHandler :: DiscordHandle -> Event -> IO ()
-- eventHandler dis event = case event of
--       MessageCreate m -> when (not (fromBot m) && isPing m) $ do
--         _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
--         threadDelay (4 * 10^6)
--         _ <- restCall dis (R.CreateMessage (messageChannel m) "Pong!")
--         pure ()
--       _ -> pure ()


isMusicChannel :: Maybe T.Text -> Bool
isMusicChannel m = case m of
    Just t -> t == ("music" :: T.Text)
    Nothing -> False

spotifyEventHandler :: DiscordHandle -> Event -> IO ()
spotifyEventHandler dis event = case event of
    MessageCreate m -> do 
        channelName <- getChannelName dis m
        when (isMusicChannel channelName) $ spotifyMessageProcess m
    _ -> pure ()

spotifyMessageProcess :: Message -> IO ()
spotifyMessageProcess m = 
    let text = messageText m 
        spotifyUrl = getSpotifyLink text
        user = userName $ messageAuthor m
    in case spotifyUrl of
        Just url -> printf "username: %s \nlink: %s\n" user url >> hFlush stdout
        Nothing -> pure ()


isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

getChannelName :: DiscordHandle -> Message -> IO (Maybe T.Text)
getChannelName dis m = do
    restResult <- restCall dis (R.GetChannel (messageChannel m))
    case restResult of
        Right res -> return (Just (channelName res))
        Left _ -> return Nothing

