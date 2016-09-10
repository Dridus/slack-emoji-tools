#! /usr/bin/env stack
-- stack --resolver lts-6.16 --install-ghc runghc --package wreq --package lens-aeson --package classy-prelude --package optparse-applicative --package taggy-lens
{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

import ClassyPrelude
import Control.Lens (Prism', (.~), (^.), (^..), (^?), _Just, at, ix, nearly, only, to)
import Data.Aeson (Value)
import Data.Aeson.Lens (_Bool, _JSON, _Object, _String, key, members)
import Data.Text (takeWhileEnd)
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as Sess
import qualified Options.Applicative as Opt
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (dropExtension, takeFileName)
import System.IO (hSetEcho)
import Text.Taggy.Lens (html, allAttributed, allNamed, attributed, attrs)

data Opts = Opts
  { teamName :: Text
  , inputDirectory :: FilePath
  }

json :: Prism' LByteString Value
json = _JSON

main :: IO ()
main = do
  let argsParserInfo =
        Opt.info (Opt.helper <*> argsParser) (Opt.fullDesc ++ Opt.progDesc "Upload Slack emojis from a directory" ++ Opt.header "import-emojis.hs")
      argsParser = Opts
        <$> (map pack . Opt.strOption $ Opt.short 't' ++ Opt.long "team" ++ Opt.metavar "TEAM-NAME" ++ Opt.help "what team url prefix TEAM-NAME.slack.com")
        <*> (           Opt.strOption $ Opt.short 'd' ++ Opt.long "input-directory" ++ Opt.metavar "DIRECTORY" ++ Opt.help "where to load the emoji images from")

  Opts {..} <- Opt.execParser argsParserInfo

  hPut stdout $ asText "Email? "
  hSetEcho stdin False
  username <- hGetLine stdin
  hPutStrLn stdout $ asText ""
  hPut stdout $ asText "Password? "
  password <- hGetLine stdin
  hPutStrLn stdout $ asText ""
  hSetEcho stdout True

  let urlPrefix = unpack $ "https://" <> teamName <> ".slack.com"
      crumb :: Wreq.Response LByteString -> IO Text
      crumb resp =
        maybe (fail "couldn't get crumb") pure $ 
          resp ^? Wreq.responseBody . to decodeUtf8
                . html . allAttributed (ix "name" . only "crumb")
                  . attrs . at "value" . _Just
      croakOnAlert :: Wreq.Response LByteString -> IO ()
      croakOnAlert resp = 
        let alerts =
              resp ^.. Wreq.responseBody . to decodeUtf8
                     . html . allNamed (only "p") . attributed (ix "class" . nearly "" (elem "alert_error" . words))
        in unless (null alerts) $ do
          traverse_ (putStrLn . ("Alert: " <>) . tshow) alerts
          fail "saw some alerts and got scared!"

  emoji <-
    map (\ s -> (pack . dropExtension . takeFileName $ s, inputDirectory </> s)) .
      filter (\ s -> ".png" `isSuffixOf` s || ".jpg" `isSuffixOf` s || ".gif" `isSuffixOf` s) <$>
        getDirectoryContents inputDirectory

  Sess.withSession $ \ sess -> do
    putStrLn "Getting login page"
    loginForm <- Sess.get sess (urlPrefix <> "/")
    loginCrumb <- crumb loginForm
    putStrLn $ "  got crumb: " <> loginCrumb
    putStrLn "Posting login form"
    login <-
      Sess.post sess (urlPrefix <> "/")
        ( [ ("signin", "1")
          , ("redir", "")
          , ("email", username)
          , ("password", password)
          , ("remember", "remember")
          , ("crumb", encodeUtf8 loginCrumb) ] :: [(ByteString, ByteString)] )
    croakOnAlert login

    putStrLn "Getting customize emoji page"
    customizeEmojiForm <- Sess.get sess (urlPrefix <> "/customize/emoji")
    customizeEmojiCrumb <- crumb customizeEmojiForm
    let existingEmoji = customizeEmojiForm ^. Wreq.responseBody . to (decodeUtf8 . toStrict)
    croakOnAlert customizeEmojiForm

    for_ emoji $ \ (name, path) -> do
      if (":" <> name <> ":") `isInfixOf` existingEmoji
        then
          putStrLn $ "not uploading " <> name <> " - already exists"
        else do
          putStrLn $ "uploading " <> pack path <> " as " <> name
          resp <- Sess.post sess (urlPrefix <> "/customize/emoji")
            [ Wreq.partBS "add" "1"
            , Wreq.partBS "crumb" (encodeUtf8 customizeEmojiCrumb)
            , Wreq.partBS "name" (encodeUtf8 name)
            , Wreq.partBS "mode" "data"
            , Wreq.partFile "img" path
            ]
          croakOnAlert resp
