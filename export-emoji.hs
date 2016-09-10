#! /usr/bin/env stack
-- stack --resolver lts-6.16 --install-ghc runghc --package wreq --package lens-aeson --package classy-prelude --package optparse-applicative
{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

import ClassyPrelude
import Control.Lens (Prism', (.~), (^.), (^..), (^?), each, withIndex)
import Data.Aeson (Value)
import Data.Aeson.Lens (_Bool, _JSON, _Object, _String, key, members)
import Data.Text (takeWhileEnd)
import qualified Network.Wreq as Wreq
import qualified Options.Applicative as Opt
import System.Directory (createDirectoryIfMissing)

data Opts = Opts
  { bearerToken :: Text
  , outputDirectory :: FilePath
  }

json :: Prism' LByteString Value
json = _JSON

main :: IO ()
main = do
  let argsParserInfo =
        Opt.info (Opt.helper <*> argsParser) (Opt.fullDesc ++ Opt.progDesc "Download Slack emojis into a directory" ++ Opt.header "export-emojis.hs")
      argsParser = Opts
        <$> (map pack . Opt.strOption $ Opt.short 't' ++ Opt.long "bearer-token" ++ Opt.metavar "BEARER-TOKEN" ++ Opt.help "what API bearer token to log in as")
        <*> (           Opt.strOption $ Opt.short 'd' ++ Opt.long "output-directory" ++ Opt.metavar "DIRECTORY" ++ Opt.help "where to place the emoji images")

  Opts {..} <- Opt.execParser argsParserInfo

  let opts = Wreq.param "token" .~ [bearerToken]
           $ Wreq.defaults

      croakResp :: Text -> Wreq.Options -> Wreq.Response LByteString -> IO ()
      croakResp s opts rp = do
        hPutStrLn stderr $ s <> " failed."
        hPutStrLn stderr $ asText "Request was:"
        hPutStrLn stderr $ tshow opts
        hPutStrLn stderr $ asText "Response was:"
        hPutStrLn stderr . decodeUtf8 $ rp ^. Wreq.responseBody
        fail . unpack $ s <> " failed"

  createDirectoryIfMissing False outputDirectory

  emojiListResp <- Wreq.getWith opts "https://slack.com/api/emoji.list"
  unless (fromMaybe False $ emojiListResp ^? Wreq.responseBody . json . key "ok" . _Bool) $
    croakResp "emoji list" opts emojiListResp

  let emoji :: [(Text, Text)]
      emoji = emojiListResp ^.. Wreq.responseBody . json . key "emoji" . members . _String . withIndex

  for_ emoji $ \ (label, url) ->
    unless ("alias:" `isPrefixOf` url) $ do
      let extension = takeWhileEnd (/= '.') url
          targetPath = outputDirectory </> unpack label <.> unpack extension
      putStrLn $ label <> " from " <> url <> " to " <> pack targetPath <> "..."
      imageResp <- Wreq.getWith opts (unpack url)
      writeFile targetPath $ imageResp ^. Wreq.responseBody

