# slack-emoji-tools

## Exporting

1. Install [stack](http://haskellstack.org)
2. Create a Slack API test token.
3. `stack export-emoji.hs -t APITOKEN -d DIRECTORY`
   * this will take a bit the first time as it installs libraries

## Importing

1. Install [stack](http://haskellstack.org)
2. `stack import-emoji.hs -t TEAMNAME -d DIRECTORY`
   * this will take a bit the first time as it installs libraries
   * this requires your email address and password
