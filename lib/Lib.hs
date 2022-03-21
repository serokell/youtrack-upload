module Lib where

import Control.Monad.RWS (MonadReader)
import qualified Control.Monad.Reader as R
import qualified Data.Aeson as J
import qualified Data.Aeson.Internal.Time as J (fromPico)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Csv (FromNamedRecord (parseNamedRecord), HasHeader (HasHeader, NoHeader), decode, decodeByName, encode, (.:))
import Data.Functor ((<$>), (<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Data.Time.Clock.POSIX as DT
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( JsonResponse,
    POST (POST),
    Req,
    ReqBodyJson (ReqBodyJson),
    defaultHttpConfig,
    header,
    https,
    jsonResponse,
    req,
    runReq,
    (/:),
    (=:),
  )
import System.Environment (getEnvironment, lookupEnv)

-- Utils to send the clocks to youtrack

data Context = Context
  { -- | yt token. you can get one on profile page by clicking "Update personal information and manage logins".
    ctxToken :: BS.ByteString,
    -- | base url of your youtrack instance, without /api
    ctxYoutrackUrl :: BS.ByteString
  }
  deriving (Show, Generic)

data Clock = Clock
  { -- | Issue ID. Both human readable (SRK-101) and internal (11-24421) forms can be used/
    clockIssue :: T.Text,
    -- | Text to show in the issue
    clockText :: T.Text,
    -- | Time of start, in millis from epoch
    clockStart :: Integer,
    -- | Clock duration, in minutes
    clockDuration :: Integer
  }
  deriving (Show, Generic, J.ToJSON, J.FromJSON)

pushClock :: MonadReader Context m => Clock -> m (Req (JsonResponse J.Value))
pushClock clock =
  do
    ctx <- R.ask
    pure $
      runReq defaultHttpConfig $
        req
          POST
          (https (T.pack . BS.unpack $ ctxYoutrackUrl ctx) /: "api" /: "issues" /: clockIssue clock /: "timeTracking" /: "workItems")
          ( ReqBodyJson
              ( J.object
                  [ ("duration", J.object [("minutes", J.toJSON @Integer $ clockDuration clock)]),
                    ("text", J.toJSON $ clockText clock),
                    ("date", J.toJSON @Integer (clockStart clock))
                  ]
              )
          )
          jsonResponse
          $ mconcat
            [ header "Authorization" (BS.concat ["Bearer ", ctxToken ctx]),
              -- https://www.jetbrains.com/help/youtrack/devportal/api-query-syntax.html
              "fields" =: ("id" :: String)
            ]

--  Some utilities to work with org files

data OrgCSV = OrgCSV
  { task :: BS.ByteString,
    parents :: BS.ByteString,
    category :: BS.ByteString,
    start :: BS.ByteString,
    end :: BS.ByteString,
    effort :: BS.ByteString,
    ishabit :: BS.ByteString,
    tags :: BS.ByteString
  }
  deriving (Show, Generic, FromNamedRecord)

-- TODO: make it less monadic
orgToClock org =
  let parsed' = (DT.parseTimeM True DT.defaultTimeLocale "%F %R" :: String -> IO DT.UTCTime) . BS.unpack
      parsed = fmap (DT.nominalDiffTimeToSeconds . DT.utcTimeToPOSIXSeconds) . parsed'
      picoMult = 1000000000
      tt = T.pack . BS.unpack

      toMinutes = flip quot (60000 * picoMult)
      toMillis = flip quot picoMult

      (issue, title) =
        let text = (tt $ task org)
            i = fromMaybe 0 $ T.findIndex (== ' ') text
            (head, tail) = T.splitAt i text
         in (head, tail)
   in do
        start <- parsed (start org)
        end <- parsed (end org)
        return
          Clock
            { clockIssue = issue,
              clockText = T.strip title,
              clockStart = toMillis $ J.fromPico start,
              clockDuration = toMinutes $ J.fromPico (end - start)
            }

processFile :: BSL.ByteString -> IO (V.Vector Clock)
processFile string = do
  (_, decoded :: V.Vector OrgCSV) <- either fail pure $ decodeByName string
  mapM orgToClock decoded

commandSend :: String -> IO ()
commandSend file = do
  token <-
    lookupEnv "YT_TOKEN" >>= \case
      Nothing -> fail "No token in env. Please, specify YT_TOKEN."
      Just f -> pure $ BS.pack f

  let context =
        ( Context
            { ctxYoutrackUrl = "issues.serokell.io",
              ctxToken = token
            }
        )

  -- Filtering out all the empty clocks.
  clocks <- (BSL.readFile file >>= processFile) <&> V.filter (\f -> clockDuration f > 0)

  mapM_ print clocks
  print ("Work items to send: ", length clocks, "Press enter to send the clocks")
  _ <- getLine

  print "Sending clocks..."

  let applyContext a = R.runReader (pushClock a) context
  lm <- traverse (runReq defaultHttpConfig) (V.map applyContext clocks)

  mapM_ print lm
