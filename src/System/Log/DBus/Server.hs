{-# LANGUAGE OverloadedStrings #-}
module System.Log.DBus.Server where

import           DBus
import           DBus.Client
import qualified DBus.Introspection as I
import           System.Log.Logger
import           Text.Read

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

setLogLevelFromPriorityString :: String -> String -> IO (Either Reply ())
setLogLevelFromPriorityString logPrefix levelString =
  let maybePriority = readMaybe levelString
      getMaybeResult = sequenceA $ setLogLevel logPrefix <$> maybePriority
  in maybeToEither (ReplyError errorInvalidParameters []) <$> getMaybeResult

setLogLevel :: String -> Priority -> IO ()
setLogLevel logPrefix level =
  getLogger logPrefix >>= saveGlobalLogger . setLevel level

logInterface :: Interface
logInterface = defaultInterface
  { interfaceName = "org.taffybar.LogServer"
  , interfaceMethods = [ autoMethod "SetLogLevel" setLogLevelFromPriorityString ]
  }

logPath :: ObjectPath
logPath = "/org/taffybar/LogServer"

startLogServer :: Client -> IO ()
startLogServer client =
  export client logPath logInterface

logIntrospectionInterface :: I.Interface
logIntrospectionInterface = buildIntrospectionInterface logInterface
