{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Lens
import           Control.Monad                          (when)
import           Control.Monad.IO.Class                 (liftIO)
import qualified Data.ByteString.Char8                  as BS8 (pack)
import           Data.Default.Class
import           Data.List
import           Data.Maybe                             (isNothing)
import           Network.Beeminder
import           System.Console.Shell
import           System.Console.Shell.Backend.Haskeline
import           System.Console.Shell.ShellMonad
import           System.Environment

type Shellbee = Sh ShellbeeSt

data ShellbeeSt = ShellbeeSt
  { _token     :: Maybe Token
  , _userCache :: Maybe User
  }
  deriving Show

makeLenses ''ShellbeeSt

instance Default ShellbeeSt where
  def = ShellbeeSt
        { _token     = Nothing
        , _userCache = Nothing
        }

react :: String -> Shellbee ()
react str = shellPutStrLn ("You entered " ++ str ++ "!")

shellbee :: ShellDescription ShellbeeSt
shellbee = initialShellDescription
  { shellCommands =
    [ exitCommand "quit"
    , helpCommand "help"
    , cmd "goals" goalsCmd "Show goals."
    ]
  , commandStyle = OnlyCommands
  , evaluateFunc = react
  , greetingText = Just "ShellBee!\n"
  }

goalsCmd :: Shellbee ()
goalsCmd = do
  cacheUser
  with userCache $ \u ->
    shellPutStrLn $ "Your goals:\n" ++ show u

with l act = use l >>= maybe (return ()) act

cacheUser :: Shellbee ()
cacheUser = do
  u <- use userCache
  when (isNothing u) getUser

getUser :: Shellbee ()
getUser = do
  mu <- runBee $ user (def & _GoalsFilter .~ Just Front)
  userCache .= mu

runBee :: Beeminder a -> Shellbee (Maybe a)
runBee bee = do
  mtok <- use token
  case mtok of
    Nothing  -> return Nothing
    Just tok -> liftIO $ runBeeminder tok bee

main = do
  [tok] <- getArgs
  let st = def & token .~ Just (BS8.pack tok)
  runShell shellbee haskelineBackend st
