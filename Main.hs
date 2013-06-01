{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Lens
import qualified Data.ByteString.Char8                  as BS8 (pack)
import           Data.Default.Class
import           Data.List
import           Network.Beeminder
import           System.Console.Shell
import           System.Console.Shell.Backend.Haskeline
import           System.Console.Shell.ShellMonad
import           System.Environment

react :: String -> Sh () ()
react str = shellPutStrLn ("You entered " ++ str ++ "!")

shellbee = initialShellDescription
  { shellCommands =
    [ exitCommand "quit"
    , helpCommand "help"
    , cmd "foo" fooFunc "Do the foo."
    ]
  , commandStyle = OnlyCommands
  , evaluateFunc = react
  , greetingText = Just "ShellBee!"
  }

fooFunc :: Completable Wibble -> File -> Username -> Sh () ()
fooFunc (Completable s) (File f) (Username u) = shellPutStrLn $ "Your choice of wibble: " ++ s ++ "; file: " ++ f ++ "; username: " ++ u

data Wibble = Wibble

instance Completion Wibble () where
  complete _ _ str = return $ filter (str `isPrefixOf`) ["hello", "hi", "blue", "black", "green"]
  completableLabel _ = "wibble"

data ShellbeeSt = ShellbeeSt
  { _token :: Maybe Token
  , _user  :: Maybe User
  }
  deriving Show

makeLenses ''ShellbeeSt

instance Default ShellbeeSt where
  def = ShellbeeSt
        { _token = Nothing
        , _user  = Nothing
        }

main = do
  [tok] <- getArgs
  let st = def & token .~ Just (BS8.pack tok)
  print st
--  runShell shellbee haskelineBackend ()
