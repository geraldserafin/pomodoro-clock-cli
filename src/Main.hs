import Commands
import qualified Deamon

main :: IO ()
main = do
  cmd <- parseCommand

  case cmd of
    StartDeamon     -> Deamon.start
    MessageDeamon m -> Deamon.sendMessage m

