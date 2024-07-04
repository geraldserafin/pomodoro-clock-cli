import Commands
import qualified Deamon

main :: IO ()
main = do
  cmd <- parseCommand

  case cmd of
    Start   s -> Deamon.start s
    Message m -> putStrLn =<< Deamon.sendMessage m

