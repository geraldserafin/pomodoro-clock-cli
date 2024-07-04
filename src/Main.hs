import Commands
import qualified Deamon

main :: IO ()
main = do
  cmd <- parseCommand

  case cmd of
    Start   s -> Deamon.sendMessage Terminate >> Deamon.start s
    Message m -> Deamon.sendMessage m >>= putStrLn

