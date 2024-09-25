import Commands
import qualified Deamon

main :: IO ()
main = do
  Interaction cmd path <- parseCommand

  case cmd of
    Message m -> Deamon.sendMessage m path >>= mapM_ putStrLn
    Start c -> Deamon.sendMessage Terminate path >> Deamon.start c path
