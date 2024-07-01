import Commands
import qualified Deamon as D

main :: IO ()
main = do
  cmd <- parseCommand

  case cmd of
    StartDeamon     -> D.start
    MessageDeamon m -> D.message m

