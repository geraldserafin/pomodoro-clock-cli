import Commands

main :: IO ()
main = do
  cmd <- parseCommand

  case cmd of
    StartDeamon     -> putStrLn "Deamon Started"
    MessageDeamon m -> print m

