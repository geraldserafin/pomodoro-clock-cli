import Commands

main :: IO ()
main = do
  cmd <- parseCommand 

  case cmd of
    Status         -> putStrLn "Status: Timer is running"
    Start wt bt cs -> putStrLn $ "Starting " 
                              <> show cs <> " cycles of pomodoro with worktime: " 
                              <> show wt <> " and breaktime: " 
                              <> show bt

