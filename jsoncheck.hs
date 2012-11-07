import ShellCheck.Simple
import Text.JSON

instance JSON ShellCheckComment where
  showJSON c = makeObj [
      ("line", showJSON $ scLine c), 
      ("column", showJSON $ scColumn c),
      ("level", showJSON $ scSeverity c),
      ("message", showJSON $ scMessage c)
      ]
  readJSON = undefined

main = do
  script <- getContents
  putStrLn $ encodeStrict $ shellCheck script 
