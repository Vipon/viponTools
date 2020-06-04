module HelloName
  ( askForName
  , nameStatement
  , helloName
  ) where

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " <> name <> "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            (\name ->
              return (nameStatement name)
            ) >>=
            putStrLn

