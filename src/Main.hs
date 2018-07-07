module Main where
import Text.Printf
import Server.Server

port :: Int
port = 5000

main :: IO ()
main = do
  printf "Starting server on port %d\n" port
  runServer port
