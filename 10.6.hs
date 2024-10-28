import System.IO

getCh :: IO Char
getCh = do
         hSetEcho stdin False
         x <- getChar
         hSetEcho stdin True
         return x

readLine :: IO String
readLine = readLine' ""
  where
    readLine' :: String -> IO String
    readLine' cs = do
      c <- getCh
      case c of

        '\DEL' ->
          if (null cs) then
            readLine' cs
          else do
            putStr "\b \b"
            readLine' (init cs)

        '\n' -> do
          putChar c
          return cs

        _    -> do
          putChar c
          readLine' (cs ++ [c])
