readLine :: IO String
readLine = readLine' ""
  where
    readLine' :: String -> IO String
    readLine' cs = do
      c <- getChar
      if c == '\DEL' then
        if not (null cs) then do
          putStr "\b \b"
          readLine' (init cs)
        else
          readLine' cs
      else if c == '\n' then
        return cs
      else
        readLine' (cs ++ [c])
