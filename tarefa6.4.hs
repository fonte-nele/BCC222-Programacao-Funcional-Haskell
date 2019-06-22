describeLetter :: Char -> String
describeLetter c | (c>='a' && c<='z') = "Lower case"
                 | (c>='A' && c<='Z') = "Upper case"
                 | otherwise = "Not an ASCII letter"
