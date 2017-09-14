module Main

-- Ex.2, palindrome?
palindrome : (str : String) -> Bool
palindrome str = str == reverse str

-- Ex.3, not case sensitive
palindrome' : (str : String) -> Bool
palindrome' str = palindrome $ toLower str

-- Ex.4, only works for long strings
-- Ex.5, define threshold as argument
palindrome'' : (n : Nat) -> (str : String) -> Bool
palindrome'' n str = if length str > n
                       then palindrome' str
                       else False

-- Ex.6, counts: number of words and chars in a string
counts : (str : String) -> (Nat, Nat)
counts str = (cws, ccs)
  where
    cws = length $ words str
    ccs = length str

-- Ex.7, top_ten: ten largest values in list
top_ten : Ord a => List a -> List a
top_ten ls = (Prelude.List.take 10 . reverse . sort) ls

-- Ex.8, over_length: words longer than n
over_length : Nat -> List String -> Nat
over_length n strs = length $ filter (\x => length x > n) strs

-- Ex.9, palindrome-repl
palindromeRepl : String -> String
palindromeRepl str = show (palindrome' str) ++ "\n"

main : IO()
main = repl "Enter a string: " palindromeRepl
