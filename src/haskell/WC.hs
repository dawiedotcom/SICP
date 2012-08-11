-- file: WC.hs
-- from: Real World Haskell

main = interact wordCount
	where wordCount input = show (length (lines input)) ++ "\n"

