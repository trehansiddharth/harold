module Harold where
	import Ennel
	import Ennel.Linguistics

	import Control.Monad
	import Control.Exception

	import Data.Monoid
	import Data.List

	import System.IO

	import Harold.Numbers

	data Result = List [Int] | Single Int | Filter (Int -> Bool) | Generator (Int -> [Int]) | Fold ([Int] -> Int) | Error

	instance Show Result where
		show (List xs)	| length xs < 15	= show xs
						| otherwise			= show (take 15 xs) ++ " and some more..."
		show (Single x) = show x
		show _ = "[lambda expression]"

	main :: IO ()
	main = do
		hSetBuffering stdout NoBuffering
		determiners <- loadFile "Rules/determiners.rules"
		numbers <- loadFile "Rules/numbers.rules"
		generators <- loadFile "Rules/generators.rules"
		filters <- loadFile "Rules/filters.rules"
		prepositions <- loadFile "Rules/prepositions.rules"
		folds <- loadFile "Rules/folds.rules"
		let table = determiners <> numbers <> generators <> filters <> prepositions <> folds
		forever $ do
			putStr "OK Harold, give me... "
			input <- getLine
			handle bugException $ do
				putStrLn $ case runInterpreter table np input of
					Nothing -> "Sorry, I can't understand what you just said. Try something else."
					Just semantics -> show (parseSemantics semantics)

	parseSemantics :: SemanticTree -> Result
	parseSemantics (SemanticTree r cs) = case r of
		"index" -> Single (x !! (n - 1))
			where
				Single n = parseSemantics (cs !! 0)
				List x = parseSemantics (cs !! 1)
		"firstn" -> List (take n x)
			where
				Single n = parseSemantics (cs !! 0)
				List x = parseSemantics (cs !! 1)
		"filter" -> List (filter f x)
			where
				Filter f = parseSemantics (cs !! 0)
				List x = parseSemantics (cs !! 1)
		"even" -> Filter even
		"odd" -> Filter odd
		"multiples" -> Generator (\n -> map (n*) [1 .. 31])
		"divisors" -> Generator (\n -> filter (\x -> n `mod` x == 0) [1 .. n])
		"numbers" -> List [1 .. 31]
		"prime" -> Filter (\n -> if (n == 1) then False else and . map (\x -> n `mod` x /= 0) $ [2 .. (n - 1)])
		"list" -> List (f n)
			where
				Generator f = parseSemantics (cs !! 0)
				Single n = parseSemantics (cs !! 1)
		"fold" -> Single (f x)
			where
				Fold f = parseSemantics (cs !! 0)
				List x = parseSemantics (cs !! 1)
		"sum" -> Fold sum
		_ -> case cs of 
			[] -> Single $ stringToIntFunc a 0
			otherwise -> Single $ stringToIntFunc a n
			where
				Just a = stripPrefix "Num" r
				Single n = parseSemantics $ cs !! 0

	bugException :: SomeException -> IO ()
	bugException = const $ putStrLn "Sorry, I understood what you said but it doesn't make any sense. This is probably my fault. Try something else."
