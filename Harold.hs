module Harold where
	import Ennel
	import Ennel.Linguistics

	import Control.Monad

	import Data.Monoid
	import Data.List

	import qualified Data.Map.Strict as Map
	import Data.JustParse hiding (length, Result)
	import Data.JustParse.Char
	import Data.JustParse.Combinator

	import System.IO

	import Control.Exception

	data Result = Num [(Int -> Int)] | List [Int] | Single Int | Filter (Int -> Bool) | Generator (Int -> [Int]) | Fold ([Int] -> Int) | Error

	instance Show Result where
		show (List xs)	| length xs < 15	= show xs
						| otherwise			= show (take 15 xs) ++ " and some more..."
		show (Single x) = show x
		show (Num x) 	= show $foldl (flip ($) ) 0 x
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

	--parseSemantics :: SemanticTree -> Result
	parseSemantics (SemanticTree r cs) = case r of
		"index" -> Single (x !! (n - 1))
			where
				Single n = convertIfNeeded . parseSemantics $ cs !! 0
				List x = parseSemantics (cs !! 1)
		"firstn" -> List (take n x)
			where
				Single n = convertIfNeeded . parseSemantics $ cs !! 0
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
				Single n = convertIfNeeded . parseSemantics $ cs !! 1
		"fold" -> Single (f x)
			where
				Fold f = parseSemantics (cs !! 0)
				List x = parseSemantics (cs !! 1)
		"sum" -> Fold sum
		_ -> case cs of 
			[] -> Num [(stringToFunc a)]
			otherwise -> Num( (stringToFunc a) : n)
			where
				Just a = stripPrefix "Num" r
				Num n = parseSemantics $ cs !! 0
	
	convertIfNeeded (Single s) = Single s
	convertIfNeeded (Num xs) = Single $ foldl (flip ($) ) 0 xs

	stringToFunc :: String -> (Int -> Int)
	stringToFunc snum = a where
		Just a = Map.lookup snum numberMap--allowed to use parseJust because non-numberKeys were parse out earlier

	numberKeys = Map.keys numberMap
	numberMap = Map.fromList numbers
	numbers = 
		[	("a" , (+1))
		,	("one", (+1))
		, 	("two", (+2))
		,	("three", (+3))
		,	("four", (+4))
		,	("five", (+5))
		,	("six", (+6))
		,	("seven", (+7))
		,	("eight", (+8))
		,	("nine", (+9))
		,	("ten", (+10)) 
		,	("eleven", (+11)) 
		,	("twelve", (+12)) 
		,	("thirteen", (+13)) 
		,	("fourteen", (+14)) 
		,	("fifteen", (+15)) 
		,	("sixteen", (+16)) 
		,	("seventeen", (+17)) 
		,	("eighteen", (+17))
		,	("nineteen", (+19)) 
		,	("twenty", (+20)) 
		,	("thirty", (+30)) 
		,	("forty", (+40)) 
		,	("fifty", (+50)) 
		,	("sixty", (+60)) 
		,	("seventy", (+70)) 
		,	("eighty", (+80))
		,	("ninety", (+90)) 
		,	("hundred", (*100)) 
		,	("thousand", (*1000)) 
		,	("million", (*1000000))
		,	("billion", (*1000000000))   ]

	bugException :: SomeException -> IO ()
	bugException = const $ putStrLn "Sorry, I understood what you said but it doesn't make any sense. This is probably my fault. Try something else."



	runTestCases = do
		hSetBuffering stdout NoBuffering
		determiners <- loadFile "Rules/determiners.rules"
		numbers <- loadFile "Rules/numbers.rules"
		generators <- loadFile "Rules/generators.rules"
		filters <- loadFile "Rules/filters.rules"
		prepositions <- loadFile "Rules/prepositions.rules"
		folds <- loadFile "Rules/folds.rules"
		let table = determiners <> numbers <> generators <> filters <> prepositions <> folds
		let theTest x = handle bugException $ do
			putStrLn $ case runInterpreter table np x of
				Nothing -> x ++" -> Sorry, I can't understand what you just said. Try something else."
				Just semantics -> x ++ " -> " ++ show (parseSemantics semantics)
		mapM theTest ["one", "one one", "two", "one two", "twenty", "one twenty", "twenty one", "twenty twenty",
			"one hundred", "twenty hundred", "one hundred hundred", "one hundred two", "one hundred twenty two",
			"one hundred twenty twenty", "one hundred twenty two two", "one hundred two hundred","ten", "one ten",
			"ten one", "twenty ten", "ten ten", "one hundred ten", "one hundred ten one", "ten hundred", "ten hundred one",
			"one thousand", "one thousand two", "one hundred thousand", "twenty thousand", "twenty hundred thousand",
			"thousand thousand", "thousand one", "one thousand one", "five thousand twenty two", 
			"two hundred thousand hundred five", "two hundred thousand one hundred five", "ten", "two ten", "ten two",
			"ten twenty", "twenty ten", "ten hundred", "one hundred ten", "one hundred ten two", "one hundred ten hundred",
			"one hundred twenty two thousand ten hundred five", "two thousand ten hundred five"]
		return ()