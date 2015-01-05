module HaroldTest where
	import Harold
	
	main = do
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