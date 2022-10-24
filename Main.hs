module Main where
    import qualified Data.Map as M
    import System.Environment (getArgs)
    import System.Console.GetOpt
    import System.Directory
    -- gf-ud
    import UDConcepts
    import UDPatterns
    -- concept-alignment
    import ConceptAlignment
    -- local
    import L2UD

    main = do
        argv <- getArgs
        let (flags,args) = parseArgv argv usage opts
        if Help `elem` flags || length args < 3 
            then putStrLn $ usageInfo usage opts
            else do
                l1ss <- parseUDFile (args !! 0)
                l2ss <- parseUDFile (args !! 1)
                let (l1p,l2p) = case length args of
                                    4 -> (read (args !! 2), read (args !! 3))
                                    3 -> parseQuery (args !! 2)
                let l1l2ss = zip l1ss l2ss
                -- obtain aligned subtrees
                let as = concatMap (M.toList . alignSent M.empty criteria Nothing False True False) l1l2ss
                -- query L1 treebank
                let l1ms = concatMap ((map (adjustUDIds . udTree2sentence . createRoot)) . (matchesUDPattern l1p) . udSentence2tree) l1ss
                -- query L2 treebank (via alignments)
                let l1l2ms = filter (\a-> linearize (sl a) `elem` map (linearize . udSentence2tree) l1ms && (not . null) (matchesUDPattern l2p (tl a))) as
                if Linearize `elem` flags
                    then mapM_ (putStrLn . prettyPrintAlignment) l1l2ms
                    else do
                        createDirectoryIfMissing True "out"
                        let (l1s, l2s) = unzip $ map alignment2sentencePair l1l2ms
                        writeFile "out/L1.conllu" (unlines $ [prUDSentence n s | (n,s) <- ([1..] `zip` l1s)])
                        writeFile "out/L2.conllu" (unlines $ [prUDSentence n s | (n,s) <- ([1..] `zip` l2s)])

    -- COMMAND LINE OPTIONS PARSING
    
    data Flag = Help | Linearize deriving Eq
    type Arg = String

    opts :: [OptDescr Flag]
    opts = [
        Option ['h'] ["help"] (NoArg Help) "show this help message and exit",
        Option ['l'] ["linearize"] (NoArg Linearize) "output linearizations instead of CoNNL-U sentences"
        ]

    usage :: String
    usage = "stack run -- PATH-TO-L1-TREEBANK PATH-TO-L2-TREEBANK PATTERN(S) [--linearize]"

    parseArgv :: [String] -> String -> [OptDescr Flag] -> ([Flag],[Arg])
    parseArgv argv usage opts = case getOpt Permute opts argv of
        (flags,args,[]) -> (flags,args)
        (_,_,errs) -> error $ concat errs ++ usage