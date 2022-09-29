module Main where
    import qualified Data.Map as M
    -- gf-ud
    import UDConcepts
    import UDPatterns
    -- concept-alignment
    import ConceptAlignment
    -- local
    import L2UD

    main = do
        let l1File = "data/Valico/L1/it_valico-ud-test.conllu"
        let l2File = "data/Valico/L2/it_valico-ud-test.conllu"
        l1ss <- parseUDFile l1File
        l2ss <- parseUDFile l2File
        let l1l2ss = zip l1ss l2ss
        let l1p = read "TREE (POS \"NOUN\") [DEPREL \"det\", DEPREL \"det:poss\"]"
        let l2p = read "TREE (POS \"NOUN\") [DEPREL \"det:poss\"]"
        let errp = (l1p, l2p) :: ErrorPattern
        -- obtain aligned subtrees
        let as = concatMap (M.toList . alignSent M.empty criteria Nothing False True False) l1l2ss
        -- query L1 treebank
        let l1ms = concatMap ((map (adjustUDIds . udTree2sentence . createRoot)) . (matchesUDPattern l1p) . udSentence2tree) l1ss
        -- query L2 treebank (via alignments)
        let l1l2ms = filter (\a-> linearize (sl a) `elem` map (linearize . udSentence2tree) l1ms && (not . null) (matchesUDPattern l2p (tl a))) as
        mapM_ (putStrLn . prettyPrintAlignment) l1l2ms