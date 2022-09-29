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
        l1Treebank <- parseUDFile l1File
        l2Treebank <- parseUDFile l2File
        let l1l2Treebank = zip l2Treebank l1Treebank
        let err_pattern = (read "TREE (POS \"NOUN\") [DEPREL \"det\", DEPREL \"det:poss\"]", read "TREE (POS \"NOUN\") [DEPREL \"det:poss\"]") :: ErrorPattern
        mapM_ putStrLn $ filter (not . null) $ map (showMatchesInUDSentence [] (fst err_pattern)) l1Treebank
        -- Align everything and print results
        {-
        let as = map (M.toList . alignSent M.empty criteria Nothing False True False) l1l2Treebank
        mapM_ (putStrLn . prettyPrintAlignment) (reverse $ concat as)
        -}