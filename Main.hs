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
        let l1l2Treebank = zip l1Treebank l2Treebank
        let l1Pattern = read "TREE (POS \"NOUN\") [DEPREL \"det\", DEPREL \"det:poss\"]"
        let l2Pattern = read "TREE (POS \"NOUN\") [DEPREL \"det:poss\"]"
        let errPattern = (l1Pattern, l2Pattern) :: ErrorPattern
        let l1Matches = concat $ map ((map udTree2sentence) . (matchesUDPattern l1Pattern ) . udSentence2tree) l1Treebank
        mapM_ (putStrLn . prt) l1Matches