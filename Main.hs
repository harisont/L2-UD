module Main where
    import UDConcepts
    import ConceptAlignment
    import L2UD

    main = do
        let l1File = "data/Valico/L1/it_valico-ud-test.conllu"
        let l2File = "data/Valico/L2/it_valico-ud-test.conllu"
        l1Treebank <- parseUDFile l1File
        l2Treebank <- parseUDFile l2File
        let l1l2Treebank = zip l1Treebank l2Treebank
        -- maybe use alignSent rather than align
        let as = align [] criteria Nothing False False (l1l2Treebank :: [(UDSentence,UDSentence)])
        let errs = filter isError as
        mapM_ (putStrLn . prLinearizedAlignment) errs