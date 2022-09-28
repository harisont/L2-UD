module Main where
    import qualified Data.Map as M
    import UDConcepts
    import ConceptAlignment
    import L2UD

    main = do
        let l1File = "data/Valico/L1/it_valico-ud-test.conllu"
        let l2File = "data/Valico/L2/it_valico-ud-test.conllu"
        l1Treebank <- parseUDFile l1File
        l2Treebank <- parseUDFile l2File
        let l1l2Treebank = zip l1Treebank l2Treebank
        let as = map (M.toList . alignSent M.empty criteria Nothing False True False) l1l2Treebank
        let errs = map (filter isError) as
        mapM_ (putStrLn . prettyPrintAlignment) (reverse $ concat errs)