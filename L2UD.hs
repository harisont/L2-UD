module L2UD where
    import RTree
    import UDConcepts
    import ConceptAlignment

    test :: String
    test = "test"

    -- ALIGNMENT CRITERIA FOR L1-L2 TREEBANKS
    
    criteria :: [Criterion]
    criteria = [
        same,
        sameRoot,
        sameForm,
        sameLemma,
        sameDeprel,
        sameSimpleDeprel,
        samePOS 
        ]

    -- | Same tree
    same :: UDTree -> UDTree -> Bool
    same t u = t == u

    -- | Exact same root token
    sameRoot :: UDTree -> UDTree -> Bool
    sameRoot (RTree n _) (RTree m _) = n == m

    -- | Same word from
    sameForm :: UDTree -> UDTree -> Bool
    sameForm (RTree n _) (RTree m _) = udFORM n == udFORM m

    -- | Same lemma
    sameLemma :: UDTree -> UDTree -> Bool
    sameLemma (RTree n _) (RTree m _) = udLEMMA n == udLEMMA m

    -- | Same dependency relation
    sameDeprel :: UDTree -> UDTree -> Bool
    sameDeprel (RTree n _) (RTree m _) = udDEPREL n == udDEPREL m

    -- | Same dependency relation (ignoring subtypes)
    sameSimpleDeprel :: UDTree -> UDTree -> Bool
    sameSimpleDeprel (RTree n _) (RTree m _) = 
        udSimpleDEPREL n == udSimpleDEPREL m

    -- | Same root UPOS  
    -- (maybe use bag of contentTags for bag of subtree POSs?)
    samePOS :: UDTree -> UDTree -> Bool
    samePOS (RTree n _) (RTree m _) = udUPOS n == udUPOS n