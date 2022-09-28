module L2UD where
    import qualified Data.Set as S
    import RTree
    import UDConcepts
    import ConceptAlignment

    -- ALIGNMENT CRITERIA FOR L1-L2 TREEBANKS
    
    -- | Ordered list of criteria
    criteria :: [Criterion]
    criteria = map mkCriterion [
        sameToken,
        sameForm,
        sameLemma,
        sameDeprel
        ]

    -- | convert a comparison function into a full-blown Criterion
    mkCriterion :: (UDTree -> UDTree -> Bool) -> Criterion
    mkCriterion f = C f (S.singleton UNKNOWN) False False 

    -- | Same tree
    same :: UDTree -> UDTree -> Bool
    same t u = t == u

    -- | Exact same root token
    sameToken :: UDTree -> UDTree -> Bool
    sameToken (RTree n _) (RTree m _) = n == m

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
    sameUPOS :: UDTree -> UDTree -> Bool
    sameUPOS (RTree n _) (RTree m _) = udUPOS n == udUPOS n

    -- ERROR EXTRACTION
    isError :: Alignment -> Bool 
    isError (AT (t,u), _) = linearize t /= linearize u
