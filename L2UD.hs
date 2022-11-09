module L2UD where
    import Data.List.Split
    import qualified Data.Set as S
    import qualified Text.Regex.Posix as R
    -- gf-ud
    import RTree
    import UDConcepts
    import UDPatterns
    -- concept-alignment
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

    -- | Check if an alignment contains an error, of any kind
    hasError :: Alignment -> Bool 
    hasError (AT (t,u), _) = linearize t /= linearize u

    -- | Check if an alignment contains a grammatical error 
    hasGrammError :: Alignment -> Bool
    hasGrammError (AT (t,u), _) = map grammFields wt /= map grammFields wu
        where
            wt = udWordLines $ udTree2sentence t
            wu = udWordLines $ udTree2sentence u
            
            -- get fields that signal grammar errors. Word order errors are
            -- captured when comparing the full lists and I hope it's not too
            -- bad that I am ignoring udHEAD
            -- known issues: 
            -- - some lexical errors are treated as grammatical due to POS
            --   tag (AUX vs. VERB) and deprel (cop) differences 
            grammFields :: UDWord -> (POS,String,[UDData],Label)
            grammFields w = (udUPOS w, udXPOS w, udFEATS w, udDEPREL w)

    -- OUTPUT
    prettyPrintAlignment :: Alignment -> String
    prettyPrintAlignment a = mark ++ " (\"" ++ lt ++ "\", \"" ++ lu ++ "\")"
        where 
            (AT (t,u)) = trees a
            (lt,lu) = (linearize t, linearize u)
            mark = if hasGrammError a 
                    then "**" 
                    else if hasError a then "* " else "  "

    -- ERROR PATTERNS
    type ErrorPattern = (UDPattern, UDPattern)

    parseQuery :: String -> ErrorPattern
    parseQuery q = (read (replace q head), read (replace q last))
        where 
            replace s f = case  s R.=~ "\\{([^}]*)\\}" :: (String,String,String) of
                (before,"",after) -> s
                -- head and tail remove {}
                (before,match,after) -> before ++ f (splitOn "->" (tail $ init match)) ++ replace after f
                