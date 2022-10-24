# L2-UD
Tools for working with UD treebanks of learner texts.

## Usage
```
stack run -- PATH-TO-L1-TREEBANK PATH-TO-L2-TREEBANK L1-PATTERN L2-PATTERN [--linearize]
```

By default, output files are created in the `out` directory and called `L1.connlu` and `L2.conllu`

### Example queries

#### Current format
- missing determiner with possessives:
  - L1-PATTERN: `'TREE (POS "NOUN") [DEPREL "det", DEPREL "det:poss"]'`
  - L2-PATTERN: `'TREE (POS "NOUN") [DEPREL "det:poss"]'`
- masculine noun with feminine determiner:
  - L1-PATTERN: `'TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender=Masc"]]'`
  - L2-PATTERN: `'TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender=Fem"]]'`

#### Hypotetical simplified format
- missing determiner with possessives: `'TREE (POS "NOUN") [{DEPREL "det"} -> {}, DEPREL "det:poss"]'`
- masculine noun with feminine determiner: `'TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender={Masc}->{Fem}"]]'`, or even better with variables (more general): `'TREE (AND [POS "NOUN", FEATS_ "Gender=$X"]) [AND [DEPREL "det", FEATS_ "Gender=$Y"]]'`, implying that `X` $\neq$ `Y`