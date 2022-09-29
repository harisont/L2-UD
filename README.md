# L2-UD
Tools for working with UD treebanks of learner texts.

## Usage
```
stack run -- PATH-TO-L1-TREEBANK PATH-TO-L2-TREEBANK L1-PATTERN L2-PATTERN [--linearize]
```

### Example queries
- missing determiner with possessives:
  - L1-PATTERN: `'TREE (POS "NOUN") [DEPREL "det", DEPREL "det:poss"]'`
  - L2-PATTERN: `'TREE (POS "NOUN") [DEPREL "det:poss"]'`
- masculine noun with feminine determiner:
  - L1-PATTERN: `'TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender=Masc"]]'`
  - L2-PATTERN: `'TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender=Fem"]]'`