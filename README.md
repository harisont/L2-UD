# L2-UD
Tools for working with UD treebanks of learner texts.

## Usage

### Querying parallel L1-L2 treebanks
```
stack run -- PATH-TO-L1-TREEBANK PATH-TO-L2-TREEBANK PATTERN(S) [--linearize]
```

Note that:

- `PATTERN(S)` can be a pair of an L1 and an L2 pattern or a single L1-L2 pattern (see [below](#example-queries)). The syntax of the pattern matching language is described extensively [here](https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/patterns.md).
- output CoNNL-U files are created in the `out` directory and called `L1.connlu` and `L2.conllu`

#### Example queries

- missing determiner with possessives:
  - L1-PATTERN: `'TREE (POS "NOUN") [DEPREL "det", DEPREL "det:poss"]'`
  - L2-PATTERN: `'TREE (POS "NOUN") [DEPREL "det:poss"]'`
  - single L1-L2 PATTERN: `'TREE (POS "NOUN") [{DEPREL "det", -> ,} DEPREL "det:poss"]'` (mind the comma, see [#1](https://github.com/harisont/L2-UD/issues/1))
- masculine noun with feminine determiner:
  - L1-PATTERN: `'TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender=Masc"]]'`
  - L2-PATTERN: `'TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender=Fem"]]'`
  - single L1-L2 PATTERN: `'TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender={Masc->Fem}"]]'`
