# L2-UD
Tools for working with parallel L1-L2 UD treebanks.

## L1-L2 error patterns
An L1-L2 error pattern is a "parallel" [`gf-ud`](https://github.com/GrammaticalFramework/gf-ud) pattern[^1].
This means that the pattern encodes, enclosed in curly braces, the differences to look for in a parallel UD treebank. For instance, the pattern

```
AND [POS "DET", FEATS_ "Gender={Masc->Fem}"]
```

reads as "feminine determiners corrected with their masculine form", or "feminine determiners that should have been masculine" and is expanded to two `gf-ud` patterns:

- `AND [POS "DET", FEATS_ "Gender=Masc"]`, to be looked in the L1 corrections treebank
- `AND [POS "DET", FEATS_ "Gender=Fem"]`, to be looked for in the L2 treebanks of original learner sentences.

Some more example patterns:

- missing determiner with possessives:
  ```
  TREE (POS "NOUN") [{DEPREL "det", -> } DEPREL "det:poss"]
  ``` 
  (mind the comma, see [#1](https://github.com/harisont/L2-UD/issues/1)).
- masculine noun with feminine determiner:
  ```
  TREE (AND [POS "NOUN", FEATS_ "Gender=Masc"]) [AND [DEPREL "det", FEATS_ "Gender={Masc->Fem}"]]
  ```

## Extracting error patterns
Return the [error patterns](#l1-l2-error-patterns) contained in a parallel L1-L2 treebank.

```
stack run -- extract L1-TREEBANK L2-TREEBANK [--markdown]
```
Note that:

- the L1 treebank contains corrections
- the L2 treebank contains the original learner sentences
- running `extract` with the `--markdown` flag outputs a markdown report instead of just the list of extracted L1-L2 patterns

## Querying parallel L1-L2 treebanks
Given a parallel L1-L2 treebank and one or more [error patterns](#l1-l2-error-patterns), return the set of sentences containing errors matching the pattern.

```
stack run -- match L1-TREEBANK L2-TREEBANK PATTERNS [--markdown]
```

Note that:

- again, the L1 treebank contains corrections while the L2 treebank contains the original learner sentences
- the two treebanks should be parallel, i.e. sentence-aligned
- `PATTERNS` is a list of space-separated L1-L2 query patterns (see [above](#l1-l2-error-patterns)) or the path to a file containing an L1-L2 pattern per line (see the [saved queries folder](queries) for examples).
- running `match` with the `--markdown` flag outputs a markdown report instead of just the list of IDs of the sentences matching the L1-L2 patterns

[^1]: The syntax of `gf-ud`'s pattern matching language is described extensively [here](https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/patterns.md).