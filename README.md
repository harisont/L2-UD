# L2-UD
Tools for working with [L1-L2 parallel UD treebanks](https://aclanthology.org/W17-6306.pdf).

## Installation
(requires [the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/))

1. clone this repository
2. move inside the corresponding folder and run
   ```
   stack install
   ```

## Usage

### Querying parallel L1-L2 treebanks
To return the set of parallel L1-L2 sentences matching an error pattern, run

```
l2-ud match L1-TREEBANK L2-TREEBANK PATTERNS [OPTIONS]
```

where:

- `L1-TREEBANK` is the CoNNL-U file containing correction hypotheses
- `L2-TREEBANK` is the CoNNL-U file containing original learner sentences
- `PATTERNS` is a list of space-separated [L1-L2 patterns](l1-l2-patterns) or the path to a file containing an L1-L2 pattern per line (see the [saved queries folder](queries) for examples).

By default, the `match` command prints the list of sentence IDs of the sentences matching the pattern.

Available `OPTIONS`:

- `--help`, `-h`: show usage instructions
- `--markdown`, `-m`: rather than sentence IDs, output a markdown report showing the sentences with matches highlighted in bold, like [this one](results/sv/S-FinV-example.md)
- `--conllu DIR`, `-c DIR`: rather than printing sentence IDs to the standard output, extract the pairs of subtrees matching the pattern and write them to an `L1.conllu` and an `L2.conllu` file in the given `DIR`ectory

### Extracting error patterns (__CURRENTLY UNDER DEVELOPMENT__)
Return the [error patterns](#l1-l2-patterns) contained in an L1-L2 treebank.

```
l2-ud extract L1-TREEBANK L2-TREEBANK
```

Where:

- `L1-TREEBANK` is the CoNNL-U file containing correction hypotheses
- `L2-TREEBANK` is the CoNNL-U file containing original learner sentences

## L1-L2 patterns
An L1-L2 error pattern is a "parallel" [`gf-ud`](https://github.com/GrammaticalFramework/gf-ud) pattern[^1], i.e. essentially a pair of UD patterns.
For conciseness, instead of writing full pairs of patterns, L1-L2 patterns are written as single UD patterns with discrepancies enclosed in curly braces. For instance, the pattern

```
AND [POS "DET", FEATS_ "Gender={Masc->Fem}"]
```

reads as "feminine determiners corrected with their masculine form", or "feminine determiners that should have been masculine" and is expanded to two `gf-ud` patterns:

- `AND [POS "DET", FEATS_ "Gender=Masc"]`, to be looked in the L1 corrections treebank
- `AND [POS "DET", FEATS_ "Gender=Fem"]`, to be looked for in the L2 treebanks of original learner sentences.

### L2-only patterns
For some types of error, an L2 pattern is sufficient to concisely describe an error. 
When that is the case, it is possible to write a single UD pattern `P`, which is expanded to a pair $\langle$ `TRUE`, `P` $\rangle$.

### Variables (__EXPERIMENTAL__)
To avoid enumerating all combinations of values for categorial attributes, it is possible to use variables, i.e. capital letters preceded by a `$` sign.

For example, rather than writing 

```
AND [POS "DET", FEATS_ "Gender={Masc->Fem}"]
AND [POS "DET", FEATS_ "Gender={Fem->Masc}"]
```

it is possible to write

```
AND [POS "DET", FEATS_ "Gender={$A->$B}"]
```

where `A` is assumed to be different from `B`.

Writing

```
AND [POS "$A", FEATS_ "Gender={$A->$B}"]
```

(using the identifier `A` twice for two different attributes) does not constitute a problem. The `$A` in `POS "$A"` will be replaced with all possible values of UPOS tags.

Variables are currently supported for morphological features, Universal POS tags and dependency relations with no subtypes. 

For attributes with many possible values, like `POS`, querying can become very slow. 

### Example queries
- generalized determiner-noun gender agreement error:
  ```
  TREE_ (AND [POS "NOUN", FEATS_ "Gender=$A"]) [AND [POS "DET", FEATS_ "Gender={$A->$B}"]]
  ```
- missing determiner with possessives (Italian):
  ```
  TREE (POS "NOUN") [{DEPREL "det", -> } DEPREL "det:poss"]
  ``` 
  (mind the position of the comma, see [issue #1](https://github.com/harisont/L2-UD/issues/1) for details).
- V2 order violation when the first token is an adverb (Swedish):
  ```
  SEQUENCE [POS "ADV", OR [POS "VERB", POS "AUX"], DEPREL_ "nsubj"]
  ```

For more examples, check the [saved queries folder](queries).

[^1]: The syntax of `gf-ud`'s pattern matching language is described extensively [here](https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/patterns.md).