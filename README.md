# L2-UD
Tools for working with [L1-L2 parallel UD treebanks](https://aclanthology.org/W17-6306.pdf).

## Installation
(requires [the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/))

1. clone this repository
2. move inside the corresponding folder and run[^1]
   ```
   stack install
   ```
   This will install two executables: the complete CLI program, `l2-ud`, and a GUI for the match command, `l2-ud-gui`
3. (optional) create a shortcut to start the GUI in one click:
   - on Linux, copy [the desktop file](l2-ud-gui.desktop) wherever you have your desktop files (`~/.local/share/applications/` maybe?)
   - on Windows, create a Desktop shortcut to [l2-ud-gui.ps1](l2-ud-gui.ps1), right click on it and make it exectuable on click by setting `Properties > Target` to `powershell.exe -ExecutionPolicy Bypass -File "WHATEVER-PATH-WAS-ALREADY-THERE"` (mind the quotes)

## Usage

### Querying parallel L1-L2 treebanks
To return the set of parallel L1-L2 sentences matching an error pattern, run

```
l2-ud match L1-TB L2-TB PATTERNS [OPTS]
```

where:

- `L1-TB` is the CoNNL-U file containing correction hypotheses
- `L2-TB` is the CoNNL-U file containing original learner sentences
- `PATTERNS` is a list of space-separated [L1-L2 patterns](l1-l2-patterns) or the path to a file containing an L1-L2 pattern per line (see the [saved queries folder](queries) for examples).

By default, the `match` command prints the list of sentence IDs of the sentences matching the pattern.

Available `OPTS`:

- `--help`, `-h`: show usage instructions
- `--markdown`, `-m`: rather than sentence IDs, output a markdown report showing the sentences with matches highlighted in bold, like [this one](results/sv/S-FinV-example.md)
- `--conllu=DIR`, `-cDIR`: on top of printing sentence IDs to the standard output, extract the pairs of subtrees matching the pattern and write them to an `L1.conllu` and an `L2.conllu` file in the given `DIR`ectory (if no directory is specified, files are created in the current folder)
- `--replacement-rules=RULE_OR_PATH`, `-rRULE_OR_PATH`: apply a custom [replacement rule](https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/patterns.md#the-semantics-of-replacements) on all (L1 and L2) matches. This can be useful, for example, to prune trees ignoring certain branches. 

### Extracting error patterns (__CURRENTLY UNDER DEVELOPMENT__)
Return the [error patterns](#l1-l2-patterns) contained in an L1-L2 treebank.

```
l2-ud extract L1-TB L2-TB [OPTS]
```

Where:

- `L1-TB` is the CoNNL-U file containing correction hypotheses
- `L2-TB` is the CoNNL-U file containing original learner sentences

Available `OPTS`:

- `--help`, `-h`: show usage instructions
- `--markdown`, `-m`: rather than sentence IDs, output a markdown report showing the sentences with errors highlighted in bold next to the error patterns that were detected
- `--conllu=DIR`, `-cDIR`: on top of printing the error patterns to the standard output, extract the pairs of subtrees where the errors were found and write them to an `L1.conllu` and an `L2.conllu` file in the given `DIR`ectory (if no directory is specified, files are created in the current folder)

### Retrieving similar examples
Given an L1-L2 sentence pair, return similar examples from an L1-L2 treebank, by:

1. annotating the sentences in UD using UDPipe2 (requires an internet connection)
2. extracting error patterns from them (analogous to [`l2-ud extract`](#extracting-error-patterns-currently-under-development))
3. querying the treebank with such error patterns (analogous to [`l2-ud match`](#querying-parallel-l1-l2-treebanks))

```
l2-ud example L1-TB L2-TB L1-SENTENCE L2-SENTENCE LANG [OPTS]
```

Where:

- `L1-TB` is the CoNNL-U file containing correction hypotheses
- `L2-TB` is the CoNNL-U file containing original learner sentences
- `L1-SENTENCE` is a correct sentence
- `L2-SENTENCE` is a sentence containing 1+ grammatical errors
- `LANG` is the name of the [UDPipe 2 model](https://ufal.mff.cuni.cz/udpipe/2/models) to be used for annotating the sentences. The default model for each language is called the English name of the language, lowercased, e.g. `swedish`   

Available `OPTS`:

- `--help`, `-h`: show usage instructions
- `--verbose`, `-v`: show intermediate results (UD-annotated example sentences and extracted patterns)
- `--markdown`, `-m`: rather than sentence IDs, show similar examples found in the treebank a markdown report. 

## L1-L2 patterns
An L1-L2 error pattern is a "parallel" [`gf-ud`](https://github.com/GrammaticalFramework/gf-ud) pattern[^2], i.e. essentially a pair of UD patterns.
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

## Citation
If you use this software in your research, you are welcome to cite

```
@inproceedings{masciolini-2023-query,
    title = "A query engine for {L}1-{L}2 parallel dependency treebanks",
    author = "Masciolini, Arianna",
    booktitle = "Proceedings of the 24th Nordic Conference on Computational Linguistics (NoDaLiDa)",
    month = may,
    year = "2023",
    address = "T{\'o}rshavn, Faroe Islands",
    publisher = "University of Tartu Library",
    url = "https://aclanthology.org/2023.nodalida-1.57",
    pages = "574--587",
    abstract = "L1-L2 parallel dependency treebanks are learner corpora with interoperability as their main design goal. They consist of sentences produced by learners of a second language (L2) paired with native-like (L1) correction hypotheses. Rather than explicitly labelled for errors, these are annotated following the Universal Dependencies standard. This implies relying on tree queries for error retrieval. Work in this direction is, however, limited. We present a query engine for L1-L2 treebanks and evaluate it on two corpora, one manually validated and one automatically parsed.",
}
```

and

```
@inproceedings{masciolini-etal-2023-towards,
    title = "Towards automatically extracting morphosyntactical error patterns from {L}1-{L}2 parallel dependency treebanks",
    author = "Masciolini, Arianna  and
      Volodina, Elena  and
      Dannlls, Dana",
    booktitle = "Proceedings of the 18th Workshop on Innovative Use of NLP for Building Educational Applications (BEA 2023)",
    month = jul,
    year = "2023",
    address = "Toronto, Canada",
    publisher = "Association for Computational Linguistics",
    url = "https://aclanthology.org/2023.bea-1.50",
    doi = "10.18653/v1/2023.bea-1.50",
    pages = "585--597",
    abstract = "L1-L2 parallel dependency treebanks are UD-annotated corpora of learner sentences paired with correction hypotheses. Automatic morphosyntactical annotation has the potential to remove the need for explicit manual error tagging and improve interoperability, but makes it more challenging to locate grammatical errors in the resulting datasets. We therefore propose a novel method for automatically extracting morphosyntactical error patterns and perform a preliminary bilingual evaluation of its first implementation through a similar example retrieval task. The resulting pipeline is also available as a prototype CALL application.",
}
```
[^1]: If you are on Windows and `stack install` does not work, it might be because of an external dependency, `curl`. Try following [these instructions](win.md).
[^2]: The syntax of `gf-ud`'s pattern matching language is described extensively [here](https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/patterns.md).
