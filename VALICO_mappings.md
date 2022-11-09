# VALICO-HST mappings

| VALICO | Description | Example | HST | Comments |
| --- | --- | --- | --- | --- |
IDG | incorrectly Inflected Determiner (wrong Gender) | un' uomo | `AND [POS "DET", OR [FEATS_ {"Gender=Masc" -> "Gender=Fem"}, FEATS_ {"Gender=Fem" -> "Gender=Masc"}]]` | mapping attempted but __not working__, variable for gender would be nice


Problem:

```
OR [AND [POS "DET", FEATS_ "Gender={Masc->Fem}"], AND [POS "DET", FEATS_ "Gender={Fem->Masc}"]]
```

is split into

```
OR [AND [POS "DET",FEATS_ "Gender=Masc"],AND [POS "DET",FEATS_ "Gender=Fem"]]
```

and

```
OR [AND [POS "DET",FEATS_ "Gender=Fem"],AND [POS "DET",FEATS_ "Gender=Masc"]]
```

causing a lot of false positives. 

On the other hand, running

```
AND [POS "DET", FEATS_ "Gender={Masc->Fem}"]
```

and 

```
AND [POS "DET", FEATS_ "Gender={Fem->Masc}"]
```

would give the expected results.

I guess variables would solve the problem because one would write

```
OR [AND [POS "DET", FEATS_ "Gender={X->Y}"]]
```

which is split into

```
AND [POS "DET", FEATS_ "Gender=X"]
```

and

```
AND [POS "DET", FEATS_ "Gender=Y"]
```

Without variables, a possibility is to allow a language-external operator `+` allowing to run 2+ queries:

```
AND [POS "DET", FEATS_ "Gender={Masc->Fem}"] + AND [POS "DET", FEATS_ "Gender={Fem->Masc}"]
```

... or simply to allow $n$ query patterns (multiple queries to run) as command line arguments.