# VALICO-HST mappings

| VALICO | Description | Example | HST | Comments |
| --- | --- | --- | --- | --- |
IDG | incorrectly Inflected Determiner (wrong Gender) | un' uomo | `AND [POS "DET", OR [FEATS_ {"Gender=Masc" -> "Gender=Fem"}, FEATS_ {"Gender=Fem" -> "Gender=Masc"}]]` | mapping attempted but __not working__, variable for gender would be nice
