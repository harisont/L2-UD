# it - sv

## `TREE_ (FEATS_ "VerbForm=Sup") [AND [LEMMA "ha", FEATS_ "Tense=Pres"]]` (presens perfekt)
- 68 hits
- översätts som:
  - passato prossimo attivo (46 + 3 med cop or similar)
  - passato prossimo passivo/composto (`AND [POS "VERB",FEATS_ "Tense=Pres"]`) (?!) (9)
  - trapassato prossimo (1)
  - presente (6)
  - casi particolari (3):
    - "De nederländska studenterna har ännu inte bestämt om de kommer att kommersialisera sin elektriska motorcykel ." - "Gli studenti olandesi devono ancora decidere se metteranno in commercio la loro motocicletta elettrica ."
    - "Det finns även språk härledda från finskan , som har utvecklats separat , kända som meänkieli i Sverige och kända kven i Norge ." - "Esistono anche lingue che derivano da il finlandese , evolute si separatamente , conosciute come Meänkeli in Svezia e Kven in Norvegia ." (particip + "si passivante")
    - "Det har betonats att vi absolut inte kan fortsätta med de som är helt emot Italien ." - "Non possiamo andare avanti insieme a quelli che sono totalmente contro l' Italia , sottolinea ." (egentligen presens)

## `TREE_ (FEATS_ "Tense=Past") [AND [OR [LEMMA "avere", LEMMA "essere"], FEATS_ "Tense=Pres", FEATS_ "Mood=Ind"]]` (passato prossimo)
- 230 hits (ganska talande! används mycket mer än passato remoto. Utan `Mood=Ind` blir det till och med 243)
- översätts som:
  - preteritum (137!)
  - presens perfekt (70?)
  - presens (19? + 1 med adj. copula complement) (but some of this is actually it present passives - maybe the query can be improved...?)
  - preteritum perfekt/pluperfekt (2)
  - speciella fall:
    - "Som tur är var någon hos Sony Australien typ ” Jo , förresten , märkte ni det här ? ” , säger Pall ." - "Per fortuna , qualcuno presso la sede australiana di Sony ha chiesto " Comunque , ragazzi , avete notato questa cosa ? " afferma Pall ."
    - "Cuaron , vars senaste film var den oscarsvinnande Gravity , var enligt rapporterna inte på plats på inspelningen när incidenten skedde ." - "È stato riferito che Cuaron , vincitore di il premio Oscar grazie al suo ultimo film Gravity , non si trovava su il set a il momento di l' incidente ."
    - ...