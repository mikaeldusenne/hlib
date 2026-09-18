# Revue de hlib — 18 septembre 2026

Base examinée : `a116d55c803cb80f1d68438034b6f2be21e46270` (2018).
Périmètre : les 23 modules, les fichiers de construction, le squelette de tests,
la documentation et la distribution. Les grands blocs commentés ont également
été lus ; ils ne constituent pas des fonctionnalités disponibles. Les cinq
tables F ont des dimensions cohérentes (34 × 19), mais leurs valeurs n'ont pas
été certifiées numériquement une par une.

## Appréciation générale

C'est une boîte à outils personnelle avec une vraie cohérence d'usage : les
listes, fractions, tableaux et statistiques se combinent facilement pour des
scripts. L'emploi de fonctions pures et de générateurs aléatoires explicites
est un bon point. Le volume reste suffisamment petit pour comprendre l'ensemble.
Il n'y a pas besoin d'en faire un framework ni de supprimer son vocabulaire.

En revanche, le contrat réel était essentiellement dans la tête de l'auteur.
Des noms rassurants (`safe_nth`, `Read`, `Maybe`) masquaient des boucles ou des
exceptions ; plusieurs fonctions mathématiques renvoyaient un résultat faux sur
des entrées ordinaires. L'absence de tests rendait ces erreurs silencieuses.
Les principaux risques sont donc la correction, les préconditions implicites
et la reproductibilité, davantage que l'esthétique du code.

La remise à niveau conserve les modules et les noms publics. Les corrections
sont ciblées ; les changements plus structurants sont proposés ci-dessous.
Elle ne rend pas l'ensemble de la bibliothèque sûr ou numériquement validé.

## Défauts corrigés dans cette branche

| Priorité | Défaut initial et cas révélateur | Correction |
| --- | --- | --- |
| Haute | Les indices XLSX sont devenus des newtypes incompatibles avec `DF.Point` | Conversion explicite aux frontières et test d’une feuille décalée |
| Haute | `Json.lookup` traite `Object` comme un `HashMap Text`, incompatible avec Aeson 2 | `Key`/`KeyMap`, plus deux variantes retournant `Maybe` |
| Haute | `safe_nth 3 [10,20]` se rappelle indéfiniment sur `[]` | Retourne `Nothing` |
| Haute | `median [1,2,3]` vaut 1.5 ; `median [1,2,3,4]` vaut 2 | Sélection correcte selon la parité |
| Haute | `choose 5 0` et `choose 5 1` échouent à cause de `reduce` | Produits avec élément neutre et cas limites explicites |
| Haute | Le rendu HTML insère directement le texte et utilise l'échappement Haskell des attributs | Échappement HTML du texte et des valeurs ; noms et URL restent à contrôler |
| Moyenne | `transpose []` produit une infinité de listes vides ; lignes irrégulières partielles | Sémantique de `Data.List.transpose` |
| Moyenne | `flatten []`, `flatten [[1]]`, `reduce (+) [1]` échouent | `concat` et singleton accepté |
| Moyenne | Taille de découpe nulle, séparateur vide ou remplacement de motif vide : absence de progression | Erreur explicite pour découpe/séparation ; remplacement vide sans effet |
| Moyenne | `rowN` utilise le nombre de lignes comme largeur | Extraction correcte d'une matrice rectangulaire |
| Moyenne | `Requester.build GET` manque, alors que `GET` est exposé | Implémentation du constructeur ; ajout de `sendWith` pour réutiliser un manager |
| Moyenne | K-means s'arrête lorsqu'un seul centre se stabilise ; un groupe vide perd ses dimensions | Tous les centres doivent converger ; conservation des centres vides et validation des entrées |

Les tests couvrent ces cas, quelques usages existants et quatre propriétés.
Les changements de sémantique sont recensés dans `ChangeLog.md` : notamment,
une chaîne HTML déjà échappée sera rééchappée, et une transposition irrégulière
ne s'arrête plus à la première ligne vide.

## Défauts restants : à traiter avant un usage exigeant

### Fractions et conversions

Dans `Maths`, `Fraction 1 0` est constructible. `simplify (Fraction 0 0)` devient
même zéro, et les comparaisons par produits croisés n'ont plus de sens avec ces
valeurs. `pgcd 5 0` divise par zéro. Le `Read` de `Fraction` appelle un parseur
partiel sur les jetons : `readMaybe` n'est donc pas une protection suffisante.
La notation scientifique et les signes ne sont pas traités uniformément.
`fact (-1)` ne termine pas, et `prettyBytes 1` affiche une unité kB sans conversion.
`sumOfDigits 99` renvoie 9 : c'est une réduction répétée des chiffres, pas leur
simple somme (18). Le nom et le contrat méritent d'être clarifiés.

Proposition : ajouter un constructeur validé et un parseur `Either`, tester les
lois arithmétiques sur des dénominateurs non nuls, puis discuter une représentation
fondée sur `Rational`. Remplacer le type maintenant casserait les constructeurs,
les accesseurs et potentiellement les formats utilisés par d'autres projets.

### Statistiques

`qnorm` est littéralement `undefined`. `cloppearson` parcourt une distribution
binomiale pour un `p` donné ; cela ne suffit pas à établir un intervalle exact
pour une proportion inconnue. Il faut une définition précise et des comparaisons
avec une référence indépendante avant de présenter ce nom comme une garantie.

`f_stat_critical` prend une probabilité cumulée inférieure malgré son paramètre
nommé alpha ; les tables stockées et leur générateur emploient une probabilité
de queue supérieure. Le retour `Maybe` ne valide rien : la fonction enveloppe
systématiquement le résultat dans `Just`. L'ANOVA hérite de cette ambiguïté et
suppose des groupes non vides, des degrés de liberté valides et une variance
résiduelle exploitable. Corrélation et covariance tronquent les paires de
longueurs différentes. `normalr` calcule `log ra`, avec un endpoint zéro possible.
La table normale traite toute sa ligne zéro comme `pnorm 0` à cause de `signum 0`.
Les routines d'échantillonnage ne bornent pas la taille demandée et utilisent
un modulo pour choisir l'indice, avec un biais théorique.

Proposition : valider les domaines et définir explicitement les probabilités,
puis confronter les résultats à des jeux de référence R ou à une bibliothèque
statistique maintenue. Garder les wrappers familiers. Aucun résultat scientifique
ne doit être considéré comme validé par la seule réussite des tests de cette PR.

### Parsing et encodage

`CSV.parseCSV ',' "\"unfinished"` termine sur un motif manquant. Une ligne `""`
ou `\"\"` est éliminée ; cela confond une valeur vide avec une ligne absente.
`guessSep` compte les séparateurs à l'intérieur des guillemets. Les concaténations
en fin de liste rendent les grandes cellules coûteuses.

`Bases` ne valide pas tous les chiffres : un caractère absent peut devenir la
longueur de l'alphabet. Les bases 0/1, nombres négatifs et débordements `Int`
posent problème ; base 64 n'est pas implémentée. `Read Color` peut échouer au
lieu de renvoyer une liste de parses vide. Le constructeur accepte des canaux
hors de `[0,1]`.

`QuotedPrintable` recode UTF-8 manuellement avec des frontières incorrectes :
U+0080 produit `=02` au lieu de `=C2=80`, et U+0800 produit `=E0=80` au lieu de
`=E0=A0=80`. DEL passe littéralement. Le compteur de longueur inclut les lignes
antérieures après un saut de ligne, ce qui provoque des coupures prématurées ;
un CRLF existant conserve aussi un CR encodé supplémentaire. La promesse de conformité MIME
n'est pas tenue. Proposition : s'appuyer sur `Data.Text.Encoding` pour UTF-8 et
vérifier l'encodage MIME avec les vecteurs de la RFC 2045 et un décodeur indépendant.

Ces corrections doivent préciser les comportements attendus ; un remplacement
silencieux du parseur CSV par un autre changerait notamment la gestion des lignes
vides. Une API `parseCSVChecked` additive serait une bonne première étape.

### Effets et dépendances à la machine

Les chemins absolus dans `List.dico`, `Smiley.smileyList*` et `Stats.path_data_stats`
sont conservés comme compatibilité, mais ne sont pas portables. `loadMatrix`
suppose un répertoire courant particulier et `uploadFile` un script personnel.
Ajouter des variantes prenant explicitement un chemin permettrait de garder les
anciens raccourcis. Les paramètres devraient précéder une éventuelle configuration
XDG ; nul besoin de l'imposer aux fonctions pures.

`Hunix` emploie correctement `proc` plutôt qu'une concaténation de shell. Cela
évite cette classe d'injection, mais les arguments commençant par `-` ne sont pas
protégés par `--`. `getFileMimeType` découpe une sortie humaine au premier `:`,
ce qui échoue sur certains noms de fichiers. `fsize` dépend des unités de `du`.
`mvi` vérifie seulement les fichiers existants avant de renommer : vérification
et écriture ne sont pas atomiques. `mkdir_noexists` a une course similaire.
`exec` convertit l'échec du processus en `error`, sans type d'erreur structuré.

`Requester` reçoit tout le corps en mémoire et transforme les octets via Char8.
L'ajout de `sendWith` permet le partage des connexions, mais le statut, les en-têtes,
les délais et le décodage doivent encore être gérés au niveau de l'appelant.
Le générateur de tables R lance des processus dont il ne vérifie pas correctement
le statut de sortie et lit paresseusement les pipes.

## Inventaire des 23 modules

| Module | Appréciation et prochaine amélioration utile |
| --- | --- |
| [Bases](../src/Bases.hs) | Petit et lisible, mais domaine valide trop implicite ; parsing validé et débordements à traiter. |
| [CSV](../src/CSV.hs) | Gère déjà les guillemets doublés, CRLF et sauts intégrés ; ajouter erreurs explicites et fixtures limites. |
| [Colors](../src/Colors.hs) | Représentation simple ; valider canaux et parsing, fixer la casse hex acceptée. |
| [Constants](../src/Constants.hs) | Les longues décimales restent des `Double` inférés ; préciser les types et supprimer l'illusion de précision. |
| [DF](../src/DF.hs) | Pratique pour des scripts, mais listes parallèles sans invariant ; `Sheet1` imposée, feuille vide et cellules d'erreur partielles, matrice dense potentiellement énorme. |
| [Distribution_Tables](../src/Distribution_Tables.hs) | Dimensions vérifiées ; provenance, convention alpha et sentinelle 64 bits à documenter/valider. |
| [Html](../src/Html.hs) | Défaut d'échappement corrigé ; éléments vides, espaces ajoutés, noms et contextes script/style encore rudimentaires. |
| [Hunix](../src/Hunix.hs) | Argument lists plutôt que shell : bon choix ; unités, options, exceptions et atomicité à revoir. |
| [Json](../src/Json.hs) | Compatible Aeson 2, helpers sûrs disponibles ; conserver les anciens partiels seulement pour compatibilité. |
| [Kmeans](../src/Kmeans.hs) | Deux erreurs locales corrigées ; exposer ensuite tolérance, budget d'itérations et stratégie d'initialisation, tester le coût objectif. |
| [List](../src/List.hs) | Cœur utile ; réduire doublons avec `Data.List` progressivement, clarifier les conventions divergentes et les entrées vides. |
| [Maths](../src/Maths.hs) | Type fraction riche mais invariants absents ; priorité au contrat avant une substitution de représentation. |
| [Matrix](../src/Matrix.hs) | `rowN` corrigé ; constructor/`readMatrix`/`elementwise` ne garantissent pas les dimensions, `toMatrix` a une représentation incohérente avec les autres helpers. |
| [Misc](../src/Misc.hs) | Beaucoup d'alias de fonctions standard ; ne pas les renommer en masse, documenter les prédicats ASCII et les partiels. |
| [Paths](../src/Paths.hs) | Concaténation de slashs, pas sémantique système ; adopter `System.FilePath` dans le nouveau code. |
| [Pixels](../src/Pixels.hs) | Six groupes seulement, motifs `[x,y]` partiels, coût pixels × points ; une table de pixels serait une amélioration mesurable. |
| [QuotedPrintable](../src/QuotedPrintable.hs) | Défauts de correction démontrables ; ne pas présenter ce module comme encodeur MIME utilisable sans validation. |
| [Requester](../src/Requester.hs) | GET et réutilisation du manager ajoutés ; réponse structurée et encodage explicite seraient les ajouts suivants. |
| [SVG_creator](../src/SVG_creator.hs) | Suffisant pour dessins de confiance ; composantes de couleur brutes, données non validées et parsing hex partiel. |
| [Smiley](../src/Smiley.hs) | Bonne séparation partielle pur/IO ; chemin paramétrable, mapping vide pouvant boucler et frontières Unicode à traiter. |
| [Stats](../src/Stats.hs) | Utilitaires descriptifs plus fiables après correction ; inférence et échantillonnage restent expérimentaux. |
| [Trees](../src/Trees.hs) | Instances Functor/Foldable/Traversable cohérentes à la lecture ; `Read` et `Show` incompatibles, `toList` quadratique par concaténation. |
| [Tuple](../src/Tuple.hs) | Faible complexité ; ajouter signatures et variantes sûres des conversions de listes. |

## Structure, conventions et publication

Le problème initial n'était pas l'utilisation de Stack : c'était un snapshot
LTS 10.7 ancien, un fichier Cabal ignoré, les métadonnées du template, 22 dépendances
additionnelles non bornées, aucune CI et un test qui affichait simplement qu'il
n'était pas implémenté. La branche versionne un seul manifeste Cabal, garde Stack
avec un snapshot récent, supprime les stubs inutilisés, borne les dépendances et
retire les imports/dépendances directs devenus inutiles. Le build reste assez
lourd, surtout à cause de XLSX et TLS.

Haskell2010 reste déclaré explicitement. GHC recommande GHC2024 pour le nouveau
code ; activer toutes ses extensions d'un coup sur cette bibliothèque à types
largement inférés ne serait pas une amélioration gratuite. Les warnings sont
activés, sans `-Werror` global : les omissions de signatures, motifs partiels et
défauts de style sont visibles, avec une dette à résorber progressivement.
Le monomorphisme est concret : `s²` est inférée en `Double`, contrairement à
`σ²`, qui possède une signature polymorphe. Les tests et exemples respectent
ces types existants. Les noms génériques sont gardés ; des exports explicites et un namespace `Hlib.*`
seraient souhaitables à terme, avec modules de compatibilité et inventaire des
usages, pas par renommage immédiat.

La distribution est vérifiée par `cabal check`, Haddock et reconstruction d'une
archive extraite. La CI ne publie rien automatiquement. Le texte BSD existant
reste intact, mais contient une attribution factice à confirmer avant publication.
La version 0.2.0.0 est préparatoire et signale les changements de support et de
comportement ; aucun tag ni paquet Hackage n'est créé par cette revue.

## Suite proposée, par ordre d'intérêt

1. Contrats sûrs additifs : fractions, CSV, matrices, domaines statistiques et
   chemins explicites. Ajouter les tests de régression correspondants.
2. Validation numérique : jeux de référence, probabilités clairement nommées,
   correction/suppression des promesses non tenues (`qnorm`, `cloppearson`).
3. Signature et exports : stabiliser ce qui doit réellement être public, distinguer
   les fonctions utilitaires des démonstrations, garder des wrappers historiques.
4. Performance guidée par mesure : concaténations de listes, groupes/fréquences,
   pixels, grosses feuilles ; ajouter des benchmarks seulement sur les vrais usages.
5. Dépendances optionnelles : envisager des sous-bibliothèques pour XLSX, images
   et réseau si des consommateurs du noyau souffrent du temps de compilation.

## Vérification et sources

Les résultats de compilation et de tests de cette intervention sont consignés
séparément dans [VALIDATION.md](VALIDATION.md). La lecture complète des sources
n'est pas une preuve exhaustive des fonctions : les tests ajoutés portent sur
les corrections et des comportements représentatifs, pas sur chaque export.

Sources primaires consultées pour les choix de maintenance :

- [Cabal : description des paquets](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html)
- [GHC : éditions du langage](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/control.html)
- [Stackage LTS 24.59](https://www.stackage.org/lts-24.59)
- [GHCup : gestion des outils](https://www.haskell.org/ghcup/guide/)
- [Aeson 2 : KeyMap](https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson-KeyMap.html)
- [Haskell PVP](https://pvp.haskell.org/)
- [RFC 2045 : quoted-printable](https://www.rfc-editor.org/rfc/rfc2045)
