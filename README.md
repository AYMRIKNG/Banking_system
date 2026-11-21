# Banking_system
Banking systeme in COBOL

## Fonctionnalites
Au demarrage, un ecran de connexion permet de choisir le role :
- **Conseiller** : acces complet aux sous-menus bancaires (mot de passe
  defini au premier lancement, 3 essais maximum).
- **Client** : acces a l'espace client restreint par ID client et code
  PIN a 4 chiffres (compte bloque apres 3 echecs ; un conseiller peut
  definir ou debloquer le PIN).

Les mots de passe et PIN ne sont jamais stockes en clair (hachage
pedagogique dans `hachage.cbl`). La deconnexion renvoie a l'ecran de
connexion ; on quitte le programme depuis l'ecran de connexion.

### Menu conseiller
Le menu conseiller est decoupe en sous-menus (option `6. Deconnexion`).

**1. Clients**
- Ajouter client
- Modifier client (adresse, telephone)
- Rechercher client par nom (ou partie du nom)
- Definir / debloquer PIN client

**2. Comptes**
- Creer compte (Courant, Epargne, Entreprise, Livret A ; un seul `LivretA` par client, plafond des depots 22950.00 ; enregistre une transaction `Ouverture`)
- Lister comptes client (avec IBAN francais, solde signe et decouvert autorise si > 0)
- Cloturer compte (solde rendu, refuse si un credit est en cours ou si le solde est debiteur)
- Verifier un IBAN (validation modulo 97 et verification du compte)
- Definir decouvert autorise (plafond dans `decouvert.dat`, `0` pour supprimer ; interdit sur compte `Epargne`, `LivretA` ou `Cloture`)
- Simuler agios du mois (option `2.6` : methode des nombres debiteurs, tableau detaille des jours debiteurs, aucune modification des fichiers)
- Interets Livret A par quinzaines (option `2.7` : `1` simulation detaillee pour un compte et une annee sans modifier les fichiers, `2` versement idempotent des interets de l'annee pour tous les `LivretA`)

**3. Operations**
- Effectuer transaction (Depot, Retrait, Virement)
  - soldes signes (`PIC S9(9)V99`) et regle de decouvert autorise (`solde - montant >= -plafond`) appliquee a tous les debits (retraits, virements, lots, virements permanents, mensualites, frais, annulations) ; les comptes `Epargne` et `LivretA` ont toujours un plafond de `0`
  - plafond des depots Livret A fixe a `22950.00` (refus sur depot, virement entrant, lot `R08`, virement permanent ; seuls les interets `IntLivret` peuvent faire depasser le plafond)
  - plafond de retrait de 1000.00 par jour et par compte
  - aucune operation possible sur un compte cloture
  - detection de fraude automatique sur Retrait et Virement (gros montant, rafale, vidage y compris passage en negatif)
- Historique des transactions
- Generer releve mensuel (avec IBAN en en-tete)
- Export CSV des transactions (`transactions.csv`)
- Virements permanents : creation et arret (executes en fin de mois)
- Annuler une transaction (Depot, Retrait, Virement : contre-passation)
- Traitement par lot (batch de mouvements avec controle en-tete/fin et rejets dans `rejets.txt`)

**4. Credits**
- Accorder credit (mensualite calculee par annuites constantes, scoring client prealable via `scoring.cbl`)
- Rembourser credit (paie une mensualite)
- Simuler credit (tableau d'amortissement dans `amortissement.txt`)

**5. Banque**
- Interets epargne (taux de la banque, une fois par mois)
- Traitement fin de mois, une seule fois par mois :
  1. interets epargne
  2. agios et commissions d'intervention sur comptes `Courant` et `Entreprise` debiteurs (prelevement force, transactions `Agios` et `Commission`, idempotent par compte et par mois, journalise sous `AGIOS`)
  3. prelevement automatique des mensualites de credit
  4. virements permanents
  5. frais de tenue de compte (2.00 Courant, 5.00 Entreprise)
- Statistiques (comptes par type, total des soldes, credits en cours, alertes...)
- Classement des clients par avoirs (verbe SORT)
- Consulter le journal d'audit (`journal.log`, filtre par date)
- Alertes fraude (consulter et marquer comme traitees)
- Controle integrite des soldes (rapprochement comptes / transactions, rapport dans `controle.txt`)
- Cloture annuelle / archivage (archive les transactions `<= AAAA` dans `transaction_AAAA.dat`, genere les reports a nouveau `Report` / `ReportDeb` et l'IFU dans `ifu_AAAA.txt`)

**7. International**
- Consulter les taux de change (`devises.dat`, unites de devise pour 1 EUR)
- Mettre a jour un taux (devise existante ou nouvelle, taux > 0, journal `TAUX`)
- Convertir un montant (achat `A` ou vente `V` de devise via `conversion.cbl`, marge bancaire de 1,50 %, affiche taux moyen, taux applique, montant EUR et marge banque)
- Virement international :
  - compte source actif, devise existante, montant en devise
  - validation d'IBAN etranger generique (`iban.cbl`, mode `E` : 2 lettres pays, 2 chiffres cle, 11 a 30 caracteres alphanumeriques, conversion A=10..Z=35, modulo 97 = 1)
  - validation du BIC (8 ou 11 caracteres : 4 lettres banque, 2 lettres pays, 2 alphanumeriques, 3 alphanumeriques optionnels)
  - option de frais `SHA` (15.00 EUR) ou `OUR` (25.00 EUR)
  - transactions `VirInter` (montant EUR) et `FraisInter` (frais du virement), credit de `banque.dat` des frais et de la marge, enregistrement dans `virinter.dat`, journal `VIR INTER`
  - controle fraude standard + alerte supplementaire `LCB-FT` dans `alerte.dat` des 10000.00 EUR (seuil de declaration de soupcon)
- Historique des virements internationaux (filtre optionnel par compte, total EUR)

**8. Moyens de paiement**
- Emettre une carte bancaire (compte actif hors `Epargne`, numero 16 chiffres `49730130` + 7 chiffres sequentiels + cle de Luhn via `luhn.cbl`, expiration a 4 ans, CVV deterministe affiche une seule fois et stocke hache avec sel = 4 derniers chiffres, plafond 7 jours glissants par defaut 1500.00, journal `CARTE EMISE`)
- Paiement par carte (terminal commercant : numero, expiration, CVV, commercant, montant ; controles `C01` a `C07`, opposition automatique apres 3 CVV faux consecutifs, debit + transaction `CB`, enregistrement dans `paiement.dat` des paiements acceptes et refuses, `CONTROLE-FRAUDE` sur paiement accepte, journal `PAIEMENT CB`)
- Opposition sur une carte (par numero complet ou 4 derniers chiffres + compte, motif Perte / Vol / Fraude, confirmation, journal `OPPOSITION`)
- Cartes d'un client (liste masquee `4973 01XX XXXX 1234`, compte, expiration, statut, consommation des 7 derniers jours / plafond)
- `5.` Creer un mandat SEPA (`mandat.dat`, controle compte actif, unicite du RUM, format ICS `FR` + 2 chiffres + `ZZZ` + 6 alphanumeriques, montant max optionnel, type `R` recurrent ou `U` ponctuel, journal `MANDAT`)
- `6.` Revoquer un mandat SEPA (confirmation, passage au statut `R`, journal `REVOCATION`)
- `7.` Traiter un fichier de prelevements SEPA (SDD : controle de structure en phase 1, anti-doublon dans `sdd.dat`, debits `Prelev` avec regle du decouvert et controle fraude, rejets SEPA dans `rejets-sdd.txt`, journal `SDD TRAITE` / `SDD REFUSE`)

### Espace client
Acces restreint au client connecte :
- **1. Mes comptes** : liste uniquement les comptes du client connecte (avec IBAN)
- **2. Historique d'un compte** : uniquement pour un compte appartenant au client
- **3. Effectuer un virement** : uniquement depuis un compte appartenant au client (Depot et Retrait reserves au guichet)
- **4. Changer mon PIN** : verification de l'ancien PIN puis saisie + confirmation du nouveau PIN
- **5. Faire opposition sur ma carte** : opposition sur une carte rattachee a un compte appartenant au client connecte
- **6. Revoquer un mandat SEPA** : revocation d'un mandat portant uniquement sur un compte du client connecte (journal `REVOCATION`)
- **9. Deconnexion**

## Compiler
Linux :
```
cobc -x -o Bank Bank.cbl calcul-credit.cbl iban.cbl hachage.cbl scoring.cbl conversion.cbl luhn.cbl
./Bank
```

Windows (GnuCOBOL installe avec MSYS2 : `pacman -S mingw-w64-ucrt-x86_64-gnucobol mingw-w64-ucrt-x86_64-gcc`) :
```
.\bank.ps1
```
Le script met `COB_UNIX_LF=Y`, sinon les REWRITE cassent les fichiers .dat sous Windows.

## Fichiers
- `banque.dat` : infos de la banque (solde, taux, frais)
- `client.dat` : clients
- `compte.dat` : comptes (soldes signes `PIC S9(9)V99`, signe incorpore en fin de zone)
- `decouvert.dat` : plafonds de decouvert autorise par compte (cree au premier decouvert)
- `transaction.dat` : transactions
- `credit.dat` : credits en cours (cree au premier credit)
- `virement.dat` : virements permanents (cree au premier virement)
- `journal.log` : journal d'audit de toutes les operations
- `alerte.dat` : alertes de fraude (cree a la premiere alerte)
- `conseiller.dat` : hash du mot de passe conseiller (cree au premier lancement)
- `pin.dat` : hash des PIN clients, essais et statut de blocage (cree au premier PIN)
- `controle.txt` : rapport du controle d'integrite des soldes
- `lots.dat` : historique des lots deja traites (anti-doublon)
- `rejets.txt` : lignes rejetees lors du dernier traitement par lot
- `lot-exemple.txt` : exemple de fichier lot pret a l'emploi
- `transaction_AAAA.dat` : archive des transactions de l'annee `AAAA` et anterieures
- `ifu_AAAA.txt` : releve fiscal annuel des interets par client (brut, PFU 30 %, net)
- `devises.dat` : table des taux de change (code devise, taux pour 1 EUR, date)
- `virinter.dat` : historique des virements internationaux (cree au premier virement international)
- `carte.dat` : cartes bancaires emises (numero, compte, expiration, hash CVV, plafond 7j, statut, essais CVV ; ignore par git)
- `paiement.dat` : historique des paiements et refus CB (numero, date, commercant, montant, resultat `OK` ou `C01`..`C07` ; ignore par git)
- `mandat.dat` : mandats de prelevement SEPA (cree au premier mandat)
- `sdd.dat` : historique des fichiers SDD traites (anti-doublon ICS + date)
- `rejets-sdd.txt` : lignes rejetees lors du dernier traitement SDD
- `sdd-exemple.txt` : exemple de fichier de prelevements SEPA pret a l'emploi

## Cartes bancaires (option 8)
Les numeros de carte suivent le format `49730130` (BIN banque) + 7 chiffres sequentiels + chiffre de controle de Luhn (`luhn.cbl`, modes `G` et `V`). Le numero est affiche masque (`4973 01XX XXXX 1234`) partout hors emission, et le CVV n'est jamais stocke en clair.

Codes de refus lors d'un paiement par carte (`paiement.dat`) :
- `C01` : numero de carte invalide (controle de Luhn)
- `C02` : carte inconnue
- `C03` : carte en opposition
- `C04` : carte expiree ou date d'expiration incorrecte
- `C05` : CVV incorrect (3 erreurs consecutives entrainent une opposition automatique)
- `C06` : plafond de paiement sur 7 jours glissants depasse
- `C07` : depassement du decouvert autorise (ou compte indisponible)

## Format des fichiers de lot (option 3.8)
Fichier texte `LINE SEQUENTIAL` (lignes de 60 caracteres max, colonne 1 = type) :
- **En-tete `E`** (1ere ligne, unique) : `E` + numero de lot `9(5)` + date `9(8)` (AAAAMMJJ) + nombre de lignes detail annonce `9(5)`
- **Detail `D`** : `D` + compte `9(10)` + type `A(10)` (`Depot`, `Retrait`, `Virement`, complete par espaces) + montant `9(9)V99` (11 chiffres sans point) + compte destination `9(10)` (`0000000000` hors virement)
- **Fin `F`** (derniere ligne, unique) : `F` + nombre de lignes detail `9(5)` + total des montants `9(11)V99` (13 chiffres sans point)

Codes de rejet (`rejets.txt`) :
- `R01` : compte inconnu
- `R02` : compte cloture
- `R03` : type invalide
- `R04` : montant nul
- `R05` : depassement du decouvert autorise
- `R06` : compte destination inconnu ou cloture
- `R07` : compte destination egal au compte source
- `R08` : plafond livret A depasse

## Scoring credit (option 4.1)
Avant tout octroi de credit, le programme calcule la mensualite puis evalue le dossier client (`CALCULER-SCORE` + sous-programme `SCORING-CREDIT` dans `scoring.cbl`) avant toute modification des fichiers :
- **Revenus mensuels** : `(Depots + Virements recus depuis un compte tiers sur les 90 derniers jours) / 3`
- **Charges mensuelles** : somme des mensualites des credits en cours (`CREDIT-RESTANT > 0`) du client
- **Taux d'endettement** : `(charges + nouvelle mensualite) / revenus * 100`
- **Anciennete** : nombre de mois depuis la plus ancienne transaction sur les comptes du client
- **Alertes ouvertes** : nombre d'alertes fraude non traitees (`N`) sur les comptes du client
- **Epargne** : somme des soldes des comptes `Epargne` du client

Regles de decision :
- Revenus = 0 : **R** (`Aucun revenu regulier`)
- Endettement > 35 % : **R** direct (`Endettement superieur a 35 pour cent`)
- Score initial = 100 (borne entre 0 et 100) :
  - endettement > 30 % : `-25` ; > 20 % : `-10`
  - anciennete < 3 mois : `-20` ; < 12 mois : `-10`
  - chaque alerte fraude ouverte : `-15` (plafond `-45`)
  - compte debiteur : `-10` (`Compte debiteur`)
  - epargne >= 10 % du montant demande : `+10`
- Seuils : **A** (>= 70, octroi automatique), **E** (50 a 69, confirmation conseiller `O/N`), **R** (< 50, refus sans modification). Chaque scoring est trace dans `journal.log` (`SCORING`).

## Agios et commissions d'intervention (option 2.6 et fin de mois)
Les agios sur comptes debiteurs sont calcules par la **methode des nombres debiteurs** (`CALCULER-AGIOS` dans `calcul-agios.cpy`) :
- **Solde de debut de mois** : reconstitue depuis le solde actuel moins l'effet net signe des transactions du mois et ulterieures (memes regles que `rejouer-transaction.cpy`).
- **Parcours journalier** : du 1er au dernier jour du mois (ou jusqu'a la date du jour pour le mois en cours). Pour chaque jour ou le solde de fin de journee est negatif, `nombres debiteurs += |solde|`.
- **Agios** : `nombres debiteurs * taux / 36500` arrondi au centime, avec un taux annuel de **16.00 %** (`WS-TAUX-AGIOS`).
- **Commission d'intervention** : **8.00** par jour ou le solde de fin de journee est strictement inferieur a `-plafond` du decouvert autorise (`decouvert.dat`), plafonnee a **80.00** par mois.
- **Simulation** (menu `Comptes -> 6. Simuler agios du mois`) : affiche le tableau detaille des jours debiteurs et le resume sans modifier aucun fichier.
- **Prelevement en fin de mois** (etape 2 de `Traitement fin de mois`, juste apres les interets) : pour chaque compte `Courant` ou `Entreprise` sans transaction `Agios`/`Commission` sur le mois en cours, preleve de facon forcee `agios + commission` si > 0, enregistre les transactions `Agios` et `Commission`, credite `banque.dat` et trace l'operation dans `journal.log` (`AGIOS`).


## Livret A et calcul des interets par quinzaines (option 2.7)
Le type de compte `LivretA` (option `4 - Livret A` lors de la creation de compte) applique les regles suivantes :
- **Un seul Livret A par client** : toute creation d'un second `LivretA` actif pour le meme client est refusee.
- **Plafond des depots** : **22950.00** (`WS-PLAFOND-LIVRETA`). Tout credit qui ferait depasser ce solde est refuse (creation, depot, virement entrant, lot avec code rejet `R08`, virement permanent comptabilise en echec). Seuls les interets `IntLivret` peuvent faire depasser ce plafond.
- **Regles communes avec Epargne** : jamais de decouvert autorise (`Definir decouvert` refuse), pas de frais de tenue de compte, pas d'agios, et exclu du calcul mensuel `Interets epargne` (option `5.1`).
- **Regle des quinzaines** (`CALCULER-QUINZAINES` dans `calcul-quinzaines.cpy`) :
  - Chaque annee compte **24 quinzaines** : du 1er au 15 (`AAAA-MM-1`) et du 16 a la fin du mois (`AAAA-MM-2`).
  - **Credits** (`Ouverture`, `Depot`, `Virement` recu, `Report`, `IntLivret`) : commencent a rapporter au debut de la **quinzaine suivante** (le 16 du mois ou le 1er du mois suivant).
  - **Debits** (`Retrait`, `Virement` emis, `Cloture`) : cessent de rapporter des le debut de la **quinzaine en cours** (le 1er ou le 16).
  - **Formule** : `interet = somme(soldes de valeur positifs des quinzaines) * taux / 2400`, avec `WS-TAUX-LIVRETA = 2.40 %` et arrondi final au centime. Pour l'annee en cours, le calcul s'arrete a la quinzaine d'aujourd'hui avec un avertissement.
- **Exemple chiffre (annee complete, taux 2,40 %)** :
  - Ouverture le `10/01` de `1000.00` (rapporte a partir du `16/01`, quinzaine `01-2`), depot de `500.00` le `03/03` (rapporte a partir du `16/03`, quinzaine `03-2`), retrait de `200.00` le `20/06` (cesse de rapporter des le `16/06`, quinzaine `06-2`) :
    - `01-1` (1 quinzaine) : `0.00`
    - `01-2` a `03-1` (4 quinzaines) : `1000.00` -> `4 * 1000.00 = 4000.00`
    - `03-2` a `06-1` (6 quinzaines) : `1500.00` -> `6 * 1500.00 = 9000.00`
    - `06-2` a `12-2` (13 quinzaines) : `1300.00` -> `13 * 1300.00 = 16900.00`
    - Total : `(4000 + 9000 + 16900) * 2.40 / 2400 = 29900 * 0.001 = 29.90`.
- **Menu Comptes -> 7. Interets Livret A (quinzaines)** :
  - `1` : simulation detaillee quinzaine par quinzaine pour un compte et une annee (aucune modification des fichiers).
  - `2` : versement des interets de l'annee pour tous les `LivretA` actifs (transaction `IntLivret` datee `AAAA1231`, credit du compte, debit de `banque.dat`, idempotent par compte et par annee, resume et trace dans `journal.log` sous `INTERETS LIVRET`).
## Mandats et prelevements SEPA (option 8)
### Mandats SEPA (`mandat.dat`)
Un mandat autorise un creancier (identifie par son ICS au format `FR` + 2 chiffres + `ZZZ` + 6 caracteres alphanumeriques) a prelever un compte non cloture :
- **RUM** (`X(20)`) : Reference Unique de Mandat (unique dans `mandat.dat`)
- **ICS** (`X(13)`) : Identifiant Creancier SEPA
- **Creancier** (`X(30)`), **Compte** (`9(10)`), **Montant max** (`9(7)V99`, `0` = sans limite)
- **Type** : `R` (recurrent) ou `U` (ponctuel, passe au statut `C` apres son unique prelevement)
- **Statut** : `A` (actif), `R` (revoque), `C` (consomme)

Pour tester `sdd-exemple.txt`, creer le mandat suivant via `8. Moyens de paiement -> 5. Creer un mandat SEPA` :
- Numero de compte : `0000000001` (IBAN `FR7630001000010000000000112`)
- RUM : `RUM-EDF-001`
- ICS : `FR12ZZZ123456`
- Nom du creancier : `EDF`
- Montant maximum : `100.00`
- Type : `R`

Puis lancer `8. Moyens de paiement -> 7. Traiter un fichier de prelevements` avec `sdd-exemple.txt` (1 ligne acceptee de 45.50, 1 rejet `AM05` doublon, 1 rejet `MD01` RUM inconnu).

### Format des fichiers de prelevements SEPA (`SDD`)
Fichier texte `LINE SEQUENTIAL` (lignes de 120 caracteres max, colonne 1 = type) :
- **En-tete `E`** (1ere ligne, unique) : `E` + ICS `X(13)` + date d'echeance `9(8)` (AAAAMMJJ) + nombre de lignes detail `9(5)`
- **Detail `D`** : `D` + RUM `X(20)` + IBAN debiteur `X(27)` + montant `9(9)V99` (11 chiffres sans point) + libelle `X(30)`
- **Fin `F`** (derniere ligne, unique) : `F` + nombre de lignes detail `9(5)` + total des montants `9(11)V99` (13 chiffres sans point)

Phase 1 controle la structure (`E` en premier, `F` en dernier, compteurs, somme des montants = total `F`) et l'absence de doublon `(ICS, date)` dans `sdd.dat` ; en cas d'echec, rien n'est applique (`SDD REFUSE`).
En phase 2, chaque ligne acceptee debite le compte, enregistre une transaction `Prelev` (dest = `0`), met a jour `MANDAT-DERNIER` (et statut `C` si ponctuel) et declenche `CONTROLE-FRAUDE`.

### Codes de rejet SEPA (`rejets-sdd.txt`)
- `MD01` : pas de mandat (RUM inconnu ou ICS different de l'en-tete)
- `MS02` : mandat revoque (`R`) ou ponctuel deja consomme (`C`)
- `AC01` : IBAN incorrect (controle modulo 97 via `CALCUL-IBAN` mode `V`, ou IBAN ne correspondant pas au compte du mandat)
- `AC04` : compte cloture
- `AM04` : provision insuffisante (regle du decouvert autorise, `0` sur `Epargne`)
- `AM05` : doublon (meme RUM deja preleve a la meme date d'echeance)
- `MD07` : montant superieur au maximum du mandat
