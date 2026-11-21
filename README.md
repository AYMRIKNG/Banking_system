# Banking_system
Banking systeme in COBOL

## Fonctionnalites
Le menu principal est decoupe en sous-menus.

**1. Clients**
- Ajouter client
- Modifier client (adresse, telephone)
- Rechercher client par nom (ou partie du nom)

**2. Comptes**
- Creer compte (Courant, Epargne, Entreprise)
- Lister comptes client
- Cloturer compte (solde rendu, refuse si un credit est en cours)

**3. Operations**
- Effectuer transaction (Depot, Retrait, Virement)
  - plafond de retrait de 1000.00 par jour et par compte
  - aucune operation possible sur un compte cloture
- Historique des transactions
- Generer releve mensuel
- Export CSV des transactions (`transactions.csv`)
- Virements permanents : creation et arret (executes en fin de mois)
- Annuler une transaction (Depot, Retrait, Virement : contre-passation)

**4. Credits**
- Accorder credit (mensualite calculee avec le taux de la banque)
- Rembourser credit (paie une mensualite)

**5. Banque**
- Interets epargne (taux de la banque, une fois par mois)
- Traitement fin de mois, une seule fois par mois :
  1. interets epargne
  2. prelevement automatique des mensualites de credit
  3. virements permanents
  4. frais de tenue de compte (2.00 Courant, 5.00 Entreprise)
- Statistiques (comptes par type, total des soldes, credits en cours...)
- Classement des clients par avoirs (verbe SORT)
- Consulter le journal d'audit (`journal.log`, filtre par date)

## Compiler
Linux :
```
cobc -x -o Bank Bank.cbl
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
- `compte.dat` : comptes
- `transaction.dat` : transactions
- `credit.dat` : credits en cours (cree au premier credit)
- `virement.dat` : virements permanents (cree au premier virement)
- `journal.log` : journal d'audit de toutes les operations
