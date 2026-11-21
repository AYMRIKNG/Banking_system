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

**4. Credits**
- Accorder credit (mensualite calculee avec le taux de la banque)
- Rembourser credit (paie une mensualite)

**5. Banque**
- Interets epargne (taux de la banque, une fois par mois)
- Traitement fin de mois : interets + prelevement automatique des mensualites
- Statistiques (comptes par type, total des soldes, credits en cours...)

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
