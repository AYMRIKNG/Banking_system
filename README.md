# Banking_system
Banking systeme in COBOL

## Fonctionnalites
1. Ajouter client
2. Creer compte (Courant, Epargne, Entreprise)
3. Effectuer transaction (Depot, Retrait, Virement)
4. Lister comptes client
5. Historique des transactions
6. Generer releve mensuel
7. Accorder credit (mensualite calculee avec le taux de la banque)
8. Interets epargne (taux de la banque applique aux comptes Epargne)

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
