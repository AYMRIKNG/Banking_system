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
```
cobc -x -o Bank Bank.cbl
./Bank
```

## Fichiers
- `banque.dat` : infos de la banque (solde, taux, frais)
- `client.dat` : clients
- `compte.dat` : comptes
- `transaction.dat` : transactions
