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
- Creer compte (Courant, Epargne, Entreprise)
- Lister comptes client (avec IBAN francais)
- Cloturer compte (solde rendu, refuse si un credit est en cours)
- Verifier un IBAN (validation modulo 97 et verification du compte)

**3. Operations**
- Effectuer transaction (Depot, Retrait, Virement)
  - plafond de retrait de 1000.00 par jour et par compte
  - aucune operation possible sur un compte cloture
  - detection de fraude automatique sur Retrait et Virement (gros montant, rafale, vidage)
- Historique des transactions
- Generer releve mensuel (avec IBAN en en-tete)
- Export CSV des transactions (`transactions.csv`)
- Virements permanents : creation et arret (executes en fin de mois)
- Annuler une transaction (Depot, Retrait, Virement : contre-passation)

**4. Credits**
- Accorder credit (mensualite calculee par annuites constantes)
- Rembourser credit (paie une mensualite)
- Simuler credit (tableau d'amortissement dans `amortissement.txt`)

**5. Banque**
- Interets epargne (taux de la banque, une fois par mois)
- Traitement fin de mois, une seule fois par mois :
  1. interets epargne
  2. prelevement automatique des mensualites de credit
  3. virements permanents
  4. frais de tenue de compte (2.00 Courant, 5.00 Entreprise)
- Statistiques (comptes par type, total des soldes, credits en cours, alertes...)
- Classement des clients par avoirs (verbe SORT)
- Consulter le journal d'audit (`journal.log`, filtre par date)
- Alertes fraude (consulter et marquer comme traitees)

### Espace client
Acces restreint au client connecte :
- **1. Mes comptes** : liste uniquement les comptes du client connecte (avec IBAN)
- **2. Historique d'un compte** : uniquement pour un compte appartenant au client
- **3. Effectuer un virement** : uniquement depuis un compte appartenant au client (Depot et Retrait reserves au guichet)
- **4. Changer mon PIN** : verification de l'ancien PIN puis saisie + confirmation du nouveau PIN
- **9. Deconnexion**

## Compiler
Linux :
```
cobc -x -o Bank Bank.cbl calcul-credit.cbl iban.cbl hachage.cbl
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
- `alerte.dat` : alertes de fraude (cree a la premiere alerte)
- `conseiller.dat` : hash du mot de passe conseiller (cree au premier lancement)
- `pin.dat` : hash des PIN clients, essais et statut de blocage (cree au premier PIN)
