*****************************************************************
      * PROGRAMME: SYSTEME BANCAIRE
      * AUTEUR: DEVELOPPEUR COBOL
      * DATE: 2025
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYSTEME-BANCAIRE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENT-FILE ASSIGN TO "client.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT COMPTE-FILE ASSIGN TO "compte.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT TRANSACTION-FILE ASSIGN TO "transaction.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT BANQUE-FILE ASSIGN TO "banque.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT RELEVE-FILE ASSIGN TO WS-RELEVE-NOM
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT OPTIONAL CREDIT-FILE ASSIGN TO "credit.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT CSV-FILE ASSIGN TO "transactions.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT OPTIONAL VIREMENT-FILE ASSIGN TO "virement.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT TRI-FILE ASSIGN TO "tri.tmp".
           SELECT OPTIONAL JOURNAL-FILE ASSIGN TO "journal.log"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT OPTIONAL ALERTE-FILE ASSIGN TO "alerte.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT OPTIONAL CONSEILLER-FILE ASSIGN TO "conseiller.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT OPTIONAL PIN-FILE ASSIGN TO "pin.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT CONTROLE-FILE ASSIGN TO "controle.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT LOT-FILE ASSIGN TO WS-LOT-NOM
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT OPTIONAL LOTS-FILE ASSIGN TO "lots.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT REJET-FILE ASSIGN TO "rejets.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT ARCHIVE-FILE ASSIGN TO WS-ARCHIVE-NOM
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT IFU-FILE ASSIGN TO WS-IFU-NOM
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT OPTIONAL DECOUVERT-FILE ASSIGN TO "decouvert.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD BANQUE-FILE.
       01 BANQUE-RECORD.
           05 BANQUE-ID            PIC 9(5).
           05 BANQUE-NAME          PIC A(30).
           05 BANQUE-SOLDE         PIC 9(7)V99.
           05 BANQUE-INTERETS      PIC 9(2)V99.
           05 BANQUE-NB-CLIENTS    PIC 9(5).
           05 BANQUE-FRAIS-COMPTE  PIC 9(3)V99.

       FD CLIENT-FILE.
       01 CLIENT-RECORD.
           05 CLIENT-ID          PIC 9(5).
           05 CLIENT-NAME        PIC A(30).
           05 CLIENT-ADDRESS     PIC A(50).
           05 CLIENT-PHONE       PIC A(15).

       FD COMPTE-FILE.
       01 COMPTE-RECORD.
           05 COMPTE-NUM         PIC 9(10).
           05 COMPTE-TYPE        PIC A(10).
           05 COMPTE-SOLDE       PIC S9(9)V99.
           05 COMPTE-CLIENT-ID   PIC 9(5).

       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANSACTION-ID     PIC 9(10).
           05 TRANSACTION-COMPTE PIC 9(10).
           05 TRANSACTION-DATE   PIC 9(8).
           05 TRANSACTION-TYPE   PIC A(10).
           05 TRANSACTION-MONTANT PIC 9(9)V99.
           05 TRANSACTION-DEST        PIC 9(10).

       FD RELEVE-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RELEVE-LINE.
       01 RELEVE-LINE     PIC X(100).

       FD CREDIT-FILE.
       01 CREDIT-RECORD.
           05 CREDIT-COMPTE       PIC 9(10).
           05 CREDIT-DATE         PIC 9(8).
           05 CREDIT-MONTANT      PIC 9(9)V99.
           05 CREDIT-RESTANT      PIC 9(9)V99.
           05 CREDIT-MENSUALITE   PIC 9(9)V99.
           05 CREDIT-DERNIER-MOIS PIC 9(6).

       FD CSV-FILE.
       01 CSV-LINE                PIC X(100).

       FD VIREMENT-FILE.
       01 VIREMENT-RECORD.
           05 VP-ID                PIC 9(5).
           05 VP-SOURCE            PIC 9(10).
           05 VP-DEST              PIC 9(10).
           05 VP-MONTANT           PIC 9(9)V99.
           05 VP-ACTIF             PIC X.
           05 VP-DERNIER-MOIS      PIC 9(6).

       SD TRI-FILE.
       01 TRI-RECORD.
           05 TRI-TOTAL            PIC S9(9)V99.
           05 TRI-CLIENT-ID        PIC 9(5).
           05 TRI-NAME             PIC A(30).
           05 TRI-NB-COMPTES       PIC 9(3).

       FD JOURNAL-FILE.
       01 JOURNAL-LINE             PIC X(100).

       FD ALERTE-FILE.
       01 ALERTE-RECORD.
           05 ALERTE-ID            PIC 9(5).
           05 ALERTE-DATE          PIC 9(8).
           05 ALERTE-COMPTE        PIC 9(10).
           05 ALERTE-REGLE         PIC X(15).
           05 ALERTE-MONTANT       PIC 9(9)V99.
           05 ALERTE-STATUT        PIC X.

       FD CONSEILLER-FILE.
       01 CONSEILLER-RECORD.
           05 CONS-HASH            PIC 9(10).

       FD PIN-FILE.
       01 PIN-RECORD.
           05 PIN-CLIENT-ID        PIC 9(5).
           05 PIN-HASH             PIC 9(10).
           05 PIN-ESSAIS           PIC 9.
           05 PIN-BLOQUE           PIC X.

       FD CONTROLE-FILE.
       01 CONTROLE-LINE            PIC X(100).

       FD LOT-FILE.
       01 LOT-RECORD               PIC X(60).

       FD LOTS-FILE.
       01 LOTS-RECORD.
           05 LOTS-NUM             PIC 9(5).
           05 LOTS-DATE            PIC 9(8).
           05 LOTS-NB-OK           PIC 9(5).
           05 LOTS-NB-REJET        PIC 9(5).

       FD REJET-FILE.
       01 REJET-LINE               PIC X(130).

       FD ARCHIVE-FILE.
       01 ARCHIVE-RECORD           PIC X(59).

       FD IFU-FILE.
       01 IFU-LINE                 PIC X(100).

       FD DECOUVERT-FILE.
       01 DECOUVERT-RECORD.
           05 DEC-COMPTE           PIC 9(10).
           05 DEC-PLAFOND          PIC 9(7)V99.
         

       WORKING-STORAGE SECTION.
       77 WS-STATUS           PIC XX VALUE "00".
       77 CHOIX               PIC 9.
       77 SOUS-CHOIX          PIC 9.
       77 WS-CONFIRMATION     PIC X.
       77 QUITTER             PIC X VALUE "N".
       77 WS-TODAY            PIC 9(8).
       77 WS-COMPTE-CHOIX     PIC 9(10).
        77 WS-RELEVE-NOM       PIC X(50).
          77 WS-MOIS             PIC 9(6).  *> Format AAAAMM
          77 WS-TRANSACTION-MOIS PIC 9(6).
          77 WS-TMP-STR          PIC X(100).

       01 WS-NEW-CLIENT.
           05 NEW-ID           PIC 9(5).
           05 NEW-NAME         PIC A(30).
           05 NEW-ADDRESS      PIC A(50).
           05 NEW-PHONE        PIC A(15).

       01 WS-NEW-COMPTE.
           05 NEW-COMPTE-NUM   PIC 9(10).
           05 NEW-COMPTE-TYPE  PIC A(10).
           05 NEW-COMPTE-SOLDE PIC 9(9)V99.
           05 NEW-COMPTE-CLIENT-ID PIC 9(5).

       01 WS-TRANSACTION.
           05 WS-TRANS-ID      PIC 9(10).
           05 WS-TRANS-COMPTE  PIC 9(10).
           05 WS-TRANS-DATE    PIC 9(8).
           05 WS-TRANS-TYPE    PIC A(10).
           05 WS-TRANS-MONTANT PIC 9(9)V99.
           05 WS-TRANS-DEST    PIC 9(10).

       01 WS-INPUT-ID         PIC 9(10).
       01 WS-INPUT-COMPTE     PIC 9(10).
       01 WS-INPUT-MONTANT    PIC 9(9)V99.
       01 WS-FOUND            PIC X VALUE "N".
       01 WS-COUNTER          PIC 9(10) VALUE 1.

       01 WS-CREDIT.
           05 WS-DUREE             PIC 9(3).
           05 WS-TOTAL-CREDIT      PIC 9(9)V99.
           05 WS-MENSUALITE        PIC 9(9)V99.

       01 WS-CALCUL-CREDIT.
           05 WS-CC-MODE           PIC X.
           05 WS-CC-CAPITAL        PIC 9(9)V99.
           05 WS-CC-TAUX-ANNUEL    PIC 9(2)V99.
           05 WS-CC-DUREE          PIC 9(3).
           05 WS-CC-MENSUALITE     PIC 9(9)V99.
           05 WS-CC-COUT-TOTAL     PIC 9(9)V99.
           05 WS-CC-TOTAL          PIC 9(9)V99.
           05 WS-CC-RETOUR         PIC 9.

       01 WS-SCORING.
           05 WS-SC-REVENUS        PIC 9(9)V99.
           05 WS-SC-CHARGES        PIC 9(9)V99.
           05 WS-SC-MENSUALITE     PIC 9(9)V99.
           05 WS-SC-ANCIENNETE     PIC 9(3).
           05 WS-SC-NB-ALERTES     PIC 9(3).
           05 WS-SC-EPARGNE        PIC 9(9)V99.
           05 WS-SC-MONTANT        PIC 9(9)V99.
           05 WS-SC-NB-DEBITEURS   PIC 9(3).
           05 WS-SC-ENDETTEMENT    PIC 9(3)V99.
           05 WS-SC-SCORE          PIC 9(3).
           05 WS-SC-DECISION       PIC X.
           05 WS-SC-MOTIF          PIC X(40).

       01 WS-SCORING-WORK.
           05 WS-SCW-STATUS        PIC XX.
           05 WS-SCW-CLIENT-ID     PIC 9(5).
           05 WS-SCW-NB-COMPTES    PIC 9(2).
           05 WS-SCW-TAB-COMPTES OCCURS 50 TIMES
               INDEXED BY WS-SCW-IDX.
               10 WS-SCW-CPT       PIC 9(10).
           05 WS-SCW-TOTAL-REV     PIC 9(11)V99.
           05 WS-SCW-JOUR-AUJ      PIC 9(8).
           05 WS-SCW-JOUR-LIM      PIC 9(8).
           05 WS-SCW-JOUR-TR       PIC 9(8).
           05 WS-SCW-JOUR-MIN      PIC 9(8).
           05 WS-SCW-DIFF-JOURS    PIC 9(8).
           05 WS-SCW-ANC-CALC      PIC 9(5).
           05 WS-SCW-TR-DEST       PIC 9(10).
           05 WS-SCW-SRC-CLIENT    PIC X.
           05 WS-SCW-DEST-CLIENT   PIC X.
           05 WS-SCW-TAUX-AFF      PIC Z(2)9.99.

       01 WS-CALCUL-IBAN.
           05 WS-IBAN-MODE         PIC X.
           05 WS-IBAN-COMPTE       PIC 9(10).
           05 WS-IBAN-TEXTE        PIC X(27).
           05 WS-IBAN-AFFICHE      PIC X(34).
           05 WS-IBAN-RETOUR       PIC 9.

       01 WS-VERIF-IBAN.
           05 WS-IBAN-SAISIE       PIC X(40).

       01 WS-INTERETS.
           05 WS-INTERET           PIC 9(9)V99.
           05 WS-TOTAL-INTERETS    PIC 9(9)V99.

       01 WS-FIN-MOIS.
           05 WS-FIN-CREDITS       PIC X.
           05 WS-TOTAL-PRELEVE     PIC 9(9)V99.
           05 WS-NB-PRELEVES       PIC 9(5).
           05 WS-NB-IMPAYES        PIC 9(5).

       01 WS-VIREMENTS.
           05 WS-FIN-VIREMENTS     PIC X.
           05 WS-NB-VIREMENTS-OK   PIC 9(5).
           05 WS-NB-VIREMENTS-ECHEC PIC 9(5).
           05 WS-MAX-VP-ID         PIC 9(5).
           05 WS-INPUT-VP-ID       PIC 9(5).
           05 WS-VP-SOURCE         PIC 9(10).
           05 WS-VP-DEST           PIC 9(10).
           05 WS-VP-MONTANT        PIC 9(9)V99.
           05 WS-VP-NB-ACTIFS      PIC 9(5).

       01 WS-FRAIS-TENUE.
           05 WS-FRAIS-COURANT     PIC 9(3)V99 VALUE 2.00.
           05 WS-FRAIS-ENTREPRISE  PIC 9(3)V99 VALUE 5.00.
           05 WS-FRAIS             PIC 9(3)V99.
           05 WS-TOTAL-FRAIS       PIC 9(9)V99.
           05 WS-NB-FRAIS          PIC 9(5).
           05 WS-NB-FRAIS-ECHEC    PIC 9(5).

       01 WS-STATS.
           05 WS-NB-COURANT        PIC 9(5).
           05 WS-NB-EPARGNE        PIC 9(5).
           05 WS-NB-ENTREPRISE     PIC 9(5).
           05 WS-NB-CLOTURE        PIC 9(5).
           05 WS-TOTAL-DEPOTS      PIC S9(9)V99.
           05 WS-MAX-SOLDE         PIC S9(9)V99.
           05 WS-MAX-COMPTE        PIC 9(10).
           05 WS-NB-CREDITS        PIC 9(5).
           05 WS-TOTAL-RESTANT     PIC 9(9)V99.
           05 WS-NB-TRANS-MOIS     PIC 9(5).
           05 WS-NB-ALERTES        PIC 9(5).
       77 WS-MONTANT-AFF          PIC Z(8)9.99.
       77 WS-SOLDE-AFF            PIC -(9)9.99.
       77 WS-NB-EXPORT            PIC 9(5).

       01 WS-PLAFOND.
           05 WS-PLAFOND-RETRAIT   PIC 9(9)V99 VALUE 1000.00.
           05 WS-TOTAL-JOUR        PIC 9(9)V99.

       01 WS-MODIF-CLIENT.
           05 WS-MODIF-ADRESSE     PIC X(50).
           05 WS-MODIF-PHONE       PIC X(15).

       01 WS-RECHERCHE-CLIENT.
           05 WS-RECHERCHE         PIC X(30).
           05 WS-NOM-MAJ           PIC X(30).
           05 WS-LONGUEUR          PIC 99.
           05 WS-OCCURENCES        PIC 9(3).
           05 WS-NB-TROUVES        PIC 9(5).

       01 WS-GENERATION-COMPTE.
           05 WS-MAX-COMPTE-NUM    PIC 9(10) VALUE ZEROS.
           05 WS-NOUVEAU-NUM       PIC 9(10).
           05 WS-SOLDE-INIT        PIC 9(9)V99.
       
       01 WS-GENERATION-CLIENT.
           05 WS-MAX-CLIENT-NUM    PIC 9(10) VALUE ZEROS.
           05 WS-NOUVELLE-ID       PIC 9(10).

       01 WS-GENERATION-TRANSACTION.
           05 WS-MAX-TRANS-NUM     PIC 9(10) VALUE ZEROS.

       01 WS-ANNULATION.
           05 WS-ANN-ID            PIC 9(10).
           05 WS-ANN-COMPTE        PIC 9(10).
           05 WS-ANN-DATE          PIC 9(8).
           05 WS-ANN-TYPE          PIC A(10).
           05 WS-ANN-MONTANT       PIC 9(9)V99.
           05 WS-ANN-DEST          PIC 9(10).

       01 WS-CLASSEMENT.
           05 WS-FIN-CLIENTS       PIC X.
           05 WS-FIN-TRI           PIC X.
           05 WS-CL-TOTAL          PIC S9(9)V99.
           05 WS-CL-NB-COMPTES     PIC 9(3).
           05 WS-CL-RANG           PIC 9(5).
           05 WS-CL-RANG-AFF       PIC Z(4)9.
           05 WS-CL-TOTAL-GENERAL  PIC S9(9)V99.

       01 WS-JOURNAL.
           05 WS-JOURNAL-ACTION    PIC X(15).
           05 WS-JOURNAL-DETAIL    PIC X(60).
           05 WS-JOURNAL-STATUS    PIC XX.
           05 WS-JOURNAL-NOW       PIC X(21).
           05 WS-JOURNAL-DATE      PIC 9(8).
           05 WS-JOURNAL-DATE-FMT  PIC X(10).
           05 WS-JOURNAL-NB        PIC 9(5).

       01 WS-FRAUDE.
           05 WS-SEUIL-MONTANT     PIC 9(9)V99 VALUE 5000.00.
           05 WS-SEUIL-RAFALE      PIC 9(3) VALUE 3.
           05 WS-SEUIL-VIDAGE      PIC 9(9)V99 VALUE 1000.00.
           05 WS-SOLDE-AVANT       PIC S9(9)V99.
           05 WS-SOLDE-APRES       PIC S9(10)V99.
           05 WS-NB-OPS-JOUR       PIC 9(5).
           05 WS-FRAUDE-STATUS     PIC XX.
           05 WS-FRAUDE-IDX        PIC 9.
           05 WS-MAX-ALERTE-ID     PIC 9(5).
           05 WS-ALERTE-REGLE-CUR  PIC X(15).
           05 WS-NB-ALERTES-NOUV   PIC 9(5).
           05 WS-INPUT-ALERTE-ID   PIC 9(5).
           05 WS-NB-TRAITEES       PIC 9(5).

       01 WS-HACHAGE.
           05 WS-H-TEXTE           PIC X(20).
           05 WS-H-SEL             PIC 9(5).
           05 WS-H-RESULTAT        PIC 9(10).

       01 WS-AUTH.
           05 WS-MODE-SESSION      PIC X VALUE SPACE.
           05 WS-CLIENT-CONNECTE   PIC 9(5) VALUE ZEROS.
           05 WS-DECONNEXION       PIC X VALUE "N".
           05 WS-PROPRIETAIRE-OK   PIC X VALUE "N".
           05 WS-AUTH-STATUS       PIC XX VALUE "00".
           05 WS-AUTH-OK           PIC X VALUE "N".
           05 WS-CHOIX-CONN        PIC 9 VALUE 0.
           05 WS-ESSAI-AUTH        PIC 9 VALUE 0.
           05 WS-AUTH-CLIENT-ID    PIC 9(5) VALUE ZEROS.
           05 WS-CONS-HASH-LU      PIC 9(10) VALUE ZEROS.
           05 WS-PIN-HASH-LU       PIC 9(10) VALUE ZEROS.
           05 WS-PIN-BLOQUE-LU     PIC X VALUE "N".
           05 WS-VENANT-BLOQUE     PIC X VALUE "N".
           05 WS-MDP-SAISIE        PIC X(20).
           05 WS-MDP-CONFIRM       PIC X(20).
           05 WS-PIN-ANCIEN        PIC X(20).
           05 WS-PIN-SAISIE        PIC X(20).
           05 WS-PIN-CONFIRM       PIC X(20).

       01 WS-CONTROLE.
           05 WS-CI-NB-COMPTES     PIC 9(4) VALUE 0.
           05 WS-CI-NB-TRANS       PIC 9(4) VALUE 0.
           05 WS-TAB-COMPTES OCCURS 500 TIMES
               INDEXED BY WS-CI-IDX-C.
               10 WS-TC-NUM        PIC 9(10).
               10 WS-TC-CLIENT-ID  PIC 9(5).
               10 WS-TC-SOLDE-FIC  PIC S9(9)V99.
               10 WS-TC-SOLDE-REC  PIC S9(9)V99.
               10 WS-TC-REPORT     PIC S9(10)V99.
               10 WS-TC-INTERET    PIC 9(9)V99.
           05 WS-TAB-TRANS OCCURS 3000 TIMES
               INDEXED BY WS-CI-IDX-T.
               10 WS-TT-ID         PIC 9(10).
               10 WS-TT-COMPTE     PIC 9(10).
               10 WS-TT-DATE       PIC 9(8).
               10 WS-TT-DATE-R REDEFINES WS-TT-DATE.
                   15 WS-TT-ANNEE  PIC 9(4).
                   15 WS-TT-MMJJ   PIC 9(4).
               10 WS-TT-TYPE       PIC A(10).
               10 WS-TT-MONTANT    PIC 9(9)V99.
               10 WS-TT-DEST       PIC 9(10).
               10 WS-TT-RAW        PIC X(59).
           05 WS-CI-I              PIC 9(4).
           05 WS-CI-TROP-PLEIN     PIC X.
           05 WS-CI-ERREUR         PIC X.
           05 WS-CI-NB-OK          PIC 9(4).
           05 WS-CI-NB-ECART       PIC 9(4).
           05 WS-CI-NB-ANOMALIES   PIC 9(4).
           05 WS-CI-ECART          PIC S9(9)V99.
           05 WS-CI-ECART-ABS      PIC 9(9)V99.
           05 WS-CI-TOTAL-ECARTS   PIC 9(9)V99.
           05 WS-CI-STATUT         PIC X(5).
           05 WS-CI-CIBLE-COMPTE   PIC 9(10).
           05 WS-CI-CIBLE-DEST     PIC 9(10).
           05 WS-CI-CIBLE-TYPE     PIC A(10).
           05 WS-CI-CIBLE-MONTANT  PIC 9(9)V99.
           05 WS-CI-AFF-FIC        PIC -(9)9.99.
           05 WS-CI-AFF-REC        PIC -(9)9.99.
           05 WS-CI-AFF-ECART      PIC -(9)9.99.
           05 WS-CI-AFF-TOTAL      PIC Z(8)9.99.
           05 WS-CI-DATE-FMT       PIC X(10).
           05 WS-CI-LIGNE          PIC X(100).

       01 WS-LOT.
           05 WS-LOT-NOM           PIC X(200).
           05 WS-LOT-LIGNE         PIC X(60).
           05 WS-LOT-E REDEFINES WS-LOT-LIGNE.
               10 WS-LE-TYPE       PIC X.
               10 WS-LE-NUM        PIC 9(5).
               10 WS-LE-DATE       PIC 9(8).
               10 WS-LE-NB         PIC 9(5).
               10 FILLER           PIC X(41).
           05 WS-LOT-D REDEFINES WS-LOT-LIGNE.
               10 WS-LD-TYPE       PIC X.
               10 WS-LD-COMPTE     PIC 9(10).
               10 WS-LD-OP         PIC A(10).
               10 WS-LD-MONTANT    PIC 9(9)V99.
               10 WS-LD-DEST       PIC 9(10).
               10 FILLER           PIC X(18).
           05 WS-LOT-F REDEFINES WS-LOT-LIGNE.
               10 WS-LF-TYPE       PIC X.
               10 WS-LF-NB         PIC 9(5).
               10 WS-LF-TOTAL      PIC 9(11)V99.
               10 FILLER           PIC X(41).
           05 WS-LOT-EOF           PIC X.
           05 WS-LOT-NB-LIGNES     PIC 9(5).
           05 WS-LOT-NB-E          PIC 9(5).
           05 WS-LOT-NB-D          PIC 9(5).
           05 WS-LOT-NB-F          PIC 9(5).
           05 WS-LOT-DERNIER-TYPE  PIC X.
           05 WS-LOT-NUM           PIC 9(5).
           05 WS-LOT-DATE          PIC 9(8).
           05 WS-LOT-NB-ANNONCE    PIC 9(5).
           05 WS-LOT-F-NB          PIC 9(5).
           05 WS-LOT-F-TOTAL       PIC 9(11)V99.
           05 WS-LOT-SOMME-D       PIC 9(11)V99.
           05 WS-LOT-MOTIF         PIC X(50).
           05 WS-LOT-NUM-LIGNE     PIC 9(5).
           05 WS-LOT-NB-OK         PIC 9(5).
           05 WS-LOT-NB-REJET      PIC 9(5).
           05 WS-LOT-VERIF-NB      PIC 9(5).
           05 WS-LOT-MNT-OK        PIC 9(11)V99.
           05 WS-LOT-MNT-REJET     PIC 9(11)V99.
           05 WS-LOT-MNT-AFF       PIC Z(10)9.99.
           05 WS-LOT-CODE-REJET    PIC X(3).
           05 WS-LOT-LIB-REJET     PIC X(40).
           05 WS-LOT-SOLDE-SRC     PIC S9(9)V99.
           05 WS-LOT-TYPE-SRC      PIC A(10).
           05 WS-LOT-TYPE-DEST     PIC A(10).

       01 WS-DECOUVERT.
           05 WS-DEC-COMPTE        PIC 9(10).
           05 WS-DEC-PLAFOND       PIC 9(7)V99.
           05 WS-DEC-NEW-PLAFOND   PIC 9(7)V99.
           05 WS-DEC-STATUS        PIC XX.
           05 WS-DEC-FOUND         PIC X.
           05 WS-DEC-SOLDE-PROJ    PIC S9(10)V99.
           05 WS-DEC-LIMITE        PIC S9(10)V99.
           05 WS-DEC-AFF           PIC Z(6)9.99.

       01 WS-ARCHIVAGE.
           05 WS-ARCHIVE-NOM       PIC X(30).
           05 WS-IFU-NOM           PIC X(30).
           05 WS-ARCH-ANNEE        PIC 9(4).
           05 WS-ARCH-ANNEE-COUR   PIC 9(4).
           05 WS-ARCH-DATE-REP     PIC 9(8).
           05 WS-ARCH-NB-ARCH      PIC 9(4).
           05 WS-ARCH-NB-GARDE     PIC 9(4).
           05 WS-ARCH-NB-REPORTS   PIC 9(4).
           05 WS-ARCH-REPORT       PIC S9(10)V99.
           05 WS-ARCH-ERREUR       PIC X.
           05 WS-ARCH-FIN-CLI      PIC X.
           05 WS-ARCH-LIGNE        PIC X(100).
           05 WS-IFU-BRUT          PIC 9(9)V99.
           05 WS-IFU-PFU           PIC 9(9)V99.
           05 WS-IFU-NET           PIC 9(9)V99.
           05 WS-IFU-TOT-BRUT      PIC 9(9)V99.
           05 WS-IFU-TOT-PFU       PIC 9(9)V99.
           05 WS-IFU-TOT-NET       PIC 9(9)V99.
           05 WS-IFU-AFF-BRUT      PIC Z(8)9.99.
           05 WS-IFU-AFF-PFU       PIC Z(8)9.99.
           05 WS-IFU-AFF-NET       PIC Z(8)9.99.

       PROCEDURE DIVISION.

       MAIN-PROGRAM.
           PERFORM INITIALISER-BANQUE
           PERFORM UNTIL QUITTER = "O"
               PERFORM ECRAN-CONNEXION
           END-PERFORM
           MOVE "ARRET" TO WS-JOURNAL-ACTION
           MOVE "Fin de session" TO WS-JOURNAL-DETAIL
           PERFORM ECRIRE-JOURNAL
           DISPLAY "Merci d'avoir utilise le systeme bancaire."
           STOP RUN.

       INITIALISER-BANQUE.
      *    Date du jour au format AAAAMMJJ
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TODAY.

           OPEN I-O BANQUE-FILE
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur ouverture fichier banque: " WS-STATUS
               MOVE "O" TO QUITTER
               EXIT PARAGRAPH
           END-IF

           READ BANQUE-FILE
            
            DISPLAY "Name :" BANQUE-NAME ",Solde :" BANQUE-SOLDE

           CLOSE BANQUE-FILE.

      *    Reprendre le compteur de transaction apres le dernier ID
           MOVE ZEROS TO WS-MAX-TRANS-NUM.
           OPEN INPUT TRANSACTION-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10"
                   READ TRANSACTION-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF TRANSACTION-ID > WS-MAX-TRANS-NUM
                               MOVE TRANSACTION-ID TO WS-MAX-TRANS-NUM
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF.
           ADD 1 TO WS-MAX-TRANS-NUM GIVING WS-COUNTER.

           MOVE "DEMARRAGE" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Banque " DELIMITED BY SIZE
               FUNCTION TRIM(BANQUE-NAME) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.

       AFFICHER-MENU.
           DISPLAY "==== MENU BANCAIRE ====".
           DISPLAY "1. Clients".
           DISPLAY "2. Comptes".
           DISPLAY "3. Operations".
           DISPLAY "4. Credits".
           DISPLAY "5. Banque".
           DISPLAY "6. Deconnexion".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT CHOIX.

           EVALUATE CHOIX
               WHEN 1
                   PERFORM MENU-CLIENTS
               WHEN 2
                   PERFORM MENU-COMPTES
               WHEN 3
                   PERFORM MENU-OPERATIONS
               WHEN 4
                   PERFORM MENU-CREDITS
               WHEN 5
                   PERFORM MENU-BANQUE
               WHEN 6
                   MOVE "O" TO WS-DECONNEXION
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.

       MENU-CLIENTS.
           DISPLAY "---- CLIENTS ----".
           DISPLAY "1. Ajouter client".
           DISPLAY "2. Modifier client".
           DISPLAY "3. Rechercher client".
           DISPLAY "4. Definir / debloquer PIN client".
           DISPLAY "9. Retour".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT SOUS-CHOIX.

           EVALUATE SOUS-CHOIX
               WHEN 1
                   PERFORM AJOUTER-CLIENT
               WHEN 2
                   PERFORM MODIFIER-CLIENT
               WHEN 3
                   PERFORM RECHERCHER-CLIENT
               WHEN 4
                   PERFORM DEFINIR-PIN
               WHEN 9
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.

       MENU-COMPTES.
           DISPLAY "---- COMPTES ----".
           DISPLAY "1. Creer compte".
           DISPLAY "2. Lister comptes client".
           DISPLAY "3. Cloturer compte".
           DISPLAY "4. Verifier un IBAN".
           DISPLAY "5. Definir decouvert autorise".
           DISPLAY "9. Retour".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT SOUS-CHOIX.

           EVALUATE SOUS-CHOIX
               WHEN 1
                   PERFORM CREER-COMPTE
               WHEN 2
                   PERFORM LISTER-COMPTES-CLIENT
               WHEN 3
                   PERFORM CLOTURER-COMPTE
               WHEN 4
                   PERFORM VERIFIER-IBAN
               WHEN 5
                   PERFORM DEFINIR-DECOUVERT
               WHEN 9
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.

       MENU-OPERATIONS.
           DISPLAY "---- OPERATIONS ----".
           DISPLAY "1. Effectuer transaction".
           DISPLAY "2. Historique des transactions".
           DISPLAY "3. Generer releve".
           DISPLAY "4. Export CSV".
           DISPLAY "5. Creer virement permanent".
           DISPLAY "6. Arreter virement permanent".
           DISPLAY "7. Annuler transaction".
           DISPLAY "8. Traitement par lot".
           DISPLAY "9. Retour".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT SOUS-CHOIX.

           EVALUATE SOUS-CHOIX
               WHEN 1
                   PERFORM EFFECTUER-TRANSACTION
               WHEN 2
                   PERFORM HISTORIQUE-TRANSACTIONS
               WHEN 3
                   PERFORM GENERER-RELEVE
               WHEN 4
                   PERFORM EXPORT-CSV
               WHEN 5
                   PERFORM CREER-VIREMENT
               WHEN 6
                   PERFORM ARRETER-VIREMENT
               WHEN 7
                   PERFORM ANNULER-TRANSACTION
               WHEN 8
                   PERFORM TRAITEMENT-LOT
               WHEN 9
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.

       MENU-CREDITS.
           DISPLAY "---- CREDITS ----".
           DISPLAY "1. Accorder credit".
           DISPLAY "2. Rembourser credit".
           DISPLAY "3. Simuler credit".
           DISPLAY "9. Retour".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT SOUS-CHOIX.

           EVALUATE SOUS-CHOIX
               WHEN 1
                   PERFORM ACCORDER-CREDIT
               WHEN 2
                   PERFORM REMBOURSER-CREDIT
               WHEN 3
                   PERFORM SIMULER-CREDIT
               WHEN 9
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.

       MENU-BANQUE.
           DISPLAY "---- BANQUE ----".
           DISPLAY "1. Interets epargne".
           DISPLAY "2. Traitement fin de mois".
           DISPLAY "3. Statistiques".
           DISPLAY "4. Classement clients".
           DISPLAY "5. Consulter journal".
           DISPLAY "6. Alertes fraude".
           DISPLAY "7. Controle integrite des soldes".
           DISPLAY "8. Cloture annuelle / archivage".
           DISPLAY "9. Retour".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT SOUS-CHOIX.

           EVALUATE SOUS-CHOIX
               WHEN 1
                   PERFORM INTERET-EPARGNE
               WHEN 2
                   PERFORM FIN-DE-MOIS
               WHEN 3
                   PERFORM STATISTIQUES
               WHEN 4
                   PERFORM CLASSEMENT-CLIENTS
               WHEN 5
                   PERFORM CONSULTER-JOURNAL
               WHEN 6
                   PERFORM ALERTES-FRAUDE
               WHEN 7
                   PERFORM CONTROLE-INTEGRITE
               WHEN 8
                   PERFORM CLOTURE-ANNUELLE
               WHEN 9
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.

       AJOUTER-CLIENT.
           COPY "ajouter-client.cpy".

       MODIFIER-CLIENT.
           COPY "modifier-client.cpy".

       RECHERCHER-CLIENT.
           COPY "rechercher-client.cpy".

       CREER-COMPTE.
           COPY "creer-compte.cpy".

       CLOTURER-COMPTE.
           COPY "cloturer-compte.cpy".

       VERIFIER-IBAN.
           COPY "verifier-iban.cpy".

        EFFECTUER-TRANSACTION.
           COPY "effectuer-transaction.cpy".

       LISTER-COMPTES-CLIENT.
           COPY "comptes-client.cpy".

       HISTORIQUE-TRANSACTIONS.
           COPY "Historique-Transactions.cpy".

       GENERER-RELEVE.
           COPY "generer-relever.cpy".
       
       ACCORDER-CREDIT.
           COPY "accorder-credit.cpy".

       CALCULER-SCORE.
           COPY "calcul-score.cpy".

       INTERET-EPARGNE.
           COPY "interet-epargne.cpy".

       REMBOURSER-CREDIT.
           COPY "rembourser-credit.cpy".

       SIMULER-CREDIT.
           COPY "simuler-credit.cpy".

       FIN-DE-MOIS.
           COPY "fin-de-mois.cpy".

       PRELEVER-MENSUALITE.
           COPY "prelever-mensualite.cpy".

       STATISTIQUES.
           COPY "statistiques.cpy".

       EXPORT-CSV.
           COPY "export-csv.cpy".

       CREER-VIREMENT.
           COPY "creer-virement.cpy".

       ARRETER-VIREMENT.
           COPY "arreter-virement.cpy".

       EXECUTER-VIREMENT.
           COPY "executer-virement.cpy".

       FRAIS-TENUE-COMPTE.
           COPY "frais-tenue.cpy".

       ANNULER-TRANSACTION.
           COPY "annuler-transaction.cpy".

       CLASSEMENT-CLIENTS.
           COPY "classement-clients.cpy".

       CLASSEMENT-ENTREE.
           COPY "classement-entree.cpy".

       CLASSEMENT-SORTIE.
           COPY "classement-sortie.cpy".

       ECRIRE-JOURNAL.
           COPY "ecrire-journal.cpy".

       CONSULTER-JOURNAL.
           COPY "consulter-journal.cpy".

       CONTROLE-FRAUDE.
           COPY "controle-fraude.cpy".

       ALERTES-FRAUDE.
           COPY "alertes-fraude.cpy".

       ECRAN-CONNEXION.
           COPY "connexion.cpy".

       MENU-ESPACE-CLIENT.
           COPY "espace-client.cpy".

       DEFINIR-PIN.
           COPY "definir-pin.cpy".

       VERIFIER-PROPRIETAIRE.
           COPY "verifier-proprietaire.cpy".

       CONTROLE-INTEGRITE.
           COPY "controle-integrite.cpy".

       CHARGER-COMPTES-TRANS.
           COPY "charger-comptes-trans.cpy".

       REJOUER-TRANSACTION.
           COPY "rejouer-transaction.cpy".

       CLOTURE-ANNUELLE.
           COPY "cloture-annuelle.cpy".

       TRAITEMENT-LOT.
           COPY "traitement-lot.cpy".

       DEFINIR-DECOUVERT.
           COPY "definir-decouvert.cpy".

       LIRE-DECOUVERT.
           COPY "lire-decouvert.cpy".


       
