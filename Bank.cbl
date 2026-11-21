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
           SELECT RELEVE-FILE ASSIGN TO "releve.txt"
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
           05 COMPTE-SOLDE       PIC 9(9)V99.
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
         

       WORKING-STORAGE SECTION.
       77 WS-STATUS           PIC XX VALUE "00".
       77 CHOIX               PIC 9.
       77 QUITTER             PIC X VALUE "N".
       77 WS-TODAY            PIC 9(8) VALUE 20250616.
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

       01 WS-INTERETS.
           05 WS-INTERET           PIC 9(9)V99.
           05 WS-TOTAL-INTERETS    PIC 9(9)V99.

       01 WS-GENERATION-COMPTE.
           05 WS-MAX-COMPTE-NUM    PIC 9(10) VALUE ZEROS.
           05 WS-NOUVEAU-NUM       PIC 9(10).
       
       01 WS-GENERATION-CLIENT.
           05 WS-MAX-CLIENT-NUM    PIC 9(10) VALUE ZEROS.
           05 WS-NOUVELLE-ID       PIC 9(10).

       01 WS-GENERATION-TRANSACTION.
           05 WS-MAX-TRANS-NUM     PIC 9(10) VALUE ZEROS.

       

       PROCEDURE DIVISION.

       MAIN-PROGRAM.
           PERFORM INITIALISER-BANQUE
           PERFORM UNTIL QUITTER = "O"
               PERFORM AFFICHER-MENU
           END-PERFORM
           DISPLAY "Merci d'avoir utilise le systeme bancaire."
           STOP RUN.

       INITIALISER-BANQUE.
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

       AFFICHER-MENU.
           DISPLAY "==== MENU BANCAIRE ====".
           DISPLAY "1. Ajouter client".
           DISPLAY "2. Creer compte".
           DISPLAY "3. Effectuer transaction".
           DISPLAY "4. Lister comptes client".
           DISPLAY "5. Historique des transactions".
           DISPLAY "6. Generer releve"
           DISPLAY "7. Accorder credit".
           DISPLAY "8. Interets epargne".
           DISPLAY "9. Quitter".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT CHOIX.

           EVALUATE CHOIX
               WHEN 1
                   PERFORM AJOUTER-CLIENT
               WHEN 2
                   PERFORM CREER-COMPTE
               WHEN 3
                   PERFORM EFFECTUER-TRANSACTION
               WHEN 4
                   PERFORM LISTER-COMPTES-CLIENT
               WHEN 5
                   PERFORM HISTORIQUE-TRANSACTIONS
               WHEN 6
                   PERFORM GENERER-RELEVE
               WHEN 7
                   PERFORM ACCORDER-CREDIT
               WHEN 8
                   PERFORM INTERET-EPARGNE
               WHEN 9
                   MOVE "O" TO QUITTER
               
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.

       AJOUTER-CLIENT.
           COPY "ajouter-client.cpy".

       CREER-COMPTE.
           COPY "creer-compte.cpy".

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

       INTERET-EPARGNE.
           COPY "interet-epargne.cpy".


       
