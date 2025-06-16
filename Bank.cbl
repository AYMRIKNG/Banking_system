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

       DATA DIVISION.
       FILE SECTION.

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

       WORKING-STORAGE SECTION.
       77 WS-STATUS           PIC XX VALUE "00".
       77 CHOIX               PIC 9.
       77 QUITTER             PIC X VALUE "N".

       77 WS-TODAY            PIC 9(8) VALUE 20250616.

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

       01 WS-INPUT-ID         PIC 9(5).
       01 WS-INPUT-COMPTE     PIC 9(10).
       01 WS-INPUT-MONTANT    PIC 9(9)V99.
       01 WS-FOUND            PIC X VALUE "N".

       01 WS-COUNTER          PIC 9(10) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.

           PERFORM UNTIL QUITTER = "O"
               PERFORM AFFICHER-MENU
           END-PERFORM

           DISPLAY "Merci d'avoir utilisé le système bancaire."
           STOP RUN.

       AFFICHER-MENU.
           DISPLAY "==== MENU BANCAIRE ===="
           DISPLAY "1. Ajouter client"
           DISPLAY "2. Créer compte"
           DISPLAY "3. Effectuer transaction"
           DISPLAY "4. Lister comptes client"
           DISPLAY "5. Quitter"
           DISPLAY "Choisissez une option : " WITH NO ADVANCING
           ACCEPT CHOIX

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
                   MOVE "O" TO QUITTER
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.

       AJOUTER-CLIENT.
           DISPLAY "Ajout d'un nouveau client."
           DISPLAY "ID client (5 chiffres) : " WITH NO ADVANCING
           ACCEPT NEW-ID
           DISPLAY "Nom complet : " WITH NO ADVANCING
           ACCEPT NEW-NAME
           DISPLAY "Adresse : " WITH NO ADVANCING
           ACCEPT NEW-ADDRESS
           DISPLAY "Téléphone : " WITH NO ADVANCING
           ACCEPT NEW-PHONE

           OPEN EXTEND CLIENT-FILE
           MOVE NEW-ID TO CLIENT-ID
           MOVE NEW-NAME TO CLIENT-NAME
           MOVE NEW-ADDRESS TO CLIENT-ADDRESS
           MOVE NEW-PHONE TO CLIENT-PHONE
           WRITE CLIENT-RECORD
           CLOSE CLIENT-FILE

           DISPLAY "Client ajouté avec succès.".

       CREER-COMPTE.
           DISPLAY "Création d'un compte."
           DISPLAY "Numéro de compte (10 chiffres) : " 
               WITH NO ADVANCING
           ACCEPT NEW-COMPTE-NUM
           DISPLAY "Type de compte (Courant/Epargne) : " 
               WITH NO ADVANCING
           ACCEPT NEW-COMPTE-TYPE
           DISPLAY "ID du client propriétaire : " WITH NO ADVANCING
           ACCEPT NEW-COMPTE-CLIENT-ID
           DISPLAY "Solde initial (ex: 1000.00) : " WITH NO ADVANCING
           ACCEPT NEW-COMPTE-SOLDE

      *>   Vérifier si client existe
           MOVE "N" TO WS-FOUND
           OPEN INPUT CLIENT-FILE
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ CLIENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF CLIENT-ID = NEW-COMPTE-CLIENT-ID
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CLIENT-FILE

           IF WS-FOUND = "N"
               DISPLAY "Erreur : client non trouvé."
               EXIT PARAGRAPH
           END-IF

           OPEN EXTEND COMPTE-FILE
           MOVE NEW-COMPTE-NUM TO COMPTE-NUM
           MOVE NEW-COMPTE-TYPE TO COMPTE-TYPE
           MOVE NEW-COMPTE-SOLDE TO COMPTE-SOLDE
           MOVE NEW-COMPTE-CLIENT-ID TO COMPTE-CLIENT-ID
           WRITE COMPTE-RECORD
           CLOSE COMPTE-FILE

           DISPLAY "Compte créé avec succès.".

       EFFECTUER-TRANSACTION.
           DISPLAY "Effectuer une transaction."
           DISPLAY "Type (Depot, Retrait, Virement) : " 
               WITH NO ADVANCING
           ACCEPT WS-TRANS-TYPE

           IF WS-TRANS-TYPE = "Depot" OR WS-TRANS-TYPE = "Retrait"
               DISPLAY "Numéro du compte concerné : " WITH NO ADVANCING
               ACCEPT WS-INPUT-COMPTE
               DISPLAY "Montant : " WITH NO ADVANCING
               ACCEPT WS-INPUT-MONTANT
           ELSE 
               IF WS-TRANS-TYPE = "Virement"
                   DISPLAY "Numéro du compte source : " 
                       WITH NO ADVANCING
                   ACCEPT WS-INPUT-COMPTE
                   DISPLAY "Numéro du compte destination : " 
                       WITH NO ADVANCING
                   ACCEPT WS-INPUT-ID
                   DISPLAY "Montant : " WITH NO ADVANCING
                   ACCEPT WS-INPUT-MONTANT
               ELSE
                   DISPLAY "Type de transaction invalide."
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   Ouverture fichier compte pour lecture et mise à jour
           MOVE "N" TO WS-FOUND
           OPEN I-O COMPTE-FILE

           PERFORM VARYING WS-COUNTER FROM 1 BY 1 
               UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = WS-INPUT-COMPTE
                           MOVE "Y" TO WS-FOUND
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = "N"
               DISPLAY "Compte source non trouvé."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF

      *>   Selon type de transaction
           IF WS-TRANS-TYPE = "Depot"
               ADD WS-INPUT-MONTANT TO COMPTE-SOLDE
               REWRITE COMPTE-RECORD
               DISPLAY "Dépôt effectué."
           ELSE 
               IF WS-TRANS-TYPE = "Retrait"
                   IF COMPTE-SOLDE < WS-INPUT-MONTANT
                       DISPLAY "Solde insuffisant."
                       CLOSE COMPTE-FILE
                       EXIT PARAGRAPH
                   ELSE
                       SUBTRACT WS-INPUT-MONTANT FROM COMPTE-SOLDE
                       REWRITE COMPTE-RECORD
                       DISPLAY "Retrait effectué."
                   END-IF
               ELSE 
                   IF WS-TRANS-TYPE = "Virement"
      *>               Trouver compte destination
                       MOVE "N" TO WS-FOUND
                       PERFORM VARYING WS-COUNTER FROM 1 BY 1 
                           UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                           READ COMPTE-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                   IF COMPTE-NUM = WS-INPUT-ID
                                       MOVE "Y" TO WS-FOUND
                                       EXIT PERFORM
                                   END-IF
                           END-READ
                       END-PERFORM

                       IF WS-FOUND = "N"
                           DISPLAY "Compte destination non trouvé."
                           CLOSE COMPTE-FILE
                           EXIT PARAGRAPH
                       END-IF

      *>               Vérifier solde source
                       IF COMPTE-SOLDE < WS-INPUT-MONTANT
                           DISPLAY "Solde insuffisant pour virement."
                           CLOSE COMPTE-FILE
                           EXIT PARAGRAPH
                       END-IF

      *>               Soustraire du compte source
                       SUBTRACT WS-INPUT-MONTANT FROM COMPTE-SOLDE
                       REWRITE COMPTE-RECORD

      *>               Ajouter au compte destination
                       MOVE "N" TO WS-FOUND
                       PERFORM VARYING WS-COUNTER FROM 1 BY 1 
                           UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                           READ COMPTE-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                   IF COMPTE-NUM = WS-INPUT-ID
                                       ADD WS-INPUT-MONTANT TO COMPTE-SOLDE
                                       REWRITE COMPTE-RECORD
                                       MOVE "Y" TO WS-FOUND
                                       EXIT PERFORM
                                   END-IF
                           END-READ
                       END-PERFORM

                       DISPLAY "Virement effectué avec succès."
                   END-IF
               END-IF
           END-IF

           CLOSE COMPTE-FILE

      *>   Enregistrer la transaction
           OPEN EXTEND TRANSACTION-FILE
           MOVE WS-COUNTER TO TRANSACTION-ID
           MOVE WS-INPUT-COMPTE TO TRANSACTION-COMPTE
           MOVE WS-TODAY TO TRANSACTION-DATE
           MOVE WS-TRANS-TYPE TO TRANSACTION-TYPE
           MOVE WS-INPUT-MONTANT TO TRANSACTION-MONTANT
           WRITE TRANSACTION-RECORD
           CLOSE TRANSACTION-FILE.

       LISTER-COMPTES-CLIENT.
           DISPLAY "Liste des comptes d'un client."
           DISPLAY "ID client : " WITH NO ADVANCING
           ACCEPT WS-INPUT-ID

           OPEN INPUT COMPTE-FILE
           MOVE "00" TO WS-STATUS
           MOVE "N" TO WS-FOUND

           PERFORM UNTIL WS-STATUS = "10"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-CLIENT-ID = WS-INPUT-ID
                           DISPLAY "Compte : " COMPTE-NUM
                           DISPLAY "Type : " COMPTE-TYPE
                           DISPLAY "Solde : " COMPTE-SOLDE
                           DISPLAY "-----------------------------"
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM
           CLOSE COMPTE-FILE

           IF WS-FOUND = "N"
               DISPLAY "Aucun compte trouvé pour ce client."
           END-IF.