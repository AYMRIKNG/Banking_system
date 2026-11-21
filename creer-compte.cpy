           DISPLAY "Creation d'un compte.".
           MOVE ZEROS TO WS-MAX-COMPTE-NUM.
           OPEN INPUT COMPTE-FILE.
           MOVE "00" TO WS-STATUS.
           PERFORM UNTIL WS-STATUS = "10"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM > WS-MAX-COMPTE-NUM
                           MOVE COMPTE-NUM TO WS-MAX-COMPTE-NUM
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

           ADD 1 TO WS-MAX-COMPTE-NUM GIVING WS-NOUVEAU-NUM.
           MOVE WS-NOUVEAU-NUM TO NEW-COMPTE-NUM.
           DISPLAY "Numero de compte: " NEW-COMPTE-NUM.


           DISPLAY "Choisir le type de compte :".
           DISPLAY "1 - Courant".
           DISPLAY "2 - Epargne".
           DISPLAY "3 - Entreprise".
           DISPLAY "Votre choix : " WITH NO ADVANCING.
           ACCEPT WS-COMPTE-CHOIX.
    
           EVALUATE WS-COMPTE-CHOIX
               WHEN 1
                   MOVE "Courant" TO NEW-COMPTE-TYPE
               WHEN 2
                   MOVE "Epargne" TO NEW-COMPTE-TYPE
               WHEN 3
                   MOVE "Entreprise" TO NEW-COMPTE-TYPE
               WHEN OTHER
                   DISPLAY "Choix invalide. Type par defaut : Courant"
                   MOVE "Courant" TO NEW-COMPTE-TYPE
           END-EVALUATE.

           DISPLAY "ID du client proprietaire : " WITH NO ADVANCING.
           ACCEPT NEW-COMPTE-CLIENT-ID.
           DISPLAY "Solde initial (ex: 1000.00) : " WITH NO ADVANCING.
           ACCEPT NEW-COMPTE-SOLDE.
           
           IF NEW-COMPTE-SOLDE < BANQUE-FRAIS-COMPTE
               DISPLAY "Solde initial insuffisant"
               EXIT PARAGRAPH
           END-IF
           
           SUBTRACT BANQUE-FRAIS-COMPTE FROM NEW-COMPTE-SOLDE.
           
      *    Verifier si client existe
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CLIENT-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ CLIENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF CLIENT-ID = NEW-COMPTE-CLIENT-ID
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE CLIENT-FILE.

           IF WS-FOUND = "N"
               DISPLAY "Erreur : client non trouve."
               EXIT PARAGRAPH
           END-IF.

           OPEN EXTEND COMPTE-FILE.
           MOVE NEW-COMPTE-NUM TO COMPTE-NUM.
           MOVE NEW-COMPTE-TYPE TO COMPTE-TYPE.
           MOVE NEW-COMPTE-SOLDE TO COMPTE-SOLDE.
           MOVE NEW-COMPTE-CLIENT-ID TO COMPTE-CLIENT-ID.
           WRITE COMPTE-RECORD.
           CLOSE COMPTE-FILE.

           DISPLAY "Compte cree avec succes.".

           OPEN EXTEND TRANSACTION-FILE.
           MOVE WS-COUNTER         TO WS-TRANS-ID.
           MOVE NEW-COMPTE-NUM     TO WS-TRANS-COMPTE.
           MOVE WS-TODAY           TO WS-TRANS-DATE.
           MOVE "Retrait"          TO WS-TRANS-TYPE.
           MOVE BANQUE-FRAIS-COMPTE TO WS-TRANS-MONTANT.
           MOVE 0                  TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION     TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           CLOSE TRANSACTION-FILE.
           
           ADD 1 TO WS-COUNTER.
           
           DISPLAY "Ajout des frais bancaires au solde de la banque."

           OPEN I-O BANQUE-FILE.
           
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.
           
           ADD BANQUE-FRAIS-COMPTE TO BANQUE-SOLDE.
           
           REWRITE BANQUE-RECORD.
           DISPLAY "S " BANQUE-FRAIS-COMPTE.

           
           CLOSE BANQUE-FILE.

           MOVE "CREATION CPT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               NEW-COMPTE-NUM DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(NEW-COMPTE-TYPE) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
           
