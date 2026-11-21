           DISPLAY "=== Statistiques de la banque ===".

           MOVE ZEROS TO WS-STATS.

           OPEN INPUT BANQUE-FILE.
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.
           CLOSE BANQUE-FILE.

      *    Comptes par type et argent des clients
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       EVALUATE COMPTE-TYPE
                           WHEN "Courant"
                               ADD 1 TO WS-NB-COURANT
                           WHEN "Epargne"
                               ADD 1 TO WS-NB-EPARGNE
                           WHEN "Entreprise"
                               ADD 1 TO WS-NB-ENTREPRISE
                           WHEN "Cloture"
                               ADD 1 TO WS-NB-CLOTURE
                       END-EVALUATE
                       IF COMPTE-TYPE NOT = "Cloture"
                           ADD COMPTE-SOLDE TO WS-TOTAL-DEPOTS
                           IF COMPTE-SOLDE > WS-MAX-SOLDE
                               MOVE COMPTE-SOLDE TO WS-MAX-SOLDE
                               MOVE COMPTE-NUM TO WS-MAX-COMPTE
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

      *    Credits en cours
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CREDIT-FILE.
           PERFORM UNTIL WS-STATUS = "10"
               READ CREDIT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF CREDIT-RESTANT > 0
                           ADD 1 TO WS-NB-CREDITS
                           ADD CREDIT-RESTANT TO WS-TOTAL-RESTANT
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE CREDIT-FILE.

      *    Transactions du mois en cours
           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           PERFORM UNTIL WS-STATUS = "10"
               READ TRANSACTION-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF TRANSACTION-DATE(1:6) = WS-TODAY(1:6)
                           ADD 1 TO WS-NB-TRANS-MOIS
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE TRANSACTION-FILE.

           DISPLAY "Banque             : " BANQUE-NAME.
           MOVE BANQUE-SOLDE TO WS-MONTANT-AFF.
           DISPLAY "Solde banque       : " WS-MONTANT-AFF.
           DISPLAY "Nombre de clients  : " BANQUE-NB-CLIENTS.
           DISPLAY "Comptes courants   : " WS-NB-COURANT.
           DISPLAY "Comptes epargne    : " WS-NB-EPARGNE.
           DISPLAY "Comptes entreprise : " WS-NB-ENTREPRISE.
           DISPLAY "Comptes clotures   : " WS-NB-CLOTURE.
           MOVE WS-TOTAL-DEPOTS TO WS-MONTANT-AFF.
           DISPLAY "Total des soldes   : " WS-MONTANT-AFF.
           MOVE WS-MAX-SOLDE TO WS-MONTANT-AFF.
           DISPLAY "Plus gros compte   : " WS-MAX-COMPTE
               " (" WS-MONTANT-AFF ")".
           DISPLAY "Credits en cours   : " WS-NB-CREDITS.
           MOVE WS-TOTAL-RESTANT TO WS-MONTANT-AFF.
           DISPLAY "Reste a rembourser : " WS-MONTANT-AFF.
           DISPLAY "Transactions mois  : " WS-NB-TRANS-MOIS.
