           DISPLAY "=== Interets des comptes epargne ===".

      *    Une seule fois par mois
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ TRANSACTION-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF TRANSACTION-TYPE = "Interet"
                          AND TRANSACTION-DATE(1:6) = WS-TODAY(1:6)
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE TRANSACTION-FILE.

           IF WS-FOUND = "Y"
               DISPLAY "Interets deja verses ce mois-ci."
               EXIT PARAGRAPH
           END-IF.

           OPEN I-O BANQUE-FILE.
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.

           DISPLAY "Taux applique : " BANQUE-INTERETS " %".

           MOVE ZEROS TO WS-TOTAL-INTERETS.
           OPEN I-O COMPTE-FILE.
           OPEN EXTEND TRANSACTION-FILE.
           MOVE "00" TO WS-STATUS.

           PERFORM UNTIL WS-STATUS = "10"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-TYPE = "Epargne"
                          AND COMPTE-SOLDE > 0
                           COMPUTE WS-INTERET ROUNDED =
                               COMPTE-SOLDE * BANQUE-INTERETS / 100
                           IF WS-INTERET > 0
                               ADD WS-INTERET TO COMPTE-SOLDE
                               REWRITE COMPTE-RECORD
                               ADD WS-INTERET TO WS-TOTAL-INTERETS

                               MOVE WS-COUNTER       TO WS-TRANS-ID
                               MOVE COMPTE-NUM       TO WS-TRANS-COMPTE
                               MOVE WS-TODAY         TO WS-TRANS-DATE
                               MOVE "Interet"        TO WS-TRANS-TYPE
                               MOVE WS-INTERET       TO WS-TRANS-MONTANT
                               MOVE ZERO             TO WS-TRANS-DEST
                               MOVE WS-TRANSACTION TO TRANSACTION-RECORD
                               WRITE TRANSACTION-RECORD
                               ADD 1 TO WS-COUNTER

                               DISPLAY "Compte " COMPTE-NUM
                                   " : +" WS-INTERET
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE TRANSACTION-FILE.
           CLOSE COMPTE-FILE.

      *    La banque paie les interets
           IF WS-TOTAL-INTERETS > BANQUE-SOLDE
               DISPLAY "Attention : solde banque insuffisant."
               MOVE ZEROS TO BANQUE-SOLDE
           ELSE
               SUBTRACT WS-TOTAL-INTERETS FROM BANQUE-SOLDE
           END-IF.
           REWRITE BANQUE-RECORD.
           CLOSE BANQUE-FILE.

           DISPLAY "Total des interets verses : " WS-TOTAL-INTERETS.
