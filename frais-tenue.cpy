           DISPLAY "--- Frais de tenue de compte ---".

      *    Une seule fois par mois
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ TRANSACTION-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF TRANSACTION-TYPE = "Frais"
                          AND TRANSACTION-DATE(1:6) = WS-TODAY(1:6)
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE TRANSACTION-FILE.

           IF WS-FOUND = "Y"
               DISPLAY "Frais deja preleves ce mois-ci."
               EXIT PARAGRAPH
           END-IF.

           MOVE ZEROS TO WS-TOTAL-FRAIS.
           MOVE ZEROS TO WS-NB-FRAIS.
           MOVE ZEROS TO WS-NB-FRAIS-ECHEC.

           OPEN I-O COMPTE-FILE.
           OPEN EXTEND TRANSACTION-FILE.
           MOVE "00" TO WS-STATUS.
           PERFORM UNTIL WS-STATUS = "10"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       EVALUATE COMPTE-TYPE
                           WHEN "Courant"
                               MOVE WS-FRAIS-COURANT TO WS-FRAIS
                           WHEN "Entreprise"
                               MOVE WS-FRAIS-ENTREPRISE TO WS-FRAIS
                           WHEN OTHER
                               MOVE ZEROS TO WS-FRAIS
                       END-EVALUATE
                       IF WS-FRAIS > 0
                           IF COMPTE-SOLDE < WS-FRAIS
                               DISPLAY "Compte " COMPTE-NUM
                                   " : solde insuffisant pour les frais"
                               ADD 1 TO WS-NB-FRAIS-ECHEC
                           ELSE
                               SUBTRACT WS-FRAIS FROM COMPTE-SOLDE
                               REWRITE COMPTE-RECORD
                               ADD WS-FRAIS TO WS-TOTAL-FRAIS
                               ADD 1 TO WS-NB-FRAIS

                               MOVE WS-COUNTER     TO WS-TRANS-ID
                               MOVE COMPTE-NUM     TO WS-TRANS-COMPTE
                               MOVE WS-TODAY       TO WS-TRANS-DATE
                               MOVE "Frais"        TO WS-TRANS-TYPE
                               MOVE WS-FRAIS       TO WS-TRANS-MONTANT
                               MOVE ZERO           TO WS-TRANS-DEST
                               MOVE WS-TRANSACTION TO TRANSACTION-RECORD
                               WRITE TRANSACTION-RECORD
                               ADD 1 TO WS-COUNTER
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE TRANSACTION-FILE.
           CLOSE COMPTE-FILE.

      *    Les frais vont a la banque
           IF WS-TOTAL-FRAIS > 0
               OPEN I-O BANQUE-FILE
               READ BANQUE-FILE
                   AT END
                       DISPLAY "Erreur : banque manquante."
                   NOT AT END
                       ADD WS-TOTAL-FRAIS TO BANQUE-SOLDE
                       REWRITE BANQUE-RECORD
               END-READ
               CLOSE BANQUE-FILE
           END-IF.

           DISPLAY "Comptes preleves : " WS-NB-FRAIS.
           DISPLAY "Total des frais : " WS-TOTAL-FRAIS.
           DISPLAY "Frais non preleves : " WS-NB-FRAIS-ECHEC.
