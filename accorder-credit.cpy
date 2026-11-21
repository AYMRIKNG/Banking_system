           DISPLAY "=== Accorder un credit ===".
           DISPLAY "Numero du compte : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.
           DISPLAY "Montant du credit : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-MONTANT.
           DISPLAY "Duree en mois : " WITH NO ADVANCING.
           ACCEPT WS-DUREE.

           IF WS-DUREE = 0
               DISPLAY "Duree invalide."
               EXIT PARAGRAPH
           END-IF.

      *    Verifier que la banque a assez de fonds
           OPEN I-O BANQUE-FILE.
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.

           IF BANQUE-SOLDE < WS-INPUT-MONTANT
               DISPLAY "Fonds de la banque insuffisants."
               CLOSE BANQUE-FILE
               EXIT PARAGRAPH
           END-IF.

      *    Chercher le compte
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = WS-INPUT-COMPTE
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "Compte non trouve."
               CLOSE COMPTE-FILE
               CLOSE BANQUE-FILE
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Compte cloture : credit impossible."
               CLOSE COMPTE-FILE
               CLOSE BANQUE-FILE
               EXIT PARAGRAPH
           END-IF.

           ADD WS-INPUT-MONTANT TO COMPTE-SOLDE.
           REWRITE COMPTE-RECORD.
           CLOSE COMPTE-FILE.

           SUBTRACT WS-INPUT-MONTANT FROM BANQUE-SOLDE.
           REWRITE BANQUE-RECORD.
           CLOSE BANQUE-FILE.

      *    Calcul de la mensualite avec le taux de la banque
           COMPUTE WS-TOTAL-CREDIT ROUNDED =
               WS-INPUT-MONTANT * (1 + BANQUE-INTERETS / 100).
           COMPUTE WS-MENSUALITE ROUNDED = WS-TOTAL-CREDIT / WS-DUREE.

           OPEN EXTEND TRANSACTION-FILE.
           MOVE WS-COUNTER        TO WS-TRANS-ID.
           MOVE WS-INPUT-COMPTE   TO WS-TRANS-COMPTE.
           MOVE WS-TODAY          TO WS-TRANS-DATE.
           MOVE "Credit"          TO WS-TRANS-TYPE.
           MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT.
           MOVE ZERO              TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION    TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           CLOSE TRANSACTION-FILE.
           ADD 1 TO WS-COUNTER.

      *    Garder le credit pour les remboursements
           OPEN EXTEND CREDIT-FILE.
           MOVE WS-INPUT-COMPTE   TO CREDIT-COMPTE.
           MOVE WS-TODAY          TO CREDIT-DATE.
           MOVE WS-TOTAL-CREDIT   TO CREDIT-MONTANT.
           MOVE WS-TOTAL-CREDIT   TO CREDIT-RESTANT.
           MOVE WS-MENSUALITE     TO CREDIT-MENSUALITE.
           MOVE ZEROS             TO CREDIT-DERNIER-MOIS.
           WRITE CREDIT-RECORD.
           CLOSE CREDIT-FILE.

           DISPLAY "Credit accorde : " WS-INPUT-MONTANT.
           DISPLAY "Total a rembourser : " WS-TOTAL-CREDIT.
           DISPLAY "Mensualite : " WS-MENSUALITE
               " sur " WS-DUREE " mois".

           MOVE WS-INPUT-MONTANT TO WS-MONTANT-AFF.
           MOVE "CREDIT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               WS-INPUT-COMPTE DELIMITED BY SIZE
               " Mnt=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
