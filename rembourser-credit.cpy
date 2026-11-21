           DISPLAY "=== Rembourser un credit ===".
           DISPLAY "Numero du compte : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.

      *    Chercher un credit en cours sur ce compte
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O CREDIT-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ CREDIT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF CREDIT-COMPTE = WS-INPUT-COMPTE
                          AND CREDIT-RESTANT > 0
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "Aucun credit en cours pour ce compte."
               CLOSE CREDIT-FILE
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Reste a payer : " CREDIT-RESTANT.
           DISPLAY "Mensualite : " CREDIT-MENSUALITE.

      *    La derniere mensualite peut etre plus petite
           IF CREDIT-RESTANT < CREDIT-MENSUALITE
               MOVE CREDIT-RESTANT TO WS-INPUT-MONTANT
           ELSE
               MOVE CREDIT-MENSUALITE TO WS-INPUT-MONTANT
           END-IF.

      *    Debiter le compte
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
               CLOSE CREDIT-FILE
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Epargne" OR COMPTE-TYPE = "LivretA"
               MOVE ZEROS TO WS-DEC-PLAFOND
           ELSE
               MOVE COMPTE-NUM TO WS-DEC-COMPTE
               PERFORM LIRE-DECOUVERT
           END-IF.
           COMPUTE WS-DEC-SOLDE-PROJ =
               COMPTE-SOLDE - WS-INPUT-MONTANT.
           COMPUTE WS-DEC-LIMITE = 0 - WS-DEC-PLAFOND.
           IF WS-DEC-SOLDE-PROJ < WS-DEC-LIMITE
               DISPLAY "Depassement du decouvert autorise."
               CLOSE COMPTE-FILE
               CLOSE CREDIT-FILE
               EXIT PARAGRAPH
           END-IF.

           SUBTRACT WS-INPUT-MONTANT FROM COMPTE-SOLDE.
           REWRITE COMPTE-RECORD.
           CLOSE COMPTE-FILE.

           SUBTRACT WS-INPUT-MONTANT FROM CREDIT-RESTANT.
           MOVE WS-TODAY(1:6) TO CREDIT-DERNIER-MOIS.
           REWRITE CREDIT-RECORD.
           CLOSE CREDIT-FILE.

      *    La banque recupere l'argent
           OPEN I-O BANQUE-FILE.
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.
           ADD WS-INPUT-MONTANT TO BANQUE-SOLDE.
           REWRITE BANQUE-RECORD.
           CLOSE BANQUE-FILE.

           OPEN EXTEND TRANSACTION-FILE.
           MOVE WS-COUNTER        TO WS-TRANS-ID.
           MOVE WS-INPUT-COMPTE   TO WS-TRANS-COMPTE.
           MOVE WS-TODAY          TO WS-TRANS-DATE.
           MOVE "Rembourse"       TO WS-TRANS-TYPE.
           MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT.
           MOVE ZERO              TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION    TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           CLOSE TRANSACTION-FILE.
           ADD 1 TO WS-COUNTER.

           DISPLAY "Mensualite payee : " WS-INPUT-MONTANT.
           IF CREDIT-RESTANT = 0
               DISPLAY "Credit rembourse en totalite."
           ELSE
               DISPLAY "Reste a payer : " CREDIT-RESTANT
           END-IF.

           MOVE WS-INPUT-MONTANT TO WS-MONTANT-AFF.
           MOVE "REMBOURSEMENT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               WS-INPUT-COMPTE DELIMITED BY SIZE
               " Mnt=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
