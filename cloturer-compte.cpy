           DISPLAY "=== Cloture d'un compte ===".
           DISPLAY "Numero du compte : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.

      *    Pas de cloture si un credit est en cours
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CREDIT-FILE.
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
           CLOSE CREDIT-FILE.

           IF WS-FOUND = "Y"
               DISPLAY "Credit en cours : cloture impossible."
               EXIT PARAGRAPH
           END-IF.

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
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Compte deja cloture."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-SOLDE < 0
               DISPLAY "Solde debiteur : regularisez avant cloture."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Type  : " COMPTE-TYPE.
           MOVE COMPTE-SOLDE TO WS-SOLDE-AFF.
           DISPLAY "Solde : " FUNCTION TRIM(WS-SOLDE-AFF).
           DISPLAY "Confirmer la cloture (O/N) : " WITH NO ADVANCING.
           ACCEPT WS-CONFIRMATION.

           IF WS-CONFIRMATION NOT = "O" AND WS-CONFIRMATION NOT = "o"
               DISPLAY "Cloture annulee."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

      *    Le solde restant est rendu au client
           MOVE COMPTE-SOLDE TO WS-INPUT-MONTANT.
           MOVE ZEROS TO COMPTE-SOLDE.
           MOVE "Cloture" TO COMPTE-TYPE.
           REWRITE COMPTE-RECORD.
           CLOSE COMPTE-FILE.

           OPEN EXTEND TRANSACTION-FILE.
           MOVE WS-COUNTER        TO WS-TRANS-ID.
           MOVE WS-INPUT-COMPTE   TO WS-TRANS-COMPTE.
           MOVE WS-TODAY          TO WS-TRANS-DATE.
           MOVE "Cloture"         TO WS-TRANS-TYPE.
           MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT.
           MOVE ZERO              TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION    TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           CLOSE TRANSACTION-FILE.
           ADD 1 TO WS-COUNTER.

           DISPLAY "Compte cloture. Solde rendu : " WS-INPUT-MONTANT.

           MOVE WS-INPUT-MONTANT TO WS-MONTANT-AFF.
           MOVE "CLOTURE CPT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               WS-INPUT-COMPTE DELIMITED BY SIZE
               " Solde=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
