      *    Appele par la fin de mois pour le credit en cours de lecture
      *    (CREDIT-FILE ouvert en I-O, TRANSACTION-FILE en EXTEND)

      *    La derniere mensualite peut etre plus petite
           IF CREDIT-RESTANT < CREDIT-MENSUALITE
               MOVE CREDIT-RESTANT TO WS-INPUT-MONTANT
           ELSE
               MOVE CREDIT-MENSUALITE TO WS-INPUT-MONTANT
           END-IF.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = CREDIT-COMPTE
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "Compte " CREDIT-COMPTE " non trouve."
               CLOSE COMPTE-FILE
               ADD 1 TO WS-NB-IMPAYES
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
               DISPLAY "Impaye : compte " CREDIT-COMPTE
                   " depassement du decouvert autorise."
               CLOSE COMPTE-FILE
               ADD 1 TO WS-NB-IMPAYES
               EXIT PARAGRAPH
           END-IF.

           SUBTRACT WS-INPUT-MONTANT FROM COMPTE-SOLDE.
           REWRITE COMPTE-RECORD.
           CLOSE COMPTE-FILE.

           SUBTRACT WS-INPUT-MONTANT FROM CREDIT-RESTANT.
           MOVE WS-TODAY(1:6) TO CREDIT-DERNIER-MOIS.
           REWRITE CREDIT-RECORD.

           MOVE WS-COUNTER        TO WS-TRANS-ID.
           MOVE CREDIT-COMPTE     TO WS-TRANS-COMPTE.
           MOVE WS-TODAY          TO WS-TRANS-DATE.
           MOVE "Rembourse"       TO WS-TRANS-TYPE.
           MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT.
           MOVE ZERO              TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION    TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           ADD 1 TO WS-COUNTER.

           ADD WS-INPUT-MONTANT TO WS-TOTAL-PRELEVE.
           ADD 1 TO WS-NB-PRELEVES.
           DISPLAY "Compte " CREDIT-COMPTE " : -" WS-INPUT-MONTANT.
