      *    Appele par la fin de mois pour le virement en cours
      *    (VIREMENT-FILE ouvert en I-O, TRANSACTION-FILE en EXTEND)

      *    Verifier le compte destination avant de debiter la source
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = VP-DEST
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "Virement " VP-ID
                   " : compte destination non trouve."
               CLOSE COMPTE-FILE
               ADD 1 TO WS-NB-VIREMENTS-ECHEC
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Virement " VP-ID
                   " : compte destination cloture."
               CLOSE COMPTE-FILE
               ADD 1 TO WS-NB-VIREMENTS-ECHEC
               EXIT PARAGRAPH
           END-IF.
           IF COMPTE-TYPE = "LivretA"
               COMPUTE WS-LA-SOLDE-PROJ = COMPTE-SOLDE + VP-MONTANT
               IF WS-LA-SOLDE-PROJ > WS-PLAFOND-LIVRETA
                   DISPLAY "Virement " VP-ID
                       " : plafond livret A depasse sur " VP-DEST
                   CLOSE COMPTE-FILE
                   ADD 1 TO WS-NB-VIREMENTS-ECHEC
                   EXIT PARAGRAPH
               END-IF
           END-IF.
           CLOSE COMPTE-FILE.

      *    Verifier et debiter le compte source
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = VP-SOURCE
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "Virement " VP-ID
                   " : compte source non trouve."
               CLOSE COMPTE-FILE
               ADD 1 TO WS-NB-VIREMENTS-ECHEC
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Virement " VP-ID
                   " : compte source cloture."
               CLOSE COMPTE-FILE
               ADD 1 TO WS-NB-VIREMENTS-ECHEC
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Epargne" OR COMPTE-TYPE = "LivretA"
               MOVE ZEROS TO WS-DEC-PLAFOND
           ELSE
               MOVE COMPTE-NUM TO WS-DEC-COMPTE
               PERFORM LIRE-DECOUVERT
           END-IF.
           COMPUTE WS-DEC-SOLDE-PROJ = COMPTE-SOLDE - VP-MONTANT.
           COMPUTE WS-DEC-LIMITE = 0 - WS-DEC-PLAFOND.
           IF WS-DEC-SOLDE-PROJ < WS-DEC-LIMITE
               DISPLAY "Virement " VP-ID
                   " : depassement du decouvert sur " VP-SOURCE
               CLOSE COMPTE-FILE
               ADD 1 TO WS-NB-VIREMENTS-ECHEC
               EXIT PARAGRAPH
           END-IF.

           SUBTRACT VP-MONTANT FROM COMPTE-SOLDE.
           REWRITE COMPTE-RECORD.
           CLOSE COMPTE-FILE.

      *    Crediter le compte destination
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = VP-DEST
                           ADD VP-MONTANT TO COMPTE-SOLDE
                           REWRITE COMPTE-RECORD
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

           MOVE WS-COUNTER        TO WS-TRANS-ID.
           MOVE VP-SOURCE         TO WS-TRANS-COMPTE.
           MOVE WS-TODAY          TO WS-TRANS-DATE.
           MOVE "Virement"        TO WS-TRANS-TYPE.
           MOVE VP-MONTANT        TO WS-TRANS-MONTANT.
           MOVE VP-DEST           TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION    TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           ADD 1 TO WS-COUNTER.

           MOVE WS-TODAY(1:6) TO VP-DERNIER-MOIS.
           REWRITE VIREMENT-RECORD.

           ADD 1 TO WS-NB-VIREMENTS-OK.
           DISPLAY "Virement " VP-ID " : " VP-SOURCE
               " -> " VP-DEST " (" VP-MONTANT ")".
