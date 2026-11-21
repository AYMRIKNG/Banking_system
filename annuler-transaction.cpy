           DISPLAY "=== Annuler une transaction ===".
           DISPLAY "ID de la transaction : " WITH NO ADVANCING.
           ACCEPT WS-ANN-ID.

      *    Chercher la premiere transaction avec cet ID
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ TRANSACTION-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF TRANSACTION-ID = WS-ANN-ID
                               MOVE TRANSACTION-COMPTE
                                   TO WS-ANN-COMPTE
                               MOVE TRANSACTION-DATE
                                   TO WS-ANN-DATE
                               MOVE TRANSACTION-TYPE
                                   TO WS-ANN-TYPE
                               MOVE TRANSACTION-MONTANT
                                   TO WS-ANN-MONTANT
                               MOVE TRANSACTION-DEST
                                   TO WS-ANN-DEST
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Transaction non trouvee."
               EXIT PARAGRAPH
           END-IF.

           IF WS-ANN-TYPE NOT = "Depot"
              AND WS-ANN-TYPE NOT = "Retrait"
              AND WS-ANN-TYPE NOT = "Virement"
               DISPLAY "Ce type de transaction ne peut pas "
                   "etre annule."
               EXIT PARAGRAPH
           END-IF.

      *    Refuser si la transaction a deja ete annulee
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ TRANSACTION-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF TRANSACTION-TYPE = "Annulation"
                               IF TRANSACTION-DEST = WS-ANN-ID
                                   MOVE "Y" TO WS-FOUND
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF.

           IF WS-FOUND = "Y"
               DISPLAY "Transaction deja annulee."
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Date    : " WS-ANN-DATE.
           DISPLAY "Type    : " WS-ANN-TYPE.
           DISPLAY "Compte  : " WS-ANN-COMPTE.
           DISPLAY "Montant : " WS-ANN-MONTANT.
           IF WS-ANN-TYPE = "Virement"
               DISPLAY "Dest    : " WS-ANN-DEST
           END-IF.

           DISPLAY "Confirmer l'annulation (O/N) : "
               WITH NO ADVANCING.
           ACCEPT WS-CONFIRMATION.

           IF WS-CONFIRMATION NOT = "O" AND WS-CONFIRMATION NOT = "o"
               DISPLAY "Annulation abandonnee."
               EXIT PARAGRAPH
           END-IF.

      *    Contre-passation Depot ou Retrait
           IF WS-ANN-TYPE = "Depot" OR WS-ANN-TYPE = "Retrait"
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN I-O COMPTE-FILE
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-ANN-COMPTE
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM

               IF WS-FOUND = "N"
                   DISPLAY "Compte non trouve."
                   CLOSE COMPTE-FILE
                   EXIT PARAGRAPH
               END-IF

               IF COMPTE-TYPE = "Cloture"
                   DISPLAY "Compte cloture : annulation impossible."
                   CLOSE COMPTE-FILE
                   EXIT PARAGRAPH
               END-IF

               IF WS-ANN-TYPE = "Depot"
                   IF COMPTE-SOLDE < WS-ANN-MONTANT
                       DISPLAY "Solde insuffisant pour annulation."
                       CLOSE COMPTE-FILE
                       EXIT PARAGRAPH
                   END-IF
                   SUBTRACT WS-ANN-MONTANT FROM COMPTE-SOLDE
               ELSE
                   ADD WS-ANN-MONTANT TO COMPTE-SOLDE
               END-IF

               REWRITE COMPTE-RECORD
               CLOSE COMPTE-FILE
           END-IF.

      *    Contre-passation Virement : verifications avant ecriture
           IF WS-ANN-TYPE = "Virement"
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN INPUT COMPTE-FILE
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-ANN-DEST
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM

               IF WS-FOUND = "N"
                   DISPLAY "Compte destination non trouve."
                   CLOSE COMPTE-FILE
                   EXIT PARAGRAPH
               END-IF
               IF COMPTE-TYPE = "Cloture"
                   DISPLAY "Compte destination cloture."
                   CLOSE COMPTE-FILE
                   EXIT PARAGRAPH
               END-IF
               IF COMPTE-SOLDE < WS-ANN-MONTANT
                   DISPLAY "Solde destination insuffisant."
                   CLOSE COMPTE-FILE
                   EXIT PARAGRAPH
               END-IF
               CLOSE COMPTE-FILE

               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN INPUT COMPTE-FILE
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-ANN-COMPTE
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM

               IF WS-FOUND = "N"
                   DISPLAY "Compte source non trouve."
                   CLOSE COMPTE-FILE
                   EXIT PARAGRAPH
               END-IF
               IF COMPTE-TYPE = "Cloture"
                   DISPLAY "Compte source cloture."
                   CLOSE COMPTE-FILE
                   EXIT PARAGRAPH
               END-IF
               CLOSE COMPTE-FILE

      *        Debiter le compte destination
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN I-O COMPTE-FILE
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-ANN-DEST
                               SUBTRACT WS-ANN-MONTANT
                                   FROM COMPTE-SOLDE
                               REWRITE COMPTE-RECORD
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE

      *        Crediter le compte source (relire depuis le debut)
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN I-O COMPTE-FILE
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-ANN-COMPTE
                               ADD WS-ANN-MONTANT TO COMPTE-SOLDE
                               REWRITE COMPTE-RECORD
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
           END-IF.

           OPEN EXTEND TRANSACTION-FILE.
           MOVE WS-COUNTER        TO WS-TRANS-ID.
           MOVE WS-ANN-COMPTE     TO WS-TRANS-COMPTE.
           MOVE WS-TODAY          TO WS-TRANS-DATE.
           MOVE "Annulation"      TO WS-TRANS-TYPE.
           MOVE WS-ANN-MONTANT    TO WS-TRANS-MONTANT.
           MOVE WS-ANN-ID         TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION    TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           CLOSE TRANSACTION-FILE.
           ADD 1 TO WS-COUNTER.

           DISPLAY "Transaction annulee.".

           MOVE "ANNULATION" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Trans=" DELIMITED BY SIZE
               WS-ANN-ID DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
