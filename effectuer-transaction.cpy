           DISPLAY "Effectuer une transaction.".
           DISPLAY "(Depot, Retrait, Virement) : " WITH NO ADVANCING.
           ACCEPT WS-TRANS-TYPE.

           IF WS-TRANS-TYPE = "Depot" OR WS-TRANS-TYPE = "Retrait"
               DISPLAY "Numero du compte concerne : " WITH NO ADVANCING
               ACCEPT WS-INPUT-COMPTE
               DISPLAY "Montant : " WITH NO ADVANCING
               ACCEPT WS-INPUT-MONTANT
           ELSE
               IF WS-TRANS-TYPE = "Virement"
                DISPLAY "Numero du compte source : " WITH NO ADVANCING
                ACCEPT WS-INPUT-COMPTE
                DISPLAY "Numero du compte desti : " WITH NO ADVANCING
                ACCEPT WS-INPUT-ID
                DISPLAY "Montant : " WITH NO ADVANCING
                ACCEPT WS-INPUT-MONTANT
               ELSE
                   DISPLAY "Type de transaction invalide."
                   EXIT PARAGRAPH
               END-IF
           END-IF.

      *    Verifier le compte destination avant de debiter
           IF WS-TRANS-TYPE = "Virement"
               IF WS-INPUT-ID = WS-INPUT-COMPTE
                   DISPLAY "Compte source et destination identiques."
                   EXIT PARAGRAPH
               END-IF
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN INPUT COMPTE-FILE
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-INPUT-ID
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
               IF WS-FOUND = "N"
                   DISPLAY "Compte destination non trouve."
                   EXIT PARAGRAPH
               END-IF
               IF COMPTE-TYPE = "Cloture"
                   DISPLAY "Compte destination cloture."
                   EXIT PARAGRAPH
               END-IF
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
               DISPLAY "Compte source non trouve."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Compte cloture : operation impossible."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

           IF WS-TRANS-TYPE = "Depot"
               ADD WS-INPUT-MONTANT TO COMPTE-SOLDE
               REWRITE COMPTE-RECORD
               DISPLAY "Depot effectue."
               
               OPEN EXTEND TRANSACTION-FILE
               MOVE WS-COUNTER        TO WS-TRANS-ID
               MOVE WS-INPUT-COMPTE   TO WS-TRANS-COMPTE
               MOVE WS-TODAY          TO WS-TRANS-DATE
               MOVE WS-TRANS-TYPE     TO WS-TRANS-TYPE
               MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT
               MOVE ZERO              TO WS-TRANS-DEST
               MOVE WS-TRANSACTION    TO TRANSACTION-RECORD
               WRITE TRANSACTION-RECORD
               CLOSE TRANSACTION-FILE
               ADD 1 TO WS-COUNTER

           ELSE
               IF WS-TRANS-TYPE = "Retrait"
                   IF COMPTE-SOLDE < WS-INPUT-MONTANT
                       DISPLAY "Solde insuffisant."
                       CLOSE COMPTE-FILE
                       EXIT PARAGRAPH
                   ELSE
                       SUBTRACT WS-INPUT-MONTANT FROM COMPTE-SOLDE
                       REWRITE COMPTE-RECORD
                       DISPLAY "Retrait effectue."

                       OPEN EXTEND TRANSACTION-FILE
                       MOVE WS-COUNTER        TO WS-TRANS-ID
                       MOVE WS-INPUT-COMPTE   TO WS-TRANS-COMPTE
                       MOVE WS-TODAY          TO WS-TRANS-DATE
                       MOVE WS-TRANS-TYPE     TO WS-TRANS-TYPE
                       MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT
                       MOVE ZERO              TO WS-TRANS-DEST
                       MOVE WS-TRANSACTION    TO TRANSACTION-RECORD
                       WRITE TRANSACTION-RECORD
                       CLOSE TRANSACTION-FILE
                       ADD 1 TO WS-COUNTER
                   END-IF
               ELSE
                   IF WS-TRANS-TYPE = "Virement"
                       IF COMPTE-SOLDE < WS-INPUT-MONTANT
                           DISPLAY "Solde insuffisant pour virement."
                           CLOSE COMPTE-FILE
                           EXIT PARAGRAPH
                       END-IF

                       SUBTRACT WS-INPUT-MONTANT FROM COMPTE-SOLDE
                       REWRITE COMPTE-RECORD
                       DISPLAY "Compte source debite."

      *                Repartir du debut : la destination peut etre
      *                avant le compte source dans le fichier
                       CLOSE COMPTE-FILE
                       OPEN I-O COMPTE-FILE
                       MOVE "N" TO WS-FOUND
                       MOVE "00" TO WS-STATUS
                       PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                           READ COMPTE-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                  IF COMPTE-NUM = WS-INPUT-ID
                                   ADD WS-INPUT-MONTANT TO COMPTE-SOLDE
                                   REWRITE COMPTE-RECORD
                                   DISPLAY "Compte destination credite."
                                   MOVE "Y" TO WS-FOUND
                                  END-IF
                           END-READ
                       END-PERFORM

                       IF WS-FOUND = "N"
                           DISPLAY "Compte destination non trouve."
                       END-IF

                       OPEN EXTEND TRANSACTION-FILE
                       MOVE WS-COUNTER        TO WS-TRANS-ID
                       MOVE WS-INPUT-COMPTE   TO WS-TRANS-COMPTE
                       MOVE WS-TODAY          TO WS-TRANS-DATE
                       MOVE WS-TRANS-TYPE     TO WS-TRANS-TYPE
                       MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT
                       MOVE WS-INPUT-ID       TO WS-TRANS-DEST
                       MOVE WS-TRANSACTION    TO TRANSACTION-RECORD
                       WRITE TRANSACTION-RECORD
                       CLOSE TRANSACTION-FILE
                       ADD 1 TO WS-COUNTER
                   END-IF
               END-IF
           END-IF.

           CLOSE COMPTE-FILE.


