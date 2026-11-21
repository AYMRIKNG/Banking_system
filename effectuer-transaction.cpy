           DISPLAY "Effectuer une transaction.".
           DISPLAY "(Depot, Retrait, Virement) : " WITH NO ADVANCING.
           ACCEPT WS-TRANS-TYPE.

           IF WS-MODE-SESSION = "U"
              AND (WS-TRANS-TYPE = "Depot"
                   OR WS-TRANS-TYPE = "Retrait")
               DISPLAY "Operation reservee au guichet."
               EXIT PARAGRAPH
           END-IF.

           IF WS-TRANS-TYPE = "Depot" OR WS-TRANS-TYPE = "Retrait"
               DISPLAY "Numero du compte concerne : " WITH NO ADVANCING
               ACCEPT WS-INPUT-COMPTE
               DISPLAY "Montant : " WITH NO ADVANCING
               ACCEPT WS-INPUT-MONTANT
           ELSE
               IF WS-TRANS-TYPE = "Virement"
                DISPLAY "Numero du compte source : " WITH NO ADVANCING
                ACCEPT WS-INPUT-COMPTE
                IF WS-MODE-SESSION = "U"
                    PERFORM VERIFIER-PROPRIETAIRE
                    IF WS-PROPRIETAIRE-OK NOT = "O"
                        DISPLAY "Ce compte ne vous appartient pas."
                        EXIT PARAGRAPH
                    END-IF
                END-IF
                DISPLAY "Numero du compte desti : " WITH NO ADVANCING
                ACCEPT WS-INPUT-ID
                DISPLAY "Montant : " WITH NO ADVANCING
                ACCEPT WS-INPUT-MONTANT
               ELSE
                   DISPLAY "Type de transaction invalide."
                   EXIT PARAGRAPH
               END-IF
           END-IF.

      *    Plafond de retrait par jour et par compte
           IF WS-TRANS-TYPE = "Retrait"
               MOVE ZEROS TO WS-TOTAL-JOUR
               MOVE "00" TO WS-STATUS
               OPEN INPUT TRANSACTION-FILE
               PERFORM UNTIL WS-STATUS = "10"
                   READ TRANSACTION-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF TRANSACTION-COMPTE = WS-INPUT-COMPTE
                              AND TRANSACTION-TYPE = "Retrait"
                              AND TRANSACTION-DATE = WS-TODAY
                               ADD TRANSACTION-MONTANT TO WS-TOTAL-JOUR
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
               ADD WS-INPUT-MONTANT TO WS-TOTAL-JOUR
               IF WS-TOTAL-JOUR > WS-PLAFOND-RETRAIT
                   DISPLAY "Plafond de retrait journalier depasse."
                   DISPLAY "Plafond : " WS-PLAFOND-RETRAIT
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

           MOVE COMPTE-SOLDE TO WS-SOLDE-AVANT.

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
                   IF COMPTE-TYPE = "Epargne"
                       MOVE ZEROS TO WS-DEC-PLAFOND
                   ELSE
                       MOVE COMPTE-NUM TO WS-DEC-COMPTE
                       PERFORM LIRE-DECOUVERT
                   END-IF
                   COMPUTE WS-DEC-SOLDE-PROJ =
                       COMPTE-SOLDE - WS-INPUT-MONTANT
                   COMPUTE WS-DEC-LIMITE = 0 - WS-DEC-PLAFOND
                   IF WS-DEC-SOLDE-PROJ < WS-DEC-LIMITE
                       DISPLAY "Depassement du decouvert autorise."
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
                       IF COMPTE-TYPE = "Epargne"
                           MOVE ZEROS TO WS-DEC-PLAFOND
                       ELSE
                           MOVE COMPTE-NUM TO WS-DEC-COMPTE
                           PERFORM LIRE-DECOUVERT
                       END-IF
                       COMPUTE WS-DEC-SOLDE-PROJ =
                           COMPTE-SOLDE - WS-INPUT-MONTANT
                       COMPUTE WS-DEC-LIMITE = 0 - WS-DEC-PLAFOND
                       IF WS-DEC-SOLDE-PROJ < WS-DEC-LIMITE
                           DISPLAY "Depassement du decouvert autorise."
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

           MOVE WS-INPUT-MONTANT TO WS-MONTANT-AFF.
           MOVE "TRANSACTION" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           IF WS-TRANS-TYPE = "Virement"
               STRING FUNCTION TRIM(WS-TRANS-TYPE) DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   WS-INPUT-COMPTE DELIMITED BY SIZE
                   "->" DELIMITED BY SIZE
                   WS-INPUT-ID DELIMITED BY SIZE
                   " Mnt=" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
           ELSE
               STRING FUNCTION TRIM(WS-TRANS-TYPE) DELIMITED BY SIZE
                   " Cpt=" DELIMITED BY SIZE
                   WS-INPUT-COMPTE DELIMITED BY SIZE
                   " Mnt=" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
           END-IF.
           PERFORM ECRIRE-JOURNAL.

           IF WS-TRANS-TYPE = "Retrait"
              OR WS-TRANS-TYPE = "Virement"
               PERFORM CONTROLE-FRAUDE
           END-IF.


