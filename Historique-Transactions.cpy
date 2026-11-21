           DISPLAY "Historique des transactions pour un compte".
           DISPLAY "Numero du compte : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.

           IF WS-MODE-SESSION = "U"
               PERFORM VERIFIER-PROPRIETAIRE
               IF WS-PROPRIETAIRE-OK NOT = "O"
                   DISPLAY "Ce compte ne vous appartient pas."
                   EXIT PARAGRAPH
               END-IF
           END-IF.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.

           OPEN INPUT TRANSACTION-FILE.

           PERFORM UNTIL WS-STATUS = "10"
               READ TRANSACTION-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF TRANSACTION-COMPTE = WS-INPUT-COMPTE
                           DISPLAY "-----------------------------"
                           DISPLAY "ID        : " TRANSACTION-ID
                           DISPLAY "Date      : " TRANSACTION-DATE
                           DISPLAY "Type      : " TRANSACTION-TYPE
                           DISPLAY "Montant   : " TRANSACTION-MONTANT
                           IF TRANSACTION-TYPE = "Virement"
                               DISPLAY "Destination : " TRANSACTION-DEST
                           END-IF
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE TRANSACTION-FILE.

           IF WS-FOUND = "N"
               DISPLAY "Aucune transaction trouvee pour ce compte."
           END-IF.
