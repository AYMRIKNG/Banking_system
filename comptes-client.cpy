           DISPLAY "Liste des comptes d'un client.".
           DISPLAY "ID client : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-ID.

           OPEN INPUT COMPTE-FILE.
           MOVE "00" TO WS-STATUS.
           MOVE "N" TO WS-FOUND.

           PERFORM UNTIL WS-STATUS = "10"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-CLIENT-ID = WS-INPUT-ID
                           DISPLAY "Compte : " COMPTE-NUM
                           DISPLAY "Type : " COMPTE-TYPE
                           DISPLAY "Solde : " COMPTE-SOLDE
                           DISPLAY "-----------------------------"
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

           IF WS-FOUND = "N"
               DISPLAY "Aucun compte trouve pour ce client."
           END-IF.
