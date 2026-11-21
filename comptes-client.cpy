           IF WS-MODE-SESSION = "U"
               DISPLAY "Mes comptes :"
               MOVE WS-CLIENT-CONNECTE TO WS-INPUT-ID
           ELSE
               DISPLAY "Liste des comptes d'un client."
               DISPLAY "ID client : " WITH NO ADVANCING
               ACCEPT WS-INPUT-ID
           END-IF.

           OPEN INPUT COMPTE-FILE.
           MOVE "00" TO WS-STATUS.
           MOVE "N" TO WS-FOUND.

           PERFORM UNTIL WS-STATUS = "10"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-CLIENT-ID = WS-INPUT-ID
                           MOVE "G" TO WS-IBAN-MODE
                           MOVE COMPTE-NUM TO WS-IBAN-COMPTE
                           MOVE SPACES TO WS-IBAN-TEXTE
                                          WS-IBAN-AFFICHE
                           MOVE ZERO TO WS-IBAN-RETOUR
                           CALL "CALCUL-IBAN" USING WS-CALCUL-IBAN
                           DISPLAY "Compte : " COMPTE-NUM
                           DISPLAY "Type : " COMPTE-TYPE
                           DISPLAY "Solde : " COMPTE-SOLDE
                           DISPLAY "IBAN : " WS-IBAN-AFFICHE
                           DISPLAY "-----------------------------"
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

           IF WS-FOUND = "N"
               DISPLAY "Aucun compte trouve pour ce client."
           END-IF.
