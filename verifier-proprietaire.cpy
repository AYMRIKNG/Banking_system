      *    Sauvegarder WS-STATUS (partage par tous les fichiers)
           MOVE WS-STATUS TO WS-AUTH-STATUS.
           MOVE "N" TO WS-PROPRIETAIRE-OK.

           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10"
                  OR WS-PROPRIETAIRE-OK = "O"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-INPUT-COMPTE
                              AND COMPTE-CLIENT-ID
                                  = WS-CLIENT-CONNECTE
                               MOVE "O" TO WS-PROPRIETAIRE-OK
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
           END-IF.

           MOVE WS-AUTH-STATUS TO WS-STATUS.
