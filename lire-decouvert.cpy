      *    Lire le plafond de decouvert d'un compte (0 si absent)
      *    Sauvegarde et restaure WS-STATUS (partage par les fichiers)
           MOVE WS-STATUS TO WS-DEC-STATUS.
           MOVE ZEROS TO WS-DEC-PLAFOND.
           MOVE "N" TO WS-DEC-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT DECOUVERT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-DEC-FOUND = "Y"
                   READ DECOUVERT-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF DEC-COMPTE = WS-DEC-COMPTE
                               MOVE DEC-PLAFOND TO WS-DEC-PLAFOND
                               MOVE "Y" TO WS-DEC-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DECOUVERT-FILE
           END-IF.
           MOVE WS-DEC-STATUS TO WS-STATUS.
