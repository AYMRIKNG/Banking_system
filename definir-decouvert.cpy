           DISPLAY "=== Definir decouvert autorise ===".
           DISPLAY "Numero du compte : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
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
           CLOSE COMPTE-FILE.

           IF WS-FOUND = "N"
               DISPLAY "Compte non trouve."
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Compte cloture : decouvert impossible."
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Epargne"
               DISPLAY "Pas de decouvert sur un compte epargne."
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Nouveau plafond (0 = supprime) : "
               WITH NO ADVANCING.
           ACCEPT WS-DEC-NEW-PLAFOND.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O DECOUVERT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ DECOUVERT-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF DEC-COMPTE = WS-INPUT-COMPTE
                               MOVE WS-DEC-NEW-PLAFOND TO DEC-PLAFOND
                               REWRITE DECOUVERT-RECORD
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DECOUVERT-FILE
           END-IF.

           IF WS-FOUND = "N" AND WS-DEC-NEW-PLAFOND > 0
               MOVE "00" TO WS-STATUS
               OPEN EXTEND DECOUVERT-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   MOVE WS-INPUT-COMPTE    TO DEC-COMPTE
                   MOVE WS-DEC-NEW-PLAFOND TO DEC-PLAFOND
                   WRITE DECOUVERT-RECORD
                   CLOSE DECOUVERT-FILE
               END-IF
           END-IF.

           MOVE WS-DEC-NEW-PLAFOND TO WS-DEC-AFF.
           IF WS-DEC-NEW-PLAFOND = 0
               DISPLAY "Decouvert autorise supprime."
           ELSE
               DISPLAY "Decouvert autorise defini : "
                   FUNCTION TRIM(WS-DEC-AFF)
           END-IF.

           MOVE "DECOUVERT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               WS-INPUT-COMPTE DELIMITED BY SIZE
               " Plafond=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-DEC-AFF) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
