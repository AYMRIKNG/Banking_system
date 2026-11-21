           DISPLAY "=== Arreter un virement permanent ===".

      *    Lister les virements actifs
           MOVE ZEROS TO WS-VP-NB-ACTIFS.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT VIREMENT-FILE.
           PERFORM UNTIL WS-STATUS = "10"
               READ VIREMENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF VP-ACTIF = "O"
                           ADD 1 TO WS-VP-NB-ACTIFS
                           DISPLAY "ID: " VP-ID
                               " | Src: " VP-SOURCE
                               " | Dest: " VP-DEST
                               " | Montant: " VP-MONTANT
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE VIREMENT-FILE.

           IF WS-VP-NB-ACTIFS = 0
               DISPLAY "Aucun virement permanent actif."
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "ID du virement a arreter : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-VP-ID.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O VIREMENT-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ VIREMENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF VP-ID = WS-INPUT-VP-ID
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "Virement permanent non trouve."
               CLOSE VIREMENT-FILE
               EXIT PARAGRAPH
           END-IF.

           IF VP-ACTIF = "N"
               DISPLAY "Virement permanent deja arrete."
               CLOSE VIREMENT-FILE
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO VP-ACTIF.
           REWRITE VIREMENT-RECORD.
           CLOSE VIREMENT-FILE.

           DISPLAY "Virement permanent " WS-INPUT-VP-ID " arrete.".
