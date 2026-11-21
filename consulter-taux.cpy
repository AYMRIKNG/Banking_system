           DISPLAY "=== Taux de change (pour 1 EUR) ===".

           MOVE ZEROS TO WS-DEV-NB.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT DEVISE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ DEVISE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF DEV-CODE NOT = SPACES
                               ADD 1 TO WS-DEV-NB
                               MOVE DEV-TAUX TO WS-DEV-TAUX-AFF
                               DISPLAY DEV-CODE
                                   " | Taux: "
                                   FUNCTION TRIM(WS-DEV-TAUX-AFF)
                                   " | Maj: " DEV-DATE
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DEVISE-FILE
           END-IF.

           IF WS-DEV-NB = 0
               DISPLAY "Aucune devise enregistree."
           END-IF.
