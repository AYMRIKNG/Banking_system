           DISPLAY "=== Mettre a jour un taux ===".
           MOVE SPACES TO WS-DEV-CODE.
           DISPLAY "Code devise (3 lettres, ex: USD) : "
               WITH NO ADVANCING.
           ACCEPT WS-DEV-CODE.
           MOVE FUNCTION UPPER-CASE(WS-DEV-CODE) TO WS-DEV-CODE.

           MOVE "O" TO WS-DEV-OK.
           PERFORM VARYING WS-VI-IDX FROM 1 BY 1
               UNTIL WS-VI-IDX > 3
               IF WS-DEV-CODE(WS-VI-IDX:1) < "A"
                  OR WS-DEV-CODE(WS-VI-IDX:1) > "Z"
                   MOVE "N" TO WS-DEV-OK
               END-IF
           END-PERFORM.

           IF WS-DEV-OK = "N"
               DISPLAY "Code devise invalide (3 lettres attendues)."
               EXIT PARAGRAPH
           END-IF.

           MOVE ZEROS TO WS-DEV-TAUX-SAISIE.
           DISPLAY "Nouveau taux (unites pour 1 EUR) : "
               WITH NO ADVANCING.
           ACCEPT WS-DEV-TAUX-SAISIE.

           IF WS-DEV-TAUX-SAISIE <= 0
               DISPLAY "Taux invalide (doit etre superieur a 0)."
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O DEVISE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ DEVISE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF DEV-CODE = WS-DEV-CODE
                               MOVE WS-DEV-TAUX-SAISIE TO DEV-TAUX
                               MOVE WS-TODAY TO DEV-DATE
                               REWRITE DEVISE-RECORD
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DEVISE-FILE
           END-IF.

           IF WS-FOUND = "N"
               MOVE "00" TO WS-STATUS
               OPEN EXTEND DEVISE-FILE
               MOVE WS-DEV-CODE        TO DEV-CODE
               MOVE WS-DEV-TAUX-SAISIE TO DEV-TAUX
               MOVE WS-TODAY           TO DEV-DATE
               WRITE DEVISE-RECORD
               CLOSE DEVISE-FILE
               DISPLAY "Nouvelle devise ajoutee : " WS-DEV-CODE
           ELSE
               DISPLAY "Taux mis a jour pour " WS-DEV-CODE "."
           END-IF.

           MOVE WS-DEV-TAUX-SAISIE TO WS-DEV-TAUX-AFF.
           MOVE "TAUX" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING WS-DEV-CODE DELIMITED BY SIZE
               "=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-DEV-TAUX-AFF) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
