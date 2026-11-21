           DISPLAY "=== Convertir un montant ===".
           MOVE SPACES TO WS-DEV-CODE.
           DISPLAY "Code devise (ex: USD) : " WITH NO ADVANCING.
           ACCEPT WS-DEV-CODE.
           MOVE FUNCTION UPPER-CASE(WS-DEV-CODE) TO WS-DEV-CODE.

           MOVE "N" TO WS-FOUND.
           MOVE ZEROS TO WS-DEV-TAUX-LU.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT DEVISE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ DEVISE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF DEV-CODE = WS-DEV-CODE
                               MOVE DEV-TAUX TO WS-DEV-TAUX-LU
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DEVISE-FILE
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Devise inconnue."
               EXIT PARAGRAPH
           END-IF.

           MOVE ZEROS TO WS-DEV-MONTANT.
           DISPLAY "Montant en " WS-DEV-CODE " : " WITH NO ADVANCING.
           ACCEPT WS-DEV-MONTANT.

           IF WS-DEV-MONTANT <= 0
               DISPLAY "Montant invalide."
               EXIT PARAGRAPH
           END-IF.

           MOVE SPACE TO WS-DEV-SENS.
           DISPLAY "Sens (A=Achat devise, V=Vente devise) : "
               WITH NO ADVANCING.
           ACCEPT WS-DEV-SENS.
           MOVE FUNCTION UPPER-CASE(WS-DEV-SENS) TO WS-DEV-SENS.

           IF WS-DEV-SENS NOT = "A" AND WS-DEV-SENS NOT = "V"
               DISPLAY "Sens invalide (A ou V)."
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-DEV-SENS      TO WS-CONV-MODE.
           MOVE WS-DEV-TAUX-LU   TO WS-CONV-TAUX-MOYEN.
           MOVE WS-DEV-MONTANT   TO WS-CONV-MONTANT-DEV.
           MOVE WS-MARGE-DEFAUT  TO WS-CONV-MARGE-PCT.
           MOVE ZEROS TO WS-CONV-TAUX-APPL
                         WS-CONV-MONTANT-EUR
                         WS-CONV-RETOUR.

           CALL "CONVERSION" USING WS-CONVERSION.

           IF WS-CONV-RETOUR NOT = 0
               DISPLAY "Erreur de conversion."
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-DEV-EUR-MOYEN ROUNDED =
               WS-DEV-MONTANT / WS-DEV-TAUX-LU.
           IF WS-DEV-SENS = "A"
               COMPUTE WS-DEV-DIFF-EUR =
                   WS-CONV-MONTANT-EUR - WS-DEV-EUR-MOYEN
           ELSE
               COMPUTE WS-DEV-DIFF-EUR =
                   WS-DEV-EUR-MOYEN - WS-CONV-MONTANT-EUR
           END-IF.
           IF WS-DEV-DIFF-EUR < 0
               MOVE ZEROS TO WS-DEV-MARGE-EUR
           ELSE
               MOVE WS-DEV-DIFF-EUR TO WS-DEV-MARGE-EUR
           END-IF.

           MOVE WS-DEV-TAUX-LU      TO WS-DEV-TAUX-AFF.
           DISPLAY "Taux moyen    : " FUNCTION TRIM(WS-DEV-TAUX-AFF).
           MOVE WS-CONV-TAUX-APPL   TO WS-DEV-TAUX-AFF.
           DISPLAY "Taux applique : " FUNCTION TRIM(WS-DEV-TAUX-AFF).
           MOVE WS-CONV-MONTANT-EUR TO WS-MONTANT-AFF.
           DISPLAY "Montant EUR   : " FUNCTION TRIM(WS-MONTANT-AFF)
               " EUR".
           MOVE WS-DEV-MARGE-EUR    TO WS-MONTANT-AFF.
           DISPLAY "Marge banque  : " FUNCTION TRIM(WS-MONTANT-AFF)
               " EUR".
