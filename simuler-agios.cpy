           DISPLAY "=== Simulation agios du mois ===".
           DISPLAY "Numero de compte : " WITH NO ADVANCING.
           ACCEPT WS-AG-COMPTE.
           DISPLAY "Mois (AAAAMM) : " WITH NO ADVANCING.
           ACCEPT WS-AG-MOIS.

           IF WS-AG-ANNEE < 1900 OR WS-AG-MM < 1 OR WS-AG-MM > 12
               DISPLAY "Mois invalide."
               EXIT PARAGRAPH
           END-IF.

           IF WS-AG-MOIS > WS-TODAY(1:6)
               DISPLAY "Refus : mois futur."
               EXIT PARAGRAPH
           END-IF.

           MOVE "O" TO WS-AG-DETAIL.
           PERFORM CALCULER-AGIOS.

           IF WS-AG-FOUND = "N"
               DISPLAY "Compte non trouve."
               EXIT PARAGRAPH
           END-IF.

           IF WS-AG-ERREUR = "O"
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-AG-TOTAL-CPT = WS-AG-AGIOS + WS-AG-COMMISSION.
           MOVE WS-TAUX-AGIOS    TO WS-AG-AFF-TAUX.
           MOVE WS-DEC-PLAFOND   TO WS-AG-AFF-DEC.
           MOVE WS-AG-NOMBRES    TO WS-AG-AFF-NOMBRES.
           MOVE WS-AG-AGIOS      TO WS-AG-AFF-TOT-A.
           MOVE WS-AG-COMMISSION TO WS-AG-AFF-TOT-C.
           MOVE WS-AG-TOTAL-CPT  TO WS-MONTANT-AFF.

           DISPLAY "Compte              : " WS-AG-COMPTE.
           DISPLAY "Mois                : " WS-AG-MOIS.
           DISPLAY "Decouvert autorise  : "
               FUNCTION TRIM(WS-AG-AFF-DEC).
           DISPLAY "Taux annuel agios   : "
               FUNCTION TRIM(WS-AG-AFF-TAUX) " %".
           DISPLAY "Jours debiteurs     : " WS-AG-NB-JOURS-DEBIT.
           DISPLAY "Nombres debiteurs   : "
               FUNCTION TRIM(WS-AG-AFF-NOMBRES).
           DISPLAY "Agios               : "
               FUNCTION TRIM(WS-AG-AFF-TOT-A).
           DISPLAY "Jours depassement   : " WS-AG-NB-DEPASSEMENTS.
           DISPLAY "Commission interv.  : "
               FUNCTION TRIM(WS-AG-AFF-TOT-C).
           DISPLAY "Total a prelever    : "
               FUNCTION TRIM(WS-MONTANT-AFF).
