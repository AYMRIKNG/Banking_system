           DISPLAY "=== Simuler un credit ===".

           OPEN INPUT BANQUE-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur ouverture fichier banque : " WS-STATUS
               EXIT PARAGRAPH
           END-IF.
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.
           CLOSE BANQUE-FILE.

           DISPLAY "Montant du credit : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-MONTANT.
           DISPLAY "Duree en mois : " WITH NO ADVANCING.
           ACCEPT WS-DUREE.

           MOVE "T"                TO WS-CC-MODE.
           MOVE WS-INPUT-MONTANT   TO WS-CC-CAPITAL.
           MOVE BANQUE-INTERETS    TO WS-CC-TAUX-ANNUEL.
           MOVE WS-DUREE           TO WS-CC-DUREE.
           MOVE ZEROS              TO WS-CC-MENSUALITE
                                      WS-CC-COUT-TOTAL
                                      WS-CC-TOTAL
                                      WS-CC-RETOUR.

           CALL "CALCUL-CREDIT" USING WS-CALCUL-CREDIT.

           IF WS-CC-RETOUR NOT = 0
               DISPLAY "Parametres invalides (montant ou duree a 0)."
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CC-MENSUALITE TO WS-MONTANT-AFF.
           DISPLAY "Mensualite         : " WS-MONTANT-AFF.
           MOVE WS-CC-TOTAL TO WS-MONTANT-AFF.
           DISPLAY "Total a rembourser : " WS-MONTANT-AFF.
           MOVE WS-CC-COUT-TOTAL TO WS-MONTANT-AFF.
           DISPLAY "Cout du credit     : " WS-MONTANT-AFF.
           DISPLAY "Tableau enregistre dans amortissement.txt".
