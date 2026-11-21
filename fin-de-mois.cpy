           DISPLAY "=== Traitement de fin de mois ===".

      *    1. Interets des comptes epargne
           PERFORM INTERET-EPARGNE.

      *    2. Prelevement automatique des mensualites de credit
           DISPLAY "--- Prelevement des mensualites ---".
           MOVE ZEROS TO WS-TOTAL-PRELEVE.
           MOVE ZEROS TO WS-NB-PRELEVES.
           MOVE ZEROS TO WS-NB-IMPAYES.
           MOVE "N" TO WS-FIN-CREDITS.

           OPEN I-O CREDIT-FILE.
           OPEN EXTEND TRANSACTION-FILE.

      *    WS-STATUS sert aussi au fichier compte : drapeau a part
           PERFORM UNTIL WS-FIN-CREDITS = "O"
               READ CREDIT-FILE
                   AT END
                       MOVE "O" TO WS-FIN-CREDITS
                   NOT AT END
                       IF CREDIT-RESTANT > 0
                          AND CREDIT-DERNIER-MOIS NOT = WS-TODAY(1:6)
                           PERFORM PRELEVER-MENSUALITE
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE TRANSACTION-FILE.
           CLOSE CREDIT-FILE.

      *    La banque recupere les mensualites
           IF WS-TOTAL-PRELEVE > 0
               OPEN I-O BANQUE-FILE
               READ BANQUE-FILE
                   AT END
                       DISPLAY "Erreur : banque manquante."
                   NOT AT END
                       ADD WS-TOTAL-PRELEVE TO BANQUE-SOLDE
                       REWRITE BANQUE-RECORD
               END-READ
               CLOSE BANQUE-FILE
           END-IF.

           DISPLAY "Mensualites prelevees : " WS-NB-PRELEVES.
           DISPLAY "Total preleve : " WS-TOTAL-PRELEVE.
           DISPLAY "Impayes : " WS-NB-IMPAYES.

      *    3. Virements permanents
           DISPLAY "--- Virements permanents ---".
           MOVE ZEROS TO WS-NB-VIREMENTS-OK.
           MOVE ZEROS TO WS-NB-VIREMENTS-ECHEC.
           MOVE "N" TO WS-FIN-VIREMENTS.

           OPEN I-O VIREMENT-FILE.
           OPEN EXTEND TRANSACTION-FILE.

      *    WS-STATUS sert aussi au fichier compte : drapeau a part
           PERFORM UNTIL WS-FIN-VIREMENTS = "O"
               READ VIREMENT-FILE
                   AT END
                       MOVE "O" TO WS-FIN-VIREMENTS
                   NOT AT END
                       IF VP-ACTIF = "O"
                          AND VP-DERNIER-MOIS NOT = WS-TODAY(1:6)
                           PERFORM EXECUTER-VIREMENT
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE TRANSACTION-FILE.
           CLOSE VIREMENT-FILE.

           DISPLAY "Virements effectues : " WS-NB-VIREMENTS-OK.
           DISPLAY "Virements en echec : " WS-NB-VIREMENTS-ECHEC.

      *    4. Frais de tenue de compte
           PERFORM FRAIS-TENUE-COMPTE.
