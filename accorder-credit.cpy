           DISPLAY "=== Accorder un credit ===".
           DISPLAY "Numero du compte : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.
           DISPLAY "Montant du credit : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-MONTANT.
           DISPLAY "Duree en mois : " WITH NO ADVANCING.
           ACCEPT WS-DUREE.

           IF WS-DUREE = 0
               DISPLAY "Duree invalide."
               EXIT PARAGRAPH
           END-IF.

      *    Verifier les fonds de la banque (lecture seule)
           MOVE "00" TO WS-STATUS.
           OPEN INPUT BANQUE-FILE.
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.
           CLOSE BANQUE-FILE.

           IF BANQUE-SOLDE < WS-INPUT-MONTANT
               DISPLAY "Fonds de la banque insuffisants."
               EXIT PARAGRAPH
           END-IF.

      *    Verifier le compte (lecture seule)
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
               DISPLAY "Compte cloture : credit impossible."
               EXIT PARAGRAPH
           END-IF.

      *    Calcul de la mensualite avec le taux de la banque
           MOVE "M"                TO WS-CC-MODE.
           MOVE WS-INPUT-MONTANT   TO WS-CC-CAPITAL.
           MOVE BANQUE-INTERETS    TO WS-CC-TAUX-ANNUEL.
           MOVE WS-DUREE           TO WS-CC-DUREE.
           MOVE ZEROS              TO WS-CC-MENSUALITE
                                      WS-CC-COUT-TOTAL
                                      WS-CC-TOTAL
                                      WS-CC-RETOUR.
           CALL "CALCUL-CREDIT" USING WS-CALCUL-CREDIT.
           IF WS-CC-RETOUR NOT = 0
               DISPLAY "Parametres de credit invalides."
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-CC-TOTAL        TO WS-TOTAL-CREDIT.
           MOVE WS-CC-MENSUALITE   TO WS-MENSUALITE.

      *    Scoring client avant toute ecriture
           PERFORM CALCULER-SCORE.
           CALL "SCORING-CREDIT" USING WS-SCORING.

           DISPLAY "=== Fiche de scoring ===".
           MOVE WS-SC-REVENUS TO WS-MONTANT-AFF.
           DISPLAY "Revenus mensuels    : "
               FUNCTION TRIM(WS-MONTANT-AFF).
           MOVE WS-SC-CHARGES TO WS-MONTANT-AFF.
           DISPLAY "Charges mensuelles  : "
               FUNCTION TRIM(WS-MONTANT-AFF).
           MOVE WS-SC-MENSUALITE TO WS-MONTANT-AFF.
           DISPLAY "Nouvelle mensualite : "
               FUNCTION TRIM(WS-MONTANT-AFF).
           MOVE WS-SC-ENDETTEMENT TO WS-SCW-TAUX-AFF.
           DISPLAY "Taux endettement    : "
               FUNCTION TRIM(WS-SCW-TAUX-AFF) " %".
           DISPLAY "Anciennete          : "
               WS-SC-ANCIENNETE " mois".
           DISPLAY "Alertes ouvertes    : "
               WS-SC-NB-ALERTES.
           DISPLAY "Comptes debiteurs   : "
               WS-SC-NB-DEBITEURS.
           MOVE WS-SC-EPARGNE TO WS-MONTANT-AFF.
           DISPLAY "Epargne totale      : "
               FUNCTION TRIM(WS-MONTANT-AFF).
           DISPLAY "Score               : "
               WS-SC-SCORE " / 100".
           EVALUATE WS-SC-DECISION
               WHEN "A"
                   DISPLAY "Decision            : A (Accepte)"
               WHEN "E"
                   DISPLAY "Decision            : E (A etudier)"
               WHEN OTHER
                   DISPLAY "Decision            : R (Refuse)"
           END-EVALUATE.
           DISPLAY "Motif               : "
               FUNCTION TRIM(WS-SC-MOTIF).
           DISPLAY "=== Fin fiche scoring ===".

           MOVE "SCORING" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               WS-INPUT-COMPTE DELIMITED BY SIZE
               " Score=" DELIMITED BY SIZE
               WS-SC-SCORE DELIMITED BY SIZE
               " Dec=" DELIMITED BY SIZE
               WS-SC-DECISION DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.

           IF WS-SC-DECISION = "R"
               DISPLAY "Credit refuse."
               EXIT PARAGRAPH
           END-IF.

           IF WS-SC-DECISION = "E"
               DISPLAY "Accorder malgre tout (O/N) : "
                   WITH NO ADVANCING
               ACCEPT WS-CONFIRMATION
               IF WS-CONFIRMATION NOT = "O"
                  AND WS-CONFIRMATION NOT = "o"
                   DISPLAY "Credit annule."
                   EXIT PARAGRAPH
               END-IF
           END-IF.

      *    Decision validee : mise a jour des fichiers
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = WS-INPUT-COMPTE
                           ADD WS-INPUT-MONTANT TO COMPTE-SOLDE
                           REWRITE COMPTE-RECORD
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

           MOVE "00" TO WS-STATUS.
           OPEN I-O BANQUE-FILE.
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.
           SUBTRACT WS-INPUT-MONTANT FROM BANQUE-SOLDE.
           REWRITE BANQUE-RECORD.
           CLOSE BANQUE-FILE.

           OPEN EXTEND TRANSACTION-FILE.
           MOVE WS-COUNTER        TO WS-TRANS-ID.
           MOVE WS-INPUT-COMPTE   TO WS-TRANS-COMPTE.
           MOVE WS-TODAY          TO WS-TRANS-DATE.
           MOVE "Credit"          TO WS-TRANS-TYPE.
           MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT.
           MOVE ZERO              TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION    TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           CLOSE TRANSACTION-FILE.
           ADD 1 TO WS-COUNTER.

      *    Garder le credit pour les remboursements
           OPEN EXTEND CREDIT-FILE.
           MOVE WS-INPUT-COMPTE   TO CREDIT-COMPTE.
           MOVE WS-TODAY          TO CREDIT-DATE.
           MOVE WS-TOTAL-CREDIT   TO CREDIT-MONTANT.
           MOVE WS-TOTAL-CREDIT   TO CREDIT-RESTANT.
           MOVE WS-MENSUALITE     TO CREDIT-MENSUALITE.
           MOVE ZEROS             TO CREDIT-DERNIER-MOIS.
           WRITE CREDIT-RECORD.
           CLOSE CREDIT-FILE.

           DISPLAY "Credit accorde : " WS-INPUT-MONTANT.
           DISPLAY "Total a rembourser : " WS-TOTAL-CREDIT.
           DISPLAY "Mensualite : " WS-MENSUALITE
               " sur " WS-DUREE " mois".

           MOVE WS-INPUT-MONTANT TO WS-MONTANT-AFF.
           MOVE "CREDIT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               WS-INPUT-COMPTE DELIMITED BY SIZE
               " Mnt=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
