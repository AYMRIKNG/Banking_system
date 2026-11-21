      *    Sauvegarder WS-STATUS (partage par tous les fichiers)
           MOVE WS-STATUS TO WS-SCW-STATUS.

           MOVE ZEROS TO WS-SC-REVENUS
                         WS-SC-CHARGES
                         WS-SC-ANCIENNETE
                         WS-SC-NB-ALERTES
                         WS-SC-EPARGNE
                         WS-SC-NB-DEBITEURS
                         WS-SC-ENDETTEMENT
                         WS-SC-SCORE.
           MOVE SPACE TO WS-SC-DECISION.
           MOVE SPACES TO WS-SC-MOTIF.
           MOVE WS-MENSUALITE TO WS-SC-MENSUALITE.
           MOVE WS-INPUT-MONTANT TO WS-SC-MONTANT.

           MOVE ZEROS TO WS-SCW-CLIENT-ID
                         WS-SCW-NB-COMPTES
                         WS-SCW-TOTAL-REV
                         WS-SCW-JOUR-MIN.

      *    1. Identifier le client proprietaire de WS-INPUT-COMPTE
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-INPUT-COMPTE
                               MOVE COMPTE-CLIENT-ID
                                   TO WS-SCW-CLIENT-ID
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
           END-IF.

      *    2. Charger les comptes du client et sommer son epargne
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-CLIENT-ID = WS-SCW-CLIENT-ID
                               IF WS-SCW-NB-COMPTES < 50
                                   ADD 1 TO WS-SCW-NB-COMPTES
                                   MOVE COMPTE-NUM TO
                                       WS-SCW-CPT(WS-SCW-NB-COMPTES)
                               END-IF
                               IF COMPTE-TYPE = "Epargne"
                                   IF COMPTE-SOLDE > 0
                                       ADD COMPTE-SOLDE
                                           TO WS-SC-EPARGNE
                                   END-IF
                               END-IF
                               IF COMPTE-TYPE NOT = "Epargne"
                                  AND COMPTE-TYPE NOT = "Cloture"
                                  AND COMPTE-SOLDE < 0
                                   IF WS-SC-NB-DEBITEURS < 999
                                       ADD 1 TO WS-SC-NB-DEBITEURS
                                   END-IF
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
           END-IF.

      *    3. Revenus (90 derniers jours) et anciennete
           COMPUTE WS-SCW-JOUR-AUJ =
               FUNCTION INTEGER-OF-DATE(WS-TODAY).
           IF WS-SCW-JOUR-AUJ > 90
               COMPUTE WS-SCW-JOUR-LIM = WS-SCW-JOUR-AUJ - 90
           ELSE
               MOVE ZERO TO WS-SCW-JOUR-LIM
           END-IF.

           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ TRANSACTION-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF TRANSACTION-DEST IS NUMERIC
                               MOVE TRANSACTION-DEST TO WS-SCW-TR-DEST
                           ELSE
                               MOVE ZEROS TO WS-SCW-TR-DEST
                           END-IF

                           MOVE "N" TO WS-SCW-SRC-CLIENT
                           SET WS-SCW-IDX TO 1
                           SEARCH WS-SCW-TAB-COMPTES
                               AT END
                                   MOVE "N" TO WS-SCW-SRC-CLIENT
                               WHEN WS-SCW-IDX > WS-SCW-NB-COMPTES
                                   MOVE "N" TO WS-SCW-SRC-CLIENT
                               WHEN WS-SCW-CPT(WS-SCW-IDX)
                                   = TRANSACTION-COMPTE
                                   MOVE "O" TO WS-SCW-SRC-CLIENT
                           END-SEARCH

                           MOVE "N" TO WS-SCW-DEST-CLIENT
                           IF WS-SCW-TR-DEST > 0
                               SET WS-SCW-IDX TO 1
                               SEARCH WS-SCW-TAB-COMPTES
                                   AT END
                                       MOVE "N" TO WS-SCW-DEST-CLIENT
                                   WHEN WS-SCW-IDX > WS-SCW-NB-COMPTES
                                       MOVE "N" TO WS-SCW-DEST-CLIENT
                                   WHEN WS-SCW-CPT(WS-SCW-IDX)
                                       = WS-SCW-TR-DEST
                                       MOVE "O" TO WS-SCW-DEST-CLIENT
                               END-SEARCH
                           END-IF

                           IF TRANSACTION-DATE IS NUMERIC
                              AND TRANSACTION-DATE > 0
                               COMPUTE WS-SCW-JOUR-TR =
                                   FUNCTION INTEGER-OF-DATE(
                                       TRANSACTION-DATE)
                           ELSE
                               MOVE ZERO TO WS-SCW-JOUR-TR
                           END-IF

                           IF (WS-SCW-SRC-CLIENT = "O"
                               OR (TRANSACTION-TYPE = "Virement"
                                   AND WS-SCW-DEST-CLIENT = "O"))
                              AND WS-SCW-JOUR-TR > 0
                               IF WS-SCW-JOUR-MIN = 0
                                  OR WS-SCW-JOUR-TR < WS-SCW-JOUR-MIN
                                   MOVE WS-SCW-JOUR-TR
                                       TO WS-SCW-JOUR-MIN
                               END-IF
                           END-IF

                           IF WS-SCW-JOUR-TR >= WS-SCW-JOUR-LIM
                              AND WS-SCW-JOUR-TR <= WS-SCW-JOUR-AUJ
                               IF TRANSACTION-TYPE = "Depot"
                                  AND WS-SCW-SRC-CLIENT = "O"
                                   ADD TRANSACTION-MONTANT
                                       TO WS-SCW-TOTAL-REV
                               END-IF
                               IF TRANSACTION-TYPE = "Virement"
                                  AND WS-SCW-DEST-CLIENT = "O"
                                  AND WS-SCW-SRC-CLIENT = "N"
                                   ADD TRANSACTION-MONTANT
                                       TO WS-SCW-TOTAL-REV
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF.

           COMPUTE WS-SC-REVENUS ROUNDED = WS-SCW-TOTAL-REV / 3.

           IF WS-SCW-JOUR-MIN > 0
              AND WS-SCW-JOUR-AUJ > WS-SCW-JOUR-MIN
               COMPUTE WS-SCW-DIFF-JOURS =
                   WS-SCW-JOUR-AUJ - WS-SCW-JOUR-MIN
               DIVIDE WS-SCW-DIFF-JOURS BY 30
                   GIVING WS-SCW-ANC-CALC
               IF WS-SCW-ANC-CALC > 999
                   MOVE 999 TO WS-SC-ANCIENNETE
               ELSE
                   MOVE WS-SCW-ANC-CALC TO WS-SC-ANCIENNETE
               END-IF
           ELSE
               MOVE ZERO TO WS-SC-ANCIENNETE
           END-IF.

      *    4. Charges mensuelles (credits en cours sur ses comptes)
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CREDIT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ CREDIT-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF CREDIT-RESTANT > 0
                               SET WS-SCW-IDX TO 1
                               SEARCH WS-SCW-TAB-COMPTES
                                   AT END
                                       CONTINUE
                                   WHEN WS-SCW-IDX > WS-SCW-NB-COMPTES
                                       CONTINUE
                                   WHEN WS-SCW-CPT(WS-SCW-IDX)
                                       = CREDIT-COMPTE
                                       ADD CREDIT-MENSUALITE
                                           TO WS-SC-CHARGES
                               END-SEARCH
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CREDIT-FILE
           END-IF.

      *    5. Alertes fraude ouvertes (statut N) sur ses comptes
           MOVE "00" TO WS-STATUS.
           OPEN INPUT ALERTE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ ALERTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF ALERTE-STATUT = "N"
                               SET WS-SCW-IDX TO 1
                               SEARCH WS-SCW-TAB-COMPTES
                                   AT END
                                       CONTINUE
                                   WHEN WS-SCW-IDX > WS-SCW-NB-COMPTES
                                       CONTINUE
                                   WHEN WS-SCW-CPT(WS-SCW-IDX)
                                       = ALERTE-COMPTE
                                       IF WS-SC-NB-ALERTES < 999
                                           ADD 1 TO WS-SC-NB-ALERTES
                                       END-IF
                               END-SEARCH
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE ALERTE-FILE
           END-IF.

           MOVE WS-SCW-STATUS TO WS-STATUS.
