      *    Sauvegarder WS-STATUS (partage par tous les fichiers)
           MOVE WS-STATUS TO WS-FRAUDE-STATUS.

      *    Compter Retrait + Virement du compte aujourd'hui
           MOVE ZEROS TO WS-NB-OPS-JOUR.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10"
                   READ TRANSACTION-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF TRANSACTION-COMPTE = WS-INPUT-COMPTE
                              AND TRANSACTION-DATE = WS-TODAY
                              AND (TRANSACTION-TYPE = "Retrait"
                                   OR TRANSACTION-TYPE = "Virement"
                                   OR TRANSACTION-TYPE = "VirInter")
                               ADD 1 TO WS-NB-OPS-JOUR
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF.

           COMPUTE WS-SOLDE-APRES =
               WS-SOLDE-AVANT - WS-INPUT-MONTANT.

      *    Evaluer chaque regle de fraude
           PERFORM VARYING WS-FRAUDE-IDX FROM 1 BY 1
               UNTIL WS-FRAUDE-IDX > 3
               MOVE SPACES TO WS-ALERTE-REGLE-CUR
               EVALUATE WS-FRAUDE-IDX
                   WHEN 1
                       IF WS-INPUT-MONTANT >= WS-SEUIL-MONTANT
                           MOVE "GROS MONTANT" TO WS-ALERTE-REGLE-CUR
                       END-IF
                   WHEN 2
                       IF WS-NB-OPS-JOUR >= WS-SEUIL-RAFALE
                           MOVE "RAFALE" TO WS-ALERTE-REGLE-CUR
                       END-IF
                   WHEN 3
                       IF WS-INPUT-MONTANT >= WS-SEUIL-VIDAGE
                          AND WS-SOLDE-AVANT > 0
                          AND (WS-SOLDE-APRES <= 0
                               OR (WS-SOLDE-APRES * 10)
                                  < WS-SOLDE-AVANT)
                           MOVE "VIDAGE" TO WS-ALERTE-REGLE-CUR
                       END-IF
               END-EVALUATE

               IF WS-ALERTE-REGLE-CUR NOT = SPACES
                   MOVE ZEROS TO WS-MAX-ALERTE-ID
                   MOVE "00" TO WS-STATUS
                   OPEN INPUT ALERTE-FILE
                   IF WS-STATUS = "00" OR WS-STATUS = "05"
                       PERFORM UNTIL WS-STATUS = "10"
                           READ ALERTE-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                   IF ALERTE-ID > WS-MAX-ALERTE-ID
                                       MOVE ALERTE-ID
                                           TO WS-MAX-ALERTE-ID
                                   END-IF
                           END-READ
                       END-PERFORM
                       CLOSE ALERTE-FILE
                   END-IF

                   ADD 1 TO WS-MAX-ALERTE-ID

                   MOVE "00" TO WS-STATUS
                   OPEN EXTEND ALERTE-FILE
                   IF WS-STATUS = "00" OR WS-STATUS = "05"
                       MOVE WS-MAX-ALERTE-ID    TO ALERTE-ID
                       MOVE WS-TODAY            TO ALERTE-DATE
                       MOVE WS-INPUT-COMPTE     TO ALERTE-COMPTE
                       MOVE WS-ALERTE-REGLE-CUR TO ALERTE-REGLE
                       MOVE WS-INPUT-MONTANT    TO ALERTE-MONTANT
                       MOVE "N"                 TO ALERTE-STATUT
                       WRITE ALERTE-RECORD
                       CLOSE ALERTE-FILE
                   END-IF

                   DISPLAY "*** ALERTE FRAUDE : "
                       FUNCTION TRIM(WS-ALERTE-REGLE-CUR)

                   MOVE WS-INPUT-MONTANT TO WS-MONTANT-AFF
                   MOVE "ALERTE" TO WS-JOURNAL-ACTION
                   MOVE SPACES TO WS-JOURNAL-DETAIL
                   STRING FUNCTION TRIM(WS-ALERTE-REGLE-CUR)
                       DELIMITED BY SIZE
                       " Cpt=" DELIMITED BY SIZE
                       WS-INPUT-COMPTE DELIMITED BY SIZE
                       " Mnt=" DELIMITED BY SIZE
                       FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
                       INTO WS-JOURNAL-DETAIL
                   END-STRING
                   PERFORM ECRIRE-JOURNAL
               END-IF
           END-PERFORM.

           MOVE WS-FRAUDE-STATUS TO WS-STATUS.
