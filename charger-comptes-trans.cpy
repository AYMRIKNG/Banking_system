      *    Chargement des comptes en memoire (max 500)
           MOVE "N" TO WS-CI-ERREUR.
           MOVE ZEROS TO WS-CI-NB-COMPTES.
           MOVE "N" TO WS-CI-TROP-PLEIN.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur ouverture fichier comptes : " WS-STATUS
               MOVE "O" TO WS-CI-ERREUR
               EXIT PARAGRAPH
           END-IF.
           PERFORM UNTIL WS-STATUS = "10" OR WS-CI-TROP-PLEIN = "O"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF WS-CI-NB-COMPTES >= 500
                           MOVE "O" TO WS-CI-TROP-PLEIN
                       ELSE
                           ADD 1 TO WS-CI-NB-COMPTES
                           MOVE COMPTE-NUM
                               TO WS-TC-NUM(WS-CI-NB-COMPTES)
                           MOVE COMPTE-CLIENT-ID
                               TO WS-TC-CLIENT-ID(WS-CI-NB-COMPTES)
                           MOVE COMPTE-SOLDE
                               TO WS-TC-SOLDE-FIC(WS-CI-NB-COMPTES)
                           MOVE ZEROS
                               TO WS-TC-SOLDE-REC(WS-CI-NB-COMPTES)
                           MOVE ZEROS
                               TO WS-TC-REPORT(WS-CI-NB-COMPTES)
                           MOVE ZEROS
                               TO WS-TC-INTERET(WS-CI-NB-COMPTES)
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

           IF WS-CI-TROP-PLEIN = "O"
               DISPLAY "Erreur : plus de 500 comptes en fichier."
               MOVE "O" TO WS-CI-ERREUR
               EXIT PARAGRAPH
           END-IF.

      *    Chargement des transactions en memoire (max 3000)
           MOVE ZEROS TO WS-CI-NB-TRANS.
           MOVE "N" TO WS-CI-TROP-PLEIN.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10"
                   OR WS-CI-TROP-PLEIN = "O"
                   READ TRANSACTION-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF WS-CI-NB-TRANS >= 3000
                               MOVE "O" TO WS-CI-TROP-PLEIN
                           ELSE
                               ADD 1 TO WS-CI-NB-TRANS
                               MOVE TRANSACTION-ID
                                   TO WS-TT-ID(WS-CI-NB-TRANS)
                               MOVE TRANSACTION-COMPTE
                                   TO WS-TT-COMPTE(WS-CI-NB-TRANS)
                               MOVE TRANSACTION-DATE
                                   TO WS-TT-DATE(WS-CI-NB-TRANS)
                               MOVE TRANSACTION-TYPE
                                   TO WS-TT-TYPE(WS-CI-NB-TRANS)
                               MOVE TRANSACTION-MONTANT
                                   TO WS-TT-MONTANT(WS-CI-NB-TRANS)
                               IF TRANSACTION-DEST IS NUMERIC
                                   MOVE TRANSACTION-DEST
                                       TO WS-TT-DEST(WS-CI-NB-TRANS)
                               ELSE
                                   MOVE ZEROS
                                       TO WS-TT-DEST(WS-CI-NB-TRANS)
                               END-IF
                               MOVE TRANSACTION-RECORD
                                   TO WS-TT-RAW(WS-CI-NB-TRANS)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF.

           IF WS-CI-TROP-PLEIN = "O"
               DISPLAY "Erreur : plus de 3000 transactions en fichier."
               MOVE "O" TO WS-CI-ERREUR
               EXIT PARAGRAPH
           END-IF.
