      *    Chargement des comptes en memoire (max 500)
           MOVE ZEROS TO WS-CI-NB-COMPTES.
           MOVE "N" TO WS-CI-TROP-PLEIN.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur ouverture fichier comptes : " WS-STATUS
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
                           MOVE COMPTE-SOLDE
                               TO WS-TC-SOLDE-FIC(WS-CI-NB-COMPTES)
                           MOVE ZEROS
                               TO WS-TC-SOLDE-REC(WS-CI-NB-COMPTES)
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

           IF WS-CI-TROP-PLEIN = "O"
               DISPLAY "Erreur : plus de 500 comptes en fichier."
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
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF.

           IF WS-CI-TROP-PLEIN = "O"
               DISPLAY "Erreur : plus de 3000 transactions en fichier."
               EXIT PARAGRAPH
           END-IF.

      *    Rejeu des transactions dans l'ordre du fichier
           MOVE ZEROS TO WS-CI-NB-ANOMALIES.
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               EVALUATE WS-TT-TYPE(WS-CI-I)
                   WHEN "Ouverture"
                   WHEN "Depot"
                   WHEN "Interet"
                   WHEN "Credit"
                       SET WS-CI-IDX-C TO 1
                       SEARCH WS-TAB-COMPTES
                           AT END
                               CONTINUE
                           WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                               CONTINUE
                           WHEN WS-TC-NUM(WS-CI-IDX-C)
                               = WS-TT-COMPTE(WS-CI-I)
                               ADD WS-TT-MONTANT(WS-CI-I)
                                   TO WS-TC-SOLDE-REC(WS-CI-IDX-C)
                       END-SEARCH

                   WHEN "Retrait"
                   WHEN "Frais"
                   WHEN "Rembourse"
                   WHEN "Cloture"
                       SET WS-CI-IDX-C TO 1
                       SEARCH WS-TAB-COMPTES
                           AT END
                               CONTINUE
                           WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                               CONTINUE
                           WHEN WS-TC-NUM(WS-CI-IDX-C)
                               = WS-TT-COMPTE(WS-CI-I)
                               SUBTRACT WS-TT-MONTANT(WS-CI-I)
                                   FROM WS-TC-SOLDE-REC(WS-CI-IDX-C)
                       END-SEARCH

                   WHEN "Virement"
                       SET WS-CI-IDX-C TO 1
                       SEARCH WS-TAB-COMPTES
                           AT END
                               CONTINUE
                           WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                               CONTINUE
                           WHEN WS-TC-NUM(WS-CI-IDX-C)
                               = WS-TT-COMPTE(WS-CI-I)
                               SUBTRACT WS-TT-MONTANT(WS-CI-I)
                                   FROM WS-TC-SOLDE-REC(WS-CI-IDX-C)
                       END-SEARCH
                       IF WS-TT-DEST(WS-CI-I) > 0
                           SET WS-CI-IDX-C TO 1
                           SEARCH WS-TAB-COMPTES
                               AT END
                                   CONTINUE
                               WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                                   CONTINUE
                               WHEN WS-TC-NUM(WS-CI-IDX-C)
                                   = WS-TT-DEST(WS-CI-I)
                                   ADD WS-TT-MONTANT(WS-CI-I)
                                       TO WS-TC-SOLDE-REC(WS-CI-IDX-C)
                           END-SEARCH
                       END-IF

                   WHEN "Report"
                       SET WS-CI-IDX-C TO 1
                       SEARCH WS-TAB-COMPTES
                           AT END
                               CONTINUE
                           WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                               CONTINUE
                           WHEN WS-TC-NUM(WS-CI-IDX-C)
                               = WS-TT-COMPTE(WS-CI-I)
                               MOVE WS-TT-MONTANT(WS-CI-I)
                                   TO WS-TC-SOLDE-REC(WS-CI-IDX-C)
                       END-SEARCH

                   WHEN "Annulation"
                       MOVE "N" TO WS-FOUND
                       SET WS-CI-IDX-T TO 1
                       SEARCH WS-TAB-TRANS
                           AT END
                               CONTINUE
                           WHEN WS-CI-IDX-T > WS-CI-NB-TRANS
                               CONTINUE
                           WHEN WS-TT-ID(WS-CI-IDX-T)
                               = WS-TT-DEST(WS-CI-I)
                               MOVE "Y" TO WS-FOUND
                               MOVE WS-TT-COMPTE(WS-CI-IDX-T)
                                   TO WS-CI-CIBLE-COMPTE
                               MOVE WS-TT-TYPE(WS-CI-IDX-T)
                                   TO WS-CI-CIBLE-TYPE
                               MOVE WS-TT-MONTANT(WS-CI-IDX-T)
                                   TO WS-CI-CIBLE-MONTANT
                               MOVE WS-TT-DEST(WS-CI-IDX-T)
                                   TO WS-CI-CIBLE-DEST
                       END-SEARCH
                       IF WS-FOUND = "N"
                           ADD 1 TO WS-CI-NB-ANOMALIES
                           DISPLAY "Anomalie : cible introuvable ID "
                               WS-TT-ID(WS-CI-I)
                       ELSE
                           EVALUATE WS-CI-CIBLE-TYPE
                               WHEN "Depot"
                                   SET WS-CI-IDX-C TO 1
                                   SEARCH WS-TAB-COMPTES
                                       AT END
                                           CONTINUE
                                       WHEN WS-CI-IDX-C
                                           > WS-CI-NB-COMPTES
                                           CONTINUE
                                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                                           = WS-CI-CIBLE-COMPTE
                                           SUBTRACT WS-CI-CIBLE-MONTANT
                                               FROM WS-TC-SOLDE-REC(
                                                   WS-CI-IDX-C)
                                   END-SEARCH
                               WHEN "Retrait"
                                   SET WS-CI-IDX-C TO 1
                                   SEARCH WS-TAB-COMPTES
                                       AT END
                                           CONTINUE
                                       WHEN WS-CI-IDX-C
                                           > WS-CI-NB-COMPTES
                                           CONTINUE
                                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                                           = WS-CI-CIBLE-COMPTE
                                           ADD WS-CI-CIBLE-MONTANT
                                               TO WS-TC-SOLDE-REC(
                                                   WS-CI-IDX-C)
                                   END-SEARCH
                               WHEN "Virement"
                                   SET WS-CI-IDX-C TO 1
                                   SEARCH WS-TAB-COMPTES
                                       AT END
                                           CONTINUE
                                       WHEN WS-CI-IDX-C
                                           > WS-CI-NB-COMPTES
                                           CONTINUE
                                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                                           = WS-CI-CIBLE-COMPTE
                                           ADD WS-CI-CIBLE-MONTANT
                                               TO WS-TC-SOLDE-REC(
                                                   WS-CI-IDX-C)
                                   END-SEARCH
                                   IF WS-CI-CIBLE-DEST > 0
                                       SET WS-CI-IDX-C TO 1
                                       SEARCH WS-TAB-COMPTES
                                           AT END
                                               CONTINUE
                                           WHEN WS-CI-IDX-C
                                               > WS-CI-NB-COMPTES
                                               CONTINUE
                                           WHEN WS-TC-NUM(WS-CI-IDX-C)
                                               = WS-CI-CIBLE-DEST
                                               SUBTRACT
                                                   WS-CI-CIBLE-MONTANT
                                                   FROM WS-TC-SOLDE-REC(
                                                       WS-CI-IDX-C)
                                       END-SEARCH
                                   END-IF
                               WHEN OTHER
                                   ADD 1 TO WS-CI-NB-ANOMALIES
                                   DISPLAY "Anomalie : annulation ID "
                                       WS-TT-ID(WS-CI-I)
                           END-EVALUATE
                       END-IF

                   WHEN OTHER
                       ADD 1 TO WS-CI-NB-ANOMALIES
                       DISPLAY "Anomalie : type inconnu ID "
                           WS-TT-ID(WS-CI-I)
               END-EVALUATE
           END-PERFORM.

      *    Preparation de l'en-tete et ouverture de controle.txt
           MOVE SPACES TO WS-CI-DATE-FMT.
           STRING WS-TODAY(1:4) DELIMITED BY SIZE
               "-" DELIMITED BY SIZE
               WS-TODAY(5:2) DELIMITED BY SIZE
               "-" DELIMITED BY SIZE
               WS-TODAY(7:2) DELIMITED BY SIZE
               INTO WS-CI-DATE-FMT
           END-STRING.

           OPEN OUTPUT CONTROLE-FILE.

           MOVE SPACES TO WS-CI-LIGNE.
           STRING "=== Controle integrite des soldes - "
               DELIMITED BY SIZE
               WS-CI-DATE-FMT DELIMITED BY SIZE
               " ===" DELIMITED BY SIZE
               INTO WS-CI-LIGNE
           END-STRING.
           DISPLAY FUNCTION TRIM(WS-CI-LIGNE).
           MOVE WS-CI-LIGNE TO CONTROLE-LINE.
           WRITE CONTROLE-LINE.

           MOVE SPACES TO WS-CI-LIGNE.
           STRING "Compte    " DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "Solde fichier" DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "Solde calcule" DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "Ecart        " DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "Statut" DELIMITED BY SIZE
               INTO WS-CI-LIGNE
           END-STRING.
           DISPLAY FUNCTION TRIM(WS-CI-LIGNE).
           MOVE WS-CI-LIGNE TO CONTROLE-LINE.
           WRITE CONTROLE-LINE.

           MOVE ALL "-" TO WS-CI-LIGNE(1:67).
           MOVE SPACES TO WS-CI-LIGNE(68:33).
           DISPLAY WS-CI-LIGNE(1:67).
           MOVE WS-CI-LIGNE TO CONTROLE-LINE.
           WRITE CONTROLE-LINE.

      *    Rapport compte par compte
           MOVE ZEROS TO WS-CI-NB-OK.
           MOVE ZEROS TO WS-CI-NB-ECART.
           MOVE ZEROS TO WS-CI-TOTAL-ECARTS.

           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-COMPTES
               COMPUTE WS-CI-ECART =
                   WS-TC-SOLDE-FIC(WS-CI-I) - WS-TC-SOLDE-REC(WS-CI-I)
               IF WS-CI-ECART = 0
                   ADD 1 TO WS-CI-NB-OK
                   MOVE "OK" TO WS-CI-STATUT
               ELSE
                   ADD 1 TO WS-CI-NB-ECART
                   MOVE "ECART" TO WS-CI-STATUT
                   COMPUTE WS-CI-ECART-ABS = FUNCTION ABS(WS-CI-ECART)
                   ADD WS-CI-ECART-ABS TO WS-CI-TOTAL-ECARTS
               END-IF

               MOVE WS-TC-SOLDE-FIC(WS-CI-I) TO WS-CI-AFF-FIC
               MOVE WS-TC-SOLDE-REC(WS-CI-I) TO WS-CI-AFF-REC
               MOVE WS-CI-ECART TO WS-CI-AFF-ECART

               MOVE SPACES TO WS-CI-LIGNE
               STRING WS-TC-NUM(WS-CI-I) DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   WS-CI-AFF-FIC DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   WS-CI-AFF-REC DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   WS-CI-AFF-ECART DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   WS-CI-STATUT DELIMITED BY SIZE
                   INTO WS-CI-LIGNE
               END-STRING
               DISPLAY FUNCTION TRIM(WS-CI-LIGNE)
               MOVE WS-CI-LIGNE TO CONTROLE-LINE
               WRITE CONTROLE-LINE
           END-PERFORM.

      *    Totaux
           MOVE ALL "-" TO WS-CI-LIGNE(1:67).
           MOVE SPACES TO WS-CI-LIGNE(68:33).
           DISPLAY WS-CI-LIGNE(1:67).
           MOVE WS-CI-LIGNE TO CONTROLE-LINE.
           WRITE CONTROLE-LINE.

           MOVE WS-CI-TOTAL-ECARTS TO WS-CI-AFF-TOTAL.
           MOVE SPACES TO WS-CI-LIGNE.
           STRING "Comptes controles : " DELIMITED BY SIZE
               WS-CI-NB-COMPTES DELIMITED BY SIZE
               " | OK : " DELIMITED BY SIZE
               WS-CI-NB-OK DELIMITED BY SIZE
               " | ECART : " DELIMITED BY SIZE
               WS-CI-NB-ECART DELIMITED BY SIZE
               INTO WS-CI-LIGNE
           END-STRING.
           DISPLAY FUNCTION TRIM(WS-CI-LIGNE).
           MOVE WS-CI-LIGNE TO CONTROLE-LINE.
           WRITE CONTROLE-LINE.

           MOVE SPACES TO WS-CI-LIGNE.
           STRING "Total ecarts (abs) : " DELIMITED BY SIZE
               FUNCTION TRIM(WS-CI-AFF-TOTAL) DELIMITED BY SIZE
               " | Anomalies : " DELIMITED BY SIZE
               WS-CI-NB-ANOMALIES DELIMITED BY SIZE
               INTO WS-CI-LIGNE
           END-STRING.
           DISPLAY FUNCTION TRIM(WS-CI-LIGNE).
           MOVE WS-CI-LIGNE TO CONTROLE-LINE.
           WRITE CONTROLE-LINE.

           MOVE SPACES TO WS-CI-LIGNE.
           STRING "Les ECART peuvent venir de comptes crees avant "
               DELIMITED BY SIZE
               "l'ajout de la transaction Ouverture."
               DELIMITED BY SIZE
               INTO WS-CI-LIGNE
           END-STRING.
           DISPLAY FUNCTION TRIM(WS-CI-LIGNE).
           MOVE WS-CI-LIGNE TO CONTROLE-LINE.
           WRITE CONTROLE-LINE.

           CLOSE CONTROLE-FILE.

           DISPLAY "Rapport enregistre dans controle.txt".

      *    Journalisation
           MOVE "CONTROLE" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "OK=" DELIMITED BY SIZE
               WS-CI-NB-OK DELIMITED BY SIZE
               " ECART=" DELIMITED BY SIZE
               WS-CI-NB-ECART DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
