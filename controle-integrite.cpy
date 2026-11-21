      *    Chargement des comptes et transactions en memoire
           PERFORM CHARGER-COMPTES-TRANS.
           IF WS-CI-ERREUR = "O"
               EXIT PARAGRAPH
           END-IF.

      *    Rejeu des transactions dans l'ordre du fichier
           MOVE ZEROS TO WS-CI-NB-ANOMALIES.
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               PERFORM REJOUER-TRANSACTION
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
