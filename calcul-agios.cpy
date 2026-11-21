      *    Calcul des agios (methode des nombres debiteurs)
      *    Entrees : WS-AG-COMPTE, WS-AG-MOIS, WS-AG-DETAIL
      *    Sorties : WS-AG-NOMBRES, WS-AG-NB-JOURS-DEBIT, WS-AG-AGIOS,
      *              WS-AG-NB-DEPASSEMENTS, WS-AG-COMMISSION
           MOVE ZEROS TO WS-AG-NOMBRES
                         WS-AG-NB-JOURS-DEBIT
                         WS-AG-AGIOS
                         WS-AG-NB-DEPASSEMENTS
                         WS-AG-COMMISSION.
           MOVE "N" TO WS-AG-FOUND.
           MOVE "N" TO WS-AG-ERREUR.

           IF WS-AG-ANNEE < 1900 OR WS-AG-MM < 1 OR WS-AG-MM > 12
               MOVE "O" TO WS-AG-ERREUR
               EXIT PARAGRAPH
           END-IF.

      *    Chargement comptes et transactions en memoire
           PERFORM CHARGER-COMPTES-TRANS.
           IF WS-CI-ERREUR = "O"
               MOVE "O" TO WS-AG-ERREUR
               EXIT PARAGRAPH
           END-IF.

      *    Recherche de l'indice du compte dans WS-TAB-COMPTES
           MOVE ZEROS TO WS-AG-IDX-C.
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-COMPTES
                  OR WS-AG-FOUND = "Y"
               IF WS-TC-NUM(WS-CI-I) = WS-AG-COMPTE
                   MOVE WS-CI-I TO WS-AG-IDX-C
                   MOVE "Y" TO WS-AG-FOUND
               END-IF
           END-PERFORM.

           IF WS-AG-FOUND = "N"
               MOVE "O" TO WS-AG-ERREUR
               EXIT PARAGRAPH
           END-IF.

      *    Bornes de dates du mois (1er au dernier jour ou aujourd'hui)
           COMPUTE WS-AG-DATE-DEB =
               WS-AG-ANNEE * 10000 + WS-AG-MM * 100 + 1.
           COMPUTE WS-AG-INT-DEB =
               FUNCTION INTEGER-OF-DATE(WS-AG-DATE-DEB).

           IF WS-AG-MM = 12
               COMPUTE WS-AG-DATE-SUIV =
                   (WS-AG-ANNEE + 1) * 10000 + 101
           ELSE
               COMPUTE WS-AG-DATE-SUIV =
                   WS-AG-ANNEE * 10000
                   + (WS-AG-MM + 1) * 100 + 1
           END-IF.
           COMPUTE WS-AG-INT-FIN =
               FUNCTION INTEGER-OF-DATE(WS-AG-DATE-SUIV) - 1.

           IF WS-AG-MOIS = WS-TODAY(1:6)
               COMPUTE WS-AG-INT-FIN =
                   FUNCTION INTEGER-OF-DATE(WS-TODAY)
           END-IF.
           COMPUTE WS-AG-DATE-FIN =
               FUNCTION DATE-OF-INTEGER(WS-AG-INT-FIN).

      *    Effet net des transactions datees du mois ou apres
           MOVE ZEROS TO WS-CI-NB-ANOMALIES.
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               IF WS-TT-DATE(WS-CI-I) >= WS-AG-DATE-DEB
                   PERFORM REJOUER-TRANSACTION
               END-IF
           END-PERFORM.

      *    Solde de debut de mois = solde actuel - effet net
           COMPUTE WS-AG-SOLDE-DEB =
               WS-TC-SOLDE-FIC(WS-AG-IDX-C)
               - WS-TC-SOLDE-REC(WS-AG-IDX-C).

      *    Reinitialiser WS-TC-SOLDE-REC pour le parcours jour par jour
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-COMPTES
               MOVE ZEROS TO WS-TC-SOLDE-REC(WS-CI-I)
           END-PERFORM.
           MOVE WS-AG-SOLDE-DEB TO WS-TC-SOLDE-REC(WS-AG-IDX-C).

      *    Lire le plafond de decouvert autorise du compte
           MOVE WS-AG-COMPTE TO WS-DEC-COMPTE.
           PERFORM LIRE-DECOUVERT.
           COMPUTE WS-AG-LIMITE-DEC = 0 - WS-DEC-PLAFOND.

           IF WS-AG-DETAIL = "O" OR WS-AG-DETAIL = "o"
               MOVE SPACES TO WS-AG-LIGNE
               STRING "Jour      " DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   "Solde fin de journee" DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   "Nombres du jour" DELIMITED BY SIZE
                   INTO WS-AG-LIGNE
               END-STRING
               DISPLAY FUNCTION TRIM(WS-AG-LIGNE)
               MOVE ALL "-" TO WS-AG-LIGNE(1:51)
               MOVE SPACES TO WS-AG-LIGNE(52:29)
               DISPLAY WS-AG-LIGNE(1:51)
           END-IF.

      *    Parcours jour par jour du 1er a la fin de periode
           PERFORM VARYING WS-AG-INT-COUR
               FROM WS-AG-INT-DEB BY 1
               UNTIL WS-AG-INT-COUR > WS-AG-INT-FIN
               COMPUTE WS-AG-DATE-COUR =
                   FUNCTION DATE-OF-INTEGER(WS-AG-INT-COUR)

               PERFORM VARYING WS-CI-I FROM 1 BY 1
                   UNTIL WS-CI-I > WS-CI-NB-TRANS
                   IF WS-TT-DATE(WS-CI-I) = WS-AG-DATE-COUR
                       PERFORM REJOUER-TRANSACTION
                   END-IF
               END-PERFORM

               MOVE WS-TC-SOLDE-REC(WS-AG-IDX-C) TO WS-AG-SOLDE-JOUR
               IF WS-AG-SOLDE-JOUR < 0
                   ADD 1 TO WS-AG-NB-JOURS-DEBIT
                   COMPUTE WS-AG-NB-JOUR =
                       FUNCTION ABS(WS-AG-SOLDE-JOUR)
                   ADD WS-AG-NB-JOUR TO WS-AG-NOMBRES

                   IF WS-AG-SOLDE-JOUR < WS-AG-LIMITE-DEC
                       ADD 1 TO WS-AG-NB-DEPASSEMENTS
                   END-IF

                   IF WS-AG-DETAIL = "O" OR WS-AG-DETAIL = "o"
                       MOVE SPACES TO WS-AG-JOUR-FMT
                       STRING WS-AG-DATE-COUR(1:4) DELIMITED BY SIZE
                           "-" DELIMITED BY SIZE
                           WS-AG-DATE-COUR(5:2) DELIMITED BY SIZE
                           "-" DELIMITED BY SIZE
                           WS-AG-DATE-COUR(7:2) DELIMITED BY SIZE
                           INTO WS-AG-JOUR-FMT
                       END-STRING
                       MOVE WS-AG-SOLDE-JOUR TO WS-AG-AFF-SOLDE
                       MOVE WS-AG-NB-JOUR    TO WS-AG-AFF-NBJ
                       MOVE SPACES TO WS-AG-LIGNE
                       STRING WS-AG-JOUR-FMT DELIMITED BY SIZE
                           " | " DELIMITED BY SIZE
                           WS-AG-AFF-SOLDE DELIMITED BY SIZE
                           " | " DELIMITED BY SIZE
                           WS-AG-AFF-NBJ DELIMITED BY SIZE
                           INTO WS-AG-LIGNE
                       END-STRING
                       DISPLAY FUNCTION TRIM(WS-AG-LIGNE)
                   END-IF
               END-IF
           END-PERFORM.

           IF WS-AG-DETAIL = "O" OR WS-AG-DETAIL = "o"
               IF WS-AG-NB-JOURS-DEBIT = 0
                   DISPLAY "Aucun jour debiteur sur ce mois."
               END-IF
               MOVE ALL "-" TO WS-AG-LIGNE(1:51)
               MOVE SPACES TO WS-AG-LIGNE(52:29)
               DISPLAY WS-AG-LIGNE(1:51)
           END-IF.

      *    Agios = nombres * taux annuel / 36500 (arrondi au centime)
           COMPUTE WS-AG-AGIOS ROUNDED =
               WS-AG-NOMBRES * WS-TAUX-AGIOS / 36500.

      *    Commission d'intervention : 8.00 / jour, plafond 80.00 / mois
           COMPUTE WS-AG-COMMISSION =
               WS-AG-NB-DEPASSEMENTS * WS-AG-COMM-JOUR.
           IF WS-AG-COMMISSION > WS-AG-COMM-MAX
               MOVE WS-AG-COMM-MAX TO WS-AG-COMMISSION
           END-IF.
