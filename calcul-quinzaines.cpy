      *    Calcul des interets Livret A par quinzaines
      *    Entrees : WS-LA-COMPTE, WS-LA-ANNEE, WS-LA-DETAIL
      *    Sorties : WS-LA-FOUND, WS-LA-ERREUR, WS-LA-SOLDE-DEB,
      *              WS-LA-Q-MAX, WS-LA-SOMME-SOLDES, WS-LA-INTERETS
           MOVE ZEROS TO WS-LA-SOLDE-DEB
                         WS-LA-SOMME-SOLDES
                         WS-LA-INTERETS
                         WS-LA-Q-MAX.
           MOVE "N" TO WS-LA-FOUND.
           MOVE "N" TO WS-LA-ERREUR.

           MOVE WS-TODAY(1:4) TO WS-LA-ANNEE-COUR.
           IF WS-LA-ANNEE < 1900 OR WS-LA-ANNEE > WS-LA-ANNEE-COUR
               MOVE "O" TO WS-LA-ERREUR
               EXIT PARAGRAPH
           END-IF.

      *    Chargement comptes et transactions en memoire
           PERFORM CHARGER-COMPTES-TRANS.
           IF WS-CI-ERREUR = "O"
               MOVE "O" TO WS-LA-ERREUR
               EXIT PARAGRAPH
           END-IF.

      *    Recherche de l'indice du compte dans WS-TAB-COMPTES
           MOVE ZEROS TO WS-LA-IDX-C.
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-COMPTES
                  OR WS-LA-FOUND = "Y"
               IF WS-TC-NUM(WS-CI-I) = WS-LA-COMPTE
                   MOVE WS-CI-I TO WS-LA-IDX-C
                   MOVE "Y" TO WS-LA-FOUND
               END-IF
           END-PERFORM.

           IF WS-LA-FOUND = "N"
               MOVE "O" TO WS-LA-ERREUR
               EXIT PARAGRAPH
           END-IF.

      *    Nombre de quinzaines a calculer (24 ou jusqu'a aujourd'hui)
           IF WS-LA-ANNEE = WS-LA-ANNEE-COUR
               MOVE WS-TODAY(5:2) TO WS-LA-MM
               MOVE WS-TODAY(7:2) TO WS-LA-JJ
               IF WS-LA-JJ <= 15
                   COMPUTE WS-LA-Q-MAX = (WS-LA-MM - 1) * 2 + 1
               ELSE
                   COMPUTE WS-LA-Q-MAX = (WS-LA-MM - 1) * 2 + 2
               END-IF
           ELSE
               MOVE 24 TO WS-LA-Q-MAX
           END-IF.

      *    Effet net des transactions datees de l'annee ou apres
           COMPUTE WS-LA-DATE-DEB = WS-LA-ANNEE * 10000 + 101.
           MOVE ZEROS TO WS-CI-NB-ANOMALIES.
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               IF WS-TT-DATE(WS-CI-I) >= WS-LA-DATE-DEB
                   PERFORM REJOUER-TRANSACTION
               END-IF
           END-PERFORM.

      *    Solde de debut d'annee = solde actuel - effet net
           COMPUTE WS-LA-SOLDE-DEB =
               WS-TC-SOLDE-FIC(WS-LA-IDX-C)
               - WS-TC-SOLDE-REC(WS-LA-IDX-C).

      *    Initialiser les variations par quinzaine
           PERFORM VARYING WS-LA-Q FROM 1 BY 1
               UNTIL WS-LA-Q > 24
               MOVE ZEROS TO WS-LA-DELTA(WS-LA-Q)
           END-PERFORM.

      *    Appliquer la regle des dates de valeur sur l'annee
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               IF WS-TT-ANNEE(WS-CI-I) = WS-LA-ANNEE
                   DIVIDE WS-TT-MMJJ(WS-CI-I) BY 100
                       GIVING WS-LA-MM REMAINDER WS-LA-JJ
                   IF WS-LA-MM >= 1 AND WS-LA-MM <= 12
                       IF WS-LA-JJ <= 15
                           COMPUTE WS-LA-Q-TR =
                               (WS-LA-MM - 1) * 2 + 1
                       ELSE
                           COMPUTE WS-LA-Q-TR =
                               (WS-LA-MM - 1) * 2 + 2
                       END-IF
                       COMPUTE WS-LA-Q-EFF = WS-LA-Q-TR + 1

                       EVALUATE WS-TT-TYPE(WS-CI-I)
                         WHEN "Ouverture"
                         WHEN "Depot"
                         WHEN "Interet"
                         WHEN "IntLivret"
                         WHEN "Credit"
                         WHEN "Report"
                           IF WS-TT-COMPTE(WS-CI-I) = WS-LA-COMPTE
                              AND WS-LA-Q-EFF <= 24
                               ADD WS-TT-MONTANT(WS-CI-I)
                                   TO WS-LA-DELTA(WS-LA-Q-EFF)
                           END-IF

                         WHEN "Retrait"
                         WHEN "Frais"
                         WHEN "FraisInter"
                         WHEN "Rembourse"
                         WHEN "Cloture"
                         WHEN "Agios"
                         WHEN "Commission"
                         WHEN "VirInter"
                         WHEN "ReportDeb"
                           IF WS-TT-COMPTE(WS-CI-I) = WS-LA-COMPTE
                               SUBTRACT WS-TT-MONTANT(WS-CI-I)
                                   FROM WS-LA-DELTA(WS-LA-Q-TR)
                           END-IF

                         WHEN "Virement"
                           IF WS-TT-COMPTE(WS-CI-I) = WS-LA-COMPTE
                               SUBTRACT WS-TT-MONTANT(WS-CI-I)
                                   FROM WS-LA-DELTA(WS-LA-Q-TR)
                           END-IF
                           IF WS-TT-DEST(WS-CI-I) = WS-LA-COMPTE
                              AND WS-LA-Q-EFF <= 24
                               ADD WS-TT-MONTANT(WS-CI-I)
                                   TO WS-LA-DELTA(WS-LA-Q-EFF)
                           END-IF

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
                           IF WS-FOUND = "Y"
                             EVALUATE WS-CI-CIBLE-TYPE
                               WHEN "Depot"
                                 IF WS-CI-CIBLE-COMPTE = WS-LA-COMPTE
                                   SUBTRACT WS-CI-CIBLE-MONTANT
                                     FROM WS-LA-DELTA(WS-LA-Q-TR)
                                 END-IF
                               WHEN "Retrait"
                                 IF WS-CI-CIBLE-COMPTE = WS-LA-COMPTE
                                    AND WS-LA-Q-EFF <= 24
                                   ADD WS-CI-CIBLE-MONTANT
                                     TO WS-LA-DELTA(WS-LA-Q-EFF)
                                 END-IF
                               WHEN "Virement"
                                 IF WS-CI-CIBLE-COMPTE = WS-LA-COMPTE
                                    AND WS-LA-Q-EFF <= 24
                                   ADD WS-CI-CIBLE-MONTANT
                                     TO WS-LA-DELTA(WS-LA-Q-EFF)
                                 END-IF
                                 IF WS-CI-CIBLE-DEST = WS-LA-COMPTE
                                   SUBTRACT WS-CI-CIBLE-MONTANT
                                     FROM WS-LA-DELTA(WS-LA-Q-TR)
                                 END-IF
                             END-EVALUATE
                           END-IF
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

      *    Affichage en-tete en mode detail
           IF WS-LA-DETAIL = "O" OR WS-LA-DETAIL = "o"
               MOVE SPACES TO WS-LA-LIGNE
               STRING "Quinzaine" DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   "Solde de valeur" DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   "Interet quinzaine" DELIMITED BY SIZE
                   INTO WS-LA-LIGNE
               END-STRING
               DISPLAY FUNCTION TRIM(WS-LA-LIGNE)
               MOVE ALL "-" TO WS-LA-LIGNE(1:47)
               MOVE SPACES TO WS-LA-LIGNE(48:33)
               DISPLAY WS-LA-LIGNE(1:47)
           END-IF.

      *    Parcours des quinzaines de 1 a WS-LA-Q-MAX
           MOVE WS-LA-SOLDE-DEB TO WS-LA-SOLDE-Q.
           PERFORM VARYING WS-LA-Q FROM 1 BY 1
               UNTIL WS-LA-Q > WS-LA-Q-MAX
               ADD WS-LA-DELTA(WS-LA-Q) TO WS-LA-SOLDE-Q

               IF WS-LA-SOLDE-Q > 0
                   ADD WS-LA-SOLDE-Q TO WS-LA-SOMME-SOLDES
                   COMPUTE WS-LA-INT-Q ROUNDED =
                       WS-LA-SOLDE-Q * WS-TAUX-LIVRETA / 2400
               ELSE
                   MOVE ZEROS TO WS-LA-INT-Q
               END-IF

               IF WS-LA-DETAIL = "O" OR WS-LA-DETAIL = "o"
                   DIVIDE WS-LA-Q BY 2
                       GIVING WS-LA-MM REMAINDER WS-LA-NUM-Q
                   IF WS-LA-NUM-Q = 1
                       ADD 1 TO WS-LA-MM
                   ELSE
                       MOVE 2 TO WS-LA-NUM-Q
                   END-IF
                   MOVE SPACES TO WS-LA-Q-FMT
                   STRING WS-LA-ANNEE DELIMITED BY SIZE
                       "-" DELIMITED BY SIZE
                       WS-LA-MM DELIMITED BY SIZE
                       "-" DELIMITED BY SIZE
                       WS-LA-NUM-Q DELIMITED BY SIZE
                       INTO WS-LA-Q-FMT
                   END-STRING
                   MOVE WS-LA-SOLDE-Q TO WS-LA-AFF-SOLDE
                   MOVE WS-LA-INT-Q   TO WS-LA-AFF-INT
                   MOVE SPACES TO WS-LA-LIGNE
                   STRING WS-LA-Q-FMT DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       WS-LA-AFF-SOLDE DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       WS-LA-AFF-INT DELIMITED BY SIZE
                       INTO WS-LA-LIGNE
                   END-STRING
                   DISPLAY FUNCTION TRIM(WS-LA-LIGNE)
               END-IF
           END-PERFORM.

           IF WS-LA-DETAIL = "O" OR WS-LA-DETAIL = "o"
               MOVE ALL "-" TO WS-LA-LIGNE(1:47)
               MOVE SPACES TO WS-LA-LIGNE(48:33)
               DISPLAY WS-LA-LIGNE(1:47)
           END-IF.

      *    Interet total arrondi au centime
           COMPUTE WS-LA-INTERETS ROUNDED =
               WS-LA-SOMME-SOLDES * WS-TAUX-LIVRETA / 2400.
