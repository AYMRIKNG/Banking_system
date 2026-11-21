           DISPLAY "Cartes d'un client.".
           DISPLAY "ID client : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-ID.

      *    Charger les comptes du client
           MOVE ZEROS TO WS-CB-NB-COMPTES.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-CLIENT-ID = WS-INPUT-ID
                              AND WS-CB-NB-COMPTES < 50
                               ADD 1 TO WS-CB-NB-COMPTES
                               MOVE COMPTE-NUM
                                   TO WS-CB-CPT(WS-CB-NB-COMPTES)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
           END-IF.

           IF WS-CB-NB-COMPTES = 0
               DISPLAY "Aucun compte trouve pour ce client."
               EXIT PARAGRAPH
           END-IF.

      *    Charger les cartes rattachees aux comptes du client
           MOVE ZEROS TO WS-CB-NB-CARTES.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CARTE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ CARTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           MOVE "N" TO WS-CB-MATCH
                           PERFORM VARYING WS-CB-I FROM 1 BY 1
                               UNTIL WS-CB-I > WS-CB-NB-COMPTES
                                  OR WS-CB-MATCH = "Y"
                               IF CARTE-COMPTE
                                  = WS-CB-CPT(WS-CB-I)
                                   MOVE "Y" TO WS-CB-MATCH
                               END-IF
                           END-PERFORM
                           IF WS-CB-MATCH = "Y"
                              AND WS-CB-NB-CARTES < 50
                               ADD 1 TO WS-CB-NB-CARTES
                               MOVE CARTE-NUM
                                   TO WS-CB-TC-NUM(WS-CB-NB-CARTES)
                               MOVE CARTE-COMPTE
                                   TO WS-CB-TC-COMPTE(WS-CB-NB-CARTES)
                               MOVE CARTE-EXPIRATION
                                   TO WS-CB-TC-EXP(WS-CB-NB-CARTES)
                               MOVE CARTE-PLAFOND
                                   TO WS-CB-TC-PLAFOND(WS-CB-NB-CARTES)
                               MOVE CARTE-STATUT
                                   TO WS-CB-TC-STATUT(WS-CB-NB-CARTES)
                               MOVE ZEROS
                                   TO WS-CB-TC-CONSO(WS-CB-NB-CARTES)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CARTE-FILE
           END-IF.

           IF WS-CB-NB-CARTES = 0
               DISPLAY "Aucune carte trouvee pour ce client."
               EXIT PARAGRAPH
           END-IF.

      *    Calculer la consommation des 7 derniers jours par carte
           COMPUTE WS-CB-JOUR-AUJ =
               FUNCTION INTEGER-OF-DATE(WS-TODAY).
           IF WS-CB-JOUR-AUJ > 6
               COMPUTE WS-CB-JOUR-LIM = WS-CB-JOUR-AUJ - 6
           ELSE
               MOVE ZERO TO WS-CB-JOUR-LIM
           END-IF.

           MOVE "00" TO WS-STATUS.
           OPEN INPUT PAIEMENT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ PAIEMENT-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF PAI-RESULTAT = "OK"
                              AND PAI-DATE > 16010000
                               COMPUTE WS-CB-JOUR-PAI =
                                   FUNCTION INTEGER-OF-DATE(PAI-DATE)
                               IF WS-CB-JOUR-PAI >= WS-CB-JOUR-LIM
                                  AND WS-CB-JOUR-PAI <= WS-CB-JOUR-AUJ
                                   PERFORM VARYING WS-CB-J FROM 1 BY 1
                                       UNTIL WS-CB-J > WS-CB-NB-CARTES
                                       IF PAI-CARTE-NUM
                                          = WS-CB-TC-NUM(WS-CB-J)
                                           ADD PAI-MONTANT
                                               TO WS-CB-TC-CONSO(
                                                   WS-CB-J)
                                       END-IF
                                   END-PERFORM
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE PAIEMENT-FILE
           END-IF.

      *    Afficher la liste masquee des cartes
           PERFORM VARYING WS-CB-J FROM 1 BY 1
               UNTIL WS-CB-J > WS-CB-NB-CARTES
               MOVE WS-CB-TC-NUM(WS-CB-J) TO WS-CB-NUM-BRUT
               PERFORM MASQUER-CARTE
               IF WS-CB-TC-STATUT(WS-CB-J) = "A"
                   MOVE "Active" TO WS-CB-STATUT-LIB
               ELSE
                   MOVE "Opposee" TO WS-CB-STATUT-LIB
               END-IF
               MOVE WS-CB-TC-PLAFOND(WS-CB-J) TO WS-CB-PLAF-AFF
               MOVE WS-CB-TC-CONSO(WS-CB-J)   TO WS-CB-CONSO-AFF
               DISPLAY "Carte      : " WS-CB-NUM-MASQUE
               DISPLAY "Compte     : " WS-CB-TC-COMPTE(WS-CB-J)
               DISPLAY "Expiration : " WS-CB-TC-EXP(WS-CB-J)
               DISPLAY "Statut     : " FUNCTION TRIM(WS-CB-STATUT-LIB)
               DISPLAY "Plafond 7j : " FUNCTION TRIM(WS-CB-PLAF-AFF)
               DISPLAY "Conso 7j   : "
                   FUNCTION TRIM(WS-CB-CONSO-AFF) " / "
                   FUNCTION TRIM(WS-CB-PLAF-AFF)
               DISPLAY "-----------------------------"
           END-PERFORM.
