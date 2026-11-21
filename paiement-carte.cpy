           DISPLAY "Paiement par carte bancaire.".
           MOVE SPACES TO WS-CB-SAISIE-NUM
                          WS-CB-SAISIE-EXP-X
                          WS-CB-SAISIE-CVV
                          WS-CB-COMMERCANT.
           DISPLAY "Numero de carte (16 chiffres) : "
               WITH NO ADVANCING.
           ACCEPT WS-CB-SAISIE-NUM.
           DISPLAY "Expiration (AAAAMM) : " WITH NO ADVANCING.
           ACCEPT WS-CB-SAISIE-EXP-X.
           DISPLAY "CVV : " WITH NO ADVANCING.
           ACCEPT WS-CB-SAISIE-CVV.
           DISPLAY "Nom du commercant : " WITH NO ADVANCING.
           ACCEPT WS-CB-COMMERCANT.
           DISPLAY "Montant : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-MONTANT.

           IF WS-INPUT-MONTANT = 0
               DISPLAY "Montant invalide."
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CB-SAISIE-NUM(1:16) TO WS-CB-CARTE-NUM.
           MOVE WS-CB-CARTE-NUM TO WS-CB-NUM-BRUT.

      *    C01 : controle de Luhn
           MOVE "V" TO WS-LUHN-MODE.
           MOVE WS-CB-CARTE-NUM TO WS-LUHN-NUMERO.
           MOVE 0 TO WS-LUHN-CHIFFRE.
           MOVE 1 TO WS-LUHN-RETOUR.
           CALL "LUHN" USING WS-LUHN.
           IF WS-CB-SAISIE-NUM(17:4) NOT = SPACES
              OR WS-LUHN-RETOUR NOT = 0
               DISPLAY "Paiement refuse (C01) : numero invalide."
               MOVE "C01" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.

      *    C02 : carte existante dans carte.dat
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CARTE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ CARTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF CARTE-NUM = WS-CB-CARTE-NUM
                               MOVE CARTE-COMPTE
                                   TO WS-CB-CARTE-COMPTE
                               MOVE CARTE-EXPIRATION
                                   TO WS-CB-CARTE-EXP
                               MOVE CARTE-CVV-HASH
                                   TO WS-CB-CARTE-HASH
                               MOVE CARTE-PLAFOND
                                   TO WS-CB-CARTE-PLAFOND
                               MOVE CARTE-STATUT
                                   TO WS-CB-CARTE-STATUT
                               MOVE CARTE-ESSAIS
                                   TO WS-CB-CARTE-ESSAIS
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CARTE-FILE
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Paiement refuse (C02) : carte inconnue."
               MOVE "C02" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.

      *    C03 : carte en opposition
           IF WS-CB-CARTE-STATUT NOT = "A"
               DISPLAY "Paiement refuse (C03) : carte en opposition."
               MOVE "C03" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.

      *    C04 : carte expiree ou date d'expiration differente
           MOVE WS-TODAY(1:6) TO WS-CB-MOIS-COUR.
           IF WS-CB-SAISIE-EXP-X(1:6) NOT NUMERIC
              OR WS-CB-SAISIE-EXP-X(7:4) NOT = SPACES
               DISPLAY "Paiement refuse (C04) : "
                   "expiration incorrecte."
               MOVE "C04" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-CB-SAISIE-EXP-X(1:6) TO WS-CB-SAISIE-EXP.
           IF WS-CB-SAISIE-EXP NOT = WS-CB-CARTE-EXP
              OR WS-CB-CARTE-EXP < WS-CB-MOIS-COUR
               DISPLAY "Paiement refuse (C04) : "
                   "carte expiree ou date incorrecte."
               MOVE "C04" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.

      *    C05 : verification du hash CVV
           MOVE "N" TO WS-CB-CVV-OK.
           IF WS-CB-SAISIE-CVV(1:3) IS NUMERIC
              AND WS-CB-SAISIE-CVV(4:17) = SPACES
               MOVE SPACES TO WS-H-TEXTE
               MOVE WS-CB-SAISIE-CVV(1:3) TO WS-H-TEXTE(1:3)
               MOVE WS-CB-CARTE-NUM(13:4) TO WS-CB-SEL-4
               MOVE WS-CB-SEL-4 TO WS-H-SEL
               MOVE ZEROS TO WS-H-RESULTAT
               CALL "HACHAGE" USING WS-HACHAGE
               IF WS-H-RESULTAT = WS-CB-CARTE-HASH
                   MOVE "O" TO WS-CB-CVV-OK
               END-IF
           END-IF.
           MOVE SPACES TO WS-CB-SAISIE-CVV.

           IF WS-CB-CVV-OK = "N"
               MOVE "N" TO WS-CB-AUTO-OPP
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN I-O CARTE-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                       READ CARTE-FILE
                           AT END
                               MOVE "10" TO WS-STATUS
                           NOT AT END
                               IF CARTE-NUM = WS-CB-CARTE-NUM
                                   ADD 1 TO CARTE-ESSAIS
                                   IF CARTE-ESSAIS >= 3
                                       MOVE "O" TO CARTE-STATUT
                                       MOVE "O" TO WS-CB-AUTO-OPP
                                   END-IF
                                   REWRITE CARTE-RECORD
                                   MOVE "Y" TO WS-FOUND
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE CARTE-FILE
               END-IF
               DISPLAY "Paiement refuse (C05) : CVV incorrect."
               IF WS-CB-AUTO-OPP = "O"
                   DISPLAY "3 CVV faux consecutifs : "
                       "carte mise en opposition."
                   PERFORM MASQUER-CARTE
                   MOVE "OPPOSITION" TO WS-JOURNAL-ACTION
                   MOVE SPACES TO WS-JOURNAL-DETAIL
                   STRING WS-CB-NUM-MASQUE DELIMITED BY SIZE
                       " Auto 3 CVV faux" DELIMITED BY SIZE
                       INTO WS-JOURNAL-DETAIL
                   END-STRING
                   PERFORM ECRIRE-JOURNAL
               END-IF
               MOVE "C05" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.

      *    CVV correct : remettre le compteur d'essais a 0 si > 0
           IF WS-CB-CARTE-ESSAIS > 0
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN I-O CARTE-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                       READ CARTE-FILE
                           AT END
                               MOVE "10" TO WS-STATUS
                           NOT AT END
                               IF CARTE-NUM = WS-CB-CARTE-NUM
                                   MOVE 0 TO CARTE-ESSAIS
                                   REWRITE CARTE-RECORD
                                   MOVE "Y" TO WS-FOUND
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE CARTE-FILE
               END-IF
           END-IF.

      *    C06 : plafond sur 7 jours glissants
           COMPUTE WS-CB-JOUR-AUJ =
               FUNCTION INTEGER-OF-DATE(WS-TODAY).
           IF WS-CB-JOUR-AUJ > 6
               COMPUTE WS-CB-JOUR-LIM = WS-CB-JOUR-AUJ - 6
           ELSE
               MOVE ZERO TO WS-CB-JOUR-LIM
           END-IF.

           MOVE ZEROS TO WS-CB-CONSO-7J.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT PAIEMENT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ PAIEMENT-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF PAI-CARTE-NUM = WS-CB-CARTE-NUM
                              AND PAI-RESULTAT = "OK"
                              AND PAI-DATE > 16010000
                               COMPUTE WS-CB-JOUR-PAI =
                                   FUNCTION INTEGER-OF-DATE(PAI-DATE)
                               IF WS-CB-JOUR-PAI >= WS-CB-JOUR-LIM
                                  AND WS-CB-JOUR-PAI <= WS-CB-JOUR-AUJ
                                   ADD PAI-MONTANT TO WS-CB-CONSO-7J
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE PAIEMENT-FILE
           END-IF.

           COMPUTE WS-CB-TOTAL-7J = WS-CB-CONSO-7J + WS-INPUT-MONTANT.
           IF WS-CB-TOTAL-7J > WS-CB-CARTE-PLAFOND
               DISPLAY "Paiement refuse (C06) : "
                   "plafond 7 jours depasse."
               MOVE "C06" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.

      *    C07 : controle du decouvert autorise et debit du compte
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = WS-CB-CARTE-COMPTE
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N" OR COMPTE-TYPE = "Cloture"
               CLOSE COMPTE-FILE
               DISPLAY "Paiement refuse (C07) : compte indisponible."
               MOVE "C07" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Epargne"
               MOVE ZEROS TO WS-DEC-PLAFOND
           ELSE
               MOVE COMPTE-NUM TO WS-DEC-COMPTE
               PERFORM LIRE-DECOUVERT
           END-IF.
           COMPUTE WS-DEC-SOLDE-PROJ =
               COMPTE-SOLDE - WS-INPUT-MONTANT.
           COMPUTE WS-DEC-LIMITE = 0 - WS-DEC-PLAFOND.
           IF WS-DEC-SOLDE-PROJ < WS-DEC-LIMITE
               CLOSE COMPTE-FILE
               DISPLAY "Paiement refuse (C07) : "
                   "depassement du decouvert autorise."
               MOVE "C07" TO WS-CB-CODE-RES
               PERFORM ENREGISTRER-PAIEMENT
               EXIT PARAGRAPH
           END-IF.

      *    Paiement accepte : debit du compte
           MOVE COMPTE-SOLDE TO WS-SOLDE-AVANT.
           MOVE COMPTE-NUM TO WS-INPUT-COMPTE.
           SUBTRACT WS-INPUT-MONTANT FROM COMPTE-SOLDE.
           REWRITE COMPTE-RECORD.
           MOVE COMPTE-SOLDE TO WS-SOLDE-AFF.
           CLOSE COMPTE-FILE.

      *    Transaction de type CB
           OPEN EXTEND TRANSACTION-FILE.
           MOVE WS-COUNTER        TO WS-TRANS-ID.
           MOVE WS-INPUT-COMPTE   TO WS-TRANS-COMPTE.
           MOVE WS-TODAY          TO WS-TRANS-DATE.
           MOVE "CB"              TO WS-TRANS-TYPE.
           MOVE WS-INPUT-MONTANT  TO WS-TRANS-MONTANT.
           MOVE ZERO              TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION    TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           CLOSE TRANSACTION-FILE.
           ADD 1 TO WS-COUNTER.

      *    Enregistrement paiement.dat + journal PAIEMENT CB
           MOVE "OK" TO WS-CB-CODE-RES.
           PERFORM ENREGISTRER-PAIEMENT.

           MOVE WS-INPUT-MONTANT TO WS-MONTANT-AFF.
           PERFORM MASQUER-CARTE.
           DISPLAY "Paiement accepte (OK).".
           DISPLAY "Carte      : " WS-CB-NUM-MASQUE.
           DISPLAY "Commercant : " FUNCTION TRIM(WS-CB-COMMERCANT).
           DISPLAY "Montant    : " FUNCTION TRIM(WS-MONTANT-AFF).
           DISPLAY "Solde      : " FUNCTION TRIM(WS-SOLDE-AFF).

      *    Controle fraude apres paiement accepte
           PERFORM CONTROLE-FRAUDE.
