           DISPLAY "Emission d'une carte bancaire.".
           DISPLAY "Numero de compte : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.

      *    Verifier l'existence et le type du compte
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
               DISPLAY "Compte cloture : emission impossible."
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Epargne"
               DISPLAY "Carte interdite sur un compte Epargne."
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Plafond 7 jours (0 = 1500.00 par defaut) : "
               WITH NO ADVANCING.
           ACCEPT WS-CB-PLAFOND-SAISIE.
           IF WS-CB-PLAFOND-SAISIE = 0
               MOVE 1500.00 TO WS-CB-PLAFOND-SAISIE
           END-IF.

      *    Calculer le prochain numero sequentiel sur 7 chiffres
           MOVE ZEROS TO WS-CB-MAX-SEQ.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CARTE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ CARTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF CARTE-NUM(9:7) IS NUMERIC
                               MOVE CARTE-NUM(9:7) TO WS-CB-SEQ-LU
                               IF WS-CB-SEQ-LU > WS-CB-MAX-SEQ
                                   MOVE WS-CB-SEQ-LU
                                       TO WS-CB-MAX-SEQ
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CARTE-FILE
           END-IF.

           ADD 1 TO WS-CB-MAX-SEQ.

      *    Composer BIN + sequence + chiffre de Luhn
           MOVE SPACES TO WS-LUHN-NUMERO.
           STRING "49730130" DELIMITED BY SIZE
               WS-CB-MAX-SEQ DELIMITED BY SIZE
               "0" DELIMITED BY SIZE
               INTO WS-LUHN-NUMERO
           END-STRING.
           MOVE "G" TO WS-LUHN-MODE.
           MOVE 0 TO WS-LUHN-CHIFFRE.
           MOVE 1 TO WS-LUHN-RETOUR.
           CALL "LUHN" USING WS-LUHN.
           IF WS-LUHN-RETOUR NOT = 0
               DISPLAY "Erreur generation numero Luhn."
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-LUHN-CHIFFRE TO WS-LUHN-NUMERO(16:1).
           MOVE WS-LUHN-NUMERO TO WS-CB-CARTE-NUM.

      *    Expiration = mois du jour + 4 ans (AAAAMM)
           MOVE WS-TODAY(1:4) TO WS-CB-EXP-AA.
           ADD 4 TO WS-CB-EXP-AA.
           MOVE WS-TODAY(5:2) TO WS-CB-EXP-MM.

      *    Deriver un CVV deterministe a 3 chiffres
           MOVE WS-CB-CARTE-NUM(10:7) TO WS-CB-CVV-BASE.
           COMPUTE WS-CB-CVV-NUM =
               FUNCTION MOD(
                   WS-CB-CVV-BASE * 37 + WS-CB-EXP * 13 + 173,
                   900) + 100.

      *    Hacher le CVV (sel = 4 derniers chiffres du numero)
           MOVE SPACES TO WS-H-TEXTE.
           MOVE WS-CB-CVV-STR TO WS-H-TEXTE(1:3).
           MOVE WS-CB-CARTE-NUM(13:4) TO WS-CB-SEL-4.
           MOVE WS-CB-SEL-4 TO WS-H-SEL.
           MOVE ZEROS TO WS-H-RESULTAT.
           CALL "HACHAGE" USING WS-HACHAGE.

      *    Enregistrer la carte dans carte.dat
           MOVE "00" TO WS-STATUS.
           OPEN EXTEND CARTE-FILE.
           IF WS-STATUS NOT = "00" AND WS-STATUS NOT = "05"
               DISPLAY "Erreur ouverture carte.dat : " WS-STATUS
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-CB-CARTE-NUM      TO CARTE-NUM.
           MOVE WS-INPUT-COMPTE      TO CARTE-COMPTE.
           MOVE WS-CB-EXP            TO CARTE-EXPIRATION.
           MOVE WS-H-RESULTAT        TO CARTE-CVV-HASH.
           MOVE WS-CB-PLAFOND-SAISIE TO CARTE-PLAFOND.
           MOVE "A"                  TO CARTE-STATUT.
           MOVE 0                    TO CARTE-ESSAIS.
           WRITE CARTE-RECORD.
           CLOSE CARTE-FILE.

           MOVE WS-CB-PLAFOND-SAISIE TO WS-CB-PLAF-AFF.
           DISPLAY "Carte emise avec succes.".
           DISPLAY "Numero     : " WS-CB-CARTE-NUM.
           DISPLAY "Compte     : " WS-INPUT-COMPTE.
           DISPLAY "Expiration : " WS-CB-EXP.
           DISPLAY "CVV (affiche une seule fois) : " WS-CB-CVV-STR.
           DISPLAY "Plafond 7j : " FUNCTION TRIM(WS-CB-PLAF-AFF).

           MOVE ZEROS TO WS-CB-CVV-NUM.

           MOVE WS-CB-CARTE-NUM TO WS-CB-NUM-BRUT.
           PERFORM MASQUER-CARTE.
           MOVE "CARTE EMISE" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING WS-CB-NUM-MASQUE DELIMITED BY SIZE
               " Cpt=" DELIMITED BY SIZE
               WS-INPUT-COMPTE DELIMITED BY SIZE
               " Plaf=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-CB-PLAF-AFF) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
