           DISPLAY "=== Virement international ===".
           DISPLAY "Numero du compte source : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.

      *    Verifier compte source existant et non cloture
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
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Compte source non trouve."
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Compte cloture : operation impossible."
               EXIT PARAGRAPH
           END-IF.

      *    Verifier devise
           MOVE SPACES TO WS-DEV-CODE.
           DISPLAY "Code devise (ex: USD) : " WITH NO ADVANCING.
           ACCEPT WS-DEV-CODE.
           MOVE FUNCTION UPPER-CASE(WS-DEV-CODE) TO WS-DEV-CODE.

           MOVE "N" TO WS-FOUND.
           MOVE ZEROS TO WS-DEV-TAUX-LU.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT DEVISE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ DEVISE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF DEV-CODE = WS-DEV-CODE
                               MOVE DEV-TAUX TO WS-DEV-TAUX-LU
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DEVISE-FILE
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Devise inconnue."
               EXIT PARAGRAPH
           END-IF.

      *    Montant en devise
           MOVE ZEROS TO WS-DEV-MONTANT.
           DISPLAY "Montant en " WS-DEV-CODE " : " WITH NO ADVANCING.
           ACCEPT WS-DEV-MONTANT.

           IF WS-DEV-MONTANT <= 0
               DISPLAY "Montant invalide."
               EXIT PARAGRAPH
           END-IF.

      *    IBAN beneficiaire (validation etrangere mode E)
           MOVE SPACES TO WS-VI-IBAN-SAISIE.
           DISPLAY "IBAN beneficiaire (sans espaces) : "
               WITH NO ADVANCING.
           ACCEPT WS-VI-IBAN-SAISIE.
           MOVE FUNCTION UPPER-CASE(WS-VI-IBAN-SAISIE)
               TO WS-VI-IBAN-SAISIE.

           IF WS-VI-IBAN-SAISIE(35:6) NOT = SPACES
               DISPLAY "IBAN invalide."
               EXIT PARAGRAPH
           END-IF.

           MOVE "E" TO WS-IBAN-MODE.
           MOVE ZEROS TO WS-IBAN-COMPTE.
           MOVE WS-VI-IBAN-SAISIE(1:34) TO WS-IBAN-TEXTE.
           MOVE SPACES TO WS-IBAN-AFFICHE.
           MOVE ZERO TO WS-IBAN-RETOUR.
           CALL "CALCUL-IBAN" USING WS-CALCUL-IBAN.

           IF WS-IBAN-RETOUR NOT = 0
               DISPLAY "IBAN invalide."
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-VI-IBAN-SAISIE(1:34) TO WS-VI-IBAN.

      *    BIC beneficiaire (8 ou 11 caracteres)
           MOVE SPACES TO WS-VI-BIC-SAISIE.
           DISPLAY "BIC (8 ou 11 caracteres) : " WITH NO ADVANCING.
           ACCEPT WS-VI-BIC-SAISIE.
           MOVE FUNCTION UPPER-CASE(WS-VI-BIC-SAISIE)
               TO WS-VI-BIC-SAISIE.

           MOVE ZEROS TO WS-VI-BIC-LEN.
           PERFORM VARYING WS-VI-IDX FROM 20 BY -1
               UNTIL WS-VI-IDX < 1 OR WS-VI-BIC-LEN > 0
               IF WS-VI-BIC-SAISIE(WS-VI-IDX:1) NOT = SPACE
                   MOVE WS-VI-IDX TO WS-VI-BIC-LEN
               END-IF
           END-PERFORM.

           MOVE "O" TO WS-DEV-OK.
           IF WS-VI-BIC-LEN NOT = 8 AND WS-VI-BIC-LEN NOT = 11
               MOVE "N" TO WS-DEV-OK
           ELSE
               PERFORM VARYING WS-VI-IDX FROM 1 BY 1
                   UNTIL WS-VI-IDX > WS-VI-BIC-LEN
                   MOVE WS-VI-BIC-SAISIE(WS-VI-IDX:1) TO WS-VI-CAR
                   IF WS-VI-IDX <= 6
                       IF WS-VI-CAR < "A" OR WS-VI-CAR > "Z"
                           MOVE "N" TO WS-DEV-OK
                       END-IF
                   ELSE
                       IF NOT ((WS-VI-CAR >= "A"
                                AND WS-VI-CAR <= "Z")
                               OR (WS-VI-CAR >= "0"
                                AND WS-VI-CAR <= "9"))
                           MOVE "N" TO WS-DEV-OK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

           IF WS-DEV-OK = "N"
               DISPLAY "BIC invalide."
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-VI-BIC-SAISIE(1:11) TO WS-VI-BIC.

      *    Option de frais SHA ou OUR
           MOVE SPACES TO WS-VI-OPTION.
           DISPLAY "Option de frais (SHA ou OUR) : "
               WITH NO ADVANCING.
           ACCEPT WS-VI-OPTION.
           MOVE FUNCTION UPPER-CASE(WS-VI-OPTION) TO WS-VI-OPTION.

           IF WS-VI-OPTION = "SHA"
               MOVE 15.00 TO WS-VI-FRAIS
           ELSE
               IF WS-VI-OPTION = "OUR"
                   MOVE 25.00 TO WS-VI-FRAIS
               ELSE
                   DISPLAY "Option de frais invalide (SHA ou OUR)."
                   EXIT PARAGRAPH
               END-IF
           END-IF.

      *    Conversion en EUR (achat de devise)
           MOVE "A"              TO WS-CONV-MODE.
           MOVE WS-DEV-TAUX-LU   TO WS-CONV-TAUX-MOYEN.
           MOVE WS-DEV-MONTANT   TO WS-CONV-MONTANT-DEV.
           MOVE WS-MARGE-DEFAUT  TO WS-CONV-MARGE-PCT.
           MOVE ZEROS TO WS-CONV-TAUX-APPL
                         WS-CONV-MONTANT-EUR
                         WS-CONV-RETOUR.
           CALL "CONVERSION" USING WS-CONVERSION.

           IF WS-CONV-RETOUR NOT = 0
               DISPLAY "Erreur de conversion."
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-DEV-EUR-MOYEN ROUNDED =
               WS-DEV-MONTANT / WS-DEV-TAUX-LU.
           COMPUTE WS-DEV-DIFF-EUR =
               WS-CONV-MONTANT-EUR - WS-DEV-EUR-MOYEN.
           IF WS-DEV-DIFF-EUR < 0
               MOVE ZEROS TO WS-DEV-MARGE-EUR
           ELSE
               MOVE WS-DEV-DIFF-EUR TO WS-DEV-MARGE-EUR
           END-IF.

           COMPUTE WS-VI-TOTAL-DEBIT =
               WS-CONV-MONTANT-EUR + WS-VI-FRAIS.

      *    Debit du compte source
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O COMPTE-FILE.
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

           IF WS-FOUND = "N"
               DISPLAY "Compte source non trouve."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Compte cloture : operation impossible."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

           MOVE COMPTE-SOLDE TO WS-SOLDE-AVANT.

      *    Regle du decouvert autorise (pas de decouvert en epargne)
           IF COMPTE-TYPE = "Epargne"
               MOVE ZEROS TO WS-DEC-PLAFOND
           ELSE
               MOVE COMPTE-NUM TO WS-DEC-COMPTE
               PERFORM LIRE-DECOUVERT
           END-IF.
           COMPUTE WS-DEC-SOLDE-PROJ =
               COMPTE-SOLDE - WS-VI-TOTAL-DEBIT.
           COMPUTE WS-DEC-LIMITE = 0 - WS-DEC-PLAFOND.
           IF WS-DEC-SOLDE-PROJ < WS-DEC-LIMITE
               DISPLAY "Depassement du decouvert autorise."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

           SUBTRACT WS-VI-TOTAL-DEBIT FROM COMPTE-SOLDE.
           REWRITE COMPTE-RECORD.
           CLOSE COMPTE-FILE.

      *    Ecriture des transactions VirInter et Frais
           OPEN EXTEND TRANSACTION-FILE.
           MOVE WS-COUNTER          TO WS-TRANS-ID.
           MOVE WS-INPUT-COMPTE     TO WS-TRANS-COMPTE.
           MOVE WS-TODAY            TO WS-TRANS-DATE.
           MOVE "VirInter"          TO WS-TRANS-TYPE.
           MOVE WS-CONV-MONTANT-EUR TO WS-TRANS-MONTANT.
           MOVE ZERO                TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION      TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           ADD 1 TO WS-COUNTER.

           MOVE WS-COUNTER          TO WS-TRANS-ID.
           MOVE WS-INPUT-COMPTE     TO WS-TRANS-COMPTE.
           MOVE WS-TODAY            TO WS-TRANS-DATE.
           MOVE "FraisInter"        TO WS-TRANS-TYPE.
           MOVE WS-VI-FRAIS         TO WS-TRANS-MONTANT.
           MOVE ZERO                TO WS-TRANS-DEST.
           MOVE WS-TRANSACTION      TO TRANSACTION-RECORD.
           WRITE TRANSACTION-RECORD.
           CLOSE TRANSACTION-FILE.
           ADD 1 TO WS-COUNTER.

      *    Crediter la banque des frais et de la marge
           COMPUTE WS-VI-GAIN-BANQUE =
               WS-VI-FRAIS + WS-DEV-MARGE-EUR.
           MOVE "00" TO WS-STATUS.
           OPEN I-O BANQUE-FILE.
           IF WS-STATUS = "00"
               READ BANQUE-FILE
                   AT END
                       DISPLAY "Erreur : banque manquante."
                   NOT AT END
                       ADD WS-VI-GAIN-BANQUE TO BANQUE-SOLDE
                       REWRITE BANQUE-RECORD
               END-READ
               CLOSE BANQUE-FILE
           END-IF.

      *    Enregistrement dans virinter.dat
           MOVE ZEROS TO WS-MAX-VI-ID.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT VIRINTER-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ VIRINTER-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF VI-ID > WS-MAX-VI-ID
                               MOVE VI-ID TO WS-MAX-VI-ID
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE VIRINTER-FILE
           END-IF.

           ADD 1 TO WS-MAX-VI-ID.

           MOVE "00" TO WS-STATUS.
           OPEN EXTEND VIRINTER-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               MOVE WS-MAX-VI-ID        TO VI-ID
               MOVE WS-TODAY            TO VI-DATE
               MOVE WS-INPUT-COMPTE     TO VI-COMPTE
               MOVE WS-DEV-CODE         TO VI-DEVISE
               MOVE WS-DEV-MONTANT      TO VI-MONTANT-DEV
               MOVE WS-CONV-TAUX-APPL   TO VI-TAUX-APPL
               MOVE WS-CONV-MONTANT-EUR TO VI-MONTANT-EUR
               MOVE WS-VI-FRAIS         TO VI-FRAIS
               MOVE WS-VI-IBAN          TO VI-IBAN
               MOVE WS-VI-BIC           TO VI-BIC
               MOVE WS-VI-OPTION        TO VI-OPTION
               WRITE VIRINTER-RECORD
               CLOSE VIRINTER-FILE
           END-IF.

           DISPLAY "Virement international effectue (ID: "
               WS-MAX-VI-ID ").".
           MOVE WS-CONV-TAUX-APPL TO WS-DEV-TAUX-AFF.
           DISPLAY "Taux applique : " FUNCTION TRIM(WS-DEV-TAUX-AFF).
           MOVE WS-CONV-MONTANT-EUR TO WS-MONTANT-AFF.
           DISPLAY "Montant EUR   : " FUNCTION TRIM(WS-MONTANT-AFF)
               " EUR".
           MOVE WS-VI-FRAIS TO WS-MONTANT-AFF.
           DISPLAY "Frais (" WS-VI-OPTION ")   : "
               FUNCTION TRIM(WS-MONTANT-AFF) " EUR".
           MOVE WS-VI-TOTAL-DEBIT TO WS-MONTANT-AFF.
           DISPLAY "Total debite  : " FUNCTION TRIM(WS-MONTANT-AFF)
               " EUR".

      *    Journalisation VIR INTER
           MOVE WS-CONV-MONTANT-EUR TO WS-MONTANT-AFF.
           MOVE "VIR INTER" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               WS-INPUT-COMPTE DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-DEV-CODE DELIMITED BY SIZE
               " EUR=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(WS-VI-BIC) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.

      *    Controle fraude standard
           MOVE WS-CONV-MONTANT-EUR TO WS-INPUT-MONTANT.
           PERFORM CONTROLE-FRAUDE.

      *    Alerte supplementaire LCB-FT des 10000.00 EUR
           IF WS-CONV-MONTANT-EUR >= WS-SEUIL-LCB-FT
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
                                   MOVE ALERTE-ID TO WS-MAX-ALERTE-ID
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
                   MOVE "LCB-FT"            TO ALERTE-REGLE
                   MOVE WS-CONV-MONTANT-EUR TO ALERTE-MONTANT
                   MOVE "N"                 TO ALERTE-STATUT
                   WRITE ALERTE-RECORD
                   CLOSE ALERTE-FILE
               END-IF

               DISPLAY "Declaration de soupcon a examiner "
                   "(seuil LCB-FT)."

               MOVE WS-CONV-MONTANT-EUR TO WS-MONTANT-AFF
               MOVE "ALERTE" TO WS-JOURNAL-ACTION
               MOVE SPACES TO WS-JOURNAL-DETAIL
               STRING "LCB-FT Cpt=" DELIMITED BY SIZE
                   WS-INPUT-COMPTE DELIMITED BY SIZE
                   " Mnt=" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
               PERFORM ECRIRE-JOURNAL
           END-IF.
