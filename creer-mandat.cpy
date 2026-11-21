      *    Creation d'un mandat de prelevement SEPA (SDD)
           DISPLAY "=== Creer un mandat SEPA ===".
           MOVE ZEROS TO WS-MD-COMPTE WS-MD-MAX.
           MOVE SPACES TO WS-MD-RUM
                          WS-MD-ICS-SAISIE
                          WS-MD-ICS
                          WS-MD-CREANCIER
                          WS-MD-TYPE.

           DISPLAY "Numero de compte : " WITH NO ADVANCING.
           ACCEPT WS-MD-COMPTE.

      *    Verifier compte existant et non cloture
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-MD-COMPTE
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Compte non trouve."
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Compte cloture : operation impossible."
               EXIT PARAGRAPH
           END-IF.

      *    Saisie et unicite du RUM
           DISPLAY "RUM (20 car. max) : " WITH NO ADVANCING.
           ACCEPT WS-MD-RUM.
           MOVE FUNCTION UPPER-CASE(WS-MD-RUM) TO WS-MD-RUM.

           IF WS-MD-RUM = SPACES
               DISPLAY "RUM obligatoire."
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT MANDAT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ MANDAT-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF MANDAT-RUM = WS-MD-RUM
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE MANDAT-FILE
           END-IF.

           IF WS-FOUND = "Y"
               DISPLAY "Ce RUM existe deja."
               EXIT PARAGRAPH
           END-IF.

      *    Saisie et controle du format ICS (FR + 2 ch + ZZZ + 6 alnum)
           DISPLAY "ICS (ex: FR12ZZZ123456) : " WITH NO ADVANCING.
           ACCEPT WS-MD-ICS-SAISIE.
           MOVE FUNCTION UPPER-CASE(WS-MD-ICS-SAISIE)
               TO WS-MD-ICS-SAISIE.

           MOVE "O" TO WS-MD-OK.
           IF WS-MD-ICS-SAISIE(14:7) NOT = SPACES
               MOVE "N" TO WS-MD-OK
           END-IF.
           IF WS-MD-ICS-SAISIE(1:2) NOT = "FR"
               MOVE "N" TO WS-MD-OK
           END-IF.
           IF WS-MD-ICS-SAISIE(3:1) < "0"
              OR WS-MD-ICS-SAISIE(3:1) > "9"
              OR WS-MD-ICS-SAISIE(4:1) < "0"
              OR WS-MD-ICS-SAISIE(4:1) > "9"
               MOVE "N" TO WS-MD-OK
           END-IF.
           IF WS-MD-ICS-SAISIE(5:3) NOT = "ZZZ"
               MOVE "N" TO WS-MD-OK
           END-IF.
           PERFORM VARYING WS-MD-IDX FROM 8 BY 1
               UNTIL WS-MD-IDX > 13
               MOVE WS-MD-ICS-SAISIE(WS-MD-IDX:1) TO WS-MD-CAR
               IF NOT ((WS-MD-CAR >= "A" AND WS-MD-CAR <= "Z")
                       OR (WS-MD-CAR >= "0" AND WS-MD-CAR <= "9"))
                   MOVE "N" TO WS-MD-OK
               END-IF
           END-PERFORM.

           IF WS-MD-OK = "N"
               DISPLAY "Format ICS invalide "
                   "(FR + 2 chiffres + ZZZ + 6 alphanum)."
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-MD-ICS-SAISIE(1:13) TO WS-MD-ICS.

      *    Nom du creancier
           DISPLAY "Nom du creancier : " WITH NO ADVANCING.
           ACCEPT WS-MD-CREANCIER.
           IF WS-MD-CREANCIER = SPACES
               DISPLAY "Nom du creancier obligatoire."
               EXIT PARAGRAPH
           END-IF.

      *    Montant max par prelevement (0 = sans limite)
           DISPLAY "Montant maximum (0 = sans limite) : "
               WITH NO ADVANCING.
           ACCEPT WS-MD-MAX.

      *    Type R (recurrent) ou U (ponctuel)
           DISPLAY "Type (R recurrent / U ponctuel) : "
               WITH NO ADVANCING.
           ACCEPT WS-MD-TYPE.
           MOVE FUNCTION UPPER-CASE(WS-MD-TYPE) TO WS-MD-TYPE.

           IF WS-MD-TYPE NOT = "R" AND WS-MD-TYPE NOT = "U"
               DISPLAY "Type invalide (R ou U)."
               EXIT PARAGRAPH
           END-IF.

      *    Enregistrement dans mandat.dat
           MOVE "00" TO WS-STATUS.
           OPEN EXTEND MANDAT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               MOVE WS-MD-RUM       TO MANDAT-RUM
               MOVE WS-MD-ICS       TO MANDAT-ICS
               MOVE WS-MD-CREANCIER TO MANDAT-CREANCIER
               MOVE WS-MD-COMPTE    TO MANDAT-COMPTE
               MOVE WS-MD-MAX       TO MANDAT-MAX
               MOVE WS-MD-TYPE      TO MANDAT-TYPE
               MOVE WS-TODAY        TO MANDAT-DATE
               MOVE "A"             TO MANDAT-STATUT
               MOVE ZEROS           TO MANDAT-DERNIER
               WRITE MANDAT-RECORD
               CLOSE MANDAT-FILE
           END-IF.

           DISPLAY "Mandat SEPA cree : " FUNCTION TRIM(WS-MD-RUM).
           DISPLAY "ICS : " WS-MD-ICS " | Compte : " WS-MD-COMPTE
               " | Type : " WS-MD-TYPE.
           IF WS-MD-MAX > 0
               MOVE WS-MD-MAX TO WS-MD-MAX-AFF
               DISPLAY "Montant max : " FUNCTION TRIM(WS-MD-MAX-AFF)
           ELSE
               DISPLAY "Montant max : sans limite"
           END-IF.

      *    Journalisation
           MOVE "MANDAT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "RUM=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MD-RUM) DELIMITED BY SIZE
               " Cpt=" DELIMITED BY SIZE
               WS-MD-COMPTE DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-MD-ICS DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
