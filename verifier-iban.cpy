           DISPLAY "=== Verifier un IBAN ===".
           DISPLAY "IBAN (sans espaces) : " WITH NO ADVANCING.
           MOVE SPACES TO WS-IBAN-SAISIE.
           ACCEPT WS-IBAN-SAISIE.

           IF WS-IBAN-SAISIE(28:13) NOT = SPACES
               DISPLAY "IBAN invalide."
               EXIT PARAGRAPH
           END-IF.

           MOVE "V" TO WS-IBAN-MODE.
           MOVE ZEROS TO WS-IBAN-COMPTE.
           MOVE WS-IBAN-SAISIE(1:27) TO WS-IBAN-TEXTE.
           MOVE SPACES TO WS-IBAN-AFFICHE.
           MOVE ZERO TO WS-IBAN-RETOUR.

           CALL "CALCUL-IBAN" USING WS-CALCUL-IBAN.

           IF WS-IBAN-RETOUR NOT = 0
               DISPLAY "IBAN invalide."
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "IBAN valide.".
           DISPLAY "Numero de compte : " WS-IBAN-COMPTE.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-IBAN-COMPTE
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE
           END-IF.

           IF WS-FOUND = "Y"
               DISPLAY "Ce compte existe dans compte.dat."
           ELSE
               DISPLAY "Ce compte n'existe pas dans compte.dat."
           END-IF.
