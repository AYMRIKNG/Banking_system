           DISPLAY "Definir / debloquer le PIN d'un client.".
           DISPLAY "ID client : " WITH NO ADVANCING.
           ACCEPT WS-AUTH-CLIENT-ID.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CLIENT-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ CLIENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF CLIENT-ID = WS-AUTH-CLIENT-ID
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE CLIENT-FILE.

           IF WS-FOUND = "N"
               DISPLAY "Client non trouve."
               EXIT PARAGRAPH
           END-IF.

           MOVE SPACES TO WS-PIN-SAISIE WS-PIN-CONFIRM.
           DISPLAY "Nouveau PIN (4 chiffres) : " WITH NO ADVANCING.
           ACCEPT WS-PIN-SAISIE.
           DISPLAY "Confirmer le PIN : " WITH NO ADVANCING.
           ACCEPT WS-PIN-CONFIRM.

           IF WS-PIN-SAISIE(1:4) NOT NUMERIC
              OR WS-PIN-SAISIE(5:16) NOT = SPACES
               DISPLAY "Le PIN doit contenir exactement 4 chiffres."
               MOVE SPACES TO WS-PIN-SAISIE WS-PIN-CONFIRM
               EXIT PARAGRAPH
           END-IF.

           IF WS-PIN-SAISIE NOT = WS-PIN-CONFIRM
               DISPLAY "Les deux PIN ne correspondent pas."
               MOVE SPACES TO WS-PIN-SAISIE WS-PIN-CONFIRM
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-PIN-SAISIE TO WS-H-TEXTE.
           MOVE WS-AUTH-CLIENT-ID TO WS-H-SEL.
           MOVE ZEROS TO WS-H-RESULTAT.
           CALL "HACHAGE" USING WS-HACHAGE.
           MOVE SPACES TO WS-PIN-SAISIE WS-PIN-CONFIRM.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O PIN-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ PIN-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF PIN-CLIENT-ID = WS-AUTH-CLIENT-ID
                               MOVE WS-H-RESULTAT TO PIN-HASH
                               MOVE 0 TO PIN-ESSAIS
                               MOVE "N" TO PIN-BLOQUE
                               REWRITE PIN-RECORD
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE PIN-FILE
           END-IF.

           IF WS-FOUND = "N"
               MOVE "00" TO WS-STATUS
               OPEN EXTEND PIN-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   MOVE WS-AUTH-CLIENT-ID TO PIN-CLIENT-ID
                   MOVE WS-H-RESULTAT     TO PIN-HASH
                   MOVE 0                 TO PIN-ESSAIS
                   MOVE "N"               TO PIN-BLOQUE
                   WRITE PIN-RECORD
                   CLOSE PIN-FILE
               END-IF
           END-IF.

           DISPLAY "PIN client enregistre et debloque.".

           MOVE "PIN DEFINI" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Client " DELIMITED BY SIZE
               WS-AUTH-CLIENT-ID DELIMITED BY SIZE
               " (conseiller)" DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
