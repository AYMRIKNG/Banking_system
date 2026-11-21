           DISPLAY "Modification d'un client.".
           DISPLAY "ID client : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-ID.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O CLIENT-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ CLIENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF CLIENT-ID = WS-INPUT-ID
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "Client non trouve."
               CLOSE CLIENT-FILE
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Nom       : " CLIENT-NAME.
           DISPLAY "Adresse   : " CLIENT-ADDRESS.
           DISPLAY "Telephone : " CLIENT-PHONE.

      *    Laisser vide pour garder l'ancienne valeur
           MOVE SPACES TO WS-MODIF-ADRESSE.
           MOVE SPACES TO WS-MODIF-PHONE.
           DISPLAY "Nouvelle adresse (vide = inchangee) : "
               WITH NO ADVANCING.
           ACCEPT WS-MODIF-ADRESSE.
           DISPLAY "Nouveau telephone (vide = inchange) : "
               WITH NO ADVANCING.
           ACCEPT WS-MODIF-PHONE.

           IF WS-MODIF-ADRESSE NOT = SPACES
               MOVE WS-MODIF-ADRESSE TO CLIENT-ADDRESS
           END-IF.
           IF WS-MODIF-PHONE NOT = SPACES
               MOVE WS-MODIF-PHONE TO CLIENT-PHONE
           END-IF.

           REWRITE CLIENT-RECORD.
           CLOSE CLIENT-FILE.

           DISPLAY "Client modifie avec succes.".

           MOVE "MODIF CLIENT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "ID=" DELIMITED BY SIZE
               WS-INPUT-ID(6:5) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
