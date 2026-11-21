           DISPLAY "Ajout d'un nouveau client.".
           MOVE ZEROS TO WS-MAX-CLIENT-NUM.
           MOVE "00" TO WS-STATUS.
           
           OPEN INPUT CLIENT-FILE.
           
           PERFORM UNTIL WS-STATUS = "10"
               READ CLIENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF CLIENT-ID > WS-MAX-CLIENT-NUM
                           MOVE CLIENT-ID TO WS-MAX-CLIENT-NUM
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE CLIENT-FILE.
           
           ADD 1 TO WS-MAX-CLIENT-NUM GIVING WS-NOUVELLE-ID.
           MOVE WS-NOUVELLE-ID TO NEW-ID.
           DISPLAY "ID client : " NEW-ID.
           
           DISPLAY "Nom complet : " WITH NO ADVANCING.
           ACCEPT NEW-NAME.
           DISPLAY "Adresse : " WITH NO ADVANCING.
           ACCEPT NEW-ADDRESS.
           DISPLAY "Telephone : " WITH NO ADVANCING.
           ACCEPT NEW-PHONE.

           OPEN EXTEND CLIENT-FILE.
           MOVE NEW-ID TO CLIENT-ID.
           MOVE NEW-NAME TO CLIENT-NAME.
           MOVE NEW-ADDRESS TO CLIENT-ADDRESS.
           MOVE NEW-PHONE TO CLIENT-PHONE.
           WRITE CLIENT-RECORD.
           CLOSE CLIENT-FILE.

           DISPLAY "Client ajoute avec succes.".

      *    Mettre a jour le nombre de clients de la banque
           OPEN I-O BANQUE-FILE.
           READ BANQUE-FILE
               AT END
                   DISPLAY "Erreur : enregistrement banque manquant."
                   CLOSE BANQUE-FILE
                   EXIT PARAGRAPH
           END-READ.
           ADD 1 TO BANQUE-NB-CLIENTS.
           REWRITE BANQUE-RECORD.
           CLOSE BANQUE-FILE.

           MOVE "AJOUT CLIENT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "ID=" DELIMITED BY SIZE
               NEW-ID DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(NEW-NAME) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
