           DISPLAY "=== Creer un virement permanent ===".
           DISPLAY "Numero du compte source : " WITH NO ADVANCING.
           ACCEPT WS-VP-SOURCE.
           DISPLAY "Numero du compte destination : " WITH NO ADVANCING.
           ACCEPT WS-VP-DEST.
           DISPLAY "Montant mensuel : " WITH NO ADVANCING.
           ACCEPT WS-VP-MONTANT.

           IF WS-VP-MONTANT <= 0
               DISPLAY "Montant invalide."
               EXIT PARAGRAPH
           END-IF.

           IF WS-VP-SOURCE = WS-VP-DEST
               DISPLAY "Compte source et destination identiques."
               EXIT PARAGRAPH
           END-IF.

      *    Verifier le compte source
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = WS-VP-SOURCE
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
               DISPLAY "Compte source cloture."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.
           CLOSE COMPTE-FILE.

      *    Verifier le compte destination
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-NUM = WS-VP-DEST
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "Compte destination non trouve."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.

           IF COMPTE-TYPE = "Cloture"
               DISPLAY "Compte destination cloture."
               CLOSE COMPTE-FILE
               EXIT PARAGRAPH
           END-IF.
           CLOSE COMPTE-FILE.

      *    Calculer le nouvel ID (plus grand ID existant + 1)
           MOVE ZEROS TO WS-MAX-VP-ID.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT VIREMENT-FILE.
           PERFORM UNTIL WS-STATUS = "10"
               READ VIREMENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF VP-ID > WS-MAX-VP-ID
                           MOVE VP-ID TO WS-MAX-VP-ID
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE VIREMENT-FILE.

           ADD 1 TO WS-MAX-VP-ID.

           OPEN EXTEND VIREMENT-FILE.
           MOVE WS-MAX-VP-ID      TO VP-ID.
           MOVE WS-VP-SOURCE      TO VP-SOURCE.
           MOVE WS-VP-DEST        TO VP-DEST.
           MOVE WS-VP-MONTANT     TO VP-MONTANT.
           MOVE "O"               TO VP-ACTIF.
           MOVE ZEROS             TO VP-DERNIER-MOIS.
           WRITE VIREMENT-RECORD.
           CLOSE VIREMENT-FILE.

           DISPLAY "Virement permanent cree. ID : " WS-MAX-VP-ID.

           MOVE "VIR PERMANENT" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Creation ID=" DELIMITED BY SIZE
               WS-MAX-VP-ID DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
