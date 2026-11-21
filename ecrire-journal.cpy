      *    Sauvegarder WS-STATUS (partage par tous les fichiers)
           MOVE WS-STATUS TO WS-JOURNAL-STATUS.

           MOVE FUNCTION CURRENT-DATE TO WS-JOURNAL-NOW.
           MOVE SPACES TO JOURNAL-LINE.
           STRING WS-JOURNAL-NOW(1:4) DELIMITED BY SIZE
               "-" DELIMITED BY SIZE
               WS-JOURNAL-NOW(5:2) DELIMITED BY SIZE
               "-" DELIMITED BY SIZE
               WS-JOURNAL-NOW(7:2) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-JOURNAL-NOW(9:2) DELIMITED BY SIZE
               ":" DELIMITED BY SIZE
               WS-JOURNAL-NOW(11:2) DELIMITED BY SIZE
               ":" DELIMITED BY SIZE
               WS-JOURNAL-NOW(13:2) DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               WS-JOURNAL-ACTION DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               WS-JOURNAL-DETAIL DELIMITED BY SIZE
               INTO JOURNAL-LINE
           END-STRING.

           OPEN EXTEND JOURNAL-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               WRITE JOURNAL-LINE
               CLOSE JOURNAL-FILE
           END-IF.

           MOVE WS-JOURNAL-STATUS TO WS-STATUS.
