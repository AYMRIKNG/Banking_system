           DISPLAY "=== Journal d'audit ===".
           DISPLAY "Date (AAAAMMJJ, 0 = tout) : " WITH NO ADVANCING.
           ACCEPT WS-JOURNAL-DATE.

           IF WS-JOURNAL-DATE NOT = 0
               MOVE SPACES TO WS-JOURNAL-DATE-FMT
               STRING WS-JOURNAL-DATE(1:4) DELIMITED BY SIZE
                   "-" DELIMITED BY SIZE
                   WS-JOURNAL-DATE(5:2) DELIMITED BY SIZE
                   "-" DELIMITED BY SIZE
                   WS-JOURNAL-DATE(7:2) DELIMITED BY SIZE
                   INTO WS-JOURNAL-DATE-FMT
               END-STRING
           END-IF.

           MOVE ZEROS TO WS-JOURNAL-NB.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT JOURNAL-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ JOURNAL-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF WS-JOURNAL-DATE = 0
                              OR JOURNAL-LINE(1:10)
                                 = WS-JOURNAL-DATE-FMT
                               DISPLAY FUNCTION TRIM(JOURNAL-LINE)
                               ADD 1 TO WS-JOURNAL-NB
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE JOURNAL-FILE
           END-IF.

           DISPLAY WS-JOURNAL-NB " ligne(s) affichee(s).".
