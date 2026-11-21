           DISPLAY "=== Export CSV des transactions ===".
           DISPLAY "Numero de compte (0 = tous) : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.

           MOVE ZEROS TO WS-NB-EXPORT.
           OPEN OUTPUT CSV-FILE.
           MOVE "ID;Compte;Date;Type;Montant;Destination" TO CSV-LINE.
           WRITE CSV-LINE.

           MOVE "00" TO WS-STATUS.
           OPEN INPUT TRANSACTION-FILE.
           PERFORM UNTIL WS-STATUS = "10"
               READ TRANSACTION-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF WS-INPUT-COMPTE = 0
                          OR TRANSACTION-COMPTE = WS-INPUT-COMPTE
                           MOVE TRANSACTION-MONTANT TO WS-MONTANT-AFF
                           MOVE SPACES TO CSV-LINE
                           STRING TRANSACTION-ID DELIMITED BY SIZE
                               ";" DELIMITED BY SIZE
                               TRANSACTION-COMPTE DELIMITED BY SIZE
                               ";" DELIMITED BY SIZE
                               TRANSACTION-DATE(1:4) DELIMITED BY SIZE
                               "-" DELIMITED BY SIZE
                               TRANSACTION-DATE(5:2) DELIMITED BY SIZE
                               "-" DELIMITED BY SIZE
                               TRANSACTION-DATE(7:2) DELIMITED BY SIZE
                               ";" DELIMITED BY SIZE
                               TRANSACTION-TYPE DELIMITED BY SPACE
                               ";" DELIMITED BY SIZE
                               FUNCTION TRIM(WS-MONTANT-AFF)
                                   DELIMITED BY SIZE
                               ";" DELIMITED BY SIZE
                               TRANSACTION-DEST DELIMITED BY SIZE
                               INTO CSV-LINE
                           END-STRING
                           WRITE CSV-LINE
                           ADD 1 TO WS-NB-EXPORT
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE TRANSACTION-FILE.
           CLOSE CSV-FILE.

           DISPLAY WS-NB-EXPORT " transaction(s) exportee(s)".
           DISPLAY "Fichier : transactions.csv".
