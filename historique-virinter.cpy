           DISPLAY "=== Historique virements internationaux ===".
           DISPLAY "Numero de compte (0 = tous) : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.

           MOVE ZEROS TO WS-VI-NB.
           MOVE ZEROS TO WS-VI-TOTAL-EUR.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT VIRINTER-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ VIRINTER-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF WS-INPUT-COMPTE = 0
                              OR VI-COMPTE = WS-INPUT-COMPTE
                               ADD 1 TO WS-VI-NB
                               ADD VI-MONTANT-EUR TO WS-VI-TOTAL-EUR
                               MOVE VI-MONTANT-DEV TO WS-VI-AFF-DEV
                               MOVE VI-MONTANT-EUR TO WS-VI-AFF-EUR
                               MOVE VI-FRAIS       TO WS-VI-AFF-FRAIS
                               MOVE VI-TAUX-APPL   TO WS-DEV-TAUX-AFF
                               DISPLAY "ID: " VI-ID
                                   " | Date: " VI-DATE
                                   " | Cpt: " VI-COMPTE
                                   " | "
                                   FUNCTION TRIM(WS-VI-AFF-DEV)
                                   " " VI-DEVISE " -> "
                                   FUNCTION TRIM(WS-VI-AFF-EUR)
                                   " EUR"
                               DISPLAY "   Taux: "
                                   FUNCTION TRIM(WS-DEV-TAUX-AFF)
                                   " | Frais: "
                                   FUNCTION TRIM(WS-VI-AFF-FRAIS)
                                   " (" VI-OPTION ") | IBAN: "
                                   FUNCTION TRIM(VI-IBAN)
                                   " | BIC: "
                                   FUNCTION TRIM(VI-BIC)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE VIRINTER-FILE
           END-IF.

           IF WS-VI-NB = 0
               DISPLAY "Aucun virement international trouve."
           END-IF.

           MOVE WS-VI-TOTAL-EUR TO WS-MONTANT-AFF.
           DISPLAY "Nombre de virements : " WS-VI-NB.
           DISPLAY "Total EUR : " FUNCTION TRIM(WS-MONTANT-AFF)
               " EUR".
