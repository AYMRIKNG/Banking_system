           DISPLAY "=== Alertes fraude ===".

      *    Lister les alertes nouvelles
           MOVE ZEROS TO WS-NB-ALERTES-NOUV.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT ALERTE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10"
                   READ ALERTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF ALERTE-STATUT = "N"
                               ADD 1 TO WS-NB-ALERTES-NOUV
                               MOVE ALERTE-MONTANT TO WS-MONTANT-AFF
                               DISPLAY "ID: " ALERTE-ID
                                   " | Date: " ALERTE-DATE
                                   " | Cpt: " ALERTE-COMPTE
                                   " | " FUNCTION TRIM(ALERTE-REGLE)
                                   " | Mnt: "
                                   FUNCTION TRIM(WS-MONTANT-AFF)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE ALERTE-FILE
           END-IF.

           DISPLAY WS-NB-ALERTES-NOUV " alerte(s) nouvelle(s).".
           IF WS-NB-ALERTES-NOUV = 0
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "ID a marquer traitee (0=aucune, 99999=toutes) : "
               WITH NO ADVANCING.
           ACCEPT WS-INPUT-ALERTE-ID.

           IF WS-INPUT-ALERTE-ID = 0
               EXIT PARAGRAPH
           END-IF.

           IF WS-INPUT-ALERTE-ID = 99999
               MOVE ZEROS TO WS-NB-TRAITEES
               MOVE "00" TO WS-STATUS
               OPEN I-O ALERTE-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   PERFORM UNTIL WS-STATUS = "10"
                       READ ALERTE-FILE
                           AT END
                               MOVE "10" TO WS-STATUS
                           NOT AT END
                               IF ALERTE-STATUT = "N"
                                   MOVE "T" TO ALERTE-STATUT
                                   REWRITE ALERTE-RECORD
                                   ADD 1 TO WS-NB-TRAITEES
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE ALERTE-FILE
               END-IF
               DISPLAY WS-NB-TRAITEES
                   " alerte(s) marquee(s) traitee(s)."
               MOVE "ALERTE TRAITEE" TO WS-JOURNAL-ACTION
               MOVE SPACES TO WS-JOURNAL-DETAIL
               STRING "Toutes (" DELIMITED BY SIZE
                   WS-NB-TRAITEES DELIMITED BY SIZE
                   " alerte(s))" DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
               PERFORM ECRIRE-JOURNAL
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O ALERTE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ ALERTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF ALERTE-ID = WS-INPUT-ALERTE-ID
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Alerte non trouvee."
               CLOSE ALERTE-FILE
               EXIT PARAGRAPH
           END-IF.

           IF ALERTE-STATUT = "T"
               DISPLAY "Alerte deja traitee."
               CLOSE ALERTE-FILE
               EXIT PARAGRAPH
           END-IF.

           MOVE "T" TO ALERTE-STATUT.
           REWRITE ALERTE-RECORD.
           CLOSE ALERTE-FILE.

           DISPLAY "Alerte " WS-INPUT-ALERTE-ID " marquee traitee.".

           MOVE "ALERTE TRAITEE" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "ID=" DELIMITED BY SIZE
               WS-INPUT-ALERTE-ID DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
