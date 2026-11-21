      *    Revocation d'un mandat SEPA (conseiller ou espace client)
           DISPLAY "=== Revoquer un mandat SEPA ===".
           MOVE SPACES TO WS-MD-RUM
                          WS-MD-ICS
                          WS-MD-CREANCIER
                          WS-MD-STATUT
                          WS-MD-TYPE.
           MOVE ZEROS TO WS-MD-COMPTE.

           DISPLAY "RUM du mandat : " WITH NO ADVANCING.
           ACCEPT WS-MD-RUM.
           MOVE FUNCTION UPPER-CASE(WS-MD-RUM) TO WS-MD-RUM.

           IF WS-MD-RUM = SPACES
               DISPLAY "RUM invalide."
               EXIT PARAGRAPH
           END-IF.

      *    Recherche du mandat
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT MANDAT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ MANDAT-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF MANDAT-RUM = WS-MD-RUM
                               MOVE "Y" TO WS-FOUND
                               MOVE MANDAT-ICS TO WS-MD-ICS
                               MOVE MANDAT-CREANCIER
                                   TO WS-MD-CREANCIER
                               MOVE MANDAT-COMPTE TO WS-MD-COMPTE
                               MOVE MANDAT-STATUT TO WS-MD-STATUT
                               MOVE MANDAT-TYPE TO WS-MD-TYPE
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE MANDAT-FILE
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Mandat non trouve."
               EXIT PARAGRAPH
           END-IF.

      *    Controle proprietaire en espace client
           IF WS-MODE-SESSION = "U"
               MOVE WS-MD-COMPTE TO WS-INPUT-COMPTE
               PERFORM VERIFIER-PROPRIETAIRE
               IF WS-PROPRIETAIRE-OK NOT = "O"
                   DISPLAY "Ce mandat n'appartient pas a vos comptes."
                   EXIT PARAGRAPH
               END-IF
           END-IF.

           IF WS-MD-STATUT = "R"
               DISPLAY "Ce mandat est deja revoque."
               EXIT PARAGRAPH
           END-IF.

           IF WS-MD-STATUT = "C"
               DISPLAY "Ce mandat ponctuel est deja consomme."
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Creancier : " FUNCTION TRIM(WS-MD-CREANCIER).
           DISPLAY "ICS       : " WS-MD-ICS.
           DISPLAY "Compte    : " WS-MD-COMPTE.
           DISPLAY "Confirmer la revocation (O/N) : "
               WITH NO ADVANCING.
           ACCEPT WS-CONFIRMATION.

           IF WS-CONFIRMATION NOT = "O"
              AND WS-CONFIRMATION NOT = "o"
               DISPLAY "Revocation annulee."
               EXIT PARAGRAPH
           END-IF.

      *    Mise a jour du statut a R
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O MANDAT-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ MANDAT-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF MANDAT-RUM = WS-MD-RUM
                               MOVE "R" TO MANDAT-STATUT
                               REWRITE MANDAT-RECORD
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE MANDAT-FILE
           END-IF.

           DISPLAY "Mandat " FUNCTION TRIM(WS-MD-RUM) " revoque.".

      *    Journalisation
           MOVE "REVOCATION" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           IF WS-MODE-SESSION = "U"
               STRING "RUM=" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-MD-RUM) DELIMITED BY SIZE
                   " Cpt=" DELIMITED BY SIZE
                   WS-MD-COMPTE DELIMITED BY SIZE
                   " (client)" DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
           ELSE
               STRING "RUM=" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-MD-RUM) DELIMITED BY SIZE
                   " Cpt=" DELIMITED BY SIZE
                   WS-MD-COMPTE DELIMITED BY SIZE
                   " (conseiller)" DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
           END-IF.
           PERFORM ECRIRE-JOURNAL.
