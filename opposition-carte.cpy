           DISPLAY "Opposition sur une carte bancaire.".
           MOVE SPACES TO WS-CB-SAISIE-NUM.
           DISPLAY "Numero de carte (16 chif.) ou 4 derniers : "
               WITH NO ADVANCING.
           ACCEPT WS-CB-SAISIE-NUM.

           MOVE "N" TO WS-CB-MODE-RECH.
           IF WS-CB-SAISIE-NUM(1:4) IS NUMERIC
              AND WS-CB-SAISIE-NUM(5:16) = SPACES
               MOVE "4" TO WS-CB-MODE-RECH
               DISPLAY "Numero de compte : " WITH NO ADVANCING
               ACCEPT WS-INPUT-COMPTE
           ELSE
               IF WS-CB-SAISIE-NUM(1:16) IS NUMERIC
                  AND WS-CB-SAISIE-NUM(17:4) = SPACES
                   MOVE "F" TO WS-CB-MODE-RECH
               ELSE
                   DISPLAY "Saisie invalide."
                   EXIT PARAGRAPH
               END-IF
           END-IF.

      *    Rechercher la carte dans carte.dat
           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CARTE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ CARTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF (WS-CB-MODE-RECH = "F"
                               AND CARTE-NUM = WS-CB-SAISIE-NUM(1:16))
                              OR (WS-CB-MODE-RECH = "4"
                                  AND CARTE-COMPTE = WS-INPUT-COMPTE
                                  AND CARTE-NUM(13:4)
                                      = WS-CB-SAISIE-NUM(1:4))
                               MOVE CARTE-NUM TO WS-CB-CARTE-NUM
                               MOVE CARTE-COMPTE
                                   TO WS-CB-CARTE-COMPTE
                               MOVE CARTE-STATUT
                                   TO WS-CB-CARTE-STATUT
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CARTE-FILE
           END-IF.

           IF WS-FOUND = "N"
               DISPLAY "Carte non trouvee."
               EXIT PARAGRAPH
           END-IF.

      *    En espace client : verifier le proprietaire du compte
           IF WS-MODE-SESSION = "U"
               MOVE WS-CB-CARTE-COMPTE TO WS-INPUT-COMPTE
               PERFORM VERIFIER-PROPRIETAIRE
               IF WS-PROPRIETAIRE-OK NOT = "O"
                   DISPLAY "Cette carte ne vous appartient pas."
                   EXIT PARAGRAPH
               END-IF
           END-IF.

           IF WS-CB-CARTE-STATUT = "O"
               DISPLAY "Cette carte est deja en opposition."
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CB-CARTE-NUM TO WS-CB-NUM-BRUT.
           PERFORM MASQUER-CARTE.
           DISPLAY "Carte  : " WS-CB-NUM-MASQUE.
           DISPLAY "Compte : " WS-CB-CARTE-COMPTE.

           DISPLAY "Motif (1=Perte, 2=Vol, 3=Fraude) : "
               WITH NO ADVANCING.
           ACCEPT WS-CB-MOTIF-CHOIX.
           EVALUATE WS-CB-MOTIF-CHOIX
               WHEN 1
                   MOVE "Perte" TO WS-CB-MOTIF-LIB
               WHEN 2
                   MOVE "Vol" TO WS-CB-MOTIF-LIB
               WHEN 3
                   MOVE "Fraude" TO WS-CB-MOTIF-LIB
               WHEN OTHER
                   DISPLAY "Motif invalide."
                   EXIT PARAGRAPH
           END-EVALUATE.

           DISPLAY "Confirmer l'opposition (O/N) : "
               WITH NO ADVANCING.
           ACCEPT WS-CONFIRMATION.
           IF WS-CONFIRMATION NOT = "O"
              AND WS-CONFIRMATION NOT = "o"
               DISPLAY "Opposition annulee."
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO WS-FOUND.
           MOVE "00" TO WS-STATUS.
           OPEN I-O CARTE-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ CARTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF CARTE-NUM = WS-CB-CARTE-NUM
                               MOVE "O" TO CARTE-STATUT
                               REWRITE CARTE-RECORD
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CARTE-FILE
           END-IF.

           DISPLAY "Carte " WS-CB-NUM-MASQUE
               " mise en opposition ("
               FUNCTION TRIM(WS-CB-MOTIF-LIB) ").".

           MOVE "OPPOSITION" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING WS-CB-NUM-MASQUE DELIMITED BY SIZE
               " Motif=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-CB-MOTIF-LIB) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
