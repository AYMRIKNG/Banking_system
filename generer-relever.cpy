           DISPLAY "=== Generation de releve mensuel ===".
           DISPLAY "Numero de compte : " WITH NO ADVANCING.
           ACCEPT WS-INPUT-COMPTE.
           DISPLAY "Mois du releve (AAAAMM, ex: 202506) : "
               WITH NO ADVANCING.
           ACCEPT WS-MOIS.

           MOVE SPACES TO WS-RELEVE-NOM.
           STRING "releve_" DELIMITED BY SIZE
                  WS-INPUT-COMPTE DELIMITED BY SIZE
                  "_" DELIMITED BY SIZE
                  WS-MOIS DELIMITED BY SIZE
                  ".txt" DELIMITED BY SIZE
                  INTO WS-RELEVE-NOM.

           OPEN OUTPUT RELEVE-FILE.

           MOVE "=== Releve du compte " TO RELEVE-LINE.
           STRING RELEVE-LINE DELIMITED BY SIZE
                  WS-INPUT-COMPTE DELIMITED BY SIZE
                  " - Mois: " DELIMITED BY SIZE
                  WS-MOIS DELIMITED BY SIZE
                  INTO RELEVE-LINE.
           MOVE SPACES TO RELEVE-LINE.
           MOVE SPACES TO RELEVE-LINE.
           WRITE RELEVE-LINE.

           MOVE "G" TO WS-IBAN-MODE.
           MOVE WS-INPUT-COMPTE TO WS-IBAN-COMPTE.
           MOVE SPACES TO WS-IBAN-TEXTE
                          WS-IBAN-AFFICHE.
           MOVE ZERO TO WS-IBAN-RETOUR.
           CALL "CALCUL-IBAN" USING WS-CALCUL-IBAN.

           MOVE SPACES TO RELEVE-LINE.
           STRING "IBAN : " DELIMITED BY SIZE
                  WS-IBAN-AFFICHE DELIMITED BY SIZE
                  INTO RELEVE-LINE
           END-STRING.
           WRITE RELEVE-LINE.
           MOVE SPACES TO RELEVE-LINE.



           OPEN INPUT TRANSACTION-FILE.
           MOVE "00" TO WS-STATUS.

           PERFORM UNTIL WS-STATUS = "10"
               READ TRANSACTION-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF TRANSACTION-COMPTE = WS-INPUT-COMPTE
                           MOVE ZEROS TO WS-TRANSACTION-MOIS
                           EVALUATE TRUE
                               WHEN TRANSACTION-TYPE = "Ouverture"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Depot"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Retrait"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Virement"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Credit"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Interet"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Rembourse"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Cloture"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Frais"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Annulation"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                               WHEN TRANSACTION-TYPE = "Report"
                                    MOVE TRANSACTION-DATE(1:6)
                                        TO WS-TRANSACTION-MOIS
                           END-EVALUATE

                           IF WS-TRANSACTION-MOIS = WS-MOIS
                               MOVE SPACES TO RELEVE-LINE
                               IF TRANSACTION-TYPE = "Report"
                                   STRING
                                       "Date: " DELIMITED BY SIZE
                                       TRANSACTION-DATE
                                           DELIMITED BY SIZE
                                       " | Type: Report a nouveau"
                                           DELIMITED BY SIZE
                                       " | Montant: " DELIMITED BY SIZE
                                       TRANSACTION-MONTANT
                                           DELIMITED BY SIZE
                                       " | Compte: " DELIMITED BY SIZE
                                       TRANSACTION-COMPTE
                                           DELIMITED BY SIZE
                                       INTO RELEVE-LINE
                                   END-STRING
                               ELSE
                                   STRING
                                       "Date: " DELIMITED BY SIZE
                                       TRANSACTION-DATE
                                           DELIMITED BY SIZE
                                       " | Type: " DELIMITED BY SIZE
                                       TRANSACTION-TYPE
                                           DELIMITED BY SIZE
                                       " | Montant: " DELIMITED BY SIZE
                                       TRANSACTION-MONTANT
                                           DELIMITED BY SIZE
                                       " | Compte: " DELIMITED BY SIZE
                                       TRANSACTION-COMPTE
                                           DELIMITED BY SIZE
                                       INTO RELEVE-LINE
                                   END-STRING
                               END-IF

                               IF TRANSACTION-TYPE = "Virement"
                                   STRING RELEVE-LINE DELIMITED BY SIZE
                                      " | Dest: " DELIMITED BY SIZE
                                      TRANSACTION-DEST DELIMITED BY SIZE
                                      INTO RELEVE-LINE
                               END-IF

                               WRITE RELEVE-LINE
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE TRANSACTION-FILE.
           CLOSE RELEVE-FILE.

           DISPLAY "Releve genere : " WS-RELEVE-NOM.
