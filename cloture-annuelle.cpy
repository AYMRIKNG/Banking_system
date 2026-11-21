           DISPLAY "=== Cloture annuelle / archivage ===".
           DISPLAY "Annee a archiver : " WITH NO ADVANCING.
           ACCEPT WS-ARCH-ANNEE.

           MOVE WS-TODAY(1:4) TO WS-ARCH-ANNEE-COUR.
           IF WS-ARCH-ANNEE = 0
               DISPLAY "Refus : annee invalide."
               EXIT PARAGRAPH
           END-IF.
           IF WS-ARCH-ANNEE >= WS-ARCH-ANNEE-COUR
               DISPLAY "Refus : impossible d'archiver l'annee "
                   "en cours ou future (" WS-ARCH-ANNEE ")."
               EXIT PARAGRAPH
           END-IF.

      *    Verifier que transaction_AAAA.dat n'existe pas deja
           MOVE SPACES TO WS-ARCHIVE-NOM.
           STRING "transaction_" DELIMITED BY SIZE
               WS-ARCH-ANNEE DELIMITED BY SIZE
               ".dat" DELIMITED BY SIZE
               INTO WS-ARCHIVE-NOM
           END-STRING.

           MOVE "00" TO WS-STATUS.
           OPEN INPUT ARCHIVE-FILE.
           IF WS-STATUS = "00"
               CLOSE ARCHIVE-FILE
               DISPLAY "Refus : archive deja existante ("
                   FUNCTION TRIM(WS-ARCHIVE-NOM) ")."
               EXIT PARAGRAPH
           END-IF.
           IF WS-STATUS NOT = "35"
               DISPLAY "Erreur verification archive : " WS-STATUS
               EXIT PARAGRAPH
           END-IF.

      *    Charger comptes et transactions en memoire
           PERFORM CHARGER-COMPTES-TRANS.
           IF WS-CI-ERREUR = "O"
               EXIT PARAGRAPH
           END-IF.

      *    Compter les transactions a archiver (<= AAAA) et gardees
           MOVE ZEROS TO WS-ARCH-NB-ARCH.
           MOVE ZEROS TO WS-ARCH-NB-GARDE.
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               IF WS-TT-ANNEE(WS-CI-I) <= WS-ARCH-ANNEE
                   ADD 1 TO WS-ARCH-NB-ARCH
               ELSE
                   ADD 1 TO WS-ARCH-NB-GARDE
               END-IF
           END-PERFORM.

           IF WS-ARCH-NB-ARCH = 0
               DISPLAY "Refus : aucune transaction en "
                   WS-ARCH-ANNEE " ou avant."
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Transactions a archiver : " WS-ARCH-NB-ARCH.
           DISPLAY "Transactions conservees : " WS-ARCH-NB-GARDE.
           DISPLAY "Confirmer l'archivage (O/N) : "
               WITH NO ADVANCING.
           ACCEPT WS-CONFIRMATION.
           IF WS-CONFIRMATION NOT = "O"
              AND WS-CONFIRMATION NOT = "o"
               DISPLAY "Archivage annule."
               EXIT PARAGRAPH
           END-IF.

      *    Rejouer uniquement les transactions gardees (> AAAA)
           MOVE ZEROS TO WS-CI-NB-ANOMALIES.
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               IF WS-TT-ANNEE(WS-CI-I) > WS-ARCH-ANNEE
                   PERFORM REJOUER-TRANSACTION
               END-IF
           END-PERFORM.

      *    Calculer les reports avant toute ecriture
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-COMPTES
               COMPUTE WS-ARCH-REPORT =
                   WS-TC-SOLDE-FIC(WS-CI-I) - WS-TC-SOLDE-REC(WS-CI-I)
               MOVE WS-ARCH-REPORT TO WS-TC-REPORT(WS-CI-I)
           END-PERFORM.

      *    Ecrire l'archive transaction_AAAA.dat
           MOVE "00" TO WS-STATUS.
           OPEN OUTPUT ARCHIVE-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur creation archive : " WS-STATUS
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               IF WS-TT-ANNEE(WS-CI-I) <= WS-ARCH-ANNEE
                   MOVE WS-TT-RAW(WS-CI-I) TO ARCHIVE-RECORD
                   WRITE ARCHIVE-RECORD
               END-IF
           END-PERFORM.
           CLOSE ARCHIVE-FILE.

      *    Reecrire transaction.dat : Reports puis gardees
           COMPUTE WS-ARCH-DATE-REP = WS-ARCH-ANNEE * 10000 + 1231.

           MOVE ZEROS TO WS-ARCH-NB-REPORTS.
           MOVE "00" TO WS-STATUS.
           OPEN OUTPUT TRANSACTION-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur reecriture transactions : " WS-STATUS
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-COMPTES
               MOVE WS-COUNTER            TO WS-TRANS-ID
               MOVE WS-TC-NUM(WS-CI-I)    TO WS-TRANS-COMPTE
               MOVE WS-ARCH-DATE-REP      TO WS-TRANS-DATE
               IF WS-TC-REPORT(WS-CI-I) < 0
                   MOVE "ReportDeb"       TO WS-TRANS-TYPE
                   COMPUTE WS-TRANS-MONTANT =
                       FUNCTION ABS(WS-TC-REPORT(WS-CI-I))
               ELSE
                   MOVE "Report"          TO WS-TRANS-TYPE
                   MOVE WS-TC-REPORT(WS-CI-I) TO WS-TRANS-MONTANT
               END-IF
               MOVE ZERO                  TO WS-TRANS-DEST
               MOVE WS-TRANSACTION        TO TRANSACTION-RECORD
               WRITE TRANSACTION-RECORD
               ADD 1 TO WS-COUNTER
               ADD 1 TO WS-ARCH-NB-REPORTS
           END-PERFORM.

           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               IF WS-TT-ANNEE(WS-CI-I) > WS-ARCH-ANNEE
                   MOVE WS-TT-RAW(WS-CI-I) TO TRANSACTION-RECORD
                   WRITE TRANSACTION-RECORD
               END-IF
           END-PERFORM.
           CLOSE TRANSACTION-FILE.

      *    Cumuler les interets de l'annee AAAA par compte
           PERFORM VARYING WS-CI-I FROM 1 BY 1
               UNTIL WS-CI-I > WS-CI-NB-TRANS
               IF WS-TT-TYPE(WS-CI-I) = "Interet"
                  AND WS-TT-ANNEE(WS-CI-I) = WS-ARCH-ANNEE
                   SET WS-CI-IDX-C TO 1
                   SEARCH WS-TAB-COMPTES
                       AT END
                           CONTINUE
                       WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                           CONTINUE
                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                           = WS-TT-COMPTE(WS-CI-I)
                           ADD WS-TT-MONTANT(WS-CI-I)
                               TO WS-TC-INTERET(WS-CI-IDX-C)
                   END-SEARCH
               END-IF
           END-PERFORM.

      *    Generer l'IFU (ifu_AAAA.txt) et afficher le resume
           MOVE SPACES TO WS-IFU-NOM.
           STRING "ifu_" DELIMITED BY SIZE
               WS-ARCH-ANNEE DELIMITED BY SIZE
               ".txt" DELIMITED BY SIZE
               INTO WS-IFU-NOM
           END-STRING.

           MOVE "00" TO WS-STATUS.
           OPEN OUTPUT IFU-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur creation IFU : " WS-STATUS
               EXIT PARAGRAPH
           END-IF.

           MOVE SPACES TO WS-ARCH-LIGNE.
           STRING "=== IFU - Releve fiscal des interets - "
               DELIMITED BY SIZE
               WS-ARCH-ANNEE DELIMITED BY SIZE
               " ===" DELIMITED BY SIZE
               INTO WS-ARCH-LIGNE
           END-STRING.
           DISPLAY FUNCTION TRIM(WS-ARCH-LIGNE).
           MOVE WS-ARCH-LIGNE TO IFU-LINE.
           WRITE IFU-LINE.

           MOVE SPACES TO WS-ARCH-LIGNE.
           STRING "ID   " DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "Client                        " DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "Interets    " DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "PFU 30%     " DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "Net         " DELIMITED BY SIZE
               INTO WS-ARCH-LIGNE
           END-STRING.
           DISPLAY FUNCTION TRIM(WS-ARCH-LIGNE).
           MOVE WS-ARCH-LIGNE TO IFU-LINE.
           WRITE IFU-LINE.

           MOVE ALL "-" TO WS-ARCH-LIGNE(1:83).
           MOVE SPACES TO WS-ARCH-LIGNE(84:17).
           DISPLAY WS-ARCH-LIGNE(1:83).
           MOVE WS-ARCH-LIGNE TO IFU-LINE.
           WRITE IFU-LINE.

           MOVE ZEROS TO WS-IFU-TOT-BRUT.
           MOVE ZEROS TO WS-IFU-TOT-PFU.
           MOVE ZEROS TO WS-IFU-TOT-NET.
           MOVE "N" TO WS-ARCH-FIN-CLI.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT CLIENT-FILE.
           IF WS-STATUS = "00"
               PERFORM UNTIL WS-ARCH-FIN-CLI = "O"
                   READ CLIENT-FILE
                       AT END
                           MOVE "O" TO WS-ARCH-FIN-CLI
                       NOT AT END
                           MOVE ZEROS TO WS-IFU-BRUT
                           PERFORM VARYING WS-CI-I FROM 1 BY 1
                               UNTIL WS-CI-I > WS-CI-NB-COMPTES
                               IF WS-TC-CLIENT-ID(WS-CI-I) = CLIENT-ID
                                   ADD WS-TC-INTERET(WS-CI-I)
                                       TO WS-IFU-BRUT
                               END-IF
                           END-PERFORM
                           COMPUTE WS-IFU-PFU ROUNDED =
                               WS-IFU-BRUT * 30 / 100
                           COMPUTE WS-IFU-NET =
                               WS-IFU-BRUT - WS-IFU-PFU
                           ADD WS-IFU-BRUT TO WS-IFU-TOT-BRUT
                           ADD WS-IFU-PFU  TO WS-IFU-TOT-PFU
                           ADD WS-IFU-NET  TO WS-IFU-TOT-NET

                           MOVE WS-IFU-BRUT TO WS-IFU-AFF-BRUT
                           MOVE WS-IFU-PFU  TO WS-IFU-AFF-PFU
                           MOVE WS-IFU-NET  TO WS-IFU-AFF-NET

                           MOVE SPACES TO WS-ARCH-LIGNE
                           STRING CLIENT-ID DELIMITED BY SIZE
                               " | " DELIMITED BY SIZE
                               CLIENT-NAME DELIMITED BY SIZE
                               " | " DELIMITED BY SIZE
                               WS-IFU-AFF-BRUT DELIMITED BY SIZE
                               " | " DELIMITED BY SIZE
                               WS-IFU-AFF-PFU DELIMITED BY SIZE
                               " | " DELIMITED BY SIZE
                               WS-IFU-AFF-NET DELIMITED BY SIZE
                               INTO WS-ARCH-LIGNE
                           END-STRING
                           DISPLAY FUNCTION TRIM(WS-ARCH-LIGNE)
                           MOVE WS-ARCH-LIGNE TO IFU-LINE
                           WRITE IFU-LINE
                   END-READ
               END-PERFORM
               CLOSE CLIENT-FILE
           END-IF.

           MOVE ALL "-" TO WS-ARCH-LIGNE(1:83).
           MOVE SPACES TO WS-ARCH-LIGNE(84:17).
           DISPLAY WS-ARCH-LIGNE(1:83).
           MOVE WS-ARCH-LIGNE TO IFU-LINE.
           WRITE IFU-LINE.

           MOVE WS-IFU-TOT-BRUT TO WS-IFU-AFF-BRUT.
           MOVE WS-IFU-TOT-PFU  TO WS-IFU-AFF-PFU.
           MOVE WS-IFU-TOT-NET  TO WS-IFU-AFF-NET.

           MOVE SPACES TO WS-ARCH-LIGNE.
           STRING "TOTAL" DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               "Total general                 " DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               WS-IFU-AFF-BRUT DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               WS-IFU-AFF-PFU DELIMITED BY SIZE
               " | " DELIMITED BY SIZE
               WS-IFU-AFF-NET DELIMITED BY SIZE
               INTO WS-ARCH-LIGNE
           END-STRING.
           DISPLAY FUNCTION TRIM(WS-ARCH-LIGNE).
           MOVE WS-ARCH-LIGNE TO IFU-LINE.
           WRITE IFU-LINE.

           CLOSE IFU-FILE.

           DISPLAY "Archivage termine : "
               FUNCTION TRIM(WS-ARCHIVE-NOM)
               " (" WS-ARCH-NB-ARCH " transactions).".
           DISPLAY "Reports generes : " WS-ARCH-NB-REPORTS
               " | Conservees : " WS-ARCH-NB-GARDE ".".
           DISPLAY "IFU enregistre dans "
               FUNCTION TRIM(WS-IFU-NOM).

      *    Journalisation
           MOVE "ARCHIVAGE" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Annee=" DELIMITED BY SIZE
               WS-ARCH-ANNEE DELIMITED BY SIZE
               " Archivees=" DELIMITED BY SIZE
               WS-ARCH-NB-ARCH DELIMITED BY SIZE
               " Reports=" DELIMITED BY SIZE
               WS-ARCH-NB-REPORTS DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
