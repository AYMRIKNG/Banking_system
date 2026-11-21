           DISPLAY "--- Agios et commissions d'intervention ---".

           MOVE ZEROS TO WS-AG-NB-CPT-DEB
                         WS-AG-TOT-AGIOS
                         WS-AG-TOT-COMM
                         WS-AG-TOT-GEN
                         WS-AG-NB-ELIG.

      *    Lister les comptes Courant et Entreprise
           MOVE "00" TO WS-STATUS.
           OPEN INPUT COMPTE-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur ouverture fichier comptes : " WS-STATUS
               EXIT PARAGRAPH
           END-IF.
           PERFORM UNTIL WS-STATUS = "10"
               READ COMPTE-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF COMPTE-TYPE = "Courant"
                          OR COMPTE-TYPE = "Entreprise"
                           IF WS-AG-NB-ELIG < 500
                               ADD 1 TO WS-AG-NB-ELIG
                               MOVE COMPTE-NUM
                                   TO WS-AG-ELIG-NUM(WS-AG-NB-ELIG)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

      *    Traiter chaque compte eligible
           PERFORM VARYING WS-AG-K FROM 1 BY 1
               UNTIL WS-AG-K > WS-AG-NB-ELIG

      *        Verifier idempotence : deja preleve ce mois-ci ?
               MOVE "N" TO WS-AG-DEJA
               MOVE "00" TO WS-STATUS
               OPEN INPUT TRANSACTION-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   PERFORM UNTIL WS-STATUS = "10"
                       OR WS-AG-DEJA = "O"
                       READ TRANSACTION-FILE
                           AT END
                               MOVE "10" TO WS-STATUS
                           NOT AT END
                               IF TRANSACTION-COMPTE
                                   = WS-AG-ELIG-NUM(WS-AG-K)
                                  AND TRANSACTION-DATE(1:6)
                                   = WS-TODAY(1:6)
                                  AND (TRANSACTION-TYPE = "Agios"
                                   OR TRANSACTION-TYPE = "Commission")
                                   MOVE "O" TO WS-AG-DEJA
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE TRANSACTION-FILE
               END-IF

               IF WS-AG-DEJA = "O"
                   DISPLAY "Compte " WS-AG-ELIG-NUM(WS-AG-K)
                       " : agios deja preleves ce mois-ci."
               ELSE
                   MOVE WS-AG-ELIG-NUM(WS-AG-K) TO WS-AG-COMPTE
                   MOVE WS-TODAY(1:6)           TO WS-AG-MOIS
                   MOVE "N"                     TO WS-AG-DETAIL
                   PERFORM CALCULER-AGIOS

                   COMPUTE WS-AG-TOTAL-CPT =
                       WS-AG-AGIOS + WS-AG-COMMISSION

                   IF WS-AG-ERREUR = "N" AND WS-AG-TOTAL-CPT > 0
      *                Prelevement force sur le compte
                       MOVE "N" TO WS-FOUND
                       MOVE "00" TO WS-STATUS
                       OPEN I-O COMPTE-FILE
                       PERFORM UNTIL WS-STATUS = "10"
                           OR WS-FOUND = "Y"
                           READ COMPTE-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                   IF COMPTE-NUM = WS-AG-COMPTE
                                       SUBTRACT WS-AG-TOTAL-CPT
                                           FROM COMPTE-SOLDE
                                       REWRITE COMPTE-RECORD
                                       MOVE "Y" TO WS-FOUND
                                   END-IF
                           END-READ
                       END-PERFORM
                       CLOSE COMPTE-FILE

      *                Ecrire transactions Agios et Commission
                       MOVE "00" TO WS-STATUS
                       OPEN EXTEND TRANSACTION-FILE
                       IF WS-AG-AGIOS > 0
                           MOVE WS-COUNTER   TO WS-TRANS-ID
                           MOVE WS-AG-COMPTE TO WS-TRANS-COMPTE
                           MOVE WS-TODAY     TO WS-TRANS-DATE
                           MOVE "Agios"      TO WS-TRANS-TYPE
                           MOVE WS-AG-AGIOS  TO WS-TRANS-MONTANT
                           MOVE ZERO         TO WS-TRANS-DEST
                           MOVE WS-TRANSACTION TO TRANSACTION-RECORD
                           WRITE TRANSACTION-RECORD
                           ADD 1 TO WS-COUNTER
                       END-IF
                       IF WS-AG-COMMISSION > 0
                           MOVE WS-COUNTER       TO WS-TRANS-ID
                           MOVE WS-AG-COMPTE     TO WS-TRANS-COMPTE
                           MOVE WS-TODAY         TO WS-TRANS-DATE
                           MOVE "Commission"     TO WS-TRANS-TYPE
                           MOVE WS-AG-COMMISSION TO WS-TRANS-MONTANT
                           MOVE ZERO             TO WS-TRANS-DEST
                           MOVE WS-TRANSACTION   TO TRANSACTION-RECORD
                           WRITE TRANSACTION-RECORD
                           ADD 1 TO WS-COUNTER
                       END-IF
                       CLOSE TRANSACTION-FILE

                       ADD 1                TO WS-AG-NB-CPT-DEB
                       ADD WS-AG-AGIOS      TO WS-AG-TOT-AGIOS
                       ADD WS-AG-COMMISSION TO WS-AG-TOT-COMM
                   END-IF
               END-IF
           END-PERFORM.

      *    Crediter la banque du total agios + commissions
           COMPUTE WS-AG-TOT-GEN = WS-AG-TOT-AGIOS + WS-AG-TOT-COMM.
           IF WS-AG-TOT-GEN > 0
               OPEN I-O BANQUE-FILE
               READ BANQUE-FILE
                   AT END
                       DISPLAY "Erreur : banque manquante."
                   NOT AT END
                       ADD WS-AG-TOT-GEN TO BANQUE-SOLDE
                       REWRITE BANQUE-RECORD
               END-READ
               CLOSE BANQUE-FILE
           END-IF.

           MOVE WS-AG-TOT-AGIOS TO WS-AG-AFF-TOT-A.
           MOVE WS-AG-TOT-COMM  TO WS-AG-AFF-TOT-C.
           DISPLAY "Comptes debiteurs : " WS-AG-NB-CPT-DEB.
           DISPLAY "Total agios       : "
               FUNCTION TRIM(WS-AG-AFF-TOT-A).
           DISPLAY "Total commissions : "
               FUNCTION TRIM(WS-AG-AFF-TOT-C).

      *    Journalisation
           MOVE "AGIOS" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Cpt=" DELIMITED BY SIZE
               WS-AG-NB-CPT-DEB DELIMITED BY SIZE
               " Agios=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-AG-AFF-TOT-A) DELIMITED BY SIZE
               " Comm=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-AG-AFF-TOT-C) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
