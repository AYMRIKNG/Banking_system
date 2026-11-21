           DISPLAY "=== Interets Livret A (quinzaines) ===".
           DISPLAY "1 - Simulation detaillee (un compte)".
           DISPLAY "2 - Versement des interets (tous les Livret A)".
           DISPLAY "Votre choix : " WITH NO ADVANCING.
           ACCEPT WS-LA-CHOIX.

           IF WS-LA-CHOIX NOT = 1 AND WS-LA-CHOIX NOT = 2
               DISPLAY "Choix invalide."
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-TODAY(1:4) TO WS-LA-ANNEE-COUR.

      *    Mode 1 : simulation detaillee pour un compte et une annee
           IF WS-LA-CHOIX = 1
               DISPLAY "Numero de compte : " WITH NO ADVANCING
               ACCEPT WS-LA-COMPTE

               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN INPUT COMPTE-FILE
               PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                   READ COMPTE-FILE
                       AT END
                           MOVE "10" TO WS-STATUS
                       NOT AT END
                           IF COMPTE-NUM = WS-LA-COMPTE
                               MOVE "Y" TO WS-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE COMPTE-FILE

               IF WS-FOUND = "N"
                   DISPLAY "Compte non trouve."
                   EXIT PARAGRAPH
               END-IF
               IF COMPTE-TYPE NOT = "LivretA"
                   DISPLAY "Ce compte n'est pas un Livret A."
                   EXIT PARAGRAPH
               END-IF

               DISPLAY "Annee (AAAA) : " WITH NO ADVANCING
               ACCEPT WS-LA-ANNEE

               IF WS-LA-ANNEE < 1900
                  OR WS-LA-ANNEE > WS-LA-ANNEE-COUR
                   DISPLAY "Refus : annee invalide ou future."
                   EXIT PARAGRAPH
               END-IF

               IF WS-LA-ANNEE = WS-LA-ANNEE-COUR
                   DISPLAY "Attention : annee en cours, le calcul "
                       "s'arrete a aujourd'hui."
               END-IF

               MOVE "O" TO WS-LA-DETAIL
               PERFORM CALCULER-QUINZAINES

               IF WS-LA-ERREUR = "O"
                   EXIT PARAGRAPH
               END-IF

               MOVE WS-TAUX-LIVRETA  TO WS-LA-AFF-TAUX
               MOVE WS-LA-SOLDE-DEB  TO WS-SOLDE-AFF
               MOVE WS-LA-INTERETS   TO WS-MONTANT-AFF
               DISPLAY "Compte            : " WS-LA-COMPTE
               DISPLAY "Annee             : " WS-LA-ANNEE
               DISPLAY "Taux annuel       : " WS-LA-AFF-TAUX " %"
               DISPLAY "Quinzaines        : " WS-LA-Q-MAX
               DISPLAY "Solde debut annee : "
                   FUNCTION TRIM(WS-SOLDE-AFF)
               DISPLAY "Total interets    : "
                   FUNCTION TRIM(WS-MONTANT-AFF)
               EXIT PARAGRAPH
           END-IF.

      *    Mode 2 : versement des interets pour tous les Livret A
           DISPLAY "Annee (AAAA) : " WITH NO ADVANCING.
           ACCEPT WS-LA-ANNEE.

           IF WS-LA-ANNEE < 1900 OR WS-LA-ANNEE > WS-LA-ANNEE-COUR
               DISPLAY "Refus : annee invalide ou future."
               EXIT PARAGRAPH
           END-IF.

           IF WS-LA-ANNEE = WS-LA-ANNEE-COUR
               DISPLAY "Attention : annee en cours, le calcul "
                   "s'arrete a aujourd'hui."
           END-IF.

      *    Lister tous les Livret A actifs
           MOVE ZEROS TO WS-LA-NB-ELIG
                         WS-LA-NB-VERS
                         WS-LA-NB-DEJA
                         WS-LA-TOT-INTERETS.

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
                       IF COMPTE-TYPE = "LivretA"
                           IF WS-LA-NB-ELIG < 500
                               ADD 1 TO WS-LA-NB-ELIG
                               MOVE COMPTE-NUM
                                   TO WS-LA-ELIG-NUM(WS-LA-NB-ELIG)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE COMPTE-FILE.

           IF WS-LA-NB-ELIG = 0
               DISPLAY "Aucun Livret A actif."
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-LA-DATE-VERS = WS-LA-ANNEE * 10000 + 1231.

      *    Traiter chaque Livret A
           PERFORM VARYING WS-LA-K FROM 1 BY 1
               UNTIL WS-LA-K > WS-LA-NB-ELIG

      *        Verifier idempotence pour ce compte et cette annee
               MOVE "N" TO WS-LA-DEJA
               MOVE "00" TO WS-STATUS
               OPEN INPUT TRANSACTION-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   PERFORM UNTIL WS-STATUS = "10" OR WS-LA-DEJA = "O"
                       READ TRANSACTION-FILE
                           AT END
                               MOVE "10" TO WS-STATUS
                           NOT AT END
                               IF TRANSACTION-COMPTE
                                   = WS-LA-ELIG-NUM(WS-LA-K)
                                  AND TRANSACTION-TYPE = "IntLivret"
                                  AND TRANSACTION-DATE(1:4)
                                   = WS-LA-DATE-VERS(1:4)
                                   MOVE "O" TO WS-LA-DEJA
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE TRANSACTION-FILE
               END-IF

               IF WS-LA-DEJA = "O"
                   ADD 1 TO WS-LA-NB-DEJA
                   DISPLAY "Compte " WS-LA-ELIG-NUM(WS-LA-K)
                       " : interets deja verses pour " WS-LA-ANNEE "."
               ELSE
                   MOVE WS-LA-ELIG-NUM(WS-LA-K) TO WS-LA-COMPTE
                   MOVE "N"                     TO WS-LA-DETAIL
                   PERFORM CALCULER-QUINZAINES

                   IF WS-LA-ERREUR = "N" AND WS-LA-INTERETS > 0
      *                Crediter le Livret A (peut depasser le plafond)
                       MOVE "N" TO WS-FOUND
                       MOVE "00" TO WS-STATUS
                       OPEN I-O COMPTE-FILE
                       PERFORM UNTIL WS-STATUS = "10"
                           OR WS-FOUND = "Y"
                           READ COMPTE-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                   IF COMPTE-NUM = WS-LA-COMPTE
                                       ADD WS-LA-INTERETS
                                           TO COMPTE-SOLDE
                                       REWRITE COMPTE-RECORD
                                       MOVE "Y" TO WS-FOUND
                                   END-IF
                           END-READ
                       END-PERFORM
                       CLOSE COMPTE-FILE

      *                Ecrire transaction IntLivret datee AAAA1231
                       MOVE "00" TO WS-STATUS
                       OPEN EXTEND TRANSACTION-FILE
                       MOVE WS-COUNTER       TO WS-TRANS-ID
                       MOVE WS-LA-COMPTE     TO WS-TRANS-COMPTE
                       MOVE WS-LA-DATE-VERS  TO WS-TRANS-DATE
                       MOVE "IntLivret"      TO WS-TRANS-TYPE
                       MOVE WS-LA-INTERETS   TO WS-TRANS-MONTANT
                       MOVE ZERO             TO WS-TRANS-DEST
                       MOVE WS-TRANSACTION   TO TRANSACTION-RECORD
                       WRITE TRANSACTION-RECORD
                       CLOSE TRANSACTION-FILE
                       ADD 1 TO WS-COUNTER

                       ADD 1 TO WS-LA-NB-VERS
                       ADD WS-LA-INTERETS TO WS-LA-TOT-INTERETS
                       MOVE WS-LA-INTERETS TO WS-MONTANT-AFF
                       DISPLAY "Compte " WS-LA-COMPTE
                           " : +" FUNCTION TRIM(WS-MONTANT-AFF)
                   END-IF
               END-IF
           END-PERFORM.

      *    La banque paie les interets
           IF WS-LA-TOT-INTERETS > 0
               MOVE "00" TO WS-STATUS
               OPEN I-O BANQUE-FILE
               READ BANQUE-FILE
                   AT END
                       DISPLAY "Erreur : banque manquante."
                   NOT AT END
                       IF WS-LA-TOT-INTERETS > BANQUE-SOLDE
                           DISPLAY "Attention : solde banque "
                               "insuffisant."
                           MOVE ZEROS TO BANQUE-SOLDE
                       ELSE
                           SUBTRACT WS-LA-TOT-INTERETS
                               FROM BANQUE-SOLDE
                       END-IF
                       REWRITE BANQUE-RECORD
               END-READ
               CLOSE BANQUE-FILE
           END-IF.

           MOVE WS-LA-TOT-INTERETS TO WS-MONTANT-AFF.
           DISPLAY "=== Resume versement Livret A " WS-LA-ANNEE " ===".
           DISPLAY "Livrets credites : " WS-LA-NB-VERS.
           DISPLAY "Deja verses      : " WS-LA-NB-DEJA.
           DISPLAY "Total verse      : "
               FUNCTION TRIM(WS-MONTANT-AFF).

      *    Journalisation
           MOVE "INTERETS LIVRET" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Annee=" DELIMITED BY SIZE
               WS-LA-ANNEE DELIMITED BY SIZE
               " Cpt=" DELIMITED BY SIZE
               WS-LA-NB-VERS DELIMITED BY SIZE
               " Total=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MONTANT-AFF) DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
