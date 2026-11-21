      *    Traitement par lot (batch) avec controle E/F et rejets
           MOVE SPACES TO WS-LOT-NOM.
           DISPLAY "Nom du fichier lot : " WITH NO ADVANCING.
           ACCEPT WS-LOT-NOM.

           MOVE ZEROS TO WS-LOT-NB-LIGNES
                         WS-LOT-NB-E
                         WS-LOT-NB-D
                         WS-LOT-NB-F
                         WS-LOT-NUM
                         WS-LOT-DATE
                         WS-LOT-NB-ANNONCE
                         WS-LOT-F-NB
                         WS-LOT-F-TOTAL
                         WS-LOT-SOMME-D.
           MOVE SPACES TO WS-LOT-DERNIER-TYPE
                          WS-LOT-MOTIF.

      *    Phase 1 : ouverture et controle de structure
           MOVE "00" TO WS-STATUS.
           OPEN INPUT LOT-FILE.
           IF WS-STATUS NOT = "00"
               MOVE "Fichier lot introuvable ou illisible"
                   TO WS-LOT-MOTIF
               DISPLAY "Lot refuse : "
                   FUNCTION TRIM(WS-LOT-MOTIF)
               MOVE "LOT REFUSE" TO WS-JOURNAL-ACTION
               MOVE SPACES TO WS-JOURNAL-DETAIL
               STRING "Motif=" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-LOT-MOTIF) DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
               PERFORM ECRIRE-JOURNAL
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO WS-LOT-EOF.
           PERFORM UNTIL WS-LOT-EOF = "O"
             READ LOT-FILE
               AT END
                 MOVE "O" TO WS-LOT-EOF
               NOT AT END
                 MOVE LOT-RECORD TO WS-LOT-LIGNE
                 ADD 1 TO WS-LOT-NB-LIGNES
                 MOVE WS-LOT-LIGNE(1:1) TO WS-LOT-DERNIER-TYPE
                 IF WS-LOT-NB-LIGNES = 1
                    AND WS-LOT-LIGNE(1:1) NOT = "E"
                   IF WS-LOT-MOTIF = SPACES
                     MOVE "Premiere ligne doit etre E"
                       TO WS-LOT-MOTIF
                   END-IF
                 END-IF
                 EVALUATE WS-LOT-LIGNE(1:1)
                   WHEN "E"
                     ADD 1 TO WS-LOT-NB-E
                     IF WS-LOT-NB-E > 1
                       IF WS-LOT-MOTIF = SPACES
                         MOVE "Plusieurs lignes en-tete E"
                           TO WS-LOT-MOTIF
                       END-IF
                     ELSE
                       IF WS-LE-NUM IS NUMERIC
                          AND WS-LE-DATE IS NUMERIC
                          AND WS-LE-NB IS NUMERIC
                         MOVE WS-LE-NUM TO WS-LOT-NUM
                         MOVE WS-LE-DATE TO WS-LOT-DATE
                         MOVE WS-LE-NB TO WS-LOT-NB-ANNONCE
                       ELSE
                         IF WS-LOT-MOTIF = SPACES
                           MOVE "En-tete E non numerique"
                             TO WS-LOT-MOTIF
                         END-IF
                       END-IF
                     END-IF
                   WHEN "D"
                     ADD 1 TO WS-LOT-NB-D
                     IF WS-LD-MONTANT IS NUMERIC
                       ADD WS-LD-MONTANT TO WS-LOT-SOMME-D
                     ELSE
                       IF WS-LOT-MOTIF = SPACES
                         MOVE "Montant non numerique en ligne D"
                           TO WS-LOT-MOTIF
                       END-IF
                     END-IF
                   WHEN "F"
                     ADD 1 TO WS-LOT-NB-F
                     IF WS-LOT-NB-F > 1
                       IF WS-LOT-MOTIF = SPACES
                         MOVE "Plusieurs lignes fin F"
                           TO WS-LOT-MOTIF
                       END-IF
                     ELSE
                       IF WS-LF-NB IS NUMERIC
                          AND WS-LF-TOTAL IS NUMERIC
                         MOVE WS-LF-NB TO WS-LOT-F-NB
                         MOVE WS-LF-TOTAL TO WS-LOT-F-TOTAL
                       ELSE
                         IF WS-LOT-MOTIF = SPACES
                           MOVE "Ligne fin F non numerique"
                             TO WS-LOT-MOTIF
                         END-IF
                       END-IF
                     END-IF
                   WHEN OTHER
                     IF WS-LOT-MOTIF = SPACES
                       MOVE "Type de ligne inconnu"
                         TO WS-LOT-MOTIF
                     END-IF
                 END-EVALUATE
             END-READ
           END-PERFORM.
           CLOSE LOT-FILE.

           IF WS-LOT-MOTIF = SPACES AND WS-LOT-NB-LIGNES = 0
               MOVE "Fichier lot vide" TO WS-LOT-MOTIF
           END-IF.
           IF WS-LOT-MOTIF = SPACES AND WS-LOT-NB-E = 0
               MOVE "Ligne en-tete E manquante" TO WS-LOT-MOTIF
           END-IF.
           IF WS-LOT-MOTIF = SPACES AND WS-LOT-NB-F = 0
               MOVE "Ligne de fin F manquante" TO WS-LOT-MOTIF
           END-IF.
           IF WS-LOT-MOTIF = SPACES
              AND WS-LOT-DERNIER-TYPE NOT = "F"
               MOVE "Derniere ligne doit etre F" TO WS-LOT-MOTIF
           END-IF.
           IF WS-LOT-MOTIF = SPACES
              AND WS-LOT-NB-D NOT = WS-LOT-NB-ANNONCE
               MOVE "Nombre de lignes D different de E"
                   TO WS-LOT-MOTIF
           END-IF.
           IF WS-LOT-MOTIF = SPACES
              AND WS-LOT-NB-D NOT = WS-LOT-F-NB
               MOVE "Nombre de lignes D different de F"
                   TO WS-LOT-MOTIF
           END-IF.
           IF WS-LOT-MOTIF = SPACES
              AND WS-LOT-SOMME-D NOT = WS-LOT-F-TOTAL
               MOVE "Somme des montants D differente du total F"
                   TO WS-LOT-MOTIF
           END-IF.

      *    Verification anti-doublon dans lots.dat
           IF WS-LOT-MOTIF = SPACES
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN INPUT LOTS-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                       READ LOTS-FILE
                           AT END
                               MOVE "10" TO WS-STATUS
                           NOT AT END
                               IF LOTS-NUM = WS-LOT-NUM
                                   MOVE "Y" TO WS-FOUND
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE LOTS-FILE
               END-IF
               IF WS-FOUND = "Y"
                   MOVE "Numero de lot deja traite"
                       TO WS-LOT-MOTIF
               END-IF
           END-IF.

           IF WS-LOT-MOTIF NOT = SPACES
               DISPLAY "Lot refuse : "
                   FUNCTION TRIM(WS-LOT-MOTIF)
               MOVE "LOT REFUSE" TO WS-JOURNAL-ACTION
               MOVE SPACES TO WS-JOURNAL-DETAIL
               STRING "Lot=" DELIMITED BY SIZE
                   WS-LOT-NUM DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-LOT-MOTIF) DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
               PERFORM ECRIRE-JOURNAL
               EXIT PARAGRAPH
           END-IF.

      *    Phase 2 : application ligne par ligne
           MOVE ZEROS TO WS-LOT-NUM-LIGNE
                         WS-LOT-NB-OK
                         WS-LOT-NB-REJET
                         WS-LOT-MNT-OK
                         WS-LOT-MNT-REJET.

           MOVE "00" TO WS-STATUS.
           OPEN OUTPUT REJET-FILE.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT LOT-FILE.
           MOVE "N" TO WS-LOT-EOF.

           PERFORM UNTIL WS-LOT-EOF = "O"
             READ LOT-FILE
               AT END
                 MOVE "O" TO WS-LOT-EOF
               NOT AT END
                 ADD 1 TO WS-LOT-NUM-LIGNE
                 MOVE LOT-RECORD TO WS-LOT-LIGNE
                 IF WS-LOT-LIGNE(1:1) = "D"
                   MOVE SPACES TO WS-LOT-CODE-REJET
                                  WS-LOT-LIB-REJET

      *            R03 : type invalide
                   IF WS-LD-OP NOT = "Depot"
                      AND WS-LD-OP NOT = "Retrait"
                      AND WS-LD-OP NOT = "Virement"
                     MOVE "R03" TO WS-LOT-CODE-REJET
                     MOVE "Type invalide" TO WS-LOT-LIB-REJET
                   END-IF

      *            R04 : montant nul
                   IF WS-LOT-CODE-REJET = SPACES
                      AND WS-LD-MONTANT = 0
                     MOVE "R04" TO WS-LOT-CODE-REJET
                     MOVE "Montant nul" TO WS-LOT-LIB-REJET
                   END-IF

      *            R01 / R02 : compte source inconnu ou cloture
                   IF WS-LOT-CODE-REJET = SPACES
                     IF WS-LD-COMPTE NOT NUMERIC
                       MOVE "R01" TO WS-LOT-CODE-REJET
                       MOVE "Compte inconnu" TO WS-LOT-LIB-REJET
                     ELSE
                       MOVE "N" TO WS-FOUND
                       MOVE SPACES TO WS-LOT-TYPE-SRC
                       MOVE ZEROS TO WS-LOT-SOLDE-SRC
                       MOVE "00" TO WS-STATUS
                       OPEN INPUT COMPTE-FILE
                       PERFORM UNTIL WS-STATUS = "10"
                           OR WS-FOUND = "Y"
                         READ COMPTE-FILE
                           AT END
                             MOVE "10" TO WS-STATUS
                           NOT AT END
                             IF COMPTE-NUM = WS-LD-COMPTE
                               MOVE "Y" TO WS-FOUND
                               MOVE COMPTE-TYPE
                                 TO WS-LOT-TYPE-SRC
                               MOVE COMPTE-SOLDE
                                 TO WS-LOT-SOLDE-SRC
                             END-IF
                         END-READ
                       END-PERFORM
                       CLOSE COMPTE-FILE
                       IF WS-FOUND = "N"
                         MOVE "R01" TO WS-LOT-CODE-REJET
                         MOVE "Compte inconnu"
                           TO WS-LOT-LIB-REJET
                       ELSE
                         IF WS-LOT-TYPE-SRC = "Cloture"
                           MOVE "R02" TO WS-LOT-CODE-REJET
                           MOVE "Compte cloture"
                             TO WS-LOT-LIB-REJET
                         END-IF
                       END-IF
                     END-IF
                   END-IF

      *            R07 / R06 : controles destination si Virement
                   IF WS-LOT-CODE-REJET = SPACES
                      AND WS-LD-OP = "Virement"
                     IF WS-LD-DEST NOT NUMERIC
                       MOVE "R06" TO WS-LOT-CODE-REJET
                       MOVE "Compte dest inconnu ou cloture"
                         TO WS-LOT-LIB-REJET
                     ELSE
                       IF WS-LD-DEST = WS-LD-COMPTE
                         MOVE "R07" TO WS-LOT-CODE-REJET
                         MOVE "Compte dest egal au compte source"
                           TO WS-LOT-LIB-REJET
                       ELSE
                         MOVE "N" TO WS-FOUND
                         MOVE SPACES TO WS-LOT-TYPE-DEST
                         MOVE "00" TO WS-STATUS
                         OPEN INPUT COMPTE-FILE
                         PERFORM UNTIL WS-STATUS = "10"
                             OR WS-FOUND = "Y"
                           READ COMPTE-FILE
                             AT END
                               MOVE "10" TO WS-STATUS
                             NOT AT END
                               IF COMPTE-NUM = WS-LD-DEST
                                 MOVE "Y" TO WS-FOUND
                                 MOVE COMPTE-TYPE
                                   TO WS-LOT-TYPE-DEST
                               END-IF
                           END-READ
                         END-PERFORM
                         CLOSE COMPTE-FILE
                         IF WS-FOUND = "N"
                            OR WS-LOT-TYPE-DEST = "Cloture"
                           MOVE "R06" TO WS-LOT-CODE-REJET
                           MOVE "Compte dest inconnu ou cloture"
                             TO WS-LOT-LIB-REJET
                         END-IF
                       END-IF
                     END-IF
                   END-IF

      *            R05 : depassement du decouvert autorise
                   IF WS-LOT-CODE-REJET = SPACES
                      AND (WS-LD-OP = "Retrait"
                           OR WS-LD-OP = "Virement")
                     IF WS-LOT-TYPE-SRC = "Epargne"
                       MOVE ZEROS TO WS-DEC-PLAFOND
                     ELSE
                       MOVE WS-LD-COMPTE TO WS-DEC-COMPTE
                       PERFORM LIRE-DECOUVERT
                     END-IF
                     COMPUTE WS-DEC-SOLDE-PROJ =
                       WS-LOT-SOLDE-SRC - WS-LD-MONTANT
                     COMPUTE WS-DEC-LIMITE = 0 - WS-DEC-PLAFOND
                     IF WS-DEC-SOLDE-PROJ < WS-DEC-LIMITE
                       MOVE "R05" TO WS-LOT-CODE-REJET
                       MOVE "Depassement du decouvert autorise"
                         TO WS-LOT-LIB-REJET
                     END-IF
                   END-IF

      *            Rejet ou application
                   IF WS-LOT-CODE-REJET NOT = SPACES
                     ADD 1 TO WS-LOT-NB-REJET
                     ADD WS-LD-MONTANT TO WS-LOT-MNT-REJET
                     MOVE SPACES TO REJET-LINE
                     STRING WS-LOT-NUM-LIGNE DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       WS-LOT-CODE-REJET DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-LOT-LIB-REJET)
                         DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-LOT-LIGNE TRAILING)
                         DELIMITED BY SIZE
                       INTO REJET-LINE
                     END-STRING
                     WRITE REJET-LINE
                   ELSE
      *              Mise a jour compte source
                     MOVE "N" TO WS-FOUND
                     MOVE "00" TO WS-STATUS
                     OPEN I-O COMPTE-FILE
                     PERFORM UNTIL WS-STATUS = "10"
                         OR WS-FOUND = "Y"
                       READ COMPTE-FILE
                         AT END
                           MOVE "10" TO WS-STATUS
                         NOT AT END
                           IF COMPTE-NUM = WS-LD-COMPTE
                             MOVE COMPTE-SOLDE TO WS-SOLDE-AVANT
                             IF WS-LD-OP = "Depot"
                               ADD WS-LD-MONTANT TO COMPTE-SOLDE
                             ELSE
                               SUBTRACT WS-LD-MONTANT
                                 FROM COMPTE-SOLDE
                             END-IF
                             REWRITE COMPTE-RECORD
                             MOVE "Y" TO WS-FOUND
                           END-IF
                       END-READ
                     END-PERFORM
                     CLOSE COMPTE-FILE

      *              Credit compte destination si Virement
                     IF WS-LD-OP = "Virement"
                       MOVE "N" TO WS-FOUND
                       MOVE "00" TO WS-STATUS
                       OPEN I-O COMPTE-FILE
                       PERFORM UNTIL WS-STATUS = "10"
                           OR WS-FOUND = "Y"
                         READ COMPTE-FILE
                           AT END
                             MOVE "10" TO WS-STATUS
                           NOT AT END
                             IF COMPTE-NUM = WS-LD-DEST
                               ADD WS-LD-MONTANT TO COMPTE-SOLDE
                               REWRITE COMPTE-RECORD
                               MOVE "Y" TO WS-FOUND
                             END-IF
                         END-READ
                       END-PERFORM
                       CLOSE COMPTE-FILE
                     END-IF

      *              Ecriture transaction
                     OPEN EXTEND TRANSACTION-FILE
                     MOVE WS-COUNTER    TO WS-TRANS-ID
                     MOVE WS-LD-COMPTE  TO WS-TRANS-COMPTE
                     MOVE WS-TODAY      TO WS-TRANS-DATE
                     MOVE WS-LD-OP      TO WS-TRANS-TYPE
                     MOVE WS-LD-MONTANT TO WS-TRANS-MONTANT
                     IF WS-LD-OP = "Virement"
                       MOVE WS-LD-DEST TO WS-TRANS-DEST
                     ELSE
                       MOVE ZERO TO WS-TRANS-DEST
                     END-IF
                     MOVE WS-TRANSACTION TO TRANSACTION-RECORD
                     WRITE TRANSACTION-RECORD
                     CLOSE TRANSACTION-FILE
                     ADD 1 TO WS-COUNTER

                     ADD 1 TO WS-LOT-NB-OK
                     ADD WS-LD-MONTANT TO WS-LOT-MNT-OK

                     IF WS-LD-OP = "Retrait"
                        OR WS-LD-OP = "Virement"
                       MOVE WS-LD-COMPTE  TO WS-INPUT-COMPTE
                       MOVE WS-LD-MONTANT TO WS-INPUT-MONTANT
                       PERFORM CONTROLE-FRAUDE
                     END-IF
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.

           CLOSE LOT-FILE.
           CLOSE REJET-FILE.

      *    Compte-rendu a l'ecran
           DISPLAY "=== Compte-rendu du lot " WS-LOT-NUM " ===".
           DISPLAY "Date du lot     : " WS-LOT-DATE.
           DISPLAY "Lignes lues (D) : " WS-LOT-NB-D.
           DISPLAY "Acceptees       : " WS-LOT-NB-OK.
           DISPLAY "Rejetees        : " WS-LOT-NB-REJET.
           MOVE WS-LOT-MNT-OK TO WS-LOT-MNT-AFF.
           DISPLAY "Montant accepte : "
               FUNCTION TRIM(WS-LOT-MNT-AFF).
           MOVE WS-LOT-MNT-REJET TO WS-LOT-MNT-AFF.
           DISPLAY "Montant rejete  : "
               FUNCTION TRIM(WS-LOT-MNT-AFF).
           COMPUTE WS-LOT-VERIF-NB =
               WS-LOT-NB-OK + WS-LOT-NB-REJET.
           IF WS-LOT-VERIF-NB = WS-LOT-NB-ANNONCE
               DISPLAY "Controle OK : " WS-LOT-NB-OK " + "
                   WS-LOT-NB-REJET " = " WS-LOT-NB-ANNONCE
           ELSE
               DISPLAY "Controle ECART : " WS-LOT-NB-OK " + "
                   WS-LOT-NB-REJET " <> " WS-LOT-NB-ANNONCE
           END-IF.
           IF WS-LOT-NB-REJET > 0
               DISPLAY "Rejets enregistres dans rejets.txt"
           END-IF.

      *    Enregistrement dans lots.dat
           MOVE "00" TO WS-STATUS.
           OPEN EXTEND LOTS-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               MOVE WS-LOT-NUM      TO LOTS-NUM
               MOVE WS-LOT-DATE     TO LOTS-DATE
               MOVE WS-LOT-NB-OK    TO LOTS-NB-OK
               MOVE WS-LOT-NB-REJET TO LOTS-NB-REJET
               WRITE LOTS-RECORD
               CLOSE LOTS-FILE
           END-IF.

      *    Journalisation
           MOVE "LOT TRAITE" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "Lot=" DELIMITED BY SIZE
               WS-LOT-NUM DELIMITED BY SIZE
               " OK=" DELIMITED BY SIZE
               WS-LOT-NB-OK DELIMITED BY SIZE
               " Rejets=" DELIMITED BY SIZE
               WS-LOT-NB-REJET DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
