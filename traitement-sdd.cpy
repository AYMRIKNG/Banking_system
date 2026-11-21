      *    Traitement d'un fichier de prelevements SEPA (SDD)
           MOVE SPACES TO WS-SDD-NOM.
           DISPLAY "Nom du fichier de prelevements : "
               WITH NO ADVANCING.
           ACCEPT WS-SDD-NOM.

           MOVE ZEROS TO WS-SDD-NB-LIGNES
                         WS-SDD-NB-E
                         WS-SDD-NB-D
                         WS-SDD-NB-F
                         WS-SDD-DATE
                         WS-SDD-NB-ANNONCE
                         WS-SDD-F-NB
                         WS-SDD-F-TOTAL
                         WS-SDD-SOMME-D.
           MOVE SPACES TO WS-SDD-DERNIER-TYPE
                          WS-SDD-ICS
                          WS-SDD-MOTIF.

      *    Phase 1 : ouverture et controle de structure
           MOVE "00" TO WS-STATUS.
           OPEN INPUT SDD-IN-FILE.
           IF WS-STATUS NOT = "00"
               MOVE "Fichier prelevements introuvable ou illisible"
                   TO WS-SDD-MOTIF
               DISPLAY "Fichier SDD refuse : "
                   FUNCTION TRIM(WS-SDD-MOTIF)
               MOVE "SDD REFUSE" TO WS-JOURNAL-ACTION
               MOVE SPACES TO WS-JOURNAL-DETAIL
               STRING "Motif=" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-SDD-MOTIF) DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
               PERFORM ECRIRE-JOURNAL
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO WS-SDD-EOF.
           PERFORM UNTIL WS-SDD-EOF = "O"
             READ SDD-IN-FILE
               AT END
                 MOVE "O" TO WS-SDD-EOF
               NOT AT END
                 MOVE SDD-IN-RECORD TO WS-SDD-LIGNE
                 ADD 1 TO WS-SDD-NB-LIGNES
                 MOVE WS-SDD-LIGNE(1:1) TO WS-SDD-DERNIER-TYPE
                 IF WS-SDD-NB-LIGNES = 1
                    AND WS-SDD-LIGNE(1:1) NOT = "E"
                   IF WS-SDD-MOTIF = SPACES
                     MOVE "Premiere ligne doit etre E"
                       TO WS-SDD-MOTIF
                   END-IF
                 END-IF
                 EVALUATE WS-SDD-LIGNE(1:1)
                   WHEN "E"
                     ADD 1 TO WS-SDD-NB-E
                     IF WS-SDD-NB-E > 1
                       IF WS-SDD-MOTIF = SPACES
                         MOVE "Plusieurs lignes en-tete E"
                           TO WS-SDD-MOTIF
                       END-IF
                     ELSE
                       IF WS-SE-ICS NOT = SPACES
                          AND WS-SE-DATE IS NUMERIC
                          AND WS-SE-NB IS NUMERIC
                         MOVE FUNCTION UPPER-CASE(WS-SE-ICS)
                           TO WS-SDD-ICS
                         MOVE WS-SE-DATE TO WS-SDD-DATE
                         MOVE WS-SE-NB TO WS-SDD-NB-ANNONCE
                       ELSE
                         IF WS-SDD-MOTIF = SPACES
                           MOVE "En-tete E invalide"
                             TO WS-SDD-MOTIF
                         END-IF
                       END-IF
                     END-IF
                   WHEN "D"
                     ADD 1 TO WS-SDD-NB-D
                     IF WS-SD-MONTANT IS NUMERIC
                       ADD WS-SD-MONTANT TO WS-SDD-SOMME-D
                     ELSE
                       IF WS-SDD-MOTIF = SPACES
                         MOVE "Montant non numerique en ligne D"
                           TO WS-SDD-MOTIF
                       END-IF
                     END-IF
                   WHEN "F"
                     ADD 1 TO WS-SDD-NB-F
                     IF WS-SDD-NB-F > 1
                       IF WS-SDD-MOTIF = SPACES
                         MOVE "Plusieurs lignes fin F"
                           TO WS-SDD-MOTIF
                       END-IF
                     ELSE
                       IF WS-SF-NB IS NUMERIC
                          AND WS-SF-TOTAL IS NUMERIC
                         MOVE WS-SF-NB TO WS-SDD-F-NB
                         MOVE WS-SF-TOTAL TO WS-SDD-F-TOTAL
                       ELSE
                         IF WS-SDD-MOTIF = SPACES
                           MOVE "Ligne fin F non numerique"
                             TO WS-SDD-MOTIF
                         END-IF
                       END-IF
                     END-IF
                   WHEN OTHER
                     IF WS-SDD-MOTIF = SPACES
                       MOVE "Type de ligne inconnu"
                         TO WS-SDD-MOTIF
                     END-IF
                 END-EVALUATE
             END-READ
           END-PERFORM.
           CLOSE SDD-IN-FILE.

           IF WS-SDD-MOTIF = SPACES AND WS-SDD-NB-LIGNES = 0
               MOVE "Fichier prelevements vide" TO WS-SDD-MOTIF
           END-IF.
           IF WS-SDD-MOTIF = SPACES AND WS-SDD-NB-E = 0
               MOVE "Ligne en-tete E manquante" TO WS-SDD-MOTIF
           END-IF.
           IF WS-SDD-MOTIF = SPACES AND WS-SDD-NB-F = 0
               MOVE "Ligne de fin F manquante" TO WS-SDD-MOTIF
           END-IF.
           IF WS-SDD-MOTIF = SPACES
              AND WS-SDD-DERNIER-TYPE NOT = "F"
               MOVE "Derniere ligne doit etre F" TO WS-SDD-MOTIF
           END-IF.
           IF WS-SDD-MOTIF = SPACES
              AND WS-SDD-NB-D NOT = WS-SDD-NB-ANNONCE
               MOVE "Nombre de lignes D different de E"
                   TO WS-SDD-MOTIF
           END-IF.
           IF WS-SDD-MOTIF = SPACES
              AND WS-SDD-NB-D NOT = WS-SDD-F-NB
               MOVE "Nombre de lignes D different de F"
                   TO WS-SDD-MOTIF
           END-IF.
           IF WS-SDD-MOTIF = SPACES
              AND WS-SDD-SOMME-D NOT = WS-SDD-F-TOTAL
               MOVE "Somme des montants D differente du total F"
                   TO WS-SDD-MOTIF
           END-IF.

      *    Verification anti-doublon dans sdd.dat (ICS + date)
           IF WS-SDD-MOTIF = SPACES
               MOVE "N" TO WS-FOUND
               MOVE "00" TO WS-STATUS
               OPEN INPUT SDD-HIST-FILE
               IF WS-STATUS = "00" OR WS-STATUS = "05"
                   PERFORM UNTIL WS-STATUS = "10" OR WS-FOUND = "Y"
                       READ SDD-HIST-FILE
                           AT END
                               MOVE "10" TO WS-STATUS
                           NOT AT END
                               IF SDDH-ICS = WS-SDD-ICS
                                  AND SDDH-DATE = WS-SDD-DATE
                                   MOVE "Y" TO WS-FOUND
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE SDD-HIST-FILE
               END-IF
               IF WS-FOUND = "Y"
                   MOVE "Fichier SDD deja traite (ICS + date)"
                       TO WS-SDD-MOTIF
               END-IF
           END-IF.

           IF WS-SDD-MOTIF NOT = SPACES
               DISPLAY "Fichier SDD refuse : "
                   FUNCTION TRIM(WS-SDD-MOTIF)
               MOVE "SDD REFUSE" TO WS-JOURNAL-ACTION
               MOVE SPACES TO WS-JOURNAL-DETAIL
               STRING WS-SDD-ICS DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-SDD-MOTIF) DELIMITED BY SIZE
                   INTO WS-JOURNAL-DETAIL
               END-STRING
               PERFORM ECRIRE-JOURNAL
               EXIT PARAGRAPH
           END-IF.

      *    Phase 2 : application ligne par ligne
           MOVE ZEROS TO WS-SDD-NUM-LIGNE
                         WS-SDD-NB-OK
                         WS-SDD-NB-REJET
                         WS-SDD-MNT-OK
                         WS-SDD-MNT-REJET.

           MOVE "00" TO WS-STATUS.
           OPEN OUTPUT REJET-SDD-FILE.
           MOVE "00" TO WS-STATUS.
           OPEN INPUT SDD-IN-FILE.
           MOVE "N" TO WS-SDD-EOF.

           PERFORM UNTIL WS-SDD-EOF = "O"
             READ SDD-IN-FILE
               AT END
                 MOVE "O" TO WS-SDD-EOF
               NOT AT END
                 ADD 1 TO WS-SDD-NUM-LIGNE
                 MOVE SDD-IN-RECORD TO WS-SDD-LIGNE
                 IF WS-SDD-LIGNE(1:1) = "D"
                   MOVE SPACES TO WS-SDD-CODE-REJET
                                  WS-SDD-LIB-REJET
                   MOVE FUNCTION UPPER-CASE(WS-SD-RUM)
                     TO WS-SDD-RUM-CUR
                   MOVE FUNCTION UPPER-CASE(WS-SD-IBAN)
                     TO WS-SDD-IBAN-CUR

      *            MD01 : recherche mandat (RUM + ICS)
                   MOVE "N" TO WS-FOUND
                   MOVE SPACES TO WS-MD-ICS
                                  WS-MD-TYPE
                                  WS-MD-STATUT
                   MOVE ZEROS TO WS-MD-COMPTE
                                 WS-MD-MAX
                                 WS-MD-DERNIER
                   MOVE "00" TO WS-STATUS
                   OPEN INPUT MANDAT-FILE
                   IF WS-STATUS = "00" OR WS-STATUS = "05"
                     PERFORM UNTIL WS-STATUS = "10"
                         OR WS-FOUND = "Y"
                       READ MANDAT-FILE
                         AT END
                           MOVE "10" TO WS-STATUS
                         NOT AT END
                           IF MANDAT-RUM = WS-SDD-RUM-CUR
                             MOVE "Y" TO WS-FOUND
                             MOVE MANDAT-ICS     TO WS-MD-ICS
                             MOVE MANDAT-COMPTE  TO WS-MD-COMPTE
                             MOVE MANDAT-MAX     TO WS-MD-MAX
                             MOVE MANDAT-TYPE    TO WS-MD-TYPE
                             MOVE MANDAT-STATUT  TO WS-MD-STATUT
                             MOVE MANDAT-DERNIER TO WS-MD-DERNIER
                           END-IF
                       END-READ
                     END-PERFORM
                     CLOSE MANDAT-FILE
                   END-IF

                   IF WS-FOUND = "N"
                      OR WS-MD-ICS NOT = WS-SDD-ICS
                     MOVE "MD01" TO WS-SDD-CODE-REJET
                     MOVE "Pas de mandat valide"
                       TO WS-SDD-LIB-REJET
                   END-IF

      *            MS02 : mandat revoque ou ponctuel consomme
                   IF WS-SDD-CODE-REJET = SPACES
                      AND WS-MD-STATUT NOT = "A"
                     MOVE "MS02" TO WS-SDD-CODE-REJET
                     MOVE "Mandat revoque ou consomme"
                       TO WS-SDD-LIB-REJET
                   END-IF

      *            AC01 : IBAN invalide ou compte different
                   IF WS-SDD-CODE-REJET = SPACES
                     MOVE "V" TO WS-IBAN-MODE
                     MOVE ZEROS TO WS-IBAN-COMPTE
                     MOVE SPACES TO WS-IBAN-TEXTE
                                    WS-IBAN-AFFICHE
                     MOVE WS-SDD-IBAN-CUR TO WS-IBAN-TEXTE(1:27)
                     MOVE ZERO TO WS-IBAN-RETOUR
                     CALL "CALCUL-IBAN" USING WS-CALCUL-IBAN
                     IF WS-IBAN-RETOUR NOT = 0
                        OR WS-IBAN-COMPTE NOT = WS-MD-COMPTE
                       MOVE "AC01" TO WS-SDD-CODE-REJET
                       MOVE "IBAN incorrect"
                         TO WS-SDD-LIB-REJET
                     END-IF
                   END-IF

      *            AC04 : compte cloture (ou introuvable)
                   IF WS-SDD-CODE-REJET = SPACES
                     MOVE "N" TO WS-FOUND
                     MOVE SPACES TO WS-SDD-TYPE-CPT
                     MOVE ZEROS TO WS-SDD-SOLDE-CPT
                     MOVE "00" TO WS-STATUS
                     OPEN INPUT COMPTE-FILE
                     PERFORM UNTIL WS-STATUS = "10"
                         OR WS-FOUND = "Y"
                       READ COMPTE-FILE
                         AT END
                           MOVE "10" TO WS-STATUS
                         NOT AT END
                           IF COMPTE-NUM = WS-MD-COMPTE
                             MOVE "Y" TO WS-FOUND
                             MOVE COMPTE-TYPE  TO WS-SDD-TYPE-CPT
                             MOVE COMPTE-SOLDE TO WS-SDD-SOLDE-CPT
                           END-IF
                       END-READ
                     END-PERFORM
                     CLOSE COMPTE-FILE
                     IF WS-FOUND = "N"
                        OR WS-SDD-TYPE-CPT = "Cloture"
                       MOVE "AC04" TO WS-SDD-CODE-REJET
                       MOVE "Compte cloture"
                         TO WS-SDD-LIB-REJET
                     END-IF
                   END-IF

      *            AM05 : doublon meme RUM meme date d'echeance
                   IF WS-SDD-CODE-REJET = SPACES
                      AND WS-MD-DERNIER = WS-SDD-DATE
                     MOVE "AM05" TO WS-SDD-CODE-REJET
                     MOVE "Doublon meme RUM meme date"
                       TO WS-SDD-LIB-REJET
                   END-IF

      *            MD07 : montant superieur au maximum du mandat
                   IF WS-SDD-CODE-REJET = SPACES
                      AND WS-MD-MAX > 0
                      AND WS-SD-MONTANT > WS-MD-MAX
                     MOVE "MD07" TO WS-SDD-CODE-REJET
                     MOVE "Montant superieur au max mandat"
                       TO WS-SDD-LIB-REJET
                   END-IF

      *            AM04 : provision insuffisante (regle decouvert)
                   IF WS-SDD-CODE-REJET = SPACES
                     IF WS-SDD-TYPE-CPT = "Epargne"
                       MOVE ZEROS TO WS-DEC-PLAFOND
                     ELSE
                       MOVE WS-MD-COMPTE TO WS-DEC-COMPTE
                       PERFORM LIRE-DECOUVERT
                     END-IF
                     COMPUTE WS-DEC-SOLDE-PROJ =
                       WS-SDD-SOLDE-CPT - WS-SD-MONTANT
                     COMPUTE WS-DEC-LIMITE = 0 - WS-DEC-PLAFOND
                     IF WS-DEC-SOLDE-PROJ < WS-DEC-LIMITE
                       MOVE "AM04" TO WS-SDD-CODE-REJET
                       MOVE "Provision insuffisante"
                         TO WS-SDD-LIB-REJET
                     END-IF
                   END-IF

      *            Rejet ou application
                   IF WS-SDD-CODE-REJET NOT = SPACES
                     ADD 1 TO WS-SDD-NB-REJET
                     ADD WS-SD-MONTANT TO WS-SDD-MNT-REJET
                     MOVE WS-SD-MONTANT TO WS-MONTANT-AFF
                     MOVE SPACES TO REJET-SDD-LINE
                     STRING WS-SDD-NUM-LIGNE DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       WS-SDD-CODE-REJET DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-SDD-LIB-REJET)
                         DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-SDD-RUM-CUR)
                         DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-MONTANT-AFF)
                         DELIMITED BY SIZE
                       INTO REJET-SDD-LINE
                     END-STRING
                     WRITE REJET-SDD-LINE
                   ELSE
      *              Debit du compte
                     MOVE "N" TO WS-FOUND
                     MOVE "00" TO WS-STATUS
                     OPEN I-O COMPTE-FILE
                     PERFORM UNTIL WS-STATUS = "10"
                         OR WS-FOUND = "Y"
                       READ COMPTE-FILE
                         AT END
                           MOVE "10" TO WS-STATUS
                         NOT AT END
                           IF COMPTE-NUM = WS-MD-COMPTE
                             MOVE COMPTE-SOLDE TO WS-SOLDE-AVANT
                             SUBTRACT WS-SD-MONTANT
                               FROM COMPTE-SOLDE
                             REWRITE COMPTE-RECORD
                             MOVE "Y" TO WS-FOUND
                           END-IF
                       END-READ
                     END-PERFORM
                     CLOSE COMPTE-FILE

      *              Mise a jour du mandat (dernier + statut C si U)
                     MOVE "N" TO WS-FOUND
                     MOVE "00" TO WS-STATUS
                     OPEN I-O MANDAT-FILE
                     IF WS-STATUS = "00" OR WS-STATUS = "05"
                       PERFORM UNTIL WS-STATUS = "10"
                           OR WS-FOUND = "Y"
                         READ MANDAT-FILE
                           AT END
                             MOVE "10" TO WS-STATUS
                           NOT AT END
                             IF MANDAT-RUM = WS-SDD-RUM-CUR
                               MOVE WS-SDD-DATE TO MANDAT-DERNIER
                               IF MANDAT-TYPE = "U"
                                 MOVE "C" TO MANDAT-STATUT
                               END-IF
                               REWRITE MANDAT-RECORD
                               MOVE "Y" TO WS-FOUND
                             END-IF
                         END-READ
                       END-PERFORM
                       CLOSE MANDAT-FILE
                     END-IF

      *              Ecriture transaction Prelev
                     OPEN EXTEND TRANSACTION-FILE
                     MOVE WS-COUNTER    TO WS-TRANS-ID
                     MOVE WS-MD-COMPTE  TO WS-TRANS-COMPTE
                     MOVE WS-TODAY      TO WS-TRANS-DATE
                     MOVE "Prelev"      TO WS-TRANS-TYPE
                     MOVE WS-SD-MONTANT TO WS-TRANS-MONTANT
                     MOVE ZERO          TO WS-TRANS-DEST
                     MOVE WS-TRANSACTION TO TRANSACTION-RECORD
                     WRITE TRANSACTION-RECORD
                     CLOSE TRANSACTION-FILE
                     ADD 1 TO WS-COUNTER

                     ADD 1 TO WS-SDD-NB-OK
                     ADD WS-SD-MONTANT TO WS-SDD-MNT-OK

                     MOVE WS-MD-COMPTE  TO WS-INPUT-COMPTE
                     MOVE WS-SD-MONTANT TO WS-INPUT-MONTANT
                     PERFORM CONTROLE-FRAUDE
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.

           CLOSE SDD-IN-FILE.
           CLOSE REJET-SDD-FILE.

      *    Compte-rendu a l'ecran
           DISPLAY "=== Compte-rendu SDD " WS-SDD-ICS " ===".
           DISPLAY "Date echeance   : " WS-SDD-DATE.
           DISPLAY "Lignes lues (D) : " WS-SDD-NB-D.
           DISPLAY "Acceptees       : " WS-SDD-NB-OK.
           DISPLAY "Rejetees        : " WS-SDD-NB-REJET.
           MOVE WS-SDD-MNT-OK TO WS-SDD-MNT-AFF.
           DISPLAY "Montant accepte : "
               FUNCTION TRIM(WS-SDD-MNT-AFF).
           MOVE WS-SDD-MNT-REJET TO WS-SDD-MNT-AFF.
           DISPLAY "Montant rejete  : "
               FUNCTION TRIM(WS-SDD-MNT-AFF).
           COMPUTE WS-SDD-VERIF-NB =
               WS-SDD-NB-OK + WS-SDD-NB-REJET.
           IF WS-SDD-VERIF-NB = WS-SDD-NB-ANNONCE
               DISPLAY "Controle OK : " WS-SDD-NB-OK " + "
                   WS-SDD-NB-REJET " = " WS-SDD-NB-ANNONCE
           ELSE
               DISPLAY "Controle ECART : " WS-SDD-NB-OK " + "
                   WS-SDD-NB-REJET " <> " WS-SDD-NB-ANNONCE
           END-IF.
           IF WS-SDD-NB-REJET > 0
               DISPLAY "Rejets enregistres dans rejets-sdd.txt"
           END-IF.

      *    Enregistrement dans sdd.dat
           MOVE "00" TO WS-STATUS.
           OPEN EXTEND SDD-HIST-FILE.
           IF WS-STATUS = "00" OR WS-STATUS = "05"
               MOVE WS-SDD-ICS      TO SDDH-ICS
               MOVE WS-SDD-DATE     TO SDDH-DATE
               MOVE WS-SDD-NB-OK    TO SDDH-NB-OK
               MOVE WS-SDD-NB-REJET TO SDDH-NB-REJET
               WRITE SDD-HIST-RECORD
               CLOSE SDD-HIST-FILE
           END-IF.

      *    Journalisation
           MOVE "SDD TRAITE" TO WS-JOURNAL-ACTION.
           MOVE SPACES TO WS-JOURNAL-DETAIL.
           STRING "ICS=" DELIMITED BY SIZE
               WS-SDD-ICS DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-SDD-DATE DELIMITED BY SIZE
               " OK=" DELIMITED BY SIZE
               WS-SDD-NB-OK DELIMITED BY SIZE
               " Rejets=" DELIMITED BY SIZE
               WS-SDD-NB-REJET DELIMITED BY SIZE
               INTO WS-JOURNAL-DETAIL
           END-STRING.
           PERFORM ECRIRE-JOURNAL.
