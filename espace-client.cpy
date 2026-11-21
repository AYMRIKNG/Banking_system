           DISPLAY "==== ESPACE CLIENT ====".
           DISPLAY "1. Mes comptes".
           DISPLAY "2. Historique d'un compte".
           DISPLAY "3. Effectuer un virement".
           DISPLAY "4. Changer mon PIN".
           DISPLAY "5. Faire opposition sur ma carte".
           DISPLAY "6. Revoquer un mandat SEPA".
           DISPLAY "9. Deconnexion".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT SOUS-CHOIX.

           EVALUATE SOUS-CHOIX
               WHEN 1
                   PERFORM LISTER-COMPTES-CLIENT
               WHEN 2
                   PERFORM HISTORIQUE-TRANSACTIONS
               WHEN 3
                   PERFORM EFFECTUER-TRANSACTION
               WHEN 4
                   MOVE SPACES TO WS-PIN-ANCIEN
                                  WS-PIN-SAISIE
                                  WS-PIN-CONFIRM
                   DISPLAY "Ancien PIN : " WITH NO ADVANCING
                   ACCEPT WS-PIN-ANCIEN

                   MOVE WS-PIN-ANCIEN TO WS-H-TEXTE
                   MOVE WS-CLIENT-CONNECTE TO WS-H-SEL
                   MOVE ZEROS TO WS-H-RESULTAT
                   CALL "HACHAGE" USING WS-HACHAGE
                   MOVE SPACES TO WS-PIN-ANCIEN

                   MOVE "N" TO WS-FOUND
                   MOVE ZEROS TO WS-PIN-HASH-LU
                   MOVE "00" TO WS-STATUS
                   OPEN INPUT PIN-FILE
                   IF WS-STATUS = "00" OR WS-STATUS = "05"
                       PERFORM UNTIL WS-STATUS = "10"
                          OR WS-FOUND = "Y"
                           READ PIN-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                   IF PIN-CLIENT-ID
                                      = WS-CLIENT-CONNECTE
                                       MOVE PIN-HASH TO WS-PIN-HASH-LU
                                       MOVE "Y" TO WS-FOUND
                                   END-IF
                           END-READ
                       END-PERFORM
                       CLOSE PIN-FILE
                   END-IF

                   IF WS-FOUND = "N"
                      OR WS-H-RESULTAT NOT = WS-PIN-HASH-LU
                       DISPLAY "Ancien PIN incorrect."
                       EXIT PARAGRAPH
                   END-IF

                   DISPLAY "Nouveau PIN (4 chiffres) : "
                       WITH NO ADVANCING
                   ACCEPT WS-PIN-SAISIE
                   DISPLAY "Confirmer le nouveau PIN : "
                       WITH NO ADVANCING
                   ACCEPT WS-PIN-CONFIRM

                   IF WS-PIN-SAISIE(1:4) NOT NUMERIC
                      OR WS-PIN-SAISIE(5:16) NOT = SPACES
                       DISPLAY "Le PIN doit contenir "
                           "exactement 4 chiffres."
                       MOVE SPACES TO WS-PIN-SAISIE WS-PIN-CONFIRM
                       EXIT PARAGRAPH
                   END-IF

                   IF WS-PIN-SAISIE NOT = WS-PIN-CONFIRM
                       DISPLAY "Les deux PIN ne correspondent pas."
                       MOVE SPACES TO WS-PIN-SAISIE WS-PIN-CONFIRM
                       EXIT PARAGRAPH
                   END-IF

                   MOVE WS-PIN-SAISIE TO WS-H-TEXTE
                   MOVE WS-CLIENT-CONNECTE TO WS-H-SEL
                   MOVE ZEROS TO WS-H-RESULTAT
                   CALL "HACHAGE" USING WS-HACHAGE
                   MOVE SPACES TO WS-PIN-SAISIE WS-PIN-CONFIRM

                   MOVE "N" TO WS-FOUND
                   MOVE "00" TO WS-STATUS
                   OPEN I-O PIN-FILE
                   IF WS-STATUS = "00" OR WS-STATUS = "05"
                       PERFORM UNTIL WS-STATUS = "10"
                          OR WS-FOUND = "Y"
                           READ PIN-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                   IF PIN-CLIENT-ID
                                      = WS-CLIENT-CONNECTE
                                       MOVE WS-H-RESULTAT TO PIN-HASH
                                       MOVE 0 TO PIN-ESSAIS
                                       MOVE "N" TO PIN-BLOQUE
                                       REWRITE PIN-RECORD
                                       MOVE "Y" TO WS-FOUND
                                   END-IF
                           END-READ
                       END-PERFORM
                       CLOSE PIN-FILE
                   END-IF

                   DISPLAY "PIN modifie avec succes."

                   MOVE "PIN DEFINI" TO WS-JOURNAL-ACTION
                   MOVE SPACES TO WS-JOURNAL-DETAIL
                   STRING "Client " DELIMITED BY SIZE
                       WS-CLIENT-CONNECTE DELIMITED BY SIZE
                       " (client)" DELIMITED BY SIZE
                       INTO WS-JOURNAL-DETAIL
                   END-STRING
                   PERFORM ECRIRE-JOURNAL

               WHEN 5
                   PERFORM OPPOSITION-CARTE
               WHEN 6
                   PERFORM REVOQUER-MANDAT

               WHEN 9
                   MOVE "O" TO WS-DECONNEXION
               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.
