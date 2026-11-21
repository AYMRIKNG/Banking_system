           DISPLAY "==== CONNEXION ====".
           DISPLAY "1. Conseiller".
           DISPLAY "2. Client".
           DISPLAY "3. Quitter".
           DISPLAY "Choisissez une option : " WITH NO ADVANCING.
           ACCEPT WS-CHOIX-CONN.

           EVALUATE WS-CHOIX-CONN
               WHEN 1
                   MOVE "N" TO WS-FOUND
                   MOVE "N" TO WS-AUTH-OK
                   MOVE ZEROS TO WS-CONS-HASH-LU
                   MOVE "00" TO WS-STATUS
                   OPEN INPUT CONSEILLER-FILE
                   IF WS-STATUS = "00" OR WS-STATUS = "05"
                       READ CONSEILLER-FILE
                           AT END
                               MOVE "N" TO WS-FOUND
                           NOT AT END
                               IF CONSEILLER-RECORD NOT = SPACES
                                   MOVE "Y" TO WS-FOUND
                                   MOVE CONS-HASH TO WS-CONS-HASH-LU
                               END-IF
                       END-READ
                       CLOSE CONSEILLER-FILE
                   END-IF

                   IF WS-FOUND = "N"
                       DISPLAY "Premier lancement : definir le "
                           "mot de passe conseiller."
                       MOVE SPACES TO WS-MDP-SAISIE WS-MDP-CONFIRM
                       DISPLAY "Mot de passe : " WITH NO ADVANCING
                       ACCEPT WS-MDP-SAISIE
                       DISPLAY "Confirmer : " WITH NO ADVANCING
                       ACCEPT WS-MDP-CONFIRM
                       IF WS-MDP-SAISIE = SPACES
                           DISPLAY "Mot de passe vide refuse."
                           MOVE SPACES TO WS-MDP-SAISIE WS-MDP-CONFIRM
                           EXIT PARAGRAPH
                       END-IF
                       IF WS-MDP-SAISIE NOT = WS-MDP-CONFIRM
                           DISPLAY "Les mots de passe ne "
                               "correspondent pas."
                           MOVE SPACES TO WS-MDP-SAISIE WS-MDP-CONFIRM
                           EXIT PARAGRAPH
                       END-IF
                       MOVE WS-MDP-SAISIE TO WS-H-TEXTE
                       MOVE 0 TO WS-H-SEL
                       MOVE ZEROS TO WS-H-RESULTAT
                       CALL "HACHAGE" USING WS-HACHAGE
                       MOVE SPACES TO WS-MDP-SAISIE WS-MDP-CONFIRM
                       MOVE "00" TO WS-STATUS
                       OPEN OUTPUT CONSEILLER-FILE
                       MOVE WS-H-RESULTAT TO CONS-HASH
                       WRITE CONSEILLER-RECORD
                       CLOSE CONSEILLER-FILE
                       DISPLAY "Mot de passe conseiller enregistre."
                       MOVE "O" TO WS-AUTH-OK
                   ELSE
                       MOVE 1 TO WS-ESSAI-AUTH
                       PERFORM UNTIL WS-ESSAI-AUTH > 3
                          OR WS-AUTH-OK = "O"
                           MOVE SPACES TO WS-MDP-SAISIE
                           DISPLAY "Mot de passe conseiller : "
                               WITH NO ADVANCING
                           ACCEPT WS-MDP-SAISIE
                           MOVE WS-MDP-SAISIE TO WS-H-TEXTE
                           MOVE 0 TO WS-H-SEL
                           MOVE ZEROS TO WS-H-RESULTAT
                           CALL "HACHAGE" USING WS-HACHAGE
                           MOVE SPACES TO WS-MDP-SAISIE
                           IF WS-H-RESULTAT = WS-CONS-HASH-LU
                               MOVE "O" TO WS-AUTH-OK
                           ELSE
                               DISPLAY "Mot de passe incorrect."
                               MOVE "ECHEC CONNEXION"
                                   TO WS-JOURNAL-ACTION
                               MOVE "Conseiller" TO WS-JOURNAL-DETAIL
                               PERFORM ECRIRE-JOURNAL
                               ADD 1 TO WS-ESSAI-AUTH
                           END-IF
                       END-PERFORM
                       IF WS-AUTH-OK NOT = "O"
                           DISPLAY "Trop d'essais."
                           EXIT PARAGRAPH
                       END-IF
                   END-IF

                   MOVE "C" TO WS-MODE-SESSION
                   MOVE ZEROS TO WS-CLIENT-CONNECTE
                   MOVE "N" TO WS-DECONNEXION
                   MOVE "CONNEXION" TO WS-JOURNAL-ACTION
                   MOVE "Conseiller" TO WS-JOURNAL-DETAIL
                   PERFORM ECRIRE-JOURNAL
                   DISPLAY "Connexion conseiller reussie."

                   PERFORM UNTIL WS-DECONNEXION = "O"
                       PERFORM AFFICHER-MENU
                   END-PERFORM

                   MOVE "DECONNEXION" TO WS-JOURNAL-ACTION
                   MOVE "Conseiller" TO WS-JOURNAL-DETAIL
                   PERFORM ECRIRE-JOURNAL
                   MOVE SPACE TO WS-MODE-SESSION
                   DISPLAY "Deconnexion effectuee."

               WHEN 2
                   DISPLAY "ID client : " WITH NO ADVANCING
                   ACCEPT WS-AUTH-CLIENT-ID

                   MOVE "N" TO WS-FOUND
                   MOVE ZEROS TO WS-PIN-HASH-LU
                   MOVE "N" TO WS-PIN-BLOQUE-LU
                   MOVE "00" TO WS-STATUS
                   OPEN INPUT PIN-FILE
                   IF WS-STATUS = "00" OR WS-STATUS = "05"
                       PERFORM UNTIL WS-STATUS = "10"
                          OR WS-FOUND = "Y"
                           READ PIN-FILE
                               AT END
                                   MOVE "10" TO WS-STATUS
                               NOT AT END
                                   IF PIN-CLIENT-ID = WS-AUTH-CLIENT-ID
                                       MOVE "Y" TO WS-FOUND
                                       MOVE PIN-HASH TO WS-PIN-HASH-LU
                                       MOVE PIN-BLOQUE
                                           TO WS-PIN-BLOQUE-LU
                                   END-IF
                           END-READ
                       END-PERFORM
                       CLOSE PIN-FILE
                   END-IF

                   IF WS-FOUND = "N"
                       DISPLAY "PIN non defini, "
                           "contactez votre conseiller."
                       MOVE "ECHEC CONNEXION" TO WS-JOURNAL-ACTION
                       MOVE SPACES TO WS-JOURNAL-DETAIL
                       STRING "Client " DELIMITED BY SIZE
                           WS-AUTH-CLIENT-ID DELIMITED BY SIZE
                           " (PIN non defini)" DELIMITED BY SIZE
                           INTO WS-JOURNAL-DETAIL
                       END-STRING
                       PERFORM ECRIRE-JOURNAL
                       EXIT PARAGRAPH
                   END-IF

                   IF WS-PIN-BLOQUE-LU = "O"
                       DISPLAY "Compte bloque, "
                           "contactez votre conseiller."
                       MOVE "ECHEC CONNEXION" TO WS-JOURNAL-ACTION
                       MOVE SPACES TO WS-JOURNAL-DETAIL
                       STRING "Client " DELIMITED BY SIZE
                           WS-AUTH-CLIENT-ID DELIMITED BY SIZE
                           " (bloque)" DELIMITED BY SIZE
                           INTO WS-JOURNAL-DETAIL
                       END-STRING
                       PERFORM ECRIRE-JOURNAL
                       EXIT PARAGRAPH
                   END-IF

                   MOVE SPACES TO WS-PIN-SAISIE
                   DISPLAY "PIN (4 chiffres) : " WITH NO ADVANCING
                   ACCEPT WS-PIN-SAISIE

                   MOVE WS-PIN-SAISIE TO WS-H-TEXTE
                   MOVE WS-AUTH-CLIENT-ID TO WS-H-SEL
                   MOVE ZEROS TO WS-H-RESULTAT
                   CALL "HACHAGE" USING WS-HACHAGE
                   MOVE SPACES TO WS-PIN-SAISIE

                   IF WS-H-RESULTAT = WS-PIN-HASH-LU
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
                                          = WS-AUTH-CLIENT-ID
                                           MOVE 0 TO PIN-ESSAIS
                                           REWRITE PIN-RECORD
                                           MOVE "Y" TO WS-FOUND
                                       END-IF
                               END-READ
                           END-PERFORM
                           CLOSE PIN-FILE
                       END-IF

                       MOVE "U" TO WS-MODE-SESSION
                       MOVE WS-AUTH-CLIENT-ID TO WS-CLIENT-CONNECTE
                       MOVE "N" TO WS-DECONNEXION
                       MOVE "CONNEXION" TO WS-JOURNAL-ACTION
                       MOVE SPACES TO WS-JOURNAL-DETAIL
                       STRING "Client " DELIMITED BY SIZE
                           WS-CLIENT-CONNECTE DELIMITED BY SIZE
                           INTO WS-JOURNAL-DETAIL
                       END-STRING
                       PERFORM ECRIRE-JOURNAL
                       DISPLAY "Connexion client reussie."

                       PERFORM UNTIL WS-DECONNEXION = "O"
                           PERFORM MENU-ESPACE-CLIENT
                       END-PERFORM

                       MOVE "DECONNEXION" TO WS-JOURNAL-ACTION
                       MOVE SPACES TO WS-JOURNAL-DETAIL
                       STRING "Client " DELIMITED BY SIZE
                           WS-CLIENT-CONNECTE DELIMITED BY SIZE
                           INTO WS-JOURNAL-DETAIL
                       END-STRING
                       PERFORM ECRIRE-JOURNAL
                       MOVE SPACE TO WS-MODE-SESSION
                       MOVE ZEROS TO WS-CLIENT-CONNECTE
                       DISPLAY "Deconnexion effectuee."
                   ELSE
                       MOVE "N" TO WS-FOUND
                       MOVE "N" TO WS-VENANT-BLOQUE
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
                                          = WS-AUTH-CLIENT-ID
                                           ADD 1 TO PIN-ESSAIS
                                           IF PIN-ESSAIS >= 3
                                               MOVE "O" TO PIN-BLOQUE
                                               MOVE "O"
                                                   TO WS-VENANT-BLOQUE
                                           END-IF
                                           REWRITE PIN-RECORD
                                           MOVE "Y" TO WS-FOUND
                                       END-IF
                               END-READ
                           END-PERFORM
                           CLOSE PIN-FILE
                       END-IF

                       MOVE "ECHEC CONNEXION" TO WS-JOURNAL-ACTION
                       MOVE SPACES TO WS-JOURNAL-DETAIL
                       STRING "Client " DELIMITED BY SIZE
                           WS-AUTH-CLIENT-ID DELIMITED BY SIZE
                           INTO WS-JOURNAL-DETAIL
                       END-STRING
                       PERFORM ECRIRE-JOURNAL

                       IF WS-VENANT-BLOQUE = "O"
                           MOVE "BLOCAGE" TO WS-JOURNAL-ACTION
                           MOVE SPACES TO WS-JOURNAL-DETAIL
                           STRING "Client " DELIMITED BY SIZE
                               WS-AUTH-CLIENT-ID DELIMITED BY SIZE
                               INTO WS-JOURNAL-DETAIL
                           END-STRING
                           PERFORM ECRIRE-JOURNAL
                           DISPLAY "PIN incorrect."
                           DISPLAY "Compte bloque, "
                               "contactez votre conseiller."
                       ELSE
                           DISPLAY "PIN incorrect."
                       END-IF
                   END-IF

               WHEN 3
                   MOVE "O" TO QUITTER

               WHEN OTHER
                   DISPLAY "Option invalide."
           END-EVALUATE.
