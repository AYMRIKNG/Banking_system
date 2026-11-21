           DISPLAY "Recherche d'un client par nom.".
           DISPLAY "Nom (ou partie du nom) : " WITH NO ADVANCING.
           MOVE SPACES TO WS-RECHERCHE.
           ACCEPT WS-RECHERCHE.

           IF WS-RECHERCHE = SPACES
               DISPLAY "Recherche vide."
               EXIT PARAGRAPH
           END-IF.

      *    Recherche sans tenir compte des majuscules
           MOVE FUNCTION UPPER-CASE(WS-RECHERCHE) TO WS-RECHERCHE.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-RECHERCHE))
               TO WS-LONGUEUR.
           MOVE ZEROS TO WS-NB-TROUVES.

           MOVE "00" TO WS-STATUS.
           OPEN INPUT CLIENT-FILE.
           PERFORM UNTIL WS-STATUS = "10"
               READ CLIENT-FILE
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       MOVE FUNCTION UPPER-CASE(CLIENT-NAME)
                           TO WS-NOM-MAJ
                       MOVE ZEROS TO WS-OCCURENCES
                       INSPECT WS-NOM-MAJ TALLYING WS-OCCURENCES
                           FOR ALL WS-RECHERCHE(1:WS-LONGUEUR)
                       IF WS-OCCURENCES > 0
                           DISPLAY "-----------------------------"
                           DISPLAY "ID        : " CLIENT-ID
                           DISPLAY "Nom       : " CLIENT-NAME
                           DISPLAY "Adresse   : " CLIENT-ADDRESS
                           DISPLAY "Telephone : " CLIENT-PHONE
                           ADD 1 TO WS-NB-TROUVES
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE CLIENT-FILE.

           IF WS-NB-TROUVES = 0
               DISPLAY "Aucun client trouve."
           ELSE
               DISPLAY WS-NB-TROUVES " client(s) trouve(s)."
           END-IF.
