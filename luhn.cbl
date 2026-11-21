      ******************************************************************
      * SOUS-PROGRAMME: LUHN
      * CALCUL (MODE G) ET VALIDATION (MODE V) CHIFFRE DE LUHN 16 CHIF.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LUHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-IDX                   PIC 9(2).
       77 WS-DIGIT                 PIC 9.
       77 WS-CHIFFRE               PIC 9(2).
       77 WS-SOMME                 PIC 9(4).
       77 WS-RESTE                 PIC 9(2).
       77 WS-DOUBLER               PIC X.
       77 WS-VALIDE                PIC X.

       LINKAGE SECTION.
       01 LK-LUHN.
           05 LK-LUHN-MODE         PIC X.
           05 LK-LUHN-NUMERO       PIC X(16).
           05 LK-LUHN-CHIFFRE      PIC 9.
           05 LK-LUHN-RETOUR       PIC 9.

       PROCEDURE DIVISION USING LK-LUHN.

       MAIN-LUHN.
           MOVE 1 TO LK-LUHN-RETOUR.
           MOVE 0 TO LK-LUHN-CHIFFRE.

           EVALUATE LK-LUHN-MODE
               WHEN "G"
                   PERFORM CALCULER-CHIFFRE
               WHEN "V"
                   PERFORM VALIDER-NUMERO
               WHEN OTHER
                   MOVE 1 TO LK-LUHN-RETOUR
           END-EVALUATE.

           GOBACK.

       CALCULER-CHIFFRE.
           MOVE "O" TO WS-VALIDE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 15
               IF LK-LUHN-NUMERO(WS-IDX:1) < "0"
                  OR LK-LUHN-NUMERO(WS-IDX:1) > "9"
                   MOVE "N" TO WS-VALIDE
               END-IF
           END-PERFORM.

           IF WS-VALIDE = "N"
               MOVE 1 TO LK-LUHN-RETOUR
               EXIT PARAGRAPH
           END-IF.

           MOVE ZEROS TO WS-SOMME.
           MOVE "O" TO WS-DOUBLER.
           PERFORM VARYING WS-IDX FROM 15 BY -1 UNTIL WS-IDX < 1
               MOVE LK-LUHN-NUMERO(WS-IDX:1) TO WS-DIGIT
               MOVE WS-DIGIT TO WS-CHIFFRE
               IF WS-DOUBLER = "O"
                   COMPUTE WS-CHIFFRE = WS-CHIFFRE * 2
                   IF WS-CHIFFRE > 9
                       SUBTRACT 9 FROM WS-CHIFFRE
                   END-IF
                   MOVE "N" TO WS-DOUBLER
               ELSE
                   MOVE "O" TO WS-DOUBLER
               END-IF
               ADD WS-CHIFFRE TO WS-SOMME
           END-PERFORM.

           COMPUTE WS-RESTE = FUNCTION MOD(WS-SOMME, 10).
           IF WS-RESTE = 0
               MOVE 0 TO LK-LUHN-CHIFFRE
           ELSE
               COMPUTE LK-LUHN-CHIFFRE = 10 - WS-RESTE
           END-IF.
           MOVE LK-LUHN-CHIFFRE TO LK-LUHN-NUMERO(16:1).
           MOVE 0 TO LK-LUHN-RETOUR.

       VALIDER-NUMERO.
           MOVE "O" TO WS-VALIDE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 16
               IF LK-LUHN-NUMERO(WS-IDX:1) < "0"
                  OR LK-LUHN-NUMERO(WS-IDX:1) > "9"
                   MOVE "N" TO WS-VALIDE
               END-IF
           END-PERFORM.

           IF WS-VALIDE = "N"
               MOVE 1 TO LK-LUHN-RETOUR
               EXIT PARAGRAPH
           END-IF.

           MOVE ZEROS TO WS-SOMME.
           MOVE "N" TO WS-DOUBLER.
           PERFORM VARYING WS-IDX FROM 16 BY -1 UNTIL WS-IDX < 1
               MOVE LK-LUHN-NUMERO(WS-IDX:1) TO WS-DIGIT
               MOVE WS-DIGIT TO WS-CHIFFRE
               IF WS-DOUBLER = "O"
                   COMPUTE WS-CHIFFRE = WS-CHIFFRE * 2
                   IF WS-CHIFFRE > 9
                       SUBTRACT 9 FROM WS-CHIFFRE
                   END-IF
                   MOVE "N" TO WS-DOUBLER
               ELSE
                   MOVE "O" TO WS-DOUBLER
               END-IF
               ADD WS-CHIFFRE TO WS-SOMME
           END-PERFORM.

           MOVE LK-LUHN-NUMERO(16:1) TO WS-DIGIT.
           MOVE WS-DIGIT TO LK-LUHN-CHIFFRE.
           COMPUTE WS-RESTE = FUNCTION MOD(WS-SOMME, 10).
           IF WS-RESTE = 0
               MOVE 0 TO LK-LUHN-RETOUR
           ELSE
               MOVE 1 TO LK-LUHN-RETOUR
           END-IF.
