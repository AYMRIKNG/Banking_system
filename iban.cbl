      ******************************************************************
      * SOUS-PROGRAMME: CALCUL-IBAN
      * GENERATION ET VALIDATION D'IBAN FRANCAIS (MODULO 97)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCUL-IBAN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-BANQUE-NUM            PIC 9(18) VALUE 30001.
       77 WS-GUICHET-NUM           PIC 9(18) VALUE 1.
       77 WS-COMPTE-NUM            PIC 9(18).
       77 WS-SOMME-RIB             PIC 9(18).
       77 WS-CLE-RIB               PIC 9(2).
       77 WS-CLE-IBAN              PIC 9(2).
       77 WS-CHAINE-29             PIC X(29).
       77 WS-IDX                   PIC 9(2).
       77 WS-CHIFFRE               PIC 9.
       77 WS-RESTE                 PIC 9(4).
       77 WS-LEN                   PIC 9(2).
       77 WS-CAR                   PIC X.
       77 WS-VAL-LETTRE            PIC 9(2).
       77 WS-D1                    PIC 9.
       77 WS-D2                    PIC 9.

       01 WS-BBAN.
           05 WS-BBAN-BANQUE       PIC X(5) VALUE "30001".
           05 WS-BBAN-GUICHET      PIC X(5) VALUE "00001".
           05 WS-BBAN-ZERO         PIC X    VALUE "0".
           05 WS-BBAN-COMPTE       PIC 9(10).
           05 WS-BBAN-CLE-RIB      PIC 9(2).

       LINKAGE SECTION.
       01 LK-IBAN.
           05 LK-IBAN-MODE         PIC X.
           05 LK-IBAN-COMPTE       PIC 9(10).
           05 LK-IBAN-TEXTE        PIC X(34).
           05 LK-IBAN-AFFICHE      PIC X(34).
           05 LK-IBAN-RETOUR       PIC 9.

       PROCEDURE DIVISION USING LK-IBAN.

       MAIN-IBAN.
           MOVE 1 TO LK-IBAN-RETOUR.

           EVALUATE LK-IBAN-MODE
               WHEN "G"
                   PERFORM GENERER-IBAN
               WHEN "V"
                   PERFORM VALIDER-IBAN
               WHEN "E"
                   PERFORM VALIDER-IBAN-ETRANGER
               WHEN OTHER
                   MOVE 1 TO LK-IBAN-RETOUR
           END-EVALUATE.

           GOBACK.

       GENERER-IBAN.
           MOVE 30001 TO WS-BANQUE-NUM.
           MOVE 1 TO WS-GUICHET-NUM.
           MOVE LK-IBAN-COMPTE TO WS-COMPTE-NUM.

           COMPUTE WS-SOMME-RIB =
               89 * WS-BANQUE-NUM
               + 15 * WS-GUICHET-NUM
               + 3 * WS-COMPTE-NUM.
           COMPUTE WS-CLE-RIB =
               97 - FUNCTION MOD(WS-SOMME-RIB, 97).

           MOVE "30001" TO WS-BBAN-BANQUE.
           MOVE "00001" TO WS-BBAN-GUICHET.
           MOVE "0" TO WS-BBAN-ZERO.
           MOVE LK-IBAN-COMPTE TO WS-BBAN-COMPTE.
           MOVE WS-CLE-RIB TO WS-BBAN-CLE-RIB.

           MOVE SPACES TO WS-CHAINE-29.
           STRING WS-BBAN DELIMITED BY SIZE
                  "152700" DELIMITED BY SIZE
                  INTO WS-CHAINE-29
           END-STRING.

           PERFORM CALCULER-MOD97.
           COMPUTE WS-CLE-IBAN = 98 - WS-RESTE.

           MOVE SPACES TO LK-IBAN-TEXTE.
           STRING "FR" DELIMITED BY SIZE
                  WS-CLE-IBAN DELIMITED BY SIZE
                  WS-BBAN DELIMITED BY SIZE
                  INTO LK-IBAN-TEXTE
           END-STRING.

           PERFORM FORMATER-IBAN.
           MOVE 0 TO LK-IBAN-RETOUR.

       VALIDER-IBAN.
           MOVE ZEROS TO LK-IBAN-COMPTE.
           MOVE SPACES TO LK-IBAN-AFFICHE.
           MOVE 1 TO LK-IBAN-RETOUR.

           IF LK-IBAN-TEXTE(1:2) NOT = "FR"
               EXIT PARAGRAPH
           END-IF.

           IF LK-IBAN-TEXTE(28:7) NOT = SPACES
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING WS-IDX FROM 3 BY 1 UNTIL WS-IDX > 27
               IF LK-IBAN-TEXTE(WS-IDX:1) < "0"
                   OR LK-IBAN-TEXTE(WS-IDX:1) > "9"
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

           MOVE SPACES TO WS-CHAINE-29.
           STRING LK-IBAN-TEXTE(5:23) DELIMITED BY SIZE
                  "1527" DELIMITED BY SIZE
                  LK-IBAN-TEXTE(3:2) DELIMITED BY SIZE
                  INTO WS-CHAINE-29
           END-STRING.

           PERFORM CALCULER-MOD97.

           IF WS-RESTE = 1
               MOVE LK-IBAN-TEXTE(16:10) TO LK-IBAN-COMPTE
               PERFORM FORMATER-IBAN
               MOVE 0 TO LK-IBAN-RETOUR
           END-IF.

       CALCULER-MOD97.
           MOVE 0 TO WS-RESTE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 29
               MOVE WS-CHAINE-29(WS-IDX:1) TO WS-CHIFFRE
               COMPUTE WS-RESTE =
                   FUNCTION MOD(WS-RESTE * 10 + WS-CHIFFRE, 97)
           END-PERFORM.

       FORMATER-IBAN.
           MOVE SPACES TO LK-IBAN-AFFICHE.
           STRING LK-IBAN-TEXTE(1:4)  DELIMITED BY SIZE
                  " "                 DELIMITED BY SIZE
                  LK-IBAN-TEXTE(5:4)  DELIMITED BY SIZE
                  " "                 DELIMITED BY SIZE
                  LK-IBAN-TEXTE(9:4)  DELIMITED BY SIZE
                  " "                 DELIMITED BY SIZE
                  LK-IBAN-TEXTE(13:4) DELIMITED BY SIZE
                  " "                 DELIMITED BY SIZE
                  LK-IBAN-TEXTE(17:4) DELIMITED BY SIZE
                  " "                 DELIMITED BY SIZE
                  LK-IBAN-TEXTE(21:4) DELIMITED BY SIZE
                  " "                 DELIMITED BY SIZE
                  LK-IBAN-TEXTE(25:3) DELIMITED BY SIZE
                  INTO LK-IBAN-AFFICHE
           END-STRING.

       VALIDER-IBAN-ETRANGER.
           MOVE ZEROS TO LK-IBAN-COMPTE.
           MOVE SPACES TO LK-IBAN-AFFICHE.
           MOVE 1 TO LK-IBAN-RETOUR.

           MOVE ZEROS TO WS-LEN.
           PERFORM VARYING WS-IDX FROM 34 BY -1
               UNTIL WS-IDX < 1 OR WS-LEN > 0
               IF LK-IBAN-TEXTE(WS-IDX:1) NOT = SPACE
                   MOVE WS-IDX TO WS-LEN
               END-IF
           END-PERFORM.

           IF WS-LEN < 15 OR WS-LEN > 34
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 2
               MOVE LK-IBAN-TEXTE(WS-IDX:1) TO WS-CAR
               IF WS-CAR < "A" OR WS-CAR > "Z"
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

           PERFORM VARYING WS-IDX FROM 3 BY 1 UNTIL WS-IDX > 4
               MOVE LK-IBAN-TEXTE(WS-IDX:1) TO WS-CAR
               IF WS-CAR < "0" OR WS-CAR > "9"
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

           PERFORM VARYING WS-IDX FROM 5 BY 1 UNTIL WS-IDX > WS-LEN
               MOVE LK-IBAN-TEXTE(WS-IDX:1) TO WS-CAR
               IF NOT ((WS-CAR >= "0" AND WS-CAR <= "9")
                       OR (WS-CAR >= "A" AND WS-CAR <= "Z"))
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

           MOVE 0 TO WS-RESTE.
           PERFORM VARYING WS-IDX FROM 5 BY 1 UNTIL WS-IDX > WS-LEN
               MOVE LK-IBAN-TEXTE(WS-IDX:1) TO WS-CAR
               PERFORM AJOUTER-CAR-MOD97
           END-PERFORM.

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 4
               MOVE LK-IBAN-TEXTE(WS-IDX:1) TO WS-CAR
               PERFORM AJOUTER-CAR-MOD97
           END-PERFORM.

           IF WS-RESTE = 1
               MOVE LK-IBAN-TEXTE TO LK-IBAN-AFFICHE
               MOVE 0 TO LK-IBAN-RETOUR
           END-IF.

       AJOUTER-CAR-MOD97.
           IF WS-CAR >= "0" AND WS-CAR <= "9"
               MOVE WS-CAR TO WS-CHIFFRE
               COMPUTE WS-RESTE =
                   FUNCTION MOD(WS-RESTE * 10 + WS-CHIFFRE, 97)
           ELSE
               COMPUTE WS-VAL-LETTRE =
                   FUNCTION ORD(WS-CAR) - FUNCTION ORD("A") + 10
               DIVIDE WS-VAL-LETTRE BY 10
                   GIVING WS-D1 REMAINDER WS-D2
               COMPUTE WS-RESTE =
                   FUNCTION MOD(WS-RESTE * 10 + WS-D1, 97)
               COMPUTE WS-RESTE =
                   FUNCTION MOD(WS-RESTE * 10 + WS-D2, 97)
           END-IF.
