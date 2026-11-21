      ******************************************************************
      * SOUS-PROGRAMME: HACHAGE
      * HACHAGE SIMPLE PEDAGOGIQUE (CE N'EST PAS DE LA VRAIE CRYPTO)
      * NE PAS UTILISER EN PRODUCTION POUR SECURISER DES MOTS DE PASSE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HACHAGE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-H                     PIC 9(18).
       77 WS-TEMP                  PIC 9(18).
       77 WS-ORD                   PIC 9(4).
       77 WS-IDX                   PIC 9(2).
       77 WS-MODULUS               PIC 9(10) VALUE 9999999967.

       LINKAGE SECTION.
       01 LK-HACHAGE.
           05 LK-H-TEXTE           PIC X(20).
           05 LK-H-SEL             PIC 9(5).
           05 LK-H-RESULTAT        PIC 9(10).

       PROCEDURE DIVISION USING LK-HACHAGE.

       MAIN-HACHAGE.
           COMPUTE WS-H = LK-H-SEL + 7.

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 20
               IF LK-H-TEXTE(WS-IDX:1) NOT = SPACE
                   COMPUTE WS-ORD =
                       FUNCTION ORD(LK-H-TEXTE(WS-IDX:1))
                   COMPUTE WS-TEMP = WS-H * 131 + WS-ORD
                   COMPUTE WS-H =
                       FUNCTION MOD(WS-TEMP, WS-MODULUS)
               END-IF
           END-PERFORM.

           MOVE WS-H TO LK-H-RESULTAT.
           GOBACK.
