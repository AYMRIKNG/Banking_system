      ******************************************************************
      * SOUS-PROGRAMME: CONVERSION
      * CONVERSION DEVISE / EUR AVEC MARGE BANCAIRE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERSION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
       01 LK-CONVERSION.
           05 LK-CONV-MODE         PIC X.
           05 LK-CONV-TAUX-MOYEN   PIC 9(4)V9(6).
           05 LK-CONV-MONTANT-DEV  PIC 9(9)V99.
           05 LK-CONV-MARGE-PCT    PIC 9V99.
           05 LK-CONV-TAUX-APPL    PIC 9(4)V9(6).
           05 LK-CONV-MONTANT-EUR  PIC 9(9)V99.
           05 LK-CONV-RETOUR       PIC 9.

       PROCEDURE DIVISION USING LK-CONVERSION.

       MAIN-CONVERSION.
           MOVE ZEROS TO LK-CONV-TAUX-APPL.
           MOVE ZEROS TO LK-CONV-MONTANT-EUR.
           MOVE 1 TO LK-CONV-RETOUR.

           IF LK-CONV-TAUX-MOYEN = 0
               GOBACK
           END-IF.

           EVALUATE LK-CONV-MODE
               WHEN "A"
                   COMPUTE LK-CONV-TAUX-APPL ROUNDED =
                       LK-CONV-TAUX-MOYEN
                       * (1 - LK-CONV-MARGE-PCT / 100)
               WHEN "V"
                   COMPUTE LK-CONV-TAUX-APPL ROUNDED =
                       LK-CONV-TAUX-MOYEN
                       * (1 + LK-CONV-MARGE-PCT / 100)
               WHEN OTHER
                   GOBACK
           END-EVALUATE.

           IF LK-CONV-TAUX-APPL = 0
               GOBACK
           END-IF.

           COMPUTE LK-CONV-MONTANT-EUR ROUNDED =
               LK-CONV-MONTANT-DEV / LK-CONV-TAUX-APPL.

           MOVE 0 TO LK-CONV-RETOUR.
           GOBACK.
