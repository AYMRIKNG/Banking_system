      ******************************************************************
      * SOUS-PROGRAMME: SCORING-CREDIT
      * CALCUL DU TAUX D ENDETTEMENT, SCORE (0-100) ET DECISION CREDIT
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCORING-CREDIT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-ENDETT-CALC           COMP-2.
       77 WS-SCORE-CALC            PIC S9(4) VALUE 100.
       77 WS-MALUS-ENDETT          PIC 9(3) VALUE 0.
       77 WS-MALUS-ANC             PIC 9(3) VALUE 0.
       77 WS-MALUS-ALERTE          PIC 9(3) VALUE 0.
       77 WS-MALUS-DEBIT           PIC 9(3) VALUE 0.
       77 WS-MAX-MALUS             PIC 9(3) VALUE 0.

       LINKAGE SECTION.
       01 LK-SCORING.
           05 LK-SC-REVENUS        PIC 9(9)V99.
           05 LK-SC-CHARGES        PIC 9(9)V99.
           05 LK-SC-MENSUALITE     PIC 9(9)V99.
           05 LK-SC-ANCIENNETE     PIC 9(3).
           05 LK-SC-NB-ALERTES     PIC 9(3).
           05 LK-SC-EPARGNE        PIC 9(9)V99.
           05 LK-SC-MONTANT        PIC 9(9)V99.
           05 LK-SC-NB-DEBITEURS   PIC 9(3).
           05 LK-SC-ENDETTEMENT    PIC 9(3)V99.
           05 LK-SC-SCORE          PIC 9(3).
           05 LK-SC-DECISION       PIC X.
           05 LK-SC-MOTIF          PIC X(40).

       PROCEDURE DIVISION USING LK-SCORING.

       MAIN-SCORING.
           MOVE ZEROS TO LK-SC-ENDETTEMENT
                         LK-SC-SCORE.
           MOVE SPACE TO LK-SC-DECISION.
           MOVE SPACES TO LK-SC-MOTIF.

      *    Regle 1 : aucun revenu regulier -> refus direct
           IF LK-SC-REVENUS = 0
               MOVE "R" TO LK-SC-DECISION
               MOVE "Aucun revenu regulier" TO LK-SC-MOTIF
               GOBACK
           END-IF.

      *    Regle 2 : taux d endettement > 35 pour cent -> refus direct
           COMPUTE WS-ENDETT-CALC =
               ((LK-SC-CHARGES + LK-SC-MENSUALITE)
               / LK-SC-REVENUS) * 100.
           IF WS-ENDETT-CALC > 999.99
               MOVE 999.99 TO LK-SC-ENDETTEMENT
           ELSE
               COMPUTE LK-SC-ENDETTEMENT ROUNDED = WS-ENDETT-CALC
           END-IF.

           IF WS-ENDETT-CALC > 35
               MOVE "R" TO LK-SC-DECISION
               MOVE "Endettement superieur a 35 pour cent"
                   TO LK-SC-MOTIF
               GOBACK
           END-IF.

      *    Regle 3 : calcul du score a partir de 100
           MOVE 100 TO WS-SCORE-CALC.
           MOVE ZEROS TO WS-MALUS-ENDETT
                         WS-MALUS-ANC
                         WS-MALUS-ALERTE
                         WS-MALUS-DEBIT
                         WS-MAX-MALUS.

           IF LK-SC-ENDETTEMENT > 30
               MOVE 25 TO WS-MALUS-ENDETT
           ELSE
               IF LK-SC-ENDETTEMENT > 20
                   MOVE 10 TO WS-MALUS-ENDETT
               END-IF
           END-IF.
           SUBTRACT WS-MALUS-ENDETT FROM WS-SCORE-CALC.

           IF LK-SC-ANCIENNETE < 3
               MOVE 20 TO WS-MALUS-ANC
           ELSE
               IF LK-SC-ANCIENNETE < 12
                   MOVE 10 TO WS-MALUS-ANC
               END-IF
           END-IF.
           SUBTRACT WS-MALUS-ANC FROM WS-SCORE-CALC.

           IF LK-SC-NB-ALERTES >= 3
               MOVE 45 TO WS-MALUS-ALERTE
           ELSE
               COMPUTE WS-MALUS-ALERTE = LK-SC-NB-ALERTES * 15
           END-IF.
           SUBTRACT WS-MALUS-ALERTE FROM WS-SCORE-CALC.

           IF LK-SC-NB-DEBITEURS > 0
               MOVE 10 TO WS-MALUS-DEBIT
           END-IF.
           SUBTRACT WS-MALUS-DEBIT FROM WS-SCORE-CALC.

           IF LK-SC-MONTANT > 0
              AND (LK-SC-EPARGNE * 10) >= LK-SC-MONTANT
               ADD 10 TO WS-SCORE-CALC
           END-IF.

           IF WS-SCORE-CALC < 0
               MOVE 0 TO WS-SCORE-CALC
           END-IF.
           IF WS-SCORE-CALC > 100
               MOVE 100 TO WS-SCORE-CALC
           END-IF.
           MOVE WS-SCORE-CALC TO LK-SC-SCORE.

      *    Regle 4 : decision selon le score
           IF LK-SC-SCORE >= 70
               MOVE "A" TO LK-SC-DECISION
           ELSE
               IF LK-SC-SCORE >= 50
                   MOVE "E" TO LK-SC-DECISION
               ELSE
                   MOVE "R" TO LK-SC-DECISION
               END-IF
           END-IF.

      *    Motif expliquant le plus gros malus
           IF WS-MALUS-ALERTE > WS-MAX-MALUS
               MOVE WS-MALUS-ALERTE TO WS-MAX-MALUS
           END-IF.
           IF WS-MALUS-ENDETT > WS-MAX-MALUS
               MOVE WS-MALUS-ENDETT TO WS-MAX-MALUS
           END-IF.
           IF WS-MALUS-ANC > WS-MAX-MALUS
               MOVE WS-MALUS-ANC TO WS-MAX-MALUS
           END-IF.
           IF WS-MALUS-DEBIT > WS-MAX-MALUS
               MOVE WS-MALUS-DEBIT TO WS-MAX-MALUS
           END-IF.

           EVALUATE TRUE
               WHEN WS-MAX-MALUS = 0
                   MOVE "Dossier favorable" TO LK-SC-MOTIF
               WHEN WS-MALUS-ALERTE = WS-MAX-MALUS
                   MOVE "Alertes fraude ouvertes" TO LK-SC-MOTIF
               WHEN WS-MALUS-ENDETT = WS-MAX-MALUS
                   IF WS-MALUS-ENDETT = 25
                       MOVE "Taux d endettement eleve (> 30%)"
                           TO LK-SC-MOTIF
                   ELSE
                       MOVE "Taux d endettement modere (> 20%)"
                           TO LK-SC-MOTIF
                   END-IF
               WHEN WS-MALUS-ANC = WS-MAX-MALUS
                   IF WS-MALUS-ANC = 20
                       MOVE "Anciennete insuffisante (< 3 mois)"
                           TO LK-SC-MOTIF
                   ELSE
                       MOVE "Anciennete limitee (< 12 mois)"
                           TO LK-SC-MOTIF
                   END-IF
               WHEN OTHER
                   MOVE "Compte debiteur" TO LK-SC-MOTIF
           END-EVALUATE.

           GOBACK.
