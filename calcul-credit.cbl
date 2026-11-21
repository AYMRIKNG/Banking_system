      ******************************************************************
      * SOUS-PROGRAMME: CALCUL-CREDIT
      * CALCUL DE MENSUALITE (ANNUITES CONSTANTES) ET AMORTISSEMENT
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCUL-CREDIT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AMORT-FILE ASSIGN TO "amortissement.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-AMORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD AMORT-FILE.
       01 AMORT-LINE               PIC X(80).

       WORKING-STORAGE SECTION.
       77 WS-AMORT-STATUS          PIC XX VALUE "00".
       77 WS-TAUX-MENS             COMP-2.
       77 WS-FACTEUR               COMP-2.
       77 WS-MENS-CALC             COMP-2.
       77 WS-NEG-DUREE             PIC S9(4) COMP.
       77 WS-MOIS                  PIC 9(3).
       77 WS-RESTANT               PIC 9(9)V99.
       77 WS-INT-MOIS              PIC 9(9)V99.
       77 WS-CAP-MOIS              PIC 9(9)V99.
       77 WS-MENS-MOIS             PIC 9(9)V99.

       01 WS-ENTETE.
           05 FILLER               PIC X(4)  VALUE "Mois".
           05 FILLER               PIC X(3)  VALUE " | ".
           05 FILLER               PIC X(12) VALUE "  Mensualite".
           05 FILLER               PIC X(3)  VALUE " | ".
           05 FILLER               PIC X(12) VALUE "    Interets".
           05 FILLER               PIC X(3)  VALUE " | ".
           05 FILLER               PIC X(12) VALUE "     Capital".
           05 FILLER               PIC X(3)  VALUE " | ".
           05 FILLER               PIC X(12) VALUE "  Restant du".

       01 WS-LIGNE.
           05 WS-L-MOIS            PIC Z(3)9.
           05 FILLER               PIC X(3)  VALUE " | ".
           05 WS-L-MENS            PIC Z(8)9.99.
           05 FILLER               PIC X(3)  VALUE " | ".
           05 WS-L-INT             PIC Z(8)9.99.
           05 FILLER               PIC X(3)  VALUE " | ".
           05 WS-L-CAP             PIC Z(8)9.99.
           05 FILLER               PIC X(3)  VALUE " | ".
           05 WS-L-REST            PIC Z(8)9.99.

       LINKAGE SECTION.
       01 LK-CREDIT.
           05 LK-MODE              PIC X.
           05 LK-CAPITAL           PIC 9(9)V99.
           05 LK-TAUX-ANNUEL       PIC 9(2)V99.
           05 LK-DUREE             PIC 9(3).
           05 LK-MENSUALITE        PIC 9(9)V99.
           05 LK-COUT-TOTAL        PIC 9(9)V99.
           05 LK-TOTAL             PIC 9(9)V99.
           05 LK-RETOUR            PIC 9.

       PROCEDURE DIVISION USING LK-CREDIT.

       MAIN-CALCUL.
           MOVE ZEROS TO LK-MENSUALITE
                         LK-COUT-TOTAL
                         LK-TOTAL
                         LK-RETOUR.

           IF LK-CAPITAL = 0 OR LK-DUREE = 0
               MOVE 1 TO LK-RETOUR
               GOBACK
           END-IF.

           MOVE LK-TAUX-ANNUEL TO WS-TAUX-MENS.
           COMPUTE WS-TAUX-MENS = WS-TAUX-MENS / 100 / 12.

           IF WS-TAUX-MENS = 0
               COMPUTE LK-MENSUALITE ROUNDED =
                   LK-CAPITAL / LK-DUREE
           ELSE
               COMPUTE WS-NEG-DUREE = 0 - LK-DUREE
               COMPUTE WS-FACTEUR =
                   (1 + WS-TAUX-MENS) ** WS-NEG-DUREE
               COMPUTE WS-MENS-CALC =
                   LK-CAPITAL * WS-TAUX-MENS / (1 - WS-FACTEUR)
               COMPUTE LK-MENSUALITE ROUNDED = WS-MENS-CALC
           END-IF.

           COMPUTE LK-TOTAL ROUNDED = LK-MENSUALITE * LK-DUREE.
           IF LK-TOTAL > LK-CAPITAL
               COMPUTE LK-COUT-TOTAL = LK-TOTAL - LK-CAPITAL
           ELSE
               MOVE ZERO TO LK-COUT-TOTAL
           END-IF.

           IF LK-MODE = "T"
               PERFORM GENERER-TABLEAU
           END-IF.

           GOBACK.

       GENERER-TABLEAU.
           OPEN OUTPUT AMORT-FILE.
           IF WS-AMORT-STATUS NOT = "00"
               DISPLAY "Erreur ouverture amortissement.txt : "
                   WS-AMORT-STATUS
               EXIT PARAGRAPH
           END-IF.

           DISPLAY WS-ENTETE.
           MOVE WS-ENTETE TO AMORT-LINE.
           WRITE AMORT-LINE.

           MOVE LK-CAPITAL TO WS-RESTANT.

           PERFORM VARYING WS-MOIS FROM 1 BY 1
               UNTIL WS-MOIS > LK-DUREE
               COMPUTE WS-INT-MOIS ROUNDED =
                   WS-RESTANT * WS-TAUX-MENS
               IF WS-MOIS = LK-DUREE
                   MOVE WS-RESTANT TO WS-CAP-MOIS
                   COMPUTE WS-MENS-MOIS =
                       WS-CAP-MOIS + WS-INT-MOIS
                   MOVE ZERO TO WS-RESTANT
               ELSE
                   MOVE LK-MENSUALITE TO WS-MENS-MOIS
                   COMPUTE WS-CAP-MOIS =
                       WS-MENS-MOIS - WS-INT-MOIS
                   IF WS-CAP-MOIS > WS-RESTANT
                       MOVE WS-RESTANT TO WS-CAP-MOIS
                       COMPUTE WS-MENS-MOIS =
                           WS-CAP-MOIS + WS-INT-MOIS
                       MOVE ZERO TO WS-RESTANT
                   ELSE
                       SUBTRACT WS-CAP-MOIS FROM WS-RESTANT
                   END-IF
               END-IF
               MOVE WS-MOIS      TO WS-L-MOIS
               MOVE WS-MENS-MOIS TO WS-L-MENS
               MOVE WS-INT-MOIS  TO WS-L-INT
               MOVE WS-CAP-MOIS  TO WS-L-CAP
               MOVE WS-RESTANT   TO WS-L-REST
               DISPLAY WS-LIGNE
               MOVE WS-LIGNE TO AMORT-LINE
               WRITE AMORT-LINE
           END-PERFORM.

           CLOSE AMORT-FILE.
