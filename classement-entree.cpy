           MOVE "N" TO WS-FIN-CLIENTS.
           OPEN INPUT CLIENT-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Erreur ouverture fichier clients : " WS-STATUS
               EXIT PARAGRAPH
           END-IF.

      *    WS-STATUS sert aussi a compte.dat : drapeau a part
           PERFORM UNTIL WS-FIN-CLIENTS = "O"
               READ CLIENT-FILE
                   AT END
                       MOVE "O" TO WS-FIN-CLIENTS
                   NOT AT END
                       MOVE ZEROS TO WS-CL-TOTAL
                       MOVE ZEROS TO WS-CL-NB-COMPTES
                       MOVE "00" TO WS-STATUS
                       OPEN INPUT COMPTE-FILE
                       IF WS-STATUS = "00"
                           PERFORM UNTIL WS-STATUS = "10"
                               READ COMPTE-FILE
                                   AT END
                                       MOVE "10" TO WS-STATUS
                                   NOT AT END
                                       IF COMPTE-CLIENT-ID = CLIENT-ID
                                          AND COMPTE-TYPE NOT =
                                              "Cloture"
                                           ADD COMPTE-SOLDE
                                               TO WS-CL-TOTAL
                                           ADD 1 TO WS-CL-NB-COMPTES
                                       END-IF
                               END-READ
                           END-PERFORM
                           CLOSE COMPTE-FILE
                       END-IF
                       MOVE WS-CL-TOTAL      TO TRI-TOTAL
                       MOVE CLIENT-ID        TO TRI-CLIENT-ID
                       MOVE CLIENT-NAME      TO TRI-NAME
                       MOVE WS-CL-NB-COMPTES TO TRI-NB-COMPTES
                       RELEASE TRI-RECORD
               END-READ
           END-PERFORM.

           CLOSE CLIENT-FILE.
