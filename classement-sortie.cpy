           MOVE "N" TO WS-FIN-TRI.
           MOVE ZEROS TO WS-CL-RANG.
           MOVE ZEROS TO WS-CL-TOTAL-GENERAL.

           PERFORM UNTIL WS-FIN-TRI = "O"
               RETURN TRI-FILE
                   AT END
                       MOVE "O" TO WS-FIN-TRI
                   NOT AT END
                       ADD 1 TO WS-CL-RANG
                       ADD TRI-TOTAL TO WS-CL-TOTAL-GENERAL
                       MOVE WS-CL-RANG TO WS-CL-RANG-AFF
                       MOVE TRI-TOTAL TO WS-MONTANT-AFF
                       DISPLAY WS-CL-RANG-AFF
                           " | ID " TRI-CLIENT-ID
                           " | " TRI-NAME
                           " | " TRI-NB-COMPTES " cpt"
                           " | " WS-MONTANT-AFF
               END-RETURN
           END-PERFORM.

           IF WS-CL-RANG = 0
               DISPLAY "Aucun client trouve."
           ELSE
               MOVE WS-CL-TOTAL-GENERAL TO WS-MONTANT-AFF
               DISPLAY "Total general : " WS-MONTANT-AFF
           END-IF.
