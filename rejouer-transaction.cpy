      *    Rejeu de la transaction d'indice WS-CI-I sur WS-TAB-COMPTES
           EVALUATE WS-TT-TYPE(WS-CI-I)
               WHEN "Ouverture"
               WHEN "Depot"
               WHEN "Interet"
               WHEN "IntLivret"
               WHEN "Credit"
                   SET WS-CI-IDX-C TO 1
                   SEARCH WS-TAB-COMPTES
                       AT END
                           CONTINUE
                       WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                           CONTINUE
                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                           = WS-TT-COMPTE(WS-CI-I)
                           ADD WS-TT-MONTANT(WS-CI-I)
                               TO WS-TC-SOLDE-REC(WS-CI-IDX-C)
                   END-SEARCH

               WHEN "Retrait"
               WHEN "Frais"
               WHEN "FraisInter"
               WHEN "Rembourse"
               WHEN "Cloture"
               WHEN "Agios"
               WHEN "Commission"
               WHEN "VirInter"
               WHEN "CB"
               WHEN "Prelev"
                   SET WS-CI-IDX-C TO 1
                   SEARCH WS-TAB-COMPTES
                       AT END
                           CONTINUE
                       WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                           CONTINUE
                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                           = WS-TT-COMPTE(WS-CI-I)
                           SUBTRACT WS-TT-MONTANT(WS-CI-I)
                               FROM WS-TC-SOLDE-REC(WS-CI-IDX-C)
                   END-SEARCH

               WHEN "Virement"
                   SET WS-CI-IDX-C TO 1
                   SEARCH WS-TAB-COMPTES
                       AT END
                           CONTINUE
                       WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                           CONTINUE
                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                           = WS-TT-COMPTE(WS-CI-I)
                           SUBTRACT WS-TT-MONTANT(WS-CI-I)
                               FROM WS-TC-SOLDE-REC(WS-CI-IDX-C)
                   END-SEARCH
                   IF WS-TT-DEST(WS-CI-I) > 0
                       SET WS-CI-IDX-C TO 1
                       SEARCH WS-TAB-COMPTES
                           AT END
                               CONTINUE
                           WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                               CONTINUE
                           WHEN WS-TC-NUM(WS-CI-IDX-C)
                               = WS-TT-DEST(WS-CI-I)
                               ADD WS-TT-MONTANT(WS-CI-I)
                                   TO WS-TC-SOLDE-REC(WS-CI-IDX-C)
                       END-SEARCH
                   END-IF

               WHEN "Report"
                   SET WS-CI-IDX-C TO 1
                   SEARCH WS-TAB-COMPTES
                       AT END
                           CONTINUE
                       WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                           CONTINUE
                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                           = WS-TT-COMPTE(WS-CI-I)
                           MOVE WS-TT-MONTANT(WS-CI-I)
                               TO WS-TC-SOLDE-REC(WS-CI-IDX-C)
                   END-SEARCH

               WHEN "ReportDeb"
                   SET WS-CI-IDX-C TO 1
                   SEARCH WS-TAB-COMPTES
                       AT END
                           CONTINUE
                       WHEN WS-CI-IDX-C > WS-CI-NB-COMPTES
                           CONTINUE
                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                           = WS-TT-COMPTE(WS-CI-I)
                           COMPUTE WS-TC-SOLDE-REC(WS-CI-IDX-C) =
                               0 - WS-TT-MONTANT(WS-CI-I)
                   END-SEARCH

               WHEN "Annulation"
                   MOVE "N" TO WS-FOUND
                   SET WS-CI-IDX-T TO 1
                   SEARCH WS-TAB-TRANS
                       AT END
                           CONTINUE
                       WHEN WS-CI-IDX-T > WS-CI-NB-TRANS
                           CONTINUE
                       WHEN WS-TT-ID(WS-CI-IDX-T)
                           = WS-TT-DEST(WS-CI-I)
                           MOVE "Y" TO WS-FOUND
                           MOVE WS-TT-COMPTE(WS-CI-IDX-T)
                               TO WS-CI-CIBLE-COMPTE
                           MOVE WS-TT-TYPE(WS-CI-IDX-T)
                               TO WS-CI-CIBLE-TYPE
                           MOVE WS-TT-MONTANT(WS-CI-IDX-T)
                               TO WS-CI-CIBLE-MONTANT
                           MOVE WS-TT-DEST(WS-CI-IDX-T)
                               TO WS-CI-CIBLE-DEST
                   END-SEARCH
                   IF WS-FOUND = "N"
                       ADD 1 TO WS-CI-NB-ANOMALIES
                       DISPLAY "Anomalie : cible introuvable ID "
                           WS-TT-ID(WS-CI-I)
                   ELSE
                       EVALUATE WS-CI-CIBLE-TYPE
                           WHEN "Depot"
                               SET WS-CI-IDX-C TO 1
                               SEARCH WS-TAB-COMPTES
                                   AT END
                                       CONTINUE
                                   WHEN WS-CI-IDX-C
                                       > WS-CI-NB-COMPTES
                                       CONTINUE
                                   WHEN WS-TC-NUM(WS-CI-IDX-C)
                                       = WS-CI-CIBLE-COMPTE
                                       SUBTRACT WS-CI-CIBLE-MONTANT
                                           FROM WS-TC-SOLDE-REC(
                                               WS-CI-IDX-C)
                               END-SEARCH
                           WHEN "Retrait"
                               SET WS-CI-IDX-C TO 1
                               SEARCH WS-TAB-COMPTES
                                   AT END
                                       CONTINUE
                                   WHEN WS-CI-IDX-C
                                       > WS-CI-NB-COMPTES
                                       CONTINUE
                                   WHEN WS-TC-NUM(WS-CI-IDX-C)
                                       = WS-CI-CIBLE-COMPTE
                                       ADD WS-CI-CIBLE-MONTANT
                                           TO WS-TC-SOLDE-REC(
                                               WS-CI-IDX-C)
                               END-SEARCH
                           WHEN "Virement"
                               SET WS-CI-IDX-C TO 1
                               SEARCH WS-TAB-COMPTES
                                   AT END
                                       CONTINUE
                                   WHEN WS-CI-IDX-C
                                       > WS-CI-NB-COMPTES
                                       CONTINUE
                                   WHEN WS-TC-NUM(WS-CI-IDX-C)
                                       = WS-CI-CIBLE-COMPTE
                                       ADD WS-CI-CIBLE-MONTANT
                                           TO WS-TC-SOLDE-REC(
                                               WS-CI-IDX-C)
                               END-SEARCH
                               IF WS-CI-CIBLE-DEST > 0
                                   SET WS-CI-IDX-C TO 1
                                   SEARCH WS-TAB-COMPTES
                                       AT END
                                           CONTINUE
                                       WHEN WS-CI-IDX-C
                                           > WS-CI-NB-COMPTES
                                           CONTINUE
                                       WHEN WS-TC-NUM(WS-CI-IDX-C)
                                           = WS-CI-CIBLE-DEST
                                           SUBTRACT WS-CI-CIBLE-MONTANT
                                               FROM WS-TC-SOLDE-REC(
                                                   WS-CI-IDX-C)
                                   END-SEARCH
                               END-IF
                           WHEN OTHER
                               ADD 1 TO WS-CI-NB-ANOMALIES
                               DISPLAY "Anomalie : annulation ID "
                                   WS-TT-ID(WS-CI-I)
                       END-EVALUATE
                   END-IF

               WHEN OTHER
                   ADD 1 TO WS-CI-NB-ANOMALIES
                   DISPLAY "Anomalie : type inconnu ID "
                       WS-TT-ID(WS-CI-I)
           END-EVALUATE.
