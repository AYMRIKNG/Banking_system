           DISPLAY "=== Classement des clients par avoirs ===".

           SORT TRI-FILE
               ON DESCENDING KEY TRI-TOTAL
               INPUT PROCEDURE CLASSEMENT-ENTREE
               OUTPUT PROCEDURE CLASSEMENT-SORTIE.
