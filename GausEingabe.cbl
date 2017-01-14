      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAUSSEINGABE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-MATRIX ASSIGN TO 'eingabe.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
            FD FD-MATRIX.
               01 D-N               PIC 999.
               01 D-MATRIX-ROW.
                  05 D-MATRIX-VALUE PIC +999.99
                     OCCURS 1 TO 100 DEPENDING ON NUMBER-OF-COLUMNS.

       WORKING-STORAGE SECTION.
           01 EINGABE-DATEI-EOF PIC X.
           01 NUMBER-OF-COLUMNS   PIC 99 COMP-3.

           01 E-MATRIX.
              05 E-MATRIX-ROW OCCURS 100.
                 10 E-MATRIX-CLM OCCURS 100 TIMES.
                    15 E-MATRIX-VALUE PIC -ZZ9.99.

           01 R-MATRIX.
              COPY "MATRIX.CPY" REPLACING ==#== BY ==R==.

           01 MAX-ROWS PIC 99 COMP-3 VALUE ZERO.
           01 ROW PIC 99 COMP-3.
           01 CLM PIC 99 COMP-3.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
               PERFORM Vorlauf
               PERFORM Hauptlauf
               PERFORM NACHLAUF

               DISPLAY 'E-Matrix: '
               PERFORM VARYING ROW FROM 1 BY 1 UNTIL ROW > MAX-ROWS
                   DISPLAY E-MATRIX-ROW(ROW)
               END-PERFORM

               PERFORM VARYING ROW
                       FROM 1 BY 1 UNTIL ROW > MAX-ROWS
                       AFTER CLM FROM 1 BY 1 UNTIL CLM > MAX-ROWS + 1
                       MOVE E-MATRIX-VALUE(ROW,CLM) TO
                       R-MATRIX-VALUE(ROW, CLM)
               END-PERFORM

               CALL "GAUSALGO"
                   USING R-MATRIX, MAX-ROWS
               STOP RUN.
       Vorlauf.
           OPEN INPUT FD-MATRIX
           MOVE SPACES TO EINGABE-DATEI-EOF

           READ FD-MATRIX INTO D-N
           MOVE D-N TO NUMBER-OF-COLUMNS

           PERFORM EINZELVERARBEITUNG.
       Hauptlauf.
               PERFORM Einzelverarbeitung until EINGABE-DATEI-EOF ="C".

       NACHLAUF.
               CLOSE FD-MATRIX.

       EINZELVERARBEITUNG.
           READ FD-MATRIX INTO D-MATRIX-ROW
               AT END MOVE "C" TO EINGABE-DATEI-EOF
           END-READ
           IF EINGABE-DATEI-EOF NOT EQUAL "C"
              ADD 1 TO MAX-ROWS
              MOVE D-MATRIX-ROW TO E-MATRIX-ROW(MAX-ROWS)
           END-IF.
