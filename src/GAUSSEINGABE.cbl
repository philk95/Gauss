      ******************************************************************
      * Author: Björn Lüpschen
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
               01 D-N                      PIC +999.
               01 D-MATRIX-ROW.
                  05 D-MATRIX-VALUE        PIC +999.99
                     OCCURS 1 TO 100 DEPENDING ON NUMBER-OF-COLUMNS.

       WORKING-STORAGE SECTION.
           01 INPUT-DATA-EOF               PIC X.
           01 NUMBER-OF-COLUMNS            PIC 99 COMP-3.

           01 E-MATRIX.
              05 E-MATRIX-ROW              OCCURS 100.
                 10 E-MATRIX-CLM           OCCURS 100.
                    15 E-MATRIX-VALUE      PIC -ZZ9.99.

           01 R-MATRIX.
              COPY "MATRIX.CPY" REPLACING ==#== BY ==R==.


           01 ERRORS                       PIC 9.
              88 OUT-OF-MEMORY             VALUE 0.
              88 NOT-SPARSE-MATRIX         VALUE 1.

           01 ERRORS-FOUND                 PIC 9.
              88 ERRORS-FOUND-NO           VALUE 0.
              88 ERRORS-FOUND-YES          VALUE 1.

           01 MAX-ROWS                     PIC 99 COMP-3 VALUE ZERO.
           01 ROW                          PIC 99 COMP-3.
           01 CLM                          PIC 99 COMP-3.
           01 NOT-ZERO-COUNTER             PIC 99999 VALUE ZERO.
           01 NUMBER-OF-ELEMENTS           PIC 99999 VALUE ZERO.
           01 NUMBER-OF-ROWS               PIC 99999 VALUE ZERO.
           01 MAX-NUMBER-OF-ELEMENTS       PIC 99999 VALUE ZERO.
           01 ZAHL                         PIC S999V99 COMP-3.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
               PERFORM FORERUN
               PERFORM MAINRUN
               PERFORM LASTRUN

               IF ERRORS-FOUND-YES
                  EVALUATE TRUE
                   WHEN OUT-OF-MEMORY
                           DISPLAY "ERROR: NICHT GENUG SPEICHER"
                   WHEN NOT-SPARSE-MATRIX
                           DISPLAY "ERROR: KEINE DUENN BESETZTE MATRIX"
                   WHEN OTHER
                           DISPLAY "ERROR: UNBEKANNTER FEHLER"
                  END-EVALUATE
               ELSE

      *     Verschieben der Eingabe-Matrix in die Übergabe-Matrix
                   PERFORM VARYING ROW
                           FROM 1 BY 1
                           UNTIL ROW > MAX-ROWS
                           AFTER CLM
                           FROM 1 BY 1
                           UNTIL CLM > MAX-ROWS + 1
                               MOVE E-MATRIX-VALUE(ROW,CLM) TO
                                    R-MATRIX-VALUE(ROW, CLM)
                   END-PERFORM

                   CALL "GAUSSALGO"
                       USING R-MATRIX, MAX-ROWS
               END-IF
               STOP RUN.
       FORERUN.
           OPEN INPUT FD-MATRIX
           MOVE SPACES TO INPUT-DATA-EOF

      *    Lese Spaltenanzahl
           READ FD-MATRIX INTO D-N
               AT END MOVE "C" TO INPUT-DATA-EOF
           END-READ

           MOVE D-N TO NUMBER-OF-COLUMNS

           COMPUTE NUMBER-OF-ROWS = NUMBER-OF-COLUMNS - 1
           COMPUTE NUMBER-OF-ELEMENTS =
                   NUMBER-OF-COLUMNS * NUMBER-OF-ROWS
           COMPUTE MAX-NUMBER-OF-ELEMENTS = NUMBER-OF-ELEMENTS * 0.3

           PERFORM SINGLE-PROCESSING.
       MAINRUN.
               PERFORM SINGLE-PROCESSING until INPUT-DATA-EOF ="C".

       LASTRUN.
               CLOSE FD-MATRIX.

       SINGLE-PROCESSING.
           READ FD-MATRIX INTO D-MATRIX-ROW
               AT END MOVE "C" TO INPUT-DATA-EOF
           END-READ
           PERFORM VARYING ROW FROM 1 BY 1 UNTIL ROW > NUMBER-OF-ROWS
               MOVE D-MATRIX-VALUE(ROW) TO ZAHL
               IF ZAHL NOT EQUALS 0
                    ADD 1 TO NOT-ZERO-COUNTER
                    IF NOT-ZERO-COUNTER > MAX-NUMBER-OF-ELEMENTS
                        SET ERRORS-FOUND-YES TO TRUE
                        SET NOT-SPARSE-MATRIX TO TRUE
                    END-IF
               END-IF
           END-PERFORM
           IF INPUT-DATA-EOF NOT EQUAL "C"
              ADD 1 TO MAX-ROWS
              MOVE D-MATRIX-ROW TO E-MATRIX-ROW(MAX-ROWS)
           END-IF.
