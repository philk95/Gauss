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
           SELECT Tabelle ASSIGN TO 'eingabe.txt'
           ORGANIZATION IS line SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
            FD TABELLE.
               01 D-MATRIX-ROW.
                  05 D-MATRIX-VALUE PIC +999.99 OCCURS 100
                     INDEXED BY D.

       WORKING-STORAGE SECTION.
           01 EINGABE-DATEI-EOF PIC X.
           01 E-MATRIX.
              05 E-MATRIX-ROW OCCURS 100 INDEXED BY R.
                 10 E-MATRIX-CLM OCCURS 100 INDEXED BY E.
                    15 E-MATRIX-VALUE PIC S999V99.
           01 MAX-ROWS PIC 99 COMP-3 VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
               PERFORM Vorlauf
               PERFORM Hauptlauf
               PERFORM NACHLAUF
               DISPLAY 'E-Matrix: '
               PERFORM VARYING R FROM 1 BY 1 UNTIL R > MAX-ROWS
                   DISPLAY E-MATRIX-ROW(R)
               END-PERFORM

               CALL "GAUSALGO"
                   USING E-MATRIX, MAX-ROWS
               STOP RUN.
       Vorlauf.
           OPEN INPUT TABELLE
           MOVE SPACES TO EINGABE-DATEI-EOF
           PERFORM EINZELVERARBEITUNG.
       Hauptlauf.
               PERFORM Einzelverarbeitung until EINGABE-DATEI-EOF ="C".

       NACHLAUF.
               CLOSE Tabelle.

       EINZELVERARBEITUNG.
           READ TABELLE INTO D-MATRIX-ROW
               AT END MOVE "C" TO EINGABE-DATEI-EOF
           END-READ
           IF EINGABE-DATEI-EOF NOT EQUAL "C"
              ADD 1 TO MAX-ROWS
              MOVE D-MATRIX-ROW TO E-MATRIX-ROW(MAX-ROWS)
           END-IF.
