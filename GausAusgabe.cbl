      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAUSSAUSGABE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MATRIX-ROW ASSIGN TO 'ausgabe.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
           FD MATRIX-ROW.
           01 MATRIX.
               05 D-MATRIX-ROW
                   OCCURS 1 TO 100 DEPENDING ON NUMBER-OF-COLUMNS.
                   10 D-MATRIX-VALUE        PIC +999.99.
                   10 FILLER                PIC X VALUE ' '.

       WORKING-STORAGE SECTION.
           01 A-MATRIX.
               05 A-MATRIX-ROW OCCURS 4 INDEXED BY AR.
                  10 A-MATRIX-COL OCCURS 4 INDEXED BY AC.
                       15 A-MATRIX-VALUE PIC -ZZ9.9999.
                       15 FILLER         PIC XX VALUE ' '.

           01 INDEX-ROW   PIC 999.
           01 INDEX-COL   PIC 999.
       LINKAGE SECTION.
           01 R-MATRIX.
            COPY "MATRIX.CPY" REPLACING ==#== BY ==R==.
           01 NUMBER-OF-COLUMNS          PIC 99 COMP-3.
           01 MAX-ROWS                   PIC 99 COMP-3.

       PROCEDURE DIVISION
           USING R-MATRIX,NUMBER-OF-COLUMNS, MAX-ROWS.

           MAIN-PROCEDURE.
           OPEN OUTPUT MATRIX-ROW

           PERFORM VARYING INDEX-ROW
                   FROM 1 BY 1
                   UNTIL INDEX-ROW > MAX-ROWS
                   PERFORM VARYING INDEX-COL
                   FROM 1 BY 1
                   UNTIL INDEX-COL > MAX-ROWS + 1
                   MOVE R-MATRIX-VALUE(INDEX-ROW, INDEX-COL)
                   TO D-MATRIX-VALUE(INDEX-COL)
                   END-PERFORM

                   WRITE MATRIX
                   END-WRITE
           END-PERFORM



           CLOSE MATRIX-ROW
           .
           END PROGRAM GAUSSAUSGABE.
