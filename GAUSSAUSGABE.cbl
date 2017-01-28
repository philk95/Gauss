      ******************************************************************
      * Author: Bjoern Luepschen
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAUSSAUSGABE.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MATRIX-ROW ASSIGN TO "ausgabe.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
           FD MATRIX-ROW.
           01 MATRIX.
               05 D-MATRIX-ROW
                   OCCURS 100.
                   10 D-MATRIX-VALUE        PIC -ZZ9.999.
               05 FILL         PIC XX VALUE X'0D0A'.


       WORKING-STORAGE SECTION.
           01 INDEX-ROW   PIC 999.
           01 INDEX-COL   PIC 999.
           01 MAX-ROWS    PIC 99.
       LINKAGE SECTION.
           01 R-MATRIX.
            COPY "MATRIX.CPY" REPLACING ==#== BY ==R==.
           01 NUMBER-OF-COLUMNS                 PIC 99 COMP-3.

       PROCEDURE DIVISION
           USING R-MATRIX, NUMBER-OF-COLUMNS.

           MAIN-PROCEDURE.
           OPEN OUTPUT MATRIX-ROW

           COMPUTE MAX-ROWS = NUMBER-OF-COLUMNS - 1

           PERFORM VARYING INDEX-ROW
                   FROM 1 BY 1
                   UNTIL INDEX-ROW > MAX-ROWS

                   PERFORM VARYING INDEX-COL
                   FROM 1 BY 1
                   UNTIL INDEX-COL > MAX-ROWS + 1
                       MOVE R-MATRIX-VALUE(INDEX-ROW, INDEX-COL)
                       TO D-MATRIX-VALUE(INDEX-COL)

                   END-PERFORM

                   MOVE X'0D0A' TO FILL

                   WRITE MATRIX
                   END-WRITE
           END-PERFORM



           CLOSE MATRIX-ROW
           .
           END PROGRAM GAUSSAUSGABE.
