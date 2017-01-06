      ******************************************************************
      * Author: Björn Lüpschen und Philipp Kohl
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAUSALGO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

           01 MAX-COLS   PIC 99 COMP-3 VALUE 3.

           01 A-MATRIX.
               05 A-MATRIX-ROW OCCURS 4 INDEXED BY AR.
                  10 A-MATRIX-COL OCCURS 4 INDEXED BY AC.
                       15 A-MATRIX-VALUE PIC -ZZ9.9999.
                       15 FILLER         PIC XX VALUE ' '.

           01 MATRIX-TEMP-ROWCHANGER.
               05 MATRIX-ROW-CHANGER.
                10 EX-MATRIX-VALUE PIC S999V9(10) COMP-3 OCCURS 1 TO 100
                DEPENDING ON MAX-COLS.

           01 I-COLUMN PIC 99 COMP-3.
           01 TEMP-MAX PIC S999V9(10) COMP-3.

           01 MAX-ROW-INDEX PIC 99 COMP-3.
           01 TEMP-MAX-INDEX PIC 99 COMP-3.

           01 INDEX-ROW PIC 99 COMP-3.
           01 INDEX-COL PIC 99 COMP-3.

           01 INDEX-SWAPFIRST PIC 99 COMP-3.
           01 INDEX-SWAPSECOND PIC 99 COMP-3.

           01 SUBTRACT-ROW-FACTOR PIC S99V9(10) COMP-3.
           01 DIVIDE-ROW-FACTOR PIC S99V9(10) COMP-3.

           01 PRINT-VALUE  PIC +999.99.

           77 DEBUG            PIC 9.
               88 DEBUG-ON     VALUE 0.
               88 DEBUG-OFF    VALUE 1.

       LINKAGE SECTION.
           01 MATRIX.
              05 MATRIX-ROW OCCURS 100 INDEXED BY R.
                 10 MATRIX-CLM OCCURS 100 INDEXED BY C.
                    15 MATRIX-VALUE            PIC S999V9(10) COMP-3.

           01 MAX-ROWS                         PIC 99 COMP-3.
       PROCEDURE DIVISION
                   USING MATRIX,MAX-ROWS.
       MAIN-PROCEDURE.
           COMPUTE MAX-COLS = MAX-ROWS + 1

           SET DEBUG-OFF TO TRUE

           DISPLAY 'Ausgangs Matrix: '
           PERFORM PRINT

            PERFORM VARYING I-COLUMN
                    FROM 1 BY 1
                    UNTIL I-COLUMN > MAX-COLS - 1
                    PERFORM FIND-MAX-ELEMENT-IN-COLUMN
                    MOVE TEMP-MAX TO PRINT-VALUE

                    IF DEBUG-ON
                    DISPLAY 'Maximun in Spalte ' I-COLUMN
                    ' betreagt ' PRINT-VALUE
                    END-IF

                    MOVE I-COLUMN TO INDEX-SWAPFIRST
                    MOVE TEMP-MAX-INDEX TO INDEX-SWAPSECOND
                    PERFORM SWAP-ROWS

                    IF DEBUG-ON
                       PERFORM PRINT
                       DISPLAY ' '
                    END-IF

                    PERFORM SUBTRACT-ROWS

                    IF DEBUG-ON
                       DISPLAY 'Zwischenergebnis'
                       PERFORM PRINT
                       DISPLAY ' '
                    END-IF


            END-PERFORM

            PERFORM DIVIDE-ROWS
            DISPLAY 'Ergebnis:'
            PERFORM PRINT
            EXIT PROGRAM.

       DIVIDE-ROWS.
           PERFORM VARYING R FROM MAX-ROWS BY -1 UNTIL R < 1
                   AFTER C FROM MAX-COLS BY -1 UNTIL C < 1
                       COMPUTE MATRIX-VALUE(R,C) =
                       MATRIX-VALUE(R,C) / MATRIX-VALUE(R,R)
           END-PERFORM
       .

       SUBTRACT-ROWS.
           PERFORM VARYING R FROM 1 BY 1 UNTIL R > MAX-ROWS

           IF R NOT EQUAL I-COLUMN
               COMPUTE SUBTRACT-ROW-FACTOR = MATRIX-VALUE(R, I-COLUMN)
                                   / MATRIX-VALUE(I-COLUMN, I-COLUMN)
               PERFORM VARYING C FROM 1 BY 1 UNTIL C > MAX-COLS
                   COMPUTE MATRIX-VALUE(R, C) = MATRIX-VALUE(R,C)
                   - (SUBTRACT-ROW-FACTOR * MATRIX-VALUE(I-COLUMN,C))
               END-PERFORM
           END-IF
           END-PERFORM
           .
       FIND-MAX-ELEMENT-IN-COLUMN.
           MOVE 0 TO TEMP-MAX
           PERFORM VARYING R
                   FROM 1 BY 1
                   UNTIL R > MAX-ROWS
                   IF MATRIX-VALUE(R, I-COLUMN)  < 0
                       IF MATRIX-VALUE(R, I-COLUMN)*-1 > TEMP-MAX
                           COMPUTE TEMP-MAX =
                                   MATRIX-VALUE(R, I-COLUMN)
                           MOVE R TO TEMP-MAX-INDEX
                       END-IF
                   ELSE
                       IF MATRIX-VALUE(R, I-COLUMN) > TEMP-MAX
                           MOVE MATRIX-VALUE(R, I-COLUMN) TO TEMP-MAX
                           MOVE R TO TEMP-MAX-INDEX
                   END-IF
           END-PERFORM
           .

       PRINT.
           PERFORM VARYING INDEX-ROW
                   FROM 1 BY 1 UNTIL INDEX-ROW > MAX-ROWS
                   AFTER INDEX-COL
                   FROM 1 BY 1 UNTIL INDEX-COL > MAX-COLS
                   MOVE MATRIX-VALUE(INDEX-ROW, INDEX-COL) TO
                        A-MATRIX-VALUE(INDEX-ROW, INDEX-COL)
           END-PERFORM

           PERFORM VARYING INDEX-ROW
                   FROM 1 BY 1 UNTIL INDEX-ROW > MAX-ROWS
                   DISPLAY A-MATRIX-ROW(INDEX-ROW)
           END-PERFORM
           .

       SWAP-ROWS.
           IF DEBUG-ON
               DISPLAY 'Vertausche Zeile ' INDEX-SWAPFIRST ' mit '
                   INDEX-SWAPSECOND
           END-IF
           MOVE MATRIX-ROW(INDEX-SWAPFIRST)
           TO   MATRIX-ROW-CHANGER
           MOVE MATRIX-ROW(INDEX-SWAPSECOND)
           TO   MATRIX-ROW(INDEX-SWAPFIRST)
           MOVE MATRIX-ROW-CHANGER
           TO   MATRIX-ROW(INDEX-SWAPSECOND)
           .
