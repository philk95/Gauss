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

           01 MAX-COLS   PIC 99 COMP-3.

           01 A-MATRIX.
               05 A-MATRIX-ROW OCCURS 4 INDEXED BY AR.
                  10 A-MATRIX-COL OCCURS 4 INDEXED BY AC.
                       15 A-MATRIX-VALUE PIC -ZZ9.9999.
                       15 FILLER         PIC XX VALUE ' '.


           01 R-MATRIX.
               COPY "MATRIX.CPY" REPLACING ==#== BY ==R==.

           01 MATRIX-TEMP-ROWCHANGER.
               05 MATRIX-ROW-CHANGER.
                10 EX-MATRIX-VALUE PIC S999V9(10) COMP-3 OCCURS 1 TO 100
                DEPENDING ON MAX-COLS.

           01 EPSILON  PIC 9V9(8) COMP-3 VALUE 0.00000001.

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

           01 VALUE-NOT-ZERO-COUNTER PIC 9(10).

           77 DEBUG            PIC 9.
               88 DEBUG-ON     VALUE 0.
               88 DEBUG-OFF    VALUE 1.

       LINKAGE SECTION.
           01 MATRIX.
              COPY "MATRIX.CPY" REPLACING ==#== BY ==E==.

           01 MAX-ROWS                         PIC 99 COMP-3.
       PROCEDURE DIVISION
                   USING MATRIX, MAX-ROWS.
       MAIN-PROCEDURE.
           COMPUTE MAX-COLS = MAX-ROWS + 1
           MOVE MATRIX TO R-MATRIX

           SET DEBUG-ON TO TRUE

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

            PERFORM INTERPRET-RESULT
            EXIT PROGRAM.

       INTERPRET-RESULT.
           PERFORM VARYING R-I-COL
                   FROM 1 BY 1
                   UNTIL R-I-COL > MAX-COLS - 1
                   IF R-MATRIX-VALUE(MAX-ROWS, R-I-COL) NOT EQUAL 0
                      ADD 1 TO VALUE-NOT-ZERO-COUNTER
                   END-IF
           END-PERFORM


       .
       DIVIDE-ROWS.
           PERFORM VARYING R-I-ROW FROM MAX-ROWS BY -1 UNTIL R-I-ROW < 1
                   AFTER R-I-COL FROM MAX-COLS BY -1 UNTIL R-I-COL < 1
                       COMPUTE R-MATRIX-VALUE(R-I-ROW,R-I-COL) =
                       R-MATRIX-VALUE(R-I-ROW,R-I-COL) /
                       R-MATRIX-VALUE(R-I-ROW,R-I-ROW)
           END-PERFORM
       .

       SUBTRACT-ROWS.
           PERFORM VARYING R-I-ROW FROM 1 BY 1 UNTIL R-I-ROW > MAX-ROWS

           IF R-I-ROW NOT EQUAL I-COLUMN
               COMPUTE SUBTRACT-ROW-FACTOR =
                       R-MATRIX-VALUE(R-I-ROW, I-COLUMN)
                       / R-MATRIX-VALUE(I-COLUMN, I-COLUMN)
               PERFORM VARYING R-I-COL
                       FROM 1 BY 1
                       UNTIL R-I-COL > MAX-COLS
                   COMPUTE R-MATRIX-VALUE(R-I-ROW, R-I-COL)
                           = R-MATRIX-VALUE(R-I-ROW,R-I-COL)
                   - (SUBTRACT-ROW-FACTOR *
                   R-MATRIX-VALUE(I-COLUMN,R-I-COL))


                   IF R-MATRIX-VALUE(R-I-ROW, R-I-COL) > 0
                       IF R-MATRIX-VALUE(R-I-ROW, R-I-COL) < EPSILON
                           MOVE 0 TO R-MATRIX-VALUE(R-I-ROW, R-I-COL)
                       END-IF
                   ELSE
                       IF R-MATRIX-VALUE(R-I-ROW, R-I-COL) > EPSILON*-1
                           MOVE 0 TO R-MATRIX-VALUE(R-I-ROW, R-I-COL)
                       END-IF
                   END-IF



               END-PERFORM
           END-IF
           END-PERFORM
           .
       FIND-MAX-ELEMENT-IN-COLUMN.
           MOVE 0 TO TEMP-MAX
           PERFORM VARYING R-I-ROW
                   FROM 1 BY 1
                   UNTIL R-I-ROW > MAX-ROWS
                   IF R-MATRIX-VALUE(R-I-ROW, I-COLUMN)  < 0
                      IF R-MATRIX-VALUE(R-I-ROW, I-COLUMN)*-1 > TEMP-MAX
                           COMPUTE TEMP-MAX =
                                   R-MATRIX-VALUE(R-I-ROW, I-COLUMN)
                           MOVE R-I-ROW TO TEMP-MAX-INDEX
                      END-IF
                   ELSE
                   IF R-MATRIX-VALUE(R-I-ROW, I-COLUMN) > TEMP-MAX
                      MOVE R-MATRIX-VALUE(R-I-ROW, I-COLUMN) TO TEMP-MAX
                      MOVE R-I-ROW TO TEMP-MAX-INDEX
                   END-IF
           END-PERFORM
           .

       PRINT.
           PERFORM VARYING INDEX-ROW
                   FROM 1 BY 1 UNTIL INDEX-ROW > MAX-ROWS
                   AFTER INDEX-COL
                   FROM 1 BY 1 UNTIL INDEX-COL > MAX-COLS
                   MOVE R-MATRIX-VALUE(INDEX-ROW, INDEX-COL) TO
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
           MOVE R-MATRIX-ROW(INDEX-SWAPFIRST)
           TO   MATRIX-ROW-CHANGER
           MOVE R-MATRIX-ROW(INDEX-SWAPSECOND)
           TO   R-MATRIX-ROW(INDEX-SWAPFIRST)
           MOVE MATRIX-ROW-CHANGER
           TO   R-MATRIX-ROW(INDEX-SWAPSECOND)
           .
