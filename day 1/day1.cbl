       IDENTIFICATION DIVISION.
       PROGRAM-ID. HYSTORIAN-HYSTERIA.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 1 2025.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2025 DAY 1 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/1                   *
      *************************************************************** 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    INPUT FILE
           SELECT INPUT-FILE ASSIGN TO 'INFILE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.



       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 20 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  IN-NUM-1                         PIC 9(5).
           05  FILLER                           PIC X(3).
           05  IN-NUM-2                         PIC 9(5).


       WORKING-STORAGE SECTION.
       01  WS-BEGIN                             PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-EOF                               PIC X(1).
           88 EOF                                        VALUE 'Y'.
           88 NOT-EOF                                    VALUE 'N'.
       01  WS-NUM-ARR-1                         PIC 9(5)
           OCCURS 1000 TIMES.
       01  WS-NUM-ARR-2                         PIC 9(5)
           OCCURS 1000 TIMES.
       01  WS-ARRAY-SUB                         PIC 9(4) VALUE 1.
       01  WS-POINTER                           PIC 9(4).
       01  WS-TEMP                              PIC 9(5).
       01  WS-DIFF                              PIC 9(5).
       01  WS-OUTPUT                            PIC 9(10) VALUE 0.
       01  WS-END                               PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE      THRU 1000-EXIT
           PERFORM 2000-COPY-DATA      THRU 2000-EXIT
               UNTIL EOF
           PERFORM 2500-SORT-DATA      THRU 2500-EXIT
           PERFORM 3000-CALCULATE-DIFF THRU 3000-EXIT
           PERFORM 4000-RETURN-OUTPUT  THRU 4000-EXIT
           PERFORM 9000-CLOSE-FILE     THRU 9000-EXIT
           .
       0000-EXIT.
           GOBACK.

      ****************************************************************
      * OPEN FILE                                                    *
      ****************************************************************
       1000-OPEN-FILE.

           OPEN INPUT INPUT-FILE
           MOVE SPACES TO WS-EOF
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * COPY THE NUMBERS INTO UNIQUE ARRAYS                          *
      ****************************************************************
       2000-COPY-DATA.
       
           READ INPUT-FILE
               AT END 
                   SET EOF TO TRUE
               NOT AT END
                   MOVE IN-NUM-1 TO WS-NUM-ARR-1(WS-ARRAY-SUB)
                   MOVE IN-NUM-2 TO WS-NUM-ARR-2(WS-ARRAY-SUB)
                   ADD 1 TO WS-ARRAY-SUB    
           END-READ
           .
       2000-EXIT.
           EXIT.
       
      ****************************************************************
      * INSERTION SORT THE DATA ARRAYS INTO ASCENDING ORDER          *
      ****************************************************************
       2500-SORT-DATA.
          

      *    SORT ARRAY 1
           MOVE 2 TO WS-ARRAY-SUB
           PERFORM UNTIL WS-ARRAY-SUB EQUAL 1001
               MOVE WS-ARRAY-SUB TO WS-POINTER
                PERFORM UNTIL WS-POINTER LESS 2 
                           OR (WS-NUM-ARR-1(WS-POINTER) 
                               GREATER WS-NUM-ARR-1(WS-POINTER - 1))
                   MOVE WS-NUM-ARR-1(WS-POINTER) TO WS-TEMP
                   MOVE WS-NUM-ARR-1(WS-POINTER - 1) 
                                             TO WS-NUM-ARR-1(WS-POINTER)
                   MOVE WS-TEMP          TO WS-NUM-ARR-1(WS-POINTER - 1)
                   SUBTRACT 1 FROM WS-POINTER
               END-PERFORM
               ADD 1 TO WS-ARRAY-SUB
           END-PERFORM

      *    SORT ARRAY 2
           MOVE 2 TO WS-ARRAY-SUB
           PERFORM UNTIL WS-ARRAY-SUB EQUAL 1001
               MOVE WS-ARRAY-SUB TO WS-POINTER
               PERFORM UNTIL WS-POINTER LESS 2 
                           OR (WS-NUM-ARR-2(WS-POINTER) 
                               GREATER WS-NUM-ARR-2(WS-POINTER - 1))
                   MOVE WS-NUM-ARR-2(WS-POINTER) TO WS-TEMP
                   MOVE WS-NUM-ARR-2(WS-POINTER - 1) 
                                             TO WS-NUM-ARR-2(WS-POINTER)
                   MOVE WS-TEMP          TO WS-NUM-ARR-2(WS-POINTER - 1)
                   SUBTRACT 1 FROM WS-POINTER
               END-PERFORM
               ADD 1 TO WS-ARRAY-SUB
           END-PERFORM
           .
       2500-EXIT.
           EXIT.

      ****************************************************************
      * WALK THROUGH ARRAYS IN ASCNEDING ORDER, CALCULATING THE      *
      * DIFFERENCE BETWEEN THE VALUES, AND ADDING TO OUTPUT.         *
      ****************************************************************
       3000-CALCULATE-DIFF.
           MOVE 1 TO WS-ARRAY-SUB
           PERFORM UNTIL WS-ARRAY-SUB EQUAL 1001
               IF WS-NUM-ARR-1(WS-ARRAY-SUB) GREATER 
                       WS-NUM-ARR-2(WS-ARRAY-SUB)
                   SUBTRACT WS-NUM-ARR-1(WS-ARRAY-SUB) 
                       FROM WS-NUM-ARR-2(WS-ARRAY-SUB)
                           GIVING WS-DIFF
               ELSE
                   SUBTRACT WS-NUM-ARR-2(WS-ARRAY-SUB)
                       FROM WS-NUM-ARR-1(WS-ARRAY-SUB)
                           GIVING WS-DIFF
               END-IF
               ADD WS-DIFF TO WS-OUTPUT
               ADD 1 TO WS-ARRAY-SUB
           END-PERFORM
           .
       3000-EXIT.
           EXIT.

      ****************************************************************
      * DISPLAY RESULTING COUNT                                      *
      ****************************************************************
       4000-RETURN-OUTPUT.
           DISPLAY 'TOTAL DISTANCE: ' WS-OUTPUT
           .
       4000-EXIT.
           EXIT.

      ****************************************************************
      * CLOSE FILE                                                   *
      ****************************************************************
       9000-CLOSE-FILE.
           CLOSE INPUT-FILE
           .       
       9000-EXIT.
           EXIT.

