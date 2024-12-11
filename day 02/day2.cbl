       IDENTIFICATION DIVISION.
       PROGRAM-ID. RED-NOSED-REPORTS.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 2 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 2 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/2                   *
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
           RECORD CONTAINS 100 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                         PIC X(100).


       WORKING-STORAGE SECTION.
       01  WS-BEGIN                             PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-END-OF-FILE                               PIC X(1).
           88 END-OF-FILE                                    VALUE 'Y'.
           88 NOT-END-OF-FILE                                VALUE 'N'.
       01  WS-END-OF-RECORD                             PIC X(1).
           88 END-OF-RECORD                                  VALUE 'Y'.
           88 NOT-END-OF-RECORD                              VALUE 'N'.
       01  WS-REPORT-DIRECTION                  PIC X(1).
           88 INCREASING                                     VALUE '>'.     
           88 DECREASING                                     VALUE '<'.
       01  WS-SAFE-FLAG                         PIC X(1).
           88 SAFE                                           VALUE 'Y'.
           88 UNSAFE                                         VALUE 'N'.

       01  WS-UNSAFE-CNT                        PIC 9(2).            
       01  WS-REMOVE-LEVEL                      PIC 9(2).
       01  WS-INREC-POINTER                     PIC 9(2).

       01  WS-LEVELS.
           05  WS-LEVELS-ARR OCCURS 1002 TIMES  PIC 9(2)  VALUE 0.
       01  WS-LEVEL2.
           05  WS-LEVEL2-ARR OCCURS 1002 TIMES  PIC 9(2)  VALUE 0.
       01  WS-LEVELS-ORIG.
           05  WS-LEVELO-ARR OCCURS 1002 TIMES  PIC 9(2)  VALUE 0.
       01  WS-ARR-SUB                           PIC 9(4).
       01  WS-ARR-SUB-PLUS-1                    PIC 9(1).
       01  WS-ARR2-SUB                          PIC 9(4).
       01  WS-SAFE-RPT-CNT                      PIC 9(4)    VALUE 0.
       01  WS-UNSAFE-RPT-CNT                    PIC 9(4)    VALUE 0.
       01  WS-TOTAL-RPT-CNT                     PIC 9(4)    VALUE 0.
       
       01  WS-DIFF                              PIC 9(2).

       01  C-NEW-LINE-CHAR                      PIC X(1)    VALUE X'00'.

       01  WS-END                               PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE      THRU 1000-EXIT
           PERFORM 2000-PROCESS-DATA   THRU 2000-EXIT
               UNTIL END-OF-FILE
           PERFORM 8000-DISPLAY-TOTALS THRU 8000-EXIT
           PERFORM 9000-CLOSE-FILE     THRU 9000-EXIT
           .
       0000-EXIT.
           GOBACK.

      ****************************************************************
      * OPEN FILE                                                    *
      ****************************************************************
       1000-OPEN-FILE.

           OPEN INPUT INPUT-FILE
           MOVE SPACES TO WS-END-OF-FILE
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * READ FILE LINE BY LINE                                       *
      ****************************************************************
       2000-PROCESS-DATA.
       
           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   MOVE ZEROS TO WS-LEVELS
                                 WS-LEVEL2
                                 WS-LEVELS-ORIG
                                 WS-UNSAFE-CNT
                   MOVE 1     TO WS-REMOVE-LEVEL
                   PERFORM 2100-PARSE-LEVELS THRU 2100-EXIT
                   PERFORM 2200-CHECK-SAFETY THRU 2200-EXIT  
                   ADD 1 TO WS-TOTAL-RPT-CNT
           END-READ
           .
       2000-EXIT.
           EXIT.

      ****************************************************************
      * READ REPORT LINE AND CREATE ARRAY WITH ALL LEVELS (VALUES)   *
      ****************************************************************
       2100-PARSE-LEVELS.
           
           MOVE 1      TO WS-INREC-POINTER 
                          WS-ARR-SUB
           MOVE SPACES TO WS-END-OF-RECORD 

           PERFORM UNTIL END-OF-RECORD
               EVALUATE INPUT-RECORD(WS-INREC-POINTER:1)
                   WHEN SPACE
                       CONTINUE
                   WHEN C-NEW-LINE-CHAR 
                       SET END-OF-RECORD TO TRUE
                   WHEN OTHER
                       IF INPUT-RECORD(WS-INREC-POINTER:1) NUMERIC
      *                    LOOK AHEAD ONE MORE CHARACTER TO SEE IF
      *                    WE ARE DEALING WITH A TWO-DIGIT NUMBER
                           IF INPUT-RECORD(WS-INREC-POINTER:2) NUMERIC
                               MOVE INPUT-RECORD(WS-INREC-POINTER:2)
                                   TO WS-LEVELS-ARR(WS-ARR-SUB)
      *                        BOUNCE ADDITIONAL CHARACTER FOR TWO
      *                        DIGIT NUMBER TO PREVENT BAD DATA
                               ADD 1 TO WS-INREC-POINTER 
                           ELSE                                
                               MOVE INPUT-RECORD(WS-INREC-POINTER:1)
                                   TO WS-LEVELS-ARR(WS-ARR-SUB)
                           END-IF
                           ADD 1 TO WS-ARR-SUB
                       ELSE
                           DISPLAY 'ERROR! INVALID DATA!'
                           PERFORM 9999-ABEND THRU 9999-EXIT
                       END-IF  
               END-EVALUATE
               ADD 1 TO WS-INREC-POINTER
           END-PERFORM

           MOVE WS-LEVELS TO WS-LEVELS-ORIG
           .
       2100-EXIT.
           EXIT.

      ****************************************************************
      * CHECK IF REPORT IS "SAFE" BY TESTING THAT ALL LEVELS ARE     *
      * INCREASING OR DECREASING ONLY, AS WELL AS EACH INCREMENT IS  *
      * WITHIN THE RANGE OF 1-3                                      *
      ****************************************************************
       2200-CHECK-SAFETY.
           MOVE SPACES TO WS-REPORT-DIRECTION
                          WS-SAFE-FLAG
      *    DETERMINE DIRECTION
           EVALUATE TRUE
               WHEN WS-LEVELS-ARR(1) LESS WS-LEVELS-ARR(2)
                   SET INCREASING TO TRUE
               WHEN WS-LEVELS-ARR(1) GREATER WS-LEVELS-ARR(2)
                   SET DECREASING TO TRUE
      *        REPORT IS AUTOMATICALLY UNSAFE IF FIRST TWO LEVELS
      *        ARE EQUAL
               WHEN WS-LEVELS-ARR(1) EQUAL WS-LEVELS-ARR(2)
                   SET UNSAFE TO TRUE
                   GO TO INCREMENT-COUNTERS
               WHEN OTHER
                   PERFORM 9999-ABEND THRU 9999-EXIT
           END-EVALUATE
      *    WALK THROUGH REMAINDER OF ARRAY AND VERIFY IT REMAINS IN
      *    THE SAME DIRECTION AND ONLY DOES SO IN INCREMENTS OF 1-3
           MOVE 1      TO WS-ARR-SUB
           MOVE 2      TO WS-ARR-SUB-PLUS-1 

      *    ONCE THE LEVEL AFTER THE CURRENT ONE IS 0, WE STOP CHEKING
      *    BECAUSE THAT IS THE END OF THE RECORD
           PERFORM UNTIL WS-LEVELS-ARR(WS-ARR-SUB-PLUS-1) EQUALS 0
               IF INCREASING
                   PERFORM 2300-CHECK-INCREASING THRU 2300-EXIT
               ELSE
                   PERFORM 2400-CHECK-DECREASING THRU 2400-EXIT
               END-IF
               IF UNSAFE
      *            BREAK LOOP IF DEEMED UNSAFE
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-ARR-SUB
                        WS-ARR-SUB-PLUS-1 
           END-PERFORM
           .
       INCREMENT-COUNTERS.
           IF UNSAFE
               IF WS-LEVELO-ARR(WS-REMOVE-LEVEL) EQUALS 0
                   ADD 1 TO WS-UNSAFE-RPT-CNT
               ELSE
      *            IF UNSAFE, AND THERE IS MORE DATA WE CAN TRY 
      *            REMOVING, REMOVE AN ELEMENT FROM THE ARRAY AND 
      *            TRY AGAIN
                   MOVE 1 TO WS-ARR-SUB
                             WS-ARR2-SUB
                   PERFORM UNTIL WS-LEVELO-ARR(WS-ARR-SUB) EQUALS 0
                       IF WS-ARR-SUB EQUALS WS-REMOVE-LEVEL
                           CONTINUE
                       ELSE
                           MOVE WS-LEVELO-ARR(WS-ARR-SUB) 
                                TO WS-LEVEL2-ARR(WS-ARR2-SUB)
                           ADD 1 TO WS-ARR2-SUB
                       END-IF
                       ADD 1 TO WS-ARR-SUB
                                
                   END-PERFORM
                   MOVE WS-LEVEL2 TO WS-LEVELS
                   ADD 1 TO WS-REMOVE-LEVEL
                   GO TO 2200-CHECK-SAFETY
           ELSE
               ADD 1 TO WS-SAFE-RPT-CNT
           END-IF
           .
       2200-EXIT.
           EXIT.

      ****************************************************************
      * CHECK INCREASING SEQUENCE REMAINS INCREASING AND ONLY DOES   *
      * SO IN INCREMENTS OF 1-3                                      *
      ****************************************************************
       2300-CHECK-INCREASING.

           IF WS-LEVELS-ARR(WS-ARR-SUB) LESS
              WS-LEVELS-ARR(WS-ARR-SUB-PLUS-1)
               CONTINUE
           ELSE
               SET UNSAFE TO TRUE
               GO TO 2300-EXIT
           END-IF

           SUBTRACT WS-LEVELS-ARR(WS-ARR-SUB) FROM
                     WS-LEVELS-ARR(WS-ARR-SUB-PLUS-1) GIVING WS-DIFF
           
           IF (WS-DIFF GREATER 3 OR WS-DIFF LESS 1)
               SET UNSAFE TO TRUE
               GO TO 2300-EXIT
           END-IF
           .
       2300-EXIT.
           EXIT.
      ****************************************************************
      * CHECK DECREASING SEQUENCE REMAINS DECREASING AND ONLY DOES   *
      * SO IN INCREMENTS OF 1-3                                      *
      ****************************************************************
       2400-CHECK-DECREASING.

       IF WS-LEVELS-ARR(WS-ARR-SUB) GREATER 
              WS-LEVELS-ARR(WS-ARR-SUB-PLUS-1)
               CONTINUE
           ELSE
               SET UNSAFE TO TRUE
               GO TO 2400-EXIT
           END-IF

           SUBTRACT WS-LEVELS-ARR(WS-ARR-SUB-PLUS-1) FROM
                    WS-LEVELS-ARR(WS-ARR-SUB) GIVING WS-DIFF
           
           IF (WS-DIFF GREATER 3 OR WS-DIFF LESS 1)
               SET UNSAFE TO TRUE
               GO TO 2400-EXIT
           END-IF
           .
       2400-EXIT.
           EXIT.

      ****************************************************************
      * BALANCE/DISPLAY TOTALS                                       *
      ****************************************************************
       8000-DISPLAY-TOTALS.

           DISPLAY 'SAFE REPORTS  : ' WS-SAFE-RPT-CNT
           DISPLAY 'UNSAFE REPORTS: ' WS-UNSAFE-RPT-CNT
           DISPLAY 'TOTAL REPORTS : ' WS-TOTAL-RPT-CNT

           ADD WS-UNSAFE-RPT-CNT TO WS-SAFE-RPT-CNT
           IF WS-SAFE-RPT-CNT EQUAL WS-TOTAL-RPT-CNT
               CONTINUE
           ELSE
               DISPLAY 'ERROR! OUT OF BALANCE!'
               PERFORM 9999-ABEND THRU 9999-EXIT
           END-IF
           .
       8000-EXIT.
           EXIT.
      
      ****************************************************************
      * CLOSE FILE                                                   *
      ****************************************************************
       9000-CLOSE-FILE.

           CLOSE INPUT-FILE
           .       
       9000-EXIT.
           EXIT.

      ****************************************************************
      * ABEND PARAGRAPH IF A FATAL ERROR IS FOUND                    *
      ****************************************************************
       9999-ABEND.
           
           DISPLAY 'ABENDING PROGRAM'
           STOP RUN
           .
       9999-EXIT.
           EXIT.
