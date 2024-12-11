       IDENTIFICATION DIVISION.
       PROGRAM-ID. CERES-SEARCH.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 4 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 4 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/4                  *
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
           RECORD CONTAINS 140 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                           PIC X(140).


       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.
       01  WS-ARR-LENGTH                          PIC 9(3) VALUE 140.            
       01  WS-WORD-SEARCH-ARR.
           05 WS-WSA OCCURS 0 TO 140 TIMES DEPENDING ON WS-ARR-LENGTH
                                                  PIC X(140).
       01  WS-ARR-SUB                             PIC 9(3) VALUE 1.
       01  WS-ROWS                                PIC 9(3) VALUE 1.
       01  WS-COLS                                PIC 9(3) VALUE 1.
       01  WS-TOTAL-STRING                        PIC 9(8) VALUE 0.
       01  WS-TOTAL-CROSSES                       PIC 9(8) VALUE 0.
       01  WS-END                                 PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE             THRU 1000-EXIT
           PERFORM 2000-CONVERT-FILE-TO-ARRAY THRU 2000-EXIT
               UNTIL END-OF-FILE
           PERFORM 3000-SCAN-FOR-STRING       THRU 3000-EXIT
           PERFORM 4000-SCAN-FOR-CROSS        THRU 4000-EXIT
           PERFORM 8000-DISPLAY-RESULT        THRU 8000-EXIT
           PERFORM 9000-CLOSE-FILE            THRU 9000-EXIT
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
      * READ FILE LINE BY LINE, CONVERTING EACH LINE INTO A NODE IN  *
      * AN ARRAY                                          
      ****************************************************************
       2000-CONVERT-FILE-TO-ARRAY.
       
           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   MOVE INPUT-RECORD TO WS-WSA(WS-ARR-SUB)
                   ADD 1 TO WS-ARR-SUB
           END-READ
           .
       2000-EXIT.
           EXIT.

      ****************************************************************
      * SEARCH FOR OCCURRENCES OF 'XMAS' THROUGHOUT WORD SEARCH      *
      *****************************************************************
       3000-SCAN-FOR-STRING.

           MOVE 1 TO WS-ROWS
           PERFORM UNTIL WS-ROWS EQUALS (WS-ARR-LENGTH + 1)
               MOVE 1 TO WS-COLS
               PERFORM UNTIL WS-COLS EQUALS (WS-ARR-LENGTH + 1)
                   PERFORM 3100-CHECK-CHAR THRU 3100-EXIT
                   ADD 1 TO WS-COLS
               END-PERFORM
               ADD 1 TO WS-ROWS
           END-PERFORM
           .
       3000-EXIT.
           EXIT. 

      ****************************************************************
      * CHECK CURRENT CHAR FOR BEGINNING OF TARGET STRING            *
      ****************************************************************
       3100-CHECK-CHAR.
           IF WS-WSA(WS-ROWS)(WS-COLS:1) NOT EQUALS 'X'
               GO TO 3100-EXIT
           END-IF
           
      *    CHECK ABOVE
           IF WS-ROWS > 3 *> X GOING UP CANNOT BE HIGHER THAN ROW4
               PERFORM 3110-CHECK-ABOVE THRU 3110-EXIT
           END-IF
      *    CHECK TOP RIGHT
      *    X GOING UP AND TO THE RIGHT MUST BE AT LEAST IN ROW 4
      *    AND NO FURTHER THAN 3 FROM THE RIGHT.
           IF WS-ROWS > 3 AND (WS-ARR-LENGTH - WS-COLS) >= 3
               PERFORM 3120-CHECK-TOP-RIGHT THRU 3120-EXIT
           END-IF
      *    CHECK RIGHT
      *    X GOING RIGHT CANNOT BE CLOSER THAN 3 FROM THE RIGHT
           IF (WS-ARR-LENGTH - WS-COLS) >= 3 
               PERFORM 3130-CHECK-RIGHT THRU 3130-EXIT
           END-IF
      *    CHECK BOTTOM RIGHT
      *    X GOING DOWN AND TO THE RIGHT CANNOT BE CLOSER THAN
      *    3 FROM THE BOTTOM AND TO THE RIGHT 
           IF  (WS-ARR-LENGTH - WS-ROWS) >= 3 
           AND (WS-ARR-LENGTH - WS-COLS) >= 3
               PERFORM 3140-CHECK-BOTTOM-RIGHT THRU 3140-EXIT
           END-IF
      *    CHECK BELOW
      *    X GOING DOWN CANNOT BE CLOSER THAN 3 FROM THE BOTTOM
           IF (WS-ARR-LENGTH - WS-ROWS) >= 3 
               PERFORM 3150-CHECK-DOWN THRU 3150-EXIT
           END-IF
      *    CHECK BOTTOM LEFT
      *    X GOING DOWN AND TO THE LEFT CANNOT BE CLOSER THAN 3 FROM
      *    THE BOTTOM AND MUST BE IN AT LEAST COL 4
           IF  (WS-ARR-LENGTH - WS-ROWS) >= 3 
           AND WS-COLS > 3 
               PERFORM 3160-CHECK-BOTTOM-LEFT THRU 3160-EXIT
           END-IF
      *    CHECK LEFT
           IF WS-COLS > 3 *> X GOING LEFT MUST BE AT LEAST IN COL4
               PERFORM 3170-CHECK-LEFT THRU 3170-EXIT
           END-IF
      *    CHECK TOP LEFT
      *    X GOING UP AND TO THE LEFT MUST BE IN AT LEAST ROW4 AND COL4
           IF WS-ROWS > 3 AND WS-COLS > 3
               PERFORM 3180-CHECK-TOP-LEFT THRU 3180-EXIT
           END-IF
           .
       3100-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR XMAS GOING UP                                      *
      ****************************************************************
       3110-CHECK-ABOVE.

           IF   WS-WSA(WS-ROWS - 1)(WS-COLS:1) EQUALS 'M'
           AND  WS-WSA(WS-ROWS - 2)(WS-COLS:1) EQUALS 'A'
           AND  WS-WSA(WS-ROWS - 3)(WS-COLS:1) EQUALS 'S'
               ADD 1 TO WS-TOTAL-STRING
           END-IF
           .
       3110-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR XMAS GOING UP                                      *
      ****************************************************************
       3120-CHECK-TOP-RIGHT.

           IF   WS-WSA(WS-ROWS - 1)(WS-COLS + 1:1) EQUALS 'M'
           AND  WS-WSA(WS-ROWS - 2)(WS-COLS + 2:1) EQUALS 'A'
           AND  WS-WSA(WS-ROWS - 3)(WS-COLS + 3:1) EQUALS 'S'
                ADD 1 TO WS-TOTAL-STRING
           END-IF
           .
       3120-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR XMAS GOING RIGHT                                   *
      ****************************************************************
       3130-CHECK-RIGHT.

           IF   WS-WSA(WS-ROWS)(WS-COLS + 1:1) EQUALS 'M'
           AND  WS-WSA(WS-ROWS)(WS-COLS + 2:1) EQUALS 'A'
           AND  WS-WSA(WS-ROWS)(WS-COLS + 3:1) EQUALS 'S'
               ADD 1 TO WS-TOTAL-STRING
           END-IF
           .
       3130-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR XMAS GOING DOWN AND TO THE RIGHT                   *
      ****************************************************************
       3140-CHECK-BOTTOM-RIGHT.

           IF   WS-WSA(WS-ROWS + 1)(WS-COLS + 1:1) EQUALS 'M'
           AND  WS-WSA(WS-ROWS + 2)(WS-COLS + 2:1) EQUALS 'A'
           AND  WS-WSA(WS-ROWS + 3)(WS-COLS + 3:1) EQUALS 'S'
               ADD 1 TO WS-TOTAL-STRING
           END-IF
           .
       3140-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR XMAS GOING DOWN                                    *
      ****************************************************************
       3150-CHECK-DOWN.

           IF   WS-WSA(WS-ROWS + 1)(WS-COLS:1) EQUALS 'M'
           AND  WS-WSA(WS-ROWS + 2)(WS-COLS:1) EQUALS 'A'
           AND  WS-WSA(WS-ROWS + 3)(WS-COLS:1) EQUALS 'S'
               ADD 1 TO WS-TOTAL-STRING
           END-IF
           .
       3150-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR XMAS GOING DOWN AND TO THE LEFT                    *
      ****************************************************************
       3160-CHECK-BOTTOM-LEFT.

           IF   WS-WSA(WS-ROWS + 1)(WS-COLS - 1:1) EQUALS 'M'
           AND  WS-WSA(WS-ROWS + 2)(WS-COLS - 2:1) EQUALS 'A'
           AND  WS-WSA(WS-ROWS + 3)(WS-COLS - 3:1) EQUALS 'S'
               ADD 1 TO WS-TOTAL-STRING
           END-IF
           .
       3160-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR XMAS GOING LEFT                                    *
      ****************************************************************
       3170-CHECK-LEFT.

           IF   WS-WSA(WS-ROWS)(WS-COLS - 1:1) EQUALS 'M'
           AND  WS-WSA(WS-ROWS)(WS-COLS - 2:1) EQUALS 'A'
           AND  WS-WSA(WS-ROWS)(WS-COLS - 3:1) EQUALS 'S'
               ADD 1 TO WS-TOTAL-STRING
           END-IF
           .
       3170-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR XMAS GOING UP AND TO THE LEFT                      *
      ****************************************************************
       3180-CHECK-TOP-LEFT.

           IF   WS-WSA(WS-ROWS - 1)(WS-COLS - 1:1) EQUALS 'M'
           AND  WS-WSA(WS-ROWS - 2)(WS-COLS - 2:1) EQUALS 'A'
           AND  WS-WSA(WS-ROWS - 3)(WS-COLS - 3:1) EQUALS 'S'
               ADD 1 TO WS-TOTAL-STRING
           END-IF
           .
       3180-EXIT.
           EXIT.


      ****************************************************************
      * CHECK FOR MIDDLE OF CROSS                                    *
      ****************************************************************
       4000-SCAN-FOR-CROSS.

           MOVE 2 TO WS-ROWS
           PERFORM UNTIL WS-ROWS EQUALS (WS-ARR-LENGTH)
               MOVE 2 TO WS-COLS
               PERFORM UNTIL WS-COLS EQUALS (WS-ARR-LENGTH)
                   IF WS-WSA(WS-ROWS)(WS-COLS:1) EQUALS 'A'
                       PERFORM 4100-CHECK-CORNERS THRU 4100-EXIT
                   END-IF
                   ADD 1 TO WS-COLS
               END-PERFORM
               ADD 1 TO WS-ROWS
           END-PERFORM
           .
       4000-EXIT.
           EXIT.

      ****************************************************************
      * CHECK FOR PROPER CORNERS OF THE CROSS                        *
      ****************************************************************
       4100-CHECK-CORNERS.

           EVALUATE TRUE
      *        M S
      *         A
      *        M S
               WHEN WS-WSA(WS-ROWS - 1)(WS-COLS + 1:1) EQUALS 'S'
                AND WS-WSA(WS-ROWS - 1)(WS-COLS - 1:1) EQUALS 'M'
                AND WS-WSA(WS-ROWS + 1)(WS-COLS - 1:1) EQUALS 'M'
                AND WS-WSA(WS-ROWS + 1)(WS-COLS + 1:1) EQUALS 'S' 
      *        M M
      *         A
      *        S S  
               WHEN WS-WSA(WS-ROWS - 1)(WS-COLS + 1:1) EQUALS 'M'
                AND WS-WSA(WS-ROWS - 1)(WS-COLS - 1:1) EQUALS 'M'
                AND WS-WSA(WS-ROWS + 1)(WS-COLS - 1:1) EQUALS 'S'
                AND WS-WSA(WS-ROWS + 1)(WS-COLS + 1:1) EQUALS 'S'            
      *        S M
      *         A
      *        S M 
               WHEN WS-WSA(WS-ROWS - 1)(WS-COLS + 1:1) EQUALS 'M'
                AND WS-WSA(WS-ROWS - 1)(WS-COLS - 1:1) EQUALS 'S'
                AND WS-WSA(WS-ROWS + 1)(WS-COLS - 1:1) EQUALS 'S'
                AND WS-WSA(WS-ROWS + 1)(WS-COLS + 1:1) EQUALS 'M' 
      *        S S
      *         A
      *        M M
               WHEN WS-WSA(WS-ROWS - 1)(WS-COLS + 1:1) EQUALS 'S'
                AND WS-WSA(WS-ROWS - 1)(WS-COLS - 1:1) EQUALS 'S'
                AND WS-WSA(WS-ROWS + 1)(WS-COLS - 1:1) EQUALS 'M'
                AND WS-WSA(WS-ROWS + 1)(WS-COLS + 1:1) EQUALS 'M' 
                   ADD 1 TO WS-TOTAL-CROSSES
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       4100-EXIT.
           EXIT.

      ****************************************************************
      * DISPLAY WORD SEARCH RESULTS                                  *
      ****************************************************************
       8000-DISPLAY-RESULT.

           DISPLAY 'TOTAL ''XMAS'' = ' WS-TOTAL-STRING
           DISPLAY 'TOTAL  X-MAS = ' WS-TOTAL-CROSSES
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