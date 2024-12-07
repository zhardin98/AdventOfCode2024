       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRIDGE-REPAIR.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 7 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 7 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/7                   *
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
           RECORD CONTAINS 50 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                          PIC X(50).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                              PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.

       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.

       01  WS-RESULT                              PIC X(1).
           88 RESULT-TRUE                                   VALUE 'Y'.
           88 RESULT-FALSE                                  VALUE 'N'.

       01  WS-VALUES-ARR.
           05 WS-VALUES OCCURS 100 TIMES          PIC 9(3).

       01  WS-ARR-SUB                             PIC 9(3) VALUE 1.

       01  WS-TARGET-VALUE                        PIC 9(18).
       01  WS-SOLUTION                            PIC 9(31).
       01  WS-EQUATION-REC                        PIC X(36).
       01  WS-REC-POINTER                         PIC 9(2).
       01  WS-FUNCTION-ARR.                  
           05 WS-FUNC OCCURS 100 TIMES            PIC X(1).
       01  WS-FUNC-SUB                            PIC 9(3).

       01  WS-PLUS-COUNT                          PIC 9(3).
       
       01  WS-CALIBRATION                         PIC 9(18)  VALUE 0.
       01  WS-END                                 PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE             THRU 1000-EXIT
           PERFORM 2000-PROCESS-DATA          THRU 2000-EXIT
               UNTIL END-OF-FILE
           PERFORM 8000-DISPLAY-RESULTS       THRU 8000-EXIT
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
      * READ FILE LINE BY LINE                                       *
      ****************************************************************
       2000-PROCESS-DATA.
       
           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM 3000-UNSTRING-TARGET THRU 3000-EXIT
                   PERFORM 3100-UNSTRING-VALUES THRU 3100-EXIT
                   PERFORM 4000-TEST-EQUATION   THRU 4000-EXIT
           END-READ
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * UNSTRING ':' TO SEPERATE TARGET VALUE FROM NUMBERS IN EQUATION*
      *****************************************************************
       3000-UNSTRING-TARGET.

           UNSTRING INPUT-RECORD DELIMITED BY ': ' INTO WS-TARGET-VALUE
                                                        WS-EQUATION-REC
           END-UNSTRING
           .
       3000-EXIT.


      *****************************************************************
      * UNSTRING ' ' TO GET INDIVIDUAL NUMBERS                        *
      *****************************************************************
       3100-UNSTRING-VALUES.

           MOVE 1 TO WS-REC-POINTER
                     WS-ARR-SUB
           MOVE ZEROS TO WS-VALUES-ARR
           PERFORM UNTIL WS-EQUATION-REC(WS-REC-POINTER:2) EQUALS SPACES
               EVALUATE TRUE
                   WHEN WS-EQUATION-REC(WS-REC-POINTER:3) NUMERIC
                       MOVE WS-EQUATION-REC(WS-REC-POINTER:3) 
                                                TO WS-VALUES(WS-ARR-SUB)
                       ADD 3 TO WS-REC-POINTER
                   WHEN WS-EQUATION-REC(WS-REC-POINTER:2) NUMERIC
                       MOVE WS-EQUATION-REC(WS-REC-POINTER:2) 
                                                TO WS-VALUES(WS-ARR-SUB)
                       ADD 2 TO WS-REC-POINTER
                   WHEN OTHER
                       MOVE WS-EQUATION-REC(WS-REC-POINTER:1) 
                                                TO WS-VALUES(WS-ARR-SUB)
                       ADD 1 TO WS-REC-POINTER
               END-EVALUATE
               ADD 1 TO WS-REC-POINTER
                        WS-ARR-SUB
           END-PERFORM  
           .
       3100-EXIT.
           EXIT.

      *****************************************************************
      * TEST EQUATION BY PLACING + AND * IN BETWEEN EACH VALUE        *
      *****************************************************************
       4000-TEST-EQUATION.

           MOVE SPACES TO WS-RESULT
      *    FILL FUNCTION ARRAY WITH + FOR NOW
           PERFORM 4100-SET-PLUSES THRU 4100-EXIT          
           .
           ATTEMPT-CALC.               
      *    MOVE THROUGH VALUE ARRAY, LEFT TO RIGHT, CALCULATING A RESULT
           MOVE 1            TO WS-FUNC-SUB
           MOVE WS-VALUES(1) TO WS-SOLUTION
           MOVE 2            TO WS-ARR-SUB
           PERFORM UNTIL WS-VALUES(WS-ARR-SUB) EQUALS 0
               IF WS-FUNC(WS-FUNC-SUB) EQUALS '+'
                   ADD WS-VALUES(WS-ARR-SUB) TO WS-SOLUTION
               ELSE
                   MULTIPLY WS-VALUES(WS-ARR-SUB) BY WS-SOLUTION
               END-IF
               ADD 1 TO WS-ARR-SUB 
                        WS-FUNC-SUB                   
           END-PERFORM  

           IF WS-SOLUTION EQUALS WS-TARGET-VALUE                        
               SET RESULT-TRUE TO TRUE  
               ADD WS-SOLUTION TO WS-CALIBRATION
           ELSE
               PERFORM 4200-INCRMENT-BINARY THRU 4200-EXIT
               IF RESULT-FALSE
                   GO TO 4000-EXIT
               END-IF
               GO TO ATTEMPT-CALC
           END-IF 
           .
       4000-EXIT.
           EXIT.

      *****************************************************************
      * FILL FUNCTION ARRAY WITH + FOR NOW                            *
      *****************************************************************
       4100-SET-PLUSES.

      *    THE ARRAY SUB WILL BE 1 AHEAD OF ACTUAL LENGTH WHEN
      *    ENTERING THIS PARAGRAPH. SUBTRACTING 2 WILL GET US TO ONE
      *    LESS THAN LENGTH, WHICH IS THE AMOUNT OF FUNCTIONS WE 
      *    NEED.
           SUBTRACT 2 FROM WS-ARR-SUB   
           MOVE 0 TO WS-FUNC-SUB
           MOVE SPACES TO WS-FUNCTION-ARR
    
           PERFORM UNTIL WS-FUNC-SUB EQUALS WS-ARR-SUB
               ADD 1 TO WS-FUNC-SUB
               MOVE '+' TO WS-FUNC(WS-FUNC-SUB)
           END-PERFORM
           .
       4100-EXIT.
           EXIT.
      ***************************************************************** 
      * USING BINARY PATTERNS, ICNREMENT + AND/OR *                   *
      *****************************************************************
       4200-INCRMENT-BINARY.

           MOVE 1 TO WS-FUNC-SUB
           PERFORM UNTIL WS-FUNC(WS-FUNC-SUB) EQUALS SPACE
               IF WS-FUNC(WS-FUNC-SUB) EQUALS '+'
                   MOVE '*' TO WS-FUNC(WS-FUNC-SUB)
                   GO TO 4200-EXIT
               ELSE
      *            CHECK IF ALL FUNCTIONS ARE *. IF SO, REC DOESN'T WORK   
                   MOVE 0 TO WS-PLUS-COUNT                 
                   INSPECT WS-FUNCTION-ARR
                   TALLYING WS-PLUS-COUNT FOR ALL '+'          
                   IF WS-PLUS-COUNT EQUALS 0                                     
                       SET RESULT-FALSE TO TRUE
                       GO TO 4200-EXIT
                   END-IF
                   MOVE '+' TO WS-FUNC(WS-FUNC-SUB)
               END-IF
               ADD 1 TO WS-FUNC-SUB
           END-PERFORM
           .
       4200-EXIT.
           EXIT.

      *****************************************************************
      * DISPLAY RESULTING SUM OF PRODUCTS                             *
      *****************************************************************
       8000-DISPLAY-RESULTS.
            
           DISPLAY 'CALIBRATION RESULT = ' WS-CALIBRATION
           .
       8000-EXIT.
           EXIT.

      *****************************************************************
      * CLOSE FILE                                                    *
      *****************************************************************
       9000-CLOSE-FILE.

           CLOSE INPUT-FILE
           .       
       9000-EXIT.
           EXIT.