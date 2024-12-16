       IDENTIFICATION DIVISION.
       PROGRAM-ID. WAREHOUSE-WOES.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 15 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 15 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/15                  *
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
           RECORD CONTAINS 1000 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                          PIC X(1000).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                              PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.

       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.
       01  WS-END-OF-MAP                         PIC X(1).
           88 END-OF-MAP                                    VALUE 'Y'.
           88 NOT-END-OF-MAP                                VALUE 'N'.    
       01  WS-START-POS-FOUND                     PIC X(1).
           88 START-POS-FOUND                               VALUE 'Y'.
           88 START-POS-NOT-FOUND                           VALUE 'N'.
       
       01  WS-DIRECTION                           PIC X(1).
           88 DIR-UP                                        VALUE '^'.
           88 DIR-DOWN                                      VALUE 'v'.
           88 DIR-LEFT                                      VALUE '<'.
           88 DIR-RIGHT                                     VALUE '>'.

       01  WS-ARR-LENGTH                          PIC 9(3)  VALUE 0.            
       01  WS-MAP-ARR.
           05 WS-MAP OCCURS 0 TO 130 TIMES DEPENDING ON WS-ARR-LENGTH
                                                  PIC X(140).
       01  WS-MAP-ARR-SUB                         PIC 9(3) VALUE 1.
       01  WS-MAP-SUB-CHAR                        PIC 9(3).

       01  WS-BIGGER-MAP-ARR.
           05 WS-BIG-MAP OCCURS 260 TIMES         PIC X(280).
       01  WS-BIG-MAP-ARR-SUB                     PIC 9(3) VALUE 1.
       01  WS-BIG-MAP-SUB-CHAR                    PIC 9(3).       


       01  WS-INSTRUCTIONS                        PIC X(20000).
       01  WS-INPUT-RECORD-POINTER                PIC 9(4).
       01  WS-INSTRUCTIONS-POINTER                PIC 9(5).

       01  WS-START-ROW                           PIC 9(3).
       01  WS-START-COL                           PIC 9(3).

       01  WS-TARGET-NODE.
              05  WS-TARGET-ROW                   PIC 9(3).
              05  WS-TARGET-COL                   PIC 9(3).

      *STACK FOR PUSHING BOXES        
       01  WS-STACK-TABLE.
           05 WS-STACK-ITEM
           OCCURS 1 TO 2500 TIMES DEPENDING ON WS-STACK-CNT.
               10 WS-STACK-ITEM-ROW               PIC 9(3).
               10 WS-STACK-ITEM-COL               PIC 9(3).
       01  WS-STACK-CNT                           PIC 9(4) VALUE 0.

       01  WS-STACK-IO.
           05 WS-STACK-IO-ROW                     PIC 9(3).
           05 WS-STACK-IO-COL                     PIC 9(3).  

       01  WS-SUM                                 PIC 9(10) VALUE 0.     


       01  WS-END                                 PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE             THRU 1000-EXIT
           PERFORM 2000-CONVERT-FILE-TO-ARRAY THRU 2000-EXIT
               UNTIL END-OF-MAP           
           PERFORM 2100-DOUBLE-MAP            THRU 2100-EXIT
           PERFORM 2500-FIND-STARTING-POS     THRU 2500-EXIT
            MOVE 1 TO WS-INSTRUCTIONS-POINTER
            PERFORM 2600-PROCESS-INSTRUCTIONS  THRU 2600-EXIT
                UNTIL END-OF-FILE
            MOVE 1 TO WS-INSTRUCTIONS-POINTER 
            PERFORM 3000-WALK-ROBOT            THRU 3000-EXIT 
                UNTIL WS-INSTRUCTIONS(WS-INSTRUCTIONS-POINTER:1) EQUALS
                      SPACE OR WS-INSTRUCTIONS-POINTER GREATER 20000
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
           MOVE SPACES TO WS-END-OF-MAP
                          WS-END-OF-FILE
                          WS-MAP-ARR
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * READ FILE LINE BY LINE, CONVERTING EACH LINE INTO A NODE IN  *
      * AN ARRAY                                          
      ****************************************************************
       2000-CONVERT-FILE-TO-ARRAY.
       
           READ INPUT-FILE
           IF INPUT-RECORD EQUALS SPACES
               SET END-OF-MAP TO TRUE
           ELSE
               MOVE INPUT-RECORD TO WS-MAP(WS-MAP-ARR-SUB)
               ADD 1 TO WS-MAP-ARR-SUB
                        WS-ARR-LENGTH  
           END-IF
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * EXPAND MAP TO WIDER FORMAT                                    *
      *****************************************************************
       2100-DOUBLE-MAP.

           MOVE 1 TO WS-MAP-ARR-SUB
                     WS-BIG-MAP-ARR-SUB
           PERFORM UNTIL WS-MAP-ARR-SUB GREATER WS-ARR-LENGTH
               MOVE 1 TO WS-MAP-SUB-CHAR
                         WS-BIG-MAP-SUB-CHAR
               PERFORM UNTIL WS-MAP-SUB-CHAR GREATER WS-ARR-LENGTH
                   EVALUATE WS-MAP(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1)
                       WHEN '#'
                           MOVE '##' TO 
                                          WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)
                                                 (WS-BIG-MAP-SUB-CHAR:2)
                       WHEN 'O'
                           MOVE '[]' TO 
                                          WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)
                                                 (WS-BIG-MAP-SUB-CHAR:2)
                       WHEN '.'
                           MOVE '..' TO 
                                          WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)
                                                 (WS-BIG-MAP-SUB-CHAR:2)                          
                       WHEN '@'
                           MOVE '@.' TO 
                                          WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)
                                                 (WS-BIG-MAP-SUB-CHAR:2) 
                       WHEN OTHER
                           PERFORM 9999-ABEND THRU 9999-EXIT
                   END-EVALUATE
                   ADD 1 TO WS-MAP-SUB-CHAR
                   ADD 2 TO WS-BIG-MAP-SUB-CHAR
               END-PERFORM
               ADD 1 TO WS-MAP-ARR-SUB
                        WS-BIG-MAP-ARR-SUB
           END-PERFORM
           MULTIPLY 2 BY WS-ARR-LENGTH
           .
       2100-EXIT.
           EXIT.
           
      *****************************************************************
      * FIND GUARD'S STARTING POSITION                                *
      *****************************************************************
       2500-FIND-STARTING-POS.

           MOVE SPACES TO WS-START-POS-FOUND
           MOVE 1 TO WS-BIG-MAP-ARR-SUB
                     WS-BIG-MAP-SUB-CHAR
           PERFORM UNTIL START-POS-FOUND
               IF WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)(WS-BIG-MAP-SUB-CHAR:1) 
                  EQUALS '@'
                   SET START-POS-FOUND TO TRUE      
                   MOVE WS-BIG-MAP-ARR-SUB  TO WS-START-ROW
                   MOVE WS-BIG-MAP-SUB-CHAR TO WS-START-COL               
                   GO TO 2500-EXIT   
               END-IF
               IF WS-BIG-MAP-SUB-CHAR EQUALS WS-ARR-LENGTH
                   ADD  1 TO WS-BIG-MAP-ARR-SUB
                   MOVE 1 TO WS-BIG-MAP-SUB-CHAR
               ELSE
                   ADD 1 TO  WS-BIG-MAP-SUB-CHAR
           END-PERFORM
           .
       2500-EXIT.
           EXIT.

      *****************************************************************
      * READ REMAINDER OF INPUT FILE LINE BY LINE TO GET THE ROBOT'S  *
      * INTENTED DIRECTIONAL INSTRUCTIONS                             *
      *****************************************************************
       2600-PROCESS-INSTRUCTIONS.

           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END          
                   MOVE 1 TO WS-INPUT-RECORD-POINTER
                   PERFORM UNTIL INPUT-RECORD(WS-INPUT-RECORD-POINTER:1)
                                 EQUALS SPACE  OR
                                 WS-INPUT-RECORD-POINTER
                                 GREATER 1000
                       MOVE INPUT-RECORD(WS-INPUT-RECORD-POINTER:1)
                           TO WS-INSTRUCTIONS(WS-INSTRUCTIONS-POINTER:1)
                       ADD 1 TO WS-INPUT-RECORD-POINTER
                                WS-INSTRUCTIONS-POINTER
                   END-PERFORM
           END-READ
           .
       2600-EXIT.
           EXIT.

      *****************************************************************
      * HAVE ROBOT WALK IN DIRECTION HE IS FACING                     *
      *****************************************************************
       3000-WALK-ROBOT. 
 
      *    GET NEXT DIRECTIONAL INSTRUCTION
           MOVE WS-INSTRUCTIONS(WS-INSTRUCTIONS-POINTER:1) 
                                                         TO WS-DIRECTION
           EVALUATE TRUE     
               WHEN DIR-UP
                   SUBTRACT 1 FROM WS-BIG-MAP-ARR-SUB 
                         GIVING WS-TARGET-ROW
                   MOVE WS-BIG-MAP-SUB-CHAR TO WS-TARGET-COL
               WHEN DIR-DOWN
                   ADD 1 TO WS-BIG-MAP-ARR-SUB GIVING WS-TARGET-ROW
                   MOVE WS-BIG-MAP-SUB-CHAR TO WS-TARGET-COL
               WHEN DIR-LEFT                       
                   MOVE WS-BIG-MAP-ARR-SUB TO WS-TARGET-ROW
                   SUBTRACT 1 FROM WS-BIG-MAP-SUB-CHAR 
                         GIVING WS-TARGET-COL
               WHEN DIR-RIGHT
                   MOVE WS-BIG-MAP-ARR-SUB TO WS-TARGET-ROW
                   ADD 1 TO WS-BIG-MAP-SUB-CHAR GIVING WS-TARGET-COL
           END-EVALUATE

      *    DETERMINE WHAT NEXT NODE IS 
           EVALUATE WS-BIG-MAP(WS-TARGET-ROW)(WS-TARGET-COL:1)
               WHEN '.'
                   MOVE '.' TO 
                   WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)(WS-BIG-MAP-SUB-CHAR:1)
                   MOVE '@' TO 
                              WS-BIG-MAP(WS-TARGET-ROW)(WS-TARGET-COL:1)
                   MOVE WS-TARGET-ROW TO WS-BIG-MAP-ARR-SUB
                   MOVE WS-TARGET-COL TO WS-BIG-MAP-SUB-CHAR
               WHEN '#'
                   CONTINUE
               WHEN '['
               WHEN ']'
                   EVALUATE TRUE     
                     WHEN DIR-UP
                         PERFORM 3500-PUSH-BOX-UP    THRU 3500-EXIT
                     WHEN DIR-DOWN
                         CONTINUE
                     WHEN DIR-LEFT                       
                         PERFORM 3700-PUSH-BOX-LEFT  THRU 3700-EXIT
                     WHEN DIR-RIGHT
                         PERFORM 3800-PUSH-BOX-RIGHT THRU 3800-EXIT
                 END-EVALUATE
           END-EVALUATE

           ADD 1 TO WS-INSTRUCTIONS-POINTER

           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * PUSH BOX(ES) UP                                               *
      *****************************************************************
       3500-PUSH-BOX-UP.

      *    IF LOOKING AT RIGHT SIDE, FOCUS ON LEFT SIDE
           IF WS-BIG-MAP(WS-TARGET-ROW)(WS-TARGET-COL:1) EQUALS ']'          
               SUBTRACT 1 FROM WS-TARGET-COL
           END-IF
      *    SAVE LOCATION OF BOX TO STACK SO EVERYTHING CAN BE UPDATED 
           MOVE WS-TARGET-NODE TO WS-STACK-IO
           PERFORM 7000-STACK-PUSH THRU 7000-EXIT

      *    CHECK IF NEXT LOCATION IS ALSO A BOX
       
           SUBTRACT 1 FROM WS-TARGET-ROW

           EVALUATE WS-BIG-MAP(WS-TARGET-ROW)(WS-TARGET-COL:1)
               WHEN ']'   
               WHEN '['
                   GO TO 3500-PUSH-BOX-UP
               WHEN '.'
                   CONTINUE
               WHEN '#'           
                   PERFORM 7100-STACK-POP THRU 7100-EXIT 
                       UNTIL WS-STACK-CNT EQUALS 0
                   GO TO 3500-EXIT
           END-EVALUATE

      *    REACHING THIS INSTRUCTION MEANS ALL BOXES IN LINE HAVE BEEN
      *    IDENTIFIED AND IT'S TIME TO "PUSH" THEM  
      *    CLEAR ORIGINAL LOCATION/BOXES
           MOVE '.' TO 
               WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)(WS-BIG-MAP-SUB-CHAR:1)      
           PERFORM UNTIL WS-STACK-CNT EQUALS 0  
               PERFORM 7100-STACK-POP THRU 7100-EXIT 
               MOVE '.' TO 
                      WS-BIG-MAP(WS-STACK-IO-ROW)(WS-STACK-IO-COL:1)                    
               SUBTRACT 1 FROM WS-STACK-IO-ROW
               MOVE '[' TO 
                          WS-BIG-MAP(WS-STACK-IO-ROW)(WS-STACK-IO-COL:1) 
               ADD 1 TO WS-STACK-IO-COL
               MOVE ']' TO 
                          WS-BIG-MAP(WS-STACK-IO-ROW)(WS-STACK-IO-COL:1) 
           END-PERFORM

      *    SET NEW STARTING LOCATION
           SUBTRACT 1 FROM WS-BIG-MAP-ARR-SUB
           MOVE '@' TO 
                  WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)(WS-BIG-MAP-SUB-CHAR :1)  
           .
       3500-EXIT.
           EXIT.

      *****************************************************************
      * PUSH BOX(ES) TO LEFT                                          *
      *****************************************************************
       3700-PUSH-BOX-LEFT.

           CONTINUE
           .
       3700-EXIT.
           EXIT.

      *****************************************************************
      * PUSH BOX(ES) TO RIGHT                                         *
      *****************************************************************
       3800-PUSH-BOX-RIGHT.

      *    IF LOOKING AT LEFT SIDE, FOCUS ON RIGHT SIDE
           IF WS-BIG-MAP(WS-TARGET-ROW)(WS-TARGET-COL:1) EQUALS '['          
               ADD 1 TO WS-TARGET-COL
           END-IF
      *    SAVE LOCATION OF BOX TO STACK SO EVERYTHING CAN BE UPDATED 
           MOVE WS-TARGET-NODE TO WS-STACK-IO
           PERFORM 7000-STACK-PUSH THRU 7000-EXIT

      *    CHECK IF NEXT LOCATION IS ALSO A BOX
       
           ADD 1 TO WS-TARGET-COL

           EVALUATE WS-BIG-MAP(WS-TARGET-ROW)(WS-TARGET-COL:1)
               WHEN '['
                   GO TO 3800-PUSH-BOX-RIGHT
               WHEN '.'
                   CONTINUE
               WHEN '#'
                   PERFORM 7100-STACK-POP THRU 7100-EXIT 
                       UNTIL WS-STACK-CNT EQUALS 0
                   GO TO 3800-EXIT
           END-EVALUATE

      *    REACHING THIS INSTRUCTION MEANS ALL BOXES IN LINE HAVE BEEN
      *    IDENTIFIED AND IT'S TIME TO "PUSH" THEM  
      *    CLEAR ORIGINAL LOCATION/BOXES
           MOVE '.' TO 
                   WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)(WS-BIG-MAP-SUB-CHAR:1)
           PERFORM UNTIL WS-STACK-CNT EQUALS 0  
               PERFORM 7100-STACK-POP THRU 7100-EXIT           
               ADD 1 TO WS-STACK-IO-COL
               MOVE ']' TO 
                          WS-BIG-MAP(WS-STACK-IO-ROW)(WS-STACK-IO-COL:1) 
               SUBTRACT 1 FROM WS-STACK-IO-COL
               MOVE '[' TO 
                          WS-BIG-MAP(WS-STACK-IO-ROW)(WS-STACK-IO-COL:1) 
           END-PERFORM

           MOVE '[' TO WS-BIG-MAP(WS-STACK-IO-ROW)(WS-STACK-IO-COL:1)
           MOVE WS-STACK-IO-ROW TO WS-BIG-MAP-ARR-SUB
           MOVE WS-STACK-IO-COL TO WS-BIG-MAP-SUB-CHAR             
           SUBTRACT 1 FROM WS-STACK-IO-COL
           
      *    SET NEW STARTING LOCATION
           MOVE '@' TO WS-BIG-MAP(WS-STACK-IO-ROW)(WS-STACK-IO-COL:1)  
           MOVE WS-STACK-IO-ROW TO WS-BIG-MAP-ARR-SUB   
           MOVE WS-STACK-IO-COL TO WS-BIG-MAP-SUB-CHAR  
           .
       3800-EXIT.
           EXIT.

      *****************************************************************
      * PUSH AN ITEM ONTO STACK                                       *
      *****************************************************************
       7000-STACK-PUSH.

           ADD 1 TO WS-STACK-CNT
           MOVE WS-STACK-IO TO WS-STACK-ITEM(WS-STACK-CNT)
           .
       7000-EXIT.

      *****************************************************************
      * POP AN ITEM OFF STACK                                         *
      *****************************************************************
       7100-STACK-POP.

           IF WS-STACK-CNT GREATER 0
               MOVE WS-STACK-ITEM(WS-STACK-CNT) TO WS-STACK-IO
               MOVE SPACES TO WS-STACK-ITEM(WS-STACK-CNT)
               SUBTRACT 1 FROM WS-STACK-CNT
           ELSE
               DISPLAY 'ERROR: ATTEMPTED TO POP FROM EMPTY STACK'
               PERFORM 9999-ABEND THRU 9999-EXIT
           END-IF
           .
       7100-EXIT.
           EXIT.

      *****************************************************************
      * PEEK WHICH ITEM IS ON TOP OF STACK                            *
      *****************************************************************
       7200-STACK-PEEK.

           MOVE WS-STACK-ITEM(WS-STACK-CNT) TO WS-STACK-IO
           .
       7200-EXIT.
           EXIT.    

      *****************************************************************
      * DISPLAY SUM OF GPS COORDINATES                                *
      *****************************************************************
       8000-DISPLAY-RESULTS.
            
TEST       MOVE 1 TO WS-BIG-MAP-ARR-SUB
TEST       PERFORM UNTIL WS-BIG-MAP-ARR-SUB EQUALS WS-ARR-LENGTH
TEST          DISPLAY WS-BIG-MAP(WS-BIG-MAP-ARR-SUB)(1:20)
TEST       ADD 1 TO WS-BIG-MAP-ARR-SUB
TEST       END-PERFORM            
           MOVE 1 TO WS-BIG-MAP-ARR-SUB
           PERFORM UNTIL WS-BIG-MAP-ARR-SUB GREATER WS-ARR-LENGTH
               MOVE 1 TO WS-BIG-MAP-SUB-CHAR
               PERFORM UNTIL WS-BIG-MAP-SUB-CHAR GREATER WS-ARR-LENGTH
                   IF WS-MAP(WS-BIG-MAP-ARR-SUB)(WS-BIG-MAP-SUB-CHAR:1)
                      EQUALS '['
                       COMPUTE WS-SUM = (100 * (WS-BIG-MAP-ARR-SUB - 1)) 
                               + WS-SUM + (WS-BIG-MAP-SUB-CHAR - 1)
                   END-IF
                   ADD 1 TO WS-BIG-MAP-SUB-CHAR
               END-PERFORM
               ADD 1 TO WS-BIG-MAP-ARR-SUB
           END-PERFORM

           DISPLAY 'TOTAL SUM = ' WS-SUM
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

      ****************************************************************
      * ABEND PARAGRAPH IF A FATAL ERROR IS FOUND                    *
      ****************************************************************
       9999-ABEND.
           
           DISPLAY 'ABENDING PROGRAM'
           STOP RUN
           .
       9999-EXIT.
           EXIT.                      