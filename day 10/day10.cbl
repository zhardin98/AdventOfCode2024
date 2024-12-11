       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOOF-IT.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 10 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 10 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/10                  *
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
           RECORD CONTAINS 45 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                           PIC X(45).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.

       01  WS-MAP-ARR.
           05 WS-MAP OCCURS 4 TO 45 TIMES DEPENDING ON ARR-LENGTH
                                                  PIC X(45).
       01  WS-MAP-ARR-SUBSCRIPTS.
           05  ARR-LENGTH                         PIC 9(2)  VALUE 0.     
           05  MAP-ARR-SUB                        PIC 9(2)  VALUE 1.
           05  MAP-SUB-CHAR                       PIC 9(2).

       01  WS-COUNTED-TRAILS-ARR.
           05 WS-COUNTED-TRAILS 
                    OCCURS 4 TO 45 TIMES DEPENDING ON ARR-LENGTH
                                                  PIC X(45).


      *    STACK FOR LOCATIONS VISITED            
       01  WS-STACK-TABLE.
           05 WS-STACK-ITEM
           OCCURS 1 TO 2025 TIMES DEPENDING ON WS-STACK-CNT.
               10 WS-STACK-ITEM-ROW               PIC 9(2).
               10 WS-STACK-ITEM-COL               PIC 9(2).
       01  WS-STACK-CNT                           PIC 9(4) VALUE 0.

       01  WS-STACK-IO.
           05 WS-STACK-IO-ROW                     PIC 9(2).
           05 WS-STACK-IO-COL                     PIC 9(2).

       01  WS-CURRENT-NODE.
           05 WS-CURR-NODE-ROW                    PIC 9(2).
           05 WS-CURR-NODE-COL                    PIC 9(2).
       01  WS-PRIOR-NODE.
           05 WS-PREV-NODE-ROW                    PIC 9(2).
           05 WS-PREV-NODE-COL                    PIC 9(2).

       01  WS-NODE-DIFF-ROW                       PIC S9(1).
       01  WS-NODE-DIFF-COL                       PIC S9(1).

       01  NODE-VAL-1-X                           PIC X(1).
       01  NODE-VAL-1-9 REDEFINES NODE-VAL-1-X    PIC 9(1).
       01  NODE-VAL-2-X                           PIC X(1).
       01  NODE-VAL-2-9 REDEFINES NODE-VAL-2-X    PIC 9(1).       

       01  WS-TOTAL-SCORE                         PIC 9(10).
       01  WS-TOTAL-RATING                        PIC 9(10).
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
           PERFORM 3000-FIND-TRAILHEADS       THRU 3000-EXIT
           PERFORM 8000-DISPLAY-RESULTS       THRU 8000-EXIT
           PERFORM 9000-CLOSE-FILE            THRU 9000-EXIT
           .
       0000-EXIT.
           GOBACK.

      ****************************************************************
      * OPEN/READ FILE                                               *
      ****************************************************************
       1000-OPEN-FILE.

           OPEN INPUT INPUT-FILE
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * READ FILE LINE BY LINE                                       *
      ****************************************************************
       2000-CONVERT-FILE-TO-ARRAY.
       
           
           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   MOVE INPUT-RECORD TO WS-MAP(MAP-ARR-SUB)
                   ADD 1 TO MAP-ARR-SUB
                            ARR-LENGTH    
           END-READ
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * IDENTIFY TRAILHEADS (0 NODES)                                 *
      *****************************************************************
       3000-FIND-TRAILHEADS.

           
           MOVE 1 TO MAP-ARR-SUB
           PERFORM UNTIL MAP-ARR-SUB EQUALS(ARR-LENGTH + 1)
               MOVE 1 TO MAP-SUB-CHAR
               PERFORM UNTIL MAP-SUB-CHAR EQUALS(ARR-LENGTH + 1)   
                   IF WS-MAP(MAP-ARR-SUB)(MAP-SUB-CHAR:1) EQUALS 0
                       PERFORM 3100-CALCULATE-SCORE THRU 3100-EXIT                 
                   END-IF
                   ADD 1 TO MAP-SUB-CHAR
               END-PERFORM                
               ADD 1 TO MAP-ARR-SUB                
           END-PERFORM           
           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * DETERMINE HOW MANY COMPLETE TRAILS EXIST FOR GIVEN TRAILHEAD  *
      *****************************************************************
       3100-CALCULATE-SCORE.

      *    START POINTERS AT TRAILHEAD
           MOVE MAP-ARR-SUB     TO WS-CURR-NODE-ROW  
           MOVE MAP-SUB-CHAR    TO WS-CURR-NODE-COL
      *    CLEAR OUT ENDPOINT ARRAY TO NOT COUNT FINAL DESTINATIONS
      *    TWICE     
           MOVE SPACES TO WS-COUNTED-TRAILS-ARR
           .
           LOOK-FOR-PATH.         
      *    ADD NODE TO STACK     
           MOVE WS-CURRENT-NODE TO WS-STACK-IO
           PERFORM 7000-STACK-PUSH THRU 7000-EXIT
           .
           LOOK-UP.
      *    MOVE UP IF POSSIBLE
           IF  WS-CURR-NODE-ROW GREATER 1 
               MOVE WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
                                                         TO NODE-VAL-1-X 
               MOVE WS-MAP(WS-CURR-NODE-ROW - 1)(WS-CURR-NODE-COL:1)
                                                         TO NODE-VAL-2-X
               IF (NODE-VAL-2-9 - NODE-VAL-1-9) EQUALS 1
                   SUBTRACT 1 FROM WS-CURR-NODE-ROW
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           LOOK-RIGHT.
      *    MOVE RIGHT IF POSSIBLE
           IF  WS-CURR-NODE-COL LESS ARR-LENGTH 
               MOVE WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
                                                         TO NODE-VAL-1-X 
               MOVE WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL + 1 :1)
                                                         TO NODE-VAL-2-X
               IF (NODE-VAL-2-9 - NODE-VAL-1-9) EQUALS 1               
                   ADD 1 TO WS-CURR-NODE-COL
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           LOOK-DOWN.
      *    MOVE DOWN IF POSSIBLE
           IF  WS-CURR-NODE-ROW LESS ARR-LENGTH
               MOVE WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
                                                         TO NODE-VAL-1-X 
               MOVE WS-MAP(WS-CURR-NODE-ROW + 1)(WS-CURR-NODE-COL:1)
                                                         TO NODE-VAL-2-X
               IF (NODE-VAL-2-9 - NODE-VAL-1-9) EQUALS 1                    
                   ADD 1 TO WS-CURR-NODE-ROW
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           LOOK-LEFT.
      *    MOVE LEFT IF POSSIBLE
           IF  WS-CURR-NODE-COL GREATER 1 
               MOVE WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
                                                         TO NODE-VAL-1-X 
               MOVE WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL - 1:1)
                                                         TO NODE-VAL-2-X                
               IF (NODE-VAL-2-9 - NODE-VAL-1-9) EQUALS 1   
                   SUBTRACT 1 FROM WS-CURR-NODE-COL
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF

      *    INDENTED 2 INSTEAD OF 4 DUE TO SPACING ISSUES
           IF WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1) EQUALS '9' 
             ADD 1 TO WS-TOTAL-RATING
             IF WS-COUNTED-TRAILS(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
                NOT EQUALS 'Y'             
               ADD 1 TO WS-TOTAL-SCORE
               MOVE 'Y' TO 
                 WS-COUNTED-TRAILS(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
               END-IF
           END-IF
           .
           POP-STACK.
           IF WS-STACK-CNT GREATER 0
               PERFORM 7100-STACK-POP THRU 7100-EXIT
      *    EXIT CONDITION: STACK HAS ONE NODE (TRAILHEAD) REMAINING
               IF WS-STACK-CNT EQUALS 0 
                   GO TO 3100-EXIT
               END-IF      
               MOVE WS-STACK-IO TO WS-PRIOR-NODE
               PERFORM 7200-STACK-PEEK THRU 7200-EXIT
               MOVE WS-STACK-IO TO WS-CURRENT-NODE
               SUBTRACT WS-PREV-NODE-ROW FROM WS-CURR-NODE-ROW 
                                                 GIVING WS-NODE-DIFF-ROW
               SUBTRACT WS-PREV-NODE-COL FROM WS-CURR-NODE-COL 
                                                 GIVING WS-NODE-DIFF-COL    
               EVALUATE TRUE
      *            NODE MOVED DOWN
                   WHEN WS-NODE-DIFF-ROW EQUALS  1 AND 
                        WS-NODE-DIFF-COL EQUALS  0
                       GO TO LOOK-RIGHT
      *            NODE MOVED UP              
                   WHEN WS-NODE-DIFF-ROW EQUALS -1 AND 
                        WS-NODE-DIFF-COL EQUALS  0
                       GO TO LOOK-LEFT
      *            NODE MOVED LEFT
                   WHEN WS-NODE-DIFF-ROW EQUALS  0 AND 
                        WS-NODE-DIFF-COL EQUALS -1
                       GO TO LOOK-DOWN
      *            NODE MOVED RIGHT             
                   WHEN WS-NODE-DIFF-ROW EQUALS  0 AND 
                        WS-NODE-DIFF-COL EQUALS  1
                       GO TO POP-STACK                     
               END-EVALUATE     
           END-IF
           .
       3100-EXIT.
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
      * DISPLAY SCORE OF TRAIL                                        *
      *****************************************************************
       8000-DISPLAY-RESULTS.
            
           DISPLAY 'TOTAL SCORE  = ' WS-TOTAL-SCORE
           DISPLAY 'TOTAL RATING = ' WS-TOTAL-RATING
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