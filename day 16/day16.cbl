       IDENTIFICATION DIVISION.
       PROGRAM-ID. REINDEER-MAZE.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 16 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 16 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/16                  *
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
           RECORD CONTAINS 141 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                          PIC X(141).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.

       01  WS-MAP-ARR.
           05 WS-MAP OCCURS 15 TO 141 TIMES DEPENDING ON ARR-LENGTH
                                                  PIC X(141).
       01  WS-MAP-ARR-SUBSCRIPTS.
           05  ARR-LENGTH                         PIC 9(3)  VALUE 0.     
           05  MAP-ARR-SUB                        PIC 9(3)  VALUE 1.
           05  MAP-SUB-CHAR                       PIC 9(3).

       01  WS-VISITED-ARR.
           05 WS-VISITED 
                    OCCURS 15 TO 141 TIMES DEPENDING ON ARR-LENGTH
                                                  PIC X(141).


      *    STACK FOR LOCATIONS VISITED            
       01  WS-STACK-TABLE.
           05 WS-STACK-ITEM
           OCCURS 1 TO 19881 TIMES DEPENDING ON WS-STACK-CNT.
               10 WS-STACK-ITEM-ROW               PIC 9(3).
               10 WS-STACK-ITEM-COL               PIC 9(3).
       01  WS-STACK-CNT                           PIC 9(5)  VALUE 0.

       01  WS-STACK-IO.
           05 WS-STACK-IO-ROW                     PIC 9(3).
           05 WS-STACK-IO-COL                     PIC 9(3).

       01  WS-CURRENT-NODE.
           05 WS-CURR-NODE-ROW                    PIC 9(3).
           05 WS-CURR-NODE-COL                    PIC 9(3).
       01  WS-PRIOR-NODE.
           05 WS-PREV-NODE-ROW                    PIC 9(3).
           05 WS-PREV-NODE-COL                    PIC 9(3).

       01  WS-NODE-DIFF-ROW                       PIC S9(1).
       01  WS-NODE-DIFF-COL                       PIC S9(1).      

       01  WS-TURNS                               PIC 9(5).
       01  WS-TURN-PTR                            PIC 9(4).
       01  WS-TURN-CURR-NODE.
           05 WS-TURN-CURR-NODE-ROW               PIC 9(3).
           05 WS-TURN-CURR-NODE-COL               PIC 9(3).
       01  WS-TURN-NEXT-NODE.
           05 WS-TURN-NEXT-NODE-ROW               PIC 9(3).
           05 WS-TURN-NEXT-NODE-COL               PIC 9(3).

       01  WS-DIRECTION                           PIC X(1).
           88 DIR-UP                                        VALUE '^'.
           88 DIR-DOWN                                      VALUE 'v'.
           88 DIR-LEFT                                      VALUE '<'.
           88 DIR-RIGHT                                     VALUE '>'.

       01  WS-CURRENT-SCORE                       PIC 9(10).
       01  WS-FINAL-SCORE                         PIC 9(10).
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
           PERFORM 3000-FIND-START            THRU 3000-EXIT
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
      * IDENTIFYSTARTING POINT                                        *
      *****************************************************************
       3000-FIND-START.

           MOVE 1 TO MAP-ARR-SUB
           PERFORM UNTIL MAP-ARR-SUB EQUALS(ARR-LENGTH + 1)
               MOVE 1 TO MAP-SUB-CHAR
               PERFORM UNTIL MAP-SUB-CHAR EQUALS(ARR-LENGTH + 1)   
                   IF WS-MAP(MAP-ARR-SUB)(MAP-SUB-CHAR:1) EQUALS 'S'
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
      * DETERMINE HOW MANY COMPLETE PATHS EXIST                       *
      *****************************************************************
       3100-CALCULATE-SCORE.

      *    START POINTERS
           MOVE MAP-ARR-SUB     TO WS-CURR-NODE-ROW  
           MOVE MAP-SUB-CHAR    TO WS-CURR-NODE-COL
           MOVE 0               TO WS-CURRENT-SCORE
           MOVE SPACES          TO WS-VISITED-ARR
           .
           LOOK-FOR-PATH.       
      *    CHECK IF ON END POINT
           IF WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1) EQUALS 'E' 
TEST  *     DISPLAY 'END REACHED'
TEST  *     DISPLAY 'STACK = ' WS-STACK-TABLE    
               PERFORM 3900-GET-TURNS THRU 3900-EXIT   
               MOVE WS-STACK-CNT TO WS-CURRENT-SCORE       
               MULTIPLY 1000 BY WS-TURNS
               ADD WS-TURNS TO WS-CURRENT-SCORE              
               IF WS-CURRENT-SCORE LESS WS-FINAL-SCORE 
               OR WS-FINAL-SCORE EQUALS 0
TEST       DISPLAY 'SCORE = ' WS-CURRENT-SCORE                
                   MOVE WS-CURRENT-SCORE TO WS-FINAL-SCORE
               END-IF
               GO TO POP-STACK
           END-IF         

      *    ADD NODE TO STACK     
           MOVE WS-CURRENT-NODE TO WS-STACK-IO         
           PERFORM 7000-STACK-PUSH THRU 7000-EXIT
           MOVE 'Y' TO WS-VISITED(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
           .
           LOOK-UP.
      *    MOVE UP IF POSSIBLE
           IF  WS-CURR-NODE-ROW GREATER 1       
               IF (WS-MAP(WS-CURR-NODE-ROW - 1)(WS-CURR-NODE-COL:1) 
                  EQUALS '.' 
               OR WS-MAP(WS-CURR-NODE-ROW - 1)(WS-CURR-NODE-COL:1) 
                  EQUALS 'E')  
              AND WS-VISITED(WS-CURR-NODE-ROW - 1)(WS-CURR-NODE-COL:1)
                  NOT EQUALS 'Y'    
                   SUBTRACT 1 FROM WS-CURR-NODE-ROW   
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           LOOK-RIGHT.        
      *    MOVE RIGHT IF POSSIBLE
           IF  WS-CURR-NODE-COL LESS ARR-LENGTH
               IF (WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL + 1:1) 
                  EQUALS '.'  
               OR WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL + 1:1) 
                  EQUALS 'E')  
              AND WS-VISITED(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL + 1:1)
                  NOT EQUALS 'Y'                
                   ADD 1 TO WS-CURR-NODE-COL
                   GO TO LOOK-FOR-PATH
           END-IF
           .
           LOOK-DOWN.
      *    MOVE DOWN IF POSSIBLE
           IF  WS-CURR-NODE-ROW LESS ARR-LENGTH    
               IF (WS-MAP(WS-CURR-NODE-ROW + 1)(WS-CURR-NODE-COL:1) 
                  EQUALS '.' 
               OR WS-MAP(WS-CURR-NODE-ROW + 1)(WS-CURR-NODE-COL:1) 
                  EQUALS 'E')  
              AND WS-VISITED(WS-CURR-NODE-ROW + 1)(WS-CURR-NODE-COL:1)
                  NOT EQUALS 'Y'                                                   
                   ADD 1 TO WS-CURR-NODE-ROW         
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           LOOK-LEFT.
      *    MOVE LEFT IF POSSIBLE
           IF  WS-CURR-NODE-COL GREATER 1
               IF (WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL - 1:1) 
                  EQUALS '.'  
               OR WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL - 1:1) 
                  EQUALS 'E')  
              AND WS-VISITED(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL - 1:1)
                  NOT EQUALS 'Y'  
                   SUBTRACT 1 FROM WS-CURR-NODE-COL
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           POP-STACK.
           IF WS-STACK-CNT GREATER 0
               PERFORM 7100-STACK-POP THRU 7100-EXIT    
               MOVE ' ' TO 
                          WS-VISITED(WS-STACK-IO-ROW)(WS-STACK-IO-COL:1)      
      *    EXIT CONDITION: STACK HAS ONE NODE (STARTING POINT) REMAINING
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
      * LOOP THROUGH STACK AND DETERMINE AMOUNT OF TURNS REQUIRED     *
      ***************************************************************** 
       3900-GET-TURNS.

           MOVE 0      TO WS-TURNS
           MOVE 1      TO WS-TURN-PTR
           MOVE SPACES TO WS-DIRECTION

           PERFORM UNTIL (WS-TURN-PTR + 1) EQUALS WS-STACK-CNT
               MOVE WS-STACK-ITEM(WS-TURN-PTR)     TO WS-TURN-CURR-NODE
               MOVE WS-STACK-ITEM(WS-TURN-PTR + 1) TO WS-TURN-NEXT-NODE

               EVALUATE TRUE
                   WHEN WS-TURN-CURR-NODE-ROW EQUALS 
                        WS-TURN-NEXT-NODE-ROW                  AND
                        WS-TURN-CURR-NODE-COL LESS 
                        WS-TURN-NEXT-NODE-COL
                           IF DIR-RIGHT
                               CONTINUE
                           ELSE 
                               ADD 1 TO WS-TURNS
                               SET DIR-RIGHT TO TRUE                               
                           END-IF
                   WHEN WS-TURN-CURR-NODE-ROW EQUALS 
                        WS-TURN-NEXT-NODE-ROW                  AND
                        WS-TURN-CURR-NODE-COL GREATER 
                        WS-TURN-NEXT-NODE-COL
                           IF DIR-LEFT
                               CONTINUE
                           ELSE 
                               ADD 1 TO WS-TURNS
                               SET DIR-LEFT TO TRUE                                
                           END-IF                           
                   WHEN WS-TURN-CURR-NODE-ROW LESS 
                        WS-TURN-NEXT-NODE-ROW                  AND
                        WS-TURN-CURR-NODE-COL EQUALS 
                        WS-TURN-NEXT-NODE-COL
                           IF DIR-DOWN
                               CONTINUE
                           ELSE 
                               ADD 1 TO WS-TURNS
                               SET DIR-DOWN TO TRUE                               
                           END-IF                             
                   WHEN WS-TURN-CURR-NODE-ROW GREATER
                        WS-TURN-NEXT-NODE-ROW                  AND
                        WS-TURN-CURR-NODE-COL EQUALS 
                        WS-TURN-NEXT-NODE-COL
                           IF DIR-UP
                               CONTINUE
                           ELSE 
                               ADD 1 TO WS-TURNS
                               SET DIR-UP TO TRUE                                
                           END-IF      
               END-EVALUATE
               ADD 1 TO WS-TURN-PTR
           END-PERFORM
           .
       3900-EXIT.
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
            
           DISPLAY 'LOWEST SCORE = ' WS-FINAL-SCORE
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