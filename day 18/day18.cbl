       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAM-RUN.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 18 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 18 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/18                  *
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
           RECORD CONTAINS 5 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                          PIC X(5).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                              PIC X(271)
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

       01  ARR-LENGTH                             PIC 9(2)  VALUE  71.            
       01  WS-MAP-ARR.
           05 WS-MAP OCCURS 71 TIMES              PIC X(71).
       01  MAP-ARR-SUB                            PIC 9(2) VALUE 1.
       01  MAP-SUB-CHAR                           PIC 9(2).

       01  WS-VISITED-ARR.
           05 WS-VISITED OCCURS 71 TIMES          PIC X(71).

      *    STACK FOR LOCATIONS VISITED            
       01  WS-STACK-TABLE.
           05 WS-STACK-ITEM
           OCCURS 1 TO 5041 TIMES DEPENDING ON WS-STACK-CNT.
               10 WS-STACK-ITEM-ROW               PIC 9(2).
               10 WS-STACK-ITEM-COL               PIC 9(2).
       01  WS-STACK-CNT                           PIC 9(5)  VALUE 0.

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

       01  WS-DIRECTION                           PIC X(1).
           88 DIR-UP                                        VALUE '^'.
           88 DIR-DOWN                                      VALUE 'v'.
           88 DIR-LEFT                                      VALUE '<'.
           88 DIR-RIGHT                                     VALUE '>'.  

       01  WS-INPUT.               
           05 IN-ROW                              PIC 9(2).
           05 IN-COL                              PIC 9(2).     
  
       01  WS-PATH-LENGTH                         PIC 9(4).

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
           PERFORM 3000-TRAVERSE-MAP          THRU 3000-EXIT         
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
      * READ FILE LINE BY LINE, CONVERTING EACH LINE INTO A NODE IN  *
      * AN ARRAY                                          
      ****************************************************************
       2000-CONVERT-FILE-TO-ARRAY.
       
           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END          
                   UNSTRING INPUT-RECORD DELIMITED BY ',' INTO IN-COL
                                                               IN-ROW                                                      
                   PERFORM 2100-PLACE-NODE THRU 2100-EXIT
           END-READ
           .
       2000-EXIT.
           EXIT.
 
      *****************************************************************
      * ADD GIVEN NODE TO MAP                                         *
      ***************************************************************** 
       2100-PLACE-NODE.

      *    ADD 1 DUE TO 0-DISPLACED INPUT
           ADD 1 TO  IN-COL
                     IN-ROW

           MOVE '#' TO WS-MAP(IN-ROW)(IN-COL:1)
           .
       2100-EXIT.
           EXIT.


      *****************************************************************
      * TRAVERSE FROM 1,1 TO   71,71                                    *
      *****************************************************************
       3000-TRAVERSE-MAP.

      *    START POINTERS
           MOVE 1      TO WS-CURR-NODE-ROW  
                          WS-CURR-NODE-COL
           MOVE SPACES TO WS-VISITED-ARR
           .
           LOOK-FOR-PATH.       
      *    CHECK IF ON END POINT
           IF WS-CURR-NODE-ROW EQUALS 71 AND WS-CURR-NODE-COL EQUALS 71
               IF WS-STACK-CNT LESS WS-PATH-LENGTH OR 
                  WS-PATH-LENGTH EQUALS 0
                   MOVE WS-STACK-CNT TO WS-PATH-LENGTH
               END-IF
               GO TO POP-STACK
           END-IF         

      *    ADD NODE TO STACK   
           MOVE WS-CURRENT-NODE TO WS-STACK-IO         
           PERFORM     71000-STACK-PUSH THRU 71000-EXIT          
           MOVE 'Y' TO WS-VISITED(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
           .
           LOOK-UP.
      *    MOVE UP IF POSSIBLE
           IF  WS-CURR-NODE-ROW GREATER 1               
               IF (WS-MAP(WS-CURR-NODE-ROW - 1)(WS-CURR-NODE-COL:1) 
                  EQUALS SPACE)
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
                  EQUALS SPACE)
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
                  EQUALS SPACE)
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
                  EQUALS SPACE) 
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
                   GO TO 3000-EXIT
               END-IF      
               MOVE WS-STACK-IO TO WS-PRIOR-NODE
               PERFORM 71200-STACK-PEEK THRU 71200-EXIT
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
       3000-EXIT.
           EXIT.
      *****************************************************************
      * PUSH AN ITEM ONTO STACK                                       *
      *****************************************************************
       71000-STACK-PUSH.

           ADD 1 TO WS-STACK-CNT
           MOVE WS-STACK-IO TO WS-STACK-ITEM(WS-STACK-CNT)          
           .
       71000-EXIT.

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
       71200-STACK-PEEK.

           MOVE WS-STACK-ITEM(WS-STACK-CNT) TO WS-STACK-IO
           .
       71200-EXIT.
           EXIT.  

      *****************************************************************
      * DISPLAY NUMBER OF STEPS                                       *
      *****************************************************************
       8000-DISPLAY-RESULTS.
            
            DISPLAY 'SHORTEST PATH = ' WS-PATH-LENGTH 
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