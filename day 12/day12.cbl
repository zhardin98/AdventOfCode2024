       IDENTIFICATION DIVISION.
       PROGRAM-ID. GARDEN-GROUPS.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 12 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 12 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/12                  *
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
       01  WS-FINISH-PROCESSING                   PIC X(1).
           88 FINISH-PROCESSING                             VALUE 'Y'.
           88 NOT-FINISH-PROCESSING                         VALUE 'N'.


       01  WS-MAP-ARR.
           05 WS-MAP OCCURS 10 TO 140 TIMES DEPENDING ON ARR-LENGTH
                                                  PIC X(140).
       01  WS-MAP-ARR-SUBSCRIPTS.
           05  ARR-LENGTH                          PIC 9(3)  VALUE 0.
           05  MAP-ARR-SUB                         PIC 9(3)  VALUE 1.
           05  MAP-SUB-CHAR                        PIC 9(3).     

       01  WS-ID-ARR.
           05 WS-ID OCCURS 10 TO 140 TIMES DEPENDING ON ARR-LENGTH
                                                  PIC X(140).     
       01  WS-REGION-VALUE                        PIC X(1).  

       01  WS-CURRENT-NODE.
           05 WS-CURR-NODE-ROW                    PIC 9(3).
           05 WS-CURR-NODE-COL                    PIC 9(3).

       01  WS-MAP-ID                              PIC X(1) VALUE ' '.

      *    STACK FOR LOCATIONS VISITED            
       01  WS-STACK-TABLE.
           05 WS-STACK-ITEM
           OCCURS 1 TO 19600 TIMES DEPENDING ON WS-STACK-CNT.
               10 WS-STACK-ITEM-ROW               PIC 9(3).
               10 WS-STACK-ITEM-COL               PIC 9(3).
       01  WS-STACK-CNT                           PIC 9(4) VALUE 0.

       01  WS-STACK-IO.
           05 WS-STACK-IO-ROW                     PIC 9(3).
           05 WS-STACK-IO-COL                     PIC 9(3).       

       01  WS-NODE-DIFF-ROW                       PIC S9(3).
       01  WS-NODE-DIFF-COL                       PIC S9(3).

       01  WS-PRIOR-NODE.
           05 WS-PREV-NODE-ROW                    PIC 9(3).
           05 WS-PREV-NODE-COL                    PIC 9(3).

       01  WS-PERM-ARR.
           05 WS-PERM OCCURS 1000 TIMES           PIC 9(10). 

       01  WS-AREA-ARR.
           05 WS-AREA OCCURS 1000 TIMES           PIC 9(10). 

       01  WS-CALC-SUB                            PIC 9(4).

       01  WS-TOTAL-PRICE                         PIC 9(10) VALUE 0.

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
           PERFORM 3000-IDENTIFY-REGIONS      THRU 3000-EXIT
           PERFORM 4000-CALCULATIONS          THRU 4000-EXIT
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
      * IDENTIFY UNIQUE REGIONS                                       *
      *****************************************************************
       3000-IDENTIFY-REGIONS.
       
           PERFORM 3010-INITIALIZE-MAP THRU 3010-EXIT
           PERFORM UNTIL FINISH-PROCESSING      
               PERFORM 3020-FIND-MIN-POS   THRU 3020-EXIT   
               IF FINISH-PROCESSING
                   GO TO 3000-EXIT
               END-IF
               PERFORM 3005-INCREMENT-MAP-ID THRU 3005-EXIT
               MOVE WS-MAP(MAP-ARR-SUB)(MAP-SUB-CHAR:1) 
                                    TO WS-REGION-VALUE 
               MOVE MAP-ARR-SUB     TO WS-CURR-NODE-ROW  
               MOVE MAP-SUB-CHAR    TO WS-CURR-NODE-COL
               PERFORM 7000-STACK-PUSH THRU 7000-EXIT
               PERFORM 3100-SEARCH-NEIGHBORS THRU 3100-EXIT
           END-PERFORM
           .

       3000-EXIT.
           EXIT.

      *****************************************************************
      * "INCREMENT" MAP ID                                            *
      *****************************************************************
       3005-INCREMENT-MAP-ID.

           EVALUATE WS-MAP-ID
               WHEN ' '
                   MOVE 'A' TO WS-MAP-ID
               WHEN 'A' 
                   MOVE 'B' TO WS-MAP-ID
               WHEN 'B' 
                   MOVE 'C' TO WS-MAP-ID
               WHEN 'C' 
                   MOVE 'D' TO WS-MAP-ID
               WHEN 'D' 
                   MOVE 'E' TO WS-MAP-ID
               WHEN 'E' 
                   MOVE 'F' TO WS-MAP-ID
               WHEN 'F' 
                   MOVE 'G' TO WS-MAP-ID
               WHEN 'G' 
                   MOVE 'H' TO WS-MAP-ID
               WHEN 'H' 
                   MOVE 'I' TO WS-MAP-ID
               WHEN 'I' 
                   MOVE 'J' TO WS-MAP-ID
               WHEN 'J' 
                   MOVE 'K' TO WS-MAP-ID
               WHEN 'K' 
                   MOVE 'L' TO WS-MAP-ID
               WHEN 'L' 
                   MOVE 'M' TO WS-MAP-ID
               WHEN 'M'                *> N SKIPPED SINCE THAT IS A FLAG
                   MOVE 'O' TO WS-MAP-ID
               WHEN 'O' 
                   MOVE 'P' TO WS-MAP-ID
               WHEN 'P' 
                   MOVE 'Q' TO WS-MAP-ID
               WHEN 'Q' 
                   MOVE 'R' TO WS-MAP-ID
               WHEN 'R' 
                   MOVE 'S' TO WS-MAP-ID
               WHEN 'S' 
                   MOVE 'T' TO WS-MAP-ID
               WHEN 'T' 
                   MOVE 'U' TO WS-MAP-ID
               WHEN 'U' 
                   MOVE 'V' TO WS-MAP-ID
               WHEN 'V' 
                   MOVE 'W' TO WS-MAP-ID
               WHEN 'W' 
                   MOVE 'X' TO WS-MAP-ID
               WHEN 'X' 
                   MOVE 'Y' TO WS-MAP-ID
               WHEN 'Y' 
                   MOVE 'Z' TO WS-MAP-ID
               WHEN 'Z' 
                   MOVE 'a' TO WS-MAP-ID
               WHEN 'a' 
                   MOVE 'b' TO WS-MAP-ID
               WHEN 'b' 
                   MOVE 'c' TO WS-MAP-ID
               WHEN 'c' 
                   MOVE 'd' TO WS-MAP-ID
               WHEN 'd' 
                   MOVE 'e' TO WS-MAP-ID
               WHEN 'e' 
                   MOVE 'f' TO WS-MAP-ID
               WHEN 'f' 
                   MOVE 'g' TO WS-MAP-ID
               WHEN 'g' 
                   MOVE 'h' TO WS-MAP-ID
               WHEN 'h' 
                   MOVE 'i' TO WS-MAP-ID
               WHEN 'i' 
                   MOVE 'j' TO WS-MAP-ID
               WHEN 'j' 
                   MOVE 'k' TO WS-MAP-ID
               WHEN 'k' 
                   MOVE 'l' TO WS-MAP-ID
               WHEN 'l' 
                   MOVE 'm' TO WS-MAP-ID
               WHEN 'm'                
                   MOVE 'n' TO WS-MAP-ID
               WHEN 'n'
                   MOVE 'o' TO WS-MAP-ID
               WHEN 'o' 
                   MOVE 'p' TO WS-MAP-ID
               WHEN 'p' 
                   MOVE 'q' TO WS-MAP-ID
               WHEN 'q' 
                   MOVE 'r' TO WS-MAP-ID
               WHEN 'r' 
                   MOVE 's' TO WS-MAP-ID
               WHEN 's' 
                   MOVE 't' TO WS-MAP-ID
               WHEN 't' 
                   MOVE 'u' TO WS-MAP-ID
               WHEN 'u' 
                   MOVE 'v' TO WS-MAP-ID
               WHEN 'v' 
                   MOVE 'w' TO WS-MAP-ID
               WHEN 'w' 
                   MOVE 'x' TO WS-MAP-ID
               WHEN 'x' 
                   MOVE 'y' TO WS-MAP-ID
               WHEN 'y' 
                   MOVE 'z' TO WS-MAP-ID
               WHEN 'z' 
                   MOVE '0' TO WS-MAP-ID   
               WHEN '0' 
                   MOVE '1' TO WS-MAP-ID
               WHEN '1' 
                   MOVE '2' TO WS-MAP-ID
               WHEN '2' 
                   MOVE '3' TO WS-MAP-ID
               WHEN '3' 
                   MOVE '4' TO WS-MAP-ID
               WHEN '4' 
                   MOVE '5' TO WS-MAP-ID
               WHEN '5' 
                   MOVE '6' TO WS-MAP-ID
               WHEN '6' 
                   MOVE '7' TO WS-MAP-ID
               WHEN '7' 
                   MOVE '8' TO WS-MAP-ID
               WHEN '8' 
                   MOVE '9' TO WS-MAP-ID
               WHEN '9' 
                   MOVE '!' TO WS-MAP-ID   
               WHEN '!' 
                   MOVE '"' TO WS-MAP-ID
               WHEN '"' 
                   MOVE '#' TO WS-MAP-ID
               WHEN '#' 
                   MOVE '$' TO WS-MAP-ID
               WHEN '$' 
                   MOVE '%' TO WS-MAP-ID
               WHEN '%' 
                   MOVE '&' TO WS-MAP-ID
               WHEN '&' 
                   MOVE '\' TO WS-MAP-ID
               WHEN '\'
                   MOVE '(' TO WS-MAP-ID
               WHEN '(' 
                   MOVE ')' TO WS-MAP-ID
               WHEN ')'
                   MOVE '*' TO WS-MAP-ID
               WHEN '*'
                   MOVE '+' TO WS-MAP-ID
               WHEN '+'
                   MOVE ',' TO WS-MAP-ID
               WHEN ','
                   MOVE '-' TO WS-MAP-ID
               WHEN '-'
                   MOVE '.' TO WS-MAP-ID
               WHEN '.'
                   MOVE '/' TO WS-MAP-ID
               WHEN '/'
                   MOVE ':' TO WS-MAP-ID
               WHEN ':'
                   MOVE ';' TO WS-MAP-ID
               WHEN ';'
                   MOVE '<' TO WS-MAP-ID
               WHEN '<'
                   MOVE '=' TO WS-MAP-ID
               WHEN '='
                   MOVE '>' TO WS-MAP-ID
               WHEN '>'
                   MOVE '?' TO WS-MAP-ID
               WHEN '?'
                   MOVE '@' TO WS-MAP-ID
               WHEN '@'
                   MOVE '[' TO WS-MAP-ID
               WHEN '['
                   MOVE ']' TO WS-MAP-ID
               WHEN ']'
                   MOVE '^' TO WS-MAP-ID
               WHEN '^'
                   MOVE '_' TO WS-MAP-ID
               WHEN '_'
                   MOVE '`' TO WS-MAP-ID
               WHEN '`'
                   DISPLAY 'END OF ID SEQUENCE'
                   PERFORM 9999-ABEND THRU 9999-EXIT
           END-EVALUATE


           .
       3005-EXIT.
           EXIT.


      *****************************************************************
      * INTIALIZE ID MAP WITH N FOR NO VALUE                          *
      *****************************************************************
       3010-INITIALIZE-MAP.

           MOVE 1 TO MAP-ARR-SUB
           PERFORM UNTIL MAP-ARR-SUB GREATER ARR-LENGTH
               MOVE 1 TO MAP-SUB-CHAR
               PERFORM UNTIL MAP-SUB-CHAR GREATER ARR-LENGTH              
                   MOVE 'N' TO WS-ID(MAP-ARR-SUB)(MAP-SUB-CHAR:1)
                   ADD 1 TO MAP-SUB-CHAR
               END-PERFORM
               ADD 1 TO MAP-ARR-SUB
           END-PERFORM
           .
       3010-EXIT.
           EXIT.

      *****************************************************************
      * FIND NEXT UNDISCOVERED REGION                                 *
      *****************************************************************
       3020-FIND-MIN-POS.

           MOVE 1 TO MAP-ARR-SUB
           PERFORM UNTIL MAP-ARR-SUB GREATER ARR-LENGTH
               MOVE 1 TO MAP-SUB-CHAR
               PERFORM UNTIL MAP-SUB-CHAR GREATER ARR-LENGTH
                   IF WS-ID(MAP-ARR-SUB)(MAP-SUB-CHAR:1) EQUALS 'N'                    
                       GO TO 3020-EXIT
                   END-IF
                   ADD 1 TO MAP-SUB-CHAR
               END-PERFORM
               ADD 1 TO MAP-ARR-SUB
           END-PERFORM      

      *    REACHING THIS INSTRUCTION MEANS THE ENTIRE MAP HAS BEEN
      *    COVERED AND CALCULATED.
           SET FINISH-PROCESSING TO TRUE             
           .
       3020-EXIT.
           EXIT.

      *****************************************************************
      * SEARCH NEARBY NODES AND DETERMINE IF THEY BELONG TO REGION    *
      *****************************************************************
       3100-SEARCH-NEIGHBORS.

           LOOK-FOR-PATH.    
      *    MARK NODE AS VISITED                  
           MOVE WS-MAP-ID TO 
                            WS-ID(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL:1)
           MOVE WS-CURRENT-NODE TO WS-STACK-IO
      *    ADD NODE TO STACK       
           PERFORM 7000-STACK-PUSH THRU 7000-EXIT 
           .
           LOOK-UP.
      *    MOVE UP IF POSSIBLE
           IF  WS-CURR-NODE-ROW GREATER 1 
               IF WS-MAP(WS-CURR-NODE-ROW - 1)(WS-CURR-NODE-COL:1)
                  EQUALS WS-REGION-VALUE                   AND 
                  WS-ID(WS-CURR-NODE-ROW - 1)(WS-CURR-NODE-COL:1)
                  EQUALS 'N'
                   SUBTRACT 1 FROM WS-CURR-NODE-ROW
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           LOOK-RIGHT.
      *    MOVE RIGHT IF POSSIBLE
           IF  WS-CURR-NODE-COL LESS ARR-LENGTH               
               IF WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL + 1 :1)
                  EQUALS WS-REGION-VALUE                   AND 
                  WS-ID(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL + 1 :1)
                  EQUALS 'N'            
                   ADD 1 TO WS-CURR-NODE-COL
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           LOOK-DOWN.
      *    MOVE DOWN IF POSSIBLE
           IF  WS-CURR-NODE-ROW LESS ARR-LENGTH
               IF WS-MAP(WS-CURR-NODE-ROW + 1)(WS-CURR-NODE-COL:1)
                  EQUALS WS-REGION-VALUE                   AND 
                  WS-ID(WS-CURR-NODE-ROW + 1)(WS-CURR-NODE-COL:1)
                  EQUALS 'N'                   
                   ADD 1 TO WS-CURR-NODE-ROW
                   GO TO LOOK-FOR-PATH
               END-IF
           END-IF
           .
           LOOK-LEFT.
      *    MOVE LEFT IF POSSIBLE
           IF  WS-CURR-NODE-COL GREATER 1               
               IF WS-MAP(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL - 1 :1)
                  EQUALS WS-REGION-VALUE                   AND 
                  WS-ID(WS-CURR-NODE-ROW)(WS-CURR-NODE-COL - 1 :1)
                  EQUALS 'N'     
                   SUBTRACT 1 FROM WS-CURR-NODE-COL
                   GO TO LOOK-FOR-PATH
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
      * PROCESS CALCULATIONS                                          *
      *****************************************************************
       4000-CALCULATIONS.

           MOVE ' ' TO WS-MAP-ID
           MOVE 0   TO WS-CALC-SUB
           PERFORM 4100-INITIALIZE-CALC-ARRAYS THRU 4100-EXIT
           MOVE 1 TO WS-CALC-SUB
           PERFORM UNTIL WS-MAP-ID EQUALS '/'
                  PERFORM 3005-INCREMENT-MAP-ID       THRU 3005-EXIT
                  PERFORM 4200-CALCULATE-AREA         THRU 4200-EXIT
                  PERFORM 4300-CALCULATE-PERIMETER    THRU 4300-EXIT
                  ADD 1 TO WS-CALC-SUB
           END-PERFORM

           .
       4000-EXIT.
           EXIT.

      *****************************************************************
      * INITIALIZE PERIMETER AND AREA ARRAYS                          *
      *****************************************************************
       4100-INITIALIZE-CALC-ARRAYS.

           MOVE 1 TO WS-CALC-SUB

           PERFORM UNTIL WS-CALC-SUB GREATER 1000
               MOVE 0 TO WS-PERM(WS-CALC-SUB)
                         WS-AREA(WS-CALC-SUB)
               ADD 1 TO WS-CALC-SUB
           END-PERFORM
           .
       4100-EXIT.
           EXIT.

      *****************************************************************
      * GET AREA OF CURRENT REGION                                    *
      *****************************************************************
       4200-CALCULATE-AREA.

           MOVE 1 TO MAP-ARR-SUB
           PERFORM UNTIL MAP-ARR-SUB GREATER ARR-LENGTH
               MOVE 1 TO MAP-SUB-CHAR
               PERFORM UNTIL MAP-SUB-CHAR GREATER ARR-LENGTH
                   IF WS-ID(MAP-ARR-SUB)(MAP-SUB-CHAR:1) 
                      EQUALS WS-MAP-ID                     
                       ADD 1 TO WS-AREA(WS-CALC-SUB)
                   END-IF
                   ADD 1 TO MAP-SUB-CHAR
               END-PERFORM
               ADD 1 TO MAP-ARR-SUB
           END-PERFORM 
           .
       4200-EXIT.
           EXIT.

      *****************************************************************
      * GET PERIMETER OF CURRENT REGION                               *
      *****************************************************************
       4300-CALCULATE-PERIMETER.

           MOVE 1 TO MAP-ARR-SUB
           PERFORM UNTIL MAP-ARR-SUB GREATER ARR-LENGTH
               MOVE 1 TO MAP-SUB-CHAR
               PERFORM UNTIL MAP-SUB-CHAR GREATER ARR-LENGTH
                   IF WS-ID(MAP-ARR-SUB)(MAP-SUB-CHAR:1) 
                      EQUALS WS-MAP-ID   
                       PERFORM 4310-CHECK-DIRECTIONS THRU 4310-EXIT
                   END-IF
                   ADD 1 TO MAP-SUB-CHAR
               END-PERFORM
               ADD 1 TO MAP-ARR-SUB
           END-PERFORM            
           .
       4300-EXIT.
           EXIT.

      *****************************************************************
      * CHECK IN ALL DIRECTIONS AND SEE IF A "BORDER" EXISTS          *
      *****************************************************************
       4310-CHECK-DIRECTIONS.

      *    CHECK UP
           IF MAP-ARR-SUB - 1 EQUALS 0 OR
              WS-ID(MAP-ARR-SUB - 1)(MAP-SUB-CHAR:1) 
                                NOT EQUALS WS-MAP-ID   
               ADD 1 TO WS-PERM(WS-CALC-SUB)
           END-IF
      *    CHECK DOWN
           IF MAP-ARR-SUB + 1 GREATER ARR-LENGTH OR           
              WS-ID(MAP-ARR-SUB + 1)(MAP-SUB-CHAR:1)
                                NOT EQUALS WS-MAP-ID                               
               ADD 1 TO WS-PERM(WS-CALC-SUB)
           END-IF
      *    CHECK RIGHT
           IF MAP-SUB-CHAR + 1 EQUALS 0 OR
              WS-ID(MAP-ARR-SUB)(MAP-SUB-CHAR + 1:1)
                                NOT EQUALS WS-MAP-ID                             
               ADD 1 TO WS-PERM(WS-CALC-SUB)
           END-IF      
      *    CHCEK LEFT
           IF MAP-SUB-CHAR - 1 GREATER ARR-LENGTH OR
              WS-ID(MAP-ARR-SUB)(MAP-SUB-CHAR - 1:1)
                                NOT EQUALS WS-MAP-ID                             
               ADD 1 TO WS-PERM(WS-CALC-SUB)
           END-IF       
           .
       4310-EXIT.
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
      * DISPLAY AMOUNT OF STONES                                      *
      *****************************************************************
       8000-DISPLAY-RESULTS.

           MOVE 1 TO WS-CALC-SUB
           PERFORM UNTIL WS-PERM(WS-CALC-SUB) EQUALS 0
               COMPUTE WS-TOTAL-PRICE = WS-TOTAL-PRICE +
                           (WS-PERM(WS-CALC-SUB) * WS-AREA(WS-CALC-SUB))  
               ADD 1 TO WS-CALC-SUB
           END-PERFORM
           DISPLAY 'TOTAL PRICE = ' WS-TOTAL-PRICE
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