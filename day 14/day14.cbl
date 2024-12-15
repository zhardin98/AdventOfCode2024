       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLAW-CONTRAPTION.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 13 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 13 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/13                  *
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
       01  INPUT-RECORD                           PIC X(20).                       

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.

       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.

       01  WS-TREE-FOUND                          PIC X(1).
           88 TREE-FOUND                                    VALUE 'Y'.
           88 TREE-NOT-FOUND                                VALUE 'N'.

       01  WS-NUM-SIGN                            PIC X(1).
           88 NEGATIVE-NUMBER                               VALUE '-'.
           88 POSITIVE-NUMBER                               VALUE '+'.

       01  WS-INREC-POINTER                       PIC 9(2).
       01  WS-DIGIT-CNT                           PIC 9(2).

      *VALUES ARE 1 LESS THAN SPECIFIED IN PROBLEM DUE TO 0-BASE
       01  C-MAP-HEIGHT                          PIC 9(3)  VALUE 102.
       01  C-MAP-WIDTH                           PIC 9(3)  VALUE 100.

       01  WS-MID-HEIGHT                         PIC 9(3).
       01  WS-MID-WIDTH                          PIC 9(3).

       01  WS-ROB-POS.
           05 WS-ROB-POS-X                        PIC S9(3).
           05 WS-ROB-POS-Y                        PIC S9(3).

       01  WS-ROB-VEL.
           05 WS-ROB-VEL-X                        PIC S9(3).
           05 WS-ROB-VEL-Y                        PIC S9(3).

       01  WS-SAFETY-FACTOR                       PIC 9(10) VALUE 0.
       
       01  WS-QUAD1-SAFETY                        PIC 9(4)  VALUE 0.
       01  WS-QUAD2-SAFETY                        PIC 9(4)  VALUE 0.
       01  WS-QUAD3-SAFETY                        PIC 9(4)  VALUE 0.
       01  WS-QUAD4-SAFETY                        PIC 9(4)  VALUE 0.  

       01  WS-MAP-ARR.                            
           05 WS-MAP OCCURS 103 TIMES             PIC X(101).
       01  WS-MAP-POINTER                         PIC 9(3).

       01  WS-SECONDS                             PIC 9(5)  VALUE 1.

       01  WS-END                                 PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           MOVE SPACES TO WS-TREE-FOUND
                          WS-MAP-ARR                        
           PERFORM 1000-OPEN-FILE             THRU 1000-EXIT
           MOVE 0 TO WS-QUAD1-SAFETY
                     WS-QUAD2-SAFETY
                     WS-QUAD3-SAFETY
                     WS-QUAD4-SAFETY 
           PERFORM 1500-DETERMINE-MIDPOINTS   THRU 1500-EXIT
           PERFORM 2000-PROCESS-DATA          THRU 2000-EXIT
               UNTIL END-OF-FILE   
           PERFORM 8000-DISPLAY-RESULTS       THRU 8000-EXIT
           PERFORM 9000-CLOSE-FILE            THRU 9000-EXIT
           IF TREE-NOT-FOUND
               ADD 1 TO WS-SECONDS
               MOVE SPACES TO WS-END-OF-FILE
               GO TO 0000-MAINLINE
           ELSE
               PERFORM UNTIL WS-MAP-POINTER GREATER C-MAP-HEIGHT + 1
                   DISPLAY WS-MAP(WS-MAP-POINTER)
                   ADD 1 TO WS-MAP-POINTER
               END-PERFORM
               DISPLAY 'TREE FOUND AFTER ' WS-SECONDS ' SECONDS'
               GO TO 0000-EXIT
           END-IF

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
      * DETERMINE MIDPOINTS OF THE GRID                              *
      ****************************************************************
       1500-DETERMINE-MIDPOINTS.

           DIVIDE C-MAP-HEIGHT BY 2 GIVING WS-MID-HEIGHT
           DIVIDE C-MAP-WIDTH  BY 2 GIVING WS-MID-WIDTH
           .
       1500-EXIT.
           EXIT.

      ****************************************************************
      * READ FILE LINE BY LINE                                       *
      ****************************************************************
       2000-PROCESS-DATA.
       
           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END          
                   PERFORM 2010-PARSE-DATA   THRU 2010-EXIT
                   PERFORM 3000-FORWARD-TIME THRU 3000-EXIT 
                       WS-SECONDS TIMES
                   ADD 1 TO WS-ROB-POS-X
                            WS-ROB-POS-Y
      *            TREE DOES NOT OCCUR ON DUPLICATED NODES                      
                   IF WS-MAP(WS-ROB-POS-X)(WS-ROB-POS-Y:1) EQUALS '1'              
                       SET TREE-NOT-FOUND TO TRUE
                   END-IF
                   MOVE '1' TO WS-MAP(WS-ROB-POS-X)(WS-ROB-POS-Y:1)
                   SUBTRACT 1 FROM WS-ROB-POS-X
                                   WS-ROB-POS-Y                                  
                   PERFORM 4000-QUADRANT     THRU 4000-EXIT
           END-READ
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * PARSE ROBOT INFORMATION                                       *
      *****************************************************************
       2010-PARSE-DATA.

      *    GET STARTING POSITION
           MOVE 3 TO WS-INREC-POINTER
           MOVE 1 TO WS-DIGIT-CNT

           PERFORM UNTIL INPUT-RECORD(WS-INREC-POINTER:WS-DIGIT-CNT) 
                         NOT NUMERIC
               MOVE INPUT-RECORD(WS-INREC-POINTER:WS-DIGIT-CNT)
                    TO WS-ROB-POS-X
               ADD 1 TO WS-DIGIT-CNT
           END-PERFORM      

           ADD WS-DIGIT-CNT TO WS-INREC-POINTER
           MOVE 1 TO WS-DIGIT-CNT    
           PERFORM UNTIL INPUT-RECORD(WS-INREC-POINTER:WS-DIGIT-CNT)
                         NOT NUMERIC
               MOVE INPUT-RECORD(WS-INREC-POINTER:WS-DIGIT-CNT)
                    TO WS-ROB-POS-Y
               ADD 1 TO WS-DIGIT-CNT
           END-PERFORM   

      *    GET VELOCITY

           ADD 2 WS-DIGIT-CNT TO WS-INREC-POINTER         
           MOVE SPACES        TO WS-NUM-SIGN       
           IF INPUT-RECORD(WS-INREC-POINTER:1) EQUALS '-'
               SET NEGATIVE-NUMBER TO TRUE
               ADD 1 TO WS-INREC-POINTER   
           END-IF
            
           MOVE 1 TO WS-DIGIT-CNT
           PERFORM UNTIL INPUT-RECORD(WS-INREC-POINTER:WS-DIGIT-CNT)
                         NOT NUMERIC
               MOVE INPUT-RECORD(WS-INREC-POINTER:WS-DIGIT-CNT)
                    TO WS-ROB-VEL-X
               ADD 1 TO WS-DIGIT-CNT
           END-PERFORM 

           IF NEGATIVE-NUMBER
               MULTIPLY -1 BY WS-ROB-VEL-X
               MOVE SPACES TO WS-NUM-SIGN  
           END-IF

           ADD WS-DIGIT-CNT TO WS-INREC-POINTER
           IF INPUT-RECORD(WS-INREC-POINTER:1) EQUALS '-'
               SET NEGATIVE-NUMBER TO TRUE
               ADD 1 TO WS-INREC-POINTER    
           END-IF           
                      
           MOVE 1 TO WS-DIGIT-CNT
           PERFORM UNTIL INPUT-RECORD(WS-INREC-POINTER:WS-DIGIT-CNT)
                         NOT NUMERIC
               MOVE INPUT-RECORD(WS-INREC-POINTER:WS-DIGIT-CNT)
                    TO WS-ROB-VEL-Y
               ADD 1 TO WS-DIGIT-CNT
           END-PERFORM 

           IF NEGATIVE-NUMBER
               MULTIPLY -1 BY WS-ROB-VEL-Y
               MOVE SPACES TO WS-NUM-SIGN
           END-IF           
           .
       2010-EXIT.
           EXIT.

      *****************************************************************
      * FORWARD 1 SECOND                                              *
      *****************************************************************
       3000-FORWARD-TIME.
           
           ADD WS-ROB-VEL-X TO WS-ROB-POS-X 
           ADD WS-ROB-VEL-Y TO WS-ROB-POS-Y          

           IF WS-ROB-POS-X LESS 0
               ADD 1 C-MAP-WIDTH TO WS-ROB-POS-X
           END-IF

           IF WS-ROB-POS-X GREATER C-MAP-WIDTH
               SUBTRACT 1 C-MAP-WIDTH FROM WS-ROB-POS-X
           END-IF

           IF WS-ROB-POS-Y LESS 0
               ADD 1 C-MAP-HEIGHT TO WS-ROB-POS-Y
           END-IF

           IF WS-ROB-POS-Y GREATER C-MAP-HEIGHT
               SUBTRACT 1 C-MAP-HEIGHT FROM WS-ROB-POS-Y
           END-IF           
           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * IF THE NODE IS IN A QUADRANT, INCREMENT THAT RELEVANT COUNTER *
      *****************************************************************
       4000-QUADRANT.

       
           EVALUATE TRUE
               WHEN WS-ROB-POS-X EQUALS  WS-MID-WIDTH     AND
                    WS-ROB-POS-Y EQUALS  WS-MID-HEIGHT 
                   CONTINUE
               WHEN WS-ROB-POS-X LESS    WS-MID-WIDTH     AND
                    WS-ROB-POS-Y LESS    WS-MID-HEIGHT 
                   ADD 1 TO WS-QUAD1-SAFETY
               WHEN WS-ROB-POS-X LESS    WS-MID-WIDTH     AND
                    WS-ROB-POS-Y GREATER WS-MID-HEIGHT 
                   ADD 1 TO WS-QUAD2-SAFETY   
               WHEN WS-ROB-POS-X GREATER WS-MID-WIDTH     AND
                    WS-ROB-POS-Y GREATER WS-MID-HEIGHT 
                   ADD 1 TO WS-QUAD3-SAFETY     
               WHEN WS-ROB-POS-X GREATER WS-MID-WIDTH     AND
                    WS-ROB-POS-Y LESS    WS-MID-HEIGHT 
                   ADD 1 TO WS-QUAD4-SAFETY    
           END-EVALUATE                                                         
           .
       4000-EXIT.
           EXIT.

      *****************************************************************
      * DISPLAY SAFETY FACTOR                                         *
      *****************************************************************
       8000-DISPLAY-RESULTS.
           
           MOVE 0 TO WS-SAFETY-FACTOR
           COMPUTE WS-SAFETY-FACTOR = WS-QUAD1-SAFETY * WS-QUAD2-SAFETY
                                    * WS-QUAD3-SAFETY * WS-QUAD4-SAFETY

           IF WS-SECONDS EQUALS 100
               DISPLAY 'SAFETY FACTOR = ' WS-SAFETY-FACTOR
           END-IF

           MOVE 1 TO WS-MAP-POINTER
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