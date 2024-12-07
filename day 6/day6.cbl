       IDENTIFICATION DIVISION.
       PROGRAM-ID. GUARD-GALLIVANT.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 6 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 6 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/6                   *
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
           RECORD CONTAINS 130 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                          PIC X(130).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                              PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.

       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.
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
       01  WS-MAP-ARR-SUB-ORIG                    PIC 9(3).
       01  WS-MAP-SUB-CHAR                        PIC 9(3).
       01  WS-MAP-SUB-CHAR-ORIG                   PIC 9(3).

       01  WS-VISITED-ARR.
           05 WS-VISITED OCCURS 0 TO 130 TIMES 
           DEPENDING ON WS-ARR-LENGTH             PIC X(140).
       01  WS-VISITED-ARR-BKUP.
           05 WS-VISITED-BKUP OCCURS 0 TO 130 TIMES 
           DEPENDING ON WS-ARR-LENGTH             PIC X(140).
       01  WS-VIS-ARR-SUB                         PIC 9(3).
       01  WS-VIS-SUB-CHAR                        PIC 9(3).
       01  WS-STEPS-WALKED                        PIC 9(6).
       01  WS-OBSTACLES-PLACED                    PIC 9(6).

       01  WS-START-ROW                           PIC 9(3).
       01  WS-START-COL                           PIC 9(3).
       01  WS-START-DIR                           PIC X(1).
       01  WS-STEP-CNT                            PIC 9(5).

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
           PERFORM 3000-FIND-STARTING-POS     THRU 3000-EXIT
           PERFORM 4000-WALK-GUARD            THRU 4000-EXIT
           PERFORM 5000-PLACE-OBSTACLES       THRU 5000-EXIT
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
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   MOVE INPUT-RECORD TO WS-MAP(WS-MAP-ARR-SUB)
                   ADD 1 TO WS-MAP-ARR-SUB
                            WS-ARR-LENGTH  
                            
           END-READ
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * FIND GUARD'S STARTING POSITION                                *
      *****************************************************************
       3000-FIND-STARTING-POS.

           MOVE SPACES TO WS-START-POS-FOUND
           MOVE ZEROS  TO WS-VISITED-ARR
           MOVE 1 TO WS-MAP-ARR-SUB
                     WS-MAP-SUB-CHAR
           PERFORM UNTIL START-POS-FOUND
               IF WS-MAP(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1) EQUALS '.'
               OR WS-MAP(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1) EQUALS '#'
                   CONTINUE
               ELSE
                   SET START-POS-FOUND TO TRUE
                   EVALUATE WS-MAP(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1)
                       WHEN '^'
                           SET DIR-UP    TO TRUE
                       WHEN 'v'
                           SET DIR-DOWN  TO TRUE
                       WHEN '<'
                           SET DIR-LEFT  TO TRUE
                       WHEN '>'
                           SET DIR-RIGHT TO TRUE
                   END-EVALUATE        
                   PERFORM 4100-MARK-VISITED THRU 4100-EXIT  
                   MOVE WS-MAP-ARR-SUB  TO WS-START-ROW
                   MOVE WS-MAP-SUB-CHAR TO WS-START-COL
                   MOVE WS-DIRECTION    TO WS-START-DIR
                   GO TO 3000-EXIT   
               END-IF
               IF WS-MAP-SUB-CHAR EQUALS WS-ARR-LENGTH
                   ADD  1 TO WS-MAP-ARR-SUB
                   MOVE 1 TO WS-MAP-SUB-CHAR
               ELSE
                   ADD 1 TO WS-MAP-SUB-CHAR
           END-PERFORM
           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * HAVE GUARD WALK IN DIRECTION SHE IS FACING IF THERE IS SPACE  *
      *****************************************************************
       4000-WALK-GUARD.

           MOVE WS-MAP-ARR-SUB  TO WS-MAP-ARR-SUB-ORIG
           MOVE WS-MAP-SUB-CHAR TO WS-MAP-SUB-CHAR-ORIG    
      *    CHECK IF THERE IS ROOM FOR THE GUARD TO WALK
      *    IF NOT, GO TO EXIT SO FINISH PARA CAN RUN
      *    FYI: THESE GO TOS ARE THE ONLY VALID WAY TO LEAVE THIS PARA
      *         TO RETURN TO MAINLINE PROCESSING
           EVALUATE TRUE
               WHEN DIR-UP
                   IF WS-MAP-ARR-SUB EQUALS 1
                       GO TO 4000-EXIT
                   ELSE
                       SUBTRACT 1 FROM WS-MAP-ARR-SUB
                   END-IF
               WHEN DIR-DOWN
                   IF WS-MAP-ARR-SUB EQUALS WS-ARR-LENGTH
                       GO TO 4000-EXIT
                   ELSE
                       ADD 1 TO WS-MAP-ARR-SUB
                   END-IF
               WHEN DIR-LEFT
                   IF WS-MAP-SUB-CHAR EQUALS 1
                       GO TO 4000-EXIT
                   ELSE
                       SUBTRACT 1 FROM WS-MAP-SUB-CHAR
                   END-IF
               WHEN DIR-RIGHT
                   IF WS-MAP-SUB-CHAR EQUALS WS-ARR-LENGTH
                       GO TO 4000-EXIT
                   ELSE                 
                       ADD 1 TO WS-MAP-SUB-CHAR
                   END-IF
           END-EVALUATE
      *    IF THE GUARD IS NOW STANDING ON AN OBSTACLE, RETURN THEM
      *    TO THEIR ORIGINAL POSITION AND ROTATE THEM CLOCKWISE
           IF WS-MAP(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1) EQUALS '#'
               MOVE WS-MAP-ARR-SUB-ORIG  TO WS-MAP-ARR-SUB  
               MOVE WS-MAP-SUB-CHAR-ORIG TO WS-MAP-SUB-CHAR 
               EVALUATE TRUE
                   WHEN DIR-UP
                       SET DIR-RIGHT TO TRUE
                   WHEN DIR-DOWN
                       SET DIR-LEFT  TO TRUE
                   WHEN DIR-LEFT
                       SET DIR-UP    TO TRUE
                   WHEN DIR-RIGHT
                       SET DIR-DOWN  TO TRUE
               END-EVALUATE
           END-IF

      *    MARK SPOT AS VISITED, IF IT HASN'T ALREADY BEEN
           PERFORM 4100-MARK-VISITED THRU 4100-EXIT       

      *    REACHING THIS INSTRUCTION MEANS THERE IS MORE MOVEMENT TO DO
           GO TO 4000-WALK-GUARD
           .
       4000-EXIT.
           EXIT.


      *****************************************************************
      * MARK LOCATION AS VISITED (IF IT HASN'T ALREADY)               *
      *****************************************************************
       4100-MARK-VISITED.

      *    0 = NOT VISITED
      *    1 = VISITED
           IF WS-VISITED(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1) EQUALS '1'
               CONTINUE
           ELSE
               MOVE 1 TO WS-VISITED(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1)
               ADD  1 TO WS-STEPS-WALKED
           END-IF
           .
       4100-EXIT.
           EXIT.
      *****************************************************************
      * IDENTIFY OBSTACLES TO CAUSE AN INFINITE LOOP                  *
      *****************************************************************
       5000-PLACE-OBSTACLES.
           
           MOVE WS-VISITED-ARR TO WS-VISITED-ARR-BKUP
           MOVE 1 TO WS-VIS-ARR-SUB

           PERFORM UNTIL WS-VIS-ARR-SUB  EQUALS (WS-ARR-LENGTH + 1)
               MOVE 1 TO WS-VIS-SUB-CHAR
               PERFORM UNTIL WS-VIS-SUB-CHAR EQUALS (WS-ARR-LENGTH + 1)              
                  IF WS-VISITED(WS-VIS-ARR-SUB)(WS-VIS-SUB-CHAR:1)
                     NOT EQUAL 0
                         MOVE '#' 
                            TO WS-MAP(WS-VIS-ARR-SUB)(WS-VIS-SUB-CHAR:1)
  
                         MOVE WS-START-ROW TO WS-MAP-ARR-SUB
                         MOVE WS-START-COL TO WS-MAP-SUB-CHAR
                         MOVE WS-START-DIR TO WS-DIRECTION
TEST       DISPLAY 'ATTEMPTING AT ' WS-VIS-ARR-SUB ' ' WS-VIS-SUB-CHAR  
                         MOVE 0 TO WS-STEP-CNT                    
                         PERFORM 5100-WALK-WITH-OBST    THRU 5100-EXIT
                         MOVE WS-VISITED-ARR-BKUP TO WS-VISITED-ARR
                         MOVE '.' 
                            TO WS-MAP(WS-VIS-ARR-SUB)(WS-VIS-SUB-CHAR:1)
                  END-IF
                  ADD 1 TO WS-VIS-SUB-CHAR              
               END-PERFORM               
               ADD 1 TO WS-VIS-ARR-SUB
           END-PERFORM
           .
       5000-EXIT.
           EXIT.

      *****************************************************************
      * DISPLAY RESULTING SUM OF PRODUCTS                             *
      *****************************************************************
       5100-WALK-WITH-OBST.

           MOVE WS-MAP-ARR-SUB  TO WS-MAP-ARR-SUB-ORIG
           MOVE WS-MAP-SUB-CHAR TO WS-MAP-SUB-CHAR-ORIG    
      *    CHECK IF VISIT TABLE HAS DIRECTION MATCHING THE ONE 
      *    BEING TRAVELED. 
      *    IF SO, LEAVE THIS PARA AND LABEL THE BLOCKER
      *    IF NOT, UPDATE WITH DIRECTION
TEST  *      DISPLAY WS-MAP-ARR-SUB ' ' WS-MAP-SUB-CHAR
TEST  *     DISPLAY 'VIS =' WS-VISITED(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1)
TEST  *     DISPLAY 'DIR =' WS-DIRECTION
           IF WS-VISITED(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1) EQUALS 
               WS-DIRECTION OR WS-STEP-CNT EQUALS 99999
TEST           DISPLAY 'LOOP FOUND!'
               ADD 1 TO WS-OBSTACLES-PLACED
               GO TO 5100-EXIT
           ELSE
               MOVE WS-DIRECTION TO 
                    WS-VISITED(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1)
           END-IF

      *    CHECK IF THERE IS ROOM FOR THE GUARD TO WALK
      *    IF NOT, GO TO EXIT SO FINISH PARA CAN RUN
      *    FYI: THESE GO TOS ARE THE ONLY VALID WAY TO LEAVE THIS PARA
      *         TO RETURN TO 5000 PARA UNLESS THE GUARD IS IN A LOOP
           EVALUATE TRUE
               WHEN DIR-UP
                   IF WS-MAP-ARR-SUB EQUALS 1
                       GO TO 5100-EXIT
                   ELSE
                       SUBTRACT 1 FROM WS-MAP-ARR-SUB
                   END-IF
               WHEN DIR-DOWN
                   IF WS-MAP-ARR-SUB EQUALS WS-ARR-LENGTH
                       GO TO 5100-EXIT
                   ELSE
                       ADD 1 TO WS-MAP-ARR-SUB
                   END-IF
               WHEN DIR-LEFT
                   IF WS-MAP-SUB-CHAR EQUALS 1
                       GO TO 5100-EXIT
                   ELSE
                       SUBTRACT 1 FROM WS-MAP-SUB-CHAR
                   END-IF
               WHEN DIR-RIGHT
                   IF WS-MAP-SUB-CHAR EQUALS WS-ARR-LENGTH
                       GO TO 5100-EXIT
                   ELSE                 
                       ADD 1 TO WS-MAP-SUB-CHAR
                   END-IF
           END-EVALUATE
      *    IF THE GUARD IS NOW STANDING ON AN OBSTACLE, RETURN THEM
      *    TO THEIR ORIGINAL POSITION AND ROTATE THEM CLOCKWISE
           IF WS-MAP(WS-MAP-ARR-SUB)(WS-MAP-SUB-CHAR:1) EQUALS '#'
               MOVE WS-MAP-ARR-SUB-ORIG  TO WS-MAP-ARR-SUB  
               MOVE WS-MAP-SUB-CHAR-ORIG TO WS-MAP-SUB-CHAR 
               EVALUATE TRUE
                   WHEN DIR-UP
                       SET DIR-RIGHT TO TRUE
                   WHEN DIR-DOWN
                       SET DIR-LEFT  TO TRUE
                   WHEN DIR-LEFT
                       SET DIR-UP    TO TRUE
                   WHEN DIR-RIGHT
                       SET DIR-DOWN  TO TRUE
               END-EVALUATE
           END-IF  

      *    REACHING THIS INSTRUCTION MEANS THERE IS MORE MOVEMENT TO DO
           ADD 1 TO WS-STEP-CNT
           GO TO 5100-WALK-WITH-OBST
           .
       5100-EXIT.
           EXIT.

      *****************************************************************
      * DISPLAY RESULTING SUM OF PRODUCTS                             *
      *****************************************************************
       8000-DISPLAY-RESULTS.
            
           DISPLAY 'DISTINCT POSITIONS = ' WS-STEPS-WALKED
           DISPLAY 'ADDED OBSTACLES    = ' WS-OBSTACLES-PLACED
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