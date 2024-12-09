       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESONANT-COLLINEARITY.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 8 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 8 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/8                   *
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

       01  WS-MAP-ARR.
           05 WS-MAP OCCURS 12 TO 50 TIMES DEPENDING ON ARR-LENGTH
                                                  PIC X(140).
       01  WS-MAP-ARR-SUBSCRIPTS.
           05  ARR-LENGTH                          PIC 9(2)  VALUE 0.
      *    MAIN POINTERS       
           05  MAP-ARR-SUB                         PIC 9(2)  VALUE 1.
           05  MAP-SUB-CHAR                        PIC 9(2).
      *    SECONDARY POINTERS
           05  MAP-ARR-SUB2                       PIC 9(2).
           05  MAP-SUB-CHAR2                      PIC 9(2).

       01  WS-FREQ                                PIC X(1).
       01  WS-PAIR-DIFF-X                         PIC S9(2).
       01  WS-PAIR-DIFF-Y                         PIC S9(2).

       01  WS-POSS-AN-SUB                         PIC S9(2).
       01  WS-POSS-AN-CHAR                        PIC S9(2).
       01  WS-POS1-SUB                            PIC  9(2).
       01  WS-POS1-CHAR                           PIC  9(2).
       01  WS-POS2-SUB                            PIC  9(2).
       01  WS-POS2-CHAR                           PIC  9(2).       
       01  WS-ANTINODES-ARR. 
           05 WS-ANTINODES OCCURS 12 TO 50 TIMES DEPENDING ON ARR-LENGTH
                                                  PIC X(140).     
       01  WS-ANTENNAS-ARR.
           05 WS-ANTENNAS  OCCURS 500             PIC X(1).
       01  ANT-SUB                                PIC 9(3).
       01  WS-ANT-CNT                             PIC 9(3).
            
       01  WS-ANTINODES-CNT                       PIC 9(4)  VALUE 0.  

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
           PERFORM 3000-IDENTIFY-PAIRS        THRU 3000-EXIT
           PERFORM 7000-ADD-VALID-ANTENNAS    THRU 7000-EXIT
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
      * IDENTIFY PAIRS                                                *
      *****************************************************************
       3000-IDENTIFY-PAIRS.

           MOVE SPACES TO WS-ANTINODES-ARR 
                          WS-ANTENNAS-ARR      
           MOVE 1      TO MAP-ARR-SUB

           PERFORM UNTIL MAP-ARR-SUB EQUALS (ARR-LENGTH + 1)
               MOVE 1 TO MAP-SUB-CHAR
               PERFORM UNTIL MAP-SUB-CHAR EQUALS (ARR-LENGTH + 1)             
                   MOVE WS-MAP(MAP-ARR-SUB)(MAP-SUB-CHAR:1) TO WS-FREQ 
                   IF WS-FREQ NOT EQUALS '.'
                       PERFORM 3010-SCAN-FOR-MATCH THRU 3010-EXIT
                   END-IF
                   ADD 1 TO MAP-SUB-CHAR
               END-PERFORM                
               ADD 1 TO MAP-ARR-SUB                
           END-PERFORM
           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * SCAN GRID FOR MATCH                                           *
      *****************************************************************
       3010-SCAN-FOR-MATCH.

           MOVE 1 TO MAP-ARR-SUB2

           PERFORM UNTIL MAP-ARR-SUB2 EQUALS (ARR-LENGTH + 1)
              MOVE 1 TO MAP-SUB-CHAR2
              PERFORM UNTIL MAP-SUB-CHAR2 EQUALS (ARR-LENGTH + 1)           
                 IF WS-FREQ EQUALS WS-MAP(MAP-ARR-SUB2)(MAP-SUB-CHAR2:1)                       
                    PERFORM 3020-CONFIRM-ANTINODE THRU 3020-EXIT
                 END-IF
                 ADD 1 TO MAP-SUB-CHAR2                            
              END-PERFORM           
              ADD 1 TO MAP-ARR-SUB2         
           END-PERFORM           

           .
       3010-EXIT.
           EXIT.

      *****************************************************************
      * ONCE MATCH IS FOUND, CALCULATE WHERE THE ANTINODE SHOULD BE.  *
      * AS LONG AS IT IS WITHIN THE BOUNDS OF THE MAP, INCREMENT THE  *
      * OUTPUT COUNTER.                                               *
      *****************************************************************
       3020-CONFIRM-ANTINODE.

      *    IF MATCH FOUND DUE TO BEING THE SAME NODE, SKIP.
           IF MAP-ARR-SUB  EQUALS MAP-ARR-SUB2   AND
              MAP-SUB-CHAR EQUALS MAP-SUB-CHAR2
               GO TO 3020-EXIT
           END-IF
         
           MOVE MAP-ARR-SUB     TO WS-POS1-SUB
           MOVE MAP-SUB-CHAR    TO WS-POS1-CHAR
           MOVE MAP-ARR-SUB2    TO WS-POS2-SUB
           MOVE MAP-SUB-CHAR2   TO WS-POS2-CHAR
           .

           CHECK-FOR-ANTINODE.
           
           SUBTRACT WS-POS2-SUB  FROM WS-POS1-SUB GIVING WS-PAIR-DIFF-X
           SUBTRACT WS-POS2-CHAR FROM WS-POS1-CHAR 
                                                   GIVING WS-PAIR-DIFF-Y                                                 

           ADD WS-POS1-SUB  WS-PAIR-DIFF-X GIVING WS-POSS-AN-SUB
           ADD WS-POS1-CHAR WS-PAIR-DIFF-Y GIVING WS-POSS-AN-CHAR                                      

           IF (WS-POSS-AN-SUB) GREATER 0              AND
              (WS-POSS-AN-SUB) LESS (ARR-LENGTH + 1)  AND
              (WS-POSS-AN-CHAR) GREATER 0             AND
              (WS-POSS-AN-CHAR) LESS (ARR-LENGTH + 1) 
      *        CHECK IF LOCATION ALREADY HAS ANTINODE BEFORE INCREMENT    
               IF WS-ANTINODES(WS-POSS-AN-SUB)(WS-POSS-AN-CHAR:1) 
                  EQUALS 'Y'
                   CONTINUE
               ELSE 
      *            CHECK IF ANTENNA ALREADY EXISTS ON SELECTED NODE
      *            (WAS FINE HERE IN PART 1, BUT THAT NOW IS HANDLED BY
      *             PARA 7000, SO ADDING CHECK HERE TO NOT COUNT TWICE)
                   IF WS-MAP(WS-POSS-AN-SUB)(WS-POSS-AN-CHAR:1)
                      EQUALS '.' 
                       ADD 1 TO WS-ANTINODES-CNT                     
                       MOVE 'Y' TO 
                         WS-ANTINODES(WS-POSS-AN-SUB)(WS-POSS-AN-CHAR:1) 
                       PERFORM 3025-ADD-ANTENNA THRU 3025-EXIT
                   END-IF
               END-IF
                   MOVE WS-POS1-SUB     TO WS-POS2-SUB
                   MOVE WS-POS1-CHAR    TO WS-POS2-CHAR
                   MOVE WS-POSS-AN-SUB  TO WS-POS1-SUB
                   MOVE WS-POSS-AN-CHAR TO WS-POS1-CHAR
                   GO TO CHECK-FOR-ANTINODE       
           END-IF
           
           .
       3020-EXIT.
           EXIT. 

      *****************************************************************
      * IF IT ISN'T ALREADY, ADD ANTENNA TO UNIQUE ARRAY AND ADD TO   *
      * OUTPUT COUNTER                                                *
      *****************************************************************
       3025-ADD-ANTENNA.

           MOVE 1 TO ANT-SUB
           PERFORM UNTIL WS-ANTENNAS(ANT-SUB) EQUALS SPACE
               IF WS-ANTENNAS(ANT-SUB) EQUALS WS-FREQ
                   GO TO 3025-EXIT
               END-IF
               ADD 1 TO ANT-SUB
           END-PERFORM
      
      *    REACHING THIS INSTRUCTION MEANS THE FREQUENCY THAT CREATED
      *    AN ANTINODE IS "NEW" AND NEEDS TO BE ACCOUNTED FOR
           MOVE WS-FREQ TO WS-ANTENNAS(ANT-SUB)
           .
       3025-EXIT.
           EXIT.
           
      *****************************************************************
      * ADD 1 UNIT TO OUTPUT COUNTER FOR EVERY ANTENNA THAT PRODUCED  *
      * AN ANTINODE.                                                  *
      *****************************************************************
       7000-ADD-VALID-ANTENNAS.

           MOVE 1 TO ANT-SUB      
                     
           PERFORM UNTIL WS-ANTENNAS(ANT-SUB) EQUALS SPACE
               MOVE 0 TO WS-ANT-CNT
               INSPECT WS-MAP-ARR
               TALLYING WS-ANT-CNT FOR ALL WS-ANTENNAS(ANT-SUB)
               ADD WS-ANT-CNT TO WS-ANTINODES-CNT
               ADD 1 TO ANT-SUB
           END-PERFORM
           .
       7000-EXIT.
           EXIT.
      *****************************************************************
      * DISPLAY RESULTING SUM OF PRODUCTS                             *
      *****************************************************************
       8000-DISPLAY-RESULTS.
            
           DISPLAY 'TOTAL ANTINODES = ' WS-ANTINODES-CNT
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