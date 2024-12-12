       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLUTONIAN-PEBBLES.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 11 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 11 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/11                  *
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
           RECORD CONTAINS 100 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                           PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.

       01  WS-VALUES-ARR.
           05 WS-VALUES 
           OCCURS 5 TO 274229228071551 TIMES DEPENDING ON WS-ARR-LENGTH 
                                                  PIC 9(15).

       01  WS-EVEN-DIGITS                         PIC X(1).
           88 EVEN-DIGITS                                  VALUE 'Y'.
           88 NOT-EVEN-DIGITS                              VALUE 'N'.

       01  WS-LEADING-ZEROS                       PIC X(1).
           88 LEADING-ZEROS                                VALUE 'Y'.
           88 NOT-LEADING-ZEROS                            VALUE 'N'.

       01  WS-ARR-SUB                             PIC 9(15).    
       01  WS-ARR-SUB2                            PIC 9(15).   
       01  WS-SUB-CHAR                            PIC 9(15).   
       01  WS-START-DIGIT                         PIC 9(15).
       01  WS-DIVISOR                             PIC 9(15).
       01  WS-SPLIT-DIGIT                         PIC 9(15).
       01  WS-SPLIT-LEFT                          PIC 9(15).
       01  WS-SPLIT-RIGHT                         PIC 9(15). 
       01  WS-ARR-LENGTH                          PIC 9(9) VALUE 8. 
       01  WS-ARR-ORIG-LENGTH                     PIC 9(9) VALUE 8.

       01  WS-END                                 PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE             THRU 1000-EXIT
           PERFORM 2000-CONVERT-FILE-TO-ARRAY THRU 2000-EXIT
           PERFORM 3000-BLINK                 THRU 3000-EXIT
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
           READ INPUT-FILE
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * UNSTRING INPNUT RECORD INTO AN ARRAY                         *
      ****************************************************************
       2000-CONVERT-FILE-TO-ARRAY.
       
           UNSTRING INPUT-RECORD DELIMITED BY ' ' INTO WS-VALUES(1)
                                                       WS-VALUES(2)
                                                       WS-VALUES(3)
                                                       WS-VALUES(4)
                                                       WS-VALUES(5)   
                                                       WS-VALUES(6)
                                                       WS-VALUES(7)
                                                       WS-VALUES(8)        
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * APPLY BLINK PATTERN                                           *
      *****************************************************************
       3000-BLINK.

           PERFORM 75 TIMES
               MOVE 1 TO WS-ARR-SUB    
               MOVE WS-ARR-LENGTH TO WS-ARR-ORIG-LENGTH        
               PERFORM UNTIL WS-ARR-SUB GREATER WS-ARR-ORIG-LENGTH
                   PERFORM 3100-COUNT-AMT-OF-DIGITS THRU 3100-EXIT
                   EVALUATE TRUE
      *                RULE 1: 0 BECOMES 1
                       WHEN WS-VALUES(WS-ARR-SUB) EQUALS 0                 
                           MOVE 1 TO WS-VALUES(WS-ARR-SUB)             
      *                RULE 2: IF THE VALUE HAS AN EVEN AMONT OF DIGITS,
      *                        BREAK IT UP INTO TWO STONES
                       WHEN EVEN-DIGITS                          
                           PERFORM 3200-SPLIT-STONES THRU 3200-EXIT                       
      *                RULE 3: ELSE, MULTIPLY VALUE BY 2024
                       WHEN OTHER                 
                           MULTIPLY 2024 BY WS-VALUES(WS-ARR-SUB)                          
                   END-EVALUATE               
                   ADD 1 TO WS-ARR-SUB
               END-PERFORM
           END-PERFORM
           
           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * DETERMINE IF THE CURRENT VALUE HAS AN EVEN AMOUNT OF DIGITS   *
      * (IGNORING LEADING ZEROS)                                      *
      *****************************************************************
       3100-COUNT-AMT-OF-DIGITS.

           MOVE SPACES TO WS-EVEN-DIGITS
           SET LEADING-ZEROS TO TRUE
           MOVE 1 TO WS-SUB-CHAR 

           PERFORM UNTIL NOT-LEADING-ZEROS OR (WS-SUB-CHAR GREATER 30)
               IF WS-VALUES(WS-ARR-SUB)(WS-SUB-CHAR:1) EQUALS 0
                   CONTINUE
               ELSE          
                   SET NOT-LEADING-ZEROS TO TRUE
                   MOVE WS-SUB-CHAR TO WS-START-DIGIT                  
               END-IF
               ADD 1 TO WS-SUB-CHAR
           END-PERFORM

           MOVE FUNCTION MOD(WS-SUB-CHAR,2) TO WS-SUB-CHAR
           IF WS-SUB-CHAR EQUALS 0    
               SET EVEN-DIGITS TO TRUE   
           END-IF
           .
       3100-EXIT.
           EXIT.

      *****************************************************************
      * SPLIT STONE INTO TWO                                          *
      *****************************************************************
       3200-SPLIT-STONES.
                      
           COMPUTE WS-DIVISOR = (14 - (WS-START-DIGIT - 1))
           COMPUTE WS-SPLIT-DIGIT = WS-DIVISOR / 2
           MOVE WS-VALUES(WS-ARR-SUB)(WS-START-DIGIT:WS-SPLIT-DIGIT)
                                                        TO WS-SPLIT-LEFT
           MOVE WS-VALUES(WS-ARR-SUB)(WS-START-DIGIT + 
                                          WS-SPLIT-DIGIT:WS-SPLIT-DIGIT)
                                                       TO WS-SPLIT-RIGHT 
           ADD 1 TO WS-ARR-LENGTH
           MOVE WS-SPLIT-LEFT TO WS-VALUES(WS-ARR-SUB)
           MOVE WS-SPLIT-RIGHT TO WS-VALUES(WS-ARR-LENGTH)
           .
       3200-EXIT.
           EXIT.
           
      *****************************************************************
      * DISPLAY AMOUNT OF STONES                                      *
      *****************************************************************
       8000-DISPLAY-RESULTS.
           DISPLAY 'TOTAL AMOUNT OF STONES = ' WS-ARR-LENGTH
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