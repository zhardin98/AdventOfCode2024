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
           RECORD CONTAINS 30 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.                      *>  PIC X(30).
           05 INPUT-REC-TYPE                      PIC X(6).
               88 INREC-BUTTON                           VALUE 'Button'.     
               88 INREC-PRIZE                            VALUE 'Prize:'.   
           05 INPUT-REC-DTL.
               10 INREC-BUTTON-TYPE               PIC X(3).
                   88 INREC-BUTTON-A                     VALUE 'A: '.    
                   88 INREC-BUTTON-B                     VALUE 'B: '.  
               10 INREC-BUTTON-VALUES.
                   15 INREC-X-IDENTIFIER         PIC  X(3).
                   15 INREC-X-VALUE              PIC  9(2).
                   15 INREC-BTN-DELIM            PIC  X(2).
                   15 INREC-Y-IDENTIFIER         PIC  X(2).
                   15 INREC-Y-VALUE              PIC  9(2).   
      *            SPARE BYTES USED FOR PRIZE LOCATION, PROCESSED IN
      *            WORKING STORAGE
                   15 INREC-SPARE                PIC  X(11).                    

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.

       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.

       01  WS-VALID-MACHINE                       PIC X(1).
           88 IS-VALID-MACHINE                              VALUE 'Y'.   
           88 IS-NOT-VALID-MACHINE                          VALUE 'N'.                  

       01  WS-A-PRESS.
           05 C-A-COST                            PIC 9(1)  VALUE 3.
           05 WS-A-DELTA-X                        PIC 9(2).
           05 WS-A-DELTA-Y                        PIC 9(2).
           05 WS-A-PUSH-AMT                       PIC 9(30).

       01  WS-B-PRESS.
           05 C-B-COST                            PIC 9(1)  VALUE 1.
           05 WS-B-DELTA-X                        PIC 9(2).
           05 WS-B-DELTA-Y                        PIC 9(2).  
           05 WS-B-PUSH-AMT                       PIC 9(30).           

       01  WS-PRIZE-LOCATION                      PIC X(25).
       01  WS-PRIZE-REC-POINTER                   PIC 9(2).
       01  WS-DIGIT-CNT                           PIC 9(2).
           
       01  WS-PRIZE-X                             PIC 9(30).
       01  WS-PRIZE-Y                             PIC 9(30).

       01  WS-TOTAL-TOKENS                        PIC 9(30) VALUE 0.

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
       2000-PROCESS-DATA.
       
           READ INPUT-FILE
               AT END 
      *            REACHING THIS INSTRUCTION SHOULD BE AN ERROR
      *            EOF IS HANDLES IN PARA 2300
                   DISPLAY 'FILE INCOMPLETE'
                   PERFORM 9999-ABEND             THRU 9999-EXIT
               NOT AT END
                   MOVE SPACES TO WS-VALID-MACHINE 
                   PERFORM 2100-SAVE-A-VALUES     THRU 2100-EXIT
                   PERFORM 2200-SAVE-B-VALUES     THRU 2200-EXIT
                   PERFORM 2300-SAVE-PRIZE-LOC    THRU 2300-EXIT      
                   PERFORM 3000-CALC-INTERSEC     THRU 3000-EXIT  
                   PERFORM 4000-VALIDATE-EQUATION THRU 4000-EXIT
                   PERFORM 4500-CALC-TOKENS       THRU 4500-EXIT
           END-READ
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * SAVE VALUES OF BUTTON A                                       *
      *****************************************************************
       2100-SAVE-A-VALUES.

     
           MOVE INREC-X-VALUE TO WS-A-DELTA-X
           MOVE INREC-Y-VALUE TO WS-A-DELTA-Y
           .
       2100-EXIT.
           EXIT.


      *****************************************************************
      * SAVE VALUES OF BUTTON B                                       *
      *****************************************************************
       2200-SAVE-B-VALUES.

           READ INPUT-FILE
               AT END 
                   DISPLAY 'FILE INCOMPLETE'
                   PERFORM 9999-ABEND THRU 9999-EXIT
               NOT AT END           
                    MOVE INREC-X-VALUE TO WS-B-DELTA-X
                    MOVE INREC-Y-VALUE TO WS-B-DELTA-Y
           END-READ
           .
       2200-EXIT.
           EXIT.  

      *****************************************************************
      * SAVE LOCATION OF THE PRIZE                                    *
      *****************************************************************
       2300-SAVE-PRIZE-LOC.         
           
           READ INPUT-FILE
               AT END 
                   DISPLAY 'FILE INCOMPLETE'
                   PERFORM 9999-ABEND THRU 9999-EXIT
               NOT AT END           
                    MOVE INPUT-REC-DTL TO WS-PRIZE-LOCATION                               
           END-READ

      *    GET X LOCATION
           MOVE 1 TO WS-PRIZE-REC-POINTER
           PERFORM UNTIL WS-PRIZE-LOCATION(WS-PRIZE-REC-POINTER:2)
                         EQUALS 'X='         
               ADD 1 TO WS-PRIZE-REC-POINTER              
           END-PERFORM

           ADD 2 TO WS-PRIZE-REC-POINTER  
           MOVE 1 TO WS-DIGIT-CNT
           PERFORM UNTIL 
                    WS-PRIZE-LOCATION(WS-PRIZE-REC-POINTER:WS-DIGIT-CNT)
                    NOT NUMERIC
               MOVE WS-PRIZE-LOCATION(WS-PRIZE-REC-POINTER:WS-DIGIT-CNT)
                    TO WS-PRIZE-X
               ADD 1 TO WS-DIGIT-CNT
           END-PERFORM

      *    GET Y LOCATION
           ADD 1 WS-DIGIT-CNT TO WS-PRIZE-REC-POINTER
           PERFORM UNTIL WS-PRIZE-LOCATION(WS-PRIZE-REC-POINTER:2)
                         EQUALS 'Y=' 
               ADD 1 TO WS-PRIZE-REC-POINTER              
           END-PERFORM

           ADD 2 TO WS-PRIZE-REC-POINTER  
           MOVE 1 TO WS-DIGIT-CNT
           PERFORM UNTIL 
                    WS-PRIZE-LOCATION(WS-PRIZE-REC-POINTER:WS-DIGIT-CNT)
                    NOT NUMERIC
               MOVE WS-PRIZE-LOCATION(WS-PRIZE-REC-POINTER:WS-DIGIT-CNT)
                    TO WS-PRIZE-Y
               ADD 1 TO WS-DIGIT-CNT
           END-PERFORM           

           ADD 10000000000000 TO WS-PRIZE-X
                                 WS-PRIZE-Y

      *    BYPASS BLANK RECORD
           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   CONTINUE
           END-READ

           .
       2300-EXIT.
           EXIT.

      *****************************************************************
      * CALCULATE AMOUJT OF A AND B BUTTON PUSHES TO GET TRAJECTORIES *
      * TO INTERSECT                                                  *
      *****************************************************************
       3000-CALC-INTERSEC.

           COMPUTE WS-A-PUSH-AMT = 
           ((WS-PRIZE-X*WS-B-DELTA-Y)-(WS-PRIZE-Y*WS-B-DELTA-X)) /
           ((WS-A-DELTA-X*WS-B-DELTA-Y)-(WS-A-DELTA-Y*WS-B-DELTA-X))

           COMPUTE WS-B-PUSH-AMT = 
           ((WS-PRIZE-X*WS-A-DELTA-Y)-(WS-PRIZE-Y*WS-A-DELTA-X)) /
           ((WS-A-DELTA-X*WS-B-DELTA-Y)-(WS-A-DELTA-Y*WS-B-DELTA-X))    
           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * VERIFY AMOUNT OF CALCULATED BUTTON PUSHES PUTS CRANE IN       *
      * DESIRED SPOT                                                  *
      *****************************************************************
       4000-VALIDATE-EQUATION.

           IF  ((WS-A-DELTA-X * WS-A-PUSH-AMT) + 
                (WS-B-DELTA-X * WS-B-PUSH-AMT) 
                 EQUALS WS-PRIZE-X)  
           AND ((WS-A-DELTA-Y * WS-A-PUSH-AMT) + 
                (WS-B-DELTA-Y * WS-B-PUSH-AMT) 
                 EQUALS WS-PRIZE-Y)  
               SET IS-VALID-MACHINE     TO TRUE      
           ELSE      
               SET IS-NOT-VALID-MACHINE TO TRUE
           END-IF

           .
       4000-EXIT.
           EXIT.

      *****************************************************************
      * IF IT IS POSSIBLE, CALCULATE HOW MANY TOKENS ARE REQUIRED TO  *
      * WIN THE PRIZE.                                                *
      *****************************************************************
       4500-CALC-TOKENS.

           IF IS-VALID-MACHINE
               COMPUTE WS-TOTAL-TOKENS = (WS-A-PUSH-AMT * C-A-COST) +
                                         (WS-B-PUSH-AMT * C-B-COST) +
                                         WS-TOTAL-TOKENS
           END-IF
           .
       4500-EXIT.
           EXIT.

      *****************************************************************
      * DISPLAY AMOUNT OF TOKENS                                      *
      *****************************************************************
       8000-DISPLAY-RESULTS.
           
           DISPLAY 'TOTAL TOKENS REQUIRED = ' WS-TOTAL-TOKENS
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