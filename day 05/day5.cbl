       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINT-QUEUE.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 5 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 5 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/5                  *
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
           RECORD CONTAINS 68 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                          PIC X(68).


       WORKING-STORAGE SECTION.
       01  WS-BEGIN                              PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-END-OF-FILE                       PIC X(1).
           88 END-OF-FILE                                 VALUE 'Y'.
           88 NOT-END-OF-FILE                             VALUE 'N'.
       01  WS-END-OF-RULES                      PIC X(1).
           88 END-OF-RULES                                VALUE 'Y'.
           88 NOT-END-OF-RULES                            VALUE 'N'.
       01  WS-VALID-SEQ                         PIC X(1).
           88 VALID-SEQ                                   VALUE 'Y'.
           88 INVALID-SEQ                                 VALUE 'N'.

       01  WS-PAGE-ORDER-RULES-REC.
           05 WS-PAGE-RULE-1                    PIC X(2).
           05 WS-PAGE-RULE-DELIM                PIC X(1).
           05 WS-PAGE-RULE-2                    PIC X(2).
           05 FILLER                            PIC X(63).

       01  WS-RULES-ARR.          
           05 WS-RULES OCCURS 100 TIMES.
               10 WS-RULES-AFTER                PIC X(180) VALUE SPACES.
       01  WS-RULES-SUB                         PIC 9(2).
       01  WS-RULES-POINTER                     PIC 9(2).
  
       01  WS-PAGES-FOR-PRINT-REC               PIC X(68).    
       01  WS-REC-POINTER                       PIC 9(2).

       01  WS-ORDER-ARR.
           05 WS-ORDER OCCURS 30 TIMES          PIC 9(2).
       01  WS-ORDER-SUB                         PIC 9(2).
       01  WS-ORDER-SUB2                        PIC 9(2).
       01  WS-TEMP-NUM                          PIC 9(2).
       01  WS-ARR-COUNT                         PIC 9(2).
       
       01  WS-MID-TOTAL-1                       PIC 9(6).
       01  WS-MID-TOTAL-2                       PIC 9(6).
       01  WS-END                               PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE             THRU 1000-EXIT
           PERFORM 2000-PROCESS-RULES         THRU 2000-EXIT
               UNTIL END-OF-RULES
           PERFORM 3000-VALIDATE-PRINTS       THRU 3000-EXIT
               UNTIL END-OF-FILE
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

      *****************************************************************
      * PROCESS PAGE ORDERING RULES                                   *
      *****************************************************************
       2000-PROCESS-RULES.

           PERFORM 4000-READ-REC THRU 4000-EXIT
           
           MOVE INPUT-RECORD TO WS-PAGE-ORDER-RULES-REC
           IF WS-PAGE-RULE-1 NUMERIC
               CONTINUE
           ELSE
               SET END-OF-RULES TO TRUE
               GO TO 2000-EXIT
           END-IF

           MOVE WS-PAGE-RULE-1 TO WS-RULES-SUB
           MOVE 1 TO WS-RULES-POINTER
           PERFORM UNTIL WS-RULES(WS-RULES-SUB)(WS-RULES-POINTER:2) 
                         EQUALS SPACES
           ADD 2 TO WS-RULES-POINTER
           END-PERFORM

           MOVE WS-PAGE-RULE-2 TO 
               WS-RULES(WS-RULES-SUB)(WS-RULES-POINTER:2)
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * VALIDATE PRINTOUTS AND CONFIRM THEY FOLLOW THE ORDERING RULES *
      *****************************************************************
       3000-VALIDATE-PRINTS.

           PERFORM 4000-READ-REC THRU 4000-EXIT
           IF END-OF-FILE
               GO TO 3000-EXIT
           END-IF

      *    MOVE SEQUENCE INTO ARRAY
           MOVE 0            TO WS-ORDER-ARR
           MOVE INPUT-RECORD TO WS-PAGES-FOR-PRINT-REC

           MOVE 1 TO WS-REC-POINTER    
                     WS-ORDER-SUB
           PERFORM UNTIL WS-PAGES-FOR-PRINT-REC(WS-REC-POINTER:2)
                         NOT NUMERIC
               MOVE WS-PAGES-FOR-PRINT-REC(WS-REC-POINTER:2)
                    TO WS-ORDER(WS-ORDER-SUB)
               ADD 1 TO WS-ORDER-SUB
               ADD 3 TO WS-REC-POINTER
           END-PERFORM

      *    VALIDATE SEQUENCE
           MOVE 0      TO WS-ARR-COUNT
                          WS-ORDER-SUB2
           MOVE SPACES TO WS-VALID-SEQ
           MOVE 1      TO WS-ORDER-SUB
           PERFORM UNTIL WS-ORDER(WS-ORDER-SUB) EQUALS SPACES
               ADD 1 TO WS-ORDER-SUB GIVING WS-ORDER-SUB2
               PERFORM UNTIL WS-ORDER(WS-ORDER-SUB2) EQUALS SPACES
                   MOVE WS-ORDER(WS-ORDER-SUB2) TO WS-RULES-SUB
                   MOVE 1                       TO WS-RULES-POINTER
                   PERFORM UNTIL 
                   WS-RULES(WS-RULES-SUB)(WS-RULES-POINTER:2) 
                   EQUALS SPACES
                   IF WS-RULES(WS-RULES-SUB)(WS-RULES-POINTER:2)
                      EQUALS WS-ORDER(WS-ORDER-SUB)
                       SET INVALID-SEQ TO TRUE
                   END-IF
                   ADD 2 TO WS-RULES-POINTER
                   END-PERFORM
                   ADD 1 TO WS-ORDER-SUB2
               END-PERFORM
               ADD 1 TO WS-ORDER-SUB
                        WS-ARR-COUNT
           END-PERFORM

           IF INVALID-SEQ
               PERFORM 5000-CORRECT-RECORD THRU 5000-EXIT
           ELSE
               COMPUTE WS-ARR-COUNT = ((WS-ARR-COUNT - 1) / 2) + 1
               ADD WS-ORDER(WS-ARR-COUNT) TO WS-MID-TOTAL-1
           END-IF

           .
       3000-EXIT.
           EXIT.  
      
      *****************************************************************
      * READ FILE RECORD BY RECORD                                    *
      *****************************************************************
       4000-READ-REC.

           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   CONTINUE
           END-READ 
           .
       4000-EXIT.
           EXIT.

      *****************************************************************
      * REORDER INVALID SEQUENCES INTO A VALID ORDER                  *
      *****************************************************************
       5000-CORRECT-RECORD.

      *    LOOP BACKWARDS THROUGH SEQUENCE TO REORDER
           MOVE SPACES        TO WS-VALID-SEQ
           MOVE WS-ARR-COUNT TO WS-ORDER-SUB

           PERFORM UNTIL WS-ORDER-SUB EQUALS 0
               MOVE WS-ORDER(WS-ORDER-SUB) TO WS-RULES-SUB
               MOVE 1 TO WS-RULES-POINTER
      *        SEND SECOND POINTER BACKWARDS
               SUBTRACT 1 FROM WS-ORDER-SUB GIVING WS-ORDER-SUB2
               PERFORM UNTIL WS-ORDER-SUB2 EQUALS 0
      *            CHECK RULES ARRAY FOR RULE THAT 
      *            POINTER1 MUST PRECEDE POINTER2
      *            IF SO, SWAP VALUES AND RESTART BACKWARDS LOOP
                   MOVE WS-ORDER(WS-ORDER-SUB)  TO WS-RULES-SUB
                   MOVE 1                       TO WS-RULES-POINTER
                   PERFORM UNTIL 
                              WS-RULES(WS-RULES-SUB)(WS-RULES-POINTER:2) 
                              EQUALS SPACES
                   IF WS-RULES(WS-RULES-SUB)(WS-RULES-POINTER:2)
                      EQUALS WS-ORDER(WS-ORDER-SUB2)
                       MOVE WS-ORDER(WS-ORDER-SUB)  TO WS-TEMP-NUM
                       MOVE WS-ORDER(WS-ORDER-SUB2) TO
                                                  WS-ORDER(WS-ORDER-SUB) 
                       MOVE WS-TEMP-NUM             TO 
                                                 WS-ORDER(WS-ORDER-SUB2)
                       GO TO 5000-CORRECT-RECORD
                   END-IF
                   ADD 2 TO WS-RULES-POINTER
                   END-PERFORM
                   SUBTRACT 1 FROM WS-ORDER-SUB2
               END-PERFORM
               SUBTRACT 1 FROM WS-ORDER-SUB
           END-PERFORM

           COMPUTE WS-ARR-COUNT = ((WS-ARR-COUNT - 1) / 2) + 1
           ADD WS-ORDER(WS-ARR-COUNT) TO WS-MID-TOTAL-2
           .
       5000-EXIT.
           EXIT.

      *****************************************************************
      * DISPLAY RESULTING SUM OF PRODUCTS                             *
      *****************************************************************
       8000-DISPLAY-RESULTS.
           DISPLAY 'TOTAL OF   VALID MIDDLES = ' WS-MID-TOTAL-1
           DISPLAY 'TOTAL OF INVALID MIDDLES = ' WS-MID-TOTAL-2
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