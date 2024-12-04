       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULL-IT-OVER.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 3 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 3 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/3                  *
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
           RECORD CONTAINS 3005 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                         PIC X(3005).


       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.
       01  WS-END-OF-RECORD                       PIC X(1).
           88 END-OF-RECORD                                 VALUE 'Y'.
           88 NOT-END-OF-RECORD                             VALUE 'N'.
       01  WS-NUMERIC-FOUND                       PIC X(1).
           88 NUMERIC-FOUND                                 VALUE 'Y'.
           88 NO-NUMERIC-FOUND                              VALUE 'N'.   
       01  WS-COND-FLAG                           PIC X(1).
           88 DO-RUN                                        VALUE 'Y'.
           88 DONT-RUN                                      VALUE 'N'.
           
       01  WS-REC-POINTER                         PIC 9(4).
       01  WS-INS-POINTER                         PIC 9(4).
       01  WS-X-NUM                               PIC 9(9).
       01  WS-Y-NUM                               PIC 9(9).
       01  WS-PRODUCT                             PIC 9(20).
       01  WS-TOTAL                               PIC 9(20) VALUE 0.
       01  WS-NUM-CHAR-X                          PIC X(1).
       01  WS-NUM-CHAR-9 REDEFINES WS-NUM-CHAR-X  PIC 9(1).
       01  WS-FOUND-NUM                           PIC 9(9).
       01  C-NEW-LINE-CHAR                        PIC X(1)  VALUE X'00'.

       01  WS-END                                 PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE      THRU 1000-EXIT
           SET DO-RUN TO TRUE
           PERFORM 2000-PROCESS-DATA   THRU 2000-EXIT
               UNTIL END-OF-FILE
           PERFORM 8000-DISPLAY-RESULT THRU 8000-EXIT
           PERFORM 9000-CLOSE-FILE     THRU 9000-EXIT
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
       2000-PROCESS-DATA.
       
           READ INPUT-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM 3000-SCAN-RECORD THRU 3000-EXIT
           END-READ
           .
       2000-EXIT.
           EXIT.
     
      ****************************************************************
      * SCAN RECORD FOR VALID MULTIPLICATION INSTRUCTIONS            *
      ****************************************************************
       3000-SCAN-RECORD.

           MOVE 1 TO WS-REC-POINTER
           PERFORM UNTIL INPUT-RECORD(WS-REC-POINTER:1) EQUALS 
                                                         C-NEW-LINE-CHAR
               IF INPUT-RECORD(WS-REC-POINTER:4) EQUALS 'do()'
                   SET DO-RUN TO TRUE
               END-IF

               IF INPUT-RECORD(WS-REC-POINTER:7) EQUALS 'don''t()'
                   SET DONT-RUN TO TRUE
               END-IF
               
               IF INPUT-RECORD(WS-REC-POINTER:3) EQUALS 'mul' AND DO-RUN
                   PERFORM 3100-VALIDATE-INSTRUCTION THRU 3100-EXIT
               END-IF
               ADD 1 TO WS-REC-POINTER
           END-PERFORM
           .
       3000-EXIT.
           EXIT.

      ****************************************************************
      * VALIDATE FORMAT OF MULTIPLY INSTRUCTION: mul(X,Y)            *
      ****************************************************************
       3100-VALIDATE-INSTRUCTION.

      *    VERIFY LEFT PARENTHETICAL   
           ADD 3 TO WS-REC-POINTER GIVING WS-INS-POINTER
           IF INPUT-RECORD(WS-INS-POINTER:1) NOT EQUALS '('
               GO TO 3100-EXIT
           END-IF

           

      *    VERIFY FIRST (X) NUMBER
           PERFORM 3150-VALIDATE-NUMBER THRU 3150-EXIT
           MOVE WS-FOUND-NUM TO WS-X-NUM
           IF NO-NUMERIC-FOUND       
               GO TO 3100-EXIT
           END-IF
      
      *    VERIFY COMMA
      *    WS-INS-POINTER IS INCREMENTED BY END OF PARA 3150
           IF INPUT-RECORD(WS-INS-POINTER:1) NOT EQUALS ','
               GO TO 3100-EXIT
           END-IF
      
      *    VERIFY FIRST (Y) NUMBER
           PERFORM 3150-VALIDATE-NUMBER THRU 3150-EXIT
           MOVE WS-FOUND-NUM TO WS-Y-NUM
           IF NO-NUMERIC-FOUND
               GO TO 3100-EXIT
           END-IF

      *    VERIFY RIGHT PARENTHETICAL   
      *    WS-INS-POINTER IS INCREMENTED BY END OF PARA 3150
           IF INPUT-RECORD(WS-INS-POINTER:1) NOT EQUALS ')'            
               GO TO 3100-EXIT
           END-IF

      *    IF WE REACH THIS INSTRUCTION, THAT MEANS WE HAVE A VALID
      *    MULTIPLICATION AND CAN CALCULATE A PRODUCT
           PERFORM 4000-MULTIPLY-PRODUCT THRU 4000-EXIT
           PERFORM 4100-ADD-RESULT       THRU 4100-EXIT
      *    MOVE WS-INS-POINTER TO WS-REC-POINTER
           .
       3100-EXIT.
           EXIT.
       
      ****************************************************************
      * INVESTIGATE INSTURCTIONS CHARACTER BY CHARACTER AND FIND     *
      * NUMBER.                                                      *
      ****************************************************************
       3150-VALIDATE-NUMBER.
           ADD 1 TO WS-INS-POINTER
           MOVE 0 TO WS-FOUND-NUM
      *    ASSUMING VALUE IS NOT A NUMERIC UNTIL WE CONFIRM ONE EXISTS
           SET NO-NUMERIC-FOUND TO TRUE
           PERFORM UNTIL INPUT-RECORD(WS-INS-POINTER:1) NOT NUMERIC
               MULTIPLY 10 BY WS-FOUND-NUM
               MOVE INPUT-RECORD(WS-INS-POINTER:1) TO WS-NUM-CHAR-X
               ADD WS-NUM-CHAR-9 TO WS-FOUND-NUM
               ADD 1 TO WS-INS-POINTER
               SET NUMERIC-FOUND TO TRUE   

           END-PERFORM
           .
       3150-EXIT.
           EXIT.

      ****************************************************************
      * MULTIPLY TWO NUMBERS                                         *
      ****************************************************************
       4000-MULTIPLY-PRODUCT.

           MULTIPLY WS-X-NUM BY WS-Y-NUM GIVING WS-PRODUCT
           .
       4000-EXIT.
           EXIT.

      ****************************************************************
      * ADD FOUND PRODUCT TO RESULT OUTPUT                           *
      ****************************************************************
       4100-ADD-RESULT.

           ADD WS-PRODUCT TO WS-TOTAL
           .
       4100-EXIT.
           EXIT.

      ****************************************************************
      * DISPLAY RESULTING SUM OF PRODUCTS                            *
      ****************************************************************
       8000-DISPLAY-RESULT.

           DISPLAY 'TOTAL = ' WS-TOTAL
           .
       8000-EXIT.
           EXIT.

      ****************************************************************
      * CLOSE FILE                                                   *
      ****************************************************************
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
