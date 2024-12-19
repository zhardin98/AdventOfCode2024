       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHRONOSPATIAL-COMPUTER.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 17 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 17 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/17                  *
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
           RECORD CONTAINS 3 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                          PIC X(40).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                    VALUE 'Y'.
           88 NOT-END-OF-FILE                                VALUE 'N'.

       01  WS-REG-A                               PIC 9(20).
       01  WS-REG-B                               PIC 9(20).
       01  WS-REG-C                               PIC 9(20).

       01  WS-REG-INPUT                           PIC X(20).
       01  WS-REG-NUMERIC                         PIC 9(20).
      
      *SIGNED DUE TO POSSIBILITY VARIABLE COULD BECOME NEGATIVE (JNZ)
       01  WS-INPUT-POINTER                       PIC S9(2).
       01  WS-INPUT-DIGIT-CNT                     PIC 9(2).

       01  WS-SOURCE-CODE                         PIC X(50).
       01  WS-INSTRUCTION                         PIC 9(1).
           88 ADV                                           VALUE 0.
           88 BXL                                           VALUE 1.
           88 BST                                           VALUE 2.
           88 JNZ                                           VALUE 3.
           88 BXC                                           VALUE 4.
           88 OUT                                           VALUE 5.
           88 BDV                                           VALUE 6.
           88 CDV                                           VALUE 7.
       01  WS-PARAMETER                           PIC 9(1).
       01  WS-COMBO                               PIC 9(30).
       01  WS-BIN-1                               PIC 9(16).
       01  WS-BIN-1-X REDEFINES WS-BIN-1          PIC X(16).
       01  WS-BIN-2                               PIC 9(16). 
       01  WS-BIN-2-X REDEFINES WS-BIN-2          PIC X(16).
       01  WS-BIN-XOR-RESULT                      PIC 9(16).
       01  WS-BIN-XOR-RESULT-X REDEFINES WS-BIN-XOR-RESULT          
                                                  PIC X(16).

       01  WS-BIN-POINTER                         PIC 9(2).

       01  WS-POINTER-TARGET                      PIC 9(1).
       01  WS-OUTPUT                              PIC ZZZZ9.


      *FOR DECIMAL TO BINARY CONVERSION
       01 INPUT-DECIMAL                           PIC 9(5)  VALUE 0.
       01 OUTPUT-BINARY                           PIC 9(16) VALUE 0.
       01 REMAINDER-FIELD                         PIC 9(1)  VALUE 0.
       01 TEMP-VALUE                              PIC 9(5)  VALUE 0.
       01 MULTIPLIER                              PIC 9(16) VALUE 1.
       01 BINARY-DIGIT                            PIC 9(1)  VALUE 0.

      *FOR BINARY TO DECIMAL CONVERSION
       01 BINARY-INPUT                            PIC X(16) VALUE 
                                                                 SPACES.
       01 DECIMAL-OUTPUT                          PIC 9(5)  VALUE 0.
       01 INPUT-LENGTH                            PIC 9(2)  VALUE 0.
       01 CURRENT-POSITION                        PIC 9(2)  VALUE 0.
       01 CURRENT-DIGIT                           PIC 9(1)  VALUE 0.
       01 POWER-OF-TWO                            PIC 9(5)  VALUE 1.

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
           PERFORM 3000-EXECUTE-PROGRAM       THRU 3000-EXIT
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
                   SET END-OF-FILE TO TRUE
               NOT AT END 
                   MOVE SPACES TO WS-REG-INPUT        
                   EVALUATE TRUE
                       WHEN INPUT-RECORD(1:12) EQUALS 'Register A: '
                           MOVE INPUT-RECORD(13:27) TO WS-REG-INPUT
                           PERFORM 2100-CONVERT-REGISTER THRU 2100-EXIT
                           MOVE WS-REG-NUMERIC TO WS-REG-A
                       WHEN INPUT-RECORD(1:12) EQUALS 'Register B: '                     
                           MOVE INPUT-RECORD(13:27) TO WS-REG-INPUT
                           PERFORM 2100-CONVERT-REGISTER THRU 2100-EXIT
                           MOVE WS-REG-NUMERIC TO WS-REG-B
                       WHEN INPUT-RECORD(1:12) EQUALS 'Register C: '
                           MOVE INPUT-RECORD(13:27) TO WS-REG-INPUT
                           PERFORM 2100-CONVERT-REGISTER THRU 2100-EXIT
                           MOVE WS-REG-NUMERIC TO WS-REG-C
                       WHEN INPUT-RECORD(1:9) EQUALS 'Program: '
                           MOVE INPUT-RECORD(10:31) TO WS-SOURCE-CODE
                   END-EVALUATE
           END-READ
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * CONVERT INPUT VALUE TO A NUMERIC                              *
      *****************************************************************
       2100-CONVERT-REGISTER.
           
           MOVE 0 TO WS-REG-NUMERIC
           MOVE 1 TO WS-INPUT-POINTER
                     WS-INPUT-DIGIT-CNT
           PERFORM UNTIL 
           WS-REG-INPUT(WS-INPUT-POINTER:WS-INPUT-DIGIT-CNT) NOT NUMERIC           
               MOVE WS-REG-INPUT(WS-INPUT-POINTER:WS-INPUT-DIGIT-CNT)
                   TO WS-REG-NUMERIC
               ADD 1 TO WS-INPUT-DIGIT-CNT
           END-PERFORM
           .
       2100-EXIT.
           EXIT.

      *****************************************************************
      * PROCESS INSTRUCTIONS                                          *
      *****************************************************************
       3000-EXECUTE-PROGRAM.

           MOVE 1 TO WS-INPUT-POINTER

           PERFORM UNTIL WS-SOURCE-CODE(WS-INPUT-POINTER:1) EQUALS ' '
TEST  *         DISPLAY 'REG A = ' WS-REG-A
TEST  *         DISPLAY 'REG B = ' WS-REG-B
TEST  *         DISPLAY 'REG C = ' WS-REG-C
TEST  *         DISPLAY '*********************************'
               MOVE WS-SOURCE-CODE(WS-INPUT-POINTER:1) TO WS-INSTRUCTION            
               EVALUATE TRUE
      *            DIVIDES TO A
                   WHEN ADV
                       PERFORM 3100-GET-COMBO THRU 3100-EXIT
                       COMPUTE WS-REG-A = WS-REG-A / (2 ** WS-COMBO)
      *            BITWISE XOR B AND LITERAL
                   WHEN BXL               
                       MOVE WS-REG-B TO INPUT-DECIMAL
                       PERFORM 3200-NUM-TO-BIN THRU 3200-EXIT
TEST  *                 DISPLAY 'BIN1 = ' OUTPUT-BINARY  
                       MOVE OUTPUT-BINARY TO WS-BIN-1
                       ADD 2 TO WS-INPUT-POINTER
                       MOVE WS-SOURCE-CODE(WS-INPUT-POINTER:1) 
                                                        TO INPUT-DECIMAL
                       PERFORM 3200-NUM-TO-BIN THRU 3200-EXIT
                       MOVE OUTPUT-BINARY TO WS-BIN-2
TEST  *                 DISPLAY 'BIN2 = ' OUTPUT-BINARY                         
                       PERFORM 3300-XOR        THRU 3300-EXIT
TEST  *                 DISPLAY 'RES  = ' WS-BIN-XOR-RESULT                       
                       MOVE WS-BIN-XOR-RESULT TO BINARY-INPUT
                       PERFORM 3250-BIN-TO-NUM THRU 3250-EXIT  
TEST  *                 DISPLAY 'EXPL = ' DECIMAL-OUTPUT 
                       MOVE DECIMAL-OUTPUT TO WS-REG-B
      *            MODULO 8 -> B
                   WHEN BST
                       PERFORM 3100-GET-COMBO THRU 3100-EXIT
                       MOVE FUNCTION MOD(WS-COMBO,8) TO WS-REG-B
      *            JUMP NOT ZERO
                   WHEN JNZ
                       IF WS-REG-A EQUALS 0
                           ADD 2 TO WS-INPUT-POINTER
                       ELSE
                           ADD 2 TO WS-INPUT-POINTER
                           MOVE WS-SOURCE-CODE(WS-INPUT-POINTER:1)
                                TO WS-POINTER-TARGET
                           COMPUTE WS-INPUT-POINTER =
                                   WS-POINTER-TARGET - 1
                       END-IF
      *            BITWISE XOR B AND C        
                   WHEN BXC
                       MOVE WS-REG-B TO INPUT-DECIMAL
                       PERFORM 3200-NUM-TO-BIN THRU 3200-EXIT
                       MOVE OUTPUT-BINARY TO WS-BIN-1
                       ADD 2 TO WS-INPUT-POINTER
                       MOVE WS-REG-C TO INPUT-DECIMAL
                       PERFORM 3200-NUM-TO-BIN THRU 3200-EXIT
                       MOVE OUTPUT-BINARY TO WS-BIN-2
                       PERFORM 3300-XOR        THRU 3300-EXIT
                       MOVE WS-BIN-XOR-RESULT TO BINARY-INPUT
                       PERFORM 3250-BIN-TO-NUM THRU 3250-EXIT  
                       MOVE DECIMAL-OUTPUT TO WS-REG-B                   
      *            MODULO 8 -> OUTPUT
                   WHEN OUT
                       PERFORM 3100-GET-COMBO THRU 3100-EXIT
                       MOVE FUNCTION MOD(WS-COMBO,8) TO WS-OUTPUT
                       DISPLAY WS-OUTPUT
      *            DIVIDES TO B
                   WHEN BDV
                       PERFORM 3100-GET-COMBO THRU 3100-EXIT
                       COMPUTE WS-REG-B = WS-REG-A / (2 ** WS-COMBO)                   
      *            DIVIDES TO C
                   WHEN CDV
                       PERFORM 3100-GET-COMBO THRU 3100-EXIT
                       COMPUTE WS-REG-C = WS-REG-A / (2 ** WS-COMBO)                    
               END-EVALUATE
               ADD 2 TO WS-INPUT-POINTER
           END-PERFORM
           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * GET THE COMBO OPERAND                                         *
      *****************************************************************
       3100-GET-COMBO.
           
           ADD  2 TO WS-INPUT-POINTER
           MOVE 0 TO WS-COMBO        
           EVALUATE WS-SOURCE-CODE(WS-INPUT-POINTER:1)
               WHEN '0'
               WHEN '1'
               WHEN '2'
               WHEN '3'
                   MOVE WS-SOURCE-CODE(WS-INPUT-POINTER:1) 
                                 TO WS-COMBO
               WHEN '4'
                   MOVE WS-REG-A TO WS-COMBO
               WHEN '5'
                   MOVE WS-REG-B TO WS-COMBO
               WHEN '6'
                   MOVE WS-REG-C TO WS-COMBO
               WHEN OTHER
                   DISPLAY 'INVALID COMBO OPERAND' 
                           WS-SOURCE-CODE(WS-INPUT-POINTER:1)
           END-EVALUATE
           .
       3100-EXIT.
           EXIT.        

      *****************************************************************
      * CONVERT GIVEN NUMERIC INTO BINARY                             *
      *****************************************************************
       3200-NUM-TO-BIN.

           MOVE INPUT-DECIMAL TO TEMP-VALUE
           MOVE 1 TO MULTIPLIER
           MOVE 0 TO OUTPUT-BINARY

           PERFORM UNTIL TEMP-VALUE = 0
               DIVIDE TEMP-VALUE BY 2 GIVING TEMP-VALUE 
                   REMAINDER REMAINDER-FIELD
               MOVE REMAINDER-FIELD TO BINARY-DIGIT
               COMPUTE OUTPUT-BINARY = OUTPUT-BINARY + 
                   (BINARY-DIGIT * MULTIPLIER)
               MULTIPLY 10 BY MULTIPLIER
           END-PERFORM
           .
       3200-EXIT.
           EXIT.       

      *****************************************************************
      * CONVERT GIVEN BINARY INTO NUMERIC                             *
      *****************************************************************
       3250-BIN-TO-NUM.

           MOVE 0 TO INPUT-LENGTH
           INSPECT BINARY-INPUT TALLYING INPUT-LENGTH FOR 
               CHARACTERS BEFORE " "

           MOVE 0 TO DECIMAL-OUTPUT.
           MOVE 1 TO POWER-OF-TWO.

           PERFORM VARYING CURRENT-POSITION FROM INPUT-LENGTH BY -1
               UNTIL CURRENT-POSITION = 0
               MOVE FUNCTION NUMVAL(BINARY-INPUT(CURRENT-POSITION:1)) 
                   TO CURRENT-DIGIT
               IF CURRENT-DIGIT = 1
                   ADD POWER-OF-TWO TO DECIMAL-OUTPUT
               END-IF
               MULTIPLY 2 BY POWER-OF-TWO
           END-PERFORM
           .
       3250-EXIT.
           EXIT.       

      *****************************************************************     
      * TAKE EXCLUSIVE OR BETWEEN TWO BINARY VALUES                   *
      *****************************************************************
       3300-XOR.
           
           MOVE 1 TO WS-BIN-POINTER
           PERFORM UNTIL WS-BIN-POINTER GREATER 16
               IF ((WS-BIN-1-X(WS-BIN-POINTER:1) EQUALS '0' AND 
                    WS-BIN-2-X(WS-BIN-POINTER:1) EQUALS '0') OR
                   (WS-BIN-1-X(WS-BIN-POINTER:1) EQUALS '1' AND 
                    WS-BIN-2-X(WS-BIN-POINTER:1) EQUALS '1'))
                   MOVE '0' TO WS-BIN-XOR-RESULT-X(WS-BIN-POINTER:1)
               ELSE
                  MOVE '1' TO WS-BIN-XOR-RESULT-X(WS-BIN-POINTER:1)
               END-IF                  
               ADD 1 TO WS-BIN-POINTER
           END-PERFORM
           .
       3300-EXIT.
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