       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISK-FRAGMENTER.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 9 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 9 PROBLEM                     *
      * LINK: https://adventofcode.com/2024/day/9                   *
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
           RECORD CONTAINS 20000 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                           PIC X(20000).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
 
       01  WS-INREC-NUM                           PIC 9(1).
       01  WS-INREC-POINTER                       PIC 9(5) VALUE 1.

       01  WS-DISK-ARR.
           05  WS-DISK   OCCURS 500000 TIMES      PIC S9(5).
                
       01  WS-DISK-POINTER                        PIC 9(6) VALUE 1.

       01  WS-ID-SEQUENCE                         PIC 9(5) VALUE 0.

       01  WS-LEFT-POINTER                        PIC 9(5).
       01  WS-RIGHT-POINTER                       PIC 9(5).
       01  WS-SAVE-VALUE                          PIC S9(5).
       01  WS-ID-SPACE-CNT                        PIC 9(5).
       01  WS-AVAIL-SPACE-CNT                     PIC 9(5).
       01  WS-MOVE-CNT                            PIC 9(5).

       01  WS-CHECKSUM                            PIC 9(18) VALUE 0.
       01  WS-END                                 PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE             THRU 1000-EXIT
           PERFORM 2000-CONVERT-TO-BLOCKS     THRU 2000-EXIT
           PERFORM 3000-CONSOLIDATE-SPACE     THRU 3000-EXIT
           PERFORM 4000-CALCULATE-CHECKSUM    THRU 4000-EXIT
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
           PERFORM 1500-INITIALIZE-ARRAY THRU 1500-EXIT
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * POPULATE -1 IN ENTIRE DISK ARRAY                             *
      ****************************************************************
       1500-INITIALIZE-ARRAY.

           MOVE 1 TO WS-DISK-POINTER
           PERFORM UNTIL WS-DISK-POINTER GREATER 10000
               MOVE -1 TO WS-DISK(WS-DISK-POINTER)
               ADD 1 TO WS-DISK-POINTER
           END-PERFORM
           .
       1500-EXIT.
           EXIT.

      ****************************************************************
      * CONVERT INPUT RECORD TO BLOCK FORMAT                         *
      ****************************************************************
       2000-CONVERT-TO-BLOCKS.
           
           MOVE 1 TO WS-DISK-POINTER

      *    END OF FILE CONDITION = NON-NUMERIC
           PERFORM UNTIL INPUT-RECORD(WS-INREC-POINTER:1) NOT NUMERIC
               MOVE INPUT-RECORD(WS-INREC-POINTER:1) TO WS-INREC-NUM
               PERFORM WS-INREC-NUM TIMES
                   MOVE WS-ID-SEQUENCE TO WS-DISK(WS-DISK-POINTER)
                   ADD 1 TO WS-DISK-POINTER
               END-PERFORM

               ADD 1 TO WS-INREC-POINTER

               MOVE INPUT-RECORD(WS-INREC-POINTER:1) TO WS-INREC-NUM
               PERFORM WS-INREC-NUM TIMES
                   MOVE -1 TO WS-DISK(WS-DISK-POINTER)
                   ADD 1 TO WS-DISK-POINTER
               END-PERFORM

               ADD 1 TO WS-INREC-POINTER
                        WS-ID-SEQUENCE                   
           END-PERFORM
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * MOVE RIGHT-MOST BYTE OF DATA TO FIRST AVAILABLE SPACE         *
      *****************************************************************
       3000-CONSOLIDATE-SPACE.
           
           SUBTRACT 1 FROM WS-DISK-POINTER GIVING WS-RIGHT-POINTER        
           .
           
           KEEP-CONSOLIDATING.     

      *    LOOP UNTIL FILE ID FOUND
           PERFORM UNTIL WS-DISK(WS-RIGHT-POINTER) GREATER -1                
               SUBTRACT 1 FROM WS-RIGHT-POINTER
           END-PERFORM

           MOVE WS-DISK(WS-RIGHT-POINTER) TO WS-SAVE-VALUE  

      *    LOOP UNTIL ALL OF SAME FILE ID ARE FOUND 
           MOVE 0 TO WS-ID-SPACE-CNT
           PERFORM UNTIL WS-DISK(WS-RIGHT-POINTER) 
                         NOT EQUALS WS-SAVE-VALUE                                            
               ADD 1 TO WS-ID-SPACE-CNT
               SUBTRACT 1 FROM WS-RIGHT-POINTER
           END-PERFORM

           ADD 1 TO WS-RIGHT-POINTER
           
      *    LOOP FROM LEFT OF ARRAY, SEARCHING FOR NEEDED SPACE TO 
      *    CONSOLIDATE FILE IN QUESTION
           MOVE 1 TO WS-LEFT-POINTER
           MOVE 0 TO WS-AVAIL-SPACE-CNT

           PERFORM UNTIL WS-LEFT-POINTER EQUALS WS-RIGHT-POINTER
               IF WS-DISK(WS-LEFT-POINTER) EQUALS -1
                   ADD 1 TO WS-AVAIL-SPACE-CNT
               ELSE
                   MOVE 0 TO WS-AVAIL-SPACE-CNT
               END-IF
               
               IF WS-AVAIL-SPACE-CNT EQUALS WS-ID-SPACE-CNT
                   MOVE 0 TO WS-MOVE-CNT
                   PERFORM WS-AVAIL-SPACE-CNT TIMES
                       MOVE -1 TO 
                                 WS-DISK(WS-RIGHT-POINTER + WS-MOVE-CNT)
                       MOVE WS-SAVE-VALUE TO
                                  WS-DISK(WS-LEFT-POINTER - WS-MOVE-CNT)
                       ADD 1 TO WS-MOVE-CNT
                   END-PERFORM
                   GO TO BREAK-LOOP
               END-IF  
               ADD 1 TO WS-LEFT-POINTER
           END-PERFORM
           .
           SUBTRACT 1 FROM WS-RIGHT-POINTER
           .
           BREAK-LOOP.                 
           IF WS-RIGHT-POINTER GREATER 0     
               GO TO KEEP-CONSOLIDATING
           END-IF
           .
       3000-EXIT.
           EXIT.

      *****************************************************************
      * LOOP THROUGH DISK AND CALCULATE CHECKSUM                      *
      *****************************************************************
       4000-CALCULATE-CHECKSUM.

           MOVE 1 TO WS-DISK-POINTER
           PERFORM UNTIL WS-DISK-POINTER EQUALS 500001           
               IF WS-DISK(WS-DISK-POINTER) EQUALS -1
                   CONTINUE
               ELSE 
                   COMPUTE WS-CHECKSUM = 
                       WS-CHECKSUM + 
                      ((WS-DISK-POINTER - 1) * WS-DISK(WS-DISK-POINTER))
               END-IF
               ADD 1 TO WS-DISK-POINTER
           END-PERFORM

           .
       4000-EXIT.
           EXIT.

      *****************************************************************
      * DISPLAY CHECKSUM OF DEFRAGGED FILE                            *
      *****************************************************************
       8000-DISPLAY-RESULTS.
            
           DISPLAY 'CHECKSUM = ' WS-CHECKSUM
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