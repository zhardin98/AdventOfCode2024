       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOOF-IT.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 10 2024.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2024 DAY 10 PROBLEM                    *
      * LINK: https://adventofcode.com/2024/day/10                  *
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
           RECORD CONTAINS 45 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD                           PIC X(45).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                               PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-END-OF-FILE                         PIC X(1).
           88 END-OF-FILE                                   VALUE 'Y'.
           88 NOT-END-OF-FILE                               VALUE 'N'.

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
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM 3000-***** THRU 3000-EXITT
           END-READ
           .
       2000-EXIT.
           EXIT.

      *****************************************************************
      * DISPLAY RESULTING SUM OF PRODUCTS                             *
      *****************************************************************
       8000-DISPLAY-RESULTS.
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