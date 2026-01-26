       IDENTIFICATION DIVISION.
       PROGRAM-ID. Annulus.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COORDINATE-FILE
               ASSIGN TO
               *> Insert your own file path if you are using GnuCOBOL.
               "C:\Users\danie\Desktop\imgbol\image_coordinates.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUTPUT-FILE
               ASSIGN TO
               "C:\Users\danie\Desktop\imgbol\new_coords.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TEMP-FILE
               ASSIGN TO
               "C:/Users/danie/Desktop/imgbol/temp_coords.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  COORDINATE-FILE.
       01  COORD-RAW-LINE      PIC X(50).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05 OUT-X            PIC 9(4).
           05 OUT-SPACE-1      PIC X.
           05 OUT-Y            PIC 9(4).
           05 OUT-SPACE-2      PIC X.
           05 OUT-PIXEL        PIC 9(4). *> When 0000, the pixel is blank.

       FD  TEMP-FILE.
       01  TEMP-RECORD         PIC X(14).
       WORKING-STORAGE SECTION.

      *Equation Variables
       01  DIAMETER-RECEIVE-SMALL      PIC 9(4).
       01  DIAMETER-RECEIVE-LARGE      PIC 9(4).

       01  RADIUS-SQ-1                 PIC 9(10).
       01  RADIUS-SQ-2                 PIC 9(10).

       01  WS-COORDS.
           05 PIXEL-X                     PIC 9(4).
           05 PIXEL-Y                     PIC 9(4).
           05 PIXEL-B                     PIC 9(3).

      *Arithmetic and coordinate Variables
       01  COORD-VARIABLES.
           03  X-COORD-SET                 PIC 9(4).
           03  Y-COORD-SET                 PIC 9(4).
           03  X-COORD-VAR                 PIC 9(4).
           03  Y-COORD-VAR                 PIC 9(4).


      *This is where the integer overflow came in. If you set the
      *Variables defined with PIC 9(10) to any value that truncates, you
      *Will start to see repeating patters.
       01  ARITHMETIC-VARIABLES.
           03  X-VAR-MINUS-X-SET           PIC S9(4).
           03  X-VAR-MINUS-X-SET-SQ        PIC 9(10).
           03  Y-VAR-MINUS-Y-SET           PIC S9(4).
           03  Y-VAR-MINUS-Y-SET-SQ        PIC 9(10).
      *These are used to test if the equation is working.
       01  FINAL-VALUE                     PIC 9(10).



      *These are to find the Brightest point
       01  CHAMPION-VALUES.
           05 MAX-X            PIC 9(4) VALUE 0.
           05 MAX-Y            PIC 9(4) VALUE 0.
           05 MAX-PIXEL        PIC 9(4) VALUE 0.

      *File control variable.
       01  END-OF-FILE         PIC X VALUE "N".

      *This converts the comma delimited file into a space DELIMITED
      *file
       PROCEDURE DIVISION.
       PROGRAM-INIT.
           OPEN INPUT COORDINATE-FILE.
           OPEN OUTPUT TEMP-FILE.      *> This is a 3 phase file manager
                                       *> because I-O operations are limited
                                       *> with line sequential files.
                                       *> All the data editing is done through
                                       *> writing data from one file to another.
           PERFORM RADIUS-INITIALIZATION.
           PERFORM RADIUS-CALCULATION.
           PERFORM CONVERSION-PROCEDURE.
           PERFORM HIGHEST-VALUE-FINDER.

           CLOSE COORDINATE-FILE.

           CLOSE TEMP-FILE.
           PERFORM APPLY-ANNULUS.

           CLOSE OUTPUT-FILE.
           CLOSE TEMP-FILE.
           STOP RUN.

       RADIUS-INITIALIZATION.

           DISPLAY "Please enter the diameter for the inner and outer"
           " circles of the annulus."
           DISPLAY "Inner Circle:".

           ACCEPT DIAMETER-RECEIVE-SMALL.
           DISPLAY "Outer Circle:".
           ACCEPT DIAMETER-RECEIVE-LARGE.
           DISPLAY DIAMETER-RECEIVE-SMALL " " DIAMETER-RECEIVE-LARGE.


       RADIUS-CALCULATION.
      * Calculate Radii Squares
           COMPUTE DIAMETER-RECEIVE-SMALL ROUNDED
                 = DIAMETER-RECEIVE-SMALL / 2.

           COMPUTE RADIUS-SQ-1
                 = DIAMETER-RECEIVE-SMALL * DIAMETER-RECEIVE-SMALL.

           COMPUTE DIAMETER-RECEIVE-LARGE ROUNDED
                 = DIAMETER-RECEIVE-LARGE / 2.

           COMPUTE RADIUS-SQ-2
                 = DIAMETER-RECEIVE-LARGE * DIAMETER-RECEIVE-LARGE.
           DISPLAY "Inner radius squared: " RADIUS-SQ-1.
           DISPLAY "Outer radius squared: " RADIUS-SQ-2.

       CONVERSION-PROCEDURE.

           PERFORM UNTIL END-OF-FILE = "Y"
               READ COORDINATE-FILE
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       PERFORM WRITE-COORDINATES-TO-FILE
               END-READ
           END-PERFORM.



       WRITE-COORDINATES-TO-FILE.
           UNSTRING COORD-RAW-LINE
                   DELIMITED BY ","
                   INTO PIXEL-X
                        PIXEL-Y
                        PIXEL-B
           END-UNSTRING
      *Note: You assigned zero to MAX-PIXEL from the gate, therefore
      *    The first PIXEL-BRIGHTNESS is always going to initialize this.
           IF PIXEL-B > MAX-PIXEL THEN
                    MOVE PIXEL-B             TO MAX-PIXEL
                    MOVE PIXEL-X             TO MAX-X
                    MOVE PIXEL-Y             TO MAX-Y
           END-IF

           MOVE PIXEL-X                      TO OUT-X
           MOVE SPACE                        TO OUT-SPACE-1
           MOVE PIXEL-Y                      TO OUT-Y
           MOVE SPACE                        TO OUT-SPACE-2
           MOVE PIXEL-B                      TO OUT-PIXEL

           WRITE TEMP-RECORD FROM OUTPUT-RECORD.





      *This displays the brightest pixel and its coordinates. That is where
      * your annulus will generate.
       HIGHEST-VALUE-FINDER.
           DISPLAY "HIGHEST BRIGHTNESS FOUND: " MAX-PIXEL.
           DISPLAY "LOCATED AT X: " MAX-X " Y: " MAX-Y.


       APPLY-ANNULUS.
      * Open TEMP as Input to read what you just wrote
           OPEN INPUT TEMP-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           MOVE "N" TO END-OF-FILE.

           PERFORM UNTIL END-OF-FILE = "Y"
               READ TEMP-FILE INTO OUTPUT-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       PERFORM ANNULUS-CHECK
                       PERFORM ANNULUS-CHECK-2
                       PERFORM ANNULUS-CHECK-3

                       *> This WRITE now goes to the final file
                       MOVE SPACE TO OUT-SPACE-1
                       MOVE SPACE TO OUT-SPACE-2
                       WRITE OUTPUT-RECORD
               END-READ
           END-PERFORM.

       ANNULUS-CHECK.
      * Calculate Distance Squares (using Signed variables)
           COMPUTE X-VAR-MINUS-X-SET = MAX-X - OUT-X.

           COMPUTE X-VAR-MINUS-X-SET-SQ
               =   X-VAR-MINUS-X-SET * X-VAR-MINUS-X-SET.


           COMPUTE Y-VAR-MINUS-Y-SET = MAX-Y - OUT-Y.

           COMPUTE Y-VAR-MINUS-Y-SET-SQ
               =   Y-VAR-MINUS-Y-SET * Y-VAR-MINUS-Y-SET.

       ANNULUS-CHECK-2.
      * Calculate final value needed for the logical test
           COMPUTE FINAL-VALUE =
                        X-VAR-MINUS-X-SET-SQ
                           + Y-VAR-MINUS-Y-SET-SQ.

       ANNULUS-CHECK-3.
      * 0000 Will produce a blank pixel when rendered from the data file
           IF FINAL-VALUE < RADIUS-SQ-1 OR FINAL-VALUE > RADIUS-SQ-2
               MOVE 0 TO OUT-PIXEL
           END-IF.
