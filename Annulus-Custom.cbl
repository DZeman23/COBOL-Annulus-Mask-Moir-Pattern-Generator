       IDENTIFICATION DIVISION.
       PROGRAM-ID. Annulus-Custom.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COORDINATE-FILE
               ASSIGN TO
               "C:\Users\danie\Desktop\ImageBOL\image_coordinates.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUTPUT-FILE
               ASSIGN TO
               "C:\Users\danie\Desktop\ImageBOL\new_coords.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TEMP-FILE
               ASSIGN TO
               "C:\Users\danie\Desktop\ImageBOL\temp_coords.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  COORDINATE-FILE.
       01  COORD-RAW-LINE      PIC X(50).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05 OUT-X            PIC 9(4).
           05 FILLER           PIC X VALUE SPACE.
           05 OUT-Y            PIC 9(4).
           05 FILLER           PIC X VALUE SPACE.
           05 OUT-PIXEL        PIC 9(4).

       FD  TEMP-FILE.
       01  TEMP-RECORD         PIC X(14).
       WORKING-STORAGE SECTION.

      *Equation Variables
       01  DIAMETER-RECEIVE-SMALL      PIC 9(4).
       01  DIAMETER-RECEIVE-LARGE      PIC 9(4).

       01  RADIUS-SQ-1                 PIC 9(4).
       01  RADIUS-SQ-2                 PIC 9(4).

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

           *> FUN EXPERIMENT: Change the square variables to a lower
           *> pic VALUE for some crazy psychedelic patterns.

       01  ARITHMETIC-VARIABLES.
           03  X-VAR-MINUS-X-SET           PIC S9(4).
           03  X-VAR-MINUS-X-SET-SQ        PIC 9(10).
           03  Y-VAR-MINUS-Y-SET           PIC S9(4).
           03  Y-VAR-MINUS-Y-SET-SQ        PIC 9(10).
      *These are used to test if the equation is working.
       01  FINAL-VALUE                     PIC 9(10).



      *These are to find the highest point
       01  CHAMPION-VALUES.
           05 MAX-X            PIC 9(4) VALUE 0.
           05 MAX-Y            PIC 9(4) VALUE 0.
           05 MAX-PIXEL        PIC 9(4) VALUE 0.

       01  END-OF-FILE         PIC X VALUE "N".

      *This converts the comma delimited file into a space DELIMITED
      *file
       PROCEDURE DIVISION.
       PROGRAM-INIT.
           OPEN INPUT COORDINATE-FILE.
           OPEN OUTPUT TEMP-FILE.      *> Open TEMP, not Output

           PERFORM RADIUS-INITIALIZATION.
           PERFORM RADIUS-CALCULATION.
           PERFORM CENTER-POINT-INPUT.
           PERFORM CONVERSION-PROCEDURE.


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
           DISPLAY RADIUS-SQ-1 " " RADIUS-SQ-2.


       CENTER-POINT-INPUT.
           DISPLAY "Please enter the X and Y coordinates for the center"
           DISPLAY "X Coordinate:".
           ACCEPT MAX-X.
           DISPLAY "Y Coordinate:".
           ACCEPT MAX-Y.
           DISPLAY "Center set to X: " MAX-X " Y: " MAX-Y.


       CONVERSION-PROCEDURE.

           PERFORM UNTIL END-OF-FILE = "Y"
               READ COORDINATE-FILE
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       PERFORM WRITE-COORDINATES-TO-FILE
               END-READ
           END-PERFORM.


      *In the future, focus on ways in which you can split things up for
      *    readability. You will thank yourself.
       WRITE-COORDINATES-TO-FILE.
           UNSTRING COORD-RAW-LINE
                   DELIMITED BY ","
                   INTO PIXEL-X
                        PIXEL-Y
                        PIXEL-B
           END-UNSTRING
      *Note: You assigned zero to MAX-PIXEL from the gate, therefore
      *    The first PIXEL-BRIGHTNESS is always going to initialize this.
      *     IF PIXEL-B > MAX-PIXEL THEN
      *              MOVE PIXEL-B             TO MAX-PIXEL
      *              MOVE PIXEL-X             TO MAX-X
      *              MOVE PIXEL-Y             TO MAX-Y
      *     END-IF

           MOVE PIXEL-X                      TO OUT-X
           MOVE PIXEL-Y                      TO OUT-Y
           MOVE PIXEL-B                      TO OUT-PIXEL

           WRITE TEMP-RECORD FROM OUTPUT-RECORD.





      *This displays the highest pixel and its coordinates. Commented for redundancy
      *HIGHEST-VALUE-FINDER.
      *     DISPLAY "HIGHEST BRIGHTNESS FOUND: " MAX-PIXEL.
      *     DISPLAY "LOCATED AT X: " MAX-X " Y: " MAX-Y.


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
           COMPUTE FINAL-VALUE =
                        X-VAR-MINUS-X-SET-SQ
                           + Y-VAR-MINUS-Y-SET-SQ.

       ANNULUS-CHECK-3.
           *> note for future reference, the OR statement makes this kind
           *> of thing much easier than the way I tried it at first
           IF FINAL-VALUE < RADIUS-SQ-1 OR FINAL-VALUE > RADIUS-SQ-2
               MOVE 0000 TO OUT-PIXEL
           END-IF.
