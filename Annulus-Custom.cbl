       IDENTIFICATION DIVISION.
       PROGRAM-ID. Annulus-Custom.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COORDINATE-FILE
               ASSIGN TO
               path to\image_coordinates.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUTPUT-FILE
               ASSIGN TO
               path to\new_coords.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TEMP-FILE
               ASSIGN TO
               path to/temp_coords.dat"
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
           05 OUT-PIXEL        PIC 9(4).

       FD  TEMP-FILE.
       01  TEMP-RECORD.
           05 TEMP-X            PIC 9(4).
           05 TEMP-SPACE-1      PIC X.
           05 TEMP-Y            PIC 9(4).
           05 TEMP-SPACE-2      PIC X.
           05 TEMP-PIXEL        PIC 9(4).

       WORKING-STORAGE SECTION.

       01  WS-OUTPUT-RECORD.
           05 WS-OUT-X          PIC 9(4).
           05 WS-OUT-SPACE-1    PIC X VALUE SPACE.
           05 WS-OUT-Y          PIC 9(4).
           05 WS-OUT-SPACE-2    PIC X VALUE SPACE.
           05 WS-OUT-PIXEL      PIC 9(4).

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
           OPEN OUTPUT TEMP-FILE.

           PERFORM RADIUS-INITIALIZATION.
           PERFORM RADIUS-CALCULATION.
           PERFORM CENTER-POINT-INPUT.
           PERFORM CONVERSION-PROCEDURE.

           CLOSE COORDINATE-FILE.
           CLOSE TEMP-FILE.

           PERFORM APPLY-ANNULUS.

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
           DISPLAY "Inner radius squared: " RADIUS-SQ-1
           DISPLAY "Outer radius squared: " RADIUS-SQ-2.


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


       WRITE-COORDINATES-TO-FILE.
           UNSTRING COORD-RAW-LINE
                   DELIMITED BY ","
                   INTO PIXEL-X
                        PIXEL-Y
                        PIXEL-B
           END-UNSTRING

           MOVE PIXEL-X                      TO TEMP-X
           MOVE SPACE                        TO TEMP-SPACE-1
           MOVE PIXEL-Y                      TO TEMP-Y
           MOVE SPACE                        TO TEMP-SPACE-2
           MOVE PIXEL-B                      TO TEMP-PIXEL

           WRITE TEMP-RECORD.


       APPLY-ANNULUS.
      * Open TEMP as Input to read what you just wrote
           OPEN INPUT TEMP-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           MOVE "N" TO END-OF-FILE.

           PERFORM UNTIL END-OF-FILE = "Y"
               READ TEMP-FILE
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       MOVE TEMP-X TO WS-OUT-X
                       MOVE TEMP-Y TO WS-OUT-Y
                       MOVE TEMP-PIXEL TO WS-OUT-PIXEL
                       PERFORM ANNULUS-CHECK

                       *> This WRITE now goes to the final file
                       MOVE WS-OUT-X TO OUT-X
                       MOVE SPACE TO OUT-SPACE-1
                       MOVE WS-OUT-Y TO OUT-Y
                       MOVE SPACE TO OUT-SPACE-2
                       MOVE WS-OUT-PIXEL TO OUT-PIXEL
                       WRITE OUTPUT-RECORD
               END-READ
           END-PERFORM.

           CLOSE OUTPUT-FILE.
           CLOSE TEMP-FILE.

       ANNULUS-CHECK.
      * Calculate Distance Squared from center
           COMPUTE X-VAR-MINUS-X-SET = WS-OUT-X - MAX-X.
           COMPUTE X-VAR-MINUS-X-SET-SQ
               =   X-VAR-MINUS-X-SET * X-VAR-MINUS-X-SET.

           COMPUTE Y-VAR-MINUS-Y-SET = WS-OUT-Y - MAX-Y.
           COMPUTE Y-VAR-MINUS-Y-SET-SQ
               =   Y-VAR-MINUS-Y-SET * Y-VAR-MINUS-Y-SET.

           COMPUTE FINAL-VALUE =
                        X-VAR-MINUS-X-SET-SQ
                           + Y-VAR-MINUS-Y-SET-SQ.

      * Check if pixel is outside annulus (too close or too far)
           IF FINAL-VALUE < RADIUS-SQ-1 OR FINAL-VALUE > RADIUS-SQ-2
               MOVE 0 TO WS-OUT-PIXEL
           END-IF.

