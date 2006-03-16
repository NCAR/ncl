
      PROGRAM TAUTOG
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL AUTOG(IERR)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE AUTOG (IERROR)
C
C PURPOSE                To provide a simple demonstration of
C                        the AUTOGRPH package.
C
C USAGE                  CALL AUTOG (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C               AUTOGRAPH TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 4
C                        frames are produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plots.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      AUTOGRPH, DASHCHAR
C
C REQUIRED GKS LEVEL     0A
C
C HISTORY                Originally written in April, 1979.
C                        Converted to FORTRAN 77 and GKS in Feb., 1985.
C
C ALGORITHM              TAUTOG computes data which is plotted in single
C                        calls to each of the entries
C
C                          EZY, EZXY, EZMY, AND EZMXY.
C
C                        On 3 of the plots, routines AGSETF, AGSETI,
C                        and AGSETP are called to specify Y-axis labels
C                        or to introduce log scaling.
C
      REAL            X(21)      ,Y1D(21)    ,Y2D(21,5)
C
C X contains the abscissae for the plots produced by EZXY and
C EZMXY,  Y1D contains the ordinate values for the plots produced by
C EZXY and EZY,  and Y2D contains the ordinate values for the plots
C produced by EZMY and EZMXY.
C
C
C     Frame 1 -- EZY entry of AUTOGRAPH.
C
C Fill Y1D array for entry EZY.
C
      DO  10 I=1,21
         Y1D(I) = EXP(-.1*REAL(I))*COS(REAL(I)*.5)
   10 CONTINUE
C
C Entry EZY plots Y1D as a function of a set of continuous integers.
C
C       DEMONSTRATING EZY ENTRY OF AUTOGRAPH
C
      CALL EZY (Y1D(1),21,'DEMONSTRATING EZY ENTRY OF AUTOGRAPH$')
C
C     Frame 2 -- EZXY entry of AUTOGRAPH.
C
C Fill X and Y1D arrays for entry EZXY.
C
      DO  20 I=1,21
         X(I) = REAL(I-1)*.314
         Y1D(I) = X(I)+COS(X(I))*2.0
   20 CONTINUE
C
C Set AUTOGRAPH control parameters for Y-axis label   "X+COS(X)*2"
C
      CALL AGSETC('LABEL/NAME.','L')
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETC('LINE/TEXT.','X+COS(X)*2$')
C
C Entry EZXY plots contents of X-array vs. Y1D-array.
C
C       DEMONSTRATING EZXY ENTRY OF AUTOGRAPH
C
      CALL EZXY (X,Y1D,21,'DEMONSTRATING EZXY ENTRY IN AUTOGRAPH$')
C
C     Frame 3 -- EZMY entry of AUTOGRAPH.
C
C Fill Y2D array for entry EZMY.
C
      DO  40 I=1,21
         T = .5*REAL(I-1)
         DO  30 J=1,5
            Y2D(I,J) = EXP(-.5*T)*COS(T)/REAL(J)
   30    CONTINUE
   40 CONTINUE
C
C Set the AUTOGRAPH control parameters for Y-axis label
C         EXP(-X/2)*COS(X)*SCALE
C
      CALL AGSETC('LABEL/NAME.','L')
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETC('LINE/TEXT.','EXP(-X/2)*COS(X)*SCALE$')
C
C Use the AUTOGRAPH control parameter for integers to specify the
C alphabetic set of dashed line patterns.
C
      CALL AGSETI('DASH/SELECTOR.',-1)
C
C Use the AUTOGRAPH control parameter for integers to specify the
C graph drawn is to be logarithmic in the X-axis.
C
      CALL AGSETI('X/LOGARITHMIC.',1)
C
C Entry EZMY plots multiple arrays as a function of continuous integers.
C
C       DEMONSTRATING EZMY ENTRY OF AUTOGRAPH
C
      CALL EZMY (Y2D,21,5,10,'DEMONSTRATING EZMY ENTRY OF AUTOGRAPH$')
C
C     Frame 4 -- EZMXY entry of AUTOGRAPH.
C
C Fill Y2D array for EZMXY.
C
      DO  60 I=1,21
         DO  50 J=1,5
            Y2D(I,J) = X(I)**J+COS(X(I))
   50    CONTINUE
   60 CONTINUE
C
C Set the AUTOGRAPH control parameters for Y-axis label
C         X**J+COS(X)
C
      CALL AGSETC('LABEL/NAME.','L')
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETC('LINE/TEXT.','X**J+COS(X)$')
C
C Use the AUTOGRAPH control parameter for integers to specify the
C alphabetic set of dashed line patterns.
C
      CALL AGSETI('DASH/SELECTOR.',-1)
C
C Use the AUTOGRAPH control parameter for integers to specify the
C graph have a linear X-axis and a logarithmic Y-axis.
C
      CALL AGSETI('X/LOGARITHMIC.',0)
      CALL AGSETI('Y/LOGARITHMIC.',1)
C
C Entry EZMXY plots multiple Y arrays as a function of a single
C or multiple X arrays.
C
C       DEMONSTRATING EZMXY ENTRY OF AUTOGRAPH
C
      CALL EZMXY (X,Y2D,21,5,21,
     +            'DEMONSTRATING EZMXY ENTRY OF AUTOGRAPH$')
C
C Note that AUTOGRAPH makes its own FRAME advance calls.
C
      IERROR = 0
      WRITE (6,1001)
C
      RETURN
C
 1001 FORMAT (' AUTOGRAPH TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
