
      PROGRAM TAGUPW
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
      CALL AGUPW(IERR)
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
      SUBROUTINE AGUPW (IERROR)
C
C PURPOSE                To provide a simple demonstration of the use
C                        of AGUPWRTX with AUTOGRAPH.
C
C USAGE                  CALL AGUPW (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C
C                          an error parameter
C                          = 0, if the test is successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C                          AGUPWRTX TEST SUCCESSFUL  . . .  SEE PLOTS
C                          TO VERIFY PERFORMANCE
C
C                        is written on unit 6.
C
C                        In addition, four (4) labelled frames
C                        containing the two-dimensional plots are
C                        produced on the machine graphics device.
C                        To determine if the test was successful,
C                        it is necessary to examine these plots.
C
C PRECISION              Single.
C
C REQUIRED LIBRARY       AUTOGRAPH, AGUPWRTX, PWRITX, SPPS
C FILES
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN
C
C HISTORY                Adapted from TAUTOG, in June, 1987.
C
C ALGORITHM              TAGUPW computes data for AUTOGRAPH subroutines
C
C                          EZY, EZXY, EZMY, and EZMXY,
C
C                        and calls each of these routines to produce
C                        one plot each.
C
C                        On three of the plots, TAGUPW uses the
C                        AUTOGRAPH control parameter routines
C                        AGSETF, AGSETI, and AGSETP to specify
C                        Y-axis labels or introduce log scaling.
C
C                        Loading AGUPWRTX with AUTOGRAPH caused the
C                        labels to be written by PWRITX.
C
C PORTABILITY            FORTRAN 77
C
C
C
C Declare the data arrays X, Y1D, and Y2D.  X contains abscissae for
C the plots produced by EZXY and EZMXY,  Y1D contains ordinates for
C the plots produced by EZXY and EZY,  and Y2D contains ordinates for
C the plots produced by EZMY and EZMXY.
C
      REAL X(21),Y1D(21),Y2D(21,5)
C
C
C
C Fill the array Y1D for the call to EZY.
C
      DO  10 I=1,21
         Y1D(I) = EXP(-.1*REAL(I))*COS(REAL(I)*.5)
   10 CONTINUE
C
C Plot the contents of Y1D as a function of the integers.
C
      CALL EZY (Y1D(1),21,'DEMONSTRATING EZY ENTRY OF AUTOGRAPH$')
C
C
C
C Fill the arrays X and Y1D for the call to EZXY.
C
      DO  20 I=1,21
         X(I) = REAL(I-1)*.314
         Y1D(I) = X(I)+COS(X(I))*2.0
   20 CONTINUE
C
C Redefine the Y-axis label.
C
      CALL AGSETC('LABEL/NAME.','L')
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETC('LINE/TEXT.','X+COS(X)*2$')
C
C Plot the array Y1D as a function of the array X.
C
      CALL EZXY (X,Y1D,21,'DEMONSTRATING EZXY ENTRY IN AUTOGRAPH$')
C
C
C
C Fill the array Y2D for the call to EZMY.
C
      DO  40 I=1,21
         T = .5*REAL(I-1)
         DO  30 J=1,5
            Y2D(I,J) = EXP(-.5*T)*COS(T)/REAL(J)
   30    CONTINUE
   40 CONTINUE
C
C Redefine the Y-axis label.
C
      CALL AGSETC('LABEL/NAME.','L')
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETC('LINE/TEXT.','EXP(-X/2)*COS(X)*SCALE$')
C
C Specify that the alphabetic set of dashed line patterns is to be used.
C
      CALL AGSETI('DASH/SELECTOR.',-1)
C
C Specify that the graph drawn is to be logarithmic in X.
C
      CALL AGSETI('X/LOGARITHMIC.',1)
C
C Plot the five curves defined by Y2D as functions of the integers.
C
      CALL EZMY (Y2D,21,5,10,'DEMONSTRATING EZMY ENTRY OF AUTOGRAPH$')
C
C
C
C Fill the array Y2D for the call to EZMXY.
C
      DO  60 I=1,21
         DO  50 J=1,5
            Y2D(I,J) = X(I)**J+COS(X(I))
   50    CONTINUE
   60 CONTINUE
C
C Redefine the Y-axis label.
C
      CALL AGSETC('LABEL/NAME.','L')
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETC('LINE/TEXT.','X**J+COS(X)$')
C
C Specify that the graph is to be linear in X and logarithmic in Y.
C
      CALL AGSETI('X/LOGARITHMIC.',0)
      CALL AGSETI('Y/LOGARITHMIC.',1)
C
C Plot the five curves defined by Y2D as functions of X.
C
      CALL EZMXY (X,Y2D,21,5,21,
     +            'DEMONSTRATING EZMXY ENTRY OF AUTOGRAPH$')
C
C
C
C Done.
C
      IERROR = 0
      WRITE (6,1001)
C
      RETURN
C
C
C
 1001 FORMAT ('  AGUPWRTX TEST SUCCESSFUL',24X,
     1        'SEE PLOTS TO VERIFY PERFORMANCE')
C
      END
