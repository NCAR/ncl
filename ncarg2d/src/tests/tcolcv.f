
      PROGRAM TCOLCV

      CALL COLCV(IERR)
C
      STOP
      END
C
      SUBROUTINE COLCV (IERR)
C
C PURPOSE                To provide a demonstration of the routines in
C                        the package COLCONV and to test them.
C
C USAGE                  CALL COLCV (IERR)
C
C ARGUMENTS
C
C ON OUTPUT              IERR
C                          An integer variable
C                          = 0, if the test is successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C                        COLCONV TEST SUCCESSFUL
C
C                        is written on unit 6.
C
C                        Otherwise, the message
C
C                        COLCONV TEST SUCCESSFUL
C
C                        is written on unit 6.
C
C PRECISION              Single
C
C REQUIRED PACKAGES      COLCONV
C
C REQUIRED GKS LEVEL     NONE
C
C LANGUAGE               FORTRAN
C
C ALGORITHM              TCOLCV executes six calls to test each of
C                        the color conversions:
C
C                              HLS to RGB
C                              RGB to HLS
C                              HSV to RGB
C                              RGB to HSV
C                              YIQ to RGB
C                              RGB to YIQ
C
C ---------------------------------------------------------------------
C
C  Initialize the error flag.
C
      IERR = 0
C
C  Set tolerance limit for HLS and HSV tests.
C
      EPS = 0.00001
C
C  HLS to RGB.
C
      CALL HLSRGB(120.,50.,100.,R,G,B)
      IF ( (ABS(R-1.) .GT. EPS) .OR. (ABS(G) .GT. EPS) .OR.
     *     (ABS(B) .GT. EPS) ) IERR = 1
C
C  RGB to HLS.
C
      CALL RGBHLS(1.,0.,0.,H,FL,S)
      IF ( (ABS(H-120.) .GT. EPS) .OR. (ABS(FL-50.) .GT. EPS) .OR.
     *     (ABS(S-100.) .GT. EPS) ) IERR = 1
C
C  HSV to RGB.
C
      CALL HSVRGB(120.,1.,1.,R,G,B)
      IF ( (ABS(R-0.) .GT. EPS) .OR. (ABS(G-1.) .GT. EPS) .OR.
     *     (ABS(B-0.) .GT. EPS) ) IERR = 1
C
C  RGB to HSV.
C
      CALL RGBHSV(0.,0.,1.,H,S,V)
      IF ( (ABS(H-240.) .GT. EPS) .OR. (ABS(S-1.) .GT. EPS) .OR.
     *     (ABS(V-1.) .GT. EPS) ) IERR = 1
C
C  Set tolerance limit for YIQ tests.
C
      EPS = 0.01
C
C  YIQ to RGB.
C
      CALL YIQRGB(0.58701, -0.27431, -0.52299,R,G,B)
      IF ( (ABS(R-0.) .GT. EPS) .OR. (ABS(G-1.) .GT. EPS) .OR.
     *     (ABS(B-0.) .GT. EPS) ) IERR = 1
C
C  RGB to YIQ.
C
      CALL RGBYIQ(1.,1.,1.,Y,FI,Q)
      IF ( (ABS(Y-1.) .GT. EPS) .OR. (ABS(FI) .GT. EPS) .OR.
     *     (ABS(Q) .GT. EPS) ) IERR = 1
C
      IF (IERR .EQ. 0) THEN
        WRITE (6,1001)
      ELSE
        WRITE (6,1002)
      ENDIF
C
      RETURN
C
 1001 FORMAT (' COLCONV TEST SUCCESSFUL')
 1002 FORMAT (' COLCONV TEST UNSUCCESSFUL')
      END
