
      PROGRAM FCCE02
C
C PURPOSE                To provide a demonstration of the routines in
C                        the package COLCONV and to test them.
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
C REQUIRED PACKAGES      COLCONV
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
      HUE    = 120.
      RLIGHT =  50.
      SATR   = 100.
      CALL HLSRGB(HUE,RLIGHT,SATR,RED,GREEN,BLUE)
      IF ( (ABS(RED-1.) .GT. EPS) .OR. (ABS(GREEN) .GT. EPS) .OR.
     *     (ABS(BLUE) .GT. EPS) ) IERR = 1
C
C  RGB to HLS.
C
      RED   = 1.
      GREEN = 0.
      BLUE  = 0.
      CALL RGBHLS(RED,GREEN,BLUE,HUE,RLIGHT,SATR)
      IF ( (ABS(HUE-120.) .GT. EPS) .OR. (ABS(RLIGHT-50.) .GT. EPS) .OR.
     *     (ABS(SATR-100.) .GT. EPS) ) IERR = 1
C
C  HSV to RGB.
C
      HUE   = 120.
      SATR  = 1.
      VALUE = 1.
      CALL HSVRGB(HUE,SATR,VALUE,RED,GREEN,BLUE)
      IF ( (ABS(RED-0.) .GT. EPS) .OR. (ABS(GREEN-1.) .GT. EPS) .OR.
     *     (ABS(BLUE-0.) .GT. EPS) ) IERR = 1
C
C  RGB to HSV.
C
      RED   = 0.
      GREEN = 0.
      BLUE  = 1.
      CALL RGBHSV(RED,GREEN,BLUE,HUE,SATR,VALUE)
      IF ( (ABS(HUE-240.) .GT. EPS) .OR. (ABS(SATR-1.) .GT. EPS) .OR.
     *     (ABS(VALUE-1.) .GT. EPS) ) IERR = 1
C
C  Set tolerance limit for YIQ tests.
C
      EPS = 0.01
C
C  YIQ to RGB.
C
      Y = 0.59
      RI = -.28
      Q = -.52
      CALL YIQRGB(Y,RI,Q,RED,GREEN,BLUE)
      IF ( (ABS(RED-0.) .GT. EPS) .OR. (ABS(GREEN-1.) .GT. EPS) .OR.
     *     (ABS(BLUE-0.) .GT. EPS) ) IERR = 1
C
C  RGB to YIQ.
C
      RED   = 1.0
      GREEN = 1.0
      BLUE  = 1.0
      CALL RGBYIQ(RED,GREEN,BLUE,Y,RI,Q)
      IF ( (ABS(Y-1.) .GT. EPS) .OR. (ABS(RI) .GT. EPS) .OR.
     *     (ABS(Q) .GT. EPS) ) IERR = 1
C
      IF (IERR .EQ. 0) THEN
        WRITE (6,1001)
      ELSE
        WRITE (6,1002)
      ENDIF
C
      STOP
C
 1001 FORMAT (' COLCONV TEST SUCCESSFUL')
 1002 FORMAT (' COLCONV TEST UNSUCCESSFUL')
      END
