
      PROGRAM NNEX10
C
C  Code to illustrate retrieving natural neighbor weights.
C
      PARAMETER(ISLIM = 171, NUMXOUT = 21, NUMYOUT = 21, 
     +          IDIM=2*NUMXOUT*NUMYOUT)
C
      REAL X(ISLIM+3), Y(ISLIM+3), Z(ISLIM+3)
      REAL XI(NUMXOUT),YI(NUMYOUT),ZI(NUMXOUT,NUMYOUT)
C
C  Array to store indices for retrieved neighbors and 
C  associated weights.
C
      INTEGER NBRS(ISLIM)
      REAL    WTS(ISLIM)
C
C  The data values for X and Y (defined in BLOCKDATA) are random numbers 
C  in the range  -0.2 to 1.2.  They are explicitly set here so that they 
C  will be uniform across all compilers.
C
      COMMON /DVALUE/X,Y
C
C  Define some input function values.
C
      DO 5 I=1,ISLIM
        Z(I) = (X(I)-0.25)**2 + (Y(I)-0.50)**2
    5 CONTINUE
C
C  Define an output grid.
C
      XMIN =  -0.2
      XMAX =   1.2
      XINC = (XMAX-XMIN)/(NUMXOUT-1.)
      DO 20 I=1,NUMXOUT
        XI(I) = XMIN+REAL(I-1) * XINC
   20 CONTINUE
C
      YMIN =  -0.2
      YMAX =   1.2
      YINC = (YMAX-YMIN)/(NUMYOUT-1.)
      DO 30 J=1,NUMYOUT
        YI(J) = YMIN+REAL(J-1) * YINC
   30 CONTINUE
C
C  Enter single point mode.
C
      CALL NNPNTINITS(ISLIM,X,Y,Z)
C
C  Calculate the interpolated values at the desired points.
C
      DO 40 I=1,21
        DO 50 J=1,21
          CALL NNPNTS(XI(I),YI(J),ZI(I,J))
C
C  Get the indices for the neighbors and the associated weights.
C
          CALL NNGETWTS(NUMW,NBRS,WTS,X(ISLIM+1), Y(ISLIM+1), 
     +                  Z(ISLIM+1))
C
C  Calculated the interpolated function value at (XI(I),YI(J))
C  using the retrieved neighbors and weights and compare this
C  with the value returned from NNPNTS.
C
          ZP = 0.
          DO 13 L=1,NUMW
            ZP = ZP+WTS(L)*Z(NBRS(L))
   13     CONTINUE
          DIFF = (ZP-ZI(I,J))/ZP
          IF (DIFF .GT. 0.000001) THEN
            WRITE(6,512) I,J,ZP,ZI(I,J)
          ENDIF
  512     FORMAT(2I5,2F15.8)
   50   CONTINUE
   40 CONTINUE
C
C  Test one extrapolation point.
C
      CALL NNPNTS(-0.17,-0.17,ZX)
      CALL NNGETWTS(NUMW,NBRS,WTS,X(ISLIM+1),Y(ISLIM+1),Z(ISLIM+1))
      ZP = 0.
      DO 15 L=1,NUMW
        ZP = ZP+WTS(L)*Z(NBRS(L))
   15 CONTINUE
      DIFF = (ZP-ZX)/ZP
      IF (DIFF .GT. 0.000001) THEN
        WRITE(6,512) I,J,ZP,ZX
      ENDIF
C
C  Exit single point mode.
C
      CALL NNPNTEND()

      STOP
      END
C
      BLOCKDATA DATAV
      COMMON /DVALUE/X,Y
      DIMENSION X(174),Y(174)
C
      DATA (X(L),L=1,171)/
     +    1.16,  0.47,  0.29,  0.72,  0.52,  1.12,  0.33,  0.20,  0.30,
     +    0.78,  0.92,  0.52,  0.44,  0.22, -0.10,  0.11,  0.59,  1.13,
     +    0.68,  1.11,  0.93,  0.29,  0.74,  0.43,  0.87,  0.87, -0.10,
     +    0.26,  0.85,  0.00, -0.02,  1.01, -0.12,  0.65,  0.39,  0.96,
     +    0.39,  0.38,  0.94, -0.03, -0.17,  0.00,  0.03,  0.67, -0.06,
     +    0.82, -0.03,  1.08,  0.37,  1.02, -0.11, -0.13,  1.03,  0.61,
     +    0.26,  0.18,  0.62,  0.42,  1.03,  0.72,  0.97,  0.08,  1.18,
     +    0.00,  0.69,  0.10,  0.80,  0.06,  0.82,  0.20,  0.46,  0.37,
     +    1.16,  0.93,  1.09,  0.96,  1.00,  0.80,  0.01,  0.12,  1.01,
     +    0.48,  0.79,  0.04,  0.42,  0.48, -0.18,  1.16,  0.85,  0.97,
     +    0.14,  0.40,  0.78,  1.12,  1.19,  0.68,  0.65,  0.41,  0.90,
     +    0.84, -0.11, -0.01, -0.02, -0.10,  1.04,  0.58,  0.61,  0.12,
     +   -0.02, -0.03,  0.27,  1.17,  1.02,  0.16, -0.17,  1.03,  0.13,
     +    0.04, -0.03,  0.15,  0.00, -0.01,  0.91,  1.20,  0.54, -0.14,
     +    1.03,  0.93,  0.42,  0.36, -0.10,  0.57,  0.22,  0.74,  1.15,
     +    0.40,  0.82,  0.96,  1.09,  0.42,  1.13,  0.24,  0.51,  0.60,
     +    0.06,  0.38,  0.15,  0.59,  0.76,  1.16,  0.02,  0.86,  1.14,
     +    0.37,  0.38,  0.26,  0.26,  0.07,  0.87,  0.90,  0.83,  0.09,
     +    0.03,  0.56, -0.19,  0.51,  1.07, -0.13,  0.99,  0.84,  0.22/
      DATA (Y(L),L=1,171)/
     +   -0.11,  1.07,  1.11, -0.17,  0.08,  0.09,  0.91,  0.17, -0.02,
     +    0.83,  1.08,  0.87,  0.46,  0.66,  0.50, -0.14,  0.78,  1.08,
     +    0.65,  0.00,  1.03,  0.06,  0.69, -0.16,  0.02,  0.59,  0.19,
     +    0.54,  0.68,  0.95,  0.30,  0.77,  0.94,  0.76,  0.56,  0.12,
     +    0.05, -0.07,  1.01,  0.61,  1.04, -0.07,  0.46,  1.07,  0.87,
     +    0.11,  0.63,  0.06,  0.53,  0.95,  0.78,  0.48,  0.45,  0.77,
     +    0.78,  0.29,  0.38,  0.85, -0.10,  1.17,  0.35,  1.14, -0.04,
     +    0.34, -0.18,  0.78,  0.17,  0.63,  0.88, -0.12,  0.58, -0.12,
     +    1.00,  0.99,  0.45,  0.86, -0.15,  0.97,  0.99,  0.90,  0.42,
     +    0.61,  0.74,  0.41,  0.44,  1.08,  1.06,  1.18,  0.89,  0.74,
     +    0.74, -0.06,  0.00,  0.99,  0.03,  1.00, -0.04,  0.24,  0.65,
     +    0.12,  0.13, -0.09, -0.05,  1.03,  1.07, -0.02,  1.18,  0.19,
     +    0.03, -0.03,  0.86,  1.12,  0.38,  0.72, -0.20, -0.08, -0.18,
     +    0.32,  0.13, -0.19,  0.93,  0.81,  0.31,  1.09, -0.03,  1.01,
     +   -0.17,  0.84, -0.11,  0.45,  0.18,  0.23,  0.81,  0.39,  1.09,
     +   -0.05,  0.58,  0.53,  0.96,  0.43,  0.48,  0.96, -0.03,  1.13,
     +    1.16,  0.16,  1.15,  0.57,  0.13,  0.71,  0.35,  1.04,  0.62,
     +    1.03,  0.98,  0.31,  0.70,  0.97,  0.87,  1.14,  0.08,  1.19,
     +    0.88,  1.00,  0.51,  0.03,  0.17,  1.01,  0.44,  0.17, -0.11/
C
      END
