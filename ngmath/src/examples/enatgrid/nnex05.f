
      PROGRAM NNEX05
C
C  Code to illustrate the negative value feature.
C
      PARAMETER(ISLIM = 171, NUMXOUT = 27, NUMYOUT = 27, 
     +          IDIM=2*NUMXOUT*NUMYOUT)
C
      REAL X(ISLIM), Y(ISLIM), Z(ISLIM)
      REAL XI(NUMXOUT), YI(NUMYOUT), ZI(NUMXOUT,NUMYOUT)
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
      INTEGER I,J,IWORK(IDIM)
C
C  The data values for X and Y (defined in BLOCKDATA) are random numbers 
C  in the range  -0.2 to 1.2.  They are explicitly set so that they 
C  will be uniform across all compilers.
C
      COMMON /DVALUE/X,Y
C
C  Define the function values.
C
      DO 5 I=1,ISLIM
        Z(I) = (X(I)-0.375)**2 + (Y(I)-0.50)**2 - 0.1
    5 CONTINUE
C
C  Define the output grid.
C
      XMIN =  0.0
      XMAX =  1.0
      XINC = (XMAX-XMIN)/(NUMXOUT-1.) 
      DO 20 I=1,NUMXOUT
        XI(I) = XMIN+REAL(I-1) * XINC
   20 CONTINUE
C
      YMIN =  0.0
      YMAX =  1.0
      YINC = (YMAX-YMIN)/(NUMYOUT-1.)
      DO 30 J=1,NUMYOUT
        YI(J) = YMIN+REAL(J-1) * YINC
   30 CONTINUE 
C
      CALL NNSETI('NON - control of negative values',0)
      CALL NATGRIDS(ISLIM,X,Y,Z,NUMXOUT,NUMYOUT,XI,YI,ZI,IER)
C
C  Plot the interpolated surface.
C
C
C Open GKS and define the foreground and background color.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID, 0, 1.00, 1.00, 1.00)
      CALL GSCR(IWKID, 1, 0.00, 0.00, 0.00)
C
      CALL DRWSRF(NUMXOUT,NUMYOUT,XI,YI,ZI,10.,-25.,50.,IWORK)
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      BLOCKDATA DATAV
      COMMON /DVALUE/X,Y
      DIMENSION X(171),Y(171)
C
      DATA X/
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
      DATA Y/
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
