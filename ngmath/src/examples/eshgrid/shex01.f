
      PROGRAM SHEX01
C
C Test SHGETNP in package Shgrid.
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
C  Number of points in the dataset, number of near points, 
C  number of far points.
C
      PARAMETER(N=1331,NEAREST=500,NFARTHER=N-NEAREST)
      INTEGER NPTS(NEAREST), MPTS(NFARTHER)
C
      REAL X(N), Y(N), Z(N) 
      INTEGER IWK(2*N)
      REAL    RWK(11*N+6)
C
C  Workspace arrays for Tdpack.
C
      PARAMETER (MTRI=150000)
      DIMENSION RTRI(10,MTRI),RTWK(MTRI,2),ITWK(MTRI)
C
C  Generate an array of randomly-positioned points in the unit cube.
C
      DO 3 I=1,N
        X(I) = DSRND1()
        Y(I) = DSRND1()
        Z(I) = DSRND1()
    3 CONTINUE
C
C  Specify the reference point from which we want to find the NEAREST
C  nearest points.
C
      PX = 0.5
      PY = 0.5
      PZ = 0.5
C
C  Plot the points.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Initialize Tdpack parameters.
C
C
C  Move the plot up a bit.
C
      CALL TDSETR('VPB',0.09)
      CALL TDSETR('VPT',0.99)
      CALL TDSETR('VPL',0.11)
      CALL TDSETR('VPR',1.00)
      CALL TDINIT (4.6,  3.0, 3.3, 0.5, 0.5, 0.5, 0.5, 0.5, 2.7,0.)
C
C  Set up some colors using the standard Tdpack entry for that.
C
      CALL TDCLRS (IWKID, 1, 0., 0.8, 8, 37, 8)
C
C  Define style indices for shades of gray, green, and red.
C
      CALL TDSTRS (1,  8, 37,   8,  37, 1, 1, 0, 0.05, 0.05, 0.)
      CALL TDSTRS (3,  8, 37,  68,  97, 1, 1, 0, 0.05, 0.05, 0.)
      CALL TDSTRS (4,  8, 37,  98, 127, 1, 1, 0, 0.05, 0.05, 0.)
C
C  Store the indices of the nearest points in NPTS and the complement
C  of that set (with respect to the entire input dataset) in MPTS.
C
      CALL SHGETNP(PX,PY,PZ,N,X,Y,Z,0,IWK,RWK,NPTS(1),IER)
      DO 80 I=2,N
        IF (I .LE. NEAREST) THEN
          CALL SHGETNP(PX,PY,PZ,N,X,Y,Z,1,IWK,RWK,NPTS(I),IER)
        ELSE
          CALL SHGETNP(PX,PY,PZ,N,X,Y,Z,1,IWK,RWK,
     +                 MPTS(I-NEAREST),IER)
        ENDIF
   80 CONTINUE
C
C  Plot the near points in green.
C
      NTRI = 0
      DOTSIZE = 0.02
      DO 30 I=1,NEAREST
        INX = NPTS(I)
        CALL TDMTRI(-5,X(INX),Y(INX),Z(INX),DOTSIZE,RTRI,MTRI,NTRI,4,
     +                0.,0.,0.,1.,1.,1.)
   30 CONTINUE
C
C  Plot the farther points in gray.
C
      DO 90 I=1,NFARTHER
        INX = MPTS(I)
        CALL TDMTRI(-5,X(INX),Y(INX),Z(INX),DOTSIZE,RTRI,MTRI,NTRI,1,
     +                0.,0.,0.,1.,1.,1.)
   90 CONTINUE
C
C  Mark the reference point in red.
C
      CALL TDMTRI(-5,PX,PY,PZ,1.2*DOTSIZE,RTRI,MTRI,NTRI,3,
     +                0.,0.,0.,1.,1.,1.)
C
C  Draw.
C
      CALL TDOTRI(RTRI,MTRI,NTRI,RTWK,ITWK,0) 
      CALL TDDTRI(RTRI,MTRI,NTRI,ITWK) 
C
C  Draw a box around the perimeter.
C
      CALL TDGRDS (0., 1., 0., 1., 0., 1., -1., -1., -1.,11,0)
      CALL TDGRDS (0., 1., 0., 1., 0., 1., -1., -1., -1.,11,1)
C
C  Label the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.5,0.95,
     +            ':F26:Find the nearest N points in three space',
     +            0.025,0.,0.)
      CALL PLCHHQ(0.05,0.17,
     +            ':F22:Red ball = reference point',0.02,0.,-1.)
      CALL PLCHHQ(0.05,0.12,
     +            ':F22:Green balls = near points',0.02,0.,-1.)
      CALL PLCHHQ(0.05,0.07,
     +            ':F22:Gray balls = far points',0.02,0.,-1.)
C
      CALL FRAME
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
C
      END
      REAL FUNCTION DSRND1()
C
C  Portable random number generator.
C
      PARAMETER (MPLIER=16807,MODLUS=2147483647,MOBYMP=127773,
     +           MOMDMP=2836)
C
      INTEGER HVLUE, LVLUE, TESTV, NEXTN
      SAVE    NEXTN
      DATA JSEED,IFRST/123456789,0/
C
      IF (IFRST .EQ. 0) THEN
        NEXTN = JSEED
        IFRST = 1
      ENDIF
C
      HVLUE = NEXTN / MOBYMP
      LVLUE = MOD(NEXTN, MOBYMP)
      TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
      IF (TESTV .GT. 0) THEN
        NEXTN = TESTV
      ELSE
        NEXTN = TESTV + MODLUS
      ENDIF
      DSRND1 = REAL(NEXTN)/REAL(MODLUS)
C
      RETURN
      END
