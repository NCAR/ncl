
      PROGRAM CARDB1
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

      PARAMETER (NPTS=101, MAPSIZ=5000, IDSIZE=1, MCS=1000)
      REAL X, Y, ANGLE, D2R
      REAL X1(NPTS), Y1(NPTS), X2(NPTS), Y2(NPTS), X3(11), Y3(11)
      REAL XC(MCS), YC(MCS)
      INTEGER I, MAP(MAPSIZ)
      INTEGER AREAID(IDSIZE), GRPID(IDSIZE)
      
      EXTERNAL FILL
      
      DATA D2R /.017453292519943 /
C
C draw circles of radius .9 and .85 centered on the origin
C
      DO 10 I=1,NPTS
         ANGLE = D2R*3.6*REAL(I-1)
         X = COS(ANGLE)
         Y = SIN(ANGLE)
         X1(I)= 0.90*X
         Y1(I)= 0.90*Y
         X2(I)= 0.85*X
         Y2(I)= 0.85*Y
 10   CONTINUE
C
C get data to draw a pentagram inside inner circle.
C
      NVERT = 6
      CALL STAR(X3,Y3,NVERT,D2R)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Define color table
C
      CALL COLOR(IWKID)
C
C Define window from -1. to 1.
C
      CALL SET(0.,1.,0.,1.,-1.,1.,-1.,1.,1)
      CALL GSCHH (.2)
C 
C Initialize Areas
C
      CALL ARINAM(MAP, MAPSIZ)
      CALL ARSETI('DB - DEBUG PLOTS',1)
C
C Add edges to area map
C
      CALL AREDAM(MAP, X1, Y1, NPTS, 1, 1, 0)
      CALL AREDAM(MAP, X2, Y2, NPTS, 1, 2, 1)
      CALL AREDAM(MAP, X3, Y3, NVERT, 1, 3, 2)
C
C Fill regions according to instructions
C
      CALL ARSCAM(MAP, XC, YC, MCS, AREAID, GRPID, IDSIZE, FILL)
C
C Advance frame
C
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE COLOR(IWKID)
C
C Define color table
C
      CALL GSCR(IWKID,0,0.,0.,0.)
      CALL GSCR(IWKID,1,1.,0.,0.)
      CALL GSCR(IWKID,2,0.,1.,0.)
      CALL GSCR(IWKID,3,1.,1.,0.)
      CALL GSCR(IWKID,4,0.,0.,1.)
      CALL GSCR(IWKID,5,1.,0.,1.)
      CALL GSCR(IWKID,6,0.,1.,1.)
      CALL GSCR(IWKID,7,1.,1.,1.)
      
      RETURN
      END
      
      SUBROUTINE FILL (XC, YC, PTS, AREAID, GRPID, IDSIZE)
C
C Fill area map
C
      INTEGER IDSIZE, PTS
      INTEGER AREAID(IDSIZE), GRPID(IDSIZE)
      REAL XC(PTS), YC(PTS)
C
C In this case, we have only one group, so we know that
C AREAID(IDSIZE) is a unique area identifier.
C
C If the area is the ring between circles solid fill red
C
      IF (AREAID(IDSIZE) .EQ. 1) THEN
         CALL GSFAIS(1)
         CALL GSFACI(1)
         CALL GFA(PTS,XC,YC)
C
C If the area is between the ring and the star, hatch fill in yellow
C
      ELSE IF (AREAID(IDSIZE) .EQ. 2) THEN
         CALL GSFAIS(3)
         CALL GSFASI(6)
         CALL GSFACI(3)
         CALL GFA(PTS,XC,YC)
C
C If the area is inside the star, solid fill in aqua
C
      ELSE IF (AREAID(IDSIZE) .EQ. 3) THEN
         CALL GSFAIS(1)
         CALL GSFACI(6)
         CALL GFA(PTS,XC,YC)
      ENDIF

      RETURN
      END
      
      SUBROUTINE STAR (X3,Y3,NVERT,D2R)
      
      INTEGER NVERT
      REAL X3(NVERT),Y3(NVERT),D2R
      
      DIST = (1.0 - 0.835*COS(D2R*36.))
      
      X3(1) = 0.85*COS(D2R* 18.)
      Y3(1) = 0.85*SIN(D2R* 18.)
      X3(4) = 0.00
      Y3(4) = 0.85
      X3(2) = 0.85*COS(D2R*162.)
      Y3(2) = 0.85*SIN(D2R*162.)
      X3(5) = 0.85*COS(D2R*234.)
      Y3(5) = 0.85*SIN(D2R*234.)
      X3(3) = 0.85*COS(D2R*306.)
      Y3(3) = 0.85*SIN(D2R*306.)
      X3(6) = X3(1)
      Y3(6) = Y3(1)

      RETURN
      END
