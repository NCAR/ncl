
      PROGRAM FCIRC
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
C  Demonstrate different GKS fill types
C
      PARAMETER (NPTS=101,MAPSIZ=5000, NGRPS=3, NC=200)
      INTEGER MAP(MAPSIZ), IAREA(NGRPS), IGRP(NGRPS)
      REAL X1(NPTS),Y1(NPTS),X2(NPTS),Y2(NPTS),X3(NPTS),Y3(NPTS)
      REAL XC(NC), YC(NC)

      EXTERNAL FILL
C
C Convert from degrees to radians.
C
      DATA D2R / .017453292519943 /
C
C Generate three intersecting circles of radius 1.
C
      DO 100 I=1,NPTS
         ANG=D2R*3.6*REAL(I-1)
         X=COS(ANG)
         Y=SIN(ANG)
         X1(I) = X - .5
         X2(I) = X + .5
         X3(I) = X
         Y1(I) = Y + .5
         Y2(I) = Y + .5
         Y3(I) = Y - .5
 100  CONTINUE
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Define the entire viewport and
C a window from -2. to 2 with linear scaling.
C
      CALL SET(0.,1.,0.,1.,-2.,2.,-2.,2.,1)
C
C Initialize the area map
C
      CALL ARINAM(MAP,MAPSIZ)
C
C Add the three objects as 3 edge groups
C
      CALL AREDAM(MAP,X1,Y1,NPTS,1,1,0)
      CALL AREDAM(MAP,X2,Y2,NPTS,2,2,0)
      CALL AREDAM(MAP,X3,Y3,NPTS,3,4,0)
C
C Fill the different regions
C
      CALL ARSCAM(MAP,XC,YC,NC,IAREA,IGRP,NGRPS,FILL)
C
C Close the plot
C
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      WRITE (6,*) 'WARNING:'
      WRITE (6,*) 'fcirc.ncgm generates an error message when viewed.'

      STOP
      END

      SUBROUTINE FILL(XC, YC, NC, IAREA, IGRP, NGRPS)
      INTEGER NC,IAREA(NGRPS), IGRP(NGRPS)
      REAL XC(NC), YC(NC)

      ICOLOR=0
      DO 200 I=1,NGRPS
        ICOLOR = ICOLOR + IAREA(I)
200   CONTINUE
C
C  FILL THE REGION WITH THE APPROPRIATE COLOR
C
      IF (ICOLOR .GT. 0) THEN
        CALL GSFAIS(MOD(ICOLOR,4))
        CALL GFA(NC-1,XC,YC)
      ENDIF

      RETURN
      END

