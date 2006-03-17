
      PROGRAM FSFWRLD
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

      PARAMETER (NPTS=101,LRWK=1000,LIWK=1000)
      INTEGER IWRK(LIWK)
      REAL X1(NPTS),Y1(NPTS),X2(NPTS),Y2(NPTS),X3(NPTS),Y3(NPTS)
      REAL RWRK(LRWK)
C
C Convert from degrees to radians.
C
      DATA D2R / .017453292519943 /
C
C  Demonstrate the use of SFWRLD.
C
C  Generate three intersecting circles of radius 1.
      DO 100 II=1,NPTS
      ANG=D2R*3.6*REAL(II-1)
        X=COS(ANG)
        Y=SIN(ANG)
        X1(II) = X - .5
        X2(II) = X + .5
        X3(II) = X
        Y1(II) = Y + .5
        Y2(II) = Y + .5
        Y3(II) = Y - .5
 100  CONTINUE
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define the entire Viewport and a Window from -2. to 2 with linear scaling.
C
      CALL SET(0.,1.,0.,1.,-2.,2.,-2.,2.,1)
C
C  Process the area definitions (regions) and fill according to instructions
C
      CALL SFSETR ('SP',0.006)
      CALL SFSETR ('AN',0.)
      CALL SFWRLD (X1,Y1,NPTS,RWRK,LRWK,IWRK,LIWK)
      CALL SFSETR ('AN',45.)
      CALL SFWRLD (X2,Y2,NPTS,RWRK,LRWK,IWRK,LIWK)
      CALL SFSETR ('AN',90.)
      CALL SFWRLD (X3,Y3,NPTS,RWRK,LRWK,IWRK,LIWK)
C
C  Do all the rest necesary to display the picture and end the plot
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

