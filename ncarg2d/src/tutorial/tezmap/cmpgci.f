
      PROGRAM CMPGCI
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

      PARAMETER(IGRD=2)
      PARAMETER(M=180/IGRD,N=360/IGRD)
      REAL RLAT(100),RLON(100)
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Draw a map
C
      CALL SUPMAP(8,0.,-50.,0.,0.,-80.,90.,10.,2,0.,0,0,IERR)
C
C Get data values defining a great circle between Washinton DC and
C London
C
      CALL MAPGCI(38.,-77.,51.,0.,100,RLAT,RLON)
C
C Draw the great circle
C
      CALL MAPIT(38.,-77.,0)
      DO 10, I=1,100
         CALL MAPIT(RLAT(I),RLON(I),1)
 10   CONTINUE
      CALL MAPIT(51.,0.,1)
      CALL MAPIQ
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END

