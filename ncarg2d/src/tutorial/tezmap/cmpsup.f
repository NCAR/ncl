
      PROGRAM CMPSUP
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

      REAL PLIM1(2),PLIM2(2),PLIM3(2),PLIM4(2)
      DATA PLIM1 /0.0, 0.0/
      DATA PLIM2 /0.0, 0.0/
      DATA PLIM3 /0.0, 0.0/
      DATA PLIM4 /0.0, 0.0/
C
C Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      
      CALL SUPMAP(7,0.,0.,0.,PLIM1,PLIM2,PLIM3,PLIM4,1,5,0,0,IERR)
      CALL FRAME
C
C Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END
