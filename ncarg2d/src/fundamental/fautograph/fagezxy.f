      PROGRAM FAGEZXY
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
      PARAMETER (NPTS=200)
      REAL YDRA(NPTS),XDRA(NPTS)

      DO 10 I=1,NPTS
         XDRA(I)=I*0.1
         YDRA(I)=SIN(XDRA(I)+0.2)*EXP(-0.01*XDRA(I)*4)
  10  CONTINUE
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)

      CALL EZXY (XDRA,YDRA,NPTS,'EZXY$')
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
