      PROGRAM FAGEZY
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
      PARAMETER (NPTS=200)
      REAL YDRA(NPTS)

      DO 10 I=1,NPTS
         YDRA(I)=SIN(I*0.1)*EXP(-0.01*I*0.1*4)
  10  CONTINUE

C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)

      CALL EZY (YDRA,NPTS,'EZY$')
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
