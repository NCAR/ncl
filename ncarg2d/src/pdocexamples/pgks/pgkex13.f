
      PROGRAM PGKEX13
C
C  Illustrate character spacings and expansion factors.
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

      DIMENSION XP(2)
      CHARACTER*16 LABEL
      DATA XP(1),XP(2)/.5,2./
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Define colors.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,1.)
      CALL GSCR(IWKID,2,.4,.0,.4)
C
C  Alignment = [center, center]
C
      CALL GSTXAL(2,3)
C
C  Character spacings.
C
      Y = .95
      DO 10 I=0,2,1
      SPACNG = 0.5*REAL(I)
      WRITE(LABEL,100) SPACNG
  100 FORMAT(' Spacing = ',F4.1,' ')
C
C  Attributes for the label for the demo text string.
C
      CALL GSTXFP(-12,2)
      CALL GSCHH(.033)
      CALL GSTXCI(2)
      CALL GSCHSP(0.)
      CALL GTX(.5,Y,LABEL)
      Y = Y-.08
C
C  Attributes for the demo string.
C
      CALL GSCHH(.04)
      CALL GSCHSP(SPACNG)
      CALL GSTXFP(-13,2)
      CALL GSTXCI(1)
      CALL GTX(.5,Y,'NCAR Graphics')
      Y = Y-.12
   10 CONTINUE
C
C  Character expansion factors.
C
      DO 20 I=1,2
      CEXP = XP(I)
      WRITE(LABEL,200) CEXP
  200 FORMAT('Expansion = ',F4.1)
C
C  Attributes for the label for the demo text string.
C
      CALL GSTXFP(-12,2)
      CALL GSCHH(.033)
      CALL GSTXCI(2)
      CALL GSCHSP(0.)
      CALL GSCHXP(1.)
      CALL GTX(.5,Y,LABEL)
      Y = Y-.08
C
C  Attributes for the demo string.
C
      CALL GSCHH(.04)
      CALL GSCHXP(CEXP)
      CALL GSTXFP(-13,2)
      CALL GSTXCI(1)
      CALL GTX(.5,Y,'NCAR Graphics')
      Y = Y-.12
   20 CONTINUE
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
