
      PROGRAM FAGAXCLR
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

      PARAMETER (NPTS=200)
      PARAMETER (NCURVE=4)
      REAL YDRA(NPTS,NCURVE),XDRA(NPTS)

      DO 10 I=1,NPTS
          XDRA(I  )=I*0.1
          DO 10 J=1,NCURVE
              YDRA(I,J)=SIN(XDRA(I)+0.2*J)*EXP(-0.01*XDRA(I)*J**2)
  10  CONTINUE
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)

      CALL DEFCLR (IWKID)

      CALL EZMXY (XDRA,YDRA,NPTS,NCURVE,NPTS,'AXIS COLORS$')
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE DEFCLR (IWKID)
      CALL GSCR(IWKID, 0, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID, 1, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 2, 1.0, 0.0, 0.0)
      CALL GSCR(IWKID, 3, 0.0, 1.0, 0.0)
      CALL GSCR(IWKID, 4, 0.4, 0.7, 0.9)
      CALL GSCR(IWKID, 5, 0.7, 0.4, 0.7)
      CALL GSCR(IWKID, 6, 0.9, 0.7, 0.4)
      CALL GSCR(IWKID, 7, 0.4, 0.9, 0.7)
      RETURN
      END

      SUBROUTINE AGCHAX(IFLG,IAXS,IPRT,VILS)
      CALL PLOTIF (0.,0.,2)
      IF (IFLG .EQ. 0) THEN
        CALL GSPLCI( 2 )
        CALL GSTXCI( 3 )
      ELSE
        CALL GSPLCI(1)
        CALL GSTXCI(1)
      ENDIF
      RETURN
      END
