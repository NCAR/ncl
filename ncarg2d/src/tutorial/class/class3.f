
      PROGRAM CLASS3
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
      REAL YDRA(NPTS),XDRA(NPTS)

      DO 10 I=1,NPTS
         XDRA(I)=I*0.1
         YDRA(I)=EXP(XDRA(I)*SIN(XDRA(I)))
 10   CONTINUE

C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)

      CALL DEFCLR(IWKID)

      CALL AGSETI ('Y/LOGARITHMIC.',1)
      CALL AGSETF('DASH/SELECTOR.',-1.0)

      CALL AGSETC('LABEL/NAME.','B')
      CALL AGSETI('LINE/NUMBER.',-100)
      CALL AGSETC('LINE/TEXT.','TIME (SECONDS)$')

      CALL AGSETC('LABEL/NAME.','L')
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETC('LINE/TEXT.','POSITION (METERS)$')

      CALL EZXY (XDRA,YDRA,NPTS,
     +     'Log scaling and publication quality text$')

C
C Deactivate and close workstation, close GKS.
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

      SUBROUTINE AGCHCU(IFLG,KDSH)
      CALL PLOTIF (0.,0.,2)
      IF (IFLG .EQ. 0) THEN
         CALL GSPLCI( ABS(KDSH)+3 )
         CALL GSTXCI( ABS(KDSH)+3 )
      ELSE
         CALL GSPLCI(1)
         CALL GSTXCI(1)
      ENDIF
      RETURN
      END

      SUBROUTINE AGCHIL(IFLG,LBNM,LNNO)
      CALL PLOTIF (0.,0.,2)
      IF (IFLG .EQ. 0) THEN
         CALL GSTXCI( 4 )
      ELSE
         CALL GSTXCI( 1 )
      ENDIF
      RETURN
      END

      SUBROUTINE AGPWRT (XPOS, YPOS, CHRS, NCHS, ISIZ, IORI, ICEN)
      CHARACTER*(*) CHRS
      CALL PCGETR ('CS - CONSTANT SPACING FLAG', CSFL)
C If the label centering option is on, give wider spacing.
      IF (ICEN.NE.0) THEN
        CALL PCSETR ('CS - CONSTANT SPACING FLAG', 1.25)
      ELSE
        CALL PCSETR ('CS - CONSTANT SPACING FLAG', 0.0 )
      ENDIF
C Set the size of the labels to be the same as Autograph
C would normally use.
      CALL PLCHHQ (XPOS, YPOS, CHRS(1:NCHS),
     +             .8*REAL(ISIZ), REAL(IORI), REAL(ICEN))
C Return spacing to whatever it was before we wrote this label
      CALL PCSETR ('CS - CONSTANT SPACING FLAG', CSFL)
C                                                                       
      RETURN                                                            
      END                                                               
