      PROGRAM CLASS3
      PARAMETER (NPTS=200)
      REAL YDRA(NPTS),XDRA(NPTS)

      DO 10 I=1,NPTS
         XDRA(I)=I*0.1
            YDRA(I)=EXP(XDRA(I)*SIN(XDRA(I)))
  10  CONTINUE

      CALL OPNGKS

      CALL DEFCLR

      CALL AGSETI ('Y/LOGARITHMIC.',1)
      CALL AGSETF('DASH/SELECTOR.',-1.0)

      CALL AGSETC('LABEL/NAME.','B')
      CALL AGSETI('LINE/NUMBER.',-100)
      CALL AGSETC('LINE/TEXT.','TIME (SECONDS)$')

      CALL AGSETC('LABEL/NAME.','L')
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETC('LINE/TEXT.','POSITION (METERS)$')

      CALL EZXY (XDRA,YDRA,NPTS,
     +	'Log scaling and publication quality text$')

      CALL CLSGKS

      STOP
      END

      SUBROUTINE DEFCLR
      CALL GSCR(1, 0, 0.0, 0.0, 0.0)
      CALL GSCR(1, 1, 1.0, 1.0, 1.0)
      CALL GSCR(1, 2, 1.0, 0.0, 0.0)
      CALL GSCR(1, 3, 0.0, 1.0, 0.0)
      CALL GSCR(1, 4, 0.4, 0.7, 0.9)
      CALL GSCR(1, 5, 0.7, 0.4, 0.7)
      CALL GSCR(1, 6, 0.9, 0.7, 0.4)
      CALL GSCR(1, 7, 0.4, 0.9, 0.7)
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
