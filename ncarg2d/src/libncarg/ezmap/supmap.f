C
C	$Id: supmap.f,v 1.3 1992-09-04 20:38:48 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SUPMAP (JPRJ,PLAT,PLON,ROTA,PLM1,PLM2,PLM3,PLM4,JLTS,
     +                   JGRD,IOUT,IDOT,IERR)
C
      DIMENSION PLM1(2),PLM2(2),PLM3(2),PLM4(2)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM5/ DDCT(5),DDCL(5),LDCT(5),LDCL(5),PDCT(10),PDCL(10)
      CHARACTER*2     DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
      SAVE /MAPCM5/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
C
      DIMENSION LPRJ(10),LLTS(5)
C
      DATA LPRJ / 2,3,1,4,5,6,10,7,8,9 /
      DATA LLTS / 1,2,5,4,3 /
C
C Set EZMAP's grid-spacing parameter.
C
      CALL MAPSTI ('GR',MOD(IABS(JGRD),1000))
C
C Set EZMAP's outline-selection parameter.
C
      IF (IABS(IOUT).EQ.0.OR.IABS(IOUT).EQ.1) THEN
        I=1+2*IABS(IOUT)+(1+ISIGN(1,JPRJ))/2
      ELSE
        I=MAX(1,MIN(5,IOUT))
      END IF
C
      CALL MAPSTC ('OU',DDCT(I))
C
C Set EZMAP's perimeter-drawing flag.
C
      CALL MAPSTL ('PE',JGRD.GE.0)
C
C Set EZMAP's grid-line-labelling flag.
C
      CALL MAPSTL ('LA',MOD(IABS(JGRD),1000).NE.0)
C
C Set EZMAP's dotted-outline flag.
C
      CALL MAPSTI ('DO',MAX(0,MIN(1,IDOT)))
C
C Set EZMAP's projection-selection parameters.
C
      I=MAX(1,MIN(10,IABS(JPRJ)))
      CALL MAPROJ (PDCT(LPRJ(I)),PLAT,PLON,ROTA)
C
C Set EZMAP's rectangular-limits-selection parameters.
C
      I=LLTS(MAX(1,MIN(5,IABS(JLTS))))
      CALL MAPSET (LDCT(I),PLM1,PLM2,PLM3,PLM4)
C
C Draw the map.
C
      CALL MAPDRW
C
C Return the error flag to the user.
C
      IERR=IIER
C
C Done.
C
      RETURN
C
      END
