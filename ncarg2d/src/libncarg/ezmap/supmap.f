C
C $Id: supmap.f,v 1.10 1999-04-02 22:59:49 kennison Exp $
C
      SUBROUTINE SUPMAP (JPRJ,PLAT,PLON,ROTA,PLM1,PLM2,PLM3,PLM4,JLTS,
     +                   JGRD,IOUT,IDOT,IERR)
C
      DIMENSION PLM1(2),PLM2(2),PLM3(2),PLM4(2)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM5/ DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(12),PDCL(12)
      CHARACTER*2     DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
      SAVE   /MAPCM5/
C
      DIMENSION LPRJ(11),LLTS(6)
C
      DATA LPRJ / 2,3,1,4,5,6,11,7,8,9,10 /
      DATA LLTS / 1,2,5,4,3,6 /
C
C Set the error flag to indicate an error; if all goes well, this flag
C will be cleared.
C
      IERR=1
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SUPMAP - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set EZMAP's grid-spacing parameter.
C
      CALL MAPSTI ('GR',MOD(IABS(JGRD),1000))
      IF (ICFELL('SUPMAP',2).NE.0) RETURN
C
C Set EZMAP's outline-selection parameter.
C
      IF (IABS(IOUT).EQ.0.OR.IABS(IOUT).EQ.1) THEN
        I=1+2*IABS(IOUT)+(1+ISIGN(1,JPRJ))/2
      ELSE
        I=MAX(1,MIN(6,IOUT))
      END IF
C
      IF (I.LE.5) THEN
        CALL MAPSTC ('OU',DDCT(I))
        IF (ICFELL('SUPMAP',3).NE.0) RETURN
      END IF
C
C Set EZMAP's perimeter-drawing flag.
C
      CALL MAPSTL ('PE',JGRD.GE.0)
      IF (ICFELL('SUPMAP',4).NE.0) RETURN
C
C Set EZMAP's grid-line-labelling flag.
C
      CALL MAPSTL ('LA',MOD(IABS(JGRD),1000).NE.0)
      IF (ICFELL('SUPMAP',5).NE.0) RETURN
C
C Set EZMAP's dotted-outline flag.
C
      CALL MAPSTI ('DO',MAX(0,MIN(1,IDOT)))
      IF (ICFELL('SUPMAP',6).NE.0) RETURN
C
C Set EZMAP's projection-selection parameters.
C
      I=MAX(1,MIN(11,IABS(JPRJ)))
      CALL MAPROJ (PDCT(LPRJ(I)+1),PLAT,PLON,ROTA)
      IF (ICFELL('SUPMAP',7).NE.0) RETURN
C
C Set EZMAP's rectangular-limits-selection parameters.
C
      I=LLTS(MAX(1,MIN(6,IABS(JLTS))))
      CALL MAPSET (LDCT(I),PLM1,PLM2,PLM3,PLM4)
      IF (ICFELL('SUPMAP',8).NE.0) RETURN
C
C Draw the map.
C
      CALL MPGETI ('IN',INTF)
C
      IF (INTF.NE.0) THEN
        CALL MAPINT
        IF (ICFELL('SUPMAP',9).NE.0) RETURN
      END IF
C
      CALL MAPGRD
      IF (ICFELL('SUPMAP',10).NE.0) RETURN
C
      CALL MAPLBL
      IF (ICFELL('SUPMAP',11).NE.0) RETURN
C
      IF (IOUT.LT.100) THEN
        CALL MAPLOT
        IF (ICFELL('SUPMAP',12).NE.0) RETURN
      ELSE
        CALL MPLNDR ('Earth..1',MOD(IOUT,100))
        IF (ICFELL('SUPMAP',13).NE.0) RETURN
      END IF
C
C All seems to have gone well - turn off the error flag.
C
      IERR=0
C
C Done.
C
      RETURN
C
      END
