C
C $Id: supmap.f,v 1.26 2008-10-22 20:14:55 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SUPMAP (JPRJ,PLAT,PLON,ROTA,PLM1,PLM2,PLM3,PLM4,JLTS,
     +                   JGRD,IOUT,IDOT,IERR)
C
      INTEGER JPRJ,JLTS,JGRD,IOUT,IDOT,IERR
      REAL    PLAT,PLON,ROTA,PLM1(2),PLM2(2),PLM3(2),PLM4(2)
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(19),
     +                   PDCL(19)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
C Declare local variables.
C
        INTEGER          I,INTF,LLTS(6),LPRJ(18)
C
        DATA LPRJ / 2,3,1,4,5,6,16,7,8,9,10,18,11,12,13,14,15,17 /
C
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
        CALL MDSETI ('GR',MOD(ABS(JGRD),1000))
        IF (ICFELL('SUPMAP',2).NE.0) RETURN
C
C Set EZMAP's outline-selection parameter.
C
        IF (ABS(IOUT).EQ.0.OR.ABS(IOUT).EQ.1) THEN
          I=1+2*ABS(IOUT)+(1+SIGN(1,JPRJ))/2
        ELSE
          I=MAX(1,MIN(6,IOUT))
        END IF
C
        IF (I.LE.5) THEN
          CALL MDSETC ('OU',DDCT(I))
          IF (ICFELL('SUPMAP',3).NE.0) RETURN
        END IF
C
C Set EZMAP's perimeter-drawing flag.
C
        CALL MDSETL ('PE',JGRD.GE.0)
        IF (ICFELL('SUPMAP',4).NE.0) RETURN
C
C Set EZMAP's grid-line-labelling flag.
C
        CALL MDSETL ('LA',MOD(ABS(JGRD),1000).NE.0)
        IF (ICFELL('SUPMAP',5).NE.0) RETURN
C
C Set EZMAP's dotted-outline flag.
C
        CALL MDSETI ('DO',MAX(0,MIN(1,IDOT)))
        IF (ICFELL('SUPMAP',6).NE.0) RETURN
C
C Set EZMAP's projection-selection parameters.
C
        I=MAX(1,MIN(18,ABS(JPRJ)))
        CALL MAPROJ (PDCT(LPRJ(I)+1),PLAT,PLON,ROTA)
        IF (ICFELL('SUPMAP',7).NE.0) RETURN
C
C Set EZMAP's rectangular-limits-selection parameters.
C
        I=LLTS(MAX(1,MIN(6,ABS(JLTS))))
        CALL MAPSET (LDCT(I),PLM1,PLM2,PLM3,PLM4)
        IF (ICFELL('SUPMAP',8).NE.0) RETURN
C
C Draw the map.
C
        CALL MDGETI ('IN',INTF)
C
        IF (INTF.NE.0) THEN
          CALL MDPINT
          IF (ICFELL('SUPMAP',9).NE.0) RETURN
        END IF
C
        CALL MDPGRD
        IF (ICFELL('SUPMAP',10).NE.0) RETURN
C
        CALL MDPLBL
        IF (ICFELL('SUPMAP',11).NE.0) RETURN
C
        IF      (IOUT.LT.100) THEN
          CALL MDPLOT
          IF (ICFELL('SUPMAP',12).NE.0) RETURN
        ELSE IF (IOUT.LT.200) THEN
          CALL MDLNDR ('Earth..1',MAX(1,MIN(5,MOD(IOUT,100))))
          IF (ICFELL('SUPMAP',13).NE.0) RETURN
        ELSE IF (IOUT.LT.300) THEN
          CALL MDLNDR ('Earth..2',MAX(1,MIN(5,MOD(IOUT,100))))
          IF (ICFELL('SUPMAP',14).NE.0) RETURN
        ELSE IF (IOUT.LT.400) THEN
          CALL MDLNDR ('Earth..3',MAX(1,MIN(5,MOD(IOUT,100))))
          IF (ICFELL('SUPMAP',15).NE.0) RETURN
        ELSE IF (IOUT.LT.500) THEN
          CALL MDLNDR ('Earth..4',MAX(1,MIN(5,MOD(IOUT,100))))
          IF (ICFELL('SUPMAP',16).NE.0) RETURN
        ELSE
          CALL MDPLOT
          IF (ICFELL('SUPMAP',17).NE.0) RETURN
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
