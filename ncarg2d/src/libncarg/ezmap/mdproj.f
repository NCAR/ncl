C
C $Id: mdproj.f,v 1.12 2008-09-18 12:19:11 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPROJ (ARG1,ARG2,ARG3,ARG4)
C
        CHARACTER*(*)    ARG1
        DOUBLE PRECISION ARG2,ARG3,ARG4
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(19),
     +                   PDCL(19)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
        COMMON /MAPCMW/  CSLS,CSLT,SLTD,ISLT
        DOUBLE PRECISION CSLS,CSLT,SLTD
        INTEGER ISLT
        SAVE  /MAPCMW/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Declare local variables.
C
        INTEGER          I
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPROJ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the parameters defining the projection.
C
        I=IDICTL(ARG1,PDCT,19)
        IF (I.EQ.0) I=IDICTL(ARG1,PDCL,19)
        IF (I.EQ.0) GO TO 901
C
        JPRJ=I-1
C
        PLTO=MAX(-90.D0,MIN(90.D0,ARG2))
        PLNO=ARG3+(SIGN(180.D0,180.D0-ARG3)-SIGN(180.D0,ARG3+180.D0))
C
C Fix for NCL-2558 -- dbrown -- 2/14/2016
C If Lambert Conformal then ARG4 is a latitude so treat it like ARG2
C
        IF (JPRJ .EQ. 1) THEN
          ROTA=MAX(-90.D0,MIN(90.D0,ARG4))
        ELSE
          ROTA=ARG4+(SIGN(180.D0,180.D0-ARG4)-SIGN(180.D0,ARG4+180.D0))
        END IF
C
        IF (JPRJ.EQ.3) THEN
          CALL MDSETD ('SA',0.D0)
          IF (ICFELL('MDPROJ',2).NE.0) RETURN
        ELSE IF (JPRJ.EQ.7) THEN
          ISLT=0
        ELSE IF (JPRJ.EQ.16) THEN
          JPRJ=3
          IF (ABS(SALT).LE.1.D0) THEN
            CALL MDSETD ('SA',6.631D0)
            IF (ICFELL('MDPROJ',3).NE.0) RETURN
          END IF
        ELSE IF (JPRJ.EQ.17) THEN
          JPRJ=7
          ISLT=1
        ELSE IF (JPRJ.EQ.18) THEN
          JPRJ=25
          PLTO=0.D0
        END IF
C
C Set the flag to indicate that initialization is now required.
C
        INTF=.TRUE.
C
C Done.
C
        RETURN
C
C Error exit.
C
  901   CALL MDPCEM ('MDPROJ - UNKNOWN PROJECTION NAME ',ARG1,4,1)
        RETURN
C
      END
