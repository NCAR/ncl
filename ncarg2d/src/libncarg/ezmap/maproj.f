C
C $Id: maproj.f,v 1.5 1994-03-16 23:51:57 kennison Exp $
C
      SUBROUTINE MAPROJ (ARG1,ARG2,ARG3,ARG4)
C
      CHARACTER*(*) ARG1
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCM5/ DDCT(5),DDCL(5),LDCT(5),LDCL(5),PDCT(10),PDCL(10)
      CHARACTER*2     DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
      SAVE /MAPCM5/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE /MAPDPS/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPROJ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the parameters defining the projection.
C
      I=IDICTL(ARG1,PDCT,10)
      IF (I.EQ.0) I=IDICTL(ARG1,PDCL,10)
      IF (I.EQ.0) GO TO 901
C
      JPRJ=I
C
      IF (JPRJ.EQ.3) THEN
        CALL MAPSTR ('SA',0.)
        IF (ICFELL('MAPROJ',2).NE.0) RETURN
      ELSE IF (JPRJ.EQ.10) THEN
        JPRJ=3
        IF (ABS(SALT).LE.1.) THEN
          CALL MAPSTR ('SA',6.631)
          IF (ICFELL('MAPROJ',3).NE.0) RETURN
        END IF
      END IF
C
      PHIA=ARG2
      PHIO=ARG3
      ROTA=ARG4
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
  901 IIER=9
      CALL MAPCEM (' MAPROJ - UNKNOWN PROJECTION NAME ',ARG1,IIER,1)
      RETURN
C
      END
