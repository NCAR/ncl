C
C $Id: mapsav.f,v 1.6 1998-05-23 20:19:54 kennison Exp $
C
      SUBROUTINE MAPSAV (IFNO)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE /MAPCM3/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
C
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
C
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
C
      COMMON /MAPCMQ/ ICIN(7)
      SAVE /MAPCMQ/
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
C
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE /MAPDPS/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPSAV - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Write a record containing all the user-settable parameters.
C
      WRITE (IFNO,ERR=901)        NOUT,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,
     +                            PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,
     +                            PLTR,GRID,IDSH,IDOT,LBLF,PRMF,ELPF,
     +                            XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,
     +                            ILCW,DPLT,DDTS,IGI1,IGI2,NOVS,SALT,
     +                            SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,
     +                            RCSB,DSNA,DCSA,DSNB,DCSB,ICIN,GRLA,
     +                            GRLO,GRPO
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 CALL SETER ('MAPSAV - ERROR ON WRITE',2,1)
      RETURN
C
      END
