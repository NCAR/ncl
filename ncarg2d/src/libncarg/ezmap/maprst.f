C
C $Id: maprst.f,v 1.3 1994-03-16 23:52:01 kennison Exp $
C
      SUBROUTINE MAPRST (IFNO)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE /MAPCM3/
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
      COMMON /MAPCMQ/ ICIN(7)
      SAVE /MAPCMQ/
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE /MAPDPS/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPRST - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Read a record of saved parameters.
C
      READ (IFNO,ERR=901,END=902) NOUT,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,
     +                            PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,
     +                            PLTR,GRID,IDSH,IDOT,LBLF,PRMF,ELPF,
     +                            XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,
     +                            ILCW,DPLT,DDTS,IGI1,IGI2,NOVS,SALT,
     +                            SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,
     +                            RCSB,DSNA,DCSA,DSNB,DCSB,ICIN
C
C Re-initialize EZMAP.
C
      CALL MAPINT
      IF (ICFELL('MAPRST',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 IIER=20
      CALL SETER ('MAPRST - ERROR ON READ',IIER,1)
      RETURN
C
  902 IIER=21
      CALL SETER ('MAPRST - EOF ON READ',IIER,1)
      RETURN
C
      END
