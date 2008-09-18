C
C $Id: mdpsav.f,v 1.10 2008-09-18 12:19:11 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPSAV (IFNO)
C
        INTEGER IFNO
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM3/  ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,
     +                   SLOG,PNTS(200),IDOS(4)
        INTEGER          ITPN,NOUT,NPTS,IGID,IDLS,IDRS,IDOS
        REAL             BLAG,SLAG,BLOG,SLOG,PNTS
        SAVE   /MAPCM3/
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
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /MAPCMC/  IGI1,IGI2,NCRA,NOVS,XCRA(100),YCRA(100)
        INTEGER          IGI1,IGI2,NCRA,NOVS
        REAL             XCRA,YCRA
        SAVE   /MAPCMC/
C
        COMMON /MAPCMQ/  ICIN(8)
        INTEGER          ICIN
        SAVE   /MAPCMQ/
C
        COMMON /MAPCMW/  CSLS,CSLT,SLTD,ISLT
        DOUBLE PRECISION CSLS,CSLT,SLTD
        INTEGER ISLT
        SAVE   /MAPCMW/
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPSAV - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Write a record containing all the user-settable parameters.
C
        WRITE (IFNO,ERR=901)        NOUT,GRDR,GRID,GRLA,GRLO,GRPO,OTOL,
     +                              PLTO,PLNO,PLA1,PLA2,PLA3,PLA4,PLB1,
     +                              PLB2,PLB3,PLB4,PDRE,ROTA,SRCH,XLOW,
     +                              XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                              ILTS,JPRJ,ELPF,LBLF,PRMF,DDTS,DPLT,
     +                              IGI1,IGI2,NOVS,ICIN,ALFA,BETA,DCSA,
     +                              DCSB,DSNA,DSNB,SALT,SSMO,SRSS,ICOL,
     +                              ICSF,NILN,NILT,IDPF,CSLT,CSLS,SLTD,
     +                              ISLT
C
C Done.
C
        RETURN
C
C Error exits.
C
  901   CALL SETER ('MDPSAV - ERROR ON WRITE',2,1)
        RETURN
C
      END
