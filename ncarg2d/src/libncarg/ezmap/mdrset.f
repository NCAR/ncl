C
C $Id: mdrset.f,v 1.10 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRSET
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM1/  COSO,COSR,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
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
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(19),
     +                   PDCL(19)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
        COMMON /MAPCM7/  ULOW,UROW,VBOW,VTOW
        DOUBLE PRECISION ULOW,UROW,VBOW,VTOW
        SAVE   /MAPCM7/
C
        COMMON /MAPCM8/  P,Q,R
        DOUBLE PRECISION P,Q,R
        SAVE   /MAPCM8/
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
        COMMON /MAPCMP/  NPTB,XPTB(50),YPTB(50)
        INTEGER          NPTB
        REAL             XPTB,YPTB
        SAVE   /MAPCMP/
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
C Declare local variables.
C
        INTEGER          I
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDRSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Reset all common variables to default values.
C
        IROD=0
        UMAX=1.D0
        UMIN=0.D0
        ITPN=1
        NOUT=1
        GRDR=1.D0
        GRID=10.D0
        GRLA=0.D0
        GRLO=0.D0
        GRPO=90.D0
        PDRE=32768.D0
        PLA1=0.D0
        PLA2=0.D0
        PLA3=0.D0
        PLA4=0.D0
        PLB1=0.D0
        PLB2=0.D0
        PLB3=0.D0
        PLB4=0.D0
        PLNO=0.D0
        PLTO=0.D0
        ROTA=0.D0
        SRCH=1.D0
        XLOW=.05D0
        XROW=.95D0
        YBOW=.05D0
        YTOW=.95D0
        IDOT=0
        IDSH=21845
        IDTL=0
        OTOL=.001D0
        ILCW=1
        ILTS=1
        JPRJ=7
        ELPF=.FALSE.
        INTF=.TRUE.
        LBLF=.TRUE.
        PRMF=.TRUE.
        ULOW=0.D0
        UROW=1.D0
        DDTS=96.D0
        DPLT=1.D0
        DSCA=1.D0
        IGI1=1
        IGI2=2
        NCRA=0
        NOVS=1
        NPTB=0
        DO 101 I=1,8
          ICIN(I)=-1
  101   CONTINUE
        CSLT=1.D0
        CSLS=1.D0
        SLTD=0.D0
        ISLT=0
        DO 102 I=1,5
          ICOL(I)=1
          ICSF(I)=MOD(I-1,2)
  102   CONTINUE
        IDPF=1
        NILN=50
        NILT=50
        ALFA=0.D0
        BETA=0.D0
        DCSA=1.D0
        DCSB=1.D0
        DSNA=0.D0
        DSNB=0.D0
        SALT=0.D0
        CALL MDPINT
        IF (ICFELL('MDRSET',2).NE.0) RETURN
        RETURN
      END
