C
C $Id: mprset.f,v 1.2 1998-04-16 20:21:31 kennison Exp $
C
      SUBROUTINE MPRSET
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE   /MAPCM1/
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE   /MAPDP1/
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
      SAVE   /MAPCM2/
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE   /MAPCM3/
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
      COMMON /MAPCM5/ DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(10),PDCL(10)
      CHARACTER*2     DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
      SAVE   /MAPCM5/
      COMMON /MAPCM6/ ELPM,UMNM,UMXM,VMNM,VMXM,UCNM,VCNM,URNM,VRNM
      LOGICAL ELPM
      SAVE   /MAPCM6/
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
      SAVE   /MAPCM7/
      COMMON /MAPCM8/ P,Q,R
      SAVE   /MAPCM8/
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE   /MAPCMA/
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE   /MAPCMC/
      COMMON /MAPCMP/ NPTB,XPTB(50),YPTB(50)
      SAVE   /MAPCMP/
      COMMON /MAPCMQ/ ICIN(7)
      SAVE   /MAPCMQ/
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE   /MAPSAT/
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE   /MAPDPS/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MPRSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Reset all common variables to default values.
C
      IROD=0
      ITPN=1
      NOUT=1
      INTF=.TRUE.
      JPRJ=7
      PHIA=0.
      PHIO=0.
      ROTA=0.
      ILTS=1
      PLA1=0.
      PLA2=0.
      PLA3=0.
      PLA4=0.
      PLB1=0.
      PLB2=0.
      PLB3=0.
      PLB4=0.
      PLTR=4096.
      GRID=10.
      IDSH=21845
      IDOT=0
      LBLF=.TRUE.
      PRMF=.TRUE.
      ELPF=.FALSE.
      IDTL=0
      XLOW=.05
      XROW=.95
      YBOW=.05
      YTOW=.95
      GRDR=1.
      SRCH=1.
      ILCW=1
      ULOW=0.
      UROW=1.
      DPLT=4.
      DDTS=12.
      DSCA=1.
      IGI1=1
      IGI2=2
      NOVS=1
      NCRA=0
      NPTB=0
      DO 101 I=1,7
        ICIN(I)=-1
  101 CONTINUE
      SALT=0.
      ALFA=0.
      BETA=0.
      RSNA=0.
      RCSA=1.
      RSNB=0.
      RCSB=1.
      DSNA=0.D0
      DCSA=1.D0
      DSNB=0.D0
      DCSB=1.D0
      CALL MAPINT
      RETURN
      END
