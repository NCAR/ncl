C
C $Id: mprset.f,v 1.7 2000-07-12 16:23:23 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE MPRSET
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE   /MAPCM1/
C
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE   /MAPDP1/
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UCEN,VCEN,URNG,VRNG,BLAM,SLAM,
     +                BLOM,SLOM,ISSL,PEPS
      SAVE   /MAPCM2/
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE   /MAPCM3/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
      COMMON /MAPCM5/ DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(12),PDCL(12)
      CHARACTER*2     DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
      SAVE   /MAPCM5/
C
      COMMON /MAPCM6/ ELPM,UMNM,UMXM,VMNM,VMXM,UCNM,VCNM,URNM,VRNM
      LOGICAL ELPM
      SAVE   /MAPCM6/
C
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
      SAVE   /MAPCM7/
C
      COMMON /MAPCM8/ P,Q,R
      SAVE   /MAPCM8/
C
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE   /MAPCMA/
C
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE   /MAPCMC/
C
      COMMON /MAPCMP/ NPTB,XPTB(50),YPTB(50)
      SAVE   /MAPCMP/
C
      COMMON /MAPCMQ/ ICIN(8)
      SAVE   /MAPCMQ/
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE   /MAPSAT/
C
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
      PLTR=32768.
      GRID=10.
      GRLA=0.
      GRLO=0.
      GRPO=90.
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
      DPLT=1.
      DDTS=96.
      DSCA=1.
      IGI1=1
      IGI2=2
      NOVS=1
      NCRA=0
      NPTB=0
      DO 101 I=1,8
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
      IF (ICFELL('MPRSET',2).NE.0) RETURN
      RETURN
      END
