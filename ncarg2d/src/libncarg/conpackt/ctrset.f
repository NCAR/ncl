C
C $Id: ctrset.f,v 1.2 2003-08-19 21:07:01 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE CTRSET
C
C This subroutine may be called to reset all variables which have
C default values to those values.
C
C Declare all of the CONPACKT common blocks.
C
C
C CTCOM1 contains integer and real variables.
C
      COMMON /CTCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CTCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CTCOM1/ CLDT(256),CLEV(256),CLWA(258),CXCF
      COMMON /CTCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DMAX
      COMMON /CTCOM1/ DMIN,DOPT,DVAL,EPSI,FNCM,GRAV,GRSD,GSDM,HCHL
      COMMON /CTCOM1/ HCHS,HLSR,IAIA(258),IAIB(256),IBCF,IBHL
      COMMON /CTCOM1/ IBIL,IBLL,ICAF,ICCF,ICCL(258),ICFF,ICHI
      COMMON /CTCOM1/ ICHL,ICIL,ICLL(256),ICLO,ICLP(256),ICLS
      COMMON /CTCOM1/ ICLU(258),ICLV,ICLW,IDUF,IGCL,IGLB,IGRM
      COMMON /CTCOM1/ IGRN,IGVS,IHCF,IHLE,IIWS(2),IIWU,ILBC
      COMMON /CTCOM1/ IMPF,INCX(8),INCY(8),INHL,INIL,INIT,INLL
      COMMON /CTCOM1/ IOCF,IOHL,IOLL,IPAI,IPCF,IPIC,IPIE,IPIL,IPLL
      COMMON /CTCOM1/ IRWS(4),IRWU,ISET,IWSO,JODP,JOMA,JOTZ
      COMMON /CTCOM1/ LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3,LINS
      COMMON /CTCOM1/ LINT(10),LINU,LIWK,LIWM,LIWS(2),LNLG,LOEN
      COMMON /CTCOM1/ LOPN,LOTN,LRWC,LRWG,LRWK,LRWM,LRWS(4)
      COMMON /CTCOM1/ LSDD,LSDL,LSDM,LTCF,LTHI,LTIL,LTLO,MIRO
      COMMON /CTCOM1/ NCLB(256),NCLV,NDGL,NEDG,NEXL,NEXT,NEXU
      COMMON /CTCOM1/ NLBS,NLSD,NLZF,NOMF,NOVS,NPNT,NR04,NSDL
      COMMON /CTCOM1/ NSDR,NTRI,OORV,PITH,SCFS,SCFU,SEGL,T2DS
      COMMON /CTCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CTCOM1/ UWDR,UWDT,WCCF,WCHL,WCIL,WCLL,WLCF,WLHL,WLIL
      COMMON /CTCOM1/ WLLL,WOCH,WODA,WTCD,WTGR,WTNC,WTOD,WWCF,WWHL
      COMMON /CTCOM1/ WWIL,WWLL,XLBC,XMAX,XMIN,XVPL,XVPR,XWDL,XWDR
      COMMON /CTCOM1/ YLBC,YMAX,YMIN,YVPB,YVPT,YWDB,YWDT,ZMAX,ZMIN
C
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CTCOM1/
C
C CTCOM2 holds character parameters.
C
      COMMON /CTCOM2/ CHEX,CLBL(256),CLDP(258),CTMA,CTMB,FRMT
      COMMON /CTCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CTCOM2/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTRSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Reset individual parameters.
C
      ANCF=0.
      ANHL=0.
      ANIL=0.
      ANLL=0.
      CDMX=60.
      CHWM=1.
      CINS=0.
      CINU=0.
      CTMA=' '
      CTMB=' '
      CXCF=.50
      CYCF=.50
      CXIL=.98
      CYIL=-.02
      DBLF=.25
      DBLM=.30
      DBLN=.25
      DBLV=.05
      DFLD=.15
      DMAX=0.
      DMIN=0.
      DOPT=.05
      DVAL=0.
      FNCM=5.
      GSDM=1.
      HCHL=.004
      HCHS=.010
      HLSR=.075
      IBCF=0
      IBHL=0
      IBIL=0
      IBLL=0
      ICAF=0
      ICCF=-1
      ICFF=0
      ICHI=-1
      ICHL=-1
      ICIL=-1
      ICLO=-1
      ICLS=16
      IDUF=3
      IGCL=3
      IGLB=3
      IGVS=4
      IHCF=0
      IHLE=0
      IIWU=0
      ILBC=0
      IMPF=0
      INIT=0
      IOHL=3
      IOLL=0
      IPAI=0
      IPCF=0
      IPIC=0
      IPIE=0
      IPIL=4
      IPLL=1
      IRWU=0
      ISET=1
      IWSO=1
      LCTM=1
      LINS=5
      LINU=0
      LIWM=10
      LRWC=1000
      LRWM=100
      LRWG=1000
      LTCF=31
      LTHI=12
      LTIL=36
      LTLO=12
      MIRO=0
      NCLV=0
      NEXL=0
      NEXT=1
      NEXU=5
      NLBS=0
      NLSD=1
      NLZF=0
      NOMF=6
      NOVS=1
      NSDL=4
      OORV=0.
      PITH=0.
      SCFS=1.
      SCFU=1.
      SEGL=.01
      T2DS=0.
      TXCF='CONSTANT FIELD - VALUE IS $DVA$'
      TXHI='H:B:$DVA$:E:'
      TXIL='CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$'
      TXLO='L:B:$DVA$:E:'
      UCMN=1.
      UCMX=0.
      UVPL=.05
      UVPR=.95
      UVPB=.05
      UVPT=.95
      UVPS=.25
      UWDL=0.
      UWDR=0.
      UWDB=0.
      UWDT=0.
      WCCF=.012
      WCHL=.012
      WCIL=.012
      WCLL=.010
      WLCF=0.
      WLHL=0.
      WLIL=0.
      WLLL=0.
      WOCH=.010
      WODA=.005
      WTCD=1.
      WTGR=2.
      WTNC=0.
      WTOD=1.
      WWCF=.005
      WWHL=.005
      WWIL=.005
      WWLL=.005
      XLBC=0.
      XMAX=0.
      XMIN=0.
      YLBC=0.
      YMAX=0.
      YMIN=0.
      ZMAX=0.
      ZMIN=0.
C
C Reset parameter array elements.
C
      CINT(1)=1.
      CINT(2)=2.
      CINT(3)=2.5
      CINT(4)=4.
      CINT(5)=5.
      DO 10001 I=6,10
        CINT(I)=0.
10001 CONTINUE
      DO 10002 I=1,256
        CLBL(I)=' '
        CLEV(I)=0.
        IAIA(I)=0
        IAIB(I)=0
        ICCL(I)=0
        ICLL(I)=-1
10002 CONTINUE
      IAIA(257)=0
      IAIA(258)=-1
      ICCL(257)=-1
      ICCL(258)=-1
      DO 10003 I=1,258
        CLDP(I)='$$$$$$$$$$$$$$$$'
        CLWA(I)=0.
        ICLU(I)=0
10003 CONTINUE
      DO 10004 I=1,2
        IIWS(I)=0
        LIWS(I)=0
10004 CONTINUE
      DO 10005 I=1,4
        IRWS(I)=0
        LRWS(I)=0
10005 CONTINUE
      LINT(1)=5
      LINT(2)=5
      LINT(3)=4
      LINT(4)=5
      LINT(5)=5
      DO 10006 I=6,10
        LINT(I)=0
10006 CONTINUE
C
C Done.
C
      RETURN
C
      END
