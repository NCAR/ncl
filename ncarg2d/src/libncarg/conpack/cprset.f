C
C $Id: cprset.f,v 1.2 1994-03-17 01:51:47 kennison Exp $
C
      SUBROUTINE CPRSET
C
C This subroutine may be called to reset all variables which have
C default values to those values.
C
C Declare all of the CONPACK common blocks.
C
C
C CPCOM1 contains integer and real variables.
C
      COMMON /CPCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CPCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CPCOM1/ CLDT(256),CLEV(256),CLWA(259),CXCF
      COMMON /CPCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DOPT
      COMMON /CPCOM1/ EPSI,FNCM,GRAV,GRSD,GSDM,HCHL,HCHS,IAIA(259)
      COMMON /CPCOM1/ IAIB(256),IBCF,IBHL,IBIL,IBLL,ICAF,ICCF
      COMMON /CPCOM1/ ICCL(259),ICFF,ICHI,ICHL,ICIL,ICLL(256)
      COMMON /CPCOM1/ ICLO,ICLP(256),ICLS,ICLU(259),ICLV,ICLW
      COMMON /CPCOM1/ IDUF,IGCL,IGLB,IGRM,IGRN,IGVS,IHCF,IHLX,IHLY
      COMMON /CPCOM1/ IIWS(2),IIWU,ILBC,IMPF,INCX(8),INCY(8)
      COMMON /CPCOM1/ INHL,INIL,INIT,INLL,IOCF,IOHL,IOLL,IPAI,IPCF
      COMMON /CPCOM1/ IPIC,IPIE,IPIL,IPLL,IRWS(4),IRWU,ISET,IWSO
      COMMON /CPCOM1/ IZD1,IZDM,IZDN,IZDS,JODP,JOMA,JOTZ,LCTM,LEA1
      COMMON /CPCOM1/ LEA2,LEA3,LEE1,LEE2,LEE3,LINS,LINT(10),LINU
      COMMON /CPCOM1/ LIWK,LIWM,LIWS(2),LNLG,LRWC,LRWG,LRWK
      COMMON /CPCOM1/ LRWM,LRWS(4),LSDD,LSDL,LSDM,LTCF,LTHI
      COMMON /CPCOM1/ LTIL,LTLO,MIRO,NCLB(256),NCLV,NDGL,NEXL
      COMMON /CPCOM1/ NEXT,NEXU,NLBS,NLSD,NLZF,NOMF,NOVS,NR04,NSDL
      COMMON /CPCOM1/ NSDR,OORV,SCFS,SCFU,SEGL,SVAL,T2DS,T3DS,UCMN
      COMMON /CPCOM1/ UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL,UWDR
      COMMON /CPCOM1/ UWDT,UXA1,UXAM,UYA1,UYAN,WCCF,WCHL,WCIL,WCLL
      COMMON /CPCOM1/ WLCF,WLHL,WLIL,WLLL,WOCH,WODA,WTCD,WTGR,WTNC
      COMMON /CPCOM1/ WTOD,WWCF,WWHL,WWIL,WWLL,XAT1,XATM,XLBC,XVPL
      COMMON /CPCOM1/ XVPR,XWDL,XWDR,YAT1,YATN,YLBC,YVPB,YVPT,YWDB
      COMMON /CPCOM1/ YWDT,ZDVL,ZMAX,ZMIN
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CPCOM1/
C
C CPCOM2 holds character parameters.
C
      COMMON /CPCOM2/ CHEX,CLBL(256),CLDP(259),CTMA,CTMB,FRMT
      COMMON /CPCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*40 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*40 TXCF
      CHARACTER*20 TXHI
      CHARACTER*100 TXIL
      CHARACTER*20 TXLO
      SAVE   /CPCOM2/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPRSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
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
      DOPT=.05
      FNCM=5.
      GSDM=1.
      HCHL=.004
      HCHS=.010
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
      IHLX=0
      IHLY=0
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
      IZD1=1
      IZDM=1
      IZDN=1
      IZDS=1
      LCTM=1
      LINS=5
      LINU=0
      LIWM=10
      LRWC=100
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
      SCFS=1.
      SCFU=1.
      SEGL=.01
      SVAL=0.
      T2DS=0.
      T3DS=1.
      TXCF='CONSTANT FIELD - VALUE IS $ZDV$'
      TXHI='H:B:$ZDV$:E:'
      TXIL='CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$'
      TXLO='L:B:$ZDV$:E:'
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
      UXA1=0.
      UXAM=0.
      UYA1=0.
      UYAN=0.
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
      YLBC=0.
      ZDVL=0.
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
      IAIA(259)=-1
      ICCL(257)=-1
      ICCL(258)=-1
      ICCL(259)=-1
      DO 10003 I=1,259
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
