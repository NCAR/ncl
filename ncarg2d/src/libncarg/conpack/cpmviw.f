C
C $Id: cpmviw.f,v 1.5 1995-04-26 22:44:54 kennison Exp $
C
      SUBROUTINE CPMVIW (IWKO,IWKN,LWKN)
C
      DIMENSION IWKO(LIWK),IWKN(LWKN)
C
C This subroutine is called to move what CONPACK has in the integer
C workspace array to a new array.  IWKO is the old array, IWKN the
C new one.  LWKN is the length of the new array.
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
      COMMON /CPCOM1/ NSDR,OORV,PITH,SCFS,SCFU,SEGL,SVAL,T2DS,T3DS
      COMMON /CPCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CPCOM1/ UWDR,UWDT,UXA1,UXAM,UYA1,UYAN,WCCF,WCHL,WCIL
      COMMON /CPCOM1/ WCLL,WLCF,WLHL,WLIL,WLLL,WOCH,WODA,WTCD,WTGR
      COMMON /CPCOM1/ WTNC,WTOD,WWCF,WWHL,WWIL,WWLL,XAT1,XATM,XLBC
      COMMON /CPCOM1/ XVPL,XVPR,XWDL,XWDR,YAT1,YATN,YLBC,YVPB,YVPT
      COMMON /CPCOM1/ YWDB,YWDT,ZDVL,ZMAX,ZMIN
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
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CPCOM2/
C
C Declare local versions of the arrays used to keep track of workspace
C usage.
C
      DIMENSION LCLI(2),LCLL(2)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPMVIW - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C First, zero the local pointers and lengths and, at the same time,
C compute the total space required in the new array.
C
      ITMP=0
C
      DO 10001 I=1,2
        LCLI(I)=0
        LCLL(I)=0
        ITMP=ITMP+LIWS(I)
10001 CONTINUE
C
C If there isn't enough space available in the new array, log an error
C and quit.
C
      IF (ITMP.GT.LWKN) THEN
        CALL SETER ('CPMVIW - NEW WORKSPACE ARRAY IS TOO SMALL',2,1)
        RETURN
      END IF
C
C Zero an index into the new workspace array.
C
      IINW=0
C
C Now, the trick is to move the stuff without stepping on our own toes
C if the user gives us the same array as both the old and the new array.
C We move the blocks closer to the beginning of the array first.
C
10002 CONTINUE
 
        ITM1=0
        ITM2=LIWK
C
        DO 10003 I=1,2
          IF (LIWS(I).NE.0.AND.IIWS(I).LT.ITM2) ITM1=I
10003   CONTINUE
C
        IF (ITM1.NE.0) THEN
          DO 10004 J=1,LIWS(ITM1)
            IWKN(IINW+J)=IWKO(IIWS(ITM1)+J)
10004     CONTINUE
          LCLI(ITM1)=IINW
          LCLL(ITM1)=LIWS(ITM1)
          IIWS(ITM1)=0
          LIWS(ITM1)=0
          IINW=IINW+LCLL(ITM1)
        END IF
C
      IF (.NOT.(ITM1.EQ.0)) GO TO 10002
C
C Now, copy the local set of pointers and lengths to common.
C
      DO 10005 I=1,2
        IIWS(I)=LCLI(I)
        LIWS(I)=LCLL(I)
10005 CONTINUE
C
C Update the variable that says how much integer workspace we have.
C
      LIWK=LWKN
C
C Done.
C
      RETURN
C
      END
