C
C $Id: cplbam.f,v 1.6 1995-12-13 22:52:41 kennison Exp $
C
      SUBROUTINE CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*),IAMA(*)
C
C The function of the routine CPLBAM is to add to an area map boxes
C surrounding all of the contour-line labels.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C IAMA is the user's area-map array.
C
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
C Check for an uncleared prior error.
C
      IF (ICFELL('CPLBAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPLBAM - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CPLBAM',3).NE.0) RETURN
C
C Make sure we have space for 10 coordinate values in real workspace 1.
C
      CALL CPGRWS (RWRK,1,10,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CPLBAM',4).NE.0) GO TO 102
C
C If the constant-field flag is set, put the constant-field message box
C into the area map and quit.
C
      IF (ICFF.NE.0) THEN
        CALL CPCFLB (2,RWRK,IAMA)
        IF (ICFELL('CPLBAM',5).NE.0) RETURN
        GO TO 101
      END IF
C
C If no labels are in the label list and labels are being positioned
C using either the regular scheme or the penalty scheme, force the
C label positions to be picked, which will have the side effect of
C forcing contour levels to be sorted.  Otherwise, just do the sort.
C
      IF (NLBS.LE.0.AND.(ABS(IPLL).EQ.2.OR.ABS(IPLL).EQ.3)) THEN
        CALL CPPKLP (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPLBAM',6).NE.0) THEN
          NLBS=0
          NR04=0
          INIL=0
          INHL=0
          INLL=0
          RETURN
        END IF
      ELSE
        IF (NCLV.GT.0) CALL CPSORT (CLEV,NCLV,ICLP)
      END IF
C
C If there are still no labels in the label list, do the informational
C label and the high/low labels directly and quit.
C
      IF (NLBS.LE.0) THEN
        CALL CPINLB (ZDAT,RWRK,IWRK,3,IAMA)
        IF (ICFELL('CPLBAM',7).NE.0) RETURN
        CALL CPHLLB (ZDAT,RWRK,IWRK,3,IAMA)
        IF (ICFELL('CPLBAM',8).NE.0) RETURN
        GO TO 101
      END IF
C
C Put label boxes in the area map.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XVPL,XVPR,YVPB,YVPT,1)
      IF (ICFELL('CPLBAM',9).NE.0) RETURN
C
      DO 10001 I=1,NLBS
        XCLB=RWRK(IR03+4*(I-1)+1)
        YCLB=RWRK(IR03+4*(I-1)+2)
        ANLB=RWRK(IR03+4*(I-1)+3)
        SALB=SIN(ANLB)
        CALB=COS(ANLB)
        ICLB=INT(RWRK(IR03+4*(I-1)+4))
        IF (ICLB.LE.0) THEN
          DLLB=RWRK(IR04-ICLB+3)
          DRLB=RWRK(IR04-ICLB+4)
          DBLB=RWRK(IR04-ICLB+5)
          DTLB=RWRK(IR04-ICLB+6)
        ELSE
          DLLB=CLDL(ICLB)
          DRLB=CLDR(ICLB)
          DBLB=CLDB(ICLB)
          DTLB=CLDT(ICLB)
        END IF
        RWRK(IR01+ 1)=XCLB-DLLB*CALB+DBLB*SALB
        RWRK(IR01+ 2)=XCLB+DRLB*CALB+DBLB*SALB
        RWRK(IR01+ 3)=XCLB+DRLB*CALB-DTLB*SALB
        RWRK(IR01+ 4)=XCLB-DLLB*CALB-DTLB*SALB
        RWRK(IR01+ 5)=RWRK(IR01+1)
        RWRK(IR01+ 6)=YCLB-DLLB*SALB-DBLB*CALB
        RWRK(IR01+ 7)=YCLB+DRLB*SALB-DBLB*CALB
        RWRK(IR01+ 8)=YCLB+DRLB*SALB+DTLB*CALB
        RWRK(IR01+ 9)=YCLB-DLLB*SALB+DTLB*CALB
        RWRK(IR01+10)=RWRK(IR01+6)
        CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGLB,-1,0)
        IF (ICFELL('CPLBAM',10).NE.0) RETURN
10001 CONTINUE
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CPLBAM',11).NE.0) RETURN
C
C If the area map has no edges in it, something has gone wrong.  Put
C a dummy rectangle in the area map to prevent problems which result.
C
  101 IF (IAMA(7).EQ.0) THEN
        CALL SET (XVPL,XVPR,YVPB,YVPT,XVPL,XVPR,YVPB,YVPT,1)
        IF (ICFELL('CPLBAM',12).NE.0) RETURN
        RWRK(IR01+ 1)=0.
        RWRK(IR01+ 2)=1.
        RWRK(IR01+ 3)=1.
        RWRK(IR01+ 4)=0.
        RWRK(IR01+ 5)=RWRK(IR01+1)
        RWRK(IR01+ 6)=0.
        RWRK(IR01+ 7)=0.
        RWRK(IR01+ 8)=1.
        RWRK(IR01+ 9)=1.
        RWRK(IR01+10)=RWRK(IR01+6)
        CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGLB,0,-1)
        IF (ICFELL('CPLBAM',13).NE.0) RETURN
        CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('CPLBAM',14).NE.0) RETURN
      END IF
C
C Release real workspace 1.
C
  102 LR01=0
C
C Done.
C
      RETURN
C
      END
