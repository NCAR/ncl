C
C $Id: cppklp.f,v 1.2 1994-03-17 01:51:36 kennison Exp $
C
      SUBROUTINE CPPKLP (ZDAT,RWRK,IWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C The routine CPPKLP is called to pick the label positions.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
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
      IF (ICFELL('CPPKLP - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPPKLP - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CPPKLP',3).NE.0) RETURN
C
C If the constant-field flag is set, do nothing.
C
      IF (ICFF.NE.0) RETURN
C
C If labels have already been positioned, don't do it again.
C
      IF (NLBS.NE.0) RETURN
C
C Make sure contour labels are completely defined.
C
      CALL CPPKLB (ZDAT,RWRK,IWRK)
      IF (ICFELL('CPPKLP',4).NE.0) RETURN
      CALL CPSTLS (ZDAT,RWRK,IWRK)
      IF (ICFELL('CPPKLP',5).NE.0) RETURN
C
C This routine only does something meaningful if labels are being
C positioned along the contour lines using the regular scheme or the
C penalty scheme.  In other cases, quit now.
C
      IF (ABS(IPLL).NE.2.AND.ABS(IPLL).NE.3) RETURN
C
C Save the index of the informational label.
C
      INIL=NLBS+1
C
C Add the informational label, if any, to the list.
C
      CALL CPINLB (ZDAT,RWRK,IWRK,1,IWRK)
      IF (ICFELL('CPPKLP',6).NE.0) RETURN
C
C Save the index of the high/low labels.
C
      INHL=NLBS+1
C
C Add the high/low labels, if any, to the list.
C
      CALL CPHLLB (ZDAT,RWRK,IWRK,1,IWRK)
      IF (ICFELL('CPPKLP',7).NE.0) RETURN
C
C Save the index of the contour-line labels.
C
      INLL=NLBS+1
C
C If it will be needed, compute the array of gradients.
C
      IF (ABS(IPLL).EQ.3.AND.(WTGR.GT.0..OR.WTNC.GT.0.)) THEN
        RWTH=(XVPR-XVPL)/(YVPT-YVPB)
        IGRM=MAX(10,INT(SQRT(RWTH*REAL(LRWG))))
        IGRN=MAX(10,LRWG/IGRM)
        CALL CPGRWS (RWRK,2,IGRM*IGRN,IWSE)
        IF (IWSE.NE.0.OR.ICFELL('CPPKLP',8).NE.0) RETURN
        CALL CPCPAG (ZDAT,RWRK)
        IF (ICFELL('CPPKLP',9).NE.0) RETURN
      END IF
C
C If the label-positioning flag is positive, force 2D smoothing off
C temporarily.
C
      IF (IPLL.GT.0) THEN
        S2DS=T2DS
        T2DS=0.
      END IF
C
C Trace all the contour lines, positioning labels along each.
C
        ICLW = 1
        GO TO 10003
10001   CONTINUE
        ICLW =ICLW +1
10003   CONTINUE
        IF (ICLW .GT.(NCLV)) GO TO 10002
        IF (CLEV(ICLW).GT.ZMIN.AND.CLEV(ICLW).LT.ZMAX) THEN
          ICLV=ICLP(ICLW)
          IF (MOD(ICLU(ICLV)/2,2).NE.0) THEN
            IJMP=0
10004       CONTINUE
              CALL CPTRCL (ZDAT,RWRK,IWRK,CLEV(ICLV),IJMP,IRW1,IRW2,
     +                                                           NRWK)
              IF (ICFELL('CPPKLP',10).NE.0) RETURN
              IF (IJMP.EQ.0) GO TO 10005
              IF (ABS(IPLL).EQ.2) THEN
                CALL CPPLAR (RWRK,IRW1,IRW2,NRWK)
                IF (ICFELL('CPPKLP',11).NE.0) RETURN
              ELSE
                CALL CPPLPS (RWRK,IRW1,IRW2,NRWK)
                IF (ICFELL('CPPKLP',12).NE.0) RETURN
              END IF
            GO TO 10004
10005       CONTINUE
          END IF
        END IF
      GO TO 10001
10002 CONTINUE
C
C If the label-positioning flag is positive, restore 2D smoothing to
C its original state.
C
      IF (IPLL.GT.0) THEN
        T2DS=S2DS
      END IF
C
C Release the space used for the gradient array, if any.
C
      LR02=0
C
C Done.
C
      RETURN
C
      END
