C
C $Id: cppkcl.f,v 1.2 1994-03-17 01:51:30 kennison Exp $
C
      SUBROUTINE CPPKCL (ZDAT,RWRK,IWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C The routine CPPKCL is called to pick a set of contour levels.
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
      IF (ICFELL('CPPKCL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPPKCL - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C If contour level selection is suppressed, do nothing.
C
      IF (ICLS.EQ.0) RETURN
C
C If the constant-field flag is set, do nothing.
C
      IF (ICFF.NE.0) RETURN
C
C If the contour-selection flag is negative and is equal to "-n",
C generate "n" contour levels, equally spaced between the minimum and
C the maximum.  By default, none of these levels will be labelled.
C
      IF (ICLS.LT.0) THEN
C
        IF (-ICLS.GT.256) THEN
          CALL SETER ('CPPKCL - TOO MANY CONTOUR LEVELS',3,1)
          RETURN
        END IF
C
        NCLV=MIN(-ICLS,256)
        CINU=(ZMAX-ZMIN)/REAL(NCLV+1)
        LINU=0
C
        DO 10001 I=1,NCLV
          CLEV(I)=ZMIN+REAL(I)*CINU
          ICLU(I)=1
          IAIA(I)=I+1
          IAIB(I)=I
          CLBL(I)=' '
          NCLB(I)=-1
          CLDP(I)='$$$$$$$$$$$$$$$$'
          ICCL(I)=-1
          ICLL(I)=-1
          CLWA(I)=0.
10001   CONTINUE
C
C Otherwise (if the contour-selection flag is positive), generate the
C contour levels at equal intervals, either as specified by the user
C or as chosen in order to get roughly the right number of levels.
C Certain levels will be labelled.
C
      ELSE
C
        IF (CINS.LE.0.) THEN
          CINU=0.
        ELSE
          CINU=CINS
          LINU=LINS
          IF (UCMN.LE.UCMX) THEN
            NCLV=0
10002       CONTINUE
              NCLV=NCLV+1
              CLEV(NCLV)=UCMN+REAL(NCLV-1)*CINU
              IF (ABS(CLEV(NCLV)).LT.1.E-3*CINU) CLEV(NCLV)=0.
              IF (MOD(NCLV-1,LINU).NE.0) THEN
                ICLU(NCLV)=1
              ELSE
                ICLU(NCLV)=3
              END IF
              IAIA(NCLV)=NCLV+1
              IAIB(NCLV)=NCLV
              CLBL(NCLV)=' '
              NCLB(NCLV)=-1
              CLDP(NCLV)='$$$$$$$$$$$$$$$$'
              ICCL(NCLV)=-1
              ICLL(NCLV)=-1
              CLWA(NCLV)=0.
              IF (NCLV.EQ.256.OR.CLEV(NCLV)+.999*CINU.GT.UCMX) GO TO 100
     +03
            GO TO 10002
10003       CONTINUE
            GO TO 101
          END IF
        END IF
C
        IF (CINU.EQ.0.) THEN
          CINU=(ZMAX-ZMIN)/REAL(ICLS)
          LINU=1
          ITMP=INT(10000.+ALOG10(CINU))-10000
          CINU=CINU/10.**ITMP
          IF (CINU.LT.1.) THEN
            ITMP=ITMP-1
            CINU=CINU*10.
          ELSE IF (CINU.GE.10.) THEN
            ITMP=ITMP+1
            CINU=CINU/10.
          END IF
          IINV=0
          DO 10004 I=1,10
            IF (CINT(I).NE.0..AND.CINT(I).LE.CINU) IINV=I
10004     CONTINUE
          IF (IINV.NE.0) THEN
            CINU=CINT(IINV)
            LINU=LINT(IINV)
          END IF
          IF (ITMP.LT.0) THEN
            CINU=CINU*(1./10.**(-ITMP))
          ELSE IF (ITMP.GT.0) THEN
            CINU=CINU*10.**ITMP
          END IF
        END IF
        NCLV=0
        RTM2=ZMIN+.001*(ZMAX-ZMIN)
        IF (RTM2.LT.0.) THEN
          RTM1=-AINT(-RTM2/CINU)
        ELSE
          RTM1=1.+AINT(RTM2/CINU)
        END IF
        RTM2=ZMAX-.001*(ZMAX-ZMIN)
10005   CONTINUE
        IF (.NOT.(NCLV.LT.256.AND.RTM1*CINU.LT.RTM2)) GO TO 10006
          NCLV=NCLV+1
          CLEV(NCLV)=RTM1*CINU
          IF (MOD(RTM1,REAL(LINU)).NE.0) THEN
            ICLU(NCLV)=1
          ELSE
            ICLU(NCLV)=3
          END IF
          IAIA(NCLV)=NCLV+1
          IAIB(NCLV)=NCLV
          CLBL(NCLV)=' '
          NCLB(NCLV)=-1
          CLDP(NCLV)='$$$$$$$$$$$$$$$$'
          ICCL(NCLV)=-1
          ICLL(NCLV)=-1
          CLWA(NCLV)=0.
          RTM1=RTM1+1.
        GO TO 10005
10006   CONTINUE
      END IF
C
C Done.
C
  101 RETURN
C
      END
