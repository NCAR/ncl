C
C $Id: cpsps2.f,v 1.5 1995-04-26 22:45:11 kennison Exp $
C
      SUBROUTINE CPSPS2 (XSPS,YSPS,ZSPS,KSPS,MSPS,NSPS,RWRK,KRWK,IWRK,
     +                   KIWK,ZDAT,KZDT)
C
      DIMENSION XSPS(*),YSPS(*),ZSPS(KSPS,*),RWRK(*),IWRK(*),
     +          ZDAT(IZD1,*)
C
C The routine CPSPS2 is called to start the process of drawing a
C contour plot, given a rectangular array of data which is sparse and
C irregularly spaced in X and Y.
C
C XSPS is a one-dimensional array containing the MSPS X coordinates.
C
C YSPS is a one-dimensional array containing the NSPS Y coordinates.
C
C ZSPS is a two-dimensional array containing the data to be contoured.
C
C KSPS is the first dimension of the array ZSPS.
C
C MSPS specifies the number of elements in each row of the array to be
C contoured.
C
C NSPS specifies the number of elements in each column of the array to
C be contoured.
C
C RWRK is a singly-subscripted real work array of length KRWK.
C
C KRWK is the dimension of RWRK.
C
C IWRK is a singly-subscripted integer work array of length KIWK.
C
C KIWK is the dimension of IWRK.
C
C ZDAT is an array in which an interpolated dense array is to be
C generated.
C
C KZDT is the length of the array ZDAT.
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
      IF (ICFELL('CPSPS2 - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If no CONPACK routine has been called before, initialize required
C constants.
C
      IF (INIT.EQ.0) THEN
        CALL CPINRC
        IF (ICFELL('CPSPS2',2).NE.0) RETURN
      END IF
C
C If the user has not provided the dimensions of the dense array,
C compute them; otherwise, check the supplied values for errors.
C
      IF (IZDS.NE.0) THEN
        XDIF=XSPS(MSPS)-XSPS(1)
        YDIF=YSPS(NSPS)-YSPS(1)
        IZDM=INT((1.+EPSI)*SQRT(REAL(KZDT)*(XDIF/YDIF)))
        IZDN=INT((1.+EPSI)*SQRT(REAL(KZDT)*(YDIF/XDIF)))
        IZD1=IZDM
      ELSE
        IF (IZD1.LT.IZDM.OR.IZDM.LT.2.OR.IZDN.LT.2.OR.IZDM*IZDN.GT.KZDT)
     + THEN
          CALL SETER ('CPSPS2 - IZD1, IZDM, OR IZDN SET INCORRECTLY',
     +                                                            3,1)
          RETURN
        END IF
      END IF
C
C Transfer the dimensions of the work arrays to COMMON.
C
      LRWK=KRWK
      LIWK=KIWK
C
C Transfer to a subroutine.  This is necessary so that the change in
C dimensioning of ZDAT should take effect.
C
      CALL CPSP2A (XSPS,YSPS,ZSPS,KSPS,MSPS,NSPS,RWRK,IWRK,ZDAT)
C
C Done.
C
      RETURN
C
      END
