C
C $Id: cppklb.f,v 1.2 1994-03-17 01:51:33 kennison Exp $
C
      SUBROUTINE CPPKLB (ZDAT,RWRK,IWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C The routine CPPKLB is called to pick the labels to be associated with
C the contour levels.
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
C SCHR is a one-character temporary variable.
C
      CHARACTER*1 SCHR
C
C SCHX is a thirteen-character temporary variable.
C
      CHARACTER*13 SCHX
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPPKLB - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPPKLB - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C If the constant-field flag is set, do nothing.
C
      IF (ICFF.NE.0) RETURN
C
C If no contour levels are defined, try to define them.
C
      IF (NCLV.LE.0) THEN
        CALL CPPKCL (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPPKLB',3).NE.0) RETURN
      END IF
C
C Get indices for the contour levels in ascending order.
C
      IF (NCLV.GT.0) CALL CPSORT (CLEV,NCLV,ICLP)
C
C Find the positions of the leftmost and rightmost digits in the
C character representations of all the contour levels.
C
      MINI=+10000
      MAXI=-10000
      ITMP=0
C
      DO 10001 ICLV=1,NCLV
        IF (MOD(ICLU(ICLV)/2,2).NE.0.AND.CLBL(ICLV).EQ.' ') THEN
          ITMP=ITMP+1
          CALL CPNUMB (CLEV(ICLV)/SCFU,NDGL,-10000,-1,-1,' ',' ',' ',
     +                                0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
          IF (SCHR.NE.'0') THEN
            MINI=MIN(MINI,IEVA-NDGS)
            MAXI=MAX(MAXI,IEVA-1)
          END IF
        END IF
10001 CONTINUE
C
C If no unset contour labels were found, quit.  There are no labels to
C be filled in and no information on which to base the selection of a
C scale factor.  CPPKLB has probably been called needlessly for a second
C time.
C
      IF (ITMP.EQ.0) RETURN
C
C If no meaningful information was found about the position of digits
C in the contour levels (which probably means there was only one
C unspecified label and it should be just a zero), find the position
C of the leftmost digit in the minimum and maximum values and use it.
C
      IF (MINI.GT.MAXI) THEN
        CALL CPNUMB (MAX(ABS(ZMIN),ABS(ZMAX))/SCFU,NDGL,-10000,-1,-1,
     +                    ' ',' ',' ',0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
        MINI=IEVA
        MAXI=IEVA
      END IF
C
C If the leftmost digit in the contour levels is too far to the right
C of the digit to be considered the leftmost significant digit while
C generating labels, increase the number of digits to be used from that
C point rightward.  This may result in recomputing the scale factor and
C other dependent quantities.
C
      IF (MAXI.LT.LSDL-1) THEN
        NDGL=NDGL+LSDL-MAXI
        IF (SCFS.LE.0..AND.SCFS.GE.-3.) THEN
          SCFO=SCFU
          ITMP=0
          IF (SCFS.EQ.0..OR.(SCFS.EQ.-3..AND.LSDM.LT.-1)) ITMP=LSDM+1
          IF (SCFS.EQ.-1.) ITMP=LSDM
          IF (SCFS.EQ.-2..OR.(SCFS.EQ.-3..AND.LSDM-NDGL.GE.0))
     +                                                ITMP=LSDM-NDGL+1
          SCFU=10.**ITMP
          LSDL=LSDM-ITMP
          ITMP=NINT(ALOG10(SCFO/SCFU))
          MINI=MINI+ITMP
          MAXI=MAXI+ITMP
        END IF
      END IF
C
C Determine the number of significant digits to be used for the contour
C labels.
C
      NSDU=MIN(MAX(LSDL,MAXI)-MINI+1,NDGL)
C
C If the scale factor is to be based on contour-level values, compute
C it now.
C
      IF (SCFS.EQ.-4.) THEN
        IF (MINI*(MAXI+1).GT.0) THEN
          SCFU=10.**MINI
          IF (LSDL.NE.-10000) LSDL=LSDL-MINI
        END IF
      END IF
C
C Generate labels for those contour lines which will be labelled.
C
      ISNX=0
      IF (ABS(IPLL).EQ.1.AND.NEXT.EQ.1) THEN
        ISNX=1
        NEXT=0
        SCHX=CHEX
        CHEX=' E '
        LEA1=1
        LEA2=1
        LEA3=1
        LEE1=0
        LEE2=1
        LEE3=0
      END IF
C
      DO 10002 ICLV=1,NCLV
        IF (MOD(ICLU(ICLV)/2,2).NE.0.AND.CLBL(ICLV).EQ.' ') THEN
          CALL CPNUMB (CLEV(ICLV)/SCFU,NSDU,LSDL,NEXU,NEXL,
     +                 CHEX(1:LEA1),CHEX(LEA1+1:LEA1+LEA2),
     +                 CHEX(LEA1+LEA2+1:LEA1+LEA2+LEA3),LEE1,LEE2,
     +                 LEE3,JOMA,JODP,JOTZ,CLBL(ICLV),NCHS,NDGS,
     +                                                     IEVA)
          NCLB(ICLV)=-NCHS
        END IF
10002 CONTINUE
C
      IF (ISNX.NE.0) THEN
        NEXT=1
        CHEX=SCHX
        LEA1=5
        LEA2=5
        LEA3=3
        LEE1=1
        LEE2=2
        LEE3=0
      END IF
C
C Done.
C
      RETURN
C
      END
