C
C	$Id: cpplar.f,v 1.1.1.1 1992-04-17 22:32:48 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CPPLAR (RWRK,IPTX,IPTY,NXYC)
C
      DIMENSION RWRK(*)
C
C (RWRK(I),I=IPTX+1,IPTX+NXYC) and (RWRK(I),I=IPTY+1,IPTY+NXYC) contain
C the X and Y coordinates (in the user coordinate system) of points
C defining a portion of a contour line.  The function of the routine
C CPPLAR is to position one or more labels at regular intervals along
C that portion.
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
      CHARACTER*32 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*40 TXCF
      CHARACTER*20 TXHI
      CHARACTER*100 TXIL
      CHARACTER*20 TXLO
      SAVE   /CPCOM2/
C
C If there are fewer than three points, skip it.
C
      IF (NXYC.LT.3) RETURN
C
C Find the dimensions of the labels being put on this line.  Add a bit
C to keep labels from being too close to each other or to the edge.
C
      XTRA=.5*CHWM*WCLL*(XVPR-XVPL)
C
      DSTB=CLDB(ICLV)+XTRA
      DSTL=CLDL(ICLV)+XTRA
      DSTR=CLDR(ICLV)+XTRA
      DSTT=CLDT(ICLV)+XTRA
C
C Convert all the coordinates from the user system to the fractional
C system.
C
      DO 10001 I=1,NXYC
        RWRK(IPTX+I)=CUFX(RWRK(IPTX+I))
        RWRK(IPTY+I)=CUFY(RWRK(IPTY+I))
10001 CONTINUE
C
C Initialize.  NLBI is the number of labels initially in the list, DATL
C is the distance along the line from the first point to the current
C point, and DANL is the desired distance to the next label.
C
      NLBI=NLBS
      DATL=0.
      DANL=DBLF+2.*(CPRANF()-.5)*DBLV
C
C Examine points along the contour line, putting labels at chosen ones.
C
      DO 10002 I=2,NXYC-1
C
C Wait until we have gone sufficiently far along the line.
C
        DATL=DATL+SQRT((RWRK(IPTX+I)-RWRK(IPTX+I-1))**2+
     +                 (RWRK(IPTY+I)-RWRK(IPTY+I-1))**2)
C
        IF (DATL.GE.DANL) THEN
C
C Consider a possible label centered at the point (XCLB,YCLB).
C
          XCLB=RWRK(IPTX+I)
          YCLB=RWRK(IPTY+I)
C
C Determine at what angle the label would be written and compute the
C coordinates of the left, right, bottom, and top edges of it.
C
          IF (IOLL.EQ.0) THEN
            ANLB=.017453292519943*ANLL
          ELSE
            IF (I.EQ.1) THEN
              ANLB=ATAN2(RWRK(IPTY+2)-YCLB,RWRK(IPTX+2)-XCLB)
            ELSE IF (I.EQ.NXYC) THEN
              ANLB=ATAN2(YCLB-RWRK(IPTY+NXYC-1),
     +                   XCLB-RWRK(IPTX+NXYC-1))
            ELSE
              ANLB=.5*(ATAN2(YCLB-RWRK(IPTY+I-1),XCLB-RWRK(IPTX+I-1))+
     +                 ATAN2(RWRK(IPTY+I+1)-YCLB,RWRK(IPTX+I+1)-XCLB))
            END IF
            IF (ANLB.LT.-1.57079632679490) ANLB=ANLB+3.14159265358979
            IF (ANLB.GT.+1.57079632679490) ANLB=ANLB-3.14159265358979
          END IF
C
          IF (ANLB.EQ.0.) THEN
            XLLB=XCLB-DSTL
            XRLB=XCLB+DSTR
            YBLB=YCLB-DSTB
            YTLB=YCLB+DSTT
          ELSE
            XLBL=XCLB-DSTL*COS(ANLB)+DSTB*SIN(ANLB)
            XRBL=XCLB+DSTR*COS(ANLB)+DSTB*SIN(ANLB)
            XRTL=XCLB+DSTR*COS(ANLB)-DSTT*SIN(ANLB)
            XLTL=XCLB-DSTL*COS(ANLB)-DSTT*SIN(ANLB)
            YLBL=YCLB-DSTL*SIN(ANLB)-DSTB*COS(ANLB)
            YRBL=YCLB+DSTR*SIN(ANLB)-DSTB*COS(ANLB)
            YRTL=YCLB+DSTR*SIN(ANLB)+DSTT*COS(ANLB)
            YLTL=YCLB-DSTL*SIN(ANLB)+DSTT*COS(ANLB)
            XLLB=MIN(XLBL,XRBL,XRTL,XLTL)
            XRLB=MAX(XLBL,XRBL,XRTL,XLTL)
            YBLB=MIN(YLBL,YRBL,YRTL,YLTL)
            YTLB=MAX(YLBL,YRBL,YRTL,YLTL)
          END IF
C
C If the label would extend outside the viewport, forget it.
C
          IF (XLLB.LE.XVPL.OR.XRLB.GE.XVPR.OR.
     +        YBLB.LE.YVPB.OR.YTLB.GE.YVPT) GO TO 101
C
C If the label would overlap a previous label, forget it.
C
            ILBL = 1
            GO TO 10005
10003       CONTINUE
            ILBL =ILBL +1
10005       CONTINUE
            IF (ILBL .GT.(NLBS)) GO TO 10004
C
            IF (ILBL.EQ.INIL) XTRA=.5*CHWM*WCIL*(XVPR-XVPL)
            IF (ILBL.EQ.INHL) XTRA=.5*CHWM*WCHL*(XVPR-XVPL)
            IF (ILBL.EQ.INLL) XTRA=.5*CHWM*WCLL*(XVPR-XVPL)
            XCOL=RWRK(IR03+4*(ILBL-1)+1)
            YCOL=RWRK(IR03+4*(ILBL-1)+2)
            ANOL=RWRK(IR03+4*(ILBL-1)+3)
            SAOL=SIN(ANOL)
            CAOL=COS(ANOL)
            ICOL=INT(RWRK(IR03+4*(ILBL-1)+4))
            IF (ICOL.LE.0) THEN
              ODSL=RWRK(IR04-ICOL+3)+XTRA
              ODSR=RWRK(IR04-ICOL+4)+XTRA
              ODSB=RWRK(IR04-ICOL+5)+XTRA
              ODST=RWRK(IR04-ICOL+6)+XTRA
            ELSE
              ODSL=CLDL(ICOL)+XTRA
              ODSR=CLDR(ICOL)+XTRA
              ODSB=CLDB(ICOL)+XTRA
              ODST=CLDT(ICOL)+XTRA
            END IF
C
            IF (ANOL.EQ.0.) THEN
              XLOL=XCOL-ODSL
              XROL=XCOL+ODSR
              YBOL=YCOL-ODSB
              YTOL=YCOL+ODST
            ELSE
              XLBO=XCOL-ODSL*CAOL+ODSB*SAOL
              XRBO=XCOL+ODSR*CAOL+ODSB*SAOL
              XRTO=XCOL+ODSR*CAOL-ODST*SAOL
              XLTO=XCOL-ODSL*CAOL-ODST*SAOL
              YLBO=YCOL-ODSL*SAOL-ODSB*CAOL
              YRBO=YCOL+ODSR*SAOL-ODSB*CAOL
              YRTO=YCOL+ODSR*SAOL+ODST*CAOL
              YLTO=YCOL-ODSL*SAOL+ODST*CAOL
              XLOL=MIN(XLBO,XRBO,XRTO,XLTO)
              XROL=MAX(XLBO,XRBO,XRTO,XLTO)
              YBOL=MIN(YLBO,YRBO,YRTO,YLTO)
              YTOL=MAX(YLBO,YRBO,YRTO,YLTO)
            END IF
C
            IF (XRLB.GE.XLOL.AND.XLLB.LE.XROL.AND.
     +          YTLB.GE.YBOL.AND.YBLB.LE.YTOL) GO TO 101
C
          GO TO 10003
10004     CONTINUE
C
C No problem.  Go ahead and put a label at this point.
C
          NLBS=NLBS+1
          IF (4*NLBS.GT.LR03) THEN
            IS01=IR01
            CALL CPGRWS (RWRK,3,MAX(4*NLBS,LR03+100),IWSE)
            IPTX=IPTX-IS01+IR01
            IPTY=IPTY-IS01+IR01
            IF (IWSE.NE.0) THEN
              NLBS=NLBS-1
              RETURN
            END IF
          END IF
          RWRK(IR03+4*(NLBS-1)+1)=RWRK(IPTX+I)
          RWRK(IR03+4*(NLBS-1)+2)=RWRK(IPTY+I)
          RWRK(IR03+4*(NLBS-1)+3)=ANLB
          RWRK(IR03+4*(NLBS-1)+4)=REAL(ICLV)
C
C Update the distance along the line to the next label.
C
          DANL=DBLF+REAL(NLBS-NLBI)*DBLN+2.*(CPRANF()-.5)*DBLV
C
        END IF
C
  101 CONTINUE
10002 CONTINUE
C
C Done.
C
      RETURN
C
      END
