C
C $Id: cptrev.f,v 1.8 1995-06-02 00:24:05 kennison Exp $
C
      SUBROUTINE CPTREV (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C This routine traces the edge of the area which is visible under the
C current mapping, using only forward mapping capabilities of CPMPXY
C (which doesn't work all that well for many mappings of interest).
C
C As pieces of the edge are generated, control is passed back to the
C caller for processing of them.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C IJMP is initially set to zero by the caller.  Upon return, it will be
C zero if all segments have been traced and processed, non-zero if the
C caller is expected to process a segment and recall CPTREV.
C
C IAIC is both an input and an output variable.  If it is initially set
C to -9 by the caller, it will not be changed by CPTREV and no attempt
C will be made to determine what area identifier should be used for the
C area on the contoured side of the edge of the visible area.  If its
C initial value is 0, it will have been updated, upon every return
C with IJMP non-zero, to the area identifier for the contoured side of
C the piece of the edge defined by IRW1, IRW2, and NRWK.
C
C IRW1 and IRW2 are output variables.  If IJMP is non-zero, they are
C base indices of X and Y coordinate arrays in RWRK.
C
C NRWK is an output variable.  If IJMP is non-zero, NRWK is the number
C of coordinates to be processed by the caller.
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
C
C Define arrays to hold coordinates used in finding the slope of the
C tangent to a limb line at a given point.
C
      DIMENSION XCUV(3),YCUV(3)
C
C Define arrays to hold the coordinates of eight points around a unit
C circle, spaced 45 degrees apart and starting at the 45-degree mark.
C
      DIMENSION XOUC(8),YOUC(8)
C
C Because of the way this routine is entered and re-entered, we need to
C save every variable it uses.
C
      SAVE
C
C Define the unit-circle data.
C
      DATA XOUC / .70711,0.,-.70711,-1.,-.70711, 0., .70711,1. /
      DATA YOUC / .70711,1., .70711, 0.,-.70711,-1.,-.70711,0. /
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (103,104,105,107) , IJMP
C
C Save the initial value of IAIC.
C
      IAID=IAIC
C
C Assign space to use for storing the X and Y coordinates of points.
C
      MPLS=LRWC
      CALL CPGRWS (RWRK,1,2*MPLS,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CPTREV',1).NE.0) GO TO 102
C
C Compute required constants.
C
      IIDM=(IZDM-1)*(ABS(IPIE)+1)+1
      IIDN=(IZDN-1)*(ABS(IPIE)+1)+1
C
      RIDM=(XATM-XAT1)/REAL(IIDM-1)
      RIDN=(YATN-YAT1)/REAL(IIDN-1)
C
      DELX=.0001*ABS(XATM-XAT1)
      DELY=.0001*ABS(YATN-YAT1)
C
C Zero the count of horizontal segments seen.
C
      NHSS=0
C
C Define the first search point.
C
      IVBX=1
      IVBY=1
      CALL HLUCPMPXY (IMPF,XAT1,YAT1,XPRN,YPRN)
      IF (ICFELL('CPTREV',2).NE.0) GO TO 102
C
C Search.
C
10001 CONTINUE
      IF (.NOT.(IVBX.LT.IIDM)) GO TO 10002
        IVBX=IVBX+1
        XPRP=XPRN
        CALL HLUCPMPXY (IMPF,XAT1+RIDM*REAL(IVBX-1),YAT1,XPRN,YPRN)
        IF (ICFELL('CPTREV',3).NE.0) GO TO 102
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10003
          INCI=1
          L10005=    1
          GO TO 10005
10004     CONTINUE
10003   CONTINUE
      GO TO 10001
10002 CONTINUE
C
10006 CONTINUE
      IF (.NOT.(IVBY.LT.IIDN)) GO TO 10007
        IVBY=IVBY+1
        XPRP=XPRN
        CALL HLUCPMPXY (IMPF,XAT1+RIDM*REAL(IIDM-1),
     +                       YAT1+RIDN*REAL(IVBY-1),
     +                                    XPRN,YPRN)
        IF (ICFELL('CPTREV',4).NE.0) GO TO 102
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10008
          INCI=7
          L10005=    2
          GO TO 10005
10009     CONTINUE
10008   CONTINUE
      GO TO 10006
10007 CONTINUE
C
10010 CONTINUE
      IF (.NOT.(IVBX.GT.1)) GO TO 10011
        IVBX=IVBX-1
        XPRP=XPRN
        CALL HLUCPMPXY (IMPF,XAT1+RIDM*REAL(IVBX-1),
     +                       YAT1+RIDN*REAL(IIDN-1),
     +                                    XPRN,YPRN)
        IF (ICFELL('CPTREV',5).NE.0) GO TO 102
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10012
          INCI=5
          L10005=    3
          GO TO 10005
10013     CONTINUE
10012   CONTINUE
      GO TO 10010
10011 CONTINUE
C
10014 CONTINUE
      IF (.NOT.(IVBY.GT.1)) GO TO 10015
        IVBY=IVBY-1
        XPRP=XPRN
        CALL HLUCPMPXY (IMPF,XAT1,YAT1+RIDN*REAL(IVBY-1),XPRN,YPRN)
        IF (ICFELL('CPTREV',6).NE.0) GO TO 102
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10016
          INCI=3
          L10005=    4
          GO TO 10005
10017     CONTINUE
10016   CONTINUE
      GO TO 10014
10015 CONTINUE
C
        IVBY = 2
        GO TO 10020
10018   CONTINUE
        IVBY =IVBY +1
10020   CONTINUE
        IF (IVBY .GT.(IIDN-1)) GO TO 10019
        RVBY=YAT1+RIDN*REAL(IVBY-1)
        CALL HLUCPMPXY (IMPF,XAT1,RVBY,XPRN,YPRN)
        IF (ICFELL('CPTREV',7).NE.0) GO TO 102
          IVBX = 2
          GO TO 10023
10021     CONTINUE
          IVBX =IVBX +1
10023     CONTINUE
          IF (IVBX .GT.(IIDM)) GO TO 10022
          XPRP=XPRN
          CALL HLUCPMPXY (IMPF,XAT1+RIDM*REAL(IVBX-1),RVBY,XPRN,YPRN)
          IF (ICFELL('CPTREV',8).NE.0) GO TO 102
          IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10024
            IPXY=IIDN*IVBX+IVBY
            DO 10025 I=1,NHSS
              IF (IPXY.EQ.IWRK(II01+I)) GO TO 101
10025       CONTINUE
            IF (.NOT.(NHSS.GE.LI01)) GO TO 10026
              CALL CPGIWS (IWRK,1,LI01+100,IWSE)
              IF (IWSE.NE.0.OR.ICFELL('CPTREV',9).NE.0) GO TO 102
10026       CONTINUE
            NHSS=NHSS+1
            IWRK(II01+NHSS)=IPXY
            INCI=1
            L10005=    5
            GO TO 10005
10027       CONTINUE
  101     CONTINUE
10024     CONTINUE
        GO TO 10021
10022   CONTINUE
      GO TO 10018
10019 CONTINUE
C
C Release the workspaces and let the user know we're done.
C
  102 LI01=0
      LR01=0
      IJMP=0
C
C Done.
C
      RETURN
C
C Limb-following algorithm.  This internal routine moves the limb-
C following vector (defined by the base point (IVBX,IVBY) and the
C components INCX(INCI) and INCY(INCI)) along a limb line.  The
C points defining the limb line are thereby determined.  The process
C stops when either the starting point or the edge of the grid is
C encountered.
C
10005 CONTINUE
C
        NPLS=0
C
        MVBX=IVBX
        MVBY=IVBY
        MNCI=INCI
C
        IVEX=IVBX+INCX(INCI)
        IVEY=IVBY+INCY(INCI)
C
        L10029=    1
        GO TO 10029
10028   CONTINUE
C
10030   CONTINUE
C
          INCI=INCI+1
          IF (INCI.GT.8) INCI=INCI-8
          IVEX=IVBX+INCX(INCI)
          IVEY=IVBY+INCY(INCI)
C
          IF (IVEX.LT.1.OR.IVEX.GT.IIDM.OR.IVEY.LT.1.OR.IVEY.GT.IIDN)
     +    GO TO 10031
C
          CALL HLUCPMPXY (IMPF,XAT1+RIDM*REAL(IVEX-1),
     +                         YAT1+RIDN*REAL(IVEY-1),
     +                                      XTMP,YTMP)
          IF (ICFELL('CPTREV',10).NE.0) GO TO 102
          IF (.NOT.(XTMP.NE.OORV)) GO TO 10032
C
            IVBX=IVEX
            IVBY=IVEY
            INCI=INCI+4
C
          GO TO 10033
10032     CONTINUE
          IF (.NOT.((INCI/2)*2.NE.INCI)) GO TO 10034
C
            L10029=    2
            GO TO 10029
10035       CONTINUE
C
            IF (.NOT.(INCI.EQ.1)) GO TO 10036
              IF (.NOT.(NHSS.GE.LI01)) GO TO 10037
                CALL CPGIWS (IWRK,1,LI01+100,IWSE)
                IF (IWSE.NE.0.OR.ICFELL('CPTREV',11).NE.0) GO TO 102
10037         CONTINUE
              NHSS=NHSS+1
              IWRK(II01+NHSS)=IIDN*IVBX+IVBY
10036       CONTINUE
C
            IF (IVBX.EQ.MVBX.AND.IVBY.EQ.MVBY.AND.INCI.EQ.MNCI) GO TO 10
     +031
C
10033     CONTINUE
10034     CONTINUE
C
        GO TO 10030
10031   CONTINUE
C
C Note: At this point, if NPLS is 1, and the call was from CPCLAM,
C control need not return there, because CPTROE is not going to be
C called.  (This is different from the situation in CPTRVE.)
C
        IF (.NOT.(NPLS.GT.1)) GO TO 10038
          IJMP=1
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
10038   CONTINUE
C
  103   IVBX=MVBX
        IVBY=MVBY
C
      GO TO (10004,10009,10013,10017,10027) , L10005
C
C The following procedure, given a point on either side of the limb,
C uses a binary-halving technique to determine a point on the limb and
C adds that point to the list.  It also estimates the angle of the
C tangent to the limb; if the angles of the last two tangents indicate
C that the limb is convex as viewed from the visible side, it adds the
C point of intersection of the two tangents to the list before adding
C the new point.
C
10029 CONTINUE
C
        XCVD=XAT1+RIDM*REAL(IVBX-1)
        YCVD=YAT1+RIDN*REAL(IVBY-1)
        CALL HLUCPMPXY (IMPF,XCVD,YCVD,XCVU,YCVU)
        IF (ICFELL('CPTREV',12).NE.0) GO TO 102
C
        XCID=XAT1+RIDM*REAL(IVEX-1)
        YCID=YAT1+RIDN*REAL(IVEY-1)
C
        ITMP=0
C
10039   CONTINUE
          XCHD=(XCVD+XCID)/2.
          YCHD=(YCVD+YCID)/2.
          CALL HLUCPMPXY (IMPF,XCHD,YCHD,XCHU,YCHU)
          IF (ICFELL('CPTREV',13).NE.0) GO TO 102
          IF (.NOT.(XCHU.NE.OORV)) GO TO 10040
            IF (XCHD.EQ.XCVD.AND.YCHD.EQ.YCVD) GO TO 10041
            XCVD=XCHD
            YCVD=YCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10042
10040     CONTINUE
            IF (XCHD.EQ.XCID.AND.YCHD.EQ.YCID) GO TO 10041
            XCID=XCHD
            YCID=YCHD
10042     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10041
        GO TO 10039
10041   CONTINUE
C
        IF (.NOT.(IAID.NE.-9)) GO TO 10043
          XTMP=1.+((XCVD-XAT1)/(XATM-XAT1))*REAL(IZDM-1)
          YTMP=1.+((YCVD-YAT1)/(YATN-YAT1))*REAL(IZDN-1)
          ITMP=INT(XTMP)
          JTMP=INT(YTMP)
          ITP1=MIN(ITMP+1,IZDM)
          JTP1=MIN(JTMP+1,IZDN)
          IF (.NOT.(SVAL.NE.0..AND.(ZDAT(ITMP,JTMP).EQ.SVAL.OR.ZDAT(ITMP
     +,JTP1).EQ.SVAL.OR.ZDAT(ITP1,JTMP).EQ.SVAL.OR.ZDAT(ITP1,JTP1).EQ.SV
     +AL))) GO TO 10044
            IAID=IAIA(258)
          GO TO 10045
10044     CONTINUE
            IF (.NOT.(NCLV.LE.0)) GO TO 10046
              IAID=1
            GO TO 10047
10046       CONTINUE
              XDEL=XTMP-REAL(ITMP)
              YDEL=YTMP-REAL(JTMP)
              ZINT=(1.-YDEL)*
     +             ((1.-XDEL)*ZDAT(ITMP,JTMP)+XDEL*ZDAT(ITP1,JTMP))+
     +             YDEL*
     +             ((1.-XDEL)*ZDAT(ITMP,JTP1)+XDEL*ZDAT(ITP1,JTP1))
              CALL CPGVAI (ZINT,IAID)
10047       CONTINUE
10045     CONTINUE
10043   CONTINUE
C
        IF (.NOT.(IAID.NE.IAIC)) GO TO 10048
          IF (.NOT.(NPLS.GT.1)) GO TO 10049
            XSAV=RWRK(IR01     +NPLS)
            YSAV=RWRK(IR01+MPLS+NPLS)
            IJMP=2
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
  104       RWRK(IR01     +1)=XSAV
            RWRK(IR01+MPLS+1)=YSAV
            NPLS=1
10049     CONTINUE
          IAIC=0
10048   CONTINUE
C
        NINT=0
C
        XCDN=MAX(MIN(XAT1,XATM),MIN(MAX(XAT1,XATM),
     +                                  XCVD+DELX))
        YCDN=MAX(MIN(YAT1,YATN),MIN(MAX(YAT1,YATN),
     +                                       YCVD))
        CALL HLUCPMPXY (IMPF,XCDN,YCDN,OORN,YTMP)
        IF (ICFELL('CPTREV',14).NE.0) GO TO 102
C
        DO 10050 I=1,8
          XCDP=XCDN
          YCDP=YCDN
          OORP=OORN
          XCDN=MAX(MIN(XAT1,XATM),MIN(MAX(XAT1,XATM),
     +                            XCVD+DELX*XOUC(I)))
          YCDN=MAX(MIN(YAT1,YATN),MIN(MAX(YAT1,YATN),
     +                            YCVD+DELY*YOUC(I)))
          CALL HLUCPMPXY (IMPF,XCDN,YCDN,OORN,YTMP)
          IF (ICFELL('CPTREV',15).NE.0) GO TO 102
          IF (.NOT.(OORP.EQ.OORV.AND.OORN.NE.OORV)) GO TO 10051
            XCDI=XCDP
            YCDI=YCDP
            XCDV=XCDN
            YCDV=YCDN
            GO TO 10053
10051     CONTINUE
          IF (.NOT.(OORP.NE.OORV.AND.OORN.EQ.OORV)) GO TO 10054
            XCDV=XCDP
            YCDV=YCDP
            XCDI=XCDN
            YCDI=YCDN
            GO TO 10053
10054     CONTINUE
          GO TO 10056
10053     CONTINUE
            IF (.NOT.(NINT.LT.3)) GO TO 10057
              NINT=NINT+1
              CALL HLUCPMPXY (IMPF,XCDV,YCDV,XCUV(NINT),YCUV(NINT))
              IF (ICFELL('CPTREV',16).NE.0) GO TO 102
              ITMP=0
10058         CONTINUE
                XCDH=(XCDV+XCDI)/2.
                YCDH=(YCDV+YCDI)/2.
                CALL HLUCPMPXY (IMPF,XCDH,YCDH,XCUH,YCUH)
                IF (ICFELL('CPTREV',17).NE.0) GO TO 102
                IF (.NOT.(XCUH.NE.OORV)) GO TO 10059
                  IF (XCDH.EQ.XCDV.AND.YCDH.EQ.YCDV) GO TO 10060
                  XCDV=XCDH
                  YCDV=YCDH
                  XCUV(NINT)=XCUH
                  YCUV(NINT)=YCUH
                GO TO 10061
10059           CONTINUE
                  IF (XCDH.EQ.XCDI.AND.YCDH.EQ.YCDI) GO TO 10060
                  XCDI=XCDH
                  YCDI=YCDH
10061           CONTINUE
                ITMP=ITMP+1
                IF (ITMP.EQ.64) GO TO 10060
              GO TO 10058
10060         CONTINUE
10057       CONTINUE
10056     CONTINUE
10050   CONTINUE
C
        IF (.NOT.(NINT.EQ.2)) GO TO 10062
C
          SINP=SINN
          COSP=COSN
          ANGP=ANGN
          SINN=YCUV(2)-YCUV(1)
          COSN=XCUV(2)-XCUV(1)
          ANGN=57.2957795130823*ATAN2(SINN,COSN)
C
          IF (.NOT.(NPLS.NE.0)) GO TO 10063
C
            IF (.NOT.(ABS(XCVU-RWRK(IR01+NPLS)).LE..001*ABS(XWDR-XWDL).A
     +ND.ABS(YCVU-RWRK(IR01+MPLS+NPLS)).LE..001*ABS(YWDT-YWDB))) GO TO 1
     +0064
              IF (.NOT.(XCVU.NE.RWRK(IR01+NPLS).OR.YCVU.NE.RWRK(IR01+MPL
     +S+NPLS))) GO TO 10065
                GO TO 106
10065         CONTINUE
                GO TO 108
10064       CONTINUE
C
            ANGC=57.2957795130823*ATAN2(YCVU-RWRK(IR01+MPLS+NPLS),
     +                                  XCVU-RWRK(IR01     +NPLS))
            ANGP=ANGP-180.*SIGN(REAL(INT((ABS(ANGP-ANGC)+90.)/180.)),
     +                                                      ANGP-ANGC)
            ANGN=ANGN-180.*SIGN(REAL(INT((ABS(ANGN-ANGC)+90.)/180.)),
     +                                                      ANGN-ANGC)
            IF (.NOT.((MIRO.EQ.0.AND.ANGP.LT.ANGC.AND.ANGC.LT.ANGN).OR.(
     +MIRO.NE.0.AND.ANGP.GT.ANGC.AND.ANGC.GT.ANGN))) GO TO 10066
              DNOM=COSN*SINP-SINN*COSP
              IF (.NOT.(DNOM.NE.0.)) GO TO 10067
                XINT=(COSN*(SINP*RWRK(IR01     +NPLS)-
     +                      COSP*RWRK(IR01+MPLS+NPLS))-
     +                COSP*(SINN*XCVU-COSN*YCVU))/DNOM
                YINT=(SINN*(SINP*RWRK(IR01     +NPLS)-
     +                      COSP*RWRK(IR01+MPLS+NPLS))-
     +                SINP*(SINN*XCVU-COSN*YCVU))/DNOM
                IF (.NOT.(NPLS.GE.MPLS-1)) GO TO 10068
                  XSAV=RWRK(IR01     +NPLS)
                  YSAV=RWRK(IR01+MPLS+NPLS)
                  IJMP=3
                  IRW1=IR01
                  IRW2=IR01+MPLS
                  NRWK=NPLS
                  RETURN
  105             RWRK(IR01     +1)=XSAV
                  RWRK(IR01+MPLS+1)=YSAV
                  NPLS=1
10068           CONTINUE
                NPLS=NPLS+1
                RWRK(IR01     +NPLS)=XINT
                RWRK(IR01+MPLS+NPLS)=YINT
10067         CONTINUE
10066       CONTINUE
C
10063     CONTINUE
C
10062   CONTINUE
C
  106   NPLS=NPLS+1
        RWRK(IR01     +NPLS)=XCVU
        RWRK(IR01+MPLS+NPLS)=YCVU
C
        IF (.NOT.(NPLS.GE.MPLS.OR.(NPLS.GT.1.AND.IAID.NE.IAIC)))
     +  GO TO 10069
          XSAV=RWRK(IR01     +NPLS)
          YSAV=RWRK(IR01+MPLS+NPLS)
          IJMP=4
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  107     RWRK(IR01     +1)=XSAV
          RWRK(IR01+MPLS+1)=YSAV
          NPLS=1
10069   CONTINUE
C
  108   IAIC=IAID
C
      GO TO (10028,10035) , L10029
C
      END
