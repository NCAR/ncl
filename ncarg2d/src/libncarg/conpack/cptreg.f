C
C	$Id: cptreg.f,v 1.1.1.1 1992-04-17 22:32:50 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CPTREG (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C This routine traces the edge of the grid.  Control is passed back to
C the caller with each piece of the edge for processing.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C IJMP is initially set to zero by the caller.  Upon return, it will be
C zero if all segments have been traced and processed, non-zero if the
C caller is expected to process a segment and recall CPTREG.
C
C IAIC is both an input and an output variable.  If it is initially set
C to -9 by the caller, it will not be changed by CPTREG and no attempt
C will be made to determine what area identifier should be used for the
C area on the contoured side of the edge of the grid.  If its initial
C value is 0, it will have been updated, upon every return with IJMP
C non-zero, to the area identifier for the contoured side of the piece
C of the edge defined by IRW1, IRW2, and NRWK.
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
C
C Because of the way this routine is entered and re-entered, we need to
C save every variable it uses.
C
      SAVE
C
C Define an arithmetic statement function for use below.
C
      FRCT(ZDT1,ZDT2)=(CLEV(ICLV)-ZDT1)/(ZDT2-ZDT1)
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (101,104,105,106,107) , IJMP
C
C Assign space to use for storing the X and Y coordinates of points.
C
      MPLS=LRWC
      CALL CPGRWS (RWRK,1,2*MPLS,IWSE)
      IF (IWSE.NE.0) RETURN
C
C Compute constants required to get from indices to X and Y coordinates.
C
      RZDM=(XATM-XAT1)/REAL(IZDM-1)
      RZDN=(YATN-YAT1)/REAL(IZDN-1)
C
C Compute quantities used to see if two points are essentially
C different from one another.
C
      SMLX=.001*ABS(XWDR-XWDL)
      SMLY=.001*ABS(YWDT-YWDB)
C
C If the mapping flag is off and no area identifiers are to be returned,
C the boundary can be defined as a simple rectangle.
C
      IF (.NOT.(IMPF.EQ.0.AND.IAIC.EQ.-9)) GO TO 10001
C
        RWRK(IR01     +1)=XAT1
        RWRK(IR01+MPLS+1)=YAT1
        RWRK(IR01     +2)=XATM
        RWRK(IR01+MPLS+2)=YAT1
        RWRK(IR01     +3)=XATM
        RWRK(IR01+MPLS+3)=YATN
        RWRK(IR01     +4)=XAT1
        RWRK(IR01+MPLS+4)=YATN
        RWRK(IR01     +5)=XAT1
        RWRK(IR01+MPLS+5)=YAT1
C
        IJMP=1
        IRW1=IR01
        IRW2=IR01+MPLS
        NRWK=5
        RETURN
C
C Otherwise, more points must be used.  In particular, all points of
C intersection of contour lines with the boundary must be included.
C Also, the possibility of invisible areas must be provided for.
C
10001 CONTINUE
C
        NPLS=0
        RUDN=0.
C
        INDX=1
        INDY=1
        XCES=1.
        YCES=1.
        ZCES=ZDAT(1,1)
        IF (SVAL.NE.0..AND.ZDAT(1,2).EQ.SVAL) ZCES=SVAL
        XCND=1.
        YCND=1.
        ZCND=ZCES
        ASSIGN 10002 TO L10003
        GO TO 10003
10002   CONTINUE
C
        IADX=0
        IADY=1
10004   CONTINUE
        IF (.NOT.(INDX.LT.IZDM)) GO TO 10005
          INDX=INDX+1
          ASSIGN 10006 TO L10007
          GO TO 10007
10006     CONTINUE
        GO TO 10004
10005   CONTINUE
C
        IADX=-1
        IADY=0
10008   CONTINUE
        IF (.NOT.(INDY.LT.IZDN)) GO TO 10009
          INDY=INDY+1
          ASSIGN 10010 TO L10007
          GO TO 10007
10010     CONTINUE
        GO TO 10008
10009   CONTINUE
C
        IADX=0
        IADY=-1
10011   CONTINUE
        IF (.NOT.(INDX.GT.1)) GO TO 10012
          INDX=INDX-1
          ASSIGN 10013 TO L10007
          GO TO 10007
10013     CONTINUE
        GO TO 10011
10012   CONTINUE
C
        IADX=1
        IADY=0
10014   CONTINUE
        IF (.NOT.(INDY.GT.1)) GO TO 10015
          INDY=INDY-1
          ASSIGN 10016 TO L10007
          GO TO 10007
10016     CONTINUE
        GO TO 10014
10015   CONTINUE
C
        IF (.NOT.(NPLS.NE.0)) GO TO 10017
          IJMP=1
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
10017   CONTINUE
C
C
C Release the real workspace and let the caller know we're done.
C
  101 LR01=0
      IJMP=0
C
C Done.
C
      RETURN
C
C The following internal procedure processes a segment along the edge
C of the grid.
C
10007 CONTINUE
        XCSS=XCES
        YCSS=YCES
        ZCSS=ZCES
        XCES=REAL(INDX)
        YCES=REAL(INDY)
        ZCES=ZDAT(INDX,INDY)
        IF (SVAL.NE.0..AND.ZDAT(INDX+IADX,INDY+IADY).EQ.SVAL)ZCES=SVAL
          INTP = 1
          GO TO 10020
10018     CONTINUE
          INTP =INTP +1
10020     CONTINUE
          IF (INTP .GT.(IABS(IPIE)+1)) GO TO 10019
          FINT=REAL(INTP)/REAL(IABS(IPIE)+1)
          XCOD=XCND
          YCOD=YCND
          ZCOD=ZCND
          XCOU=XCNU
          YCOU=YCNU
          IVOU=IVNU
          XCND=(1.-FINT)*XCSS+FINT*XCES
          YCND=(1.-FINT)*YCSS+FINT*YCES
          IF (.NOT.(INTP.NE.IABS(IPIE)+1)) GO TO 10021
            IF (.NOT.(SVAL.EQ.0..OR.(ZCSS.NE.SVAL.AND.ZCES.NE.SVAL)))
     +      GO TO 10022
              ZCND=(1.-FINT)*ZCSS+FINT*ZCES
            GO TO 10023
10022       CONTINUE
              ZCND=SVAL
10023       CONTINUE
          GO TO 10024
10021     CONTINUE
            ZCND=ZCES
10024     CONTINUE
          ASSIGN 10025 TO L10003
          GO TO 10003
10025     CONTINUE
          IF (.NOT.(SVAL.EQ.0..OR.(ZCOD.NE.SVAL.AND.ZCND.NE.SVAL)))
     +    GO TO 10026
            IF (.NOT.(ZCOD.LT.ZCND)) GO TO 10027
                I = 1
                GO TO 10030
10028           CONTINUE
                I =I +1
10030           CONTINUE
                IF (I .GT.(NCLV)) GO TO 10029
                ICLV=ICLP(I)
                IF (.NOT.(CLEV(ICLV).GT.ZCOD.AND.CLEV(ICLV).LT.ZCND.AND.
     +(IAIA(ICLV).NE.0.OR.IAIB(ICLV).NE.0))) GO TO 10031
                  ASSIGN 10032 TO L10033
                  GO TO 10033
10032             CONTINUE
10031           CONTINUE
              GO TO 10028
10029         CONTINUE
            GO TO 10034
10027       CONTINUE
            IF (.NOT.(ZCND.LT.ZCOD)) GO TO 10035
                I = NCLV
                GO TO 10038
10036           CONTINUE
                I =I -1
10038           CONTINUE
                IF (I .LT.(1)) GO TO 10037
                ICLV=ICLP(I)
                IF (.NOT.(CLEV(ICLV).GT.ZCND.AND.CLEV(ICLV).LT.ZCOD.AND.
     +(IAIA(ICLV).NE.0.OR.IAIB(ICLV).NE.0))) GO TO 10039
                  ASSIGN 10040 TO L10033
                  GO TO 10033
10040             CONTINUE
10039           CONTINUE
              GO TO 10036
10037         CONTINUE
10034       CONTINUE
10035       CONTINUE
10026     CONTINUE
          IF (.NOT.(IPIE.LT.0.AND.INTP.NE.IABS(IPIE)+1)) GO TO 10041
            IFOP=0
          GO TO 10042
10041     CONTINUE
            IFOP=1
10042     CONTINUE
          ASSIGN 10043 TO L10044
          GO TO 10044
10043     CONTINUE
        GO TO 10018
10019   CONTINUE
      GO TO L10007 , (10016,10013,10010,10006)
C
C The following internal procedure interpolates a point where a contour
C line intersects the piece of the edge segment that we're working on.
C
10033 CONTINUE
        XCSD=XCND
        YCSD=YCND
        ZCSD=ZCND
        XCSU=XCNU
        YCSU=YCNU
        IVSU=IVNU
        XCND=XCOD+(XCND-XCOD)*FRCT(ZCOD,ZCND)
        YCND=YCOD+(YCND-YCOD)*FRCT(ZCOD,ZCND)
        ZCND=CLEV(ICLV)
        ASSIGN 10045 TO L10003
        GO TO 10003
10045   CONTINUE
        IFOP=1
        ASSIGN 10046 TO L10044
        GO TO 10044
10046   CONTINUE
        XCOD=XCND
        YCOD=YCND
        ZCOD=ZCND
        XCOU=XCNU
        YCOU=YCNU
        IVOU=IVNU
        XCND=XCSD
        YCND=YCSD
        ZCND=ZCSD
        XCNU=XCSU
        YCNU=YCSU
        IVNU=IVSU
      GO TO L10033 , (10040,10032)
C
C The following internal procedure processes a piece of a segment.
C There are several cases, depending on whether both endpoints are
C visible, neither endpoint is visible, or just one of them is visible.
C
10044 CONTINUE
C
        IAID=IAIC
C
        IF (.NOT.(IAIC.NE.-9)) GO TO 10047
          IF (.NOT.(SVAL.NE.0..AND.(ZCND.EQ.SVAL.OR.ZCOD.EQ.SVAL)))
     +    GO TO 10048
            IAID=IAIA(258)
          GO TO 10049
10048     CONTINUE
            IF (.NOT.(NCLV.LE.0)) GO TO 10050
              IAID=1
            GO TO 10051
10050       CONTINUE
              IAID=0
              ZAVG=.5*(ZCND+ZCOD)
              DO 10052 J=1,NCLV
                JCLV=ICLP(J)
                IF (.NOT.(ZAVG.LE.CLEV(JCLV))) GO TO 10053
                  IF (.NOT.(IAIB(JCLV).NE.0)) GO TO 10054
                    IAID=IAIB(JCLV)
                    GO TO 103
10054             CONTINUE
                    IF (J.EQ.NCLV) GO TO 102
                    IF (ZAVG.NE.CLEV(JCLV).AND.
     +                  CLEV(JCLV).NE.CLEV(ICLP(J+1))) GO TO 102
10053           CONTINUE
10052         CONTINUE
  102         CONTINUE
              DO 10055 J=NCLV,1,-1
                JCLV=ICLP(J)
                IF (.NOT.(ZAVG.GE.CLEV(JCLV))) GO TO 10056
                  IF (.NOT.(IAIA(JCLV).NE.0)) GO TO 10057
                    IAID=IAIA(JCLV)
                    GO TO 103
10057             CONTINUE
                    IF (J.EQ.1) GO TO 103
                    IF (ZAVG.NE.CLEV(JCLV).AND.
     +                  CLEV(JCLV).NE.CLEV(ICLP(J-1))) GO TO 103
10056           CONTINUE
10055         CONTINUE
  103       CONTINUE
10051       CONTINUE
10049     CONTINUE
10047   CONTINUE
C
        IF (.NOT.(NPLS.EQ.0)) GO TO 10058
          IF (.NOT.(IVOU.NE.0)) GO TO 10059
            NPLS=1
            RWRK(IR01+NPLS)=XCOU
            RWRK(IR01+MPLS+NPLS)=YCOU
          GO TO 10060
10059     CONTINUE
            IF (.NOT.(IVNU.NE.0)) GO TO 10061
              XCID=XCOD
              YCID=YCOD
              XCVD=XCND
              YCVD=YCND
              XCVU=XCNU
              YCVU=YCNU
              ASSIGN 10062 TO L10063
              GO TO 10063
10062         CONTINUE
              XCOD=XCVD
              YCOD=YCVD
              XCOU=XCVU
              YCOU=YCVU
              IVOU=1
10061       CONTINUE
10060     CONTINUE
        GO TO 10064
10058   CONTINUE
        IF (.NOT.(NPLS.EQ.MPLS.OR.IAID.NE.IAIC)) GO TO 10065
          XSAV=RWRK(IR01+NPLS)
          YSAV=RWRK(IR01+MPLS+NPLS)
          IJMP=2
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  104     RWRK(IR01+1)=XSAV
          RWRK(IR01+MPLS+1)=YSAV
          NPLS=1
10064   CONTINUE
10065   CONTINUE
C
        IF (.NOT.(IVNU.NE.0)) GO TO 10066
          IF (.NOT.(IMPF.NE.0.AND.(XCND.NE.XCOD.OR.YCND.NE.YCOD)))
     +    GO TO 10067
            RUDO=RUDN
            RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +           (ABS(XCND-XCOD)+ABS(YCND-YCOD))
            IF (.NOT.(RUDN.GT.10.*RUDO)) GO TO 10068
              ASSIGN 10069 TO L10070
              GO TO 10070
10069         CONTINUE
10068       CONTINUE
10067     CONTINUE
          IF (.NOT.(IFOP.NE.0)) GO TO 10071
            NPLS=NPLS+1
            RWRK(IR01+NPLS)=XCNU
            RWRK(IR01+MPLS+NPLS)=YCNU
10071     CONTINUE
        GO TO 10072
10066   CONTINUE
          IF (.NOT.(IVOU.NE.0)) GO TO 10073
            XCVD=XCOD
            YCVD=YCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XCND
            YCID=YCND
            ASSIGN 10074 TO L10063
            GO TO 10063
10074       CONTINUE
            IJMP=3
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
  105       NPLS=0
            RUDN=0.
10073     CONTINUE
10072   CONTINUE
C
        IF (IAIC.NE.-9) IAIC=IAID
C
      GO TO L10044 , (10046,10043)
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the edge is seen.  It checks
C for a possible discontinuity in the mapping function (as can happen,
C for example, when a cylindrical equidistant projection is being used);
C if there is such a discontinuity, we must generate a final point on
C one side of it, dump the polyline, and then start a new polyline on
C the other side.
C
10070 CONTINUE
        XC1D=XCOD
        YC1D=YCOD
        XC1U=XCOU
        YC1U=YCOU
        XC2D=XCND
        YC2D=YCND
        XC2U=XCNU
        YC2U=YCNU
        ITMP=0
10075   CONTINUE
          XC3D=(XC1D+XC2D)/2.
          YC3D=(YC1D+YC2D)/2.
          CALL CPMPXY (IMPF,XAT1+RZDM*(XC3D-1.),YAT1+RZDN*(YC3D-1.),
     +                                                      XC3U,YC3U)
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10076
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(DST1.LT.DST2)) GO TO 10077
              IF (XC3D.EQ.XC1D.AND.YC3D.EQ.YC1D) GO TO 10078
              XC1D=XC3D
              YC1D=YC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10079
10077       CONTINUE
              IF (XC3D.EQ.XC2D.AND.YC3D.EQ.YC2D) GO TO 10078
              XC2D=XC3D
              YC2D=YC3D
              XC2U=XC3U
              YC2U=YC3U
10079       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10078
          GO TO 10080
10076     CONTINUE
            XCVD=XCOD
            YCVD=YCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XC3D
            YCID=YC3D
            ASSIGN 10081 TO L10063
            GO TO 10063
10081       CONTINUE
            IJMP=4
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
  106       NPLS=0
            RUDN=0.
            XCID=XC3D
            YCID=YC3D
            XCVD=XCND
            YCVD=YCND
            XCVU=XCNU
            YCVU=YCNU
            ASSIGN 10082 TO L10063
            GO TO 10063
10082       CONTINUE
            ITMP=1000
            GO TO 10078
10080     CONTINUE
        GO TO 10075
10078   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10083
          NPLS=NPLS+1
          RWRK(IR01+NPLS)=XC1U
          RWRK(IR01+MPLS+NPLS)=YC1U
          IJMP=5
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  107     NPLS=1
          RWRK(IR01+1)=XC2U
          RWRK(IR01+MPLS+1)=YC2U
          RUDN=0.
10083   CONTINUE
      GO TO L10070 , (10069)
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10063 CONTINUE
        ITMP=0
10084   CONTINUE
          XCHD=(XCVD+XCID)/2.
          YCHD=(YCVD+YCID)/2.
          CALL CPMPXY (IMPF,XAT1+RZDM*(XCHD-1.),YAT1+RZDN*(YCHD-1.),
     +                                                      XCHU,YCHU)
          IF (.NOT.(XCHU.NE.OORV)) GO TO 10085
            IF (XCHD.EQ.XCVD.AND.YCHD.EQ.YCVD) GO TO 10086
            XCVD=XCHD
            YCVD=YCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10087
10085     CONTINUE
            IF (XCHD.EQ.XCID.AND.YCHD.EQ.YCID) GO TO 10086
            XCID=XCHD
            YCID=YCHD
10087     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10086
        GO TO 10084
10086   CONTINUE
        NPLS=NPLS+1
        RWRK(IR01+NPLS)=XCVU
        RWRK(IR01+MPLS+NPLS)=YCVU
      GO TO L10063 , (10082,10081,10074,10062)
C
C The following internal procedure is given the data-system coordinates
C of a point (XCND,YCND) and computes the user-system coordinates of
C the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10003 CONTINUE
C
        XCNU=XAT1+RZDM*(XCND-1.)
        YCNU=YAT1+RZDN*(YCND-1.)
        IVNU=1
C
        IF (.NOT.(IMPF.NE.0)) GO TO 10088
          XTMP=XCNU
          YTMP=YCNU
          CALL CPMPXY (IMPF,XTMP,YTMP,XCNU,YCNU)
          IF ((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)) IVNU=0
10088   CONTINUE
C
      GO TO L10003 , (10045,10025,10002)
C
      END
