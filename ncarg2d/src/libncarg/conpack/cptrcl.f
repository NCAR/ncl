C
C $Id: cptrcl.f,v 1.2 1994-03-17 01:52:15 kennison Exp $
C
      SUBROUTINE CPTRCL (ZDAT,RWRK,IWRK,CLVL,IJMP,IRW1,IRW2,NRWK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C Given ZDAT (an array of data), RWRK (a real workspace), IWRK (an
C integer workspace), and CLVL (a particular contour level), CPTRCL
C finds the beginning of each contour line at the level CLVL and then
C traces it.  Control is passed back to the caller to process each
C line segment generated.
C
C ZDAT is the doubly-dimensioned array of data being contoured.
C
C RWRK is a real workspace array.
C
C IWRK is an integer workspace array.
C
C CLVL is the contour level being worked on.
C
C IJMP is initially set to zero by the caller.  Upon return, it will be
C zero if all segments have been traced and processed, non-zero if the
C caller is expected to process a segment and recall CPTRCL.
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
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*40 TXCF
      CHARACTER*20 TXHI
      CHARACTER*100 TXIL
      CHARACTER*20 TXLO
C
C Because of the way this routine is entered and reentered, we need to
C save every variable it uses.
C
      SAVE
C
C Define an interpolation function.
C
      FRCT(ZDT1,ZDT2)=(CLVL-ZDT1)/(ZDT2-ZDT1)
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (105,103,104,106,107) , IJMP
C
C Otherwise, compute some needed conversion constants.
C
      RZDM=(XATM-XAT1)/REAL(IZDM-1)
      RZDN=(YATN-YAT1)/REAL(IZDN-1)
C
C Assign space to use for storing the coordinates of points on contour
C lines.
C
      IF (.NOT.(T2DS.EQ.0.)) GO TO 10001
        CALL CPGRWS (RWRK,1,2*LRWC,IWSE)
      GO TO 10002
10001 CONTINUE
        CALL CPGRWS (RWRK,1,7*LRWC,IWSE)
10002 CONTINUE
C
      IF (IWSE.NE.0.OR.ICFELL('CPTRCL',1).NE.0) RETURN
C
C Set the offset from one portion of the real workspace to the next.
C
      MPLS=LRWC
C
C Set some tolerance parameters.
C
      IF (.NOT.(T2DS.LT.0.)) GO TO 10003
        DBPI=ABS(XATM-XAT1)*SEGL
        EPSX=ABS(XATM-XAT1)*EPSI
        EPSY=ABS(YATN-YAT1)*EPSI
      GO TO 10004
10003 CONTINUE
        DBPI=ABS(XWDR-XWDL)*SEGL
        EPSX=ABS(XWDR-XWDL)*EPSI
        EPSY=ABS(YWDT-YWDB)*EPSI
10004 CONTINUE
C
      SMLX=.001*ABS(XWDR-XWDL)
      SMLY=.001*ABS(YWDT-YWDB)
C
C Zero the count of horizontal segments seen so far.
C
      NHSS=0
C
C Initialize four variables to prevent the code from blowing up.  (The
C values only have to be legal values on the machine; they are used in
C one clause of a block-IF, the other clause of which makes the value
C immaterial.)
C
      XBFS=0.
      YBFS=0.
      XELS=0.
      YELS=0.
C
C Set IOCF to indicate that we are looking for open contours.
C
      IOCF=0
C
C Search the bottom edge of the grid.
C
      IVBY=1
        IVBX = 2
        GO TO 10007
10005   CONTINUE
        IVBX =IVBX +1
10007   CONTINUE
        IF (IVBX .GT.(IZDM)) GO TO 10006
        IF (.NOT.(ZDAT(IVBX-1,1).LT.CLVL.AND.ZDAT(IVBX,1).GE.CLVL))
     +  GO TO 10008
          INCI=1
          L10010=    1
          GO TO 10010
10009     CONTINUE
10008   CONTINUE
      GO TO 10005
10006 CONTINUE
C
C Search the right edge of the grid.
C
      IVBX=IZDM
        IVBY = 2
        GO TO 10013
10011   CONTINUE
        IVBY =IVBY +1
10013   CONTINUE
        IF (IVBY .GT.(IZDN)) GO TO 10012
        IF (.NOT.(ZDAT(IZDM,IVBY-1).LT.CLVL.AND.ZDAT(IZDM,IVBY).GE.CLVL)
     +) GO TO 10014
          INCI=7
          L10010=    2
          GO TO 10010
10015     CONTINUE
10014   CONTINUE
      GO TO 10011
10012 CONTINUE
C
C Search the top edge of the grid.
C
      IVBY=IZDN
        IVBX = IZDM-1
        GO TO 10018
10016   CONTINUE
        IVBX =IVBX -1
10018   CONTINUE
        IF (IVBX .LT.(1)) GO TO 10017
        IF (.NOT.(ZDAT(IVBX+1,IZDN).LT.CLVL.AND.ZDAT(IVBX,IZDN).GE.CLVL)
     +) GO TO 10019
          INCI=5
          L10010=    3
          GO TO 10010
10020     CONTINUE
10019   CONTINUE
      GO TO 10016
10017 CONTINUE
C
C Search the left edge of the grid.
C
      IVBX=1
        IVBY = IZDN-1
        GO TO 10023
10021   CONTINUE
        IVBY =IVBY -1
10023   CONTINUE
        IF (IVBY .LT.(1)) GO TO 10022
        IF (.NOT.(ZDAT(1,IVBY+1).LT.CLVL.AND.ZDAT(1,IVBY).GE.CLVL))
     +  GO TO 10024
          INCI=3
          L10010=    4
          GO TO 10010
10025     CONTINUE
10024   CONTINUE
      GO TO 10021
10022 CONTINUE
C
C Set IOCF to indicate that we are looking for closed contours.
C
      IOCF=1
C
C Search the interior of the grid.
C
        IVBY = 2
        GO TO 10028
10026   CONTINUE
        IVBY =IVBY +1
10028   CONTINUE
        IF (IVBY .GT.(IZDN-1)) GO TO 10027
          IVBX = 2
          GO TO 10031
10029     CONTINUE
          IVBX =IVBX +1
10031     CONTINUE
          IF (IVBX .GT.(IZDM)) GO TO 10030
          IF (.NOT.(ZDAT(IVBX-1,IVBY).LT.CLVL.AND.ZDAT(IVBX,IVBY).GE.CLV
     +L)) GO TO 10032
            IPXY=IZDN*IVBX+IVBY
            DO 10033 I=1,NHSS
              IF (IPXY.EQ.IWRK(II01+I)) GO TO 101
10033       CONTINUE
            IF (.NOT.(NHSS.GE.LI01)) GO TO 10034
              CALL CPGIWS (IWRK,1,LI01+100,IWSE)
              IF (IWSE.NE.0.OR.ICFELL('CPTRCL',2).NE.0) GO TO 102
10034       CONTINUE
            NHSS=NHSS+1
            IWRK(II01+NHSS)=IPXY
            INCI=1
            L10010=    5
            GO TO 10010
10035       CONTINUE
  101     CONTINUE
10032     CONTINUE
        GO TO 10029
10030   CONTINUE
      GO TO 10026
10027 CONTINUE
C
C Done.
C
  102 LI01=0
      LR01=0
      IJMP=0
      RETURN
C
C Line-following algorithm.  This internal routine moves the line-
C following vector (defined by the base point (IVBX,IVBY) and the
C components INCX(INCI) and INCY(INCI)) along a contour line.  The
C points defining the contour line are thereby determined.  The
C process stops when either the starting point or the edge of the
C grid is encountered.
C
10010 CONTINUE
C
C Save the parameters defining the original position of the line-
C following vector.
C
        MVBX=IVBX
        MVBY=IVBY
        MNCI=INCI
C
C Set parameters defining the position of the end of the vector.
C
        IVEX=IVBX+INCX(INCI)
        IVEY=IVBY+INCY(INCI)
C
C Compute the coordinates, in the data-index coordinate system, of the
C starting position of the contour line.
C
        IF (.NOT.(IVEX.NE.IVBX)) GO TO 10036
          XCND=REAL(IVBX)+REAL(IVEX-IVBX)*FRCT(ZDAT(IVBX,IVBY),
     +                                         ZDAT(IVEX,IVEY))
          YCND=REAL(IVBY)
        GO TO 10037
10036   CONTINUE
          XCND=REAL(IVBX)
          YCND=REAL(IVBY)+REAL(IVEY-IVBY)*FRCT(ZDAT(IVBX,IVBY),
     +                                         ZDAT(IVEX,IVEY))
10037   CONTINUE
C
C Map the coordinates (XCND,YCND) into user coordinates (XCNU,YCNU).
C
        L10039=    1
        GO TO 10039
10038   CONTINUE
C
C Zero the number of points in the coordinate arrays, initialize the
C flag which indicates that we're working on the first segment, and
C zero the parameter which keeps track of the ratio of segment length
C in the user system and segment length in the data-index system.
C
        NPLS=0
        IFSF=1
        RUDN=0.
C
C Loop, moving the line-following vector as dictated by the positions
C of its end points.
C
10040   CONTINUE
C
C At this point, we know that the base of the line-following vector is
C on the high side of the contour and that the end of it is on the other
C side.  Move the vector clockwise and see what the situation is then.
C
          INCI=INCI+1
          IF (INCI.GT.8) INCI=INCI-8
          IVEX=IVBX+INCX(INCI)
          IVEY=IVBY+INCY(INCI)
C
C Exit the loop if we've hit the edge.
C
          IF (IVEX.LT.1.OR.IVEX.GT.IZDM.OR.IVEY.LT.1.OR.IVEY.GT.IZDN)
     +    GO TO 10041
C
C If the end of the line-following vector is now on the same side of
C the contour line as its base ...
C
          IF (.NOT.(ZDAT(IVEX,IVEY).GE.CLVL)) GO TO 10042
C
C flip it end-for-end and continue the loop.
C
            IVBX=IVEX
            IVBY=IVEY
            INCI=INCI+4
C
C Otherwise, if the line-following vector is currently horizontal or
C vertical, we have another point to add to the contour line ...
C
          GO TO 10043
10042     CONTINUE
          IF (.NOT.((INCI/2)*2.NE.INCI)) GO TO 10044
C
C so save the coordinates of the old point and compute the coordinates
C of the new one.
C
            XCOD=XCND
            YCOD=YCND
            XCOU=XCNU
            YCOU=YCNU
            IVOU=IVNU
C
            IF (.NOT.(IVEX.NE.IVBX)) GO TO 10045
              XCND=REAL(IVBX)+REAL(IVEX-IVBX)*FRCT(ZDAT(IVBX,IVBY),
     +                                             ZDAT(IVEX,IVEY))
              YCND=REAL(IVBY)
            GO TO 10046
10045       CONTINUE
              XCND=REAL(IVBX)
              YCND=REAL(IVBY)+REAL(IVEY-IVBY)*FRCT(ZDAT(IVBX,IVBY),
     +                                             ZDAT(IVEX,IVEY))
10046       CONTINUE
C
            L10039=    2
            GO TO 10039
10047       CONTINUE
C
C Save the coordinates of the point.  Special values complicate things.
C
            IF (.NOT.(SVAL.EQ.0.)) GO TO 10048
              L10050=    1
              GO TO 10050
10049         CONTINUE
            GO TO 10051
10048       CONTINUE
              IF (.NOT.(INCI.EQ.1)) GO TO 10052
                INCP=9
              GO TO 10053
10052         CONTINUE
                INCP=INCI
10053         CONTINUE
              IVCX=IVBX+INCX(INCP-1)
              IVCY=IVBY+INCY(INCP-1)
              IVDX=IVBX+INCX(INCP-2)
              IVDY=IVBY+INCY(INCP-2)
              IF (.NOT.(ZDAT(IVBX,IVBY).EQ.SVAL.OR.ZDAT(IVCX,IVCY).EQ.SV
     +AL.OR.ZDAT(IVDX,IVDY).EQ.SVAL.OR.ZDAT(IVEX,IVEY).EQ.SVAL)) GO TO 1
     +0054
                IF (.NOT.(NPLS.GT.1)) GO TO 10055
                  L10057=    1
                  GO TO 10057
10056             CONTINUE
10055           CONTINUE
              GO TO 10058
10054         CONTINUE
                L10050=    2
                GO TO 10050
10059           CONTINUE
10058         CONTINUE
10051       CONTINUE
C
C If we just crossed a horizontal grid line in the upwards direction,
C save that information.
C
            IF (.NOT.(INCI.EQ.1)) GO TO 10060
              IF (.NOT.(NHSS.GE.LI01)) GO TO 10061
                CALL CPGIWS (IWRK,1,LI01+100,IWSE)
                IF (IWSE.NE.0.OR.ICFELL('CPTRCL',3).NE.0) GO TO 102
10061         CONTINUE
              NHSS=NHSS+1
              IWRK(II01+NHSS)=IZDN*IVBX+IVBY
10060       CONTINUE
C
C If we just arrived at our starting point, quit the loop.
C
            IF (IVBX.EQ.MVBX.AND.IVBY.EQ.MVBY.AND.INCI.EQ.MNCI) GO TO 10
     +041
C
10043     CONTINUE
10044     CONTINUE
C
        GO TO 10040
10041   CONTINUE
C
C Process any remaining portion of the contour line.
C
        IF (.NOT.(NPLS.GT.1)) GO TO 10062
          L10057=    2
          GO TO 10057
10063     CONTINUE
10062   CONTINUE
C
C Reset the vector base to its original position.
C
        IVBX=MVBX
        IVBY=MVBY
C
C Done.
C
      GO TO (10009,10015,10020,10025,10035) , L10010
C
C The following internal procedure, given a line segment, adds visible
C portions of it to the coordinate arrays.
C
10050 CONTINUE
C
C If point interpolation is turned on, do the first IPIC segments.
C
        IF (.NOT.(IPIC.NE.0)) GO TO 10064
          XSOD=XCOD
          YSOD=YCOD
          XSND=XCND
          YSND=YCND
          XSNU=XCNU
          YSNU=YCNU
          ISNU=IVNU
            I = 1
            GO TO 10067
10065       CONTINUE
            I =I +1
10067       CONTINUE
            IF (I .GT.(IABS(IPIC))) GO TO 10066
            XCND=XSOD+REAL(I)/REAL(IABS(IPIC)+1)*(XSND-XSOD)
            YCND=YSOD+REAL(I)/REAL(IABS(IPIC)+1)*(YSND-YSOD)
            L10039=    3
            GO TO 10039
10068       CONTINUE
            IF (.NOT.(IPIC.GT.0.OR.IVNU.NE.IVOU)) GO TO 10069
              L10071=    1
              GO TO 10071
10070         CONTINUE
              XCOD=XCND
              YCOD=YCND
              XCOU=XCNU
              YCOU=YCNU
              IVOU=IVNU
10069       CONTINUE
          GO TO 10065
10066     CONTINUE
          XCND=XSND
          YCND=YSND
          XCNU=XSNU
          YCNU=YSNU
          IVNU=ISNU
10064   CONTINUE
C
C Finish off the job.
C
        L10071=    2
        GO TO 10071
10072   CONTINUE
C
      GO TO (10049,10059) , L10050
C
C The following internal procedure examines the points (XCOD,YCOD),
C which projects into (XCOU,YCOU), and (XCND,YCND), which projects into
C (XCNU,YCNU), either of which may be visible or invisible in the
C projection space, and adds visible portions of the line segment
C between them to the polyline being built.
C
10071 CONTINUE
C
        IF (.NOT.(XCND.NE.XCOD.OR.YCND.NE.YCOD)) GO TO 10073
          IF (.NOT.(NPLS.EQ.0)) GO TO 10074
            IF (.NOT.(IVOU.NE.0)) GO TO 10075
              NPLS=1
              RWRK(IR01+NPLS)=XCOU
              RWRK(IR01+MPLS+NPLS)=YCOU
            GO TO 10076
10075       CONTINUE
              IF (.NOT.(IVNU.NE.0)) GO TO 10077
                XCID=XCOD
                YCID=YCOD
                XCVD=XCND
                YCVD=YCND
                XCVU=XCNU
                YCVU=YCNU
                L10079=    1
                GO TO 10079
10078           CONTINUE
                XCOD=XCVD
                YCOD=YCVD
                XCOU=XCVU
                YCOU=YCVU
                IVOU=1
10077         CONTINUE
10076       CONTINUE
          GO TO 10080
10074     CONTINUE
          IF (.NOT.(NPLS.EQ.MPLS)) GO TO 10081
            XSAV=RWRK(IR01+NPLS)
            YSAV=RWRK(IR01+MPLS+NPLS)
            L10057=    3
            GO TO 10057
10082       CONTINUE
            RWRK(IR01+1)=XSAV
            RWRK(IR01+MPLS+1)=YSAV
            NPLS=1
10080     CONTINUE
10081     CONTINUE
C
          IF (.NOT.(IVNU.NE.0)) GO TO 10083
            IF (.NOT.(IMPF.NE.0.AND.(XCND.NE.XCOD.OR.YCND.NE.YCOD)))
     +      GO TO 10084
              RUDO=RUDN
              RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +             (ABS(XCND-XCOD)+ABS(YCND-YCOD))
              IF (.NOT.(RUDN.GT.10.*RUDO)) GO TO 10085
                L10087=    1
                GO TO 10087
10086           CONTINUE
10085         CONTINUE
10084       CONTINUE
            NPLS=NPLS+1
            RWRK(IR01+NPLS)=XCNU
            RWRK(IR01+MPLS+NPLS)=YCNU
          GO TO 10088
10083     CONTINUE
            IF (.NOT.(IVOU.NE.0)) GO TO 10089
              XCVD=XCOD
              YCVD=YCOD
              XCVU=XCOU
              YCVU=YCOU
              XCID=XCND
              YCID=YCND
              L10079=    2
              GO TO 10079
10090         CONTINUE
              L10057=    4
              GO TO 10057
10091         CONTINUE
10089       CONTINUE
10088     CONTINUE
10073   CONTINUE
C
      GO TO (10070,10072) , L10071
C
C The following internal procedure is given the data-system coordinates
C of a point (XCND,YCND) and computes the user-system coordinates of
C the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10039 CONTINUE
C
        XCNU=XAT1+RZDM*(XCND-1.)
        YCNU=YAT1+RZDN*(YCND-1.)
        IVNU=1
C
        IF (.NOT.(IMPF.NE.0.AND.T2DS.GE.0.)) GO TO 10092
          XTMP=XCNU
          YTMP=YCNU
          CALL CPMPXY (IMPF,XTMP,YTMP,XCNU,YCNU)
          IF (ICFELL('CPTRCL',4).NE.0) RETURN
          IF ((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)) IVNU=0
10092   CONTINUE
C
      GO TO (10038,10047,10068) , L10039
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the contour line is seen.
C It checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10087 CONTINUE
        XC1D=XCOD
        YC1D=YCOD
        XC1U=XCOU
        YC1U=YCOU
        XC2D=XCND
        YC2D=YCND
        XC2U=XCNU
        YC2U=YCNU
        ITMP=0
10093   CONTINUE
          XC3D=(XC1D+XC2D)/2.
          YC3D=(YC1D+YC2D)/2.
          CALL CPMPXY (IMPF,XAT1+RZDM*(XC3D-1.),YAT1+RZDN*(YC3D-1.),
     +                                                      XC3U,YC3U)
          IF (ICFELL('CPTRCL',5).NE.0) RETURN
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10094
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(DST1.LT.DST2)) GO TO 10095
              IF (XC3D.EQ.XC1D.AND.YC3D.EQ.YC1D) GO TO 10096
              XC1D=XC3D
              YC1D=YC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10097
10095       CONTINUE
              IF (XC3D.EQ.XC2D.AND.YC3D.EQ.YC2D) GO TO 10096
              XC2D=XC3D
              YC2D=YC3D
              XC2U=XC3U
              YC2U=YC3U
10097       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10096
          GO TO 10098
10094     CONTINUE
            XCVD=XCOD
            YCVD=YCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XC3D
            YCID=YC3D
            L10079=    3
            GO TO 10079
10099       CONTINUE
            L10057=    5
            GO TO 10057
10100       CONTINUE
            XCID=XC3D
            YCID=YC3D
            XCVD=XCND
            YCVD=YCND
            XCVU=XCNU
            YCVU=YCNU
            L10079=    4
            GO TO 10079
10101       CONTINUE
            ITMP=1000
            GO TO 10096
10098     CONTINUE
        GO TO 10093
10096   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10102
          NPLS=NPLS+1
          RWRK(IR01+NPLS)=XC1U
          RWRK(IR01+MPLS+NPLS)=YC1U
          L10057=    6
          GO TO 10057
10103     CONTINUE
          NPLS=NPLS+1
          RWRK(IR01+NPLS)=XC2U
          RWRK(IR01+MPLS+NPLS)=YC2U
10102   CONTINUE
      GO TO (10086) , L10087
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10079 CONTINUE
        ITMP=0
10104   CONTINUE
          XCHD=(XCVD+XCID)/2.
          YCHD=(YCVD+YCID)/2.
          CALL CPMPXY (IMPF,XAT1+RZDM*(XCHD-1.),YAT1+RZDN*(YCHD-1.),
     +                                                      XCHU,YCHU)
          IF (ICFELL('CPTRCL',6).NE.0) RETURN
          IF (.NOT.(XCHU.NE.OORV.AND.YCHU.NE.OORV)) GO TO 10105
            IF (XCHD.EQ.XCVD.AND.YCHD.EQ.YCVD) GO TO 10106
            XCVD=XCHD
            YCVD=YCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10107
10105     CONTINUE
            IF (XCHD.EQ.XCID.AND.YCHD.EQ.YCID) GO TO 10106
            XCID=XCHD
            YCID=YCHD
10107     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10106
        GO TO 10104
10106   CONTINUE
        NPLS=NPLS+1
        RWRK(IR01+NPLS)=XCVU
        RWRK(IR01+MPLS+NPLS)=YCVU
      GO TO (10078,10090,10099,10101) , L10079
C
C The following internal procedure processes a complete line segment.
C If the 2D smoother is turned on, the routines MSKRV1 and MSKRV2 are
C called to smooth the segment.
C
C
10057 CONTINUE
C
        I=1
C
10108   CONTINUE
          I=I+1
          IF (I.GT.NPLS) GO TO 10109
          IF (.NOT.(ABS(RWRK(IR01+I)-RWRK(IR01+I-1)).LT.EPSX.AND.ABS(RWR
     +K(IR01+MPLS+I)-RWRK(IR01+MPLS+I-1)).LT.EPSY)) GO TO 10110
            IF (.NOT.(I.NE.NPLS)) GO TO 10111
              DO 10112 J=I+1,NPLS
                RWRK(IR01+J-1)=RWRK(IR01+J)
                RWRK(IR01+MPLS+J-1)=RWRK(IR01+MPLS+J)
10112         CONTINUE
            GO TO 10113
10111       CONTINUE
              RWRK(IR01     +NPLS-1)=RWRK(IR01     +NPLS)
              RWRK(IR01+MPLS+NPLS-1)=RWRK(IR01+MPLS+NPLS)
10113       CONTINUE
            I=I-1
            NPLS=NPLS-1
10110     CONTINUE
        GO TO 10108
10109   CONTINUE
C
        IF (.NOT.(NPLS.GT.1)) GO TO 10114
C
          IF (.NOT.(T2DS.EQ.0.)) GO TO 10115
C
            IJMP=1
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
C
10115     CONTINUE
C
            IF (.NOT.(NPLS.GT.3.AND.ABS(RWRK(IR01+NPLS)-RWRK(IR01+1)).LT
     +.EPSX.AND.ABS(RWRK(IR01+MPLS+NPLS)-RWRK(IR01+MPLS+1)).LT.EPSY))
     +      GO TO 10116
              ISLP=4
            GO TO 10117
10116       CONTINUE
            IF (.NOT.(IFSF.EQ.0.AND.ABS(RWRK(IR01+1)-XELS).LT.EPSX.AND.A
     +BS(RWRK(IR01+MPLS+1)-YELS).LT.EPSY)) GO TO 10118
              ISLP=1
              SLP1=SELS
              IF (.NOT.(ABS(RWRK(IR01+NPLS)-XBFS).LT.EPSX.AND.ABS(RWRK(I
     +R01+MPLS+NPLS)-YBFS).LT.EPSY)) GO TO 10119
                ISLP=0
                SLPN=SBFS
10119         CONTINUE
            GO TO 10117
10118       CONTINUE
              ISLP=3
10117       CONTINUE
C
            CALL MSKRV1 (NPLS,RWRK(IR01+1),RWRK(IR01+MPLS+1),
     +                   SLP1,SLPN,RWRK(IR01+2*MPLS+1),
     +                   RWRK(IR01+3*MPLS+1),RWRK(IR01+5*MPLS+1),
     +                   RWRK(IR01+4*MPLS+1),ABS(T2DS),ISLP)
            IF (ICFELL('CPTRCL',7).NE.0) RETURN
C
            NINT=MAX(3,1+INT(RWRK(IR01+4*MPLS+NPLS)/DBPI))
C
            NOUT=0
            TUDN=0.
C
              IINT = 0
              GO TO 10122
10120         CONTINUE
              IINT =IINT +1
10122         CONTINUE
              IF (IINT .GT.(NINT)) GO TO 10121
              IF (.NOT.(IINT.EQ.0)) GO TO 10123
                XTMP=RWRK(IR01+1)
                YTMP=RWRK(IR01+MPLS+1)
              GO TO 10124
10123         CONTINUE
              IF (.NOT.(IINT.NE.NINT)) GO TO 10125
                CALL MSKRV2 (REAL(IINT)/REAL(NINT),XTMP,YTMP,NPLS,
     +                       RWRK(IR01+1),RWRK(IR01+MPLS+1),
     +                       RWRK(IR01+2*MPLS+1),RWRK(IR01+3*MPLS+1),
     +                       RWRK(IR01+4*MPLS+1),ABS(T2DS),0,DUMI)
                IF (ICFELL('CPTRCL',8).NE.0) RETURN
              GO TO 10124
10125         CONTINUE
                XTMP=RWRK(IR01+NPLS)
                YTMP=RWRK(IR01+MPLS+NPLS)
10124         CONTINUE
              IF (.NOT.(IMPF.EQ.0.OR.T2DS.GT.0.)) GO TO 10126
                NOUT=NOUT+1
                RWRK(IR01+5*MPLS+NOUT)=XTMP
                RWRK(IR01+6*MPLS+NOUT)=YTMP
              GO TO 10127
10126         CONTINUE
                IF (.NOT.(IINT.NE.0)) GO TO 10128
                  XTOD=XTND
                  YTOD=YTND
                  XTOU=XTNU
                  YTOU=YTNU
                  IVSO=IVSN
10128           CONTINUE
                XTND=XTMP
                YTND=YTMP
                CALL CPMPXY (IMPF,XTND,YTND,XTNU,YTNU)
                IF (ICFELL('CPTRCL',9).NE.0) RETURN
                IVSN=1
                IF (OORV.NE.0..AND.
     +              (XTNU.EQ.OORV.OR.YTNU.EQ.OORV)) IVSN=0
                IF (.NOT.(NOUT.EQ.0)) GO TO 10129
                  IF (.NOT.(IVSN.NE.0)) GO TO 10130
                    IF (.NOT.(IINT.NE.0)) GO TO 10131
                      XTID=XTOD
                      YTID=YTOD
                      XTVD=XTND
                      YTVD=YTND
                      XTVU=XTNU
                      YTVU=YTNU
                      L10133=    1
                      GO TO 10133
10132                 CONTINUE
                      XTOD=XTVD
                      YTOD=YTVD
                      XTOU=XTVU
                      YTOU=YTVU
                      IVSO=1
10131               CONTINUE
                    NOUT=NOUT+1
                    RWRK(IR01+5*MPLS+NOUT)=XTNU
                    RWRK(IR01+6*MPLS+NOUT)=YTNU
10130             CONTINUE
                GO TO 10134
10129           CONTINUE
                  IF (.NOT.(IVSN.NE.0)) GO TO 10135
                    IF (.NOT.(IMPF.NE.0.AND.(XTND.NE.XTOD.OR.YTND.NE.YTO
     +D)))          GO TO 10136
                      TUDO=TUDN
                      TUDN=(ABS(XTNU-XTOU)+ABS(YTNU-YTOU))/
     +                     (ABS(XTND-XTOD)+ABS(YTND-YTOD))
                      IF (.NOT.(TUDN.GT.10.*TUDO)) GO TO 10137
                        L10139=    1
                        GO TO 10139
10138                   CONTINUE
10137                 CONTINUE
10136               CONTINUE
                    NOUT=NOUT+1
                    RWRK(IR01+5*MPLS+NOUT)=XTNU
                    RWRK(IR01+6*MPLS+NOUT)=YTNU
                  GO TO 10140
10135             CONTINUE
                    XTVD=XTOD
                    YTVD=YTOD
                    XTVU=XTOU
                    YTVU=YTOU
                    XTID=XTND
                    YTID=YTND
                    L10133=    2
                    GO TO 10133
10141               CONTINUE
                    IJMP=2
                    IRW1=IR01+5*MPLS
                    IRW2=IR01+6*MPLS
                    NRWK=NOUT
                    RETURN
  103               NOUT=0
                    TUDN=0.
10140             CONTINUE
10134           CONTINUE
10127         CONTINUE
              IF (.NOT.((IINT.EQ.NINT.OR.NOUT.EQ.MPLS).AND.NOUT.NE.0))
     +        GO TO 10142
                XTMP=RWRK(IR01+5*MPLS+NOUT)
                YTMP=RWRK(IR01+6*MPLS+NOUT)
                IJMP=3
                IRW1=IR01+5*MPLS
                IRW2=IR01+6*MPLS
                NRWK=NOUT
                RETURN
  104           RWRK(IR01+5*MPLS+1)=XTMP
                RWRK(IR01+6*MPLS+1)=YTMP
                NOUT=1
10142         CONTINUE
            GO TO 10120
10121       CONTINUE
C
            IF (.NOT.(IFSF.NE.0)) GO TO 10143
              IFSF=0
              XBFS=RWRK(IR01+1)
              YBFS=RWRK(IR01+MPLS+1)
              CALL MSKRV2 (0.,XTMP,YTMP,NPLS,RWRK(IR01+1),
     +                     RWRK(IR01+MPLS+1),RWRK(IR01+2*MPLS+1),
     +                     RWRK(IR01+3*MPLS+1),RWRK(IR01+4*MPLS+1),
     +                     ABS(T2DS),1,SBFS)
              IF (ICFELL('CPTRCL',10).NE.0) RETURN
10143       CONTINUE
C
            XELS=RWRK(IR01+NPLS)
            YELS=RWRK(IR01+MPLS+NPLS)
            CALL MSKRV2 (1.,XTMP,YTMP,NPLS,RWRK(IR01+1),
     +                   RWRK(IR01+MPLS+1),RWRK(IR01+2*MPLS+1),
     +                   RWRK(IR01+3*MPLS+1),RWRK(IR01+4*MPLS+1),
     +                   ABS(T2DS),1,SELS)
            IF (ICFELL('CPTRCL',11).NE.0) RETURN
C
C
10114   CONTINUE
C
  105   NPLS=0
        RUDN=0.
C
C Done.
C
      GO TO (10056,10063,10082,10091,10100,10103) , L10057
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the contour line is seen.
C It checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10139 CONTINUE
        XT1D=XTOD
        YT1D=YTOD
        XT1U=XTOU
        YT1U=YTOU
        XT2D=XTND
        YT2D=YTND
        XT2U=XTNU
        YT2U=YTNU
        ITMP=0
10144   CONTINUE
          XT3D=(XT1D+XT2D)/2.
          YT3D=(YT1D+YT2D)/2.
          CALL CPMPXY (IMPF,XAT1+RZDM*(XT3D-1.),YAT1+RZDN*(YT3D-1.),
     +                                                      XT3U,YT3U)
          IF (ICFELL('CPTRCL',12).NE.0) RETURN
          IF (.NOT.(OORV.EQ.0..OR.(XT3U.NE.OORV.AND.YT3U.NE.OORV)))
     +    GO TO 10145
            DST1=ABS(XT3U-XT1U)+ABS(YT3U-YT1U)
            DST2=ABS(XT3U-XT2U)+ABS(YT3U-YT2U)
            IF (.NOT.(DST1.LT.DST2)) GO TO 10146
              IF (XT3D.EQ.XT1D.AND.YT3D.EQ.YT1D) GO TO 10147
              XT1D=XT3D
              YT1D=YT3D
              XT1U=XT3U
              YT1U=YT3U
            GO TO 10148
10146       CONTINUE
              IF (XT3D.EQ.XT2D.AND.YT3D.EQ.YT2D) GO TO 10147
              XT2D=XT3D
              YT2D=YT3D
              XT2U=XT3U
              YT2U=YT3U
10148       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10147
          GO TO 10149
10145     CONTINUE
            XTVD=XTOD
            YTVD=YTOD
            XTVU=XTOU
            YTVU=YTOU
            XTID=XT3D
            YTID=YT3D
            L10133=    3
            GO TO 10133
10150       CONTINUE
            IJMP=4
            IRW1=IR01+5*MPLS
            IRW2=IR01+6*MPLS
            NRWK=NOUT
            RETURN
  106       NOUT=0
            TUDN=0.
            XTID=XT3D
            YTID=YT3D
            XTVD=XTND
            YTVD=YTND
            XTVU=XTNU
            YTVU=YTNU
            L10133=    4
            GO TO 10133
10151       CONTINUE
            ITMP=1000
            GO TO 10147
10149     CONTINUE
        GO TO 10144
10147   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XT1U-XT2U).GT.SMLX.OR.ABS(YT1U-Y
     +T2U).GT.SMLY))) GO TO 10152
          NOUT=NOUT+1
          RWRK(IR01+5*MPLS+NOUT)=XT1U
          RWRK(IR01+6*MPLS+NOUT)=YT1U
          IJMP=5
          IRW1=IR01+5*MPLS
          IRW2=IR01+6*MPLS
          NRWK=NOUT
          RETURN
  107     NOUT=1
          RWRK(IR01+5*MPLS+1)=XT2U
          RWRK(IR01+6*MPLS+1)=YT2U
          TUDN=0.
10152   CONTINUE
      GO TO (10138) , L10139
C
C Given two points in the unmapped user coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10133 CONTINUE
        ITMP=0
10153   CONTINUE
          XTHD=(XTVD+XTID)/2.
          YTHD=(YTVD+YTID)/2.
          CALL CPMPXY (IMPF,XTHD,YTHD,XTHU,YTHU)
          IF (ICFELL('CPTRCL',13).NE.0) RETURN
          IF (.NOT.(XTHU.NE.OORV.AND.YTHU.NE.OORV)) GO TO 10154
            IF (XTHD.EQ.XTVD.AND.YTHD.EQ.YTVD) GO TO 10155
            XTVD=XTHD
            YTVD=YTHD
            XTVU=XTHU
            YTVU=YTHU
          GO TO 10156
10154     CONTINUE
            IF (XTHD.EQ.XTID.AND.YTHD.EQ.YTID) GO TO 10155
            XTID=XTHD
            YTID=YTHD
10156     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10155
        GO TO 10153
10155   CONTINUE
        NOUT=NOUT+1
        RWRK(IR01+5*MPLS+NOUT)=XTVU
        RWRK(IR01+6*MPLS+NOUT)=YTVU
      GO TO (10132,10141,10150,10151) , L10133
C
      END
