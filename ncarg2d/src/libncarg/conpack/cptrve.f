C
C $Id: cptrve.f,v 1.2 1994-03-17 01:52:30 kennison Exp $
C
      SUBROUTINE CPTRVE (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C This routine traces the edge of the area which is visible under the
C current mapping, using the inverse mapping capabilities of CPMPXY.
C This routine works better than CPTREV for many mappings of interest
C (but, of course, it depends on the inverse mapping being available).
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
C caller is expected to process a segment and recall CPTRVE.
C
C IAIC is both an input and an output variable.  If it is initially set
C to -9 by the caller, it will not be changed by CPTRVE and no attempt
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
C Because of the way this routine is entered and re-entered, we need to
C save every variable it uses.
C
      SAVE
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (103,107) , IJMP
C
C Assign space to use for storing the X and Y coordinates of points.
C
      MPLS=LRWC
      CALL CPGRWS (RWRK,1,2*MPLS,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CPTRVE',1).NE.0) RETURN
C
C Compute required constants.  By default, we work with a grid of
C approximately 2500 boxes in the current viewport; each of the boxes
C is roughly square.  The user may set the value of 'PIE' non-zero to
C increase the number of boxes used.
C
      IIDM=MAX(2,INT(SQRT(2500.*(XVPR-XVPL)/(YVPT-YVPB))))
      IIDN=MAX(2,INT(SQRT(2500.*(YVPT-YVPB)/(XVPR-XVPL))))
C
      IIDM=(IIDM-1)*(ABS(IPIE)+1)+1
      IIDN=(IIDN-1)*(ABS(IPIE)+1)+1
C
      RIDM=(XVPR-XVPL)/REAL(IIDM-1)
      RIDN=(YVPT-YVPB)/REAL(IIDN-1)
C
C Zero the count of horizontal segments seen.
C
      NHSS=0
C
C Define the first search point.
C
      IVBX=1
      IVBY=1
      XDUM=CFUX(XVPL)
      IF (ICFELL('CPTRVE',2).NE.0) RETURN
      YDUM=CFUY(YVPB)
      IF (ICFELL('CPTRVE',3).NE.0) RETURN
      CALL CPMPXY (-IMPF,XDUM,YDUM,XPRN,YPRN)
      IF (ICFELL('CPTRVE',4).NE.0) RETURN
C
C Search the viewport for pieces of the visible/invisible edge.  We
C first search the edges of the viewport for open-ended pieces and
C then we search the interior of the viewport for pieces that are
C closed loops.  The common variable IOCF is used to indicate which
C type of piece we are dealing with.  Its value will be modified by
C FOLLOW-THE-LIMB to provide the calling routine with even more
C information about the pieces returned (whether or not the first
C point and the last point of the piece is included in the buffer
C load being returned); this information is passed to CPTROE by the
C routine CPCLAM.
C
10001 CONTINUE
      IF (.NOT.(IVBX.LT.IIDM)) GO TO 10002
        IVBX=IVBX+1
        XPRP=XPRN
        XDUM=CFUX(XVPL+RIDM*REAL(IVBX-1))
        IF (ICFELL('CPTRVE',5).NE.0) RETURN
        YDUM=CFUY(YVPB)
        IF (ICFELL('CPTRVE',6).NE.0) RETURN
        CALL CPMPXY (-IMPF,XDUM,YDUM,XPRN,YPRN)
        IF (ICFELL('CPTRVE',7).NE.0) RETURN
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10003
          IOCF=0
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
        XDUM=CFUX(XVPL+RIDM*REAL(IIDM-1))
        IF (ICFELL('CPTRVE',8).NE.0) RETURN
        YDUM=CFUY(YVPB+RIDN*REAL(IVBY-1))
        IF (ICFELL('CPTRVE',9).NE.0) RETURN
        CALL CPMPXY (-IMPF,XDUM,YDUM,XPRN,YPRN)
        IF (ICFELL('CPTRVE',10).NE.0) RETURN
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10008
          IOCF=0
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
        XDUM=CFUX(XVPL+RIDM*REAL(IVBX-1))
        IF (ICFELL('CPTRVE',11).NE.0) RETURN
        YDUM=CFUY(YVPB+RIDN*REAL(IIDN-1))
        IF (ICFELL('CPTRVE',12).NE.0) RETURN
        CALL CPMPXY (-IMPF,XDUM,YDUM,XPRN,YPRN)
        IF (ICFELL('CPTRVE',13).NE.0) RETURN
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10012
          IOCF=0
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
        XDUM=CFUX(XVPL)
        IF (ICFELL('CPTRVE',14).NE.0) RETURN
        YDUM=CFUY(YVPB+RIDN*REAL(IVBY-1))
        IF (ICFELL('CPTRVE',15).NE.0) RETURN
        CALL CPMPXY (-IMPF,XDUM,YDUM,XPRN,YPRN)
        IF (ICFELL('CPTRVE',16).NE.0) RETURN
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10016
          IOCF=0
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
        XDUM=CFUX(XVPL)
        IF (ICFELL('CPTRVE',17).NE.0) RETURN
        RVBY=CFUY(YVPB+RIDN*REAL(IVBY-1))
        IF (ICFELL('CPTRVE',18).NE.0) RETURN
        CALL CPMPXY (-IMPF,XDUM,RVBY,XPRN,YPRN)
        IF (ICFELL('CPTRVE',19).NE.0) RETURN
          IVBX = 2
          GO TO 10023
10021     CONTINUE
          IVBX =IVBX +1
10023     CONTINUE
          IF (IVBX .GT.(IIDM)) GO TO 10022
          XPRP=XPRN
          XDUM=CFUX(XVPL+RIDM*REAL(IVBX-1))
          IF (ICFELL('CPTRVE',20).NE.0) RETURN
          CALL CPMPXY (-IMPF,XDUM,RVBY,XPRN,YPRN)
          IF (ICFELL('CPTRVE',21).NE.0) RETURN
          IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10024
            IPXY=IIDN*IVBX+IVBY
            DO 10025 I=1,NHSS
              IF (IPXY.EQ.IWRK(II01+I)) GO TO 101
10025       CONTINUE
            IF (.NOT.(NHSS.GE.LI01)) GO TO 10026
              CALL CPGIWS (IWRK,1,LI01+100,IWSE)
              IF (IWSE.NE.0.OR.ICFELL('CPTRVE',22).NE.0) GO TO 102
10026       CONTINUE
            NHSS=NHSS+1
            IWRK(II01+NHSS)=IPXY
            IOCF=1
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
          XDUM=CFUX(XVPL+RIDM*REAL(IVEX-1))
          IF (ICFELL('CPTRVE',23).NE.0) RETURN
          YDUM=CFUY(YVPB+RIDN*REAL(IVEY-1))
          IF (ICFELL('CPTRVE',24).NE.0) RETURN
          CALL CPMPXY (-IMPF,XDUM,YDUM,XTMP,YTMP)
          IF (ICFELL('CPTRVE',25).NE.0) RETURN
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
                IF (IWSE.NE.0.OR.ICFELL('CPTRVE',26).NE.0) GO TO 102
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
        IF (.NOT.(NPLS.NE.0)) GO TO 10038
          IJMP=1
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          IOCF=IOR(IOCF,4)
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
        XCVF=XVPL+RIDM*REAL(IVBX-1)
        YCVF=YVPB+RIDN*REAL(IVBY-1)
        XDUM=CFUX(XCVF)
        IF (ICFELL('CPTRVE',27).NE.0) RETURN
        YDUM=CFUY(YCVF)
        IF (ICFELL('CPTRVE',28).NE.0) RETURN
        CALL CPMPXY (-IMPF,XDUM,YDUM,XCVD,YCVD)
        IF (ICFELL('CPTRVE',29).NE.0) RETURN
C
        XCIF=XVPL+RIDM*REAL(IVEX-1)
        YCIF=YVPB+RIDN*REAL(IVEY-1)
C
        ITMP=0
C
10039   CONTINUE
          XCHF=(XCVF+XCIF)/2.
          YCHF=(YCVF+YCIF)/2.
          XDUM=CFUX(XCHF)
          IF (ICFELL('CPTRVE',30).NE.0) RETURN
          YDUM=CFUY(YCHF)
          IF (ICFELL('CPTRVE',31).NE.0) RETURN
          CALL CPMPXY (-IMPF,XDUM,YDUM,XCHD,YCHD)
          IF (ICFELL('CPTRVE',32).NE.0) RETURN
          IF (.NOT.(XCHD.NE.OORV)) GO TO 10040
            IF (XCHF.EQ.XCVF.AND.YCHF.EQ.YCVF) GO TO 10041
            XCVF=XCHF
            YCVF=YCHF
            XCVD=XCHD
            YCVD=YCHD
          GO TO 10042
10040     CONTINUE
            IF (XCHF.EQ.XCIF.AND.YCHF.EQ.YCIF) GO TO 10041
            XCIF=XCHF
            YCIF=YCHF
10042     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10041
        GO TO 10039
10041   CONTINUE
C
        IF (.NOT.(IAIC.NE.-9.AND.IAIC.LE.0)) GO TO 10043
          XTMP=1.+((XCVD-XAT1)/(XATM-XAT1))*REAL(IZDM-1)
          YTMP=1.+((YCVD-YAT1)/(YATN-YAT1))*REAL(IZDN-1)
          ITMP=INT(XTMP)
          JTMP=INT(YTMP)
          IF (.NOT.(ITMP.LT.1.OR.ITMP.GE.IZDM.OR.JTMP.LT.1.OR.JTMP.GE.IZ
     +DN))GO TO 10044
            IAIC=IAIA(257)
          GO TO 10045
10044     CONTINUE
            ITP1=ITMP+1
            JTP1=JTMP+1
            IF (.NOT.(SVAL.NE.0..AND.(ZDAT(ITMP,JTMP).EQ.SVAL.OR.ZDAT(IT
     +MP,JTP1).EQ.SVAL.OR.ZDAT(ITP1,JTMP).EQ.SVAL.OR.ZDAT(ITP1,JTP1).EQ.
     +SVAL))) GO TO 10046
              IAIC=IAIA(258)
            GO TO 10047
10046       CONTINUE
              IF (.NOT.(NCLV.LE.0)) GO TO 10048
                IAIC=1
              GO TO 10049
10048         CONTINUE
                IAIC=0
                XDEL=XTMP-REAL(ITMP)
                YDEL=YTMP-REAL(JTMP)
                ZINT=(1.-YDEL)*
     +               ((1.-XDEL)*ZDAT(ITMP,JTMP)+XDEL*ZDAT(ITP1,JTMP))+
     +               YDEL*
     +               ((1.-XDEL)*ZDAT(ITMP,JTP1)+XDEL*ZDAT(ITP1,JTP1))
                DO 10050 J=1,NCLV
                  JCLV=ICLP(J)
                  IF (.NOT.(ZINT.LE.CLEV(JCLV))) GO TO 10051
                    IF (.NOT.(IAIB(JCLV).NE.0)) GO TO 10052
                      IAIC=IAIB(JCLV)
                      GO TO 105
10052               CONTINUE
                      IF (J.EQ.NCLV) GO TO 104
                      IF (ZINT.NE.CLEV(JCLV).AND.
     +                    CLEV(JCLV).NE.CLEV(ICLP(J+1))) GO TO 104
10051             CONTINUE
10050           CONTINUE
  104           CONTINUE
                DO 10053 J=NCLV,1,-1
                  JCLV=ICLP(J)
                  IF (.NOT.(ZINT.GE.CLEV(JCLV))) GO TO 10054
                    IF (.NOT.(IAIA(JCLV).NE.0)) GO TO 10055
                      IAIC=IAIA(JCLV)
                      GO TO 105
10055               CONTINUE
                      IF (J.EQ.1) GO TO 105
                      IF (ZINT.NE.CLEV(JCLV).AND.
     +                    CLEV(JCLV).NE.CLEV(ICLP(J-1))) GO TO 105
10054             CONTINUE
10053           CONTINUE
  105         CONTINUE
10049         CONTINUE
10047       CONTINUE
10045     CONTINUE
10043   CONTINUE
C
        IF (.NOT.(NPLS.NE.0)) GO TO 10056
          XDUM=CUFX(RWRK(IR01+     NPLS))
          IF (ICFELL('CPTRVE',33).NE.0) RETURN
          YDUM=CUFY(RWRK(IR01+MPLS+NPLS))
          IF (ICFELL('CPTRVE',34).NE.0) RETURN
          IF (.NOT.(ABS(XCVF-XDUM).LE..0001*ABS(XVPR-XVPL).AND.ABS(YCVF-
     +YDUM).LE..0001*ABS(YVPT-YVPB))) GO TO 10057
            IF (NPLS.EQ.1) GO TO 108
            NPLS=NPLS-1
10057     CONTINUE
10056   CONTINUE
C
        IF (.NOT.(NPLS.GE.MPLS)) GO TO 10058
          XSAV=RWRK(IR01     +NPLS)
          YSAV=RWRK(IR01+MPLS+NPLS)
          IJMP=2
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  107     IOCF=IOR(IOCF,2)
          RWRK(IR01     +1)=XSAV
          RWRK(IR01+MPLS+1)=YSAV
          NPLS=1
10058   CONTINUE
C
        NPLS=NPLS+1
        RWRK(IR01     +NPLS)=CFUX(XCVF)
        IF (ICFELL('CPTRVE',35).NE.0) RETURN
        RWRK(IR01+MPLS+NPLS)=CFUY(YCVF)
        IF (ICFELL('CPTRVE',36).NE.0) RETURN
C
  108 CONTINUE
      GO TO (10028,10035) , L10029
C
      END
