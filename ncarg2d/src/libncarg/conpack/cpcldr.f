C
C $Id: cpcldr.f,v 1.2 1994-03-17 01:50:37 kennison Exp $
C
      SUBROUTINE CPCLDR (ZDAT,RWRK,IWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C This routine draws contour lines.
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
C Declare the dash-package common block which contains the smoothing
C flag, so that it may be temporarily turned off as needed.
C
      COMMON /SMFLAG/ ISMO
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPCLDR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPCLDR - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CPCLDR',3).NE.0) RETURN
C
C If no contour levels are defined, try to pick a set of levels.
C
      IF (NCLV.LE.0) THEN
        CALL CPPKCL (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPCLDR',4).NE.0) RETURN
      END IF
C
C Get indices for the contour levels in ascending order.
C
      IF (NCLV.GT.0) CALL CPSORT (CLEV,NCLV,ICLP)
C
C If CURVED is to be used, compute ILDA, which is the curve length,
C in plotter address units, per dollar sign or apostrophe in a dash
C pattern, and ILCH, which is the curve length, in plotter address
C units, per other user character in a dash pattern, initialize to
C an all-solid pattern, and turn off the dash-package smoother.
C
      IF (IDUF.NE.0) THEN
        CALL GETSI (IP2X,IP2Y)
        IF (ICFELL('CPCLDR',5).NE.0) RETURN
        ILDA=MAX(1,INT(CHWM*WODA*(XVPR-XVPL)*(2.**IP2X-1.)+.5))
        ILCH=MAX(4,INT(CHWM*WOCH*(XVPR-XVPL)*(2.**IP2X-1.)+.5))
        CALL DASHDC ('$$$$$$$$$$$$$$$$',ILDA,ILCH)
        IF (ICFELL('CPCLDR',6).NE.0) RETURN
        ISMS=ISMO
        ISMO=1
      END IF
C
C If the constant-field flag is set, just output a warning message.
C
      IF (.NOT.(ICFF.NE.0)) GO TO 10001
C
        CALL CPCFLB (1,RWRK,IWRK)
        IF (ICFELL('CPCLDR',7).NE.0) RETURN
C
C Otherwise, draw contours.
C
      GO TO 10002
10001 CONTINUE
C
C If labels are being written by the dash package, make sure the labels
C are completely defined.
C
        IF (.NOT.(ABS(IPLL).EQ.1)) GO TO 10003
          CALL CPPKLB (ZDAT,RWRK,IWRK)
          IF (ICFELL('CPCLDR',8).NE.0) RETURN
          CALL CPSTLS (ZDAT,RWRK,IWRK)
          IF (ICFELL('CPCLDR',9).NE.0) RETURN
10003   CONTINUE
C
C Loop through the selected contour levels, drawing contour lines for
C the appropriate ones.
C
          ICLV = 1
          GO TO 10006
10004     CONTINUE
          ICLV =ICLV +1
10006     CONTINUE
          IF (ICLV .GT.(NCLV)) GO TO 10005
C
          IF (.NOT.(CLEV(ICLV).GT.ZMIN.AND.CLEV(ICLV).LT.ZMAX)) GO TO 10
     +007
C
C If dash patterns are in use, find the length of the dash pattern at
C this contour level.
C
            IF (.NOT.(IDUF.NE.0)) GO TO 10008
              L10010=    1
              GO TO 10010
10009         CONTINUE
10008       CONTINUE
C
C If only the line is being drawn, the dash-pattern use flag determines
C whether it will be done using CURVE or CURVED.
C
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.1)) GO TO 10011
C
              IF (.NOT.(IDUF.NE.0)) GO TO 10012
                CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
                IF (ICFELL('CPCLDR',10).NE.0) RETURN
10012         CONTINUE
C
              L10014=    1
              GO TO 10014
10013         CONTINUE
C
C If only the labels are being drawn, it can only be handled here if
C the dash-pattern flag indicates that CURVED is to be used and the
C label-positioning flag implies that it is to draw them.
C
            GO TO 10015
10011       CONTINUE
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.2)) GO TO 10016
C
              IF (.NOT.(IDUF.NE.0.AND.ABS(IPLL).EQ.1)) GO TO 10017
                NCHL=NCLB(ICLV)
                NCHD=MAX(1,MIN(IDUF*LCLD,500-NCHL))
                CTMA=' '
                DO 10018 ICHD=1,NCHD
                  CTMA(ICHD:ICHD)=''''
10018           CONTINUE
                LCTM=NCHD+NCHL
                CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                CALL DASHDC (CTMA(1:LCTM),ILDA,ILCH)
                IF (ICFELL('CPCLDR',11).NE.0) RETURN
                L10014=    2
                GO TO 10014
10019           CONTINUE
10017         CONTINUE
C
C If both lines and labels are being drawn, there are various cases,
C depending on whether the dash package is being used and how labels
C are being positioned.
C
            GO TO 10015
10016       CONTINUE
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.3)) GO TO 10020
C
              IF (.NOT.(IDUF.NE.0)) GO TO 10021
                IF (.NOT.(ABS(IPLL).EQ.1)) GO TO 10022
                  NCHL=NCLB(ICLV)
                  NCHD=MAX(1,MIN(IDUF*LCLD,500-NCHL))
                  CTMA=' '
                  DO 10023 ICHD=1,NCHD
                    JCHD=MOD(ICHD-1,LCLD)+1
                    CTMA(ICHD:ICHD)=CLDP(ICLV)(JCHD:JCHD)
10023             CONTINUE
                  LCTM=NCHD+NCHL
                  CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                  CALL DASHDC (CTMA(1:LCTM),ILDA,ILCH)
                  IF (ICFELL('CPCLDR',12).NE.0) RETURN
                GO TO 10024
10022           CONTINUE
                  CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
                  IF (ICFELL('CPCLDR',13).NE.0) RETURN
10024           CONTINUE
10021         CONTINUE
C
              L10014=    3
              GO TO 10014
10025         CONTINUE
C
10015       CONTINUE
10020       CONTINUE
C
10007     CONTINUE
C
        GO TO 10004
10005   CONTINUE
C
10002 CONTINUE
C
C Draw boundaries for areas filled with special values.
C
      IF (.NOT.(ICLU(258).NE.0)) GO TO 10026
        ICLV=258
        IF (.NOT.(IDUF.NE.0)) GO TO 10027
          L10010=    2
          GO TO 10010
10028     CONTINUE
          CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
          IF (ICFELL('CPCLDR',14).NE.0) RETURN
10027   CONTINUE
        L10030=    1
        GO TO 10030
10029   CONTINUE
        IJMP=0
10031   CONTINUE
          CALL CPTRES (ZDAT,RWRK,IWRK,IJMP,-9,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDR',15).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10032
          CALL CPDRSG (RWRK,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDR',16).NE.0) RETURN
        GO TO 10031
10032   CONTINUE
        L10034=    1
        GO TO 10034
10033   CONTINUE
10026 CONTINUE
C
C Draw boundaries for areas which are invisible.
C
      IF (.NOT.(ICLU(259).NE.0.AND.IMPF.NE.0.AND.OORV.NE.0.))
     +GO TO 10035
        TST1=REAL(IMPF)
        TST2=0.
        CALL CPMPXY (0,TST1,TST2,TST3,TST4)
        IF (ICFELL('CPCLDR',17).NE.0) RETURN
        ICLV=259
        IF (.NOT.(IDUF.NE.0)) GO TO 10036
          L10010=    3
          GO TO 10010
10037     CONTINUE
          CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
          IF (ICFELL('CPCLDR',18).NE.0) RETURN
10036   CONTINUE
        L10030=    2
        GO TO 10030
10038   CONTINUE
        IJMP=0
10039   CONTINUE
          IF (.NOT.(TST2.NE.2..AND.TST2.NE.3.)) GO TO 10040
            CALL CPTREV (ZDAT,RWRK,IWRK,IJMP,-9,IRW1,IRW2,NRWK)
            IF (ICFELL('CPCLDR',19).NE.0) RETURN
          GO TO 10041
10040     CONTINUE
            CALL CPTRVE (ZDAT,RWRK,IWRK,IJMP,-9,IRW1,IRW2,NRWK)
            IF (ICFELL('CPCLDR',20).NE.0) RETURN
10041     CONTINUE
          IF (IJMP.EQ.0) GO TO 10042
          CALL CPDRSG (RWRK,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDR',21).NE.0) RETURN
        GO TO 10039
10042   CONTINUE
        L10034=    2
        GO TO 10034
10043   CONTINUE
10035 CONTINUE
C
C Draw the edge of the grid.
C
      IF (.NOT.(ICLU(257).NE.0)) GO TO 10044
        ICLV=257
        IF (.NOT.(IDUF.NE.0)) GO TO 10045
          L10010=    4
          GO TO 10010
10046     CONTINUE
          CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
          IF (ICFELL('CPCLDR',22).NE.0) RETURN
10045   CONTINUE
        L10030=    3
        GO TO 10030
10047   CONTINUE
        IJMP=0
10048   CONTINUE
          CALL CPTREG (ZDAT,RWRK,IWRK,IJMP,-9,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDR',23).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10049
          CALL CPDRSG (RWRK,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDR',24).NE.0) RETURN
        GO TO 10048
10049   CONTINUE
        L10034=    3
        GO TO 10034
10050   CONTINUE
10044 CONTINUE
C
C If CURVED is being used, go back to a solid pattern and restore the
C smoothing flag in the dash package to its entry value.
C
      IF (IDUF.NE.0) THEN
        CALL DASHDC ('$$$$$$$$$$$$$$$$',ILDA,ILCH)
        IF (ICFELL('CPCLDR',25).NE.0) RETURN
        ISMO=ISMS
      END IF
C
C Done.
C
      RETURN
C
C The following internal procedure finds the length of a dash pattern.
C
10010 CONTINUE
        LCLD=1
        DO 10051 I=1,128
          IF (CLDP(ICLV)(I:I).NE.' ') LCLD=I
10051   CONTINUE
      GO TO (10009,10028,10037,10046) , L10010
C
C The following internal procedure calls CPTRCL to draw the contour
C line at a given level.  The user-change routine is called before
C and after the calls to CPTRCL.
C
10014 CONTINUE
        L10030=    4
        GO TO 10030
10052   CONTINUE
        IJMP=0
10053   CONTINUE
          CALL CPTRCL (ZDAT,RWRK,IWRK,CLEV(ICLV),IJMP,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDR',26).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10054
          CALL CPDRSG (RWRK,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDR',27).NE.0) RETURN
          IF (IHCF.NE.0) THEN
            CALL CPHCHR (RWRK,IRW1,IRW2,NRWK)
            IF (ICFELL('CPCLDR',28).NE.0) RETURN
          END IF
        GO TO 10053
10054   CONTINUE
        L10034=    4
        GO TO 10034
10055   CONTINUE
      GO TO (10013,10019,10025) , L10014
C
C The following internal procedures set and reset line color and width
C before and after a particular line is drawn.
C
10030 CONTINUE
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('CPCLDR',29).NE.0) RETURN
        JCCL=ICCL(ICLV)
        IF (JCCL.GE.0) THEN
          CALL GQPLCI (IGER,ISLC)
          IF (IGER.NE.0) THEN
            CALL SETER ('CPCLDR - ERROR EXIT FROM GQPLCI',30,1)
            RETURN
          END IF
          CALL GQTXCI (IGER,ISTC)
          IF (IGER.NE.0) THEN
            CALL SETER ('CPCLDR - ERROR EXIT FROM GQTXCI',31,1)
            RETURN
          END IF
          CALL GSPLCI (JCCL)
          CALL GSTXCI (JCCL)
        END IF
        CLWS=CLWA(ICLV)
        IF (CLWS.GT.0.) THEN
          CALL GQLWSC (IGER,SFLW)
          IF (IGER.NE.0) THEN
            CALL SETER ('CPCLDR - ERROR EXIT FROM GQLWSC',32,1)
            RETURN
          END IF
          CALL GSLWSC (CLWS)
        END IF
        IPAI=ICLV
        CALL CPCHCL (+1)
        IF (ICFELL('CPCLDR',33).NE.0) RETURN
      GO TO (10029,10038,10047,10052) , L10030
C
10034 CONTINUE
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('CPCLDR',34).NE.0) RETURN
        IPAI=ICLV
        CALL CPCHCL (-1)
        IF (ICFELL('CPCLDR',35).NE.0) RETURN
        IF (JCCL.GE.0) THEN
          CALL GSPLCI (ISLC)
          CALL GSTXCI (ISTC)
        END IF
        IF (CLWS.GT.0.) THEN
          CALL GSLWSC (SFLW)
        END IF
      GO TO (10033,10043,10050,10055) , L10034
C
      END
