C
C $Id: cpcldm.f,v 1.7 1995-04-26 22:44:33 kennison Exp $
C
      SUBROUTINE CPCLDM (ZDAT,RWRK,IWRK,IAMA,RTPL)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*),IAMA(*)
C
C This routine draws contour lines masked by an existing area map.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C IAMA is the user's area map.
C
C RTPL is the routine which is to process segments of the contour line.
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
C Declare the dash-package common block which contains the smoothing
C flag, so that it may be temporarily turned off as needed.
C
      COMMON /SMFLAG/ ISMO
C
C Declare local variables in which to manipulate DASHPACK parameters.
C
      CHARACTER*1 CHRB,CHRG,CHRS
      CHARACTER*16 CDPS
      CHARACTER*256 CHDP
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPCLDM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPCLDM - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CPCLDM',3).NE.0) RETURN
C
C If no contour levels are defined, try to pick a set of levels.
C
      IF (NCLV.LE.0) THEN
        CALL CPPKCL (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPCLDM',4).NE.0) RETURN
      END IF
C
C Get indices for the contour levels in ascending order.
C
      IF (NCLV.GT.0) CALL CPSORT (CLEV,NCLV,ICLP)
C
C Get real and integer workspaces to use in the calls to ARDRLN.
C
      CALL CPGRWS (RWRK,2,2*LRWM,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CPCLDM',5).NE.0) RETURN
C
      CALL CPGIWS (IWRK,2,2*LIWM,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CPCLDM',6).NE.0) RETURN
C
C Initialize whichever dash package (if any) is to be used.
C
      IF (IDUF.LT.0) THEN
C
        CALL DPGETC ('CRB',CHRB)
        IF (ICFELL('CPCLDM',7).NE.0) RETURN
        CALL DPGETC ('CRG',CHRG)
        IF (ICFELL('CPCLDM',8).NE.0) RETURN
        CALL DPGETC ('CRS',CHRS)
        IF (ICFELL('CPCLDM',9).NE.0) RETURN
        CALL DPGETI ('DPL',IDPL)
        IF (ICFELL('CPCLDM',10).NE.0) RETURN
        CALL DPGETI ('DPS',IDPS)
        IF (ICFELL('CPCLDM',11).NE.0) RETURN
        CALL DPGETC ('DPT',CHDP)
        IF (ICFELL('CPCLDM',12).NE.0) RETURN
        CALL DPGETR ('TCS',RTCS)
        IF (ICFELL('CPCLDM',13).NE.0) RETURN
        CALL DPGETR ('WOC',RWOC)
        IF (ICFELL('CPCLDM',14).NE.0) RETURN
        CALL DPGETR ('WOG',RWOG)
        IF (ICFELL('CPCLDM',15).NE.0) RETURN
        CALL DPGETR ('WOS',RWOS)
        IF (ICFELL('CPCLDM',16).NE.0) RETURN
C
        CALL DPSETI ('DPS',0)
        IF (ICFELL('CPCLDM',17).NE.0) RETURN
        CDPS=CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//
     +       CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//CHRS
        CALL DPSETC ('DPT',CDPS)
        IF (ICFELL('CPCLDM',18).NE.0) RETURN
        CALL DPSETR ('TCS',-1.)
        IF (ICFELL('CPCLDM',19).NE.0) RETURN
        CALL DPSETR ('WOC',CHWM*WOCH*(XVPR-XVPL))
        IF (ICFELL('CPCLDM',20).NE.0) RETURN
        CALL DPSETR ('WOG',CHWM*WODA*(XVPR-XVPL))
        IF (ICFELL('CPCLDM',21).NE.0) RETURN
        CALL DPSETR ('WOS',CHWM*WODA*(XVPR-XVPL))
        IF (ICFELL('CPCLDM',22).NE.0) RETURN
C
      ELSE IF (IDUF.GT.0) THEN
C
        CALL GETSI (IP2X,IP2Y)
        IF (ICFELL('CPCLDM',23).NE.0) RETURN
        ILDA=MAX(1,INT(CHWM*WODA*(XVPR-XVPL)*(2.**IP2X-1.)+.5))
        ILCH=MAX(4,INT(CHWM*WOCH*(XVPR-XVPL)*(2.**IP2X-1.)+.5))
        CALL DASHDC ('$$$$$$$$$$$$$$$$',ILDA,ILCH)
        IF (ICFELL('CPCLDM',24).NE.0) RETURN
        ISMS=ISMO
        ISMO=1
C
      END IF
C
C If the constant-field flag is set, just output a warning message.
C
      IF (.NOT.(ICFF.NE.0)) GO TO 10001
C
        CALL CPCFLB (1,RWRK,IWRK)
        IF (ICFELL('CPCLDM',25).NE.0) RETURN
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
          IF (ICFELL('CPCLDM',26).NE.0) RETURN
          CALL CPSTLS (ZDAT,RWRK,IWRK)
          IF (ICFELL('CPCLDM',27).NE.0) RETURN
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
C If only the line is being drawn, the dash-pattern-use flag determines
C whether it will be done using CURVE, DPCURV, or CURVED.
C
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.1)) GO TO 10011
C
              IF (.NOT.(IDUF.LT.0)) GO TO 10012
                CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
                IF (ICFELL('CPCLDM',28).NE.0) RETURN
              GO TO 10013
10012         CONTINUE
              IF (.NOT.(IDUF.GT.0)) GO TO 10014
                CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
                IF (ICFELL('CPCLDM',29).NE.0) RETURN
10013         CONTINUE
10014         CONTINUE
C
              L10016=    1
              GO TO 10016
10015         CONTINUE
C
C If only the labels are being drawn, it can be handled here only if
C the dash-pattern use flag indicates that DPCURV or CURVED is to be
C used and the label-positioning flag implies that the labels are to
C be incorporated into the dash pattern.
C
            GO TO 10017
10011       CONTINUE
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.2)) GO TO 10018
C
              IF (.NOT.(ABS(IPLL).EQ.1.AND.IDUF.NE.0)) GO TO 10019
                NCHL=NCLB(ICLV)
                NCHD=MAX(1,MIN(ABS(IDUF)*LCLD,500-NCHL))
                CTMA=' '
                IF (.NOT.(IDUF.LT.0)) GO TO 10020
                  DO 10021 ICHD=1,NCHD
                    CTMA(ICHD:ICHD)=CHRG
10021             CONTINUE
                  LCTM=NCHD+NCHL
                  CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                  CALL DPSETC ('DPT',CTMA(1:LCTM))
                  IF (ICFELL('CPCLDM',30).NE.0) RETURN
                GO TO 10022
10020           CONTINUE
                IF (.NOT.(IDUF.GT.0)) GO TO 10023
                  DO 10024 ICHD=1,NCHD
                    CTMA(ICHD:ICHD)=''''
10024             CONTINUE
                  LCTM=NCHD+NCHL
                  CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                  CALL DASHDC (CTMA(1:LCTM),ILDA,ILCH)
                  IF (ICFELL('CPCLDM',31).NE.0) RETURN
10022           CONTINUE
10023           CONTINUE
                L10016=    2
                GO TO 10016
10025           CONTINUE
10019         CONTINUE
C
C If both lines and labels are being drawn, there are various cases,
C depending on whether dashed lines are being used and how labels are
C being positioned.
C
            GO TO 10017
10018       CONTINUE
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.3)) GO TO 10026
C
              IF (.NOT.(IDUF.NE.0)) GO TO 10027
                IF (.NOT.(ABS(IPLL).EQ.1)) GO TO 10028
                  NCHL=NCLB(ICLV)
                  NCHD=MAX(1,MIN(ABS(IDUF)*LCLD,500-NCHL))
                  CTMA=' '
                  DO 10029 ICHD=1,NCHD
                    JCHD=MOD(ICHD-1,LCLD)+1
                    CTMA(ICHD:ICHD)=CLDP(ICLV)(JCHD:JCHD)
10029             CONTINUE
                  IF (.NOT.(IDUF.LT.0)) GO TO 10030
                    LCTM=NCHD+NCHL
                    CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                    CALL DPSETC ('DPT',CTMA(1:LCTM))
                    IF (ICFELL('CPCLDM',32).NE.0) RETURN
                  GO TO 10031
10030             CONTINUE
                    LCTM=NCHD+NCHL
                    CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                    CALL DASHDC (CTMA(1:LCTM),ILDA,ILCH)
                    IF (ICFELL('CPCLDM',33).NE.0) RETURN
10031             CONTINUE
                GO TO 10032
10028           CONTINUE
                  IF (.NOT.(IDUF.LT.0)) GO TO 10033
                    CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
                    IF (ICFELL('CPCLDM',34).NE.0) RETURN
                  GO TO 10034
10033             CONTINUE
                    CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
                    IF (ICFELL('CPCLDM',35).NE.0) RETURN
10034             CONTINUE
10032           CONTINUE
10027         CONTINUE
C
              L10016=    3
              GO TO 10016
10035         CONTINUE
C
10017       CONTINUE
10026       CONTINUE
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
      IF (.NOT.(ICLU(258).NE.0)) GO TO 10036
        ICLV=258
        IF (.NOT.(IDUF.NE.0)) GO TO 10037
          L10010=    2
          GO TO 10010
10038     CONTINUE
          IF (.NOT.(IDUF.LT.0)) GO TO 10039
            CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
            IF (ICFELL('CPCLDM',36).NE.0) RETURN
          GO TO 10040
10039     CONTINUE
            CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
            IF (ICFELL('CPCLDM',37).NE.0) RETURN
10040     CONTINUE
10037   CONTINUE
        L10042=    1
        GO TO 10042
10041   CONTINUE
        IJMP=0
        IAIC=-9
10043   CONTINUE
          CALL CPTRES (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDM',38).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10044
          CALL ARDRLN (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,
     +                 RWRK(IR02+1),RWRK(IR02+1+LRWM),LRWM,
     +                 IWRK(II02+1),IWRK(II02+1+LIWM),LIWM,RTPL)
          IF (ICFELL('CPCLDM',39).NE.0) RETURN
        GO TO 10043
10044   CONTINUE
        L10046=    1
        GO TO 10046
10045   CONTINUE
10036 CONTINUE
C
C Draw boundaries for areas which are invisible.
C
      IF (.NOT.(ICLU(259).NE.0.AND.IMPF.NE.0.AND.OORV.NE.0.))
     +GO TO 10047
        TST1=REAL(IMPF)
        TST2=0.
        CALL HLUCPMPXY (0,TST1,TST2,TST3,TST4)
        IF (ICFELL('CPCLDM',40).NE.0) RETURN
        ICLV=259
        IF (.NOT.(IDUF.NE.0)) GO TO 10048
          L10010=    3
          GO TO 10010
10049     CONTINUE
          IF (.NOT.(IDUF.LT.0)) GO TO 10050
            CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
            IF (ICFELL('CPCLDM',41).NE.0) RETURN
          GO TO 10051
10050     CONTINUE
            CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
            IF (ICFELL('CPCLDM',42).NE.0) RETURN
10051     CONTINUE
10048   CONTINUE
        L10042=    2
        GO TO 10042
10052   CONTINUE
        IJMP=0
        IAIC=-9
10053   CONTINUE
          IF (.NOT.(TST2.NE.2..AND.TST2.NE.3.)) GO TO 10054
            CALL CPTREV (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
            IF (ICFELL('CPCLDM',43).NE.0) RETURN
          GO TO 10055
10054     CONTINUE
            CALL CPTRVE (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
            IF (ICFELL('CPCLDM',44).NE.0) RETURN
10055     CONTINUE
          IF (IJMP.EQ.0) GO TO 10056
          CALL ARDRLN (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,
     +                 RWRK(IR02+1),RWRK(IR02+1+LRWM),LRWM,
     +                 IWRK(II02+1),IWRK(II02+1+LIWM),LIWM,RTPL)
          IF (ICFELL('CPCLDM',45).NE.0) RETURN
        GO TO 10053
10056   CONTINUE
        L10046=    2
        GO TO 10046
10057   CONTINUE
10047 CONTINUE
C
C Draw the edge of the grid.
C
      IF (.NOT.(ICLU(257).NE.0)) GO TO 10058
        ICLV=257
        IF (.NOT.(IDUF.NE.0)) GO TO 10059
          L10010=    4
          GO TO 10010
10060     CONTINUE
          IF (.NOT.(IDUF.LT.0)) GO TO 10061
            CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
            IF (ICFELL('CPCLDM',46).NE.0) RETURN
          GO TO 10062
10061     CONTINUE
            CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
            IF (ICFELL('CPCLDM',47).NE.0) RETURN
10062     CONTINUE
10059   CONTINUE
        L10042=    3
        GO TO 10042
10063   CONTINUE
        IJMP=0
        IAIC=-9
10064   CONTINUE
          CALL CPTREG (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDM',48).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10065
          CALL ARDRLN (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,
     +                 RWRK(IR02+1),RWRK(IR02+1+LRWM),LRWM,
     +                 IWRK(II02+1),IWRK(II02+1+LIWM),LIWM,RTPL)
          IF (ICFELL('CPCLDM',49).NE.0) RETURN
        GO TO 10064
10065   CONTINUE
        L10046=    3
        GO TO 10046
10066   CONTINUE
10058 CONTINUE
C
C Restore the state of the dash package (if any) that was used.
C
      IF (IDUF.LT.0) THEN
C
        CALL DPSETI ('DPS',IDPS)
        IF (ICFELL('CPCLDM',50).NE.0) RETURN
        CALL DPSETC ('DPT',CHDP(1:IDPL))
        IF (ICFELL('CPCLDM',51).NE.0) RETURN
        CALL DPSETR ('TCS',RTCS)
        IF (ICFELL('CPCLDM',52).NE.0) RETURN
        CALL DPSETR ('WOC',RWOC)
        IF (ICFELL('CPCLDM',53).NE.0) RETURN
        CALL DPSETR ('WOG',RWOG)
        IF (ICFELL('CPCLDM',54).NE.0) RETURN
        CALL DPSETR ('WOS',RWOS)
        IF (ICFELL('CPCLDM',55).NE.0) RETURN
C
      ELSE IF (IDUF.GT.0) THEN
C
        CALL DASHDC ('$$$$$$$$$$$$$$$$',ILDA,ILCH)
        IF (ICFELL('CPCLDM',56).NE.0) RETURN
        ISMO=ISMS
C
      END IF
C
C Release the workspaces used in the calls to ARDRLN.
C
      LR02=0
      LI02=0
C
C Done.
C
      RETURN
C
C The following internal procedure finds the length of a dash pattern.
C
10010 CONTINUE
        LCLD=1
        DO 10067 I=1,128
          IF (CLDP(ICLV)(I:I).NE.' ') LCLD=I
10067   CONTINUE
      GO TO (10009,10038,10049,10060) , L10010
C
C The following internal procedure calls CPTRCL to draw the contour
C line at a given level.  The user-change routine is called before
C and after the calls to CPTRCL.
C
10016 CONTINUE
        L10042=    4
        GO TO 10042
10068   CONTINUE
        IJMP=0
10069   CONTINUE
          CALL CPTRCL (ZDAT,RWRK,IWRK,CLEV(ICLV),IJMP,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLDM',57).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10070
          CALL ARDRLN (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,
     +                 RWRK(IR02+1),RWRK(IR02+1+LRWM),LRWM,
     +                 IWRK(II02+1),IWRK(II02+1+LIWM),LIWM,RTPL)
          IF (ICFELL('CPCLDM',58).NE.0) RETURN
          IF (IHCF.NE.0) THEN
            CALL CPHCHM (RWRK,IRW1,IRW2,NRWK,IAMA,IWRK,RTPL)
            IF (ICFELL('CPCLDM',59).NE.0) RETURN
          END IF
        GO TO 10069
10070   CONTINUE
        L10046=    4
        GO TO 10046
10071   CONTINUE
      GO TO (10015,10025,10035) , L10016
C
C The following internal procedures set and reset line color and width
C before and after a particular line is drawn.
C
10042 CONTINUE
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('CPCLDM',60).NE.0) RETURN
        JCCL=ICCL(ICLV)
        IF (JCCL.GE.0) THEN
          CALL GQPLCI (IGER,ISLC)
          IF (IGER.NE.0) THEN
            CALL SETER ('CPCLDM - ERROR EXIT FROM GQPLCI',61,1)
            RETURN
          END IF
          CALL GQTXCI (IGER,ISTC)
          IF (IGER.NE.0) THEN
            CALL SETER ('CPCLDM - ERROR EXIT FROM GQTXCI',62,1)
            RETURN
          END IF
          CALL GSPLCI (JCCL)
          CALL GSTXCI (JCCL)
        END IF
        CLWS=CLWA(ICLV)
        IF (CLWS.GT.0.) THEN
          CALL GQLWSC (IGER,SFLW)
          IF (IGER.NE.0) THEN
            CALL SETER ('CPCLDM - ERROR EXIT FROM GQLWSC',63,1)
            RETURN
          END IF
          CALL GSLWSC (CLWS)
        END IF
        IPAI=ICLV
        IF (IPAI.GT.256) IPAI=256-IPAI
        CALL HLUCPCHCL (+1)
        IF (ICFELL('CPCLDM',64).NE.0) RETURN
      GO TO (10041,10052,10063,10068) , L10042
C
10046 CONTINUE
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('CPCLDM',65).NE.0) RETURN
        IPAI=ICLV
        IF (IPAI.GT.256) IPAI=256-IPAI
        CALL HLUCPCHCL (-1)
        IF (ICFELL('CPCLDM',66).NE.0) RETURN
        IF (JCCL.GE.0) THEN
          CALL GSPLCI (ISLC)
          CALL GSTXCI (ISTC)
        END IF
        IF (CLWS.GT.0.) THEN
          CALL GSLWSC (SFLW)
        END IF
      GO TO (10045,10057,10066,10071) , L10046
C
      END
