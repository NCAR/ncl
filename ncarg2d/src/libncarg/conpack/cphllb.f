C
C $Id: cphllb.f,v 1.6 1995-04-26 22:44:47 kennison Exp $
C
      SUBROUTINE CPHLLB (ZDAT,RWRK,IWRK,IACT,IAMA)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*),IAMA(*)
C
C CPHLLB generates the high and low labels for the contour field.  If
C IACT = 1, the quantities defining the labels are added to the lists
C in real workspaces 3 and 4.  If IACT = 2, the labels are plotted.  If
C IACT = 3, the label boxes are added to the area map in IAMA.
C
C A point (I,J) is defined to be a high (low) if ZDAT(I,J) is greater
C than (less than) every other point within a certain neighborhood of
C (I,J).  The neighborhood is defined by the values of the parameters
C 'HLX' and 'HLY'.
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
C Declare local arrays to hold coordinates for area fill of boxes.
C
      DIMENSION BFXC(4),BFYC(4)
C
C If the text strings for high and low labels are blank, do nothing.
C
      IF (TXHI(1:LTHI).EQ.' '.AND.TXLO(1:LTLO).EQ.' ') RETURN
C
C Compute constants needed to get from data-index coordinates to user
C coordinates.
C
      RZDM=(XATM-XAT1)/REAL(IZDM-1)
      RZDN=(YATN-YAT1)/REAL(IZDN-1)
C
C Compute the value of the angle at which the labels are written, in
C radians, and the sine and cosine of that angle.
C
      ANLB=.017453292519943*ANHL
      SALB=SIN(ANLB)
      CALB=COS(ANLB)
C
C Compute the width of a character in the fractional system and the
C width of the white space in the fractional system.
C
      WCFS=CHWM*WCHL*(XVPR-XVPL)
      WWFS=CHWM*WWHL*(XVPR-XVPL)
C
C Determine the values of IRNG and JRNG, which specify the size of the
C neighborhood used in testing for highs and lows.
C
      IF (IHLX.GT.0) THEN
        IRNG=IHLX
      ELSE
        IRNG=MAX(2,MIN(15,INT(REAL(IZDM)/8.)))
      END IF
C
      IF (IHLY.GT.0) THEN
        JRNG=IHLY
      ELSE
        JRNG=MAX(2,MIN(15,INT(REAL(IZDN)/8.)))
      END IF
C
C If necessary, set up color-index controls.
C
      IF (IACT.EQ.2) THEN
C
        CALL GQPLCI (IGER,ISLC)
        IF (IGER.NE.0) THEN
          CALL SETER ('CPHLLB - ERROR EXIT FROM GQPLCI',1,1)
          RETURN
        END IF
        CALL GQTXCI (IGER,ISTC)
        IF (IGER.NE.0) THEN
          CALL SETER ('CPHLLB - ERROR EXIT FROM GQTXCI',2,1)
          RETURN
        END IF
        CALL GQFACI (IGER,ISFC)
        IF (IGER.NE.0) THEN
          CALL SETER ('CPHLLB - ERROR EXIT FROM GQFACI',3,1)
          RETURN
        END IF
C
        IF (ICHI.GE.0) THEN
          JCHI=ICHI
        ELSE IF (ICHL.GE.0) THEN
          JCHI=ICHL
        ELSE
          JCHI=ISTC
        END IF
C
        IF (ICLO.GE.0) THEN
          JCLO=ICLO
        ELSE IF (ICHL.GE.0) THEN
          JCLO=ICHL
        ELSE
          JCLO=ISTC
        END IF
C
        IF (ILBC.GE.0) THEN
          JLBC=ILBC
        ELSE
          JLBC=ISFC
        END IF
C
        JSLC=ISLC
        JSTC=ISTC
        JSFC=ISFC
C
      END IF
C
C If necessary, make sure we have space for 10 coordinate values in
C real workspace 1.
C
      IF (IACT.EQ.3) THEN
        CALL CPGRWS (RWRK,1,10,IWSE)
        IF (IWSE.NE.0.OR.ICFELL('CPHLLB',4).NE.0) RETURN
      END IF
C
C Make PLOTCHAR compute text-extent quantities.
C
      CALL PCGETI ('TE',ITMP)
      IF (ICFELL('CPHLLB',5).NE.0) RETURN
      CALL PCSETI ('TE',1)
      IF (ICFELL('CPHLLB',6).NE.0) RETURN
C
C Line loop follows.  The complete two-dimensional test for a minimum or
C maximum of the field is only performed for points which are minima or
C maxima along some line.  Finding these candidates is made efficient by
C using a count of consecutive increases or decreases of the function
C along the line.
C
        JPNT = 2
        GO TO 10003
10001   CONTINUE
        JPNT =JPNT +1
10003   CONTINUE
        IF (JPNT .GT.(IZDN-1)) GO TO 10002
C
        ICON=IRNG-1
        IPNT=-1
        GO TO 106
C
C Loop as long as the function is increasing along the line; we seek a
C possible maximum.
C
  101   CONTINUE
10004   CONTINUE
          IPNT=IPNT+1
          IF (IPNT.GE.IZDM) GO TO 107
          ZNOW=ZNXT
          ZNXT=ZDAT(IPNT+1,JPNT)
          IF (SVAL.NE.0..AND.ZNXT.EQ.SVAL) GO TO 105
          IF (ZNXT.LT.ZNOW) GO TO 10005
          ICON=ICON+1
          IF (ZNXT.EQ.ZNOW) ICON=0
        GO TO 10004
10005   CONTINUE
C
C Function decreases at next point.  Test for maximum on line.
C
        IF (.NOT.(ICON.LT.IRNG)) GO TO 10006
C
          IBEG=MAX(1,IPNT-IRNG)
          IEND=IPNT-ICON-1
C
          DO 10007 I=IBEG,IEND
            IF (ZNOW.LE.ZDAT(I,JPNT)) GO TO 102
10007     CONTINUE
C
10006   CONTINUE
C
        IBEG=IPNT+2
        IEND=MIN(IZDM,IPNT+IRNG)
C
        DO 10008 I=IBEG,IEND
          IF (.NOT.(SVAL.NE.0..AND.ZDAT(I,JPNT).EQ.SVAL)) GO TO 10009
            IPNT=I-1
            GO TO 105
10009     CONTINUE
          IF (ZNOW.LE.ZDAT(I,JPNT)) GO TO 102
10008   CONTINUE
C
C We have a maximum on the line.  Do a two-dimensional test for a
C maximum in the field.
C
        JBEG=MAX(1,JPNT-JRNG)
        JEND=MIN(IZDN,JPNT+JRNG)
        IBEG=MAX(1,IPNT-IRNG)
        IEND=MIN(IZDM,IPNT+IRNG)
C
        DO 10010 J=JBEG,JEND
          IF (.NOT.(J.NE.JPNT)) GO TO 10011
            DO 10012 I=IBEG,IEND
              IF (ZDAT(I,J).GE.ZNOW.OR.
     +            (SVAL.NE.0..AND.ZDAT(I,J).EQ.SVAL)) GO TO 102
10012       CONTINUE
10011     CONTINUE
10010   CONTINUE
C
C We have a maximum in the field.  Process it.
C
        IF (.NOT.(TXHI(1:LTHI).NE.' ')) GO TO 10013
          IHOL=0
          L10015=    1
          GO TO 10015
10014     CONTINUE
10013   CONTINUE
C
C Start searching for a minimum.
C
  102   ICON=1
C
C Loop as long as the function is decreasing along the line.  We seek a
C possible minimum.
C
  103   CONTINUE
10016   CONTINUE
          IPNT=IPNT+1
          IF (IPNT.GE.IZDM) GO TO 107
          ZNOW=ZNXT
          ZNXT=ZDAT(IPNT+1,JPNT)
          IF (SVAL.NE.0..AND.ZNXT.EQ.SVAL) GO TO 105
          IF (ZNXT.GT.ZNOW) GO TO 10017
          ICON=ICON+1
          IF (ZNXT.EQ.ZNOW) ICON=0
        GO TO 10016
10017   CONTINUE
C
C Function increases at next point.  Test for a minimum on the line.
C
        IF (.NOT.(ICON.LT.IRNG)) GO TO 10018
C
          IBEG=MAX(1,IPNT-IRNG)
          IEND=IPNT-ICON-1
C
          DO 10019 I=IBEG,IEND
            IF (ZNOW.GE.ZDAT(I,JPNT)) GO TO 104
10019     CONTINUE
C
10018   CONTINUE
C
        IBEG=IPNT+2
        IEND=MIN(IZDM,IPNT+IRNG)
C
        DO 10020 I=IBEG,IEND
          IF (.NOT.(SVAL.NE.0..AND.ZDAT(I,JPNT).EQ.SVAL)) GO TO 10021
            IPNT=I-1
            GO TO 105
10021     CONTINUE
          IF (ZNOW.GE.ZDAT(I,JPNT)) GO TO 104
10020   CONTINUE
C
C We have a minimum on the line.  Do a two-dimensional test for a
C minimum of the field.
C
        JBEG=MAX(1,JPNT-JRNG)
        JEND=MIN(IZDN,JPNT+JRNG)
        IBEG=MAX(1,IPNT-IRNG)
        IEND=MIN(IZDM,IPNT+IRNG)
C
        DO 10022 J=JBEG,JEND
          IF (.NOT.(J.NE.JPNT)) GO TO 10023
            DO 10024 I=IBEG,IEND
              IF (ZDAT(I,J).LE.ZNOW.OR.
     +            (SVAL.NE.0..AND.ZDAT(I,J).EQ.SVAL)) GO TO 104
10024       CONTINUE
10023     CONTINUE
10022   CONTINUE
C
C We have a minimum in the field.  Process it.
C
        IF (.NOT.(TXLO(1:LTLO).NE.' ')) GO TO 10025
          IHOL=1
          L10015=    2
          GO TO 10015
10026     CONTINUE
10025   CONTINUE
C
C Start searching for a maximum.
C
  104   ICON=1
        GO TO 101
C
C Skip special values on the line.
C
  105   ICON=0
  106   IPNT=IPNT+1
        IF (IPNT.GE.IZDM-1) GO TO 107
        IF (SVAL.NE.0..AND.ZDAT(IPNT+1,JPNT).EQ.SVAL) GO TO 105
        ICON=ICON+1
        IF (ICON.LE.IRNG) GO TO 106
C
        ICON=1
        ZNXT=ZDAT(IPNT+1,JPNT)
        IF (ZDAT(IPNT,JPNT).EQ.ZNXT) ICON=0
        IF (ZDAT(IPNT,JPNT).LE.ZNXT) GO TO 101
        GO TO 103
C
  107 CONTINUE
      GO TO 10001
10002 CONTINUE
C
C Return PLOTCHAR to its default state.
C
  108 CALL PCSETI ('TE',ITMP)
      IF (ICFELL('CPHLLB',7).NE.0) RETURN
C
C If necessary, return the color indices to their default values.
C
      IF (IACT.EQ.2) THEN
        IF (JSLC.NE.ISLC) THEN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('CPHLLB',8).NE.0) RETURN
          CALL GSPLCI (ISLC)
        END IF
        IF (JSTC.NE.ISTC) CALL GSTXCI (ISTC)
        IF (JSFC.NE.ISFC) CALL GSFACI (ISFC)
      END IF
C
C If necessary, release the space used in real workspace 1.
C
      IF (IACT.EQ.3) LR01=0
C
C Done.
C
      RETURN
C
C The following internal procedure writes a high (if IHOL=0) or low (if
C IHOL=1) label, centered at the point whose subscript coordinates are
C IPNT and JPNT.
C
10015 CONTINUE
C
        XLBC=XAT1+RZDM*(REAL(IPNT)-1.)
        YLBC=YAT1+RZDN*(REAL(JPNT)-1.)
        IVIS=1
C
        IF (IMPF.NE.0) THEN
          XTMP=XLBC
          YTMP=YLBC
          CALL HLUCPMPXY (IMPF,XTMP,YTMP,XLBC,YLBC)
          IF (ICFELL('CPHLLB',9).NE.0) RETURN
          IF ((OORV.NE.0.).AND.(XLBC.EQ.OORV.OR.YLBC.EQ.OORV)) IVIS=0
        END IF
C
        IF (IVIS.NE.0) THEN
          XCLB=CUFX(XLBC)
          IF (ICFELL('CPHLLB',10).NE.0) RETURN
          YCLB=CUFY(YLBC)
          IF (ICFELL('CPHLLB',11).NE.0) RETURN
          ZDVL=ZDAT(IPNT,JPNT)
          IF (IHOL.EQ.0) THEN
            CALL CPSBST(TXHI(1:LTHI),CTMA,LCTM)
          ELSE
            CALL CPSBST(TXLO(1:LTLO),CTMA,LCTM)
          END IF
          CALL HLUCPCHHL (+1+4*IHOL)
          IF (ICFELL('CPHLLB',12).NE.0) RETURN
          CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCFS,360.,0.)
          IF (ICFELL('CPHLLB',13).NE.0) RETURN
          CALL HLUCPCHHL (-1-4*IHOL)
          IF (ICFELL('CPHLLB',14).NE.0) RETURN
          CALL PCGETR ('DL',DTOL)
          IF (ICFELL('CPHLLB',15).NE.0) RETURN
          CALL PCGETR ('DR',DTOR)
          IF (ICFELL('CPHLLB',16).NE.0) RETURN
          CALL PCGETR ('DB',DTOB)
          IF (ICFELL('CPHLLB',17).NE.0) RETURN
          CALL PCGETR ('DT',DTOT)
          IF (ICFELL('CPHLLB',18).NE.0) RETURN
          DTOL=DTOL+WWFS
          DTOR=DTOR+WWFS
          DTOB=DTOB+WWFS
          DTOT=DTOT+WWFS
          XTRA=.5*CHWM*WCHL*(XVPR-XVPL)
          DSTL=DTOL+XTRA
          DSTR=DTOR+XTRA
          DSTB=DTOB+XTRA
          DSTT=DTOT+XTRA
C
          IF (IOHL.NE.0) THEN
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
            IF (IOHL/4.EQ.1) THEN
              IF (XLLB.LT.XVPL.OR.XRLB.GT.XVPR.OR.
     +            YBLB.LT.YVPB.OR.YTLB.GT.YVPT) GO TO 109
            ELSE IF (IOHL/4.GE.2) THEN
              DELX=0.
              IF (XLLB.LT.XVPL) DELX=XVPL-XLLB
              IF (XRLB+DELX.GT.XVPR) THEN
                IF (DELX.NE.0.) GO TO 109
                DELX=XVPR-XRLB
              END IF
              DELY=0.
              IF (YBLB.LT.YVPB) DELY=YVPB-YBLB
              IF (YTLB+DELY.GT.YVPT) THEN
                IF (DELY.NE.0.) GO TO 109
                DELY=YVPT-YTLB
              END IF
              XCLB=XCLB+DELX
              XLLB=XLLB+DELX
              XRLB=XRLB+DELX
              YCLB=YCLB+DELY
              YBLB=YBLB+DELY
              YTLB=YTLB+DELY
              XLBC=CFUX(XCLB)
              IF (ICFELL('CPHLLB',19).NE.0) RETURN
              YLBC=CFUY(YCLB)
              IF (ICFELL('CPHLLB',20).NE.0) RETURN
            END IF
C
          END IF
C
          IF (MOD(IOHL,4).NE.0) THEN
C
            ILB1=1
            ILB2=NLBS
            IF (MOD(IOHL,2).EQ.0) ILB1=INHL
            IF (MOD(IOHL/2,2).EQ.0) ILB2=INHL-1
C
              ILBL = ILB1
              GO TO 10029
10027         CONTINUE
              ILBL =ILBL +1
10029         CONTINUE
              IF (ILBL .GT.(ILB2)) GO TO 10028
C
              IF (ILBL.EQ.INIL) XTRA=.5*CHWM*WCIL*(XVPR-XVPL)
              IF (ILBL.EQ.INHL) XTRA=.5*CHWM*WCHL*(XVPR-XVPL)
              XCOL=RWRK(IR03+4*(ILBL-1)+1)
              YCOL=RWRK(IR03+4*(ILBL-1)+2)
              ANOL=RWRK(IR03+4*(ILBL-1)+3)
              SAOL=SIN(ANOL)
              CAOL=COS(ANOL)
              ICOL=INT(RWRK(IR03+4*(ILBL-1)+4))
              ODSL=RWRK(IR04-ICOL+3)+XTRA
              ODSR=RWRK(IR04-ICOL+4)+XTRA
              ODSB=RWRK(IR04-ICOL+5)+XTRA
              ODST=RWRK(IR04-ICOL+6)+XTRA
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
     +            YTLB.GE.YBOL.AND.YBLB.LE.YTOL) GO TO 109
C
            GO TO 10027
10028       CONTINUE
C
          END IF
C
          IF (IACT.EQ.1) THEN
            NLBS=NLBS+1
            IF (4*NLBS.GT.LR03) THEN
              CALL CPGRWS (RWRK,3,MAX(4*NLBS,LR03+100),IWSE)
              IF (IWSE.NE.0) THEN
                NLBS=NLBS-1
                GO TO 108
              ELSE IF (ICFELL('CPHLLB',21).NE.0) THEN
                NLBS=NLBS-1
                RETURN
              END IF
            END IF
            RWRK(IR03+4*(NLBS-1)+1)=XCLB
            RWRK(IR03+4*(NLBS-1)+2)=YCLB
            RWRK(IR03+4*(NLBS-1)+3)=ANLB
            RWRK(IR03+4*(NLBS-1)+4)=-NR04
            NR04=NR04+6
            IF (NR04.GT.LR04) THEN
              CALL CPGRWS (RWRK,4,MAX(NR04,LR04+100),IWSE)
              IF (IWSE.NE.0) THEN
                NLBS=NLBS-1
                GO TO 108
              ELSE IF (ICFELL('CPHLLB',22).NE.0) THEN
                NLBS=NLBS-1
                RETURN
              END IF
            END IF
            RWRK(IR04+NR04-5)=REAL(IHOL+1)
            RWRK(IR04+NR04-4)=ZDAT(IPNT,JPNT)
            RWRK(IR04+NR04-3)=DTOL
            RWRK(IR04+NR04-2)=DTOR
            RWRK(IR04+NR04-1)=DTOB
            RWRK(IR04+NR04  )=DTOT
          ELSE IF (IACT.EQ.2) THEN
            IF (MOD(IBHL/2,2).NE.0) THEN
              IF (JSFC.NE.JLBC) THEN
                CALL GSFACI (JLBC)
                JSFC=JLBC
              END IF
              CALL HLUCPCHHL (+2+4*IHOL)
              IF (ICFELL('CPHLLB',23).NE.0) RETURN
              IF (CTMA(1:LCTM).NE.' ') THEN
                BFXC(1)=CFUX(XCLB-DTOL*CALB+DTOB*SALB)
                IF (ICFELL('CPHLLB',24).NE.0) RETURN
                BFYC(1)=CFUY(YCLB-DTOL*SALB-DTOB*CALB)
                IF (ICFELL('CPHLLB',25).NE.0) RETURN
                BFXC(2)=CFUX(XCLB+DTOR*CALB+DTOB*SALB)
                IF (ICFELL('CPHLLB',26).NE.0) RETURN
                BFYC(2)=CFUY(YCLB+DTOR*SALB-DTOB*CALB)
                IF (ICFELL('CPHLLB',27).NE.0) RETURN
                BFXC(3)=CFUX(XCLB+DTOR*CALB-DTOT*SALB)
                IF (ICFELL('CPHLLB',28).NE.0) RETURN
                BFYC(3)=CFUY(YCLB+DTOR*SALB+DTOT*CALB)
                IF (ICFELL('CPHLLB',29).NE.0) RETURN
                BFXC(4)=CFUX(XCLB-DTOL*CALB-DTOT*SALB)
                IF (ICFELL('CPHLLB',30).NE.0) RETURN
                BFYC(4)=CFUY(YCLB-DTOL*SALB+DTOT*CALB)
                IF (ICFELL('CPHLLB',31).NE.0) RETURN
                CALL GFA (4,BFXC,BFYC)
              END IF
              CALL HLUCPCHHL (-2-4*IHOL)
              IF (ICFELL('CPHLLB',32).NE.0) RETURN
            END IF
            IF (IHOL.EQ.0) THEN
              JCOL=JCHI
            ELSE
              JCOL=JCLO
            END IF
            IF (JSLC.NE.JCOL) THEN
              CALL PLOTIF (0.,0.,2)
              IF (ICFELL('CPHLLB',33).NE.0) RETURN
              CALL GSPLCI (JCOL)
              JSLC=JCOL
            END IF
            IF (JSTC.NE.JCOL) THEN
              CALL GSTXCI (JCOL)
              JSTC=JCOL
            END IF
            CALL HLUCPCHHL (+3+4*IHOL)
            IF (ICFELL('CPHLLB',34).NE.0) RETURN
            IF (CTMA(1:LCTM).NE.' ') THEN
              CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCFS,ANHL,0.)
              IF (ICFELL('CPHLLB',35).NE.0) RETURN
            END IF
            CALL HLUCPCHHL (-3-4*IHOL)
            IF (ICFELL('CPHLLB',36).NE.0) RETURN
            IF (MOD(IBHL,2).NE.0) THEN
              WDTH=WLHL
              IF (WDTH.GT.0.) THEN
                CALL GQLWSC (IGER,SFLW)
                IF (IGER.NE.0) THEN
                  CALL SETER ('CPHLLB - ERROR EXIT FROM GQLWSC',37,1)
                  RETURN
                END IF
                CALL PLOTIF (0.,0.,2)
                IF (ICFELL('CPHLLB',38).NE.0) RETURN
                CALL GSLWSC (WDTH)
              END IF
              CALL HLUCPCHHL (+4+4*IHOL)
              IF (ICFELL('CPHLLB',39).NE.0) RETURN
              IF (CTMA(1:LCTM).NE.' ') THEN
                CALL PLOTIF (XCLB-DTOL*CALB+DTOB*SALB,
     +                       YCLB-DTOL*SALB-DTOB*CALB,0)
                IF (ICFELL('CPHLLB',40).NE.0) RETURN
                CALL PLOTIF (XCLB+DTOR*CALB+DTOB*SALB,
     +                       YCLB+DTOR*SALB-DTOB*CALB,1)
                IF (ICFELL('CPHLLB',41).NE.0) RETURN
                CALL PLOTIF (XCLB+DTOR*CALB-DTOT*SALB,
     +                       YCLB+DTOR*SALB+DTOT*CALB,1)
                IF (ICFELL('CPHLLB',42).NE.0) RETURN
                CALL PLOTIF (XCLB-DTOL*CALB-DTOT*SALB,
     +                       YCLB-DTOL*SALB+DTOT*CALB,1)
                IF (ICFELL('CPHLLB',43).NE.0) RETURN
                CALL PLOTIF (XCLB-DTOL*CALB+DTOB*SALB,
     +                       YCLB-DTOL*SALB-DTOB*CALB,1)
                IF (ICFELL('CPHLLB',44).NE.0) RETURN
                CALL PLOTIF (0.,0.,2)
                IF (ICFELL('CPHLLB',45).NE.0) RETURN
              END IF
              CALL HLUCPCHHL (-4-4*IHOL)
              IF (ICFELL('CPHLLB',46).NE.0) RETURN
              IF (WDTH.GT.0.) THEN
                CALL PLOTIF (0.,0.,2)
                IF (ICFELL('CPHLLB',47).NE.0) RETURN
                CALL GSLWSC (SFLW)
              END IF
            END IF
          ELSE
            RWRK(IR01+ 1)=CFUX(XCLB-DTOL*CALB+DTOB*SALB)
            IF (ICFELL('CPHLLB',48).NE.0) RETURN
            RWRK(IR01+ 2)=CFUX(XCLB+DTOR*CALB+DTOB*SALB)
            IF (ICFELL('CPHLLB',49).NE.0) RETURN
            RWRK(IR01+ 3)=CFUX(XCLB+DTOR*CALB-DTOT*SALB)
            IF (ICFELL('CPHLLB',50).NE.0) RETURN
            RWRK(IR01+ 4)=CFUX(XCLB-DTOL*CALB-DTOT*SALB)
            IF (ICFELL('CPHLLB',51).NE.0) RETURN
            RWRK(IR01+ 5)=RWRK(IR01+1)
            RWRK(IR01+ 6)=CFUY(YCLB-DTOL*SALB-DTOB*CALB)
            IF (ICFELL('CPHLLB',52).NE.0) RETURN
            RWRK(IR01+ 7)=CFUY(YCLB+DTOR*SALB-DTOB*CALB)
            IF (ICFELL('CPHLLB',53).NE.0) RETURN
            RWRK(IR01+ 8)=CFUY(YCLB+DTOR*SALB+DTOT*CALB)
            IF (ICFELL('CPHLLB',54).NE.0) RETURN
            RWRK(IR01+ 9)=CFUY(YCLB-DTOL*SALB+DTOT*CALB)
            IF (ICFELL('CPHLLB',55).NE.0) RETURN
            RWRK(IR01+10)=RWRK(IR01+6)
            IF ((XWDL.LT.XWDR.AND.YWDB.LT.YWDT).OR.(XWDL.GT.XWDR.AND.YWD
     +B.GT.YWDT)) THEN
              CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGLB,-1,0)
              IF (ICFELL('CPHLLB',56).NE.0) RETURN
            ELSE
              CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGLB,0,-1)
              IF (ICFELL('CPHLLB',57).NE.0) RETURN
            END IF
          END IF
        END IF
C
  109 CONTINUE
      GO TO (10014,10026) , L10015
C
      END
