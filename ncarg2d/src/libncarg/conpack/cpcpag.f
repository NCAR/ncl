C
C $Id: cpcpag.f,v 1.2 1994-03-17 01:50:44 kennison Exp $
C
      SUBROUTINE CPCPAG (ZDAT,RWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*)
C
C Given an array of data to be contoured, the routine CPCPAG computes
C an array of IGRM*IGRN gradients to be used in positioning labels.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C This routine also computes values for the quantities NGRV, which is
C the number of gradient values computed (because of special values and
C peculiarities of the mapping functions, NGRV may not equal IGRM*IGRN),
C GRAV, which is the average gradient found, and GRSD, which is the
C standard deviation of the gradient distribution.
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
C Compute some needed conversion constants.
C
      RZDM=(XATM-XAT1)/REAL(IZDM-1)
      RZDN=(YATN-YAT1)/REAL(IZDN-1)
C
C Initialize the gradient array.
C
      DO 10001 I=1,IGRM*IGRN
        RWRK(IR02+I)=-1.
10001 CONTINUE
C
C Run through all the right triangles with hypotenuses in the first
C quadrant ...
C
        I=1
        GO TO 10004
10002   CONTINUE
        I=I+1
10004   CONTINUE
        IF (I.GT.(IZDM-1)) GO TO 10003
          J=1
          GO TO 10007
10005     CONTINUE
          J=J+1
10007     CONTINUE
          IF (J.GT.(IZDN-1)) GO TO 10006
          XCD1=REAL(I)
          YCD1=REAL(J)
          ZCD1=ZDAT(I,J)
          XCD2=REAL(I+1)
          YCD2=REAL(J)
          ZCD2=ZDAT(I+1,J)
          XCD3=REAL(I)
          YCD3=REAL(J+1)
          ZCD3=ZDAT(I,J+1)
          L10009=    1
          GO TO 10009
10008     CONTINUE
        GO TO 10005
10006   CONTINUE
      GO TO 10002
10003 CONTINUE
C
C ... the second quadrant, ...
C
        I=2
        GO TO 10012
10010   CONTINUE
        I=I+1
10012   CONTINUE
        IF (I.GT.(IZDM)) GO TO 10011
          J=1
          GO TO 10015
10013     CONTINUE
          J=J+1
10015     CONTINUE
          IF (J.GT.(IZDN-1)) GO TO 10014
          XCD1=REAL(I)
          YCD1=REAL(J)
          ZCD1=ZDAT(I,J)
          XCD2=REAL(I)
          YCD2=REAL(J+1)
          ZCD2=ZDAT(I,J+1)
          XCD3=REAL(I-1)
          YCD3=REAL(J)
          ZCD3=ZDAT(I-1,J)
          L10009=    2
          GO TO 10009
10016     CONTINUE
        GO TO 10013
10014   CONTINUE
      GO TO 10010
10011 CONTINUE
C
C ... the third quadrant, ...
C
        I=2
        GO TO 10019
10017   CONTINUE
        I=I+1
10019   CONTINUE
        IF (I.GT.(IZDM)) GO TO 10018
          J=2
          GO TO 10022
10020     CONTINUE
          J=J+1
10022     CONTINUE
          IF (J.GT.(IZDN)) GO TO 10021
          XCD1=REAL(I)
          YCD1=REAL(J)
          ZCD1=ZDAT(I,J)
          XCD2=REAL(I-1)
          YCD2=REAL(J)
          ZCD2=ZDAT(I-1,J)
          XCD3=REAL(I)
          YCD3=REAL(J-1)
          ZCD3=ZDAT(I,J-1)
          L10009=    3
          GO TO 10009
10023     CONTINUE
        GO TO 10020
10021   CONTINUE
      GO TO 10017
10018 CONTINUE
C
C ... and the fourth quadrant.
C
        I=1
        GO TO 10026
10024   CONTINUE
        I=I+1
10026   CONTINUE
        IF (I.GT.(IZDM-1)) GO TO 10025
          J=2
          GO TO 10029
10027     CONTINUE
          J=J+1
10029     CONTINUE
          IF (J.GT.(IZDN)) GO TO 10028
          XCD1=REAL(I)
          YCD1=REAL(J)
          ZCD1=ZDAT(I,J)
          XCD2=REAL(I)
          YCD2=REAL(J-1)
          ZCD2=ZDAT(I,J-1)
          XCD3=REAL(I+1)
          YCD3=REAL(J)
          ZCD3=ZDAT(I+1,J)
          L10009=    4
          GO TO 10009
10030     CONTINUE
        GO TO 10027
10028   CONTINUE
      GO TO 10024
10025 CONTINUE
C
C Compute the average gradient and the standard deviation of the
C distribution of gradients.
C
      NGRV=0
      GRAV=0.
      GRSD=0.
C
      DO 10031 I=1,IGRM*IGRN
        IF (RWRK(IR02+I).GE.0.) THEN
          NGRV=NGRV+1
          GRAV=GRAV+RWRK(IR02+I)
C           GRSD=GRSD+RWRK(IR02+I)**2
        END IF
10031 CONTINUE
C
      IF (NGRV.NE.0) THEN
        GRAV=GRAV/NGRV
C         GRSD=SQRT(GRSD/NGRV-GRAV*GRAV)
        IF (GRAV.NE.0.) THEN
          DO 10032 I=1,IGRM*IGRN
            IF (RWRK(IR02+I).GE.0.) THEN
              GRSD=GRSD+((RWRK(IR02+I)-GRAV)/GRAV)**2
            END IF
10032     CONTINUE
          GRSD=GRAV*SQRT(GRSD/NGRV)
        END IF
      END IF
C
C Done.
C
      RETURN
C
C The following "internal procedure", given a triangle in three-space,
C computes the gradient of that triangle and updates relevant portions
C of the gradient array.
C
10009 CONTINUE
C
        IF (SVAL.NE.0..AND.ZCD1.EQ.SVAL) THEN
          IVI1=0
        ELSE
          XGD1=XAT1+RZDM*(XCD1-1.)
          YGD1=YAT1+RZDN*(YCD1-1.)
          IVI1=1
          IF (IMPF.NE.0) THEN
            XTMP=XGD1
            YTMP=YGD1
            CALL CPMPXY (IMPF,XTMP,YTMP,XGD1,YGD1)
            IF (ICFELL('CPCPAG',1).NE.0) RETURN
            IF (OORV.NE.0..AND.(XGD1.EQ.OORV.OR.YGD1.EQ.OORV)) IVI1=0
          END IF
          XGD1=CUFX(XGD1)
          IF (ICFELL('CPCPAG',2).NE.0) RETURN
          YGD1=CUFY(YGD1)
          IF (ICFELL('CPCPAG',3).NE.0) RETURN
          ZGD1=ZCD1
        END IF
C
        IF (SVAL.NE.0..AND.ZCD2.EQ.SVAL) THEN
          IVI2=0
        ELSE
          XGD2=XAT1+RZDM*(XCD2-1.)
          YGD2=YAT1+RZDN*(YCD2-1.)
          IVI2=1
          IF (IMPF.NE.0) THEN
            XTMP=XGD2
            YTMP=YGD2
            CALL CPMPXY (IMPF,XTMP,YTMP,XGD2,YGD2)
            IF (ICFELL('CPCPAG',4).NE.0) RETURN
            IF (OORV.NE.0..AND.(XGD2.EQ.OORV.OR.YGD2.EQ.OORV)) IVI2=0
          END IF
          XGD2=CUFX(XGD2)
          IF (ICFELL('CPCPAG',5).NE.0) RETURN
          YGD2=CUFY(YGD2)
          IF (ICFELL('CPCPAG',6).NE.0) RETURN
          ZGD2=ZCD2
        END IF
C
        IF (SVAL.NE.0..AND.ZCD3.EQ.SVAL) THEN
          IVI3=0
        ELSE
          XGD3=XAT1+RZDM*(XCD3-1.)
          YGD3=YAT1+RZDN*(YCD3-1.)
          IVI3=1
          IF (IMPF.NE.0) THEN
            XTMP=XGD3
            YTMP=YGD3
            CALL CPMPXY (IMPF,XTMP,YTMP,XGD3,YGD3)
            IF (ICFELL('CPCPAG',7).NE.0) RETURN
            IF (OORV.NE.0..AND.(XGD3.EQ.OORV.OR.YGD3.EQ.OORV)) IVI3=0
          END IF
          XGD3=CUFX(XGD3)
          IF (ICFELL('CPCPAG',8).NE.0) RETURN
          YGD3=CUFY(YGD3)
          IF (ICFELL('CPCPAG',9).NE.0) RETURN
          ZGD3=ZCD3
        END IF
C
        IF (IVI1.NE.0.AND.IVI2.NE.0.AND.IVI3.NE.0) THEN
          XD12=XGD2-XGD1
          YD12=YGD2-YGD1
          XD23=XGD3-XGD2
          YD23=YGD3-YGD2
          XD31=XGD1-XGD3
          YD31=YGD1-YGD3
          IF ((XD12.NE.0..OR.YD12.NE.0.).AND.(XD23.NE.0..OR.YD23.NE.0.).
     +AND.(XD31.NE.0..OR.YD31.NE.0.)) THEN
            IF (ZGD1.LT.ZGD2) THEN
              IF (ZGD2.LT.ZGD3) THEN
                XGDA=XGD1
                YGDA=YGD1
                ZGDA=ZGD1
                XGDB=XGD2
                YGDB=YGD2
                ZGDB=ZGD2
                XGDC=XGD3
                YGDC=YGD3
                ZGDC=ZGD3
              ELSE
                IF (ZGD1.LT.ZGD3) THEN
                  XGDA=XGD1
                  YGDA=YGD1
                  ZGDA=ZGD1
                  XGDB=XGD3
                  YGDB=YGD3
                  ZGDB=ZGD3
                  XGDC=XGD2
                  YGDC=YGD2
                  ZGDC=ZGD2
                ELSE
                  XGDA=XGD3
                  YGDA=YGD3
                  ZGDA=ZGD3
                  XGDB=XGD1
                  YGDB=YGD1
                  ZGDB=ZGD1
                  XGDC=XGD2
                  YGDC=YGD2
                  ZGDC=ZGD2
                END IF
              END IF
            ELSE
              IF (ZGD1.LT.ZGD3) THEN
                XGDA=XGD2
                YGDA=YGD2
                ZGDA=ZGD2
                XGDB=XGD1
                YGDB=YGD1
                ZGDB=ZGD1
                XGDC=XGD3
                YGDC=YGD3
                ZGDC=ZGD3
              ELSE
                IF (ZGD2.LT.ZGD3) THEN
                  XGDA=XGD2
                  YGDA=YGD2
                  ZGDA=ZGD2
                  XGDB=XGD3
                  YGDB=YGD3
                  ZGDB=ZGD3
                  XGDC=XGD1
                  YGDC=YGD1
                  ZGDC=ZGD1
                ELSE
                  XGDA=XGD3
                  YGDA=YGD3
                  ZGDA=ZGD3
                  XGDB=XGD2
                  YGDB=YGD2
                  ZGDB=ZGD2
                  XGDC=XGD1
                  YGDC=YGD1
                  ZGDC=ZGD1
                END IF
              END IF
            END IF
            DNOM=(XGDC-XGDB)*YGDA+(XGDA-XGDC)*YGDB+(XGDB-XGDA)*YGDC
            IF (DNOM.NE.0.) THEN
              IF (ZGDC-ZGDA.NE.0.) THEN
                COFA=((YGDB-YGDC)*ZGDA+(YGDC-YGDA)*ZGDB+
     +                                          (YGDA-YGDB)*ZGDC)/DNOM
                COFB=((XGDC-XGDB)*ZGDA+(XGDA-XGDC)*ZGDB+
     +                                          (XGDB-XGDA)*ZGDC)/DNOM
                XDMX=YGDB-YGDA+(YGDA-YGDC)*(ZGDB-ZGDA)/(ZGDC-ZGDA)
                YDMX=XGDA-XGDB+(XGDC-XGDA)*(ZGDB-ZGDA)/(ZGDC-ZGDA)
                GRMX=ABS(COFA*XDMX+COFB*YDMX)/SQRT(XDMX**2+YDMX**2)
C                 GANG=ATAN2(YDMX,XDMX)
              ELSE
                GRMX=0.
C                 GANG=0.
              END IF
              KMIN=MAX(   1,  INT((MIN(XGD1,XGD2,XGD3)-XVPL)/
     +                                        (XVPR-XVPL)*REAL(IGRM)))
              KMAX=MIN(IGRM,1+INT((MAX(XGD1,XGD2,XGD3)-XVPL)/
     +                                        (XVPR-XVPL)*REAL(IGRM)))
              LMIN=MAX(   1,  INT((MIN(YGD1,YGD2,YGD3)-YVPB)/
     +                                        (YVPT-YVPB)*REAL(IGRN)))
              LMAX=MIN(IGRN,1+INT((MAX(YGD1,YGD2,YGD3)-YVPB)/
     +                                        (YVPT-YVPB)*REAL(IGRN)))
              DN12=SQRT(XD12*XD12+YD12*YD12)
              DN23=SQRT(XD23*XD23+YD23*YD23)
              DN31=SQRT(XD31*XD31+YD31*YD31)
              DO 10033 K=KMIN,KMAX
                XCBX=XVPL+(REAL(K)-.5)/REAL(IGRM)*(XVPR-XVPL)
                DO 10034 L=LMIN,LMAX
                  YCBX=YVPB+(REAL(L)-.5)/REAL(IGRN)*(YVPT-YVPB)
                  TS12=(YD12*XCBX-XD12*YCBX-YD12*XGD1+XD12*YGD1)/DN12
                  TS23=(YD23*XCBX-XD23*YCBX-YD23*XGD2+XD23*YGD2)/DN23
                  TS31=(YD31*XCBX-XD31*YCBX-YD31*XGD3+XD31*YGD3)/DN31
                  IF ((TS12.LT.+.0001.AND.TS23.LT.+.0001.AND.TS31.LT.+.0
     +001).OR.(TS12.GT.-.0001.AND.TS23.GT.-.0001.AND.TS31.GT.-.0001)) TH
     +EN
                    IF (GRMX.GT.RWRK(IR02+(L-1)*IGRM+K)) THEN
                      RWRK(IR02+(L-1)*IGRM+K)=GRMX
C                       RWRK(IGRM*IGRN+IR02+(L-1)*IGRM+K)=GANG
                    END IF
                  END IF
10034           CONTINUE
10033         CONTINUE
            END IF
          END IF
        END IF
      GO TO (10008,10016,10023,10030) , L10009
C
      END
