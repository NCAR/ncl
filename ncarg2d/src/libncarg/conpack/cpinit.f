C
C $Id: cpinit.f,v 1.4 1994-09-12 22:10:31 kennison Exp $
C
      SUBROUTINE CPINIT (ZDAT,RWRK,IWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
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
C Define a variable which will hold a single character.
C
      CHARACTER*1 SCHR
C
C Decide what the range of values in X and Y should be.
C
      IF (UXA1.EQ.UXAM) THEN
        XAT1=1.
        XATM=REAL(IZDM)
      ELSE
        XAT1=UXA1
        XATM=UXAM
      END IF
C
      IF (UYA1.EQ.UYAN) THEN
        YAT1=1.
        YATN=REAL(IZDN)
      ELSE
        YAT1=UYA1
        YATN=UYAN
      END IF
C
C If the user has done a SET call, retrieve the arguments; if he hasn't
C done a SET call, do it for him.
C
      IF (ISET.EQ.0) THEN
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('CPINIT',1).NE.0) RETURN
C
      ELSE
C
        LNLG=1
C
        IF (UWDL.EQ.UWDR) THEN
          XWDL=XAT1
          XWDR=XATM
        ELSE
          XWDL=UWDL
          XWDR=UWDR
        END IF
C
        IF (UWDB.EQ.UWDT) THEN
          YWDB=YAT1
          YWDT=YATN
        ELSE
          YWDB=UWDB
          YWDT=UWDT
        END IF
C
        IF (UVPS.LT.0.) THEN
          RWTH=ABS(UVPS)
        ELSE IF (UVPS.EQ.0.) THEN
          RWTH=(UVPR-UVPL)/(UVPT-UVPB)
        ELSE IF (UVPS.LE.1.) THEN
          RWTH=ABS((XWDR-XWDL)/(YWDT-YWDB))
          IF (MIN(RWTH,1./RWTH).LT.UVPS) RWTH=(UVPR-UVPL)/(UVPT-UVPB)
        ELSE
          RWTH=ABS((XWDR-XWDL)/(YWDT-YWDB))
          IF (MAX(RWTH,1./RWTH).GT.UVPS) RWTH=1.
        END IF
C
        IF (RWTH.LT.(UVPR-UVPL)/(UVPT-UVPB)) THEN
          XVPL=.5*(UVPL+UVPR)-.5*(UVPT-UVPB)*RWTH
          XVPR=.5*(UVPL+UVPR)+.5*(UVPT-UVPB)*RWTH
          YVPB=UVPB
          YVPT=UVPT
        ELSE
          XVPL=UVPL
          XVPR=UVPR
          YVPB=.5*(UVPB+UVPT)-.5*(UVPR-UVPL)/RWTH
          YVPT=.5*(UVPB+UVPT)+.5*(UVPR-UVPL)/RWTH
        END IF
C
        CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('CPINIT',2).NE.0) RETURN
C
      END IF
C
C Set the flag MIRO, which indicates whether or not the transformations
C in effect cause mirror-imaging.
C
      MIRO=0
C
      RZDM=(XATM-XAT1)/REAL(IZDM-1)
      RZDN=(YATN-YAT1)/REAL(IZDN-1)
C
      DO 10001 I=1,IZDM-1
        DO 10002 J=1,IZDN-1
          XCFP=XAT1+RZDM*(REAL(I  )-1.)
          YCFP=YAT1+RZDN*(REAL(J  )-1.)
          XCLP=XAT1+RZDM*(REAL(I+1)-1.)
          YCLP=YAT1+RZDN*(REAL(J+1)-1.)
          XCPR=XAT1+RZDM*(REAL(I+1)-1.)
          YCPR=YAT1+RZDN*(REAL(J  )-1.)
          IF (IMPF.NE.0) THEN
            XTMP=XCFP
            YTMP=YCFP
            CALL CPMPXY (IMPF,XTMP,YTMP,XCFP,YCFP)
            IF (ICFELL('CPINIT',3).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCFP.EQ.OORV.OR.YCFP.EQ.OORV))
     +                                                       GO TO 101
            XTMP=XCLP
            YTMP=YCLP
            CALL CPMPXY (IMPF,XTMP,YTMP,XCLP,YCLP)
            IF (ICFELL('CPINIT',4).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCLP.EQ.OORV.OR.YCLP.EQ.OORV))
     +                                                       GO TO 101
            XTMP=XCPR
            YTMP=YCPR
            CALL CPMPXY (IMPF,XTMP,YTMP,XCPR,YCPR)
            IF (ICFELL('CPINIT',5).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCPR.EQ.OORV.OR.YCPR.EQ.OORV))
     +                                                       GO TO 101
            IF (ABS(XCLP-XCFP).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCLP-YCFP).LT..0001*ABS(YWDT-YWDB)) GO TO 101
            IF (ABS(XCPR-XCFP).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCPR-YCFP).LT..0001*ABS(YWDT-YWDB)) GO TO 101
            IF (ABS(XCLP-XCPR).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCLP-YCPR).LT..0001*ABS(YWDT-YWDB)) GO TO 101
            IF (ABS(XCLP-XCFP).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCLP-YCFP).GT..5*ABS(YWDT-YWDB)) GO TO 101
            IF (ABS(XCPR-XCFP).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCPR-YCFP).GT..5*ABS(YWDT-YWDB)) GO TO 101
            IF (ABS(XCLP-XCPR).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCLP-YCPR).GT..5*ABS(YWDT-YWDB)) GO TO 101
          END IF
          IF (XCLP.EQ.XCFP.AND.YCLP.EQ.YCFP) GO TO 101
          IF (ABS(XCLP-XCFP).LT.ABS(YCLP-YCFP)) THEN
            IF (XCPR.LT.XCFP+((XCLP-XCFP)/(YCLP-YCFP))*(YCPR-YCFP)) THEN
              IF (YCFP.LT.YCLP) MIRO=1
              GO TO 102
            ELSE
              IF (YCFP.GT.YCLP) MIRO=1
              GO TO 102
            END IF
          ELSE
            IF (YCPR.LT.YCFP+((YCLP-YCFP)/(XCLP-XCFP))*(XCPR-XCFP)) THEN
              IF (XCFP.GT.XCLP) MIRO=1
              GO TO 102
            ELSE
              IF (XCFP.LT.XCLP) MIRO=1
              GO TO 102
            END IF
          END IF
  101     CONTINUE
10002   CONTINUE
10001 CONTINUE
C
  102 CONTINUE
C
C Zero the count of label positions selected, the count of words used
C in real workspace number 4 (for informational and high/low label
C data), and the three indices which indicate where the different kinds
C of labels are stored.
C
      NLBS=0
      NR04=0
      INIL=0
      INHL=0
      INLL=0
C
C Initialize the value of the scale factor used.
C
      IF (SCFS.LE.0.) THEN
        SCFU=1.
      ELSE
        SCFU=SCFS
      END IF
C
C If contour levels are being chosen by CONPACK, zero the number of
C levels and the values of the contour interval and label interval
C used.  If new levels are not being chosen, force recomputation of
C the text-extent parameter elements for all existing contour levels,
C in case the user changes the character-quality parameter of PLOTCHAR.
C
      IF (ICLS.NE.0) THEN
        NCLV=0
        CINU=0.
        LINU=0
      ELSE
        DO 10003 I=1,NCLV
          NCLB(I)=-ABS(NCLB(I))
10003   CONTINUE
      END IF
C
C Find the minimum and maximum values in the field.
C
      ITMP=0
      ZMIN=0.
      ZMAX=0.
C
      DO 10004 I=1,IZDM
        DO 10005 J=1,IZDN
          IF (SVAL.EQ.0..OR.ZDAT(I,J).NE.SVAL) THEN
            IF (ITMP.EQ.0) THEN
              ITMP=1
              ZMIN=ZDAT(I,J)
              ZMAX=ZDAT(I,J)
            ELSE
              ZMIN=MIN(ZMIN,ZDAT(I,J))
              ZMAX=MAX(ZMAX,ZDAT(I,J))
            END IF
          END IF
10005   CONTINUE
10004 CONTINUE
C
C If the field is (effectively) constant, set a flag to indicate that
C and force the scale factor back to 1.  Otherwise, clear the flag.
C
      IF (ZMAX-ZMIN.LE.10.*EPSI*ABS((ZMIN+ZMAX)/2.)) THEN
        ICFF=1
        SCFU=1.
      ELSE
        ICFF=0
      END IF
C
C Find the positions of the leftmost significant digits in the largest
C absolute value in the field and in the difference between the minimum
C and the maximum values in the field.  If the field is effectively
C constant, the latter value is set equal to the former.
C
      CALL CPNUMB (MAX(ABS(ZMIN/SCFU),ABS(ZMAX/SCFU)),1,-10000,-1,-1,
     +                    ' ',' ',' ',0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
      LSDM=IEVA-1
C
      IF (ICFF.EQ.0) THEN
        CALL CPNUMB ((ZMAX-ZMIN)/SCFU,1,-10000,-1,-1,
     +                    ' ',' ',' ',0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
        LSDD=IEVA-1
      ELSE
        LSDD=LSDM
      END IF
C
C Retrieve the current PLOTCHAR function code signal character.
C
      CALL PCGETC ('FC',SCHR)
      IF (ICFELL('CPINIT',6).NE.0) RETURN
C
C If highs and lows are to be labelled, attempt to make sure that the
C string will be treated properly by PLOTCHAR.
C
      IF (LTHI.GE.4) THEN
        IF (TXHI(1:1).EQ.'H'.AND.TXHI(3:3).EQ.'B') THEN
          DO 10006 I=4,LTHI
            IF (TXHI(I:I).EQ.TXHI(2:2)) TXHI(I:I)=SCHR
10006     CONTINUE
          TXHI(2:2)=SCHR
        END IF
      END IF
C
      IF (LTLO.GE.4) THEN
        IF (TXLO(1:1).EQ.'L'.AND.TXLO(3:3).EQ.'B') THEN
          DO 10007 I=4,LTLO
            IF (TXLO(I:I).EQ.TXLO(2:2)) TXLO(I:I)=SCHR
10007     CONTINUE
          TXLO(2:2)=SCHR
        END IF
      END IF
C
C Set up the parameters used in generating numeric labels.  Set the
C number of significant digits to be used ...
C
      IF (NSDL.LT.0) THEN
        NDGL=ABS(NSDL)
      ELSE
        NDGL=MAX(0,LSDM-LSDD)+NSDL
      END IF
C
C ... the leftmost-significant digit flag ...
C
      IF (NLSD.EQ.0) THEN
        LSDL=-10000
      ELSE
        LSDL=LSDM
      END IF
C
C ... the numeric exponent type ...
C
      IF (NEXT.LE.0) THEN
        CHEX=' E '
        LEA1=1
        LEA2=1
        LEA3=1
        LEE1=0
        LEE2=1
        LEE3=0
      ELSE IF (NEXT.EQ.1) THEN
        CHEX=':L1:410:S::N:'
        IF (SCHR.NE.':') THEN
          CHEX( 1: 1)=SCHR
          CHEX( 4: 4)=SCHR
          CHEX( 8: 8)=SCHR
          CHEX(10:10)=SCHR
          CHEX(11:11)=SCHR
          CHEX(13:13)=SCHR
        END IF
        LEA1=5
        LEA2=5
        LEA3=3
        LEE1=1
        LEE2=2
        LEE3=0
      ELSE
        CHEX='x10** '
        LEA1=1
        LEA2=4
        LEA3=1
        LEE1=1
        LEE2=4
        LEE3=0
      END IF
C
C ... and the omission flags.
C
      JOMA=MOD(MAX(0,MIN(7,NOMF))/4,2)
      JODP=MOD(MAX(0,MIN(7,NOMF))/2,2)
      JOTZ=MOD(MAX(0,MIN(7,NOMF))  ,2)
C
C If the field is not constant and the scale factor is to be chosen
C here, do it now.  The parameter which specifies where the leftmost
C significant digit is assumed to be also must be updated.
C
      IF (ICFF.EQ.0.AND.SCFS.LE.0..AND.SCFS.GE.-3.) THEN
        ITMP=0
        IF (SCFS.EQ.0..OR.(SCFS.EQ.-3..AND.LSDM.LT.-1)) ITMP=LSDM+1
        IF (SCFS.EQ.-1.) ITMP=LSDM
        IF (SCFS.EQ.-2..OR.(SCFS.EQ.-3..AND.LSDM-NDGL.GE.0))
     +                                                ITMP=LSDM-NDGL+1
        SCFU=10.**ITMP
        IF (LSDL.NE.-10000) LSDL=LSDL-ITMP
      END IF
C
C Done.
C
      RETURN
C
      END
