C
C	$Id: cpsp1a.f,v 1.1.1.1 1992-04-17 22:32:46 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CPSP1A (ZSPS,KSPS,MSPS,NSPS,RWRK,IWRK,ZDAT)
C
      DIMENSION ZSPS(KSPS,*),RWRK(*),IWRK(*),ZDAT(IZD1,*)
C
C The routine CPSP1A is really just a part of CPSPS1.  It has to be
C made separate so that the change in dimension of ZDAT will actually
C take effect.
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
C Clear all the workspace block lengths.
C
      DO 10001 I=1,4
        LRWS(I)=0
10001 CONTINUE
C
      DO 10002 I=1,2
        LIWS(I)=0
10002 CONTINUE
C
C Zero the internal parameters which keep track of workspace usage.
C
      IIWU=0
      IRWU=0
C
C If the special-value flag is set, record the positions of the special
C values in the sparse array and replace them with reasonable values so
C that the interpolation routines can be called.
C
      IF (SVAL.NE.0.) THEN
C
        NSVS=0
C
        DO 10003 ISPS=1,MSPS
          DO 10004 JSPS=1,NSPS
            IF (ZSPS(ISPS,JSPS).EQ.SVAL) THEN
              IF (NSVS.GE.LI01) THEN
                CALL CPGIWS (IWRK,1,LI01+100,IWSE)
                IF (IWSE.NE.0) THEN
                  GO TO 10006
                END IF
              END IF
              IF (NSVS.GE.LR01) THEN
                CALL CPGRWS (RWRK,1,LR01+100,IWSE)
                IF (IWSE.NE.0) THEN
                  GO TO 10006
                END IF
              END IF
              NSVS=NSVS+1
              IWRK(II01+NSVS)=NSPS*(ISPS-1)+(JSPS-1)
              RWRK(IR01+NSVS)=SVAL
            END IF
10004     CONTINUE
10003   CONTINUE
C
        NSVR=NSVS
        MEST=4
C
10008   CONTINUE
        IF (.NOT.(NSVR.NE.0)) GO TO 10009
C
          NREP=0
C
          DO 10010 I=1,NSVR
            ISPS=IWRK(II01+I)/NSPS+1
            JSPS=MOD(IWRK(II01+I),NSPS)+1
            NEST=0
            REST=0.
            IF (ISPS.GE.3) THEN
              IF (ZSPS(ISPS-1,JSPS).NE.SVAL.AND.ZSPS(ISPS-2,JSPS).NE.SVA
     +L) THEN
                NEST=NEST+1
                REST=REST+1.5*ZSPS(ISPS-1,JSPS)-.5*ZSPS(ISPS-2,JSPS)
              END IF
            END IF
            IF (ISPS.LE.MSPS-2) THEN
              IF (ZSPS(ISPS+1,JSPS).NE.SVAL.AND.ZSPS(ISPS+2,JSPS).NE.SVA
     +L) THEN
                NEST=NEST+1
                REST=REST+1.5*ZSPS(ISPS+1,JSPS)-.5*ZSPS(ISPS+2,JSPS)
              END IF
            END IF
            IF (JSPS.GE.3) THEN
              IF (ZSPS(ISPS,JSPS-1).NE.SVAL.AND.ZSPS(ISPS,JSPS-2).NE.SVA
     +L) THEN
                NEST=NEST+1
                REST=REST+1.5*ZSPS(ISPS,JSPS-1)-.5*ZSPS(ISPS,JSPS-2)
              END IF
            END IF
            IF (JSPS.LE.NSPS-2) THEN
              IF (ZSPS(ISPS,JSPS+1).NE.SVAL.AND.ZSPS(ISPS,JSPS+2).NE.SVA
     +L) THEN
                NEST=NEST+1
                REST=REST+1.5*ZSPS(ISPS,JSPS+1)-.5*ZSPS(ISPS,JSPS+2)
              END IF
            END IF
            IF (NEST.GE.MEST) THEN
              NREP=NREP+1
              RWRK(IR01+I)=REST/FLOAT(NEST)
            END IF
10010     CONTINUE
C
          IF (NREP.EQ.0) THEN
C
            MEST=MEST-1
C
            IF (MEST.LE.0) THEN
              CALL SETER ('CPSPS1 - SPECIAL-VALUE REPLACEMENT FAILURE'
     +                                                           ,2,2)
              STOP
            END IF
C
          ELSE
C
            NSVT=NSVR
C
            DO 10011 I=NSVT,1,-1
              IF (RWRK(IR01+I).NE.SVAL) THEN
                ISPS=IWRK(II01+I)/NSPS+1
                JSPS=MOD(IWRK(II01+I),NSPS)+1
                ZSPS(ISPS,JSPS)=RWRK(IR01+I)
                IF (I.NE.NSVR) THEN
                  ITMP=IWRK(II01+I)
                  IWRK(II01+I)=IWRK(II01+NSVR)
                  IWRK(II01+NSVR)=ITMP
                  RWRK(IR01+I)=SVAL
                END IF
                NSVR=NSVR-1
              END IF
10011       CONTINUE
C
            MEST=4
C
          END IF
C
        GO TO 10008
10009   CONTINUE
C
      END IF
C
C Do the interpolation from the sparse array to the dense array.
C
      CALL CPGRWS (RWRK,1,3*MSPS*NSPS+MAX(MSPS+NSPS+NSPS,4*IZDM),IWSE)
C
      IF (IWSE.NE.0) THEN
        GO TO 10006
      END IF
C
      CALL MSBSF1 (MSPS,NSPS,1.,REAL(MSPS),1.,REAL(NSPS),ZSPS,KSPS,
     +             RWRK(IR01+1),RWRK(IR01+1+3*MSPS*NSPS),T3DS)
C
      CALL MSBSF2 (1.,REAL(MSPS),IZDM,1.,REAL(NSPS),IZDN,ZDAT,IZD1,
     +             MSPS,NSPS,1.,REAL(MSPS),1.,REAL(NSPS),ZSPS,KSPS,
     +             RWRK(IR01+1),RWRK(IR01+1+3*MSPS*NSPS),T3DS)
C
      LR01=0
C
C If the special-value flag is set, restore the special values to the
C sparse array and fill in the corresponding values in the dense array.
C
      IF (SVAL.NE.0.) THEN
C
        DO 10013 I=1,NSVS
          ISPS=IWRK(II01+I)/NSPS+1
          JSPS=MOD(IWRK(II01+I),NSPS)+1
          ZSPS(ISPS,JSPS)=SVAL
          JBEG=MAX(1,MIN(IZDM,INT((REAL(ISPS-2)/REAL(MSPS-1))*
     +                                   REAL(IZDM-1)+1.E-6)+2))
          JEND=MAX(1,MIN(IZDM,INT((REAL(ISPS  )/REAL(MSPS-1))*
     +                                   REAL(IZDM-1)-1.E-6)+1))
          KBEG=MAX(1,MIN(IZDN,INT((REAL(JSPS-2)/REAL(NSPS-1))*
     +                                   REAL(IZDN-1)+1.E-6)+2))
          KEND=MAX(1,MIN(IZDN,INT((REAL(JSPS  )/REAL(NSPS-1))*
     +                                   REAL(IZDN-1)-1.E-6)+1))
          DO 10014 J=JBEG,JEND
            DO 10015 K=KBEG,KEND
              ZDAT(J,K)=SVAL
10015       CONTINUE
10014     CONTINUE
10013   CONTINUE
C
        LI01=0
C
      END IF
C
C CPINIT does the rest.
C
      CALL CPINIT (ZDAT,RWRK,IWRK)
C
C Done.
C
      RETURN
C
C Error exit.
C
10006 CONTINUE
        CALL SETER ('CPSPS1 - CANNOT CONTINUE WITHOUT WORKSPACE',3,
     +                                                            2)
        STOP
C
      END
