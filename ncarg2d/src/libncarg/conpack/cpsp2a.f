C
C $Id: cpsp2a.f,v 1.5 1994-05-18 16:17:24 kennison Exp $
C
      SUBROUTINE CPSP2A (XSPS,YSPS,ZSPS,KSPS,MSPS,NSPS,RWRK,IWRK,ZDAT)
C
      DIMENSION XSPS(*),YSPS(*),ZSPS(KSPS,*),RWRK(*),IWRK(*),
     +          ZDAT(IZD1,*)
C
C The routine CPSP2A is really just a part of CPSPS2.  It has to be
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
C Declare the type of the FITPACK function MSSRF2.
C
      REAL MSSRF2
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
                  CALL SETER ('CPSPS2 - CANNOT CONTINUE WITHOUT WORKSPAC
     +E',4,1)
                  RETURN
                ELSE IF (ICFELL('CPSPS2',5).NE.0) THEN
                  RETURN
                END IF
              END IF
              IF (NSVS.GE.LR01) THEN
                CALL CPGRWS (RWRK,1,LR01+100,IWSE)
                IF (IWSE.NE.0) THEN
                  CALL SETER ('CPSPS2 - CANNOT CONTINUE WITHOUT WORKSPAC
     +E',6,1)
                  RETURN
                ELSE IF (ICFELL('CPSPS2',7).NE.0) THEN
                  RETURN
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
10005   CONTINUE
        IF (.NOT.(NSVR.NE.0)) GO TO 10006
C
          NREP=0
C
          DO 10007 I=1,NSVR
            ISPS=IWRK(II01+I)/NSPS+1
            JSPS=MOD(IWRK(II01+I),NSPS)+1
            NEST=0
            REST=0.
            IF (ISPS.GE.3) THEN
              IF (ZSPS(ISPS-1,JSPS).NE.SVAL.AND.ZSPS(ISPS-2,JSPS).NE.SVA
     +L) THEN
                NEST=NEST+1
                REST=REST+ZSPS(ISPS-1,JSPS)+.5*
     +                    (ZSPS(ISPS-2,JSPS)-ZSPS(ISPS-1,JSPS))*
     +                    ((XSPS(ISPS  )-XSPS(ISPS-1))/
     +                     (XSPS(ISPS-2)-XSPS(ISPS-1)))
              END IF
            END IF
            IF (ISPS.LE.MSPS-2) THEN
              IF (ZSPS(ISPS+1,JSPS).NE.SVAL.AND.ZSPS(ISPS+2,JSPS).NE.SVA
     +L) THEN
                NEST=NEST+1
                REST=REST+ZSPS(ISPS+1,JSPS)+.5*
     +                    (ZSPS(ISPS+2,JSPS)-ZSPS(ISPS+1,JSPS))*
     +                    ((XSPS(ISPS  )-XSPS(ISPS+1))/
     +                     (XSPS(ISPS+2)-XSPS(ISPS+1)))
              END IF
            END IF
            IF (JSPS.GE.3) THEN
              IF (ZSPS(ISPS,JSPS-1).NE.SVAL.AND.ZSPS(ISPS,JSPS-2).NE.SVA
     +L) THEN
                NEST=NEST+1
                REST=REST+ZSPS(ISPS,JSPS-1)+.5*
     +                    (ZSPS(ISPS,JSPS-2)-ZSPS(ISPS,JSPS-1))*
     +                    ((YSPS(JSPS  )-YSPS(JSPS-1))/
     +                     (YSPS(JSPS-2)-YSPS(JSPS-1)))
              END IF
            END IF
            IF (JSPS.LE.NSPS-2) THEN
              IF (ZSPS(ISPS,JSPS+1).NE.SVAL.AND.ZSPS(ISPS,JSPS+2).NE.SVA
     +L) THEN
                NEST=NEST+1
                REST=REST+ZSPS(ISPS,JSPS+1)+.5*
     +                    (ZSPS(ISPS,JSPS+2)-ZSPS(ISPS,JSPS+1))*
     +                    ((YSPS(JSPS  )-YSPS(JSPS+1))/
     +                     (YSPS(JSPS+2)-YSPS(JSPS+1)))
              END IF
            END IF
            IF (NEST.GE.MEST) THEN
              NREP=NREP+1
              RWRK(IR01+I)=REST/FLOAT(NEST)
            END IF
10007     CONTINUE
C
          IF (NREP.EQ.0) THEN
C
            MEST=MEST-1
C
            IF (MEST.LE.0) THEN
              CALL SETER ('CPSPS2 - SPECIAL-VALUE REPLACEMENT FAILURE'
     +                                                           ,8,1)
              RETURN
            END IF
C
          ELSE
C
            NSVT=NSVR
C
            DO 10008 I=NSVT,1,-1
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
10008       CONTINUE
C
            MEST=4
C
          END IF
C
        GO TO 10005
10006   CONTINUE
C
      END IF
C
C Do the interpolation from the sparse array to the dense array.
C
      CALL CPGRWS (RWRK,1,3*MSPS*NSPS+MSPS+NSPS+NSPS,IWSE)
C
      IF (IWSE.NE.0) THEN
        CALL SETER ('CPSPS2 - CANNOT CONTINUE WITHOUT WORKSPACE',9,1)
        RETURN
      ELSE IF (ICFELL('CPSPS2',10).NE.0) THEN
        RETURN
      END IF
C
      CALL MSSRF1 (MSPS,NSPS,XSPS,YSPS,ZSPS,KSPS,RWRK,RWRK,RWRK,RWRK,
     +             RWRK(1),RWRK(1),RWRK(1),RWRK(1),255,RWRK(IR01+1),
     +             RWRK(IR01+1+3*MSPS*NSPS),T3DS,IERR)
C
      IF (IERR.NE.0) THEN
        CALL SETER ('CPSPS2 - ERROR IN CALL TO MSSRF1',11,1)
        RETURN
      END IF
C
      DO 10009 I=1,IZDM
        XCPT=XSPS(1)+(REAL(I-1)/REAL(IZDM-1))*(XSPS(MSPS)-XSPS(1))
        DO 10010 J=1,IZDN
          YCPT=YSPS(1)+(REAL(J-1)/REAL(IZDN-1))*(YSPS(NSPS)-YSPS(1))
          ZDAT(I,J)=MSSRF2 (XCPT,YCPT,MSPS,NSPS,XSPS,YSPS,ZSPS,KSPS,
     +                     RWRK(IR01+1),T3DS)
          IF (ICFELL('CPSPS2',12).NE.0) RETURN
10010   CONTINUE
10009 CONTINUE
C
      LR01=0
C
C If the special-value flag is set, restore the special values to the
C sparse array and fill in the corresponding values in the dense array.
C
      IF (SVAL.NE.0.) THEN
C
        DO 10011 I=1,NSVS
C
          ISPS=IWRK(II01+I)/NSPS+1
          JSPS=MOD(IWRK(II01+I),NSPS)+1
          ZSPS(ISPS,JSPS)=SVAL
          IF (ISPS.EQ.1) THEN
            JBEG=1
            JEND=1
          ELSE IF (ISPS.EQ.MSPS) THEN
            JBEG=IZDM
            JEND=IZDM
          ELSE
            JBEG=MAX(1,MIN(IZDM,INT(((XSPS(ISPS-1)-XSPS(1))/
     +                               (XSPS(MSPS  )-XSPS(1)))*
     +                                REAL(IZDM-1)+1.E-6)+2))
            JEND=MAX(1,MIN(IZDM,INT(((XSPS(ISPS+1)-XSPS(1))/
     +                               (XSPS(MSPS  )-XSPS(1)))*
     +                                REAL(IZDM-1)-1.E-6)+1))
          END IF
          IF (JSPS.EQ.1) THEN
            KBEG=1
            KEND=1
          ELSE IF (JSPS.EQ.NSPS) THEN
            KBEG=IZDN
            KEND=IZDN
          ELSE
            KBEG=MAX(1,MIN(IZDN,INT(((YSPS(JSPS-1)-YSPS(1))/
     +                               (YSPS(NSPS  )-YSPS(1)))*
     +                                REAL(IZDN-1)+1.E-6)+2))
            KEND=MAX(1,MIN(IZDN,INT(((YSPS(JSPS+1)-YSPS(1))/
     +                               (YSPS(NSPS  )-YSPS(1)))*
     +                                REAL(IZDN-1)-1.E-6)+1))
          END IF
          DO 10012 J=JBEG,JEND
            DO 10013 K=KBEG,KEND
              ZDAT(J,K)=SVAL
10013       CONTINUE
10012     CONTINUE
C
10011   CONTINUE
C
        LI01=0
C
      END IF
C
C CPINIT does the rest.
C
      CALL CPINIT (ZDAT,RWRK,IWRK)
      IF (ICFELL('CPSPS2',13).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
