C
C $Id: cpgiws.f,v 1.2 1994-03-17 01:50:58 kennison Exp $
C
      SUBROUTINE CPGIWS (IWRK,IOWS,LOWS,IERR)
C
      DIMENSION IWRK(*)
C
C This subroutine is called to get a block of space, of a specified
C size, in the user's integer workspace array.  The block may or may
C not have been used before.
C
C IOWS is the index (into the arrays IIWS and LIWS) of the values
C saying where the block starts and how long it is.
C
C LOWS is the desired length.  The value 0 indicates that the maximum
C amount is desired; it will be replaced by the actual amount assigned.
C
C IERR is a returned error flag.  It will be 0 if no workspace overflow
C occurred, 1 if an overflow did occur.
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
C Check for argument error.
C
      IF (IOWS.LT.1.OR.IOWS.GT.2.OR.LOWS.LT.0) THEN
        CALL SETER ('CPGIWS - ARGUMENT ERROR - SEE SPECIALIST',1,1)
        RETURN
      END IF
C
C Clear error flag.
C
      IERR=0
C
C See if the desired amount of space is available.
C
      NLFT=LIWK
C
      DO 10001 I=1,2
        IF (I.NE.IOWS.AND.LIWS(I).GT.0) NLFT=NLFT-LIWS(I)
10001 CONTINUE
C
C If caller wants it all, arrange for that.
C
      IF (LOWS.LE.0) LOWS=NLFT
C
C Update the integer-workspace-used parameter.
C
      IIWU=MAX(IIWU,LIWK-NLFT+LOWS)
C
C If too little space is available, take whatever action the user has
C specified.
C
      IF (NLFT.LT.LOWS) THEN
        IF (IWSO.LE.1)
     +    WRITE (I1MACH(4),'('' CPGIWS'',
     +                       I8,'' WORDS REQUESTED'',
     +                       I8,'' WORDS AVAILABLE'')') LOWS,NLFT
        IF (IWSO.LE.0) THEN
          CALL SETER ('CPGIWS - INTEGER WORKSPACE OVERFLOW',2,2)
          STOP
        ELSE IF (IWSO.GE.3) THEN
          CALL SETER ('CPGIWS - INTEGER WORKSPACE OVERFLOW',3,1)
        ELSE
          IERR=1
        END IF
        RETURN
      END IF
C
C It may be that a reduction in size has been requested.  That's easy.
C
      IF (LOWS.LE.LIWS(IOWS)) THEN
        LIWS(IOWS)=LOWS
        RETURN
      END IF
C
C Otherwise, what we do depends on whether the workspace associated
C with this index exists already.
C
      IF (LIWS(IOWS).LE.0) THEN
C
C It does not exist.  Find (or create) an area large enough.  First,
C check for an open space large enough.
C
        JIWS=0
10002   CONTINUE
          KIWS=LIWK
          IMIN=0
          DO 10003 I=1,2
            IF (IIWS(I).GE.JIWS.AND.IIWS(I).LT.KIWS.AND.LIWS(I).GT.0) TH
     +EN
              KIWS=IIWS(I)
              IMIN=I
            END IF
10003     CONTINUE
          IF (KIWS-JIWS.GE.LOWS) THEN
            IIWS(IOWS)=JIWS
            LIWS(IOWS)=LOWS
            RETURN
          END IF
          IF (IMIN.NE.0) THEN
            JIWS=IIWS(IMIN)+LIWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0)) GO TO 10002
C
C If no space large enough was found, pack all the existing blocks
C into the beginning of the array, which will leave enough space at
C the end of it.
C
        JIWS=0
10004   CONTINUE
          KIWS=LIWK
          IMIN=0
          DO 10005 I=1,2
            IF (IIWS(I).GE.JIWS.AND.IIWS(I).LT.KIWS.AND.LIWS(I).GT.0) TH
     +EN
              KIWS=IIWS(I)
              IMIN=I
            END IF
10005     CONTINUE
          IF (IMIN.NE.0) THEN
            IF (IIWS(IMIN).NE.JIWS) THEN
              DO 10006 I=1,LIWS(IMIN)
                IWRK(JIWS+I)=IWRK(IIWS(IMIN)+I)
10006         CONTINUE
              IIWS(IMIN)=JIWS
            END IF
            JIWS=JIWS+LIWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0)) GO TO 10004
C
        IIWS(IOWS)=JIWS
        LIWS(IOWS)=LOWS
        RETURN
C
      ELSE
C
C It exists.  Extend its length.  First, see if that can be done
C without moving anything around.
C
        JIWS=IIWS(IOWS)+LIWS(IOWS)
        KIWS=LIWK
        DO 10007 I=1,2
          IF (IIWS(I).GE.JIWS.AND.IIWS(I).LT.KIWS.AND.LIWS(I).GT.0) THEN
            KIWS=IIWS(I)
          END IF
10007   CONTINUE
        IF (KIWS-JIWS.GE.LOWS) THEN
          LIWS(IOWS)=LOWS
          RETURN
        END IF
C
C Blocks have to be moved.  Move those that precede the one to be
C lengthened and that one itself toward the beginning of the workspace.
C
        JIWS=0
10008   CONTINUE
          KIWS=LIWK
          IMIN=0
          DO 10009 I=1,2
            IF (IIWS(I).GE.JIWS.AND.IIWS(I).LT.KIWS.AND.LIWS(I).GT.0) TH
     +EN
              KIWS=IIWS(I)
              IMIN=I
            END IF
10009     CONTINUE
          IF (IMIN.NE.0) THEN
            IF (IIWS(IMIN).NE.JIWS) THEN
              DO 10010 I=1,LIWS(IMIN)
                IWRK(JIWS+I)=IWRK(IIWS(IMIN)+I)
10010         CONTINUE
              IIWS(IMIN)=JIWS
            END IF
            JIWS=JIWS+LIWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0.OR.IMIN.EQ.IOWS)) GO TO 10008
C
C Move blocks that follow the one to be lengthened toward the end of
C the workspace.
C
        KIWS=LIWK
10011   CONTINUE
          JIWS=IIWS(IOWS)+LIWS(IOWS)
          IMAX=0
          DO 10012 I=1,2
            IF (IIWS(I).GE.JIWS.AND.IIWS(I).LT.KIWS.AND.LIWS(I).GT.0) TH
     +EN
              JIWS=IIWS(I)+LIWS(I)
              IMAX=I
            END IF
10012     CONTINUE
          IF (IMAX.NE.0) THEN
            IF (JIWS.NE.KIWS) THEN
              DO 10013 I=LIWS(IMAX),1,-1
                IWRK(KIWS-LIWS(IMAX)+I)=IWRK(JIWS-LIWS(IMAX)+I)
10013         CONTINUE
              IIWS(IMAX)=KIWS-LIWS(IMAX)
            END IF
            KIWS=IIWS(IMAX)
          END IF
        IF (.NOT.(IMAX.EQ.0)) GO TO 10011
C
C There should now be room, so just update the length of the block.
C
        LIWS(IOWS)=LOWS
        RETURN
C
      END IF
C
      END
