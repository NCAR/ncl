      SUBROUTINE HLUCPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*),IAMA(*)
C
C This routine adds contour lines to an area map.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C IAMA is the user's area map.
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
      COMMON /CPCOM1/ IDUF,IGCL,IGLB,IGRM,IGRN,IGVS,IHCF,IHLE,IHLX
      COMMON /CPCOM1/ IHLY,IIWS(2),IIWU,ILBC,IMPF,INCX(8),INCY(8)
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
C Declare the common block that holds the clipping-window parameters
C for the routine CPWLAM.
C
      COMMON /CPWCMN/ XMIN,XMAX,YMIN,YMAX
C
C Define a couple of little workspace arrays required by CPTROE.
C
      DIMENSION RWKL(12),RWKR(12)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPCLAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPCLAM - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CPCLAM',3).NE.0) RETURN
C
C If no contour levels are defined, try to pick a set of levels.
C
      IF (NCLV.LE.0) THEN
        CALL CPPKCL (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPCLAM',4).NE.0) RETURN
      END IF
C
C Get indices for the contour levels in ascending order.
C
      IF (NCLV.GT.0) CALL CPSORT (CLEV,NCLV,ICLP)
C
C Get a little real workspace to use and re-do the call to SET so that
C we can use fractional coordinates.
C
      CALL CPGRWS (RWRK,1,10,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CPCLAM',5).NE.0) RETURN
      CALL SET (XVPL,XVPR,YVPB,YVPT,XVPL,XVPR,YVPB,YVPT,1)
      IF (ICFELL('CPCLAM',6).NE.0) RETURN
C
C Add the viewport perimeter to the area map.  This avoids problems
C which arise when mapping is turned on and the mapping function has
C a discontinuity (as, for example, a cylindrical equidistant EZMAP
C projection does).  This used to be done only when the mapping flag
C was turned on, but now it is done unconditionally, so as to force
C an area identifier of "-1" outside the viewport.  Also, as of
C 06/04/91, the area identifier on the inside of the viewport is set
C to zero, rather than to a value associated with a contour level.
C (And, as of 8/24/04, I'm pinching in the viewport just slightly.)
C
      RWRK(IR01+ 1)=XVPL+.000001
      RWRK(IR01+ 2)=XVPR-.000001
      RWRK(IR01+ 3)=XVPR-.000001
      RWRK(IR01+ 4)=XVPL+.000001
      RWRK(IR01+ 5)=XVPL+.000001
      RWRK(IR01+ 6)=YVPB+.000001
      RWRK(IR01+ 7)=YVPB+.000001
      RWRK(IR01+ 8)=YVPT-.000001
      RWRK(IR01+ 9)=YVPT-.000001
      RWRK(IR01+10)=YVPB+.000001
C
      CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGCL,0,-1)
      IF (ICFELL('CPCLAM',7).NE.0) RETURN
C
C If it is to be done, put into the area map edges creating a set of
C vertical strips.
C
      IF (NOVS.NE.0) THEN
        CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGVS,0,-1)
        IF (ICFELL('CPCLAM',8).NE.0) RETURN
        DO 10001 IOVS=1,NOVS-1
          RWRK(IR01+1)=XVPL+REAL(IOVS)*(XVPR-XVPL)/REAL(NOVS)
          RWRK(IR01+2)=RWRK(IR01+1)
          CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+9),2,IGVS,0,0)
          IF (ICFELL('CPCLAM',9).NE.0) RETURN
10001   CONTINUE
      END IF
C
C Discard the real workspace used above and re-call SET.
C
      LR01=0
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CPCLAM',10).NE.0) RETURN
C
C Put edges of areas which are invisible into the area map.  This one
C is done first because the area-identifier information on the visible
C side is not as good as that provided by the other edges.  Of course,
C it is only done if the mapping flag is turned on and there is the
C possibility that some points are invisible under the mapping.
C
      IF (IMPF.NE.0.AND.OORV.NE.0.) THEN
C
C There are two ways to trace these edges, depending on whether the
C user-supplied CPMPXY will do the inverse transformation or not.  By
C convention, if CPMPXY is called with its first argument equal to zero
C and its second argument equal to the real equivalent of a particular
C mapping index, the third argument will be returned as a 0. if neither
C mapping is defined, as a 1. if only the forward mapping is defined,
C as a 2. if only the inverse mapping is defined, and as a 3. if both
C the forward and the inverse mappings are defined.  Find out if the
C inverse mapping is available.
C
        TST1=REAL(IMPF)
        TST2=0.
C
        CALL HLUCPMPXY (0,TST1,TST2,TST3,TST4)
        IF (ICFELL('CPCLAM',11).NE.0) RETURN
C
        IF (TST2.NE.2..AND.TST2.NE.3.) THEN
C
C CPMPXY will not do the inverse transformation.  We do the best we can.
C
          IJMP=0
          IAIC=0
C
10002     CONTINUE
            CALL CPTREV (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
            IF (ICFELL('CPCLAM',12).NE.0) RETURN
            IF (IJMP.EQ.0) GO TO 10003
            IF (MIRO.EQ.0) THEN
              CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,IGCL,
     +                                              IAIA(259),IAIC)
              IF (ICFELL('CPCLAM',13).NE.0) RETURN
            ELSE
              CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,IGCL,
     +                                              IAIC,IAIA(259))
              IF (ICFELL('CPCLAM',14).NE.0) RETURN
            END IF
          GO TO 10002
10003     CONTINUE
C
C CPMPXY will do the inverse transformation.  We use it to generate the
C desired edges.  Note that mirror imaging, which can affect the use of
C coordinates returned by all of the other edge-tracing routines, does
C not affect the use of coordinates returned by CPTRVE.
C
        ELSE
C
          XMIN=XVPL
          XMAX=XVPR
          YMIN=YVPB
          YMAX=YVPT
C
          IJMP=0
          IAIC=0
C
10004     CONTINUE
            CALL CPTRVE (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
            IF (ICFELL('CPCLAM',15).NE.0) RETURN
            IF (IJMP.EQ.0) GO TO 10005
            DO 10006 I=1,NRWK
              RWRK(IRW1+I)=CUFX(RWRK(IRW1+I))
              IF (ICFELL('CPCLAM',16).NE.0) RETURN
              RWRK(IRW2+I)=CUFY(RWRK(IRW2+I))
              IF (ICFELL('CPCLAM',17).NE.0) RETURN
10006       CONTINUE
            CALL CPTROE (RWRK(IRW1+1),RWRK(IRW2+1),NRWK,+.0005,RWKL,
     +                             IOCF,IAMA,IGCL,IAIA(259),IAIC)
            IF (ICFELL('CPCLAM',18).NE.0) RETURN
            CALL CPTROE (RWRK(IRW1+1),RWRK(IRW2+1),NRWK,-.0005,RWKR,
     +                             IOCF,IAMA,IGCL,IAIA(259),IAIC)
            IF (ICFELL('CPCLAM',19).NE.0) RETURN
          GO TO 10004
10005     CONTINUE
C
        END IF
C
      END IF
C
C Add the edge of the grid.
C
C  2016-08-12:
C  Modified such that the grid boundary is added only if the area
C  outside the grid is to be filled. Otherwise this does not seem
C  to be needed and causes problems;
C  See jira ncl-2443. Similar changes have been made for Conpackt
C
      IJMP=0
      IAIC=0
C
      IF (IAIA(257).GT.0) THEN
10007   CONTINUE
          CALL CPTREG (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
          IF (ICFELL('CPCLAM',20).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10008
          IF (MIRO.EQ.0) THEN
            CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,IGCL,
     +                                              IAIC,IAIA(257))
            IF (ICFELL('CPCLAM',21).NE.0) RETURN
          ELSE
            CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,IGCL,
     +                                              IAIA(257),IAIC)
            IF (ICFELL('CPCLAM',22).NE.0) RETURN
          END IF
        GO TO 10007
10008   CONTINUE
      END IF
C
C Add edges of areas filled with special values.
C
      IJMP=0
      IAIC=0
C
10009 CONTINUE
        CALL CPTRES (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK,0)
        IF (ICFELL('CPCLAM',23).NE.0) RETURN
        IF (IJMP.EQ.0) GO TO 10010
        IF (MIRO.EQ.0) THEN
          CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,IGCL,
     +                                              IAIA(258),IAIC)
          IF (ICFELL('CPCLAM',24).NE.0) RETURN
        ELSE
          CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,IGCL,
     +                                              IAIC,IAIA(258))
          IF (ICFELL('CPCLAM',25).NE.0) RETURN
        END IF
      GO TO 10009
10010 CONTINUE
C
C If the constant-field flag is not set, add the selected contour lines
C to the area map.
C
      CLVP=0.
C
      IF (ICFF.EQ.0) THEN
C
          I = 1
          GO TO 10013
10011     CONTINUE
          I =I +1
10013     CONTINUE
          IF (I .GT.(NCLV)) GO TO 10012
C
          ICLV=ICLP(I)
C
          IF (I.EQ.1.OR.CLEV(ICLV).NE.CLVP) THEN
C
            CLVP=CLEV(ICLV)
C
            IF (CLEV(ICLV).GT.ZMIN.AND.CLEV(ICLV).LT.ZMAX) THEN
C
              JAIA=IAIA(ICLV)
              JAIB=IAIB(ICLV)
C
              DO 10014 J=I+1,NCLV
                JCLV=ICLP(J)
                IF (CLEV(JCLV).NE.CLEV(ICLV)) GO TO 105
                IF (IAIA(JCLV).NE.0) THEN
                  IF (JAIA.NE.0.AND.JAIA.NE.IAIA(JCLV)) THEN
                    CALL SETER ('CPCLAM - CONTRADICTORY AREA-IDENTIFIER
     +INFORMATION',26,1)
                    RETURN
                  END IF
                  JAIA=IAIA(JCLV)
                END IF
                IF (IAIB(JCLV).NE.0) THEN
                  IF (JAIB.NE.0.AND.JAIB.NE.IAIB(JCLV)) THEN
                    CALL SETER ('CPCLAM - CONTRADICTORY AREA-IDENTIFIER
     +INFORMATION',27,1)
                    RETURN
                  END IF
                  JAIB=IAIB(JCLV)
                END IF
10014         CONTINUE
C
  105         IF (JAIA.NE.0.OR.JAIB.NE.0) THEN
C
                IJMP=0
C
10015           CONTINUE
                  CALL CPTRCL (ZDAT,RWRK,IWRK,CLEV(ICLV),IJMP,
     +                                         IRW1,IRW2,NRWK)
                  IF (ICFELL('CPCLAM',28).NE.0) RETURN
                  IF (IJMP.EQ.0) GO TO 10016
                  IF (MIRO.EQ.0) THEN
                    CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,
     +                                                 IGCL,JAIB,JAIA)
                    IF (ICFELL('CPCLAM',29).NE.0) RETURN
                  ELSE
                    CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,
     +                                                 IGCL,JAIA,JAIB)
                    IF (ICFELL('CPCLAM',30).NE.0) RETURN
                  END IF
                GO TO 10015
10016           CONTINUE
C
              END IF
C
            END IF
C
          END IF
C
        GO TO 10011
10012   CONTINUE
C
      END IF
C
C Done.
C
      RETURN
C
      END
