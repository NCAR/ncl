C
C	$Id: cptres.f,v 1.1.1.1 1992-04-17 22:32:50 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CPTRES (ZDAT,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,NRWK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C This routine traces the edge of the special-value area.  Control is
C passed back to the caller with each piece of the edge for processing.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C IJMP is initially set to zero by the caller.  Upon return, it will be
C zero if all segments have been traced and processed, non-zero if the
C caller is expected to process a segment and recall CPTRES.
C
C IAIC is both an input and an output variable.  If it is initially set
C to -9 by the caller, it will not be changed by CPTRES and no attempt
C will be made to determine what area identifier should be used for the
C area on the contoured side of the edge of the special-value area.  If
C its initial value is 0, it will have been updated, upon every return
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
      CHARACTER*32 CLDP
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
C Define an arithmetic statement function for use below.
C
      FRCT(ZDT1,ZDT2)=(CLEV(ICLV)-ZDT1)/(ZDT2-ZDT1)
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (101,102,108,109,110,111) , IJMP
C
C If there are no special values in the field, do nothing.
C
      IF (SVAL.EQ.0.) RETURN
C
C Assign space to use for storing the X and Y coordinates of points.
C
      MPLS=LRWC
      CALL CPGRWS (RWRK,1,2*MPLS,IWSE)
      IF (IWSE.NE.0) RETURN
C
C Compute constants required to get from indices to X and Y coordinates.
C
      RZDM=(XATM-XAT1)/REAL(IZDM-1)
      RZDN=(YATN-YAT1)/REAL(IZDN-1)
C
C Compute quantities used to see if two points are essentially
C different from one another.
C
      SMLX=.001*ABS(XWDR-XWDL)
      SMLY=.001*ABS(YWDT-YWDB)
C
C Zero the count of horizontal segments seen.
C
      NHSS=0
C
C Search.
C
        I = 1
        GO TO 10003
10001   CONTINUE
        I =I +1
10003   CONTINUE
        IF (I .GT.(IZDM-1)) GO TO 10002
          J = 1
          GO TO 10006
10004     CONTINUE
          J =J +1
10006     CONTINUE
          IF (J .GT.(IZDN-1)) GO TO 10005
          IF (.NOT.(J.EQ.1)) GO TO 10007
            IF (.NOT.(ZDAT(I,J).EQ.SVAL.OR.ZDAT(I+1,J).EQ.SVAL.OR.ZDAT(I
     +,J+1).EQ.SVAL.OR.ZDAT(I+1,J+1).EQ.SVAL)) GO TO 10008
              GO TO 10010
10008       CONTINUE
          GO TO 10011
10007     CONTINUE
            IF (.NOT.(ZDAT(I,J-1).NE.SVAL.AND.ZDAT(I+1,J-1).NE.SVAL.AND.
     +ZDAT(I,J).NE.SVAL.AND.ZDAT(I+1,J).NE.SVAL.AND.(ZDAT(I,J+1).EQ.SVAL
     +.OR.ZDAT(I+1,J+1).EQ.SVAL))) GO TO 10012
              GO TO 10010
10012       CONTINUE
10011     CONTINUE
          GO TO 10014
10010     CONTINUE
            NPLS=0
            RUDN=0.
            IPXY=IZDN*I+J
            DO 10015 K=1,NHSS
              IF (IPXY.EQ.IWRK(II01+K)) GO TO 104
10015       CONTINUE
            IF (.NOT.(NHSS.GE.LI01)) GO TO 10016
              CALL CPGIWS (IWRK,1,LI01+100,IWSE)
              IF (IWSE.NE.0) GO TO 105
10016       CONTINUE
            NHSS=NHSS+1
            IWRK(II01+NHSS)=IPXY
            XCES=REAL(I)
            YCES=REAL(J)
            ZCES=ZDAT(I,J)
            XCND=REAL(I)
            YCND=REAL(J)
            ZCND=ZDAT(I,J)
            ASSIGN 10017 TO L10018
            GO TO 10018
10017       CONTINUE
            INSX=I
            INSY=J
            INOX=I
            INOY=J
            INDX=I+1
            INDY=J
            IDIR=5
10019       CONTINUE
              IF (.NOT.((INDX.EQ.INOX.AND.(INDX.EQ.1.OR.INDX.EQ.IZDM)).O
     +R.(INDY.EQ.INOY.AND.(INDY.EQ.1.OR.INDY.EQ.IZDN)))) GO TO 10020
                IF (.NOT.(NPLS.NE.0)) GO TO 10021
                  IJMP=1
                  IRW1=IR01
                  IRW2=IR01+MPLS
                  NRWK=NPLS
                  RETURN
  101             NPLS=0
                  RUDN=0.
10021           CONTINUE
                XCES=REAL(INDX)
                YCES=REAL(INDY)
                ZCES=ZDAT(INDX,INDY)
                XCND=XCES
                YCND=YCES
                ZCND=ZCES
                ASSIGN 10022 TO L10018
                GO TO 10018
10022           CONTINUE
              GO TO 10023
10020         CONTINUE
                ASSIGN 10024 TO L10025
                GO TO 10025
10024           CONTINUE
10023         CONTINUE
              IF (.NOT.(INDX.EQ.INSX.AND.INDY.EQ.INSY)) GO TO 10026
                IF (.NOT.(NPLS.NE.0)) GO TO 10027
                  IJMP=2
                  IRW1=IR01
                  IRW2=IR01+MPLS
                  NRWK=NPLS
                  RETURN
10027           CONTINUE
  102       CONTINUE
                GO TO 10028
10026         CONTINUE
              INOX=INDX
              INOY=INDY
              IDIR=MOD(IDIR+1,8)+1
              DO 10029 K=1,3
                IF (.NOT.(IDIR.EQ.5)) GO TO 10030
                  IF (.NOT.(INOX.NE.IZDM.AND.INOY.NE.IZDN)) GO TO 10031
                    IF (.NOT.(INOY.EQ.1)) GO TO 10032
                      IF (ZDAT(INOX  ,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX+1,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX  ,INOY+1).EQ.SVAL.OR.
     +                    ZDAT(INOX+1,INOY+1).EQ.SVAL)
     +                                                       GO TO 103
                    GO TO 10033
10032               CONTINUE
                      IF (ZDAT(INOX  ,INOY-1).NE.SVAL.AND.
     +                    ZDAT(INOX+1,INOY-1).NE.SVAL.AND.
     +                    ZDAT(INOX  ,INOY  ).NE.SVAL.AND.
     +                    ZDAT(INOX+1,INOY  ).NE.SVAL.AND.
     +                   (ZDAT(INOX  ,INOY+1).EQ.SVAL.OR.
     +                    ZDAT(INOX+1,INOY+1).EQ.SVAL))
     +                                                       GO TO 103
10033               CONTINUE
10031             CONTINUE
                GO TO 10034
10030           CONTINUE
                IF (.NOT.(IDIR.EQ.3)) GO TO 10035
                  IF (.NOT.(INOX.NE.1.AND.INOY.NE.IZDN)) GO TO 10036
                    IF (.NOT.(INOX.EQ.IZDM)) GO TO 10037
                      IF (ZDAT(INOX  ,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX  ,INOY+1).EQ.SVAL.OR.
     +                    ZDAT(INOX-1,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX-1,INOY+1).EQ.SVAL)
     +                                                       GO TO 103
                    GO TO 10038
10037               CONTINUE
                      IF (ZDAT(INOX+1,INOY  ).NE.SVAL.AND.
     +                    ZDAT(INOX+1,INOY+1).NE.SVAL.AND.
     +                    ZDAT(INOX  ,INOY  ).NE.SVAL.AND.
     +                    ZDAT(INOX  ,INOY+1).NE.SVAL.AND.
     +                   (ZDAT(INOX-1,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX-1,INOY+1).EQ.SVAL))
     +                                                       GO TO 103
10038               CONTINUE
10036             CONTINUE
                GO TO 10034
10035           CONTINUE
                IF (.NOT.(IDIR.EQ.1)) GO TO 10039
                  IF (.NOT.(INOX.NE.1.AND.INOY.NE.1)) GO TO 10040
                    IF (.NOT.(INOY.EQ.IZDN)) GO TO 10041
                      IF (ZDAT(INOX  ,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX-1,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX  ,INOY-1).EQ.SVAL.OR.
     +                    ZDAT(INOX-1,INOY-1).EQ.SVAL)
     +                                                       GO TO 103
                    GO TO 10042
10041               CONTINUE
                      IF (ZDAT(INOX  ,INOY+1).NE.SVAL.AND.
     +                    ZDAT(INOX-1,INOY+1).NE.SVAL.AND.
     +                    ZDAT(INOX  ,INOY  ).NE.SVAL.AND.
     +                    ZDAT(INOX-1,INOY  ).NE.SVAL.AND.
     +                   (ZDAT(INOX  ,INOY-1).EQ.SVAL.OR.
     +                    ZDAT(INOX-1,INOY-1).EQ.SVAL))
     +                                                       GO TO 103
10042               CONTINUE
10040             CONTINUE
                GO TO 10034
10039           CONTINUE
                  IF (.NOT.(INOX.NE.IZDM.AND.INOY.NE.1)) GO TO 10043
                    IF (.NOT.(INOX.EQ.1)) GO TO 10044
                      IF (ZDAT(INOX  ,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX  ,INOY-1).EQ.SVAL.OR.
     +                    ZDAT(INOX+1,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX+1,INOY-1).EQ.SVAL)
     +                                                       GO TO 103
                    GO TO 10045
10044               CONTINUE
                      IF (ZDAT(INOX-1,INOY  ).NE.SVAL.AND.
     +                    ZDAT(INOX-1,INOY-1).NE.SVAL.AND.
     +                    ZDAT(INOX  ,INOY  ).NE.SVAL.AND.
     +                    ZDAT(INOX  ,INOY-1).NE.SVAL.AND.
     +                   (ZDAT(INOX+1,INOY  ).EQ.SVAL.OR.
     +                    ZDAT(INOX+1,INOY-1).EQ.SVAL))
     +                                                       GO TO 103
10045               CONTINUE
10043             CONTINUE
10034           CONTINUE
                IDIR=MOD(IDIR+5,8)+1
10029         CONTINUE
              CALL SETER ('CPTRES - ALGORITHM FAILURE - SEE SPECIALIST'
     +                                                           ,1,2)
              STOP
  103         INDX=INOX+INCX(IDIR)
              INDY=INOY+INCY(IDIR)
              IF (.NOT.(IDIR.EQ.5)) GO TO 10046
                IF (.NOT.(NHSS.GE.LI01)) GO TO 10047
                  CALL CPGIWS (IWRK,1,LI01+100,IWSE)
                  IF (IWSE.NE.0) GO TO 105
10047           CONTINUE
                NHSS=NHSS+1
                IWRK(II01+NHSS)=IZDN*INOX+INOY
10046         CONTINUE
            GO TO 10019
10028       CONTINUE
10014     CONTINUE
  104   CONTINUE
        GO TO 10004
10005   CONTINUE
      GO TO 10001
10002 CONTINUE
C
C Release the workspaces and let the caller know we're done.
C
  105 LI01=0
      LR01=0
      IJMP=0
C
C Done.
C
      RETURN
C
C The following internal procedure processes a segment along the edge
C of the grid.
C
10025 CONTINUE
        XCSS=XCES
        YCSS=YCES
        ZCSS=ZCES
        XCES=REAL(INDX)
        YCES=REAL(INDY)
        ZCES=ZDAT(INDX,INDY)
          INTP = 1
          GO TO 10050
10048     CONTINUE
          INTP =INTP +1
10050     CONTINUE
          IF (INTP .GT.(IABS(IPIE)+1)) GO TO 10049
          FINT=REAL(INTP)/REAL(IABS(IPIE)+1)
          XCOD=XCND
          YCOD=YCND
          ZCOD=ZCND
          XCOU=XCNU
          YCOU=YCNU
          IVOU=IVNU
          XCND=(1.-FINT)*XCSS+FINT*XCES
          YCND=(1.-FINT)*YCSS+FINT*YCES
          IF (.NOT.(SVAL.EQ.0..OR.(ZCSS.NE.SVAL.AND.ZCES.NE.SVAL)))
     +    GO TO 10051
            ZCND=(1.-FINT)*ZCSS+FINT*ZCES
          GO TO 10052
10051     CONTINUE
            ZCND=SVAL
10052     CONTINUE
          ASSIGN 10053 TO L10018
          GO TO 10018
10053     CONTINUE
          IF (.NOT.(SVAL.EQ.0..OR.(ZCOD.NE.SVAL.AND.ZCND.NE.SVAL)))
     +    GO TO 10054
            IF (.NOT.(ZCOD.LT.ZCND)) GO TO 10055
                K = 1
                GO TO 10058
10056           CONTINUE
                K =K +1
10058           CONTINUE
                IF (K .GT.(NCLV)) GO TO 10057
                ICLV=ICLP(K)
                IF (.NOT.(CLEV(ICLV).GT.ZCOD.AND.CLEV(ICLV).LT.ZCND.AND.
     +(IAIA(ICLV).NE.0.OR.IAIB(ICLV).NE.0))) GO TO 10059
                  ASSIGN 10060 TO L10061
                  GO TO 10061
10060             CONTINUE
10059           CONTINUE
              GO TO 10056
10057         CONTINUE
            GO TO 10062
10055       CONTINUE
            IF (.NOT.(ZCND.LT.ZCOD)) GO TO 10063
                K = NCLV
                GO TO 10066
10064           CONTINUE
                K =K -1
10066           CONTINUE
                IF (K .LT.(1)) GO TO 10065
                ICLV=ICLP(K)
                IF (.NOT.(CLEV(ICLV).GT.ZCND.AND.CLEV(ICLV).LT.ZCOD.AND.
     +(IAIA(ICLV).NE.0.OR.IAIB(ICLV).NE.0))) GO TO 10067
                  ASSIGN 10068 TO L10061
                  GO TO 10061
10068             CONTINUE
10067           CONTINUE
              GO TO 10064
10065         CONTINUE
10062       CONTINUE
10063       CONTINUE
10054     CONTINUE
          IF (.NOT.(IPIE.LT.0.AND.INTP.NE.IABS(IPIE)+1)) GO TO 10069
            IFOP=0
          GO TO 10070
10069     CONTINUE
            IFOP=1
10070     CONTINUE
          ASSIGN 10071 TO L10072
          GO TO 10072
10071     CONTINUE
        GO TO 10048
10049   CONTINUE
      GO TO L10025 , (10024)
C
C The following internal procedure interpolates a point where a contour
C line intersects the piece of the edge segment that we're working on.
C
10061 CONTINUE
        XCSD=XCND
        YCSD=YCND
        ZCSD=ZCND
        XCSU=XCNU
        YCSU=YCNU
        IVSU=IVNU
        XCND=XCOD+(XCND-XCOD)*FRCT(ZCOD,ZCND)
        YCND=YCOD+(YCND-YCOD)*FRCT(ZCOD,ZCND)
        ZCND=CLEV(ICLV)
        ASSIGN 10073 TO L10018
        GO TO 10018
10073   CONTINUE
        IFOP=1
        ASSIGN 10074 TO L10072
        GO TO 10072
10074   CONTINUE
        XCOD=XCND
        YCOD=YCND
        ZCOD=ZCND
        XCOU=XCNU
        YCOU=YCNU
        IVOU=IVNU
        XCND=XCSD
        YCND=YCSD
        ZCND=ZCSD
        XCNU=XCSU
        YCNU=YCSU
        IVNU=IVSU
      GO TO L10061 , (10068,10060)
C
C The following internal procedure processes a piece of a segment.
C There are several cases, depending on whether both endpoints are
C visible, neither endpoint is visible, or just one of them is visible.
C
10072 CONTINUE
C
        IAID=IAIC
C
        IF (.NOT.(IAIC.NE.-9)) GO TO 10075
          IF (.NOT.(SVAL.NE.0..AND.(ZCND.EQ.SVAL.OR.ZCOD.EQ.SVAL)))
     +    GO TO 10076
            IAID=IAIA(258)
          GO TO 10077
10076     CONTINUE
            IF (.NOT.(NCLV.LE.0)) GO TO 10078
              IAID=1
            GO TO 10079
10078       CONTINUE
              IAID=0
              ZAVG=.5*(ZCND+ZCOD)
              DO 10080 L=1,NCLV
                JCLV=ICLP(L)
                IF (.NOT.(ZAVG.LE.CLEV(JCLV))) GO TO 10081
                  IF (.NOT.(IAIB(JCLV).NE.0)) GO TO 10082
                    IAID=IAIB(JCLV)
                    GO TO 107
10082             CONTINUE
                    IF (L.EQ.NCLV) GO TO 106
                    IF (ZAVG.NE.CLEV(JCLV).AND.
     +                  CLEV(JCLV).NE.CLEV(ICLP(L+1))) GO TO 106
10081           CONTINUE
10080         CONTINUE
  106         CONTINUE
              DO 10083 L=NCLV,1,-1
                JCLV=ICLP(L)
                IF (.NOT.(ZAVG.GE.CLEV(JCLV))) GO TO 10084
                  IF (.NOT.(IAIA(JCLV).NE.0)) GO TO 10085
                    IAID=IAIA(JCLV)
                    GO TO 107
10085             CONTINUE
                    IF (L.EQ.1) GO TO 107
                    IF (ZAVG.NE.CLEV(JCLV).AND.
     +                  CLEV(JCLV).NE.CLEV(ICLP(L-1))) GO TO 107
10084           CONTINUE
10083         CONTINUE
  107       CONTINUE
10079       CONTINUE
10077     CONTINUE
10075   CONTINUE
C
        IF (.NOT.(NPLS.EQ.0)) GO TO 10086
          IF (.NOT.(IVOU.NE.0)) GO TO 10087
            NPLS=1
            RWRK(IR01+NPLS)=XCOU
            RWRK(IR01+MPLS+NPLS)=YCOU
          GO TO 10088
10087     CONTINUE
            IF (.NOT.(IVNU.NE.0)) GO TO 10089
              XCID=XCOD
              YCID=YCOD
              XCVD=XCND
              YCVD=YCND
              XCVU=XCNU
              YCVU=YCNU
              ASSIGN 10090 TO L10091
              GO TO 10091
10090         CONTINUE
              XCOD=XCVD
              YCOD=YCVD
              XCOU=XCVU
              YCOU=YCVU
              IVOU=1
10089       CONTINUE
10088     CONTINUE
        GO TO 10092
10086   CONTINUE
        IF (.NOT.(NPLS.EQ.MPLS.OR.IAID.NE.IAIC)) GO TO 10093
          XSAV=RWRK(IR01+NPLS)
          YSAV=RWRK(IR01+MPLS+NPLS)
          IJMP=3
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  108     RWRK(IR01+1)=XSAV
          RWRK(IR01+MPLS+1)=YSAV
          NPLS=1
10092   CONTINUE
10093   CONTINUE
C
        IF (.NOT.(IVNU.NE.0)) GO TO 10094
          IF (.NOT.(IMPF.NE.0.AND.(XCND.NE.XCOD.OR.YCND.NE.YCOD)))
     +    GO TO 10095
            RUDO=RUDN
            RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +           (ABS(XCND-XCOD)+ABS(YCND-YCOD))
            IF (.NOT.(RUDN.GT.10.*RUDO)) GO TO 10096
              ASSIGN 10097 TO L10098
              GO TO 10098
10097         CONTINUE
10096       CONTINUE
10095     CONTINUE
          IF (.NOT.(IFOP.NE.0)) GO TO 10099
            NPLS=NPLS+1
            RWRK(IR01+NPLS)=XCNU
            RWRK(IR01+MPLS+NPLS)=YCNU
10099     CONTINUE
        GO TO 10100
10094   CONTINUE
          IF (.NOT.(IVOU.NE.0)) GO TO 10101
            XCVD=XCOD
            YCVD=YCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XCND
            YCID=YCND
            ASSIGN 10102 TO L10091
            GO TO 10091
10102       CONTINUE
            IJMP=4
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
  109       NPLS=0
            RUDN=0.
10101     CONTINUE
10100   CONTINUE
C
        IF (IAIC.NE.-9) IAIC=IAID
C
      GO TO L10072 , (10074,10071)
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the edge is seen.  It checks
C for a possible discontinuity in the mapping function (as can happen,
C for example, when a cylindrical equidistant projection is being used);
C if there is such a discontinuity, we must generate a final point on
C one side of it, dump the polyline, and then start a new polyline on
C the other side.
C
10098 CONTINUE
        XC1D=XCOD
        YC1D=YCOD
        XC1U=XCOU
        YC1U=YCOU
        XC2D=XCND
        YC2D=YCND
        XC2U=XCNU
        YC2U=YCNU
        ITMP=0
10103   CONTINUE
          XC3D=(XC1D+XC2D)/2.
          YC3D=(YC1D+YC2D)/2.
          CALL CPMPXY (IMPF,XAT1+RZDM*(XC3D-1.),YAT1+RZDN*(YC3D-1.),
     +                                                      XC3U,YC3U)
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10104
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(DST1.LT.DST2)) GO TO 10105
              IF (XC3D.EQ.XC1D.AND.YC3D.EQ.YC1D) GO TO 10106
              XC1D=XC3D
              YC1D=YC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10107
10105       CONTINUE
              IF (XC3D.EQ.XC2D.AND.YC3D.EQ.YC2D) GO TO 10106
              XC2D=XC3D
              YC2D=YC3D
              XC2U=XC3U
              YC2U=YC3U
10107       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10106
          GO TO 10108
10104     CONTINUE
            XCVD=XCOD
            YCVD=YCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XC3D
            YCID=YC3D
            ASSIGN 10109 TO L10091
            GO TO 10091
10109       CONTINUE
            IJMP=5
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
  110       NPLS=0
            RUDN=0.
            XCID=XC3D
            YCID=YC3D
            XCVD=XCND
            YCVD=YCND
            XCVU=XCNU
            YCVU=YCNU
            ASSIGN 10110 TO L10091
            GO TO 10091
10110       CONTINUE
            ITMP=1000
            GO TO 10106
10108     CONTINUE
        GO TO 10103
10106   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10111
          NPLS=NPLS+1
          RWRK(IR01+NPLS)=XC1U
          RWRK(IR01+MPLS+NPLS)=YC1U
          IJMP=6
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  111     NPLS=1
          RWRK(IR01+1)=XC2U
          RWRK(IR01+MPLS+1)=YC2U
          RUDN=0.
10111   CONTINUE
      GO TO L10098 , (10097)
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10091 CONTINUE
        ITMP=0
10112   CONTINUE
          XCHD=(XCVD+XCID)/2.
          YCHD=(YCVD+YCID)/2.
          CALL CPMPXY (IMPF,XAT1+RZDM*(XCHD-1.),YAT1+RZDN*(YCHD-1.),
     +                                                      XCHU,YCHU)
          IF (.NOT.(XCHU.NE.OORV)) GO TO 10113
            IF (XCHD.EQ.XCVD.AND.YCHD.EQ.YCVD) GO TO 10114
            XCVD=XCHD
            YCVD=YCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10115
10113     CONTINUE
            IF (XCHD.EQ.XCID.AND.YCHD.EQ.YCID) GO TO 10114
            XCID=XCHD
            YCID=YCHD
10115     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10114
        GO TO 10112
10114   CONTINUE
        NPLS=NPLS+1
        RWRK(IR01+NPLS)=XCVU
        RWRK(IR01+MPLS+NPLS)=YCVU
      GO TO L10091 , (10110,10109,10102,10090)
C
C The following internal procedure is given the data-system coordinates
C of a point (XCND,YCND) and computes the user-system coordinates of
C the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10018 CONTINUE
C
        XCNU=XAT1+RZDM*(XCND-1.)
        YCNU=YAT1+RZDN*(YCND-1.)
        IVNU=1
C
        IF (.NOT.(IMPF.NE.0)) GO TO 10116
          XTMP=XCNU
          YTMP=YCNU
          CALL CPMPXY (IMPF,XTMP,YTMP,XCNU,YCNU)
          IF ((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)) IVNU=0
10116   CONTINUE
C
      GO TO L10018 , (10073,10053,10022,10017)
C
      END
