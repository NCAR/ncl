C NCLFORTSTART
      SUBROUTINE TRSSPHX(INTL,IGRIDA,NLONA,NLATA,DA,IGRIDB,NLONB,NLATB,
     +                   DB,WSAVE,LSAVE,LSVMIN,WORK,LWORK,LWKMIN,DWORK,
     +                   LDWORK,IER,JMODE)
      IMPLICIT NONE
      INTEGER INTL,IGRIDA(2),NLONA,NLATA,IGRIDB(2),NLONB,NLATB
      INTEGER LSAVE,LSVMIN,LWORK,LWKMIN,LDWORK,IER,JMODE
      DOUBLE PRECISION DA(*),DB(*),WSAVE(LSAVE),WORK(LWORK)
      DOUBLE PRECISION DWORK(LDWORK)
C NCLEND
      INTEGER IG,IGRDA,IGRDB,LA1,LA2,LB1,LB2,LWA,LWB,IAA,IAB,IBA,IBB
      INTEGER LWK3,LWK4,LW,IW,JB,NT,ISYM,NLAT
      DOUBLE PRECISION RN
c
c     include a save statement to ensure local variables in trssph, set during
c     an intl=0 call, are preserved if trssph is recalled with intl=1
c
      SAVE
c
c     check input arguments
c
      IER = 1
      IF (INTL* (INTL-1).NE.0) RETURN
      IER = 2
      IG = IGRIDA(1)
      IF ((IG-1)* (IG+1)* (IG-2)* (IG+2).NE.0) RETURN
      IER = 3
      IG = IGRIDA(2)
      IF (IG* (IG-1).NE.0) RETURN
      IER = 4
      IF (NLONA.LT.4) RETURN
      IER = 5
      IF (NLATA.LT.3) RETURN
      IER = 6
      IG = IGRIDB(1)
      IF ((IG-1)* (IG+1)* (IG-2)* (IG+2).NE.0) RETURN
      IER = 7
      IG = IGRIDB(2)
      IF (IG* (IG-1).NE.0) RETURN
      IER = 8
      IF (NLONB.LT.4) RETURN
      IER = 9
      IF (NLATB.LT.3) RETURN
      IER = 0

      IGRDA = IABS(IGRIDA(1))
      IGRDB = IABS(IGRIDB(1))
      IF (INTL.EQ.0) THEN
          LA1 = MIN0(NLATA, (NLONA+2)/2)
          LA2 = (NLATA+1)/2
          LB1 = MIN0(NLATB, (NLONB+2)/2)
          LB2 = (NLATB+1)/2
c
c     set saved work space length for analysis
c
          IF (IGRDA.EQ.1) THEN
c
c     saved space for analysis on  equally spaced grid
c
              LWA = 2*NLATA*LA2 + 3* ((LA1-2)* (NLATA+NLATA-LA1-1))/2 +
     +              NLONA + 15
          ELSE
c
c     saved space for analysis on gaussian grid
c
              LWA = NLATA* (2*LA2+3*LA1-2) + 3*LA1* (1-LA1)/2 + NLONA +
     +              15
          END IF
c
c     set wsave pointer
c
          JB = 1 + LWA
c
c     set pointers for spherical harmonic coefs
c
          IAA = 1
          IBA = IAA + LA1*NLATA
          IAB = IBA + LA1*NLATA
          IF (IGRDB.EQ.2) THEN
c
c     set saved work space length for gaussian synthesis
c
              LWB = NLATB* (2*LB2+3*LB1-2) + 3*LB1* (1-LB1)/2 + NLONB +
     +              15
          ELSE
c
c     set saved work space length for equally spaced synthesis
c
              LWB = 2*NLATB*LB2 + 3* ((LB1-2)* (NLATB+NLATB-LB1-1))/2 +
     +              NLONB + 15
          END IF
c
c     set minimum saved work space length
c
          LSVMIN = LWA + LWB
c
c     set remaining harmonic pointer
c
          IBB = IAB + LB1*NLATB
c
c     set pointers for remaining work
c
          IW = IBB + LB1*NLATB
c
c     set remaining work space length in lw
c
          LW = LWORK - IW
          LWK3 = NLATA*NLONA*2
          LWK4 = NLATB*NLONB*2
c
c     set minimum unsaved work space required by trssph
c
          LWKMIN = IW + MAX0(LWK3,LWK4)
c
c     set error flags if saved or unsaved work spaces are insufficient
c
          IER = 10
          IF (LSAVE.LT.LSVMIN) RETURN
          IER = 11
          IF (LWORK.LT.LWKMIN) RETURN
          IER = 13
          NLAT = MAX0(NLATA,NLATB)
          IF (LDWORK.LT.NLAT* (NLAT+4)) RETURN
          IER = 0
          IF (IGRDA.EQ.1) THEN
c
c     initialize wsave for equally spaced analysis
c
              CALL DSHAECI(NLATA,NLONA,WSAVE,LWA,DWORK,LDWORK,IER)
          ELSE
c
c     initialize wsave for gaussian analysis
c
              CALL DSHAGCI(NLATA,NLONA,WSAVE,LWA,DWORK,LDWORK,IER)
              IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
                  IER = 12
                  RETURN
              END IF
          END IF

          IF (IGRDB.EQ.2) THEN
c
c     initialize wsave for gaussian synthesis
c
              CALL DSHSGCI(NLATB,NLONB,WSAVE(JB),LWB,DWORK,LDWORK,IER)
              IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
                  IER = 12
                  RETURN
              END IF
          ELSE
c
c     initialize wsave for equally spaced synthesis
c
              CALL DSHSECI(NLATB,NLONB,WSAVE(JB),LWB,DWORK,LDWORK,IER)
          END IF
c
c     end of initialization (intl=0) call
c
      END IF
c
c     transpose and/or reorder (co)latitude if necessary for da
c     (arrays must have latitude (colatitude) as the first dimension
c     and run north to south for spherepack software)
c
      IF (IGRIDA(2).EQ.0) CALL DTRSPLAT(NLONA,NLATA,DA,WORK)
      IF (IGRIDA(1).GT.0) CALL DCONVLAT(NLATA,NLONA,DA)

      NT = 1
      ISYM = 0
      IF (IGRDA.EQ.2) THEN
c
c     do spherical harmonic analysis of "adjusted" da on gaussian grid
c
          CALL DSHAGC(NLATA,NLONA,ISYM,NT,DA,NLATA,NLONA,WORK(IAA),
     +                WORK(IBA),LA1,NLATA,WSAVE,LWA,WORK(IW),LW,IER)
      ELSE
c
C*PL*ERROR* Comment line too long
c     do spherical harmonic analysis of "adjusted" da on equally spaced grid
c
          CALL DSHAEC(NLATA,NLONA,ISYM,NT,DA,NLATA,NLONA,WORK(IAA),
     +                WORK(IBA),LA1,NLATA,WSAVE,LWA,WORK(IW),LW,IER)
      END IF
c
c     transfer da grid coefficients to db grid coefficients
c     truncating to zero as necessary
c
      CALL DTRAB(LA1,NLATA,WORK(IAA),WORK(IBA),LB1,NLATB,WORK(IAB),
     +           WORK(IBB))
c     --------------------------NEW----------------------------------
c     (possibly) truncate and (possibly) taper the spectral coef
c
      IF (JMODE.NE.0) THEN
          CALL DTRITRUNC(NLATB,ABS(JMODE),NLATB,WORK(IAB),WORK(IBB))
          IF (JMODE.LT.0) THEN
              RN = MAX(ABS(JMODE)/10,1)*10
c c c         RN = RN-1.0
              CALL DEXPTAPERSH(NLATB,NLATB,WORK(IAB),WORK(IBB),RN,4,IER)
          END IF
      END IF
c     --------------------------NEW----------------------------------

      IF (IGRDB.EQ.1) THEN
c
C*PL*ERROR* Comment line too long
c     do spherical harmonic synthesis on nlatb by nlonb equally spaced grid
c
          CALL DSHSEC(NLATB,NLONB,ISYM,NT,DB,NLATB,NLONB,WORK(IAB),
     +                WORK(IBB),LB1,NLATB,WSAVE(JB),LWB,WORK(IW),LW,IER)
      ELSE
c
c     do spherical harmonic synthesis on nlatb by nlonb gaussian grid
c
          CALL DSHSGC(NLATB,NLONB,ISYM,NT,DB,NLATB,NLONB,WORK(IAB),
     +                WORK(IBB),LB1,NLATB,WSAVE(JB),LWB,WORK(IW),LW,IER)
      END IF
c
C*PL*ERROR* Comment line too long
c     both da,db are currently latitude by longitude north to south arrays
c     restore da and set db to agree with flags in igrida and igridb
c
      IF (IGRIDA(1).GT.0) CALL DCONVLAT(NLATA,NLONA,DA)
      IF (IGRIDB(1).GT.0) CALL DCONVLAT(NLATB,NLONB,DB)
      IF (IGRIDA(2).EQ.0) CALL DTRSPLAT(NLATA,NLONA,DA,WORK)
      IF (IGRIDB(2).EQ.0) CALL DTRSPLAT(NLATB,NLONB,DB,WORK)
      RETURN
      END
