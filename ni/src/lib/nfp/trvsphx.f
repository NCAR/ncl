C NCLFORTSTART
      SUBROUTINE TRVSPHX(INTL,IGRIDA,NLONA,NLATA,IVECA,UA,VA,IGRIDB,
     +                  NLONB,NLATB,IVECB,UB,VB,WSAVE,LSAVE,LSVMIN,WORK,
     +                  LWORK,LWKMIN,DWORK,LDWORK,IER,JMODE)
      IMPLICIT NONE
      INTEGER INTL,IGRIDA(2),NLONA,NLATA,IGRIDB(2),NLONB,NLATB
      INTEGER IVECA,IVECB,LSAVE,LSVMIN,LWORK,LWKMIN,LDWORK,IER, JMODE
      DOUBLE PRECISION UA(NLONA,NLATA),VA(NLONA,NLATA)
      DOUBLE PRECISION UB(NLONB,NLATB),VB(NLONB,NLATB)
      DOUBLE PRECISION WSAVE(LSAVE),WORK(LWORK)
      DOUBLE PRECISION DWORK(LDWORK)
C NCLEND
      INTEGER IG,IGRDA,IGRDB,LA1,LA2,LB1,LB2,LWA,LWB
      INTEGER IABR,IABI,IACR,IACI,IBBR,IBBI,IBCR,IBCI
      INTEGER NLAT,LWK1,LWK2,LW,IW,JB,NT,ITYP
      DOUBLE PRECISION RN
c
c     include a save statement to ensure local variables in trvsph, set during
c     an intl=0 call, are preserved if trvsph is recalled with intl=1
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
      IF (IVECA* (IVECA-1).NE.0) RETURN
      IER = 7
      IG = IGRIDB(1)
      IF ((IG-1)* (IG+1)* (IG-2)* (IG+2).NE.0) RETURN
      IER = 8
      IG = IGRIDB(2)
      IF (IG* (IG-1).NE.0) RETURN
      IER = 9
      IF (NLONB.LT.4) RETURN
      IER = 10
      IF (NLATB.LT.3) RETURN
      IER = 11
      IF (IVECB* (IVECB-1).NE.0) RETURN
      IER = 0
      IGRDA = IABS(IGRIDA(1))
      IGRDB = IABS(IGRIDB(1))
      IF (INTL.EQ.0) THEN
          LA1 = MIN0(NLATA, (NLONA+1)/2)
          LA2 = (NLATA+1)/2
          LB1 = MIN0(NLATB, (NLONB+1)/2)
          LB2 = (NLATB+1)/2
c
c     saved space for analysis on a grid
c
          LWA = 4*NLATA*LA2 + 3*MAX0(LA1-2,0)* (2*NLATA-LA1-1) + LA2 +
     +          NLONA + 15
c
c     set saved work space length for synthesis on b grid
c
          LWB = 4*NLATB*LB2 + 3*MAX0(LB1-2,0)* (2*NLATB-LB1-1) + NLONB +
     +          15
c
c     set minimum required saved work space length
c
          LSVMIN = LWA + LWB
c
c     set wsave pointer
c
          JB = 1 + LWA
c
c     set pointers for vector spherical harmonic coefs in work
c
          IABR = 1
          IABI = IABR + LA1*NLATA
          IACR = IABI + LA1*NLATA
          IACI = IACR + LA1*NLATA
          IBBR = IACI + LA1*NLATA
          IBBI = IBBR + LB1*NLATB
          IBCR = IBBI + LB1*NLATB
          IBCI = IBCR + LB1*NLATB
c
c     set pointers for remaining work
c
          IW = IBCI + LB1*NLATB
c
c     set remaining work space length in lw
c
          LW = LWORK - IW
c
c     compute unsaved space for analysis and synthesis
c
          LWK1 = 2*NLATA* (2*NLONA+MAX0(6*LA2,NLONA))
          LWK2 = 2*NLATB* (2*NLONB+MAX0(6*LB2,NLONB))
c
c     set minimum unsaved work space required by trvsph
c
          LWKMIN = IW + MAX0(LWK1,LWK2)
c
c     set error flags if saved or unsaved work space is insufficient
c
          IER = 12
          IF (LSAVE.LT.LSVMIN) RETURN
          IER = 13
          IF (LWORK.LT.LWKMIN) RETURN
          IER = 15
          NLAT = MAX0(NLATA,NLATB)
          IF (LDWORK.LT.2*NLAT* (NLAT+1)+1) RETURN
          IER = 0
          IF (IGRDA.EQ.1) THEN
c
c     initialize wsave for equally spaced analysis
c
              CALL DVHAECI(NLATA,NLONA,WSAVE,LWA,DWORK,LDWORK,IER)
          ELSE
c
c     initialize wsave for gaussian analysis
c
              CALL DVHAGCI(NLATA,NLONA,WSAVE,LWA,DWORK,LDWORK,IER)
              IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
                  IER = 14
                  RETURN
              END IF
          END IF

          IF (IGRDB.EQ.2) THEN
c
c     initialize wsave for gaussian synthesis
c
              CALL DVHSGCI(NLATB,NLONB,WSAVE(JB),LWB,DWORK,LDWORK,IER)
              IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
                  IER = 14
                  RETURN
              END IF
          ELSE
c
c     initialize wsave for equally spaced synthesis
c
              CALL DVHSECI(NLATB,NLONB,WSAVE(JB),LWB,DWORK,LDWORK,IER)
          END IF
c
c     end of initialization (intl=0) call
c
      END IF
c
C*PL*ERROR* Comment line too long
c     convert the vector field (ua,va) to mathematical spherical coordinates
c
      IF (IGRIDA(2).EQ.0) THEN
          CALL DTRVPLAT(NLONA,NLATA,UA,WORK)
          CALL DTRVPLAT(NLONA,NLATA,VA,WORK)
      END IF
      IF (IGRIDA(1).GT.0) THEN
          CALL DCOVLAT(NLATA,NLONA,UA)
          CALL DCOVLAT(NLATA,NLONA,VA)
      END IF
      IF (IVECA.EQ.0) THEN
          CALL DNEGV(NLATA,NLONA,VA)
      END IF
      NT = 1
      ITYP = 0
c
c     analyze vector field
c
      IF (IGRDA.EQ.2) THEN
          CALL DVHAGC(NLATA,NLONA,ITYP,NT,VA,UA,NLATA,NLONA,WORK(IABR),
     +               WORK(IABI),WORK(IACR),WORK(IACI),LA1,NLATA,WSAVE,
     +               LWA,WORK(IW),LW,IER)
      ELSE
          CALL DVHAEC(NLATA,NLONA,ITYP,NT,VA,UA,NLATA,NLONA,WORK(IABR),
     +               WORK(IABI),WORK(IACR),WORK(IACI),LA1,NLATA,WSAVE,
     +               LWA,WORK(IW),LW,IER)
      END IF
c
c     transfer "a" grid coefficients to "b" grid coefficients
c
      CALL DTRVAB(LA1,NLATA,WORK(IABR),WORK(IABI),WORK(IACR),WORK(IACI),
     +            LB1,NLATB,WORK(IBBR),WORK(IBBI),WORK(IBCR),WORK(IBCI))
c     --------------------------NEW----------------------------------
c     (possibly) truncate and (possibly) taper the spectral coef
c
      IF (JMODE.NE.0) THEN
          CALL DTRITRUNC(NLATB,ABS(JMODE),NLATB,WORK(IBBR),WORK(IBBI))
          CALL DTRITRUNC(NLATB,ABS(JMODE),NLATB,WORK(IBCR),WORK(IBCI))
          IF (JMODE.LT.0) THEN
              RN = MAX(ABS(JMODE)/10,1)*10
c c c         RN = RN-1.0
              CALL DEXPTAPERSH(NLATB,NLATB,WORK(IBBR),WORK(IBBI)
     +                        ,RN,4,IER)
              CALL DEXPTAPERSH(NLATB,NLATB,WORK(IBCR),WORK(IBCI)
     +                        ,RN,4,IER)
          END IF
      END IF
c     ---------------------------------------------------------------
c
c     synthesize on b grid
c
      IF (IGRDB.EQ.1) THEN
          CALL DVHSEC(NLATB,NLONB,ITYP,NT,VB,UB,NLATB,NLONB,WORK(IBBR),
     +               WORK(IBBI),WORK(IBCR),WORK(IBCI),LB1,NLATB,
     +               WSAVE(JB),LWB,WORK(IW),LW,IER)
      ELSE
          CALL DVHSGC(NLATB,NLONB,ITYP,NT,VB,UB,NLATB,NLONB,WORK(IBBR),
     +               WORK(IBBI),WORK(IBCR),WORK(IBCI),LB1,NLATB,
     +               WSAVE(JB),LWB,WORK(IW),LW,IER)
      END IF
c
c     restore a grid and b grid vector fields (now in math coordinates) to
c     agree with grid flags in igrida,iveca,igridb,ivecb
c
      IF (IVECA.EQ.0) THEN
          CALL DNEGV(NLATA,NLONA,VA)
      END IF
      IF (IVECB.EQ.0) THEN
          CALL DNEGV(NLATB,NLONB,VB)
      END IF
      IF (IGRIDA(1).GT.0) THEN
          CALL DCOVLAT(NLATA,NLONA,UA)
          CALL DCOVLAT(NLATA,NLONA,VA)
      END IF
      IF (IGRIDB(1).GT.0) THEN
          CALL DCOVLAT(NLATB,NLONB,UB)
          CALL DCOVLAT(NLATB,NLONB,VB)
      END IF
      IF (IGRIDA(2).EQ.0) THEN
          CALL DTRVPLAT(NLATA,NLONA,UA,WORK)
          CALL DTRVPLAT(NLATA,NLONA,VA,WORK)
      END IF
      IF (IGRIDB(2).EQ.0) THEN
          CALL DTRVPLAT(NLATB,NLONB,UB,WORK)
          CALL DTRVPLAT(NLATB,NLONB,VB,WORK)
      END IF
      RETURN
      END
