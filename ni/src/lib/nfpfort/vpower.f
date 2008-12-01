C NCLFORTSTART
      SUBROUTINE SPCTIMCROSS1 (NL,NM,NT,X,Y,STC
     +                        ,NLP1,NTP1,NT2P1)
C INPUT
      INTEGER  NL, NM, NT
      DOUBLE PRECISION X(NL,NM,NT), Y(NL,NM,NT)
C OUTPUT
      DOUBLE PRECISION STC(NLP1,NT2P1,16)
C NCLEND
C NCL:    stc = spaceTimeCrossSegment(X,Y,NT,opt)

C                        ; DIMSENSION SIZES
      NLP1   = NL+1
      NTP1   = NT+1
      NT2M1  = NT/2-1
      NT2P1  = NT/2+1
      LSAVE1 = 4*NL+15
      LSAVE2 = 4*NT+15

      CALL SPCTIMCROSS2 (NL,NM,NT,X,Y,STC
     +                  ,NLP1,NTP1,NT2M1,NT2P1,LSAVE1,LSAVE2)

      RETURN
      END
c --------------------------------------
      SUBROUTINE SPCTIMCROSS2 (NL,NM,NT,X,Y,STC 
     +                        ,NLP1,NTP1,NT2M1,NT2P1,LSAVE1,LSAVE2)
      IMPLICIT NONE
      INTEGER  NL, NM, NT, NLP1,NTP1,NT2M1,NT2P1,LSAVE1,LSAVE2, TSTRT
C INPUT
      DOUBLE PRECISION X(NL,NM,NT), Y(NL,NM,NT)
C RETURN
      DOUBLE PRECISION STC(NLP1,NT2P1,16)
 
c OUTPUT VARIABLES THAT WILL BE PLACED INTO "STC"
      DOUBLE PRECISION  PEE1SUM_SYM(NLP1,NT2P1), PEE2SUM_SYM(NLP1,NT2P1)
      DOUBLE PRECISION  PEE1SUM_ASY(NLP1,NT2P1), PEE2SUM_ASY(NLP1,NT2P1)
      DOUBLE PRECISION  P12SUM_SYM (NLP1,NT2P1), Q12SUM_SYM(NLP1,NT2P1)
      DOUBLE PRECISION  P12SUM_ASY(NLP1,NT2P1) , Q12SUM_ASY(NLP1,NT2P1)
      DOUBLE PRECISION  COH2_SYM(NLP1,NT2P1)   , PHAS_SYM(NLP1,NT2P1)
      DOUBLE PRECISION  COH2_ASY(NLP1,NT2P1)   , PHAS_ASY(NLP1,NT2P1)
      DOUBLE PRECISION  V1_SYM(NLP1,NT2P1)     , V2_SYM(NLP1,NT2P1)
      DOUBLE PRECISION  V1_ASY(NLP1,NT2P1)     , V2_ASY(NLP1,NT2P1)
 
c LOCAL VARIABLES ASSOCIATED WITH THE INTERNAL PROCESSING 
      DOUBLE PRECISION  WSAVE1(LSAVE1), WSAVE2(LSAVE2)
      DOUBLE PRECISION  FRQ(NT2P1)
      DOUBLE COMPLEX    EE1(NL,NT), EEO1(NL,NT)
      DOUBLE COMPLEX    EE2(NL,NT), EEO2(NL,NT)
      DOUBLE COMPLEX    CEEA(NL)  , CEEB(NT)

c djs DOUBLE PRECISION FF(NTP1), SS(NLP1)
c
      INTEGER  IP, M , J, TT, N, T, PN, PT, PP

c------------------------MAIN PROGRAM--------------------------------
c EE(n,t) = the initial (real) data set that later becomes the
c         (complex) space-time spectrum
c ss() and ff() are arrays of wavenumbers and frequencies
c         (cycles per day) corresponding to PEE( )
 
      IF (MOD(NT,2).NE.0) THEN
         WRITE (6, *) 'NT must be event way: NT=',NT
         STOP 
      ELSE IF (MOD(NL,2).NE.0) THEN
         WRITE (6, *) 'NL must be even: NL=', NL
         STOP 
      ENDIF
 
c frequency and spatial wave number
c djs: needed for GrADS binary: do in NCL
 
c djs DO T = 1, NT + 1
c djs    FF(T) = DBLE(T - 1 - NT/2)/DBLE(NT)
c djs END DO
c djs DO N = 1, NL + 1
c djs    SS(N) = DBLE(N - 1 - NL/2)
c djs END DO
 
c Initialize SUM arrays
c djs: This had an error (memory bounds) *DO T=1,NT+1*

      DO T = 1, NT/2 + 1
         DO N = 1, NL + 1
            PEE1SUM_SYM(N,T) = 0.D0
            PEE2SUM_SYM(N,T) = 0.D0
            P12SUM_SYM(N,T)  = 0.D0
            Q12SUM_SYM(N,T)  = 0.D0
 
            PEE1SUM_ASY(N,T) = 0.D0
            PEE2SUM_ASY(N,T) = 0.D0
            P12SUM_ASY(N,T)  = 0.D0
            Q12SUM_ASY(N,T)  = 0.D0
         END DO
      END DO

c djs added ... explicitly initialize
      do n=1,nt/2+1
         frq(n) = 0.d0
      end do
C***************HERE IS THE BIG LOOP**********************************
c djs: Just pass in one time segment at a time at the NCL level.
c djs: accumulate and average in NCL     
C***************HERE IS THE BIG LOOP**********************************

      pp = 1

      DO IP = 1, PP
c read binary
c djs     DO T = 1,NT
c djs         READ (11,REC= (IP-1)*NT+T) ((X(N,M,T),N=1,NL),M=1,NM)
c djs         READ (12,REC= (IP-1)*NT+T) ((Y(N,M,T),N=1,NL),M=1,NM)
c djs     END DO
 
c Do a loop in latitude (M)!! - average
         DO M = 1, NM
            DO T = 1, NT
               DO N = 1, NL
                  EE1(N,T) = X(N,M,T)
                  EE2(N,T) = Y(N,M,T)
               END DO
            END DO
 
c Initialize FFTs
c djs: this should be moved outside loop but keep during debug
            CALL CFFTI (NL, WSAVE1)
            CALL CFFTI (NT, WSAVE2)
 
c--------------------------------------------------------------------
c---------------COMPUTING SPACE-TIME SPECTRUM for EE1----------------
 
            DO T = 1, NT
               DO N = 1, NL
c CEEa(n) contains the grid values around a latitude circle
                  CEEA(N) = EE1(N,T)
               END DO
               CALL CFFTF (NL, CEEA, WSAVE1)
               DO N = 1, NL
c Normalize
                  EE1(N,T) = CEEA(N)/DBLE(NL)
               END DO
            END DO
 
c Now the array EE(n,t) contains the Fourier coefficients (in planetary
c wavenumber space) for each time.
 
            DO N = 1, NL
               DO T = 1, NT
c CEEb(t) contains a time-series of the coefficients for a single
c planetary zonal wavenumber
                  CEEB(T) = EE1(N,T)
               END DO
               CALL CFFTF (NT, CEEB, WSAVE2)
               DO T = 1, NT
C Normalize
                  EE1(N,T) = CEEB(T)/DBLE(NT)
               END DO
            END DO
 
c Now the array EE1(n,t) contains the space-time spectrum.
 
c--------------------------------------------------------------------
c---------------COMPUTING SPACE-TIME SPECTRUM for EE2----------------
 
            DO T = 1, NT
               DO N = 1, NL
c CEEa(n) contains the grid values around a latitude circle
                  CEEA(N) = EE2(N,T)
               END DO
               CALL CFFTF (NL, CEEA, WSAVE1)
               DO N = 1, NL
                  EE2(N,T) = CEEA(N)/DBLE(NL)
               END DO
            END DO
 
c Now the array EE(n,t) contains the Fourier coefficients (in planetary
c wavenumber space) for each time.
 
            DO N = 1, NL
               DO T = 1, NT
c CEEb(t) contains a time-series of the coefficients for a single
c planetary zonal wavenumber
                  CEEB(T) = EE2(N,T)
               END DO
               CALL CFFTF (NT, CEEB, WSAVE2)
               DO T = 1, NT
                  EE2(N,T) = CEEB(T)/DBLE(NT)
               END DO
            END DO
 
c Now the array EE2(n,t) contains the space-time spectrum.
 
c-------------------------------------------------------------------
 
c Create array PEE(NL+1,NT+1) which contains the (real) power spectrum.
c In this array, the negative wavenumbers will be from pn=1 to NL/2;
c The positive wavenumbers will be for pn=NL/2+2 to NL+1.
c Negative frequencies will be from pt=1 to NT/2.
c Positive frequencies will be from pt=NT/2+2 to NT+1.
c Information about zonal mean will be for pn=NL/2+1.
c Information about time mean will be for pt=NT/2+1.
c Information about the Nyquist Frequency is at pt=1 and pt=NT+1
c In PEE, I define the WESTWARD waves to be either +ve frequency
c and -ve wavenumber or -ve freq and +ve wavenumber.
c EASTWARD waves are either +ve freq and +ve wavenumber OR -ve
c freq and -ve wavenumber.
 
c original one
c     do 191 pt=1,NT+1
c      do 189 pn=1,NL+1
c       if(pn.le.NL/2) then
c        n=NL/2+2-pn
c        if(pt.le.NT/2) then
c         t=NT/2+pt
c        else
c         t=pt-NT/2
c        endif
c       elseif(pn.ge.NL/2+1) then
c        n=pn-NL/2
c        if (pt.le.NT/2+1) then
c         t=NT/2+2-pt
c        else
c         t=NT+NT/2+2-pt
c        endif
c       endif
 
            DO PT = 1, NT/2 + 1
               DO PN = 1, NL + 1
 
                  IF (PN .LE. NL/2) THEN
                     N = NL/2 + 2 - PN
                     T = PT
                  ELSE IF (PN .EQ. NL/2+1) THEN
                     N = 1
                     T = PT
                  ELSE IF (PN .GE. NL/2+2) THEN
                     N = PN - NL/2
                     IF (PT .EQ. 1) THEN
                        T = PT
                     ELSE
                        T = NT + 2 - PT
                     ENDIF
                  ENDIF
 
                  IF (M.GE.1 .AND. M.LE.NM/2+1) THEN
 
                     PEE1SUM_SYM(PN,PT) = PEE1SUM_SYM(PN,PT) 
     +                      + CDABS(EE1(N,T))**2/DBLE(PP*(NM/2+1))
                     PEE2SUM_SYM(PN,PT) = PEE2SUM_SYM(PN,PT) 
     +                      + CDABS(EE2(N,T))**2/DBLE(PP*(NM/2+1))
 
                     P12SUM_SYM(PN,PT) = P12SUM_SYM(PN,PT) 
     +           + DBLE(DCONJG(EE1(N,T))*EE2(N,T))/DBLE(PP*(NM/2+1))
                     Q12SUM_SYM(PN,PT) = Q12SUM_SYM(PN,PT) 
     +  + DBLE((0.D0,1.D0)*DCONJG(EE1(N,T))*EE2(N,T))/DBLE(PP*(NM/2+1))
 
                  ELSE
 
                     PEE1SUM_ASY(PN,PT) = PEE1SUM_ASY(PN,PT) 
     +                      + CDABS(EE1(N,T))**2/DBLE(PP*(NM/2))
                     PEE2SUM_ASY(PN,PT) = PEE2SUM_ASY(PN,PT) 
     +                      + CDABS(EE2(N,T))**2/DBLE(PP*(NM/2))
 
                     P12SUM_ASY(PN,PT) = P12SUM_ASY(PN,PT) 
     +           + DBLE(DCONJG(EE1(N,T))*EE2(N,T))/DBLE(PP*(NM/2))
                     Q12SUM_ASY(PN,PT) = Q12SUM_ASY(PN,PT) 
     +  + DBLE((0.D0,1.D0)*DCONJG(EE1(N,T))*EE2(N,T))/DBLE(PP*(NM/2))
 
                  ENDIF
 
               END DO
            END DO
 
 
c     do pt=1,NT+1
c     do pn=1,NL+1
c      Coh2(pn,pt)=(P12sum(pn,pt)**2+Q12sum(pn,pt)**2)/
c    #                        (PEE1sum(pn,pt)*PEE2sum(pn,pt))
c      Phas(pn,pt)=ATAN(Q12sum(pn,pt)/P12sum(pn,pt))
c     enddo
c     enddo
c                          END of NM [LON] loop
         END DO
c                          END of IP loop
      END DO
c***************END BIG LOOP HERE*****************************

      DO T = 1, NT/2 + 1
         DO N = 1, NL + 1
            STC(N,T,1)  = PEE1SUM_SYM(N,T) 
            STC(N,T,2)  = PEE1SUM_ASY(N,T) 
            STC(N,T,3)  = PEE2SUM_SYM(N,T) 
            STC(N,T,4)  = PEE2SUM_ASY(N,T) 
            STC(N,T,5)  = P12SUM_SYM(N,T)  
            STC(N,T,6)  = P12SUM_ASY(N,T)  
            STC(N,T,7)  = Q12SUM_SYM(N,T)  
            STC(N,T,8)  = Q12SUM_ASY(N,T)  
         END DO
      END DO
 
c ********* APPLY SMOOTHING TO THE SPECTRUM ************************
c Apply smoothing to PEE1,PEE2,P12, and Q12
c then calculate Coh2 and Phase.

      CALL SPCTIMCROSS3 (NL,NT,STC,NLP1,NTP1,NT2P1)

      RETURN
      END 
c --------------------------------------
C NCLFORTSTART
      SUBROUTINE SPCTIMCROSS3 (NL,NT,STC,NLP1,NTP1,NT2P1)  
      IMPLICIT NONE
      INTEGER  NL, NT, NLP1,NTP1,NT2P1
C INPUT (1 thru 8) and OUTPUT (9 to 16)
      DOUBLE PRECISION STC(NLP1,NT2P1,16)
C NCLEND

C NCL:  
      
c TEMPORARY VARIABLES THAT WILL BE PLACED EXTRACTED FROM "STC"
      DOUBLE PRECISION  PEE1SUM_SYM(NLP1,NT2P1), PEE2SUM_SYM(NLP1,NT2P1)
      DOUBLE PRECISION  PEE1SUM_ASY(NLP1,NT2P1), PEE2SUM_ASY(NLP1,NT2P1)
      DOUBLE PRECISION  P12SUM_SYM (NLP1,NT2P1), Q12SUM_SYM(NLP1,NT2P1)
      DOUBLE PRECISION  P12SUM_ASY(NLP1,NT2P1) , Q12SUM_ASY(NLP1,NT2P1)

c LOCAL VARIABLES THAT WILL BE PLACED INTO "STC"
      DOUBLE PRECISION  COH2_SYM(NLP1,NT2P1)   , PHAS_SYM(NLP1,NT2P1)
      DOUBLE PRECISION  COH2_ASY(NLP1,NT2P1)   , PHAS_ASY(NLP1,NT2P1)
      DOUBLE PRECISION  V1_SYM(NLP1,NT2P1)     , V2_SYM(NLP1,NT2P1)
      DOUBLE PRECISION  V1_ASY(NLP1,NT2P1)     , V2_ASY(NLP1,NT2P1)

      DOUBLE PRECISION  FRQ(NT2P1)
      INTEGER  TSTRT, N, T
      
      DO T = 1, NT/2 + 1
         DO N = 1, NL + 1
            PEE1SUM_SYM(N,T) = STC(N,T,1) 
            PEE1SUM_ASY(N,T) = STC(N,T,2)
            PEE2SUM_SYM(N,T) = STC(N,T,3) 
            PEE2SUM_ASY(N,T) = STC(N,T,4) 
            P12SUM_SYM(N,T)  = STC(N,T,5)  
            P12SUM_ASY(N,T)  = STC(N,T,6)  
            Q12SUM_SYM(N,T)  = STC(N,T,7)  
            Q12SUM_ASY(N,T)  = STC(N,T,8)  
         END DO
      END DO

c Smoothing in frq only
c djs not sure smoothing was not put into a subroutine
c djs not sure why the 4 offset TSTRT=4
c     Several arguments were changed
      TSTRT = 1
      DO N = 1, NL + 1
         DO T = TSTRT, NT/2 + 1
            FRQ(T-TSTRT+1) = PEE1SUM_SYM(N,T)
         END DO
         CALL SMTH121STCP (FRQ, NT/2 + 1, NT/2 + 1)
         DO T = TSTRT, NT/2 + 1
            PEE1SUM_SYM(N,T) = FRQ(T-TSTRT+1)
         END DO
      END DO
 
      DO N = 1, NL + 1
         DO T = TSTRT, NT/2 + 1
            FRQ(T-TSTRT+1) = PEE2SUM_SYM(N,T)
         END DO
         CALL SMTH121STCP (FRQ, NT/2 + 1, NT/2 + 1)
         DO T = TSTRT, NT/2 + 1
            PEE2SUM_SYM(N,T) = FRQ(T-TSTRT+1)
         END DO
      END DO
 
      DO N = 1, NL + 1
         DO T = TSTRT, NT/2 + 1
            FRQ(T-TSTRT+1) = P12SUM_SYM(N,T)
         END DO
         CALL SMTH121STCP (FRQ, NT/2 + 1, NT/2 + 1)
         DO T = TSTRT, NT/2 + 1
            P12SUM_SYM(N,T) = FRQ(T-TSTRT+1)
         END DO
      END DO
 
      DO N = 1, NL + 1
         DO T = TSTRT, NT/2 + 1
            FRQ(T-TSTRT+1) = Q12SUM_SYM(N,T)
         END DO
         CALL SMTH121STCP (FRQ, NT/2 + 1, NT/2 + 1)
         DO T = TSTRT, NT/2 + 1
            Q12SUM_SYM(N,T) = FRQ(T-TSTRT+1)
         END DO
      END DO
 
c asymmetric
      DO N = 1, NL + 1
         DO T = TSTRT, NT/2 + 1
            FRQ(T-TSTRT+1) = PEE1SUM_ASY(N,T)
         END DO
         CALL SMTH121STCP (FRQ, NT/2 + 1, NT/2 + 1)
         DO T = TSTRT, NT/2 + 1
            PEE1SUM_ASY(N,T) = FRQ(T-TSTRT+1)
         END DO
      END DO
 
      DO N = 1, NL + 1
         DO T = TSTRT, NT/2 + 1
            FRQ(T-TSTRT+1) = PEE2SUM_ASY(N,T)
         END DO
         CALL SMTH121STCP (FRQ, NT/2 + 1, NT/2 + 1)
         DO T = TSTRT, NT/2 + 1
            PEE2SUM_ASY(N,T) = FRQ(T-TSTRT+1)
         END DO
      END DO
 
      DO N = 1, NL + 1
         DO T = TSTRT, NT/2 + 1
            FRQ(T-TSTRT+1) = P12SUM_ASY(N,T)
         END DO
         CALL SMTH121STCP (FRQ, NT/2 + 1, NT/2 + 1)
         DO T = TSTRT, NT/2 + 1
            P12SUM_ASY(N,T) = FRQ(T-TSTRT+1)
         END DO
      END DO
 
      DO N = 1, NL + 1
         DO T = TSTRT, NT/2 + 1
            FRQ(T-TSTRT+1) = Q12SUM_ASY(N,T)
         END DO
         CALL SMTH121STCP (FRQ, NT/2 + 1, NT/2 + 1)
         DO T = TSTRT, NT/2 + 1
            Q12SUM_ASY(N,T) = FRQ(T-TSTRT+1)
         END DO
      END DO
 
c Calculate the Coherence-squared statistic and the phase.
      DO T = 1, NT/2 + 1
         DO N = 1, NL + 1
c       if (PEE1sum_sym(n,t).ne.dmiss) then
            COH2_SYM(N,T) = (P12SUM_SYM(N,T)**2+Q12SUM_SYM(N,T)**2)
     +                    / (PEE1SUM_SYM(N,T)*PEE2SUM_SYM(N,T))
            PHAS_SYM(N,T) = ATAN(Q12SUM_SYM(N,T)/P12SUM_SYM(N,T))

            IF (N .LE. NL/2+1) THEN
               V1_SYM(N,T) = Q12SUM_SYM(N,T)/SQRT(Q12SUM_SYM(N,T)**2
     +                     + P12SUM_SYM(N,T)**2)
            ELSE
               V1_SYM(N,T) = -1.D0*Q12SUM_SYM(N,T)
     +                     / SQRT(Q12SUM_SYM(N,T)**2+P12SUM_SYM(N,T)**2)
               Q12SUM_SYM(N,T) = -1.D0*Q12SUM_SYM(N,T)
            ENDIF
            V2_SYM(N,T) = P12SUM_SYM(N,T)/SQRT(Q12SUM_SYM(N,T)**2
     +                  + P12SUM_SYM(N,T)**2)
c       else
c        Coh2_sym(n,t)=dmiss
c        Phas_sym(n,t)=dmiss
c       endif
 
            IF (COH2_SYM(N,T) .LT. 0.05D0) THEN
               V1_SYM(N,T) = 0.0D0
               V2_SYM(N,T) = 0.0D0
            ENDIF
         END DO
      END DO
 
      DO T = 1, NT/2 + 1
         DO N = 1, NL + 1
c       if (PEE1sum_asy(n,t).ne.dmiss) then
            COH2_ASY(N,T) = (P12SUM_ASY(N,T)**2+Q12SUM_ASY(N,T)**2)
     +                     /(PEE1SUM_ASY(N,T)*PEE2SUM_ASY(N,T))
            PHAS_ASY(N,T) = ATAN(Q12SUM_ASY(N,T)/P12SUM_ASY(N,T))

            IF (N .LE. NL/2+1) THEN
               V1_ASY(N,T) = Q12SUM_ASY(N,T)/SQRT(Q12SUM_ASY(N,T)**2
     +                      +P12SUM_ASY(N,T)**2)
            ELSE
               V1_ASY(N,T) = -1.D0*Q12SUM_ASY(N,T)/SQRT(Q12SUM_ASY(N,T) 
     +                        **2+P12SUM_ASY(N,T)**2)
               Q12SUM_ASY(N,T) = -1.D0*Q12SUM_ASY(N,T)
            ENDIF
            V2_ASY(N,T) = P12SUM_ASY(N,T)/SQRT(Q12SUM_ASY(N,T)**2
     +                   +P12SUM_ASY(N,T)**2)

            IF (COH2_ASY(N,T) .LT. 0.05D0) THEN
               V1_ASY(N,T) = 0.0D0
               V2_ASY(N,T) = 0.0D0
            ENDIF
         END DO
      END DO
      
      DO T = 1, NT/2 + 1
         DO N = 1, NL + 1
            STC(N,T, 9)   = COH2_SYM(N,T)  
            STC(N,T,10)   = COH2_ASY(N,T)  
            STC(N,T,11)   = PHAS_SYM(N,T)  
            STC(N,T,12)   = PHAS_ASY(N,T)  
            STC(N,T,13)   = V1_SYM(N,T)  
            STC(N,T,14)   = V1_SYM(N,T)  
            STC(N,T,15)   = V2_SYM(N,T)  
            STC(N,T,16)   = V2_ASY(N,T)  
         END DO
      END DO

      RETURN
      END 
c--------------------------------------------------------
      SUBROUTINE SMTH121STCP(VV, VN, NN)
c SpaceTimeCoherence2Phase (STCP)
c Smooths vv by passing it through a 1-2-1 filter.
c The first and last points are given 3-1 (1st) or 1-3 (last)
c weightings (Note that this conserves the total sum).
c The routine also skips-over missing data (assigned to be
c a value of -999.).
c There are 'nn' pieces of useful information, which may be less
c than or equal to 'vn'.
 
      IMPLICIT NONE
      INTEGER  NN, I, VN
      DOUBLE PRECISION  SPV, VV(VN), DUM(9999)
 
      IF (NN .GT. 5000) THEN
         WRITE (6, *) 'need to increase 9999 in smooth121.f'
         STOP 
      ENDIF
 
      SPV = -999.D0
      I = 0
   10 CONTINUE
      I = I + 1
      IF (VV(I) .EQ. SPV) THEN
         DUM(I) = SPV
      ELSE IF (I.EQ.1 .OR. VV(I-1).EQ.SPV) THEN
         DUM(I) = (3.D0*VV(I)+VV(I+1))/4.D0
      ELSE IF (I.EQ.NN .OR. VV(I+1).EQ.SPV) THEN
         DUM(I) = (VV(I-1)+3.D0*VV(I))/4.D0
      ELSE
         DUM(I) = (VV(I-1)+2.D0*VV(I)+VV(I+1))/4.D0
      ENDIF
      IF (I .NE. NN) GO TO 10
 
      DO I = 1, NN
         VV(I) = DUM(I)
      END DO

      RETURN 
c djs same answers
c djs do i=2,nn-1
c djs    dum(i) = (vv(i-1)+2.d0*vv(i)+vv(i+1))/4.d0
c djs end do
c djs dum(1 ) = (3.d0*vv( 1)+vv( 2)  )/4.d0
c djs dum(nn) = (3.d0*vv(nn)+vv(nn-1))/4.d0
c djs do i=1,nn
c djs    vv(i) = dum(i)
c djs end do
      END 
