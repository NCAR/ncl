C NCLFORTSTART
      SUBROUTINE WAVELETI(N,Y,DT,MOTHER,PARAM,S0,DJ,JTOT,NPAD,NOISE,
     +                    ISIGTEST,SIGLVL,NADOF,WAVE,SCALE,PERIOD,COI,
     +                    DOF,FFTTHEOR,SIGNIF,GWS,YMEAN,YSD,LAG1,
     +                    CDELTA,PSI0,POWER,PHASE,R1)
      IMPLICIT NONE

C     wave = wavelet (y,mother \
C                    ,param,s0,dj,jtot,npad,noise,isigtest,siglvl,nadof)

C     return arguments:  all must be allocated in the C interface
C              JTOT must be calculated in the default case
C              wave(N,JTOT,2)   <=== fortran order
C              wave@scale(JTOT), wave@period(JTOT), wave@coi(N)
C              wave@dof(JTOT), wave@ffttheor(JTOT), wave@signif(JTOT)
C              wave@mean , wave@st_dev, wave@lag1

C                                             INPUT
      INTEGER N,MOTHER,JTOT,NPAD,NOISE,ISIGTEST
      DOUBLE PRECISION Y(N),DT,PARAM,S0,DJ,SIGLVL,NADOF(*)
C                                             OUTPUT: must be allocated
C                                                     in interface
      DOUBLE PRECISION WAVE(N,JTOT,2)
C                                             RETURN AS ATTRIBUTES
      DOUBLE PRECISION SCALE(JTOT),PERIOD(JTOT),COI(N),GWS(JTOT)
      DOUBLE PRECISION POWER(N,JTOT),PHASE(N,JTOT)
      DOUBLE PRECISION DOF(JTOT),FFTTHEOR(JTOT),SIGNIF(JTOT)
      DOUBLE PRECISION YMEAN,YSD,LAG1,CDELTA,PSI0,R1
C NCLEND
C                                             LOCAL and  AUTOMATIC
      DOUBLE COMPLEX CWAVE(N,JTOT)
      DOUBLE PRECISION YVAR,RAD
      INTEGER J,K
C
      YMEAN = 0.0D0
      YVAR = 0.0D0
      YSD = 0.0D0
      LAG1 = 0.0D0
      NADOF(1) = 0.0D0
      NADOF(2) = 0.0D0
C
      DO J = 1,JTOT
          SCALE(J) = 0.0D0
          FFTTHEOR(J) = 0.0D0
          SIGNIF(J) = 0.0D0
          PERIOD(J) = 0.0D0
      END DO

      DO K = 1,N
          COI(K) = 0.0D0
      END DO

      RAD = 4.D0*ATAN(1.)/180.d0

    1 DO J = 1,JTOT
          DO K = 1,N
              CWAVE(K,J) = (0.0D0,0.0D0)
              POWER(K,J) = 0.0D0
              PHASE(K,J) = 0.0D0
          END DO
      END DO
C                                             CALCULATE SOME STATS
      CALL STATSWAVE(N,Y,NOISE,YMEAN,YVAR,YSD,LAG1)
C                                             CALL USING COMPLEX
      CALL WAVELET(N,Y,DT,MOTHER,PARAM,S0,DJ,JTOT,NPAD,YMEAN,YVAR,
     +             CWAVE,SCALE,PERIOD,COI)
C                                             RETURN REALS to NCL
      DO J = 1,JTOT
        GWS(J) = 0.0D0
        DO K = 1,N
          WAVE(K,J,1) = DBLE(CWAVE(K,J))
          WAVE(K,J,2) = DIMAG(CWAVE(K,J))
          POWER(K,J)  = WAVE(K,J,1)**2 + WAVE(K,J,2)**2
          PHASE(K,J)  = ATAN2(DIMAG(CWAVE(K,J)),DBLE(CWAVE(K,J)))/RAD
          GWS(J) = GWS(J) + WAVE(K,J,1)**2 + WAVE(K,J,2)**2
        END DO
        GWS(J) = GWS(J)/DBLE(N)
      END DO
C                            NADOF could be used here
      IF (ISIGTEST.EQ.1) THEN
          DO J = 1,JTOT
              DOF(J) = N - SCALE(J)
          END DO
      ELSE
          IF (ISIGTEST.EQ.2) THEN
              DOF(1) = NADOF(1)
              DOF(2) = NADOF(2)
          END IF
      END IF

      R1 = LAG1
      IF (NOISE.EQ.0) THEN
          R1 = 0.0D0
      END IF

C                                             SIGNIFICANCE TESTING
      CALL WAVE_SIGNIF(ISIGTEST,N,Y,DT,MOTHER,PARAM,DJ,JTOT,SCALE,
     +                 PERIOD,R1,SIGLVL,DOF,FFTTHEOR,SIGNIF,YMEAN,
     +                 YVAR,CDELTA,PSI0)

      RETURN
      END

C -------- wavelet_default ----------------------------
C     SUBROUTINE WAVELETDF90 (N,Y,MOTHER)
C     INTEGER N, MOTHER
C     REAL    Y(N)
C     real, allocatable, dimension(:)     :: scale, period, coi
C     real, allocatable, dimension(:)     :: dof, ffttheor,signif
C     real, allocatable, dimension(:,:,:) :: wave

C                                             INPUT
C     INTEGER  N, MOTHER, JTOT, NPAD, NOISE, ISIGTEST
C     REAL  Y(N), DT, PARAM, S0, DJ, SIGLVL


C NCL: wave = wavelet_default (y, mother)

c     IF (MOTHER.LE.0 .OR. MOTHER.GT.2) THEN
c         PARAM = 6.0
c     ELSEIF (MOTHER.EQ.1) THEN
c         PARAM = 4.0
c     ELSEIF (MOTHER.EQ.2) THEN
c         PARAM = 2.0
c     END IF
C                                             DEFAULT/TYPICAL VALUES
C     DT = 1.0
c     S0 = 2.*DT
c     DJ = 0.25
c     JTOT = 1+(LOG2(N*DT/S0))/DJ
c     NPAD = N
c     NOISE= 1
c     ISIGTEST = 0
c     SIGLVL   = 0.05

c     ALLOCATE (COI(N), STAT=KER)

c     ALLOCATE (SCALE(JTOT), STAT=KER)
c     ALLOCATE (PERIOD(JTOT), STAT=KER)
c     ALLOCATE (DOF(JTOT), STAT=KER)
c     ALLOCATE (FFTTHEOR(JTOT), STAT=KER)
c     ALLOCATE (SIGNIF(JTOT), STAT=KER)

C     ALLOCATE (WAVE(N,JTOT,2), STAT=KER)

c     CALL WAVELETI(N, Y,DT,MOTHER,PARAM,S0,DJ,JTOT,NPAD,NOISE
c    1             ,ISIGTEST,SIGLVL
c    2             ,WAVE, SCALE, PERIOD, COI, DOF, FFTTHEOR
c    3             ,SIGNIF,YMEAN, YSD, LAG1)
C -----------------------------------------------------

      SUBROUTINE STATSWAVE (N, Y, NOISE, YMEAN, YVAR, YSD, LAG1)

      IMPLICIT NONE
      INTEGER  N, NOISE
      DOUBLE PRECISION  Y(N), YMEAN, YVAR, YSD, LAG1
      
      DOUBLE PRECISION  YTMP
      INTEGER K
 
      YMEAN = 0.0
      YVAR  = 0.0
      DO K=1,N
         YMEAN = YMEAN + Y(K)
         YVAR  = YVAR  + Y(K)*Y(K)
      END DO

      YTMP  = YMEAN*YMEAN/N
      YVAR  = MAX( (YVAR-YTMP)/N , 0.D0 ) ! prevent possible roundoff
      YMEAN = YMEAN/N

      DO K=1,N-1
         LAG1 = LAG1 + (Y(K+1)-YMEAN)*(Y(K)-YMEAN)
      END DO 
      LAG1 = (LAG1/(N-1.))/YVAR

      YSD   = SQRT(YVAR)

      RETURN
      END

C -------------------------------------------------------
      SUBROUTINE WAVEJTOT(N,DT,S0,DJ,JTOT)
      DOUBLE PRECISION DT,S0,DJ
      INTEGER JTOT,N

C      JTOT = 1 + (LOG2(N*DT/S0))/DJ
      JTOT = 1 + ((LOG(N*DT/S0))/DJ)/LOG(2.D0)

      RETURN
      END

C****************************************************************************
C
C WAVEPACK:  routines to compute the wavelet transform of a time series,
C            and significance levels.
C
C Written by:   Christopher Torrence and Gilbert P. Compo
C
C Available from:  http://paos.colorado.edu/research/wavelets/
C
C Requires the following packages:   CFFTPACK, CHISQR
C
C Notes:
C
C  (1) All routines are written in single precision (DOUBLE PRECISION),
C      except for CHISQR, which requires double precision input.
C      Single precision should be sufficient for most applications.
C
C  (2) The CFFTPACK and CHISQR routines were not written by us,
C      and no guarentees are made as to their reliability or efficiency.
C
C  (3) No provision is made for output to files or to graphics packages.
C      The user is expected to call these routines from within
C      their own programs. See sample program "wavetest.f".
C
C  (4) Little error checking is done. Check your input carefully.
C      The programs are not completely ANSI compatible, so use caution.
C
C  (5) Time series are currently limited to 65535 points.
C
C
C Reference: Torrence, C. and G. P. Compo, 1998: A Practical Guide to
C            Wavelet Analysis. <I>Bull. Amer. Meteor. Soc.</I>, 79, 61-78.
C
C Notice: Please acknowledge the use of this software in any publications:
C    ``Wavelet software was provided by C. Torrence and G. Compo,
C      and is available at URL: http://paos.colorado.edu/research/wavelets/''.
C
C  Copyright (C) 1998, Christopher Torrence
C This software may be used, copied, or redistributed as long as it is not
C sold and this copyright notice is reproduced on each copy made.  This
C routine is provided as is without any express or implied warranties
C whatsoever.
C
C
C Modified: November 1999 by Arjan van Dijk to include IMPLICIT NONE and
C           to convert all routines to DOUBLE precision.
C****************************************************************************




C****************************************************************************
C WAVELET: computes the wavelet transform of a time series,
C          with appropriate parameters.
C
C
C INPUTS:
C
C  n [INT] = the number of points in "y".
C
C  y [DOUBLE PRECISION] = the time series of length "n".
C
C  dt [DOUBLE PRECISION] = amount of time between each Y value, i.e. the sampling time.
C
C  mother [INT] = An integer giving the mother wavelet to use.
C                     0='Morlet'
C                     1='Paul'
C                     2='DOG' (derivative of Gaussian)
C                 If (mother<0 or >2) then default is 'Morlet'.
C
C  param [DOUBLE PRECISION] = mother wavelet parameter. If <0 then default is used.
C            For 'Morlet' this is k0 (wavenumber), default is 6.
C            For 'Paul' this is m (order), default is 4.
C            For 'DOG' this is m (m-th derivative), default is 2.
C
C
C  s0 [DOUBLE PRECISION] = the smallest scale of the wavelet.  Typically = 2*dt.
C         Note: for accurate reconstruction and variance computation
C             set s0=dt for Morlet; s0=dt/4 for Paul
C
C  dj [DOUBLE PRECISION] = the spacing between discrete scales. Typically = 0.25.
C         A smaller # will give better scale resolution, but be slower.
C
C  jtot [INT] = the # of scales.
C               Scales range from s0 up to s0*2^[(jtot-1)*dj],
C               Typically jtot=1+(LOG2(n dt/s0))/dj
C
C  npad [INT] = the total number of points (including padding) to
C               use for the wavelet transform. Typically this is some
C               power of 2. It must be greater or equal to "n".
C               If npad>n, then zeroes are padded onto the end
C               of the time series.
C
C
C OUTPUTS:
C
C  wave [DCMPLX(n,jtot)] = 2D array of the real & imaginary parts
C                   of the wavelet transform, versus time & scale.
C                   CABS(wave) gives the WAVELET amplitude,
C                   ATAN2(AIMAG(wave),DBLE(wave)) gives WAVELET phase.
C                   The WAVELET power spectrum is CABS(wave)**2.
C
C  scale [DOUBLE PRECISION(jtot)] = the wavelet scales that were used.
C
C  period [DOUBLE PRECISION(jtot)] = the "Fourier" periods (in time units) corresponding
C            to "scale".
C
C  coi [DOUBLE PRECISION(n)] = the e-folding factor used for the cone of influence.
C
C
C REQUIRES:   WAVE_FUNCTION, CFFTPACK
C
C  Copyright (C) 1998, Christopher Torrence and Gilbert P. Compo
C This software may be used, copied, or redistributed as long as it is not
C sold and this copyright notice is reproduced on each copy made.  This
C routine is provided as is without any express or implied warranties
C whatsoever.

      SUBROUTINE WAVELET (n,y,dt,mother,param,s0,dj,jtot,npad,ymean,
     &                    yvar,wave,scale,period,coi)
      IMPLICIT none

      INTEGER n,mother,jtot,npad
      DOUBLE PRECISION y(n),dt,param,s0,dj,scale(jtot),period(jtot),
     &  coi(n)
      DOUBLE COMPLEX wave(n,jtot)

      INTEGER i,j,k,nk
      DOUBLE PRECISION ymean,yvar,freq1,pi,period1,coi1

C** initialize work arrays
      PARAMETER (nk=65535)
      DOUBLE PRECISION wsave(4*nk+15),kwave(nk)
      DOUBLE COMPLEX yfft(nk),daughter(nk)

      pi = 4.D0*ATAN(1.D0)

      IF (npad.LT.n) THEN
        PRINT*,'**WAVELET: "npad" must be greater than or equal to "n"'
        RETURN
      END IF

      IF ((mother.LT.0).OR.(mother.GT.2)) mother = 0

C** find the time-series mean & remove it
c Don't need to calculate the mean here because it is being done in the
c NCL driver interface for wavelet.
c
c      ymean = 0.D0
c      DO 10 i=1,n
c        ymean = ymean + y(i)
c10    CONTINUE
c      ymean = ymean/n
      DO 15 i=1,n
        yfft(i) = y(i) - ymean
15    CONTINUE

C** if desired, pad with extra zeroes
      DO 20 i=n+1,npad
        yfft(i) = 0.D0
20    CONTINUE

C** find the FFT of the time series [Eqn(3)]
      CALL CFFTI(npad,wsave)
      CALL CFFTF(npad,yfft,wsave)
      DO 30 k=1,npad
        yfft(k) = yfft(k)/npad
30    CONTINUE

C** construct the wavenumber array [Eqn(5)]
      freq1 = 2.D0*pi/(DBLE(npad)*dt)
      kwave(1) = 0.D0
      DO 40 i=2,npad/2+1
        kwave(i) = (DBLE(i)-1.D0)*freq1
40    CONTINUE
      DO 50 i=npad/2+2,npad
        kwave(i) = -kwave(npad-i+2)
50    CONTINUE

c**----- Main wavelet loop
c      PRINT '(A8,2A12,/,5X,27("-"))','j','scale','period'
      DO 100 j=1,jtot
        scale(j) = s0*(2.D0**(DBLE(j-1)*dj))
        CALL WAVE_FUNCTION(npad,dt,mother,param,scale(j),
     &                   kwave,period1,coi1,daughter)
        period(j) = period1
C**    multiply the daughter by the time-series FFT
        DO 60 k=1,npad
          daughter(k) = daughter(k)*yfft(k)
60      CONTINUE
C**    inverse FFT [Eqn(4)]
        CALL CFFTB(npad,daughter,wsave)
C**    store the wavelet transform, discard zero-padding at end
        DO 70 i=1,n
          wave(i,j) = daughter(i)
70      CONTINUE
c        PRINT '(I8,2F12.3)',j,scale(j),period(j)
100   CONTINUE
c**----- end loop

C** construct the cone of influence
      DO 110 i=1,(n+1)/2
        coi(i) = coi1*dt*(DBLE(i)-1.D0)
        coi(n-i+1) = coi(i)
110   CONTINUE

      RETURN
      END


C****************************************************************************
C WAVE_FUNCTION: computes the daughter wavelets for a particular
C                wavelet function, with appropriate parameters.
C
C
C INPUTS:
C
C  nk [INT] = the number of points in "kwave"
C
C  dt [DOUBLE PRECISION] = amount of time between each Y value, i.e. the sampling time.
C
C  mother [INT] = An integer giving the mother wavelet to use.
C                     0='Morlet'
C                     1='Paul'
C                     2='DOG' (derivative of Gaussian)
C
C  param [DOUBLE PRECISION] = mother wavelet parameter. If <0 then default is used.
C            For 'Morlet' this is k0 (wavenumber), default is 6.
C            For 'Paul' this is m (order), default is 4.
C            For 'DOG' this is m (m-th derivative), default is 2.
C
C  scale1 [DOUBLE PRECISION] = the wavelet scale used to construct the daughter.
C
C  kwave [DOUBLE PRECISION(n)] = vector of wavenumbers, used to construct daughter.
C
C
C OUTPUTS:
C
C  period1 [DOUBLE PRECISION] = the "Fourier" period (in time units) that corresponds
C            to "scale1".
C
C  coi1 [DOUBLE PRECISION] = the e-folding factor used for the cone of influence.
C
C  daughter [DCMPLX(nk)] = real & imaginary parts of the wavelet function
C                         at "scale1" and "kwave".
C
C
C REQUIRES:   FACTORIAL, CHISQR
C
C
C Reference: Tables 1 & 2 in
C            Torrence, C. and G. P. Compo, 1998: A Practical Guide to
C            Wavelet Analysis. <I>Bull. Amer. Meteor. Soc.</I>, 79, 61-78.
C
C  Copyright (C) 1998, Christopher Torrence and Gilbert P. Compo
C This software may be used, copied, or redistributed as long as it is not
C sold and this copyright notice is reproduced on each copy made.  This
C routine is provided as is without any express or implied warranties
C whatsoever.

      SUBROUTINE WAVE_FUNCTION (nk,dt,mother,param,scale1,
     &                          kwave,period1,coi1,daughter)
      IMPLICIT none

      INTEGER nk,mother
      DOUBLE PRECISION dt,kwave(nk),param,scale1,period1,coi1
      DOUBLE COMPLEX daughter(nk)

      DOUBLE PRECISION expnt,sk,pi,norm,fourier_factor
      INTEGER k,m,factorial
      DOUBLE PRECISION gamma

      pi = 4.D0*ATAN(1.D0)

      IF (mother.EQ.0) THEN
C*******************************************   Morlet wavelet
        IF (param.LT.0) param = 6.D0
        norm = SQRT(2.D0*pi*scale1/dt)*(pi**(-0.25D0))
        DO 10 k=1,nk/2+1
          expnt = -0.5D0*(scale1*kwave(k) - param)**2
          daughter(k) = DCMPLX(norm*EXP(expnt))
10      CONTINUE
         DO 20 k=nk/2+2,nk
          daughter(k) = DCMPLX(0.D0)
20      CONTINUE
        fourier_factor = (4.D0*pi)/(param + SQRT(2.D0+param**2))
        period1 = scale1*fourier_factor
        coi1 = fourier_factor/SQRT(2.D0)
      ELSE IF (mother.EQ.1) THEN
C*******************************************   Paul wavelet
        IF (param.LT.0) param = 4.D0
        m = INT(param)
        norm = SQRT(2.D0*pi*scale1/dt)*
     &         (2.D0**m/SQRT(DBLE(m*FACTORIAL(2*m-1))))
        DO 30 k=1,nk/2+1
          expnt = -scale1*kwave(k)
          daughter(k) = DCMPLX(norm*(scale1*kwave(k))**m*EXP(expnt))
30      CONTINUE
         DO 40 k=nk/2+2,nk
          daughter(k) = DCMPLX(0.D0)
40      CONTINUE
        fourier_factor = (4.D0*pi)/(2.D0*m + 1.D0)
        period1 = scale1*fourier_factor
        coi1 = fourier_factor*SQRT(2.D0)
      ELSE IF (mother.EQ.2) THEN
C*******************************************   DOG wavelet
        IF (param.LT.0) param = 2.D0
        m = INT(param)
        norm = SQRT(2.D0*pi*scale1/dt)*SQRT(1.D0/GAMMA(m+0.5D0))
        norm = -norm*(DCMPLX(0.D0,1.D0)**m)
        DO 50 k=1,nk
          sk = scale1*kwave(k)
          daughter(k) = norm*(sk**m)*EXP(-0.5D0*sk**2)
50      CONTINUE
        fourier_factor = 2.D0*pi*SQRT(2.D0/(2.D0*m+1.D0))
        period1 = scale1*fourier_factor
        coi1 = fourier_factor/SQRT(2.D0)
      ELSE
        stop
      END IF
      RETURN
      END


C****************************************************************************
C FACTORIAL: compute the factorial (n!) of an integer n
C  Copyright (C) 1998, Christopher Torrence
      FUNCTION FACTORIAL(n)
      IMPLICIT NONE
      INTEGER factorial,n,i

      factorial = 1
      DO 10 i=1,n
        factorial = factorial*i
10    CONTINUE
      END



C****************************************************************************
C WAVE_SIGNIF: computes the significance levels for a wavelet transform.
C
C
C INPUTS:
C
C  isigtest [INT] = 0, 1, or 2.
C
C          If 0, then just do a regular chi-square test,
C                i.e. Eqn (18) from Torrence & Compo.
C          If 1, then do a "time-average" test, i.e. Eqn (23).
C                In this case, DOF(j) should be set to NA, the number
C                of local wavelet spectra that were averaged together
C                at each scale. For the Global Wavelet Spectrum,
C                this would be dof(j)=N-scale(j),
C                where N is the number of points in your time series.
C          If 2, then do a "scale-average" test, i.e. Eqns (25)-(28).
C                In this case, "dof(1)" and "dof(2)" should be set to the
C                smallest (S1) and largest (S2) scales that were averaged
C                together, respectively.
C                e.g. if you scale-averaged scales between 2 and 8,
C                     then dof(1)=2.0 and dof(2)=8.0
C
C
C  n [INT] = the number of points in "y".
C
C  y [DOUBLE PRECISION] = the time series of length "n".
C
C  dt [DOUBLE PRECISION] = amount of time between each Y value, i.e. the sampling time.
C
C  mother [INT] = An integer giving the mother wavelet to use.
C                     0='Morlet'
C                     1='Paul'
C                     2='DOG' (derivative of Gaussian)
C
C  param [DOUBLE PRECISION] = mother wavelet parameter.
C
C  s0 [DOUBLE PRECISION] = the smallest scale of the wavelet.
C
C  dj [DOUBLE PRECISION] = the spacing between discrete scales.
C
C  jtot [INT] = the # of scales.
C
C  scale [DOUBLE PRECISION(jtot)] = the wavelet scales that were used.
C
C  period [DOUBLE PRECISION(jtot)] = the "Fourier" periods corresponding to "scale".
C
C  lag1 [DOUBLE PRECISION] = lag 1 Autocorrelation, used for signif levels.
C                Default is 0.0, which corresponds to white-noise.
C
C  siglvl [DOUBLE PRECISION] = significance level to use. Default is 0.05 (the "5%" level)
C
C  dof [DOUBLE PRECISION(jtot)] = degrees-of-freedom for signif test.
C     IF SIGTEST=0, then the input dof is ignored.
C     IF SIGTEST=1, then dof(j) = NA, the number of times averaged together.
C     IF SIGTEST=2, then dof(1)=S1, dof(2)=S2, the range of scales averaged.
C
C
C OUTPUTS:
C
C  dof [DOUBLE PRECISION(jtot)] = degrees-of-freedom that were actually used.
C     IF SIGTEST=0, then dof(j) = 2 (or 1 for the 'DOG')
C     IF SIGTEST=1, then dof(j) = degrees-of-freedom versus scale.
C     IF SIGTEST=2, then dof(1)=degrees-of-freedom, dof(2...jtot)=0.0
C
C  fft_theor [DOUBLE PRECISION(jtot)] = theoretical red-noise spectrum vs scale.
C     IF SIGTEST=2, then fft_theor(1) = the average spectrum from S1-->S2
C                        fft_theor(2...jtot) = 0.0
C
C  signif [DOUBLE PRECISION(jtot)] = significance levels vs scale.
C     IF SIGTEST=2, then signif(1) = the significance level
C                        signif(2...jtot) = 0.0
C
C  ymean [DOUBLE PRECISION] = the mean of the time series.
C
C  variance [DOUBLE PRECISION] = the variance of the time series.
C
C  Cdelta [DOUBLE PRECISION] = the constant "Cdelta" for the mother wavelet (Table 2).
C
C  psi0[DOUBLE PRECISION] = the constant 'psi(0)' for the mother wavelet (Table 2)
C
C REQUIRES:   CHISQR
C
C  Copyright (C) 1998, Christopher Torrence and Gilbert P. Compo
C This software may be used, copied, or redistributed as long as it is not
C sold and this copyright notice is reproduced on each copy made.  This
C routine is provided as is without any express or implied warranties
C whatsoever.

      SUBROUTINE WAVE_SIGNIF (isigtest,n,y,dt,mother,param,dj,jtot,
     &       scale,period,lag1,siglvl,dof,fft_theor,signif,
     &       ymean,variance,Cdelta,psi0)
      IMPLICIT none

      INTEGER isigtest,n,mother,jtot
      DOUBLE PRECISION y(n),dt,param,dj,scale(jtot),period(jtot)
      DOUBLE PRECISION lag1,siglvl,dof(jtot),fft_theor(jtot),
     &  signif(jtot)
      DOUBLE PRECISION ymean,variance,Cdelta,psi0

      INTEGER i,j,m,status,javg1,javg2,navg
      DOUBLE PRECISION pi,freq1,dofmin,gammafac,dj0,Savg,Smid
      DOUBLE PRECISION fft_theor1
      DOUBLE PRECISION chisqr,p,q,bound

      pi = 4.D0*ATAN(1.D0)

      IF (siglvl.LE.0.) siglvl = 0.05D0
      IF (lag1.LE.0.D0) lag1 = 0.D0

      Cdelta = -1.D0
      gammafac = -1.D0
      dj0 = -1.D0
      psi0 = -1.D0

      IF (mother.EQ.0) THEN
C*******************************************   Morlet wavelet
        dofmin = 2.D0
        IF (param.EQ.6.D0) THEN
          Cdelta = 0.776D0
          gammafac = 2.32D0
          dj0 = 0.60D0
          psi0 = pi**(-0.25D0)
        END IF
      ELSE IF (mother.EQ.1) THEN
C*******************************************   Paul wavelet
        m = INT(param)
        dofmin = 2.D0
        IF (m.EQ.4) THEN
          Cdelta = 1.132D0
          gammafac = 1.17D0
          dj0 = 1.5D0
          psi0 = 1.079D0
        END IF
      ELSE IF (mother.EQ.2) THEN
C*******************************************   DOG wavelet
        m = INT(param)
        dofmin = 1.D0
        IF (m.EQ.2) THEN
          Cdelta = 3.541D0
          gammafac = 1.43D0
          dj0 = 1.4D0
          psi0 = 0.867D0
        ELSE IF (m.EQ.6) THEN
          Cdelta = 1.966D0
          gammafac = 1.37D0
          dj0 = 0.97D0
          psi0 = 0.884D0
        END IF
      ELSE
        stop
      END IF

C** find the time-series variance
      ymean = 0.D0
      DO 10 i=1,n
        ymean = ymean + y(i)
10    CONTINUE
      ymean = ymean/n
      variance = 0.D0
      DO 15 i=1,n
        variance = variance + (y(i) - ymean)**2
15    CONTINUE
      variance = variance/(DBLE(n)) ! - 1.D0)

C** construct theoretical red(white)-noise power spectrum [Eqn(16)]
      DO 20 j=1,jtot
        freq1 = dt/period(j)
        fft_theor(j) = variance*(1.D0-lag1**2)/
     &                 (1.D0 - 2.D0*lag1*COS(freq1*2.D0*pi) + lag1**2)
20    CONTINUE

      q = DBLE(siglvl)
      p = 1d0 - q

      IF (isigtest.EQ.0) THEN
C*******************************************   no smoothing, dof=dofmin
C   see Eqn(18)
        DO 30 j=1,jtot
          dof(j) = dofmin
          CALL CDFCHI(2,p,q,chisqr,DBLE(dofmin),status,bound)
          signif(j) = fft_theor(j)*chisqr/dofmin
30      CONTINUE
      ELSE IF (isigtest.EQ.1) THEN
C***********************************   time-averaged, dof depend on scale
        IF (gammafac.LE.0.D0) THEN
        PRINT*,'**WAVE_SIGNIF: "gammafac" undefined for this wavelet'
          RETURN
        END IF
        DO 40 j=1,jtot
            IF (dof(j).LT.1.) dof(j) = 1.D0
C   see Eqn(23)
            dof(j) = dofmin*SQRT(1.D0+(dof(j)*dt/gammafac/scale(j))**2)
            IF (dof(j).LT.dofmin) dof(j) = dofmin
            CALL CDFCHI(2,p,q,chisqr,DBLE(dof(j)),status,bound)
            signif(j) = fft_theor(j)*chisqr/dof(j)
40      CONTINUE
       ELSE IF (isigtest.EQ.2) THEN
C***********************************   scale-averaged, dof depend on scale
        IF (Cdelta.LE.0.) THEN
          PRINT*,'**WAVE_SIGNIF: "Cdelta" and "dj0" '//
     &     'undefined for this wavelet'
          RETURN
        END IF
        javg1 = 0
        javg2 = 0
        DO 50 j=1,jtot
          IF ((scale(j).GE.dof(1)).AND.(javg1.EQ.0)) javg1 = j
          IF (scale(j).LE.dof(2)) javg2 = j
50      CONTINUE
        IF ((javg1.EQ.0).OR.(javg2.EQ.0).OR.(javg1.GT.javg2)) THEN
          PRINT*,'**WAVE_SIGNIF: Scales in "dof(1)" & "dof(2)" '//
     &     'are out of range.'
          RETURN
        END IF
        navg = javg2 - javg1 + 1
C   see Eqn(25)
        Savg = 0.D0
        DO 60 j=javg1,javg2
          Savg = Savg + 1.D0/scale(j)
60      CONTINUE
        Savg = 1.D0/Savg
C   see Eqn(27)
        fft_theor1 = 0.D0
        DO 70 j=javg1,javg2
          fft_theor1 = fft_theor1 + fft_theor(j)/scale(j)
70      CONTINUE
        fft_theor(1) = Savg*fft_theor1
C   see Eqn(28)
        Smid = EXP(0.5D0*(LOG(scale(javg1)) + LOG(scale(javg2))))
        dof(1) = (dofmin*navg*Savg/Smid)*SQRT(1 + (navg*dj/dj0)**2)
C   see Eqn(26)
        CALL CDFCHI(2,p,q,chisqr,DBLE(dof(1)),status,bound)
        signif(1)=(dj*dt/Cdelta/Savg)*fft_theor(1)*chisqr/dof(1)
        DO 80 j=2,jtot
          dof(j) = 0.D0
          fft_theor(j) = 0.D0
          signif(j) = 0.D0
80      CONTINUE
      ELSE
        stop
      END IF

      END

