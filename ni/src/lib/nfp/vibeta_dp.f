c ----------------------------------------------------------------------
      SUBROUTINE DVIBETA(P,X,NLEV,XMSG,LINLOG,PSFC,XSFC,PBOT,PTOP,
     +                   PLVCRT,VINT,IER)
      IMPLICIT NONE
c
c NCL: vint = vibeta (p,x,linlog,psfc,pbot,ptop)
c NCL:        plvcrt = p(nlev)  do internally
c NCL:        xsfc   = x(1)     do internally
c
c vertical integral using ket/as:
c .   "diagnostic studies using global analyses"

c .   the approach here is somewhat crude 'n lude.
c .   i did it to preserve a one-to-one correspondence
c .   between subscripts in the above paper and the
c .   program below.

c .   also, this is *far* more complicated then is
c .   generally needed. It was wriiten to handle
c .   real raob data which have missing values in
c .   the middle of a sounding

c nomenclature:
c .   p      - pressure levels
c .   x      - quantity to be integrated
c .   nlev   - number of levels
c .   xmsg   - missing data code
c .   linlog - linear (=1) or log (=2) interpolation
c .   psfc   - surface pressure
c .   xsfc   - value of quantity to be integrated at the surface
c .   pbot   - lower limit of integration
c .   ptop   - upper limit of integration
c .   plvcrt - minimum pressure level which sounding must
c .            reach for integration to proceed (plvcrt.le.ptop)
c .   vint   - result
c .   ier    - error code

      INTEGER NLEV,LINLOG,IER
      DOUBLE PRECISION P(NLEV),X(NLEV),PSFC,XSFC,PBOT,PTOP,PLVCRT,XMSG,
     +                 VINT

c local vectors and variables

      INTEGER KLVL,NL1,NL,NLL,NLMAX,NLTOP,NLSAV,NL2,NLX
      DOUBLE PRECISION ZERO,PZERO,XZERO,SLOPE,PA,PC,PSFCX
      LOGICAL VDBG,PFLAG

      PARAMETER (KLVL=300)
      DOUBLE PRECISION PP(KLVL),XX(KLVL),PI(0:KLVL),XI(0:KLVL),
     +                 BETA(0:KLVL),DELP(0:KLVL)

      DATA ZERO/0.D0/
      DATA PZERO/0.D0/
      DATA XZERO/0.D0/

c error checking

      IER = 0
      IF (NLEV.LT.3) IER = IER + 1
C make klvl larger
      IF (KLVL.LT.2*NLEV) IER = IER + 10
      IF (PSFC.EQ.XMSG) IER = IER + 100
C
C Dennis wanted this to be commented out. See email exchange
C on November 14, 2003.
C
C      IF (PLVCRT.LT.PTOP) IER = IER + 1000
      IF (PTOP.GE.PBOT) IER = IER + 10000

      IF (IER.NE.0) THEN
          VINT = XMSG
          RETURN
      END IF

c collapse the sounding to eliminate msg data
c .   this is for raobs which have data missing in the
c .   middle of a sounding

C total number of levels with data
      NLL = 0
C last lvl with data
      NLX = 0
      PFLAG = .false.
C initilize to msg code
      CALL DINITVC(PP,KLVL,XMSG,IER)
C =0. means not included in integral
      CALL DINITVC(XX,KLVL,ZERO,IER)
      DO NL = 1,NLEV
          IF (P(NL).NE.XMSG .AND. X(NL).NE.XMSG .AND.
     +        P(NL).LT.PSFC) THEN
              NLX = NL
              NLL = NLL + 1
              PP(NLL) = P(NL)
              XX(NLL) = X(NL)
              IF (AINT(PP(NLL)).EQ.AINT(PTOP)) PFLAG = .true.
          END IF
      END DO
C no. levels with data above sfc
      NLMAX = NLL

c make sure there are at least several levels.

      IF (NLMAX.LE.3) THEN
          IER = -999
          VINT = XMSG
          RETURN
      END IF

c explicitly fill the top levels with zeros

      IF (NLMAX.LT.NLEV) THEN
          DO NL = NLX + 1,NLEV
              NLL = NLL + 1
              PP(NLL+1) = P(NL)
C contribute nothing to the integral
              XX(NLL+1) = 0.0D0
          END DO
      END IF

c explicitly add a ptop  level if neccessary. [remember the daily
c .   raob data may not have all levels.]
c .   Generally, this if block is not executed.

      IF (PP(NLMAX).GT.PTOP) THEN
C add level at ptop
          NLMAX = NLMAX + 1
C assume p=0, x=0 is next level above.
          PP(NLMAX) = PTOP
          IF (LINLOG.EQ.1) THEN
              SLOPE = (XX(NLMAX-1)-XZERO)/ (PP(NLMAX-1)-PZERO)
              XX(NLMAX) = XZERO + (PP(NLMAX)-PZERO)*SLOPE
          ELSE IF (LINLOG.EQ.2) THEN
              SLOPE = XX(NLMAX-1)/DLOG(PP(NLMAX-1))
              XX(NLMAX) = XZERO + DLOG(PP(NLMAX))*SLOPE
          END IF

      ELSE IF (.NOT.PFLAG) THEN
C beta=temporary storage
          CALL DCOPYVC(PP(1),BETA(1),NLMAX,IER)
C delp=temporary storage
          CALL DCOPYVC(XX(1),DELP(1),NLMAX,IER)
C determine where
          DO NL = 1,NLMAX - 1
C ptop should be inserted
              IF (AINT(PP(NL)).GT.PTOP .AND.
     +            AINT(PP(NL+1)).LT.PTOP) THEN
C subscript location for ptop
                  NLTOP = NL + 1
                  NLSAV = NLTOP
                  GO TO 25
              END IF
          END DO
          IER  = 1000000
          VINT = XMSG
C C C     WRITE (*,FMT='(/,'' sub vibeta: problem: ier='',i9)')
          RETURN

   25     NL1 = NLTOP
          NL2 = NLTOP - 1
          IF (LINLOG.EQ.1) THEN
              SLOPE = (XX(NL2)-XX(NL1))/ (PP(NL2)-PP(NL1))
              XX(NLTOP) = XX(NL1) + (PTOP-PP(NL1))*SLOPE
          ELSE IF (LINLOG.EQ.2) THEN
              PA = DLOG(PP(NL2))
              PC = DLOG(PP(NL1))
              SLOPE = (XX(NL2)-XX(NL1))/ (PA-PC)
              XX(NLTOP) = XX(NL1) + (DLOG(PTOP)-PC)*SLOPE
          END IF

          PP(NLTOP) = PTOP

C push all levels up one subscript
          DO NL = NLSAV,NLMAX
              XX(NL+1) = DELP(NL)
              PP(NL+1) = BETA(NL)
          END DO
          NLMAX = NLMAX + 1
      END IF

c the pp and xx vectors now hold "good" data.
c .   pi and xi will be the vectors used by the integration routine.
c .   the levels associated with pi and xi correspond to
c .   the levels in fig 1 of trenberth (jc, 1991, p708)

      CALL DINITVC(PI(0), (KLVL+1),ZERO,IER)
      CALL DINITVC(XI(0), (KLVL+1),ZERO,IER)

      PI(0) = PBOT
      DO NL = 1,NLMAX - 1
C odd : levels with data
          PI(2*NL-1) = PP(NL)
C even: intermediate levels
          PI(2*NL) = (PP(NL)+PP(NL+1))*0.5D0
      END DO
      PI(2*NLMAX-1) = PP(NLMAX)
      PI(2*NLMAX) = (PP(NLMAX)+ZERO)*0.5D0

c determine the subscript of the pi vector which corresponds to ptop
c .   over ride the last two pi levels if (2*nlmax-1).ne.nltop

      NLTOP = 0
      DO NL = 1,2*NLMAX - 1
C will be odd level
          IF (AINT(PI(NL)).EQ.AINT(PTOP)) NLTOP = NL
      END DO
      IF (NLTOP.EQ.0 .OR. MOD(NLTOP,2).EQ.0) THEN
          WRITE (*,FMT='(/,'' sub viket: nltop problem='',i5)') NLTOP
          STOP
      END IF
      PI(2*NLTOP-1) = PTOP
      PI(2*NLTOP) = (PTOP+ZERO)*0.5D0

c interpolate "xi" at intermediate levels using
c .   linear (linlog=1) or log (linlog=2) interpolation.
c .   do not use the sfc pressure [pp(0)].

      IF (PSFC.NE.XMSG) THEN
          PSFCX = PSFC
      ELSE
          PSFCX = PP(1)
      END IF

c c c if (xsfc.ne.xmsg) then
c c c     xi(0) = xsfc
c c c else
c c c     xi(0) = xx(1)
c c c endif
      CALL DINT2P2(PP(1),XX(1),NLMAX,PI(1),XI(1),NLTOP,LINLOG,XMSG,IER)

c determine beta and delp
c .   odd levels correspond to actual raob data

      CALL DINITVC(DELP(0), (KLVL+1),ZERO,IER)
      CALL DINITVC(BETA(0), (KLVL+1),ZERO,IER)

      DO NL = 1,NLTOP
          DELP(NL) = PI(NL-1) - PI(NL+1)
      END DO

      DO NL = 1,NLTOP
          IF (PI(NL-1).GE.PSFCX .AND. PI(NL+1).LT.PSFCX) THEN
              BETA(NL) = (PSFCX-PI(NL+1))/ (PI(NL-1)-PI(NL+1))
          ELSE IF (PI(NL-1).GT.PTOP .AND. PI(NL+1).LT.PTOP) THEN
              BETA(NL) = (PI(NL-1)-PTOP)/ (PI(NL-1)-PI(NL+1))
          ELSE IF (PI(NL).LT.PBOT .AND. PI(NL).GE.PTOP) THEN
              BETA(NL) = 1.0D0
          END IF
      END DO

c calculate the integral (odd levels only)

      VINT = 0.0D0
      DO NL = 1,NLTOP,2
          VINT = VINT + BETA(NL)*XI(NL)*DELP(NL)
      END DO

c optional print

      VDBG = .false.
      IF (VDBG) THEN
          WRITE (*,FMT=
     +      '(/,'' sub vibeta: vint='',3i5,2(1x,f10.2),2f8.1,/)') NLMAX,
     +      2*NLMAX - 1,NLTOP,VINT,XSFC,PSFC,PSFCX
          WRITE (*,FMT='(3x,''nl'',8x,''pp'',8x,''xx from viket'')')
c c c     do nl=1,nlmax
c c c     if (pp(nl).ge.ptop) then
c c c         write (*,'(i5,1x,f9.3,1x,f9.6 )')
c c c*                  nl, pp(nl),xx(nl)
c c c     endif
c c c     enddo

          WRITE (*,FMT=
     +'(//,3x,''nl'',8x,''pi'',8x,''xi''
     +    ,6x,''delp'',6x,''beta from vibeta'')')
          DO NL = 0,NLTOP
c c c     if (pi(nl).ge.ptop) then
              WRITE (*,FMT='(i5,1x,f9.3,1x,f9.6,2(1x,f9.3) )') NL,
     +          PI(NL),XI(NL),DELP(NL),BETA(NL)
c c c     endif
          END DO
      END IF

      RETURN
      END
c ------------------------------------------------------
      SUBROUTINE DINT2P2(PIN,XIN,NPIN,POUT,XOUT,NPOUT,IFLAG,XMSG,IER)
      IMPLICIT NONE

c routine to interpolate from one set of pressure levels
c .   to another set  using linear or ln(p) interpolation

c CAUTION: THIS ROUTINE HAS NOT BEEN EXTENSIVELY TESTED

c nomenclature:

c .   pin    - input pressure levels. The pin should
c .            be in decending order (e.g., 1000,900,825,..)
c .            pin(1)>pin(2)>...>pin(npin)
c .   xin    - data at input pressure levels
c .   npin   - number of input pressure levels
c .   pout   - output pressure levels (input by user)
c .            decending order required.
c .   xout   - data at output pressure levels
c .   npout  - number of output pressure levels
c .   iflag  - if iflag=1 user linear interp in pressure
c .            if iflag=2 user linear interp in ln(pressure)
c .   xmsg   - missing data code. if none, set to some number
c .            which will not be encountered (e.g., 1.e+36)
c .   ier    - error code

C input types
      INTEGER NPIN,NPOUT,IFLAG,IER
      DOUBLE PRECISION PIN(NPIN),XIN(NPIN),XMSG,POUT(NPOUT),XOUT(NPOUT)

      INTEGER NPLVL,NP,NL,NIN,NLMAX
C local vector space
      PARAMETER (NPLVL=200)
      DOUBLE PRECISION P(NPLVL),X(NPLVL)
      DOUBLE PRECISION SLOPE,PA,PB,PC

c error check

      IER = 0
      IF (NPIN.LT.1 .OR. NPOUT.LT.1) IER = IER + 1
      IF (IFLAG.LT.1 .OR. IFLAG.GT.2) IER = IER + 10
C increase vector size
      IF (NPIN.GT.NPLVL) IER = IER + 100
      IF (IER.NE.0) THEN
          IF (NPOUT.GT.0) THEN
              DO NP = 1,NPOUT
                  XOUT(NP) = XMSG
              END DO
          END IF
          RETURN
      END IF

c eliminate levels with missing data. This can easily
c .   happen with observational data.

      NL = 0
      DO NIN = 1,NPIN
          IF (XIN(NIN).NE.XMSG .AND. PIN(NIN).NE.XMSG) THEN
              NL = NL + 1
              P(NL) = PIN(NIN)
              X(NL) = XIN(NIN)
          END IF
      END DO
      NLMAX = NL
      IF (NLMAX.EQ.0) THEN
C all missing data
          IER = IER + 1000
          RETURN
      END IF

c perform the interpolation
c                                                      ( p ,x)
c ------------------------- p(nl+1), x(nl+1)   example (200,5)
c .
c ------------------------- pout(np), xout(np)         (250,?)
c .
c ------------------------- p(nl)  , x(nl)             (300,10)

      DO NP = 1,NPOUT
          XOUT(NP) = XMSG
          DO NL = 1,NLMAX
              IF (POUT(NP).EQ.P(NL)) THEN
                  XOUT(NP) = X(NL)
              ELSE IF (NL.LT.NLMAX) THEN
                  IF (POUT(NP).LT.P(NL) .AND. POUT(NP).GT.P(NL+1)) THEN
                      IF (IFLAG.EQ.1) THEN
                          SLOPE = (X(NL)-X(NL+1))/ (P(NL)-P(NL+1))
                          XOUT(NP) = X(NL+1) + SLOPE* (POUT(NP)-P(NL+1))
                      ELSE
                          PA = DLOG(P(NL))
                          PB = DLOG(POUT(NP))
                          PC = DLOG(P(NL+1))
                          SLOPE = (X(NL)-X(NL+1))/ (PA-PC)
                          XOUT(NP) = X(NL+1) + SLOPE* (PB-PC)
                      END IF
                  END IF
              END IF
          END DO
      END DO

      RETURN
      END
c ------------------------------------------------
      SUBROUTINE DINITVC(X,NPTS,VALUE,IER)
      IMPLICIT NONE

c set all elements of a vector to a value

c arguments :
c .   x        - vector to be set to the value
c .   npts     - number of points to be set
c .   value    - value that each element of x is to be set to
c .   ier      - if (ier.ne.0) an error has occured

      INTEGER NPTS,IER
      DOUBLE PRECISION X(1:NPTS),VALUE

C local
      INTEGER N

      IER = 0
      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF

      DO N = 1,NPTS
          X(N) = VALUE
      END DO

      RETURN
      END
c -----------------------------------------------
      SUBROUTINE DCOPYVC(X,Y,NPTS,IER)
      IMPLICIT NONE
c establish a duplicate vector
c arguments :
c .   x        - input vector
c .   y        - output vector (same as x)
c .   npts     - number of points
c .   ier      - if (ier.ne.0) an error has occurred
      INTEGER NPTS,IER
      DOUBLE PRECISION X(1:NPTS),Y(1:NPTS)
      INTEGER N

      IER = 0
      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF

      DO N = 1,NPTS
          Y(N) = X(N)
      END DO
      RETURN
      END
