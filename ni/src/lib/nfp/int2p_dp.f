      SUBROUTINE DINT2P(PIN,XIN,P,X,NPIN,POUT,XOUT,NPOUT,LINLOG,XMSG,IER)
      IMPLICIT NONE

c routine to interpolate from one set of pressure levels
c .   to another set  using linear or ln(p) interpolation
c
c NCL: xout = int2p (pin,xin,pout,linlog)
c
c nomenclature:
c
c .   pin    - input pressure levels. The pin should
c .            be in decending order (e.g., 1000,900,825,..)
c .            pin(1)>pin(2)>...>pin(npin)
c .   xin    - data at input pressure levels
c .   npin   - number of input pressure levels
c .   pout   - output pressure levels (input by user)
c .            decending order required.
c .   xout   - data at output pressure levels
c .   npout  - number of output pressure levels
c .   linlog - if linlog=1 use linear interp in pressure
c .            if linlog anything but =1 linear interp in ln(pressure)
c .   xmsg   - missing data code. if none, set to some number
c .            which will not be encountered (e.g., 1.e+36)
c .   ier    - error code

C input types
      INTEGER NPIN,NPOUT,LINLOG,IER
      DOUBLE PRECISION PIN(NPIN),XIN(NPIN),POUT(NPOUT),XMSG
C output
      DOUBLE PRECISION XOUT(NPOUT)

      INTEGER J1,NP,NL,NIN,NLMAX
      DOUBLE PRECISION SLOPE,PA,PB,PC
      DOUBLE PRECISION P(NPIN),X(NPIN)

c error check

      IER = 0
      IF (NPIN.LT.1 .OR. NPOUT.LT.1) IER = IER + 1
      IF (IER.NE.0) THEN
          IF (NPOUT.GT.0) THEN
              DO NP = 1,NPOUT
                  XOUT(NP) = XMSG
              END DO
          END IF
          RETURN
      END IF
c
c eliminate levels with missing data. This can easily
c .   happen with observational data.
c
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
c
c perform the interpolation
c                                                      ( p ,x)
c ------------------------- p(nl+1), x(nl+1)   example (200,5)
c .
c ------------------------- pout(np), xout(np)         (250,?)
c .
c ------------------------- p(nl)  , x(nl)             (300,10)
c
      DO NP = 1,NPOUT
          XOUT(NP) = XMSG
          DO NL = 1,NLMAX
              IF (POUT(NP).EQ.P(NL)) THEN
                  XOUT(NP) = X(NL)
              ELSE IF (NL.LT.NLMAX) THEN
                  IF (POUT(NP).LT.P(NL) .AND. POUT(NP).GT.P(NL+1)) THEN
                      IF (LINLOG.EQ.1) THEN
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
c
      RETURN
      END
