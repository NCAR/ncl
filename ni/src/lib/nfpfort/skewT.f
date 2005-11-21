c (1) Routines from the "thermo.f" package that are
c .   used to generate the skewT plot.
c (2) Routines used to plot the data

c only routines that have NCLFORTSTART/NCLEND need have wrappers
C -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DSKEWTY(PRES,NP,YSKEWT)

C       NCL: y = ySkewT (pres[*]:numeric)   ; np is dimension of pres

      INTEGER NP
      DOUBLE PRECISION PRES(NP)
C                                dynamically allocate ySkewT in wrapper
      DOUBLE PRECISION YSKEWT(NP)
C NCLEND
      INTEGER N

      DO N = 1,NP
          YSKEWT(N) = 132.182D0 - 44.061D0*LOG10(PRES(NP))
      END DO

      RETURN
      END
C -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DSKEWTX(TEMP,Y,NTY,XSKEWT)


C NCL: y = xSkewT (temp[*]:numeric, y[*]:numeric) ; nty dimension of y

      INTEGER NTY
      DOUBLE PRECISION TEMP(NTY),Y(NTY)
C                                dynamically allocate in wrapper
      DOUBLE PRECISION XSKEWT(NTY)
C NCLEND
      INTEGER N

      DO N = 1,NTY
          XSKEWT(N) = 0.54D0*TEMP(N) + 0.90692D0*Y(N)
      END DO

      RETURN
      END
C -----------------------------------------------------------
C NCLFORTSTART
      DOUBLE PRECISION FUNCTION DTMRSKEWT(W,P)
      DOUBLE PRECISION W
      DOUBLE PRECISION P
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION C3
      DOUBLE PRECISION C4
      DOUBLE PRECISION C5
      DOUBLE PRECISION C6
      DOUBLE PRECISION X
      DOUBLE PRECISION TMRK

C       NCL: tmr = tmrSkewT (w:numeric, p:numeric)

C NCLEND
c   this function returns the temperature (celsius) on a mixing
c   ratio line w (g/kg) at pressure p (mb). the formula is given in
c   table 1 on page 7 of stipanuk (1973).
c
      DATA C1/.0498646455D0/,C2/2.4082965D0/,C3/7.07475D0/
      DATA C4/38.9114D0/,C5/.0915D0/,C6/1.2035D0/

      X = DLOG10(W*P/ (622.D0+W))
      TMRK = 10.D0** (C1*X+C2) - C3 + C4* ((10.D0** (C5*X)-C6)**2.D0)
      DTMRSKEWT = TMRK - 273.15D0
      RETURN
      END
C -----------------------------------------------------------
C NCLFORTSTART
      DOUBLE PRECISION FUNCTION DTDASKEWT(O,P)
      DOUBLE PRECISION O
      DOUBLE PRECISION P
      DOUBLE PRECISION OK
      DOUBLE PRECISION TDAK

C       NCL: tda = tdaSkewT (o:numeric, p:numeric)

c NCLEND
c   this function returns the temperature tda (celsius) on a dry adiabat
c   at pressure p (millibars). the dry adiabat is given by
c   potential temperature o (celsius). the computation is based on
c   poisson's equation.

      OK = O + 273.15D0
      TDAK = OK* ((P*.001D0)**.286D0)
      DTDASKEWT = TDAK - 273.15D0
      RETURN
      END
C -----------------------------------------------------------
C NCLFORTSTART
      DOUBLE PRECISION FUNCTION DSATLFTSKEWT(THW,P)
      DOUBLE PRECISION THW
      DOUBLE PRECISION P
      DOUBLE PRECISION CTA
      DOUBLE PRECISION AKAP
      DOUBLE PRECISION PWRP
      DOUBLE PRECISION TONE
      DOUBLE PRECISION EONE
      DOUBLE PRECISION DWOBFSKEWT
      DOUBLE PRECISION RATE
      DOUBLE PRECISION TTWO
      DOUBLE PRECISION ETWO
      DOUBLE PRECISION PT
      DOUBLE PRECISION DLT

C       NCL: satlft = satlftSkewT (thw:numeric, p:numeric)

c NCLEND

c   input:  thw = wet-bulb potential temperature (celsius).
c                 thw defines a moist adiabat.
c           p = pressure (millibars)
c   output: satlft = temperature (celsius) where the moist adiabat
c                 crosses p

      DATA CTA,AKAP/273.15D0,0.28541D0/

c   cta = difference between kelvin and celsius temperatures
c   akap = (gas constant for dry air) / (specific heat at constant
c           pressure for dry air)

c        the algorithm below can best be understood by referring to a
c   skew-t/log p chart.  it was devised by herman wobus, a mathemati-
c   cian formerly at the navy weather research facility but now retired.
c   the value returned by satlft can be checked by referring to table
c   78, pp.319-322, smithsonian meteorological tables, by roland list
c   (6th revised edition).

      IF (P.NE.1000.D0) GO TO 5
      DSATLFTSKEWT = THW
      RETURN
    5 CONTINUE

c   compute tone, the temperature where the dry adiabat with value thw
c   (celsius) crosses p.

      PWRP = (P/1000.D0)**AKAP
      TONE = (THW+CTA)*PWRP - CTA

c   consider the moist adiabat ew1 through tone at p.  using the defini-
c   tion of the wobus function (see documentation on wobf), it can be
c   shown that eone = ew1-thw.

      EONE = DWOBFSKEWT(TONE) - DWOBFSKEWT(THW)
      RATE = 1.D0
      GO TO 15

c   in the loop below, the estimate of satlft is iteratively improved.

   10 CONTINUE

c   rate is the ratio of a change in t to the corresponding change in
c   e.  its initial value was set to 1 above.

      RATE = (TTWO-TONE)/ (ETWO-EONE)
      TONE = TTWO
      EONE = ETWO
   15 CONTINUE

c   ttwo is an improved estimate of satlft.

      TTWO = TONE - EONE*RATE

c   pt is the potential temperature (celsius) corresponding to ttwo at p

      PT = (TTWO+CTA)/PWRP - CTA

c   consider the moist adiabat ew2 through ttwo at p. using the defini-
c   tion of the wobus function, it can be shown that etwo = ew2-thw.

      ETWO = PT + DWOBFSKEWT(TTWO) - DWOBFSKEWT(PT) - THW

c   dlt is the correction to be subtracted from ttwo.

      DLT = ETWO*RATE
      IF (ABS(DLT).GT.0.1D0) GO TO 10
      DSATLFTSKEWT = TTWO - DLT
      RETURN
      END
c ----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DPTLCLSKEWT(P,T,TD,PC,TC)
      DOUBLE PRECISION P
      DOUBLE PRECISION T
      DOUBLE PRECISION TD
      DOUBLE PRECISION PC
      DOUBLE PRECISION TC
      DOUBLE PRECISION AKAP
      DOUBLE PRECISION CTA
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2

C       NCL: ptlclSkewT (p:numeric, t:numeric,  td:numeric\
C                      , pc:numeric, tc:numeric)
C NCLEND

      DATA AKAP,CTA/0.28541D0,273.15D0/

      C1 = 4098.026D0/ (TD+237.3D0)**2
      C2 = 1.D0/ (AKAP* (T+CTA))
      PC = P*EXP(C1*C2* (T-TD)/ (C2-C1))
      TC = T + C1* (T-TD)/ (C2-C1)

      RETURN
      END
c ----------------------------------------------------------
C NCLFORTSTART
      DOUBLE PRECISION FUNCTION DSHOWALSKEWT(P,T,TD,NLVLS)
      DOUBLE PRECISION SHWLTR
      DOUBLE PRECISION FLAG

C NCL: showal = showalSkewT(p[*]:numeric, t[*]:numeric, td[*]:numeric)
c               nlvls = dimsizes(p)

      INTEGER NLVLS
      DOUBLE PRECISION P(NLVLS),T(NLVLS),TD(NLVLS)
C NCLEND
C
C Statement of purpose.
C ---------------------
C This routine computes the Showalter stability index for a sounding.
C ----------
C SHWLTR      Real          Showalter stability index (C).
C
C Internal variables.
C
      DOUBLE PRECISION P1,P2,P3,T850,TD850,T500,TP500
      DOUBLE PRECISION TMOIST,PRES
      INTEGER I
C
C External functions.
C
      DOUBLE PRECISION DINTERPSKEWT,DTSASKEWT,DEPTSKEWT
C
C Subroutine constants.
C
      PARAMETER (FLAG = -9999.D0)
C
C Initialize showalter index to flag.  Exit if surface pressure too low.
C
      SHWLTR = FLAG
      IF (P(1).LE.820.D0) GO TO 999
C
C Determine 850 mb temperature and dew point from sounding.  If the
C surface is above 900 mb, use 800 mb temp and dew point.
C
      IF (P(1).LT.900.D0) THEN
          PRES = 800.D0
      ELSE
          PRES = 850.D0
      END IF
      DO 1 I = 1,NLVLS
          IF (P(I).LE.PRES) THEN
              P1 = DLOG(P(I-1))
              P2 = DLOG(PRES)
              P3 = DLOG(P(I))
              T850 = DINTERPSKEWT(T(I-1),T(I),P1,P2,P3)
              TD850 = DINTERPSKEWT(TD(I-1),TD(I),P1,P2,P3)
              GO TO 2
          END IF
    1 CONTINUE
    2 CONTINUE
C
C Compute the moist adiabat (given by an equivalent potential
C temperature) through the 850 mb based LCL.
C
      TMOIST = DEPTSKEWT(T850,TD850,PRES)
C
C Compute the parcel temperature along this moist adiabat at 500 mb.
C
      TP500 = DTSASKEWT(TMOIST,500.D0)
C
C Determine 500 mb temperature from the sounding.
C
      PRES = 500.D0
      DO 3 I = 1,NLVLS
          IF (P(I).LE.PRES) THEN
              P1 = DLOG(P(I-1))
              P2 = DLOG(PRES)
              P3 = DLOG(P(I))
              T500 = DINTERPSKEWT(T(I-1),T(I),P1,P2,P3)
              GO TO 4
          END IF
    3 CONTINUE
    4 CONTINUE
C
C Compute the showalter index.
C
      SHWLTR = T500 - TP500
      DSHOWALSKEWT = SHWLTR

  999 CONTINUE
      RETURN
      END
c ---------------------------------------------------------------------
C NCLFORTSTART
      DOUBLE PRECISION FUNCTION DPWSKEWT(TD,P,N)
      DOUBLE PRECISION G
      DOUBLE PRECISION PW
      DOUBLE PRECISION WBOT
      DOUBLE PRECISION DWMRSKEWT
      DOUBLE PRECISION WTOP
      DOUBLE PRECISION W
      DOUBLE PRECISION WL
      DOUBLE PRECISION QL
      DOUBLE PRECISION DP
      INTEGER N
      DOUBLE PRECISION TD(N),P(N)

C       NCL: precpw = pwSkewT (td[*]:numeric, p[*]:numeric)
C            Note: n= dimsizes(p)
c NCLEND
c   this function computes total precipitable water precpw (cm) in a
c   vertical column of air based upon sounding data at n levels:
c          td = dew point (celsius)
c          p = pressure (millibars)
c   calculations are done in cgs units.

c   g = acceleration due to the earth's gravity (cm/s**2)
      DATA G/980.616D0/
c   initialize value of precipitable water
      PW = 0.D0
      NL = N - 1
c   calculate the mixing ratio at the lowest level.
      WBOT = DWMRSKEWT(P(1),TD(1))
      DO 5 I = 1,NL
          WTOP = DWMRSKEWT(P(I+1),TD(I+1))
c   calculate the layer-mean mixing ratio (g/kg).
          W = 0.5D0* (WTOP+WBOT)
c   make the mixing ratio dimensionless.
          WL = .001D0*W
c   calculate the specific humidity.
          QL = WL/ (WL+1.D0)
c   the factor of 1000. below converts from millibars to dynes/cm**2.
          DP = 1000.D0* (P(I)-P(I+1))
          PW = PW + (QL/G)*DP
          WBOT = WTOP
    5 CONTINUE
      DPWSKEWT = PW
      RETURN
      END

C -------------------------------------------------------
C NCLFORTSTART
      DOUBLE PRECISION FUNCTION DCAPETHERMO(PENV,TENV,NLVL,LCLMB,IPRNT,
     +                 TPARCEL,TMSG,JLCL,JLFC,JCROSS)
      DOUBLE PRECISION TNOT
      DOUBLE PRECISION CAPEVAL

      INTEGER NLVL,IPRNT
c                               ! mb [hPa]
      DOUBLE PRECISION PENV(NLVL)
c                               ! C
      DOUBLE PRECISION TENV(NLVL)
c                               ! lifting condensation lvl [mb]
      DOUBLE PRECISION LCLMB
c                               ! returned parcel temp [C]
      DOUBLE PRECISION TPARCEL(NLVL)
      DOUBLE PRECISION TMSG
c c c   real capethermo           ! J  [ignore here]

C NCL: cape = capeThermo(penv[*]:numeric, tenv[*]:numeric, lclmb:numeric\
C                       ,iprnt:integer)
C
C                   nlvl = dimsizes(penv)
C                tparcel = returned as vector attribute of cape
C                   jlcl = returned as attribute of cape
C                          (integer) subscript of lifting condensation
C                          level
C                   jlfc = returned as attribute of cape
C                          (integer) subscript of level of free
C                          convection
C                 jcross = returned as attribute of cape
C                          (integer) subscript where tparcel crosses env
C                          sounding
C NCLEND
C                               ! maximum number of sounding levels
      PARAMETER (NLMAX=5000)
C                               ! must match "tpar" in capedjs
      DOUBLE PRECISION TPAR(NLMAX)


      IER = 0
      IPLOT = 0
      TNOT = 273.15D0

      DO NL = 1,NLVL
          TPARCEL(NL) = TMSG
      END DO

      DO NL = 1,NLVL
          IF (TENV(NL).EQ.TMSG) THEN
              PRINT *,'fortran: capethermo: no missing values allowed'
              DCAPETHERMO = TMSG
              RETURN
          END IF
      END DO

      DO NL = 1,NLVL
          TENV(NL) = TENV(NL) + TNOT
      END DO

      DO N = 1,NLMAX
          TPAR(N) = TMSG
      END DO

      CALL DCAPEDJS(PENV,TENV,NLVL,LCLMB,IPLOT,IPRNT,CAPEVAL,TPAR,JLCL,
     +     JLFC,JCROSS,IER)

      DO NL = 1,NLVL
          TENV(NL) = TENV(NL) - TNOT
          TPARCEL(NL) = TPAR(NL) - TNOT
c          write (*,"(' capethermo: ',i5,4f10.2)") nl, penv(nl),tenv(nl)
c    *                          ,tparcel(nl)
c    *                          ,(tparcel(nl)-tenv(nl))
      END DO

      JLCL = JLCL - 1
      JLFC = JLFC - 1
      JCROSS = JCROSS - 1

      DCAPETHERMO = CAPEVAL
c       print *,"dcapethermo=",dcapethermo

      RETURN
      END
C -------------------------------------------------------
C ---> NO MORE FUNCTIONS FOR NCL ------------------------
C -------------------------------------------------------
      DOUBLE PRECISION FUNCTION DWOBFSKEWT(T)
      DOUBLE PRECISION T
      DOUBLE PRECISION X
      DOUBLE PRECISION POL
C called by satlftSkewT
      X = T - 20.D0
      IF (X.LE.0.D0) THEN
          POL = 1.D0 + X* (-8.8416605D-03+
     +          X* (1.4714143D-04+X* (-9.6719890D-07+X* (-3.2607217D-08+
     +          X* (-3.8598073D-10)))))
          DWOBFSKEWT = 15.130D0/POL**4
      ELSE
          POL = 1.D0 + X* (3.6182989D-03+
     +          X* (-1.3603273D-05+X* (4.9618922D-07+X* (-6.1059365D-09+
     +          X* (3.9401551D-11+X* (-1.2588129D-13+
     +          X* (1.6688280D-16)))))))
          DWOBFSKEWT = 29.930D0/POL**4 + 0.96D0*X - 14.8D0
      END IF

      RETURN
      END
c ----------------------------------------------------------
      DOUBLE PRECISION FUNCTION DINTERPSKEWT(Y1,Y3,X1,X2,X3)
      DOUBLE PRECISION Y1,Y3,X1,X2,X3

      DINTERPSKEWT = Y1 + ((Y3-Y1)* ((X2-X1)/ (X3-X1)))
      RETURN
      END
c ----------------------------------------------------------
      DOUBLE PRECISION FUNCTION DTSASKEWT(OS,P)
      DOUBLE PRECISION OS
      DOUBLE PRECISION P
      DOUBLE PRECISION B
      DOUBLE PRECISION A
      DOUBLE PRECISION TQ
      DOUBLE PRECISION D
      DOUBLE PRECISION TQK
      DOUBLE PRECISION X
      DOUBLE PRECISION DWXSKEWT

c   this function returns the temperature tsa (celsius) on a saturation
c   adiabat at pressure p (millibars). os is the equivalent potential
c   temperature of the parcel (celsius). sign(a,b) replaces the
c   algebraic sign of a with that of b.
c   b is an empirical constant approximately equal to 0.001 of the
c   latent heat of vaporization for water divided by the specific heat
c   at constant pressure for dry air.

      DATA B/2.6518986D0/

      A = OS + 273.15D0
c   tq is the first guess for tsa.
c   d is an initial value used in the iteration below.
      TQ = 253.15D0
      D = 120.D0

c   iterate to obtain sufficient accuracy....see table 1, p.8
c   of stipanuk (1973) for equation used in iteration.

      DO 1 I = 1,12
          TQK = TQ - 273.15D0
          D = D/2.D0
          X = A*EXP(-B*DWXSKEWT(TQK,P)/TQ) - TQ* ((1000.D0/P)**.286D0)
          IF (ABS(X).LT.1D-7) GO TO 2
          TQ = TQ + SIGN(D,X)
    1 CONTINUE
    2 DTSASKEWT = TQ - 273.15D0
      RETURN
      END

c ---------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DWXSKEWT(T,P)
      DOUBLE PRECISION T
      DOUBLE PRECISION P
      DOUBLE PRECISION X
      DOUBLE PRECISION DESATSKEWT

c  this function returns the mixing ratio (grams of water vapor per
c  kilogram of dry air) given the dew point (celsius) and pressure
c  (millibars). if the temperture  is input instead of the
c  dew point, then saturation mixing ratio (same units) is returned.
c  the formula is found in most meteorological texts.

      X = DESATSKEWT(T)
      DWXSKEWT = 622.D0*X/ (P-X)
      RETURN
      END
c ---------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DESATSKEWT(T)
      DOUBLE PRECISION T
      DOUBLE PRECISION TK
      DOUBLE PRECISION P1
      DOUBLE PRECISION P2
      DOUBLE PRECISION C1
      DOUBLE PRECISION ESAT

c   this function returns the saturation vapor pressure over
c   water (mb) given the temperature (celsius).
c   the algorithm is due to nordquist, w.s.,1973: "numerical approxima-
c   tions of selected meteorlolgical parameters for cloud physics prob-
c   lems," ecom-5475, atmospheric sciences laboratory, u.s. army
c   electronics command, white sands missile range, new mexico 88002.

      TK = T + 273.15D0
      P1 = 11.344D0 - 0.0303998D0*TK
      P2 = 3.49149D0 - 1302.8844D0/TK
      C1 = 23.832241D0 - 5.02808D0*DLOG10(TK)
      ESAT = 10.D0** (C1-1.3816D-7*10.D0**P1+8.1328D-3*10.D0**P2-
     +       2949.076D0/TK)
      DESATSKEWT = ESAT
      RETURN
      END
c -----------------------------------------
      DOUBLE PRECISION FUNCTION DEPTSKEWT(T,TD,P)
      DOUBLE PRECISION T
      DOUBLE PRECISION TD
      DOUBLE PRECISION P
      DOUBLE PRECISION W
      DOUBLE PRECISION DWMRSKEWT
      DOUBLE PRECISION TLCL
      DOUBLE PRECISION DTCONSKEWT
      DOUBLE PRECISION TK
      DOUBLE PRECISION TL
      DOUBLE PRECISION PT
      DOUBLE PRECISION EPTK

c   compute the mixing ratio (grams of water vapor per kilogram of
c   dry air).

      W = DWMRSKEWT(P,TD)

c   compute the temperature (celsius) at the lifting condensation level.

      TLCL = DTCONSKEWT(T,TD)
      TK = T + 273.15D0
      TL = TLCL + 273.15D0
      PT = TK* (1000.D0/P)** (0.2854D0* (1.D0-0.00028D0*W))
      EPTK = PT*EXP((3.376D0/TL-0.00254D0)*W* (1.D0+0.00081D0*W))
      DEPTSKEWT = EPTK - 273.15D0
      RETURN
      END
c -----------------------------------------
      DOUBLE PRECISION FUNCTION DWMRSKEWT(P,T)
      DOUBLE PRECISION P
      DOUBLE PRECISION T
      DOUBLE PRECISION EPS
      DOUBLE PRECISION X
      DOUBLE PRECISION WFW
      DOUBLE PRECISION FWESW
      DOUBLE PRECISION DESWSKEWT
      DOUBLE PRECISION R

c   this function approximates the mixing ratio wmr (grams of water
c   vapor per kilogram of dry air) given the pressure p (mb) and the
c   temperature t (celsius). the formula used is given on p. 302 of the
c   smithsonian meteorological tables by roland list (6th edition).

c   eps = ratio of the mean molecular weight of water (18.016 g/mole)
c         to that of dry air (28.966 g/mole)

      DATA EPS/0.62197D0/

c   the next two lines contain a formula by herman wobus for the
c   correction factor wfw for the departure of the mixture of air
c   and water vapor from the ideal gas law. the formula fits values
c   in table 89, p. 340 of the smithsonian meteorological tables,
c   but only for temperatures and pressures normally encountered in
c   in the atmosphere.

      X = 0.02D0* (T-12.5D0+7500.D0/P)
      WFW = 1.D0 + 4.5D-06*P + 1.4D-03*X*X
      FWESW = WFW*DESWSKEWT(T)
      R = EPS*FWESW/ (P-FWESW)

c   convert r from a dimensionless ratio to grams/kilogram.

      DWMRSKEWT = 1000.D0*R
      RETURN
      END
c -----------------------------------------
      DOUBLE PRECISION FUNCTION DESWSKEWT(T)
      DOUBLE PRECISION T
      DOUBLE PRECISION ES0
      DOUBLE PRECISION POL

c   this function returns the saturation vapor pressure esw (millibars)
c   over liquid water given the temperature t (celsius). the polynomial
c   approximation below is due to herman wobus, a mathematician who
c   worked at the navy weather research facility, norfolk, virginia,
c   but who is now retired. the coefficients of the polynomial were
c   chosen to fit the values in table 94 on pp. 351-353 of the smith-
c   sonian meteorological tables by roland list (6th edition). the
c   approximation is valid for -50 < t < 100c.
c
c   es0 = saturation vapor ressure over liquid water at 0c

      DATA ES0/6.1078D0/

      POL = 0.99999683D0 + T* (-0.90826951D-02+
     +      T* (0.78736169D-04+T* (-0.61117958D-06+T* (0.43884187D-08+
     +      T* (-0.29883885D-10+T* (0.21874425D-12+T* (-0.17892321D-14+
     +      T* (0.11112018D-16+T* (-0.30994571D-19)))))))))
      DESWSKEWT = ES0/POL**8
      RETURN
      END
c ---------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DTCONSKEWT(T,D)
      DOUBLE PRECISION T
      DOUBLE PRECISION D
      DOUBLE PRECISION S
      DOUBLE PRECISION DLT
C       NCL: tcon = tconSkewT (t:numeric, d:numeric)

c   this function returns the temperature tcon (celsius) at the lifting
c   condensation level, given the temperature t (celsius) and the
c   dew point d (celsius).
c
c   compute the dew point depression s.
      S = T - D
c   the approximation below, a third order polynomial in s and t,
c   is due to herman wobus. the source of data for fitting the
c   polynomial is unknown.

      DLT = S* (1.2185D0+1.278D-03*T+S*
     +      (-2.19D-03+1.173D-05*S-5.2D-06*T))
      DTCONSKEWT = T - DLT
      RETURN
      END
c ---------------------------------------------------------------------
c ------capedjs not called directly by NCL--------------------------
cNAME
c     cape - calculates CAPE, given temperature/pressure
c              data from a vertical sounding
c
cSYNOPSIS
c  call capedjs(penv,tenv,nlvl,lclmb,iplot,iprnt,capeval,ier) (old)
c  call capedjs(penv,tenv,nlvl,lclmb,iplot,iprnt,capeval,tparcel,ier)
c           (change for NCL)
c
cDESCRIPTION
c     cape will compute the convective available potential energy
c     given vertical soundings of temperature and pressure.
c     optional plots of environmental temperature, parcel
c     temperature, and cape as a function of altitude are available
c
c     definition of CAPE: the integral wrt -ln(p/p0) of
c             R*(Tparcel-Tenv)   from the Level of Free Convection
c             'up' to the 'crossing level'.
c
cPARAMETERS
c     Parameter Type    Description
c
c     penv              (i)     environmental pressure sounding array in mb;
c                             the pressure array is assumed to be
c                             loaded in a monotonically decreasing
c                             sequence.
c
c     tenv              (i)     temperature sounding array in Kelvin at
c                             the pressure levels
c
c     nlvl              (i)     number of sounding levels
c
c     lclmb             (i)     approximate pressure at lifting condensation
c                   level (mb). the pressure ACTUALLY USED will
c                   be the first value on the sounding grid greater
c                   than or equal to lclmb.
c
c     iplot             (i)     plotting flag: plots (=1); no plots (=other).
c                             the parcel and environmental temperature
c                             profiles are plotted throughout the
c                             sounding depth; the CAPE profile is
c                             plotted only between the LFC and the 
c                             crossing level.
c
c     iprnt             (i)     printing flag: print (>0); no print (=0). DJS
c
c     capeval           (o)     CAPE at the 'crossing level',
c                             defined to be the height where the
c                             parcel temp has decreased enough to
c                             again equal the environmental temp.
c                             if the Level of Free Convection DOES NOT
c                             exist (so that CAPE will be zero or
c                             negative) capeval will be assigned ZERO.
c                             if the Level of Free Convection DOES exist
c                             but the 'crossing level' DOES NOT exist,
c                             capeval will be assigned the value of
c                             CAPE at the "top" of the
c                             sounding (i.e. smallest pressure)
c
c     tparcel           (o)     temperature of parcel
c
c     ier               (o)     error status:
c                             zero on successful return;
c                             minus one on error;
c                             minus two if 'crossing level' does not
c                             exist;
cTECHNICAL NOTES
c     The parcel used to compute CAPE is assumed to initially lie at the
c     level of the largest pressure value in the sounding.
c
cGENERAL NOTES
c     No input parameters are modified by this routine.
c
c     No error checking for missing data is done in this routine.
c     It is the user's responsibility to ensure that no missing data
c     is present.
c
c       Opening and closing NCAR graphics (via opgks and clsgks) must be
c       performed in the calling program, if iplot = 1
cAUTHOR
c     GLF 9/95

      SUBROUTINE DCAPEDJS(PENV,TENV,NLVL,LCLMB,IPLOT,IPRNT,CAPEVAL,
     +                    TPARCEL,JLCL,JLFC,JCROSS,IER)

      IMPLICIT NONE
      INTEGER NLVL,IPLOT,IPRNT,IER,IFLAG
      INTEGER NLMAX,II,JLCL,JLCL1,NDIM,JSTRT,JCROSS,JLFC
      DOUBLE PRECISION PENV(NLVL),TENV(NLVL),LCLMB,CAPEVAL
      DOUBLE PRECISION CENTKEL,G,CP,RD,PLCL,TLCL,FRET,PMB,MDLNP
      DOUBLE PRECISION GAMMADRY,PPAR0,TPAR0,PARCELMB,PELFC

C                ! special for NCL return [dimensioned NLMAX]
      DOUBLE PRECISION TPARCEL(*)

C                               max number of sounding levels
c                               see also cape routine
      PARAMETER (NLMAX=5000)
      PARAMETER (CENTKEL=273.15D0)
      PARAMETER (G=9.81D0)
      PARAMETER (CP=1004.D0)

      DOUBLE PRECISION ZLEV,TPAR,PE,TEMPENV,ZZLEV,PPE
      DIMENSION ZLEV(NLMAX),TPAR(NLMAX),PE(NLMAX),TEMPENV(NLMAX)
      DIMENSION ZZLEV(NLMAX),PPE(NLMAX)
      DOUBLE PRECISION DTMLAPSKEWT

c     arrays and equivalences for NCAR graphics   [ NOT USED]
      DOUBLE PRECISION WORK,XI,THE
      DIMENSION WORK(NLMAX,2),XI(5),THE(5)
      EQUIVALENCE (WORK(1,1),TEMPENV(1)), (WORK(1,2),TPAR(1))

      RD = 287.D0
      IF (NLVL.GT.NLMAX) THEN
          PRINT *,'cape.f: too many sounding levels'
          IER = -1
          RETURN
      END IF

c     determine approximate location of lcl

      JLCL = 1
      DO II = 2,NLVL
          IF (LCLMB.LE.PENV(II)) THEN
              JLCL = II
          END IF
      END DO
      IF (IPRNT.GT.0) THEN
          PRINT *,'cape.f: LCL pressure: original ',LCLMB,' mb'
          PRINT *,'cape.f: adjusted to ',PENV(JLCL),' mb'
          PRINT *,'cape.f: and lies at level ',JLCL
          DO II = 1,NLVL
              PRINT *,' data=',PENV(II),TENV(II)
          END DO
      END IF
      PLCL = PENV(JLCL)

c     to compute cape, the parcel is assumed to lie initially at the
c     level of largest pressure

      JSTRT = 1
      PARCELMB = PENV(JSTRT)
      IF (IPRNT.GT.0) THEN
          PRINT *,'cape.f: initial parcel pressure: ',PENV(JSTRT),' mb'
          PRINT *,'cape.f: parcel temp is ',TENV(JSTRT),' K'
          PRINT *,'cape.f: initial P and T lie at level ',JSTRT
      END IF
      PPAR0 = PENV(JSTRT)
      TPAR0 = TENV(JSTRT)

c     assign log-pressure levels for input data,
c     referenced to parcel's initial pressure.

      DO II = 1,NLVL
          ZLEV(II) = -DLOG(PENV(II)/PPAR0)
      END DO

c  compute parcel temperature at dry lapse rate until the LCL is
c  reached
      GAMMADRY = G/CP
      DO II = 1,JLCL
          TPAR(II) = TPAR0*EXP((RD*GAMMADRY/G)* (-ZLEV(II)))
      END DO
      JLCL1 = JLCL + 1

c     above LCL compute temperature of saturated parcel

C                                   convert to C
      TLCL = TPAR(JLCL) - CENTKEL
      THE(1) = 62.D0
      XI(1) = 1.D0
      NDIM = 1

c     linmin determines theta_e


      FRET = 0.D0
      CALL DLINMIN(THE,XI,NDIM,FRET,PLCL,TLCL)

      DO II = JLCL1,NLVL
          PMB = PPAR0*EXP(-ZLEV(II))

c        tmlapskewt computes temperature given theta_e and pressure

          TPAR(II) = CENTKEL + DTMLAPSKEWT(THE(1),PMB)
      END DO
      IF (IPRNT.GT.0) THEN
          PRINT *,' '
          WRITE (*,FMT=
     +'(''cape.f:'',16x,''P'',6x,''zlev''
     +                 , 6x,''tpar'',6x,''tenv'')')
          DO II = 1,NLVL
              WRITE (*,FMT='(i4,7x,4f10.2)') II,PENV(II),ZLEV(II),
     +          TPAR(II),TENV(II)
          END DO
          PRINT *,' '
      END IF

c     determine approximate location of 1) the upper limit of
c     CAPE integration (the so-called 'crossing level')
c     and 2) the Level of Free Convection

      IF (TPAR(NLVL).GT.TENV(NLVL)) THEN
C                                       ! no  crossinglevel exists
          JCROSS = NLVL
C                                       ! so assign one
          IER = -2
C                                       ! now locate LFC
          IFLAG = 0
          DO II = NLVL - 1,1,-1
              IF ((TPAR(II).EQ.TENV(II)) .AND. (IFLAG.EQ.0)) THEN
                  JLFC = II
                  IFLAG = 1
              END IF
              IF ((TPAR(II).LT.TENV(II)) .AND. (IFLAG.EQ.0)) THEN
                  JLFC = II + 1
                  IFLAG = 1
              END IF
          END DO
          IF (IFLAG.EQ.0) THEN
              JLFC = 1
          END IF
      ELSE
          IFLAG = 0
          DO II = NLVL,1,-1
              IF ((TPAR(II).GE.TENV(II)) .AND. (IFLAG.EQ.0)) THEN
                  JCROSS = II
                  IFLAG = 1
              END IF
          END DO
          IF ((IFLAG.EQ.0) .OR. (JCROSS.EQ.1)) THEN
              CAPEVAL = 0.0D0
              IER = 0
              RETURN
          END IF
          IFLAG = 0
          DO II = JCROSS - 1,1,-1
              IF ((TPAR(II).EQ.TENV(II)) .AND. (IFLAG.EQ.0)) THEN
                  JLFC = II
                  IFLAG = 1
              END IF
              IF ((TPAR(II).LT.TENV(II)) .AND. (IFLAG.EQ.0)) THEN
                  JLFC = II + 1
                  IFLAG = 1
              END IF
          END DO
          IF (IFLAG.EQ.0) THEN
              JLFC = 1
          END IF
      END IF
      IF (IPRNT.GT.0) THEN
          PRINT *,'LFC pressure= ',PENV(JLFC),' Crossing pressure= ',
     +      PENV(JCROSS)
      END IF

c     determine CAPE profile

      PE(1) = 0.0D0
      DO II = 2,JCROSS
          MDLNP = ZLEV(II) - ZLEV(II-1)
          PE(II) = PE(II-1) + (RD*MDLNP*
     +             ((TPAR(II)+TPAR(II-1))- (TENV(II)+TENV(II-1)))/2.D0)
      END DO
      PELFC = PE(JLFC)
      DO II = 1,JCROSS
          PE(II) = PE(II) - PELFC
      END DO

c     save CAPE computed by integrating from LFC to crossing level

      CAPEVAL = PE(JCROSS)


      DO II = JLFC,JCROSS
          ZZLEV(II+1-JLFC) = ZLEV(II)
          PPE(II+1-JLFC) = PE(II)
      END DO
      DO II = 1,NLVL
          TPARCEL(II) = TPAR(II)
      END DO

      IF (IPRNT.GT.0) THEN
          WRITE (*,FMT='(/,'' jlfc, jcross='',2i5)') JLFC,JCROSS
          WRITE (*,FMT='(/,''Env Temp & Parcel Temp'',/)')
          DO II = 1,JCROSS
              WRITE (*,FMT='(i4,f7.1,4f9.2,f6.2)') II,PENV(II),PE(II),
     +          ZLEV(II), (TEMPENV(II)-273.15D0), (TPAR(II)-273.15D0),
     +          (TPAR(II)-TEMPENV(II))
c              write (*,"(i4,f7.1,4f9.2,f6.2)") ii
c    *                           ,penv(ii),pe(ii),zlev(ii)
c    *                           ,(work(ii,1)-273.15)
c    *                           ,(work(ii,2)-273.15)
c    *                           ,(work(ii,2)-work(ii,1))
          END DO
          WRITE (*,FMT='(  ''----------------------'',/)')
      END IF

      RETURN
      END
c ----------Not called by NCL directly--------------------------
      SUBROUTINE DLINMIN(P,XI,N,FRET,PLCL,TLCL)
      DOUBLE PRECISION P
      DOUBLE PRECISION XI
      DOUBLE PRECISION FRET
      DOUBLE PRECISION PLCL
      DOUBLE PRECISION TLCL
      DOUBLE PRECISION TOL
      DOUBLE PRECISION PCOM
      DOUBLE PRECISION XICOM
      DOUBLE PRECISION PLCLX
      DOUBLE PRECISION TLCLX
      DOUBLE PRECISION AX
      DOUBLE PRECISION XX
      DOUBLE PRECISION BX
      DOUBLE PRECISION FA
      DOUBLE PRECISION FX
      DOUBLE PRECISION FB
      DOUBLE PRECISION DBRENT
      DOUBLE PRECISION XMIN
c
c  LINMIN: given the n-dim point P and n-dim direction XI, move and
c      reset P to whee the function 'dfd' takes on a minimum along the
c      direction XI from P, and replaces XI by the actual vector
c      displacement that P was moved. Also returns as FRET the value
c      of the function at P.
c=================================================================
      SAVE
      PARAMETER (NMAX=50,TOL=1.D-5)
      COMMON /F1COM/PCOM(NMAX),XICOM(NMAX),PLCLX,TLCLX,NCOM
      DIMENSION P(N),XI(N)
      DOUBLE PRECISION DFUNCSHEA
      EXTERNAL DFUNCSHEA
c
c  set common block
c---------------------
      PLCLX = PLCL
      TLCLX = TLCL
      NCOM = N
      DO 100 J = 1,N
          PCOM(J) = P(J)
          XICOM(J) = XI(J)
  100 CONTINUE
c
c  find minimum point
c---------------------
      AX = 0.0D0
      XX = 1.0D0
      BX = 2.0D0
      CALL DMNBRAK(AX,XX,BX,FA,FX,FB,DFUNCSHEA)
      FRET = DBRENT(AX,XX,BX,DFUNCSHEA,TOL,XMIN)
c
c  construct vector results
c---------------------------
      DO 200 J = 1,N
          XI(J) = XMIN*XI(J)
          P(J) = P(J) + XI(J)
  200 CONTINUE
c
c  end routine
c---------------
      RETURN
      END
c ----------Not called by NCL directly--------------------------
      SUBROUTINE DMNBRAK(AX,BX,CX,FA,FB,FC,DFUNCSHEA)
      DOUBLE PRECISION AX
      DOUBLE PRECISION BX
      DOUBLE PRECISION CX
      DOUBLE PRECISION FA
      DOUBLE PRECISION FB
      DOUBLE PRECISION FC
      DOUBLE PRECISION DFUNCSHEA
      DOUBLE PRECISION GOLD
      DOUBLE PRECISION GLIMIT
      DOUBLE PRECISION TINY
      DOUBLE PRECISION DUM
      DOUBLE PRECISION R
      DOUBLE PRECISION Q
      DOUBLE PRECISION U
      DOUBLE PRECISION ULIM
      DOUBLE PRECISION FU
c
c  MNBRAK: given the subroutine 'func' which returns a function
c     value, and given distinct initail points AX and BX, this routine
c     searches in the downhill direction (defined by the function as
c     evaluated at the intial points) and returns new points ZX, BX, CX
c     which bracket a minimum of the funtion. Also returned are the
c     function values at the three points, FA, FB, FC.
c===================================================================
      SAVE
      parameter(GOLD=1.618034d0,GLIMIT=100.d0,TINY=1.d-20)
cc    parameter(GOLD=1.6180340,GLIMIT=100.0,TINY=1.e-7)
c      PARAMETER (GOLD=1.6180340D0,GLIMIT=100.0D0,TINY=1.D-5)
      EXTERNAL DFUNCSHEA
c
c  get intial function values switch roles of A and B if needed.
c------------------------------------------------------------------
      FA = DFUNCSHEA(AX)
      FB = DFUNCSHEA(BX)
      IF (FB.GT.FA) THEN
          DUM = AX
          AX = BX
          BX = DUM
          DUM = FB
          FB = FA
          FA = DUM
      END IF
c
c  first guess
c-------------
      CX = BX + GOLD* (BX-AX)
      FC = DFUNCSHEA(CX)
c
c  loop point for bracketing
c=============================
  100 CONTINUE
      IF (FB.GE.FC) THEN
          R = (BX-AX)* (FB-FC)
          Q = (BX-CX)* (FB-FA)
          U = BX - ((BX-CX)*Q- (BX-AX)*R)/
     +        (2.0D0*SIGN(MAX(ABS(Q-R),TINY),Q-R))
          ULIM = BX + GLIMIT* (CX-BX)
          IF ((BX-U)* (U-CX).GT.0.0D0) THEN
              FU = DFUNCSHEA(U)
              IF (FU.LT.FC) THEN
                  AX = BX
                  FA = FB
                  BX = U
                  FB = FU
                  GO TO 100
              ELSE IF (FU.GT.FB) THEN
                  CX = U
                  FC = FU
                  GO TO 100
              END IF
              U = CX + GOLD* (CX-BX)
              FU = DFUNCSHEA(U)
          ELSE IF ((CX-U)* (U-ULIM).GT.0.0D0) THEN
              FU = DFUNCSHEA(U)
              IF (FU.LT.FC) THEN
                  BX = CX
                  CX = U
                  U = CX + GOLD* (CX-BX)
                  FB = FC
                  FC = FU
                  FU = DFUNCSHEA(U)
              END IF
          ELSE IF ((U-ULIM)* (ULIM-CX).GE.0.0D0) THEN
              U = ULIM
              FU = DFUNCSHEA(U)
          ELSE
              U = CX + GOLD* (CX-BX)
              FU = DFUNCSHEA(U)
          END IF
          AX = BX
          BX = CX
          CX = U
          FA = FB
          FB = FC
          FC = FU
          GO TO 100
      END IF
c
c  end routine
c--------------
      RETURN
      END
c ----------Not called by NCL directly--------------------------
      FUNCTION DBRENT(AX,BX,CX,DFUNCSHEA,TOL,XMIN)
      DOUBLE PRECISION DBRENT
      DOUBLE PRECISION AX
      DOUBLE PRECISION BX
      DOUBLE PRECISION CX
      DOUBLE PRECISION DFUNCSHEA
      DOUBLE PRECISION TOL
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION CGOLD
      DOUBLE PRECISION ZEPS
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION X
      DOUBLE PRECISION E
      DOUBLE PRECISION FX
      DOUBLE PRECISION FV
      DOUBLE PRECISION FW
      DOUBLE PRECISION XM
      DOUBLE PRECISION TOL1
      DOUBLE PRECISION TOL2
      DOUBLE PRECISION R
      DOUBLE PRECISION Q
      DOUBLE PRECISION P
      DOUBLE PRECISION ETEMP
      DOUBLE PRECISION D
      DOUBLE PRECISION U
      DOUBLE PRECISION FU
c
c DBRENT: given a subroutine 'func' which returns a function value,
c    and given a bracketing triplet of abscissas AX, BX, CX (such that
c    BX is between AX and CX, and F(BX) is less than both F(AX) and
c    F(CX) ), this routine isolates the minimum to a frational precision
c    of about TOL using brent's method.
c    the abscissa of the minimum is returned as XMIN, and the minimum
c    function value is returned as BRENT.
c======================================================================
      SAVE
      parameter(ITMAX=100,CGOLD=.3819660,ZEPS=1.e-10)
ccc      PARAMETER (ITMAX=100,CGOLD=.3819660D0,ZEPS=1.D-3)
      EXTERNAL DFUNCSHEA
c
c  set initial values
c-------------------------
      A = MIN(AX,CX)
      B = MAX(AX,CX)
cc    print *,' ax=',ax,' bx=',bx,' cx=',cx,' a=',a,' b=',b
      V = BX
      W = V
      X = V
      E = 0.0D0
      FX = DFUNCSHEA(X)
      FV = FX
      FW = FX
c
c  loop over the maximum number of iterations
c================================================
      DO 300 ITER = 1,ITMAX
          XM = 0.50D0* (A+B)
          TOL1 = TOL*ABS(X) + ZEPS
          TOL2 = 2.0D0*TOL1
          IF (ABS(X-XM).LE. (TOL2-.5D0* (B-A))) GO TO 400
cc    print *,'|x-xm|=',abs(x-xm),' t2-(b-a)/2=',(tol2-.5e0*(b-a))
          IF (ABS(E).GT.TOL1) THEN
              R = (X-W)* (FX-FV)
              Q = (X-V)* (FX-FW)
              P = (X-V)*Q - (X-W)*R
              Q = 2.D0* (Q-R)
              IF (Q.GT.0.D0) P = -P
              Q = ABS(Q)
              ETEMP = E
              E = D

              IF ((ABS(P).GT.ABS(0.5D0*Q*ETEMP)) .OR.
     +            (P.LE.Q* (A-X)) .OR. (P.GE.Q* (B-X))) GO TO 100
              D = P/Q
              U = X + D
              IF (((U-A).LT.TOL2) .OR. ((B-U).LT.TOL2)) D = SIGN(TOL1,
     +            XM-X)
              GO TO 200
          END IF
  100     CONTINUE
          IF (X.GE.XM) THEN
              E = A - X
          ELSE
              E = B - X
          END IF
          D = CGOLD*E
  200     CONTINUE
          IF (ABS(D).GE.TOL1) THEN
              U = X + D
          ELSE
              U = X + SIGN(TOL1,D)
          END IF
          FU = DFUNCSHEA(U)
          IF (FU.LE.FX) THEN
              IF (U.GE.X) THEN
                  A = X
              ELSE
                  B = X
              END IF
              V = W
              FV = FW
              W = X
              FW = FX
              X = U
              FX = FU
          ELSE
              IF (U.LT.X) THEN
                  A = U
              ELSE
                  B = U
              END IF
              IF ((FU.LE.FW) .OR. (W.EQ.X)) THEN
                  V = W
                  FV = FW
                  W = U
                  FW = FU
              ELSE IF ((FU.LE.FV) .OR. (V.EQ.X) .OR. (V.EQ.W)) THEN
                  V = U
                  FV = FU
              END IF
          END IF
  300 CONTINUE
      PRINT *,' DBRENT ERROR STOP: Maximum Number of Iterations'
      PRINT *,'      reached, ITMAX=',ITMAX
      STOP
c
c  end routine
c-----------------
  400 CONTINUE
      XMIN = X
      DBRENT = FX
      RETURN
      END
c ----------Not called by NCL directly--------------------------
      DOUBLE PRECISION FUNCTION DFUNCSHEA(X)
      DOUBLE PRECISION X
      DOUBLE PRECISION P
      DOUBLE PRECISION XI
      DOUBLE PRECISION PLCL
      DOUBLE PRECISION TLCL
      DOUBLE PRECISION XT
      DOUBLE PRECISION DTMLAPSKEWT
c
      SAVE
      PARAMETER (NMAX=50)
      COMMON /F1COM/P(NMAX),XI(NMAX),PLCL,TLCL,NCOM
      DIMENSION XT(NMAX)
c
      DO 100 J = 1,NCOM
          XT(J) = P(J) + (X*XI(J))
  100 CONTINUE
c
c  end routine
c--------------
      DFUNCSHEA = ABS(TLCL-DTMLAPSKEWT(XT,PLCL))
      RETURN
      END
c ---------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DTMLAPSKEWT(THETAE,P)
      DOUBLE PRECISION THETAE
      DOUBLE PRECISION P
      DOUBLE PRECISION CRIT
      DOUBLE PRECISION EQ0
      DOUBLE PRECISION TLEV
      DOUBLE PRECISION EQ1
      DOUBLE PRECISION DEPTSKEWT
      DOUBLE PRECISION DIF
      DOUBLE PRECISION DT
c c c   real FUNCTION DtmlapsSkewT(thetae,p)

c   this function returns the temperature tmlapskewt (celsius) at
c   pressure p (millibars) along the moist adiabat corresponding
c   to an equivalent potential temperature thetae (celsius).
c   the algorithm was written by eric smith at colorado state
c   university.

      DATA CRIT/0.1D0/
c   cta = difference between kelvin and celsius temperatures.
c   crit = convergence criterion (degrees kelvin)

      EQ0 = THETAE

c   initial guess for solution

      TLEV = 25.D0

c   compute the saturation equivalent potential temperature correspon-
c   ding to temperature tlev and pressure p.

      EQ1 = DEPTSKEWT(TLEV,TLEV,P)
      DIF = ABS(EQ1-EQ0)
      IF (DIF.LT.CRIT) GO TO 3
      IF (EQ1.GT.EQ0) GO TO 1

c   dt is the initial stepping increment.

      DT = 10.D0
      I = -1
      GO TO 2
    1 DT = -10.D0
      I = 1
    2 TLEV = TLEV + DT
      EQ1 = DEPTSKEWT(TLEV,TLEV,P)
      DIF = ABS(EQ1-EQ0)
      IF (DIF.LT.CRIT) GO TO 3
      J = -1
      IF (EQ1.GT.EQ0) J = 1
      IF (I.EQ.J) GO TO 2

c   the solution has been passed. reverse the direction of search
c   and decrease the stepping increment.

      TLEV = TLEV - DT
      DT = DT/10.D0
      GO TO 2
    3 DTMLAPSKEWT = TLEV
      RETURN
      END
