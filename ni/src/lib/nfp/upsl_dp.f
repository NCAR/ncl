C*PL*ERROR* Comment line too long
c -----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DPSLHY1(PRES,Z,TV,XMSG)
      IMPLICIT NONE

c Compute sea level pressure (Pa) via the hypsometric eqn

c NCL: slp = pslhy1 (pres,z,tv)

c input

c pressure on lowest vertical level (Pa)
      DOUBLE PRECISION PRES
c Geopotential hgt of lowest vertical level (m)
      DOUBLE PRECISION Z
c virtual temperature (K)
      DOUBLE PRECISION TV
c missing value
      DOUBLE PRECISION XMSG

c output

c sea level pressure (Pa)
c     real pslhy1

c local

c gravity (m/s2)
      DOUBLE PRECISION G
c gas const dry air (j/{kg-k})
      DOUBLE PRECISION RDAIR
      DATA G/9.80665D0/
      DATA RDAIR/287.04D0/

c pres_msg, zmsg, tvmsg
      IF (PRES.NE.XMSG .AND. Z.NE.XMSG .AND. TV.NE.XMSG) THEN
          DPSLHY1 = PRES*EXP(G*Z/ (RDAIR*TV))
      ELSE
c pres_msg
          DPSLHY1 = XMSG
      END IF

      RETURN
      END
C*PL*ERROR* Comment line too long
c -----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DPSLHY2(PRES,Z,T,W,XMSG)
      IMPLICIT NONE

c Compute sea level pressure (Pa) via the hypsometric eqn

c NCL: slp = pslhy2 (pres,z,t,w)

c input

c pressure on lowest vertical level (Pa)
      DOUBLE PRECISION PRES
c Geopotential hgt of lowest vertical level (m)
      DOUBLE PRECISION Z
c temperature (K)
      DOUBLE PRECISION T
c mixing ratio (kg/kg) [specific hum couls also be used]
      DOUBLE PRECISION W
c missing value
      DOUBLE PRECISION XMSG

c output

c sea level pressure (Pa)
c     real pslhy2

c local

c virtual temperature (K)
      DOUBLE PRECISION TV
c gravity (m/s2)
      DOUBLE PRECISION G
c gas const dry air (j/{kg-k})
      DOUBLE PRECISION RDAIR
      DATA G/9.80665D0/
      DATA RDAIR/287.04D0/

c pres_msg, zmsg, tvmsg, wmsg
      IF (PRES.NE.XMSG .AND. Z.NE.XMSG .AND. T.NE.XMSG .AND.
     +    W.NE.XMSG) THEN
          TV = T* (1.D0+0.608D0*W)
          DPSLHY2 = PRES*EXP(G*Z/ (RDAIR*TV))
      ELSE
c pres_msg
          DPSLHY2 = XMSG
      END IF

      RETURN
      END
C*PL*ERROR* Comment line too long
c ----------------------------------------------------------------------------
      SUBROUTINE DPSLEC(T,PHIS,PS,PRES,MLON,NLAT,PSL)
      IMPLICIT NONE

c Compute sea level pressure using the ECMWF formulation.
c Uses same routine as CCM Processor's derived field: PSLECMWF

c NCL: slp = pslec (t,phis,ps,pres)
c input

c Number of longitude values
      INTEGER MLON
c Number of latitude values
      INTEGER NLAT

c Temperature of lowest vertical level (K)
      DOUBLE PRECISION T(MLON,NLAT)
c Surface geopotential (m^2/s^2)
      DOUBLE PRECISION PHIS(MLON,NLAT)
c Surface pressure (Pascals)
      DOUBLE PRECISION PS(MLON,NLAT)
c Pressure on lowest vertical level (Pa)
      DOUBLE PRECISION PRES(MLON,NLAT)

c output

c Sea Level Pressure (Pa)
      DOUBLE PRECISION PSL(MLON,NLAT)

c local

c latitude counter
      INTEGER NL


C*PL*ERROR* Comment line too long
c The CCM Processor routine cpslec processes one latitude slice at a time.

      DO NL = 1,NLAT
          CALL DCPSLEC(PRES(1,NL),PHIS(1,NL),PS(1,NL),T(1,NL),MLON,MLON,
     +                 1,PSL(1,NL))
      END DO

      RETURN
      END
C*PL*ERROR* Comment line too long
c ----------------------------------------------------------------------------
      SUBROUTINE DPSLHOR(Z,T,PHIS,PS,PRES,LATS,MLON,NLAT,KLEV,PSL,PSLU,
     +                   ZX,TX,PRESX)
      IMPLICIT NONE

c Compute sea level pressure (Pa)  using the ECMWF formulation and
c     Trenberth's horizontal correction.

c Uses same routine as CCM Processor's derived field: PSLHOR2.

c NCL: slp = pslhor (z,t,phis,ps,pres,lats)

c input

c Number of longitude values
      INTEGER MLON
c Number of latitude values
      INTEGER NLAT
c Number of vertical levels
      INTEGER KLEV

c Geopotential Height Units: meters
      DOUBLE PRECISION Z(MLON,NLAT,KLEV)
c Temperature (K)
      DOUBLE PRECISION T(MLON,NLAT,KLEV)
c Surface geopotential (m^2/s^2)
      DOUBLE PRECISION PHIS(MLON,NLAT)
c Surface pressure (Pa)
      DOUBLE PRECISION PS(MLON,NLAT)
c Pressure (Pa)
      DOUBLE PRECISION PRES(MLON,NLAT,KLEV)
c Latitudes (degrees)
      DOUBLE PRECISION LATS(NLAT)

c output

c Sea Level Pressure (Pa)
      DOUBLE PRECISION PSL(MLON,NLAT)

c local (arrays auto allocation)

c loop counters
      INTEGER ML,NL,KL
c psl from PSLEC, uncorrected
      DOUBLE PRECISION PSLU(MLON,NLAT)
c temp
      DOUBLE PRECISION ZX(MLON,KLEV)
c temp
      DOUBLE PRECISION TX(MLON,KLEV)
c temp
      DOUBLE PRECISION PRESX(MLON,KLEV)


C*PL*ERROR* Comment line too long
c The CCM Processor routine cpslec processes one latitude slice at a time.

      KL = 1
      DO NL = 1,NLAT
          CALL DCPSLEC(PRES(1,NL,KL),PHIS(1,NL),PS(1,NL),T(1,NL,KL),
     +                 MLON,MLON,1,PSLU(1,NL))
      END DO

c Do the horizontal correction
C*PL*ERROR* Comment line too long
c The CCM Processor routine cpslhor2 processes one latitude slice at a time
c   and expects data from top to bottom.

      DO NL = 1,NLAT

          DO KL = 1,KLEV
              DO ML = 1,MLON
c must be in reverse order
                  ZX(ML,KL) = Z(ML,NL,KLEV+1-KL)
c must be in reverse order
                  TX(ML,KL) = T(ML,NL,KLEV+1-KL)
c must be in reverse order
                  PRESX(ML,KL) = PRES(ML,NL,KLEV+1-KL)
              END DO
          END DO

          CALL DCPSLHOR2(PRESX,ZX,PHIS(1,NL),PSLU(1,NL),PS(1,NL),TX,
     +                   MLON,MLON,KLEV,LATS(NL),PSL(1,NL))
      END DO

      RETURN
      END
C*PL*ERROR* Comment line too long
c ----------------------------------------------------------------------------
      SUBROUTINE DCPSLEC(PRES,PHIS,PS,T,IDIM,IMAX,KMAX,PSL)
      DOUBLE PRECISION RD
      DOUBLE PRECISION G
      DOUBLE PRECISION XLAPSE
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION TSTAR
      DOUBLE PRECISION T0
      DOUBLE PRECISION ALPH
      DOUBLE PRECISION BETA
c
c       File:   /crestone/u0/ccmproc2/src/src/cpslec.F  (2312 bytes)
c       Date:   Wed Dec  1 16:07:19 1993
c       Author: Lawrence Buja <ccmproc2@sunny>
c
c       From:   inderf
c       Calls:  ** none **
c       Blocks: ** none **
c
c-----------------------------------------------------------------------
c
c       CCM2 hybrid coord version using ECMWF formulation
c       Purpose:   Compute sea level pressure for a latitude line
c       Algorithm: See section 3.1.b in NCAR NT-396 "Vertical
c                  Interpolation and Truncation of Model-Coordinate Data
c
c-----------------------------Arguments---------------------------------
c
c Input
c Atmospheric pressure (pascals)
      DOUBLE PRECISION PRES(IDIM,KMAX)
c Surface geopotential (m**2/sec**2)
      DOUBLE PRECISION PHIS(IDIM)
c Surface pressure (pascals)
      DOUBLE PRECISION PS(IDIM)
c Vertical slice of temperature (top to bot)
      DOUBLE PRECISION T(IDIM,KMAX)
c First dimension of phis, ps, and t
      INTEGER IDIM
c Number of longitude points to compute
      INTEGER IMAX
c Number of levels in t (2nd dimension)
      INTEGER KMAX
c
c Output
c
c Sea level pressures (pascals)
      DOUBLE PRECISION PSL(IDIM)
c
c-----------------------------Parameters--------------------------------
c
      PARAMETER (RD=287.04D0,G=9.80616D0,XLAPSE=6.5D-3)
      PARAMETER (ALPHA=RD*XLAPSE/G)
c
c-----------------------------------------------------------------------
c
      DO I = 1,IMAX
         IF (ABS(PHIS(I)/G) .LT. 1.D-4) THEN
            PSL(I) = PS(I)
         ELSE
c pg 7 eq 5
            TSTAR = T(I,KMAX)* (1.D0+ALPHA* (PS(I)/PRES(I,KMAX)-1.D0))

c pg 8 eq 13
            T0 = TSTAR + XLAPSE*PHIS(I)/G

c pg 8 eq 14.1
            IF (TSTAR.LE.290.5D0 .AND. T0.GT.290.5D0) THEN
               ALPH = RD/PHIS(I)* (290.5D0-TSTAR)
c pg 8 eq 14.2
            ELSE IF (TSTAR.GT.290.5D0 .AND. T0.GT.290.5D0) THEN
               ALPH = 0.D0
               TSTAR = 0.5D0* (290.5D0+TSTAR)
            ELSE
               ALPH = ALPHA
            END IF
            IF (TSTAR.LT.255.D0) THEN
c pg 8 eq 14.3
               TSTAR = 0.5D0* (255.D0+TSTAR)
            END IF
            BETA = PHIS(I)/ (RD*TSTAR)
            PSL(I) = PS(I)*EXP(BETA* (1.D0-ALPH*BETA/2.D0+
     +           ((ALPH*BETA)**2)/3.D0))
         END IF
      END DO
      RETURN
      END
C*PL*ERROR* Comment line too long
c ----------------------------------------------------------------------------
      SUBROUTINE DCPSLHOR2(PRES,Z,PHIS,PSL,PS,T,IDIM,IMAX,KMAX,DLAT,
     +                     PSLHOR)
      DOUBLE PRECISION RD
      DOUBLE PRECISION G
      DOUBLE PRECISION XLAPSE
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION ZMIN
      DOUBLE PRECISION ZMID
      DOUBLE PRECISION ZA
      DOUBLE PRECISION ZF
      DOUBLE PRECISION PA
      DOUBLE PRECISION PF
      DOUBLE PRECISION DPA
      DOUBLE PRECISION DPF
      DOUBLE PRECISION FACTOR
c
c       File:   /crestone/u0/ccmproc2/src/src/cpslhor.F  (12843 bytes)
c       Date:   Wed Dec  8 13:42:48 1993
c       Author: Lawrence Buja <ccmproc2@sunny>
c
c       From:   inderf
c       Calls:  ** none **
c       Blocks: ** none **
c
c-----------------------------------------------------------------------
c
c       PSL via Trenberth's horizontal formulation
c       Purpose:   Compute sea level pressure for a latitude line
c       Algorithm:
c
c Designation:  --Low Area----|-------High Area------|---Low Area--
c                               ___________________
c                              /
c                             /_ _ _ _ _ _ _ _ _ _ _
c                            /      2000 meters
c                           /
c Surface:      ------------                           ------------
c Gridpoints:        X     X     X     X     X     X     X     X
c Counters:          A     B     C     D     E     F     G     H
c
c-----------------------------Arguments---------------------------------
c
c Input
c Atmospheric pressure (pascals)
      DOUBLE PRECISION PRES(IDIM,KMAX)
c Geopotential of hybrid levels (m**2/sec**2)
      DOUBLE PRECISION Z(IDIM,KMAX)
c Surface geopotential (m**2/sec**2)
      DOUBLE PRECISION PHIS(IDIM)
c Sea level pressures (pascals)
      DOUBLE PRECISION PSL(IDIM)
c Surface pressure (pascals)
      DOUBLE PRECISION PS(IDIM)
c Vertical slice of temperature (top to bot)
      DOUBLE PRECISION T(IDIM,KMAX)
c First dimension of phis, ps, and t
      INTEGER IDIM
c Number of longitude points to compute
      INTEGER IMAX
c Number of levels in t (2nd dimension)
      INTEGER KMAX
c Current latitude (-90. <= dlat <= +90.00)
      DOUBLE PRECISION DLAT
c
c Output
c
c PSL with Horiz correction (pascals)
      DOUBLE PRECISION PSLHOR(IDIM)
c
c-----------------------------Parameters--------------------------------
c
      PARAMETER (RD=287.04D0,G=9.80616D0,XLAPSE=6.5D-3)
      PARAMETER (ALPHA=RD*XLAPSE/G)
c
c--------------------------Local variables------------------------------
c
c Surrounding end points (2 on each bdry)
      INTEGER IENDPTS(10,2)
c if region lon1 thru lon2 is > 2000meters

c  1st dim: Assume there are multiple high

c           areas on 1 latitude line.

c  2nd dim: (N,1)= 1st  lon of high area N

c           (N,2)= Last lon of high area N

c Counter for first dimension of iendpts.
      INTEGER NEND
c .True. = looking for high area
      LOGICAL LSTART
c .False.= looking for low area

c
c-----------------------------------------------------------------------
c
c Min height of a "high" area.
      ZMIN = 2000.D0
c Max height to use PSL data for interpolation
      ZMID = 1250.D0
      NEND = 0
      LSTART = .true.
      LASTINDEX = 0
      DO I = 1,IMAX
c This a high area and
c Latitude is .gt. -60.00
          IF (PHIS(I)/G.GT.ZMIN .AND. DLAT.GT.-60.00D0) THEN
c Is this 1st long of high area?
              IF (LSTART) THEN
c > 2 points away from last area
                  IF (I-LASTINDEX.GT.3) THEN
c Increment counter.
                      NEND = NEND + 1
c Set beginning point index (i1)
                      IENDPTS(NEND,1) = I
c In High area, search for low area
                      LSTART = .false.
c < 2 points from last high area.
                  ELSE
c  merge last area into this area
                      LSTART = .false.
                  END IF
              END IF
c This is a low area
          ELSE
c PSL is not modified.
              PSLHOR(I) = PSL(I)
c 1st long of low area
              IF (.NOT.LSTART) THEN
c In low area, search for high area
                  LSTART = .true.
c Set end point index (i2)
                  IENDPTS(NEND,2) = I - 1
                  LASTINDX = I - 1
              END IF
          END IF
      END DO

      DO N = 1,NEND
c Beginning index of high area
          I1 = IENDPTS(N,1)
c Last index of high area
          I2 = IENDPTS(N,2)
          DO WHILE (PHIS(I1)/G.GT.ZMID)
              I1 = I1 - 1
          END DO
          DO WHILE (PHIS(I2)/G.GT.ZMID)
              I2 = I2 + 1
          END DO
          I1 = I1 + 1
          I2 = I2 - 1
c loop over longitude from C to F
          DO I = I1,I2
c Z Foes from 1=top to kmax=bottom
              DO LEV = 1,KMAX
                  IF (Z(I1-1,LEV).GT.PHIS(I)/G) LEVA = LEV
                  IF (Z(I2+1,LEV).GT.PHIS(I)/G) LEVF = LEV
              END DO
c
c  zA is hgt of PHIS(i1) at point i1 minus 2.
c
              ZA = (PHIS(I)/G-Z(I1-1,LEVA+1))/
     +             (Z(I1-1,LEVA)-Z(I1-1,LEVA+1))
              ZF = (PHIS(I)/G-Z(I2+1,LEVF+1))/
     +             (Z(I2+1,LEVF)-Z(I2+1,LEVF+1))
c
c PA is Pressure at the 2000 meter level at point i1 minus 2
c        (pres goes from 1=top down to kmax=bottom)
c
              PA = EXP(LOG(PRES(I1-1,LEVA+1))-
     +             LOG(PRES(I1-1,LEVA+1)/PRES(I1-1,LEVA))*ZA)
              PF = EXP(LOG(PRES(I2+1,LEVF+1))-
     +             LOG(PRES(I2+1,LEVF+1)/PRES(I2+1,LEVF))*ZF)

              DPA = PSL(I1-1) - PA
              DPF = PSL(I2+1) - PF
              FACTOR = DBLE(I-I1)/DBLE(I2-I1)
              PSLHOR(I) = PS(I) + (FACTOR*DPF+ (1-FACTOR)*DPA)
          END DO
      END DO

      RETURN
      END

C======================================================================

      SUBROUTINE DCPSLHOR(PRES,Z,PHIS,PSL,PS,T,IDIM,IMAX,KMAX,DLAT,
     +                    PSLHOR)
      DOUBLE PRECISION RD
      DOUBLE PRECISION G
      DOUBLE PRECISION XLAPSE
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION ZMIN
      DOUBLE PRECISION ZA
      DOUBLE PRECISION ZB
      DOUBLE PRECISION ZG
      DOUBLE PRECISION ZH
      DOUBLE PRECISION PA
      DOUBLE PRECISION PB
      DOUBLE PRECISION PG
      DOUBLE PRECISION PH
      DOUBLE PRECISION DPA
      DOUBLE PRECISION DPB
      DOUBLE PRECISION DPG
      DOUBLE PRECISION DPH
      DOUBLE PRECISION DPAB
      DOUBLE PRECISION DPGH
      DOUBLE PRECISION FACTOR
c
c       File:   /crestone/u0/ccmproc2/src/src/cpslhor.F  (6747 bytes)
c       Date:   Tue Dec  7 13:52:48 1993
c       Author: Lawrence Buja <ccmproc2@sunny>
c
c       From:   inderf
c       Calls:  ** none **
c       Blocks: ** none **
c
c-----------------------------------------------------------------------
c
c       PSL via Trenberth's horizontal formulation
c       Purpose:   Compute sea level pressure for a latitude line
c       Algorithm:
c
c Designation:  --Low Area----|-------High Area------|---Low Area--
c                               ___________________
c                              /
c                             /_ _ _ _ _ _ _ _ _ _ _
c                            /      2000 meters
c                           /
c Surface:      ------------                           ------------
c Gridpoints:        X     X     X     X     X     X     X     X
c Counters:          A     B     C     D     E     F     G     H
c
c-----------------------------Arguments---------------------------------
c
c Input
c Atmospheric pressure (pascals)
      DOUBLE PRECISION PRES(IDIM,KMAX)
c Geopotential of hybrid levels (m**2/sec**2)
      DOUBLE PRECISION Z(IDIM,KMAX)
c Surface geopotential (m**2/sec**2)
      DOUBLE PRECISION PHIS(IDIM)
c Sea level pressures (pascals)
      DOUBLE PRECISION PSL(IDIM)
c Surface pressure (pascals)
      DOUBLE PRECISION PS(IDIM)
c Vertical slice of temperature (top to bot)
      DOUBLE PRECISION T(IDIM,KMAX)
c First dimension of phis, ps, and t
      INTEGER IDIM
c Number of longitude points to compute
      INTEGER IMAX
c Number of levels in t (2nd dimension)
      INTEGER KMAX
c Current latitude (-90. <= dlat <= +90.00)
      DOUBLE PRECISION DLAT
c
c Output
c
c PSL with Horiz correction (pascals)
      DOUBLE PRECISION PSLHOR(IDIM)
c
c-----------------------------Parameters--------------------------------
c
      PARAMETER (RD=287.04D0,G=9.80616D0,XLAPSE=6.5D-3)
      PARAMETER (ALPHA=RD*XLAPSE/G)
c
c--------------------------Local variables------------------------------
c
c Surrounding end points (2 on each bdry)
      INTEGER IENDPTS(10,2)
c if region lon1 thru lon2 is > 2000meters

c  1st dim: Assume there are multiple high

c           areas on 1 latitude line.

c  2nd dim: (N,1)= 1st  lon of high area N

c           (N,2)= Last lon of high area N

c Counter for first dimension of iendpts.
      INTEGER NEND
c .True. = looking for high area
      LOGICAL LSTART
c .False.= looking for low area

c
c-----------------------------------------------------------------------
c
      ZMIN = 2000.D0
      NEND = 0
      LSTART = .true.
      LASTINDEX = 0
      DO I = 1,IMAX
c This a high area and
c Latitude is .gt. -60.00
          IF (PHIS(I)/G.GT.ZMIN .AND. DLAT.GT.-60.00D0) THEN
c Is this 1st long of high area?
              IF (LSTART) THEN
c > 2 points away from last area
                  IF (I-LASTINDEX.GT.3) THEN
c Increment counter.
                      NEND = NEND + 1
c Set beginning point index (i1)
                      IENDPTS(NEND,1) = I
c In High area, search for low area
                      LSTART = .false.
c < 2 points from last high area.
                  ELSE
c  merge last area into this area
                      LSTART = .false.
                  END IF
              END IF
c This is a low area
          ELSE
c PSL is not modified.
              PSLHOR(I) = PSL(I)
c 1st long of low area
              IF (.NOT.LSTART) THEN
c In low area, search for high area
                  LSTART = .true.
c Set end point index (i2)
                  IENDPTS(NEND,2) = I - 1
                  LASTINDX = I - 1
              END IF
          END IF
      END DO

      DO N = 1,NEND
c Beginning index of high area
          I1 = IENDPTS(N,1)
c Last index of high area
          I2 = IENDPTS(N,2)
c loop over longitude from C to F
          DO I = I1,I2
c Z goes from 1=top to kmax=bottom
              DO LEV = 1,KMAX
                  IF (Z(I1-2,LEV).GT.PHIS(I)/G) LEVA = LEV
                  IF (Z(I1-1,LEV).GT.PHIS(I)/G) LEVB = LEV
                  IF (Z(I2+1,LEV).GT.PHIS(I)/G) LEVG = LEV
                  IF (Z(I2+2,LEV).GT.PHIS(I)/G) LEVH = LEV
              END DO
c
c  zA is hgt of PHIS(i1) at point i1 minus 2.
c
              ZA = (PHIS(I)/G-Z(I1-2,LEVA+1))/
     +             (Z(I1-2,LEVA)-Z(I1-2,LEVA+1))
              ZB = (PHIS(I)/G-Z(I1-1,LEVB+1))/
     +             (Z(I1-1,LEVB)-Z(I1-1,LEVB+1))
              ZG = (PHIS(I)/G-Z(I2+1,LEVG+1))/
     +             (Z(I2+1,LEVG)-Z(I2+1,LEVG+1))
              ZH = (PHIS(I)/G-Z(I2+2,LEVH+1))/
     +             (Z(I2+2,LEVH)-Z(I2+2,LEVH+1))
c
c PA is Pressure at the 2000 meter level at point i1 minus 2
c        (pres goes from 1=top down to kmax=bottom)
c
              PA = EXP(LOG(PRES(I1-2,LEVA+1))-
     +             LOG(PRES(I1-2,LEVA+1)/PRES(I1-2,LEVA))*ZA)
              PB = EXP(LOG(PRES(I1-1,LEVB+1))-
     +             LOG(PRES(I1-1,LEVB+1)/PRES(I1-1,LEVB))*ZB)
              PG = EXP(LOG(PRES(I2+1,LEVG+1))-
     +             LOG(PRES(I2+1,LEVG+1)/PRES(I2+1,LEVG))*ZG)
              PH = EXP(LOG(PRES(I2+2,LEVH+1))-
     +             LOG(PRES(I2+2,LEVH+1)/PRES(I2+2,LEVH))*ZH)
              DPA = PSL(I1-2) - PA
              DPB = PSL(I1-1) - PB
              DPG = PSL(I2+1) - PG
              DPH = PSL(I2+2) - PH
              DPAB = (DPA+DPB)*0.5D0
              DPGH = (DPG+DPH)*0.5D0
c          if (Lprint) then
c             print *,' cpslhor.100: Levs= ',levA,levB,levG,levH
c             print *,' cpslhor.101: Zs=   ',zA,zB,zG,zH
c             print *,' cpslhor.102: Ps=   ',pA,pB,pG,pH
c             print *,' cpslhor.103: dps=  ',dpA,dpB,dpG,dpH,dpAB,dpGH
c           endif
              FACTOR = (I-I1+1.5D0)/ (I2-I1+3)
              PSLHOR(I) = PS(I) + (FACTOR*DPGH+ (1-FACTOR)*DPAB)
          END DO
      END DO
      RETURN
      END
