C NCLFORTSTART
      SUBROUTINE STATTROPX(NLEV,NLEVM,PFULL,TFULL,TMSG,LAPSEC,PUNIT
     +                     ,PTROP,ITROP,LAPSE,PHALF,PMB)
      IMPLICIT NONE
c                                                   ! INPUT
      INTEGER NLEV, NLEVM, PUNIT
      DOUBLE PRECISION PFULL(NLEV),TFULL(NLEV),TMSG,LAPSEC
c                                                   ! OUTPUT
      DOUBLE PRECISION PTROP
      INTEGER ITROP
c                                                   ! Work arrays
      DOUBLE PRECISION LAPSE(NLEVM),PHALF(NLEVM)
      DOUBLE PRECISION PMB(NLEV)
C NCLEND

c NCL  trop = trop_wmo(p,t,opt)
      INTEGER N

C      NLEVM = NLEV + 1
C                            DSTATTROP REQUIRES mb  [ hPa ]
      IF (PUNIT.EQ.0) THEN
          DO N=1,NLEV
             PMB(N) = PFULL(N)
          END DO
      ELSE
          DO N=1,NLEV
             PMB(N) = PFULL(N)*0.01D0
          END DO
      END IF

      CALL DSTATTROP(NLEV,PMB,TFULL,TMSG,LAPSEC,PTROP,ITROP
     +              ,NLEVM,LAPSE,PHALF)

C                                RETURN AS Pascals [ Pa ] if necessary
      IF (PUNIT.NE.0) THEN
          PTROP = PTROP*100.D0
      END IF

C SUBTRACT ONE FOR NCL 0-BASED INDEXING

      ITROP = ITROP -1

      RETURN
      END

      SUBROUTINE DSTATTROP(NLEV,PFULL,TFULL,TMSG,LAPSEC,PTROP,ITROP
     +                    ,NLEVM,LAPSE,PHALF)
c
c**********************************************************************
c     input : p, t        real(nlev)
c     input : nlev        real
c     output: ptrop       real
c     output: itrop     integer
c
c     programmed by Dominik Brunner              V1.0 Aug 2000
c     modified for use within NCL                D Shea Jan 2009
c
c     built upon routine stattrop by Peter van Velthoven,
c     KNMI, The Netherlands
c     and on the ECHAM4/CHEM routine xttphwmo by
c     Thomas Reichle, Christine Land, B. Steil and R. Hein, DLR
c
c     purpose
c     -------
c     stattrop computes the pressure (Pa) at the thermal (static) tropopause
c     (TP) for a 1D column (sounding) of pressures and temperatures following
c     the definition of the height of the tropopause as postulated by WMO (1957).
c
c     ATTENTION: In the current formulation of the program the first
c     level (index 1) must be at the top of the atmosphere and the
c     last level (index nlev) at the surface
c
c     interface
c     ---------
c     call stattrop(pfull, tfull, nlev, ptrop, itrop)
c     - Input
c       nlev : number of vertical levels
c       pfull: pressure in 1D column at nlev full levels (layers)
c       tfull: temperature in 1D column at nlev full levels
c     - Output
c       ptrop: height of the tropopause in Pa
c       itrop: index of *layer* containing the tropopause
c
c     methods
c     -------
c     - Lapse rate gamma = -dT/dz
c       Using the hydrostatic approximation
c
c       dz = -dp/(g*rho) = -dp/p * R/g * T = -dlnp * R/g * T
c
c       we get -dT/dz = dT/T * g/R *1/dlnp = dlnT/dlnp
c
c     - Variables are assumed to vary linearly in log(pressure)
c     - The tropopause is the lowest level above 450 hPa fullfilling
c       the WMO criterium. If ptrop is < 85 hPa it is set to 85 hPa.
c       If no tropopause is found ptrop is set to 'tmsg'.
c
c     references
c     ----------
c
c     - WMO (1992): International meteorological vocabulary, Genf, 784pp.:
c
c       " 1. The first tropopause is defined as the lowest level at which
c       the lapse rate decreases to 2 deg C per kilometer or less,
c       provided also the average lapse rate between this level and
c       all higher levels within 2 kilometers does not exceed 2 deg C"
c
c     - Randel WJ, Wu F, Gaffen DJ, Interannual variability of the tropical
c       tropopause derived from radiosonde data and NCEP reanalyses,
c       JOURNAL OF GEOPHYSICAL RESEARCH, 105, 15509-15523, 2000.
c
c       The following webpage clearifies the calculation of the tropopause
c       in the NCEP reanalysis: http://dss.ucar.edu/pub/reanalysis/FAQ.html
c
c       For comparison NCEP reanalysis tropopause pressures can be obtained
c       from http://www.cdc.noaa.gov/cdc/reanalysis/reanalysis.shtml
c
c**********************************************************************

      IMPLICIT NONE
c                                                   ! INPUT
      INTEGER NLEV,NLEVM
      DOUBLE PRECISION PFULL(NLEV),TFULL(NLEV),TMSG,LAPSEC
      DOUBLE PRECISION LAPSE(NLEVM),PHALF(NLEVM)
c                                                   ! OUTPUT
      DOUBLE PRECISION PTROP
      INTEGER ITROP
C NCLEND

      DOUBLE PRECISION PMIN, PMAX, P1, P2, WEIGHT
      DOUBLE PRECISION DELTAZ,P2KM,LAPSEAVG,LAPSESUM
      DOUBLE PRECISION GRAV,RGAS,CONST,RINVAL
      PARAMETER (GRAV=9.80665D0,RGAS=287.04D0
     +          ,CONST=1000.D0*GRAV/RGAS)
      INTEGER ILEV,ILEV1,KOUNT,L
      LOGICAL FOUND

      RINVAL = TMSG
      PTROP = RINVAL
      FOUND = .false.
      ITROP = -999

c c c do ilev=1,nlevm
c c c     lapse(ilev) = tmsg
c c c     phalf(ilev) = tmsg
c c c end do

c     deltaz is layer thickness used for second tropopause criterium
c     pmin and pmax allowed tropopause pressure

      DELTAZ = 2.D0
      PMIN   = 85.D0
      PMAX   = 450.D0

c     Loop from model top to surface and calculate lapse rate gamma=-dT/dz (K/km)
      DO ILEV = 1,NLEV - 1
          ILEV1 = ILEV + 1
          LAPSE(ILEV) = DLOG(TFULL(ILEV)) - DLOG(TFULL(ILEV1))
          LAPSE(ILEV) = LAPSE(ILEV)/ (DLOG(PFULL(ILEV))-
     +                  DLOG(PFULL(ILEV1)))
          LAPSE(ILEV) = CONST*LAPSE(ILEV)
          PHALF(ILEV) = (PFULL(ILEV)+PFULL(ILEV1))*0.5D0

c c c    print *,ilev, lapse(ilev),  phalf(ilev)
      END DO

c     Loop from surface to top to find (lowest) tropopause

      DO ILEV = NLEV - 2,1,-1
c 1st test: -dT/dz less than 2 K/km and pressure LT pmax? ****
          IF (LAPSE(ILEV).LT.LAPSEC .AND. PFULL(ILEV).LT.PMAX) THEN
              IF (.NOT.FOUND) THEN
c Interpolate tropopause pressure linearly in log(p)
                  P1 = DLOG(PHALF(ILEV))
                  P2 = DLOG(PHALF(ILEV+1))
                  IF (LAPSE(ILEV).NE.LAPSE(ILEV+1)) THEN
                      WEIGHT = (LAPSEC-LAPSE(ILEV))/
     +                         (LAPSE(ILEV+1)-LAPSE(ILEV))
c                                                ! tropopause pressure
                      PTROP = EXP(P1+WEIGHT* (P2-P1))
                  ELSE
                      PTROP = PHALF(ILEV)
                  END IF
c 2nd test: average -dT/dz in layer deltaz above TP
c           must not be greater than 2K/km
c           press 2 km above TP
                  P2KM = EXP(DLOG(PTROP)-DELTAZ*CONST/TFULL(ILEV))

c init avg. lapse rate 2 km above TP
                  LAPSESUM = 0.0D0
                  LAPSEAVG = 1.D9

                  KOUNT = 0
                  DO L = ILEV,1,-1
                      IF (PHALF(L).LT.P2KM) GO TO 100
                      LAPSESUM = LAPSESUM + LAPSE(L)
                      KOUNT = KOUNT + 1
                      LAPSEAVG = LAPSESUM/KOUNT
                  END DO
  100             CONTINUE

                  FOUND = LAPSEAVG .LT. LAPSEC
c     Discard tropopause?
                  IF (.NOT.FOUND) THEN
                      PTROP = RINVAL
                  ELSE
c ptrop must be .ge. 85 hPa; assign level (layer) index
                      PTROP = MAX(PTROP,PMIN)
                      ITROP = ILEV
                  END IF
              END IF
          END IF
      END DO

c c c if (.not.found) then
c c c    write(*,*)
c c c&       'STATTROP-warning: no static tropopause found:'
c c c    write(*,*) 'phalf:', phalf
c c c    write(*,*) 'lapse:', lapse
c c c end if

      RETURN
      END
