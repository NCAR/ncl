      SUBROUTINE DCZ2CCM(PS,PHIS,TV,P0,HYAM,HYBM,HYAI,HYBI,MLON,NLAT,
     +                  KLEV,KLEV1,Z2,PMLN,HYPDLN,HYALPH,ZSLICE,HYBA,
     +                  HYBB,PTERM,TV2)
c c c implicit none

C NCL: z2 = cz2ccm (ps,phis,tv,p0,hyam,hybm,hyai,hybi)

      INTEGER MLON,NLAT,KLEV
      DOUBLE PRECISION PS(MLON,NLAT),PHIS(MLON,NLAT),TV(MLON,NLAT,KLEV),
     +                 P0,HYAM(KLEV),HYBM(KLEV),HYAI(KLEV1),HYBI(KLEV1)

C Array to hold 2D tv values
      DOUBLE PRECISION TV2(MLON,KLEV)

C OUTPUT: geopotential at midlayer
      DOUBLE PRECISION Z2(MLON,NLAT,KLEV)

C logs of midpoint pressures
C log p layer thickness
C distance from interface to level
C calculated Z for vertical slice
C Hybrid coord coeffs: =1 interface
C Hybrid coord coeffs: =2 midlayer
C vertical scratch space
      DOUBLE PRECISION PMLN(MLON,KLEV+1),HYPDLN(MLON,KLEV),
     +                 HYALPH(MLON,KLEV),ZSLICE(MLON,KLEV),
     +                 HYBA(2,KLEV+1),HYBB(2,KLEV+1),PTERM(MLON,KLEV)

      DOUBLE PRECISION ZMSG,HPRB
      INTEGER KL,ML,NL

C Scratch arrays

      ZMSG = 1.D+36
C Hybrid base pressure (pascals)
      HPRB = P0

      DO I = 1,KLEV
          DO J = 1,NLAT
              DO K = 1,MLON
C initilize to zero.
                  Z2(K,J,I) = 0.0D0
              END DO
          END DO
      END DO
      DO I = 1,KLEV
          DO K = 1,MLON
              HYPDLN(K,I) = 0.0D0
              HYALPH(K,I) = 0.0D0
              ZSLICE(K,I) = 0.0D0
              PTERM(K,I) = 0.0D0
          END DO
      END DO

      DO I = 1,KLEV + 1
          DO K = 1,2
              HYBA(K,I) = 0.0D0
              HYBB(K,I) = 0.0D0
          END DO
      END DO
C copy to temporary arrays

C required by "cz2"
      DO KL = 1,KLEV + 1
C interface=1
          HYBA(1,KL) = HYAI(KL)
          HYBB(1,KL) = HYBI(KL)
      END DO

      DO KL = 1,KLEV
C mid-points=2
          HYBA(2,KL+1) = HYAM(KL)
          HYBB(2,KL+1) = HYBM(KL)
      END DO

C one vertical slice (mlon,klev) at atime

      DO NL = 1,NLAT
          DO J = 1,KLEV
              DO I = 1,MLON
                  TV2(I,J) = TV(I,NL,J)
              END DO
          END DO

          CALL DCZ2(PS(1,NL),PHIS(1,NL),TV2,HPRB,HYBA,HYBB,KLEV,MLON,
     +             MLON,PMLN,HYPDLN,HYALPH,PTERM,ZSLICE)

          DO KL = 1,KLEV
              DO ML = 1,MLON
                  Z2(ML,NL,KL) = ZSLICE(ML,KL)
              END DO
          END DO
C end nl
      END DO

      RETURN
      END
c +++
      SUBROUTINE DCZ2(PS,PHIS,TV,HPRB,HYBA,HYBB,KMAX,IDIM,IMAX,PMLN,
     +               HYPDLN,HYALPH,PTERM,Z2)
c-----------------------------------------------------------------------
c
c       File:   /u3/ccmproc2/src/src/cz2.F  (7169 bytes)
c       Date:   Wed Oct 25 14:17:31 1995
c       Author: CCM Processor <ccmproc2@sol>
c
c       From:   inderf
c       Calls:  ** none **
c       Blocks: ** none **
c
c-----------------------------------------------------------------------
c
c       Purpose:
c         To compute geopotential height for a CCM2 hybrid coordinate
c         vertical slice.  Since the vertical integration matrix is a
c         function of latitude and longitude, it is not explicitly
c         computed as for sigma coordinates.  The integration algorithm
c         is derived from Boville's mods in the ibm file hybrid 1mods
c         (6/17/88).  All vertical slice arrays are oriented top to
c         bottom as in CCM2.  This field is on full model levels (aka
c         "midpoints") not half levels.
c
c       Equation references are to "Hybrid Coordinates for CCM1"
c
c----------------------------Code History-------------------------------
c
c       Original: Jun 25 1991  L Buja
c       Upgrades: Apr 21 1993  L Buja
c                     Truesdale reports difference from CCM2 Z2.
c                     - Make pterm a scratch array (was automatic)
c                     - Make g0=9.80616 (was 9.81).  This appears to
c                        affect only the lowest layer.
c       Change  : Feb    1999: D Shea
c                     NCL changes + error
c                     - The "Invert vertical loop" has to have
c                        the argument checked for >0.0
c
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------Parameters--------------------------------
c
      DOUBLE PRECISION R,G0,RBYG
      PARAMETER (R=287.04D0,G0=9.80616D0,RBYG=R/G0)
c
c-----------------------------Arguments---------------------------------
c
c Input
c
C Longitude dimension
      INTEGER IDIM
C Number of vertical levels
      INTEGER KMAX
C Surface pressure           (pascals)
      DOUBLE PRECISION PS(IDIM)
C Surface geoptential
      DOUBLE PRECISION PHIS(IDIM)
C Virtual temperature, top to bottom
      DOUBLE PRECISION TV(IDIM,KMAX)
C Hybrid base pressure       (pascals)
      DOUBLE PRECISION HPRB
C Hybrid coord coeffs for base pressure
      DOUBLE PRECISION HYBA(2,KMAX+1)
C  ground to top, first subscript:

C  = 1 for layer interfaces

C  = 2 for layer midpoints

C  Lowest level is ground for both layer

C    locations

C Hybrid coord coeffs for surf pressure
      DOUBLE PRECISION HYBB(2,KMAX+1)
C   (in same format as hyba)

C Num of longitude points to compute
      INTEGER IMAX
C vertical slice scratch space used to
      DOUBLE PRECISION PMLN(IDIM,KMAX+1)
C   hold logs of midpoint pressures

C Vertical slice scratch space used to
      DOUBLE PRECISION HYPDLN(IDIM,KMAX)
C   hold log p layer thichkess

C Vertical slice scratch space used to
      DOUBLE PRECISION HYALPH(IDIM,KMAX)
C hold distance from interface to level

C during vertical integration

C Note: These scratch vertical slices

C       are used to improve computaional

C       efficiency

C Vertical scratch space.
      DOUBLE PRECISION PTERM(IDIM,KMAX)
C TEMPORARY
      DOUBLE PRECISION ARG
c
c Output
c
C Geopotential height, top to bottom
      DOUBLE PRECISION Z2(IDIM,KMAX)
c
c--------------------------Local variables------------------------------
c
C indexes
      INTEGER I,K,L,NUM
c
c-----------------------------------------------------------------------
c
      DATA NUM/0/

      NUM = NUM + 1
c
c       All arrays except hyba, hybb are oriented top to bottom

c       Compute intermediate quantities using scratch space
c       pmln(i,k+1) is pressure at the midpoint of layer k

      DO I = 1,IMAX
          PMLN(I,1) = DLOG(HPRB*HYBA(2,KMAX)+PS(I)*HYBB(1,KMAX))
          PMLN(I,KMAX+1) = DLOG(HPRB*HYBA(2,1)+PS(I)*HYBB(1,1))
      END DO

c       Invert vertical loop
c       Compute top only if top interface pressure is nonzero.
c       Implemented by setting loop limit klim
c
c       hyba, hybb are bottom to top, starting at ground.
c       pmln(i,k) is the mid-point pressure of layer k.
c       SHEA MODIFICATION

      DO K = KMAX + 1,1,-1
          DO I = 1,IMAX
c DJS     pmln(i,k) = alog(hprb*hyba(2,kmax-k+2)+ps(i)*hybb(2,kmax-k+2))
              ARG = HPRB*HYBA(2,KMAX-K+2) + PS(I)*HYBB(2,KMAX-K+2)
              IF (ARG.GT.0.D0) THEN
                  PMLN(I,K) = DLOG(ARG)
              ELSE
                  PMLN(I,K) = 0.0D0
              END IF
          END DO
      END DO
c
c       Initialize Z2 to sum of ground height and thickness of
c        top half-layer  (i.e. (phi)sfc in equation 1.14)

c       (Z2(i,1)=top  ->  Z2(i,kmax)=bottom

C*PL*ERROR* Comment line too long
c       Eq 3.a.109.2  where l=K,k<K  h(k,l) = 1/2 * ln [  p(k+1) / p(k) ]

      DO K = 2,KMAX - 1
          DO I = 1,IMAX
              PTERM(I,K) = RBYG*TV(I,K)*0.5D0* (PMLN(I,K+1)-PMLN(I,K-1))
          END DO
      END DO

C 3.a.109.2
      DO K = 1,KMAX - 1
          DO I = 1,IMAX
              Z2(I,K) = PHIS(I)/G0 + RBYG*TV(I,K)*0.5D0*
     +                  (PMLN(I,K+1)-PMLN(I,K))
          END DO
      END DO

c       Eq 3.a.109.5  where l=K,k=K  h(k,l) = ln [ pi / (p(k)) ]

      K = KMAX
C 3.a.109.5
      DO I = 1,IMAX
          Z2(I,K) = PHIS(I)/G0 + RBYG*TV(I,K)*
     +              (DLOG(PS(I)*HYBB(1,1))-PMLN(I,K))

      END DO

c       Eq 3.a.109.4  where l=K,k<K  h(k,l) = 1/2*ln[pi*pi/(p(k-1)*p(k))

C 3.a.109.4
      DO K = 1,KMAX - 1
          L = KMAX
          DO I = 1,IMAX
              Z2(I,K) = Z2(I,K) + RBYG*TV(I,L)*
     +                  (DLOG(PS(I)*HYBB(1,1))-0.5D0*
     +                  (PMLN(I,L-1)+PMLN(I,L)))
          END DO
      END DO

c       Add thickness of the remaining full layers
c        (i.e., integrate from ground to highest layer interface)

c       Eqs 1.14 & 3.a.109.3 where l>K, k<K
c                                h(k,l) = 1/2 * ln [ p(l+1)/p(l-1) ]

C 3.a.109.3
      DO K = 1,KMAX - 2
          DO L = K + 1,KMAX - 1
              DO I = 1,IMAX
                  Z2(I,K) = Z2(I,K) + PTERM(I,L)
              END DO
          END DO
      END DO

      RETURN
      END
