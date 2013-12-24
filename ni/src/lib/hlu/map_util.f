C
C
C                Copyright (C)  2013
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C
      SUBROUTINE map_next_point (rlat, rlon, u, v, pval, status, init)

      real rlat, rlon, u, v
      double precision pval
C
C status is 0 for a normal next point that falls within the map boundary
C           1 for a point that is not projectable
C           2 for a point that is projectable but outside the current map boundary -- not used for now
C           3 for a point that crosses the cyclic boundary
C init is 1 for a point that starts a polygon or polyline
C         0 otherwise
C
      integer status, init
C
      COMMON /MAPCM1/  COSO,COSR,SINO,SINR,IPRJ,IROD
      DOUBLE PRECISION COSO,COSR,SINO,SINR
      INTEGER          IPRJ,IROD
      SAVE   /MAPCM1/
C
      COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +     URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
      DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +     URNG,VCEN,VMAX,VMIN,VOFF,VRNG
      INTEGER          ISSL
      SAVE   /MAPCM2/
C                                         

      COMMON /MAPCM8/  P,Q,R
      DOUBLE PRECISION P,Q,R
      SAVE   /MAPCM8/

      SAVE pold
C
C  P and PEPS are variables defined in EZMAP which are used to determine when a line crosses the cyclic boundary. PEPS is constant
C  relative to a particular map projection with set parameters for such things as center lon and map limites. P is calculated each time
C  the lat/lon to window transformation is performed for a particular point. Successive points where P differs by more than PEPS indicate
C  a crossing of the cyclic boundary. This routine is designed to give that information along with performing the transformation.
C
      status = 0
      if (peps .gt. 0.7 * (umax - umin)) then
         lpeps = .66 * peps
      else
         lpeps = peps
      end if


      CALL MAPTRN (RLAT,RLON,U,V)
      pval = p

      if (U .eq. 1e12) then
         status = 1
         pold = p
         return
      end if
         
      if (init .eq. 1) then
         pold = p
         return
      end if
      pnew = p

      if (iprj .ge. 2 .and. iprj .le. 6) then
C    
C crossover is not possible
C
         pold = pnew
         return
      end if

      if ((.not. pold .gt. 1e10) .AND. ABS(PNEW-POLD).GE.lpeps) then
         status = 3
C         print *, lpeps, pnew, pold, status
      end if

      pold = pnew

      return
      end
