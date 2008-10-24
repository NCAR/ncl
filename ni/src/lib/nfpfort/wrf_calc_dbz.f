C NCLFORTSTART
      SUBROUTINE calcdbz (mdbz, dbz, prs, tmk, qvp, qra, qsn, qgr
     &          ,west_east_dim,south_north_dim,bottom_top_dim, ivarint)

c     NCL:  
c     procedure rip_reflectivity(mdbz, dbz, prs, tmk, qvp, qra, qsn, qgr,ivarint)
c
c     This routine computes equivalent reflectivity factor (in dBZ) at
c     each model grid point.  In calculating Ze, the RIP algorithm makes
c     assumptions consistent with those made in an early version
c     (ca. 1996) of the bulk mixed-phase microphysical scheme in the MM5
c     model (i.e., the scheme known as "Resiner-2").  For each species:
c
c     1. Particles are assumed to be spheres of constant density.  The
c     densities of rain drops, snow particles, and graupel particles are
c     taken to be rho_r = rho_l = 1000 kg m^-3, rho_s = 100 kg m^-3, and
c     rho_g = 400 kg m^-3, respectively. (l refers to the density of
c     liquid water.)
c
c     2. The size distribution (in terms of the actual diameter of the
c     particles, rather than the melted diameter or the equivalent solid
c     ice sphere diameter) is assumed to follow an exponential
c     distribution of the form N(D) = N_0 * exp( lambda*D ).
c
c     3. If ivarint=0, the intercept parameters are assumed constant (as
c     in early Reisner-2), with values of 8x10^6, 2x10^7, and 4x10^6 m^-4,
c     for rain, snow, and graupel, respectively.  If ivarint=1, variable
c     intercept parameters are used, as calculated in Thompson, Rasmussen,
c     and Manning (2004, Monthly Weather Review, Vol. 132, No. 2, pp. 519-542.)
c
c     More information on the derivation of simulated reflectivity in RIP
c     can be found in Stoelinga (2005, unpublished write-up).  Contact
c     Mark Stoelinga (stoeling@atmos.washington.edu) for a copy.
c

      IMPLICIT NONE

c !Arguments                                                   ! INPUT
      integer    west_east_dim,south_north_dim,bottom_top_dim,ivarint
      double precision prs(west_east_dim,south_north_dim,bottom_top_dim)
      double precision tmk(west_east_dim,south_north_dim,bottom_top_dim)
      double precision qvp(west_east_dim,south_north_dim,bottom_top_dim)
      double precision qra(west_east_dim,south_north_dim,bottom_top_dim)
      double precision qsn(west_east_dim,south_north_dim,bottom_top_dim)
      double precision qgr(west_east_dim,south_north_dim,bottom_top_dim)

c                                                              ! OUTPUT
      double precision dbz(west_east_dim,south_north_dim,bottom_top_dim)
      double precision mdbz(west_east_dim,south_north_dim)
C NCLEND
c                                                              ! LOCAL   
      integer    i, j, k
      double precision temp_c, virtual_t
      double precision gonv, ronv, sonv
      double precision factor_g, factor_r, factor_s
      double precision factorb_g, factorb_r, factorb_s
      double precision rhoair, z_e

      double precision rn0_r, rn0_s, rn0_g
      double precision r1, ron, ron2, son, gon, ron_min, ron_qr0
      double precision gamma_seven, RHOWAT, rho_r, rho_s, rho_g 
      double precision alpha, CELKEL, PI, Rd
      double precision ron_delqr0, ron_const1r, ron_const2r

c
c   Constant intercepts
c
      rn0_r = 8.d6
      rn0_s = 2.d7
      rn0_g = 4.d6

      r1   = 1.d-15 
      ron  = 8.d6  
      ron2 = 1.d10
      son  = 2.d7  
      gon  = 5.d7  
      ron_min = 8.d6   
      ron_qr0 = 0.00010
      ron_delqr0  = 0.25d0*ron_qr0
      ron_const1r = (ron2-ron_min)*0.5d0
      ron_const2r = (ron2+ron_min)*0.5d0

c   Other constants
      gamma_seven = 720.
      RHOWAT = 1000.    
      rho_r  = RHOWAT        
      rho_s  = 100.        
      rho_g  = 400.        
      alpha  = 0.224
      CELKEL = 273.15
      PI     = 3.141592653589793d0
      Rd     = 287.04

      factor_r = gamma_seven * 1.d18 * (1./(PI*rho_r))**1.75
      factor_s = gamma_seven * 1.d18 * (1./(PI*rho_s))**1.75    
     &              * (rho_s/RHOWAT)**2 * alpha
      factor_g = gamma_seven * 1.d18 * (1./(PI*rho_g))**1.75    
     &              * (rho_g/RHOWAT)**2 * alpha
 
      DO k = 1,bottom_top_dim
        DO j = 1,south_north_dim
          DO i = 1,west_east_dim
 
             virtual_t = tmk(i,j,k)*(0.622d0+qvp(i,j,k))
     +                             /(0.622d0*(1.0d0+qvp(i,j,k)))
             rhoair=prs(i,j,k)*100.d0/ ( Rd*virtual_t )  
 
c      Adjust factor for brightband, where snow or graupel particle
c      scatters like liquid water (alpha=1.0) because it is assumed to
c      have a liquid skin.
 
            IF (tmk(i,j,k) .gt. CELKEL) THEN
              factorb_s=factor_s/alpha
              factorb_g=factor_g/alpha
            ELSE
              factorb_s=factor_s
              factorb_g=factor_g
            ENDIF
 
c      Calculate variable intercept parameters
 
         if (ivarint.eq.1) then

            temp_c = dmin1(-0.001d0, tmk(i,j,k)-CELKEL)
            sonv   = dmin1(2.0d8, 2.0d6*exp(-0.12d0*temp_c))
 
            gonv = gon
            IF (qgr(i,j,k).gt.r1) THEN
              gonv = 2.38d0*(PI*rho_g/(rhoair*qgr(i,j,k)))**0.92
              gonv = max(1.d4, min(gonv,gon))
            ENDIF
 
            ronv = ron2
            IF (qra(i,j,k).gt. r1) THEN
               ronv = ron_const1r*tanh((ron_qr0-qra(i,j,k))      
     &                /ron_delqr0) + ron_const2r
            ENDIF
 
         else

            ronv = rn0_r
            sonv = rn0_s
            gonv = rn0_g

         endif

c      Total equivalent reflectivity factor (z_e, in mm^6 m^-3) is
c      the sum of z_e for each hydrometeor species:
 
            z_e =   factor_r  * (rhoair*qra(i,j,k))**1.75 /ronv**0.75   
     &            + factorb_s * (rhoair*qsn(i,j,k))**1.75 /sonv**0.75   
     &            + factorb_g * (rhoair*qgr(i,j,k))**1.75 /gonv**0.75

c      Adjust small values of Z_e so that dBZ is no lower than -30
            z_e = max(z_e,0.001d0)

c      Convert to dBZ
            dbz(i,j,k) = 10.d0 * dlog10(z_e)
 
          ENDDO
        ENDDO
      ENDDO

c f90 mdbz(:,:) = 0.0
      DO j = 1,south_north_dim
        DO i = 1,west_east_dim
           mdbz(i,j) = 0.0d0
        ENDDO
      ENDDO

      DO j = 1,south_north_dim
        DO i = 1,west_east_dim
          DO k = 1,bottom_top_dim
             mdbz(i,j) = MAX( mdbz(i,j) , dbz(i,j,k) )
          ENDDO
        ENDDO
      ENDDO

c f90 END SUBROUTINE calcdbz
      return
      end
