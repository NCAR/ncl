C NCLFORTSTART
      subroutine omcalcccm(u     ,v    ,d    
     +                     ,dpsl ,dpsm  ,pmid ,pdel  
     +                     ,psfc ,hybd  ,hybm ,nprlev
     +                     ,omega,ilon  ,jlat ,klev )
      implicit none

c NCL: omega = omega_ccm (u     ,v    ,div         \
c                        ,dpsl ,dpsm  ,pmid ,pdel  \
c                        ,psfc ,hybd  ,hybm ,nprlev)
c
c-----------------------------------------------------------------------
c >>>> Andy Mai modified omcalc.f90 from the CAM3.0_p1 code to add
c      latitude and time dimensions.
c >>>> Dennis Shea dumbed it down to f77 and removed the "time"
c      dimension to minimize the size of the u/v arrays.  The
c      interface will loop thru the time steps.
c-----------------------------------------------------------------------
c
c Purpose:
c Calculate vertical pressure velocity (omega = dp/dt)
c
c Method:
c First evaluate the expressions for omega/p, then rescale to omega at
c the end.
c
c Author: CCM1
c-----------------------------------------------------------------------
c $Id: omcalc_ccm.f,v 1.1 2005-09-28 00:31:44 haley Exp $
c $Author: haley $
c
c------------------------------Arguments--------------------------------
c nomenclature
c .   u       - zonal wind
c .   v       - meridional wind
c .   d       - divergence
c .   dpsl    - longitudinal component of grad ln(ps)
c .   dpsm    - latitudinal component of grad ln(ps)
c .   pmid    - mid-level pressures
c .   pdel    - layer thicknesses (pressure)
c .   psfc    - bottom interface pressure ["pbot" in origial code]
c .   hybd    - hybi(k+1)-hybi(k)
c .   hybm    - mid-level hybrid coefficients
c .   nprlev  - first pure pressure level
c .
c .   omega   - vertical pressure velocity  [OUTPUT]
c-----------------------------------------------------------------------
C                                             ! input
      integer          ilon   ,jlat   ,klev   ,nprlev

      double precision d(ilon,jlat,klev)
      double precision u(ilon,jlat,klev)
      double precision v(ilon,jlat,klev)
      double precision pmid(ilon,jlat,klev)
      double precision pdel(ilon,jlat,klev)

      double precision psfc(ilon,jlat)
      double precision dpsl(ilon,jlat)
      double precision dpsm(ilon,jlat)

      double precision hybd(klev)
      double precision hybm(klev)
C                                             ! output
      double precision omega(ilon,jlat,klev)
C NCLEND
c-----------------------------------------------------------------------

c---------------------------Local workspace-----------------------------
c .   hkk     - diagonal element of hydrostatic matrix
c .   hlk     - super diagonal element
c .   suml    - partial sum over l = (1, k-1)
c .   vgpk    - v dot grad ps
c .   tmp     - vector temporary
c-------------------------- Automatic arrays----------------------------
      double precision hkk(ilon,jlat)
      double precision hlk(ilon,jlat)
      double precision suml(ilon,jlat)
      double precision rpmid(ilon,jlat,klev)
      double precision vgpk
      double precision tmp
c-----------------------------------------------------------------------
c     i,j,k - longitude, latitude, level indices
      integer  i,j,k

c
c Zero partial sum
c
        do j=1,jlat
          do i=1,ilon
             suml(i,j) = 0.d0
          end do
        end do
c
c calculate inverse [no need to check for 0.0]
c
      do k=1,klev
        do j=1,jlat
          do i=1,ilon
            rpmid(i,j,k) = 1.d0/pmid(i,j,k)
          end do
        end do
      end do
c
c Pure pressure part: top level
c
        do j=1,jlat
          do i=1,ilon
            hkk(i,j)     = 0.5*rpmid(i,j,1)
            omega(i,j,1) = -hkk(i,j)*d(i,j,1)*pdel(i,j,1)
            suml(i,j)    = suml(i,j) + d(i,j,1)*pdel(i,j,1)
          end do
        end do
c
c sum(k)(v(j)*ps*grad(lnps)*db(j)) part. Not normally invoked since
c the top layer is normally a pure pressure layer.
c
      if (1.ge.nprlev) then
            do j=1,jlat
              do i=1,ilon
                vgpk = (u(i,j,1)*dpsl(i,j)
     +                + v(i,j,1)*dpsm(i,j))*psfc(i,j)
                tmp = vgpk*hybd(1)
                omega(i,j,1) = omega(i,j,1)
     +                         + hybm(1)*rpmid(i,j,1)*vgpk
     +                         - hkk(i,j)*tmp
                suml(i,j) = suml(i,j) + tmp
              end do
            end do
      end if
c
c Integrals to level above bottom
c
      do k=2,klev-1
c
c Pure pressure part
c
          do j=1,jlat
            do i=1,ilon
              hkk(i,j)     = 0.5*rpmid(i,j,k)
              hlk(i,j)     =     rpmid(i,j,k)
              omega(i,j,k) = -hkk(i,j)*d(i,j,k)*pdel(i,j,k)
     +                        - hlk(i,j)*suml(i,j)
              suml(i,j)    = suml(i,j) + d(i,j,k)*pdel(i,j,k)
            end do
          end do
c
c v(j)*grad(lnps) part
c
        if (k.ge.nprlev) then
              do j=1,jlat
                do i=1,ilon
                  vgpk         = (u(i,j,k)*dpsl(i,j)
     +                         + v(i,j,k)*dpsm(i,j))*psfc(i,j)
                  tmp          = vgpk*hybd(k)
                  omega(i,j,k) = omega(i,j,k)
     +                         + hybm(k)*rpmid(i,j,k)*vgpk
     +                         - hkk(i,j)*tmp
                  suml(i,j)    = suml(i,j) + tmp
                end do
              end do
        end if

      end do
c
c Pure pressure part: bottom level
c
        do j=1,jlat
          do i=1,ilon
             hkk(i,j) = 0.5*rpmid(i,j,klev)
             hlk(i,j) =     rpmid(i,j,klev)
             omega(i,j,klev) =
     +           - hkk(i,j)*d(i,j,klev)*pdel(i,j,klev)
     +           - hlk(i,j)*suml(i,j)
          end do
        end do
c
c v(j)*grad(lnps) part. Normally invoked, but omitted if the model is
c running in pure pressure coordinates throughout (e.g. stratospheric
c mechanistic model).
c
      if (klev.ge.nprlev) then
          do j=1,jlat
            do i=1,ilon
              vgpk = (u(i,j,klev)*dpsl(i,j)
     +              + v(i,j,klev)*dpsm(i,j))* psfc(i,j)
              omega(i,j,klev) = omega(i,j,klev)
     +                          + hybm(klev)*rpmid(i,j,klev)*vgpk
     +                          - hkk(i,j)*vgpk*hybd(klev)
            end do
          end do
      end if
c
c The above expressions give omega/p. Rescale to omega.
c
        do k=1,klev
          do j=1,jlat
            do i=1,ilon
               omega(i,j,k) = omega(i,j,k)*pmid(i,j,k)
            end do
          end do
        end do

      return
      end
