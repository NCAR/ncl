      subroutine cz2ccm(ps,phis,tv,p0,hyam,hybm,hyai,hybi
     $                 ,mlon,nlat,klev,klev1,z2,pmln,hypdln,hyalph
     $                 ,zslice,hyba,hybb,pterm,tv2)
c c c implicit none

C NCL: z2 = cz2ccm (ps,phis,tv,p0,hyam,hybm,hyai,hybi)
                                       
      integer mlon, nlat, klev
      real    ps(mlon,nlat), phis(mlon,nlat)
     $   ,    tv(mlon,nlat,klev), p0
     $   ,    hyam(klev) , hybm(klev)
     $   ,    hyai(klev1), hybi(klev1)
     
      real tv2(mlon,klev)      ! Array to hold 2D tv values

      real  z2(mlon,nlat,klev)  ! OUTPUT: geopotential at midlayer

      real pmln(mlon,klev+1)  ! logs of midpoint pressures
     $    ,hypdln(mlon,klev)  ! log p layer thickness
     $    ,hyalph(mlon,klev)  ! distance from interface to level
     $    ,zslice(mlon,klev)  ! calculated Z for vertical slice
     $    ,hyba(2,klev+1)     ! Hybrid coord coeffs: =1 interface
     $    ,hybb(2,klev+1)     ! Hybrid coord coeffs: =2 midlayer
     $    ,pterm(mlon,klev)   ! vertical scratch space

      real    zmsg, hprb
      integer kl, ml, nl

C Scratch arrays

      zmsg = 1.e+36
      hprb = p0                    ! Hybrid base pressure (pascals)

      do i=1,klev
         do j=1,nlat
            do k=1,mlon
               z2(k,j,i)     = 0.0 ! initilize to zero.
            end do
         end do
      end do
      do i=1,klev
         do k=1,mlon
            hypdln(k,i) = 0.0
            hyalph(k,i) = 0.0
            zslice(k,i) = 0.0
            pterm(k,i)  = 0.0
         end do
      end do

      do i=1,klev+1
         do k=1,2
            hyba(k,i) = 0.0
            hybb(k,i) = 0.0
         end do
      end do
                                  ! copy to temporary arrays
      do kl=1,klev+1              ! required by "cz2"
         hyba(1,kl) = hyai(kl)    ! interface=1
         hybb(1,kl) = hybi(kl)
      end do

      do kl=1,klev    
         hyba(2,kl+1) = hyam(kl)  ! mid-points=2
         hybb(2,kl+1) = hybm(kl)
      end do

                      ! one vertical slice (mlon,klev) at atime
      do nl=1,nlat
         do j=1,klev
            do i=1,mlon
               tv2(i,j) = tv(i,nl,j)
            end do
         end do

         call cz2(ps(1,nl),phis(1,nl), tv2,
     $            hprb, hyba, hybb, klev, mlon, mlon, pmln,
     $            hypdln, hyalph, pterm, zslice )

         do kl=1,klev
          do ml=1,mlon
             z2(ml,nl,kl) = zslice(ml,kl)
          end do
         end do
      end do    ! end nl

      return
      end
c +++
      subroutine cz2(ps      ,phis   ,tv     ,hprb   ,hyba   ,
     $               hybb    ,kmax   ,idim   ,imax   ,pmln   ,
     $               hypdln  ,hyalph ,pterm  ,Z2     )
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
      implicit none
c-----------------------------Parameters--------------------------------
c      
      real       r,g0,rbyg         
      parameter (r=287.04, g0=9.80616, rbyg=r/g0)         
c      
c------------------------------Commons----------------------------------
c ncl include 'debug.com'
c-----------------------------Arguments---------------------------------
c
c Input
c
      integer idim               ! Longitude dimension               
      integer kmax               ! Number of vertical levels         
      real    ps (idim)          ! Surface pressure           (pascals) 
      real    phis (idim)        ! Surface geoptential                  
      real    tv (idim,kmax)     ! Virtual temperature, top to bottom   
      real    hprb               ! Hybrid base pressure       (pascals) 
      real    hyba (2,kmax+1)    ! Hybrid coord coeffs for base pressure
                                 !  ground to top, first subscript:
                                 !  = 1 for layer interfaces
                                 !  = 2 for layer midpoints
                                 !  Lowest level is ground for both layer
                                 !    locations
      real    hybb (2,kmax+1)    ! Hybrid coord coeffs for surf pressure
                                 !   (in same format as hyba)
      integer imax               ! Num of longitude points to compute
      real    pmln (idim,kmax+1) ! vertical slice scratch space used to
                                 !   hold logs of midpoint pressures
      real    hypdln (idim,kmax) ! Vertical slice scratch space used to
                                 !   hold log p layer thichkess
      real    hyalph (idim,kmax) ! Vertical slice scratch space used to
                                 ! hold distance from interface to level
                                 ! during vertical integration
                                 ! Note: These scratch vertical slices
                                 !       are used to improve computaional
                                 !       efficiency
      real    pterm(idim,kmax)   ! Vertical scratch space.
      real    arg                ! TEMPORARY
c
c Output
c
      real    Z2 (idim,kmax)     ! Geopotential height, top to bottom 
c
c--------------------------Local variables------------------------------
c
      integer i,k,l,num              ! indexes
c
c-----------------------------------------------------------------------
c
      data num /0/
      num=num+1
c
c       All arrays except hyba, hybb are oriented top to bottom
      
c       Compute intermediate quantities using scratch space
c       pmln(i,k+1) is pressure at the midpoint of layer k

      do i=1,imax
        pmln(i,1)      = alog( hprb*hyba(2,kmax)+ps(i)*hybb(1,kmax) )
        pmln(i,kmax+1) = alog( hprb*hyba(2,1)   +ps(i)*hybb(1,1)    )
      enddo

c       Invert vertical loop
c       Compute top only if top interface pressure is nonzero.
c       Implemented by setting loop limit klim
c
c       hyba, hybb are bottom to top, starting at ground.
c       pmln(i,k) is the mid-point pressure of layer k.
c       SHEA MODIFICATION
    
      do k=kmax+1,1,-1
        do i=1,imax
c DJS     pmln(i,k) = alog(hprb*hyba(2,kmax-k+2)+ps(i)*hybb(2,kmax-k+2))
          arg       =      hprb*hyba(2,kmax-k+2)+ps(i)*hybb(2,kmax-k+2) 
          if (arg.gt.0.) then
              pmln(i,k) = alog(arg)
          else
              pmln(i,k) = 0.0
          end if
        enddo
      enddo
c
c       Initialize Z2 to sum of ground height and thickness of
c        top half-layer  (i.e. (phi)sfc in equation 1.14)

c       (Z2(i,1)=top  ->  Z2(i,kmax)=bottom

c       Eq 3.a.109.2  where l=K,k<K  h(k,l) = 1/2 * ln [  p(k+1) / p(k) ]

      do  k=2,kmax-1
        do i=1,imax         
          pterm(i,k) = rbyg*tv(i,k)*0.5*(pmln(i,k+1) - pmln(i,k-1))
        enddo
      enddo

      do k=1,kmax-1                                   ! 3.a.109.2
        do i=1,imax
          Z2(i,k) = phis(i)/g0+rbyg*tv(i,k)*0.5*( pmln(i,k+1)-pmln(i,k))    
        enddo
      enddo

c       Eq 3.a.109.5  where l=K,k=K  h(k,l) = ln [ pi / (p(k)) ]

      k=kmax
      do i=1,imax                                     ! 3.a.109.5     
        Z2(i,k) = phis(i)/g0 + rbyg*tv(i,k)*
     $                         (  alog(ps(i)*hybb(1,1)) - pmln(i,k))

      enddo

c       Eq 3.a.109.4  where l=K,k<K  h(k,l) = 1/2*ln[pi*pi/(p(k-1)*p(k))

      do k=1,kmax-1                                   ! 3.a.109.4
        l=kmax 
        do i=1,imax
          Z2(i,k) = Z2(i,k) + rbyg*tv(i,l)*
     $        ( alog(ps(i)*hybb(1,1)) - 0.5*(pmln(i,l-1) + pmln(i,l)))
        enddo
      enddo

c       Add thickness of the remaining full layers
c        (i.e., integrate from ground to highest layer interface)

c       Eqs 1.14 & 3.a.109.3 where l>K, k<K  
c                                h(k,l) = 1/2 * ln [ p(l+1)/p(l-1) ]

      do k=1,kmax-2                                   ! 3.a.109.3
        do l=k+1,kmax-1
          do i=1,imax
            Z2(i,k) = Z2(i,k) + pterm(i,l)
          enddo
        enddo
      enddo

      return
      end
