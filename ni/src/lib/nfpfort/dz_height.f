C NCLFORTSTART
      subroutine dzhgtdrv(mlon,nlat,klvl
     +                   ,z,zsfc,zmsg,ztop,dz,iopt,ier)
      implicit none
C                                                ! input 
      integer          mlon,nlat,klvl,iopt,ier
      double precision z(mlon,nlat,klvl), zsfc(mlon,nlat), ztop, zmsg
C                                                ! output
      double precision dz(mlon,nlat,klvl)
C NCLEND
c
c     dzres = dz_height(z, zsfc, ztop, iopt)
c                                                ! local
      double precision zlvl(klvl), dzlvl(klvl)
      integer          mono, kl, nl, ml

c check to see if ztop is reasonable
c .   ztop < 0      is not allowed

      ier    = 0

      if (ztop.lt.0.0) then
          ier = 1
      end if

      do kl=1,klvl
        do nl=1,nlat
          do ml=1,mlon
             dz(ml,nl,kl) = zmsg
          end do
        end do
      end do

      nl = 1
      ml = 1
      do kl=1,klvl
         zlvl(kl) = z(ml,nl,kl)
      end do

      call dzmono (klvl,zlvl,zmsg,mono,ier)
      if (ier.ne.0) return 

      do nl=1,nlat
        do ml=1,mlon
           if (zsfc(ml,nl).ne.zmsg) then

               if (mono.eq. 1) then
                   do kl=1,klvl
                      zlvl(kl) = z(ml,nl,kl)
                   end do
               else
                   do kl=1,klvl
                      zlvl(kl) = z(ml,nl,klvl-kl+1)
                   end do
               end if

               call dzheight(klvl,zlvl,zsfc(ml,nl),zmsg,ztop
     +                      ,dzlvl,iopt,ier)

               if (mono.eq. 1) then
                   do kl=1,klvl
                      dz(ml,nl,kl) = dzlvl(kl)
                   end do
               else
                   do kl=1,klvl
                      dz(ml,nl,klvl-kl+1) = dzlvl(kl)
                   end do
               end if

           end if
        end do
      end do

      return
      end
c ==================
C NCLFORTSTART
      subroutine dzheight(klvl,z,zsfc,zmsg,ztop,dzlvl,iopt,ier)
      implicit none
C                                                ! input 
      integer          klvl,iopt,ier
      double precision z(klvl), zsfc, ztop, zmsg
C                                                ! output
      double precision dzlvl(klvl)
C NCLEND
c The input *must* be monotonically increasing
c     dzres = dz_height(z, zsfc, ztop, iopt)
c
c The returned 'dz' are equivalent to having used  beta factors.
c must be monotonically increasing
 
C                                                ! local
      integer          kl, kll
      double precision zlvl(klvl), zbot, zspan, zeps
      double precision dzsum, zsfcmx, zsfcmn, work(klvl)
      data             zeps /0.001d0/

      ier = 0

c lower/upper boundaries missing?

      if (zsfc.eq.zmsg .or. ztop.eq.zmsg) then
          do kl=1,klvl
             dzlvl(kl) = zmsg
          end do
          ier = 15
          return
      end if

c monotonically increasing?

      if (z(2).lt.z(1)) then
          ier = 25
          return
      end if

c initialize to nominal dzlvl

      dzlvl(1)    = abs( (z(1)+z(2))*0.5d0 - ztop )
      do kl=2,klvl-1 
         dzlvl(kl)= abs( (z(kl+1) - z(kl-1))*0.5d0 )
      end do
      dzlvl(klvl) = abs( zsfc -(z(klvl)-z(klvl-1))*0.5d0 )

c levels outside the range should be set to 0.0

      do kl=1,klvl 
         if (z(kl).lt.ztop) dzlvl(kl) = 0.0d0
         if (z(kl).gt.zsfc) dzlvl(kl) = 0.0d0
      end do

c modify the default dz

      if (ztop.gt.0.0d0) then
          do kl=1,klvl-1
             if (ztop.ge.z(kl) .and. ztop.lt.z(kl+1)) then
                 dzlvl(kl) =(z(kl)+z(kl+1))*0.5d0-ztop
                 go to 100
             end if
          end do
      end if

  100 if (zsfc.gt.z(klvl)) then
          dzlvl(klvl) = zsfc -(z(klvl-1)+z(klvl))*0.5d0
      else
          do kl=1,klvl-1
             if (zsfc.ge.z(kl) .and. zsfc.lt.z(kl+1)) then
                 dzlvl(kl)= zsfc-(z(kl)+z(kl-1))*0.5d0
                  do kll=(kl+1),klvl
                     dzlvl(kll) = 0.0d0
                  end do
                  go to 200
             end if
          end do
      end if

  200 zspan = zsfc-ztop

c check to make sure:     sum(dz)=zspan
c zeps could me much smaller [1e-5]
c should probably be a fatal error .... indicates something is wrong

      dzsum = 0.0d0
      do kl=1,klvl
         dzsum = dzsum + dzlvl(kl)
      end do
      if (dzsum.gt.(zspan+zeps) .or. dzsum.lt.(zspan-zeps) ) then
          ier = ier-1
      end if

      return
      end
c --------
      subroutine dzmono(klvl,z,zmsg,mono,ier)
      implicit none
C                                                ! input 
      integer          klvl, ier
      double precision z(klvl), zmsg
C                                                ! output
      integer  mono
C                                                ! local 
      integer  kl 

      do kl=1,klvl
         print *,"dzmono=",kl, z(kl)
      end do

      ier = 0
      do kl=2,klvl
         if (z(kl).ne.zmsg .and. z(kl-1).ne.zmsg) then
                 print *,"DUBUG 1"
             if (z(kl).gt.z(kl-1)) then
                 mono =  1
                 print *,"DUBUG 2: mono=", mono
                 return
             elseif(z(kl).lt.z(kl-1)) then
                 mono = -1
                 print *,"DUBUG 3: mono=",mono
                 return
             else
                 mono = 0
                 ier  = 5
                 return
             end if
         end if
      end do

      return 
      end
