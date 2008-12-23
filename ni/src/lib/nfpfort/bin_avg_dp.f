C NCLFORTSTART
      subroutine bindataavg(nz,zlon,zlat,z,zmsg
     +                     ,mlon,nlat,glon,glat
     +                     ,gbinknt,iopt,ier)
      implicit none
c                                             ; INPUT
      integer              nlat, mlon, nz, iopt, ier
      double precision     zlon(nz), zlat(nz), z(nz), zmsg
      double precision     glon(mlon), glat(nlat)
c                                             ; OUTPUT
      double precision     gbinknt(mlon,nlat,2)
C NCLEND

c NCL:   function bin_avg(zlon[*],zlat[*],z[*], glon[*], glat[*], opt )

      integer  k, nl, ml, iflag
      double precision     dlat, dlon, glatbnd, glonbnd
     
      ier  = 0 

      dlat = abs(glat(2)-glat(1))
      dlon = abs(glon(2)-glon(1))

c error checking
      if (dlat.lt.0.0d0 .or. dlon.lt.0.0d0) then
          ier =  1
      end if
c                            check for equal lat spacing
      do nl=1,nlat-1
         if (abs(glat(nl+1)-glat(nl)).ne.dlat) then
             ier = ier + 10
             go to 10
         end if
      end do
   10 continue
c                            check for equal lon spacing
      do ml=1,mlon-1
         if (abs(glon(2)-glon(1)).ne.dlon) then
             ier = ier + 100
             go to 20
         end if
      end do
   20 continue
c                            set all msg if error
      if (ier.ne.0) then
          do nl=1,nlat
            do ml=1,mlon
              gbinknt(ml,nl,1) = zmsg
              gbinknt(ml,nl,2) = zmsg
            end do
          end do
          return
      end if
c                            initialize
      do nl=1,nlat
        do ml=1,mlon
           gbinknt(ml,nl,1) = 0.0d0  
           gbinknt(ml,nl,2) = 0.0d0 
        end do
      end do
c                            ; monotonically {in/de}creasing
      if ((glat(2)-glat(1)).gt.0.0d0) then
          iflag   =  1
      else
          iflag   = -1
      end if
      glatbnd = glat(1) - iflag*dlat  
      glonbnd = glon(1) -       dlon/2

      do k=1,nz 
         if (z(k).ne.zmsg) then
             nl = abs((zlat(k)-glatbnd)/dlat) + 1
             ml =     (zlon(k)-glonbnd)/dlon  + 1
             if (nl.gt.0 .and. nl.le.nlat .and.
     +           ml.gt.0 .and. ml.le.mlon) then
                 gbinknt(ml,nl,1) = gbinknt(ml,nl,1) + z(k)
                 gbinknt(ml,nl,2) = gbinknt(ml,nl,2) + 1    
             end if
         end if
      end do
c                                     compute bin average
      do nl=1,nlat
        do ml=1,mlon
           if (gbinknt(ml,nl,2).gt.0.0d0) then
               gbinknt(ml,nl,1) = gbinknt(ml,nl,1)/gbinknt(ml,nl,2)
           else
               gbinknt(ml,nl,1) = zmsg
           end if
        end do
      end do

      return
      end
