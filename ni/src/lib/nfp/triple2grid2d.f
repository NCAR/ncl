C NCLFORTSTART
      subroutine triple2grid2d (x,y,z,kpts,zmsg,distmx,mopt
     +                         ,lat2d,lon2d,zgrid,ndim,mdim)
      implicit none
      integer  kpts, ndim, mdim, mopt
      real     x(kpts), y(kpts), z(kpts), zmsg, distmx
      real     lat2d(mdim,ndim), lon2d(mdim,ndim), zgrid(mdim,ndim)
C NCLEND

C untested

c          SET EACH GRID POINT TO THE NEAREST OBSERVATION.
c .   The user can set distmx to some value. Setting to
c .   some very large value basically means that all grid
c .   points will be filled. Otherwise, only observations within
c .   distmx will be used.
c
c mopt   option for which distance formula to be used
c .      =  0 ; use pythag approx to calculate distance (quick/less-general] 
c .      =  1 ; use great circle formula to calculate distance (slower)
c
c distmx any x/y observation .le. distmx will be used.
c .      Set to some very large number [ 1.e20 ] if all grid points
c .      are to be returned with some value.
c .      Observations .gt. distmx will be ignored. If distmx is set small than 
c .      it is possible that some grid points will be filled with missing values.

C NCL    zgrid = triple2grid2d (x,y,z, lon2d,lat2d, opt)

c                      local: re: rad earth (km) [only for mopt=1]
      integer n, m, k
      real    dist(kpts), dmin, re, rlat, rad
      data    re /6371.2200/

      rad = 4.*atan(1.0)/180.
      
      do n=1,ndim
        do m=1,mdim
           zgrid(m,n) = zmsg
c                    do separately for optimization reasons
           if (mopt.eq.0) then
               do k=1,kpts
                  dist(k) = sqrt( (x(k)-lon2d(m,n))**2
     +                          + (y(k)-lat2d(m,n))**2)
               end do
          else
               rlat = lat2d(m,n)*rad 
               do k=1,kpts
                  dist(k) = acos(sin(rlat)*sin(y(k)*rad) +              
     +                           cos(rlat)*cos(y(k)*rad)
     +                          *cos((x(k)-lon2d(m,n))*rad))*re
               end do
          end if
c                    assign z(k) to nearest grid point
          dmin = 1.e+36
          do k=1,kpts
             if (z(k).ne.zmsg) then
                 if (dist(k).lt.dmin .and. dist(k).lt.distmx) then
                     dmin = dist(k)
                     zgrid(m,n) = z(k)
                 end if
             end if
          end do

        end do
      end do

      return
      end
