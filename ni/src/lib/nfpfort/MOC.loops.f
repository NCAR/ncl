C NCLFORTSTART
      subroutine mocloops(nyaux,mlon,nlat,kdep,nrx,tlat,lat_aux_grid
     +                   ,rmlak,work1,work2,work3,wmsg,tmp1,tmp2,tmp3)
      implicit none
      integer  nyaux,mlon,nlat,kdep,nrx

      double precision     work1(mlon,nlat,kdep), work2(mlon,nlat,kdep)
     +                    ,work3(mlon,nlat,kdep), wmsg
      double precision     tmp1(nyaux,kdep,2)   , tmp2(nyaux,kdep,2)
     +                    ,tmp3(nyaux,kdep,2)  
      double precision     lat_aux_grid(nyaux)  , tlat(mlon,nlat)
      
      integer  rmlak(mlon,nlat,2)
C NCLEND
      integer  ny, kd, nl, ml, nr 
c c c logical  section(mlon,nlat,kdep)

c initilize 
      do nr=1,nrx
        do kd=1,kdep
          do ny=1,nyaux
             tmp1(ny,kd,nr) = 0.0d0 
             tmp2(ny,kd,nr) = 0.0d0 
             tmp3(ny,kd,nr) = 0.0d0 
          end do
        end do
      end do

c globe [note: rmlak(:,:,1)]

      do ny=2,nyaux

        do nl=1,nlat
          do ml=1,mlon
             if (tlat(ml,nl).ge.lat_aux_grid(ny-1) .and.
     +           tlat(ml,nl).lt.lat_aux_grid(ny)   .and.
     +           rmlak(ml,nl,1).eq.1) then

                 do kd=1,kdep
                    if (work1(ml,nl,kd).ne.wmsg)  then          
                        tmp1(ny,kd,1) = tmp1(ny,kd,1) + work1(ml,nl,kd)
                        tmp2(ny,kd,1) = tmp2(ny,kd,1) + work2(ml,nl,kd)
                        tmp3(ny,kd,1) = tmp3(ny,kd,1) + work3(ml,nl,kd)
                    end if
                 end do

             end if
          end do
        end do

      end do

c atlantic [note: rmlak(:,:,2)]

      do ny=2,nyaux

        do nl=1,nlat
          do ml=1,mlon
             if (tlat(ml,nl).ge.lat_aux_grid(ny-1) .and.
     +           tlat(ml,nl).lt.lat_aux_grid(ny)   .and.
     +           rmlak(ml,nl,2).eq.1) then

                 do kd=1,kdep
                    if (work1(ml,nl,kd).ne.wmsg)  then          
                        tmp1(ny,kd,2) = tmp1(ny,kd,2) + work1(ml,nl,kd)
                        tmp2(ny,kd,2) = tmp2(ny,kd,2) + work2(ml,nl,kd)
                        tmp3(ny,kd,2) = tmp3(ny,kd,2) + work3(ml,nl,kd)
                    end if
                 end do

             end if
          end do
        end do

      end do

      return
      end
