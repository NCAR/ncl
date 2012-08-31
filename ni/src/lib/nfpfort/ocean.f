C NCLFORTSTART
      subroutine wgt_area_smooth (field,area,field_ret,
     &                              nx,ny,no,fill_value,icyclic)
      double precision field(nx,ny,no), area(nx,ny)
      double precision field_ret(nx,ny,no), fill_value
      integer icyclic
C NCLEND
C
C      write(*,*) nx,ny,no,fill_value
C     
      do i = 1,nx
         do n = 1, no
            field_ret(i,1,n) = fill_value
            field_ret(i,ny,n) = fill_value
         end do
      end do   
C
      if (icyclic .eq. 1) then
         ibeg = 1
         iend = nx
      else
         ibeg = 2
         iend = nx - 1
         do j = 2, ny - 1
            do n = 1, no
               field_ret(1,j,n) = fill_value
               field_ret(nx,j,n) = fill_value
            end do
         end do
      end if
C
      do j = 2, ny-1
          do i = ibeg, iend
              im1 = i-1
              ip1 = i+1
              if ( i .eq. 1    ) then  
                  im1 = nx
              end if
              if ( i .eq. nx ) then
                  ip1 = 1
              end if
C
              cc = area(i  ,j  )
              ce = area(ip1,j  )
              cw = area(im1,j  )
              cn = area(i  ,j+1)
              cs = area(i  ,j-1)
              sum = cc + ce + cw + cn + cs
C
              cc = cc / sum
              ce = ce / sum
              cw = cw / sum
              cn = cn / sum
              cs = cs / sum
              do n = 1, no
                 field_ret(i,j,n) = fill_value
              end do
              if ( field(i,j,1) .ne. fill_value ) then
                 if ( field(ip1,j,1) .eq. fill_value ) then
                     cc = cc + ce
                     ce = 0.
                 end if
                 if ( field(im1,j,1) .eq. fill_value ) then
                    cc = cc + cw
                    cw = 0.
                 end if
                 if ( field(i ,j+1,1) .eq. fill_value ) then
                    cc = cc + cn
                    cn = 0.
                 end if
                 if ( field(i  ,j-1,1) .eq. fill_value ) then
                    cc = cc + cs
                    cs = 0.
                 end if
C              
                 do n = 1, no
                    field_ret(i,j,n) =  cc * field(i ,j,n) 
     &                   + ce * field(ip1,j,  n) 
     &                   + cw * field(im1,j,  n) 
     &                   + cn * field(i,  j+1,n) 
     &                   + cs * field(i,  j-1,n)
                 end do
              end if
           end do
        end do          
C
      return 
      end
C
C NCLFORTSTART
      subroutine mixed_layer_depth(field,kmt,ht,depth,field_ret,
     &               nx,ny,nz,offset,fill_value)
      integer kmt(nx,ny)
      double precision field(nx,ny,nz)
      double precision field_ret(nx,ny), fill_value
      double precision offset
      double precision depth(nz)
      double precision  ht(nx,ny)
C NCLEND
C
      do j=1,ny
         do i=1,nx

            k_max = kmt(i,j)

            if ( k_max .eq. 0 ) then

               field_ret(i,j) = fill_value

            else

               field_ret(i,j) = ht(i,j) 

               target_density = field(i,j,1) + offset
               do  k=1, k_max-1

                  if ((target_density .gt. field(i,j,k)) 
     &                 .and.
     &                 (target_density .le. field(i,j,k+1))) then
C     
                     field_ret(i,j) = depth(k) 
     &                    + (target_density - field(i,j,k)) 
     &                    * ((depth(k+1) - depth(k))
     &                    / (field(i,j,k+1) - field(i,j,k)))

                     go to 100
                  end if

               end do

 100           continue

            end if
         end do
      end do

      return
      end
