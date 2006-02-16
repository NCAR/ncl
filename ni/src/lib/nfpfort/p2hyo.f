C NCLFORTSTART
      subroutine   p2hyo (pi,mlon,nlat,klevi,xi
     +                   ,psfc,p0,hyao,hybo,klevo,xo)
      implicit none
 
c NCL: xo = p2hyo (p,xi,psfc,hyao,hybo)

c this routine interploates one hybrid level to another
c     formula for the pressure of a hybrid surface is;
c          phy(k) = hya(k)*p0 + hyb(k)*psfc

c     input  ["i" input ... "o" output]
c          pi     - pressure level                     [input]
c          psfc   - is the surface pressure Pa         [input]
c          mlon   - longitude dimension
c          nlat   - latitude  dimension
c          klevi  - number of input  levels
c          hyao   - is the "a" or pressure hybrid coef 
c          hybo   - is the "b" or sigma coeficient    
c          klevo  - number of output levels
c     output
c          xo     - pressure at hybrid levels [Pa]
c                                                 ! input
      integer  mlon, nlat, klevi, klevo
      real     p0
     +        ,pi(klevi)
     +        ,psfc(mlon,nlat)
     +        ,xi(mlon,nlat,klevi)
     +        ,hyao(klevo), hybo(klevo)
c                                                 ! output
      real     xo(mlon,nlat,klevo) 
C NCLEND
c                                                 ! local (automatic)
      real     po(klevo)
      integer  iflag
      iflag = 0

      call p2hyox (pi,mlon,nlat,klevi,xi
     +             ,psfc,p0,hyao,hybo,klevo,xo, po,iflag)
      return
      end

C NCLFORTSTART
      subroutine p2hyox (pi,mlon,nlat,klevi,xi
     +                  ,psfc,p0,hyao,hybo,klevo,xo, po,iflag)
      implicit none
      integer  mlon, nlat, klevi, klevo, iflag
      real     p0
     +        ,hyai(klevi), hybi(klevi)
     +        ,hyao(klevo), hybo(klevo)
     +        ,psfc(mlon,nlat)
     +        ,xi(mlon,nlat,klevi)
     +        ,pi(klevi), po(klevo)
c                                                 ! output
      real     xo(mlon,nlat,klevo) 
C NCLEND
c                                                 ! local
      integer nl, ml, ki, ko
c f77
      do nl=1,nlat
       do ml=1,mlon

         do ko=1,klevo
            po(ko) = hyao(ko)*p0 + hybo(ko)*psfc(ml,nl)
         end do

         do ko=1,klevo
            xo(ml,nl,ko) = 1.e20
           do ki=1,klevi-1
              if (pi(ki).le.psfc(ml,nl)) then
                  if (po(ko).ge.pi(ki) .and. po(ko).lt.pi(ki+1)) then 
                      xo(ml,nl,ko) = xi(ml,nl,ki)
     +                             + (xi(ml,nl,ki+1)-xi(ml,nl,ki))
     +                             *(alog(po(ko))  -alog(pi(ki)))
     +                              /(alog(pi(ki+1))-alog(pi(ki)))
                  else
                      iflag = 1
                  end if
              end if
           end do
         end do

       end do
      end do

      return
      end
