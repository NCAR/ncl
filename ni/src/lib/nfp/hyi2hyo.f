C NCLFORTSTART
      subroutine hyi2hyoa (p0,hyai,hybi,psfc,mlon,nlat,klevi,xi
     +                    ,hyao,hybo,klevo,xo)
      implicit none
 
c NCL: xo = hybrid2hybrid (p0,hyai,hybi,psfc,xi,  hyao,hybo)

c this routine interploates one hybrid level to another
c     formula for the pressure of a hybrid surface is;
c          phy(k) = hya(k)*p0 + hyb(k)*psfc

c     input  ["i" input ... "o" output]
c          hyai   - is the "a" or pressure hybrid coef [input]
c          hybi   - is the "b" or sigma coeficient     [input]
c          p0     - is the base pressure in Pa         [input]
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
     +        ,hyai(klevi), hybi(klevi)
     +        ,hyao(klevo), hybo(klevo)
     +        ,psfc(mlon,nlat)
     +        ,xi(mlon,nlat,klevi)
c                                                 ! output
      real     xo(mlon,nlat,klevo) 
C NCLEND
c                                                 ! local (automatic)
      real     pi(klevi), po(klevo)

      call hyi2hyob (p0,hyai,hybi,psfc,mlon,nlat,klevi,xi
     +              ,hyao,hybo,klevo,xo, pi,po)
      return
      end

C NCLFORTSTART
      subroutine hyi2hyob (p0,hyai,hybi,psfc,mlon,nlat,klevi,xi
     +                    ,hyao,hybo,klevo,xo, pi,po)
      implicit none
      integer  mlon, nlat, klevi, klevo
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

         do ki=1,klevi
            pi(ki) = hyai(ki)*p0 + hybi(ki)*psfc(ml,nl)
         end do

         do ko=1,klevo
            po(ko) = hyao(ko)*p0 + hybo(ko)*psfc(ml,nl)
         end do

         do ko=1,klevo
           do ki=1,klevi-1
              if (po(ko).ge.pi(ki) .and. po(ko).lt.pi(ki+1)) then 
                  xo(ml,nl,ko) = xi(ml,nl,ki)
     +                         + (xi(ml,nl,ki+1)-xi(ml,nl,ki))
     +                          *(alog(po(ko))  -alog(pi(ki)))
     +                          /(alog(pi(ki+1))-alog(pi(ki)))
              end if
           end do
         end do

       end do
      end do

      return
      end
