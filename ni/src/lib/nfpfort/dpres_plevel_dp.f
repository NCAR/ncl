C NCLFORTSTART
      subroutine dpresplvl(klvl,plevel
     +                    ,ntim,nlat,mlon,psfc,pmsg,ptop,dp,iopt,ier)
      implicit none
C                                                ! input 
      integer  klvl,ntim,nlat,mlon,iopt,ier
      double precision     plevel(klvl), psfc(mlon,nlat,ntim),ptop,pmsg
C                                                ! output
      double precision     dp(mlon,nlat,klvl,ntim)
C NCLEND
c     dpres = dpres_plevel(plevel, psfc, ptop, iopt)
c
c constant pressure level equivalent of dpres_hybrid_ccm
c
c The returned 'dp' are equivalent to having used  beta factors.
 
C                                                ! local
      integer mono, nt, kl, nl, ml, kll
      double precision    plvl(klvl), dplvl(klvl), pbot, pspan, peps
      double precision    dpsum, psfcmx, psfcmn, work(klvl)
      data    peps /0.001d0/

c
c check to see if ptop is reasonable
c .   ptop < 0      is not allowed
c .   ptop > psfcmx is probably due to units difference

      ier    = 0
      if (ptop.lt.0.0d0) then
          ier = 1
          return
      end if

      psfcmn =  1d20
      psfcmx = -1d20

      do nt=1,ntim
        do nl=1,nlat
          do ml=1,mlon
             if (psfc(ml,nl,nt).ne.pmsg) then
c c c            if (psfc(ml,nl,nt).lt.psfcmn) psfcmn = psfc(ml,nl,nt)
                 if (psfc(ml,nl,nt).gt.psfcmx) psfcmx = psfc(ml,nl,nt)
             end if
          end do
        end do
      end do
      
      if (ptop.ge.psfcmx) ier = 10
      if (ier.ne.0) return

c monotonically increasing or decreasing?
c if decreasing pressure make increasing; then flip back 

      if (plevel(2).gt.plevel(1)) then
          mono =  1
          do kl=1,klvl
             plvl(kl) = plevel(kl)
          end do
      else
          mono = -1
          do kl=1,klvl
             plvl(kl) = plevel(klvl-kl+1)
          end do
      end if


c initialize to nominal dplvl

      dplvl(1) = (plvl(1)+plvl(2))*0.5d0 - ptop
      do kl=2,klvl-1 
         dplvl(kl)= 0.5d0*(plvl(kl+1) - plvl(kl-1))
      end do
      dplvl(klvl) = psfcmx -(plvl(klvl)+plvl(klvl-1))*0.5d0

c levels outside the range should be ste to 0.0

      do kl=1,klvl 
         if (plvl(kl).lt.ptop  ) dplvl(kl) = 0.0d0
         if (plvl(kl).gt.psfcmx) dplvl(kl) = 0.0d0
      end do

      do nt=1,ntim
        do kl=1,klvl
          do nl=1,nlat
            do ml=1,mlon
               dp(ml,nl,kl,nt) = dplvl(kl)
            end do
          end do
        end do
      end do

c modify the default dp

      do nt=1,ntim
        do nl=1,nlat
          do ml=1,mlon
             if (psfc(ml,nl,nt).eq.pmsg) then
                 do kl=1,klvl
                    dp(ml,nl,kl,nt) = 0.0d0
                 end do
                 go to 300
             else

             if (ptop.gt.0.0d0) then
                 do kl=1,klvl-1
                    if (ptop.ge.plvl(kl) .and. ptop.lt.plvl(kl+1)) then
                       dp(ml,nl,kl,nt) =(plvl(kl)+plvl(kl+1))*0.5d0-ptop
                       go to 100
                    end if
                 end do
             end if

  100        pbot  = psfc(ml,nl,nt)
             if (pbot.gt.plvl(klvl)) then
                 dp(ml,nl,klvl,nt) = pbot -(plvl(klvl-1)+plvl(klvl))*0.5d0
             else
                 do kl=1,klvl-1
                    if (pbot.ge.plvl(kl) .and. pbot.lt.plvl(kl+1)) then
                        dp(ml,nl,kl,nt)=pbot-(plvl(kl)+plvl(kl-1))*0.5d0
                        do kll=(kl+1),klvl
                           dp(ml,nl,kll,nt) = 0.0d0
                        end do
                        go to 200
                    end if
                 end do
             end if

  200        pspan = pbot-ptop

c check to make sure:     sum(dp)=pspan
c peps could me much smaller [1e-5]
c should probably be a fatal error .... indicates something is wrong

             dpsum = 0.0d0
             do kl=1,klvl
                dpsum = dpsum + dp(ml,nl,kl,nt)
             end do
             if (dpsum.gt.(pspan+peps) .or. dpsum.lt.(pspan-peps) ) then
                 ier = ier-1
             end if

c if necessary return to original order [reuse dplvl]

             if (mono.lt.0) then
                 do kl=1,klvl
                    work(kl) = dp(ml,nl,kl,nt)
                 end do

                 do kl=1,klvl
                    dp(ml,nl,kl,nt) = work(klvl-kl+1)
                 end do
             end if

           end if 
  300      continue
          end do
        end do
      end do


      return
      end
