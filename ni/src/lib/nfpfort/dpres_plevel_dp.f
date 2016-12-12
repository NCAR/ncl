C NCLFORTSTART
      subroutine dpresplvl(klvl,plevel,ntim,nlat,mlon,psfc,pmsg
     +                    ,ptop,dp,iopt,kflag,ier)
      implicit none
C                                                ! input 
      integer  klvl,ntim,nlat,mlon,iopt,kflag,ier
      double precision     plevel(klvl), psfc(mlon,nlat,ntim),ptop,pmsg
C                                                ! output
      double precision     dp(mlon,nlat,klvl,ntim)
C NCLEND
c     dpres = dpres_plevel(plevel, psfc, ptop, iopt)
c
      integer nt, ml, nl, kl, jer, kloop 
      double precision  dplvl(klvl)
cdebugdouble precision  dpsum, pdif   

      kflag = 0
      kloop = -999

c loop over all grid points

      do nt=1,ntim
        do nl=1,nlat
          do ml=1,mlon

c calculate vertical 'dp' at current grid point

             call dpres1d(klvl,plevel,psfc(ml,nl,nt),pmsg
     +                    ,ptop,dplvl,iopt,kloop,ier)
             if (kloop.eq.1) then
                 kflag = 1
             end if

c transfer to return array

             do kl=1,klvl
                dp(ml,nl,kl,nt) = dplvl(kl)
             end do

cdebug       dpsum = sum(dplvl, 1, dplvl.ne.pmsg)   ! f90 array function
cdebug       pdif  = psfc(ml,nl,nt)-ptop
cdebug       print *,"dpsum=", dpsum,"  pdif=",pdif

          end do
        end do
      end do

      return
      end
c ------------------------------------------------------------------------
C NCLFORTSTART
      subroutine dpres1d(klvl,plevel,psfc,pmsg,ptop,dp,iopt,kflag,ier) 
      implicit none
C                                                ! input 
      integer              klvl, iopt, kflag, ier
      double precision     plevel(klvl), psfc, ptop, pmsg
C                                                ! output
      double precision     dp(klvl)
C NCLEND
c     dpres = dpres_plevel(plevel, psfc, ptop, iopt)
c
c isobaric (constant) pressure level equivalent of dpres_hybrid_ccm
c The returned 'dp' are equivalent to having used  beta factors.
 
C                                                ! local
      integer             mono, kl, klStrt, klLast
      double precision    plvl(klvl), work(klvl)
      double precision    dpsum, pspan, peps, plow, phi

      peps  = 0.001d0
c
c check to see if ptop is reasonable
c .   psfc = pmsg   is not allowed
c .   ptop < 0      is not allowed
c .   ptop > psfcmx is probably due to units difference

      ier    = 0
      if (psfc.eq.pmsg .or. psfc.lt.0.0d0) ier = ier + 100
      if (ptop.lt.0.0d0) ier = ier + 1
      if (ptop.ge.psfc)  ier = ier + 10

c if ier.ne.0; input error with psfc and/or ptop

      if (ier.ne.0) then   
          kflag = 1
          do kl=1,klvl 
             dp(kl)   = pmsg
          end do
          dpsum = pmsg
          return
      end if

c monotonically increasing or decreasing? Code wants top to bottom
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

c initialize to missing

      do kl=1,klvl
         dp(kl) = pmsg                 
      end do

c calculate 'dp'; check if dpsum.eq.(psfc-ptop) within peps then return

      if (ptop.le.plvl(1) .and. psfc.ge.plvl(klvl)) then
          kflag = 0

          dp(1) = (plvl(1)+plvl(2))*0.5d0 - ptop
          do kl=2,klvl-1 
             dp(kl)= 0.5d0*(plvl(kl+1) - plvl(kl-1))
          end do
          dp(klvl) = psfc -(plvl(klvl)+plvl(klvl-1))*0.5d0

      else 
          kflag  = 1

          klStrt = 1
          if (ptop.ge.plvl(1)) then
             do kl=1,klvl 
                if (ptop.le.plvl(kl)) then
                    klStrt = kl
                    exit
                end if
             end do
          end if

          klLast = klvl
          if (psfc.le.plvl(klvl)) then
             do kl=1,klvl 
                if (psfc.le.plvl(kl)) then
                    klLast = kl-1
                    exit
                end if
             end do
          end if

cdebugprint *,"klStrt=",klStrt," klLast=",klLast," ptop=",ptop
cdebugprint *,"plvl(klStrt)=",plvl(klStrt)," plvl(klLast)=",plvl(klLast)
cdebugprint *,"plvl(klStrt  )=",plvl(klStrt)  
cdebugprint *,"plvl(klStrt+1)=",plvl(klStrt+1)
cdebugprint *,"dp(klStrt)=",dp(klStrt)

          if (klStrt.eq.klLast) then
              dp(klStrt) = psfc-ptop
          elseif (klStrt.lt.klLast) then
              dp(klStrt) = (plvl(klStrt)+plvl(klStrt+1))*0.5d0 - ptop
              do kl=klStrt+1,klLast-1 
                 dp(kl)= 0.5d0*(plvl(kl+1) - plvl(kl-1))
              end do
              dp(klLast) = psfc -(plvl(klLast)+plvl(klLast-1))*0.5d0

c c c     else     ! klStrt>klLast  is a pathological case
c c c              ! plev(?)=500  plev(?+1)=550, psfc=540, ptop=510
c c c              ! both level are *between* levels
          end if

      end if

c error check

c f90 dpsum = sum(dplvl, 1, dplvl.ne.pmsg)  
      dpsum = 0.0d0
      do kl=1,klvl
         if (dp(kl).ne.pmsg) then
             dpsum = dpsum + dp(kl)
         end if
      end do

      pspan = psfc-ptop

      if (dpsum.gt.0.0d0 .and. pspan.ne.dpsum) then
          plow = pspan-peps 
          phi  = pspan+peps 
          if (dpsum.gt.phi .or. dpsum.lt.plow) then
              ier = -1
              do kl=1,klvl
                 dp(kl) = pmsg                 
              end do
          end if
      end if

c if necessary return to original order

      if (mono.lt.0) then
          do kl=1,klvl
             work(kl) = dp(kl)
          end do

          do kl=1,klvl
             dp(kl) = work(klvl-kl+1)
          end do
       end if

      return
      end
