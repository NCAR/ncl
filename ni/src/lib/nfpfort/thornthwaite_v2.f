C NCLFORTSTART
      subroutine thorn2(t,ntim,tmsg,lat,etp,ier)
      implicit none
C                                        INPUT
      integer ntim, ier
      double precision t(ntim), lat, tmsg
C                                        OUTPUT
      double precision etp(ntim)
C NCLEND
C --------------------------------------------------------------------
C Computation of evapotranspiration using the method of Thornthwaite
C --------------------------------------------------------------------
C                       This allows missing values 
C --------------------------------------------------------------------
C Original C-code: http://sac.csic.es/spei/spei_index.html
C --------------------------------------------------------------------
C                                        LOCAL
      integer          nt, nmo, nmos, nyrs, knt
      double precision J, J2, J3, c, N, K(12), pi, rad, tmo
      double precision tanlat, tanLatMonth, omega
     +              ,  tanDelta(12), days(12), julian(12)

      data days     /  31,28,31,30,31,30,31,31,30,31,30,31/
      data julian   /  16, 45.5, 75  ,105.5,136  ,166.5
     +              , 197,228  ,258.5,289  ,319.5,350  /
      data pi       /3.14159265358979d0/
      data rad      /0.0174532925d0/

C  average solar declination angle for each month of the year

      data tanDelta / -0.37012566,-0.23853358,-0.04679872, 0.16321764

     +              ,  0.32930908, 0.40677729, 0.3747741 , 0.239063
     +              ,  0.04044485,-0.16905776,-0.33306377,-0.40743608 /

      nmos = 12
      nyrs = ntim/nmos

      do nt=1,ntim
         etp(nt) = tmsg   
      end do

      ier  = 0
      if (mod(ntim,nyrs).ne.0) then
          ier = 1
          return
      end if

c calculate climatological monthly temperatures and use these
c .   to calculate a climatological annual temp. efficiency index (J)

      J   = 0.0d0
      do nmo=1,nmos
         knt = 0
         tmo = 0.0d0
c                               ! every 12-th value (eg: all Januaries)
        do nt=nmo,ntim,nmos  
           if (t(nt).ne.tmsg) then
               tmo = tmo + t(nt)
               knt = knt + 1
           end if
        end do
c                               ! monthly climatology
         if (knt.gt.0) then
             tmo = tmo/knt
             if (tmo.gt.0.0d0) J = J + (tmo/5d0)**1.514
         end if
      end do
c                               ! if J=0 ... nothing else matters
      if (J.eq.0.0d0) then
          do nt=1,ntim
             if (t(nt).ne.tmsg) then
                 etp(nt) = 0.0d0   
             end if
          end do
          return
      end if

C Compute exponent (c)

      J2 = J*J
      J3 = J*J2
      c  = 0.000000675d0*J3 - 0.0000771d0*J2 + 0.01792d0*J + 0.49239d0

C Compute K: monthly correction factor which is a function of latitude

       tanLat = tan(rad*lat)
       do nmo=1,nmos
          tanLatMonth = tanLat*tanDelta(nmo) 
          if (tanLatMonth.gt.-1.0d0 .and. tanLatMonth.lt.1.0d0) then
              omega = acos(-tanLatMonth)
          elseif (tanLatMonth.le.-1.0d0) then
               omega = 0
          else
               omega = pi
          end if
          N = 24d0*omega/pi    !  monthly average number of sun hours
          K(nmo) = (N/12d0)*days(nmo)/30d0
       end do

C Calculate evapotranspiration

        do nmo=1,nmos
          do nt=nmo,ntim,nmos
             if (t(nt).ne.tmsg) then
                 if (t(nt).gt.0.0d0) then
                     etp(nt) = K(nmo)*16*((10*t(nt)/J))**c 
                 else
                     etp(nt) = 0.0d0
                 end if
             end if
          end do
        end do
        
      end
