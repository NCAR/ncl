C NCLFORTSTART
        subroutine wetbulbprofs(npts,t,td,p,twb,tmsg,tdmsg,pmsg,ier)
        implicit none
c                                      INPUT
        integer ier, npts
        double precision t(npts) 
        double precision td(npts) 
        double precision p(npts) 
        double precision tmsg, tdmsg, pmsg
c                                      OUTPUT
        double precision twb(npts) 
c NCLEND
c                                      LOCAL
        integer n
        external twbprofs
        double precision twbprofs

c       g.s. stipanuk     1973            original version.
c       reference stipanuk paper entitled:
c            "algorithms for generating a skew-t, log p
c            diagram and computing selected meteorological quantities."
c            atmospheric sciences laboratory
c            u.s. army electronics command
c            white sands missile range, new mexico 88002 : 33 pages
c       baker, schlatter  17-may-1982   

c PROFS: Program for Regional Observing and Forecasting Services (NOAA)

        do n=1,npts
           if (t(n).ne.tmsg.and.td(n).ne.tdmsg.and.p(n).ne.pmsg) then
               twb(n) = twbprofs(t(n),td(n),p(n))
           else
               twb(n) = tmsg
           end if
        end do

c Note: ier isn't used for anything yet, so set to 0 here.
        ier = 0
        return
        end
c---
        double precision function twbprofs(t,td,p)
        implicit none
c                                      INPUT
        double precision t 
        double precision td 
        double precision p 
c                                      LOCAL 
        integer i
        double precision aw, ao, pi, x, ti, aos, eps

        external          wxprofs, oxprofs, tmrprofs, tmaprofs, tdaprofs
     +                 ,  osprofs, tsaprofs
        double precision  wxprofs, oxprofs , tmrprofs, tmaprofs,tdaprofs
     +                 ,  osprofs, tsaprofs

c   this function returns the wet-bulb temperature tw (celsius)
c   given the temperature t (celsius), dew point td (celsius)
c   and pressure p (mb).  see p.13 in stipanuk (1973), referenced
c   above, for a description of the technique.
c
c   determine the mixing ratio line thru td and p.

        aw = wxprofs(td,p)
c
c   determine the dry adiabat thru t and p.

        ao = oxprofs(t,p)     

c   iterate to locate pressure pi at the intersection of the two
c   curves .  pi has been set to p for the initial guess.

        eps = 0.01d0
        pi  = p
        do i= 1,10
           x= 0.02d0*(tmrprofs(aw,pi)-tdaprofs(ao,pi))
           if (abs(x).lt.eps) exit    
           pi= pi*(2**(x))
        end do

c   find the temperature on the dry adiabat ao at pressure pi.

        ti = tdaprofs(ao,pi)

c   the intersection has been located...now, find a saturation
c   adiabat thru this point. function os returns the equivalent
c   potential temperature (c) of a parcel saturated at temperature
c   ti and pressure pi.

        aos = osprofs(ti,pi)

c   function tsa returns the wet-bulb temperature (c) of a parcel at
c   pressure p whose equivalent potential temperature is aos.

        twbprofs = tsaprofs(aos,p)

        return
        end
c ---------------------------------------------------------------------

        double precision function oxprofs(t,p)
        implicit none
c                                      INPUT
        double precision t, p
c                                      LOCAL
        double precision  tk 

c   this function returns potential temperature (celsius) given
c   temperature t (celsius) and pressure p (mb) by solving the poisson
c   equation.

        tk = t  + 273.15d0
        oxprofs = tk*((1000d0/p)**0.286) - 273.15d0

        return
        end
c ---------------------------------------------------------------------

        double precision function wxprofs(t,p)
        implicit none
c                                      INPUT
        double precision t, p
c                                      LOCAL
        double precision x

        external esatprofs
        double precision esatprofs

c  this function returns the mixing ratio (grams of water vapor per
c  kilogram of dry air) given the dew point (celsius) and pressure
c  (millibars). if the temperture  is input instead of the
c  dew point, then saturation mixing ratio (same units) is returned.
c  the formula is found in most meteorological texts.

        x       = esatprofs(t)
        wxprofs = 622d0*x/(p-x)

        return
        end
c ---------------------------------------------------------------------

        double precision function esatprofs(t)
        implicit none
c                                      INPUT
        double precision t

c   this function returns the saturation vapor pressure over
c   water (mb) given the temperature (celsius).
c   the algorithm is due to nordquist, w.s.,1973: "numerical approxima-
c   tions of selected meteorlolgical parameters for cloud physics prob-
c   lems," ecom-5475, atmospheric sciences laboratory, u.s. army
c   electronics command, white sands missile range, new mexico 88002.

c                                      LOCAL
        double precision tk, p1, p2, c1

        tk = t+273.15d0
        p1 = 11.344d0-0.0303998d0*tk
        p2 = 3.49149d0-1302.8844d0/tk
        c1 = 23.832241d0-5.02808d0*log10(tk)

        esatprofs = 10.**(c1-1.3816d-7*10.**p1
     +                      +8.1328d-3*10.**p2-2949.076d0/tk)

        return
        end
c ---------------------------------------------------------------------
        double precision function tsaprofs(os,p)
        implicit none
c                                      INPUT
        double precision os, p

c   this function returns the temperature tsa (celsius) on a saturation
c   adiabat at pressure p (millibars). os is the equivalent potential
c   temperature of the parcel (celsius). sign(a,b) replaces the
c   algebraic sign of a with that of b.
c   b is an empirical constant approximately equal to 0.001 of the 
c   latent heat of vaporization for water divided by the specific heat
c   at constant pressure for dry air.
c                                      LOCAL
        integer i
        double precision a, b, d, tq, tqk, x, eps 
        data b/2.6518986d0/
       
        external wxprofs
        double precision wxprofs

        a  = os+273.15d0

c   tq is the first guess for tsa.

        tq = 253.15d0

c   d is an initial value used in the iteration below.

        d = 120d0

c   iterate to obtain sufficient accuracy....see table 1, p.8
c   of stipanuk (1973) for equation used in iteration.

        eps = 1d-1
        do 1 i= 1,12
           tqk = tq-273.15d0
           d   = d/2d0
           x   = a*exp(-b*wxprofs(tqk,p)/tq)-tq*((1000d0/p)**.286)
           if (abs(x).lt.eps) go to 2
           tq  = tq+sign(d,x)
 1      continue

 2      tsaprofs= tq-273.15d0

        return
        end
c -------------------------------------------------------------------

        double precision function tdaprofs(o,p)
        implicit none
c                                      INPUT
        double precision o, p
c                                      LOCAL
        double precision ok, tdak
 
c   this function returns the temperature tda (celsius) on a dry adiabat
c   at pressure p (millibars). the dry adiabat is given by
c   potential temperature o (celsius). the computation is based on
c   poisson's equation.
 
        ok       = o+273.15d0
        tdak     = ok*((p*0.001d0)**0.286)
        tdaprofs = tdak-273.15d0

        return
        end

c -------------------------------------------------------------------

        double precision function osprofs(t,p)
        implicit none
c                                      INPUT
        double precision t, p

c   this function returns the equivalent potential temperature os
c   (celsius) for a parcel of air saturated at temperature t (celsius)
c   and pressure p (millibars).

c                                      LOCAL
c   b is an empirical constant approximately equal to the latent heat
c   of vaporization for water divided by the specific heat at constant
c   pressure for dry air.

        double precision b, tk, osk
        data b/2.6518986d0/

        external wxprofs
        double precision wxprofs

        tk      = t+273.15d0
        osk     = tk*((1000d0/p)**.286)*(exp(b*wxprofs(t,p)/tk))
        osprofs = osk-273.15d0

        return
        end
c ---------------------------------------------------------------------
 
        double precision function tmrprofs(w,p)
        implicit none
c                                      INPUT
        double precision w, p
c                                      LOCAL
        double precision c1, c2, c3, c4, c5, c6, x, tmrk
 
c   this function returns the temperature (celsius) on a mixing
c   ratio line w (g/kg) at pressure p (mb). the formula is given in
c   table 1 on page 7 of stipanuk (1973).
c
c   initialize constants
 
        data c1/.0498646455/,c2/2.4082965/,c3/7.07475/
        data c4/38.9114/,c5/.0915/,c6/1.2035/
 
        x    = log10(w*p/(622d0+w))
        tmrk = 10.**(c1*x+c2)-c3+c4*((10.**(c5*x)-c6)**2.)
        tmrprofs = tmrk-273.15d0

        return
        end

