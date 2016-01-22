;===============================================================================
; !MODULE:   HumanIndexMod
; !CITATION: Buzan, J. R., Oleson, K., and Huber, M.: Implementation and
;            comparison of a suite of heat stress metrics within the
;            Community Land Model version 4.5, Geosci. Model Dev., 8,
;            151-170, doi:10.5194/gmd-8-151-2015, 2015.
;
; !Some (not all) functions based on HumanIndexMod.f90
;===============================================================================
; An interactive URL for 'heat' NCAR's "Extreme Heat Climate Inspector"
;             http://gis.ucar.edu/heatinspector
; The Extreme Heat Climate Inspector is an interactive web application which 
; expands GIS mapping and graphing capabilities to visualize projected heat. 
; The data displayed in this app were produced at NCAR for the NASA-funded study: 
;     "System for Integrated Modeling of Metropolitan Extreme heat Risk" (SIMMER). 
;===============================================================================
undef("wetbulb_stull")
function wetbulb_stull(t:numeric, rh:numeric, iounit[2]:integer,  opt[1]:logical)
;
; **Generally, it is best to use NCL's 'wetbulb' or 'wetbulb_Wrap' functions**
;
; HumanIndexModf90:  subroutine Wet_BulbS (Tc_6,rh,wbt)
;
; This function is only applicable at or near standard sea level pressure [ 1013.25 hPa}
; Hence, it may be of limited use.
;
; Reference:  Stull, R., 2011: Wet-bulb temperature from relative humidity
;             and air temperature, J. Appl. Meteor. Climatol.
;             http://journals.ametsoc.org/doi/pdf/10.1175/JAMC-D-11-0143.1
;
;             Note: Requires air temperature (C) and relative humidity (%)
;                   Pressure is assumed to be 1013.25 hPa 
; 
; Quote from abstract:
; The equation was found as an empirical fit using gene-expression programming. 
; This equation is valid for relative humidities between 5% and 99% and for 
; air temperatures between -20C and 50C, except for situations having both 
; low humidity and cold temperature. Over the valid range, errors in wet-bulb 
; temperature range from -1C to 0.65C, with mean absolute error of less than 0.3.
;
; J.R. Buzan email comment 17 Nov 2015
; The Stull wet bulb temperature has issues when the temperature gets high. 
; For example, when temperature approaches 40°C, the Stull wet bulb temperature 
; is over estimating the wet bulb temperature by 1°C (Figure 1b in Buzan et al, 2015).  
; Why this matters is that a human can go from hard labor to no labor at all 
; within a ~2°C change in wet bulb temperature (figure 2 in Liang et al, 2011).  
;
; Liang, C., G. Zheng, N. Zhu, Z. Tian, S. Lu, and Y. Chen (2011), 
; A new environmental heat stress index for indoor hot and humid environments 
; based on Cox regression, Building and Environment, 46(12), 2472–2479 
; doi:10.1016/j.buildenv.2011.06.013

local twets, tc, tk0, newline
begin
   if (all(rh.le.1)) then
       print("wetbulb_stull: argument are all rh <= 1: wrong units")
       exit 
   end if

   tk0 = 273.15

   if (iounit(0).eq.0) then
       twets = t*atan(0.151977*sqrt(rh + 8.313659)) \
             + atan(t + rh) - atan(rh - 1.676331)   \
             + 0.00391838*(rh^(1.5))*atan(0.023101*rh) - 4.686035

   else if (iounit(0).eq.1) then
       tc    = t - tk0
       twets = tc*atan(0.151977*sqrt(rh + 8.313659)) \
             + atan(tc + rh) - atan(rh - 1.676331)   \
             + 0.00391838*(rh^(1.5))*atan(0.023101*rh) - 4.686035
      
   else if (iounit(0).eq.2) then
       tc    = 0.555556*(t-32)        
       twets = tc*atan(0.151977*sqrt(rh + 8.313659)) \
             + atan(tc + rh) - atan(rh - 1.676331)   \
             + 0.00391838*(rh^(1.5))*atan(0.023101*rh) - 4.686035

   end if  ; end iounit(0)=2
   end if  ; end iounit(0)=1
   end if  ; end iounit(0)=0

   twets@long_name = "Wet Bulb Temperature via Stull"

   if (iounit(1).eq.0) then
       twets@units = "degC"
   else if (iounit(1).eq.1) then
       twets = twets + tk0
       twets@units = "degK"
   else if (iounit(1).eq.2) then
       twets = 1.8*twets+32
       twets@units = "F"
   end if
   end if
   end if

   twets@reference = "http://journals.ametsoc.org/doi/pdf/10.1175/JAMC-D-11-0143.1"

   newline    = str_get_nl()
   twets@info =  newline + \
    "Equation used is valid for relative humidities between 5% and 99%    "+ newline + \ 
    "and for air temperatures between -20C and 50C, except for situations "+ newline + \
    "having both low humidity and cold temperatures. Over the valid range,"+ newline + \
    "errors in wet-bulb temperature range from -1C to 0.65C, with mean    "+ newline + \
    "absolute error of less than 0.3.                                     "+ newline

   copy_VarCoords(t, twets)

   return(twets)
   end
    
;===============================================
undef("heat_wbgt_inout")
function heat_wbgt_inout(tw:numeric, tg:numeric, ta:numeric, iounit[2]:integer,  opt[1]:integer)
;
; https://en.m.wikipedia.org/wiki/Wet_Bulb_Globe_Temperature
; http://www.srh.noaa.gov/tsa/?n=wbgt
;
; NOTE: 'tg' is rarely available. This limits use of this function.
;
; The WBGT is the ISO (1989) standard for quantifying thermal comfort.
; Reference: 
;   Hot Environments—Estimation of the heat stress on working man, 
;   based on the WBGT-index (wet bulb globe temperature). 
;   ISO Standard 7243. Geneva: International Standards Organization.
;
; The wet-bulb globe temperature (WBGT) is a composite temperature used 
; to estimate the effect of temperature, humidity, wind speed (wind chill), 
; and visible and infrared radiation (usually sunlight) on humans. It is 
; used by industrial hygienists, athletes, and the military to determine 
; appropriate exposure levels to high temperatures.
; 
; The following is from http://afcintl.com/pdfs/3M%20pdf/wbgt.pdf
; Per guidance published by the Occupational Safety and Health 
; Administration (OSHA), the outdoor, with solar load, WBGT Index 
; is a weighted sum of three component temperatures:  
;
; + A globe temperature (tg) , measured by a thermometer inside a 
;   black sphere passively exposed to the ambient environment  The globe
;   temperature provides an indication of the mean radiant temperature 
;   of the environment and accounts for 20% of the WBGT Index.
;
; + A natural wet bulb temperature (tw), measured by a thermometer 
;   bearing a wetted wick passively exposed to the ambient environment.  
;   The wet bulb temperature indicates the amount of cooling provided 
;   to the human subject through evaporation and accounts for 70% of 
;   the WBGT index.
;
; + A dry bulb temperature (ta), measured by a standard air thermometer.
;   The dry bulb temperature is the temperature of the ambient air 
;   and accounts for 10% of the WBGT Index.
;
; Mochida ey al (2007): 
; Derivation and analysis of the indoor Wet Bulb Globe Temperature index 
;      (WBGT) with a human thermal engineering approach:
;       Part 1. Properties of the WBGT formula for indoor conditions 
;               with no solar radiation
; http://www.inive.org/members_area/medias/pdf/Inive/clima2007/A04/A04D1261.pdf
; suggest that 0.85*tw + 0.20*tg are better.
;
local coef, wbgt, tk0, twc, tgc, tac
begin
  tk0 = 273.15

  if (opt.lt.0 .or. opt.gt.2) then
      print("heat_wbgt_inout: illegal opt value: opt="+opt)
      exit
  end if

  if (opt.eq.0 .or. opt.eq.1) then      ; INDOOR
      if (opt.eq.0) then
          coef = (/ 0.70, 0.30 /)
      else if (opt.eq.1) then
          coef = (/ 0.85, 0.20 /)
      end if
      end if

      if (iounit(0).eq.0) then                  ; input C
          wbgt = coef(0)*tw + coef(1)*tg 
      else if (iounit(1).eq.1) then             ; input K
          wbgt = coef(0)*(tw-tk0) + coef(1)*(tg-tk0) 
      else if (iounit(0).eq.2) then             ; input F
          twc  = 0.555556*(tw-32)        
          tgc  = 0.555556*(tg-32)        
          wbgt = coef(0)*twc + coef(1)*tgc 
      end if   ; F
      end if   ; K
      end if   ; C

      wbgt@long_name = "WBGT: wet-bulb globe temperature"
      wbgt@info      = "Indoors, or when solar radiation is negligible"
      
  else                                          ; OUTDOOR
      coef = (/ 0.70, 0.20, 0.10 /)
      if (iounit(0).eq.0) then                  ; input C
          wbgt = coef(0)*tw + coef(1)*tg + coef(2)*ta 
      else if (iounit(1).eq.1) then             ; input K
          wbgt = coef(0)*(tw-tk0) + coef(1)*(tg-tk0) + coef(2)*(ta-tk0) 
      else if (iounit(0).eq.2) then             ; input F
          twc  = 0.555556*(tw-32)        
          tgc  = 0.555556*(tg-32)        
          tac  = 0.555556*(ta-32)        
          wbgt = coef(0)*twc + coef(1)*tgc + coef(2)*tac 
      end if   ; F
      end if   ; K
      end if   ; C
      wbgt@long_name = "WBGT: wet-bulb globe temperature"
      wbgt@info      = "Outdoor"
  end if

  if (iounit(1).eq.0) then
      wbgt@units = "degC"
  else if (iounit(1).eq.1) then
      wbgt       = wbgt + tk0
      wbgt@units = "degK"
  else if (iounit(1).eq.2) then
      wbgt       = 1.8*wbgt+32
      wbgt@units = "F"
  end if    ; F
  end if    ; K
  end if    ; C

  wbgt@coef =  coef
  wbgt@NCL  = "heat_wbgt_globe"

  copy_VarCoords(tw, wbgt)
  return(wbgt)
end
    
;===============================================
undef("heat_wbgt_simplified")
function heat_wbgt_simplified(t:numeric, vp:numeric, iounit[2]:integer,  opt[1]:logical)
;
; HumanIndexMod.f90:  subroutine swbgt (Tc_2, vap_pres, s_wbgt)
;
; Designed for estimating heat stress in sports medicine.
;
; Reference:  Willett, K.M., and S. Sherwood, 2010: Exceedance of heat
;             index thresholds for 15 regions under a warming 
;             climate using the wet-bulb globe temperature,
;             Int. J. Climatol., doi:10.1002/joc.2257
;
; The WBGT is the ISO (1989) standard for quantifying thermal comfort.
; Reference: 
;   Hot Environments—Estimation of the heat stress on working man, 
;   based on the WBGT-index (wet bulb globe temperature). 
;   ISO Standard 7243. Geneva: International Standards Organization.
;
; vp - vapor pressute (Pa)
;
;
local swbgt, tk0, con, conp, c1
begin
  tk0 = 273.15
  con = (/ 3.94, 0.393, 0.567 /)            ; 0,1,2

  if (iounit(1).eq.0) then                  ; consytant to convert pressure
      conp = 100.
  else if (iounit(1).eq.1) then
      conp = 1.
  else if (iounit(1).eq.2) then
      conp = 10.
  end if  ; 2 => kPa
  end if  ; 1 =>  Pa
  end if  ; 0 => hPa

  c1  = (con(1)/100)*conp

  if (iounit(0).eq.0) then                  ; input C
      swbgt = con(2)*t + c1*vp + con(0)  
  else if (iounit(0).eq.1) then             ; input K
      swbgt = con(2)*(t-tk0) + c1*vp + con(0)
  else if (iounit(0).eq.2) then             ; input F
      c2    = con(2)*0.555556
      swbgt = c2*(t-32) + c1*vp + con(0)
  end if   ; F
  end if   ; K
  end if   ; C

  swbgt@long_name = "simplified wet-bulb index"
 ;swbgt@units = ""                          ; unitless
  swbgt@NCL   = "heat_wbgt_simplified"

  copy_VarCoords(t, swbgt)
  return(swbgt)
end

;===============================================
undef("heat_apptemp")
function heat_apptemp(t:numeric, vp:numeric, w10:numeric, iounit[3]:integer, opt:logical)
;
; HumanIndexMod.f90:  subroutine AppTemp (Tc_1, vap_pres, u10_m, app_temp)
;                     Buzan et al (2015): eqn 1 
;
; Apparent Temperature (Australian BOM): Here we use equation 22 
;          where AT is a function of air temperature (C), water 
;          vapor pressure (kPa), and 10-m wind speed (m/s). vap_pres
;          from Erich Fischer (consistent with CLM equations)
;
; Reference:  Steadman, R.G., 1994: Norms of apparent temperature
;             in Australia, Aust. Met. Mag., 43, 1-16. 
;
; http://www.weather-watch.com/smf/index.php?topic=52051.0
;
; t      ! temperature (C)
; vp     ! Vapor Pressure (pa)
; w10    ! Winds at 10m (m/s)
;
local at, con, c0, tk0
begin
  con = (/ 3.30, 0.70, 4.0 /)               ; 0,1,2 
  c0  = con(0)/1000
  tk0 = 273.15

  if (iounit(1).eq.0) then                  ; consytant to convert pressure
      conp = 100.
  else if (iounit(1).eq.1) then
      conp = 1.
  else if (iounit(1).eq.2) then
      conp = 10.
  end if  ; 2 => kPa
  end if  ; 1 =>  Pa
  end if  ; 0 => hPa

  c0 = c0*conp

  if (iounit(0).eq.0) then                  ; input C
      at   = t + c0*vp - con(1)*w10 - con(2)  
  else if (iounit(0).eq.1) then             ; input K
      at   = (t-tk0) + c0*vp - con(1)*w10 - con(2)  
  else if (iounit(0).eq.2) then             ; input F
      at   = 0.555556*(t-32) + c0*vp - con(1)*w10 - con(2)
  end if   ; F
  end if   ; K
  end if   ; C


  at@long_name = "apparent temperature"

  if (iounit(2).eq.0) then
      at@units = "degC"
  else if (iounit(2).eq.1) then
      at       = at + tk0
      at@units = "degK"
  else if (iounit(2).eq.2) then
      at       = 1.8*at+32
      at@units = "F"
  end if    ; F
  end if    ; K
  end if    ; C

  at@reference_1 = "Steadman, R.G., 1994: Norms of apparent temperature in Australia, Aust. Met. Mag., 43, 1-16" 
  at@reference_2 = "Buzan et al (2015): eq (1)"
  at@NCL       = "heat_apptemp"
;;at@doi = 

  copy_VarCoords(t, at)
  return(at)
end

;===============================================
undef("heat_humidex")
function heat_humidex(t:numeric, vp:numeric, iounit[2]:integer, opt:logical)
;
; HumanIndexMod.f90:   subroutine hmdex (Tc_3, vap_pres, humidex)
;                      Buzan et al (2015): eqn 4
;
; Reference:  Masterson, J., and F. Richardson, 1979: Humidex, a 
;             method of quantifying human discomfort due to 
;             excessive heat and humidity, CLI 1-79, Environment 
;             Canada, Atmosheric Environment Service, Downsview, Ontario
;
; t      ! temperature (C)
; vp     ! Vapor Pressure (pa)
; u10    ! Winds at 10m (m/s)
;
local hx, cc, tk0, conp
begin
  cc  = 5.0/9.0 
  tk0 = 273.15

  if (iounit(1).eq.0) then                  ; consytant to convert pressure
      conp = 100.
  else if (iounit(1).eq.1) then
      conp = 1.
  else if (iounit(1).eq.2) then
      conp = 10.
  end if  ; 2 => kPa
  end if  ; 1 =>  Pa
  end if  ; 0 => hPa

  if (iounit(0).eq.0) then                  ; input C
      hx = t + (cc*((vp*conp)/100 - 10.))
  else if (iounit(0).eq.1) then             ; input K
      hx = (t-tk0) + (cc*((vp*conp)/100 - 10.))
  else if (iounit(0).eq.2) then             ; input F
      hx = 0.555556*(t-32) + (cc*((vp*conp)/100 - 10.))
  end if   ; F
  end if   ; K
  end if   ; C

  hx@long_name = "Humidex"
  hx@units     = ""
  hx@info      = "human feels-like temperature"
  hx@NCL       = "heat_humidex"

  copy_VarCoords(t, hx)
  return(hx)
end

;===============================================
undef("heat_discoi")
function heat_discoi(t:numeric, twb:numeric, iounit[1]:integer, opt:logical)
;
; HumanIndexMod.f90:  subroutine dis_coi (Tc_4, wb_t, discoi)
;                     Eqn 10 of Buzan et al (2015)
;
; Discomfort Index
;             The wet bulb temperature is from NCLL's 'wetbulb', 'wetbulb_Wrap' 
;             rather than the Davies-Jones, 2008 based wetbulb used by HumanIndexMod.f90
; Reference:  Epstein, Y., and D.S. Moran, 2006: Thermal comfort and the heat stress indices,
;             Ind. Health, 44, 388-398.

; tc     ! temperature (C)
; twb    ! Vapor Pressure (pa)
;
local discomidx, tk0, tc, twbc
begin
  tk0 = 273.15

  if (iounit(0).eq.0) then                  ; input C
      discomidx = 0.5*twb + 0.5*t
  else if (iounit(0).eq.1) then             ; input K
      discomidx = 0.5*(twb-tk0) + 0.5*(t-tk0)
  else if (iounit(0).eq.2) then             ; input F
      tc      = 0.555556*(t  -32)
      twbc    = 0.555556*(twb-32)
      discomidx = 0.5*twbc + 0.5*tc
  end if   ; F
  end if   ; K
  end if   ; C

  discomidx@long_name = "discomfort index"
  discomidx@units     = ""                  ; unitless

  discomidx@info   = "human discomfort due to heat and humidity: eqn 10 of Buzan et al (2015)"
  discomidx@NCL    = "heat_discoi"

  copy_VarCoords(t, discomidx)
  return(discomidx)
end

;===============================================
undef("heat_discoi_stull") 
function heat_discoi_stull (tc:numeric, rhum:numeric, wbt_s:numeric, iounit[2]:integer, opt[1]:logical)
;
; HumanIndexMod.f90:    subroutine dis_coiS (Tc_5, relhum, wbt_s, discois)
;
; Discomfort Index where the wet bulb temperature is from Stull, 2011.
;         Requires air temperature (C), wet bulb temperature (C) 
; Reference:  Epstein, Y., and D.S. Moran, 2006: Thermal comfort and the heat stress indices,
;             Ind. Health, 44, 388-398.
;
;   tc        ! 2-m temperature (C)
;   wbt_s     ! 2-m Wet Bulb Temperature (C) from Stull
;   rhum      ! Relative Humidity (%)
;
local t50,tcbnd, rh99, rh05, rhmin, rh, discomidx_s 
begin
    t50   = totype(50,typeof(tc))
    tcbnd = where(tc.lt.t50, tc, t50)     

    rh99  = totype(99,typeof(rhum))
    rh    = where(rhum.lt.rh99, rhum, rh99)  
    rh05  = totype( 5,typeof(rhum))
    rh    = where(rh.gt.rh05  , rh, rh05)

    rhmin = -2.27*tcbnd + 27.7        

    discomidx_s = where(tcbnd.lt.-20 .or. rh.lt.rhmin, tcbnd, 0.5*wbt_s + 0.5*tcbnd)
    discomidx_s@long_name = "discomfort index with Stull wet bulb temperature"
    discomidx_s@units     = "degC"
    discomidx_s@NCL       = "heat_discoi_stull"

    copy_VarCoords(tc, discomidx_s)

; f90
;   Tc     = min(Tc_5,50.)
;   rh_min = Tc*(-2.27)+27.7
;   rh     = min(rh,99.)
;   rh     = max(rh,5.)
;   if (Tc < -20. .or. rh < rh_min) then
;      ! wbt_s calculation invalid
;      discois = Tc
;   else
;      ! wbt_s calculation valid
;      discois = 0.5*wbt_s + 0.5*Tc
;   end if

    return(discomidx_s)
end

;===============================================
undef("heat_thic_thip")
function heat_thic_thip(t:numeric, twb:numeric, iounit[1]:integer, opt[1]:integer)
;
; HumanIndexMod.f90:  subroutine THIndex (Tc_8, wb_t, thic, thip)
;             Buzan et al (2015) eqn 5 & 6i
;
; Calculates two forms of the index:  Comfort and Physiology
; Reference:  NWSCR (1976): Livestock hot weather stress. 
;             Regional operations manual letter C-31-76. 
;             National Weather Service Central Region, USA
;
;             Ingram: Evaporative cooling in the pig. Nature (1965)
;
; t        ! temperature 
; twb      ! Wet Bulb Temperature
; thic     ! Temperature Humidity Index Comfort (unitless)
; thip     ! Temperature Humidity Index Physiology (unitless)
;
local tk0, thic, thip, tc, twbc 
begin
  tk0 = 273.15

  if (iounit(0).eq.0) then                  ; input C
      thic = 0.72*twb + 0.72*t + 40.6
      thip = 0.63*twb + 1.17*t + 32.0
  else if (iounit(0).eq.1) then             ; input K
      thic = 0.72*(twb-tk0) + 0.72*(t-tk0) + 40.6
      thip = 0.63*(twb-tk0) + 1.17*(t-tk0) + 32.0
  else if (iounit(0).eq.2) then             ; input F
      tc   = 0.555556*(t    -32)
      twbc = 0.555556*(twb  -32)
      thic = 0.72*twbc + 0.72*tc + 40.6
      thip = 0.63*twbc + 1.17*tc + 32.0
  end if   ; F
  end if   ; K
  end if   ; C

  thic@long_name = "Temperature Humidity Index Comfort: Livestock" 
  thip@long_name = "Temperature Humidity Index Physiology" 

  thic@units = ""  ; unitless
  thip@units = ""

  copy_VarCoords(t, thic)
  copy_VarCoords(t, thip)

  return( [/ thic, thip /] )   ; return as elements of a 'list' variable
end

;===============================================
undef("heat_swamp_cooleff")
function heat_swamp_cooleff(t:numeric, twb:numeric, iounit[2]:integer, opt[1]:logical)
;
; HumanIndexMod.f90:  subroutine SwampCoolEff (Tc_9, wb_t, tswmp80, tswmp65)
;
; Swamp Cooler Efficiency
;       The wet bulb temperature is Davies-Jones 2008 (subroutine WetBulb)
;       Requires air temperature (C), wet bulb temperature (C) 
;
;       Assumes that the Swamp Cooler Efficiency 80% (properly maintained)
;       and 65% (improperly maintained).  
;
; Reference:  Koca et al: Evaporative cooling pads: test 
;             procedure and evaluation. Applied engineering
;             in agriculture (1991) vol. 7

; t       ! temperature (C)
; twb     ! Wet Bulb Temperature (C)

local tswmp65, tswmp80
begin
  tk0 = 273.15

  if (iounit(0).eq.0) then                  ; input C
      tswmp80 = t - 0.80*(t - twb)
      tswmp65 = t - 0.65*(t - twb)
  else if (iounit(0).eq.1) then             ; input K
      tswmp80 = (t-tk0) - 0.80*(t - twb - tk0)
      tswmp65 = (t-tk0) - 0.65*(t - twb - tk0)
  else if (iounit(0).eq.2) then             ; input F
      tc = 0.555556*t + 32
      tswmp80 = tc - 0.80*(t - twb - tk0)
      tswmp65 = tc - 0.65*(t - twb - tk0)
  end if   ; F
  end if   ; K
  end if   ; C

  tswmp80@long_name = "Swamp Cooler: 80% Efficient"
  tswmp65@long_name = "Swamp Cooler: 65% Efficient"

  if (iounit(1).eq.0) then                  ; output C
      tswmp80@units = "degC"
      tswmp65@units = "degC"
  else if (iounit(1).eq.1) then             ; output K
      tswmp80 = tswmp80 + tk0
      tswmp65 = tswmp65 + tk0
      tswmp80@units = "degK"
      tswmp65@units = "degK"
  else if (iounit(1).eq.2) then             ; output F
      tswmp80 = 1.8*tswmp80 + 32
      tswmp65 = 1.8*tswmp65 + 32
      tswmp80@units = "F"
      tswmp65@units = "F"
  end if   ; F
  end if   ; K
  end if   ; C

  copy_VarCoords(t, tswmp80)
  copy_VarCoords(t, tswmp65)

  return( [/ tswmp80, tswmp65 /] )  ; return as elements of a 'list' variable
end

;===============================================
undef("vapor_pres_rh")
function vapor_pres_rh(rh:numeric, es:numeric)
;
;  Vapour Pressure from Erich Fischer (consistent with CLM equations, Keith Oleson)
;  rh        ! Relative Humidity (%)
;  es        ! Saturated Vapour Pressure (Pa)
;  erh       ! Vapour Pressure (Pa)

local erh    ; same as model
begin
  dim_rh  = dimsizes(rh)
  dim_es  = dimsizes(es)
  rnk_rh  = dimsizes(dim_rh)
  rnk_es  = dimsizes(dim_es)
  if (.not.(rnk_rh.eq.rnk_es  .and.  all(dim_rh.eq.dim_es))) then
      if (rnk_rh.ne.dim_es) then
          print("vapor_pres_rh: dimension error: rank_rh="+rnk_rh+", rank_es="+rnk_es)
          print(dim_rh)
          print(dim_rh)
          exit
      end if
  end if
    
  erh = (rh/100.0)*es   
  erh@long_name = "Vapor Pressure"
  if (isatt(es,"units")) then
      erh@units = es@units
  end if
  erh@NCL       = "vapor_pres_rh"
  copy_VarCoords(rh, erh)

  return( erh )
end
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; The following is a pre-existing verion the 'heat index' [Eqn 3; Buzan]
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
undef("heat_index_nws_eqns")
function heat_index_nws_eqns(t:numeric, rh:numeric, crit[3]:numeric, c[9]:numeric \
                            ,eqnType[1]:integer, opt[1]:logical)
; 'heat_index_nws' driver; input t is **degF**
local HI, A, t2, rh2, trh
begin
                                                           ; NWS practice
  HI = (0.5*(t+61.0+((t-68.0)*1.2)+(rh*0.094)) + t)*0.5    ; avg (Steadman and t)
  HI = where(t.le.40, t, HI)                               ; http://ehp.niehs.nih.gov/1206273/

 ;A  = -10.3 + 1.1*t + 0.047*HI                            ; ehp.1206273.g003.tif
 ;if (A.ge.40 .and. A.lt.lt.crit(0)) then
 ;    HI = A 
 ;end if
 ;delete(A)

  if (all(t.lt.crit(0))) then
      eqnType = 0
  else
      HI = where(HI.ge.crit(0)  \
                ,c(0)+ c(1)*t + c(2)*rh + c(3)*t*rh  + c(4)*t^2              \
                +c(5)*rh^2 + c(6)*t^2*rh  + c(7)*t*rh^2 + c(8)*(t^2)*(rh^2)  \
                ,HI)                 
    
      HI = where(rh.lt.13 .and. (t.gt.80 .and. t.lt.112) \
                ,HI-((13-rh)/4)*sqrt((17-abs(t-95.))/17), HI)
    
      HI = where(rh.gt.85 .and. (t.gt.80 .and. t.lt.87)  \
                ,HI+((rh-85)/10)*((87-t)/5), HI)

      eqnType = 1
  end if

  return(HI)
end
; ------
undef("heat_index_nws")
function heat_index_nws(t:numeric, rh:numeric, iounit[2]:integer, opt[1]:logical)
;
; HumanIndexMod.f90:   subroutine HeatIndex (Tc_7, rh, hi)
; NOTE: This function existed before the heat.HumanIndexMod.ncl was implemented.
;
; http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
; https://en.wikipedia.org/wiki/Heat_index
; Reference:
; R. G. Steadman, 1979: 
;    The Assessment of Sultriness. Part I: A Temperature-Humidity Index Based on Human Physiology and Clothing Science.
;    J. Appl. Meteor., 18, 861–873.
;    doi: http://dx.doi.org/10.1175/1520-0450(1979)018<0861:TAOSPI>2.0.CO;2 
;
; Lans P. Rothfusz (1990): NWS Technical Attachment (SR 90-23)
;
; The ‘Heat Index’ is a measure of how hot weather "feels" to the body.
; The combination of temperature an humidity produce an "apparent temperature" 
; or the temperature the body "feels". The returned values are for shady locations only. 
; Exposure to full sunshine can increase heat index values by up to 15°F. 
; Also, strong winds, particularly with very hot, dry air, can be extremely 
; hazardous as the wind adds heat to the body
;
; The computation of the heat index is a refinement of a result obtained by multiple 
; regression analysis carried out by Lans P. Rothfusz and described in a 
; 1990 National Weather Service (NWS) Technical Attachment (SR 90-23).  
;
; In practice, the Steadman formula is computed first and the result averaged 
; with the temperature. If this heat index value is 80 degrees F or higher, 
; the full regression equation along with any adjustment as described above is applied. 
;
local HI, T, Tcrit, c, eqnType, units
begin

  if (iounit(0).lt.0 .or. iounit(0).gt.2) then
      print("heat_index_nws: invalid iounit(0): invalid(0)="+iounit(0))
      exit
  end if
  if (iounit(1).lt.0 .or. iounit(1).gt.2) then
      print("heat_index_nws: invalid iounit(1): invalid(1)="+iounit(1))
      exit
  end if
  if (all(rh.lt.1)) then
      print("heat_index_nws: rh must be % not fractional; All rh are < 1")
      exit
  end if

       ; Default coef are for .ge.80F and 40-100% humidity 
  coef  = (/-42.379, 2.04901523, 10.14333127, -0.22475541   \
           ,-0.00683783, -0.05481717, 0.00122874, 0.00085282, -0.00000199 /)
  crit  = (/ 80, 40, 100/)    ; (T_low (F),  RH_low,  RH_High/)

       ; Optional coef are for 70F-115F and humidities between 0 and 80% 
       ; Within 3F of default coef
  if (opt .and. isatt(opt,"coef") .and. opt@coef.eq.2) then
      coef := (/ 0.363445176, 0.988622465, 4.777114035, -0.114037667  \
               ,-0.000850208,-0.020716198, 0.000687678,  0.000274954, 0.0 /)
      crit := (/ 70, 0, 80/)   ; F
  end if

  eqnType = -1
  if (iounit(0).eq.2) then                          ; t must be degF
      HI = heat_index_nws_eqns(t, rh, crit, coef, eqnType, opt) ; use input (t) directly
  else
      if (iounit(0).eq.0) then
           T = 1.8*t + 32                           ; degC => degF
      else
           T = 1.8*t - 459.67                       ; degK => degF
      end if
      HI = heat_index_nws_eqns(T, rh, crit, coef, eqnType, opt) ; use local T
  end if

  if (iounit(1).eq.2) then                 
      units = "degF"
  else
      if (iounit(1).eq.0) then
          HI = (HI-32)*0.55555
          units = "degC"
      else
          HI = (HI+459.67)*0.55555
          units = "degK"
      end if
  end if

  HI@long_name = "heat index: NWS"
  HI@units     = units
  HI@www       = "http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml"
  HI@info      = "appropriate for shady locations with no wind"
  if (eqnType.eq.0) then
      HI@tag   = "NCL: heat_index_nws; (Steadman+t)*0.5"
  else
      HI@tag   = "NCL: heat_index_nws; (Steadman+t)*0.5 and Rothfusz"            
  end if
  copy_VarCoords(t, HI)

  return(HI)
end
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; The following is not part of the "HumanIndexMod" module
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
undef("heat_esidx_moran")
function heat_esidx_moran(t:numeric, rh:numeric, srad:numeric, iounit[2]:integer)
; 
; Reference:
;    Moran, D.S. et al (2001)
;    An environmental stress index (ESI) as a substitute for the wet bulb globe temperature (WBGT) 
;    Journal of Thermal Biology 26: 427–431
;    doi:10.1016/S0306-4565(01)00055-9
;
; t    - ambient temperature (°C)
; rh   - relative humidity (%) 
; srad - solar radiation (W·m−1). 
;
; The correlation coefficients between ESI and wet bulb globe temperature (WBGT) 
; were very high (R2>0.981). Therefore, we conclude that ESI, based on fast response 
; and the more commonly used accurate climatic microsensors (t, rh, srad) which can 
; be combined in a small portable device, has the potential to be a practical 
; alternative to the WBGT.
;
local esidx, tk0
begin
  tk0 = 273.15

  if (iounit(0).eq.0) then                  ; input C
      esidx = 0.63*t - 0.03*rh + 0.002*srad \
            + 0.0054*(t*rh) - 0.0073/(0.1+srad) 
  else if (iounit(0).eq.1) then             ; input K
      esidx = 0.63*(t-tk0) - 0.03*rh + 0.002*srad \
            + 0.0054*((t-tk0)*rh) - 0.0073/(0.1+srad) 
  else if (iounit(0).eq.2) then             ; input F
      tc    = 0.555556*(t  -32)
      esidx = 0.63*tc - 0.03*rh + 0.002*srad \
            + 0.0054*(tc*rh) - 0.0073/(0.1+srad) 
  end if   ; F
  end if   ; K
  end if   ; C

  esidx@long_name = "environmental stress index"

  if (iounit(1).eq.0) then                  ; output C
      esidx@units = "degC"
  else if (iounit(1).eq.1) then             ; output K
      esidx = esidx + tk0
      esidx@units = "degK"
  else if (iounit(1).eq.2) then             ; output F
      esidx = 1.8*esidx + 32
      esidx@units = "F"
  end if   ; F
  end if   ; K
  end if   ; C

  esidx@doi = "http://dx.doi.org/10.1016/S0306-4565(01)00055-9"

  copy_VarCoords(t, esidx)
  return(esidx)
end
