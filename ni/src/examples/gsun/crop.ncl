;                   Released with: NCL 6.4.0
; Reference:
;
;  Richard G. Allen, Luis S. Pereira, Dirk Raes & Martin Smith (1998) 
;  Crop Evapotranspiration – Guidelines for Computing Crop Water Requirements 
;  FAO Irrigation and drainage paper 56. Rome, Italy: Food and Agriculture Organization of the United Nations. 
;  ISBN 92-5-104219-5.
;     http://www.fao.org/docrep/X0490E/x0490e00.htm#Contents
;  In particular: Chapters 3 & 4
;     http://www.fao.org/docrep/X0490E/x0490e07.htm#chapter 3   meteorological data
;     http://www.fao.org/docrep/X0490E/x0490e08.htm#chapter 4   determination of eto
; ----------------------------------------------------------------------------------
;ET is the loss of water by evaporation from both the soil and plant (evaporation + transpiration)
;
;- ET depends on several factors: solar radiation, air temperature, air vapour pressure, wind speed and surface area.
;- Potential ET (PET) is the maximum rate of ET given the current conditions. PET is not a constant value but 
;        varies with field conditions: it is the rate of water loss if water supply is not limiting. But often 
;        water supply is limited, so water loss is often smaller than PET. The rate at which water is being lost
;        is known as actual ET (AET).
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("getMidMonDay")
function getMidMonDay(time:numeric)
;
; utility function
; The input 'time' (seconds/hours/days since ...") has the middle of the month
; This returns the (*approximate*) 'middle day' of the current month as an integer
; "Approximate" because day 15 is used for all months ... 
;      rather than 14 (Feb), 14.5 (Leap year Feb) or 15.5 (31 day months).
; This is more than adequate.

local date, yyyy, mm, dd, jday
begin
    date = cd_calendar(time, 0)
    yyyy = toint(date(:,0))
    mm   = toint(date(:,1))

    dd   = conform(mm, 15, -1)    ; approx mid month        

    jday = day_of_year(yyyy, mm, dd)

    copy_VarCoords(time, jday)
    jday@long_name = "mid-month day of current year"
    return(jday)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("getMidMonDay_yyyymm")
function getMidMonDay_yyyymm(yyyymm:numeric)
;
; utility function
; This returns the (approximate) 'middle day' of the current month as an integer
; "Approximate" because ay 15 is used for all months ... 
;      rather than 14 (Feb), 14.5 (Leap year Feb) or 15.5 (31 day months).
; This is more than adequate.
local date, yyyy, mm, dd, jday
begin
    yyyy = toint(yyyymm/100)
    mm   = toint(yyyymm - (yyyy*100))
    dd   = conform(mm, 15, -1)    ; approx mid month        

    jday = day_of_year(yyyy, mm, dd)

    copy_VarCoords(time, jday)
    jday@long_name = "mid-month day of current year"
    return(jday)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;          isscalar made a builtin function in 6.4.0
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;undef("isscalar")
;;function isscalar(x)
;;;
;;; utility function
;;; is 'x' a scalar quantity: here 'scalar' means rank=1 & size=1
;;local dimx, rankx
;;begin
;;  dimx  = dimsizes(x)
;;  rankx = dimsizes(dimx)
;;  if (rankx.eq.1 .and. dimx.eq.1) then
;;      return(True)
;;  else
;;      return(False)
;;  end if 
;;end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("satvpr_temp_fao56")
function satvpr_temp_fao56(t:numeric, iounit[2]:integer) 
 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; Estimate saturation vapor pressure (kPa): EQN 11; Chapter 3
;
; Note_1: If t=t_dew then this calculates actual vapor pressure EQN 14 (ea)
; Note_2: if any (t.le.0)   svp is undefined ==> _FillValue                          
;         Do *not* return 0.0 because other eqns take log(satvpr)
;         t could have no _FillValue BUT values <0 hence svp@_FillValue
 
local svp, tc, tFill
begin
   if (iounit(0).lt.0 .or. iounit(0).gt.2) then               ; iounit error check
       print("satvpr_temp_fao56: unrecognized iounit argument: iounit="+iounit)
       exit
   end if

   if (isatt(t,"_FillValue")) then
       tFill = t@_FillValue
   else
       tFill = 1e20            ; t has no _FillValue BUT there could b t.le.0
   end if

   if (iounit(0).eq.0) then                               ; degC
       svp = where(t.gt.0, 0.6108*exp((17.27*t)/(t+237.3)), tFill )       
   else if (iounit(0).eq.1) then                          ; degK
       tc  = t-273.16   
       svp = where(tc.gt.0, 0.6108*exp((17.27*tc)/(tc+237.3)), tFill )       
   else if (iounit(0).eq.2) then                          ; F
       tc  = (t-32)*0.5555556  
       svp = where(tc.gt.0, 0.6108*exp((17.27*tc)/(tc+237.3)), tFill )       
   end if        ; F
   end if        ; K 
   end if        ; C  

   svp@long_name = "saturation vapor pressure"

   if (iounit(1).eq.0) then 
        svp   = svp*10
        svp@units     = "hPa"
   else if (iounit(1).eq.1) then 
        svp       = svp*1000
        svp@units = "Pa"
   else if (iounit(1).eq.2) then 
        svp@units     = "kPa"
   end if        ; kPa
   end if        ; Pa
   end if        ; hPa

   svp@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   svp@info      = "FAO 56; EQN 11; satvpr_temp_fao56"

   if (any(t.le.0) .and. .not.isatt(svp,"_FillValue") ) then
       svp@_FillValue = tFill
   end if

   copy_VarCoords(t, svp)
   return(svp)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("satvpr_mean_fao56")
function satvpr_mean_fao56(tmin:numeric, tmax:numeric, iounit[2]:integer) 
 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; Estimate 'mean' saturation vapor pressure (kPa): EQN 12; Chapter 3
;
; Note_1: If t=t_dew then this calculates actual vapor pressure EQN 14 (ea)
; Note_2: if any (t.le.0)   svp is undefined ==> _FillValue                          
;         Do *not* return 0.0 because other eqns take log(satvpr)
;         t could have no _FillValue BUT values <0 hence svp@_FillValue
 
local svp
begin
   svp = (satvpr_temp_fao56(tmin, iounit) + satvpr_temp_fao56(tmax, iounit))*0.5
   svp@long_name = "mean saturation vapor pressure"
   svp@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   svp@info      = "FAO 56; EQN 12; satvpr_mean_fao56"

   copy_VarCoords(tmin, svp)
   return(svp)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("satvpr_tdew_fao56")
function satvpr_tdew_fao56(tdew:numeric, iounit[2]:integer) 
 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; Compute actual saturation vapor pressure (kPa): EQN 14; Chapter 3

; This is the same code as 'satvpr_temp_fao56'
; Just different 'long_name' and 'info' attributes
 
local actsvp
begin
   actsvp           = satvpr_temp_fao56(tdew, iounit) 

   actsvp@long_name = "actual saturation vapor pressure via Tdew"
   actsvp@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   actsvp@info      = "FAO 56; EQN 14; satvpr_tdew_fao56"
   return(actsvp)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("actvpr_tmin_fao56")
function actvpr_tmin_fao56(tmin:numeric, iounit[2]:integer) 
 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; Estimate actual vapor pressure (kPa) using Tmin: EQN 48; Chapter 3
; This uses the same code as 'satvpr_temp_fao56'
; Just different 'long_name' and 'info' attributes
;
; This assumes that the dew point is near the minimum temperature.
;
; NOTE:
; (a) In well watered areas Tdew >> Tmin
; (b) In arid areas Tmin >> Tdew. 
;     In these areas (Tmin-2) or (Tmin-3) might be better.
;     
local act
begin
   act           = satvpr_temp_fao56(tmin, iounit) 

   act@long_name = "actual vapor pressure"
   act@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   act@info      = "FAO 56; EQN 48; actvpr_tmin_fao56"
   return(act)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("tdew_actvpr_fao56")
function tdew_actvpr_fao56(actvpr:numeric, iounit[2]:integer) 
 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; Compute dew point temperature from actual vapor pressure via EQN 3-11; Annex 3
; actvpr - actual vapor pressure 

local con, tdew, units
begin

   if (iounit(0).eq.0) then         ; hPa ... convert to kPa
       con  = 0.1
   else if (iounit(0).eq.1) then    ; Pa  ... convert to kPa
       con  = 0.001
   else if (iounit(0).eq.2) then    ; already kPa
       con  = 1.0
   end if
   end if
   end if

   tdew = (116.91+237.3*log(con*actvpr))/(16.78-log(con*actvpr))      

   if (iounit(1).eq.0) then                              
       units = "degC"
   else if (iounit.eq.1) then                              
       tdew  = tdew+273.16   
       units = "degK"
   else if (iounit.eq.2) then 
       tdew  = 1.8*tdew+32
       units = "F"
   end if
   end if
   end if

   tdew@long_name = "Estimated dew point temperature"
   tdew@units     =  units     
   tdew@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   tdew@fao56_info= "FAO 56; Annex: EQN 3-11"
   tdew@NCL       = "tdew_actvpr_fao56"

   copy_VarCoords(actvpr, tdew)
   return(tdew)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("prsatm_z_fao56")
function prsatm_z_fao56(z:numeric, P0[1]:numeric, z0[1]:numeric, iounit[2]:integer) 
 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; EQN 7, Chapter 3; This assumes 20C for a standard atmosphere
;
local con, tk0, prs, conP0
begin
   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then               ; unit error check
       print("prsatm_z_fao56: unrecognized iounit argument: iounit="+iounit)
       exit
   end if

   con   = 5.26
   tk0   = 293.0                                       ; 273+20                          
   conP0 = P0  
   if (iounit(0).eq.0) then 
       conP0 = conP0*0.1                               ; hPa -> kPa
   else 
       if (iounit(0).eq.1) then         
           conP0 = conP0*0.001                         ;  Pa -> kPa
        end if
   end if

   prs  = conP0*((tk0-0.0065*z)/tk0)^con

   prs@long_name = "atmospheric pressure"
   prs@units     = "kPa"       
   prs@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   prs@info      = "FAO 56; Chapter 3, EQN 7; prsatm_z_fao56"
   copy_VarCoords(z, prs)

   if (iounit(1).eq.0) then 
       prs       = (/ prs*10.0 /)                     ; kPa -> hPa
       prs@units = "hPa"       
   else 
       if (iounit(1).eq.1) then         
           prs       = (/ prs*1000.0 /)              ; kPa ->  Pa
           prs@units = "Pa"       
       end if
   end if

   return(prs)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("prsatm_tz_fao56")
function prsatm_tz_fao56(t:numeric, z:numeric, P0[1]:numeric, z0[1]:numeric, iounit[3]:integer) 
 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; EQN 3-2; Annex 3 ... requires temperature
;
local R, g, a, con, tk0, prs, conP0
begin
   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then               ; unit error check
       print("prsatm_tz_fao56: unrecognized iounit argument: iounit="+iounit)
       exit
   end if

   R    = 287.04                                       ; J/(kg-degK) <==> J/(kg-degC)
   g    = 9.807                                        ; m/2
   a    = 0.0065                                       ; degK/m <==> degC/m
   con  = g/(a*R)                                      ; exponent 

   conP0= P0  
   if (iounit(1).eq.0) then 
       conP0 = conP0*0.1                               ; hPa -> kPa
   else 
       if (iounit(1).eq.1) then         
           conP0 = conP0*0.001                         ;  Pa -> kPa
        end if
   end if

   if (iounit(0).eq.0) then        
       tk0  = 273.16 + t                              ; degC -> degK
       prs  = conP0*( (tk0-a*(z-z0))/tk0 )^con
   else if (iounit(0).eq.1) then                      ; degK
       prs  = conP0*( (t-a*(z-z0))/t )^con
   else if (iounit(0).eq.2) then                             
       tk0  = 273.16 + (t-32)*0.555556                ; degF -> degK
       prs  = conP0*( (tk0-a*(z-z0))/tk0 )^con
     end if
    end if
   end if

   prs@long_name = "atmospheric pressure"
   prs@units     = "kPa"       
   prs@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   prs@info      = "FAO 56; Annex 3: EQN 3-2; prsatm_tz_fao56"
   copy_VarCoords(t, prs)

   if (iounit(2).eq.0) then 
       prs       = (/ prs*10.0 /)                     ; kPa -> hPa
       prs@units = "hPa"       
   else 
       if (iounit(2).eq.1) then         
           prs       = (/ prs*1000.0 /)               ; kPa ->  Pa
           prs@units = "Pa"       
       end if
   end if

   return(prs)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("u2_fao56")
function u2_fao56(uz:numeric, z:numeric, wunit[2]:integer) 
 
; Compute 2-meter height wind
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; http://www.fao.org/docrep/x0490e/x0490e0j.htm#annex%202.%20meteorological%20tables

local u2, con
begin
   con = 1.0                     ; m/s
   if (wunit(0).eq.1) then
       con = 0.277778            ; km/hr => m/s
   else if (wunit(0).eq.2) then
            con = 0.44704        ; mph  => m/s
        end if
   end if
     
   u2           = (uz*con)*(4.87/(log(67.8*z-5.42)))

   u2@long_name = "Estimated 2-meter wind speed"
   u2@units     = "m/s"
   u2@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   u2@info      = "FAO 56; EQN 47; u2_fao56"
   copy_VarCoords(uz, u2)

   if (wunit(1).eq.1) then
       u2       = (/ (u2/0.277778) /)          ; m/s => km/hr 
       u2@units = "m/s"
   else if (wunit(1).eq.2) then
       u2       = (/ (u2/0.44704) /)           ; m/s => mph   
       u2@units = "mph"
        end if
   end if

   return(u2)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("rhum_fao56")
function rhum_fao56(actvpr:numeric, satvpr:numeric,rhunit [1]:integer) 
 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; Compute relative humidity

local rhum, rhum_units
begin
   if (any(actvpr.lt.0.0) .or. any(satvpr.le.0.0)) then
       print("rhum_fao56: actvpr<0 or satvpr<=0")
       exit
   end if

   rhum = ( actvpr/satvpr) 

   rhum@long_name = "Relative Humidity"
   if (rhunit.eq.0) then
       rhum = rhum*100 
       rhum@units = "%"
   else
       rhum@units = "fraction"
   end if

   rhum@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   rhum@info      = "FAO 56; EQN 10; rhum_fao56"
   copy_VarCoords(actvpr, rhum)

   return(rhum)
   end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("satvpr_slope_fao56")
function satvpr_slope_fao56(t:numeric, iounit[2]:integer)

; Compute slope (kPa/C) of saturation vapor pressure (svp) curve; EQN 13; Chapter 3
;         http://www.fao.org/docrep/X0490E/x0490e07.htm
; See:    http://www.fao.org/docrep/x0490e/x0490e0j.htm#annex 2. meteorological tables
; 

local satvpr_slope, tc, tFill
begin
  if (iounit(0).lt.0 .or. iounit(0).gt.2) then              ; tunit error check
      print("satvpr_slope_fao56: unrecognized iounit(0) argument: iounit(0)="+iounit(0))
      exit
  end if

  if (iounit(1).lt.0 .or. iounit(1).gt.2) then              ; tunit error check
      print("satvpr_slope_fao56: unrecognized iounit(1) argument: iounit(1)="+iounit(1))
      exit
  end if

  if (isatt(t,"_FillValue")) then
      tFill = t@_FillValue
  else
      tFill = 1e20
  end if

  if (iounit(0).eq.0) then                           ; degC
      satvpr_slope = where(t.gt.0, 4096*(0.6108*exp((17.27*t)/(t+237.3))/(t+237.3)^2), tFill)
  else
      if (iounit(0).eq.1) then                       ; degK
          tc = t-273.16
      else                                           ; F
          tc = (t-32)*0.5555556  
      end if
      satvpr_slope = where(tc.gt.0, 4096*(0.6108*exp((17.27*tc)/(tc+237.3))/(tc+237.3)^2), tFill)
  end if
  
  
  satvpr_slope@long_name = "slope saturation vapor pressure curve"
  satvpr_slope@units     = "kPa/C"
  satvpr_slope@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
  satvpr_slope@info      = "FAO 56; EQN 13; satvpr_slope_fao56"

                         ; if specified, change units
  if (iounit(1).eq.0) then
      satvpr_slope = (/ satvpr_slope*10 /)    ; kPa=>hPa
      satvpr_slope@units = "hPa/C"
  end if

  if (iounit(1).eq.1) then
      satvpr_slope = (/ satvpr_slope*1000 /)  ; kPa=>Pa
      satvpr_slope@units = "Pa/C"
  end if

  if (.not.isatt(satvpr_slope,"_FillValue") .and. any(satvpr_slope.eq.tFill) ) then
      satvpr_slope@_FillValue = tFill
  end if

  copy_VarCoords(t, satvpr_slope)
  return( satvpr_slope )
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("actvpr_rhmean_fao56")
function actvpr_rhmean_fao56(tmin:numeric, tmax:numeric, rhmean:numeric, iounit[2]:integer)
;
; http://www.fao.org/docrep/X0490E/x0490e07.htm
; http://www.fao.org/docrep/x0490e/x0490e0j.htm#annex 2. meteorological tables
; Compute 'actual' saturation vapor pressure with mean relative humidity (%): EQN 19
;
; tmin, tmax  - min/max temperature (C or K)
; rmean       - mean relative humidity (%)
;
local conmean, avp_rhmean, tc, tcx, tcn, tFill, tCrit, tunit, punit \
    , dim_tmin, dim_tmax, dim_rhmn, rnk_tmin, rnk_tmax, rnk_rhmn 
begin

   if (iounit(0).lt.0 .or. iounit(0).gt.2) then              ; tunit error check
       print("actvpr_rhmean_fao56: unrecognized iounit(0) argument: iounit(0)="+iounit(0))
       exit
   end if

   if (iounit(1).lt.0 .or. iounit(1).gt.2) then              ; tunit error check
       print("actvpr_rhmean_fao56: unrecognized iounit(1) argument: iounit(1)="+iounit(1))
       exit
   end if

   tCrit = 0    
   tunit = iounit(0)
   punit = iounit(1)

   dim_tmin   = dimsizes(tmin)        ; size(s)
   dim_tmax   = dimsizes(tmax)
   dim_rhmn   = dimsizes(rhmean)

   rnk_tmin   = dimsizes(dim_tmin)    ; rank
   rnk_tmax   = dimsizes(dim_tmax)
   rnk_rhmn   = dimsizes(dim_rhmn)

   if (.not.(rnk_tmin.eq.rnk_tmax .and. rnk_tmin.eq.rnk_rhmn)) then 
       print("actvpr_rhmean_fao56: rank mismatch on input arguments")
       print("                     rnk_tmin="+rnk_tmin)
       print("                     rnk_tmax="+rnk_tmax)
       print("                     rnk_rhmn="+rnk_rhmn)
       exit
   end if

   if (.not.(all(dim_tmin.eq.dim_tmax) .and. all(dim_tmin.eq.dim_rhmn))) then 
       print("actvpr_rhmean_fao56: dimension size mismatch on input arguments")
       print("                     dim_tmin="+dim_tmin+" dim_tmax="+dim_tmax+" dim_rhmn="+dim_rhmn)
       exit
   end if

   if (isatt(tmin,"_FillValue")) then
       tFill = tmin@_FillValue
   else
       tFill = 1e20
   end if

   if (all(rhmean.le.1)) then
       conmean = rhmean/2.0            ; must be fractional and not %
   else
       conmean = rhmean/200.0
   end if

   if (tunit.eq.0) then                ; degC; use input array
       avp_rhmean = where(tmin.gt.tCrit \
                         ,conmean*(satvpr_temp_fao56(tmax,iounit)+satvpr_temp_fao56(tmin,iounit)) \
                         ,tFill      )
       avp_rhmean = where(tmin.le.tCrit .and. tmax.gt.tCrit          \   ; Should this be done?
                         ,conmean*(satvpr_temp_fao56(tmax,iounit) )  \   ; satvpr_mean_fao56(0,0)=0.0
                         ,avp_rhmean )
       avp_rhmean = where(avp_rhmean.lt.0.0, tFill, avp_rhmean)
   else                                ; create and use temporary arrays
       if (tunit.eq.1) then            ; degK
           tcn = tmin-273.16           ; convert to degC
           tcx = tmax-273.16   
       else                            ; Farenheit
           tcn = (tmin-32)*0.5555556   ; convert to degC
           tcx = (tmax-32)*0.5555556
       end if
       avp_rhmean = where(tcn.gt.tCrit \
                         ,conmean*(satvpr_temp_fao56(tcx,(/0,punit/))+satvpr_temp_fao56(tcn,(/0,punit/))) \
                         ,tFill      )
       avp_rhmean = where(tcn.le.tCrit .and. tcx.gt.tCrit                \   ; Should this be done?
                         ,conmean*(satvpr_temp_fao56(tcx,(/0,punit/)) )  \   ; satvpr_mean_fao56(0,0)=0.0
                         ,avp_rhmean )
       avp_rhmean = where(avp_rhmean.lt.0.0, tFill, avp_rhmean)
   end if

   avp_rhmean@long_rhmean_name = "actual saturation vapor pressure"
   avp_rhmean@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   avp_rhmean@info      = "FAO 56; EQN 19: min/max t; mean rh; actvpr_rhmean_fao56"

   if (punit.eq.0) then 
        avp_rhmean@units = "hPa"
   else if (punit.eq.1) then 
        avp_rhmean@units = "Pa"
   else if (punit.eq.2) then 
        avp_rhmean@units = "kPa"
   end if        ; kPa
   end if        ; Pa
   end if        ; hPa

   if (.not.isatt(avp_rhmean,"_FillValue") .and. any(avp_rhmean.eq.tFill) ) then
       avp_rhmean@_FillValue = tFill
   end if

   copy_VarCoords(tmin, avp_rhmean)

   return( avp_rhmean )
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("actvpr_mnmx_fao56")
function actvpr_mnmx_fao56(tmin:numeric, tmax:numeric, rhmin:numeric, rhmax:numeric, iounit[2]:integer)

; http://www.fao.org/docrep/X0490E/x0490e07.htm
; http://www.fao.org/docrep/x0490e/x0490e0j.htm#annex 2. meteorological tables

; Compute 'actual' saturation vapor pressure with min/max relative humidity (%): EQN 17
; tmin, tmax   - min/max temperature (C)
; rhmin, rhmax - min/max relative humidity (%)

local tunit, punit, conmin, conmax, avp_mnmx, tc, tcx, tcn, tFill \
    , dim_tmin, dim_tmax, dim_rhmn, dim_rhmx, rnk_tmin, rnk_tmax, rnk_rhmn, rnk_rhmx 
begin

   tunit = iounit(0)
   punit = iounit(1)

; check input
   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then              ; tunit error check
       print("actvpr_mnmx_fao56: unrecognized iounit argument: iounit="+iounit)
       exit
   end if

   dim_tmin   = dimsizes(tmin)        ; size(s)
   dim_tmax   = dimsizes(tmax)
   dim_rhmn   = dimsizes(rhmin)
   dim_rhmx   = dimsizes(rhmax)

   rnk_tmin   = dimsizes(dim_tmin)    ; rank
   rnk_tmax   = dimsizes(dim_tmax)
   rnk_rhmn   = dimsizes(dim_rhmn)
   rnk_rhmx   = dimsizes(dim_rhmx)

   if (.not.(rnk_tmin.eq.rnk_tmax .and. rnk_tmin.eq.rnk_rhmn)) then 
       print("actvpr_rhmean_fao56: rank mismatch on input arguments")
       print("                     rnk_tmin="+rnk_tmin)
       print("                     rnk_tmax="+rnk_tmax)
       print("                     rnk_rhmn="+rnk_rhmn)
       print("                     rnk_rhmx="+rnk_rhmx)
       exit
   end if

   if (.not.(all(dim_tmin.eq.dim_tmax) .and. \
             all(dim_tmin.eq.dim_rhmn) .and. \
             all(dim_tmin.eq.dim_rhmx))) then 
       print("actvpr_mnmx_fao56: dimension size mismatch on input arguments")
       print("                   dim_tmin="+dim_tmin+" dim_tmax="+dim_tmax)
       print("                   dim_rhmn="+dim_rhmn+" dim_rhmx="+dim_rhmx)
       exit
   end if

   if (isatt(tmin,"_FillValue")) then
       tFill = tmin@_FillValue
   else
       tFill = 1e20
   end if

   if (all(rhmin.le.1)) then
       conmin = rhmin/2.0
   else
       conmin = rhmin/200.0
   end if

   if (all(rhmax.le.1)) then
       conmax = rhmax/2.0
   else
       conmax = rhmax/200.0
   end if
            ; note the (min*max) and (max*min) 'cross-product'
   if (tunit.eq.0) then
       avp_mnmx = where(tmin.gt.0 \
                       ,conmin*satvpr_temp_fao56(tmax,iounit) + conmax*satvpr_temp_fao56(tmin,iounit) \
                       ,tFill)
   else
       if (tunit.eq.1) then        ; degK
           tcn = tmin-273.16       ; convert to degC
           tcx = tmax-273.16   
       else                        ; Farenheit
           tcn = (tmin-32)*0.5555556   ; convert to degC
           tcx = (tmax-32)*0.5555556
       end if
       avp_mnmx = where(tcn.gt.0 \
                       ,conmin*satvpr_temp_fao56(tcx,(/0,punit/)) + conmax*satvpr_temp_fao56(tcn,(/0,punit/)) \
                       ,tFill)
   end if

   avp_mnmx@long_name = "actual saturation vapor pressure"
   avp_mnmx@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   avp_mnmx@info      = "FAO 56; EQN 17: min/max t & rh; actvpr_mnmx_fao56"

   if (.not.isatt(avp_mnmx,"_FillValue") .and. any(avp_mnmx.eq.tFill) ) then
       avp_mnmx@_FillValue = tFill
   end if

   if (punit.eq.0) then 
        avp_mnmx@units = "hPa"
   else if (punit.eq.1) then 
        avp_mnmx@units = "Pa"
   else if (punit.eq.2) then 
        avp_mnmx@units = "kPa"
   end if        ; kPa
   end if        ; Pa
   end if        ; hPa

   copy_VarCoords(tmin, avp_mnmx)
   return( avp_mnmx )
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("daylight_fao56")
function daylight_fao56(jday[*]:numeric, lat:numeric)

; Compute maximum day light hours; EQN 34
; http://www.fao.org/docrep/X0490E/x0490e07.htm
;
; **For  abs(lat) > 55 the eqns have limited validity**
;        The reason for the argtst and argval variables is an 
;        arbitrary attempt to minimize the effect poleward of (say) 55 

local pi, pi2, con, rad, sdec, sdec, ws, pi2yr, latrad, ntim,nlat, mlon, arg \
    , daylightmax, DAYLIGHTMAX, argtst, argval, FillValue
begin
   if (typeof(lat).eq."double") then
       pi  = 4.0d*atan(1.0d)         
   else
       pi  = 4.0*atan(1.0)         
   end if
                               ; constants
   pi2    = 2*pi
   rad    = pi/180
   pi2yr  = pi2/365
   latrad = lat*rad
   con    = (24/pi)
                               ; dimensional stuff
   ntim   = dimsizes(jday)     ; # time time steps
   dimlat = dimsizes(lat)
   ranklat= dimsizes(dimlat)
   if (ranklat.ge.3) then
       print("daylight_fao56: latitude rank exceeds 2: FATAL: ranklat="+ranklat)
       exit
   end if

   nlat   = dimlat(0)
   if (ranklat.eq.2) then
       mlon = dimlat(1)
   end if

   sdec   = 0.409*sin(pi2yr*jday - 1.39)           ; eq 24 [ntim]

   argtst = 1                                      ; arbitrary value based on heuristic test
   argval = 0.99999                                ; arbitrary value to allow completetion

   if (nlat.eq.1 .and. ntim.eq.1) then             ; both input are scalar
       arg := -tan(latrad)*tan(sdec)    
       arg  = where(arg.lt.-argtst, -argval, arg)
       arg  = where(arg.gt. argtst,  argval, arg)
       ws          = acos(arg)                     ; acos(-tan(latrad)*tan(sdec)) ; eq 25 [ntim] 
       daylightmax = con*ws                        ; eq 34

   else if (ranklat.eq.1) then
       daylightmax = new( (/ntim,nlat/), typeof(pi), "No_FillValue")

       do nl=0,nlat-1
          arg  = -tan(latrad(nl))*tan(sdec)
          arg  = where(arg.lt.-argtst, -argval, arg)
          arg  = where(arg.gt. argtst,  argval, arg)
          ws   = acos(arg)                         ; acos(-tan(latrad(nl))*tan(sdec)) ; eq 25 [ntim] 
          daylightmax(:,nl) = con*ws               ; eq 34
       end do			       
                                                   ; handle meta data
       copy_VarCoords(jday, daylightmax(:,0))
       copy_VarCoords( lat, daylightmax(0,:))  

   else if (ranklat.eq.2) then
            daylightmax   = new( (/ntim,nlat,mlon/), typeof(pi), "No_FillValue") 

            do nl=0,nlat-1
              do ml=0,mlon-1
                 arg  = -tan(latrad(nl,ml))*tan(sdec)
                 arg  = where(arg.lt.-argtst, -argval, arg)
                 arg  = where(arg.gt. argtst,  argval, arg)
                 ws   = acos(arg)                  ; acos(-tan(latrad(nl,ml))*tan(sdec)) ; eq 25 [ntim] 
                 daylightmax(:,nl) = con*ws        ; eq 34
              end do			       
            end do			       
                                                   ; handle meta data
            copy_VarCoords(jday, daylightmax(:,0,0))
            copy_VarCoords( lat, daylightmax(0,:,:))
        end if
     end if
   end if

; Change NaNs ==> 0

   FillValue = totype(1e20, typeof(daylightmax))
   zero        = totype( 0, typeof(daylightmax))
   
   if (any(daylightmax.lt.0)) then
       daylightmax@_FillValue = FillValue
       daylightmax = where(daylightmax.lt.zero, daylightmax@_FillValue, daylightmax)
   end if
   if (any(isnan_ieee(daylightmax))) then
       daylightmax = where (isnan_ieee(daylightmax), daylightmax@_FillValue , daylightmax) 
   end if

   daylightmax@long_name = "maximum day light hours: FAO_56"
   daylightmax@units     = "hours/day"
   daylightmax@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   daylightmax@info      = "FAO 56; EQN 34; daylight_fao56"

; force return of type 'float'

   if (typeof(daylightmax).eq."float") then
       return(daylightmax)
   else
       DAYLIGHTMAX = tofloat(daylightmax)
       copy_VarMeta(daylightmax, DAYLIGHTMAX)
       return(DAYLIGHTMAX)
   end if
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef ("radext_fao56")
function radext_fao56(jday[*]:integer, lat:numeric, ounit[1]:integer)

; Compute extra terrestrial radiation; EQN 21   ; return as type 'float' 
; http://www.fao.org/docrep/X0490E/x0490e07.htm
;
; jday - day of year 
; lat  - degrees_{north/south}; can be a scalar, [*], or [*][*]
;        ***For abs(lat) > 55 the eqns have limited validity ***
;        The reason for the argtst and argval variables is an 
;        arbitrary attempt to minimize the effect poleward of (say) 55 
;
local pi, pi2, con, mnday, rad, gsc, rdist, ntim, nl, dimlat, ranklat     \
    , sdec, s, pi2yr, latrad, radextday, nlat, mlon, arg, zero, RADEXTDAY \
    , argtst, argval, FillValue
begin
   if (typeof(lat).eq."double") then
       pi  = 4.0d*atan(1.0d)         
       gsc = 0.0820d           ; solar constant: MJ/(m2-min)
   else
       pi  = 4.0*atan(1.0)         
       gsc = 0.0820            ; solar constant: MJ/(m2-min)
   end if
                               ; constants
   pi2    = 2*pi  
   rad    = pi/180
   mnday  = 1440               ; minutes/day (24*60)

                               ; dimensional stuff
   ntim   = dimsizes(jday)     ; # time time steps
   dimlat = dimsizes(lat)
   ranklat= dimsizes(dimlat)
   if (ranklat.ge.3) then
       print("radext_fao56: latitude rank exceeds 2: FATAL: ranklat="+ranklat)
       exit
   end if

   nlat   = dimlat(0)
   if (ranklat.eq.2) then
       mlon = dimlat(1)
   end if

   con    = (mnday/pi)         ; units conversation   
   pi2yr  = pi2/365  
   latrad = lat*rad            ; [nlat] or [nlat,mlon]

   rdist  = 1 + 0.033*cos(pi2yr*jday)       ; eq 23 [ntim]
   sdec   = 0.409*sin(pi2yr*jday - 1.39)    ; eq 24 [ntim]

   argtst = 1                               ; arbitrary value based on heuristic test
   argval = 0.99999                         ; arbitrary value to allow completetion

   if (nlat.eq.1 .and. ntim.eq.1) then      ; both input are scalar
       arg := -tan(latrad)*tan(sdec)    
       arg  = where(arg.lt.-argtst, -argval, arg)
       arg  = where(arg.gt. argtst,  argval, arg)
       ws   = acos(arg)                     ; acos(-tan(latrad(nl))*tan(sdec)) ; eq 25 [ntim] 
       radextday = con*gsc*rdist*(ws*sin(latrad)*sin(sdec)  \
                            +cos( latrad)*cos(sdec)*sin(ws) ) 
   else if (ranklat.eq.1) then
       radextday = new( (/ntim,nlat/), typeof(pi) , "No_FillValue")

       do nl=0,nlat-1
          arg   := -tan(latrad(nl))*tan(sdec)
          arg    = where(arg.lt.-argtst, -argval, arg)
          arg    = where(arg.gt. argtst,  argval, arg)
          ws     = acos(arg)                 ; acos(-tan(latrad(nl))*tan(sdec)) ; eq 25 [ntim] 
          radextday(:,nl) = con*gsc*rdist*(ws*sin(latrad(nl))*sin(sdec)  \
                               +cos( latrad(nl))*cos(sdec)*sin(ws) ) 
       end do			       
                                             ; handle meta data
       copy_VarCoords(jday, radextday(:,0))
       copy_VarCoords( lat, radextday(0,:))  ; this does not seem to work!!!  

       if (.not.isdimnamed(radextday,1) .and. isdimnamed(lat,0)) then
           radextday!1 = lat!0               ; fix above issue 
       end if
       
   else if (ranklat.eq.2) then
            radextday   = new( (/ntim,nlat,mlon/), "float", "No_FillValue") 

            do nl=0,nlat-1
              do ml=0,mlon-1
                 arg := -tan(latrad(nl,ml))*tan(sdec)
                 arg  = where(arg.lt.-argtst, -argval, arg)
                 arg  = where(arg.gt. argtst,  argval, arg)
                 ws   = acos(arg)      ; acos(-tan(latrad(nl,ml))*tan(sdec)) ; eq 25 [ntim] 
                 radextday(:,nl,ml) = con*gsc*rdist*(ws*sin(latrad(nl,ml))*sin(sdec)  \
                                         +cos( latrad(nl,ml))*cos(sdec)*sin(ws) )
              end do			       
            end do			       
                                             ; handle meta data
            copy_VarCoords(jday, radextday(:,0,0))
            copy_VarCoords( lat, radextday(0,:,:))
        end if   ; ranklat.eq.2
     end if      ; ranklat.eq.1
   end if        ; scalar; nlat.eq.1 .and. ntim.eq.1

; Change NaNs ==> _FillValue

   FillValue = totype(1e20, typeof(radextday))
   zero      = totype( 0  , typeof(radextday))

   if (any(radextday.lt.0)) then
       radextday@_FillValue = FillValue
       radextday = where(radextday.lt.zero, radextday@_FillValue, radextday)
   end if
   if (any(isnan_ieee(radextday))) then
       radextday = where (isnan_ieee(radextday), radextday@_FillValue , radextday) 
   end if
 
; Units

   ra_unit = "MJ/(m2-day)"                  ; ounit=1

   if (ounit.eq.0) then
       radextday = (/ radextday*0.408 /)    ; MJ/(m2-day) ==> mm/day
       ra_unit   = "mm/day"
   end if

   if (ounit.eq.2) then
       radextday = (/ radextday*11.574/)    ; MJ/(m2-day) ==> W/m2
       ra_unit   = "W/m2"
   end if
   
   radextday@long_name = "extra terrestrial radiation: FAO_56"
   radextday@units     = ra_unit
   radextday@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   radextday@info      = "FAO 56; EQN 21; radext_fao56"

 ;;if (isatt(radextday,"jday")) then
 ;;    delete(radextday@jday)
 ;;end if

 ;;if (isatt(radextday,"time")) then
 ;;    delete(radextday@time)
 ;;end if

; force return of type 'float'

   if (typeof(radextday).eq."float") then
       return(radextday)
   else
       RADEXTDAY = tofloat(radextday)
       copy_VarMeta(radextday, RADEXTDAY)
       return(RADEXTDAY)
   end if
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef ("radsol_fao56")
function radsol_fao56(radext:numeric, sunhrx:numeric, sunhr:numeric, iounit[2]:integer, opt[1]:logical)
;
; Similar to 'radsol2_fao56 except that the arguments 'radext' and 'sunhrx' are input
; Avoids repetitive computations
;
; Compute solar radiation; EQN 35
; http://www.fao.org/docrep/X0490E/x0490e07.htm
;=======
; This is the 'Angstrom Formula' which relates solar radiation to extraterrestrial
; radiation and relative sunshine duration.
;
local as, bs, units, radsol, cunits, conrad 
begin

   as    = 0.25                            ; default; Chap 3, pg 23
   bs    = 0.50
   if (opt .and. isatt(opt, "as")) then
       as := opt@as
   end if
   if (opt .and. isatt(opt, "bs")) then
       bs := opt@bs
   end if
   if (opt .and. isatt(opt, "as_bs") .and. dimsizes(opt@as_bs).eq.2) then
       as := opt@as_bs(0)
       bs := opt@as_bs(1)
   end if

   if (iounit(0).eq.0) then
       conrad = 2.45068                    ; mm/day => MJ/(m2-day)
   end if
   if (iounit(0).eq.1) then
       conrad = 1.00                       ; MJ/(m2-day)
   end if
   if (iounit(0).eq.2) then
       conrad = 0.0864                     ; W/m2   => MJ/(m2-day)
   end if
                                           ; EQN 35; Rs
   if (all(sunhrx.gt.0)) then
       radsol = (as+bs*(sunhr/sunhrx))*(radext*conrad)  
   else                                    ; avoid division by zero
       sunx   = where(sunhrx.le.0, 1e20, sunhrx)
       sunx@_FillValue = 1e20
       radsol = (as+bs*(sunhr/sunx))*(radext*conrad)  
       radsol = where(ismissing(radsol), 0, radsol)   
   end if

                                           ; EQN 35; Rs
;;;if (iounit(0).eq.0) then                     
;;;   radsol = (as+bs*(sunhr/sunhrx))*(radext*2.45068)  ; mm/day => MJ/(m2-day)
;;;else if (iounit(0).eq.1) then                     
;;;    radsol = (as+bs*(sunhr/sunhrx))*radext           ; MJ/(m2-day)  
;;;else if (iounit(0).eq.2) then                     
;;;    radsol = (as+bs*(sunhr/sunhrx))*(radext*0.0864)  ; W/m2   => MJ/(m2-day)  
;;;   end if
;;; end if
;;;end if

   if (iounit(1).eq.0) then                     
       radsol = (/ radsol*0.408 /)                      ; MJ/(m2-day) => mm/day
       units = "mm/day"
   end if

   if (iounit(1).eq.1) then                     
       units  = "MJ/(m2-day)"                          ; ounit=1
   end if
                                                
   if (iounit(1).eq.2) then                     
       radsol = (/ radsol*11.574 /)                    ; MJ/(m2-day) ==> W/m2
       units = "W/m2"    
   end if

   radsol@long_name  = "solar radiation: FAO_56"
   radsol@units      =  units
   radsol@url        = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   radsol@info       = "FAO 56; EQN 35; radsol_fao56"
   radsol@parameters = "as="+as+"  bs="+bs 

   copy_VarCoords(radext, radsol)

   return(radsol)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef ("radsol2_fao56")
function radsol2_fao56(jday[*]:integer, lat:numeric, sunhr:numeric, ounit[1]:integer, opt[1]:logical)

; Compute solar radiation; EQN 35
; http://www.fao.org/docrep/X0490E/x0490e07.htm
;=======
; This is the 'Angstrom Formula' which relates solar radiation to extraterrestrial
; radiation and relative sunshine duration.
;=======
; For abs(lat) > 55 the eqns have limited validity
;=======
;
; julian day of current year
; lat can be a scalar, [*], or [*][*]
;
local as, bs, sunhrx, raext, radsol2, units 
begin

   sunhrx = daylight_fao56(jday, lat)      ; max daylight/sun; hr per day
   radext = radext_fao56(jday, lat, 1)     ; radiation MJ/(m2-day)
   
   as    = 0.25                            ; default; Chap 3, pg 23
   bs    = 0.50
   if (opt .and. isatt(opt, "as")) then
       as := opt@as
   end if
   if (opt .and. isatt(opt, "bs")) then
       bs := opt@bs
   end if
   if (opt .and. isatt(opt, "as_bs") .and. dimsizes(opt@as_bs).eq.2) then
       as := opt@as_bs(0)
       bs := opt@as_bs(1)
   end if

   radsol2 = (as+bs*(sunhr/sunhrx))*radext ; MJ/(m2-day)   ; EQN 35; Rs

   if (ounit.eq.0) then                     
       radsol2 = (/ radsol2*0.408 /)       ; MJ/(m2-day) ==? mm/day
       units = "mm/day"
   end if

   if (ounit.eq.1) then                     
       units  = "MJ/(m2-day)"              ; ounit=1
   end if
                                                
   if (ounit.eq.2) then                     
       radsol2 = (/ radsol2*11.574 /)      ; MJ/(m2-day) ==> W/m2
       units = "W/m2"    
   end if

   radsol2@long_name  = "solar radiation: FAO_56"
   radsol2@units      =  units
   radsol2@url        = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   radsol2@info       = "FAO 56; EQN 35; radsol2_fao56"
   radsol2@parameters = "as="+as+"  bs="+bs 

   copy_VarCoords(radext, radsol2)

   return(radsol2)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("radsol3_hargreaves_fao56")
function radsol3_hargreaves_fao56(tmin:numeric, tmax:numeric, radext:numeric, krs:numeric, iounit[3]:integer)
;
; Solar radiation derived from air temp differences
; FAO 56: EQN 50
;
; Another usage is: http://dx.doi.org/10.4236/as.2013.48A008
;
; tmin, tmax - degC
; radsol     - MJ/(m2-day)
;
;   iounit   - option for input [iounit(0)] and returned units [iounit(1)]
;              iounit(0) =0: degC          ; <b>input</b> units: <em>tmin/tmax</em>
;                        =1: degK
;                        =2: deg farenheit
;
;              iounit(1) =0: mm/day        ; <b>input</b>: <em>rex</em>
;                        =1: MJ/(M2-day)
;                        =2: W/m2
;
;              iounit(2) =0: mm/day        ; <b>output</b> (returned) units
;                        =1: MJ/(M2-day)
;                        =2: W/m2
;
; Allen et al [FAO-56] recommended that estimates made via this method bbe averaged over 
; longer time periods, from several days to a week to a month, to remove errors 
; associated with daily estimates.
; 
; See also comments made in:
; Evaluation of alternative methods for estimating reference evapotranspiration
; Daniel K. Fisher1, H. C. Pringle III
; Vol.4, No.8A, 51-60 (2013) Agricultural Sciences
; http://dx.doi.org/10.4236/as.2013.48A008
;
local conext, consol, tmn, tmx, radsol_hg, units
begin

   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then               ; iounit error check
       if (iounit(0).lt.0 .or. iounit(0).gt.2) then
           print("radsol3_hargreaves_fao56: unrecognized iounit(0) argument: iounit(0)="+iounit(0))
       end if
       if (iounit(1).lt.0 .or. iounit(1).gt.2) then
           print("radsol3_hargreaves_fao56: unrecognized iounit(1) argument: iounit(1)="+iounit(1))
       end if
       if (iounit(2).lt.0 .or. iounit(2).gt.2) then
           print("radsol3_hargreaves_fao56: unrecognized iounit(2) argument: iounit(2)="+iounit(2))
       end if
       exit
   end if

; constant used to change input radext units to MJ/(m2=day)

   if (iounit(1).eq.0) then                                
       conext = 2.45068                              ; mm/day ==> MJ/(m2-day)
   else if (iounit(1).eq.1) then                                
       conext = 1.0                                  ; no units change: MJ/(m2-day)
   else if (iounit(1).eq.2) then                                
       conext = 0.0864                               ; W/m2   ==> MJ/(m2-day)
     end if
    end if
   end if

   if (iounit(0).eq.0 .or. iounit(0).eq.1) then      ; degC or degK
       radsol3 = sqrt(tmax-tmin)         
   end if

   if (iounit(0).eq.2) then                          ; F
     ;;radsol3 = sqrt(((tmax-32)-(tmin-32))*0.55556)
       radsol3 = sqrt((tmax-tmin)*0.55556)
   end if

; radsol computation  [ degC, MJ/(m2=day) ]

   radsol3 = krs*radsol3*(radext*conext)             ; use converted values

; change to desired output units of radsol3

   if (iounit(2).eq.0) then                                
       consol = 0.408                                ; MJ/(m2-day) ==> mm/day
       units  = "mm/day"
   else if (iounit(2).eq.1) then                                
       consol = 1.0                                  ; no units change
       units  = "MJ/(m2-day)"
   else if (iounit(2).eq.2) then                                
       consol = 11.574                               ; MJ/(m2-day) ==> W/m2
       units  = "W/m2"          
     end if
    end if
   end if

   if (consol.ne.1.0) then
       radsol3 = radsol3*consol
   end if

   radsol3@long_name  = "solar radiation: Hargreaves"
   radsol3@units      =  units
   radsol3@url        = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   radsol3@info       = "FAQ 56: EQN 50; radsol3_hargreaves_fao56"

   copy_VarCoords(tmin, radsol3)
   return(radsol3)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("refevt_hargreaves_fao56")
function refevt_hargreaves_fao56(tmin:numeric, tmax:numeric, radext:numeric, iounit[3]:integer)

; Use the simple Hargreaves method to derive reference evapotranspiration (ETo; here, evt)
; This is from the FAO-56 [Food & Agriculture Organization] standard; EQN 52
; Hargreaves and Semani (1985)
;
; FAQ 56: This is an alternative to estimare ETo/evt when weather data are not avaliable.
;
; See also:
; Evaluation of alternative methods for estimating reference evapotranspiration
; Daniel K. Fisher, H. C. Pringle III
; Vol.4, No.8A, 51-60 (2013) Agricultural Sciences
; http://dx.doi.org/10.4236/as.2013.48A008
;
; NOTE:   This has a tendency to underestimate evapotranspiration under high wind 
;         conditions (U2 > 3/m/s) and overestimate evapotranspiration under 
;         conditions of high humidity
;
; All input arrays (tmin, tmax, rex) must be the same size (conform).

; nomenclature:
;   tmin, tmax    - temperature at 2-meters (reference height)
;   radext        - extra-terrestrial solar radiation [mm/day]
;
;   iounit        - option for input [iounit(0)] and returned units [iounit(1)]
;                  iounit(0) =0: degC          ; <b>input</b> units: <em>tmin/tmax</em>
;                            =1: degK    
;                            =2: deg farenheit
;
;                  iounit(1) =0: mm/day        ; <b>input</b>: <em>rex</em>
;                            =1: MJ/(M2-day) 
;                            =2: W/m2         
;
;                  iounit(2) =0: mm/day        ; <b>output</b> (returned) units
;                            =1: MJ/(M2-day)
;                            =2: W/m2

local units, tavg, trng, conu, evt, tk0, tCrit
begin
   
   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then               ; iounit error check
       if (iounit(0).lt.0 .or. iounit(0).gt.2) then             
           print("refevt_hargreaves_fao56: unrecognized iounit(0) argument: iounit(0)="+iounit(0))
       end if
       if (iounit(1).lt.0 .or. iounit(1).gt.2) then               
           print("refevt_hargreaves_fao56: unrecognized iounit(1) argument: iounit(1)="+iounit(1))
       end if
       if (iounit(2).lt.0 .or. iounit(2).gt.2) then               
           print("refevt_hargreaves_fao56: unrecognized iounit(2) argument: iounit(2)="+iounit(2))
       end if
       exit
   end if

   if (isatt(tmin,"_FillValue")) then
       tFill = tmin@_FillValue
   else
       tFill = 1e20
   end if

   conu  = 0.0023
   tk0   = 273.16                         ; degK
   tCrit = 0                              ; degC

   if (iounit(0).eq.0) then               ; deC
       tavg = (tmax+tmin)*0.5             ; local arrays                
       trng =  tmax-tmin

       evt  = where(tmin.ge.tCrit  \      ; standard formula
                   ,conu*(tavg+17.8)*sqrt(trng)*radext, tFill)

       if (any(tmin.lt.tCrit)) then         ; special case; partial evt
           tavg = (tmax+tCrit)*0.5
           trng =  tmax-tCrit
           evt  = where(tmin.le.tCrit .and. tmax.gt.tCrit   \
                       ,conu*(tavg+17.8)*sqrt(trng)*radext, evt)
       end if
   else
       if (iounit(0).eq.1) then           ; degK
           tmn  = tmin-tk0                ; convert to degC (local arrays)
           tmx  = tmax-tk0
       else                               ; deg Farenheit
           tmn  = (tmin-32)*0.555556      ; convert to degC (local arrays)
           tmx  = (tmax-32)*0.555556
       end if
       tavg = (tmn+tmx)*0.5               ; degC       
       trng =  tmx-tmn

       evt  = where(tmn.ge.tCrit                         \
                   ,conu*(tavg+17.8)*sqrt(trng)*radext, tFill)

       if (any(tmn.lt.tCrit)) then     ; special case; partial evt
           tavg = (tmx+tCrit)*0.5
           trng =  tmx-tCrit
           evt  = where(tmn.le.tCrit .and. tmx.gt.tCrit   \
                       ,conu*(tavg+17.8)*sqrt(trng)*radext, evt)
       end if
   end if
                                       ; units of input 'rex'
                                       ; iounit(1)=0  ... mm/day
   if (iounit(1).eq.1) then            
       evt = evt*0.408                 ; MJ/(m2-day) => mm/day
   end if

   if (iounit(1).eq.2) then            
       evt = evt*0.035 ; 1/28.4        ; W/m2 => mm/day
   end if

  ;if (opt .and. isatt(opt,"partial_evt") .and. .not.opt@partial_evt) then
  ;end if

   evt@long_name = "reference evapotranspiration: Hargreaves "
   evt@units     = "mm/day"
                                       ; standard eqn 52 

   if (iounit(2).eq.1) then            ; 1 mm/day = 2.45 MJ/(m2-day)
       evt = (/ evt*2.45098 /)         ;   mm/day => MJ/(m2-day)
       evt@units = "MJ/(m2-day)"
   end if

   if (iounit(2).eq.2) then            ; 1 mm/day = 28.4 W/m2
       evt = (/ evt*28.4 /)            ;   mm/day => W/m2
       evt@units = "W/m2"
   end if
    
   evt@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   evt@info      = "FAO 56; EQN 52; refevt_hargreaves_fao56"

   if (.not.isatt(evt,"_FillValue") .and. any(evt.eq.tFill) ) then
       evt@_FillValue = tFill
   end if

   copy_VarCoords(tmin, evt)
   return(evt)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("radsol_clrsky_fao56")
function radsol_clrsky_fao56(radext:numeric, opt[1]:logical)
;
; FAO 56: EQN 36 or 37
;
; (as+bs) => fraction of extraterrestrial radiation 
;            reaching earth on a clear sky day
;            sunhr=sunhrx
local as, bs, z, rso, info
begin
   as = 0.25
   bs = 0.50
   if (opt .and. isatt(opt,"as") .and. isatt(opt,"bs")) then
       as  = opt@as
       bs  = opt@bs
   end if
                                                ; rso: clear sky radiation
   if (opt .and. isatt(opt,"z")) then           ; z = 'station' elevarion
       rso   = (0.75 + 2e-05*(opt@z))*radext    ; EQN 37 
       info  = "FAO 56; EQN 37"  
   else
       rso   = (as+bs)*radext                   ; EQN 36
       info  = "FAO 56; EQN 36"  
   end if

   rso@long_name  = "clear sky radiation"       ; (sunhr/sunhrx)=1
   rso@units      = "MJ/(m2-day)"
   rso@url        = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   rso@info       =  info+"; radsol_clrsky_fao56"
   rso@parameters = "as="+as+"  bs="+bs 

   copy_VarCoords(radext, rso)
   return(rso)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("netsw_fao56")
function netsw_fao56(radsol:numeric, albedo:numeric)
;
; Compute net solor (shortwave) radiation
; FAO 56: EQN 38
;
local netsw, wmsg   
begin
   if (all(albedo.le.1.0)) then
       netsw = (1.0-albedo)*radsol
       wmsg  = ""
   else
       netsw = radsol
       wmsg  = "WARNING: albedo>1.0 encountered: netsw = radsol"
   end if

   netsw@long_name  = "net solar (shortwave) radiation"
   netsw@units      = radsol@units
   netsw@url        = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   netsw@info       = "FAQ 56: EQN 38; netsw_fao56"
   if (wmsg.ne."") then
       netsw@WARN_MSG = wmsg 
   end if

   copy_VarCoords(radsol, netsw)

   return(netsw)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("netrad_fao56")
function netrad_fao56(netsw:numeric, netlw:numeric)
;
; Compute net radiation
; FAO 56: EQN 40
;
local netrad   
begin
   netrad        = netsw - netlw                       ; EQN 40

   if (isatt(netsw,"units") .and. isatt(netlw,"units") .and. \
       netsw@units.eq.netlw@units ) then
       netrad@units  = netsw@units
   else
       print("netrad_fao56: WARNING")
       print("              units attribute not present or unit-mismatch")
       print("              netsw@units="+netsw@units)
       print("              netlw@units="+netlw@units)
   end if

   netrad@long_name = "net radiation"
   netrad@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   netrad@info      = "FAQ 56: EQN 40; netrad_fao56"

   copy_VarCoords(netsw, netrad)
   return(netrad)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("netlw_fao56")
function netlw_fao56(tmin, tmax, ea, radext, radsol, iounit[4]:integer, opt[1]:logical)

; http://www.fao.org/docrep/X0490E/x0490e07.htm
; Compute net longwave radiation ; EQN 39
; 
;   iounit        - option for input [iounit(0)] and returned units [iounit(1)]
;                   iounit(0) =0: degC          ; <b>input</b> units: <em>tmin/tmax</em>
;                             =1: degK    
;                             =2: deg farenheit
;                   iounit(1) =0: hPa
;                             =1: Pa
;                             =2: kPa
;                   iounit(2) =0: mm/day
;                             =1: MJ/(m2-day)
;                             =2: W/m2  
;                   iounit(3) =0: mm/day        ; <b>output</b> (returned) units
;                             =1: MJ/(M2-day)
;                             =2: W/m2

local tmn, tmx, sbc, t0, eau, ru, as, bs, rso, netlw, units
begin
   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then       ; iounit error check
       print("netlw_fao56: unrecognized tunit argument: iounit="+iounit)
       exit
   end if

   t0    = 273.16                               ; K
   sbc   = 4.903e-09                            ; MJ/(K4-m2-day) ; Stefan-Boltzmann constant
   eau   = (/0.10, 0.001, 1.0 /)                ; ea   unit convert: (0) hPa->kPa; 
                                                ;                    (1)  Pa->kPa; 
                                                ;                    (2) kPa->kPa
   ru    = (/2.4507, 1.0, 0.0864/)              ; rad* unit convert: (0) mm/day->MJ/(m2-day);
                                                ;                    (1) MJ/(m2-day)
                                                ;                    (2) W/m2->MJ/(m2-day)

   rso   = radsol_clrsky_fao56(radext, opt)     ; clear sky radiation [ EQN 36 ]
   ratio = radsol/rso
 ;;rso   = radsol_clrsky_fao56(radext*ru(iounit(2), opt)     ; clear sky radiation [ EQN 36 ]
 ;;ratio = radsol*ru(iounit(2)/rso

   if (any(ratio.gt.1.0)) then
       print("netlw_fao56: WARNING: ratio="+ratio+" ; radsol="+radsol+" ; rso="+rso)
       print("           : WARNING: ratio = (radsol/rso) > 1.0")
       ratio = ratio > 1.0                      ; clip at 1.0
   end if 

   if (iounit(0).eq.0 .or. iounit(0).eq.2) then
       if (iounit(0).eq.0) then                 ; degC
           tmn = tmin + t0
           tmx = tmax + t0                      ; C+t0 =>degK
       else
           tmn = (tmin-32)*0.5555556 + t0       ; F+t0 =>degK
           tmx = (tmax-32)*0.5555556 + t0
       end if
               ; EQN 39                         ; use local converted temperature values
       netlw   = 0.5*sbc*(tmn^4 + tmx^4)   \             ; part1: SB term
                *(0.34-0.14*sqrt(ea*eau(iounit(1)))) \   ; part2: correction for humidity
                *(1.35*(ratio)-0.35)                     ; part3: effect of cloudiness
   else
               ; EQN 39                         ; use input degK
       netlw   = 0.5*sbc*(tmin^4 + tmax^4)           \   ; part1: SB term
                *(0.34-0.14*sqrt(ea*eau(iounit(1)))) \   ; part2: correction for humidity
                *(1.35*(ratio)-0.35)                     ; part3: effect of cloudiness
   end if

   if (iounit(3).eq.0) then                     
       netlw = (/ netlw*0.408 /)                ; MJ/(m2-day) ==? mm/day
       units = "mm/day"
   end if

   if (iounit(3).eq.1) then                     
       units = "MJ/(m2-day)"
   end if
                                                
   if (iounit(3).eq.2) then                     
       netlw = (/ netlw*11.574 /)               ;  MJ/(m2-day) ==> W/m2
       units = "W/m2"    
   end if

   netlw@long_name = "net long wave radiation: FAO_39"
   netlw@units     = units
   netlw@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   netlw@info      = "FAO 56; EQN 39; netlw_fao56"

   copy_VarCoords(tmax, netlw)

   return(netlw)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("soil_heatflux_month_fao56")
function soil_heatflux_month_fao56(T:numeric, dt:numeric, dz:numeric, ndim[1]:integer, iounit[2]:integer, opt[1]:logical)
;
; Compute *monthly* soil flux
; FAO 56: EQN 41, EQN 43&44
;
; Note: Soil heat flux is small compared to net radiation.
;
; T      - soil temperature (C); any dimensionality
; dt     - monthly time step [ generally 1.0 ]
; dz     - effective soil depth (m); for montly periods 0.5 to 2 meters; use 1.0
; ndim   - number of the 'time' dimension
; iounit - units of input  T: 0=degC, 1=degK, 2=F 
;          units of output flux: 0=mm/day; 1=MJ/(m2-day); 2=W/m2
; opt    - optional arguments .. currently not used ... set to False
;
local con, units, soil_heatflux 
begin
   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then       ; iounit error check
       print("netlw_fao56: unrecognized tunit argument: iounit="+iounit)
       exit
   end if

   con = 0.14*dz   

   if (iounit(0).eq.1 .or. iounit(0).eq.2) then
                                                ; use converted temperatures
       if (iounit(0).eq.1) then                 ; degK
                                                ; degK -> degC
           soil_heatflux = con*center_finite_diff_n ((T-273.16),dt,False,0,ndim)
       else
                                                ; F => degC
           soil_heatflux = con*center_finite_diff_n ((T-32)*0.5555556,dt,False,0,ndim)
       end if
   else
               ; EQN 43 & 44                    ; use input temperatures (C)
       soil_heatflux = con*center_finite_diff_n (T ,dt,False,0,ndim)
   end if

   if (iounit(1).eq.0) then                     
       soil_heatflux = (/ soil_heatflux*0.408 /)   ; MJ/(m2-day) ==? mm/day
       units = "mm/day"
   end if

   if (iounit(1).eq.1) then                     
       units = "MJ/(m2-day)"
   end if

   if (iounit(1).eq.2) then                     
       soil_heatflux = (/ soil_heatflux*11.574 /)  ; MJ/(m2-day) == W/m2   
       units = "mm/day"
   end if

   soil_heatflux@long_name = "monthly soil heat flux"
   soil_heatflux@units     =  units
   soil_heatflux@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
   soil_heatflux@info      = "FAO 56; EQN 41, 43 & 44; soil_heatflux_month_fao56"

   copy_VarCoords(T, soil_heatflux)

   return(soil_heatflux)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("psychro_fao56")
function psychro_fao56(p:numeric, iounit[2]:integer)
;
; psychrometric constant
;
local psy, con
begin
  con = 0.66474e-3                  ;  units: (kPa/C)

  if (iounit(0).eq.0) then          ; hPa -> kPa
      con  = con * 0.1             
  end if 
  if (iounit(0).eq.1) then          ; Pa  -> kPa
       con  = con * 0.001
  end if

  psy  = con* p     
  psy@long_name = "psychrometric constant"

  psy@units     = "kPa/C" 
  psy@url       = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
  psy@info      = "FAO 56; EQN 8; psychro_fao56"

  if (iounit(1).eq.0) then          ; (kPa/C) -> (hPa/C)
      psy       = (/psy*10.0/)
      psy@units = "hPa/C" 
  end if 
  if (iounit(1).eq.1) then          ; Pa  -> kPa
      psy       = (/psy*1000.0/)
      psy@units = "Pa/C" 
  end if

  return(psy)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("refevt_penman_fao56")
function refevt_penman_fao56(Tmean:numeric, netrad:numeric, shflx:numeric, psychro:numeric \
                            ,u2:numeric, vprdef:numeric, slope:numeric\
                            ,albedo:numeric, cnumer:numeric, cdenom:numeric, ounit[1]:integer )
;
; Penman-Monteith for daily or monthly: Example 17, Chap 4
;        albedo=   0.23  ; crop leaf albedo (*not* atmospheric or surface albedo)
;        cnumer= 900.0   ; cdenom=0.34   short crop
;        cnumer=1600.0   ; cdenom=0.38   tall crop
; or
;        albedo, cnumer and cdenom are function(time, space)   ; dimensionality must conform to Tmean
;
; NOTE: The 900 and 1600 are for daily or motthly; 
;       For hourly data use (900/24. or 1600/24.) and 
;       the units for 'netrad' and 'shflx' must be MJ/(m2-hour). 
;
local dimT, dim_cnumer, dim_cdenom, num1, num2, dnom, refevt
begin
  dimT = dimsizes(Tmean)
  if (.not.isscalar(cnumer)) then
      dim_cnumer = dimsizes(cnumer)
      if (.not.all(dimT.eq.dim_cnumer)) then
          print("refevt_penman_fao56: dimensions of cnumer do not conform the Tmean: Fatal")
          exit
      end if
  end if
  if (.not.isscalar(cdenom)) then
      dim_cdenom = dimsizes(cdenom)
      if (.not.all(dimT.eq.dim_cdenom)) then
          print("refevt_penman_fao56: dimensions of cdenom do not conform the Tmean: Fatal")
          exit
      end if
  end if

  num1   = 0.408*(netrad-shflx)*slope               ; numerator (left)
  num2   = psychro*(cnumer/(Tmean+273))*u2*vprdef     ; numerator (right)
  dnom   = slope + psychro*(1+cdenom*u2)              ; denominator
  refevt = (num1 + num2)/dnom

  refevt@long_name = "Penman-Monteith reference evapotranspiration (month)"
  refevt@units = "mm/day"
  refevt@url   = "http://www.fao.org/docrep/X0490E/x0490e07.htm"
  refevt@info  = "FAO 56; EQN 6; refevt_penman_fao56" 
  if (isscalar(cnumer)) then
      refevt@parameters = "cnumer="+cnumer+" cdenom="+cdenom
  else
      refevt@parameters = "cnumer, cdenom are variable"
  end if

  if (ounit.eq.1) then
      refevt = (/ refevt*2.45068 /)
      refevt@units = "MJ/(m2-day)"
  else if (ounit.eq.2) then
      refevt = (/ refevt*28.4 /)
      refevt@units = "W/m2"
   end if
  end if

  refevt = where(refevt.lt.0, 0, refevt)
  copy_VarCoords(Tmean, refevt)
  return(refevt)
end
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("crop_water_need")
function crop_water_need(pet:numeric, act_evap:numeric, opt[1]:logical)
;
; This is not in FAO-56 but is bundled within the FAO-56 library for convenience.
;
; http://www.physicalgeography.net/fundamentals/8j.html
; http://www.fao.org/docrep/s2022e/s2022e00.htm

local cwn
begin

  cwn = pet - act_evap

  cwn@long_name = "crop water need: PET-ActualEvap"
  if (isatt(pet,"units")) then
      cwn@units = pet@units
      copy_VarCoords(pet, cwn)
      return(cwn)
  else 
      if (isatt(act_evap,"units"))
          cwn@units = act_evap@units
          copy_VarCoords(act_evap, cwn)
          return(cwn)
      end if
  end if
  return(cwn)
end
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
undef("refevt_turc")
function refevt_turc(tmean:numeric, radsol:numeric, iounit[3]:integer)
;
; This is not in FAO-56 but is bundled within the FAO-56 library for convenience.
;
; Use the simple Turc method to derive reference evapotranspiration (ETo; here, evt)
;
; **NOTE**: Different papers indicate different units for Rs and ET0
;         ; The units cal/cm2/day are used in the equation.
;
; Reference:
;
; C.-Y. Xu, V. P. Singh 
; Evaluation and generalization of radiation-based methods for calculating evaporation
; Hydrol. Process .  14 , 339±349 (2000)
; http://folk.uio.no/chongyux/papers_SCI/HYP_4.pdf
;
; Turc L. 1961
; Estimation of irrigation water requirements, potential evapotranspiration: 
;   a simple climatic formula evolved up to date.
; Annals of Agronomy 12: 13-49
;
; Each input array (tmean, radsol) must be the same size (conform).
;
; nomenclature:
;   tmean         - temperature at 2-meters (reference height) 
;   radsol        - solar radiation 
;   iounit        - option for input [iounit(0 and 1)] and returned units [iounit(2)]
;                   iounit(0) =0: degC          ; <b>input</b> units: <em>tmin/tmax</em>
;                             =1: degK    
;                             =2: deg farenheit
;
;                   iounit(1) =0: mm/day        ; <b>input</b>: <em>radsol</em>
;                             =1: MJ/(M2-day) 
;                             =2: W/m2         
;                             =3: cal/(cm2-day)
;
;                   iounit(2) =0: mm/day        ; <b>output</b> (returned) units
;                             =1: MJ/(M2-day)
;                             =2: W/m2
;                             =3: cal/(cm2-day)
;   opt           - optional    

local units, tavg, conu, evt, tk0, tCrit
begin
   
   if (any(iounit.lt.0) .or. any(iounit.gt.3)) then               ; iounit error check
       if (iounit(0).lt.0 .or. iounit(0).gt.3) then             
           print("refevt_turc: unrecognized iounit(0) argument: iounit(0)="+iounit(0))
       end if
       if (iounit(1).lt.0 .or. iounit(1).gt.3) then               
           print("refevt_turc: unrecognized iounit(1) argument: iounit(1)="+iounit(1))
       end if
       if (iounit(2).lt.0 .or. iounit(2).gt.3) then               
           print("refevt_turc: unrecognized iounit(2) argument: iounit(2)="+iounit(2))
       end if
       exit
   end if

   if (isatt(tmean,"_FillValue")) then
       tFill = tmean@_FillValue
   else
       tFill = 1e20
   end if

   conu  = 0.40                           ; monthly  
   conu  = conu/30                        ; daily (0.013) 

                                          ; radsol ===> cal/(cm2-day)
   if (iounit(1).eq.0) then
       conrad = 58.5                      ; mm/day => cal/(cm2-day) 
   end if
   if (iounit(1).eq.1) then
       conrad = 23.9                      ; MJ/(m2-day) => cal/(cm2-day)
   end if
   if (iounit(1).eq.2) then
       conrad = 2.064                     ; W/m2   => cal/(cm2-day)
   end if
   if (iounit(1).eq.3) then
       conrad = 1.0                       ; cal/(cm2-day)
   end if

   tk0   = 273.16                         ; degK
   tCrit = 0                              ; degC

   if (iounit(0).eq.0) then               ; degC
       evt  = where(tmean.gt.tCrit  \     ; standard formula
                   ,conu*((tmean/(tmean+15))*((radsol*conrad)+50)), tFill)
   else
       if (iounit(0).eq.1) then           ; degK
           tavg = tmean-tk0               ; degC       
       else                               ; deg Farenheit
           tavg = (tmean-32)*0.5555556    ; degC
       end if

       evt  = where(tavg.ge.tCrit                         \
                   ,conu*((tmean/(tmean+15))*((radsol*conrad)+50)), tFill) 
   end if

   evt@long_name = "reference evapotranspiration: Turc Method "
   evt@units     = "mm/day"
   evt@info      = "no correction for low humidity"
   evt@url       = "L. Turc (1961): Water Requirements ...(Annales Agronomiques, 12, 13-49" 
   evt@reference_doi= "http://dx.doi.org/10.4236/as.2013.48A008"

                                       ; convert mm/day 
   if (iounit(2).eq.1) then            ; 1 mm/day =  2.45 MJ/(m2-day)
       evt = (/ evt*2.45098 /)         ;   mm/day => MJ/(m2-day)
       evt@units = "MJ/(m2-day)"
   end if

   if (iounit(2).eq.2) then            ; 1 mm/day =  28.4 W/m2
       evt = (/ evt*28.4 /)            ;   mm/day => W/m2
       evt@units = "W/m2"
   end if

   if (iounit(2).eq.3) then            ; 1 mm/day =  58.5 cal/(cm2-day)
       evt = (/ evt*58.5 /)            ;   mm/day => cal/(cm2-day)  
       evt@units = "cal/(cm2-day)"
   end if
   
   return(evt)
end
;-----
undef("refevt_turc_rh")
function refevt_turc_rh(tmean:numeric, radsol:numeric, rh:numeric, iounit[3]:integer)
;
; This is not in FAO-56 but is bundled within the FAO-56 library for convenience.
;
; Use the simple Turc method to derive reference evapotranspiration (ETo; here, evt)
; Exactly the same as refevt_turc' 
; BUT 
; it has a correction factor for low relative humidity ( RH<50% )
;
; Reference:
; C.-Y. Xu, V. P. Singh 
; Evaluation and generalization of radiation-based methods for calculating evaporation
; Hydrol. Process .  14 , 339±349 (2000)
; http://folk.uio.no/chongyux/papers_SCI/HYP_4.pdf
;
; Turc L. 1961
; Estimation of irrigation water requirements, potential evapotranspiration: 
;   a simple climatic formula evolved up to date.
; Annals of Agronomy 12: 13-49
;
; Each input array (tmean, radsol, rh) must be the same size (conform).

; nomenclature:
;   tmean         - temperature at 2-meters (reference height) 
;   radsol        - solar radiation 
;   rh            - relative humidity (%)
;   iounit        - option for input [iounit(0 and 1)] and returned units [iounit(2)]
;                   iounit(0) =0: degC          ; <b>input</b> units: <em>tmin/tmax</em>
;                             =1: degK    
;                             =2: deg farenheit
;
;                   iounit(1) =0: mm/day        ; <b>input</b>: <em>radsol</em>
;                             =1: MJ/(M2-day) 
;                             =2: W/m2         
;                             =3: cal/(cm2-day)
;
;                   iounit(2) =0: mm/day        ; <b>output</b> (returned) units
;                             =1: MJ/(M2-day)
;                             =2: W/m2
;                             =3: cal/(cm2-day)

local units, tavg, conu, rhcor, evt, tk0, tCrit
begin
   
   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then               ; iounit error check
       if (iounit(0).lt.0 .or. iounit(0).gt.2) then             
           print("refevt_turc_rh: unrecognized iounit(0) argument: iounit(0)="+iounit(0))
       end if
       if (iounit(1).lt.0 .or. iounit(1).gt.2) then               
           print("refevt_turc_rh: unrecognized iounit(1) argument: iounit(1)="+iounit(1))
       end if
       if (iounit(2).lt.0 .or. iounit(2).gt.2) then               
           print("refevt_turc_rh: unrecognized iounit(2) argument: iounit(2)="+iounit(2))
       end if
       exit
   end if

   if (isatt(tmean,"_FillValue")) then
       tFill = tmean@_FillValue
   else
       tFill = 1e20
   end if

   conu  = 0.40                           ; monthly  
   conu  = conu/30                        ; daily (0.013)  

                                          ; radsol ===> cal/(cm2-day)
   if (iounit(1).eq.0) then
       conrad = 58.5                      ; mm/day => cal/(cm2-day) 
   end if
   if (iounit(1).eq.1) then
       conrad = 23.9                      ; MJ/(m2-day) => cal/(cm2-day)
   end if
   if (iounit(1).eq.2) then
       conrad = 2.064                     ; W/m2   => cal/(cm2-day)
   end if
   if (iounit(1).eq.3) then
       conrad = 1.0                       ; cal/(cm2-day)
   end if

   tk0   = 273.16                         ; degK
   tCrit = 0                              ; degC

   rhcor = where(rh.ge.50, 1.0, (1+(50-rh)/70.0))  ; rh correction factor 
   conu  = conu*rhcor

   if (iounit(0).eq.0) then               ; degC
       evt  = where(tmean.gt.tCrit  \     ; standard formula
                   ,conu*((tmean/(tmean+15))*(radsol*conrad+50)), tFill)
   else
       if (iounit(0).eq.1) then           ; degK
           tavg = tmean-tk0               ; degC       
       else                               ; deg Farenheit
           tavg = (tmean-32)*0.5555556    ; degC
       end if

       evt  = where(tavg.ge.tCrit                         \
                   ,conu*((tmean/(tmean+15))*(radsol*conrad+50)), tFill) 
   end if

   evt@long_name = "reference evapotranspiration: Turc Method "
   evt@units     = "mm/day"
   evt@info      = "correction for relative humidity < 50%"
   evt@reference_doi = "http://dx.doi.org/10.4236/as.2013.48A008"

   if (iounit(2).eq.1) then            ; 1 mm/day = 2.45 MJ/(m2-day)
       evt = (/ evt*2.45098 /)         ;   mm/day => MJ/(m2-day)
       evt@units = "MJ/(m2-day)"
   end if

   if (iounit(2).eq.2) then            ; 1 mm/day = 28.4 W/m2
       evt = (/ evt*28.4 /)            ;   mm/day => W/m2
       evt@units = "W/m2"
   end if

   if (iounit(2).eq.3) then            ; 1 mm/day =  58.5 cal/(cm2-day)
       evt = (/ evt*58.5 /)            ;   mm/day => cal/(cm2-day)  
       evt@units = "cal/(cm2-day)"
   end if
   
   return(evt)
end
;-----
undef("areores_full_fao56")
function areores_full_fao56(zm:numeric, zom:numeric, zh:numeric, zoh:numeric \
                           ,d:numeric, uz:numeric, uzopt[1]:integer)
;
; Aerodynamic Resistance: http://agsys.cra-cin.it/tools/EvapoTranspiration/help/
; .   The transfer of heat and water vapour from the evaporating surface into the air 
; .   above the canopy is determined by the aerodynamic resistance.
; .   The wind speed measurement is assumed over clipped grass, even though 
; .   the reference type is tall.
;
; FAO-56: Equation 4
;         ra  - aerodynamic resistance [s m-1],
;         zm  - height of wind measurements [m],
;         zh  - height of humidity measurements [m],
;         d   - zero plane displacement height [m],
;         zom - roughness length governing momentum transfer [m],
;         zoh - roughness length governing transfer of heat and vapour [m],
;         k   -  von Karman's constant, 0.41 [-],
;         uz  -  wind speed at height z [m s-1]. [ (a+b*u2) ]
;
; Eqn (4) is for neutral stability conditions, i.e., where temperature, atmospheric 
; pressure, and wind velocity distributions follow nearly adiabatic conditions 
; (no heat exchange). 
;
local a, b, k2, uu, ra
begin
  a = 0.0                  ; open air (default)       
  b = 1.0
  if (uzopt.eq.1) then     ; greenhouse
      a = 1.0       
      b = 0.54
  end if
  k2 = 0.41^2              ; von Karman constant aquared 
  
  uu = a+b*u2
  if (.not.isatt(u2,"_FillValue")) then     
      uu@_FillValue = default_fillvalue(typeof(u2))
  end if
  uu = where(uu.lt.0.5, 0.5, uu)   ; recommended

  ra = (log((zm-d)/zom)*log((zh-d)/zoh))/(k2*uu)

  ra@long_name = "aerodynamic resistance"
  ra@units     = "s/m"
  ra@info      = "FAO 56; EQN 4; aerores_full_fao56"

  return(ra)
end
;-----
undef("areores_grass_fao56")
function areores_grass_fao56(ch:numeric, u2:numeric, aeopt[1]:integer)
;
; Technically: Aerodynamic resistance for a grass reference surface
; In practice: For a wide range of crops 
; ---- constant crop height of 0.12 m was used --------
; 
; FAO-56: Box 4
;         ra  - aerodynamic resistance [s m-1],
;         ch  - crop height [m],
;         u2  - wind speed at 2m [m/s]. [uz =< (a+b*u2) ]
; http://agsys.cra-cin.it/tools/EvapoTranspiration/help/
;         low wind constant c=665 is from here.
;  
;
local dimu2, ranku2, a, b, c, uu, ra, dims_abc, rank_abc, info2
begin

;---check which attributes (optional arguments) are present (kinda a 'pain') 
  if (aeopt) then               
      if (isatt(aeopt,"abc")) then                  
          dims_abc = dimsizes(aeopt@abc)
          rank_abc = dimsizes(dims_abc)
          if (rank_abc.eq.1 .and. dims_abc.eq.3) then
                               ; http://agsys.cra-cin.it/tools/EvapoTranspiration/help/
              a = aeopt@abc(0) ; 1.0  recommmended for low wind
              b = aeopt@abc(1) ; 0.54
              c = aeopt@abc(2) ; 665.0
              info2 = "a="+a+" b="+b+" c="+c
          else
              print("aerores_grass_fao56: attribute abc not as expected")
              print("                     must be rank=1 and length=3  ")
              print("                     rank_abc="+rank_abc)
              print("                     dims_abc="+dims_abc)
              exit
          end if
      else
          dimu2  = dimsizes(u2)          
          ranku2 = dimsizes(dimu2)
          if (isatt(aeopt,"a") .and. isatt(aeopt,"b") .and.isatt(aeopt,"c")) then
              a = aeopt@a                ; should match dimu2, ranku2
              b = aeopt@b
              c = aeopt@c
              info2 = "multidimensional a, b, c have been used"
          else
              print("aerores_grass_fao56: one or more attributes a/b/c not present")
              exit
          end if
      end if
  else                    ; aeopt=False
     a = 0.0              ; open air (default); aeopt=False       
     b = 1.0
     c = 208.0            ; fao-56
     info2 = "a="+a+" b="+b+" c="+c
  end if    
                          ; Values used by FAO-56 to derive approximation
 ;k     = 0.41            ; von Karman constant 
 ;k2    = k^2
 ;zm    = 2
 ;zh    = 2
 ;d     = 0.666667*ch     ; (2.0*ch)/3.0
 ;zom   = 0.123*ch
 ;zoh   = 0.1*zom
 ;ra    = areores_full_fao56(zm, zom, zh, zoh, d, u2, u2opt)

  conu  = 1.0             ; unit conversion
 ;if (iunit.eq.0) then
 ;    conu   =                            ; 
 ;else if (iunit.eq.1) then
 ;         conu  =
 ;     end if
 ;end if

  uu    = a+b*(u2*conu)                   ; uu must be m/s
 ;if (.not.isatt(u2,"_FillValue")) then     
 ;    uu@_FillValue = default_fillvalue(typeof(u2))
 ;end if
  uu    = where(uu.lt.0.5, 0.5, uu)   ; low wind "agsys.cra-cin.it/tools/..."
   
  ra    = c/uu
  ra@long_name = "aerodynamic resistance"
  ra@units     = "s/m"
  ra@info      = "FAO 56; BOX 4; aerores_grass_fao56"
  if (isvar("info2")) then
      ra@info2 = info2
  end if

  return(ra)
end
;++++++++++ PROCEDURE ++++++++++++++++++++++++++++++++++++++++
undef("radcon_p")      ; radcon_p: NOT USED
procedure radcon_p(x:numeric, units_in_row[1]:integer, units_out_col[1]:integer) 
;
;   FAO 56: TABLE 3. Conversion factors for radiation
;   multiplier to obtain energy received on a unit surface per unit time:  equivalent evaporation
;                 MJ/(m2-day) J/(cm2-day) cal/(cm2-day) W/m2     mm/day 
;  1 MJ/(m2-day)    1         100         23.9          11.6      0.408
;  1 cal/(cm2-day)  4.1868e-2 4.1868       1           0.485      0.0171
;  1 W/m2           0.0864    8.64        2.06           1        0.035
;  1 mm/day         2.45068   245         58.5         28.4        1
;  ---
;  radcon_p is a procedure; use to hange units of input variable
;  radcon_p( x, 0, 4 )    ; MJ/(m2-day) ==> mm/day
;  radcon_p( x, 3, 0 )    ;      mm/day ==> MJ/(m2-day)
;
local conv, units_row, units_col
begin
   conv = (/ (/      1.0,    100,   23.9 , 11.6  ,      0.408 /), \  ; row 0
             (/4.1868e-2, 4.1868,    1.0 ,  0.485,      0.0171/), \  ;     1
             (/   0.0864,   8.64,    2.06,  1.0  ,      0.035 /), \  ;     2
             (/     2.45,    245,   58.5 , 28.4  ,          1 /) /)  ;     3
             ;  col    0,      1,       2,    3  ,          4

  units_row = (/"MJ/(m2-day)","cal/(cm2-day)",         "W/m2", "mm/day"/)
  units_col = (/"MJ/(m2-day)","J/(cm2-day)"  ,"cal/(cm2-day)",  "W/m2", "mm/day"/)

   x       = x*conv(units_in_row, units_out_col)
   x@units = units_col(units_out_col)
end
