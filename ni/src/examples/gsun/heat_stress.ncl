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
                +c(5)*rh^2 + c(6)*t^2*rh  + c(7)*t*rh^2 + c(8)*(T^2)*(rh^2)  \
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
