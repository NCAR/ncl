; -------------------------------------------------
; This library requires that the "gsn_code.ncl" library
; be loaded prior to use by skewt_func.ncl
; -------------------------------------------------
; The following contains two separate versions of the
; skewT code. One was the original version The 2nd
; contains code that allows the skewT to be  paneled.
; Since the "wmbarb" code is 'low-level' it is not
; recognized by the NCL as a plot object. Hence,
; it can not be used in paneling.
; -------------------------------------------------
; Dennis Shea
; email: shea@ucar.edu
; ------------------------------------------------

undef("skewty")
function skewty(pres[*]:numeric)            ; y-coord given pressure (mb)
  begin                             
   return (132.182-44.061*log10(pres))
  end

undef("skewtx")
function skewtx(temp[*]:numeric,y[*]:numeric) ; x-coord given temperature (c)
  begin                                     ; y=skewty(p)
   return (0.54*temp+0.90692*y)
  end

undef ("getSkewTColorIndex") 
function getSkewTColorIndex (wks:graphic, color:string)
  local iColor
  begin
    if (color.eq."Foreground") then
        iColor = 1
    else
        iColor = NhlGetNamedColorIndex(wks,color)
    end if
    return( iColor )
  end 

;======================Original===============================
; Add needed named colors for skew T plots if output is going 
; to an NCGM. This is necessary with V6.1.0 and NCGM, because
; the new default color map in use and it doesn't have any 
; pale green in it. With PDF/PS/PNG, named colors don't need
; to be explicitly added.
;=============================================================
undef("fix_skewt_colormap_for_ncgm")
function fix_skewt_colormap_for_ncgm(wks,use_my_cmap[1]:logical)
local cmap, colors, ncmap, ncolors, rgb, ii, newcolors
begin
  cmap = gsn_retrieve_colormap(wks)
;
; Colors needed by skew T plots. We shouldn't need to add
; white/black as the color map should already have these.
;
  if(NhlClassName(wks).ne."ncgmWorkstationClass") then
    use_my_cmap = True
    return(cmap)
  end if

  colors  = (/"Tan","PaleGreen","RoyalBlue","Red","Sienna","Brown"/)
  ncmap   = dimsizes(cmap(:,0))
  ncolors = dimsizes(colors)
  if((ncmap+ncolors).le.255) then
    rgb = namedcolor2rgb(colors)
    ii = NhlNewColor(wks,rgb(:,0),rgb(:,1),rgb(:,2))
  else
    newcolors      = new(ncolors+2,string)
    newcolors(0:1) = (/"white","black"/)
    newcolors(2:)  = colors
    gsn_define_colormap(wks,newcolors)
  end if
  use_my_cmap = False
  
  return(cmap)
end

;======================Original===============================
undef("skewT_BackGround_Original")
function skewT_BackGround_Original (wks:graphic,  Opts:logical)
local copt
begin
; statement of purpose.
; ---------------------
; this program generates a skew-t, log p thermodynamic diagram.  this
; program was derived to reproduce the USAF skew-t, log p diagram.
;        (form dod-wpc 9-16-1  current as of march 1978)
; source.
; --------                    
; Tom Schlatter and Joe Wakefield [NOAA/PROFS] supplied fortran codes.
;
; history.
; --------                    
; don baker      01 jul 85    original version.  [NOAA/PROFS]
; don baker      01 dec 85    updated for product version.
; dennis shea       oct 98    created the NCL version
; dennis shea     9 feb 99    fix: MoistAdiabat labels at top of curves

  localOpts                   = True
  localOpts@DrawIsotherm      = True
  localOpts@DrawIsobar        = True
  localOpts@DrawMixRatio      = True
  localOpts@DrawDryAdiabat    = True
  localOpts@DrawMoistAdiabat  = True ; aka: saturation or pseudo adibat
  localOpts@DrawWind          = True
  localOpts@DrawStandardAtm   = True
  localOpts@DrawColLine       = True 
  localOpts@DrawColAreaFill   = False 
  localOpts@DrawFahrenheit    = True  ; Fahrenheit "x" axis
  localOpts@DrawHeightScale   = False
  localOpts@DrawHeightScaleFt = True  ;default is feet [otherwise km] 
  localOpts@DrawStandardAtmThk= 2.0
  localOpts@DrawWindBarbThk   = 1.0   ; default value for wmbarb: BLW
  localOpts@UseMyColorMap     = False
  localOpts@Font              = "helvetica"
                              ; NCL resource names
  localOpts@tiMainString      = "   "
  localOpts@vpXF              = 0.07
  localOpts@vpYF              = 0.925 
  localOpts@vpWidthF          = 0.85
  localOpts@vpHeightF         = 0.85
                                      ; Override localOpts
                                      ; with input Opts
  if (Opts .and. .not.any(ismissing(getvaratts(Opts)))) then
      localAtts = getvaratts(localOpts)
      OptsAtts  = getvaratts(Opts)
      nOpts     = dimsizes  (OptsAtts)
      do i=0,nOpts-1                  ; loop and add/override attributes
         localOpts@$OptsAtts(i)$ = Opts@$OptsAtts(i)$  ; new att
      end do
  end if

  if (isatt(localOpts,"PrintOpts").and.localOpts@PrintOpts) then
     print(localOpts)
  end if

  if(.not.localOpts@UseMyColorMap) then
    cmap_orig = fix_skewt_colormap_for_ncgm(wks,localOpts@UseMyColorMap)
  end if

; --- declare isotherm [C] values and pressures [hPa] where isotherms 
; --- intersect the edge of the skew-t diagram.

  temp = (/-100.,-90.,-80.,-70.,-60.,-50.,-40.,-30.             \
          , -20.,-10.,  0., 10., 20., 30., 40., 50.             /)
  lendt= (/ 132., 181., 247., 337., 459., 625., 855.            \
          ,1050.,1050.,1050.,1050.,1050.,1050.,1050.,1050.,1050./)
  rendt= (/ 100., 100., 100., 100., 100., 100., 100.            \
          , 135., 185., 251., 342., 430., 500., 580., 730., 993./)
  ntemp= dimsizes (temp)                ; isotherms
  if (dimsizes(lendt).ne.dimsizes(rendt) .or. \
      dimsizes(lendt).ne.ntemp) then
      print ("Error: dimsizes temp, lendt, rendt do not match")
      exit
  end if

; --- declare pressure values [hPa] and x coordinates of the endpoints of each
; --- isobar.  these x,y values are computed from the equations in the
; --- transform functions listed at the beginning of this program.  
; --- refer to a skew-t diagram for reference if necessary.

  pres = (/1050.,1000.,850.,700.,500.,400.,300.,250.,200.,150.,100./)
  xpl  = (/-19.0,-19.0,-19.0,-19.0,-19.0,-19.0,-19.0,-19.0 \
          ,-19.0,-19.0,-19.0/)
  xpr  = (/27.1,27.1,27.1,27.1,22.83,18.6,18.6,18.6,18.6,18.6,18.6/)
  npres= dimsizes (pres)                ; pressures
  if (dimsizes(xpl).ne.dimsizes(xpr) .or. dimsizes(xpl).ne.npres) then
      print ("Error: dimsizes pres, xpl, xpr do not match")
      exit
  end if

; --- declare adiabat values [C] and pressures where adiabats intersect the
; --- edge of the skew-t diagram.  refer to a skew-t diagram if necessary.

  theta=  (/-30.,-20.,-10.,0.,10.,20.,30.,40.,50.,60.,70.,80.,90.  \
           ,100.,110.,120.,130.,140.,150.,160.,170.                /)
  lendth= (/880.,670.,512.,388.,292.,220.,163.,119.,100.,100.,100. \
           ,100.,100.,100.,100.,100.,100.,100.,100.,100.,100.      /)
  rendth= (/1050.,1050.,1050.,1050.,1050.,1050.,1050.,1050. \
           ,1003.,852.,728.,618.,395.,334.,286., 245., 210. \
           , 180.,155.,133.,115. /)
  ntheta= dimsizes (theta)                ; dry adiabats 
  if (dimsizes(lendth).ne.dimsizes(rendth) .or. \
      dimsizes(lendth).ne.ntheta) then
      print ("Error: dimsizes theta, lendth, rendth do not match")
      exit
  end if

; --- declare moist adiabat values and pressures of the tops of the
; --- moist adiabats. all moist adiabats to be plotted begin at 1050 mb.

  pseudo = (/ 32., 28., 24., 20., 16., 12.,  8. /)
  lendps = (/250.,250.,250.,250.,250.,250.,250. /)
  npseudo= dimsizes (pseudo)              ; moist adiabats

; --- declare mixing ratio lines.  all mixing ratio lines will begin 
; --- at 1050 mb and end at 400 mb.

  mixrat= (/ 20.,12.,8.,5.,3.,2.,1. /)
  nmix  = dimsizes (mixrat)              ; mixing ratios

; --- declare local stuff: arrays/variables for storing x,y positions 
; --- during iterations to draw curved line, etc

  sx    = new (200, float)
  sy    = new (200, float)
  xx    = new (  2, float)
  yy    = new (  2, float)
  label = new (  1, string)
  m2f   = 3.2808             ; meter-to-feet
  f2m   = 1./3.2808          ; feet-to-meter

; --- define absolute x,y max/min bounds corresponding to the outer
; --- edges of the diagram.  these are computed by inserting the appropriate
; --- pressures and temperatures at the corners of the diagram.

                 ; xmin = skewtx (-33.6, skewty(1050.)) [t=deg C]
  xmin =-19.0    ; xmin = skewtx (-109.1,skewty(100.))  [t=deg C]
  xmax = 27.1    ; xmax = skewtx (51.75, skewty(1050.)) [t=deg C]
  ymax =-0.935   ; ymax = skewty (1050.)
  ymin =44.061   ; ymin = skewty ( 100.)

; --- specify arrays to hold corners of the diagram in x,y space. 

  xc    = (/ xmin, xmin, xmax, xmax, 18.60, 18.6, xmin /)
  yc    = (/ ymin, ymax, ymax,  9.0, 17.53, ymin, ymin /)

; ---- depending on how options are set create Standard Atm Info

  if (localOpts@DrawStandardAtm .or. \ 
      localOpts@DrawHeightScale .or. localOpts@DrawWind) then
                                 ; U.S. Standard ATmosphere
      zsa = fspan (0. , 16., 17) ; (km) source Hess/Riegel
      psa = (/ 1013.25, 898.71, 794.90, 700.99, 616.29, 540.07   \
             ,  471.65, 410.46, 355.82, 307.24, 264.19           \
             ,  226.31, 193.93, 165.33, 141.35, 120.86,103.30    /)
      tsa = (/   15.0 ,   8.5 ,   2.0 ,  -4.5 , -11.0 , -17.5    \
             ,  -24.0 , -30.5 , -37.0 , -43.5 , -50.0            \
             ,  -56.5 , -56.5 , -56.5 , -56.5 , -56.5 , -56.5    /)
      nlvl= dimsizes (psa)
  end if

; PLOT

  if (localOpts@DrawColLine) then
      colGreen            = "Green"
      colBrown            = "Brown"
      colTan              = "Tan"
  else
      colGreen            = "Foreground" 
      colBrown            = "Foreground"
      colTan              = "Foreground"
  end if

; --- draw outline of the skew-t, log p diagram.  proceed in the upper left
; --- corner of the diagram and draw counter-clockwise.  the array locations
; --- below that are hardcoded refer to points on the background where the
; --- skew-t diagram deviates from a rectangle, along the right edge.  remember,
; --- the set call defines a rectangle, so as long as the boundary is along
; --- the edge of the set call, the points plotted are combinations of the min
; --- and max x,y values in the set call.
 
  if (localOpts@DrawFahrenheit) then
      tf = ispan (-20, 100, 20)      ; deg F [nice round numbers]
      tc = 0.55555*(tf-32.)          ; deg C corresponding to tf
  else
      tc = ispan(-30, 40, 10)*1.0    ; deg C (nice round numbers)
  end if
 
  xyOpts            = True      
  xyOpts@gsnDraw    = False  ; Don't draw the plot or advance the
  xyOpts@gsnFrame   = False  ; frame in the call to gsn_xy.
                             ; specify location/size of skewT diagram
  xyOpts@vpXF       = localOpts@vpXF
  xyOpts@vpYF       = localOpts@vpYF
  xyOpts@vpWidthF   = localOpts@vpWidthF
  xyOpts@vpHeightF  = localOpts@vpHeightF

  xyOpts@tmYLMode   = "Explicit" ; Define y tick mark labels.
  xyOpts@tmYLValues = skewty ( pres(1:)) ; skip 1050
  xyOpts@tmYLLabels = (/"1000","850","700","500","400","300" \
                       ,"250" , "200","150","100"/)

  xyOpts@tmXBMode   = "Explicit" ; Define x tick mark labels.
  xyOpts@tmXBValues = skewtx (tc, skewty(1050.)) ; transformed vals
  if (localOpts@DrawFahrenheit) then
      xyOpts@tmXBLabels = tf         ; plot the nice deg F
  else
      xyOpts@tmXBLabels = tc         ; plot the nice deg C
  end if

  xyOpts@trXMinF      = xmin
  xyOpts@trXMaxF      = xmax 
  xyOpts@trYMinF      = ymax       ; Note: ymin > ymax
  xyOpts@trYMaxF      = ymin
  xyOpts@xyComputeXMin= False
  xyOpts@xyComputeXMax= False
  xyOpts@xyComputeYMin= False
  xyOpts@xyComputeYMax= False
  xyOpts@tmXTOn       = False
  xyOpts@tmYROn       = False
  xyOpts@tmXTBorderOn = False
  xyOpts@tmXBBorderOn = False
  xyOpts@tmYRBorderOn = False
  xyOpts@tmYLBorderOn = False
                                   ; "tune" the plot
  xyOpts@tmXBMajorLengthF        = 0.01
  xyOpts@tmXBMajorThicknessF     = 4.0         ; default is 2.0
  xyOpts@tmXBMajorOutwardLengthF = 0.01
  xyOpts@tmXTMajorLengthF     = 0. 
  xyOpts@tmYLMajorLengthF     = 0.
  xyOpts@tmXBLabelFontHeightF = 0.014
  xyOpts@tmYLLabelFontHeightF = xyOpts@tmXBLabelFontHeightF
  if (localOpts@DrawFahrenheit) then
      xyOpts@tiXAxisString    = get_res_value_keep(Opts,"tiXAxisString","Temperature (F)")
  else
      xyOpts@tiXAxisString    = get_res_value_keep(Opts,"tiXAxisString","Temperature (C)")
  end if
  xyOpts@tiXAxisFont          = localOpts@Font
  xyOpts@tiXAxisFontColor     = "Foreground"   ; colTan
  xyOpts@tiXAxisFontHeightF   = 0.0125
  xyOpts@tiYAxisFont          = localOpts@Font
  xyOpts@tiYAxisString        = get_res_value_keep(Opts,"tiYAxisString","P (hPa)")
  xyOpts@tiYAxisOffsetXF      = 0.0200
  xyOpts@tiYAxisOffsetYF      = -0.0200
  xyOpts@tiYAxisFontColor     = "Foreground"   ; colTan
  xyOpts@tiYAxisFontHeightF   = xyOpts@tiXAxisFontHeightF
  xyOpts@tiMainString         = localOpts@tiMainString
  xyOpts@tiMainFont           = localOpts@Font
  xyOpts@tiMainFontColor      = "Foreground"   

  xyOpts@tiMainFontHeightF    = 0.025
  if (isatt(localOpts,"tiMainFontHeightF")) then
      xyOpts@tiMainFontHeightF   = localOpts@tiMainFontHeightF
  end if
  xyOpts@tiMainOffsetXF       = -0.1
  if (isatt(localOpts,"tiMainOffsetXF")) then
      xyOpts@tiMainOffsetXF   = localOpts@tiMainOffsetXF
  end if
  xyOpts@tiMainOffsetYF       =  0.0
  if (isatt(localOpts,"tiMainOffsetYF")) then
      xyOpts@tiMainOffsetYF   = localOpts@tiMainOffsetYF
  end if

  if (localOpts@DrawHeightScale) then   ; special for rhs

      xyOpts@trXMaxF      = skewtx (55. , skewty(1013.)) ; extra wide
      xyOpts@tmYUseLeft   = False ; Keep right axis independent of left.
      xyOpts@tmYRBorderOn = True
      xyOpts@tmYROn       = True  ; Turn on right axis tick marks.
      xyOpts@tmYRLabelsOn = True  ; Turn on right axis labels.
      xyOpts@tmYRLabelFontHeightF    = xyOpts@tmYLLabelFontHeightF
      xyOpts@tmYRMajorThicknessF     = xyOpts@tmXBMajorThicknessF
      xyOpts@tmYRMajorLengthF        = xyOpts@tmXBMajorLengthF
      xyOpts@tmYRMajorOutwardLengthF = xyOpts@tmXBMajorOutwardLengthF
      xyOpts@tmYRMinorOn             = False       ; No minor tick marks.
      xyOpts@tmYRMode                = "Explicit"  ; Define tick mark labels.

      zkm = fspan (0. , 16., 17) 
      pkm = ftcurv(zsa,psa,zkm)
      zft = (/ 0., 2., 4., 6., 8.,10.,12.,14.,16. \ 
             ,18.,20.,25.,30.,35.,40.,45.,50.     /)
      pft = ftcurv(zsa,psa,zft*f2m)  ; p corresponding to zkm
      if (localOpts@DrawHeightScaleFt) then
          znice =  zft        
          pnice = skewty(pft)
          zLabel= "Height (1000 Feet)"
      else
          znice = zkm
          pnice = skewty(pkm)  
          zLabel= "Height (Km)"
      end if
      xyOpts@tmYRValues   = pnice ; At each "nice" pressure value, 
      xyOpts@tmYRLabels   = znice ; put a "height" value label.
  end if    ; DrawHeightScale

  xy = gsn_xy (wks,xc,yc,xyOpts)   ; draw outline; x and y axis
                      ; right *label* MUST be added AFTER xy created
  if (localOpts@DrawHeightScale) then
      txOpts                   = True
      txOpts@txAngleF          = 270.
      txOpts@txFontColor       = "Foreground"   ; colTan
      txOpts@txFontHeightF     = xyOpts@tmYLLabelFontHeightF
      xlab                     = skewtx (53., skewty(1013.)) 
      ylab                     = skewty (350.)
      gsn_text (wks,xy,zLabel,xlab,ylab,txOpts) ; label rhs
      delete (txOpts)
  end if    ; DrawHeightScale

; --- Color Fill Area of plot
   
  if (localOpts@DrawColAreaFill) then
      color1 = "PaleGreen"       ; "LightGreen"
      if (isatt(localOpts,"DrawColAreaColor")) then
        color1 = localOpts@DrawColAreaColor
      end if
      color2 = "White"
      gsOpts = True
      do i=0,ntemp-2
         if (i%2 .eq. 0) then    ; alternate colors
             gsOpts@gsFillColor  = color1
         else
             gsOpts@gsFillColor  = color2
         end if

         nx    = 3     ; this handles most cases
         sy(0) = skewty( lendt(i) )
         sx(0) = skewtx( temp(i)  , sy(0) )
         sy(1) = skewty( lendt(i+1) )
         sx(1) = skewtx( temp(i+1), sy(1) )
         sy(2) = skewty( rendt(i+1) )
         sx(2) = skewtx( temp(i+1), sy(2) )
         sy(3) = skewty( rendt(i) )
         sx(3) = skewtx( temp(i)  , sy(3) )
                                          ; special cases
         if (temp(i).eq.-40.) then
             nx = 5
             sy(0:nx) = (/   3.00, ymax, -0.935, 38.32, 44.06, 44.06 /)
             sx(0:nx) = (/ -18.88, xmin,-17.05 , 18.55, 18.55, 18.36 /)
         end if
         if (temp(i).eq.0.) then
             nx = 4
             sy(0:nx) = (/ -0.935,-0.935, 16.15, 17.53, 20.53 /)
             sx(0:nx) = (/ -0.850, 4.55 , 20.05, 18.55, 18.55 /)
         end if
         if (temp(i).eq.30.) then
             nx = 4
             sy(0:nx) = (/-0.935, -0.935,  6.02,  9.0 , 10.42 /) 
             sx(0:nx) = (/15.35 , 20.75 , 27.06, 27.06, 25.65 /)
         end if

         gsn_polygon(wks, xy, sx(0:nx),sy(0:nx),gsOpts) 
      end do
                                          ; special cases
      gsOpts@gsFillColor  = color2
      sy(0:2) = (/ 44.06, 44.06, 38.75 /) ; upper left triangle
      sx(0:2) = (/-14.04,-18.96,-18.86 /)
      gsn_polygon(wks, xy, sx(0:2),sy(0:2),gsOpts) 

      gsOpts@gsFillColor  = color2
      sy(0:2) = (/ ymax, 0.13, ymax   /) ; lower right triangle
      sx(0:2) = (/ xmax, xmax, 26.15  /) ; skewtx(51.6,ymax)
      gsn_polygon(wks, xy, sx(0:2),sy(0:2),gsOpts) 
      delete (gsOpts)
  end if

; --- draw diagonal isotherms. 
;     [brown with labels interspersed at 45 degree angle]
;     http://www.ncl.ucar.edu/Document/HLUs/Classes/GraphicStyle.shtml
   
  if (localOpts@DrawIsotherm) then
      gsOpts                   = True
      gsOpts@gsLineDashPattern = 0            ; solid
      gsOpts@gsLineColor       = colTan     
      gsOpts@gsLineThicknessF  = 3.5
     ;gsOpts@gsLineLabelFontColor    = colTan
     ;gsOpts@gsLineLabelFontHeightF  = 0.0125 

      txOpts                   = True
      txOpts@txAngleF          = 45.
      txOpts@txFontColor       = gsOpts@gsLineColor
      txOpts@txFontHeightF     = 0.0140
      txOpts@txFontThicknessF  = 1.0

      do i=0,dimsizes(temp)-3

         yy(1) = skewty(rendt(i))
         xx(1) = skewtx(temp(i),yy(1))
         yy(0) = skewty(lendt(i))
         xx(0) = skewtx(temp(i),yy(0))
        ;gsOpts@gsLineLabelString  = toint(temp(i))   ; floattointeger(temp(i))
         gsn_polyline (wks,xy,xx,yy,gsOpts) 

         xlab  = xx(1)+0.625     
         ylab  = yy(1)+0.55     
         label = toint(temp(i))     ;  floattointeger(temp(i))
         gsn_text(wks,xy,label,xlab,ylab,txOpts)

      end do 
      delete (gsOpts)
      delete (txOpts)
  end if  ; DrawIsotherm

; --- draw horizontal isobars.

  if (localOpts@DrawIsobar) then
      gsOpts                   = True 
      gsOpts@gsLineDashPattern = 0            ; solid
      gsOpts@gsLineColor       = colTan     
      gsOpts@gsLineThicknessF  = 3.5
     ;gsOpts@gsLineLabelFontColor    = colTan
     ;gsOpts@gsLineLabelFontHeightF  = 0.0125 

      do i=0,npres-1
         xx(0) = xpl(i)
         xx(1) = xpr(i)
         ypl   = skewty(pres(i))
         yy(0) = ypl
         yy(1) = ypl
         gsn_polyline (wks,xy,xx,yy,gsOpts) 
      end do       ; end "i=1,npres"
      delete (gsOpts)
  end if       ; DrawIsobar

; --- draw saturation mixing ratio lines.  these lines run between 1050 and 400
; --- mb. the 20 line intersects the sounding below 400 mb, thus a special case 
; --- is made for it.  the lines are dashed green. the temperature where each line
; --- crosses 400 mb is computed in order to get x,y locations of the top of
; --- the lines.

  if (localOpts@DrawMixRatio) then
      gsOpts                   = True ; polyline [graphic style] opts
      gsOpts@gsLineThicknessF  = 3.5
      gsOpts@gsLineDashPattern = 2             ; sat mix ratio only
      gsOpts@gsLineColor       = colGreen ;      "

      txOpts                   = True
      txOpts@txAngleF          = 65.           ;      "
      txOpts@txFontColor       = colGreen ;      "
      txOpts@txFontHeightF     = 0.0100        ;      "

      yy(1) = skewty(400.)  ; y at top [right end of slanted line]
      yy(0) = skewty(1000.) ; y at bottom of line [was 1050.]

      do i=0,nmix-1
         if (mixrat(i).eq.20.) then   
            yy(1) = skewty(440.)
           ;tmix  = THERMO::tmr(mixrat(i),440.)
            tmix  = tmr_skewt(mixrat(i),440.)
         else
            yy(1) = skewty(400.)
           ;tmix  = THERMO::tmr(mixrat(i),400.)
            tmix  = tmr_skewt(mixrat(i),400.)
         end if
         xx(1) = skewtx(tmix,yy(1))
        ;tmix  = THERMO::tmr(mixrat(i),1000.) ; was 1050
         tmix  = tmr_skewt(mixrat(i),1000.) ; was 1050
         xx(0) = skewtx(tmix,yy(0))
         gsn_polyline (wks,xy,xx,yy,gsOpts)   ; dashed green   

         xlab  = xx(0)-0.25
         ylab  = yy(0)-0.45
         label = toint(mixrat(i))             ; floattointeger(mixrat(i))
         gsn_text(wks,xy,label,xlab,ylab,txOpts)
      end do    ; end "i=0,nmix-1"
      delete (gsOpts)
      delete (txOpts)
  end if    ; DrawMixRatio

; --- draw dry adiabats.  iterate in 10 mb increments to compute the x,y
; --- points on the curve.

  if (localOpts@DrawDryAdiabat) then
      gsOpts                   = True 
      gsOpts@gsLineDashPattern = 0            
      gsOpts@gsLineColor       = colTan     
      gsOpts@gsLineThicknessF  = 3.5

      txOpts                   = True
      txOpts@txAngleF          = 300.
      txOpts@txFontColor       = colTan
      txOpts@txFontHeightF     = 0.01
      txOpts@txFontThicknessF  = 1.0

      pinc = 10.

      do i=0,ntheta-1
         p = lendth(i)-pinc
         do j=0,dimsizes(sy)-1
            p = p+pinc
            if (p.gt.rendth(i)) then
               sy(j) = skewty(rendth(i))
              ;t     = THERMO::tda(theta(i),p)   ; get temp on dry adiabat at p
               t     = tda_skewt(theta(i),p)     ; get temp on dry adiabat at p
               sx(j) = skewtx(t,sy(j))
               break 
            end if
 
            sy(j) = skewty(p)
           ;t     = THERMO::tda(theta(i),p)
            t     = tda_skewt(theta(i),p)
            sx(j) = skewtx(t,sy(j))
         end do     ; end "j=0,dimsizes(sy)-1"
        ;gsn_polyline (wks,xy,sx(:j-1),sy(:j-1),gsOpts)   ; whole line  

         if (theta(i).lt.170.) then
             gsn_polyline (wks,xy,sx(1:j-1),sy(1:j-1),gsOpts); label room  
             ylab  = skewty(lendth(i)+5.)
            ;t     = THERMO::tda(theta(i),lendth(i)+5.)
             t     = tda_skewt(theta(i),lendth(i)+5.)
             xlab  = skewtx(t,ylab)
             label = toint(theta(i))                        ; floattointeger(theta(i))
             gsn_text(wks,xy,label,xlab,ylab,txOpts)
         else                                               ; no laabel
             gsn_polyline (wks,xy,sx(:j-1),sy(:j-1),gsOpts) ; whole line
         end if

      end do        ; end "i=0,ntheta-1
      delete (gsOpts)
      delete (txOpts)
  end if    ; DryAdiabat

; --- draw moist adiabats up to 230 [was 250] mb.
; --- draw the lines.  iterate in 10 mb increments from 1060 mb.

  if (localOpts@DrawMoistAdiabat) then
      gsOpts                   = True 
      gsOpts@gsLineColor       = colGreen     
      gsOpts@gsLineThicknessF  = 3.5
      gsOpts@gsLineDashPattern = 0            

      txOpts                   = True
      txOpts@txAngleF          = 0.
      txOpts@txFontColor       = colGreen
      txOpts@txFontHeightF     = 0.0125
      txOpts@txFontThicknessF  = 1.0

      pinc = 10.

      do i=0,npseudo-1
         p = 1060.
         do j=0,dimsizes(sy)-1
            p = p-pinc
            if (p.lt.230.) then      ; was "250"
                break
            end if
            sy(j) = skewty(p)
           ;t     = THERMO::satlft(pseudo(i),p)    ; temp on moist adiabat at p
            t     = satlft_skewt(pseudo(i),p)      ; temp on moist adiabat at p
            sx(j) = skewtx(t,sy(j))
           ;print ("j="+j+"  p="+p+"  t="+t+"   sy(j)="+sy(j)+"  sx(j)="+sx(j) )  
         end do            ; end "j=0,dimsizes(sy)-1"

         gsn_polyline (wks,xy,sx(:j-1),sy(:j-1),gsOpts)  

         ylab  = skewty(p+0.5*pinc)
        ;t     = THERMO::satlft(pseudo(i),p+0.75*pinc)
         t     = satlft_skewt(pseudo(i),p+0.75*pinc)
         xlab  = skewtx(t,ylab)
        ;label = floattointeger(pseudo(i))        ; 9 Feb 99 fix 
         label = toint(pseudo(i))                 ; Jan 2013     
         gsn_text(wks,xy,label,xlab,ylab,txOpts)
  
      end do               ; end "i-0,dimsizes(pseudo)-1"
      delete (gsOpts)
      delete (txOpts)
  end if    ; DrawMoistAdiabat

  if (localOpts@DrawStandardAtm) then
      gsOpts                   = True 
      gsOpts@gsLineColor       = colTan   
      gsOpts@gsLineThicknessF  = localOpts@DrawStandardAtmThk
      gsOpts@gsLineDashPattern = 0            
    
      do i=0,nlvl-1
         sy(i) = skewty (psa(i))
         sx(i) = skewtx (tsa(i), sy(i) )
      end do
 
      gsn_polyline (wks,xy,sx(0:nlvl-1),sy(0:nlvl-1),gsOpts)  
      delete (gsOpts)
  end if    ; DrawStandardAtm

; --- draw vertical line upon which to plot wind barbs.

  if (localOpts@DrawWind) then
      gsOpts = True 
      gsOpts@gsLineColor       = "Foreground"     
      gsOpts@gsLineThicknessF  = 1.0
      gsOpts@gsLineDashPattern = 0            
      gsOpts@gsMarkerIndex     = 4   ; "hollow_circle"=> std pres
      gsOpts@gsMarkerColor     = "Foreground"

      presWind      = pres
      presWind(0)   = 1013.          ; override 1050
      xWind         = skewtx (45. , skewty(presWind(0)))
      sx(0:npres-1) = xWind            ; "x" location of wind plot
      sy(0:npres-1) = skewty(presWind)
      gsn_polyline   (wks,xy,sx(0:npres-1),sy(0:npres-1),gsOpts)  
      gsn_polymarker (wks,xy,sx(1:npres-1),sy(1:npres-1),gsOpts)
                                     ; zwind => Pibal reports
      zftWind = (/ 0., 1., 2., 3., 4., 5., 6., 7., 8., 9., 10. \
                 ,12.,14.,16.,18.,20.,25.,30.,35.,40.,45.,50. /)
      zkmWind = zftWind*f2m
      pkmWind = ftcurv(zsa,psa,zkmWind)
      nzkmW   = dimsizes (zkmWind)

      sx(0:nzkmW-1)            = xWind  ; "x" location of wind plot
      sy(0:nzkmW-1)            = skewty(pkmWind)
      gsOpts@gsMarkerIndex     = 16     ; "circle_filled" -> Pibal
      gsOpts@gsMarkerSizeF     = 0.007 ; 0.007 is default
      gsOpts@gsMarkerThicknessF= 1.0    ; 1.0 is default
      gsn_polymarker (wks,xy,sx(0:nzkmW-1),sy(0:nzkmW-1),gsOpts)
      delete (gsOpts)
  end if    ; DrawWind

;---Restore color map, if necessary
  if(.not.localOpts@UseMyColorMap) then
    gsn_define_colormap(wks,cmap_orig)
  end if

  return (xy)      ; return object

  end
; ------------------------------------------------------
undef("skewT_PlotData_Original")
function skewT_PlotData_Original \
                        (wks:graphic, skewt_bkgd:graphic \
                        ,P[*]:numeric,TC[*]:numeric      \
                        ,TDC[*]:numeric,Z[*]:numeric     \
                        ,WSPD[*]:numeric,WDIR[*]:numeric \
                        ,dataOpts:logical                )
 begin
                        ; determine index of non-msg values
                        ; used in plotting the sounding and
                        ; calculating thermodynamic quantities
  idx = ind (.not.ismissing(P)   .and. .not.ismissing(TC)  .and. \
             .not.ismissing(TDC) .and. P.ge.100. )

  p   = P(idx)          ; transfer non-msg values to local arrays
  tc  = TC(idx)         ; *generally* these local arrays are used
  tdc = TDC(idx)

  if (any(p .gt. 1100.)) then
    print ("skewT_PlotData: pressure values are out of range (must be in millibars).")
    exit
  end if
  if (any(tc .gt. 150.)) then
    print ("skewT_PlotData: temperature values are out of range for Fahrenheit or Celsius.")
    exit
  end if
  if (any(tdc .gt. 150.)) then
    print ("skewT_PlotData: dew point temperature values are out of range for Fahrenheit or Celsius.")
    exit
  end if

 ;print (" non-msg:  p="+p+"  tc="+tc+"  tdc="+tdc+ \
 ;       " other     Z="+Z(idx)+"  WSPD="+WSPD(idx)+"  WDIR="+WDIR(idx) )

  localOpts           = True     ; options describing data and ploting
  localOpts@PrintZ    = True     ; print geopotential (Z) on skewT diagram
  localOpts@PlotWindP = True     ; plot wind barbs at p lvls
  localOpts@WspdWdir  = True     ; wind speed and dir [else: u,v]
  localOpts@PlotWindH = False    ; plot wind barbs at h lvls [pibal; special]
  localOpts@HspdHdir  = True     ; wind speed and dir [else: u,v]
  localOpts@ThermoInfo= True     ; print thermodynamic info
  localOpts@Cape      = True     ; plot CAPE parcel profile if cape>0
  localOpts@Parcel    = 0        ; subscript corresponding to initial parcel
  localOpts@hemisphere="NH"      ; hemisphere wind barbs
  localOpts@Wthin     = 2        ; plot every wind barb          
                                 ; Override the default localOpts
                                 ; with input dataOpts
  if (dataOpts .and. .not.any(ismissing(getvaratts(dataOpts)))) then
      localAtts = getvaratts(localOpts)
      dataAtts  = getvaratts(dataOpts)
      nOpts     = dimsizes  (dataAtts)
      do i=0,nOpts-1            ; loop and add new attributes
         localOpts@$dataAtts(i)$ = dataOpts@$dataAtts(i)$
      end do
  end if

  getvalues skewt_bkgd
     "vpXF"                  : vpXF
     "vpYF"                  : vpYF
     "vpWidthF"              : vpWidthF
     "vpHeightF"             : vpHeightF
     "tiMainFont"            : tiMainFont
     "tiMainFontHeightF"     : tiMainFontHeightF
     "tiMainOffsetXF"        : tiMainOffsetXF 
     "tmYLLabelFontHeightF"  : tmYLLabelFontHeightF
  end getvalues
                                          ; assorted color 'indices'
  colForeGround = "Foreground"            ; defaults
  colTemperature= "Foreground"
  colDewPt      = "RoyalBlue" 
  colCape       = "Red"
  colZLabel     = colTemperature
  colWindP      = colTemperature
  colWindZ      = colTemperature
  colWindH      = colTemperature
  colThermoInfo = "Sienna"
                                          ; Change defaults
  if (isatt(localOpts,"colTemperature")) then
      colTemperature  = localOpts@colTemperature  ; new color   
  end if
  if (isatt(localOpts,"colDewPt")) then
      colDewPt  = localOpts@colDewPt              ; new color   
  end if
  if (isatt(localOpts,"colCape")) then
      colCape   = localOpts@colCape               ; new color   
  end if
  if (isatt(localOpts,"colZLabel")) then
      colZLabel = localOpts@colZLabel             ; new color   
  end if
  if (isatt(localOpts,"colWindP")) then
      colWindP  = localOpts@colWindP              ; new color   
  end if
  if (isatt(localOpts,"colWindZ")) then
      colWindZ  = localOpts@colWindZ              ; new color   
  end if
  if (isatt(localOpts,"colWindH")) then
      colWindH  = localOpts@colWindH              ; new color   
  end if
  if (isatt(localOpts,"colThermoInfo")) then
      colThermoInfo  = localOpts@colThermoInfo    ; new color   
  end if
                                          ; Change defaults
                                          ; gs settings for gsn_polyline
  gsOpts                   = True
  gsOpts@gsLineDashPattern = 0            ; solid (default)
  gsOpts@gsLineThicknessF  = 6.0          ; make thicker

  yp   = skewty (p)                    
  xtc  = skewtx (tc , yp)              ; T-P plot
  gsOpts@gsLineColor        = colTemperature   
  gsOpts@gsLineDashPattern  = 0   
  if (isatt(localOpts,"linePatternTemperature")) then
      gsOpts@gsLineDashPattern = localOpts@linePatternTemperature
  end if
  if (isatt(localOpts,"lineThicknessTemperature")) then
      gsOpts@gsLineThicknessF = localOpts@lineThicknessTemperature
  end if
  gsn_polyline  (wks,skewt_bkgd,xtc ,yp,gsOpts) 

  xtdc = skewtx (tdc, yp)              ;Dew Pt-P plot
  gsOpts@gsLineColor        = colDewPt     
  gsOpts@gsLineDashPattern  = 0
  if (isatt(localOpts,"linePatternDewPt")) then
      gsOpts@gsLineDashPattern  = localOpts@linePatternDewPt
  end if
  gsn_polyline  (wks,skewt_bkgd,xtdc,yp,gsOpts) 

  delete (gsOpts)

 ;fmsg = default_fillvalue("float")   ; get default missing for float
  fmsg = default_fillvalue(typeof(tc))   ; get default missing 

  if (localOpts@ThermoInfo) then
      if (p(1).gt.p(2)) then
          pp  = p
          ttc = tc
          ttdc= tdc
      else
          pp  = p(::-1)
          ttc = tc(::-1)
          ttdc= tdc(::-1)
      end if
      nP   = localOpts@Parcel  ; default is the lowest level [0]
      nlvls= dimsizes(pp)
      plcl = fmsg             ; p (hPa) Lifting Condensation Lvl (lcl)
      tlcl = fmsg             ; temperature (C) of lcl
     ;THERMO::ptlcl(pp(nP),ttc(nP),ttdc(nP),plcl,tlcl)
      ptlcl_skewt(pp(nP),ttc(nP),ttdc(nP),plcl,tlcl)
     ;shox = THERMO::showal(pp,ttc,ttdc,nlvls)   ; Showwalter Index
      shox = showal_skewt(pp,ttc,ttdc)           ; Showwalter Index
     ;pwat = THERMO::precpw(ttdc,pp,nlvls)       ; precipitable water (cm)
      pwat = pw_skewt(ttdc,pp)                  ; precipitable water (cm)
      iprnt= 0                                ; debug only (>0)
      nlLcl= 0                                
      nlLfc= 0
      nlCross= 0
     ;cape = THERMO::cape_ncl(pp,ttc,nlvls,plcl,iprnt,tpar,tmsg \
     ;                       ,nlLcl,nlLfc,nlCross)
      cape = cape_thermo(pp,ttc,plcl,iprnt)
      tpar = cape@tparcel
      nlLcl= cape@jlcl
      nlLfc= cape@jlfc
      nlCross= cape@jcross
                                              ; 0.5 is for rounding
      info = " Plcl="    +toint(plcl+0.5) \ 
           + " Tlcl[C]=" +toint(tlcl+0.5) \
           + " Shox="    +toint(shox+0.5) \
           + " Pwat[cm]="+toint(pwat+0.5) \
           + " Cape[J]= "+toint(cape)

      txOpts                   = True
      txOpts@txAngleF          = 0.
      txOpts@txFont            = tiMainFont
      txOpts@txFontColor       = colThermoInfo
      txOpts@txFontHeightF     = 0.5*tiMainFontHeightF
      xinfo                    = vpXF  + 0.5*vpWidthF + tiMainOffsetXF
      yinfo                    = vpYF  + 0.5*tiMainFontHeightF 
      if (isatt(localOpts,"offsetThermoInfo")) then
          yinfo = yinfo + localOpts@offsetThermoInfo
      end if

      gsn_text_ndc (wks,info,xinfo,yinfo,txOpts) 
      delete (txOpts)

      if (localOpts@Cape .and. cape.gt.0.) then
          gsOpts                   = True
          gsOpts@gsLineColor       = colCape   
          gsOpts@gsLineDashPattern = 1         ; 14          
          if (isatt(localOpts,"gsLineDashPatternCape")) then
              gsOpts@gsLineDashPattern  = localOpts@linePatternCape
           end if
          gsOpts@gsLineThicknessF  = 2.5

          yp   = skewty (pp)                    
          xtp  = skewtx (tpar, yp)
          gsn_polyline  (wks,skewt_bkgd,xtp(nlLfc:nlCross)       \
                                       , yp(nlLfc:nlCross),gsOpts) 
          delete (gsOpts)
      end if

  end if    ; ThermoInfo

  if (localOpts@PrintZ) then           ; print geopotential (?)
      txOpts               = True
      txOpts@txAngleF      = 0.
      txOpts@txFontColor   = colZLabel
      txOpts@txFontHeightF = 0.9*tmYLLabelFontHeightF
                                       ; levels at which Z is printed
      Pprint = (/1000., 850., 700., 500., 400., 300. \
               ,  250., 200., 150., 100. /)

      yz = skewty (1000.) 
      xz = skewtx (-30., yz)           ; constant "x"
      do nl=0,dimsizes(P)-1            ; use input arrays
         if (.not.ismissing(P(nl)) .and. .not.ismissing(Z(nl)) .and. \
             any(Pprint.eq.P(nl))) then
             yz  = skewty (P(nl)) 
             gsn_text (wks,skewt_bkgd,toint(Z(nl)) \
                          ,xz,yz,txOpts) 
         end if
      end do          ; nl
      delete (txOpts)
  end if

  wmsetp("BLW",1.0)    ;  Default is 1.0 
  if (isatt(localOpts,"DrawWindBarbThk")) then
      wmsetp("BLW", localOpts@DrawWindBarbThk)    ; override the default
  end if

  if (localOpts@PlotWindP) then
          gsOpts                   = True
          gsOpts@gsLineThicknessF  = 2.5
      if (.not.all(ismissing(WSPD))) then
                   ; IDW - indices where P/WSPD/WDIR are not all missing
          IDW = ind (.not.ismissing(P)    .and. P.ge.100. .and.     \
                     .not.ismissing(WSPD) .and. .not.ismissing(WDIR) )

          if (isatt(localOpts,"Wthin") .and. localOpts@Wthin.gt.1) then
              nThin = localOpts@Wthin
              idw   = IDW(::nThin)
          else
              idw   = IDW
          end if

          pw  = P(idw)   

          wmsetp ("wdf", 1)                 ; meteorological dir (Sep 2001)
          if (localOpts@hemisphere.eq."SH") then      ; hemisphere
               wmsetp("wba",-62.0)                    ; NH (default) is 62.0
          end if

          if (localOpts@WspdWdir) then      ; wind spd,dir (?)
              dirw = 0.017453*WDIR(idw)
              up   = -WSPD(idw)*sin(dirw)
              vp   = -WSPD(idw)*cos(dirw)
          else
              up   = WSPD(idw)              ; must be u,v components
              vp   = WDIR(idw)
          end if

          wbcol = wmgetp("col")             ; get current wbarb color
          wmsetp("col",getSkewTColorIndex(wks,colWindP)) ; set new color 
                                                ; see skewT_BackGround
          ypWind = skewty (pw) 
          xpWind = new (dimsizes(pw), float)
          if (isatt(localOpts,"xpWind")) then
              xpWind = skewtx (localOpts@xpWind , skewty(1013.) ) ; location of wind barb
          else
              xpWind = skewtx (45. , skewty(1013.) ) ; location of wind barb
          end if
                                            ; wmbarb is prototyped as float
          if (typeof(up).ne."float") then
              wmbarb(wks, xpWind, ypWind, tofloat(up), tofloat(vp) )
          else
              wmbarb(wks, xpWind, ypWind, up, vp )
          end if

          wmsetp("col",wbcol)               ; reset to initial color value 
                                            ; chk for soundings where Z/wind
                                            ; were merged but no pressure
          idz = ind ( ismissing(P) .and.  .not. ismissing(Z) .and.   \
                     .not.ismissing(WSPD) .and. .not.ismissing(WDIR) )
          if (.not.all(ismissing(idz))) then
              zw  = Z(idz)   
              if (localOpts@WspdWdir) then      ; wind spd,dir (?)
                  dirz = 0.017453*WDIR(idz)
                  uz   = -WSPD(idz)*sin(dirz)
                  vz   = -WSPD(idz)*cos(dirz)
              else
                  uz   = WSPD(idz)              ; must be u,v components
                  vz   = WDIR(idz)
              end if
                                                ; idzp=indices non-msg Z and P
              idzp = ind (.not.ismissing(P) .and. .not.ismissing(Z))
              pz   = ftcurv (Z(idzp),P(idzp),zw)  ; map zw to p levels

              wbcol = wmgetp("col")             ; get current wbarb color
              wmsetp("col",getSkewTColorIndex(wks,colWindZ)) ; set new color 
                                                ; see skewT_BackGround
              yzWind = skewty (pz) 
              xzWind = new (dimsizes(pz), float)
              xzWind = skewtx (45. , skewty(1013.) ) ; location of wind barb
                                               ; wmbarb is prototyped as float
              if (typeof(uz).ne."float") then
                  wmbarb(wks, xzWind, yzWind, tofloat(uz), tofloat(vz) )
              else
                  wmbarb(wks, xzWind, yzWind, uz, vz )
              end if
              wmsetp("col",wbcol)               ; reset to initial color value 

          end if   ; end ".not.ismissing(idz)"
      end if       ; end ".not.all(ismissing(WSPD))"
  end if           ; end "PlotWindP"
                                                ; SPECIAL SECTION [IGNORE]
                                                ; allows 'other' winds to be
                                                ; input as attributes of sounding
  if (localOpts@PlotWindH) then
      if (isatt(dataOpts,"Height") .and. isatt(dataOpts,"Hspd") \
                                   .and. isatt(dataOpts,"Hdir") ) then
          dimHeight = dimsizes(dataOpts@Height)
          dimHspd   = dimsizes(dataOpts@Hspd  )
          dimHdir   = dimsizes(dataOpts@Hdir  )
          if (dimHeight.eq.dimHspd .and. dimHeight.eq.dimHdir .and. \
              .not.all(ismissing(dataOpts@Height))) then
              if (localOpts@HspdHdir) then      ; wind spd,dir (?)
                  dirh= 0.017453*dataOpts@Hdir
                  uh  = -dataOpts@Hspd*sin(dirh)
                  vh  = -dataOpts@Hspd*cos(dirh)
              else
                  uh  = dataOpts@Hspd           ; must be u,v components
                  vh  = dataOpts@Hdir
              end if  ; end "localOpts@HspdHdir"

              idzp = ind (.not.ismissing(P) .and. .not.ismissing(Z))
              ph   = ftcurv (Z(idzp),P(idzp),dataOpts@Height) ; Height to p levels

              wbcol = wmgetp("col")             ; get current color index
              wmsetp("col",getSkewTColorIndex(wks,colWindH)) ; set new color 

              yhWind = skewty (ph) 
              xhWind = new (dimsizes(ph), float)
              xhWind = skewtx (45. , skewty(1013.) ) ; location of wind barb

                                            ; wmbarb is prototyped as float
              if (typeof(uh).ne."float") then
                  wmbarb(wks, xhWind, yhWind, tofloat(uh), tofloat(vh) )
              else
                  wmbarb(wks, xhWind, yhWind, uh, vh )
              end if

              wmsetp("col",wbcol)               ; reset to initial color value 
          end if
      else
          print ("Opts@PlotWindH=True but dataOpts@Height/Hspd/Hdir msg")
      end if
  end if  ; end "PlotWindH"

  if (localOpts@ThermoInfo .and. isvar("cape") ) then
      skewt_bkgd@Cape  = (/ cape /)
      skewt_bkgd@Pwat  = pwat            ; cm
      skewt_bkgd@Shox  = shox
      skewt_bkgd@Plcl  = plcl  
      skewt_bkgd@Tlcl  = tlcl  
  end if

  return (skewt_bkgd)

 end
;
;======================Panel==================================
;
undef("skewT_BackGround_Panel")
function skewT_BackGround_Panel (wks:graphic,  Opts:logical)
local copt
begin

; statement of purpose.
; ---------------------
; this program generates a skew-t, log p thermodynamic diagram.  this
; program was derived to reproduce the USAF skew-t, log p diagram.
;        (form dod-wpc 9-16-1  current as of march 1978)
; source.
; --------                    
; Tom Schlatter and Joe Wakefield [NOAA/PROFS] supplied fortran codes.
;
; history.
; --------                    
; don baker      01 jul 85    original version.  [NOAA/PROFS]
; don baker      01 dec 85    updated for product version.
; dennis shea       oct 98    created the NCL version
; dennis shea     9 feb 99    fix: MoistAdiabat labels at top of curves

  localOpts                   = True
  localOpts@DrawIsotherm      = True
  localOpts@DrawIsobar        = True
  localOpts@DrawMixRatio      = True
  localOpts@DrawDryAdiabat    = True
  localOpts@DrawMoistAdiabat  = True  ; aka: saturation or pseudo adiabat
  localOpts@DrawWind          = False ; wmbarb can *not* be paneled
  localOpts@DrawStandardAtm   = True
  localOpts@DrawColLine       = True 
  localOpts@DrawColAreaFill   = False 
  localOpts@DrawFahrenheit    = True  ; Fahrenheit "x" axis
  localOpts@DrawHeightScale   = False
  localOpts@DrawHeightScaleFt = True  ;default is feet [otherwise km] 
  localOpts@DrawStandardAtmThk= 2.0
  localOpts@UseMyColorMap     = False
  localOpts@Font              = "helvetica"
                              ; NCL resource names
  localOpts@tiMainString      = "   "
  localOpts@vpXF              = 0.07
  localOpts@vpYF              = 0.925 
  localOpts@vpWidthF          = 0.85
  localOpts@vpHeightF         = 0.85
                                      ; Override localOpts
                                      ; with input Opts
  if (Opts .and. .not.any(ismissing(getvaratts(Opts)))) then
      localAtts = getvaratts(localOpts)
      OptsAtts  = getvaratts(Opts)
      nOpts     = dimsizes  (OptsAtts)
      do i=0,nOpts-1                  ; loop and add/override attributes
         localOpts@$OptsAtts(i)$ = Opts@$OptsAtts(i)$  ; new att
      end do
  end if

  if(.not.localOpts@UseMyColorMap) then
    cmap_orig = fix_skewt_colormap_for_ncgm(wks,localOpts@UseMyColorMap)
  end if
                                      ; FORCE this option
  localOpts@DrawWind          = False ; wmbarb can *not* be paneled

  if (isatt(localOpts,"PrintOpts").and.localOpts@PrintOpts) then
     print(localOpts)
  end if

; --- declare isotherm [C] values and pressures [hPa] where isotherms 
; --- intersect the edge of the skew-t diagram.

  temp = (/-100.,-90.,-80.,-70.,-60.,-50.,-40.,-30.             \
          , -20.,-10.,  0., 10., 20., 30., 40., 50.             /)
  lendt= (/ 132., 181., 247., 337., 459., 625., 855.            \
          ,1050.,1050.,1050.,1050.,1050.,1050.,1050.,1050.,1050./)
  rendt= (/ 100., 100., 100., 100., 100., 100., 100.            \
          , 135., 185., 251., 342., 430., 500., 580., 730., 993./)
  ntemp= dimsizes (temp)                ; isotherms
  if (dimsizes(lendt).ne.dimsizes(rendt) .or. \
      dimsizes(lendt).ne.ntemp) then
      print ("Error: dimsizes temp, lendt, rendt do not match")
      exit
  end if

; --- declare pressure values [hPa] and x coordinates of the endpoints of each
; --- isobar.  these x,y values are computed from the equations in the
; --- transform functions listed at the beginning of this program.  
; --- refer to a skew-t diagram for reference if necessary.

  pres = (/1050.,1000.,850.,700.,500.,400.,300.,250.,200.,150.,100./)
  xpl  = (/-19.0,-19.0,-19.0,-19.0,-19.0,-19.0,-19.0,-19.0 \
          ,-19.0,-19.0,-19.0/)
  xpr  = (/27.1,27.1,27.1,27.1,22.83,18.6,18.6,18.6,18.6,18.6,18.6/)
  npres= dimsizes (pres)                ; pressures
  if (dimsizes(xpl).ne.dimsizes(xpr) .or. dimsizes(xpl).ne.npres) then
      print ("Error: dimsizes pres, xpl, xpr do not match")
      exit
  end if

; --- declare adiabat values [C] and pressures where adiabats intersect the
; --- edge of the skew-t diagram.  refer to a skew-t diagram if necessary.

  theta=  (/-30.,-20.,-10.,0.,10.,20.,30.,40.,50.,60.,70.,80.,90.  \
           ,100.,110.,120.,130.,140.,150.,160.,170.                /)
  lendth= (/880.,670.,512.,388.,292.,220.,163.,119.,100.,100.,100. \
           ,100.,100.,100.,100.,100.,100.,100.,100.,100.,100.      /)
  rendth= (/1050.,1050.,1050.,1050.,1050.,1050.,1050.,1050. \
           ,1003.,852.,728.,618.,395.,334.,286., 245., 210. \
           , 180.,155.,133.,115. /)
  ntheta= dimsizes (theta)                ; dry adiabats 
  if (dimsizes(lendth).ne.dimsizes(rendth) .or. \
      dimsizes(lendth).ne.ntheta) then
      print ("Error: dimsizes theta, lendth, rendth do not match")
      exit
  end if

; --- declare moist adiabat values and pressures of the tops of the
; --- moist adiabats. all moist adiabats to be plotted begin at 1050 mb.

  pseudo = (/ 32., 28., 24., 20., 16., 12.,  8. /)
  lendps = (/250.,250.,250.,250.,250.,250.,250. /)
  npseudo= dimsizes (pseudo)              ; moist adiabats

; --- declare mixing ratio lines.  all mixing ratio lines will begin 
; --- at 1050 mb and end at 400 mb.

  mixrat= (/ 20.,12.,8.,5.,3.,2.,1. /)
  nmix  = dimsizes (mixrat)              ; mixing ratios

; --- declare local stuff: arrays/variables for storing x,y positions 
; --- during iterations to draw curved line, etc

  sx    = new (200, float)
  sy    = new (200, float)
  xx    = new (  2, float)
  yy    = new (  2, float)
  label = new (  1, string)
  m2f   = 3.2808             ; meter-to-feet
  f2m   = 1./3.2808          ; feet-to-meter

; --- define absolute x,y max/min bounds corresponding to the outer
; --- edges of the diagram.  these are computed by inserting the appropriate
; --- pressures and temperatures at the corners of the diagram.

                 ; xmin = skewtx (-33.6, skewty(1050.)) [t=deg C]
  xmin =-19.0    ; xmin = skewtx (-109.1,skewty(100.))  [t=deg C]
  xmax = 27.1    ; xmax = skewtx (51.75, skewty(1050.)) [t=deg C]
  ymax =-0.935   ; ymax = skewty (1050.)
  ymin =44.061   ; ymin = skewty ( 100.)

; --- specify arrays to hold corners of the diagram in x,y space. 

  xc    = (/ xmin, xmin, xmax, xmax, 18.60, 18.6, xmin /)
  yc    = (/ ymin, ymax, ymax,  9.0, 17.53, ymin, ymin /)

; ---- depending on how options are set create Standard Atm Info

  if (localOpts@DrawStandardAtm .or. \ 
      localOpts@DrawHeightScale .or. localOpts@DrawWind) then
                                 ; U.S. Standard ATmosphere
      zsa = fspan (0. , 16., 17) ; (km) source Hess/Riegel
      psa = (/ 1013.25, 898.71, 794.90, 700.99, 616.29, 540.07   \
             ,  471.65, 410.46, 355.82, 307.24, 264.19           \
             ,  226.31, 193.93, 165.33, 141.35, 120.86,103.30    /)
      tsa = (/   15.0 ,   8.5 ,   2.0 ,  -4.5 , -11.0 , -17.5    \
             ,  -24.0 , -30.5 , -37.0 , -43.5 , -50.0            \
             ,  -56.5 , -56.5 , -56.5 , -56.5 , -56.5 , -56.5    /)
      nlvl= dimsizes (psa)
  end if

; PLOT

  if (localOpts@DrawColLine) then
      colGreen            = "Green"
      colBrown            = "Brown"
      colTan              = "Tan"
  else
      colGreen            = "Foreground" 
      colBrown            = "Foreground"
      colTan              = "Foreground"
  end if

; --- draw outline of the skew-t, log p diagram.  proceed in the upper left
; --- corner of the diagram and draw counter-clockwise.  the array locations
; --- below that are hardcoded refer to points on the background where the
; --- skew-t diagram deviates from a rectangle, along the right edge.  remember,
; --- the set call defines a rectangle, so as long as the boundary is along
; --- the edge of the set call, the points plotted are combinations of the min
; --- and max x,y values in the set call.
 
  if (localOpts@DrawFahrenheit) then
      tf = ispan (-20, 100, 20)      ; deg F [nice round numbers]
      tc = 0.55555*(tf-32.)          ; deg C corresponding to tf
  else
      tc = ispan(-30, 40, 10)*1.0    ; deg C (nice round numbers)
  end if
 
  xyOpts            = True      
  xyOpts@gsnDraw    = False  ; Don't draw the plot or advance the
  xyOpts@gsnFrame   = False  ; frame in the call to gsn_xy.
                             ; specify location/size of skewT diagram
  xyOpts@vpXF       = localOpts@vpXF
  xyOpts@vpYF       = localOpts@vpYF
  xyOpts@vpWidthF   = localOpts@vpWidthF
  xyOpts@vpHeightF  = localOpts@vpHeightF

  xyOpts@tmYLMode   = "Explicit" ; Define y tick mark labels.
  xyOpts@tmYLValues = skewty ( pres(1:)) ; skip 1050
  xyOpts@tmYLLabels = (/"1000","850","700","500","400","300" \
                       ,"250" , "200","150","100"/)

  xyOpts@tmXBMode   = "Explicit" ; Define x tick mark labels.
  xyOpts@tmXBValues = skewtx (tc, skewty(1050.)) ; transformed vals
  if (localOpts@DrawFahrenheit) then
      xyOpts@tmXBLabels = tf         ; plot the nice deg F
  else
      xyOpts@tmXBLabels = tc         ; plot the nice deg C
  end if

  xyOpts@trXMinF      = xmin
  xyOpts@trXMaxF      = xmax 
  xyOpts@trYMinF      = ymax       ; Note: ymin > ymax
  xyOpts@trYMaxF      = ymin
  xyOpts@xyComputeXMin= False
  xyOpts@xyComputeXMax= False
  xyOpts@xyComputeYMin= False
  xyOpts@xyComputeYMax= False
  xyOpts@tmXTOn       = False
  xyOpts@tmYROn       = False
  xyOpts@tmXTBorderOn = False
  xyOpts@tmXBBorderOn = False
  xyOpts@tmYRBorderOn = False
  xyOpts@tmYLBorderOn = False
                                   ; "tune" the plot
  xyOpts@tmXBMajorLengthF        = 0.01
  xyOpts@tmXBMajorThicknessF     = 4.0         ; default is 2.0
  xyOpts@tmXBMajorOutwardLengthF = 0.01
  xyOpts@tmXTMajorLengthF     = 0. 
  xyOpts@tmYLMajorLengthF     = 0.
  xyOpts@tmXBLabelFontHeightF = 0.014
  xyOpts@tmYLLabelFontHeightF = xyOpts@tmXBLabelFontHeightF
  if (localOpts@DrawFahrenheit) then
      xyOpts@tiXAxisString    = get_res_value_keep(Opts,"tiXAxisString","Temperature (F)")
  else
      xyOpts@tiXAxisString    = get_res_value_keep(Opts,"tiXAxisString","Temperature (C)")
  end if
  xyOpts@tiXAxisFont          = localOpts@Font
  xyOpts@tiXAxisFontColor     = "Foreground"   ; colTan
  xyOpts@tiXAxisFontHeightF   = 0.0125
  xyOpts@tiYAxisFont          = localOpts@Font
  xyOpts@tiYAxisString        = get_res_value_keep(Opts,"tiYAxisString","P (hPa)")
  xyOpts@tiYAxisOffsetXF      = 0.0200
  xyOpts@tiYAxisOffsetYF      = -0.0200
  xyOpts@tiYAxisFontColor     = "Foreground"   ; colTan
  xyOpts@tiYAxisFontHeightF   = xyOpts@tiXAxisFontHeightF
  xyOpts@tiMainString         = localOpts@tiMainString
  xyOpts@tiMainFont           = localOpts@Font
  xyOpts@tiMainFontColor      = "Foreground"   

  xyOpts@tiMainFontHeightF    = 0.025
  if (isatt(localOpts,"tiMainFontHeightF")) then
      xyOpts@tiMainFontHeightF   = localOpts@tiMainFontHeightF
  end if
  xyOpts@tiMainOffsetXF       = -0.1
  if (isatt(localOpts,"tiMainOffsetXF")) then
      xyOpts@tiMainOffsetXF   = localOpts@tiMainOffsetXF
  end if
  xyOpts@tiMainOffsetYF       =  0.0
  if (isatt(localOpts,"tiMainOffsetYF")) then
      xyOpts@tiMainOffsetYF   = localOpts@tiMainOffsetYF
  end if

  if (localOpts@DrawHeightScale) then   ; special for rhs

      xyOpts@trXMaxF      = skewtx (55. , skewty(1013.)) ; extra wide
      xyOpts@tmYUseLeft   = False ; Keep right axis independent of left.
      xyOpts@tmYRBorderOn = True
      xyOpts@tmYROn       = True  ; Turn on right axis tick marks.
      xyOpts@tmYRLabelsOn = True  ; Turn on right axis labels.
      xyOpts@tmYRLabelFontHeightF    = xyOpts@tmYLLabelFontHeightF
      xyOpts@tmYRMajorThicknessF     = xyOpts@tmXBMajorThicknessF
      xyOpts@tmYRMajorLengthF        = xyOpts@tmXBMajorLengthF
      xyOpts@tmYRMajorOutwardLengthF = xyOpts@tmXBMajorOutwardLengthF
      xyOpts@tmYRMinorOn             = False       ; No minor tick marks.
      xyOpts@tmYRMode                = "Explicit"  ; Define tick mark labels.

      zkm = fspan (0. , 16., 17) 
      pkm = ftcurv(zsa,psa,zkm)
      zft = (/ 0., 2., 4., 6., 8.,10.,12.,14.,16. \ 
             ,18.,20.,25.,30.,35.,40.,45.,50.     /)
      pft = ftcurv(zsa,psa,zft*f2m)  ; p corresponding to zkm
      if (localOpts@DrawHeightScaleFt) then
          znice =  zft        
          pnice = skewty(pft)
          zLabel= "Height (1000 Feet)"
      else
          znice = zkm
          pnice = skewty(pkm)  
          zLabel= "Height (Km)"
      end if
      xyOpts@tmYRValues   = pnice ; At each "nice" pressure value, 
      xyOpts@tmYRLabels   = znice ; put a "height" value label.
  end if    ; DrawHeightScale

  xy = gsn_xy (wks,xc,yc,xyOpts)   ; draw outline; x and y axis
                      ; right *label* MUST be added AFTER xy created
  if (localOpts@DrawHeightScale) then
      txOpts                   = True
      txOpts@txAngleF          = 270.
      txOpts@txFontColor       = "Foreground"   ; colTan
      txOpts@txFontHeightF     = xyOpts@tmYLLabelFontHeightF
      xlab                     = skewtx (53., skewty(1013.)) 
      ylab                     = skewty (350.)
      fooTextHeightScale = gsn_add_text (wks,xy,zLabel,xlab,ylab,txOpts) ; label rhs

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooTextHeightScale

      delete (txOpts)
  end if    ; DrawHeightScale

; --- Color Fill Area of plot
   
  if (localOpts@DrawColAreaFill) then
      color1 = "PaleGreen"       ; "LightGreen"
      if (isatt(localOpts,"DrawColAreaColor")) then
        color1 = localOpts@DrawColAreaColor
      end if
      color2 = "White"
      gsOpts = True

      fooPolyColorAreaFill = new ( (ntemp-1),"graphic")

      do i=0,ntemp-2
         if (i%2 .eq. 0) then    ; alternate colors
             gsOpts@gsFillColor  = color1
         else
             gsOpts@gsFillColor  = color2
         end if

         nx    = 3     ; this handles most cases
         sy(0) = skewty( lendt(i) )
         sx(0) = skewtx( temp(i)  , sy(0) )
         sy(1) = skewty( lendt(i+1) )
         sx(1) = skewtx( temp(i+1), sy(1) )
         sy(2) = skewty( rendt(i+1) )
         sx(2) = skewtx( temp(i+1), sy(2) )
         sy(3) = skewty( rendt(i) )
         sx(3) = skewtx( temp(i)  , sy(3) )
                                          ; special cases
         if (temp(i).eq.-40.) then
             nx = 5
             sy(0:nx) = (/   3.00, ymax, -0.935, 38.32, 44.06, 44.06 /)
             sx(0:nx) = (/ -18.88, xmin,-17.05 , 18.55, 18.55, 18.36 /)
         end if
         if (temp(i).eq.0.) then
             nx = 4
             sy(0:nx) = (/ -0.935,-0.935, 16.15, 17.53, 20.53 /)
             sx(0:nx) = (/ -0.850, 4.55 , 20.05, 18.55, 18.55 /)
         end if
         if (temp(i).eq.30.) then
             nx = 4
             sy(0:nx) = (/-0.935, -0.935,  6.02,  9.0 , 10.42 /) 
             sx(0:nx) = (/15.35 , 20.75 , 27.06, 27.06, 25.65 /)
         end if

         fooPolyColorAreaFill(i) = gsn_add_polygon(wks, xy, sx(0:nx),sy(0:nx),gsOpts) 
      end do

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooPolyColorAreaFill
                                          ; special cases
      gsOpts@gsFillColor  = color2
      sy(0:2) = (/ 44.06, 44.06, 38.75 /) ; upper left triangle
      sx(0:2) = (/-14.04,-18.96,-18.86 /)
      fooPolyColorULT = gsn_add_polygon(wks, xy, sx(0:2),sy(0:2),gsOpts) 

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooPolyColorULT

      gsOpts@gsFillColor  = color2
      sy(0:2) = (/ ymax, 0.13, ymax   /) ; lower right triangle
      sx(0:2) = (/ xmax, xmax, 26.15  /) ; skewtx(51.6,ymax)
      fooPolyColorLRT = gsn_add_polygon(wks, xy, sx(0:2),sy(0:2),gsOpts) 

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooPolyColorLRT

      delete (gsOpts)
  end if

; --- draw diagonal isotherms. 
;     [brown with labels interspersed at 45 degree angle]
;     http://www.ncl.ucar.edu/Document/HLUs/Classes/GraphicStyle.shtml
   
  if (localOpts@DrawIsotherm) then
      gsOpts                   = True
      gsOpts@gsLineDashPattern = 0            ; solid
      gsOpts@gsLineColor       = colTan     
      gsOpts@gsLineThicknessF  = 2.5
     ;gsOpts@gsLineLabelFontColor    = colTan
     ;gsOpts@gsLineLabelFontHeightF  = 0.0125 

      txOpts                   = True
      txOpts@txAngleF          = 45.
      txOpts@txFontColor       = gsOpts@gsLineColor
      txOpts@txFontHeightF     = 0.0140
      txOpts@txFontThicknessF  = 1.0

      fooLineIsoTherm = new (dimsizes(temp)-2,"graphic")
      fooTextIsoTherm = new (dimsizes(temp)-2,"graphic")

      do i=0,dimsizes(temp)-3

         yy(1) = skewty(rendt(i))
         xx(1) = skewtx(temp(i),yy(1))
         yy(0) = skewty(lendt(i))
         xx(0) = skewtx(temp(i),yy(0))
        ;gsOpts@gsLineLabelString  = toint(temp(i))
         fooLineIsoTherm(i) = gsn_add_polyline (wks,xy,xx,yy,gsOpts) 

         xlab  = xx(1)+0.625     
         ylab  = yy(1)+0.55     
         label = toint(temp(i))
         fooTextIsoTherm(i) = gsn_add_text(wks,xy,label,xlab,ylab,txOpts)
      end do 

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooLineIsoTherm
      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooTextIsoTherm

      delete (gsOpts)
      delete (txOpts)
  end if  ; DrawIsotherm

; --- draw horizontal isobars.

  if (localOpts@DrawIsobar) then
      gsOpts                   = True 
      gsOpts@gsLineDashPattern = 0            ; solid
      gsOpts@gsLineColor       = colTan     
      gsOpts@gsLineThicknessF  = 2.5
     ;gsOpts@gsLineLabelFontColor    = colTan
     ;gsOpts@gsLineLabelFontHeightF  = 0.0125 

      fooLineIsobar = new (npres,"graphic")

      do i=0,npres-1
         xx(0) = xpl(i)
         xx(1) = xpr(i)
         ypl   = skewty(pres(i))
         yy(0) = ypl
         yy(1) = ypl
         fooLineIsobar(i) = gsn_add_polyline (wks,xy,xx,yy,gsOpts) 
      end do       ; end "i=1,npres"
      delete (gsOpts)
  end if       ; DrawIsobar

  dumname = unique_string("dum")     ; for panel
  xy@$dumname$ = fooLineIsobar

; --- draw saturation mixing ratio lines.  these lines run between 1050 and 400
; --- mb. the 20 line intersects the sounding below 400 mb, thus a special case 
; --- is made for it.  the lines are dashed green. the temperature where each line
; --- crosses 400 mb is computed in order to get x,y locations of the top of
; --- the lines.

  if (localOpts@DrawMixRatio) then
      gsOpts                   = True ; polyline [graphic style] opts
      gsOpts@gsLineThicknessF  = 2.5
      gsOpts@gsLineDashPattern = 2             ; sat mix ratio only
      gsOpts@gsLineColor       = colGreen ;      "

      txOpts                   = True
      txOpts@txAngleF          = 65.           ;      "
      txOpts@txFontColor       = colGreen ;      "
      txOpts@txFontHeightF     = 0.0100        ;      "

      yy(1) = skewty(400.)  ; y at top [right end of slanted line]
      yy(0) = skewty(1000.) ; y at bottom of line [was 1050.]

      fooLineMix = new (nmix,"graphic")
      fooTextMix = new (nmix,"graphic")

      do i=0,nmix-1
         if (mixrat(i).eq.20.) then   
            yy(1) = skewty(440.)
           ;tmix  = THERMO::tmr(mixrat(i),440.)
            tmix  = tmr_skewt(mixrat(i),440.)
         else
            yy(1) = skewty(400.)
           ;tmix  = THERMO::tmr(mixrat(i),400.)
            tmix  = tmr_skewt(mixrat(i),400.)
         end if
         xx(1) = skewtx(tmix,yy(1))
        ;tmix  = THERMO::tmr(mixrat(i),1000.) ; was 1050
         tmix  = tmr_skewt(mixrat(i),1000.) ; was 1050
         xx(0) = skewtx(tmix,yy(0))
         fooLineMix(i) = gsn_add_polyline (wks,xy,xx,yy,gsOpts)   ; dashed green   

         xlab  = xx(0)-0.25
         ylab  = yy(0)-0.45
         label = toint(mixrat(i))
         fooTextMix(i) = gsn_add_text(wks,xy,label,xlab,ylab,txOpts)
      end do    ; end "i=0,nmix-1"

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooLineMix
      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooTextMix

      delete (gsOpts)
      delete (txOpts)
  end if    ; DrawMixRatio

; --- draw dry adiabats.  iterate in 10 mb increments to compute the x,y
; --- points on the curve.

  if (localOpts@DrawDryAdiabat) then
      gsOpts                   = True 
      gsOpts@gsLineDashPattern = 0            
      gsOpts@gsLineColor       = colTan     
      gsOpts@gsLineThicknessF  = 2.5

      txOpts                   = True
      txOpts@txAngleF          = 300.
      txOpts@txFontColor       = colTan
      txOpts@txFontHeightF     = 0.01
      txOpts@txFontThicknessF  = 1.0

      pinc = 10.
      fooLineDry = new (ntheta,"graphic")
      fooTextDry = new (ntheta,"graphic")

      do i=0,ntheta-1
         p = lendth(i)-pinc
         do j=0,dimsizes(sy)-1
            p = p+pinc
            if (p.gt.rendth(i)) then
               sy(j) = skewty(rendth(i))
              ;t     = THERMO::tda(theta(i),p)   ; get temp on dry adiabat at p
               t     = tda_skewt(theta(i),p)     ; get temp on dry adiabat at p
               sx(j) = skewtx(t,sy(j))
               break 
            end if
 
            sy(j) = skewty(p)
           ;t     = THERMO::tda(theta(i),p)
            t     = tda_skewt(theta(i),p)
            sx(j) = skewtx(t,sy(j))
         end do     ; end "j=0,dimsizes(sy)-1"

        ;fooLineDry(i) = gsn_add_polyline (wks,xy,sx(:j-1),sy(:j-1),gsOpts)   ; whole line  

         if (theta(i).lt.170.) then
             fooLineDry(i) = gsn_add_polyline (wks,xy,sx(1:j-1),sy(1:j-1),gsOpts); label room  
             ylab  = skewty(lendth(i)+5.)
            ;t     = THERMO::tda(theta(i),lendth(i)+5.)
             t     = tda_skewt(theta(i),lendth(i)+5.)
             xlab  = skewtx(t,ylab)
             label = toint(theta(i))
             fooTextDry(i) = gsn_add_text(wks,xy,label,xlab,ylab,txOpts)
         else                                               ; no laabel
             fooLineDry(i) = gsn_add_polyline (wks,xy,sx(:j-1),sy(:j-1),gsOpts) ; whole line
         end if

      end do        ; end "i=0,ntheta-1

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooLineDry
      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooTextDry

      delete (gsOpts)
      delete (txOpts)
  end if    ; DryAdiabat

; --- draw moist adiabats up to 230 [was 250] mb.
; --- draw the lines.  iterate in 10 mb increments from 1060 mb.

  if (localOpts@DrawMoistAdiabat) then
      gsOpts                   = True 
      gsOpts@gsLineColor       = colGreen     
      gsOpts@gsLineThicknessF  = 2.5
      gsOpts@gsLineDashPattern = 0            

      txOpts                   = True
      txOpts@txAngleF          = 0.
      txOpts@txFontColor       = colGreen
      txOpts@txFontHeightF     = 0.0125
      txOpts@txFontThicknessF  = 1.0

      pinc = 10.

      fooLinePseudo = new (npseudo,"graphic")
      fooTextPseudo = new (npseudo,"graphic")

      do i=0,npseudo-1
         p = 1060.
         do j=0,dimsizes(sy)-1
            p = p-pinc
            if (p.lt.230.) then      ; was "250"
                break
            end if
            sy(j) = skewty(p)
           ;t     = THERMO::satlft(pseudo(i),p)    ; temp on moist adiabat at p
            t     = satlft_skewt(pseudo(i),p)      ; temp on moist adiabat at p
            sx(j) = skewtx(t,sy(j))
           ;print ("j="+j+"  p="+p+"  t="+t+"   sy(j)="+sy(j)+"  sx(j)="+sx(j) )  
         end do            ; end "j=0,dimsizes(sy)-1"

         fooLinePseudo(i) = gsn_add_polyline (wks,xy,sx(:j-1),sy(:j-1),gsOpts)  

         ylab  = skewty(p+0.5*pinc)
        ;t     = THERMO::satlft(pseudo(i),p+0.75*pinc)
         t     = satlft_skewt(pseudo(i),p+0.75*pinc)
         xlab  = skewtx(t,ylab)
         label = toint(pseudo(i))        ; 9 Feb 99 fix 
         fooTextPseudo(i) = gsn_add_text(wks,xy,label,xlab,ylab,txOpts)
  
      end do               ; end "i-0,dimsizes(pseudo)-1"

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooLinePseudo
      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooTextPseudo

      delete (gsOpts)
      delete (txOpts)
  end if    ; DrawMoistAdiabat

  if (localOpts@DrawStandardAtm) then
      gsOpts                   = True 
      gsOpts@gsLineColor       = colTan   
      gsOpts@gsLineThicknessF  = localOpts@DrawStandardAtmThk
      gsOpts@gsLineDashPattern = 0            
    
      do i=0,nlvl-1
         sy(i) = skewty (psa(i))
         sx(i) = skewtx (tsa(i), sy(i) )
      end do
 
      fooLineStdAtm = gsn_add_polyline (wks,xy,sx(0:nlvl-1),sy(0:nlvl-1),gsOpts)

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooLineStdAtm

      delete (gsOpts)
  end if    ; DrawStandardAtm

; --- draw vertical line upon which to plot wind barbs.

  if (localOpts@DrawWind) then
      gsOpts = True 
      gsOpts@gsLineColor       = "Foreground"     
      gsOpts@gsLineThicknessF  = 1.0
      gsOpts@gsLineDashPattern = 0            
      gsOpts@gsMarkerIndex     = 4   ; "hollow_circle"=> std pres
      gsOpts@gsMarkerColor     = "Foreground"


      presWind      = pres
      presWind(0)   = 1013.          ; override 1050
      xWind         = skewtx (45. , skewty(presWind(0)))
      sx(0:npres-1) = xWind            ; "x" location of wind plot
      sy(0:npres-1) = skewty(presWind)

      fooLineWindP = gsn_add_polyline   (wks,xy,sx(0:npres-1),sy(0:npres-1),gsOpts)  
      fooPolyWindP = gsn_add_polymarker (wks,xy,sx(1:npres-1),sy(1:npres-1),gsOpts)

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooLineWindP
      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooPolyWindP
                                     ; zwind => Pibal reports
      zftWind = (/ 0., 1., 2., 3., 4., 5., 6., 7., 8., 9., 10. \
                 ,12.,14.,16.,18.,20.,25.,30.,35.,40.,45.,50. /)
      zkmWind = zftWind*f2m
      pkmWind = ftcurv(zsa,psa,zkmWind)
      nzkmW   = dimsizes (zkmWind)


      sx(0:nzkmW-1)            = xWind  ; "x" location of wind plot
      sy(0:nzkmW-1)            = skewty(pkmWind)
      gsOpts@gsMarkerIndex     = 16     ; "circle_filled" -> Pibal
      gsOpts@gsMarkerSizeF     = 0.007 ; 0.007 is default
      gsOpts@gsMarkerThicknessF= 1.0    ; 1.0 is default

      fooPolyWindPZ = gsn_add_polymarker (wks,xy,sx(0:nzkmW-1),sy(0:nzkmW-1),gsOpts)

      dumname = unique_string("dum")     ; for panel
      xy@$dumname$ = fooPolyWindPZ

      delete (gsOpts)
  end if    ; DrawWind

  fooOutline = gsn_add_polyline (wks,xy,xc,yc,False)   ; add outline

  dumname = unique_string("dum")     ; for panel
  xy@$dumname$ = fooOutline

  xy@Panel = True  ; flag

;---Restore color map, if necessary
  if(.not.localOpts@UseMyColorMap) then
    gsn_define_colormap(wks,cmap_orig)
  end if

  return (xy)      ; return object

  end

; ------------------------------------------------------

undef("skewT_PlotData_Panel")
function skewT_PlotData_Panel (wks:graphic, skewt_bkgd:graphic \
                        ,P[*]:numeric,TC[*]:numeric      \
                        ,TDC[*]:numeric,Z[*]:numeric     \
                        ,WSPD[*]:numeric,WDIR[*]:numeric \
                        ,dataOpts:logical                )
 begin
                        ; determine index of non-msg values
                        ; used in plotting the sounding and
                        ; calculating thermodynamic quantities
  idx = ind (.not.ismissing(P)   .and. .not.ismissing(TC)  .and. \
             .not.ismissing(TDC) .and. P.ge.100. )

  p   = P(idx)          ; transfer non-msg values to local arrays
  tc  = TC(idx)         ; *generally* these local arrays are used
  tdc = TDC(idx)

  if (any(p .gt. 1100.)) then
    print ("skewT_PlotData: pressure values are out of range (must be in millibars).")
    exit
  end if
  if (any(tc .gt. 150.)) then
    print ("skewT_PlotData: temperature values are out of range for Fahrenheit or Celsius.")
    exit
  end if
  if (any(tdc .gt. 150.)) then
    print ("skewT_PlotData: dew point temperature values are out of range for Fahrenheit or Celsius.")
    exit
  end if

 ;print (" non-msg:  p="+p+"  tc="+tc+"  tdc="+tdc+ \
 ;       " other     Z="+Z(idx)+"  WSPD="+WSPD(idx)+"  WDIR="+WDIR(idx) )

  localOpts           = True     ; options describing data and ploting
  localOpts@PrintZ    = True     ; print geopotential (Z) on skewT diagram
                                 ; wmbarb can not be paneled
  localOpts@PlotWindP = False    ; plot wind barbs at p lvls
  localOpts@WspdWdir  = False    ; wind speed and dir [else: u,v]
  localOpts@PlotWindH = False    ; plot wind barbs at h lvls [pibal; special]
  localOpts@HspdHdir  = False    ; wind speed and dir [else: u,v]
  localOpts@Wthin     = 1        ; plot every wind barb          

  localOpts@ThermoInfo= True     ; print thermodynamic info
  localOpts@Cape      = True     ; plot CAPE parcel profile if cape>0
  localOpts@Parcel    = 0        ; subscript corresponding to initial parcel

                                 ; Override the default localOpts
                                 ; with input dataOpts
  if (dataOpts .and. .not.any(ismissing(getvaratts(dataOpts)))) then
      localAtts = getvaratts(localOpts)
      dataAtts  = getvaratts(dataOpts)
      nOpts     = dimsizes  (dataAtts)
      do i=0,nOpts-1            ; loop and add new attributes
         localOpts@$dataAtts(i)$ = dataOpts@$dataAtts(i)$
      end do
  end if
                                 ; Force regardless of dataOpts
                                 ; wmbarb can not be paneled
  localOpts@PlotWindP = False    ; plot wind barbs at p lvls
  localOpts@WspdWdir  = False    ; wind speed and dir [else: u,v]
  localOpts@PlotWindH = False    ; plot wind barbs at h lvls [pibal; special]
  localOpts@HspdHdir  = False    ; wind speed and dir [else: u,v]

  getvalues skewt_bkgd
     "vpXF"                  : vpXF
     "vpYF"                  : vpYF
     "vpWidthF"              : vpWidthF
     "vpHeightF"             : vpHeightF
     "tiMainFont"            : tiMainFont
     "tiMainFontHeightF"     : tiMainFontHeightF
     "tiMainOffsetXF"        : tiMainOffsetXF 
     "tmYLLabelFontHeightF"  : tmYLLabelFontHeightF
  end getvalues
                                          ; assorted color 'indices'
  colForeGround = "Foreground"            ; defaults
  colTemperature= "Foreground"
  colDewPt      = "RoyalBlue" 
  colCape       = "Red"
  colZLabel     = colTemperature
  colWindP      = colTemperature
  colWindZ      = colTemperature
  colWindH      = colTemperature
  colThermoInfo = "Sienna"
                                          ; Change defaults
  if (isatt(localOpts,"colTemperature")) then
      colTemperature  = localOpts@colTemperature  ; new color   
  end if
  if (isatt(localOpts,"colDewPt")) then
      colDewPt  = localOpts@colDewPt              ; new color   
  end if
  if (isatt(localOpts,"colCape")) then
      colCape   = localOpts@colCape               ; new color   
  end if
  if (isatt(localOpts,"colZLabel")) then
      colZLabel = localOpts@colZLabel             ; new color   
  end if
  if (isatt(localOpts,"colWindP")) then
      colWindP  = localOpts@colWindP              ; new color   
  end if
  if (isatt(localOpts,"colWindZ")) then
      colWindZ  = localOpts@colWindZ              ; new color   
  end if
  if (isatt(localOpts,"colWindH")) then
      colWindH  = localOpts@colWindH              ; new color   
  end if
  if (isatt(localOpts,"colThermoInfo")) then
      colThermoInfo  = localOpts@colThermoInfo    ; new color   
  end if
                                          ; Change defaults
                                          ; gs settings for gsn_polyline
  gsOpts                   = True
  gsOpts@gsLineDashPattern = 0            ; solid (default)
  gsOpts@gsLineThicknessF  = 6.0          ; make thicker

  yp   = skewty (p)                    
  xtc  = skewtx (tc , yp)              ; T-P plot
  gsOpts@gsLineColor        = colTemperature   
  gsOpts@gsLineDashPattern  = 0   
  if (isatt(localOpts,"linePatternTemperature")) then
      gsOpts@gsLineDashPattern = localOpts@linePatternTemperature
  end if
  if (isatt(localOpts,"lineThicknessTemperature")) then
      gsOpts@gsLineThicknessF = localOpts@lineThicknessTemperature
  end if

  fooLineTC = gsn_add_polyline  (wks,skewt_bkgd,xtc ,yp,gsOpts) 
  dumname = unique_string("dum")
  skewt_bkgd@$dumname$ = fooLineTC 

  xtdc = skewtx (tdc, yp)              ;Dew Pt-P plot
  gsOpts@gsLineColor        = colDewPt     
  gsOpts@gsLineDashPattern  = 0
  if (isatt(localOpts,"linePatternDewPt")) then
      gsOpts@gsLineDashPattern  = localOpts@linePatternDewPt
  end if
  fooLineTDC = gsn_add_polyline  (wks,skewt_bkgd,xtdc,yp,gsOpts) 
  dumname = unique_string("dum")
  skewt_bkgd@$dumname$ = fooLineTDC 

  delete (gsOpts)

  fmsg = default_fillvalue(typeof(tc))   ; get default missing

  if (localOpts@ThermoInfo) then
      nP   = localOpts@Parcel  ; default is the lowest level [0]
      nlvls= dimsizes(p)
      plcl = fmsg             ; p (hPa) Lifting Condensation Lvl (lcl)
      tlcl = fmsg             ; temperature (C) of lcl
     ;THERMO::ptlcl(p(nP),tc(nP),tdc(nP),plcl,tlcl)
      ptlcl_skewt(p(nP),tc(nP),tdc(nP),plcl,tlcl)
     ;shox = THERMO::showal(p,tc,tdc,nlvls)   ; Showwalter Index
      shox = showal_skewt(p,tc,tdc)           ; Showwalter Index
     ;pwat = THERMO::precpw(tdc,p,nlvls)      ; precipitable water (cm)
      pwat = pw_skewt(tdc,p)                  ; precipitable water (cm)
      iprnt= 0                                ; debug only (>0)
      nlLcl= 0                                
      nlLfc= 0
      nlCross= 0
     ;cape = THERMO::cape_ncl(p,tc,nlvls,plcl,iprnt,tpar,tmsg \
     ;                       ,nlLcl,nlLfc,nlCross)
      cape = cape_thermo(p,tc,plcl,iprnt)
      tpar = cape@tparcel
      nlLcl= cape@jlcl
      nlLfc= cape@jlfc
      nlCross= cape@jcross
                                              ; 0.5 is for rounding
      info = " Plcl="    +toint(plcl+0.5) \ 
           + " Tlcl[C]=" +toint(tlcl+0.5) \
           + " Shox="    +toint(shox+0.5) \
           + " Pwat[cm]="+toint(pwat+0.5) \
           + " Cape[J]= "+toint(cape)

    ;;txOpts                   = True
    ;;txOpts@txAngleF          = 0.
    ;;txOpts@txFont            = tiMainFont
    ;;txOpts@txFontColor       = colThermoInfo
    ;;txOpts@txFontHeightF     = 0.5*tiMainFontHeightF
    ;;xinfo                    = vpXF  + 0.5*vpWidthF + tiMainOffsetXF
    ;;yinfo                    = vpYF  + 0.5*tiMainFontHeightF 
    ;;if (isatt(localOpts,"offsetThermoInfo")) then
    ;;    yinfo = yinfo + localOpts@offsetThermoInfo
    ;;end if
    ;;gsn_text_ndc (wks,info,xinfo,yinfo,txOpts) 
    ;;delete (txOpts)

      txThermo                    = True
      if (isatt(localOpts,"txFontHeightThermo")) then
          txThermo@txFontHeightF  = localOpts@txFontHeightThermo
      else
          txThermo@txFontHeightF  = 0.65*tiMainFontHeightF    
      end if
      if (isatt(localOpts,"txBackgroundFillColorThermo")) then
          txThermo@txFontHeightF  = localOpts@txBackgroundFillColorThermo
      else
          txThermo@txBackgroundFillColor = "white"  
      end if
      txThermoInfo = gsn_create_text(wks, info, txThermo)
     
      amresThermo = True
     ;amresThermo@amParallelPosF   =  0.0    ; This is the center of the plot.
     ;amresThermo@amOrthogonalPosF = -0.5    ; This is the top edge of the plot.
      if (isatt(localOpts,"amParallelPosF")) then
           amresThermo@amParallelPosF   = localOpts@amParallelPosF
      else
           amresThermo@amParallelPosF   = -0.1    
      end if
      if (isatt(localOpts,"amOrthogonalPosF")) then
          amresThermo@amOrthogonalPosF = localOpts@amOrthogonalPosF
      else
          amresThermo@amOrthogonalPosF = -0.42 
      end if
      if (isatt(localOpts,"amJust")) then
          amresThermo@amJust = localOpts@amJust
      else
          amresThermo@amJust = "BottomCenter"   
      end if
      annoidThermo = gsn_add_annotation(skewt_bkgd, txThermoInfo, amresThermo)

      if (localOpts@Cape .and. cape.gt.0.) then
          gsOpts                   = True
          gsOpts@gsLineColor       = colCape   
          gsOpts@gsLineDashPattern = 1         ; 14          
          if (isatt(localOpts,"gsLineDashPatternCape")) then
              gsOpts@gsLineDashPattern  = localOpts@linePatternCape
           end if
          gsOpts@gsLineThicknessF  = 2.0

          yp   = skewty (p)                    
          xtp  = skewtx (tpar, yp)
         
          fooLineCape = gsn_add_polyline(wks,skewt_bkgd,xtp(nlLfc:nlCross)\
                                        , yp(nlLfc:nlCross),gsOpts) 
          dumname = unique_string("dum")
          skewt_bkgd@$dumname$ = fooLineCape
          delete (gsOpts)
      end if

  end if    ; ThermoInfo

  if (localOpts@PrintZ) then           ; print geopotential (?)
      txOpts               = True
      txOpts@txAngleF      = 0.
      txOpts@txFontColor   = colZLabel
      txOpts@txFontHeightF = 0.9*tmYLLabelFontHeightF
                                       ; levels at which Z is printed
      Pprint = (/1000., 850., 700., 500., 400., 300. \
               ,  250., 200., 150., 100. /)

      yz = skewty (1000.) 
      xz = skewtx (-30., yz)           ; constant "x"

      fooTextPrintZ = new (dimsizes(P), "graphic")
      do nl=0,dimsizes(P)-1            ; use input arrays
         if (.not.ismissing(P(nl)) .and. .not.ismissing(Z(nl)) .and. \
             any(Pprint.eq.P(nl))) then
             yz  = skewty (P(nl)) 
             fooTextPrintZ(nl) = gsn_add_text \
                          (wks,skewt_bkgd,toint(Z(nl)),xz,yz,txOpts) 
         end if
      end do          ; nl

      dumname = unique_string("dum")
      skewt_bkgd@$dumname$ = fooTextPrintZ

      delete (txOpts)
  end if

  if (localOpts@PlotWindP) then
          gsOpts                   = True
          gsOpts@gsLineThicknessF  = 1.0
      if (.not.all(ismissing(WSPD))) then
                   ; IDW - indices where P/WSPD/WDIR are not all missing
          IDW = ind (.not.ismissing(P)    .and. P.ge.100. .and.     \
                     .not.ismissing(WSPD) .and. .not.ismissing(WDIR) )

          if (isatt(localOpts,"Wthin") .and. localOpts@Wthin.gt.1) then
              nThin = localOpts@Wthin
              idw   = IDW(::nThin)
          else
              idw   = IDW
          end if

          pw  = P(idw)   

          wmsetp ("wdf", 1)                 ; meteorological dir (Sep 2001)

          if (localOpts@WspdWdir) then      ; wind spd,dir (?)
              dirw = 0.017453*WDIR(idw)
              up   = -WSPD(idw)*sin(dirw)
              vp   = -WSPD(idw)*cos(dirw)
          else
              up   = WSPD(idw)              ; must be u,v components
              vp   = WDIR(idw)
          end if

          wbcol = wmgetp("col")             ; get current wbarb color
          wmsetp("col",getSkewTColorIndex(wks,colWindP)) ; set new color 
                                                ; see skewT_BackGround
          ypWind = skewty (pw) 
          xpWind = new (dimsizes(pw), float)
          if (isatt(localOpts,"xpWind")) then
              xpWind = skewtx (localOpts@xpWind , skewty(1013.) ) ; location of wind barb
          else
              xpWind = skewtx (45. , skewty(1013.) ) ; location of wind barb
          end if

          wmbarb(wks, xpWind, ypWind, up, vp )
          wmsetp("col",wbcol)               ; reset to initial color value 
                                            ; chk for soundings where Z/wind
                                            ; were merged but no pressure
          idz = ind ( ismissing(P) .and.  .not. ismissing(Z) .and.   \
                     .not.ismissing(WSPD) .and. .not.ismissing(WDIR) )
          if (.not.all(ismissing(idz))) then
              zw  = Z(idz)   
              if (localOpts@WspdWdir) then      ; wind spd,dir (?)
                  dirz = 0.017453*WDIR(idz)
                  uz   = -WSPD(idz)*sin(dirz)
                  vz   = -WSPD(idz)*cos(dirz)
              else
                  uz   = WSPD(idz)              ; must be u,v components
                  vz   = WDIR(idz)
              end if
                                                ; idzp=indices non-msg Z and P
              idzp = ind (.not.ismissing(P) .and. .not.ismissing(Z))
              pz   = ftcurv (Z(idzp),P(idzp),zw)  ; map zw to p levels

              wbcol = wmgetp("col")             ; get current wbarb color
              wmsetp("col",getSkewTColorIndex(wks,colWindZ)) ; set new color 
                                                ; see skewT_BackGround
              yzWind = skewty (pz) 
              xzWind = new (dimsizes(pz), float)
              xzWind = skewtx (45. , skewty(1013.) ) ; location of wind barb
              wmbarb(wks, xzWind, yzWind, uz, vz )
              wmsetp("col",wbcol)               ; reset to initial color value 

          end if   ; end ".not.ismissing(idz)"
      end if       ; end ".not.all(ismissing(WSPD))"
  end if           ; end "PlotWindP"
                                                ; SPECIAL SECTION [IGNORE]
                                                ; allows 'other' winds to be
                                                ; input as attributes of sounding
  if (localOpts@PlotWindH) then
      if (isatt(dataOpts,"Height") .and. isatt(dataOpts,"Hspd") \
                                   .and. isatt(dataOpts,"Hdir") ) then
          dimHeight = dimsizes(dataOpts@Height)
          dimHspd   = dimsizes(dataOpts@Hspd  )
          dimHdir   = dimsizes(dataOpts@Hdir  )
          if (dimHeight.eq.dimHspd .and. dimHeight.eq.dimHdir .and. \
              .not.all(ismissing(dataOpts@Height))) then
              if (localOpts@HspdHdir) then      ; wind spd,dir (?)
                  dirh= 0.017453*dataOpts@Hdir
                  uh  = -dataOpts@Hspd*sin(dirh)
                  vh  = -dataOpts@Hspd*cos(dirh)
              else
                  uh  = dataOpts@Hspd           ; must be u,v components
                  vh  = dataOpts@Hdir
              end if  ; end "localOpts@HspdHdir"

              idzp = ind (.not.ismissing(P) .and. .not.ismissing(Z))
              ph   = ftcurv (Z(idzp),P(idzp),dataOpts@Height) ; Height to p levels

              wbcol = wmgetp("col")             ; get current color index
              wmsetp("col",getSkewTColorIndex(wks,colWindH)) ; set new color 

              yhWind = skewty (ph) 
              xhWind = new (dimsizes(ph), float)
              xhWind = skewtx (45. , skewty(1013.) ) ; location of wind barb
              wmbarb(wks, xhWind, yhWind, uh, vh )
              wmsetp("col",wbcol)               ; reset to initial color value 
          end if
      else
          print ("Opts@PlotWindH=True but dataOpts@Height/Hspd/Hdir msg")
      end if
  end if  ; end "PlotWindH"
                             ; attach to object
  if (isvar("cape")) then
      skewt_bkgd@Cape = (/ cape /)
      skewt_bkgd@Pwat = pwat            ; cm
      skewt_bkgd@Shox = shox
      skewt_bkgd@Plcl = plcl  
      skewt_bkgd@Tlcl = tlcl  
  end if

  return (skewt_bkgd)

 end
; =========Interface to original and panel===================
undef("skewT_BackGround")
function skewT_BackGround (wks:graphic,  Opts:logical)
begin
   if (Opts .and. isatt(Opts,"Panel") .and. Opts@Panel) then
       return( skewT_BackGround_Panel(wks, Opts) )
   else
       return( skewT_BackGround_Original(wks, Opts) )
   end if
end
 
; =========Interface to original and panel===================

undef("skewT_PlotData")
function skewT_PlotData (wks:graphic, skewt_bkgd:graphic \
                        ,P[*]:numeric,TC[*]:numeric      \
                        ,TDC[*]:numeric,Z[*]:numeric     \
                        ,WSPD[*]:numeric,WDIR[*]:numeric \
                        ,dataOpts:logical                )
begin
   if (isatt(skewt_bkgd,"Panel")) then
       return(skewT_PlotData_Panel(wks, skewt_bkgd ,P,TC,TDC \
                                  ,Z,WSPD,WDIR,dataOpts) )
   else
       return(skewT_PlotData_Original(wks, skewt_bkgd ,P,TC,TDC \
                                  ,Z,WSPD,WDIR,dataOpts) )
   end if
end
