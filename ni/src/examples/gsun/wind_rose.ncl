;
;      $Id: wind_rose.ncl,v 1.8 2009-11-04 18:02:04 shea Exp $
;
; ******************************************************************
; *******  THERE IS NO SUPPORT FOR THE WIND ROSE FUNCTIONS *********
; ******************************************************************
; *******  These were developed to draw the figures the way I like them.
; *******  If the user wishes to change something to their preference,
; *******  they may excerpt the relevent function and modify as they wish.
; *******  Good luck!
; ******************************************************************
; functions to draw variations of wind roses
;---------------------------------------
undef("WindRoseBasicF")
function WindRoseBasicF(wks:graphic, wspd[*]:numeric, wdir[*]:numeric \
                                   , numPetals[1]:integer, circFr[1]:numeric, wrRes[1]:logical)

; Plot a Basic Wind Rose [ie, frequency of wind direction + avg spd]
; Nomenclature
;      wspd      - wind speed (any units)
;      wdir      - wind direction (degrees)
;                  meteorological: dir from which the wind blows
;                              eg: 90 deg means blowing from the east
;      numPetals - number wind wind rose directions 
;                  eg: numPetals=8 is common
;      circFr    - intervals at which frequency circles are drawn
;                  eg: circFr=10. is common
;      wrRes     - resources which affect plot appearance    
;
; Note:
; It is best to set "numPetals" to a number like 8  or 16.
; [Eg, if numPetals=4 then a direction of 45 degrees could be
; counted as a north or east wind (or both) depending on the
; if statement below. Actually, the way I have implemented
; it, it would be counted as BOTH a north and an East wind.]
;
; Note2: best to set the wks as post script


 begin
  debug = False
 
  opts = True           ; local and default options
  if (wrRes) then
      opts = wrRes      ; if True over ride local/defaults with input

                        ; avoid warning messages and delete the following 'wr' (wind rose) attributes 
                        ; use them explicitly later
      if (isatt(wrRes,"petalLabExtF")) then
         delete(opt@petalLabExtF)
      end if
      if (isatt(wrRes,"txFontHeightF")) then
          delete(opt@txFontHeightF)
      end if
      if (isatt(wrRes,"minWindPrintF")) then
          delete(opt@minWindPrintF)
      end if

  end if

  nP     = numPetals            ; for convenience only
  nW     = dimsizes(wspd)       ; total number of elements
  rad    = 4.*atan(1.0)/180.    ; degress to radians
                                ; compute wind components
  u      = -wspd*sin(wdir*rad)  ; u component (zonal) 
  v      = -wspd*cos(wdir*rad)  ; v component (meridional)
  uAve   = avg(u)
  vAve   = avg(v)
  wAve   = avg(wspd) 
  wStd   = stddev(wspd) 
  wDir   = atan2(uAve,vAve)/rad +180. ; mean wind direction [0,360]
  wNum   = num( .not.ismissing(wspd) ); total number of non-msg values
  nCalm  = num( wspd.eq.0.)*1.0       ; total calm reports (float)

  wedge  = 360./nP                    ; number of directional wedges
  wedge2 = wedge/2.
                                      ; extra wedge [see below]
  wedgeL = fspan (-wedge2, 360.-wedge2, nP+1) ; "left"  edges
  wedgeR = fspan ( wedge2, 360.+wedge2, nP+1) ; "right" edges

  dirWr  = fspan (0., 360.-wedge, nP) ; central petals of the wind rose

  if (debug) then
      print ("wedgeL="+wedgeL+"  wedgeR="+wedgeR)  
      print ("dirWr="+dirWr)
  end if
                                      ; perform the counting and calculations
  dirWg = new (nP,float)              ; direction counts in each wedge (Wg) 
  spdWg = new (nP,float)              ; mean   speed in each wedge (Wg)
  stdWg = new (nP,float)              ; stddev speed in each wedge (Wg)
  do n=0,nP-1                         ; loop thru each wedge
     if (n.eq.0) then                 ; n=0 SPECIAl due to 360 <=> 0 crossover
         dirWg(n) = num(wspd.gt.0. .and. (wdir.ge.wedgeL(nP) .or. wdir.le.wedgeR(0)))
         indx     = ind(wspd.gt.0. .and. (wdir.ge.wedgeL(nP) .or. wdir.le.wedgeR(0)))
     else
         dirWg(n) = num(wspd.gt.0. .and. (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)))
         indx     = ind(wspd.gt.0. .and. (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)))
     end if

     if (dirWg(n).gt.0.) then         ; calculate ave spd and stddev per wedge
         spdWg(n) = avg(wspd(indx))
         stdWg(n) = stddev(wspd(indx))
     end if

    ;if (debug .and. .not.any(ismissing(indx))) then
    ;    print("indx="+indx+"  wdir(indx)="+wdir(indx) \
    ;                      +"  wspd(indx)="+wspd(indx))
    ;end if
     delete (indx)
  end do
 ;if (debug .and. .not.any(ismissing(dirWg))) then
 ;    print ("dirWr="+dirWr+"  dirWg="+dirWg \
 ;                         +"  spdWg="+spdWg+"  stdWg="+stdWg)
 ;end if

  wTot = sum(dirWg)                   ; total number of directional counts
                                      ; if overlap wTot >= wNum

  dirFr    = (dirWg/wTot)*100.        ; % freq for each direction wedge
  dirFrMax = max (dirFr)              ; max directioonal frequency [used for plot]

  total = sum(dirFr)                  ; make sure we have 100% total
  if (debug) then
      print ("wNum="+wNum+"   wTot="+wTot+"   total="+total)
  end if

                                      ; Draw the "percentage frequency" circles
  dirFrMaxNumCircles = toint(dirFrMax/circFr +1.)
  dirFrMaxNum        = circFr*dirFrMaxNumCircles   ; number of circles

  nCirc = 361                         ; do this once
  xCirc = new ( (/dirFrMaxNumCircles,nCirc /) , float)
  yCirc = xCirc
  xcos  = cos(fspan(0, 360, nCirc)*rad)*circFr
  xsin  = sin(fspan(0, 360, nCirc)*rad)*circFr

  do n=0,dirFrMaxNumCircles-1         ; plot coordinates for freq circles
     xCirc(n,:) = (n+1)*xcos(:)
     yCirc(n,:) = (n+1)*xsin(:)
  end do
  delete(xcos)
  delete(xsin)
                                      ; Specify data limits for X and Y axes.
  extraSpace        = max((/3.,circFr/3./))    ; Extra space beyond outer circle
  opts@trXMinF      = -dirFrMaxNum-extraSpace  ; min X 
  opts@trXMaxF      =  dirFrMaxNum+extraSpace  ; max X
  opts@trYMinF      = -dirFrMaxNum-extraSpace  ; min Y
  opts@trYMaxF      =  dirFrMaxNum+extraSpace  ; max Y
  opts@tmXTOn       = False                    ; turn off tick marks on each side
  opts@tmXBOn       = False
  opts@tmYLOn       = False
  opts@tmYROn       = False
  opts@tmXBBorderOn = False                    ; turn off borders on each side
  opts@tmXTBorderOn = False
  opts@tmYLBorderOn = False
  opts@tmYRBorderOn = False
  opts@gsnFrame     = False                    ; do not advance frame
  opts@gsnDraw      = False                    ; do not draw
  opts@xyMonoDashPattern = True                ; set all circles to solid
 
  plotWr = gsn_xy(wks,xCirc,yCirc,opts) ; GSUN routine to draw circles 

  wAveStr = new ( 1, string)            ; pre-define variable of type string
  txRes   = True                        ; set text resources
  txRes@txFontHeightF     = 0.0100
  txRes@txAngleF          = -60.
  gsRes   = True                        ; set polyline resources
  gsRes@gsLineThicknessF  = 3.0         ; Extra thick lines

  do n=0,dirFrMaxNumCircles-1           ; label % Freq circles
     xP = 1.0+ (n+1)*circFr*cos((dirWr(0)+wedge2)*rad)
     yP = 1.0+ (n+1)*circFr*sin((dirWr(0)+wedge2)*rad)
     wAveStr = toint((n+1)*circFr)+"%"  
     astring = systemfunc("echo text1$$")
     plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr,xP,yP,txRes) ; add text at end
  end do
  delete(txRes@txAngleF)
                                        ; draw petals (or spokes .. whatever)
  cext         = 1.00                   ; good for numPetals=8 
  minWindPrintF = 0.0
  txRes@txFontHeightF     = 0.0140

  if (numPetals.ge.12 .and. numPetals.le.16) then
      cext = 0.75 
      txRes@txFontHeightF = 0.0125
  end if
  if (numPetals.gt.16 .and. numPetals.le.24) then
      cext = 0.50 
      txRes@txFontHeightF = 0.0100
  end if
  if (numPetals.gt.24) then
      cext = 0.25 
      txRes@txFontHeightF = 0.0100
  end if

  if (isatt(wrRes,"petalLabExtF")) then
      cext = wrRes@petalLabExtF
  end if
  if (isatt(wrRes,"txFontHeightF")) then
      txRes@txFontHeightF = wrRes@txFontHeightF
  end if
  if (isatt(wrRes,"minWindPrintF")) then
      minWindPrintF = wrRes@minWindPrintF
  end if

  do n=0,nP-1                           ; loop thru each petal (wedge)
     if (dirWg(n).gt.0.) then           
         xP      = dirFr(n)*sin(dirWr(n)*rad)         ; math coords
         yP      = dirFr(n)*cos(dirWr(n)*rad)
         astring = systemfunc("echo line1$$")         ; some unique id
         plotWr@$astring$ = gsn_add_polyline(wks,plotWr,(/0.,xP/),(/0.,yP/),gsRes) ; draw spoke
                                                      ; write mean speed 
         xPtx    = xP + cext*sin(dirWr(n)*rad)  
         yPtx    = yP + cext*cos(dirWr(n)*rad)
         if ((spdWg(n)+0.5) .gt. minWindPrintF) then
             wAveStr = toint(spdWg(n)+0.5)       ; round mean speed
             astring = systemfunc("echo text2$$")
             plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr,xPtx,yPtx,txRes) ; add text at end
         end if
     end if
  end do
                                                      ; Manually anotate plot
  wAveStr = "SpdAve="+toint(wAve+0.5)+"  "+ \
            "SpdStd="+toint(wStd+0.5)+"  "+ \
            "DirAve="+toint(wDir+0.5)
  if (nCalm.gt.0) then
      calmFr = (nCalm/wNum)*100.                      ; % freq Calm
      wAveStr = wAveStr +"   Calm="+sprintf("%4.1f", calmFr)+"%"
  else
      wAveStr = wAveStr +"   No Calm Reports"
  end if

  wAveStr = wAveStr +"  Nwnd="+wNum

  txRes@txJust = "CenterCenter"
  astring = systemfunc("echo text3$$")
  plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr, 0.0,dirFrMaxNum+extraSpace,txRes)

  wAveStr = "Frequency circles every "+circFr+"%. Mean speed indicated."  
  astring = systemfunc("echo text4$$")
 ;plotWr@$astring$    = gsn_add_text(wks,plotWr,wAveStr, 0.0,-dirFrMaxNum-extraSpace,txRes)
  txRes@txFontHeightF = 0.70*txRes@txFontHeightF
  plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr, 0.0,dirFrMaxNum+0.4*extraSpace,txRes)

  if (isatt(wrRes,"gsnDraw")) then 
      if (wrRes@gsnDraw) then 
          draw(plotWr)
      end if
  else 
      draw(plotWr)
  end if
  if (isatt(wrRes,"gsnFrame")) then 
      if (wrRes@gsnFrame) then 
          frame (wks)
      end if
  else 
      frame (wks)
  end if
  return (plotWr)

 end
;---------------------------------------
undef("WindRoseColorF")
function WindRoseColorF(wks:graphic, wspd[*]:numeric, wdir[*]:numeric \
                                   , numPetals[1]:integer, circFr[1]:numeric \
                                   , spdBounds[*]:float, colorBounds[*]:string, wrRes[1]:logical)

; Plot a Wind Rose [ie, frequency of wind direction + avg spd]
; Nomenclature
;      wspd        - wind speed (any units)
;      wdir        - wind direction (degrees)
;                    meteorological: dir from which the wind blows
;                                eg: 90 deg means blowing from the east
;      numPetals   - number wind wind rose directions 
;      circFr      - intervals at which frequency circles are drawn
;                    eg: circFr=10. is common
;      spdBounds   - bounds for different colors
;                    eg: spdBounds = (/ 10., 20., 30., 100. /)
;                        [0+ ==>10; 10+ ==>20; 20+ ==> 30; 30+ ==>100]
;      colorBounds - colrs for each spdBounds interval
;                        colorBounds = (/ "red", "green", "blue", "brown" /)
;      wrRes       - resources which affect plot appearance    
;
; Note1:
; It is best to set "numPetals" to a number like 8  or 16.
; [Eg, if numPetals=4 then a direction of 45 degrees could be
; counted as a north or east wind (or both) depending on the
; if statement below. Actually, the way I have implemented
; it, it would be counted as BOTH a north and an East wind.]
;
; Note2: best to use the wks as post script

 begin
  debug = False
 
  opts = True           ; local and default options
  if (wrRes) then
      opts = wrRes      ; if True over ride local/defaults with input

                        ; avoid warning messages and delete the following 'wr' (wind rose) attributes 
                        ; use them explicitly later
      if (isatt(wrRes,"petalLabExtF")) then
         delete(opts@petalLabExtF)
      end if
      if (isatt(wrRes,"txFontHeightF")) then
          delete(opts@txFontHeightF)
      end if
      if (isatt(wrRes,"minWindPrintF")) then
          delete(opts@minWindPrintF)
      end if

  end if

  nP     = numPetals            ; for convenience only
  nW     = dimsizes(wspd)       ; total number of elements
  rad    = 4.*atan(1.0)/180.    ; degress to radians
                                ; compute wind components
  u      = -wspd*sin(wdir*rad)  ; u component (zonal) 
  v      = -wspd*cos(wdir*rad)  ; v component (meridional)
  uAve   = avg(u)
  vAve   = avg(v)
  wAve   = avg(wspd) 
  wStd   = stddev(wspd) 
  wDir   = atan2(uAve,vAve)/rad +180. ; mean wind direction [0,360]
  wNum   = num( .not.ismissing(wspd) ); total number of non-msg values
  nCalm  = num( wspd.eq.0.)*1.0       ; total calm reports

  wedge  = 360./nP                    ; number of directional wedges
  wedge2 = wedge/2.
                                      ; extra wedge [see below]
  wedgeL = fspan (-wedge2, 360.-wedge2, nP+1) ; "left"  edges
  wedgeR = fspan ( wedge2, 360.+wedge2, nP+1) ; "right" edges

  dirWr  = fspan (0., 360.-wedge, nP) ; central petals of the wind rose

  if (debug) then
      print ("wedgeL="+wedgeL+"  wedgeR="+wedgeR)  
      print ("dirWr="+dirWr)
  end if
                                      ; perform the counting and calculations
  dirWg = new (nP,float)              ; direction counts in each wedge (Wg) 
  spdWg = new (nP,float)              ; overall mean   speed in each wedge (Wg)
  stdWg = new (nP,float)              ; overall stddev speed in each wedge (Wg)
  do n=0,nP-1                         ; loop thru each wedge
     if (n.eq.0) then                 ; n=0 SPECIAl due to 360 <=> 0 crossover
         dirWg(n) = num(wspd.gt.0. .and. (wdir.ge.wedgeL(nP) .or. wdir.le.wedgeR(0)))
         indx     = ind(wspd.gt.0. .and. (wdir.ge.wedgeL(nP) .or. wdir.le.wedgeR(0)))
     else
         dirWg(n) = num(wspd.gt.0. .and. (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)))
         indx     = ind(wspd.gt.0. .and. (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)))
     end if

     if (dirWg(n).gt.0.) then         ; calculate ave spd and stddev per wedge
         spdWg(n) = avg(wspd(indx))
         stdWg(n) = stddev(wspd(indx))
     end if

    ;if (debug .and. .not.any(ismissing(indx))) then
    ;    print("indx="+indx+"  wdir(indx)="+wdir(indx) \
    ;                      +"  wspd(indx)="+wspd(indx))
    ;end if
     delete (indx)
  end do
 ;if (debug .and. .not.any(ismissing(dirWg))) then
 ;    print ("dirWr="+dirWr+"  dirWg="+dirWg \
 ;                         +"  spdWg="+spdWg+"  stdWg="+stdWg)
 ;end if

  wTot = sum(dirWg)                   ; total number of directional counts
                                      ; if overlap wTot >= wNum

  dirFr    = (dirWg/wTot)*100.        ; % freq for each direction wedge
  dirFrMax = max (dirFr)              ; max directioonal frequency [used for plot]

  total = sum(dirFr)                  ; make sure we have 100% total
  if (debug) then
      print ("wNum="+wNum+"   wTot="+wTot+"   total="+total)
  end if
                                      ; Draw the "percentage frequency" circles
  dirFrMaxNumCircles = toint(dirFrMax/circFr +1.)
  dirFrMaxNum        = circFr*dirFrMaxNumCircles   ; number of circles

  nCirc = 361                         ; do this once
  xCirc = new ( (/dirFrMaxNumCircles,nCirc /) , float)
  yCirc = xCirc
  xcos  = cos(fspan(0, 360, nCirc)*rad)*circFr
  xsin  = sin(fspan(0, 360, nCirc)*rad)*circFr

  do n=0,dirFrMaxNumCircles-1         ; plot coordinates for freq circles
     xCirc(n,:) = (n+1)*xcos(:)
     yCirc(n,:) = (n+1)*xsin(:)
  end do
  delete(xcos)
  delete(xsin)
                                      ; Specify data limits for X and Y axes.
  extraSpace        = max((/3.,circFr/3./))    ; Extra space beyond outer circle
  opts@trXMinF      = -dirFrMaxNum-extraSpace  ; min X 
  opts@trXMaxF      =  dirFrMaxNum+extraSpace  ; max X
  opts@trYMinF      = -dirFrMaxNum-extraSpace  ; min Y
  opts@trYMaxF      =  dirFrMaxNum+extraSpace  ; max Y
  opts@tmXTOn       = False                    ; turn off tick marks on each side
  opts@tmXBOn       = False
  opts@tmYLOn       = False
  opts@tmYROn       = False
  opts@tmXBBorderOn = False                    ; turn off borders on each side
  opts@tmXTBorderOn = False
  opts@tmYLBorderOn = False
  opts@tmYRBorderOn = False
  opts@gsnFrame     = False                    ; do not advance frame
  opts@gsnDraw      = False                    ; do not draw
  opts@xyMonoDashPattern = True                ; set all circles to solid

  plotWr = gsn_xy(wks,xCirc,yCirc,opts) ; GSUN routine to draw circles 

  wAveStr = new ( 1, string)            ; pre-define variable of type string
  txRes = True                          ; set text resources
  txRes@txFontHeightF     = 0.0100
  txRes@txAngleF          = -60.
  gsRes = True                          ; set polyline resources
  gsRes@gsLineThicknessF  = 2.0         ; Extra thick lines

  do n=0,dirFrMaxNumCircles-1           ; plot coordinates for %freq
     xP = 0.5 + (n+1)*circFr*cos((dirWr(0)+wedge2)*rad)
     yP = 0.5 + (n+1)*circFr*sin((dirWr(0)+wedge2)*rad)
     wAveStr = toint((n+1)*circFr)+"%"  
     astring = systemfunc("echo text1$$")
     plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr,xP,yP,txRes) ; add text at end
  end do
  delete(txRes@txAngleF)
  txRes@txFontHeightF     = 0.0140

  nB = dimsizes(spdBounds)
  nW = nB+1
  spdWork = new (nW, float )
  spdWork(0) = 0.
  spdWork(1:) = spdBounds
                                        ; draw petals (or spokes .. whatever)
  cext = 1.00                           ; good for numPetals=8 
  minWindPrintF = 0.0
  txRes@txFontHeightF     = 0.0140

  if (numPetals.ge.12 .and. numPetals.le.16) then
      cext = 0.75 
      txRes@txFontHeightF = 0.0125
  end if
  if (numPetals.gt.16 .and. numPetals.le.24) then
      cext = 0.50 
      txRes@txFontHeightF = 0.0100
  end if
  if (numPetals.gt.24) then
      cext = 0.25 
      txRes@txFontHeightF = 0.0100
  end if

  if (isatt(wrRes,"petalLabExtF")) then
      cext = wrRes@petalLabExtF
  end if
  if (isatt(wrRes,"txFontHeightF")) then
      txRes@txFontHeightF = wrRes@txFontHeightF
  end if
  if (isatt(wrRes,"minWindPrintF")) then
      minWindPrintF = wrRes@minWindPrintF
  end if

  do n=0,nP-1                           ; loop thru each petal (wedge)
     xP    = 0.
     yP    = 0.
     xPrev = 0.
     yPrev = 0.
     gsRes@gsLineThicknessF  = 2.0         ; Extra thick lines
    ;if (debug) then
        ;print (" ")
        ;print ("n="+n+"  dirWr(n)="+dirWr(n)+"  dirFr(n)="+dirFr(n) \
    ;end if             +"  dirWg(n)="+dirWg(n)+"  wTot="+wTot )
   do m=0,nW-2
     if (n.eq.0) then                 ; n=0 SPECIAl due to 360 <=> 0 crossover
         wknt = num(wspd.gt.0. .and. \;no calm
                   (wdir.ge.wedgeL(nP)   .or. wdir.le.wedgeR(0)) .and.\
                   (wspd.ge.spdWork(m) .and. wspd.lt.spdWork(m+1)))
         indx = ind(wspd.gt.0. .and. \
                   (wdir.ge.wedgeL(nP) .or. wdir.le.wedgeR(0)) .and.\
                   (wspd.ge.spdWork(m) .and. wspd.lt.spdWork(m+1)))
     else
         wknt = num(wspd.gt.0. .and. \
                   (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)) .and.\
                   (wspd.ge.spdWork(m) .and. wspd.lt.spdWork(m+1)))
         indx = ind(wspd.gt.0. .and. \
                   (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)) .and.\
                   (wspd.ge.spdWork(m) .and. wspd.lt.spdWork(m+1)))
     end if

     if (wknt.gt.0.) then           
         dirPc   = (wknt/wTot)*100.
         xP      = xP + dirPc*sin(dirWr(n)*rad)         ; math coords
         yP      = yP + dirPc*cos(dirWr(n)*rad)
         gsRes@gsLineColor  = colorBounds(m)
         astring = systemfunc("echo line1$$")
         plotWr@$astring$ = gsn_add_polyline(wks,plotWr,(/xPrev,xP/),(/yPrev,yP/),gsRes) ; draw spoke
        ;if (debug) then
            ;print ("  wspd(indx)="+wspd(indx))
            ;print ("  wknt="+wknt+"  dirPc="+dirPc+"  xP="+xP+"  yP="+yP\
            ;      +"  spdWork(m)="+spdWork(m)+"=>"+spdWork(m+1)  )
        ;end if
                  
         xPrev = xP
         yPrev = yP
     end if
     wknt = 0
     delete (indx)
     gsRes@gsLineThicknessF  = gsRes@gsLineThicknessF  + 3.0 
   end do
                                                      ; write mean speed 
     if (dirWg(n).gt.0.)
         xPtx    = xP +cext*sin(dirWr(n)*rad)          
         yPtx    = yP +cext*cos(dirWr(n)*rad)
         if ((spdWg(n)+0.5) .gt. minWindPrintF) then
             wAveStr = toint(spdWg(n)+0.5)       ; round mean speed
             astring = systemfunc("echo text2$$")
             plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr,xPtx,yPtx,txRes) ; add text at end
         end if
     end if
  end do
                                                      ; Manually anotate plot
  wAveStr = "SpdAve="+toint(wAve+0.5)+"  "+ \
            "SpdStd="+toint(wStd+0.5)+"  "+ \
            "DirAve="+toint(wDir+0.5)
  if (nCalm.gt.0) then
      calmFr = (nCalm/wNum)*100.                      ; % freq Calm
      wAveStr = wAveStr +"   Calm="+sprintf("%4.1f", calmFr)+"%"
  else
      wAveStr = wAveStr +"   No Calm Reports"
  end if

  wAveStr = wAveStr +"  Nwnd="+wNum

  txRes@txJust = "CenterCenter"
  astring = systemfunc("echo text3$$")
  plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr, 0.0,dirFrMaxNum+extraSpace,txRes)

  wAveStr = "Frequency circles every "+circFr+"%. Mean speed indicated."  
  astring = systemfunc("echo text4$$")
 ;plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr, 0.0,-dirFrMaxNum-extraSpace,txRes)
  txRes@txFontHeightF     = 0.70*txRes@txFontHeightF
  plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr, 0.0,dirFrMaxNum+0.4*extraSpace,txRes)

  if (isatt(wrRes,"gsnDraw")) then 
      if (wrRes@gsnDraw) then 
          draw(plotWr)
      end if
  else 
      draw(plotWr)
  end if
  if (isatt(wrRes,"gsnFrame")) then 
      if (wrRes@gsnFrame) then 
          frame (wks)
      end if
  else 
      frame (wks)
  end if
  return (plotWr)

 end
; -------------------------------------------

undef("WindRoseThickLineF")
function WindRoseThickLineF(wks:graphic, wspd[*]:numeric, wdir[*]:numeric \
                          , numPetals[1]:integer, circFr[1]:float \
                          , spdBounds[*]:numeric, wrRes[1]:logical)

; Plot a Wind Rose [ie, frequency of wind direction + avg spd]
; Nomenclature
;      wspd      - wind speed (any units)
;      wdir      - wind direction (degrees)
;                  meteorological: dir from which the wind blows
;                              eg: 90 deg means blowing from the east
;      numPetals - number wind wind rose directions 
;      circFr    - intervals at which frequency circles are drawn
;                  eg: circFr=10. is common
;      spdBounds - bounds for different Thicknesses
;                  eg: spdBounds = (/ 10., 20., 30., 100. /)
;                     [0+ ==>10; 10+ ==>20; 20+ ==> 30; 30+ ==>100]
;      wrRes     - resources which affect plot appearance    
;
; Note1:
; It is best to set "numPetals" to a number like 8  or 16.
; [Eg, if numPetals=4 then a direction of 45 degrees could be
; counted as a north or east wind (or both) depending on the
; if statement below. Actually, the way I have implemented
; it, it would be counted as BOTH a north and an East wind.]
;
; Note2: best to use the wks as post script

 begin
  debug= False
 
  opts = True           ; local and default options
  if (wrRes) then
      opts = wrRes      ; if True over ride local/defaults with input

                        ; avoid warning messages and delete the following 'wr' (wind rose) attributes 
                        ; use them explicitly later
      if (isatt(wrRes,"petalLabExtF")) then
         delete(opts@petalLabExtF)
      end if
      if (isatt(wrRes,"txFontHeightF")) then
          delete(opts@txFontHeightF)
      end if
      if (isatt(wrRes,"minWindPrintF")) then
          delete(opts@minWindPrintF)
      end if

  end if

  nP     = numPetals            ; for convenience only
  nW     = dimsizes(wspd)       ; total number of elements
  rad    = 4.*atan(1.0)/180.    ; degress to radians
                                ; compute wind components
  u      = -wspd*sin(wdir*rad)  ; u component (zonal) 
  v      = -wspd*cos(wdir*rad)  ; v component (meridional)
  uAve   = avg(u)
  vAve   = avg(v)
  wAve   = avg(wspd)                  ; average wind speed 
  wStd   = stddev(wspd)               ; st. dev of wind speed
  wDir   = atan2(uAve,vAve)/rad +180. ; mean wind direction [0,360]
  wNum   = num( .not.ismissing(wspd) ); total number of non-msg values
  nCalm  = num( wspd.eq.0.)*1.0       ; total calm reports (float) 

  wedge  = 360./nP                    ; number of directional wedges
  wedge2 = wedge/2.
                                      ; extra wedge [see below]
  wedgeL = fspan (-wedge2, 360.-wedge2, nP+1) ; "left"  edges
  wedgeR = fspan ( wedge2, 360.+wedge2, nP+1) ; "right" edges

  dirWr  = fspan (0., 360.-wedge, nP) ; central petals of the wind rose

  if (debug) then
      print ("wedgeL="+wedgeL+"  wedgeR="+wedgeR)  
      print ("dirWr="+dirWr)
  end if
                                      ; perform the counting and calculations
  dirWg = new (nP,float)              ; direction counts in each wedge (Wg) 
  spdWg = new (nP,float)              ; overall mean   speed in each wedge (Wg)
  stdWg = new (nP,float)              ; overall stddev speed in each wedge (Wg)
  do n=0,nP-1                         ; loop thru each wedge [do not include calm]
     if (n.eq.0) then                 ; n=0 SPECIAl due to 360 <=> 0 crossover
         dirWg(n) = num(wspd.gt.0. .and. (wdir.ge.wedgeL(nP) .or. wdir.le.wedgeR(0)))
         indx     = ind(wspd.gt.0. .and. (wdir.ge.wedgeL(nP) .or. wdir.le.wedgeR(0)))
     else
         dirWg(n) = num(wspd.gt.0. .and. (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)))
         indx     = ind(wspd.gt.0. .and. (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)))
     end if

     if (dirWg(n).gt.0.) then         ; calculate ave spd and stddev per wedge
         spdWg(n) = avg(wspd(indx))
         stdWg(n) = stddev(wspd(indx))
     end if

    ;if (debug .and. .not.any(ismissing(indx))) then
    ;    print("indx="+indx+"  wdir(indx)="+wdir(indx) \
    ;                      +"  wspd(indx)="+wspd(indx))
    ;end if
     delete (indx)
  end do
  if (debug .and. .not.any(ismissing(dirWg))) then
      print ("dirWr="+dirWr+"  dirWg="+dirWg \
                           +"  spdWg="+spdWg+"  stdWg="+stdWg)
  end if

  wTot = sum(dirWg)                   ; total number of directional counts
                                      ; if overlap wTot >= wNum

  dirFr    = (dirWg/wTot)*100.        ; % freq for each direction wedge
  dirFrMax = max (dirFr)              ; max directioonal frequency [used for plot]

  total = sum(dirFr)                  ; make sure we have 100% total
  if (debug) then
      print ("wNum="+wNum+"   wTot="+wTot+"   total="+total)
  end if
                                      ; Draw the "percentage frequency" circles
  dirFrMaxNumCircles = floattointeger(dirFrMax/circFr +1.)
  dirFrMaxNum        = circFr*dirFrMaxNumCircles   ; number of circles

  nCirc = 361                         ; do this once
  xCirc = new ( (/dirFrMaxNumCircles,nCirc /) , float)
  yCirc = xCirc
  xcos  = cos(fspan(0, 360, nCirc)*rad)*circFr
  xsin  = sin(fspan(0, 360, nCirc)*rad)*circFr

  do n=0,dirFrMaxNumCircles-1         ; plot coordinates for freq circles
     xCirc(n,:) = (n+1)*xcos(:)
     yCirc(n,:) = (n+1)*xsin(:)
  end do
  delete(xcos)
  delete(xsin)
                                      ; Specify data limits for X and Y axes.
  extraSpace        = max((/3.,circFr/3./))    ; Extra space beyond outer circle
  opts@trXMinF      = -dirFrMaxNum-1.0*extraSpace  ; min X 
  opts@trXMaxF      =  dirFrMaxNum+1.0*extraSpace  ; max X
  opts@trYMinF      = -dirFrMaxNum-1.0*extraSpace  ; min Y
  opts@trYMaxF      =  dirFrMaxNum+1.0*extraSpace  ; max Y
  opts@tmXTOn       = False                    ; turn off tick marks on each side
  opts@tmXBOn       = False
  opts@tmYLOn       = False
  opts@tmYROn       = False
  opts@tmXBBorderOn = False                    ; turn off borders on each side
  opts@tmXTBorderOn = False
  opts@tmYLBorderOn = False
  opts@tmYRBorderOn = False
  opts@gsnFrame     = False                    ; do not advance frame
  opts@gsnDraw      = False                    ; do not draw
  opts@xyMonoDashPattern = True                ; set all circles to solid
 
  plotWr = gsn_xy(wks,xCirc,yCirc,opts) ; GSUN routine to draw circles 

  wAveStr = new ( 1, string)            ; pre-define variable of type string
  txRes = True                          ; set text resources
  txRes@txFontHeightF     = 0.0100
  txRes@txAngleF          = -60.
  gsRes = True                          ; set polyline resources
  gsRes@gsLineThicknessF  = 2.0         ; Extra thick lines

  do n=0,dirFrMaxNumCircles-1           ; plot coordinates for %freq
     xP = 0.5 + (n+1)*circFr*cos((dirWr(0)+wedge2)*rad)
     yP = 0.5 + (n+1)*circFr*sin((dirWr(0)+wedge2)*rad)
     wAveStr = floattointeger((n+1)*circFr)+"%"  
     astring = systemfunc("echo text1$$")
     plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr,xP,yP,txRes) ; add text at end
  end do
  delete(txRes@txAngleF)
  txRes@txFontHeightF     = 0.0140

  nB = dimsizes(spdBounds)
  nW = nB+1
  spdWork = new (nW, float )
  spdWork(0) = 0.
  spdWork(1:) = spdBounds

                                        ; draw petals (or spokes .. whatever)
  cext          = 1.00                           ; good for numPetals=8 
  minWindPrintF = 0.0
  txRes@txFontHeightF     = 0.0140
  if (numPetals.ge.12 .and. numPetals.le.16) then
      cext = 0.75 
      txRes@txFontHeightF = 0.0125
  end if
  if (numPetals.gt.16 .and. numPetals.le.24) then
      cext = 0.50 
      txRes@txFontHeightF = 0.0100
  end if
  if (numPetals.gt.24) then
      cext = 0.25 
      txRes@txFontHeightF = 0.0100
  end if

  if (isatt(wrRes,"petalLabExtF")) then
      cext = wrRes@petalLabExtF
  end if
  if (isatt(wrRes,"txFontHeightF")) then
      txRes@txFontHeightF = wrRes@txFontHeightF
  end if
  if (isatt(wrRes,"minWindPrintF")) then
      minWindPrintF = wrRes@minWindPrintF
  end if
                                        ; draw petals (or spokes .. whatever)
  do n=0,nP-1                           ; loop thru each petal (wedge)
     xP    = 0.
     yP    = 0.
     xPrev = 0.
     yPrev = 0.
     gsRes@gsLineThicknessF  = 2.0         ; Extra thick lines
    ;if (debug) then
        ;print (" ")
        ;print ("n="+n+"  dirWr(n)="+dirWr(n)+"  dirFr(n)="+dirFr(n) \
    ;end if             +"  dirWg(n)="+dirWg(n)+"  wTot="+wTot )
   do m=0,nW-2
     if (n.eq.0) then                 ; n=0 SPECIAl due to 360 <=> 0 crossover
         wknt = num(wspd.gt.0. .and. \;no calm
                   (wdir.ge.wedgeL(nP)   .or. wdir.le.wedgeR(0)) .and.\
                   (wspd.ge.spdWork(m) .and. wspd.lt.spdWork(m+1)))
         indx = ind(wspd.gt.0. .and. \
                   (wdir.ge.wedgeL(nP) .or. wdir.le.wedgeR(0)) .and.\
                   (wspd.ge.spdWork(m) .and. wspd.lt.spdWork(m+1)))
     else
         wknt = num(wspd.gt.0. .and. \
                   (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)) .and.\
                   (wspd.ge.spdWork(m) .and. wspd.lt.spdWork(m+1)))
         indx = ind(wspd.gt.0. .and. \
                   (wdir.ge.wedgeL(n) .and. wdir.le.wedgeR(n)) .and.\
                   (wspd.ge.spdWork(m) .and. wspd.lt.spdWork(m+1)))
     end if
     if (wknt.gt.0.) then           
         dirPc   = (wknt/wTot)*100.
         xP      = xP + dirPc*sin(dirWr(n)*rad)         ; math coords
         yP      = yP + dirPc*cos(dirWr(n)*rad)
         astring = systemfunc("echo line1$$")
         plotWr@$astring$ = gsn_add_polyline(wks,plotWr,(/xPrev,xP/),(/yPrev,yP/),gsRes) ; draw spoke
        ;if (debug) then
            ;print ("  wspd(indx)="+wspd(indx))
            ;print ("  wknt="+wknt+"  dirPc="+dirPc+"  xP="+xP+"  yP="+yP\
            ;      +"  spdWork(m)="+spdWork(m)+"=>"+spdWork(m+1)  )
        ;end if
                  
         xPrev = xP
         yPrev = yP
     end if
     wknt = 0
     delete (indx)
     gsRes@gsLineThicknessF  = gsRes@gsLineThicknessF  + 3.0  
   end do
                                                      ; write mean speed 
     if (dirWg(n).gt.0.)
         xPtx    = xP + cext*sin(dirWr(n)*rad)      
         yPtx    = yP + cext*cos(dirWr(n)*rad)
         if ((spdWg(n)+0.5) .gt. minWindPrintF) then
             wAveStr = floattointeger(spdWg(n)+0.5)       ; round mean speed
             astring = systemfunc("echo text2$$")
             plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr,xPtx,yPtx,txRes) ; add text at end
         end if
     end if
  end do
                                                      ; Manually anotate plot
  wAveStr = "SpdAve="+floattointeger(wAve+0.5)+"  "+ \
            "SpdStd="+floattointeger(wStd+0.5)+"  "+ \
            "DirAve="+floattointeger(wDir+0.5)
  if (nCalm.gt.0.) then
      calmFr = (nCalm/wNum)*100.                      ; % freq Calm
      wAveStr = wAveStr +"   Calm="+sprintf("%4.1f", calmFr)+"%"
  else
      wAveStr = wAveStr +"   No Calm Reports"
  end if

  wAveStr = wAveStr +"  Nwnd="+wNum

  txRes@txJust = "CenterCenter"
  astring = systemfunc("echo text3$$")
  plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr, 0.0 ,dirFrMaxNum+extraSpace,txRes)

  wAveStr = "Frequency circles every "+circFr+"%. Mean speed indicated."  
  astring = systemfunc("echo text4$$")
 ;plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr, 0.0,-dirFrMaxNum-extraSpace,txRes)
  txRes@txFontHeightF     = 0.70*txRes@txFontHeightF
  plotWr@$astring$ = gsn_add_text(wks,plotWr,wAveStr, 0.0,dirFrMaxNum+0.4*extraSpace,txRes)

  if (isatt(wrRes,"gsnDraw")) then 
      if (wrRes@gsnDraw) then 
          draw(plotWr)
      end if
  else 
      draw(plotWr)
  end if
  if (isatt(wrRes,"gsnFrame")) then 
      if (wrRes@gsnFrame) then 
          frame (wks)
      end if
  else 
      frame (wks)
  end if
  return (plotWr)

 end
;---------------------------------------
undef("WindRoseBasic")
function WindRoseBasic (wks:graphic, wspd:numeric, wdir:numeric \
                                   , numPetals[1]:integer, circFr[1]:numeric, wrRes[1]:logical)
; Error check interface; If needed, force one-dimensional 'wspd' and 'wdir' arrays 
local dims, ranks, dimd, rankd, WSPD, WDIR
begin
  dims  = dimsizes(wspd)
  ranks = dimsizes(dims)
  dimd  = dimsizes(wdir)
  rankd = dimsizes(dimd)

  if (ranks.ne.rankd) then
      print("WindRoseBasic: wspd, wdir must be same size and shape")
      print("               rank(wspd)="+ranks+"  rank(wdir)="+rankd)
      exit
  end if

  if (dims(0).ne.dimd(0)) then
      print("WindRoseBasic: wspd, wdir must be same size")
      print("               dimsizes(wspd)="+dims(0)+"  dimsizes(wdir)="+dimd(0))
      exit
  end if

  if (ranks.eq.1) then
      return( WindRoseBasicF(wks, wspd, wdir, numPetals, circFr, wrRes) )
  else
      WSPD = ndtooned( wspd )
      WDIR = ndtooned( wdir )
      if (isatt(wspd, "units")) then
          WSPD@units = wspd@units
      end if
      return( WindRoseBasicF(wks, WSPD, WDIR, numPetals, circFr, wrRes) )
  end if
end
; ---------------------------------------------------
undef("WindRoseColor")
function WindRoseColor (wks:graphic, wspd:numeric, wdir:numeric \
                                   , numPetals[1]:integer, circFr[1]:numeric \
                                   , spdBounds[*]:numeric, colorBounds[*]:string, wrRes[1]:logical)
; Error check interface; If needed, force one-dimensional 'wspd' and 'wdir' arrays 
local dims, ranks, dimd, rankd, WSPD, WDIR
begin
  dims  = dimsizes(wspd)
  ranks = dimsizes(dims)
  dimd  = dimsizes(wdir)
  rankd = dimsizes(dimd)

  if (ranks.ne.rankd) then
      print("WindRoseColor: wspd, wdir must be same size and shape")
      print("               rank(wspd)="+ranks+"  rank(wdir)="+rankd)
      exit
  end if

  if (dims(0).ne.dimd(0)) then
      print("WindRoseColor: wspd, wdir must be same size")
      print("               dimsizes(wspd)="+dims(0)+"  dimsizes(wdir)="+dimd(0))
      exit
  end if

  if (ranks.eq.1) then
      return(WindRoseColorF(wks, wspd, wdir, numPetals, circFr, spdBounds, colorBounds, wrRes) )
  else
      WSPD = ndtooned( wspd )
      WDIR = ndtooned( wdir )
      if (isatt(wspd, "units")) then
          WSPD@units = wspd@units
      end if
      return(WindRoseColorF(wks, WSPD, WDIR, numPetals, circFr, spdBounds, colorBounds, wrRes) )
  end if
end
;---------------------------------------
undef("WindRoseThickLine")
function WindRoseThickLine (wks:graphic, wspd:numeric, wdir:numeric \
                           , numPetals[1]:integer, circFr[1]:numeric \
                           , spdBounds[*]:numeric, wrRes[1]:logical)
; Error check interface; If needed, force one-dimensional 'wspd' and 'wdir' arrays 
local dims, ranks, dimd, rankd, WSPD, WDIR
begin
  dims  = dimsizes(wspd)
  ranks = dimsizes(dims)
  dimd  = dimsizes(wdir)
  rankd = dimsizes(dimd)

  if (ranks.ne.rankd) then
      print("WindRoseThickLine: wspd, wdir must be same size and shape")
      print("                   rank(wspd)="+ranks+"  rank(wdir)="+rankd)
      exit
  end if

  if (dims(0).ne.dimd(0)) then
      print("WindRoseThickLine: wspd, wdir must be same size")
      print("                   dimsizes(wspd)="+dims(0)+"  dimsizes(wdir)="+dimd(0))
      exit
  end if

  if (ranks.eq.1) then
      return(WindRoseThickLineF(wks, wspd, wdir, numPetals, circFr, spdBounds, wrRes) )
  else
      WSPD = ndtooned( wspd )
      WDIR = ndtooned( wdir )
      if (isatt(wspd, "units")) then
          WSPD@units = wspd@units
      end if
      return(WindRoseThickLineF(wks, WSPD, WDIR, numPetals, circFr, spdBounds, wrRes) )
  end if
end

;------------ following for demo scripts only!!!--------
;        THIS IS ***NOT*** A GENERAL FUNCTION!!!!
;           DEVELOPED TO FACILITATE EXAMPLES

undef("wr_GenBogusData")
function wr_GenBogusData (nMax:integer) 
begin

  wrData = new ( (/2,nMax/) , "float")
  if (nMax.ne.200) then
      print ("wr_GenBogusData: nMax must be 200")
      return (wrData)
  end if

  wspd  = new (nMax, float)
  wdir  = new (nMax, float)
  wspd@long_name = "Wind Speed"
  wspd@units     = "m/s"
  wdir@long_name = "Wind Direction"
                                  ; BOGUS Winds
  wspdx = fspan (  0.,  35., 100) ; equally spaced
  wdirx = fspan (  0., 359., 100) ;   "       "
  wspdy = fspan (  5.,  15.,  50) ; light winds
  wdiry = fspan ( 45., 175.,  50) ; mainly NE ==> S
  wspdz = fspan ( 30.,  45.,  50) ; strong winds
  wdirz = fspan (240., 280.,  50) ; WSW ==> W

  wspd(  0: 99) = wspdx
  wspd(100:149) = wspdy
  wspd(150:199) = wspdz
  wdir(  0: 99) = wdirx
  wdir(100:149) = wdiry
  wdir(150:199) = wdirz

  wrData = new ( (/2,nMax/) , "float")
  wrData(0,:) = wspd
  wrData(1,:) = wdir
  return (wrData)
end
;------------------------
undef("wr_GenBogusDataRandom")
function wr_GenBogusDataRandom (nMax:integer) 
begin

  wspd  = random_uniform(18,    42,nMax)
  wdir  = random_uniform( 0,359.25,nMax)

  wspd@long_name = "Wind Speed"
  wspd@units     = "m/s"
  wdir@long_name = "Wind Direction"

  wrData = new ( (/2,nMax/) , "float")
  wrData(0,:) = (/ wspd /)
  wrData(1,:) = (/ wdir /)
  return (wrData)
end
