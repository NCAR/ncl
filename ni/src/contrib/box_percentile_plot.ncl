;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; box_percentile_plot
;   Frank Kreienkamp 2015-09-16
;
; based on:
;   cjs_auto_boxplot.ncl
;   function percent_to_value
;     Carl Schreck (carl@cicsnc.org)
;     February 2012
; 
;   boxplot
;     Adam Phillips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: Make a box_percentile_plot after calculating the necessary parameters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function to calculate percentile values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
undef("percent_to_value")
function percent_to_value( \
  i_data           : numeric, \
  i_percentiles[*] : numeric  \
)
local None
begin

  retVal = new( dimsizes(i_percentiles), float )

  data1d = ndtooned( i_data )
  notMissing = data1d( ind(.not.ismissing(data1d) ) )

  qsort(notMissing)

  do p = 0, dimsizes(i_percentiles)-1
;   pInd = round( i_percentiles(p) * .01 * dimsizes(notMissing) + 0.5, 3 ) -1
;   pInd = where( pInd.ge.dimsizes(notMissing), dimsizes(notMissing)-1, pInd )
    floatInd = i_percentiles(p) * .01 * dimsizes(notMissing) - 0.5
    floorInd = toint( floor(floatInd) )
    floorInd = where( floorInd.lt.0, 0, floorInd )
    ceilInd = toint( ceil(floatInd) )
    ceilInd = where( ceilInd.ge.dimsizes(notMissing), \
        dimsizes(notMissing)-1, ceilInd )
;   print(pInd + " " + dimsizes(notMissing))
    if( ceilInd.eq.floorInd ) then
      retVal(p) = notMissing(floorInd)
    else
      retVal(p) = notMissing(floorInd) * ( ceilInd - floatInd ) \
          + notMissing(ceilInd) * ( floatInd - floorInd )
    end if
  end do

  return(retVal)
end ; percent_to_value




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ploting Box-Plot-Core staff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ----------------------------------------------------------
; Adam Phillips
;
; changes towards a core functionality
;    Frank Kreienkamp 2015-08-26 
;
;
;    This function creates a blank linLog plot object, on which box plots are created by extensive
;    use of gsn_add_polyline. The user can draw as many or as few box plots as they wish.
;    The plot is not drawn, and the frame is not advanced. May be used with gsn_panel. Missing data
; may be present in the input Y array, but not the input X array.
;    The function options are as follows:
;
;    wks     ----- The workstation
;    x[*]    ----- A one-dimensional array containing the X-axis values of where the box plot(s)
;                  will be drawn.
;    y[*][*] ----- A two-dimensional array, where the rightmost dimension contains the box plot
;               reference pt. data. y(n,0)=bottom_value, y(n,1)=bottom_value_of_box,
;               y(n,2)=mid-value_of_box,y(n,3)=top_value_of_box,y(n,4)=top_value
;    boxOpts ----- 2 options attached as attributes may be attached here.
;               boxWidth ---- Scalar or array that contains the widths of the boxes.
;               boxColors ---- Scalar or array that contains the colors that the boxes will be drawn in.
;                           Ex. boxOpts@boxColors =  (/"green","blue"/)
;                        If the number of colors specified does not equal the number of
;                        boxes drawn, only the first color specified will be used.
;    plotres ----- An optional xy-plot resource list. Will override defaults where applicable.
;    lineres ----- An optional resource list pertaining to the lines used to draw the boxes/lines.
;   
;    Example call: plot3 = boxplot(wks,ntime,newyval,opti,res,False)
;               draw(plot3)
;               frame(wks)
;
undef("Boxplot_Core")
function Boxplot_Core (wks:graphic,x[*]:numeric,y[*][*]:numeric,boxOpts:logical,plotres:logical,lineres:logical)
begin
     dimquery = dimsizes(y)
     numbox = dimquery(0)
     boxWidths = new((/numbox/),float)
     if (numbox.ne.dimsizes(x)) then
        print("boxplot: Fatal: X must be one-dimensional and both X and Y must have the same rightmost dimension")
        exit
     end if
     if (any(ismissing(x))) then
        print("boxplot: Fatal: X array cannot contain missing data, exiting")
        exit
     end if

;    Developing x-axis
     xAxis = new(numbox+2,typeof(x))
     xAxis(1:numbox) = x
     if (numbox.ne.1) then
        dx = x(1)-x(0)
        xAxis(0) = x(0)-dx
        xAxis(numbox+1) = x(numbox-1)+dx
     else
        dx = 1
        xAxis(0) = x-dx
        xAxis(2) = x+dx
     end if

     if (boxOpts) then
        if (isatt(boxOpts,"boxWidth")) then
           if (dimsizes(boxOpts@boxWidth).ne.1.and.dimsizes(boxOpts@boxWidth).ne.numbox) then
              print("boxplot: Number of input box widths must either equal 1 or the number of boxes ("+numbox+"). Using first specified box width only.")
              boxWidths(:) = boxOpts@boxWidth(0)
           else
              boxWidths = boxOpts@boxWidth
           end if
        else
           boxWidths(:) = dx*.3
        end if
     else
        boxWidths(:) = dx*.3
     end if

     labarr = new(numbox+2,"string")         ;Prepare actual X-axis labels...
     labarr(0) = ""
     labarr(numbox+1) = ""
     labarr(1:numbox) = xAxis(1:numbox)

; Whether to maximize plot in frame.
     maximize = get_res_value(plotres,"gsnMaximize",False)

     if (plotres) then
;       print("Plot resources detected, accepting")
        fsatts = getvaratts(plotres)
        do ty = 0,dimsizes(fsatts)-1
           if (fsatts(ty).eq."tmXBLabels") then        ;Special section to test for XBLabels
              if (dimsizes(plotres@tmXBLabels).ne.numbox) then
                     print("boxplot: Fatal:Number of XB Labels does not match number of boxes, exiting")
                     exit
                  else
                     labarr(1:numbox) = plotres@$fsatts(ty)$
                  end if
                  xblab = plotres@tmXBLabels
                  delete(plotres@tmXBLabels)      ;Delete so tmXBLabels is not used when all
           end if                       ;atts are assigned below...
        end do
        delete(fsatts)
     end if
     plot = create "plot" logLinPlotClass wks
        "trYMinF" : min(y)-2
        "trYMaxF" : max(y)+2
        "trXMinF" : min(xAxis)
        "trXMaxF" : max(xAxis)
        "pmTickMarkDisplayMode" : "Always"
        "tmXBMode"   : "Explicit"
        "tmXBValues" : xAxis
        "tmXBLabels" : labarr
        "tmYROn"  : False
        "tmXTOn"  : False
        "tmYRBorderOn" : False
        "tmXTBorderOn" : False
        "pmTitleDisplayMode": "Always"                   ; allow titles
        "tiMainOn"     : True
        "tiMainString"   : ""
     end create     
     if (plotres) then
        attsetvalues(plot,plotres)
     end if

 if (maximize) then
    mres = True
    mres@gsnDraw  = False
    mres@gsnFrame = False
    maximize_output(wks,mres)
 end if
     return(plot)
end ; function Boxplot_Core
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; plot-function for Box-Percentile-Plots 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; box_percentile_plot
;   Frank Kreienkamp 2015-08-26
;   Frank Kreienkamp 2015-09-16 - added DNA
;
; based on
;   cjs_auto_boxplot.ncl
;     Carl Schreck (carl@cicsnc.org)
;     February 2012
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
undef("box_percentile_plot")
function box_percentile_plot( \\
  io_wks          : graphic, \\
  i_data          : numeric, \\
  i_res           : logical, \\
  i_boxOpts       : logical, \\
  i_lineRes       : logical, \\
  i_PerRes        : logical, \\
  i_markerRes     : logical, \\
  i_DNA           : logical, \\
  i_NoMember      : logical)
local None
begin

  res = i_res
  res@tmYLMajorOutwardLengthF = 0.020
  res@tmYRMajorOutwardLengthF = 0.020
  res@tmYLMinorOutwardLengthF = 0.010
  res@tmYRMinorOutwardLengthF = 0.010
  minData = min( (/ 0.95, 1.05 /) * min(i_data) )
  maxData = max( (/ 0.95, 1.05 /) * max(i_data) )
  nice = nice_mnmxintvl( minData, maxData, 5, True )
  if( .not.isatt( res, "trYMinF" ) ) then
    res@trYMinF = nice(0)
  end if
  if( .not.isatt( res, "trYMaxF" ) ) then
    res@trYMaxF = nice(1)
  end if

  ; These are some parameters that could be useful to have up top
  dims = dimsizes(i_data)
  ndims = dimsizes(dims)
  ;print(dims)
  if( ndims.eq.1 ) then
    nLines = 1
  else
    nLines = dims(0)
  end if
  boxData = new( (/ nLines, 5 /), float )
  NoneActiveData = new( (/ nLines/), logical )
  NoneActiveData = False
  isInsideFences = new( dims, logical )

  ; calculate the special points
  PerY = new((/nLines,101/),float)
  PerX = new((/101/),float)
  PerX(0) = 0
  PerX(50) = i_boxOpts@boxWidth
  PerX(100) = 0
  do iPer = 1,49
    PerX(iPer) =  iPer/50. * i_boxOpts@boxWidth
    PerX(100-iPer) = PerX(iPer)
  end do ; iPer  
  Avg = new((/nLines/),float)
  
  do iLine = 0, nLines-1
    if( ndims.eq.1 ) then
      currData = i_data( ind( .not.ismissing( i_data ) ) )
    else
      if (all(ismissing(i_data(iLine,:)))) then
        NoneActiveData(iLine) = True
        continue
      else
        currData = i_data( iLine, ind( .not.ismissing( i_data(iLine,:) ) ) )
      end if
    end if
    qsort(currData)
    boxData(iLine,0) = min(currData)
    boxData(iLine,1) = percent_to_value( currData, 25 )
    boxData(iLine,2) = percent_to_value( currData, 50 )
    boxData(iLine,3) = percent_to_value( currData, 75 )
    boxData(iLine,4) = max(currData)
    PerY(iLine,  0) = min(currData)
    PerY(iLine,100) = max(currData)
    Avg(iLine)      = avg(currData)
    do iPer = 1,99
      PerY(iLine,iPer) =  percent_to_value( currData, iPer )
    end do ; iPer
    iqr = boxData(iLine,3) - boxData(iLine,1)
    lowerFence = boxData(iLine,1) - ( 1.5 * iqr )
    upperFence = boxData(iLine,3) + ( 1.5 * iqr )
    if( ndims.eq.1 ) then
      isInsideFences = ( i_data.ge.lowerFence ).and.( i_data.le.upperFence )
      boxData(iLine,0) = min( i_data( ind( isInsideFences ) ) )
      boxData(iLine,4) = max( i_data( ind( isInsideFences ) ) )
    else
      isInsideFences(iLine,:) = \\
        ( i_data(iLine,:).ge.lowerFence ).and.( i_data(iLine,:).le.upperFence )
      boxData(iLine,0) = min( i_data( iLine, ind( isInsideFences(iLine,:) ) ) )
      boxData(iLine,4) = max( i_data( iLine, ind( isInsideFences(iLine,:) ) ) )
    end if
    delete(currData)
  end do
 
  i_lineRes1 = i_lineRes
  i_lineRes1@gsLineThicknessF = 0.00001
  retVal = Boxplot_Core( io_wks, ispan( 0, nLines-1, 1 ), boxData, i_boxOpts, \\
    res, i_lineRes1 )

  ; plot
    do iLine = 0, nLines-1
      if (NoneActiveData(iLine)) then
	continue
      end if
      PerRes = True
      if( dimsizes(i_PerRes@gsLineColor).gt.1 ) then
	PerRes@gsLineColor = i_PerRes@gsLineColor(iLine)
      else
	PerRes@gsLineColor = i_PerRes@gsLineColor
      end if
      PerRes = True
      if( dimsizes(i_PerRes@gsLineThicknessF).gt.1 ) then
	PerRes@gsLineThicknessF = i_PerRes@gsLineThicknessF(iLine)
      else
	PerRes@gsLineThicknessF = i_PerRes@gsLineThicknessF
      end if    
      if( dimsizes(i_PerRes@gsFillColor).gt.1 ) then
	PerRes@gsFillColor = i_PerRes@gsFillColor(iLine)
      else
	PerRes@gsFillColor = i_PerRes@gsFillColor
      end if
      
      if( dimsizes(i_PerRes@FillBox).gt.1 ) then
	iFill = i_PerRes@FillBox(iLine)
      else
	iFill = i_PerRes@FillBox
      end if    
      
      if (iFill) then
	rOffsetPos = 0. 
	if (i_DNA) then
	  rOffsetPos = -0.2
	end if
	retVal@$unique_string("PerX1")$ = gsn_add_polygon(io_wks, retVal, iLine + rOffsetPos + PerX,PerY(iLine,:),PerRes)
	retVal@$unique_string("PerX2")$ = gsn_add_polygon(io_wks, retVal, iLine + rOffsetPos  - PerX,PerY(iLine,:),PerRes)
      else
	rOffsetPos = 0.
	if (i_DNA) then
	  rOffsetPos = -0.2
	end if
	retVal@$unique_string("PerX1")$ = gsn_add_polyline(io_wks, retVal, iLine + rOffsetPos  + PerX,PerY(iLine,:),PerRes)
	retVal@$unique_string("PerX2")$ = gsn_add_polyline(io_wks, retVal, iLine + rOffsetPos  - PerX,PerY(iLine,:),PerRes)        
      end if
      
      if( dimsizes(i_PerRes@MarkPer).gt.1 ) then
	iMark = i_PerRes@MarkPer(iLine)
      else
	iMark = i_PerRes@MarkPer
      end if     
      if (iMark) then
	if (iFill) then
	  PerRes@gsLineColor = "white"
	  PerRes@gsLineThicknessF = i_PerRes@gsLineThicknessF * 2
	end if
	KoorMarkPer = new((/2,2/),float)
	KoorMarkPer(0,0) = iLine + rOffsetPos  + PerX(25)
	KoorMarkPer(1,0) = iLine + rOffsetPos  - PerX(25)
	KoorMarkPer(:,1) = PerY(iLine,25)
	retVal@$unique_string("MarkPerX1")$ = gsn_add_polyline(io_wks, retVal, KoorMarkPer(:,0),KoorMarkPer(:,1),PerRes)
	
	KoorMarkPer(0,0) = iLine + rOffsetPos + PerX(50)
	KoorMarkPer(1,0) = iLine + rOffsetPos  - PerX(50)
	KoorMarkPer(:,1) = PerY(iLine,50)
	retVal@$unique_string("MarkPerX2")$ = gsn_add_polyline(io_wks, retVal, KoorMarkPer(:,0),KoorMarkPer(:,1),PerRes)
	
	KoorMarkPer(0,0) = iLine + rOffsetPos  + PerX(75)
	KoorMarkPer(1,0) = iLine + rOffsetPos  - PerX(75)
	KoorMarkPer(:,1) = PerY(iLine,75)
	retVal@$unique_string("MarkPerX3")$ = gsn_add_polyline(io_wks, retVal, KoorMarkPer(:,0),KoorMarkPer(:,1),PerRes)  
	end if
	if (iFill) then
	  if( dimsizes(i_PerRes@gsLineColor).gt.1 ) then
	    PerRes@gsLineColor = i_PerRes@gsLineColor(iLine)
	  else
	    PerRes@gsLineColor = i_PerRes@gsLineColor
	  end if
	  PerRes = True
	  if( dimsizes(i_PerRes@gsLineThicknessF).gt.1 ) then
	    PerRes@gsLineThicknessF = i_PerRes@gsLineThicknessF(iLine)
	  else
	    PerRes@gsLineThicknessF = i_PerRes@gsLineThicknessF
	  end if 
	  retVal@$unique_string("PerX1")$ = gsn_add_polyline(io_wks, retVal, iLine + rOffsetPos  + PerX,PerY(iLine,:),PerRes)
	  retVal@$unique_string("PerX2")$ = gsn_add_polyline(io_wks, retVal, iLine + rOffsetPos  - PerX,PerY(iLine,:),PerRes)         
	end if      

      
      if( dimsizes(i_PerRes@Avg).gt.1 ) then
	iAvg = i_PerRes@Avg(iLine)
      else
	iAvg = i_PerRes@Avg
      end if      
      if (iAvg) then
	markerRes = True
	if( dimsizes(i_markerRes@gsMarkerIndex).gt.1 ) then
	  markerRes@gsMarkerIndex = i_markerRes@gsMarkerIndex(iLine)
	else
	  markerRes@gsMarkerIndex = i_markerRes@gsMarkerIndex
	end if
	if( dimsizes(i_markerRes@gsMarkerSizeF).gt.1 ) then
	  markerRes@gsMarkerSizeF = i_markerRes@gsMarkerSizeF(iLine)
	else
	  markerRes@gsMarkerSizeF = i_markerRes@gsMarkerSizeF
	end if
	if( dimsizes(i_markerRes@gsMarkerThicknessF).gt.1 ) then
	  markerRes@gsMarkerThicknessF = i_markerRes@gsMarkerThicknessF(iLine)
	else
	  markerRes@gsMarkerThicknessF = i_markerRes@gsMarkerThicknessF
	end if
	if( dimsizes(i_markerRes@gsMarkerColor).gt.1 ) then
	  markerRes@gsMarkerColor = i_markerRes@gsMarkerColor(iLine)
	else
	  markerRes@gsMarkerColor = i_markerRes@gsMarkerColor
	end if
	markerRes@gsClipOn = False
	retVal@$unique_string("avg")$ = gsn_add_polymarker(io_wks, retVal, iLine + rOffsetPos, Avg(iLine), markerRes )
      end if
      if (i_DNA) then
	if( ndims.eq.1 ) then
	  ;currData = i_data( ind( .not.ismissing( i_data ) ) )
	  currData = i_data 
	else
	  if (all(ismissing(i_data(iLine,:)))) then
	    continue
	  else
	    ;currData = i_data( iLine, ind( .not.ismissing( i_data(iLine,:) ) ) )
	    currData = i_data(iLine,:)
	  end if
	end if    
	markerRes1 = True
      
	
	;markerRes1@gsMarkerIndex = i_DNA@gsMarkerIndex
	;markerRes1@gsMarkerSizeF = i_DNA@gsMarkerSizeF
	;markerRes1@gsLineThicknessF = i_DNA@gsLineThicknessF
	;markerRes1@gsLineColor  = i_DNA@gsLineColor 
	;markerRes1@gsClipOn = False
	dimsX = dimsizes(currData)
	do i = 0, dimsX-1
	  if (ismissing(currData(i))) then
	  continue
	  end if
	  if( dimsizes(i_DNA@gsLineColor).gt.1 ) then
	    markerRes1@gsLineColor = i_DNA@gsLineColor(i)
	  else
	    markerRes1@gsLineColor = i_DNA@gsLineColor
	  end if      
	  if( dimsizes(i_DNA@gsLineThicknessF).gt.1 ) then
	    markerRes1@gsLineThicknessF = i_DNA@gsLineThicknessF(i)
	  else
	    markerRes1@gsLineThicknessF = i_DNA@gsLineThicknessF
	  end if        
	  retVal@$unique_string("dnaX1")$ = gsn_add_polyline(io_wks, retVal, (/iLine + 0.15,iLine + 0.25/),(/currData(i),currData(i)/),markerRes1)
	end do 
	delete(currData)
      end if
      if (i_NoMember) then
	if( ndims.eq.1 ) then
	  currData = i_data 
	else
	  if (all(ismissing(i_data(iLine,:)))) then
	    continue
	  else
	    currData = i_data(iLine,:)
	  end if
	end if  
	dimsX = dimsizes(currData( ind( .not.ismissing( currData ) ) ))  ;dimsizes(currData)
	retVal@$unique_string("NoX1")$ = gsn_add_text(io_wks, retVal,tostring(dimsX),iLine+0.05,i_res@trYMinF + 0.0,i_NoMember)
	delete(currData)
      end if
    end do
  return(retVal)
end ; 





