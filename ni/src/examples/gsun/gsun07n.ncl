load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin
                                     ; Initialize some data.

  xi = (/  0.000, 0.210, 0.360, 0.540, 1.000, 1.500, 1.970, 2.300, \
           2.500, 2.700  /)
  yi = (/  0.000, 2.600, 3.000, 2.500, 0.000,-1.000, 0.000, 0.800, \
           0.920, 0.700  /)

  npts   = 201
  xl     = -1.0
  xr     =  5.0
  xo     = fspan(xl,xr,npts)  ; Create the output X coordinate array.

  period = 3.0
  yo = ftcurvp(xi, yi, period, xo)  ; Interpolate.

  yint = new( (/ npts /), float)    ; Find the integrals
  do i = 0, npts-1                  ; starting from x = 0.
    yint(i) = ftcurvpi(0., xo(i), period, xi, yi)
  end do

  wks = gsn_open_wks("x11","gsun07n") ; Open an X11 workstation.

  cmap = (/"white","black","red","green","blue","yellow"/)

  gsn_define_colormap(wks,cmap)

  txres = True   ; Set up variable for TextItem resources.
  xyres = True   ; Set up variable for XyPlot resources.
  gsres = True   ; Set up variable for GraphicStyle resources.

  xyres@gsnFrame               = False     ; Don't advance the frame.

  xyres@tmXTBorderOn           = False     ; Don't draw top axis.
  xyres@tmXTOn                 = False     ; Don't draw top axis tick marks.
  xyres@tmBorderThicknessF     = 1.0       ; Default thickness is 2.0

  xyres@tmXBLabelFont          = 21        ; Change font and size of 
  xyres@tmXBLabelFontHeightF   = 0.025     ; X axis labels.
  xyres@tmXBMajorLengthF       = 0.015     ; Default is 0.02.
  xyres@tmXBMajorThicknessF    = 1.0       ; Default is 2.
  xyres@tmXBMinorLengthF       = 0.0075    ; Default is 0.01.
  xyres@tmXBMinorPerMajor      = 4         ; # of minor tick marks per major.
  xyres@tmXBMode               = "Manual"  ; Set tick mark mode.
  xyres@tmXBTickStartF         = -1.0
  xyres@tmXBTickEndF           = 5.0
  xyres@tmXBTickSpacingF       = 1.0
  xyres@tmXMajorGridThicknessF = 1.0       ; Default is 2.0

  xyres@tmYLLabelFont          = 21        ; See explanations for X axes
  xyres@tmYLLabelFontHeightF   = 0.025     ; resources.
  xyres@tmYLMajorLengthF       = 0.015
  xyres@tmYLMajorThicknessF    = 1.0
  xyres@tmYLMinorPerMajor      = 0
  xyres@tmYLMode               = "Manual"
  xyres@tmYLTickStartF         = -2.0
  xyres@tmYLTickEndF           = 3.0
  xyres@tmYLTickSpacingF       = 1.0
  xyres@tmYRBorderOn           = False    ; Don't draw right axis.
  xyres@tmYROn                 = False    ; Don't draw right axis tick marks.

  xyres@trXMaxF   = xr  ; Specify data limits for X and Y axes.
  xyres@trXMinF   = xl
  xyres@trYMaxF   =  3.0
  xyres@trYMinF   = -2.0

  xyres@vpHeightF = 0.25 ; Define height, width, and location of plot.
  xyres@vpWidthF  = 0.80
  xyres@vpXF      = 0.13
  xyres@vpYF      = 0.85

  xy = gsn_xy(wks,xo,yo,xyres) ; Plot the interpolated values.

  xyres@xyMarkLineMode = "Markers"  ; Set line mode to "Markers".
  xyres@xyMarkerColor  = "blue"     ; Set markers to blue.
  xyres@xyMarkerSizeF  = 0.025      ; Make markers larger.

  xy = gsn_xy(wks,xi,yi,xyres) ; Plot the original points with markers.

  txres@txFont        = 21                  ; Change the default font.
  txres@txFontHeightF = 0.03                ; Set the font height.
  gsn_text(wks,xy,"Function",1.5,2.5,txres) ; Label the plot.

  xx = (/xl,xr/)   ; Create data for a polyline for marking
  yy = (/0.0,0.0/) ; the Y = 0.0 line in each graph.

  gsres@gsLineColor = "red"            ; Set polyline color to red.
  gsn_polyline(wks,xy,xx,yy,gsres)     ; Draw polyline at Y=0.

;----------- Begin second plot, same frame--------------------------------

  xyres@vpYF             = 0.37    ; Set Y location of plot.

  xyres@trYMinF          = -1.0    ; Set minimum Y axis value.
  xyres@trYMaxF          =  4.0    ; Set maximum Y axis value.

  xyres@tmYLTickStartF   = -1.0    ; Define tick mark spacing
  xyres@tmYLTickSpacingF =  1.0    ; along Y axis.
  xyres@tmYLTickEndF     =  4.0

  xyres@xyMarkLineMode   = "Lines" ; Set line mode to "Lines".

  xy = gsn_xy(wks,xo,yint,xyres)  ; Plot the integrals.

  txres@txFontHeightF = 0.03                              ; Set font height.
  gsn_text(wks,xy,"Integral (from X = 0.)",0.8,3.5,txres) ; Label plot.

  gsn_polyline(wks,xy,xx,yy,gsres) ; Draw polyline at Y=0.

  txres@txFontHeightF = 0.04  ; Change the font height.

  gsn_text_ndc(wks,"Demo for ftcurvp, ftcurvpi",.5,.95,txres) 

  xperiod1 = (/.41,   .2633, .2633, .2633, .2833, .2633, .2833, .2633, .2633/)
  xperiod2 = (/.5166, .6633, .6633, .6633, .6433, .6633, .6433, .6633, .6633/)
  yperiod  = (/.503,  .503,  .523,  .503,  .513,  .503,  .493,  .503,  .483/)

  gsn_polyline_ndc(wks,xperiod1,yperiod,gsres)  ; Draw a period legend.
  gsn_polyline_ndc(wks,xperiod2,yperiod,gsres)  ; between the two plots.

  txres@txFontHeightF = 0.024                ; Set font height.
  gsn_text_ndc(wks,"Period",0.465,0.5,txres) ; Label the period legend.

  frame(wks)        ; Advance the frame.

  delete(xy)        ; Clean up.
  delete(txres)
  delete(gsres)
  delete(xyres)
end
