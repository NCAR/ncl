;---Load the NCL file that contains the gsn_* functions used below.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin
  x = new(9,float)  ; Define two 1D arrays of 9 elements each.
  y = new(9,float)

  x = (/10.,20.,30.,40.,50.,60.,70.,80.,90./)
  y = (/0.,0.71,1.,0.7,0.002,-0.71,-1.,-0.71,-0.003/)

  wks = gsn_open_wks("x11","gsun01n")  ; Open an X11 workstation.

  plot = gsn_xy(wks,x,y,False)         ; Draw an XY plot with 1 curve.

;----------- Begin second plot -----------------------------------------

   y2 = (/(/0., 0.7, 1., 0.7, 0., -0.7, -1., -0.7, 0./),\
         (/2., 2.7, 3., 2.7, 2.,  1.3,  1.,  1.3, 2./),\
         (/4., 4.7, 5., 4.7, 4.,  3.3,  3.,  3.3, 4./)/)

  x@long_name  = "X"             ; Define attributes of x
  y2@long_name = "Y"             ; and y2.

  plot = gsn_xy(wks,x,y2,False)  ; Draw an XY plot with 3 curves.

;---------- Begin third plot -----------------------------------------

  resources                    = True          ; Indicate you want to
                                               ; set some resources.

  resources@xyLineColors        = (/2,3,4/)    ; Define line colors.
  resources@xyLineThicknesses   = (/1.,2.,5./) ; Define line thicknesses 
                                               ; (1.0 is the default).

  plot = gsn_xy(wks,x,y2,resources)            ; Draw an XY plot.

;---------- Begin fourth plot ------------------------------------------
 
  resources@tiMainString    = "X-Y plot"  ; Title for the XY plot
  resources@tiXAxisString   = "X Axis"    ; Label for the X axis
  resources@tiYAxisString   = "Y Axis"    ; Label for the Y axis
  resources@tiMainFont      = "Helvetica" ; Font for title
  resources@tiXAxisFont     = "Helvetica" ; Font for X axis label
  resources@tiYAxisFont     = "Helvetica" ; Font for Y axis label

  resources@xyMarkLineModes = (/"Lines","Markers","MarkLines"/)
  resources@xyMarkers       = (/0,1,3/)   ; (none, dot, asterisk)
  resources@xyMarkerColor   = 3           ; Marker color
  resources@xyMarkerSizeF   = 0.03        ; Marker size (default 
                                          ; is 0.01)

  plot = gsn_xy(wks,x,y2,resources)       ; Draw an XY plot.

;---------- Begin fifth plot ------------------------------------------
 
  filename = "$NCARG_ROOT/lib/ncarg/data/asc/xy.asc"

  data = asciiread(filename,(/129,4/),"float")

  uv      = new((/2,129/),float)
  uv(0,:) = data(:,1)
  uv(1,:) = data(:,2)

  lon     = data(:,0)
  lon     = (lon-1) * 360./128.

  delete(resources) ; Start with new list of resources.

  resources                        = True

  resources@tiMainString           = "U/V components of wind"
  resources@tiXAxisString          = "longitude"
  resources@tiYAxisString          = "m/s"
  resources@tiXAxisFontHeightF     = 0.02        ; Change the font size.
  resources@tiYAxisFontHeightF     = 0.02

  resources@xyLineColors           = (/3,4/)     ; Set the line colors.
  resources@xyLineThicknessF       = 2.0         ; Double the width.

  resources@xyLabelMode            = "Custom"    ; Label XY curves.
  resources@xyExplicitLabels       = (/"U","V"/) ; Labels for curves
  resources@xyLineLabelFontHeightF = 0.02        ; Font size and color
  resources@xyLineLabelFontColor   = 2           ; for line labels

  plot = gsn_xy(wks,lon,uv,resources) ; Draw an XY plot with 2 curves.

  delete(plot)       ; Clean up.
  delete(resources)
end
