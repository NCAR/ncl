load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

begin
  x = new(9,float)  ; Define two 1D arrays of 9 elements each.
  y = new(9,float)

  x = (/10.,20.,30.,40.,50.,60.,70.,80.,90./)
  y = (/0.,0.71,1.,0.7,0.002,-0.71,-1.,-0.71,-0.003/)

  wks = ncl_open_wks("ncgm","gsunex01")  ; Open an X11 workstation.

  plot = ncl_xy(wks,x,y,False)           ; Draw an XY plot.

; Begin second plot.

   y2 = (/(/0., 0.7, 1., 0.7, 0., -0.7, -1., -0.7, 0./),\
         (/2., 2.7, 3., 2.7, 2.,  1.3,  1.,  1.3, 2./),\
         (/4., 4.7, 5., 4.7, 4.,  3.3,  3.,  3.3, 4./)/)

  x@long_name = "X"                            ; Define attributes
  y2@long_name = "Y"                           ; of x and y2.

  plot = ncl_xy(wks,x,y2,False)                ; Draw an XY plot.

; Begin third plot.

  resources                    = True          ; Indicate we want to set
                                               ; some resources.

  resources@xyLineColors        = (/2,3,4/)    ; Define line colors.
  resources@xyLineThicknesses   = (/1.,2.,3./) ; Define line thicknesses. 

  plot = ncl_xy(wks,x,y2,resources)            ; Draw an XY plot.

; Begin fourth plot.
 
  resources@tiMainString    = "X-Y plot"  ; Title for the XY plot.
  resources@tiMainFont      = "helvetica" ; Font for title.
  resources@tiXAxisString   = "X Axis"    ; Label for the X axis.
  resources@tiXAxisFont     = "helvetica" ; Font for X axis.
  resources@tiYAxisString   = "Y Axis"    ; Label for the Y axis.
  resources@tiYAxisFont     = "helvetica" ; Font for Y axis.

  resources@xyMarkerColor   = 5           ; Set marker color.
  resources@xyMarkerSizeF   = 0.02        ; Set marker size (default 
                                          ; is 0.01).
  resources@xyMarkLineModes = (/"lines","markers","marklines"/)

  plot = ncl_xy(wks,x,y2,resources)       ; Draw an XY plot

; Begin fifth plot.
 
  xydata = asciiread("$NCARG_ROOT/lib/ncarg/data/asc/xy.asc",(/129,4/),"float")
  lon = xydata(:,0) * 360./128.

  delete(resources) ; Start with new list of resources.

  resources                        = True
  resources@tiMainString           = "U/V components of wind"
  resources@tiMainFontColor        = 5
  resources@tiXAxisString          = "longitude"
  resources@tiYAxisString          = "m/s"
  resources@tiXAxisFontHeightF     = 0.02
  resources@tiYAxisFontHeightF     = 0.02
  resources@xyLineColors           = (/3,4/)
  resources@xyLineThicknessF       = 2.0
  resources@xyLabelMode            = "custom"
  resources@xyExplicitLabels       = (/"U","V"/)   ; Labels for lines.
  resources@xyLineLabelFontHeightF = 0.02          ; Font size.
  resources@xyLineLabelFontColor   = 5             ; Font color.

  xydata!0 = "lon"      ; Name the first dimension "lon".
  xydata!1 = "yvalues"  ; Name the second dimension "yvalues".

  plot = ncl_xy(wks,lon,xydata(yvalues|1:2,lon|:),resources)
end

