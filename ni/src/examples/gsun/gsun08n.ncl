load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin

  ascii_filename = "$NCARG_ROOT/lib/ncarg/data/asc/seismic.asc"

  seismic = asciiread(ascii_filename,(/52,3/),"float")

  x = seismic(:,0)  ; Column 1 of file contains X values.
  y = seismic(:,1)  ; Column 2 of file contains Y values.
  z = seismic(:,2)  ; Column 3 of file contains Z values.

  numxout = 20     ; Define output grid for call to "natgrids".
  numyout = 20
  xmin    = min(x)
  ymin    = min(y)
  xmax    = max(x)
  ymax    = max(y)
  xc      = (xmax-xmin)/(numxout-1)
  yc      = (ymax-ymin)/(numyout-1)
  xo      = xmin + ispan(0,numxout-1,1)*xc
  yo      = ymin + ispan(0,numyout-1,1)*yc

  zo = natgrids(x, y, z, xo, yo)  ; Interpolate.

  xo@long_name = "x values"   ; Define some attributes.
  yo@long_name = "y values"
  zo@long_name = "Depth of a subsurface stratum"

  xwks   = gsn_open_wks( "x11","gsun08n") ; Open an X11 workstation.
  cgmwks = gsn_open_wks("ncgm","gsun08n") ; Open an NCGM workstation.
  pswks  = gsn_open_wks(  "ps","gsun08n") ; Open a PS workstation.
  pdfwks = gsn_open_wks( "pdf","gsun08n") ; Open a PDF workstation.

  cmap = (/(/1., 1., 1./), (/0., 0., 0./), (/1., 0., 0./), (/1., 0., .4/), \
           (/1., 0., .8/), (/1., .2, 1./), (/1., .6, 1./), (/.6, .8, 1./), \
           (/.2, .8, 1./), (/.2, .8, .6/), (/.2, .8, 0./), (/.2, .4, .0/), \
           (/.2, .4, .4/), (/.2, .4, .8/), (/.6, .4, .8/), (/.6, .8, .8/), \
           (/.6, .8, .4/), (/1., .6, .8/)/)

  gsn_define_colormap(  xwks,cmap) ; Define a color map for each of
  gsn_define_colormap(cgmwks,cmap) ; the four workstations you
  gsn_define_colormap( pswks,cmap) ; just opened.
  gsn_define_colormap(pdfwks,cmap)

;----------- Begin first plot -----------------------------------------

  resources                       = True
  resources@sfXArray              = xo            ; X axes data points
  resources@sfYArray              = yo            ; Y axes data points

  resources@tiMainString          = zo@long_name  ; Main title
  resources@tiMainFont            = "Times-Bold"
  resources@tiXAxisString         = "x values"    ; X axis label.
  resources@tiYAxisString         = "y values"    ; Y axis label.

  resources@cnFillOn              = True     ; Turn on contour fill.
  resources@cnInfoLabelOn         = False    ; Turn off info label.
  resources@cnLineLabelsOn        = False    ; Turn off line labels.

  resources@lbOrientation         = "Horizontal" ; Draw it horizontally.
  resources@lbPerimOn             = False        ; Turn off perimeter.
  resources@pmLabelBarDisplayMode = "Always"     ; Turn on a label bar.
  resources@pmLabelBarSide        = "Bottom"     ; Change location of 
                                                 ; label bar.
  resources@vpYF = 0.9   ; Change Y location of plot.

  zo!0 = "i"  ; Name the dimensions of "zo".
  zo!1 = "j"

  contour = gsn_contour(xwks,zo(j|:,i|:),resources) ; Draw a contour plot.

;----------- Begin second plot -----------------------------------------

  delete(resources)   ; Start with a new list of resources.

  resources                     = True
  resources@tiMainString        = "~F26~slices"   ; Define a title.

  resources@xyLineColors        = (/2,8,10,14/)   ; Define line colors.
  resources@xyLineThicknessF    = 3.0             ; Define line thickness.

  resources@pmLegendDisplayMode    = "Always"     ; Turn on the drawing
                                                  ; of a legend.
  resources@pmLegendZone           = 0            ; Change the location
  resources@pmLegendOrthogonalPosF = 0.31         ; of the legend
  resources@lgJustification        = "BottomRight"

  resources@pmLegendWidthF         = 0.4          ; Change width and
  resources@pmLegendHeightF        = 0.12         ; height of legend.

  resources@pmLegendSide           = "Top"        ; Change location of 
  resources@lgPerimOn              = False        ; legend and turn off
                                                  ; the perimeter.

  resources@xyExplicitLegendLabels = (/"zo(i,2)","zo(i,4)","zo(i,6)","zo(i,8)"/)

  resources@vpYF      = 0.90  ; Change size and location of plot.
  resources@vpXF      = 0.18
  resources@vpWidthF  = 0.74
  resources@vpHeightF = 0.74

  resources@trYMaxF = 980  ; Set the maximum value for the Y axes.

  xy = gsn_xy(xwks,xo,zo(j|2:8:2,i|:),resources) ; Draw an XY plot.

;----------- Draw to other workstations  ------------------------------

  NhlChangeWorkstation(contour,cgmwks)  ; Change the workstation that the
  NhlChangeWorkstation(xy,cgmwks)       ; contour and XY plot is drawn to.
  draw(contour)                         ; Draw the contour plot to the new
  frame(cgmwks)                         ; workstation and advance the frame.
  draw(xy)                              ; Draw the XY plot to the new
  frame(cgmwks)                         ; workstation and advance the frame.

  NhlChangeWorkstation(xy,pswks)        ; Do the same for the PostScript
  NhlChangeWorkstation(contour,pswks)   ; workstation.
  draw(contour)
  frame(pswks)
  draw(xy)
  frame(pswks)

  NhlChangeWorkstation(xy,pdfwks)       ; And for the PDF workstation...
  NhlChangeWorkstation(contour,pdfwks)
  draw(contour)
  frame(pdfwks)
  draw(xy)
  frame(pdfwks)

  delete(xy)       ; Clean up.
  delete(contour)
end

