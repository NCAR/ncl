load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

;***********************************************************************;
; gsun05n.ncl : contour and XY plots                                    ;
;                                                                       ;
; This example reads an ASCII file of seismic data, interpolates it to  ;
; a 3D grid, and creates a contour and an XY plot.                      ;
;                                                                       ;
; This example also shows how to write your graphical output to an NCGM ;
; file and a PostScript file.                                           ;
;***********************************************************************;

begin

;***********************************************************************;
; Read seismic data provided by Dave Watson.  The data is in three      ;
; where the first column is x values, the second y values, and the third;
; z values.                                                             ;
;                                                                       ;
; Read in these values (52 of them) with "asciiread" into one 2D array  ;
; that is 52 x 3 and then extract the x, y, and z arrays.               ;
;***********************************************************************;

  ascii_filename = "$NCARG_ROOT/lib/ncarg/data/asc/seismic.asc"
  seismic = asciiread(ascii_filename,(/52,3/),"float")
  x = seismic(:,0)
  y = seismic(:,1)
  z = seismic(:,2)


;***********************************************************************;
; Define an output grid for our interpolation.  Use the "ispan" function;
; to get a range of evenly-spaced values.                               ;
;***********************************************************************;

  numxout = 20
  numyout = 20
  xmin = min(x)
  ymin = min(y)
  xmax = max(x)
  ymax = max(y)
  xc = (xmax-xmin)/(numxout-1)
  yc = (ymax-ymin)/(numyout-1)
  xo = xmin + ispan(0,numxout-1,1)*xc
  yo = ymin + ispan(0,numyout-1,1)*yc

;***********************************************************************;
; Interpolate our random data to a 2D grid using "natgrids", Dave       ;
; Watson's nearest neighbor interpolation. "zo" will be returned as a 2D;
; array dimensioned numxout x numyout.                                  ;
;***********************************************************************;

  zo = natgrids(x, y, z, xo, yo)

  xo@name = "x values"                      ; Name the xo variable
  yo@name = "y values"                      ; Name the yo variable
  zo@name = "depth of a subsurface stratum" ; Name the zo variable

;***********************************************************************;
; We want our graphical output to go to multiple output devices, so     ;
; create them here.                                                     ;
;***********************************************************************;

  xwks   = ncl_open_wks( "x11","gsun05n")
  cgmwks = ncl_open_wks("ncgm","gsun05n")
  pswks  = ncl_open_wks(  "ps","gsun05n")

;***********************************************************************;
; Define a color table with 18 entries. For each output device that we  ;
; want the color table to take effect for, we have to call the          ;
; ncl_define_colormap procedure.                                        ;
;***********************************************************************;

  cmap = (/(/1.,1.,1./),(/0.,0.,0./),(/1.,0.,0./),(/1.,0.,.4/),\
           (/1.,0.,.8/),(/1.,.2,1./),(/1.,.6,1./),(/.6,.8,1./),\
           (/.2,.8,1./),(/.2,.8,.6/),(/.2,.8,0./),(/.2,.4,.0/),\
           (/.2,.4,.4/),(/.2,.4,.8/),(/.6,.4,.8/),(/.6,.8,.8/),\
           (/.6,.8,.4/),(/1.,.6,.8/)/)

  ncl_define_colormap(  xwks,cmap)
  ncl_define_colormap(cgmwks,cmap)
  ncl_define_colormap( pswks,cmap)

;***********************************************************************;
; Set some contour data resources so that our x and y axis values will  ;
; be the values of our output grid that we defined, rather than index   ;
; values (which is the default).                                        ;
;                                                                       ;
; In NCL, contour plot data is called a "scalar field", thus the data   ;
; resources start with "sf" and are documented at the URL:              ;
;                                                                       ;
;  http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/obj/ScalarField.res.html      ;
;                                                                       ;
;***********************************************************************;

  resources                       = True
  resources@sfXArray              = xo
  resources@sfYArray              = yo

  resources@tiMainString          = zo@name  ; Define some title
  resources@tiXAxisString         = xo@name  ; resources.
  resources@tiYAxisString         = yo@name

  resources@cnFillOn              = True     ; Turn on contour fill.
  resources@cnInfoLabelOn         = False    ; Turn off info label.
  resources@cnLineLabelsOn        = False    ; Turn off line labels.

;***********************************************************************;
; Turn on the drawing of a labelbar.  By default, the labelbar is       ;
; vertical and centered at the right side of the plot. In this case, we ;
; want to draw it at the bottom and horizontally. Since the drawing of a;
; labelbar associated with a plot is controlled by the Plot Manager     ;
; (PM), the PM also controls the location of it.  Thus, we use PM       ;
; resources to change the location of the labelbar.                     ;
;***********************************************************************;

  resources@lbOrientation         = "horizontal" ; Draw it horizontally.
  resources@lbPerimOn             = False        ; Turn off perimeter.
  resources@pmLabelBarDisplayMode = "always"     ; Turn on a label bar.
  resources@pmLabelBarSide        = "bottom"     ; Change location of 
                                                 ; labelbar.

;***********************************************************************;
; Drawing a labelbar at the bottom causes our plot to go off the view   ;
; port window, so adjust the plot on the view port by moving it up.     ;
;***********************************************************************;

  resources@vpYF = 0.9

;***********************************************************************;
; Create a contour plot.  Natgrids returns zo(i,j) as the interpolated  ;
; value at the coordinate (x(i),y(j)) for 0 < i < numxout and 0 < j <   ;
; numyout. For the purpose of drawing a contour plot, NCL identifies the;
; fastest varying dimension (the second dimension of a 2D in NCL) with  ;
; the Cartesian X-axis; if it is desired to have the xo values along the;
; Cartesian X-axis, we need to swap the indices before contouring.      ;
;***********************************************************************;

  zo!0 = "x"
  zo!1 = "y"
  contour = ncl_contour(xwks,zo(y|:,x|:),resources)

;***********************************************************************;
; For our next plot, take some slices of the data and generate an XY    ;
; plot with several of these slices.  Since we previously used the      ;
; "resources" variable to define some contour plot resources, we need to;
; start with a new list of resources for our XY plot, and thus should   ;
; delete the variable before we redefine it.                            ;
;                                                                       ;
; XY plot resources start with "xy" and are defined at the URL:         ;
;                                                                       ;
;   http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/obj/XyPlot.res.html          ;
;***********************************************************************;

  delete(resources)
  resources                     = True
  resources@tiMainString        = "slices"        ; Set some titles.
  resources@tiYAxisString       = zo@name
  resources@tiXAxisString       = xo@name

  resources@xyLineColors        = (/2,8,10,14/)   ; Define line colors.
  resources@xyLineThicknesses   = (/3.,3.,3.,3./) ; Define line
                                                  ; thicknesses.

;***********************************************************************;
; Turn on the drawing of a legend.  By default, the legend is centered  ;
; below the plot. In this case, we want to draw it in the upper right   ;
; corner. As with the labelbar above, we use PM resources to control    ;
; where the legend is drawn.                                            ;
;                                                                       ;
; Legend resources start with "lg" and are defined at the URL:          ;
;                                                                       ;
;   http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/obj/Legend.res.html          ;
;                                                                       ;
;***********************************************************************;

  resources@pmLegendDisplayMode    = "always"     ; Turn on the drawing
                                                  ; of a legend.

  resources@pmLegendSide           = "top"        ; Change location of 
                                                  ; legend.
  resources@lgJustification        = "bottomleft" ; Move legend to right.
  resources@lgPerimOn              = False        ; Turn off perimeter.

  resources@xyExplicitLegendLabels = (/"y=2","y=4","y=6","y=8"/)

;***********************************************************************;
; Drawing a legend at the top causes our plot to go off the view port   ;
; window, so adjust the plot on the view port by moving it down.        ;
;***********************************************************************;

  resources@vpYF = 0.75

;***********************************************************************;
; We are taking slices of the data at y=2, 4, 6, and 8, so use NCL's    ;
; array stride capability to do this.                                   ;
;***********************************************************************;

  xy = ncl_xy(xwks,xo,zo(y|2:8:2,x|:),resources)  

;***********************************************************************;
; We want to draw the plots to the other workstations that we opened    ;
; (an NCGM file and a PostScript file), so we need to change the        ;
; current workstation being drawn to to an NCGM and redraw the plots.   ;
; Do the same for the PostScript workstation. We are using the functions;
; "draw" and "frame" to draw the plots and advance the frames.          ;
;***********************************************************************;

  NhlChangeWorkstation(contour,cgmwks)
  NhlChangeWorkstation(xy,cgmwks)
  draw(contour)
  frame(cgmwks)
  draw(xy)
  frame(cgmwks)

  NhlChangeWorkstation(xy,pswks)
  NhlChangeWorkstation(contour,pswks)
  draw(contour)
  frame(pswks)
  draw(xy)
  frame(pswks)

end

