load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

;***********************************************************************;
; gsunex06.ncl : vector and contour plots                               ;
;                                                                       ;
; This example opens three netCDF files of storm data and produces      ;
; vector and contour plots. It also shows how to put multiple plots on  ;
; a page and how to mask areas in a map plot.                           ;
;***********************************************************************;

begin

;***********************************************************************;
; Open three netCDF files containing storm data.                        ;
;***********************************************************************;

  tfile = addfile("$NCARG_ROOT/lib/ncarg/data/cdf/Tstorm.cdf","r")
  ufile = addfile("$NCARG_ROOT/lib/ncarg/data/cdf/Ustorm.cdf","r")
  vfile = addfile("$NCARG_ROOT/lib/ncarg/data/cdf/Vstorm.cdf","r")
  
;***********************************************************************;
; Get the u, v, and t arrays for the first time step.                   ;
;***********************************************************************;

  u = ufile->u(0,:,:)
  v = vfile->v(0,:,:)
  t = tfile->t(0,:,:)

;***********************************************************************;
; Check if lat and lon are variable coordinates of u. If so, use        ;
; them to define the lat/lon grid.  Otherwise, generate some dummy      ;
; lat/lon values using the "fspan" function.                            ;
;***********************************************************************;

  if(isfilevarcoord(ufile,"u","lat")) then
    lat = ufile->u&lat
  else
    latlen = dimsizes(u(:,0))
    lat = fspan(0,90.,latlen-1)
  end if

  if(isfilevarcoord(ufile,"u","lon")) then
    lon = ufile->u&lon
  else
    lonlen = dimsizes(u(0,:))
    lon = fspan(0,180.,lonlen-1)
  end if

  lat@name = "latitude"    ; Name the lat variable.
  lon@name = "longitude"   ; Name the lon variable.

  xwks = ncl_open_wks("ncgm","gsunex06")  ; open an X11 workstation

;***********************************************************************;
; Set some resources for our first plot, which is a vector plot over a  ;
; map.                                                                  ;
;                                                                       ;
; These four resources determine where to place the vector plot         ;
; on the map.                                                           ;
;***********************************************************************;

  resources                       = True
  resources@vfXCStartV            = lon(0)
  resources@vfXCEndV              = lon(dimsizes(lon)-1)
  resources@vfYCStartV            = lat(0)
  resources@vfYCEndV              = lat(dimsizes(lat)-1)

  resources@vcMonoLineArrowColor  = False  ; Don't draw the vectors in
                                           ; a single color.

;***********************************************************************;
; We don't want a plot of the whole map, so limit the map view by       ;
; indicating the four corners of the rectangle we want to view. Map     ;
; transformation resources start with "mp" and are documented at the    ;
; URL:                                                                  ;
;                                                                       ;
; http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/obj/MapTransformation.res.html ;
;                                                                       ;
;***********************************************************************;

  resources@mpLimitMode           = "corners"
  resources@mpLeftCornerLatF      = lat(0)
  resources@mpLeftCornerLonF      = lon(0)
  resources@mpRightCornerLatF     = lat(dimsizes(lat)-1)
  resources@mpRightCornerLonF     = lon(dimsizes(lon)-1)

;***********************************************************************;
; Map plot resources also start with "mp" and are documented at the     ;
; URL:                                                                  ;
;                                                                       ;
; http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/obj/MapPlot.res.html           ;
;                                                                       ;
;***********************************************************************;

  resources@mpPerimOn             = True    ; Draw perimeter around map.
  resources@mpGridAndLimbOn       = False   ; Turn off grid/limb lines.
  resources@mpFillOn              = True    ; Turn on map fill.

;***********************************************************************;
; We have turned map fill on, and we can further specify how we want the;
; various boundaries filled by specifing an array of arrays to fill and ;
; an array of color indices to fill them in.                            ;
;***********************************************************************;

  resources@mpFillAreaSpecifiers  = (/"water","land"/)
  resources@mpSpecifiedFillColors = (/6,26/)

  resources@tiMainString          = "January 1996 snow storm"

;***********************************************************************;
; By default, whenever we call one of the ncl_* functions to draw a plot;
; the function draws it and advances the frame.  If for some reason you ;
; don't want to draw the plot right away or you don't want to advance   ;
; the frame (because maybe you want to draw something else on it), there;
; are two special resources set up to control this: "Draw" and "Frame". ;
; These resources are particular to these scripts only, and take the    ;
; values of True or False. We are setting them both to False here,      ;
; because we want to do some footwork before drawing the plot.          ;
;***********************************************************************;

  resources@Draw = False
  resources@Frame = False

  vector = ncl_vector_map(xwks,u,v,resources)   ; Create the vector plot,
                                               ; but it won't get drawn.

;***********************************************************************;
; If you want to change the width and/or height of your plot, you use   ;
; the viewport resources to do this. One caveat of NCL, however,        ;
; requires that these resources be set as a group, and thus can't be set;
; using the resource scheme we've been using all along.  We can set     ;
; these resources as a group using "setvalues" and "end setvalues".     ;
; "setvalues" operates on a plot *after* it has already been created,   ;
; and allows you to change resources after the fact.  "setvalues" is    ;
; especially useful if you need to set resources based on other         ;
; resources, because after a plot is created, you can *retrieve*        ;
; resource values (using "getvalues") and then use these resources to   ;
; set other ones with "setvalues".                                      ;
;                                                                       ;
; Using the variable returned from the call to ncl_vector_map, we start ;
; our "setvalues" block and define the four viewport resources we want  ;
; to change.  Resources set in a "setvalues" block must have double     ;
; quotes around them, followed by a colon, followed by the value.  Once ;
; you are done setting resources in this fashion, close the block with  ;
; "end setvalues".                                                      ;
;***********************************************************************;

  setvalues vector
    "vpWidthF"  : 0.8
    "vpHeightF" : 0.8
    "vpXF"      : 0.1
    "vpYF"      : 0.9
  end setvalues

  draw(vector)   ; Now we can draw the vector plot
  frame(xwks)    ; and advance the frame.

;***********************************************************************;
; Define a color table.                                                 ;
;***********************************************************************;

  cmap = (/(/1.00,1.00,1.00/),(/0.00,0.00,0.00/),(/1.00,.000,.000/),\
           (/.950,.010,.000/),(/.870,.050,.000/),(/.800,.090,.000/),\
           (/.700,.090,.000/),(/.700,.120,.000/),(/.700,.180,.000/),\
           (/.700,.260,.000/),(/.700,.285,.000/),(/.680,.330,.000/),\
           (/.570,.420,.000/),(/.560,.530,.000/),(/.550,.550,.000/),\
           (/.130,.570,.000/),(/.060,.680,.000/),(/.000,.690,.000/),\
           (/.000,.700,.100/),(/.000,.600,.300/),(/.000,.500,.500/),\
           (/.000,.400,.700/),(/.000,.300,.700/),(/.000,.200,.700/),\
           (/.000,.100,.700/),(/.000,.000,.700/),(/.100,.100,.700/),\
           (/.200,.200,.700/),(/.300,.300,.700/),(/.420,.400,.700/),\
           (/.560,.500,.700/),(/.610,.600,.700/),(/.700,.700,.700/)/)

  ncl_define_colormap(xwks,cmap)

;***********************************************************************;
; For the second plot, we want to add a scalar array to color the       ;
; vectors by, and also change the map projection. The scalar array used ;
; here will be "t" which is temperature.                                ;
;***********************************************************************;

  resources@sfXCStartV     = lon(0)               ; Define where scalar
  resources@sfXCEndV       = lon(dimsizes(lon)-1) ; field is to lie on 
  resources@sfYCStartV     = lat(0)               ; the map.
  resources@sfYCEndV       = lat(dimsizes(lat)-1)

;***********************************************************************;
; We don't want to draw the vectors over the ocean, so we use NCL's     ;
; masking capabilities to handle this. Usually, when you do an overlay  ;
; plot (a contour over a map, vectors over a map, etc.) you draw the map;
; first and then the plot.  When you want to mask some part of the plot,;
; however, you want to draw the plot first and then the map, because the;
; map will have a "hole" it in where you want the plot to show through. ;
;                                                                       ;
; In the resources below, we are turning on map fill, and filling the   ;
; ocean in one color and the land in other.  But, we are masking the    ;
; United States so it won't get filled with any color (it will be the   ;
; "hole" in our map. We also change the drawing order of vectors and the;
; map, so the vectors get drawn first.                                  ;
;***********************************************************************;

  resources@mpAreaMaskingOn       = True         ; Indicate we want to 
  resources@mpMaskAreaSpecifiers  = "us-border"  ; mask the US.
  resources@vcVectorDrawOrder     = "predraw"
  delete(resources@mpFillAreaSpecifiers)
  delete(resources@mpSpecifiedFillColors)
  resources@mpFillAreaSpecifiers  = (/"water","land","usstateswater"/)
  resources@mpSpecifiedFillColors = (/26,32,26/)
  resources@mpFillBoundarySets    = "NoBoundaries"

;***********************************************************************;
; Set some resources to change the map projection and the portion of the;
; map we want to view.  Since we still have the resources set from our  ;
; previous plot creation, they will also affect this map. That is, we   ;
; are still masking the US.                                             ;
;***********************************************************************;

  resources@mpLimitMode           = "angles"
  resources@mpProjection          = "orthographic"
  resources@mpLeftAngleF          = 45.
  resources@mpRightAngleF         = 45.
  resources@mpTopAngleF           = 45.
  resources@mpBottomAngleF        = 45.
  resources@mpCenterLonF          = (lon(dimsizes(lon)-1)+lon(0))/2
  resources@mpCenterLatF          = (lat(dimsizes(lat)-1)+lat(0))/2

;***********************************************************************;
; Change some vector resources.                                         ;
;***********************************************************************;

  resources@vcFillArrowsOn           = True
  resources@vcGlyphStyle             = "FillArrow"
  resources@vcLevelColors            = (/2,5,8,11,14,17,20,23,26,29,32/)
  resources@vcLineArrowThicknessF    =  2.0
  resources@vcMinFracLengthF         = 0.33
  resources@vcMinMagnitudeF          = 0.001
  resources@vcMonoFillArrowFillColor = False
  resources@vcRefLengthF             = .045
  resources@vcRefMagnitudeF          = 20.

  resources@Draw                     = False
  resources@Frame                    = False
  resources@tiMainString             = "January 1996 snow storm:C:(vectors colored by temperature)"
  resources@tiMainFont               = 25
  resources@tiMainFontHeightF        = .020

  tvector = ncl_vector_scalar_map(xwks,u,v,t,resources)

  setvalues tvector     ; Change the viewport.
    "vpWidthF"  : 0.8
    "vpHeightF" : 0.8
    "vpXF"      : 0.1
    "vpYF"      : 0.9
  end setvalues

  draw(tvector) ; Draw the plot
  frame(xwks)   ; and advance the frame.

;***********************************************************************;
; For the third plot, we're going to put four contour plots on one page.;
; Get the temperature values for the first four time steps.             ;
;***********************************************************************;

  temp = tfile->t(0:3,:,:)

  delete(resources)  ; Start over with a new list of resources.

;***********************************************************************;
; The next set of resources will apply to all four plots.  Since we are ;
; drawing four plots on a page, we have to change the width and height  ;
; of each plot so it will fit.                                          ;
;***********************************************************************;

  resources               = True

  resources@vpWidthF      = 0.35       ; Set width to be 0.35.
  resources@vpHeightF     = 0.35       ; Set height to be 0.35.

  resources@cnMaxLevelCount = 24       ; Define number of contour levels.
  resources@cnLinesOn       = False    ; Turn off drawing of contour lines.
  resources@cnFillOn        = True     ; Turn on fill of contours.

  resources@sfXArray        = lon      ; Define x array to be lon values.
  resources@sfYArray        = lat      ; Define y array to be lat values.

  resources@tiXAxisString   = lon@name ; Label the x axis.
  resources@tiYAxisString   = lat@name ; Label the y axis.

  xpos = (/.10,.55,.10,.55/)           ; Set the positions for our four
  ypos = (/.95,.95,.45,.45/)           ; plots on the page.

;***********************************************************************;
; Loop through the four timesteps and draw each plot. Title and position;
; it according to which timestep it is. Since we are drawing all four   ;
; plots on the same frame, we don't want "frame" called after each plot ;
; so we set "Frame" to False and call it ourselves later.               ;
;***********************************************************************;

  do i=0,3
    resources@tiMainString  = "Temperature at time = " + i
    resources@vpXF          = xpos(i)
    resources@vpYF          = ypos(i)
    resources@Frame         = False
    plot = ncl_contour(xwks,temp(i,:,:),resources)
  end do

  frame(xwks)  ; Advance the frame.

;***********************************************************************;
; Put contour plot of temperature values at first timestep over a map.  ;
;***********************************************************************;

  delete(resources)
  resources                       = True
  resources@cnMaxLevelCount       = 24
  resources@cnLinesOn             = False
  resources@cnFillOn              = True
  resources@cnLineLabelsOn        = False
  resources@pmLabelBarDisplayMode = "always"
  resources@lbPerimOn             = False

;***********************************************************************;
; Define where on map we want contour plot to lie.                      ;
;***********************************************************************;

  resources@sfXCStartV        = lon(0)
  resources@sfXCEndV          = lon(dimsizes(lon)-1)
  resources@sfYCStartV        = lat(0)
  resources@sfYCEndV          = lat(dimsizes(lat)-1)
  resources@mpLimitMode       = "corners"
  resources@mpLeftCornerLatF  = lat(0)
  resources@mpLeftCornerLonF  = lon(0)
  resources@mpRightCornerLatF = lat(dimsizes(lat)-1)
  resources@mpRightCornerLonF = lon(dimsizes(lon)-1)
  resources@mpPerimOn         = True
  resources@mpGridMaskMode    = "MaskLand"
  resources@mpGridSpacingF    = 5.
  resources@tiMainString      = "Temperature at time = 0"

  resources@Draw              = False
  resources@Frame             = False

  plot = ncl_contour_map(xwks,temp(0,:,:),resources)

  setvalues plot        ; Change the viewport before drawing plot.
    "vpWidthF"  : 0.70
    "vpHeightF" : 0.70
    "vpXF"      : 0.05
    "vpYF"      : 0.90
  end setvalues

  draw(plot)
  frame(xwks)

end

