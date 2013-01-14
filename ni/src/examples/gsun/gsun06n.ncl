load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin
  dir = ncargpath("data")
  uf = addfile(dir+"/cdf/Ustorm.cdf","r")  ; Open three netCDF files.
  vf = addfile(dir+"/cdf/Vstorm.cdf","r")
  tf = addfile(dir+"/cdf/Tstorm.cdf","r")

  u   = uf->u(0,:,:)   ; Get u, v, lat, lon data.
  v   = vf->v(0,:,:)
  lat = uf->u&lat
  lon = uf->u&lon

  wks = gsn_open_wks("x11","gsun06n") ; Open a workstation.

;----------- Begin first plot -----------------------------------------

  resources = True 

  nlon = dimsizes(lon)
  nlat = dimsizes(lat)
  resources@vfXCStartV  = lon(0)      ; Define lat/lon corners
  resources@vfXCEndV    = lon(nlon-1) ; for vector plot.
  resources@vfYCStartV  = lat(0)
  resources@vfYCEndV    = lat(nlat-1)

  map = gsn_vector_map(wks,u,v,resources)  ; Draw a vector plot of u and v
                                           ; over a map.
;----------- Begin second plot -----------------------------------------

  resources@mpLimitMode       = "Corners"  ; Zoom in on the plot area.
  resources@mpLeftCornerLonF  = lon(0)
  resources@mpRightCornerLonF = lon(nlon-1)
  resources@mpLeftCornerLatF  = lat(0)
  resources@mpRightCornerLatF = lat(nlat-1)

  resources@mpPerimOn         =   True    ; Turn on map perimeter.

  resources@vpXF      = 0.1   ; Increase size and change location
  resources@vpYF      = 0.92  ; of vector plot.
  resources@vpWidthF  = 0.75
  resources@vpHeightF = 0.75

  resources@vcMonoLineArrowColor = False  ; Draw vectors in color.
  resources@vcMinFracLengthF     = 0.33   ; Increase length of 
  resources@vcMinMagnitudeF      = 0.001  ; vectors.
  resources@vcRefLengthF         = 0.045
  resources@vcRefMagnitudeF      = 20.0

  map = gsn_vector_map(wks,u,v,resources)  ; Draw a vector plot.

;----------- Begin third plot -----------------------------------------

  temp  = (tf->t(0,:,:)-273.15)*9.0/5.0+32.0 ; Read in temp data and
                                             ; convert from K to F.

  cmap = (/(/1.00, 1.00, 1.00/), (/0.00, 0.00, 0.00/), (/.560, .500, .700/), \
           (/.300, .300, .700/), (/.100, .100, .700/), (/.000, .100, .700/), \
           (/.000, .300, .700/), (/.000, .500, .500/), (/.000, .700, .100/), \
           (/.060, .680, .000/), (/.550, .550, .000/), (/.570, .420, .000/), \
           (/.700, .285, .000/), (/.700, .180, .000/), (/.870, .050, .000/), \
           (/1.00, .000, .000/), (/.700, .700, .700/)/)

  gsn_define_colormap(wks, cmap)  ; Define a new color map.

  resources@mpProjection = "Mercator"  ; Change the map projection.
  resources@mpCenterLonF = -100.0
  resources@mpCenterLatF =   40.0

  resources@mpLimitMode  = "LatLon"  ; Change the area of the map 
  resources@mpMinLatF    =  18.0     ; viewed.
  resources@mpMaxLatF    =  65.0
  resources@mpMinLonF    = -128.
  resources@mpMaxLonF    = -58.

  resources@mpFillOn               = True  ; Turn on map fill.
  resources@mpLandFillColor        = 16    ; Change land color to grey.
  resources@mpOceanFillColor       = -1    ; Change oceans and inland
  resources@mpInlandWaterFillColor = -1    ; waters to transparent.

  resources@mpGridMaskMode         = "MaskNotOcean"  ; Draw grid over ocean.
  resources@mpGridLineDashPattern  = 2

  resources@vcFillArrowsOn           = True  ; Fill the vector arrows
  resources@vcMonoFillArrowFillColor = False ; using multiple colors.
  resources@vcFillArrowEdgeColor     = 1     ; Draw the edges in black.

  resources@tiMainString      = "~F25~Wind velocity vectors"  ; Title
  resources@tiMainFontHeightF = 0.03

  resources@pmLabelBarDisplayMode  = "Always"     ; Turn on a label bar.
  resources@pmLabelBarSide         = "Bottom"     ; Change orientation
  resources@lbOrientation          = "Horizontal" ; of label bar.
  resources@lbPerimOn              = False        ; Turn off perimeter.
  resources@lbTitleString          = "TEMPERATURE (~S~o~N~F)" ; Title for
  resources@lbTitleFont            = 25                       ; label bar.

  resources@mpOutlineBoundarySets = "GeophysicalAndUSStates"

  map = gsn_vector_scalar_map(wks,u(::2,::2),v(::2,::2),temp(::2,::2),resources)

  delete(resources) ; Clean up.
  delete(map)
  delete(u)
  delete(v)
  delete(temp)
end
