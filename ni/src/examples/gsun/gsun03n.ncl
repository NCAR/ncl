load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin
  dir = ncargpath("data")
  uf = addfile(dir+"/cdf/Ustorm.cdf","r")  ; Open two netCDF files.
  vf = addfile(dir+"/cdf/Vstorm.cdf","r")

  u   = uf->u(0,:,:)   ; Get u, v, latitude, and longitude data.
  v   = vf->v(0,:,:)
  lat = uf->u&lat
  lon = uf->u&lon

  wks = gsn_open_wks("x11","gsun03n") ; Open a workstation.

  vc = gsn_vector(wks,u,v,False)    ; Draw a vector plot of u and v.

;----------- Begin second plot -----------------------------------------

  resources = True 

  resources@vcMinFracLengthF = 0.33
  resources@vcRefMagnitudeF  = 20.0
  resources@vcRefLengthF     = 0.045
  resources@vcMonoLineArrowColor  = False   ; Draw vectors in color.

  vc = gsn_vector(wks,u,v,resources)

;----------- Begin third plot -----------------------------------------

  resources@tiMainString  = "~F26~wind velocity vectors - January 1996"
  resources@tiXAxisString = "longitude"
  resources@tiYAxisString = "latitude"

  resources@vfXCStartV  = lon(0)               ; Define X/Y axes range
  resources@vfXCEndV    = lon(dimsizes(lon)-1) ; for vector plot.
  resources@vfYCStartV  = lat(0)
  resources@vfYCEndV    = lat(dimsizes(lat)-1)

  resources@pmLabelBarDisplayMode    = "Always"   ; Turn on a label bar.
  resources@pmLabelBarWidthF         = 0.1
  resources@lbPerimOn                = False

  vc = gsn_vector(wks,u,v,resources)

;---------- Begin fourth plot ------------------------------------------

  tf = addfile(dir+"/cdf/Tstorm.cdf","r")    ; Open a netCDF file.
  temp  = (tf->t(0,:,:)-273.15)*9.0/5.0+32.0 ; Read in temp data and
                                             ; convert from K to F.
  temp@units = "(deg F)"

  cmap = (/(/1.00, 1.00, 1.00/), (/0.00, 0.00, 0.00/), \
           (/.560, .500, .700/), (/.300, .300, .700/), \
           (/.100, .100, .700/), (/.000, .100, .700/), \
           (/.000, .300, .700/), (/.000, .500, .500/), \
           (/.000, .700, .100/), (/.060, .680, .000/), \
           (/.550, .550, .000/), (/.570, .420, .000/), \
           (/.700, .285, .000/), (/.700, .180, .000/), \
           (/.870, .050, .000/), (/1.00, .000, .000/), \
           (/.700, .700, .700/)/)

  gsn_define_colormap(wks, cmap)  ; Define a new color map.

  resources@vcFillArrowsOn           = True  ; Fill the vector arrows
  resources@vcMonoFillArrowFillColor = False ; in different colors
  resources@vcFillArrowEdgeColor     = 1     ; Draw the edges in black.
  resources@vcFillArrowWidthF        = 0.055 ; Make vectors thinner.

  resources@tiMainString      = "~F26~wind velocity vectors colored by temperature " + temp@units
  resources@tiMainFontHeightF = 0.02  ; Make font slightly smaller.
  resources@lbLabelFont       = 21    ; Change font of label bar labels.

  vc = gsn_vector_scalar(wks,u,v,temp,resources) ; Draw a vector plot of
                                                 ; u and v and color them
                                                 ; according to the scalar
                                                 ; field "temp."
  delete(vc)    ; Clean up.
  delete(u)
  delete(v)
  delete(temp)
end
