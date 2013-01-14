load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin
  data_dir  = ncargpath("data")   ; Get ready to open three netCDF files.
  cdf_file1 = addfile(data_dir + "/cdf/941110_P.cdf","r")
  cdf_file2 = addfile(data_dir + "/cdf/sstdata_netcdf.nc","r")
  cdf_file3 = addfile(data_dir + "/cdf/Pstorm.cdf","r")

  psl = cdf_file1->Psl    ; Store some data from the three netCDF
  sst = cdf_file2->sst    ; files to local variables.
  pf  = cdf_file3->p

  psl@nlon = dimsizes(psl&lon)  ; Store the sizes of the lat/lon
  psl@nlat = dimsizes(psl&lat)  ; arrays as attributes of the psl
  pf@nlon  = dimsizes(pf&lon)   ; and pf variables.
  pf@nlat  = dimsizes(pf&lat)

  sst!1    = "lat"                 ; Name dimensions 0 and 1 of sst
  sst!2    = "lon"                 ; "lat" and "lon.
  sst&lat  = cdf_file2->lat        ; Create coordinate variables
  sst&lon  = cdf_file2->lon        ; for sst and store the sizes
  sst@nlon = dimsizes(sst&lon)     ; of the arrays as attributes 
  sst@nlat = dimsizes(sst&lat)     ; of sst.

  wks = gsn_open_wks("x11","gsun05n") ; Open a workstation.

;----------- Begin first plot -----------------------------------------

  resources = True
  
  resources@sfXCStartV = min(psl&lon)   ; Define where contour plot
  resources@sfXCEndV   = max(psl&lon)   ; should lie on the map plot.
  resources@sfYCStartV = min(psl&lat)
  resources@sfYCEndV   = max(psl&lat)

  map = gsn_contour_map(wks,psl,resources) ; Draw contours over a map.

;----------- Begin second plot -----------------------------------------

  getvalues wks                   ; Retrieve the default color map.
    "wkColorMap" : cmap
  end getvalues

  cmap(0,:) = (/1.,1.,1./)        ; Change background to white.
  cmap(1,:) = (/0.,0.,0./)        ; and foreground to black.
  gsn_define_colormap(wks, cmap)

  resources@mpProjection = "Orthographic" ; Change the map projection.
  resources@mpCenterLonF = 180.           ; Rotate the projection.
  resources@mpFillOn     = True           ; Turn on map fill.
  resources@mpFillColors = (/0,-1,28,-1/) ; Fill land and leave oceans
                                          ; and inland water transparent.

  resources@vpXF      = 0.1    ; Change the size and location of the 
  resources@vpYF      = 0.9    ; plot on the viewport.
  resources@vpWidthF  = 0.7
  resources@vpHeightF = 0.7

  mnlvl = 0                        ; Minimum contour level.
  mxlvl = 28                       ; Maximum contour level.
  spcng = 2                        ; Contour level spacing.
  ncn   = (mxlvl-mnlvl)/spcng + 1  ; Number of contour levels.

  resources@cnLevelSelectionMode = "ManualLevels" ; Define your own
  resources@cnMinLevelValF       = mnlvl          ; contour levels.
  resources@cnMaxLevelValF       = mxlvl
  resources@cnLevelSpacingF      = spcng

  resources@cnLineThicknessF     = 2.0   ; Double the line thickness.

  resources@cnFillOn           = True  ; Turn on contour level fill.
  resources@cnMonoFillColor    = True  ; Use one fill color.
  resources@cnMonoFillPattern  = False ; Use multiple fill patterns.
  resources@cnFillPatterns     = new((/ncn+1/),integer) ; Only fill
  resources@cnFillPatterns(:)  = -1                     ; last two
  resources@cnFillPatterns(ncn-1:ncn) = 17              ; contour levels.

  resources@cnLineDrawOrder      = "Predraw" ; Draw lines and filled
  resources@cnFillDrawOrder      = "Predraw" ; areas before map gets
                                             ; drawn.

  resources@tiMainString = "~F26~" + cdf_file2@title
  
  resources@sfXCStartV = min(sst&lon)   ; Define where contour plot
  resources@sfXCEndV   = max(sst&lon)   ; should lie on the map plot.
  resources@sfYCStartV = min(sst&lat)
  resources@sfYCEndV   = max(sst&lat)

  map = gsn_contour_map(wks,sst(0,:,:),resources) ; Draw contours over a map.

;----------- Begin third plot -----------------------------------------

  delete(resources)  ; Start with a new list of resources.
  resources = True

  resources@tiXAxisString = "~F25~longitude"
  resources@tiYAxisString = "~F25~latitude"

  resources@cnFillOn              = True     ; Turn on contour fill.
  resources@cnLineLabelsOn        = False    ; Turn off line labels.
  resources@cnInfoLabelOn         = False    ; Turn off info label.
  resources@pmLabelBarDisplayMode = "Always" ; Turn on a label bar.
  resources@lbPerimOn             = False    ; Turn off label bar perim.

  resources@sfXCStartV = min(pf&lon)   ; Define where contour plot
  resources@sfXCEndV   = max(pf&lon)   ; should lie on the map plot.
  resources@sfYCStartV = min(pf&lat)
  resources@sfYCEndV   = max(pf&lat)

  resources@mpProjection = "LambertEqualArea"  ; Change the map projection.
  resources@mpCenterLonF = (pf&lon(pf@nlon-1) + pf&lon(0))/2
  resources@mpCenterLatF = (pf&lat(pf@nlat-1) + pf&lat(0))/2

  resources@mpLimitMode = "LatLon"    ; Limit the map view.
  resources@mpMinLonF   = min(pf&lon)
  resources@mpMaxLonF   = max(pf&lon)
  resources@mpMinLatF   = min(pf&lat)
  resources@mpMaxLatF   = max(pf&lat)
  resources@mpPerimOn   = True        ; Turn on map perimeter.

  resources@tiMainString = "~F26~January 1996 storm" ; Set a title.

  resources@vpXF      = 0.1    ; Change the size and location of the 
  resources@vpYF      = 0.9    ; plot on the viewport.
  resources@vpWidthF  = 0.7
  resources@vpHeightF = 0.7

  resources@gsnScale = True  ; Force X/Y axes labels to be the same size.
  resources@gsnFrame = False ; Don't advance frame.

  map = gsn_contour_map(wks,pf(0,:,:)*0.01,resources) ; Convert pf to "mb" and
                                                     ; draw contours over map.
  txres               = True   ; Set some resources
  txres@txFontHeightF = 0.025  ; for a text string.
  txres@txFontColor   = 4
  gsn_text_ndc(wks,"~F25~Pressure (mb)",.45,.25,txres) ; Draw a text string.
                                                       ; on the viewport.
  frame(wks)   ; Advance the frame.
  
;---------- Begin fourth plot ------------------------------------------

  delete(resources@tiXAxisString)  ; Delete some resources you don't
  delete(resources@tiYAxisString)  ; need anymore.
  delete(resources@gsnFrame)

  delete(cmap)
  cmap = (/(/1.00, 1.00, 1.00/), (/0.00, 0.00, 0.00/), \
           (/.560, .500, .700/), (/.300, .300, .700/), \
           (/.100, .100, .700/), (/.000, .100, .700/), \
           (/.000, .300, .700/), (/.000, .500, .500/), \
           (/.000, .400, .200/), (/.000, .600, .000/), \
           (/.000, 1.00, 0.00/), (/.550, .550, .000/), \
           (/.570, .420, .000/), (/.700, .285, .000/), \
           (/.700, .180, .000/), (/.870, .050, .000/), \
           (/1.00, .000, .000/), (/0.00, 1.00, 1.00/), \
           (/.700, .700, .700/)/)

  gsn_define_colormap(wks, cmap)  ; Define a new color map.

  resources@mpFillOn              = True         ; Turn on map fill.
  resources@mpFillAreaSpecifiers  = (/"Water","Land","USStatesWater"/)
  resources@mpSpecifiedFillColors = (/17,18,17/)
  resources@mpAreaMaskingOn       = True            ; Indicate we want to 
  resources@mpMaskAreaSpecifiers  = "USStatesLand"  ; mask land.
  resources@mpPerimOn             = True            ; Turn on a perimeter.
  resources@mpGridMaskMode        = "MaskLand"      ; Mask grid over land.
  resources@cnFillDrawOrder       = "Predraw"       ; Draw contours first.

  resources@cnLevelSelectionMode = "ExplicitLevels" ; Define own levels.
  resources@cnLevels             = fspan(985.,1045.,13)

  resources@lbTitleString  = "~F25~pressure (mb)" ; Title for label bar.
  resources@cnLinesOn      = False         ; Turn off contour lines.
  resources@pmLabelBarSide = "Bottom"      ; Change orientation of
  resources@lbOrientation  = "Horizontal"  ; label bar.

  map = gsn_contour_map(wks,pf(1,:,:)*0.01,resources)

  delete(map)          ; Clean up.
  delete(resources)
  delete(txres)
end
