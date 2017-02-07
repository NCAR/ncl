;*****************************************************
; rotatedltln_2.ncl
;
; Concepts illustrated:
;   - Drawing filled contours over a rotated lat-lon grid
;   - Drawing a map using the medium resolution map outlines
;   - Overlaying contours on a map without having lat,lon coordinates
;   - Overlaying contours on a map using lat,lon coordinates
;*****************************************************
;
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
  albedo_filename      = "albedo_a.nc"
  albedo_grid_filename = "albedo_a_curvilinear.nc"

  f1 = addfile(albedo_filename,"r")
  f2 = addfile(albedo_grid_filename,"r")
  
;---Variable to plot
  sbt = f1->var242     

;---Needed for getting the projection parameters in plot.
  lat2d          = f2->lat
  lon2d          = f2->lon
  north_pole_lat = f1->rotated_pole@grid_north_pole_latitude   ; 6.55
  north_pole_lon = f1->rotated_pole@grid_north_pole_longitude  ; 0.0

  nlat  = dimsizes(lat2d(:,0))
  nlon  = dimsizes(lon2d(0,:))

;----------------------------------------------------------------------
; Start the graphics section
;----------------------------------------------------------------------
  wks  = gsn_open_wks("png", "rotatedltln")     ; send graphics to PNG file

;---Set resources common to both types of plots we plan to create

  res                        = True             ; plot mods desired

  res@gsnMaximize            = True             ; maxmize plot in frame

  res@cnFillOn               = True             ; turn on color 
  res@cnLinesOn              = False            ; no contour lines
  res@cnLineLabelsOn         = False            ; no contour labels
  res@cnFillPalette          = "BlGrYeOrReVi200"
  res@lbOrientation          = "Vertical"       ; vertical labelbar
  res@pmLabelBarOrthogonalPosF = 0.18           ; move lbar away from plot

  res@mpDataBaseVersion      = "MediumRes"      ; use finer database

  res@gsnAddCyclic           = False

;------------------------------------------------------------
; First frame: use native projection information to plot 
; the data.
;------------------------------------------------------------
  res_native = res      ; Copy over common resources.
;
; Setting tfDoNDCOverlay to True means you have specified the
; exact projection that your data is on, and thus no data
; transformation takes place when the contours are overlaid
; on the map.
; 
  res_native@tfDoNDCOverlay         = True
  res_native@mpLimitMode            = "Corners"
  res_native@mpLeftCornerLatF       = lat2d(0,nlon-1)      
  res_native@mpLeftCornerLonF       = lon2d(0,nlon-1)
  res_native@mpRightCornerLatF      = lat2d(nlat-1,0) 
  res_native@mpRightCornerLonF      = lon2d(nlat-1,0) 

  res_native@tiMainString           = "Native projection"
  res_native@pmTickMarkDisplayMode  = "always"

  res_native@mpCenterLatF           = 90 - north_pole_lat  ; north_pole_lat=6.55
  res_native@mpCenterLonF           = 180 + north_pole_lon ; north_pole_lon=0
  
  plot_native = gsn_csm_contour_map (wks,sbt(0,0,:,:),res_native)

;------------------------------------------------------------
; Second frame: use lat2d/lon2d coordinates to plot 
; the data.
;------------------------------------------------------------
  res_nonnative = res           ; Copy over common resources.

  res_nonnative@sfXArray                 = lon2d    ; needed for non-native
  res_nonnative@sfYArray                 = lat2d    ; contouring
  res_nonnative@gsnPolar                 = "NH"
  res_nonnative@mpMinLatF                = min(lat2d)
  res_nonnative@pmLabelBarOrthogonalPosF = 0.05
  res_nonnative@tiMainString             = "Non-native projection"

  plot_nonnative = gsn_csm_contour_map (wks,sbt(0,0,:,:),res_nonnative)

end 
