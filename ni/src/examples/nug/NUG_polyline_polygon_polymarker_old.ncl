;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_polyline_polygon_polymarker_old.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- define coordinates for the cities:  Hamburg and Munich
  ham_lat = 53.51
  ham_lon = 10.0
  muc_lat = 48.12
  muc_lon = 11.51
  ber_lat = 52.5
  ber_lon = 13.35
  dx      = 1.0
  dy      = 0.2
  xoff    = 0.8
  yoff    = 0.45
  box_x   = (/ber_lon+xoff-dx, ber_lon+xoff+dx, \
              ber_lon+xoff+dx, ber_lon+xoff-dx, \
              ber_lon+xoff-dx/)
  box_y   = (/ber_lat+yoff-dy, ber_lat+yoff-dy, \
              ber_lat+yoff+dy, ber_lat+yoff+dy, \
              ber_lat+yoff-dy/)

;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","plot_polyline_polygon_polymarker_old")

;-- set map resources
  res                         =  True
  res@gsnDraw                 =  False
  res@gsnFrame                =  False
  res@gsnMaximize             =  True
  res@mpFillOn                =  True
  res@mpOutlineOn             =  True
  res@mpOutlineDrawOrder      = "Draw"
  res@mpGridAndLimbOn         =  True
  res@mpGridLineDashPattern   =  2
  res@mpGridMaskMode          = "MaskLand"
  res@pmTickMarkDisplayMode   = "Always"
  res@mpGridLatSpacingF       =  1
  res@mpGridLonSpacingF       =  2
  res@mpOceanFillColor        = "LightBlue"
  res@mpLandFillColor         = "tan"
  res@mpInlandWaterFillColor  = "Blue"
  res@mpDataBaseVersion       = "MediumRes"
  res@mpDataSetName           = "Earth..4"
  res@mpOutlineBoundarySets   = "National" 
  res@mpGeophysicalLineColor  = "Navy" 
  res@mpGeophysicalLineThicknessF = 1.5 
  res@mpProjection            = "Mercator"
  res@mpLimitMode             = "Corners"
  res@mpLeftCornerLonF        =   5.0
  res@mpRightCornerLonF       =  16.0
  res@mpLeftCornerLatF        =  47.0
  res@mpRightCornerLatF       =  58.0
  res@tiMainString            = "NCL UG Example: polyline, polygon, polymarker"
  res@tiMainFontHeightF       =  0.02
  map = gsn_csm_map(wks, res)

;-- trace: polyline resources
  pres                        =  True
  pres@gsLineThicknessF       =  2.0
  pres@gsLineColor            = "blue"
  trace = gsn_add_polyline(wks, map, (/ham_lon, muc_lon/), (/ham_lat, muc_lat/), pres)

;-- cities: polymarker resources
  mkres                       =  True
  mkres@gsMarkerIndex         =  14
  mkres@gsMarkerColor         = "blue"
  mkres@gsMarkerThicknessF    =  2.0
  ham_city = gsn_add_polymarker(wks, map, ham_lon, ham_lat, mkres)
  muc_city = gsn_add_polymarker(wks, map, muc_lon, muc_lat, mkres)

  mkres@gsMarkerSizeF         =  0.02
  mkres@gsMarkerThicknessF    =  4.0
  mkres@gsMarkerColor         = "red"
  ber_city = gsn_add_polymarker(wks, map, ber_lon, ber_lat, mkres)

;-- cities: text resources
  tres                        =  True
  tres@txFontHeightF          =  0.015
  ham_text = gsn_add_text(wks, map, "Hamburg", ham_lon+1.2, ham_lat, tres)
  muc_text = gsn_add_text(wks, map, "Munich",  muc_lon-1.0, muc_lat, tres)

;-- text field: polygon  and text resources
  lres                        =  True
  lres@gsLineThicknessF       =  2.0
  lres@gsFillColor            = "white"
  ber_line = gsn_add_polygon(wks, map, box_x, box_y, lres)
  tres@txFontHeightF          =  0.02
  ber_text = gsn_add_text(wks, map, "Berlin", ber_lon+xoff, ber_lat+yoff, tres)

;-- draw the plot
  draw(map)
  frame(wks)

end
