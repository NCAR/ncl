;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_polyline_polygon_polymarker.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
undef("add_poly")
procedure add_poly(wks,map)
local xx, yy, pres
begin
  xx = (/-75., -10., -10., -75., -75./) ;-- define polygon x-array
  yy = (/ 55.,  55.,  87.,  87.,  57./) ;-- define polygon y-array
  
  pres                      =  True
  pres@gsFillColor          = "orange"             ;-- fill color
  pres@gsFillOpacityF       =  0.2                 ;-- set opacity

;-- add polygon to map
  map@polygon = gsn_add_polygon(wks, map, xx, yy, pres)
end

;----------
;  MAIN
;----------
begin
;-- open a workstation and define colormap
  wks = gsn_open_wks("png","NUG_polyline_polygon_polymarker")

;-- set resources
  res                       =  True  
  res@gsnDraw               =  False        ;-- don't draw the plot yet
  res@gsnFrame              =  False        ;-- don't advance the frame yet
  
  res@vpXF                  =  0.08         ;-- x-position
  res@vpYF                  =  0.92         ;-- y-position
  res@vpWidthF              =  0.88         ;-- width
  res@vpHeightF             =  0.65         ;-- height

  res@mpFillOn              =  True
  
  map = gsn_csm_map(wks,res)      ;-- create the map, but don't draw it yet

;-- write strings at the bottom of the plot
  txres                    =  True
  txres@txFontHeightF      =  0.014         ;-- default size is HUGE!
  txres@txFontColor        = "blue"
  txres@txJust             = "CenterLeft"   ;-- puts text on top of bars
  dty = 0.3
  gsn_text_ndc(wks,"Marker", 0.1, dty, txres)
  txres@txFontColor        = "red"
  gsn_text_ndc(wks,"Polyline", 0.2, dty, txres)
  txres@txFontColor        = "green"
  gsn_text_ndc(wks,"Polygon transparent", 0.3, dty, txres)

;-- polyline
  x = (/ 6.,  15.,  15.,   6.,   6./)
  y = (/47.5, 47.5, 54.5, 54.5, 47.5/)
  
;-- trace: polyline resources
  plres                        =  True
  plres@gsLineThicknessF       =  2.0        ;-- set line thickness
  plres@gsLineColor            = "red"       ;-- set line color
  box_1 = gsn_add_polyline(wks, map, x, y, plres)  ;-- add polyline to map

;-- define polygon x- and y-arrays
  x = (/110., 160., 160., 110., 110./)
  y = (/-45., -45., -10., -10., -45./)
  
;-- trace: polygon resources
  pgres                        =  True
  pgres@gsFillColor            = "green"      ;-- fill color
  pgres@gsFillOpacityF         =  0.3         ;-- set opacity of polygon
  gon_1 = gsn_add_polygon(wks, map, x, y, pgres)
                                              ;-- add filled polygon to map

;-- polymarker
  pmres                        =  True
  pmres@gsMarkerColor          = "blue"       ;-- marker color
  pmres@gsMarkerIndex          =  1           ;-- use marker 1
  pmres@gsMarkerSizeF          =  0.03        ;-- set size of marker
  pmres@gsLineThicknessF       =  3.          ;-- marker line thickness

;-- unique identifier name for polymarker drawing, here marker_1
  marker_1 = gsn_add_polymarker(wks, map, -100., 30., pmres)

;-- draw all 16 marker on plot using unique identifier name and 
;-- additional map attribute settings
  x = -160.                                 ;-- x-position of first marker
  y =  -80.                                 ;-- y-position of first marker
  do i = 0,15                                      ;-- 16 different marker
     pmres@gsMarkerIndex  =  i+1
     str = unique_string("poly")            ;-- result is poly0-poly15
     map@$str$ = gsn_add_polymarker(wks, map, x+(i*20.), y+(i*10.), pmres) 
                                            ;-- add marker to map
  end do
  
;-- add polygon calling the procedure add_poly()
  add_poly(wks,map)

;-- print map contents to see how unique_string works with map@$str$
  print(map)

;-- create the plot and advance the frame
  draw(map)
  frame(wks)
  
end

