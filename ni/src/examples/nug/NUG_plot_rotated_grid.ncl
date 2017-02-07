;------------------------------------------------------------
; NCL User Guide Example:      NUG_plot_rotated_grid.ncl
;
; Description:  How to plot rotated data
;
;             - data on a rotated grid
;             - two projections: Cylindrical Equidistant and 
;                                Orthographic
;
; 10.12.15  kmf
;------------------------------------------------------------
;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

;------------------------------------------------------------
;-- set global constants
;------------------------------------------------------------
pi       =  4.0*atan(1.)
deg2rad  =  pi/180.
rad2deg  =  45./atan(1.)
fillval  = -99999.9

;------------------------------------------------------------
;-- Function:      unrot_lon(rotlat,rotlon,pollat,pollon)
;-- Description:   transform rotated longitude to longitude
;------------------------------------------------------------
undef("unrot_lon")
function unrot_lon( rotlat:numeric, rotlon:numeric, pollat[1]:numeric, pollon[1]:numeric )
local rotlat, rotlon, nrlat, nrlon, nrlat_rank, nrlon_rank, pollon, pollat, \
      lon, s1, c1, s2, c2, rlo, rla, i, tmp1, tmp2
begin
  lon = fillval
  lon@_FillValue = fillval
  
  nrlat      = dimsizes(rotlat)
  nrlon      = dimsizes(rotlon)
  nrlat_rank = dimsizes(nrlat)
  nrlon_rank = dimsizes(nrlon)

  if (any(nrlat .ne. nrlon) .and. (nrlat_rank.ne.1 .or. nrlon_rank.ne.1)) then
      print("Function unrot_lon: unrot_lon:  rotlat and rotlon dimensions do not match")
    return(lon)
  end if
  
  if (nrlat_rank.eq.1 .and. nrlon_rank.eq.1) then
    rla = conform_dims((/nrlat,nrlon/),rotlat,0)    ;-- create 2D latitude array
    rlo = conform_dims((/nrlat,nrlon/),rotlon,1)    ;-- create 2D longitude array
  else
    rla = rotlat
    rlo = rotlon
  end if
  
  rla = rla*deg2rad                      ;-- convert from degree to radians
  rlo = rlo*deg2rad                      ;-- convert from degree to radians
    
  lon := (/rlo/)                         ;-- reassign lon
  lon@_FillValue=fillval
  
  s1   = sin(pollat*deg2rad)
  c1   = cos(pollat*deg2rad)
  s2   = sin(pollon*deg2rad)
  c2   = cos(pollon*deg2rad)
  
  tmp1 = s2*(-s1*cos(rlo)*cos(rla)+c1*sin(rla))-c2*sin(rlo)*cos(rla)
  tmp2 = c2*(-s1*cos(rlo)*cos(rla)+c1*sin(rla))+s2*sin(rlo)*cos(rla)
  
  lon  = atan(tmp1/tmp2)*rad2deg
;  lon  = where(lon.lt.0, lon+360,lon)
;  lon  = where(lon.gt.180, lon-180,lon)
  
  lon@units = "degrees_east"
  print("Function unrot_lon: min/max     "+sprintf("%8.4f", min(lon(0,:)))+\
        "  "+sprintf("%8.4f", max(lon(0,:))))
  
  delete([/rlo,rlo,c1,s1,c2,s2,tmp1,tmp2/])
  
  return(lon)
end

;------------------------------------------------------------
;-- Function:      unrot_lat(rotlat,rotlon,pollat,pollon)
;-- Description:   transform rotated latitude to latitude
;------------------------------------------------------------
undef("unrot_lat")
function unrot_lat( rotlat:numeric, rotlon:numeric, pollat[1]:numeric, pollon[1]:numeric )
local rotlat, rotlon, nrlat, nrlon, nrlat_rank, nrlon_rank, pollon, pollat, \
      lat, s1, c1, rlo, rla, i
begin
  lat = fillval
  lat@_FillValue = fillval
  
  nrlat      = dimsizes(rotlat)
  nrlon      = dimsizes(rotlon)
  nrlat_rank = dimsizes(nrlat)
  nrlon_rank = dimsizes(nrlon)

  if (any(nrlat .ne. nrlon) .and. (nrlat_rank.ne.1 .or. nrlon_rank.ne.1)) then
    print("Function unrot_lat:  rotlat and rotlon dimensions do not match")
    return(lat)
  end if
  
  if (nrlat_rank.eq.1 .and. nrlon_rank.eq.1) then
    rla = conform_dims((/nrlat,nrlon/),rotlat,0)    ;-- create 2D latitude array
    rlo = conform_dims((/nrlat,nrlon/),rotlon,1)    ;-- create 2D longitude array
  else
    rla = rotlat
    rlo = rotlon
  end if
  
  rla = rla*deg2rad                      ;-- convert from degree to radians
  rlo = rlo*deg2rad                      ;-- convert from degree to radians
  
  lat := (/rla/)                         ;-- reassign lat
  lat@_FillValue=fillval

  s1  = sin(pollat*deg2rad)
  c1  = cos(pollat*deg2rad)
  
  lat = s1*sin(rla)+c1*cos(rla)*cos(rlo)
  lat = asin(lat)*rad2deg
  
  lat@units = "degrees_north"
  print("Function unrot_lat: min/max     "+sprintf("%8.4f", min(lat(:,0)))+\
        "  "+sprintf("%8.4f", max(lat(:,0))))
  
  delete([/rlo,rla,c1,s1/])
  
  return(lat)
end


;----------------
;--  MAIN
;----------------
begin
;-- open file and read variables
  diri    = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili    = "tas_rotated_grid_EUR11.nc"
  
  if (.not. fileexists(diri+fili)) then
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     exit
  end if
  
  f       =  addfile(diri+fili,"r")
  var     =  f->tas
  rlat    =  f->rlat
  rlon    =  f->rlon
  rotpole =  f->rotated_pole
  pollat  =  rotpole@grid_north_pole_latitude
  pollon  =  rotpole@grid_north_pole_longitude

;-- unrotate the grid and set 2D lat/lons
  var@lon2d      =  unrot_lon(rlat, rlon, pollat, pollon)
  var@lat2d      =  unrot_lat(rlat, rlon, pollat, pollon)

;-- calculate the min and max lat/lons for the map plot
  minlat  =  min(var@lat2d)                             ;-- retrieve minimum latitude value
  minlon  =  min(var@lon2d)                             ;-- retrieve maximum latitude value
  maxlat  =  max(var@lat2d)                             ;-- retrieve minimum longitude value
  maxlon  =  max(var@lon2d)                             ;-- retrieve maximum longitude value
   
;-- open a workstation
  wks = gsn_open_wks("png","NUG_plot_rotated_grid")

;-- set resources
  res                       =  True
  res@gsnFrame              =  False                   ;-- don't advance frame
  res@gsnAddCyclic          =  False                   ;-- data are not global, don't add lon cyclic point

  res@pmTickMarkDisplayMode = "Always"                 ;-- draw nicer tickmarks
 
  res@mpDataBaseVersion     = "MediumRes"              ;-- choose map database
  res@mpMinLatF             =  minlat - 1.             ;-- set min lat
  res@mpMaxLatF             =  maxlat + 1.             ;-- set max lat
  res@mpMinLonF             =  minlon - 1.             ;-- set min lon
  res@mpMaxLonF             =  maxlon + 1.             ;-- set max lon
  res@mpGridAndLimbOn       =  True                    ;-- turn on grid lines

  res@cnFillOn              =  True                    ;-- turn on contour fill
  res@cnLinesOn             =  False                   ;-- don't draw contour lines
  res@cnFillPalette         = "BlueYellowRed"          ;-- choose color map

  res@lbLabelBarOn          =  True                    ;-- turn on labelbar

  res@tiMainString          = "NCL Doc: rotated grid"  ;-- title
  res@tiMainOffsetYF        = -0.025                   ;-- move title downward
  
  res@vpWidthF              =  0.6                     ;-- width of viewport
  res@vpHeightF             =  0.48                    ;-- height of viewport
  
;-- create the first plot
  res@vpXF                  =  0.12                    ;-- start x-position
  res@vpYF                  =  1.02                    ;-- start y-Position
  
  plot1 = gsn_csm_contour_map(wks,var(0,0,:,:),res)    ;-- use default projection (CE)

;-- create the second plot
  delete(res@tiMainString)                             ;-- we don't need the title twice
  
  res@vpXF                  =  0.15                    ;-- start x-position
  res@vpYF                  =  0.493                   ;-- start y-position
  
  res@mpProjection          = "Orthographic"           ;-- change projection
  res@mpCenterLatF          =  minlat + (maxlat -minlat)/2 ;-- center point of view latitude
  res@mpCenterLonF          =  minlon + (maxlon -minlon)/2 ;-- center point of view longitude
  res@mpLimitMode           = "LatLon"                 ;-- map limits mode
  res@mpMinLatF             =  minlat - 1.             ;-- set min lat
  res@mpMaxLatF             =  maxlat + 1.             ;-- set max lat
  res@mpMinLonF             =  minlon - 1.             ;-- set min lon
  res@mpMaxLonF             =  maxlon + 1.             ;-- set max lon
  res@mpPerimOn             =  False                   ;-- don't draw the box around the plot
  
  res@lbOrientation         = "vertical"               ;-- vertical label bar
  res@lbLabelPosition       = "Left"                   ;-- labelbar labels on left side
  res@pmLabelBarOrthogonalPosF = -1.37                 ;-- labelbar on the left side

  res@tmXTLabelDeltaF       = -0.5                     ;-- decrease space between ticks and labels
  res@tmXBLabelDeltaF       = -0.5                     ;-- decrease space between ticks and labels
  res@tmYLLabelDeltaF       = -0.5                     ;-- decrease space between ticks and labels
  res@tmYRLabelDeltaF       = -0.5                     ;-- decrease space between ticks and labels

  plot2 = gsn_csm_contour_map(wks,var(0,0,:,:),res)    ;-- draw second plot

;-- draw text
  txres                =  True
  txres@txFontHeightF  =  0.016
  txres@txJust         = "CenterLeft"
  
  gsn_text_ndc(wks,"Projection:",             0.74, 0.91, txres)  ;-- next to first plot
  gsn_text_ndc(wks,"Cylindrical Equidistant", 0.74, 0.89, txres)
  
  gsn_text_ndc(wks,"Projection:",  0.77, 0.43, txres)             ;-- next to second plot
  gsn_text_ndc(wks,"Orthographic", 0.77, 0.41, txres)

;-- advance the frame
  frame(wks)
  
end
