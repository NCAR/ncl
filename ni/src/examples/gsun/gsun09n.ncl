load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

function addcyclic(data[*][*]:float)
; add a cyclic point to a 2D array

local dims,newdata,nlat,mlon,mlon1
begin
	dims    = dimsizes(data)
        nlat    = dims(0)
        mlon    = dims(1)
        mlon1   = mlon+1

	newdata = new((/nlat,mlon1/),float)

	newdata(:,0:mlon-1) = data             ; pass everything
	newdata(:,mlon)     = (/ data(:,0) /)  ; value only

	if((.not.ismissing(newdata!1)) .and. iscoord(data,newdata!1)) then 
             newdata&$newdata!1$(mlon) = newdata&$newdata!1$(0) + 360.0
	end if

	return(newdata)
end

begin

  file1 = "/fs/cgd/data0/shields/g017.00/ice/0000-0009.nc"

  ice1 = addfile(file1,"r")

  fice = ice1->fice   ;   read fice -- ice concentration
  hlat = ice1->hlat
  hlon = ice1->hlon

  dimf     = dimsizes(fice) ;  Define an array to hold long-term monthly means.
  ntime    = dimf(0)
  nhlat    = dimf(1)
  nhlon    = dimf(2)

  icemon   = new ( (/nhlat,nhlon/) , float, -999.0)
  icemon!0 = "hlat"    
  icemon!1 = "hlon"
  icemon&hlat = hlat    ; necessary for overlay
  icemon&hlon = hlon

; calculate the january (nmo=0)  average
  nmo    = 0
  month  = nmo+1
  icemon = dim_avg(fice(hlat | :, hlon | :, time | nmo:ntime-1:12))
  icemon = mask(icemon, icemon.eq.0., False)  ; set 0.0 to _FillValue


  nsub = 16   ; subscript location of northern most hlat to be plotted

  wks = ncl_open_wks("x11","gsun09n")

  resources = True
  resources@cnFillOn   =      True
  resources@cnHighLabelsOn=        False
  resources@cnInfoLabelOn   =      False
  resources@cnLevelSpacingF =      0.10      ; tenths
  resources@cnLineLabelsOn=        False
  resources@cnLowLabelsOn=         False
  resources@mpBottomAngleF = 45.0
  resources@mpCenterLatF = -90.0
  resources@mpFillOn  =  "True"                ; fill the map
  resources@mpGeophysicalLineColor    = 30              ; same as mpLandFillColor
  resources@mpGeophysicalLineThicknessF = 2.0
  resources@mpGridAndLimbOn = False           ; turn of lat/lon lines 
  resources@mpInlandWaterFillColor =  1                ; white
  resources@mpLandFillColor =   30                   ; light gray
  resources@mpLeftAngleF = 45.0
  resources@mpLimitMode = "ANGLES"
  resources@mpOceanFillColor = "Transparent"      
  resources@mpProjection = "STEREOGRAPHIC"
  resources@mpRightAngleF = 45.0
  resources@mpTopAngleF = 45.0
  resources@pmLabelBarDisplayMode= "always"
  resources@tiMainFontColor = 7
  resources@tiMainFontHeightF = .02
  resources@tiMainString = "CSM Y00-99 Mean Ice Fraction Month =" + month
  resources@Draw = False
  resources@Frame = False

   icemonnew = addcyclic(icemon(0:nsub,:))
   resources@sfXArray = icemonnew&hlon
   resources@sfYArray = icemonnew&hlat
   contourmap = ncl_contour_map( wks, icemonnew,resources)
   setvalues contourmap
    "vpXF" : .05 
    "vpYF" : .95 
    "vpWidthF" : .75
    "vpHeightF" : .75
  end setvalues

  draw(contourmap)
  frame(wks)

  nmos = 12
  do nmo=1,nmos-1
    month  = nmo+1
    icemon = dim_avg(fice(hlat | :, hlon | :, time | nmo:ntime-1:12))
    icemon = mask(icemon, icemon.eq.0., False)  ; set 0.0 to _FillValue

    setvalues contourmap@data
      "sfDataArray" : addcyclic(icemon(0:nsub,:)) 
    end setvalues

    setvalues contourmap@contour
     "tiMainString" : "CSM Y00-99 Mean Ice Fraction Month =" + month
    end setvalues

    draw(contourmap)
    frame(wks)
  end do
end

