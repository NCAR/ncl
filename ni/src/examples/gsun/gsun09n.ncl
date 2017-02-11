load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

undef("addcyclic")
function addcyclic(data[*][*]:float)
;
; Add a cyclic point in "x" to a 2D array
; for a lat/lon plot "x"  corresponds to "lon"
;                    "ny" corresponds to "nlat"
;                    "mx" corresponds to "mlon"
local dims, newdata, ny, mx, mx1
begin
    dims    = dimsizes(data)
    ny      = dims(0)
    mx      = dims(1)
    mx1     = mx+1

    newdata = new((/ny  ,mx1/),float)

    newdata(:,0:mx-1) = data             ; pass everything
    newdata(:,mx)     = (/ data(:,0) /)  ; value only

    if((.not.ismissing(newdata!1)) .and. iscoord(data,newdata!1)) then 
        newdata&$newdata!1$(mx) = newdata&$newdata!1$(0) + 360.0
    end if
 
    return(newdata)
end
 
begin

  file1 = ncargpath("data") + "/cdf/fice.nc"

  ice1 = addfile(file1,"r")

  fice = ice1->fice   ; Read fice -- ice concentration
  hlat = ice1->hlat
  hlon = ice1->hlon

  dimf     = dimsizes(fice) ; Define an array to hold long-term monthly means.
  ntime    = dimf(0)
  nhlat    = dimf(1)
  nhlon    = dimf(2)

  icemon      = new ( (/nhlat,nhlon/) , float, -999.0)

  icemon!0    = "hlat"    ; Name dimensions 0 and 1
  icemon!1    = "hlon"    ; of icemon and create
  icemon&hlat = hlat      ; coordinate variables for both.
  icemon&hlon = hlon

                       ; Calculate the January (nmo=0) average.
  nmo    = 0
  month  = nmo+1
  icemon = dim_avg(fice(hlat | :, hlon | :, time | nmo:ntime-1:12))
  icemon = mask(icemon, icemon.eq.0., False)  ; Set 0.0 to _FillValue.

  nsub = 16 ; Subscript location of northernmost hlat to be plotted.

  wks = gsn_open_wks("ncgm","gsun09n") ; Open an NCGM.
  
  cmap = (/(/1.00,1.00,1.00/), (/0.00,0.00,0.00/), (/1.00,1.00,0.50/), \
           (/0.00,0.00,0.50/), (/0.50,1.00,1.00/), (/0.50,0.00,0.00/), \
           (/1.00,0.00,1.00/), (/0.00,1.00,1.00/), (/1.00,1.00,0.00/), \
           (/0.00,0.00,1.00/), (/0.00,1.00,0.00/), (/1.00,0.00,0.00/), \
           (/0.50,0.00,1.00/), (/1.00,0.50,0.00/), (/0.00,0.50,1.00/), \
           (/0.50,1.00,0.00/), (/0.50,0.00,0.50/), (/0.50,1.00,0.50/), \
           (/1.00,0.50,1.00/), (/0.00,0.50,0.00/), (/0.50,0.50,1.00/), \
           (/1.00,0.00,0.50/), (/0.50,0.50,0.00/), (/0.00,0.50,0.50/), \
           (/1.00,0.50,0.50/), (/0.00,1.00,0.50/), (/0.50,0.50,0.50/), \
           (/0.625,0.625,0.625/)/)

  gsn_define_colormap(wks,cmap) ; Define a color map.

  resources = True

  icemonnew = addcyclic(icemon(0:nsub,:))

  resources@sfXArray = icemonnew&hlon   ; Necessary for overlay on a map.
  resources@sfYArray = icemonnew&hlat

  resources@tiMainString = "CSM Y00-99 Mean Ice Fraction Month =" + month

  map = gsn_contour_map(wks,icemonnew,resources) ; Draw a contour
                                                 ; over a map.
  nmos = 12
  do nmo = 1,nmos-1
    month  = nmo+1
    icemon = dim_avg(fice(hlat | :, hlon | :, time | nmo:ntime-1:12))
    icemon = mask(icemon, icemon.eq.0., False)  ; set 0.0 to _FillValue

    setvalues map@contour ; Change the title for the contour plot.
     "tiMainString" : "CSM Y00-99 Mean Ice Fraction Month =" + month
    end setvalues

    setvalues map@data   ; Change the data for the contour plot.
      "sfDataArray" : addcyclic(icemon(0:nsub,:)) 
    end setvalues

    draw(map)  ; Draw the contour plot.
    frame(wks) ; Advance the frame.
  end do
  delete(icemon)      ; Clean up.
  delete(icemonnew)
  delete(map)
end
