undef("hsv2rgb")
undef("FixLongitude")
undef("FixLongitudeCoord")
undef("GeopHgt")
undef("PlotTitles")
undef("UniformTickmarks")
undef("GetLonValues")
undef("GetLonLabels")
undef("GetLatValues")
undef("GetLatLabels")
undef("MapTickmarks")
undef("MapPolarTickmarks")
undef("SpreadColors")
undef("SetColormap")
undef("SetContourLevels")
undef("FixZeroContour")
undef("SetMapFillColors")
undef("LtGtContourDashPattern")
undef("LtGtContourFillPattern")
undef("SetContourLevelThickness")
undef("LabelLon")
undef("LabelLatX")
undef("LabelLatY")
undef("LabelHgtYR")
undef("ColorWheel")
undef("GenCmapHueRange")
undef("GenCmapMnMxCnInt")
undef("AdjustFontSizes")
undef("Panel")

;
; This function converts between HSV and RGB color space
;
; Input:  h [0.0-360.0], s [0.0-1.0], v [0.0-1.0]
; Output: r [0.0-1.0], g [0.0-1.0], b [0.0-1.0]
;
function hsv2rgb (hue[*]:float,sat[*]:float,val[*]:float)
local r_g_b, indexs, f, p, q, t, i
begin
  r_g_b = new((/3,dimsizes(hue)/),float)
  r_g_b!0 = "rgb"
  r_g_b!1 = "cmap_len"
 
  if (any((sat .eq. 0.0).and.(hue.eq.0.0.or.hue.eq.360))) then
    indexs = ind((hue.eq.0.0.or.hue.eq.360).and.sat.eq.0.0)
    r_g_b(:,indexs) = (/val(indexs),val(indexs),val(indexs)/)
    delete(indexs)
  end if

  f = new(dimsizes(hue),float)
  p = new(dimsizes(hue),float)
  q = new(dimsizes(hue),float)
  t = new(dimsizes(hue),float)
  i = new(dimsizes(hue),integer)
  if any(hue.eq.360.0)  
    hue(ind(hue.eq.360.0)) = 0.0
  end if

  hue = hue/60.0
  i = floattoint(floor(hue))
  f = hue - i
  p = val*(1.0 - sat)
  q = val*(1.0 - (sat*f))
  t = val*(1.0 - (sat*(1.0 - f)))
  if any(i.eq.0) then
    indexs = ind(i.eq.0)
    r_g_b(:,indexs) = (/val(indexs),t(indexs),p(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.1) then
    indexs = ind(i.eq.1)
    r_g_b(:,indexs) = (/q(indexs),val(indexs),p(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.2) then
    indexs = ind(i.eq.2)
    r_g_b(:,indexs) = (/p(indexs),val(indexs),t(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.3) then
    indexs = ind(i.eq.3)
    r_g_b(:,indexs) = (/p(indexs),q(indexs),val(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.4) then
    indexs = ind(i.eq.4)
    r_g_b(:,indexs) = (/t(indexs),p(indexs),val(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.5) then
    indexs = ind(i.eq.5)
    r_g_b(:,indexs) = (/val(indexs),p(indexs),q(indexs)/)
    delete(indexs)
  end if
  if(any(ismissing(r_g_b)))
    print("hsv2rgb: some invalid HSV values were passed to hsv2rgb.")
  end if
  return(r_g_b(cmap_len|:,rgb|:))
end

;
; This function adds a cyclic point to a coordinate variable
; representing longitude.
;
; Input:  longitude array.
; Output: longitude array with cyclic point added.
;
function FixLongitudeCoord(lon[*])

local dims, newdata, ny, mx, mx1, sizes,lon,maxloninc,i,sign, \
      loninc,monotonic,nonmono_ix1, nonmono_ix2,llon
begin

  mx = dimsizes(lon)

  if (lon(1) - lon(0) .gt. 0)
    sign = 1.0
  else    
    sign = -1.0
  end if
    
  if (fabs(lon(mx-1) - lon(0)).ge. 360.)
    ; cyclic point already exists -- might be more points than we need
    return lon
  end if

  maxloninc = 0.0
  monotonic = True
  nonmono_ix1 = -1
  nonmono_ix2 = -1
  do i = 1, dimsizes(lon) - 1 
    loninc = (lon(i) - lon(i-1)) * sign
    if (loninc .lt. 0.0)
      monotonic = False
      if (nonmono_ix1.lt.0)
        nonmono_ix1 = i
      else
        nonmono_ix2 = i
      end if
    end if
    if (loninc.gt.maxloninc)
      maxloninc = loninc
    end if
  end do

  if (.not. monotonic)
    print("FixLongitudeCoord: not monotonic - not handled")
    return lon
  end if

  if (maxloninc + fabs(lon(mx-1) - lon(0)) .lt. 360)
    ; it's not a global dataset -- cyclical point not needed
    return lon
  end if

  if (typeof(lon).eq."double")
    newlon = new((/mx+1/),double)
  else
    newlon = new((/mx+1/),float)
  end if 
  newlon(0:mx-1) = lon
  newlon(mx) = newlon(0) + 360 * sign

  return newlon
end


;
; This function adds a cyclic point to the longitude dimension of a 
; 2-dimensional array called "data". The longitude dimension is assumed 
; to be the second dimension.
;
; Input:  2-dimensional data array.
; Output: 2-dimensional data array with second dimension size increased
;         by 1.
;
function FixLongitude(data[*][*])

local dims, newdata, nlat, mlon, mlon1, sizes,lon,maxloninc,i,sign, \
      loninc,monotonic,nonmono_ix1, nonmono_ix2
begin
  dims    = dimsizes(data)
  nlat    = dims(0)
  mlon    = dims(1)
  mlon1   = mlon+1

  if (ismissing(data!1) .or. .not. iscoord(data,data!1))
    return data
  end if

  lon = data&$data!1$ 

  if (lon(1) - lon(0) .gt. 0)
    sign = 1.0
    else    
    sign = -1.0
  end if
    
  if (fabs(lon(mlon-1) - lon(0)).ge. 360.)
    ; cyclic point already exists -- might be more points than we need
    return data
  end if

  maxloninc = 0.0
  monotonic = True
  nonmono_ix1 = -1
  nonmono_ix2 = -1
  do i = 1, dimsizes(lon) - 1 
    loninc = (lon(i) - lon(i-1)) * sign
    if (loninc .lt. 0.0)
      monotonic = False
      if (nonmono_ix1.lt.0)
        nonmono_ix1 = i
      else
        nonmono_ix2 = i
      end if
    end if
    if (loninc.gt.maxloninc)
      maxloninc = loninc
    end if
  end do

  if (.not. monotonic)
    print("FixLongitude: not monotonic - not handled")
    return data
  end if

  if (maxloninc + fabs(lon(mlon-1) - lon(0)) .lt. 360)
    ; it's not a global dataset -- cyclical point not needed
    return data
  end if

  if (typeof(data).eq."double")
     newdata = new((/nlat  ,mlon1/),double)
  else
     newdata = new((/nlat  ,mlon1/),float)
  end if

  newdata(:,0:mlon-1) = data             ; pass everything
  newdata(:,mlon)     = (/ data(:,0) /)  ; value only

  return(newdata)
end

;
; This function returns geopotential height (in km) given array p
; (pressure in mb). p must lie between 1013.25 mb and 0.010 mb.
; Algorithm is simply logarithmic interpolation from Standard
; Atmosphere.  Intended to provide an ESTIMATE of geopotential height
; when temperature and geopotential data are lacking.
;
 
function GeopHgt( pres[*]:float )
local nsa,psa,zsa,ptmp,npres,found,i,j,gph
begin
  if(isatt(pres,"units").and.(pres@units.eq."Pascals".or.pres@units.eq."Pa"))
    ptmp = pres * 0.01  ; Convert to mb
  else
    ptmp = 1.* pres     ; Assume already converted to mb!
  end if

  if ( .not. ( ( any(0.010.le.ptmp) ) .and. ( any(ptmp.le.1013.25) ) ) ) then
    print("GeopHgt: Fatal: Argument p (pressure) does not lie between")
    print("0.010 mb and 1013.25 mb.")
    return 1
  end if
 
  nsa = 40
  psa = new( (/nsa/), float, 1.e36 )
  zsa = new( (/nsa/), float, 1.e36 )
 
  zsa = (/ 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,      \
           5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0,          \
           11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, \
           25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 70.0, 80.0       /)
 
  psa = (/ 1013.25, 954.61, 898.76, 845.59, 795.01, 746.91, 701.21,    \
           657.80, 616.60, 577.52, 540.48, 505.39, 472.17, 440.75,     \
           411.05, 382.99, 356.51, 331.54, 308.00, 285.84, 264.99,     \
           226.99, 193.99, 165.79, 141.70, 121.11, 103.52, 88.497,     \
           75.652, 64.674, 55.293, 25.492, 11.970, 5.746, 2.871, 1.491,\
           0.798, 0.220, 0.052, 0.010 /)

  npres = dimsizes(ptmp)
  gph = new(npres,float)

  do i = 0,npres-1
    found = False
    j = 0

    do while(.not.found.and.j.le.nsa-2)
     if ( ( ptmp(i) .le. psa(j) ) .and. ( ptmp(i) .ge. psa(j+1) ) ) then
       gph(i) = zsa(j) + (zsa(j+1) - zsa(j)) * \
                log( psa(j)/ptmp(i) )/log( psa(j)/psa(j+1) )
       found = True 
     end if
     j = j + 1
    end do

  end do
 
  delete(psa)
  delete(zsa)
  delete(ptmp)
 
  return(gph)
end
 
;
; Add three subtitles to top of plot (left, center, and right).
;
; Input: plot to add titles to, three text items representing each title,
;        three strings representing the titles, and a max height and
;        height factor for calculating the font height.
;
; Output: 0 if okay, 1 if not.
;
function PlotTitles (plot : graphic, \
             left_obj   : graphic,   left_string : string, \
             center_obj : graphic, center_string : string, \
             right_obj  : graphic,  right_string : string, \
             max_height : float,   height_factor : float )
local vp_width,c1,c2,c3,height1,height2,height3,asp1,asp2,asp3,vpon1,vpon2,\
      vpon3,text1,text2,text3,tchars,\
      height,asp,sp_per_char,vpon,ret
begin

    ret = 0
;
; need check to see if these objects are what they are supposed to be
;
    getvalues plot
        "vpWidthF"  : vp_width
    end getvalues

    getvalues left_obj
        "txFontAspectF" : asp1
        "txFontHeightF" : height1
        "txString"      : text1
        "vpOn"          : vpon1
    end getvalues

    getvalues center_obj
        "txFontAspectF" : asp2
        "txFontHeightF" : height2
        "txString"      : text2
        "vpOn"          : vpon2
    end getvalues

    getvalues right_obj
        "txFontAspectF" : asp3
        "txFontHeightF" : height3
        "txString"      : text3
        "vpOn"          : vpon3
    end getvalues

;
;   uniform aspect and height enforced for these titles
;
    asp = (asp1 + asp2 + asp3) / 3.0
    c1 = stringtochar(left_string)
    c2 = stringtochar(center_string)
    c3 = stringtochar(right_string)
    tchars = sizeof(c1) + sizeof(c2) + sizeof(c3) + 3
;
; assume constant spacing since that takes the most room
;
    sp_per_char = vp_width / tchars
    
    height = min((/max_height,sp_per_char/)) * height_factor

    if (sizeof(c1).lt.2)
        if (vpon1 .ne. False)
            setvalues left_obj
                "vpOn" : False
            end setvalues
            ret = 1
        end if
    else
      if (.not. (asp1 .eq. asp .and. \
           height1 .eq. height .and. \
           text1 .eq. left_string .and. \
           vpon1 .eq. True))
        
        setvalues left_obj
            "txFontAspectF" : asp
            "txFontHeightF" : height
            "txString"      : left_string
            "vpOn"          : True
        end setvalues
        ret = 1
      end if
    end if

    if (sizeof(c2).lt.2)
        if (vpon2 .ne. False)
            setvalues center_obj
                "vpOn" : False
            end setvalues
            ret = 1
        end if
    else
      if (.not. (asp2 .eq. asp .and. \
           height2 .eq. height .and. \
           text2 .eq. center_string .and. \
           vpon2 .eq. True))
        
        setvalues center_obj
            "txFontAspectF" : asp
            "txFontHeightF" : height
            "txString"      : center_string
            "vpOn"          : True
        end setvalues
        ret = 1
      end if
    end if

    if (sizeof(c3).lt.2)
        if (vpon3 .ne. False)
            setvalues right_obj
                "vpOn" : False
            end setvalues
            ret = 1
        end if
    else
      if (.not. (asp3 .eq. asp .and. \
           height3 .eq. height .and. \
           text3 .eq. right_string .and. \
           vpon3 .eq. True))
    
        setvalues right_obj
            "txFontAspectF" : asp
            "txFontHeightF" : height
            "txString"      : right_string
            "vpOn"          : True
        end setvalues
        ret = 1
      end if
    end if

    return ret
end 

;
; This procedure forces tickmark labels to be the same size and
; points the tickmarks outward.
; 
; Input: the plot containing the tickmarks.
;
procedure UniformTickmarks(plot:graphic)

local xbheight0,xlength0,xmlength0,xoutl0,xminoutl0, \
      ylheight0,ylength0,ymlength0,youtl0,yminoutl0, \
      major_length, minor_length, height, ret
begin

; Get tickmark labels sizes and lengths.

  getvalues plot
    "tmXBLabelFontHeightF"    : xbheight0
    "tmXBMajorLengthF"        : xlength0
    "tmXBMinorLengthF"        : xmlength0
    "tmXBMajorOutwardLengthF" : xoutl0
    "tmXBMinorOutwardLengthF" : xminoutl0

    "tmYLLabelFontHeightF"    : ylheight0
    "tmYLMajorLengthF"        : ylength0
    "tmYLMinorLengthF"        : ymlength0
    "tmYLMajorOutwardLengthF" : youtl0
    "tmYLMinorOutwardLengthF" : yminoutl0
  end getvalues

;
; Make tickmark label sizes and lengths uniform sizes, and point tick
; marks outward.
;
  major_length = ( xlength0 +  ylength0) / 2.
  minor_length = (xmlength0 + ymlength0) / 2.
  height       = min((/xbheight0,ylheight0/))

  if (.not. (xbheight0 .eq. height .and. \
              ylength0 .eq. major_length .and. \
             xmlength0 .eq. minor_length .and. \
                xoutl0 .eq. major_length .and. \
             xminoutl0 .eq. minor_length .and. \
             ylheight0 .eq. height .and. \
              ylength0 .eq. major_length .and. \
             ymlength0 .eq. minor_length .and. \
                youtl0 .eq. major_length .and. \
             yminoutl0 .eq. minor_length))

    ret = 1
    setvalues plot
      "tmXBLabelFontHeightF"    : height
      "tmYLLabelFontHeightF"    : height
      "tmXBMajorLengthF"        : major_length
      "tmXBMinorLengthF"        : minor_length
      "tmYLMajorLengthF"        : major_length
      "tmYLMinorLengthF"        : minor_length
      "tmXBMajorOutwardLengthF" : major_length
      "tmXBMinorOutwardLengthF" : minor_length
      "tmYLMajorOutwardLengthF" : major_length  
      "tmYLMinorOutwardLengthF" : minor_length
    end setvalues
  end if
end

;
; This function returns a set of longitude values for placing major
; and minor tickmarks.
;
; Input: min/max longitude value, major and minor longitude spacing
;        to use (if spacings=0, then they are calculated in this function).
;
; Output: An array of "nice" longitude values, with the minor values
;         returned through the "minor" attribute.
;
function GetLonValues(min_lon:float,max_lon:float,major_lon_spacing:integer,\
                      minor_lon_spacing:integer)
local lon_range, lcheck_arr, lspcng_arr, mlspcng_arr, lon_spcng, \
      mlon_spcng, lon_ind, start_lon, end_lon
begin

; Initialize

  lcheck_arr  = (/ 20, 40, 60, 360/) ; lon range will determine
                                     ; spacing of minor/major ticks
  lspcng_arr  = (/  5, 10, 20, 30/)  ; spacings for major ticks
  mlspcng_arr = (/  1,  2,  5, 10/)  ; spacings for minor ticks

; Longitude range, will help determine longitude spacing.

  lon_range = max_lon - min_lon   ; lat/lon range

  lon_ind    = ind(lon_range.le.lcheck_arr)
  if(major_lon_spacing.le.0)
    lon_spcng  = lspcng_arr(lon_ind(0))  ; spacing for lon major tickmarks
  else
    lon_spcng = major_lon_spacing
  end if

  if(minor_lon_spacing.le.0)
    mlon_spcng = mlspcng_arr(lon_ind(0)) ; spacing for lon minor tickmarks
  else
    mlon_spcng = minor_lon_spacing
  end if
  delete(lon_ind)
  start_lon = (floattointeger(min_lon/lon_spcng)-1)*lon_spcng
  end_lon   = (floattointeger(max_lon/lon_spcng)+1)*lon_spcng

; Calculate values for major and minor lon tickmark locations.
  lon_values       = ispan(start_lon,end_lon,lon_spcng)
  lon_values@minor = ispan(min(lon_values),max(lon_values),mlon_spcng)
  return(lon_values)
end

;
; This function returns a set of "nice" longitude labels for labelling
; major and minor tickmarks.
;
; Input:  an array of "nice" longitude values where labels are desired.
;
; Output: an array of longitude labels (like "120W", "60E", and so on).
;
function GetLonLabels(lon_values:numeric)
local lon_index, lon_labels
begin
  lon_labels  = new(dimsizes(lon_values),string)

  lon_index = ind(lon_values.gt.180.and.lon_values.lt.360)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = 360-lon_values(lon_index) + "W" ; west
  end if
  delete(lon_index)

  lon_index = ind((lon_values).lt.0)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = fabs(lon_values(lon_index)) + "W"  ; west
  end if
  delete(lon_index)

  lon_index = ind(lon_values.gt.0.and.lon_values.lt.180)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = lon_values(lon_index) + "E"  ; east
  end if
  delete(lon_index)

  lon_index = ind(lon_values.gt.180)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = 360-lon_values(lon_index) + "W"  ; west
  end if
  delete(lon_index)

  lon_index = ind(lon_values.eq.0.or.fabs(lon_values).eq.180)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = fabs(lon_values(lon_index))      ; 0 or 180
  end if
  delete(lon_index)

  lon_index = ind(lon_values.eq.360)
  if(.not.all(ismissing(lon_index)))
      lon_labels(lon_index) = "0"
  end if
  delete(lon_index)

  return(lon_labels)
end

;
; This function returns a set of latitude values for placing major
; and minor tickmarks.
;
; Input: min/max latitude value, major and minor latitude spacing
;        to use (if spacings=0, then they are calculated in this function).
;
; Output: An array of "nice" latitude values, with the minor values returned
;         through the "minor" attribute.
;
function GetLatValues(min_lat:float,max_lat:float,major_lat_spacing:integer,\
                      minor_lat_spacing:integer)
local lat_range, lcheck_arr, lspcng_arr, mlspcng_arr, lat_spcng, mlat_spcng,\
lat_ind, start_lat, end_lat
begin

; Initialize

  lcheck_arr  = (/ 20, 40, 60, 180/) ; lat range will determine
                                     ; spacing of minor/major ticks
  lspcng_arr  = (/  5, 10, 20, 30/)  ; spacings for major ticks
  mlspcng_arr = (/  1,  2,  5, 10/)  ; spacings for minor ticks

; Latitude range, will help determine latitude spacing.

  lat_range = max_lat - min_lat   ; lat/lat range

  lat_ind    = ind(lat_range.le.lcheck_arr)
  if(major_lat_spacing.le.0)
    lat_spcng = lspcng_arr(lat_ind(0))  ; spacing for lat major tickmarks
  else
    lat_spcng = major_lat_spacing
  end if

  if(minor_lat_spacing.le.0)
    mlat_spcng = mlspcng_arr(lat_ind(0)) ; spacing for lat minor tickmarks
  else
    mlat_spcng = minor_lat_spacing
  end if
  delete(lat_ind)
  
  start_lat = (floattointeger(min_lat/lat_spcng)-1)*lat_spcng
  end_lat = (floattointeger(max_lat/lat_spcng)+1)*lat_spcng

; Calculate values for major and minor lat tickmark locations.
  lat_values       = ispan(start_lat,end_lat,lat_spcng)
  lat_values@minor = ispan(min(lat_values),max(lat_values),mlat_spcng)
  return(lat_values)
end

;
; This function returns a set of latitude labels for labelling
; major and minor tickmarks
;
; Input:  an array of latitude values where labels are desired.
;
; Output: array of latitude labels (labels like "90S", "30N", and so on).
;
function GetLatLabels(lat_values:numeric)
local lat_index, lat_labels
begin

; Create labels for latitude tick marks.
  lat_labels = new(dimsizes(lat_values),string)

  lat_index = ind(lat_values.lt.0)
  if(.not.all(ismissing(lat_index)))
     lat_labels(lat_index) = fabs(lat_values(lat_index)) + "S"    ; south
  end if
  delete(lat_index)

  lat_index = ind(lat_values.gt.0)
  if(.not.all(ismissing(lat_index)))
    lat_labels(lat_index) = lat_values(lat_index) + "N"    ; north
  end if
  delete(lat_index)

  lat_index = ind(lat_values.eq.0)
  if(.not.all(ismissing(lat_index)))
    lat_labels(lat_index) = lat_values(lat_index)          ; equator
  end if
  delete(lat_index)
 
  return(lat_labels)
end

;
; This function adds lat/lon labels to a rectangular map projection.
;
; Input: the map object to add the labels to, the loglin object that
;        will contain the labels.
;
; Output: 0 if okay, 1 if not.
;
function MapTickmarks (map : graphic, tickmarks : graphic)
    
local vpxf,vpyf,vpwf,vphf,min_lon,max_lon,min_lat,max_lat,center_lon, \
      projection,lon_range,lat_range,lcheck_arr,lspcng_arr,mlspcng_arr, \
      lon_ind,lon_spcng,mlon_spcng,lat_ind,lat_spcng,mlat_spcng,offset, \
      yvalues,myvalues,ylabels,xvalues,xlabels,mxvalues,indexx, quot, \
      major_length,minor_length,int_min_lat,int_min_lon,sign,round_val,tval,\
      center_lat,center_rot,xmin0,ymin0,xmax0,ymax0,xbmode0,ylmode0, \
      xlabels0,ylabels0,xvalues0,yvalues0,mxvalues0,myvalues0,height,\
      xbheight0,ylheight0,xlength0,ylength0,xmlength0,ymlength0,\
      xoutl0,youtl0,xminoutl0,yminoutl0,ret
begin
  ret = 0
  getvalues map
    "vpXF"         : vpxf
    "vpYF"         : vpyf
    "vpWidthF"     : vpwf
    "vpHeightF"    : vphf
    "mpMinLonF"    : min_lon
    "mpMaxLonF"    : max_lon
    "mpMinLatF"    : min_lat
    "mpMaxLatF"    : max_lat
    "mpCenterLonF" : center_lon
    "mpCenterLatF" : center_lat
    "mpCenterRotF" : center_rot
    "mpProjection" : projection
  end getvalues

  if (projection.ne.8 .or. center_lat.ne.0.0 .or. center_rot.ne.0.0 )
    setvalues tickmarks
      "vpOn" : False
    end setvalues
  else
    setvalues tickmarks
      "vpOn" : True
    end setvalues
  end if

  if (fabs(max_lon - min_lon) .ge. 360)
    tval = (max_lon + min_lon) / 2.0 
    quot = floattointeger(tval / 180.0)
    offset = tval - (quot * 180.0)
    if (offset.gt.0.0)
      max_lon = max_lon - offset
      min_lon = min_lon - offset
    end if
  end if
    
  lon_range = max_lon - min_lon   ; longitude range
  lat_range = max_lat - min_lat   ; latitude range

; Calculate longitude/latitude values to label.

  lcheck_arr  = (/ 20,  40,  60, 360/)  ; lat/lon range will determine
                                        ; spacing of minor/major ticks
  lspcng_arr  = (/  5,  10,  20,  30/)  ; spacings for major ticks
  mlspcng_arr = (/  1,   2,   5,  10/)  ; spacings for minor ticks

  lon_ind    = ind(lon_range.le.lcheck_arr)
  lon_spcng  = lspcng_arr(lon_ind(0))  ; spacing for lon major tickmarks
  mlon_spcng = mlspcng_arr(lon_ind(0)) ; spacing for lon minor tickmarks

  lat_ind    = ind(lat_range.le.lcheck_arr)
  lat_spcng  = lspcng_arr(lat_ind(0))  ; spacing for lat major tickmarks
  mlat_spcng = mlspcng_arr(lat_ind(0)) ; spacing for lat minor tickmarks


  if (lat_ind(0).eq.3)
  round_val = 10
  tval = 5
  else 
  if (lat_ind(0).eq.2)
    round_val = 10
    tval = 5
  else 
    if (lat_ind(0).eq.1)
      round_val = 5
      tval = 3
    else
      round_val = 1
      tval = 1
      end if
    end if
  end if

  int_min_lat = floattointeger(min_lat)
  if (int_min_lat.lt.0)
    sign = -1
  else
    sign = 1
  end if

  if (((sign * int_min_lat) % round_val).ge.tval)
    int_min_lat = round_val * (int_min_lat / round_val) + sign * round_val
  else
    int_min_lat = round_val * (int_min_lat / round_val)
  end if  

  if (lon_ind(0).eq.3)
    round_val = 10
    tval = 5
  else 
    if (lon_ind(0).eq.2)
      round_val = 10
      tval = 5
    else 
      if (lon_ind(0).eq.1)
        round_val = 5
        tval = 3
      else
        round_val = 1
        tval = 1
      end if
    end if
  end if

  int_min_lon = floattointeger(min_lon)
  if (int_min_lon.lt.0)
    sign = -1
  else
    sign = 1
  end if

  if (((sign * int_min_lon) % round_val).ge.tval)
    int_min_lon = round_val * (int_min_lon / round_val) + sign * round_val
  else
    int_min_lon = round_val * (int_min_lon / round_val)
  end if  

; Calculate values for latitude minor/major tickmark locations.
  yvalues  = ispan(int_min_lat,\
                   floattointeger(max_lat),lat_spcng)
  myvalues = ispan(int_min_lat,\
                   floattointeger(max_lat),mlat_spcng)

; Create labels for latitude tick marks.
  ylabels  = GetLatLabels(yvalues)

; Calculate values for longitude major tickmark locations.
  xvalues = ispan(int_min_lon,\
                  floattointeger(max_lon),lon_spcng)

; Convert to (-180,180) range if necessary.
  if(any(xvalues.gt.180))
    xvalues = xvalues-180
    xvalues = ispan(min(xvalues),max(xvalues),lon_spcng)
    min_lon = min_lon - 180
    max_lon = max_lon - 180
  end if

; Calculate values for longitude minor tickmark locations.
  mxvalues = ispan(floattointeger(min(xvalues)),\
                   floattointeger(max(xvalues)),mlon_spcng)

; If the lon range is 360, then it is possible to have a center longitude
; that is not equal to 0. If this is the case, then the lon labels need 
; to change accordingly.

  xvalues2 = xvalues + center_lon
  if(lon_range.eq.360.and.center_lon.gt.0)
    indexx = ind(xvalues2.gt.180)
    xvalues2(indexx) = xvalues2(indexx) - 360
    delete(indexx)
  end if
  if(lon_range.eq.360.and.center_lon.lt.0)
    indexx = ind(xvalues2.lt.-180)
    xvalues2(indexx) = xvalues2(indexx) + 360
    delete(indexx)
  end if

  xlabels  = GetLonLabels(xvalues2)

; Get some tickmark resources from the lat/lon grid.

  getvalues tickmarks
     "trXMinF"      : xmin0
     "trXMaxF"      : xmax0
     "trYMinF"      : ymin0
     "trYMaxF"      : ymax0

     "tmXBMode"         : xbmode0
     "tmXBLabels"       : xlabels0
     "tmXBValues"       : xvalues0
     "tmXBMinorValues"  : mxvalues0

     "tmYLMode"         : ylmode0
     "tmYLLabels"       : ylabels0
     "tmYLValues"       : yvalues0
     "tmYLMinorValues"  : myvalues0
  end getvalues

  if (.not. (xmin0 .eq. min_lon .and. \
             xmax0 .eq. max_lon .and. \
             ymin0 .eq. min_lat .and. \
             ymax0 .eq. max_lat .and. \
           xbmode0 .eq. 2 .and. \             ; "Explicit" == 2
           dimsizes(xlabels0).eq.dimsizes(xlabels) .and. \
           dimsizes(xvalues0).eq.dimsizes(xvalues) .and. \
           dimsizes(mxvalues0).eq.dimsizes(mxvalues) .and. \
           all(xlabels0 .eq. xlabels) .and. \
           all(xvalues0 .eq. xvalues) .and. \
           all(mxvalues0 .eq.mxvalues) .and. \
           ylmode0 .eq. 2 .and. \
           dimsizes(ylabels0).eq.dimsizes(ylabels) .and. \
           dimsizes(yvalues0).eq.dimsizes(yvalues) .and. \
           dimsizes(myvalues0).eq.dimsizes(myvalues) .and. \
           all(ylabels0 .eq. ylabels) .and. \
           all(yvalues0 .eq. yvalues) .and. \
           all(myvalues0 .eq.myvalues)))

      ret = 1
; Set some tickmark resources from the lat/lon grid.
      setvalues tickmarks
         "trXMinF"      : min_lon
         "trXMaxF"      : max_lon
         "trYMinF"      : min_lat
         "trYMaxF"      : max_lat

         "tmXBMode"         : "Explicit" ; Indicate that we want to 
         "tmXBLabels"       : xlabels    ; explicitly label the X axis.
         "tmXBValues"       : xvalues
         "tmXBMinorValues"  : mxvalues

         "tmYLMode"         : "Explicit" ; Indicate that we want to 
         "tmYLLabels"       : ylabels    ; explicitly label the Y axis.
         "tmYLValues"       : yvalues
         "tmYLMinorValues"  : myvalues
      end setvalues
    end if

; Force tickmarks to be same length and pointing outwards

  UniformTickmarks(tickmarks)

  return ret
end

;
; This function labels longitude values on an elliptical polar
; map projection.
;
; Input: the map object to add the labels to, a flag indicating
;        whether labelling northern (0) or southern (not 0) hemisphere.
;
; Output: 0 if okay, 1 if not.
;
function MapPolarTickmarks (map:graphic, polar:string,lon_objs:graphic,\
                            font_scale:float)
local min_lat, max_lat, ngrid, plat, plon, am_ids, lon_spacing, i, wks, \
      font_height, xndc, yndc, labels, num_labels, text_ids, side, just, \
      orth, para, vpxf, vpyf, vpwf, vphf, quad1, quad2, quad3, quad4, \
      topbot, xcen, ycen
begin

  if (.not. isatt(map,"ndvWks"))
    print("set_polar_labels: error creating map labels, no workstation specified")
    return 1
  else
    wks = map@ndvWks      ; Get workstation id
  end if

;
; Retrieve some map resources.
;
  getvalues map
    "mpMinLatF"         : min_lat
    "mpMaxLatF"         : max_lat
    "mpGridLonSpacingF" : lon_spacing
    "vpXF"              : vpxf
    "vpYF"              : vpyf
    "vpWidthF"          : vpwf
    "vpHeightF"         : vphf
  end getvalues

;
; Get # of longitude labels to be created.
;
  ngrid = floattoint(360./lon_spacing)
  if(ngrid.ne.dimsizes(lon_objs)) 
    print("set_polar_labels: error creating map labels")
  end if
;
; Get lat/lon positions for lon labels.  For a polar plot, the lat values
; stay the same as you march around the circle and get the lon values.
;
  if(min((/min_lat,max_lat/)).ge.0.or.polar.eq."NH")
    plat = fspan(min_lat,min_lat,ngrid)
  else
    plat = fspan(max_lat,max_lat,ngrid)
  end if 
  plon = fspan(0.,360.-lon_spacing,ngrid)

  xcen = vpxf + vpwf/2.     ; X center
  ycen = vpyf - vphf/2.     ; Y center

; Compute the font height to be used for label sizes.

  font_height = 0.02 * vphf * font_scale

; Create arrays to hold the NDC values that we're going to convert 
; the lat/lon values to.

  xndc = new(dimsizes(plon),float)
  yndc = new(dimsizes(plat),float)

; Convert lat/lon coordinates to NDC coordinates since we have to
; draw the labels in NDC space and not in lat/lon space.

  datatondc(map,plon,plat,xndc,yndc)   ; Get NDC values of lon labels that
                                       ; are right on map circle.
  labels = GetLonLabels(plon)          ; Get an array of strings to label
                                       ; the longitude values.
  num_labels = dimsizes(labels)        ; # of labels

; Set some resources in TextItem objects that we'll use to hold the
; longitude labels.

  do i=0,num_labels-1
    setvalues lon_objs(i)
      "txString"      : labels(i)
      "txFontHeightF" : font_height
    end setvalues
  end do

; Retrieve the ids of the AnnoManager objects created by the PlotManager
; and then set their location in data coordinate space. The AnnoManager
; objects are arranged in the same order as the TextItems in the
; pmAnnoViews resource.

  getvalues map
    "pmAnnoManagers" : am_ids
  end getvalues

;
; Determine which quadrant each label is in, and then set values
; for justification, side, orthogonal and parallel positions.
;

  quad1 = ind((ycen-yndc).le.0.and.(xcen-xndc).le.0)
  quad2 = ind((ycen-yndc).le.0.and.(xcen-xndc).gt.0)
  quad3 = ind((ycen-yndc).gt.0.and.(xcen-xndc).gt.0)
  quad4 = ind((ycen-yndc).gt.0.and.(xcen-xndc).le.0)

  side = new(ngrid,string)
  just = new(ngrid,string)
  orth = new(ngrid,float)
  para = new(ngrid,float)

  side(quad1) = "Top"      ; Position labels in quadrants I and II wrt
  side(quad2) = "Top"      ; top of plot.
  side(quad3) = "Bottom"   ; Position labels in quadrants III and IV wrt
  side(quad4) = "Bottom"   ; bottom of plot.

;
; The zone is being set to 0, so this means all labels will appear in the
; center of the map by default.  We set the orthogonal and parallel values
; to move the labels away from the center of the plot.  The orth/para values
; are a fraction of the total width of the map, so if you set an orthogonal
; value to 0.5, this will move the label from the center of the plot to the
; edge of the plot.
;
; We increase the orth/para values by 4 percent so that the labels are
; placed outside of the plot.
;
  orth(quad1) = ((yndc(quad1) - ycen)/vphf) * 1.04
  orth(quad2) = ((yndc(quad2) - ycen)/vphf) * 1.04
  orth(quad3) = ((ycen - yndc(quad3))/vphf) * 1.04
  orth(quad4) = ((ycen - yndc(quad4))/vphf) * 1.04

  para = ((xndc - xcen)/vpwf) * 1.04

  just(quad1) = "CenterLeft"
  just(quad2) = "CenterRight"
  just(quad3) = "CenterRight"
  just(quad4) = "CenterLeft"
;
; Labels at the very top or bottom of the map should be centered, not
; right or left justified.
;
  topbot = ind(fabs(xndc-xcen).le.1e-5)
  if(.not.all(ismissing(topbot)))
    just(topbot) = "CenterCenter"
  end if

  do i=0,num_labels-1
;
; Because of the way the plot style is set up, the first 3 am_ids are
; the 3 subtitles at top, so start at index 3.
;
    setvalues am_ids(i+3)
      "amTrackData"      : False
      "amResizeNotify"   : True
      "amZone"           : 0
      "amSide"           : side(i)
      "amJust"           : just(i)
      "amOrthogonalPosF" : orth(i)
      "amParallelPosF"   : para(i)
    end setvalues
  end do

  return 0
end

;
; This function returns an array of color indexes that spans the full
; length of the color map. The number of color indexes returned is
; equal to the number of contour or vector levels.
;
; Input: The contour or vector plot object, the starting color index,
;        and the ending color index (a -1 indicates to use the full color
;        map, a -2 indicates not to include the last color, and so on)
;
; Output: An integer array of color indexes.
;
function SpreadColors (plot : graphic, \
            start_color_index : integer, \
            end_color_index : integer)

local ncols,lcount,fcols,icols,stride,rem,start,minix,maxix,nc,fmin,fmax

begin
  if (ismissing(plot))
    return 0
  end if

  if (isatt(plot,"ndvClass"))
    class = plot@ndvClass
    if (class.eq."contourPlotClass")
      colres = "cnFillColors"
      levelcountres = "cnLevelCount"
    else
      if (class.eq."vectorPlotClass")
        colres = "vcLevelColors"
        levelcountres = "vcLevelCount"
      else
        return 0
      end if
    end if
  else
    return 0
  end if
  if (.not. isatt(plot,"ndvWks"))
    print("SpreadColors: returning original color indexes")
    getvalues plot
      colres : icols
    end getvalues
    return icols
  end if

  getvalues plot@ndvWks
    "wkColorMapLen" : ncols
  end getvalues
  getvalues plot
    levelcountres : lcount
  end getvalues

;
; -1 indicates that end_color_index should be set equal to ncols - 1
;
  maxix = new(1,integer)
  minix = new(1,integer)
  maxix = end_color_index
  minix = start_color_index

  if (maxix .lt. 0)
    maxix = ncols + maxix
  end if

  if (maxix .le. minix .or. minix .lt. 0)
    print("SpreadColors: invalid parameters: defaulting")
    maxix = ncols - 1
    minix = 2
  end if

  minix = max((/0,minix/))
  maxix = min((/ncols - 1,maxix/))
  nc = maxix - minix + 1

  fmin = new(1,float)
  fmax = new(1,float)

  fmin = minix
  fmax = maxix
  fcols = fspan(fmin,fmax,lcount+1)
  icols = floattointeger(fcols + 0.5)

  return icols
end

;
; This function defines a colormap in the given workstation.
;
; Input: the plot object containing the workstation and the color map.
;
; Output: 0 if okay, 1 if not.
;
function SetColormap (plot : graphic, colormap)
begin
  if (.not. isatt(plot,"ndvWks"))
    print("SetColormap: using default color map")
    return 1
  end if

  if(all(colormap.ne.""))
    setvalues plot@ndvWks
      "wkColorMap" : colormap
    end setvalues
  end if
  return 0
end

;
; This function sets the contour levels given the contour object,
; the min and max contour levels, and the contour level spacing.
;
; Input: the contour plot, min and max contour level values, contour 
;        level spacing.
;
; If level_spacing is <= 0, then this routine doesn't do anything.
; This allows the routine to be available to the user only if
; he/she needs it.  If min_level=max_level, then only the level spacing 
; is set.
;
; Output: 0 if okay, 1 if not.
;
function SetContourLevels(plot:graphic,min_cont_level:float, \
                          max_cont_level:float,cont_level_spacing:float)
local class_name
begin

  if(cont_level_spacing.le.0.)
    return 0     ; Don't do anything if invalid level spacing
  end if

  class_name = NhlClassName(plot)
  if(class_name.ne."contourPlotClass")
    print("SetContourLevels: the first argument must be a contour plot object.")
    return 1
  else
    getvalues plot
      "cnLevels"   : cnlvls   ; Retrieve contour levels
    end getvalues
  end if
  
  if(min_cont_level.gt.max_cont_level)
    print("SetContourLevels: the minimum contour level must be less than")
    print("the maximum contour level.")
    return 1
  end if

  if(min_cont_level.eq.max_cont_level)
    setvalues plot
      "cnLevelSpacingF"       : cont_level_spacing
    end setvalues
  else
    setvalues plot
      "cnLevelSelectionMode"  : "ManualLevels"
      "cnLevelSpacingF"       : cont_level_spacing
      "cnMinLevelValF"        : min_cont_level
      "cnMaxLevelValF"        : max_cont_level
    end setvalues
  end if
  return 0
end

;
; This function "fixes" zero-th contour level. That is, it sets all
; contour levels within epsilon of 0 and forces them to be exactly 0.
;
; Input: an array of contour levels
;
; Output: an array of contour levels with the 0-th level fixed.
;
function FixZeroContour (contour_levels[*]:float)
local eps, indEps
begin
  eps    = 1.e-09                          ; arbitrary
  indEps=ind(fabs(contour_levels).le.eps) 
  if (.not.ismissing(indEps))
    contour_levels(indEps) = 0.0        ; the "zero" line ==>-0.8e-09
  end if
  return (contour_levels)                                      
end

;
; This function defines the resources mpDefaultFillColor, mpOceanFillColor,
; mpLandFillColor, and mpInlandWaterFillColor using color indices or named
; colors.  It is necessary to call this function *after* the color map
; is set, so the named colors get the correct color indices.
;
; Input: the map plot object and the four colors (as indices or named
;        colors).
;
; Output: 0 if okay, 1 if not.
;
function SetMapFillColors(plot : graphic, default_fill_color, \
                ocean_fill_color, land_fill_color, inland_water_fill_color)
begin
  class_name = NhlClassName(plot)
  if(class_name.ne."mapPlotClass")
    print("SetMapFillColors: the first argument must be a map plot object.")
    return 1
  end if

  setvalues plot
    "mpDefaultFillColor"     : default_fill_color
    "mpOceanFillColor"       : ocean_fill_color
    "mpLandFillColor"        : land_fill_color
    "mpInlandWaterFillColor" : inland_water_fill_color
  end setvalues

  return 0
end


;
; This function sets the dash patterns of contour levels above
; and below the given contour level values.
;
; Input: a contour plot, a contour level and a dash pattern for rendering
;        all contours lines below this level, and a contour level and a 
;        dash pattern for rendering all contour lines above this level.
;
; If either of the dash patterns are less than zero, then nothing is
; done.  This allows the routine to be available to the user only if
; he/she needs it.
;
; Output: 0 if okay, 1 if not.
;
function LtGtContourDashPattern (plot:graphic, \
                          Contour_Level_LT:float, Dash_Pattern_LT:integer,\
                          Contour_Level_GT:float, Dash_Pattern_GT:integer ) 
local class_name, cnlvls, n, N, cnlinepat, pat_index
begin

  if(Dash_Pattern_LT.lt.0.and.Dash_Pattern_GT.lt.0)
    return 0    ; Don't do anything if invalid dash patterns.
  end if

  class_name = NhlClassName(plot)
  if(class_name.ne."contourPlotClass")
    print("LtGtContourDashPattern: the first argument must be a contour plot object.")
    return 1
  else
    getvalues plot
      "cnLevels"   : cnlvls   ; Retrieve contour levels
    end getvalues
  end if

  N = dimsizes(cnlvls)
  if (ismissing(N) .or. N.eq.0)
    print ("LtGtContourDashPattern: dimsizes(cnlvls)="+N)
    return 0
  else 
    cnlvls = FixZeroContour (cnlvls)  ; Fix the 0th contour level.
  end if

  cnlinepat = new (dimsizes(cnlvls), integer) ; line pattern vector
  cnlinepat = 0                               ; default is solid (=0)

  if ((any(cnlvls.lt.Contour_Level_LT).and.Dash_Pattern_LT.ge.0).or.\
      (any(cnlvls.gt.Contour_Level_GT).and.Dash_Pattern_GT.ge.0))

    if(Dash_Pattern_LT.gt.0)
      pat_index = ind(cnlvls.lt.Contour_Level_LT)
      if(.not.all(ismissing(pat_index)))
        cnlinepat(pat_index) = Dash_Pattern_LT  ; dsh pat for levels < Contour_Level_LT
      end if
      delete(pat_index)
    end if

    if(Dash_Pattern_GT.gt.0)
      pat_index = ind(cnlvls.gt.Contour_Level_GT)
      if(.not.all(ismissing(pat_index)))
        cnlinepat(pat_index) = Dash_Pattern_GT  ; dsh pat for levels > Contour_Level_GT
      end if
      delete(pat_index)
    end if
;
; Set the dash patterns.
;
    setvalues plot
      "cnMonoLineDashPattern"  : False
      "cnLineDashPatterns"     : cnlinepat
    end setvalues
  end if

  return 0
end

;
; This function sets the fill patterns of contour levels above
; and below the given contour level values.
;
; Input: a contour plot, a contour level and a fill pattern/fill scale for
;        filling all contours lines below this level, and a contour level
;        and a fill pattern/scale for filling all contour lines above this
;        level.
;
; If both of the fill patterns are less than -1, then nothing is
; filled.  This allows the routine to be available to the user only if
; he/she needs it.  A fill pattern of -1 is transparent, and a fill pattern
; of 0 is solid.  Fill patterns 2-17 are various shadings.
;
; Output: 0 if okay, 1 if not.
;
function LtGtContourFillPattern (plot:graphic,\
     Contour_Level_LT:float,Fill_Pattern_LT:integer,Fill_Scale_LT:float,\
     Contour_Level_GT:float,Fill_Pattern_GT:integer,Fill_Scale_GT:float ) 
local class_name, cnlvls, N, patterns, scales, fill_index
begin

  if(Fill_Pattern_LT.lt.-1.and.Fill_Pattern_GT.lt.-1)
    return 0    ; Don't do anything if invalid fill patterns.
  end if

  class_name = NhlClassName(plot)
  if(class_name.ne."contourPlotClass")
    print("LtGtContourFillPattern: the first argument must be a contour plot object.")
    return 1
  else
    getvalues plot
      "cnLevels"   : cnlvls   ; Retrieve contour levels
    end getvalues
  end if

  N = dimsizes(cnlvls)
  if (ismissing(N) .or. N.eq.0) then
    print ("LtGtContourFillPattern: dimsizes(cnlvls)="+N)
    return 1
  else 
    cnlvls = FixZeroContour (cnlvls)  ; Fix the 0th contour level.
  end if

  patterns  = new(dimsizes(cnlvls)+1,integer) ; Create array for fill 
  patterns(:)  = -1                           ; patterns and initialize
                                              ; it to transparent.

  scales  = new(dimsizes(cnlvls)+1,float) ; Create array for fill
  scales(:)  = 1.0                        ; pattern scales and initialize
                                          ; it to 1.
  if ((any(cnlvls.lt.Contour_Level_LT).and.Fill_Pattern_LT.ge.-1).or. \
      (any(cnlvls.gt.Contour_Level_GT).and.Fill_Pattern_GT.ge.-1)) then

    if(Fill_Pattern_LT.ge.-1)              ; If pattern < 0, don't use
      fill_index = ind(cnlvls.le.Contour_Level_LT)
      if(.not.all(ismissing(fill_index)))
        patterns(fill_index) = Fill_Pattern_LT
        scales(fill_index)   = Fill_Scale_LT 
      end if
      delete(fill_index)
    end if

    if(Fill_Pattern_GT.ge.-1)              ; If pattern < 0, don't use
      fill_index = ind(cnlvls.gt.Contour_Level_GT)
      if(.not.all(ismissing(fill_index)))
        patterns(fill_index) = Fill_Pattern_GT
        scales(fill_index)   = Fill_Scale_GT 
      end if
      patterns(N)  = Fill_Pattern_GT        ; last pattern
      scales(N)    = Fill_Scale_GT          ; last scale
      delete(fill_index)
    end if
;
; Set the fill patterns and the fill scales.
;
    setvalues plot
      "cnFillOn"           : True
      "cnMonoFillColor"    : True
      "cnMonoFillPattern"  : False
      "cnMonoFillScale"    : False
      "cnFillScales"       : scales
      "cnFillPatterns"     : patterns
    end setvalues
  end if
 
  return 0
end

;
; This function changes the line thickness of a given contour level.
;
; Input: a contour plot, a contour level, and a line thickness.
;
; Output: 0 if okay, 1 if not.
;
; If the line thickness is <= 0, then this routine doesn't do anything.
; This allows the routine to be available to the user only if
; he/she needs it.
;
function SetContourLevelThickness (plot:graphic, contour_level:float, \
                                   contour_level_line_thickness: float) 
local class_name, contour_levels, n, N, thk_index, cnlinethk
begin

  if(contour_level_line_thickness.le.0.)
    return 0              ; Just ignore if invalid line thickness.
  end if

;
; Make sure dealing with a contour plot.
;
  class_name = NhlClassName(plot)
  if(class_name.ne."contourPlotClass")
    print("SetContourLevelThickness: the first argument must be a contour plot object.")
    return 1
  else
    getvalues plot
      "cnLevels"   : contour_levels   ; Retrieve contour levels
    end getvalues
  end if

  N = dimsizes(contour_levels)
  if (ismissing(N) .or. N.eq.0)
    print ("SetContourLevelThickness: dimsizes(contour_levels)="+N)
    return 0
  else 
    contour_levels = FixZeroContour (contour_levels) ; Fix the 0-th contour level.
  end if

  if (any(contour_levels.eq.contour_level))
    cnlinethk = new (dimsizes(contour_levels), float) ; line thick vector
    cnlinethk = 1.                            ; default

    thk_index = ind(contour_levels.eq.contour_level)
    cnlinethk(thk_index) = contour_level_line_thickness  ; change thickness
                                                         ; of this level

    setvalues plot
      "cnMonoLineThickness"   : False
      "cnLineThicknesses"     : cnlinethk
    end setvalues
  end if

  return 0
end

;
; This function puts "nice" longitude labels on the X axis of a
; non-map object.
;
; Input: a plot object, the major/minor longitude spacing to use.
;
; Output: 0 if okay, 1 if not.
;
function LabelLon (plot : graphic,major_lon_spacing:integer,\
                                  minor_lon_spacing:integer)
local min_lon,max_lon, lon_values,lon_labels,major_length, \
      minor_length, xbmode0, lon_labels0,lon_values0, ret 
begin
    
  ret = 0
  getvalues plot
    "trXMinF"      : min_lon
    "trXMaxF"      : max_lon
  end getvalues

; Get longitude labels and tickmark locations.

  lon_values = GetLonValues(min_lon,max_lon,major_lon_spacing,\
                                            minor_lon_spacing)
  lon_labels = GetLonLabels(lon_values)

; Get some tickmark resources

  getvalues plot
    "tmXBMode"         : xbmode0
    "tmXBLabels"       : lon_labels0
    "tmXBValues"       : lon_values0
    "tmXBMinorValues"  : mlon_values0
  end getvalues

  if (.not. (xbmode0 .eq. 2 .and. \             ; "Explicit" == 2
      dimsizes(lon_labels0).eq.dimsizes(lon_labels) .and. \
      dimsizes(lon_values0).eq.dimsizes(lon_values) .and. \
      dimsizes(mlon_values0).eq.dimsizes(lon_values@minor) .and. \
      all(lon_labels0 .eq. lon_labels) .and. \
      all(lon_values0 .eq. lon_values) .and. \
      all(mlon_values0 .eq.lon_values@minor)))
    ret = 1
    setvalues plot
      "tmXBMode"         : "Explicit"  ; Indicate that we want to 
      "tmXBLabels"       : lon_labels  ; explicitly label the X axis.
      "tmXBValues"       : lon_values
      "tmXBMinorValues"  : lon_values@minor
    end setvalues
  end if

  UniformTickmarks(plot)  ; Force tickmarks to be same length 
                                 ; and pointing outwards.
  return ret
end

;
; This function puts "nice" latitude labels on the X axis of a
; non-map object.
;
; Input: a plot object, the major/minor latitude spacing to use.
;
; Output: 0 if okay, 1 if not.
;
function LabelLatX (plot : graphic,major_lat_spacing:integer, \
                                   minor_lat_spacing:integer)
local min_lat, max_lat, lat_values, lat_labels,\
      major_length, minor_length, xbmode0, lat_labels0, lat_values0, ret
begin
    
  ret = 0
  getvalues plot
    "trXMinF"      : min_lat
    "trXMaxF"      : max_lat
  end getvalues

; Get latitude labels and tickmark locations.

  lat_values = GetLatValues(min_lat,max_lat,major_lat_spacing, \
                                            minor_lat_spacing)
  lat_labels = GetLatLabels(lat_values)

; Get some tickmark resources

  getvalues plot
    "tmXBMode"         : xbmode0
    "tmXBLabels"       : lat_labels0
    "tmXBValues"       : lat_values0
    "tmXBMinorValues"  : mlat_values0
  end getvalues

  if (.not. (xbmode0 .eq. 2 .and. \             ; "Explicit" == 2
      dimsizes(lat_labels0).eq.dimsizes(lat_labels) .and. \
      dimsizes(lat_values0).eq.dimsizes(lat_values) .and. \
      dimsizes(mlat_values0).eq.dimsizes(lat_values@minor) .and. \
      all(lat_labels0 .eq. lat_labels) .and. \
      all(lat_values0 .eq. lat_values) .and. \
      all(mlat_values0 .eq.lat_values@minor)))
    ret = 1
    setvalues plot
      "tmXBMode"         : "Explicit"   ; Indicate that we want to 
      "tmXBLabels"       : lat_labels   ; explicitly label the X axis.
      "tmXBValues"       : lat_values
      "tmXBMinorValues"  : lat_values@minor
    end setvalues
  end if

  UniformTickmarks(plot)  ; Force tickmarks to be same length 
                                 ; and pointing outwards.
  return ret
end

;
; This function puts "nice" latitude labels on the Y axis of a
; non-map object.
;
; Input: a plot object, the major/minor latitude spacing to use.
;
; Output: 0 if okay, 1 if not.
;
function LabelLatY (plot : graphic,major_lat_spacing:integer, \
                                   minor_lat_spacing:integer)
local min_lat, max_lat, lat_values, lat_labels,\
      major_length, minor_length, ylmode0, lat_labels0, lat_values0, ret
begin
    
  ret = 0
  getvalues plot
    "trYMinF"      : min_lat
    "trYMaxF"      : max_lat
  end getvalues

; Get latitude labels and tickmark locations.

  lat_values = GetLatValues(min_lat,max_lat,major_lat_spacing, \
                                            minor_lat_spacing)
  lat_labels = GetLatLabels(lat_values)

; Get some tickmark resources

  getvalues plot
    "tmYLMode"         : ylmode0
    "tmYLLabels"       : lat_labels0
    "tmYLValues"       : lat_values0
    "tmYLMinorValues"  : mlat_values0
  end getvalues

  if (.not. (ylmode0 .eq. 2 .and. \             ; "Explicit" == 2
      dimsizes(lat_labels0).eq.dimsizes(lat_labels) .and. \
      dimsizes(lat_values0).eq.dimsizes(lat_values) .and. \
      dimsizes(mlat_values0).eq.dimsizes(lat_values@minor) .and. \
      all(lat_labels0 .eq. lat_labels) .and. \
      all(lat_values0 .eq. lat_values) .and. \
      all(mlat_values0 .eq.lat_values@minor)))
    ret = 1
    setvalues plot
      "tmYLMode"         : "Explicit"   ; Indicate that we want to 
      "tmYLLabels"       : lat_labels   ; explicitly label the Y axis.
      "tmYLValues"       : lat_values
      "tmYLMinorValues"  : lat_values@minor
    end setvalues
  end if

  UniformTickmarks(plot)  ; Force tickmarks to be same length 
                          ; and pointing outwards.
  return ret
end

;
; This function puts nice height values on the right axis, given pressure
; values on the left axis.
;
; Input: the plot to add the height labels to, pressure array.
;
; Output: 0 if okay, 1 if not.
;
function LabelHgtYR (plot : graphic,tickmark : graphic, pres, \
                     right_axis_title_obj : graphic)
local ptmp, hgt, hnice, pnice, xvp, yvp, widthvp, heightvp, xmin, xmax, \
      ymin, ymax, yaxis_font_height
begin
  if(pres(0).lt.pres(dimsizes(pres)-1))
    ptmp = 1.* pres(::-1)    ; reverse values so descending order
  else
    ptmp = 1.* pres          ; Make sure values are floating point.
  end if

;
; Calculate "nice" height values for which to label the right axis
;
  hgt   = GeopHgt(ptmp)     ; Calculate hgts as a fcn of pres.
  hnice = fspan(2.,34.,9)        ; Set range of "nice" hgt values.
  pnice = ftcurv(hgt,ptmp,hnice) ; Get pres vals at nice hgt vals.

  setvalues plot
    "tmYRMode"      : "Explicit"
    "tmYUseLeft"    : False
    "tmYRValues"    : pnice     ; At each "nice" pressure value, 
    "tmYRLabels"    : hnice     ; put a "height" value label.
    "tmYROn"        : True      ; Turn on right axis tick marks.
    "tmYRLabelsOn"  : True      ; Turn on right axis tick marks.
    "tmYRMinorOn"   : False     ; No minor tick marks.

    "tmYLMode"      : "Explicit" ; Define own tick mark labels.
    "tmYLValues"    : (/1000.,  850., 700., 500., 400., 300., 250.,\
                         200., 150., 100.,  70., 50., 30., 10./)
    "tmYLLabels"    : (/"1000","850","700","500","400","300","250",\
                         "200","150","100", "70", "50", "30", "10"/)
    "tmYLMinorOn"   : False        ; No minor tick marks.
    "tiYAxisString" : "Pressure (mb)"
  end setvalues

  getvalues plot
    "vpXF"                 : xvp
    "vpYF"                 : yvp
    "vpWidthF"             : widthvp
    "vpHeightF"            : heightvp
    "trXMinF"              : xmin
    "trXMaxF"              : xmax
    "trYMinF"              : ymin
    "trYMaxF"              : ymax
    "tiYAxisFontHeightF"   : yaxis_font_height
  end getvalues

  setvalues tickmark
    "vpXF"       : xvp
    "vpYF"       : yvp
    "vpWidthF"   : widthvp
    "vpHeightF"  : heightvp
    "trYReverse" : True
    "trXMinF"    : xmin
    "trXMaxF"    : xmax
    "trYMinF"    : ymin
    "trYMaxF"    : ymax
    "trYLog"     : True
  end setvalues
;
; Make right axis "height" title the same size as left axis title.
;
  setvalues right_axis_title_obj
    "txFontHeightF" : yaxis_font_height
    "txAngleF"      : 90.
  end setvalues

  return 0
end

;
; This function returns an RGB color map given the number of colors
; and a range of hue values.
;
function ColorWheel (ncolors:integer, hueRange:float)
local cmap, hleft, hright, colorWheel, h, s, v, sMax, sMin, nc2
begin
  hleft      = fabs(hueRange(0))
  hright     = fabs(hueRange(1))
  colorWheel = hueRange(2)

  h          = new (ncolors,float) 
  s          = new (ncolors,float) 
  v          = new (ncolors,float)        ; keep at 1.0 

  sMax       = 0.90                    ; 1.0/0.8 fine also 
  sMin       = 0.15                       
  if (colorWheel.eq.0.) then
    nc2           = ncolors/2
    h(0:nc2-1)    = hleft
    h(nc2:ncolors-1) = hright

    s(0:nc2-1)    = fspan(sMax,sMin,nc2)
    s(nc2:ncolors-1) = fspan(sMin,sMax,nc2)

    v(0:ncolors-1)   = 1.0
  else 
    h             = new (ncolors,float) 
    h(0:ncolors-1)   = fspan(hleft,hright,ncolors)
    s(0:ncolors-1)   = sMax                      ; keep constant
    v(0:ncolors-1)   = 1.0
  end if 

  cmap             = new((/ncolors+2,3/),float); create array
  cmap(0,:)        = (/1.,1.,1./)           ; white background
  cmap(1,:)        = (/0.,0.,0./)           ; black foreground
  cmap(2:ncolors+1,:) = hsv2rgb(h,s,v)

  return cmap
end

; This function generates left and right "hue" values for predefined
; colors.
;    hueRange = GenCmapHueRange("Blue","Red") 
;    hueRange = GenCmapHueRange("BlueGreen","YellowRed") 
;    hueRange = GenCmapHueRange(hleft,hright)   
;
; hueLeft - "Blue","Red","Yellow","Green","BlueGreen","RedYellow" are
;           predefined
;           "blue","red","yellow","green","bluegreen","redyellow" also
;           acceptable
; hueRight- "Blue","Red","Yellow","Green","GreenBlue","YellowRed" are
;           predefined
;           "blue","red","yellow","green","greenblue","yellowred" also
;           acceptable
;
;           numbers can also be used for new colors
;
; hueLeft/hueRight  =string, float or integer [both must have same type] 
; hueLeft/hueRight  =red [0]  =yellow [60]  =green [135]  =blue [225]
;                   =RedYellow or YellowRed [0]   color wheel traversed
;                   =BlueGreen or GreenBlue [225] differently
function GenCmapHueRange (hueLeft, hueRight) 
local colorWheel, hleft, hright, hueRange
begin
  colorWheel = 0.     ; =0. means two color map [=1. means multi-color]
  if (isstring(hueLeft) .and. isstring(hueRight) ) then
    if (hueLeft.eq."Red" .or. hueLeft.eq."red") then
      hleft = 0.
    end if
    if (hueLeft.eq."Blue" .or. hueLeft.eq."blue") then
      hleft = 225.
    end if
    if (hueLeft.eq."Green" .or. hueLeft.eq."green") then
      hleft = 135.
    end if
    if (hueLeft.eq."Yellow" .or. hueLeft.eq."yellow") then
      hleft = 67.5             ; was 60.
    end if
    if (hueLeft.eq."RedYellow" .or. hueLeft.eq."redyellow") then
      hleft      = 0.
      colorWheel = 1.
    end if
    if (hueLeft.eq."BlueGreen" .or. hueLeft.eq."bluegreen") then
      hleft      = 247.5
      colorWheel = 1.
    end if

    if (hueRight.eq."Red" .or. hueRight.eq."red") then
      hright = 0.
    end if
    if (hueRight.eq."Blue" .or. hueRight.eq."blue") then
      hright = 225.
    end if
    if (hueRight.eq."Green" .or. hueRight.eq."green") then
      hright = 135.
    end if
    if (hueRight.eq."Yellow" .or. hueRight.eq."yellow") then
      hright = 67.5             ; was 60.
    end if
    if (hueRight.eq."YellowRed" .or. hueRight.eq."yellowred") then
      hright     = 0.
      colorWheel = 1.
    end if
    if (hueRight.eq."GreenBlue" .or. hueRight.eq."greenblue") then
      hright     = 247.5
      colorWheel = 1.
    end if
  else  
    hleft  = fabs(hueLeft)   
    hright = fabs(hueRight)
    if (hueLeft.lt.0. .or. hueRight.lt.0.) then
      colorWheel = 1.
    end if
  end if  

  hueRange    = new ( 3, float)
  hueRange(0) = hleft
  hueRange(1) = hright
  hueRange(2) = colorWheel     ; flag for how to traverse the 
                               ; color wheel
  return (hueRange)
end

;
; This function generates RGB values for the given min and max contour
; intervals. If the interval spacing is equal to 0, then the min and max
; contour levels are retrieved from the plot object.
;
;    GenCmapMnMxCnInt(contour, -10, 35, 5,"Blue","Red") 
;    GenCmapMnMxCnInt(contour, -30, 30,10,"Green","Yellow") 
;    GenCmapMnMxCnInt(contour, -30, 30,10,"BlueGreen","YellowRed") 
;    GenCmapMnMxCnInt(contour, 100,250,10,-225. , -67.)     ; Blue-Green-Yellow
;    GenCmapMnMxCnInt(contour, 100,250,10,  35. , 190.)     ; Orange-LightBlue
;
; contour - contour plot
; MnCnLvl - minimum contour level [same as cnMinLevelValF]
; MxCnLvl - maximum contour level [same as cnMaxLevelValF]
; CnInt   - Contour interval      [same as cnLevelSpacingF]
; hueLeft - "Blue","Red","Yellow","Green","BlueGreen","RedYellow" are
;           predefined
;           "blue","red","yellow","green","bluegreen","redyellow" also
;           acceptable
;           numbers could also be used for new colors
; hueRight- "Blue","Red","Yellow","Green","GreenBlue","YellowRed" are
;            predefined
;           "blue","red","yellow","green","greenblue","yellowred" also
;           acceptable
;           numbers could also be used for new colors
;
; hueLeft/hueRight  =string, float or integer [both must have same type] 
; hueLeft/hueRight  =red [0]  =yellow [60]  =green [135]  =blue [225]
; note: nCnLvl can also be integer or float

function GenCmapMnMxCnInt (contour:graphic, \
                           min_contour_level:float, \
                           max_contour_level:float, \
                           contour_level_interval:float, \
                           color_left, color_right)
local i, class_name, cmap, cnlevels, hueRange, ncol, colorWheel, nCnLvl, \
      use_user_def_levels
begin
  class_name = NhlClassName(contour)
  if(class_name.ne."contourPlotClass")
    print("GenCmapMnMxCnInt: the first argument must be a contour plot object.")
    return 1
  end if

  if (typeof(color_left).ne.typeof(color_right) ) then
    print ("GenCmapMnMxCnInt: color_left/color_right are not the same type")
    return 1
  end if

  if (min_contour_level.gt.max_contour_level) then
    print ("GenCmapMnMxCnInt: min_contour_level > max_contour_level")
    return 1
  end if

  if (.not. isatt(contour,"ndvWks"))
    print("GenCmapMnMxCnInt: error retrieving workstation")
    return 1
  end if

  if (contour_level_interval.eq.0.) then
    use_user_def_levels = False
    getvalues contour
      "cnLevels"         : cnlevels
      "cnLevelSpacingF"  : contour_level_interval
    end getvalues
    nCnLvl = dimsizes(cnlevels)
    min_contour_level = min(cnlevels)
    max_contour_level = max(cnlevels)
  else
    use_user_def_levels = True
    nCnLvl = floattointeger((max_contour_level-min_contour_level)/fabs(contour_level_interval)+1.) 
  end if

  hueRange = GenCmapHueRange(color_left, color_right)
  ncol     = nCnLvl+1
  colorWheel = hueRange(2)
  if (colorWheel.eq.0. .and. ncol%2 .eq. 1) then
    ncol = ncol+1  ; require even # colors for 2 color maps
  end if

  cmap = ColorWheel (ncol, hueRange)   ; Generate color map.

  setvalues contour@ndvWks
    "wkColorMap" : cmap         ; Set the new colormap
  end setvalues

  if(use_user_def_levels)
    i = SetContourLevels(contour,min_contour_level,max_contour_level,\
                                 contour_level_interval)
  else
    i = 0
  end if

  return i
end

;
; This function makes labelbar and other label fonts the same size as
; tickmark fonts.
;
; Input: plot object, tickmark object (might be separate or same as plot
;        object).
;
; Output: 0 if okay, 1 if not.
;
function AdjustFontSizes (plot:graphic,tickmark:graphic,font_scale:float)
local height, class_name_plot, class_name_tick
begin
  class_name_tick = NhlClassName(tickmark)

  if(class_name_tick.eq."tickMarkClass".or. \
     class_name_tick.eq."logLinPlotClass")
    getvalues tickmark
      "tmXBLabelFontHeightF" : height
    end getvalues
  else
    if(class_name_tick.eq."textItemClass")
      getvalues tickmark
        "txFontHeightF" : height
      end getvalues
    else
      if(class_name_tick.eq."contourPlotClass")
        getvalues tickmark
          "tiXAxisFontHeightF" : height
        end getvalues
      else
        print("AdjustFontSizes: cannot adjust labelbar font height")
        return 0
      end if
    end if
  end if

  class_name_plot = NhlClassName(plot)

  if(class_name_plot.eq."contourPlotClass")
    setvalues plot
      "cnInfoLabelFontHeightF"  : height*font_scale
      "lbAutoManage"            : False
      "lbLabelFontHeightF"      : height*font_scale
    end setvalues
  else
    if(class_name_plot.eq."vectorPlotClass")
      setvalues plot
        "vcRefAnnoFontHeightF"  : height*font_scale
        "lbAutoManage"          : False
        "lbLabelFontHeightF"    : height*font_scale
      end setvalues
    end if
  end if

  return 0
end

;
; This function takes an array of plots and resizes them so they'll
; all fit on one page in a given row/column layout.
;
; Input: array of plots, an array giving row x column layout, an optional
;        list of resources.
;
; Output: 0 if okay, 1 if not.
;
; Possible resources:
;
;
; gsnPanelXWhiteSpacePercent - a percentage value from 0 to 100 to
;                              indicate what percentage of the total plot
;                              height should be used for white space at
;                              the left and right of each plot
;
; gsnPanelYWhiteSpacePercent - a percentage value from 0 to 100 to
;                              indicate what percentage of the total plot
;                              height should be used for white space at
;                              the top and bottom of each plot
;
; The default for the above resources is 1.
;
; gsnPanelLeft - A limit for where the leftmost plot(s) can be
;                drawn. This allows you to reserve white space at the
;                left. The default is 0.
;
; gsnPanelRight - A limit for where the rightmost plot(s) can be
;                 drawn. This allows you to reserve white space at the
;                 right. The default is 1.
;
; gsnPanelTop - A limit for where the topmost plot(s) can be
;               drawn. This allows you to reserve white space at the
;               top. The default is 1.
;
; gsnPanelBottom - A limit for where the bottommost plot(s) can be
;                  drawn. This allows you to reserve white space at the
;                  bottom. The default is 0.
;
function Panel(plot[*]:graphic,dims[2]:float,resources:logical )
local i, j, rows, cols, xrows, xcols, top, bottom, left, right, \
      vpx, vpy, vpwidth, vpheight, bb, plot_width, plot_height, \
      xwsp_perc, ywsp_perc, xwsp, ywsp, dx, dy, total_width, total_height, \
      col_scale, row_scale, scale, xpos, ypos, xsp, ysp, \
      new_plot_height, new_plot_width, new_total_height, new_total_width
begin
  rows = floattointeger(dims(0))
  cols = floattointeger(dims(1))
  if(rows.eq.0.or.cols.eq.0)
    print("Panel: rows and/or columns = 0  - no paneling done")
    return 0
  end if
  xrows = dims(0)
  xcols = dims(1)
  xwsp_perc = 1.0   ; White space is 1% of total length of plot.
  ywsp_perc = 1.0   ; This can be changed with an attribute.
  x_lft = 0.0       ; Initially assume plots will occupy full viewport.
  y_top = 1.0
  x_rgt = 1.0
  y_bot = 0.0

;
; Check for resources.
;
  if(resources)
    if(isatt(resources,"gsnPanelXWhiteSpacePercent"))
      xwsp_perc = resources@gsnPanelXWhiteSpacePercent
      if(xwsp_perc.lt.0.or.xwsp_perc.ge.100.)
        print("Panel: attribute gsnPanelXWhiteSpacePercent must be >= 0 and < 100.")
        print("Defaulting to 1.")
        xwsp_perc = 1.
      end if
    end if
    if(isatt(resources,"gsnPanelYWhiteSpacePercent"))
      ywsp_perc = resources@gsnPanelYWhiteSpacePercent
      if(ywsp_perc.lt.0.or.ywsp_perc.ge.100.)
        print("Panel: attribute gsnPanelYWhiteSpacePercent must be >= 0 and < 100.")
        print("Defaulting to 1.")
        ywsp_perc = 1.
      end if
    end if

    if(isatt(resources,"gsnPanelLeft"))
      x_lft = resources@gsnPanelLeft
      if(x_lft.lt.0..or.x_lft.ge.1.)
        print("Panel: attribute gsnPanelLeft must be >= 0.0 and < 1.0")
        print("Defaulting to 0.")
        x_lft = 0.0
      end if
    end if

    if(isatt(resources,"gsnPanelRight"))
      x_rgt = resources@gsnPanelRight
      if(x_rgt.le.0..or.x_rgt.gt.1.)
        print("Panel: attribute gsnPanelRight must be > 0.0 and <= 1.0")
        print("Defaulting to 1.")
        x_rgt = 1.0
      end if
    end if

    if(isatt(resources,"gsnPanelTop"))
      y_top = resources@gsnPanelTop
      if(y_top.le.0..or.y_top.gt.1.)
        print("Panel: attribute gsnPanelTop must be > 0.0 and <= 1.0")
        print("Defaulting to 1.")
        y_top = 1.0
      end if
    end if

    if(isatt(resources,"gsnPanelBottom"))
      y_bot = resources@gsnPanelBottom
      if(y_bot.lt.0..or.y_bot.ge.1.)
        print("Panel: attribute gsnPanelBottom must be >= 0.0 and < 1.0")
        print("Defaulting to 0.")
        y_bot = 0.0
      end if
    end if

    if(x_rgt.le.x_lft)
      print("Panel: attribute gsnPanelRight ("+x_rgt+") must be greater")
      print("than gsnPanelLeft ("+x_lft+").")
      return 1
    end if
    if(y_top.le.y_bot)
      print("Panel: attribute gsnPanelTop ("+y_top+") must be greater")
      print("than gsnPanelBottom ("+y_bot+").")
      return 1
    end if
  end if
;
; Make sure we have enough panels to fit all of the plots.
;
  nplots  = dimsizes(plot)      ; Total number of plots.
  npanels = rows*cols           ; Total number of panels.
  if(nplots.gt.npanels)
    print("Panel: you have more plots than you have panels.")
    print("Only " + npanels + " plots will be drawn.")
  end if
;
; We assume all plots are the same size, so if we get the size of
; the first one, this should be the size of all of them.
;
  getvalues plot(0)
    "vpXF"      : vpx       ; Get original view port coordinates.
    "vpYF"      : vpy
    "vpWidthF"  : vpwidth
    "vpHeightF" : vpheight
  end getvalues

  bb = NhlGetBB(plot(0))   ; Get bounding box of plot with
  top    = bb(0)           ; all of its annotations.
  bottom = bb(1)
  left   = bb(2)
  right  = bb(3)

;
; plot_width  : total width of plot with all of its annotations
; total_width : plot_width plus white space on both sides
;
  plot_width  = right - left     ; Calculate total width of plot.
  plot_height = top - bottom     ; Calculate total height of plot.

  xwsp = xwsp_perc/100. * plot_width  ; White space is a percentage of total
  ywsp = ywsp_perc/100. * plot_height ; width and height.

  total_width  = 2.*xwsp + plot_width   ; Calculate total width and height
  total_height = 2.*ywsp + plot_height  ; with white space added.
;
; We want:
;
;   ncols * scale * total_width  <= x_rgt - x_lft (the viewport width)
;   nrows * scale * total_height <= y_top - y_bot (the viewport height)
;
; By taking the minimum of these two, we get the scale
; factor that we need to fit all plots on a page.
;
  xrange = x_rgt - x_lft
  yrange = y_top - y_bot

  col_scale = min((/xrange/(xcols*total_width), xrange/))
  row_scale = min((/yrange/(xrows*total_height),yrange/))
  scale = min((/col_scale,row_scale/))

  new_plot_width  = scale*plot_width    ; Calculate new width
  new_plot_height = scale*plot_height   ; and height.

  xwsp = xwsp_perc/100. * new_plot_width   ; Calculate new white space.
  ywsp = ywsp_perc/100. * new_plot_height

  new_total_width  = 2.*xwsp + new_plot_width  ; Calculate new total width
  new_total_height = 2.*ywsp + new_plot_height ; and height w/white space.

  xsp = xrange - new_total_width*cols   ; Calculate total amt of white space
  ysp = yrange - new_total_height*rows  ; left in both X and Y directions.

  dx = scale * (vpx - left) ; Calculate distance from plot's left position
                            ; to its leftmost annotation
  dy = scale * (top - vpy) ; Calculate distance from plot's top position
                           ; to its topmost annotation.

  xpos = x_lft + xwsp + dx +(xsp/2.+new_total_width*ispan(0,cols-1,1))
  ypos = y_top - ywsp - dy -(ysp/2.+new_total_height*ispan(0,rows-1,1))

;
; Loop through each plot and draw it in the new scaled-down size.
;
  do i = 0,rows-1
    do j = 0,cols-1
      if(i*cols+j .lt. dimsizes(plot))
        if(.not.ismissing(plot(i*cols+j)))
          setvalues plot(i*cols+j)
            "vpXF"      : xpos(j)
            "vpYF"      : ypos(i)
            "vpWidthF"  : scale*vpwidth 
            "vpHeightF" : scale*vpheight
          end setvalues
        end if
      end if
    end do  ; end of columns
  end do    ; end of rows
  return 0
end

