undef("level_colors")
undef("plot_titles")
undef("uniform_tickmarks")
undef("vc_scalar_field_color")
undef("SetContourLevels")
undef("set_vector_size_params")
undef("SetVectorLevels")
undef("NgAdjustLongitude")
undef("NgAdjustLongitudeCoord")
undef("SetMapLimitsFromObject")
undef("NgSetMapLimits")
undef("CycleLon")
undef("CycleLonCoord")
undef("NgMapTickmarks")
	
function CycleLonCoord(lonin[*],start_lon,end_lon)

local lon,newlon,size1,size2,pivot,newdata

begin
	lon = lonin
	lon&$lon!0$ = lon
	size1 = dimsizes(lon({start_lon:}))
	size2 = dimsizes(lon({:end_lon}))
	pivot = new(1,typeof(lon))

	if (lon(dimsizes(lon)-1) .gt. 360) then
		pivot = 540
	else 
		if (lon(dimsizes(lon)-1) .gt. 180) then
			pivot = 360
		else
			pivot = 180
		end if
	end if

	newlon = new(size1 + size2,typeof(lon))
	newlon(:size1-1) = lon({start_lon:}) - pivot
	newlon(size1:) = lon({:end_lon})
	newlon!0 = lon!0
	newlon&$lon!0$ = newlon

	return newlon
end

function NgAdjustLongitudeCoord(lon[*],start_lon,end_lon)

;
; Add a cyclic point a to coord var (representing longitude)

local dims, newdata, ny, mx, mx1, sizes,lon,maxloninc,i,sign, \
	loninc,monotonic,nonmono_ix1, nonmono_ix2,llon,tmp
begin

    if (start_lon .gt. end_lon) then
	return CycleLonCoord(lon,start_lon,end_lon)
    end if

    mx = dimsizes(lon)

    if (lon(1) - lon(0) .gt. 0) then
	sign = 1.0
    else	
	sign = -1.0
    end if
	
    if (fabs(lon(mx-1) - lon(0)).ge. 360.) then
	; cyclic point already exists -- might be more points than we need
	return lon
    end if

    maxloninc = new(1,typeof(lon))
    maxloninc = 0.0
    monotonic = True
    nonmono_ix1 = -1
    nonmono_ix2 = -1
    do i = 1, dimsizes(lon) - 1 
	loninc = (lon(i) - lon(i-1)) * sign
	if (loninc .lt. 0.0) then
		monotonic = False
		if (nonmono_ix1.lt.0) then
			nonmono_ix1 = i
		else
			nonmono_ix2 = i
		end if
	end if
	if (loninc.gt.maxloninc) then
		maxloninc = loninc
	end if
    end do

    if (.not. monotonic) then
	print("not monotonic - not handled")
	return lon
    end if

    tmp = maxloninc + fabs(lon(mx-1) - lon(0))
    if (tmp+0.5 .lt. 360) then
	; it's not a global dataset -- cyclical point not needed
	return lon
    end if

    newlon = new((/mx+1/),typeof(lon))	
    newlon(0:mx-1) = lon
    if (sign .gt. 0)
	    newlon(mx) = newlon(0) + 360
    else
	    newlon(mx) = newlon(0) - 360
    end if

    return newlon

end

;
; assumes lat/lon data
; creates a new data set consisting of a piece running from start_lon to
; the end of the dataset, followed by a piece running from the beginning of
; the dataset to end_lon. The longitude array is made monotonic.

function CycleLon(data[*][*],start_lon,end_lon)

local lon,lat,newlon,size1,size2,pivot,newdata

begin

	print("in CycleLon")

	lon = data&$data!1$

	lon&$data!1$ = lon
	size1 = dimsizes(lon({start_lon:}))
	size2 = dimsizes(lon({:end_lon}))
	pivot = new(1,typeof(lon))

	if (lon(dimsizes(lon)-1) .gt. 360) then
		pivot = 540
	else 
		if (lon(dimsizes(lon)-1) .gt. 180) then
			pivot = 360
		else
			pivot = 180
		end if
	end if
	print(pivot)

	newlon = new(size1 + size2,typeof(lon))
	newlon(:size1-1) = lon({start_lon:}) - pivot 
	newlon(size1:) = lon({:end_lon})
	print(newlon)

	lat = data&$data!0$

	newdata = new((/dimsizes(lat),dimsizes(newlon)/),typeof(data))
	newdata!0 = data!0
	newdata&$data!0$ = lat
	newdata!1 = data!1
	newdata(:,:size1-1) = data(:,{start_lon:})
	newdata(:,size1:) = data(:,{:end_lon})
	newdata&$data!1$ = newlon

	if (isatt(data,"_FillValue")) then
    		newdata@_FillValue = data@_FillValue
	else
		if (isatt(data,"missing_value")) then
			newdata@_FillValue = data@missing_value
	        else
			delete(newdata@_FillValue)
		end if
	end if
	
	if (isatt(newdata,"_FillValue")) then
		    print("new cyclic dataset "+ newdata@_FillValue)
	else
		    print("new cyclic dataset - no fill value")
	end if

	return newdata
end

function NgAdjustLongitude(data[*][*],start_lon,end_lon)

;
; Add a cyclic point in "x" to a 2D array
; for a lat/lon plot "x"  corresponds to "lon"
;                    "ny" corresponds to "nlat"
;                    "mx" corresponds to "mlon"

local dims, newdata, ny, mx, mx1, sizes,lon,maxloninc,i,sign, \
	loninc,monotonic,nonmono_ix1, nonmono_ix2,tmp
begin
    dims    = dimsizes(data)
    ny      = dims(0)
    mx      = dims(1)
    mx1     = mx+1
;
    print("in fix_longitude")
 
    if (isatt(data,"_FillValue")) then
	print("fillvalue "+ data@_FillValue)
    else
	print("no fill value")
    end if

    if (ismissing(data!1) .or. .not. iscoord(data,data!1)) then
	print("returning")
	return data
    end if

    if (start_lon .gt. end_lon) then
	return CycleLon(data,start_lon,end_lon)
    end if

    lon = data&$data!1$	

    if (lon(1) - lon(0) .gt. 0) then
	sign = 1.0
    else	
	sign = -1.0
    end if
	
    if (fabs(lon(mx-1) - lon(0)).ge. 360.) then
	; cyclic point already exists -- might be more points than we need
	if (isatt(data,"_FillValue")) then
		print("cyclic point not needed "+ data@_FillValue)
	else
		print("cyclic point not needed - no fill value")
	end if
	return data
    end if

    maxloninc = new(1,typeof(lon))
    maxloninc = 0.0
    monotonic = True
    nonmono_ix1 = -1
    nonmono_ix2 = -1
    do i = 1, dimsizes(lon) - 1 
	loninc = (lon(i) - lon(i-1)) * sign
	if (loninc .lt. 0.0) then
		monotonic = False
		if (nonmono_ix1.lt.0) then
			nonmono_ix1 = i
		else
			nonmono_ix2 = i
		end if
	end if
	if (loninc.gt.maxloninc) then
		maxloninc = loninc
	end if
    end do

    if (.not. monotonic) then
	print("not monotonic - not handled")
	return data
    end if

    tmp = maxloninc + fabs(lon(mx-1) - lon(0))
    if (tmp+0.5 .lt. 360) then
	; it's not a global dataset -- cyclical point not needed
	print("cyclic point not added, range is: " + tmp)
	return data
    end if

     newdata = new((/ny  ,mx1/),typeof(data))	
     newdata(:,0:mx-1) = data             ; pass everything
     newdata(:,mx)     = (/ data(:,0) /)  ; value only

    if((.not.ismissing(newdata!1)) .and. iscoord(data,newdata!1)) then 
	if (sign .gt. 0)
               newdata&$newdata!1$(mx) = newdata&$newdata!1$(0) + 360
	else
               newdata&$newdata!1$(mx) = newdata&$newdata!1$(0) - 360
        end if
    end if

    if (isatt(data,"_FillValue")) then
    	newdata@_FillValue = data@_FillValue
    else
	if (isatt(data,"missing_value")) then
		newdata@_FillValue = data@missing_value
        else
		delete(newdata@_FillValue)
	end if
    end if
	

    if (isatt(newdata,"_FillValue")) then
	    print("added cyclic point "+ newdata@_FillValue)
    else
	    print("added cyclic point - no fill value")
    end if
    return(newdata)

end

function set_vector_size_params(plot:graphic, \
       	ref_magnitude:float,ref_length:float,\
	min_frac_length:float,min_distance:float)
local class_name,cur_mag,cur_len,cur_frac_len,cur_dist
begin

  class_name = NhlClassName(plot)
  if(class_name.ne."vectorPlotClass")
    print("set_vector_size_params: the first argument must be a vector plot object.")
    return -1
  end if

  getvalues plot
	"vcRefLengthF" : cur_len
	"vcRefMagnitudeF" : cur_mag
	"vcMinFracLengthF" : cur_frac_len
	"vcMinDistanceF" : cur_dist
  end getvalues

  if (ref_magnitude .ne. cur_mag .or. \
      ref_length .ne. cur_len .or. \
      min_frac_length .ne. cur_frac_len .or. \
      min_distance .ne. cur_dist)
	setvalues plot
		"vcRefLengthF" : ref_length
		"vcRefMagnitudeF" : ref_magnitude
		"vcMinFracLengthF" : min_frac_length
		"vcMinDistanceF" : min_distance
        end setvalues
	return 1
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
; Output: 0 if okay, -1 if not.
;
function SetContourLevels(plot:graphic, \
                          min_level:float,max_level:float,level_spacing:float)
local class_name,mode,cur_spacing,cur_min,cur_max,ret
begin

  if(level_spacing.lt.0.)
    return 0     ; Don't do anything if invalid level spacing
  end if

  class_name = NhlClassName(plot)
  if(class_name.ne."contourPlotClass")
    print("SetContourLevels: the first argument must be a contour plot object.")
    return -1
  else
    getvalues plot
	"cnLevelSelectionMode" : mode
	"cnLevelSpacingF" : cur_spacing
	"cnMinLevelValF"  : cur_min
        "cnMaxLevelValF"  : cur_max
    end getvalues
  end if
  
  if(min_level.gt.max_level)
    print("SetContourLevels: the minimum contour level must be less than")
    print("the maximum contour level.")
    return -1
  end if

  if (level_spacing .eq. 0)
	if (mode .ne. 0) 
		setvalues plot
	           "cnLevelSelectionMode"   : "AutomaticLevels"
		end setvalues
	        return 1
	end if
	return 0
  end if

  ret = 0
  if(min_level.eq.max_level)
	if (level_spacing .ne. cur_spacing) 
		setvalues plot
		       	"cnLevelSpacingF"       : level_spacing
		end setvalues
		ret = 1
	end if
  else 
	if (mode .ne. 1 .or. \
	   cur_spacing .ne. level_spacing .or. \
	   cur_min .ne. min_level .or. \
	   cur_max .ne. max_level)
	  	setvalues plot
		      "cnLevelSelectionMode"  : "ManualLevels"
		      "cnLevelSpacingF"       : level_spacing
		      "cnMinLevelValF"        : min_level
		      "cnMaxLevelValF"        : max_level
		end setvalues
		ret = 1
	end if
  end if

  return ret
end

;
; This function sets the vector levels given the vector object,
; the min and max vector levels, and the vector level spacing.
;
; Input: the vector plot, min and max vector level values, vector 
;        level spacing.
;
; If level_spacing is <= 0, then this routine doesn't do anything.
; This allows the routine to be available to the user only if
; he/she needs it.  If min_level=max_level, then only the level spacing 
; is set.
;
; Output: 0 if okay, -1 if not.
;
function SetVectorLevels(plot:graphic, \
                          min_level:float,max_level:float,level_spacing:float)
local class_name,mode,cur_spacing,cur_min,cur_max,ret
begin

  if(level_spacing.lt.0.)
    return 0     ; Don't do anything if invalid level spacing
  end if

  class_name = NhlClassName(plot)
  if(class_name.ne."vectorPlotClass")
    print("SetVectorLevels: the first argument must be a vector plot object.")
    return -1
  else
    getvalues plot
	"vcLevelSelectionMode" : mode
	"vcLevelSpacingF" : cur_spacing
	"vcMinLevelValF"  : cur_min
        "vcMaxLevelValF"  : cur_max
    end getvalues
  end if
  
  if(min_level.gt.max_level)
    print("SetVectorLevels: the minimum vector level must be less than")
    print("the maximum vector level.")
    return -1
  end if

  if (level_spacing .eq. 0)
	if (mode .ne. 0) 
		setvalues plot
	           "vcLevelSelectionMode"   : "AutomaticLevels"
		end setvalues
	        return 1
	end if
	return 0
  end if

  ret = 0
  if(min_level.eq.max_level)
	if (level_spacing .ne. cur_spacing) 
		setvalues plot
		       	"vcLevelSpacingF"       : level_spacing
		end setvalues
		ret = 1
	end if
  else 
	if (mode .ne. 1 .or. \
	   cur_spacing .ne. level_spacing .or. \
	   cur_min .ne. min_level .or. \
	   cur_max .ne. max_level)
	  	setvalues plot
		      "vcLevelSelectionMode"  : "ManualLevels"
		      "vcLevelSpacingF"       : level_spacing
		      "vcMinLevelValF"        : min_level
		      "vcMaxLevelValF"        : max_level
		end setvalues
		ret = 1
	end if
  end if

  return ret
end

function vc_scalar_field_color (vcplot : graphic, \
				enable : logical)
local use, sf

begin
	if (ismissing(vcplot))
		return -1
	end if

	if (isatt(vcplot,"ndvClass"))
		class = vcplot@ndvClass
		if (class.ne."vectorPlotClass") then
			return -1
		end if
	else
		return -1
	end if

	getvalues vcplot
		"vcScalarFieldData" : sf
		"vcUseScalarArray"  : use
	end getvalues

	if (ismissing(sf)) then
		return 0
	end if

	if (use.eq.False .and. enable.eq.True) then
		setvalues vcplot
			"vcUseScalarArray" : True;
		end setvalues
		return 1
	else 
		if (use.eq.True .and. enable.eq.False) then
			setvalues vcplot
				"vcUseScalarArray" : False;
			end setvalues
			return 1
		end if
	end if

	return 0
end


function level_colors (plot : graphic, \
			min_index : integer, \
			max_index : integer)

local ncols,lcount,fcols,icols,stride,rem,start,minix,maxix,nc,fmin,fmax

begin
	if (ismissing(plot))
		return 0
	end if
;	print(plot)

	if (isatt(plot,"ndvClass"))
		class = plot@ndvClass
		if (class.eq."contourPlotClass") then
			colres = "cnFillColors"
			levelcountres = "cnLevelCount"
		else
			if (class.eq."vectorPlotClass") then
				colres = "vcLevelColors"
				levelcountres = "vcLevelCount"
			else
				return 0
			end if
		end if
	else
		return 0
	end if
	if (.not. isatt(plot,"ndvWks")) then
		print("error: returning original color indexes")
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

;	print("numcolors " + ncols + "level count " + lcount)
;
; -1 indicates that max_index should be set equal to ncols - 1
;
	maxix = new(1,integer)
	minix = new(1,integer)
	maxix = max_index
	minix = min_index

;	print(minix)
;	print(maxix)
;	print(ncols)

	if (maxix .eq. -1) then
		maxix = ncols -1
	end if

	if (maxix .le. minix .or. minix .lt. 0) then
		print("invalid parameters to spread_colors: defaulting")
		maxix = ncols - 1
		minix = 2
	end if

	minix = max((/0,minix/))
	maxix = min((/ncols - 1,maxix/))
	nc = maxix - minix + 1
;	stride = (nc) / (lcount + 1)

;	print(stride)

;	if (stride.lt.1) then
;		icols = new(lcount+1,integer)
;		icols(0:nc - 1) = ispan(minix,maxix,1)
;		start = nc
;		do while (start + nc .le. lcount + 1)
;			icols(start:start+nc-1) = icols(0:nc-1)
;			start = start + nc
;		end do
;		if (start.lt.lcount+1) then
;			icols(start:lcount) = icols(0:lcount-start)
;		end if
;		return icols
;	end if

	fmin = new(1,float)
	fmax = new(1,float)

	fmin = minix
	fmax = maxix
	fcols = fspan(fmin,fmax,lcount+1)
;	icols = ispan(2,ncols-1,stride)
	icols = floattointeger(fcols + 0.5)

;	print(fcols)
;	print(icols)

	return icols
end

 
;
; Add up to three subtitles to top of plot (left, center, and right).
;
; Input: plot to add titles to, three text items representing each title,
;        three strings representing the titles, and a max height and
;        height factor for calculating the font height.
;
;  Output: 1 if something happened, 0 if okay, -1 if error.
;
function plot_titles (plot : graphic, \
             ltitle : graphic, ltext : string, \
             ctitle : graphic, ctext : string, \
             rtitle : graphic, rtext : string, \
             max_height : float, height_factor : float )

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

    getvalues ltitle
        "txFontAspectF" : asp1
        "txFontHeightF" : height1
        "txString"      : text1
        "vpOn"          : vpon1
    end getvalues

    getvalues ctitle
        "txFontAspectF" : asp2
        "txFontHeightF" : height2
        "txString"      : text2
        "vpOn"          : vpon2
    end getvalues

    getvalues rtitle
        "txFontAspectF" : asp3
        "txFontHeightF" : height3
        "txString"      : text3
        "vpOn"          : vpon3
    end getvalues
;
;   uniform aspect and height enforced for these titles
;
    asp = (asp1 + asp2 + asp3) / 3.0
    c1 = stringtochar(ltext)
    c2 = stringtochar(ctext)
    c3 = stringtochar(rtext)
;
; assume constant spacing since that takes the most room
; if there is a center title then measurement must be broken in two parts:
; half the center strings plus each of the two end strings
;
    if (sizeof(c2).lt.2) then
	    tchars = sizeof(c1) + sizeof(c2) + sizeof(c3) + 3
	    sp_per_char = vp_width / tchars
    else
	    tchars = 2 + max((/sizeof(c1) + sizeof(c2) / 2, \
			       sizeof(c3) + sizeof(c2) / 2 /))
	    sp_per_char = vp_width / (tchars * 2)
    end if

    height = min((/max_height,sp_per_char/)) * height_factor
    print (tchars + " tchars " + sp_per_char + " sp_per_char " + height)

    if (sizeof(c1).lt.2)
        if (vpon1 .ne. False)
            setvalues ltitle
                "vpOn" : False
            end setvalues
            ret = 1
        end if
    else
      if (.not. (asp1 .eq. asp .and. \
           height1 .eq. height .and. \
           text1 .eq. ltext .and. \
           vpon1 .eq. True))
        
        setvalues ltitle
            "txFontAspectF" : asp
            "txFontHeightF" : height
            "txString"      : ltext
            "vpOn"          : True
        end setvalues
        ret = 1
      end if
    end if

    if (sizeof(c2).lt.2)
        if (vpon2 .ne. False)
            setvalues ctitle
                "vpOn" : False
            end setvalues
            ret = 1
        end if
    else
      if (.not. (asp2 .eq. asp .and. \
           height2 .eq. height .and. \
           text2 .eq. ctext .and. \
           vpon2 .eq. True))
        
        setvalues ctitle
            "txFontAspectF" : asp
            "txFontHeightF" : height
            "txString"      : ctext
            "vpOn"          : True
        end setvalues
        ret = 1
      end if
    end if

    if (sizeof(c3).lt.2)
        if (vpon3 .ne. False)
            setvalues rtitle
                "vpOn" : False
            end setvalues
            ret = 1
        end if
    else
      if (.not. (asp3 .eq. asp .and. \
           height3 .eq. height .and. \
           text3 .eq. rtext .and. \
           vpon3 .eq. True))
    
        setvalues rtitle
            "txFontAspectF" : asp
            "txFontHeightF" : height
            "txString"      : rtext
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
procedure uniform_tickmarks(plot:graphic)

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


function SetMapLimitsFromObject(map : graphic, \
				obj : graphic, \
				center_lon : float)

local xmin,ymin,xmax,ymax,latlon0,xmin0,ymin0,xmax0,ymax0,\
	cen_lon0,cen_lon,rel_lon,rel_lon0

begin

	if (.not. (isatt(map,"ndvClass") .and. \
	    map@ndvClass .eq. "mapPlotClass")) then
		return -1;
	end if

	if (.not. (isatt(obj,"ndvClass") .and. NhlIsTransform(obj))) then
		return -1;
	end if
	
	getvalues obj
		"trXMinF" : xmin
		"trXMaxF" : xmax
		"trYMinF" : ymin
		"trYMaxF" : ymax
	end getvalues

	getvalues map
		"mpLimitMode" : latlon0
		"mpMinLonF"   : xmin0
		"mpMaxLonF"   : xmax0
		"mpMinLatF"   : ymin0
		"mpMaxLatF"   : ymax0
		"mpCenterLonF" : cen_lon0
		"mpRelativeCenterLon" : rel_lon0
	end getvalues

	if (xmax - xmin .lt. 359.5) then
		cen_lon = 0
		rel_lon = True
	else
		cen_lon = center_lon
		rel_lon = False
        end if

	if ( latlon0 .eq. 1 .and. \
	     xmin0 .eq. xmin .and. \
	     ymin0 .eq. ymin .and. \
	     xmax0 .eq. xmax .and. \
	     ymax0 .eq. ymax) then
;	     ymax0 .eq. ymax .and. \
;	     cen_lon0 .eq. cen_lon .and. \
;	     rel_lon0 .eq. rel_lon ) then
		return 0
	end if
	 
;	print("latlon " + latlon0)
;	print("xmin: " + xmin + " xmin0: " + xmin0)
;	print("ymin: " + ymin + " ymin0: " + ymin0)
;	print("xmax: " + xmax + " xmax0: " + xmax0)
;	print("ymax: " + ymax + " ymax0: " + ymax0)
;	print("rel_lon: " + rel_lon + " rel_lon0: " + rel_lon0)
;	print("cen_lon: " + cen_lon + " cen_lon0: " + cen_lon0)

	setvalues map
		"mpLimitMode" : "latlon"
		"mpMinLonF"   : xmin
		"mpMaxLonF"   : xmax
		"mpMinLatF"   : ymin
		"mpMaxLatF"   : ymax
;		"mpRelativeCenterLon" : rel_lon
;		"mpCenterLonF" : cen_lon
	end setvalues

	return 1
end

function NgSetMapLimits(map : graphic, \
	 	        obj : graphic, \
			limit_mode : integer, \
			left_min[*] : float, \
			right_max[*] : float, \
			bottom_min[*] : float, \
			top_max[*] : float)

local lmode0,left_min0,bottom_min0,right_max0,top_max0,ret


begin
	if (.not. (isatt(map,"ndvClass") .and. \
	    map@ndvClass .eq. "mapPlotClass")) then
		return -1;
	end if

	if (limit_mode .lt. 0) then
		ret = SetMapLimitsFromObject(map,obj,0)
		return ret
	end if

	ret = 0
	getvalues map
		"mpLimitMode" : lmode0
	end getvalues

	if (lmode0 .ne. limit_mode) then
		ret = 1
	end if

	if (ret .eq. 0) then
		if (limit_mode .eq. 0) then
			return ret
		end if

		if (limit_mode .eq. 1) then
			getvalues map
				"mpMinLonF"   : left_min0
				"mpMaxLonF"   : right_max0
				"mpMinLatF"   : bottom_min0
				"mpMaxLatF"   : top_max0
			end getvalues
		else if (limit_mode .eq. 2) then
			getvalues map
				"mpLeftAngleF"   : left_min0
				"mpRightAngleF"   : right_max0
				"mpBottomAngleF"   : bottom_min0
				"mpTopAngleF"   : top_max0
			end getvalues
		else if (limit_mode .eq. 3) then
			getvalues map
				"mpLeftNPCF"   : left_min0
				"mpRightNPCF"   : right_max0
				"mpBottomNPCF"   : bottom_min0
				"mpTopNPCF"   : top_max0
			end getvalues
		else if (limit_mode .eq. 4) then
			getvalues map
				"mpLeftNDCF"   : left_min0
				"mpRightNDCF"   : right_max0
				"mpBottomNDCF"   : bottom_min0
				"mpTopNDCF"   : top_max0
			end getvalues
		else if (limit_mode .eq. 5) then
			getvalues map
				"mpLeftCornerLonF"   : left_min0
				"mpRightCornerLonF"   : right_max0
				"mpLeftCornerLatF"   : bottom_min0
				"mpRightCornerLatF"   : top_max0
			end getvalues
		else if (limit_mode .eq. 6) then
			left_min0 = new(2,float)
			right_max0 = new(2,float)
			bottom_min0 = new(2,float)
			top_max0 = new(2,float)
			getvalues map
				"mpLeftPointLonF"   : left_min0(0)
				"mpLeftPointLatF"   : left_min0(1)
				"mpBottomPointLonF"   : bottom_min0(0)
				"mpBottomPointLatF"   : bottom_min0(1)
				"mpRightPointLonF"   : right_max0(0)
				"mpRightPointLatF"   : right_max0(1)
				"mpTopPointLonF"   : top_max0(0)
				"mpTopPointLatF"   : top_max0(1)
			end getvalues
		end if
		end if
		end if
		end if
		end if
		end if
		if (limit_mode .gt. 0 .and. \
		    any(left_min0 .ne. left_min) .or. \
		    any(right_max0 .ne. right_max) .or. \
		    any(bottom_min0 .ne. bottom_min) .or. \
		    any(top_max0 .ne. top_max)) then
			ret = 1
		end if 
	end if
	if (ret .eq. 0) then
		return ret
	end if

	if (limit_mode .eq. 0) then
		setvalues map
			"mpLimitMode" : "maximalarea"
		end setvalues
	else if (limit_mode .eq. 1) then
		setvalues map
			"mpLimitMode" : "latlon"
			"mpMinLonF"   : left_min
			"mpMaxLonF"   : right_max
			"mpMinLatF"   : bottom_min
			"mpMaxLatF"   : top_max
		end setvalues
	else if (limit_mode .eq. 2) then
		setvalues map
			"mpLimitMode" : "angles"
			"mpLeftAngleF"   : left_min
			"mpRightAngleF"   : right_max
			"mpBottomAngleF"   : bottom_min
			"mpTopAngleF"   : top_max
		end setvalues
	else if (limit_mode .eq. 3) then
		setvalues map
			"mpLimitMode" : "npc"
			"mpLeftNPCF"   : left_min
			"mpRightNPCF"   : right_max
			"mpBottomNPCF"   : bottom_min
			"mpTopNPCF"   : top_max
		end setvalues
	else if (limit_mode .eq. 4) then
		setvalues map
			"mpLimitMode" : "ndc"
			"mpLeftNDCF"   : left_min
			"mpRightNDCF"   : right_max
			"mpBottomNDCF"   : bottom_min
			"mpTopNDCF"   : top_max
		end setvalues
	else if (limit_mode .eq. 5) then
		setvalues map
			"mpLimitMode" : "corners"
			"mpLeftCornerLonF"   : left_min
			"mpRightCornerLonF"   : right_max
			"mpLeftCornerLatF"   : bottom_min
			"mpRightCornerLatF"   : top_max
		end setvalues
	else if (limit_mode .eq. 6) then
		setvalues map
			"mpLimitMode" : "points"
			"mpLeftPointLonF"   : left_min(0)
			"mpLeftPointLatF"   : left_min(1)
			"mpBottomPointLonF"   : bottom_min(0)
			"mpBottomPointLatF"   : bottom_min(1)
			"mpRightPointLonF"   : right_max(0)
			"mpRightPointLonF"   : right_max(1)
			"mpTopPointLonF"   : top_max(0)
			"mpTopPointLatF"   : top_max(1)
		end setvalues
	end if
	end if
	end if
	end if
	end if
	end if
	end if

	return 1
end

function NgMapTickmarks(map : graphic, tm : graphic,\
			max_font_height : float,\
			max_major_length : float,\
			max_minor_length : float )
	
local	vpxf,vpyf,vpwf,vphf,min_lon,max_lon,min_lat,max_lat,center_lon, \
	projection,rel_lon,rel_lat,has_max_lon_span,adj_min_lon,adj_min_lat,\
	lon_range,lat_range,lcheck_arr,lspcng_arr,mlspcng_arr,\
	lon_ind,lon_spcng,mlon_spcng,lat_ind,lat_spcng,mlat_spcng,\
	yvalues,myvalues,ylabels,indexy,xvalues,xvalues2,\
	xlabels,mxvalues,indexx,height,\
	major_length,minor_length,int_min_lat,int_min_lon,sign,round_val,tval,\
	center_lat,center_rot,xmin0,ymin0,xmax0,ymax0,xbmode0,ylmode0, \
	xlabels0,ylabels0,xvalues0,yvalues0,mxvalues0,myvalues0,\
	xbheight0,ylheight0,xlength0,ylength0,xmlength0,ymlength0,\
	lmap_pos,rmap_pos,bmap_pos,tmap_pos,mrange,llrange,\
	xmin,xmax,ymin,ymax,tmp,\
	xoutl0,youtl0,xminoutl0,yminoutl0,on,ret
begin
	
    print("in map tickmarks")
    ret = 0
    getvalues map
      "vpXF"         : vpxf
      "vpYF"         : vpyf
      "vpWidthF"     : vpwf
      "vpHeightF"    : vphf
      "mpLeftMapPosF" : lmap_pos
      "mpRightMapPosF" : rmap_pos
      "mpBottomMapPosF" : bmap_pos
      "mpTopMapPosF" : tmap_pos
      "mpMinLonF"    : min_lon
      "mpMaxLonF"    : max_lon
      "mpMinLatF"    : min_lat
      "mpMaxLatF"    : max_lat
      "mpCenterLonF" : center_lon
      "mpCenterLatF" : center_lat
      "mpCenterRotF" : center_rot
      "mpProjection" : projection
      "mpRelativeCenterLon" : rel_lon
      "mpRelativeCenterLat" : rel_lat
    end getvalues

    getvalues tm
	"vpOn" : on
    end getvalues
    if (projection.ne.8 .or. center_lat.ne.0.0 .or. center_rot.ne.0.0 ) then
	if (on.ne.False) then
	        setvalues tm
			"vpOn" : False
		end setvalues
		ret = 1
        end if
	return ret
    else 
	if (on.ne.True) then
        	setvalues tm
			"vpOn" : True
		end setvalues
		ret = 1
	end if
    end if

    if (center_lon .le. -180) then
	center_lon = center_lon + 360
    end if
    if (center_lon .gt. 180) then
	center_lon = center_lon - 360
    end if

;    print("max " + max_lon + "min " +  min_lon + "center " + center_lon)
    print(lmap_pos + " " + bmap_pos + " " + rmap_pos + " " + tmap_pos)

    ndctodata(map,lmap_pos,bmap_pos,min_lon,min_lat)
    ndctodata(map,rmap_pos,tmap_pos,max_lon,max_lat)

    print("ll " + min_lon + "," + min_lat + " ur " + max_lon +","+ max_lat)

    do while (min_lon .gt. max_lon)
	min_lon = min_lon - 360
    end do
    do while (max_lon - min_lon .gt. 360) 
		max_lon = max_lon - 360
    end do
    do while (fabs(min_lon) .gt. 180) 
	if (min_lon .lt. -180) then
		min_lon = min_lon + 360
		max_lon = max_lon + 360
	else
		if (min_lon .gt. 180) then
			min_lon = min_lon - 360
			max_lon = max_lon - 360
		end if
	end if
    end do
    if (min_lon .eq. max_lon .and. lmap_pos .ne. rmap_pos) then
	if (floattointeger(min_lon + .5) .eq. \
	    floattointeger(180 + center_lon +.5)) then
		min_lon = min_lon - 360
	else
		if (floattointeger(max_lon - .5) .eq. \
	            floattointeger(-180 + center_lon - .5)) then
			max_lon = max_lon + 360	
		end if
	end if
    end if
	

    print("ll " + min_lon + "," + min_lat + " ur " + max_lon +","+ max_lat)
    lmap_pos = (lmap_pos - vpxf) / vpwf
    rmap_pos = (rmap_pos - vpxf) / vpwf
    bmap_pos = (bmap_pos - (vpyf - vphf)) / vphf
    tmap_pos = (tmap_pos - (vpyf - vphf)) / vphf
    print("l " + lmap_pos + " r " + rmap_pos + " b " \
		 + bmap_pos + " t " + tmap_pos)

;   print(min_lon)
;   print(center_lon)
;   print(projection)


; Create a TickMark object to label the lat/lon grid.

    lcheck_arr  = (/ 20,  40,  60, 360/)  ; lat/lon range will determine
                                          ; spacing of minor/major ticks
    lspcng_arr  = (/  5,  10,  20,  30/)  ; spacings for major ticks
    mlspcng_arr = (/  1,   2,   5,  10/)  ; spacings for minor ticks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; first figure out the latitude tickmarks 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lat_range = max_lat - min_lat   ; latitude range
	
    lat_ind    = ind(lat_range.le.lcheck_arr)
    lat_spcng  = lspcng_arr(lat_ind(0))  ; spacing for lat major tickmarks
    mlat_spcng = mlspcng_arr(lat_ind(0)) ; spacing for lat minor tickmarks


    if (lat_ind(0).eq.3) then
	round_val = 10
	tval = 5
    else 
	if (lat_ind(0).eq.2) then
		round_val = 10
		tval = 5
	else 
	    if (lat_ind(0).eq.1) then
	    	round_val = 5
	    	tval = 3
	    else
		round_val = 1
		tval = 1
	    end if
        end if
    end if

    int_min_lat = floattointeger(min_lat)
    if (int_min_lat.lt.0) then
	sign = -1
    else
	sign = 1
    end if

    if (((sign * int_min_lat) % round_val).ge.tval) then
	int_min_lat = round_val * (int_min_lat / round_val) + sign * round_val
    else
	int_min_lat = round_val * (int_min_lat / round_val)
    end if	

; Calculate values for latitude minor/major tickmark locations.

    yvalues  = ispan(int_min_lat,\
                     floattointeger(max_lat),lat_spcng)
    myvalues = ispan(int_min_lat,\
                     floattointeger(max_lat),mlat_spcng)

; Create labels for latitude tick marks.

    ylabels  = new(dimsizes(yvalues),string)
    indexy = ind(yvalues.lt.0)
    if(.not.all(ismissing(indexy)))
        ylabels(indexy) = fabs(yvalues(indexy)) + "S"    ; south
    end if
    delete(indexy)

    indexy = ind(yvalues.gt.0)
    if(.not.all(ismissing(indexy)))
        ylabels(indexy) = yvalues(indexy) + "N"    ; north
    end if
    delete(indexy)

    indexy = ind(yvalues.eq.0)
    if(.not.all(ismissing(indexy)))
        ylabels(indexy) = yvalues(indexy)          ; equator
    end if
    delete(indexy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; now figure out the longitude tickmarks 
; it's a little tricky, because we need to consider the center longitude;
; and whether Ezmap will decide to plot the maximal longitudinal extent.
; It depends on where the center longitude is in relation to the maximum
; and minimum longitudes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; first get everything into the canonical -180 to 180 range
;
;
; not really sure how this condition should be handled
;
;
    if (rel_lon) then
	center_lon = center_lon + min_lon + (max_lon - min_lon) / 2
    end if
    do while (fabs(center_lon) .gt. 180) 
	if (center_lon .lt. -180) then
		center_lon = center_lon + 360
	else
		if (center_lon .gt. 180) then
			center_lon = center_lon - 360
		end if
	end if
    end do
	

    print("max " + max_lon + " min " +  min_lon + " center " + center_lon)

    has_max_lon_span = False
    if (max_lon - min_lon .ge. 359.5) then
	has_max_lon_span = True
;    else
;	if (min_lon .gt. center_lon .and. \
;	    center_lon + 360 .gt. max_lon .and. \
;	    center_lon + 180 .lt. max_lon) then
;		has_max_lon_span = True
;	else
;		if (max_lon .lt. center_lon .and. \
;		    center_lon - 360 .lt. min_lon .and. \
;		    center_lon - 180 .gt. min_lon) then
;			has_max_lon_span = True
;		end if
;	end if
    end if
;
; the center longitude only plays a role in the maximum span situation
; may have to adjust into the 180 range again; but note that max_lon
; may still be outside this range
;
    if (has_max_lon_span) then
	lon_range = 360
	min_lon = center_lon - 180
	max_lon = center_lon + 180
	if (min_lon .lt. -180) then
		min_lon = min_lon + 360
		max_lon = max_lon + 360
	else
		if (min_lon .gt. 180) then
			min_lon = min_lon - 360
			max_lon = max_lon - 360
		end if
	end if
    else	
	    lon_range = max_lon - min_lon   ; longitude range
    end if

    print(max_lon)
    print(min_lon)

    lon_ind    = ind(lon_range.le.lcheck_arr)
    lon_spcng  = lspcng_arr(lon_ind(0))  ; spacing for lon major tickmarks
    mlon_spcng = mlspcng_arr(lon_ind(0)) ; spacing for lon minor tickmarks

    if (lon_ind(0).eq.3) then
	round_val = 10
	tval = 5
    else 
	if (lon_ind(0).eq.2) then
		round_val = 10
		tval = 5
	else 
	    if (lon_ind(0).eq.1) then
	    	round_val = 5
	    	tval = 3
	    else
		round_val = 1
		tval = 1
	    end if
        end if
    end if

    int_min_lon = floattointeger(min_lon)
    if (int_min_lon.lt.0) then
	sign = -1
    else
	sign = 1
    end if

    if (((sign * int_min_lon) % round_val).ge.tval) then
	int_min_lon = round_val * (int_min_lon / round_val) + sign * round_val
    else
	int_min_lon = round_val * (int_min_lon / round_val)
    end if	

; Calculate values for longitude major tickmark locations.
    xvalues = ispan(int_min_lon,\
                    floattointeger(max_lon),lon_spcng)
;    print(xvalues)

; Calculate values for longitude minor tickmark locations.
    mxvalues = ispan(floattointeger(min(xvalues)),\
                     floattointeger(max(xvalues)),mlon_spcng)

    xlabels  = new(dimsizes(xvalues),string)

    xvalues2 = xvalues

    indexx = ind(xvalues2.gt.180)
;    print(indexx)
    if(.not.all(ismissing(indexx)))
	    xvalues2(indexx) = xvalues2(indexx) - 360
    end if
    delete(indexx)

    indexx = ind(xvalues2.lt.-180)
;    print(indexx)
    if(.not.all(ismissing(indexx)))
	    xvalues2(indexx) = xvalues2(indexx) + 360
    end if
    delete(indexx)

;    print(xvalues2)

    indexx = ind((xvalues2).lt.0)
;    print(indexx)
    if(.not.all(ismissing(indexx)))
        xlabels(indexx) = fabs(xvalues2(indexx)) + "W"     ; west
    end if
    delete(indexx)

    indexx = ind(xvalues2.gt.0.and.xvalues2.lt.180)
;    print(indexx)
    if(.not.all(ismissing(indexx)))
        xlabels(indexx) = fabs(xvalues2(indexx)) + "E"     ; east
    end if
    delete(indexx)

    indexx = ind(xvalues2.eq.0.or.fabs(xvalues2).eq.180)
    if(.not.all(ismissing(indexx)))
        xlabels(indexx) = fabs(xvalues2(indexx))           ; 0 or 180
    end if
    delete(indexx)

    mrange = rmap_pos - lmap_pos
    if (mrange .eq. 1.0) then
    	xmin = min_lon
	xmax = max_lon
    else
	xmin = min_lon - lon_range * lmap_pos / mrange
	xmax = max_lon + lon_range * (1 - rmap_pos) / mrange
    end if

    print("xmin " + xmin + " xmax " + xmax + " mrange " + mrange)

    mrange = tmap_pos - bmap_pos
    if (mrange .eq. 1.0) then
    	ymin = min_lat
	ymax = max_lat
    else
	ymin = min_lat - lat_range * bmap_pos / mrange
	ymax = max_lat + lat_range * (1 - tmap_pos) / mrange
    end if


;    mrange = tmap_pos - bmap_pos
;    llrange = 1.0 * (yvalues(dimsizes(yvalues)-1) - int_min_lat)
;    fyvalues = bmap_pos + mrange * (yvalues - int_min_lat) / llrange
;    fmyvalues = bmap_pos + mrange * (myvalues - int_min_lat) / llrange

; Set the tickmark lat/lon grid.

    getvalues tm
       "trXMinF"	  : xmin0
       "trXMaxF"	  : xmax0
       "trYMinF"	  : ymin0
       "trYMaxF"	  : ymax0

       "tmXBMode"         : xbmode0
       "tmXBLabels"       : xlabels0
       "tmXBValues"       : xvalues0
       "tmXBMinorValues"  : mxvalues0

       "tmYLMode"         : ylmode0
       "tmYLLabels"       : ylabels0
       "tmYLValues"       : yvalues0
       "tmYLMinorValues"  : myvalues0
    end getvalues

    if (.not. (xmin0 .eq. xmin .and. \
	       xmax0 .eq. xmax .and. \
	       ymin0 .eq. ymin .and. \
	       ymax0 .eq. ymax .and. \
	       xbmode0 .eq. 2 .and. \             ; "Explicit" == 2
	       dimsizes(xlabels0) .eq. dimsizes(xlabels) .and. \
	       all(xlabels0 .eq. xlabels) .and. \
	       dimsizes(xvalues0) .eq. dimsizes(xvalues) .and. \
	       all(xvalues0 .eq. xvalues) .and. \
	       dimsizes(mxvalues0) .eq. dimsizes(mxvalues) .and. \
	       all(mxvalues0 .eq.mxvalues) .and. \
	       ylmode0 .eq. 2 .and. \
	       dimsizes(ylabels0) .eq. dimsizes(ylabels) .and. \
	       all(ylabels0 .eq. ylabels) .and. \
	       dimsizes(yvalues0) .eq. dimsizes(yvalues) .and. \
	       all(yvalues0 .eq. yvalues) .and. \
	       dimsizes(myvalues0) .eq. dimsizes(myvalues) .and. \
	       all(myvalues0 .eq.myvalues))) then

;	    print("first")	       
	    ret = 1
	    setvalues tm
	       "trXMinF"	  : xmin
	       "trXMaxF"	  : xmax
	       "trYMinF"	  : ymin
	       "trYMaxF"	  : ymax

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

; Get tickmark labels sizes and lengths.
    getvalues tm
      "tmXBLabelFontHeightF" : xbheight0
      "tmXBMajorLengthF"     : xlength0
      "tmXBMinorLengthF"     : xmlength0
      "tmXBMajorOutwardLengthF" : xoutl0
      "tmXBMinorOutwardLengthF" : xminoutl0

      "tmYLLabelFontHeightF" : ylheight0
      "tmYLMajorLengthF"     : ylength0
      "tmYLMinorLengthF"     : ymlength0
      "tmYLMajorOutwardLengthF" : youtl0
      "tmYLMinorOutwardLengthF" : yminoutl0
    end getvalues

; Make tickmark label sizes and lengths uniform sizes, and point tick
; marks outward.
;
    major_length = min((/max_major_length,(ylength0 + xlength0) / 2./))
    minor_length = min((/max_minor_length,(ymlength0 + xmlength0) / 2./))
    height = min((/max_font_height,xbheight0 + ylheight0 / 2.0/))

    print ("height " + height + " xb " + xbheight0 + " yl " + ylheight0)
    if (.not. (xbheight0 .eq. height .and. \
	       xlength0 .eq. major_length .and. \
	       xmlength0 .eq. minor_length .and. \
               xoutl0 .eq. major_length .and. \
	       xminoutl0 .eq. minor_length .and. \
	       ylheight0 .eq. height .and.\
	       ylength0 .eq. major_length .and. \
	       ymlength0 .eq. minor_length .and. \
               youtl0 .eq. major_length .and. \
	       yminoutl0 .eq. minor_length)) then

;	    print("second")	       
	    ret = 1
	    setvalues tm
	      "tmXBLabelFontHeightF" : height
	      "tmYLLabelFontHeightF" : height
	      "tmXBMajorLengthF"     : major_length
	      "tmXBMinorLengthF"     : minor_length
	      "tmYLMajorLengthF"     : major_length
	      "tmYLMinorLengthF"     : minor_length
	      "tmXBMajorOutwardLengthF" : major_length
	      "tmXBMinorOutwardLengthF" : minor_length
	      "tmYLMajorOutwardLengthF" : major_length
	      "tmYLMinorOutwardLengthF" : minor_length
	    end setvalues
    end if

;    print(ret)
    return ret

end

