undef("spread_colors")
undef("map_tickmarks")
undef("pos_xy_anno")
undef("dim_average")
undef("plot_title")
undef("fix_longitude")
undef("fix_longitude_coord")
undef("set_map_limits_from_object")


function dim_average (data)

local is,ldata

begin
 	is = isatt(data,(/"_FillValue","missing_value"/))

	if (is(0).eq.True .or. is(1).eq.False) then
		return dim_avg(data)
	else
		ldata = data
		ldata@_FillValue = data@missing_value
		return dim_avg(ldata)
	end if
end


function spread_colors (plot : graphic, \
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
	stride = (nc) / (lcount + 1)

;	print(stride)

	if (stride.lt.1) then
		icols = new(lcount+1,integer)
		icols(0:nc - 1) = ispan(minix,maxix,1)
		start = nc
		do while (start + nc .le. lcount + 1)
			icols(start:start+n-1) = icols(0:nc-1)
			start = start + nc
		end do
		if (start.lt.lcount+1) then
			icols(start:lcount) = icols(0:lcount-start)
		end if
		return icols
	end if

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

function fix_longitude_coord(lon[*])

;
; Add a cyclic point a to coord var (representing longitude)

local dims, newdata, ny, mx, mx1, sizes,lon,maxloninc,i,sign, \
	loninc,monotonic,nonmono_ix1, nonmono_ix2,llon
begin

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

    if (maxloninc + fabs(lon(mx-1) - lon(0)) .lt. 360) then
	; it's not a global dataset -- cyclical point not needed
	return lon
    end if

    if (typeof(data).eq."double") then
	    newlon = new((/mx+1/),double)
    else
	    newlon = new((/mx+1/),float)
    end if 
    newlon(0:mx-1) = lon
    newlon(mx) = newlon(0) + 360 * sign

    return newlon

end

function fix_longitude(data[*][*])

;
; Add a cyclic point in "x" to a 2D array
; for a lat/lon plot "x"  corresponds to "lon"
;                    "ny" corresponds to "nlat"
;                    "mx" corresponds to "mlon"

local dims, newdata, ny, mx, mx1, sizes,lon,maxloninc,i,sign, \
	loninc,monotonic,nonmono_ix1, nonmono_ix2
begin
    dims    = dimsizes(data)
    ny      = dims(0)
    mx      = dims(1)
    mx1     = mx+1

    if (ismissing(data!1) .or. .not. iscoord(data,data!1)) then
	return data
    end if

    lon = data&$data!1$	

    if (lon(1) - lon(0) .gt. 0) then
	sign = 1.0
    else	
	sign = -1.0
    end if
	
    if (fabs(lon(mx-1) - lon(0)).ge. 360.) then
	; cyclic point already exists -- might be more points than we need
	return data
    end if

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

    if (maxloninc + fabs(lon(mx-1) - lon(0)) .lt. 360) then
	; it's not a global dataset -- cyclical point not needed
	return data
    end if

;    if (typeof(data).eq."float") then
;	    newdata = new((/ny  ,mx1/),float)
;	    newdata(:,0:mx-1) = data             ; pass everything
;	    newdata(:,mx)     = (/ data(:,0) /)  ; value only
;    else

     if (typeof(data).eq."double") then
	    newdata = new((/ny  ,mx1/),double)
     else
	    newdata = new((/ny  ,mx1/),float)
     end if

     newdata(:,0:mx-1) = data             ; pass everything
     newdata(:,mx)     = (/ data(:,0) /)  ; value only

;    end if

    if((.not.ismissing(newdata!1)) .and. iscoord(data,newdata!1)) then 
	    if (typeof(lon).eq."float".or.typeof(lon).eq."double") then
               newdata&$newdata!1$(mx) = newdata&$newdata!1$(0) + 360.0 * sign
	    else
               newdata&$newdata!1$(mx) = newdata&$newdata!1$(0) + \
			360.0 * floattointeger(sign)
	    end if
    end if

    return(newdata)

end

;
; the remaining functions are all of ndvUpdateFunc type. They can 
; setvalues of objects based on dynamic considerations, or do other
; things. They should not create graphic objects.
; All variables should be declared local.
;
; To work properly they must return one of three values:
;
; -1:  an error occurred, usually an invalid parameter
;  0:  no action was taken that might affect the appearance of the plot
;  1:  a setvalues or some other action occurred that should result in a
;	redraw.
;
; If 1 is returned unnecessarily the result will be superfluous redraws of
; the plot in the ndv XWorkstation.
;

function plot_title (plot : graphic, \
		     ltitle : graphic, ltext : string, \
		     rtitle : graphic, rtext : string,
		     max_height : float, height_factor : float )

local vp_width,c1,c2,height1,height2,asp1,asp2,vpon1,vpon2,text1,text2,tchars,\
	height,asp,sp_per_char,vpon,ret

begin

	ret = 0
;
; need check to see if these objects are what they are supposed to be
;
	getvalues plot
		"vpWidthF" 	: vp_width
	end getvalues

	getvalues ltitle
		"txFontAspectF" : asp1
		"txFontHeightF" : height1
		"txString" 	: text1
		"vpOn"		: vpon1
	end getvalues

	getvalues rtitle
		"txFontAspectF" : asp2
		"txFontHeightF" : height2
		"txString" 	: text2
		"vpOn"		: vpon2
	end getvalues
;
;	uniform aspect and height enforced for these titles
;
	asp = (asp1 + asp2) / 2.0
	c1 = stringtochar(ltext)
	c2 = stringtochar(rtext)
	tchars = sizeof(c1) + sizeof(c2) + 2
;
; assume constant spacing since that takes the most room
;
	sp_per_char = vp_width / tchars
	
	height = min((/max_height,sp_per_char/)) * height_factor

	if (sizeof(c1).lt.2) then
		if (vpon1 .ne. False) then
			setvalues ltitle
				"vpOn" : False
			end setvalues
			ret = 1
		end if
	else
		if (.not. (asp1 .eq. asp .and. \
			   height1 .eq. height .and. \
			   text1 .eq. ltext .and. \
			   vpon1 .eq. True)) then
		
			setvalues ltitle
				"txFontAspectF" : asp
				"txFontHeightF" : height
				"txString" : ltext
				"vpOn" : True
			end setvalues
			ret = 1
		end if
	end if

	if (sizeof(c2).lt.2) then
		if (vpon2 .ne. False) then
			setvalues rtitle
				"vpOn" : False
			end setvalues
			ret = 1
		end if
	else
		if (.not. (asp2 .eq. asp .and. \
			   height2 .eq. height .and. \
			   text2 .eq. ltext .and. \
			   vpon2 .eq. True)) then
		
			setvalues rtitle
				"txFontAspectF" : asp
				"txFontHeightF" : height
				"txString" : rtext
				"vpOn" : True
			end setvalues
			ret = 1
		end if
	end if
	
	print(ret)
	return ret
end	

function pos_xy_anno (base : graphic, xy : graphic)

local vpheight,vpwidth,vpx,xyx,bb,bb1,vpheight0,vpwidth0, \
	am,opos,opos0,ret

begin
      ret = 0

      getvalues base
        "vpHeightF"    : vpheight
        "vpXF"         : vpx
        "vpWidthF"     : vpwidth
      end getvalues

      getvalues xy
	"vpAnnoManagerId" : am
        "vpHeightF"    : vpheight0
        "vpWidthF"     : vpwidth0
        "vpXF"         : xyx
      end getvalues

      vpwidth = min((/vpheight,vpwidth * .625/))

      if (.not. (vpwidth .eq. vpwidth0 .and. \
		 vpheight .eq. vpheight0)) then
	      print("vpwidth: " + vpwidth + " vpwidth0: " + vpwidth0)
	      print("vpheight: " + vpheight + " vpheight0: " + vpheight0)
	      setvalues xy
        	"vpHeightF"    : vpheight
	        "vpWidthF"     : vpwidth
	      end setvalues
	      ret = 1
      end if

      bb = NhlGetBB(base)
      bb1 = NhlGetBB(xy)


;
; unfortunately you can't really get the right edge of the map plot with
; tickmark, because the xy has already been added as an annotation, and
; because the maptick is an overlay, its tickmarks have been appropriated
; by the map, and therefore its bounding box is simply the logLin viewport.
; this code will make the separation seem too large when the map projection
; is changed, oh well.
;
;      print(bb)
;      print(bb1)

      opos = 0.001 + max((/0.0,xyx-bb1(2)/)) / vpwidth
;
;      print ("opos: " + opos + " xyx: " + xyx)
;
      getvalues am
	"amOrthogonalPosF" : opos0
      end getvalues

      if (.not. (opos .eq. opos0)) then
	      print("opos: " + opos + " opos0: " + opos0)
	      setvalues am
		"amOrthogonalPosF" : opos
	      end setvalues
	      ret = 1
      end if

      print(ret)
      return ret	
end


function map_tickmarks (map : graphic, tm : graphic)
	
local	vpxf,vpyf,vpwf,vphf,min_lon,max_lon,min_lat,max_lat,center_lon, \
	projection, \
	lon_range,lat_range,lcheck_arr,lspcng_arr,mlspcng_arr, \
	lon_ind,lon_spcng,mlon_spcng,lat_ind,lat_spcng,mlat_spcng, \
	yvalues,myvalues,ylabels,indexy,xvalues,xlabels,mxvalues,indexx, \
	quot,offset,height, \
	major_length,minor_length,int_min_lat,int_min_lon,sign,round_val,tval,\
	center_lat,center_rot,xmin0,ymin0,xmax0,ymax0,xbmode0,ylmode0, \
	xlabels0,ylabels0,xvalues0,yvalues0,mxvalues0,myvalues0,\
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

;    print(min_lon)
;    print(center_lon)
;   print(projection)

    if (projection.ne.8 .or. center_lat.ne.0.0 .or. center_rot.ne.0.0 ) then
        setvalues tm
		"vpOn" : False
	end setvalues
    else
        setvalues tm
		"vpOn" : True
	end setvalues
    end if

    if (fabs(max_lon - min_lon) .ge. 360) then
;	print("doing this")
	tval = (max_lon + min_lon) / 2.0 
;	print(tval)
	quot = floattointeger(tval / 180.0)
;	print(quot)
	offset = tval - (quot * 180.0)
;	print(offset)
	if (offset.gt.0.0) then
	    max_lon = max_lon - offset
	    min_lon = min_lon - offset
        end if
    end if
	
;    print(max_lon)
;    print(min_lon)
    lon_range = max_lon - min_lon   ; longitude range
    lat_range = max_lat - min_lat   ; latitude range

; Create a TickMark object to label the lat/lon grid.

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
    xlabels  = new(dimsizes(xvalues),string)

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

    indexx = ind((xvalues2).lt.0)
    if(.not.all(ismissing(indexx)))
        xlabels(indexx) = fabs(xvalues2(indexx)) + "W"     ; west
    end if
    delete(indexx)

    indexx = ind(xvalues2.gt.0.and.xvalues2.lt.180)
    if(.not.all(ismissing(indexx)))
        xlabels(indexx) = fabs(xvalues2(indexx)) + "E"     ; east
    end if
    delete(indexx)

    indexx = ind(xvalues2.eq.0.or.fabs(xvalues2).eq.180)
    if(.not.all(ismissing(indexx)))
        xlabels(indexx) = fabs(xvalues2(indexx))           ; 0 or 180
    end if
    delete(indexx)

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

    if (.not. (xmin0 .eq. min_lon .and. \
	       xmax0 .eq. max_lon .and. \
	       ymin0 .eq. min_lat .and. \
	       ymax0 .eq. max_lat .and. \
	       xbmode0 .eq. 2 .and. \             ; "Explicit" == 2
	       all(xlabels0 .eq. xlabels) .and. \
	       all(xvalues0 .eq. xvalues) .and. \
	       all(mxvalues0 .eq.mxvalues) .and. \
	       ylmode0 .eq. 2 .and. \
	       all(ylabels0 .eq. ylabels) .and. \
	       all(yvalues0 .eq. yvalues) .and. \
	       all(myvalues0 .eq.myvalues))) then

	    print("first")	       
	    ret = 1
	    setvalues tm
	       "trXMinF"	  : min_lon
	       "trXMaxF"	  : max_lon
	       "trYMinF"	  : min_lat
	       "trYMaxF"	  : max_lat

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
    major_length = (ylength0 + xlength0) / 2.
    minor_length = (ymlength0 + xmlength0) / 2.
    height = (xbheight0 + ylheight0) / 2.0

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

	    print("second")	       
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

    print(ret)
    return ret

end


function set_map_limits_from_object(map : graphic, \
				    obj : graphic, \
				    relative_lon : logical,
				    relative_lat : logical)

local xmin,ymin,xmax,ymax,latlon0,xmin0,ymin0,xmax0,ymax0,rel_lat0,rel_lon0

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
		"mpRelativeCenterLon" : rel_lon0
		"mpRelativeCenterLat" : rel_lat0
	end getvalues

	if ( latlon0 .eq. 1 .and. \
	     xmin0 .eq. xmin .and. \
	     ymin0 .eq. ymin .and. \
	     xmax0 .eq. xmax .and. \
	     ymax0 .eq. ymax .and. \
	     rel_lat0 .eq. relative_lat .and. \
	     rel_lon0 .eq. relative_lon ) then
		return 0
	end if
	 
	print("latlon " + latlon0)
	print("xmin: " + xmin + " xmin0: " + xmin0)
	print("ymin: " + ymin + " ymin0: " + ymin0)
	print("xmax: " + xmax + " xmax0: " + xmax0)
	print("ymax: " + ymax + " ymax0: " + ymax0)
	print("rel_lat: " + relative_lat + " rel_lat0: " + rel_lat0)
	print("rel_lon: " + relative_lon + " rel_lon0: " + rel_lon0)

	setvalues map
		"mpLimitMode" : "latlon"
		"mpMinLonF"   : xmin
		"mpMaxLonF"   : xmax
		"mpMinLatF"   : ymin
		"mpMaxLatF"   : ymax
		"mpRelativeCenterLon" : relative_lon
		"mpRelativeCenterLat" : relative_lat
	end setvalues

	return 1
end
