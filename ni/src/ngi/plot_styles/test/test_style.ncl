undef("spread_colors")
undef("spread_colors_around_level_val")
undef("map_tickmarks")
undef("pos_xy_anno")
undef("dim_average")
undef("plot_title")
undef("fix_longitude")
undef("fix_longitude_coord")
undef("set_map_limits_from_object")
undef("test_func")
undef("difference")
undef("match_contour_plots")
undef("add_annotations")
undef("match_viewport")
undef("adjust_plot_spacing")
undef("set_colormap")
undef("adjust_labelbar")
undef("adjust_tickmarks")
undef("cyclelon")
undef("cycleloncoord")
undef("match_map_transform")
undef("set_map_limits")

function test_func ( value )

begin
	print("executing test func" + value)
	return 0
end


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

function difference(data1, data2)

local is,data

begin
	if (any(dimsizes(data1) - dimsizes(data2)) .ne. False) then
		print("error: non-conformable data")
		return 0
	end if
		 
 	is = isatt(data1,(/"_FillValue","missing_value"/))
	if (is(0).eq.False .and. is(1).eq.True) then
		data1@_FillValue = data1@missing_value
	end if
 	is = isatt(data2,(/"_FillValue","missing_value"/))
	if (is(0).eq.False .and. is(1).eq.True) then
		data2@_FillValue = data2@missing_value
	end if

	data = data1
	data = data1 - data2

	return data
end

function spread_colors (plot : graphic, \
			min_index : integer, \
			max_index : integer)

local ncols,lcount,fcols,icols,stride,rem,start,minix,maxix,nc,fmin,fmax, \
	colres,levelcountres

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
	
function cycleloncoord(lonin[*],start_lon,end_lon)

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

function fix_longitude_coord(lon[*],start_lon,end_lon)

;
; Add a cyclic point a to coord var (representing longitude)

local dims, newdata, ny, mx, mx1, sizes,lon,maxloninc,i,sign, \
	loninc,monotonic,nonmono_ix1, nonmono_ix2,llon,tmp
begin

    if (start_lon .gt. end_lon) then
	return cycleloncoord(lon,start_lon,end_lon)
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

function cyclelon(data[*][*],start_lon,end_lon)

local lon,lat,newlon,size1,size2,pivot,newdata

begin

	print("in cyclelon")

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

function fix_longitude(data[*][*],start_lon,end_lon)

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
	return cyclelon(data,start_lon,end_lon)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;

;
; This function defines a colormap in the given workstation.
;
; Input: the plot object containing the workstation and the color map.
;
; Output: 0 if okay, 1 if not.
;
function set_colormap (plot : graphic, colormap)
begin
  if (.not. isatt(plot,"ndvWks"))
    print("SetColormap: using default color map")
    return -1
  end if

  if(all(colormap.ne.""))
    setvalues plot@ndvWks
      "wkColorMap" : colormap
    end setvalues
    return 1
  end if
  return 0
end

; Function: match_viewport
; Description: Matches the viewport size of an array of views to the size
; of the model view. 
;
function match_viewport \
	(model_view : graphic, views[*] : graphic)

local i,mheight,mwidth,vheight,vwidth

begin
	ret = 0
	getvalues model_view
		"vpWidthF" : mwidth
		"vpHeightF" : mheight
	end getvalues

	do i = 0, dimsizes(views) - 1
		getvalues views(i)
			"vpWidthF" : vwidth
			"vpHeightF" : vheight
		end getvalues
		if (mwidth .ne. vwidth .or. \
		    mheight .ne. vheight) then
			setvalues views(i)
				"vpWidthF" : mwidth
				"vpHeightF" : mheight
			end setvalues
			ret = 1
		end if
	end do
	
	return ret
end				

;
; adjust plot spacing
;
function adjust_plot_spacing(model_view : graphic, views[*] : graphic)

local i,mheight,mwidth,vy,vheight,vbboxes,am,off,ret,blap,tlap

begin
	ret = 0
	getvalues model_view
		"vpWidthF" : mwidth
		"vpHeightF" : mheight
	end getvalues

	vbboxes = NhlGetBB(views)
;
; this is cheating; estimating the effect of the map tickmarks, which
; due to HLU difficulties are hard to place (this should be fixed)
;
        blap = (mheight / 0.6) * 0.1
	do i = 0, dimsizes(views) - 1
		getvalues views(i)
			"vpYF" : vy
			"vpHeightF" : vheight
			"vpAnnoManagerId" : am
		end getvalues
	        tlap = vbboxes(i,0) - vy
		print("b " + blap + " t " + tlap + " h " + mheight)
		off = (blap + tlap) / mheight + 0.08
		getvalues am
			"amOrthogonalPosF" : oldoff
		end getvalues
		print ("old" + oldoff)
		if (off .ne. oldoff) then
			print("match vp off " + off)
			setvalues am
				"amOrthogonalPosF" : off
			end setvalues
			ret = 1
		end if
		blap = 0
	end do
	
	return ret
end				

;
; Description: spreads a range of a color table in both directions from
; a defined level value. Useful for plots such a difference plot where a
; particular level (e.g. 0) should always be the same color even though
; it is not known in advance how many levels will be on each side of the 
; color. 
;

function spread_colors_around_level_val \
	(plot : graphic, \
	 min_index : integer, \
	 max_index : integer, \
	 level_val : float)

local ncols,lcount,fcols,icols,levels,colors, \
	minix,maxix,cenix,valix,nc,tmin,tmax, \
	fmin,fmax,fcen, \
	colres,levelres,levelcountres

begin
	if (ismissing(plot))
		return -1
	end if
;	print(plot)

	if (isatt(plot,"ndvClass"))
		class = plot@ndvClass
		if (class.eq."contourPlotClass") then
			colres = "cnFillColors"
			levelcountres = "cnLevelCount"
			levelres = "cnLevels"	
		else
			if (class.eq."vectorPlotClass") then
				colres = "vcLevelColors"
				levelcountres = "vcLevelCount"
				levelres = "vcLevels"
			else
				return -1
			end if
		end if
	else
		return -1
	end if
	if (.not. isatt(plot,"ndvWks")) then
		return -1
	end if

	getvalues plot@ndvWks
		"wkColorMapLen" : ncols
	end getvalues
	getvalues plot
		levelcountres : lcount
		levelres : levels
		colres : colors
	end getvalues

;	print("numcolors " + ncols + "level count " + lcount)
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
	cenix = minix + nc / 2

	fmin = new(1,float)
	fmax = new(1,float)
	fcen = new(1,float)

	fmin = minix
	fmax = maxix
	fcen = cenix
	fcols = new(lcount + 1,float)

;	print(levels)
;	print("fmin " + fmin + " fmax " + fmax + " fcen " + fcen)

	tmax = min(ind(levels.gt.level_val))
	tmin = max(ind(levels.le.level_val))

	if (ismissing(tmin)) then
		fcols = fspan(fcen,fmax,lcount + 1)
	else
		if (ismissing(tmax)) then
			fcols = fspan(fmin,fcen,lcount + 1)
		else
			if (levels(tmax) - level_val .lt. \
			    level_val - levels(tmin)) then
				valix = tmax
			else
				valix = tmin
			end if
			fcols(0:valix) = fspan(fmin,fcen,valix+1)
			if (valix .lt. lcount) then
			     fcols(valix+1:lcount) = \
					fspan(fcen,fmax,lcount-valix)
			end if
	        end if
	end if

	icols = floattointeger(fcols + 0.5)

;	print(fcols)
;	print(icols)

	if (dimsizes(colors).ne.dimsizes(icols) .or. \
	    any(colors-icols) .ne. 0) then
		setvalues plot
			colres : icols
		end setvalues
		return 1
	end if

	return 0
end

;
; This function matches the contour settings of a second contour
; object to the first, as well as it can.
;
; Input: two contour plots
;
; Output: 1 if changes were made, 0 if no change necessary, -1 if error.
;

function match_contour_plots(contour1:graphic, contour2: graphic)

local mode1,spacing1,min1,max1,level1,fillcolors1 , \
      mode2,spacing2,min2,max2,level2,fillcolors2

begin

  class_name = NhlClassName(contour1)
  if(class_name.ne."contourPlotClass")
   print\
    ("match_contour_levels: the first argument must be a contour plot object.")
    	return -1
  end if
  class_name = NhlClassName(contour2)
  if(class_name.ne."contourPlotClass")
   print\
    ("match_contour_levels: the first argument must be a contour plot object.")
    	return -1
  end if

  getvalues contour1
	"cnLevelSelectionMode" : mode1
	"cnLevelSpacingF" : spacing1
	"cnMinLevelValF"  : min1
        "cnMaxLevelValF"  : max1
	"cnLevels"	  : levels1
	"cnFillColors"	  : fillcolors1
  end getvalues

  getvalues contour2
	"cnLevelSelectionMode" : mode2
	"cnLevelSpacingF" : spacing2
	"cnMinLevelValF"  : min2
        "cnMaxLevelValF"  : max2
	"cnLevels"	  : levels2
	"cnFillColors"	  : fillcolors2
  end getvalues

  if (mode1 .eq. 2) then 
	if (dimsizes(levels1).ne.dimsizes(levels2) .or. \
	    any(levels1 - levels2) .ne. False .or. \
	    any(fillcolors1 - fillcolors2) .ne. False) then
		setvalues contour2
		    "cnLevelSelectionMode" : 2
		    "cnLevels" : levels1
	 	    "cnFillColors" : fillcolors1
		end setvalues
        end if
        return 1
  end if

  if (spacing1 .ne. spacing2 .or. \
      min1 .ne. min2 .or. \
      max1 .ne. max2 .or. \
      dimsizes(fillcolors1).ne.dimsizes(fillcolors2) .or. \
      any(fillcolors1 - fillcolors2) .ne. False) then
	setvalues contour2
	    "cnLevelSelectionMode" : "manuallevels"
            "cnLevelSpacingF" : spacing1
	    "cnMinLevelValF"  : min1
            "cnMaxLevelValF"  : max1
	    "cnFillColors" : fillcolors1
	end setvalues
	return 1
  end if

  return 0
end


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
			   text1 .eq. ltext)) then
		
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
			   text2 .eq. rtext)) then
		
			setvalues rtitle
				"txFontAspectF" : asp
				"txFontHeightF" : height
				"txString" : rtext
				"vpOn" : True
			end setvalues
			ret = 1
		end if
	end if
	
;	print(ret)
	return ret
end	

function pos_xy_anno (base : graphic, ca : graphic, xy : graphic)

local width,vpheight,vpwidth,vpx,xyx,bb,bb1,vpheight0,vpwidth0, \
	am,opos,opos0,ret,\
	tmdisplay,xmin,xmax,ymin,ymax,ylmode,ylvalues,ylminorvalues,\
	tmdisplay0,xmin0,xmax0,ymin0,ymax0,ylmode0,ylvalues0,ylminorvalues0

begin
      ret = 0

      getvalues base
        "vpHeightF"    : vpheight
        "vpXF"         : vpx
        "vpWidthF"     : width
	"pmTickMarkDisplayMode" : tmdisplay
	"trYMinF"	: ymin
	"trYMaxF"	: ymax
      end getvalues

      if (tmdisplay .gt. 0) then
	      getvalues base	
		"tmYLMode"  :	ylmode
	      end getvalues

	      if (ylmode .eq. 2) then
		      getvalues base	
			"tmYLValues" :  ylvalues
			"tmYLMinorValues" : ylminorvalues
		      end getvalues
	      end if
      end if

;      getvalues ca
;	"caXMinV"	: xmin
;	"caXMaxV"	: xmax
;      end getvalues

      getvalues xy
	"vpAnnoManagerId" : am
        "vpHeightF"    : vpheight0
        "vpWidthF"     : vpwidth0
        "vpXF"         : xyx
	"pmTickMarkDisplayMode" : tmdisplay0
	"trYMinF"	: ymin0
	"trYMaxF"	: ymax0
;	"trXMinF"	: xmin0
;	"trXMaxF"	: xmax0
      end getvalues

      ylmode0 = 0
      if (tmdisplay0 .ge. 0) then
	      getvalues xy	
		"tmYLMode"  :	ylmode0
	      end getvalues
	      if (ylmode0 .eq. 2) then
		      getvalues xy	
			"tmYLValues" :  ylvalues0
			"tmYLMinorValues" : ylminorvalues0
		      end getvalues
	      end if
      end if

      vpwidth = min((/vpheight,width * .375/))

      if (.not. (vpwidth .eq. vpwidth0 .and. \
		 vpheight .eq. vpheight0 .and. \
		 ymin .eq. ymin0 .and. \
		 ymax .eq. ymax0 .and. \
;		 xmin .eq. xmin0 .and. \
;		 xmax .eq. xmax0 .and. \
		 tmdisplay .eq. tmdisplay0)) then

;	      print("vpwidth: " + vpwidth + " vpwidth0: " + vpwidth0)
;	      print("vpheight: " + vpheight + " vpheight0: " + vpheight0)

	      setvalues xy
        	"vpHeightF"    : vpheight
	        "vpWidthF"     : vpwidth
		"pmTickMarkDisplayMode" : tmdisplay
		"trYMinF"	: ymin
		"trYMaxF"	: ymax
;		"trXMinF"	: xmin
;		"trXMaxF"	: xmax
	      end setvalues

	      ret = 1
      end if
      if (ylmode .eq. 2 .and. ylmode0 .ne. 2) then
      		ylvalues0 = new(dimsizes(ylvalues),float)
      		ylminorvalues0 = new(dimsizes(ylminorvalues),float)
      end if
      if (tmdisplay .gt. 0 .and. ylmode .eq. 2) then
          if (ylmode0 .ne. 2 .or. \
	      dimsizes(ylvalues) .ne. dimsizes(ylvalues0) .or. \
	      any(ylvalues - ylvalues0) .ne. False .or. \
	      dimsizes(ylminorvalues) .ne. dimsizes(ylminorvalues0) .or. \
	      any(ylminorvalues - ylminorvalues0) .ne. False) then
	      setvalues xy	
		"tmYLMode"  :	ylmode
		"tmYLValues" :  ylvalues
		"tmYLMinorValues" : ylminorvalues
	      end setvalues
	      ret = 1
           end if
      end if

      bb = NhlGetBB(base)
      bb1 = NhlGetBB(xy)

      getvalues xy
        "vpXF"         : xyx
      end getvalues

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
;      print(width)

      opos = 0.02 + max((/0.0,xyx-bb1(2)/)) / width
;
;      print ("opos: " + opos + " xyx: " + xyx)
;
      getvalues am
	"amOrthogonalPosF" : opos0
      end getvalues

      if (.not. (opos .eq. opos0)) then
;	      print("opos: " + opos + " opos0: " + opos0)
	      setvalues am
		"amOrthogonalPosF" : opos
	      end setvalues
	      ret = 1
      end if

;      print(ret)
      return ret	
end
;
; Function: adjust_labelbar
;
; Description: 
; Adjusts the labelbar label stride and font height automatically
; if no parameters are modified from their default values, or under user
; control otherwise. In the automatic mode, the stride is repeatedly set
; until the labelbar font height is within a bounded range of a reference
; value, obtained from the font height of the reference plot tickmark labels.
; Under user control, the height is scaled by a user-supplied factor from
; the reference value, and the stride may also be set.
;
; Parameters:
; tick: a plot with tickmark labels from which to get a reference height
; for the labelbar labels. If it doesn't exist it's an error. It it has
; no tickmark labels a value of 0.015 * the viewport width is assumed
;
; plot: the graphic to which the labelbar is attached. It could be the
; same as tick.
;
; stride: the stride value. If 0, the stride is calculated automatically
;
; font_height_factor: the factor applied to the reference font height
; supplied by tick. If 0, then the labelbar determines font height based
; on the stride and its own geometry.
;
function adjust_labelbar(reftick : graphic, plot : graphic, \
			 stride : integer, font_height_factor : float)

local dmode,vwidth,fheight,fheight0,stride0,\
	automanage0,max_llen,size,orient,direction,spacing,fstride,ret

begin
	ret = 0 

	if (.not. (isatt(reftick,"ndvClass") .and. \
		NhlIsTransform(reftick) .and. \
	    isatt(plot,"ndvClass") .and. NhlIsTransform(plot))) then
		return -1;
	end if

	getvalues plot
		"pmLabelBarDisplayMode" : dmode
		"vpWidthF" : vwidth
	end getvalues

;
; return if LabelBar not displayed
;
	if (dmode .lt. 1) then
		return 0
	end if
;
; see if the reference tickmark plot is displaying tickmarks
; if not use a standard reference size
;
	getvalues reftick
		"pmTickMarkDisplayMode" : dmode
		"vpWidthF" : vwidth
	end getvalues

	if (dmode .lt. 1) then
		fheight = min((/0.009,vwidth * 0.015/))
	else
		getvalues reftick
			"tmXBLabelFontHeightF" : fheight
		end getvalues
	end if

	getvalues plot
		"lbLabelStride" : stride0
		"lbLabelFontHeightF" : fheight0
		"lbAutoManage" : automanage0
		"lbMaxLabelLenF" : max_llen
		"lbMinLabelSpacingF" : spacing	
		"lbOrientation" : orient
		"lbLabelDirection" : direction	
	end getvalues

	print("stride0 " + stride0 + " stride " + stride)
	if (font_height_factor .gt. 0) then
		fheight = font_height_factor * fheight
	end if

	if (fheight .ne. fheight0 \
	    .or. automanage0 .eq. 1) then
		ret = 1
		setvalues plot
			"lbAutoManage" : False
			"lbLabelFontHeightF" : fheight
		end setvalues
;
; setting the font height can affect these resource values, so retrieve
; them again.

		getvalues plot
			"lbMaxLabelLenF" : max_llen
			"lbMinLabelSpacingF" : spacing	
		end getvalues
	end if
		
	if (stride .gt. 0 .and. stride .ne. stride0) then
;
; explicit stride
;

		setvalues plot
			"lbLabelStride" : stride
		end setvalues
		return 1
	end if
;
; adjust the spacing value for a stride of 1
;
	if (stride0 .gt. 1) then
		spacing = spacing / stride0
	end if

	stride = stride0
	if ((orient .eq. 0 .and. direction .eq. 1) .or. \
	    (orient .eq. 1 .and. direction .eq. 0) ) then
		size = max_llen + fheight / 2.0
	else
		size = 1.5 * fheight
	end if		

	if (spacing .gt. 0) then
		fstride = size / spacing
		print(fstride)
		stride = floattointeger(fstride)
		print(stride)
		if (fstride .gt. stride) then
			stride = stride + 1
		end if
	end if
	
	if (stride .ne. stride0) then
		setvalues plot
			"lbLabelStride" : stride
		end setvalues
		ret = 1
	end if

	return ret
end


function adjust_tickmarks(reftick : graphic, plot : graphic, \
			  xstride : integer, ystride : integer, \
			  font_height_factor : float)

local dmode,vwidth,fheight,xfheight,yfheight,\
	xmaxlen,xspacing,oxstride,ixstride,fxstride,\
	ymaxlen,yspacing,oystride,iystride,fystride


begin
	ret = 0

	if (.not. (isatt(reftick,"ndvClass") .and. \
		   NhlIsTransform(reftick) .and. \
	           isatt(plot,"ndvClass") .and. NhlIsTransform(plot))) then
		return -1;
	end if

	getvalues plot
		"pmTickMarkDisplayMode" : dmode
	end getvalues
;
; return if TickMark not displayed
;
	if (dmode .lt. 1) then
		return 0
	end if

;
; see if the reference tickmark plot is displaying tickmarks
; if not use a standard reference size
; we're assuming here that the ref tickmark plot uses the XB labels, hardly
; a warranted assumption
;
	getvalues reftick
		"pmTickMarkDisplayMode" : dmode
		"vpWidthF" : vwidth
	end getvalues

	if (dmode .lt. 1) then
		fheight = min((/0.009,vwidth * 0.015/))
	else
		getvalues reftick
			"tmXBLabelFontHeightF" : fheight
		end getvalues
	end if
;
; first set the font heights if necessary
;
	getvalues plot
		"tmXBLabelFontHeightF" :     xfheight
		"tmYLLabelFontHeightF" :     yfheight
	end getvalues

	if (font_height_factor .gt. 0) then
		fheight = font_height_factor * fheight
	end if

	print("fheight " + fheight + " xf " + xfheight + " yf " + yfheight)
	if (xfheight .ne. fheight .or. yfheight .ne. fheight)
		ret = 1
		setvalues plot
			"tmXBLabelFontHeightF" :     fheight
			"tmYLLabelFontHeightF" :     fheight
		end setvalues
	end if
	
;
; get all the tickmark info
;
	getvalues plot
		"tmXBMaxLabelLenF" :     xmaxlen
		"tmXBMinLabelSpacingF" : xspacing
		"tmXBLabelStride" : 	 oxstride
		"tmYLMaxLabelLenF" :     ymaxlen
		"tmYLMinLabelSpacingF" : yspacing
		"tmYLLabelStride" : 	 oystride
	end getvalues
;
; get the spacing for a stride of 1
;
	if (oxstride .gt. 1) then
		xspacing = xspacing / oxstride
	end if
	print ("xmaxlen " + xmaxlen + " xspacing " + xspacing + " oxstride " + oxstride)
	ixstride = oxstride
	if (xspacing .gt. 0) then
		fxstride = (xmaxlen + fheight / 2.0) / xspacing
		print(fxstride)
		ixstride = floattointeger(fxstride)
		print(ixstride)
		if (fxstride .gt. ixstride) then
			ixstride = ixstride + 1
		end if
	end if
	print(ixstride)

	if (oystride .gt. 1) then
		yspacing = yspacing / oystride
	end if
	iystride = oystride
	if (ymaxlen + fheight .gt. yspacing .and. yspacing .gt. 0) then
		fystride = (ymaxlen + fheight) / yspacing
		iystride = floattointeger(fystride)
		if (fystride .gt. iystride) then
			iystride = iystride + 1
		end if
	end if

	if (xstride .ne. 0) then
		ixstride = xstride
	end if
	if (ystride .ne. 0) then
		iystride = ystride
	end if
	if (ixstride .ne. oxstride .or. iystride .ne. oystride) then
		ret = 1
		setvalues plot
			"tmXBLabelStride" : 	 ixstride
			"tmYLLabelStride" : 	 iystride
		end setvalues
	end if
		
	return ret
end

function map_tickmarks (map : graphic, tm : graphic,\
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

function add_annotations(base : graphic, \
			 annos[*] : graphic)

local cur_annos,new_annos,cur_count,new_count

begin

;	if (.not. NhlIsTransform(base)) then
;		return -1
;	end if

	print("in add_annotations")
	getvalues base
		"pmAnnoViews" : cur_annos
	end getvalues

	cur_count = dimsizes(cur_annos)
	new_count = cur_count + dimsizes(annos)
	new_annos = new(new_count,graphic)

	print(cur_count + "   " + new_count)
	new_annos(0:cur_count-1) = cur_annos
	new_annos(cur_count:new_count-1) = annos

	setvalues base
		"pmAnnoViews" : new_annos
	end setvalues

	return 1
end

;
; this function propagates the map transformation of map1 to map2
;

function match_map_transform(map1 : graphic, map2 : graphic)

local ret,proj1,proj2,clon1,clon2,clat1,clat2,crot1,crot2,lmode1,lmode2, \
	xmin1,xmax1,ymin1,ymax1,xmin2,xmax2,ymin2,ymax2,\
	rlon1,rlat1,rlon2,rlat2

begin

    ret = 0

    getvalues map1
	"mpProjection" : proj1
      	"mpCenterLonF" : clon1
      	"mpCenterLatF" : clat1
      	"mpCenterRotF" : crot1
	"mpLimitMode"  : lmode1
    end getvalues

    getvalues map2
	"mpProjection" : proj2
      	"mpCenterLonF" : clon2
      	"mpCenterLatF" : clat2
      	"mpCenterRotF" : crot2
	"mpLimitMode"  : lmode2
    end getvalues

    if (proj1 .ne. proj2 .or. \
	clon1 .ne. clon2 .or. \
	clat1 .ne. clat2 .or. \
	crot2 .ne. crot2 .or. \
	lmode1 .ne. lmode2) then
	ret = 1;
    end if 
    if (ret .eq. 1 .and. lmode1 .eq. 0) then
	setvalues map2
		"mpLimitMode"  : lmode1
		"mpProjection" : proj1
	      	"mpCenterLonF" : clon1
		"mpCenterLatF" : clat1
	      	"mpCenterRotF" : crot1
	end setvalues
	return ret
    end if
	
    if (lmode1 .eq.1) then
 	getvalues map1
      		"mpMinLonF"    : xmin1
      		"mpMaxLonF"    : xmax1
      		"mpMinLatF"    : ymin1
      		"mpMaxLatF"    : ymax1
      		"mpRelativeCenterLon" : rlon1
      		"mpRelativeCenterLat" : rlat1
    	end getvalues

    	getvalues map2
      		"mpMinLonF"    : xmin2
      		"mpMaxLonF"    : xmax2
      		"mpMinLatF"    : ymin2
      		"mpMaxLatF"    : ymax2
      		"mpRelativeCenterLon" : rlon2
      		"mpRelativeCenterLat" : rlat2
    	end getvalues

	if (ret .eq. 1 .or. \
	    xmin1 .ne. xmin2 .or. \
	    xmax1 .ne. xmax2 .or. \
	    ymin1 .ne. ymin2 .or. \
	    ymax1 .ne. ymax2 .or. \
	    rlon1 .ne. rlon2 .or. \
	    rlat1 .ne. rlat2) then

	        print("LatLon: xmin1 " + xmin1 + " xmin2 " + xmin2 + \
			" ymin1 " + ymin1 + " ymin2 " + ymin2)
		ret = 1
		setvalues map2
			"mpLimitMode"  : lmode1
			"mpProjection" : proj1
		      	"mpCenterLonF" : clon1
      			"mpCenterLatF" : clat1
		      	"mpCenterRotF" : crot1
      			"mpMinLonF"    : xmin1
      			"mpMaxLonF"    : xmax1
	      		"mpMinLatF"    : ymin1
      			"mpMaxLatF"    : ymax1
      			"mpRelativeCenterLon" : rlon1
	      		"mpRelativeCenterLat" : rlat1
		end setvalues
	end if
    else 	
 	getvalues map1
      		"mpLeftNPCF"    : xmin1
      		"mpRightNPCF"    : xmax1
      		"mpBottomNPCF"    : ymin1
      		"mpTopNPCF"    : ymax1
    	end getvalues

    	getvalues map2
      		"mpLeftNPCF"    : xmin2
      		"mpRightNPCF"    : xmax2
      		"mpBottomNPCF"    : ymin2
      		"mpTopNPCF"    : ymax2
    	end getvalues

	if (ret .eq. 1 .or. \
	    xmin1 .ne. xmin2 .or. \
	    xmax1 .ne. xmax2 .or. \
	    ymin1 .ne. ymin2 .or. \
	    ymax1 .ne. ymax2)

	        print("NPC: xmin1 " + xmax1 + " xmax1 " + xmax1 + \
			" ymin1 " + ymin1 + " ymax1 " + ymax1)
		ret = 1
		setvalues map2
			"mpLimitMode"  : "npc"
			"mpProjection" : proj1
		      	"mpCenterLonF" : clon1
      			"mpCenterLatF" : clat1
		      	"mpCenterRotF" : crot1
      			"mpLeftNPCF"    : xmin1
      			"mpRightNPCF"    : xmax1
	      		"mpBottomNPCF"    : ymin1
      			"mpTopNPCF"    : ymax1
		end setvalues
	end if
    end if

    return ret

end

;
; this function is tailored for cylindrical equidistant, though it
; may be somewhat useful for other projections
;

function set_map_limits_from_object(map : graphic, \
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

function set_map_limits(map : graphic, \
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
		ret = set_map_limits_from_object(map,obj,0)
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
