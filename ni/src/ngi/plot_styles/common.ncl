undef("level_colors")
undef("plot_titles")
undef("uniform_tickmarks")
undef("vc_scalar_field_color")
undef("SetContourLevels")
undef("set_vector_size_params")
undef("SetVectorLevels")

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
; Add three subtitles to top of plot (left, center, and right).
;
; Input: plot to add titles to, three text items representing each title,
;        three strings representing the titles, and a max height and
;        height factor for calculating the font height.
;
; Output: 0 if okay, 1 if not.
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
    tchars = sizeof(c1) + sizeof(c2) + sizeof(c3) + 3
;
; assume constant spacing since that takes the most room
;
    sp_per_char = vp_width / tchars
    
    height = min((/max_height,sp_per_char/)) * height_factor

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
