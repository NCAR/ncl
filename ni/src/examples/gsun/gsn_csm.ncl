;
;      $Id: gsn_csm.ncl,v 1.2 1999-02-19 22:33:51 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1998                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       gsn_csm.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Mon Sep 28 14:08:49 MST 1995
;;
;;  Description: This script contains some specialized plotting functions
;;               used by CGD for the CSM processor. To use the functions
;;               and procedures in this script, you must have the line:
;;
;;                   load "gsn_csm.ncl"
;; 
;;               at the top of your NCL script, before the begin statement.
;;

;***********************************************************************;
; For every function and procedure defined in this script, undefine it  ;
; with a call to "undef" so it doesn't clash with other functions and   ;
; procedures with the same name.                                        ;
;***********************************************************************;

undef("gsn_add_cyclic_point")
undef("gsn_csm_map_ce")
undef("gsn_csm_map_polar")
undef("gsn_csm_map")
undef("gsn_csm_contour_map_polar")
undef("gsn_csm_contour_map_ce")
undef("gsn_csm_contour_map")
undef("gsn_csm_xy")

function gsn_add_cyclic_point(data[*][*]:float)
;
; Add a cyclic point in "x" to a 2D array
; for a lat/lon plot "x"  corresponds to "lon"
;                    "ny" corresponds to "nlat"
;                    "mx" corresponds to "mlon"
local dims, newdata, ny, mx, mx1
    lonlabels  = get_lon_labels(lonvalues)

; Create a TickMark object to label lat/lon grid.

    tickmark = create "tickmarks" tickMarkClass wks
       "vpXF"            : vpxf       ; Set the viewport location of the
    newdata = new((/ny  ,mx1/),float)

       "vpWidthF"        : vpwf       ; the map.
       "vpHeightF"       : vphf

    if((.not.ismissing(newdata!1)) .and. iscoord(data,newdata!1)) then 
        newdata&$newdata!1$(mx) = newdata&$newdata!1$(0) + 360.0
       "tmYLMinorValues"  : latvalues@minor
 

    attsetvalues_check(tickmark,tmres)

; Point tick marks outward and make labels the same size.
    return(ret)
end

;***********************************************************************;
; Procedure : pop_latlon_grid                                           ;
;                  wks: graphic                                         ;
;                 plot: graphic                                         ;
;              popgrid: string                                          ;
;                                                                       ;
; This procedure overlays a POP grid on a plot.                         ;
;***********************************************************************;

procedure pop_latlon_grid(wks:graphic,plot:graphic,popgrid:string)
local latlon_res, tmplon, lat_object, lon_object, valid_popgrids, \
popgrid_dir, popgrid_files, i, found 
begin
;
; Open POP grid that contains lat/lon information.
;
    popgrid_dir = "/fs/cgd/data0/shea/pop/"
    valid_popgrids = (/"POP23"/)       ; list of valid POP grids
local i, attnames, res, mp_res_index, map_object, tickmark_object, \
lft_str_object, tm_res_index, ctr_str_object, rgt_str_object, calldraw, \
callframe, left_string, center_string, right_string, sub_zone, main_zone, \
min_lat, max_lat, min_lon, max_lon, res2, tickmark_res
      print("pop_latlon_grid: "+popgrid+" is not a supported POP grid.")

      print("                 Supported POP grids include: " + valid_popgrids)

    calldraw      = True
    callframe     = True
    map_vpxf      = 0.1       ; X position of map plot in viewport.
    map_vpyf      = 0.9       ; Y position of map plot in viewport.
    map_vpwidthf  = 0.8       ; Viewport width and height for
    map_vpheightf = 0.8       ; map plot.

    left_string   = False     ; Default is no subtitles unless
    center_string = False     ; they are explicitly set with the
    right_string  = False     ; special "gsn" resources.
    tickmark_res  = False
    sub_zone      = 2         ; Zone for subtitles
      print("                 No grid will be overlaid.")
    res2 = resources
    end if

    i = 0
    found = False
    if((res2).and..not.any(ismissing(getvaratts(res2))))

    do while(i.lt.dimsizes(valid_popgrids).and..not.found)

        if(isatt(res2,"gsnDraw"))
            if(.not.res2@gsnDraw)
                calldraw = False
            end if
            delete(res2@gsnDraw)
        end if
        if(isatt(res2,"gsnFrame"))
            if(.not.res2@gsnFrame)
                callframe = False
            end if
            delete(res2@gsnFrame)
        end if

; Check for left subtitle at top. If it exists, create a TextItem
; object for it.

        if(isatt(res2,"gsnLeftString"))
            lft_str_object = create "left_string" textItemClass wks
              "txString" : res2@gsnLeftString
            end create
            left_string = True
            main_zone   = 3
            delete(res2@gsnLeftString)
        end if

; Check for center subtitle at top. If it exists, create a TextItem
; object for it.

        if(isatt(res2,"gsnCenterString"))
            ctr_str_object = create "center_string" textItemClass wks
              "txString" : res2@gsnCenterString
            end create
            center_string = True
            main_zone     = 3
            delete(res2@gsnCenterString)
        end if

; Check for right subtitle at top. If it exists, create a TextItem
; object for it.

        if(isatt(res2,"gsnRightString"))
            rgt_str_object = create "right_string" textItemClass wks
              "txString" : res2@gsnRightString
            end create
            right_string = True
            main_zone    = 3
            delete(res2@gsnRightString)
        end if
    latlon_res@gsnFrame             = False

; Create the map object.

    map_object = create "map" mapPlotClass wks
      "vpXF"            : map_vpxf
      "vpYF"            : map_vpyf
      "vpWidthF"        : map_vpwidthf
      "vpHeightF"       : map_vpheightf
      "mpGridAndLimbOn" : False    ; Turn off grid lines, limb line,
      "mpOutlineOn"     : False    ; and map outlines.
      "mpFillOn"        : True     ; Fill map boundaries according to
                                   ; "mpFillColors".
      "mpFillColors"    : (/"background","transparent","LightGray",\
                            "transparent"/)   ; (default,ocean,land,
                                              ;  inland water)
      "pmTitleDisplayMode" : "Always"
      "pmTitleZone"        : main_zone        ; Zone for main title
    end create

;
; By default, mpOutlineOn is False, unless mpFillOn is set to False,
; then it is set back to True.
;
    if((isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        setvalues map_object
          "mpOutlineOn" : True
        end setvalues
    end if
      exit
    end if

    if((res2).and..not.any(ismissing(getvaratts(res2))))
; Get list of resources.
        attnames = getvaratts(res2)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))
        if(dimsizes(dimsizes(res)).eq.1)

;***********************************************************************;
; Check to see if any resources were set.                               ;
;***********************************************************************;

            if(chartostring(res(0:1)).eq."tm")
                tickmark_res = True
                num_tickmark_res = 1   ; Only 1 tick mark resource.
            else
              setvalues map_object
                  attnames : res2@$attnames$
              end setvalues
            end if
        else
            tm_res_index = ind(chartostring(res(:,0:1)).eq."tm")
            if(.not.all(ismissing(tm_res_index))) 
              tickmark_res = True
              num_tickmark_res = 2   ; Indicates there are two or more 
                                     ; tick mark resources set.
            end if
            mp_res_index = ind(chartostring(res(:,0:1)).ne."tm")
            if(.not.all(ismissing(mp_res_index)))
                mpres = True
                do i = 0,dimsizes(mp_res_index)-1
                    mpres@$attnames(mp_res_index(i))$ = res2@$attnames(mp_res_index(i))$
                end do
                attsetvalues(map_object,mpres)
            end if
        end if
    end if

; Retrieve the view port location of the map plot so we know where
; to put titles.

    getvalues map_object
      "vpXF"         : vpxf
      "vpYF"         : vpyf
      "vpWidthF"     : vpwidthf
      "vpHeightF"    : vpheightf
      "mpMinLonF"    : min_lon
      "mpMaxLonF"    : max_lon
      "mpMinLatF"    : min_lat
      "mpMaxLatF"    : max_lat
      "mpCenterLonF" : center_lon
    end getvalues

    lon_range = max_lon - min_lon   ; longitude range
    lat_range = max_lat - min_lat   ; latitude range

    if( center_lon.ne.0.and.lon_range.ne.360)
        print("The resource mpCenterLonF can only be non-zero if")
        print("the longitude range is 360. Your plot may be wrong.")
    end if

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

; Calculate values for latitude minor/major tickmark locations.
    yvalues  = ispan(floattointeger(min_lat),\
                     floattointeger(max_lat),lat_spcng)
    myvalues = ispan(floattointeger(min_lat),\
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
        ylabels(indexy) = fabs(yvalues(indexy)) + "N"    ; north
    end if
    delete(indexy)

    indexy = ind(yvalues.eq.0)
    if(.not.all(ismissing(indexy)))
        ylabels(indexy) = fabs(yvalues(indexy))          ; equator
    end if
    delete(indexy)

; Calculate values for longitude major tickmark locations.
    xvalues = ispan(floattointeger(min_lon),\
                    floattointeger(max_lon),lon_spcng)

; Convert to (-180,180) range if necessary.
    if(any(xvalues.gt.180))
        xvalues = xvalues-180
        xvalues = ispan(floattointeger(min(xvalues)),\
                        floattointeger(max(xvalues)),lon_spcng)
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

; Create a TickMark object to label lat/lon grid.

    tickmark_object = create "tickmarks" tickMarkClass wks
       "vpXF"            : vpxf       ; Set the viewport location of the
       "vpYF"            : vpyf       ; tick marks to be the same as 
       "vpWidthF"        : vpwidthf   ; the map.
       "vpHeightF"       : vpheightf

       "tmYLDataBottomF" : min_lat    ; Use the lat/lon limits
       "tmYLDataTopF"    : max_lat    ; from the coordinate variables.
       "tmXBDataLeftF"   : min_lon
       "tmXBDataRightF"  : max_lon

       "tmXBMode"         : "Explicit" ; Indicate that we want to 
       "tmXBLabels"       : xlabels    ; explicitly label the X axis.
       "tmXBValues"       : xvalues
       "tmXBMinorValues"  : mxvalues

       "tmYLMode"         : "Explicit" ; Indicate that we want to 
       "tmYLLabels"       : ylabels    ; explicitly label the Y axis.
       "tmYLValues"       : yvalues
       "tmYLMinorValues"  : myvalues
    end create

; Check to see if any tick mark resources have been set and set them.
    if(tickmark_res) then
      if(num_tickmark_res.eq.1) then
        setvalues tickmark_object
          attnames : res2@$attnames$
        end setvalues
      else
        tmres = True
        do i = 0,dimsizes(tm_res_index)-1
          tmres@$attnames(tm_res_index(i))$ = res2@$attnames(tm_res_index(i))$
        end do
        attsetvalues(tickmark_object,tmres)
      end if
    end if

; Get tickmark labels sizes and lengths.
    getvalues tickmark_object
      "tmXBLabelFontHeightF" : xbfontf
      "tmYLLabelFontHeightF" : ylfontf
      "tmYLMajorLengthF"     : ylength
      "tmXBMajorLengthF"     : xlength
      "tmYLMinorLengthF"     : ymlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

; Make tickmark label sizes and lengths uniform sizes, and point tick
; marks outward.
    major_length = min((/ylength,xlength/))
    minor_length = min((/ymlength,xmlength/))
    setvalues tickmark_object
      "tmXBLabelFontHeightF" : min((/xbfontf,ylfontf/))
      "tmYLLabelFontHeightF" : min((/xbfontf,ylfontf/))
      "tmXBMajorLengthF"     : major_length
      "tmXBMinorLengthF"     : minor_length
      "tmYLMajorLengthF"     : major_length
      "tmYLMinorLengthF"     : minor_length
      "tmXBMajorOutwardLengthF" : major_length
      "tmXBMinorOutwardLengthF" : minor_length
      "tmYLMajorOutwardLengthF" : major_length
      "tmYLMinorOutwardLengthF" : minor_length
    end setvalues
    ratio2 = lon_range/lat_range
    ratio = min((/ratio1,ratio2/))

    getvalues map_object
        "tiXAxisFontHeightF"   : xfontf
        "tiYAxisFontHeightF"   : yfontf
    wh_ind    = ind(ratio.le.ratios)
    def_vpwf  = wharray(wh_ind(0))    ; default width for plot
    def_vphf  = def_vpwf              ; default height for plot
                                          ; the size of the X/Y axis labels.
                                   "transparent"/)) ; (default,ocean,land,
        "tiXAxisFontHeightF" : font_height   ; X axis label size
        "tiYAxisFontHeightF" : font_height   ; Y axis label size
        "tiMainFontHeightF"  : 1.3*font_height  ; main title size
        "tiMainFont"         : "helvetica-bold"     ; main title font
; By default, mpOutlineOn is False, unless mpFillOn is set to False,
;
; Add the tick mark object as an annotation of the map object, so that
; whenever the map object is drawn the tick mark object will also be
; drawn.  It will also be rescaled automatically.
;
    anno = NhlAddAnnotation(map_object,tickmark_object)
    setvalues anno
      "amZone"         : 0     ; Zone 0 centers tick marks over map.
      "amResizeNotify" : True  ; Resize tick marks if map resized.
    end setvalues
; then it is set back to True.
;
; Leftmost subtitle.
    if(left_string)
      setvalues lft_str_object
        "txFontHeightF" : font_height
      end setvalues
;
      anno = NhlAddAnnotation(map_object,lft_str_object)
      setvalues anno
        "amZone"          : sub_zone     ; Just outside plot area
        "amSide"          : "top"        ; Subtitle at top.
        "amParallelPosF"  : 0.0          ; 0.0 is right edge of plot
        "amJust"          : "bottomleft" ; Left justify
        "amOrthogonalPosF": 0.05         ; Move away from top edge
        "amResizeNotify"  : True         ; Resize subtitle if map resized.
      end setvalues
    end if

; Center subtitle.
    if(center_string)
      setvalues ctr_str_object
        "txFontHeightF" : font_height
      end setvalues
      anno = NhlAddAnnotation(map_object,ctr_str_object)
      setvalues anno
        "amZone"          : sub_zone       ; Just outside plot area
        "amSide"          : "top"          ; Subtitle at top.
        "amParallelPosF"  : 0.5            ; 0.5 is center of plot
        "amJust"          : "bottomcenter" ; Center it about the center
        "amOrthogonalPosF": 0.05           ; Move away from top edge
        "amResizeNotify"  : True           ; Resize subtitle if map resized.
      end setvalues
    end if

; Rightmost subtitle.
    if(right_string)
      setvalues rgt_str_object
        "txFontHeightF" : font_height
      end setvalues

      anno = NhlAddAnnotation(map_object,rgt_str_object)
      setvalues anno
        "amZone"          : sub_zone      ; Just outside plot area
        "amSide"          : "top"         ; Subtitle at top.
        "amParallelPosF"  : 1.0           ; 1.0 is right edge of plot
        "amJust"          : "bottomright" ; Right justify
        "amOrthogonalPosF": 0.05          ; Move away from top edge
        "amResizeNotify"  : True          ; Resize subtitle if map resized.
      end setvalues
    end if
      map_object = create "map" mapPlotClass wks
; Draw all this stuff: map plot, subtitles, and tick marks.
    if(calldraw)
        draw(map_object) 
    end if

    if(callframe)
        frame(wks)           ; advance the frame
    end if
        "vpYF"               : res2@vpYF
        "vpWidthF"           : res2@vpWidthF
        "vpHeightF"          : res2@vpHeightF
    map_object@tickmarks = tickmark_object
        "mpLimitMode"        : limit_mode
        "mpMinLonF"          : min_lon
        "mpMaxLonF"          : max_lon
        "mpMinLatF"          : min_lat
        "mpMaxLatF"          : max_lat
        "mpShapeMode"        : shape_mode   ; must be set when map created
        "pmTitleDisplayMode" : "Always"
        "pmTitleZone"        : main_zone    ; Zone for main title
      end create
    else
      if(lower_case(limit_mode).eq."corners")
        map_object = create "map" mapPlotClass wks
          "vpXF"                : res2@vpXF
          "vpYF"                : res2@vpYF
          "vpWidthF"            : res2@vpWidthF
          "vpHeightF"           : res2@vpHeightF
          "mpLimitMode"         : limit_mode
          "mpLeftCornerLonF"    : min_lon
          "mpRightCornerLonF"   : max_lon
          "mpLeftCornerLatF"    : min_lat
          "mpRightCornerLatF"   : max_lat
          "mpShapeMode"         : shape_mode   ; must be set when map created
          "pmTitleDisplayMode"  : "Always"
          "pmTitleZone"         : main_zone    ; Zone for main title
        end create
    delete(max_lat)
    delete(lon_range)
    delete(lat_range)
local i, attnames, res, res2, mp_res_index, map_object, \
lft_str_object, ctr_str_object, rgt_str_object, main_zone, sub_zone, \
calldraw, callframe, left_string, center_string, right_string, \
min_lat, max_lat, center_lat, polar_nh, polar_sh, yoffset, main_offset
      tmres = True
      gsnp_turn_off_tickmarks(tmres)
    end if

    calldraw      = True
    callframe     = True
    left_string   = False
    center_string = False
    right_string  = False
    yoffset       = 0.08
    mpres = get_res_ne(res2,(/"tm","tx","am"/))  ; Get rest of resources
    polar_sh      = False
    polar_nh      = False
    attsetvalues_check(map_object,mpres)
;
; Add tickmark object, even if the tickmarks have been turned off, because
    sub_zone      = 2         ; Zone for subtitles
; we need to get the size of the X/Y axis labels.
    res2          = resources
    add_map_tickmarks(wks,map_object,tmres,lon_spacing,mlon_spacing,\
                      lat_spacing,mlat_spacing)

    if((res2).and..not.any(ismissing(getvaratts(res2))))
    end getvalues
; Check if frame and/or draw are not supposed to be called.
      "tiMainFontHeightF"  : main_font_height ; main title size
        if(isatt(res2,"gsnDraw"))
            if(.not.res2@gsnDraw)
                calldraw = False
            end if
            delete(res2@gsnDraw)
        end if
        if(isatt(res2,"gsnFrame"))
            if(.not.res2@gsnFrame)
                callframe = False
            end if
            delete(res2@gsnFrame)
        end if

; Check for left title at top. If it exists, create a TextItem object
; for it. 

        if(isatt(res2,"gsnLeftString"))
            lft_str_object = create "ezplot1p_left_string" textItemClass wks
              "txString" : res2@gsnLeftString
            end create
            left_string = True
            main_zone   = 3
            delete(res2@gsnLeftString)
        end if

; Check for center title at top. If it exists, create a TextItem
; object for it.

        if(isatt(res2,"gsnCenterString"))
            ctr_str_object = create "ezplot1p_center_string" textItemClass wks
              "txString" : res2@gsnCenterString
            end create
            center_string = True
            main_zone     = 3
            delete(res2@gsnCenterString)
        end if

; Check for right title at top. If it exists, create a TextItem
; object for it.  If the data has an attribute called "units", then use
; this string for the right title.

        if(isatt(res2,"gsnRightString"))
            rgt_str_object = create "ezplot1p_right_string" textItemClass wks
              "txString" : res2@gsnRightString
            end create
            right_string = True
            main_zone    = 3
            delete(res2@gsnRightString)
        end if

; Check for whether user wants northern or southern hemisphere.

        if(isatt(res2,"gsnPolarSH").and.res2@gsnPolarSH)
            polar_sh   = True   ; Southern hemisphere
            min_lat    = -90
            max_lat    =   0
            center_lat = -90
            delete(res2@gsnPolarSH)
        else
          if(isatt(res2,"gsnPolarNH").and.res2@gsnPolarNH)
              polar_nh   = True   ; Northern hemisphere
              min_lat    =   0 
              max_lat    =  90
              center_lat =  90
              delete(res2@gsnPolarNH)
          end if
        end if
    end if

;                   wks: workstation object                             ;
;               resources: optional resources                           ;
;                                                                       ;
      "vpHeightF"            : 0.62
      "vpWidthF"             : 0.62
      "vpXF"                 : 0.20
      "vpYF"                 : 0.80
;                                                                       ;
; This function behaves differently from gsn_map in that it will        ;
; create a special kind of plot if certain attributes are set.          ;
      "mpLimitMode"          : "LatLon"
;    2. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top of the plot.                          ;
;       (in addition, the regular resource "tiMainString" can be set to ;
      "mpOutlineOn"          : False
      "mpGridAndLimbOn"      : True
      "mpGridLineDashPattern": 2
      "mpGridLonSpacingF"    : 30

;       create a regular title).                                        ;
;    3. If either of the resources "gsnPolarNH" or "gsnPolarSH" are set ;
;       to True then only the northern or southern hemisphere is        ;
;       displayed.                                                      ;
; Check if we want northern or southern hemisphere.

    if(polar_nh.or.polar_sh)
        setvalues map_object
            "mpMinLatF"    : min_lat
            "mpMaxLatF"    : max_lat
            "mpCenterLatF" : center_lat
        end setvalues
    end if

side, just, orth, para, quad1, quad2, quad3, quad4, topbot, xcen, ycen, \
; By default, mpOutlineOn is False, mpFillOn is set to False, then 
; it is set back to True.
    main_zone     = 2         ; Zone for main title (may change later)
    if((isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        setvalues map_object
          "mpOutlineOn" : True
        end setvalues
    end if

; This section tests for special resources: those that start with "gsn."
    calldraw     = get_res_value(res2,"gsnDraw", True)
    if((res2).and..not.any(ismissing(getvaratts(res2))))

; Get list of resources.
        attnames = getvaratts(res2)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))
        if(dimsizes(dimsizes(res)).eq.1)
            setvalues map_object
                attnames : res2@$attnames$
            end setvalues
        else
            mp_res_index = ind(chartostring(res(:,0:1)).ne."gs")
            mpres = True
            do i = 0,dimsizes(mp_res_index)-1
                mpres@$attnames(mp_res_index(i))$ = res2@$attnames(mp_res_index(i))$
            end do
            attsetvalues(map_object,mpres)
        end if
    end if
    maxbb        = get_bb_res(res2)
    polar_dst    = get_res_value(res2,"gsnPolarLabelDistance",1.04)
    ticks_on     = get_res_value(res2,"gsnTickMarksOn",True)

    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)
      "vpWidthF"  : vpwidthf
      "vpHeightF" : vpheightf
    if(left_string.or.center_string.or.right_string)
      main_zone   = 4
    end if
;
; Check for type of polar plot desired.
;
    font_height = 0.02 * vpheightf     ; Make various label sizes a function
                                       ; of the height of the viewport.
      center_lat = -90
        "tiXAxisFontHeightF"   : 1.2*font_height      ; x axis font size
        "tiYAxisFontHeightF"   : 1.2*font_height      ; y axis font size
        "tiMainOffsetYF"       : main_offset          ; main title offset
        "tiMainFont"           : "helvetica-bold"     ; main title font
        "tiMainFontHeightF"    : 2. * font_height     ; main title size
    width  = get_res_value(res2,"vpWidthF",0.62)
    xpos   = get_res_value(res2,"vpXF",0.2)
; If doing a northern or southern hemisphere map, then create TextItem
; objects to label the longitude lines.

    if(polar_nh.or.polar_sh)

; Define an array of strings to label the longitude values.
      labels = (/"0","30E","60E","90E","120E","150E","180","150W",\
                 "120W","90W","60W","30W"/)

      num_labels = dimsizes(labels)
      text_ids = new(num_labels,graphic)

; Create an array of TextItem objects that we'll use to label the
; lat/lon grid.  

      do i=0,num_labels-1
        text_ids(i) = create labels(i) textItemClass wks
          "txString"      : labels(i)
          "txFont"        : "helvetica-bold"
          "txFontHeightF" : font_height
        end create
      end do

; Create the lat/lon coordinates where you want labels to appear.
; In this case, we want the labels to appear at a fixed latitude value
; and at longitude values of 0, 30, 60, ..., 330.
      if(min((/min_lat,max_lat/)).ge.0.or.polar_nh)
          plat = fspan(min_lat,min_lat,12)
      else
          plat = fspan(max_lat,max_lat,12)
      end if 
      plon = fspan(0.,330.,12)

; Increase the viewport of the map object; this is where the labels
; will get drawn.

      setvalues map_object
          "vpXF"      : vpxf-0.02
          "vpYF"      : vpyf+0.02
          "vpWidthF"  : vpwidthf+0.04
          "vpHeightF" : vpheightf+0.04
      end setvalues

; Create arrays to hold the NDC values that we're going to convert 
; the lat/lon values to.

      xndc = new(dimsizes(plon),float)
      yndc = new(dimsizes(plat),float)

; Convert lat/lon cooridinates to NDC coordinates since we are 
; drawing the labels in NDC space and NOT in lat/lon space.

      datatondc(map_object,plon,plat,xndc,yndc)

; Set map back to original viewport.

      setvalues map_object
          "vpXF"            : vpxf
          "vpYF"            : vpyf
          "vpWidthF"        : vpwidthf
          "vpHeightF"       : vpheightf
          "mpGridAndLimbOn" : True
      end setvalues

; Retrieve graphic style object.

      getvalues wks 
          "wkDefGraphicStyleId":  gsid
      end getvalues
;
; Create a LogLinPlot that covers the entire NDC space
; to use as a drawing canvas, and add the text items as annotations.
;
      canvas = create "canvas" logLinPlotClass wks
        "vpXF"        : 0.0
        "vpYF"        : 0.0
        "vpWidthF"    : 1.0
        "vpHeightF"   : 1.0
        "pmAnnoViews" : text_ids
      end create

; Retrieve the ids of the AnnoManager objects created by the PlotManager
; and then set their location in data coordinate space. The AnnoManager
; objects are arranged in the same order as the TextItems in the
; pmAnnoViews resource.

      getvalues canvas
        "pmAnnoManagers" : am_ids
      end getvalues
      num_am_ids = dimsizes(am_ids)
      do i=0,num_am_ids-1
        setvalues am_ids(i)
          "amTrackData"    : True
          "amDataXF"       : xndc(i)
          "amDataYF"       : yndc(i)
          "amResizeNotify" : True
        end setvalues
      end do
      anno = NhlAddAnnotation(map_object,canvas)  ; Add canvas to map_object.
      setvalues anno
        "amJust"           : "centercenter"
        "amZone"           : 0
        "amResizeNotify"   : True
        "amParallelPosF"   : -0.015
        "amOrthogonalPosF" : -0.010
      end setvalues
    end if

; Set up three titles at top, if they exist.
; Leftmost title.

    if(left_string)
      setvalues lft_str_object
        "txFont"        : "helvetica-bold"
        "txFontHeightF" : 1.2 * font_height
      end setvalues

      anno = NhlAddAnnotation(map_object,lft_str_object)
      setvalues anno
        "amZone"          : sub_zone     ; Just outside plot area
        "amSide"          : "top"        ; Title at top.
        "amParallelPosF"  : 0.0          ; 0.0 is right edge of plot
        "amJust"          : "bottomleft" ; Left justify
        "amOrthogonalPosF": 0.06         ; Move away from top edge
        "amResizeNotify"  : True         ; Resize title if map resized.
      end setvalues
    end if

; Center title.
    if(center_string)
      setvalues ctr_str_object
        "txFont"        : "helvetica-bold"
        "txFontHeightF" : 1.2 * font_height
      end setvalues
      anno = NhlAddAnnotation(map_object,ctr_str_object)
      setvalues anno
        "amZone"          : sub_zone       ; Just outside plot area
        "amSide"          : "top"          ; Title at top.
        "amParallelPosF"  : 0.5            ; 0.5 is center of plot
        "amJust"          : "bottomcenter" ; Center it about the center
        "amOrthogonalPosF": 0.06           ; Move away from top edge
        "amResizeNotify"  : True           ; Resize title if map resized.
      end setvalues
    end if

; Rightmost title.
    if(right_string)
      setvalues rgt_str_object
        "txFont"        : "helvetica-bold"
        "txFontHeightF" : 1.2 * font_height
      end setvalues
      anno = NhlAddAnnotation(map_object,rgt_str_object)
      setvalues anno
        "amZone"          : sub_zone      ; Just outside plot area
        "amSide"          : "top"         ; Title at top.
        "amParallelPosF"  : 1.0           ; 1.0 is right edge of plot
        "amJust"          : "bottomright" ; Right justify
        "amOrthogonalPosF": 0.06          ; Move away from top edge
        "amResizeNotify"  : True          ; Resize title if map resized.
      end setvalues
    end if

    if(calldraw)
        draw(map_object)     ; Draw everything.
    end if

    if(callframe)
        frame(wks)           ; Advance the frame.
    end if
;
; The zone is being set to 0, so this means all labels will appear in the
; center of the map by default.  We set the orthogonal and parallel values
; to move the labels away from the center of the plot.  The orth/para values
; are a fraction of the total width of the map, so if you set an orthogonal
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)
; This function calls either gsn_csm_map_ce or                          ;
; gsn_csm_map_polar depending on if gsnPolar is set to True.            ;

    shape_mode   = get_res_value(res2,"mpShapeMode","FixedAspectFitBB")
function gsn_csm_map(wks:graphic,res:logical)
    set_attr(res2,"vpXF",     0.2)

  if(res.and.isatt(res,"gsnPolarNH").and.res@gsnPolarNH.or.\
             isatt(res,"gsnPolarSH").and.res@gsnPolarSH)
    return(gsn_csm_map_polar(wks,res))
    set_attr(res2,"mpPerimOn", True)
    return(gsn_csm_map_ce(wks,res))
; then it is set back to True.
  delete(res)
;
    set_attr(res2,"mpOutlineOn",.not.res2@mpFillOn)
    
; Create the map object. We have to use this code instead of gsn_map
; because we want to set the size of the map.  If we set the size of the
; map later with setvalues, as gsn_map would do, then the size wouldn't
; be correct.

    map_object = create "map" mapPlotClass wks
      "vpXF"               : res2@vpXF
      "vpYF"               : res2@vpYF
      "vpWidthF"           : res2@vpWidthF
      "vpHeightF"          : res2@vpHeightF
      "pmTitleDisplayMode" : "Always"
      "pmTitleZone"        : main_zone        ; Zone for main title
      "mpShapeMode"        : shape_mode   ; must be set when map created
    end create

; This section tests for regular resources.

    mpres = get_res_ne(res2,(/"tm","tx"/))  ; Get rest of resources
    attsetvalues_check(map_object,mpres)

; Make sure axes labels (if any) are the same size.

    getvalues map_object
        "tiXAxisFontHeightF"   : xfontf
        "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value_keep(res2,"tiMainFontHeightF", \
                                          1.1*font_height)
    main_font = get_res_value_keep(res2,"tiMainFont","helvetica-bold")

    setvalues map_object
      "tiMainFontHeightF"  : main_font_height ; main title size
local i, attnames, contour_object, map_object, res, cn_res_index, \
mp_res_index, lbar_pos, lbar_zone, lbar_on, calldraw, callframe, \
main_offset, addcyclic, datanew, res2, cnres, mapres, \
vpwidthf, vpheightf, font_height, levels, colors, zonal_zone, lbar_zone, \
infolabel_on, infolabel_zone
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",0.8*font_height)

    calldraw      = True      ; Default is to draw plot
    callframe     = True      ; Default is to advance frame
    set_attr(subres,"amOrthogonalPosF",0.015)
    addcyclic     = True      ; Default is cyclic data
    res2          = resources ; Copy of resources
                  subres)
    mapres        = True      ; Will hold map resources
    cnres         = True      ; Will hold contour resources
; Draw all this stuff: map plot, subtitles, and tick marks.
; The default is to add a cyclic point.
    if((res2).and.isatt(res2,"gsnAddCyclic"))
        if(.not.res2@gsnAddCyclic)
            addcyclic = False
        end if
        delete(res2@gsnAddCyclic)
    end if
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)
    if(addcyclic)
; Return plot object and tickmark object.

    return(map_object)
end

; This section tests for special resources: those that start with "gsn."
; mpProjection is set.                                                  ;
    if((res2).and..not.any(ismissing(getvaratts(res2))))

; Check if frame and/or draw are not supposed to be called.

        if(isatt(res2,"gsnDraw"))
            if(.not.res2@gsnDraw)
                calldraw = False
            end if
            delete(res2@gsnDraw)
        end if
        if(isatt(res2,"gsnFrame"))
            if(.not.res2@gsnFrame)
                callframe = False
            end if
            delete(res2@gsnFrame)
        end if

; Check for left title. If the data has an attribute called "long_name",
; then use this string for the left title.

        if(isatt(datanew,"long_name").and..not.isatt(res2,"gsnLeftString"))
            mapres@gsnLeftString = datanew@long_name
            delete(datanew@long_name)
        end if
        if(isatt(res2,"gsnLeftString"))
            mapres@gsnLeftString = res2@gsnLeftString
            delete(res2@gsnLeftString)
        end if

; Check for center title at top.

        if(isatt(res2,"gsnCenterString"))
            mapres@gsnCenterString = res2@gsnCenterString
            delete(res2@gsnCenterString)
        end if

; Check for right title at top. If the data has an attribute called "units",
; then use this string for the right title.

        if(isatt(datanew,"units").and..not.isatt(res2,"gsnRightString"))
            mapres@gsnRightString = datanew@units
        end if
        if(isatt(res2,"gsnRightString"))
            mapres@gsnRightString = res2@gsnRightString
            delete(res2@gsnRightString)
        end if

; Check for whether user wants northern or southern hemisphere.

        if(isatt(res2,"gsnPolarSH").and.res2@gsnPolarSH)
            mapres@gsnPolarSH = True
            delete(res2@gsnPolarSH)
        end if
        if(isatt(res2,"gsnPolarNH").and.res2@gsnPolarNH)
            mapres@gsnPolarNH = True
            delete(res2@gsnPolarNH)
        end if
    end if

; list of resources. The id of the map plot is returned.                ;
;                                                                       ;
    if(iscoord(datanew,"lat").and.iscoord(datanew,"lon"))
      cnres@sfYCStartV  = datanew&lat(0)
      cnres@sfYCEndV    = datanew&lat(dimsizes(datanew&lat)-1)
      cnres@sfXCStartV  = datanew&lon(0)
      cnres@sfXCEndV    = datanew&lon(dimsizes(datanew&lon)-1)
    end if
;                                                                       ;
; Check for a missing value.

    if(isatt(datanew,"_FillValue")) then
      cnres@sfMissingValueV = datanew@_FillValue
    end if

;    1. By default, a cyclic point is added.  If gsnAddCyclic is set to ;
;       False, then the cyclic point is not added.                      ;
    cnres@cnInfoLabelFont            = "helvetica"
    cnres@cnLineLabelBackgroundColor = -1
    cnres@cnLineLabelFont            = "helvetica"
    cnres@cnInfoLabelOrthogonalPosF  = 0.06
    cnres@cnInfoLabelZone            = 2

; Set some map plot resources.

    mapres@vpHeightF            = 0.62
    mapres@vpWidthF             = 0.62
    mapres@vpXF                 = 0.20
    mapres@vpYF                 = 0.80

    mapres@mpProjection         = "Stereographic"
    mapres@mpEllipticalBoundary = True
    mapres@mpLimitMode          = "LatLon"

    mapres@mpFillOn             = True
    mapres@mpFillColors         = (/"background","transparent","LightGray",\
                                    "transparent"/)
    mapres@mpOutlineOn          = False
    mapres@mpGridAndLimbOn      = True
    mapres@mpGridLineDashPattern= 2
    mapres@mpGridLonSpacingF    = 30
;       are used to title the top of the plot.                          ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;    4. If the resource "cnFillOn" is set to True, then a labelbar is   ;
;       drawn and line labels are turned off.                           ;
    if((isatt(res2,"cnFillOn").and.res2@cnFillOn).or.\
       (isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        mapres@mpOutlineOn  = True
;    6. If data has an attribute called "units," and "gsnRightString"   ;
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the right string title.                             ;
;    7. If either of the resources "gsnPolarNH" or "gsnPolarSH" are set ;
    if((res2).and..not.any(ismissing(getvaratts(res2))))

; Get list of resources.
        attnames = getvaratts(res2)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))
        if(dimsizes(dimsizes(res)).eq.1)

;***********************************************************************;
; Check to see if any resources were set. For the data object, resources;
; begin with "sf". For the map object, resources begin with "mp". For   ;
; the contour object, any resource that doesn't start with "mp" or "sf" ;
; is assumed to be a contour or contour-related resource.               ;
;***********************************************************************;

          if(chartostring(res(0:1)).eq."mp".or.\
             chartostring(res(0:1)).eq."ti".or.\
             chartostring(res(0:1)).eq."vp")
             mapres@$attnames$  = res2@$attnames$
          else 
             cnres@$attnames$  = res2@$attnames$
             if(attnames.eq."cnFillOn".and.res2@$attnames$)
               lbar_on = True   ; Turn on a labelbar
               cnres@cnLineLabelsOn  = False
             end if
          end if
        else
          mp_res_index = ind(chartostring(res(:,0:1)).eq."mp".or.\
                             chartostring(res(:,0:1)).eq."ti".or.\
                             chartostring(res(:,0:1)).eq."vp")
          cn_res_index = ind(chartostring(res(:,0:1)).ne."mp".and.\
                             chartostring(res(:,0:1)).ne."ti".and.\
                             chartostring(res(:,0:1)).ne."vp")
          if(.not.all(ismissing(mp_res_index)))
            do i = 0,dimsizes(mp_res_index)-1
              mapres@$attnames(mp_res_index(i))$ = res2@$attnames(mp_res_index(i))$
            end do
          end if
          if(.not.all(ismissing(cn_res_index)))
            do i = 0,dimsizes(cn_res_index)-1
              cnres@$attnames(cn_res_index(i))$ = res2@$attnames(cn_res_index(i))$
            end do
            if(isatt(cnres,"cnFillOn").and.cnres@cnFillOn)
              cnres@cnLineLabelsOn  = False
              lbar_on = True   ; Turn on a labelbar
            end if
          end if
       end if
    lbar_on       = False     ; Labelbar flag

    res2          = get_resources(resources) ; Copy of resources
    lbar_zone     = 3         ; Zone for labelbar (may change later)
    mpres         = True      ; Will hold map resources

    if(isatt(res2,"cnInfoLabelOn").and..not.res2@cnInfoLabelOn)

    if(get_res_value(res2,"gsnAddCyclic",True))
        datanew = gsn_add_cyclic_point(data)
    else

; Check for left/right titles at top. Use attributes "long_name" and
    mapres@gsnDraw  = False
    mapres@gsnFrame = False
    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)
    map_object = gsn_csm_map(wks,mapres)            ; Create map.
; Check for draw and frame.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

;
      "vpWidthF"  : vpwidthf
      "vpHeightF" : vpheightf
    mpres@gsnPolar = get_polar_type(res2)

; Check for subtitles at top and add to mpres if exist.

    font_height = 0.02 * vpheightf     ; Make various label sizes a function
                                       ; of the height of the viewport.
    if(isatt(res2,"gsnPolarLabelSpacing"))
        "tiXAxisFontHeightF"   : 1.2*font_height      ; x axis font size
        "tiYAxisFontHeightF"   : 1.2*font_height      ; y axis font size
        "tiMainFont"           : "helvetica-bold"     ; main title font
        "tiMainFontHeightF"    : 2. * font_height     ; main title size
    end if

    if(isatt(res2,"gsnPolarLabelDistance"))
      mpres@gsnPolarLabelDistance = get_res_value(res2,  \
                                                 "gsnPolarLabelDistance",0)
;
; Find out what levels and colors were used for contours.
;
      getvalues contour_object
        "cnLevels"     : levels
        "cnFillColors" : colors
      end getvalues
;
; Generate a labelbar from the above information.
;
      labelbar_object = create "labelbar" labelBarClass  wks
        "lbAutoManage"      : False
        "lbBoxCount"        : dimsizes(colors)
        "lbFillColors"      : colors
        "lbFillPattern"     : "SolidFill"
        "lbLabelAlignment"  : "InteriorEdges"
        "lbLabelFontHeightF": font_height
        "lbLabelStrings"    : levels
        "lbMonoFillPattern" : True
        "lbOrientation"     : "Horizontal"
        "lbPerimOn"         : False
        "vpHeightF"         : 0.20 * vpheightf
        "vpWidthF"          : 0.75 * vpwidthf
      end create
; 
; Add labelbar as an annotation of the contour plot.
;
      anno = NhlAddAnnotation(contour_object,labelbar_object)
      if(lbar_zone.eq.2)
        lbar_pos = 0.06
      else
        lbar_pos = 0.0
      end if
      setvalues anno
        "amZone"          : lbar_zone      ; Just outside plot area
        "amSide"          : "bottom"       ; Labelbar on bottom.
        "amParallelPosF"  : 0.5            ; 0.5 is center of plot
        "amJust"          : "bottomcenter" ; Center it about the center
        "amResizeNotify"  : True           ; Resize labelbar if map resized.
        "amOrthogonalPosF": lbar_pos       ; Move away from bottom edge
      end setvalues

; Create data object and use coordinate variables if they exist.
    if(calldraw)
        draw(map_object)     ; Draw everything.
    end if

    if(callframe)
        frame(wks)           ; Advance the frame.
    end if
    check_for_y_lat_coord(datanew,res2,"contour")
    check_for_lon_coord(datanew,res2,"contour")

; Set some more contour plot resources.

    set_attr(res2,"cnLineLabelBackgroundColor",-1)
    set_attr(res2,"cnInfoLabelOrthogonalPosF",0.06)
    set_attr(res2,"cnInfoLabelZone",2)

;
; By default, mpOutlineOn is False, unless cnFillOn is set to True
; or mpFillOn is set to False, then it is set back to True.
;
    if(check_attr(res2,"cnFillOn",True,False).or.\
       check_attr(res2,"mpFillOn",False,False))
      set_attr(res2,"mpOutlineOn",True)
    end if

; This section tests for regular resources.

    lbres = get_res_eq(res2,(/"lb","pm"/))
    mpres = get_res_eq(res2,(/"mp","ti","vp","tx","am","pmA","pmO","pmT"/))
    cnres = get_res_ne(res2,(/"mp","vp","lb","tx","am"/))

    if(cnres)
      if(check_attr(cnres,"cnFillOn",True,False))
        set_attr(cnres,"cnInfoLabelOn",False)
        if(.not.isatt(lbres,"lbLabelBarOn").or.\
          check_attr(lbres,"lbLabelBarOn",True,False))
          set_attr(cnres,"cnLineLabelsOn",False)
          lbar_on = True   ; Turn on a labelbar
        end if
      end if
    end if
;
; Compute zone for labelbar if it is supposed to get drawn.
; Zone for labelbar changes only if an info label is not drawn.
;
    if(check_attr(cnres,"cnInfoLabelOn",False,False))
      lbar_zone = 2
    end if

; Before we create the objects, turn off draw and frame for them.
local i, attnames, contour_object, labelbar_object, xy_object, map_object, \
cn_res_index, mp_res_index, calldraw, callframe, lbar_on, zonal_mean_plot,\
min_lat, max_lat, addcyclic, datanew, res, res2, \
tickmark_res, xy_res, mapres, cnres, levels, colors, \
map_vpwidthf, map_vpheightf, vpwidthf, vpheightf
    mpres@gsnFrame = False



    calldraw      = True
    callframe     = True

    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    addcyclic      = True     ; Default is to add a cyclic point.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
    tickmark_res   = False
    cnres          = True
    mapres         = True
                                                    ; on map.
;
; Retrieve the view port location of the map plot so we know where
; to put titles and labelbar (if there is one).
;
    res2 = resources
      "vpWidthF"  : vpwf
; The default is to add a cyclic point.
    if((res2).and.isatt(res2,"gsnAddCyclic"))
        if(.not.res2@gsnAddCyclic)
            addcyclic = False
        end if
        delete(res2@gsnAddCyclic)
    end if
    end getvalues
    if(addcyclic)
; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check for a missing value.

    if(isatt(datanew,"_FillValue")) then
      cnres@sfMissingValueV = datanew@_FillValue
    end if

; Check if user setting own font heights.
;
    main_font_height = get_res_value_keep(res2,"tiMainFontHeightF", \
    if(iscoord(datanew,"lat").and.iscoord(datanew,"lon")) then
        cnres@sfYCStartV = datanew&lat(0)
        cnres@sfYCEndV   = datanew&lat(dimsizes(datanew&lat)-1)
        cnres@sfXCStartV = datanew&lon(0)
        cnres@sfXCEndV   = datanew&lon(dimsizes(datanew&lon)-1)
    else
        cnres@sfYCStartV =  -90.
        cnres@sfYCEndV   =   90.
        cnres@sfXCStartV = -180.
        cnres@sfXCEndV   =  180.
      add_labelbar(wks,contour_object,lbar_zone,font_height,"polar",lbres)
    end if
  
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)
    if((res2).and.isatt(res2,"gsnZonalMean"))
      if(iscoord(datanew,"lat").and.iscoord(datanew,"lon")) then
    map_object@data    = contour_object@data
    map_object@contour = contour_object
            map_vpheightf = res2@vpHeightF
            map_vpwidthf = res2@vpWidthF
end
            map_vpheightf  = 0.7 ; make sure zonalplot will fit in view port
            map_vpwidthf   = 0.7
            if(.not.isatt(res2,"vpHeightF")) then
              res2@vpHeightF = 0.7
            end if

            if(.not.isatt(res2,"vpWidthF")) then
              res2@vpWidthF = 0.7
            end if

            if(.not.isatt(res2,"vpXF")) then
              res2@vpXF = 0.1
            end if

            if(.not.isatt(res2,"vpYF")) then
              res2@vpYF = 0.9
            end if
; This function creates and draws a contour plot over a map plot to the ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "data" is the 2-dimensional data to be contoured,   ;
; and "resources" is an optional list of resources. The Id of the map   ;
; plot is returned.                                                     ;
;                                                                       ;
; This function behaves differently from gsn_contour in that it will    ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. The lat/lon grid is labeled with tickmarks.                     ;
;    2. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top left, center, and right of the plot   ;
;       (in addition, the regular resource "tiMainString" can be set to ;
        print("The resource gsnZonalMean can only be set to True")
        print("if the data has coordinate variables 'lat' and 'lon'")
;       a zonal mean XY plot is drawn.
;    4. If the resource "cnFillOn" is set to True, then a labelbar is   ;
;       drawn and line labels are turned off.                           ;
;    5. If data has an attribute called "long_name," and "gsnLeftString";
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the left string title.                              ;
    cnres@cnInfoLabelFont            = "helvetica"
    cnres@cnLineLabelFont            = "helvetica"
    cnres@cnLineLabelBackgroundColor = "transparent"

; Create a cylindrical equidistant map object.

    mapres@tiXAxisOffsetYF = -0.02
    mapres@tiYAxisOffsetXF = -0.04

    mapres@mpGridAndLimbOn = False    ; Turn off grid lines, limb line,
    mapres@mpOutlineOn     = False    ; and map outlines.
    mapres@mpFillOn        = True     ; Fill map boundaries according to
                                      ; "mpFillColors".
    mapres@mpFillColors    = (/"background","transparent","LightGray",\
                               "transparent"/)   ; (default,ocean,land,
                                                 ;  inland water)
;       is used for the right string title.                             ;
;***********************************************************************;

function gsn_csm_contour_map_ce(wks:graphic,data[*][*]:numeric,\
    if(isatt(datanew,"long_name").and..not.isatt(res2,"gsnLeftString"))
        mapres@gsnLeftString = datanew@long_name
        delete(datanew@long_name)
    end if
    if(isatt(datanew,"units").and..not.isatt(res2,"gsnRightString"))
        mapres@gsnRightString = datanew@units
    end if
        datanew = data
    if((res2).and..not.any(ismissing(getvaratts(res2))))

; Check if frame and/or draw are not supposed to be called.

        if(isatt(res2,"gsnDraw"))
            if(.not.res2@gsnDraw)
                calldraw = False
            end if
            delete(res2@gsnDraw)
        end if
        if(isatt(res2,"gsnFrame"))
            if(.not.res2@gsnFrame)
                callframe = False
            end if
            delete(res2@gsnFrame)
        end if

; Check for left subtitle at top.

        if(isatt(res2,"gsnLeftString"))
            mapres@gsnLeftString = res2@gsnLeftString
            delete(res2@gsnLeftString)
        end if

; Check for center subtitle at top.

        if(isatt(res2,"gsnCenterString"))
            mapres@gsnCenterString = res2@gsnCenterString
            delete(res2@gsnCenterString)
        end if

; Check for right subtitle at top.
      set_attr(res2,"sfYCStartV",-90.)
        if(isatt(res2,"gsnRightString"))
            mapres@gsnRightString = res2@gsnRightString
            delete(res2@gsnRightString)
        end if
    end if
    end if
; Check if a zonal mean plot is desired.

    if(res2.and.isatt(res2,"gsnZonalMean"))
      if(any(datanew!0.eq.get_allowed_latnames()).and.\
    if((isatt(res2,"cnFillOn").and.res2@cnFillOn).or.\
       (isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        mapres@mpOutlineOn = True
            map_vphf = res2@vpHeightF
          else
            map_vphf  = 0.7 ; make sure zonalplot will fit in view port

    if((res2).and..not.any(ismissing(getvaratts(res2))))

; Get list of resources.
        attnames = getvaratts(res2)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))
        if(dimsizes(dimsizes(res)).eq.1)

;***********************************************************************;
; Check to see if any resources were set. For the map object, resources ;
; begin with "mp" or "ti" or "vp". For the contour object, any resource ;
; that doesn't start with "mp", "ti", or "vp" is assumed to be a contour;
; or contour-related  resource.                                         ;
;***********************************************************************;

          if(chartostring(res(0:1)).eq."mp".or.\
             chartostring(res(0:1)).eq."vp".or.\
             chartostring(res(0:1)).eq."tm".or.\
             chartostring(res(0:1)).eq."ti")
            mapres@$attnames$ = res2@$attnames$
          else 
            cnres@$attnames$ = res2@$attnames$
            if(attnames.eq."cnFillOn".and.res2@$attnames$)
              lbar_on = True
              cnres@cnLineLabelsOn = False
            end if
          end if
        else
            mp_res_index = ind(chartostring(res(:,0:1)).eq."mp".or.\
                               chartostring(res(:,0:1)).eq."vp".or.\
                               chartostring(res(:,0:1)).eq."tm".or.\
                               chartostring(res(:,0:1)).eq."ti")
            cn_res_index = ind(chartostring(res(:,0:1)).ne."mp".and.\
                               chartostring(res(:,0:1)).ne."vp".and.\
                               chartostring(res(:,0:1)).ne."tm".and.\
                               chartostring(res(:,0:1)).ne."ti")
            if(.not.all(ismissing(mp_res_index)))
                do i = 0,dimsizes(mp_res_index)-1
                    mapres@$attnames(mp_res_index(i))$ = res2@$attnames(mp_res_index(i))$
                end do
            end if
            if(.not.all(ismissing(cn_res_index)))
                do i = 0,dimsizes(cn_res_index)-1
                    cnres@$attnames(cn_res_index(i))$ = res2@$attnames(cn_res_index(i))$
                end do
                if(isatt(cnres,"cnFillOn").and.cnres@cnFillOn)
                    lbar_on = True
                    if(.not.isatt(cnres,"cnLineLabelsOn"))
                      cnres@cnLineLabelsOn = False
                    end if
                end if
            end if
        end if
        end if

      else
        print("gsn_csm_contour_map_ce: Warning: The resource gsnZonalMean can only be set to True")
        print("if the data has one of the coordinate variables " + get_allowed_latnames() + "and " + get_allowed_lonnames())
      end if
    if(isatt(res2,"cnInfoLabelOn").and..not.res2@cnInfoLabelOn)
      infolabel_on = False
    else
      infolabel_on = True
    end if

; Create some contour plot resources.

    res2 = True
    set_attr(res2,"cnLineLabelBackgroundColor", "transparent")

; This section tests for more special resources: those that start
; with "gsn."

    if(isatt(res2,"gsnMajorLonSpacing"))
      mpres@gsnMajorLonSpacing = res2@gsnMajorLonSpacing
      delete(res2@gsnMajorLonSpacing)
    end if
    if(.not.isatt(res2,"cnInfoLabelOrthogonalPosF").and.infolabel_zone.eq.2)
      cnres@cnInfoLabelOrthogonalPosF = 0.13
    if(isatt(res2,"gsnMinorLonSpacing"))
    cnres@cnInfoLabelZone           = infolabel_zone
      mpres@gsnMinorLonSpacing = res2@gsnMinorLonSpacing
      delete(res2@gsnMinorLonSpacing)
      mpres@gsnMinorLatSpacing = res2@gsnMinorLatSpacing
      delete(res2@gsnMinorLatSpacing)
    mapres@gsnDraw  = False
    mapres@gsnFrame = False
; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.
    map_object = gsn_csm_map(wks,mapres)            ; Create map.
    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
;
; Tickmarks.
;
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2) then
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",False)
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    else
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    end if

; or mpFillOn is set to False, then it is set back to True.
        "tiXAxisFontHeightF"   : font_height   ; X axis label size
        "tiYAxisFontHeightF"   : font_height   ; Y axis label size
        "tiMainFontHeightF"    : 1.3*font_height  ; main title size
    if(check_attr(res2,"cnFillOn",True,False).or.\
       check_attr(res2,"mpFillOn",False,False))
    setvalues contour_object
        "cnInfoLabelFontHeightF"    : xbfontf
        "cnLineLabelFontHeightF"    : xbfontf
    end setvalues
            set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
; Retrieve the view port size of the map plot so we know how big
; to make labelbar and where to put zonal means plot (if there is one).

    getvalues map_object
      "vpWidthF"     : vpwidthf
      "vpHeightF"    : vpheightf
    end getvalues
            set_attr(mpres,     "vpXF",0.08)
; Create a labelbar.
            set_attr(mpres,     "vpYF",0.90)
;
; Find out what levels and what colors were used for contours.
;
      getvalues contour_object
        "cnLevels"     : levels
        "cnFillColors" : colors
      end getvalues
;
; Generate a labelbar from the above information.
;
      labelbar_object = create "labelbar" labelBarClass  wks
        "lbAutoManage"      : False
        "lbBoxCount"        : dimsizes(colors)
        "lbFillColors"      : colors
        "lbFillPattern"     : "SolidFill"
        "lbLabelAlignment"  : "InteriorEdges"
        "lbLabelFontHeightF": xbfontf
        "lbLabelStrings"    : levels
        "lbMonoFillPattern" : True
        "lbOrientation"     :"Horizontal"
        "lbPerimOn"         : False
        "vpHeightF"         : 0.30 * vpheightf
        "vpWidthF"          : 0.75 * vpwidthf
      end create
; 
; Add labelbar as an annotation of the contour plot.
;
      anno = NhlAddAnnotation(contour_object,labelbar_object)
      if(lbar_zone.eq.2)
        lbar_pos = 0.06
      else
        lbar_pos = 0.0
      end if
      setvalues anno
        "amZone"          : lbar_zone      ; Just outside plot area
        "amSide"          : "bottom"       ; Labelbar on bottom.
        "amParallelPosF"  : 0.5            ; 0.5 is center of plot
        "amJust"          : "bottomcenter" ; Center it about the center
        "amResizeNotify"  : True           ; Resize labelbar if map resized.
        "amOrthogonalPosF": lbar_pos       ; Move away from bottom edge
      end setvalues
    end if
  
; Add a zonal mean plot if requested.
    if(zonal_mean_plot)

    if(zonal_mean_plot)
        infolabel_zone = zonal_zone + 1
        lbar_zone      = zonal_zone + 2
      else
        lbar_zone      = zonal_zone + 1
      end if
      data_object = create "data" coordArraysClass noparent
        "caXArray" : zmeans
        "caYArray" : datanew&lat
      end create


      if(isatt(zmeans,"_FillValue")) then
      if(.not.isatt(cnres,"cnInfoLabelOrthogonalPosF").and.infolabel_zone.eq.2)
              "caXMissingV" :zmeans@_FillValue
      end if
      end if
    end if

      xy_object = create "xyplot" xyPlotClass wks
        "xyCoordData"            : data_object
        "vpHeightF"              : vpheightf
        "vpWidthF"               : 0.15 * map_vpheightf
        "trYMaxF"                : max_lat
        "trYMinF"                : min_lat
        "tmXBLabelFontHeightF"   : xbfontf
        "tmYLOn"                 : False
        "tmYROn"                 : False
        "tmXBMaxTicks"           : 3
        "tmXBMinorPerMajor"      : 1
        "tmXTMinorPerMajor"      : 1
        "tmXBMajorLengthF"       : xlength
        "tmXBMinorLengthF"       : xmlength
        "tmYRMajorLengthF"       : xlength
        "tmYRMinorLengthF"       : xmlength
        "tmXBMajorOutwardLengthF": xlength
        "tmXBMinorOutwardLengthF": xmlength
        "tmYRMajorOutwardLengthF": xlength
        "tmYRMinorOutwardLengthF": xmlength
        "tiXAxisOn"              : False
        "tiYAxisOn"              : False
      end create
      "tiYAxisFontHeightF"   : yfontf
    end getvalues
      anno = NhlAddAnnotation(map_object,xy_object)
      setvalues anno
        "amZone"          : zonal_zone   ; Just outside plot area
        "amSide"          : "right"      ; Plot at right.
        "amResizeNotify"  : True         ; Resize if map resized.
        "amParallelPosF"  : 0.5          ; 0.5 is center of plot
        "amOrthogonalPosF": 0.05         ; Move away from right edge
      end setvalues
    end if

; Set font heights only if they haven't been set explicitly by user.
    if(calldraw)
        draw(map_object) 
    end if

    if(callframe)
        frame(wks)           ; advance the frame
    end if

    if(.not.isatt(cnres,"cnLineLabelFontHeightF"))
      setvalues contour_plot
        "cnLineLabelFontHeightF"    : xbfontf
      end setvalues
    end if

    if(.not.isatt(cnres,"cnInfoLabelFontHeightF"))
      setvalues contour_plot
        "cnInfoLabelFontHeightF"    : xbfontf
      end setvalues
    end if
; Function : gsn_csm_contour_map                                        ;
          lbar_on = True   ; Turn on a labelbar
;                  data: 2-dimensional data                             ;
            set_attr(mpres,"vpHeightF",0.75)   ; on the side.
            set_attr(mpres,     "vpXF",0.08)
; This function calls either gsn_csm_contour_map_ce or                  ;
; gsn_csm_contour_map_polar depending on if gsnPolar is set to True.    ;
    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
function gsn_csm_contour_map(wks:graphic,data[*][*]:numeric,\
resources:logical)
local res2
    getvalues map_object
  res2     = resources
      "tiXAxisFontHeightF"   : xfontf
  if((res2).and.isatt(res2,"gsnPolarNH").and.res2@gsnPolarNH.or.\
                isatt(res2,"gsnPolarSH").and.res2@gsnPolarSH)
    return(gsn_csm_contour_map_polar(wks,data,res2))
  else
    return(gsn_csm_contour_map_ce(wks,data,res2))
  end if
  delete(res2)
end


;***********************************************************************;
; Function : gsn_csm_contour_map_overlay                                ;
;                   wks: workstation object                             ;
;                 data1: 2-dimensional data                             ;
;                 data2: 2-dimensional data                             ;
;                  res1: optional resources                             ;
;                  res2: optional resources                             ;
;                                                                       ;
; This is similar to gsn_csm_contour_map, only it overlays an           ;
; additional contour plot.                                              ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_contour_map_overlay(wks:graphic,data1[*][*]:numeric,\
                                data2[*][*]:numeric,res1:logical,res2:logical)
local calldraw, callframe, res1_tmp, res2_tmp
begin
  res1_tmp  = get_resources(res1)   ; Transfer attributes
  res2_tmp  = get_resources(res2)   ; Transfer attributes
; If "gsnYReferenceLine" is set to some value, then a reference line    ;
; will be drawn.
  end if 

  res2_tmp@gsnDraw  = False
  res2_tmp@gsnFrame = False
local res, xy_object, res2, xfontf, yfontf, font_height, \
lft_str_object, ctr_str_object, rgt_str_object, calldraw, callframe, \
left_string, center_string, right_string, sub_zone, main_zone
  if(get_res_value(res2_tmp,"gsnAddCyclic",True))
    datanew = gsn_add_cyclic_point(data2)
  else

    calldraw      = True
    callframe     = True

    sub_zone      = 2         ; Zone for subtitles
    datanew = data2
    left_string   = False     ; Default is no subtitles unless
    center_string = False     ; they are explicitly set with the
    right_string  = False     ; special "gsn" resources.
    tickmark_res  = False
    res2 = resources

; This function creates and draws a titled XY plot to the workstation   ;
; "wks" (the variable returned from a previous call to "gsn_open_wks"). ;
; "resources" is an optional list of  resources. The Id of the map plot ;
; is returned.                                                          ;

; Check if frame and/or draw are not supposed to be called.

        if(isatt(res2,"gsnDraw"))
            if(.not.res2@gsnDraw)
                calldraw = False
            end if
            delete(res2@gsnDraw)
        end if
        if(isatt(res2,"gsnFrame"))
            if(.not.res2@gsnFrame)
                callframe = False
            end if
            delete(res2@gsnFrame)
        end if

;                                                                       ;
; This function behaves differently from gsn_xy in that it will         ;
; add additional titles to the top of the plot if any of the special    ;
        if(isatt(res2,"gsnShape").and.res2@gsnShape)
            sub_zone      = sub_zone+1  ; Zone for subtitles
            main_zone     = main_zone+1 ; Zone for main title
        end if
; "tiMainString" can be set to create a regular title).                 ;
; Check for left subtitle at top. If it exists, create a TextItem
; object for it.
; If gsnYRefLine is set to some value(s), then reference line(s) will be;
        if(isatt(res2,"gsnLeftString"))
            lft_str_object = create "left_string" textItemClass wks
              "txString" : res2@gsnLeftString
            end create
            left_string = True
            main_zone   = main_zone+1
            delete(res2@gsnLeftString)
        end if
      if(isatt(res2,"gsnShape").and.res2@gsnShape)
; Check for center subtitle at top. If it exists, create a TextItem
; object for it.
      end if
        if(isatt(res2,"gsnCenterString"))
            ctr_str_object = create "center_string" textItemClass wks
              "txString" : res2@gsnCenterString
            end create
            center_string = True
            main_zone     = main_zone+1
            delete(res2@gsnCenterString)
        end if
        delete(res2@gsnYRefLine)
; Check for right subtitle at top. If it exists, create a TextItem
; object for it.

        if(isatt(res2,"gsnRightString"))
            rgt_str_object = create "right_string" textItemClass wks
              "txString" : res2@gsnRightString
            end create
            right_string = True
            main_zone    = main_zone+1
            delete(res2@gsnRightString)
        end if
    end if
            print("gsn_csm_xy: Fatal: you must specify either one Y reference line color")
            return
          end if
        else
    xy_object = gsn_xy(wks,x,y,res2)   ; Create an XY plot.

; Get title label sizes and tickmark lengths.
    getvalues xy_object
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf

      "tmYLMajorLengthF"     : ylength
      "tmXBMajorLengthF"     : xlength
      "tmYLMinorLengthF"     : ymlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues
          yref_fill_on    = True
; Make tick marks same length and point outward.
    if(xref_line_on)
    major_length = min((/ylength,xlength/))
    minor_length = min((/ymlength,xmlength/))
    setvalues xy_object
      "tmXBMajorLengthF"     : major_length
      "tmXBMinorLengthF"     : minor_length
      "tmYLMajorLengthF"     : major_length
      "tmYLMinorLengthF"     : minor_length
      "tmXBMajorOutwardLengthF" : major_length
      "tmXBMinorOutwardLengthF" : minor_length
      "tmYLMajorOutwardLengthF" : major_length
      "tmYLMinorOutwardLengthF" : minor_length
    end setvalues
      end getvalues
    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.

; Leftmost subtitle.
    if(left_string)
      setvalues lft_str_object
        "txFontHeightF" : font_height
      end setvalues

      anno = NhlAddAnnotation(xy_object,lft_str_object)
      setvalues anno
        "amZone"          : sub_zone     ; Just outside plot area
        "amSide"          : "top"        ; Subtitle at top.
        "amParallelPosF"  : 0.0          ; 0.0 is right edge of plot
        "amJust"          : "bottomleft" ; Left justify
        "amOrthogonalPosF": 0.05         ; Move away from top edge
        "amResizeNotify"  : True         ; Resize subtitle if plot resized.
      end setvalues
    end if

; Center subtitle.
    if(center_string)
      setvalues ctr_str_object
        "txFontHeightF" : font_height
      end setvalues
      anno = NhlAddAnnotation(xy_object,ctr_str_object)
      setvalues anno
        "amZone"          : sub_zone       ; Just outside plot area
        "amSide"          : "top"          ; Subtitle at top.
        "amParallelPosF"  : 0.5            ; 0.5 is center of plot
        "amJust"          : "bottomcenter" ; Center it about the center
        "amOrthogonalPosF": 0.05           ; Move away from top edge
        "amResizeNotify"  : True           ; Resize subtitle if plot resized.
      end setvalues
    end if
main_zone, datanew, popgrid, contour_plot
; Rightmost subtitle.
    if(right_string)
      setvalues rgt_str_object
        "txFontHeightF" : font_height
      end setvalues
;
      anno = NhlAddAnnotation(xy_object,rgt_str_object)
      setvalues anno
        "amZone"          : sub_zone      ; Just outside plot area
        "amSide"          : "top"         ; Subtitle at top.
        "amParallelPosF"  : 1.0           ; 1.0 is right edge of plot
        "amJust"          : "bottomright" ; Right justify
        "amOrthogonalPosF": 0.05          ; Move away from top edge
        "amResizeNotify"  : True          ; Resize subtitle if plot resized.
      end setvalues
    end if
    set_right_subtitle(datanew,res2,res2)
; Draw all this stuff: XY plot and subtitles.
    if(calldraw)
        draw(xy_object) 
    end if

    if(callframe)
        frame(wks)           ; advance the frame
    end if
      set_attr(res2,"cnLineLabelsOn",False)
; Return XY plot object.
    end if
    return(xy_object)
      "tiXAxisFontHeightF"   : xfontf

                  right_string,subres)

; Draw all this stuff: streamline plot and subtitles.
    draw_and_frame(wks,stream_object,calldraw,callframe,maxbb)

; Return streamline plot object.

    return(stream_object)
end

;***********************************************************************;
; Function : gsn_csm_pres_hgt_streamline                                ;
;                     wks: workstation object                           ;
;                    data: data to be contoured                         ;
;                       u: u component of streamlines                   ;
;                       v: u component of streamlines                   ;
;               resources: optional resources                           ;
;                                                                       ;
; This function behaves like gsn_csm_pres_hgt, only it overlays a       ;
; streamline plot as well.                                              ;
;***********************************************************************;

function gsn_csm_pres_hgt_streamline(wks:graphic,data[*][*]:numeric, \
                                     u[*][*]:numeric, v[*][*]:numeric, \
                                     resources:logical)
local res2, cnres, stres, calldraw, callframe, npres
begin
  res2      = get_resources(resources)

  calldraw  = get_res_value(res2,"gsnDraw", True)
  callframe = get_res_value(res2,"gsnFrame",True)
  maxbb     = get_bb_res(res2)

  cnres = get_res_ne(res2,(/"st","vf","pm"/))
  stres = get_res_eq(res2,(/"st","vf"/))

;
; Set some contour resources.
;
  cnres = True
  cnres@gsnDraw                  = False
  cnres@gsnFrame                 = False
;
; Create contour pressure/height plot.
;
  contour = gsn_csm_pres_hgt(wks,data,cnres)

;
; Set some streamline resources.
;
  stres = True
  stres@gsnDraw         = False
  stres@gsnFrame        = False
  stres@gsnRightString  = ""  ; Use gsnRightString and gsnLeftString
  stres@gsnLeftString   = ""  ; from contour plot.
;
; Create streamline plot. We have to check the pressure values.  If they
; are not in descending order, then reverse 'em.
;
  if(is_valid_ycoord(u))
    npres = dimsizes(u&$u!0$)
    if(u&$u!0$(0).lt.u&$u!0$(npres-1))
      streamline = gsn_csm_streamline(wks,u(::-1,:),v(::-1,:),stres)
    else
      streamline = gsn_csm_streamline(wks,u,v,stres)
    end if
  else
    streamline = gsn_csm_streamline(wks,u,v,stres)
  end if

;
; Overlay the streamlines on the contour plot.
; 
  overlay(contour,streamline)

; Draw all this stuff: streamline over contour plot.

  draw_and_frame(wks,contour,calldraw,callframe,maxbb)

; Return contour/streamline plot with data objects as attributes.

  contour@sfdata = contour@data
  contour@vcdata = streamline@data
  return(contour)
end

