;
;      $Id: gsn_cgd.ncl,v 1.1 1999-01-27 17:39:03 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1998                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       gsn_cgd.ncl
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
;;                   load "gsn_cgd.ncl"
;; 
;;               at the top of your NCL script, before the begin statement.
;;

;***********************************************************************;
; For every function and procedure defined in this script, undefine it  ;
; with a call to "undef" so it doesn't clash with other functions and   ;
; procedures with the same name.                                        ;
;***********************************************************************;

undef("add_cyclic_point")
undef("gsn_labeled_map_ce")
undef("gsn_labeled_map_polar")
undef("gsn_labeled_map")
undef("gsn_contour_ezp1_polar")
undef("gsn_contour_ezp1_ce")
undef("gsn_contour_labeled_map")

function add_cyclic_point(data[*][*]:float)
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
 
;***********************************************************************;
; Function : gsn_labeled_map_ce                                         ;
;                   wks: workstation object                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a labeled cylindrical equidistant map ;
; plot to the workstation "wks" (the variable returned from a previous  ;
; call to "gsn_open_wks").  "resources" is an optional list of          ;
; resources. The Id of the map plot is returned.                        ;
;                                                                       ;
; This function behaves differently from gsn_map in that it will        ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. The lat/lon grid is labeled with tickmarks.                     ;
;    2. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top left, center, and right of the plot   ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;***********************************************************************;

function gsn_labeled_map_ce(wks:graphic,resources:logical)
local i, attnames, res, mp_res_index, map_object, tickmark_object, \
lft_str_object, tm_res_index, ctr_str_object, rgt_str_object, calldraw, \
callframe, left_string, center_string, right_string, sub_zone, main_zone, \
min_lat, max_lat, min_lon, max_lon, res2, tickmark_res
begin

; Initialize.

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
    main_zone     = 2         ; Zone for main title (may change later)
    res2 = resources

;
; Any viewport resources that are set need to be set when the object is
; created, and not later in a setvalues call.
;
    if(isatt(res2,"vpXF"))
      map_vpxf = res2@vpXF
      delete(res2@vpXF)
    end if

    if(isatt(res2,"vpYF"))
      map_vpyf = res2@vpYF
      delete(res2@vpYF)
    end if

    if(isatt(res2,"vpWidthF"))
      map_vpwidthf = res2@vpWidthF
      delete(res2@vpWidthF)
    end if

    if(isatt(res2,"vpHeightF"))
      map_vpheightf = res2@vpHeightF
      delete(res2@vpHeightF)
    end if

; This section tests for more special resources: those that start
; with "gsn."

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

; Check for left subtitle at top. If it exists, create a TextItem
; object for it.

        if(isatt(res2,"gsnLeftString"))
            lft_str_object = create "ezplot1_left_string" textItemClass wks
              "txString" : res2@gsnLeftString
            end create
            left_string = True
            main_zone   = 3
            delete(res2@gsnLeftString)
        end if

; Check for center subtitle at top. If it exists, create a TextItem
; object for it.

        if(isatt(res2,"gsnCenterString"))
            ctr_str_object = create "ezplot1_center_string" textItemClass wks
              "txString" : res2@gsnCenterString
            end create
            center_string = True
            main_zone     = 3
            delete(res2@gsnCenterString)
        end if

; Check for right subtitle at top. If it exists, create a TextItem
; object for it.

        if(isatt(res2,"gsnRightString"))
            rgt_str_object = create "ezplot1_right_string" textItemClass wks
              "txString" : res2@gsnRightString
            end create
            right_string = True
            main_zone    = 3
            delete(res2@gsnRightString)
        end if
    end if

; Create the map object.

    map_object = create "ezplot1_map" mapPlotClass wks
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

; This section tests for regular resources.

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

    tickmark_object = create "ezplot1_tickmarks" tickMarkClass wks
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

; Make sure axes labels (if any) are the same size.

    getvalues map_object
        "tiXAxisFontHeightF"   : xfontf
        "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
    setvalues map_object
        "tiXAxisFontHeightF" : font_height   ; X axis label size
        "tiYAxisFontHeightF" : font_height   ; Y axis label size
        "tiMainFontHeightF"  : 1.3*font_height  ; main title size
        "tiMainFont"         : "helvetica-bold"     ; main title font
    end setvalues
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

; Set up three subtitles at top, if they exist.
; Leftmost subtitle.
    if(left_string)
      setvalues lft_str_object
        "txFontHeightF" : font_height
      end setvalues

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

; Draw all this stuff: map plot, subtitles, and tick marks.
    if(calldraw)
        draw(map_object) 
    end if

    if(callframe)
        frame(wks)           ; advance the frame
    end if

; Return plot object and tickmark object.

    map_object@tickmarks = tickmark_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_labeled_map_polar                                      ;
;                   wks: workstation object                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a polar stereographic map plot to the ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks"). "resources" is an optional list of resources. The id ;
; of the map plot is returned.                                          ;
;                                                                       ;
; This function behaves differently from gsn_map in that it will        ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. The longitude lines are labeled.                                ;
;    2. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top of the plot.                          ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;    3. If either of the resources "gsnPolarNH" or "gsnPolarSH" are set ;
;       to True then only the northern or southern hemisphere is        ;
;       displayed.                                                      ;
;***********************************************************************;

function gsn_labeled_map_polar(wks:graphic,resources:logical)
local i, attnames, res, res2, mp_res_index, map_object, \
lft_str_object, ctr_str_object, rgt_str_object, main_zone, sub_zone, \
calldraw, callframe, left_string, center_string, right_string, \
min_lat, max_lat, center_lat, polar_nh, polar_sh, yoffset, main_offset
begin

; Initialize.

    calldraw      = True
    callframe     = True
    left_string   = False
    center_string = False
    right_string  = False
    yoffset       = 0.08
    main_offset   = 0.02
    polar_sh      = False
    polar_nh      = False
    min_lat       = 0.0
    max_lat       = 90.0
    center_lat    = 0.0 
    sub_zone      = 2         ; Zone for subtitles
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = resources

; This section tests for special resources: those that start with "gsn."

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

; Create a polar stereographic map object.

    map_object = create "ezplot1p_map" mapPlotClass wks
      "vpHeightF"            : 0.62
      "vpWidthF"             : 0.62
      "vpXF"                 : 0.20
      "vpYF"                 : 0.80

      "mpProjection"         : "Stereographic"
      "mpEllipticalBoundary" : True
      "mpLimitMode"          : "LatLon"

      "mpFillOn"             : True
      "mpFillColors"         : (/"background","transparent","LightGray",\
                                 "transparent"/)
      "mpOutlineOn"          : False
      "mpGridAndLimbOn"      : True
      "mpGridLineDashPattern": 2
      "mpGridLonSpacingF"    : 30

      "pmTitleDisplayMode"   : "Always"
      "pmTitleZone"          : main_zone
    end create

; Check if we want northern or southern hemisphere.

    if(polar_nh.or.polar_sh)
        setvalues map_object
            "mpMinLatF"    : min_lat
            "mpMaxLatF"    : max_lat
            "mpCenterLatF" : center_lat
        end setvalues
    end if

;
; By default, mpOutlineOn is False, mpFillOn is set to False, then 
; it is set back to True.
;
    if((isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        setvalues map_object
          "mpOutlineOn" : True
        end setvalues
    end if

; This section tests for regular resources.

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

; Retrieve the view port location of the map plot so we know where
; to put titles.

    getvalues map_object
      "vpXF"      : vpxf
      "vpYF"      : vpyf
      "vpWidthF"  : vpwidthf
      "vpHeightF" : vpheightf
      "mpMinLatF" : min_lat
      "mpMaxLatF" : max_lat
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vpheightf     ; Make various label sizes a function
                                       ; of the height of the viewport.
    setvalues map_object
        "tiXAxisFontHeightF"   : 1.2*font_height      ; x axis font size
        "tiYAxisFontHeightF"   : 1.2*font_height      ; y axis font size
        "tiMainOffsetYF"       : main_offset          ; main title offset
        "tiMainFont"           : "helvetica-bold"     ; main title font
        "tiMainFontHeightF"    : 2. * font_height     ; main title size
    end setvalues

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

    return(map_object)
end

;***********************************************************************;
; Function : gsn_labeled_map                                        ;
;                   wks: workstation object                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_labeled_map_ce or                      ;
; gsn_labeled_map_polar depending on if gsnPolar is set to True.        ;
;***********************************************************************;

function gsn_labeled_map(wks:graphic,res:logical)
begin

  if(res.and.isatt(res,"gsnPolarNH").and.res@gsnPolarNH.or.\
             isatt(res,"gsnPolarSH").and.res@gsnPolarSH)
    return(gsn_labeled_map_polar(wks,res))
  else
    return(gsn_labeled_map_ce(wks,res))
  end if
  delete(res)
end


;***********************************************************************;
; Function : gsn_contour_ezp1_polar                                     ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a contour plot over a polar           ;
; stereographic map plot to the workstation "wks" (the variable         ;
; returned from a previous call to "gsn_open_wks").  "data" is the      ;
; 2-dimensional data to be contoured, and "resources" is an optional    ;
; list of resources. The id of the map plot is returned.                ;
;                                                                       ;
; This function behaves differently from gsn_contour in that it will    ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. By default, a cyclic point is added.  If gsnAddCyclic is set to ;
;       False, then the cyclic point is not added.                      ;
;    2. The longitude lines are labeled.                                ;
;    3. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top of the plot.                          ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;    4. If the resource "cnFillOn" is set to True, then a labelbar is   ;
;       drawn and line labels are turned off.                           ;
;    5. If data has an attribute called "long_name," and "gsnLeftString";
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the left string title.                              ;
;    6. If data has an attribute called "units," and "gsnRightString"   ;
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the right string title.                             ;
;    7. If either of the resources "gsnPolarNH" or "gsnPolarSH" are set ;
;       to True then only the northern or southern hemisphere is        ;
;       displayed.                                                      ;
;***********************************************************************;

function gsn_contour_ezp1_polar(wks:graphic,data[*][*]:numeric, \
                                resources:logical)
local i, attnames, contour_object, map_object, res, cn_res_index, \
mp_res_index, lbar_pos, lbar_zone, lbar_on, calldraw, callframe, \
main_offset, addcyclic, datanew, res2, cnres, mapres, \
vpwidthf, vpheightf, font_height, levels, colors, zonal_zone, lbar_zone, \
infolabel_on, infolabel_zone
begin

; Initialize.

    calldraw      = True      ; Default is to draw plot
    callframe     = True      ; Default is to advance frame
    lbar_on       = False     ; Labelbar flag
    addcyclic     = True      ; Default is cyclic data
    res2          = resources ; Copy of resources
    lbar_zone     = 3         ; Zone for labelbar (may change later)
    mapres        = True      ; Will hold map resources
    cnres         = True      ; Will hold contour resources

; The default is to add a cyclic point.
    if((res2).and.isatt(res2,"gsnAddCyclic"))
        if(.not.res2@gsnAddCyclic)
            addcyclic = False
        end if
        delete(res2@gsnAddCyclic)
    end if

    if(addcyclic)
        datanew = add_cyclic_point(data)
    else
        datanew = data
    end if

; This section tests for special resources: those that start with "gsn."

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

; Create data object and use coordinate variables if they exist.

    if(iscoord(datanew,"lat").and.iscoord(datanew,"lon"))
      cnres@sfYCStartV  = datanew&lat(0)
      cnres@sfYCEndV    = datanew&lat(dimsizes(datanew&lat)-1)
      cnres@sfXCStartV  = datanew&lon(0)
      cnres@sfXCEndV    = datanew&lon(dimsizes(datanew&lon)-1)
    end if

; Check for a missing value.

    if(isatt(datanew,"_FillValue")) then
      cnres@sfMissingValueV = datanew@_FillValue
    end if

; Set some more contour plot resources.

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

;
; By default, mpOutlineOn is False, unless cnFillOn is set to True
; or mpFillOn is set to False, then it is set back to True.
;
    if((isatt(res2,"cnFillOn").and.res2@cnFillOn).or.\
       (isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        mapres@mpOutlineOn  = True
    end if

; This section tests for regular resources.

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
    end if

;
; Compute zone for labelbar if it is supposed to get drawn.
; Zone for labelbar changes only if an info label is not drawn.
;
    if(isatt(res2,"cnInfoLabelOn").and..not.res2@cnInfoLabelOn)
      lbar_zone = 2
    end if

; Before we create the objects, turn off draw and frame for them.
    cnres@gsnDraw   = False
    cnres@gsnFrame  = False
    mapres@gsnDraw  = False
    mapres@gsnFrame = False

    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    map_object = gsn_labeled_map(wks,mapres)        ; Create map.
    overlay(map_object,contour_object)              ; Overlay contour plot
                                                    ; on map.
;
; Retrieve the view port location of the map plot so we know where
; to put titles and labelbar (if there is one).
;
    getvalues map_object
      "vpWidthF"  : vpwidthf
      "vpHeightF" : vpheightf
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vpheightf     ; Make various label sizes a function
                                       ; of the height of the viewport.
    setvalues map_object
        "tiXAxisFontHeightF"   : 1.2*font_height      ; x axis font size
        "tiYAxisFontHeightF"   : 1.2*font_height      ; y axis font size
        "tiMainFont"           : "helvetica-bold"     ; main title font
        "tiMainFontHeightF"    : 2. * font_height     ; main title size
    end setvalues

; Create a labelbar.

    if(lbar_on)
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
    end if
  
    if(calldraw)
        draw(map_object)     ; Draw everything.
    end if

    if(callframe)
        frame(wks)           ; Advance the frame.
    end if

; Return plot object and data object (as attribute of plot object).

    map_object@data    = contour_object@data
    map_object@contour = contour_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_contour_ezp1_ce                                        ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
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
;       create a regular title).                                        ;
;    3. If the special GSUN resource "gsnZonalMean" is set to True, then;
;       a zonal mean XY plot is drawn.
;    4. If the resource "cnFillOn" is set to True, then a labelbar is   ;
;       drawn and line labels are turned off.                           ;
;    5. If data has an attribute called "long_name," and "gsnLeftString";
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the left string title.                              ;
;    6. If data has an attribute called "units," and "gsnRightString"   ;
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the right string title.                             ;
;***********************************************************************;

function gsn_contour_ezp1_ce(wks:graphic,data[*][*]:numeric,resources:logical)
local i, attnames, contour_object, labelbar_object, xy_object, map_object, \
cn_res_index, mp_res_index, calldraw, callframe, lbar_on, zonal_mean_plot,\
min_lat, max_lat, addcyclic, datanew, res, res2, \
tickmark_res, xy_res, mapres, cnres, levels, colors, \
map_vpxf, map_vpyf, map_vpwidthf, map_vpheightf, vpwidthf, vpheightf
begin

; Initialize.

    calldraw      = True
    callframe     = True
    map_vpxf      = 0.1       ; X position of map plot in viewport.
    map_vpyf      = 0.9       ; Y position of map plot in viewport.
    map_vpwidthf  = 0.8       ; Viewport width and height for
    map_vpheightf = 0.8       ; map plot.

    lbar_on        = False    ; Default is no labelbar.
    addcyclic      = True     ; Default is to add a cyclic point.
    zonal_mean_plot= False    ; Default is no zonal mean plot.
    tickmark_res   = False
    cnres          = True
    mapres         = True

    infolabel_zone = 2        ; Zone for info label (may change later)
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    zonal_zone     = 2        ; Zone for zonal means plot

    res2 = resources

; The default is to add a cyclic point.
    if((res2).and.isatt(res2,"gsnAddCyclic"))
        if(.not.res2@gsnAddCyclic)
            addcyclic = False
        end if
        delete(res2@gsnAddCyclic)
    end if

    if(addcyclic)
        datanew = add_cyclic_point(data)
    else
        datanew = data
    end if

; Check for a missing value.

    if(isatt(datanew,"_FillValue")) then
      cnres@sfMissingValueV = datanew@_FillValue
    end if

; Check for coordinate variables. These values will determine where to 
; overlay contour on map.

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
    end if

; Check if a zonal mean plot is desired.

    if((res2).and.isatt(res2,"gsnZonalMean"))
      if(iscoord(datanew,"lat").and.iscoord(datanew,"lon")) then
        if(res2@gsnZonalMean)
          zonal_mean_plot = True
          zmeans2 = dim_avg(datanew(lat | : ,lon | :))
          map_vpwidthf  = 0.7   ; Make map slightly smaller so that
          map_vpheightf = 0.7   ; zonal mean plot will fit in view port.
          if(min(zmeans2).lt.0.and.max(zmeans2).gt.0)
            nmeans = dimsizes(zmeans2)
            zmeans = new((/2,nmeans/),float)
            zmeans(0,:) = zmeans2
            zmeans(1,:) = fspan(0.,0.,nmeans)
          else
            zmeans = zmeans2
          end if
          delete(zmeans2)
        end if
      else
        print("The resource gsnZonalMean can only be set to True")
        print("if the data has coordinate variables 'lat' and 'lon'")
      end if
      delete(res2@gsnZonalMean)
    end if

; Create some contour plot resources.

    cnres@cnInfoLabelFont            = "helvetica"
    cnres@cnLineLabelFont            = "helvetica"
    cnres@cnLineLabelBackgroundColor = "transparent"

; Create a cylindrical equidistant map object.

    mapres@tiXAxisOffsetYF = -0.02
    mapres@tiYAxisOffsetXF = -0.04

    mapres@vpXF            = map_vpxf
    mapres@vpYF            = map_vpyf
    mapres@vpWidthF        = map_vpwidthf
    mapres@vpHeightF       = map_vpheightf
    mapres@mpGridAndLimbOn = False    ; Turn off grid lines, limb line,
    mapres@mpOutlineOn     = False    ; and map outlines.
    mapres@mpFillOn        = True     ; Fill map boundaries according to
                                      ; "mpFillColors".
    mapres@mpFillColors    = (/"background","transparent","LightGray",\
                               "transparent"/)   ; (default,ocean,land,
                                                 ;  inland water)

; This section tests for more special resources: those that start
; with "gsn."

    if(isatt(datanew,"long_name").and..not.isatt(res2,"gsnLeftString"))
        mapres@gsnLeftString = datanew@long_name
    end if
    if(isatt(datanew,"units").and..not.isatt(res2,"gsnRightString"))
        mapres@gsnRightString = datanew@units
    end if

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

        if(isatt(res2,"gsnRightString"))
            mapres@gsnRightString = res2@gsnRightString
            delete(res2@gsnRightString)
        end if
    end if

;
; By default, mpOutlineOn is False, unless cnFillOn is set to True
; or mpFillOn is set to False, then it is set back to True.
;
    if((isatt(res2,"cnFillOn").and.res2@cnFillOn).or.\
       (isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        mapres@mpOutlineOn = True
    end if

; This section tests for regular resources.

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

;
; Compute zones for zonal means plot, info label, and labelbar if
; they are supposed to get drawn.
;
    if(isatt(res2,"cnInfoLabelOn").and..not.res2@cnInfoLabelOn)
      infolabel_on = False
    else
      infolabel_on = True
    end if
    if(zonal_mean_plot)
      if(infolabel_on)
        infolabel_zone = zonal_zone + 1
        lbar_zone      = zonal_zone + 2
      else
        lbar_zone      = zonal_zone + 1
      end if
    else
      if(infolabel_on)
        lbar_zone      = infolabel_zone + 1
      end if
    end if

    if(.not.isatt(res2,"cnInfoLabelOrthogonalPosF").and.infolabel_zone.eq.2)
      cnres@cnInfoLabelOrthogonalPosF = 0.13
    end if
    cnres@cnInfoLabelZone           = infolabel_zone

; Before we create the objects, turn off draw and frame for them.
    cnres@gsnDraw   = False
    cnres@gsnFrame  = False
    mapres@gsnDraw  = False
    mapres@gsnFrame = False

    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    map_object = gsn_labeled_map(wks,mapres)    ; Create map.
    overlay(map_object,contour_object)              ; Overlay contours on map.

    getvalues map_object@tickmarks
      "tmXBLabelFontHeightF" : xbfontf
      "tmXBMajorLengthF"     : xlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

    getvalues map_object
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
    setvalues map_object
        "tiXAxisFontHeightF"   : font_height   ; X axis label size
        "tiYAxisFontHeightF"   : font_height   ; Y axis label size
        "tiMainFontHeightF"    : 1.3*font_height  ; main title size
    end setvalues

    setvalues contour_object
        "cnInfoLabelFontHeightF"    : xbfontf
        "cnLineLabelFontHeightF"    : xbfontf
    end setvalues

; Retrieve the view port size of the map plot so we know how big
; to make labelbar and where to put zonal means plot (if there is one).

    getvalues map_object
      "vpWidthF"     : vpwidthf
      "vpHeightF"    : vpheightf
    end getvalues

; Create a labelbar.
    if(lbar_on)
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

      getvalues map_object
        "mpMinLatF"    : min_lat
        "mpMaxLatF"    : max_lat
      end getvalues

      xy_res = True
      xy_res@gsnDraw              = False
      xy_res@gsnFrame             = False
      xy_res@tmXBLabelFontHeightF = xbfontf
      xy_res@tmYLOn               = False
      xy_res@tmYROn               = False
      xy_res@tmXBMaxTicks         = 3
      xy_res@tmXBMinorPerMajor    = 1
      xy_res@tmXTMinorPerMajor    = 1
      xy_res@trYMaxF              = max_lat
      xy_res@trYMinF              = min_lat
      xy_res@vpHeightF            = vpheightf
      xy_res@vpWidthF             = 0.15 * map_vpheightf
      xy_res@xyDashPatterns       = (/0,0/)
      xy_res@tiXAxisOn            = False
      xy_res@tiYAxisOn            = False
      xy_res@tmXBMajorLengthF     = xlength
      xy_res@tmXBMinorLengthF     = xmlength
      xy_res@tmYRMajorLengthF     = xlength
      xy_res@tmYRMinorLengthF     = xmlength
      xy_res@tmXBMajorOutwardLengthF = xlength
      xy_res@tmXBMinorOutwardLengthF = xmlength
      xy_res@tmYRMajorOutwardLengthF = xlength
      xy_res@tmYRMinorOutwardLengthF = xmlength
;      xy_res@tiMainFontHeightF    = font_height
;      xy_res@tiMainString         = "Zonal Mean"
      xy_object = gsn_xy(wks,zmeans,datanew&lat,xy_res)
      anno = NhlAddAnnotation(map_object,xy_object)
      setvalues anno
        "amZone"          : zonal_zone   ; Just outside plot area
        "amSide"          : "right"      ; Plot at right.
        "amResizeNotify"  : True         ; Resize if map resized.
        "amParallelPosF"  : 0.5          ; 0.5 is center of plot
        "amOrthogonalPosF": 0.05         ; Move away from right edge
      end setvalues
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.
    if(calldraw)
        draw(map_object) 
    end if

    if(callframe)
        frame(wks)           ; advance the frame
    end if

; Return plot object and data object (as attribute of plot object).

    map_object@data    = contour_object@data
    map_object@contour = contour_object
    if(zonal_mean_plot)
      map_object@xy = xy_object
    end if
    return(map_object)
end

;***********************************************************************;
; Function : gsn_contour_labeled_map                                    ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_contour_ezp1_ce or                     ;
; gsn_contour_ezp1_polar depending on if gsnPolar is set to True.       ;
;***********************************************************************;

function gsn_contour_labeled_map(wks:graphic,data[*][*]:numeric,\
resources:logical)
local res2
begin
  res2     = resources

  if((res2).and.isatt(res2,"gsnPolarNH").and.res2@gsnPolarNH.or.\
                isatt(res2,"gsnPolarSH").and.res2@gsnPolarSH)
    return(gsn_contour_ezp1_polar(wks,data,res2))
  else
    return(gsn_contour_ezp1_ce(wks,data,res2))
  end if
  delete(res2)
end

