;
;      $Id: gsn_code.ncl,v 1.28 2000-01-07 01:05:35 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1998                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       gsn_code.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Sat Apr 11 12:42:53 MST 1998
;;
;;  Description: This script defines all of the basic plotting and
;;               miscellaneous functions and procedures used in the 
;;               examples in the "Getting started using NCL" documention.
;;               The URL for this document is:
;;
;;                   http://ngwww.ucar.edu/ngdoc/ng/ug/ncl/gsun/
;;
;;               To use the functions and procedures in this script,
;;               you must have the line:
;;
;;                   load "gsn_code.ncl"
;; 
;;               at the top of your NCL script, before the begin statement.
;;

;***********************************************************************;
; For every function and procedure defined in this script, undefine it  ;
; with a call to "undef" so it doesn't clash with other functions and   ;
; procedures with the same name.                                        ;
;***********************************************************************;

undef("set_attr")
undef("get_res_eq")
undef("get_res_ne")
undef("get_special_res_value")
undef("spread_colors")
undef("check_for_irreg2loglin")
undef("gsnp_turn_off_tickmarks")
undef("gsnp_point_tickmarks_outward")
undef("gsnp_uniform_tickmark_labels")
undef("gsnp_shape_plot")
undef("gsnp_scale_plot")
undef("check_for_tickmarks_off")
undef("draw_and_frame")
undef("gsn_open_ps")
undef("gsn_open_x11")
undef("gsn_open_ncgm")
undef("gsn_open_wks")
undef("gsn_define_colormap")
undef("gsn_retrieve_colormap")
undef("gsn_panel")
undef("gsn_contour")
undef("gsn_contour_map")
undef("gsn_labelbar_ndc")
undef("gsn_map")
undef("gsn_polygon")
undef("gsn_polygon_ndc")
undef("gsn_polyline")
undef("gsn_polyline_ndc")
undef("gsn_polymarker")
undef("gsn_polymarker_ndc")
undef("gsn_streamline")
undef("gsn_streamline_map")
undef("gsn_text")
undef("gsn_text_ndc")
undef("gsn_vector")
undef("gsn_vector_contour")
undef("gsn_vector_contour_map")
undef("gsn_vector_map")
undef("gsn_vector_scalar")
undef("gsn_vector_scalar_map")
undef("gsn_xy")

;***********************************************************************;
; Procedure : set_attr                                                  ;
;                res:logical                                            ;
;           att_name: string                                            ;
;          att_value                                                    ;
;                                                                       ;
; Add resource and its value to a resource list if it isn't already set.;
;***********************************************************************;

procedure set_attr(res:logical,att_name:string,att_value)
begin
  res = True
  if(.not.isatt(res,att_name))
    res@$att_name$  = att_value
  end if
  return
end

;***********************************************************************;
; Function : get_res_eq                                                 ;
;                res:logical                                            ;
;             prefix: string                                            ;
;                                                                       ;
; Get a list of resources that start with res_prefix.                   ;
;***********************************************************************;

function get_res_eq(res,res_prefix:string)
local i, j, ret_res, res2, attnames, res_index
begin
  ret_res = False

  if(res.and..not.any(ismissing(getvaratts(res))))
    attnames = getvaratts(res)
    res2 = stringtochar(attnames(ind(attnames.ne."_FillValue")))
;
; Only one resource set.
;
    if(dimsizes(dimsizes(res2)).eq.1)
      if(any(chartostring(res2(0:1)).eq.res_prefix))
        ret_res = True
        ret_res@$attnames$ = res@$attnames$
      end if
    else
;
; Multiple resources set. They have to be checked differently than if
; just one resource is set. 
;
      do j=0,dimsizes(res_prefix)-1
        res_index = ind(chartostring(res2(:,0:1)).eq.res_prefix(j))
        if(.not.all(ismissing(res_index)))
          ret_res = True
          do i = 0,dimsizes(res_index)-1
            ret_res@$attnames(res_index(i))$ = res@$attnames(res_index(i))$
          end do
        end if
        delete(res_index)
      end do
    end if
    delete(res2)
    delete(attnames)
  end if
  return(ret_res)
end

;***********************************************************************;
; Function : get_res_ne                                                 ;
;                res:logical                                            ;
;             prefix: string                                            ;
;                                                                       ;
; Get a list of resources that don't start with res_prefix.             ;
;***********************************************************************;

function get_res_ne(res,res_prefix:string)
local i, ret_res, res2, attnames, res_index
begin
  ret_res = False

  if(res.and..not.any(ismissing(getvaratts(res))))
    attnames = getvaratts(res)
    res2 = stringtochar(attnames(ind(attnames.ne."_FillValue")))

    if(dimsizes(dimsizes(res2)).eq.1)
      if(all(chartostring(res2(0:1)).ne.res_prefix))
        ret_res = True
        ret_res@$attnames$ = res@$attnames$
      end if
    else
      nres = dimsizes(res2(:,0))
      do i = 0,nres-1
        if(all(chartostring(res2(i,0:1)).ne.res_prefix))
          ret_res = True
          ret_res@$attnames(i)$ = res@$attnames(i)$
        end if
      end do
    end if
    delete(res2)
    delete(attnames)
  end if
  return(ret_res)
end

;***********************************************************************;
; Function : get_special_res_value                                      ;
;                res:logical                                            ;
;            resname:string                                             ;
;        default_val                                                    ;
;                                                                       ;
; This function checks to see if the given special "gsn" resource has   ;
; been set, and if it has, it returns its value and removes it from the ;
; resource list.  Otherwise, it returns the default value which is the  ;
; last argument passed in.                                              ;
;***********************************************************************;
function get_special_res_value(res:logical,resname:string,default_val)
local return_val
begin
  return_val = default_val

  if(res.and..not.any(ismissing(getvaratts(res)))) then
    if(isatt(res,resname)) then
      return_val = res@$resname$
      delete(res@$resname$)
    end if
  end if

  return(return_val)
end

;***********************************************************************;
; Function : spread_colors                                              ;
;                wks:graphic                                            ;
;               plot:graphic                                            ;
;          min_index:logical                                            ;
;          max_index:logical                                            ;
;                                                                       ;
; By default, all of the plotting routines use the first n colors from  ;
; a color map, where "n" is the number of contour or vector levels.     ;
; If "gsnSpreadColors" is set to  True, then the colors are spanned     ;
; across the whole color map. The min_index and max_index values are    ;
; used for the start and end colors.  If either min_index or max_index  ;
; is < 0 (but not both), then this indicates to use ncol-i, where "i"   ;
; is equal to the negative value.                                       ;
;                                                                       ;
; If after adjusting for negative index color(s), and                   ;
; max_index < min_index, then the colors are reversed.                  ;
;***********************************************************************;
function spread_colors(wks:graphic,plot:graphic,min_index:integer,\
                        max_index:integer)
local ncols, lcount, fcols, icols, minix, maxix, nc, fmin, fmax, class,\
levelcountres
begin
  class = NhlClassName(plot)
  if(.not.any(class.eq.(/"contourPlotClass","logLinPlotClass",\
                         "vectorPlotClass"/)))
    print("spread_colors: invalid plot: defaulting")
    return(ispan(2,255,1))
  end if

  if (class.eq."contourPlotClass".or.class.eq."logLinPlotClass")
    levelcountres = "cnLevelCount"
  else
    levelcountres = "vcLevelCount"
  end if

  getvalues wks
    "wkColorMapLen" : ncols
  end getvalues

  if (class.eq."contourPlotClass".or.class.eq."vectorPlotClass")
    getvalues plot
      levelcountres : lcount
    end getvalues
  else
    getvalues plot@contour
      levelcountres : lcount
    end getvalues
  end if
;
; -1 indicates that min/max_index should be set equal to ncols - 1
; -2 indicates that min/max_index should be set equal to ncols - 2, etc.
;
; If after adjusting for negative indices, and maxix < minix, then 
; this implies that the user wants to reverse the colors.
;
  if (min_index .lt. 0)
    minix = ncols + min_index
  else
    minix = min_index
  end if

  if (max_index .lt. 0)
    maxix = ncols + max_index
  else
    maxix = max_index
  end if

;
; Make sure indices fall within range of the color map.
;
  minix = min((/ncols-1,max((/0,minix/))/))
  maxix = min((/ncols-1,max((/0,maxix/))/))
  icols = floattointeger(fcols + 0.5)
; If maxix < minix, then colors are to be reversed.
;
  reverse = False
  if(maxix.lt.minix)
    reverse = True
; something like "1.00001e-08".  But, we also have to make sure we don't;
; have values like 1e-10, 1e-9, etc, where we *do* want "1e-10" label   ;
; and not a "0". Don't even bother with checking if the minimum         ;
; difference between the levels is less than 1e-5.                      ;
;***********************************************************************;
procedure fix_zero_contour(levels)
begin
  nlevels = dimsizes(levels)
  delta_levels = min( levels(1:nlevels-1) - levels(0:nlevels-2) )
  if(delta_levels.ge.1e-5)
    do n=1,nlevels-2
      if(fabs(levels(n)).le.1.e-5.and.levels(n-1).lt.0..and.levels(n+1).gt.0.)
        levels(n) = 0.0
      end if
    end do
  end if

procedure check_for_irreg2loglin(res:logical,xlinear:logical,ylinear:logical,\
end

;***********************************************************************;
  xlinear = get_special_res_value(res,"gsnXAxisIrregular2Linear",xlinear)
  ylinear = get_special_res_value(res,"gsnYAxisIrregular2Linear",ylinear)
  xlog    = get_special_res_value(res,"gsnXAxisIrregular2Log",xlog)
  ylog    = get_special_res_value(res,"gsnYAxisIrregular2Log",ylog)
;               xlog:logical                                            ;
;               ylog:logical                                            ;
;                                                                       ;
; If any of the sf*Array or vf*Array resources are set, this puts the   ;
; plot into "irregular" mode. If you want to make any of your axes log  ;
; or linear then, you have to overlay it on a LogLin plot.              ;
;                                                                       ;
; By setting one of the resources gsn{X,Y}AxisIrregular2Linear or       ;
; gsnXAxisIrregular2Log to True, the overlay is done for you. This      ;
; procedure checks for these resources being set and sets some logical  ;
; variables accordingly.                                                ;
;***********************************************************************;
procedure check_for_irreg2loglin(res:logical,xlinear:logical, \
                                 ylinear:logical,\
                                 xlog:logical,ylog:logical)
begin
  dy = scale * (top - vpy) ; Calculate distance from plot's top position
                           ; to its topmost annotation.
;
; Calculate new viewport coordinates.
; 
  new_uw = uw * scale
  new_uh = uh * scale
  new_ux =     .5 * (1-new_uw)
  new_uy = 1 - .5 * (1-new_uh)


  new_vpx = new_ux + dx
  new_vpy = new_uy - dy
  new_vpw = vpw * scale
  new_vph = vph * scale
;
; Return new coordinates 
;
  return((/new_vpx,new_vpy,new_vpw,new_vph/))
end


;***********************************************************************;
; Procedure : gsnp_turn_off_tickmarks                                   ;
;                res:logical                                            ;
;              x_major_length:float                                     ;
;              y_major_length:float                                     ;
;              x_minor_length:float                                     ;
;              y_minor_length:float                                     ;
;              major_length:float                                       ;
;              minor_length:float                                       ;
begin
  set_attr(res,"tmXBBorderOn",False )
  set_attr(res,"tmXBOn",      False)
  set_attr(res,"tmXTBorderOn",False)
  set_attr(res,"tmXTOn",      False)
  set_attr(res,"tmYLBorderOn",False)

procedure gsnp_point_tickmarks_outward(plot:graphic, \
                              x_major_length:float, y_major_length:float, \
                              x_minor_length:float, y_minor_length:float, \
end
;***********************************************************************;
; Procedure : gsnp_point_tickmarks_outward                              ;
;              plot:object                                              ;
;               res:logical                                             ;
;              x_major_length:numeric                                   ;
;              y_major_length:numeric                                   ;
;              x_minor_length:numeric                                   ;
;              y_minor_length:numeric                                   ;
;              major_length:numeric                                     ;
;              minor_length:numeric                                     ;
;                                                                       ;
; By default, tickmarks are drawn pointing inwards.  This procedure     ;
; makes them point out. This procedure also sets the major and/or minor ;
; tickmarks on both axes to be the same length if the major and/or minor;
; tickmarks lengths are != 0.                                           ;
;***********************************************************************;
procedure gsnp_point_tickmarks_outward(plot:graphic,res:logical, \
                              x_major_length, y_major_length, \
                              x_minor_length, y_minor_length, \
                              major_length, minor_length)
local tmres
begin
  if(major_length.lt.0.)
    getvalues plot
      "tmXBMajorLengthF"   : x_major_length
      "tmYLMajorLengthF"   : y_major_length
    end getvalues
    major_length = min((/x_major_length,y_major_length/))
    if(x_major_length.gt.0..and.y_major_length.gt.0.)
      x_major_length = min((/x_major_length,y_major_length/))
      y_major_length = x_major_length
    end if
  else
    if(x_major_length.gt.0.)
      x_major_length = major_length
    end if
    if(y_major_length.gt.0.)
      y_major_length = major_length
  setvalues plot
    "tmXBMajorLengthF"        : x_major_length
    "tmXBMajorOutwardLengthF" : x_major_length
    "tmXBMinorLengthF"        : x_minor_length
    "tmXBMinorOutwardLengthF" : x_minor_length
    "tmXTMajorLengthF"        : x_major_length
    "tmXTMajorOutwardLengthF" : x_major_length
    "tmXTMinorLengthF"        : x_minor_length
    "tmXTMinorOutwardLengthF" : x_minor_length
    "tmYLMajorLengthF"        : y_major_length
    "tmYLMajorOutwardLengthF" : y_major_length
    "tmYLMinorLengthF"        : y_minor_length
    "tmYLMinorOutwardLengthF" : y_minor_length
    "tmYRMajorLengthF"        : y_major_length
    "tmYRMajorOutwardLengthF" : y_major_length
    "tmYRMinorLengthF"        : y_minor_length
    "tmYRMinorOutwardLengthF" : y_minor_length
  end setvalues

  tmres = res
  tmres = True
  set_attr(tmres,"tmXBMajorLengthF"        , x_major_length)
  set_attr(tmres,"tmXBMajorOutwardLengthF" , x_major_length)
  set_attr(tmres,"tmXBMinorLengthF"        , x_minor_length)
  set_attr(tmres,"tmXBMinorOutwardLengthF" , x_minor_length)
;              font_height:float                                        ;
  set_attr(tmres,"tmXTMinorLengthF"        , x_minor_length)
  set_attr(tmres,"tmXTMinorOutwardLengthF" , x_minor_length)
  set_attr(tmres,"tmYLMajorLengthF"        , y_major_length)
  set_attr(tmres,"tmYLMajorOutwardLengthF" , y_major_length)

procedure gsnp_uniform_tickmark_labels(plot:graphic,font_height:float)
local xbfont, ylfont
  set_attr(tmres,"tmYRMajorOutwardLengthF" , y_major_length)
  set_attr(tmres,"tmYRMinorLengthF"        , y_minor_length)
  set_attr(tmres,"tmYRMinorOutwardLengthF" , y_minor_length)

  attsetvalues_check(plot,tmres)

  return
end

;***********************************************************************;
; Procedure : gsnp_uniform_tickmark_labels                              ;
;              plot:object                                              ;
;               res:logical                                             ;
;              font_height                                              ;
  setvalues plot
    "tmXBLabelFontHeightF" : font_height
    "tmYLLabelFontHeightF" : font_height
    "tmXTLabelFontHeightF" : font_height
    "tmYRLabelFontHeightF" : font_height
  end setvalues
; Get tickmark labels sizes

  if(font_height.le.0)
    getvalues plot
      "tmXBLabelFontHeightF" : xbfont
      "tmYLLabelFontHeightF" : ylfont
    end getvalues
    font_height = min((/xbfont,ylfont/))
  end if

; Make tickmark label sizes the same.


  tmres = res
  tmres = True

  set_attr(tmres,"tmXBLabelFontHeightF" , font_height)
  set_attr(tmres,"tmYLLabelFontHeightF" , font_height)
  set_attr(tmres,"tmXTLabelFontHeightF" , font_height)
  set_attr(tmres,"tmYRLabelFontHeightF" , font_height)

  attsetvalues_check(plot,tmres)

  return
end

;***********************************************************************;
; Procedure : gsnp_shape_plot                                           ;
;              plot:graphic                                             ;
;                                                                       ;
; If gsnShape is set to True, then the plot is scaled such that the X   ;
; and Y axes are proportional to each other.                            ;
;***********************************************************************;
procedure gsnp_shape_plot(plot:graphic)
local xf, yf, width, height, trxmin, trxmax, trymin, trymax, xrange, yrange, \
new_xf, new_yf, new_width, new_height
begin
  getvalues plot
    "vpXF"      : xf
    "vpYF"      : yf
    "vpWidthF"  : width
    "vpHeightF" : height
    "trXMinF"   : trxmin
    "trXMaxF"   : trxmax
    "trYMinF"   : trymin
    "trYMaxF"   : trymax
  end getvalues

  xrange = trxmax - trxmin
  yrange = trymax - trymin

  if(xrange.lt.yrange)
    new_width  = width * (xrange/yrange)
    new_height = height
    new_xf     = xf + 0.5*(width-new_width)
    new_yf     = yf
  else
    new_height = height * (yrange/xrange)
    new_width  = width

    new_yf     = yf - 0.5*(height-new_height)
    new_xf     = xf
  end if

  setvalues plot
    "vpXF"      : new_xf
    "vpYF"      : new_yf
    "vpWidthF"  : new_width
    "vpHeightF" : new_height
  end setvalues

  return
end

;***********************************************************************;
; Procedure : gsnp_scale_plot                                           ;
;              plot:graphic                                             ;
;                                                                       ;
; If gsnScale is set to True, then the plot is scaled such the tickmarks;
; and tickmark labels are the same size on both axes.                   ;
;***********************************************************************;
procedure gsnp_scale_plot(plot:graphic)
local xfont, yfont, xbfont, xlength, xmlength, ylfont, ylength, ymlength
begin
  end if	
    "tiXAxisFontHeightF"   : xfont
    "tiYAxisFontHeightF"   : yfont
    "tmXBLabelFontHeightF" : xbfont
    "tmXBMajorLengthF"     : xlength
    "tmXBMinorLengthF"     : xmlength
    "tmYLLabelFontHeightF" : ylfont
    "tmYLMajorLengthF"     : ylength
    "tmYLMinorLengthF"     : ymlength
  end getvalues

  if(xlength.ne.0..and.ylength.ne.0.)
    major_length = (ylength+xlength)/2. 
    xlength = major_length
    ylength = major_length
  end if

  if(xmlength.ne.0..and.ymlength.ne.0.)
    minor_length = (ymlength+xmlength)/2. 
    xmlength = minor_length
    ymlength = minor_length
  end if


  setvalues plot
    "tiXAxisFontHeightF"   : (xfont+yfont)/2.
    "tiYAxisFontHeightF"   : (xfont+yfont)/2.
    "tmXBLabelFontHeightF" : (xbfont+ylfont)/2.
    "tmXBMajorLengthF"     : xlength
    "tmXBMinorLengthF"     : xmlength
  ticks_on = get_special_res_value(res,"gsnTickMarksOn",True)
    "tmYLMajorLengthF"     : ylength
    "tmYLMinorLengthF"     : ymlength
  end setvalues
end


;
; Return device coordinates and the orientation.
;
  coords = tointeger((/lx,ly,ux,uy/))
  coords@gsnPaperOrientation = lc_orient
;
;
  if(is_debug)
   print("-------Device coordinates for PostScript-------")
   print("    wkDeviceLowerX = " + coords(0))
   print("    wkDeviceLowerY = " + coords(1))
   print("    wkOrientation  = " + coords@gsnPaperOrientation)

  end if
                         callframe:logical)
  return(coords)
      setvalues wks
        "wkOrientation"  : coords@gsnPaperOrientation
        "wkDeviceLowerX" : coords(0)
        "wkDeviceLowerY" : coords(1)
        "wkDeviceUpperX" : coords(2)
        "wkDeviceUpperY" : coords(3)
      end setvalues
  return
    else
;
; If the workstation is NCGM or X11, then maximize the bounding box.
  end if
;
; Indicate here whether the panel resources have been set.
;
  if(isatt(res,"gsnPanelLeft"))
    maxbb@gsnPanelLeft   = get_res_value(res,"gsnPanelLeft",0.)
  end if

  if(isatt(res,"gsnPanelRight"))
    maxbb@gsnPanelRight  = get_res_value(res,"gsnPanelRight",1.)
  end if
    if(isatt(name,"res_file"))
      res_file = name@res_file
    else
      res_file = "gsnapp"
    end if
    maxbb@gsnPanelBottom = get_res_value(res,"gsnPanelBottom",0.)
  end if
  if(isatt(res,"gsnPanelTop"))
    maxbb@gsnPanelTop    = get_res_value(res,"gsnPanelTop",1.)
  end if

  return(maxbb)
end

;***********************************************************************;
; Function : gsn_open_ncgm                                              ;
;               name : name of output cgm file                          ;
;                                                                       ;
; This function opens an NCGM output file called "<name>.ncgm" and      ;
; returns the workstation id. If "name" is an empty string, then the    ;
; NCGM is given its default name "gmeta".                               ;
;***********************************************************************;
function gsn_open_ncgm(name[1]:string)
local ncgm, res_file
begin
    res_file=get_res_value_keep(name,"res_file","gsnapp")

    if(isatt(name,"wkColorMap"))

      ncgm = create res_file ncgmWorkstationClass defaultapp 
        "wkMetaName" : name
        "wkColorMap" : name@wkColorMap
      end create
    else
      ncgm = create res_file ncgmWorkstationClass defaultapp 
        "wkMetaName" : name
      end create
    end if


    return(ncgm)
end

;***********************************************************************;
; Function : gsn_open_x11                                               ;
;                                                                       ;
    "vpXF"              : vpxf
    "vpYF"              : vpyf
    "vpWidthF"          : vpwidthf
    "vpHeightF"         : vpheightf
    "lbBoxCount"        : nbox
    "lbFillColors"      : colors
    "lbLabelStrings"    : labels

    "lbOrientation"     : orientation
    "lbPerimOn"         : perim_on
    "lbLabelAlignment"  : label_align
    lower_x  =  36
    lower_y  = 126
    upper_x  = 576
    upper_y  = 666
    cmap     = "default"
    orient   = "portrait"
    resltn   = 1800
    res_file = "gsnapp"
    
    if(isatt(name,"res_file"))
      res_file = name@res_file
    end if

    if(isatt(type,"wkPSResolution"))
      resltn = type@wkPSResolution
    end if

    if(isatt(type,"wkOrientation"))
      orient = type@wkOrientation
    end if

    if(isatt(type,"wkDeviceLowerX"))
      lower_x = type@wkDeviceLowerX
    end if

    if(isatt(type,"wkDeviceLowerY"))
      lower_y = type@wkDeviceLowerY
    end if

    if(isatt(type,"wkDeviceUpperX"))
      upper_x = type@wkDeviceUpperX
    end if

    if(isatt(type,"wkDeviceUpperY"))
      upper_y = type@wkDeviceUpperY
    end if
end
    if(isatt(type,"wkColorMap"))
      delete(cmap)
      cmap = type@wkColorMap
    end if


;***********************************************************************;
; Function : gsn_open_ps                                                ;
;               name : name of PostScript file                          ;
;                                                                       ;
; This function opens a PostScript file called "<name>.ps" and returns  ;
; the workstation id. If "name" is an empty string, then the PostScript ;
; file is called "gmeta.ps".                                            ;
;***********************************************************************;
function gsn_open_ps(type:string,name[1]:string)
local ps, res_file
begin
    res_file = get_res_value_keep(name,"res_file","gsnapp")
    resltn   = get_res_value_keep(type,"wkPSResolution",1800)
    orient   = get_res_value_keep(type,"wkOrientation","portrait")
    lower_x  = get_res_value_keep(type,"wkDeviceLowerX",36)
    lower_y  = get_res_value_keep(type,"wkDeviceLowerY",126)
    upper_x  = get_res_value_keep(type,"wkDeviceUpperX",576)
    upper_y  = get_res_value_keep(type,"wkDeviceUpperY",666)
    cmap     = get_res_value_keep(type,"wkColorMap","default")

    ps = create res_file psWorkstationClass defaultapp
      "wkColorMap"     : cmap
      "wkOrientation"  : orient
      "wkPSResolution" : resltn
      "wkPSFileName"   : name
      "wkPSFormat"     : type
      "wkDeviceLowerX" : lower_x

      "wkDeviceLowerY" : lower_y
      "wkDeviceUpperX" : upper_x
      "wkDeviceUpperY" : upper_y
    end create


;***********************************************************************;
; Function : gsn_open_wks                                               ;
;               type : type of workstation to open                      ;
;               name : name of workstation                              ;
;                                                                       ;
; This function opens either an X11 window, an NCGM file, or a          ;
; PostScript file depending on "type", which can be "x11", "ncgm", or   ;
; "ps". If "type" is a PS file or an NCGM, then it will be named        ;
; <name>.ps or <name>.ncgm respectively. This function also looks for a ;
; resource file called "name.res". If it exists, then it loads the      ;
; resources defined in that file. This function returns the workstation ;
; id.                                                                   ;
;***********************************************************************;
function gsn_open_wks(type[1]:string,name[1]:string)
local i, wks, appusrdir, name_char, not_found, res_file, res_dir
begin
    res_dir = "./"          ; Default resource directory.
    res_file = "gsnapp"     ; Default resource file name.
;
; Parse "name" to get the directory and the file prefix.
    if(isatt(type,"wkMetaName"))
        ncgm_file = type@wkMetaName
    else
        ncgm_file = res_file + ".ncgm"
    end if
    if(name.ne."") then
      name_char = stringtochar(name)
    if(isatt(type,"wkPSFileName"))
        ps_file = type@wkPSFileName
    else
        ps_file = res_file + "." + type
    end if

      i = name_len-1      ; Start checking if a directory pathname
      not_found = True    ; was specified for the resource file.
      do while(not_found.and.i.ge.0)
        if(name_char(i).eq."/")
          res_dir  = chartostring(name_char(0:i))
          not_found = False
        end if
        i = i - 1
      end do

      res_file = chartostring(name_char(i+1:name_len-1))

      if(isatt(name,"appUsrDir").and.not_found)
        res_dir = name@appUsrDir   ; No directory specified.
      end if
    end if

    ncgm_file = get_res_value_keep(type,"wkMetaName",res_file + ".ncgm")
    ncgm_file@res_file = res_file

    ps_file = get_res_value_keep(type,"wkPSFileName",res_file + "." + type)
    ps_file@res_file = res_file

            wks = gsn_open_ps(type,ps_file)

;
                wks = gsn_open_ncgm(ncgm_file)
; passed automatically through "type".
;
    if(isatt(type,"wkColorMap"))
      ncgm_file@wkColorMap = type@wkColorMap
      x_file@wkColorMap    = type@wkColorMap
    end if

    appid = create res_file appClass defaultapp
        "appDefaultParent" : True
        "appUsrDir"        : res_dir
    end create

; Procedure : gsn_panel                                                 ;
;                 wks: workstation object                               ;
;               plot : array of plots to put on one page.               ;
;               dims : a 2-D array indicating number of rows and columns;
;             resources: optional resources                             ;
;                                                                       ;
; This procedure takes the array of plots and draws them all on one     ;
; workstation in the configuration specified by dims.                   ;
;                                                                       ;
; For example, if you have six plots and dims is (/2,3/), then the six  ;
; plots will be drawn in 2 rows and 3 columns.                          ;
        else
    end if
procedure gsn_panel(wks:graphic,plot[*]:graphic,dims[2]:float,\
                    resources:logical )
local rows, cols, xrows, xcols, top, bottom, left, right, \
vpx, vpy, vpwidth, vpheight, bb, plot_width, plot_height, \
xwsp_perc, ywsp_perc, xwsp, ywsp, dx, dy, total_width, total_height, \
col_scale, row_scale, scale, xpos, ypos, xsp, ysp, \
new_plot_height, new_plot_width, new_total_height, new_total_width, \
i, j, old_vpx, old_vpy, old_vpwidth, old_vpheight, callframe

    rows = floattointeger(dims(0))
    cols = floattointeger(dims(1))
    xrows = dims(0)
    xcols = dims(1)
    xwsp_perc = 1.0   ; White space is 1% of total length of plot.
    ywsp_perc = 1.0   ; This can be changed with an attribute.
    x_lft = 0.0
    y_top = 1.0
    x_rgt = 1.0
    y_bot = 0.0
    callframe = True
;***********************************************************************;
    if(resources)
      if(isatt(resources,"gsnFrame").and..not.resources@gsnFrame)
          callframe = False
      end if
      if(isatt(resources,"gsnPanelXWhiteSpacePercent"))
        xwsp_perc = resources@gsnPanelXWhiteSpacePercent
        if(xwsp_perc.lt.0.or.xwsp_perc.ge.100.)
          print("Warning: gsn_panel: attribute gsnPanelXWhiteSpacePercent must be >= 0 and < 100.")
          print("Defaulting to 1.")
          xwsp_perc = 1.
        end if
      end if
      if(isatt(resources,"gsnPanelYWhiteSpacePercent"))
        ywsp_perc = resources@gsnPanelYWhiteSpacePercent
        if(ywsp_perc.lt.0.or.ywsp_perc.ge.100.)
          print("Warning: gsn_panel: attribute gsnPanelYWhiteSpacePercent must be >= 0 and < 100.")
          print("Defaulting to 1.")
          ywsp_perc = 1.
        end if
      end if
;               resources: optional resources                           ;
      if(isatt(resources,"gsnPanelLeft"))
        x_lft = resources@gsnPanelLeft
        if(x_lft.lt.0..or.x_lft.ge.1.)
          print("Warning: gsn_panel: attribute gsnPanelLeft must be >= 0.0 and < 1.0")
          print("Defaulting to 0.")
          x_lft = 0.0
        end if
      end if
; variable returned from a previous call to "gsn_open_wks") in the same ;
      if(isatt(resources,"gsnPanelRight"))
        x_rgt = resources@gsnPanelRight
        if(x_rgt.le.0..or.x_rgt.gt.1.)
          print("Warning: gsn_panel: attribute gsnPanelRight must be > 0.0 and <= 1.0")
          print("Defaulting to 1.")
          x_rgt = 1.0
        end if
      end if
; one of the gsn_* plotting functions). "x" and "y" are the x and y     ;
      if(isatt(resources,"gsnPanelTop"))
        y_top = resources@gsnPanelTop
        if(y_top.le.0..or.y_top.gt.1.)
          print("Warning: gsn_panel: attribute gsnPanelTop must be > 0.0 and <= 1.0")
          print("Defaulting to 1.")
          y_top = 1.0
        end if
      end if

      if(isatt(resources,"gsnPanelBottom"))
        y_bot = resources@gsnPanelBottom
        if(y_bot.lt.0..or.y_bot.ge.1.)
          print("Warning: gsn_panel: attribute gsnPanelBottom must be >= 0.0 and < 1.0")
          print("Defaulting to 0.")
          y_bot = 0.0
        end if
      end if

      if(x_rgt.le.x_lft)
        print("Error: gsn_panel: attribute gsnPanelRight ("+x_rgt+") must be greater")
        print("than gsnPanelLeft ("+x_lft+").")
        exit
      end if

      if(y_top.le.y_bot)
        print("Error: gsn_panel: attribute gsnPanelTop ("+y_top+") must be greater")
        print("than gsnPanelBottom ("+y_bot+").")
        exit
      end if
begin
    end if
end
;
; Make sure we have enough panels to fit all of the plots.
;
    nplots  = dimsizes(plot)      ; Total number of plots.
    npanels = rows*cols           ; Total number of panels.
    if(nplots.gt.npanels)
      print("Warning: gsn_panel: you have more plots than you have panels.")
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
;***********************************************************************;
; Procedure : gsn_polygon_ndc                                           ;
    bb = NhlGetBB(plot(0))   ; Get bounding box of plot with
    top    = bb(0)           ; all of its annotations.
    bottom = bb(1)
    left   = bb(2)
    right  = bb(3)

;                   wks: workstation object                             ;
; plot_width  : total width of plot with all of its annotations
; total_width : plot_width plus white space on both sides
;               resources: optional resources                           ;
    plot_width  = right - left     ; Calculate total width of plot.
    plot_height = top - bottom     ; Calculate total height of plot.
procedure gsn_polygon_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
    xwsp = xwsp_perc/100. * plot_width  ; White space is a percentage of total
    ywsp = ywsp_perc/100. * plot_height ; width and height.
begin
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
; Retrieve graphic style object.
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

  ywsp = ywsp_perc/100. * new_plot_height
; Loop through each plot and draw it in the new scaled-down size.
    end if
    do i = 0,rows-1
;
; Before we add plots to a row, check to see if we have enough plots
; left to fill that row.  If not, then center the remaining plots.
;
      num_plots_left = dimsizes(plot) - i*cols

      if(num_plots_left.lt.cols)
        num_miss_plots = cols - num_plots_left
        if(num_miss_plots.gt.0.and.num_miss_plots.lt.cols)
          delete(xpos)
          xsp = xrange - new_total_width*(cols-num_miss_plots)
          xpos = x_lft + xwsp + dx +(xsp/2.+new_total_width*\
                 ispan(0,cols-num_miss_plots-1,1))
        end if
      end if
      do j = 0,cols-1
        if(i*cols+j .lt. dimsizes(plot))
          if(.not.ismissing(plot(i*cols+j)))
            getvalues plot(i*cols+j)
              "vpXF"      : old_vpx
              "vpYF"      : old_vpy
              "vpWidthF"  : old_vpwidth
              "vpHeightF" : old_vpheight
            end getvalues

            setvalues plot(i*cols+j)
              "vpXF"      : xpos(j)
              "vpYF"      : ypos(i)
              "vpWidthF"  : scale*old_vpwidth 
              "vpHeightF" : scale*old_vpheight
            end setvalues
    new_ncols = min((/num_plots_left,row_spec(nr)/))
            draw(plot(i*cols+j))  ; Draw the scaled down plot.

            setvalues plot(i*cols+j)
              "vpXF"      : old_vpx
              "vpYF"      : old_vpy
              "vpWidthF"  : old_vpwidth
              "vpHeightF" : old_vpheight
            end setvalues
          end if
        end if
      end do  ; end of columns
    end do    ; end of rows

    if(callframe)
      frame(wks)   ; Advance the frame.
    end if
end

procedure draw_bb(wks:graphic,plot:graphic)
;
; Color names start in the 14th column of the rgb.txt file.
;                  cmap: Colormap (n x 3 array)                         ;
  names = chartostring(rgb_text_char(:,13:))

  ncolors = dimsizes(named_colors)
; RGB values.                                                           ;


  do i=0,ncolors-1
    index = ind(named_colors(i).eq.names)
    if(.not.ismissing(index))
  if((typeof(cmap).eq."float".and.(dimsizes(dim_cmap).ne.2.or.dim_cmap(1).ne.3)).or.\
      rgb_values(i,2) = blue(index)
    print("Warning: gsn_define_colormap: cmap must either be a 2-dimensional float array")
    print("dimensioned n x 3 or a 1-dimensional string array.")
  return(rgb_values)
end


;***********************************************************************;
; Procedure : gsn_define_colormap                                       ;
;                   wks: workstation object                             ;
;                  cmap: Colormap                                       ;
;                                                                       ;
; This procedure defines a color map for workstation "wks" (the         ;
; variable returned from a previous call to "gsn_open_wks") using float ;
; RGB values or a pre-defined color name.                               ;
;***********************************************************************;
procedure gsn_define_colormap(wks:graphic, cmap)
begin
  dim_cmap = dimsizes(cmap)

  if((typeof(cmap).eq."float".and.(dimsizes(dim_cmap).ne.2.or.\
                                   dim_cmap(1).ne.3)).or.\
     (typeof(cmap).eq."string".and.dimsizes(dim_cmap).ne.1))
    print("Warning: gsn_define_colormap: cmap must either be an n x 3 float array,")
    print("a single pre-defined colormap name, or a 1-dimensional string array of named colors.")
  else
    setvalues wks
        "wkColorMap" : cmap
    end setvalues
  end if
  tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
  gsnp_point_tickmarks_outward(xy,tmres,-1.,-1.,-1.,-1.,-1.,-1.)
  gsnp_uniform_tickmark_labels(xy,tmres,0.)

  gsres = get_res_eq(res2,"gs")
  histogram = hist_columns(wks,xy,locs,buck,width,colors,gsres)
  draw_and_frame(wks,histogram,calldraw,callframe,maxbb)

; Return histogram.
;***********************************************************************;

; Function : gsn_contour                                                ;
local i, attnames, data_object, plot_object, res, sf_res_index, \
;                  data: 2-dimensional data                             ;
;             resources: optional resources                             ;
;                                                                       ;
; This function creates and draws a contour plot to the workstation     ;
; "wks" (the variable returned from a previous call to "gsn_open_wks"). ;
; "data" is the 2-dimensional data to be contoured, and "resources" is  ;
; an optional list of resources. The id of the contour plot is returned.;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors                                                     ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
;   gsnSpreadColorEnd                                                   ;
;***********************************************************************;
function gsn_contour(wks:graphic, data[*][*]:numeric, resources:logical )
local i, data_object, plot_object, res, sf_res_index, \
begin
    cnres     = False
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False

; Create plot object.
    data_object = create wksname + "_data" scalarFieldClass noparent
        "sfDataArray" : data
    sfres = get_res_eq(res2,"sf")
    attsetvalues_check(data_object,sfres)

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues data_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
    sprdcols  = get_special_res_value(res2,"gsnSpreadColors",False)
    min_index = get_special_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_special_res_value(res2,"gsnSpreadColorEnd",-1)
    plot_object = create wksname + "_contour" contourPlotClass wks
        "trXTensionF"       : xtension
        "trYTensionF"       : ytension
        "cnScalarFieldData" : data_object
    end create
    sfres = get_res_eq(res2,"sf")

; it exists.
    if(sprdcols)
      set_attr(cnres,"cnFillColors",\
               spread_colors(wks,plot_object,min_index,max_index))
    end if

    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(data_object,sfres)
    end if
    if(cnres.and..not.any(ismissing(getvaratts(cnres))))
      attsetvalues(plot_object,cnres)
    end if
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    cnres = get_res_ne(res2,"sf")
    attsetvalues_check(plot_object,cnres)

    if(sprdcols)
      cnres2 = True
      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,plot_object,min_index,max_index))
      attsetvalues(plot_object,cnres2)
    end if

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
        llres = get_res_eq(res2,(/"tr","vp"/))
    end if
;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if
;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

; Check if we need to force the X or Y axis to be linear or log.
      if(llres.and..not.any(ismissing(getvaratts(llres))))
        attsetvalues(plot_object,llres)
      end if

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

    draw_and_frame(wks,plot_object,calldraw,callframe)
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues

      plot_object = create wksname + "_loglin" logLinPlotClass wks
        "trXLog"  : force_x_log
        "trYLog"  : force_y_log
        "trXMinF" : trxmin
        "trXMaxF" : trxmax
        "trYMinF" : trymin
        "trYMaxF" : trymax
      end create
      attsetvalues_check(plot_object,llres)

      overlay(plot_object,overlay_plot_object)
      plot_object@contour = overlay_plot_object
; Function : gsn_contour_map                                            ;

;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
local i, attnames, data_object, contour_object, res, sf_res_index, \
;                                                                       ;
; This function creates and draws a contour plot over a map plot to the ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "data" is the 2-dimensional data to be contoured,   ;
; and "resources" is an optional list of resources. The id of the map   ;
; plot is returned.                                                     ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors                                                     ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
function gsn_contour_map(wks:graphic,data[*][*]:numeric,\
                         resources:logical)
; Create plot object.
    data_object = create wksname + "_data" scalarFieldClass noparent
        "sfDataArray" : data
    end create

    if(isatt(data,"_FillValue")) then
        setvalues data_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

    sfres = get_res_eq(res2,"sf")
    attsetvalues_check(data_object,sfres)
;
; Create plot object. Make sure you set the tension values (if
; any) when you create the plot. This works better than setting
; them later.
;
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    scale     = get_special_res_value(res2,"gsnScale",False)
    shape     = get_special_res_value(res2,"gsnShape",scale)
    sprdcols  = get_special_res_value(res2,"gsnSpreadColors",False)
    min_index = get_special_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_special_res_value(res2,"gsnSpreadColorEnd",-1)

    sfres = get_res_eq(res2,"sf")
; Check for existence of data@long_name and use it in a title it
; it exists.

    if(sprdcols)
      set_attr(cnres,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))
    end if

    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(data_object,sfres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    if(cnres.and..not.any(ismissing(getvaratts(cnres))))
      attsetvalues(contour_object,cnres)
    end if

    overlay(map_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(contour_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@contour = contour_object
    return(map_object)
end

;***********************************************************************;
; Procedure : gsn_labelbar_ndc                                          ;
;                   wks: workstation object                             ;
;                  nbox: number of labelbar boxes                       ;
;                labels: labels for boxes                               ;
;                     x: X NDC position of labelbar                     ;
;                     y: Y NDC position of labelbar                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a labelbar on the workstation "wks" (the         ;
; variable returned from a previous call to "gsn_open_wks").            ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;

procedure gsn_labelbar_ndc(wks:graphic, nbox:integer, labels:string, \
                           x:float,y:float,resources:logical )
local i, lbid, attnames, res, lb_res_index, lbres
begin

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    lbid = create wksname + "_labelbar" labelBarClass wks
      "vpXF"           : x
      "vpYF"           : y
      "lbBoxCount"     : nbox
      "lbLabelStrings" : labels
    end create

    lbres = get_res_eq(resources,(/"lb","vp"/))
    if(lbres.and..not.any(ismissing(getvaratts(lbres))))

; A special test is needed for the resource lbLabelFontHeightF.
; If it is set, then we need to turn off lbAutoManage.

      if(isatt(lbres,"lbLabelFontHeightF"))
        setvalues lbid
          "lbAutoManage"       : False
          "lbLabelJust"        : "CenterCenter"
          "lbLabelFontHeightF" : lbres@lbLabelFontHeightF
        end setvalues
        delete(lbres@lbLabelFontHeightF)
      end if
      if(lbres.and..not.any(ismissing(getvaratts(lbres))))
        attsetvalues(lbid,lbres)
      end if
    end if

; Draw labelbar.

    draw(lbid)
    delete(lbid)
end

;***********************************************************************;
; Function : gsn_map                                                    ;
;                      wks: workstation object                          ;
;               projection: Map projection                              ;
;                  resources: optional resources                        ;
;                                                                       ;
; This function creates and draws a map plot to the workstation "wks"   ;
; (the variable returned from a previous call to "gsn_open_wks").       ;
; "projection" is one of the ten supported map projections, and         ;
; "resources" is an optional list of resources. The id of the map plot  ;
; is returned.                                                          ;
;***********************************************************************;

function gsn_map(wks:graphic, projection:string, resources:logical )
local i, attnames, plot_object, res2
begin
    res2      = resources

; Create plot object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    plot_object = create wksname + "_map" mapPlotClass wks
        "mpProjection" : projection
    end create

; Check to see if any resources were set.

    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)

    if(res2.and..not.any(ismissing(getvaratts(res2))))
      attsetvalues(plot_object,res2)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe)

; Return plot object.

    return(plot_object)
end

;***********************************************************************;
; Procedure : gsn_polygon                                               ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a filled polygon on the workstation "wks" (the   ;
; variable returned from a previous call to "gsn_open_wks") in the same ;
; data space as the data in "plotid" (returned from a previous call to  ;
; one of the gsn_* plotting functions). "x" and "y" are the x and y     ;
; locations of each point in the polygon, and should be in the same data;
; space as the data from "plotid". "resources" is an optional list of   ;
; resources.                                                            ;
;***********************************************************************;

procedure gsn_polygon(wks:graphic,plotid:graphic,x[*]:numeric,\
                      y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index, res2
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Draw a polygon.

    if(.not.any(ismissing(x)).and..not.any(ismissing(y)))
      NhlDataPolygon(plotid,gsid,x,y)
    else
      x2 = x(ind(.not.ismissing(x).and..not.ismissing(y)))
      y2 = y(ind(.not.ismissing(x).and..not.ismissing(y)))
      NhlDataPolygon(plotid,gsid,x2,y2)
    end if
end


;***********************************************************************;
; Procedure : gsn_polygon_ndc                                           ;
;                   wks: workstation object                             ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a filled polygon on the workstation "wks" (the   ;
; variable returned from a previous call to "gsn_open_wks") in NDC      ;
; space. "x" and "y" are the x and y locations of each point in the     ;
; polygon, and "resources" is an optional list of resources.            ;
;***********************************************************************;

procedure gsn_polygon_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                          resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

;
; Create a LogLinPlot that covers the entire NDC space
; to use as a drawing canvas
;
    canvas = create "canvas" logLinPlotClass wks
      "vpXF"      : 0.0
      "vpYF"      : 1.0
      "vpWidthF"  : 1.0
      "vpHeightF" : 1.0
    end create

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Draw a polygon.

    if(.not.any(ismissing(x)).and..not.any(ismissing(y)))
      NhlNDCPolygon(canvas,gsid,x,y)
    else
      x2 = x(ind(.not.ismissing(x).and..not.ismissing(y)))
      y2 = y(ind(.not.ismissing(x).and..not.ismissing(y)))
      NhlNDCPolygon(canvas,gsid,x2,y2)
    end if
end

;***********************************************************************;
; Procedure : gsn_polyline                                              ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a polyline on the workstation "wks" (the variable;
; returned from a previous call to "gsn_open_wks") in the same data     ;
; space as the data in "plotid" (returned from a previous call to one of;
; the gsn_* plotting functions). "x" and "y" are the x and y locations  ;
; of each point in the line, and should be in the same data space as the;
; data from "plotid". "resources" is an optional list of resources.     ;
;***********************************************************************;

procedure gsn_polyline(wks:graphic,plotid:graphic,x[*]:numeric,\
                       y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index
begin

; Retrieve graphic style object.
    callframe = get_res_value(res2,"gsnFrame",True)
    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues
    mpres = get_res_eq(res2,(/"mp","vp"/))
    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Draw a polyline.
    attsetvalues_check(contour_object,cnres)
    if(.not.any(ismissing(x)).and..not.any(ismissing(y)))
      NhlDataPolyline(plotid,gsid,x,y)
    else
;
; Since the data contains missing values, leave gaps where the missing
; values are located.
;
      x2 = new(dimsizes(x),float)
      y2 = new(dimsizes(x),float)
      j = 0
      do i = 0,dimsizes(x)-1
        if(ismissing(x(i)).or.ismissing(y(i)))
          if(j.eq.1)    ; only one value, so draw a marker
            NhlDataPolymarker(plotid,gsid,x2(0),y2(0))
          end if
          if(j.gt.1)    ; multiple values, so draw a line
            NhlDataPolyline(plotid,gsid,x2(0:j-1),y2(0:j-1))
          end if
          j = 0
        else
          x2(j) = x(i)   ; Neither point is missing, so store in
          y2(j) = y(i)   ; separate arrays to draw later.
          j = j + 1
        end if
      end do
      if(j.eq.1)         
        NhlDataPolymarker(plotid,gsid,x2(0),y2(0))
        j = 0
      end if
      if(j.gt.1)         
        NhlDataPolyline(plotid,gsid,x2(0:j-1),y2(0:j-1))
        j = 0
      end if
    end if
end

;***********************************************************************;
; Procedure : gsn_polyline_ndc                                          ;
;                   wks: workstation object                             ;
;                     x: 1-dimensional array of x ndc points            ;
;                     y: 1-dimensional array of y ndc points            ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a polyline on the workstation "wks" (the variable;
; returned from a previous call to "gsn_open_wks") in NDC space.        ;
; "x" and "y" are the x and y locations of each point in the line.      ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;

procedure gsn_polyline_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                           resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues
;
; Create a LogLinPlot that covers the entire NDC space
; to use as a drawing canvas
;
    canvas = create "canvas" logLinPlotClass wks
      "vpXF"      : 0.0
      "vpYF"      : 1.0
      "vpWidthF"  : 1.0
      "vpHeightF" : 1.0
    end create

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Draw a polyline.

    if(.not.any(ismissing(x)).and..not.any(ismissing(y)))
      NhlNDCPolyline(canvas,gsid,x,y)
    else
;
; Since the data contains missing values, leave gaps where the missing
; values are located.
;
      x2 = new(dimsizes(x),float)
      y2 = new(dimsizes(x),float)
      j = 0
      do i = 0,dimsizes(x)-1
        if(ismissing(x(i)).or.ismissing(y(i)))
          if(j.eq.1)    ; only one value, so draw a marker
            NhlNDCPolymarker(canvas,gsid,x2(0),y2(0))
          end if
          if(j.gt.1)    ; multiple values, so draw a line
            NhlNDCPolyline(canvas,gsid,x2(0:j-1),y2(0:j-1))
          end if
          j = 0
        else
          x2(j) = x(i)   ; Neither point is missing, so store in
          y2(j) = y(i)   ; separate arrays to draw later.
          j = j + 1
        end if
      end do
      if(j.eq.1)         
        NhlNDCPolymarker(canvas,gsid,x2(0),y2(0))
        j = 0
      end if
      if(j.gt.1)         
        NhlNDCPolyline(canvas,gsid,x2(0:j-1),y2(0:j-1))
        j = 0
      end if
    end if
               spread_colors(wks,contour_object,min_index,max_index))
      attsetvalues(contour_object,cnres2)
    end if
; Procedure : gsn_polymarker                                            ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
; If gsnScale was set to True, then make sure the X and Y axis labels
; This procedure draws polymarkers on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks") in the same ;
; data space as the data in "plotid" (returned from a previous call to  ;
; one of the gsn_* plotting functions). "x" and "y" are the x and y     ;
; locations of each marker, and should be in the same data space as the ;
; data from "plotid". "resources" is an optional list of resources.     ;
    map_object@contour = contour_object

procedure gsn_polymarker(wks:graphic,plotid:graphic,x[*]:numeric,\
                         y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index

; Function : gsn_map                                                    ;
; Retrieve graphic style object.
;               projection: Map projection                              ;
    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Draw some polymarkers.

    if(.not.any(ismissing(x)).and..not.any(ismissing(y)))
      NhlDataPolymarker(plotid,gsid,x,y)
    else
      x2 = x(ind(.not.ismissing(x).and..not.ismissing(y)))
      y2 = y(ind(.not.ismissing(x).and..not.ismissing(y)))
      NhlDataPolymarker(plotid,gsid,x2,y2)
    end if
end
;                                                                       ;
;***********************************************************************;
; Procedure : gsn_polymarker_ndc                                        ;
;                   wks: workstation object                             ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws polymarkers on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks") in NDC      ;
; space. "x" and "y" are the x and y locations of each marker in NDC    ;
; coordinates. "resources" is an optional list of resources.            ;
;***********************************************************************;
; "resources" is an optional list of resources. The id of the map plot  ;
procedure gsn_polymarker_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                         resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index
begin
;                                                                       ;
; Retrieve graphic style object.
;   gsnFrame                                                            ;
    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues
;***********************************************************************;
;
; Create a LogLinPlot that covers the entire NDC space
; to use as a drawing canvas
;
    canvas = create "canvas" logLinPlotClass wks
      "vpXF"      : 0.0
      "vpYF"      : 1.0
      "vpWidthF"  : 1.0
      "vpHeightF" : 1.0
    end create
local i, plot_object, res2
    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if
    res2      = resources
; Draw some polymarkers.

    if(.not.any(ismissing(x)).and..not.any(ismissing(y)))
      NhlNDCPolymarker(canvas,gsid,x,y)
    else
      x2 = x(ind(.not.ismissing(x).and..not.ismissing(y)))
      y2 = y(ind(.not.ismissing(x).and..not.ismissing(y)))
      NhlNDCPolymarker(canvas,gsid,x2,y2)
    end if
; Create plot object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    plot_object = create wksname + "_map" mapPlotClass wks
        "mpProjection" : projection
    end create

; Check to see if any resources were set.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)



;***********************************************************************;
; Function : gsn_streamline                                             ;
local i,attnames,data_object,plot_object,res,vf_res_index,st_res_index, \
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot to the workstation  ;
; "wks" (the variable returned from a previous call to "gsn_open_wks"). ;
; "u" and "v" are the 2-dimensional arrays to be streamlined, and       ;
; "resources" is an optional list of resources. The id of the streamline;
; plot is returned.                                                     ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;                                                                       ;
;***********************************************************************;
function gsn_streamline(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                        resources:logical)
local i, data_object,plot_object,res,vf_res_index,st_res_index, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, res2, scale, shape
begin
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = create wksname + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(data_object,vfres)
    end if
    if(stres.and..not.any(ismissing(getvaratts(stres))))
      attsetvalues(plot_object,stres)
    end if

; Create plot object.

    plot_object = create wksname + "_stream" streamlinePlotClass wks
        "stVectorFieldData" : data_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    vfres = get_res_eq(res2,"vf")
    stres = get_res_ne(res2,"vf")
    attsetvalues_check(data_object,vfres)
    attsetvalues_check(plot_object,stres)

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
        llres = get_res_eq(res2,(/"tr","vp"/))
    end if

;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

; Check if we need to force the X or Y axis to be linear or log.
      if(llres.and..not.any(ismissing(getvaratts(llres))))
        attsetvalues(plot_object,llres)
      end if

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

    draw_and_frame(wks,plot_object,calldraw,callframe)
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues

      plot_object = create wksname + "_loglin" logLinPlotClass wks
        "trXLog"  : force_x_log
        "trYLog"  : force_y_log
        "trXMinF" : trxmin
        "trXMaxF" : trxmax
        "trYMinF" : trymin
        "trYMaxF" : trymax
      end create
      attsetvalues_check(plot_object,llres)

      overlay(plot_object,overlay_plot_object)
      plot_object@contour = overlay_plot_object
    end if


;***********************************************************************;
; Function : gsn_streamline_map                                         ;
local i, attnames, data_object, contour_object, res, vf_res_index, \
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot over a map plot to  ;
; the workstation "wks" (the variable returned from a previous call to  ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
; streamlined, and "resources" is an optional list of resources. The id ;
; of the map plot is returned.                                          ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;                                                                       ;
;***********************************************************************;
function gsn_streamline_map(wks:graphic,u[*][*]:numeric,\
                            v[*][*]:numeric,resources:logical)
local i, data_object, contour_object, res, vf_res_index, \
st_res_index, mp_res_index, map_object, res2
begin
    res2      = resources

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = create wksname + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(data_object,vfres)
    end if
    if(stres.and..not.any(ismissing(getvaratts(stres))))
      attsetvalues(stream_object,stres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
        "stVectorFieldData" : data_object
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    draw_and_frame(wks,map_object,calldraw,callframe)
    scale     = get_res_value(res2,"gsnScale",shape)

    vfres = get_res_eq(res2,"vf")
    mpres = get_res_ne(res2,(/"vf","mp","vp"/))
    stres = get_res_ne(res2,(/"vf","mp","vp"/))

    attsetvalues_check(data_object,vfres)
    attsetvalues_check(stream_object,stres)
    attsetvalues_check(map_object,mpres)
; Procedure : gsn_text                                                  ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                  text: array of text strings                          ;
;                     x: 1-dimensional array of x data positions        ;
;                     y: 1-dimensional array of y data positions        ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws text strings on the workstation "wks" (the       ;
; variable returned from a previous call to "gsn_open_wks").  "x" and   ;
; "y" are the x and y locations of each text string, and should be      ;
; specified in the same data space as the data space of "plotid".       ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;

procedure gsn_text(wks:graphic, plotid:graphic, text:string, x:numeric, \
                   y:numeric, resources:logical )
local i, txid, attnames, plot_object, res, tx_res_index, x2, y2, funccode
begin

    x2 = new(dimsizes(x),float)
    y2 = new(dimsizes(y),float)
    datatondc(plotid,x,y,x2,y2)
;
; The "txFuncCode" can't be set during a setvalues  call. It must be
; set during the creation of the object.  
;
    if((resources).and.isatt(resources,"txFuncCode")) then
      funccode = resources@txFuncCode
    else
      funccode = ":"     ; The default.
    end if

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    txid = create wksname + "_text" textItemClass wks
        "txString"   : text
        "txPosXF"    : x2
        "txPosYF"    : y2
        "txFuncCode" : funccode
    end create

    txres = get_res_eq(resources,"tx")  ; Get text resources.
    if(txres.and..not.any(ismissing(getvaratts(txres))))
      attsetvalues(txid,txres)            ; Set text resources.
    end if
    draw(txid)                          ; Draw text string.
    delete(txid)
end

;***********************************************************************;
; Procedure : gsn_text_ndc                                              ;
;                   wks: workstation object                             ;
;                  text: array of text strings                          ;
;                     x: 1-dimensional array of x ndc positions         ;
;                     y: 1-dimensional array of y ndc positions         ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws text strings on the workstation "wks" (the       ;
; variable returned from a previous call to "gsn_open_wks").  "x" and   ;
; "y" are the x and y locations of each text string, and should be      ;
; specified in NDC space. "resources" is an optional list of resources. ;
;***********************************************************************;

procedure gsn_text_ndc(wks:graphic, text:string, x:numeric, \
                   y:numeric, resources:logical )
local i, txid, attnames, plot_object, res, tx_res_index, x2, y2
begin

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    if((resources).and.isatt(resources,"txFuncCode")) then
      txid = create wksname + "_text_ndc" textItemClass wks
        "txString" : text
        "txPosXF"  : x
        "txPosYF"  : y
        "txFuncCode" : resources@txFuncCode
      end create
    else
      txid = create wksname + "_text_ndc" textItemClass wks
        "txString" : text
        "txPosXF"  : x
        "txPosYF"  : y
      end create
    end if

    txres = get_res_eq(resources,"tx")  ; Get text resources.
    if(txres.and..not.any(ismissing(getvaratts(txres))))
      attsetvalues(txid,txres)            ; Set text resources.
    end if
    draw(txid)                          ; Draw text string.
    delete(txid)
; Outline box in black.
;
        gsn_polyline_ndc(wks,xbox+xpos(jj),ybox+ypos(ii),lineres)
;
; Draw color label.
;
        gsn_text_ndc(wks,i-1,font_space+xpos(jj),ypos(ii)+font_space,txres)
        ii = ii + 1
      end do
      jj = jj +1
    end do
    frame(wks)   ; Advance the frame.
  end do
;
;                   wks: workstation object                             ;

;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
local i,attnames,data_object,plot_object,res,vf_res_index,vc_res_index, \
;                                                                       ;
; This function creates and draws a vector plot to the workstation "wks";
; (the variable returned from a previous call to "gsn_open_wks").  "u"  ;
; and "v" are the 2-dimensional arrays to be vectorized, and "resources";
; is an optional list of resources. The id of the vector plot is        ;
; returned.                                                             ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
function gsn_vector(wks:graphic, u[*][*]:numeric, v[*][*]:numeric, \
                    resources:logical )
local i,data_object,plot_object,res,vf_res_index,vc_res_index, \
force_x_linear, force_y_linear, force_x_log, force_y_log, sprdcols, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, res2
begin
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = create wksname + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
    sprdcols  = get_special_res_value(res2,"gsnSpreadColors",False)
    min_index = get_special_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_special_res_value(res2,"gsnSpreadColorEnd",-1)
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : data_object
    if(sprdcols)
      set_attr(vcres,"vcLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index))
    end if

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(data_object,vfres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(plot_object,vcres)
    end if

    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    vfres = get_res_eq(res2,"vf")
    vcres = get_res_ne(res2,"vf")
    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      llres = get_res_eq(res2,(/"tr","vp"/))
    end if

    attsetvalues_check(data_object,vfres)
    attsetvalues_check(plot_object,vcres)
    if(sprdcols)
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index))
      attsetvalues(plot_object,vcres2)
    end if
;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

; Check if we need to force the X or Y axis to be linear or log.
      if(llres.and..not.any(ismissing(getvaratts(llres))))
        attsetvalues(plot_object,llres)
      end if

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

    draw_and_frame(wks,plot_object,calldraw,callframe)
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues

      plot_object = create wksname + "_loglin" logLinPlotClass wks
        "trXLog"  : force_x_log
        "trYLog"  : force_y_log
        "trXMinF" : trxmin
        "trXMaxF" : trxmax
        "trYMinF" : trymin
        "trYMaxF" : trymax
      end create
      attsetvalues_check(plot_object,llres)
; This function creates and draws vectors and contours over a map plot  ;
; to the workstation "wks" (the variable returned from a previous call  ;
; to "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be   ;
    end if
; "resources" is an optional list of resources. The id of the map plot  ;
; is returned.                                                          ;
;                     v: 2-dimensional V data                           ;

;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
local i, attnames, vfdata_object, sfdata_object, contour_object, res, \
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "data" is the scalar field to be contoured.           ;
; "resources" is an optional list of resources. The id of the vector    ;
; plot is returned.                                                     ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors                                                     ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
function gsn_vector_contour(wks:graphic,u[*][*]:numeric,\
                            v[*][*]:numeric,data[*][*]:numeric,\
                            resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, res2
begin
    res2      = resources

;
; Create the vector field object.
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = create wksname + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

;
; Check for missing values.
;
    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create the scalar field object.

    sfdata_object = create wksname + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)

; Create vector plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : vfdata_object
    end create
    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(vfdata_object,vfres)
    end if
    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(sfdata_object,sfres)
    end if
    if(cnres.and..not.any(ismissing(getvaratts(cnres))))
      attsetvalues(contour_object,cnres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(vector_object,vcres)
    end if
        "cnScalarFieldData" : sfdata_object
    cnres = get_res_eq(res2,(/"cn","vp"/))
    vcres = get_res_ne(res2,(/"vf","sf","cn","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(contour_object,cnres)
    attsetvalues_check(vector_object,vcres)

    if(sprdcols)
      cnres2 = True
    draw_and_frame(wks,vector_object,calldraw,callframe)

      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))

      attsetvalues(contour_object,cnres2)
      attsetvalues(contour_object,cnres2)
    end if

    overlay(stream_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(stream_object)
    end if

    draw_and_frame(wks,stream_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).
;                     v: 2-dimensional V data                           ;

;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
local i, attnames, vfdata_object, sfdata_object, contour_object, res, \
; to the workstation "wks" (the variable returned from a previous call  ;
; to "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be   ;
; vectorized, and "data" is the scalar field to be contoured.           ;
; "resources" is an optional list of resources. The id of the map plot  ;
; is returned.                                                          ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors                                                     ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
function gsn_vector_contour_map(wks:graphic,u[*][*]:numeric,\
                               v[*][*]:numeric,data[*][*]:numeric,\
                               resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, mp_res_index, map_object, res2
begin
    res2      = resources

;
; Create the vector field object.
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = create wksname + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

;
; Check for missing values.
;
    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create the scalar field object.

    sfdata_object = create wksname + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
        "vcVectorFieldData" : vfdata_object
    end create

; Create contour plot object.

    contour_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : sfdata_object
    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(vfdata_object,vfres)
    end if
    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(sfdata_object,sfres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    if(cnres.and..not.any(ismissing(getvaratts(cnres))))
      attsetvalues(contour_object,cnres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(vector_object,vcres)
    end if

    vcres = get_res_ne(res2,(/"cn","mp","sf","vf","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(map_object,mpres)
    attsetvalues_check(contour_object,cnres)
    attsetvalues_check(vector_object,vcres)

    if(sprdcols)
      cnres2 = True
      vcres2 = True
    draw_and_frame(wks,map_object,calldraw,callframe)
      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))

      attsetvalues(contour_object,cnres2)
      attsetvalues(vector_object,vcres2)
    end if

    overlay(map_object,vector_object)
    overlay(map_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

;                   wks: workstation object                             ;

;                     : 2-dimensional U data                            ;
;                     v: 2-dimensional V data                           ;
local i, attnames, data_object, contour_object, res, vf_res_index, \
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "resources" is an optional list of resources. The id  ;
; of the map plot is returned.                                          ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors                                                     ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
function gsn_vector_map(wks:graphic, u[*][*]:numeric, v[*][*]:numeric, \
                        resources:logical )
local i, data_object, contour_object, res, vf_res_index, \
vc_res_index, mp_res_index, map_object, res2, sprdcols
begin
    res2      = resources

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = create wksname + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
    sprdcols  = get_special_res_value(res2,"gsnSpreadColors",False)
    min_index = get_special_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_special_res_value(res2,"gsnSpreadColorEnd",-1)
    end if

; Create plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
    if(sprdcols)
      set_attr(vcres,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))
    end if

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(data_object,vfres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(vector_object,vcres)
    end if
; Create map object.
    shape     = get_res_value(res2,"gsnShape",False)

    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    mpres = get_res_eq(res2,(/"mp","vp"/))
    vcres = get_res_ne(res2,(/"mp","vf","vp"/))
    draw_and_frame(wks,map_object,calldraw,callframe)
    attsetvalues_check(data_object,vfres)
    attsetvalues_check(map_object,mpres)
    attsetvalues_check(vector_object,vcres)

    if(sprdcols)
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))
      attsetvalues(vector_object,vcres2)
    end if

    overlay(map_object,vector_object)
;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)
;                     u: 2-dimensional U array                          ;

;                     v: 2-dimensional V array                          ;
;                  data: 2-dimensional scalar field                     ;
local i, attnames, vfdata_object, sfdata_object, plot_object, res, \
;                                                                       ;
; This function creates and draws a vector plot to the workstation "wks";
; (the variable returned from a previous call to "gsn_open_wks").  "u"  ;
; and "v" are the 2-dimensional arrays to be vectorized, and "data" is  ;
; the scalar field that the vectors are colored by. "resources" is an   ;
; optional list of resources. The id of the vector plot is returned.    ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors                                                     ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
function gsn_vector_scalar(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                           data[*][*]:numeric, resources:logical )
local i, vfdata_object, sfdata_object, plot_object, res, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, vf_res_index, \
vc_res_index, sf_res_index, res2, sprdcols
begin
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the vector field data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = create wksname + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Create the scalar field data object.

    sfdata_object = create wksname + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Check for missing values.
    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
    sprdcols  = get_special_res_value(res2,"gsnSpreadColors",False)
    min_index = get_special_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_special_res_value(res2,"gsnSpreadColorEnd",-1)
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData"     : vfdata_object
        "vcScalarFieldData"     : sfdata_object
    if(sprdcols)
      set_attr(vcres,"vcLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index))
    end if

        "vcUseScalarArray"      : True
        "vcMonoLineArrowColor"  : False
    end create

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(vfdata_object,vfres)
    end if
    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(sfdata_object,sfres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(plot_object,vcres)
    end if

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    vcres = get_res_ne(res2,(/"sf","vf"/))

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      llres = get_res_eq(res2,(/"tr","vp"/))
    end if

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(plot_object,vcres)
    if(sprdcols)
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index))
      attsetvalues(plot_object,vcres2)
    end if
;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

; Check if we need to force the X or Y axis to be linear or log.
      if(llres.and..not.any(ismissing(getvaratts(llres))))
        attsetvalues(plot_object,llres)
      end if

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

    draw_and_frame(wks,plot_object,calldraw,callframe)
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues

      plot_object = create wksname + "_loglin" logLinPlotClass wks
        "trXLog"  : force_x_log
        "trYLog"  : force_y_log
        "trXMinF" : trxmin
        "trXMaxF" : trxmax
        "trYMinF" : trymin
        "trYMaxF" : trymax
      end create
      attsetvalues_check(plot_object,llres)

      overlay(plot_object,overlay_plot_object)
      plot_object@contour = overlay_plot_object
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe,maxbb)

;                     v: 2-dimensional V data                           ;

;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
local i, attnames, vfdata_object, sfdata_object, contour_object, res, \
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "data" is the scalar field that the vectors are       ;
; colored by. "resources" is an optional list of resources. The id of   ;
; the map plot is returned.                                             ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors                                                     ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
function gsn_vector_scalar_map(wks:graphic,u[*][*]:numeric,\
                               v[*][*]:numeric,data[*][*]:numeric,\
                               resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, mp_res_index, map_object, res2, \
sprdcols
begin
    res2      = resources
;
; Create the vector field object.
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = create wksname + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

;
; Check for missing values.
;
    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create the scalar field object.

    sfdata_object = create wksname + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
    sprdcols  = get_special_res_value(res2,"gsnSpreadColors",False)
    min_index = get_special_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_special_res_value(res2,"gsnSpreadColorEnd",-1)
; Create plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData"     : vfdata_object
        "vcScalarFieldData"     : sfdata_object
        "vcUseScalarArray"      : True
    if(sprdcols)
      set_attr(vcres,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))
    end if

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(vfdata_object,vfres)
    end if
    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(sfdata_object,sfres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(vector_object,vcres)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    mpres = get_res_eq(res2,(/"mp","vp"/))
    vcres = get_res_ne(res2,(/"mp","sf","vf","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    draw_and_frame(wks,map_object,calldraw,callframe)
    attsetvalues_check(map_object,mpres)
    attsetvalues_check(vector_object,vcres)

    if(sprdcols)
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))
      attsetvalues(vector_object,vcres2)
    end if

    overlay(map_object,vector_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)


;***********************************************************************;
; Function : gsn_xy                                                     ;
;                   wks: workstation object                             ;
;                     x: n-dimensional array of X arrays                ;
;                     y: n-dimensional array of Y array                 ;
;             resources: optional resources                             ;
;                                                                       ;
; This function creates and draws an xy plot to the workstation "wks"   ;
; (the variable returned from a previous call to "gsn_open_wks").  "x"  ;
; and "y" are either 1 or 2-dimensional arrays containing the X and Y   ;
; data points and "resources" is an optional list of resources. The id  ;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;                                                                       ;
;***********************************************************************;
function gsn_xy(wks:graphic, x:numeric, y:numeric, resources:logical )
local i, attnames, data_object, plot_object, res, ca_res_index, \
xy_res_index, xydp_res_index, dspec, res2, set_dash
begin
    set_dash = True       ; Default is to set some dash patterns.
    res2     = resources

; Determine if we have multiple lines or just one line.

    nxdims = dimsizes(dimsizes(x))
    xdims = dimsizes(x)
    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = create wksname + "_data" coordArraysClass noparent
        "caXArray" : x
        "caYArray" : y
    end create
      "trXMinF" : trxmin
      "trXMaxF" : trxmax
      "trYMinF" : trymin
      "trYMaxF" : trymax
        setvalues data_object
            "caXMissingV" :x@_FillValue
    if(res2.and..not.any(ismissing(getvaratts(res2))))
      if(isatt(res2,"trXMinF")) then
        trxmin = res2@trXMinF
      end if 
      if(isatt(res2,"trXMaxF")) then
        trxmax = res2@trXMaxF
      end if 
      if(isatt(res2,"trYMinF")) then
        trymin = res2@trYMinF
      end if 
      if(isatt(res2,"trYMayF")) then
        trymax = res2@trYMaxF
      end if 
    end if
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_xy" xyPlotClass wks
        "xyCoordData" : data_object
    end create

    getvalues plot_object
      "trXMinF" : trxmin2
      "trXMaxF" : trxmax2
      "trYMinF" : trymin2
      "trYMaxF" : trymax2
    end getvalues

    trxmin = get_res_value_keep(res2,"trXMinF",trxmin2)
    trxmax = get_res_value_keep(res2,"trXMaxF",trxmax2)
    trymin = get_res_value_keep(res2,"trYMinF",trymin2)
    trymax = get_res_value_keep(res2,"trYMaxF",trymax2)

    plot_object = create wksname + "_xy" xyPlotClass wks
        "xyCoordData" : data_object
        "trXMinF"     : trxmin
        "trXMaxF"     : trxmax
        "trYMinF"     : trymin
        "trYMaxF"     : trymax
    end create

; Check for existence of x@long_name and y@long_name and use them
; to label X and Y axes.

    if(isatt(x,"long_name")) then
      set_attr(res2,"tiXAxisString",x@long_name)
    end if
    if(isatt(y,"long_name")) then
      set_attr(res2,"tiYAxisString",y@long_name)
    end if

    calldraw  = get_special_res_value(res2,"gsnDraw", True)
    callframe = get_special_res_value(res2,"gsnFrame",True)
    shape     = get_special_res_value(res2,"gsnShape",False)
    scale     = get_special_res_value(res2,"gsnScale",shape)
    end getvalues

    if(res2.and..not.any(ismissing(getvaratts(res2))))
      if(isatt(res2,"xyDashPattern").or.isatt(res2,"xyDashPatterns"))
    if(cares.and..not.any(ismissing(getvaratts(cares))))
      attsetvalues(data_object,cares)
    end if
      end if
    end if

    if(set_dash)
      setvalues dspec
        "xyDashPatterns" : (/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,\
                             0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,\
                             0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/)
      end setvalues
    end if

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)

    check_for_tickmarks_off(res2)


    cares = get_res_eq(res2,"ca")
    attsetvalues_check(data_object,cares)

    if(res2.and..not.any(ismissing(getvaratts(res2))))
; Get list of resources.

        attnames = getvaratts(res2)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

;***********************************************************************;
; Check to see if any xy plot resources were set.  There are two kinds  ;
; of xy plot resources, the regular kind, and the data spec kind.  If   ;
; the resource starts with an "xy", it could be either kind, so we need ;
; to have some tests to see which object it belongs to.  Any "xy"       ;
; resources that start with "xyCo", "xyX", or "xyY" are regular         ;
; resources (meaning, it belongs to the XyPlot object). The remaining   ;
; "xy" resources belong to the data spec object. Any resources that do  ;
; not start with "xy" or "ca" are assumed to also go with the XyPlot    ;
; object.                                                               ;
;***********************************************************************;
        if(dimsizes(dimsizes(res)).eq.1)
            if((chartostring(res(0:1)).ne."ca".and.\
                chartostring(res(0:1)).ne."xy").or.\
               (chartostring(res(0:1)).eq."xy".and.\
               (chartostring(res(0:3)).eq."xyCo".or.\
                chartostring(res(0:2)).eq."xyX".or.\
                chartostring(res(0:2)).eq."xyY")))
                setvalues plot_object
                    attnames : res2@$attnames$
                end setvalues
                xyres = True
                do i = 0,dimsizes(xy_res_index)-1
                    xyres@$attnames(xy_res_index(i))$ = res2@$attnames(xy_res_index(i))$
                end do
                attsetvalues(plot_object,xyres)
                setvalues dspec
                    attnames : res2@$attnames$
                end setvalues
            end if
        else
            xy_res_index = ind((chartostring(res(:,0:1)).ne."ca".and.\
                                chartostring(res(:,0:1)).ne."xy").or.\
                               (chartostring(res(:,0:1)).eq."xy".and.\
                               (chartostring(res(:,0:3)).eq."xyCo".or.\
                attsetvalues(dspec,xydpres)
                                chartostring(res(:,0:2)).eq."xyY")))
            xydp_res_index = ind(chartostring(res(:,0:1)).eq."xy".and.\
                                (chartostring(res(:,0:3)).ne."xyCo".and.\
                                 chartostring(res(:,0:2)).ne."xyX".and.\
                                 chartostring(res(:,0:2)).ne."xyY"))
            if(.not.all(ismissing(xy_res_index)))
              xyres = True
              do i = 0,dimsizes(xy_res_index)-1
                  xyres@$attnames(xy_res_index(i))$ = res2@$attnames(xy_res_index(i))$
              end do
              attsetvalues_check(plot_object,xyres)
            end if
            if(.not.all(ismissing(xydp_res_index)))
                getvalues plot_object
                    "xyCoordDataSpec" : dspec
                end getvalues
                xydpres = True
                do i = 0,dimsizes(xydp_res_index)-1
                    xydpres@$attnames(xydp_res_index(i))$ = res2@$attnames(xydp_res_index(i))$
                end do
    draw_and_frame(wks,plot_object,calldraw,callframe)
            end if
        end if

    end if
;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.

      gsnp_shape_plot(plot_object)
    end if

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    plot_object@data     = data_object
    plot_object@dataspec = dspec
    return(plot_object)
end
