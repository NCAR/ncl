;
;      $Id: gsn_code.ncl,v 1.68 2001-05-17 16:24:11 haley Exp $
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

undef("smooth92d")
undef("smooth93d")
undef("hsv2rgb")
undef("tofloat")
undef("tointeger")
undef("lower_case")
undef("set_attr")
undef("check_attr")
undef("get_res_eq")
undef("get_res_ne")
undef("get_res_value")
undef("get_res_value_keep")
undef("get_non_missing_pairs")
undef("attsetvalues_check")
undef("spread_colors")
undef("check_for_irreg2loglin")
undef("maximize_bb")
undef("gsnp_turn_off_tickmarks")
undef("gsnp_point_tickmarks_outward")
undef("gsnp_uniform_tickmark_labels")
undef("gsnp_shape_plot")
undef("gsnp_scale_plot")
undef("check_for_tickmarks_off")
undef("compute_ps_device_coords")
undef("draw_and_frame")
undef("get_bb_res")
undef("gsn_open_ncgm")
undef("gsn_open_x11")
undef("create_labelbar")
undef("gsn_open_ps")
undef("gsn_open_wks")
undef("gsn_polygon")
undef("gsn_add_primitive")
undef("gsn_add_polygon")
undef("gsn_polygon_ndc")
undef("gsn_polyline")
undef("gsn_add_polyline")
undef("gsn_polyline_ndc")
undef("gsn_polymarker")
undef("gsn_add_polymarker")
undef("gsn_polymarker_ndc")
undef("gsn_labelbar_ndc")
undef("gsn_text_ndc_return")
undef("gsn_text_ndc")
undef("gsn_text")
undef("gsn_add_text")
undef("gsn_panel_return")
undef("gsn_panel")
undef("gsn_attach_plots")
undef("draw_bb")
undef("get_rgb_values")
undef("gsn_define_colormap")
undef("gsn_retrieve_colormap")
undef("gsn_merge_colormaps")
undef("hist_columns")
undef("compute_hist_vals")
undef("gsn_histogram")
undef("gsn_contour")
undef("gsn_contour_map")
undef("gsn_map")
undef("gsn_streamline")
undef("gsn_streamline_map")
undef("gsn_draw_colormap")
undef("gsn_vector")
undef("gsn_vector_contour")
undef("gsn_streamline_contour")
undef("gsn_vector_contour_map")
undef("gsn_vector_map")
undef("gsn_vector_scalar")
undef("gsn_vector_scalar_map")
undef("gsn_xy")

;***********************************************************************;
; function : smooth92d                                                  ;
;               var[*][*]:float                                         ;
;                       p:float                                         ;
;                       q:float                                         ;
;                                                                       ;
; Performs smoothing on a 2-dimensional array.                          ;
;                                                                       ;
;***********************************************************************;
function smooth92d(var[*][*]:float,p[1]:float,q[1]:float)
local dims,output,coef,m,n,p4,q4,i
begin
  dims = dimsizes(var)
  output = new((/dims(0),dims(1)/),float)

  coef = 1 - p - q
  m = dims(0)
  n = dims(1)
  p4 = p/4.0
  q4 = q/4.0

  do i = 1, m -2
    output(i,1:n-2) = (p4)*(var( i-1, 1 : n-2 ) + var( i, 2 : n-1) + \
                            var( i+1, 1 : n-2) + var( i, 0 : n-3)) + \
                            (q4)*(var(i-1, 0 : n-3 ) + var(i-1, 2 : n-1) + \
                                  var( i+1, 2 : n-1) + var( i+1, 0 : n-3))
  end do

  output = output + (coef * var) 
        
  if(iscoord(var,var!0))  then
    output!0 = var!0
    output&$var!0$ = var&$var!0$
  end if

  if(iscoord(var,var!1))  then
    output!1 = var!1
    output&$var!1$ = var&$var!1$
  end if
        
  return(output)
end

;***********************************************************************;
; function : smooth93d                                                  ;
;               var[*][*][*]:float                                      ;
;                          p:float                                      ;
;                          q:float                                      ;
;                                                                       ;
; Performs smoothing on a 3-dimensional array.                          ;
;                                                                       ;
;***********************************************************************;
function smooth93d(var[*][*][*]:float,p[1]:float,q[1]:float)
local dims,output,coef,m,n,p4,q4,i
begin
  dims = dimsizes(var)
        
  output = new((/dims(0),dims(1),dims(2)/),float)

  coef = 1 - p - q
  m = dims(1)
  n = dims(2)
  p4 = p/4.0
  q4 = q/4.0

  do i = 1, m -2
    output(:,i,1:n-2) = (p4)*(var( :,i-1, 1 : n-2 ) + var(:, i, 2 : n-1) + \
                              var( :,i+1, 1 : n-2) + var(:, i, 0 : n-3)) + \
                        (q4)*(var( :,i-1, 0 : n-3 ) + var( :,i-1, 2 : n-1) + \
                              var( :,i+1, 2 : n-1) + var(:, i+1, 0 : n-3))
  end do

  output = output + (coef * var) 
        
  if(iscoord(var,var!0))  then
    output!0 = var!0
    output&$var!0$ = var&$var!0$
  end if

  if(iscoord(var,var!1))  then
    output!1 = var!1
    output&$var!1$ = var&$var!1$
  end if
  if(iscoord(var,var!2))  then
    output!2 = var!2
    output&$var!2$ = var&$var!2$
  end if
  
  return(output)
end

;***********************************************************************;
; function : hsv2rgb                                                    ;
;                 h:float                                               ;
;                 s:float                                               ;
;                 v:float                                               ;
; This function maps values from the HSV color model to the RGB color   ;
; model. HSV is a good model for generating smooth color maps. See      ;
; (Computer Graphics: Principles and Practice by Foley). The return     ;
; value is a 2 dimensional array of rgb color triplets. The return      ;
; value from this function can be directly assigned to the "wkColorMap" ;
; resource of a workstation object or to the second argument of         ;
; gsn_define_colormap.                                                  ;
;***********************************************************************;
function hsv2rgb (h[*]:float,s[*]:float,v[*]:float)
begin
;
; This function converts between HSV and RGB color space
; Input: h [0.0-360.0], s [0.0-1.0], v [0.0-1.0]
; Output: r [0.0-1.0], g [0.0-1.0], b [0.0-1.0]
;
  r_g_b = new((/3,dimsizes(h)/),float)
  r_g_b!0 = "rgb"
  r_g_b!1 = "cmap_len"
 
  if (any((s .eq. 0.0).and.(h.eq.0.0.or.h.eq.360))) then
    indexs = ind((h.eq.0.0.or.h.eq.360).and.s.eq.0.0)
    r_g_b(:,indexs) = (/v(indexs),v(indexs),v(indexs)/)
    delete(indexs)
  end if

  f = new(dimsizes(h),float)
  p = new(dimsizes(h),float)
  q = new(dimsizes(h),float)
  t = new(dimsizes(h),float)
  i = new(dimsizes(h),integer)
  if any(h.eq.360.0)  
    h(ind(h.eq.360.0)) = 0.0
  end if

  h = h/60.0
  i = floattoint(floor(h))
  f = h - i
  p = v*(1.0 - s)
  q = v*(1.0 - (s*f))
  t = v*(1.0 - (s*(1.0 - f)))
  if any(i.eq.0) then
    indexs = ind(i.eq.0)
    r_g_b(:,indexs) = (/v(indexs),t(indexs),p(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.1) then
    indexs = ind(i.eq.1)
    r_g_b(:,indexs) = (/q(indexs),v(indexs),p(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.2) then
    indexs = ind(i.eq.2)
    r_g_b(:,indexs) = (/p(indexs),v(indexs),t(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.3) then
    indexs = ind(i.eq.3)
    r_g_b(:,indexs) = (/p(indexs),q(indexs),v(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.4) then
    indexs = ind(i.eq.4)
    r_g_b(:,indexs) = (/t(indexs),p(indexs),v(indexs)/)
    delete(indexs)
  end if
  if any(i.eq.5) then
    indexs = ind(i.eq.5)
    r_g_b(:,indexs) = (/v(indexs),p(indexs),q(indexs)/)
    delete(indexs)
  end if
  if(any(ismissing(r_g_b)))
    print("Warning: hsv2rgb: Some invalid HSV values were passed to hsv2rgb")
  end if
  return(r_g_b(cmap_len|:,rgb|:))
end


;***********************************************************************;
; function : tofloat                                                    ;
;                x:numeric                                              ;
;                                                                       ;
; Convert input to float.                                               ;
;                                                                       ;
;***********************************************************************;
function tofloat(x:numeric)
local xf
begin
  if(typeof(x).eq."double")
    xf = doubletofloat(x)
  else
    if(isatt(x,"_FillValue"))
      xf = new(dimsizes(x),float,x@_FillValue)
    else
      xf = new(dimsizes(x),float)
      delete(xf@_FillValue)
    end if
    xf = x
  end if
  return(xf)
end

;***********************************************************************;
; function : tointeger                                                  ;
;                x:numeric                                              ;
;                                                                       ;
; Convert input to integer.                                             ;
;                                                                       ;
;***********************************************************************;
function tointeger(x:numeric)
local xi
begin
  if(typeof(x).eq."double")
    xi = doubletointeger(x)
  else
    if(typeof(x).eq."float")
      xi = floattointeger(x)
    else
      if(isatt(x,"_FillValue"))
        xi = new(dimsizes(x),integer,x@_FillValue)
      else
        xi = new(dimsizes(x),integer)
        delete(xi@_FillValue)
      end if
      xi = x
    end if
  end if
  return(xi)
end

;***********************************************************************;
; Function : lower_case                                                 ;
;           name : string                                               ;
;                                                                       ;
; Converts "name" to lowercase.                                         ;
;***********************************************************************;
function lower_case(name:string)
local lower, upper, lowc, upc, namec, i
begin
  lower = (/"a","b","c","d","e","f","g","h","i","j","k","l","m", \
            "n","o","p","q","r","s","t","u","v","w","x","y","z" /)
  upper = (/"A","B","C","D","E","F","G","H","I","J","K","L","M", \
            "N","O","P","Q","R","S","T","U","V","W","X","Y","Z" /)
  
  lowc  = stringtochar(lower)
  upc   = stringtochar(upper)
  namec = stringtochar(name)

  do i = 0, dimsizes(namec)-2
    if (any(upc(:,0).eq.namec(i))) then
      namec(i) = lowc(ind(upc(:,0).eq.namec(i)),0)
    end if
  end do
  return(chartostring(namec))
end


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
; Function : check_attr                                                 ;
;                res    : logical                                       ;
;           att_name    : string                                        ;
;          att_value                                                    ;
;          convert_lower: logical                                       ;
;                                                                       ;
; Checks if res@att_name exists and if it is equal to att_value.        ;
;***********************************************************************;
function check_attr(res:logical,att_name:string,att_value, \
                    convert_lower:logical)
local res2, new_att_value
begin
  res2 = res

  if(res2.and.isatt(res2,att_name))
    if(typeof(att_value).eq."string".and.convert_lower)
      new_att_value = lower_case(att_value)
      res2@$att_name$ = lower_case(res2@$att_name$)
    else
      new_att_value = att_value
    end if

    if(res2@$att_name$.eq.new_att_value)
      return(True)
    end if
  end if

  return(False)
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
; Function : get_res_value                                              ;
;                res:logical                                            ;
;            resname:string                                             ;
;        default_val                                                    ;
;                                                                       ;
; This function checks to see if the given resource has been set, and if;
; so, it returns its value and removes it from the resource list.       ;
; Otherwise, it returns the default value which is the last argument    ;
; passed in.                                                            ;
;                                                                       ;
;***********************************************************************;
function get_res_value(res:logical,resname:string,default_val)
local return_val
begin
  if(res.and..not.any(ismissing(getvaratts(res)))) then
    if(isatt(res,resname)) then
      return_val = res@$resname$
      delete(res@$resname$)
    else
      return_val = default_val
    end if
  else
    return_val = default_val
  end if

  return(return_val)
end

;***********************************************************************;
; Function : get_res_value_keep                                         ;
;                res:logical                                            ;
;            resname:string                                             ;
;        default_val                                                    ;
;                                                                       ;
; This function checks to see if the given resource has been set, and if;
; so, it returns its value and keeps it from the resource list.         ;
; Otherwise, it returns the default value which is the last argument    ;
; passed in.                                                            ;
;                                                                       ;
;***********************************************************************;
function get_res_value_keep(res,resname:string,default_val)
local return_val
begin
  if(((typeof(res).eq."logical".and.res).or.(typeof(res).ne."logical")).and.\
     .not.any(ismissing(getvaratts(res)))) then
    if(isatt(res,resname)) then
      return_val = res@$resname$
    else
      return_val = default_val
    end if
  else
    return_val = default_val
  end if

  return(return_val)
end

;***********************************************************************;
; This function peruses two arrays of the same length and returns pairs ;
; of indices that represent ranges of data values where there are no    ;
; missing values.                                                       ;
;***********************************************************************;
function get_non_missing_pairs(x[*]:numeric,y[*]:numeric)
local ibeg, iend, indices, ndimx, ndimy, is_missing
begin
  ndimx = dimsizes(x)
  ndimy = dimsizes(y)

  if(ndimx.ne.ndimy)
    print("get_non_missing_pairs: x and y must be the same length")
  end if

  indices = new((/ndimx,2/),integer,-999)

  counter = 0
  ibeg = -1   ; First non-missing point in a group.
  do i = 0,ndimx-1
    if(.not.ismissing(x(i)).and..not.ismissing(y(i)))
      if(ibeg.lt.0)    ; on the first point of the line
        ibeg = i
        iend = i       ; Represents last non-missing point in a group
      else
        iend = i
      end if
      is_missing = False
    else
      is_missing = True
    end if
    if(ibeg.ge.0.and.(is_missing.or.iend.eq.ndimx-1))
      indices(counter,0) = ibeg
      indices(counter,1) = iend
      ibeg    = -1            ; Reinitialize
      counter = counter + 1
    end if
  end do
  return(indices)
end

;***********************************************************************;
; Procedure : attsetvalues_check                                        ;
;               plot:graphic                                            ;
;                res:logical                                            ;
;                                                                       ;
; This procedure passes plot and res to attsetvalues only if res        ;
; resources.                                                            ;
;***********************************************************************;
procedure attsetvalues_check(plot:graphic,res:logical)
begin
  if(res.and..not.any(ismissing(getvaratts(res))))
    attsetvalues(plot,res)
  end if
  return
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
;
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

  return
end

;***********************************************************************;
; Procedure : check_for_irreg2loglin                                    ;
;                res:logical                                            ;
;            xlinear:logical                                            ;
;            ylinear:logical                                            ;
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

  xlinear = get_res_value(res,"gsnXAxisIrregular2Linear",xlinear)
  ylinear = get_res_value(res,"gsnYAxisIrregular2Linear",ylinear)
  xlog    = get_res_value(res,"gsnXAxisIrregular2Log",xlog)
  ylog    = get_res_value(res,"gsnYAxisIrregular2Log",ylog)

  if(ylog.and.ylinear)
    print("Error: You cannot set both gsnYAxisIrregular2Log")
    print("and gsnYAxisIrregular2Linear to True.")
    exit
  end if

  if(xlog.and.xlinear)
     print("Error: You cannot set both gsnXAxisIrregular2Log")
     print("and gsnXAxisIrregular2Linear to True.")
     exit
  end if

  return
end

;**********************************************************************;
; Function : maximize_bb                                               ;
;               plot : graphic                                         ;
;                res : logical                                         ;
;                                                                      ;
; This function computes the viewport coordinates needed to optimize   ;
; the size of a plot on a page.                                        ;
;                                                                      ;
; plot : plot to maximize on the page.                                 ;
;                                                                      ;
; res : list of optional resources. Ones accepted include:             ;
;                                                                      ;
;     "gsnBoxMargin" - margin to leave around plots (in NDC units,     ;
;                     default is 0.02)                                 ;
;                                                                      ;
;**********************************************************************;
function maximize_bb(plot[1]:graphic,res:logical)
local coords, top, bot, lft, rgt, width, height, margin
begin
;
; Get resources.
;
  margin = get_res_value_keep(res,"gsnBoxMargin",0.02)

;
; Get bounding box of plot.
;
  bb = NhlGetBB(plot)

  top = bb(0)
  bot = bb(1)
  lft = bb(2)
  rgt = bb(3)

;
; Get height/width of plot in NDC units.
;
  uw = rgt - lft
  uh = top - bot

;
; Calculate scale factor needed to make plot larger.
;
  scale = (1 - 2*margin)/max((/uw,uh/))

;
; Get the viewport.
;
  getvalues plot
    "vpXF"      : vpx
    "vpYF"      : vpy
    "vpWidthF"  : vpw
    "vpHeightF" : vph
  end getvalues

  dx = scale * (vpx - lft) ; Calculate distance from plot's left position
                           ; to its leftmost annotation
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
;                                                                       ;
; By default, tickmarks are drawn on all plots that aren't overlaid on  ;
; a map. If gsnTickMarksOn is set to False, then this turns off the     ;
; drawing of tick marks.  This procedure just sets the resources        ;
; necessary in order to turn off tick marks.                            ;
;***********************************************************************;
procedure gsnp_turn_off_tickmarks(res:logical)
begin
  set_attr(res,"tmXBBorderOn",False )
  set_attr(res,"tmXBOn",      False)
  set_attr(res,"tmXTBorderOn",False)
  set_attr(res,"tmXTOn",      False)
  set_attr(res,"tmYLBorderOn",False)
  set_attr(res,"tmYLOn",      False)
  set_attr(res,"tmYRBorderOn",False)
  set_attr(res,"tmYROn",      False)
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
    end if
  end if

  if(minor_length.lt.0.)
    getvalues plot
      "tmXBMinorLengthF"        : x_minor_length
      "tmYLMinorLengthF"        : y_minor_length
    end getvalues
    if(x_minor_length.gt.0..and.y_minor_length.gt.0.)
      x_minor_length = min((/x_minor_length,y_minor_length/))
      y_minor_length = x_minor_length
    end if
  else
    if(x_minor_length.gt.0.)
      x_minor_length = minor_length
    end if
    if(y_minor_length.gt.0.)
      y_minor_length = minor_length
    end if
  end if

  tmres = res
  tmres = True
  set_attr(tmres,"tmXBMajorLengthF"        , x_major_length)
  set_attr(tmres,"tmXBMajorOutwardLengthF" , x_major_length)
  set_attr(tmres,"tmXBMinorLengthF"        , x_minor_length)
  set_attr(tmres,"tmXBMinorOutwardLengthF" , x_minor_length)
  set_attr(tmres,"tmXTMajorLengthF"        , x_major_length)
  set_attr(tmres,"tmXTMajorOutwardLengthF" , x_major_length)
  set_attr(tmres,"tmXTMinorLengthF"        , x_minor_length)
  set_attr(tmres,"tmXTMinorOutwardLengthF" , x_minor_length)
  set_attr(tmres,"tmYLMajorLengthF"        , y_major_length)
  set_attr(tmres,"tmYLMajorOutwardLengthF" , y_major_length)
  set_attr(tmres,"tmYLMinorLengthF"        , y_minor_length)
  set_attr(tmres,"tmYLMinorOutwardLengthF" , y_minor_length)
  set_attr(tmres,"tmYRMajorLengthF"        , y_major_length)
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
;                                                                       ;
; This procedure makes the tickmark labels the same font height on both ;
; axes. If font_height <= 0., then a uniform font height is calculated. ;
;***********************************************************************;
procedure gsnp_uniform_tickmark_labels(plot:graphic,res:logical, \
                                       font_height)
local xbfont, ylfont, tmres
begin

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
  getvalues plot
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
    "tmYLLabelFontHeightF" : (xbfont+ylfont)/2.
    "tmYLMajorLengthF"     : ylength
    "tmYLMinorLengthF"     : ymlength
  end setvalues
end

;***********************************************************************;
; Procedure : check_for_tickmarks_off                                   ;
;                res:logical                                            ;
;                                                                       ;
; By default, tickmarks are drawn on all plots that aren't overlaid on  ;
; a map. If gsnTickMarksOn is set to False, then this turns off the     ;
; drawing of tick marks.  This procedure checks for the setting of this ;
; resource, and then calls the routine that turns off tickmarks.        ;
;***********************************************************************;
procedure check_for_tickmarks_off(res:logical)
local ticks_ons
begin

; Check if turning tickmarks off.

  ticks_on = get_res_value(res,"gsnTickMarksOn",True)
  if(.not.ticks_on)
    gsnp_turn_off_tickmarks(res)
  end if
end

;**********************************************************************;
; Function : compute_ps_device_coords                                  ;
;                bb(:,4) : float                                       ;
;                  res : logical                                       ;
;                                                                      ;
; This function computes the PostScript device coordinates needed to   ;
; make a plot fill up the full page.                                   ;
;                                                                      ;
; bb     : bounding box that contains all graphical objects. It should ;
;          be a n x 4 float array with values between 0 and 1.         ;
;            (top,bottom,left,right)                                   ;
;                                                                      ;
; res : list of optional resources. Ones accepted include:             ;
;                                                                      ;
; "gsnPaperOrientation" - orientation of paper. Can be "landscape",    ;
;                         "portrait", or "auto". Default is "auto".    ;
;                                                                      ;
;       "gsnPaperWidth"  - width of paper (in inches, default is 8.5)  ;
;       "gsnPaperHeight" - height of paper (in inches, default is 11.0);
;       "gsnPaperMargin" - margin to leave around plots (in inches,    ;
;                        default is 0.5)                               ;
;                                                                      ;
;**********************************************************************;
function compute_ps_device_coords(bb,res)
local coords, top, bot, lft, rgt, dpi, dpi_pw, dpi_ph, dpi_margin, \
      paper_width, paper_height, paper_margin
begin
;
; Get resources.
;
  paper_height = get_res_value_keep(res,"gsnPaperHeight",11.0)
  paper_width  = get_res_value_keep(res,"gsnPaperWidth",8.5)
  paper_margin = get_res_value_keep(res,"gsnPaperMargin",0.5)
  paper_orient = get_res_value_keep(res,"gsnPaperOrientation","auto")
  is_debug     = get_res_value_keep(res,"gsnDebug",False)
;
; Check to see if any panel resources have been set.  No defaults
; will be assumed for these. They are only used if they have been
; explicitly set by the user.
;
  lft_pnl = isatt(res,"gsnPanelLeft")
  rgt_pnl = isatt(res,"gsnPanelRight")
  bot_pnl = isatt(res,"gsnPanelBottom")
  top_pnl = isatt(res,"gsnPanelTop")

  if(typeof(paper_orient).eq."integer")
    if(paper_orient.eq.0)
      lc_orient = "portrait"
    else
      lc_orient = "landscape"
    end if
  else
    lc_orient = lower_case(paper_orient)
  end if
;
; Get the bounding box that covers all the plots.  If gsnPanel 
; resources have been added to add white space around plots, then
; count this white space in as well.  Note that even though the bounding
; box coordinates should be positive, it *is* possible for them to be
; negative, and we need to keep these negative values in our calculations
; later to preserve the aspect ratio.
;
  dimbb = dimsizes(bb)
  if(dimsizes(dimbb).eq.1) then
;
; Force newbb to be 2-dimensional so we don't have to have a
; bunch of "if" tests later.
;
    newbb = new((/1,4/),float)
    newbb(0,:) = bb
  else
    newbb = bb
  end if

  if(top_pnl)
    top = max((/1.,max(newbb(:,0))/))
  else
    top = max(newbb(:,0))
  end if
  if(bot_pnl)
    bot = min((/0.,min(newbb(:,1))/))
  else
    bot = min(newbb(:,1))
  end if
  if(lft_pnl)
    lft = min((/0.,min(newbb(:,2))/))
  else
    lft = min(newbb(:,2))
  end if
  if(rgt_pnl)
    rgt = max((/1.,max(newbb(:,3))/))
  else
    rgt = max(newbb(:,3))
  end if

;  if(bot.lt.0.or.bot.gt.1.or.top.lt.0.or.top.gt.1.or. \
;     lft.lt.0.or.lft.gt.1.or.rgt.lt.0.or.rgt.gt.1)
;    print("compute_ps_device_coords: warning: bounding box values should be between 0 and 1 inclusive. Will continue anyway.")
;  end if

  if(bot.ge.top.or.lft.ge.rgt)
    print("compute_ps_device_coords: bottom must be < top and left < right")
    return((/0,0,0,0/))
  end if
;
; Debug prints
;
  if(is_debug)
    print("-------Bounding box values for PostScript-------")
    print("    top = " + top + " bot = " + bot + \
          "    lft = " + lft + " rgt = " + rgt)
  end if
;
; Initialization
;
  dpi        = 72                       ; Dots per inch.
  dpi_pw     = paper_width  * dpi
  dpi_ph     = paper_height * dpi
  dpi_margin = paper_margin * dpi

;
; Get paper height/width in dpi units
;
  pw = rgt - lft
  ph = top - bot

  lx = dpi_margin
  ly = dpi_margin

  ux = dpi_pw - dpi_margin
  uy = dpi_ph - dpi_margin

  dw = ux - lx
  dh = uy - ly

;
; Determine orientation, and then calculate device coordinates based
; on this.
; 
  if(lc_orient.eq."portrait".or. \
     (lc_orient.eq."auto".and.(ph / pw).ge.1.0))
;
; If plot is higher than it is wide, then default to portrait if
; orientation is not specified.
;
    lc_orient = "portrait"

    if (ph / pw .gt. dh / dw) then
                                             ; paper height limits size
      ndc2du = dh / ph
    else
      ndc2du = dw / pw
    end if
;
; Compute device coordinates.
;
    lx = dpi_margin + 0.5 * ( dw - pw * ndc2du) - lft * ndc2du
    ly = dpi_margin + 0.5 * ( dh - ph * ndc2du) - bot * ndc2du
    ux = lx + ndc2du
    uy = ly + ndc2du
  else
;
; If plot is wider than it is high, then default to landscape if
; orientation is not specified.
;
    lc_orient = "landscape"
    if (pw / ph .gt. dh / dw) then
                                             ; paper height limits size
      ndc2du = dh / pw
    else
      ndc2du = dw / ph
    end if

;
; Compute device coordinates.
;
    ly = dpi_margin + 0.5 * (dh - pw * ndc2du) - (1.0 - rgt) * ndc2du
    lx = dpi_margin + 0.5 * (dw - ph * ndc2du) - bot * ndc2du
    ux = lx + ndc2du
    uy = ly + ndc2du
  end if

;
; Return device coordinates and the orientation.
;
  coords = tointeger((/lx,ly,ux,uy/))
  coords@gsnPaperOrientation = lc_orient
;
; Debug prints.
;
  if(is_debug)
   print("-------Device coordinates for PostScript-------")
   print("    wkDeviceLowerX = " + coords(0))
   print("    wkDeviceLowerY = " + coords(1))
   print("    wkDeviceUpperX = " + coords(2))
   print("    wkDeviceUpperY = " + coords(3))
   print("    wkOrientation  = " + coords@gsnPaperOrientation)
  end if

  return(coords)
end


;***********************************************************************;
; Procedure : draw_and_frame                                            ;
;                wks:graphic                                            ;
;               plot:graphic                                            ;
;           calldraw:logical                                            ;
;          callframe:logical                                            ;
;              maxbb:logical                                            ;
;                                                                       ;
; By default, all of the plotting routines will draw the plot and       ;
; advance the frame, unless the special resources gsnDraw and/or        ;
; gsnFrame are set to False. This procedure checks if these resources   ;
; had been set, and calls draw and/or frame accordingly.                ;
; If maxbb is True, then the plot is maximized in the NCGM, X11, or PS  ;
; window.                                                               ; 
;***********************************************************************;
procedure draw_and_frame(wks:graphic,plot:graphic,calldraw:logical, \
                         callframe:logical,maxbb:logical)
begin
  if(maxbb) then
    class = NhlClassName(wks)

    if(class(0).eq."psWorkstationClass") then
;
; Compute device coordinates that will make plot fill the whole page.
;
      coords = compute_ps_device_coords(NhlGetBB(plot),maxbb)
;
; Set these device coordinates.
;
      setvalues wks
        "wkOrientation"  : coords@gsnPaperOrientation
        "wkDeviceLowerX" : coords(0)
        "wkDeviceLowerY" : coords(1)
        "wkDeviceUpperX" : coords(2)
        "wkDeviceUpperY" : coords(3)
      end setvalues
    else
;
; If the workstation is NCGM or X11, then maximize the bounding box.
;
      coords = maximize_bb(plot,maxbb)
      setvalues plot
        "vpXF"      : coords(0)
        "vpYF"      : coords(1)
        "vpWidthF"  : coords(2)
        "vpHeightF" : coords(3)
      end setvalues
    end if
  end if

  if(calldraw)
    draw(plot)
  end if

  if(callframe)
    frame(wks)           ; advance the frame
  end if
end

;***********************************************************************;
; Function : get_bb_res                                                 ;
;               res : list of resources                                 ;
;                                                                       ;
; Get list of resources for use with maximizing the plots within an     ;
; X11, NCGM, or PS window.                                              ;
;***********************************************************************;
function get_bb_res(res:logical)
begin
  maxbb = get_res_value(res,"gsnMaximize", False)
  maxbb@gsnPaperMargin = get_res_value(res,"gsnPaperMargin",0.5)
  maxbb@gsnPaperHeight = get_res_value(res,"gsnPaperHeight",11.0)
  maxbb@gsnPaperWidth  = get_res_value(res,"gsnPaperWidth",8.5)
  maxbb@gsnBoxMargin   = get_res_value(res,"gsnBoxMargin",0.02)
  maxbb@gsnDebug       = get_res_value(res,"gsnDebug",False)
;
; Don't assume a default on this one, because the default will be
; determined by doing a getvalues on the PostScript workstation. 
;
  if(isatt(res,"gsnPaperOrientation"))
    maxbb@gsnPaperOrientation = get_res_value(res,"gsnPaperOrientation","")
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
  if(isatt(res,"gsnPanelBottom"))
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
;               name : name of X11 window                               ;
;                                                                       ;
; This function opens an X11 output window and returns the workstation  ;
; id.                                                                   ;
;***********************************************************************;
function gsn_open_x11(name[1]:string)
local window
begin
    if(isatt(name,"wkColorMap"))
      window = create name + "_x11" xWorkstationClass defaultapp
        "wkPause" : True
        "wkColorMap" : name@wkColorMap
      end create
    else
      window = create name + "_x11" xWorkstationClass defaultapp
        "wkPause" : True
      end create
    end if
    return(window)
end


;***********************************************************************;
; Function : create_labelbar                                            ;
;                wks: graphic                                           ;
;               nbox: integer                                           ;
;             colors: array                                             ;
;             labels: array                                             ;
;              lbres: logical                                           ;
;                                                                       ;
; This function creates a labelbar given a workstation, the number of   ;
; boxes, the colors and labels to use, and an optional list of          ;
; labelbar resources. By default, lbAutoManage is set to False, the     ;
; perimeter is turned off, and the fill patterns are set to solid.      ;
;***********************************************************************;
function create_labelbar(wks:graphic, nbox:integer, colors, labels, \
lbres:logical)
local perim_on, mono_fill_pat, label_align, labelbar_object
begin
;
; Set some defaults
;
  perim_on      = get_res_value(lbres,"lbPerimOn",False)
  mono_fill_pat = get_res_value(lbres,"lbMonoFillPattern",True);
  label_align   = get_res_value(lbres,"lbLabelAlignment","InteriorEdges")
  font_height   = get_res_value(lbres,"lbLabelFontHeightF",0.1)
  orientation   = get_res_value(lbres,"lbOrientation","horizontal")
  vpxf          = get_res_value(lbres,"vpXF",0.1)
  vpyf          = get_res_value(lbres,"vpYF",0.1)
  vpwidthf      = get_res_value(lbres,"vpWidthF",0.8)
  vpheightf     = get_res_value(lbres,"vpHeightF",0.3)

  labelbar_object = create "labelbar" labelBarClass wks
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
    "lbLabelFontHeightF": font_height
    "lbMonoFillPattern" : mono_fill_pat
    "lbAutoManage"      : False
  end create

  attsetvalues_check(labelbar_object,lbres)

  return(labelbar_object)
end

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

    return(ps)
end

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
;
    if(name.ne."") then
      name_char = stringtochar(name)
      name_len  = dimsizes(name_char)-1
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

    x_file = res_file

;
; Check if color map being set.  The PostScript color map will get 
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

    if(type.eq."x11".or.type.eq."X11") then
        wks = gsn_open_x11(x_file)
    else
        if(type.eq."ps".or.type.eq."eps".or.type.eq."epsi".or.\
           type.eq."PS".or.type.eq."EPS".or.type.eq."EPSI") then
            wks = gsn_open_ps(type,res_dir+ps_file)
        else
            if(type.eq."ncgm".or.type.eq."NCGM") then
                wks = gsn_open_ncgm(res_dir+ncgm_file)
            else
                 print("Error: gsn_open_wks: "+ type + " is an illegal workstation type.")
                 exit
            end if
        end if
    end if
    wks@name = res_file
    wks@app = appid 
    return(wks)
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
local i, gsid, plot_object, res, gs_res_index, res2, \
xf, yf, x2, y2
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    gsres = get_res_eq(resources,"gs")
    attsetvalues_check(gsid,gsres)

; Draw a polygon.

    xf = tofloat(x)
    yf = tofloat(y)

    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlDataPolygon(plotid,gsid,xf,yf)
    else
      x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      NhlDataPolygon(plotid,gsid,x2,y2)
    end if
end

;***********************************************************************;
; Function : gsn_add_primitive                                          ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;              polytype: type of primitive                              ;
;               resources: optional resources                           ;
;                                                                       ;
; This function adds a primitive to the plot "plotid", in the same data ;
; space as the data in the plot ("plotid" is returned from a previous   ;
; call to one of the gsn_* plotting functions). "x" and "y" are the x   ;
; and y locations of each point in the primitive, and should be in the  ;
; same data space as the data from "plotid". "resources" is an optional ;
; list of resources.  This function returns the primitive object        ;
; created. polytype is the type of primitive to add (polymarker,        ;
; polygon, or polyline)                                                 ;
;                                                                       ;
;***********************************************************************;
function gsn_add_primitive(wks:graphic,plotid:graphic,x[*]:numeric,\
                           y[*]:numeric,polytype:string,resources:logical)
local gsid, gsres, prim_object, dummy
begin

  if(.not.any(polytype.eq.(/"polymarker","polygon","polyline"/)))
    print("Warning: gsn_add_primitive: Do not recognize primitive type '"+ polytype + "'.")
   return(0)
  end if

; Create a graphic style object.  We have to do this instead of using
; the default one, because if we add two primitive objects to a plot
; and assign each one a different color, the two objects will have the
; same color as the last color that was set.

    gsid = create "graphic_style" graphicStyleClass wks end create
;
; Set graphic style resources, if any.
;
    gsres = get_res_eq(resources,"gs")
    gmres = False
    attsetvalues_check(gsid,gsres)
    if(isatt(gsres,"gsLineColor"))
      gmres = True
      gmres@gsMarkerColor = gsres@gsLineColor
    end if

    if(any(ismissing(x)).or.any(ismissing(y)))
;
; If the primitive is a polymarker or polygon, then just use the
; non-missing values.
;
      if(polytype.eq."polygon".or.polytype.eq."polymarker")
        inds = ind(.not.ismissing(x).and..not.ismissing(y))
        if(.not.any(ismissing(inds)))
          x2 = x(inds)
          y2 = y(inds)
          prim_object = create polytype primitiveClass noparent
            "prXArray"       : x2
            "prYArray"       : y2
            "prPolyType"     : polytype
            "prGraphicStyle" : gsid
          end create
          delete(x2)
          delete(y2)
          delete(inds)
;
; Add primitive to the plot object.
;
          dummy = new(1,graphic)
          NhlAddPrimitive(plotid,prim_object,dummy)
        else
          prim_object = new(1,graphic)
        end if
      else
;
; If the primitive is a polyline, then retrieve the pairs of non-missing
; points, and plot them individually.
;
        dummy = new(1,graphic)
        indices = get_non_missing_pairs(x,y)
        i = 0
;
; Get the number of non-missing pairs of lines.
;
        nlines = dimsizes(ind(.not.ismissing(indices(:,0))))
        if(.not.ismissing(nlines)) 
          prim_object = new(nlines,graphic)
          astring     = new(nlines,string)
          astring     = polytype + ispan(0,nlines-1,1)
          first_marker = True
          do i=0,nlines-1
            ibeg = indices(i,0)
            iend = indices(i,1)
            if(iend.eq.ibeg)
;
; If there's only one point in our line, then indicate it
; with a polymarker.
;
              polytype2 = "polymarker"
              if(first_marker)
                attsetvalues_check(gsid,gmres)
                first_marker = False
              end if
            else
              polytype2 = "polyline"
            end if
            prim_object(i) = create astring(i) primitiveClass noparent
              "prXArray"       : x(ibeg:iend)
              "prYArray"       : y(ibeg:iend)
              "prPolyType"     : polytype2
              "prGraphicStyle" : gsid
            end create
            NhlAddPrimitive(plotid,prim_object(i),dummy)
          end do
        else
          prim_object = new(1,graphic)
        end if
      end if
    else
;
; No data is missing, so create a primitive object.
;
      prim_object = create polytype primitiveClass noparent
        "prXArray"       : x
        "prYArray"       : y
        "prPolyType"     : polytype
        "prGraphicStyle" : gsid
      end create
;
; Add primitive to the plot object.
;
      dummy = new(1,graphic)
      NhlAddPrimitive(plotid,prim_object,dummy)
    end if

    return(prim_object)
end

;***********************************************************************;
; Function : gsn_add_polygon                                            ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This function adds a polygon to the plot "plotid", in the same data   ;
; space as the data in the plot ("plotid" is returned from a previous   ;
; call to one of the gsn_* plotting functions). "x" and "y" are the x   ;
; and y locations of each point in the polygon, and should be in the    ;
; same data space as the data from "plotid". "resources" is an optional ;
; list of resources.  This function returns the primitive object        ;
; created.                                                              ;
;                                                                       ;
; This function is different from gsn_polygon because it actually       ;
; attaches the polygon to the plot. This means that if you resize or    ;
; move the plot, the polygon will stay with the plot.                   ;
;***********************************************************************;
function gsn_add_polygon(wks:graphic,plotid:graphic,x[*]:numeric,\
                         y[*]:numeric,resources:logical)
begin
  return(gsn_add_primitive(wks,plotid,x,y,"polygon",resources))
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
local i, gsid, plot_object, res, gs_res_index, xf, yf, x2, y2
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
    attsetvalues_check(gsid,gsres)

; Draw a polygon.

    xf = tofloat(x)
    yf = tofloat(y)

    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlNDCPolygon(canvas,gsid,xf,yf)
    else
      x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
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
local i, gsid, plot_object, res, gs_res_index
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    gsres = get_res_eq(resources,"gs")
    attsetvalues_check(gsid,gsres)

    gmres = False
;
; If we end up with a line with just one point, then we draw it with
; a polymarker. Thus, we need to make sure the marker will be the
; same color as the line.
; 
    if(isatt(gsres,"gsLineColor"))
      gmres = True
      gmres@gsMarkerColor = gsres@gsLineColor
    end if

;
; Convert data to float, since NhlDataPolyline only takes floats.
;
    xf = tofloat(x)
    yf = tofloat(y)
;
; Since NhlDataPolyline doesn't accept missing values, this routine steps
; through all the points, and only draws the ones that aren't missing. If
; a single point is surrounded by missing values, then a marker is drawn.
;
    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlDataPolyline(plotid,gsid,xf,yf)
    else
      first_marker = True
      indices = get_non_missing_pairs(xf,yf)
      i = 0
      do while(.not.ismissing(indices(i,0)).and.i.lt.dimsizes(xf))
        ibeg = indices(i,0)
        iend = indices(i,1)
        if(iend.gt.ibeg)
          NhlDataPolyline(plotid,gsid,xf(ibeg:iend),yf(ibeg:iend))
        else    ; iend = ibeg --> only one point
           if(first_marker)
             attsetvalues_check(gsid,gmres)
             first_marker = False
           end if
           NhlDataPolymarker(plotid,gsid,xf(ibeg),yf(ibeg))
        end if
        i = i + 1
      end do
    end if
end

;***********************************************************************;
; Function : gsn_add_polyline                                           ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This function adds a polyline to the plot "plotid", in the same data  ;
; space as the data in the plot ("plotid" is returned from a previous   ;
; call to one of the gsn_* plotting functions). "x" and "y" are the x   ;
; and y locations of each point in the line, and should be in the same  ;
; data space as the data from "plotid". "resources" is an optional list ;
; of resources. This function returns the primitive object created.     ;
;                                                                       ;
; This function is different from gsn_polyline because it actually      ;
; attaches the line to the plot. This means that if you resize or move  ;
; the plot, the line will stay with the plot.                           ;
;***********************************************************************;
function gsn_add_polyline(wks:graphic,plotid:graphic,x[*]:numeric,\
                           y[*]:numeric,resources:logical)
begin
  return(gsn_add_primitive(wks,plotid,x,y,"polyline",resources))
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
local gsid, plot_object
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
    attsetvalues_check(gsid,gsres)
;
; Convert data to float, since NhlDataPolyline only takes floats.
;
    xf = tofloat(x)
    yf = tofloat(y)
;
; Since NhlNDCPolyline doesn't accept missing values, this routine steps
; through all the points, and only draws the ones that aren't missing. If
; a single point is surrounded by missing values, then a marker is drawn.
;
    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlNDCPolyline(canvas,gsid,xf,yf)
    else
;
; Since the data contains missing values, loop through each point and only
; draw the non-missing points.
;
      indices = get_non_missing_pairs(xf,yf)
      i = 0
      do while(.not.ismissing(indices(i,0)).and.i.lt.dimsizes(xf))
        ibeg = indices(i,0)
        iend = indices(i,1)
        if(iend.gt.ibeg)
          NhlNDCPolyline(canvas,gsid,xf(ibeg:iend),yf(ibeg:iend))
        else    ; iend = ibeg --> only one point
          NhlNDCPolymarker(canvas,gsid,xf(ibeg),yf(ibeg))
        end if
        i = i + 1
      end do
    end if
end

;***********************************************************************;
; Procedure : gsn_polymarker                                            ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws polymarkers on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks") in the same ;
; data space as the data in "plotid" (returned from a previous call to  ;
; one of the gsn_* plotting functions). "x" and "y" are the x and y     ;
; locations of each marker, and should be in the same data space as the ;
; data from "plotid". "resources" is an optional list of resources.     ;
;***********************************************************************;
procedure gsn_polymarker(wks:graphic,plotid:graphic,x[*]:numeric,\
                         y[*]:numeric,resources:logical)
local i, gsid, plot_object, res, xf, yf, x2, y2
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    gsres = get_res_eq(resources,"gs")
    attsetvalues_check(gsid,gsres)
;
; Make sure data is float, since NhlDataPolymarker only takes floats.
;
    xf = tofloat(x)
    yf = tofloat(y)
;
; Since NhlDataPolymarker doesn't accept missing values, this routine
; only draws the ones that aren't missing.
;
    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlDataPolymarker(plotid,gsid,xf,yf)
    else
      x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      NhlDataPolymarker(plotid,gsid,x2,y2)
    end if
end

;***********************************************************************;
; Function : gsn_add_polymarker                                         ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This function adds polymarkers to the plot "plotid", in the same      ;
; data space as the data in the plot ("plotid" is returned from a       ;
; previous call to one of the gsn_* plotting functions). "x" and "y" are;
; the x and y locations of each marker, and should be in the same data  ;
; space as the data from "plotid". "resources" is an optional list of   ;
; resources. This function returns the primitive object created.        ;
;                                                                       ;
; This function is different from gsn_polymarker because it actually    ;
; attaches the markers to the plot. This means that if you resize or    ;
; move the plot, the markers will stay with the plot.                   ;
;***********************************************************************;
function gsn_add_polymarker(wks:graphic,plotid:graphic,x[*]:numeric,\
                            y[*]:numeric,resources:logical)
begin
  return(gsn_add_primitive(wks,plotid,x,y,"polymarker",resources))
end


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
procedure gsn_polymarker_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                             resources:logical)
local i, gsid, plot_object, res, xf, yf, x2, y2
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
    attsetvalues_check(gsid,gsres)

; Make sure data is float.
    xf = tofloat(x)
    yf = tofloat(y)

; Draw some polymarkers.
    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlNDCPolymarker(canvas,gsid,xf,yf)
    else
      x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      NhlNDCPolymarker(canvas,gsid,x2,y2)
    end if
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
                           x,y,resources:logical )
local i, lbid, res, lb_res_index, lbres
begin
    wksname = get_res_value_keep(wks,"name","gsnapp")

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
      attsetvalues_check(lbid,lbres)
    end if

; Draw labelbar.

    draw(lbid)
    delete(lbid)
end

;***********************************************************************;
; Function : gsn_text_ndc_return                                        ;
;                   wks: workstation object                             ;
;                  text: array of text strings                          ;
;                     x: 1-dimensional array of x ndc positions         ;
;                     y: 1-dimensional array of y ndc positions         ;
;               resources: optional resources                           ;
;                                                                       ;
; This function draws text strings on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks").  "x" and   ;
; "y" are the x and y locations of each text string, and should be      ;
; specified in NDC space. "resources" is an optional list of resources. ;
; This function returns the text string created.                        ;
;***********************************************************************;
function gsn_text_ndc_return(wks:graphic, text:string, x:numeric, \
                             y:numeric, resources:logical )
local i, txid, plot_object, res, tx_res_index, x2, y2, res2, \
calldraw, callframe
begin

    res2 = resources

    wksname = get_res_value_keep(wks,"name","gsnapp")

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",False)
    maxbb     = get_bb_res(res2)

    if((res2).and.isatt(res2,"txFuncCode")) then
      txid = create wksname + "_text_ndc" textItemClass wks
        "txString" : text
        "txPosXF"  : x
        "txPosYF"  : y
        "txFuncCode" : res2@txFuncCode
      end create
    else
      txid = create wksname + "_text_ndc" textItemClass wks
        "txString" : text
        "txPosXF"  : x
        "txPosYF"  : y
      end create
    end if

    txres = get_res_eq(res2,"tx")  ; Get text resources.
    attsetvalues_check(txid,txres)            ; Set text resources.

    draw_and_frame(wks,txid,calldraw,callframe,maxbb)
    return(txid)
end

;***********************************************************************;
; Procedure : gsn_text_ndc                                              ;
;                                                                       ;
; This procedure is the same as gsn_text, only it doesn't return        ;
; anything.                                                             ;
;***********************************************************************;
procedure gsn_text_ndc(wks:graphic, text:string, x:numeric, \
                       y:numeric, resources:logical )
local txid
begin
    txid = gsn_text_ndc_return(wks,text,x,y,resources)
end

;***********************************************************************;
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
procedure gsn_text(wks:graphic,plotid:graphic,text:string,x:numeric, \
                   y:numeric, resources:logical )
local i, txid, plot_object, res, tx_res_index, x2, y2, xf, yf, \
funccode, res2, calldraw, callframe
begin
    res2 = resources

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",False)
    maxbb     = get_bb_res(res2)
;
; datatondc can't accept doubles, so have to demote doubles if they
; come in.
;
    xf = tofloat(x)
    yf = tofloat(y)

    x2 = new(dimsizes(x),float)
    y2 = new(dimsizes(y),float)

    datatondc(plotid,xf,yf,x2,y2)

    delete(xf)
    delete(yf)
;
; The "txFuncCode" can't be set during a setvalues  call. It must be
; set during the creation of the object.  
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    if(res2.and.isatt(res2,"txFuncCode")) then
      txid = create wksname + "_text" textItemClass wks
          "txString"   : text
          "txPosXF"    : x2
          "txPosYF"    : y2
          "txFuncCode" : res2@txFuncCode
      end create
    else
      txid = create wksname + "_text" textItemClass wks
          "txString"   : text
          "txPosXF"    : x2
          "txPosYF"    : y2
      end create
    end if

    txres = get_res_eq(res2,"tx")  ; Get text resources.
    attsetvalues_check(txid,txres)            ; Set text resources.

    draw_and_frame(wks,txid,calldraw,callframe,maxbb)
end

;***********************************************************************;
; Function : gsn_add_text                                               ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                  text: array of text strings                          ;
;                     x: 1-dimensional array of x data positions        ;
;                     y: 1-dimensional array of y data positions        ;
;               resources: optional resources                           ;
;                                                                       ;
; This function adds text strings to the plot "plotid". "x" and "y" are ;
; the x and y locations of each text string, and should be specified in ;
; the same data space as the data space of "plotid". "resources" is an  ;
; optional list of TextItem and AnnoManager resources.                  ;
;                                                                       ;
; This function is different from gsn_text because it actually attaches ;
; the text to the plot. This means that if you resize or move the plot, ;
; the text will stay with the plot.                                     ;
;***********************************************************************;
function gsn_add_text(wks:graphic,plotid:graphic,text:string,x:numeric, \
                      y:numeric, resources:logical )
local txid, txres, amres, just, res2, wksname, am_ids
begin
  res2 = resources
;
; The "txFuncCode" can't be set during a setvalues  call. It must be
; set during the creation of the object.  
;
  wksname = get_res_value_keep(wks,"name","gsnapp")

  if(res2.and.isatt(res2,"txFuncCode")) then
    txid = create wksname + "_text" textItemClass wks
      "txString"   : text
      "txFuncCode" : res2@txFuncCode
    end create
  else
    txid = create wksname + "_text" textItemClass wks
      "txString"   : text
    end create
  end if

  txres = get_res_eq(res2,"tx")  ; Get text resources.
  attsetvalues_check(txid,txres)            ; Set text resources.

  getvalues plotid
    "pmAnnoViews" : text_ids
  end getvalues

  if(.not.any(ismissing(text_ids)))
    new_text_ids = new(dimsizes(text_ids)+1,graphic)
    new_text_ids(0)  = txid
    new_text_ids(1:) = text_ids
  else
    new_text_ids = txid
  end if

  setvalues plotid
    "pmAnnoViews" : new_text_ids
  end setvalues
;
; Retrieve the id of the AnnoManager object created by the PlotManager and
; then set its location in data coordinate space.
;
  getvalues plotid
    "pmAnnoManagers": am_ids
  end getvalues

  tmp_just  = get_res_value(txres,"txJust","CenterCenter")
  just  = get_res_value(res2,"amJust",tmp_just)

  setvalues am_ids(0)
    "amDataXF"       : x
    "amDataYF"       : y
    "amResizeNotify" : True
    "amTrackData"    : True
    "amJust"         : just
  end setvalues

  amres = get_res_eq(res2,"am")  ; Get annomanager resources.
  attsetvalues_check(am_ids(0),amres)            ; Set text resources.

  return(am_ids(0))
end

;***********************************************************************;
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
;                                                                       ;
; However, if you set gsnPanelRowSpec to True, and dims to an array of  ;
; integers, then each integer will represent the number of plots in that;
; row.  For example, setting gsnPanelRowSpec = (/2,3,1/) will cause     ;
; there to be two plots in the first row, three in the second row, and  ;
; one in the third row.                                                 ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnPanelCenter                                                      ;
;   gsnPanelLabelBar                                                    ;
;   gsnPanelRowSpec                                                     ;
;   gsnPanelXWhiteSpacePercent                                          ;
;   gsnPanelYWhiteSpacePercent                                          ;
;   gsnPanelBoxes                                                       ;
;   gsnPanelLeft                                                        ;
;   gsnPanelRight                                                       ;
;   gsnPanelBottom                                                      ;
;   gsnPanelTop                                                         ;
;   gsnPanelSave                                                        ;
;   gsnDraw                                                             ;
;                                                                       ;
;***********************************************************************;
function gsn_panel_return(wks:graphic,plot[*]:graphic,dims[*]:integer,\
                          resources:logical)
local res, nrows, ncols, ddims, is_row_spec, row_spec, npanels, nplots
begin
  res = resources              ; Make copy of resources

  ddims = dimsizes(dims)
;
; First check if paneling is to be specified by (#rows x #columns) or
; by #columns per row.  The default is rows x columns, unless 
; resource gsnPanelRowSpec is set to True
; 
  is_row_spec = get_res_value(res,"gsnPanelRowSpec",False)
;
; Check to see if we have enough plots to fit panels, and vice versa.
;
  if(is_row_spec)
    row_spec = dims
    npanels = 0
    nrows = ddims
    ncols = max(row_spec)
    do i=0,nrows-1
      if(row_spec(i).lt.0)
        print("Error: gsn_panel: you have specified a negative value for the number of plots in a row.")
        exit
      end if
      npanels = npanels + row_spec(i)
    end do
  else
    if(ddims.ne.2)
      print("Error: gsn_panel: for the third argument of gsn_panel, you must either specify # rows by # columns or set gsnPanelRowSpec to True and set the number of plots per row.")
      exit
    end if
    nrows = dims(0)
    ncols = dims(1)
    npanels = nrows * ncols
    row_spec = new(nrows,integer)
    row_spec = ncols
  end if
 
  nplots  = dimsizes(plot)      ; Total number of plots.

  if(nplots.gt.npanels)
    print("Warning: gsn_panel: you have more plots than you have panels.")
    print("Only " + npanels + " plots will be drawn.")
    nplots = npanels
  end if

;
; Check for special resources.
; 
  panel_save     = get_res_value_keep(res,"gsnPanelSave",True)
  panel_debug    = get_res_value_keep(res,"gsnDebug",False)
  panel_center   = get_res_value_keep(res,"gsnPanelCenter",True)
  panel_labelbar = get_res_value_keep(res,"gsnPanelLabelBar",False)
  calldraw       = get_res_value_keep(res,"gsnDraw",True)
  callframe      = get_res_value_keep(res,"gsnFrame",True)
  xwsp_perc      = get_res_value_keep(res,"gsnPanelXWhiteSpacePercent",1.)
  ywsp_perc      = get_res_value_keep(res,"gsnPanelYWhiteSpacePercent",1.)
  draw_boxes     = get_res_value_keep(res,"gsnPanelBoxes",False)
  x_lft          = get_res_value_keep(res,"gsnPanelLeft",0.)
  x_rgt          = get_res_value_keep(res,"gsnPanelRight",1.)
  y_bot          = get_res_value_keep(res,"gsnPanelBottom",0.)
  y_top          = get_res_value_keep(res,"gsnPanelTop",1.)
  main_string    = get_res_value_keep(res,"txString","")
  maxbb          = get_bb_res(res)
;
; Check if a main string has been specified. If so, we need to make sure
; we leave some room for it by computing y_top (if the user hasn't set
; it). Also, we have to check if the font height has been set, because
; this could affect the title position.
; 
  if(main_string.ne."") then
    main_string_on = True
    main_font_hgt  = get_res_value_keep(res,"txFontHeightF",0.02)
;
; By default, we want a distance of 0.01 between top of title and the
; frame, and a distance of 0.03  between the bottom of the title (txPosYF)
; and the top of the panel box (gsnPanelTop).
;
    if(y_top.eq.1.) then
      if(isatt(res,"txPosYF"))
        y_top = min((/1.,res@txPosYF - 0.03/))
      else
        y_top = min((/1.,0.96-main_font_hgt/))
      end if
    end if
  else
    main_string_on = False
  end if
;
; Calculate number of plot objects that will actually be drawn.
; (Panel plots plus labelbar and main string, if any.)
;
  nnewplots = nplots
  if(panel_labelbar) then
    nnewplots = nnewplots + 1
  end if
  if(main_string_on) then
    nnewplots = nnewplots + 1
  end if

  newplots = new(nnewplots,graphic)   ; Create array to save these plots
                                      ; objects.
;
; We only need to set maxbb to True if the plots are being drawn to
; a PostScript workstation, because the bounding box is already
; maximized for an NCGM/X11 window.
; 
  if(maxbb) then
    class = NhlClassName(wks)
    if(class(0).ne."psWorkstationClass") then
      maxbb = False
    end if
    delete(class)
  end if
;
; Get some resources for the figure strings, if they exist.
;
  if(isatt(res,"gsnPanelFigureStrings"))
    is_figure_strings = True
    panel_strings = get_res_value(res,"gsnPanelFigureStrings","")      
;
; Get and set resource values for figure strings on the plots.
;
    justs  = (/"BottomRight", "TopRight", "TopLeft", "BottomLeft"/)
    ljusts = (/"bottomright", "topright", "topleft", "bottomleft"/)
    ujusts = (/"BOTTOMRIGHT", "TOPRIGHT", "TOPLEFT", "BOTTOMLEFT"/)
    paras = (/     1.0,           1.0,      -1.0,       -1.0/)
    orths = (/     1.0,          -1.0,      -1.0,        1.0/)

    amres = get_res_eq(res,"am")
    just  = get_res_value(amres,"amJust","BottomRight")
;
; Don't look for tx* resources, because this will conflict with the
; title string which is also set with tx* resources. We have to figure
; out how to handle this later.
;
;    txres = get_res_eq(res,"tx")
;    perim = get_res_value(txres,"txPerimOn",True)
;    bkgrn = get_res_value(txres,"txBackgroundFillColor",0)
  else
    is_figure_strings = False
  end if

  if(xwsp_perc.lt.0.or.xwsp_perc.ge.100.)
    print("Warning: gsn_panel: attribute gsnPanelXWhiteSpacePercent must be >= 0 and < 100.")
    print("Defaulting to 1.")
    xwsp_perc = 1.
  end if

  if(ywsp_perc.lt.0.or.ywsp_perc.ge.100.)
    print("Warning: gsn_panel: attribute gsnPanelYWhiteSpacePercent must be >= 0 and < 100.")
    print("Defaulting to 1.")
    ywsp_perc = 1.
  end if

  if(x_lft.lt.0..or.x_lft.ge.1.)
    print("Warning: gsn_panel: attribute gsnPanelLeft must be >= 0.0 and < 1.0")
    print("Defaulting to 0.")
    x_lft = 0.0
  end if

  if(x_rgt.le.0..or.x_rgt.gt.1.)
    print("Warning: gsn_panel: attribute gsnPanelRight must be > 0.0 and <= 1.0")
    print("Defaulting to 1.")
    x_rgt = 1.0
  end if

  if(y_top.le.0..or.y_top.gt.1.)
    print("Warning: gsn_panel: attribute gsnPanelTop must be > 0.0 and <= 1.0")
    print("Defaulting to 1.")
    y_top = 1.0
  end if

  if(y_bot.lt.0..or.y_bot.ge.1.)
    print("Warning: gsn_panel: attribute gsnPanelBottom must be >= 0.0 and < 1.0")
    print("Defaulting to 0.")
    y_bot = 0.0
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
;
; Get the type of plots we have. "plot" can be a map, in which case
; the vector or contour plot overlaid
; on it will be indicated by "plot@contour" or "plot@vector"
;
  plot_type = "unknown"
  class = NhlClassName(plot)

  if(class(0).ne."contourPlotClass".and.class(0).ne."vectorPlotClass".and. \
     class(0).ne."xyPlotClass") then
    if(isatt(plot,"contour")) then
      contour_plot = plot@contour
      plot_type = "contour"
    else
      if(isatt(plot,"vector")) then
        vector_plot = plot@vector
        plot_type = "vector"
      end if
    end if
  else
    if(class(0).eq."contourPlotClass") then
      contour_plot = plot(0)
      plot_type = "contour"
    else
      if(class(0).eq."vectorPlotClass") then
        vector_plot = plot(0)
        plot_type = "vector"
      else
        if(class(0).eq."xyPlotClass") then
          xy_plot = plot(0)
          plot_type = "xy"
        end if
      end if
    end if
  end if

;
; Get the font height
;
  if(is_figure_strings.or.panel_labelbar) then
    if(plot_type.eq."contour") then
      getvalues contour_plot
        "cnInfoLabelFontHeightF" : font_height
      end getvalues
    else
      if(plot_type.eq."vector") then
        getvalues vector_plot
          "vcRefAnnoFontHeightF" : font_height
        end getvalues
      else
        if(plot_type.eq."xy") then
          getvalues xy_plot
            "tiXAxisFontHeightF" : font_height
          end getvalues
          font_height = 0.6*font_height
        else
          font_height = 0.01
          print("Warning: gsn_panel: unrecognized plot type, thus unable to get information for font height.")
          print("Defaulting to " + font_height)
        end if
      end if
    end if
  end if
;
; We assume all plots are the same size, so if we get the size of
; the first one, this should be the size of all of them.
;
  bb = NhlGetBB(plot(0))   ; Get bounding box of plot with
  top    = bb(0)           ; all of its annotations.
  bottom = bb(1)
  left   = bb(2)
  right  = bb(3)
;
; plot_width  : total width of plot with all of its annotations
; plot_height : total height of plot with all of its annotations
; total_width : plot_width plus white space on both sides
; total_height: plot_height plus white space on top and bottom
;
  plot_width  = right - left     ; Calculate total width of plot.
  plot_height = top - bottom     ; Calculate total height of plot.

  xwsp = xwsp_perc/100. * plot_width  ; White space is a percentage of total
  ywsp = ywsp_perc/100. * plot_height ; width and height.

  total_width  = 2.*xwsp + plot_width   ; Calculate total width and height
  total_height = 2.*ywsp + plot_height  ; with white space added.
;
; If we are putting a global labelbar at the bottom (right), make 
; it 2/10 the height (width) of the plot.
;
  lbhor = True
  if(panel_labelbar) then
    lbres = get_res_eq(res,(/"lb","vp"/))  ; Get labelbar resources.
    if(check_attr(lbres,"lbOrientation","vertical",True))
      lbhor = False
      labelbar_width = 0.20 * plot_width + 2.*xwsp
;
; Adjust height depending on whether we have one row or multiple rows.
;
      if(nplots.gt.1.and.nrows.gt.1) then
        labelbar_height  = (nrows-1) * (2.*ywsp + plot_height)
      else
        labelbar_height  = plot_height
      end if
    else         
      set_attr(lbres,"lbOrientation","Horizontal")

      labelbar_height = 0.20 * plot_height + 2.*ywsp
;
; Adjust width depending on whether we have one column or multiple 
; columns.
;
      if(nplots.gt.1.and.ncols.gt.1) then
        labelbar_width  = (ncols-1) * (2.*xwsp + plot_width)
      else
        labelbar_width  = plot_width
      end if
    end if
  else
    labelbar_height = 0.
    labelbar_width  = 0.
  end if
;
; We want:
;
;   ncols * scale * total_width  <= x_rgt - x_lft (the viewport width)
;   nrows * scale * total_height <= y_top - y_bot (the viewport height)
;   [or scale * (nrows * total_height + labelbar_height) if a labelbar
;    is being drawn]
;
; By taking the minimum of these two, we get the scale
; factor that we need to fit all plots on a page.
;
  xrange = x_rgt - x_lft
  yrange = y_top - y_bot

  if(lbhor)
    row_scale = min((/yrange/(nrows*total_height+labelbar_height),yrange/))
    col_scale = min((/xrange/(ncols*total_width), xrange/))
    scale = min((/col_scale,row_scale/))
    yrange = yrange - scale * labelbar_height
  else
    row_scale = min((/yrange/(nrows*total_height),yrange/))
    col_scale = min((/xrange/(ncols*total_width+labelbar_width), xrange/))
    scale = min((/col_scale,row_scale/))
    xrange = xrange - scale * labelbar_width
  end if

  new_plot_width  = scale*plot_width    ; Calculate new width
  new_plot_height = scale*plot_height   ; and height.

  xwsp = xwsp_perc/100. * new_plot_width   ; Calculate new white space.
  ywsp = ywsp_perc/100. * new_plot_height

  new_total_width  = 2.*xwsp + new_plot_width  ; Calculate new total width
  new_total_height = 2.*ywsp + new_plot_height ; and height w/white space.

  xsp = xrange - new_total_width*ncols  ; Calculate total amt of white space
  ysp = yrange - new_total_height*nrows ; left in both X and Y directions.

  getvalues plot(0)
    "vpXF"      : vpx
    "vpYF"      : vpy
    "vpWidthF"  : vpw
    "vpHeightF" : vph
  end getvalues

  dx = scale * (vpx - left) ; Calculate distance from plot's left position
                            ; to its leftmost annotation
  dy = scale * (top - vpy) ; Calculate distance from plot's top position
                           ; to its topmost annotation.

  ypos = y_top - ywsp - dy -(ysp/2.+new_total_height*ispan(0,nrows-1,1))
;
; If we have figure strings, then determine white spacing around 
; the text box.
;
  if(is_figure_strings) then
    fig_index = ind(just.eq.justs.or.just.eq.ljusts.or.just.eq.ujusts)
    if(ismissing(fig_index))
      fig_index = 0
      just      = justs(fig_index)
    end if

    len_pct = 0.025           ; Percentage of width/height of plot
                                ; for white space around text box.
    if(vpw .lt. vph) then
      wsp_hpct = (len_pct * vpw) / vph
      wsp_wpct = len_pct
    else
      wsp_hpct = len_pct
      wsp_wpct = (len_pct * vph) / vpw
    end if
    para  = get_res_value(amres,"amParallelPosF",  paras(fig_index) * \
                                                   (0.5 - wsp_wpct))
    orth  = get_res_value(amres,"amOrthogonalPosF", orths(fig_index) * \
                                                    (0.5 - wsp_hpct))
  end if
;
; Variable to store rightmost location of rightmost plot, and topmost
; location of top plot.
;
  max_rgt = 0.
  max_top = 0.
;
; Variable to hold original viewport coordinates, and annotations (if
; they exist).
;
  old_vp = new((/nplots,4/),float)
  anno   = new(nplots, graphic)
;
; Loop through each row and create each plot in the new scaled-down
; size. We will draw plots later, outside the loop.
;
  num_plots_left = nplots
  nplot          = 0
  nr             = 0
  do while(num_plots_left.gt.0)
    new_ncols = min((/num_plots_left,row_spec(nr)/))

    if(panel_center)
      xsp = xrange - new_total_width*new_ncols  ; space before plots. 
    else
      xsp = xrange - new_total_width*ncols      ; space before plots. 
    end if
;
; Calculate new x positions.
;
    xpos = x_lft + xwsp + dx +(xsp/2.+new_total_width*ispan(0,new_ncols-1,1))

    do nc = 0,new_ncols-1
      if(.not.ismissing(plot(nplot)))
        pplot = plot(nplot)
        getvalues pplot
          "vpXF"      : old_vp(nplot,0)
          "vpYF"      : old_vp(nplot,1)
          "vpWidthF"  : old_vp(nplot,2)
          "vpHeightF" : old_vp(nplot,3)
        end getvalues

        if(panel_debug) then
          print("-------Panel viewport values for each plot-------")
          print("    plot #" + nplot)
          print("    x,y     = " + xpos(nc) + "," + ypos(nr))
          print("    wdt,hgt = " + scale*old_vp(nplot,2) + "," + scale*old_vp(nplot,3))
        end if

        setvalues pplot
          "vpXF"      : xpos(nc)
          "vpYF"      : ypos(nr)
          "vpWidthF"  : scale*old_vp(nplot,2)
          "vpHeightF" : scale*old_vp(nplot,3)
        end setvalues

        added_anno = False
        if(is_figure_strings) then
          if(nplot .lt. dimsizes(panel_strings).and. \
            panel_strings(nplot).ne."")
            text = create "string" textItemClass wks
              "txString"              : panel_strings(nplot)
              "txFontHeightF"         : font_height
              "txPerimOn"             : True
              "txBackgroundFillColor" : 0
            end create
;
; See note above about not checking for tx* resources here.
;
;            attsetvalues_check(text,txres)
            anno(nplot) = NhlAddAnnotation(pplot,text)
            added_anno = True
            setvalues anno(nplot)
              "amZone"           : 0
              "amJust"           : just
              "amParallelPosF"   : para
              "amOrthogonalPosF" : orth
              "amResizeNotify"   : True
            end setvalues
            attsetvalues_check(anno(nplot),amres)
            delete(text)
          end if
        end if
;
; Save this plot.
;
        newplots(nplot) = pplot
;
          max_rgt = max((/rgt,max_rgt/))
          max_top = max((/top,max_top/))

          if(draw_boxes)
            gsres = True
            gsres@gsLineThicknessF = 5.0
            gsn_polyline_ndc(wks,(/lft,rgt,rgt,lft,lft/), \
                                 (/bot,bot,top,top,bot/),gsres)
          end if
        end if
      end if
      nplot = nplot + 1
    end do  ; end of columns
;
; Retain the smallest x position.
;
    if(nr.eq.0)
      min_xpos = min(xpos)
    else
      min_xpos = min( (/min(xpos),min_xpos/) )
    end if
    delete(xpos)
    num_plots_left = nplots - nplot
    nr = nr + 1   ; increment rows
  end do    ; end of plots
;
; Check if a labelbar is to be drawn at the bottom.
;
  if(panel_labelbar) then
;
; If plot type is unknown or xy, then we can't get labelbar information.
;
    if(plot_type.ne."unknown".and.plot_type.ne."xy") then
      if(plot_type.eq."contour") then
;
; Get information on how contour plot is filled, so we can recreate 
; labelbar.
;
        getvalues contour_plot
          "cnLevels"               : levels
          "cnFillColors"           : colors
          "cnFillPatterns"         : fill_patterns
          "cnFillScales"           : fill_scales
          "cnMonoFillPattern"      : mono_fill_pat
          "cnMonoFillScale"        : mono_fill_scl
          "cnMonoFillColor"        : mono_fill_col
        end getvalues
      else
;
; There are no fill patterns in VectorPlot, only solids.
;
        mono_fill_pat = True
        mono_fill_scl = True
        mono_fill_col = False
        getvalues vector_plot
          "vcLevels"                 : levels
          "vcLevelColors"            : colors
          set_attr(lbres,"vpXF",xpos(0)+(scale*max(old_vp(:,2)) - lbres@vpWidthF)/2.)
      end if
          set_attr(lbres,"vpXF",(1. - lbres@vpWidthF)/2.)
;
      labelbar_height      = scale * labelbar_height
      labelbar_width       = scale * labelbar_width
      labelbar_font_height = font_height
;
; Set some labelbar resources.
          set_attr(lbres,"vpYF",1.-(1. - lbres@vpHeightF)/2.)
      set_attr(lbres,"vpWidthF", labelbar_width)
      set_attr(lbres,"vpHeightF",labelbar_height)
;
; Set position of labelbar depending on whether it's horizontal or
; vertical.
;
      if(lbhor)
        set_attr(lbres,"vpYF",max ((/ywsp+labelbar_height,bot-ywsp/)))
        if(ncols.eq.1.and.lbres@vpWidthF.le.scale*max(old_vp(:,2)))
          set_attr(lbres,"vpXF",min_xpos+(scale*max(old_vp(:,2)) - lbres@vpWidthF)/2.)
        else
          tmp_range = x_rgt - x_lft
          set_attr(lbres,"vpXF",x_lft + (tmp_range - lbres@vpWidthF)/2.)
        end if
      else
        set_attr(lbres,"vpXF",min ((/1.-(xwsp+labelbar_width),max_rgt+xwsp/)))
        if(nrows.eq.1.and.lbres@vpHeightF.le.scale*max(old_vp(:,3)))
; Create the labelbar.
          set_attr(lbres,"vpYF",y_top-(tmp_range - lbres@vpHeightF)/2.)
      end if
      set_attr(lbres,"lbLabelFontHeightF",labelbar_font_height)
;
; Check if we want different fill patterns or fill scales.  If so, we
; have to pass these on to the labelbar.
;
      set_attr(lbres,"lbMonoFillColor",mono_fill_col)
      if(.not.mono_fill_pat)
        set_attr(lbres,"lbMonoFillPattern", False)
        set_attr(lbres,"lbFillPatterns",    fill_patterns)
      end if
      if(.not.mono_fill_scl)
        set_attr(lbres,"lbMonoFillScale", False)
        set_attr(lbres,"lbFillScales",    fill_scales)
      end if
;
; Create the labelbar.  First check the levels to make sure that a
; contour level with a value like 1e-8 is not really supposed to be
; a value of 0.
;
      fix_zero_contour(levels)
      newplots(nplot) = create_labelbar(wks,dimsizes(colors),colors, \
                                         levels,lbres)
      nplot = nplot + 1
    else
       print("Warning: gsn_panel: unrecognized plot type for getting labelbar information. Ignoring labelbar request.")
    end if
  end if
;
; Create the main string, if exists.
;
  if(main_string_on) then
    y_top     = min((/y_top,max_top/))
    main_ypos = get_res_value_keep(res,"txPosYF",y_top + 0.03)
    main_xpos = get_res_value_keep(res,"txPosXF",0.5)

    if(panel_debug) 
      print("-------Panel title values-------")
      print("    title                = " + main_string)
      print("    top of paneled plots = " + y_top)
      print("    y location of title  = " + main_ypos)
    end if

    if((main_ypos+main_font_hgt).gt.1)
       print("Warning: gsn_panel: font height (" + main_font_hgt + ") of main string is too large to fit in space provided. Either decrease font size or set gsnPanelTop.")
    end if

    txres               = get_res_eq(res,"tx")
    txres               = True
    txres@gsnDraw       = False
    txres@gsnFrame      = False
    txres@txFontHeightF = main_font_hgt
    newplots(nplot) = gsn_text_ndc_return(wks, main_string, main_xpos, \
                      main_ypos, txres)
  end if
;
; Draw plots plus labelbar and main title (if they exists). This is
; also where the plots will be maximized for PostScript output,
; if so indicated.
;
  draw_and_frame(wks,newplots,calldraw,callframe,maxbb)
;
; Draw bounding boxes around each plot object for debugging purposes.
;
  if(draw_boxes)
    gsres = True
    gsres@gsLineThicknessF = 3.0
    do i=0,dimsizes(newplots)-1
      if(.not.ismissing(newplots(i)))
        bb  = NhlGetBB(newplots(i)) ; Get bounding box of plot
        top = bb(0)
        lft = bb(2)
        bot = bb(1)
        rgt = bb(3)

        gsn_polyline_ndc(wks,(/lft,rgt,rgt,lft,lft/), \
                             (/bot,bot,top,top,bot/),gsres)
      end if
    end do
  end if
;
; Restore plots to original size.
;
  if(.not.panel_save) then
    do i=0,nplots-1
      if(.not.ismissing(plot(i)))
        if(added_anno.and..not.ismissing(anno(i)))
          NhlRemoveAnnotation(plot(i),anno(i))
        end if
        setvalues plot(i)
          "vpXF"      : old_vp(i,0)
          "vpYF"      : old_vp(i,1)
          "vpWidthF"  : old_vp(i,2)
          "vpHeightF" : old_vp(i,3)
        end setvalues
      end if
    end do
  end if

  return(newplots)
end


;***********************************************************************;
; procedure gsn_panel - same as gsn_panel_return, only it doesn't return;
;                       anything.                                       ;
;***********************************************************************;
procedure gsn_panel(wks:graphic,plot[*]:graphic,dims[*]:integer,\
                    resources:logical)
local res2
begin
  res2 = resources
  set_attr(res2,"gsnPanelSave",False )
  plots = gsn_panel_return(wks,plot,dims,res2)
end

;***********************************************************************;
; Function : gsn_attach_plots                                           ;
;                  plot1 : first plot                                   ;
;                  plot2 : second plot                                  ;
;               resplot1 : logical                                      ;
;               resplot2 : logical                                      ;
;                                                                       ;
; This function attaches plot2 to plot1 either on the right Y axis or   ;
; bottom X axis of plot1. The default is to attach the plots at the Y   ;
; axis, unless gsnAttachPlotsXAxis is set to True.                      ;
;                                                                       ;
; By default, the viewport heights of both plots will be made the same, ;
; appropriate tick marks and labels will be turned off, and the aspect  ;
; ratio preserved.                                                      ;
;                                                                       ;
; For example, if you have the following plots and you want them        ;
; attached at the Y axis:                                               ;
;                                                                       ;
;    ___________      _____                                             ;
;    |         |      |   |                                             ;
;    |         |      |   |                                             ;
;    |         |      |   |                                             ;
;    |         |      |   |                                             ;
;    |         |      |   |                                             ;
;    -----------      -----                                             ;
;                                                                       ;
; you will end up with:                                                 ;
;                                                                       ;
;    _______________                                                    ;
;    |         |   |                                                    ;
;    |         |   |                                                    ;
;    |         |   |                                                    ;
;    |         |   |                                                    ;
;    |         |   |                                                    ;
;    ---------------                                                    ;
;                                                                       ;
; Or, if you have the following plots and you want them attached at the ;
; X axis:                                                               ;
;                                                                       ;
;    ___________      ___________                                       ;
;    |         |      |         |                                       ;
;    |         |      |         |                                       ;
;    |         |      |         |                                       ;
;    |         |      -----------                                       ;
;    |         |                                                        ;
;    -----------                                                        ;
;                                                                       ;
; you will end up with:                                                 ;
;                                                                       ;
;    ___________                                                        ;
;    |         |                                                        ;
;    |         |                                                        ;
;    |         |                                                        ;
;    |         |                                                        ;
;    |         |                                                        ;
;    -----------                                                        ;
;    |         |                                                        ;
;    |         |                                                        ;
;    |         |                                                        ;
;    -----------                                                        ;
;                                                                       ;
; plotres1 and plotres2 are resources changing the default behavior of  ;
; this function.                                                        ;
;                                                                       ;
;***********************************************************************;
function gsn_attach_plots(oldplot1:graphic,oldplot2:graphic, \
                          plotres1:logical, plotres2:logical)
local anno, width1, width2, height1, height2, font_height1, font_height2, \
mj_length1, mj_length2, mjo_length1, mjo_length2, mno_length1, mno_length2, \
mno_length1, mno_length2, total_width1, total_width2, scale1, scale2, scale
begin
  res1  = plotres1
  res2  = plotres2
  plot1 = oldplot1
  plot2 = oldplot2

  attach_y = .not.get_res_value(res1,"gsnAttachPlotsXAxis",False)
  attach_y = .not.get_res_value(res2,"gsnAttachPlotsXAxis",.not.attach_y)
;
; Retrieve tickmark lengths and font height labels so we can make
; them the same size later.
;
; Also get the viewport widths and heights so we can maintain the
; aspect ratio, but yet make the heights the same.
;
  getvalues plot1
    "vpWidthF"                : width1
    "vpHeightF"               : height1

    "tiMainFontHeightF"       : main_font_height1
  end getvalues

  getvalues plot2
    "vpWidthF"                : width2
    "vpHeightF"               : height2
  end getvalues

  if(attach_y)
    getvalues plot1
      "tmXBMajorLengthF"        : mj_length1
      "tmXBMajorOutwardLengthF" : mjo_length1
      "tmXBMinorLengthF"        : mn_length1
      "tmXBMinorOutwardLengthF" : mno_length1
      "tmXBLabelFontHeightF"    : font_height1
    end getvalues

    getvalues plot2
      "tmXBMajorLengthF"        : mj_length2
      "tmXBMajorOutwardLengthF" : mjo_length2
      "tmXBMinorLengthF"        : mn_length2
      "tmXBMinorOutwardLengthF" : mno_length2
      "tmXBLabelFontHeightF"    : font_height2
    end getvalues

  else
    getvalues plot1
      "tmYLMajorLengthF"        : mj_length1
      "tmYLMajorOutwardLengthF" : mjo_length1
      "tmYLMinorLengthF"        : mn_length1
      "tmYLMinorOutwardLengthF" : mno_length1
      "tmYLLabelFontHeightF"    : font_height1
    end getvalues

    getvalues plot2
      "tmYLMajorLengthF"        : mj_length2
      "tmYLMajorOutwardLengthF" : mjo_length2
      "tmYLMinorLengthF"        : mn_length2
      "tmYLMinorOutwardLengthF" : mno_length2
      "tmYLLabelFontHeightF"    : font_height2
    end getvalues
  end if

;
; Calculate the scale factor needed to make the plots the same
; size in the appropriate axis.  If we are attaching plots at the Y axis,
; then we want to make them the same height. Otherwise, we want to make
; them the same width.
;
  if(attach_y) then
    if(height1.lt.height2) then
      scale1 = height2/height1
      scale2 = 1.
    else
      scale2 = height1/height2
      scale1 = 1.
    end if
  else 
    if(width1.lt.width2) then
      scale1 = width2/width1
      scale2 = 1.
    else
      scale2 = width1/width2
      scale1 = 1.
    end if
  end if
;
; Because we are attaching two plots right at an axis, turn off
; tickmarks and labels where appropriate.
;
  if(attach_y) then
    setvalues plot1
      "tmYROn"       : get_res_value(res1,"tmYROn",False)
      "tmYUseLeft"   : get_res_value(res1,"tmYUseLeft",False)
    end setvalues

    setvalues plot2
      "tiYAxisOn"    : get_res_value(res2,"tiYAxisOn",False)
      "tmYLOn"       : get_res_value(res2,"tmYLOn",False)
      "tmYUseLeft"   : get_res_value(res2,"tmYUseLeft",False)
    end setvalues
  else
    setvalues plot1
      "tmXBOn"       : get_res_value(res1,"tmXBOn",False)
      "tmXUseBottom" : get_res_value(res1,"tmXUseBottom",False)
      "tiXAxisOn"    : get_res_value(res1,"tiXAxisOn",False)
    end setvalues

    setvalues plot2
      "tmXTOn"       : get_res_value(res2,"tmXTOn",False)
      "tmXUseBottom" : get_res_value(res2,"tmXUseBottom",False)
      "tiMainOn"     : get_res_value(res2,"tiMainOn",False)
    end setvalues
  end if

;
; Now that we've turned off the necessary tickmark stuff, 
; retrieve the bounding box of each plot.
;
  bb1 = NhlGetBB(plot1) ; Get bounding box of plot
  top1 = bb1(0)
  bot1 = bb1(1)
  lft1 = bb1(2)
  rgt1 = bb1(3)

  bb2 = NhlGetBB(plot2) ; Get bounding box of plot
  top2 = bb2(0)
  bot2 = bb2(1)
  lft2 = bb2(2)
  rgt2 = bb2(3)

  total_height1 = top1 - bot1
  total_height2 = top2 - bot2
  total_width1  = rgt1 - lft1
  total_width2  = rgt2 - lft2

;
; Calculate the largest scale factor possible that will allow us
; to fit both plots on the page, with 5% white space on both sides.
;
  if(attach_y) then
    scale_width   = 1. / (1.1 * (scale1*total_width1 + scale2*total_width2))
    scale_height1 = 1. / (1.1 * scale1*total_height1)
    scale_height2 = 1. / (1.1 * scale2*total_height2)
    scale = min((/scale_height1,scale_height2,scale_width/))
  else
    scale_height = 1. / (1.1 * (scale1*total_height1 + scale2*total_height2))
    scale_width1 = 1. / (1.1 * scale1*total_width1)
    scale_width2 = 1. / (1.1 * scale2*total_width2)
    scale = min((/scale_height,scale_width1,scale_width2/))
  end if
;
; Resize both plots with new scale factor, and set sizes of tick marks
; and tick marks labels to be the same.
;
  new_scale1 = scale * scale1
  new_scale2 = scale * scale2

  new_mj_length   = (new_scale1*mj_length1  + new_scale2*mj_length2)/2.
  new_mjo_length  = (new_scale1*mjo_length1 + new_scale2*mjo_length2)/2.
  new_mn_length   = (new_scale1*mn_length1  + new_scale2*mn_length2)/2.
  new_mno_length  = (new_scale1*mno_length1 + new_scale2*mno_length2)/2.
  new_font_height = (new_scale1*font_height1 + new_scale2*font_height2)/2.
  new_main_font_height = new_scale1*main_font_height1

  if(attach_y) then
    mj_length  =  get_res_value(res1,"tmXBMajorLengthF",new_mj_length)
    mjo_length =  get_res_value(res1,"tmXBMajorOutwardLengthF",\
                                new_mjo_length)
    mn_length  =  get_res_value(res1,"tmXBMinorLengthF",new_mn_length)
    mno_length =  get_res_value(res1,"tmXBMinorOutwardLengthF",\
                                new_mno_length)
  else
    mj_length  =  get_res_value(res1,"tmYLMajorLengthF",new_mj_length)
    mjo_length =  get_res_value(res1,"tmYLMajorOutwardLengthF",\
                                new_mjo_length)
    mn_length  =  get_res_value(res1,"tmYLMinorLengthF",new_mn_length)
    mno_length =  get_res_value(res1,"tmYLMinorOutwardLengthF",\
                              new_mno_length)
  end if

  font_heightxl = get_res_value(res1,"tmXBFontHeight",new_font_height)
  font_heightyl = get_res_value(res1,"tmYLFontHeight",new_font_height)
  font_heightx  = get_res_value(res1,"tiXAxisFontHeightF",new_font_height)
  font_heighty  = get_res_value(res1,"tiYAxisFontHeightF",new_font_height)
  main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                     max((/new_main_font_height,new_font_height/)))
  setvalues plot1
    "vpHeightF"               : new_scale1 * height1
    "vpWidthF"                : new_scale1 * width1

    "tiXAxisFontHeightF"      : font_heightx
    "tiYAxisFontHeightF"      : font_heighty
    "tiMainFontHeightF"       : main_font_height

    "tmYRMajorLengthF"        : mj_length
    "tmYRMajorOutwardLengthF" : mjo_length
    "tmYRMinorLengthF"        : mn_length
    "tmYRMinorOutwardLengthF" : mno_length

    "tmYLMajorLengthF"        : mj_length
    "tmYLMajorOutwardLengthF" : mjo_length
    "tmYLMinorLengthF"        : mn_length
    "tmYLMinorOutwardLengthF" : mno_length

    "tmXBMajorLengthF"        : mj_length
    "tmXBMajorOutwardLengthF" : mjo_length
    "tmXBMinorLengthF"        : mn_length
    "tmXBMinorOutwardLengthF" : mno_length

    "tmXTMajorLengthF"        : mj_length
    "tmXTMajorOutwardLengthF" : mjo_length
    "tmXTMinorLengthF"        : mn_length
    "tmXTMinorOutwardLengthF" : mno_length

    "tmXBLabelFontHeightF"    : font_heightx
    "tmYLLabelFontHeightF"    : font_heighty
  end setvalues

  if(attach_y) then
    mj_length  =  get_res_value(res2,"tmXBMajorLengthF",new_mj_length)
    mjo_length =  get_res_value(res2,"tmXBMajorOutwardLengthF",\
                                new_mjo_length)
    mn_length  =  get_res_value(res2,"tmXBMinorLengthF",new_mn_length)
    mno_length =  get_res_value(res2,"tmXBMinorOutwardLengthF",\
                                new_mno_length)
  else
    mj_length  =  get_res_value(res2,"tmYLMajorLengthF",new_mj_length)
    mjo_length =  get_res_value(res2,"tmYLMajorOutwardLengthF",\
                                new_mjo_length)
    mn_length  =  get_res_value(res2,"tmYLMinorLengthF",new_mn_length)
    mno_length =  get_res_value(res2,"tmYLMinorOutwardLengthF",\
                                new_mno_length)
  end if

  font_heightxl = get_res_value(res2,"tmXBFontHeight",new_font_height)
  font_heightyl = get_res_value(res2,"tmYLFontHeight",new_font_height)
  font_heightx  = get_res_value(res2,"tiXAxisFontHeightF",new_font_height)
  font_heighty  = get_res_value(res2,"tiYAxisFontHeightF",new_font_height)
  main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                     max((/new_main_font_height,new_font_height/)))

  setvalues plot2
    "vpHeightF"               : new_scale2 * height2
    "vpWidthF"                : new_scale2 * width2

    "tiXAxisFontHeightF"      : font_heightx
    "tiYAxisFontHeightF"      : font_heighty
    "tiMainFontHeightF"       : main_font_height

    "tmYRMajorLengthF"        : mj_length
    "tmYRMajorOutwardLengthF" : mjo_length
    "tmYRMinorLengthF"        : mn_length
    "tmYRMinorOutwardLengthF" : mno_length

    "tmYLMajorLengthF"        : mj_length
    "tmYLMajorOutwardLengthF" : mjo_length
    "tmYLMinorLengthF"        : mn_length
    "tmYLMinorOutwardLengthF" : mno_length

    "tmXBMajorLengthF"        : mj_length
    "tmXBMajorOutwardLengthF" : mjo_length
    "tmXBMinorLengthF"        : mn_length
    "tmXBMinorOutwardLengthF" : mno_length

    "tmXTMajorLengthF"        : mj_length
    "tmXTMajorOutwardLengthF" : mjo_length
    "tmXTMinorLengthF"        : mn_length
    "tmXTMinorOutwardLengthF" : mno_length

    "tmXBLabelFontHeightF"    : font_heightx
    "tmYLLabelFontHeightF"    : font_heighty
  end setvalues
;
; Get new bounding boxes and sizes of resized plots, so we can
; figure out where to position the first plot.
;
  bb1 = NhlGetBB(plot1) ; Get bounding box of plot
  top1 = bb1(0)
  bot1 = bb1(1)
  lft1 = bb1(2)
  rgt1 = bb1(3)

  bb2 = NhlGetBB(plot2) ; Get bounding box of plot
  top2 = bb2(0)
  bot2 = bb2(1)
  lft2 = bb2(2)
  rgt2 = bb2(3)

  total_height1 = top1 - bot1
  total_height2 = top2 - bot2
  total_width1  = rgt1 - lft1
  total_width2  = rgt2 - lft2

  getvalues plot1
    "vpXF" : vpx1
    "vpYF" : vpy1
  end getvalues

  if(attach_y) then
    total_width_left  = 1. - (total_width1 + total_width2)
    total_height_left = 1. - max((/total_height1,total_height2/))
  else
    total_height_left = 1. - (total_height1 + total_height2)
    total_width_left  = 1. - max((/total_width1,total_width2/))
  end if

  new_vpx1 = total_width_left/2. + (vpx1-lft1)
  new_vpy1 = 1. - (total_height_left/2. + (top1-vpy1))

  setvalues plot1
    "vpYF" : new_vpy1
    "vpXF" : new_vpx1
  end setvalues

  anno = NhlAddAnnotation(plot1,plot2)
  zone = get_res_value(res2,"amZone",2)
  para = get_res_value(res2,"amParallelPosF",0.5)
  orth = get_res_value(res2,"amOrthogonalPosF",0.0)
  if(attach_y) then
    setvalues anno
      "amZone"          : zone
      "amSide"          : "right"  ; Plot at right.
      "amResizeNotify"  : True     ; Allow resize if plot resized.
      "amParallelPosF"  : para
      "amOrthogonalPosF": orth
    end setvalues
  else
    setvalues anno
      "amZone"          : zone
      "amSide"          : "bottom" ; Plot at bottom
      "amResizeNotify"  : True     ; Allow resize if plot resized.
      "amParallelPosF"  : para
      "amOrthogonalPosF": orth
    end setvalues
  end if

  return(anno)
  
end

;***********************************************************************;
; Procedure : draw_bb                                                   ;
;                wks:graphic                                            ;
;               plot:graphic                                            ;
; This procedure draws a box around the bounding box of the given plot  ;
; objects.                                                              ; 
;***********************************************************************;
procedure draw_bb(wks:graphic,plot:graphic)
begin
  dimplot = dimsizes(plot)
  if(dimsizes(dimplot).eq.1) then
;
; Force bb to be 2-dimensional so we don't have to have a
; bunch of "if" tests later.
;
    bb = new((/1,4/),float)
    bb(0,:) = NhlGetBB(plot)
  else
    bb = NhlGetBB(plot)
  end if
  top = max(bb(:,0))
  bot = min(bb(:,1))
  lft = min(bb(:,2))
  rgt = max(bb(:,3))

  gsn_polyline_ndc(wks,(/lft,rgt,rgt,lft,lft/),(/bot,bot,top,top,bot/),False)
end

;***********************************************************************;
; Function : get_rgb_values                                             ;
;              named_colors: string array of named colors               ;
;                                                                       ;
; This function retrieves the RGB of the list of named colors, if they  ;
; exist.  If any of the named colors don't exist, then a missing value  ;
; is returned.                                                          ;
;                                                                       ;
;***********************************************************************;
function get_rgb_values(named_colors)
begin
;
; Read in rgb.txt file that has named colors and RGB values.
;
  rgb_file = "$NCARG_ROOT/lib/ncarg/database/rgb.txt"
  rgb_text = asciiread(rgb_file,-1,"string")

  do i=0,dimsizes(rgb_text)-1
    rgb_text(i) = lower_case(rgb_text(i))  ; convert to lower case
  end do

  rgb_text_char = stringtochar(rgb_text)    ; convert to character
;
; Normalize RGB values.
;
  red   = stringtointeger(chartostring(rgb_text_char(:,0:2)))/255.
  green = stringtointeger(chartostring(rgb_text_char(:,4:6)))/255.
  blue  = stringtointeger(chartostring(rgb_text_char(:,8:10)))/255.
;
; Color names start in the 14th column of the rgb.txt file.
;
  names = chartostring(rgb_text_char(:,13:))

  ncolors = dimsizes(named_colors)
  rgb_values = new((/ncolors,3/),float,-999)

  do i=0,ncolors-1
    index = ind(named_colors(i).eq.names)
    if(.not.ismissing(index))
      rgb_values(i,0) = red(index)
      rgb_values(i,1) = green(index)
      rgb_values(i,2) = blue(index)
    end if
  end do
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
end

;***********************************************************************;
; Function : gsn_retrieve_colormap                                      ;
;                   wks: workstation object                             ;
;                                                                       ;
; This function retrieves the current color map in use for workstation  ;
; "wks". "wks is the workstation id returned from a call to             ;
; gsn_open_wks.  The return variable will be an n x 3 array, where n is ;
; the number of colors, and the 3 represents the R, G, and B values.    ;
;***********************************************************************;
function gsn_retrieve_colormap(wks:graphic)
begin
    getvalues wks
        "wkColorMap" : cmap
    end getvalues

    return(cmap)
end

;***********************************************************************;
; Procedure : gsn_merge_colormaps                                       ;
;                   wks: workstation object                             ;
;                  cmap1: colormap                                      ;
;                  cmap2: colormap                                      ;
;                                                                       ;
; This procedure two colormaps into one, and then sets the workstaion   ;
; to use this new colormap. If the merged colormaps exceed 255 colors,  ;
; then only the first 255 colors will be used.                          ;
;                                                                       ;
; Both cmaps must either be an n x 3  float array, an array of color    ;
; names, or a single string representing a pre-defined colormap name.   ;
; Each colormap is assumed to have its own background/foreground color, ;
; so the first two colors of the second color map are not included in   ;
; the new color map.                                                    ;
;***********************************************************************;
procedure gsn_merge_colormaps(wks:graphic,cmap1,cmap2)
local dim_cmap1, dim_cmap2, new_cmap1, new_cmap2, len_cmap1, len_cmap2
begin
  dim_cmap1 = dimsizes(cmap1)
  dim_cmap2 = dimsizes(cmap2)
;
; Error checking.
;
  if((typeof(cmap1).eq."float".and.(dimsizes(dim_cmap1).ne.2.or.\
                                   dim_cmap1(1).ne.3)).or.\
     (typeof(cmap1).eq."string".and.dimsizes(dim_cmap1).ne.1))
    print("Warning: gsn_merge_colormaps: cmap1 must either be an n x 3 float array,")
    print("a single pre-defined colormap name, or a 1-dimensional string array of named colors.")
  end if

  if((typeof(cmap2).eq."float".and.(dimsizes(dim_cmap2).ne.2.or.\
                                   dim_cmap2(1).ne.3)).or.\
     (typeof(cmap2).eq."string".and.dimsizes(dim_cmap2).ne.1))
    print("Warning: gsn_merge_colormaps: cmap2 must either be an n x 3 float array,")
    print("a single pre-defined colormap name, or a 1-dimensional string array of named colors.")
  end if

;
; Get first colormap in RGB values, and include background and
; foreground colors.
; 
  if(typeof(cmap1).eq."float") then
    new_cmap1 = cmap1
  else
    gsn_define_colormap(wks,cmap1)
    new_cmap1 = gsn_retrieve_colormap(wks)
  end if
  len_cmap1 = dimsizes(new_cmap1(:,0))

;
; Get second colormap in RGB values, and ignore background and
; foreground colors.
; 
  if(typeof(cmap2).eq."float") then
    len_cmap2 = dimsizes(cmap2(:,0)) - 2
    if(len_cmap2.gt.0) then
      new_cmap2 = cmap2(2:,:)
    else
      len_cmap2 = 0
    end if
  else
;
; Test if the strings are named colors or a color map.
; If it's a color map, then we will drop the foreground/background
; colors and only append colors 2 and on.  If it is named colors, 
; then we'll append all of the named colors.
;
    rgb_values = get_rgb_values(cmap2)
    indices    = ind(.not.ismissing(rgb_values(:,0)))
	if(all(ismissing(indices)))
      gsn_define_colormap(wks,cmap2)    ; Must be a color map name.
      tmp_cmap2 = gsn_retrieve_colormap(wks)
      new_cmap2 = tmp_cmap2(2:,:)
    else
      new_cmap2 = new((/dimsizes(indices),3/),"float")
      new_cmap2(:,0) = rgb_values(indices,0)   ; Must be named colors.
      new_cmap2(:,1) = rgb_values(indices,1)
      new_cmap2(:,2) = rgb_values(indices,2)
    end if
    len_cmap2 = dimsizes(new_cmap2(:,0))
    delete(indices)
    delete(rgb_values)
  end if

;
; Make sure two colormaps don't total more than 256 colors.
;
  if(len_cmap1.ge.256) then
    len_cmap1 = 256
    len_cmap2 = 0
  else
    if( (len_cmap1+len_cmap2).gt.256 ) then
      len_cmap2 = 256-len_cmap1
    end if
  end if
;
; Create new merged colormap.
;
  len_cmap = len_cmap1+len_cmap2
  new_cmap = new((/len_cmap,3/),float)

  new_cmap(0:len_cmap1-1,:) = new_cmap1(0:len_cmap1-1,:)
  if(len_cmap2.gt.0)
    new_cmap(len_cmap1:,:) = new_cmap2(0:len_cmap2-1,:)
  end if

  gsn_define_colormap(wks,new_cmap)
end

;***********************************************************************;
; Function : hist_columns                                               ;
;                   wks: workstation object                             ;
;                    xy: graphic                                        ;
;                  bins: numeric                                        ;
;             binvalues: numeric                                        ;
;                 width: numeric                                        ;
;                colors: numeric                                        ;
;                 gsres: logical                                        ;
;                                                                       ;
;    xy      - xy plot id to draw columns on                            ;
;  bins      - the center of each bin range.                            ;
;  binvalues - the number of values in the corresponding bin            ;
;  width     - half the width of the bar                                ;
;  colors    - array of color indices to use                            ; 
;  gsres     - optional primitive resources                             ;
;                                                                       ;
; This function creates the columns for a histogram plot. The Y axis    ;
; will represent the number of values in a bin and a percentage.        ;
;                                                                       ;
;***********************************************************************;
function hist_columns(wks[1]:graphic,xy[1]:graphic,bins[*]:numeric, \
                      binvalues:numeric, width[*]:numeric, \
                      colors[*]:integer, gsres:logical)
local i, gsres, points, compare
begin
;
; If binvalues is 2-dimensional, then comparing two fields.
;
  nbins = dimsizes(bins)
  dims  = dimsizes(binvalues)
  if(dims(0).eq.2.and.dimsizes(dims).eq.2)
    nbinvalues  = dims(1)
    compare     = True
  else
    nbinvalues  = dims(0)
    compare     = False
  end if

  ncols = dimsizes(colors)
  if(ncols.ne.nbins.or.nbins.ne.nbinvalues)
    print("Error: columns: Dimension sizes of bins (" +nbins+ ") ,binvalues(" +nbinvalues + ") and colors (" + ncols + ") must be the same")
    return
  end if

  if(compare)
    xpoints   = new((/2,nbins,5/),float)
    ypoints   = new((/2,nbins,5/),float)
    polygons0 = new(nbins,graphic)
    polygons1 = new(nbins,graphic)
  else
    xpoints  = new((/nbins,5/),float)
    ypoints  = new((/nbins,5/),float)
    polygons = new(nbins,graphic)
  end if

  gsres           = True
  set_attr(gsres,"gsEdgesOn",True)

  if(compare)
    width23 = 0.6667 * width
    ypoints(0,:,0) = (/0/)
    ypoints(1,:,0) = (/0/)
    ypoints(0,:,1) = (/binvalues(0,:)/)
    ypoints(1,:,1) = (/binvalues(1,:)/)
    ypoints(0,:,2) = (/ypoints(0,:,1)/)
    ypoints(1,:,2) = (/ypoints(1,:,1)/)
    ypoints(0,:,3) = (/0/)
    ypoints(1,:,3) = (/0/)
    ypoints(0,:,4) = (/0/)
    ypoints(1,:,4) = (/0/)
    xpoints(0,:,0) = (/bins - width/)
    xpoints(1,:,0) = xpoints(0,:,0) + width23
    xpoints(0,:,1) = (/xpoints(0,:,0)/)
    xpoints(1,:,1) = xpoints(0,:,1) + width23
    xpoints(0,:,2) = (/bins + width/)
    xpoints(1,:,2) = xpoints(0,:,2) + width23
    xpoints(0,:,3) = (/xpoints(0,:,2)/)
    xpoints(1,:,3) = xpoints(0,:,3) + width23
    xpoints(0,:,4) = (/xpoints(0,:,0)/)
    xpoints(1,:,4) = xpoints(0,:,4) + width23
  else
    ypoints(:,0) = (/0/)
    ypoints(:,1) = (/binvalues/)
    ypoints(:,2) = (/ypoints(:,1)/)
    ypoints(:,3) = (/0/)
    ypoints(:,4) = (/0/)
    xpoints(:,0) = (/bins - width/)
    xpoints(:,1) = (/xpoints(:,0)/)
    xpoints(:,2) = (/bins + width/)
    xpoints(:,3) = (/xpoints(:,2)/)
    xpoints(:,4) = (/xpoints(:,0)/)
  end if

  newcolors = get_res_value(gsres,"gsFillColor",colors)
  ncol      = dimsizes(newcolors)

  fillindex = get_res_value(gsres,"gsFillIndex",(/0,6/))
;
; Make sure fill indices are between 0 and 17.
;
  fillindex1 = min((/max((/fillindex(0),0/)),17/))
  if(dimsizes(fillindex).ge.2)
    fillindex2 = min((/max((/fillindex(1),0/)),17/))
  else
    fillindex2 = fillindex1
  end if

  do i = 0, nbins - 1
    gsres@gsFillColor = newcolors(i%ncol)

    if(compare) 
;
; Add the second histogram bars first, so that they will be drawn
; first. The first histogram bars will thus be drawn on top.
;
      gsres@gsFillIndex = fillindex2
      if(binvalues@horizontal) then
        polygons1(i) = gsn_add_polygon(wks,xy,ypoints(1,i,:), \
                                       xpoints(1,i,:), gsres)
      else
        polygons1(i) = gsn_add_polygon(wks,xy,xpoints(1,i,:), \
                                       ypoints(1,i,:), gsres)
      end if
      gsres@gsFillIndex = fillindex1
      if(binvalues@horizontal) then
        polygons0(i) = gsn_add_polygon(wks,xy,ypoints(0,i,:), \
                                       xpoints(0,i,:), gsres)
      else
        polygons0(i) = gsn_add_polygon(wks,xy,xpoints(0,i,:), \
                                       ypoints(0,i,:), gsres)
      end if
    else
      gsres@gsFillIndex = fillindex1
      if(binvalues@horizontal) then
        polygons(i) = gsn_add_polygon(wks,xy,ypoints(i,:),xpoints(i,:),gsres)
      else
        polygons(i) = gsn_add_polygon(wks,xy,xpoints(i,:),ypoints(i,:),gsres)
      end if
    end if
  end do
;
; Return the polygons created as attributes of the XY plot. This is
; necessary, b/c otherwise the polygons will go away.
;
  if(compare)
    var_string0 = systemfunc("echo hpolygons0$$")
    var_string1 = systemfunc("echo hpolygons1$$")
    xy@$var_string0$ = polygons0
    xy@$var_string1$ = polygons1
  else
    var_string = systemfunc("echo hpolygons$$")
    xy@$var_string$ = polygons
  end if
  return(xy)
end

;***********************************************************************;
; Function : compute_hist_vals                                          ;
;                     x: numeric                                        ;
;                  bins: numeric                                        ;
;                 nbins: integer                                        ;
;             setendpts: logical                                        ;
;             setmidpts: integer                                        ;
;                isnice: integer                                        ;
;                                                                       ;
; By default, this routine calculates a nice set of ranges for "binning";
; the given data. If setendpts is True, then the user has explicitly set;
; set their own bin end points via the bins array. If setmidpts is True,;
; then "bins" will represent the mid point values.                      ;
;                                                                       ;
; If both setendpts and isnice are False, then the intervals of the     ;
; bins are just based on the min and max of the data exactly.           ;
;                                                                       ;
;***********************************************************************;
function compute_hist_vals(x:numeric,bins:numeric,nbins[1]:integer, \
                           bin_width:numeric,setendpts:logical,setmidpts:logical, \
                           isnice:logical,compare:logical)
local xmin, xmax, sprd, nbuckets, buckets, xx, i
begin
  xmin = tofloat(min(x))
  xmax = tofloat(max(x))
;
; If the bins are set, need to determine if you want to have the bins
; represent ranges of values or discrete values.
;
  if(setmidpts)
    sprd     = bins
    nbuckets = dimsizes(sprd)
  else
;
; Check if range values have been set by user, or if we need to
; calculate them.
;
    if(setendpts)
      sprd = bins
    else
      if(nbins.lt.0) then
        if(bin_width.lt.0.) then
          nbins = 10
        else 
         nbins = floattointeger(((xmax - xmin)/bin_width))
          if(nbins.le.0) then
            print("Warning: compute_hist_vals: cannot use select bin width. Defaulting...")
            nbins = 10 
          end if
        end if
      end if
      if(isnice) then
;
; Based on min and max of data, compute a new min/max/step that will
; give us "nice" values.
;
         nicevals = nice_mnmxintvl(xmin,xmax,nbins,True)
         nvals    = floattoint((nicevals(1) - nicevals(0))/nicevals(2) + 1)
         sprd     = fspan(nicevals(0),nicevals(1),nvals)
       else
;
; Don't bother with "nice" values; just span the data. 
;
         sprd = fspan(xmin,xmax,nbins+1)
       end if
    end if
    nbuckets = dimsizes(sprd)-1              ; Number of bins
  end if
;
; Set up variable to hold bucket (or bin) values. Bin values can have
; unequal spacing.
;
  if(compare) then
    num_in_bins = new((/2,nbuckets/),integer)
  else
    num_in_bins = new(nbuckets,integer)
  end if
;
; Count number of values in a particular bin range, or exactly
; equal to a bin value if discrete.
;
  xx = ndtooned(x)

  if(compare) then
    xdims = dimsizes(x)
    elems = 1
    do i = 1,dimsizes(xdims)-1
      elems = elems * xdims(i)
    end do

    xx1 = xx(0:elems-1)
    xx2 = xx(elems:)
  end if

  if(setmidpts)
    if(compare) then
      do i = 0, nbuckets-1
        num_in_bins(0,i) = num(xx1.eq.sprd(i))
        num_in_bins(1,i) = num(xx2.eq.sprd(i))
      end do
    else
      do i = 0, nbuckets-1
        num_in_bins(i) = num(xx.eq.sprd(i))
      end do
    end if
  else
;
; The last bin interval will contain values that fall on both the
; right and left end points.  All the other bin intervals contain
; values that fall on the left only.
;
    if(compare) then
      do i = 0, nbuckets-2
        num_in_bins(0,i) = num(xx1.ge.sprd(i).and.xx1.lt.sprd(i+1))
        num_in_bins(1,i) = num(xx2.ge.sprd(i).and.xx2.lt.sprd(i+1))
      end do
;
; Special case for very last bin.
;
      num_in_bins(0,nbuckets-1) = num(xx1.ge.sprd(nbuckets-1).and. \
                                      xx1.le.sprd(nbuckets))
      num_in_bins(1,nbuckets-1) = num(xx2.ge.sprd(nbuckets-1).and. \
                                      xx2.le.sprd(nbuckets))
    else
      do i = 0, nbuckets-2
        num_in_bins(i) = num(xx.ge.sprd(i).and.xx.lt.sprd(i+1))
      end do
;
; Special case for very last bin.
;
      num_in_bins(nbuckets-1) = num(xx.ge.sprd(nbuckets-1).and. \
                                    xx.le.sprd(nbuckets))
    end if
  end if
;
; Count number of missing values
;
  if(compare)
    if(isatt(xx,"_FillValue")) then
      num_missing1 = 0
      num_missing2 = 0
    else
      num_missing1 = num(ismissing(xx1))
      num_missing2 = num(ismissing(xx2))
    end if
  else
    if(isatt(xx,"_FillValue")) then
      num_missing = num(ismissing(xx))
    else
      num_missing = 0
    end if
  end if

  delete(xx)

;
; Calculate the deltas between each bin value. These will be used to
; determine the width of the histogram bars later.
;
  nsprd            = dimsizes(sprd)
  num_in_bins@dx   = new(nsprd-1,float)
  do i=0,nsprd-2
    num_in_bins@dx(i) = sprd(i+1) - sprd(i)
  end do
;
; Return this information
; 
  num_in_bins@sprd       = sprd
  if(compare)
    num_in_bins@NumMissing1 = num_missing1
    num_in_bins@NumMissing2 = num_missing2
  else
    num_in_bins@NumMissing = num_missing
  end if
  return(num_in_bins)
end 

;***********************************************************************;
; Function : gsn_histogram                                              ;
;                   wks: workstation object                             ;
;                 xdata: numeric                                        ;
;                   res: resources                                      ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;  gsnHistogramNumberOfBins - Indicates number of bin intervals you     ;
;       want. The default is around 10.                                 ;
;                                                                       ;
;  gsnHistogramBinWidth - Instead of indicating number of bins, you can ;
;       specify a bin width. Depending on whether SelectNiceIntervals   ;
;       is set to True, you will either get exactly bins of this size,  ;
;       or approximately this size.                                     ; 
;                                                                       ;
;  gsnHistogramSelectNiceIntervals - Indicates whether we want          ;
;       gsn_histogram to select "nice" range values. Default is True.   ;
;                                                                       ;
;  gsnHistogramComputePercentages - If True, then percentage values     ;
;       will be put on right axis. Default is False.                    ;
;                                                                       ;
;  gsnHistogramClassIntervals - By default, gsn_histogram will pick the ;
;       bin class intervals for you. If you set this, then it will use  ;
;       these values for the bin ranges.                                ;
;                                                                       ;
;  gsnHistogramBinIntervals   - Same as gsnHistogramClassIntervals.     ;
;                                                                       ;
;  gsnHistogramDiscreteClassValues - By default, gsn_histogram will bin ;
;       your data into ranges.  If you set this resource, then your data;
;       is assumed to already be "binned", and it just counts the number;
;       of values exactly equal to the discrete values.                 ;
;                                                                       ;
;  gsnHistogramDiscreteBinValues - Same as                              ;
;                                  gsnHistogramDiscreteClassValues.     ;
;                                                                       ;
;  gsnHistogramCompare - Compare two fields.                            ;
;                                                                       ;
;  gsnHistogramHorizontal - Draw horizontal bars.  Default is False     ;
;                                                                       ;
; The number of missing values counted is returned as an attribute      ;
; called "NumMissing" (or "NumMissing1" and "NumMissing2" if comparing  ;
; two fields.)                                                          ;
;***********************************************************************;
function gsn_histogram(wks[1]:graphic,xdata:numeric, res:logical)
local res2, calldraw, callframe, maxbb, ispercent, setmidpts, setendpts, \
      bins, nbucks, nsprd, xmin, top, botton, left, right, lenc, tmp, \
      colors, locs, lbs
begin
  dummy = new(1,graphic)      ; Dummy graphic to return if things go wrong.
  res2  = res

  calldraw   = get_res_value(res2,"gsnDraw", True)
  callframe  = get_res_value(res2,"gsnFrame",True)
  maxbb      = get_bb_res(res2)
  nbins      = get_res_value(res2,"gsnHistogramNumberOfBins",-1)
  bin_width  = get_res_value(res2,"gsnHistogramBinWidth",-1)
  ispercent  = get_res_value(res2,"gsnHistogramComputePercentages",False)
  percentsign= get_res_value(res2,"gsnHistogramPercentSign",False)
  isnice     = get_res_value(res2,"gsnHistogramSelectNiceIntervals",True)
  compare    = get_res_value(res2,"gsnHistogramCompare",False)
  horizontal = get_res_value(res2,"gsnHistogramHorizontal",False)
  setmidpts  = False
  setendpts  = False
;
; Check if user explicitly specifying bin values. If so, and they represent
; end points, then make sure there are at least two values, and sort them.
;
  if(isatt(res2,"gsnHistogramClassIntervals").or. \
     isatt(res2,"gsnHistogramBinIntervals"))
    setendpts = True
    if(isatt(res2,"gsnHistogramClassIntervals"))
      bins = get_res_value(res2,"gsnHistogramClassIntervals",1)
    else
      bins = get_res_value(res2,"gsnHistogramBinIntervals",1)
    end if
    nbins = dimsizes(bins)
    if(nbins.lt.2)
      print("Error: gsn_histogram: There must be at least two values in the bin intervals.")
      return(dummy)
    end if
    if(max(bins).lt.min(xdata).or.min(bins).gt.max(xdata))
      print("Error: gsn_histogram: The bin values you picked are out of range.")
      return(dummy)
    end if
    qsort(bins)    ; Sort the bins and put back into "bins" variable.
  end if
;
; If setting the mid point values, then you only need at least one value.
; But, you can't be setting both Intervals and Discrete values.
;
  if(isatt(res2,"gsnHistogramDiscreteClassValues").or. \
     isatt(res2,"gsnHistogramDiscreteBinValues"))
    if(setendpts) then
      print("warning: gsn_histogram: You already set gsnHistogramClassIntervals, so ignoring gsnHistogramDiscreteClassValues.")
;
; Delete so it doesn't get passed on to other routines.
;
      if(isatt(res2,"gsnHistogramDiscreteBinValues")) then
        delete(res2@gsnHistogramDiscreteBinValues)
      else
        delete(res2@gsnHistogramDiscreteClassValues)
      end if
    else
      setmidpts = True
      if(isatt(res2,"gsnHistogramDiscreteBinValues")) then
        bins = get_res_value(res2,"gsnHistogramDiscreteBinValues",1)
      else
        bins = get_res_value(res2,"gsnHistogramDiscreteClassValues",1)
      end if
      if(max(bins).lt.min(xdata).or.min(bins).gt.max(xdata))
        print("Error: gsn_histogram: The bin values you picked are out of range.")
        return(dummy)
      end if
    end if
  end if

  if(.not.setendpts.and..not.setmidpts) then
    bins = 0
  end if
;
; If comparing two fields, then first dimension must be two.
;
  if(compare) then
    dims = dimsizes(xdata)
    if(dims(0).ne.2.or.dimsizes(dims).eq.1) then
      print("Error: gsn_histogram: If comparing two fields, then the first dimension of the input array must be 2, and there must be at least two dimensions.")
      return(dummy)
    end if
    delete(dims)
  end if    
;
; Get number of values in each bin and bin values (buck@sprd).
;
  buck = compute_hist_vals(xdata,bins,nbins,bin_width,setendpts,setmidpts, \
                           isnice,compare)

  buck@horizontal = horizontal    ; horizontal or vertical bars

  if(all(ismissing(buck)))
    print("Error: gsn_histogram: Unable to calculate bin values.")
    return(dummy)
  end if

  if(all(buck.eq.0))
    print("Error: gsn_histogram: No data found in selected bins. No histogram will be drawn.")
    return(dummy)
  end if

  nsprd  = dimsizes(buck@sprd)
  xmin   = min(xdata)
;
; Count the number of bins.
;
  if(compare)
    dimbucks = dimsizes(buck)
    nbucks   = dimbucks(1)
  else
    nbucks = dimsizes(buck)
  end if
;
; Calculate values for setting up X and Y axis limits, and the
; width of the histogram bars (width).  If doing intervals, then the
; bar widths should be proportional to the range they represent.
; Otherwise, if doing mid points, use the smallest width.
;
; If comparing two fields, then the histogram columns must be
; smaller in width.
;
  if(setendpts)
    if(compare)
      width = buck@dx/4.0
    else
      width = buck@dx/3.0
    end if
  else
    if(compare)
      width = min(buck@dx/4.0)
    else
      width = min(buck@dx/3.0)
    end if
  end if
  top    = max(buck) * 1.1
  bottom = 0
  range  = buck@sprd(nsprd-1)-buck@sprd(0)
  if(setmidpts)
    right  = buck@sprd(nsprd-1) + buck@dx(nsprd-2)
    left   = buck@sprd(0) - buck@dx(0)
  else
    right  = buck@sprd(nsprd-1) + (range * .05)
    left   = buck@sprd(0) - (range * .05)
  end if

;
; Span the full color map when color-filling the bins.
;
  getvalues wks
    "wkColorMapLen" : lenc
  end getvalues

  tmp = (lenc-2)/nbucks
  if(tmp .eq. 0 )
    print("Warning: gsn_histogram: not enough colors, using single color")
    colors = new(nbucks,integer)
    colors = 2
  else 
    if(setmidpts)
      colors = new(nsprd,integer)
    else
      colors = new(nsprd-1,integer)
    end if
    tmpc   = ispan(2,lenc,tmp)
    if(dimsizes(tmpc).ne.dimsizes(colors))
      colors = tmpc(0:dimsizes(colors)-1)
    else
      colors = tmpc
    end if
  end if
;
; Calculate locations of center of bins and labels and 
; values for tick marks. If comparing two fields, and using ranges,
; then the center won't be exactly the center, but rather 5/12 of 
; the width of the range in.
;
  if(setmidpts) then 
    locs = buck@sprd
  else
    locs = new(nbucks,float)
    if(compare)
      locs = buck@sprd(0:nbucks-1) + 5.*buck@dx(0:nbucks-1)/12.
    else
      locs = buck@sprd(0:nbucks-1) + buck@dx(0:nbucks-1)/2.
    end if
  end if
  lbs = buck@sprd

  if(horizontal) then
    xy = create "xy" xyPlotClass wks
      "trYMinF" : get_res_value(res2,"trYMinF",left)
      "trYMaxF" : get_res_value(res2,"trYMaxF",right)
      "trXMinF" : get_res_value(res2,"trXMinF",bottom)
      "trXMaxF" : get_res_value(res2,"trXMaxF",top)
    end create
  else
    xy = create "xy" xyPlotClass wks
      "trXMinF" : get_res_value(res2,"trXMinF",left)
      "trXMaxF" : get_res_value(res2,"trXMaxF",right)
      "trYMinF" : get_res_value(res2,"trYMinF",bottom)
      "trYMaxF" : get_res_value(res2,"trYMaxF",top)
    end create
  end if
;
; If gsnHistogramComputePercentages is True, then we'll add percentage
; labels to the right (or top) axis.
;
  if(ispercent)
    if(compare)
      npts = dimsizes(ndtooned(xdata))/2
    else
      npts = dimsizes(ndtooned(xdata))    ; Total number of points. 
    end if
;
; Compute min, max, and step necessary to later get
; "nice" values for the percentages.
;
    nicevals = nice_mnmxintvl(bottom,min((/100.,100.*(top/npts)/)),7,False)
    nvals    = floattoint((nicevals(1) - nicevals(0))/nicevals(2) + 1)
;
; Generate nice values for minor and major tick marks. For the minor
; tick marks, just add one between each major.
; 
    pnice  = fspan(nicevals(0),nicevals(1),nvals)
    pmnice = fspan(nicevals(0)-nicevals(2)/2,nicevals(1)+nicevals(2)/2, \
                   2*nvals+1)
;
; Calculate bins at these percentages. These are the values we'll use
; for the tick marks.
;
    bins_at_pnice  = (npts * pnice) / 100.
    bins_at_pmnice = (npts * pmnice) / 100.
;
; Set some resources to control tickmarks.
;
    if(horizontal) then
      set_attr(res2,"tmXUseBottom", False)
      set_attr(res2,"tmXTOn",True)
      set_attr(res2,"tmXTLabelsOn",True)
      set_attr(res2,"tmXTMode","Explicit")
      set_attr(res2,"tmXTValues",bins_at_pnice)
      set_attr(res2,"tmXTMinorValues",bins_at_pmnice)
      if(percentsign)
        set_attr(res2,"tmXTLabels",pnice+"%")
      else
        set_attr(res2,"tmXTLabels",pnice)
      end if
    else
      set_attr(res2,"tmYUseLeft", False)
      set_attr(res2,"tmYROn",True)
      set_attr(res2,"tmYRLabelsOn",True)
      set_attr(res2,"tmYRMode","Explicit")
      set_attr(res2,"tmYRValues",bins_at_pnice)
      set_attr(res2,"tmYRMinorValues",bins_at_pmnice)
      if(percentsign)
        set_attr(res2,"tmYRLabels",pnice+"%")
      else
        set_attr(res2,"tmYRLabels",pnice)
      end if  
    end if  
  end if

;
; Label the X (or Y) Axis with xdata@long_name if it exists.
;
  if(isatt(xdata,"long_name")) then
    if(horizontal) then
      set_attr(res2,"tiYAxisString",xdata@long_name)
    else
      set_attr(res2,"tiXAxisString",xdata@long_name)
    end if
  else
    if(setendpts)
      if(horizontal) then
        set_attr(res2,"tiYAxisString","Class Intervals")
      else
        set_attr(res2,"tiXAxisString","Class Intervals")
      end if
    end if
  end if
;
; Set some resources.
;
  if(horizontal) then
    set_attr(res2,"tiXAxisString","Frequency")
    set_attr(res2,"tmYROn",False)
    set_attr(res2,"tmYLMode","EXPLICIT")
    set_attr(res2,"tmYLValues",lbs)
    set_attr(res2,"tmYLLabels",lbs)
  else
    set_attr(res2,"tiYAxisString","Frequency")
    set_attr(res2,"tmXTOn",False)
    set_attr(res2,"tmXBMode","EXPLICIT")
    set_attr(res2,"tmXBValues",lbs)
    set_attr(res2,"tmXBLabels",lbs)
  end if
  xyres = get_res_ne(res2,"gs")
  attsetvalues_check(xy,xyres)

;
; Add a right Y (or top X axis) "Percent Occurence" or
; "Percent of Maximum" label.
;
  if(ispercent.and..not.percentsign)
    if(horizontal) then
      getvalues xy
        "tiXAxisFontHeightF" : font_height
      end getvalues
      txangle = 0.
      txside  = "top"
    else
      getvalues xy
        "tiYAxisFontHeightF" : font_height
      end getvalues
      txangle = 90.
      txside  = "right"
    end if

    axis_string = create "axis_string" textItemClass wks
      "txString"      : "Percent"
      "txFontHeightF" : font_height
      "txAngleF"      : txangle
    end create

    anno = NhlAddAnnotation(xy,axis_string)

    setvalues anno
      "amZone"          : 3      ; Just outside plot area
      "amJust"          : "centercenter"
      "amSide"          : txside
      "amParallelPosF"  : 0.5
      "amOrthogonalPosF": 0.03
      "amResizeNotify"  : True     ; Resize if plot resized.
    end setvalues
  end if

  tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
  gsnp_point_tickmarks_outward(xy,tmres,-1.,-1.,-1.,-1.,-1.,-1.)
  gsnp_uniform_tickmark_labels(xy,tmres,0.)

  gsres = get_res_eq(res2,"gs")
  histogram = hist_columns(wks,xy,locs,buck,width,colors,gsres)
  draw_and_frame(wks,histogram,calldraw,callframe,maxbb)

; Return histogram.

  if(compare)
    histogram@NumMissing1 = buck@NumMissing1
    histogram@NumMissing2 = buck@NumMissing2
  else
    histogram@NumMissing = buck@NumMissing
  end if
  return(histogram)
end


;***********************************************************************;
; Function : gsn_contour                                                ;
;                   wks: workstation object                             ;
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
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
function gsn_contour(wks:graphic, data[*][*]:numeric, resources:logical )
local i, data_object, plot_object, res, sf_res_index, \
datares, cnres, llres, cn_res_index, ll_res_index, calldraw, callframe, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, res2, scale, shape, sprdcols
begin
    cnres     = False
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

; Create the data object.
    data_object = create wksname + "_data" scalarFieldClass noparent
        "sfDataArray" : data
    end create

    sfres = get_res_eq(res2,"sf")
    attsetvalues_check(data_object,sfres)

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues data_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if
;
; Create plot object. Make sure you set the tension values (if
; any) when you create the plot. This works better than setting
; them later.
;
    xtension  = get_res_value(res2,"trXTensionF", 2.0)
    ytension  = get_res_value(res2,"trYTensionF", 2.0)

    plot_object = create wksname + "_contour" contourPlotClass wks
        "trXTensionF"       : xtension
        "trYTensionF"       : ytension
        "cnScalarFieldData" : data_object
    end create

; Check for existence of data@long_name and use it in a title it
; it exists.

    if(isatt(data,"long_name")) then
      set_attr(res2,"tiMainString",data@long_name)
    end if

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
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
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      getvalues overlay_plot_object
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

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_contour_map                                            ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a contour plot over a map plot to the ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "data" is the 2-dimensional data to be contoured,   ;
; and "resources" is an optional list of resources. The id of the map   ;
; plot is returned.                                                     ;
;                                                                       ;
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
local i, data_object, contour_object, res, sf_res_index, \
cn_res_index, mp_res_index, map_object, res2, scale, shape, sprdcols
begin
    res2      = resources

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = create wksname + "_data" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

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
    xtension  = get_res_value(res2,"trXTensionF", 2.0)
    ytension  = get_res_value(res2,"trYTensionF", 2.0)

    contour_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
        "trYTensionF"       : ytension
        "trXTensionF"       : xtension
    end create

; Check for existence of data@long_name and use it in a title it
; it exists.

    if(isatt(data,"long_name")) then
      set_attr(res2,"tiMainString",data@long_name)
    end if

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    scale     = get_res_value(res2,"gsnScale",False)
    shape     = get_res_value(res2,"gsnShape",scale)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    mpres = get_res_eq(res2,(/"mp","vp"/))
    cnres = get_res_ne(res2,(/"mp","sf","vp"/))

    attsetvalues_check(map_object,mpres)
    attsetvalues_check(contour_object,cnres)
    if(sprdcols)
      cnres2 = True
      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))
      attsetvalues(contour_object,cnres2)
    end if

    overlay(map_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(contour_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@contour = contour_object
    return(map_object)
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
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;                                                                       ;
;***********************************************************************;
function gsn_map(wks:graphic, projection:string, resources:logical )
local i, plot_object, res2
begin
    res2      = resources

; Create plot object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    plot_object = create wksname + "_map" mapPlotClass wks
        "mpProjection" : projection
    end create

; Check to see if any resources were set.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

    attsetvalues_check(plot_object,res2)

    draw_and_frame(wks,plot_object,calldraw,callframe,maxbb)

; Return plot object.

    return(plot_object)
end

;***********************************************************************;
; Function : gsn_streamline                                             ;
;                   wks: workstation object                             ;
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
;   gsnFrame                                                            ;
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
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
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
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      getvalues overlay_plot_object
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

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_streamline_map                                         ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot over a map plot to  ;
; the workstation "wks" (the variable returned from a previous call to  ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
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
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    stream_object = create wksname + "_stream" streamlinePlotClass wks
        "stVectorFieldData" : data_object
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)

    vfres = get_res_eq(res2,"vf")
    mpres = get_res_ne(res2,(/"vf","mp","vp"/))
    stres = get_res_ne(res2,(/"vf","mp","vp"/))

    attsetvalues_check(data_object,vfres)
    attsetvalues_check(stream_object,stres)
    attsetvalues_check(map_object,mpres)

    overlay(map_object,stream_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(stream_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@streamline = stream_object
    return(map_object)
end

;***********************************************************************;
; Procedure : gsn_draw_colormap                                         ;
;                   wks: workstation object                             ;
;                                                                       ;
; This procedure retrieves the current colormap and draws it.           ;
; wks is a variable returned from a previous call to "gsn_open_wks".    ;
;***********************************************************************;
procedure gsn_draw_colormap(wks)
local nrows, ncols, ncolors, maxcols, ntotal, offset, width, height, \
xpos, ypos, xbox, ybox

begin
  nrows   = 16                   ; # of rows of colors per page.
  maxcols = 256                  ; max # of colors per color table.

  getvalues wks
    "wkColorMapLen" : ncolors    ; Get # of colors in color map.
  end getvalues

;
; Figure out ncols such that the columns will span across the page.
; Or, just set ncols to 16, which is big enough to cover the largest
; possible color map.
;
  ncols = floattoint(ncolors/nrows)
  if(ncols*nrows.lt.ncolors)
    ncols = ncols+1
  end if

  ntotal  = nrows * ncols        ; # of colors per page.
;
; If the number of colors in our color map is less than the allowed
; maximum, then this gives us room to add a white background and/or a
; black foreground.
;
  if(ncolors.lt.maxcols) then
;
; Get current color map.
;
    getvalues wks
      "wkColorMap" : cmap
    end getvalues

    if(ncolors.lt.maxcols-1) then
      offset = 2
      cmapnew = new((/ncolors+2,3/),float)
      cmapnew(0,:)  = (/1.,1.,1./)     ; white background
      cmapnew(1,:)  = (/0.,0.,0./)     ; black background
      cmapnew(2:,:) = cmap    
    else
      offset = 1
      cmapnew = new((/ncolors+1,3/),float)
      cmapnew(0,:)  = (/1.,1.,1./)     ; white background
      cmapnew(1:,:) = cmap    
    end if
;
; Set new color map.
;
    setvalues wks
      "wkColorMap" : cmapnew
    end setvalues

    delete(cmapnew)

  else
    offset = 0
  end if
;
; X and Y positions of text and box in the view port.
;
  width  = 1./ncols
  height = 1./nrows
  xpos  = fspan(0,1-width,ncols)
  ypos  = fspan(1-height,0,nrows)

;
; Box coordinates.
;
  xbox = (/0,width, width,     0,0/)
  ybox = (/0,    0,height,height,0/)

  font_height = 0.015
  font_space  = font_height/2.

  gonres   = True   ; variables to hold list of resources
  lineres  = True
  txres    = True

  txres@txFontHeightF         = font_height
  txres@txFont                = "helvetica-bold"
  txres@txJust                = "BottomLeft"
  txres@txPerimOn             = True
  txres@txPerimColor          = "black"  ; Or close to black if 
  txres@txFontColor           = "black"  ; black is not in color map.
  txres@txBackgroundFillColor = "white"  ; Or close to white.

  lineres@gsLineColor        = "black"

;
; ntotal colors per page.
;
  do k = 1,ncolors,ntotal
    jj = 0
    do j=k,min((/k+ntotal-1,ncolors/)),nrows
      ii = 0
      do i=j,min((/j+nrows-1,ncolors/))
;
; Draw box and fill in the appropriate color.
;
        gonres@gsFillColor = offset + (i-1)
        gsn_polygon_ndc(wks,xbox+xpos(jj),ybox+ypos(ii),gonres) ; Draw box.
;
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
; Put the original color map back.
;
  setvalues wks
    "wkColorMap" : cmap
  end setvalues
  delete(cmap)

end

;***********************************************************************;
; Function : gsn_vector                                                 ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;               resources: optional resources                           ;
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
;   gsnSpreadColors                                                     ;
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
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : data_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
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
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      getvalues overlay_plot_object
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

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_vector_contour                                         ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws vectors and contours to the           ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "data" is the scalar field to be contoured.           ;
; "resources" is an optional list of resources. The id of the vector    ;
; plot is returned.                                                     ;
;                                                                       ;
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

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create vector plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : vfdata_object
    end create

; Create contour plot object.

    contour_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : sfdata_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    cnres = get_res_eq(res2,(/"cn","vp"/))
    vcres = get_res_ne(res2,(/"vf","sf","cn","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(contour_object,cnres)
    attsetvalues_check(vector_object,vcres)

    if(sprdcols)
      cnres2 = True
      vcres2 = True

      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))

      attsetvalues(contour_object,cnres2)
      attsetvalues(vector_object,vcres2)
    end if

    overlay(vector_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object)
    end if

    draw_and_frame(wks,vector_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    vector_object@vfdata = vfdata_object
    vector_object@sfdata = sfdata_object
    vector_object@contour = contour_object
    return(vector_object)
end

;***********************************************************************;
; Function : gsn_streamline_contour                                     ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws streamlines and contours to the       ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; streamlines, and "data" is the scalar field to be contoured.          ;
; "resources" is an optional list of resources. The id of the streamline;
; plot is returned.                                                     ;
;                                                                       ;
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
function gsn_streamline_contour(wks:graphic,u[*][*]:numeric,\
                                v[*][*]:numeric,data[*][*]:numeric,\
                                resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, st_res_index, sf_res_index, res2
begin
    res2      = resources

;
; Create the streamline field object.
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
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create streamline plot object.

    stream_object = create wksname + "_stream" streamlinePlotClass wks
        "stVectorFieldData" : vfdata_object
    end create

; Create contour plot object.

    contour_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : sfdata_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    cnres = get_res_eq(res2,(/"cn","vp"/))
    stres = get_res_ne(res2,(/"vf","sf","cn","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(contour_object,cnres)
    attsetvalues_check(stream_object,stres)

    if(sprdcols)
      cnres2 = True

      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))

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

    stream_object@vfdata = vfdata_object
    stream_object@sfdata = sfdata_object
    stream_object@contour = contour_object
    return(stream_object)
end

;***********************************************************************;
; Function : gsn_vector_contour_map                                     ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws vectors and contours over a map plot  ;
; to the workstation "wks" (the variable returned from a previous call  ;
; to "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be   ;
; vectorized, and "data" is the scalar field to be contoured.           ;
; "resources" is an optional list of resources. The id of the map plot  ;
; is returned.                                                          ;
;                                                                       ;
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
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create vector plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : vfdata_object
    end create

; Create contour plot object.

    contour_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : sfdata_object
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    cnres = get_res_eq(res2,"cn")
    mpres = get_res_eq(res2,(/"mp","vp"/))
    vcres = get_res_ne(res2,(/"cn","mp","sf","vf","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(map_object,mpres)
    attsetvalues_check(contour_object,cnres)
    attsetvalues_check(vector_object,vcres)

    if(sprdcols)
      cnres2 = True
      vcres2 = True

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

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata = vfdata_object
    map_object@sfdata = sfdata_object
    map_object@vector = vector_object
    map_object@contour = contour_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_vector_map                                             ;
;                   wks: workstation object                             ;
;                     : 2-dimensional U data                            ;
;                     v: 2-dimensional V data                           ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "resources" is an optional list of resources. The id  ;
; of the map plot is returned.                                          ;
;                                                                       ;
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
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : data_object
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    mpres = get_res_eq(res2,(/"mp","vp"/))
    vcres = get_res_ne(res2,(/"mp","vf","vp"/))

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

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@vector = vector_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_vector_scalar                                          ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
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
;   gsnSpreadColorStart                                                 ;
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

; Create plot object.

    plot_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData"     : vfdata_object
        "vcScalarFieldData"     : sfdata_object
        "vcUseScalarArray"      : True
        "vcMonoLineArrowColor"  : False
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

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
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      getvalues overlay_plot_object
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

; Return plot object and data object (as attribute of plot object).

    plot_object@vfdata = vfdata_object
    plot_object@sfdata = sfdata_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_vector_scalar_map                                      ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "data" is the scalar field that the vectors are       ;
; colored by. "resources" is an optional list of resources. The id of   ;
; the map plot is returned.                                             ;
;                                                                       ;
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

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData"     : vfdata_object
        "vcScalarFieldData"     : sfdata_object
        "vcUseScalarArray"      : True
        "vcMonoLineArrowColor"  : False
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
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
    attsetvalues_check(sfdata_object,sfres)
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

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata = vfdata_object
    map_object@sfdata = sfdata_object
    map_object@vector = vector_object
    return(map_object)
end

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
; of the xy plot is returned.                                           ;
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

; Check for missing values.

    if(isatt(x,"_FillValue")) then
        setvalues data_object
            "caXMissingV" :x@_FillValue
        end setvalues
    end if
    if(isatt(y,"_FillValue")) then
        setvalues data_object
            "caYMissingV" :y@_FillValue
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

; By default, only solid lines get drawn if there are multiple lines, so
; set some dash patterns to use instead.  Also set different marker styles.

    getvalues plot_object
        "xyCoordDataSpec" : dspec
    end getvalues

    if(res2.and..not.any(ismissing(getvaratts(res2))))
      if(isatt(res2,"xyDashPattern").or.isatt(res2,"xyDashPatterns"))
        set_dash = False
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
            end if
            if(chartostring(res(0:1)).eq."xy".and.\
              (chartostring(res(0:3)).ne."xyCo".and.\
               chartostring(res(0:2)).ne."xyX".and.\
               chartostring(res(0:2)).ne."xyY"))
                setvalues dspec
                    attnames : res2@$attnames$
                end setvalues
            end if
        else
            xy_res_index = ind((chartostring(res(:,0:1)).ne."ca".and.\
                                chartostring(res(:,0:1)).ne."xy").or.\
                               (chartostring(res(:,0:1)).eq."xy".and.\
                               (chartostring(res(:,0:3)).eq."xyCo".or.\
                                chartostring(res(:,0:2)).eq."xyX".or.\
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
                attsetvalues_check(dspec,xydpres)
            end if
        end if

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

    draw_and_frame(wks,plot_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    plot_object@data     = data_object
    plot_object@dataspec = dspec
    return(plot_object)
end
