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
;;               http://www.ncl.ucar.edu/Document/Manuals/Getting_Started/
;;
;;               To use the functions and procedures in this script,
;;               you must have the line:
;;
;;                   load "gsn_code.ncl"
;; 
;;               at the top of your NCL script, before the begin statement.
;;

;***********************************************************************;
; function : smooth92d                                                  ;
;               var[*][*]:float                                         ;
;                       p:float                                         ;
;                       q:float                                         ;
;                                                                       ;
; Performs smoothing on a 2-dimensional array.                          ;
;                                                                       ;
;***********************************************************************;
undef("smooth92d")
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
        
  if(isdimnamed(var,0).and.iscoord(var,var!0))  then
    output!0 = var!0
    output&$var!0$ = var&$var!0$
  end if

  if(isdimnamed(var,1).and.iscoord(var,var!1))  then
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
undef("smooth93d")
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
        
  if(isdimnamed(var,0).and.iscoord(var,var!0))  then
    output!0 = var!0
    output&$var!0$ = var&$var!0$
  end if

  if(isdimnamed(var,1).and.iscoord(var,var!1))  then
    output!1 = var!1
    output&$var!1$ = var&$var!1$
  end if
  if(isdimnamed(var,2).and.iscoord(var,var!2))  then
    output!2 = var!2
    output&$var!2$ = var&$var!2$
  end if
  
  return(output)
end

;***********************************************************************;
; function : tofloat_wunits                                             ;
;                x:numeric                                              ;
;                                                                       ;
; Convert input to float and retain units attribute.                    ;
;                                                                       ;
; Note that after V5.1.1, a built-in version of "tofloat" was added.    ;
;                                                                       ;
;***********************************************************************;
undef("tofloat_wunits")
function tofloat_wunits(x:numeric)
local xf
begin
  xf = tofloat(x)
  if(isatt(x,"units").and..not.isatt(xf,"units")) then
    xf@units = x@units
  end if
  return(xf)
end

;***********************************************************************;
; function : stringtoxxx                                                ;
;             str : string                                              ;
;             type: string                                              ;
;                                                                       ;
; Convert string to int, float or double, depending on type             ; 
;                                                                       ;
;***********************************************************************;
undef("stringtoxxx")
function stringtoxxx(str:string,type:string)
begin
  if(type.eq."double")
    return(stringtodouble(str))
  else
    if(type.eq."float")
      return(stringtofloat(str))
    else
      if(type.eq."integer")
        return(stringtointeger(str))
      end if
    end if
  end if
  return(str)
end

;***********************************************************************;
; Function : get_long_name_units_string                                 ;
;            data : numeric                                             ;
;                                                                       ;
; This function checks if the long_name and units attributes exist, and ;
; if so, constructs a string using them. A missing value is returned    ;
; otherwise.                                                            ;
;***********************************************************************;
undef("get_long_name_units_string")
function get_long_name_units_string(data)
begin
  lu_string = new(1,string)
  if(isatt(data,"long_name")) then
    lu_string = data@long_name
;
; Comment out this code for now, because I'm not sure I want
; the default behavior to change from just a long_name string
; to a long_name (units) string for now.  This was added around
; version a031 (Jan 2004).
;
;    if(isatt(data,"units").and.data@units.ne."") then
;      lu_string = lu_string + " (" + data@units + ")"
;    end if
  end if
  return(lu_string)
end        

;***********************************************************************;
; Procedure : copy_var_atts                                             ;
;               var_from                                                ;
;               var_to                                                  ;
;              att_names: string                                        ;
; This is almost identical to D Shea's "copy_VarAtts" routine, except   ;
; it allows you to specify which atts to copy.                          ;
;***********************************************************************;
undef("copy_var_atts")
procedure copy_var_atts(var_from,var_to,att_names)
local i
begin                                       
  if(ismissing(att_names).or.att_names.eq."") then
    att_names_copy = getvaratts(var_from)
  else
    att_names_copy = att_names
  end if
  if(.not.all(ismissing(att_names_copy)))
    do i = 0,dimsizes(att_names_copy)-1
      if (isatt(var_from,att_names_copy(i)).and.\
          .not.isatt(var_to,att_names_copy(i))) then
	 var_to@$att_names_copy(i)$ = var_from@$att_names_copy(i)$
      end if
    end do
  end if
end

;***********************************************************************;
; Function : cat_strings                                                ;
;              strings : string                                         ;
;                                                                       ;
; Takes an array of strings and cats them into a single string.         ;
;***********************************************************************;
undef("cat_strings")
function cat_strings(str_array)
local i, slen
begin
  slen          = dimsizes(str_array)
  single_string = "'" + str_array(0) + "'"
  do i=1,slen-1
    single_string = single_string + " '" + str_array(i) + "'"
  end do

  return(single_string)
end

;***********************************************************************;
; This procedure removes the internal resources set by WRFUserARW.ncl
; These resources are used by the wrf_xxx plotting scripts, but are
; not recognized by the gsn_csm plotting scripts.
;***********************************************************************;
undef("remove_internal_wrf_resources")
procedure remove_internal_wrf_resources(res:logical)
local wlist, atts_set
begin
  wlist = (/"start_lat","start_lon","end_lat","end_lon","mpNestTime"/)
  atts_set = isatt(res,wlist)
  if(all(atts_set)) then
    do na=0,dimsizes(wlist)-1
      delete(res@$wlist(na)$)
    end do
  end if
end

;**********************************************************************
; This procedure fixes invalid settings of various labelbar resources
; related to contour plots.
;**********************************************************************
undef("_fix_labelbar_resources")
procedure _fix_labelbar_resources(cres)
local aln_res_name, end_res_name, aln_res_str_val, end_res_str_val
begin
  if(.not.cres) then
    return
  end if

  end_on_res   = "cnLabelBarEndLabelsOn"
  aln_res_name = "lbLabelAlignment"
  str_res_name = "lbLabelStrings"
  end_res_name = "cnLabelBarEndStyle"
  cap_res_name = "lbBoxEndCapStyle"
  if(isatt(cres,end_on_res)) then
    print("_fix_labelbar_resources: Warning: " + end_on_res + " is deprecated.")
    if(isatt(cres,end_res_name)) then
      print("  Your setting of " + end_res_name + " will override it.")
    else if(cres@$end_on_res$) then
      print("  Will remove it and set " + end_res_name + " to 'IncludeMinMaxLabels'.")
      cres@$end_res_name$ = "IncludeMinMaxLabels"
    else if(.not.cres@$end_on_res$) then
      print("  Will remove it.")
    end if
    end if
    end if
    delete(cres@$end_on_res$)
  end if

;
; The next set of checks are for if cnLabelBarEndStyle is set to
; something, and one of lbLabelAlignment, lbBoxEndCapStyle, or 
; lbLabelStrings is also set to something that doesn't make sense.
;
  if(.not.(isatt(cres,aln_res_name).or.\
           isatt(cres,cap_res_name).or.\
           isatt(cres,str_res_name)).and.\
     .not.isatt(cres,end_res_name)) then
    return
  end if

  if(isatt(cres,aln_res_name).and.\
     (.not.check_attr_enum_value(cres,aln_res_name,"ExternalEdges").and.\
      check_attr_enum_value(cres,end_res_name,"IncludeMinMaxLabels"))) then
    print("_fix_labelbar_resources: Warning: if you set " + end_res_name + " to")
    print("  IncludeMinMaxLabels, then you should not set " + aln_res_name + ".")
    print("  Will remove the "+ aln_res_name + " resource.")
    delete(cres@$aln_res_name$)
  end if

  if((isatt(cres,cap_res_name).and.isatt(cres,end_res_name)).and.\
     (.not.check_attr_enum_value(cres,cap_res_name,"RectangleEnds").and.\
           check_attr_enum_value(cres,end_res_name,"ExcludeOuterBoxes"))) then
    print("_fix_labelbar_resources: Warning: if you set " + end_res_name + " to")
    print("  ExcludeOuterBoxes, then you should not set " + cap_res_name + " to")
    print("  any of the triangle ends.")
    print("  Will remove the " + cap_res_name + " resource.")
    if(isatt(cres,str_res_name)) then
      print("  Be sure to check that " + str_res_name + " is correct!")
    end if
    delete(cres@$cap_res_name$)
  end if
end

;***********************************************************************;
; Function : get_resources                                              ;
;                res:logical                                            ;
;                                                                       ;
; This function takes a logical value and a list of resources, and      ;
; assigns them to another variable.  If res is False, however, then no  ;
; resources are carried over, but res is set to True for future use.    ;
;                                                                       ;
;***********************************************************************;
undef("get_resources")
function get_resources(res:logical)
begin
  if(res) then
    res2 = res
  else
;
; Set res2 to True, but don't carry over any resources.
;
    res2 = True
  end if
  remove_internal_wrf_resources(res2)
  _fix_labelbar_resources(res2)
  return(res2)
end

;***********************************************************************;
; Procedure : set_attr                                                  ;
;                res:logical                                            ;
;           att_name: string                                            ;
;          att_value                                                    ;
;                                                                       ;
; Add resource and its value to a resource list if it isn't already set.;
;***********************************************************************;
undef("set_attr")
procedure set_attr(res:logical,att_name:string,att_value)
begin
  res = True
  if(.not.isatt(res,att_name))
    res@$att_name$  = att_value
  end if
  return
end

;***********************************************************************;
; Procedure : delete_attr                                               ;
;                res:logical                                            ;
;           att_name: string                                            ;
;                                                                       ;
; Delete a resource if it exists.                                       ;
;***********************************************************************;
undef("delete_attr")
procedure delete_attr(res:logical,att_name:string)
begin
  if(isatt(res,att_name))
    delete(res@$att_name$)
  end if
  return
end

;***********************************************************************;
;                res:logical                                            ;
;             prefix: string                                            ;
;         rep_prefix: string                                            ;
;                                                                       ;
; Get a list of resources that start with res_prefix, and replace them  ;
; with rep_prefix.                                                      ;
;***********************************************************************;
undef("get_res_eq_replace")
function get_res_eq_replace(res,res_prefix:string,rep_prefix:string)
local i, j, ret_res, res2, attnames, res_index, nres2
begin
  ret_res = False

  if(res.and..not.any(ismissing(getvaratts(res))))
    attnames = getvaratts(res)
    do i = 0, dimsizes(attnames)-1
      res2  = stringtocharacter(attnames(i))
      nres2 =  dimsizes(res2)
;
; Loop through the resource prefixes and determine their length
; so that we only check that many characters in the resource name.
;
      do j = 0, dimsizes(res_prefix)-1
        res_prefix_c = stringtocharacter(res_prefix(j))
        rpclen       = dimsizes(res_prefix_c)-1   ; Don't count null char
        if(nres2.ge.rpclen.and. \
           charactertostring(res2(0:rpclen-1)).eq.res_prefix(j))
          ret_res = True
;
; Make sure we have enough room in the rest of the resource name to
; replace the current prefix with the rep_prefix. This code will take
; something like "gsnPanelFigureStringsFontHeightF" and replace it with
; "txFontHeightF".
;
          if(rep_prefix.ne."".and.nres2.gt.(rpclen+1)) then
            resnew_attname = rep_prefix + charactertostring(res2(rpclen:nres2-2))
            ret_res@$resnew_attname$ = res@$attnames(i)$
          else
            ret_res@$attnames(i)$ = res@$attnames(i)$
          end if
        end if
        delete(res_prefix_c)
      end do
      delete(res2)
    end do
    delete(attnames)
  end if
  return(ret_res)
end


;***********************************************************************;
; Function : get_res_eq                                                 ;
;                res:logical                                            ;
;             prefix: string                                            ;
;                                                                       ;
; Get a list of resources that start with res_prefix.                   ;
;***********************************************************************;
undef("get_res_eq")
function get_res_eq(res,res_prefix:string)
local i, j, ret_res, res2, attnames, res_index
begin
  return(get_res_eq_replace(res,res_prefix,""))
end


;***********************************************************************;
; Function : get_res_ne                                                 ;
;                res:logical                                            ;
;             prefix: string                                            ;
;                                                                       ;
; Get a list of resources that don't start with res_prefix.             ;
;***********************************************************************;
undef("get_res_ne")
function get_res_ne(res,res_prefix:string)
local i, j, ret_res, res2, attnames, res_index
begin
  ret_res = False

  if(res.and..not.any(ismissing(getvaratts(res))))
    attnames = getvaratts(res)
    do i = 0, dimsizes(attnames)-1
      res2 = stringtocharacter(attnames(i))
;
; Loop through the resource prefixes and determine their length
; so that we only check that many characters in the resource name.
;
      j = 0
      found = False
      do while(.not.found.and.j.le.dimsizes(res_prefix)-1)
        res_prefix_c = stringtocharacter(res_prefix(j))
        rpclen       = dimsizes(res_prefix_c)-1   ; Don't count null char
        if(dimsizes(res2).ge.rpclen.and.\
          charactertostring(res2(0:rpclen-1)).eq.res_prefix(j))
          found = True
        end if
        j = j + 1
        delete(res_prefix_c)
      end do
      if(.not.found) then
        ret_res = True
        ret_res@$attnames(i)$ = res@$attnames(i)$
      end if
      delete(res2)
    end do
    delete(attnames)
  end if
  return(ret_res)
end




;***********************************************************************;
; This function peruses two arrays of the same length and returns pairs ;
; of indices that represent ranges of data values where there are no    ;
; missing values.                                                       ;
;***********************************************************************;
undef("get_non_missing_pairs")
function get_non_missing_pairs(x[*]:numeric,y[*]:numeric)
local ibeg, iend, indices, ndimx, ndimy, is_missing
begin
  ndimx = dimsizes(x)
  ndimy = dimsizes(y)

  if(ndimx.ne.ndimy)
    print("get_non_missing_pairs: x and y must be the same length")
  end if

  indices = new((/ndimx,2/),integer)

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
; Function : get_display_mode                                           ;
;               res:logical                                             ;
;               name:string                                             ;
;               value                                                   ;
;                                                                       ;
; This procedure checks if a DisplayMode resource is set, and returns   ;
; an integer value if it's set as a string.                             ;
;***********************************************************************;
undef("get_display_mode")
function get_display_mode(res[1]:logical,name[1]:string,value[1])
local display_mode, new_display_mode 
begin
  display_mode = get_res_value_keep(res,name,value)

  if(typeof(display_mode).ne."string") then
    return(display_mode)
  end if
  
  new_display_mode = get_enum_value(display_mode)
  if(any(ismissing(new_display_mode))) then
    return(get_enum_value("NoCreate"))
  else
    return(new_display_mode)
  end if
end

;***********************************************************************;
; Procedure : attsetvalues_check                                        ;
;               plot:graphic                                            ;
;                res:logical                                            ;
;                                                                       ;
; This procedure passes plot and res to attsetvalues only if res is     ;
; True and non-empty.                                                   ;
;***********************************************************************;
undef("attsetvalues_check")
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
;                res:logical                                            ;
;                                                                       ;
;***********************************************************************;
; In Version 6.1.0, gsnSpreadColors is basically deprecated, and        ;
; replaced with cnSpanFillPalette, vcSpanFillPalette, etc, which are    ;
; True by default. If the user sets gsnSpreadColorStart and/or          ;
; gsnSpreadColorEnd to something other than their default values (2 and ;
; -1), then the spread_colors algorithm will kick in.                   ;
;***********************************************************************;
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
;                                                                       ;
; In NCL versions 6.0.0 and earlier, if you had both xxYYYYColors set   ;
; and gsnSpreadColors was set to True, then the xxYYYYColors resource   ;
; would be ignored, and gsnSpreadColors took precedence.                ;
;                                                                       ;
; In versions 6.1.0 and later, the opposite is True. gsnSpreadColors    ;
; is now True by default, but if you set xxYYYYColors, then this will   ;
; take precedence, and no message will be printed.                      ;
;***********************************************************************;
undef("spread_colors")
function spread_colors(wks:graphic,plot:graphic,min_index:integer,\
                        max_index:integer,res:logical)
local ncols, lcount, fcols, icols, minix, maxix, nc, fmin, fmax, class,\
levelcountres, cmap
begin
  if(.not.isvar("GSN_SPREAD_COLOR_DEBUG")) then
    GSN_SPREAD_COLOR_DEBUG = False
  end if
  if(GSN_SPREAD_COLOR_DEBUG) then
    print("spread_colors: at the top of this function")
  end if

  class = NhlClassName(plot)
  if(.not.any(class.eq.(/"contourPlotClass","logLinPlotClass",\
                         "irregularPlotClass","vectorPlotClass", \
                         "streamlinePlotClass"/)))
    print("spread_colors: invalid plot: defaulting")
    return(ispan(2,255,1))
  end if


  if (class.eq."contourPlotClass".or.class.eq."logLinPlotClass".or.\
      class.eq."irregularPlotClass")
    levelcountres = "cnLevelCount"
  else
    if (class.eq."vectorPlotClass") then
      levelcountres = "vcLevelCount"
    else
      levelcountres = "stLevelCount"
    end if
  end if

;**********************************************************************
; This code has to be replaced with the proper way to retrieve colors
; once 6.1.0
;**********************************************************************
  getvalues wks
    "wkColorMap" : cmap
  end getvalues
  ncols = dimsizes(cmap(:,0))

  if(GSN_SPREAD_COLOR_DEBUG) then
    print("spread_colors: size of current color map is " + ncols)
  end if

  if (class.eq."contourPlotClass".or.class.eq."vectorPlotClass".or. \
      class.eq."streamlinePlotClass")
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
    itmp    = maxix
    maxix   = minix
    minix   = itmp
  end if

  if(GSN_SPREAD_COLOR_DEBUG) then
    print("spread_colors: minix/maxix = " + minix + "/" + maxix)
  end if

  fmin = new(1,float)
  fmax = new(1,float)

  fmin = minix
  fmax = maxix
  fcols = fspan(fmin,fmax,lcount+1)
  if(.not.reverse)
    icols = tointeger(fcols + 0.5)
  else
    icols = tointeger(fcols(::-1) + 0.5)
  end if

  if(GSN_SPREAD_COLOR_DEBUG) then
    print("spread_colors: icols = " + icols)
  end if

;
; There may be some cases where you want to return the indexes
; here, for compatibility with NCL Version 6.0.0 and earlier.
;
  return(icols)
end

;***********************************************************************;
; Function : get_overlay_plot                                           ;
;                  plot  : graphic                                      ;
;             class_name : string                                       ;
;             plot_index : integer                                      ;
;                                                                       ;
; Get a specified overlaid plot id. This function is based on Dennis'   ;
; original GetOverlayPlot function.                                     ;
;***********************************************************************;
undef("get_overlay_plot")
function get_overlay_plot(plot:graphic,class_name:string,plot_index:integer)
begin
;
; Retrieve objects that have been overlaid on "plot".
; 
  getvalues plot
    "pmOverlaySequenceIds" : overlay_ids
  end getvalues
;
; Loop through these objects and check if any of them are a
; match.
;
  if(.not.any(ismissing(overlay_ids))) then
    num_instances = 0
    do i=0,dimsizes(overlay_ids)-1
      if(NhlClassName(overlay_ids(i)).eq.class_name)
        if(num_instances.eq.plot_index) then
          return(overlay_ids(i))
        end if
        num_instances = num_instances + 1
      end if
    end do
  end if
;
; If no match found, then check the plot itself.
;
  if(NhlClassName(plot).eq.class_name) then
    return(plot)
  end if
;
; No match found, so return a missing object.
;
  print("get_overlay_plot: Error: no plot found matching conditions")
  print("                  Returning a missing value.")
  dum = new(1,graphic)
  return(dum)
end

;***********************************************************************;
; Function : get_contour_levels                                         ;
;               plot: graphic                                           ;
;                                                                       ;
; Get contour levels associated with "plot".                            ;
;                                                                       ;
;***********************************************************************;
undef("get_contour_levels")
function get_contour_levels(plot:graphic)
local overlay_plot
begin
  overlay_plot = get_overlay_plot (plot, "contourPlotClass", 0)

  if(.not.ismissing(overlay_plot)) then
    getvalues overlay_plot
      "cnLevels"     : levels
      "cnLevelFlags" : level_flags
    end getvalues
    levels@flags = level_flags
    return(levels)
  end if
;
; Return missing if no plot was found.
;
  dum = new(1,float)
  return(dum)
end

;***********************************************************************;
; Function : get_contour_line_thicknesses                               ;
;               plot: graphic                                           ;
;                                                                       ;
; Get contour line thicknesses associated with "plot".                  ;
;                                                                       ;
;***********************************************************************;
undef("get_contour_line_thicknesses")
function get_contour_line_thicknesses(plot:graphic)
local overlay_plot
begin
  overlay_plot = get_overlay_plot (plot, "contourPlotClass", 0)

  if(.not.ismissing(overlay_plot)) then
    getvalues overlay_plot
      "cnLineThicknesses" : thicknesses
    end getvalues
    return(thicknesses)
  end if
;
; Return missing if no plot was found.
;
  dum = new(1,float)
  return(dum)
end

;***********************************************************************;
; Function : fix_zero_contour                                           ;
;              levels:numeric                                           ;
;                                                                       ;
; Make sure the 0th contour (if it exists) really is "0" and not        ;
; something like "1.00001e-08".  But, we also have to make sure we don't;
; have values like 1e-10, 1e-9, etc, where we *do* want "1e-10" label   ;
; and not a "0". Don't even bother with checking if the minimum         ;
; difference between the levels is less than 1e-5.                      ;
;***********************************************************************;
undef("fix_zero_contour")
function fix_zero_contour(levels)
begin
  nlevels = dimsizes(levels)
  if(nlevels.gt.1) then
    delta_levels = min( levels(1:nlevels-1) - levels(0:nlevels-2) )

    if (ismissing(delta_levels)) then
      return(levels)
    end if

    if(delta_levels.ge.1e-5)
      do n=1,nlevels-2
        if(fabs(levels(n)).le.1.e-5.and.levels(n-1).lt.0..and.levels(n+1).gt.0.)
          levels(n) = 0.0
        end if
      end do
    end if
  end if  
  return(levels)
end

;***********************************************************************;
; Function : set_zero_line_thickness                                    ;
;                plot : graphic                                         ;
;           zthickness : numeric                                        ;
;           cthickness : numeric                                        ;
;                                                                       ;
; Make the 0-th contour line the given thickness.                       ;
;                                                                       ;
; Note that this function now recognizes the cnLineThicknesses resource ;
; and will set the contour lines to this thickness, if set.             ;
;                                                                       ;
; If thickness is equal to 0, then the line is just not drawn.          ;
;                                                                       ;
;***********************************************************************;
undef("set_zero_line_thickness")
function set_zero_line_thickness(plot:graphic,zthickness,cthickness)
begin
  levels  = get_contour_levels (plot)
  nlevels = dimsizes(levels)

  if (any(ismissing(levels)).or. nlevels.le.0) then
    print ("set_zero_line_thickness: invalid contour levels, returning...")
    return (plot)
  end if

  levels      = fix_zero_contour (levels)
  thicknesses = new(nlevels, float)
;
; If cthickness is an array, then deal with that here.
;
  ncthk = dimsizes(cthickness)
  if(ncthk.gt.1) then
    thicknesses = 1.      ; First, default them to default thickness.
    i = 0
    do while(i.lt.ncthk.and.i.lt.nlevels)
      thicknesses(i) = cthickness(i)
      i = i + 1
    end do
  else
    thicknesses = cthickness
  end if

  zeroind = ind(levels.eq.0.0)      ; Get index where level equals 0.0
  if(.not.ismissing(zeroind)) then
    if(zthickness.gt.0) then
      thicknesses(zeroind) = zthickness
   else
      thicknesses(zeroind)  = 1.    ; Make it 1.0, but it doesn't matter
                                    ; b/c we are turning off the drawing
                                    ; of this line.
      levels@flags(zeroind) = 0     ; Turn off the zero contour line
    end if
  end if

  overlay_plot = get_overlay_plot (plot, "contourPlotClass", 0)
   
  if(zthickness.gt.0) then
    setvalues overlay_plot
      "cnMonoLineThickness" : False
      "cnLineThicknesses"   : thicknesses
    end setvalues
  else
    setvalues overlay_plot
      "cnMonoLineThickness" : False
      "cnLevelFlags"        : levels@flags
      "cnLineThicknesses"   : thicknesses
    end setvalues
  end if

  return (plot)
end

;***********************************************************************;
; Function : set_line_thickness_scale                                   ;
;                plot : graphic                                         ;
;               scale : numeric                                         ;
;                                                                       ;
; Scale the line thickness by the given numbers.                        ;
;                                                                       ;
;***********************************************************************;
undef("set_line_thickness_scale")
function set_line_thickness_scale(plot:graphic,scale)
begin
  thicknesses  = get_contour_line_thicknesses (plot)
  nthicknesses = dimsizes(thicknesses)

  if (any(ismissing(thicknesses)).or. nthicknesses.le.0) then
    print ("set_line_thickness_scale: invalid contour line thicknesses, returning...")
    return (plot)
  end if

  thicknesses = scale * thicknesses
  
  overlay_plot = get_overlay_plot (plot, "contourPlotClass", 0)
  setvalues overlay_plot
    "cnMonoLineThickness" : False
    "cnLineThicknesses"   : thicknesses
  end setvalues

  return (plot)
end

;***********************************************************************;
; Function : set_pos_neg_line_pattern                                   ;
;                plot : graphic                                         ;
;             npattern : numeric                                        ;
;             ppattern : numeric                                        ;
;                                                                       ;
; Set the dash pattern of negative and/or positive contour lines.       ;
;                                                                       ;
; Note that this function also sets the rest of the line patterns to 0  ;
; if both npattern and ppattern aren't both set.                        ;
; The user can use the cnLineDashPatterns resource to override this.    ;
;                                                                       ;
;***********************************************************************;
undef("set_pos_neg_line_pattern")
function set_pos_neg_line_pattern(plot:graphic,ppattern,npattern)
local set_pos, set_neg, levels, n, nlevels, patterns, overlay_plot
begin
  levels  = get_contour_levels (plot)
  nlevels = dimsizes(levels)

  if (any(ismissing(levels)) .or. nlevels.le.0) then
    print ("set_pos_neg_line_pattern: invalid contour levels, returning...")
    return (plot)
  end if

  levels   = fix_zero_contour (levels)
  patterns = new(nlevels,integer)
  patterns = 0                                 ; default to solid line.

;
; Check if we have negative and/or positive patterns to set.
;
  if(.not.any(ismissing(npattern))) then
    set_neg = True
  else
    set_neg = False
  end if

  if(.not.any(ismissing(ppattern))) then
    set_pos = True
  else
    set_pos = False
  end if

;
; Loop through and set each contour level, if applicable.
;
  do n=0,nlevels-1
    if (set_neg.and.levels(n).lt.0.) then
      patterns(n) = npattern
    end if
    if (set_pos.and.levels(n).gt.0.) then
      patterns(n) = ppattern
    end if
  end do

  overlay_plot = get_overlay_plot (plot, "contourPlotClass", 0)

  setvalues overlay_plot
    "cnMonoLineDashPattern" : False
    "cnLineDashPatterns"    : patterns
  end setvalues

  return (plot)
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
undef("check_for_irreg2loglin")
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

;***********************************************************************;
;                                                                       ;
; This function checks if a data array is 1D or 2D and returns False if ;
; is not.                                                               ;
;                                                                       ;
;***********************************************************************;
undef("is_data_1d_or_2d")
function is_data_1d_or_2d(data)
begin
  dims = dimsizes(data)
  rank = dimsizes(dims)

  if(rank.eq.1.or.rank.eq.2) then
    return(True)
  else
    return(False)
  end if
end


;***********************************************************************;
; Function : overlay_irregular                                          ;
;                        wks:graphic                                    ;
;                    wksname: string                                    ;
;        overlay_plot_object: graphic                                   ;
;                data_object: graphic                                   ;
;                    xlinear: logical                                   ;
;                    ylinear: logical                                   ;
;                       xlog: logical                                   ;
;                       ylog: logical                                   ;
;                       type: string                                    ;
;                      llres: logical                                   ;
;                                                                       ;
; If xlinear and/or ylinear are set to linear or log, then overlay      ;
; plot on an irregularPlotClass so that we can linearize or logize      ;
; the appropriate axis.                                                 ;
;***********************************************************************;
undef("overlay_irregular")
function overlay_irregular(wks,wksname,overlay_plot_object:graphic,\
                           data_object:graphic,xlinear:logical, \
                           ylinear:logical,xlog:logical,ylog:logical,\
                           type:string,llres:logical)
local xaxistype,yaxistype,trxmin,trymin,trxmax,trymax,Xpts,Ypts,is_tm_mode
begin

  if(xlinear) then
    xaxistype = "LinearAxis"
  else  
    if(xlog) then
      xaxistype = "LogAxis"
    else
      xaxistype = "IrregularAxis"
    end if
  end if

  if(ylinear) then
    yaxistype = "LinearAxis"
  else  
    if(ylog) then
      yaxistype = "LogAxis"
    else
      yaxistype = "IrregularAxis"
    end if
  end if
;
; Retrieve information about existing plot so we can use these values
; to create new overlay plot object.
;
  getvalues overlay_plot_object
    "trXMinF"    : trxmin
    "trXMaxF"    : trxmax
    "trYMinF"    : trymin
    "trYMaxF"    : trymax
  end getvalues

  if(type.eq."contour") then
    getvalues data_object
      "sfXArray"   : Xpts
      "sfYArray"   : Ypts
    end getvalues
  else 
    if(type.eq."vector".or.type.eq."streamline") then
      getvalues data_object
        "vfXArray"   : Xpts
        "vfYArray"   : Ypts
      end getvalues
    end if
  end if
;
; If x/yaxistype is irregular, then we must set trX/YCoordPoints.
; Oherwise, we can't set trX/YCoordPoints, because we'll get an
; error message. So, we have to do all kinds of tests to see which
; axes are irregular, and which ones are log or linear.
;
; Also, if Xpts or Ypts are missing, this means the corresponding
; axis can't be irregular.
;
  if(any(ismissing(Xpts)).and.xaxistype.eq."IrregularAxis") then
    xaxistype = "LinearAxis"
  end if

  if(any(ismissing(Ypts)).and.yaxistype.eq."IrregularAxis") then
    yaxistype = "LinearAxis"
  end if
;
; If both axes at this point are Irregular, then there is no point
; in overlaying it on an irregular plot class. We then have
; three possible cases:
;
;   Case 1: Both X and Y axes are either linear or log.
;   Case 2: X axis is irregular and Y axis is linear or log.
;   Case 3: Y axis is irregular and X axis is linear or log.
;
  if(xaxistype.eq."IrregularAxis".and.yaxistype.eq."IrregularAxis") then
    return(overlay_plot_object)
  else
;
; Case 1
;
; If pmTickMarkDisplayMode is set, we need to set it during the
; create call, and not afterwards.  But, we can't assume a default
; default value, because it varies depending on the type of plot
; being created. So, we have to do the kludgy thing of checking
; for it, and only setting it if the user has set it.
;
    is_tm_mode = isatt(llres,"pmTickMarkDisplayMode")
    if(is_tm_mode) then
      tm_mode = get_display_mode(llres,"pmTickMarkDisplayMode","NoCreate")
      delete(llres@pmTickMarkDisplayMode)
    end if
    if(xaxistype.ne."IrregularAxis".and. \
       yaxistype.ne."IrregularAxis") then
      if(is_tm_mode) then
        plot_object = create wksname + "_irregular" irregularPlotClass wks
          "pmTickMarkDisplayMode" : tm_mode
          "trXAxisType"    : xaxistype
          "trYAxisType"    : yaxistype
          "trXMinF"        : trxmin
          "trXMaxF"        : trxmax
          "trYMinF"        : trymin
          "trYMaxF"        : trymax
        end create
      else
        plot_object = create wksname + "_irregular" irregularPlotClass wks
          "trXAxisType"    : xaxistype
          "trYAxisType"    : yaxistype
          "trXMinF"        : trxmin
          "trXMaxF"        : trxmax
          "trYMinF"        : trymin
          "trYMaxF"        : trymax
      end create
      end if
    end if
;
; Case 2
;
    if(xaxistype.eq."IrregularAxis".and. \
       yaxistype.ne."IrregularAxis") then
      if(is_tm_mode) then
        plot_object = create wksname + "_irregular" irregularPlotClass wks
          "pmTickMarkDisplayMode" : tm_mode
          "trXAxisType"    : xaxistype
          "trYAxisType"    : yaxistype
          "trXCoordPoints" : Xpts
          "trXMinF"        : trxmin
          "trXMaxF"        : trxmax
          "trYMinF"        : trymin
          "trYMaxF"        : trymax
        end create
      else
        plot_object = create wksname + "_irregular" irregularPlotClass wks
          "trXAxisType"    : xaxistype
          "trYAxisType"    : yaxistype
          "trXCoordPoints" : Xpts
          "trXMinF"        : trxmin
          "trXMaxF"        : trxmax
          "trYMinF"        : trymin
          "trYMaxF"        : trymax
        end create
      end if
    end if
;
; Case 3
;
    if(yaxistype.eq."IrregularAxis".and. \
       xaxistype.ne."IrregularAxis") then
      if(is_tm_mode) then
        plot_object = create wksname + "_irregular" irregularPlotClass wks
          "pmTickMarkDisplayMode" : tm_mode
          "trXAxisType"    : xaxistype
          "trYAxisType"    : yaxistype
          "trYCoordPoints" : Ypts
          "trXMinF"        : trxmin
          "trXMaxF"        : trxmax
          "trYMinF"        : trymin
          "trYMaxF"        : trymax
        end create
      else
        plot_object = create wksname + "_irregular" irregularPlotClass wks
          "trXAxisType"    : xaxistype
          "trYAxisType"    : yaxistype
          "trYCoordPoints" : Ypts
          "trXMinF"        : trxmin
          "trXMaxF"        : trxmax
          "trYMinF"        : trymin
          "trYMaxF"        : trymax
        end create
      end if
    end if

    attsetvalues_check(plot_object,llres)

    overlay(plot_object,overlay_plot_object)
    plot_object@$type$ = overlay_plot_object
  end if

  return(plot_object)
end

;***********************************************************************;
; function : get_plot_not_loglin                                        ;
;                plot:graphic                                           ;
;                                                                       ;
; Determine what class type "plot" is. If it's a logLinPlotClass, then  ;
; It should have an attribute "contour", "vector", "xy", or             ;
; "streamine" that is the corresponding contour or vector plot.         ;
;                                                                       ;
;***********************************************************************;
undef("get_plot_not_loglin")
function get_plot_not_loglin(plot:graphic)
local class
begin
  new_plot           = new(1,graphic)
  new_plot@plot_type = "unknown"

  class = NhlClassName(plot)

  if(class(0).ne."contourPlotClass".and.class(0).ne."vectorPlotClass".and. \
     class(0).ne."xyPlotClass".and.class(0).ne."streamlinePlotClass") then
    if(isatt(plot,"contour")) then
      new_plot = plot@contour
      new_plot@plot_type = "contour"
    else if(isatt(plot,"vector")) then
      new_plot = plot@vector
      new_plot@plot_type = "vector"
    else if(isatt(plot,"streamline")) then
      new_plot = plot@streamline
      new_plot@plot_type = "streamline"
    else
      found = False
      getvalues plot
        "pmOverlaySequenceIds" : base_ids
      end getvalues
      if(.not.any(ismissing(base_ids))) then
        nbase = dimsizes(base_ids)
        i = 0
        do while(.not.found.and.i.lt.nbase) 
          bclass = NhlClassName(base_ids(i))
          if(bclass.eq."contourPlotClass") then
            new_plot = base_ids(i)
            new_plot@plot_type = "contour"
            found = True
          else if(bclass.eq."vectorPlotClass") then
            new_plot = base_ids(i)
            new_plot@plot_type = "vector"
            found = True
          else if(bclass.eq."streamlinePlotClass") then
            new_plot = base_ids(i)
            new_plot@plot_type = "streamline"
            found = True
          else if(bclass.eq."xyPlotClass") then
            new_plot = base_ids(i)
            new_plot@plot_type = "xy"
            found = True
          end if
          end if
          end if
          end if
          i = i + 1
        end do
      end if
      if(.not.found) then
        new_plot = plot(0)
      end if
    end if
    end if
    end if
  else
    if(class(0).eq."contourPlotClass") then
      new_plot = plot(0)
      new_plot@plot_type = "contour"
    else if(class(0).eq."vectorPlotClass") then
      new_plot = plot(0)
      new_plot@plot_type = "vector"
    else if(class(0).eq."streamlinePlotClass") then
      new_plot = plot(0)
      new_plot@plot_type = "streamline"
    else if(class(0).eq."xyPlotClass") then
      new_plot = plot(0)
      new_plot@plot_type = "xy"
    end if
    end if
    end if
    end if
  end if
  return(new_plot)
end

;***********************************************************************;
; function : get_plot_labelbar                                          ;
;                plot:graphic                                           ;
;                                                                       ;
; This function is for gsn_panel, to help it determine which plot info  ;
; to use to construct a labelbar.                                       ;
;                                                                       ;
; In a future release of NCL we hope to make the PlotManager the        ;
; default labelbar, so we added some checks to this code for            ; 
; pmLabelBarDisplayMode being equal to "Always".                        ;
;***********************************************************************;
undef("get_plot_labelbar")
function get_plot_labelbar(plot:graphic)
local found, id_class, i, ids, nids, class_name
begin
  new_plot = new(1,graphic)

;
; If new_plot contains a "labelbar" attribute, it likely came
; from one of the vector_scalar functions, in which the
; labelbar could either be from filled contours, or colored
; vectors.
;
  if(isatt(plot,"labelbar").and.isatt(plot,"labelbar_type")) then
    new_plot           = plot@labelbar
    new_plot@plot_type = plot@labelbar_type
    return(new_plot)
  end if
;
; This part was added in NCL V6.1.0, to test for a PlotManager
; labelbar. If it's there, then return this.
;
  class_name = NhlClassName(plot)
  if(any(class_name.eq.(/"ContourPlot","VectorPlot","StreamlinePlot"/))) then
    getvalues plot
      "pmLabelBarDisplayMode" : lbar_mode
    end getvalues
    if(lbar_mode.eq.get_enum_value("Always")) then
      new_plot = plot
      new_plot@PlotManagerLabelBar = True
      return(new_plot)
    end if
  end if
;
; This code was added after V5.1.1. It is better to use the
; overlay ids to detect what overlay plots there are, and
; then figure out from resources set which ones might 
; potentially require a labelbar.
;
; Note that this test will favor contour plots over
; vector over streamline plots, and it will use the
; first plot that it finds.
;
  getvalues plot
    "pmOverlaySequenceIds" : ids
  end getvalues
  nids = dimsizes(ids)
  found = False
  i = 0
  do while(.not.found.and.i.lt.nids) 
    id_class = NhlClassName(ids(i))
    if(id_class.eq."contourPlotClass") then
      getvalues ids(i)
        "cnFillOn"              : cn_fillon
        "pmLabelBarDisplayMode" : lbar_mode
      end getvalues 
      if(lbar_mode.eq.get_enum_value("Always").or.cn_fillon) then
        found = True
        new_plot = ids(i)
        new_plot@plot_type = "contour"
      end if
    end if
    if(id_class.eq."vectorPlotClass") then
      getvalues ids(i)
        "vcGlyphStyle"             : vc_glyphstyle
        "vcMonoFillArrowFillColor" : vc_monofillarrowfillcolor
        "vcMonoLineArrowColor"     : vc_monolinearrowcolor
        "vcMonoWindBarbColor"      : vc_monowindbarbcolor
        "pmLabelBarDisplayMode"    : lbar_mode
      end getvalues
      if( (lbar_mode.eq.get_enum_value("Always")) .or.\
          (vc_glyphstyle.eq.get_enum_value("LineArrow").and..not.vc_monolinearrowcolor) .or.\
          (vc_glyphstyle.eq.get_enum_value("FillArrow").and..not.vc_monofillarrowfillcolor) .or. \
          (vc_glyphstyle.eq.get_enum_value("WindBarb").and..not.vc_monowindbarbcolor)  .or. \
          (vc_glyphstyle.eq.get_enum_value("CurlyVector").and..not.vc_monolinearrowcolor)) then
        found = True
        new_plot = ids(i)
        new_plot@plot_type = "vector"
      end if
    end if
    if(id_class.eq."streamlinePlotClass") then
      getvalues ids(i)
        "stMonoLineColor"       : st_monolinecolor
        "pmLabelBarDisplayMode" : lbar_mode
      end getvalues
      if(lbar_mode.eq.get_enum_value("Always").or..not.st_monolinecolor) then
        found = True
        new_plot = ids(i)
        new_plot@plot_type = "streamline"
      end if
    end if
    i = i + 1
  end do
  if(found) then
    return(new_plot)
  end if
;
; If all else fails, do it the "old" way before V5.2.0.
;
  new_plot = get_plot_not_loglin(plot)
  return(new_plot)
end

;**********************************************************************;
; Function : maximize_bb                                               ;
;               plot : graphic                                         ;
;                res : logical                                         ;
;                                                                      ;
; This function computes the viewport coordinates needed to optimize   ;
; the size of a plot on a page. If the plot is too big for the         ;
; viewport, then this function will decrease the plot size.            ;
;                                                                      ;
; plot : plot to maximize on the page.                                 ;
;                                                                      ;
; res : list of optional resources. Ones accepted include:             ;
;                                                                      ;
;     "gsnBoxMargin" - margin to leave around plots (in NDC units,     ;
;                     default is 0.02)                                 ;
;                                                                      ;
;**********************************************************************;
undef("maximize_bb")
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
; Calculate scale factor needed to make plot larger (or smaller, if it's
; outside the viewport).
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
; This procedure writes a series of strings to a given string array and ;
; increments the line counter.                                          ;
;***********************************************************************;
undef("write_lines")
procedure write_lines(strs, thelines, numlines)
local nstrs
begin
  nstrs   = dimsizes(strs)        ; Number of strings to write.
  ntlines = dimsizes(thelines)    ; Number of available lines.

  if((numlines+nstrs).gt.ntlines) then
    print("write_lines: error: array is not big enough to hold new lines")
    print("             no lines will be written")
    return
  end if

  thelines(numlines:numlines+nstrs-1) = strs          ; Add strings
  numlines                            = numlines + nstrs
end

;***********************************************************************;
; This procedure is to be used in conjunction with gsnp_write_debug_info;
; It takes care of the special case where "lat2d" and "lon2d" attributes;
; may be attached to the data array. You can't write 2D attributes to a ;
; netCDF file, so we write them as 1D with information that we can use  ;
; to reconstruct the 2D array later.                                    ;
;***********************************************************************;
undef("add_latlon2d_debug_info")
procedure add_latlon2d_debug_info(data)
local dims_lat, dims_lon
begin
  if(isatt(data,"lat2d")) then
    dims_lat = dimsizes(data@lat2d)
    if(dimsizes(dims_lat).eq.2) then
      data@lat1d_yndim = dims_lat(0)
      data@lat1d_xndim = dims_lat(1)
      data@lat1d       = ndtooned(data@lat2d)
      delete(data@lat2d)
    end if
  end if
  if(isatt(data,"lon2d")) then
    dims_lon = dimsizes(data@lon2d)
    if(dimsizes(dims_lon).eq.2) then
      data@lon1d_yndim = dims_lon(0)
      data@lon1d_xndim = dims_lon(1)
      data@lon1d       = ndtooned(data@lon2d)
      delete(data@lon2d)
    end if
  end if
end

;***********************************************************************;
; This procedure is used in conjunction with add_laton2d_debug_info.    ;
; It adds back lat2d/lon2d attributes that were removed earlier, and    ;
; removes attribute info that was added to a data array (all in support ;
; of the gsnp_write_debug_info procedure).                              ;
;***********************************************************************;
undef("fix_latlon2d_debug_info")
procedure fix_latlon2d_debug_info(ldata)
begin
  if(isatt(ldata,"lat1d").and. \
     isatt(ldata,"lat1d_yndim").and.\
     isatt(ldata,"lat1d_xndim").and.\
     isatt(ldata,"lon1d").and. \
     isatt(ldata,"lon1d_yndim").and.\
     isatt(ldata,"lon1d_xndim")) then
    ldata@lat2d = onedtond(ldata@lat1d,(/ldata@lat1d_yndim,ldata@lat1d_xndim/))
    ldata@lon2d = onedtond(ldata@lon1d,(/ldata@lon1d_yndim,ldata@lon1d_xndim/))
    delete(ldata@lat1d_yndim)
    delete(ldata@lat1d_xndim)
    delete(ldata@lon1d_yndim)
    delete(ldata@lon1d_xndim)
    delete(ldata@lat1d)
    delete(ldata@lon1d)
  end if
end

;***********************************************************************;
; This procedure is used in conjunction with add_laton2d_debug_info.    ;
; and gsnp_write_debug_info. It writes lines to an NCL script that      ;
; reconstructs lat2d and lon2d attribute information.                   ;
;***********************************************************************;
undef("write_latlon2d_lines")
procedure write_latlon2d_lines(ldata, ldata_name[1]:string, thelines, numlines)
local nlines
begin
  if(isatt(ldata,"lat1d").and. \
     isatt(ldata,"lat1d_yndim").and.\
     isatt(ldata,"lat1d_xndim").and.\
     isatt(ldata,"lon1d").and. \
     isatt(ldata,"lon1d_yndim").and.\
     isatt(ldata,"lon1d_xndim")) then
    nlines = (/"  " + ldata_name + "@lat2d = onedtond(" + ldata_name + \
               "@lat1d,(/" + ldata_name + "@lat1d_yndim," + ldata_name + \
               "@lat1d_xndim/))", \
               "  " + ldata_name + "@lon2d = onedtond(" + ldata_name + \
               "@lon1d,(/" + ldata_name + "@lon1d_yndim," + ldata_name + \
               "@lon1d_xndim/))"/)
    
    write_lines( nlines, thelines, numlines)
  end if
end

;***********************************************************************;
; This procedure writes a standard resource file that changes the font  ;
; to helvetica, foreground/background colors to white/black, and the    ;
; function code to "~".                                                 ;
;***********************************************************************;
undef("write_debug_res_file")
procedure write_debug_res_file(resfname[1]:string)
local reslines
begin
  reslines = (/"*wkForegroundColor : (/0.,0.,0./)", \
               "*wkBackgroundColor : (/1.,1.,1./)", \
               "*Font              : helvetica",    \
               "*TextFuncCode      : ~"/)

  asciiwrite(resfname,reslines)
end

;***********************************************************************;
; This procedure writes some debug info (data and plot resource values) ;
; to a netCDF file and creates an NCL file to plot the data.            ;
;***********************************************************************;
undef("gsnp_write_debug_info")
procedure gsnp_write_debug_info(data1,data2,data3,gsn_name[1]:string,\
                                plot_res[1]:logical,ndfiles[1]:integer)
begin
;
; Can only deal with up to three data files.
;
  if(ndfiles.lt.0.or.ndfiles.gt.3) then
    print("gsnp_write_debug_info: error: can only have 0-3 data files.")
    exit
  end if
;
; Valid plot function names that this routine can be use for.
;
  gsn_names = (/"gsn_contour", "gsn_contour_map", "gsn_csm_contour", \
                "gsn_csm_contour_map", "gsn_csm_contour_map_ce", \
                "gsn_csm_contour_map_other", \
                "gsn_csm_contour_map_polar", "gsn_csm_hov", \
                "gsn_csm_lat_time", "gsn_csm_map", "gsn_csm_map_ce", \
                "gsn_csm_map_other", "gsn_csm_map_polar", \
                "gsn_csm_pres_hgt", "gsn_csm_pres_hgt_streamline", \
                "gsn_csm_pres_hgt_vector", "gsn_csm_streamline", \
                "gsn_csm_streamline_contour_map", \
                "gsn_csm_streamline_contour_map_ce", \
                "gsn_csm_streamline_contour_map_other", \
                "gsn_csm_streamline_contour_map_polar", \
                "gsn_csm_streamline_map", "gsn_csm_streamline_map_ce", \
                "gsn_csm_streamline_map_other", \
                "gsn_csm_streamline_map_polar",  \
                "gsn_csm_streamline_scalar_map", \
                "gsn_csm_streamline_scalar_map_ce", \
                "gsn_csm_streamline_scalar_map_other", \
                "gsn_csm_streamline_scalar_map_polar", \
                "gsn_csm_time_lat", "gsn_csm_vector", "gsn_csm_vector_map", \
                "gsn_csm_vector_map_ce", "gsn_csm_vector_map_other", \
                "gsn_csm_vector_map_polar", "gsn_csm_vector_scalar", \
                "gsn_csm_vector_scalar_map", \
                "gsn_csm_vector_scalar_map_ce", \
                "gsn_csm_vector_scalar_map_other", \
                "gsn_csm_vector_scalar_map_polar", \
                "gsn_csm_xy", "gsn_csm_y", "gsn_histogram", "gsn_map", \
                "gsn_streamline", "gsn_streamline_contour", \
                "gsn_streamline_map", "gsn_vector", \
                "gsn_vector_contour", "gsn_vector_contour_map", \
                "gsn_vector_map", "gsn_vector_scalar", "gsn_streamline_scalar", \
                "gsn_vector_scalar_map", "gsn_streamline_scalar_map", \
                "gsn_xy", "gsn_y"/)

  if(.not.any(gsn_name.eq.gsn_names)) then
    print("gsnp_write_debug_info: error: do not recognize " + gsn_name + " as a")
    print("valid plot function name that can be used with this procedure.")
    exit
  end if

;
; Create netCDF and NCL files to write to. The default will be
; "debug.ncl" and "debug.nc" unless otherwise specified.
;
  debug_file = get_res_value(plot_res,"gsnDebugWriteFileName", \
                             unique_string("debug"))

  cdf_debug_file = debug_file +  ".nc"
  ncl_debug_file = debug_file +  ".ncl"
  res_debug_file = debug_file +  ".res"

  if(fileexists(cdf_debug_file).or.fileexists(ncl_debug_file).or.\
     fileexists(res_debug_file)) then
    print("gsnp_write_debug_info: error: debug files '" + cdf_debug_file + "',")
    print("'" + ncl_debug_file + "' and/or " + res_debug_file + " exist.")
    print("Please remove file(s) and start script again.")
    exit
  else
    dfile = addfile(cdf_debug_file,"c")
  end if

;
; Write the plot data to the netCDF file.  If the data contains the
; special 2D lat2d/lon2d arrays, we have to write these as 1D arrays
; and reconstruct them as 2D later.
;
  if(ndfiles.ge.1) then
    add_latlon2d_debug_info(data1)
    dfile->PlotData = data1           ; Write the data
  end if
  if(ndfiles.ge.2) then
    add_latlon2d_debug_info(data2)
    dfile->PlotData2 = data2
  end if
  if(ndfiles.ge.3) then
    add_latlon2d_debug_info(data3)
    dfile->PlotData3 = data3
  end if

  dfile@gsn_function = stringtochar(gsn_name)  ; Write name of gsn_xxx routine

;
; If the colormap is not a name of a colormap, or a string array of
; colors, then this means we have a colormap that we need to write
; to the netCDF file as data.
;
  colormap = get_res_value(plot_res,"gsnDebugWriteColorMap","")
  if(typeof(colormap).ne."string")
    dfile->ColorMap = colormap         ; Write to file so we can read later.
  end if

;
; Get plot resources, if any.
;
  pattnames = getvaratts(plot_res)
  if(.not.any(ismissing(pattnames))) then
    natt = dimsizes(pattnames)
  else
    natt = 0
  end if
;
; Get plot attributes, if any, and check if any of them are ones
; that can contain big data arrays, like sfXArray, vfYArray, etc.
; If so, then write these to the netCDF file. Otherwise, write them
; to the file as attributes.
;
  array_resources = (/"sfXArray","sfYArray","vfXArray","vfYArray"/)

  if(natt.gt.0) then
    do i=0,natt-1
      if(any(pattnames(i).eq.array_resources)) then
        dfile->$pattnames(i)$ = plot_res@$pattnames(i)$
      else
        if(typeof(plot_res@$pattnames(i)$).eq."logical") then
          if(plot_res@$pattnames(i)$) then
            dfile@$pattnames(i)$ = 1
          else
            dfile@$pattnames(i)$ = 0
          end if
        else
          dfile@$pattnames(i)$ = plot_res@$pattnames(i)$
        end if
      end if
    end do
  end if

;
; Create NCL script that plots data.
;
  q = inttochar(34)      ; 34 is the decimal equivalent of double quote (")

  lines  = new(30+natt,string)   ; Roughly how many lines we need
  nlines = 0

  write_lines("load " + q + "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl" + q, lines, nlines)
  write_lines("load " + q + "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" + q, lines, nlines)
  write_lines("", lines, nlines)
  write_lines("begin", lines, nlines)
  write_lines(";", lines, nlines)
  write_lines("; Read in data.", lines, nlines)
  write_lines(";", lines, nlines)
  write_lines("  cdf_file = addfile(" + q + cdf_debug_file + q + "," + q + "r" + q + ")", lines, nlines)

  if(ndfiles.ge.1) then
    write_lines("  data1 = cdf_file->PlotData", lines, nlines)
    write_latlon2d_lines(data1, "data1", lines, nlines)
  end if
  if(ndfiles.ge.2) then
    write_lines("  data2 = cdf_file->PlotData2", lines, nlines)
    write_latlon2d_lines(data2, "data2", lines, nlines)
  end if
  if(ndfiles.ge.3) then
    write_lines("  data3 = cdf_file->PlotData3", lines, nlines)
    write_latlon2d_lines(data3, "data3", lines, nlines)
  end if
  write_lines("", lines, nlines)
  write_lines(";", lines, nlines)
  write_lines("; Open PNG file.", lines, nlines)
  write_lines(";", lines, nlines)
  write_lines("  wks = gsn_open_wks(" + q + "png" + q + ", " + q + debug_file + q + ")", lines, nlines)
;
; Set colormap, if one inputted by user.
;
  if(typeof(colormap).ne."string") then
    write_lines("  gsn_define_colormap(wks, cdf_file->ColorMap)", \
                lines, nlines)
  else
    if(colormap.ne."") then
      write_lines("  gsn_define_colormap(wks," +  q + colormap + q + \
                  ")", lines, nlines)
    else
      write_lines(";  gsn_define_colormap(wks, " + colormap + ")", lines, nlines)
    end if
  end if
  write_lines("", lines, nlines)

;
; Loop through resources written to netCDF file and add them to NCL
; script. If one of the array resources are encountered, this means the
; value should be read off the file as a variable.
;
; If one of the ignore_resources is encountered, write it to
; the script, but comment it out.
;
  ignore_resources = (/"gsnDraw","gsnFrame"/)

  if(natt.gt.0) then
    write_lines("  res = True", lines, nlines)
    do i=0,natt-1
      if(any(pattnames(i).eq.ignore_resources)) then
        cc = ";"      ; Set comment char
      else
        cc = ""       ; Don't set comment char
      end if
      if(typeof(plot_res@$pattnames(i)$).eq."string") then
        qc = q      ; Set quote char
      else
        qc = ""     ; Don't set quote char
      end if
      if(any(pattnames(i).eq.array_resources)) then
        write_lines(cc + "  res@" + pattnames(i) + " = cdf_file->" + \
                    pattnames(i), lines, nlines)
      else
        narr = dimsizes(plot_res@$pattnames(i)$)
        if(narr.eq.1) then
          write_lines(cc + "  res@" + pattnames(i) + " = " + qc + \
                      plot_res@$pattnames(i)$ + qc, lines, nlines)
        else
;
; We have to write out an array of attribute values.
;
          build_str = cc + "  res@" + pattnames(i) + " = (/"
          do na=0,narr-2
            build_str = build_str + qc + plot_res@$pattnames(i)$(na) + qc + ", "
          end do
          build_str = build_str + qc + plot_res@$pattnames(i)$(narr-1) + qc + "/)"
          write_lines(build_str, lines, nlines)
        end if
      end if
      delete(qc)
    end do
  else
    write_lines("  res = False", lines, nlines)
  end if

  write_lines("", lines, nlines)
  if(ndfiles.eq.0) then
    if(gsn_name.eq."gsn_map") then
      proj = get_res_value_keep(plot_res,"mpProjection","CylindricalEquidistant")
      write_lines("  plot = " + gsn_name + "(wks," + q + proj + \
                  q + ", res)", lines, nlines)
    else
      write_lines("  plot = " + gsn_name + "(wks, res)", lines, nlines)
    end if
  else
    if(ndfiles.eq.1) then
      write_lines("  plot = " + gsn_name + "(wks, data1, res)", lines, nlines)
    else
      if(ndfiles.eq.2) then
        write_lines("  plot = " + gsn_name + "(wks, data1, data2, res)", lines, nlines)
      else
        write_lines("  plot = " + gsn_name + "(wks, data1, data2, data3, res)", lines, nlines)
      end if
    end if
  end if
  write_lines("end", lines, nlines)

;
; Write NCL script and resource file.
;
  asciiwrite(ncl_debug_file,lines(:nlines-1))
  write_debug_res_file(res_debug_file)

;
; Print some information.
;
  print("gsnp_write_debug_info: debug info written to:")
  print("  '" + ncl_debug_file + "'")
  print("  '" + cdf_debug_file + "'")
  print("  '" + res_debug_file + "'")
;
; Clean up. Put the lat2d/lon2d attributes back, if there were any.
;  
  if(ndfiles.ge.1) then
    fix_latlon2d_debug_info(data1)
  end if
  if(ndfiles.ge.2) then
    fix_latlon2d_debug_info(data2)
  end if
  if(ndfiles.ge.3) then
    fix_latlon2d_debug_info(data3)
  end if

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
undef("gsnp_turn_off_tickmarks")
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
undef("gsnp_point_tickmarks_outward")
procedure gsnp_point_tickmarks_outward(plot:graphic,res:logical, \
                              x_major_length, y_major_length, \
                              x_minor_length, y_minor_length, \
                              major_length, minor_length,point_outward)
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
  set_attr(tmres,"tmXBMinorLengthF"        , x_minor_length)
  set_attr(tmres,"tmXTMajorLengthF"        , x_major_length)
  set_attr(tmres,"tmXTMinorLengthF"        , x_minor_length)
  set_attr(tmres,"tmYLMajorLengthF"        , y_major_length)
  set_attr(tmres,"tmYLMinorLengthF"        , y_minor_length)
  set_attr(tmres,"tmYRMajorLengthF"        , y_major_length)
  set_attr(tmres,"tmYRMinorLengthF"        , y_minor_length)

  if (point_outward) then
    set_attr(tmres,"tmXBMajorOutwardLengthF" , x_major_length)
    set_attr(tmres,"tmXBMinorOutwardLengthF" , x_minor_length)
    set_attr(tmres,"tmXTMajorOutwardLengthF" , x_major_length)
    set_attr(tmres,"tmXTMinorOutwardLengthF" , x_minor_length)
    set_attr(tmres,"tmYLMajorOutwardLengthF" , y_major_length)
    set_attr(tmres,"tmYLMinorOutwardLengthF" , y_minor_length)
    set_attr(tmres,"tmYRMajorOutwardLengthF" , y_major_length)
    set_attr(tmres,"tmYRMinorOutwardLengthF" , y_minor_length)
  end if

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
undef("gsnp_uniform_tickmark_labels")
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
undef("gsnp_shape_plot")
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
;              prefix:string                                            ;
;               res:logical                                             ;
;                                                                       ;
; If gsnScale is set to True, then the plot is scaled such the tickmarks;
; and tickmark labels are the same size on both axes.                   ;
;                                                                       ;
; As of 5.0.1, this procedure checks the resource list to see if the    ;
; xxX/YArray resources are attached, and if they are 2D. If so, then we ;
; also need to make sure trGridType is TriangularMesh, otherwise we     ;
; can't have a tickmark object.                                         ;
;***********************************************************************;
undef("gsnp_scale_plot")
procedure gsnp_scale_plot(plot:graphic,scalar_prefix,res:logical)
local xfont, yfont, xbfont, xlength, xmlength, ylfont, ylength, ymlength, \
xresname, yresname, valid_tm, tr_grid_type
begin
;
; We can get/set the title sizes no matter what.
;
  getvalues plot
    "tiXAxisFontHeightF"   : xfont
    "tiYAxisFontHeightF"   : yfont
  end getvalues
  setvalues plot
    "tiXAxisFontHeightF"   : (xfont+yfont)/2.
    "tiYAxisFontHeightF"   : (xfont+yfont)/2.
  end setvalues
;
; Here's the code for checking if we have a valid tickmark object.
;
  valid_tm = True       ; Assume we have a valid one, unless proven otherwise.
  if(res.and.scalar_prefix.ne."") then
    xresname = scalar_prefix + "XArray"
    yresname = scalar_prefix + "YArray"
    if(((isatt(res,xresname).and.dimsizes(dimsizes(res@$xresname$)).gt.1).or.\
        (isatt(res,yresname).and.dimsizes(dimsizes(res@$yresname$)).gt.1))) then
      getvalues plot
        "trGridType" : tr_grid_type
      end getvalues
      if(tr_grid_type.ne.5) then
        valid_tm = False
      end if
    end if
  end if
  if(valid_tm) then
    getvalues plot
      "tmXBLabelFontHeightF" : xbfont
      "tmXBMajorLengthF"     : xlength
      "tmXBMinorLengthF"     : xmlength
      "tmYLLabelFontHeightF" : ylfont
      "tmYLMajorLengthF"     : ylength
      "tmYLMinorLengthF"     : ymlength
    end getvalues

    if(xlength.ne.0..and.ylength.ne.0.) then
      major_length = (ylength+xlength)/2. 
      xlength = major_length
      ylength = major_length
    end if

    if(xmlength.ne.0..and.ymlength.ne.0.) then
      minor_length = (ymlength+xmlength)/2. 
      xmlength = minor_length
      ymlength = minor_length
    end if

    setvalues plot
      "tmXBLabelFontHeightF" : (xbfont+ylfont)/2.
      "tmXBMajorLengthF"     : xlength
      "tmXBMinorLengthF"     : xmlength
      "tmYLLabelFontHeightF" : (xbfont+ylfont)/2.
      "tmYLMajorLengthF"     : ylength
      "tmYLMinorLengthF"     : ymlength
    end setvalues
  end if
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
undef("check_for_tickmarks_off")
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
; Function : compute_device_coords                                     ;
;                  plot : graphic                                      ;
;                  res  : logical                                      ;
;                                                                      ;
; This function computes the PDF/PS device coordinates needed to       ;
; make a plot fill up the full page.                                   ;
;                                                                      ;
; plot : the plot or the workstation                                   ;
; res  : list of optional resources. Ones accepted include:            ;
;                                                                      ;
; "gsnPaperOrientation" - orientation of paper. Can be "landscape",    ;
;                         "portrait", or "auto". Default is "auto".    ;
;                                                                      ;
;       "gsnPaperWidth"  - width of paper (in inches, default is 8.5)  ;
;       "gsnPaperHeight" - height of paper (in inches, default is 11.0);
;       "gsnPaperMargin" - margin to leave around plots (in inches,    ;
;                        default is 0.5)                               ;
;                                                                      ;
; In V5.2.0, new paper size resources were added, and this function    ;
; was updated to recognize them (wkPaperSize, wkPaperWidthF,           ;
;    and wkPaperHeightF).                                              ;
;                                                                      ;
; gsnPaperWidth and gsnPaperHeight are considered deprecated, but      ;
; still recognized.                                                    ;
;                                                                      ;
;                                                                      ;
;**********************************************************************;
undef("compute_device_coords")
function compute_device_coords(plot,res)
local coords, top, bot, lft, rgt, dpi, dpi_pw, dpi_ph, dpi_margin, \
      paper_width, paper_height, paper_margin, bb, tmp_wks, wks_ph, wks_pw, def_margin
begin
  def_margin =  0.5    ; Default margin, in inches.
  def_orient =  "auto" ; Best possible orientation will be used, by default.

; Get workstation parent of plot, and retrieve paper size and width.
  ii = ind(.not.ismissing(plot))
  if(all(ismissing(ii))) then
    print("compute_device_coords: Error: all input plots are missing")
    status_exit(1)
  end if

  tmp_wks = NhlGetParentWorkstation(plot(ii(0)))
  getvalues tmp_wks
    "wkPaperWidthF"  : wks_pw
    "wkPaperHeightF" : wks_ph
  end getvalues

;
; Get resources.
;
  paper_height = get_res_value_keep(res,"gsnPaperHeight",wks_ph)
  paper_width  = get_res_value_keep(res,"gsnPaperWidth",wks_pw)
  paper_margin = get_res_value_keep(res,"gsnPaperMargin",def_margin)
  paper_orient = get_res_value_keep(res,"gsnPaperOrientation",def_orient)
  is_debug     = get_res_value_keep(res,"gsnDebug",False)

  if(is_debug)
    print("    Paper width/height = " + paper_width + "/" + paper_height)
  end if

;
; Check to see if any panel resources have been set.  No defaults
; will be assumed for these. They are only used if they have been
; explicitly set by the user.
;
  lft_pnl = isatt(res,"gsnPanelLeft")
  rgt_pnl = isatt(res,"gsnPanelRight")
  bot_pnl = isatt(res,"gsnPanelBottom")
  top_pnl = isatt(res,"gsnPanelTop")

  lft_inv_pnl = isatt(res,"gsnPanelInvsblLeft")
  rgt_inv_pnl = isatt(res,"gsnPanelInvsblRight")
  bot_inv_pnl = isatt(res,"gsnPanelInvsblBottom")
  top_inv_pnl = isatt(res,"gsnPanelInvsblTop")

  if(typeof(paper_orient).eq."integer")
    if(paper_orient.eq.0)
      lc_orient = "portrait"
    else
      lc_orient = "landscape"
    end if
  else
    lc_orient = str_lower(paper_orient)
  end if
;
; Get the bounding box that covers all the plots.  If gsnPanel 
; resources have been added to add white space around plots, then
; count this white space in as well.  Note that even though the bounding
; box coordinates should be positive, it *is* possible for them to be
; negative, and we need to keep these negative values in our calculations
; later to preserve the aspect ratio.
;
  bb    = NhlGetBB(plot)
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

  if(top_inv_pnl)
    top = max((/res@gsnPanelInvsblTop,max(newbb(:,0))/))
  else
    if(top_pnl)
      top = max((/1.,max(newbb(:,0))/))
    else 
      top = max(newbb(:,0))
    end if
  end if
  if(bot_inv_pnl)
    bot = min((/res@gsnPanelInvsblBottom,min(newbb(:,1))/))
  else
    if(bot_pnl)
      bot = min((/0.,min(newbb(:,1))/))
    else
      bot = min(newbb(:,1))
    end if
  end if
  if(lft_inv_pnl)
    lft = min((/res@gsnPanelInvsblLeft,min(newbb(:,2))/))
  else
    if(lft_pnl)
      lft = min((/0.,min(newbb(:,2))/))
    else
      lft = min(newbb(:,2))
    end if
  end if
  if(rgt_inv_pnl)
    rgt = max((/res@gsnPanelInvsblRight,max(newbb(:,3))/))
  else
    if(rgt_pnl)
      rgt = max((/1.,max(newbb(:,3))/))
    else
      rgt = max(newbb(:,3))
    end if
  end if

;  if(bot.lt.0.or.bot.gt.1.or.top.lt.0.or.top.gt.1.or. \
;     lft.lt.0.or.lft.gt.1.or.rgt.lt.0.or.rgt.gt.1)
;    print("compute_device_coords: warning: bounding box values should be between 0 and 1 inclusive. Will continue anyway.")
;  end if

  if(bot.ge.top.or.lft.ge.rgt)
    print("compute_device_coords: bottom must be < top and left < right")
    return((/0,0,0,0/))
  end if
;
; Debug prints
;
  if(is_debug)
    print("-------Bounding box values for PS/PDF-------")
    print("    top = " + top + " bot = " + bot + \
          "    lft = " + lft + " rgt = " + rgt)
  end if
;
; Initialization
;
  dpi        = 72.                       ; Dots per inch.
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
; Procedure : reset_device_coordinates                                  ;
;                wks:graphic                                            ;
;                                                                       ;
; This procedure resets the PS/PDF device coordinates back to their     ;
; default values. The default values will be whatever ones the user     ;
; might have set when they called gsn_open_wks, or the defaults that    ;
; NCL uses if none are set.                                             ;
;***********************************************************************;

undef("reset_device_coordinates")
procedure reset_device_coordinates(wks)
begin
  setvalues wks
    "wkOrientation"  : get_res_value(wks,"wkOrientation",   0)
    "wkDeviceLowerX" : get_res_value(wks,"wkDeviceLowerX", 36)
    "wkDeviceLowerY" : get_res_value(wks,"wkDeviceLowerY",126)
    "wkDeviceUpperX" : get_res_value(wks,"wkDeviceUpperX",576)
    "wkDeviceUpperY" : get_res_value(wks,"wkDeviceUpperY",666)
  end setvalues
end


;***********************************************************************;
; Procedure : maximize_plot                                             ;
;               plot:graphic                                            ;
;              psres:logical                                            ;
;                                                                       ;
; This procedure takes a plot that probably has had some additional     ;
; objects attached to it, like a labelbar, and maximizes it in the      ;
; given PS or PDF workstation.                                          ;
;***********************************************************************;
undef("maximize_plot")
procedure maximize_plot(plot,psres)
local bb, coords, res2, tmp_wks
begin
  res2 = psres

  tmp_wks = NhlGetParentWorkstation(plot)
;
; Calculate device coords to maximize this plot on the PDF or PS 
; workstation.
;
  coords = compute_device_coords(plot,res2)
;
; Using the coordinate values just calculated, set them in the
; workstation (also set the orientation).
;
  setvalues tmp_wks
    "wkDeviceLowerX" : coords(0)
    "wkDeviceLowerY" : coords(1)
    "wkDeviceUpperX" : coords(2)
    "wkDeviceUpperY" : coords(3)
    "wkOrientation"  : coords@gsnPaperOrientation
  end setvalues
end

;***********************************************************************;
; Procedure : maximize_output                                           ;
;                wks:graphic                                            ;
;              psres:logical                                            ;
;                                                                       ;
; This procedure takes a workstation (that supposedly has several plots ;
; drawn on it), calculates the device coordinates needed to maximize    ;
; the plots on a PS or PDF workstation, and then sets these device      ;
; coordinates back to the workstation. draw and frame happen by default ;
; unless otherwise specified.                                           ;
;***********************************************************************;
undef("maximize_output")
procedure maximize_output(wks:graphic,psres:logical)
local calldraw, callframe, class, res2
begin
  res2 = psres
;
; Get draw and frame values, if set.
;
  calldraw  = get_res_value(res2,"gsnDraw", True)
  callframe = get_res_value(res2,"gsnFrame",True)
;
; Only do this type of maximization for PS or PDF workstations.
; The device coordinates mean nothing for NCGM and X11 workstations.
;
  max_device = False
  class = NhlClassName(wks)
  if(any(class.eq.(/"psWorkstationClass","pdfWorkstationClass",\
                    "documentWorkstationClass"/)))
    max_device = True
;
; Calculate device coords to maximize these plots on the PDF or PS 
; workstation.
;
    coords = compute_device_coords(wks,res2)

;
; Using the coordinate values just calculated, set them in the
; workstation (also set the orientation).
;
    setvalues wks
      "wkDeviceLowerX" : coords(0)
      "wkDeviceLowerY" : coords(1)
      "wkDeviceUpperX" : coords(2)
      "wkDeviceUpperY" : coords(3)
      "wkOrientation"  : coords@gsnPaperOrientation
    end setvalues

  end if

  if(calldraw) then
    draw(wks)            ; This will draw everything on the workstation.
  end if
  if(callframe) then
    frame(wks)
;
; Only set the device coordinates back if the frame is advanced, because 
; if we do it when the frame hasn't been advanced, then anything that
; gets drawn on this plot later will be drawn under the old device
; coordinates.
;
; This means that the user will have to be aware that if he/she decides to
; advance the frame him/herself, then any subsequent plots draw (in which
; the device coordinates are not recalculated), may be drawn incorrectly.
;
    if(max_device) then
      reset_device_coordinates(wks)
    end if
  end if
end

;***********************************************************************;
; Procedure : draw_and_frame                                            ;
;                wks:graphic                                            ;
;               plot:graphic                                            ;
;           calldraw:logical                                            ;
;          callframe:logical                                            ;
;            ispanel: logical                                           ;
;              maxbb:logical                                            ;
;                                                                       ;
; By default, all of the plotting routines will draw the plot and       ;
; advance the frame, unless the special resources gsnDraw and/or        ;
; gsnFrame are set to False. This procedure checks if these resources   ;
; had been set, and calls draw and/or frame accordingly.                ;
; If maxbb is True, then the plot is maximized in the NCGM, X11, PS     ;
; or PDF window.                                                        ; 
;***********************************************************************;
undef("draw_and_frame")
procedure draw_and_frame(wks:graphic,plot:graphic,calldraw:logical, \
                         callframe:logical,ispanel:logical,maxbb:logical)
local nplots, class, coords 
begin
  max_device = False
  if(maxbb) then
;
; If dealing with panel plots, then this means that the viewport 
; coordinates have already been calculated to maximize the plots in
; the unit square, so we don't need to do it again here. However, we
; will still need to calculate the optimal device  coordinates if the
; output is PDF or PS.
;
    if(.not.ispanel) then
;
; First, optimize the plot size in the viewport (unit square). This 
; may involve making it bigger or smaller.
;
      coords = maximize_bb(plot,maxbb)
      setvalues plot
        "vpXF"      : coords(0)
        "vpYF"      : coords(1)
        "vpWidthF"  : coords(2)
        "vpHeightF" : coords(3)
      end setvalues
    end if

    class = NhlClassName(wks)
    if(any(class(0).eq.(/"psWorkstationClass","pdfWorkstationClass",\
                         "documentWorkstationClass"/))) then
;
; Keep track of whether device coordinates were recalculated.
;
      max_device = True
;
; Compute device coordinates that will make plot fill the whole page.
;
      coords = compute_device_coords(plot,maxbb)
;
; Set device coordinates to new ones.
;
      setvalues wks
        "wkOrientation"  : coords@gsnPaperOrientation
        "wkDeviceLowerX" : coords(0)
        "wkDeviceLowerY" : coords(1)
        "wkDeviceUpperX" : coords(2)
        "wkDeviceUpperY" : coords(3)
      end setvalues
    end if
  end if

  if(calldraw)
    draw(plot)
  end if

  if(callframe)
    frame(wks)           ; advance the frame
;
; Only set the device coordinates back if the frame is advanced, because 
; if we do it when the frame hasn't been advanced, then anything that
; gets drawn on this plot later will be drawn under the old device
; coordinates.
;
; This means that the user will have to be aware that if he/she decides to
; advance the frame him/herself, then any subsequent plots draw (in which
; the device coordinates are not recalculated), may be drawn incorrectly.
;
    if(max_device) then
      reset_device_coordinates(wks)
    end if
  end if

end

;***********************************************************************;
; Function : get_bb_res                                                 ;
;               res : list of resources                                 ;
;                                                                       ;
; Get list of resources for use with maximizing the plots within an     ;
; X11, NCGM, PS or PDF window.                                          ;
;***********************************************************************;
undef("get_bb_res")
function get_bb_res(res:logical)
begin
  maxbb                = get_res_value(res,"gsnMaximize", False) 
  maxbb@gsnPaperMargin = get_res_value(res,"gsnPaperMargin",0.5)
;
; No longer set these two, because they will be retrieved later.
;
;  maxbb@gsnPaperHeight = get_res_value(res,"gsnPaperHeight",11.0)
;  maxbb@gsnPaperWidth  = get_res_value(res,"gsnPaperWidth",8.5)
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
; Function : gsn_blank_plot                                                 ;
;               wks : workstation                                       ;
;               res : optional resources                                ;
;                                                                       ;
; This function creates a blank tickmark object that can be used for    ;
; drawing primitives.                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_blank_plot")
function gsn_blank_plot(wks:graphic,res:logical)
local res2
begin
  res2      = res
  point_outward = get_res_value(res2,"gsnTickMarksPointOutward",False)
  calldraw  = get_res_value(res2,"gsnDraw", True)
  callframe = get_res_value(res2,"gsnFrame",False)
  shape     = get_res_value(res2,"gsnShape",False)
  scale     = get_res_value(res2,"gsnScale",shape)
  ticks     = get_res_value(res2,"pmTickMarkDisplayMode","Always")
  title     = get_res_value(res2,"pmTitleDisplayMode","Always")
  maxbb     = get_bb_res(res2)

  canvas = create "canvas" irregularPlotClass wks
    "pmTickMarkDisplayMode" : ticks
    "pmTitleDisplayMode"    : title
  end create

  attsetvalues_check(canvas,res2)

  tmres = get_res_eq(res2,"tm")
  gsnp_point_tickmarks_outward(canvas,tmres,-1.,-1.,-1.,-1.,-1.,-1.,\
                               point_outward)

; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.

  if(shape)
    gsnp_shape_plot(canvas)
  end if

; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.

  if(scale)
    gsnp_scale_plot(canvas,"",False)
  end if

  draw_and_frame(wks,canvas,calldraw,callframe,False,maxbb)

  return(canvas)
end

;***********************************************************************;
; Function : create_canvas                                              ;
;               wks : workstation                                       ;
;                                                                       ;
; This function creates a blank LogLin plot that can be used for drawing;
; primitives.                                                           ;
;                                                                       ;
;***********************************************************************;
undef("create_canvas")
function create_canvas(wks:graphic)
begin
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

  return(canvas)
end

;***********************************************************************;
; Function : gsn_open_ncgm                                              ;
;               name : name of output cgm file                          ;
;                                                                       ;
; This function opens an NCGM output file called "<name>.ncgm" and      ;
; returns the workstation id. If "name" is an empty string, then the    ;
; NCGM is given its default name "gmeta".                               ;
;***********************************************************************;
undef("gsn_open_ncgm")
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
undef("gsn_open_x11")
function gsn_open_x11(name[1]:string,res[1]:string)
local window
begin
    ; apply width/height resources if present; must be applied at creation time.
    imageWidth = get_res_value(res,"wkWidth",-1)
    imageHeight = get_res_value(res,"wkHeight",-1)
    if (imageWidth.gt.0 .and. imageHeight.gt.0) then
        if(isatt(res,"wkColorMap"))
            window = create name + "_x11" windowWorkstationClass defaultapp
                "wkFormat"   : "x11"
                "wkPause"    : True
                "wkColorMap" : res@wkColorMap
                "wkWidth"    : imageWidth
                "wkHeight"   : imageHeight
            end create
        else
            window = create name + "_x11" windowWorkstationClass defaultapp
                "wkFormat" : "x11"
                "wkPause"  : True
                "wkWidth"  : imageWidth
                "wkHeight" : imageHeight
            end create
        end if
    else
        if(isatt(res,"wkColorMap"))
            window = create name + "_x11" windowWorkstationClass defaultapp
                "wkFormat": "x11"
                "wkPause" : True
                "wkColorMap" : res@wkColorMap
            end create
        else
            window = create name + "_x11" windowWorkstationClass defaultapp
                "wkFormat": "x11"
                "wkPause" : True
            end create
        end if
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
;                                                                       ;
; "EndStyle" is a special internal resource that comes from the         ;
; cnLabelBarEndStyle resource. Since there is no specific EndStyle      ;
; resource for labelbar objects, we may have to subset the colors and   ;
; the labels to account for this. This is what "SubsetStuff" is for.    ;
;***********************************************************************;
undef("create_labelbar")
function create_labelbar(wks:graphic, nbox:integer, colors, labels, \
lbres:logical)
local perim_on, mono_fill_pat, label_align, labelbar_object
begin
;
; Set some defaults
;
  vpxf          = get_res_value(lbres,"vpXF",0.1)
  vpyf          = get_res_value(lbres,"vpYF",0.1)
  vpwidthf      = get_res_value(lbres,"vpWidthF",0.8)
  vpheightf     = get_res_value(lbres,"vpHeightF",0.3)
  new_labels    = get_res_value(lbres,"lbLabelStrings",labels)
  orientation   = get_res_value(lbres,"lbOrientation","horizontal")
  perim_on      = get_res_value(lbres,"lbPerimOn",False)
  font_height   = get_res_value(lbres,"lbLabelFontHeightF",0.1)
  mono_fill_pat = get_res_value(lbres,"lbMonoFillPattern",True);
  subset_stuff  = get_res_value(lbres,"SubsetStuff",False)

  ncolors = dimsizes(colors)
  if(isatt(lbres,"EndStyle")) then
    end_style = get_res_value(lbres,"EndStyle",get_enum_value("IncludeOuterBoxes"))
    if(.not.any(end_style.eq.(/get_enum_value("IncludeOuterBoxes"),\
                               get_enum_value("IncludeMinMaxLabels"),\
                               get_enum_value("ExcludeOuterBoxes")/))) then
      end_style = get_enum_value("IncludeOuterBoxes")
    end if
    if(end_style.eq.get_enum_value("ExcludeOuterBoxes")) then
      label_align = get_res_value(lbres,"lbLabelAlignment","ExternalEdges")
      if(subset_stuff) then
        nbox2       = nbox-2
        new_colors  = get_res_value(lbres,"lbFillColors",colors(1:ncolors-2))
;;        if(label_align.eq.get_enum_value("InteriorEdges"))
;;          tmp_labels = new_labels(1:)
;;          delete(new_labels)
;;          new_labels = tmp_labels
;;        end if
      else
        nbox2       = nbox
        new_colors  = get_res_value(lbres,"lbFillColors",colors)
      end if
    else
      nbox2       = nbox
      new_colors  = get_res_value(lbres,"lbFillColors",colors)
      if(end_style.eq.get_enum_value("IncludeOuterBoxes")) then
        label_align = get_res_value(lbres,"lbLabelAlignment","InteriorEdges")
      else ; IncludeMinMaxLabels
        label_align = get_res_value(lbres,"lbLabelAlignment","ExternalEdges")
      end if
    end if
  else
    nbox2       = nbox
    new_colors  = get_res_value(lbres,"lbFillColors",colors)
    label_align = get_res_value(lbres,"lbLabelAlignment","InteriorEdges")
  end if

  labelbar_object = create "labelbar" labelBarClass wks
    "vpXF"              : vpxf
    "vpYF"              : vpyf
    "vpWidthF"          : vpwidthf
    "vpHeightF"         : vpheightf
    "lbBoxCount"        : nbox2
    "lbFillColors"      : new_colors
    "lbLabelStrings"    : new_labels
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
; Function to determine if a cairo PS is being requested.               ;
;***********************************************************************;
undef("is_cairo_ps")
function is_cairo_ps(wtype)
local lwtype, cpsenv
begin
  lwtype = str_lower(wtype)
  cpsenv = getenv("NCARG_OLD_PS")
  if(lwtype.eq."newps".or.(lwtype.eq."ps".and.ismissing(cpsenv))) then
    return(True)
  else
    return(False)
  end if
end

;***********************************************************************;
; Function to determine if a cairo PDF is being requested.              ;
;***********************************************************************;
undef("is_cairo_pdf")
function is_cairo_pdf(wtype)
local lwtype, cpdfenv
begin
  lwtype  = str_lower(wtype)
  cpdfenv = getenv("NCARG_OLD_PDF")
  if(lwtype.eq."newpdf".or.(lwtype.eq."pdf".and.ismissing(cpdfenv))) then
    return(True)
  else
    return(False)
  end if
end

;***********************************************************************;
; Function to determine if a cairo PNG is being requested.              ;
;***********************************************************************;
undef("is_cairo_png")
function is_cairo_png(wtype)
local lwtype
begin
  lwtype = str_lower(wtype)
  if(lwtype.eq."newpng".or.lwtype.eq."png") then
    return(True)
  else
    return(False)
  end if
end

;***********************************************************************;
; Function to determine if a cairo EPS is being requested.              ;
;***********************************************************************;
undef("is_cairo_eps")
function is_cairo_eps(wtype)
local lwtype, cpsenv
begin
  lwtype = str_lower(wtype)
  cpsenv = getenv("NCARG_OLD_EPS")
  if (lwtype.eq."eps".and.ismissing(cpsenv)) then
    return(True)
  else
    return(False)
  end if
end

;***********************************************************************;
; Function to determine if a cairo SVG is being requested.              ;
;***********************************************************************;
undef("is_cairo_svg")
function is_cairo_svg(wtype)
local lwtype
begin
  lwtype = str_lower(wtype)
  if (lwtype.eq."svg") then
    return(True)
  else
    return(False)
  end if
end

;***********************************************************************;
; Function to determine if a cairo workstation is being requested.      ;
;***********************************************************************;
undef("is_cairo_wks")
function is_cairo_wks(wtype)
begin
  if (is_cairo_ps(wtype).or.is_cairo_pdf(wtype).or.is_cairo_png(wtype).or. \
       is_cairo_eps(wtype).or.is_cairo_svg(wtype)) then
    return(True)
  else
    return(False)
  end if
end

;***********************************************************************;
; Function : gsn_open_ps                                                ;
;               name : name of PostScript file                          ;
;                                                                       ;
; This function opens a PostScript file called "<name>.ps" and returns  ;
; the workstation id. If "name" is an empty string, then the PostScript ;
; file is called "gmeta.ps".                                            ;
;***********************************************************************;
undef("gsn_open_ps")
function gsn_open_ps(type:string,name[1]:string)
local ps, res_file, wkres, wkresnew, names, i
begin
;
; If there are resources, copy them over to a logical variable,
; and only grab the ones that start with "wk".
;
    if(.not.any(ismissing(getvaratts(type))))
      wkres = True
      names = getvaratts(type)
      do i=0,dimsizes(names)-1
        if(names(i).ne."_FillValue") then
          wkres@$names(i)$ = type@$names(i)$
        end if
      end do
;
; Grab only the ones that start with "wk".
;
      wkresnew = get_res_eq(wkres,"wk")
      delete(wkres)
      delete(names)
    else
      wkresnew = False
    end if

    res_file = get_res_value_keep(name,"res_file","gsnapp")
;
; These PS resources are ones that must be set at the time
; the workstation is created. This means that these resources
; will override whatever setting you might have in a resource
; file or your .hluresfile.
;
    cmodel   = get_res_value(wkresnew,"wkColorModel","rgb")
    resltn   = get_res_value(wkresnew,"wkPSResolution",1800)
    visualt  = get_res_value(wkresnew,"wkVisualType","color")
    paperSize = get_res_value(wkresnew,"wkPaperSize","")
    paperW = get_res_value(wkresnew,"wkPaperWidthF", -1.)
    paperH = get_res_value(wkresnew,"wkPaperHeightF", -1.)

; Be sure to add any resources set here to "res_list" in gsn_open_wks
; so they don't get set again.

    ps = create res_file psWorkstationClass defaultapp
      "wkColorModel"   : cmodel
      "wkPSResolution" : resltn
      "wkPSFileName"   : name
      "wkPSFormat"     : type
      "wkVisualType"   : visualt
      "wkPaperSize"     : paperSize
      "wkPaperWidthF"   : paperW
      "wkPaperHeightF"  : paperH
    end create

;
; Set resources, if any.
;
    attsetvalues_check(ps,wkresnew)
    delete(wkresnew)

;
; Retrieve the device coordinates and the orientation so we can
; reset them later if necessary.
;
    getvalues ps
      "wkOrientation"  : ps@wkOrientation
      "wkDeviceLowerX" : ps@wkDeviceLowerX
      "wkDeviceLowerY" : ps@wkDeviceLowerY
      "wkDeviceUpperX" : ps@wkDeviceUpperX
      "wkDeviceUpperY" : ps@wkDeviceUpperY
      "wkPaperWidthF"  : ps@wkPaperWidthF
      "wkPaperHeightF" : ps@wkPaperHeightF
    end getvalues

    return(ps)
end

;***********************************************************************;
; Function : gsn_open_pdf                                               ;
;               name : name of PDF file                                 ;
;                                                                       ;
; This function opens a PDF file called "<name>.pdf" and returns        ;
; the workstation id. If "name" is an empty string, then the PDF file   ;
; is called "gmeta.pdf".                                                ;
;***********************************************************************;
undef("gsn_open_pdf")
function gsn_open_pdf(type:string, name[1]:string)
local pdf, res_file, wkres, wkresnew, names, i
begin
;
; If there are resources, copy them over to a logical variable,
; and only grab the ones that start with "wk".
;
    if(.not.any(ismissing(getvaratts(type))))
      wkres = True
      names = getvaratts(type)
      do i=0,dimsizes(names)-1
        if(names(i).ne."_FillValue") then
          wkres@$names(i)$ = type@$names(i)$    
        end if
      end do
;
; Grab only the ones that start with "wk".
;
      wkresnew = get_res_eq(wkres,"wk")
      delete(wkres)
      delete(names)
    else
      wkresnew = False
    end if

    res_file = get_res_value_keep(name,"res_file","gsnapp")
;
; These PDF resources are ones that must be set at the time
; the workstation is created. This means that these resources
; will override whatever setting you might have in a resource
; file or your .hluresfile.
;
    cmodel   = get_res_value(wkresnew,"wkColorModel","rgb")
    resltn   = get_res_value(wkresnew,"wkPDFResolution",1800)
    visualt  = get_res_value(wkresnew,"wkVisualType","color")
    paperSize = get_res_value(wkresnew,"wkPaperSize","")
    paperW = get_res_value(wkresnew,"wkPaperWidthF", -1.)
    paperH = get_res_value(wkresnew,"wkPaperHeightF", -1.)

; Be sure to add any resources set here to "res_list" in gsn_open_wks
; so they don't get set again.

    pdf = create res_file pdfWorkstationClass defaultapp
      "wkColorModel"    : cmodel
      "wkPDFResolution" : resltn
      "wkPDFFileName"   : name
      "wkPDFFormat"     : type
      "wkVisualType"    : visualt
      "wkPaperSize"     : paperSize
      "wkPaperWidthF"   : paperW
      "wkPaperHeightF"  : paperH
    end create

;
; Set resources, if any.
;
    attsetvalues_check(pdf,wkresnew)
    delete(wkresnew)

;
; Retrieve the device coordinates and the orientation so we can
; reset them later if necessary.
;
    getvalues pdf
      "wkOrientation"  : pdf@wkOrientation
      "wkDeviceLowerX" : pdf@wkDeviceLowerX
      "wkDeviceLowerY" : pdf@wkDeviceLowerY
      "wkDeviceUpperX" : pdf@wkDeviceUpperX
      "wkDeviceUpperY" : pdf@wkDeviceUpperY
      "wkPaperWidthF"  : pdf@wkPaperWidthF
      "wkPaperHeightF" : pdf@wkPaperHeightF
    end getvalues

    return(pdf)
end

;***********************************************************************;
; Function : gsn_open_cairo                                             ;
;               name : name of cairo file/device                        ;
;                                                                       ;
; This function opens a cairo file or device and returns                ;
; the workstation id.                                                   ; 
;***********************************************************************;
undef("gsn_open_cairo")
function gsn_open_cairo(type:string, name[1]:string)
local cairo, res_file, wkres, tmpres, names, i
begin
;
; If there are resources, copy them over to a logical variable,
; and only grab the ones that start with "wk".
;
    if(.not.any(ismissing(getvaratts(type))))
      tmpres = True
      names = getvaratts(type)
      do i=0,dimsizes(names)-1
        if(names(i).ne."_FillValue") then
          tmpres@$names(i)$ = type@$names(i)$    
        end if
      end do
;
; Grab only the ones that start with "wk".
;
      wkres = get_res_eq(tmpres,"wk")
      delete(tmpres)
      delete(names)
    end if

    wkres = True  ; force True, even if no resources present; get_res_eq() can return False.

    res_file = get_res_value_keep(name,"res_file","gsnapp")

;
; Add these resources if not already present.
;
    if (.not.isatt(wkres, "wkFileName")) then
        wkres@wkFileName = name
    end if
    if (.not.isatt(wkres, "wkFormat")) then
        wkres@wkFormat = type
        if (is_cairo_ps(type)) then
	    wkres@wkFormat = "ps"     ; user can specify "ps" or "newps"
	end if
	if (is_cairo_pdf(type)) then  
	    wkres@wkFormat = "pdf"    ; user can specify "pdf" or "newpdf"
	end if
    end if

;
; We need to distinguish between document-based versus image-based output formats
;
    if (is_cairo_ps(type).or.is_cairo_pdf(type).or.is_cairo_eps(type).or.is_cairo_svg(type)) then
        cairo = create_graphic(res_file, "documentWorkstationClass", "defaultapp", wkres)
    else
	cairo = create_graphic(res_file, "imageWorkstationClass", "defaultapp", wkres)
    end if

    delete(wkres)
    return(cairo)
end


;***********************************************************************;
; Function : gsn_open_image                                             ;
;               type : type of image file                               ;
;               name : name of output image file                        ;
;                                                                       ;
; This function opens an image file called "<name>.<type>" and          ;
; returns the workstation id. If "name" is an empty string, then the    ;
; NCGM is given a default name "gmeta.type".                            ;
;***********************************************************************;
undef("gsn_open_image")
function gsn_open_image(type:string,name[1]:string)
local image, res_file
begin
    res_file = get_res_value_keep(name,"res_file","gsnapp")
    wkwidth  = get_res_value_keep(type,"wkWidth",512)
    wkheight = get_res_value_keep(type,"wkHeight",512)

    if(isatt(name,"wkColorMap"))
      image = create res_file xwdimageWorkstationClass defaultapp 
        "wkImageFileName" : name
        "wkImageFormat"   : type
        "wkColorMap"      : name@wkColorMap
        "wkWidth"         : wkwidth
        "wkHeight"        : wkheight
      end create
    else
      image = create res_file xwdimageWorkstationClass defaultapp 
        "wkImageFileName" : name
        "wkImageFormat"   : type
        "wkWidth"         : wkwidth
        "wkHeight"        : wkheight
      end create
    end if

    return(image)
end


;***********************************************************************;
; Function : gsn_open_wks                                               ;
;               type : type of workstation to open                      ;
;               name : name of workstation                              ;
;                                                                       ;
; This function opens either an X11 window, an NCGM file, a Postscript  ;
; or a PDF file depending on "type", which can be "x11", "ncgm", "ps"   ;
; or "pdf". If "type" is a PS or PDF file or an NCGM, then it will be   ;
; named <name>.ps or <name>.pdf <name>.ncgm respectively. This function ;
; also looks for a resource file called "name.res". If it exists, then  ;
; it loads the resources defined in that file. This function returns    ;
; the workstation id.                                                   ;
;***********************************************************************;
undef("gsn_open_wks")
function gsn_open_wks(type[1]:string,name[1]:string)
local i, wks, appusrdir, name_char, not_found, res_file, res_dir, type2, realtype, suffix
begin
    type2   = type          ; Make copy of type and its resources
    type2  = str_lower(type)
    res_dir = "./"          ; Default resource directory.
    res_file = "gsnapp"     ; Default resource file name.
    valid_type = False      ; Is type valid?

;
; Parse "name" to get the directory and the file prefix.
;
    if(name.ne."") then
      name_char = stringtocharacter(name)
      name_len  = dimsizes(name_char)-1
      i = name_len-1      ; Start checking if a directory pathname
      not_found = True    ; was specified for the resource file.
      do while(not_found.and.i.ge.0)
        if(name_char(i).eq."/")
          res_dir  = charactertostring(name_char(0:i))
          not_found = False
        end if
        i = i - 1
      end do

      res_file = charactertostring(name_char(i+1:name_len-1))

      if(isatt(name,"appUsrDir").and.not_found)
        res_dir = name@appUsrDir   ; No directory specified.
      end if
    end if

    ; This 5-line block addresses Jira 1750; we test to make sure the target directory
    ; exists and is writable.
    cmdStr = "if [ -w " + res_dir + " ] ; then echo 1 ; else echo 0 ; fi"
    if (type.ne."x11" .and. systemfunc(cmdStr).ne.1) then
        print("gsn_open_wks: Error: directory '" + res_dir + "' does not exist or lacks write permissions.");
        status_exit(1)
    end if

    appid = create res_file appClass defaultapp
        "appDefaultParent" : True
        "appUsrDir"        : res_dir
    end create

;
; If we had a case statement or an "elseif" in NCL, this next
; section would look better!
;
    wks = new(1,graphic,"No_FillValue")

    if (type2.eq."x11") then
      wks = gsn_open_x11(res_file, type2)
      valid_type = True
    end if

    if (type2.eq."oldps".or.type2.eq."oldeps".or. \
        type2.eq."epsi".or.(type2.eq."ps".and..not.ismissing(getenv("NCARG_OLD_PS"))).or. \
        (type2.eq."eps".and..not.ismissing(getenv("NCARG_OLD_EPS")))) then
      realtype = type2   ; copy value and attributes
      suffix = type2
      if (realtype.eq."oldps") then
        realtype = "ps"
        suffix = "ps"
      end if
      if (realtype.eq."oldeps") then
        realtype = "eps"
        suffix = "eps"
      end if
      ps_file = get_res_value(realtype,"wkPSFileName",res_file + "." + suffix)
      ps_file@res_file = res_file
      wks = gsn_open_ps(realtype,res_dir+ps_file)
      valid_type = True
    end if

    if (type2.eq."ncgm") then
      ncgm_file = get_res_value(type2,"wkMetaName",res_file + ".ncgm")
      ncgm_file = res_dir + ncgm_file
      ncgm_file@res_file = res_file
      if (isatt(type2,"wkColorMap"))
        ncgm_file@wkColorMap = type2@wkColorMap
      end if
      wks = gsn_open_ncgm(ncgm_file)
      valid_type = True
    end if

    if (type2.eq."oldpdf".or.(type2.eq."pdf".and..not.ismissing(getenv("NCARG_OLD_PDF")))) then
      realtype = type2  ; copy value and attributes
      if (realtype.eq."oldpdf") then
          realtype = "pdf"
      end if
      pdf_file = get_res_value(realtype,"wkPDFFileName", \
                               res_file + "." + "pdf")
      pdf_file@res_file = res_file
      wks = gsn_open_pdf(realtype, res_dir+pdf_file)
      valid_type = True
    end if

    if (is_cairo_wks(type2)) then
      cairo_file = get_res_value(type2,"wkFileName", res_file)
      if (is_cairo_png(type2)) then
        cairo_file = get_res_value(type2,"wkImageFileName", cairo_file)
      end if
      cairo_file@res_file = res_file
      wks = gsn_open_cairo(type2, res_dir+cairo_file)
      valid_type = True
    end if

    if (type2.eq."xwd") then
      image_file = get_res_value(type2,"wkFileName", res_file)
      image_file@res_file = res_file
      wks = gsn_open_image(type2, res_dir+image_file)
      valid_type = True
    end if

    if (.not.valid_type.or.ismissing(wks)) then
      print("Error: gsn_open_wks: '"+ type2 + "' is an invalid workstation type.")
      exit
    end if

;
; Apply other resources.
;
; First create list of resources that we *don't* want applied, as we've
; should have applied them by this point.
;
  varatts = getvaratts(type2)
  if(.not.any(ismissing(varatts))) then
    wks_res = True
    res_list = (/"wkColorMap","wkWidth","wkHeight","wkColorModel", \
                 "wkPSResolution","wkPDFResolution","wkVisualType", \
                 "wkPaperSize","wkPaperWidthF","wkPaperHeightF", \
                 "_FillValue"/)
    do i=0,dimsizes(varatts)-1
      if(all(varatts(i).ne.res_list)) then
        wks_res@$varatts(i)$ = type2@$varatts(i)$
      end if  
    end do
    attsetvalues_check(wks,wks_res)
    delete(wks_res)
  end if
  delete(varatts)
;
; Return workstation and application id.
;
    wks@name = res_file
    wks@app  = appid 
    return(wks)
end

;***********************************************************************;
; Function : gsn_add_annotation                                         ;
;                plotid : graphic                                       ;
;                annoid : graphic                                       ;
;             resources : logical                                       ;
;                                                                       ;
; This function attaches one graphical object to another, for example,  ;
; a labelbar to a contour plot. The default is for the annotation to be ;
; added to the center of the plot. You can use the amJust resource      ;
; to change the general location of the annotation (top/center,         ;
; top/left top/right, bottom/center, bottom/right, etc. You can use     ;
; the amOrthogonalPosF and amParallelPosF resources to then move the    ;
; annotation perpendicular or parallel to the plot.                     ;
;                                                                       ;
; "amJust" is the corner or side of the annotation of which you want to ;
; position using values for "amParallelPosF" and "amOrthogonalPosF". It ;
; can be any one of the four corners, or the center of any edge of the  ;
; annotation.                                                           ;
;                                                                       ;
; "amParallelPosF" is the amount to move the annotation to the right or ;
; left, and "amOrthogonalPosF" is the amount to move it up and down. The;
; move is applied to the corner or the side of the annotation that is   ;
; indicated by "amJust".                                                ;
;                                                                       ;
; Here's what various values of amParallelPosF and amOrthogonalPosF     ;
; mean for moving the annotation:                                       ;
;                                                                       ;
; amParallelPosF/amOrthogonalPosF                                       ;
;    0.0/ 0.0  -  annotation in dead center of plot                     ;
;    0.5/ 0.5  -  annotation at bottom right of plot                    ;
;    0.5/-0.5  -  annotation at top right of plot                       ;
;   -0.5/-0.5  -  annotation at top left of plot                        ;
;   -0.5/ 0.5  -  annotation at bottom left of plot                     ;
;                                                                       ;
; So, for example, an amJust value of "TopRight" and amParallelPosF,    ;
; amOrthogonalPosF values of 0.5 and -0.5 will position the top right   ;
; corner of the annotation in the top right corner of the plot.         ;
;                                                                       ;
; Values of just = "TopCenter", para = -0.5, orth = -0.5 will position  ;
; the top center of the annotation in the top left corner of the plot,  ;
; effectively placing part of the annotation outside the plot.          ;
;                                                                       ;
; Since adding an annotation to a plot can make it bigger, this         ;
; function will recognize gsnMaximize if it is set, and resize the plot ;
; if necessary.                                                         ;
;                                                                       ;
;***********************************************************************;
undef("gsn_add_annotation")
function gsn_add_annotation(plot:graphic, anno:graphic, resources:logical)
local res2, just, para, orth, just, zone, resize, maxbb, tmp_wks
begin
  res2  = get_resources(resources)
  just = get_res_value(res2,"amJust","CenterCenter")
  para = get_res_value(res2,"amParallelPosF",0)
  orth = get_res_value(res2,"amOrthogonalPosF",0)
  zone = get_res_value(res2,"amZone",0)
  resize = get_res_value(res2,"amResizeNotify",True)
  maxbb = get_bb_res(res2)
;
; Add annotation to plot.
;
   anno_id = NhlAddAnnotation(plot,anno)
;
; Set some resource values.
;
   setvalues anno_id
     "amZone"           : zone
     "amJust"           : just
     "amParallelPosF"   : para
     "amOrthogonalPosF" : orth
     "amResizeNotify"   : resize
   end setvalues
;
; Apply rest of resources, if any.
;
   attsetvalues_check(anno_id,res2)
;
; Remaximize the plot if necessary.
;
   tmp_wks = NhlGetParentWorkstation(plot)
   draw_and_frame(tmp_wks,plot,False,False,False,maxbb)
;
; Return id
;
   return(anno_id)
end

;***********************************************************************;
; Function : gsn_add_primitive                                          ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;                 isndc: NDC space or not                               ;
;              polytype: type of primitive                              ;
;             resources: optional resources                             ;
;                                                                       ;
; This function adds a primitive to the plot "plotid", either in the    ;
; same data space as the data in the plot, or in NDC space. ("plotid"   ;
; is returned from a previous call to one of the gsn_* plotting         ;
; functions), "x" and "y" are the x and y locations of each point in the;
; primitive, and should either be in the same data space as the data    ;
; from "plotid" (isndc = False) or should be values from 0 to 1 if in   ;
; NDC space (isndc = True). Note that using isndc = True should only    ;
; be used internally. It currently only works for NDC coordinates that  ;
; actually fall within the plot's boundaries.                           ;
;                                                                       ;
; "resources" is an optional list of resources.  This function returns  ;
; the primitive object created. polytype is the type of primitive to    ;
; add (polymarker, polygon, or polyline)                                ;
;                                                                       ;
;***********************************************************************;
undef("gsn_add_primitive")
function gsn_add_primitive(wks:graphic,plotid:graphic,x[*]:numeric,\
                           y[*]:numeric,isndc:logical,polytype:string, \
                           resources:logical)
local res2, gsid, gsres, prim_object, dummy, tfres, vpx, vpy, vpw, vph
begin
  if(.not.any(polytype.eq.(/"polymarker","polygon","polyline"/)))
    print("Warning: gsn_add_primitive: Do not recognize primitive type '"+ polytype + "'.")
   return(0)
  end if

  res2 = get_resources(resources)  ; Make a copy of resource list.

;
; After V5.1.1, this function was updated to recognize 
; "tf*" resources, and have them applied to the plot
; in question. This is especially important for the 
; "tfPolyDrawOrder" resource, which allows you to control
; the order which primitives are drawn.
;
  tfres = get_res_eq(res2,"tf")
  attsetvalues_check(plotid,tfres)

;
; If in NDC space, make sure X and Y values are in the range 0 to 1 AND
; within the viewport of the polot.
;
  if(isndc) then
    getvalues plotid
      "vpHeightF" : vph
      "vpWidthF"  : vpw
      "vpXF"      : vpx
      "vpYF"      : vpy
    end getvalues
;
; This part is commented out because sometimes if values are equal
; to each other, it will incorrectly register as one being greater/less
; than the other value.
;
;    if(any(x.lt.vpx.or.x.gt.(vpx+vpw).or.y.gt.vpy.or.y.lt.(vpy-vph))) then
;      print("Warning: gsn_add_primitive: The X and Y values must be between the viewport values of the plot if you are in NDC space.")
;      return(0)
;    end if
;
; Create a canvas to draw on.
;
    canvas = create "ndc_canvas" logLinPlotClass wks
      "tfDoNDCOverlay" : True
      "trXMinF"        : vpx
      "trXMaxF"        : vpx+vpw
      "trYMaxF"        : vpy
      "trYMinF"        : vpy-vph
    end create
  end if 
;
; Create a graphic style object.  We have to do this instead of using
; the default one, because if we add two primitive objects to a plot
; and assign each one a different color, the two objects will have the
; same color as the last color that was set.

  gsid = create "graphic_style" graphicStyleClass wks end create
;
; Set graphic style resources, if any.
;
  gsres = get_res_eq(res2,"gs")
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
; Add primitive to the plot object.  If in NDC space, then add it to the 
; canvas, and then add the canvas as an annotation.
;
        dummy = new(1,graphic)
        if(isndc) then
          NhlAddPrimitive(canvas,prim_object,dummy)
          overlay(plotid,canvas)
        else
          NhlAddPrimitive(plotid,prim_object,dummy)
        end if
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
          if(isndc) then
            NhlAddPrimitive(canvas,prim_object(i),dummy)
          else
            NhlAddPrimitive(plotid,prim_object(i),dummy)
          end if
        end do
;
; If in NDC space, we need to add the canvas as an annotation of
; the plot.
;
        if(isndc) then
          overlay(plotid,canvas)
        end if
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
    if(isndc) then
      NhlAddPrimitive(canvas,prim_object,dummy)
	  overlay(plotid,canvas)
    else
      NhlAddPrimitive(plotid,prim_object,dummy)
    end if
  end if

  return(prim_object)
end

;***********************************************************************;
; Function : gsn_primitive                                              ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;                 isndc: NDC space or not                               ;
;              polytype: type of primitive                              ;
;             resources: optional resources                             ;
;                                                                       ;
; This function draws a primitive to the plot "plotid", either in the   ;
; same data space as the data in the plot, or in NDC space. ("plotid"   ;
; is returned from a previous call to one of the gsn_* plotting         ;
; functions), "x" and "y" are the x and y locations of each point in the;
; primitive, and should either be in the same data space as the data    ;
; from "plotid" (isndc = False) or should be values from 0 to 1 if in   ;
; NDC space (isndc = True). Note that using isndc = True should only    ;
; be used internally. It currently only works for NDC coordinates that  ;
; actually fall within the plot's boundaries.                           ;
;                                                                       ;
; "resources" is an optional list of resources.                         ;
;                                                                       ;
;***********************************************************************;
undef("gsn_primitive")
procedure gsn_primitive(wks:graphic,plotid:graphic,x[*]:numeric,\
                        y[*]:numeric,isndc:logical,polytype:string, \
                        resources:logical)
local res2, gsid, gsres, gmres, canvas, xf, yf, x2, y2
begin
  if(.not.any(polytype.eq.(/"polymarker","polygon","polyline"/)))
    print("Warning: gsn_primitive: Do not recognize primitive type '"+ polytype + "'.")
    return
  end if

  res2 = get_resources(resources)

; Create graphic style object.

  gsid = create "graphic_style" graphicStyleClass wks end create

;
; Create a canvas to draw on, if this is an NDC draw.
; 
  if(isndc) then
    canvas = create_canvas(wks)
  end if

  gsres = get_res_eq(res2,"gs")
  attsetvalues_check(gsid,gsres)
;
; Make sure data is float, since NhlDataPolymarker only takes floats.
;
  xf = tofloat(x)
  yf = tofloat(y)
;
; Since the NhlData*/NhlNDC* routines don't accept missing values, this
; routine only draws the ones that aren't missing. For polylines, a pen up
; and pen down takes place after each section of missing values.  We'll
; handle this later.
;
  if(.not.any(ismissing(xf)).and..not.any(ismissing(yf))) then
    x2 = xf
    y2 = yf
    nomsg = True
  else
    x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
    y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
    nomsg = False
  end if

  if(polytype.eq."polymarker") then
    if(isndc) then
      NhlNDCPolymarker(canvas,gsid,x2,y2)
    else
      NhlDataPolymarker(plotid,gsid,x2,y2)
    end if
  end if
  if(polytype.eq."polygon") then
    if(isndc) then
      NhlNDCPolygon(canvas,gsid,x2,y2)
    else
      NhlDataPolygon(plotid,gsid,x2,y2)
    end if
  end if
  if(polytype.eq."polyline".and.nomsg) then
    if(isndc) then
      NhlNDCPolyline(canvas,gsid,x2,y2)
    else
      NhlDataPolyline(plotid,gsid,x2,y2)
    end if
  end if
  if(polytype.eq."polyline".and..not.nomsg) then
    first_marker = True
;
; If we end up with a line with just one point, then we draw it with
; a polymarker. Thus, we need to make sure the marker will be the
; same color as the line.
; 
    gmres = False
    if(isatt(gsres,"gsLineColor"))
      gmres               = True
      gmres@gsMarkerColor = gsres@gsLineColor
    end if

    indices = get_non_missing_pairs(xf,yf)
    i = 0
    do while(.not.ismissing(indices(i,0)).and.i.lt.dimsizes(xf))
      ibeg = indices(i,0)
      iend = indices(i,1)
      if(iend.gt.ibeg)
        if(isndc) then
          NhlNDCPolyline(canvas,gsid,xf(ibeg:iend),yf(ibeg:iend))
        else
          NhlDataPolyline(plotid,gsid,xf(ibeg:iend),yf(ibeg:iend))
        end if
      else    ; iend = ibeg --> only one point
        if(first_marker)
          attsetvalues_check(gsid,gmres)
          first_marker = False
        end if
        if(isndc) then
          NhlNDCPolymarker(canvas,gsid,xf(ibeg),yf(ibeg))
        else
          NhlDataPolymarker(plotid,gsid,xf(ibeg),yf(ibeg))
        end if
      end if
      i = i + 1
    end do
    delete(indices)
  end if
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
undef("gsn_polygon")
procedure gsn_polygon(wks:graphic,plotid:graphic,x[*]:numeric,\
                      y[*]:numeric,resources:logical)
local res2
begin
  res2 = get_resources(resources)
  gsn_primitive(wks,plotid,x,y,False,"polygon",res2)
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
undef("gsn_add_polygon")
function gsn_add_polygon(wks:graphic,plotid:graphic,x[*]:numeric,\
                         y[*]:numeric,resources:logical)
begin
  res2 = get_resources(resources)
  return(gsn_add_primitive(wks,plotid,x,y,False,"polygon",res2))
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
undef("gsn_polygon_ndc")
procedure gsn_polygon_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                          resources:logical)
local res2, dummy
begin
  dummy = new(1,graphic)
  res2 = get_resources(resources)
  gsn_primitive(wks,dummy,x,y,True,"polygon",res2)
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
undef("gsn_polyline")
procedure gsn_polyline(wks:graphic,plotid:graphic,x[*]:numeric,\
                       y[*]:numeric,resources:logical)
local res2
begin
  res2 = get_resources(resources)
  gsn_primitive(wks,plotid,x,y,False,"polyline",res2)
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
undef("gsn_add_polyline")
function gsn_add_polyline(wks:graphic,plotid:graphic,x[*]:numeric,\
                           y[*]:numeric,resources:logical)
local res2
begin
  res2 = get_resources(resources)
  return(gsn_add_primitive(wks,plotid,x,y,False,"polyline",res2))
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
undef("gsn_polyline_ndc")
procedure gsn_polyline_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                           resources:logical)
local res2, dummy
begin
  dummy = new(1,graphic)
  res2 = get_resources(resources)
  gsn_primitive(wks,dummy,x,y,True,"polyline",res2)
end

;***********************************************************************;
; Procedure : gsn_polymarker                                            ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: n-dimensional array of x points                ;
;                     y: n-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws polymarkers on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks") in the same ;
; data space as the data in "plotid" (returned from a previous call to  ;
; one of the gsn_* plotting functions). "x" and "y" are the x and y     ;
; locations of each marker, and should be in the same data space as the ;
; data from "plotid". "resources" is an optional list of resources.     ;
;***********************************************************************;
undef("gsn_polymarker")
procedure gsn_polymarker(wks:graphic,plotid:graphic,x:numeric,\
                         y:numeric,resources:logical)
local res2, rank
begin
  res2 = get_resources(resources)
  rank = dimsizes(dimsizes(x))
  if(rank.eq.1) then
    gsn_primitive(wks,plotid,x,y,False,"polymarker",res2)
  else
    gsn_primitive(wks,plotid,ndtooned(x),ndtooned(y),False,"polymarker",res2)
  end if
end

;***********************************************************************;
; Function : gsn_add_polymarker                                         ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: n-dimensional array of x points                ;
;                     y: n-dimensional array of y points                ;
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
undef("gsn_add_polymarker")
function gsn_add_polymarker(wks:graphic,plotid:graphic,x:numeric,\
                            y:numeric,resources:logical)
local res2, rank
begin
  res2 = get_resources(resources)

  rank = dimsizes(dimsizes(x))
  if(rank.eq.1) then
    return(gsn_add_primitive(wks,plotid,x,y,False,"polymarker",res2))
  else
    return(gsn_add_primitive(wks,plotid,ndtooned(x),ndtooned(y),False,"polymarker",res2))
  end if
end

;***********************************************************************;
; Procedure : gsn_shapefile_polylines                                   ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                 fname: Name of shapefile ("xxxx.shp")                 ;
;               resources: optional resources                           ;
;                                                                       ;
; ANY CHANGES MADE TO THIS CODE SHOULD POTENTIALLY BE MADE TO           ;
; gsn_add_shapefile_polylines BELOW!! Note that in V6.2.0, the "add"    ;
; version of this code was sped up significantly, so this procedure is  ;
; not needed so much anymore.                                           ;
;
; This function draws shapefile polylines on the plot "plotid".         ;
; See gsn_add_shapefile_polylines if you want to add them instead (this ;
; can be much slower).                                                  ;
;                                                                       ;
; In version 6.1.0, some code was added to add checks if the lat/lon    ;
; segments are within the range of the map. This works best for a C.E.  ;
; map. You have to set the special min/max/lat/lon attributes for this  ;
; to work.  I won't advertise this yet, because the interface could     ;
; probably be made better. See note about V6.2.0 above.                 ;
;***********************************************************************;
undef("gsn_shapefile_polylines")
procedure gsn_shapefile_polylines(wks,plot,fname:string,lnres)
local f, segments, geometry, segsDims, geomDims, geom_segIndex, \
geom_numSegs, segs_xyzIndex, segs_numPnts, numFeatures, i, lat, lon, \
startSegment, numSegments, seg, startPT, endPT
begin
;---Open the shapefile
  f = addfile(fname,"r")

;---Error checking
  if(ismissing(f)) then
    print("Error: gsn_shapefile_polylines: Can't open shapefile '" + \
           fname + "'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;---We can't use this routine to plot point data
  if(.not.any(f@geometry_type.eq.(/"polygon","polyline"/))) then
    print("Error: gsn_shapefile_polylines: geometry_type attribute must be 'polygon' or 'polyline'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;---Read data off the shapefile
  geomDims    = getfilevardimsizes(f,"geometry")
  numFeatures = geomDims(0)
  if(numFeatures.eq.0) then
    print("Error: gsn_shapefile_polylines: the number of features in this file is 0.")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

  segments = f->segments
  geometry = f->geometry
  segsDims = dimsizes(segments)

;---Read global attributes  
  geom_segIndex = f@geom_segIndex
  geom_numSegs  = f@geom_numSegs
  segs_xyzIndex = f@segs_xyzIndex
  segs_numPnts  = f@segs_numPnts

;---Section to attach polylines to plot.
  lon = f->x
  lat = f->y
;
; Special check for minlat/maxlat/minlon/maxlon attributes.
;
; If set, then each lat/lon segment will be checked if it's
; in the range.  This can speed up plotting, but I need to
; verify this!
; 
  if(isatt(lnres,"minlon").and.isatt(lnres,"maxlon").and.\
     isatt(lnres,"minlat").and.isatt(lnres,"maxlat")) then
    do i=0, numFeatures-1  
       startSegment = geometry(i, geom_segIndex)
       numSegments  = geometry(i, geom_numSegs)
       do seg=startSegment, startSegment+numSegments-1
          startPT = segments(seg, segs_xyzIndex)
          endPT   = startPT + segments(seg, segs_numPnts) - 1
          lat_sub = lat(startPT:endPT)
          lon_sub = lon(startPT:endPT) 
          if(.not.(all(lon_sub.lt.lnres@minlon).or. \
                   all(lon_sub.gt.lnres@maxlon).or. \
                   all(lat_sub.lt.lnres@minlat).or. \
                   all(lat_sub.gt.lnres@maxlat))) then
            gsn_polyline(wks, plot, lon_sub, lat_sub, lnres)
          end if
          delete([/lat_sub,lon_sub/])
       end do
    end do
  else       ; Don't do any range checking. 
    do i=0, numFeatures-1  
       startSegment = geometry(i, geom_segIndex)
       numSegments  = geometry(i, geom_numSegs)
       do seg=startSegment, startSegment+numSegments-1
          startPT = segments(seg, segs_xyzIndex)
          endPT   = startPT + segments(seg, segs_numPnts) - 1
          gsn_polyline(wks, plot, lon(startPT:endPT),  \
                                  lat(startPT:endPT), lnres)
       end do
    end do
  end if
end

;***********************************************************************;
; Function : gsn_add_shapefile_polylines                                ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                 fname: Name of shapefile ("xxxx.shp")                 ;
;               resources: optional resources                           ;
;                                                                       ;
; ANY CHANGES MADE TO THIS CODE SHOULD POTENTIALLY BE MADE TO           ;
; gsn_shapefile_polylines ABOVE!!                                       ;
;                                                                       ;
; This function attaches shapefile polylines to the plot "plotid".      ;
; See gsn_shapefile_polylines if you want to just draw them.            ; 
;                                                                       ;
; In version 6.2.0 this function was significantly sped-up via the use  ;
; of a new resource called "gsSegments". Only one primitive object id   ;
; is now created. In some cases, we saw speed-ups of 80x (for example,  ;
; with the France_adm5 shapefile). Because of this speed-up, the code   ;
; added in V6.1.0 for checking for special minlat/maxlat/minlon/maxlon  ;
; is no longer needed. It is commented out for now. It may eventually   ;
; be removed once we are certain it doesn't serve any benefit.          ;
;                                                                       ;
; In version 6.1.0, some code was added to add checks if the lat/lon    ;
; segments are within the range of the map. This works best for a C.E.  ;
; map. You have to set the special min/max/lat/lon attributes for this  ;
; to work.  I won't advertise this yet, because the interface could     ;
; probably be made better. THIS CODE HAS BEEN COMMENTED OUT in V6.2.0.  ;
;                                                                       ;
; In version 6.2.1, this function was modified to allow multiple plots. ;
;***********************************************************************;
undef("gsn_add_shapefile_polylines")
function gsn_add_shapefile_polylines(wks,plots[*]:graphic,fname:string,lnres)
local f, geomDims, numFeatures, lnres2
begin
;---Open the shapefile
  f = addfile(fname,"r")

;---Error checking
  if(ismissing(f)) then
    print("Error: gsn_add_shapefile_polylines: Can't open shapefile '" + \
           fname + "'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;---We can't use this routine to plot point data
  if(.not.any(f@geometry_type.eq.(/"polygon","polyline"/))) then
    print("Error: gsn_add_shapefile_polylines: geometry_type attribute must be 'polygon' or 'polyline'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

  if(lnres) then
    lnres2 = lnres      ; Make a copy so that we don't keep gsSegments
  else
    lnres2 = True
  end if

;---Read data off the shapefile
  geomDims    = getfilevardimsizes(f,"geometry")
  numFeatures = geomDims(0)
  if(numFeatures.eq.0) then
    print("Error: gsn_add_shapefile_polylines: the number of features in this file is 0.")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;;  segments = f->segments
;;  geometry = f->geometry
;;  segsDims = dimsizes(segments)
;;
;;;---Read global attributes  
;;  geom_segIndex = f@geom_segIndex
;;  geom_numSegs  = f@geom_numSegs
;;  segs_xyzIndex = f@segs_xyzIndex
;;  segs_numPnts  = f@segs_numPnts
;;
;;;---Create array to hold all polylines
;;  npoly = sum(geometry(:,geom_numSegs)) 
;;  poly  = new(npoly,graphic)
;;
;;;---Section to attach polylines to plot.
;;  lon = f->x
;;  lat = f->y
;;  npl = 0     ; polyline counter
;;;
;;; Special check for minlat/maxlat/minlon/maxlon attributes.
;;;
;;; If set, then each lat/lon segment will be checked if it's
;;; in the range.  This can speed up plotting, but I need to
;;; verify this!
;;; 
;;  if(isatt(lnres,"minlon").and.isatt(lnres,"maxlon").and.\
;;     isatt(lnres,"minlat").and.isatt(lnres,"maxlat")) then
;;    do i=0, numFeatures-1  
;;       startSegment = geometry(i, geom_segIndex)
;;       numSegments  = geometry(i, geom_numSegs)
;;       do seg=startSegment, startSegment+numSegments-1
;;          startPT = segments(seg, segs_xyzIndex)
;;          endPT   = startPT + segments(seg, segs_numPnts) - 1
;;          lat_sub = lat(startPT:endPT)
;;          lon_sub = lon(startPT:endPT) 
;;          if(.not.(all(lon_sub.lt.lnres@minlon).or. \
;;                   all(lon_sub.gt.lnres@maxlon).or. \
;;                   all(lat_sub.lt.lnres@minlat).or. \
;;                   all(lat_sub.gt.lnres@maxlat))) then
;;            poly(npl) = gsn_add_polyline(wks, plot, lon_sub, lat_sub, lnres)
;;            npl = npl + 1
;;          end if
;;          delete([/lat_sub,lon_sub/])
;;       end do
;;    end do
;;  else       ; Don't do any range checking. 
;;    do i=0, numFeatures-1  
;;       startSegment = geometry(i, geom_segIndex)
;;       numSegments  = geometry(i, geom_numSegs)
;;       do seg=startSegment, startSegment+numSegments-1
;;          startPT = segments(seg, segs_xyzIndex)
;;          endPT   = startPT + segments(seg, segs_numPnts) - 1
;;          poly(npl) = gsn_add_polyline(wks, plot, lon(startPT:endPT),  \
;;                                       lat(startPT:endPT), lnres)
;;          npl = npl + 1
;;       end do
;;    end do
;;  end if
;;  return(poly(0:npl-1))

;---This is all that's needed in V6.2.0.
  lnres2@gsSegments = f->segments(:,0)
  nplots = dimsizes(plots)
  poly   = new(nplots,graphic)
  do i=0,nplots-1
    poly(i) = gsn_add_polyline(wks, plots(i), f->x, f->y, lnres2)
  end do
  return(poly)
end

;***********************************************************************;
; Function : gsn_add_shapefile_polygons                                 ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                 fname: Name of shapefile ("xxxx.shp")                 ;
;               resources: optional resources                           ;
;                                                                       ;
; This function attaches shapefile polygons to the plot "plotid".       ;
;                                                                       ;
; In version 6.2.0 this function was significantly sped-up via the use  ;
; of a new resource called "gsSegments". Only one primitive object id   ;
; is now created. In some cases, we saw speed-ups of 80x (for example,  ;
; with the France_adm5 shapefile). Because of this speed-up, the code   ;
; added in V6.1.0 for checking for special minlat/maxlat/minlon/maxlon  ;
; is no longer needed. It is commented out for now. It may eventually   ;
; be removed once we are certain it doesn't serve any benefit.          ;
;                                                                       ;
; In version 6.1.0, some code was added to add checks if the lat/lon    ;
; segments are within the range of the map. This works best for a C.E.  ;
; map. You have to set the special min/max/lat/lon attributes for this  ;
; to work.  I won't advertise this yet, because the interface could     ;
; probably be made better. THIS CODE HAS BEEN COMMENTED OUT in V6.2.0.  ;
;                                                                       ;
; In version 6.2.1, this function was modified to allow multiple plots. ;
;***********************************************************************;
undef("gsn_add_shapefile_polygons")
function gsn_add_shapefile_polygons(wks,plots[*]:graphic,fname:string,gnres)
local f, geomDims, numFeatures, gnres2
begin
;---Open the shapefile
  f = addfile(fname,"r")

;---Error checking
  if(ismissing(f)) then
    print("Error: gsn_add_shapefile_polygons: Can't open shapefile '" + \
           fname + "'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;---We can't use this routine to plot point data
  if(f@geometry_type.ne."polygon") then
    print("Error: gsn_add_shapefile_polygon: geometry_type attribute must be 'polygon'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;---Read data off the shapefile
  geomDims    = getfilevardimsizes(f,"geometry")
  numFeatures = geomDims(0)
  if(numFeatures.eq.0) then
    print("Error: gsn_add_shapefile_polygon: the number of features in this file is 0.")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

  if(gnres) then
    gnres2 = gnres      ; Make a copy so that we don't keep gsSegments
  else
    gnres2 = True
  end if

;---Get the number of colors
  if(.not.gnres2.or.(.not.isatt(gnres2,"gsFillColor").and.\
                     .not.isatt(gnres2,"gsColors"))) then
    getvalues wks
      "wkColorMapLen"  : cmap_len
    end getvalues
    gnres2          = True   ; Make sure this is True
    gnres2@gsColors = toint(random_uniform(2,cmap_len-2,numFeatures))
  end if

;;  segments = f->segments
;;  geometry = f->geometry
;;  segsDims = dimsizes(segments)
;;
;;;---Read global attributes  
;;  geom_segIndex = f@geom_segIndex
;;  geom_numSegs  = f@geom_numSegs
;;  segs_xyzIndex = f@segs_xyzIndex
;;  segs_numPnts  = f@segs_numPnts
;;
;;;---Create array to hold all polylines
;;  npoly = sum(geometry(:,geom_numSegs)) 
;;  poly  = new(npoly,graphic)
;;
;;;---Section to attach polygons to plot.
;;  lon = f->x
;;  lat = f->y
;;  npl = 0     ; polyline counter
;;;
;;; Special check for minlat/maxlat/minlon/maxlon attributes.
;;;
;;; If set, then each lat/lon segment will be checked if it's
;;; in the range.  This can speed up plotting, but I need to
;;; verify this!
;;; 
;;  if(isatt(gnres,"minlon").and.isatt(gnres,"maxlon").and.\
;;     isatt(gnres,"minlat").and.isatt(gnres,"maxlat")) then
;;    do i=0, numFeatures-1  
;;       startSegment = geometry(i, geom_segIndex)
;;       numSegments  = geometry(i, geom_numSegs)
;;       do seg=startSegment, startSegment+numSegments-1
;;          startPT = segments(seg, segs_xyzIndex)
;;          endPT   = startPT + segments(seg, segs_numPnts) - 1
;;          lat_sub = lat(startPT:endPT)
;;          lon_sub = lon(startPT:endPT) 
;;          if(set_fill_color) then
;;;---Pick a random color 
;;            gnres@gsFillColor = toint(random_uniform(2,cmap_len-2,1))
;;          end if
;;          if(.not.(all(lon_sub.lt.gnres@minlon).or. \
;;                   all(lon_sub.gt.gnres@maxlon).or. \
;;                   all(lat_sub.lt.gnres@minlat).or. \
;;                   all(lat_sub.gt.gnres@maxlat))) then
;;;---Attach the line segment
;;            poly(npl) = gsn_add_polygon(wks, plot, lon_sub, lat_sub, gnres)
;;            npl = npl + 1
;;          end if
;;          delete([/lat_sub,lon_sub/])
;;       end do
;;    end do
;;  else
;;    do i=0, numFeatures-1  
;;       startSegment = geometry(i, geom_segIndex)
;;       numSegments  = geometry(i, geom_numSegs)
;;       do seg=startSegment, startSegment+numSegments-1
;;          startPT = segments(seg, segs_xyzIndex)
;;          endPT   = startPT + segments(seg, segs_numPnts) - 1
;;          if(set_fill_color) then
;;;---Pick a random color 
;;            gnres@gsFillColor = toint(random_uniform(2,cmap_len-2,1))
;;          end if
;;;---Attach the line segment
;;          poly(npl) = gsn_add_polygon(wks, plot, lon(startPT:endPT),  \
;;                                      lat(startPT:endPT), gnres)
;;          npl = npl + 1
;;       end do
;;    end do
;;  end if
;;  return(poly)

;---This is all that's needed in V6.2.0.
  gnres2@gsSegments = f->segments(:,0)
  nplots = dimsizes(plots)
  poly   = new(nplots,graphic)
  do i=0,nplots-1
    poly(i) = gsn_add_polygon(wks, plots(i), f->x, f->y, gnres2)
  end do
  return(poly)
end

;***********************************************************************;
; Function : gsn_add_shapefile_polymarkers                              ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                 fname: Name of shapefile ("xxxx.shp")                 ;
;               resources: optional resources                           ;
;                                                                       ;
; This function attaches shapefile point data to the plot "plotid".     ;
;                                                                       ;
; In version 6.1.0, some code was added to add checks if the lat/lon    ;
; segments are within the range of the map. This works best for a C.E.  ;
; map. You have to set the special min/max/lat/lon attributes for this  ;
; to work.  I won't advertise this yet, because the interface could     ;
; probably be made better.                                              ;
;                                                                       ;
; In version 6.2.1, this function was modified to allow multiple plots. ;
;***********************************************************************;
undef("gsn_add_shapefile_polymarkers")
function gsn_add_shapefile_polymarkers(wks,plots[*]:graphic,fname:string,mkres)
local f, segments, geometry, segsDims, geomDims, geom_segIndex, \
geom_numSegs, segs_xyzIndex, segs_numPnts, numFeatures, i, lat, lon, \
startSegment, numSegments, seg, startPT, endPT, npoly, npl
begin
;---Open the shapefile
  f = addfile(fname,"r")

;---Error checking
  if(ismissing(f)) then
    print("Error: gsn_add_shapefile_polymarkers: Can't open shapefile '" + \
           fname + "'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;---We can't use this routine to plot point data
  if(f@geometry_type.ne."point") then
    print("Error: gsn_add_shapefile_polymarkers: geometry_type attribute must be 'point'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;---Read data off the shapefile
  geomDims    = getfilevardimsizes(f,"geometry")
  numFeatures = geomDims(0)
  if(numFeatures.eq.0) then
    print("Error: gsn_add_shapefile_polymarkers: the number of features in this file is 0.")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

  segments = f->segments
  geometry = f->geometry
  segsDims = dimsizes(segments)

;---Read global attributes  
  geom_segIndex = f@geom_segIndex
  geom_numSegs  = f@geom_numSegs
  segs_xyzIndex = f@segs_xyzIndex
  segs_numPnts  = f@segs_numPnts

;---Create array to hold all polymarkers
  npoly  = sum(geometry(:,geom_numSegs)) 
  nplots = dimsizes(plots)
  poly   = new(npoly*nplots,graphic)

;---Section to attach polymarkers to plots.
  lon = f->x
  lat = f->y
  npl = 0     ; polyline counter
;
; Special check for minlat/maxlat/minlon/maxlon attributes.
;
; If set, then each lat/lon segment will be checked if it's
; in the range.  This can speed up plotting, but I need to
; verify this!
; 
  if(isatt(mkres,"minlon").and.isatt(mkres,"maxlon").and.\
     isatt(mkres,"minlat").and.isatt(mkres,"maxlat")) then
    do i=0, numFeatures-1  
       startSegment = geometry(i, geom_segIndex)
       numSegments  = geometry(i, geom_numSegs)
       do seg=startSegment, startSegment+numSegments-1
          startPT = segments(seg, segs_xyzIndex)
          endPT   = startPT + segments(seg, segs_numPnts) - 1
          lat_sub = lat(startPT:endPT)
          lon_sub = lon(startPT:endPT) 
          if(.not.(all(lon_sub.lt.mkres@minlon).or. \
                   all(lon_sub.gt.mkres@maxlon).or. \
                   all(lat_sub.lt.mkres@minlat).or. \
                   all(lat_sub.gt.mkres@maxlat))) then
;---Attach the markers
            do n=0,nplots-1
              poly(npl) = gsn_add_polymarker(wks, plots(n), lon_sub, lat_sub, mkres)
              npl = npl + 1
            end do
           end if
           delete([/lat_sub,lon_sub/])
         end do
    end do
  else       ; Don't do any range checking. 
    do i=0, numFeatures-1  
       startSegment = geometry(i, geom_segIndex)
       numSegments  = geometry(i, geom_numSegs)
       do seg=startSegment, startSegment+numSegments-1
          startPT = segments(seg, segs_xyzIndex)
          endPT   = startPT + segments(seg, segs_numPnts) - 1
;---Attach the markers
          do n=0,nplots-1
            poly(npl) = gsn_add_polymarker(wks, plots(n), lon(startPT:endPT),  \
                                           lat(startPT:endPT), mkres)
            npl = npl + 1
          end do
       end do
    end do
  end if
  return(poly(0:npl-1))
end

;***********************************************************************;
; Function : gsn_add_shapefile_text                                     ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                 fname: Name of shapefile ("xxxx.shp")                 ;
;                 vname: Name of string variable containing text strings;
;               resources: optional resources                           ;
;                                                                       ;
; This function attaches shapefile text strings to the plot "plotid".   ;
; The assumption is that there are "num_features" text strings, and this;
; routine gets the approximate mid lat/lon area for each text string.   ;
;***********************************************************************;
undef("gsn_add_shapefile_text")
function gsn_add_shapefile_text(wks,plot,fname[1]:string,vname[1]:string,txres)
local f, segments, geometry, segsDims, geomDims, geom_segIndex, \
geom_numSegs, segs_xyzIndex, segs_numPnts, numFeatures, i, lat, lon, \
startSegment, numSegments, seg, startPT, endPT, ntxt, \
minlat, maxlat, minlon, maxlon
begin
;---Open the shapefile
  f = addfile(fname,"r")
  if(.not.isfilevar(f,vname))
    print("Error: gsn_add_shapefile_text: '" + vname + "' is not a variable")
    print("       in file '" + fname + "'.")
    return(new(1,graphic))
  end if
;---Error checking
  if(ismissing(f)) then
    print("Error: gsn_add_shapefile_text: Can't open shapefile '" + \
           fname + "'")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

;---Read data off the shapefile
  geomDims    = getfilevardimsizes(f,"geometry")
  numFeatures = geomDims(0)
  if(numFeatures.eq.0) then
    print("Error: gsn_add_shapefile_text: the number of features in this file is 0.")
    print("       No shapefile information will be added.")
    return(new(1,graphic))
  end if

  segments = f->segments
  geometry = f->geometry
  segsDims = dimsizes(segments)

;---Read global attributes  
  geom_segIndex = f@geom_segIndex
  geom_numSegs  = f@geom_numSegs
  segs_xyzIndex = f@segs_xyzIndex
  segs_numPnts  = f@segs_numPnts
  numFeatures   = geomDims(0)

;---Create array to hold all text
  text  = new(numFeatures,graphic)

;---Section to attach text to plot.
  lon = f->x
  lat = f->y
  ntxt = 0     ; text counter
;
; Special check for minlat/maxlat/minlon/maxlon attributes.
;
; If set, then each lat/lon segment will be checked if it's
; in the range.  This can speed up plotting, but I need to
; verify this!
;
  if(isatt(txres,"minlon").and.isatt(txres,"maxlon").and.\
     isatt(txres,"minlat").and.isatt(txres,"maxlat")) then
    do i=0, numFeatures-1
       startSegment = geometry(i, geom_segIndex)
       numSegments  = geometry(i, geom_numSegs)
       minlat = new(1,typeof(lat))
       maxlat = new(1,typeof(lat))
       minlon = new(1,typeof(lon))
       maxlon = new(1,typeof(lon))
       do seg=startSegment, startSegment+numSegments-1
          startPT = segments(seg, segs_xyzIndex)
          endPT   = startPT + segments(seg, segs_numPnts) - 1
          lat_sub = lat(startPT:endPT)
          lon_sub = lon(startPT:endPT)
          if(.not.(all(lon_sub.lt.txres@minlon).or. \
                   all(lon_sub.gt.txres@maxlon).or. \
                   all(lat_sub.lt.txres@minlat).or. \
                   all(lat_sub.gt.txres@maxlat))) then
            if(any((/ismissing(minlat),ismissing(maxlat), \
                     ismissing(minlon),ismissing(maxlon)/))) then
              minlat = min(lat_sub)
              maxlat = max(lat_sub)
              minlon = min(lon_sub)
              maxlon = max(lon_sub)
            else
              minlat = min((/minlat,min(lat_sub)/))
              maxlat = max((/maxlat,max(lat_sub)/))
              minlon = min((/minlon,min(lon_sub)/))
              maxlon = max((/maxlon,max(lon_sub)/))
            end if
          end if
          delete([/lat_sub,lon_sub/])
       end do
;---Attach the text string
       if(.not.any((/ismissing(minlat),ismissing(maxlat), \
                     ismissing(minlon),ismissing(maxlon)/))) then
         avglat = (minlat+maxlat)/2.
         avglon = (minlon+maxlon)/2.
         print("Text = '" + f->$vname$(i) + "'")
         print("Location = " + avglat + "/" + avglon)
         text(ntxt) = gsn_add_text(wks, plot,f->$vname$(i),avglon,avglat,txres)
         ntxt = ntxt + 1
       end if
    end do
  else
    do i=0, numFeatures-1  
       startSegment = geometry(i, geom_segIndex)
       numSegments  = geometry(i, geom_numSegs)
       minlat = new(1,typeof(lat))
       maxlat = new(1,typeof(lat))
       minlon = new(1,typeof(lon))
       maxlon = new(1,typeof(lon))
       do seg=startSegment, startSegment+numSegments-1
          startPT = segments(seg, segs_xyzIndex)
          endPT   = startPT + segments(seg, segs_numPnts) - 1
          if(any((/ismissing(minlat),ismissing(maxlat), \
                   ismissing(minlon),ismissing(maxlon)/))) then
            minlat = min(lat(startPT:endPT))
            maxlat = max(lat(startPT:endPT))
            minlon = min(lon(startPT:endPT))
            maxlon = max(lon(startPT:endPT))
          else
            minlat = min((/minlat,min(lat(startPT:endPT))/))
            maxlat = max((/maxlat,max(lat(startPT:endPT))/))
            minlon = min((/minlon,min(lon(startPT:endPT))/))
            maxlon = max((/maxlon,max(lon(startPT:endPT))/))
          end if
       end do
;---Attach the text string
       if(.not.any((/ismissing(minlat),ismissing(maxlat), \
                     ismissing(minlon),ismissing(maxlon)/))) then
         avglat = (minlat+maxlat)/2.
         avglon = (minlon+maxlon)/2.
         text(ntxt) = gsn_add_text(wks,plot,f->$vname$(i),avglon,avglat,txres)
         ntxt = ntxt + 1
       end if
    end do
  end if
  return(text)
end


;***********************************************************************;
; Procedure : gsn_polymarker_ndc                                        ;
;                   wks: workstation object                             ;
;                     x: n-dimensional array of x points                ;
;                     y: n-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws polymarkers on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks") in NDC      ;
; space. "x" and "y" are the x and y locations of each marker in NDC    ;
; coordinates. "resources" is an optional list of resources.            ;
;***********************************************************************;
undef("gsn_polymarker_ndc")
procedure gsn_polymarker_ndc(wks:graphic,x:numeric,y:numeric,\
                             resources:logical)
local res2, dummy, rank
begin
  dummy = new(1,graphic)
  res2 = get_resources(resources)

  rank = dimsizes(dimsizes(x))
  if(rank.eq.1) then
    gsn_primitive(wks,dummy,x,y,True,"polymarker",res2)
  else
    gsn_primitive(wks,dummy,ndtooned(x),ndtooned(y),True,"polymarker",res2)
  end if
end

;***********************************************************************;
; Function : gsn_create_labelbar_ndc                                    ;
;                   wks: workstation object                             ;
;                  nbox: number of labelbar boxes                       ;
;                labels: labels for boxes                               ;
;                     x: X NDC position of labelbar                     ;
;                     y: Y NDC position of labelbar                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is identical to gsn_create_labelbar, except the location;
; of the labelbar is passed in (NDC coordinate values).                 ;
;***********************************************************************;
undef("gsn_create_labelbar_ndc")
function gsn_create_labelbar_ndc(wks:graphic, nbox:integer, labels:string, \
                                 x,y,resources:logical )
local res2, lbres, wksname
begin
    res2 = get_resources(resources)

    lbres = get_res_eq(res2,(/"lb","vp"/))

    wksname = get_res_value_keep(wks,"name","gsnapp")
;
; A special test is needed for the resource lbLabelFontHeightF.
; If it is set, then we need to turn off lbAutoManage. The
; user can override this, of course.
;
    if(lbres.and..not.any(ismissing(getvaratts(lbres))).and.\
        isatt(lbres,"lbLabelFontHeightF"))
      auto_manage = get_res_value(lbres,"lbAutoManage",False)
    else
      auto_manage = get_res_value(lbres,"lbAutoManage",True)
    end if

;
; If x,y < 0, this is invalid, and hence don't use these values.
; This was a special way to allow gsn_create_labelbar to call this
; routine without needing valid x, y values.
;
    if(x.lt.0.or.y.lt.0) then
      lbid = create wksname + "_labelbar" labelBarClass wks
        "lbBoxCount"     : nbox
        "lbLabelStrings" : labels
        "lbAutoManage"   : auto_manage
      end create
    else
      lbid = create wksname + "_labelbar" labelBarClass wks
        "vpXF"           : x
        "vpYF"           : y
        "lbBoxCount"     : nbox
        "lbLabelStrings" : labels
        "lbAutoManage"   : auto_manage
      end create
    end if

    if(lbres.and..not.any(ismissing(getvaratts(lbres))))
      attsetvalues_check(lbid,lbres)
    end if

; Return labelbar.

    return(lbid)
end

;***********************************************************************;
; Function : gsn_create_labelbar                                        ;
;                   wks: workstation object                             ;
;                  nbox: number of labelbar boxes                       ;
;                labels: labels for boxes                               ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and returns a labelbar on the workstation "wks" ;
; (the variable returned from a previous call to "gsn_open_wks").       ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;
undef("gsn_create_labelbar")
function gsn_create_labelbar(wks:graphic, nbox:integer, labels:string, \
                             resources:logical )
begin
    x = -1.   ; Special values to tip off the routine
    y = -1.   ; that we don't have X,Y values.

    labelbar = gsn_create_labelbar_ndc(wks, nbox, labels, x, y, resources)
    return(labelbar)
end

;***********************************************************************;
; Function : gsn_labelbar_ndc                                           ;
;                   wks: workstation object                             ;
;                  nbox: number of labelbar boxes                       ;
;                labels: labels for boxes                               ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is identical to gsn_create_labelbar_ndc except it draws ;
; the labelbar that's created.                                          ;
;***********************************************************************;
undef("gsn_labelbar_ndc")
procedure gsn_labelbar_ndc(wks:graphic, nbox:integer, labels:string, \
                           x,y,resources:logical )
local labelbar
begin
    labelbar = gsn_create_labelbar_ndc(wks, nbox, labels, x, y, resources)
    draw(labelbar)
    delete(labelbar)
end

;***********************************************************************;
; Function : gsn_create_legend_ndc                                      ;
;                   wks: workstation object                             ;
;                  nitems: number of legend items                       ;
;                labels: labels for items                               ;
;                     x: X NDC position of legend                       ;
;                     y: Y NDC position of legend                       ;
;               resources: optional resources                           ;
;                                                                       ;
; This function draws a legend on the workstation "wks" (the variable   ;
; returned from a previous call to "gsn_open_wks"). "resources" is an   ;
; optional list of resources.                                           ;
;***********************************************************************;
undef("gsn_create_legend_ndc")
function gsn_create_legend_ndc(wks:graphic, nitems:integer, labels:string, \
                               x,y,resources:logical )
local i, res2, lgres, wksname, lgres, item_order
begin
    res2 = get_resources(resources)

    wksname = get_res_value_keep(wks,"name","gsnapp")

    item_order = get_res_value(res2,"lgItemOrder",ispan(0,nitems-1,1))
;
; If x,y < 0, this is invalid, and hence don't use these values.
; This was a special way to allow gsn_create_legend to call this
; routine without needing valid x, y values.
;
    if(x.lt.0.or.y.lt.0) then
      legend = create wksname + "_legend" legendClass wks
        "lgItemCount"    : nitems
        "lgLabelStrings" : labels
        "lgItemOrder"    : item_order
      end create
    else
      legend = create wksname + "_legend" legendClass wks
        "vpXF"           : x
        "vpYF"           : y
        "lgItemCount"    : nitems
        "lgLabelStrings" : labels
        "lgItemOrder"    : item_order
      end create
    end if
    lgres = get_res_eq(res2,(/"lg","vp"/))
    if(lgres.and..not.any(ismissing(getvaratts(lgres))))

; A special test is needed for the resource lgLabelFontHeightF.
; If it is set, then we need to turn off lgAutoManage.

      if(isatt(lgres,"lgLabelFontHeightF"))
        setvalues legend
          "lgAutoManage"       : False
          "lgLabelFontHeightF" : lgres@lgLabelFontHeightF
        end setvalues
        delete(lgres@lgLabelFontHeightF)
      end if
      attsetvalues_check(legend,lgres)
    end if
; Return legend.

    return(legend)
end

;***********************************************************************;
; Function : gsn_create_legend                                          ;
;                   wks: workstation object                             ;
;                  nitems: number of legend items                       ;
;                labels: labels for items                               ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates a legend.                                       ;
;***********************************************************************;
undef("gsn_create_legend")
function gsn_create_legend(wks:graphic, nitems:integer, labels:string, \
                           resources:logical )
begin
    x = -1.   ; Special values to tip off the routine
    y = -1.   ; that we don't have X,Y values.

    legend = gsn_create_legend_ndc(wks, nitems, labels, x, y, resources)
    return(legend)
end

;***********************************************************************;
; Procedure : gsn_legend_ndc                                            ;
;                   wks: workstation object                             ;
;                  nitems: number of legend items                       ;
;                labels: labels for items                               ;
;                     x: X NDC position of legend                       ;
;                     y: Y NDC position of legend                       ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a legend on the workstation "wks" (the variable  ;
; returned from a previous call to "gsn_open_wks"). "resources" is an   ;
; optional list of resources.                                           ;
;***********************************************************************;
undef("gsn_legend_ndc")
procedure gsn_legend_ndc(wks:graphic, nitems:integer, labels:string, \
                         x,y,resources:logical )
local legend
begin
  legend = gsn_create_legend_ndc(wks,nitems,labels,x,y,resources)
  draw(legend)
  delete(legend)
end

;***********************************************************************;
; Function : gsn_create_text_ndc                                        ;
;                   wks: workstation object                             ;
;                  text: array of text strings                          ;
;                     x: n-dimensional array of x ndc positions         ;
;                     y: n-dimensional array of y ndc positions         ;
;               resources: optional resources                           ;
;                                                                       ;
; This function draws text strings on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks").  "x" and   ;
; "y" are the x and y locations of each text string, and should be      ;
; specified in NDC space. "resources" is an optional list of resources. ;
; This function returns the text string created.                        ;
;***********************************************************************;
undef("gsn_create_text_ndc")
function gsn_create_text_ndc(wks:graphic, texto:string, xo:numeric, \
                             yo:numeric, resources:logical )
local i, txid, plot_object, res, tx_res_index, x, y, res2, \
calldraw, callframe, xsize, ysize, tsize, len, dims
begin
;
; Any one of xo, yo, and texto can just be one element, but if two or more
; are more than one element, then they must be exactly the same size.
;
  xsize = product(dimsizes(xo))
  ysize = product(dimsizes(yo))
  tsize = product(dimsizes(texto))
  if((xsize.gt.1.and.ysize.gt.1.and.xsize.ne.ysize).or.\
     (xsize.gt.1.and.tsize.gt.1.and.xsize.ne.tsize).or.\
     (ysize.gt.1.and.tsize.gt.1.and.ysize.ne.tsize))
    print("Error: gsn_create_text_ndc: x, y, and text must have the same dimension sizes, or be scalars.")
    dummy = new(1,graphic)
    return(dummy)
  end if

;---Convert to 1-dimensional arrays of all the same length.
  all_scalar = all((/xsize,ysize,tsize/).eq.1)
  if(all_scalar.or.all((/xsize,ysize,tsize/).gt.1)) then
    len  = xsize
    dims = dimsizes(xo)
    x    = ndtooned(xo)
    y    = ndtooned(yo)
    text = ndtooned(texto)
  else  
;---We know at least one of these is not a scalar.
    len = max((/xsize,ysize,tsize/))
    if(xsize.gt.1) then
      x    = ndtooned(xo)
      dims = dimsizes(xo)
    else
      x = conform_dims(len,xo,-1)
    end if
    if(ysize.gt.1) then
      y    = ndtooned(yo)
      dims = dimsizes(yo)
    else
      y = conform_dims(len,yo,-1)
    end if
    if(tsize.gt.1) then
      text = ndtooned(texto)
      dims = dimsizes(texto)
    else
      text = conform_dims(len,texto,-1)
    end if
  end if
  res2 = get_resources(resources)

  wksname = get_res_value_keep(wks,"name","gsnapp")

  calldraw  = get_res_value(res2,"gsnDraw", False)
  callframe = get_res_value(res2,"gsnFrame",False)
  maxbb     = get_bb_res(res2)

  txres = get_res_eq(res2,"tx")  ; Get text resources.
  txid  = new(len,graphic)

  if((res2).and.isatt(res2,"txFuncCode")) then
;
; Special case where we don't have x,y values.
;
    if(all(x.lt.0).and.all(y.lt.0)) then
      do i=0,len-1
        txid(i) = create wksname + "_text_ndc"+i textItemClass wks
          "txString"   : text(i)
          "txFuncCode" : res2@txFuncCode
        end create
        attsetvalues_check(txid(i),txres)       ; Set text resources.
        draw_and_frame(wks,txid(i),calldraw,callframe,0,maxbb)
      end do
    else
      do i=0,len-1
        txid(i) = create wksname + "_text_ndc"+i textItemClass wks
          "txString" : text(i)
          "txPosXF"  : x(i)
          "txPosYF"  : y(i)
          "txFuncCode" : res2@txFuncCode
        end create
        attsetvalues_check(txid(i),txres)       ; Set text resources.
        draw_and_frame(wks,txid(i),calldraw,callframe,0,maxbb)
      end do
    end if
  else
;
; Special case where we don't have x,y values.
;
    if(all(x.lt.0).and.all(y.lt.0)) then
      do i=0,len-1
        txid(i) = create wksname + "_text_ndc"+i textItemClass wks
          "txString" : text(i)
        end create
        attsetvalues_check(txid(i),txres)       ; Set text resources.
        draw_and_frame(wks,txid(i),calldraw,callframe,0,maxbb)
      end do
    else
      do i=0,len-1
        txid(i) = create wksname + "_text_ndc"+i textItemClass wks
          "txString" : text(i)
          "txPosXF"  : x(i)
          "txPosYF"  : y(i)
        end create
        attsetvalues_check(txid(i),txres)       ; Set text resources.
        draw_and_frame(wks,txid(i),calldraw,callframe,0,maxbb)
      end do
    end if
  end if

  if(all_scalar) then
    return(txid)
  else
    return(reshape(txid,dims))
  end if
end

;***********************************************************************;
; Function : gsn_create_text                                            ;
;                   wks: workstation object                             ;
;                  text: text strings                                   ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates text strings.                                   ;
;***********************************************************************;
undef("gsn_create_text")
function gsn_create_text(wks:graphic, text:string, resources:logical )
local x, y
begin
    x = -1.   ; Special values to tip off the routine
    y = -1.   ; that we don't have X,Y values.

    txid = gsn_create_text_ndc(wks, text, x, y, resources)
    return(txid)
end


;***********************************************************************;
; Procedure : gsn_text_ndc                                              ;
;                                                                       ;
; This procedure is the same as gsn_text, only it doesn't return        ;
; anything.                                                             ;
;***********************************************************************;
undef("gsn_text_ndc")
procedure gsn_text_ndc(wks:graphic, text:string, x:numeric, \
                       y:numeric, resources:logical )
local txid, res2
begin
  if(resources) then
    res2 = get_resources(resources)
  else
    res2 = True
  end if
  res2@gsnDraw = True               ; False by default
  txid = gsn_create_text_ndc(wks,text,x,y,res2)
end

;***********************************************************************;
; Procedure : gsn_text                                                  ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                  text: array of text strings                          ;
;                     x: n-dimensional array of x data positions        ;
;                     y: n-dimensional array of y data positions        ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws text strings on the workstation "wks" (the       ;
; variable returned from a previous call to "gsn_open_wks").  "x" and   ;
; "y" are the x and y locations of each text string, and should be      ;
; specified in the same data space as the data space of "plotid".       ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;
undef("gsn_text")
procedure gsn_text(wks:graphic,plotid:graphic,texto:string,xo:numeric, \
                   yo:numeric, resources:logical )
local i, txid, x2, y2, xf, yf, x, y, res2, calldraw, callframe, maxbb, \
wksname, xsizes, ysizes, tsizes, xrank, yrank, trank, text, len, txres
begin
;
; Any one of xo, yo, and texto can just be one element, but if two or more
; are more than one element, then they must be exactly the same size.
;
  xsizes = dimsizes(xo)
  ysizes = dimsizes(yo)
  tsizes = dimsizes(texto)
  xrank  = dimsizes(xsizes)
  yrank  = dimsizes(ysizes)
  trank  = dimsizes(tsizes)
  if(xrank.gt.1.and.yrank.gt.1.and..not.all(xsizes.eq.ysizes)) then
    print("Error: gsn_text: x and y must have the same dimension sizes, or either be a single value.")
    return
  end if
  if(trank.gt.1.and. \
     (xrank.gt.1.and..not.all(xsizes.eq.tsizes)) .or. \
     (yrank.gt.1.and..not.all(ysizes.eq.tsizes))) then
    print("Error: gsn_text: text must be a single string or the same dimension size as x and/or y.")
    return
  end if
;
; Convert to 1-dimensional arrays of all the same length.
;
  if(xrank.gt.1) then
    x    = ndtooned(new(xsizes, typeof(xo)))
    y    = ndtooned(new(xsizes, typeof(yo)))
    text = ndtooned(new(xsizes, typeof(texto)))
  else
    if(yrank.gt.1) then
      x    = ndtooned(new(ysizes, typeof(xo)))
      y    = ndtooned(new(ysizes, typeof(yo)))
      text = ndtooned(new(ysizes, typeof(texto)))
    else
      x    = new(xsizes > ysizes, typeof(xo))
      y    = new(xsizes > ysizes, typeof(yo))
      text = new(xsizes > ysizes, typeof(texto))
    end if
  end if

  x    = ndtooned(xo)
  y    = ndtooned(yo)
  text = ndtooned(texto)
  len  = dimsizes(x)

  res2 = get_resources(resources)

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

  txid  = new(len,graphic)
  txres = get_res_eq(res2,"tx")  ; Get text resources.

  if(res2.and.isatt(res2,"txFuncCode")) then
    do i=0,len-1
      txid = create wksname + "_text"+i textItemClass wks
        "txString"   : text(i)
        "txPosXF"    : x2(i)
        "txPosYF"    : y2(i)
        "txFuncCode" : res2@txFuncCode
      end create
      attsetvalues_check(txid(i),txres)            ; Set text resources.
      draw_and_frame(wks,txid(i),calldraw,callframe,0,maxbb)
    end do
  else 
    do i=0,len-1
      txid(i) = create wksname + "_text"+i textItemClass wks
        "txString"   : text(i)
        "txPosXF"    : x2(i)
        "txPosYF"    : y2(i)
      end create
      attsetvalues_check(txid(i),txres)            ; Set text resources.
      draw_and_frame(wks,txid(i),calldraw,callframe,0,maxbb)
    end do
  end if
end

;***********************************************************************;
; Function : gsn_add_text                                               ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                  text: array of text strings                          ;
;                     x: n-dimensional array of x data positions        ;
;                     y: n-dimensional array of y data positions        ;
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
undef("gsn_add_text")
function gsn_add_text(wks:graphic,plotid:graphic,texto:string, \
                      xo:numeric,yo:numeric, resources:logical )
local txid, txres, amres, just, res2, wksname, am_ids
begin
;
; Any one of xo, yo, and texto can just be one element, but if two or more
; are more than one element, then they must be exactly the same size.
;
  xsizes = dimsizes(xo)
  ysizes = dimsizes(yo)
  tsizes = dimsizes(texto)
  xrank  = dimsizes(xsizes)
  yrank  = dimsizes(ysizes)
  trank  = dimsizes(tsizes)
  if(xrank.gt.1.and.yrank.gt.1.and..not.all(xsizes.eq.ysizes)) then
    print("Error: gsn_add_text: x and y must have the same dimension sizes, or either be a single value.")
    dummy = new(1,graphic)
    return(dummy)
  end if
  if(trank.gt.1.and. \
     (xrank.gt.1.and..not.all(xsizes.eq.tsizes)) .or. \
     (yrank.gt.1.and..not.all(ysizes.eq.tsizes))) then
    print("Error: gsn_add_text: text must be a single string or the same dimension size as x and/or y.")
    dummy = new(1,graphic)
    return(dummy)
  end if
;
; Convert to 1-dimensional arrays of all the same length.
;
  if(xrank.gt.1) then
    x    = ndtooned(new(xsizes, typeof(xo)))
    y    = ndtooned(new(xsizes, typeof(yo)))
    text = ndtooned(new(xsizes, typeof(texto)))
  else
    if(yrank.gt.1) then
      x    = ndtooned(new(ysizes, typeof(xo)))
      y    = ndtooned(new(ysizes, typeof(yo)))
      text = ndtooned(new(ysizes, typeof(texto)))
    else
      x    = new(xsizes > ysizes, typeof(xo))
      y    = new(xsizes > ysizes, typeof(yo))
      text = new(xsizes > ysizes, typeof(texto))
    end if
  end if

  x    = ndtooned(xo)
  y    = ndtooned(yo)
  text = ndtooned(texto)
  len  = dimsizes(x)

  res2 = get_resources(resources)
;
; The "txFuncCode" can't be set during a setvalues call. It must be
; set during the creation of the object.  
;
  wksname = get_res_value_keep(wks,"name","gsnapp")

  txres = get_res_eq(res2,"tx")  ; Get text resources.
  txid  = new(len,graphic)

  if(res2.and.isatt(res2,"txFuncCode")) then
    do i=0,len-1
      txid(i) = create wksname + "_text"+i textItemClass wks
        "txString"   : text(i)
        "txFuncCode" : res2@txFuncCode
      end create
      attsetvalues_check(txid(i),txres)          ; Set text resources.
    end do
  else
    do i=0,len-1
      txid(i) = create wksname + "_text"+i textItemClass wks
        "txString" : text(i)
      end create
      attsetvalues_check(txid(i),txres)          ; Set text resources.
    end do
  end if
;
; Get current list of annotations that are already attached to
; the plot.
;
  getvalues plotid
    "pmAnnoViews" : text_ids
  end getvalues
;
; Make sure the next text strings are first in the list.
;
  if(.not.any(ismissing(text_ids)))
    new_text_ids            = new(dimsizes(text_ids)+len,graphic)
    new_text_ids(0:len-1)  = txid
    new_text_ids(len:)     = text_ids
  else
    new_text_ids = txid
  end if
;
; Set the old and new annotations, with the new ones being first.
;
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
  just      = get_res_value(res2,"amJust",tmp_just)

  do i=0,len-1
    setvalues am_ids(i)
      "amDataXF"       : x(i)
      "amDataYF"       : y(i)
      "amResizeNotify" : True
      "amTrackData"    : True
      "amJust"         : just
    end setvalues
  end do

  amres = get_res_eq(res2,"am")           ; Get annomanager resources.
  attsetvalues_check(am_ids(0),amres)     ; Set annomanager resources.

  if(xrank.gt.1) then
    return(onedtond(am_ids(0:len-1),xsizes))
  else
    return(onedtond(am_ids(0:len-1),ysizes))
  end if
end

;***********************************************************************;
; Procedure : draw_bb                                                   ;
;               plot:graphic                                            ;
;               opts:logical                                            ;
;                                                                       ;
; This procedure draws a box around the bounding box of the given plot  ;
; objects.                                                              ; 
;***********************************************************************;
undef("draw_bb")
procedure draw_bb(plot:graphic,opts:logical)
local wks, bb, top, bot, lft, rgt, gsres, drawit, frameit
begin
  drawit  = isatt(opts,"gsnDraw").and.opts@gsnDraw
  frameit = isatt(opts,"gsnFrame").and.opts@gsnFrame

  wks = NhlGetParentWorkstation(plot(0))
  dimplot = dimsizes(plot)

  if(dimplot.eq.1) then
;
; Force bb to be 2-dimensional so we don't have to have a
; bunch of "if" tests later.
;
    bb = new((/1,4/),float)
    bb(0,:) = NhlGetBB(plot)
  else
    bb = NhlGetBB(plot)
  end if

  gsres = True
;  gsres@gsLineThicknessF = 5.0
  gsres@gsLineColor      = "red"
  do i=0,dimplot-1
    top = bb(i,0)
    bot = bb(i,1)
    lft = bb(i,2)
    rgt = bb(i,3)
  
    if(drawit) then
       draw(plot(i))
    end if
    gsn_polyline_ndc(wks,(/lft,rgt,rgt,lft,lft/), \
                         (/bot,bot,top,top,bot/),gsres)
    if(frameit) 
      frame(wks)
    end if
  end do
end

;***********************************************************************;
; This procedure draws or attaches markers or lines on the given plot
; at coordinates associated with the input data array. For the markers,
; you can have the markers colored according to where the data values
; are missing.  The arguments are:
; 
;    wks[1]:graphic 
;    plot[1]:graphic
;    data:numeric
;    res[1]:logical
;
; This procedure examines "data" to see if it has 1D coordinate 
; arrays, or the special "lat2d"/"lon2d" attributes. If it has
; neither, then the special "gsnCoordsX" and "gsnCoordsY" attributes
; must be attached to "res", indicating the coordinates. 
; If you are drawing lines, then these two attributes must be 2D
; and the same size as each other. If you are drawing markers,
; then these arrays don't have to be 2D, but they must be the
; same size. They will get converted to 1D arrays.
;
; By default, this procedure draws the plot, and the markers or 
; lines are drawn on the plot with gsn_polymarker[line], and
; the frame is advanced. Nothing is attached to the plot. This
; method is faster.
;
; If gsnCoordsAttach is set to True, then the markers or lines
; are attached using gsn_add_polymarker[line], and the plot is 
; not drawn and the frame is not advanced, unless gsnDraw[Frame]
; are set to True.
;
; If res@gsnCoordsX and res@gsnCoordsY are set, these will be used for the X
; and Y coordinate points.  Otherwise, this function will try to get
; the 1D coordinate information from the input data variable, or look
; for the special "lat2d", "lon2d" attributes.
; 
; Note that this function requires the X and Y coordinates to be set the
; same way. You can't set one via, say, an attribute, and the other via
; a coordinate array.
;
; Special coordinates recognized by this routine:
;
;    "gsnCoordsX" and "gsnCoordsY" - The X and Y coordinates to plot.
;       You only need to set these if the input data array 
;       doesn't have 1D coordinate arrays attached, or doesn't
;       have the special "lat2d", "lon2d" attributes.
;
;    "gsnCoordsAttach" - if True, then the markers will be
;       attached via gsn_add_polymarker, instead of just 
;       drawn with gsn_polymarker. Note that gsnDraw and Frame will
;       both be set to False in this case.
;
;    "gsnCoordsAsLines" - [not implemented yet] if True, 
;      coordinates will be drawn as lines rather than markers.
;      This only works for 2D coordinate arrays.
;
;    "gsnCoordsMissingColor"
;    "gsnCoordsNonMissingColor" - If either one of these are set,
;       then at locations where the data is or isn't missing, they
;       will be drawn in the given color
;
;***********************************************************************;
undef("gsn_coordinates")
procedure gsn_coordinates(wks[1]:graphic,plot[1]:graphic, \
                               data:numeric,res[1]:logical)
local type_xcoord, type_ycoord, pres, res2, pres, dims, rank, \
  dims_xcoord, dims_ycoord, xcoord_1d, ycoord_1d, xcoord_2d, ycoord_2d, \
  xdim, ydim, calldraw, callframe, maxbb, defclr, nonmsgclr, msgclr, \
  draw_lines, draw_non_msg, attach_coords, is_msgclr_trans, 
  is_nonmsgclr_trans, is_defclr_trans, tmpstr, i, imsg
begin
  res2 = res                     ; Make copy so we can modify it

;---Check for special resources
  maxbb         = get_bb_res(res2)
  defclr        = get_res_value(res2,"gsMarkerColor",1)
  defmrk        = get_res_value(res2,"gsMarkerIndex",16)
  msgclr        = get_res_value(res2,"gsnCoordsMissingColor",defclr)
  msgidx        = get_res_value(res2,"gsnCoordsMissingIndex",defmrk)
  nonmsgclr     = get_res_value(res2,"gsnCoordsNonMissingColor",defclr)
  nonmsgidx     = get_res_value(res2,"gsnCoordsNonMissingIndex",defmrk)
  attach_coords = get_res_value(res2,"gsnCoordsAttach",False)
  draw_lines    = get_res_value(res2,"gsnCoordsAsLines",False)
  draw_msg      = .not.is_color_transparent(msgclr)        ; this is for markers only
  draw_nonmsg   = .not.is_color_transparent(nonmsgclr)     ; ditto

  if(attach_coords) then
    calldraw  = get_res_value(res2,"gsnDraw", False)
    callframe = get_res_value(res2,"gsnFrame",False)
  else
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
  end if
;
; Figure out what kind of X and Y coordinates we have.
; Valid kinds include:
;    gsnCoordsLon or gsnCoordsLat attributes attached to res ("res_coord").
;    gsnCoordsX or gsnCoordsY attributes attached to res ("res_coord").
;    1D coord array attached to data ("data_coord").
;    2D lat2d/lon2d attributes attached to data ("data_att")
; 
  type_xcoord = ""
  type_ycoord = ""
  dims        = dimsizes(data)
  rank       = dimsizes(dims)

;----------------------------------------------------------------------
; This section gets the X,Y coordinate arrays and converts them
; to 1D for markers, and 2D for lines.
;----------------------------------------------------------------------

  if(res2.and.isatt(res2,"gsnCoordsLat").and.isatt(res2,"gsnCoordsLon")) then
    type_xcoord = "res_coord"
    type_ycoord = "res_coord"
    if(draw_lines) then
      xcoord_2d = res2@gsnCoordsLon
      ycoord_2d = res2@gsnCoordsLat
    else
      xcoord_1d = ndtooned(res2@gsnCoordsLon)
      ycoord_1d = ndtooned(res2@gsnCoordsLat)
    end if
    delete(res2@gsnCoordsLat)
    delete(res2@gsnCoordsLon)
  end if

  if(res2.and.type_xcoord.eq."".and.type_ycoord.eq."".and.\
     isatt(res2,"gsnCoordsX").and.isatt(res2,"gsnCoordsY")) then
    type_xcoord = "res_coord"
    type_ycoord = "res_coord"
    if(draw_lines) then
      xcoord_2d = res2@gsnCoordsX
      ycoord_2d = res2@gsnCoordsY
    else
      xcoord_1d = ndtooned(res2@gsnCoordsX)
      ycoord_1d = ndtooned(res2@gsnCoordsY)
    end if
    delete(res2@gsnCoordsX)
    delete(res2@gsnCoordsY)
  end if

;---Check if "data" contains 1D coordinate arrays.
  xdim = rank-1
  ydim = rank-2
  if((type_xcoord.eq."".and.(isdimnamed(data,xdim).and. \
                             iscoord(data,data!xdim))).and.\
     (type_ycoord.eq."".and.(isdimnamed(data,ydim).and. \
                             iscoord(data,data!ydim)))) then
    type_xcoord = "data_coord"
    type_ycoord = "data_coord"
    dd          = (/dims(ydim),dims(xdim)/)
    if(draw_lines) then
      xcoord_2d = conform_dims(dd,data&$data!xdim$,1)
      ycoord_2d = conform_dims(dd,data&$data!ydim$,0)
    else
      xcoord_1d = ndtooned(conform_dims(dd,data&$data!xdim$,1))
      ycoord_1d = ndtooned(conform_dims(dd,data&$data!ydim$,0))
    end if
  end if

;---Check if "data" contains "lat2d", "lon2d" attributes
  if(type_xcoord.eq."".and.isatt(data,"lon2d")) then
    type_xcoord = "data_att"
    if(draw_lines) then
      xcoord_2d = data@lon2d
    else
      xcoord_1d = ndtooned(data@lon2d)
    end if
  end if
  if(type_ycoord.eq."".and.isatt(data,"lat2d")) then
    type_ycoord = "data_att"
    if(draw_lines) then
      ycoord_2d = data@lat2d
    else
      ycoord_1d = ndtooned(data@lat2d)
    end if
  end if

;---Check if "data" contains "lat1d", "lon1d" attributes
  if(type_xcoord.eq."".and.isatt(data,"lon1d")) then
    if(draw_lines) then
      print("gsn_coordinates: cannot draw an unstructured grid using lines")
      return
    end if
    type_xcoord = "data_att"
    xcoord_1d   = data@lon1d
  end if
  if(type_ycoord.eq."".and.isatt(data,"lat1d")) then
    if(draw_lines) then
      print("gsn_coordinates: cannot draw an unstructured grid using lines")
      return
    end if
    type_ycoord = "data_att"
    ycoord_1d   = data@lat1d
  end if

;---Error checking
  if(type_xcoord.eq."".or.type_ycoord.eq."") then
    print("gsn_coordinates: no valid X and/or Y coordinate values provided.")
    return
  end if
  if(type_xcoord.ne.type_ycoord) then
    print("gsn_coordinates: conflicting X/Y coordinate values provided.")
    return
  end if

  if(draw_lines) then
    dims_xcoord = dimsizes(xcoord_2d)
    dims_ycoord = dimsizes(ycoord_2d)
    rank_xcoord = dimsizes(dims_xcoord)
    rank_ycoord = dimsizes(dims_ycoord)
    if(rank_xcoord.ne.2.or.rank_xcoord.ne.rank_ycoord) then
      print("gsn_coordinates: can't draw coordinates as lines if they")
      print("                      are not 2D arrays of the same size.")
      return
    end if
    ny = dims_xcoord(0)
    nx = dims_xcoord(1)
    if(dims_ycoord(0).ne.ny.and.dims_ycoord(1).ne.nx) then
      print("gsn_coordinates: coordinates must have the same")
      print("                      dimensionality")
      return
    end if
  else
    dims_xcoord = dimsizes(xcoord_1d)
    dims_ycoord = dimsizes(ycoord_1d)
    if(dims_xcoord.ne.dims_ycoord) then
      print("gsn_coordinates: coordinates must have the same")
      print("                      dimensionality")
      return
    end if
  end if

;---Copy over resources if True
  if(res2) then
    pres = res2
  else
    pres = True
  end if

  if(.not.attach_coords.and.calldraw) then
    draw_and_frame(wks,plot,calldraw,False,False,maxbb)
  end if

;---Draw coordinates using lines.
  if(draw_lines) then
    if(attach_coords) then
      do i=0,ny-1
        tmpstr = unique_string("xlines")
        plot@$tmpstr$ = gsn_add_polyline(wks,plot,xcoord_2d(i,:),\
                                                  ycoord_2d(i,:),pres)
      end do
      do i=0,nx-1
        tmpstr = unique_string("ylines")
        plot@$tmpstr$ = gsn_add_polyline(wks,plot,xcoord_2d(:,i),\
                                                  ycoord_2d(:,i),pres)
      end do
    else
      do i=0,ny-1
        gsn_polyline(wks,plot,xcoord_2d(i,:),\
                              ycoord_2d(i,:),pres)
      end do
      do i=0,nx-1
        gsn_polyline(wks,plot,xcoord_2d(:,i),\
                              ycoord_2d(:,i),pres)
      end do
    end if
  else
;---Draw coordinates using markers.
    if(draw_nonmsg) then
      pres@gsMarkerColor := nonmsgclr
      pres@gsMarkerIndex := nonmsgidx
      ii = ind(.not.ismissing(ndtooned(data)))
      if(.not.all(ismissing(ii))) then
        if(attach_coords) then
          tmpstr = unique_string("markers_nonmsg")
          plot@$tmpstr$ = gsn_add_polymarker(wks,plot,xcoord_1d(ii),\
                                             ycoord_1d(ii),pres)
        else
          gsn_polymarker(wks,plot,xcoord_1d(ii),ycoord_1d(ii),pres)
        end if
      end if
    end if ; draw_nonmsg
    if(draw_msg) then
      pres@gsMarkerColor := msgclr
      pres@gsMarkerIndex := msgidx
      ii := ind(ismissing(ndtooned(data)))
      if(.not.all(ismissing(ii))) then
        if(attach_coords) then
          tmpstr = unique_string("markers_msg")
          plot@$tmpstr$ = gsn_add_polymarker(wks,plot,xcoord_1d(ii),\
                                             ycoord_1d(ii),pres)
        else
          gsn_polymarker(wks,plot,xcoord_1d(ii),ycoord_1d(ii),pres)
        end if
      end if
    end if ; draw_msg
  end if   ; draw_lines

  if(attach_coords) then
    draw_and_frame(wks,plot,calldraw,callframe,False,maxbb)
  else
    if(callframe) then
      frame(wks)
    end if
  end if
end

;----------------------------------------------------------------------
; Retrieve resource needed to determine the end style for a 
; labelbar. This resource is only available for contour plots, so
; we have to default it to "IncludeOuterBoxes" for other plot types.
;----------------------------------------------------------------------
undef("get_lbar_end_style")
function get_lbar_end_style(plot[1]:graphic)
begin
  if(plot@plot_type.eq."contour") then
    getvalues plot
      "cnLabelBarEndStyle" : end_style
    end getvalues
  else
    end_style = get_enum_value("IncludeOuterBoxes")
  end if
  return(end_style)
end

;----------------------------------------------------------------------
; Retrieve resource values from a labelbar object so we can construct
; a new labelbar object for a panel plot.
;----------------------------------------------------------------------
undef("get_panel_labelbar_info_from_lbar")
function get_panel_labelbar_info_from_lbar(plot[1]:graphic,\
                                           lbar_object[1]:graphic)
local subset_stuff
begin
  panel_labelbar = True
  getvalues lbar_object
    "lbMonoFillColor" : mono_fill_color
    "lbMonoFillPattern" : mono_fill_pattern
    "lbFillPatterns" : fill_patterns
    "lbMonoFillScale" : mono_fill_scale
    "lbFillScales" : fill_scales
    "lbBoxEndCapStyle" : cap_style
    "lbLabelStrings" : label_strings
    "lbLabelAlignment"  : label_alignment
    "lbFillColors" : fill_colors
  end getvalues
  end_style = get_lbar_end_style(plot)
  
  subset_stuff = False
  return([/panel_labelbar,label_strings,fill_colors,fill_patterns,\
           fill_scales,mono_fill_color,mono_fill_pattern,mono_fill_scale,\
           end_style,cap_style,label_alignment,subset_stuff/])
end

;----------------------------------------------------------------------
; Retrieve resource values from a plot object so we can construct
; a new labelbar object for a panel plot.  Unfortunately, we can't
; allow a cnLabelBarEndStyle of IncludeMinMaxLabels, because we
; don't have an easy way to reconstruct the min/max labels from the
; plot object.
;----------------------------------------------------------------------
undef("get_panel_labelbar_info_from_plot")
function get_panel_labelbar_info_from_plot(plot[1]:graphic)
local end_style,cap_style,fill_on,colors,fill_on,fill_patterns,\
fill_scales,mono_fill_pat,mono_fill_scl,mono_fill_col,levels,gstyle,\
fill_arrows_on,mono_line_color,mono_fill_arrow,mono_wind_barb,colors,\
levels,tmp_levels,nlevels,dmin,dmax, data_id, subset_stuff
begin

;---Set these defaults just in case
  end_style   = get_enum_value("IncludeOuterBoxes")
  cap_style   = get_enum_value("RectangleEnds")
  label_align = get_enum_value("InteriorEdges")

  panel_labelbar = True
  if(plot@plot_type.eq."contour") then
    getvalues plot
      "cnFillOn" : fill_on
    end getvalues
    if(fill_on) then
      getvalues plot
        "cnFillColors"       : colors
        "cnFillPatterns"     : fill_patterns
        "cnFillScales"       : fill_scales
        "cnMonoFillPattern"  : mono_fill_pat
        "cnMonoFillScale"    : mono_fill_scl
        "cnMonoFillColor"    : mono_fill_col
        "cnLevels"           : tmp_levels
        "cnLabelBarEndStyle" : end_style
        "lbBoxEndCapStyle"   : cap_style
        "lbLabelAlignment"   : label_align
        "lbLabelStrings"     : label_strings
      end getvalues
      if(end_style.eq.get_enum_value("IncludeMinMaxLabels")) then
        getvalues plot
          "cnScalarFieldData" : data_id    
        end getvalues
        getvalues data_id
          "sfDataMinV" : dmin
          "sfDataMaxV" : dmax
        end getvalues
        nlevels = dimsizes(tmp_levels)
        levels = new(nlevels+2,typeof(tmp_levels))
        levels(0)         = dmin  
        levels(1:nlevels) = tmp_levels
        levels(nlevels+1) = dmax
      else
        levels = tmp_levels
      end if
    else
      panel_labelbar = False
    end if
  end if
  if(plot@plot_type.eq."vector") then
    getvalues plot
      "vcGlyphStyle"             : gstyle
      "vcFillArrowsOn"           : fill_arrows_on
      "vcMonoLineArrowColor"     : mono_line_color
      "vcMonoFillArrowFillColor" : mono_fill_arrow
      "vcMonoWindBarbColor"      : mono_wind_barb
    end getvalues
    if(    (fill_arrows_on .and. .not.mono_fill_arrow) .or. \
      (.not.fill_arrows_on .and. .not.mono_line_color) .or. \
      (gstyle.eq.get_enum_value("FillArrow") .and. .not.mono_fill_arrow) .or. \
      (gstyle.eq.get_enum_value("WindBarb") .and. .not.mono_wind_barb) .or. \
      (gstyle.eq.get_enum_value("LineArrow") .or. get_enum_value("CurlyVector")) .and. \
        .not.mono_line_color) then
;---There are no fill patterns in VectorPlot, only solids.
      mono_fill_pat = True
      mono_fill_scl = True
      mono_fill_col = False
      fill_scales = -1
      getvalues plot
        "vcLevels"         : levels
        "vcLevelColors"    : colors
        "lbBoxEndCapStyle" : cap_style
        "lbLabelAlignment" : label_align
        "lbLabelStrings"   : label_strings
      end getvalues
    else
      panel_labelbar = False
    end if
  else if(plot@plot_type.eq."streamline") then
    getvalues plot
      "stMonoLineColor"  : mono_line_color
    end getvalues
    if(.not.mono_line_color) then
;---There are no fill patterns in StreamlinePlot, only solids.
      mono_fill_pat = True
      mono_fill_scl = True
      mono_fill_col = False
      getvalues plot
        "stLevels"         : levels
        "stLevelColors"    : colors
        "lbBoxEndCapStyle" : cap_style
        "lbLabelAlignment" : label_align
        "lbLabelStrings"   : label_strings
      end getvalues
    else
      panel_labelbar = False
    end if
  else
  end if
  end if
  if(.not.isvar("fill_scales")) then
    fill_scales = -1
  end if
  if(.not.isvar("fill_patterns")) then
    fill_patterns = -1
  end if

;
; There's no lb resource equivalent for removing the end boxes 
; from a contour label bar (cnLabelBarEndStyle = ExcludeOuterBoxes),
; so we have to set a flag that will be used by the create_labelbar
; routine, telling it to remove the first and last colors.
;
  subset_stuff = True
  return([/panel_labelbar,label_strings,colors,fill_patterns,fill_scales,\
           mono_fill_col,mono_fill_pat,mono_fill_scl,end_style,cap_style,\
           label_align,subset_stuff/])
end

;----------------------------------------------------------------------
; Determine if we have to retrieve the labelbar information from
; a plot, or from a labelbar added as an annotation, and then call
; the appropriate function to get the necessary information to 
; reconstruct the labelbar from scratch.
;----------------------------------------------------------------------
undef("get_panel_labelbar_info")
function get_panel_labelbar_info(plot[1]:graphic)
local pm_annos, class_name, ilab
begin
  getvalues plot
    "pmAnnoViews" : pm_annos
  end getvalues
  class_names = NhlClassName(pm_annos)
  ilab = ind(class_names.eq."labelBarClass")
  if(.not.all(ismissing(ilab))) then
    lbar_list = get_panel_labelbar_info_from_lbar(plot,pm_annos(ilab))
  else
    lbar_list = get_panel_labelbar_info_from_plot(plot)
  end if
  return(lbar_list)
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
; Added in V6.4.0 to replace txStringXXX:                               ;
;    gsnPanelMainString                                                 ;
;    gsnPanelMainStringFontHeightF                                      ;
;    gsnPanelMainPosYF / gsnPanelMainPosXF                              ;
;    Other gsnPanelMainXXXX resources                                   ;  
;                                                                       ;
;***********************************************************************;
undef("gsn_panel_return")
function gsn_panel_return(wks:graphic,plot[*]:graphic,dims[*]:integer,\
                          resources:logical)
local res, nrows, ncols, ddims, is_row_spec, row_spec, npanels, nplots, \
      perim_on
begin
  res = get_resources(resources)           ; Make copy of resources
;
; First check if paneling is to be specified by (#rows x #columns) or
; by #columns per row.  The default is rows x columns, unless 
; resource gsnPanelRowSpec is set to True, in which case "dims"
; will represent number of plots per row.
; 
  is_row_spec = get_res_value(res,"gsnPanelRowSpec",False)
;
; Check to see if we have enough plots to fit panels, and vice versa.
;
  ddims = dimsizes(dims)
  if(is_row_spec)
    row_spec = dims
    npanels = 0
    nrows = ddims
    ncols = max(row_spec)
    do i=0,nrows-1
      if(row_spec(i).lt.0)
        print("gsn_panel: Error: you have specified a negative value for the number of plots in a row.")
        exit
      end if
      npanels = npanels + row_spec(i)
    end do
  else
    if(ddims.ne.2)
      print("gsn_panel: Error: for the third argument of gsn_panel, you must either specify # rows by # columns or set gsnPanelRowSpec to True and set the number of plots per row.")
      exit
    end if
    nrows    = dims(0)
    ncols    = dims(1)
    npanels  = nrows * ncols
    row_spec = new(nrows,integer)
    row_spec = ncols
  end if
 
  nplots  = dimsizes(plot)      ; Total number of plots.

  if(nplots.gt.npanels)
    print("gsn_panel: warning: you have more plots than you have panels.")
    print("Only " + npanels + " plots will be drawn.")
    nplots = npanels
  end if

;
; Check for special resources.
; 
  panel_save     = get_res_value_keep(res,"gsnPanelSave",True)
  panel_debug    = get_res_value_keep(res,"gsnPanelDebug",False)
  panel_center   = get_res_value_keep(res,"gsnPanelCenter",True)
  panel_labelbar = get_res_value_keep(res,"gsnPanelLabelBar",False)
  panel_plotid   = get_res_value_keep(res,"gsnPanelScalePlotIndex",-1)
  calldraw       = get_res_value_keep(res,"gsnDraw",True)
  callframe      = get_res_value_keep(res,"gsnFrame",True)
  xwsp_perc      = get_res_value_keep(res,"gsnPanelXWhiteSpacePercent",1.)
  ywsp_perc      = get_res_value_keep(res,"gsnPanelYWhiteSpacePercent",1.)
  draw_boxes     = get_res_value_keep(res,"gsnPanelBoxes",False)
  x_lft          = get_res_value_keep(res,"gsnPanelLeft",0.)
  x_rgt          = get_res_value_keep(res,"gsnPanelRight",1.)
  y_bot          = get_res_value_keep(res,"gsnPanelBottom",0.)
  y_top          = get_res_value_keep(res,"gsnPanelTop",1.)
  main_string    = get_res_value_keep(res,(/"gsnPanelMainString","txString"/),"")
  maxbb          = get_bb_res(res)

  lft_pnl        = isatt(res,"gsnPanelLeft")
  rgt_pnl        = isatt(res,"gsnPanelRight")
  bot_pnl        = isatt(res,"gsnPanelBottom")
  top_pnl        = isatt(res,"gsnPanelTop")
;
; Check if a main string has been specified. If so, we need to make sure
; we leave some room for it by computing y_top (if the user hasn't set
; it). Also, we have to check if the font height has been set, because
; this could affect the title position.
; 
  if(main_string.ne."") then
    main_string_on = True
    main_font_hgt  = get_res_value_keep(res,(/"gsnPanelMainFontHeightF","txFontHeightF"/),0.02)
;
; By default, we want a distance of 0.01 between top of title and the
; frame, and a distance of 0.03  between the bottom of the title (txPosYF)
; and the top of the panel box (gsnPanelTop).
;
    if(y_top.eq.1.) then
      if(isatt(res,"gsnPanelMainPosYF"))
        y_top = min((/1.,res@gsnPanelMainPosYF - 0.03/))
      else if(isatt(res,"txPosYF"))
        y_top = min((/1.,res@txPosYF - 0.03/))
      else
        y_top = min((/1.,0.96-main_font_hgt/))
      end if
      end if
    end if
  else
    main_string_on = False
  end if
;
; Calculate number of plot objects that will actually be drawn.
; This include the panel plots, the labelbar (if any), and 
; main string (if any).
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
; a PostScript or PDF workstation, because the bounding box is already
; maximized for an NCGM/X11 window.
; 
  if(maxbb) then
    class = NhlClassName(wks)
    if(.not.any(class(0).eq.(/"psWorkstationClass", "pdfWorkstationClass", \
                              "documentWorkstationClass"/)))
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
    justs = (/"bottomright", "topright", "topleft", "bottomleft"/)
    paras = (/     1.0,           1.0,      -1.0,       -1.0/)
    orths = (/     1.0,          -1.0,      -1.0,        1.0/)

    amres = get_res_eq(res,"am")
    just = str_lower(get_res_value(res,(/"gsnPanelFigureStringsJust","amJust"/),"bottomright"))
;
; Look for all resources that start with gsnPanelFigureStrings, and replace
; this with just "tx". This is what allows us to sneak in text resources
; and have them only apply to the figure strings, and not the main string.
;
    txres    = get_res_eq_replace(res,"gsnPanelFigureStrings","tx")
    perim_on = get_res_value(txres,"txPerimOn",True)
    bkgrn    = get_res_value(txres,"txBackgroundFillColor",0)
  else
    is_figure_strings = False
  end if
;
; Error check the values that the user has entered, to make sure
; they are valid.
;
  if(xwsp_perc.lt.0.or.xwsp_perc.ge.100.)
    print("gsn_panel: Warning: attribute gsnPanelXWhiteSpacePercent must be >= 0 and < 100.")
    print("Defaulting to 1.")
    xwsp_perc = 1.
  end if

  if(ywsp_perc.lt.0.or.ywsp_perc.ge.100.)
    print("gsn_panel: Warning: attribute gsnPanelYWhiteSpacePercent must be >= 0 and < 100.")
    print("Defaulting to 1.")
    ywsp_perc = 1.
  end if

  if(x_lft.lt.0..or.x_lft.ge.1.)
    print("gsn_panel: Warning: attribute gsnPanelLeft must be >= 0.0 and < 1.0")
    print("Defaulting to 0.")
    x_lft = 0.0
  end if

  if(x_rgt.le.0..or.x_rgt.gt.1.)
    print("gsn_panel: Warning: attribute gsnPanelRight must be > 0.0 and <= 1.0")
    print("Defaulting to 1.")
    x_rgt = 1.0
  end if

  if(y_top.le.0..or.y_top.gt.1.)
    print("gsn_panel: Warning: attribute gsnPanelTop must be > 0.0 and <= 1.0")
    print("Defaulting to 1.")
    y_top = 1.0
  end if

  if(y_bot.lt.0..or.y_bot.ge.1.)
    print("gsn_panel: Warning: attribute gsnPanelBottom must be >= 0.0 and < 1.0")
    print("Defaulting to 0.")
    y_bot = 0.0
  end if

  if(x_rgt.le.x_lft)
    print("gsn_panel: Error: attribute gsnPanelRight ("+x_rgt+") must be greater")
    print("than gsnPanelLeft ("+x_lft+").")
    exit
  end if

  if(y_top.le.y_bot)
    print("gsn_panel: Error: attribute gsnPanelTop ("+y_top+") must be greater")
    print("than gsnPanelBottom ("+y_bot+").")
    exit
  end if

;
; Get the plot object id that we will use to determine the scale factor
; for resizing all of the plots. If gsnPanelScalePlotId was set
; (panel_id), then use this. Otherwise, we will start at the first
; plot and keep going until we find a "valid" (non missing) plot.
;
; We assume all plots are the same size, so if we get the size of
; one of them, then this should represent the size of the rest
; of them. Also, count the number of non-missing plots for later.
;
  ind_nomsg = ind(.not.ismissing(plot(0:nplots-1)))
  if(all(ismissing(ind_nomsg))) then
    print("gsn_panel: Error: all of the plots passed to gsn_panel appear to be invalid")
    exit
  end if
  if(panel_plotid.ge.0.and.panel_plotid.le.(nplots-1).and. \
     .not.ismissing(plot(panel_plotid))) then
    valid_plot = panel_plotid
  else
    valid_plot = ind_nomsg(0)
  end if
  bb     = NhlGetBB(plot(valid_plot))   ; Get bounding box of this plot
  top    = bb(0)
  bottom = bb(1)
  left   = bb(2)
  right  = bb(3)
  delete(bb)
  nvalid_plots = dimsizes(ind_nomsg)
  delete(ind_nomsg)

  if(panel_debug) then
    print("There are " + nvalid_plots + " valid plots out of " + nplots + " total plots")
  end if

;
; Get the type of plots we have. "plot" can be a map, in which case
; the contour, vector, streamine plot overlaid on it will be indicated
; by an attribute, for example, "plot@contour" or "plot@vector".
;
  new_plot     = get_plot_not_loglin(plot(valid_plot))
  new_plot_lab = get_plot_labelbar(plot(valid_plot))
;
; Get the font height of a particular plot element, so we can
; use this for panel plot elements, like the size of the 
; common labelbar labels, the main title, etc.
;
  if(is_figure_strings.or.panel_labelbar) then
    if(new_plot@plot_type.eq."contour") then
      getvalues new_plot
        "cnInfoLabelFontHeightF" : font_height
      end getvalues
    else if(new_plot@plot_type.eq."vector") then
      getvalues new_plot
        "vcRefAnnoFontHeightF" : font_height
      end getvalues
    else if(new_plot@plot_type.eq."xy") then
      getvalues new_plot
        "tiXAxisFontHeightF" : font_height
      end getvalues
      font_height = 0.6*font_height
    else if(new_plot@plot_type.eq."streamline") then
;
; There's no equivalent font height resource for streamline to 
; retrieve, so just set it here.
;
      font_height = 0.01
    else
      font_height = 0.01
    end if
    end if
    end if
    end if
;
; Use this font height for the panel figure strings, if any, unless
; the user has set gsnPanelFigureStringsFontHeightF.
;
    pfont_height = get_res_value(res,"gsnPanelFigureStringsFontHeightF",\
                                 font_height)
  end if

;---Get information needed for reconstructing a labelbar from scratch.
  if(panel_labelbar) then
    lbar_list = get_panel_labelbar_info(new_plot_lab)
    panel_labelbar = lbar_list[0]
    lb_label_strings = lbar_list[1]
    lb_fill_colors = lbar_list[2]
    lb_fill_patterns = lbar_list[3]
    lb_fill_scales = lbar_list[4]
    lb_mono_fill_color = lbar_list[5]
    lb_mono_fill_pattern = lbar_list[6]
    lb_mono_fill_scale = lbar_list[7]
    lb_end_style = lbar_list[8]
    lb_cap_style = lbar_list[9]
    lb_label_alignment = lbar_list[10]
    lb_subset_stuff = lbar_list[11]
  end if

;
; This section calculates the scale factor needed to rescale the 
; plots so they fit on one page.  It has to account for white 
; space between each plot, and space for the labelbar and main title, 
; if any.
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
; it 0.2x the height (width) of the plot.
;
  lbhor = True
  if(panel_labelbar) then
    lbres = get_res_eq(res,(/"lb","pmLabelBar","vp"/))  ; Get labelbar resources.
    if(check_attr_enum_value(lbres,"lbOrientation","vertical")) then
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

  if(lbhor) then
;
; Previously, we used to include xrange and yrange as part of the min
; statement. This seemed to cause problems if you set one of
; gsnPanelTop/Bottom/Right/Left however, so I removed it.
;
    row_scale = yrange/(nrows*total_height+labelbar_height)
    col_scale = xrange/(ncols*total_width)
    scale     = min((/col_scale,row_scale/))
    yrange    = yrange - scale * labelbar_height
  else
;
; See above comments.
;
    row_scale = yrange/(nrows*total_height)
    col_scale = xrange/(ncols*total_width+labelbar_width)
    scale     = min((/col_scale,row_scale/))
    xrange    = xrange - scale * labelbar_width
  end if

  new_plot_width  = scale*plot_width    ; Calculate new width
  new_plot_height = scale*plot_height   ; and height.

  xwsp = xwsp_perc/100. * new_plot_width   ; Calculate new white space.
  ywsp = ywsp_perc/100. * new_plot_height

  new_total_width  = 2.*xwsp + new_plot_width  ; Calculate new total width
  new_total_height = 2.*ywsp + new_plot_height ; and height w/white space.

  xsp = xrange - new_total_width*ncols  ; Calculate total amt of white space
  ysp = yrange - new_total_height*nrows ; left in both X and Y directions.

  getvalues plot(valid_plot)
    "vpXF"      : vpx
    "vpYF"      : vpy
    "vpWidthF"  : vpw
    "vpHeightF" : vph
  end getvalues

  dxl = scale * (vpx-left)           ; Distance from plot's left
                                     ; position to its leftmost annotation
  dxr = scale * (right-(vpx+vpw))    ; Distance from plot's right
                                     ; position to its rightmost annotation
  dyt = scale * (top-vpy)            ; Distance from plot's top
                                     ; position to its topmost annotation.
  dyb = scale * ((vpy-vph)-bottom)   ; Distance from plot's bottom
                                     ; position to its bottommost annotation.

  ypos = y_top - ywsp - dyt -(ysp/2.+new_total_height*ispan(0,nrows-1,1))

  delete([/top,bottom,right,left/])
;
; If we have figure strings, then determine white spacing around 
; the text box.
;
  if(is_figure_strings) then
    fig_index = ind(just.eq.justs)
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
  added_anno     = False             ; For figure strings

  do while(num_plots_left.gt.0)
    vpy_new = ypos(nr)
    new_ncols = min((/num_plots_left,row_spec(nr)/))

    if(panel_center)
      xsp = xrange - new_total_width*new_ncols  ; space before plots. 
    else
      xsp = xrange - new_total_width*ncols      ; space before plots. 
    end if
;
; Calculate new x positions.
;
    xpos = x_lft + xwsp + dxl +(xsp/2.+new_total_width*ispan(0,new_ncols-1,1))

    do nc = 0,new_ncols-1
      vpx_new = xpos(nc)
      if(.not.ismissing(plot(nplot)))
        pplot = plot(nplot)
        getvalues pplot
          "vpXF"      : old_vp(nplot,0)
          "vpYF"      : old_vp(nplot,1)
          "vpWidthF"  : old_vp(nplot,2)
          "vpHeightF" : old_vp(nplot,3)
        end getvalues
;
; If user setting gsnPanelXF or gsnPanelYF resources, then use these instead.
; They must be set as an array of the same length as you have plots.
; If any of these are negative, then use the calculated values.
;
        vpx_new = xpos(nc)
        if(isatt(res,"gsnPanelXF").and.dimsizes(res@gsnPanelXF).eq.nplots.and.\
          res@gsnPanelXF(nplot).ge.0.and.res@gsnPanelXF(nplot).le.1) then
          vpx_new = res@gsnPanelXF(nplot)
        end if

        vpy_new = ypos(nr)
        if(isatt(res,"gsnPanelYF").and.dimsizes(res@gsnPanelYF).eq.nplots.and.\
          res@gsnPanelYF(nplot).ge.0.and.res@gsnPanelYF(nplot).le.1) then
          vpy_new = res@gsnPanelYF(nplot)
        end if
;
; Print out values used.
;
        if(panel_debug) then
          print("-------Panel viewport values for each plot-------")
          print("    plot #" + nplot)
          print("    new x,y      = " + vpx_new + "," + vpy_new)
          print("    orig wdt,hgt = " + old_vp(nplot,2) + "," + old_vp(nplot,3))
          print("    new wdt,hgt  = " + scale*old_vp(nplot,2) + "," + scale*old_vp(nplot,3))
        end if

        setvalues pplot
          "vpXF"      : vpx_new
          "vpYF"      : vpy_new
          "vpWidthF"  : scale*old_vp(nplot,2)
          "vpHeightF" : scale*old_vp(nplot,3)
        end setvalues

        if(is_figure_strings) then
          if(nplot .lt. dimsizes(panel_strings).and. \
            panel_strings(nplot).ne."")
            text = create "string" textItemClass wks
              "txString"              : panel_strings(nplot)
              "txFontHeightF"         : pfont_height
              "txPerimOn"             : perim_on
              "txBackgroundFillColor" : bkgrn
            end create
;
; Set some text resources for figure strings, if any.
;
            attsetvalues_check(text,txres)
;
; Add annotation to plot.
;
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
; Info for possible labelbar or main_string
;
        if(main_string_on.or.panel_labelbar.or.draw_boxes) then
          bb  = NhlGetBB(pplot) ; Get bounding box of plot.
          top = bb(0)
          lft = bb(2)
          bot = bb(1)
          rgt = bb(3)
          max_rgt = max((/rgt,max_rgt/))
          max_top = max((/top,max_top/))

          if(draw_boxes)
            draw_bb(pplot,False)
          end if
        end if
      end if      ;   if(.not.ismissing(plot(nplot)))
;
; Retain the smallest and largest x and y positions.
;
      if(nplot.eq.0) then
        min_xpos = vpx_new
        max_xpos = vpx_new
        min_ypos = vpy_new
        max_ypos = vpy_new
      else
        min_xpos = min( (/vpx_new,min_xpos/) )
        max_xpos = max( (/vpx_new,max_xpos/) )
        min_ypos = min( (/vpy_new,min_ypos/) )
        max_ypos = max( (/vpy_new,max_ypos/) )
      end if

      nplot = nplot + 1    ; Increment plot counter
    end do  ; end of columns

    num_plots_left = nplots - nplot
    nr = nr + 1   ; increment rows
    delete(xpos)
  end do    ; end of plots

;
; Print min/max information.
;
  if(panel_debug) then
    print("-------min/max X,Y viewport positions for plots-------")
    print("min/max x viewport position = " + min_xpos + "/" + max_xpos)
    print("min/max y viewport position = " + min_ypos + "/" + max_ypos)
  end if
;
; Calculate the biggest rescaled widths and heights (technically, they
; should all be the same).  These values will be used a few times 
; throughout the rest of the code.
;
  scaled_width  = scale*max(old_vp(:,2))
  scaled_height = scale*max(old_vp(:,3))
;
; Check if a labelbar is to be drawn.
;
  if(panel_labelbar) then
    lbres@EndStyle = lb_end_style
;
; If plot type is unknown or xy, then we can't get labelbar information.
;
    if(new_plot@plot_type.ne."unknown".and.new_plot@plot_type.ne."xy") then
;
; Set labelbar height, width, and font height.
;
      labelbar_height      = scale * labelbar_height
      labelbar_width       = scale * labelbar_width
      labelbar_font_height = font_height
;
; Set some labelbar resources.  If pmLabelBarWidth/Height are set,
; use these no matter what, for the labelbar width and height. Otherwise,
; use vpWidth/Height if they are set.
;
      lbres = True
      if(isatt(lbres,"pmLabelBarWidthF")) then
        lbres@vpWidthF = get_res_value(lbres,"pmLabelBarWidthF",labelbar_width)
      else
        set_attr(lbres,"vpWidthF", labelbar_width)
      end if
        
      if(isatt(lbres,"pmLabelBarHeightF")) then
        lbres@vpHeightF = get_res_value(lbres,"pmLabelBarHeightF",labelbar_height)
      else
        set_attr(lbres,"vpHeightF",labelbar_height)
      end if
;
; Set position of labelbar depending on whether it's horizontal or
; vertical.
;
      if(lbhor)
        set_attr(lbres,"vpYF",max ((/ywsp+labelbar_height,bot-ywsp/)))
        if(ncols.eq.1.and.lbres@vpWidthF.le.scaled_width)
          set_attr(lbres,"vpXF",min_xpos + (scaled_width-lbres@vpWidthF)/2.)
        else
          tmp_range = x_rgt - x_lft
          set_attr(lbres,"vpXF",x_lft + (tmp_range - lbres@vpWidthF)/2.)
        end if
        lbres@vpYF = lbres@vpYF + get_res_value(lbres,"pmLabelBarOrthogonalPosF",0.)
        lbres@vpXF = lbres@vpXF + get_res_value(lbres,"pmLabelBarParallelPosF",0.)
      else
        set_attr(lbres,"vpXF",min ((/1.-(xwsp+labelbar_width),max_rgt+xwsp/)))
        if(nrows.eq.1.and.lbres@vpHeightF.le.scaled_height)
          set_attr(lbres,"vpYF",max_ypos-(scaled_height - lbres@vpHeightF)/2.)
        else
          tmp_range = y_top - y_bot
          set_attr(lbres,"vpYF",y_top-(tmp_range - lbres@vpHeightF)/2.)
        end if
        lbres@vpXF = lbres@vpXF + get_res_value(lbres,"pmLabelBarOrthogonalPosF",0.)
        lbres@vpYF = lbres@vpYF + get_res_value(lbres,"pmLabelBarParallelPosF",0.)
      end if
      set_attr(lbres,"lbLabelFontHeightF",labelbar_font_height)
;
; Check if we want different fill patterns or fill scales.  If so, we
; have to pass these on to the labelbar.
;
      set_attr(lbres,"lbMonoFillColor",lb_mono_fill_color)
      if(.not.lb_mono_fill_pattern)
        set_attr(lbres,"lbMonoFillPattern", False)
        set_attr(lbres,"lbFillPatterns", lb_fill_patterns)
      end if
      if(.not.lb_mono_fill_scale)
        set_attr(lbres,"lbMonoFillScale", False)
        set_attr(lbres,"lbFillScales", lb_fill_scales)
      end if
      if(isvar("lb_cap_style"))
        set_attr(lbres,"lbBoxEndCapStyle",lb_cap_style)
      end if
      set_attr(lbres,"lbLabelAlignment",lb_label_alignment)
;      if(isvar("lb_levels"))
;        lb_label_strings = fix_zero_contour(lb_levels)   ; Make sure level 0 is not something like 1e-8
;      end if
      lbres@SubsetStuff = lb_subset_stuff

;---Create the labelbar.
      newplots(nplot) = create_labelbar(wks,dimsizes(lb_fill_colors),lb_fill_colors, \
                                        lb_label_strings,lbres)
      nplot = nplot + 1
    else
       print("gsn_panel: Warning: unrecognized plot type for getting labelbar information. Ignoring labelbar request.")
    end if
  end if
;
; Create the main string, if exists.
;
  if(main_string_on) then
    y_top     = min((/y_top,max_top/))
    main_ypos = get_res_value_keep(res,(/"gsnPanelMainPosYF","txPosYF"/),y_top + 0.03)
    main_xpos = get_res_value_keep(res,(/"gsnPanelMainPosXF","txPosXF"/),0.5)
    if(panel_debug) 
      print("-------Panel title values-------")
      print("    title                = " + main_string)
      print("    top of paneled plots = " + y_top)
      print("    y location of title  = " + main_ypos)
    end if

    if((main_ypos+main_font_hgt).gt.1)
       print("gsn_panel: Warning: font height (" + main_font_hgt + ") of main string is too large to fit in space provided. Either decrease font size or set gsnPanelTop.")
    end if

;
; Look for all resources that start with gsnPanelMain, and replace
; this with just "tx". This is is for backwards compatibility in
; V6.3.0 and earlier, where "gsnPanelMain" resources weren't
; recognized, but "tx" resources were.
;
    mntxres               = get_res_eq_replace(res,"gsnPanelMain","tx")
    mntxres               = True
    mntxres@gsnDraw       = False
    mntxres@gsnFrame      = False
    mntxres@txFontHeightF = main_font_hgt
    newplots(nplot) = gsn_create_text_ndc(wks, main_string, main_xpos, \
                      main_ypos, mntxres)
    mntxres = get_res_eq(res,"tx")   ; This is a kludge to remove any tx*** resources from "res"
  end if
;
; If some of the paneled plots are missing, we need to take these into
; account so that the maximization will still work properly.  For
; example, if we ask for a 2 x 2 configuration, but plots 1 and 3 (the
; rightmost plots) are missing, then we need to set a new resource
; called gsnPanelInvsblRight to whatever approximate X value it 
; would have been if those plots weren't missing.  Setting just gsnPanelRight
; won't work in this case, because that resource is only used to control
; where the plots are drawn in a 0 to 1 square, and *not* to indicate the
; rightmost location of the rightmost graphic (which could be a vertical 
; labelbar).
;
; Not dealing with the case of gsnPanelRowSpec = True yet.
;  
  if(.not.is_row_spec) then
    newbb  = new((/dimsizes(newplots),4/),float)
;
; Have to deal with special case of only having one plot.
;
    if(dimsizes(newplots).eq.1)
      newbb(0,:)  = NhlGetBB(newplots)  ; Get bounding boxes of plots, plus
                                        ; labelbar and text string if they
                                        ; exist.  
    else
      newbb  = NhlGetBB(newplots)       ; Get bounding boxes of plots, plus
                                        ; labelbar and text string if they
                                        ; exist.  
    end if
    getvalues newplots(valid_plot)
      "vpXF"      : vpx
      "vpYF"      : vpy
      "vpWidthF"  : vpw
      "vpHeightF" : vph
    end getvalues
    dxl = vpx-newbb(valid_plot,2)
    dxr = newbb(valid_plot,3)-(vpx+vpw)
    dyt = (newbb(valid_plot,0)-vpy)
    dyb = (vpy-vph)-newbb(valid_plot,1)
;
; Get largest bounding box that encompasses all non-missing graphical
; objects.
;
    newtop = max(newbb(:,0))
    newbot = min(newbb(:,1))
    newlft = min(newbb(:,2))
    newrgt = max(newbb(:,3))
    delete(newbb)

;
; This section checks to see if all plots along one side are 
; missing, because if they are, we have to pretend like they
; are just invisible (i.e. do the maximization as if the invisible
; plots were really there).  This section needs to take
; place even if no plots are missing, because it's possible the
; user specified fewer plots than panels.
;
    xlft = min_xpos - dxl
    xrgt = max_xpos + vpw + dxr
    xtop = max_ypos + dyt
    xbot = min_ypos - vph - dyb
    if(.not.rgt_pnl.and.xrgt.gt.newrgt) then
      maxbb@gsnPanelInvsblRight = xrgt
      if(panel_debug)
        print("gsnPanelInvsblRight = " + maxbb@gsnPanelInvsblRight)
      end if
    end if

    if(.not.lft_pnl.and.xlft.lt.newlft) then
      maxbb@gsnPanelInvsblLeft = xlft
      if(panel_debug)
        print("gsnPanelInvsblLeft = " + maxbb@gsnPanelInvsblLeft)
      end if
    end if

    if(.not.top_pnl.and.xtop.gt.newtop) then
      maxbb@gsnPanelInvsblTop = xtop
      if(panel_debug)
        print("gsnPanelInvsblTop = " + maxbb@gsnPanelInvsblTop)
      end if
    end if

    if(.not.bot_pnl.and.xbot.lt.newbot) then
      maxbb@gsnPanelInvsblBottom = xbot
      if(panel_debug)
        print("gsnPanelInvsblBottom = " + maxbb@gsnPanelInvsblBottom)
      end if
    end if
  end if
; 
; Draw plots plus labelbar and main title (if they exists). This is
; also where the plots will be maximized for PostScript output,
; if so indicated.
;
  if(draw_boxes)
    draw_and_frame(wks,newplots,calldraw,False,1,maxbb)
  else
    draw_and_frame(wks,newplots,calldraw,callframe,1,maxbb)
  end if  
;
; Draw bounding boxes around each plot object for debugging purposes.
;
  if(draw_boxes)
    do i=0,dimsizes(newplots)-1
      if(.not.ismissing(newplots(i)))
        draw_bb(newplots(i),False)
      end if
    end do
    if(callframe) then
      frame(wks)
    end if
  end if
;
; Debug information
;
  if(panel_debug) then
    bb_dbg = NhlGetBB(newplots)
    if(dimsizes(newplots).gt.1) then
      print("-------min/max NDC values for all objects in panel-------")
      print("min/max x position = " + min(bb_dbg(:,2)) + "/" + max(bb_dbg(:,3)))
      print("min/max y position = " + min(bb_dbg(:,1)) + "/" + max(bb_dbg(:,0)))
    else
      print("-------min/max NDC values for the object in panel-------")
      print("min/max x position = " + min(bb_dbg(2)) + "/" + max(bb_dbg(3)))
      print("min/max y position = " + min(bb_dbg(1)) + "/" + max(bb_dbg(0)))
    end if
    delete(bb_dbg)
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
undef("gsn_panel")
procedure gsn_panel(wks:graphic,plot[*]:graphic,dims[*]:integer,\
                    resources:logical)
local res2
begin
  res2 = get_resources(resources)
  set_attr(res2,"gsnPanelSave",False )
  plots = gsn_panel_return(wks,plot,dims,res2)
end

;***********************************************************************;
; Function : gsn_attach_plots                                           ;
;                   base : base plot                                    ;
;                  plots : list of plots to attach                      ;
;               resplot1 : logical                                      ;
;               resplot2 : logical                                      ;
;                                                                       ;
; This function attaches the list of "plots" to the "base" plot,  either;
; on the right Y axis or bottom X axis of the base plot. The default is ;
; to attach the plots at the Y axis, unless gsnAttachPlotsXAxis is set  ;
; to True.                                                              ;
;                                                                       ;
; By default, the viewport heights of all plots will be made the same,  ;
; appropriate tick marks and labels will be turned off, and the aspect  ;
; ratio preserved.                                                      ;
;                                                                       ;
; For example, if you have the following plots and you want them        ;
; attached at the Y axis:                                               ;
;                                                                       ;
;    ___________      _____  __________                                 ;
;    |         |      |   |  |        |                                 ;
;    |         |      |   |  |        |                                 ;
;    |  base   |      |   |  |        |                                 ;
;    |         |      |   |  |        |                                 ;
;    |         |      |   |  |        |                                 ;
;    -----------      -----  ----------                                 ;
;                                                                       ;
; you will end up with:                                                 ;
;                                                                       ;
;    _________________________                                          ;
;    |         |   |         |                                          ;
;    |         |   |         |                                          ;
;    |  base   |   |         |                                          ;
;    |         |   |         |                                          ;
;    |         |   |         |                                          ;
;    -------------------------                                          ;
;                                                                       ;
; Or, if you have the following plots and you want them attached at the ;
; X axis:                                                               ;
;                                                                       ;
;    ___________      ___________                                       ;
;    |         |      |         |                                       ;
;    |         |      |         |                                       ;
;    |  base   |      |         |                                       ;
;    |         |      -----------                                       ;
;    |         |                                                        ;
;    -----------                                                        ;
;                                                                       ;
; you will end up with:                                                 ;
;                                                                       ;
;    ___________                                                        ;
;    |         |                                                        ;
;    |         |                                                        ;
;    |  base   |                                                        ;
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
undef("gsn_attach_plots")
function gsn_attach_plots(oldbaseplot:graphic,oldplots:graphic, \
                          plotres1:logical, plotres2:logical)
local anno, width1, width2, height1, height2, font_height1, font_height2, \
mj_length1, mj_length2, mjo_length1, mjo_length2, mno_length1, mno_length2, \
mno_length1, mno_length2, total_width1, total_width2, scale1, scale2, scale
begin
  res1  = get_resources(plotres1)
  res2  = get_resources(plotres2)
  base  = oldbaseplot
  plots = oldplots
  nplots= dimsizes(plots)

  res1@gsnMaximize = get_res_value(res1,"gsnMaximize",True)

;---Check for maximization
  maxbb = get_bb_res(res1)
  if(.not.maxbb) then
    maxbb = get_bb_res(res2)
  end if

  attach_y = .not.get_res_value(res1,"gsnAttachPlotsXAxis",False)
  attach_y = .not.get_res_value(res2,"gsnAttachPlotsXAxis",.not.attach_y)
  border_on = get_res_value(res1,"gsnAttachBorderOn",True)

;
; The plots to be attached may not be regular plots (contour, xy, vector,
; etc), so we won't be able to retrieve tickmark info from them.  We have
; to see if they are overlay plots, instead, that have regular plots 
; overlaid on them. If so, we can use the overlaid plots for tickmark
; info. If not, then we are in trouble.
;
; Here's the list of "regular" plot types:
;
  plot_types = (/"contourPlotClass","xyPlotClass","vectorPlotClass",\
                 "streamlinePlotClass","mapPlotClass","logLinPlotClass"/)

;
; First check the base plot for "regular plotness".
;
  found_base = False

  if(any(NhlClassName(base).eq.plot_types)) then
;
; The base plot is a regular plot.
;
    new_base   = base
    found_base = True
  else
;
; The base plot is not a regular plot, so find out if it has a regular
; plot overlaid on it.
;
    getvalues base
      "pmOverlaySequenceIds" : base_ids
    end getvalues
    if(.not.ismissing(base_ids(0))) then
      j = 0
;
; Loop through the overlaid plots and find a "regular" one. We will
; use the first one we find.
;
      do while(j.lt.dimsizes(base_ids).and..not.found_base)
        if(any(NhlClassName(base_ids(j)).eq.plot_types)) then
          new_base   = base_ids(j)
          found_base = True
        end if
        j = j + 1
      end do
    end if
  end if

  if(.not.found_base) then
    print("Warning: gsn_attach_plots: the base plot is an unrecognized plot type; may get unexpected results.")
    new_base = base
  end if
  getvalues new_base
    "trGridType" : grid_type
  end getvalues
  if(any(grid_type.eq.(/get_enum_value("Spherical"),\
                        get_enum_value("TriangularMesh")/))) then
    print("Warning: gsn_attach_plots: the base plot is either spherical or a triangular mesh, so can't fix the tickmarks")
    found_base = False
  end if
;
; Now test the plots to be attached, and see if they are "regular" plots.
;
  found_plots  = new(nplots,logical)
  new_plots    = new(nplots,graphic)
  found_plots  = False

  do i=0,nplots-1
    if(any(NhlClassName(plots(i)).eq.plot_types)) then
      new_plots(i)   = plots(i)
      found_plots(i) = True
    else
      getvalues plots(i)
        "pmOverlaySequenceIds" : tmp_plot_ids
      end getvalues

      if(.not.ismissing(tmp_plot_ids(0))) then
        j = 0
;
; Loop through the overlaid plots and find a "regular" one. We will
; use the first one we find.
;
        do while(j.lt.dimsizes(tmp_plot_ids).and..not.found_plots(i)) 
          if(any(NhlClassName(tmp_plot_ids(j)).eq.plot_types)) then
            new_plots(i)   = tmp_plot_ids(j)
            found_plots(i) = True
          end if
          j = j + 1
        end do
      end if
      delete(tmp_plot_ids)
    end if
    if(.not.found_plots(i)) then
      print("Warning: gsn_attach_plots: unrecognized plot type, may get unexpected results.")
      new_plots(i)   = plots(i)
      found_plots(i) = False
    end if
  end do
;
; Retrieve tickmark lengths and font height labels so we can make
; them the same size later.
;
; Also get the viewport widths and heights so we can maintain the
; aspect ratio, but yet make the heights or widths the same.
;
  getvalues base
    "vpWidthF"          : width1
    "vpHeightF"         : height1
    "tiMainFontHeightF" : main_font_height1
  end getvalues

  widths  = new(dimsizes(plots),float)
  heights = new(dimsizes(plots),float)

  do i=0,nplots-1
    getvalues plots(i)
      "vpWidthF"  : widths(i)
      "vpHeightF" : heights(i)
    end getvalues
  end do

  mj_lengths   = new(nplots,float)
  mjo_lengths  = new(nplots,float)
  mn_lengths   = new(nplots,float)
  mno_lengths  = new(nplots,float)
  font_heights = new(nplots,float)

  if(attach_y)
;
; If didn't find a regular base plot, then we can't do anything
; about the tickmarks.
;
    if(found_base) then
      getvalues new_base
        "tmXBMajorLengthF"        : mj_length1
        "tmXBMajorOutwardLengthF" : mjo_length1
        "tmXBMinorLengthF"        : mn_length1
        "tmXBMinorOutwardLengthF" : mno_length1
        "tmXBLabelFontHeightF"    : font_height1
      end getvalues
    end if

    do i=0,nplots-1
;
; If didn't find a regular plot, then we can't do anything
; about the tickmarks.
;
      if(found_plots(i)) then
        getvalues new_plots(i)
          "tmXBMajorLengthF"        : mj_lengths(i)
          "tmXBMajorOutwardLengthF" : mjo_lengths(i)
          "tmXBMinorLengthF"        : mn_lengths(i)
          "tmXBMinorOutwardLengthF" : mno_lengths(i)
          "tmXBLabelFontHeightF"    : font_heights(i)
        end getvalues
      end if
    end do
  else
;
; If didn't find a regular base plot, then we can't do anything
; about the tickmarks.
;
    if(found_base) then
      getvalues new_base
        "tmYLMajorLengthF"        : mj_length1
        "tmYLMajorOutwardLengthF" : mjo_length1
        "tmYLMinorLengthF"        : mn_length1
        "tmYLMinorOutwardLengthF" : mno_length1
        "tmYLLabelFontHeightF"    : font_height1
      end getvalues
    end if

    do i=0,nplots-1
      if(found_plots(i)) then
        getvalues new_plots(i)
          "tmYLMajorLengthF"        : mj_lengths(i)
          "tmYLMajorOutwardLengthF" : mjo_lengths(i)
          "tmYLMinorLengthF"        : mn_lengths(i)
          "tmYLMinorOutwardLengthF" : mno_lengths(i)
          "tmYLLabelFontHeightF"    : font_heights(i)
        end getvalues
      end if
    end do
  end if

;
; Calculate the scale factor needed to make the plots the same
; size in the appropriate axis.  If we are attaching plots at the Y axis,
; then we want to make them the same height. Otherwise, we want to make
; them the same width.  We do this by keeping the size of the largest
; plot the same, and scaling the rest of the plots to be the same height
; (or width).
;
  scales = new(nplots,float)
  if(attach_y) then
    if(any(height1.lt.heights)) then
      scale1 = max(heights)/height1
      scales = max(heights)/heights
    else
      scale1 = 1.
      scales = height1/heights
    end if
  else 
    if(any(width1.lt.widths)) then
      scale1 = max(widths)/width1
      scales = max(widths)/widths
    else
      scale1 = 1.
      scales = width1/widths
    end if
  end if
;
; Because we are attaching plots along an axis, turn off
; tickmarks and labels where appropriate.
;
  if(attach_y) then
    if(found_base) then
      setvalues new_base
        "tmYUseLeft"   : get_res_value(res1,"tmYUseLeft",False)
        "tmYROn"       : get_res_value(res1,"tmYROn",False)
        "tmYRLabelsOn" : get_res_value(res1,"tmYRLabelsOn",False)
        "tmYRBorderOn" : get_res_value(res1,"tmYRBorderOn",border_on)
      end setvalues
    end if
    do i=0,nplots-2
      if(found_plots(i)) then
        setvalues new_plots(i)
          "tmYUseLeft"   : get_res_value(res2,"tmYUseLeft",False)
          "tmYLOn"       : get_res_value(res2,"tmYLOn",False)
          "tmYLBorderOn" : get_res_value(res2,"tmYLBorderOn",border_on)
          "tmYROn"       : get_res_value(res2,"tmYROn",False)
          "tmYRLabelsOn" : get_res_value(res2,"tmYRLabelsOn",False)
          "tmYRBorderOn" : get_res_value(res2,"tmYRBorderOn",border_on)
          "tiYAxisOn"    : get_res_value(res2,"tiYAxisOn",False)
        end setvalues
      end if
    end do
    if(found_plots(nplots-1)) then
      setvalues new_plots(nplots-1)
        "tmYUseLeft"   : get_res_value(res2,"tmYUseLeft",False)
        "tiYAxisOn"    : get_res_value(res2,"tiYAxisOn",False)
        "tmYLOn"       : get_res_value(res2,"tmYLOn",False)
        "tmYLBorderOn" : get_res_value(res2,"tmYLBorderOn",border_on)
        "tmYLLabelsOn" : get_res_value(res1,"tmYLLabelsOn",False)
      end setvalues
    end if
  else
    if(found_base) then
      setvalues new_base
        "tmXUseBottom" : get_res_value(res1,"tmXUseBottom",False)
        "tmXBOn"       : get_res_value(res1,"tmXBOn",False)
        "tmXBBorderOn" : get_res_value(res1,"tmXBBorderOn",border_on)
        "tmXBLabelsOn" : get_res_value(res1,"tmXBLabelsOn",False)
        "tiXAxisOn"    : get_res_value(res1,"tiXAxisOn",False)
      end setvalues
    end if
    do i=0,nplots-2
      if(found_plots(i)) then
        setvalues new_plots(i)
          "tmXUseBottom" : get_res_value(res2,"tmXUseBottom",False)
          "tmXBOn"       : get_res_value(res2,"tmXBOn",False)
          "tmXBBorderOn" : get_res_value(res2,"tmXBBorderOn",border_on)
          "tmXBLabelsOn" : get_res_value(res2,"tmXBLabelsOn",False)
          "tmXTOn"       : get_res_value(res2,"tmXTOn",False)
          "tmXTBorderOn" : get_res_value(res2,"tmXTBorderOn",border_on)
          "tmXTLabelsOn" : get_res_value(res2,"tmXTLabelsOn",False)
          "tiMainOn"     : get_res_value(res2,"tiMainOn",False)
          "tiXAxisOn"    : get_res_value(res2,"tiXAxisOn",False)
        end setvalues
      end if
    end do
    if(found_plots(nplots-1)) then
      setvalues new_plots(nplots-1)
        "tmXUseBottom" : get_res_value(res2,"tmXUseBottom",False)
        "tmXTOn"       : get_res_value(res2,"tmXTOn",False)
        "tmXTBorderOn" : get_res_value(res2,"tmXTBorderOn",border_on)
        "tmXTLabelsOn" : get_res_value(res2,"tmXTLabelsOn",False)
        "tiMainOn"     : get_res_value(res2,"tiMainOn",False)
      end setvalues
    end if
  end if

;
; Now that we've turned off the tickmark stuff, retrieve the bounding box
; of each plot.
;
; First create arrays to hold bounding box and viewport information.
;
  bbs  = new((/nplots,4/),float)
  vpxs = new((/nplots/),float)
  vpys = new((/nplots/),float)
  vphs = new((/nplots/),float)
  vpws = new((/nplots/),float)

  bb1 = NhlGetBB(base) ; Get bounding box of plot
  top1 = bb1(0)
  bot1 = bb1(1)
  lft1 = bb1(2)
  rgt1 = bb1(3)

;
; Have to deal with special case of only having one plot.
;
  if(nplots.eq.1)
    bbs(0,:) = NhlGetBB(plots)
  else
    bbs = NhlGetBB(plots)
  end if

  tops = bbs(:,0)
  bots = bbs(:,1)
  lfts = bbs(:,2)
  rgts = bbs(:,3)
;
; Retrieve viewports.
;
; Calculate the largest scale factor possible that will allow us
; to fit all plots on the page, with 0.5% white space on the ends.
;
  getvalues base
    "vpYF"      : vpy1
    "vpHeightF" : vph1
    "vpXF"      : vpx1
    "vpWidthF"  : vpw1
  end getvalues 

  do i=0,nplots-1
    getvalues plots(i)
      "vpYF"      : vpys(i)
      "vpHeightF" : vphs(i)
      "vpXF"      : vpxs(i)
      "vpWidthF"  : vpws(i)
    end getvalues 
  end do

  if(maxbb) then
    if(attach_y) then
      total_height1 = top1 - bot1
      total_heights = tops - bots
      total_width1  = (vpx1+vpw1) - lft1
      total_widths  = vpws
      total_widths(nplots-1) = rgts(nplots-1) - vpxs(nplots-1)
  
      scale_widths  = 1. / (1.01 * (scale1*total_width1 + sum(scales*total_widths)))
      scale_height1 = 1. / (1.01 * scale1*total_height1)
      scale_heights = 1. / (1.01 * scales*total_heights)
      scale = min((/scale_height1,min(scale_heights),min(scale_widths)/))
    else
      total_width1  = rgt1 - lft1
      total_widths  = rgts - lfts
      total_height1 = vph1 + (top1 - vpy1)
      total_heights = vphs
      total_heights(nplots-1) = vpys(nplots-1)-bots(nplots-1)
  
      scale_heights = 1. / (1.01 * (scale1*total_height1 + sum(scales*total_heights)))
      scale_width1  = 1. / (1.01 * scale1*total_width1)
      scale_widths  = 1. / (1.01 * scales*total_widths)
      scale = min((/scale_width1,min(scale_heights),min(scale_widths)/))
    end if
  else
    scale = 1.0
  end if

;
; Resize all plots with new scale factor, and set sizes of tick marks
; and tick marks labels to be the same.
;
  new_scale1 = scale * scale1
  new_scales = scale * scales

  if(found_base) then
    new_mj_length = (new_scale1*mj_length1 + sum(new_scales*mj_lengths))/(nplots+1)
    new_mjo_length = (new_scale1*mjo_length1 + sum(new_scales*mjo_lengths))/(nplots+1)
    new_mn_length = (new_scale1*mn_length1 + sum(new_scales*mn_lengths))/(nplots+1)
    new_mno_length = (new_scale1*mno_length1 + sum(new_scales*mno_lengths))/(nplots+1)
    new_font_height = (new_scale1*font_height1 + sum(new_scales*font_heights))/(nplots+1)
  else
    new_font_height = sum(new_scales*font_heights)/nplots
  end if
  new_main_font_height = new_scale1*main_font_height1

  if(found_base) then
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
  end if

  font_heightxl = get_res_value(res1,"tmXBFontHeightF",new_font_height)
  font_heightyl = get_res_value(res1,"tmYLFontHeightF",new_font_height)
  font_heightx  = get_res_value(res1,"tiXAxisFontHeightF",new_font_height)
  font_heighty  = get_res_value(res1,"tiYAxisFontHeightF",new_font_height)
  main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                     max((/new_main_font_height,new_font_height/)))

  setvalues base
    "vpHeightF"               : new_scale1 * height1
    "vpWidthF"                : new_scale1 * width1
  end setvalues

  if(found_base) then
    setvalues new_base
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
  
      "tmXBLabelFontHeightF"    : font_heightxl
      "tmYLLabelFontHeightF"    : font_heightyl
    end setvalues
  end if

  if(found_base) then
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
  end if

  font_heightxl = get_res_value(res2,"tmXBFontHeightF",new_font_height)
  font_heightyl = get_res_value(res2,"tmYLFontHeightF",new_font_height)
  font_heightx  = get_res_value(res2,"tiXAxisFontHeightF",new_font_height)
  font_heighty  = get_res_value(res2,"tiYAxisFontHeightF",new_font_height)
  main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                     max((/new_main_font_height,new_font_height/)))

  do i=0,nplots-1
    setvalues plots(i)
      "vpHeightF"               : new_scales(i) * heights(i)
      "vpWidthF"                : new_scales(i) * widths(i)
    end setvalues

    if(found_plots(i)) then
      setvalues new_plots(i)
        "tiXAxisFontHeightF"      : font_heightx
        "tiYAxisFontHeightF"      : font_heighty
        "tiMainFontHeightF"       : main_font_height
      end setvalues
      if(found_base) then
        setvalues new_plots(i)
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
  
          "tmXBLabelFontHeightF"    : font_heightxl
          "tmYLLabelFontHeightF"    : font_heightyl
        end setvalues
      end if
    end if
  end do
;
; Get new bounding boxes and sizes of resized plots, so we can
; figure out where to position the base plot.
;
  bb1 = NhlGetBB(base) ; Get bounding box of plot

  top1 = bb1(0)
  bot1 = bb1(1)
  lft1 = bb1(2)
  rgt1 = bb1(3)

  if(nplots.eq.1)
    bbs(0,:) = NhlGetBB(plots)
  else
    bbs = NhlGetBB(plots)
  end if
  tops = bbs(:,0)
  bots = bbs(:,1)
  lfts = bbs(:,2)
  rgts = bbs(:,3)

  getvalues base
    "vpYF"      : vpy1
    "vpHeightF" : vph1
    "vpXF"      : vpx1
    "vpWidthF"  : vpw1
  end getvalues 

  do i=0,nplots-1
    getvalues plots(i)
      "vpYF"      : vpys(i)
      "vpHeightF" : vphs(i)
      "vpXF"      : vpxs(i)
      "vpWidthF"  : vpws(i)
    end getvalues 
  end do

  if(attach_y) then
    total_height1          = top1 - bot1
    total_heights          = tops - bots
    total_width1           = (vpx1+vpw1) - lft1
    total_widths           = vpws
    total_widths(nplots-1) = rgts(nplots-1) - vpxs(nplots-1)
    total_width_left  = max((/0.,1. - (total_width1 + sum(total_widths))/))
    total_height_left = max((/1. - max((/total_height1,max(total_heights)/))/))
  else
    total_width1  = rgt1 - lft1
    total_widths  = rgts - lfts
    total_height1 = vph1 + (top1 - vpy1)
    total_heights = vphs
    total_heights(nplots-1) = vpys(nplots-1)-bots(nplots-1)

    total_height_left = max((/0.,1. - (total_height1 + sum(total_heights))/))
    total_width_left  = max((/0.,1. - max((/total_width1,max(total_widths)/))/))
  end if

  new_vpx1 = total_width_left/2. + (vpx1-lft1)
  new_vpy1 = 1. - (total_height_left/2. + (top1-vpy1))

  setvalues base
    "vpYF" : new_vpy1
    "vpXF" : new_vpx1
  end setvalues

;
; Attach each plot.  If attaching them on the X axis, then start with
; the bottommost plot. If attaching on the Y axis, start with the
; rightmost plot.
;
  annos = new(nplots,graphic)

  zone = get_res_value(res2,"amZone",1)
  orth = get_res_value(res2,"amOrthogonalPosF",0.0)
  para = get_res_value(res2,"amParallelPosF",0.5)

  if(attach_y) then
    side = get_res_value(res2,"amSide","Right")
    just = get_res_value(res2,"amJust","CenterLeft")
  else
    side = get_res_value(res2,"amSide","Bottom")
    just = get_res_value(res2,"amJust","TopCenter")
  end if

  do i=nplots-1,0,1
    if(i.gt.0) then
      annos(i) = NhlAddAnnotation(plots(i-1),plots(i))
    else
      annos(0) = NhlAddAnnotation(base,plots(0))
    end if
    setvalues annos(i)
      "amZone"          : zone
      "amJust"          : just
      "amSide"          : side
      "amResizeNotify"  : True     ; Allow resize if plot resized.
      "amParallelPosF"  : para
      "amOrthogonalPosF": orth
    end setvalues
  end do

;---The plot does not get drawn in this function!
  wks   = NhlGetParentWorkstation(base)
  draw_and_frame(wks,base,False,False,0,maxbb)

  return(annos)
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
undef("gsn_define_colormap")
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
undef("gsn_retrieve_colormap")
function gsn_retrieve_colormap(wks:graphic)
begin
    getvalues wks
        "wkColorMap" : cmap
    end getvalues

    return(cmap)
end

;***********************************************************************;
; Procedure : gsn_reverse_colormap                                      ;
;                   wks: workstation object                             ;
;                                                                       ;
; This function reverses the current color map in use for workstation   ;
; "wks". The foregound/background colors will stay the same.            ;
;***********************************************************************;
undef("gsn_reverse_colormap")
procedure gsn_reverse_colormap(wks:graphic)
begin
  getvalues wks
    "wkColorMap" : cmap
  end getvalues
  cmap(2:,:) = cmap(2::-1,:)               ; reverse (exclude fore/back)
  gsn_define_colormap (wks, cmap)
  return
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
undef("gsn_merge_colormaps")
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
    if(all(is_valid_named_colors(cmap2))) then
      rgb_values = namedcolor2rgb(cmap2)
      indices    = ind(.not.ismissing(rgb_values(:,0)))
      new_cmap2 = new((/dimsizes(indices),3/),"float")
      new_cmap2(:,0) = rgb_values(indices,0)   ; Must be named colors.
      new_cmap2(:,1) = rgb_values(indices,1)
      new_cmap2(:,2) = rgb_values(indices,2)
      delete([/indices,rgb_values/])
    else
      gsn_define_colormap(wks,cmap2)    ; Must be a color map name.
      tmp_cmap2 = gsn_retrieve_colormap(wks)
      new_cmap2 = tmp_cmap2(2:,:)
      delete(tmp_cmap2)
    end if
    len_cmap2 = dimsizes(new_cmap2(:,0))
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
; Function : scalar_field                                               ;
;            sfname : string                                            ;
;              data : numeric                                           ;
;               res : logical                                           ;
;                                                                       ;
; This function creates a scalarField or meshScalarField object.        ;
;***********************************************************************;
undef("scalar_field")
function scalar_field(sfname:string,data:numeric,res2:logical)
local dims, rank
begin
;
; Check input data. If it is 2D, then create a scalar field.
; If it is 1D, then it must have coordinate arrays the same length.
;
  dims = dimsizes(data)
  rank = dimsizes(dims)
  if(rank.ne.1.and.rank.ne.2) then
    print("Error: scalar_field: The input data must either be 1-dimensional or 2-dimensional")
    dum = new(1,graphic)
    return(dum)
  end if

;
; Get sf resources.
;
  if(rank.eq.2) then

; Create the data object; also, check for a missing value and
; set during the create call (if you don't do this, then if the
; user's happens by freak chance to have a constant field of 
; missing values, it will choke.

    if(isatt(data,"_FillValue")) then
;
; We need to check for stride, because if we have a case where
; we are creating a scalar field that will be used with the vector
; field, and we are setting the stride for both via setvalues rather
; than during a create call, then at some point the sizes of the
; vector and scalar fields will be different and you will get a
; warning message:
;
;    warning:VectorPlotSetValues: ignoring vcScalarFieldData: size does
;     not match vcVectorFieldData
;
      if(isatt(res2,"sfXCStride").and.isatt(res2,"sfYCStride")) then 
        data_object = create sfname scalarFieldClass noparent
          "sfMissingValueV" : data@_FillValue
          "sfDataArray"     : data
          "sfXCStride"      : get_res_value(res2,"sfXCStride",1)
          "sfYCStride"      : get_res_value(res2,"sfYCStride",1)
        end create
      else
        data_object = create sfname scalarFieldClass noparent
          "sfMissingValueV" : data@_FillValue
          "sfDataArray"     : data
        end create
      end if
    else
      if(isatt(res2,"sfXCStride").and.isatt(res2,"sfYCStride")) then 
        data_object = create sfname scalarFieldClass noparent
          "sfDataArray" : data
          "sfXCStride"  : get_res_value(res2,"sfXCStride",1)
          "sfYCStride"  : get_res_value(res2,"sfYCStride",1)
        end create
      else
        data_object = create sfname scalarFieldClass noparent
          "sfDataArray" : data
        end create
      end if
    end if
  else
;
; Rank is 1. This means we have to use the mesh scalar object.
; Make sure sfXArray and sfYArray have been set.
;
    if(isatt(res2,"sfXArray").and.isatt(res2,"sfYArray")) then
;
; Create the data object.
;
      if(isatt(data,"_FillValue")) then
         data_object = create sfname meshScalarFieldClass noparent
            "sfDataArray"     : data
            "sfMissingValueV" : data@_FillValue
            "sfXArray"        : get_res_value(res2,"sfXArray",1)
            "sfYArray"        : get_res_value(res2,"sfYArray",1)
          end create
      else
        data_object = create sfname meshScalarFieldClass noparent
          "sfDataArray" : data
          "sfXArray"    : get_res_value(res2,"sfXArray",1)
          "sfYArray"    : get_res_value(res2,"sfYArray",1)
        end create
      end if
    else
      print("Error: scalar_field: If the input data is 1-dimensional, you must set sfXArray and sfYArray to 1-dimensional arrays of the same length.")
      dum = new(1,graphic)
      return(dum)
    end if
  end if

  return(data_object)
end


;***********************************************************************;
; Function : vector_field                                               ;
;              name : string                                            ;
;                u  : numeric                                           ;
;                v  : numeric                                           ;
;               res : logical                                           ;
;                                                                       ;
; This function creates a vectorField object.                           ;
;***********************************************************************;
undef("vector_field")
function vector_field(vfname:string,u:numeric,v:numeric,res2:logical)
begin
;
; We need to check for stride, because if we have a case where
; we are creating a scalar field that will be used with the vector
; field, and we are setting the stride for both via setvalues rather
; than during a create call, then at some point the sizes of the
; vector and scalar fields will be different and you will get a
; warning message:
;
;    warning:VectorPlotSetValues: ignoring vcScalarFieldData: size does
;     not match vcVectorFieldData
;
  if(isatt(res2,"vfXCStride").and.isatt(res2,"vfYCStride")) then
    data_object = create vfname vectorFieldClass noparent
      "vfUDataArray" : u
      "vfVDataArray" : v
      "vfXCStride"   : get_res_value(res2,"vfXCStride",1)
      "vfYCStride"   : get_res_value(res2,"vfYCStride",1)
    end create
  else
    data_object = create vfname vectorFieldClass noparent
      "vfUDataArray" : u
      "vfVDataArray" : v
    end create
  end if

; Check for missing values.

  if(isatt(u,"_FillValue")) then
      setvalues data_object
          "vfMissingUValueV" : u@_FillValue
      end setvalues
  end if
  if(isatt(v,"_FillValue")) then
      setvalues data_object
          "vfMissingVValueV" : v@_FillValue
      end setvalues
  end if

  return(data_object)
end


;***********************************************************************;
; Function : hist_columns                                               ;
;                   wks: workstation object                             ;
;                    xy: graphic                                        ;
;             binvalues: numeric                                        ;
;               barlocs: numeric                                        ;
;              barwidth: numeric                                        ;
;                colors                                                 ;
;               compare: logical                                        ;
;                 gsres: logical                                        ;
;                                                                       ;
;  xy        - xy plot id to draw columns on                            ;
;  bins      - the center of each bin range.                            ;
;  binvalues - the number of values in the corresponding bin            ;
;  barlocs   - the start of the first bar in each bin                   ;
;  width     - the width of the bar                                     ;
;  colors    - array of colors to use                                   ;
;  gsres     - optional primitive resources                             ;
;                                                                       ;
; This function creates the columns for a histogram plot. The Y axis    ;
; will represent the number of values in a bin and a percentage.        ;
;                                                                       ;
;***********************************************************************;
undef("hist_columns")
function hist_columns(wks[1]:graphic,xy[1]:graphic,binvalues:numeric, \
                      barlocs[*][*]:numeric, barwidth[*]:numeric, \
                      colors, compare:logical, gsres:logical)
local i, nbins, dims, nbinvalues, gsres, xpoints, ypoints, multibars,\
col_dims, col_rank
begin
  dims       = dimsizes(barlocs)
  nbarsinbin = dims(0)
  nbars      = dims(1)
  delete(dims)

  dims = dimsizes(binvalues)
  if(dimsizes(dims).eq.1) then
    nbinvalues = dims(0)
  else
    nbinvalues = dims(1)
  end if
  delete(dims)
  if(nbars.ne.nbinvalues) then
    print("Error: hist_columns: Dimension sizes of bins (" + nbars+ ") and binvalues (" + nbinvalues + ") must be the same")
    return
  end if

  if(nbarsinbin.ge.2)
    multibars   = True
  else
    multibars   = False
  end if

;
; Set up arrays to hold polygon points.
;
  if(multibars) then
    xpoints   = new((/nbarsinbin,nbars,5/),float)
    ypoints   = new((/nbarsinbin,nbars,5/),float)
    polygons  = new((/nbarsinbin,nbars/),graphic)
  else
    xpoints  = new((/nbars,5/),float)
    ypoints  = new((/nbars,5/),float)
    polygons = new(nbars,graphic)
  end if
;
; Set up variable to hold resources.
;
  gsres           = True
  set_attr(gsres,"gsEdgesOn",True)
;
; Begin assigning polygon points.
;
  if(multibars)
    do i=0,nbarsinbin-1
      ypoints(i,:,0) = (/0/)
      ypoints(i,:,1) = (/binvalues(i,:)/)
      ypoints(i,:,2) = (/ypoints(i,:,1)/)
      ypoints(i,:,3) = (/0/)
      ypoints(i,:,4) = (/0/)

      xpoints(i,:,0) = (/barlocs(i,:)/)
      xpoints(i,:,1) = (/xpoints(i,:,0)/)
      xpoints(i,:,2) = (/barlocs(i,:) + barwidth/)
      xpoints(i,:,3) = (/xpoints(i,:,2)/)
      xpoints(i,:,4) = (/xpoints(i,:,0)/)
    end do
  else
    ypoints(:,0) = (/0/)
    ypoints(:,1) = (/binvalues/)
    ypoints(:,2) = (/ypoints(:,1)/)
    ypoints(:,3) = (/0/)
    ypoints(:,4) = (/0/)

    xpoints(:,0) = (/barlocs(0,:)/)
    xpoints(:,1) = (/xpoints(:,0)/)
    xpoints(:,2) = (/barlocs(0,:) + barwidth/)
    xpoints(:,3) = (/xpoints(:,2)/)
    xpoints(:,4) = (/xpoints(:,0)/)
  end if

  if(compare)
    fillindex = get_res_value(gsres,"gsFillIndex",(/0,6/))
  else
    if(multibars) then
      fillindex = get_res_value(gsres,"gsFillIndex",0)
      if(dimsizes(dimsizes(fillindex)).eq.1) then
         ftmp = new(nbarsinbin,typeof(fillindex))
         ftmp = fillindex
         delete(fillindex)
         fillindex = ftmp
         delete(ftmp)
      end if
    else
      fillindex = get_res_value(gsres,"gsFillIndex",0)
    end if
  end if
;
; Make sure fill indices are between 0 and 17.
; No more, because you have have added your own fill index.
;
;  fillindex = min((/max((/max(fillindex),0/)),17/))

  rgba_arr = convert_color_to_rgba(wks,colors)
  col_dims = dimsizes(rgba_arr)   ; better be 2D or 3D
  col_rank = dimsizes(col_dims)
  if(col_rank.gt.3.or.(col_rank.eq.3.and.col_dims(0).ne.nbarsinbin)) then
    print("Error: hist_columns: Invalid dimensionality of array of colors passed in.")
    print("   Was expecting a 1D or 2D array of colors")
    exit
  end if
  if(col_rank.eq.3) then
    ncolors = col_dims(1)
  else
    ncolors = col_dims(0)
  end if
  do i = 0, nbars - 1
    if(col_rank.eq.1) then
      gsres@gsFillColor = rgba_arr
    else if(col_rank.eq.2) then
      gsres@gsFillColor = rgba_arr(i % ncolors,:)
    end if
    end if
    if(multibars) 
;
; Add the remaining histogram bars first, so that they will be drawn
; second. The first histogram bars will thus be drawn on top, if
; they are being stacked.
;
      do j=nbarsinbin-1,0,1
        if(col_rank.eq.3) then
          gsres@gsFillColor = rgba_arr(j,i % ncolors,:)
        end if
        gsres@gsFillIndex = fillindex(j)
        if(binvalues@horizontal) then
          polygons(j,i) = gsn_add_polygon(wks,xy,ypoints(j,i,:), \
                                         xpoints(j,i,:), gsres)
        else
          polygons(j,i) = gsn_add_polygon(wks,xy,xpoints(j,i,:), \
                                          ypoints(j,i,:), gsres)
        end if
      end do
    else
      gsres@gsFillIndex = fillindex
      if(binvalues@horizontal) then
        polygons(i) = gsn_add_polygon(wks,xy,ypoints(i,:),xpoints(i,:),gsres)
      else
        polygons(i) = gsn_add_polygon(wks,xy,xpoints(i,:),ypoints(i,:),gsres)
      end if
    end if
  end do
;
; Return the polygons created as attributes of the XY plot. This is
; necessary, b/c otherwise the polygons will go away when you exit the
; function.
;
  var_string      = unique_string("hpolygons")
  xy@$var_string$ = polygons
  return(xy)
end

;***********************************************************************;
; Function : compute_hist_vals                                          ;
;                     x: numeric or string                              ;
;               binlocs: numeric or string                              ;
;              nbinlocs: integer                                        ;
;             bin_width: numeric                                        ;
;           setinterval: logical                                        ;
;           setdiscrete: integer                                        ;
;            minmaxbins: logical                                        ;
;             count_msg: logical                                        ;
;                isnice: integer                                        ;
;               compare: integer                                        ;
;            sidebyside: integer                                        ;
;                                                                       ;
; By default, this routine calculates a nice set of ranges for "binning";
; the given data. The following cases are possible:                     ;
;                                                                       ;
; 1. If setinterval is True, then the user has set their own bin        ;
;    intervals via either the gsnHistogramBinIntervals or the           ;
;    gsnHistogramClassIntervals resource (stored in binlocs array).     ;
;                                                                       ;
; 2. If setdiscrete is True, then the user has set discrete bin values  ;
;    via the gsnHistogramDiscreteBinValues resource (stored in          ;
;    "binlocs" array).                                                  ;
;                                                                       ;
; 3. If neither setinterval or setdiscrete is True, and if the resource ;
;    gsnHistogramBinWidth is set, then its value will be used as a bin  ;
;    width (bin_width). By default, bin_width will only be used as an   ;
;    approximate value, because it attempts to select "nice" values     ;
;    based on a width close to bin_width. If the resource               ;
;    gsnHistogramSelectNiceIntervals (isnice) is set to False, then     ;
;    you will get a bin width exactly equal to bin_width.               ;
;                                                                       ;
; 4. If neither setinterval or setdiscrete is True, and if the resource ;
;    gsnHistogramNumberOfBins is set, then its value (nbinlocs) will be ;
;    used to determine the number of bins. By default, nbinlocs is only ;
;    used as an approximate value, because it attempts to select "nice" ;
;    values based on a number of bins close to nbinlocs. If the         ;
;    resource gsnHistogramSelectNiceIntervals (isnice) is set to False, ;
;    then you will get a number of bins exactly equal to nbinlocs.      ;
;                                                                       ;
; 5. If no special resources are set, then this routine defaults to     ;
;    calculating approximately 10 "nice" bin intervals, based on the    ;
;    range of the data.                                                 ;
;                                                                       ;
;***********************************************************************;
undef("compute_hist_vals")
function compute_hist_vals(xx,binlocs,nbinlocs[1]:integer, \
                           bin_width:numeric,setinterval:logical, \
                           setdiscrete:logical, minmaxbins:logical, \
                           count_msg:logical,isnice:logical, \
                           compare:logical,sidebyside:logical)
local xmin, xmax, new_binlocs, nbars, buckets, x, i
begin
  if(isnumeric(xx)) then
    xmin = tofloat(min(xx))
    xmax = tofloat(max(xx))
  end if

;
; If the bins are set, need to determine if you want to have the bins
; represent ranges of values or discrete values.
;
  if(setdiscrete) then
    new_binlocs = binlocs
    nbars       = dimsizes(new_binlocs)   ; # of bars equals # of binlocs
    nsets       = dimsizes(new_binlocs)   ; # of bars equals # of binlocs
  else
;
; Check if range values have been set by user, or if we need to
; calculate them.
;
    if(setinterval) then
      new_binlocs = binlocs
    else
      if(nbinlocs.lt.0) then
        if(bin_width.lt.0.) then
          nbinlocs = 10                   ; Default to 10 bin locations.
        else 
          nbinlocs = floattointeger(((xmax - xmin)/bin_width))
          if(nbinlocs.le.0) then
            print("Warning: compute_hist_vals: cannot use given bin width. Defaulting...")
            nbinlocs = 10 
          end if
        end if
      end if
      if(.not.setdiscrete) then
        if(isnice) then
;
; Based on min and max of data, compute a new min/max/step that will
; give us "nice" bin values.
;
           nicevals = nice_mnmxintvl(xmin,xmax,nbinlocs,True)
           nvals    = floattoint((nicevals(1) - nicevals(0))/nicevals(2) + 1)
           new_binlocs = fspan(nicevals(0),nicevals(1),nvals)
         else
;
; Don't bother with "nice" values; just span the data. 
;
           new_binlocs = fspan(xmin,xmax,nbinlocs+1)
         end if
      end if
    end if
    nbars = dimsizes(new_binlocs)-1
  end if
;
; Count number of values in a particular bin range, or exactly
; equal to a bin value if discrete.
;
  if(compare.or.sidebyside) then
    dims  = dimsizes(xx)
    nsets = dims(0)
    npts  = dims(1)
    x     = xx
  else
    nsets  = 1
    npts   = dimsizes(xx)
    x      = new((/1,npts/),typeof(xx))
    x(0,:) = xx
  end if

;
; Set up variable to hold binned values. Binned values can have
; unequal spacing.
;
  num_in_bins = new((/nsets,nbars/),integer)
;
; Count the values in each discrete bin.
;
  if(setdiscrete) then
    do j = 0,nsets-1
      do i = 0, nbars-1
        num_in_bins(j,i) = num(x(j,:).eq.new_binlocs(i))
      end do
    end do
  else
;
; Count the values in each bin interval. Bin intervals can be
; of length 0, meaning an exact count is done.
;
    do j = 0,nsets-1
      do i = 0, nbars-1
        if(new_binlocs(i).eq.new_binlocs(i+1)) then
          num_in_bins(j,i) = num(x(j,:).eq.new_binlocs(i))
        else
;
; Special tests for last interval are required.
;
          if(i.eq.(nbars-1)) then
            if(nbars.gt.1.and.new_binlocs(i).eq.new_binlocs(i-1)) then
              num_in_bins(j,i) = num(x(j,:).gt.new_binlocs(i).and. \
                                     x(j,:).le.new_binlocs(i+1))
            else
              num_in_bins(j,i) = num(x(j,:).ge.new_binlocs(i).and. \
                                     x(j,:).le.new_binlocs(i+1))
            end if
          else
;
; If the previous interval was not really an interval, but an exact
; bin value, then be careful not to count those values in the current
; interval.
;
            if(i.gt.0.and.new_binlocs(i).eq.new_binlocs(i-1)) then
              num_in_bins(j,i) = num(x(j,:).gt.new_binlocs(i).and. \
                                     x(j,:).lt.new_binlocs(i+1))
            else
              num_in_bins(j,i) = num(x(j,:).ge.new_binlocs(i).and. \
                                     x(j,:).lt.new_binlocs(i+1))
            end if
          end if
        end if
      end do
    end do
  end if
;
; If minmaxbins is True, then we need to also count the values
; outside the range of our new_binlocs.
;
  if(minmaxbins) then
    new_num_in_bins            = new((/nsets,nbars+2/),integer)
    new_num_in_bins(:,1:nbars) = num_in_bins
    do j = 0,nsets-1
      new_num_in_bins(j,0)       = num(x(j,:).lt.new_binlocs(0))
      new_num_in_bins(j,nbars+1) = num(x(j,:).gt.new_binlocs(nbars))
    end do
    delete(num_in_bins)
    num_in_bins = new_num_in_bins
    delete(new_num_in_bins)
    nbars = nbars + 2
  end if
;
; Count number of missing values.
;
  num_missing = new(nsets,integer)
  if(isatt(x,"_FillValue")) then
    do j = 0,nsets-1
      num_missing(j) = num(ismissing(x(j,:)))
    end do
  else
    num_missing = 0
  end if
;
; If count_msg is True, then we need to bin the number of missing values.
;
  if(count_msg) then
    new_num_in_bins              = new((/nsets,nbars+1/),integer)
    new_num_in_bins(:,0:nbars-1) = num_in_bins
    new_num_in_bins(:,nbars)     = num_missing

    delete(num_in_bins)
    num_in_bins = new_num_in_bins
    delete(new_num_in_bins)
    nbars = nbars + 1
  end if

;
; Calculate percentages, both with and without missing points included.
;
  npts_nomiss = npts - num_missing

  if(compare.or.sidebyside) then
    percs          = (100.*num_in_bins)/tofloat(npts)
    percs_nomiss   = new((/nsets,nbars/),float)
    do i=0,nsets-1
      percs_nomiss(i,:) = (100.*num_in_bins(i,:))/tofloat(npts_nomiss(i))
    end do
  else
    percs        = (100.*num_in_bins(0,:))/tofloat(npts)
    percs_nomiss = (100.*num_in_bins(0,:))/tofloat(npts_nomiss)
  end if
;
; Return information.
; 
  num_in_bins@NumMissing     = num_missing
  num_in_bins@binlocs        = new_binlocs
  num_in_bins@percentages    = percs
  num_in_bins@percentages_nm = percs_nomiss

  delete(x)
  if(compare.or.sidebyside) then
    return(num_in_bins)
  else
    return(num_in_bins(0,:))
  end if
end 

;***********************************************************************;
; Function : gsn_histogram                                              ;
;                   wks: workstation object                             ;
;                 xdata: numeric or string                              ;
;                   res: resources                                      ;
;                                                                       ;
; This function draws a histogram plot. The user can enter the bin      ;
;      ranges, discrete bin values, or let this function calculate ones ;
;      automatically.  This function will also compare multiple sets of ;
;      histograms. If xdata is a string, then you must be using         ;
;      discrete values, and not ranges.                                 ;
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
; gsnHistogramBarWidthPercent - This indicates the percentage of the    ;
;       the bin width that the bar width should be. The default is 66%  ;
;       for single histograms, and 50% for comparison.                  ;
;                                                                       ;
;  gsnHistogramSelectNiceIntervals - Indicates whether we want          ;
;       gsn_histogram to select "nice" range values. Default is True.   ;
;                                                                       ;
;  gsnHistogramComputePercentages - If True, then percentage values     ;
;       will be put on right (top) axis. Default is False.              ;
;                                                                       ;
;  gsnHistogramComputePercentagesNoMissing - If True, percentage values ;
;       will be put on the right (top) axis, and the number of missing  ;
;       values will be subtracted from the total number of values       ;
;       before the percentages are calculated.                          ;
;                                                                       ;
;  gsnHistogramPercentSign - If True, then a percent sign (%) is used on;
;       the percentage axis.                                            ;
;                                                                       ;
;  gsnHistogramClassIntervals - By default, gsn_histogram will pick the ;
;       bin class intervals for you. If you set this, then it will use  ;
;       these values for the bin ranges.                                ;
;                                                                       ;
;  gsnHistogramBinIntervals   - Same as gsnHistogramClassIntervals.     ;
;                                                                       ;
;  gsnHistogramMinMaxBinsOn - If this is True, then two extra bins will ;
;      be added that count all the values less than the smallest bin,   ;
;      and greater than the largest bin.  This resource can only be     ;
;      used when BinIntervals or ClassIntervals are set.                ;
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
; called "NumMissing".                                                  ;
;                                                                       ;
; This function does the following:                                     ;
;                                                                       ;
; 1. Sets/retrieves all of the special "gsn" resource allowed and       ;
;    checks validity of data.                                           ;
; 2. Calls "compute_hist_vals" to calculate bins (if not specified by   ;
;    user) and count number of values in each bin range, or equal to    ;
;    each discrete bin value.                                           ;
; 3. Count number of bin locations and bars.                            ;
; 4. Calculate width of bins and bars in each bin.                      ;
; 5. Figure out color indices to use for bar colors.                    ;
; 6. Calculate axis limits for plot, and create plot.                   ;
; 7. Set some post-resources for labeling the percentage axis.          ;
; 8. Set some post-resources for labeling the other axis.               ;
; 9. Set some post-resources for labeling the axis that indicates the   ;
;    bin ranges or discrete values.                                     ;
;10. Apply resources set by user.                                       ;
;11. Calculate starting locations of each bar.                          ;
;12. Add a percent label, if there is one.                              ;
;13. Force tickmarks to point outward.                                  ;
;14. Create histogram.                                                  ;
;15. Draw and advance frame.                                            ;
;16. Return histogram information.                                      ;
;***********************************************************************;
undef("gsn_histogram")
function gsn_histogram(wks[1]:graphic,xdata, resources:logical)
local res2, calldraw, callframe, maxbb, ispercent, bins, nbars, \
      setdiscrete, setinterval,top, bottom, left, right, lenc, tmp, \
      colors, locs, lbs, compare
begin
  dummy = new(1,graphic)      ; Dummy graphic to return if things go wrong.
;
; 1. Retrieve special resources.
;
  res2       = get_resources(resources)

;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res2,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(xdata,new(1,float),new(1,float),"gsn_histogram", \
                     res2,1)
  end if

  calldraw   = get_res_value(res2,"gsnDraw", True)
  callframe  = get_res_value(res2,"gsnFrame",True)
  maxbb      = get_bb_res(res2)
  nbins      = get_res_value(res2,"gsnHistogramNumberOfBins",-1)
  xbin_width = get_res_value(res2,"gsnHistogramBinWidth",-1)
  minmaxbins = get_res_value(res2,"gsnHistogramMinMaxBinsOn",False)
  count_msg  = get_res_value(res2,"gsnHistogramBinMissing",False)
  perc_nomiss= get_res_value(res2,"gsnHistogramComputePercentagesNoMissing",False)
  ispercent  = get_res_value(res2,"gsnHistogramComputePercentages",perc_nomiss)
  percentsign= get_res_value(res2,"gsnHistogramPercentSign",False)
  isnice     = get_res_value(res2,"gsnHistogramSelectNiceIntervals",True)
  compare    = get_res_value(res2,"gsnHistogramCompare",False)
  sidebyside = get_res_value(res2,"gsnHistogramSideBySide",False)
  horizontal = get_res_value(res2,"gsnHistogramHorizontal",False)
  bar_spc_perc = get_res_value(res2,"gsnHistogramBarSpacingPercent",0.)

  setdiscrete= False
  setinterval= False
;
; xdata can be nD. The left most dimension indicates the number of bars
; per bin, and the rightmost dimension the data values.

; In the case where the leftmost dimension is 2, there's
; the special case of "comparing" histograms - that is, the bars
; will be stacked (gsnHistogramCompare = True)
;
; If the leftmost dimension is > 2, then the bars will be 
; side by side (sidebyside = True).
;

;
; Also, if gsnHistogramCompare is set to True, then 
; double-check that data is correct size.
;
  dims = dimsizes(xdata)
  rank = dimsizes(dims)
  if(rank.eq.1) then
    nbars_per_bin = 1
  else
    nbars_per_bin = dims(0)
  end if

;
; If comparing two fields, then first dimension must be two.
;
  if(compare) then
    if(nbars_per_bin.ne.2.or.rank.eq.1) then
      print("Error: gsn_histogram: If comparing two fields, then you must input a 2D array dimensioned 2 x npts.")
      print("No plot will be drawn.")
      return(dummy)
    end if
  else 
    if(rank.eq.2.and.dims(0).eq.2) then
      if(.not.sidebyside) then
        print("gsn_histogram: You input a 2D array dimensioned 2 x npts, so will go into compare mode.")
        compare = True
      end if
    end if
  end if    

;
; If you have > 2 leftmost dimensions, then we are in "side-by-side" mode.
;
  if(rank.gt.1.and..not.compare) then
    sidebyside = True
  end if    
;
; Calculate the percentage of the bin width that the bar width should
; be.
;
  if(compare) then
    bar_wdt_perc = 0.5
  else
    if(sidebyside) then
      bar_wdt_perc = 1./(nbars_per_bin+1)
    else
      bar_wdt_perc = 0.66667
    end if
  end if
  bar_wdt_perc_was_set = False
  if(isatt(res2,"gsnHistogramBarWidthPercent")) then
    if(res2@gsnHistogramBarWidthPercent.le.0.or. \
       res2@gsnHistogramBarWidthPercent.gt.100.) then
      print("gsn_histogram: The bar width percentage must be in the range (0,100.].")
      print("               Defaulting to " + bar_wdt_perc)
    else
      bar_wdt_perc = 0.01*res2@gsnHistogramBarWidthPercent
      bar_wdt_perc_was_set = True
    end if
    delete(res2@gsnHistogramBarWidthPercent)
  end if
;
; Check if user explicitly specified bin values. If so, and they represent
; end points, then make sure there are at least two values and sort them.
;
  if(isatt(res2,"gsnHistogramClassIntervals").or. \
     isatt(res2,"gsnHistogramBinIntervals"))
    setinterval = True
    if(isatt(res2,"gsnHistogramClassIntervals"))
      bins = tofloat(get_res_value(res2,"gsnHistogramClassIntervals",1.))
    else
      bins = tofloat(get_res_value(res2,"gsnHistogramBinIntervals",1.))
    end if
    nbins = dimsizes(bins)
    if(nbins.lt.2)
      print("Error: gsn_histogram: There must be at least two values in the bin intervals. No plot will be drawn.")
      return(dummy)
    end if
    if(max(bins).lt.min(xdata).or.min(bins).gt.max(xdata))
      print("Error: gsn_histogram: The bin values you picked are out of range. No plot will be drawn.")
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
    if(setinterval) then
      print("warning: gsn_histogram: You already set gsnHistogramClassIntervals,")
      print("so ignoring gsnHistogramDiscreteClassValues.")
;
; Delete so they don't get passed on to other routines.
;
      delete_attr(res2,"gsnHistogramDiscreteBinValues")
      delete_attr(res2,"gsnHistogramDiscreteClassValues")
    else
      setdiscrete = True
      if(isnumeric(xdata)) then
        if(isatt(res2,"gsnHistogramDiscreteBinValues")) then
          bins = tofloat(get_res_value(res2,"gsnHistogramDiscreteBinValues",1))
        else
          bins = tofloat(get_res_value(res2,"gsnHistogramDiscreteClassValues",1))
        end if
        if(max(bins).lt.min(xdata).or.min(bins).gt.max(xdata)) then
          print("Error: gsn_histogram: The bin values you picked are out of range. No plot will be drawn.")
          return(dummy)
        end if
      else
        if(isatt(res2,"gsnHistogramDiscreteBinValues")) then
          bins = get_res_value(res2,"gsnHistogramDiscreteBinValues",1)
        else
          bins = get_res_value(res2,"gsnHistogramDiscreteClassValues",1)
        end if
      end if
    end if
  end if
;
; You can only count bin values outside the intervals if the intervals
; were set explicitly by the user.
;
  if(.not.setinterval) then
    minmaxbins = False
  end if

  if(.not.setinterval.and..not.setdiscrete) then
    bins = 0.
  end if

  if(setinterval.and..not.isnumeric(xdata)) then
    print("Error: gsn_histogram: If you are doing bin intervals,")
    print("       your data must be numeric.")
    return(dummy)
  end if

;
; 2. Get number of values in each bin (num_in_bins) and bin locations
;    (num_in_bins@binlocs).
;
  num_in_bins = compute_hist_vals(xdata,bins,nbins,xbin_width,setinterval, \
                                  setdiscrete,minmaxbins,count_msg, \
                                  isnice,compare,sidebyside)

  if(all(ismissing(num_in_bins)))
    print("Error: gsn_histogram: Unable to calculate bin values. No plot will be drawn.")
    return(dummy)
  end if

  if(all(num_in_bins.eq.0))
    print("Error: gsn_histogram: No data found in selected bins. No plot will be drawn.")
    return(dummy)
  end if
;
; 3. Count number of bars and bin locations. If minmaxbins is True, then
;    later we need to account for two extra bars.  Also, if count_msg is
;    True, then this adds one extra bar.
;
  nbinlocs = dimsizes(num_in_bins@binlocs)
  if(compare.or.sidebyside)
    dims  = dimsizes(num_in_bins)
    nbars = dims(1)
    delete(dims)
  else
    nbars = dimsizes(num_in_bins)
  end if
;
; 4. Calculate width of histogram bins and bars in each bin. If 
;    comparing two fields, then the histogram bars must be slightly
;    smaller in width to accomodate two bars. If doing side-by-side
;    bars, then the bars need to be even smaller.
;    
;    A note about how the bin widths are calculated: the bin locations
;    (that is, the bin "tickmarks") will be placed along the X axis such
;    that the first bin location is at X = 0, and the last bin location
;    is at X = 1.0. When dealing with discrete values, the bin tickmark
;    falls in the *middle* of the bar, whereas for intervals, the bin
;    tickmarks fall on either side of the bar. The width of each bin,
;    then, is [1.0/(the number of bin locations - 1)]. So, for example,
;    if you have 2 discrete values, the first bin tickmark is at X=0,
;    and the second bin tickmark at X=1.0, giving a bin width of
;    1.0. For the special case where you have only 1 discrete value,
;    you essentially don't have a bin width, so "bin_width" will just 
;    be equal to 1.
;
  extra_bins = 0

  if(minmaxbins) then
    extra_bins = 2
  end if

  if(count_msg) then
    extra_bins = extra_bins + 1
  end if

  if(nbinlocs.gt.1) then
    bin_width = 1./(nbinlocs-1+extra_bins)
  else
    bin_width = 1.
  end if 

;
; Calculate the width of the histogram bar. It needs to be smaller
; if we are comparing two histograms.
;
  if(sidebyside) then
    if(bar_spc_perc.gt.0.and.bar_spc_perc.le.100) then
      bar_spcng  = 0.01 * bar_spc_perc * bin_width
      if(.not.bar_wdt_perc_was_set) then
        bar_width  = (bin_width - (bar_spcng*nbars))/nbars
      else
        bar_width  = bar_wdt_perc * bin_width 
      end if
      if( ((bar_spcng*nbars) + (bar_width*nbars)).gt.bin_width)
        print("Warning: gsn_histogram: The bar spacing and bar width combined is wider than the bin width.")
        print("Will default to no spacing.")
        bar_spc_perc = 0
        bar_width  = (bin_width - (bar_spcng*nbars))/nbars
      end if
      lft_margin = 0.5*bar_spcng
      if(bar_width.le.0) then
        print("Warning: gsn_histogram: The bar spacing percentage selected is too large for the bin width.")
        print("Will default to no spacing.")
        bar_spc_perc = 0
      end if
    end if
    if(bar_spc_perc.eq.0) then
      bar_spcng  = 0.0
      bar_width  = bar_wdt_perc * bin_width 
      lft_margin = 0.5*(bin_width - (nbars*bar_width))
    end if
    if(setdiscrete) then
      lft_margin = -lft_margin
    end if
  else
    bar_width = bar_wdt_perc * bin_width 
  end if
  if(compare.and.1.3333*bar_width.gt.bin_width) then
    print("Warning: gsn_histogram: The bar width percentage selected is too large for this comparison histogram.")
    print("       Resetting to " + 1/1.3333 + " for better results")
    bar_width = bin_width/1.3333
  end if

;
; 5. Get set of color indexes to use for filling bars. Span the full
;    color map when color-filling the bins.
;
  getvalues wks
    "wkColorMapLen" : lenc
  end getvalues
  if(isatt(res2,"gsnHistogramBarColors")) then
    colors = get_res_value(res2,"gsnHistogramBarColors",1)
  else 
    if(isatt(res2,"gsFillColor")) then
      colors = get_res_value(res2,"gsFillColor",1)
    else
      tmp = (lenc-2)/nbars
      if(tmp .eq. 0 )
        print("Warning: gsn_histogram: not enough colors, using single color")
        colors = new(nbars,integer)
        colors = 2
      else 
        colors = new(nbars,integer)
        tmpc   = ispan(2,lenc,tmp)
        if(dimsizes(tmpc).ne.dimsizes(colors))
         colors = tmpc(0:dimsizes(colors)-1)
        else
          colors = tmpc
        end if
        delete(tmpc)
      end if
      delete(tmp)
    end if  
  end if  
;
; 6. Calculate axis limits for plot, and create plot.
;
  if(horizontal) then
    left   = 0.
    right  = max(num_in_bins) * 1.1
    if(setdiscrete) then
      if(nbars.gt.1) then
         bottom = -0.6667 * bin_width
         top    = 1. + 0.6667 * bin_width
       else
         bottom = -0.6667
         top    =  0.6667
      end if
    else
      bottom = 0
      top    = 1.
    end if
  else
    if(setdiscrete) then
      if(nbars.gt.1) then
        left  = -0.6667 * bin_width
        right = 1. + 0.6667 * bin_width
      else
        left  = -0.6667
        right =  0.6667
      end if
    else
      left  = 0.
      right = 1.
    end if
    bottom = 0.
    top    = max(num_in_bins) * 1.1
  end if

;
; Create plot class with limits.
;
  res2@trXMinF = get_res_value(res2,"trXMinF",left)
  res2@trXMaxF = get_res_value(res2,"trXMaxF",right)
  res2@trYMinF = get_res_value(res2,"trYMinF",bottom)
  res2@trYMaxF = get_res_value(res2,"trYMaxF",top)

  xy = create "xy" logLinPlotClass wks
    "pmTickMarkDisplayMode"  : "Always"
    "pmTitleDisplayMode"     : "Always"
    "trXMinF"                : res2@trXMinF
    "trXMaxF"                : res2@trXMaxF
    "trYMinF"                : res2@trYMinF
    "trYMaxF"                : res2@trYMaxF
  end create
;
; 7. Set some post-resources for labeling the percentage axis, if desired.
;
; If gsnHistogramComputePercentages is True, then we'll add percentage
; labels to the right (or top) axis.
;
; If we are comparing two sets of data, and we want to have a
; percent calculation on the other axis, then in order for this
; axis to correctly represent both sets of data, they must both
; have the same number of missing values, or no missing values at
; all.
;
  if(compare.and.perc_nomiss) then
    if(num_in_bins@NumMissing(0).ne.num_in_bins@NumMissing(1)) then
      print("Warning: gsn_histogram: When comparing two sets of data, you must have")
      print("the same number of missing values in both sets (or no missing values at")
      print("all) in order to display a percentage calculation on the other axis.")
      print("gsnHistogramComputePercentages will be set to False.")
      ispercent   = False
      perc_nomiss = False
    end if
  end if
;
; The default is to include the total number of data points in the
; percentage calculate, even if there are missing values.  The user
; must set gsnHistogramComputePercentagesNoMissing to True if he doesn't
; want the missing values included in the calculation.
;
  if(ispercent) then
    if(compare) then
      dims = dimsizes(xdata)
      npts = dims(1)
    else
      npts = dimsizes(xdata)             ; Total number of points. 
    end if
    if(perc_nomiss) then
      npts = npts - num_in_bins@NumMissing(0) ; Don't include missing values
                                              ; in calculation.
    end if
;
; Compute min, max, and step necessary to later get "nice" values for
; the percentages.
;
    xnpts = get_res_value(res2,"gsnHistogramNptsForPercent",npts)*1.
    if(horizontal) then
      nicevals = nice_mnmxintvl(res2@trXMinF,min((/100., \
                                100.*(res2@trXMaxF/xnpts)/)),7,False)
    else
      nicevals = nice_mnmxintvl(res2@trYMinF,min((/100., \
                                100.*(res2@trYMaxF/xnpts)/)),7,False)
    end if
    nvals = floattoint((nicevals(1) - nicevals(0))/nicevals(2) + 1)
;
; Generate nice values for minor and major percent tick marks. For
; the minor tick marks, just add one tick between each major.
; 
    pnice  = fspan(nicevals(0),nicevals(1),nvals)
    pmnice = fspan(nicevals(0)-nicevals(2)*0.5, \
                   nicevals(1)+nicevals(2)*0.5, 2*nvals+1)
;
; Calculate the bin values that correspond to these percentages.
; These are the values we'll use for the tick marks.
;
    bins_at_pnice  = 0.01 * (xnpts * pnice)
    bins_at_pmnice = 0.01 * (xnpts * pmnice)
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
; 8. Set some post-resources for labeling the other axis, if desired.
;
  axis_string = get_long_name_units_string(xdata)
  if(.not.ismissing(axis_string)) then
    if(horizontal) then
      set_attr(res2,"tiYAxisString",axis_string)
    else
      set_attr(res2,"tiXAxisString",axis_string)
    end if
  else
    if(setinterval) then
      if(horizontal) then
        set_attr(res2,"tiYAxisString","Class Intervals")
      else
        set_attr(res2,"tiXAxisString","Class Intervals")
      end if
    end if
  end if
;
;
; 9. Set some post-resources for labeling the axis that indicates the
;    bin ranges or discrete values.  If minmaxbins is True, then we 
;    need to make sure not to label the end bin locations. Also, if
;    count_msg is True, then we need to add a special label for this.
;
  if(minmaxbins) then
    if(.not.count_msg) then
      lbs             = new(nbinlocs+2,string)
      lbs(0)          = ""
      lbs(1:nbinlocs) = num_in_bins@binlocs
      lbs(nbinlocs+1) = ""
    else
      lbs = new(nbinlocs+3,string)
      lbs(0)          = ""
      lbs(1:nbinlocs) = num_in_bins@binlocs
      lbs(nbinlocs+1) = ""
      lbs(nbinlocs+2) = "# msg"
    end if
  else
    if(.not.count_msg) then
      lbs = num_in_bins@binlocs
    else
      lbs               = new(nbinlocs+1,string)
      lbs(0:nbinlocs-1) = num_in_bins@binlocs
      lbs(nbinlocs)     = "# msg"
    end if
  end if
;
; Calculate location for tickmarks.
;
  nlbs = dimsizes(lbs)
  if(nlbs.gt.1) then
    lbs_vals = fspan(0.,1.,nlbs)
  else
    lbs_vals = 0.0
  end if

  if(count_msg.and.setinterval) then
    dx               = lbs_vals(nlbs-1) - lbs_vals(nlbs-2)
    lbs_vals(nlbs-1) = lbs_vals(nlbs-2) + dx/2.
  end if

  if(horizontal) then
    set_attr(res2,"tiXAxisString","Frequency")
    set_attr(res2,"tmYROn",False)
    set_attr(res2,"tmYLMode","EXPLICIT")
    set_attr(res2,"tmYLValues",lbs_vals)
    set_attr(res2,"tmYLLabels",lbs)
  else
    set_attr(res2,"tiYAxisString","Frequency")
    set_attr(res2,"tmXTOn",False)
    set_attr(res2,"tmXBMode","EXPLICIT")
    set_attr(res2,"tmXBValues",lbs_vals)
    set_attr(res2,"tmXBLabels",lbs)
  end if
;
; 10. Apply resources set by user.
;
  xyres = get_res_ne(res2,"gs")
  attsetvalues_check(xy,xyres)
;
; 11. Calculate starting locations of each bar.
;     If comparing two fields, then the second set of bars will be
;     slightly to the right (or above for horizontal bars) of the
;     first set of bars.
;
;     If doing side-by-side bars, then each new set of bars
;     will start to the right of the previous bar. (This may
;     change in future to allow some white space.)
;
  if(sidebyside) then 
    bar_locs      = new((/nbars_per_bin,nbars/),float)
    bar_locs(0,:) = lft_margin + ispan(0,nbars-1,1)*bin_width
    do i=1,nbars_per_bin-1
      bar_locs(i,:) = bar_locs(0,:) + i*(bar_width+bar_spcng)
    end do
  else
    if(setdiscrete) then
      if(compare) then
        bar_locs      = new((/2,nbars/),float)
        bar_locs(0,:) = -.5*bar_width + bin_width * ispan(0,nbars-1,1)
        bar_locs(1,:) = -.166667*bar_width + bin_width * ispan(0,nbars-1,1)
      else
        bar_locs = new((/1,nbars/),float)
        bar_locs = -.5*bar_width + bin_width * ispan(0,nbars-1,1)
      end if
    else
      if(compare) then
        bar_locs      = new((/2,nbars/),float)
        bar_locs(0,:) = .5*(bin_width-(bar_width+0.3333*bar_width)) + \
                         bin_width*ispan(0,nbars-1,1)
        bar_locs(1,:) = bar_locs(0,:) + 0.3333*bar_width
      else
        bar_locs = new((/1,nbars/),float)
        bar_locs = .5*(bin_width-bar_width) + bin_width*ispan(0,nbars-1,1)
      end if
    end if
  end if
;
; 12. Add a right Y (or top X axis) "Percent" label.
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

    perc_string = "Percent"
    if(perc_nomiss) then
      perc_string = perc_string + " (missing values not counted)"
    else
      if(num_in_bins@NumMissing(0).gt.1.or. \
        (compare.and.num_in_bins@NumMissing(1).gt.1)) then
        perc_string = perc_string + " (missing values counted)"
      end if
    end if

    hist_axis_string = create "axis_string" textItemClass wks
      "txString"      : perc_string
      "txFontHeightF" : font_height
      "txAngleF"      : txangle
    end create

    anno = NhlAddAnnotation(xy,hist_axis_string)

    setvalues anno
      "amZone"          : 3      ; Just outside plot area
      "amJust"          : "centercenter"
      "amSide"          : txside
      "amParallelPosF"  : 0.5
      "amOrthogonalPosF": 0.03
      "amResizeNotify"  : True     ; Resize if plot resized.
    end setvalues
  end if
;
; 13. Force tickmarks to be the same length, and pointing outward.
;
  tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
  gsnp_point_tickmarks_outward(xy,tmres,-1.,-1.,-1.,-1.,-1.,-1.,True)
  gsnp_uniform_tickmark_labels(xy,tmres,0.)
;
; 14. Create (but don't draw) histogram plot.
;
  gsres = get_res_eq(res2,"gs")          ; Get GraphicStyle resources.
  num_in_bins@horizontal = horizontal    ; horizontal or vertical bars

  histogram = hist_columns(wks,xy,num_in_bins,bar_locs,bar_width, \
              colors,compare,gsres)
;
; 15. Draw and advance frame.
;
  draw_and_frame(wks,histogram,calldraw,callframe,0,maxbb)
;
; 16. Return histogram and the values.
;
; Return begin, mid, and end point location of each bar.
;
  if(compare.or.sidebyside) then
    histogram@BeginBarLocs  = bar_locs
    histogram@MidBarLocs    = bar_locs + 0.5*bar_width
    histogram@EndBarLocs    = bar_locs + bar_width
  else
    histogram@BeginBarLocs  = bar_locs(0,:)
    histogram@MidBarLocs    = bar_locs(0,:) + 0.5*bar_width
    histogram@EndBarLocs    = bar_locs(0,:) + bar_width
  end if
  histogram@BinLocs       = num_in_bins@binlocs
  histogram@NumInBins     = num_in_bins
  histogram@NumMissing    = num_in_bins@NumMissing
  histogram@Percentages   = num_in_bins@percentages
  histogram@PercentagesNoMissing = num_in_bins@percentages_nm
  return(histogram)
end

;***********************************************************************;
; Function : gsn_contour                                                ;
;                   wks: workstation object                             ;
;                  data: 1 or 2-dimensional data                        ;
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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_contour")
function gsn_contour(wks:graphic, data:numeric, resources:logical )
local i, data_object, plot_object, res, sf_res_index, \
datares, cnres, llres, cn_res_index, ll_res_index, calldraw, callframe, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, res2, scale, shape, sprdcols, \
is_lb_mode, lb_mode
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_contour: Fatal: the input data array must be 1D or 2D")
      return
    end if

    cnres     = False
    llres     = False
    res2      = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),"gsn_contour",res2,1)
    end if

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = scalar_field(wksname+"_data",data,res2)

    sfres = get_res_eq(res2,"sf")
    attsetvalues_check(data_object,sfres)

;
; Create plot object. Make sure you set the tension values (if
; any) when you create the plot. This works better than setting
; them later.
;
    xtension  = get_res_value(res2,"trXTensionF", 2.0)
    ytension  = get_res_value(res2,"trYTensionF", 2.0)

;
; Temporarily (I hope), I need to check if trGridType is being set.
; If so, it needs to be set during the create call, or otherwise it
; will cause the tickmarks to possibly not appear.
; pmLabelBarDisplayMode is another resource that can only be set at
; create time. Too bad it has to be so clunky.
;
    is_lb_mode = isatt(res2,"pmLabelBarDisplayMode")

    if(is_lb_mode) then
      lb_mode = get_display_mode(res2,"pmLabelBarDisplayMode","NoCreate")
      delete(res2@pmLabelBarDisplayMode)
    end if
    if (is_lb_mode) then
      if(res2.and.isatt(res2,"trGridType")) then
        plot_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
        "pmLabelBarDisplayMode" : lb_mode
        "trXTensionF"       : xtension
        "trYTensionF"       : ytension
        "trGridType"        : res2@trGridType
        end create
        delete(res2@trGridType)
      else
        plot_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
        "pmLabelBarDisplayMode" : lb_mode
        "trXTensionF"       : xtension
        "trYTensionF"       : ytension
        end create
      end if
    else
      if(res2.and.isatt(res2,"trGridType")) then
        plot_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
        "trXTensionF"       : xtension
        "trYTensionF"       : ytension
        "trGridType"        : res2@trGridType
        end create
        delete(res2@trGridType)
      else
        plot_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
        "trXTensionF"       : xtension
        "trYTensionF"       : ytension
        end create
      end if
    end if

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
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"cnSpanFillPalette")) then
        res2@cnSpanFillPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    if(res2.and.isatt(res2,"gsnContourZeroLineThicknessF")) then
      zthickness = res2@gsnContourZeroLineThicknessF
      if(.not.get_res_value_keep(res2,"cnMonoLineThickness",True).and.\
         isatt(res2,"cnLineThicknesses")) then
        cthickness = get_res_value(res2,"cnLineThicknesses",1.)
      else
        cthickness = get_res_value(res2,"cnLineThicknessF",1.)
      end if
      delete(res2@gsnContourZeroLineThicknessF)
    end if

    if(res2.and.isatt(res2,"gsnContourLineThicknessesScale")) then
      linescale = res2@gsnContourLineThicknessesScale
      delete(res2@gsnContourLineThicknessesScale)
    end if

    if(res2.and.isatt(res2,"gsnContourNegLineDashPattern")) then
      npattern = res2@gsnContourNegLineDashPattern
      delete(res2@gsnContourNegLineDashPattern)
    else
      npattern = new(1,integer)    ; Set to missing
    end if

    if(res2.and.isatt(res2,"gsnContourPosLineDashPattern")) then
      ppattern = res2@gsnContourPosLineDashPattern
      delete(res2@gsnContourPosLineDashPattern)
    else
      ppattern = new(1,integer)    ; Set to missing
    end if

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    cnres = get_res_ne(res2,"sf")

;
; Don't let pmTickMarkDisplayMode be set after the fact if we are
; going to overlay this plot later, because you might get an error
; message about warning:PlotManagerSetValues: TickMark annotation 
; cannot be added after NhlCreate.
; Also any tr[X/Y][Max/Min]F resources that go outside the data boundaries
; must be temporarily removed because they are not permitted when using the
; irregular transformation. However, do not remove them if they are inside
; the boundaries or there will be an error later when the data boundaries 
; are checked
;
    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log) then
      getvalues plot_object
      "trXMinF"    : trxmin
      "trXMaxF"    : trxmax
      "trYMinF"    : trymin
      "trYMaxF"    : trymax
      end getvalues
      delete_attr(cnres,"pmTickMarkDisplayMode")
      if (force_x_linear .or. force_x_log) then
        if(isatt(cnres,"trXMinF")) then
          if (cnres@trXMinF .lt. trxmin) then
            delete(cnres@trXMinF)
          end if
        end if
        if(isatt(cnres,"trXMaxF")) then
          if (cnres@trXMaxF .gt. trxmax) then
            delete(cnres@trXMaxF)
          end if
        end if
      end if
      if (force_y_linear .or. force_y_log) then
        if(isatt(cnres,"trYMinF")) then
          if (cnres@trYMinF .lt. trymin) then
            delete(cnres@trYMinF)
          end if
        end if
        if(isatt(cnres,"trYMaxF")) then
          if (cnres@trYMaxF .gt. trymax) then
            delete(cnres@trYMaxF)
          end if
        end if
      end if
      llres = get_res_eq(res2,(/"pm","tr","vp"/))
    end if

    attsetvalues_check(plot_object,cnres)

    if(isvar("zthickness")) then
      plot_object = set_zero_line_thickness(plot_object,zthickness,cthickness)
      delete(zthickness)
    end if

    if(isvar("linescale")) then
      plot_object = set_line_thickness_scale(plot_object,linescale)
      delete(linescale)
    end if

    if(.not.ismissing(npattern).or..not.ismissing(ppattern)) then
      plot_object = set_pos_neg_line_pattern(plot_object,ppattern,npattern)
    end if

    if(sprdcols.and..not.isatt(res2,"cnFillColors")) then
      cnres2 = True
      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,plot_object,min_index,max_index,res2))
      attsetvalues(plot_object,cnres2)
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
      gsnp_scale_plot(plot_object,"sf",resources)
    end if
;
; Check if we need to force the X or Y axis to be linear or log.
;
    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      plot_object = overlay_irregular(wks,wksname,overlay_plot_object,\
                                      data_object,force_x_linear,\
                                      force_y_linear,force_x_log, \
                                      force_y_log,"contour",llres)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_contour_map                                            ;
;                   wks: workstation object                             ;
;                  data: 1 or 2-dimensional data                        ;
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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_contour_map")
function gsn_contour_map(wks:graphic,data:numeric,\
                         resources:logical)
local i, data_object, contour_object, res, sf_res_index, \
cn_res_index, mp_res_index, map_object, res2, scale, shape, sprdcols
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_contour_map: Fatal: the input data array must be 1D or 2D")
      return
    end if

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),"gsn_contour_map",res2,1)
    end if

    wksname = get_res_value_keep(wks,"name","gsnapp")

; Create contour plot.

    cnres          = get_res_eq(res2,(/"sf","tr"/))
    cnres          = True
    cnres@gsnDraw  = False
    cnres@gsnFrame = False
    contour_object = gsn_contour(wks,data,cnres)
    delete(cnres)

; Check for existence of data@long_name and use it in a title it
; it exists.

    if(isatt(data,"long_name")) then
      set_attr(res2,"tiMainString",data@long_name)
    end if

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    scale     = get_res_value(res2,"gsnScale",False)
    shape     = get_res_value(res2,"gsnShape",scale)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"cnSpanFillPalette")) then
        res2@cnSpanFillPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    mpres = get_res_eq(res2,(/"mp","vp","pmA","pmO","pmT","tm"/))
    cnres = get_res_ne(res2,(/"mp","sf","vp"/))

    attsetvalues_check(map_object,mpres)
    attsetvalues_check(contour_object,cnres)
    if(sprdcols.and..not.isatt(res2,"cnFillColors")) then
      cnres2 = True
      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index,res2))
      attsetvalues(contour_object,cnres2)
    end if

    overlay(map_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(contour_object,"sf",resources)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data    = contour_object@data
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
undef("gsn_map")
function gsn_map(wks:graphic, projection:string, resources:logical )
local i, plot_object, res2, res3
begin
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res2,"gsnDebugWriteFileName")) then
    if(.not.isatt(res2,"mpProjection")) then
      res2@mpProjection = "CylindricalEquidistant"
    end if
    gsnp_write_debug_info(new(1,float),new(1,float),new(1,float),"gsn_map", \
                     res2,0)
  end if

; Create plot object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

;
; Check if the user is setting tiMainString. If so, then set
; pmTitleDisplayMode to "Always" (unless the user is also setting
; this himself).  Otherwise, just use the default value of "NoCreate".
;
    if(res2.and.isatt(res2,"tiMainString")) then
      title_display = get_res_value(res2,"pmTitleDisplayMode","Always")
    else
      title_display = get_res_value(res2,"pmTitleDisplayMode","NoCreate")
    end if

    plot_object = create wksname + "_map" mapPlotClass wks
        "mpProjection"       : projection
        "pmTitleDisplayMode" : title_display
    end create

; Check to see if any resources were set.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

    attsetvalues_check(plot_object,res2)

    draw_and_frame(wks,plot_object,calldraw,callframe,0,maxbb)

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
undef("gsn_streamline")
function gsn_streamline(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                        resources:logical)
local i, data_object,plot_object,res,vf_res_index,st_res_index, \
force_x_linear, force_y_linear, force_x_log, force_y_log, stres2, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, res2, scale, shape
begin
    llres = False
    res2  = get_resources(resources)

;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_streamline",res2,2)
    end if

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = vector_field(wksname+"_data",u,v,res2)

; Create plot object.

    plot_object = create wksname + "_stream" streamlinePlotClass wks
        "stVectorFieldData" : data_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)

    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"stSpanLevelPalette")) then
        res2@stSpanLevelPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    vfres = get_res_eq(res2,"vf")
    stres = get_res_ne(res2,"vf")
    attsetvalues_check(data_object,vfres)
    attsetvalues_check(plot_object,stres)

    if(sprdcols.and..not.isatt(res2,"stLevelColors")) then
      stres2 = True
      set_attr(stres2,"stLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index,res2))
      attsetvalues(plot_object,stres2)
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
      gsnp_scale_plot(plot_object,"vf",resources)
    end if

; Check if we need to force the X or Y axis to be linear or log.
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      plot_object = overlay_irregular(wks,wksname,overlay_plot_object,\
                                      data_object,force_x_linear,\
                                      force_y_linear,force_x_log, \
                                      force_y_log,"streamline",llres)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe,0,maxbb)

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
undef("gsn_streamline_map")
function gsn_streamline_map(wks:graphic,u[*][*]:numeric,\
                            v[*][*]:numeric,resources:logical)
local i, data_object, contour_object, res, vf_res_index, \
st_res_index, mp_res_index, map_object, res2, stres2
begin
    res2 = get_resources(resources)

;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_streamline_map",res2,2)
    end if

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = vector_field(wksname+"_data",u,v,res2)

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
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"stSpanLevelPalette")) then
        res2@stSpanLevelPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    vfres = get_res_eq(res2,"vf")
    mpres = get_res_eq(res2,(/"mp","vp","pmA","pmO","pmT","tm"/))
    stres = get_res_ne(res2,(/"vf","mp","vp"/))

    attsetvalues_check(data_object,vfres)
    attsetvalues_check(stream_object,stres)
    attsetvalues_check(map_object,mpres)

    if(.not.isatt(res2,"stLevelColors")) then
      stres2 = True
      set_attr(stres2,"stLevelColors",\
               spread_colors(wks,stream_object,min_index,max_index,res2))
      attsetvalues(stream_object,stres2)
    end if

    overlay(map_object,stream_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(stream_object,"vf",resources)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_draw_colormap")
procedure gsn_draw_colormap(wks)
local nrows, ncols, ncolors, maxcols, ntotal, offset, width, height, \
xpos, ypos, xbox, ybox, cmap, cmapnew
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
  reset_colormap = False
  if(ncolors.lt.maxcols) then
    reset_colormap = True
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
  if(ncols.gt.1) then
    xpos = fspan(0,1-width,ncols)
  else
    xpos = 0.
  end if
  if(nrows.gt.1) then
    ypos = fspan(1-height,0,nrows)
  else
    ypos = 1.-height
  end if
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

  if(reset_colormap) then
;
; Put the original color map back.
;
    setvalues wks
      "wkColorMap" : cmap
    end setvalues
    delete(cmap)
  end if
  return
end

;***********************************************************************;
; Procedure : gsn_draw_named_colors                                     ;
;                   wks: workstation object                             ;
;                   colors: colors array                                ;
;                   box: array defining number of rows and columns      ;
;                                                                       ;
; This procedure takes a named color array and draws it.                ;
; wks is a variable returned from a previous call to "gsn_open_wks".    ;
;***********************************************************************;
undef("gsn_draw_named_colors")
procedure gsn_draw_named_colors(wks:graphic,colors[*]:string, box[2]:integer)
local ncolors, ntotal, nrows, ncols, offset, width, height, \
xpos, ypos, xbox, ybox

begin
  nrows   = box(0)                   ; # of rows of colors per page.
  ncols   = box(1)                   ; # of columns of colors per page.

  if(typeof(colors).ne."string".or.dimsizes(dimsizes(colors)).ne.1)
    print("The input array is not a one dimensional array of named color strings")
    exit
  end if

  if((nrows.gt.16).or.(ncols.gt.8))
    print("Number of rows should be less than 16 and Number of columns should be less than 8.")
    exit
  end if

  ncolors = dimsizes(colors)     ; # of colors given

  getvalues wks
    "wkColorMap" : oldcmap    ; Get current color map.
  end getvalues

;
; Find out the rgb values of each color and put it into
; an array
;
  rgb_values = namedcolor2rgb(colors)
  rgb_val = new(ncolors, string)
  do l =0, ncolors-1, 1
	rgb_val(l) = sprintf("%.2f", rgb_values(l,0)) + "," + \
                     sprintf("%.2f", rgb_values(l,1)) + "," + \
                     sprintf("%.2f", rgb_values(l,2))
  end do

;
; npages number of frames.
;
  ntotal = ncols * nrows

  npages = floattoint(ncolors/ntotal)
  if(npages*ntotal.lt.ncolors)
    npages = npages+1
  end if

;
; X and Y positions of text and box in the view port.
;
  width  = 1./ncols
  height = 1./nrows

  if(ncols.gt.1) then
    xpos = fspan(0,1-width,ncols)
  else
    xpos = 0.
  end if
  if(nrows.gt.1) then
    ypos = fspan(1-height,0,nrows)
  else
    ypos = 1.-height
  end if
;
; Box coordinates.
;
  xbox = (/0,width, width,     0,0/)
  ybox = (/0,    0,height,height,0/)

;
; Calculate font height.
;
  font_heights = (/.0143, .0143, .0143, .0141, .013, .011, .0093, .0085/)
  font_height = font_heights(ncols-1)
  font_space  = font_height/2.

  gonres   = True   ; variables to hold list of resources
  lineres  = True
  txres    = True

  txres@gsnDraw               = True
  txres@txFontHeightF         = font_height
  txres@txFont                = "helvetica-bold"
  txres@txPerimOn             = True
  txres@txPerimColor          = "black"  ; Or close to black if 
  txres@txFontColor           = "black"  ; black is not in color map.
  txres@txBackgroundFillColor = "white"  ; Or close to white.

  lineres@gsLineColor         = "black"

  kk = 0
  offset = 2
  do k = 1,ncolors,ntotal
    start = kk*ntotal
    colorindex = 0
;
; Set the colormap
;
    if(npages.eq.1)
      cmapnew = new(ncolors-start+2,string)
      cmapnew(0)  = "White"     ; white background
      cmapnew(1)  = "Black"     ; black background
      cmapnew(2:) = colors(start:ncolors-1)
    else
      cmapnew = new(ntotal+2,string)
      cmapnew(0)  = "White"     ; white background
      cmapnew(1)  = "Black"     ; black background
      cmapnew(2:) = colors(start:start+ntotal-1)
    end if

    setvalues wks
      "wkColorMap" : cmapnew
    end setvalues
    delete(cmapnew)

    jj = 0
    do j=k,min((/k+ntotal-1,ncolors/)),nrows
      ii = 0
      do i=j,min((/j+nrows-1,ncolors/))

            ; Draw box and fill in the appropriate color.
        gonres@gsFillColor = offset + colorindex
        gsn_polygon_ndc(wks,xbox+xpos(jj),ybox+ypos(ii),gonres) ; Draw box.

            ; Outline box in black.
        gsn_polyline_ndc(wks,xbox+xpos(jj),ybox+ypos(ii),lineres)

            ; Draw color label.
        txres@txJust                = "BottomLeft"
        text = gsn_create_text_ndc(wks,colors(i-1),font_space+xpos(jj),\
                                   ypos(ii)+font_space,txres)
        txres@txJust                = "TopLeft"
        gsn_text_ndc(wks,rgb_val(i-1),font_space+xpos(jj), \
                     height+ypos(ii)-font_space,txres)

        ii = ii + 1
        colorindex = colorindex + 1
      end do
      jj = jj +1
    end do
    kk = kk + 1
    npages = npages - 1
    frame(wks)   ; Advance the frame.
  end do

;
; Put the original color map back.
;
    setvalues wks
      "wkColorMap" : oldcmap
    end setvalues
    delete(oldcmap)

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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_vector")
function gsn_vector(wks:graphic, u[*][*]:numeric, v[*][*]:numeric, \
                    resources:logical )
local i,data_object,plot_object,res,vf_res_index,vc_res_index, \
force_x_linear, force_y_linear, force_x_log, force_y_log, sprdcols, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, res2
begin
    llres = False
    res2  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_vector",res2,2)
    end if

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = vector_field(wksname+"_data",u,v,res2)

; Create plot object.

    plot_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : data_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"vcSpanLevelPalette")) then
        res2@vcSpanLevelPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

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
    if(sprdcols.and..not.isatt(res2,"vcLevelColors")) then
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index,res2))
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
      gsnp_scale_plot(plot_object,"vf",resources)
    end if

; Check if we need to force the X or Y axis to be linear or log.
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      plot_object = overlay_irregular(wks,wksname,overlay_plot_object,\
                                      data_object,force_x_linear,\
                                      force_y_linear,force_x_log, \
                                      force_y_log,"vector",llres)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe,0,maxbb)

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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_vector_contour")
function gsn_vector_contour(wks:graphic,u[*][*]:numeric,\
                            v[*][*]:numeric,data:numeric,\
                            resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, res2
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_vector_contour: Fatal: the input data array must be 1D or 2D")
      return
    end if

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_vector_contour",res2,3)
    end if
;
; Create the scalar and vector field object.
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = vector_field(wksname+"_vfdata",u,v,res2)
    sfdata_object = scalar_field(wksname+"_sfdata",data,res2);

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
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"cnSpanFillPalette")) then
        res2@cnSpanFillPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    if(res2.and.isatt(res2,"gsnContourZeroLineThicknessF")) then
      zthickness = res2@gsnContourZeroLineThicknessF
      if(.not.get_res_value_keep(res2,"cnMonoLineThickness",True).and.\
         isatt(res2,"cnLineThicknesses")) then
        cthickness = get_res_value(res2,"cnLineThicknesses",1.)
      else
        cthickness = get_res_value(res2,"cnLineThicknessF",1.)
      end if
      delete(res2@gsnContourZeroLineThicknessF)
    end if

    if(res2.and.isatt(res2,"gsnContourLineThicknessesScale")) then
      linescale = res2@gsnContourLineThicknessesScale
      delete(res2@gsnContourLineThicknessesScale)
    end if

    if(res2.and.isatt(res2,"gsnContourNegLineDashPattern")) then
      npattern = res2@gsnContourNegLineDashPattern
      delete(res2@gsnContourNegLineDashPattern)
    else
      npattern = new(1,integer)    ; Set to missing
    end if

    if(res2.and.isatt(res2,"gsnContourPosLineDashPattern")) then
      ppattern = res2@gsnContourPosLineDashPattern
      delete(res2@gsnContourPosLineDashPattern)
    else
      ppattern = new(1,integer)    ; Set to missing
    end if

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
;
; Make sure that the labelbar resources get passed to the
; correct object.
;
    if(res2.and.isatt(res2,"cnFillOn").and.res2@cnFillOn) then
      cnres = get_res_eq(res2,(/"cn","tf","lb","pmLabel"/))
      vcres = get_res_ne(res2,(/"vf","sf","cn","lb","pmLabel"/))
    else
      cnres = get_res_eq(res2,(/"cn","tf"/))
      vcres = get_res_ne(res2,(/"vf","sf","cn"/))
    end if
    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(contour_object,cnres)
    attsetvalues_check(vector_object,vcres)

    if(isvar("zthickness")) then
      contour_object = set_zero_line_thickness(contour_object,zthickness,cthickness)
      delete(zthickness)
    end if

    if(isvar("linescale")) then
      contour_object = set_line_thickness_scale(contour_object,linescale)
      delete(linescale)
    end if

    if(.not.ismissing(npattern).or..not.ismissing(ppattern)) then
      contour_object = set_pos_neg_line_pattern(contour_object, \
                                                ppattern,npattern)
    end if

    if(sprdcols)
      if(.not.isatt(res2,"cnFillColors")) then
        cnres2 = True
        set_attr(cnres2,"cnFillColors",\
                 spread_colors(wks,contour_object,min_index,max_index,res2))
        attsetvalues(contour_object,cnres2)
      end if
      if(.not.isatt(res2,"vcLevelColors")) then
        vcres2 = True
        set_attr(vcres2,"vcLevelColors",\
                 spread_colors(wks,vector_object,min_index,max_index,res2))
        attsetvalues(vector_object,vcres2)
      end if
    end if

    overlay(contour_object,vector_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(contour_object,"vf",resources)
    end if

;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(contour_object)
    end if

    draw_and_frame(wks,contour_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    contour_object@vfdata = vfdata_object
    contour_object@sfdata = sfdata_object
    contour_object@vector = vector_object
    return(contour_object)
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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_streamline_contour")
function gsn_streamline_contour(wks:graphic,u[*][*]:numeric,\
                                v[*][*]:numeric,data:numeric,\
                                resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, st_res_index, sf_res_index, res2
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_streamline_contour: Fatal: the input data array must be 1D or 2D")
      return
    end if

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_streamline_contour",res2,3)
    end if
;
; Create the vector and scalar fields.
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = vector_field(wksname+"_vfdata",u,v,res2)
    sfdata_object = scalar_field(wksname+"_sfdata",data,res2);

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
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"cnSpanFillPalette")) then
        res2@cnSpanFillPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    if(res2.and.isatt(res2,"gsnContourZeroLineThicknessF")) then
      zthickness = res2@gsnContourZeroLineThicknessF
      if(.not.get_res_value_keep(res2,"cnMonoLineThickness",True).and.\
         isatt(res2,"cnLineThicknesses")) then
        cthickness = get_res_value(res2,"cnLineThicknesses",1.)
      else
        cthickness = get_res_value(res2,"cnLineThicknessF",1.)
      end if
      delete(res2@gsnContourZeroLineThicknessF)
    end if

    if(res2.and.isatt(res2,"gsnContourLineThicknessesScale")) then
      linescale = res2@gsnContourLineThicknessesScale
      delete(res2@gsnContourLineThicknessesScale)
    end if

    if(res2.and.isatt(res2,"gsnContourNegLineDashPattern")) then
      npattern = res2@gsnContourNegLineDashPattern
      delete(res2@gsnContourNegLineDashPattern)
    else
      npattern = new(1,integer)    ; Set to missing
    end if

    if(res2.and.isatt(res2,"gsnContourPosLineDashPattern")) then
      ppattern = res2@gsnContourPosLineDashPattern
      delete(res2@gsnContourPosLineDashPattern)
    else
      ppattern = new(1,integer)    ; Set to missing
    end if

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    cnres = get_res_eq(res2,(/"cn"/))
    stres = get_res_ne(res2,(/"vf","sf","cn"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(contour_object,cnres)
    attsetvalues_check(stream_object,stres)

    if(isvar("zthickness")) then
      contour_object = set_zero_line_thickness(contour_object,zthickness,cthickness)
      delete(zthickness)
    end if

    if(isvar("linescale")) then
      contour_object = set_line_thickness_scale(contour_object,linescale)
      delete(linescale)
    end if

    if(.not.ismissing(npattern).or..not.ismissing(ppattern)) then
      contour_object = set_pos_neg_line_pattern(contour_object, \
                                                ppattern,npattern)
    end if

     if(.not.isatt(res2,"cnFillColors")) then
      cnres2 = True

      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index,res2))

      attsetvalues(contour_object,cnres2)
    end if

    overlay(stream_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(stream_object,"vf",resources)
    end if

    draw_and_frame(wks,stream_object,calldraw,callframe,0,maxbb)

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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_vector_contour_map")
function gsn_vector_contour_map(wks:graphic,u[*][*]:numeric,\
                               v[*][*]:numeric,data:numeric,\
                               resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, mp_res_index, map_object, res2
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_vector_contour_map: Fatal: the input data array must be 1D or 2D")
      return
    end if

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_vector_contour_map",res2,3)
    end if
;
; Create the vector and scalar field objects.
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = vector_field(wksname+"_vfdata",u,v,res2)
    sfdata_object = scalar_field(wksname+"_sfdata",data,res2);

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
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"cnSpanFillPalette")) then
        res2@cnSpanFillPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    if(res2.and.isatt(res2,"gsnContourZeroLineThicknessF")) then
      zthickness = res2@gsnContourZeroLineThicknessF
      if(.not.get_res_value_keep(res2,"cnMonoLineThickness",True).and.\
         isatt(res2,"cnLineThicknesses")) then
        cthickness = get_res_value(res2,"cnLineThicknesses",1.)
      else
        cthickness = get_res_value(res2,"cnLineThicknessF",1.)
      end if
      delete(res2@gsnContourZeroLineThicknessF)
    end if

    if(res2.and.isatt(res2,"gsnContourLineThicknessesScale")) then
      linescale = res2@gsnContourLineThicknessesScale
      delete(res2@gsnContourLineThicknessesScale)
    end if

    if(res2.and.isatt(res2,"gsnContourNegLineDashPattern")) then
      npattern = res2@gsnContourNegLineDashPattern
      delete(res2@gsnContourNegLineDashPattern)
    else
      npattern = new(1,integer)    ; Set to missing
    end if

    if(res2.and.isatt(res2,"gsnContourPosLineDashPattern")) then
      ppattern = res2@gsnContourPosLineDashPattern
      delete(res2@gsnContourPosLineDashPattern)
    else
      ppattern = new(1,integer)    ; Set to missing
    end if

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    cnres = get_res_eq(res2,(/"cn","tf"/))
    mpres = get_res_eq(res2,(/"mp","vp","pmA","pmO","pmT","tm"/))
    vcres = get_res_ne(res2,(/"cn","mp","sf","vf","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(map_object,mpres)
    attsetvalues_check(contour_object,cnres)
    attsetvalues_check(vector_object,vcres)

    if(isvar("zthickness")) then
      contour_object = set_zero_line_thickness(contour_object,zthickness,cthickness)
      delete(zthickness)
    end if

    if(isvar("linescale")) then
      contour_object = set_line_thickness_scale(contour_object,linescale)
      delete(linescale)
    end if

    if(.not.ismissing(npattern).or..not.ismissing(ppattern)) then
      contour_object = set_pos_neg_line_pattern(contour_object, \
                                                ppattern,npattern)
    end if

    if(sprdcols) then
     if(.not.isatt(res2,"cnFillColors")) then
        cnres2 = True
        set_attr(cnres2,"cnFillColors",\
                 spread_colors(wks,contour_object,min_index,max_index,res2))
        attsetvalues(contour_object,cnres2)
     end if
     if(.not.isatt(res2,"vcLevelColors")) then
        vcres2 = True

        set_attr(vcres2,"vcLevelColors",\
                 spread_colors(wks,vector_object,min_index,max_index,res2))

        attsetvalues(vector_object,vcres2)
      end if
    end if

    overlay(map_object,contour_object)
    overlay(map_object,vector_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object,"vf",resources)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_vector_map")
function gsn_vector_map(wks:graphic, u[*][*]:numeric, v[*][*]:numeric, \
                        resources:logical )
local i, data_object, contour_object, res, vf_res_index, \
vc_res_index, mp_res_index, map_object, res2, sprdcols
begin
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_vector_map",res2,2)
    end if

; Create the data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    data_object = vector_field(wksname+"_data",u,v,res2)

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
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"vcSpanLevelPalette")) then
        res2@vcSpanLevelPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    vfres = get_res_eq(res2,"vf")
    mpres = get_res_eq(res2,(/"mp","vp","pmA","pmO","pmT","tm"/))
    vcres = get_res_ne(res2,(/"mp","vf","vp"/))

    attsetvalues_check(data_object,vfres)
    attsetvalues_check(map_object,mpres)
    attsetvalues_check(vector_object,vcres)

    if(.not.isatt(res2,"vcLevelColors")) then
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index,res2))
      attsetvalues(vector_object,vcres2)
    end if

    overlay(map_object,vector_object)
;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object,"vf",resources)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_vector_scalar")
function gsn_vector_scalar(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                           data:numeric, resources:logical )
local i, vfdata_object, sfdata_object, plot_object, res, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, vf_res_index, \
vc_res_index, sf_res_index, res2, sprdcols
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_vector_scalar: Fatal: the input data array must be 1D or 2D")
      return
    end if

    llres = False
    res2  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_vector_scalar",res2,3)
    end if

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the scalar and vector field data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = vector_field(wksname+"_vfdata",u,v,res2)
    sfdata_object = scalar_field(wksname+"_sfdata",data,res2);

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
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"vcSpanLevelPalette")) then
        res2@vcSpanLevelPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
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
    if(sprdcols.and..not.isatt(res2,"vcLevelColors")) then
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index,res2))
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
      gsnp_scale_plot(plot_object,"vf",resources)
    end if

; Check if we need to force the X or Y axis to be linear or log.
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      plot_object = overlay_irregular(wks,wksname,overlay_plot_object,\
                                      data_object,force_x_linear,\
                                      force_y_linear,force_x_log, \
                                      force_y_log,"vector",llres)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    plot_object@vfdata = vfdata_object
    plot_object@sfdata = sfdata_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_streamline_scalar                                      ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot to the workstation  ;
; "wks"; (the variable returned from a previous call to "gsn_open_wks").;
; "u" and "v" are the 2-dimensional vector arrays, and "data" is the    ;
; scalar field that the streamlines are colored by. "resources" is an   ;
; optional list of resources. The id of the streamline plot is returned.;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_streamline_scalar")
function gsn_streamline_scalar(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                           data:numeric, resources:logical )
local i, vfdata_object, sfdata_object, plot_object, res, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, vf_res_index, \
st_res_index, sf_res_index, res2, sprdcols
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_streamline_scalar: Fatal: the input data array must be 1D or 2D")
      return
    end if

    llres = False
    res2  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_streamline_scalar",res2,3)
    end if

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the scalar and vector field data object.

    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = vector_field(wksname+"_vfdata",u,v,res2)
    sfdata_object = scalar_field(wksname+"_sfdata",data,res2);

; Create plot object.

    plot_object = create wksname + "_streamline" streamlinePlotClass wks
        "stVectorFieldData"     : vfdata_object
        "stScalarFieldData"     : sfdata_object
        "stUseScalarArray"      : True
        "stMonoLineColor"       : False
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"stSpanLevelPalette")) then
        res2@stSpanLevelPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    stres = get_res_ne(res2,(/"sf","vf"/))

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      llres = get_res_eq(res2,(/"tr","vp"/))
    end if

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(plot_object,stres)
    if(sprdcols.and..not.isatt(res2,"stLevelColors")) then
      stres2 = True
      set_attr(stres2,"stLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index,res2))
      attsetvalues(plot_object,stres2)
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
      gsnp_scale_plot(plot_object,"vf",resources)
    end if

; Check if we need to force the X or Y axis to be linear or log.
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      plot_object = overlay_irregular(wks,wksname,overlay_plot_object,\
                                      data_object,force_x_linear,\
                                      force_y_linear,force_x_log, \
                                      force_y_log,"vector",llres)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe,0,maxbb)

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
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_vector_scalar_map")
function gsn_vector_scalar_map(wks:graphic,u[*][*]:numeric,\
                               v[*][*]:numeric,data:numeric,\
                               resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, mp_res_index, map_object, res2, \
sprdcols
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_vector_scalar_map: Fatal: the input data array must be 1D or 2D")
      return
    end if

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_vector_scalar_map",res2,3)
    end if

;
; Create the vector and scalar field object.
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = vector_field(wksname+"_vfdata",u,v,res2)
    sfdata_object = scalar_field(wksname+"_sfdata",data,res2);

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
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"vcSpanLevelPalette")) then
        res2@vcSpanLevelPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    mpres = get_res_eq(res2,(/"mp","vp","pmA","pmO","pmT","tm"/))
    vcres = get_res_ne(res2,(/"mp","sf","vf","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(map_object,mpres)
    attsetvalues_check(vector_object,vcres)

    if(sprdcols.and..not.isatt(res2,"vcLevelColors")) then
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index,res2))
      attsetvalues(vector_object,vcres2)
    end if

    overlay(map_object,vector_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object,"vf",resources)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata = vfdata_object
    map_object@sfdata = sfdata_object
    map_object@vector = vector_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_streamline_scalar_map                                  ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot over a map plot to  ; 
; the workstation "wks" (the variable returned from a previous call to  ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional streamline arrays,;
; and "data" is the scalar field that the streamlines are colored by.   ;
; "resources" is an optional list of resources. The id of the map plot  ; 
; is returned.                                                          ;
;                                                                       ;
; Special resources ("gsn" prefix) allowed:                             ;
;                                                                       ;
;   gsnDraw                                                             ;
;   gsnFrame                                                            ;
;   gsnShape                                                            ;
;   gsnScale                                                            ;
;   gsnSpreadColors - deprecated as of NCL V6.1.0. Only used if         ;
;     gsnSpreadColorStart and/or gsnSpreadColorEnd are set to something ;
;     other than 2 and -1.                                              ;
;   gsnSpreadColorStart                                                 ;
;   gsnSpreadColorEnd                                                   ;
;                                                                       ;
;***********************************************************************;
undef("gsn_streamline_scalar_map")
function gsn_streamline_scalar_map(wks:graphic,u[*][*]:numeric,\
                                   v[*][*]:numeric,data:numeric,\
                                   resources:logical)
local i, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, st_res_index, sf_res_index, mp_res_index, map_object, res2, \
sprdcols
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_streamline_scalar_map: Fatal: the input data array must be 1D or 2D")
      return
    end if

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_streamline_scalar_map",res2,3)
    end if

;
; Create the streamline and scalar field object.
;
    wksname = get_res_value_keep(wks,"name","gsnapp")

    vfdata_object = vector_field(wksname+"_vfdata",u,v,res2)
    sfdata_object = scalar_field(wksname+"_sfdata",data,res2);

; Create plot object.

    streamline_object = create wksname + "_streamline" streamlinePlotClass wks
        "stVectorFieldData"     : vfdata_object
        "stScalarFieldData"     : sfdata_object
        "stUseScalarArray"      : True
        "stMonoLineColor"       : False
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)
    if(min_index.ne.2.or.max_index.ne.-1.) then
      sprdcols = get_res_value(res2,"gsnSpreadColors",True)
    else
      if(check_attr(res2,"gsnSpreadColors",False,True).and..not.\
        isatt(res2,"stSpanLevelPalette")) then
        res2@stSpanLevelPalette = False
      end if
      sprdcols = get_res_value(res2,"gsnSpreadColors",False)
    end if

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    mpres = get_res_eq(res2,(/"mp","vp","pmA","pmO","pmT","tm"/))
    stres = get_res_ne(res2,(/"mp","sf","vf","vp"/))

    attsetvalues_check(vfdata_object,vfres)
    attsetvalues_check(sfdata_object,sfres)
    attsetvalues_check(map_object,mpres)
    attsetvalues_check(streamline_object,stres)

    if(sprdcols.and..not.isatt(res2,"stLevelColors")) then
      stres2 = True
      set_attr(stres2,"stLevelColors",\
               spread_colors(wks,streamline_object,min_index,max_index,res2))
      attsetvalues(streamline_object,stres2)
    end if

    overlay(map_object,streamline_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(streamline_object,"vf",resources)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata = vfdata_object
    map_object@sfdata = sfdata_object
    map_object@streamline = streamline_object
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
undef("gsn_xy")
function gsn_xy(wks:graphic, x:numeric, y:numeric, resources:logical )
local i, attnames, data_object, plot_object, res, ca_res_index, \
xy_res_index, xydp_res_index, dspec, res2, set_dash
begin
    set_dash = True       ; Default is to set some dash patterns.
    res2     = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(x,y,new(1,float),"gsn_xy",res2,2)
    end if

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
            "caXMissingV" : x@_FillValue
        end setvalues
    end if
    if(isatt(y,"_FillValue")) then
        setvalues data_object
            "caYMissingV" : y@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_xy" xyPlotClass wks
        "xyCoordData" : data_object
    end create
;
; I'm guessing that we can't set the tr* resources when we create
; the XY plot because it probably affects other resources.  So, we go ahead
; and create the full plot, and *then* we set the tr* resources, if any.
;  
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

; Check for existence of x/y@long_name/units and use them to
; label X and Y axes.

    xaxis_string = get_long_name_units_string(x)
    yaxis_string = get_long_name_units_string(y)

    if(.not.ismissing(xaxis_string)) then
      set_attr(res2,"tiXAxisString",xaxis_string)
    end if
    if(.not.ismissing(yaxis_string)) then
      set_attr(res2,"tiYAxisString",yaxis_string)
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
        res = stringtocharacter(attnames(ind(attnames.ne."_FillValue")))

;***********************************************************************;
; Check to see if any xy plot resources were set.  There are two kinds  ;
; of xy plot resources, the regular kind, and the data spec kind.  If   ;
; the resource starts with an "xy", it could be either kind, so we need ;
; to have some tests to see which object it belongs to.  Any "xy"       ;
; resources that start with "xyC", "xyX", or "xyY" are regular          ;
; resources (meaning, it belongs to the XyPlot object). The remaining   ;
; "xy" resources belong to the data spec object. Any resources that do  ;
; not start with "xy" or "ca" are assumed to also go with the XyPlot    ;
; object.                                                               ;
;***********************************************************************;
        if(dimsizes(dimsizes(res)).eq.1)
            if((charactertostring(res(0:1)).ne."ca".and.\
                charactertostring(res(0:1)).ne."xy").or.\
               (charactertostring(res(0:1)).eq."xy".and.\
               (charactertostring(res(0:2)).eq."xyC".or.\
                charactertostring(res(0:2)).eq."xyX".or.\
                charactertostring(res(0:2)).eq."xyY")))
                setvalues plot_object
                    attnames : res2@$attnames$
                end setvalues
            end if
            if(charactertostring(res(0:1)).eq."xy".and.\
              (charactertostring(res(0:2)).ne."xyC".and.\
               charactertostring(res(0:2)).ne."xyX".and.\
               charactertostring(res(0:2)).ne."xyY"))
                setvalues dspec
                    attnames : res2@$attnames$
                end setvalues
            end if
        else
            xy_res_index = ind((charactertostring(res(:,0:1)).ne."ca".and.\
                                charactertostring(res(:,0:1)).ne."xy").or.\
                               (charactertostring(res(:,0:1)).eq."xy".and.\
                               (charactertostring(res(:,0:2)).eq."xyC".or.\
                                charactertostring(res(:,0:2)).eq."xyX".or.\
                                charactertostring(res(:,0:2)).eq."xyY")))
            xydp_res_index = ind(charactertostring(res(:,0:1)).eq."xy".and.\
                                (charactertostring(res(:,0:2)).ne."xyC".and.\
                                 charactertostring(res(:,0:2)).ne."xyX".and.\
                                 charactertostring(res(:,0:2)).ne."xyY"))
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
      gsnp_scale_plot(plot_object,"",False)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    plot_object@data     = data_object
    plot_object@dataspec = dspec
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_y                                                      ;
;                   wks: workstation object                             ;
;                     y: n-dimensional array of Y array                 ;
;             resources: optional resources                             ;
;                                                                       ;
; This function is similar to gsn_xy, except instead of a specific X    ;
; array, index values are used.                                         ;
;                                                                       ;
;***********************************************************************;
undef("gsn_y")
function gsn_y(wks:graphic, y:numeric, resources:logical )
local dsizes_y, npts, x, rank_y, xy
begin
  res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res2,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(y,new(1,float),new(1,float),"gsn_y",res2,1)
  end if
;
; Get dimension sizes of Y array.
;
  dsizes_y = dimsizes(y)
  rank_y   = dimsizes(dsizes_y)
  if(rank_y.eq.1) then
    npts = dsizes_y
  else
    if(rank_y.ne.2) then
      print("Error: gsn_y: The input Y array must either be 1-dimensional, or 2-dimensional, where the leftmost dimension represents the number of curves and the rightmost dimension the number of points in each curve.")
      exit
    end if
    npts = dsizes_y(1) 
  end if

; 
; Create the indexed X array.
;
  x = ispan(0,npts-1,1)
  x@long_name = ""

;
; Call gsn_xy.
;
  xy = gsn_xy(wks,x,y,res2)
  return(xy)
end


;***********************************************************************;
; Function : gsn_contour_shade                                          ;
;                                                                       ;
; This function shades contour regions given low and/or high values     ;
; using colors or patterns.                                             ;
;                                                                       ;
; This function was written by Adam Phillips, 2006                      ;
;                                                                       ;
; This function was updated July 2013 to allow any combination of       ;
; gsnShadeLow, gsnShadeMid, and gsnShadeHigh to be set.                 ;
;                                                                       ;
; This function was updated July 2014 to allow RGB and RGBA values for  ;
; color. The following are now valid for colors:                        ;    
;                                                                       ;
;  Color index value:  opt@gsnShadeLow  = 5                             ;
;  Named color:        opt@gsnShadeHigh = "blue"                        ;
;  RGB color:          opt@gsnShadeHigh = (/1.,0.,0.5/)                 ;
;  RGBA color:         opt@gsnShadeHigh = (/1.,0.,0.5,0.5/)             ;
;***********************************************************************;
undef("gsn_contour_shade")
function gsn_contour_shade(plot:graphic,lowval:numeric,highval:numeric,\
                           opt:logical) 
local shaden_set, shadem_set, shadep_set, shaden, shadep, shadem, ovrly_ids, \
idstringcnlvls, colist, i, N, tmp_wks
begin
   if (.not.opt) then
     print("gsn_contour_shade: Options list must be used as one of the following option resources must be set: opt@gsnShadeLow, opt@gsnShadeHigh, opt@gsnShadeMid.")
     print("   Returning without making any changes to plot.")
     return(plot)
   end if

   shade_type = str_lower(get_res_value_keep(opt,"gsnShadeFillType","color"))   ; "pattern"

   if(.not.any(shade_type.eq.(/"color","pattern"/))) then
     print("gsn_contour_shade: gsnShadeFillType can only be 'pattern' or 'color'.")
     print("   Returning without making any changes to plot.")
     return(plot)
   end if

;----------------------------------------------------------------------
; This section is used to retrieve the gsnShadeLow/Mid/High resources
; and make sure they are valid.
;
; If doing color fill, you can mix and match color types (index color,
; named color, rgb, rgba).
;
; If doing pattern fill, only pattern indexes can be used.
;----------------------------------------------------------------------
   tmp_wks = NhlGetParentWorkstation(plot)
   if (isatt(opt,"gsnShadeLow")) then
     shaden_set = True
     if(shade_type.eq."color") then
       shaden = rm_single_dims_no_meta(convert_color_to_rgba(tmp_wks,opt@gsnShadeLow))
     else
       shaden      = opt@gsnShadeLow
       shaden_type = typeof(shaden)
     end if
   else
     shaden_set = False
     if(shade_type.eq."color") then
       shaden      = (/0.,0.,0.,0./)  ; transparent
       shaden_type = "rgba"
     else
       shaden      = -1              ; no fill
       shaden_type = "integer"
     end if
   end if
   if (isatt(opt,"gsnShadeMid")) then
     shadem_set = True
     if(shade_type.eq."color") then
       shadem      = rm_single_dims_no_meta(convert_color_to_rgba(tmp_wks,opt@gsnShadeMid))
       shadem_type = "rgba"
     else
       shadem      = opt@gsnShadeMid
       shadem_type = typeof(shadem)
     end if
   else
     shadem_set = False
     if(shade_type.eq."color") then
       shadem      = (/0.,0.,0.,0./)  ; transparent
       shadem_type = "rgba"
     else
       shadem      = -1               ; no fill
       shadem_type = "integer"
     end if
   end if
   if (isatt(opt,"gsnShadeHigh")) then
     shadep_set = True
     if(shade_type.eq."color") then
       shadep      = rm_single_dims_no_meta(convert_color_to_rgba(tmp_wks,opt@gsnShadeHigh))
       shadep_type = "rgba"
     else
       shadep      = opt@gsnShadeHigh
       shadep_type = typeof(shadep)
     end if
   else
     shadep_set = False
     if(shade_type.eq."color") then
      shadep      = (/0.,0.,0.,0./)  ; transparent
      shadep_type = "rgba"
     else
       shadep      = -1              ; no fill
       shadep_type = "integer"
     end if
   end if

;---Error checking on the gsnShadeLow/Mid/High resources
   if(.not.any((/shaden_set,shadem_set,shadep_set/))) then
     print("gsn_contour_shade: one of the following resources must be set: opt@gsnShadeLow, opt@gsnShadeHigh, opt@gsnShadeMid.")
     print("   Returning without making any changes to plot.")
     return(plot)
   end if

;---Error checking if we have pattern fill.
   if(shade_type.eq."pattern".and.\
      any((/shaden_type,shadem_type,shadep_type/).ne."integer")) then
     print("gsn_contour_shade: You must use integer values when doing pattern fill.")
     print("   Returning without making any changes to plot.")
     return(plot)
   end if

   if(shade_type.eq."color".and.any((/all(ismissing(shaden)),\
         all(ismissing(shadem)),all(ismissing(shadep))/))) then
     print("gsn_contour_shade: One of your gsnShadeLow/Mid/High resources is set to an invalid color.")
     print("   Returning without making any changes to plot.")
     return(plot)
   end if

   getvalues plot
      "pmOverlaySequenceIds" : ovrly_ids
   end getvalues
   if (.not.any(ismissing(ovrly_ids))) then
     do i=0,dimsizes(ovrly_ids)-1
         if (NhlClassName(ovrly_ids(i)).eq."contourPlotClass")
            idstring = ovrly_ids(i)
	    end if
      end do
   end if 
   getvalues idstring
     "cnLevels"  : cnlvls
   end getvalues

   if ((isatt(opt,"printcnlevels"))) then
      if (opt@printcnlevels) then
	    print(cnlvls)
	 end if
   end if

;---Make sure we have contour levels
   N = dimsizes(cnlvls)
   if (ismissing(N) .or. N.le.0) then
       print ("gsn_contour_shade: dimsizes(cnlvls)="+N+" return (non-fatal)")
       return (plot)
   end if

;---Create array for fill, set all to transparent or white
   if(shade_type.eq."color") then
     colist      = new((/N+1,4/),double)
     colist      = 1.d     ; initialize to all white so no missing values are used
     colist(:,3) = 0.      ; now set all colors to transparent.
   else
     colist = new(N+1,integer)
     colist = -1
   end if

;---Start filling colist
   if (shaden_set.and.any(cnlvls.le.lowval)) then
     ii := ind(cnlvls.le.lowval)
     ii_dims = dimsizes(ii)
     if(shade_type.eq."pattern") then
       colist(ii) = shaden
     else if(ii_dims.eq.1) then
       colist(ii,:) = shaden
     else
       colist(ii,:) = conform(colist(ii,:),shaden,1)
     end if
     end if
   end if

   if (shadep_set.and.any(cnlvls.ge.highval)) then     
     ii := ind(cnlvls.ge.highval)+1
     ii_dims = dimsizes(ii)
     if(shade_type.eq."pattern") then
       colist(ii) = shadep
     else if(ii_dims.eq.1) then
       colist(ii,:) = shadep
     else
       colist(ii,:) = conform(colist(ii,:),shadep,1)
     end if
   end if
   end if

   if (shadem_set.and.any(cnlvls.ge.lowval.and.cnlvls.le.highval)) then
     ii := ind(cnlvls.ge.lowval.and.cnlvls.le.highval)
     ii_dims = dimsizes(ii)
     if(shade_type.eq."pattern") then
       colist(ii) = shadem
     else if (ii_dims.ge.2) then
       colist(ii(1:),:) = conform(colist(ii(1:),:),shadem,1)
     else
       print("gsn_contour_shade: 1 contour level or less found between "+lowval+" and "+highval+", not color filling")
     end if
     end if
   end if

   if (shade_type.eq."color") then
	 setvalues idstring               
	    "cnFillOn"          : True
            "cnMonoFillPattern" : True
            "cnMonoFillColor"   : False
            "cnFillColors"      : colist			   
	 end setvalues
   else
	 setvalues idstring               
	    "cnFillOn"          : True
            "cnMonoFillColor"   : True
            "cnMonoFillPattern" : False
            "cnFillPatterns"    : colist	   
	 end setvalues
   end if
   return (plot)
end
undef("fill_res")
procedure fill_res(res1:logical,res2:logical,ncr[2]:integer,attnames[*]:string)
local natts, i, dsizes, rank, success, nrows, ncols
begin
  nrows = ncr(0)
  ncols = ncr(1)
  natts = dimsizes(attnames)

  if(res1.and.natts.ge.1) then
    res2 = True
;
; Loop through each attribute, check its size, and copy it to new
; 2D variable if needed. We have to do this so that later we don't
; have to check each attribute for the right size.
;
    do i=0,natts-1
      if(isatt(res1,attnames(i))) then
;
; Get the dimension size and rank of this attribute.
;
        dsizes  = dimsizes(res1@$attnames(i)$)
        rank    = dimsizes(dsizes)
        success = False
;
; We need elseif here!!
;
        if(rank.eq.1) then
          if(dsizes.eq.1) then
;
; Scalar attribute.
;
            res2@$attnames(i)$ = new(nrows*ncols,typeof(res1@$attnames(i)$))
            res2@$attnames(i)$(:) = res1@$attnames(i)$
            success = True
          else
;
; 1D attribute of length nrows (ncols must be 1).
;
            if(dsizes.eq.nrows.and.ncols.eq.1) then
              res2@$attnames(i)$ = new(nrows*ncols,typeof(res1@$attnames(i)$))
              res2@$attnames(i)$(:) = res1@$attnames(i)$
              success = True
            else
;
; 1D attribute of length ncols (nrows must be 1).
;
              if(dsizes.eq.ncols.and.nrows.eq.1) then
                res2@$attnames(i)$ = new(nrows*ncols,typeof(res1@$attnames(i)$))
                res2@$attnames(i)$(:) = res1@$attnames(i)$
                success = True
              end if
            end if
          end if
        else
          if(rank.eq.2.and.dsizes(0).eq.nrows.and.dsizes(1).eq.ncols) then
;
; 2D attribute of size nrows x ncols.
;
            res2@$attnames(i)$ = ndtooned(res1@$attnames(i)$)
            success = True
          end if
        end if
        if(.not.success) then
          print("fill_res: attribute '" + attnames(i) + "' is the wrong size.")
          print("         Not using it.")
        end if
        delete(dsizes)
      end if
    end do
  end if
end

;***********************************************************************;
; Procedure : gsn_table                                                 ;
; This procedure draws a grid given the workstation                     ;
; to draw to, the beginning X and ending Y values (in NDC coords),      ;
; and the number of rows and columns.  Text strings are drawn           ;
; in the center of each cell, if specified.                             ;
;                                                                       ;
;  draw_grid(                                                           ;
;             wks    - workstation value returned from gsn_open_wks.    ;
;             ncr[2] - integers, number of rows and columns             ;
;             x      - begin and end values of x position of table      ;
;             y      - begin and end values of y position of table      ;
;             text   - optional list of text strings. Use:              ;
;                          text = new(1,string)                         ;
;                      if you don't want any text strings.              ;
;             res    - optional list of "gs" (for the table lines)      ;
;                      or "tx" (for the text) resources.                ;
;          )                                                            ;
;***********************************************************************;
undef("gsn_table")
procedure gsn_table(wks:graphic,ncr[2]:integer,x[2]:numeric, \
                         y[2]:numeric,text:string,res:logical)
local nrows, ncols, i, ii, txres, txres2, lnres, attnames, natts, text2d
begin
  debug = get_res_value(res,"gsnDebug",False)

  nrows = ncr(0)
  ncols = ncr(1)
;
; Error checking.
;
  if(nrows.lt.1.or.ncols.lt.1) then
     print("gsn_table: nrows and ncols must be >= 1.")
     exit
  end if
  if(any(x.lt.0.or.x.gt.1.or.y.lt.0.or.y.gt.1)) then
     print("gsn_table: the begin and end x and y values must")
     print("           be in the range [0,1].")
     exit
  end if

  if(x(1).le.x(0).or.y(1).le.y(0)) then
    print("gsn_table: the begin x,y points must be less")
    print("           than the end x,y points.")
    exit
  end if

;
; Check if text desired.
;
  if(.not.all(ismissing(text))) then
    text_avail = True
  else
    text_avail = False
  end if

  if(text_avail) then
; 
; Check that the text dimens are correct.  If you have nrows x ncols,
; then the text can either be (nrows x ncols) strings, a scalar string,
; or (ncols) if nrows=1, or (nrows) if ncols=1.
;
    dsizes_text = dimsizes(text)
    rank_text   = dimsizes(dsizes_text)
    if( (rank_text.ne.2.and.rank_text.ne.1).or.\
        (rank_text.eq.1.and.(nrows.ne.1.and.ncols.ne.1)).or.\
        (rank_text.eq.1.and.(nrows.eq.1.and.ncols.ne.dsizes_text)).or.\
        (rank_text.eq.1.and.(ncols.eq.1.and.nrows.ne.dsizes_text)).or.\
        (rank_text.ne.2.and.(nrows.gt.1.and.ncols.gt.1)) ) then
      print("gsn_table: the dimensionality of the text must be ")
      print("           " + nrows + " row(s) x " + ncols + " column(s).")
      exit
    end if
  end if

;
; Check all resource values. They must either be scalars, or
; arrays of same size as nrows x ncols.
;
  res2     = False
  attnames = getvaratts(res)
  fill_res(res,res2,ncr,attnames)
  delete(attnames)                    ; We're going to use this later.

;
; Get ready to draw table.
;
  xsize = (x(1) - x(0))/ncols         ; width of grid cell
  ysize = (y(1) - y(0))/nrows         ; height of grid cell

  lnres = get_res_eq(res2,"gs")       ; Resource list for lines.

;
; Check for a box fill color.
;
  fill_on = False
  if(isatt(res2,"gsFillColor").or.isatt(res2,"gsFillIndex")) then
    fill_on = True
  end if

;
; Check for desired filling of each grid cell. Do this before drawing
; grid lines, because we want lines drawn on top of filled boxes.
;
  if(fill_on) then
    gonres = get_res_eq(res2,"gsFill")     ; Get fill resources.
    gonres2 = True
    attnames = getvaratts(gonres)
    natts    = dimsizes(attnames)
    do nr = 0,nrows-1
      ypos = y(1) - ((nr+1) * ysize)
      do nc = 0,ncols-1
        ii = nr*ncols+nc
;
; Copy all resources over to temporary array.
;
        do i=0,natts-1
          gonres2@$attnames(i)$ = gonres@$attnames(i)$(ii)
        end do
        xpos = x(0) + (nc * xsize)
        gsn_polygon_ndc(wks,(/xpos,xpos+xsize,xpos+xsize,xpos,xpos/), \
                            (/ypos,ypos,ypos+ysize,ypos+ysize,ypos/),gonres2)
      end do
    end do
    delete(attnames)                    ; We're going to use this later.
  end if

; Draw horizontal lines, top to bottom.
  do nr = 0,nrows
    ypos = y(1) - (nr * ysize)
    gsn_polyline_ndc(wks,(/x(0),x(1)/),(/ypos,ypos/),lnres)

    if(debug) then
      print("Horizontal line from (" + x(0) + "," + ypos + ") to (" + \
                                      x(1) + "," + ypos + ")")
    end if
  end do

; Draw vertical lines, left to right.
  do nc = 0,ncols
    xpos = x(0) + (nc * xsize)
    gsn_polyline_ndc(wks,(/xpos,xpos/),(/y(0),y(1)/),lnres)

    if(debug) then
      print("Vertical line from (" + xpos + "," + y(0) + ") to (" + \
                                     xpos + "," + y(1) + ")")
    end if
  end do

;
; Draw text, if any.  The text will be drawn left to right,
; top to bottom.
;
  if(text_avail) then
    txres = get_res_eq(res2,"tx")     ; Get text resources.
;
; Conform text to nrows x ncols if it is 1D.
;
    if(rank_text.eq.1) then
      text2d = new((/nrows,ncols/),string)
      if(nrows.eq.1) then
        text2d(0,:) = text
      else
        text2d(:,0) = text
      end if
    else
      text2d = text       ; Already 2D.
    end if

    xsize2 = xsize/2.     ; Half width of box.
    ysize2 = ysize/2.     ; Half height of box.

;
; All text resources should be nrows x ncols at this point. Now,
; for each individual text string, we need to grab the appropriate
; resource value, and attach it to a new resource list.
;
    txres2 = True    ; True no matter what, because we have to at least set
                     ; txJust.
;
; If txJust is not being set, use "CenterCenter" for each one.
; Note that if txres is set to False and it is setting txJust,
; it will be ignored. that's because setting txres=False means
; ignore all attributes set to this logical variable.
;
    if(.not.txres.or.(txres.and..not.isatt(txres,"txJust"))) then
      txres = True
      txres@txJust    = new(nrows*ncols,string)
      txres@txJust(:) = "CenterCenter"
    end if      

    attnames = getvaratts(txres)
    natts    = dimsizes(attnames)

    do nr = 0,nrows-1
      do nc = 0,ncols-1
        
        if(.not.ismissing(text2d(nr,nc))) then
          ii = nr*ncols+nc
;
; Copy all resources over to temporary array.
;
          do i=0,natts-1
            txres2@$attnames(i)$ = txres@$attnames(i)$(ii)
          end do
;
; Check the text justification.
;
          txjust = txres2@txJust

          if(any(str_lower(txjust).eq. \
                 (/"bottomleft","bottomcenter","bottomright"/))) then
            ypos = y(1) - ((nr+1) * ysize)
          end if
          if(any(str_lower(txjust).eq. \
                 (/"centerleft","centercenter","centerright"/))) then
            ypos = (y(1) - ((nr+1) * ysize)) + ysize2
          end if
          if(any(str_lower(txjust).eq. \
                 (/"topleft","topcenter","topright"/))) then
            ypos = y(1) - (nr * ysize)
          end if
          if(any(str_lower(txjust).eq. \
                 (/"bottomleft","centerleft","topleft"/))) then
            xpos = x(0) + (nc * xsize)
          end if
          if(any(str_lower(txjust).eq. \
                 (/"bottomcenter","centercenter","topcenter"/))) then
            xpos = (x(0) + (nc * xsize)) + xsize2
          end if
          if(any(str_lower(txjust).eq. \
                 (/"bottomright","centerright","topright"/))) then
            xpos = x(0) + ((nc+1) * xsize)
          end if
; Draw text.
          gsn_text_ndc(wks,text2d(nr,nc),xpos,ypos,txres2)
        end if
      end do
    end do
  end if
end
