;
;      $Id: gsn_csm.ncl,v 1.102 2002-08-23 20:40:08 haley Exp $
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
;;  Date:       Tue Feb 11 14:08:49 MST 1999
;;
;;  Description: This script contains some specialized plotting functions
;;               used by CGD for the CSM processor. To use the functions
;;               and procedures in this script, you must have the lines:
;;
;;                   load "gsn_code.ncl"
;;                   load "gsn_csm.ncl"
;; 
;;               at the top of your NCL script, before the begin statement.
;;

;***********************************************************************;
; For every function and procedure defined in this script, undefine it  ;
; with a call to "undef" so it doesn't clash with other functions and   ;
; procedures with the same name.                                        ;
;***********************************************************************;

undef("xy_ref_interp")
undef("ref_line_interp")
undef("get_bar_widths")
undef("outlined_bars")
undef("filled_bars")
undef("find_cross_xy")
undef("check_for_subtitles")
undef("set_right_subtitle")
undef("set_left_subtitle")
undef("set_xaxis_string")
undef("set_yaxis_string")
undef("set_subtitles_res")
undef("is_valid_xcoord")
undef("is_valid_ycoord")
undef("check_for_coord_arrays")
undef("check_class_name")
undef("get_allowed_latnames")
undef("get_allowed_latnames_without_units")
undef("get_allowed_lonnames")
undef("get_allowed_lonnames_without_units")
undef("get_allowed_pres_units_mb")
undef("get_allowed_pres_units_pa")
undef("get_allowed_pres_units_hpa")
undef("get_allowed_pres_units")
undef("get_allowed_lat_units")
undef("get_allowed_lon_units")
undef("is_valid_lat_xcoord")
undef("is_valid_lon_xcoord")
undef("is_valid_lat_ycoord")
undef("gsn_geop_hgt")
undef("check_for_y_lat_coord")
undef("check_for_x_lat_coord")
undef("check_for_lon_coord")
undef("get_polar_type")
undef("add_subtitles")
undef("add_labelbar")
undef("set_pres_hgt_axes")
undef("fill_xy_ref")
undef("fill_xy2")
undef("get_lon_values")
undef("get_lat_values")
undef("get_lon_labels")
undef("get_lat_labels")
undef("add_lon_labels")
undef("add_lat_labels_xaxis")
undef("add_lat_labels_yaxis")
undef("add_latlon_labels")
undef("add_map_tickmarks")
undef("gsn_add_cyclic_point")
undef("pop_tick_locs")
undef("pop_latlon_grid")
undef("gsn_csm_map_ce")
undef("gsn_csm_map_polar")
undef("gsn_csm_map")
undef("gsn_csm_map_other")
undef("gsn_csm_contour_map_polar")
undef("gsn_csm_contour_map_ce")
undef("gsn_csm_contour_map_other")
undef("gsn_csm_vector_map_ce")
undef("gsn_csm_vector_map_polar")
undef("gsn_csm_vector_map_other")
undef("gsn_csm_streamline_map_ce")
undef("gsn_csm_streamline_map_polar")
undef("gsn_csm_streamline_map_other")
undef("gsn_csm_streamline_contour_map_ce")
undef("gsn_csm_streamline_contour_map_polar")
undef("gsn_csm_streamline_contour_map_other")
undef("gsn_csm_vector_scalar")
undef("gsn_csm_vector_scalar_map_ce")
undef("gsn_csm_vector_scalar_map_polar")
undef("gsn_csm_vector_scalar_map_other")
undef("gsn_csm_contour_map")
undef("gsn_csm_streamline_map")
undef("gsn_csm_vector_map")
undef("gsn_csm_vector_scalar_map")
undef("gsn_csm_streamline_contour_map")
undef("gsn_csm_contour_map_overlay")
undef("gsn_csm_xy")
undef("gsn_csm_y")
undef("gsn_csm_contour")
undef("gsn_csm_hov")
undef("gsn_csm_lat_time")
undef("gsn_csm_time_lat")
undef("gsn_csm_pres_hgt")
undef("gsn_csm_vector")
undef("gsn_csm_pres_hgt_vector")
undef("gsn_csm_streamline")
undef("gsn_csm_pres_hgt_streamline")


;***********************************************************************;
; Function : xy_ref_interp                                              ;
;                  x1: numeric                                          ;
;                  x2: numeric                                          ;
;                  y1: numeric                                          ;
;                  y2: numeric                                          ;
;                                                                       ;
; Given two points, (x1,y1) and (x2,y1) and a reference value (ref),    ;
; this function interpolates where on the X axis the line from the 1st  ;
; point to the 2nd point crosses the line y=ref.                        ;
; plot to the workstation "wks" (the variable returned from a previous  ;
; call to "gsn_open_wks").  "resources" is an optional list of          ;
; resources. The Id of the map plot is returned.                        ;
;***********************************************************************;

function xy_ref_interp(x1:numeric,x2:numeric,y1:numeric,y2:numeric, \
                       ref:numeric)
begin
 return(x2-((y2-ref)*(x2-x1))/(y2-y1))
end

;***********************************************************************;
; Function : ref_line_interp                                            ;
;                  x: numeric                                           ;
;                  y: numeric                                           ;
;            xinterp: numeric                                           ;
;            yinterp: numeric                                           ;
;           ref_line: numeric                                           ;
;                                                                       ;
; Given a set of points represented by x and y, add interpolated values ;
; where the line crosses at y = ref_line. x and y must be the same      ;
; length, and ref_line must either be a scalar or the same length as x  ;
; and y. ref_line can be an array of reference values, as long as there ;
; is only one X/Y curve.                                                ;
;                                                                       ;
; The dimension size of each interpolated set of points is returned.    ;
;***********************************************************************;

function ref_line_interp(x,y,xinterp,yinterp,ref_line)
local ncurves, nref_lines, nlines, i, j, location, xnew, ynew, npts
begin

; Check for missing values.

  xmsg = get_res_value_keep(x,"_FillValue",-999)
  ymsg = get_res_value_keep(y,"_FillValue",-999)
;
; Convert x and y into two dimensional arrays so we don't have to
; test for whether we have one line or multiple lines.  Convert
; ref_line to a 1-d array (if it isn't already).
;
  ndimsy = dimsizes(dimsizes(y))
  ndimsx = dimsizes(dimsizes(x))
  nref_lines = dimsizes(ref_line)

  if(ndimsy.eq.1)
    ncurves = 1
    nptsy   = dimsizes(y)
    ynew    = onedtond(y,(/nref_lines,nptsy/))
  else
    ncurves = dimsizes(y(:,0))
    nptsy   = dimsizes(y(0,:))
    ynew    = y
  end if

  nlines = max((/ncurves,nref_lines/))

  if(ndimsx.eq.1)
    xnew = onedtond(x,(/nlines,nptsy/))
  else
    xnew = x
  end if

  refnew  = new((/nlines/),typeof(ref_line))
  refnew  = ref_line

  xinterp(:,0) = xnew(:,0)
  yinterp(:,0) = ynew(:,0)
  npts         = new((/nlines/),integer)

; Loop through the rest of the points and find out where the curve crosses
; the reference line.  If it does, then do an interpolation.
; Store the location of the previous point (-1 if it is below the reference
; line, 0 if it is on the reference line, and 1 if it is above).

  do j=0, nlines-1
    num_crosses = 0
    do i=1, nptsy-1
      xinterp(j,i+num_crosses) = xnew(j,i)
      yinterp(j,i+num_crosses) = ynew(j,i)

      if(.not.ismissing(ynew(j,i)).and..not.ismissing(xnew(j,i)).and.\
         .not.ismissing(ynew(j,i-1)).and..not.ismissing(xnew(j,i-1)))
        if((ynew(j,i-1).lt.refnew(j).and.ynew(j,i).gt.refnew(j)).or.\
           (ynew(j,i-1).gt.refnew(j).and.ynew(j,i).lt.refnew(j)))
          xinterp(j,i+num_crosses)   = xy_ref_interp(xnew(j,i-1),xnew(j,i),\
                                                     ynew(j,i-1),ynew(j,i),\
                                                     refnew(j))
          xinterp(j,i+num_crosses+1) = xnew(j,i)
          yinterp(j,i+num_crosses)   = refnew(j)
          yinterp(j,i+num_crosses+1) = ynew(j,i)
          num_crosses = num_crosses + 1
        end if
      end if
    end do
    npts(j) = nptsy + num_crosses
  end do
  delete(xnew)
  delete(ynew)
  delete(refnew)
  return(npts)
end

;***********************************************************************;
; Function : get_bar_widths                                             ;
;                  x: numeric                                           ;
;               res2: logical                                           ;
;                                                                       ;
; Given a set of x points, return a bar width for filled bars later.    ;
;                                                                       ;
;***********************************************************************;

function get_bar_widths(x,ncurves,res2)
local ncurves, npts, bar_widths, dx, nwidths
begin
;
; Get number of curves and points, and the distances between x values.
;
  dims = dimsizes(x)
  if(dimsizes(dims).eq.1)
    npts    = dimsizes(x)
    dx      = min( x(1:npts-1) - x(0:npts-2) )
  else
    npts    = dims(1)
    x!0     = "ncurves"
    x!1     = "npts"
;
; Get the minimum dx for each set of curves.
;
    dx = dim_min(x(:,1:npts-1)-x(:,0:npts-2))
    delete(x!0)
    delete(x!1)
  end if
;
; If bar width is set explicitly, we have to check it first.
;
  if(isatt(res2,"gsnXYBarChartBarWidth"))
    bar_width = get_res_value(res2,"gsnXYBarChartBarWidth",0.)
    nwidths   = dimsizes(dimsizes(bar_width))
    if(.not.(nwidths.eq.1.or.nwidths.eq.ncurves))
      print("get_bar_widths: Fatal: You must either select one constant bar width, or the same number of bar widths as you have curves.")
      return
    end if
    if(any(bar_width.gt.dx))
      print("get_bar_widths: Warning: The bar width(s) you selected ("+bar_width+") is larger than the smallest delta x ("+dx+").")
      print("Defaulting to " + dx + ".")
      
      if(nwidths.eq.1.and.ncurves.gt.1)
        bar_widths = new(ncurves,typeof(dx))
      end if
      bar_widths = dx
    else
;
; Make sure bar_widths is 1D array of length (/ncurves/).
;
      if(nwidths.eq.1)
        bar_widths = new(ncurves,double)
        bar_widths = bar_width
      end if
    end if
  else
    bar_widths = new(ncurves,double)
    bar_widths = dx        ; Bar width is not being set.
  end if

  return(bar_widths)
end


;***********************************************************************;
; Procedure : outlined_bars                                             ;
;                  x: numeric                                           ;
;                  y: numeric                                           ;
;            xinterp: numeric                                           ;
;            yinterp: numeric                                           ;
;                                                                       ;
; Given a set of points represented by x and y, represent the y values  ;
; with bar outlines.                                                    ;
;***********************************************************************;

procedure outlined_bars(x,y,xinterp,yinterp)
local ndimsx, ndimsy, ncurves, dims, npts, nyb, bar_width
begin
;
; Convert x and y into two dimensional arrays so we don't have to
; test for whether we have one line or multiple lines.
;
  ndimsx = dimsizes(dimsizes(x))
  ndimsy = dimsizes(dimsizes(y))
  
  if(ndimsy.eq.1)
    ncurves = 1
    npts    = dimsizes(y)
    ynew    = onedtond(y,(/ncurves,npts/))
  else
    ncurves = dimsizes(y(:,0))
    npts    = dimsizes(y(0,:))
    ynew    = y
  end if

  if(ndimsx.eq.1)
    xnew = onedtond(x,(/ncurves,npts/))
  else
    xnew = x
  end if
;
; For each old y point, we need two new y points to represent
; the horizontal line. The new X values will be the width of the bars.
;
  nyb = 2 * (npts-1) + 1

  yinterp(:,0:nyb-2:2) = ynew(:,:npts-2)
  yinterp(:,1:nyb-1:2) = ynew(:,:npts-2)
  yinterp(:,nyb-1)     = ynew(:,npts-1)   ; last point

  bar_widths           = xnew(:,1:) - xnew(:,:npts-2)
  xinterp(:,0:nyb-2:2) = xnew(:,:npts-2)
  xinterp(:,1:nyb-2:2) = xnew(:,:npts-2) + bar_widths
  xinterp(:,nyb-1)     = xnew(:,npts-1)     ; last point, no bar

  delete(xnew)
  delete(ynew)
end

;***********************************************************************;
; procedure : filled_bars                                               ;
;                  x: numeric                                           ;
;                  y: numeric                                           ;
;             xabove: numeric                                           ;
;             yabove: numeric                                           ;
;             xbelow: numeric                                           ;
;             ybelow: numeric                                           ;
;                nya: integer                                           ;
;                nyb: integer                                           ;
;          yref_line: numeric                                           ;
;         bar_widths: numeric                                           ;
;                                                                       ;
; Given a set of points represented by x and y, and y reference values, ;
; represent the y values with bars above and below the reference lines. ;
;                                                                       ;
; Bars aren't filled in this routine. They are returned as two sets of  ;
; polygons: those above the reference lines, and those below.           ;
;***********************************************************************;

procedure filled_bars(x,y,xabove,yabove,xbelow,ybelow,nya,nyb,yref_line, \
                      bar_widths)
local ndimsx, ndimsy, ncurves, dims, npts, nya, nyb
begin
;
; Convert x and y into two dimensional arrays so we don't have to
; test for whether we have one line or multiple lines.
;
  ndimsx = dimsizes(dimsizes(x))
  ndimsy = dimsizes(dimsizes(y))
  
  if(ndimsy.eq.1)
    ncurves = 1
    npts    = dimsizes(y)
    ynew    = new((/ncurves,npts/),double)
    ynew    = onedtond(y,(/ncurves,npts/))
  else
    ncurves = dimsizes(y(:,0))
    npts    = dimsizes(y(0,:))
    ynew    = new(dimsizes(y),double)
    ynew    = y
  end if

  if(ndimsx.eq.1)
    xnew = new((/ncurves,npts/),double)
    xnew = onedtond(x,(/ncurves,npts/))
  else
    xnew = new(dimsizes(x),double)
    xnew = x
  end if
;
; Make yref_line a 1D double array of length ncurves.
;
  yref_lines = new(ncurves,double)
  yref_lines = yref_line
;
; Loop across ncurves.
;
  do i=0,ncurves-1
;
; Get the indices where the y curve is above and below the ref line.
;
    ya_ind = ind(ynew(i,:).gt.yref_lines(i))
    yb_ind = ind(ynew(i,:).lt.yref_lines(i))
;
; Create new array to hold bar charts.  For each old y point, we need
; five new y points to represent the closed rectangle.
;
    if(.not.any(ismissing(ya_ind)))
      nya(i) = 5*dimsizes(ya_ind)              ; # values above ref line.
      yabove(i,0:nya(i)-5:5)  = yref_lines(i)
      yabove(i,1:nya(i)-4:5)  = ynew(i,ya_ind)
      yabove(i,2:nya(i)-3:5)  = ynew(i,ya_ind)
      yabove(i,3:nya(i)-2:5)  = yref_lines(i)
      yabove(i,4:nya(i)-1:5)  = yref_lines(i)

      xabove(i,0:nya(i)-5:5) = xnew(i,ya_ind) - bar_widths(i)/2.
      xabove(i,1:nya(i)-4:5) = xnew(i,ya_ind) - bar_widths(i)/2.
      xabove(i,2:nya(i)-3:5) = xnew(i,ya_ind) + bar_widths(i)/2.
      xabove(i,3:nya(i)-2:5) = xnew(i,ya_ind) + bar_widths(i)/2.
      xabove(i,4:nya(i)-1:5) = xnew(i,ya_ind) - bar_widths(i)/2.
    else
      nya(i) = 0
    end if  
    if(.not.any(ismissing(yb_ind))) 
      nyb(i) = 5*dimsizes(yb_ind)               ; # values below ref line.

      ybelow(i,0:nyb(i)-5:5) = yref_lines(i)
      ybelow(i,1:nyb(i)-4:5) = ynew(i,yb_ind)
      ybelow(i,2:nyb(i)-3:5) = ynew(i,yb_ind)
      ybelow(i,3:nyb(i)-2:5) = yref_lines(i)
      ybelow(i,4:nyb(i)-1:5) = yref_lines(i)

      xbelow(i,0:nyb(i)-5:5) = xnew(i,yb_ind) - bar_widths(i)/2.
      xbelow(i,1:nyb(i)-4:5) = xnew(i,yb_ind) - bar_widths(i)/2.
      xbelow(i,2:nyb(i)-3:5) = xnew(i,yb_ind) + bar_widths(i)/2.
      xbelow(i,3:nyb(i)-2:5) = xnew(i,yb_ind) + bar_widths(i)/2.
      xbelow(i,4:nyb(i)-1:5) = xnew(i,yb_ind) - bar_widths(i)/2.
    else
      nyb(i) = 0
    end if  
    delete(ya_ind)
    delete(yb_ind)
  end do

  delete(xnew)
  delete(ynew)  
end


;***********************************************************************;
; Function : find_cross_xy                                              ;
;                  x1: numeric                                          ;
;                  x2: numeric                                          ;
;                 y11: numeric                                          ;
;                 y12: numeric                                          ;
;                 y21: numeric                                          ;
;                 y22: numeric                                          ;
;                                                                       ;
; Given two points of two curves:                                       ;
;                                                                       ;
;       (x1,y11) & (x2,y12) and (x1,y21) & (x2,y22)                     ;
;                                                                       ;
; compute the point at which the two curves cross.                      ;
;                                                                       ;
;***********************************************************************;

function find_cross_xy(x1:double,x2:double,y11:double,y12:double, \
                       y21:double,y22:double)
begin
  if((x2-x1).eq.0.or.(y12-y11+y21-y22).eq.0)
    x0 = new(1,double,-9999)
    y0 = new(1,double,-9999)
    x0 = -9999
    y0 = -9999
  else
    x0 = (x2*y21 - x2*y11 + x1*y12 - x1*y22)/(y12-y11+y21-y22)
    y0 = (y11*y22 - y12*y21) / (y11-y12+y22-y21)
;    y0 = (x0*(y22-y21)-x1*y22+x2*y21)/(x2-x1)
  end if
  return((/x0,y0/))
end

;***********************************************************************;
; Procedure : check_for_subtitles                                       ;
;                 res: logical                                          ;
;                left: logical                                          ;
;              center: logical                                          ;
;               right: logical                                          ;
;                                                                       ;
; This procedure checks if the resources gsnLeftString, gsnCenterString,;
; and/or gsnRightString have been set. These resources provide three    ;
; subtitles for the top of a plot.                                      ;
;***********************************************************************;
 
procedure check_for_subtitles(res:logical,left:logical,center:logical,\
                              right:logical)
begin
; Initialize
  left   = False
  right  = False
  center = False

  if((res).and..not.any(ismissing(getvaratts(res))))
    if(isatt(res,"gsnLeftString"))
      left            = True
      left@sub_string = res@gsnLeftString
      delete(res@gsnLeftString)
    end if
    if(isatt(res,"gsnCenterString"))
      center            = True
      center@sub_string = res@gsnCenterString
      delete(res@gsnCenterString)
    end if
    if(isatt(res,"gsnRightString"))
      right            = True
      right@sub_string = res@gsnRightString
      delete(res@gsnRightString)
    end if
  end if
  return
end

;***********************************************************************;
; Procedure : set_right_subtitle                                        ;
;                data: numeric                                          ;
;                 res: logical                                          ;
;              newres: logical                                          ;
;                                                                       ;
; This procedure checks if gsnRightString is set.  If not, then uses    ;
; data@units if it exists.                                              ;
;***********************************************************************;
procedure set_right_subtitle(data:numeric,res:logical,newres:logical)
begin
;
; If gsnRightString hasn't been set, then use data@units if it
; exists.
;
  if(isatt(res,"gsnRightString"))
    newres = True
    newres@gsnRightString = res@gsnRightString
  else
    if(isatt(data,"units"))
      newres = True
      newres@gsnRightString = data@units
    end if
  end if

  return
end

;***********************************************************************;
; Procedure : set_left_subtitle                                         ;
;                data: numeric                                          ;
;                 res: logical                                          ;
;              newres: logical                                          ;
;                                                                       ;
; This procedure checks if gsnLeftString is set.  If not, then uses     ;
; data@long_name if it exists.                                          ;
;***********************************************************************;
procedure set_left_subtitle(data:numeric,res:logical,newres:logical)
begin
;
; If gsnLeftString hasn't been set, then use data@long_name if it
; exists.  Also have to delete data@long_name so it doesn't get
; used in the title by gsn_code.ncl.
;
  if(isatt(data,"long_name"))
    newres = True
    newres@gsnLeftString = get_res_value_keep(res,"gsnLeftString",data@long_name)
    delete(data@long_name)  ; Delete so it doesn't get used for main title
  else
    if(isatt(res,"gsnLeftString"))
      newres = True
      newres@gsnLeftString = res@gsnLeftString
    end if
  end if
  return
end

;***********************************************************************;
; Function : is_valid_xcoord                                            ;
;                                                                       ;
; Checks if the X coordinate variable exist and if it is 1-dimensional. ; 
;                                                                       ;
;***********************************************************************;
function is_valid_xcoord(data:numeric)
local dims1
begin
  if(.not.ismissing(data!1).and.iscoord(data,data!1))
    dims1 = dimsizes(dimsizes(data&$data!1$))
    if(dims1.eq.1) then
      return(True)
    else
      return(False)
    end if
  else
    return(False)
  end if
end

;***********************************************************************;
; Function : is_valid_ycoord                                            ;
;                                                                       ;
; Checks if the Y coordinate variable exist and if it is 1-dimensional. ; 
;                                                                       ;
;***********************************************************************;
function is_valid_ycoord(data:numeric)
local dims0
begin
  if(.not.ismissing(data!0).and.iscoord(data,data!0))
    dims0 = dimsizes(dimsizes(data&$data!0$))
    if(dims0.eq.1) then
      return(True)
    else
      return(False)
    end if
  else
    return(False)
  end if
end

;***********************************************************************;
; Procedure : set_xaxis_string                                          ;
;                 res: logical                                          ;
;                data: numeric                                          ;
;                                                                       ;
; This procedure sets the tiXAxisString resource depending on current   ;
; resource and/or attribute settings.                                   ;
;***********************************************************************;
procedure set_xaxis_string(res:logical,data:numeric)
begin
  res2 = res
  is_xyplot = get_res_value_keep(res2,"gsnXYPlot", False)
  is_xaxis  = get_res_value_keep(res2,"gsnXAxis", False)
;
; If the X axis tickmarks are being labeled explicitly by the user, 
; then don't put a label on the X axis.
;
  if(check_attr(res,"tmXBMode","Explicit",True))
    set_attr(res,"tiXAxisString","")
  else
    if(is_xyplot.and.is_xaxis) then
      if(is_valid_ycoord(data).and.isatt(data&$data!0$,"long_name"))
        set_attr(res,"tiXAxisString",data&$data!0$@long_name)
      end if
    else    
      if(.not.is_xyplot.and..not.ismissing(data!1).and.iscoord(data,data!1) \
              .and.isatt(data&$data!1$,"long_name"))
        set_attr(res,"tiXAxisString",data&$data!1$@long_name)
      end if
    end if
  end if

  return
end

;***********************************************************************;
; Procedure : set_yaxis_string                                          ;
;                 res: logical                                          ;
;                data: numeric                                          ;
;                                                                       ;
; This procedure sets the tiYAxisString resource depending on current   ;
; resource and/or attribute settings.                                   ;
;***********************************************************************;
procedure set_yaxis_string(plot:graphic,res:logical,data:numeric)
begin
  tires = get_res_eq(res,(/"ti"/))  ; Get tickmark resources
  is_xyplot = get_res_value_keep(tires,"gsnXYPlot", False)
  is_yaxis  = get_res_value_keep(tires,"gsnYAxis", False)
;
; If the Y axis tickmarks are being labeled explicitly by the user, 
; then don't put a label on the Y axis.
;
  if(check_attr(tires,"tmYLMode","Explicit",True))
    set_attr(tires,"tiYAxisString","")
  else
    if((is_xyplot.and.is_yaxis).or.(.not.is_xyplot)) then
      if(is_valid_ycoord(data).and.isatt(data&$data!0$,"long_name"))
        set_attr(tires,"tiYAxisString",data&$data!0$@long_name)
      end if
    end if
  end if

  attsetvalues_check(plot,tires)
  return
end

;***********************************************************************;
; Procedure : set_subtitles_res                                         ;
;                 res: logical                                          ;
;             plotres: logical                                          ;
;                                                                       ;
; This procedure checks if any of the gsn*String subtitle resources have;
; been set. If so, it adds them to plotres as resources so they can     ;
; be passed to another plotting function to be processed.               ;
;***********************************************************************;
procedure set_subtitles_res(res:logical,plotres:logical)
begin
  if((res).and..not.any(ismissing(getvaratts(res))))
    if(isatt(res,"gsnLeftString"))
      plotres = True
      plotres@gsnLeftString = res@gsnLeftString
      delete(res@gsnLeftString)
    end if
    if(isatt(res,"gsnCenterString"))
      plotres = True
      plotres@gsnCenterString = res@gsnCenterString
      delete(res@gsnCenterString)
    end if
    if(isatt(res,"gsnRightString"))
      plotres = True
      plotres@gsnRightString = res@gsnRightString
      delete(res@gsnRightString)
    end if
  end if
  return
end

;***********************************************************************;
; Procedure : check_for_coord_arrays                                    ;
;                data: numeric                                          ;
;                 res: logical                                          ;
;                type: string                                           ;
;                                                                       ;
; This procedure checks if the data contains a coordinate variable in   ;
; dimension 0 or 1.  If so, then it sets sf/vfX/YArray (depending on if ;
; type equals "contour" or "vector" to these coordinate arrays.         ;
;                                                                       ;
; If no coordinate array exists, then this procedure checks for the     ;
; special "lat2d"/"lon2d" *attributes*. This is an indicator that we    ;
; have "2D coordinate" arrays, and we can set the sf/vfX/YArray         ;
; resources to these values.                                            ;
;                                                                       ;
;***********************************************************************;
procedure check_for_coord_arrays(data[*][*]:numeric,res:logical,type:string)
local prefix
begin 
  if(type.eq."vector")
    prefix = "vf"
  else
    prefix = "sf"
  end if

;
; First check if "lon2d" is set, which must be the same dimension
; as the data.  This special attribute indicates a 2D longitude
; coordinate array.  If "lon2d" is not set, then check for
; a regular coordinate array.  One of these will be used for 
; sf/vf/XArray.
;
  if(isatt(data,"lon2d").and. \
     all(dimsizes(data@lon2d).eq.dimsizes(data))) then
    if(.not.(isatt(res,prefix+"XCStartV").and.isatt(res,prefix+"XCEndV")))
      set_attr(res,prefix+"XArray",data@lon2d)
    end if
  else
    if(is_valid_xcoord(data))
      if(.not.(isatt(res,prefix+"XCStartV").and.isatt(res,prefix+"XCEndV")))
        set_attr(res,prefix+"XArray",data&$data!1$)
      end if
    end if
  end if

;
; First check if "lat2d" is set, which must be the same dimension
; as the data.  This special attribute indicates a 2D latitude
; coordinate array.  If "lat2d" is not set, then check for
; a regular coordinate array.  One of these will be used for 
; sf/vf/YArray.
;
  if(isatt(data,"lat2d").and. \
     all(dimsizes(data@lat2d).eq.dimsizes(data))) then
    if(.not.(isatt(res,prefix+"YCStartV").and.isatt(res,prefix+"YCEndV")))
      set_attr(res,prefix+"YArray",data@lat2d)
    end if
  else
    if(is_valid_ycoord(data))
      if(.not.(isatt(res,prefix+"YCStartV").and.isatt(res,prefix+"YCEndV")))
        set_attr(res,prefix+"YArray",data&$data!0$)
      end if
    end if
  end if
  return
end

;***********************************************************************;
; Procedure : check_class_name                                          ;
;                  plot: graphic                                        ;
;             plot_type: string                                         ;
;                                                                       ;
; This procedure checks what type "plot" is, and returns the plot from  ;
; it (for example, if "plot" is an overlay, and you want the contour    ;
; part of it, it will return the contour plot.                          ;
;                                                                       ;
;***********************************************************************;

function check_class_name(plot:graphic,plot_type:string)
begin
  class_name = NhlClassName(plot)

  if(class_name.eq."logLinPlotClass".or.class_name.eq."irregularPlotClass")
    new_plot = plot@$plot_type$
  else
    new_plot = plot
  end if
 return(new_plot)

end

;***********************************************************************;
; Function : get_allowed_latnames                                       ;
;                                                                       ;
; Get list of names allowed for a latitude coordinate array.            ; 
;                                                                       ;
;***********************************************************************;
function get_allowed_latnames()
begin
  return((/"lat","Lat","Latitude","LAT","latitude","LATITUDE","hlat",\
           "lat_u","lat_t","lat_98","yc"/))
end

;***********************************************************************;
; Function : get_allowed_latnames_without_units                         ;
;                                                                       ;
; Get list of names allowed for a latitude coordinate array.  These     ; 
; are latitudes names from a grib file, so their units are assumed to   ;
; be "degrees_north".                                                   ;
;***********************************************************************;
function get_allowed_latnames_without_units()
begin
  return((/"g0_lat_2","g0_lat_3","g0_lat_4"/))
end

;***********************************************************************;
; Function : get_allowed_lonnames                                       ;
;                                                                       ;
; Get list of names allowed for a longitude coordinate array.           ; 
;                                                                       ;
;***********************************************************************;
function get_allowed_lonnames()
begin
  return((/"lon","Lon","Longitude","LON","longitude","LONGITUDE","hlon",\
           "long","lon_u","lon_t","lon_98","xc"/))
end

;***********************************************************************;
; Function : get_allowed_lonnames_without_units                         ;
;                                                                       ;
; Get list of names allowed for a longitude coordinate array.  These    ; 
; are longitudes names from a grib file, so their units are assumed to  ;
; be "degrees_east".                                                    ;
;***********************************************************************;
function get_allowed_lonnames_without_units()
begin
  return((/"g0_lon_2","g0_lon_3","g0_lon_4"/))
end

;***********************************************************************;
; Function : get_allowed_pres_units_pa                                  ;
;                                                                       ;
; Get list of names allowed for pressure units in pascals.              ; 
;                                                                       ;
;***********************************************************************;
function get_allowed_pres_units_pa()
begin
  return((/"Pa","pa","PA","Pascals","pascals","PASCALS"/))
end

;***********************************************************************;
; Function : get_allowed_pres_units_hpa                                 ;
;                                                                       ;
; Get list of names allowed for pressure units in hecto-pascals.        ; 
;                                                                       ;
;***********************************************************************;
function get_allowed_pres_units_hpa()
begin
  return((/"hpa","hPa","HPA","hecto-pascals","HECTO-PASCALS"/))
end

;***********************************************************************;
; Function : get_allowed_pres_units_mb                                  ;
;                                                                       ;
; Get list of names allowed for pressure units in millibars.            ; 
;                                                                       ;
;***********************************************************************;
function get_allowed_pres_units_mb()
begin
  return((/"mb","Mb","MB","millibars","MILLIBARS"/))
end

;***********************************************************************;
; Function : get_allowed_pres_units                                     ;
;                                                                       ;
; Get list of names allowed for pressure units.                         ; 
;                                                                       ;
;***********************************************************************;
function get_allowed_pres_units()
begin
  mb  = get_allowed_pres_units_mb()
  pa  = get_allowed_pres_units_pa()
  hpa = get_allowed_pres_units_hpa()
  nmb  = dimsizes(mb)
  npa  = dimsizes(pa)
  nhpa = dimsizes(hpa)
  all_pres = new(nmb+npa+nhpa,string)
  all_pres(0:nmb-1)       = mb
  all_pres(nmb:nmb+npa-1) = pa
  all_pres(nmb+npa:)      = hpa
  return(all_pres)
end

;***********************************************************************;
; Function : get_allowed_lat_units                                      ;
;                                                                       ;
; Get list of names allowed for the units attribute of a latitude       ;
; coordinate array.                                                     ; 
;                                                                       ;
;***********************************************************************;
function get_allowed_lat_units()
begin
  return((/"degrees_north","degree_north","degrees north","degrees_N"/))
end

;***********************************************************************;
; Function : get_allowed_lon_units                                      ;
;                                                                       ;
; Get list of names allowed for the units attribute of a longitude      ;
; coordinate array.                                                     ; 
;                                                                       ;
;***********************************************************************;
function get_allowed_lon_units()
begin
  return((/"degrees_east","degree_east","degrees east","degrees_E"/))
end

;***********************************************************************;
; Function : is_valid_lat_xcoord                                        ;
;                                                                       ;
; Checks if the X coordinate variable contains real lat values, and not ;
; just indices. This is determined by the "units" attribute, which must ;
; be set by CSM convention.                                             ;
;                                                                       ;
;***********************************************************************;
function is_valid_lat_xcoord(data:numeric)
begin
  if(is_valid_xcoord(data)) then
    if(any(data!1.eq.get_allowed_latnames()))
      if(isatt(data&$data!1$,"units"))
        if(any(data&$data!1$@units.eq.get_allowed_lat_units))
          return(True)
        else
          print("is_valid_lat_xcoord: Warning: The units attribute of the X coordinate array is not set to one of the allowable units values (i.e. 'degrees_north'). Your latitude labels may not be correct.")
          return(True)
        end if
      else
        print("is_valid_lat_xcoord: Warning: The units attribute of the X coordinate array is not set. Assuming units to be degrees_north.")
        return(True)
      end if
    else
      if(any(data!1.eq.get_allowed_latnames_without_units()))
        return(True)
      end if
    end if
  end if
  return(False)
end

;***********************************************************************;
; Function : is_valid_lon_xcoord                                        ;
;                                                                       ;
; Checks if the X coordinate variable contains real lon values, and not ;
; just indices. This is determined by the "units" attribute, which must ;
; be set by CSM convention.                                             ;
;                                                                       ;
;***********************************************************************;
function is_valid_lon_xcoord(data:numeric)
begin
  if(is_valid_xcoord(data))
    if(any(data!1.eq.get_allowed_lonnames()))
      if(isatt(data&$data!1$,"units"))
        if(any(data&$data!1$@units.eq.get_allowed_lon_units))
          return(True)
        else
          print("is_valid_lon_xcoord: Warning: The units attribute of the X coordinate array is not set to one of the allowable units values (i.e. 'degrees_east'). Your longitude labels may not be correct.")
          return(True)
        end if
      else
        print("is_valid_lon_xcoord: Warning: The units attribute of the X coordinate array is not set. Assuming units to be degrees_east.")
        return(True)
      end if
    else
      if(any(data!1.eq.get_allowed_lonnames_without_units()))
        return(True)
      end if
    end if
  end if
  return(False)
end

;***********************************************************************;
; Function : is_valid_lat_ycoord                                        ;
;                                                                       ;
; Checks if the Y coordinate variable contains real lat values, and not ;
; just indices. This is determined by the "units" attribute, which must ;
; be set by CSM convention.                                             ;
;                                                                       ;
;***********************************************************************;
function is_valid_lat_ycoord(data:numeric)
begin
  if(is_valid_ycoord(data))
    if(any(data!0.eq.get_allowed_latnames()))
      if(isatt(data&$data!0$,"units"))
        if(any(data&$data!0$@units.eq.get_allowed_lat_units))
          return(True)
        else
          print("is_valid_lat_ycoord: Warning: The units attribute of the Y coordinate array is not set to one of the allowable units values (i.e. 'degrees_north'). Your latitude labels may not be correct.")
          return(True)
        end if
      else
        print("is_valid_lat_ycoord: Warning: The units attribute of the Y coordinate array is not set. Assuming units to be degrees_north.")
        return(True)
      end if
    else
      if(any(data!0.eq.get_allowed_latnames_without_units()))
        return(True)
      end if
    end if
  end if
  return(False)
end

;***********************************************************************;
; Procedure : gsn_geop_hgt                                              ;
;                                                                       ;
; Returns geopotential height (in km) given array p (pressure in mb)    ;
; p must lie between 1013.25 mb and 2.54e-06 mb.                        ;
;                                                                       ;
; Algorithm is simply logarithmic interpolation from Standard           ;
; Atmosphere.                                                           ;
; Intended to provide an ESTIMATE of geopotential height when           ;
; temperature and geopotential data are lacking.                        ;
;                                                                       ;
; Values above 85km were obtained from the COSPAR International         ;
; Reference Atmosphere: 1986 QB495 .A38 v.10 No.12  [FL]                ;
;                                                                       ;
;***********************************************************************;
 
function gsn_geop_hgt( p[*]:numeric )
local nsa,psa,zsa,ptmp,npres,found,i,j
 
begin
  if(isatt(p,"units").and.(any(p@units.eq.get_allowed_pres_units_pa())))
    ptmp = p * 0.01  ; Convert to mb
  else
    if((.not.isatt(p,"units")).or. \
       (isatt(p,"units").and. \
        .not.(any(p@units.eq.get_allowed_pres_units_hpa()).or. \
              any(p@units.eq.get_allowed_pres_units_mb()))))
      print("gsn_geop_hgt: Warning: The 'units' attribute is either not set, or it is not set")
      print("to the recognized names for 'hecto-pascals' or 'millibars', so")
      print("assuming pressure values are already converted to millibars.")
    end if
    ptmp = tofloat(p)     ; Assume already converted to mb!
  end if

  nsa = 53
  psa = new( (/nsa/), float, 1.e36 )
  zsa = new( (/nsa/), float, 1.e36 )
 
  zsa = (/ -0.3, \                      ; km
            0.0,  0.5,  1.0,  1.5,  2.0,  2.5,  3.0, \
            3.5,  4.0,  4.5,  5.0,  5.5,  6.0,  6.5, \
            7.0,  7.5,  8.0,  8.5,  9.0,  9.5, 10.0, \
           11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, \
           18.0, 19.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, \
           50.0, 60.0, 70.0, 80.0, 84.8, 87.7, 90.6, \
           93.3, 96.1, 97.5,100.4,104.9, \
          110.0,114.2,116.7,119.7/)

  psa = (/ 1050., \                     ; mb (hPa)
           1013.25, 954.61, 898.76, 845.59, 795.01, 746.91, 701.21,    \
           657.80, 616.60, 577.52, 540.48, 505.39, 472.17, 440.75,     \
           411.05, 382.99, 356.51, 331.54, 308.00, 285.84, 264.99,     \
           226.99, 193.99, 165.79, 141.70, 121.11, 103.52, 88.497,     \
           75.652, 64.674, 55.293, 25.492, 11.970, 5.746, 2.871, 1.491,\
           0.798, 0.220, 0.052, 0.010, 0.00485,0.00294,0.000178, \
           0.000108, 0.0000656, 0.0000511, 0.0000310, 0.0000146, \
           0.00000691, 0.00000419, 0.00000327, 0.00000254 /)


  if ( any(ptmp.lt.min(psa)) .or. any(ptmp.gt.max(psa))) then
    print("gsn_geop_hgt: Fatal: The pressure values do not fall between")
    print(min(psa) + " mb and " + max(psa) + " mb.")
    print("Execution halted.")
    exit
  end if
 
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
 
;***********************************************************************;
; Procedure : check_for_y_lat_coord                                     ;
;                data: numeric                                          ;
;                 res: logical                                          ;
;                type: string                                           ;
;                                                                       ;
; This procedure checks if the data contains a coordinate variable in   ;
; dimension 0 of an allowable list of latnames                          ;
;                                                                       ;
; If so, then it sets sf/vfYArray (depending on if type equals          ;
; "contour" or "vector" to this coordinate array.                       ;
;                                                                       ;
;***********************************************************************;
procedure check_for_y_lat_coord(data[*][*]:numeric,res:logical,type:string)
local latname, char_latname, array_name, start_name, end_name
begin 
;
; Determine what kind of plot we need to set the data field resources for.
; 
  if(type.eq."vector")
    array_name = "vfYArray"
    start_name = "vfYCStartV"
    end_name   = "vfYCEndV"
  else
    array_name = "sfYArray"
    start_name = "sfYCStartV"
    end_name   = "sfYCEndV"
  end if

;
; Make sure user is not already setting his own *Start* and *End*
; resources, and that the first dimension of the data actually contains
; coordinate information.
;
; Check the name of the coordinate variable against the allowed
; lat names. The coordinate variable must also be one-dimensional to
; be valid.
;
; First check if "lat2d" is set, which must be the same dimension
; as the data.  This special attribute indicates a 2D latitude
; coordinate array. If "lat2d" is not set, then check for
; a regular coordinate array.
;
  if(isatt(data,"lat2d").and. \
     all(dimsizes(data@lat2d).eq.dimsizes(data)).and. \
     .not.(isatt(res,start_name).and.isatt(res,end_name)))
    set_attr(res,array_name,data@lat2d)
    return
  else
    if(is_valid_lat_ycoord(data))
      if(.not.(isatt(res,start_name).and.isatt(res,end_name)))
        set_attr(res,array_name,data&$data!0$)
        return
      end if
    end if
  end if 

  print("check_for_y_lat_coord: Warning: Data either does not contain a valid latitude coordinate array or doesn't contain one at all.")
  print("Valid latitude names include: ")
  print("    " + cat_strings(get_allowed_latnames))
  return
end

;***********************************************************************;
; Procedure : check_for_x_lat_coord                                     ;
;                data: numeric                                          ;
;                 res: logical                                          ;
;                type: string                                           ;
;                                                                       ;
; This procedure checks if the data contains a coordinate variable in   ;
; dimension 1 of an allowable list of latnames                          ;
;                                                                       ;
; If so, then it sets sf/vfYArray (depending on if type equals          ;
; "contour" or "vector" to this coordinate array.                       ;
;                                                                       ;
;***********************************************************************;
procedure check_for_x_lat_coord(data[*][*]:numeric,res:logical,type:string)
local latname, char_latname, array_name, start_name, end_name
begin 
;
; Determine what kind of plot we need to set the data field resources for.
; 
  if(type.eq."vector")
    array_name = "vfXArray"
    start_name = "vfXCStartV"
    end_name   = "vfXCEndV"
  else
    array_name = "sfXArray"
    start_name = "sfXCStartV"
    end_name   = "sfXCEndV"
  end if

;
; Make sure user is not already setting his own *Start* and *End*
; resources, and that the second dimension of the data actually contains
; coordinate information.
;
; Also check the name of the coordinate variable against the allowed
; lat names. The coordinate variable must also be one-dimensional to
; be valid.
;
; If no coordinate data is set, then check for the special attribute
; "lat2d", which must be the same dimension as the data.  This special
; attribute indicates a 2D latitude coordinate array.
;
  if(is_valid_lat_xcoord(data))
    if(.not.(isatt(res,start_name).and.isatt(res,end_name)))
      set_attr(res,array_name,data&$data!1$)
      return
    end if
  else
    if(isatt(data,"lat2d").and. \
       all(dimsizes(data@lat2d).eq.dimsizes(data)).and. \
       .not.(isatt(res,start_name).and.isatt(res,end_name)))
      set_attr(res,array_name,data@lat2d)
      return
    end if 
  end if

  print("check_for_x_lat_coord: Warning: Data either does not contain an known latitude coordinate array or doesn't contain one at all")
  return
end

;***********************************************************************;
; Procedure : check_for_lon_coord                                       ;
;                data: numeric                                          ;
;                 res: logical                                          ;
;                type: string                                           ;
;                                                                       ;
; This procedure checks if the data contains a coordinate variable in   ;
; dimension 1 of an allowable list of lonnames                          ;
;                                                                       ;
; If so, then it sets sf/vfXArray (depending on if type equals          ;
; "contour" or "vector" to this coordinate array.                       ;
;                                                                       ;
;***********************************************************************;
procedure check_for_lon_coord(data[*][*]:numeric,res:logical,type:string)
local lonname, char_lonname, array_name, start_name, end_name
begin 
;
; Determine what kind of plot we need to set the data field resources for.
; 
  if(type.eq."vector")
    array_name = "vfXArray"
    start_name = "vfXCStartV"
    end_name   = "vfXCEndV"
  else
    array_name = "sfXArray"
    start_name = "sfXCStartV"
    end_name   = "sfXCEndV"
  end if

;
; Make sure user is not already setting his own *Start* and *End*
; resources, and that the second dimension of the data actually
; contains coordinate information (or that "lon2d" is set).
;
; Check the name of the coordinate variable against the allowed
; lon names
;
; Frist check for the special attribute"lon2d", which must be the
; same dimension as the data.  This special attribute indicates a
; 2D longitude coordinate array.  Otherwise, check for a regular
; coordinate array.
;
  if(isatt(data,"lon2d").and. \
     all(dimsizes(data@lon2d).eq.dimsizes(data)).and. \
     .not.(isatt(res,start_name).and.isatt(res,end_name)))
    set_attr(res,array_name,data@lon2d)
    return
  else
    if(is_valid_lon_xcoord(data))
      if(.not.(isatt(res,start_name).and.isatt(res,end_name)))
        set_attr(res,array_name,data&$data!1$)
        return
      end if
    end if 
  end if

  print("check_for_lon_coord: Warning: Data either does not contain a valid longitude coordinate array or doesn't contain one at all.")
  print("Valid longitude names include: ")
  print("    " + cat_strings(get_allowed_lonnames))
  return
end

;***********************************************************************;
; Function : get_polar_type                                             ;
;                 res: logical                                          ;
;                                                                       ;
; This function checks the resource list for gsnPolar, gsnPolarNH, or   ;
; gsnPolarSH to and returns what kind of polar plot is desired.         ;
;                                                                       ;
; The default will be northern ("NH") if none is set.                   ;
;***********************************************************************;
function get_polar_type(res:logical)
local polar_nh, polar_sh
begin
  polar_type = get_res_value(res,"gsnPolar","")
  polar_nh   = get_res_value(res,"gsnPolarNH",False)
  polar_sh   = get_res_value(res,"gsnPolarSH",False)

  if(polar_type.eq."NH".or.polar_type.eq."nh")
    return("NH")
  end if
  if(polar_type.eq."SH".or.polar_type.eq."sh")
    return("SH")
  end if
  if(polar_type.ne."")
    print("get_polar_type: Warning: Invalid option for gsnPolar. Defaulting to northern hemisphere.")
    return("NH")
  end if    
;
; Can't have polar_nh and polar_sh both False or both True.
;
  if(polar_nh.and.polar_sh) 
    print("get_polar_type: Fatal: You have indicated you want both northern and southern polar projections.")
    print("Only one of them can be selected, so please correct this.")
    print("Execution halted.")
    exit        
  end if

  if(.not.polar_nh.and..not.polar_sh) 
    print("get_polar_type: Warning: No polar projection has been selected. Defaulting to northern hemisphere.")
    return("NH")
  end if

  if(polar_nh) 
    return("NH")
  else
    return("SH")
  end if
end

;***********************************************************************;
; Procedure : add_subtitles                                             ;
;                wks: graphic                                           ;
;               plot: graphic                                           ;
;        left_string: logical                                           ;
;      center_string: logical                                           ;
;       right_string: logical                                           ;
;                res: logical                                           ;
;                                                                       ;
; This procedure adds one to three subtitles to the top of the plot,    ;
; depending on whether the special resources gsnLeftString,             ;
; gsnCenterString, and/or gsnRightString had been set.                  ;
;***********************************************************************;
 
procedure add_subtitles(wks:graphic,plot:graphic,left_string:logical,\
                        center_string:logical,right_string:logical,\
                        res:logical)
local text_object, parallel_pos, anno, just, strings, txres, amres, \
num_res, ablank, chararray, string_len
begin
  amres = False   ; Annotation resources
  txres = False   ; Text resources

  num_res  = 0
  parallel_pos = (/0.0,0.5,1.0/)
  just         = (/"left","center","right"/)

  strings = (/"","",""/)  
  if(left_string)
    strings(0) =   left_string@sub_string
  end if
  if(center_string) 
    strings(1) = center_string@sub_string
  end if
  if(right_string)
    strings(2) =  right_string@sub_string
  end if

; Check for text and annotation resources

  txres = get_res_eq(res,"tx")
  amres = get_res_eq(res,"am")
  
  getvalues plot
    "vpHeightF"  : vph
  end getvalues

  orthpos = get_res_value(amres,"amOrthogonalPosF",0.01*vph)
  ablank = stringtochar(" ")
  do i=0,2
    if(.not.ismissing(strings(i)).and.strings(i).ne."")
;
; Check to make sure the string doesn't contain only blanks.
;
      chararray = stringtochar(strings(i)) 
      string_len = dimsizes(chararray)-1

      if(any(chararray(0:string_len-1).ne.ablank(0)))
        text_object = create just(i)+"string" textItemClass wks
          "txString" : strings(i)
        end create

        attsetvalues_check(text_object,txres)   ; Set some text resources.

        anno = NhlAddAnnotation(plot,text_object)  ; Add annotation to plot.

        setvalues anno
          "amZone"          : 3             ; Just outside plot area
          "amSide"          : "top"         ; Subtitle at top.
          "amParallelPosF"  : parallel_pos(i)
          "amJust"          : "bottom"+just(i)
          "amOrthogonalPosF": orthpos       ; Move away from top edge
          "amResizeNotify"  : True          ; Resize subtitle if map resized.
        end setvalues

        attsetvalues_check(anno,amres)   ; Set some annotation resources.
      end if
      delete(chararray)
    end if
  end do
  return
end

;***********************************************************************;
; Procedure : add_labelbar                                              ;
;                wks: graphic                                           ;
;               plot: graphic                                           ;
;               zone: integer                                           ;
;        font_height: numeric                                           ;
;               type: string                                            ;
;                res: logical                                           ;
;                                                                       ;
; This procedure adds a labelbar to "plot" (which must be either a      ;
; contour or vector plot). The default is to add the labelbar to the    ;
; bottom of the plot, unless resources are set to change this. The zone ;
; must be passed in, as well as the font height for the labelbar labels.;
; "type" is the type of plot ("ce", "polar", and so on) so the labelbar ;
; can be positioned differently depending on the plot.                  ;
;***********************************************************************;
 
procedure add_labelbar(wks:graphic,plot,zone:integer,font_height:numeric,\
                       type:string,res:logical)
local anno, num_res, amres, lbres, labelbar_object, lbar_orient, \
lbar_side, lbar_just, fill_patterns, fill_scales, mono_fill_pat, \
mono_fill_scl, mono_fill_col
begin
;
; if lbLabelBarOn is set to False, then don't bother with a labelbar.
;
  if(check_attr(res,"lbLabelBarOn",False,False))
    return
  end if

  lbres = True   ; hold labelbar resources
  amres = True   ; hold annomanager resources 

  parallel = get_res_value(res,"pmLabelBarParallelPosF",0.5)

; Check for annotation/labelbar resources

  lbres = get_res_eq(res,(/"lb","vp"/))
  amres = get_res_eq(res,"am")

  set_attr(amres,"amZone",zone)
  set_attr(amres,"amResizeNotify",True)
  set_attr(amres,"amParallelPosF",parallel)
  set_attr(lbres,"lbLabelFontHeightF",font_height)

; Default is horizontal at the bottom.

  lbar_orient = "horizontal"
  lbar_side   = "bottom"
  lbar_just   = "bottomcenter"

  if(check_attr(lbres,"lbOrientation","vertical",True))
    lbar_orient = "vertical"
    lbar_side   = "right"
    lbar_just   = "centerright"
  end if

  set_attr(lbres,"lbOrientation",lbar_orient)
  set_attr(amres,"amSide",lbar_side)
  set_attr(amres,"amJust",lbar_just)

; 
; Determine what the default labelbar width and height should be depending
; on shape of plot (this is for a horizontal labelbar).  For example, if
; the plot is square, then make the labelbar the same width as the plot.
; If the plot is 1:2, then make the labelbar .75 * width of the plot.
;
; For a vertical labelbar, make the height the same as the height of the
; plot.
;
  getvalues plot
    "vpWidthF"  : width    ; Width of plot
    "vpHeightF" : height   ; Height of plot
  end getvalues

  ratio1 = width/height
  ratio2 = height/width
  ratio = min((/ratio1,ratio2/))
  ratios  = (/0.50, 0.75, 1.00/)  ; viewport ratios

  lwscale   = (/0.75, 0.90, 1.00/)
  wh_ind    = ind(ratio.le.ratios)
  def_lvpwf = lwscale(wh_ind(0))    ; default width scale for labelbar

  amres = get_res_eq(res,"am")
  if(lbar_orient.eq."vertical") then
    if(type.eq."polar")
      orth = get_res_value(res,"pmLabelBarOrthogonalPosF",0.05)
    else
      orth = get_res_value(res,"pmLabelBarOrthogonalPosF",0.03)
    end if
    height = get_res_value(res,"pmLabelBarHeightF",height)
    width  = get_res_value(res,"pmLabelBarWidthF",0.2*width)
  else
    height = get_res_value(res,"pmLabelBarHeightF",0.3*height)
    width  = get_res_value(res,"pmLabelBarWidthF",def_lvpwf*width)
    if(zone.eq.2)
      if(type.eq."polar")
        orth = get_res_value(res,"pmLabelBarOrthogonalPosF",0.03)
      else
        orth = get_res_value(res,"pmLabelBarOrthogonalPosF",0.06)
      end if
    else
      orth = get_res_value(res,"pmLabelBarOrthogonalPosF",0.0)
    end if
  end if
  set_attr(lbres,"vpHeightF",height)
  set_attr(lbres,"vpWidthF",width)
  set_attr(amres,"amOrthogonalPosF",orth)

  class_name = NhlClassName(plot)
  if(class_name.eq."logLinPlotClass".or.class_name.eq."irregularPlotClass")
    the_plot = plot@contour
  else
    the_plot = plot
  end if
  if(class_name.eq."contourPlotClass".or.class_name.eq."logLinPlotClass".or.\
     class_name.eq."irregularPlotClass")
    getvalues the_plot
      "cnLevels"          : levels
      "cnFillColors"      : colors
      "cnFillPatterns"    : fill_patterns
      "cnFillScales"      : fill_scales
      "cnMonoFillPattern" : mono_fill_pat
      "cnMonoFillScale"   : mono_fill_scl
      "cnMonoFillColor"   : mono_fill_col
    end getvalues
;
; Check if we want different fill patterns or fill scales.  If so, we
; have to pass these on to the labelbar.
;
    lbres@lbMonoFillColor = mono_fill_col
    if(.not.mono_fill_pat)
      lbres@lbMonoFillPattern = False
      lbres@lbFillPatterns    = fill_patterns
    end if
    if(.not.mono_fill_scl)
      lbres@lbMonoFillScale = False
      lbres@lbFillScales    = fill_scales
    end if
  else
    getvalues the_plot
      "vcLevels"      : levels
      "vcLevelColors" : colors
    end getvalues
  end if

  fix_zero_contour(levels)

  labelbar_object = create_labelbar(wks,dimsizes(colors),colors,levels,lbres)

  anno = NhlAddAnnotation(plot,labelbar_object)

  attsetvalues_check(anno,amres)   ; Set annotation resources.

  return
end

;***********************************************************************;
; Procedure : set_pres_hgt_axes                                         ;
;               pres: numeric                                           ;
;                res: logical                                           ;
;            add_hgt: logical                                           ;
;                                                                       ;
; This procedure sets some resources necessary to label the X and Y axes;
; with "nice" pressure and height values. The left axis is for pressure ;
; values, and the right for height values. The pressure values are      ;
; assumed to be in millibars.                                           ;
;***********************************************************************;

procedure set_pres_hgt_axes(pres:numeric,res:logical,add_hgt:logical)
local hgt, hnice, pnice, ptmp
begin
  if(pres(0).lt.pres(dimsizes(pres)-1))
    ptmp = tofloat(pres(::-1))    ; reverse values so descending order
  else
    ptmp = tofloat(pres)          ; Make sure values are floating point.
  end if

  if(.not.(isatt(res,"sfYCStartV").and.isatt(res,"sfYCEndV")))
    set_attr(res,"sfYArray",ptmp)
  end if

;
; Set up the "nice" pressure values for which to label the left axis.
;
  if(.not.isatt(res,"tmYLMode"))
    res@tmYLMode   = "Explicit" ; Define own tick mark labels.
    res@tmYLValues = (/1000.,  850., 700., 500., 400., 300., 250.,\
                         200., 150., 100.,  70., 50., 30., 10./)
    res@tmYLLabels = (/"1000","850","700","500","400","300","250",\
                         "200","150","100", "70", "50", "30", "10"/)
    res@tmYLMinorOn= False        ; No minor tick marks.
    set_attr(res,"tiYAxisString","Pressure (mb)")
  end if

;
; Calculate "nice" height values for which to label the right axis
;
  if(.not.isatt(res,"tmYRMode"))
    add_hgt = True
    hgt    = gsn_geop_hgt(ptmp)     ; Calculate hgts as a fcn of pres.
    hrange = fabs(hgt(0)-hgt(dimsizes(hgt)-1))
    if(hrange.le.35) then
      step = 4
    else
      if(hrange.le.70) then
        step = 7
      else
        step = 10
      end if
    end if
;
; If user has set the resource "tmYRTickSpacingF", then use this for
; the value of the height spacing, instead of our calculated values
; above.  This value must be a "nice" value, like 1, 2, 4, etc.
;
    step = tointeger(get_res_value(res,"tmYRTickSpacingF",step))
                                            ; Set range of "nice" hgt values.
    hnice = tofloat(ispan(tointeger(floor(hgt(0))), \
                    tointeger(ceil(hgt(dimsizes(hgt)-1))),step))
    pnice = ftcurv(hgt,ptmp,hnice)  ; Get pres vals at nice hgt vals.

    use_left    = get_res_value_keep(res,"tmYUseLeft",False)
    yrvalues    = get_res_value_keep(res,"tmYRValues",pnice)
    yrlabels    = get_res_value_keep(res,"tmYRLabels",hnice)
    yron        = get_res_value_keep(res,"tmYROn",True)
    yrlabelson  = get_res_value_keep(res,"tmYRLabelsOn",True )
    yrminoron   = get_res_value_keep(res,"tmYRMinorOn",False)
;
; At each "nice" pressure value put a "height" value label, unless
; the user has specified own labels.  The user has to know what he's
; doing if he specifies own labels, because the values must be in pressure
; units, not height units.
;
    if(.not.isatt(res,"tmYRValues")) then
      res@tmYRValues  = yrvalues
    end if
    if(.not.isatt(res,"tmYRLabels")) then
      res@tmYRLabels  = yrlabels
    end if
    res@tmYRMode      = "Explicit"
    res@tmYUseLeft    = use_left
    res@tmYROn        = yron
    res@tmYRLabelsOn  = yrlabelson
    res@tmYRMinorOn   = yrminoron
    delete(hnice)
    delete(hgt)
    delete(pnice)
  else
    add_hgt = False
  end if
  delete(ptmp)
  return
end

;***********************************************************************;
; Function : fill_xy_ref                                                ;
;                    wks: graphic                                       ;
;                     xy: graphic                                       ;
;                 xin[*]: numeric                                       ;
;                 yin[*]: numeric                                       ;
;                    ref: numeric                                       ;
;   ref_line_above_color: integer or string or RGB value                ;
;   ref_line_below_color: integer or string or RGB value                ;
;                                                                       ;
; Take a set of X and Y points, and fill the Y points in one color if   ;
; they are above the ref line, and in another color if they are below   ;
; the ref line.                                                         ;
;***********************************************************************;

function fill_xy_ref(wks:graphic,xy:graphic,xin[*]:numeric,yin[*]:numeric,\
                     ref:numeric,ref_line_above_color,ref_line_below_color)
local npts, yabove_gon, ybelow_gon, i, gsresa, gsresb, gsresr, \
fill_above, fill_below
begin

  npts = dimsizes(yin)

  anumgons = 0  ; Count the number of polygons
  bnumgons = 0
  if(ref_line_above_color.ne.-1)
    gsresa             = True
    gsresa@gsFillColor = ref_line_above_color
;
; First count the number of the above-the-y-reference-line polygons we
; need to create.
;
    igon       = 0
    create_gon = False
    do i=0,npts-1
      if(.not.ismissing(yin(i)).and..not.ismissing(xin(i)))
        if(yin(i).ge.ref)
          if(igon.eq.0)
            igon = 1
          else
            igon = igon + 1
          end if
          if(i.eq.npts-1)      ; On the last point.
            create_gon = True
          end if
        else
          create_gon = True
        end if
      else
        create_gon = True
      end if   ; not ismissing
      if(create_gon)
        if(igon.gt.1)
          anumgons = anumgons + 1 
        end if
        igon       = 0
        create_gon = False
      end if
    end do
  end if

  if(ref_line_below_color.ne.-1)
    gsresb             = True
    gsresb@gsFillColor = ref_line_below_color
;
; First count the number of the above-the-y-reference-line polygons we
; need to create.
;
    igon       = 0
    create_gon = False
    do i=0,npts-1
      if(.not.ismissing(yin(i)).and..not.ismissing(xin(i)))
        if(yin(i).le.ref)
          if(igon.eq.0)
            igon = 1
          else
            igon = igon + 1
          end if
          if(i.eq.npts-1)      ; On the last point.
            create_gon = True
          end if
        else
          create_gon = True
        end if
      else
        create_gon = True
      end if   ; not ismissing
      if(create_gon)
        if(igon.gt.1)
          bnumgons = bnumgons + 1 
        end if
        igon       = 0
        create_gon = False
      end if
    end do
  end if
  if(anumgons.gt.0.or.bnumgons.gt.0) then
    polygons = new(anumgons+bnumgons,graphic) 
  else
    polygons = new(1,graphic) 
  end if
;
; Loop through and add above-ref-line polygons.
;
  if(ref_line_above_color.ne.-1.and.anumgons.gt.0)
    ngons      = 0
    igon       = 0
    create_gon = False
    do i=0,npts-1
      if(.not.ismissing(yin(i)).and..not.ismissing(xin(i)))
        if(yin(i).ge.ref)
          if(igon.eq.0)
            xabove_line = new(npts+2,typeof(xin))
            yabove_line = new(npts+2,typeof(yin))
            yabove_line(0) = yin(i)
            xabove_line(0) = xin(i)
            igon = 1
          else
            xabove_line(igon) = xin(i) 
            yabove_line(igon) = yin(i) 
            igon = igon + 1
          end if
          if(i.eq.npts-1)      ; On the last point.
            create_gon = True
          end if
        else
          create_gon = True
        end if
      else
        create_gon = True
      end if   ; not ismissing
      if(create_gon)
        if(igon.gt.1)
          yabove_gon = new((/igon+3/),typeof(yin))
          xabove_gon = new((/igon+3/),typeof(xin))
; Close up and draw polygon
          if(yabove_line(0).ne.ref)
            xabove_gon(0) = xabove_line(0)
            yabove_gon(0) = ref
            xabove_gon(1:igon) = xabove_line(0:igon-1)
            yabove_gon(1:igon) = yabove_line(0:igon-1)
            newgon = igon + 1
          else
            xabove_gon(0:igon-1) = xabove_line(0:igon-1)
            yabove_gon(0:igon-1) = yabove_line(0:igon-1)
            newgon = igon
          end if
          if(yabove_line(igon-1).ne.ref)
            xabove_gon(newgon) = xabove_line(igon-1)
            yabove_gon(newgon) = ref
            newgon = newgon + 1
          end if

          polygons(ngons) = gsn_add_polygon(wks,xy,xabove_gon(0:newgon-1),\
                                            yabove_gon(0:newgon-1),gsresa)
          ngons = ngons + 1 
; Delete temporary arrays so we can recreate them.
          delete(xabove_line)
          delete(yabove_line)
          delete(xabove_gon)
          delete(yabove_gon)
        end if
        igon       = 0
        create_gon = False
      end if
    end do
  end if

;
; Loop through and add below-ref-line polygons.
;
  if(ref_line_below_color.ne.-1.and.bnumgons.gt.0) then
    igon       = 0
    create_gon = False

    do i=0,npts-1
      if(.not.ismissing(yin(i)).and..not.ismissing(xin(i)))
        if(yin(i).le.ref)
          if(igon.eq.0)
            xbelow_line = new(npts+2,typeof(xin))
            ybelow_line = new(npts+2,typeof(yin))
            ybelow_line(0) = yin(i)
            xbelow_line(0) = xin(i)
            igon = 1
          else
            xbelow_line(igon) = xin(i) 
            ybelow_line(igon) = yin(i) 
            igon = igon + 1
          end if
          if(i.eq.npts-1)      ; On the last point.
            create_gon = True
          end if
        else
          create_gon = True
        end if
      else
        create_gon = True
      end if   ; not ismissing
      if(create_gon)
        if(igon.gt.1)
          ybelow_gon = new((/igon+3/),typeof(yin))
          xbelow_gon = new((/igon+3/),typeof(xin))
; Close up and draw polygon
          if(ybelow_line(0).ne.ref)
            xbelow_gon(0) = xbelow_line(0)
            ybelow_gon(0) = ref
            xbelow_gon(1:igon) = xbelow_line(0:igon-1)
            ybelow_gon(1:igon) = ybelow_line(0:igon-1)
            newgon = igon + 1
          else
            xbelow_gon(0:igon-1) = xbelow_line(0:igon-1)
            ybelow_gon(0:igon-1) = ybelow_line(0:igon-1)
            newgon = igon
          end if
          if(ybelow_line(igon-1).ne.ref)
            xbelow_gon(newgon) = xbelow_line(igon-1)
            ybelow_gon(newgon) = ref
            newgon = newgon + 1
          end if

          polygons(ngons) = gsn_add_polygon(wks,xy,xbelow_gon(0:newgon-1), \
                                            ybelow_gon(0:newgon-1),gsresb)
          ngons = ngons + 1 
; Delete temporary arrays so we can recreate them.
          delete(xbelow_line)
          delete(ybelow_line)
          delete(xbelow_gon)
          delete(ybelow_gon)
        end if
        igon       = 0
        create_gon = False
      end if
    end do
  end if
  return(polygons)
end

;***********************************************************************;
; Function : fill_xy2                                                   ;
;                                                                       ;
; This function takes as input two Y curves, defined on the same set    ;
; of X points, and fills the area between the two curves with two       ;
; different colors, depending on which curve is above the other one.    ;
;                                                                       ;
;***********************************************************************;
function fill_xy2(wks,plot,xi:numeric,yi1:numeric,yi2:numeric,colora,colorb)
local nx, ny1, ny2, y1_gt_y2, i, ab, bpt, ept, x, y1, y2, color, first, last
begin
  res = True
;
; Check length of arrays.
;
  nx  = dimsizes(xi)
  ny1 = dimsizes(yi1)
  ny2 = dimsizes(yi2)

  if(nx.ne.ny1.or.nx.ne.ny2.or.nx.lt.2) 
    print("fill_xy2: Error: x and y arrays must be the same length and at least two elements")
    return 
  end if

  if(any(ismissing(xi)))
    print("fill_xy2: Error: The x array cannot contain any missing values")
    return 
  end if
;
; Convert input arrays to double.
;
; If both y arrays are equal to each other, then just return.
;
  if(all(yi1.eq.yi2))
    return(plot)
  end if
;
; Convert input arrays to double.
;
  x  = new(dimsizes(xi),double)
  y1 = new(ny1,double,1.e36)
  y2 = new(ny1,double,1.e36)
  x  = (/xi/)
  y1 = (/yi1/)
  y2 = (/yi2/)

  res@gsFillColor = colora ; Color for when first curve is > second curve

;
; Compute a delta that will be used to determine if two points are
; actually the same point. 
;
  range_y1 = max(y1) - min(y1)
  range_y2 = max(y2) - min(y2)

  delta_y1 = 0.01 * range_y1
  delta_y2 = 0.01 * range_y2
  if(delta_y1.eq.0)
    delta = delta_y2
  else
    if(delta_y2.eq.0)
      delta = delta_y1
    else
      delta = min((/delta_y1,delta_y2/))
    end if
  end if

;
; Create arrays for storing polygon points.
;
  first     = new(2,double)
  last      = new(2,double)
  polygon_x = new(2*nx+3,double)
  polygon_y = new(2*nx+3,double)

  npoly = 0     ; Number of polygons
;
; First fill in polygons where y1 is above y2, and then fill in
; polygons where y2 is above y1.
;
  do ab = 0,1
    if(ab.eq.1)
      y1 = (/yi2/)
      y2 = (/yi1/)
      res@gsFillColor = colorb ; Color for when first curve is > second curve
    end if
;
; Get areas where y1 > y2.
;
    y1_gt_y2 = y1.gt.y2

    bpt = -1    ; Index of first point of polygon.
    ept = -1    ; Index of last point of polygon.
;
; Loop through points.
;
    do i=0,nx-1
      if(bpt.lt.0)
        if(.not.ismissing(y1_gt_y2(i)).and.y1_gt_y2(i))
          bpt = i
          ept = i
        end if
      else
        if(.not.ismissing(y1_gt_y2(i)).and.y1_gt_y2(i))
          ept = i
        end if
        if(ismissing(y1_gt_y2(i)).or..not.y1_gt_y2(i).or.ept.eq.(nx-1))
;
; Draw polygon, If bpt is the very first point or ept is the
; very last point, then these are special cases we have to
; handle separately.
;
          if(bpt.eq.0.or.(bpt.gt.0.and.(ismissing(y1(bpt-1)).or.\
                                        ismissing(y2(bpt-1)).or.\
                                        ismissing(x(bpt-1)))))
            first(0) =  x(bpt)
            first(1) = y2(bpt)
          else
            if(fabs(y1(bpt-1)-y2(bpt-1)).le.delta)
;
; If the two points are within delta of each other, then we'll
; consider them to be the same point.
;
              first(0) =  x(bpt-1)
              first(1) = y1(bpt-1)
            else
;
; Otherwise, find the intersection where the two curves cross.
;
              first = find_cross_xy(x(bpt-1),x(bpt),y1(bpt-1), \
                                    y1(bpt),y2(bpt-1),y2(bpt))
            end if
          end if
          if(ept.eq.(nx-1).or.(ept.lt.(nx-1).and.(ismissing(y1(ept+1)).or.\
                                                  ismissing(y2(ept+1)).or.\
                                                  ismissing(x(ept+1)))))
            last(0) =  x(ept)
            last(1) = y2(ept)
          else
            if(fabs(y1(ept+1)-y2(ept+1)).le.delta)
;
; If the two points are within delta of each other, then we'll
; consider them to be the same point.
;
              last(0) =  x(ept+1)
              last(1) = y1(ept+1)
            else
;
; Otherwise, find the intersection where the two curves cross.
;
              last = find_cross_xy(x(ept),x(ept+1),y1(ept),y1(ept+1), \
                                   y2(ept),y2(ept+1))
            end if
          end if
;
; Initialize polygon.
;
          npts  = ept - bpt + 1
          npts2 = npts * 2
          polygon_x(0)              = first(0)
          polygon_y(0)              = first(1)
          polygon_x(1:npts)         = x(bpt:ept)
          polygon_y(1:npts)         = y1(bpt:ept)
          polygon_x(npts+1)         = last(0)
          polygon_y(npts+1)         = last(1)
          polygon_x(npts+2:npts2+1) = x(ept:bpt)
          polygon_y(npts+2:npts2+1) = y2(ept:bpt)
          polygon_x(npts2+2)        = first(0)
          polygon_y(npts2+2)        = first(1)
;
; Make sure polygons get drawn *after* the plot gets drawn.
;
          if(npoly.eq.0)
            setvalues plot
              "tfPolyDrawOrder" : "Predraw"
            end setvalues
          end if
;
; Add polygon to XY plot.
;
          var_string = systemfunc("echo fill_polygon$$") + npoly
          plot@$var_string$ = gsn_add_polygon(wks,plot, \
                                              polygon_x(0:npts2+2), \
                                              polygon_y(0:npts2+2),res)
;
; Advance polygon counter.
;
          npoly = npoly + 1
          bpt = -1            ; Reinitialize
          ept = -1
        end if
      end if
    end do
  end do
  return(plot)
end


;***********************************************************************;
; Function : get_lon_values                                             ;
;             min_lon: numeric                                          ;
;             max_lon: numeric                                          ;
;         lon_spacing: numeric                                          ;
;        mlon_spacing: numeric                                          ;
;                                                                       ;
; Calculate "nice" longitude values to use for placing longitude labels ;
; an axis. min_lon and max_lon are the min/max longitude values, and    ;
; lon_spacing and mlon_spacing are the spacings to use for major and    ;
; minor tickmarks.                                                      ;
;***********************************************************************;

function get_lon_values(min_lon:numeric,max_lon:numeric,lon_spacing:numeric,\
                        mlon_spacing:numeric,range_array,lonspacing_array, \
                        mlonspacing_array)
local lon_range, lcheck_arr, lspcng_arr, mlspcng_arr, lon_spcng, mlon_spcng, \
start_lon, end_lon, xstart_lon, xend_lon
begin

; Initialize

  if(all(range_array.eq.0))
    lcheck_arr = (/ 20, 40, 60, 360/) ; lon range will determine
                                      ; spacing of minor/major ticks
  else
    lcheck_arr = range_array
  end if
    
  if(all(lonspacing_array.eq.0))
    lspcng_arr = (/ 5, 10, 20, 30/)  ; spacings for major ticks
  else
    lspcng_arr = lonspacing_array
  end if

  if(all(mlonspacing_array.eq.0))
    mlspcng_arr = (/ 1,  2,  5, 10/)  ; spacings for minor ticks
  else
    mlspcng_arr = mlonspacing_array
  end if

; Longitude range, will help determine longitude spacing.

  lon_range = max_lon - min_lon   ; lat/lon range

  if(lon_range.gt.360)
    print("get_lon_values: Warning: The range your of longitude values is greater than 360.")
    lon_ind = dimsizes(lcheck_arr)-1
  else
    lon_ind = ind(lon_range.le.lcheck_arr)
  end if


  if(lon_spacing.le.0)
    lon_spcng  = lspcng_arr(lon_ind(0))  ; spacing for lon major tickmarks
  else
    lon_spcng = tointeger(lon_spacing)
  end if

  if(mlon_spacing.le.0)
    mlon_spcng = mlspcng_arr(lon_ind(0)) ; spacing for lon minor tickmarks
  else
    mlon_spcng = tointeger(mlon_spacing)
  end if
  delete(lon_ind)
;
; This code will find the first integer value that is divisible
; by the major lon spacing value.  This will end up being the first
; labeled longitude value.
;
   start_lon = floattointeger(ceil(tofloat(min_lon)))
   end_lon   = floattointeger(floor(tofloat(max_lon)))
   lon = start_lon
   found = False
   do while(lon.le.end_lon.and..not.found)
     if((lon % lon_spcng).eq.0)
       start_lon = lon
       found = True
     end if
     lon = lon + 1
  end do

; Calculate values for major and minor lon tickmark locations.
  lon_values       = ispan(start_lon,end_lon,lon_spcng)
  lon_values@minor = ispan(min(lon_values)-lon_spcng,max(lon_values)+lon_spcng,mlon_spcng)
  return(lon_values)
end

;***********************************************************************;
; Function : get_lat_values                                             ;
;             min_lat: numeric                                          ;
;             max_lat: numeric                                          ;
;         lat_spacing: numeric                                          ;
;        mlat_spacing: numeric                                          ;
;                                                                       ;
; Calculate "nice" latitude values to use for placing latitude labels   ;
; an axis. min_lat and max_lat are the min/max latitude values, and     ;
; lat_spacing and mlat_spacing are the spacings to use for major and    ;
; minor tickmarks.                                                      ;
;***********************************************************************;

function get_lat_values(min_lat:numeric,max_lat:numeric,lat_spacing:numeric,\
                        mlat_spacing:numeric)
local lat_range, lcheck_arr, lspcng_arr, mlspcng_arr, lat_spcng, mlat_spcng, \
start_lat, end_lat, lat, found
begin

; Initialize

  if(min_lat.lt.-90.or.max_lat.gt.90) then
    print("get_lat_values: Warning: Your latitude values do not fall between -90 and 90 inclusive.")
    print("You will not get nice get 'nice' latitude labels.")
    lat_values = new(1,float,-999)
    return(lat_values)
  end if

  lcheck_arr  = (/ 20, 40, 60, 180/) ; lat range will determine
                                     ; spacing of minor/major ticks
  lspcng_arr  = (/  5, 10, 20, 30/)  ; spacings for major ticks
  mlspcng_arr = (/  1,  2,  5, 10/)  ; spacings for minor ticks

; Latitude range, will help determine latitude spacing.

  lat_range = max_lat - min_lat   ; lat/lat range

  lat_ind    = ind(lat_range.le.lcheck_arr)
  if(lat_spacing.le.0)
    lat_spcng = lspcng_arr(lat_ind(0))  ; spacing for lat major tickmarks
  else
    lat_spcng = tointeger(lat_spacing)
  end if

  if(mlat_spacing.le.0)
    mlat_spcng = mlspcng_arr(lat_ind(0)) ; spacing for lat minor tickmarks
  else
    mlat_spcng = tointeger(mlat_spacing)
  end if
  delete(lat_ind)
  
;
; This code will find the first integer value that is divisible
; by the major lat spacing value.  This will end up being the first
; labeled latitude value.
;
   start_lat = floattointeger(ceil(tofloat(min_lat)))
   end_lat   = floattointeger(floor(tofloat(max_lat)))
   lat = start_lat
   found = False
   do while(lat.le.end_lat.and..not.found)
     if((lat % lat_spcng).eq.0)
       start_lat = lat
       found = True
     end if
     lat = lat + 1
  end do

; Calculate values for major and minor lat tickmark locations.
  lat_values       = ispan(start_lat,end_lat,lat_spcng)
  lat_values@minor = ispan(min(lat_values)-lat_spcng,max(lat_values)+lat_spcng,mlat_spcng)
  return(lat_values)
end

;***********************************************************************;
; Function : get_lon_labels                                             ;
;                lon_values[*]: numeric                                 ;
;                                                                       ;
; Return some nice longitude labels given longitude values "lon_values".;
;***********************************************************************;

function get_lon_labels(lon_values:numeric)
local lon_index
begin
  lon_labels  = new(dimsizes(lon_values),string)

; -180 < lon < 0 (west)

  lon_index = ind((lon_values).lt.0)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = fabs(lon_values(lon_index)) + "W"  ; west
  end if
  delete(lon_index)

; 0 < lon < 180 (east)

  lon_index = ind(lon_values.gt.0.and.lon_values.lt.180)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = lon_values(lon_index) + "E"  ; east
  end if
  delete(lon_index)

; 180 < lon < 360 (west)

  lon_index = ind(lon_values.gt.180.and.lon_values.lt.360)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = 360-lon_values(lon_index) + "W"  ; west
  end if
  delete(lon_index)

; 360 < lon < 540 (east)

  lon_index = ind(lon_values.gt.360.and.lon_values.lt.540)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = lon_values(lon_index)-360 + "E" ; east
  end if
  delete(lon_index)

; 540 < lon < 720 (west)

  lon_index = ind(lon_values.gt.540.and.lon_values.lt.720)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = 540-lon_values(lon_index) + "W"  ; west
  end if
  delete(lon_index)

; lon = +/-180 or lon=540 (180)

  lon_index = ind(fabs(lon_values).eq.180.or.lon_values.eq.540)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = "180"
  end if
  delete(lon_index)

; lon=0, lon=360, or lon=720 (0)

  lon_index = ind(lon_values.eq.0.or.lon_values.eq.360.or.lon_values.eq.720)
  if(.not.all(ismissing(lon_index)))
      lon_labels(lon_index) = "0"
  end if
  delete(lon_index)

  return(lon_labels)
end

;***********************************************************************;
; Function : get_lat_labels                                             ;
;                lat_values[*]: numeric                                 ;
;                                                                       ;
; Return some nice latitude labels given latitude values "lat_values".  ;
;***********************************************************************;

function get_lat_labels(lat_values:numeric)
local lat_index
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

;***********************************************************************;
; Function : add_lon_labels                                             ;
;               plot: graphic                                           ;
;             min_lon: numeric                                          ;
;             max_lon: numeric                                          ;
;         lon_spacing: numeric                                          ;
;        mlon_spacing: numeric                                          ;
;                 res: logical                                          ;
;                                                                       ;
; Add "nice" longitude labels ("120W", "60E", etc) to the X axis.       ;
;***********************************************************************;

procedure add_lon_labels(plot:graphic,min_lon:numeric,max_lon:numeric,\
                         lon_spacing:numeric,mlon_spacing:numeric,\
                         range_array,lonspacing_array,mlonspacing_array, \
                         res:logical)
local tmres, lonvalues, lonlabels
begin
  tmres = get_res_eq(res,(/"tm","ti"/))  ; Get tickmark resources

  lon_spacing2 = get_res_value(tmres,"tmXBTickSpacingF",lon_spacing)
  lonvalues   = get_lon_values(min_lon,max_lon,lon_spacing2,mlon_spacing,\
                             range_array,lonspacing_array,mlonspacing_array)
  if(.not.any(ismissing(lonvalues)))
    lonlabels   = get_lon_labels(lonvalues)

    tmres = True
    set_attr(tmres,"tmXBMode"         , "Explicit")
    set_attr(tmres,"tmXBLabels"       , lonlabels)
    set_attr(tmres,"tmXBValues"       , lonvalues)
    set_attr(tmres,"tmXBMinorValues"  , lonvalues@minor)
;
; If the user has set tiXAxisString explicitly, then don't set tiXAxisOn
; to False.
;
    if(.not.(isatt(tmres,"tiXAxisString").and.tmres@tiXAxisString.ne.""))
      set_attr(tmres,"tiXAxisOn"      , False)
    end if
    attsetvalues(plot,tmres)
  end if
  return
end

;***********************************************************************;
; Procedure : add_lat_labels_xaxis                                      ;
;               plot: graphic                                           ;
;             min_lat: numeric                                          ;
;             max_lat: numeric                                          ;
;         lat_spacing: numeric                                          ;
;        mlat_spacing: numeric                                          ;
;                 res: logical                                          ;
;                                                                       ;
;                                                                       ;
; Add "nice" latitude labels ("90S", "30N", etc) to the X axis in       ;
; "plot".                                                               ;
;***********************************************************************;

procedure add_lat_labels_xaxis(plot:graphic,min_lat:numeric,max_lat:numeric,\
                               lat_spacing:numeric,mlat_spacing:numeric, \
                               res:logical)
local tmres, latvalues, latlabels
begin
  tmres = get_res_eq(res,(/"tm","ti"/))  ; Get tickmark resources

  lat_spacing2 = get_res_value(tmres,"tmXBTickSpacingF",lat_spacing)
  latvalues    = get_lat_values(min_lat,max_lat,lat_spacing2,mlat_spacing)
  if(.not.any(ismissing(latvalues)))
    latlabels    = get_lat_labels(latvalues)

    tmres = True
    set_attr(tmres,"tmXBMode"         , "Explicit")
    set_attr(tmres,"tmXBLabels"       , latlabels)
    set_attr(tmres,"tmXBValues"       , latvalues)
    set_attr(tmres,"tmXBMinorValues"  , latvalues@minor)
;
; If the user has set tiXAxisString explicitly, then don't set tiXAxisOn
; to False.
;
    if(.not.(isatt(tmres,"tiXAxisString").and.tmres@tiXAxisString.ne.""))
      set_attr(tmres,"tiXAxisOn"      , False)
    end if

    attsetvalues(plot,tmres)
  end if

  return
end

;***********************************************************************;
; Procedure : add_lat_labels_yaxis                                      ;
;               plot: graphic                                           ;
;             min_lat: numeric                                          ;
;             max_lat: numeric                                          ;
;         lat_spacing: numeric                                          ;
;        mlat_spacing: numeric                                          ;
;                 res: logical                                          ;
;                                                                       ;
; Add "nice" latitude labels ("90S", "30N", etc) to the Y axis in       ;
; "plot".                                                               ;
;***********************************************************************;

procedure add_lat_labels_yaxis(plot:graphic,min_lat:numeric,max_lat:numeric,\
                               lat_spacing:numeric,mlat_spacing:numeric, \)
                               res:logical)
local tmres, latvalues, latlabels
begin
  tmres = get_res_eq(res,(/"tm","ti"/))  ; Get tickmark resources

  lat_spacing2 = get_res_value(tmres,"tmYLTickSpacingF",lat_spacing)
  latvalues    = get_lat_values(min_lat,max_lat,lat_spacing2,mlat_spacing)
  if(.not.any(ismissing(latvalues)))
    latlabels    = get_lat_labels(latvalues)

    tmres = True
    set_attr(tmres,"tmYLMode"         , "Explicit")
    set_attr(tmres,"tmYLLabels"       , latlabels)
    set_attr(tmres,"tmYLValues"       , latvalues)
    set_attr(tmres,"tmYLMinorValues"  , latvalues@minor)
;
; If the user has set tiYAxisString explicitly, then don't set tiYAxisOn
; to False.
;
    if(.not.(isatt(tmres,"tiYAxisString").and.tmres@tiYAxisString.ne.""))
      set_attr(tmres,"tiYAxisOn"      , False)
    end if
    attsetvalues(plot,tmres)
  end if
  return
end

;***********************************************************************;
; Procedure : add_latlon_labels                                         ;
;               plot: graphic                                           ;
;               data: numeric                                           ;
;                res: logical                                           ;
;                                                                       ;
; Add "nice" latitude/longitude labels ("90S", "30N", etc) to the X/Y   ;
; axes if the appropriate lat/lon coordinate arrays exist.              ;
;                                                                       ;
;***********************************************************************;
procedure add_latlon_labels(plot:graphic,data:numeric,res:logical)
local min_lat, max_lat, min_lon, max_lon, lat_spacing
begin
;
; If the plot coming in is an XY plot, then it won't have 
; coordinate variables associated with it.  Instead, we have
; to determine if it's the X or Y data coming in, and then see
; if the data contains a valid "units" attribute.
;
  is_xyplot = get_res_value_keep(res,"gsnXYPlot", False)
  is_xaxis  = get_res_value_keep(res,"gsnXAxis", False)
  is_yaxis  = get_res_value_keep(res,"gsnYAxis", False)
  set_xaxis = False

  if((is_xyplot.and.is_xaxis.and.isatt(data,"units").and. \
      any(data@units.eq.get_allowed_lat_units)).or. \
     (.not.is_xyplot.and.is_valid_lat_xcoord(data))) then

; Label X axis with nice latitude labels.

    getvalues plot
      "trXMinF"   : min_lat
      "trXMaxF"   : max_lat
    end getvalues

    add_lat_labels_xaxis(plot,min_lat,max_lat,0,0,res)

    set_xaxis = True
  end if

  if((is_xyplot.and.is_yaxis.and.isatt(data,"units").and. \
      any(data@units.eq.get_allowed_lat_units)).or. \
     (.not.is_xyplot.and.is_valid_lat_ycoord(data))) then

; Label Y axis with nice latitude labels.

    getvalues plot
      "trYMinF"   : min_lat
      "trYMaxF"   : max_lat
    end getvalues

    add_lat_labels_yaxis(plot,min_lat,max_lat,0,0,res)
  else
;
; Only set a Y axis label if it is not a lat axis.
;
    set_yaxis_string(plot,res,data)
  end if

  if((is_xyplot.and.is_xaxis.and.isatt(data,"units").and. \
      any(data@units.eq.get_allowed_lon_units)).or. \
     (.not.is_xyplot.and.is_valid_lon_xcoord(data))) then

; Label X axis with nice longitude labels.

    getvalues plot
      "trXMinF"   : min_lon
      "trXMaxF"   : max_lon
    end getvalues
;
; We want different defaults for longitude spacing, so set them up here.
;
    lon_range        = (/ 20,  40,  60, 180, 360/)
    lon_spacing_arr  = (/ 10,  20,  30,  45,  60/)
    mlon_spacing_arr = (/  5,   5,  10,  15,  30/)

    add_lon_labels(plot,min_lon,max_lon,0,0,lon_range, \
                   lon_spacing_arr,mlon_spacing_arr,res)

    set_xaxis = True
  end if
end

;***********************************************************************;
; Procedure : add_map_tickmarks                                         ;
;              plot:object                                              ;
;               res:logical                                             ;
;              font_height:numeric                                      ;
;                                                                       ;
; This procedure adds tickmarks to a C.E. map projection.               ;
;***********************************************************************;
procedure add_map_tickmarks(wks:graphic,map:graphic,tmres:logical, \
                            lon_spacing,mlon_spacing,lat_spacing,\
                            mlat_spacing)
local vpxf, vpyf, vpwf, vphf, min_lon, max_lon, min_lat, max_lat, \
center_lon, lonvalues, latvalues, lonlabels, latlabels, tickmark
begin
; Retrieve the view port location of the map plot so we know where
; to put lat/lon labels.

    getvalues map
      "vpXF"         : vpxf
      "vpYF"         : vpyf
      "vpWidthF"     : vpwf
      "vpHeightF"    : vphf
      "mpLimitMode"  : limit_mode
    end getvalues
;
; We allow either mpLimitMode to be "LatLon" (1) or "Corners" (5)
;
    if(limit_mode.eq.1)
      getvalues map
        "mpMinLonF"    : min_lon
        "mpMaxLonF"    : max_lon
        "mpMinLatF"    : min_lat
        "mpMaxLatF"    : max_lat
        "mpCenterLonF" : center_lon
      end getvalues
    else
      if(limit_mode.eq.5)
        getvalues map
          "mpLeftCornerLonF"  : min_lon
          "mpRightCornerLonF" : max_lon
          "mpLeftCornerLatF"  : min_lat
          "mpRightCornerLatF" : max_lat
          "mpCenterLonF"      : center_lon
        end getvalues
      else
        print("add_map_tickmarks: Warning: you cannot use mpLimitMode=" + limit_mode + " with the gsm_csm_map routines")
        return 
      end if
    end if

; Create tickmark values for lat/lon labels.

    lonvalues = get_lon_values(min_lon,max_lon,lon_spacing,mlon_spacing,0,0,0)
    latvalues = get_lat_values(min_lat,max_lat,lat_spacing,mlat_spacing)

; Create labels for lat/lon tick marks.

    latlabels  = get_lat_labels(latvalues)
    lonlabels  = get_lon_labels(lonvalues)

; Create a TickMark object to label lat/lon grid.

    tickmark = create "tickmarks" tickMarkClass wks
       "vpXF"            : vpxf       ; Set the viewport location of the
       "vpYF"            : vpyf       ; tick marks to be the same as 
       "vpWidthF"        : vpwf       ; the map.
       "vpHeightF"       : vphf

       "tmYLDataBottomF" : min_lat    ; Use the lat/lon limits
       "tmYLDataTopF"    : max_lat    ; from the coordinate variables.
       "tmXBDataLeftF"   : min_lon
       "tmXBDataRightF"  : max_lon

       "tmXBMode"         : "Explicit" ; Indicate that we want to 
       "tmXBLabels"       : lonlabels  ; explicitly label the X axis.
       "tmXBValues"       : lonvalues
       "tmXBMinorValues"  : lonvalues@minor

       "tmYLMode"         : "Explicit" ; Indicate that we want to 
       "tmYLLabels"       : latlabels  ; explicitly label the Y axis.
       "tmYLValues"       : latvalues
       "tmYLMinorValues"  : latvalues@minor
    end create

    attsetvalues_check(tickmark,tmres)

; Point tick marks outward and make labels the same size.

    gsnp_point_tickmarks_outward(tickmark,tmres,-1.,-1.,-1.,-1.,-1.,-1.)
    gsnp_uniform_tickmark_labels(tickmark,tmres,0.)

;
; Add the tick mark object as an annotation of the map object, so that
; whenever the map object is drawn the tick mark object will also be
; drawn.  It will also be rescaled automatically.
;
    anno = NhlAddAnnotation(map,tickmark)
    setvalues anno
      "amZone"         : 0     ; Zone 0 centers tick marks over map.
      "amResizeNotify" : True  ; Resize tick marks if map resized.
    end setvalues

    map@tickmarks = tickmark

    return
end

;***********************************************************************;
; Procedure : gsn_add_cyclic_point                                      ;
;             data[*][*]: numeric                                       ;
;                                                                       ;
; Add a cyclic point in "x" to the 2D array "data".                     ;
; For a lat/lon plot "x"  corresponds to "lon"                          ;
;                    "ny" corresponds to "nlat"                         ;
;                    "mx" corresponds to "mlon"                         ;
;                                                                       ;
; If a lon coordinate doesn't exist, then the data is tested to see if  ;
; it contains a "lon2d" attribute, and uses this instead.               ;
;***********************************************************************;

function gsn_add_cyclic_point(data[*][*]:numeric)
local dims, newdata, ny, mx, mx1, range, delta, first_val, second_val, \
last_val
begin
    dims    = dimsizes(data)
    ny      = dims(0)
    mx      = dims(1)
    mx1     = mx+1

    newdata = new((/ny, mx1/),typeof(data))
    newdata(:,0:mx-1) = data             ; pass everything
    newdata(:,mx)     = (/ data(:,0) /)  ; value only

    if((.not.ismissing(newdata!1)) .and. iscoord(newdata,newdata!1)) then 
      first_val  = newdata&$newdata!1$(0)
      second_val = newdata&$newdata!1$(1)
      last_val   = newdata&$newdata!1$(mx-1)
      delta      = second_val - first_val
      range      = last_val - first_val
    else
      if(isatt(newdata,"lon2d").and. \
         all(dimsizes(data@lon2d).eq.dimsizes(data))) then
;
; Have to store lon2d to a local variable, because NCL doesn't let you 
; subscript attributes that are greater than one dimension.
;
        lon2d = newdata@lon2d
;
; We are not guaranteed that the lons start around 0, and end around
; 359. In fact, we have no idea where the 0 to 360 jump might be.
; So, to calculate the delta x, we should calculate it from both
; ends of the data, just to be safe.  One of them has to be the
; correct delta.  The correct delta will be the positive one, because
; at the "split", the delta will be negative (i.e. 0.5 - 358.5).
;
        first_val            = lon2d(0,0)
        second_val           = lon2d(0,1)
        second_to_last_val   = lon2d(0,mx-2)
        last_val             = lon2d(0,mx-1)
        dx1 = second_val - first_val
        dxn = last_val - second_to_last_val
        if(dx1.gt.0) then
          delta = lon2d(:,1) - lon2d(:,0)
        else
          delta = lon2d(:,mx-1) - lon2d(:,mx-2)
        end if 
        range = dim_max(lon2d) - dim_min(lon2d)
      else
        return(newdata)
      end if
    end if

    if(any((range+delta).lt.359.))
      print("gsn_add_cyclic: Warning: The range of your longitude data is not 360.")
      print("You may want to set gsnAddCyclic to False to avoid a warning")
      print("message from the spline function.")
    end if

    if((.not.ismissing(newdata!1)) .and. iscoord(newdata,newdata!1)) then 
      newdata&$newdata!1$(mx) = newdata&$newdata!1$(0) + 360
    else 
      lon2d               = newdata@lon2d
      lat2d               = newdata@lat2d
      new_lon2d           = new((/ny,mx1/),typeof(lon2d))
      new_lat2d           = new((/ny,mx1/),typeof(lat2d))
      new_lon2d(:,0:mx-1) = lon2d
      new_lon2d(:,mx)     = lon2d(:,0) + 360.
      new_lat2d(:,0:mx-1) = lat2d
      new_lat2d(:,mx)     = (new_lat2d(:,0) + new_lat2d(:,mx-1))/2.
      delete(newdata@lon2d)
      delete(newdata@lat2d)
      newdata@lon2d       = new_lon2d
      newdata@lat2d       = new_lat2d

      delete(lon2d)
      delete(lat2d)
    end if

    return(newdata)
end
 
;***********************************************************************;
; Function : pop_tick_locs                                              ;
;             lonorlat : numeric                                        ;
;                 locs : numeric                                        ;
;                                                                       ;
; This function calculates tickmark values for placing tick marks on a  ;
; plot that has a POP grid overlaid on it.                              ;
;                                                                       ;
;***********************************************************************;

function pop_tick_locs(lonorlat,locs)
local ret, i, in, j1, j0
begin
    ret = new(dimsizes(locs),typeof(lonorlat))
    do i =0 , dimsizes(locs)-1
        in = ind(lonorlat.gt.locs(i))
        j1 = in(0)
        j0 = in(0)-1
        delete(in)
        ret(i) = j0 + (lonorlat(j1)-locs(i))/(lonorlat(j1) -lonorlat(j0))
    end do
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
    popgrid_files  = (/"POP23PCM.nc"/) ; list of corresponding POP files

    if(.not.any(popgrid.eq.valid_popgrids))
      print("pop_latlon_grid: "+popgrid+" is not a supported POP grid.")
      print("                 Supported POP grids include: " + valid_popgrids)
      print("                 No grid will be overlaid.")
      return
    end if

    i = 0
    found = False
    do while(i.lt.dimsizes(valid_popgrids).and..not.found)
      if(popgrid.eq.valid_popgrids(i))
        a = addfile(popgrid_dir + popgrid_files(i),"r")
        found = True
      end if
      i = i + 1
    end do
;
; Set up list of latitude and longitude resources.
;
    latlon_res = True
    latlon_res@gsnDraw              = False
    latlon_res@gsnFrame             = False
    latlon_res@cnInfoLabelOn        = False
    latlon_res@cnLineLabelsOn       = False
    latlon_res@cnLevelSelectionMode = "ManualLevels"
    latlon_res@cnMinLevelValF       = -90
    latlon_res@cnMaxLevelValF       = 90
    latlon_res@cnLevelSpacingF      = 15
    latlon_res@cnLineDashPattern    = 10

    lat_object = gsn_contour(wks,a->LAT,latlon_res)

    tmplon            = a->LON - 360.0
    tmplon(242:,190)  = -999.0
    tmplon@_FillValue = -999.0

    latlon_res@cnMinLevelValF     = -90
    latlon_res@cnMaxLevelValF     = 270
    lon_object = gsn_contour(wks,tmplon,latlon_res)

    setvalues plot
      "tmXBMode"              : "Explicit"
      "tmXBValues"            : pop_tick_locs((a->LON(0,:)-360.0),\
                                   (/-90,-45,0.0,45,90,135,180,225/))
      "tmXBLabels"            : (/"90W","45W","0","45E","90E","135E",\
                                  "180E","135W"/)
      "tmYLMode"              : "EXPLICIT"
      "tmYLValues"            : pop_tick_locs(a->LAT(:,0),\
                                   (/-75,-60,-45,-30,-15,0,15,30/))
      "tmYLLabels"            : (/"75S","60S","45S","30S","15S","EQ",\
                                  "15N","30N"/)      
      "tmXBLabelFontHeightF"  : 0.015
      "tmYLLabelFontHeightF"  : 0.015
      "tmXBMajorLengthF"      : 0.0001
      "tmYLMajorLengthF"      : 0.0001
    end setvalues

    overlay(plot,lat_object)    ; Overlay latitude/longitude
    overlay(plot,lon_object)    ; lines on plot.
    return
end

;***********************************************************************;
; Function : gsn_csm_map_ce                                             ;
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

function gsn_csm_map_ce(wks:graphic,resources:logical)
local i, res, map_object, tickmark_object, calldraw, callframe, \
left_string, center_string, right_string, main_zone, min_lat, max_lat, \
min_lon, max_lon, res2, tmres, ticks_on, display_mode
begin
; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
    res2 = get_resources(resources)

; This section tests for more special resources: those that start
; with "gsn."

; Check if frame and/or draw are not supposed to be called.
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    
    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)
    if(left_string.or.center_string.or.right_string)
      main_zone   = 4
    end if

;
; If automatic tick marks are turned on, then turn off gsn_csm generated
; tickmarks.
; 
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2) then
      ticks_on = get_res_value(res2,"gsnTickMarksOn",False)
    else
      ticks_on = get_res_value(res2,"gsnTickMarksOn",True)
    end if
    lat_spacing  = get_res_value(res2,"gsnMajorLatSpacing",0.)
    lat_spacing  = get_res_value(res2,"tmYLTickSpacingF",lat_spacing)
    lon_spacing  = get_res_value(res2,"gsnMajorLonSpacing",0.)
    lon_spacing  = get_res_value(res2,"tmXBTickSpacingF",lon_spacing)
    mlat_spacing = get_res_value(res2,"gsnMinorLatSpacing",0.)
    mlon_spacing = get_res_value(res2,"gsnMinorLonSpacing",0.)
    shape_mode   = get_res_value(res2,"mpShapeMode","FixedAspectFitBB")
    limit_mode   = get_res_value(res2,"mpLimitMode","LatLon")
    if((typeof(limit_mode).eq."integer".and.(limit_mode.ne.1.and. \
                                             limit_mode.ne.5)).or. \
       (typeof(limit_mode).eq."string").and.(lower_case(limit_mode).ne. \
                                             "latlon".and. \
                                             lower_case(limit_mode).ne. \
                                             "corners"))
     print("gsn_csm_map_ce: Warning: you cannot use mpLimitMode=" + limit_mode + " with the gsm_csm_map routines.")
     return 
   end if
;
; If mpCenterLonF is set, but mpMinLonF/mpLeftCornerLonF and/or 
; mpMaxLonF/mpRightCornerLonF are not, then we need to set them.
;
    if(lower_case(limit_mode).eq."latlon")
      min_lon_str = "mpMinLonF"
      max_lon_str = "mpMaxLonF"
      min_lat_str = "mpMinLatF"
      max_lat_str = "mpMaxLatF"
    else
      if(lower_case(limit_mode).eq."corners")
        min_lon_str = "mpLeftCornerLonF"
        max_lon_str = "mpRightCornerLonF"
        min_lat_str = "mpLeftCornerLatF"
        max_lat_str = "mpRightCornerLatF"
      end if
    end if
    if(res2.and.isatt(res2,"mpCenterLonF").and.\
           .not.isatt(res2,min_lon_str).and.\
           .not.isatt(res2,max_lon_str))
      res2@$min_lon_str$ = res2@mpCenterLonF - 180
      res2@$max_lon_str$ = res2@mpCenterLonF + 180
    end if
  
    if(res2.and.isatt(res2,"mpCenterLonF").and..not.isatt(res2,min_lon_str))
      res2@$min_lon_str$ = res2@mpCenterLonF -  \
                           (res2@$max_lon_str$-res2@mpCenterLonF)
    end if
  
    if(res2.and.isatt(res2,"mpCenterLonF").and..not.isatt(res2,max_lon_str))
      res2@$max_lon_str$ = res2@mpCenterLonF + \
                         (res2@mpCenterLonF-res2@$min_lon_str$)
    end if
  
    min_lon = get_res_value(res2,min_lon_str,-180)
    max_lon = get_res_value(res2,max_lon_str, 180)
    min_lat = get_res_value(res2,min_lat_str, -90)
    max_lat = get_res_value(res2,max_lat_str,  90)

    if(min_lon.ge.max_lon)
      print("gsn_csm_map_ce: Fatal: The resources mpMinLonF/mpLeftCornerLonF must be less than the resources mpMaxLonF/mpRightCornerLonF.")
      print("Execution halted.")
      exit
    end if

    if(min_lat.ge.max_lat)
      print("gsn_csm_map_ce: Fatal: The resources mpMinLatF/mpLeftCornerLatF must be less than the resources mpMaxLatF/mpRightCornerF.")
      print("Execution halted.")
      exit
    end if
; 
; Determine what the default width and height should be depending
; on shape of plot.
;
    lat_range = tofloat(max_lat - min_lat)
    lon_range = tofloat(max_lon - min_lon)

    ratio1 = lat_range/lon_range
    ratio2 = lon_range/lat_range
    ratio = min((/ratio1,ratio2/))

    ratios  = (/0.50, 0.75, 1.00/)  ; lat/lon ratios
    wharray = (/0.80, 0.70, 0.52/)  ; default widths/heights

    wh_ind    = ind(ratio.le.ratios)
    def_vpwf  = wharray(wh_ind(0))    ; default width for plot
    def_vphf  = def_vpwf              ; default height for plot

    set_attr(res2,"vpWidthF",min((/lon_range/lat_range,1./)) * def_vpwf)

    set_attr(res2,"vpHeightF",min((/lat_range/lon_range,1./)) * def_vphf)
    set_attr(res2,"vpXF",(1. - res2@vpWidthF)/2.)
    set_attr(res2,"vpYF",1. - (1. - res2@vpHeightF)/2.)
    set_attr(res2,"mpGridAndLimbOn", False)
    set_attr(res2,"mpFillOn", True)
    set_attr(res2,"mpFillColors",(/"background","transparent","LightGray",\
                                   "transparent"/)) ; (default,ocean,land,
                                                    ;  inland water)
;
; By default, mpOutlineOn is False, unless mpFillOn is set to False,
; then it is set back to True.
;
    set_attr(res2,"mpOutlineOn",.not.res2@mpFillOn)
;    
; Create the map object. We have to use this code instead of gsn_map
; because we want to set the size of the map.  If we set the size of the
; map later with setvalues, as gsn_map would do, then the size wouldn't
; be correct.
;
    if(lower_case(limit_mode).eq."latlon")
      map_object = create "map" mapPlotClass wks
        "vpXF"               : res2@vpXF
        "vpYF"               : res2@vpYF
        "vpWidthF"           : res2@vpWidthF
        "vpHeightF"          : res2@vpHeightF
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
      end if
    end if
    delete(min_lon)
    delete(max_lon)
    delete(min_lat)
    delete(max_lat)
    delete(lon_range)
    delete(lat_range)

; This section tests for regular resources.

    tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
    if(.not.ticks_on)
      tmres = True
      gsnp_turn_off_tickmarks(tmres)
    end if
    mpres = get_res_ne(res2,(/"tx","am"/))  ; Get rest of resources
    attsetvalues_check(map_object,mpres)
;
; Add tickmark object, even if the tickmarks have been turned off, because
; we need to get the size of the X/Y axis labels.
;
    add_map_tickmarks(wks,map_object,tmres,lon_spacing,mlon_spacing,\
                      lat_spacing,mlat_spacing)

; Make sure axes labels (if any) are the same size.

    getvalues map_object@tickmarks
      "tmYLLabelFontHeightF"   : yfontf
      "tmXBLabelFontHeightF"   : xfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the tickmark labels.
    font_height = 1.25 * font_height      ; Make fonts 25% bigger.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value_keep(res2,"tiMainFontHeightF", \
                                          1.3*font_height)
    main_font = get_res_value_keep(res2,"tiMainFont","helvetica-bold")

    setvalues map_object
      "tiMainFontHeightF"  : main_font_height ; main title size
      "tiMainFont"         : main_font        ; main title font
    end setvalues

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    set_attr(subres,"amOrthogonalPosF",0.05)
    add_subtitles(wks,map_object,left_string,center_string,right_string, \
                  subres)

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and tickmark object.

    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_map_polar                                          ;
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
;    4. If gsnPolarLabelSpacing is set, then this spacing will be used  ;
;       to label the longitude lines.                                   ;
;    5. If gsnPolarLabelDistance is set, then this will be used to      ;
;       increase/decrease the default position of the labels from the   ;
;       perimeter of the map.                                           ;
;***********************************************************************;

function gsn_csm_map_polar(wks:graphic,resources:logical)
local i, res, res2, map_object, main_zone, \
calldraw, callframe, left_string, center_string, right_string, yoffset, \
min_lat, max_lat, center_lat, main_offset, \
side, just, orth, para, quad1, quad2, quad3, quad4, topbot, xcen, ycen, \
vpxf, vpyf, vphf, vpwf, plon, plat, xndc, yndc, polar_type
begin

; Initialize.
    main_offset   = 0.02
    min_lat       = 0.0
    max_lat       = 90.0
    center_lat    = 0.0 
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = get_resources(resources)

; This section tests for special resources: those that start with "gsn."

    calldraw      = get_res_value(res2,"gsnDraw", True)
    callframe     = get_res_value(res2,"gsnFrame",True)
    maxbb         = get_bb_res(res2)
    polar_dst     = get_res_value(res2,"gsnPolarLabelDistance",1.04)
    ticks_on      = get_res_value(res2,"gsnTickMarksOn",True)
    polar_time    = get_res_value(res2,"gsnPolarTime",False)
    ut            = tofloat(get_res_value(res2,"gsnPolarUT",0.))

    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)
    if(left_string.or.center_string.or.right_string)
      main_zone   = 4
    end if
;
; Check for type of polar plot desired.
;
    polar_type = get_polar_type(res2)

    if(polar_type.eq."NH")
      min_lat    =   0 
      max_lat    =  90
      center_lat =  90
    else
      min_lat    = -90
      max_lat    =   0
      center_lat = -90
    end if

    height = get_res_value(res2,"vpHeightF",0.62)
    width  = get_res_value(res2,"vpWidthF",0.62)
    xpos   = get_res_value(res2,"vpXF",0.2)
    ypos   = get_res_value(res2,"vpYF",0.8)

;
; Print a warning if user tries to set mpMinLonF and/or mpMaxLonF,
; since these values should not be messed with in a polar plot (they
; should be -180 and 180.
;
    if(isatt(res2,"mpMinLonF").or.isatt(res2,"mpMaxLonF")) then
      print("gsn_csm_map_polar: Warning: you should not set the mpMinLonF and/or mpMaxLonF resources.")
      print("Setting these resources to something other than -180 and 180 may produce unexpected results.")
    end if

;
; Check for other resources that we need to set at the time the
; map is created.
;
    limit_mode   = get_res_value(res2,"mpLimitMode","LatLon")
    shape_mode   = get_res_value(res2,"mpShapeMode","FixedAspectFitBB")

; Create a polar stereographic map object.

    map_object = create "ezplot1p_map" mapPlotClass wks
      "vpHeightF"            : height
      "vpWidthF"             : width
      "vpXF"                 : xpos
      "vpYF"                 : ypos

      "mpProjection"         : "Stereographic"
      "mpEllipticalBoundary" : True
      "mpLimitMode"          : limit_mode
      "mpShapeMode"          : shape_mode   ; must be set when map created

      "mpFillOn"             : True
      "mpFillColors"         : (/"background","transparent","LightGray",\
                                 "transparent"/)
      "pmTitleDisplayMode"   : "Always"
      "pmTitleZone"          : main_zone
    end create

; Set some map plot resources.

    res2 = True
    set_attr(res2,"mpMinLatF",min_lat)
    set_attr(res2,"mpMaxLatF",max_lat)
    set_attr(res2,"mpCenterLatF",center_lat)
    set_attr(res2,"mpFillOn",True)
    set_attr(res2,"mpGridAndLimbOn",True)
    set_attr(res2,"mpGridLineDashPattern",2)
    set_attr(res2,"mpGridLonSpacingF",30.)
    set_attr(res2,"mpPerimOn",True)
;
; If labeling the polar plot with time values, then we want slt = 0
; to be at the bottom of the plot, so we need to rotate the map 
; accordingly, by setting mpCenterLonF.
;
;  If slt = ut + lon/15, then at slt=0:
;
;   0 = ut + lon/15  --> lon/15 = -ut -->  lon = -ut*15.
;
    if(polar_time) then
      set_attr(res2,"mpCenterLonF",-ut*15)
    end if 
;
; Set the spacing of the polar labels to the same as mpGridLonSpacingF,
; unless user has explicitly set gsnPolarLabelSpacing.
;
    label_spacing = get_res_value(res2,"gsnPolarLabelSpacing",  \
                                  res2@mpGridLonSpacingF)
;
; By default, mpOutlineOn is False, unless mpFillOn is set to False,
; then it is set back to True.
;
    set_attr(res2,"mpOutlineOn",.not.res2@mpFillOn)

; This section tests for regular resources.
    mpres = get_res_ne(res2,(/"gs","tx"/))

    attsetvalues_check(map_object,mpres)

; Retrieve the view port location of the map plot so we know where
; to put titles.

    getvalues map_object
      "vpXF"      : vpxf
      "vpYF"      : vpyf
      "vpWidthF"  : vpwf
      "vpHeightF" : vphf
      "mpMinLatF" : min_lat
      "mpMaxLatF" : max_lat
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value_keep(res2,"tiMainFontHeightF", \
                                          2.*font_height)
    main_font = get_res_value_keep(res2,"tiMainFont","helvetica-bold")

    setvalues map_object
        "tiMainOffsetYF"       : main_offset      ; main title offset
        "tiMainFont"           : main_font        ; main title font
        "tiMainFontHeightF"    : main_font_height ; main title size
    end setvalues

  if(ticks_on) then
;
; Create the lat/lon coordinates where you want labels to appear.
;
; In this case, we want the labels to appear at a fixed latitude value
; and at varying longitude values.
;
      ngrid = floattoint(360/label_spacing)

      if(min((/min_lat,max_lat/)).ge.0.or.polar_type.eq."NH")
          plat = fspan(min_lat,min_lat,ngrid)
      else
          plat = fspan(max_lat,max_lat,ngrid)
      end if 
      if(.not.polar_time) then
        plon = fspan(0.,360.-label_spacing,ngrid)
      else
;
; If labeling with time, we want time "0" to be labeled, and so
; on, so make sure we shift plon accordingly.
;
        plon = fspan(0.,360.-label_spacing,ngrid) + (ut*15)
      end if

; Create arrays to hold the NDC values that we're going to convert 
; the lat/lon values to.

      xndc = new(dimsizes(plon),float)
      yndc = new(dimsizes(plat),float)

; Convert lat/lon cooridinates to NDC coordinates since we are 
; drawing the labels in NDC space and NOT in lat/lon space.

      datatondc(map_object,plon,plat,xndc,yndc)
;
; Get center of plot in NDC coordinates.
;
      xcen = vpxf + vpwf/2.     ; X center
      ycen = vpyf - vphf/2.     ; Y center

;
; Determine if we want longitude labels, or time labels.
; 
      labels = new(dimsizes(plon),string)

      if(.not.polar_time) then
; Define an array of strings to label the longitude values.
        indexes = ind(plon.gt.0.and.plon.lt.180)
        labels(indexes) = floattoint(plon(indexes)) + "E"
        delete(indexes)

        indexes = ind(plon.gt.180.and.plon.lt.360)
        labels(indexes) = floattoint(360-plon(indexes)) + "W"
        delete(indexes)

        indexes = ind(plon.eq.0.or.plon.eq.0.or.plon.eq.180)
        labels(indexes) = floattoint(plon(indexes))
        delete(indexes)
      else
;
; Define an array of strings to label the time values.
;
; Calculate slt, given ut and lon (slt = ut + lon/15)
;
        slt = ut + plon/15
        ind_lt_00 = ind(slt.lt.0)
        ind_ge_24 = ind(slt.ge.24)
        if(.not.any(ismissing(ind_lt_00))) then
          slt(ind_lt_00) = slt(ind_lt_00) + 24
        end if
        if(.not.any(ismissing(ind_ge_24))) then
          slt(ind_ge_24) = slt(ind_ge_24) - 24
        end if
        delete(ind_lt_00)
        delete(ind_ge_24)
; 
; Convert "slt" to integer. If it is equal to the float version of "slt",
; then we know that our values are effectively integers, and we can plot
; them without decimal points. Otherwise, we'll go ahead and plot them
; with decimal points.
;
        labels = new(dimsizes(slt),string)
        islt = tointeger(slt)
        if(all(slt.eq.islt)) then
          labels = islt
        else 
          labels = slt
        end if
      end if
      num_labels = dimsizes(labels)
      text_ids = new(num_labels,graphic)

;
; Check if user setting own font heights.
;
    text_font_height = get_res_value_keep(res2,"txFontHeightF",font_height)
    text_font        = get_res_value_keep(res2,"txFont","helvetica-bold")

; Create an array of TextItem objects that we'll use to label the
; lat/lon grid.  

      do i=0,num_labels-1
        text_ids(i) = create "lon_"+labels(i) textItemClass wks
          "txString"      : labels(i)
          "txFont"        : text_font
          "txFontHeightF" : text_font_height
        end create
      end do

      setvalues map_object
        "pmAnnoViews" : text_ids  ; Add text items as annotations of map.
      end setvalues

      getvalues map_object
        "pmAnnoManagers" : am_ids   ; Retrieve anno managers so we can 
                                    ; change some stuff.
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
      if(.not.all(ismissing(quad1)))
        side(quad1) = "Top"      ; Position labels in quadrants I and II wrt
        orth(quad1) = ((yndc(quad1) - ycen)/vphf) * polar_dst
        just(quad1) = "CenterLeft"
      end if
      if(.not.all(ismissing(quad2)))
        side(quad2) = "Top"      ; top of plot.
        orth(quad2) = ((yndc(quad2) - ycen)/vphf) * polar_dst
        just(quad2) = "CenterRight"
      end if
      if(.not.all(ismissing(quad3)))
        side(quad3) = "Bottom"   ; Position labels in quadrants III and IV wrt
        orth(quad3) = ((ycen - yndc(quad3))/vphf) * polar_dst
        just(quad3) = "CenterRight"
      end if
      if(.not.all(ismissing(quad4)))
        side(quad4) = "Bottom"   ; bottom of plot.
        orth(quad4) = ((ycen - yndc(quad4))/vphf) * polar_dst
        just(quad4) = "CenterLeft"
      end if

      para = ((xndc - xcen)/vpwf) * polar_dst
;
; Labels at the very top or bottom of the map should be centered, not
; right or left justified.
;
      topbot = ind(fabs(xndc-xcen).le.1e-5)
      if(.not.all(ismissing(topbot)))
        just(topbot) = "CenterCenter"
      end if
;
; map_object may already have some other annotations added to it,
; so we have to search until we find the lon label annotations.  They
; all start with "lon_", so the first occurrence of this string will
; be the consecutive start of the lon label annotations.
;
      names = NhlName(am_ids)
      found = False
      i = 0
      do while(i.lt.dimsizes(names).and..not.found)
        names_char = stringtochar(names)
        if(chartostring(names_char(i,0:3)).eq."lon_")
          start = i
          found = True
        end if
        delete(names_char)
        i = i + 1
      end do
      delete(names)
;
; Set the side, justifcation, and position of each longitude label.
;
      do i=0,num_labels-1
        setvalues am_ids(start+i)
          "amTrackData"      : False
          "amResizeNotify"   : True
          "amZone"           : 0
          "amSide"           : side(i)
          "amJust"           : just(i)
          "amOrthogonalPosF" : orth(i)
          "amParallelPosF"   : para(i)
        end setvalues
      end do
    end if

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,"tx")  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",1.2*font_height)
    set_attr(subres,"txFont","helvetica-bold")
    subres@amOrthogonalPosF = 0.06 
    add_subtitles(wks,map_object,left_string,center_string,right_string, \
                  subres)

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_map_other                                          ;
;                   wks: workstation object                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a labeled "other" map (non-polar,     ;
; cylindrical equidistant.                                              ;
;                                                                       ;
; This function behaves differently from gsn_map in that it will        ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top left, center, and right of the plot   ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;***********************************************************************;

function gsn_csm_map_other(wks:graphic,resources:logical)
local i, res, map_object, calldraw, callframe, \
left_string, center_string, right_string, main_zone, res2
begin
; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
    res2 = get_resources(resources)

; This section tests for more special resources: those that start
; with "gsn."

; Check if frame and/or draw are not supposed to be called.
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    
    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)
    if(left_string.or.center_string.or.right_string)
      main_zone   = 4
    end if

    shape_mode   = get_res_value(res2,"mpShapeMode","FixedAspectFitBB")

    set_attr(res2,"vpXF",     0.2)
    set_attr(res2,"vpYF",     0.8)
    set_attr(res2,"vpWidthF", 0.6)
    set_attr(res2,"vpHeightF",0.6)

    set_attr(res2,"mpGridAndLimbOn", False)
    set_attr(res2,"mpPerimOn", True)
    set_attr(res2,"mpFillOn", True)
    set_attr(res2,"mpFillColors",(/"background","transparent","LightGray",\
                                   "transparent"/)) ; (default,ocean,land,
                                                    ;  inland water)
;
; By default, mpOutlineOn is False, unless mpFillOn is set to False,
; then it is set back to True.
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

    mpres = get_res_ne(res2,"tx")  ; Get rest of resources
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
      "tiMainFont"         : main_font        ; main title font
    end setvalues

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",0.8*font_height)
    set_attr(subres,"amOrthogonalPosF",0.015)
    add_subtitles(wks,map_object,left_string,center_string,right_string, \
                  subres)

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and tickmark object.

    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_map                                                ;
;                   wks: workstation object                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_csm_map_ce, gsn_csm_map_other, or      ;
; gsn_csm_map_polar depending on if gsnPolar is set, or how             ;
; mpProjection is set.                                                  ;
;***********************************************************************;

function gsn_csm_map(wks:graphic,resources:logical)
begin
  res2 = get_resources(resources)

  if(res2.and.(isatt(res2,"gsnPolarNH").or.isatt(res2,"gsnPolarSH").or.\
               isatt(res2,"gsnPolar")))
    return(gsn_csm_map_polar(wks,res2))
  else
    if(.not.isatt(res2,"mpProjection").or.\
       check_attr(res2,"mpProjection","cylindricalequidistant",True))
      return(gsn_csm_map_ce(wks,res2))
    else   
      return(gsn_csm_map_other(wks,res2))
    end if
  end if
end


;***********************************************************************;
; Function : gsn_csm_contour_map_polar                                  ;
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

function gsn_csm_contour_map_polar(wks:graphic,data[*][*]:numeric, \
                                resources:logical)
local i, contour_object, map_object, res, lbar_pos, lbar_zone, \
lbar_on, calldraw, callframe, main_offset, datanew, res2, cnres, mpres, \
vpwf, vphf, font_height, levels, colors, zonal_zone, lbres, \
infolabel_on, infolabel_zone, lbar_zone, lbar_height, lbar_width
begin

; Initialize.
    lbar_on       = False     ; Labelbar flag
    res2          = get_resources(resources) ; Copy of resources
    lbar_zone     = 3         ; Zone for labelbar (may change later)
    mpres         = True      ; Will hold map resources

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
        datanew = gsn_add_cyclic_point(data)
    else
        datanew = data
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

; Check for draw and frame.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

;
; Check for type of polar plot and polar labels desired.
;
    mpres@gsnPolar     = get_polar_type(res2)
    mpres@gsnPolarTime = get_res_value(res2,"gsnPolarTime",False)
    mpres@gsnPolarUT   = get_res_value(res2,"gsnPolarUT",0.)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
;
; Tickmarks.
;
    mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
;
; If gsnPolar* resources are set, be sure to retrieve them with
; "get_res_value" so that they don't get passed onto the contour plot.
;
    if(isatt(res2,"gsnPolarLabelSpacing"))
      mpres@gsnPolarLabelSpacing = get_res_value(res2,  \
                                                 "gsnPolarLabelSpacing",0)
    end if

    if(isatt(res2,"gsnPolarLabelDistance"))
      mpres@gsnPolarLabelDistance = get_res_value(res2,  \
                                                 "gsnPolarLabelDistance",0)
    end if

; Create data object and use coordinate variables if they exist.

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
    cnres           = True 
    mpres           = True 
    cnres@gsnDraw   = False
    cnres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
    overlay(map_object,contour_object)              ; Overlay contour plot
                                                    ; on map.
;
; Retrieve the view port location of the map plot so we know where
; to put titles and labelbar (if there is one).
;
    getvalues map_object
      "vpWidthF"  : vpwf
      "vpHeightF" : vphf
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value_keep(res2,"tiMainFontHeightF", \
                                          2.*font_height)
    main_font = get_res_value_keep(res2,"tiMainFont","helvetica-bold")

    setvalues map_object
        "tiMainFont"           : main_font        ; main title font
        "tiMainFontHeightF"    : main_font_height ; main title size
    end setvalues

; Create a labelbar.

    if(lbar_on)
      add_labelbar(wks,contour_object,lbar_zone,font_height,"polar",lbres)
    end if
  
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data    = contour_object@data
    map_object@contour = contour_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_contour_map_ce                                     ;
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

function gsn_csm_contour_map_ce(wks:graphic,data[*][*]:numeric,\
                                resources:logical)
local i, contour_object, labelbar_object, xy_object, map_object, \
calldraw, callframe, lbar_on, zonal_mean_plot, min_lat, max_lat, datanew, \
res, res2, lbres, xy_res, mpres, cnres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf, contour_plot
begin
; Initialize.
    lbar_on        = False    ; Default is no labelbar.
    zonal_mean_plot= False    ; Default is no zonal mean plot.
    mpres          = True

    infolabel_zone = 2        ; Zone for info label (may change later)
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    zonal_zone     = 2        ; Zone for zonal means plot

    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
        datanew = gsn_add_cyclic_point(data)
    else
        datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay contour on map.

    check_for_y_lat_coord(datanew,res2,"contour")
    check_for_lon_coord(datanew,res2,"contour")

    if(.not.(isatt(res2,"sfXArray")))
      set_attr(res2,"sfXCStartV",-180.)
      set_attr(res2,"sfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"sfYArray")))
      set_attr(res2,"sfYCStartV",-90.)
      set_attr(res2,"sfYCEndV",   90.)
    end if

; Check if a zonal mean plot is desired.

    if(res2.and.isatt(res2,"gsnZonalMean"))
      if(any(datanew!0.eq.get_allowed_latnames()).and.\
         any(datanew!1.eq.get_allowed_lonnames()))
        if(res2@gsnZonalMean)
          if(isatt(res2,"vpHeightF"))
            map_vphf = res2@vpHeightF
          else
            map_vphf  = 0.7 ; make sure zonalplot will fit in view port

            set_attr(res2,"vpHeightF",0.7)
            set_attr(res2, "vpWidthF",0.7)
            set_attr(res2,     "vpXF",0.1)
            set_attr(res2,     "vpYF",0.9)

          end if
          zonal_mean_plot = True
          zmeans2 = dim_avg(datanew)
          if(min(zmeans2).lt.0.and.max(zmeans2).gt.0)
            nmeans = dimsizes(zmeans2)
            zmeans = new((/2,nmeans/),typeof(zmeans2))
            zmeans(0,:) = zmeans2
            zmeans(1,:) = fspan(0.,0.,nmeans)
          else
            zmeans = zmeans2
          end if
          delete(zmeans2)
        end if
      else
        print("gsn_csm_contour_map_ce: Warning: The resource gsnZonalMean can only be set to True")
        print("if the data has one of the coordinate variables " + get_allowed_latnames() + "and " + get_allowed_lonnames())
      end if
      delete(res2@gsnZonalMean)
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
    if(isatt(res2,"gsnMajorLatSpacing"))
      mpres@gsnMajorLatSpacing = res2@gsnMajorLatSpacing
      delete(res2@gsnMajorLatSpacing)
    end if

    if(isatt(res2,"gsnMinorLonSpacing"))
      mpres@gsnMinorLonSpacing = res2@gsnMinorLonSpacing
      delete(res2@gsnMinorLonSpacing)
    end if
    if(isatt(res2,"gsnMinorLatSpacing"))
      mpres@gsnMinorLatSpacing = res2@gsnMinorLatSpacing
      delete(res2@gsnMinorLatSpacing)
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
;
; Tickmarks.
;
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2) then
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",False)
    else
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    end if

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    cnres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))

    if(cnres)
      if(check_attr(cnres,"cnFillOn",True,False))
        set_attr(cnres,"cnInfoLabelOn",False)
        if(.not.check_attr(lbres,"lbLabelBarOn",False,False))
          set_attr(cnres,"cnLineLabelsOn",False)
          lbar_on = True   ; Turn on a labelbar
          if(check_attr(lbres,"lbOrientation","vertical",True))
            set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
            set_attr(mpres,"vpHeightF",0.75)   ; on the side.
            set_attr(mpres,     "vpXF",0.08)
            set_attr(mpres,     "vpYF",0.90)
          end if
        end if
      end if
    end if
;
; Compute zones for zonal means plot, info label, and labelbar if
; they are supposed to get drawn.
;
    infolabel_on = get_res_value_keep(cnres,"cnInfoLabelOn",True)

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

    if(infolabel_on)
      if(.not.isatt(cnres,"cnInfoLabelOrthogonalPosF").and.infolabel_zone.eq.2)
        cnres@cnInfoLabelOrthogonalPosF = 0.13
      end if
      cnres@cnInfoLabelZone = infolabel_zone
    end if

; Before we create the objects, turn off draw and frame for them.
    cnres           = True
    mpres           = True
    cnres@gsnDraw   = False
    cnres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
    overlay(map_object,contour_object)              ; Overlay contours on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues map_object@tickmarks
      "tmXBLabelFontHeightF" : xbfontf
      "tmXBMajorLengthF"     : xlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

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
                                          1.3*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Set font heights only if they haven't been set explicitly by user.

    contour_plot = check_class_name(contour_object,"contour")

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

; Create a labelbar.

    if(lbar_on)
      add_labelbar(wks,contour_object,lbar_zone,xbfontf,"ce",lbres)
    end if

; Add a zonal mean plot if requested.

    if(zonal_mean_plot)

; Retrieve the view port size of the map plot so we know where
; to put zonal means plot (if there is one).

      getvalues map_object
        "vpHeightF"    : vphf
        "mpMinLatF"    : min_lat
        "mpMaxLatF"    : max_lat
      end getvalues

; Create data object.
      if(any(datanew!0.eq.get_allowed_latnames()))
        data_object = create "data" coordArraysClass noparent
          "caXArray" : zmeans
          "caYArray" : datanew&$datanew!0$
        end create
; Check for missing values.
        if(isatt(zmeans,"_FillValue")) then
          setvalues data_object
            "caXMissingV" :zmeans@_FillValue
          end setvalues
        end if

; Create zonal means XY plot.
        xy_object = create "xyplot" xyPlotClass wks
          "xyCoordData"            : data_object
          "vpHeightF"              : vphf
          "vpWidthF"               : 0.15 * map_vphf
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

; Add XY plot as annotation of map plot.
        anno = NhlAddAnnotation(map_object,xy_object)
        setvalues anno
          "amZone"          : zonal_zone   ; Just outside plot area
          "amSide"          : "right"      ; Plot at right.
          "amResizeNotify"  : True         ; Resize if map resized.
          "amParallelPosF"  : 0.5          ; 0.5 is center of plot
          "amOrthogonalPosF": 0.05         ; Move away from right edge
        end setvalues
      end if

    end if  

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data    = contour_object@data
    map_object@contour = contour_object
    if(zonal_mean_plot)
      map_object@xy = xy_object
    end if
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_contour_map_other                                  ;
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
;    3. If the resource "cnFillOn" is set to True, then a labelbar is   ;
;       drawn and line labels are turned off.                           ;
;    4. If data has an attribute called "long_name," and "gsnLeftString";
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the left string title.                              ;
;    5. If data has an attribute called "units," and "gsnRightString"   ;
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the right string title.                             ;
;***********************************************************************;

function gsn_csm_contour_map_other(wks:graphic,data[*][*]:numeric,\
                                   resources:logical)
local i, contour_object, labelbar_object, xy_object, map_object, \
calldraw, callframe, lbar_on, min_lat, max_lat, datanew, \
res, res2, lbres, xy_res, mpres, cnres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf, contour_plot
begin
; Initialize.
    lbar_on        = False    ; Default is no labelbar.
    mpres          = True

    infolabel_zone = 2        ; Zone for info label (may change later)
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    zonal_zone     = 2        ; Zone for zonal means plot

    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
        datanew = gsn_add_cyclic_point(data)
    else
        datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay contour on map.

    check_for_y_lat_coord(datanew,res2,"contour")
    check_for_lon_coord(datanew,res2,"contour")

; Create some contour plot resources.

    res2 = True
    set_attr(res2,"cnLineLabelBackgroundColor", "transparent")

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    cnres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))

    if(cnres)
      if(check_attr(cnres,"cnFillOn",True,False))
        set_attr(cnres,"cnInfoLabelOn",False)
        if(.not.isatt(lbres,"lbLabelBarOn").or.\
          check_attr(lbres,"lbLabelBarOn",True,False))
          set_attr(cnres,"cnLineLabelsOn",False)
          lbar_on = True   ; Turn on a labelbar
          if(check_attr(lbres,"lbOrientation","vertical",True))
            set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
            set_attr(mpres,"vpHeightF",0.75)   ; on the side.
            set_attr(mpres,     "vpXF",0.08)
            set_attr(mpres,     "vpYF",0.85)
          end if
        end if
      end if
    end if
;
; Compute zones for info label and labelbar if
; they are supposed to get drawn.
;
    infolabel_on = get_res_value_keep(cnres,"cnInfoLabelOn",True)
    if(infolabel_on)
      lbar_zone = infolabel_zone + 1
      cnres@cnInfoLabelZone = infolabel_zone
    end if

; Before we create the objects, turn off draw and frame for them.
    cnres           = True
    mpres           = True
    cnres@gsnDraw   = False
    cnres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
    overlay(map_object,contour_object)              ; Overlay contours on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues map_object
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.1*font_height)
    setvalues map_object
      "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Set font heights only if they haven't been set explicitly by user.

    contour_plot = check_class_name(contour_object,"contour")

    if(.not.isatt(cnres,"cnLineLabelFontHeightF"))
      setvalues contour_plot
        "cnLineLabelFontHeightF"    : 0.6 * font_height
      end setvalues
    end if

    if(.not.isatt(cnres,"cnInfoLabelFontHeightF"))
      setvalues contour_plot
        "cnInfoLabelFontHeightF"  : 0.6 * font_height
      end setvalues
    end if

; Create a labelbar.

    if(lbar_on)
      add_labelbar(wks,contour_object,lbar_zone,0.6 * font_height,"other",lbres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data    = contour_object@data
    map_object@contour = contour_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_vector_map_ce                                      ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u,v" is the 2-dimensional data to be vectorized,   ;
; and "resources" is an optional list of resources. The Id of the map   ;
; plot is returned.                                                     ;
;                                                                       ;
; This function behaves differently from gsn_vector in that it will     ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. The lat/lon grid is labeled with tickmarks.                     ;
;    2. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top left, center, and right of the plot   ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;    3. If data has an attribute called "long_name," and "gsnLeftString";
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the left string title.                              ;
;    4. If data has an attribute called "units," and "gsnRightString"   ;
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the right string title.                             ;
;    5. If the resource "vcMonoLineArrowColor" is set to False, then a  ;
;       labelbar is drawn.                                              ;
;***********************************************************************;

function gsn_csm_vector_map_ce(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                                resources:logical)
local i, vector_object, map_object, res, res2, calldraw, vcres, \
lbres, map_vpwf, map_vphf, vpwf, vphf, callframe, min_lat, max_lat, \
lbar_on, lbar_zone, unew, vnew
begin

; Initialize.
    annolabel_zone = 2        ; Zone for vector anno label
    mpres          = True
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    lbar_on        = False

    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vector on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
      set_attr(res2,"vfYCStartV",-90.)
      set_attr(res2,"vfYCEndV",   90.)
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

;
; Check for other special resources.
;
    if(isatt(res2,"gsnMajorLonSpacing"))
      mpres@gsnMajorLonSpacing = res2@gsnMajorLonSpacing
      delete(res2@gsnMajorLonSpacing)
    end if
    if(isatt(res2,"gsnMajorLatSpacing"))
      mpres@gsnMajorLatSpacing = res2@gsnMajorLatSpacing
      delete(res2@gsnMajorLatSpacing)
    end if

    if(isatt(res2,"gsnMinorLonSpacing"))
      mpres@gsnMinorLonSpacing = res2@gsnMinorLonSpacing
      delete(res2@gsnMinorLonSpacing)
    end if
    if(isatt(res2,"gsnMinorLatSpacing"))
      mpres@gsnMinorLatSpacing = res2@gsnMinorLatSpacing
      delete(res2@gsnMinorLatSpacing)
    end if

;
; Tickmarks.
;
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2) then
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",False)
    else
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm"/))
    lbres = get_res_eq(res2,(/"lb","pm"/))

    if(vcres)
      if(check_attr(vcres,"vcMonoLineArrowColor",False,False))
        if(.not.isatt(lbres,"lbLabelBarOn").or.\
          check_attr(lbres,"lbLabelBarOn",True,False))
          lbar_on = True   ; Turn on a labelbar
          if(check_attr(lbres,"lbOrientation","vertical",True))
            set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
            set_attr(mpres,"vpHeightF",0.75)   ; on the side.
            set_attr(mpres,     "vpXF",0.08)
            set_attr(mpres,     "vpYF",0.90)
          end if
        end if
      end if
    end if
;
; Default is for the vector reference anno label to be on.  To be
; turned off, it has to be explicitly turned off.
;
    refanno_on = get_res_value_keep(vcres,"vcRefAnnoOn",True)
    if(refanno_on)
      lbar_zone    = annolabel_zone + 1
    end if

; Before we create the objects, turn off draw and frame for them.
    vcres = True
    mpres = True
    vcres@gsnDraw   = False
    vcres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False
    set_attr(vcres,"vcRefAnnoZone",annolabel_zone)
    set_attr(vcres,"vcRefAnnoOrthogonalPosF", 0.12)
    set_attr(vcres,"vcRefAnnoString2On","False")

    vector_object = gsn_vector(wks,unew,vnew,vcres) ; Create vectors

    map_object = gsn_csm_map(wks,mpres)            ; Create map.
    overlay(map_object,vector_object)               ; Overlay vectors on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the anno label size the same as the tick mark label size.

    getvalues map_object@tickmarks
      "tmXBLabelFontHeightF" : xbfontf
      "tmXBMajorLengthF"     : xlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

    getvalues map_object
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.3*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

    if(.not.isatt(vcres,"vcRefAnnoFontHeightF"))
      setvalues vector_object
        "vcRefAnnoFontHeightF"    : xbfontf
      end setvalues
    end if

; Create a labelbar.

    if(lbar_on)
      add_labelbar(wks,vector_object,lbar_zone,xbfontf,"vector",lbres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data   = vector_object@data
    map_object@vector = vector_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_vector_map_polar                                   ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a polar            ;
; stereographic map projection to the workstation "wks" (the variable   ;
; returned from a previous call to "gsn_open_wks").  "u,v" is the       ;
; 2-dimensional data to be vectorized, and "resources" is an optional   ;
; list of resources. The Id of the map plot is returned.                ;
;                                                                       ;
; This function behaves differently from gsn_vector in that it will     ;
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
;    4. If the resource "vcMonoLineArrowColor" is set to False, then a  ;
;       labelbar is drawn.                                              ;
;***********************************************************************;

function gsn_csm_vector_map_polar(wks:graphic,u[*][*]:numeric, \
                                  v[*][*]:numeric,resources:logical)
local i, vector_object, map_object, res, res2, calldraw, vcres, \
lbres, map_vpwf, map_vphf, vpwf, vphf, callframe, min_lat, max_lat, \
lbar_on, lbar_zone, unew, vnew
begin

; Initialize.
    lbar_on        = False
    annolabel_zone = 2        ; Zone for vector anno label
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    mpres          = True
    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check for draw and frame.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

;
; Check for type of polar plot desired.
;
    mpres@gsnPolar = get_polar_type(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
;
; Tickmarks.
;
    mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)

; Create data object and use coordinate variables if they exist.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm"/))
    lbres = get_res_eq(res2,(/"lb","pm"/))

    if(vcres)
      if(check_attr(vcres,"vcMonoLineArrowColor",False,False))
        if(.not.isatt(lbres,"lbLabelBarOn").or.\
          check_attr(lbres,"lbLabelBarOn",True,False))
          lbar_on = True   ; Turn on a labelbar
        end if
      end if
    end if

    if(lbar_on)
      if(check_attr(lbres,"lbOrientation","vertical",True))
        set_attr(mpres,"vpXF",0.15)
        set_attr(vcres,"vcRefAnnoOrthogonalPosF",0.04)
        set_attr(lbres,"pmLabelBarOrthogonalPosF",0.1)
      else
        set_attr(mpres,"vpYF",0.82)
      end if
    end if
;
; Default is for the vector reference anno label to be on.  To be
; turned off, it has to be explicitly turned off.
;
    refanno_on = get_res_value_keep(vcres,"vcRefAnnoOn",True)
    if(refanno_on)
      lbar_zone    = annolabel_zone + 1
    end if

; Before we create the objects, turn off draw and frame for them.
    vcres = True
    mpres = True
    vcres@gsnDraw   = False
    vcres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False
    set_attr(vcres,"vcRefAnnoZone",annolabel_zone)
    set_attr(vcres,"vcRefAnnoOrthogonalPosF", 0.01)
    set_attr(vcres,"vcRefAnnoParallelPosF", 0.8)
    set_attr(vcres,"vcRefAnnoString2On","False")

    vector_object = gsn_vector(wks,unew,vnew,vcres) ; Create vectors
    map_object    = gsn_csm_map(wks,mpres)          ; Create map.
    overlay(map_object,vector_object)               ; Overlay vectors on map.

;
; Retrieve the view port location of the map plot so we know where
; to put titles and labelbar (if there is one).
;
    getvalues map_object
      "vpWidthF"  : vpwf
      "vpHeightF" : vphf
    end getvalues
; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     2.*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Create a labelbar.

    if(lbar_on)
      add_labelbar(wks,vector_object,lbar_zone,font_height,"vector",lbres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data   = vector_object@data
    map_object@vector = vector_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_vector_map_other                                   ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u"/"v" is the 2-dimensional data to be vectored,   ;
; and "resources" is an optional list of resources. The Id of the map   ;
; plot is returned.                                                     ;
;                                                                       ;
; This function behaves differently from gsn_vector in that it will     ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. The lat/lon grid is labeled with tickmarks.                     ;
;    2. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top left, center, and right of the plot   ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;    3. If the resource "vcMonoLineArrowColor" is set to False, then a  ;
;       labelbar is drawn.                                              ;
;***********************************************************************;

function gsn_csm_vector_map_other(wks:graphic,u[*][*]:numeric,\
                                  v[*][*]:numeric,resources:logical)
local i, vector_object, labelbar_object, xy_object, map_object, \
calldraw, callframe, lbar_on, min_lat, max_lat, datanew, \
res, res2, lbres, xy_res, mpres, vcres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf 
begin
; Initialize.
    lbar_on        = False    ; Default is no labelbar.
    mpres          = True

    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vectors on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
;
; By default, mpOutlineOn is False, unless vcMonoLineArrowColor is set to
; False or mpFillOn is set to False, then it is set back to True.
;
   if(check_attr(res2,"vcMonoLineArrowColor",False,False).or.\
      check_attr(res2,"mpFillOn",False,False))
      set_attr(res2,"mpOutlineOn",True)
    end if

; This section tests for regular resources.
    lbres = get_res_eq(res2,(/"lb","pm"/))
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))

    if(vcres)
      if(check_attr(vcres,"vcMonoLineArrowColor",False,False))
        if(.not.isatt(lbres,"lbLabelBarOn").or.\
          check_attr(lbres,"lbLabelBarOn",True,False))
          lbar_on = True   ; Turn on a labelbar
        end if
      end if
    end if

; Before we create the objects, turn off draw and frame for them.
    vcres           = True
    mpres           = True
    vcres@gsnDraw   = False
    vcres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    set_attr(vcres,"vcRefAnnoString2On","False")

    vector_object = gsn_vector(wks,unew,vnew,vcres) ; Create vectors.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
    overlay(map_object,vector_object)               ; Overlay vectors on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues map_object
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.1*font_height)
    setvalues map_object
      "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Set font heights only if they haven't been set explicitly by user.

    if(.not.isatt(vcres,"vcRefAnnoFontHeightF"))
      setvalues vector_object
        "vcRefAnnoFontHeightF"  : 0.6 * font_height
      end setvalues
    end if

; Create a labelbar.

    if(lbar_on)
      add_labelbar(wks,vector_object,2,0.6 * font_height,"other",lbres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data   = vector_object@data
    map_object@vector = vector_object
    return(map_object)
end


;***********************************************************************;
; Function : gsn_csm_streamline_map_ce                                  ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot over a map plot to  ;
; the workstation "wks" (the variable returned from a previous call to  ;
; "gsn_open_wks").  "u,v" is the 2-dimensional data to be streamlined,  ;
; and "resources" is an optional list of resources. The Id of the map   ;
; plot is returned.                                                     ;
;                                                                       ;
; This function behaves differently from gsn_streamline in that it will ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. The lat/lon grid is labeled with tickmarks.                     ;
;    2. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top left, center, and right of the plot   ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;    3. If data has an attribute called "long_name," and "gsnLeftString";
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the left string title.                              ;
;    4. If data has an attribute called "units," and "gsnRightString"   ;
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the right string title.                             ;
;***********************************************************************;

function gsn_csm_streamline_map_ce(wks:graphic,u[*][*]:numeric,\
                                   v[*][*]:numeric,resources:logical)
local i, stream_object, map_object, stres, calldraw, callframe, \
min_lat, max_lat, unew, vnew, res, res2, map_vpwf, map_vphf, vpwf, vphf
begin

; Initialize.
    infolabel_zone = 2        ; Zone for info label (may change later)
    mpres          = True

    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for coordinate variables. These values will determine where to 
; overlay streamline on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
      set_attr(res2,"vfYCStartV",-90.)
      set_attr(res2,"vfYCEndV",   90.)
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

;
; Check for other special resources.
;
    if(isatt(res2,"gsnMajorLonSpacing"))
      mpres@gsnMajorLonSpacing = res2@gsnMajorLonSpacing
      delete(res2@gsnMajorLonSpacing)
    end if
    if(isatt(res2,"gsnMajorLatSpacing"))
      mpres@gsnMajorLatSpacing = res2@gsnMajorLatSpacing
      delete(res2@gsnMajorLatSpacing)
    end if

    if(isatt(res2,"gsnMinorLonSpacing"))
      mpres@gsnMinorLonSpacing = res2@gsnMinorLonSpacing
      delete(res2@gsnMinorLonSpacing)
    end if
    if(isatt(res2,"gsnMinorLatSpacing"))
      mpres@gsnMinorLatSpacing = res2@gsnMinorLatSpacing
      delete(res2@gsnMinorLatSpacing)
    end if
;
; Tickmarks.
;
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2) then
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",False)
    else
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","tx","am"/))

; Before we create the objects, turn off draw and frame for them.
    stres = True
    mpres = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    stream_object = gsn_streamline(wks,unew,vnew,stres) ; Create streamlines

    map_object = gsn_csm_map(wks,mpres)   ; Create map.
    overlay(map_object,stream_object)     ; Overlay streamlines on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues map_object@tickmarks
      "tmXBLabelFontHeightF" : xbfontf
      "tmXBMajorLengthF"     : xlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

    getvalues map_object
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.3*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data       = stream_object@data
    map_object@streamline = stream_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_streamline_map_polar                               ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot over a polar        ;
; stereographic map projection to the workstation "wks" (the variable   ;
; returned from a previous call to "gsn_open_wks").  "u,v" is the       ;
; 2-dimensional data to be streamlined, and "resources" is an optional  ;
; list of resources. The Id of the map plot is returned.                ;
;                                                                       ;
; This function behaves differently from gsn_streamline in that it will ;
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

function gsn_csm_streamline_map_polar(wks:graphic,u[*][*]:numeric, \
                                      v[*][*]:numeric,resources:logical)
local i, stream_object, map_object, res, res2, calldraw, stres, \
map_vpwf, map_vphf, vpwf, vphf, callframe, unew, vnew
begin

; Initialize.
    mpres          = True
    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check for draw and frame.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

;
; Check for type of polar plot desired.
;
    mpres@gsnPolar = get_polar_type(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
;
; Tickmarks.
;
    mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)

; Create data object and use coordinate variables if they exist.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","tx","am"/))

; Before we create the objects, turn off draw and frame for them.
    stres = True
    mpres = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    stream_object = gsn_streamline(wks,unew,vnew,stres) ; Create streamlines
    map_object    = gsn_csm_map(wks,mpres)          ; Create map.
    overlay(map_object,stream_object)               ; Overlay strmlns on map.
;
; Retrieve the view port location of the map plot so we know where
; to put titles.
;
    getvalues map_object
      "vpWidthF"  : vpwf
      "vpHeightF" : vphf
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     2.*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data       = stream_object@data
    map_object@streamline = stream_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_streamline_map_other                               ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot over a map plot to  ;
; the workstation "wks" (the variable returned from a previous call to  ;
; "gsn_open_wks").  "u"/"v" is the 2-dimensional data to be             ;
; streamlined, and "resources" is an optional list of resources. The    ;
; id of the map plot is returned.                                       ;
;                                                                       ;
; This function behaves differently from gsn_streamline in that it will ;
; create a special kind of plot if certain attributes are set.          ;
;                                                                       ;
;    1. The lat/lon grid is labeled with tickmarks.                     ;
;    2. If any of the special GSUN resources "gsnLeftString,"           ;
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;
;       are used to title the top left, center, and right of the plot   ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;***********************************************************************;

function gsn_csm_streamline_map_other(wks:graphic,u[*][*]:numeric,\
                                      v[*][*]:numeric,resources:logical)
local i, stream_object, labelbar_object, xy_object, map_object, \
calldraw, callframe, lbar_on, min_lat, max_lat, datanew, \
res, res2, mpres, stres, map_vpwf, map_vphf, vphf 
begin
; Initialize.
    mpres          = True

    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for coordinate variables. These values will determine where to 
; overlay streamlines on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
;
; By default, mpOutlineOn is False, unless mpFillOn is set to False, then
; it is set back to True.
;
   if(check_attr(res2,"mpFillOn",False,False))
      set_attr(res2,"mpOutlineOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am"/))

; Before we create the objects, turn off draw and frame for them.
    stres           = True
    mpres           = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    stream_object = gsn_streamline(wks,unew,vnew,stres) ; Create streamlines.
    map_object = gsn_csm_map(wks,mpres)     ; Create map.
    overlay(map_object,stream_object)       ; Overlay streamlines on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues map_object
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.1*font_height)
    setvalues map_object
      "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data       = stream_object@data
    map_object@streamline = stream_object
    return(map_object)
end


;***********************************************************************;
; Function : gsn_csm_streamline_contour_map_ce                          ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_streamline_map_ce except it       ;
; overlays contours.                                                    ;
;***********************************************************************;

function gsn_csm_streamline_contour_map_ce(wks:graphic,u[*][*]:numeric,\
                   v[*][*]:numeric,data[*][*]:numeric,resources:logical)
local i, stream_object, map_object, res, res2, \
calldraw, callframe, min_lat, max_lat, unew, vnew, datanew, \
map_vpwf, map_vphf, vpwf, vphf, lbar_on
begin

; Initialize.
    infolabel_on   = False
    lbar_on        = True
    mpres          = True
    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
      datanew = gsn_add_cyclic_point(data)
    else
      unew    = u
      vnew    = v
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vector on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
      set_attr(res2,"vfYCStartV",-90.)
      set_attr(res2,"vfYCEndV",   90.)
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

    lbres = get_res_eq(res2,(/"lb","pm"/))

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

;
; Check for other special resources.
;
    if(isatt(res2,"gsnMajorLonSpacing"))
      mpres@gsnMajorLonSpacing = res2@gsnMajorLonSpacing
      delete(res2@gsnMajorLonSpacing)
    end if
    if(isatt(res2,"gsnMajorLatSpacing"))
      mpres@gsnMajorLatSpacing = res2@gsnMajorLatSpacing
      delete(res2@gsnMajorLatSpacing)
    end if

    if(isatt(res2,"gsnMinorLonSpacing"))
      mpres@gsnMinorLonSpacing = res2@gsnMinorLonSpacing
      delete(res2@gsnMinorLonSpacing)
    end if
    if(isatt(res2,"gsnMinorLatSpacing"))
      mpres@gsnMinorLatSpacing = res2@gsnMinorLatSpacing
      delete(res2@gsnMinorLatSpacing)
    end if

    lbar_on = False
; Turn on labelbar if cnFillOn is True and it wasn't explicitly 
; turned off.
    if(check_attr(res2,"cnFillOn",True,False))
      set_attr(res2,"cnFillDrawOrder","Predraw")
      set_attr(res2,"cnInfoLabelOn",False)
      if(.not.isatt(lbres,"lbLabelBarOn").or.\
         check_attr(lbres,"lbLabelBarOn",True,False))
        set_attr(res2,"cnLineLabelsOn",False)
        lbar_on = True   ; Turn on a labelbar
      end if
    end if

    check_for_y_lat_coord(datanew,res2,"contour")
    check_for_lon_coord(datanew,res2,"contour")

    if(.not.(isatt(res2,"sfXArray")))
      set_attr(res2,"sfXCStartV",-180.)
      set_attr(res2,"sfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"sfYArray")))
      set_attr(res2,"sfYCStartV",-90.)
      set_attr(res2,"sfYCEndV",   90.)
    end if

    if(check_attr(res2,"lbLabelBarOn",False,False))
      lbar_on = False
    end if

; Turn off info label if labelbar is on, unless otherwise specified by user.
    if(lbar_on)
      set_attr(res2,"cnInfoLabelOn",False)
    else
      set_attr(res2,"cnInfoLabelOn",True)
    end if
;
; Tickmarks.
;
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2) then
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",False)
    else
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))

; Before we create the objects, turn off draw and frame for them.
    stres          = True
    mpres          = True
    stres@gsnDraw  = False
    stres@gsnFrame = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    if(lbar_on.and.check_attr(lbres,"lbOrientation","vertical",True))
      set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
      set_attr(mpres,"vpHeightF",0.75)   ; on the side.
      set_attr(mpres,     "vpXF",0.08)
      set_attr(mpres,     "vpYF",0.90)
    end if

    set_attr(stres,"cnInfoLabelZone",2)
    set_attr(stres,"cnInfoLabelOrthogonalPosF", 0.13)

    stream_object = gsn_streamline_contour(wks,unew,vnew,datanew,stres)

    map_object = gsn_csm_map(wks,mpres)       ; Create map.
    overlay(map_object,stream_object)         ; Overlay streamlines on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues map_object@tickmarks
      "tmXBLabelFontHeightF" : xbfontf
    end getvalues

    getvalues map_object
      "vpWidthF"           : vpwf
      "vpHeightF"          : vphf
      "tiXAxisFontHeightF" : xfontf
      "tiYAxisFontHeightF" : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.3*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Make sure line labels and info labels are same size.
    if(.not.isatt(stres,"cnLineLabelFontHeightF"))
      setvalues stream_object@contour
        "cnInfoLabelFontHeightF" : xbfontf
     end setvalues
    end if
    if(.not.isatt(stres,"cnLineLabelFontHeightF"))
      setvalues stream_object@contour
        "cnLineLabelFontHeightF"    : xbfontf
      end setvalues
    end if

; Create a labelbar.

    if(lbar_on)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if

      add_labelbar(wks,stream_object@contour,lbar_zone,xbfontf, \
                   "vector",lbres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata      = stream_object@vfdata
    map_object@sfdata      = stream_object@sfdata
    map_object@streamline  = stream_object
    map_object@contour     = stream_object@contour
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_streamline_contour_map_polar                       ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_streamline_map_polar except it    ;
; overlays a contour plot.                                              ;
;***********************************************************************;

function gsn_csm_streamline_contour_map_polar(wks:graphic,u[*][*]:numeric, \
                                         v[*][*]:numeric,data[*][*]:numeric,
                                         resources:logical)
local i, stream_object, map_object, res, res2, calldraw, vcres, \
lbres, vpwf, vphf, callframe, lbar_on, lbar_zone, unew, vnew
begin
;
; The default is to draw streamlines colored by a scalar field, so this
; means a labelbar should be drawn, and no contour information is 
; needed.
;
; Initialize.
    res2  = get_resources(resources)
    mpres = True

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
      datanew = gsn_add_cyclic_point(data)
    else
      unew    = u
      vnew    = v
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay streamlines on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check for draw and frame.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

;
; Check for type of polar plot.
;
    mpres@gsnPolar = get_polar_type(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
; 
; Determine which annotations (labelbar, info label) should be turned on. 
; By default, labelbar is on, and contour info label is off.
;
    if(check_attr(res2,"cnFillOn",True,False))
      set_attr(res2,"cnFillDrawOrder","Predraw")
      set_attr(res2,"cnLineLabelsOn",False)
      set_attr(res2,"cnLinesOn",False)
      set_attr(res2,"cnInfoLabelOn",False)
    else    
      set_attr(res2,"cnInfoLabelOn",True)
      set_attr(res2,"lbLabelBarOn",False)
    end if
;
; Check for coordinate variables for scalar field.
;
    check_for_y_lat_coord(datanew,res2,"contour")
    check_for_lon_coord(datanew,res2,"contour")

    lbar_on      = get_res_value_keep(res2,"lbLabelBarOn",True)
    lbar_orient  = lower_case(get_res_value_keep(res2,"lbOrientation", \
                              "horizontal"))
    infolabel_on = get_res_value_keep(res2,"cnInfoLabelOn",False)
;
; Now that we know what's being drawn for annotations (labelbar, 
; info label), we can determine where to put all this
; stuff.
;
    if(infolabel_on)
      set_attr(res2,"cnInfoLabelZone",2)
      set_attr(res2,"cnInfoLabelOrthogonalPosF", 0.1)
      set_attr(res2,"cnInfoLabelParallelPosF", 1.0)
      set_attr(res2,"cnInfoLabelJust","TopRight")
    end if

    if(lbar_on)
      if(lbar_orient.eq."vertical")
        set_attr(res2,"vpXF",0.15)
        set_attr(res2,"pmLabelBarOrthogonalPosF",0.1)
      else
        set_attr(res2,"vpYF",0.82)
      end if
    end if
;
; Tickmarks.
;
    mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm"/))

; Before we create the objects, turn off draw and frame for them.
    stres = True
    mpres = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    stream_object = gsn_streamline_contour(wks,unew,vnew,datanew,stres)
    map_object    = gsn_csm_map(wks,mpres)      ; Create map.
    overlay(map_object,stream_object)           ; Overlay streamlines on map.

; Retrieve plot height so we can compute font height.

    getvalues map_object
      "vpHeightF"          : vphf
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.3*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Create a labelbar.

    if(lbar_on)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
;
; Get labelbar resources, if any.
;
      lbres = get_res_eq(res2,(/"lb","pm"/))

      add_labelbar(wks,stream_object@contour,lbar_zone,font_height, \
                   "vector",lbres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata  = stream_object@vfdata
    map_object@sfdata  = stream_object@sfdata
    map_object@streamline  = stream_object
    map_object@contour = stream_object@contour
    return(map_object)
end


;***********************************************************************;
; Function : gsn_csm_streamline_contour_map_other                       ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_streamline_map_other except       ;
; overlays a contour plot.                                              ;
;***********************************************************************;

function gsn_csm_streamline_contour_map_other(wks:graphic,u[*][*]:numeric,\
                                        v[*][*]:numeric,data[*][*]:numeric, \
                                        resources:logical)
local i, stream_object, labelbar_object, map_object, \
calldraw, callframe, lbar_on, min_lat, max_lat, datanew, \
res, res2, lbres, mpres, vcres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf 
begin
; Initialize.
    infolabel_on   = False
    lbar_on        = False    ; Default is no labelbar.
    mpres          = True

    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
      datanew = gsn_add_cyclic_point(data)
    else
      unew = u
      vnew = v
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay streamlines on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

; 
; Determine which annotations (labelbar, info label) should be turned on.
; By default, labelbar is on, and contour info label is off.
;
    if(check_attr(res2,"cnFillOn",True,False))
      set_attr(res2,"cnFillDrawOrder","Predraw")
      set_attr(res2,"cnLineLabelsOn",False)
      set_attr(res2,"cnLinesOn",False)
      set_attr(res2,"cnInfoLabelOn",False)
    else    
      set_attr(res2,"cnInfoLabelOn",True)
      set_attr(res2,"lbLabelBarOn",False)
    end if
;
; Check for coordinate variables for scalar field.
;
    check_for_y_lat_coord(datanew,res2,"contour")
    check_for_lon_coord(datanew,res2,"contour")

    lbar_on      = get_res_value_keep(res2,"lbLabelBarOn",True)
    lbar_orient  = lower_case(get_res_value_keep(res2,"lbOrientation", \
                              "horizontal"))
    infolabel_on = get_res_value_keep(res2,"cnInfoLabelOn",False)

;
; Now that we know what's being drawn for annotations (labelbar, 
; info label), we can determine where to put all this
; stuff.
;
    if(infolabel_on)
      set_attr(res2,"cnInfoLabelZone",2)
      set_attr(res2,"cnInfoLabelOrthogonalPosF", 0.1)
      set_attr(res2,"cnInfoLabelParallelPosF", 1.0)
      set_attr(res2,"cnInfoLabelJust","TopRight")
    end if
;
; By default, mpOutlineOn is False, unless mpFillOn is set to False,
; then it is set back to True.
;
   if(check_attr(res2,"mpFillOn",False,False))
      set_attr(res2,"mpOutlineOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))

; Before we create the objects, turn off draw and frame for them.
    stres           = True
    mpres           = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    stream_object = gsn_streamline_contour(wks,unew,vnew,datanew,stres)

    map_object = gsn_csm_map(wks,mpres)      ; Create map.
    overlay(map_object,stream_object)        ; Overlay streamlines on map.

; Retrieve plot height so we can compute font height.

    getvalues map_object
      "vpHeightF"          : vphf
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     2.*font_height)
    setvalues map_object
      "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Create a labelbar.

    if(lbar_on)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
;
; Get labelbar resources, if any.
;
      lbres = get_res_eq(res2,(/"lb","pm"/))

      add_labelbar(wks,stream_object@contour,lbar_zone,font_height, \
                   "vector",lbres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata  = stream_object@vfdata
    map_object@sfdata  = stream_object@sfdata
    map_object@vector  = stream_object
    map_object@contour = stream_object@contour

    return(map_object)
end


;***********************************************************************;
; Function : gsn_csm_vector_scalar                                      ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function colors vectors according to a scalar field.             ;
;                                                                       ;
; There's a special resource associated with this function called       ;
; "gsnScalarContour".  If it's set to True, then the scalar field will  ;
; be drawn as a contour plot.  Otherwise, the vectors will be colored   ;
; according to the scalar field. This resource defaults to False.       ;
;***********************************************************************;

function gsn_csm_vector_scalar(wks:graphic,u[*][*]:numeric,\
                   v[*][*]:numeric,data[*][*]:numeric,resources:logical)
local i, vector_object, res, res2, \
calldraw, callframe, unew, vnew, datanew, vpwf, vphf, lbar_on
begin

; Initialize.
    main_zone      = 2         ; Zone for main title (may change later)
    infolabel_on   = False
    refanno_on     = True
    lbar_on        = True
    res2 = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
      datanew = gsn_add_cyclic_point(data)
    else
      unew    = u
      vnew    = v
      datanew = data
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,res2)
    set_left_subtitle(unew,res2,res2)

; Check for existence of the left, center, and right subtitles.

    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)

    if(left_string.or.center_string.or.right_string)
      main_zone   = main_zone+1
    end if

; Check if gsnShape set.

    if(check_attr(res2,"gsnShape",True,False))
      main_zone     = main_zone+1 ; Zone for main title
    end if

; Use coordinate variables for X and/or Y if they exist.

    check_for_coord_arrays(datanew,res2,"contour")
    check_for_coord_arrays(unew,res2,"vector")

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for type of scalar field representation.
    scalar_contour = get_res_value(res2,"gsnScalarContour",False)

; Get labelbar resources.
    lbres = get_res_eq(res2,(/"lb","pm"/))

    if(scalar_contour)
      lbar_on = False
; Turn on labelbar if cnFillOn is True and it wasn't explicitly 
; turned off.
      if(check_attr(res2,"cnFillOn",True,False))
        set_attr(res2,"cnFillDrawOrder","Predraw")
        set_attr(res2,"cnInfoLabelOn",False)
        if(.not.isatt(lbres,"lbLabelBarOn").or.\
           check_attr(lbres,"lbLabelBarOn",True,False))
          set_attr(res2,"cnLineLabelsOn",False)
          lbar_on = True   ; Turn on a labelbar
        end if
      end if
    end if

    if(check_attr(res2,"lbLabelBarOn",False,False))
      lbar_on = False
    end if

; Turn off anno label if specified by user.
    if(check_attr(res2,"vcRefAnnoOn",False,False))
      refanno_on = False
    end if

; Turn off info label if labelbar is on, unless otherwise specified by user.
    if(scalar_contour)
      if(lbar_on)
        set_attr(res2,"cnInfoLabelOn",False)
      else
        set_attr(res2,"cnInfoLabelOn",True)
      end if
      infolabel_on = res2@cnInfoLabelOn
    end if

    if(lbar_on.and.check_attr(lbres,"lbOrientation","vertical",True))
      set_attr(res2, "vpWidthF",0.75)   ; Make room for labelbar
      set_attr(res2,"vpHeightF",0.75)   ; on the side.
      set_attr(res2,     "vpXF",0.08)
      set_attr(res2,     "vpYF",0.90)
    end if

    if(scalar_contour.and.infolabel_on)
      set_attr(res2,"cnInfoLabelZone",2)
      set_attr(res2,"cnInfoLabelOrthogonalPosF", 0.13)
    end if

    if(refanno_on)
      if(infolabel_on)
        set_attr(res2,"vcRefAnnoParallelPosF",0.)
        set_attr(res2,"vcRefAnnoJust","TopLeft")
      end if
      set_attr(res2,"vcRefAnnoString2On","False")
      set_attr(res2,"vcRefAnnoZone",2)
      set_attr(res2,"vcRefAnnoOrthogonalPosF", 0.13)
    end if

; Before we create the vector object, turn off draw and frame for it.
    vcres = get_res_ne(res2,(/"mp","lb","tx","am","pm"/))

    vcres          = True
    vcres@gsnDraw  = False
    vcres@gsnFrame = False

    if(scalar_contour)
      vector_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
    else
      vector_object = gsn_vector_scalar(wks,unew,vnew,datanew,vcres)
    end if

; Add lat/lon labels to X/Y axes if appropriate coordinate arrays exist.

    add_latlon_labels(vector_object,unew,res2)

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues vector_object
      "vpHeightF"            : height
      "vpWidthF"             : width

      "tmXBLabelFontHeightF" : xbfontf
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
      "tmYLMajorLengthF"     : ylength
      "tmXBMajorLengthF"     : xlength
      "tmYLMinorLengthF"     : ymlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make subtitle label sizes a
                                          ; function of the size of the
                                          ; X/Y axis labels.

    major_length = min((/ylength,xlength/))    ; New length for major ticks.
    minor_length = min((/ymlength,xmlength/))  ; New length for minor ticks.

; If the plot is close to square in size, then make the 
; three top titles and the tick marks smaller.

    font_scale = (/1.0,0.8,0.8/)
    ratios     = (/0.5,0.75,1.0/)

    ratio = height/width
    if(ratio.gt.1) 
      ratio = 1./ratio
    end if
    index = ind(ratio.le.ratios)

    scale = font_scale(index(0))
    font_height  = scale * font_height
;
; Make tick marks same length and point outward.
;
    major_length = scale * major_length
    minor_length = scale * minor_length

    tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
    gsnp_point_tickmarks_outward(vector_object,tmres,xlength,ylength,\
                                               xmlength,ymlength,\
                                               major_length,minor_length)

; Make sure info labels, line labels, and vector labels are same size.
    if(scalar_contour)
      if(.not.isatt(res2,"cnLineLabelFontHeightF"))
        setvalues vector_object@contour
          "cnInfoLabelFontHeightF" : xbfontf
       end setvalues
      end if
      if(.not.isatt(res2,"cnLineLabelFontHeightF"))
        setvalues vector_object@contour
          "cnLineLabelFontHeightF"    : xbfontf
        end setvalues
      end if
    end if

    if(.not.isatt(res2,"vcRefAnnoFontHeightF"))
      setvalues vector_object
        "vcRefAnnoFontHeightF" : xbfontf
     end setvalues
    end if

; Create a labelbar.

    if(lbar_on)
      if(.not.refanno_on.and..not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if

      if(scalar_contour)
        add_labelbar(wks,vector_object@contour,lbar_zone,xbfontf, \
                     "vector",lbres)
      else
        add_labelbar(wks,vector_object,lbar_zone,xbfontf, \
                     "vector",lbres)
      end if
    end if
  
; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,vector_object,left_string,center_string,\
                  right_string,subres)

; Draw all this stuff: vector/contour plot, subtitles, and tick marks.
    draw_and_frame(wks,vector_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    vector_object@vfdata  = vector_object@vfdata
    vector_object@sfdata  = vector_object@sfdata
    if(scalar_contour)
      vector_object@contour = vector_object@contour
    end if
    return(vector_object)
end

;***********************************************************************;
; Function : gsn_csm_vector_scalar_map_ce                               ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_vector_map_ce except it colors the;
; vectors according to a scalar field.                                  ;
;                                                                       ;
; There's a special resource associated with this function called       ;
; "gsnScalarContour".  If it's set to True, then the scalar field will  ;
; be drawn as a contour plot.  Otherwise, the vectors will be colored   ;
; according to the scalar field. This resource defaults to False.       ;
;***********************************************************************;

function gsn_csm_vector_scalar_map_ce(wks:graphic,u[*][*]:numeric,\
                   v[*][*]:numeric,data[*][*]:numeric,resources:logical)
local i, vector_object, map_object, res, res2, \
calldraw, callframe, min_lat, max_lat, unew, vnew, datanew, \
map_vpwf, map_vphf, vpwf, vphf, lbar_on
begin

; Initialize.
    infolabel_on   = False
    refanno_on   = True
    lbar_on        = True
    mpres          = True
    res2           = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
      datanew = gsn_add_cyclic_point(data)
    else
      unew    = u
      vnew    = v
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vector on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
      set_attr(res2,"vfYCStartV",-90.)
      set_attr(res2,"vfYCEndV",   90.)
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

    lbres = get_res_eq(res2,(/"lb","pm"/))

; Check for type of scalar field representation.
    scalar_contour = get_res_value(res2,"gsnScalarContour",False)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

;
; Check for other special resources.
;
    if(isatt(res2,"gsnMajorLonSpacing"))
      mpres@gsnMajorLonSpacing = res2@gsnMajorLonSpacing
      delete(res2@gsnMajorLonSpacing)
    end if
    if(isatt(res2,"gsnMajorLatSpacing"))
      mpres@gsnMajorLatSpacing = res2@gsnMajorLatSpacing
      delete(res2@gsnMajorLatSpacing)
    end if

    if(isatt(res2,"gsnMinorLonSpacing"))
      mpres@gsnMinorLonSpacing = res2@gsnMinorLonSpacing
      delete(res2@gsnMinorLonSpacing)
    end if
    if(isatt(res2,"gsnMinorLatSpacing"))
      mpres@gsnMinorLatSpacing = res2@gsnMinorLatSpacing
      delete(res2@gsnMinorLatSpacing)
    end if

    if(scalar_contour)
      lbar_on = False
; Turn on labelbar if cnFillOn is True and it wasn't explicitly 
; turned off.
      if(check_attr(res2,"cnFillOn",True,False))
        set_attr(res2,"cnFillDrawOrder","Predraw")
        set_attr(res2,"cnInfoLabelOn",False)
        if(.not.isatt(lbres,"lbLabelBarOn").or.\
           check_attr(lbres,"lbLabelBarOn",True,False))
          set_attr(res2,"cnLineLabelsOn",False)
          lbar_on = True   ; Turn on a labelbar
        end if
      end if

      check_for_y_lat_coord(datanew,res2,"contour")
      check_for_lon_coord(datanew,res2,"contour")

      if(.not.(isatt(res2,"sfXArray")))
        set_attr(res2,"sfXCStartV",-180.)
        set_attr(res2,"sfXCEndV",   180.)
      end if

      if(.not.(isatt(res2,"sfYArray")))
        set_attr(res2,"sfYCStartV",-90.)
        set_attr(res2,"sfYCEndV",   90.)
      end if
    end if

    if(check_attr(res2,"lbLabelBarOn",False,False))
      lbar_on = False
    end if

; Turn off anno label if specified by user.
    if(check_attr(res2,"vcRefAnnoOn",False,False))
      refanno_on = False
    end if

; Turn off info label if labelbar is on, unless otherwise specified by user.
    if(scalar_contour)
      if(lbar_on)
        set_attr(res2,"cnInfoLabelOn",False)
      else
        set_attr(res2,"cnInfoLabelOn",True)
      end if
      infolabel_on = res2@cnInfoLabelOn
    end if
;
; Tickmarks.
;
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2) then
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",False)
    else
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))

; Before we create the objects, turn off draw and frame for them.
    vcres          = True
    mpres          = True
    vcres@gsnDraw  = False
    vcres@gsnFrame = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    if(lbar_on.and.check_attr(lbres,"lbOrientation","vertical",True))
      set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
      set_attr(mpres,"vpHeightF",0.75)   ; on the side.
      set_attr(mpres,     "vpXF",0.08)
      set_attr(mpres,     "vpYF",0.90)
    end if

    if(scalar_contour.and.infolabel_on)
      set_attr(vcres,"cnInfoLabelZone",2)
      set_attr(vcres,"cnInfoLabelOrthogonalPosF", 0.13)
    end if

    if(refanno_on)
      if(infolabel_on)
        set_attr(vcres,"vcRefAnnoParallelPosF",0.)
        set_attr(vcres,"vcRefAnnoJust","TopLeft")
      end if
      set_attr(vcres,"vcRefAnnoString2On","False")
      set_attr(vcres,"vcRefAnnoZone",2)
      set_attr(vcres,"vcRefAnnoOrthogonalPosF", 0.13)
    end if

    if(scalar_contour)
      vector_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
    else
      vector_object = gsn_vector_scalar(wks,unew,vnew,datanew,vcres)
    end if

    map_object = gsn_csm_map(wks,mpres)            ; Create map.
    overlay(map_object,vector_object)               ; Overlay vectors on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues map_object@tickmarks
      "tmXBLabelFontHeightF" : xbfontf
    end getvalues

    getvalues map_object
      "vpWidthF"           : vpwf
      "vpHeightF"          : vphf
      "tiXAxisFontHeightF" : xfontf
      "tiYAxisFontHeightF" : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.3*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Make sure info labels, line labels, and vector labels are same size.
    if(scalar_contour)
      if(.not.isatt(vcres,"cnLineLabelFontHeightF"))
        setvalues vector_object@contour
          "cnInfoLabelFontHeightF" : xbfontf
       end setvalues
      end if
      if(.not.isatt(vcres,"cnLineLabelFontHeightF"))
        setvalues vector_object@contour
          "cnLineLabelFontHeightF"    : xbfontf
        end setvalues
      end if
    end if

    if(.not.isatt(vcres,"vcRefAnnoFontHeightF"))
      setvalues vector_object
        "vcRefAnnoFontHeightF" : xbfontf
     end setvalues
    end if

; Create a labelbar.

    if(lbar_on)
      if(.not.refanno_on.and..not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if

      if(scalar_contour)
        add_labelbar(wks,vector_object@contour,lbar_zone,xbfontf, \
                     "vector",lbres)
      else
        add_labelbar(wks,vector_object,lbar_zone,xbfontf, \
                     "vector",lbres)
      end if
    end if
  

; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata  = vector_object@vfdata
    map_object@sfdata  = vector_object@sfdata
    if(scalar_contour)
      map_object@vector  = vector_object
      map_object@contour = vector_object@contour
    else
      map_object@vector  = vector_object
    end if
    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_vector_scalar_map_polar                            ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_vector_map_polar except it either ;
; colors the vectors according to a scalar field, or it overlays a      ;
; contour plot.                                                         ;
;                                                                       ;
; There's a special resource associated with this function called       ;
; "gsnScalarContour".  If it's set to True, then the scalar field will  ;
; be drawn as a contour plot.  Otherwise, the vectors will be colored   ;
; according to the scalar field. This resource defaults to False.       ;
;***********************************************************************;

function gsn_csm_vector_scalar_map_polar(wks:graphic,u[*][*]:numeric, \
                                         v[*][*]:numeric,data[*][*]:numeric,
                                         resources:logical)
local i, vector_object, map_object, res, res2, calldraw, vcres, \
lbres, vpwf, vphf, callframe, lbar_on, lbar_zone, unew, vnew
begin
;
; The default is to draw vectors colored by a scalar field, so this
; means a labelbar should be drawn, and no contour information is 
; needed.
;
; Initialize.
    res2  = get_resources(resources)
    mpres = True

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
      datanew = gsn_add_cyclic_point(data)
    else
      unew    = u
      vnew    = v
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vectors on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check for draw and frame.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

;
; Check for type of polar plot and scalar field representation.
;
    mpres@gsnPolar = get_polar_type(res2)
    scalar_contour = get_res_value(res2,"gsnScalarContour",False)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
; 
; Determine which annotations (labelbar, vector reference anno, info label)
; should be turned on.  By default, labelbar and vector anno are on, and
; contour info label is off.
;
    if(scalar_contour)
      if(check_attr(res2,"cnFillOn",True,False))
        set_attr(res2,"cnFillDrawOrder","Predraw")
        set_attr(res2,"cnLineLabelsOn",False)
        set_attr(res2,"cnLinesOn",False)
        set_attr(res2,"cnInfoLabelOn",False)
      else    
        set_attr(res2,"cnInfoLabelOn",True)
        set_attr(res2,"lbLabelBarOn",False)
      end if
;
; Check for coordinate variables for scalar field.
;
      check_for_y_lat_coord(datanew,res2,"contour")
      check_for_lon_coord(datanew,res2,"contour")
    end if

    lbar_on      = get_res_value_keep(res2,"lbLabelBarOn",True)
    lbar_orient  = lower_case(get_res_value_keep(res2,"lbOrientation", \
                              "horizontal"))
    infolabel_on = get_res_value_keep(res2,"cnInfoLabelOn",False)
    refanno_on   = get_res_value_keep(res2,"vcRefAnnoOn",True)
;
; Now that we know what's being drawn for annotations (labelbar, 
; info label, ref anno), we can determine where to put all this
; stuff. If both info label and ref anno are on, put the info label
; on the right, and the ref anno on the left.  If only one of them is
; on, put it on the right.
;
    if(refanno_on)
      set_attr(res2,"vcRefAnnoZone",2)
      set_attr(res2,"vcRefAnnoString2On","False")
      if(infolabel_on)
        set_attr(res2,"vcRefAnnoOrthogonalPosF", 0.1)
        set_attr(res2,"vcRefAnnoJust","TopLeft")
        set_attr(res2,"vcRefAnnoParallelPosF", 0.0)
      else
        set_attr(res2,"vcRefAnnoOrthogonalPosF",0.02)
        set_attr(res2,"vcRefAnnoParallelPosF",0.8)
      end if
    end if

    if(infolabel_on)
      set_attr(res2,"cnInfoLabelZone",2)
      set_attr(res2,"cnInfoLabelOrthogonalPosF", 0.1)
      set_attr(res2,"cnInfoLabelParallelPosF", 1.0)
      set_attr(res2,"cnInfoLabelJust","TopRight")
    end if

    if(lbar_on)
      if(lbar_orient.eq."vertical")
        set_attr(res2,"vpXF",0.15)
        set_attr(res2,"pmLabelBarOrthogonalPosF",0.1)
      else
        set_attr(res2,"vpYF",0.82)
      end if
    end if
;
; Tickmarks.
;
    mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm"/))

; Before we create the objects, turn off draw and frame for them.
    vcres = True
    mpres = True
    vcres@gsnDraw   = False
    vcres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    if(scalar_contour)
      vector_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
    else
      vector_object = gsn_vector_scalar(wks,unew,vnew,datanew,vcres)
    end if
    map_object    = gsn_csm_map(wks,mpres)          ; Create map.
    overlay(map_object,vector_object)               ; Overlay vectors on map.

; Retrieve plot height so we can compute font height.

    getvalues map_object
      "vpHeightF"          : vphf
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     1.3*font_height)
    setvalues map_object
        "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Create a labelbar.

    if(lbar_on)
      if(.not.refanno_on.and..not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
;
; Get labelbar resources, if any.
;
      lbres = get_res_eq(res2,(/"lb","pm"/))

      if(scalar_contour)
        add_labelbar(wks,vector_object@contour,lbar_zone,font_height, \
                     "vector",lbres)
      else
        add_labelbar(wks,vector_object,lbar_zone,font_height, \
                     "vector",lbres)
      end if
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata  = vector_object@vfdata
    map_object@sfdata  = vector_object@sfdata
    if(scalar_contour)
      map_object@vector  = vector_object
      map_object@contour = vector_object@contour
    else
      map_object@vector  = vector_object
    end if
    return(map_object)
end


;***********************************************************************;
; Function : gsn_csm_vector_scalar_map_other                            ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_vector_map_other except it either ;
; colors the vectors according to a scalar field, or it overlays a      ;
; contour plot.                                                         ;
;                                                                       ;
; There's a special resource associated with this function called       ;
; "gsnScalarContour".  If it's set to True, then the scalar field will  ;
; be drawn as a contour plot.  Otherwise, the vectors will be colored   ;
; according to the scalar field. This resource defaults to False.       ;
;***********************************************************************;

function gsn_csm_vector_scalar_map_other(wks:graphic,u[*][*]:numeric,\
                                        v[*][*]:numeric,data[*][*]:numeric, \
                                        resources:logical)
local i, vector_object, labelbar_object, map_object, \
calldraw, callframe, lbar_on, min_lat, max_lat, datanew, \
res, res2, lbres, mpres, vcres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf 
begin
; Initialize.
    infolabel_on   = False
    refanno_on     = True
    lbar_on        = False    ; Default is no labelbar.
    mpres          = True

    res2           = get_resources(resources)

; Check for cyclic point (default is True).

    if(get_res_value(res2,"gsnAddCyclic",True))
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
      datanew = gsn_add_cyclic_point(data)
    else
      unew = u
      vnew = v
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vectors on map.

    check_for_y_lat_coord(unew,res2,"vector")
    check_for_lon_coord(unew,res2,"vector")

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

; Check for type of scalar field representation.
    scalar_contour = get_res_value(res2,"gsnScalarContour",False)

; 
; Determine which annotations (labelbar, vector reference anno, info label)
; should be turned on.  By default, labelbar and vector anno are on, and
; contour info label is off.
;
    if(scalar_contour)
      if(check_attr(res2,"cnFillOn",True,False))
        set_attr(res2,"cnFillDrawOrder","Predraw")
        set_attr(res2,"cnLineLabelsOn",False)
        set_attr(res2,"cnLinesOn",False)
        set_attr(res2,"cnInfoLabelOn",False)
      else    
        set_attr(res2,"cnInfoLabelOn",True)
        set_attr(res2,"lbLabelBarOn",False)
      end if
;
; Check for coordinate variables for scalar field.
;
      check_for_y_lat_coord(datanew,res2,"contour")
      check_for_lon_coord(datanew,res2,"contour")
    end if

    lbar_on      = get_res_value_keep(res2,"lbLabelBarOn",True)
    lbar_orient  = lower_case(get_res_value_keep(res2,"lbOrientation", \
                              "horizontal"))
    infolabel_on = get_res_value_keep(res2,"cnInfoLabelOn",False)
    refanno_on   = get_res_value_keep(res2,"vcRefAnnoOn",True)

;
; Now that we know what's being drawn for annotations (labelbar, 
; info label, ref anno), we can determine where to put all this
; stuff. If both info label and ref anno are on, put the info label
; on the right, and the ref anno on the left.  If only one of them is
; on, put it on the right.
;
    if(refanno_on)
      set_attr(res2,"vcRefAnnoZone",2)
      set_attr(res2,"vcRefAnnoString2On","False")
      if(infolabel_on)
        set_attr(res2,"vcRefAnnoOrthogonalPosF", 0.1)
        set_attr(res2,"vcRefAnnoJust","TopLeft")
        set_attr(res2,"vcRefAnnoParallelPosF", 0.0)
      else
        set_attr(res2,"vcRefAnnoOrthogonalPosF",0.02)
        set_attr(res2,"vcRefAnnoParallelPosF",0.8)
      end if
    end if

    if(infolabel_on)
      set_attr(res2,"cnInfoLabelZone",2)
      set_attr(res2,"cnInfoLabelOrthogonalPosF", 0.1)
      set_attr(res2,"cnInfoLabelParallelPosF", 1.0)
      set_attr(res2,"cnInfoLabelJust","TopRight")
    end if
;
; By default, mpOutlineOn is False, unless vcMonoLineArrowColor is set to
; False or mpFillOn is set to False, then it is set back to True.
;
   if(check_attr(res2,"vcMonoLineArrowColor",False,False).or.\
      check_attr(res2,"mpFillOn",False,False))
      set_attr(res2,"mpOutlineOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))

; Before we create the objects, turn off draw and frame for them.
    vcres           = True
    mpres           = True
    vcres@gsnDraw   = False
    vcres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    if(scalar_contour)
      vector_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
    else
      vector_object = gsn_vector_scalar(wks,unew,vnew,datanew,vcres)
    end if

    map_object = gsn_csm_map(wks,mpres)         ; Create map.
    overlay(map_object,vector_object)           ; Overlay vectors on map.

; Retrieve plot height so we can compute font height.

    getvalues map_object
      "vpHeightF"          : vphf
    end getvalues

; Make sure axes labels are the same size.

    font_height = 0.02 * vphf     ; Make various label sizes a function
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
                                     2.*font_height)
    setvalues map_object
      "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Set font heights only if they haven't been set explicitly by user.

    if(.not.isatt(vcres,"vcRefAnnoFontHeightF"))
      setvalues vector_object
        "vcRefAnnoFontHeightF"  : font_height
      end setvalues
    end if

; Create a labelbar.

    if(lbar_on)
      if(.not.refanno_on.and..not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
;
; Get labelbar resources, if any.
;
      lbres = get_res_eq(res2,(/"lb","pm"/))

      if(scalar_contour)
        add_labelbar(wks,vector_object@contour,lbar_zone,font_height, \
                     "vector",lbres)
      else
        add_labelbar(wks,vector_object,lbar_zone,font_height, \
                     "vector",lbres)
      end if
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata  = vector_object@vfdata
    map_object@sfdata  = vector_object@sfdata
    if(scalar_contour)
      map_object@vector  = vector_object
      map_object@contour = vector_object@contour
    else
      map_object@vector  = vector_object
    end if
    return(map_object)
end


;***********************************************************************;
; Function : gsn_csm_contour_map                                        ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_csm_contour_map_ce,                    ;
; gsn_csm_contour_map_polar or gsn_csm_contour_map_polar depending on   ;
; the map projection selected.                                          ;
;***********************************************************************;

function gsn_csm_contour_map(wks:graphic,data[*][*]:numeric,\
                             resources:logical)
local res
begin
  res = get_resources(resources)

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_contour_map_polar(wks,data,res))
  else
    if(check_attr(res,"mpProjection","CylindricalEquidistant",True).or.\
       .not.isatt(res,"mpProjection"))
      return(gsn_csm_contour_map_ce(wks,data,res))
    else
      return(gsn_csm_contour_map_other(wks,data,res))
    end if
  end if
end


;***********************************************************************;
; Function : gsn_csm_streamline_map                                     ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_csm_streamline_map_ce,                 ;
; gsn_csm_streamline_map_polar or gsn_csm_streamline_map_other          ;
; depending on the map projection selected.                             ;
;***********************************************************************;

function gsn_csm_streamline_map(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                                resources:logical)
local res
begin
  res = get_resources(resources)

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_streamline_map_polar(wks,u,v,res))
  else
    if(check_attr(res,"mpProjection","CylindricalEquidistant",True).or.\
       .not.isatt(res,"mpProjection"))
      return(gsn_csm_streamline_map_ce(wks,u,v,res))
    else
      return(gsn_csm_streamline_map_other(wks,u,v,res))
    end if
  end if
end


;***********************************************************************;
; Function : gsn_csm_vector_map                                         ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_csm_vector_map_ce,                     ;
; gsn_csm_vector_map_polar or gsn_csm_vector_map_polar depending on the ;
; map projection selected.                                              ;
;***********************************************************************;

function gsn_csm_vector_map(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                            resources:logical)
local res
begin
  res  = get_resources(resources)

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_vector_map_polar(wks,u,v,res))
  else
    if(check_attr(res,"mpProjection","CylindricalEquidistant",True).or.\
       .not.isatt(res,"mpProjection"))
      return(gsn_csm_vector_map_ce(wks,u,v,res))
    else
      return(gsn_csm_vector_map_other(wks,u,v,res))
    end if
  end if
end


;***********************************************************************;
; Function : gsn_csm_vector_scalar_map                                  ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_csm_vector_scalar_map_ce,              ;
; gsn_csm_vector_scalar_map_polar or gsn_csm_vector_scalar_map_other    ;
; depending on the map projection selected.                             ;
;***********************************************************************;

function gsn_csm_vector_scalar_map(wks:graphic,u[*][*]:numeric, \
                                   v[*][*]:numeric,data[*][*]:numeric, \
                                   resources:logical)
local res
begin
  res  = get_resources(resources)

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_vector_scalar_map_polar(wks,u,v,data,res))
  else
    if(check_attr(res,"mpProjection","CylindricalEquidistant",True).or.\
       .not.isatt(res,"mpProjection"))
      return(gsn_csm_vector_scalar_map_ce(wks,u,v,data,res))
    else
      return(gsn_csm_vector_scalar_map_other(wks,u,v,data,res))
    end if
  end if
end


;***********************************************************************;
; Function : gsn_csm_streamline_contour_map                             ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_csm_streamline_contour_map_ce,         ;
; gsn_csm_streamline_contour_map_polar, or                              ;
; gsn_csm_streamline_contour_map_other depending on the map projection  ;
; selected.                                                             ;
;***********************************************************************;

function gsn_csm_streamline_contour_map(wks:graphic,u[*][*]:numeric, \
                                   v[*][*]:numeric,data[*][*]:numeric, \
                                   resources:logical)
local res
begin
  res  = get_resources(resources)

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_streamline_contour_map_polar(wks,u,v,data,res))
  else
    if(check_attr(res,"mpProjection","CylindricalEquidistant",True).or.\
       .not.isatt(res,"mpProjection"))
      return(gsn_csm_streamline_contour_map_ce(wks,u,v,data,res))
    else
      return(gsn_csm_streamline_contour_map_other(wks,u,v,data,res))
    end if
  end if
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
  res1_tmp  = True 
  res2_tmp  = True 
  
  calldraw  = get_res_value(res1_tmp,"gsnDraw", True)
  callframe = get_res_value(res1_tmp,"gsnFrame",True)
  maxbb     = get_bb_res(res1_tmp)

  res1_tmp@gsnDraw  = False
  res1_tmp@gsnFrame = False

; Default for first plot is to turn fill on and turn off lines and line
; labels.

  if(.not.isatt(res1_tmp,"cnFillOn"))
    res1_tmp@cnFillOn = True
    set_attr(res1_tmp,"cnInfoLabelOn",False)
  end if 
  set_attr(res1_tmp,"cnLinesOn",False)
  set_attr(res1_tmp,"cnLineLabelsOn",False)

  contour1 = gsn_csm_contour_map(wks,data1,res1_tmp)

  res2_tmp@gsnDraw  = False
  res2_tmp@gsnFrame = False

; Check for cyclic point (default is True).

  if(get_res_value(res2_tmp,"gsnAddCyclic",True))
    datanew = gsn_add_cyclic_point(data2)
  else
    datanew = data2
  end if

  check_for_y_lat_coord(datanew,res2_tmp,"contour")
  check_for_lon_coord(datanew,res2_tmp,"contour")

; Default for second plot is to draw line labels, but make background
; label box transparent. Also, move the info label around depending on
; whether the labelbar from the first plot is on the side or the bottom.

  set_attr(res2_tmp,"cnLineLabelBackgroundColor",-1)
  if(.not.isatt(res2_tmp,"cnInfoLabelOrthogonalPosF"))
    if(res1_tmp@cnFillOn)
      if(check_attr(res1_tmp,"lbOrientation","horizontal",True))
        set_attr(res2_tmp,"cnInfoLabelOrthogonalPosF",-0.01)
      else
        set_attr(res2_tmp,"cnInfoLabelOrthogonalPosF",0.12)
      end if
    else
      set_attr(res2_tmp,"cnInfoLabelOrthogonalPosF",0.12)
    end if
  end if

  contour2 = gsn_contour(wks,datanew,res2_tmp)

  overlay(contour1,contour2)

; Draw all this stuff: contour plots and subtitles.
    draw_and_frame(wks,contour1,calldraw,callframe,maxbb)

; Return contour plot object.

  contour1@contour2 = contour2
  return(contour1)

end

;***********************************************************************;
; Function : gsn_csm_xy                                                 ;
;                     wks: workstation object                           ;
;                       x: X values                                     ;
;                       y: X values                                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a titled XY plot to the workstation   ;
; "wks" (the variable returned from a previous call to "gsn_open_wks"). ;
; "resources" is an optional list of  resources. The Id of the map plot ;
; is returned.                                                          ;
;                                                                       ;
; This function behaves differently from gsn_xy in that it will         ;
; add additional titles to the top of the plot if any of the special    ;
; GSUN resources "gsnLeftString," "gsnCenterString," and/or             ;
; "gsnRightString" are set, They are used to title the top left, center,;
; and right of the plot (in addition, the regular resource              ;
; "tiMainString" can be set to create a regular title).                 ;
;                                                                       ;
; If gsnYRefLine is set to some value(s), then reference line(s) will be;
; drawn at this value(s). In addition, if gsnAboveYRefLineColor and/or  ;
; gsnBelowYRefLineColor is set, the polygons above and below the        ;
; reference line will be filled in these colors.                        ;
;                                                                       ;
; If gsnXRefLine is set to some value, then a reference line will be    ;
; drawn at this value.                                                  ;
;                                                                       ;
; If gsnXYBarChart is set to True, then a bar chart will be drawn       ;
; instead of a line plot. You need to also set gsnYRefLine in order to  ;
; use this resource.                                                    ;
;                                                                       ;
; The resource gsnXYBarChartColors can be used to specify an array of   ;
; bar colors.                                                           ;
;                                                                       ;
; The resource gsnXYBarChartBarWidth can be used to specify the width   ;
; of each bar. The smallest dx is used by default.                      ;
;                                                                       ;
; The resource gsnAbove/BelowYRefLineBarColors can be used to specify   ;
; an array of bar colors.                                               ;
;                                                                       ;
; Tick marks will be made to point outwards.                            ;
;                                                                       ;
; If the user sets lgLabelFontHeightF to change the size of the legend  ;
; labels, then lgAutoManage will be set to False (unless it is being    ;
; set explicitly by the user.                                           ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_xy(wks:graphic,x:numeric,y:numeric,resources:logical)
local res, xy_object, res2, xfontf, yfontf, font_height, calldraw, nlines, \
callframe, left_string, center_string, right_string, main_zone, ncurves, \
yabove, ybelow, ya_ind, yb_ind, xinterp, yinterp, bar_chart
begin

; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
    xref_line_on  = False     ; X reference line flag
    yref_line_on  = False     ; Y reference line flag
    yref_fill_on  = False     ; Whether to fill above/below Y ref line.
    res2          = get_resources(resources)
    left_string   = False
    center_string = False
    right_string  = False
    bar_chart     = False

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Calculate number of Y points.

    ndimsy = dimsizes(dimsizes(y))
    ndimsx = dimsizes(dimsizes(x))

    if(ndimsy.eq.1)
      ncurves = 1
      npts    = dimsizes(y)
    else
      ncurves = dimsizes(y(:,0))
      npts    = dimsizes(y(0,:))
    end if

    if(ndimsx.eq.1)
      nptsx = dimsizes(x)
    else
      nptsx = dimsizes(x(0,:))
    end if
;
; Test dimensions. They must both either be the same, or have the
; same rightmost dimension.
;
    if( (ndimsx.gt.1.and.ndimsy.gt.1.and.(ndimsx.ne.ndimsy.or. \
         .not.all(dimsizes(x).eq.dimsizes(y)))).or.\
        ((ndimsx.eq.1.or.ndimsy.eq.1).and.nptsx.ne.npts))
      print("gsn_csm_xy: Fatal: X and Y must have the same dimensions sizes, or one must be one-dimensional and both have the same rightmost dimension.")
    end if

; This section tests for more special resources: those that start
; with "gsn."

    if((res2).and..not.any(ismissing(getvaratts(res2))))
;
; Check if gsnShape set.
;
      if(isatt(res2,"gsnShape").and.res2@gsnShape)
        main_zone     = main_zone+1 ; Zone for main title
      end if

; Check if gsnYRefLine set.

      if(isatt(res2,"gsnYRefLine"))
        yref_line_on = True
        yref_line    = res2@gsnYRefLine
        nyref        = dimsizes(yref_line)
        delete(res2@gsnYRefLine)
;
; Determine color of fill above reference line(s).
;
        if(isatt(res2,"gsnAboveYRefLineColor"))
          if(.not.(dimsizes(res2@gsnAboveYRefLineColor).eq.nyref.or.\
                   dimsizes(res2@gsnAboveYRefLineColor).eq.1))
            print("gsn_csm_xy: Fatal: there must either be an above-line fill color for every reference line, or one global above-line fill color specified")
            return
          end if
          yref_line_above = res2@gsnAboveYRefLineColor
          yref_fill_on    = True
          delete(res2@gsnAboveYRefLineColor)
        else
          yref_line_above = -1  ; defaults to no fill.
        end if
;
; Determine color of fill below reference line(s).
;
        if(isatt(res2,"gsnBelowYRefLineColor"))
          if(.not.(dimsizes(res2@gsnBelowYRefLineColor).eq.nyref.or.\
                   dimsizes(res2@gsnBelowYRefLineColor).eq.1))
            print("gsn_csm_xy: Fatal: there must either be an below-line fill color for every reference line, or one global below-line fill color specified ")
            return
          end if
          yref_line_below = get_res_value(res2,"gsnBelowYRefLineColor",1)
          yref_fill_on    = True
        else
          yref_line_below = -1    ; defaults to no fill.
        end if
;
; If yref_fill_on is True, and we have more than one curve, then there
; should be the same number of reference lines as curves.
;
        if(yref_fill_on.and.ncurves.gt.1.and.ncurves.ne.nyref.and.nyref.ne.1)
          print("gsn_csm_xy: Fatal: if you plan to fill above and/or below reference lines, then the number of reference lines must either equal the number")
          print("of curves, or be one.")
          return
        end if
      end if                            ; Check if gsnYRefLine set.
;
; Determine color of Y reference line.
;
      if(isatt(res2,"gsnYRefLineColor"))
        yref_line_color = res2@gsnYRefLineColor
        if(.not.(dimsizes(yref_line_color).eq.nyref.or.\
                 dimsizes(yref_line_color).eq.1))
          print("gsn_csm_xy: Fatal: you must specify either one Y reference line color")
          print("or the same number of colors as there are reference lines.")
          return
        end if
        delete(res2@gsnYRefLineColor)
      else
        yref_line_color = "foreground"  ; defaults to foreground color.
      end if
;
; Check if we want a bar chart. If so, gsnYRefLine must be set.
;
      bar_chart        = get_res_value(res2,"gsnXYBarChart",False)
      bar_outline_only = get_res_value(res2,"gsnXYBarChartOutlineOnly",False)
      bar_chart_orient = lower_case(get_res_value(res2, \
                                    "gsnXYBarChartOrientation","vertical"))
      if(bar_chart.and..not.bar_outline_only)
        if(.not.yref_line_on) then
;
; If no ref line specified, but bar fill is on, then set the reference
; line to the minimum y value.
;
          yref_line    = get_res_value_keep(res2,"trYMinF",min(y))
          nyref        = 1 
          yref_line_on = True
        else
          if((ncurves.gt.1.and.nyref.gt.1.and.ncurves.ne.nyref).or. \
             (ncurves.eq.1.and.nyref.ne.1))
            print("gsn_csm_xy: Fatal: if you plan to draw a bar chart with values above")
            print("and/or below reference lines, then the number of reference lines")
            print("must either equal the number of curves, or be one.")
            return
          end if
        end if          ; Check for Y ref line.
      end if            ; Check for bar chart settage.

; Check if gsnXRefLine set.

      if(isatt(res2,"gsnXRefLine"))
        xref_line_on = True
        xref_line    = get_res_value(res2,"gsnXRefLine",0.)
        nxref        = dimsizes(xref_line)

        xref_line_color = get_res_value(res2,"gsnXRefLineColor","foreground")
      end if

; Check for existence of the left, center, and right subtitles.

      check_for_subtitles(res2,left_string,center_string,right_string)
      if(left_string.or.center_string.or.right_string)
        main_zone   = main_zone+1
      end if
    end if                  ; Done checking special resources.

    res2          = True
    res2@gsnDraw  = False   ; Internally, don't draw plot or advance
    res2@gsnFrame = False   ; frame since we take care of that later.
    res2@gsnScale = True    ; force labels and ticks to be same.

; If user explicitly sets X or Y tick marks, then don't label the X 
; or Y axis automatically.

    if(check_attr(res2,"tmXBMode","Explicit",True))
      set_attr(res2,"tiXAxisString","")
    end if

    if(check_attr(res2,"tmYLMode","Explicit",True))
      set_attr(res2,"tiYAxisString","")
    end if

; Check if we want fill above or below Y reference value, and whether we
; want to do bar charts or regular curves.

    if(yref_fill_on.and..not.bar_chart) then
;
; Create new X and Y arrays to hold new data values.  Wherever the curve
; crosses the reference line, an interpolation needs to be done to create 
; a point *on* that reference line.
;
      xinterp = new((/nyref,2*npts/),double,1e36)
      yinterp = new((/nyref,2*npts/),double,1e36)

      if(isatt(x,"_FillValue"))
        xinterp@_FillValue = x@_FillValue
      end if

      if(isatt(y,"_FillValue"))
        yinterp@_FillValue = y@_FillValue
      end if

      new_npts = ref_line_interp(x,y,xinterp,yinterp,yref_line)
    else
      if(bar_chart) then
;
; Figure out if we want filled bars or outlined bars.
;
        if(bar_outline_only)
;
; For each old y point, we need two new y points to represent
; the horizontal line.
;
          nyb = 2 * (npts-1) + 1

          yinterp = new((/ncurves,nyb/),double,1e36)
          xinterp = new((/ncurves,nyb/),double,1e36)

          if(isatt(x,"_FillValue"))
            xinterp@_FillValue = x@_FillValue
          end if
          if(isatt(y,"_FillValue"))
            yinterp@_FillValue = y@_FillValue
          end if
          outlined_bars(x,y,xinterp,yinterp)
        else
;
; Filled bars are desired.  
;
; Get bar widths.  They are either set by user via a resource, or 
; from the smallest dx.
;
          bar_widths = get_bar_widths(x,ncurves,res2)
          if(any(ismissing(bar_widths)).or.any(bar_widths.le.0))
            print("gsn_csm_xy: Fatal: The x array is not monotonically increasing.")
            print("Cannot draw a bar chart.")
            return
          end if
;
; Check if gsnAbove/BelowYRefLineBarColors is set. If so, then values
; in these arrays will be used to color each above/below bar.
;
          mlt_above_bar_colors = False
          mlt_below_bar_colors = False
          mlt_above_bar_pttrns = False
          mlt_below_bar_pttrns = False
          if(isatt(res2,"gsnAboveYRefLineBarColors")) then
            yref_fill_on         = True
            mlt_above_bar_colors = True
            bar_colors_above = get_res_value(res2,"gsnAboveYRefLineBarColors",1)
;
; If both gsnAboveYRefLineBarColors and gsnXYBarChartColors are set,
; then only use the first one.
;
            if(isatt(res2,"gsnXYBarChartColors")) then
              delete(res2@gsnXYBarChartColors)
            end if
          end if  
          if(.not.mlt_above_bar_colors.and.isatt(res2,"gsnXYBarChartColors")) then
            yref_fill_on         = True
            mlt_below_bar_colors = True
            bar_colors_below = get_res_value(res2,"gsnBelowYRefLineBarColors",1)
;
; If both gsnBelowYRefLineBarColors and gsnXYBarChartColors are set,
; then only use the first one.
;
            if(isatt(res2,"gsnXYBarChartColors")) then
              delete(res2@gsnXYBarChartColors)
            end if
          end if

          if(isatt(res2,"gsnXYBarChartColors")) then
            yref_fill_on         = True
            mlt_above_bar_colors = True
            mlt_below_bar_colors = True
            bar_colors_above = get_res_value(res2,"gsnXYBarChartColors",1)
            bar_colors_below = bar_colors_above
          end if
          if(isatt(res2,"gsnAboveYRefLineBarPatterns")) then
            yref_fill_on         = True
            mlt_above_bar_pttrns = True
            bar_pttrns_above = get_res_value(res2,"gsnAboveYRefLineBarPatterns",1)
;
; If both gsnAboveYRefLineBarPatterns and gsnXYBarChartPatterns are set,
; then only use the first one.
;
            if(isatt(res2,"gsnXYBarChartPatterns")) then
              delete(res2@gsnXYBarChartPatterns)
            end if
          end if  

          if(isatt(res2,"gsnBelowYRefLineBarPatterns")) then
            yref_fill_on         = True
            mlt_below_bar_pttrns = True
            bar_pttrns_below = get_res_value(res2,"gsnBelowYRefLineBarPatterns",1)
;
; If both gsnBelowYRefLineBarPatterns and gsnXYBarChartPatterns are set,
; then only use the first one.
;
            if(isatt(res2,"gsnXYBarChartPatterns")) then
              delete(res2@gsnXYBarChartPatterns)
            end if
          end if  

          if(isatt(res2,"gsnXYBarChartPatterns")) then
            yref_fill_on         = True
            mlt_above_bar_pttrns = True
            mlt_below_bar_pttrns = True
            bar_pttrns_above = get_res_value(res2,"gsnXYBarChartPatterns",1)
            bar_pttrns_below = bar_pttrns_above
          end if  
;
; Loop across curves and figure out which points are above and
; below references lines.  First create arrays to hold points
; above and below reference lines.
;
          xabove = new((/ncurves,5*npts/),double,1e36)
          yabove = new((/ncurves,5*npts/),double,1e36)
          xbelow = new((/ncurves,5*npts/),double,1e36)
          ybelow = new((/ncurves,5*npts/),double,1e36)
;
; Arrays to hold number of points in each set of above/below points.
;
          nabove = new(ncurves,integer)
          nbelow = new(ncurves,integer)
;
; Fill in values for filled bars (that will be filled later).
;
          filled_bars(x,y,xabove,yabove,xbelow,ybelow,nabove,nbelow, \
                      yref_line,bar_widths)
;
; Combine the above/below arrays into one X/Y array so that we can pass
; to gsn_xy routine. Make the 5th point missing, since the 5th point
; closes the polygon, and we only want an outline here.
;
          na_max  = max(nabove)
          nb_max  = max(nbelow)
          nab_max = na_max + nb_max

          if(na_max.le.0.and.nb_max.le.0)
            print("gsn_csm_xy: Fatal: There are no values above or below the chosen reference line.")
            print("Cannot draw a bar chart.")
            xinterp = x
            yinterp = y
          else
            xinterp = new((/2*ncurves,nab_max/),double,xabove@_FillValue)
            yinterp = new((/2*ncurves,nab_max/),double,yabove@_FillValue)

            if(na_max.gt.0)
              xinterp(0:2*ncurves-1:2,0:na_max-1) = xabove(:,0:na_max-1)
              yinterp(0:2*ncurves-1:2,0:na_max-1) = yabove(:,0:na_max-1)
              yinterp(0:2*ncurves-1:2,4:na_max-1:5) = yinterp@_FillValue
            end if
            if(nb_max.gt.0)
              xinterp(1:2*ncurves-1:2,0:nb_max-1) = xbelow(:,0:nb_max-1)
              yinterp(1:2*ncurves-1:2,0:nb_max-1) = ybelow(:,0:nb_max-1)
              yinterp(1:2*ncurves-1:2,4:nb_max-1:5) = yinterp@_FillValue
            end if
;
; Make sure all lines are solid, and not dashed as is the default
; with the second curve drawn.
; 
            res2@xyDashPatterns = get_res_value(res2,"xyDashPatterns",0)
          end if
        end if
      else
        xinterp = x
        yinterp = y
      end if
    end if

    if(xref_line_on)
      set_attr(res2,"trYMinF",min(yinterp))
      set_attr(res2,"trYMaxF",max(yinterp))
    end if

    if(yref_line_on)
      set_attr(res2,"trXMinF",min(xinterp))
      set_attr(res2,"trXMaxF",max(xinterp))
    end if

;
; If user sets lgLabelFontHeightF, then lgAutoManage needs to
; be False in order for this resource to have any affect.
;
    if(isatt(res2,"lgLabelFontHeightF"))
      set_attr(res2,"lgAutoManage",False)
    end if

; Create XY plot
    xyres = get_res_ne(res2,(/"tx"/))
    xy_object = gsn_xy(wks,xinterp,yinterp,xyres)

; Add lat/lon labels to X/Y axes if appropriate units exist.

    res2@gsnXYPlot = True
    res2@gsnXAxis = True

    add_latlon_labels(xy_object,x,res2)
    delete(res2@gsnXAxis)
    res2@gsnYAxis = True
    add_latlon_labels(xy_object,y,res2)
    delete(res2@gsnYAxis)
    delete(res2@gsnXYPlot)

; Fill above/below Y reference line.

    if(yref_fill_on.and..not.bar_chart)
;
; Make sure polygons get filled before xy plot is drawn. This way,
; any lines will be drawn *after* the fill. This makes the filled 
; polygons look better if the lines are drawn thicker.
;
      xyres_tmp = True
      xyres_tmp@tfPolyDrawOrder =  "Predraw"
      attsetvalues_check(xy_object,xyres_tmp)
      delete(xyres_tmp)

      fill_above = yref_line_above(0)
      fill_below = yref_line_below(0)

      do i=0,max((/ncurves,nyref/))-1
        if(dimsizes(yref_line_above).gt.1)
          fill_above = yref_line_above(i)
        end if
        if(dimsizes(yref_line_below).gt.1)
          fill_below = yref_line_below(i)
        end if
        var_string = systemfunc("echo polygons$$") + i
        xy_object@$var_string$=fill_xy_ref(wks,xy_object, \
                                     xinterp(i,0:new_npts(i)-1), \
                                     yinterp(i,0:new_npts(i)-1),\
                                     yref_line(i),fill_above,fill_below)
      end do
    end if

; Fill bars with either a single color for above and below, or with
; individual colors.

    if(yref_fill_on.and.bar_chart.and..not.bar_outline_only)
      gsres = True
      do i=0,ncurves-1 
        var_string = systemfunc("echo polygons$$") + i
;
; Fill above reference line.
;
        if(nabove(i).gt.0)
;
; Single bar color for each above and below bars.
;
          if(.not.mlt_above_bar_colors)
            gsres@gsFillColor = yref_line_above(0)
            if(dimsizes(yref_line_above).gt.1)
              gsres@gsFillColor = yref_line_above(i)
            end if
          end if
;
; Multiple bar colors and/or patterns.
;
          if(mlt_above_bar_colors.or.mlt_above_bar_pttrns) then
            if(mlt_above_bar_colors) then
              nca = dimsizes(bar_colors_above)
            end if
            if(mlt_above_bar_pttrns) then
              npa = dimsizes(bar_pttrns_above)
            end if
            do ii = 0,npts-1
              var_string2 = var_string + "above" + ii
              if(mlt_above_bar_colors) then
                gsres@gsFillColor = bar_colors_above(ii%nca)
              end if
              if(mlt_above_bar_pttrns) then
                gsres@gsFillIndex = bar_pttrns_above(ii%npa)
              end if
              xy_object@$var_string2$ = gsn_add_polygon(wks,xy_object, \
                                                     xabove(i,ii*5:ii*5+4), \
                                                     yabove(i,ii*5:ii*5+4), \
                                                     gsres)
            end do
          else
            var_string2 = var_string + "above"
            xy_object@$var_string2$ = gsn_add_polygon(wks,xy_object, \
                                                     xabove(i,:), \
                                                     yabove(i,:),gsres)
          end if
        end if
;
; Fill below reference line.
;
        if(nbelow(i).gt.0)
          if(.not.mlt_below_bar_colors)
            gsres@gsFillColor = yref_line_below(0)
            if(dimsizes(yref_line_below).gt.1)
              gsres@gsFillColor = yref_line_below(i)
            end if
          end if
;
; Multiple bar colors and/or patterns.
;
          if(mlt_below_bar_colors.or.mlt_below_bar_pttrns) then
            if(mlt_below_bar_colors) then
              ncb = dimsizes(bar_colors_below)
            end if
            if(mlt_below_bar_pttrns) then
              npb = dimsizes(bar_colors_pttrns)
            end if
            do ii = 0,npts-1
              var_string2 = var_string + "below" + ii
              if(mlt_below_bar_colors) then
                gsres@gsFillColor = bar_colors_below(ii%ncb)
              end if
              if(mlt_below_bar_pttrns) then
                gsres@gsFillIndex = bar_colors_pttrns(ii%npb)
              end if
              xy_object@$var_string2$ = gsn_add_polygon(wks,xy_object, \
                                                     xbelow(i,ii*5:ii*5+4), \
                                                     ybelow(i,ii*5:ii*5+4), \
                                                     gsres)
            end do
          else
            var_string2 = var_string + "below"
            xy_object@$var_string2$ = gsn_add_polygon(wks,xy_object, \
                                                     xbelow(i,:), \
                                                     ybelow(i,:), gsres)
          end if
        end if
      end do
;
; Make sure polygons get drawn first, so that XY lines are drawn on top.
;
      setvalues xy_object
        "tfPolyDrawOrder" : "Predraw"
      end setvalues

      delete(gsres)    
    end if

; Draw X reference line.

    if(xref_line_on)
      getvalues xy_object
        "trYMinF"  : ymin
        "trYMaxF"  : ymax
      end getvalues

      gsres             = True

      yline = (/ymin,ymax/)

      gsres@gsLineColor = xref_line_color(0)

      do i=0,nxref-1
        if(dimsizes(xref_line_color).gt.1)
          gsres@gsLineColor = xref_line_color(i)
        end if
        xline = fspan(xref_line(i),xref_line(i),2)
        var_string = systemfunc("echo xpolyline$$") + i
        xy_object@$var_string$ = gsn_add_polyline(wks,xy_object,xline,yline, \
                                                  gsres)
      end do
      delete(xline)
      delete(yline)
    end if

; Draw Y reference line.

    if(yref_line_on)
      getvalues xy_object
        "trXMinF"  : xmin
        "trXMaxF"  : xmax
      end getvalues

      gsres             = True

      xline = (/xmin,xmax/)

      gsres@gsLineColor = yref_line_color(0)

      do i=0,nyref-1
        if(dimsizes(yref_line_color).gt.1)
          gsres@gsLineColor = yref_line_color(i)
        end if
        yline = fspan(tofloat(yref_line(i)),tofloat(yref_line(i)),2)
        var_string = systemfunc("echo ypolyline$$") + i

        xy_object@$var_string$ = gsn_add_polyline(wks,xy_object,xline, \
                                                  yline,gsres)
      end do

      delete(xline)
      delete(yline)
    end if

; Get title label sizes and tickmark lengths.
    getvalues xy_object
      "vpWidthF"             : width
      "vpHeightF"            : height
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf

      "tmYLMajorLengthF"     : ylength
      "tmXBMajorLengthF"     : xlength
      "tmYLMinorLengthF"     : ymlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of
                                          ; the size of the X/Y axis labels.
    major_length = min((/ylength,xlength/))
    minor_length = min((/ymlength,xmlength/))

; If the plot is close to square in size, then make the 
; three top titles and the tick marks smaller.

    ratio = height/width
    if(ratio.gt.1) 
      ratio = 1./ratio
    end if
    if(ratio.gt.0.5)
      font_height = 0.75 * font_height
      major_length = 0.75 * major_length
      minor_length = 0.75 * minor_length
    end if

; Make tick marks same length and point outward.

    tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
    gsnp_point_tickmarks_outward(xy_object,tmres,xlength,ylength,xmlength, \
                                 ymlength,major_length,minor_length)

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres               = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,xy_object,left_string,center_string,right_string, \
                  subres)

; Draw all this stuff: XY plot and subtitles.
    draw_and_frame(wks,xy_object,calldraw,callframe,maxbb)

; Return XY plot object.

    return(xy_object)
end

;***********************************************************************;
; Function : gsn_csm_y                                                  ;
;                   wks: workstation object                             ;
;                     y: n-dimensional array of Y array                 ;
;             resources: optional resources                             ;
;                                                                       ;
; This function is similar to gsn_csm_xy, except instead of a specific  ;
; X array, index values are used.                                       ;
;                                                                       ;
;***********************************************************************;
function gsn_csm_y(wks:graphic, y:numeric, resources:logical )
local dsizes_y, npts, x, rank_y, xy
begin
  res2 = get_resources(resources)
;
; Get dimension sizes of Y array.
;
  dsizes_y = dimsizes(y)
  rank_y   = dimsizes(dsizes_y)
  if(rank_y.eq.1) then
    npts = dsizes_y
  else
    if(rank_y.ne.2) then
      print("Error: gsn_csm_y: The input Y array must either be 1-dimensional, or 2-dimensional, where the leftmost dimension represents the number of curves and the rightmost dimension the number of points in each curve.")
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
; Call gsn_csm_xy.
;
  xy = gsn_csm_xy(wks,x,y,res2)
  return(xy)
end


;***********************************************************************;
; Function : gsn_csm_contour                                            ;
;                     wks: workstation object                           ;
;                    data: data to contour                              ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a titled contour plot to the          ;
; workstation  "wks" (the variable returned from a previous call to     ;
; "gsn_open_wks"). "resources" is an optional list of  resources. The   ;
; Id of the contour plot is returned.                                   ;
;                                                                       ;
; This function behaves differently from gsn_contour in that it will    ;
; add additional titles to the top of the plot if any of the special    ;
; GSUN resources "gsnLeftString," "gsnCenterString," and/or             ;
; "gsnRightString" are set, They are used to title the top left, center,;
; and right of the plot (in addition, the regular resource              ;
; "tiMainString" can be set to create a regular title).                 ;
;                                                                       ;
; If cnFillOn is True, then a labelbar will be added, and the info label;
; will be removed.
;                                                                       ;
; Tick marks will be made to point outwards.                            ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_contour(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, xfontf, yfontf, font_height, lbar_on, \
calldraw, callframe, left_string, center_string, right_string, \
main_zone, datanew, popgrid, contour_plot
begin

; Initialize.
    lbar_on       = False     ; Labelbar flag
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = get_resources(resources)

; This section tests for more special resources: those that start
; with "gsn."

; The default is to not add a cyclic point.

    if(get_res_value(res2,"gsnAddCyclic",False))
        datanew = gsn_add_cyclic_point(data)
    else
        datanew = data
    end if
;
; Check if frame and/or draw are not supposed to be called.
;
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
; 
; Check if a POP grid is to be overlaid.
;
    popgrid = get_res_value(res2,"gsnPopGrid","")
;
; If adding a POP grid, then don't label the X and Y axis since the
; axes will have lat/lon labels.
;
    if(popgrid.ne."")
      set_attr(res2,"tiXAxisString","")
      set_attr(res2,"tiYAxisString","")
    end if

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(datanew,res2,res2)
    set_left_subtitle(datanew,res2,res2)

; Check for existence of the left, center, and right subtitles.

    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)

    if(left_string.or.center_string.or.right_string)
      main_zone   = main_zone+1
    end if

; Check if gsnShape set.

    if(check_attr(res2,"gsnShape",True,False))
      main_zone     = main_zone+1 ; Zone for main title
    end if

; Use coordinate variables for X and/or Y if they exist.

    check_for_coord_arrays(datanew,res2,"contour")
;
; Turn on a labelbar if fill is True and if the labelbar is not
; explicitly being turned off.
;
    if(check_attr(res2,"cnFillOn",True,False).and. \
       (.not.isatt(res2,"lbLabelBarOn").or.\
        .not.check_attr(res2,"lbLabelBarOn",False,False)))
      lbar_on   = True
      set_attr(res2,"cnLineLabelsOn",False)
      set_attr(res2,"cnInfoLabelOn",False)
    end if
    if(isatt(res2,"lbLabelBarOn"))
      delete(res2@lbLabelBarOn)
    end if

    res2          = True
    res2@gsnDraw  = False   ; Internally, don't draw plot or advance
    res2@gsnFrame = False   ; frame since we take care of that later.
    res2@gsnScale = True    ; force labels and ticks to be same.

    cnres = get_res_ne(res2,(/"tx"/))
    contour_object = gsn_contour(wks,datanew,cnres)

; Get some information from contour plot that was created.

    contour_plot = check_class_name(contour_object,"contour")

; Add lat/lon labels to X/Y axes if appropriate coordinate arrays exist.

    add_latlon_labels(contour_plot,datanew,res2)

    getvalues contour_plot
      "vpWidthF"             : width
      "vpHeightF"            : height
  
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf

      "tmYLMajorLengthF"     : ylength
      "tmXBMajorLengthF"     : xlength
      "tmYLMinorLengthF"     : ymlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make subtitle label sizes a
                                          ; function of the size of the
                                          ; X/Y axis labels.

; If the plot is close to square in size, then make the 
; three top titles and the tick marks smaller.

    font_scale = (/1.0,0.8,0.8/)
    ratios     = (/0.5,0.75,1.0/)

    ratio = height/width
    if(ratio.gt.1) 
      ratio = 1./ratio
    end if
    index = ind(ratio.le.ratios)

    scale = font_scale(index(0))
    font_height  = scale * font_height

    if(popgrid.ne."") then
      pop_latlon_grid(wks,contour_object,popgrid)  ; Add a POP grid.
    else
;
; Make tick marks same length and point outward.
;
      major_length = scale * min((/ylength,xlength/))
      minor_length = scale * min((/ymlength,xmlength/))

      tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
      gsnp_point_tickmarks_outward(contour_plot,tmres,xlength,ylength,\
                                                xmlength,ymlength,\
                                                major_length,minor_length)
    end if

; Create a labelbar.

    if (lbar_on)
      lbres = get_res_eq(res2,(/"lb","pm"/))
      add_labelbar(wks,contour_object,3,font_height,"contour",lbres)
    end if
  
; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,contour_object,left_string,center_string,\
                  right_string,subres)

; Draw all this stuff: contour plot and subtitles.
    draw_and_frame(wks,contour_object,calldraw,callframe,maxbb)

; Return contour plot object.

    return(contour_object)
end
;***********************************************************************;
; Function : gsn_csm_hov                                                ;
;                     wks: workstation object                           ;
;                    data: data to be contoured                         ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a titled Hovmueller plot to the       ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks"). "resources" is an optional list of  resources. The   ;
; Id of the contour plot is returned.                                   ;
;                                                                       ;
; This function behaves differently from gsn_csm_contour in that it     ;
; draws a specially-labelled longitude X axis.                          ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_hov(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, font_height, calldraw, callframe, \
datanew, lon_spacing, contour_plot
begin

; Initialize.
    res2         = get_resources(resources)

    calldraw     = get_res_value(res2,"gsnDraw", True)
    callframe    = get_res_value(res2,"gsnFrame",True)
    maxbb        = get_bb_res(res2)
    lon_spacing  = get_res_value(res2,"gsnMajorLonSpacing",0)
    mlon_spacing = get_res_value(res2,"gsnMinorLonSpacing",0)

    if(is_valid_lat_xcoord(data).or.is_valid_lon_xcoord(data))
      set_attr(res2,"tiXAxisString","")
    end if

; Use coordinate variable for X if it exists.

    if(is_valid_xcoord(data).and..not.(isatt(res2,"sfXCStartV").and. \
       isatt(res2,"sfXCEndV")))
      set_attr(res2,"sfXArray",data&$data!1$)
    end if

    res2           = True
    res2@gsnDraw   = False
    res2@gsnFrame  = False

    contour_object = gsn_csm_contour(wks,data,res2) ; Create a contour plot.

; Check if we should label X axis with nice longitude labels.

    if(is_valid_lon_xcoord(data)) then

; Get min and max X axis values (longitude units)

      contour_plot = check_class_name(contour_object,"contour")

      getvalues contour_plot
        "trXMinF"   : min_lon
        "trXMaxF"   : max_lon
      end getvalues
;
; We want different defaults for longitude spacing, so set them up here.
;
      lon_range        = (/ 20,  40,  60, 180, 360/)
      lon_spacing_arr  = (/ 10,  20,  30,  45,  60/)
      mlon_spacing_arr = (/  5,   5,  10,  15,  30/)
      add_lon_labels(contour_plot,min_lon,max_lon,lon_spacing,mlon_spacing,\
                     lon_range,lon_spacing_arr,mlon_spacing_arr,res2)
    end if

; Draw all this stuff: contour plot and subtitles.
    draw_and_frame(wks,contour_object,calldraw,callframe,maxbb)

; Return contour plot object.

    return(contour_object)
end

;***********************************************************************;
; Function : gsn_csm_lat_time                                           ;
;                     wks: workstation object                           ;
;                    data: data to be contoured                         ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a titled Lat/time plot to the         ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks"). "resources" is an optional list of  resources. The   ;
; Id of the contour plot is returned.                                   ;
;                                                                       ;
; This function behaves differently from gsn_csm_contour in that it     ;
; draws a specially-labelled latitude Y axis.                           ;
;                                                                       ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_lat_time(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, font_height, calldraw, callframe, \
datanew, lat_spacing
begin

; Initialize.
    res2         = get_resources(resources)

    calldraw     = get_res_value(res2,"gsnDraw", True)
    callframe    = get_res_value(res2,"gsnFrame",True)
    maxbb        = get_bb_res(res2)
    lat_spacing  = get_res_value(res2,"gsnMajorLatSpacing",0)
    mlat_spacing = get_res_value(res2,"gsnMinorLatSpacing",0)

    if((res2).and..not.any(ismissing(getvaratts(res2))))
      set_attr(res2,"tiYAxisString","")
    end if

; Use coordinate variable for Y if it exists.

    check_for_y_lat_coord(data,res2,"contour")

    res2           = True
    res2@gsnDraw   = False
    res2@gsnFrame  = False

    contour_object = gsn_csm_contour(wks,data,res2) ; Create a contour plot.

; Get min and max Y axis values (latitude units)

    contour_plot = check_class_name(contour_object,"contour")

    getvalues contour_plot
      "trYMinF"   : min_lat
      "trYMaxF"   : max_lat
    end getvalues

    add_lat_labels_yaxis(contour_plot,min_lat,max_lat,\
                         lat_spacing,mlat_spacing,res2)

; Draw all this stuff: contour plot and subtitles.
    draw_and_frame(wks,contour_object,calldraw,callframe,maxbb)

; Return contour plot object.

    return(contour_object)
end


;***********************************************************************;
; Function : gsn_csm_time_lat                                           ;
;                     wks: workstation object                           ;
;                    data: data to be contoured                         ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a titled time/lat plot to the         ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks"). "resources" is an optional list of  resources. The   ;
; Id of the contour plot is returned.                                   ;
;                                                                       ;
; This function behaves differently from gsn_csm_contour in that it     ;
; draws a specially-labeled latitude X axis.                            ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_time_lat(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, font_height, calldraw, callframe, \
datanew, lat_spacing
begin

; Initialize.
    res2         = get_resources(resources)

    calldraw     = get_res_value(res2,"gsnDraw", True)
    callframe    = get_res_value(res2,"gsnFrame",True)
    maxbb        = get_bb_res(res2)
    lat_spacing  = get_res_value(res2,"gsnMajorLatSpacing",15)
    mlat_spacing = get_res_value(res2,"gsnMinorLatSpacing",5)

    if((res2).and..not.any(ismissing(getvaratts(res2))))
      set_attr(res2,"tiXAxisString","")
    end if

; Use coordinate variable for X if it exists.

    check_for_x_lat_coord(data,res2,"contour")

    res2           = True
    res2@gsnDraw   = False
    res2@gsnFrame  = False

    contour_object = gsn_csm_contour(wks,data,res2) ; Create a contour plot.

; Get min and max X axis values (latitude units)

    contour_plot = check_class_name(contour_object,"contour")

    getvalues contour_plot
      "trXMinF"   : min_lat
      "trXMaxF"   : max_lat
    end getvalues

    add_lat_labels_xaxis(contour_plot,min_lat,max_lat,lat_spacing,\
                         mlat_spacing,res2)

; Draw all this stuff: contour plot and subtitles.
    draw_and_frame(wks,contour_object,calldraw,callframe,maxbb)

; Return contour plot object.

    return(contour_object)
end


;***********************************************************************;
; Function : gsn_csm_pres_hgt                                           ;
;                     wks: workstation object                           ;
;                    data: data to be contoured                         ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a titled contour plot to the          ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks"). "resources" is an optional list of  resources. The   ;
; Id of the contour plot is returned.                                   ;
;                                                                       ;
; This function behaves differently from gsn_csm_contour in that it     ;
; draws a specially-labelled pressure axis on one side and a height axis;
; on the other side.  It also labels the longitude X axis, if one       ;
; exists.                                                               ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_pres_hgt(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, xfontf, yfontf, font_height, \
calldraw, callframe, add_hgt, contour_plot
begin

; Initialize.
    add_hgt       = False
    pres_reverse  =  True  ; Default is to assume pressure values
                           ; already reversed.
    res2          = get_resources(resources)

    calldraw     = get_res_value(res2,"gsnDraw", True)
    callframe    = get_res_value(res2,"gsnFrame",True)
    maxbb        = get_bb_res(res2)
    lat_spacing  = get_res_value(res2,"gsnMajorLatSpacing",0)
    mlat_spacing = get_res_value(res2,"gsnMinorLatSpacing",0)

    if(is_valid_ycoord(data).and.isatt(data&$data!0$,"long_name"))
      set_attr(res2,"tiYAxisString",data&$data!0$@long_name)
    end if

;
; If this is a pressure/hgt plot, then do stuff to the pressure values.
;
    if(is_valid_ycoord(data))
      npres = dimsizes(data&$data!0$)
;
; Check for pressure values.  If they are not in descending order, then
; reverse 'em.
;
      if(data&$data!0$(0).lt.data&$data!0$(npres-1))
        pres = tofloat(data&$data!0$(::-1))  ; reverse the values
        pres_reverse =  False          ; pres values weren't already reversed
       else
        pres = tofloat(data&$data!0$)
      end if
;
; Check the units.
;
      if(isatt(data&$data!0$,"units").and.\
         any(data&$data!0$@units.eq.get_allowed_pres_units()))
;
; If pressure values are Pascals, convert them to millibars.
;
        if(any(data&$data!0$@units.eq.get_allowed_pres_units_pa()))
          pres = data&$data!0$ * 0.01  ; convert to mb
          pres@units = "mb"
        end if
      else
        print("gsn_csm_pres_hgt: Fatal: The coordinate array for the first dimension of the input data must be in Pascals, Hecto-pascals, or millibars")
        print("and it must contain the attribute 'units' set to one of the following strings (depending on your units):")
        print("    " + cat_strings(get_allowed_pres_units))
        print("Cannot create plot.")
        return
      end if

      set_pres_hgt_axes(pres,res2,add_hgt)
    else
      print("gsn_csm_pres_hgt: Fatal: The first dimension of the input data must")
      print("have a coordinate variable called 'lev.'")
      print("Cannot create plot.")
      return
    end if

    log_y = get_res_value(res2,"trYLog",True)  ; Check if Y axis to stay linear.
    res2           = True
    res2@gsnDraw   = False
    res2@gsnFrame  = False

    if(pres_reverse)
      contour_object = gsn_csm_hov(wks,data,res2) ; Create a contour plot.
    else
      contour_object = gsn_csm_hov(wks,data(::-1,:),res2)  ; reverse Y
    end if

; Retrieve some values from the contour plot so we can create a LogLin
; plot with the same information.

    contour_plot = check_class_name(contour_object,"contour")

    getvalues contour_plot
      "vpXF"                 : xvp
      "vpYF"                 : yvp
      "vpWidthF"             : widthvp
      "vpHeightF"            : heightvp
      "trXMinF"              : xmin
      "trXMaxF"              : xmax
      "trYMinF"              : ymin
      "trYMaxF"              : ymax
      "tiYAxisFontHeightF"   : yaxis_font_height
      "tmYLLabelFontHeightF" : yaxis_label_height
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make subtitles sizes a function of
                                          ; the size of the X/Y axis labels.
    setvalues contour_plot
      "tmYLLabelFontHeightF" : yaxis_label_height * 0.8
      "tmYRLabelFontHeightF" : yaxis_label_height * 0.8
      "tmXBLabelFontHeightF" : yaxis_label_height * 0.8
    end setvalues

; Check if we should label X axis with nice latitude labels.

    if(is_valid_lat_xcoord(data)) then
      add_lat_labels_xaxis(contour_plot,xmin,xmax,lat_spacing,  \
                           mlat_spacing,res2)
    end if
;
; Add a right Y axis "height" label.
;
    if(add_hgt)
      rightaxis_string = create "right_axis" textItemClass wks
        "txString"      : "Height (km)"
        "txFontHeightF" : yaxis_font_height
        "txAngleF"      : 90.
      end create

      anno = NhlAddAnnotation(contour_object,rightaxis_string)

      setvalues anno
        "amZone"          : 3      ; Just outside plot area
        "amJust"          : "centercenter"
        "amSide"          : "right" 
        "amParallelPosF"  : 0.5
        "amOrthogonalPosF": 0.03
        "amResizeNotify"  : True     ; Resize if plot resized.
      end setvalues
    end if

    wksname = get_res_value_keep(wks,"name","gsnapp")

    loglin_object = create wksname + "_loglin" logLinPlotClass wks
      "vpXF"       : xvp
      "vpYF"       : yvp
      "vpWidthF"   : widthvp
      "vpHeightF"  : heightvp
      "trYReverse" : True
      "trXMinF"    : xmin
      "trXMaxF"    : xmax
      "trYMinF"    : ymin
      "trYMaxF"    : ymax
      "trYLog"     : log_y
    end create
;
; Check for transformation resources and apply them to loglin plot.
; Not all transformation resources can be applied to the loglin plot.
; For example, you can't apply IrregularTransformation resources. So,
; we have to explicitly test for these. 
;
    trres = get_res_eq(res2,"tr")

    irreg_res = (/"trXAxisType","trXCoordPoints","trXInterPoints", \
                  "trXTensionF","trXSamples","trYAxisType","trYCoordPoints",\
                  "trYInterPoints","trYTensionF","trYSamples"/)

    do i = 0, dimsizes(irreg_res)-1
      if(isatt(trres,irreg_res(i))) then
        delete(trres@$irreg_res(i)$)        ; Remove irreg trans resource.
      end if
    end do

    attsetvalues_check(loglin_object,trres)

; Overlay this on a LogLin plot so we can get a logarithmic Y axis.

    overlay(loglin_object,contour_object)

; Draw all this stuff: contour plot and subtitles.
    draw_and_frame(wks,loglin_object,calldraw,callframe,maxbb)

; Return loglin plot with contour plot object as an attribute.

    loglin_object@contour = contour_object
    loglin_object@data    = contour_object@data
    return(loglin_object)
end

;***********************************************************************;
; Function : gsn_csm_vector                                             ;
;                     wks: workstation object                           ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a titled vector plot to the           ;
; workstation  "wks" (the variable returned from a previous call to     ;
; "gsn_open_wks"). "resources" is an optional list of  resources. The   ;
; Id of the vector plot is returned.                                    ;
;                                                                       ;
; This function behaves differently from gsn_vector in that it will     ;
; add additional titles to the top of the plot if any of the special    ;
; GSUN resources "gsnLeftString," "gsnCenterString," and/or             ;
; "gsnRightString" are set, They are used to title the top left, center,;
; and right of the plot (in addition, the regular resource              ;
; "tiMainString" can be set to create a regular title).                 ;
;                                                                       ;
; Tick marks will be made to point outwards.                            ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_vector(wks:graphic,u[*][*]:numeric,v[*][*],resources:logical)
local res, vector_object, res2, xfontf, yfontf, font_height, lbar_on, \
calldraw, callframe, left_string, center_string, right_string, \
main_zone, unew, vnew
begin

; Initialize.
    lbar_on       = False     ; Labelbar flag
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = get_resources(resources)

; This section tests for more special resources: those that start
; with "gsn."

; The default is to not add a cyclic point.

    if(get_res_value(res2,"gsnAddCyclic",False))
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if
;
; Check if frame and/or draw are not supposed to be called.
;
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,res2)
    set_left_subtitle(unew,res2,res2)

; Check for existence of the left, center, and right subtitles.

    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)

    if(left_string.or.center_string.or.right_string)
      main_zone   = main_zone+1
    end if

; Check if gsnShape set.

    if(check_attr(res2,"gsnShape",True,False))
      main_zone     = main_zone+1 ; Zone for main title
    end if

; Use coordinate variables for X and/or Y if they exist.

    check_for_coord_arrays(unew,res2,"vector")
;
; Turn on a labelbar if fill is True and if the labelbar is not
; explicitly being turned off.
;
    if(check_attr(res2,"vcMonoLineArrowColor",False,False).and.\
       (.not.isatt(res2,"lbLabelBarOn").or.\
        check_attr(res2,"lbLabelBarOn",True,False)))
      lbar_on   = True
    end if
    if(isatt(res2,"lbLabelBarOn"))
      delete(res2@lbLabelBarOn)
    end if

    res2          = True
    res2@gsnDraw  = False   ; Internally, don't draw plot or advance
    res2@gsnFrame = False   ; frame since we take care of that later.
    res2@gsnScale = True    ; force labels and ticks to be same.

    vcres = get_res_ne(res2,(/"tx"/))
    vector_object = gsn_vector(wks,unew,vnew,vcres)

; Get some information from vector plot that was created.

    vector_plot = check_class_name(vector_object,"vector")

; Add lat/lon labels to X/Y axes if appropriate coordinate arrays exist.

    add_latlon_labels(vector_plot,unew,res2)

    getvalues vector_plot
      "vpWidthF"             : width
      "vpHeightF"            : height
  
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
  
      "tmYLMajorLengthF"     : ylength
      "tmXBMajorLengthF"     : xlength
      "tmYLMinorLengthF"     : ymlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make subtitle label sizes a
                                          ; function of the size of the
                                          ; X/Y axis labels.

    major_length = min((/ylength,xlength/))    ; New length for major ticks.
    minor_length = min((/ymlength,xmlength/))  ; New length for minor ticks.

; If the plot is close to square in size, then make the 
; three top titles and the tick marks smaller.

    font_scale = (/1.0,0.8,0.8/)
    ratios     = (/0.5,0.75,1.0/)

    ratio = height/width
    if(ratio.gt.1) 
      ratio = 1./ratio
    end if
    index = ind(ratio.le.ratios)

    scale = font_scale(index(0))
    font_height  = scale * font_height
;
; Make tick marks same length and point outward.
;
    major_length = scale * major_length
    minor_length = scale * minor_length

    tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
    gsnp_point_tickmarks_outward(vector_plot,tmres,xlength,ylength,\
                                 xmlength,ymlength,\
                                 major_length,minor_length)
; Create a labelbar.

    if (lbar_on)
      lbres = get_res_eq(res2,(/"lb","pm"/))
      add_labelbar(wks,vector_object,3,font_height,"vector",lbres)
    end if
  
; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"am","tx"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,vector_object,left_string,center_string,\
                  right_string,subres)

; Draw all this stuff: vector plot and subtitles.
    draw_and_frame(wks,vector_object,calldraw,callframe,maxbb)

; Return vector plot object.

    return(vector_object)
end

;***********************************************************************;
; Function : gsn_csm_pres_hgt_vector                                    ;
;                     wks: workstation object                           ;
;                    data: data to be contoured                         ;
;                       u: u component of vectors                       ;
;                       v: u component of vectors                       ;
;               resources: optional resources                           ;
;                                                                       ;
; This function behaves like gsn_csm_pres_hgt, only it overlays a       ;
; vector plot as well.                                                  ;
;***********************************************************************;

function gsn_csm_pres_hgt_vector(wks:graphic,data[*][*]:numeric, \
                                 u[*][*]:numeric, v[*][*]:numeric, \
                                 resources:logical)
local res2, cnres, vcres, calldraw, callframe, npres, lbar_zone, anno_zone
begin
  res2      = get_resources(resources)

  calldraw  = get_res_value(res2,"gsnDraw", True)
  callframe = get_res_value(res2,"gsnFrame",True)
  maxbb     = get_bb_res(res2)
  anno_zone = get_res_value(res2,"vcRefAnnoZone",3)
  ypos      = get_res_value(res2,"vpYF",0.87)
  info_para = get_res_value(res2,"cnInfoLabelParallelPosF",0.)
  info_just = get_res_value(res2,"cnInfoLabelJust","TopLeft")
  refanno_on= get_res_value_keep(res2,"vcRefAnnoOn",True)

  if(refanno_on)
    lbar_zone = get_res_value(res2,"pmLabelBarZone",4)
    lbar_orth = get_res_value(res2,"pmLabelBarOrthogonalPosF",0.05)
  else
    lbar_zone = get_res_value(res2,"pmLabelBarZone",3)
    lbar_orth = get_res_value(res2,"pmLabelBarOrthogonalPosF",0.0)
  end if

  cnres = get_res_ne(res2,(/"vc","vf","pm"/))
  vcres = get_res_eq(res2,(/"vc","vf","tr"/))

;
; Set some contour resources.
;
  cnres = True
  cnres@gsnDraw                  = False
  cnres@gsnFrame                 = False
  cnres@vpYF                     = ypos
  cnres@cnInfoLabelParallelPosF  = info_para  ; Change locations of info
  cnres@cnInfoLabelJust          = info_just  ; label and label bar so 
  cnres@pmLabelBarZone           = lbar_zone  ; they don't run into vector
  cnres@pmLabelBarOrthogonalPosF = lbar_orth  ; reference anno.
;
; Create contour pressure/height plot.
;
  contour = gsn_csm_pres_hgt(wks,data,cnres)

;
; Set some vector resources.
;
  vcres = True
  vcres@vcRefAnnoZone   = anno_zone  ; change zones so ref anno and
                                     ; labelbar don't run into each other
  vcres@gsnDraw         = False
  vcres@gsnFrame        = False
  vcres@gsnRightString  = ""  ; Use gsnRightString and gsnLeftString
  vcres@gsnLeftString   = ""  ; from contour plot.
;
; Create vector plot. We have to check the pressure values.  If they
; are not in descending order, then reverse 'em.
;
  if(is_valid_ycoord(u))
    npres = dimsizes(u&$u!0$)
    if(u&$u!0$(0).lt.u&$u!0$(npres-1))
      vector = gsn_csm_vector(wks,u(::-1,:),v(::-1,:),vcres)
    else
      vector = gsn_csm_vector(wks,u,v,vcres)
    end if
  else
    vector = gsn_csm_vector(wks,u,v,vcres)
  end if

;
; Overlay the vectors on the contour plot.
; 
  overlay(contour,vector)

; Draw all this stuff: vector over contour plot.

  draw_and_frame(wks,contour,calldraw,callframe,maxbb)

; Return contour/vector plot with data objects as attributes.

  contour@sfdata = contour@data
  contour@vcdata = vector@data
  return(contour)
end

;***********************************************************************;
; Function : gsn_csm_streamline                                         ;
;                     wks: workstation object                           ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a titled streamline plot to the       ;
; workstation  "wks" (the variable returned from a previous call to     ;
; "gsn_open_wks"). "resources" is an optional list of  resources. The   ;
; Id of the streamline plot is returned.                                ;
;                                                                       ;
; This function behaves differently from gsn_streamline in that it will ;
; add additional titles to the top of the plot if any of the special    ;
; GSUN resources "gsnLeftString," "gsnCenterString," and/or             ;
; "gsnRightString" are set, They are used to title the top left, center,;
; and right of the plot (in addition, the regular resource              ;
; "tiMainString" can be set to create a regular title).                 ;
;                                                                       ;
; Tick marks will be made to point outwards.                            ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_streamline(wks:graphic,u[*][*]:numeric,v[*][*],\
                            resources:logical)
local res, stream_object, res2, xfontf, yfontf, font_height, \
calldraw, callframe, left_string, center_string, right_string, \
main_zone, unew, vnew
begin

; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = get_resources(resources)

; This section tests for more special resources: those that start
; with "gsn."

; The default is to not add a cyclic point.

    if(get_res_value(res2,"gsnAddCyclic",False))
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if
;
; Check if frame and/or draw are not supposed to be called.
;  
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,res2)
    set_left_subtitle(unew,res2,res2)

; Check for existence of the left, center, and right subtitles.

    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)

    if(left_string.or.center_string.or.right_string)
      main_zone   = main_zone+1
    end if

    if(check_attr(res2,"gsnShape",True,False))
      main_zone     = main_zone+1 ; Zone for main title
    end if

; Use coordinate variables for X and/or Y if they exist.

    check_for_coord_arrays(unew,res2,"vector")

    res2          = True
    res2@gsnDraw  = False   ; Internally, don't draw plot or advance
    res2@gsnFrame = False   ; frame since we take care of that later.
    res2@gsnScale = True    ; force labels and ticks to be same.

    stres = get_res_ne(res2,(/"tx"/))
    stream_object = gsn_streamline(wks,unew,vnew,stres)

; Get some information from streamline plot that was created.

    streamline_plot = check_class_name(stream_object,"streamline")

; Add lat/lon labels to X/Y axes if appropriate coordinate arrays exist.

    add_latlon_labels(streamline_plot,unew,res2)

; Get some information from streamline plot that was created.

    getvalues streamline_plot
      "vpWidthF"             : width
      "vpHeightF"            : height
  
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
  
      "tmYLMajorLengthF"     : ylength
      "tmXBMajorLengthF"     : xlength
      "tmYLMinorLengthF"     : ymlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues

    font_height = min((/xfontf,yfontf/))  ; Make subtitle label sizes a
                                          ; function of the size of the
                                          ; X/Y axis labels.

    major_length = min((/ylength,xlength/))    ; New length for major ticks.
    minor_length = min((/ymlength,xmlength/))  ; New length for minor ticks.

; If the plot is close to square in size, then make the 
; three top titles and the tick marks smaller.

    font_scale = (/1.0,0.8,0.8/)
    ratios     = (/0.5,0.75,1.0/)

    ratio = height/width
    if(ratio.gt.1) 
      ratio = 1./ratio
    end if
    index = ind(ratio.le.ratios)

    scale = font_scale(index(0))
    font_height  = scale * font_height
;
; Make tick marks same length and point outward.
;
    major_length = scale * major_length
    minor_length = scale * minor_length

    tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
    gsnp_point_tickmarks_outward(streamline_plot,tmres,xlength,ylength,\
                                               xmlength,ymlength,\
                                               major_length,minor_length)

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"am","tx"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,stream_object,left_string,center_string,\
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

