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
;;       load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
;;       load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;; 
;;               at the top of your NCL script, before the begin statement.
;;

; Global attribute for whether to use old-style (pre V6.4.0) labebar. 
; We are still in the process of making a decision whether to make this
; the default for V6.4.0
GSN_OLD_LABELBAR = True

;***********************************************************************;
; Function : xy_ref_interp                                              ;
;                  x1: numeric                                          ;
;                  x2: numeric                                          ;
;                  y1: numeric                                          ;
;                  y2: numeric                                          ;
;            reftype[1]:string                                          ;
;                                                                       ;
; Given two points, (x1,y1) and (x2,y1) and a reference value (ref),    ;
; this function interpolates where on the X or Y axis the line from the ;
; 1st point to the 2nd point crosses the line y=ref or x=ref.           ;
;***********************************************************************;
undef("xy_ref_interp")
function xy_ref_interp(x1:numeric,x2:numeric,y1:numeric,y2:numeric, \
                       ref:numeric,reftype[1]:string)
begin
 if(reftype.eq."y") then
   return(x2-((y2-ref)*(x2-x1))/(y2-y1))
 else
   return(y2-((x2-ref)*(y2-y1))/(x2-x1))
 end if
end

;***********************************************************************;
; Function : ref_line_interp                                            ;
;                  x: numeric                                           ;
;                  y: numeric                                           ;
;           ref_line: numeric                                           ;
;           reftype[1]: string                                          ;
;                                                                       ;
; Given a two arrays X and Y and a set of reference point values,       ;
; return new X and Y arrays that contain extra interpolated points where;
; values where Y (or X) crosses the reference point values.             ;
;                                                                       ;
; This function was updated in NCL V6.4.0 to allow different            ;
; dimensionalities for x, y, and ref_line. The different cases that     ;
; it can handle are:
;                                                                       ;
;  - X and Y are 1D arrays of length npts, and ref_line is a scalar or  ;
;    a 1D array of values                                               ;
;  - X and Y are 2D arrays of length ncurves x npts, and ref_line is    ;
;    a scalar or an array of length ncurves                             ;
;  - One of X or Y is a 1D array of length npts, and the other is       ;
;    length ncurves x npts, and ref_line is a scalar or an array of     ;
;    length ncurves                                                     ;
;  - X and Y are 1D arrays of length npts, and ref_line is a scalar or  ;
;    a 1D array of values                                               ;
;                                                                       ;
; The dimension size of each interpolated set of points is also         ;
; returned.                                                             ;
;***********************************************************************;
undef("ref_line_interp")
function ref_line_interp(x:numeric,y:numeric,ref_lines[*]:numeric,\
                         reftype[1]:string)
local dimsx, dimsy, rankx, ranky, nlines, ncurvesxy, ncurves, \
nref_lines, nlines, i, j, location, xnew, ynew, npts,x1d,y1d
begin
;---Lots of error checking here
  dimsx  = dimsizes(x)
  dimsy  = dimsizes(y)
  rankx  = dimsizes(dimsx)
  ranky  = dimsizes(dimsy)
  nlines = dimsizes(ref_lines)

  if(any((/rankx,ranky/).gt.2)) then
    print("ref_line_interp: error: X and Y must either be 1D or 2D arrays.")
    exit
  end if
;---Check X and Y array sizes
  if(rankx.eq.2) then
    ncurvesxy = dimsx(0)
    npts    = dimsx(1)
    if(ranky.eq.1) then                                   ; Y is 1D, X is 2D
      if(dimsy(0).ne.npts) then 
        print("ref_line_interp: error: X and Y must have the same rightmost dimension.")
        exit
      end if        
    else
      if(dimsy(0).ne.ncurvesxy.or.dimsy(1).ne.npts) then    ; Y is 2D, X is 2D
        print("ref_line_interp: error: If X and Y are 2D, they must the same dimensionality")
        exit
      end if        
    end if
  else
    if(ranky.eq.2) then                                   ; Y is 2D, X is 1D
      ncurvesxy = dimsy(0)
      npts    = dimsy(1)
      if(dimsx(0).ne.npts) then 
        print("ref_line_interp: error: X and Y must have the same rightmost dimension.")
        exit
      end if        
      y2d = y
    else                                                  ; Y is 1D, X is 1D
      ncurvesxy = 1
      npts    = dimsy(0)
    end if
  end if

;---Error checking on ref_lines 
  if(.not.(nlines.eq.1.or.nlines.eq.ncurvesxy.or.\
          (nlines.gt.1.and.ncurvesxy.eq.1))) then
    print("ref_line_interp: error: gsnYRefLine must be a scalar or an array of length ncurves")
    exit
  end if        
;
; Special case where X/Y are 1D, but we have multiple reference lines.
; We will simply propagate X and Y to have nlines copies.
;
  if(nlines.gt.1.and.ncurvesxy.eq.1)
    ncurves = nlines
  else
    ncurves = ncurvesxy
  end if

;---Propagate X and Y to 2D if necessary.
  if(rankx.eq.1) then
    x2d = conform_dims((/ncurves,npts/),x,1)
  else if(rankx.eq.2.and.nlines.ne.ncurvesxy) then    
    x2d = reshape(x,(/ncurves,npts/))
  else
    x2d = x
  end if
  end if

  if(ranky.eq.1) then
    y2d = conform_dims((/ncurves,npts/),y,1)
  else if(ranky.eq.2.and.nlines.ne.ncurvesxy) then    
    y2d = reshape(y,(/ncurves,npts/))
  else
    y2d = y
  end if
  end if

;---Propagate ref_lines to 1D array if necessary
  if(nlines.gt.1) then
    ref1d = ref_lines
  else
    ref1d = conform_dims(ncurves,ref_lines,-1)
  end if

;
; Create new X and Y arrays to hold new data values.  Wherever the curve
; crosses the reference line, an interpolation needs to be done to create 
; a point *on* that reference line.
;
  xinterp = new((/ncurves,2*npts/),double,1e36)
  yinterp = new((/ncurves,2*npts/),double,1e36)

  copy_var_atts(x,xinterp,"")
  copy_var_atts(y,yinterp,"")

  xinterp(:,0) = x2d(:,0)
  yinterp(:,0) = y2d(:,0)
  num_interp_pts= new(ncurves,integer)
;
; Loop through the rest of the points and find out where the curve crosses
; the reference line.  If it does, then do an interpolation.
; Store the location of the previous point (-1 if it is less than the
; reference line, 0 if it is on the reference line, and 1 if it is greater).
;
  do j=0, ncurves-1
    num_crosses = 0
    do i=1, npts-1
      xinterp(j,i+num_crosses) = x2d(j,i)
      yinterp(j,i+num_crosses) = y2d(j,i)
      if(.not.ismissing(y2d(j,i)).and..not.ismissing(x2d(j,i)).and.\
         .not.ismissing(y2d(j,i-1)).and..not.ismissing(x2d(j,i-1)))
        if((reftype.eq."y").and.\
           ((y2d(j,i-1).lt.ref1d(j).and.y2d(j,i).gt.ref1d(j)).or.\
            (y2d(j,i-1).gt.ref1d(j).and.y2d(j,i).lt.ref1d(j)))) then
          xinterp(j,i+num_crosses) = xy_ref_interp(x2d(j,i-1),x2d(j,i),\
                                                   y2d(j,i-1),y2d(j,i),\
                                                   ref1d(j),reftype)
          xinterp(j,i+num_crosses+1) = x2d(j,i)
          yinterp(j,i+num_crosses)   = ref1d(j)
          yinterp(j,i+num_crosses+1) = y2d(j,i)
          num_crosses = num_crosses + 1
        else if((reftype.eq."x").and.\
                ((x2d(j,i-1).lt.ref1d(j).and.x2d(j,i).gt.ref1d(j)).or.\
                 (x2d(j,i-1).gt.ref1d(j).and.x2d(j,i).lt.ref1d(j)))) then
          yinterp(j,i+num_crosses) = xy_ref_interp(x2d(j,i-1),x2d(j,i),\
                                                   y2d(j,i-1),y2d(j,i),\
                                                   ref1d(j),reftype)
          yinterp(j,i+num_crosses+1) = y2d(j,i)
          xinterp(j,i+num_crosses)   = ref1d(j)
          xinterp(j,i+num_crosses+1) = x2d(j,i)
          num_crosses = num_crosses + 1
        end if
        end if
      end if
    end do
    num_interp_pts(j) = npts + num_crosses
  end do
  delete([/x2d,y2d/])
  return([/xinterp,yinterp,ref1d,num_interp_pts/])
end

;***********************************************************************;
; Function : get_bar_widths                                             ;
;                  x: numeric                                           ;
;                  y: numeric                                           ;
;          type_ref[1]:string                                           ;
;               res2: logical                                           ;
;                                                                       ;
; Given a set of x points, return a bar width for filled bars later.    ;
;                                                                       ;
;***********************************************************************;
undef("get_bar_widths")
function get_bar_widths(x,y,type_ref[1]:string,res2)
local dims, rank, ncurves, npts, bar_widths, delta, nwidths
begin
;---Get number of curves and points.
  if(type_ref.eq."y")
    dims  = dimsizes(y)
    rank  = dimsizes(dims)
    drank = dimsizes(dimsizes(x))
  else
    dims  = dimsizes(x)
    rank  = dimsizes(dims)
    drank = dimsizes(dimsizes(y))
  end if
  if(rank.eq.1)
    ncurves = 1
    npts    = dims(0)
  else
    ncurves = dims(0)
    npts    = dims(1)
  end if
;---If bar width is set explicitly, we have to check it first.
  if(isatt(res2,"gsnXYBarChartBarWidth"))
    bar_width = get_res_value_keep(res2,"gsnXYBarChartBarWidth",0.)
    nwidths   = dimsizes(dimsizes(bar_width))
    if(.not.(nwidths.eq.1.or.nwidths.eq.ncurves))
      print("get_bar_widths: Fatal: You must either select one constant bar width, or the same number of bar widths as you have curves.")
      return
    end if
  end if

;---Get the distances between x or y values.
  if(drank.eq.1)
;---Special case if we only have one bar.
    if(npts.eq.1)
      if(isatt(res2,"gsnXYBarChartBarWidth")) then
        b = get_res_value(res2,"gsnXYBarChartBarWidth",0.)
        return(b)
      else
        print("get_bar_widths: Warning: You only have one element,")
        print("and gsnXYBarChartBarWidth is not set.")
        if(type_ref.eq."y") then
          print("Will default to a bar width equal to x")
          return(x)
        else
          print("Will default to a bar width equal to y")
          return(y)
        end if
      end if
    else
      if(type_ref.eq."y") then
        delta = min( x(1:npts-1) - x(0:npts-2) )
      else
        delta = min( y(1:npts-1) - y(0:npts-2) )
      end if
    end if
  else
    if(type_ref.eq."y") then
      x!0 = "ncurves"
      x!1 = "npts"
    else
      y!0 = "ncurves"
      y!1 = "npts"
    end if
;---Special case if we only have one bar.
    if(npts.eq.1) then
      if(isatt(res2,"gsnXYBarChartBarWidth")) then
        b = get_res_value(res2,"gsnXYBarChartBarWidth",0.)
        return(b)
      else
        print("get_bar_widths: Warning: You only have one element in each set of curves,")
        print("and gsnXYBarChartBarWidth is not set.")
        if(type_ref.eq."y") then
          print("Will default to bar widths equal to x")
          b = conform_dims(ncurves,x,-1)
        else
          print("Will default to bar widths equal to y")
          b = conform_dims(ncurves,y,-1)
        end if
        return(b)
      end if
    end if
;---Get the minimum delta for each set of curves.
    if(type_ref.eq."y") then
      delta = dim_min(x(:,1:npts-1)-x(:,0:npts-2))
      delete(x!0)
      delete(x!1)
    else
      delta = dim_min(y(:,1:npts-1)-y(:,0:npts-2))
      delete(y!0)
      delete(y!1)
    end if
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
    if(any(bar_width.gt.delta))
      print("get_bar_widths: Warning: The bar width(s) you selected ("+bar_width+") is larger than the smallest delta ("+delta+").")
      print("Defaulting to " + delta + ".")
      bar_widths = conform_dims(ncurves,delta,-1)
    else
;---Make sure bar_widths is 1D array of length (/ncurves/).
      bar_widths = conform_dims(ncurves,bar_width,-1)
    end if
  else
    bar_widths = conform_dims(ncurves,delta,-1)
  end if

  return(bar_widths)
end


;***********************************************************************;
; Function : outlined_bars                                              ;
;                  x: numeric                                           ;
;                  y: numeric                                           ;
;                                                                       ;
; Given a set of points represented by x and y, return new arrays       ;
; that represent bar outlines.                                          ;
;***********************************************************************;
undef("outlined_bars")
function outlined_bars(x,y)
local nyb, npts, ncurves, ndimsx, ndimsy, ncurves, nyb, \
bar_width, xnew, ynew
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
; the horizontal line.
;
  nyb = 2 * (npts-1) + 1

  yinterp = new((/ncurves,nyb/),double,1e36)
  xinterp = new((/ncurves,nyb/),double,1e36)

  copy_var_atts(x,xinterp,"")
  copy_var_atts(y,yinterp,"")
;
; For each old y point, we need two new y points to represent
; the horizontal line. The new X values will be the width of the bars.
;
  yinterp(:,0:nyb-2:2) = ynew(:,:npts-2)
  yinterp(:,1:nyb-1:2) = ynew(:,:npts-2)
  yinterp(:,nyb-1)     = ynew(:,npts-1)   ; last point

  bar_widths           = xnew(:,1:) - xnew(:,:npts-2)
  xinterp(:,0:nyb-2:2) = xnew(:,:npts-2)
  xinterp(:,1:nyb-2:2) = xnew(:,:npts-2) + bar_widths
  xinterp(:,nyb-1)     = xnew(:,npts-1)     ; last point, no bar

  return([/xinterp,yinterp/])
end

;**********************************************************************
; Function to return the arrays needed for filling in bars for a
; bar chart plot (rather than an XY plot).
;
; Note: there are several if-else statements to check whether this
; is a vertical (type_ref="y") or horizontal (type_ref="x") plot.
; Instead of doing if-else statements, since the "else" parts are
; identical with the xb and yb arrays being swapped, we could have
; made temporary copies of the xb and yb arrays and swapped them for
; the type_ref="x" case. However, this could cause a memory hit if 
; the arrays are large. So, keeping the if-else statements for now.
;**********************************************************************
undef("get_bar_arrays")
function get_bar_arrays(xb,yb,type_ref[1]:string,bres)
local ybarmsg, npts, ndimsx, ndimsy, ncurves
begin
  ndimsy = dimsizes(dimsizes(yb))
  ndimsx = dimsizes(dimsizes(xb))

  if(type_ref.eq."y")
    if(ndimsy.eq.1)
      ncurves = 1
      npts    = dimsizes(yb)
    else
      ncurves = dimsizes(yb(:,0))
      npts    = dimsizes(yb(0,:))
    end if
  else
    if(ndimsx.eq.1)
      ncurves = 1
      npts    = dimsizes(xb)
    else
      ncurves = dimsizes(xb(:,0))
      npts    = dimsizes(xb(0,:))
    end if
  end if

  bw = get_bar_widths(xb,yb,type_ref,bres)
  if(any(ismissing(bw)).or.any(bw.le.0))
    print("get_bar_arrays: Fatal: The array is not monotonically increasing.")
    print("Cannot draw a bar chart.")
    return
  end if
  if(npts.eq.1) then
;
; Special test for just one point. If just one point, add some
; dummy missing vals before and after.
;
    if(type_ref.eq."y")
      ybarmsg = get_res_value_keep(yb,"_FillValue",1e20)
      xbar = (/xb-bw,xb,xb+bw/)
      ybar = (/ybarmsg,yb,ybarmsg/)
      nptsbar = 3
      ybar@_FillValue = ybarmsg
    else
      xbarmsg = get_res_value_keep(xb,"_FillValue",1e20)
      ybar = (/yb-bw,yb,yb+bw/)
      xbar = (/xbarmsg,xb,xbarmsg/)
      nptsbar = 3
      xbar@_FillValue = xbarmsg
    end if
  else
    xbar = xb
    ybar = yb
    nptsbar = npts
  end if
  return([/xbar,ybar,bw,nptsbar/])
end

;***********************************************************************;
; function : filled_bars_interp                                         ;
;                  x: numeric                                           ;
;                  y: numeric                                           ;
;        type_ref[1]:string                                             ;
;           ref_line: numeric                                           ;
;                                                                       ;
; Given a set of points represented by x and y, and reference values,   ;
; return the y values with bars greater, less, and equal to the         ;
; reference lines.                                                      ;
;***********************************************************************;
undef("filled_bars_interp")
function filled_bars_interp(x,y,type_ref[1]:string,ref_line,res2)
local ndimsx, ndimsy, dims, ncurves, npts, xnew, ynew, \
gt_ind,ls_ind,eq_ind,bar_array_list,xb,yb,bar_widths
begin
;---Get arrays needed to fill in locations for bars later.
    bar_array_list = get_bar_arrays(x,y,type_ref,res2)
    xb = bar_array_list[0]
    yb = bar_array_list[1]
    bar_widths = bar_array_list[2]
    nptsbar = bar_array_list[3]

    if(all(ismissing(ref_line))) then
      if(type_ref.eq."y") then
        ref_line = get_res_value_keep(res2,"trYMinF",min(y))
      else
        ref_line = get_res_value_keep(res2,"trXMinF",min(x))
      end if
    end if
;
; Convert x and y into two dimensional arrays so we don't have to
; test for whether we have one line or multiple lines.
;
  ndimsx = dimsizes(dimsizes(xb))
  ndimsy = dimsizes(dimsizes(yb))
  
  if(type_ref.eq."y") then
    if(ndimsy.eq.1)
      ncurves = 1
      npts    = dimsizes(yb)
      ynew    = onedtond(todouble(yb),(/ncurves,npts/))
    else
      ncurves = dimsizes(yb(:,0))
      npts    = dimsizes(yb(0,:))
      ynew    = todouble(yb)
    end if
    if(ndimsx.eq.1)
      xnew = onedtond(todouble(xb),(/ncurves,npts/))
    else
      xnew = todouble(xb)
    end if
  else
;
; Note: we are treating the "X" values here as "Y"
; values and vice versa, so we don't have to duplicate
; the code below when you check if values are > or <
; ref line value.
;
; When we're all done, we'll need to switch the arrays
; back.
;
    if(ndimsx.eq.1)
      ncurves = 1
      npts    = dimsizes(xb)
      ynew    = onedtond(todouble(xb),(/ncurves,npts/))
    else
      ncurves = dimsizes(xb(:,0))
      npts    = dimsizes(xb(0,:))
      ynew    = todouble(xb)
    end if
    if(ndimsy.eq.1)
      xnew = onedtond(todouble(yb),(/ncurves,npts/))
    else
      xnew = todouble(yb)
    end if
  end if

;---Create arrays to hold points greater than, less than, and equal to reference lines.
  xgrtr = new((/ncurves,5*nptsbar/),double,1e36)
  ygrtr = new((/ncurves,5*nptsbar/),double,1e36)
  xless = new((/ncurves,5*nptsbar/),double,1e36)
  yless = new((/ncurves,5*nptsbar/),double,1e36)
  xequal = new((/ncurves,5*nptsbar/),double,1e36)
  yequal = new((/ncurves,5*nptsbar/),double,1e36)
;
; Arrays to hold number of points in each set of greater/less points,
; and the index values where points fall greater, less, and equal
; to the Y ref value.
;
  ngrtr    = new(ncurves,integer)
  nless    = new(ncurves,integer)
  nequal   = new(ncurves,integer)
  indgrtr  = new((/ncurves,nptsbar/),integer)
  indless  = new((/ncurves,nptsbar/),integer)
  indequal = new((/ncurves,nptsbar/),integer)

;--Make ref_line a 1D double array of length ncurves.
  ref_lines = conform_dims(ncurves,todouble(ref_line),-1)
;
; Loop across curves and figure out which points are greater than,
; less than, and equal to the references lines.
;
  do i=0,ncurves-1
;---Get the indices where the y curve is greater, less, equal to the ref line.
    gt_ind := ind(ynew(i,:).gt.ref_lines(i))
    ls_ind := ind(ynew(i,:).lt.ref_lines(i))
    eq_ind := ind(ynew(i,:).eq.ref_lines(i))
;
; Create new array to hold bar charts.  For each old y point, we need
; five new y points to represent the closed rectangle.
;
    if(.not.any(ismissing(gt_ind)))
      ngrtr(i) = 5*dimsizes(gt_ind)                ; # values > ref line
      indgrtr(i,0:dimsizes(gt_ind)-1) = gt_ind     ; indexes > ref line
      ygrtr(i,0:ngrtr(i)-5:5)  = ref_lines(i)
      ygrtr(i,1:ngrtr(i)-4:5)  = ynew(i,gt_ind)
      ygrtr(i,2:ngrtr(i)-3:5)  = ynew(i,gt_ind)
      ygrtr(i,3:ngrtr(i)-2:5)  = ref_lines(i)
      ygrtr(i,4:ngrtr(i)-1:5)  = ref_lines(i)
  
      xgrtr(i,0:ngrtr(i)-5:5) = xnew(i,gt_ind) - bar_widths(i)/2.
      xgrtr(i,1:ngrtr(i)-4:5) = xnew(i,gt_ind) - bar_widths(i)/2.
      xgrtr(i,2:ngrtr(i)-3:5) = xnew(i,gt_ind) + bar_widths(i)/2.
      xgrtr(i,3:ngrtr(i)-2:5) = xnew(i,gt_ind) + bar_widths(i)/2.
      xgrtr(i,4:ngrtr(i)-1:5) = xnew(i,gt_ind) - bar_widths(i)/2.
    else
      ngrtr(i) = 0
    end if  
    if(.not.any(ismissing(ls_ind))) 
      nless(i) = 5*dimsizes(ls_ind)                ; # values < ref line
      indless(i,0:dimsizes(ls_ind)-1) = ls_ind     ; indexes < ref line
      yless(i,0:nless(i)-5:5) = ref_lines(i)
      yless(i,1:nless(i)-4:5) = ynew(i,ls_ind)
      yless(i,2:nless(i)-3:5) = ynew(i,ls_ind)
      yless(i,3:nless(i)-2:5) = ref_lines(i)
      yless(i,4:nless(i)-1:5) = ref_lines(i)
  
      xless(i,0:nless(i)-5:5) = xnew(i,ls_ind) - bar_widths(i)/2.
      xless(i,1:nless(i)-4:5) = xnew(i,ls_ind) - bar_widths(i)/2.
      xless(i,2:nless(i)-3:5) = xnew(i,ls_ind) + bar_widths(i)/2.
      xless(i,3:nless(i)-2:5) = xnew(i,ls_ind) + bar_widths(i)/2.
      xless(i,4:nless(i)-1:5) = xnew(i,ls_ind) - bar_widths(i)/2.
    else
      nless(i) = 0
    end if  
    if(.not.any(ismissing(eq_ind))) 
      nequal(i) = 5*dimsizes(eq_ind)              ; # values equal ref line
      indequal(i,0:dimsizes(eq_ind)-1) = eq_ind     ; indexes equal ref line
      yequal(i,0:nequal(i)-1:5) = ref_lines(i)
      xequal(i,0:nequal(i)-5:5) = xnew(i,eq_ind) - bar_widths(i)/2.
      xequal(i,1:nequal(i)-4:5) = xnew(i,eq_ind) - bar_widths(i)/2.
      xequal(i,2:nequal(i)-3:5) = xnew(i,eq_ind) + bar_widths(i)/2.
      xequal(i,3:nequal(i)-2:5) = xnew(i,eq_ind) + bar_widths(i)/2.
      xequal(i,4:nequal(i)-1:5) = xnew(i,eq_ind) - bar_widths(i)/2.
    else
      nequal(i) = 0
    end if  
  end do

  delete([/xnew,ynew/])

;
; Combine the greater/less/equal arrays into one X/Y array so that we can
; pass to gsn_xy routine. Make the 5th point missing, since the 5th point
; closes the polygon, and we only want an outline here.
;
  ng_max   = max(ngrtr)
  nl_max   = max(nless)
  ne_max   = max(nequal)
  ngle_max = ng_max + nl_max + ne_max

  if(ng_max.le.0.and.nl_max.le.0) then
    print("filled_bars_interp: Fatal: There are no values less than or greater than the chosen reference line.")
    print("Cannot draw a bar chart.")
    xinterp = x
    yinterp = y
  else
    ncurves3 = 3*ncurves
    xinterp  = new((/ncurves3,ngle_max/),double,xgrtr@_FillValue)
    yinterp  = new((/ncurves3,ngle_max/),double,ygrtr@_FillValue)

    if(type_ref.eq."y") then
      copy_var_atts(xb,xinterp,"")
      copy_var_atts(yb,yinterp,"")
    else
      copy_var_atts(xb,yinterp,"")    ; remember we have to swap them if dealing
      copy_var_atts(yb,yinterp,"")    ; with the x reference line case
    end if
    if(ng_max.gt.0)
      xinterp(0:ncurves3-1:3,0:ng_max-1)   = xgrtr(:,0:ng_max-1)
      yinterp(0:ncurves3-1:3,0:ng_max-1)   = ygrtr(:,0:ng_max-1)
      yinterp(0:ncurves3-1:3,4:ng_max-1:5) = yinterp@_FillValue
    end if
    if(nl_max.gt.0)
      xinterp(1:ncurves3-1:3,0:nl_max-1)   = xless(:,0:nl_max-1)
      yinterp(1:ncurves3-1:3,0:nl_max-1)   = yless(:,0:nl_max-1)
      yinterp(1:ncurves3-1:3,4:nl_max-1:5) = yinterp@_FillValue
    end if
    if(ne_max.gt.0)
      xinterp(2:ncurves3-1:3,0:ne_max-1)   = xequal(:,0:ne_max-1)
      yinterp(2:ncurves3-1:3,0:ne_max-1)   = yequal(:,0:ne_max-1)
      yinterp(2:ncurves3-1:3,4:ne_max-1:5) = yinterp@_FillValue
    end if
  end if
;
; Here's where we switch the X and Y values back
; for the type_ref="x" case.
;
  if(type_ref.eq."y") then
    return([/xinterp,yinterp,xgrtr,ygrtr,xless,yless,ngrtr,nless,\
             indgrtr,indless/])  
  else
    return([/yinterp,xinterp,ygrtr,xgrtr,yless,xless,ngrtr,nless,\
             indgrtr,indless/])  
  end if
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
undef("find_cross_xy")
function find_cross_xy(x1:numeric,x2:numeric,y11:numeric,y12:numeric, \
                       y21:numeric,y22:numeric)
begin
  if((x2-x1).eq.0.or.(y12-y11+y21-y22).eq.0)
    x0 = new(1,double)
    y0 = new(1,double)
  else
    x0 = (x2*y21 - x2*y11 + x1*y12 - x1*y22)/(y12-y11+y21-y22)
    y0 = (y11*y22 - y12*y21) / (y11-y12+y22-y21)
;    y0 = (x0*(y22-y21)-x1*y22+x2*y21)/(x2-x1)
  end if
  return((/x0,y0/))
end


;***********************************************************************;
; Function : get_valid_long_names()                                     ;
;                                                                       ;
; This function returns the list of valid names that can be used in     ;
; place of "long_name". Note that these names are in the order that     ;
; they should be used. I.e. if both "long_name" and "Description" are   ;
; present, then long_name takes precedence over "Description".          ;
;                                                                       ;
;***********************************************************************;
undef("get_valid_long_names")
function get_valid_long_names()
begin
  return((/"long_name","standard_name","description","DESCRIPTION", \
           "Description","DataFieldName","hdf_name","hdfeos_name"/))
end

;***********************************************************************;
; Function : get_valid_units()                                          ;
;                                                                       ;
; This function returns the list of valid names that can be used for    ;
; "units".                                                              ;
;                                                                       ;
;***********************************************************************;
undef("get_valid_units")
function get_valid_units()
begin
  return((/"units","Units","UNITS","unit","Unit","UNIT"/))
end

;***********************************************************************;
; Function : get_valid_long_name_value()                                ;
;                                                                       ;
; This function returns the attribute and the attribute value of the    ;
; valid long_name, if it exists, and missing otherwise.                 ;
;***********************************************************************;
undef("get_valid_long_name_value")
function get_valid_long_name_value(data)
local data_atts, n, found_valid_name, data_atts
begin
  data_atts        = getvaratts(data)
  found_valid_name = False
  
  if(.not.any(ismissing(data_atts))) then
    valid_long_names = get_valid_long_names()
    n = 0
    do while(n.lt.dimsizes(valid_long_names).and..not.found_valid_name)
      if (any(valid_long_names(n).eq.data_atts)) then
;
; Return both the attribute name and its value.
;
        return((/valid_long_names(n),data@$valid_long_names(n)$/))
      end if
      n = n + 1
    end do
  end if
  
  return(new(1,string))
end

;***********************************************************************;
; Function : get_valid_units_value()                                    ;
;                                                                       ;
; This function returns the attribute and the attribute value of the    ;
; valid units, if it exists, and missing otherwise.                     ;
;***********************************************************************;
undef("get_valid_units_value")
function get_valid_units_value(data)
local data_atts, n, found_valid_name, data_atts
begin
  data_atts        = getvaratts(data)
  found_valid_name = False
  
  if(.not.any(ismissing(data_atts))) then
    valid_units = get_valid_units()
    n = 0
    do while(n.lt.dimsizes(valid_units).and..not.found_valid_name)
      if (any(valid_units(n).eq.data_atts)) then
;
; Return both the attribute name and its value.
;
        return((/valid_units(n),data@$valid_units(n)$/))
      end if
      n = n + 1
    end do
  end if
  
  return(new(1,string))
end

;***********************************************************************;
; Function : get_csm_long_name_units_string                             ;
;            data : numeric                                             ;
;                                                                       ;
; This function checks if a valid long_name and units attributes exist, ;
; and if so, constructs a string using them. A missing value is returned;
; otherwise.                                                            ;
;***********************************************************************;
undef("get_csm_long_name_units_string")
function get_csm_long_name_units_string(data)
local lname, uname
begin
  lu_string = new(1,string)
  lname = get_valid_long_name_value(data)
  uname = get_valid_units_value(data)

  if(.not.any(ismissing(lname))) then
    lu_string = lname(1)
;
; Comment out this code for now, because I'm not sure I want
; the default behavior to change from just a long_name string
; to a long_name (units) string for now.  This was added around
; version a031 (Jan 2004).
;
;    if(.not.any(ismissing(uname)).and.uname(1).ne."") then
;      lu_string = lu_string + " (" + uname(1) + ")"
;    end if
  end if
  return(lu_string)
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
 
undef("check_for_subtitles")
procedure check_for_subtitles(res:logical,left:logical,center:logical,\
                              right:logical)
begin
; Initialize
  left   = False
  right  = False
  center = False

  if((res).and..not.any(ismissing(getvaratts(res)))) then
;
; Check for font setting.
;
    if(isatt(res,"gsnStringFont")) then
      left@gsnStringFont   = res@gsnStringFont
      center@gsnStringFont = res@gsnStringFont
      right@gsnStringFont  = res@gsnStringFont
      delete(res@gsnStringFont)
    end if
;
; Check for left string.
;
    if(isatt(res,"gsnLeftString")) then
      left            = True
      left@sub_string = res@gsnLeftString
;
; Check for ParallelPosF/OrthogonalPosF setting.
;
      if(isatt(res,"gsnLeftStringParallelPosF")) then
        left@gsnLeftStringParallelPosF = \
                    get_res_value(res,"gsnLeftStringParallelPosF",0.0)
      end if
      if(isatt(res,"gsnLeftStringOrthogonalPosF")) then
;
; 0.0 is just a dummy value. It will not be used.
;
        left@gsnLeftStringOrthogonalPosF = \
                     get_res_value(res,"gsnLeftStringOrthogonalPosF",0.0)
      end if
;
; Check for font heights.
;
      if(isatt(res,"gsnLeftStringFontHeightF")) then
        left@gsnLeftStringFontHeightF = get_res_value(res, \
                                        "gsnLeftStringFontHeightF",0.0)
      else
        if(isatt(res,"gsnStringFontHeightF")) then
          left@gsnLeftStringFontHeightF = get_res_value_keep(res, \
                                          "gsnStringFontHeightF",0.0)
        end if 
      end if 
;
; Check for function codes
;
      if(isatt(res,"gsnLeftStringFuncCode")) then
        left@gsnLeftStringFuncCode = get_res_value(res, \
                                        "gsnLeftStringFuncCode","~")
      else
        if(isatt(res,"gsnStringFuncCode")) then
          left@gsnLeftStringFuncCode = get_res_value_keep(res, \
                                          "gsnStringFuncCode","~")
        end if 
      end if 
;
; Check for font color
;
      if(isatt(res,"gsnLeftStringFontColor")) then
        left@gsnLeftStringFontColor = get_res_value(res, \
                                        "gsnLeftStringFontColor",1)
      else
        if(isatt(res,"gsnStringFontColor")) then
          left@gsnLeftStringFontColor = get_res_value_keep(res, \
                                          "gsnStringFontColor",1)
        end if 
      end if 
      delete(res@gsnLeftString)
    end if
;
; Check for center string.
;
    if(isatt(res,"gsnCenterString"))
      center            = True
      center@sub_string = res@gsnCenterString
;
; Check for ParallelPosF/OrthogonalPosF setting.
;
      if(isatt(res,"gsnCenterStringParallelPosF")) then
        center@gsnCenterStringParallelPosF =  \
                        get_res_value(res,"gsnCenterStringParallelPosF",0.5)
      end if

      if(isatt(res,"gsnCenterStringOrthogonalPosF")) then
;
; 0.0 is just a dummy value. It will not be used.
;
        center@gsnCenterStringOrthogonalPosF = \
                     get_res_value(res,"gsnCenterStringOrthogonalPosF",0.0)
      end if
;
; Check for font heights.
;
      if(isatt(res,"gsnCenterStringFontHeightF")) then
        center@gsnCenterStringFontHeightF = get_res_value(res, \
                                           "gsnCenterStringFontHeightF",0.0)
      else
        if(isatt(res,"gsnStringFontHeightF")) then
          center@gsnCenterStringFontHeightF = get_res_value_keep(res, \
                                             "gsnStringFontHeightF",0.0)
        end if 
      end if 
;
; Check for function codes
;
      if(isatt(res,"gsnCenterStringFuncCode")) then
        center@gsnCenterStringFuncCode = get_res_value(res, \
                                           "gsnCenterStringFuncCode","~")
      else
        if(isatt(res,"gsnStringFuncCode")) then
          center@gsnCenterStringFuncCode = get_res_value_keep(res, \
                                             "gsnStringFuncCode","~")
        end if 
      end if 
;
; Check for font color
;
      if(isatt(res,"gsnCenterStringFontColor")) then
        center@gsnCenterStringFontColor = get_res_value(res, \
                                           "gsnCenterStringFontColor",1)
      else
        if(isatt(res,"gsnStringFontColor")) then
          center@gsnCenterStringFontColor = get_res_value_keep(res, \
                                             "gsnStringFontColor",1)
        end if 
      end if 
      delete(res@gsnCenterString)
    end if
;
; Check for right string.
;
    if(isatt(res,"gsnRightString"))
      right            = True
      right@sub_string = res@gsnRightString
;
; Check for ParallelPosF/OrthogonalPosF setting.
;
      if(isatt(res,"gsnRightStringParallelPosF")) then
        right@gsnRightStringParallelPosF = \
                     get_res_value(res,"gsnRightStringParallelPosF",1.0)
      end if
      if(isatt(res,"gsnRightStringOrthogonalPosF")) then
;
; 0.0 is just a dummy value. It will not be used.
;
        right@gsnRightStringOrthogonalPosF = \
                     get_res_value(res,"gsnRightStringOrthogonalPosF",0.0)
      end if
;
; Check for font heights.
;
      if(isatt(res,"gsnRightStringFontHeightF")) then
        right@gsnRightStringFontHeightF = get_res_value(res, \
                                         "gsnRightStringFontHeightF",0.0)
      else
        if(isatt(res,"gsnStringFontHeightF")) then
          right@gsnRightStringFontHeightF = get_res_value_keep(res, \
                                           "gsnStringFontHeightF",0.0)
        end if 
      end if 
;
; Check for function codes.
;
      if(isatt(res,"gsnRightStringFuncCode")) then
        right@gsnRightStringFuncCode = get_res_value(res, \
                                         "gsnRightStringFuncCode","~")
      else
        if(isatt(res,"gsnStringFuncCode")) then
          right@gsnRightStringFuncCode = get_res_value_keep(res, \
                                           "gsnStringFuncCode","~")
        end if 
      end if 
;
; Check for font color
;
      if(isatt(res,"gsnRightStringFontColor")) then
        right@gsnRightStringFontColor = get_res_value(res, \
                                         "gsnRightStringFontColor",1)
      else
        if(isatt(res,"gsnStringFontColor")) then
          right@gsnRightStringFontColor = get_res_value_keep(res, \
                                           "gsnStringFontColor",1)
        end if 
      end if 
      delete(res@gsnRightString)
    end if
  end if
  delete_attr(res,"gsnStringFontHeightF")
  delete_attr(res,"gsnStringFuncCode")
  delete_attr(res,"gsnStringFontColor")
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
undef("set_right_subtitle")
procedure set_right_subtitle(data:numeric,res:logical,newres:logical)
begin
;
; If gsnRightString hasn't been set, then use the valid units 
; attribute if it exists.
;
  if(isatt(res,"gsnRightString"))
    newres = True
    newres@gsnRightString = res@gsnRightString
  else
    uname = get_valid_units_value(data)
    if(.not.any(ismissing(uname)))    
      newres = True
      newres@gsnRightString = uname(1)
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
; one of the valid "long_name" attributes, if they exist.               ;
;***********************************************************************;
undef("set_left_subtitle")
procedure set_left_subtitle(data:numeric,res:logical,newres:logical)
begin
;
; If gsnLeftString hasn't been set, then use one of several valid "long
; names" if they exist. Also have to delete the attribute used so it
; doesn't get used in the title by gsn_code.ncl. But, be careful here
; because you don't want to remove the attribute from the actual data. 
; Before calling this routine, you should be using a copy of your
; data.
;
  lname = get_valid_long_name_value(data)
  if(.not.any(ismissing(lname)))    
    newres = True
    newres@gsnLeftString = get_res_value_keep(res,"gsnLeftString",lname(1))
;
; Delete so it doesn't get used for main title
;
    delete(data@$lname(0)$)
  end if

;
; Of course, if gsnLeftString is set, use it. We had to do the test
; above first to make sure the attribute got removed, if it existed,
; so it doesn't get used again for another resource.
;
  if(isatt(res,"gsnLeftString"))
    newres = True
    newres@gsnLeftString = res@gsnLeftString
  end if
  return
end

;***********************************************************************;
; Function : is_valid_coord                                             ;
;                                                                       ;
; Checks if the X or Y coordinate variable exist and if it is           ;
; 1-dimensional.                                                       ; 
;                                                                       ;
;***********************************************************************;
undef("is_valid_coord")
function is_valid_coord(data:numeric,axis:string)
local dims
begin
  dims = -1
  if(axis.eq."x") then
    if(isdimnamed(data,1).and.iscoord(data,data!1))
      dims = dimsizes(dimsizes(data&$data!1$))
    end if
  else
    if(isdimnamed(data,0).and.iscoord(data,data!0))
      dims = dimsizes(dimsizes(data&$data!0$))
    end if
  end if
  if(dims.eq.1) then
    return(True)
  else
    return(False)
  end if
end

;***********************************************************************;
; Function : is_valid_latlon2d_attr                                     ;
;                                                                       ;
; Checks for a valid lon2d or lat2d attribute. This special attribute is;
; used by the gsn_csm* scripts for cases where the data is represented  ;
; by 2D lat/lon arrays, instead of 1D coordinate arrays.                ;
;                                                                       ;
; The lat2d/lon2d arrays can be one element larger in both dimensions.  ;
; This can mean that we are dealing with cell boundaries instead of cell;
; centers.                                                              ;
;***********************************************************************;
undef("is_valid_latlon2d_attr")
function is_valid_latlon2d_attr(data:numeric,lstring:string)
local ldims, data_dims
begin
  if(isatt(data,lstring)) then
    ldims     = dimsizes(data@$lstring$)
    data_dims = dimsizes(data)
    if(all(ldims.eq.data_dims).or.all(ldims.eq.(data_dims+1))) then
      return(True)
    else
      print("is_valid_latlon2d_attr: Warning: The '" + lstring + \
            "' attribute must either be")
       print("the same dimension sizes as the data, or one element larger in both directions.")
       print("Your data will most likely not be overlaid on the map correctly.")
    end if
  end if
  return(False)
end 


;***********************************************************************;
; Function : is_valid_latlon1d_attr                                     ;
;                                                                       ;
; Checks for a valid lon1d or lat1d attribute. This special attribute is;
; used by the gsn_csm* scripts for cases where the data is represented  ;
; by 1D lat/lon arrays of the same length as the 1D data array.         ;
;                                                                       ;
; This was added in NCL V6.4.0. I'm not sure I like it, so it may stay  ;
; unadvertised for a bit.                                               ;
;***********************************************************************;
undef("is_valid_latlon1d_attr")
function is_valid_latlon1d_attr(data:numeric,lstring:string)
local ldims, lrank, data_dims, data_rank
begin
  if(isatt(data,lstring)) then
    ldims     = dimsizes(data@$lstring$)
    data_dims = dimsizes(data)
    lrank     = dimsizes(ldims)
    data_rank = dimsizes(data_dims)
    if(lrank.eq.1.and.data_rank.eq.1.and.all(ldims.eq.data_dims)) then
      return(True)
    else
      print("is_valid_latlon1d_attr: Warning: The '" + lstring + \
            "' attribute must be 1D and same length as the data")
      print("Your data will most likely not be overlaid on the map correctly.")
    end if
  end if
  return(False)
end 

;***********************************************************************;
; Function : is_tm_obj_valid                                            ;
;                                                                       ;
;          pobj : plot object to retrieve trGridType from               ;
;                                                                       ;
; This function checks to see if the given plot object is using a       ;
; spherical or curvilinear transformation. If so, you can't have a      ;
; tickmark objects. This function used to additionally check for 2D     ;
; lat/lon coords, but Dave B and I decided this wasn't necessary.       ;
;                                                                       ;
;***********************************************************************;
undef("is_tm_obj_valid")
function is_tm_obj_valid(pobj)
local tr_grid_type
begin
; trGridType == 3 --> Curvilinear
; trGridType == 4 --> Spherical
  getvalues pobj
    "trGridType" : tr_grid_type
  end getvalues
  if(any(tr_grid_type.eq.get_enum_value((/"Curvilinear","Spherical"/)))) then
    return(False)
  else
    return(True)
  end if
end


;***********************************************************************;
; Procedure : set_axis_string                                           ;
;                 res: logical                                          ;
;                data: numeric                                          ;
;                axis: string                                           ;
;                                                                       ;
; This procedure sets the tiXAxisString or tiYAxisString resource       ;
; depending on current resource and/or attribute settings.              ;
;***********************************************************************;
undef("set_axis_string")
procedure set_axis_string(plot:graphic,res:logical,data:numeric,axis:string)
local title, tires, is_xyplot
begin

  tires = get_res_eq(res,(/"ti"/))  ; Get tickmark resources
  is_xyplot = get_res_value_keep(res,"gsnXYPlot", False)

  valid_coord = is_valid_coord(data,axis)
  if(axis.eq."y") then
    mode_res = "tmYLMode"
    axis_res = "tiYAxisString"
    if(valid_coord) then
      coord_var = data&$data!0$
    end if
  else
    mode_res = "tmXBMode"
    axis_res = "tiXAxisString"
    if(valid_coord) then
      coord_var = data&$data!1$
    end if
  end if
;
; If the axis tickmarks are being labeled explicitly by the user, 
; then don't put a label on that axis.
;
  if(check_attr_enum_value(res,mode_res,"Explicit")) then
    set_attr(tires,axis_res,"")
  else
    if(.not.is_xyplot) then
      if(valid_coord) then
        axis_string = get_csm_long_name_units_string(coord_var)
        if(.not.ismissing(axis_string)) then
          set_attr(tires,axis_res,axis_string)
        end if
      end if
    else    
      axis_string = get_csm_long_name_units_string(data)
      if(.not.ismissing(axis_string)) then
        set_attr(tires,axis_res,axis_string)
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
undef("set_subtitles_res")
procedure set_subtitles_res(res:logical,plotres:logical)
local valid_string_res
begin
  valid_subttl_res = (/"gsnLeftString","gsnCenterString","gsnRightString",\
                       "gsnStringFont","gsnStringFontHeightF", \
                       "gsnStringFontColor","gsnLeftStringFontColor", \
                       "gsnCenterStringFontColor","gsnRightStringFontColor", \
                       "gsnLeftStringFontHeightF",      \
                       "gsnCenterStringFontHeightF",    \
                       "gsnRightStringFontHeightF",     \
                       "gsnStringFuncCode",      \
                       "gsnLeftStringFuncCode",      \
                       "gsnCenterStringFuncCode",    \
                       "gsnRightStringFuncCode",     \
                       "gsnLeftStringParallelPosF",     \
                       "gsnCenterStringParallelPosF",   \
                       "gsnRightStringParallelPosF",    \
                       "gsnLeftStringOrthogonalPosF",   \
                       "gsnCenterStringOrthogonalPosF", \
                       "gsnRightStringOrthogonalPosF"/)
  if((res).and..not.any(ismissing(getvaratts(res))))
    nstrings = dimsizes(valid_subttl_res)
    do i=0,nstrings-1,1
      if(isatt(res,valid_subttl_res(i)))
        plotres = True
        plotres@$valid_subttl_res(i)$ = res@$valid_subttl_res(i)$
        delete(res@$valid_subttl_res(i)$)
      end if
    end do
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
; type equals "contour*" or "vector*" to these coordinate arrays.       ;
;                                                                       ;
; If no coordinate array exists, then this procedure checks for the     ;
; special "lat2d"/"lon2d" *attributes*. This is an indicator that we    ;
; have "2D coordinate" arrays, and we can set the sf/vfX/YArray         ;
; resources to these values. These 2D coords only work if you plan to   ;
; overlay the vectors/contours on a map, however.                       ;
;                                                                       ;
; In NCL V6.4.0, we added recognition of the special "lat1d"/"lon1d"    ;
; attributes which behave similarly to lat2d/lon2d, except it's an      ;
; indicator that we have 1D arrays of x,y,z data. These special attrs   ;
; only work for contouring, and not vectors or streamlines.             ;
;***********************************************************************;
undef("check_for_coord_arrays")
procedure check_for_coord_arrays(data:numeric,res:logical,type:string)
local prefix, xstart_name, xend_name, ystart_name, yend_name, \
xarray_name, yarray_name, valid_2dcoord_types
begin 
  if(type.eq."vector".or.type.eq."vector_map")
    prefix = "vf"
  else
    prefix = "sf"
  end if

  xarray_name = prefix + "XArray"
  xstart_name = prefix + "XCStartV"
  xend_name   = prefix + "XCEndV"
  yarray_name = prefix + "YArray"
  ystart_name = prefix + "YCStartV"
  yend_name   = prefix + "YCEndV"
  
;
; I originally thought that only plots over maps could have
; 2D lat/lon coordinates, but I don't believe this is the case.
; So, I've commented out the valid_2dcoord_types stuff.
;
;  valid_2dcoord_types = (/"contour_map","vector_map"/)
;
; We check for the following, in this order:
;
;   - special lat2d/lon2d attributes
;   - special lat1d/lon1d attributes (contouring only)
;   - coordinate arrays
;
; and then set vf/sfX/YArray accordingly.
;
  if(.not.((isatt(res,xstart_name).and.isatt(res,xend_name)).or.\
            isatt(res,xarray_name))) then
;    if(any(type.eq.valid_2dcoord_types).and. \
;       is_valid_latlon2d_attr(data,"lon2d")) then
    if(is_valid_latlon2d_attr(data,"lon2d")) then
      res@$xarray_name$ = data@lon2d
    else if(prefix.eq."sf".and.is_valid_latlon1d_attr(data,"lon1d")) then
      res@$xarray_name$ = data@lon1d
    else if(is_valid_coord(data,"x")) then
      res@$xarray_name$ = data&$data!1$
    end if
    end if
    end if
  end if

  if(.not.((isatt(res,ystart_name).and.isatt(res,yend_name)).or.\
            isatt(res,yarray_name))) then
;    if(any(type.eq.valid_2dcoord_types).and. \
;       is_valid_latlon2d_attr(data,"lat2d")) then
    if(is_valid_latlon2d_attr(data,"lat2d")) then
      res@$yarray_name$ = data@lat2d
    else if(prefix.eq."sf".and.is_valid_latlon1d_attr(data,"lat1d")) then
      res@$yarray_name$ = data@lat1d
    else if(is_valid_coord(data,"y")) then
      res@$yarray_name$ = data&$data!0$
    end if
    end if
    end if
  end if

  return
end

;***********************************************************************;
; Function : check_class_name                                           ;
;                  plot: graphic                                        ;
;             plot_type: string                                         ;
;                                                                       ;
; This procedure checks what type "plot" is, and returns the plot from  ;
; it (for example, if "plot" is an overlay, and you want the contour    ;
; part of it, it will return the contour plot.                          ;
;                                                                       ;
;***********************************************************************;
undef("check_class_name")
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
undef("get_allowed_latnames")
function get_allowed_latnames()
begin
  return((/"lat","Lat","Latitude","LAT","latitude","LATITUDE","hlat",\
           "lat_u","lat_t","lat_98","lat1","lat2","yc"/))
end

;***********************************************************************;
; Function : get_allowed_lonnames                                       ;
;                                                                       ;
; Get list of names allowed for a longitude coordinate array.           ; 
;                                                                       ;
;***********************************************************************;
undef("get_allowed_lonnames")
function get_allowed_lonnames()
begin
  return((/"lon","Lon","Longitude","LON","longitude","LONGITUDE","hlon",\
           "long","lon_u","lon_t","lon_98","lon1","lon2","xc"/))
end

;***********************************************************************;
; Function : get_allowed_pres_units_pa                                  ;
;                                                                       ;
; Get list of names allowed for pressure units in pascals.              ; 
;                                                                       ;
;***********************************************************************;
undef("get_allowed_pres_units_pa")
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
undef("get_allowed_pres_units_hpa")
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
undef("get_allowed_pres_units_mb")
function get_allowed_pres_units_mb()
begin
  return((/"mb","Mb","MB","millibar","millibars","MILLIBARS", \
           "hybrid_sigma_pressure"/))
end

;***********************************************************************;
; Function : get_allowed_pres_units                                     ;
;                                                                       ;
; Get list of names allowed for pressure units.                         ; 
;                                                                       ;
;***********************************************************************;
undef("get_allowed_pres_units")
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
undef("get_allowed_lat_units")
function get_allowed_lat_units()
begin
  return((/"degrees_north","degrees-north","degree_north","degrees north",\
           "degrees_N","Degrees_north","degree_N","degreeN","degreesN", \
           "deg north"/))
end

;***********************************************************************;
; Function : get_allowed_lon_units                                      ;
;                                                                       ;
; Get list of names allowed for the units attribute of a longitude      ;
; coordinate array.                                                     ; 
;                                                                       ;
;***********************************************************************;
undef("get_allowed_lon_units")
function get_allowed_lon_units()
begin
  return((/"degrees_east","degrees-east","degree_east","degrees east",\
           "degrees_E","Degrees_east","degree_E","degreeE","degreesE",\
           "deg east"/))
end

;***********************************************************************;
; Function : is_valid_latlon_coord                                      ;
;                                                                       ;
; Checks if the X or Y coordinate variable contains real a valid lat or ;
; lon coordinate array. It does this just by checking for a valid       ;
; "units" attribute.                                                    ;
;                                                                       ;
; It used to be that we checked the name of the coordinate array as well;
; as the units.  But, according to D. Shea, it is enough check for just ;
; the units. So, as of Dec 2003, we are now just checking units.        ;
;                                                                       ;
; We don't print out any warning messages in this function, b/c         ;
; sometimes the calling routine is just calling it for informational    ;
; purposes. It is up to the calling routine to print an error if a False;
; value is returned.                                                    ;
;***********************************************************************;
undef("is_valid_latlon_coord")
function is_valid_latlon_coord(data:numeric,axis:string,latorlon:string,\
                               res:logical)
begin
;
; If tfDoNDCOverlay is set to True, then coordinate arrays shouldn't be
; used in the first place, and hence the units don't need to be correct.
; It is up to the user to do the right thing and *not* set coordinate
; arrays.  Here, we check for the setting of this resource to True,
; and if so, then the units attribute isn't even checked.
;
  if(get_res_value_keep(res,"tfDoNDCOverlay",False)) then
    return(False)
  end if

  if(axis.eq."x") then
    if(is_valid_coord(data,"x")) then
      if(latorlon.eq."lat") then
        if(isatt(data&$data!1$,"units").and. \
           any(data&$data!1$@units.eq.get_allowed_lat_units)) then
          return(True)
        end if
;
; Otherwise, we must be dealing with longitudes.
;
      else
        if(isatt(data&$data!1$,"units").and.\
          any(data&$data!1$@units.eq.get_allowed_lon_units)) then
          return(True)
        end if
      end if
    end if
;
; We are dealing with the Y axis.
;
  else
    if(is_valid_coord(data,"y")) then
      if(latorlon.eq."lat") then
        if(isatt(data&$data!0$,"units").and.\
          any(data&$data!0$@units.eq.get_allowed_lat_units)) then
          return(True)
        end if
;
; Otherwise, we must be dealing with longitudes.
;
      else
        if(isatt(data&$data!0$,"units").and.\
          any(data&$data!0$@units.eq.get_allowed_lon_units)) then
          return(True)
        end if
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
 
undef("gsn_geop_hgt")
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
    ptmp = tofloat_wunits(p)     ; Assume already converted to mb!
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
; This function calculates the zonal means.                             ;
;***********************************************************************;
undef("calculate_zonal_means")
function calculate_zonal_means(data2,res2)
local zms2, nmeans
begin
  zref  = get_res_value(res2,"gsnZonalMeanYRefLine",0.)
  zmns2 = dim_avg(data2)
;
; The "if" code below was originally checking for le/ge "0.", rather
; than "zref". I think this is a bug, because I think originally, 
; the zref line was set to 0. no matter what.
;
  if(min(zmns2).le.zref.or.max(zmns2).ge.zref) then
    nmeans    = dimsizes(zmns2)
    zmns      = new((/2,nmeans/),typeof(zmns2))
    zmns(0,:) = zmns2
    zmns(1,:) = fspan(zref,zref,nmeans)
  else
    zmns = zmns2
  end if
  delete(zmns2)
  return(zmns)
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
; "contour*" or "vector*" to this coordinate array.                     ;
;                                                                       ;
;***********************************************************************;
undef("check_for_y_lat_coord")
procedure check_for_y_lat_coord(data:numeric,res:logical,type:string)
local latname, char_latname, array_name, start_name, end_name, \
valid_2dcoord_types, prefix
begin 
;
;
; If tfDoNDCOverlay is set to True, then coordinate arrays shouldn't be
; used in the first place.  It is up to the user to do the right thing
; and *not* set coordinate arrays.  Here, we check for the setting
; of this resource to True, and if so, then no warning messages are 
; printed out if no coordinate arrays are set.
;
  if(get_res_value_keep(res,"tfDoNDCOverlay",False)) then
    return(False)
  end if
;
; Determine what kind of plot we need to set the data field resources for.
; 
  if(type.eq."vector".or.type.eq."vector_map")
    prefix = "vf"
  else
    prefix = "sf"
  end if
  array_name = prefix + "YArray"
  start_name = prefix + "YCStartV"
  end_name   = prefix + "YCEndV"

;
; I originally thought that Only plots over maps could have
; 2D lat/lon coordinates, but I don't believe this is the case.
; So, I've commented out the valid_2dcoord_types stuff.
;
;  valid_2dcoord_types = (/"contour_map","vector_map"/)
;
; Make sure user is not already setting own *Start* and *End*
; or *Array resources. Then check if a valid "lat2d" attribute
; or "lat2d" attribute set or if a valid coordinate array exists.
;
  if(.not.((isatt(res,start_name).and.isatt(res,end_name)).or.\
            isatt(res,array_name))) then
;    if(any(type.eq.valid_2dcoord_types).and. \
;       is_valid_latlon2d_attr(data,"lat2d")) then
    if(is_valid_latlon2d_attr(data,"lat2d")) then
      res@$array_name$ = data@lat2d
    else if(prefix.eq."sf".and.is_valid_latlon1d_attr(data,"lat1d")) then
      res@$array_name$ = data@lat1d
    else if(is_valid_latlon_coord(data,"y","lat",res))
      res@$array_name$ = data&$data!0$
    else
      print("check_for_y_lat_coord: Warning: Data either does not contain")
      print("a valid latitude coordinate array or doesn't contain one at all.")
      print("A valid latitude coordinate array should have a 'units'")
      print("attribute equal to one of the following values: ")
      print("    " + cat_strings(get_allowed_lat_units))
    end if
    end if
    end if
  end if 

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
; "contour*" or "vector*" to this coordinate array.                     ;
;                                                                       ;
;***********************************************************************;
undef("check_for_x_lat_coord")
procedure check_for_x_lat_coord(data:numeric,res:logical,type:string)
local latname, char_latname, array_name, start_name, end_name, \
valid_2dcoord_types, prefx
begin 
;
; If tfDoNDCOverlay is set to True, then coordinate arrays shouldn't be
; used in the first place.  It is up to the user to do the right thing
; and *not* set coordinate arrays.  Here, we check for the setting
; of this resource to True, and if so, then no warning messages are 
; printed out if no coordinate arrays are set.
;
  if(get_res_value_keep(res,"tfDoNDCOverlay",False)) then
    return(False)
  end if
;
; Determine what kind of plot we need to set the data field resources for.
; 
  if(type.eq."vector".or.type.eq."vector_map")
    prefix = "vf"
  else
    prefix = "sf"
  end if
  array_name = prefix + "XArray"
  start_name = prefix + "XCStartV"
  end_name   = prefix + "XCEndV"

;
; I originally thought that Only plots over maps could have
; 2D lat/lon coordinates, but I don't believe this is the case.
; So, I've commented out the valid_2dcoord_types stuff.
;
;  valid_2dcoord_types = (/"contour_map","vector_map"/)

;
; Make sure user is not already setting own *Start* and *End*
; or *Array resources. Then check if a valid "lat2d" or
; "lat1d" attributes are set, or if a valid coordinate array exists.
;
  if(.not.((isatt(res,start_name).and.isatt(res,end_name)).or. \
            isatt(res,array_name))) then
;    if(any(type.eq.valid_2dcoord_types).and. \
;       is_valid_latlon2d_attr(data,"lat2d")) then
    if(is_valid_latlon2d_attr(data,"lat2d")) then
      res@$array_name$ = data@lat2d
    else if(prefix.eq."sf".and.is_valid_latlon1d_attr(data,"lat1d")) then
      res@$array_name$ = data@lat1d
    else if(is_valid_latlon_coord(data,"x","lat",res))
      res@$array_name$ = data&$data!1$
    else
      print("check_for_x_lat_coord: Warning: Data either does not contain")
      print("a valid latitude coordinate array or doesn't contain one at all.")
      print("A valid latitude coordinate array should have a 'units'")
      print("attribute equal to one of the following values: ")
      print("    " + cat_strings(get_allowed_lat_units))
    end if
    end if
    end if
  end if

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
; "contour*" or "vector*" to this coordinate array.                     ;
;                                                                       ;
;***********************************************************************;
undef("check_for_lon_coord")
procedure check_for_lon_coord(data:numeric,res:logical,type:string)
local lonname, char_lonname, array_name, start_name, end_name, \
valid_2dcoord_types
begin 
;
; If tfDoNDCOverlay is set to True, then coordinate arrays shouldn't be
; used in the first place.  It is up to the user to do the right thing
; and *not* set coordinate arrays.  Here, we check for the setting
; of this resource to True, and if so, then no warning messages are 
; printed out if no coordinate arrays are set.
;
  if(get_res_value_keep(res,"tfDoNDCOverlay",False)) then
    return(False)
  end if

;
; Determine what kind of plot we need to set the data field resources for.
; 
  if(type.eq."vector".or.type.eq."vector_map")
    prefix = "vf"
  else
    prefix = "sf"
  end if
  array_name = prefix + "XArray"
  start_name = prefix + "XCStartV"
  end_name   = prefix + "XCEndV"

;
; I originally thought that Only plots over maps could have
; 2D lat/lon coordinates, but I don't believe this is the case.
; So, I've commented out the valid_2dcoord_types stuff.
;
;  valid_2dcoord_types = (/"contour_map","vector_map"/)

;
; Make sure user is not already setting his own *Start* and *End*
; or *Array resources. If not, check for a valid lon2d attribute
; or a valid coordinate array.
;
  if(.not.((isatt(res,start_name).and.isatt(res,end_name)).or.\
            isatt(res,array_name))) then
;    if(any(type.eq.valid_2dcoord_types).and. \
;       is_valid_latlon2d_attr(data,"lon2d")) then
    if(is_valid_latlon2d_attr(data,"lon2d")) then
      res@$array_name$ = data@lon2d
    else if(prefix.eq."sf".and.is_valid_latlon1d_attr(data,"lon1d")) then
      res@$array_name$ = data@lon1d
    else if(is_valid_latlon_coord(data,"x","lon",res))
      res@$array_name$ = data&$data!1$
    else
      print("check_for_lon_coord: Warning: Data either does not contain")
      print("a valid longitude coordinate array or doesn't contain one at all.")
      print("A valid longitude coordinate array should have a 'units'")
      print("attribute equal to one of the following values: ")
      print("    " + cat_strings(get_allowed_lon_units))
    end if
    end if
    end if 
  end if

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
undef("get_polar_type")
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
undef("add_subtitles")
procedure add_subtitles(wks:graphic,plot:graphic,left_string:logical,\
                        center_string:logical,right_string:logical,\
                        res:logical)
local text_object, parallel_pos, anno, just, strings, txres, amres, \
num_res, ablank, chararray, string_len, colors, ss
begin
  amres = False   ; Annotation resources
  txres = False   ; Text resources
  txres = get_res_eq(res,"tx")
  amres = get_res_eq(res,"am")
;
; Get the height, as we use this to calculate distance of strings
; from top of plot.
;
  getvalues plot
    "vpHeightF"  : vph
  end getvalues

  num_res  = 0
  parallel_pos = new(3,float)
  parallel_pos(0) = get_res_value(left_string, \
                                  "gsnLeftStringParallelPosF",   0.0)
  parallel_pos(1) = get_res_value(center_string, \
                                  "gsnCenterStringParallelPosF", 0.5)
  parallel_pos(2) = get_res_value(right_string, \
                                  "gsnRightStringParallelPosF",  1.0)

  zone    = get_res_value(amres,"amZone",3)
  orthpos = get_res_value(amres,"amOrthogonalPosF",0.01*vph)
  orthogonal_pos = new(3,float)
  orthogonal_pos(0) = get_res_value(left_string, \
                                  "gsnLeftStringOrthogonalPosF",  orthpos)
  orthogonal_pos(1) = get_res_value(center_string, \
                                  "gsnCenterStringOrthogonalPosF",orthpos)
  orthogonal_pos(2) = get_res_value(right_string, \
                                  "gsnRightStringOrthogonalPosF", orthpos)

  just         = (/"left","center","right"/)
  fhghts       = (/0.,0.,0./)
  fcodes       = (/"~","~","~"/)
  strings      = (/"","",""/)  
  colors       = True  ;  We can't set to an array, because colors could
                       ; be integers or strings. Instead, we'll use
                       ; attributes to set the colors for each string.
;
; Set the three strings and their fonts, colors, and font heights. 
;
  if(left_string)
    if(isatt(left_string,"gsnStringFont"))
      txres@txFont = left_string@gsnStringFont
    end if
    strings(0) = left_string@sub_string
    if(isatt(left_string,"gsnLeftStringFontHeightF")) then
      fhghts(0) = get_res_value_keep(left_string,"gsnLeftStringFontHeightF",0.)
    else if(isatt(left_string,"gsnStringFontHeightF")) then
      fhghts(0) = get_res_value_keep(left_string,"gsnStringFontHeightF",0.)
    else
      fhghts(0) = get_res_value_keep(txres,"txFontHeightF",0.0)
    end if
    end if
    if(isatt(left_string,"gsnLeftStringFuncCode")) then
      fcodes(0) = get_res_value_keep(left_string,"gsnLeftStringFuncCode","~")
    else
      fcodes(0) = get_res_value_keep(left_string,"gsnStringFuncCode","~")
    end if
    if(isatt(left_string,"gsnLeftStringFontColor")) then
      colors@s0 = get_res_value_keep(left_string,"gsnLeftStringFontColor",1)
    else
      colors@s0 = get_res_value_keep(left_string,"gsnStringFontColor",1)
    end if
  end if
  if(center_string) 
    if(isatt(center_string,"gsnStringFont"))
      txres@txFont = center_string@gsnStringFont
    end if
    strings(1) = center_string@sub_string
    if(isatt(center_string,"gsnCenterStringFontHeightF")) then
      fhghts(1) = get_res_value_keep(center_string,"gsnCenterStringFontHeightF",0.)
    else if(isatt(center_string,"gsnStringFontHeightF")) then
      fhghts(1) = get_res_value_keep(center_string,"gsnStringFontHeightF",0.)
    else
      fhghts(1) = get_res_value_keep(txres,"txFontHeightF",0.0)
    end if
    end if
    if(isatt(center_string,"gsnCenterStringFuncCode")) then
      fcodes(1) = get_res_value_keep(center_string,"gsnCenterStringFuncCode","~")
    else
      fcodes(1) = get_res_value_keep(center_string,"gsnStringFuncCode","~")
    end if
    if(isatt(center_string,"gsnCenterStringFontColor")) then
      colors@s1 = get_res_value_keep(center_string,"gsnCenterStringFontColor",1)
    else
      colors@s1 = get_res_value_keep(center_string,"gsnStringFontColor",1)
    end if
  end if
  if(right_string)
    if(isatt(right_string,"gsnStringFont"))
      txres@txFont = right_string@gsnStringFont
    end if
    strings(2) = right_string@sub_string
    if(isatt(right_string,"gsnRightStringFontHeightF")) then
      fhghts(2) = get_res_value_keep(right_string,"gsnRightStringFontHeightF",0.)
    else if(isatt(right_string,"gsnStringFontHeightF")) then
      fhghts(2) = get_res_value_keep(right_string,"gsnStringFontHeightF",0.)
    else
      fhghts(2) = get_res_value_keep(txres,"txFontHeightF",0.0)
    end if
    end if
    if(isatt(right_string,"gsnRightStringFuncCode")) then
      fcodes(2) = get_res_value_keep(right_string,"gsnRightStringFuncCode","~")
    else
      fcodes(2) = get_res_value_keep(right_string,"gsnStringFuncCode","~")
    end if
    if(isatt(right_string,"gsnRightStringFontColor")) then
      colors@s2 = get_res_value_keep(right_string,"gsnRightStringFontColor",1)
    else
      colors@s2 = get_res_value_keep(right_string,"gsnStringFontColor",1)
    end if
  end if
;
; Remove the text font height and txString, so they don't get
; applied at the end.
;
  delete_attr(txres,"txFontHeightF")
  delete_attr(txres,"txString")

  ablank = stringtocharacter(" ")
  do i=0,2
    if(.not.ismissing(strings(i)).and.strings(i).ne."")
;
; Check to make sure the string doesn't contain only blanks.
;
      chararray = stringtocharacter(strings(i)) 
      string_len = dimsizes(chararray)-1

      if(any(chararray(0:string_len-1).ne.ablank(0)))
        ss = "s" + i
        text_object = create just(i)+"string" textItemClass wks
          "txString"      : strings(i)
          "txFontHeightF" : fhghts(i)
          "txFuncCode"    : fcodes(i)
          "txFontColor"   : colors@$ss$
        end create

        attsetvalues_check(text_object,txres)   ; Set some text resources.

        anno = NhlAddAnnotation(plot,text_object)  ; Add annotation to plot.

        setvalues anno
          "amZone"          : zone          ; Outside plot area
          "amSide"          : "top"         ; Subtitle at top.
          "amParallelPosF"  : parallel_pos(i)
          "amJust"          : "bottom"+just(i)
          "amOrthogonalPosF": orthogonal_pos(i) ; Move away from top edge
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
; Procedure : set_labelbar_res                                          ;
;                  res[1]  : logical                                    ;
;                  type[1] : string                                     ;
;                                                                       ;
; Sets some resources for a PlotManager labelbar. This is for when we   ;
; decide to use the PlotManager labelbar instead of the custom labelbar.;
;                                                                       ;
; The "type" allows you to check for special plots, like polar plots.   ;
;***********************************************************************;
undef("set_labelbar_res")
procedure set_labelbar_res(r[1]:logical,type[1]:string)
begin
  set_attr(r,"pmLabelBarDisplayMode","Always")
  set_attr(r,"lbPerimOn",False)
  set_attr(r,"lbLabelJust","CenterCenter")
  set_attr(r,"lbAutoManage",False)
  if(check_attr_enum_value(r,"lbOrientation","vertical"))
    set_attr(r,"pmLabelBarSide","right")
  else
    set_attr(r,"pmLabelBarSide","bottom")
    set_attr(r,"lbOrientation","Horizontal")
  end if
  if(type.eq."polar")
    set_attr(r,"pmLabelBarOrthogonalPosF",0.05)
  else if(type.eq."ce".and..not.\
          (check_attr_enum_value(r,"pmTickMarkDisplayMode","Always").or.\
           check_attr_enum_value(r,"pmTickMarkDisplayMode","Conditional").or.\
           check_attr_enum_value(r,"pmTickMarkDisplayMode","ForceAlways"))) then
    set_attr(r,"pmLabelBarOrthogonalPosF",0.08)
  end if
  end if
end

;***********************************************************************;
; Procedure : add_labelbar                                              ;
;                wks: graphic                                           ;
;               plot: graphic                                           ;
;               zone: integer                                           ;
;        font_height: numeric                                           ;
;               type: string                                            ;
;           lbar_on : logical                                           ;
;                res: logical                                           ;
;                                                                       ;
; This procedure attaches a labelbar to "plot" (which must be either a  ;
; contour or vector plot). The default is to add the labelbar to the    ;
; bottom of the plot, unless resources are set to change this. The zone ;
; must be passed in, as well as the font height for the labelbar labels.;
; "type" is the type of plot ("ce", "polar", and so on) so the labelbar ;
; can be positioned differently depending on the plot.                  ;
;                                                                       ;
; Eventually we want to deprecate this procedure and use the PlotManager;
; labelbar. This is what the global GSN_OLD_LABELBAR variable is for.   ;
; If set to True, then the PlotManager labelbar is *not* used, and a    ;
; a custom labelbar via this routine, is created.                       ;
;
; In V6.4.0 we changed this code to create the lablebar, but only add   ;
; it as an annotation if lbar_on is True.                               ;
;***********************************************************************;
undef("add_labelbar")
procedure add_labelbar(wks:graphic,plot,zone:integer, \
                       font_height:numeric,type:string,\
                       lbar_on,res:logical)
local anno, num_res, amres, lbres, labelbar_object, lbar_orient, \
lbar_side, lbar_just, fill_patterns, fill_scales, mono_fill_pat, \
mono_fill_scl, mono_fill_col, levels, class_name, the_plot, anno_off
begin
  lbres = True   ; hold labelbar resources
  amres = True   ; hold annomanager resources 

  parallel = get_res_value(res,"pmLabelBarParallelPosF",0.5)

;---Check for annotation/labelbar resources
  lbres = get_res_eq(res,(/"lb","vp"/))
  amres = get_res_eq(res,"am")

;---Set some defaults, which may be overridden later.
  set_attr(amres,"amZone",zone)
  set_attr(amres,"amResizeNotify",True)
  set_attr(amres,"amParallelPosF",parallel)
  set_attr(lbres,"lbLabelFontHeightF",font_height)
  set_attr(lbres,"lbBoxEndCapStyle",get_enum_value("RectangleEnds"))

;---The default is to attach a horizontal labelbar at the bottom of the plot.
  if(check_attr_enum_value(lbres,"lbOrientation","vertical")) then
    lbar_orient = "vertical"
    lbar_side   = "right"
    lbar_just   = "centerright"
  else
    lbar_orient = "horizontal"
    lbar_side   = "bottom"
    lbar_just   = "bottomcenter"
  end if

  set_attr(lbres,"lbOrientation",lbar_orient)
  set_attr(amres,"amSide",lbar_side)
  set_attr(amres,"amJust",lbar_just)
; 
; Calculate the labelbar width and height, based on the plot type and
; shape. Also calculate how far to move the labelbar from the plot.
;
; Here are some examples on how the width/height are determined.
;
;  - Square plot: make the labelbar the same width as the plot.
;  - Plot w/aspect ratio of 1-to-2: make the labelbar 3/4 the width of
;    the plot.
;  - Plot w/vertical labelbar: make the height the same as the height 
;    of the plot.
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

;
; If we have a contour plot, then there are a bunch of contour
; resources that we have to retrieve in order to reconstruct the 
; labelbar.
; 
  class_name = NhlClassName(plot)

  if(class_name.eq."logLinPlotClass".or.class_name.eq."irregularPlotClass")
    the_plot = plot@contour
  else
    the_plot = plot
  end if
  if(class_name.eq."contourPlotClass".or.class_name.eq."logLinPlotClass".or.\
     class_name.eq."irregularPlotClass")
    getvalues the_plot
      "cnLevels"              : levels
      "cnFillDotSizeF"        : dot_size
      "cnFillColors"          : tmp_colors
      "cnFillOpacityF"        : fill_opacity
      "cnFillPatterns"        : tmp_fill_patterns
      "cnFillPattern"         : fill_pattern
      "cnFillScales"          : tmp_fill_scales
      "cnFillScaleF"          : fill_scale
      "cnFillMode"            : fill_mode
      "cnMonoFillPattern"     : mono_fill_pat
      "cnMonoFillScale"       : mono_fill_scl
      "cnMonoFillColor"       : mono_fill_col
      "cnLabelBarEndStyle"    : end_labelbar_style
      "cnLabelBarEndLabelsOn" : end_labels_on
      "lbLabelStrings"        : lbstrings
    end getvalues
;
; This next section is a bit of a nightmare.
;
; It's an attempt to handle various combinations of lbBoxEndCapStyle, 
; cnLabelBarEndStyle, cnLabelBarEndLabelsOn, vcLabelBarEndLabelsOn,
; and stLabelBarEndLabelsOn.
;
; To make things more complicated, cnLabelBarEndLabelsOn is deprecated
; and replaced with cnLabelBarEndStyle, but we still have to deal with
; it. On top of that, vcLabelBarEndLabelsOn and stLabelBarEndLabelsOn
; are NOT deprecated, because we don't have vcLabelBarEndStyle or 
; stLabelBarEndStyle resources.
;
; lbBoxEndCapStyle was added in V6.4.0 and allows you to add triangle
; ends for the labelbar. 
;
; cnLabelBarEndLabelsOn was deprecated in V5.0, but we still need to
; check for it. It's been replaced by cnLabelBarEndStyle.
;
; If the end label style is IncludeOuterBoxes *and* 
; xxLabelBarEndLabelsOn is True, then set the style to 
; IncludeMinMaxLabels. Conversely, if the style is IncludeMinMaxLabels 
; or ExcludeOuterBoxes, then xxLabelBarEndLabelsOn should be set True.
;
    cap_style = lbres@lbBoxEndCapStyle
    if(end_labelbar_style.eq.get_enum_value("IncludeOuterBoxes")) then
      if(end_labels_on) then
        end_labelbar_style = get_enum_value("IncludeMinMaxLabels")
      end if
    else
      end_labels_on = True
    end if
    if(end_labelbar_style.eq.get_enum_value("ExcludeOuterBoxes")) then
; Don't use the first fill color, scale, or pattern.
      nc2 = dimsizes(tmp_colors)
      end_labels_on = True
      colors        = tmp_colors(1:nc2-2)
      fill_scales   = tmp_fill_scales(1:nc2-2)
      fill_patterns = tmp_fill_patterns(1:nc2-2)
    else
      colors        = tmp_colors
      fill_scales   = tmp_fill_scales
      fill_patterns = tmp_fill_patterns
    end if
    delete([/tmp_colors,tmp_fill_scales,tmp_fill_patterns/])
;
; Check if the fill mode is "RasterFill". If so, be sure to 
; set lbRasterFillOn to True
;
    if(.not.isatt(lbres,"lbRasterFillOn").and.fill_mode.eq.get_enum_value("RasterFill"))
      lbres@lbRasterFillOn = True
    end if
;
; Check if we want different fill patterns or fill scales.  If so, we
; have to pass these on to the labelbar.
;
    lbres@lbMonoFillColor  = mono_fill_col
    lbres@lbBoxEndCapStyle = cap_style
    if(.not.mono_fill_pat)
      lbres@lbMonoFillPattern = False
      lbres@lbFillPatterns    = fill_patterns
      lbres@lbFillDotSizeF    = dot_size
    else
      lbres@lbFillPattern = fill_pattern
    end if
    if(.not.mono_fill_scl)
      lbres@lbMonoFillScale = False
      lbres@lbFillScales    = fill_scales
      lbres@lbFillDotSizeF  = dot_size
    else
      lbres@lbFillScaleF    = fill_scale
    end if
    lbres@lbFillOpacityF = fill_opacity     ; Added in NCL V6.4.0
  else if(class_name.eq."vectorPlotClass") then
    getvalues the_plot
      "vcLevels"              : levels
      "vcLevelColors"         : colors
      "vcLabelBarEndLabelsOn" : end_labels_on
      "lbLabelStrings"        : lbstrings
    end getvalues
  else
;---Better be a streamline plot!
    getvalues the_plot
      "stLevels"              : levels
      "stLevelColors"         : colors
      "stLabelBarEndLabelsOn" : end_labels_on
      "lbLabelStrings"        : lbstrings
    end getvalues
  end if
  end if
;
; Vector and Streamline don't have the xxLabelBarEndStyle resource
; implemented yet. So, based on setting of xxLabelBarEndLabelsOn,
; we can set the style.
;
  if(.not.isvar("end_labelbar_style")) then
    if(end_labels_on) then
      end_labelbar_style = get_enum_value("IncludeMinMaxLabels")
    else
      end_labelbar_style = get_enum_value("IncludeOuterBoxes")
    end if
  end if
;
; Now we have to figure out what labels to use for the labelbar.
;
; If user set cn/st/vcLabelBarEndLabelsOn to True or the LabelBarStyle
; resource to IncludeMinMaxLabels, then we need to add min/max 
; labels, if the user hasn't done so (by setting lbLabelStrings). 
; Use lbLabelStrings rather than actual min/max of data, because the 
; labels will already be formatted correctly.
;
  if(.not.isatt(res,"lbLabelStrings")) then
    nlev = dimsizes(levels)
    nlab = dimsizes(lbstrings)
    if(end_labels_on.and.end_labelbar_style.eq.get_enum_value("IncludeMinMaxLabels").and. \
        (lbstrings(0).eq."".or.lbstrings(nlab-1).eq."")) then
      print("Warning: add_labelbar: invalid end labels, turning them off.")
      end_labels_on = False
    end if
    if(end_labels_on) then
      if(end_labelbar_style.eq.get_enum_value("IncludeMinMaxLabels")) then
        lbres@lbLabelStrings = lbstrings
      end if
    end if
  end if  
  if(end_labels_on) then
    set_attr(lbres,"lbLabelAlignment","ExternalEdges")
  end if

  levels = fix_zero_contour(levels)

  labelbar_object = create_labelbar(wks,dimsizes(colors),colors,levels,lbres)
;
; Check if the user has requested to turn off the labelbar.
; We will go ahead and create it, just in case it is needed
; later (by gsn_panel, for example), but we will turn it off
; by setting amOn to False.
;
  anno = NhlAddAnnotation(plot,labelbar_object)

  set_attr(amres,"amOn",lbar_on)
  attsetvalues_check(anno,amres)   ; Set annotation resources.

  return
end

;***********************************************************************;
; Procedure : fix_labelbar                                              ;
;                                                                       ;
; This procedure "fixes" a PlotManager labelbar by adjusting the label  ;
; font heights, among possibly other things.                            ;
;                                                                       ;
; This is for when we decide to start using the PlotManager labelbar    ;
; again (GSN_OLD_LABELBAR = False).                                     ;
;                                                                       ;
; lbar_on is the setting from lbLabelBarOn. For now, don't do anything  ;
; if this is False. This may change when/if we decide to use the        ;
; PlotManager labelbar as the default.                                  ;
;***********************************************************************;
undef("fix_labelbar")
procedure fix_labelbar(plot:graphic,font_height:numeric,\
                       lbar_on[1]:logical,res)
local width, height, the_plot, class_name, ratio1, ratio2, ratio, \
      ratios, lwscale, wh_ind, def_lvpwf, lbar_orient, lbres
begin
  if(.not.lbar_on) then
    return
  end if
  class_name = NhlClassName(plot)
  if(class_name.eq."logLinPlotClass".or.class_name.eq."irregularPlotClass") then
    if(isatt(plot,"contour")) then
      the_plot = plot@contour
    else if(isatt(plot,"vector")) then
      the_plot = plot@vector
    else if(isatt(plot,"streamline")) then
      the_plot = plot@streamline
    else
      print("Warning: fix_labelbar: invalid plot type, won't try to fix the labelbar.")
      exit      
    end if
    end if
    end if
  else
    the_plot = plot
  end if

  if(check_attr_enum_value(res,"lbOrientation","vertical")) then
    lbar_orient = "vertical"
  else
    lbar_orient = "horizontal"
  end if
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
  ratio  = min((/ratio1,ratio2/))
  ratios = (/0.50, 0.75, 1.00/)  ; viewport ratios

  lwscale   = (/0.75, 0.90, 1.00/)
  wh_ind    = ind(ratio.le.ratios)
  def_lvpwf = lwscale(wh_ind(0))    ; default width scale for labelbar

  if(lbar_orient.eq."vertical") then
    height = get_res_value(res,"pmLabelBarHeightF",height)
    width  = get_res_value(res,"pmLabelBarWidthF",0.2*width)
  else
    height = get_res_value(res,"pmLabelBarHeightF",0.3*height)
    width  = get_res_value(res,"pmLabelBarWidthF",def_lvpwf*width)
  end if
;
; Don't understand why you have to set cnExplicitLabelBarLabelsOn to
; True if lbLabelAlignment and/or lbLabelStrings are set. Can't it
; detect that one of these resources is set, and turn it on by default?
;
  lbres = True
  lbres = get_res_eq(res,"lb")
  if(res.and.any(isatt(res,(/"lbLabelAlignment","lbLabelStrings"/)))) then
    if(class_name.eq."contourPlotClass") then
      lbres@cnExplicitLabelBarLabelsOn = get_res_value_keep(res,"cnExplicitLabelBarLabelsOn",True)
    else if(class_name.eq."vectorPlotClass") then
      lbres@vcExplicitLabelBarLabelsOn = get_res_value_keep(res,"vcExplicitLabelBarLabelsOn",True)
    else if(class_name.eq."streamlinePlotClass") then
      lbres@stExplicitLabelBarLabelsOn = get_res_value_keep(res,"stExplicitLabelBarLabelsOn",True)
    end if
    end if
    end if
  end if
  if(.not.res.or..not.isatt(res,"pmLabelBarHeightF"))
    lbres@pmLabelBarHeightF = height
  end if
  if(.not.res.or..not.isatt(res,"pmLabelBarWidthF"))
    lbres@pmLabelBarWidthF = width
  end if
  if(.not.res.or..not.isatt(res,"lbLabelFontHeightF")) then
    lbres@lbLabelFontHeightF = font_height
    lbres@lbAutoManage       = False
  end if

  attsetvalues(the_plot,lbres)
  return
end

;***********************************************************************;
; Procedure : set_pres_hgt_axes                                         ;
;               pres: numeric                                           ;
;                res: logical                                           ;
;            add_hgt: logical                                           ;
;                                                                       ;
; This procedure sets some resources necessary to label the left and    ;
; right Y axis with "nice" pressure and height values. The left axis is ;
; values, and the right for height values. The pressure values are      ;
; assumed to be in millibars.                                           ;
;***********************************************************************;
undef("set_pres_hgt_axes")
procedure set_pres_hgt_axes(pres:numeric,res:logical,add_hgt:logical)
local hgt, hnice, pnice, ptmp
begin
  if(pres(0).lt.pres(dimsizes(pres)-1))
    ptmp = tofloat_wunits(pres(::-1))    ; reverse values so descending order
  else
    ptmp = tofloat_wunits(pres)          ; Make sure values are floating point.
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
;             reftype[1]: string
;                 xin[*]: numeric                                       ;
;                 yin[*]: numeric                                       ;
;                    ref: numeric                                       ;
;   ref_line_greater_color: integer or string or RGB value              ;
;   ref_line_less_color:    integer or string or RGB value              ;
;                                                                       ;
; Given a set of X and Y points, fill the Y (or X) points in one color  ;
; if they are greater than the Y (X) ref line, and in another color if  ;
; they are less than the Y (X) ref line.                                ;
;***********************************************************************;
undef("fill_xy_ref")
function fill_xy_ref(wks:graphic,xy:graphic,reftype[1]:string,\   
                     xin[*]:numeric,yin[*]:numeric,ref:numeric,\  
                     ref_line_greater_color,ref_line_less_color)
local npts, greater_gon, less_gon, i, gsresg, gsresl, gsresr, \
fill_greater, fill_less, npts, gnumgons, lnumgons
begin
  npts = dimsizes(yin)

  fill_greater = .not.any(ismissing(ref_line_greater_color)).and.\
                 .not.is_color_transparent(ref_line_greater_color)  
  fill_less    = .not.any(ismissing(ref_line_less_color)).and.\
                 .not.is_color_transparent(ref_line_less_color)

  gnumgons = 0  ; Count the number of polygons
  lnumgons = 0
  if(fill_greater) then
    gsresg             = True
    gsresg@gsFillColor = ref_line_greater_color
;
; First count the number of the greater-than-reference-line polygons we
; need to create.
;
    igon       = 0
    create_gon = False
    do i=0,npts-1
      if(.not.ismissing(yin(i)).and..not.ismissing(xin(i)))
        if((reftype.eq."y".and.yin(i).ge.ref).or.\
           (reftype.eq."x".and.xin(i).ge.ref)) then
          igon = igon + 1
          if(i.eq.(npts-1))      ; On the last point.
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
          gnumgons = gnumgons + 1 
        end if
        igon       = 0
        create_gon = False
      end if
    end do
  end if

  if(fill_less) then
    gsresl             = True
    gsresl@gsFillColor = ref_line_less_color
;
; Count the number of the less-than-reference-line polygons we
; need to create.
;
    igon       = 0
    create_gon = False
    do i=0,npts-1
      if(.not.ismissing(yin(i)).and..not.ismissing(xin(i)))
        if((reftype.eq."y".and.yin(i).le.ref).or.\
           (reftype.eq."x".and.xin(i).le.ref)) then
          igon = igon + 1
          if(i.eq.(npts-1))      ; On the last point.
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
          lnumgons = lnumgons + 1 
        end if
        igon       = 0
        create_gon = False
      end if
    end do
  end if
  if(gnumgons.gt.0.or.lnumgons.gt.0) then
    polygons = new(gnumgons+lnumgons,graphic) 
  else
    polygons = new(1,graphic) 
  end if
;
; Loop through and add greater-ref-line polygons.
;
  ngons = 0
  if(fill_greater.and.gnumgons.gt.0)
    igon       = 0
    create_gon = False
    do i=0,npts-1
      if(.not.ismissing(yin(i)).and..not.ismissing(xin(i)))
        if((reftype.eq."y".and.yin(i).ge.ref).or.\
           (reftype.eq."x".and.xin(i).ge.ref)) then
          if(igon.eq.0)
            xgreater_line = new(npts+2,typeof(xin))
            ygreater_line = new(npts+2,typeof(yin))
            ygreater_line(0) = yin(i)
            xgreater_line(0) = xin(i)
            igon = 1
          else
            xgreater_line(igon) = xin(i) 
            ygreater_line(igon) = yin(i) 
            igon = igon + 1
          end if
          if(i.eq.(npts-1))      ; On the last point.
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
          ygreater_gon = new((/igon+3/),typeof(yin))
          xgreater_gon = new((/igon+3/),typeof(xin))
; Close up and draw polygon
          if(reftype.eq."y".and.ygreater_line(0).ne.ref) then
            xgreater_gon(0) = xgreater_line(0)
            ygreater_gon(0) = ref
            xgreater_gon(1:igon) = xgreater_line(0:igon-1)
            ygreater_gon(1:igon) = ygreater_line(0:igon-1)
            newgon = igon + 1
          else if(reftype.eq."x".and.xgreater_line(0).ne.ref) then
            ygreater_gon(0) = ygreater_line(0)
            xgreater_gon(0) = ref
            ygreater_gon(1:igon) = ygreater_line(0:igon-1)
            xgreater_gon(1:igon) = xgreater_line(0:igon-1)
            newgon = igon + 1
          else
            xgreater_gon(0:igon-1) = xgreater_line(0:igon-1)
            ygreater_gon(0:igon-1) = ygreater_line(0:igon-1)
            newgon = igon
          end if
          end if
          if(reftype.eq."y".and.ygreater_line(igon-1).ne.ref) then
            xgreater_gon(newgon) = xgreater_line(igon-1)
            ygreater_gon(newgon) = ref
            newgon = newgon + 1
          else if(reftype.eq."x".and.xgreater_line(igon-1).ne.ref) then
            ygreater_gon(newgon) = ygreater_line(igon-1)
            xgreater_gon(newgon) = ref
            newgon = newgon + 1
          end if
          end if

          polygons(ngons) = gsn_add_polygon(wks,xy,xgreater_gon(0:newgon-1),\
                                            ygreater_gon(0:newgon-1),gsresg)
          ngons = ngons + 1 
; Delete temporary arrays so we can recreate them.
          delete([/xgreater_line,ygreater_line,xgreater_gon,ygreater_gon/])
        end if
        igon       = 0
        create_gon = False
      end if
    end do
  end if
;---Loop through and add less-ref-line polygons.
  if(fill_less.and.lnumgons.gt.0) then
    igon       = 0
    create_gon = False

    do i=0,npts-1
      if(.not.ismissing(yin(i)).and..not.ismissing(xin(i)))
        if((reftype.eq."y".and.yin(i).le.ref).or.\
           (reftype.eq."x".and.xin(i).le.ref)) then
          if(igon.eq.0)
            xless_line = new(npts+2,typeof(xin))
            yless_line = new(npts+2,typeof(yin))
            yless_line(0) = yin(i)
            xless_line(0) = xin(i)
            igon = 1
          else
            xless_line(igon) = xin(i) 
            yless_line(igon) = yin(i) 
            igon = igon + 1
          end if
          if(i.eq.(npts-1))      ; On the last point.
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
          yless_gon = new((/igon+3/),typeof(yin))
          xless_gon = new((/igon+3/),typeof(xin))
; Close up and draw polygon
          if(reftype.eq."y".and.yless_line(0).ne.ref) then
            xless_gon(0) = xless_line(0)
            yless_gon(0) = ref
            xless_gon(1:igon) = xless_line(0:igon-1)
            yless_gon(1:igon) = yless_line(0:igon-1)
            newgon = igon + 1
          else if(reftype.eq."x".and.xless_line(0).ne.ref) then
            yless_gon(0) = yless_line(0)
            xless_gon(0) = ref
            yless_gon(1:igon) = yless_line(0:igon-1)
            xless_gon(1:igon) = xless_line(0:igon-1)
            newgon = igon + 1
          else
            xless_gon(0:igon-1) = xless_line(0:igon-1)
            yless_gon(0:igon-1) = yless_line(0:igon-1)
            newgon = igon
          end if
          end if
          if(reftype.eq."y".and.yless_line(igon-1).ne.ref) then
            xless_gon(newgon) = xless_line(igon-1)
            yless_gon(newgon) = ref
            newgon = newgon + 1
          else if(reftype.eq."x".and.xless_line(igon-1).ne.ref) then
            yless_gon(newgon) = yless_line(igon-1)
            xless_gon(newgon) = ref
            newgon = newgon + 1
          end if
          end if

          polygons(ngons) = gsn_add_polygon(wks,xy,xless_gon(0:newgon-1), \
                                            yless_gon(0:newgon-1),gsresl)
          ngons = ngons + 1 
; Delete temporary arrays so we can recreate them.
          delete([/xless_line,yless_line,xless_gon,yless_gon/])
        end if
        igon       = 0
        create_gon = False
      end if
    end do
  end if
  return(polygons)
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
undef("get_lon_values")
function get_lon_values(min_lon:numeric,max_lon:numeric,lon_spacing:numeric,\
                        mlon_spacing:numeric,range_array,lonspacing_array, \
                        mlonspacing_array)
local lon_range, lcheck_arr, lspcng_arr, mlspcng_arr, lon_spcng, mlon_spcng, \
start_lon, end_lon, xstart_lon, xend_lon, lon
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
undef("get_lat_values")
function get_lat_values(min_lat:numeric,max_lat:numeric,lat_spacing:numeric,\
                        mlat_spacing:numeric)
local lat_range, lcheck_arr, lspcng_arr, mlspcng_arr, lat_spcng, mlat_spcng, \
start_lat, end_lat, lat, found, lat
begin

; Initialize

  if(min_lat.lt.-90.or.max_lat.gt.90) then
    print("get_lat_values: Warning: Your latitude values do not fall between -90 and 90 inclusive.")
    print("You will not get 'nice' latitude labels.")
    lat_values = new(1,float)
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
undef("get_lon_labels")
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
    lon_labels(lon_index) = (360-lon_values(lon_index)) + "W"  ; west
  end if
  delete(lon_index)

; 360 < lon < 540 (east)

  lon_index = ind(lon_values.gt.360.and.lon_values.lt.540)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = (lon_values(lon_index)-360)+ "E" ; east
  end if
  delete(lon_index)

; 540 < lon < 720 (west)

  lon_index = ind(lon_values.gt.540.and.lon_values.lt.720)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = (540-lon_values(lon_index)) + "W"  ; west
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
undef("get_lat_labels")
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
undef("add_lon_labels")
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
undef("add_lat_labels_xaxis")
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
undef("add_lat_labels_yaxis")
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
undef("add_latlon_labels")
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

  if((is_xyplot.and.is_xaxis.and.isatt(data,"units").and. \
      any(data@units.eq.get_allowed_lat_units)).or. \
     (.not.is_xyplot.and.is_valid_latlon_coord(data,"x","lat",res))) then

; Label X axis with nice latitude labels.

    getvalues plot
      "trXMinF"   : min_lat
      "trXMaxF"   : max_lat
    end getvalues

    add_lat_labels_xaxis(plot,min_lat,max_lat,0,0,res)
  else
    if((is_xyplot.and.is_xaxis.and.isatt(data,"units").and. \
        any(data@units.eq.get_allowed_lon_units)).or. \
       (.not.is_xyplot.and.is_valid_latlon_coord(data,"x","lon",res))) then

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
    else
;
; Only set a X axis label if it is not a lon axis.
;
      if((is_xyplot.and.is_xaxis).or..not.is_xyplot) then
        set_axis_string(plot,res,data,"x")
      end if
    end if
  end if

  if((is_xyplot.and.is_yaxis.and.isatt(data,"units").and. \
      any(data@units.eq.get_allowed_lat_units)).or. \
     (.not.is_xyplot.and.is_valid_latlon_coord(data,"y","lat",res))) then

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
    if((is_xyplot.and.is_yaxis).or..not.is_xyplot) then
      set_axis_string(plot,res,data,"y")
    end if
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
undef("add_map_tickmarks")
procedure add_map_tickmarks(wks:graphic,map:graphic,tmres:logical, \
                            lon_spacing,mlon_spacing,lat_spacing,\
                            mlat_spacing,point_outward)
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
    if(limit_mode.ne.get_enum_value("Satellite")) then
      getvalues map
        "mpMinLonF"    : min_lon
        "mpMaxLonF"    : max_lon
        "mpMinLatF"    : min_lat
        "mpMaxLatF"    : max_lat
        "mpCenterLonF" : center_lon
      end getvalues
    else
      getvalues map
        "mpLeftCornerLonF"  : min_lon
        "mpRightCornerLonF" : max_lon
        "mpLeftCornerLatF"  : min_lat
        "mpRightCornerLatF" : max_lat
        "mpCenterLonF"      : center_lon
      end getvalues
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

    gsnp_point_tickmarks_outward(tickmark,tmres,-1.,-1.,-1.,-1.,-1.,-1.,\
                                 point_outward)
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
; Function : get_lon_cyclic_point_default(data_list,res,ftype,keep)     ;
;                                                                       ;
; This function determines an appropriate default value for the special ;
; gsnAddCyclic resource, based on the data input and options.           ;
;                                                                       ;
; If res@gsnAddCyclic=True, then True is returned regardless of anything;
; else. If tfDoNDCOverlay is set to True, or if the calling function is ;
; not one of the gsn_csm_xxx_map functions, then False is returned.     ;
;                                                                       ;
; Otherwise, this function checks the given list of data variables to   ;
; see if they all have the *potential* for having a longitude cyclic    ;
; point added.                                                          ;
;                                                                       ;
; If a data variable doesn't contain the special lat2d/lon2d or         ; 
; lat1d/lon1d attributes, then it's assumed that this is a good         ;
; candidate for adding a cyclic point.                                  ;
;***********************************************************************;
undef("get_lon_cyclic_point_default")
function get_lon_cyclic_point_default(data,res,ftype[1]:string,keep[1]:logical)
local set_cyclic, nitems, n
begin
;---If gsnAddCyclic is set, then just return this
  if(res.and.isatt(res,"gsnAddCyclic"))
    if(keep) then
      return(get_res_value_keep(res,"gsnAddCyclic",False))
    else
      return(get_res_value(res,"gsnAddCyclic",False))
    end if
  end if

;---If tfDoNDCOverlay is set to True, then gsnAddCyclic shouldn't be True.
  if(get_res_value_keep(res,"tfDoNDCOverlay",False).or.ftype.ne."map") then
    return(False)
  end if
  
;
; Otherwise, set gsnAddCyclic True if not using lat2d/lon2d OR lat1d/lon1d.
; "data" can either be a single array, or a list of arrays.

  if(isnumeric(data)) then
    return(.not.(isatt(data,"lat2d").and.isatt(data,"lon2d")).or.\
                (isatt(data,"lat1d").and.isatt(data,"lon1d")))
  end if
;
; If *any* of the data items in the list have lat2d/lon2d or lat1d/lon1d 
; attached, then return True.
;
  if(typeof(data).eq."list") then
    nitems = ListCount(data)
    do n=0,nitems-1
      set_cyclic = .not.(isatt(data[n],"lat2d").and.isatt(data[n],"lon2d")).or.\
                        (isatt(data[n],"lat1d").and.isatt(data[n],"lon1d"))
      if(.not.set_cyclic) then
        return(False)
      end if
    end do
;
; If we get through the whole loop w/o encountering the lat/lon1d/2d
; attributes, then return True.
;
    return(True)
  end if
  return(False)
end

;***********************************************************************;
; Function : gsn_add_cyclic_point                                       ;
;                  data : numeric                                       ;
;                                                                       ;
; Add a cyclic point in "x" to the 2D array "data".                     ;
; For a lat/lon plot "x"  corresponds to "lon"                          ;
;                    "ny" corresponds to "nlat"                         ;
;                    "mx" corresponds to "mlon"                         ;
;                                                                       ;
; If a lat/lon coordinate doesn't exist, then the data is tested to see ;
; if it contains a lat2d/lon2d attribute, and uses this instead.        ;
;***********************************************************************;
undef("gsn_add_cyclic_point")
function gsn_add_cyclic_point(data:numeric)
local dims, newdata, ny, mx, mx1, range, delta, first_val, second_val, \
last_val
begin
;
; If the data is 1D, no cyclic point should be added.
;
    dims  = dimsizes(data)
    if(dimsizes(dims).eq.1) then
       return(data)
    end if

    ny      = dims(0)
    mx      = dims(1)
    mx1     = mx+1

    newdata = new((/ny, mx1/),typeof(data))
    newdata(:,0:mx-1) = data             ; pass everything
    newdata(:,mx)     = (/ data(:,0) /)  ; value only
;
; Only check the longitude values if we are dealing with a 1D coordinate
; array. It is possible that if we have a 2D longitudate "coordinate"
; array, it will not be 360 in range, so we don't want to check 2D arrays.
;
    res = False
    if(is_valid_latlon_coord(newdata,"x","lon",res)) then
      first_val  = newdata&$newdata!1$(0)
      second_val = newdata&$newdata!1$(1)
      last_val   = newdata&$newdata!1$(mx-1)
      delta      = second_val - first_val
      range      = last_val - first_val

      if(range.ge.360.) then
        print("gsn_add_cyclic: Warning: The range of your longitude coordinate")
        print("array is at least 360. No cyclic point will be added.")
        return(data)
      end if
      if(any((range+delta).lt.359.)) then
        print("gsn_add_cyclic: Warning: The range of your longitude data is not 360.")
        print("You may want to set the gsnAddCyclic resource to False to avoid a")
        print("warning message from the spline function.")
      end if
    else
      if(.not.(isatt(newdata,"lon2d").and.isatt(newdata,"lat2d"))) then
        return(newdata)
      else 
        if(.not.all(dimsizes(newdata@lon2d).eq.dimsizes(newdata@lat2d))) then
          print("gsn_add_cyclic: Warning: Your lat2d/lon2d attributes must have the same dimension sizes.")
        end if
      end if
    end if

    if(is_valid_latlon_coord(newdata,"x","lon",res)) then
      newdata&$newdata!1$(mx) = newdata&$newdata!1$(0) + 360
    else 
;
; Have to store lat2d/lon2d to a local variable, because NCL
; doesn't let you subscript attributes that are greater than
; one dimension.
;
      lon2d                 = newdata@lon2d
      lat2d                 = newdata@lat2d
      lon_dims              = dimsizes(lon2d)
      nlat                  = lon_dims(0)
      nlon                  = lon_dims(1)
      nlon1                 = nlon+1
      new_lon2d             = new((/nlat,nlon1/),typeof(lon2d))
      new_lat2d             = new((/nlat,nlon1/),typeof(lat2d))
      new_lon2d(:,0:nlon-1) = lon2d
      new_lon2d(:,nlon)     = lon2d(:,0)
      new_lat2d(:,0:nlon-1) = lat2d
      new_lat2d(:,nlon)     = lat2d(:,0)

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
undef("pop_tick_locs")
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
undef("pop_latlon_grid")
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
    tmplon(242:,190)  = default_fillvalue("float")
    tmplon@_FillValue = default_fillvalue("float")

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
; Function : mask_lambert_conformal                                     ;
;                 wks: graphic                                          ;
;               maplc: graphic                                          ;
;              minlat: numeric                                          ;
;              maxlat: numeric                                          ;
;              minlon: numeric                                          ;
;              maxlon: numeric                                          ;
;           maskoutln: logical                                          ;
;                 res: logical                                          ;
;                                                                       ;
;   Given a lambert conformal projection, and min/max lat/lon coords,   ;
;   this function will mask the map outside the boundaries defined by   ;
;   the coords. maskoutln determines whether the area will be outlined  ;
;   in the foreground color. "res" is an optional list of resources.    ;
;                                                                       ;
;   Note, due to the nature of Lambert Conformal plots, lon labels      ;
;   cannot be automatically drawn on this type of plot.                 ;
;                                                                       ;
;   Programming Note: The function expects longitude input data to      ;
;   range from -360:180E. If this is not the case, the function         ;
;   will alter the min/max to be in that range.                         ;
;                                                                       ;
;***********************************************************************;
undef("mask_lambert_conformal")
function mask_lambert_conformal(wks:graphic, maplc:graphic, minlat:numeric,\
                                maxlat:numeric, minlon:numeric, \
                                maxlon:numeric, maskoutln, res)
local pres, var_string, maplc, mpres, tmplat, vpx, vpy, vpw, vph, \
xlft, xrgt, ytop, ybot, nscirc, nncirc, nxpoly, nypoly, sxpoly, sypoly, \
var_string, scirc_xndc, scirc_yndc, ncirc_xndc, ncirc_yndc, mask_outline, \
xmnlnmnlt, ymnlnmnlt, xmnlnmxlt, ymnlnmxlt, \
xmxlnmnlt, ymxlnmnlt, xmxlnmxlt, ymxlnmxlt, is_nh
begin
;
; Some error checking.
;
  if(maxlat.gt.0.and.minlat.lt.0) then
    print("mask_lambert_conformal: warning: you are not authorized to specify.")
    print("  a maxlat that is above the equator and a minlat that is below")
    print("  the equator. No masking will take place.")
    return(maplc)
  end if

  if (minlon.gt.maxlon) then
     print("mask_lambert_conformal: warning: Minimum longitude is greated than")
     print("  maximum, subtracting 360 from minumum.")
     minlon = minlon - 360
     print("minlon = "+minlon+", maxlon = "+maxlon)
  end if
  if (minlat.gt.maxlat) then
     print("mask_lambert_conformal: warning: Minimum latitude is greater than")
     print("  maximum, swapping the two.")
     tmplat = minlat
     minlat = maxlat
     maxlat = tmplat
     print("minlat = "+minlat+", maxlat = "+maxlat)
  end if
  if (minlon.ge.180.and.maxlon.gt.180) then
     minlon = minlon - 360
     maxlon = maxlon - 360  
  end if
  if (minlon.lt.180.and.maxlon.ge.180) then
     minlon = minlon - 360
     maxlon = maxlon - 360  
  end if
;
; Set up list of map resources. The user should have already created a 
; lambert conformal map, but just in case  he/she didn't, it will be done
; here. Some of these resources may have already been set by the user, so
; be sure to use these values.
;
  meridian = minlon + (maxlon - minlon)/2.
  mpres = True
  if(isatt(res,"mpProjection")) then
    iproj = res@mpProjection
    if(.not.check_attr_enum_value(res,"mpProjection","LambertConformal")) then
      print("mask_lambert_conformal: warning: you are using a projection other")
      print("  than LambertConformal. Unexpected results will likely occur.")
    end if
    delete(iproj)
  end if

  mpres@mpProjection = get_res_value_keep(res,"mpProjection","LambertConformal")

  if(isatt(res,"mpLimitMode")) then
    need_to_set_limits = False
  else
    need_to_set_limits = True
    mpres@mpLimitMode  = "LatLon"
  end if
  mpres@mpLambertMeridianF     = get_res_value_keep(res, \
                                 "mpLambertMeridianF",meridian)
  if (minlat.lt.0) then
     mpres@mpLambertParallel1F = get_res_value_keep(res, \
                                 "mpLambertParallel1F",-0.001)
     mpres@mpLambertParallel2F = get_res_value_keep(res, \
                                 "mpLambertParallel2F",-89.999  )
  else
     mpres@mpLambertParallel1F = get_res_value_keep(res, \
                                 "mpLambertParallel1F", 0.001)
     mpres@mpLambertParallel2F = get_res_value_keep(res, \
                                 "mpLambertParallel2F",89.999  )
  end if
;
; Determine whether we are in the southern or northern hemisphere.
;
  if(minlat.ge.0) then
    is_nh = True
  else
    is_nh = False
  end if

;
; If the user hasn't already set the limits of the map, then set them
; here. Make sure there's some space around the area we want to mask.
;
  if(need_to_set_limits) then
    if(is_nh)
      mpres@mpMinLatF = max((/minlat-0.5,  0/))
      mpres@mpMaxLatF = min((/maxlat+0.5, 90/))
    else
      mpres@mpMinLatF = max((/minlat-0.5,-90/))
      mpres@mpMaxLatF = min((/maxlat+0.5,  0/))
    end if
    mpres@mpMinLonF = minlon-0.5
    mpres@mpMaxLonF = maxlon+0.5
  end if

;
; These draw order resources are necessary to make sure the map
; outlines, lat/lon lines, and perimeter gets drawn *after* the
; masking polygons are drawn.
;
  mpres@mpOutlineDrawOrder     = get_res_value_keep(res, \
                                 "mpOutlineDrawOrder","PostDraw")
  mpres@mpGridAndLimbDrawOrder = get_res_value_keep(res, \
                                 "mpGridAndLimbDrawOrder","PostDraw")
  mpres@mpPerimDrawOrder       = get_res_value_keep(res, \
                                 "mpPerimDrawOrder","PostDraw")

;
; This section is to get rid of some of the remnants that appear around the
; edges of the plot, even after the masking is done. To accomplish this,
; tickmarks are being turned on, but only the border is being drawn in the
; background color.  Since things like streamlines can *still* leave 
; remnants, even with this extra code, we have put in a hook to allow the
; user to specify the thickness of the border line.
; 
  mpres@pmTickMarkDisplayMode = "Always"
  mpres@tmXBLabelsOn          = False
  mpres@tmXTLabelsOn          = False
  mpres@tmYLLabelsOn          = False
  mpres@tmYRLabelsOn          = False
  mpres@tmYLOn                = False
  mpres@tmYROn                = False
  mpres@tmXBOn                = False
  mpres@tmXTOn                = False
  mpres@tmBorderLineColor     = "background"
  mpres@tmBorderThicknessF    = get_res_value_keep(res, \
                                "tmBorderThicknessF",2.0)
;
; Now that we've set all these resources, apply them to the map.
;
  attsetvalues(maplc,mpres)

;
; Thus begins the section for creating the two masking polygons.
; The polygons must be drawn in NDC, because trying to draw them in
; lat/lon space is not a good use of time and produces unpredictable
; results.
;
; Get viewport coordinates and calculate NDC positionsd of the
; four corners of the plot.
;
  getvalues maplc
    "vpXF"      : vpx
    "vpYF"      : vpy
    "vpWidthF"  : vpw
    "vpHeightF" : vph
  end getvalues

  xlft = vpx
  xrgt = vpx + vpw
  ytop = vpy
  ybot = vpy - vph

;
; Calculate NDC coordinates of four corners of area defined by min/max
; lat/lon coordinates.
;
  xmnlnmnlt = new(1,float)       ; (minlon,minlat)
  ymnlnmnlt = new(1,float)
  xmxlnmnlt = new(1,float)       ; (maxlon,minlat)
  ymxlnmnlt = new(1,float)
  xmnlnmxlt = new(1,float)       ; (minlon,maxlat)
  ymnlnmxlt = new(1,float)
  xmxlnmxlt = new(1,float)       ; (maxlon,maxlat)
  ymxlnmxlt = new(1,float)

  datatondc(maplc, tofloat(minlon), tofloat(minlat), xmnlnmnlt, ymnlnmnlt)
  datatondc(maplc, tofloat(maxlon), tofloat(minlat), xmxlnmnlt, ymxlnmnlt)
  datatondc(maplc, tofloat(minlon), tofloat(maxlat), xmnlnmxlt, ymnlnmxlt)
  datatondc(maplc, tofloat(maxlon), tofloat(maxlat), xmxlnmxlt, ymxlnmxlt)

;
; Calculate NDC coordinates of southern hemisphere semi-circle.
;
  nscirc     = 100
  scirc_xndc = new(nscirc,float) 
  scirc_yndc = new(nscirc,float) 
  scirc_lon  = new(nscirc,float) 
  scirc_lat  = new(nscirc,float) 
  scirc_lon  = fspan(maxlon,minlon,nscirc)
  scirc_lat  = minlat
  datatondc(maplc,scirc_lon,scirc_lat,scirc_xndc,scirc_yndc)

;
; Calculate NDC coordinates of northern hemisphere semi-circle.
;
  nncirc     = 100
  ncirc_xndc = new(nncirc,float) 
  ncirc_yndc = new(nncirc,float) 
  ncirc_lon  = new(nncirc,float) 
  ncirc_lat  = new(nncirc,float) 
  ncirc_lon  = fspan(maxlon,minlon,nncirc)
  ncirc_lat  = maxlat
  datatondc(maplc,ncirc_lon,ncirc_lat,ncirc_xndc,ncirc_yndc)

;
; Create two polygons in NDC space (northern and southern hemisphere),
; using all the coordinates we gathered above. The two polygons will be
; set differently depending on whether we're in the northern or 
; southern hemisphere.  Yes, we could do this with one polygon, but it's
; a little cleaner this way, trust me.
;
  if(is_nh) then
    nxpoly = new(nncirc+7,float)
    nypoly = new(nncirc+7,float)
    sxpoly = new(nscirc+5,float)
    sypoly = new(nscirc+5,float)
  else
    nxpoly = new(nncirc+5,float)
    nypoly = new(nncirc+5,float)
    sxpoly = new(nscirc+7,float)
    sypoly = new(nscirc+7,float)
  end if
;
; Define masking polygons for map that is in the northern hemisphere.
;
  if(is_nh) then
    nxpoly(0)          = xrgt
    nypoly(0)          = ymxlnmnlt
    nxpoly(1)          = xmxlnmnlt
    nypoly(1)          = ymxlnmnlt
    nxpoly(2:nncirc+1) = ncirc_xndc
    nypoly(2:nncirc+1) = ncirc_yndc
    nxpoly(nncirc+2)   = xmnlnmnlt
    nypoly(nncirc+2)   = ymnlnmnlt
    nxpoly(nncirc+3)   = xlft
    nypoly(nncirc+3)   = ymnlnmnlt
    nxpoly(nncirc+4)   = xlft
    nypoly(nncirc+4)   = ytop
    nxpoly(nncirc+5)   = xrgt
    nypoly(nncirc+5)   = ytop
    nxpoly(nncirc+6)   = xrgt
    nypoly(nncirc+6)   = ymxlnmnlt

    sxpoly(0)        = xrgt
    sypoly(0)        = ymxlnmnlt
    sxpoly(1:nscirc) = scirc_xndc
    sypoly(1:nscirc) = scirc_yndc
    sxpoly(nscirc+1) = xlft
    sypoly(nscirc+1) = ymnlnmnlt
    sxpoly(nscirc+2) = xlft
    sypoly(nscirc+2) = ybot
    sxpoly(nscirc+3) = xrgt
    sypoly(nscirc+3) = ybot
    sxpoly(nscirc+4) = xrgt
    sypoly(nscirc+4) = ymxlnmnlt
  else
;
; Define masking polygons for plot that is in the southern hemisphere.
;
    nxpoly(0)        = xrgt
    nypoly(0)        = ymxlnmxlt
    nxpoly(1:nncirc) = ncirc_xndc
    nypoly(1:nncirc) = ncirc_yndc
    nxpoly(nncirc+1) = xlft
    nypoly(nncirc+1) = ymnlnmxlt
    nxpoly(nncirc+2) = xlft
    nypoly(nncirc+2) = ytop
    nxpoly(nncirc+3) = xrgt
    nypoly(nncirc+3) = ytop
    nxpoly(nncirc+4) = xrgt
    nypoly(nncirc+4) = ymxlnmxlt

    sxpoly(0)          = xrgt
    sypoly(0)          = ymxlnmxlt
    sxpoly(1)          = xmxlnmxlt
    sypoly(1)          = ymxlnmxlt
    sxpoly(2:nscirc+1) = scirc_xndc
    sypoly(2:nscirc+1) = scirc_yndc
    sxpoly(nscirc+2)   = xmnlnmxlt
    sypoly(nscirc+2)   = ymnlnmxlt
    sxpoly(nscirc+3)   = xlft
    sypoly(nscirc+3)   = ymnlnmxlt
    sxpoly(nscirc+4)   = xlft
    sypoly(nscirc+4)   = ybot
    sxpoly(nscirc+5)   = xrgt
    sypoly(nscirc+5)   = ybot
    sxpoly(nscirc+6)   = xrgt
    sypoly(nscirc+6)   = ymxlnmxlt
  end if
;
; Attach the two polygons (and optionally, the outline polyline) 
; to the map.  Fill the polygons in the background color.
;
  pres             = True
  pres@gsFillColor = "background"

;
; Northern hemisphere polygon
;
  var_string         = unique_string("lamb_polygon")
  maplc@$var_string$ = gsn_add_primitive(wks,maplc,nxpoly,nypoly,True, \
                                         "polygon",pres)
;
; Southern hemisphere polygon
;
  var_string         = unique_string("lamb_polygon")
  maplc@$var_string$ = gsn_add_primitive(wks,maplc,sxpoly,sypoly,True, \
                                         "polygon",pres)
;
; For debugging purposes only.  This draws outlines around the two
; polygons, in case you need to see what areas are being masked.
;
;  pres@gsLineThicknessF = 3.0
;  pres@gsLineColor      = "red"
;  var_string            = unique_string("lamb_polyline")
;  maplc@$var_string$    = gsn_add_primitive(wks,maplc,nxpoly,nypoly,True, \
;                                            "polyline",pres)
;  pres@gsLineColor   = "blue"
;  var_string         = unique_string("lamb_polyline")
;  maplc@$var_string$ = gsn_add_primitive(wks,maplc,sxpoly,sypoly,True, \
;                                         "polyline",pres)
;
;
; Outline the area we are looking at (optional).
;
   if(maskoutln) then
     pres@gsLineColor      = rm_single_dims_no_meta(convert_color_to_rgba(wks,"foreground"))
     pres@gsLineThicknessF = 3.0
     var_string         = unique_string("lamb_polyline")
     outline_lon        = (/ minlon, maxlon, maxlon, minlon, minlon /)
     outline_lat        = (/ minlat, minlat, maxlat, maxlat, minlat /)
     maplc@$var_string$ = gsn_add_polyline(wks,maplc,outline_lon, \
                                                     outline_lat,pres)
  end if

  return(maplc)
end             


;***********************************************************************;
; Function : gsn_csm_blank_plot                                         ;
;               wks : workstation                                       ;
;               res : optional resources                                ;
;                                                                       ;
; This function creates a blank tickmark object that can be used for    ;
; drawing primitives. This function is similar to gsn_blank_plot        ;
; except the tickmarks point outward and the gsnXXXString resources are ;
; recognized.                                                           ;
;                                                                       ;
;***********************************************************************;
undef("gsn_csm_blank_plot")
function gsn_csm_blank_plot(wks:graphic,res:logical)
local calldraw, callframe, center_string, font_height, height, left_string, \
main_zone, major_length, maxbb, minor_length, point_outward, ratio, \
res2, right_string, scale, shape, subres, ticks, title, tmres, width, \
xfontf, xlength, xmlength, yfontf, ylength, ymlength
begin
  res2          = res    ; Copy of resources.

  point_outward = get_res_value(res2,"gsnTickMarksPointOutward",True)

  calldraw      = get_res_value(res2,"gsnDraw", True)
  callframe     = get_res_value(res2,"gsnFrame",False)
  shape         = get_res_value(res2,"gsnShape",False)
  scale         = get_res_value(res2,"gsnScale",shape)
  ticks         = get_res_value(res2,"pmTickMarkDisplayMode","Always")
  title         = get_res_value(res2,"pmTitleDisplayMode","Always")
  maxbb         = get_bb_res(res2)

; Check for existence of the left, center, and right subtitles.

  left_string   = new(1,logical)
  center_string = new(1,logical)
  right_string  = new(1,logical)

  check_for_subtitles(res2,left_string,center_string,right_string)
  if(left_string.or.center_string.or.right_string)
    main_zone = 4
  else
    main_zone = 3
  end if

  canvas = create "canvas" irregularPlotClass wks
    "pmTickMarkDisplayMode" : ticks
    "pmTitleDisplayMode"    : title
    "pmTitleZone"           : main_zone    ; Zone for main title
  end create

  attsetvalues_check(canvas,res2)

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

; Get title label sizes and tickmark lengths.
  getvalues canvas
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
; three top titles smaller.

  ratio = height/width
  if(ratio.gt.1) 
    ratio = 1./ratio
  end if
  if(ratio.gt.0.5)
    font_height  = 0.75 * font_height
    major_length = 0.75 * major_length
    minor_length = 0.75 * minor_length
  end if

  tmres = get_res_eq(res2,"tm")
  gsnp_point_tickmarks_outward(canvas,tmres,xlength,ylength,xmlength, \
                               ymlength,major_length,minor_length, \
                               point_outward)

; Set up three subtitles at top, if they exist.
  subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
  subres                  = True
  set_attr(subres,"txFontHeightF",font_height)
  add_subtitles(wks,canvas,left_string,center_string,\
                right_string,subres)

  draw_and_frame(wks,canvas,calldraw,callframe,False,maxbb)

  return(canvas)
end

;----------------------------------------------------------------------
; Returns the color(s) of X or Y reference line(s). If just one color
; is given and there are multiple reference lines, then propagate
; the value for each reference line.
;----------------------------------------------------------------------
undef("get_ref_line_color")
function get_ref_line_color(wks,reftype,nref,rfres)
local dims, rank, color_res1, color_res2
begin
  if(reftype.eq."y") then
    color_res1 = "gsnYRefLineColor"
    color_res2 = "gsnYRefLineColors"
  else if(reftype.eq."x") then
    color_res1 = "gsnXRefLineColor"
    color_res2 = "gsnXRefLineColors"
  else
    print("get_ref_line_color: fatal, invalid type for reference line color.")
    return
  end if
  end if
  if(isatt(rfres,color_res2))
    ref_line_color = convert_color_to_rgba(wks,rfres@$color_res2$)
  else if(isatt(rfres,color_res1))
    ref_line_color = convert_color_to_rgba(wks,rfres@$color_res1$)
  else
    ref_line_color = convert_color_to_rgba(wks,"foreground")
  end if
  end if
  dims = dimsizes(ref_line_color)
  rank = dimsizes(dims)
  if(rank.ne.2.or.(dims(0).ne.nref.and.dims(0).ne.1)) then
    print("get_ref_line_color: Warning: you must specify either one reference line color")
    print("      or the same number of colors as there are reference lines.")
    print("      Will use the foreground color.") 
    ref_line_color := convert_color_to_rgba(wks,"foreground")
  end if

  return(conform_dims((/nref,4/),ref_line_color,-1))
;;  if(dims(0).eq.1.and.nref.gt.1) then
;;    return(conform_dims((/nref,4/),ref_line_color(0,:),1))
;;  else
;;    return(ref_line_color)
;;  end if
end

;----------------------------------------------------------------------
; Returns the pattern(s) of X or Y reference line(s). If just one
; pattern is given and there are multiple reference lines, then 
; propagate the value for each reference line.
; For use by gsn_csm_xy.
;----------------------------------------------------------------------
undef("get_ref_line_pattern")
function get_ref_line_pattern(wks,reftype,nref,rfres)
begin
  if(reftype.eq."y") then
    pattern_res1 = "gsnYRefLineDashPattern"
    pattern_res2 = "gsnYRefLineDashPatterns"
  else if(reftype.eq."x") then
    pattern_res1 = "gsnXRefLineDashPattern"
    pattern_res2 = "gsnXRefLineDashPatterns"
  else
    print("get_ref_line_pattern: fatal, invalid type for reference line pattern.")
    return
  end if
  end if
  if(isatt(rfres,pattern_res1)) then
    if(dimsizes(rfres@$pattern_res1$).gt.1) then
      print("get_ref_line_pattern: Warning: only one pattern should be specified with " + pattern_res1)
      print("                      Use " + pattern_res2 + " if multiple patterns desired.")
    end if
    ref_line_pattern := rfres@$pattern_res1$(0)
  else if(isatt(rfres,pattern_res2))
    ref_line_pattern := rfres@$pattern_res2$
  else  
    ref_line_pattern = 0
  end if
  end if

  dims = dimsizes(ref_line_pattern)
  rank = dimsizes(dims)
  if(.not.isscalar(ref_line_pattern).and.rank.ne.1.and.dimsizes(ref_line_pattern).ne.nref) then
    print("get_ref_line_pattern: Warning: you must specify either one reference line dash pattern")
    print("      or the same number of dash patterns as there are reference lines.")
    print("      Will use a solid line.") 
    ref_line_pattern := 0
  end if
  return(conform_dims(nref,ref_line_pattern,-1))
end

;----------------------------------------------------------------------
; Returns a thickness or list of thicknesses for a reference line.
; If just one thickness is given and there are multiple reference 
; lines, then propagate the value for each reference line.
; For use by gsn_csm_xy.
;----------------------------------------------------------------------
undef("get_ref_line_thickness")
function get_ref_line_thickness(wks,reftype,nref,rfres)
local dims, rank
begin
  if(reftype.eq."y") then
    thickness_res1 = "gsnYRefLineThicknessF"
    thickness_res2 = "gsnYRefLineThicknesses"
  else if(reftype.eq."x") then
    thickness_res1 = "gsnXRefLineThicknessF"
    thickness_res2 = "gsnXRefLineThicknesses"
  else
    print("get_ref_line_thickness: fatal, invalid type for reference line thickness.")
    return
  end if
  end if
  if(isatt(rfres,thickness_res1)) then
    if(dimsizes(rfres@$thickness_res1$).gt.1) then
      print("get_ref_line_thickness: Warning: only one thickness should be specified with " + thickness_res1)
      print("                        Use " + thickness_res2 + " if multiple thicknesses desired.")
    end if
    ref_line_thickness = rfres@$thickness_res1$(0)
  else if(isatt(rfres,thickness_res2)) then
    ref_line_thickness = rfres@$thickness_res2$
  else
    ref_line_thickness = 1.0
  end if
  end if
  dims = dimsizes(ref_line_thickness)
  rank = dimsizes(dims)
  if(.not.isscalar(ref_line_thickness).and.rank.ne.1.and.dimsizes(ref_line_thickness).ne.nref) then
    print("get_ref_line_thickness: Warning: you must specify either one reference line thickness")
    print("      or the same number of dash thicknesss as there are reference lines.")
    print("      Will use a solid line.") 
    ref_line_thickness := 1.0
  end if
  return(conform_dims(nref,ref_line_thickness,-1))
end

;----------------------------------------------------------------------
; Returns a color or list of colors for filling curves or bars
; above (to the right) or below (to the left) a given ref line or 
; series of lines. If there are a series of lines, but only single
; colors, then the single color will be propagated to all lines.
;----------------------------------------------------------------------
undef("get_ref_line_fill_color")
function get_ref_line_fill_color(wks,location[1]:string,ncurves,nref,rfres)
local tmpdims, color_res, msg_color
begin
  if(location.eq."above") then
    color_res = "gsnAboveYRefLineColor"
  else if(location.eq."below") then
    color_res = "gsnBelowYRefLineColor"
  else if(location.eq."right") then
    color_res = "gsnRightXRefLineColor"
  else if(location.eq."left") then
    color_res = "gsnLeftXRefLineColor"
  else
    print("get_ref_line_fill_color: fatal, invalid type for reference fill color.")
    return
  end if
  end if
  end if
  end if

  msg_color = new((/1,4/),float)

  if(any(isatt(rfres,(/"gsnXYBarChartPatterns", \
                       "gsnXYBarChartPatterns2", \
                       "gsnAboveYRefLineBarPatterns", \
                       "gsnBelowYRefLineBarPatterns", \
                       "gsnRightXRefLineBarPatterns", \
                       "gsnLeftXRefLineBarPatterns"/)))) then
    ref_curve_fill_color = convert_color_to_rgba(wks,"foreground")
  else
;
; You need transparent here because the NCL default for polygon
; fill is the foreground color.
;
    ref_curve_fill_color = convert_color_to_rgba(wks,"transparent")
  end if
  if(isatt(rfres,color_res))
    ref_curve_fill_color := convert_color_to_rgba(wks,rfres@$color_res$)
    tmpdims = dimsizes(ref_curve_fill_color)
    if(.not.(tmpdims(0).eq.1.or.tmpdims(0).eq.nref)) then
      print("get_ref_line_fill_color: Fatal: there must either be an above/below/right/left fill color")
      print("      for every reference line, or one global fill color specified.")
      print("      No fill will be done.")
      ref_curve_fill_color := msg_color
    end if
  end if
;
; If ref_curve_fill_on is True, and we have more than one curve, then
; there should be the same number of reference lines as curves.
;
  if(isvar("ref_curve_fill_color")) then
    if(.not.any(ismissing(ref_curve_fill_color)).and.ncurves.gt.1.and.\
       ncurves.ne.nref.and.nref.ne.1)
      print("get_ref_line_fill_color: Fatal: if you plan to fill above/below/right/left reference lines,")
      print("      then the number of reference lines must either equal the number")
      print("      of curves or be a scalar.")
      print("      No fill will be done.")
      ref_curve_fill_color := msg_color
    end if
  else
    ref_curve_fill_color := msg_color
  end if
  dims := dimsizes(ref_curve_fill_color)
  rank = dimsizes(dims)
  if(rank.ne.2.or.(dims(0).ne.nref.and.dims(0).ne.1)) then
    print("get_ref_line_fill_color: Warning: you must specify either one above/below/right/left fill color")
    print("      or the same number of colors as there are reference lines.")
    print("      Will use the foreground color.") 
    ref_curve_fill_color := convert_color_to_rgba(wks,"foreground")
  end if

  return(conform_dims((/max((/ncurves,nref/)),4/),ref_curve_fill_color,-1))
;;  if(dims(0).eq.1.and.any((/ncurves,nref/).gt.1)) then
;;    return(conform_dims((/max((/ncurves,nref/)),4/),ref_curve_fill_color(0,:),1))
;;  else
;;    return(ref_curve_fill_color)
;;  end if
end

;**********************************************************************
; This function sets a bunch of variables for drawing an X or Y 
; reference line and/or filling curves above/below or to the right/left
; of a reference line.
;**********************************************************************
undef("get_ref_line_options")
function get_ref_line_options(wks,x,y,reftype[1]:string,ncurves,rfres)
local nref, imsg, fmsg, tmpdims, nref, set_ref_line_color
begin
  imsg = new(1,integer)
  fmsg = new(1,float)
  cmsg = new(4,float)
  nref                   = 0
  ref_line               = fmsg
  ref_line_on            = False
  ref_line_color         = cmsg
  ref_line_pattern       = imsg
  ref_line_thickness     = fmsg
  ref_grtr_curve_fill_on = False
  ref_less_curve_fill_on = False
  ref_fill_color_g       = cmsg   ; greater than
  ref_fill_color_l       = cmsg   ; less than

;
; If neither gsnYRefLine or gsnXRefLine are set, but gsnXYBarChart has 
; been set, then assume the user wants a Y reference line, and return 
; a default value for Y ref line.
;
  bar_chart        = get_res_value_keep(rfres,"gsnXYBarChart",False)
  bar_outline_only = get_res_value_keep(rfres,"gsnXYBarChartOutlineOnly",False)
  if(.not.isatt(rfres,"gsnYRefLine").and..not.isatt(rfres,"gsnXRefLine")) then
    if(reftype.eq."y".and.bar_chart.and..not.bar_outline_only) then
      ref_line = get_res_value_keep(rfres,"trYMinF",min(y))
;      print("get_ref_line_options: warning: you've set gsnXYBarChart=True, but haven't")
;      print("    specified an X or Y reference line. Will assume a default Y reference")
;      print("    line of " + ref_line)
      nref               := 1
      ref_line_on        = True
      ref_line_color     := convert_color_to_rgba(wks,"foreground")
      ref_line_pattern   := 0
      ref_line_thickness := 1.
    end if
    return([/ref_line,ref_line_on,ref_line_color,ref_line_pattern,\
             ref_line_thickness,ref_grtr_curve_fill_on,\
             ref_less_curve_fill_on,ref_fill_color_g,ref_fill_color_l/])
  end if
  if(bar_outline_only.or.(reftype.eq."x".and..not.isatt(rfres,"gsnXRefLine").or.\
                          reftype.eq."y".and..not.isatt(rfres,"gsnYRefLine"))) then
    if( (reftype.eq."y".and.isatt(rfres,"gsnYRefLine")).or.\
        (reftype.eq."x".and.isatt(rfres,"gsnXRefLine"))) then
      ref_line_on        = True
      ref_line           := get_res_value_keep(rfres,"gsn" + str_upper(reftype)+"RefLine",0)
      nref               = dimsizes(ref_line)
      ref_line_color     := get_ref_line_color(wks,reftype,nref,rfres)
      ref_line_pattern   := get_ref_line_pattern(wks,reftype,nref,rfres)
      ref_line_thickness := get_ref_line_thickness(wks,reftype,nref,rfres)
    end if
    return([/ref_line,ref_line_on,ref_line_color,ref_line_pattern,\
             ref_line_thickness,ref_grtr_curve_fill_on,\
             ref_less_curve_fill_on,ref_fill_color_g,ref_fill_color_l/])
  end if

  if(reftype.eq."y") then
    fill_string_g = "above"
    fill_string_l = "below"
  else
    fill_string_g = "right"
    fill_string_l = "left"
  end if
  ref_line_on = True
  ref_line    := get_res_value_keep(rfres,"gsn" + str_upper(reftype)+"RefLine",0)
  nref         = dimsizes(ref_line)
  ref_line_color       := get_ref_line_color(wks,reftype,nref,rfres)
  ref_line_pattern     := get_ref_line_pattern(wks,reftype,nref,rfres)
  ref_line_thickness   := get_ref_line_thickness(wks,reftype,nref,rfres)
  ref_fill_color_g     := get_ref_line_fill_color(wks,fill_string_g,ncurves,\
                           nref,rfres)
  ref_fill_color_l     := get_ref_line_fill_color(wks,fill_string_l,ncurves,\
                           nref,rfres)
  ref_grtr_curve_fill_on = .not.any(ismissing(ref_fill_color_g))
  ref_less_curve_fill_on = .not.any(ismissing(ref_fill_color_l))

  return([/ref_line,ref_line_on,ref_line_color,ref_line_pattern,\
           ref_line_thickness,ref_grtr_curve_fill_on,\
           ref_less_curve_fill_on,ref_fill_color_g,\
           ref_fill_color_l/])
end

;**********************************************************************
; This function sets a bunch of variables for determining how to
; fill bars in an XY plot, when gsnXYBarChart has been set to True.
;  The "bres" variable may have some attributes removed too.;
;
; Three resources can be used to specify fill colors, in this order:
;
;     1. gsnAbove/BelowYRefLineBarColors
;     2. gsnXYBarChartColors
;     3. gsnXYBarChartColors2
;     4. gsnAbove/BelowYRefLineColor
;
; 1. gsnAbove/BelowYRefLineBarColors
;
; If you specify either one of these resources, then the top bars 
; will be colored in the gsnAboveYRefLineBarColors, and the
; below bars in gsnBelowYRefLineBarColors. The colors will repeat
; if you specify fewer colors than bars.
;
; For example, if you have 10 "up" bars and 3 "above" colors specified,
; and 8 "below" bars and 3 "below" colors specified:
;
;   up bars #1-3:  above colors 1-3    down bars #1-3: below colors 1-3
;   up bars #4-6:  above colors 1-3    down bars #4-6  below colors 1-3
;   up bars #7-9:  above colors 1-3    down bars #7-8  below colors 1-2
;   up bar  #10:   above color 1
; 
; 2. gsnXYBarChartColors
;
; If you specify this resource, then then up bars will cycle through
; these colors independently of the down bars.
;
; 3. gsnXYBarChartColors2
;
; If you specify this resource, then the colors will be applied 
; to each consecutive bar, regardless if it is up or down.
;
; By the way, the same logic applies to the gsnXYBarChart*Pattern* 
; resources.
;
; 4. gsnAbove/BelowYRefLineColor can also be used to color bars,
;    but mostly for backwards compatibility.
;;**********************************************************************
undef("get_bar_colors")
function get_bar_colors(wks,type_ref[1]:string,bres,bar_list)
local bac_rank, bbc_rank, mesg_color
begin
  ngrtr = bar_list[6]
  nless = bar_list[7]
  indgrtr = bar_list[8]
  indless = bar_list[9]
  dims = dimsizes(indgrtr)
  ncurves = dims(0)
  nptsbar = dims(1)
  if(any(isatt(bres,(/"gsnXYBarChartPatterns", \
                      "gsnXYBarChartPatterns2", \
                      "gsnLeftXRefLineBarPatterns", \
                      "gsnRightXRefLineBarPatterns",\
                      "gsnAboveYRefLineBarPatterns", \
                      "gsnBelowYRefLineBarPatterns"/)))) then
    def_rgba := convert_color_to_rgba(wks,"foreground")
  else     
    def_rgba := convert_color_to_rgba(wks,"transparent")
  end if
  grt_colors_set = False
  lss_colors_set = False
  def_grt_colors = conform_dims((/ncurves,nptsbar,4/),def_rgba(0,:),2)
  def_lss_colors = conform_dims((/ncurves,nptsbar,4/),def_rgba(0,:),2)

  if(type_ref.eq."y") then
    grt_res_name = "gsnAboveYRefLineBarColors"
    lss_res_name = "gsnBelowYRefLineBarColors"
  else
    grt_res_name = "gsnRightXRefLineBarColors"
    lss_res_name = "gsnLeftXRefLineBarColors"
  end if
  if(isatt(bres,grt_res_name)) then
    tmp_rgba  := convert_color_to_rgba(wks,bres@$grt_res_name$)
    rgba_dims := dimsizes(tmp_rgba)
    rgba_rank = dimsizes(rgba_dims)
    if((rgba_rank.ne.2.and.rgba_rank.ne.3).or.(rgba_rank.eq.3.and.rgba_dims(0).ne.ncurves)) then
      print("get_bar_colors: " + grt_res_name + " must be a 1D array")
      print("    of color indexes, color names, or an RGB/A array. Will ignore.")
      return([/def_grt_colors,def_lss_colors/])
    end if
    ncol = rgba_dims(rgba_rank-2)
    grt_colors = new((/ncurves,nptsbar,4/),float)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        ci = ii % ncol
        if(rgba_rank.eq.2) then
          grt_colors(nc,ii,:) = tmp_rgba(ci,:)
        else
          grt_colors(nc,ii,:) = tmp_rgba(nc,ci,:)
        end if
      end do
    end do
    grt_colors_set = True
  end if  

  if(isatt(bres,lss_res_name)) then
    tmp_rgba  := convert_color_to_rgba(wks,bres@$lss_res_name$)
    rgba_dims := dimsizes(tmp_rgba)
    rgba_rank = dimsizes(rgba_dims)
    if((rgba_rank.ne.2.and.rgba_rank.ne.3).or.(rgba_rank.eq.3.and.rgba_dims(0).ne.ncurves)) then
      print("get_bar_colors: " + lss_re_name + " must be a 1D array")
      print("    of color indexes, color names, or an RGB/A array. Will ignore.")
      return([/def_grt_colors,def_lss_colors/])
    end if
    ncol = rgba_dims(rgba_rank-2)
    lss_colors = new((/ncurves,nptsbar,4/),float)
    do nc = 0,ncurves-1
      do ii = 0,nless(nc)/5-1
        ci = ii % ncol
        if(rgba_rank.eq.2) then
          lss_colors(nc,ii,:) = tmp_rgba(ci,:)
        else
          lss_colors(nc,ii,:) = tmp_rgba(nc,ci,:)
        end if
      end do
    end do
    lss_colors_set = True
  end if  
;
; gsnXYBarChartColors will apply independently to both the greater
; than and less than ref line bars. bhat is, if you give this resource
; the colors "red", "green", and "blue", then all the colors > the ref
; line will cycle b/w these three colors, and independently, the
; colors < the ref line will cycle b/w these colors. 
;
; If you want to specify colors consecutively for each bar, not caring 
; whether it is > or < the ref line, use the "gsnXYBarChartColors2" resource.
;
  if(.not.grt_colors_set.and..not.lss_colors_set.and.isatt(bres,"gsnXYBarChartColors")) then
    tmp_rgba := convert_color_to_rgba(wks,bres@gsnXYBarChartColors)
    rgba_dims := dimsizes(tmp_rgba)
    rgba_rank = dimsizes(rgba_dims)
    if((rgba_rank.ne.2.and.rgba_rank.ne.3).or.(rgba_rank.eq.3.and.rgba_dims(0).ne.ncurves)) then
      print("get_bar_colors: gsnXYBarChartColors must be a 1D array")
      print("    of color indexes, color names, or an RGB/A array. Will ignore.")
      return([/def_grt_colors,def_lss_colors/])
    end if
    ncol = rgba_dims(rgba_rank-2)
    grt_colors = new((/ncurves,nptsbar,4/),float)
    lss_colors = new((/ncurves,nptsbar,4/),float)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        ci = ii % ncol
        if(rgba_rank.eq.2) then
          grt_colors(nc,ii,:) = tmp_rgba(ci,:)
        else
          grt_colors(nc,ii,:) = tmp_rgba(nc,ci,:)
        end if
      end do
      do ii = 0,nless(nc)/5-1
        ci = ii % ncol
        if(rgba_rank.eq.2) then
          lss_colors(nc,ii,:) = tmp_rgba(ci,:)
        else
          lss_colors(nc,ii,:) = tmp_rgba(nc,ci,:)
        end if
      end do
    end do
    grt_colors_set = True
    lss_colors_set = True
  end if

  if(.not.grt_colors_set.and..not.lss_colors_set.and.isatt(bres,"gsnXYBarChartColors2")) then
    tmp_rgba := convert_color_to_rgba(wks,bres@gsnXYBarChartColors2)
    rgba_dims := dimsizes(tmp_rgba)
    rgba_rank = dimsizes(rgba_dims)
    if((rgba_rank.ne.2.and.rgba_rank.ne.3).or.(rgba_rank.eq.3.and.rgba_dims(0).ne.ncurves)) then
      print("get_bar_colors: gsnXYBarChartColors2 must be a 1D array")
      print("    of color indexes, color names, or an RGB/A array. Will ignore.")
      return([/def_grt_colors,def_lss_colors/])
    end if
    ncol = rgba_dims(rgba_rank-2)
    grt_colors = new((/ncurves,nptsbar,4/),float)
    lss_colors = new((/ncurves,nptsbar,4/),float)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        ci = indgrtr(nc,ii) % ncol
        if(rgba_rank.eq.2) then
          grt_colors(nc,ii,:) = tmp_rgba(ci,:)
        else
          grt_colors(nc,ii,:) = tmp_rgba(nc,ci,:)
        end if
      end do
      do ii = 0,nless(nc)/5-1
        ci = indless(nc,ii) % ncol
        if(rgba_rank.eq.2) then
          lss_colors(nc,ii,:) = tmp_rgba(ci,:)
        else
          lss_colors(nc,ii,:) = tmp_rgba(nc,ci,:)
        end if
      end do
    end do
    grt_colors_set = True
    lss_colors_set = True
  end if

  if(type_ref.eq."y") then
    grt_res_name = "gsnAboveYRefLineColor"
    lss_res_name = "gsnBelowYRefLineColor"
  else
    grt_res_name = "gsnRightXRefLineColor"
    lss_res_name = "gsnLeftXRefLineColor"
  end if

  if(.not.grt_colors_set.and.isatt(bres,grt_res_name)) then
    tmp_rgba := convert_color_to_rgba(wks,bres@$grt_res_name$)
    rgba_dims = dimsizes(tmp_rgba)
    rgba_rank = dimsizes(rgba_dims)
    if(rgba_rank.ne.2.or.(rgba_rank.eq.2.and.rgba_dims(0).ne.ncurves.and.rgba_dims(0).ne.1)) then
      print("get_bar_colors: " + grt_res_name + " must be a 1D array")
      print("    of color indexes, color names, or an RGB/A array. Will ignore.")
      return([/def_grt_colors,def_lss_colors/])
    end if
    grt_colors = conform_dims((/ncurves,nptsbar,4/),tmp_rgba,(/0,2/))
    grt_colors_set = True
  end if  

  if(.not.lss_colors_set.and.isatt(bres,lss_res_name)) then
    tmp_rgba := convert_color_to_rgba(wks,bres@$lss_res_name$)
    rgba_dims = dimsizes(tmp_rgba)
    rgba_rank = dimsizes(rgba_dims)
    if(rgba_rank.ne.2.or.(rgba_rank.eq.2.and.rgba_dims(0).ne.ncurves.and.rgba_dims(0).ne.1)) then
      print("get_bar_colors: " + lss_res_name + " must be a 1D array")
      print("    of color indexes, color names, or an RGB/A array. Will ignore.")
      return([/def_grt_colors,def_lss_colors/])
    end if
    lss_colors = conform_dims((/ncurves,nptsbar,4/),tmp_rgba,(/0,2/))
    lss_colors_set = True
  end if  

  if(.not.grt_colors_set) then
    grt_colors = def_grt_colors
  end if
  if(.not.lss_colors_set) then
    lss_colors = def_lss_colors
  end if

  return([/grt_colors,lss_colors/])
end

;**********************************************************************
; See description above for get_bar_colors

; This function sets a bunch of variables for determining how to
; pattern fill bars in an XY plot, when gsnXYBarChart has been set 
; to True.
;
; Three resources can be used to specify fill colors, in this order:
;
;     1. gsnAbove/BelowYRefLineBarPatterns
;     2. gsnXYBarChartPatterns
;     3. gsnXYBarChartPatterns2
;**********************************************************************
undef("get_bar_patterns")
function get_bar_patterns(bres,type_ref[1]:string,bar_list)
local ngrtr,nless,indgrtr,indless,dims,ncurves,nptsbar,tmp_pattrn,\
pttrn_dims,pttrn_rank,pi,ii,nc,res_name,grt_res_name,lss_res_name
begin
  ngrtr = bar_list[6]
  nless = bar_list[7]
  indgrtr = bar_list[8]
  indless = bar_list[9]
  dims = dimsizes(indgrtr)
  ncurves = dims(0)
  nptsbar = dims(1)
  def_grt_patterns = conform_dims((/ncurves,nptsbar/),0,-1)   ; default to solid fill (no
  def_lss_patterns = conform_dims((/ncurves,nptsbar/),0,-1)   ;    patterns)
  grt_patterns_set = False
  lss_patterns_set = False

  if(type_ref.eq."y") then
    grt_res_name = "gsnAboveYRefLineBarPatterns"
    lss_res_name = "gsnBelowYRefLineBarPatterns"
  else
    grt_res_name = "gsnRightXRefLineBarPatterns"
    lss_res_name = "gsnLeftXRefLineBarPatterns"
  end if
  if(isatt(bres,grt_res_name)) then
    tmp_pttrn := bres@$grt_res_name$
    pttrn_dims := dimsizes(tmp_pttrn)
    pttrn_rank = dimsizes(pttrn_dims)
    if((pttrn_rank.ne.1.and.pttrn_rank.ne.2).or.(pttrn_rank.eq.2.and.pttrn_dims(0).ne.ncurves)) then
      print("get_bar_patterns: error: " + grt_res_name + " must either")
      print("     be 1D, or 2D where the first dimension is equal to the number")
      print("     of Y reference lines or curves. Won't pattern fill any bars.")
      return([/def_grt_patterns,def_lss_patterns/])
    end if
    npat = pttrn_dims(pttrn_rank-1)
    grt_patterns = new((/ncurves,nptsbar/),integer)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        pi = ii % npat
        if(pttrn_rank.eq.1) then
          grt_patterns(nc,ii) = tmp_pttrn(pi)
        else
          grt_patterns(nc,ii) = tmp_pttrn(nc,pi)
        end if
      end do
    end do
    grt_patterns_set = True
  end if

  if(isatt(bres,lss_res_name)) then
    tmp_pttrn := bres@$lss_res_name$
    pttrn_dims := dimsizes(tmp_pttrn)
    pttrn_rank = dimsizes(pttrn_dims)
    if((pttrn_rank.ne.1.and.pttrn_rank.ne.2).or.(pttrn_rank.eq.2.and.pttrn_dims(0).ne.ncurves)) then
      print("get_bar_patterns: error: " + lss_res_name + " must either")
      print("     be 1D, or 2D where the first dimension is equal to the number")
      print("     of Y reference lines or curves. Won't pattern fill any bars.")
      return([/def_grt_patterns,def_lss_patterns/])
    end if
    npat = pttrn_dims(pttrn_rank-1)
    lss_patterns = new((/ncurves,nptsbar/),integer)
    do nc = 0,ncurves-1
      do ii = 0,nless(nc)/5-1
        pi = ii % npat
        if(pttrn_rank.eq.1) then
          lss_patterns(nc,ii) = tmp_pttrn(pi)
        else
          lss_patterns(nc,ii) = tmp_pttrn(nc,pi)
        end if
      end do
    end do
    lss_patterns_set = True
  end if

  res_name = "gsnXYBarChartPatterns"
  if(.not.grt_patterns_set.and..not.lss_patterns_set.and.isatt(bres,res_name)) then
    tmp_pttrn := bres@$res_name$
    pttrn_dims := dimsizes(tmp_pttrn)
    pttrn_rank = dimsizes(pttrn_dims)
    if((pttrn_rank.ne.1.and.pttrn_rank.ne.2).or.(pttrn_rank.eq.2.and.pttrn_dims(0).ne.ncurves)) then
      print("get_bar_patterns: error: " + res_name + " must either")
      print("     be 1D, or 2D where the first dimension is equal to the number")
      print("     of Y reference lines or curves. Won't pattern fill any bars.")
      return([/def_grt_patterns,def_lss_patterns/])
    end if
    npat = pttrn_dims(pttrn_rank-1)
    grt_patterns = new((/ncurves,nptsbar/),integer)
    lss_patterns = new((/ncurves,nptsbar/),integer)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        pi = ii % npat
        if(pttrn_rank.eq.1) then
          grt_patterns(nc,ii) = tmp_pttrn(pi)
        else
          grt_patterns(nc,ii) = tmp_pttrn(nc,pi)
        end if
      end do
      do ii = 0,nless(nc)/5-1
        pi = ii % npat
        if(pttrn_rank.eq.1) then
          lss_patterns(nc,ii) = tmp_pttrn(pi)
        else
          lss_patterns(nc,ii) = tmp_pttrn(nc,pi)
        end if
      end do
    end do
    grt_patterns_set = True
    lss_patterns_set = True
  end if

  res_name = "gsnXYBarChartPatterns2"
  if(.not.grt_patterns_set.and..not.lss_patterns_set.and.isatt(bres,res_name)) then
    tmp_pttrn := bres@$res_name$
    pttrn_dims := dimsizes(tmp_pttrn)
    pttrn_rank = dimsizes(pttrn_dims)
    if((pttrn_rank.ne.1.and.pttrn_rank.ne.2).or.(pttrn_rank.eq.2.and.pttrn_dims(0).ne.ncurves)) then
      print("get_bar_patterns: error: " + res_name + " must either")
      print("     be 1D, or 2D where the first dimension is equal to the number")
      print("     of Y reference lines or curves. Won't pattern fill any bars.")
      return([/def_grt_patterns,def_lss_patterns/])
    end if
    npat = pttrn_dims(pttrn_rank-1)
    grt_patterns = new((/ncurves,nptsbar/),integer)
    lss_patterns = new((/ncurves,nptsbar/),integer)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        pi = indgrtr(nc,ii) % npat
        if(pttrn_rank.eq.1) then
          grt_patterns(nc,ii) = tmp_pttrn(pi)
        else
          grt_patterns(nc,ii) = tmp_pttrn(nc,pi)
        end if
      end do
      do ii = 0,nless(nc)/5-1
        pi = indless(nc,ii) % npat
        if(pttrn_rank.eq.1) then
          lss_patterns(nc,ii) = tmp_pttrn(pi)
        else
          lss_patterns(nc,ii) = tmp_pttrn(nc,pi)
        end if
      end do
    end do
    grt_patterns_set = True
    lss_patterns_set = True
  end if

  if(.not.grt_patterns_set.and..not.lss_patterns_set) then
    return([/def_grt_patterns,def_lss_patterns/])
  end if

  return([/grt_patterns,lss_patterns/])
end


;**********************************************************************
; This function returns a list of scale factors for bar filling
; depending on special "gsn" resources.
;
; It was updated in NCL V6.4.0 to contain the same type as resources
; that are available for fill patterns (gsnXYBarChartFillScales and
; gsnXYBarChartFillScales2)
;**********************************************************************
undef("get_bar_scales")
function get_bar_scales(wks,type_ref[1]:string,bres,bar_list)
local ngrtr,nless,indgrtr,indless,ncurves,nptsbar,tmp_scale,
def_grt_scales,def_lss_scales,grt_scales_set,lss_scales_set,\
scales,dims,rank,nscl
begin
  ngrtr = bar_list[6]
  nless = bar_list[7]
  indgrtr = bar_list[8]
  indless = bar_list[9]
  dims = dimsizes(indgrtr)
  ncurves = dims(0)
  nptsbar = dims(1)
  grt_scales_set = False
  lss_scales_set = False
  def_grt_scales = conform_dims((/ncurves,nptsbar/),1.0,-1)
  def_lss_scales = conform_dims((/ncurves,nptsbar/),1.0,-1)

  if(type_ref.eq."y") then
    grt_res_name = "gsnAboveYRefLineBarFillScales"
    lss_res_name = "gsnBelowYRefLineBarFillScales"
  else
    grt_res_name = "gsnRightXRefLineBarFillScales"
    lss_res_name = "gsnLeftXRefLineBarFillScales"
  end if

  if(isatt(bres,grt_res_name)) then
    tmp_scale := bres@$grt_res_name$
    scale_dims := dimsizes(tmp_scale)
    scale_rank = dimsizes(scale_dims)
    if((scale_rank.ne.1.and.scale_rank.ne.2).or.(scale_rank.eq.2.and.scale_dims(0).ne.ncurves)) then
      print("get_bar_scales: error: " + grt_res_name + " must either")
      print("     be 1D, or 2D where the first dimension is equal to the number")
      print("     of Y reference lines or curves. Will return a default of 1.")
      return([/def_grt_scales,def_lss_scales/])
    end if
    nscl = scale_dims(scale_rank-1)
    grt_scales = new((/ncurves,nptsbar/),float)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        si = ii % nscl
        if(scale_rank.eq.1) then
          grt_scales(nc,ii) = tmp_scale(si)
        else
          grt_scales(nc,ii) = tmp_scale(nc,si)
        end if
      end do
    end do
    grt_scales_set = True
  end if

  if(isatt(bres,lss_res_name)) then
    tmp_scale := bres@$lss_res_name$
    scale_dims := dimsizes(tmp_scale)
    scale_rank = dimsizes(scale_dims)
    if((scale_rank.ne.1.and.scale_rank.ne.2).or.(scale_rank.eq.2.and.scale_dims(0).ne.ncurves)) then
      print("get_bar_scales: error: " + lss_res_name + " must either")
      print("     be 1D, or 2D where the first dimension is equal to the number")
      print("     of Y reference lines or curves. Will return a default of 1.")
      return([/def_grt_scales,def_lss_scales/])
    end if
    nscl = scale_dims(scale_rank-1)
    lss_scales = new((/ncurves,nptsbar/),float)
    do nc = 0,ncurves-1
      do ii = 0,nless(nc)/5-1
        si = ii % nscl
        if(scale_rank.eq.1) then
          lss_scales(nc,ii) = tmp_scale(si)
        else
          lss_scales(nc,ii) = tmp_scale(nc,si)
        end if
      end do
    end do
    lss_scales_set = True
  end if

  res_name = "gsnXYBarChartFillScales"
  if(.not.grt_scales_set.and..not.lss_scales_set.and.isatt(bres,res_name)) then
    tmp_scale := bres@$res_name$
    scale_dims := dimsizes(tmp_scale)
    scale_rank = dimsizes(scale_dims)
    if((scale_rank.ne.1.and.scale_rank.ne.2).or.(scale_rank.eq.2.and.scale_dims(0).ne.ncurves)) then
      print("get_bar_scales: error: " + res_name + " must either")
      print("     be 1D, or 2D where the first dimension is equal to the number")
      print("     of Y reference lines or curves. Won't pattern fill any bars.")
      return([/def_grt_scales,def_lss_scales/])
    end if
    nscl = scale_dims(scale_rank-1)
    grt_scales = new((/ncurves,nptsbar/),integer)
    lss_scales = new((/ncurves,nptsbar/),integer)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        si = ii % nscl
        if(scale_rank.eq.1) then
          grt_scales(nc,ii) = tmp_scale(si)
        else
          grt_scales(nc,ii) = tmp_scale(nc,si)
        end if
      end do
      do ii = 0,nless(nc)/5-1
        si = ii % nscl
        if(scale_rank.eq.1) then
          lss_scales(nc,ii) = tmp_scale(si)
        else
          lss_scales(nc,ii) = tmp_scale(nc,si)
        end if
      end do
    end do
    grt_scales_set = True
    lss_scales_set = True
  end if

  res_name = "gsnXYBarChartFillScales2"
  if(.not.grt_scales_set.and..not.lss_scales_set.and.isatt(bres,res_name)) then
    tmp_scale := bres@$res_name$
    scale_dims := dimsizes(tmp_scale)
    scale_rank = dimsizes(scale_dims)
    if((scale_rank.ne.1.and.scale_rank.ne.2).or.(scale_rank.eq.2.and.scale_dims(0).ne.ncurves)) then
      print("get_bar_scales: error: " + res_name + " must either")
      print("     be 1D, or 2D where the first dimension is equal to the number")
      print("     of Y reference lines or curves. Won't pattern fill any bars.")
      return([/def_grt_scales,def_lss_scales/])
    end if
    nscl = scale_dims(scale_rank-1)
    grt_scales = new((/ncurves,nptsbar/),integer)
    lss_scales = new((/ncurves,nptsbar/),integer)
    do nc = 0,ncurves-1
      do ii = 0,ngrtr(nc)/5-1
        si = indgrtr(nc,ii) % nscl
        if(scale_rank.eq.1) then
          grt_scales(nc,ii) = tmp_scale(si)
        else
          grt_scales(nc,ii) = tmp_scale(nc,si)
        end if
      end do
      do ii = 0,nless(nc)/5-1
        si = indless(nc,ii) % nscl
        if(scale_rank.eq.1) then
          lss_scales(nc,ii) = tmp_scale(si)
        else
          lss_scales(nc,ii) = tmp_scale(nc,si)
        end if
      end do
    end do
    grt_scales_set = True
    lss_scales_set = True
  end if

  res_name = "gsnXYBarChartFillScaleF"
  if(.not.grt_scales_set.and..not.lss_scales_set.and.isatt(bres,res_name)) then
    tmp_scale := bres@$res_name$
    if(.not.isscalar(tmp_scale)) then
      print("get_bar_scales: error: " + res_name + " must be a scalar.")
      print("     Use gsnXYBarChartFillScales if you want an array of fill scales.")
      return([/def_grt_scales,def_lss_scales/])
    end if
    grt_scales = conform_dims((/ncurves,nptsbar/),tmp_scale,-1)
    lss_scales = conform_dims((/ncurves,nptsbar/),tmp_scale,-1)
    grt_scales_set = True
    lss_scales_set = True
  end if

  if(.not.grt_scales_set.and..not.lss_scales_set) then
    return([/def_grt_scales,def_lss_scales/])
  end if

  return([/grt_scales,lss_scales/])
end

;**********************************************************************
; This procedure fills the bars in a bar chart drawn from gsn_csm_xy
; when gsnXYBarChart = True
;**********************************************************************
undef("fill_the_bars")
procedure fill_the_bars(wks,xyobj,type_ref[1]:string,ncurves,nref,filled_bar_list,res2)
local gsres,xgrtr,ygrtr,xless,yless,ngrtr,nless,indgrtr,indless,\
bar_color_list,bar_pattern_list,bar_scales_list,bar_colors_above,\
bar_colors_below,bar_patterns_above,bar_patterns_below,\
bar_scales_above,bar_scales_below,ymin
begin
  res3 = res2
  if(type_ref.eq."y") then
    if(.not.isatt(res3,"gsnYRefLine")) then
      getvalues xyobj
        "trYMinF" : ymin
      end getvalues
      res3@gsnYRefLine = ymin
    end if
  else
    if(.not.isatt(res3,"gsnXRefLine")) then
      getvalues xyobj
        "trXMinF" : xmin
      end getvalues
      res3@gsnXRefLine = xmin
    end if
  end if

;--Set bar filling variables (above==right==greater,below==left=less)
  bar_color_list   = get_bar_colors(wks,type_ref,res3,filled_bar_list)
  bar_pattern_list = get_bar_patterns(res3,type_ref,filled_bar_list)
  bar_scales_list  = get_bar_scales(wks,type_ref,res3,filled_bar_list)

  bar_colors_above   = bar_color_list[0]
  bar_colors_below   = bar_color_list[1]
  bar_patterns_above = bar_pattern_list[0]
  bar_patterns_below = bar_pattern_list[1]

  bar_scales_above = bar_scales_list[0]
  bar_scales_below = bar_scales_list[1]

  xgrtr = filled_bar_list[2]
  ygrtr = filled_bar_list[3]
  xless = filled_bar_list[4]
  yless = filled_bar_list[5]
  ngrtr = filled_bar_list[6]
  nless = filled_bar_list[7]

;---Delete unneeded lists
  delete([/bar_color_list,bar_pattern_list,bar_scales_list/])
;
; These resources allow you to control the look of the fill patterns
; in the bars. You can change the fill line thickness, the density of
; the fill pattern, the thickness of the stippled dots, or the opacity
; of the bar colors.
;
  bar_outline_thickness = get_res_value(res3,"gsnXYBarChartOutlineThicknessF",1.0)
  bar_fill_line_thickness = get_res_value(res3,"gsnXYBarChartFillLineThicknessF",1.0)
  bar_fill_opacity = get_res_value(res3,"gsnXYBarChartFillOpacityF",1.0)
  bar_fill_dot_size = get_res_value(res3,"gsnXYBarChartFillDotSizeF",0.0)

  gsres = True
  if(bar_outline_thickness.gt.0.and.bar_outline_thickness.ne.1) then
    gsres@gsEdgesOn        = True
    gsres@gsEdgeThicknessF = bar_outline_thickness
  end if
  gsres@gsFillLineThicknessF = bar_fill_line_thickness
  gsres@gsFillOpacityF       = bar_fill_opacity
  gsres@gsFillDotSizeF       = bar_fill_dot_size
                                                 ; otherwise, you will get bars in the foreground color
  do i=0,ncurves-1 
    var_string = unique_string("polygons"+i)
;---Fill above reference line.
    if(ngrtr(i).gt.0)
      do ii = 0,ngrtr(i)/5-1
        gsres@gsFillColor := bar_colors_above(i,ii,:)
        gsres@gsFillIndex = bar_patterns_above(i,ii)
        gsres@gsFillScaleF = bar_scales_above(i,ii)
        var_string2 = var_string + "above" + ii
        xyobj@$var_string2$ = gsn_add_polygon(wks,xyobj,xgrtr(i,ii*5:ii*5+4), \
                                              ygrtr(i,ii*5:ii*5+4),gsres)
      end do
    end if
;---Fill below reference line.
    if(nless(i).gt.0)
      do ii = 0,nless(i)/5-1
        gsres@gsFillColor := bar_colors_below(i,ii,:)
        gsres@gsFillIndex = bar_patterns_below(i,ii)
        gsres@gsFillScaleF = bar_scales_below(i,ii)
        var_string2 = var_string + "below" + ii
        xyobj@$var_string2$ = gsn_add_polygon(wks,xyobj,xless(i,ii*5:ii*5+4), \
                                                 yless(i,ii*5:ii*5+4),gsres)
      end do
    end if
  end do

;---Make sure polygons get drawn first, so that XY lines are drawn on top.
  setvalues xyobj
    "tfPolyDrawOrder" : "Predraw"
  end setvalues
end


;**********************************************************************
; This function takes "n" Y or X curves, defined on the same set of X  ;
; or Y points, and fills the area between each adjacent curves with    ;
; some given color. Depending on options set by the user, the areas    ;
; between curves can be filled differently if one is greater than      ;
; another.                                                             ;
;**********************************************************************
undef("fill_between_curves")
function fill_between_curves(wks,xyobj,xx,yy,xyres2)
local fill_atts 
begin
;
; Check if filling between curves is desired. This can be
; specified via gsnXYFillColors, gsnXYAboveFillColors,  
; gsnXYBelowFillColors, gsnXYLeftFillColors, gsnXYRightFillColors.
;
  fill_atts = (/"gsnXYFillColors", "gsnXYAboveFillColors", \
                "gsnXYBelowFillColors","gsnXYLeftFillColors", \
                "gsnXYRightFillColors","gsnXYFillOpacities", \
                "gsnXYFillOpacityF"/)
  if(.not.xyres2.or..not.any(isatt(xyres2,fill_atts))) then
    return(xyobj)
  end if
  ndimsx = dimsizes(xx)
  ndimsy = dimsizes(yy)
  rankx  = dimsizes(ndimsx)
  ranky  = dimsizes(ndimsy)

;
; This next requirement is mostly out of laziness. It wouldn't be that 
; hard to let both X and Y be multi-dimensional. Or would it?
;
  if(rankx.ne.1.and.ranky.ne.1) then
    print("fill_between_curves: Warning: to fill between curves, either X or Y must be 1D.")
    return(xyobj)
  end if

;---Determine whether we're filling between Y curves or X curves.
  if(rankx.eq.1.and.ranky.eq.2) then
    fill_x = False
    xi = xx
    yi = yy
  else
    if(rankx.eq.2.and.ranky.eq.1) then
      fill_x = True
      xi = yy
      yi = xx
    else
      print("fill_between_curves: Error: If filling between two curves, one set must be 2D, and the other 1D.")
      return(xyobj)
    end if
  end if

;---Check length of arrays.
  dsizes_y = dimsizes(yi)
  npts     = dimsizes(xi)
  ncurves  = dsizes_y(0)

  if(dsizes_y(1).ne.npts) then
    print("fill_between_curves: Error: The rightmost dimension of both arrays must be the same.")
    return(xyobj)
  end if

  if(ncurves.le.1) then
    print("fill_between_curves: Error: The leftmost dimension of input array must be at least 2.")
    return(xyobj)
  end if

  if(any(ismissing(xi)))
    print("fill_between_curves: Error: The input array cannot contain any missing values")
    return(xyobj)
  end if

;
; Check resources. There are several possible resources:
;    gsnXYFillColors
;    gsnXYAboveFillColors
;    gsnXYBelowFillColors
;    gsnXYRightFillColors
;    gsnXYLeftFillColors
;    gsnXYFillOpacities
;
; If the first one is set, it overwrites the others. If
; none of them are set, then no fill color will be used.
;
; To make things a little easier (or harder?): 
;      above==right and below==left
;
  res2 = xyres2       ; Make a copy
  if(isatt(res2,"gsnXYFillColors")) then
    above_fill_colors = convert_color_to_rgba(wks,get_res_value(res2,"gsnXYFillColors",-1))
    below_fill_colors = above_fill_colors
    dum               = get_res_value(res2,"gsnXYAboveFillColors",-1)
    dum               = get_res_value(res2,"gsnXYBelowFillColors",-1)
    dum               = get_res_value(res2,"gsnXYRightFillColors",-1)
    dum               = get_res_value(res2,"gsnXYLeftFillColors",-1)
  else
    if(fill_x) then
      above_fill_colors = convert_color_to_rgba(wks,get_res_value(res2,"gsnXYRightFillColors",-1))
      below_fill_colors = convert_color_to_rgba(wks,get_res_value(res2,"gsnXYLeftFillColors",-1))
    else
      above_fill_colors = convert_color_to_rgba(wks,get_res_value(res2,"gsnXYAboveFillColors",-1))
      below_fill_colors = convert_color_to_rgba(wks,get_res_value(res2,"gsnXYBelowFillColors",-1))
    end if
  end if

  nacol = dimsizes(above_fill_colors(:,0))
  nbcol = dimsizes(below_fill_colors(:,0))

;---Check for a fill opacity array. If none, then make colors fully opaque (1.0)
  nfopacs = max((/nacol,nbcol/))
  if(isatt(res2,"gsnXYFillOpacityF")) then
    fill_opacs = conform_dims(nfopacs,get_res_value(res2,"gsnXYFillOpacityF",1.0),-1)
  else if(isatt(res2,"gsnXYFillOpacities")) then
    fill_opacs = get_res_value(res2,"gsnXYFillOpacities",1.0)
  else
    fill_opacs = conform_dims(nfopacs,1.0,-1)
  end if
  end if
;
; Convert input arrays to double.
;
  x  = new(npts,double)
  y  = new(dsizes_y,double,1.e36)
  x  = (/xi/)
  y  = (/yi/)

;
; Create arrays for storing polygon points.
;
  first     = new(2,double)
  last      = new(2,double)
  polygon_x = new(2*npts+3,double)
  polygon_y = new(2*npts+3,double)

;
; Loop through each set of two curves, filling them as we go.
;
  gsres = True
  do n=0,ncurves-2
    y1 = yi(n,:)    ; Grab the current curve
    y2 = yi(n+1,:)  ; and the next curve.
  
    iacol = n % nacol
    ibcol = n % nbcol
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

    npoly = 0     ; Number of polygons
;
; First fill in polygons where y1 is above y2, and then fill in
; polygons where y2 is above y1.
;
    do ab = 0,1
      if(ab.eq.0) then
        gsres@gsFillColor    := above_fill_colors(iacol,:)
        gsres@gsFillOpacityF = fill_opacs(iacol)
      else
        y1 = (/yi(n+1,:)/)
        y2 = (/yi(n,:)/)
        gsres@gsFillColor    := below_fill_colors(ibcol,:)
        gsres@gsFillOpacityF = fill_opacs(ibcol)
      end if

;---Get areas where y1 > y2.
      y1_gt_y2 = y1.gt.y2

      bpt = -1    ; Index of first point of polygon.
      ept = -1    ; Index of last point of polygon.

;---Loop through points.
      do i=0,npts-1
        if(bpt.lt.0)
          if(.not.ismissing(y1_gt_y2(i)).and.y1_gt_y2(i))
            bpt = i
            ept = i
          end if
        else
          if(.not.ismissing(y1_gt_y2(i)).and.y1_gt_y2(i))
            ept = i
          end if
          if(ismissing(y1_gt_y2(i)).or..not.y1_gt_y2(i).or.ept.eq.(npts-1))
;
; Draw polygon. If bpt is the very first point or ept is the
; very last point, then these are special cases we have to
; handle separately.
;
            if(bpt.eq.0.or.(bpt.gt.0.and.(ismissing(y1(bpt-1)).or.\
                                          ismissing(y2(bpt-1)).or.\
                                          ismissing(x(bpt-1)))))
              first(0) = (/ x(bpt) /)
              first(1) = (/ y2(bpt)/)
            else
              if(fabs(y1(bpt-1)-y2(bpt-1)).le.delta)
;
; If the two points are within delta of each other, then we'll
; consider them to be the same point.
;
                first(0) = (/  x(bpt-1) /)
                first(1) = (/ y1(bpt-1) /)
              else

;---Otherwise, find the intersection where the two curves cross.
                first = find_cross_xy(x(bpt-1),x(bpt),y1(bpt-1), \
                                      y1(bpt),y2(bpt-1),y2(bpt))
              end if
            end if
            if(ept.eq.(npts-1).or.(ept.lt.(npts-1).and.\ 
                 (ismissing(y1(ept+1)).or.\
                  ismissing(y2(ept+1)).or.\
                  ismissing(x(ept+1)))))
              last(0) = (/  x(ept) /)
              last(1) = (/ y2(ept) /)
            else
              if(fabs(y1(ept+1)-y2(ept+1)).le.delta)
;
; If the two points are within delta of each other, then we'll
; consider them to be the same point.
;
                last(0) = (/  x(ept+1) /)
                last(1) = (/ y1(ept+1) /)
              else
;---Otherwise, find the intersection where the two curves cross.
                last = find_cross_xy(x(ept),x(ept+1),y1(ept),y1(ept+1), \
                                     y2(ept),y2(ept+1))
              end if
            end if
;---Initialize polygon.
            ppts  = ept - bpt + 1
            ppts2 = ppts * 2
            polygon_x(0)              = (/first(0)/)
            polygon_y(0)              = (/first(1)/)
            polygon_x(1:ppts)         = x(bpt:ept)
            polygon_y(1:ppts)         = y1(bpt:ept)
            polygon_x(ppts+1)         = (/last(0)/)
            polygon_y(ppts+1)         = (/last(1)/)
            polygon_x(ppts+2:ppts2+1) = x(ept:bpt)
            polygon_y(ppts+2:ppts2+1) = y2(ept:bpt)
            polygon_x(ppts2+2)        = (/first(0)/)
            polygon_y(ppts2+2)        = (/first(1)/)

;---Make sure polygons get drawn *after* the plot gets drawn.
            if(npoly.eq.0)
              setvalues xyobj
                "tfPolyDrawOrder" : "Predraw"
              end setvalues
            end if
;---Add polygon to XY plot.
            var_string = unique_string("fill_polygon"+npoly)

            if(fill_x) then
              xyobj@$var_string$ = gsn_add_polygon(wks,xyobj, \
                                                  polygon_y(0:ppts2+2), \
                                                  polygon_x(0:ppts2+2),gsres)
            else
              xyobj@$var_string$ = gsn_add_polygon(wks,xyobj, \
                                                  polygon_x(0:ppts2+2), \
                                                  polygon_y(0:ppts2+2),gsres)
            end if
;---Advance polygon counter.
            npoly = npoly + 1
            bpt = -1            ; Reinitialize
            ept = -1
          end if
        end if
      end do
    end do
  end do

  return(xyobj)
end


;***********************************************************************;
; Function : fill_xy2                                                   ;
;                                                                       ;
; This function is obsolete. Use resources gsnXYFillColors,             ;
;    gsnXYAboveFillColors, gsnXYBelowFillColors, gsnXYLeftFillColors,   ;
;    gsnXYRightFillColors.                                              ;
;                                                                       ;
; This function takes as input two Y curves, defined on the same set    ;
; of X points, and fills the area between the two curves with two       ;
; different colors, depending on which curve is above the other one.    ;
;                                                                       ;
;***********************************************************************;
undef("fill_xy2")
function fill_xy2(wks,plot,xi[*]:numeric,yi1[*]:numeric,yi2[*]:numeric, \
                  colora,colorb)
local res, nx, ny1, ny2
begin
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

  res = True
  res@gsnXYAboveFillColors = colora
  res@gsnXYBelowFillColors = colorb

  return(fill_between_curves(wks,plot,xi,(/yi1,yi2/),res))
end

;**********************************************************************
; This procedure adds X or Y reference lines to an existing plot.
;**********************************************************************
undef("add_ref_line")
procedure add_ref_line(wks,xyobj,axis,lines,color,pattern,thickness)
local i, dmin, dmax, gsres, xline, yline, var_string
begin
  if(axis.ne."y".and.axis.ne."x") then
    print("add_ref_line: warning, invalid type for adding reference line.")
    return
  end if
  if(axis.eq."y") then
    getvalues xyobj
      "trXMinF"  : dmin
      "trXMaxF"  : dmax
    end getvalues
    xline = (/dmin,dmax/)
  else
    getvalues xyobj
      "trYMinF"  : dmin
      "trYMaxF"  : dmax
    end getvalues
    yline = (/dmin,dmax/)
  end if

  gsres = True
  do i=0,dimsizes(lines)-1
    gsres@gsLineColor = color(i,:)
    gsres@gsLineDashPattern = pattern(i)
    gsres@gsLineThicknessF  = thickness(i)
    if(axis.eq."y") then
      yline = fspan(tofloat(lines(i)),tofloat(lines(i)),2)
    else
      xline = fspan(tofloat(lines(i)),tofloat(lines(i)),2)
    end if
    var_string = unique_string(axis+"polyline"+i)
    xyobj@$var_string$ = gsn_add_polyline(wks,xyobj,xline,yline,gsres)
  end do
end

;**********************************************************************
; This procedure adds filled curves based on X/Y reference lines and
; fill colors. The X/Y arrays have already been interpolated based
; on where they intersect with the reference line(s).
;**********************************************************************
undef("fill_xy_ref_curves")
procedure fill_xy_ref_curves(wks,xyobj,reftype,xin,yin,ncurves,nref,\
                             npts_array,ref_line_array,\
                             ref_greater_fill_colors,\
                             ref_less_fill_colors)
local xyres_tmp, i, fill_greater, fill_less, var_string
begin
;
; Make sure polygons get filled before xy plot is drawn. This way,
; any lines will be drawn *after* the fill. This makes the filled 
; polygons look better if the lines are drawn thicker.
;
  xyres_tmp = True
  xyres_tmp@tfPolyDrawOrder = "Predraw"
  attsetvalues_check(xyobj,xyres_tmp)

  fill_greater = ref_greater_fill_colors(0,:)
  fill_less    = ref_less_fill_colors(0,:)
  do i=0,max((/ncurves,nref/))-1
    if(dimsizes(ref_greater_fill_colors(:,0)).gt.1)
      fill_greater = ref_greater_fill_colors(i,:)
    end if
    if(dimsizes(ref_less_fill_colors(:,0)).gt.1)
      fill_less = ref_less_fill_colors(i,:)
    end if
    var_string = unique_string("polygons"+i)
    xyobj@$var_string$=fill_xy_ref(wks,xyobj,reftype, \
                                   xin(i,0:npts_array(i)-1), \
                                   yin(i,0:npts_array(i)-1),\
                                   ref_line_array(i),fill_greater,\
                                   fill_less)
  end do
end

;**********************************************************************
; This procedure adds special "30N", "20E", "EQ" type of labels
; to the X and/or Y axis if it detects that xx or yy are valid
; latitude or longitude arrays.
;**********************************************************************
undef("add_latlon_labels_xy_plot")
procedure add_latlon_labels_xy_plot(xy_obj,xx,yy,res2)
begin
  res2@gsnXYPlot = True

;---X axis
  res2@gsnXAxis = True
  add_latlon_labels(xy_obj,xx,res2)
  delete(res2@gsnXAxis)

;---Y axis
  res2@gsnYAxis = True
  add_latlon_labels(xy_obj,yy,res2)

  delete([/res2@gsnYAxis,res2@gsnXYPlot/])
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
; instead of a line plot. You can also set gsnYRefLine to have your     ;
; bars point up or down from the reference line.                        ;
;                                                                       ;
; The resource gsnXYBarChartColors can be used to specify an array of   ;
; bar colors. This resource was not implemented very smartly. These     ;
; colors will apply independently to both the above ref line bars, and  ;
; the below ref line bars. So, if you give this resource the colors     ;
; "red", "green", and "blue", then all the colors above the ref line    ;
; will cycle b/w these three colors, and independently, the colors below;
; the ref line will cycle b/w these colors. If you want to have one     ;
; resource that specifies the color for each bar, not caring whether it ;
; is above or below the ref line, use the "gsnXYBarChartColors2"        ;
; resource.                                                             ;
;                                                                       ;
; Same for the gsnXYBarChartPatterns/gsnXYBarChartPatterns2 resource.   ;
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
undef("gsn_csm_xy")
function gsn_csm_xy(wks:graphic,x:numeric,y:numeric,resources:logical)
local res, xy_object, res2, xfontf, yfontf, font_height, calldraw, nlines, \
callframe, left_string, center_string, right_string, main_zone, ncurves, \
xinterp, yinterp, bar_chart
begin

; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
; Whether an X or Y reference line is desired
    xref_line_on  = False
    yref_line_on  = False
; Whether to fill curves around a reference line
    yref_fill_on       = False
    yref_above_fill_on = False
    yref_below_fill_on = False
    xref_fill_on       = False
    xref_right_fill_on = False
    xref_left_fill_on  = False
    bar_chart     = False     ; Whether we want bars or curves

    res2          = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(x,y,new(1,float),"gsn_csm_xy",res2,2)
    end if

    left_string   = False
    center_string = False
    right_string  = False
    tm_scale      = get_res_value_keep(res2,"gsnScale",True)
    point_outward = get_res_value(res2,"gsnTickMarksPointOutward",True)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Calculate number of Y points.

    ndimsy = dimsizes(dimsizes(y))
    ndimsx = dimsizes(dimsizes(x))

    if(ndimsx.ne.1.and.ndimsy.ne.1.and.ndimsx.ne.2.and.ndimsy.ne.2)
      print("gsn_csm_xy: Fatal: X and Y must have the same dimensions sizes, or one must be one-dimensional and both have the same rightmost dimension.")
      exit
    end if

    if(ndimsx.eq.1)
      nptsx    = dimsizes(x)
      ncurvesx = 1
    else
      nptsx    = dimsizes(x(0,:))
      ncurvesx = dimsizes(x(:,0))
    end if
    if(ndimsy.eq.1)
      nptsy    = dimsizes(y)
      ncurvesy = 1
    else
      nptsy    = dimsizes(y(0,:))
      ncurvesy = dimsizes(y(:,0))
    end if
;
; Test dimensions. They must both either be the same, or have the
; same rightmost dimension.
;
    if( (ndimsx.gt.1.and.ndimsy.gt.1.and.(ndimsx.ne.ndimsy.or. \
         .not.all(dimsizes(x).eq.dimsizes(y)))).or.\
        ((ndimsx.eq.1.or.ndimsy.eq.1).and.nptsx.ne.nptsy))
      print("gsn_csm_xy: Fatal: X and Y must have the same dimensions sizes, or one must be one-dimensional and both have the same rightmost dimension.")
      exit
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

;---Check if gsnYRefLine set.
      yref_line_list = get_ref_line_options(wks,x,y,"y",ncurvesy,res2)
      yref_line = yref_line_list[0]
      yref_line_on = yref_line_list[1]
      yref_line_color = yref_line_list[2]
      yref_line_pattern = yref_line_list[3]
      yref_line_thickness = yref_line_list[4]
      yref_above_fill_on = yref_line_list[5]
      yref_below_fill_on = yref_line_list[6]
      yref_above_fill_color = yref_line_list[7]
      yref_below_fill_color = yref_line_list[8]
      nyref = dimsizes(yref_line)
      yref_fill_on = yref_above_fill_on.or.yref_below_fill_on
      delete(yref_line_list)

;---Check if gsnXRefLine set.
      xref_line_list = get_ref_line_options(wks,x,y,"x",ncurvesx,res2)
      xref_line = xref_line_list[0]
      xref_line_on = xref_line_list[1]
      xref_line_color = xref_line_list[2]
      xref_line_pattern = xref_line_list[3]
      xref_line_thickness = xref_line_list[4]
      xref_right_fill_on = xref_line_list[5]
      xref_left_fill_on = xref_line_list[6]
      xref_right_fill_color = xref_line_list[7]
      xref_left_fill_color = xref_line_list[8]
      nxref = dimsizes(xref_line)
      xref_fill_on = xref_right_fill_on.or.xref_left_fill_on
      delete(xref_line_list)

;---Check if we want a bar chart.
      bar_chart        = get_res_value(res2,"gsnXYBarChart",False)
      bar_outline_only = get_res_value(res2,"gsnXYBarChartOutlineOnly",False)

; Check for existence of the left, center, and right subtitles.

      check_for_subtitles(res2,left_string,center_string,right_string)
      if(left_string.or.center_string.or.right_string)
        main_zone   = main_zone+1
      end if
    end if                  ; Done checking special resources.

    res2          = True
    res2@gsnDraw  = False     ; Internally, don't draw plot or advance
    res2@gsnFrame = False     ; frame since we take care of that later.
    res2@gsnScale = tm_scale  ; Force labels and ticks to be same.

; If user explicitly sets X or Y tick marks, then don't label the X 
; or Y axis automatically.

    if(check_attr_enum_value(res2,"tmXBMode","Explicit"))
      set_attr(res2,"tiXAxisString","")
    end if

    if(check_attr_enum_value(res2,"tmYLMode","Explicit"))
      set_attr(res2,"tiYAxisString","")
    end if
;
; The following code is going to create new X,Y arrays, depending
; on whether the user wants:
;
;   1a. A regular XY plot with curves above/below a given
;       reference line filled in.
;   1b. A regular XY plot with curves left/right a given
;       reference line filled in.
;   2.  A bar plot without any fill.
;   3.  A bar plot with filling above/below a given Y reference value.
;   4.  A regular XY plot.

;----------------------------------------------------------------------
; 1a. A regular XY plot with the curve above and below a given
; reference line filled in with some color.
;----------------------------------------------------------------------
    if(.not.bar_chart.and.yref_fill_on) then
      interp_list = ref_line_interp(x,y,yref_line,"y")
      xinterp  = interp_list[0]
      yinterp  = interp_list[1]
      new_yref_line = interp_list[2]
      new_npts = interp_list[3]
      delete(interp_list)
    end if
;----------------------------------------------------------------------
; 1b. A regular XY plot with the curve to the right and left of a
; given reference line filled in with some color.
;----------------------------------------------------------------------
    if(.not.bar_chart.and.xref_fill_on) then
      interp_list = ref_line_interp(x,y,xref_line,"x")
      xinterp  = interp_list[0]
      yinterp  = interp_list[1]
      new_xref_line = interp_list[2]
      new_npts = interp_list[3]
      delete(interp_list)
    end if
    if(bar_chart.and.bar_outline_only) then
;----------------------------------------------------------------------
; 2. A bar XY plot with outlined bars only.
;----------------------------------------------------------------------
      interp_list = outlined_bars(x,y)
      xinterp  = interp_list[0]
      yinterp  = interp_list[1]
      delete(interp_list)
    end if
    if(bar_chart.and..not.bar_outline_only)
;----------------------------------------------------------------------
; 3. A bar plot with filled bars.
;----------------------------------------------------------------------
      if(xref_fill_on) then
        filled_bar_list = filled_bars_interp(x,y,"x",xref_line,res2)
      else
        filled_bar_list = filled_bars_interp(x,y,"y",yref_line,res2)
      end if
      xinterp = filled_bar_list[0]
      yinterp = filled_bar_list[1]
;
; Make sure all lines are solid, and not dashed as is the default
; with the second curve drawn.
; 
      res2@xyDashPatterns = get_res_value(res2,"xyDashPatterns",0)
    end if
    if(.not.any((/yref_fill_on,xref_fill_on,bar_chart/))) then
;----------------------------------------------------------------------
;  4. A regular XY plot
;----------------------------------------------------------------------
      xinterp = x
      yinterp = y
    end if    ; if bar_chart
;
; Force the X or Y min/max values to be that of the data,
; if a reference line is drawn.
;
    if(xref_line_on) then
      set_attr(res2,"trYMinF",min(yinterp))
      set_attr(res2,"trYMaxF",max(yinterp))
    end if
    if(yref_line_on) then
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

;---Create XY plot
    xyres = get_res_ne(res2,(/"tx","gsnXY","gsnYRef","gsnXRef",\
                              "gsnAbove","gsnBelow","gsnRight","gsnLeft"/))

    xy_object = gsn_xy(wks,xinterp,yinterp,xyres)
;---Fill between two curves if desired
    xy_object = fill_between_curves(wks,xy_object,x,y,res2)

;---Add lat/lon labels to X/Y axes if appropriate units exist.
    add_latlon_labels_xy_plot(xy_object,x,y,res2)

;---Fill above/below/left/right Y/X reference line. 
    if(yref_fill_on.and..not.bar_chart) then
      fill_xy_ref_curves(wks,xy_object,"y",xinterp,yinterp,ncurvesy,\
                         nyref,new_npts,new_yref_line,\
                         yref_above_fill_color,yref_below_fill_color)
    end if
    if(xref_fill_on.and..not.bar_chart) then
      fill_xy_ref_curves(wks,xy_object,"x",xinterp,yinterp,ncurvesx,\
                         nxref,new_npts,new_xref_line,\
                         xref_right_fill_color,xref_left_fill_color)
    end if

; Fill bars with either a single color for above and below, or with
; individual colors.

    if(bar_chart.and..not.bar_outline_only)
      if(xref_fill_on) then
        fill_the_bars(wks,xy_object,"x",ncurvesx,nxref,filled_bar_list,res2)
      else
        fill_the_bars(wks,xy_object,"y",ncurvesy,nyref,filled_bar_list,res2)
      end if
    end if

;---Add X/Y reference lines.
    if(xref_line_on) then
      add_ref_line(wks,xy_object,"x",xref_line,xref_line_color,xref_line_pattern,xref_line_thickness)
    end if
    if(yref_line_on) then
      add_ref_line(wks,xy_object,"y",yref_line,yref_line_color,yref_line_pattern,yref_line_thickness)
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

    tmres = get_res_eq(res2,"tm")
    gsnp_point_tickmarks_outward(xy_object,tmres,xlength,ylength,xmlength, \
                                 ymlength,major_length,minor_length, \
                                 point_outward)

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres               = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,xy_object,left_string,center_string,right_string, \
                  subres)

; Draw all this stuff: XY plot and subtitles.
    draw_and_frame(wks,xy_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_y")
function gsn_csm_y(wks:graphic, y:numeric, resources:logical )
local dsizes_y, npts, x, rank_y, xy
begin
  res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(y,new(1,float),new(1,float),"gsn_csm_y",res2,1)
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
; Function : gsn_csm_zonal_means                                        ;
;                                                                       ;
;    wks        : Workstation object                                    ;
;    zdata      : data for calculating zonal means                      ;
;    zres       : optional resource list                                ;
;                                                                       ;
; This function creates a zonal means plot.                             ;
;                                                                       ;
;***********************************************************************;
undef("gsn_csm_zonal_means")
function gsn_csm_zonal_means(wks,zdata,zres)
local max_lat, zres2, zmeans
begin
; Calculate zonal means, and add Y reference line curve if requested.
  zmeans = calculate_zonal_means(zdata,zres)

  if(zres) then
    zres2 = zres
  end if

  zres2 = True

; Set up rest of resource list.
  zres2@xyDashPattern = get_res_value(zres2,"xyDashPattern",0)

; Set trXMinF/trXMaxF only if specified by user.
  if(isatt(zres,"gsnZonalMeanXMinF")) then
    zres2@trXMinF = get_res_value(zres2,"gsnZonalMeanXMinF",0.)
  end if

  if(isatt(zres,"gsnZonalMeanXMaxF")) then
    zres2@trXMaxF = get_res_value(zres2,"gsnZonalMeanXMaxF",0.)
  end if

; Create zonal XY plot, using the coordinate array of the Y axis for
; the Y values.

  xy_object = gsn_csm_xy(wks,zmeans,zdata&$zdata!0$,zres2)

  return(xy_object)
end

;***********************************************************************;
; Function : gsn_csm_attach_zonal_means                                 ;
;                                                                       ;
;    wks        : Workstation object                                    ;
;    map_object : map plot to attach zonal plot to                      ;
;    zdata      : data for calculating zonal means                      ;
;    zres       : optional resource list                                ;
;                                                                       ;
; This function creates a zonal means plot and attaches it to the       ;
; given map object.                                                     ;
;                                                                       ;
;***********************************************************************;
undef("gsn_csm_attach_zonal_means")
function gsn_csm_attach_zonal_means(wks,map_object,zdata,zres)
local zres2, zres3
begin

; Resources for creating the zonal plot. Make sure gsnMaximize is not
; set for this phase.
  zres2 = get_res_ne(zres,(/"am","gsnDraw","gsnFrame","gsnMaximize"/))
  zres2 = True

;
; Retrieve the lat limits and view port size of the map plot so we plot
; the correct range in Y, and make the plot the correct size.
;
  getvalues map_object
    "vpHeightF"    : vphf
    "mpMinLatF"    : min_lat
    "mpMaxLatF"    : max_lat
  end getvalues
;
; If the map has tickmarks, then retrieve font heights and tickmark lengths.
;
  if(isatt(map_object,"tickmarks")) then
    getvalues map_object@tickmarks
      "tmXBLabelFontHeightF" : xbfontf
      "tmXBMajorLengthF"     : xlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues
    zres2@tmXBLabelFontHeightF    = get_res_value(zres2, \
                                                  "tmXBLabelFontHeightF", \
                                                   xbfontf)
    zres2@tmXBMajorLengthF        = get_res_value(zres2, \
                                                  "tmXBMajorLengthF", \
                                                  xlength)
    zres2@tmYRMajorLengthF        = get_res_value(zres2, \
                                                  "tmYRMajorLengthF", \
                                                  xlength)
    zres2@tmXBMajorOutwardLengthF = get_res_value(zres2, \
                                                  "tmXBMajorOutwardLengthF", \
                                                  xlength)
    zres2@tmYRMajorOutwardLengthF = get_res_value(zres2, \
                                                  "tmYRMajorOutwardLengthF", \
                                                  xlength)
    zres2@tmXBMinorLengthF        = get_res_value(zres2, \
                                                  "tmXBMinorLengthF", \
                                                  xmlength)
    zres2@tmYRMinorLengthF        = get_res_value(zres2, \
                                                  "tmYRMinorLengthF", \
                                                  xmlength)
    zres2@tmXBMinorOutwardLengthF = get_res_value(zres2, \
                                                  "tmXBMinorOutwardLengthF", \
                                                  xmlength)
    zres2@tmYRMinorOutwardLengthF = get_res_value(zres2, \
                                                  "tmYRMinorOutwardLengthF", \
                                                  xmlength)
  end if

; Set up rest of resource list.
  zres2@vpHeightF         = get_res_value(zres2,"vpHeightF",vphf)
  zres2@vpWidthF          = get_res_value(zres2,"vpWidthF",0.15)
  zres2@trYMaxF           = get_res_value(zres2,"trYMaxF",max_lat)
  zres2@trYMinF           = get_res_value(zres2,"trYMinF",min_lat)
  zres2@tmYLOn            = get_res_value(zres2,"tmYLOn",False)
  zres2@tmYROn            = get_res_value(zres2,"tmYROn",False)
  zres2@tmXBMaxTicks      = get_res_value(zres2,"tmXBMaxTicks",3)
  zres2@tmXBMinorPerMajor = get_res_value(zres2,"tmXBMinorPerMajor",1)
  zres2@tmXTMinorPerMajor = get_res_value(zres2,"tmXTMinorPerMajor",1)
  zres2@tiXAxisOn         = get_res_value(zres2,"tiXAxisOn",False)
  zres2@tiYAxisOn         = get_res_value(zres2,"tiYAxisOn",False)
  zres2@gsnDraw           = False
  zres2@gsnFrame          = False

; Create zonal XY plot, using the coordinate array of the Y axis for
; the Y values.

  zonal_plot = gsn_csm_zonal_means(wks,zdata,zres2)
;
; Resources for attaching the zonal plot. By default, we don't
; want the attached plots drawn or the frame advanced.
;
  zres3          = zres
  maxbb          = get_bb_res(zres)
  zres3          = True
  zres3@gsnDraw  = get_res_value(zres3,"gsnDraw",False)
  zres3@gsnFrame = get_res_value(zres3,"gsnFrame",False)
;
; Make maximize True by default, because the zonal plot is likely to
; go off the frame.
;
  maxbb = get_res_value(zres3,"gsnMaximize",True)

; Add zonal plot as annotation of map plot.
  anno = NhlAddAnnotation(map_object,zonal_plot)
  setvalues anno
    "amZone"          : get_res_value(zres3,"amZone",2)
    "amSide"          : get_res_value(zres3,"amSide","right")
    "amResizeNotify"  : get_res_value(zres3,"amResizeNotify",True)
    "amParallelPosF"  : get_res_value(zres3,"amParallelPosF",0.5)
    "amOrthogonalPosF": get_res_value(zres3,"amOrthogonalPosF",0.05)
  end setvalues

  draw_and_frame(wks,map_object,zres3@gsnDraw,zres3@gsnFrame,0,maxbb)

  return(zonal_plot)
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
undef("gsn_csm_map_ce")
function gsn_csm_map_ce(wks:graphic,resources:logical)
local i, res, map_object, tickmark_object, calldraw, callframe, \
left_string, center_string, right_string, main_zone, min_lat, max_lat, \
min_lon, max_lon, res2, tmres, ticks_on, display_mode, rel_c_lat, rel_c_lon, \
center_lon, center_lat, lon_range, lat_range, font_height
begin
; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(new(1,float),new(1,float),new(1,float),\
                       "gsn_csm_map_ce",res2,0)
    end if

;
; Make sure user is not calling this function, and then trying to
; change the map projection, because this can cause problems later.
;
    if(isatt(res2,"mpProjection")) then
      iproj = res2@mpProjection
      if( (typeof(iproj).eq."integer".and.iproj.ne.8).or.\
          (typeof(iproj).eq."string".and.str_lower(iproj).ne. \
                                             "cylindricalequidistant")) then
        print("gsn_csm_map_ce: Warning: you are calling one of the CE map functions,")
        print("but setting the map projection to something other than")
        print("'CylindricalEquidistant'. You may get errors or unexpected results.")
      end if
    end if                

;---Fix special case where map goes from 0 to 360, and not -180 to 180
    if(isatt(res2,"mpMinLonF").and.isatt(res2,"mpMaxLonF").and.\
       .not.isatt(res2,"mpCenterLonF").and.res2@mpMaxLonF.gt.180) then
       res2@mpCenterLonF = (res2@mpMaxLonF+res2@mpMinLonF)/2.
       print("gsn_csm_map_ce: Warning: you set mpMaxLonF to a value > 180, but")
       print("                didn't set mpCenterLonF. Setting mpCenterLonF to " + res2@mpCenterLonF)
    end if

; This section tests for more special resources: those that start
; with "gsn."

; Check if frame and/or draw are not supposed to be called.
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
    point_outward = get_res_value(res2,"gsnTickMarksPointOutward",True)

    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(res2,left_string,center_string,right_string)
    if(left_string.or.center_string.or.right_string)
      main_zone   = 4
    end if
;
; Increase title zone if map tickmarks are on.
;
    if(check_attr_enum_value(res2,"pmTickMarkDisplayMode","Always").or.\
       check_attr_enum_value(res2,"pmTickMarkDisplayMode","Conditional").or.\
       check_attr_enum_value(res2,"pmTickMarkDisplayMode","ForceAlways")) then
      main_zone = main_zone + 1
    end if
    lat_spacing  = get_res_value(res2,"gsnMajorLatSpacing",0.)
    lat_spacing  = get_res_value(res2,"tmYLTickSpacingF",lat_spacing)
    lon_spacing  = get_res_value(res2,"gsnMajorLonSpacing",0.)
    lon_spacing  = get_res_value(res2,"tmXBTickSpacingF",lon_spacing)
    mlat_spacing = get_res_value(res2,"gsnMinorLatSpacing",0.)
    mlon_spacing = get_res_value(res2,"gsnMinorLonSpacing",0.)
    center_lon   = get_res_value_keep(res2, "mpCenterLonF",  0)
    center_lat   = get_res_value_keep(res2, "mpCenterLatF",  0)
    center_rot   = get_res_value_keep(res2, "mpCenterRotF",  0)
    rel_c_lat    = get_res_value(res2, "mpRelativeCenterLat", False)
    rel_c_lon    = get_res_value(res2, "mpRelativeCenterLon", False)
    shape_mode   = get_res_value(res2,"mpShapeMode","FixedAspectFitBB")
    limit_mode   = get_res_value(res2,"mpLimitMode","LatLon")
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

;---Convert limit_mode to string, if necessary.
    if(typeof(limit_mode).eq."integer") then
      limit_modes = (/"MaximalArea","LatLon","Angles","NPC","NDC",\
                      "Corners","Points","Window"/)
      limit_mode := str_lower(limit_modes(limit_mode))
    else
      limit_mode = str_lower(limit_mode)
    end if
;
; If automatic tick marks are turned on, or if mpCenterLatF != 0,
; or if mpCenterRotF != 0 (rotated map), or if limit_mode is not 
; latlon or corners, then turn off gsn_csm generated tickmarks.
; 
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2.or.\
        center_lat.ne.0.or.center_rot.ne.0.or.\
       (limit_mode.ne."latlon".and.limit_mode.ne."corners")) then
      ticks_on = get_res_value(res2,"gsnTickMarksOn",False)   ; Do this to remove
      ticks_on = False                                        ; gsnTickMarksOn as an attribute
    else
      ticks_on = get_res_value(res2,"gsnTickMarksOn",True)
    end if

    if(limit_mode.eq."latlon".or.limit_mode.eq."corners") then
;
; If mpCenterLonF is set, but mpMinLonF/mpLeftCornerLonF and/or 
; mpMaxLonF/mpRightCornerLonF are not, then we need to set them.
;
      if(limit_mode.eq."latlon") then
        min_lon_str = "mpMinLonF"
        max_lon_str = "mpMaxLonF"
        min_lat_str = "mpMinLatF"
        max_lat_str = "mpMaxLatF"
      else
        min_lon_str = "mpLeftCornerLonF"
        max_lon_str = "mpRightCornerLonF"
        min_lat_str = "mpLeftCornerLatF"
        max_lat_str = "mpRightCornerLatF"
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
  
      min_lon    = get_res_value(res2,min_lon_str,-180)
      max_lon    = get_res_value(res2,max_lon_str, 180)
      min_lat    = get_res_value(res2,min_lat_str, -90)
      max_lat    = get_res_value(res2,max_lat_str,  90)

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
    end if
;    
; Create the map object. We have to use this code instead of gsn_map
; because we want to set the size of the map.  If we set the size of the
; map later with setvalues, as gsn_map would do, then the size wouldn't
; be correct.
;
    if(limit_mode.eq."latlon") then
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
        "mpCenterLonF"       : center_lon
        "mpCenterLatF"       : center_lat
        "mpRelativeCenterLat" : rel_c_lat
        "mpRelativeCenterLon" : rel_c_lon
        "mpShapeMode"        : shape_mode   ; must be set when map created
        "pmTitleDisplayMode" : "Always"
        "pmTitleZone"        : main_zone    ; Zone for main title
      end create
    else if(limit_mode.eq."corners") then
      map_object = create "map" mapPlotClass wks
        "vpXF"                : res2@vpXF
        "vpYF"                : res2@vpYF
        "vpWidthF"            : res2@vpWidthF
        "vpHeightF"           : res2@vpHeightF
        "mpShapeMode"         : shape_mode
        "mpLimitMode"         : limit_mode
        "mpLeftCornerLonF"    : min_lon
        "mpRightCornerLonF"   : max_lon
        "mpLeftCornerLatF"    : min_lat
        "mpRightCornerLatF"   : max_lat
        "mpCenterLonF"        : center_lon
        "mpCenterLatF"        : center_lat
        "mpRelativeCenterLat" : rel_c_lat
        "mpRelativeCenterLon" : rel_c_lon
        "pmTitleDisplayMode"  : "Always"
        "pmTitleZone"         : main_zone    ; Zone for main title
      end create
    else
      map_object = create "map" mapPlotClass wks
        "pmTickMarkDisplayMode" : display_mode
        "pmTitleDisplayMode"    : "Always"
        "pmTitleZone"           : main_zone    ; Zone for main title
        "mpLimitMode"           : limit_mode
      end create
    end if
    end if
; This section tests for regular resources.

    tmres = get_res_eq(res2,"tm")  ; Get tickmark resources
    if(.not.ticks_on)
      tmres = True
      gsnp_turn_off_tickmarks(tmres)
    end if
;
; If user has turned on built-in map tickmarks, then go ahead and
; let the tickmark resources (if any) be passed to the map object.
;
    if(display_mode.eq.1.or.display_mode.eq.2) then
      mpres = get_res_ne(res2,(/"tx","am"/))       ; Get rest of resources
    else
      mpres = get_res_ne(res2,(/"tx","am","tm"/))  ; Get rest of resources
    end if
;
; Set some map resources
;
    attsetvalues_check(map_object,mpres)
;
; Add tickmark object, even if the tickmarks have been turned off, because
; we need to get the size of the X/Y axis labels.
;
    add_map_tickmarks(wks,map_object,tmres,lon_spacing,mlon_spacing,\
                      lat_spacing,mlat_spacing,point_outward)

; Make sure axes labels (if any) are the same size.

    if(isatt(map_object,"tickmarks")) then
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
    end if

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    if(isvar("font_height"))
      set_attr(subres,"txFontHeightF",font_height)
    end if
    set_attr(subres,"amOrthogonalPosF",0.05)
    add_subtitles(wks,map_object,left_string,center_string,right_string, \
                  subres)

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
;    6. gsnPolarLabelFont and gsnPolarLabelFontHeightF can be used to   ;
;       control the label font and heights of the lat/lon labels.       ;
;***********************************************************************;
undef("gsn_csm_map_polar")
function gsn_csm_map_polar(wks:graphic,resources:logical)
local i, res, res2, map_object, main_zone, \
calldraw, callframe, left_string, center_string, right_string, yoffset, \
min_lat, max_lat, center_lat, main_offset, \
side, just, orth, para, quad1, quad2, quad3, quad4, topbot, xcen, ycen, \
vpxf, vpyf, vphf, vpwf, plon, plat, xndc, yndc, polar_type, center_lon, \
center_lat, rel_c_lon, rel_c_lat, set_text_font
begin

; Initialize.
    min_lat       = 0.0
    max_lat       = 90.0
    center_lat    = 0.0 
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(new(1,float),new(1,float),new(1,float),\
                       "gsn_csm_map_polar",res2,0)
    end if

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
    limit_mode = get_res_value(res2,"mpLimitMode","LatLon")
    shape_mode = get_res_value(res2,"mpShapeMode","FixedAspectFitBB")
    rel_c_lat  = get_res_value(res2,"mpRelativeCenterLat", False)
    rel_c_lon  = get_res_value(res2,"mpRelativeCenterLon", False)

;
; If labeling the polar plot with time values, then we want slt = 0
; to be at the bottom of the plot, so we need to rotate the map 
; accordingly, by setting mpCenterLonF.
;
;  If slt = ut + lon/15, then at slt=0:
;
;   0 = ut + lon/15  --> lon/15 = -ut -->  lon = -ut*15.
;
    center_lon = 0.
    if(polar_time) then
      if(polar_type.eq."NH")
        center_lon = -ut*15
      else
        center_lon = -ut*15+180
      end if 
    end if 
    center_lon = get_res_value(res2, "mpCenterLonF", center_lon)

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

      "mpCenterLonF"         : center_lon
      "mpCenterLatF"         : center_lat
      "mpRelativeCenterLat"  : rel_c_lat
      "mpRelativeCenterLon"  : rel_c_lon

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
    set_attr(res2,"mpFillOn",True)
    set_attr(res2,"mpGridAndLimbOn",True)
    set_attr(res2,"mpGridLineDashPattern",2)
    set_attr(res2,"mpGridLonSpacingF",30.)
    set_attr(res2,"mpPerimOn",True)
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
;
; Retrieve the map resources.
;
    mpres = get_res_ne(res2,(/"gs","tx","tm"/))

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
    main_font   = get_res_value_keep(res2,"tiMainFont","helvetica-bold")
    main_offset = get_res_value_keep(res2,"tiMainOffsetYF",0.02)

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
      if(.not.polar_time) then
        ngrid = floattoint(360/label_spacing)
        if(ngrid.gt.0)
          plon = fspan(0.,360.-label_spacing,ngrid)
        else
	  plon = 0.
        end if
      else
;
; If labeling with time, we calculate the longitudes, given nice
; time values of 0, 3, ..., 21.
;
        slt   = ispan(0,21,3)
        plon  = 15.*(slt-ut)
        ngrid = dimsizes(plon)
      end if
;
; The "0.005" addition is to make sure we are within the map perimeter.
; Otherwise,; if we enter points right along the edge of the circle, then
; when they are converted to NDC coords, you might get missing values.
;
      if(min((/min_lat,max_lat/)).ge.0.or.polar_type.eq."NH")
          plat = fspan(min_lat+.005,min_lat+.005,ngrid)
      else
          plat = fspan(max_lat-.005,max_lat-.005,ngrid)
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

        indexes = ind(plon.eq.0.or.plon.eq.180)
        labels(indexes) = floattoint(plon(indexes))
        delete(indexes)
      else
;
; Define an array of strings to label the time values.
;
        labels = slt
      end if
      num_labels = dimsizes(labels)
      text_ids = new(num_labels,graphic)
;
; Check if user setting own font heights.  gsnPolarLabelFontHeightF
; takes precedence over txFontHeightF.
;
      text_font_height = get_res_value(res2,"gsnPolarLabelFontHeightF", \
                         get_res_value_keep(res2,"txFontHeightF",\
                         font_height))
      if(isatt(res2,"gsnPolarLabelFont")) then
        set_text_font = True
        text_font     = get_res_value(res2,"gsnPolarLabelFont",1)
      else if(isatt(res2,"txFont")) then
        set_text_font = True
        text_font     = get_res_value_keep(res2,"txFont",1)
      else
        set_text_font = False
      end if
      end if

; Create an array of TextItem objects that we'll use to label the
; lat/lon grid.  

      if(set_text_font) then
        do i=0,num_labels-1
          text_ids(i) = create "lon_"+labels(i) textItemClass wks
            "txString"      : labels(i)
            "txFont"        : text_font
            "txFontHeightF" : text_font_height
          end create
        end do
      else
        do i=0,num_labels-1
          text_ids(i) = create "lon_"+labels(i) textItemClass wks
            "txString"      : labels(i)
            "txFontHeightF" : text_font_height
          end create
        end do
      end if
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
        names_char = stringtocharacter(names)
        if(charactertostring(names_char(i,0:3)).eq."lon_")
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
;    set_attr(subres,"txFont","helvetica")
    subres@amOrthogonalPosF = 0.06 
    add_subtitles(wks,map_object,left_string,center_string,right_string, \
                  subres)

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
;    2. If the special resource "gsnMaskLambertConformal" is set, then  ;
;       if the resources mpMinLatF, mpMaxLatF, mpMinLonF, and mpMaxLonF ;
;       are also set, the lambert conformal map will be masked outside  ;
;       of these min/max lat/lon boundaries.  Note: mpProjection must   ;
;       be set to "LambertConformal".                                   ;
;    3. If the special resource "gsnMaskLambertConformalOutlineOn" is   ;
;       set to False, then the lat/lon area of interest will not be     ;
;       outlined in the foreground color.                               ;
;***********************************************************************;
undef("gsn_csm_map_other")
function gsn_csm_map_other(wks:graphic,resources:logical)
local i, res, map_object, calldraw, callframe, \
left_string, center_string, right_string, main_zone, res2
begin
; Initialize.
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(new(1,float),new(1,float),new(1,float),\
                       "gsn_csm_map_other",res2,0)
    end if
;
; Need to check what kind of tickmarks are being used so we can set
; the zone appropriately. It may be the case that it's okay to always
; set it to 3, but this makes the title move slightly up, even if there
; are no tickmarks.
;
    display_mode = get_display_mode(res2,"pmTickMarkDisplayMode","nocreate")
    if(display_mode.eq.1.or.display_mode.eq.2) then
      main_zone = 3
    else
      main_zone = 2
    end if

; This section tests for more special resources: those that start
; with "gsn."

; Check if frame and/or draw are not supposed to be called.
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    masklc    = get_res_value(res2,"gsnMaskLambertConformal",False)
    maskoutln = get_res_value(res2,"gsnMaskLambertConformalOutlineOn",True)
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
    set_attr(res2,"mpPerimOn", .not.masklc)
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

    mpres = get_res_ne(res2,(/"tx","tm"/))  ; Get rest of resources
;
; If user wants to mask the lambert conformal map, then to avoid 
; the error message:
;
;      warning:MapSetTrans: map limits invalid - using maximal area
; 
; we must set the lambert parallels here.   We also use this opportunity
; to make sure the Min/Max/Lat/LonF resources are set.
;
    if(masklc) then
      if(isatt(res2,"mpMinLatF").and.isatt(res2,"mpMaxLatF").and. \
         isatt(res2,"mpMinLonF").and.isatt(res2,"mpMaxLonF")) then
        if (res2@mpMinLatF.lt.0) then
          mpres@mpLambertParallel1F = -0.001
          mpres@mpLambertParallel2F = -89.999
        end if
      else
        print("gsn_csm_map_other: Warning: one or more of the resources mpMinLatF, mpMaxLatF, mpMinLonF, and mpMaxLonF have not been set.")
        print("No masking of the Lambert Conformal map will take place.")
        masklc = False
      end if
    end if

;
; If user has turned on built-in map tickmarks, then go ahead and
; let the tickmark resources (if any) be passed to the map object.
;
    if(display_mode.eq.1.or.display_mode.eq.2) then
      mpres = get_res_ne(res2,(/"tx","am"/))       ; Get rest of resources
    else
      mpres = get_res_ne(res2,(/"tx","am","tm"/))  ; Get rest of resources
    end if
;
; Set some map resources.
;
    attsetvalues_check(map_object,mpres)
;
; If user wants to mask the lambert conformal map, then the four
; mpMin/Max resources must be set. The masking routine will be called,
; and masking polygons will be attached to map_object.
;
    if(masklc) then
      map_object = mask_lambert_conformal(wks,map_object,res2@mpMinLatF, \
                                          res2@mpMaxLatF,res2@mpMinLonF, \
                                          res2@mpMaxLonF,maskoutln,res2)
    end if

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

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_map")
function gsn_csm_map(wks:graphic,resources:logical)
begin
  res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res2,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(new(1,float),new(1,float),new(1,float),\
                     "gsn_csm_map",res2,0)
  end if

  if(res2.and.(isatt(res2,"gsnPolarNH").or.isatt(res2,"gsnPolarSH").or.\
               isatt(res2,"gsnPolar")))
    return(gsn_csm_map_polar(wks,res2))
  else
    if(.not.isatt(res2,"mpProjection").or.\
       check_attr_enum_value(res2,"mpProjection","CylindricalEquidistant"))
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
undef("gsn_csm_contour_map_polar")
function gsn_csm_contour_map_polar(wks:graphic,data:numeric,resources:logical)
local i, contour_object, map_object, res, lbar_pos, lbar_zone, \
use_old_lbar, use_mgr_lbar, calldraw, callframe, main_offset, datanew, res2, \
cnres, mpres, vpwf, vphf, font_height, levels, colors, lbres, \
infolabel_on, infolabel_zone, lbar_zone, lbar_height, lbar_width
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_contour_map_polar: Fatal: the input data array must be 1D or 2D")
      return
    end if

    res2  = get_resources(resources) ; Copy of resources

;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),\
                       "gsn_csm_contour_map_polar",res2,1)
    end if
    mpres = True      ; Will hold map resources

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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

; Create data object and use coordinate variables if they exist.

    check_for_y_lat_coord(datanew,res2,"contour_map")
    check_for_lon_coord(datanew,res2,"contour_map")

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
    mpres = get_res_eq(res2,(/"mp","ti","vp","tx","am","pmA","pmO","pmT",\
                              "gsnPolar"/))
    cnres = get_res_ne(res2,(/"mp","vp","lb","tx","am","gsnPolar"/))

;---Labelbar stuff
    lbar_zone = 3         ; Zone for labelbar (may change later)
    if(check_attr(cnres,"cnFillOn",True,False))
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(cnres,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
;---Make sure we keep the labelbar resources as part of cnres resources
          delete(cnres)
          cnres = get_res_ne(res2,(/"mp","vp","tx","am","gsnPolar"/))
          set_labelbar_res(cnres,"polar")
        end if
        set_attr(cnres,"cnLineLabelsOn",False)
      end if
      set_attr(cnres,"cnInfoLabelOn",False)
    else
      use_old_lbar = False
      use_mgr_lbar = False
      lbar_on = False
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
    map_object = gsn_csm_map_polar(wks,mpres)       ; Create map.
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

;---Create a labelbar or fix an existing one
    if(use_old_lbar) then
      add_labelbar(wks,contour_object,lbar_zone,\
                   font_height,"polar",lbar_on,lbres)
    end if
    if(use_mgr_lbar) then
      fix_labelbar(contour_object,font_height,lbar_on,cnres)
    end if
  
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
;    3. If the special GSUN resource "gsnZonalMean" is set to True,     ;
;       then a zonal mean XY plot is drawn.                             ;
;   3a. In addition, if "gsnZonalMeanYRefLine" is set, then a vertical  ;
;       line is drawn at this value. Otherwise, a line is drawn at 0.   ;
;    4. If the resource "cnFillOn" is set to True, then a labelbar is   ;
;       drawn and line labels are turned off.                           ;
;    5. If data has an attribute called "long_name," and "gsnLeftString";
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the left string title.                              ;
;    6. If data has an attribute called "units," and "gsnRightString"   ;
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the right string title.                             ;
;***********************************************************************;
undef("gsn_csm_contour_map_ce")
function gsn_csm_contour_map_ce(wks:graphic,data:numeric,resources:logical)
local i, contour_object, labelbar_object, xy_object, map_object, \
calldraw, callframe, use_old_lbar, zonal_mean_plot, min_lat, max_lat, datanew, \
res, res2, lbres, xy_res, mpres, cnres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf, contour_plot, zonal_zone
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_contour_map_ce: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    mpres          = True

    infolabel_zone = 2        ; Zone for info label (may change later)
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    zonal_zone     = 2        ; Zone for zonal means plot

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),\
                       "gsn_csm_contour_map_ce",res2,1)
    end if

; Default is no zonal mean plot.
    zonal_mean_plot = get_res_value(res2,"gsnZonalMean",False)

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
        datanew = gsn_add_cyclic_point(data)
    else
        datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay contour on map.

    check_for_y_lat_coord(datanew,res2,"contour_map")
    check_for_lon_coord(datanew,res2,"contour_map")

    if(.not.(isatt(res2,"sfXArray")))
      set_attr(res2,"sfXCStartV",-180.)
      set_attr(res2,"sfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"sfYArray")))
      set_attr(res2,"sfYCStartV",-90.)
      set_attr(res2,"sfYCEndV",   90.)
    end if

; Check if a zonal mean plot is desired. First make sure data
; is not all missing and that we have valid lat/lon coords.
    if(zonal_mean_plot) then
      if(all(ismissing(datanew))) then
        zonal_mean_plot = False
      end if
      if(.not.(is_valid_latlon_coord(datanew,"y","lat",res2).and. \
               is_valid_latlon_coord(datanew,"x","lon",res2))) then
        print("gsn_csm_contour_map_ce: Warning: The resource gsnZonalMean can only be set to True")
        print("if the data has one of the coordinate variables " + get_allowed_latnames() + "and " + get_allowed_lonnames())
        zonal_mean_plot = False
      end if
    end if

    if(zonal_mean_plot) then
      if(isatt(res2,"vpHeightF"))
        map_vphf = res2@vpHeightF
      else
        map_vphf  = 0.7 ; make sure zonal plot will fit in view port
         set_attr(res2,"vpHeightF",0.7)
        set_attr(res2, "vpWidthF",0.7)
        set_attr(res2,     "vpXF",0.1)
        set_attr(res2,     "vpYF",0.9)
      end if
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
    mpres@gsnTickMarksPointOutward = get_res_value(res2, \
                                     "gsnTickMarksPointOutward",True)

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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
    cnres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm","gsnZonal"/))

;---Labelbar stuff
    if(check_attr(cnres,"cnFillOn",True,False))
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(cnres,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
;---Make sure we keep the labelbar resources as part of cnres resources
          delete(cnres)
          cnres = get_res_ne(res2,(/"mp","vp","tm","tx","am","pm","gsnZonal"/))
          set_labelbar_res(cnres,"ce")
        else
          if(check_attr_enum_value(lbres,"lbOrientation","vertical"))
            set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
            set_attr(mpres,"vpHeightF",0.75)   ; on the side.
            set_attr(mpres,     "vpXF",0.08)
            set_attr(mpres,     "vpYF",0.90)
          end if
        end if
        set_attr(cnres,"cnLineLabelsOn",False)
      end if
      set_attr(cnres,"cnInfoLabelOn",False)
    else
      lbar_on = False
      use_old_lbar = False
      use_mgr_lbar = False
    end if
;
; Compute zones for zonal means plot, info label, and labelbar if
; they are supposed to get drawn.
;
    infolabel_on = get_res_value_keep(cnres,"cnInfoLabelOn",True)

    if(zonal_mean_plot) then
      if(infolabel_on) then
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
    map_object = gsn_csm_map_ce(wks,mpres)             ; Create map.
    overlay(map_object,contour_object)              ; Overlay contours on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    if(isatt(map_object,"tickmarks")) then
      getvalues map_object@tickmarks
        "tmXBLabelFontHeightF" : xbfontf
      end getvalues
   end if
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

    if(isvar("xbfontf")) then
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
    end if

;---Create a labelbar or fix an existing one.
    if(use_old_lbar) then
      add_labelbar(wks,contour_object,lbar_zone,xbfontf,"ce",lbar_on,lbres)
    end if
    if(use_mgr_lbar) then
      fix_labelbar(contour_object,xbfontf,lbar_on,cnres)
    end if

; Add a zonal mean plot if requested.
    if(zonal_mean_plot) then
      zres                         = get_res_eq(res2,"gsnZonal")
      zres                         = True   ; Make sure it is True
      zres@vpWidthF                = 0.15 * map_vphf
      zres@amZone                  = zonal_zone
      zonal_object = gsn_csm_attach_zonal_means(wks,map_object,datanew,zres)

      map_object@xy = zonal_object
    end if  

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    map_object@data    = contour_object@data
    map_object@contour = contour_object
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
;    6. If the special GSUN resource "gsnZonalMean" is set to True,     ;
;       then a zonal mean XY plot is drawn.                             ;
;   6a. In addition, if "gsnZonalMeanYRefLine" is set, then a vertical  ;
;       line is drawn at this value. Otherwise, a line is drawn at 0.   ;
;***********************************************************************;
undef("gsn_csm_contour_map_other")
function gsn_csm_contour_map_other(wks:graphic,data:numeric,\
                                   resources:logical)
local i, contour_object, labelbar_object, xy_object, map_object, \
calldraw, callframe, use_old_lbar, min_lat, max_lat, datanew, \
res, res2, lbres, xy_res, mpres, cnres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf, contour_plot, zonal_mean_plot, zonal_zone
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_contour_map_other: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    mpres          = True
    infolabel_zone = 2        ; Zone for info label (may change later)
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    zonal_zone     = 2        ; Zone for zonal means plot

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float), \
                       "gsn_csm_contour_map_other",res2,1)
    end if

; Default is no zonal mean plot.
    zonal_mean_plot = get_res_value(res2,"gsnZonalMean",False)

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
        datanew = gsn_add_cyclic_point(data)
    else
        datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay contour on map.

    check_for_y_lat_coord(datanew,res2,"contour_map")
    check_for_lon_coord(datanew,res2,"contour_map")

; Check if a zonal mean plot is desired. First make sure data
; is not all missing.

    if(zonal_mean_plot) then
      if(all(ismissing(datanew))) then
        zonal_mean_plot = False
      end if

      if(.not.(is_valid_latlon_coord(datanew,"y","lat",res2).and. \
               is_valid_latlon_coord(datanew,"x","lon",res2))) then
        print("gsn_csm_contour_map_other: Warning: The resource gsnZonalMean can only be set to True")
        print("if the data has one of the coordinate variables " + get_allowed_latnames() + "and " + get_allowed_lonnames())
        zonal_mean_plot = False

      end if
    end if

    if(zonal_mean_plot) then
      if(isatt(res2,"vpHeightF"))
        map_vphf = res2@vpHeightF
      else
        map_vphf  = 0.7 ; make sure zonal plot will fit in view port
         set_attr(res2,"vpHeightF",0.7)
        set_attr(res2, "vpWidthF",0.7)
        set_attr(res2,     "vpXF",0.1)
        set_attr(res2,     "vpYF",0.9)
      end if
    end if

; Create some contour plot resources.

    res2 = True
    set_attr(res2,"cnLineLabelBackgroundColor", "transparent")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT","gsnMask"/))
    cnres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm","gsnMask","gsnZonal"/))

;---Labelbar stuff
    if(check_attr(cnres,"cnFillOn",True,False))
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(cnres,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
;---Make sure we keep the labelbar resources as part of cnres resources
          delete(cnres)
          cnres = get_res_ne(res2,(/"mp","vp","tm","tx","am","pm","gsnMask","gsnZonal"/))
          set_labelbar_res(cnres,"other")
        else
          if(check_attr_enum_value(lbres,"lbOrientation","vertical"))
            set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
            set_attr(mpres,"vpHeightF",0.75)   ; on the side.
            set_attr(mpres,     "vpXF",0.08)
            set_attr(mpres,     "vpYF",0.85)
          end if
        end if
        set_attr(cnres,"cnLineLabelsOn",False)
      end if
      set_attr(cnres,"cnInfoLabelOn",False)
    else
      use_old_lbar = False
      use_mgr_lbar = False
      lbar_on = False
    end if
;
; Compute zones for info label and labelbar if
; they are supposed to get drawn.
;
    infolabel_on = get_res_value_keep(cnres,"cnInfoLabelOn",True)

    if(zonal_mean_plot) then
      if(infolabel_on) then
        infolabel_zone = zonal_zone + 1
        lbar_zone      = zonal_zone + 2
      else
        lbar_zone      = zonal_zone + 1
      end if
    else
      if(infolabel_on)
        lbar_zone = infolabel_zone + 1
        cnres@cnInfoLabelZone = infolabel_zone
      end if
    end if

;
; Increase labelbar zone if map tickmarks are on.
;
    if(lbar_on.and.use_old_lbar.and.\
        (check_attr_enum_value(mpres,"pmTickMarkDisplayMode","Always").or.\
         check_attr_enum_value(mpres,"pmTickMarkDisplayMode","Conditional").or.\
         check_attr_enum_value(mpres,"pmTickMarkDisplayMode","ForceAlways"))) then
      lbar_zone = lbar_zone + 1
    end if

; Before we create the objects, turn off draw and frame for them.
    cnres           = True
    mpres           = True
    cnres@gsnDraw   = False
    cnres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    map_object = gsn_csm_map_other(wks,mpres)       ; Create map.
    overlay(map_object,contour_object)              ; Overlay contours on map.

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues map_object
      "tmXBLabelFontHeightF" : xbfontf
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

; Create a labelbar or fix existing one.
    if(use_old_lbar) then
      add_labelbar(wks,contour_object,lbar_zone,\
                   0.6 * font_height,"other",lbar_on,lbres)
    end if
    if(use_mgr_lbar) then
      fix_labelbar(contour_object,0.6*font_height,lbar_on,cnres)
    end if

; Add a zonal mean plot if requested.
    if(zonal_mean_plot) then
      zres                      = get_res_eq(res2,"gsnZonal")
      zres                      = True            ; Make sure it is True
      zres@vpWidthF             = 0.15 * map_vphf
      zres@tmXBLabelFontHeightF = 0.6*font_height      
      zonal_object = gsn_csm_attach_zonal_means(wks,map_object,datanew,zres)
      map_object@xy = zonal_object
    end if  

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_vector_map_ce")
function gsn_csm_vector_map_ce(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                               resources:logical)
local i, vector_object, map_object, res, res2, calldraw, vcres, \
lbres, map_vpwf, map_vphf, vpwf, vphf, callframe, min_lat, max_lat, \
use_old_lbar, lbar_zone, unew, vnew
begin

; Initialize.
    annolabel_zone = 2        ; Zone for vector anno label
    mpres          = True

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_vector_map_ce",res2,2)
    end if

    if(isatt(res2,"vcRefAnnoOrthogonalPosF")) then
      user_set_ref_orth = True
    else
      user_set_ref_orth = False
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",False)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vector on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
      set_attr(res2,"vfYCStartV",-90.)
      set_attr(res2,"vfYCEndV",   90.)
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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

;---Labelbar stuff
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    if(check_attr(vcres,"vcMonoLineArrowColor",False,False))
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(vcres,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
;---Make sure we keep the labelbar resources as part of vcres resources
          delete(vcres)
          vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","pm"/))
          set_labelbar_res(vcres,"vector")
        else
          if(check_attr_enum_value(lbres,"lbOrientation","vertical"))
            set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
            set_attr(mpres,"vpHeightF",0.75)   ; on the side.
            set_attr(mpres,     "vpXF",0.08)
            set_attr(mpres,     "vpYF",0.90)
          end if
        end if
      end if
    else
      use_old_lbar = False
      use_mgr_lbar = False
      lbar_on = False
    end if
;
; Default is for the vector reference anno label to be on.  To be
; turned off, it has to be explicitly turned off.
;
    refanno_on = get_res_value_keep(vcres,"vcRefAnnoOn",True)
    if(refanno_on)
;
; If the user is moving the vector annotation around, then 
; the labelbar will no longer be in the zone outside the ref anno.
; This is to avoid the problem of the user moving the ref anno into
; the plot, and then having the labelbar follow it up too far, and
; running into the tickmarks.
;
      if(user_set_ref_orth) then
        lbar_zone = annolabel_zone
      else
        lbar_zone = annolabel_zone + 1
      end if
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

    map_object = gsn_csm_map_ce(wks,mpres)          ; Create map.
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

; Create a labelbar or fix an existing one.
    if(use_old_lbar) then
      add_labelbar(wks,vector_object,lbar_zone,xbfontf,"vector",lbar_on,lbres)
    end if
    if(use_mgr_lbar) then
      fix_labelbar(vector_object,xbfontf,lbar_on,vcres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_vector_map_polar")
function gsn_csm_vector_map_polar(wks:graphic,u[*][*]:numeric, \
                                  v[*][*]:numeric,resources:logical)
local i, vector_object, map_object, res, res2, calldraw, vcres, \
lbres, map_vpwf, map_vphf, vpwf, vphf, callframe, min_lat, max_lat, \
use_old_lbar, lbar_zone, unew, vnew
begin

; Initialize.
    annolabel_zone = 2        ; Zone for vector anno label
    lbar_zone      = 2        ; Zone for labelbar (may change later)
    mpres          = True
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_vector_map_polar",res2,2)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",False)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

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

; Create data object and use coordinate variables if they exist.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO", \
                       "pmT","gsnPolar"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm","gsnPolar"/))
    lbres = get_res_eq(res2,(/"lb","pm"/))

;---Labelbar stuff
    if(check_attr(vcres,"vcMonoLineArrowColor",False,False))
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(vcres,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
;---Make sure we keep the labelbar resources as part of vcres resources
          delete(vcres)
          vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","pm","gsnPolar"/))
          set_labelbar_res(vcres,"polar")
        end if
      end if
    else
      use_old_lbar = False
      use_mgr_lbar = False
      lbar_on = False
    end if

    if(lbar_on.and.use_old_lbar)
      if(check_attr_enum_value(lbres,"lbOrientation","vertical"))
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
    map_object    = gsn_csm_map_polar(wks,mpres)          ; Create map.
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

; Create a labelbar or fix an existing one.
    if(use_old_lbar) then
      add_labelbar(wks,vector_object,lbar_zone, \
                   font_height,"vector",lbar_on,lbres)
    end if
    if(use_mgr_lbar) then
      fix_labelbar(vector_object,font_height,lbar_on,vcres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_vector_map_other")
function gsn_csm_vector_map_other(wks:graphic,u[*][*]:numeric,\
                                  v[*][*]:numeric,resources:logical)
local i, vector_object, labelbar_object, xy_object, map_object, \
calldraw, callframe, use_old_lbar, min_lat, max_lat, datanew, \
res, res2, lbres, xy_res, mpres, vcres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf 
begin
; Initialize.
    mpres = True
    res2  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_vector_map_other",res2,2)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",False)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vectors on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT","gsnMask"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm","gsnMask"/))

;---Labelbar stuff
    if(check_attr(vcres,"vcMonoLineArrowColor",False,False))
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(vcres,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
;---Make sure we keep the labelbar resources as part of vcres resources
          delete(vcres)
          vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","pm","gsnMask"/))
          set_labelbar_res(vcres,"other")
        end if
      end if
    else
      use_old_lbar = False
      use_mgr_lbar = False
      lbar_on = False
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
    map_object = gsn_csm_map_other(wks,mpres)             ; Create map.
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

; Create a labelbar or fix an existing one.
    if(use_old_lbar) then
      add_labelbar(wks,vector_object,2,0.6 * font_height,"other",lbar_on,lbres)
    end if
    if(use_mgr_lbar) then
      fix_labelbar(vector_object,0.6*font_height,lbar_on,vcres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
;                                                                       ;
; The labelbar stuff was never added to this routine until V6.1.0, so I ;
; only added the new labelbar.                                          ;
;***********************************************************************;
undef("gsn_csm_streamline_map_ce")
function gsn_csm_streamline_map_ce(wks:graphic,u[*][*]:numeric,\
                                   v[*][*]:numeric,resources:logical)
local i, stream_object, map_object, stres, calldraw, callframe, \
min_lat, max_lat, unew, vnew, res, res2, map_vpwf, map_vphf, vpwf, vphf
begin

; Initialize.
    infolabel_zone = 2        ; Zone for info label (may change later)
    mpres          = True
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_streamline_map_ce",res2,2)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",False)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for coordinate variables. These values will determine where to 
; overlay streamline on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
      set_attr(res2,"vfYCStartV",-90.)
      set_attr(res2,"vfYCEndV",   90.)
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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

;---Labelbar stuff. We only use the PlotManager labelbar here, just because.
    use_mgr_lbar = False
    if(check_attr(stres,"stMonoLineColor",False,False))
      lbar_on = get_res_value(stres,"lbLabelBarOn",True)
      if(lbar_on)
        use_mgr_lbar = True
        set_labelbar_res(stres,"ce")
      end if
    else
      lbar_on = get_res_value(stres,"lbLabelBarOn",False) ; make sure to remove it
    end if

; Before we create the objects, turn off draw and frame for them.
    stres = True
    mpres = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    stream_object = gsn_streamline(wks,unew,vnew,stres) ; Create streamlines

    map_object = gsn_csm_map_ce(wks,mpres)   ; Create map.
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

;---Create a labelbar.
    if(use_mgr_lbar)
      fix_labelbar(stream_object,font_height,lbar_on,stres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_streamline_map_polar")
function gsn_csm_streamline_map_polar(wks:graphic,u[*][*]:numeric, \
                                      v[*][*]:numeric,resources:logical)
local i, stream_object, map_object, res, res2, calldraw, stres, \
use_mgr_lbar, map_vpwf, map_vphf, vpwf, vphf, callframe, unew, vnew
begin

; Initialize.
    mpres          = True
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_streamline_map_polar",res2,2)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",False)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

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

; Create data object and use coordinate variables if they exist.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO", \
                              "pmT","gsnPolar"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","tx","am","gsnPolar"/))

;---Labelbar stuff
    if(check_attr(stres,"stMonoLineColor",False,False))
      lbar_on = get_res_value(stres,"lbLabelBarOn",True)
      use_mgr_lbar = True
      set_labelbar_res(stres,"polar")
    else
      lbar_on = get_res_value(stres,"lbLabelBarOn",False)  ; to make sure it is not set
      use_mgr_lbar = False
    end if

; Before we create the objects, turn off draw and frame for them.
    stres = True
    mpres = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    stream_object = gsn_streamline(wks,unew,vnew,stres) ; Create streamlines
    map_object    = gsn_csm_map_polar(wks,mpres)        ; Create map.
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

;---Create a labelbar.
    if(use_mgr_lbar)
      fix_labelbar(stream_object,font_height,lbar_on,stres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
;                                                                       ;
; The labelbar stuff was never added to this routine until V6.1.0, so I ;
; only added the new labelbar.                                          ;
;***********************************************************************;
undef("gsn_csm_streamline_map_other")
function gsn_csm_streamline_map_other(wks:graphic,u[*][*]:numeric,\
                                      v[*][*]:numeric,resources:logical)
local i, stream_object, labelbar_object, xy_object, map_object, \
calldraw, callframe, min_lat, max_lat, datanew, use_mgr_lbar, \
res, res2, mpres, stres, map_vpwf, map_vphf, vphf 
begin
; Initialize.
    mpres          = True

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_streamline_map_other",res2,2)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",False)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

; Check for coordinate variables. These values will determine where to 
; overlay streamlines on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT","gsnMask"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT","gsnMask"/))

;---Labelbar stuff
    if(check_attr(stres,"stMonoLineColor",False,False))
      lbar_on = get_res_value(stres,"lbLabelBarOn",True)
      use_mgr_lbar = True
      set_labelbar_res(stres,"other")
    else
      lbar_on = get_res_value(stres,"lbLabelBarOn",False)
      use_mgr_lbar = False
    end if

; Before we create the objects, turn off draw and frame for them.
    stres           = True
    mpres           = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    stream_object = gsn_streamline(wks,unew,vnew,stres) ; Create streamlines.
    map_object = gsn_csm_map_other(wks,mpres)     ; Create map.
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

;---Create a labelbar.
    if(use_mgr_lbar)
      fix_labelbar(stream_object,font_height,lbar_on,stres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_streamline_contour_map_ce")
function gsn_csm_streamline_contour_map_ce(wks:graphic,u[*][*]:numeric,\
                   v[*][*]:numeric,data:numeric,resources:logical)
local i, stream_object, map_object, res, res2, \
calldraw, callframe, min_lat, max_lat, unew, vnew, datanew, \
map_vpwf, map_vphf, vpwf, vphf, use_old_lbar
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_streamline_contour_map_ce: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    infolabel_on   = False
    mpres          = True
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_contour_map_ce",res2,3)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vector on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
      set_attr(res2,"vfYCStartV",-90.)
      set_attr(res2,"vfYCEndV",   90.)
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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

;---Labelbar stuff
    if(check_attr(res2,"cnFillOn",True,False))
      set_attr(res2,"cnFillDrawOrder","Predraw")
      set_attr(res2,"cnInfoLabelOn",False)
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        set_attr(res2,"cnLineLabelsOn",False)
        set_attr(res2,"cnInfoLabelOn",False)
        if(use_mgr_lbar) then
          set_labelbar_res(res2,"vector")
        end if
      end if
    else
      use_old_lbar = False
      use_mgr_lbar = False
      lbar_on = False
    end if

    check_for_y_lat_coord(datanew,res2,"contour_map")
    check_for_lon_coord(datanew,res2,"contour_map")

    if(.not.(isatt(res2,"sfXArray")))
      set_attr(res2,"sfXCStartV",-180.)
      set_attr(res2,"sfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"sfYArray")))
      set_attr(res2,"sfYCStartV",-90.)
      set_attr(res2,"sfYCEndV",   90.)
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

    if(lbar_on.and.use_old_lbar.and.\
       check_attr_enum_value(lbres,"lbOrientation","vertical"))
      set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
      set_attr(mpres,"vpHeightF",0.75)   ; on the side.
      set_attr(mpres,     "vpXF",0.08)
      set_attr(mpres,     "vpYF",0.90)
    end if

    set_attr(stres,"cnInfoLabelZone",2)
    set_attr(stres,"cnInfoLabelOrthogonalPosF", 0.13)

    stream_object = gsn_streamline_contour(wks,unew,vnew,datanew,stres)

    map_object = gsn_csm_map_ce(wks,mpres)    ; Create map.
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

; Create a labelbar or fix an existing one
    if(use_old_lbar)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
      add_labelbar(wks,stream_object@contour,lbar_zone,xbfontf,"vector",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(stream_object@contour,xbfontf,lbar_on,stres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_streamline_contour_map_polar")
function gsn_csm_streamline_contour_map_polar(wks:graphic,u[*][*]:numeric, \
                                         v[*][*]:numeric,data:numeric,
                                         resources:logical)
local i, stream_object, map_object, res, res2, calldraw, vcres, \
lbres, vpwf, vphf, callframe, use_old_lbar, lbar_zone, unew, vnew
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_streamline_contour_map_polar: Fatal: the input data array must be 1D or 2D")
      return
    end if

;
; The default is to draw streamlines colored by a scalar field, so this
; means a labelbar should be drawn, and no contour information is 
; needed.
;
; Initialize.
    res2  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_contour_map_polar",res2,3)
    end if

    mpres = True

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay streamlines on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

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

;---Labelbar stuff
    if(check_attr(res2,"cnFillOn",True,False))
      set_attr(res2,"cnFillDrawOrder","Predraw")
      set_attr(res2,"cnLineLabelsOn",False)
      set_attr(res2,"cnLinesOn",False)
      set_attr(res2,"cnInfoLabelOn",False)
      lbar_on = get_res_value(res2,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(use_mgr_lbar) then
        set_labelbar_res(res2,"polar")
      end if
    else    
      set_attr(res2,"cnInfoLabelOn",True)
      set_attr(res2,"lbLabelBarOn",False)
      use_old_lbar = False
      use_mgr_lbar = False
      lbar_on = False
    end if
;
; Check for coordinate variables for scalar field.
;
    check_for_y_lat_coord(datanew,res2,"contour_map")
    check_for_lon_coord(datanew,res2,"contour_map")

    lbar_orient  = str_lower(get_res_value_keep(res2,"lbOrientation", \
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

    if(lbar_on.and.use_old_lbar)
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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO", \
                              "pmT","gsnPolar"/))
    stres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm","gsnPolar"/))

; Before we create the objects, turn off draw and frame for them.
    stres = True
    mpres = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    stream_object = gsn_streamline_contour(wks,unew,vnew,datanew,stres)
    map_object    = gsn_csm_map_polar(wks,mpres)    ; Create map.
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

; Create a labelbar or fix an existing one.
    if(use_old_lbar)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
    end if
    if(use_old_lbar)
      lbres = get_res_eq(res2,(/"lb","pm"/))
      add_labelbar(wks,stream_object@contour, \
                   lbar_zone,font_height,"vector",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(stream_object@contour,font_height,lbar_on,stres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_streamline_contour_map_other")
function gsn_csm_streamline_contour_map_other(wks:graphic,u[*][*]:numeric,\
                                        v[*][*]:numeric,data:numeric, \
                                        resources:logical)
local i, stream_object, labelbar_object, map_object, \
calldraw, callframe, use_old_lbar, min_lat, max_lat, datanew, \
res, res2, lbres, mpres, vcres, levels, colors, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf 
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_streamline_contour_map_other: Fatal: the input data array must be 1D or 2D")
      return
    end if
; Initialize.
    infolabel_on   = False
    mpres          = True

    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_contour_map_other",res2,3)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay streamlines on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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
    check_for_y_lat_coord(datanew,res2,"contour_map")
    check_for_lon_coord(datanew,res2,"contour_map")
;
; Labelbar stuff
;
; This is a bit difference because we have to check for labelbars
; for both contours and streamlines.
;
    lbar_on = get_res_value_keep(res2,"lbLabelBarOn",True)
    if(lbar_on)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(use_mgr_lbar) then
        set_labelbar_res(res2,"contour")
      end if
    else
      use_old_lbar = False
      use_mgr_lbar = False
    end if
    lbar_orient  = str_lower(get_res_value_keep(res2,"lbOrientation", \
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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT","gsnMask"/))
    if(use_old_lbar) then
      stres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm","gsnMask"/))
    else
      stres = get_res_ne(res2,(/"mp","vp","tm","tx","am","pm","gsnMask"/))
    end if

; Before we create the objects, turn off draw and frame for them.
    stres           = True
    mpres           = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    stream_object = gsn_streamline_contour(wks,unew,vnew,datanew,stres)

    map_object = gsn_csm_map_other(wks,mpres)   ; Create map.
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
                                     2.*font_height)
    setvalues map_object
      "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Create a labelbar or fix an existing one.
    if(use_old_lbar)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
    end if
    if(use_old_lbar)
      lbres = get_res_eq(res2,(/"lb","pm"/))
      add_labelbar(wks,stream_object@contour,lbar_zone,font_height,"vector",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(stream_object@contour,font_height,lbar_on,stres)
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_vector_scalar")
function gsn_csm_vector_scalar(wks:graphic,u[*][*]:numeric,v[*][*]:numeric, \
                               data:numeric,resources:logical)
local i, vector_object, res, res2, use_old_lbar, use_mgr_lbar, \
calldraw, callframe, unew, vnew, datanew, vpwf, vphf, use_old_lbar, lbar_type
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_vector_scalar: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    main_zone      = 2         ; Zone for main title (may change later)
    infolabel_on   = False
    refanno_on     = True
    res2 = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_vector_scalar",res2,3)
    end if

    tm_scale      = get_res_value_keep(res2,"gsnScale",True)
    point_outward = get_res_value(res2,"gsnTickMarksPointOutward",True)

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"",True)) then
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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
    check_for_coord_arrays(unew,res2,"vector")

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for type of scalar field representation.
    scalar_contour = get_res_value(res2,"gsnScalarContour",False)

;---Labelbar stuff
    lbar_type = ""
    lbres = get_res_eq(res2,(/"lb","pm"/))
    if(scalar_contour)
      if(check_attr(res2,"cnFillOn",True,False))
        lbar_type = "contour"
        set_attr(res2,"cnFillDrawOrder","Predraw")
        set_attr(res2,"cnInfoLabelOn",False)
        lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
        use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
        use_mgr_lbar = .not.use_old_lbar
        if(lbar_on)
          set_attr(res2,"cnLineLabelsOn",False)
          if(use_mgr_lbar) then
            set_labelbar_res(res2,"contour")
          end if
        end if
      else
        lbar_on = False
        use_old_lbar = False
        use_mgr_lbar = False
      end if
    else
;
; Check for a vector labelbar. vcMonoLineArrowColor is set to False
; by gsn_vector_scalar, so you have to check for it explicitly
; being True here.
;
      if(.not.check_attr(res2,"vcMonoLineArrowColor",True,False))
        lbar_type = "vector"
        lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
        use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
        use_mgr_lbar = .not.use_old_lbar
        if(lbar_on)
          if(use_mgr_lbar) then
            set_labelbar_res(res2,"vector")
          end if
        end if
      else
        lbar_on = False
        use_old_lbar = False
        use_mgr_lbar = False
      end if
    end if

; Turn off anno label if specified by user.
    if(check_attr(res2,"vcRefAnnoOn",False,False))
      refanno_on = False
    end if

; Turn off info label if labelbar is on, unless otherwise specified by user.
    if(scalar_contour)
      if(lbar_on.and.(use_old_lbar.or.use_mgr_lbar))
        set_attr(res2,"cnInfoLabelOn",False)
      else
        set_attr(res2,"cnInfoLabelOn",True)
      end if
      infolabel_on = res2@cnInfoLabelOn
    end if

    if(lbar_on.and.use_old_lbar.and.\
       check_attr_enum_value(lbres,"lbOrientation","vertical")) then
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
    if(lbar_on.and.use_mgr_lbar) then
      vcres = get_res_ne(res2,(/"mp","tx","am"/))
    else
      vcres = get_res_ne(res2,(/"mp","lb","tx","am","pm"/))
    end if

    vcres          = True
    vcres@gsnDraw  = False
    vcres@gsnFrame = False
    vcres@gsnScale = tm_scale  ; Force labels and ticks to be same, if True.

    if(scalar_contour)
      base_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
    else
      base_object = gsn_vector_scalar(wks,unew,vnew,datanew,vcres)
    end if

; Add lat/lon labels to X/Y axes if appropriate coordinate arrays exist.

    add_latlon_labels(base_object,unew,res2)

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues base_object
      "vpHeightF"            : height
      "vpWidthF"             : width

      "tmXBLabelFontHeightF" : xbfontf
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
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
;
; If gsnScale is True, then make sure we have a valid tickmark
; object before we do anything else.
;
    if(tm_scale.and.is_tm_obj_valid(base_object)) then
;
; Make tick marks same length and point outward.
;
      getvalues base_object
        "tmYLMajorLengthF"     : ylength
        "tmXBMajorLengthF"     : xlength
        "tmYLMinorLengthF"     : ymlength
        "tmXBMinorLengthF"     : xmlength
      end getvalues

      major_length = min((/ylength,xlength/))    ; New length for major ticks.
      minor_length = min((/ymlength,xmlength/))  ; New length for minor ticks.

      major_length = scale * major_length
      minor_length = scale * minor_length

      tmres = get_res_eq(res2,"tm")
      gsnp_point_tickmarks_outward(base_object,tmres,xlength,ylength,\
                                   xmlength,ymlength,\
                                   major_length,minor_length,point_outward)
    end if

; Make sure info labels, line labels, and vector labels are same size.
    if(scalar_contour)
      if(.not.isatt(res2,"cnInfoLabelFontHeightF"))
        setvalues base_object
          "cnInfoLabelFontHeightF" : xbfontf
       end setvalues
      end if
      if(.not.isatt(res2,"cnLineLabelFontHeightF"))
        setvalues base_object
          "cnLineLabelFontHeightF"    : xbfontf
        end setvalues
      end if
    end if

    if(.not.isatt(res2,"vcRefAnnoFontHeightF"))
      if(scalar_contour) then
        setvalues base_object@vector
          "vcRefAnnoFontHeightF" : xbfontf
        end setvalues
      else
        setvalues base_object
          "vcRefAnnoFontHeightF" : xbfontf
        end setvalues
      end if
    end if

;---Create a labelbar or fix an existing one.
    if(use_old_lbar)
      if(.not.refanno_on.and..not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
    end if
    if(scalar_contour)
      if(use_old_lbar)
        add_labelbar(wks,base_object,lbar_zone,xbfontf, \
                     "vector",lbar_on,lbres)
      end if
      if(use_mgr_lbar)
        fix_labelbar(base_object,xbfontf,lbar_on,vcres)
      end if
    else
      if(use_old_lbar)
        add_labelbar(wks,base_object,lbar_zone,xbfontf, \
                     "vector",lbar_on,lbres)
      end if
      if(use_mgr_lbar)
        fix_labelbar(base_object,xbfontf,lbar_on,vcres)
      end if
    end if
  
; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,base_object,left_string,center_string,\
                  right_string,subres)

; Draw all this stuff: vector/contour plot, subtitles, and tick marks.
    draw_and_frame(wks,base_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    base_object@vfdata  = base_object@vfdata
    base_object@sfdata  = base_object@sfdata
    if(lbar_type.eq."contour") then
      base_object@labelbar      = base_object
      base_object@labelbar_type = lbar_type
    end if
    return(base_object)
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
undef("gsn_csm_vector_scalar_map_ce")
function gsn_csm_vector_scalar_map_ce(wks:graphic,u[*][*]:numeric,\
                   v[*][*]:numeric,data:numeric,resources:logical)
local i, vector_object, map_object, res, res2, \
calldraw, callframe, min_lat, max_lat, unew, vnew, datanew, \
map_vpwf, map_vphf, vpwf, vphf, use_old_lbar, use_mgr_lbar, lbar_type
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_vector_scalar_map_ce: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    infolabel_on   = False
    refanno_on     = True
    mpres          = True
    res2           = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_vector_scalar_map_ce",res2,3)
    end if

    if(isatt(res2,"vcRefAnnoOrthogonalPosF")) then
      user_set_ref_orth = True
    else
      user_set_ref_orth = False
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vector on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
      set_attr(res2,"vfYCStartV",-90.)
      set_attr(res2,"vfYCEndV",   90.)
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

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

;---Labelbar stuff
    lbar_type = ""
    if(scalar_contour)
      if(check_attr(res2,"cnFillOn",True,False))
        lbar_type = "contour"
        set_attr(res2,"cnFillDrawOrder","Predraw")
        set_attr(res2,"cnInfoLabelOn",False)
        lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
        use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
        use_mgr_lbar = .not.use_old_lbar
        if(lbar_on) then
          if(use_mgr_lbar) then
            set_labelbar_res(res2,"contour")
          end if
          set_attr(res2,"cnLineLabelsOn",False)
          set_attr(res2,"cnInfoLabelOn",False)
        end if
      else
        lbar_on = False
        use_old_lbar = False
        use_mgr_lbar = False
      end if

      check_for_y_lat_coord(datanew,res2,"contour_map")
      check_for_lon_coord(datanew,res2,"contour_map")

      if(.not.(isatt(res2,"sfXArray")))
        set_attr(res2,"sfXCStartV",-180.)
        set_attr(res2,"sfXCEndV",   180.)
      end if

      if(.not.(isatt(res2,"sfYArray")))
        set_attr(res2,"sfYCStartV",-90.)
        set_attr(res2,"sfYCEndV",   90.)
      end if
    else
;
; Check for a vector labelbar. vcMonoLineArrowColor is set to False
; by gsn_vector_scalar, so you have to check for it explicitly
; being True here.
;
      if(.not.check_attr(res2,"vcMonoLineArrowColor",True,False))
        lbar_type = "vector"
        lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
        use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
        use_mgr_lbar = .not.use_old_lbar
        if(lbar_on) then
          if(use_mgr_lbar) then
            set_labelbar_res(res2,"vector")
          end if
        end if
      else
        lbar_on = False
        use_old_lbar = False
        use_mgr_lbar = False
      end if
    end if

; Turn off anno label if specified by user.
    if(check_attr(res2,"vcRefAnnoOn",False,False))
      refanno_on = False
    end if

; Turn off info label if labelbar is on, unless otherwise specified by user.
    if(scalar_contour)
      if(lbar_on.and.(use_old_lbar.or.use_mgr_lbar))
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
    if(use_mgr_lbar.and.lbar_on) then
      vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am"/))
    else
      vcres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))
    end if

; Before we create the objects, turn off draw and frame for them.
    vcres          = True
    mpres          = True
    vcres@gsnDraw  = False
    vcres@gsnFrame = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    if(lbar_on.and.use_old_lbar.and.\
       check_attr_enum_value(lbres,"lbOrientation","vertical"))
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
      base_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
    else
      base_object = gsn_vector_scalar(wks,unew,vnew,datanew,vcres)
    end if

    map_object = gsn_csm_map_ce(wks,mpres)          ; Create map.
    overlay(map_object,base_object)               ; Overlay vectors on map.

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
      if(.not.isatt(vcres,"cnInfoLabelFontHeightF"))
        setvalues base_object
          "cnInfoLabelFontHeightF" : xbfontf
       end setvalues
      end if
      if(.not.isatt(vcres,"cnLineLabelFontHeightF"))
        setvalues base_object
          "cnLineLabelFontHeightF"    : xbfontf
        end setvalues
      end if
    end if

    if(.not.isatt(vcres,"vcRefAnnoFontHeightF"))
      if(scalar_contour) then
        setvalues base_object@vector
          "vcRefAnnoFontHeightF" : xbfontf
        end setvalues
      else
        setvalues base_object
          "vcRefAnnoFontHeightF" : xbfontf
        end setvalues
      end if
    end if

; Create a labelbar.

    if(use_old_lbar)
      if(.not.refanno_on.and..not.infolabel_on)
        lbar_zone = 2
      else
;
; If the user is moving the vector annotation around, then 
; the labelbar will no longer be in the zone outside the ref anno.
; This is to avoid the problem of the user moving the ref anno into
; the plot, and then having the labelbar follow it up too far, and
; running into the tickmarks.
;
        if(user_set_ref_orth) then
          lbar_zone = 2
        else
          lbar_zone = 3
        end if
      end if
    end if
    if(scalar_contour)
      if(use_old_lbar)
        add_labelbar(wks,base_object,lbar_zone,xbfontf, \
                     "vector",lbar_on,lbres)
      end if
      if(use_mgr_lbar)
        fix_labelbar(base_object,xbfontf,lbar_on,vcres)
      end if
    else
      if(use_old_lbar)
        add_labelbar(wks,base_object,lbar_zone,xbfontf,"vector",lbar_on,lbres)
      end if
      if(use_mgr_lbar)
        fix_labelbar(base_object,xbfontf,lbar_on,vcres)
      end if
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    if(lbar_type.eq."contour") then
      map_object@labelbar = base_object
      map_object@labelbar_type = lbar_type
    end if

    map_object@vfdata  = base_object@vfdata
    map_object@sfdata  = base_object@sfdata
    if(scalar_contour)
      map_object@contour = base_object
      map_object@vector  = base_object@vector
    else
      map_object@vector  = base_object
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
undef("gsn_csm_vector_scalar_map_polar")
function gsn_csm_vector_scalar_map_polar(wks:graphic,u[*][*]:numeric, \
                                         v[*][*]:numeric,data:numeric,
                                         resources:logical)
local i, vector_object, map_object, res, res2, calldraw, vcres, lbres, \
vpwf, vphf, callframe, use_old_lbar, use_mgr_lbar, lbar_type, lbar_zone, unew, vnew
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_vector_scalar_map_polar: Fatal: the input data array must be 1D or 2D")
      return
    end if
;
; The default is to draw vectors colored by a scalar field, so this
; means a labelbar should be drawn, and no contour information is 
; needed.
;
; Initialize.
    res2  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_vector_scalar_map_polar",res2,3)
    end if

    mpres = True

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vectors on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

; Check for draw and frame.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

;
; Check for type of polar plot and scalar field representation.
;
    mpres@gsnPolar     = get_polar_type(res2)
    mpres@gsnPolarTime = get_res_value(res2,"gsnPolarTime",False)
    mpres@gsnPolarUT   = get_res_value(res2,"gsnPolarUT",0.)
    scalar_contour     = get_res_value(res2,"gsnScalarContour",False)

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
      check_for_y_lat_coord(datanew,res2,"contour_map")
      check_for_lon_coord(datanew,res2,"contour_map")
    end if

;---Labelbar stuff
    lbar_type = ""
    if(scalar_contour)
      if(check_attr(res2,"cnFillOn",True,False))
        lbar_on = get_res_value(res2,"lbLabelBarOn",True)
        use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
        use_mgr_lbar = .not.use_old_lbar
        lbar_type = "contour"
        set_attr(res2,"cnFillDrawOrder","Predraw")
        set_attr(res2,"cnInfoLabelOn",False)
        if(lbar_on) then
          if(use_mgr_lbar) then
            set_labelbar_res(res2,"polar")
          end if
          set_attr(res2,"cnInfoLabelOn",False)
        end if
      else
        lbar_on = get_res_value(res2,"lbLabelBarOn",False)   ; make sure this resource gets removed from res2
        use_old_lbar = False
        use_mgr_lbar = False
      end if
    else
;
; Check for a vector labelbar. vcMonoLineArrowColor is set to False
; by gsn_vector_scalar, so you have to check for it explicitly
; being True here.
;
      if(.not.check_attr(res2,"vcMonoLineArrowColor",True,False))
        lbar_on = get_res_value(res2,"lbLabelBarOn",True)
        use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
        use_mgr_lbar = .not.use_old_lbar
        lbar_type = "vector"
        if(lbar_on) then
          if(use_mgr_lbar) then
            set_labelbar_res(res2,"polar")
          end if
        end if
      else
        lbar_on = get_res_value(res2,"lbLabelBarOn",False)   ; make sure this resource gets removed from res2
        use_old_lbar = False
        use_mgr_lbar = False
      end if
    end if

    lbar_orient  = str_lower(get_res_value_keep(res2,"lbOrientation", \
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

    if(lbar_on.and.use_old_lbar)
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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO",\
                              "pmT","gsnPolar"/))

    if(lbar_on.and.use_old_lbar) then
      vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm","gsnPolar"/))
    else
      vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","gsnPolar"/))
    end if

; Before we create the objects, turn off draw and frame for them.
    vcres = True
    mpres = True
    vcres@gsnDraw   = False
    vcres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    if(scalar_contour)
      base_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
    else
      base_object = gsn_vector_scalar(wks,unew,vnew,datanew,vcres)
    end if

    map_object    = gsn_csm_map_polar(wks,mpres)    ; Create map.
    overlay(map_object,base_object)               ; Overlay vectors on map.

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

    if(use_old_lbar)
      if(.not.refanno_on.and..not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
      lbres = get_res_eq(res2,(/"lb","pm"/))
    end if

    if(scalar_contour)
      if(use_old_lbar)
        add_labelbar(wks,base_object,lbar_zone,font_height, \
                     "vector",lbar_on,lbres)
      end if
      if(use_mgr_lbar)
        fix_labelbar(base_object,font_height,lbar_on,vcres)
      end if
    else
      if(use_old_lbar)
        add_labelbar(wks,base_object,lbar_zone,font_height,"vector",lbar_on,lbres)
      end if
      if(use_mgr_lbar)
        fix_labelbar(base_object,font_height,lbar_on,vcres)
      end if
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    if(lbar_type.eq."contour") then
      map_object@labelbar      = base_object
      map_object@labelbar_type = lbar_type
    end if

    map_object@vfdata  = base_object@vfdata
    map_object@sfdata  = base_object@sfdata
    if(scalar_contour)
      map_object@contour = base_object
      map_object@vector  = base_object@vector
    else
      map_object@vector  = base_object
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
undef("gsn_csm_vector_scalar_map_other")
function gsn_csm_vector_scalar_map_other(wks:graphic,u[*][*]:numeric,\
                                        v[*][*]:numeric,data:numeric, \
                                        resources:logical)
local i, vector_object, labelbar_object, map_object, \
calldraw, callframe, use_old_lbar, lbar_type, min_lat, max_lat, datanew, \
res, res2, lbres, mpres, vcres, levels, colors, use_mgr_lbar, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf 
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_vector_scalar_map_other: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    infolabel_on   = False
    refanno_on     = True
    mpres          = True
    res2           = get_resources(resources)

;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_vector_scalar_map_other",res2,3)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to 
; overlay vectors on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

; Check for type of scalar field representation.
    scalar_contour = get_res_value(res2,"gsnScalarContour",False)

; 
; Labelbar stuff
;
; Determine which annotations (labelbar, vector reference anno, info label)
; should be turned on.  By default, labelbar and vector anno are on, and
; contour info label is off.
;
    lbar_on        = False
    lbar_type      = ""
    use_old_lbar   = False    ; Default is no labelbar.
    use_mgr_lbar   = False
    if(scalar_contour)
      if(check_attr(res2,"cnFillOn",True,False))
        lbar_type = "contour"
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
      check_for_y_lat_coord(datanew,res2,"contour_map")
      check_for_lon_coord(datanew,res2,"contour_map")
    end if

    lbar_type = ""
    if(scalar_contour)
; Turn on labelbar if cnFillOn is True and it wasn't explicitly 
; turned off.
      if(check_attr(res2,"cnFillOn",True,False))
        lbar_type = "contour"
        set_attr(res2,"cnFillDrawOrder","Predraw")
        set_attr(res2,"cnInfoLabelOn",False)
        lbar_on = get_res_value(res2,"lbLabelBarOn",True)
        use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
        use_mgr_lbar = .not.use_old_lbar
        if(lbar_on) then
          if(use_mgr_lbar) then
            set_labelbar_res(res2,"contour")
          end if
          set_attr(res2,"cnInfoLabelOn",False)
        end if
      else
        lbar_on = get_res_value(res2,"lbLabelBarOn",False)  ; make sure this gets removed from res2
      end if
    else
;
; Check for a vector labelbar. vcMonoLineArrowColor is set to False
; by gsn_vector_scalar, so you have to check for it explicitly
; being True here.
;
      if(.not.check_attr(res2,"vcMonoLineArrowColor",True,False))
        lbar_type = "vector"
        use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
        use_mgr_lbar = .not.use_old_lbar
        lbar_on = get_res_value(res2,"lbLabelBarOn",True)
        if(lbar_on) then
          if(use_mgr_lbar) then
            set_labelbar_res(res2,"vector")
          end if
        end if
      else
        lbar_on = get_res_value(res2,"lbLabelBarOn",False)  ; make sure this gets removed from res2
      end if
    end if

    lbar_orient  = str_lower(get_res_value_keep(res2,"lbOrientation", \
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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT","gsnMask"/))

    if(lbar_on.and.use_old_lbar) then
      vcres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm","gsnMask"/))
    else
      vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","gsnMask"/))
    end if

; Before we create the objects, turn off draw and frame for them.
    vcres           = True
    mpres           = True
    vcres@gsnDraw   = False
    vcres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

    if(scalar_contour)
      base_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
    else
      base_object = gsn_vector_scalar(wks,unew,vnew,datanew,vcres)
    end if

    map_object = gsn_csm_map_other(wks,mpres)   ; Create map.
    overlay(map_object,base_object)           ; Overlay vectors on map.

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
      if(scalar_contour) then
        setvalues base_object@vector
          "vcRefAnnoFontHeightF" : font_height
        end setvalues
      else
        setvalues base_object
          "vcRefAnnoFontHeightF" : font_height
        end setvalues
      end if
    end if

; Create a labelbar.

    if(use_old_lbar)
      if(.not.refanno_on.and..not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
;---Get labelbar resources, if any.
      lbres = get_res_eq(res2,(/"lb","pm"/))
    end if
    if(scalar_contour)
      if(use_old_lbar)
        add_labelbar(wks,base_object,lbar_zone,font_height, \
                     "vector",lbar_on,lbres)
      end if
      if(use_mgr_lbar)
        fix_labelbar(base_object,font_height,lbar_on,vcres)
      end if
    else
      if(use_old_lbar)
        add_labelbar(wks,base_object,lbar_zone,font_height, \
                     "vector",lbar_on,lbres)
      end if
      if(use_mgr_lbar)
        fix_labelbar(base_object,font_height,lbar_on,vcres)
      end if
    end if
  
; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    if(lbar_type.eq."contour") then
      map_object@labelbar      = base_object
      map_object@labelbar_type = lbar_type
    end if

    map_object@vfdata  = base_object@vfdata
    map_object@sfdata  = base_object@sfdata
    if(scalar_contour)
      map_object@contour = base_object
      map_object@vector  = base_object@vector
    else
      map_object@vector  = base_object
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
undef("gsn_csm_contour_map")
function gsn_csm_contour_map(wks:graphic,data:numeric,resources:logical)
local res
begin
  res = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(data,new(1,float),new(1,float),\
                     "gsn_csm_contour_map",res,1)
  end if

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_contour_map_polar(wks,data,res))
  else
    if(check_attr_enum_value(res,"mpProjection","CylindricalEquidistant").or.\
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
undef("gsn_csm_streamline_map")
function gsn_csm_streamline_map(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                                resources:logical)
local res
begin
  res = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_streamline_map",res,2)
  end if

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_streamline_map_polar(wks,u,v,res))
  else
    if(check_attr_enum_value(res,"mpProjection","CylindricalEquidistant").or.\
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
undef("gsn_csm_vector_map")
function gsn_csm_vector_map(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                            resources:logical)
local res
begin
  res  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_vector_map",res,2)
  end if

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_vector_map_polar(wks,u,v,res))
  else
    if(check_attr_enum_value(res,"mpProjection","CylindricalEquidistant").or.\
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
undef("gsn_csm_vector_scalar_map")
function gsn_csm_vector_scalar_map(wks:graphic,u[*][*]:numeric, \
                                   v[*][*]:numeric,data:numeric, \
                                   resources:logical)
local res
begin
  res  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(u,v,data,"gsn_csm_vector_scalar_map",res,3)
  end if

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_vector_scalar_map_polar(wks,u,v,data,res))
  else
    if(check_attr_enum_value(res,"mpProjection","CylindricalEquidistant").or.\
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
undef("gsn_csm_streamline_contour_map")
function gsn_csm_streamline_contour_map(wks:graphic,u[*][*]:numeric, \
                                   v[*][*]:numeric,data:numeric, \
                                   resources:logical)
local res
begin
  res  = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_contour_map",res,3)
  end if

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_streamline_contour_map_polar(wks,u,v,data,res))
  else
    if(check_attr_enum_value(res,"mpProjection","CylindricalEquidistant").or.\
       .not.isatt(res,"mpProjection"))
      return(gsn_csm_streamline_contour_map_ce(wks,u,v,data,res))
    else
      return(gsn_csm_streamline_contour_map_other(wks,u,v,data,res))
    end if
  end if
end


;***********************************************************************;
; Function : gsn_csm_streamline_scalar                                  ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function colors streamlines according to a scalar field.         ;
;                                                                       ;
;          Otherwise, the streamlines will be colored                   ;
;                   according to the scalar field.                      ;
;                                                                       ;
;               Edited from gsn_csm_vector_scalar                       ;
;                       by Alan Brammer   2015-02-04                    ;
;***********************************************************************;
undef("gsn_csm_streamline_scalar")
function gsn_csm_streamline_scalar(wks:graphic,u[*][*]:numeric,v[*][*]:numeric, \
                               data:numeric,resources:logical)
local i, streamline_object, res, res2, use_old_lbar, use_mgr_lbar, \
calldraw, callframe, unew, vnew, datanew, vpwf, vphf, use_old_lbar, lbar_type
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_streamline_scalar: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    main_zone      = 2         ; Zone for main title (may change later)
    infolabel_on   = False
    res2 = get_resources(resources)

;
; Write data and plot resource information to a file so we can
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_scalar",res2,3)
    end if

    tm_scale      = get_res_value_keep(res2,"gsnScale",True)
    point_outward = get_res_value(res2,"gsnTickMarksPointOutward",True)

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"",True)) then
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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
    check_for_coord_arrays(unew,res2,"vector")

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Get labelbar resources.
    lbres = get_res_eq(res2,(/"lb","pm"/))

;
; Labelbar stuff
;
; Check for a streamline labelbar. stMonoLineColor is set to False
; by gsn_streamline_scalar, so you have to check for it explicitly
; being True here.
;
    lbar_type      = ""
    if(.not.check_attr(res2,"stMonoLineColor",True,False))
      lbar_type = "streamline"
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
          set_labelbar_res(res2,"other")
        end if
      end if
    else
      lbar_on = False
      use_old_lbar   = False
      use_mgr_lbar   = False
    end if

    if(lbar_on.and.use_old_lbar.and.check_attr_enum_value(lbres,"lbOrientation","vertical"))
      set_attr(res2, "vpWidthF",0.75)   ; Make room for labelbar
      set_attr(res2,"vpHeightF",0.75)   ; on the side.
      set_attr(res2,     "vpXF",0.08)
      set_attr(res2,     "vpYF",0.90)
    end if

; Before we create the streamline object, turn off draw and frame for it.
    if(lbar_on.and.use_mgr_lbar) then
      stres = get_res_ne(res2,(/"mp","tx","am"/))
    else
      stres = get_res_ne(res2,(/"mp","lb","tx","am","pm"/))
    end if

    stres          = True
    stres@gsnDraw  = False
    stres@gsnFrame = False
    stres@gsnScale = tm_scale  ; Force labels and ticks to be same, if True.

      streamline_object = gsn_streamline_scalar(wks,unew,vnew,datanew,stres)

; Add lat/lon labels to X/Y axes if appropriate coordinate arrays exist.

    add_latlon_labels(streamline_object,unew,res2)

; Retrieve some font heights and make the X/Y axis labels the same
; size, and the info label size the same as the tick mark label size.

    getvalues streamline_object
      "vpHeightF"            : height
      "vpWidthF"             : width

      "tmXBLabelFontHeightF" : xbfontf
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
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
;
; If gsnScale is True, then make sure we have a valid tickmark
; object before we do anything else.
;
    if(tm_scale.and.is_tm_obj_valid(streamline_object)) then
;
; Make tick marks same length and point outward.
;
      getvalues streamline_object
        "tmYLMajorLengthF"     : ylength
        "tmXBMajorLengthF"     : xlength
        "tmYLMinorLengthF"     : ymlength
        "tmXBMinorLengthF"     : xmlength
      end getvalues

      major_length = min((/ylength,xlength/))    ; New length for major ticks.
      minor_length = min((/ymlength,xmlength/))  ; New length for minor ticks.

      major_length = scale * major_length
      minor_length = scale * minor_length

      tmres = get_res_eq(res2,"tm")
      gsnp_point_tickmarks_outward(streamline_object,tmres,xlength,ylength,\
                                   xmlength,ymlength,\
                                   major_length,minor_length,point_outward)
    end if

;---Create a labelbar or fix an existing one.
    if(lbar_on.and.use_old_lbar)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
    end if
    if(use_old_lbar)
      add_labelbar(wks,streamline_object,lbar_zone,xbfontf,"streamline",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(streamline_object,xbfontf,lbar_on,stres)
    end if

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,streamline_object,left_string,center_string,\
                  right_string,subres)

; Draw all this stuff: streamline/contour plot, subtitles, and tick marks.
    draw_and_frame(wks,streamline_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    streamline_object@vfdata  = streamline_object@vfdata
    streamline_object@sfdata  = streamline_object@sfdata
    if(lbar_type.eq."contour") then
      streamline_object@labelbar      = streamline_object@contour
      streamline_object@labelbar_type = lbar_type
    end if
    return(streamline_object)
end

;***********************************************************************;
; Function : gsn_csm_streamline_scalar_map_ce                           ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_streamline_map_ce except it       ;
; colors the streamlines according to a scalar field.                   ;
;                                                                       ;
;                                                                       ;
;               Edited from gsn_csm_streamline_scalar_map_ce            ;
;                       by Alan Brammer   2015-02-04                    ;
;***********************************************************************;
undef("gsn_csm_streamline_scalar_map_ce")
function gsn_csm_streamline_scalar_map_ce(wks:graphic,u[*][*]:numeric,\
                   v[*][*]:numeric,data:numeric,resources:logical)
local i, streamline_object, map_object, res, res2, \
calldraw, callframe, min_lat, max_lat, unew, vnew, datanew, \
map_vpwf, map_vphf, vpwf, vphf, use_old_lbar, use_mgr_lbar, lbar_type
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_streamline_scalar_map_ce: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    infolabel_on   = False
    mpres          = True
    res2           = get_resources(resources)
;
; Write data and plot resource information to a file so we can
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_scalar_map_ce",res2,3)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to
; overlay streamline on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")


; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

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

;
; Labelbar stuff
;
; Check for a streamline labelbar. stMonoLineColor is set to False
; by gsn_streamline_scalar, so you have to check for it explicitly
; being True here.
;
    lbar_type = ""
    if(.not.check_attr(res2,"stMonoLineColor",True,False))
      lbar_type = "streamline"
      lbar_on = get_res_value(lbres,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
          set_labelbar_res(res2,"other")
        end if
      end if
    else
      lbar_on = False
      use_old_lbar = False
      use_mgr_lbar = False
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
    if(lbar_on.and.use_mgr_lbar) then
      stres = get_res_ne(res2,(/"mp","vp","tm","tx","am"/))
    else
      stres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))
    end if

; Before we create the objects, turn off draw and frame for them.
    stres          = True
    mpres          = True
    stres@gsnDraw  = False
    stres@gsnFrame = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    if(lbar_on.and.use_old_lbar.and.check_attr_enum_value(lbres,"lbOrientation","vertical"))
      set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
      set_attr(mpres,"vpHeightF",0.75)   ; on the side.
      set_attr(mpres,     "vpXF",0.08)
      set_attr(mpres,     "vpYF",0.90)
    end if


      streamline_object = gsn_streamline_scalar(wks,unew,vnew,datanew,stres)

    map_object = gsn_csm_map_ce(wks,mpres)          ; Create map.
    overlay(map_object,streamline_object)               ; Overlay streamlines on map.

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

; Create a labelbar or fix an existing one.
    if(use_old_lbar.and..not.infolabel_on)
      lbar_zone = 2
    end if
    if(use_old_lbar)
      add_labelbar(wks,streamline_object,lbar_zone,xbfontf,"streamline",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(streamline_object,xbfontf,lbar_on,stres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    if(lbar_type.eq."contour") then
      map_object@labelbar      = streamline_object@contour
      map_object@labelbar_type = lbar_type
    end if

    map_object@vfdata  = streamline_object@vfdata
    map_object@sfdata  = streamline_object@sfdata
    map_object@streamline  = streamline_object

    return(map_object)
end

;***********************************************************************;
; Function : gsn_csm_streamline_scalar_map_polar                        ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_streamline_map_polar except it    ;
; either colors the streamlines according to a scalar field, or it      ;
; overlays a contour plot.                                              ;
;                                                                       ;
;                                                                       ;
;               Edited from gsn_csm_vector_scalar_map_polar             ;
;                       by Alan Brammer   2015-02-04                    ;
;***********************************************************************;
undef("gsn_csm_streamline_scalar_map_polar")
function gsn_csm_streamline_scalar_map_polar(wks:graphic,u[*][*]:numeric, \
                                         v[*][*]:numeric,data:numeric,
                                         resources:logical)
local i, streamline_object, map_object, res, res2, calldraw, stres, lbres, \
vpwf, vphf, callframe, use_old_lbar, use_mgr_lbar, lbar_type, lbar_zone, unew, vnew
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_streamline_scalar_map_polar: Fatal: the input data array must be 1D or 2D")
      return
    end if
;
; The default is to draw streamlines colored by a scalar field, so this
; means a labelbar should be drawn, and no contour information is
; needed.
;
; Initialize.
    res2  = get_resources(resources)
;
; Write data and plot resource information to a file so we can
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_scalar_map_polar",res2,3)
    end if

    mpres = True

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
    else
      unew    = u
      vnew    = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where
; overlay streamlines on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

; Check for draw and frame.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

;
; Check for type of polar plot and scalar field representation.
;
    mpres@gsnPolar     = get_polar_type(res2)
    mpres@gsnPolarTime = get_res_value(res2,"gsnPolarTime",False)
    mpres@gsnPolarUT   = get_res_value(res2,"gsnPolarUT",0.)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
;
; Labelbar stuff
;
; Determine which annotations (labelbar, streamline reference anno, info label)
; should be turned on.  By default, labelbar and streamline anno are on, and
; contour info label is off.
;
; Check for a streamline labelbar. stMonoLineColor is set to False
; by gsn_streamline_scalar, so you have to check for it explicitly
; being True here.
;
;
    lbar_type     = "";
    if(.not.check_attr(res2,"stMonoLineColor",True,False))
      lbar_type = "streamline"
      lbar_on = get_res_value(res2,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
          set_labelbar_res(res2,"polar")
        end if
      end if
    else
      lbar_on = get_res_value(res2,"lbLabelBarOn",False) ; make sure gets removed from res2
      use_old_lbar  = False
      use_mgr_lbar  = False
    end if

    lbar_orient  = str_lower(get_res_value_keep(res2,"lbOrientation", \
                              "horizontal"))
    infolabel_on = get_res_value_keep(res2,"cnInfoLabelOn",False)
;
; Now that we know what's being drawn for annotations (labelbar,
; info label, ref anno), we can determine where to put all this
; stuff. If both info label and ref anno are on, put the info label
; on the right, and the ref anno on the left.  If only one of them is
; on, put it on the right.
;

    if(infolabel_on)
      set_attr(res2,"cnInfoLabelZone",2)
      set_attr(res2,"cnInfoLabelOrthogonalPosF", 0.1)
      set_attr(res2,"cnInfoLabelParallelPosF", 1.0)
      set_attr(res2,"cnInfoLabelJust","TopRight")
    end if

    if(lbar_on.and.use_old_lbar)
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
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO",\
                              "pmT","gsnPolar"/))

    if(lbar_on.and.use_old_lbar) then
      stres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm","gsnPolar"/))
    else
      stres = get_res_ne(res2,(/"mp","vp","tm","tx","am","gsnPolar"/))
    end if

; Before we create the objects, turn off draw and frame for them.
    stres = True
    mpres = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw  = False
    mpres@gsnFrame = False

    streamline_object = gsn_streamline_scalar(wks,unew,vnew,datanew,stres)
    map_object    = gsn_csm_map_polar(wks,mpres)    ; Create map.
    overlay(map_object,streamline_object)               ; Overlay streamlines on map.

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

    if(lbar_on.and.use_old_lbar)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
      lbres = get_res_eq(res2,(/"lb","pm"/))
    end if

    if(use_old_lbar)
      add_labelbar(wks,streamline_object,lbar_zone,font_height,"streamline",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(streamline_object,font_height,lbar_on,stres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.
    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    if(lbar_type.eq."contour") then
      map_object@labelbar      = streamline_object@contour
      map_object@labelbar_type = lbar_type
    end if

    map_object@vfdata  = streamline_object@vfdata
    map_object@sfdata  = streamline_object@sfdata
    map_object@streamline  = streamline_object
    return(map_object)
end


;***********************************************************************;
; Function : gsn_csm_streamline_scalar_map_other                        ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional scalar data                      ;
;               resources: optional resources                           ;
;                                                                       ;
; This function is similar to gsn_csm_streamline_map_other except it    ;
; either colors the streamlines according to a scalar field, or it      ;
; overlays a contour plot.                                              ;
;                                                                       ;
;                                                                       ;
;               Edited from gsn_csm_vector_scalar_map_other             ;
;                       by Alan Brammer   2015-02-04                    ;
;***********************************************************************;
undef("gsn_csm_streamline_scalar_map_other")
function gsn_csm_streamline_scalar_map_other(wks:graphic,u[*][*]:numeric,\
                                        v[*][*]:numeric,data:numeric, \
                                        resources:logical)
local i, streamline_object, labelbar_object, map_object, \
calldraw, callframe, use_old_lbar, lbar_type, min_lat, max_lat, datanew, \
res, res2, lbres, mpres, stres, levels, colors, use_mgr_lbar, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just, \
map_vpwf, map_vphf, vphf
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_streamline_scalar_map_other: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    infolabel_on   = False
    mpres          = True
    res2           = get_resources(resources)

;
; Write data and plot resource information to a file so we can
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_scalar_map_other",res2,3)
    end if

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"map",True)) then
      unew = gsn_add_cyclic_point(u)
      vnew = gsn_add_cyclic_point(v)
    else
      unew = u
      vnew = v
    end if

    if(get_lon_cyclic_point_default(data,res2,"map",False)) then
      datanew = gsn_add_cyclic_point(data)
    else
      datanew = data
    end if

; Check for coordinate variables. These values will determine where to
; overlay streamlines on map.

    check_for_y_lat_coord(unew,res2,"vector_map")
    check_for_lon_coord(unew,res2,"vector_map")

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

    set_right_subtitle(datanew,res2,mpres)
    set_left_subtitle(datanew,res2,mpres)

; Check if frame and/or draw are not supposed to be called.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
;
; Labelbar stuff
;
; Check for a streamline labelbar. stMonoLineColor is set to False
; by gsn_streamline_scalar, so you have to check for it explicitly
; being True here.
;
    lbar_type = ""
    if(.not.check_attr(res2,"stMonoLineColor",True,False))
      lbar_type = "streamline"
      lbar_on = get_res_value(res2,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
          set_labelbar_res(res2,"streamline")
        end if
      end if
    else
      lbar_on = get_res_value(res2,"lbLabelBarOn",False)    ; to make sure removed from res2
      use_old_lbar    = False
      use_mgr_lbar    = False
    end if

    lbar_orient  = str_lower(get_res_value_keep(res2,"lbOrientation", \
                              "horizontal"))
    infolabel_on = get_res_value_keep(res2,"cnInfoLabelOn",False)

;
; Now that we know what's being drawn for annotations (labelbar,
; info label, ref anno), we can determine where to put all this
; stuff. If both info label and ref anno are on, put the info label
; on the right, and the ref anno on the left.  If only one of them is
; on, put it on the right.
;
    if(infolabel_on)
      set_attr(res2,"cnInfoLabelZone",2)
      set_attr(res2,"cnInfoLabelOrthogonalPosF", 0.1)
      set_attr(res2,"cnInfoLabelParallelPosF", 1.0)
      set_attr(res2,"cnInfoLabelJust","TopRight")
    end if
;
; By default, mpOutlineOn is False, unless stMonoLineColor is set to
; False or mpFillOn is set to False, then it is set back to True.
;
   if(check_attr(res2,"mpFillOn",False,False))
      set_attr(res2,"mpOutlineOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT","gsnMask"/))

    if(lbar_on.and.use_old_lbar) then
      stres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm","gsnMask"/))
    else
      stres = get_res_ne(res2,(/"mp","vp","tm","tx","am","gsnMask"/))
    end if

; Before we create the objects, turn off draw and frame for them.
    stres           = True
    mpres           = True
    stres@gsnDraw   = False
    stres@gsnFrame  = False
    mpres@gsnDraw   = False
    mpres@gsnFrame  = False

      streamline_object = gsn_streamline_scalar(wks,unew,vnew,datanew,stres)

    map_object = gsn_csm_map_other(wks,mpres)   ; Create map.
    overlay(map_object,streamline_object)           ; Overlay streamlines on map.

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

; Create a labelbar or fix an existing one.
    if(lbar_on.and.use_old_lbar)
      if(.not.infolabel_on)
        lbar_zone = 2
      else
        lbar_zone = 3
      end if
      lbres = get_res_eq(res2,(/"lb","pm"/))
    end if
    if(use_old_lbar)
      add_labelbar(wks,streamline_object,lbar_zone,font_height, \
                   "streamline",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(streamline_object,font_height,lbar_on,stres)
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.

    draw_and_frame(wks,map_object,calldraw,callframe,0,maxbb)

; Return plot object and data object (as attribute of plot object).

    if(lbar_type.eq."contour") then
      map_object@labelbar      = streamline_object@contour
      map_object@labelbar_type = lbar_type
    end if

    map_object@vfdata  = streamline_object@vfdata
    map_object@sfdata  = streamline_object@sfdata
    map_object@streamline  = streamline_object
    return(map_object)
end


;***********************************************************************;
; Function : gsn_csm_streamline_scalar_map                              ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_csm_streamline_scalar_map_ce,          ;
; gsn_csm_streamline_scalar_map_polar or                                ;
; gsn_csm_streamline_scalar_map_other depending on the map projection   ;
; selected.                                                             ;
;                                                                       ;
;               Edited from gsn_csm_vector_scalar_map                   ;
;                       by Alan Brammer   2015-02-04                    ;
;***********************************************************************;
undef("gsn_csm_streamline_scalar_map")
function gsn_csm_streamline_scalar_map(wks:graphic,u[*][*]:numeric, \
                                   v[*][*]:numeric,data:numeric, \
                                   resources:logical)
local res
begin
  res  = get_resources(resources)
;
; Write data and plot resource information to a file so we can
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(u,v,data,"gsn_csm_streamline_scalar_map",res,3)
  end if

  if(res.and.(isatt(res,"gsnPolarNH").or.isatt(res,"gsnPolarSH").or.\
              isatt(res,"gsnPolar")))
    return(gsn_csm_streamline_scalar_map_polar(wks,u,v,data,res))
  else
    if(check_attr_enum_value(res,"mpProjection","CylindricalEquidistant").or.\
       .not.isatt(res,"mpProjection"))
      return(gsn_csm_streamline_scalar_map_ce(wks,u,v,data,res))
    else
      return(gsn_csm_streamline_scalar_map_other(wks,u,v,data,res))
    end if
  end if
end



;***********************************************************************;
; Function : gsn_csm_contour_map_overlay                                ;
;                   wks: workstation object                             ;
;                 data1: first data to contour                          ;
;                 data2: second data to contour                         ;
;                  res1: optional resources                             ;
;                  res2: optional resources                             ;
;                                                                       ;
; This is similar to gsn_csm_contour_map, only it overlays an           ;
; additional contour plot.                                              ;
;                                                                       ;
;***********************************************************************;
undef("gsn_csm_contour_map_overlay")
function gsn_csm_contour_map_overlay(wks:graphic,data1:numeric,data2:numeric, \
                                     res1:logical,res2:logical)
local calldraw, callframe, res1_tmp, res2_tmp
begin
;
; Make sure input data is 1D or 2D
;
  if(.not.is_data_1d_or_2d(data1).or..not.is_data_1d_or_2d(data2)) then
    print("gsn_csm_contour_map_overlay: Fatal: the input data arrays must be 1D or 2D")
    return
  end if

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

; Check for whether a cyclic point might need to be added.
  if(get_lon_cyclic_point_default(data2,res2_tmp,"map",False)) then
    datanew = gsn_add_cyclic_point(data2)
  else
    datanew = data2
  end if

  check_for_y_lat_coord(datanew,res2_tmp,"contour_map")
  check_for_lon_coord(datanew,res2_tmp,"contour_map")

; Default for second plot is to draw line labels, but make background
; label box transparent. Also, move the info label around depending on
; whether the labelbar from the first plot is on the side or the bottom.

  set_attr(res2_tmp,"cnLineLabelBackgroundColor",-1)
  if(.not.isatt(res2_tmp,"cnInfoLabelOrthogonalPosF"))
    if(res1_tmp@cnFillOn)
      if(check_attr_enum_value(res1_tmp,"lbOrientation","horizontal"))
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
    draw_and_frame(wks,contour1,calldraw,callframe,0,maxbb)

; Return contour plot object.

  contour1@contour2 = contour2
  return(contour1)

end

;***********************************************************************;
; Function : plot_x2y2                                                  ;
;               wks: workstation object                                 ;
;                x1: X values, representing the bottom axis             ;
;                x2: second set of X values (opt'l)                     ;
;                    representing the top axis                          ;
;                y1: Y values, representing the left axis               ;
;                y2: second set of Y values (opt'l)                     ;
;                    representing the right axis                        ;
;              res1: opt'l resources, to apply to first plot            ;
;                    axes used are bottom/left.                         ;
;              res2: opt'l resources, to apply to second plot           ;
;                    axes used are top/right.                           ;
;              axis: which axis has the multiple curves ("x" means      ;
;                    the X axes has the multiple curves, "y" is for the ;
;                    Y axes, and "xy" for both axes)                    ;
;         plot_type: whether we're doing generic gsn, or gsn_csm        ;
;                                                                       ;
; Originally created by Dennis Shea to allow two Y curves with two      ;
; separate Y axes to reside on the same plot and share the same X axis. ;
; The Y axes are labeled both on the left and right to indicate the two ;
; different axes.                                                       ;
;                                                                       ;
; Modified by Mary Haley and made more generic so it can handle the X   ;
; or Y axis or both axes having multiple curves.                        ;
;***********************************************************************;
undef("plot_x2y2")
function plot_x2y2(wks:graphic, x1:numeric, x2:numeric, \
                                y1:numeric, y2:numeric, \
                                res1:logical, res2:logical, \
                                axis:string, plot_type:string)
local locRes1, locRes2, Atts1, Atts2, xy1, xy2, calldraw, callframe, maxbb 
begin
;
; Make copy of local resources.
;
    if (res2) then
      locRes2 = res2
    end if
    if (res1) then
      locRes1 = get_res_ne(res1,"tiMain")
      locRes2 = get_res_eq(res1,"tiMain")
    end if
    locRes1 = True
    locRes2 = True
;
; Retain draw, frame, and maximize settings for later.
;
    calldraw  = get_res_value(locRes1,"gsnDraw", True)
    callframe = get_res_value(locRes1,"gsnFrame",True)
    maxbb     = get_bb_res(locRes1)
;
; Check for subtitles. We don't want to add the subtitles until all
; the plot stuff is done, in case some zones need to be adjusted.
;
    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

    check_for_subtitles(locRes1,left_string,center_string,right_string)
;
; Set some zone information for the subtitles and main string.
;
    if(axis.eq."x".or.axis.eq."xy") then
      mainzone = 5 
      amzone   = 4
    else
      mainzone = 6 
      amzone   = 5
    end if

    locRes2@pmTitleZone = mainzone
;
; For first plot, don't auto advance the frame or draw or maximize
; the plot.
;
    locRes1@gsnFrame    = False
    locRes1@gsnDraw     = False
    locRes1@gsnMaximize = False

;
; If drawing a multiple axis plot, make sure you turn off the
; tickmarks on the other side, since they will later be defined
; by the second set of data.
;
    if(axis.eq."y".or.axis.eq."xy") then
        locRes1@tmYROn       = False  ; Turn off right tick marks.
        locRes1@tmYRMinorOn  = False  ; Turn off right minor tick marks.
        locRes1@tmYUseLeft   = False  ; Keep independent of right.
    end if
    if(axis.eq."x".or.axis.eq."xy") then
        locRes1@tmXTOn       = False  ; Turn off top tick marks.
        locRes1@tmXTMinorOn  = False  ; Turn off top minor tick marks.
        locRes1@tmXUseBottom = False  ; Keep independent of top.
    end if

;
; Create first plot.
;
    if(plot_type.eq."csm") then
      xy1 = gsn_csm_xy (wks,x1,y1,locRes1)
    else
      xy1 = gsn_xy (wks,x1,y1,locRes1)
    end if
;
; Get viewport coordinates, tickmark lengths, and font heights of
; first plot to use for second plot.
;
    getvalues xy1
      "vpHeightF"               : vph      ; Get assorted info from previous
      "vpWidthF"                : vpw      ; plot for use on second plot.
      "vpXF"                    : vpx
      "vpYF"                    : vpy
                                            ; Bottom X axis stuff
      "trXMinF"                 : trxmin 
      "trXMaxF"                 : trxmax
      "trYMinF"                 : trymin 
      "trYMaxF"                 : trymax

      "tiXAxisFontHeightF"      : xfonth
      "tmXBLabelFontHeightF"    : xbfonth
      "tmXBMajorLengthF"        : xmjlength
      "tmXBMinorLengthF"        : xmnlength
      "tmXBMajorOutwardLengthF" : xmjolength
      "tmXBMinorOutwardLengthF" : xmnolength
                                            ; Left Y axis stuff
      "tiYAxisFontHeightF"      : yfonth
      "tmYLLabelFontHeightF"    : ylfonth
      "tmYLMajorLengthF"        : ymjlength
      "tmYLMinorLengthF"        : ymnlength
      "tmYLMajorOutwardLengthF" : ymjolength
      "tmYLMinorOutwardLengthF" : ymnolength
    end getvalues

;
; If the user is setting any of these, then use these settings and
; not the ones we just retrieved.
;
; For second plot, don't auto advance the frame or draw or maximize
; the plot.
;
    locRes2@gsnFrame    = False
    locRes2@gsnDraw     = False
    locRes2@gsnMaximize = False
;
; Set resources that are common for second plot, regardless of what
; kind of axes situation we have.
;
    locRes2@gsnFrame    = False
    locRes2@gsnDraw     = False
    locRes2@gsnMaximize = False

    locRes2@vpHeightF   = vph       ; set to same size as first plot
    locRes2@vpWidthF    = vpw 
    locRes2@vpXF        = vpx     
    locRes2@vpYF        = vpy

    locRes2@tmXBLabelsOn = False  ; Turn off bottom x axis labels
    locRes2@tmXBMinorOn  = False  ; and tick marks.
    locRes2@tmXBOn       = False
    locRes2@tmYLLabelsOn = False  ; Turn off left y axis labels
    locRes2@tmYLMinorOn  = False  ; and tick marks.
    locRes2@tmYLOn       = False

;
; Set some resources that will apply if you have two Y axes.
;
    if(axis.eq."y".or.axis.eq."xy") then
      locRes2@tmYRLabelsOn            = get_res_value_keep(locRes2, \
                                        "tmYRLabelsOn",True) ; rgt axis labels
      locRes2@tmYROn                  = get_res_value_keep(locRes2, \
                                        "tmYROn",True) ; rgt tick marks on
      locRes2@tmYUseLeft              = False  
      locRes2@tiYAxisSide             = "Right"; put title on right scale
      locRes2@tiYAxisFontHeightF      = get_res_value_keep(locRes2, \
                                        "tiYAxisFontHeightF",yfonth) 
      locRes2@tmYRLabelFontHeightF    = get_res_value_keep(locRes2, \
                                        "tmYLLabelFontHeightF",ylfonth)
      locRes2@tmYRMajorLengthF        = get_res_value_keep(locRes2, \
                                        "tmYLMajorLengthF",ymjlength)
      locRes2@tmYRMinorLengthF        = get_res_value_keep(locRes2, \
                                        "tmYLMinorLengthF",ymnlength)
      locRes2@tmYRMajorOutwardLengthF = get_res_value_keep(locRes2, \
                                        "tmYLMajorOutwardLengthF",ymjolength)
      locRes2@tmYRMinorOutwardLengthF = get_res_value_keep(locRes2, \
                                        "tmYLMinorOutwardLengthF",ymnolength)
    end if

;
; Set resources specific only to having two Y axes and one X axis.
;
    if(axis.eq."y") then
      locRes2@trXMinF       = get_res_value_keep(locRes2,"trXMinF",trxmin) 
      locRes2@trXMaxF       = get_res_value_keep(locRes2,"trXMaxF",trxmax)
      locRes2@tiXAxisString = ""     ; already done 
      locRes2@tmXTLabelsOn  = False  ; Turn off top x axis labels
      locRes2@tmXTMinorOn   = False  ; and tick marks.
      locRes2@tmXTOn        = False
;
; Create second plot, but only for the case of two Y axes and one X axis.
;
      if(plot_type.eq."csm") then
        xy2 = gsn_csm_xy(wks,x1,y2,locRes2)
      else
        xy2 = gsn_xy(wks,x1,y2,locRes2)
      end if
    end if

;
; Set some resources that will apply if you have two X axes.
; Note that normally, the x axis will use "long_name" for a title.
; However, this causes clutter if you try to put this title at
; the top of the plot, so for now, we are disabling an x axis string
; for the top, unless it is explicitly set by the user.
;
    if(axis.eq."x".or.axis.eq."xy") then
      locRes2@tmXTLabelsOn            = get_res_value_keep(locRes2, \
                                        "tmXTLabelsOn",True) ; top axis labels
      locRes2@tmXTOn                  = get_res_value_keep(locRes2, \
                                        "tmXTOn",True) ; top tick marks on
      locRes2@tmXUseBottom            = False  
      if(isatt(locRes2,"tiXAxisString")) then
        locRes2@tiXAxisSide           = "Top"; put title on Top scale
      else
        locRes2@tiXAxisOn             = False
      end if 
      locRes2@tiXAxisFontHeightF      = get_res_value_keep(locRes2, \
                                        "tiXAxisFontHeightF",xfonth)
      locRes2@tmXTLabelFontHeightF    = get_res_value_keep(locRes2,\
                                        "tmXBLabelFontHeightF",xbfonth)
      locRes2@tmXTMajorLengthF        = get_res_value_keep(locRes2, \
                                        "tmXBMajorLengthF",xmjlength)
      locRes2@tmXTMinorLengthF        =  get_res_value_keep(locRes2, \
                                         "tmXBMinorLengthF",xmnlength)
      locRes2@tmXTMajorOutwardLengthF = get_res_value_keep(locRes2, \
                                        "tmXBMajorOutwardLengthF",xmjolength)
      locRes2@tmXTMinorOutwardLengthF = get_res_value_keep(locRes2, \
                                        "tmXBMinorOutwardLengthF",xmnolength)
    end if

;
; Set resources specific only to having two X axes and one Y axis.
;
    if(axis.eq."x") then
      locRes2@trYMinF        = get_res_value_keep(locRes2,"trYMinF",trymin)
      locRes2@trYMaxF        = get_res_value_keep(locRes2,"trYMaxF",trymax)
      locRes2@tiYAxisString  = ""     ; already done 
      locRes2@tmYRLabelsOn   = False  ; Turn off right y axis labels
      locRes2@tmYRMinorOn    = False  ; and tick marks.
      locRes2@tmYROn         = False

      if(plot_type.eq."csm") then
        xy2 = gsn_csm_xy(wks,x2,y1,locRes2)
      else
        xy2 = gsn_xy(wks,x2,y1,locRes2)
      end if
    end if

;
; Create a second plot for the case of two Y axes and two X axes.
;
    if(axis.eq."xy") then 
      if(plot_type.eq."csm") then
        xy2 = gsn_csm_xy(wks,x2,y2,locRes2)
      else
        xy2 = gsn_xy(wks,x2,y2,locRes2)
      end if
    end if

;
; Set up three subtitles at top, if they exist. Note that we are
; attaching the subtitles to the second plot. This is because the
; second plot is the one that might have tickmarks and labels 
; coming out the top, and we need to make sure these gsn strings stay
; outside of them. This is also true for the main title, which we
; already handled above.
;
    getvalues xy2
      "vpHeightF"            : height
      "vpWidthF"             : width
      "tiYAxisFontHeightF"   : yfontf
      "tiXAxisFontHeightF"   : xfontf
    end getvalues

; If the plot is close to square in size, then make the 
; three top titles and the tick marks smaller.

    font_height = min((/xfontf,yfontf/))  ; Make label sizes a function of

    ratio = height/width
    if(ratio.gt.1) 
      ratio = 1./ratio
    end if
    if(ratio.gt.0.5)
      font_height = 0.75 * font_height
    end if

    subres = True
    subres = get_res_eq(locRes1,"tx")  ; Get textitem resources
    set_attr(subres,"txFontHeightF",font_height)
;    set_attr(subres,"txFont","helvetica")
    set_attr(subres,"amZone",amzone)
    subres@amOrthogonalPosF = 0.06 

    add_subtitles(wks,xy2,left_string,center_string,right_string,subres)

;
; Add second plot as annotation of 1st plot.
;
    anno = NhlAddAnnotation(xy1,xy2)
    setvalues anno
      "amZone"         : 0     ; Zone 0 centers tick marks over map.
      "amResizeNotify" : True  ; Resize tick marks if map resized.
    end setvalues

    draw_and_frame(wks,xy1,calldraw,callframe,0,maxbb)
;
; Copy over xy2 atts, if any. This is just in case any
; primitives were attached.
;
    xy2att = getvaratts(xy2)
    if(.not.all(ismissing(xy2att))) then
      do i=0,dimsizes(xy2att)-1
        xy1@$xy2att(i)$ = xy2@$xy2att(i)$
      end do
    end if
    xy1@xy2 = xy2
    return (xy1)
 end


;***********************************************************************;
; Function : plot_xy3                                                   ;
;               wks: workstation object                                 ;
;                x1 : X values                                          ;
;                y1 : First set of Y values                             ;
;                y2 : Second set of Y values                            ;
;                y3 : Third set of Y values                             ;
;              res1 : opt'l resources, to apply to first curve          ;
;              res2 : opt'l resources, to apply to second curve         ;
;              res3 : opt'l resources, to apply to third curve          ;
;         plot_type : whether we're doing generic gsn, or gsn_csm       ;
;                                                                       ;
; This function calls plot_x2y2 with a single set of X values, and two  ;
; sets of Y values, each with their own Y axis, and puts them on the    ;
; same plot. The left axis is labeled according to the y1 values, and   ;
; the right axis is labeled according to the y2 values.                 ;
;                                                                       ;
;***********************************************************************;
undef("plot_xy3")
function plot_xy3(wks:graphic, x1:numeric, y1:numeric, y2:numeric, \
                  y3:numeric, res1:logical, res2:logical, res3:logical, \
                  plot_type:string)
begin
;
; Make a copy of the resources
;
  if(res1) then
    res1_new = res1
  end if
  if(res2) then
    res2_new = res2
  end if
  if(res3) then
    res3_new = get_res_ne(res3,(/"am"/))
  end if
;
; Make sure these are True
;
  res1_new = True
  res2_new = True
  res3_new = True
;
; Retain draw, frame, and maximize settings for later.
;
  calldraw  = get_res_value(res1_new,"gsnDraw", True)
  callframe = get_res_value(res1_new,"gsnFrame",True)
  maxbb     = get_bb_res(res1_new)

;
; Create some dummy variables.
;
  x2 = new(1,typeof(x1))
;
; Call plot_x2y2 function.
;
  res1_new@gsnDraw     = False
  res1_new@gsnFrame    = False
  res1_new@gsnMaximize = False   ; Don't need to maximize yet.
  res2_new@gsnDraw     = False
  res2_new@gsnFrame    = False
  res2_new@gsnMaximize = False   ; Don't need to maximize yet.
  xy12 = plot_x2y2 (wks, x1, x2, y1, y2, res1_new, res2_new, "y", plot_type)
  delete(x2)
;
; Get the size and X limits of this plot, and use them for the third plot.
;
  getvalues xy12
    "vpHeightF"  : vph
    "vpWidthF"   : vpw
    "vpXF"       : vpx
    "vpYF"       : vpy
    "trXMinF"    : xmin
    "trXMaxF"    : xmax
    "tiXAxisFontHeightF"   : xaxis_fh
    "tiYAxisFontHeightF"   : yaxis_fh
    "tmXBLabelFontHeightF" : xlab_fh
    "tmYLLabelFontHeightF" : ylab_fh
  end getvalues

;
; Set up some resources so we can add the third Y curve to the
; previous plot in the same location as the first two curves,
; but without any tickmarks.
;
; Notice that we are allowing the user to override some of these 
; settings, but the user really shouldn't be doing this in the
; first place.
;
  res3_new@gsnFrame    = False
  res3_new@gsnDraw     = False
  res3_new@gsnMaximize = False
  res3_new@vpHeightF   = get_res_value_keep(res3, "vpHeightF",vph)
  res3_new@vpWidthF    = get_res_value_keep(res3, "vpWidthF", vpw)
  res3_new@vpXF        = get_res_value_keep(res3, "vpXF",     vpx)
  res3_new@vpYF        = get_res_value_keep(res3, "vpYF",     vpy)
  res3_new@trXMinF     = get_res_value_keep(res3, "trXMinF",  xmin)
  res3_new@trXMaxF     = get_res_value_keep(res3, "trXMaxF",  xmax)
  res3_new@tmXTOn      = get_res_value_keep(res3, "tmXTOn",   False)
  res3_new@tmXBOn      = get_res_value_keep(res3, "tmXBOn",   False)
  res3_new@tmYROn      = get_res_value_keep(res3, "tmYROn",   False)
  res3_new@tmYLOn      = get_res_value_keep(res3, "tmYLOn",   False)
  res3_new@tiYAxisOn   = get_res_value_keep(res3,"tiYAxisOn",False)
  res3_new@tiXAxisOn   = get_res_value_keep(res3,"tiXAxisOn",False)
  res3_new@tmEqualizeXYSizes = get_res_value_keep(res3,"tmEqualizeXYSizes",True)
  if(plot_type.eq."csm") then
    xy3 = gsn_csm_xy(wks, (/x1/), (/y3/), res3_new)
  else
    xy3 = gsn_xy(wks, (/x1/), (/y3/), res3_new)
  end if
  delete(res3_new)

;
; Add this third curve as annotation of the xy12 plot.
;
  anno1 = NhlAddAnnotation(xy12,xy3)
  setvalues anno1
    "amZone"         : 0     ; Zone 0 centers tick marks over map.
    "amResizeNotify" : True  ; Resize tick marks if map resized.
  end setvalues

;
; Last step is to attach a single vertical line that will
; represent the Y axis for the third curve.  Make the width
; of this plot basically 0, so we can then use the amZone
; resource to attach the left edge of this plot to right side
; of the previous  plot, outside of the tickmarks and labels.
;
  if(res3) then
    res3_new = get_res_ne(res3,(/"am","xy","gsnXY"/))
  end if
  res3_new = True      ; Make sure this is True.

  res3_new@gsnFrame     = False
;  res3_new@xyMarkLineMode = "Lines"  ; Make sure not markers!
  res3_new@gsnDraw      = False
  res3_new@vpHeightF    = vph
  res3_new@vpWidthF     = 0.0001
  res3_new@vpXF         = vpx
  res3_new@vpYF         = vpy
  res3_new@trXMinF      = xmin
  res3_new@trXMaxF      = xmax
;
; Pick some "nice" values for this single vertical axis, and
; use them if the user hasn't already set their own.
;
  if(.not.isatt(res3_new,"trYMinF").or..not.isatt(res3_new,"trYMaxF")) then
    mnmxint               = nice_mnmxintvl(min(y3), max(y3), 10, True)
    res3_new@trYMinF      = get_res_value(res3_new,"trYMinF",mnmxint(0))
    res3_new@trYMaxF      = get_res_value(res3_new,"trYMaxF",mnmxint(1))
  else
    res3_new@trYMinF      = get_res_value(res3_new,"trYMinF",0)
    res3_new@trYMaxF      = get_res_value(res3_new,"trYMaxF",0)
  end if
  res3_new@tmXBOn       = get_res_value(res3_new,"tmXBOn",False)
  res3_new@tmXBBorderOn = get_res_value(res3_new,"tmXBBorderOn",False)
  res3_new@tmXTOn       = get_res_value(res3_new,"tmXTOn",False)
  res3_new@tmXTBorderOn = get_res_value(res3_new,"tmXTBorderOn",False)
  res3_new@tmYLOn       = get_res_value(res3_new,"tmYLOn",False)
  res3_new@tmYROn       = get_res_value(res3_new,"tmYROn",True)
;
; Turn on right axis labels; they are off by default.
;
  res3_new@tmYRLabelsOn = get_res_value(res3_new,"tmYRLabelsOn",True) 
  res3_new@tmYUseLeft   = get_res_value(res3_new,"tmYUseLeft",False)
  res3_new@tiYAxisSide  = get_res_value(res3_new,"tiYAxisSide","Right")
  res3_new@tiXAxisFontHeightF  = get_res_value(res3_new, \
                                 "tiXAxisFontHeightF",xaxis_fh)
  res3_new@tiYAxisFontHeightF  = get_res_value(res3_new, \
                                 "tiYAxisFontHeightF",yaxis_fh)
  res3_new@tmXBLabelFontHeightF  = get_res_value(res3_new, \
                                 "tmXBFontHeightF",xlab_fh)
  res3_new@tmYLLabelFontHeightF  = get_res_value(res3_new, \
                                 "tmYLFontHeightF",ylab_fh)
  res3_new@tmEqualizeXYSizes     = get_res_value_keep(res3_new,\
                                  "tmEqualizeXYSizes",True)
;
; Generate some dummy data for our plot.
;
  xdummy    = new( 2, double)       ; Make it double so it can handle
  ydummy    = new( 2, double)
  if (isatt(y3,"long_name")) then
      ydummy@long_name = y3@long_name
  end if
  ydummy(0) = res3_new@trYMinF   ; dummy for vertical line
  ydummy(1) = res3_new@trYMaxF
  xdummy(0) = xmax
  xdummy(1) = xmax
;
; Create the vertical line.
;
  if(plot_type.eq."csm") then
    line = gsn_csm_xy(wks,xdummy,ydummy,res3_new)
  else
    line = gsn_xy(wks,xdummy,ydummy,res3_new)
  end if

;
; Right now we are using 0.7 for an orthgonal distance for the
; single vertical axis to be positioned with respect to the 
; previous plot. The real way to  do this is to figure out the
; distance from the outermost right edge of the xy3
; plot to the plot border. Then, figure out what fraction of the
; width of the plot this is.  This fraction is how much we need to
; move the vertical line to the right. The problem is that if we
; do an NhlGetBB on the plot, it won't include the annotation that
; was added, and hence the outermost edge of the plot space is the
; same as the edge of the plot itself.
;
; At least let the user change this value if he/she wants to.
;
  anno2 = NhlAddAnnotation(xy12,line)
  amres = get_res_eq(res3,(/"am"/))
  setvalues anno2
    "amZone"           : 0     ; Zone 2 is outside the tickmarks
    "amResizeNotify"   : True  ; Resize tick marks if plot resized.
    "amJust"           : "CenterLeft"
    "amOrthogonalPosF" : get_res_value(res3,"amOrthgonalPosF",0.7)
    "amSide"           : "Right" 
  end setvalues
  attsetvalues_check(anno2,amres)

;
; Maximize plot, draw and advance the frame, if so desired.
;
    draw_and_frame(wks,xy12,calldraw,callframe,0,maxbb)

    xy12@xy3 = xy3
    return (xy12)
end

;***********************************************************************;
; Function : plot_xy2                                                   ;
;               wks: workstation object                                 ;
;                x1: X values                                           ;
;                y1: First set of Y values                              ;
;                y2: Second set of Y values                             ;
;              res1: opt'l resources, to apply to first plot            ;
;              res2: opt'l resources, to apply to second plot           ;;
;                                                                       ;
; This function calls plot_x2y2 with a single set of X values, and two  ;
; sets of Y values, each with their own Y axis, and puts them on the    ;
; same plot. The left axis is labeled according to the y1 values, and   ;
; the right axis is labeled according to the y2 values.                 ;
;                                                                       ;
; "gsn_xy" is the underlying XY plotting routine called to generate     ;
; the XY plots.                                                         ;
;                                                                       ;
; Note: the reason I didn't call this routine "gsn_xy2" is two-fold:    ;
;                                                                       ;
;    1. I would have had to put a version of "plot_x2y2" in both        ;
;       gsn_code.ncl and gsn_csm.ncl.                                   ;
;    2. It's not a widely used routine, so I didn't think people would  ;
;       miss it.  They can just call "plot_xy2".                        ;
;***********************************************************************;
undef("plot_xy2")
function plot_xy2(wks:graphic, x1:numeric, y1:numeric, y2:numeric, \
                  res1:logical, res2:logical)
begin
;
; Create some dummy variables.
;
    x2 = new(1,typeof(x1))
;
; Call plot_x2y2 function.
;
    xy = plot_x2y2 (wks, x1, x2, y1, y2, res1, res2, "y", "gsun")
    delete(x2)
    return(xy)
end

;***********************************************************************;
; Function : plot_x2y                                                   ;
;               wks: workstation object                                 ;
;                x1: First set of X values                              ;
;                x2: Second set of X values                             ;
;                y1: Y values                                           ;
;              res1: opt'l resources, to apply to first plot            ;
;              res2: opt'l resources, to apply to second plot           ;
;                                                                       ;
; This function calls plot_x2y2 with a single set of Y values, and two  ;
; sets of X values, each with their own X axis, and puts them on the    ;
; same plot. The bottom axis is labeled according to the x1 values, and ;
; the top axis is labeled according to the x2 values.                   ;
;                                                                       ;
; "gsn_xy" is the underlying XY plotting routine called to generate     ;
; the XY plots.                                                         ;
;                                                                       ;
; Note: the reason I didn't call this routine "gsn_x2y" is two-fold:    ;
;                                                                       ;
;    1. I would have had to put a version of "plot_x2y2" in both        ;
;       gsn_code.ncl and gsn_csm.ncl.                                   ;
;    2. It's not a widely used routine, so I didn't think people would  ;
;       miss it.  They can just call "plot_x2y".                        ;
;***********************************************************************;
undef("plot_x2y")
function plot_x2y(wks:graphic, x1:numeric, x2:numeric, y1:numeric, \
                  res1:logical, res2:logical)
begin
;
; Create some dummy variables.
;
    y2 = new(1,typeof(y1))
;
; Call plot_x2y2 function.
;
    xy = plot_x2y2 (wks, x1, x2, y1, y2, res1, res2, "x", "gsun")
    delete(y2)
    return(xy)
end


;***********************************************************************;;
; Function : gsn_csm_xy2                                                ;
;               wks: workstation object                                 ;
;                x1: X values                                           ;
;                y1: First set of Y values                              ;
;                y2: Second set of Y values                             ;
;              res1: opt'l resources, to apply to first plot            ;
;              res2: opt'l resources, to apply to second plot           ;
;                                                                       ;
; This function calls plot_x2y2 with a single set of X values, and two  ;
; sets of Y values, each with their own Y axis, and puts them on the    ;
; same plot. The left axis is labeled according to the y1 values, and   ;
; the right axis is labeled according to the y2 values.                 ;
;                                                                       ;
; "gsn_csm_xy" is the underlying XY plotting routine called to generate ;
; the XY plots.                                                         ;
;***********************************************************************;
undef("gsn_csm_xy2")
function gsn_csm_xy2(wks:graphic, x1:numeric, y1:numeric, y2:numeric, \
                                 res1:logical, res2:logical)
begin
;
; Create some dummy variables.
;
    x2 = new(1,typeof(x1))
;
; Call plot_x2y2 function.
;
    xy = plot_x2y2 (wks, x1, x2, y1, y2, res1, res2, "y", "csm")
    delete(x2)
    return(xy)
end

;***********************************************************************;
; Function : gsn_csm_x2y                                                ;
;               wks: workstation object                                 ;
;                x1: First set of X values                              ;
;                x2: Second set of X values                             ;
;                y1: Y values                                           ;
;              res1: opt'l resources, to apply to first plot            ;
;              res2: opt'l resources, to apply to second plot           ;
;                                                                       ;
; This function calls plot_x2y2 with a single set of Y values, and two  ;
; sets of X values, each with their own X axis, and puts them on the    ;
; same plot. The bottom axis is labeled according to the x1 values, and ;
; the top axis is labeled according to the x2 values.                   ;
;                                                                       ;
; "gsn_csm_xy" is the underlying XY plotting routine called to generate ;
; the XY plots.                                                         ;
;***********************************************************************;
undef("gsn_csm_x2y")
function gsn_csm_x2y(wks:graphic, x1:numeric, x2:numeric, y1:numeric, \
                     res1:logical, res2:logical)
begin
;
; Create some dummy variables.
;
    y2 = new(1,typeof(y1))
;
; Call plot_x2y2 function.
;
    xy = plot_x2y2 (wks, x1, x2, y1, y2, res1, res2, "x", "csm")
    delete(y2)
    return(xy)
end


;***********************************************************************;
; Function : gsn_csm_x2y2                                               ;
;               wks: workstation object                                 ;
;                x1: First set of Y values                              ;
;                x2: First set of Y values                              ;
;                y1: First set of X values                              ;
;                y2: Second set of X values                             ;
;              res1: optl resources, to apply to first plot             ;
;              res2: optl resources, to apply to second plot            ;
;                                                                       ;
; This function calls plot_x2y2 with a two sets of X *and* Y values,    ;
; and puts them on the same plot.                                       ;
;                                                                       ;
; "gsn_csm_xy" is the underlying XY plotting routine called to generate ;
; the XY plots.                                                         ;
;***********************************************************************;
undef("gsn_csm_x2y2")
function gsn_csm_x2y2(wks:graphic, x1:numeric, x2:numeric, \
                      y1:numeric, y2:numeric, res1:logical, res2:logical)
begin
;
; Call plot_x2y2 function.
;
    xy = plot_x2y2 (wks, x1, x2, y1, y2, res1, res2, "xy", "csm")
    return(xy)
end

;***********************************************************************;;
; Function : gsn_csm_xy3                                                ;
;               wks: workstation object                                 ;
;                x1 : X values                                          ;
;                y1 : First set of Y values                             ;
;                y2 : Second set of Y values                            ;
;                y3 : Third set of Y values                             ;
;              res1 : opt'l resources, to apply to first curve          ;
;              res2 : opt'l resources, to apply to second curve         ;
;              res3 : opt'l resources, to apply to third curve          ;
;                                                                       ;
; This function calls plot_xy3 with a plot_type of "csm".               ;
;***********************************************************************;
undef("gsn_csm_xy3")
function gsn_csm_xy3(wks:graphic, x1:numeric, y1:numeric, y2:numeric, \
                     y3:numeric, res1:logical, res2:logical, res3:logical)
begin
;
; Call plot_xy3 function.
;
    xy = plot_xy3 (wks, x1, y1, y2, y3, res1, res2, res3, "csm")
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
undef("gsn_csm_contour")
function gsn_csm_contour(wks:graphic,data:numeric,resources:logical)
local res, contour_object, res2, xfontf, yfontf, font_height, use_old_lbar, \
calldraw, callframe, left_string, center_string, right_string, \
main_zone, datanew, popgrid, contour_plot
begin
;
; Make sure input data is 1D or 2D
;
    if(.not.is_data_1d_or_2d(data)) then
      print("gsn_csm_contour: Fatal: the input data array must be 1D or 2D")
      return
    end if

; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),"gsn_csm_contour",res2,1)
    end if

    tm_scale      = get_res_value_keep(res2,"gsnScale",True)
    point_outward = get_res_value(res2,"gsnTickMarksPointOutward",True)

; This section tests for more special resources: those that start
; with "gsn."

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default(data,res2,"",False)) then
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

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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
; Labelbar stuff
;
; Turn on a labelbar if fill is True and if the labelbar is not
; explicitly being turned off.
;
    if(check_attr(res2,"cnFillOn",True,False)) then
      lbar_on      = get_res_value(res2,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on)
        if(use_mgr_lbar) then
          set_labelbar_res(res2,"contour")
        end if
        set_attr(res2,"cnLineLabelsOn",False)
        set_attr(res2,"cnInfoLabelOn",False)
      end if
    else
      lbar_on = get_res_value(res2,"lbLabelBarOn",False)
      use_old_lbar   = False
      use_mgr_lbar   = False
    end if

    res2          = True
    res2@gsnDraw  = False   ; Internally, don't draw plot or advance
    res2@gsnFrame = False   ; frame since we take care of that later.
    res2@gsnScale = tm_scale  ; Force labels and ticks to be same, if True.

    cnres = get_res_ne(res2,(/"tx","am"/))
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
      if(tm_scale.and.is_tm_obj_valid(contour_plot)) then
        getvalues contour_plot
          "tmYLMajorLengthF"     : ylength
          "tmXBMajorLengthF"     : xlength
          "tmYLMinorLengthF"     : ymlength
          "tmXBMinorLengthF"     : xmlength
        end getvalues

        major_length = scale * min((/ylength,xlength/))
        minor_length = scale * min((/ymlength,xmlength/))

        tmres = get_res_eq(res2,"tm")

        gsnp_point_tickmarks_outward(contour_plot,tmres,xlength,ylength,\
                                     xmlength,ymlength,\
                                     major_length,minor_length,point_outward)
      end if
    end if

; Create a labelbar or fix an existing one.
    if(use_old_lbar)
      lbres = get_res_eq(res2,(/"lb","pm"/))
      add_labelbar(wks,contour_object,3,font_height,"contour",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(contour_object,font_height,lbar_on,cnres)
    end if
  
; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,contour_object,left_string,center_string,\
                  right_string,subres)

; Draw all this stuff: contour plot and subtitles.
    draw_and_frame(wks,contour_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_hov")
function gsn_csm_hov(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, font_height, calldraw, callframe, \
datanew, lon_spacing, contour_plot
begin

; Initialize.
    res2         = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),"gsn_csm_hov",res2,1)
    end if

    calldraw     = get_res_value(res2,"gsnDraw", True)
    callframe    = get_res_value(res2,"gsnFrame",True)
    maxbb        = get_bb_res(res2)
    lon_spacing  = get_res_value(res2,"gsnMajorLonSpacing",0)
    mlon_spacing = get_res_value(res2,"gsnMinorLonSpacing",0)

    if(is_valid_latlon_coord(data,"x","lat",res2).or.\
       is_valid_latlon_coord(data,"x","lon",res2))
      set_attr(res2,"tiXAxisString","")
    end if

; Use coordinate variable for X if it exists.

    if(is_valid_coord(data,"x").and..not.(isatt(res2,"sfXCStartV").and. \
       isatt(res2,"sfXCEndV")))
      set_attr(res2,"sfXArray",data&$data!1$)
    end if

    res2           = True
    res2@gsnDraw   = False
    res2@gsnFrame  = False

    contour_object = gsn_csm_contour(wks,data,res2) ; Create a contour plot.

; Check if we should label X axis with nice longitude labels.

    if(is_valid_latlon_coord(data,"x","lon",res2)) then

; Get min and max X axis values (longitude units)

      contour_plot = check_class_name(contour_object,"contour")

      getvalues contour_plot
        "trXMinF"   : min_lon
        "trXMaxF"   : max_lon
      end getvalues
;
; We want different defaults for longitude spacing, so set them up here
;
      lon_range        = (/ 20,  40,  60, 180, 360/)
      lon_spacing_arr  = (/ 10,  20,  30,  45,  60/)
      mlon_spacing_arr = (/  5,   5,  10,  15,  30/)
      add_lon_labels(contour_plot,min_lon,max_lon,lon_spacing,mlon_spacing,\
                     lon_range,lon_spacing_arr,mlon_spacing_arr,res2)
    end if

; Draw all this stuff: contour plot and subtitles.
    draw_and_frame(wks,contour_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_lat_time")
function gsn_csm_lat_time(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, font_height, calldraw, callframe, \
datanew, lat_spacing
begin

; Initialize.
    res2         = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),"gsn_csm_lat_time",res2,1)
    end if

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
    draw_and_frame(wks,contour_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_time_lat")
function gsn_csm_time_lat(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, font_height, calldraw, callframe, \
datanew, lat_spacing
begin

; Initialize.
    res2         = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),"gsn_csm_time_lat",res2,1)
    end if

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
    draw_and_frame(wks,contour_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_pres_hgt")
function gsn_csm_pres_hgt(wks:graphic,data[*][*]:numeric,resources:logical)
local res, contour_object, res2, xfontf, yfontf, font_height, \
calldraw, callframe, add_hgt, contour_plot
begin

; Initialize.
    add_hgt       = False
    pres_reverse  =  True  ; Default is to assume pressure values
                           ; already reversed.
    res2          = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(data,new(1,float),new(1,float),"gsn_csm_pres_hgt",res2,1)
    end if

    add_hgt_label = get_res_value(res2,"gsnPresHgtHeightLabelOn",True)
    calldraw      = get_res_value(res2,"gsnDraw", True)
    callframe     = get_res_value(res2,"gsnFrame",True)
    maxbb         = get_bb_res(res2)
    lat_spacing   = get_res_value(res2,"gsnMajorLatSpacing",0)
    mlat_spacing  = get_res_value(res2,"gsnMinorLatSpacing",0)

    if(is_valid_coord(data,"y"))
      title = get_csm_long_name_units_string(data&$data!0$)
      if(.not.ismissing(title)) then
        set_attr(res2,"tiYAxisString",title)
      end if
    end if

;
; If this is a pressure/hgt plot, then do stuff to the pressure values.
;
    if(is_valid_coord(data,"y"))
      npres = dimsizes(data&$data!0$)
;
; Check for pressure values.  If they are not in descending order, then
; reverse 'em.
;
      if(data&$data!0$(0).lt.data&$data!0$(npres-1))
        pres = tofloat_wunits(data&$data!0$(::-1))  ; reverse the values
        pres_reverse =  False          ; pres values weren't already reversed
       else
        pres = tofloat_wunits(data&$data!0$)
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
          pres = tofloat_wunits(data&$data!0$) * 0.01  ; convert to mb
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
      "tiYAxisFontColor"     : yaxis_font_color
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

    if(is_valid_latlon_coord(data,"x","lat",res2)) then
      add_lat_labels_xaxis(contour_plot,xmin,xmax,lat_spacing,  \
                           mlat_spacing,res2)
    end if
;
; Add a right Y axis "height" label.
;
    if(add_hgt.and.add_hgt_label)
      rightaxis_string = create "right_axis" textItemClass wks
        "txString"      : "Height (km)"
        "txFontHeightF" : yaxis_font_height
        "txFontColor"   : yaxis_font_color
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

;
; The setting for "pmTickMarkDisplayMode" to "Always" was added in V6.2.0
; to fix a bug in which gsn_attach_plots didn't work for gsn_csm_pres_hgt
; plots. This is because loglin has this resource turned off by default,
; which means you can't retrieve any tickmark information, which is 
; required by gsn_attach_plots.
;
; We have to turn off pmTickMarkDisplayMode before leaving this function,
; however, otherwise the Y axes of the pressure/height plot won't be correct.
;
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
      "pmTickMarkDisplayMode" : "Always"
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
      delete_attr(trres,irreg_res(i))
    end do

    attsetvalues_check(loglin_object,trres)

; Turn off the tickmarks before we leave function.

    setvalues loglin_object
      "pmTickMarkDisplayMode" : "NoCreate"
    end setvalues

; Overlay this on a LogLin plot so we can get a logarithmic Y axis.

    overlay(loglin_object,contour_object)

; Draw all this stuff: contour plot and subtitles.
    draw_and_frame(wks,loglin_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_vector")
function gsn_csm_vector(wks:graphic,u[*][*]:numeric,v[*][*],resources:logical)
local res, vector_object, res2, xfontf, yfontf, font_height, use_old_lbar, \
calldraw, callframe, left_string, center_string, right_string, \
main_zone, unew, vnew
begin
; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = get_resources(resources)

;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_vector",res2,2)
    end if

    tm_scale      = get_res_value_keep(res2,"gsnScale",True)
    point_outward = get_res_value(res2,"gsnTickMarksPointOutward",True)

; This section tests for more special resources: those that start
; with "gsn."

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"",False)) then
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

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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

;---Labelbar stuff
    if(check_attr(res2,"vcMonoLineArrowColor",False,False)) then
      lbar_on = get_res_value(res2,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on) then
        if(use_mgr_lbar) then
          set_labelbar_res(res2,"vector")
        end if
      end if
    else
      lbar_on = get_res_value(res2,"lbLabelBarOn",False)
      use_old_lbar   = False
      use_mgr_lbar   = False
    end if

    res2          = True
    res2@gsnDraw  = False     ; Internally, don't draw plot or advance
    res2@gsnFrame = False     ; frame since we take care of that later.
    res2@gsnScale = tm_scale  ; Force labels and ticks to be same.

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

    if(tm_scale.and.is_tm_obj_valid(vector_plot)) then
;
; Make tick marks same length and point outward.
;
      getvalues vector_plot
        "tmYLMajorLengthF"     : ylength
        "tmXBMajorLengthF"     : xlength
        "tmYLMinorLengthF"     : ymlength
        "tmXBMinorLengthF"     : xmlength
      end getvalues

      major_length = min((/ylength,xlength/))    ; New length for major ticks.
      minor_length = min((/ymlength,xmlength/))  ; New length for minor ticks.

      major_length = scale * major_length
      minor_length = scale * minor_length

      tmres = get_res_eq(res2,"tm")
      gsnp_point_tickmarks_outward(vector_plot,tmres,xlength,ylength,\
                                   xmlength,ymlength,\
                                   major_length,minor_length,point_outward)
    end if

; Create a labelbar or fix an existing one.
    if(use_old_lbar)
      lbres = get_res_eq(res2,(/"lb","pm"/))
      add_labelbar(wks,vector_object,3,font_height,"vector",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(vector_object,font_height,lbar_on,vcres)
    end if
  
; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"am","tx"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,vector_object,left_string,center_string,\
                  right_string,subres)

; Draw all this stuff: vector plot and subtitles.
    draw_and_frame(wks,vector_object,calldraw,callframe,0,maxbb)

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
;                                                                       ;
; As of NCL V6.2.0, vcMapDirection is set to False by default.          ;
; It was discussed that this is the only vector plotting script that    ;
; should really have the default changed.                               ;
;***********************************************************************;
undef("gsn_csm_pres_hgt_vector")
function gsn_csm_pres_hgt_vector(wks:graphic,data[*][*]:numeric, \
                                 u[*][*]:numeric, v[*][*]:numeric, \
                                 resources:logical)
local res2, cnres, vcres, calldraw, callframe, npres, lbar_zone, anno_zone
begin
  res2      = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res2,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(data,u,v,"gsn_csm_pres_hgt_vector",res2,3)
  end if

  calldraw  = get_res_value(res2,"gsnDraw", True)
  callframe = get_res_value(res2,"gsnFrame",True)
  maxbb     = get_bb_res(res2)
  anno_zone = get_res_value(res2,"vcRefAnnoZone",3)
  ypos      = get_res_value(res2,"vpYF",0.87)
  info_para = get_res_value(res2,"cnInfoLabelParallelPosF",0.)
  info_just = get_res_value(res2,"cnInfoLabelJust","TopLeft")
  refanno_on= get_res_value_keep(res2,"vcRefAnnoOn",True)
  map_dir   = get_res_value_keep(res2,"vcMapDirection",False)

  if(refanno_on)
    lbar_zone = get_res_value(res2,"pmLabelBarZone",4)
    lbar_orth = get_res_value(res2,"pmLabelBarOrthogonalPosF",0.05)
  else
    lbar_zone = get_res_value(res2,"pmLabelBarZone",3)
    lbar_orth = get_res_value(res2,"pmLabelBarOrthogonalPosF",0.0)
  end if

; V6.4.0: Changed this to not exclude the "pm" resources.
; Since the contour plot is the base plot, I'm thinking that
; it *should* pass through the "pm" resources.
;  cnres = get_res_ne(res2,(/"vc","vf","pm"/))
  cnres = get_res_ne(res2,(/"vc","vf"/))
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
  vcres@vcMapDirection  = map_dir

;---If pressure values are Pascals, convert them to millibars.
  if(is_valid_coord(u,"y").and.isatt(u&$u!0$,"units").and.\
     any(u&$u!0$@units.eq.get_allowed_pres_units_pa()))
    set_attr(vcres,"vfYArray",tofloat_wunits(u&$u!0$) * 0.01)
  end if
;---If pressure values are not in descending order, reverse them.
  if(is_valid_coord(u,"y")) then
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

  draw_and_frame(wks,contour,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_streamline")
function gsn_csm_streamline(wks:graphic,u[*][*]:numeric,v[*][*],\
                            resources:logical)
local res, stream_object, res2, xfontf, yfontf, font_height, \
calldraw, callframe, left_string, center_string, right_string, \
main_zone, unew, vnew, use_old_lbar
begin
; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
    res2          = get_resources(resources)

;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
    if(isatt(res2,"gsnDebugWriteFileName")) then
      gsnp_write_debug_info(u,v,new(1,float),"gsn_csm_streamline",res2,2)
    end if

    tm_scale      = get_res_value_keep(res2,"gsnScale",True)
    point_outward = get_res_value(res2,"gsnTickMarksPointOutward",True)

; This section tests for more special resources: those that start
; with "gsn."

; Check for whether a cyclic point might need to be added.
    if(get_lon_cyclic_point_default([/u,v/],res2,"",False)) then
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

; Check for left/right titles at top. Use valid "long_name" type attributes
; and "units" if they exist.

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

;---Labelbar stuff
    if(check_attr(res2,"stMonoLineColor",False,False))
      lbar_on = get_res_value(res2,"lbLabelBarOn",True)
      use_old_lbar = get_res_value(res2,"gsnOldLabelBar",GSN_OLD_LABELBAR)
      use_mgr_lbar = .not.use_old_lbar
      if(lbar_on)
        if(use_mgr_lbar) then
          set_labelbar_res(res2,"streamline")
        end if
      end if
    else
      lbar_on = get_res_value(res2,"lbLabelBarOn",False)
      use_old_lbar   = False
      use_mgr_lbar   = False
    end if

    res2          = True
    res2@gsnDraw  = False     ; Internally, don't draw plot or advance
    res2@gsnFrame = False     ; frame since we take care of that later.
    res2@gsnScale = tm_scale  ; Force labels and ticks to be same.

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

    if(tm_scale.and.is_tm_obj_valid(streamline_plot)) then
;
; Make tick marks same length and point outward.
;
      getvalues streamline_plot
        "tmYLMajorLengthF"     : ylength
        "tmXBMajorLengthF"     : xlength
        "tmYLMinorLengthF"     : ymlength
        "tmXBMinorLengthF"     : xmlength
      end getvalues

      major_length = min((/ylength,xlength/))    ; New length for major ticks.
      minor_length = min((/ymlength,xmlength/))  ; New length for minor ticks.

      major_length = scale * major_length
      minor_length = scale * minor_length

      tmres = get_res_eq(res2,"tm")
      gsnp_point_tickmarks_outward(streamline_plot,tmres,xlength,ylength,\
                                   xmlength,ymlength,\
                                   major_length,minor_length,point_outward)
    end if

; Set up three subtitles at top, if they exist.
    subres = get_res_eq(res2,(/"am","tx"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",font_height)
    add_subtitles(wks,stream_object,left_string,center_string,\
                  right_string,subres)

; Create a labelbar or fix an existing one.
    if(use_old_lbar)
      lbres = get_res_eq(res2,(/"lb","pm"/))
      add_labelbar(wks,stream_object,3,font_height,"streamline",lbar_on,lbres)
    end if
    if(use_mgr_lbar)
      fix_labelbar(stream_object,font_height,lbar_on,stres)
    end if

; Draw all this stuff: streamline plot and subtitles.
    draw_and_frame(wks,stream_object,calldraw,callframe,0,maxbb)

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
undef("gsn_csm_pres_hgt_streamline")
function gsn_csm_pres_hgt_streamline(wks:graphic,data[*][*]:numeric, \
                                     u[*][*]:numeric, v[*][*]:numeric, \
                                     resources:logical)
local res2, cnres, stres, calldraw, callframe, npres
begin
  res2      = get_resources(resources)
;
; Write data and plot resource information to a file so we can 
; reconstruct plot if desired, without all the computational
; code beforehand.
;
  if(isatt(res2,"gsnDebugWriteFileName")) then
    gsnp_write_debug_info(data,u,v,"gsn_csm_pres_hgt_streamline",res2,3)
  end if

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
;---If pressure values are Pascals, convert them to millibars.
  if(is_valid_coord(u,"y").and.isatt(u&$u!0$,"units").and.\
     any(u&$u!0$@units.eq.get_allowed_pres_units_pa()))
    set_attr(stres,"vfYArray",tofloat_wunits(u&$u!0$) * 0.01)
  end if

  if(is_valid_coord(u,"y"))
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

  draw_and_frame(wks,contour,calldraw,callframe,0,maxbb)

; Return contour/streamline plot with data objects as attributes.

  contour@sfdata = contour@data
  contour@vcdata = streamline@data
  return(contour)
end

