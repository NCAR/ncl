;
;      $Id: gsn_csm.ncl,v 1.12 1999-06-23 14:33:04 haley Exp $
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

undef("xy_interp")
undef("ref_line_interp")
undef("set_attr")
undef("gsn_geop_hgt")
undef("check_for_subtitles")
undef("check_for_draw_and_frame")
undef("draw_and_frame")
undef("get_res_eq")
undef("get_res_ne")
undef("create_labelbar")
undef("add_subtitles")
undef("add_labelbar")
undef("set_pres_hgt_axes")
undef("fill_xy")
undef("get_lon_values")
undef("get_lat_values")
undef("get_lon_labels")
undef("get_lat_labels")
undef("add_lon_labels")
undef("add_lat_labels_xaxis")
undef("add_lat_labels_yaxis")
undef("gsn_add_cyclic_point")
undef("gsn_csm_map_ce")
undef("gsn_csm_map_polar")
undef("gsn_csm_map")
undef("gsn_csm_contour_map_polar")
undef("gsn_csm_contour_map_ce")
undef("gsn_csm_vector_map_ce")
undef("gsn_csm_vector_scalar_map_ce")
undef("gsn_csm_contour_map")
undef("gsn_csm_contour_map_overlay")
undef("gsn_csm_xy")
undef("gsn_csm_contour")
undef("gsn_csm_hov")
undef("gsn_csm_lat_time")
undef("gsn_csm_pres_hgt")


function xy_interp(x1:float, x2:float, y1:float, y2:float, ref:float)
begin
 return(x2-((y2-ref)*(x2-x1))/(y2-y1))
end

function ref_line_interp(x,y,xinterp,yinterp,ref_line)
local nlines, i, j, location, xnew, ynew, npts
begin

; Check for missing values.

  if(isatt(x,"_FillValue"))
    xmsg = x@_FillValue
  else
    xmsg = -999
  end if
  if(isatt(y,"_FillValue"))
    ymsg = y@_FillValue
  else
    ymsg = -999
  end if

; Convert x and y into two dimensional arrays so we don't have to
; test for whether we have one line or multiple lines.  Convert ref_line
; to a 1-d array.

  ndimsy = dimsizes(dimsizes(y))
  ndimsx = dimsizes(dimsizes(x))
  ndimsref = dimsizes(ref_line)

  if(ndimsy.eq.1)
    nlines     = 1
    nptsy      = dimsizes(y)

    ynew       = new((/1,nptsy/),float,ymsg)
    ynew(0,:)  = y
  else
    nlines     = dimsizes(y(:,0))
    nptsy      = dimsizes(y(0,:))

    ynew       = y
  end if
  if(ndimsx.eq.1)
    xnew       = new((/nlines,nptsy/),float,xmsg)
    do i = 0,nlines-1
      xnew(i,:)  = x
    end do
  else
    xnew       = x
  end if

  if(ndimsref.eq.1)
    refnew  = new((/nlines/),float)
    refnew  = ref_line
  else
    refnew  = ref_line
  end if

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
          xinterp(j,i+num_crosses)   = xy_interp(xnew(j,i-1),xnew(j,i),\
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

;
; Set an attribute only if it isn't already set.
;
procedure set_attr(res:logical,att_name:string,att_value)
begin
  if(.not.isatt(res,att_name))
    res@$att_name$  = att_value
  end if
  return
end

; Returns geopotential height (in km) given array p (pressure in mb);
; p must lie between 1013.25 mb and 0.010 mb.
; Algorithm is simply logarithmic interpolation from Standard Atmosphere.
; Intended to provide an ESTIMATE of geopotential height when temperature
; and geopotential data are lacking.
 
function gsn_geop_hgt( p[*]:float )
local nsa,psa,zsa,ptmp,npres,found,i,j
 
begin
  if(isatt(p,"units").and.(p@units.eq."Pascals".or.p@units.eq."Pa"))
    ptmp = p * 0.01  ; Convert to mb
  else
    ptmp = 1.* p     ; Assume already converted to mb!
  end if

  if ( .not. ( ( any(0.010.le.ptmp) ) .and. ( any(ptmp.le.1013.25) ) ) ) then
    print("FATAL ERROR CONDITION:")
    print("Argument p (pressure) does not lie between")
    print("0.010 mb and 1013.25 mb.")
    print("Execution halted in function gsn_geop_hgt.")
    exit
  end if
 
  nsa = 40
  psa = new( (/nsa/), float, 1.e36 )
  zsa = new( (/nsa/), float, 1.e36 )
 
  zsa = (/ 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,      \
           5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0,          \
           11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, \
           25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 70.0, 80.0       /)
 
  psa = (/ 1013.25, 954.61, 898.76, 845.59, 795.01, 746.91, 701.21,    \
           657.80, 616.60, 577.52, 540.48, 505.39, 472.17, 440.75,     \
           411.05, 382.99, 356.51, 331.54, 308.00, 285.84, 264.99,     \
           226.99, 193.99, 165.79, 141.70, 121.11, 103.52, 88.497,     \
           75.652, 64.674, 55.293, 25.492, 11.970, 5.746, 2.871, 1.491,\
           0.798, 0.220, 0.052, 0.010 /)

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
 
procedure check_for_subtitles(res2:logical,left:logical,center:logical,\
                              right:logical)
begin
; Initialize
  left   = False
  right  = False
  center = False

  if((res2).and..not.any(ismissing(getvaratts(res2))))
    if(isatt(res2,"gsnLeftString"))
      left            = True
      left@sub_string = res2@gsnLeftString
      delete(res2@gsnLeftString)
    end if
    if(isatt(res2,"gsnCenterString"))
      center            = True
      center@sub_string = res2@gsnCenterString
      delete(res2@gsnCenterString)
    end if
    if(isatt(res2,"gsnRightString"))
      right            = True
      right@sub_string = res2@gsnRightString
      delete(res2@gsnRightString)
    end if
  end if
  return
end

procedure check_for_draw_and_frame(res2:logical,calldraw:logical,\
                                   callframe:logical)
begin

  if((res2).and..not.any(ismissing(getvaratts(res2))))
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
  end if
  return
end

; Draw plot and advance frame.
procedure draw_and_frame(wks:graphic,plot:graphic,calldraw:logical, \
                         callframe:logical)
begin
  if(calldraw)
    draw(plot)
  end if

  if(callframe)
    frame(wks)           ; advance the frame
  end if
  return
end
;
; Get list of resources that start with res_prefix.
;
function get_res_eq(res,res_prefix:string)
local i, j, ret_res, res2, attnames, res_index
begin
  ret_res = False

  if((res).and..not.any(ismissing(getvaratts(res))))
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

;
; Get list of resources that don't start with res_prefix.
;
function get_res_ne(res,res_prefix:string)
local i, ret_res, res2, attnames, res_index
begin
  ret_res = False

  if((res).and..not.any(ismissing(getvaratts(res))))
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

;
; Create a labelbar.
;

function create_labelbar(wks:graphic, nbox:integer, colors, labels, \
lbres:logical)
begin

  labelbar_object = create "labelbar" labelBarClass wks
    "lbBoxCount"     : nbox
    "lbFillColors"   : colors
    "lbLabelStrings" : labels
    "lbAutoManage"   : False
    "lbPerimOn"      : False

    "lbMonoFillPattern" : True
    "lbLabelAlignment"  : "InteriorEdges"
    "lbMonoFillPattern" : True
  end create

  if(lbres.and..not.any(ismissing(getvaratts(lbres))))
    attsetvalues(labelbar_object,lbres)
  end if

  return(labelbar_object)
end

;
; Add three subtitles at the top.
;
;***********************************************************************;
; Function : get_polar_type                                             ;
;                 res: logical                                          ;
local text_object, parallel_pos, anno, just, strings, txres, amres, num_res
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

  if(amres.and.isatt(amres,"amOrthogonalPosF"))
    orthpos = amres@amOrthogonalPosF
    delete(amres@amOrthogonalPosF)
  else
    getvalues plot
      "vpHeightF"  : vph
    end getvalues
    orthpos = 0.01*(vph)
  end if


  if(.not.polar_nh.and..not.polar_sh) 
      text_object = create just(i)+"string" textItemClass wks
        "txString" : strings(i)
      end create

      if(txres.and..not.any(ismissing(getvaratts(txres))))
        attsetvalues(text_object,txres)   ; Set some text resources.
      end if
; Procedure : add_subtitles                                             ;
      anno = NhlAddAnnotation(plot,text_object)  ; Add annotation to plot.
;               plot: graphic                                           ;
      setvalues anno
        "amZone"          : 3             ; Just outside plot area
        "amSide"          : "top"         ; Subtitle at top.
        "amParallelPosF"  : parallel_pos(i)
        "amJust"          : "bottom"+just(i)
        "amOrthogonalPosF": orthpos       ; Move away from top edge
        "amResizeNotify"  : True          ; Resize subtitle if map resized.
      end setvalues
;***********************************************************************;
      if(amres.and..not.any(ismissing(getvaratts(amres))))
        attsetvalues(anno,amres)   ; Set some annotation resources.
procedure add_subtitles(wks:graphic,plot:graphic,left_string:logical,\
                        res:logical)
local text_object, parallel_pos, anno, just, strings, txres, amres, \
num_res, ablank, chararray, string_len
begin
  amres = False   ; Annotation resources
; 
; Add labelbar as an annotation of the given plot.
;
procedure add_labelbar(wks:graphic,plot:graphic,nbox:integer,colors,labels,\
res:logical)
local anno, num_res, amres, lbres, labelbar_object
  getvalues plot
    if(strings(i).ne."")
; Check for annotation/labelbar resources
      delete(chararray)
  lbres = get_res_eq(res,(/"lb","vp"/))
  vpres = get_res_eq(res,"vp")
  amres = get_res_eq(res,"am")
end
  labelbar_object = create_labelbar(wks, nbox, colors, labels, lbres)
      orth = get_res_value(res,"pmLabelBarOrthogonalPosF",0.05)
    else
      orth = get_res_value(res,"pmLabelBarOrthogonalPosF",0.03)
  if(amres.and..not.any(ismissing(getvaratts(amres))))
    attsetvalues(anno,amres)   ; Set some annotation resources.
  end if
    height = get_res_value(res,"pmLabelBarHeightF",height)
    width  = get_res_value(res,"pmLabelBarWidthF",0.2*width)
  else
    height = get_res_value(res,"pmLabelBarHeightF",0.3*height)
;
; Set some resources needed to label the Y and X axis with
; "nice" pressure and height values.
;
procedure set_pres_hgt_axes(pres:numeric,res2:logical,add_hgt:logical)
  set_attr(amres,"amOrthogonalPosF",orth)


  class_name = NhlClassName(plot)
    ptmp = 1.* pres(::-1)    ; reverse values
    the_plot = plot@contour
    ptmp = 1.* pres
    the_plot = plot
  end if
  res2@sfYArray  = ptmp
      "cnFillColors"      : colors
  if(.not.isatt(res2,"tmYLMode"))
    res2@tmYLMode   = "Explicit" ; Define own tick mark labels.
    res2@tmYLValues = (/1000.,  850., 700., 500., 400., 300., 250.,\
;
    res2@tmYLLabels = (/"1000","850","700","500","400","300","250",\
; have to pass these on to the labelbar.
    res2@tmYLMinorOn= False        ; No minor tick marks.
    set_attr(res2,"tiYAxisString","Pressure (mb)")
    if(.not.mono_fill_pat)
      lbres@lbMonoFillPattern = False
  if(.not.isatt(res2,"tmYRMode"))
      lbres@lbFillScales    = fill_scales
    hgt   = gsn_geop_hgt(ptmp)     ; Calculate hgts as a fcn of pre.
    hnice = fspan(2.,34.,9)        ; Set range of "nice" hgt values.
    pnice = ftcurv(hgt,ptmp,hnice) ; Get pres vals at nice hgt vals.

    res2@tmYRMode      = "Explicit"
    res2@tmYUseLeft    = False
    res2@tmYRValues    = pnice     ; At each "nice" pressure value, 
    res2@tmYRLabels    = hnice     ; put a "height" value label.
    res2@tmYROn        = True      ; Turn on right axis tick marks.
    res2@tmYRLabelsOn  = True      ; Turn on right axis tick marks.
    res2@tmYRMinorOn   = False     ; No minor tick marks.
;                res: logical                                           ;
;            add_hgt: logical                                           ;
;                                                                       ;
; This procedure sets some resources necessary to label the X and Y axes;
; with "nice" pressure and height values. The left axis is for pressure ;
; values, and the right for height values. The pressure values are      ;
; assumed to be in millibars.                                           ;
;***********************************************************************;

procedure set_pres_hgt_axes(pres:numeric,res:logical,add_hgt:logical)
;
;
; Take a set of X and Y points, and draw the Y points in one color if
; they are above the ref line, and in another color if they are below
; the ref line.
;
procedure fill_xy(wks:graphic,xy:graphic,xin[*]:float,yin[*]:float,ref:float, \
                  ref_line_above_color,ref_line_below_color)
    res@tmYLValues = (/1000.,  850., 700., 500., 400., 300., 250.,\
                         200., 150., 100.,  70., 50., 30., 10./)
    res@tmYLLabels = (/"1000","850","700","500","400","300","250",\
                         "200","150","100", "70", "50", "30", "10"/)
    res@tmYLMinorOn= False        ; No minor tick marks.
    set_attr(res,"tiYAxisString","Pressure (mb)")
;
; Calculate "nice" height values for which to label the right axis
;
    if(hrange.le.35) then
      step = 4
end
          create_gon = True
        end if
      else
        create_gon = True
            xabove_line = new(npts+2,float)
            yabove_line = new(npts+2,float)
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
          yabove_gon = new((/igon+3/),float)
          xabove_gon = new((/igon+3/),float)
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
          gsn_polygon(wks,xy,xabove_gon(0:newgon-1),yabove_gon(0:newgon-1),\
                      gsresa)
; Initialize everything back to 0
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
  if(ref_line_below_color.ne.-1)
    gsresb             = True
    gsresb@gsFillColor = ref_line_below_color
    igon      = 0
            xabove_line = new(npts+2,typeof(xin))
            yabove_line = new(npts+2,typeof(yin))
            yabove_line(0) = yin(i)
            xabove_line(0) = xin(i)
            igon = 1
            xbelow_line = new(npts+2,float)
            ybelow_line = new(npts+2,float)
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
          ybelow_gon = new((/igon+3/),float)
          xbelow_gon = new((/igon+3/),float)
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
          gsn_polygon(wks,xy,xbelow_gon(0:newgon-1),ybelow_gon(0:newgon-1),\
                      gsresb)
; Initialize everything back to 0
        igon       = 0
        create_gon = False
      end if
    end do
  end if

;
; Loop through and add below-ref-line polygons.
;
  if(ref_line_below_color.ne.-1.and.bnumgons.gt.0) then

  return
    create_gon = False

;
; Calculate "nice" longitude values for longitude labels.
;
function get_lon_values(min_lon:float,max_lon:float)
            end if
lon_starts, lon_ends, start_lon, end_lon
;
; Initialize polygon.
;
          npts  = ept - bpt + 1
  lcheck_arr  = (/ 20, 40, 60, 360/) ; lon range will determine
                                     ; spacing of minor/major ticks
  lspcng_arr  = (/  5, 10, 20, 30/)  ; spacings for major ticks
  mlspcng_arr = (/  1,  2,  5, 10/)  ; spacings for minor ticks
          end if
; Longitude range, will help determin longitude spacing.
; Add polygon to XY plot.
;
          var_string = systemfunc("echo fill_polygon$$") + npoly
          plot@$var_string$ = gsn_add_polygon(wks,plot, \
  lon_spcng  = lspcng_arr(lon_ind(0))  ; spacing for lon major tickmarks
  mlon_spcng = mlspcng_arr(lon_ind(0)) ; spacing for lon minor tickmarks
  delete(lon_ind)
  
  if(any((/min_lon,max_lon/).lt.0))
    lon_starts = ispan(-180,180,mlon_spcng)   ; Pick nice values to start
    lon_ends   = ispan(180,-180,mlon_spcng)   ; and end the longitudes labels.
; Advance polygon counter.
    lon_starts = ispan(0,360,mlon_spcng)  ; Pick nice values to start
    lon_ends   = ispan(360,0,mlon_spcng)  ; and end the longitudes labels.
          npoly = npoly + 1
          bpt = -1            ; Reinitialize
  lon_ind    = ind(min_lon.ge.lon_starts)
  start_lon  = lon_starts(lon_ind(0))
  delete(lon_ind)

  lon_ind  = ind(max_lon.le.lon_ends)
  end_lon  = lon_ends(lon_ind(0))
  return(plot)
function get_lon_values(min_lon:numeric,max_lon:numeric,lon_spacing:numeric,\
                        mlon_spacing:numeric,range_array,lonspacing_array, \
                        mlonspacing_array)
  lon_values@minor = ispan(min(lon_values),max(lon_values),mlon_spcng)
start_lon, end_lon, xstart_lon, xend_lon
begin

;
; Calculate "nice" latitude values for latitude labels.
;
function get_lat_values(min_lat:float,max_lat:float)
local lat_range, lcheck_arr, lspcng_arr, mlspcng_arr, lat_spcng, mlat_spcng, \
lat_starts, lat_ends
  else
    mlspcng_arr = mlonspacing_array
  end if

    lon_spcng  = lspcng_arr(lon_ind(0))  ; spacing for lon major tickmarks
  else
    lon_spcng = tointeger(lon_spacing)
  end if

; Latgitude range, will help determin latgitude spacing.
    mlon_spcng = mlspcng_arr(lon_ind(0)) ; spacing for lon minor tickmarks
  else
    mlon_spcng = tointeger(mlon_spacing)
  end if
  lat_spcng  = lspcng_arr(lat_ind(0))  ; spacing for lat major tickmarks
  mlat_spcng = mlspcng_arr(lat_ind(0)) ; spacing for lat minor tickmarks
     if((lon % lon_spcng).eq.0)
       start_lon = lon
  lat_starts = ispan(-90,90,mlat_spcng)   ; Pick nice values to start
  lat_ends   = ispan(90,-90,mlat_spcng)   ; and end the latgitudes labels.

  lat_ind    = ind(min_lat.ge.lat_starts)
  start_lat  = lat_starts(lat_ind(0))
  delete(lat_ind)

  lat_ind  = ind(max_lat.le.lat_ends)
  end_lat  = lat_ends(lat_ind(0))
  delete(lat_ind)
;        mlat_spacing: numeric                                          ;
;                                                                       ;
; Calculate "nice" latitude values to use for placing latitude labels   ;
  lat_values@minor = ispan(min(lat_values),max(lat_values),mlat_spcng)
; lat_spacing and mlat_spacing are the spacings to use for major and    ;
; minor tickmarks.                                                      ;
;***********************************************************************;
;
; Create some nice longitude labels given longitude values.
;
; Initialize

  if(min_lat.lt.-90.or.max_lat.gt.90) then
    print("get_lat_values: Warning: Your latitude values do not fall between -90 and 90 inclusive.")
    print("You will not get nice get 'nice' latitude labels.")
  lon_index = ind(lon_values.gt.180.and.lon_values.lt.360)
  if(.not.all(ismissing(lon_index)))
    lon_labels(lon_index) = 360-lon_values(lon_index) + "W" ; west
  end if
  delete(lon_index)
    return(lat_values)
  end if

  lcheck_arr  = (/ 20, 40, 60, 180/) ; lat range will determine
                                     ; spacing of minor/major ticks
  lspcng_arr  = (/  5, 10, 20, 30/)  ; spacings for major ticks
  mlspcng_arr = (/  1,  2,  5, 10/)  ; spacings for minor ticks

  lat_range = max_lat - min_lat   ; lat/lat range

  lat_ind    = ind(lat_range.le.lcheck_arr)
  if(lat_spacing.le.0)
    lat_spcng = lspcng_arr(lat_ind(0))  ; spacing for lat major tickmarks
  lon_index = ind(lon_values.gt.180)

  if(mlat_spacing.le.0)
    mlat_spcng = mlspcng_arr(lat_ind(0)) ; spacing for lat minor tickmarks
  else
    mlat_spcng = tointeger(mlat_spacing)
  lon_index = ind(lon_values.eq.0.or.fabs(lon_values).eq.180)

    lon_labels(lon_index) = fabs(lon_values(lon_index))      ; 0 or 180
  lat_values       = ispan(start_lat,end_lat,lat_spcng)
  lat_values@minor = ispan(min(lat_values)-lat_spcng,max(lat_values)+lat_spcng,mlat_spcng)
  return(lat_values)
  lon_index = ind(lon_values.eq.360)
; Function : get_lon_labels                                             ;
;                lon_values[*]: numeric                                 ;
;                                                                       ;
; Return some nice longitude labels given longitude values "lon_values".;
;***********************************************************************;

function get_lon_labels(lon_values:numeric)
local lon_index
;
; Create some nice latitude labels given latitude values.
;
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

;
; Add "nice" longitude labels to the X axis.
;
procedure add_lon_labels(plot:graphic,min_lon:numeric,max_lon:numeric)

  lonvalues = get_lon_values(min_lon,max_lon)
  lonlabels = get_lon_labels(lonvalues)
  return(lon_labels)
  setvalues plot
    "tmXBMode"         : "Explicit" ; Indicate that we want to 
    "tmXBLabels"       : lonlabels    ; explicitly label the X axis.
    "tmXBValues"       : lonvalues
    "tmXBMinorValues"  : lonvalues@minor
  end setvalues
; Create labels for latitude tick marks.
  lat_labels = new(dimsizes(lat_values),string)

;
; Add "nice" latitude labels to the X axis.
;
procedure add_lat_labels_xaxis(plot:graphic,min_lat:numeric,max_lat:numeric)
  return(lat_labels)
  latvalues = get_lat_values(min_lat,max_lat)
  latlabels = get_lat_labels(latvalues)
;             max_lon: numeric                                          ;
  setvalues plot
    "tmXBMode"         : "Explicit" ; Indicate that we want to 
    "tmXBLabels"       : latlabels    ; explicitly label the Y axis.
    "tmXBValues"       : latvalues
    "tmXBMinorValues"  : latvalues@minor
  end setvalues

  lon_spacing2 = get_res_value(tmres,"tmXBTickSpacingF",lon_spacing)
  lonvalues   = get_lon_values(min_lon,max_lon,lon_spacing2,mlon_spacing,\
;
; Add "nice" latitude labels to the Y axis.
;
procedure add_lat_labels_yaxis(plot:graphic,min_lat:numeric,max_lat:numeric)
end
  latvalues = get_lat_values(min_lat,max_lat)
  latlabels = get_lat_labels(latvalues)
;         lat_spacing: numeric                                          ;
  setvalues plot
    "tmYLMode"         : "Explicit" ; Indicate that we want to 
    "tmYLLabels"       : latlabels    ; explicitly label the Y axis.
    "tmYLValues"       : latvalues
    "tmYLMinorValues"  : latvalues@minor
  end setvalues

  lat_spacing2 = get_res_value(tmres,"tmXBTickSpacingF",lat_spacing)
  latvalues    = get_lat_values(min_lat,max_lat,lat_spacing2,mlat_spacing)
;
; Add a cyclic point in "x" to a 2D array
; for a lat/lon plot "x"  corresponds to "lon"
;                    "ny" corresponds to "nlat"
;                    "mx" corresponds to "mlon"
function gsn_add_cyclic_point(data[*][*]:float)
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
    calldraw  = True
    callframe = True

    map_vpxf      = 0.1       ; X position of map plot in viewport.
    map_vpyf      = 0.9       ; Y position of map plot in viewport.
    map_vpwf      = 0.8       ; Viewport width and height for
    map_vphf      = 0.8       ; map plot.
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
    do while(i.lt.dimsizes(valid_popgrids).and..not.found)
    check_for_draw_and_frame(res2,calldraw,callframe)

;
    latlon_res = True
      main_zone   = 3
    latlon_res@gsnFrame             = False
;
; If mpCenterLonF is set, but mpMinLonF and/or minMaxLonF are not,
; then we need to set them.
;
; This function creates and draws a labeled cylindrical equidistant map ;
           .not.isatt(res2,"mpMinLonF").and.\
           .not.isatt(res2,"mpMaxLonF"))
      res2@mpMinLonF = res2@mpCenterLonF - 180
      res2@mpMaxLonF = res2@mpCenterLonF + 180
; This function behaves differently from gsn_map in that it will        ;

    if(res2.and.isatt(res2,"mpCenterLonF").and..not.isatt(res2,"mpMinLonF"))
      res2@mpMinLonF = res2@mpCenterLonF - (res2@mpMaxLonF-res2@mpCenterLonF)
;       "gsnCenterString," and/or "gsnRightString" are set, then they   ;

    if(res2.and.isatt(res2,"mpCenterLonF").and..not.isatt(res2,"mpMaxLonF"))
      res2@mpMaxLonF = res2@mpCenterLonF + (res2@mpCenterLonF-res2@mpMinLonF)


    if(res2.and.isatt(res2,"mpMinLonF"))
      min_lon = res2@mpMinLonF
    else
      min_lon = -180
    end if

    if(res2.and.isatt(res2,"mpMaxLonF"))
      max_lon = res2@mpMaxLonF
    else
      max_lon = 180
    end if

    if(res2.and.isatt(res2,"mpMinLatF"))
      min_lat = res2@mpMinLatF
    else
      min_lat = -90
    end if

    if(res2.and.isatt(res2,"mpMaxLatF"))
      max_lat = res2@mpMaxLatF
    else
      max_lat = 90
    end if
; Initialize.
    main_zone     = 2         ; Zone for main title (may change later)
      print("The resource mpMinLonF must be less than the resource mpMaxLonF.")
; This section tests for more special resources: those that start
; with "gsn."

; Check if frame and/or draw are not supposed to be called.
      print("The resource mpMinLatF must be less than the resource mpMaxLatF.")
    maxbb     = get_bb_res(res2)
    

    lat_range = 1.*(max_lat - min_lat)
    lon_range = 1.*(max_lon - min_lon)

    left_string   = new(1,logical)
    center_string = new(1,logical)
    right_string  = new(1,logical)

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
    map_vpwf = res2@vpWidthF
    lat_spacing  = get_res_value(res2,"gsnMajorLatSpacing",0.)
    lat_spacing  = get_res_value(res2,"tmYLTickSpacingF",lat_spacing)
    map_vphf = res2@vpHeightF

    set_attr(res2,"vpXF",(1. - map_vpwf)/2.)
    map_vpxf  = res2@vpXF

    set_attr(res2,"vpYF",1. - (1. - map_vphf)/2.)
    map_vpyf = res2@vpYF

     return 
   end if
;
; If mpCenterLonF is set, but mpMinLonF/mpLeftCornerLonF and/or 

    map_object = create "map" mapPlotClass wks
      "vpXF"            : map_vpxf
      "vpYF"            : map_vpyf
      "vpWidthF"        : map_vpwf
      "vpHeightF"       : map_vphf
      "mpLimitMode"     : "LatLon"
      "mpMinLonF"       : min_lon
      "mpMaxLonF"       : max_lon
      "mpMinLatF"       : min_lat
      "mpMaxLatF"       : max_lat
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
    min_lat = get_res_value(res2,min_lat_str, -90)
    max_lat = get_res_value(res2,max_lat_str,  90)

    if(min_lon.ge.max_lon)
      print("gsn_csm_map_ce: Fatal: The resources mpMinLonF/mpLeftCornerLonF must be less than the resources mpMaxLonF/mpRightCornerLonF.")
      print("Execution halted.")
;
; If mpCenterLonF is set, but mpMinLonF and/or minMaxLonF are not,
; then we need to set them.
;
    if(isatt(res2,"mpCenterLonF"))
      if(.not.isatt(res2,"mpMinLonF").and..not.isatt(res2,"mpMaxLonF"))
        res2@mpMinLonF = res2@mpCenterLonF - 180
        res2@mpMaxLonF = res2@mpCenterLonF + 180
      else
        set_attr(res2,"mpMinLonF",res2@mpCenterLonF-(res2@mpMaxLonF-res2@mpCenterLonF))
        set_attr(res2,"mpMaxLonF",res2@mpCenterLonF+(res2@mpCenterLonF-res2@mpMinLonF))
      end if
    end if

;
; By default, mpOutlineOn is False, unless mpFillOn is set to False,
; then it is set back to True.
;
    if((isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
      set_attr(res2,"mpOutlineOn",True)
    end if
      exit
    end if

    if(min_lat.ge.max_lat)
    mpres = get_res_ne(res2,"tm")  ; Get rest of resources
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if

; Retrieve the view port location of the map plot so we know where
; to put lat/lon labels.

    getvalues map_object
      "vpXF"         : vpxf
      "vpYF"         : vpyf
      "vpWidthF"     : vpwf
      "vpHeightF"    : vphf
      "mpMinLonF"    : min_lon
      "mpMaxLonF"    : max_lon
      "mpMinLatF"    : min_lat
      "mpMaxLatF"    : max_lat
      "mpCenterLonF" : center_lon
    end getvalues

; Create tickmark values for lat/lon labels.

   lonvalues = get_lon_values(min_lon,max_lon)
   latvalues = get_lat_values(min_lat,max_lat)

; Create labels for lat/lon tick marks.

    latlabels  = get_lat_labels(latvalues)
    lonlabels  = get_lon_labels(lonvalues)

; Create a TickMark object to label lat/lon grid.

    tickmark_object = create "tickmarks" tickMarkClass wks
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

    if(tmres.and..not.any(ismissing(getvaratts(tmres))))
      attsetvalues(tickmark_object,tmres)
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
      "tmXBLabelFontHeightF"    : min((/xbfontf,ylfontf/))
      "tmYLLabelFontHeightF"    : min((/xbfontf,ylfontf/))
      "tmXBMajorLengthF"        : major_length
      "tmXBMinorLengthF"        : minor_length
      "tmYLMajorLengthF"        : major_length
      "tmYLMinorLengthF"        : minor_length
      "tmXBMajorOutwardLengthF" : major_length
      "tmXBMinorOutwardLengthF" : minor_length
      "tmYLMajorOutwardLengthF" : major_length
      "tmYLMinorOutwardLengthF" : minor_length
      "tmXTLabelFontHeightF"    : min((/xbfontf,ylfontf/))
      "tmYRLabelFontHeightF"    : min((/xbfontf,ylfontf/))
      "tmXTMajorLengthF"        : major_length
      "tmXTMinorLengthF"        : minor_length
      "tmYRMajorLengthF"        : major_length
      "tmYRMinorLengthF"        : minor_length
      "tmXTMajorOutwardLengthF" : major_length
      "tmXTMinorOutwardLengthF" : minor_length
      "tmYRMajorOutwardLengthF" : major_length
      "tmYRMinorOutwardLengthF" : minor_length
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

    set_attr(res2,"vpHeightF",min((/lat_range/lon_range,1./)) * def_vphf)
    set_attr(res2,"vpXF",(1. - res2@vpWidthF)/2.)
    if(isatt(res2,"tiMainFontHeightF"))
      main_font_height = res2@tiMainFontHeightF
    else
      main_font_height = 1.3*font_height
    end if
    if(isatt(res2,"tiXAxisFontHeightF"))
      x_font_height = res2@tiXAxisFontHeightF
    else
      x_font_height = font_height
    end if
    if(isatt(res2,"tiYAxisFontHeightF"))
      y_font_height = res2@tiYAxisFontHeightF
    else
      y_font_height = font_height
    end if
                                   "transparent"/)) ; (default,ocean,land,
      "tiXAxisFontHeightF" : x_font_height      ; X axis label size
      "tiYAxisFontHeightF" : y_font_height      ; Y axis label size
                                                    ;  inland water)
      "tiMainFont"         : "helvetica-bold" ; main title font
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
;    
    subres@txFontHeightF    = font_height
    subres@amOrthogonalPosF = 0.05
; map later with setvalues, as gsn_map would do, then the size wouldn't
; be correct.
;
    if(lower_case(limit_mode).eq."latlon")
      map_object = create "map" mapPlotClass wks
    draw_and_frame(wks,map_object,calldraw,callframe)
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
    calldraw  = True
    callframe = True

    yoffset       = 0.08
    mpres = get_res_ne(res2,(/"tm","tx","am"/))  ; Get rest of resources
    polar_sh      = False
    polar_nh      = False
    attsetvalues_check(map_object,mpres)
;
; Add tickmark object, even if the tickmarks have been turned off, because
    left_string   = False     ; Default is no subtitles unless
    center_string = False     ; they are explicitly set with the
    right_string  = False     ; special "gsn" resources.
    sub_zone      = 2         ; Zone for subtitles
; we need to get the size of the X/Y axis labels.
    res2          = resources
    add_map_tickmarks(wks,map_object,tmres,lon_spacing,mlon_spacing,\
                      lat_spacing,mlat_spacing)

    check_for_draw_and_frame(res2,calldraw,callframe)
    end getvalues
;
; Check if user setting own font heights.
      main_zone   = 3
    main_font_height = get_res_value_keep(res2,"tiMainFontHeightF", \
      "tiMainFontHeightF"  : main_font_height ; main title size
    if((res2).and..not.any(ismissing(getvaratts(res2))))

; Check for whether user wants northern or southern hemisphere.

        if(isatt(res2,"gsnPolarSH").and.res2@gsnPolarSH)
            polar_sh   = True   ; Southern hemisphere
            min_lat    = -90
            max_lat    =   0
            center_lat = -90
            delete(res2@gsnPolarSH)
        end if
        if(isatt(res2,"gsnPolarNH").and.res2@gsnPolarNH)
              polar_nh   = True   ; Northern hemisphere
              min_lat    =   0 
              max_lat    =  90
              center_lat =  90
              delete(res2@gsnPolarNH)
        end if
    end if

    if(polar_nh.and.polar_sh) 
        print("Fatal: You have set both gsnPolarNH and gsnPolarSH to True.")
        print("Only one of them can be True, so please correct this.")
        exit        
    end if

    if(.not.polar_nh.and..not.polar_sh) 
        print("Warning: Neither gsnPolarNH or gsnPolarSH have been set to True,")
        print("so will default to plotting the northern hemisphere.")
        polar_nh = True
    add_subtitles(wks,map_object,left_string,center_string,right_string, \
                  subres)
    if(isatt(res2,"vpHeightF"))
      height = res2@vpHeightF
      delete(res2@vpHeightF)
    else
      height = 0.62
    end if

    if(isatt(res2,"vpWidthF"))
      width = res2@vpWidthF
      delete(res2@vpWidthF)
    else
      width = 0.62
    end if

    if(isatt(res2,"vpXF"))
      xpos = res2@vpXF
      delete(res2@vpXF)
    else
      xpos = 0.20
    end if

    if(isatt(res2,"vpYF"))
      ypos = res2@vpYF
      delete(res2@vpYF)
    else
      ypos = 0.80
    end if

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
      "mpLimitMode"          : "LatLon"
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
    set_attr(res2,            "mpMinLatF",   min_lat)
    set_attr(res2,            "mpMaxLatF",   max_lat)
    set_attr(res2,         "mpCenterLatF",center_lat)
    set_attr(res2,          "mpOutlineOn",     False)
    set_attr(res2,      "mpGridAndLimbOn",      True)
    set_attr(res2,"mpGridLineDashPattern",         2)
    set_attr(res2,    "mpGridLonSpacingF",       30.)
    set_attr(res2,            "mpPerimOn",      True)

side, just, orth, para, quad1, quad2, quad3, quad4, topbot, xcen, ycen, \
; By default, mpOutlineOn is False, mpFillOn is set to False, then 
; it is set back to True.
    main_zone     = 2         ; Zone for main title (may change later)
    if((isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
      res2@mpOutlineOn = True
    end if

; This section tests for special resources: those that start with "gsn."
    mpres = get_res_ne(res2,"gs")
    calldraw     = get_res_value(res2,"gsnDraw", True)
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    maxbb        = get_bb_res(res2)
    polar_dst    = get_res_value(res2,"gsnPolarLabelDistance",1.04)
    ticks_on     = get_res_value(res2,"gsnTickMarksOn",True)

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
    if(isatt(res2,"tiMainFontHeightF"))
      main_font_height = res2@tiMainFontHeightF
    else
      main_font_height = 2.*font_height
    end if
    if(isatt(res2,"tiXAxisFontHeightF"))
      x_font_height = res2@tiXAxisFontHeightF
    else
      x_font_height = 1.2*font_height
    end if
    if(isatt(res2,"tiYAxisFontHeightF"))
      y_font_height = res2@tiYAxisFontHeightF
    else
      y_font_height = 1.2*font_height
    end if
      center_lat = -90
        "tiXAxisFontHeightF"   : x_font_height      ; x axis font size
        "tiYAxisFontHeightF"   : y_font_height      ; y axis font size
        "tiMainOffsetYF"       : main_offset        ; main title offset
        "tiMainFont"           : "helvetica-bold"   ; main title font
        "tiMainFontHeightF"    : main_font_height   ; main title size
    width  = get_res_value(res2,"vpWidthF",0.62)
    xpos   = get_res_value(res2,"vpXF",0.2)
; Set up three subtitles at top, if they exist.
    subres                  = True
    subres@txFontHeightF    = 1.2*font_height
    subres@txFont           = "helvetica-bold"
    subres@amOrthogonalPosF = 0.06 
    add_subtitles(wks,map_object,left_string,center_string,right_string, \
                  subres)

;
; Check for other resources that we need to set at the time the
; map is created.
;
    limit_mode   = get_res_value(res2,"mpLimitMode","LatLon")
    ngrid = floattoint(360/res2@mpGridLonSpacingF)

    if(min((/min_lat,max_lat/)).ge.0.or.polar_nh)
        plat = fspan(min_lat,min_lat,ngrid)
    else
        plat = fspan(max_lat,max_lat,ngrid)
    end if 
    plon = fspan(0.,360.-res2@mpGridLonSpacingF,ngrid)
      "vpYF"                 : ypos
; Increase the viewport of the map object; this is where the labels
; will get drawn.

    setvalues map_object
        "vpXF"      : vpxf-0.02
        "vpYF"      : vpyf+0.02
        "vpWidthF"  : vpwf+0.04
        "vpHeightF" : vphf+0.04
    end setvalues


      "mpProjection"         : "Stereographic"
      "mpEllipticalBoundary" : True
    xndc = new(dimsizes(plon),float)
    yndc = new(dimsizes(plat),float)

      "mpFillOn"             : True
      "mpFillColors"         : (/"background","transparent","LightGray",\
                                 "transparent"/)
    datatondc(map_object,plon,plat,xndc,yndc)
      "pmTitleZone"          : main_zone
    set_attr(res2,"mpMaxLatF",max_lat)
    set_attr(res2,"mpCenterLatF",center_lat)
    labels = new(dimsizes(plon),string)
    set_attr(res2,"mpGridAndLimbOn",True)
    indexes = ind(plon.gt.0.and.plon.lt.180)
    labels(indexes) = floattoint(plon(indexes)) + "E"
    delete(indexes)

    indexes = ind(plon.gt.180.and.plon.lt.360)
    labels(indexes) = floattoint(360-plon(indexes)) + "W"
    delete(indexes)

    indexes = ind(plon.eq.0.or.plon.eq.0.or.plon.eq.180)
    labels(indexes) = floattoint(plon(indexes))
    delete(indexes)
; then it is set back to True.
    num_labels = dimsizes(labels)
    text_ids = new(num_labels,graphic)

; to put titles.

    getvalues map_object
    do i=0,num_labels-1
      text_ids(i) = create labels(i) textItemClass wks
        "txString"      : labels(i)
        "txFont"        : "helvetica-bold"
        "txFontHeightF" : font_height
      end create
    end do

; Set map back to original viewport.

    setvalues map_object
        "vpXF"            : vpxf
        "vpYF"            : vpyf
        "vpWidthF"        : vpwf
        "vpHeightF"       : vphf
    end setvalues
                                  ; of the height of the viewport.
; Retrieve graphic style object.
                                          2.*font_height)
    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues
    main_font = get_res_value_keep(res2,"tiMainFont","helvetica-bold")
; Create a LogLinPlot that covers the entire NDC space
; to use as a drawing canvas, and add the text items as annotations.
      end do
    canvas = create "canvas" logLinPlotClass wks
      "vpXF"      : vpxf-0.03
      "vpYF"      : vpyf+0.03
      "vpWidthF"  : vpwf+0.06
      "vpHeightF" : vphf+0.06
      "pmAnnoViews" : text_ids
    end create

; Compute new locations of labels with regard to canvas we just created.

    correction = 0.01       ; Not sure why we need this, but it seems
                            ; to adjust the labels to the right positions.

    new_xndc = (xndc-(vpxf-0.03))/(vpwf+0.06)+correction
    new_yndc = (yndc-(vpyf+0.03-(vphf+0.06)))/(vphf+0.06)-correction

; Retrieve the ids of the AnnoManager objects created by the PlotManager
; and then set their location in data coordinate space. The AnnoManager
; objects are arranged in the same order as the TextItems in the
; pmAnnoViews resource.

    getvalues canvas
      "pmAnnoManagers" : am_ids
    end getvalues

    do i=0,num_labels-1
      setvalues am_ids(i)
        "amTrackData"    : True
        "amDataXF"       : new_xndc(i)
        "amDataYF"       : new_yndc(i)
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
; for justification, side, orthogonal and parallel positions.
    draw_and_frame(wks,map_object,calldraw,callframe)
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
  if(res.and.(isatt(res,"gsnPolarNH").and.res@gsnPolarNH.or.\
             isatt(res,"gsnPolarSH").and.res@gsnPolarSH))
    return(gsn_csm_map_polar(wks,res))
    set_attr(res2,"mpPerimOn", True)
    return(gsn_csm_map_ce(wks,res))
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
main_offset, addcyclic, datanew, res2, cnres, mpres, \
vpwf, vphf, font_height, levels, colors, zonal_zone, \
lbres, infolabel_on, infolabel_zone, \
lbar_zone, lbar_orient, lbar_side, lbar_height, lbar_width, lbar_just
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres                  = True
    set_attr(subres,"txFontHeightF",0.8*font_height)
    calldraw  = True
    callframe = True

    set_attr(subres,"amOrthogonalPosF",0.015)
    addcyclic     = True      ; Default is cyclic data
    res2          = resources ; Copy of resources
                  subres)

    polar_nh      = False     ; Northern hemisphere
    polar_sh      = False     ; Southern hemisphere
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
    check_for_draw_and_frame(res2,calldraw,callframe)
begin
; If the data has an attribute called "long_name",
; then use this string for the left title.

    if(.not.isatt(res2,"gsnLeftString"))
      if(isatt(datanew,"long_name"))
        mpres@gsnLeftString = datanew@long_name
        delete(datanew@long_name)
      end if
    else
      mpres@gsnLeftString = res2@gsnLeftString
      delete(res2@gsnLeftString)
    end if

; If the data has an attribute called "units",
; then use this string for the right title.

    if(.not.isatt(res2,"gsnRightString"))
      if(isatt(datanew,"units"))
        mpres@gsnRightString = datanew@units
        delete(datanew@units)
      end if
    else
      mpres@gsnRightString = res2@gsnRightString
      delete(res2@gsnRightString)
    end if

    if((res2).and..not.any(ismissing(getvaratts(res2))))

; Check for center title at top.

      if(isatt(res2,"gsnCenterString"))
        mpres@gsnCenterString = res2@gsnCenterString
        delete(res2@gsnCenterString)
      end if

; Check for whether user wants northern or southern hemisphere.

      if(isatt(res2,"gsnPolarSH"))
        if(res2@gsnPolarSH)
          mpres@gsnPolarSH = True
          polar_sh = True
        end if
        delete(res2@gsnPolarSH)
      end if
      if(isatt(res2,"gsnPolarNH"))
       if(res2@gsnPolarNH)
          mpres@gsnPolarNH = True
          polar_nh = True
        end if
        delete(res2@gsnPolarNH)
      end if
    end if

    if(polar_nh.and.polar_sh) 
        print("Fatal: You have set both gsnPolarNH and gsnPolarSH to True.")
        print("Only one of them can be True, so please correct this.")
        exit        
    end if

    if(.not.polar_nh.and..not.polar_sh) 
        print("Warning: Neither gsnPolarNH or gsnPolarSH have been set to True,")
        print("so will default to plotting the northern hemisphere.")
        mpres@gsnPolarNH = True
    end if
    return(gsn_csm_map_polar(wks,res2))
; list of resources. The id of the map plot is returned.                ;
;                                                                       ;
    if(iscoord(datanew,"lat").and.iscoord(datanew,"lon"))
      res2@sfYArray  = datanew&lat
      res2@sfXArray  = datanew&lon
    else
      if(iscoord(datanew,"latitude").and.iscoord(datanew,"longitude"))
        res2@sfYArray  = datanew&latitude
        res2@sfXArray  = datanew&longitude
      end if
    end if
;                                                                       ;
; Check for a missing value.

    if(isatt(datanew,"_FillValue")) then
      res2@sfMissingValueV = datanew@_FillValue
    end if

;    1. By default, a cyclic point is added.  If gsnAddCyclic is set to ;
;       False, then the cyclic point is not added.                      ;
    res2@cnInfoLabelFont            = "helvetica"
    res2@cnLineLabelBackgroundColor = -1
    res2@cnLineLabelFont            = "helvetica"
    res2@cnInfoLabelOrthogonalPosF  = 0.06
    res2@cnInfoLabelZone            = 2
;       are used to title the top of the plot.                          ;
;       (in addition, the regular resource "tiMainString" can be set to ;
;       create a regular title).                                        ;
;    4. If the resource "cnFillOn" is set to True, then a labelbar is   ;
;       drawn and line labels are turned off.                           ;
    if((isatt(res2,"cnFillOn").and.res2@cnFillOn).or.\
       (isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        res2@mpOutlineOn  = True
;    6. If data has an attribute called "units," and "gsnRightString"   ;
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the right string title.                             ;
;    7. If either of the resources "gsnPolarNH" or "gsnPolarSH" are set ;
    lbres = get_res_eq(res2,"lb")
    mpres = get_res_eq(res2,(/"mp","ti","vp"/))

    cnres = get_res_ne(res2,(/"mp","ti","vp","lb"/))
function gsn_csm_contour_map_polar(wks:graphic,data[*][*]:numeric, \
      if(isatt(cnres,"cnFillOn").and.cnres@cnFillOn)
	set_attr(cnres,"cnInfoLabelOn",False)
        if(.not.isatt(cnres,"lbLabelBarOn").or.\
          isatt(cnres,"lbLabelBarOn").and.cnres@lbLabelBarOn)
infolabel_on, infolabel_zone, lbar_zone, lbar_height, lbar_width
begin

; Initialize.
    lbar_on       = False     ; Labelbar flag
    res2          = get_resources(resources) ; Copy of resources
    lbar_zone     = 3         ; Zone for labelbar (may change later)
    mpres         = True      ; Will hold map resources

    if(isatt(cnres,"cnInfoLabelOn").and..not.cnres@cnInfoLabelOn)

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
; Check for type of polar plot desired.
;
    mpres@gsnPolar = get_polar_type(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
;
; Tickmarks.
;
    mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    if(isatt(res2,"tiMainFontHeightF"))
      main_font_height = res2@tiMainFontHeightF
    else
      main_font_height = 2.*font_height
    end if
    if(isatt(res2,"tiXAxisFontHeightF"))
      x_font_height = res2@tiXAxisFontHeightF
    else
      x_font_height = 1.2*font_height
    end if
    if(isatt(res2,"tiYAxisFontHeightF"))
      y_font_height = res2@tiYAxisFontHeightF
    else
      y_font_height = 1.2*font_height
    end if
    if(isatt(res2,"gsnPolarLabelSpacing"))
        "tiXAxisFontHeightF"   : x_font_height      ; x axis font size
        "tiYAxisFontHeightF"   : y_font_height      ; y axis font size
        "tiMainFont"           : "helvetica-bold"   ; main title font
        "tiMainFontHeightF"    : main_font_height   ; main title size
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

; Generate a labelbar from the above information.
; If lbOrientation has been set to "vertical", then we have to position
; the labelbar differently.  The default is "horizontal".

      lbar_orient = "horizontal"
      lbar_height = 0.20 * vphf
      lbar_width  = 0.75 * vpwf
      lbar_side   = "bottom"
      lbar_just   = "bottomcenter"

      if(lbres) then
        if(isatt(lbres,"lbOrientation").and.\
          (lbres@lbOrientation.eq."vertical".or.\
           lbres@lbOrientation.eq."VERTICAL".or.\
           lbres@lbOrientation.eq."Vertical"))
       
           lbar_orient = "vertical"
           lbar_height = vphf
           lbar_width  = 0.15 * vpwf
           lbar_side   = "right"
           lbar_just   = "centerright"
        end if
      end if

; Generate a labelbar from the above information.
  
      lbres = True
      lbres@lbOrientation     = lbar_orient
      lbres@vpWidthF          = lbar_width
      lbres@vpHeightF         = lbar_height
      lbres@lbLabelFontHeightF= font_height
      
      if(lbar_orient.eq."vertical")
        lbar_pos = 0.04
      else
        if(lbar_zone.eq.2)
          lbar_pos = 0.06
        else
          lbar_pos = 0.0
        end if
      end if

      lbres@amZone          = lbar_zone      ; Just outside plot area
      lbres@amSide          = lbar_side      ; Labelbar on bottom.
      lbres@amParallelPosF  = 0.5            ; 0.5 is center of plot
      lbres@amJust          = lbar_just      ; Center it about the center
      lbres@amResizeNotify  = True           ; Resize labelbar if map resized.
      lbres@amOrthogonalPosF= lbar_pos       ; Move away from bottom edge

      add_labelbar(wks,contour_object,dimsizes(colors),colors,levels,lbres)

; Create data object and use coordinate variables if they exist.
    draw_and_frame(wks,map_object,calldraw,callframe)
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
lbres, xy_res, mpres, cnres, levels, colors, \
    cnres@gsnFrame  = False
map_vpwf, map_vphf, vpwf, vphf
    mpres@gsnFrame = False


    calldraw  = True
    callframe = True

    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    addcyclic      = True     ; Default is to add a cyclic point.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
    overlay(map_object,contour_object)              ; Overlay contour plot
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
      res2@sfMissingValueV = datanew@_FillValue
    end if

; Check if user setting own font heights.
;
    main_font_height = get_res_value_keep(res2,"tiMainFontHeightF", \
    if(iscoord(datanew,"lat").and.iscoord(datanew,"lon")) then
      res2@sfYArray  = datanew&lat
      res2@sfXArray  = datanew&lon
    else
      if(iscoord(datanew,"latitude").and.iscoord(datanew,"longitude")) then
        res2@sfYArray  = datanew&latitude
        res2@sfXArray  = datanew&longitude
      else
        res2@sfYCStartV =  -90.
        res2@sfYCEndV   =   90.
        res2@sfXCStartV = -180.
        res2@sfXCEndV   =  180.
      end if
      add_labelbar(wks,contour_object,lbar_zone,font_height,"polar",lbres)
    end if
  
    draw_and_frame(wks,map_object,calldraw,callframe,maxbb)
    if((res2).and.isatt(res2,"gsnZonalMean"))
      if((iscoord(datanew,"lat").and.iscoord(datanew,"lon")).or.\
         (iscoord(datanew,"latitude").and.iscoord(datanew,"longitude"))) then
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
        print("The resource gsnZonalMean can only be set to True")
        print("if the data has coordinate variables 'lat' and 'lon'")
;       a zonal mean XY plot is drawn.
;    4. If the resource "cnFillOn" is set to True, then a labelbar is   ;
;       drawn and line labels are turned off.                           ;
;    5. If data has an attribute called "long_name," and "gsnLeftString";
;       hasn't explicitly been set, then the value of this attribute    ;
;       is used for the left string title.                              ;
    res2@cnInfoLabelFont            = "helvetica"
    res2@cnLineLabelFont            = "helvetica"
    res2@cnLineLabelBackgroundColor = "transparent"
;       is used for the right string title.                             ;
;***********************************************************************;

function gsn_csm_contour_map_ce(wks:graphic,data[*][*]:numeric,\
    if(isatt(datanew,"long_name").and..not.isatt(res2,"gsnLeftString"))
      mpres@gsnLeftString = datanew@long_name
      delete(datanew@long_name)
    end if
    if(isatt(datanew,"units").and..not.isatt(res2,"gsnRightString"))
      mpres@gsnRightString = datanew@units
    end if
        datanew = data
    end if

    check_for_draw_and_frame(res2,calldraw,callframe)

    if((res2).and..not.any(ismissing(getvaratts(res2))))

; Check for left subtitle at top.

      if(isatt(res2,"gsnLeftString"))
        mpres@gsnLeftString = res2@gsnLeftString
        delete(res2@gsnLeftString)
      end if

; Check for center subtitle at top.

      if(isatt(res2,"gsnCenterString"))
        mpres@gsnCenterString = res2@gsnCenterString
        delete(res2@gsnCenterString)
      end if

; Check for right subtitle at top.
      set_attr(res2,"sfYCStartV",-90.)
      if(isatt(res2,"gsnRightString"))
        mpres@gsnRightString = res2@gsnRightString
        delete(res2@gsnRightString)
      end if
    end if
    end if
; Check if a zonal mean plot is desired.

    if(res2.and.isatt(res2,"gsnZonalMean"))
      if(any(datanew!0.eq.get_allowed_latnames()).and.\
    if((isatt(res2,"cnFillOn").and.res2@cnFillOn).or.\
       (isatt(res2,"mpFillOn").and..not.res2@mpFillOn))
        res2@mpOutlineOn = True
            map_vphf = res2@vpHeightF
          else
            map_vphf  = 0.7 ; make sure zonalplot will fit in view port
    lbres = get_res_eq(res2,"lb")
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti"/))
    cnres = get_res_ne(res2,(/"mp","vp","tm","ti","lb"/))
            set_attr(res2,     "vpXF",0.1)
            set_attr(res2,     "vpYF",0.9)
      if(isatt(cnres,"cnFillOn").and.cnres@cnFillOn)
	set_attr(cnres,"cnInfoLabelOn",False)
        if(.not.isatt(cnres,"lbLabelBarOn").or.\
          isatt(cnres,"lbLabelBarOn").and.cnres@lbLabelBarOn)
          zmeans2 = dim_avg(datanew(lat | : ,lon | :))
          if(min(zmeans2).lt.0.and.max(zmeans2).gt.0)
          end if
          delete(zmeans2)
        end if
      else
        print("gsn_csm_contour_map_ce: Warning: The resource gsnZonalMean can only be set to True")
        print("if the data has one of the coordinate variables " + get_allowed_latnames() + "and " + get_allowed_lonnames())
      end if
    if(isatt(cnres,"cnInfoLabelOn").and..not.cnres@cnInfoLabelOn)
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
    map_object = gsn_csm_map(wks,mpres)            ; Create map.
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
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    else
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    end if

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)
    if(isatt(res2,"tiMainFontHeightF"))
      main_font_height = res2@tiMainFontHeightF
    else
      main_font_height = 1.3*font_height
    end if
    if(isatt(res2,"tiXAxisFontHeightF"))
      x_font_height = res2@tiXAxisFontHeightF
    else
      x_font_height = font_height
    end if
    if(isatt(res2,"tiYAxisFontHeightF"))
      y_font_height = res2@tiYAxisFontHeightF
    else
      y_font_height = font_height
    end if
; or mpFillOn is set to False, then it is set back to True.
        "tiXAxisFontHeightF"   : x_font_height     ; X axis label size
        "tiYAxisFontHeightF"   : y_font_height     ; Y axis label size
;
    if(check_attr(res2,"cnFillOn",True,False).or.\
       check_attr(res2,"mpFillOn",False,False))
      set_attr(res2,"mpOutlineOn",True)
    end if
    lbres = get_res_eq(res2,(/"lb","pm"/))
      setvalues contour_object
        "cnLineLabelFontHeightF"    : xbfontf
      end setvalues
    if(cnres)
      if(check_attr(cnres,"cnFillOn",True,False))
        set_attr(cnres,"cnInfoLabelOn",False)
      setvalues contour_object
        "cnInfoLabelFontHeightF"    : xbfontf
      end setvalues
          if(check_attr(lbres,"lbOrientation","vertical",True))
            set_attr(mpres, "vpWidthF",0.75)   ; Make room for labelbar
; Retrieve the view port size of the map plot so we know how big
; to make labelbar and where to put zonal means plot (if there is one).

    getvalues map_object
      "vpWidthF"     : vpwf
      "vpHeightF"    : vphf
    end getvalues
            set_attr(mpres,     "vpXF",0.08)
; Create a labelbar.
            set_attr(mpres,     "vpYF",0.90)
; 
; Determine what the default labelbar width and height should be depending
; on shape of plot (this is for a horizontal labelbar).  For example, if
; the plot is square, then make the labelbar the same width as the plot.
; If the plot is 1:2, then make the labelbar .75 * width of the plot.
;
; For a vertical labelbar, make the height the same as the height of the
; plot.
;
      ratio1 = vpwf/vphf
      ratio2 = vphf/vpwf
      ratio = min((/ratio1,ratio2/))
      ratios  = (/0.50, 0.75, 1.00/)  ; viewport ratios

      lwscale   = (/0.75, 0.90, 1.00/)
      wh_ind    = ind(ratio.le.ratios)
      def_lvpwf = lwscale(wh_ind(0))    ; default width scale for labelbar
;
; Find out what levels and what colors were used for contours.
;
      getvalues contour_object
        "cnLevels"     : levels
        "cnFillColors" : colors
      end getvalues

; Generate a labelbar from the above information.
; If lbOrientation has been set to "vertical", then we have to position
; the labelbar differently.  The default is "horizontal".
;
      lbar_orient = "horizontal"
      lbar_side   = "bottom"
      lbar_height = 0.30 * vphf
      lbar_width  = def_lvpwf * vpwf
      lbar_just   = "bottomcenter"

      if(lbres) then
        if(isatt(lbres,"lbOrientation").and.\
          (lbres@lbOrientation.eq."vertical".or.\
           lbres@lbOrientation.eq."VERTICAL".or.\
           lbres@lbOrientation.eq."Vertical"))
          lbar_orient = "vertical"
          lbar_side   = "right"
          lbar_just   = "centerright"
          lbar_height = vphf
          lbar_width  = 0.20 * vpwf
        end if
      end if

      lbres = True
      lbres@lbOrientation     = lbar_orient
      lbres@vpWidthF          = lbar_width
      lbres@vpHeightF         = lbar_height
      lbres@lbLabelFontHeightF= xbfontf
      
      if(lbar_zone.eq.2)
        if(lbar_orient.eq."vertical")
          lbar_pos = 0.03
        else
          lbar_pos = 0.06
        end if
      else
        lbar_pos = 0.0
      end if

      lbres@amZone          = lbar_zone      ; Just outside plot area
      lbres@amSide          = lbar_side      ; Labelbar on bottom.
      lbres@amParallelPosF  = 0.5            ; 0.5 is center of plot
      lbres@amJust          = lbar_just      ; Center it about the center
      lbres@amResizeNotify  = True           ; Resize labelbar if map resized.
      lbres@amOrthogonalPosF= lbar_pos       ; Move away from bottom edge
; they are supposed to get drawn.
      add_labelbar(wks,contour_object,dimsizes(colors),colors,levels,lbres)
    end if
  
; Add a zonal mean plot if requested.
    if(zonal_mean_plot)

    if(zonal_mean_plot)
        infolabel_zone = zonal_zone + 1
        lbar_zone      = zonal_zone + 2
      else
        lbar_zone      = zonal_zone + 1
      end if
      if(iscoord(datanew,"lat"))
      if(infolabel_on)
        lbar_zone      = infolabel_zone + 1
          "caYArray" : datanew&lat
    end if
      else
        if(iscoord(datanew,"latitude"))
          data_object = create "data" coordArraysClass noparent
            "caXArray" : zmeans
            "caYArray" : datanew&latitude
          end create
        end if
      end if

      if(isatt(zmeans,"_FillValue")) then
      if(.not.isatt(cnres,"cnInfoLabelOrthogonalPosF").and.infolabel_zone.eq.2)
              "caXMissingV" :zmeans@_FillValue
      end if
      end if
    end if

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

    draw_and_frame(wks,map_object,calldraw,callframe)

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
    contour_object = gsn_contour(wks,datanew,cnres) ; Create contours.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
    overlay(map_object,contour_object)              ; Overlay contours on map.

local i, attnames, vector_object, map_object, \
vc_res_index, mp_res_index, calldraw, callframe, \
min_lat, max_lat, addcyclic, unew, vnew, res, res2, \
map_vpwf, map_vphf, vpwf, vphf
    getvalues map_object
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
    calldraw  = True
    callframe = True

    infolabel_zone = 2        ; Zone for info label (may change later)
    addcyclic      = True     ; Default is to add a cyclic point.

;
    res2 = resources
;
; The default is to add a cyclic point.
    if((res2).and.isatt(res2,"gsnAddCyclic"))
      if(.not.res2@gsnAddCyclic)
        addcyclic = False
      end if
      delete(res2@gsnAddCyclic)
    end if
                                     1.1*font_height)
    if(addcyclic)
      "tiMainFontHeightF"    : main_font_height  ; main title size
    end setvalues

; Set font heights only if they haven't been set explicitly by user.

    contour_plot = check_class_name(contour_object,"contour")

    if(.not.isatt(cnres,"cnLineLabelFontHeightF"))
      setvalues contour_plot
        "cnLineLabelFontHeightF"    : 0.6 * font_height
    if(iscoord(unew,"lat").and.iscoord(unew,"lon")) then
      res2@vfYArray  = unew&lat
      res2@vfXArray  = unew&lon
    else
      if(iscoord(unew,"latitude").and.iscoord(unew,"longitude")) then
        res2@vfYArray  = unew&latitude
        res2@vfXArray  = unew&longitude
      else
        res2@vfYCStartV =  -90.
        res2@vfYCEndV   =   90.
        res2@vfXCStartV = -180.
        res2@vfXCEndV   =  180.
      end if
    if(lbar_on)
      add_labelbar(wks,contour_object,lbar_zone,0.6 * font_height,"other",lbres)
; This section tests for more special resources: those that start
; with "gsn."
; Draw all this stuff: map plot, subtitles, and tick marks.
    if(isatt(unew,"long_name").and..not.isatt(res2,"gsnLeftString"))
      mpres@gsnLeftString = unew@long_name
      delete(unew@long_name)
    end if
    if(isatt(unew,"units").and..not.isatt(res2,"gsnRightString"))
      mpres@gsnRightString = unew@units
    end if

; Return plot object and data object (as attribute of plot object).

    check_for_draw_and_frame(res2,calldraw,callframe)
end
    if((res2).and..not.any(ismissing(getvaratts(res2))))
;***********************************************************************;
; Check for left subtitle at top.

      if(isatt(res2,"gsnLeftString"))
        mpres@gsnLeftString = res2@gsnLeftString
        delete(res2@gsnLeftString)
      end if

; Check for center subtitle at top.

      if(isatt(res2,"gsnCenterString"))
        mpres@gsnCenterString = res2@gsnCenterString
        delete(res2@gsnCenterString)
      end if

; Check for right subtitle at top.

      if(isatt(res2,"gsnRightString"))
        mpres@gsnRightString = res2@gsnRightString
        delete(res2@gsnRightString)
      end if
    end if
;                   wks: workstation object                             ;
; By default, mpOutlineOn is False, unless mpFillOn is set to False,
; then it is set back to True.

    if(isatt(res2,"mpFillOn").and..not.res2@mpFillOn)
      res2@mpOutlineOn = True
    end if
                                resources:logical)
local i, vector_object, map_object, res, res2, calldraw, vcres, \
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","ti"/))
    check_for_lon_coord(unew,res2,"vector")

    if(.not.(isatt(res2,"vfXArray")))
      set_attr(res2,"vfXCStartV",-180.)
      set_attr(res2,"vfXCEndV",   180.)
    end if

    if(.not.(isatt(res2,"vfYArray")))
    set_attr(vcres,"vcRefAnnoZone",infolabel_zone)
      set_attr(res2,"vfYCEndV",   90.)

; Check for left/right titles at top. Use attributes "long_name" and
; "units" if they exist.

    set_right_subtitle(unew,res2,mpres)
    set_left_subtitle(unew,res2,mpres)

; size, and the info label size the same as the tick mark label size.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

;
      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf
; Check for other special resources.
;
    if(isatt(res2,"gsnMajorLonSpacing"))
      mpres@gsnMajorLonSpacing = res2@gsnMajorLonSpacing
      delete(res2@gsnMajorLonSpacing)
    end if
    if(isatt(res2,"gsnMajorLatSpacing"))
    if(isatt(res2,"tiMainFontHeightF"))
      main_font_height = res2@tiMainFontHeightF
    else
      main_font_height = 1.3*font_height
    end if
    if(isatt(res2,"tiXAxisFontHeightF"))
      x_font_height = res2@tiXAxisFontHeightF
    else
      x_font_height = font_height
    end if
    if(isatt(res2,"tiYAxisFontHeightF"))
      y_font_height = res2@tiYAxisFontHeightF
    else
      y_font_height = font_height
    end if
    end if
        "tiXAxisFontHeightF"   : x_font_height     ; X axis label size
        "tiYAxisFontHeightF"   : y_font_height     ; Y axis label size

    if(isatt(res2,"gsnMinorLonSpacing"))
      mpres@gsnMinorLonSpacing = res2@gsnMinorLonSpacing
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",False)
    draw_and_frame(wks,map_object,calldraw,callframe)
      mpres@gsnTickMarksOn = get_res_value(res2,"gsnTickMarksOn",True)
    end if

; This section tests for regular resources.
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","tx","am","lb","pm"/))
    lbres = get_res_eq(res2,(/"lb","pm"/))

    if(vcres)
; Function : gsn_csm_vector_scalar_map_ce                               ;

    set_subtitles_res(res2,mpres)
;
;                  data: 2-dimensional scalar data                      ;
; By default, mpOutlineOn is False, unless vcMonoLineArrowColor is set to
; False or mpFillOn is set to False, then it is set back to True.
; This function is similar to gsn_csm_vector_map_ce except it colors the;
; vectors according to a scalar field.                                  ;
    mpres = get_res_eq(res2,(/"mp","vp","tm","ti","tx","am","pmA","pmO","pmT"/))
    vcres           = True
    mpres           = True
function gsn_csm_vector_scalar_map_ce(wks:graphic,u[*][*]:numeric,\
                   v[*][*]:numeric,data[*][*]:numeric,resources:logical)
local i, attnames, vector_object, map_object, vc_res_index, mp_res_index, \
calldraw, callframe, min_lat, max_lat, addcyclic, unew, vnew, datanew, \
res, res2, map_vpwf, map_vphf, vpwf, vphf, lbar_on

    set_attr(vcres,"vcRefAnnoString2On","False")

    calldraw  = True
    callframe = True

    infolabel_zone = 3        ; Zone for info label
    lbar_zone      = 2        ; Zone for labelbar
    lbar_on        = True     ; Default is to add a label bar.
    addcyclic      = True     ; Default is to add a cyclic point.
    map_object = gsn_csm_map(wks,mpres)             ; Create map.
    scalar_contour = False    ; Default is a scalar vector plot.
    res2 = resources
; Retrieve some font heights and make the X/Y axis labels the same
; The default is to add a cyclic point.
    if((res2).and.isatt(res2,"gsnAddCyclic"))
      if(.not.res2@gsnAddCyclic)
        addcyclic = False
      end if
      delete(res2@gsnAddCyclic)
    end if

    if(addcyclic)
      unew    = gsn_add_cyclic_point(u)
      vnew    = gsn_add_cyclic_point(v)
      datanew = gsn_add_cyclic_point(data)
    end getvalues
      unew    = u
      vnew    = v
      datanew = data
                                          ; the size of the X/Y axis labels.
;
; Check if user setting own font heights.
; overlay vector on map.
    main_font_height = get_res_value(res2,"tiMainFontHeightF", \
    if(iscoord(unew,"lat").and.iscoord(unew,"lon")) then
      res2@vfYArray  = unew&lat
      res2@vfXArray  = unew&lon
    else
      if(iscoord(unew,"latitude").and.iscoord(unew,"longitude")) then
        res2@vfYArray  = unew&latitude
        res2@vfXArray  = unew&longitude
      else
        res2@vfYCStartV =  -90.
        res2@vfYCEndV   =   90.
        res2@vfXCStartV = -180.
        res2@vfXCEndV   =  180.
      end if
    end if

; This section tests for special resources: those that start
; with "gsn."
    if(lbar_on)
    if(isatt(unew,"long_name").and..not.isatt(res2,"gsnLeftString"))
      mpres@gsnLeftString = unew@long_name
      delete(unew@long_name)
    end if
    if(isatt(unew,"units").and..not.isatt(res2,"gsnRightString"))
      mpres@gsnRightString = unew@units
    end if

; Draw all this stuff: map plot, subtitles, and tick marks.

    check_for_draw_and_frame(res2,calldraw,callframe)

    lbres = get_res_eq(res2,"lb")
     end setvalues
    if((res2).and..not.any(ismissing(getvaratts(res2))))
; Create a labelbar.
; Check if we're doing a scalar vector or scalar contour plot.
      else
      if(isatt(res2,"gsnScalarContour"))
        if(res2@gsnScalarContour)
          scalar_contour = True
          lbar_on        = False
        end if
        delete(res2@gsnScalarContour)
      end if
        add_labelbar(wks,vector_object@contour,lbar_zone,xbfontf, \
; Check for left subtitle at top.

      if(isatt(res2,"gsnLeftString"))
        mpres@gsnLeftString = res2@gsnLeftString
        delete(res2@gsnLeftString)
      end if

; Check for center subtitle at top.

      if(isatt(res2,"gsnCenterString"))
        mpres@gsnCenterString = res2@gsnCenterString
        delete(res2@gsnCenterString)
      end if

; Check for right subtitle at top.

      if(isatt(res2,"gsnRightString"))
        mpres@gsnRightString = res2@gsnRightString
        delete(res2@gsnRightString)
      end if
    end if
; according to the scalar field. This resource defaults to False.       ;
;***********************************************************************;
      if(iscoord(datanew,"lat").and.iscoord(datanew,"lon")) then
        res2@sfYArray  = datanew&lat
        res2@sfXArray  = datanew&lon
      else
        if(iscoord(datanew,"latitude").and.iscoord(datanew,"longitude")) then
          res2@sfYArray  = datanew&latitude
          res2@sfXArray  = datanew&longitude
        else
          res2@sfYCStartV =  -90.
          res2@sfYCEndV   =   90.
          res2@sfXCStartV = -180.
          res2@sfXCEndV   =  180.
        end if
      datanew = data
    end if

; By default, mpOutlineOn is False, unless mpFillOn is set to False,
; then it is set back to True.

    if(isatt(res2,"mpFillOn").and..not.res2@mpFillOn)
      res2@mpOutlineOn = True
; "units" if they exist.

    lbres = get_res_eq(res2,(/"lb","pm"/))

    mpres = get_res_eq(res2,(/"mp","vp","tm","ti"/))
    vcres = get_res_ne(res2,(/"mp","vp","tm","ti","lb"/))
    scalar_contour = get_res_value(res2,"gsnScalarContour",False)

; Check for subtitles at top and add to mpres if exist.

    set_subtitles_res(res2,mpres)

;
; Check for other special resources.
;
    if(scalar_contour)
      set_attr(vcres,"cnInfoLabelZone",infolabel_zone)
    if(isatt(res2,"gsnMinorLonSpacing"))
    else
      set_attr(vcres,"vcRefAnnoZone",infolabel_zone)
      set_attr(vcres,"vcRefAnnoOrthogonalPosF", 0.06)
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
      "tmXBMajorLengthF"     : xlength
      "tmXBMinorLengthF"     : xmlength
        set_attr(res2,"sfXCStartV",-180.)
        set_attr(res2,"sfXCEndV",   180.)
      end if

      if(.not.(isatt(res2,"sfYArray")))
        set_attr(res2,"sfYCStartV",-90.)
        set_attr(res2,"sfYCEndV",   90.)
      "tiXAxisFontHeightF" : xfontf
      "tiYAxisFontHeightF" : yfontf
      end if
    end if

    if(check_attr(res2,"lbLabelBarOn",False,False))
      lbar_on = False
    end if

    if(isatt(res2,"tiMainFontHeightF"))
      main_font_height = res2@tiMainFontHeightF
    else
      main_font_height = 1.3*font_height
    end if
    if(isatt(res2,"tiXAxisFontHeightF"))
      x_font_height = res2@tiXAxisFontHeightF
    else
      x_font_height = font_height
    end if
    if(isatt(res2,"tiYAxisFontHeightF"))
      y_font_height = res2@tiYAxisFontHeightF
    else
      y_font_height = font_height
    end if
      refanno_on = False
        "tiXAxisFontHeightF"   : x_font_height     ; X axis label size
        "tiYAxisFontHeightF"   : y_font_height     ; Y axis label size
    end if

; Turn off info label if labelbar is on, unless otherwise specified by user.
    vcres = get_res_ne(res2,(/"mp","vp","tm","lb","tx","am","pm"/))

; Before we create the objects, turn off draw and frame for them.
;
; Find out what levels and colors were used for contours or vectors.
;
    if(scalar_contour)
      getvalues vector_object
        "cnLevels"     : levels
        "cnFillColors" : colors
      end getvalues
    else
      getvalues vector_object
        "vcLevels"      : levels
        "vcLevelColors" : colors
      end getvalues
    end if

; Generate a labelbar from the above information.
; If lbOrientation has been set to "vertical", then we have to position
; the labelbar differently.  The default is "horizontal".
    mpres@gsnFrame = False
      lbar_orient = "horizontal"
      lbar_height = 0.20 * vphf
      lbar_width  = 0.75 * vpwf
      lbar_side   = "bottom"
      lbar_just   = "bottomcenter"

      if(lbres) then
        if(isatt(lbres,"lbOrientation").and.\
          (lbres@lbOrientation.eq."vertical".or.\
           lbres@lbOrientation.eq."VERTICAL".or.\
           lbres@lbOrientation.eq."Vertical"))
       
           lbar_orient = "vertical"
           lbar_height = vphf
           lbar_width  = 0.15 * vpwf
           lbar_side   = "right"
           lbar_just   = "centerright"
        end if
    end if

; Generate a labelbar from the above information.
  
      lbres = True
      lbres@lbOrientation     = lbar_orient
      lbres@vpWidthF          = lbar_width
      lbres@vpHeightF         = lbar_height
      lbres@lbLabelFontHeightF= font_height
      
      if(lbar_orient.eq."vertical")
        lbar_pos = 0.04
      else
        if(lbar_zone.eq.2)
          lbar_pos = 0.1
        else
          lbar_pos = 0.0
        end if
      end if

      lbres@amZone          = lbar_zone      ; Just outside plot area
      lbres@amSide          = lbar_side      ; Labelbar on bottom.
      lbres@amParallelPosF  = 0.5            ; 0.5 is center of plot
      lbres@amJust          = lbar_just      ; Center it about the center
      lbres@amResizeNotify  = True           ; Resize labelbar if map resized.
      lbres@amOrthogonalPosF= lbar_pos       ; Move away from bottom edge

      add_labelbar(wks,vector_object,dimsizes(colors),colors,levels,lbres)

    if(scalar_contour.and.infolabel_on)
      set_attr(vcres,"cnInfoLabelZone",2)
      set_attr(vcres,"cnInfoLabelOrthogonalPosF", 0.13)
    draw_and_frame(wks,map_object,calldraw,callframe)

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
        set_attr(res2,"vpYF",0.82)

; Before we create the objects, turn off draw and frame for them.
    vcres           = True
    mpres           = True
    vcres@gsnDraw   = False
; This function calls either gsn_csm_contour_map_ce or                  ;
; gsn_csm_contour_map_polar depending on if gsnPolar is set to True.    ;

    if(scalar_contour)
      vector_object = gsn_vector_contour(wks,unew,vnew,datanew,vcres)
resources:logical)
local res2
    end if
  res2     = resources

  if((res2).and.isatt(res2,"gsnPolarNH").and.res2@gsnPolarNH.or.\
                isatt(res2,"gsnPolarSH").and.res2@gsnPolarSH)
    return(gsn_csm_contour_map_polar(wks,data,res2))

    return(gsn_csm_contour_map_ce(wks,data,res2))
    font_height = 0.02 * vphf     ; Make various label sizes a function
  delete(res2)
                                  ; of the height of the viewport.
;
; Check if user setting own font heights.
;
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
local calldraw, callframe

  calldraw  = True
  callframe = True
  res1_tmp  = res1   ; Transfer attributes
  res2_tmp  = res2   ; Transfer attributes
;                   wks: workstation object                             ;
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
  check_for_draw_and_frame(res1_tmp,calldraw,callframe)
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
  if(isatt(res2_tmp,"gsnAddCyclic"))
    if(res2_tmp@gsnAddCyclic)
      datanew = gsn_add_cyclic_point(data2)
    else
      datanew = data2
    end if
    delete(res2_tmp@gsnAddCyclic)
end
    datanew = gsn_add_cyclic_point(data2)

;***********************************************************************;
  if(iscoord(datanew,"lat").and.iscoord(datanew,"lon"))
    set_attr(res2_tmp,"sfYArray",datanew&lat)
    set_attr(res2_tmp,"sfXArray",datanew&lon)
  else
    if(iscoord(datanew,"latitude").and.iscoord(datanew,"longitude"))
      set_attr(res2_tmp,"sfYArray",datanew&latitude)
      set_attr(res2_tmp,"sfXArray",datanew&longitude)
    end if
  end if
;                     u: 2-dimensional data                             ;
;                     v: 2-dimensional data                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function calls either gsn_csm_streamline_contour_map_ce,         ;
; gsn_csm_streamline_contour_map_polar, or                              ;
; gsn_csm_streamline_contour_map_other depending on the map projection  ;
      if(isatt(res1_tmp,"lbOrientation").and.\
              (res1_tmp@lbOrientation.eq."horizontal".or. \
               res1_tmp@lbOrientation.eq."Horizontal"))
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
    draw_and_frame(wks,contour1,calldraw,callframe)
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
; If gsnYRefLine is set to some value, then a reference line will be    ;
; drawn at this value. In addition, if gsnAboveYRefLineColor and/or     ;
  
  calldraw  = get_res_value(res1_tmp,"gsnDraw", True)
  callframe = get_res_value(res1_tmp,"gsnFrame",True)
  maxbb     = get_bb_res(res1_tmp)

  res1_tmp@gsnDraw  = False
    set_attr(res1_tmp,"cnInfoLabelOn",False)
  end if 

  res2_tmp@gsnDraw  = False
  res2_tmp@gsnFrame = False
local res, xy_object, res2, xfontf, yfontf, font_height, \
lft_str_object, ctr_str_object, rgt_str_object, calldraw, callframe, \
left_string, center_string, right_string, sub_zone, main_zone
  if(get_res_value(res2_tmp,"gsnAddCyclic",True))
    datanew = gsn_add_cyclic_point(data2)
  else
    calldraw  = True
    callframe = True

    sub_zone      = 2         ; Zone for subtitles
    datanew = data2
    left_string   = False     ; Default is no subtitles unless
    center_string = False     ; they are explicitly set with the
    right_string  = False     ; special "gsn" resources.
  end if

  check_for_y_lat_coord(datanew,res2_tmp,"contour")
    yref_line_above= -1       ; Fill color above Y ref line
    yref_line_below= -1       ; Fill color below Y ref line
    res2 = resources

  set_attr(res2_tmp,"cnLineLabelBackgroundColor",-1)
  if(.not.isatt(res2_tmp,"cnInfoLabelOrthogonalPosF"))
    check_for_draw_and_frame(res2,calldraw,callframe)
      else
; This function creates and draws a titled XY plot to the workstation   ;
; "wks" (the variable returned from a previous call to "gsn_open_wks"). ;
; "resources" is an optional list of  resources. The Id of the map plot ;
; is returned.                                                          ;
;                                                                       ;
; This function behaves differently from gsn_xy in that it will         ;
; add additional titles to the top of the plot if any of the special    ;
; GSUN resources "gsnLeftString," "gsnCenterString," and/or             ;
        sub_zone      = sub_zone+1  ; Zone for subtitles
; "gsnRightString" are set, They are used to title the top left, center,;
; and right of the plot (in addition, the regular resource              ;
; "tiMainString" can be set to create a regular title).                 ;
;                                                                       ;
; If gsnYRefLine is set to some value(s), then reference line(s) will be;
; drawn at this value(s). In addition, if gsnAboveYRefLineColor and/or  ;
; gsnBelowYRefLineColor is set, the polygons above and below the        ;
; reference line will be filled in these colors.                        ;

; If gsnXRefLine is set to some value, then a reference line will be    ;
callframe, left_string, center_string, right_string, main_zone, ncurves, \
          delete(yref_line_above)
    xref_line_on  = False     ; X reference line flag
    yref_line_on  = False     ; Y reference line flag
    yref_fill_on  = False     ; Whether to fill above/below Y ref line.
    center_string = False
; Check if frame and/or draw are not supposed to be called.
          delete(yref_line_below)
          yref_line_below = res2@gsnBelowYRefLineColor

          delete(res2@gsnBelowYRefLineColor)
        end if
      end if
      if(isatt(res2,"gsnShape").and.res2@gsnShape)
; Check if gsnYRefLine set.
      end if

; Check if gsnYRefLine set.
        xref_line    = res2@gsnXRefLine
        yref_line_on = True
        delete(res2@gsnXRefLine)
        nyref        = dimsizes(yref_line)
        delete(res2@gsnYRefLine)
;

;
        if(isatt(res2,"gsnYRefLineColor"))
          yref_line_color = res2@gsnYRefLineColor
          if(.not.(dimsizes(yref_line_color).eq.nyref.or.\
    end if
            print("gsn_csm_xy: Fatal: you must specify either one Y reference line color")
            print("or the same number of colors as there are reference lines.")
            return
          end if
        else
          yref_line_color = "foreground"  ; defaults to foreground color.
        end if

    if(isatt(res2,"tmXBMode").and.(res2@tmXBMode.eq."Explicit".or.\
                                   res2@tmXBMode.eq."EXPLICIT"))
; Determine color of fill above reference line(s).
;
        if(isatt(res2,"gsnAboveYRefLineColor"))
    if(isatt(res2,"tmYLMode").and.(res2@tmYLMode.eq."Explicit".or.\
                                   res2@tmYLMode.eq."EXPLICIT"))
                   dimsizes(res2@gsnAboveYRefLineColor).eq.1))
            print("gsn_csm_xy: Fatal: there must either be an above-line fill color for every reference line, or one global above-line fill color specified")
            return
; Calculate number of X and Y points.

    ndimsy = dimsizes(dimsizes(y))
    ndimsx = dimsizes(dimsizes(x))

    if(ndimsy.eq.1)
      nlines = 1
      nptsy  = dimsizes(y)
    else
      nlines = dimsizes(y(:,0))
      nptsy  = dimsizes(y(0,:))
    end if

; Check if we want fill above or below Y reference value.
          yref_fill_on    = True
    if(yref_fill_on)

          yref_line_above = -1  ; defaults to no fill.
        end if
;

      npts         = new((/nlines/),integer)
                   dimsizes(res2@gsnBelowYRefLineColor).eq.1))
        xmsg = x@_FillValue
      else
        xmsg = 1.e36
            return
          yref_line_below = get_res_value(res2,"gsnBelowYRefLineColor",1)
        ymsg = y@_FillValue
      else
        ymsg = 1.e36
        else
      xinterp      = new((/nlines,2*nptsy/),float,xmsg)
      yinterp      = new((/nlines,2*nptsy/),float,ymsg)
      npts = ref_line_interp(x,y,xinterp,yinterp,yref_line)
;
      xinterp = x
      yinterp = y
; For each old y point, we need two new y points to represent
; the horizontal line.
;
          nyb = 2 * (npts-1) + 1


      if(dimsizes(xref_line).eq.1)
        xref = new((/nlines/),float)
        xref = fspan(xref_line,xref_line,nlines)
      else
        xref = xref_line
      end if
          yinterp = new((/ncurves,nyb/),double,1e36)
          xinterp = new((/ncurves,nyb/),double,1e36)

          if(isatt(x,"_FillValue"))
            xinterp@_FillValue = x@_FillValue

      if(dimsizes(yref_line).eq.1)
        yref = new((/nlines/),float)
        yref = fspan(yref_line,yref_line,nlines)
      else
        yref = yref_line
      end if
;
; Get bar widths.  They are either set by user via a resource, or 
; from the smallest dx.

    xy_object = gsn_xy(wks,xinterp,yinterp,res2)
          xbelow = new((/ncurves,5*npts/),double,1e36)
          ybelow = new((/ncurves,5*npts/),double,1e36)
;
    if(yref_fill_on)
      do i=0,nlines-1
        fill_xy(wks,xy_object,xinterp(i,0:npts(i)-1),yinterp(i,0:npts(i)-1),\
                yref(i),yref_line_above,yref_line_below)

            if(na_max.gt.0)
              xinterp(0:2*ncurves-1:2,0:na_max-1) = xabove(:,0:na_max-1)
    res2@gsnXYPlot = True
    res2@gsnXAxis = True

    delete(res2@gsnXYPlot)
      gsres@gsLineColor = "black"

      do i=0,nlines-1
        gsn_polyline(wks,xy_object,fspan(xref(i),xref(i),2),\
                     (/min(yinterp),max(yinterp)/),gsres)


      do i=0,max((/ncurves,nyref/))-1
        if(dimsizes(yref_line_above).gt.1)
          fill_above = yref_line_above(i)
        end if
                                     xinterp(i,0:new_npts(i)-1), \
      gsres@gsLineColor = "black"

      do i=0,nlines-1
        gsn_polyline(wks,xy_object,(/min(xinterp),max(xinterp)/), \
                     fspan(yref(i),yref(i),2),gsres)

          end if
          var_string = systemfunc("echo polygons$$")
          xy_object@$var_string$ = gsn_add_polygon(wks,xy_object, \
                                                   xabove(i,:), \
                                                   yabove(i,:),gsres)
        end if
;
; Fill below reference line.
;
        gsres@gsFillColor = yref_line_below(0)

        if(dimsizes(yref_line_below).gt.1)
          gsres@gsFillColor = yref_line_below(i)
        end if

        if(nbelow(i).gt.0)
          var_string = systemfunc("echo polygons$$")
          xy_object@$var_string$ = gsn_add_polygon(wks,xy_object, \
                                                   xbelow(i,:), \
                                                   ybelow(i,:), gsres)
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
    setvalues xy_object
      "tmXBMajorLengthF"        : major_length
      "tmXBMinorLengthF"        : minor_length
      "tmYLMajorLengthF"        : major_length
      "tmYLMinorLengthF"        : minor_length
      "tmXBMajorOutwardLengthF" : major_length
      "tmXBMinorOutwardLengthF" : minor_length
      "tmYLMajorOutwardLengthF" : major_length
      "tmYLMinorOutwardLengthF" : minor_length
      "tmXTMajorLengthF"        : major_length
      "tmXTMinorLengthF"        : minor_length
      "tmYRMajorLengthF"        : major_length
      "tmYRMinorLengthF"        : minor_length
      "tmXTMajorOutwardLengthF" : major_length
      "tmXTMinorOutwardLengthF" : minor_length
      "tmYRMajorOutwardLengthF" : major_length
      "tmYRMinorOutwardLengthF" : minor_length
    end setvalues
      end getvalues


    subres@txFontHeightF = font_height

      gsres@gsLineColor = xref_line_color(0)

      do i=0,nxref-1
    draw_and_frame(wks,xy_object,calldraw,callframe)
          gsres@gsLineColor = xref_line_color(i)
        end if
        xline = fspan(xref_line(i),xref_line(i),2)
        var_string = systemfunc("echo xpolyline$$") + i
        xy_object@$var_string$ = gsn_add_polyline(wks,xy_object,xline,yline, \
      "tmXBMajorLengthF"     : xlength

;***********************************************************************;
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
local res, contour_object, res2, xfontf, yfontf, font_height, \
lft_str_object, ctr_str_object, rgt_str_object, calldraw, callframe, \
left_string, center_string, right_string, sub_zone, main_zone, datanew
    subres = get_res_eq(res2,(/"tx","am"/))  ; Get textitem resources
    subres               = True
    set_attr(subres,"txFontHeightF",font_height)
    calldraw  = True
    callframe = True

    left_string   = False     ; Default is no subtitles unless
    center_string = False     ; they are explicitly set with the
    right_string  = False     ; special "gsn" resources.
    add_subtitles(wks,xy_object,left_string,center_string,right_string, \
    sub_zone      = 2         ; Zone for subtitles
                  subres)
    addcyclic     = False     ; Don't add a cyclic point.
    res2 = resources
; Draw all this stuff: XY plot and subtitles.
    draw_and_frame(wks,xy_object,calldraw,callframe,maxbb)

; Return XY plot object.

    if((res2).and.isatt(res2,"gsnAddCyclic"))
        if(res2@gsnAddCyclic)
            addcyclic = True
        end if
        delete(res2@gsnAddCyclic)
    end if
    return(xy_object)
    if(addcyclic)

;***********************************************************************;
; Function : gsn_csm_y                                                  ;
;                   wks: workstation object                             ;

;             resources: optional resources                             ;
    check_for_draw_and_frame(res2,calldraw,callframe)
  else
; Check for left title. If the data has an attribute called "long_name",
; then use this string for the left title.

    if(isatt(datanew,"long_name").and..not.isatt(res2,"gsnLeftString"))
      res2 = True
      res2@gsnLeftString = datanew@long_name
      delete(datanew@long_name)
    end if
      exit
; Check for right title at top. If the data has an attribute called "units",
; then use this string for the right title.
  end if
    if(isatt(datanew,"units").and..not.isatt(res2,"gsnRightString"))
      res2 = True
      res2@gsnRightString = datanew@units
    end if
; 
; Check for existence of the left, center, and right subtitles.
  x@long_name = ""

; Call gsn_csm_xy.
;
  xy = gsn_csm_xy(wks,x,y,res2)
  return(xy)
    if((res2).and..not.any(ismissing(getvaratts(res2))))

end

      if(isatt(res2,"gsnShape").and.res2@gsnShape)
        sub_zone      = sub_zone+1  ; Zone for subtitles
        main_zone     = main_zone+1 ; Zone for main title
      end if
; Function : gsn_csm_contour                                            ;
;                     wks: workstation object                           ;
;                    data: data to contour                              ;
;               resources: optional resources                           ;
    if(.not.ismissing(datanew!1).and.iscoord(data,datanew!1))
      set_attr(res2,"sfXArray",datanew&$datanew!1$)
    end if

    if(.not.ismissing(datanew!0).and.iscoord(data,datanew!0))
      set_attr(res2,"sfYArray",datanew&$datanew!0$)
    end if

; Check to see if X and/or Y axis labels should be set.

    if(isatt(res2,"tmXBMode").and.(res2@tmXBMode.eq."Explicit".or.\
                                   res2@tmXBMode.eq."EXPLICIT"))
      set_attr(res2,"tiXAxisString","")
    else
      if(isatt(datanew&$datanew!1$,"long_name"))
        set_attr(res2,"tiXAxisString",datanew&$datanew!1$@long_name)
      end if
    end if

    if(isatt(res2,"tmYLMode").and.(res2@tmYLMode.eq."Explicit".or.\
                                   res2@tmYLMode.eq."EXPLICIT"))
      set_attr(res2,"tiYAxisString","")
    else
      if(isatt(datanew&$datanew!0$,"long_name"))
        set_attr(res2,"tiYAxisString",datanew&$datanew!0$@long_name)
      end if
    end if

; Check for a missing value.

    if(isatt(datanew,"_FillValue")) then
      res2@sfMissingValueV = datanew@_FillValue
    end if

; Set some more contour plot resources.

    res2@cnInfoLabelFont            = "helvetica"
    res2@cnLineLabelFont            = "helvetica"

; This function creates and draws a titled contour plot to the          ;
; workstation  "wks" (the variable returned from a previous call to     ;
; "gsn_open_wks"). "resources" is an optional list of  resources. The   ;
; Id of the contour plot is returned.                                   ;
    if((isatt(res2,"cnFillOn").and.res2@cnFillOn).and. \
; This function behaves differently from gsn_contour in that it will    ;
        (isatt(res2,"lbLabelBarOn").and.res2@lbLabelBarOn)))
  
; GSUN resources "gsnLeftString," "gsnCenterString," and/or             ;
      lbar_zone = 3
      if(isatt(res2,"lbOrientation").and.\
        (res2@lbOrientation.eq."vertical".or.\
         res2@lbOrientation.eq."VERTICAL".or.\
         res2@lbOrientation.eq."Vertical"))
        lbar_side   = "right"
        lbar_orient = "vertical"
        lbar_just   = "centerright"
        lbar_orth   = 0.0
        delete(res2@lbOrientation)
      else
        lbar_just   = "bottomcenter"
        lbar_side   = "bottom"
        lbar_orth   = 0.02
        lbar_orient = "horizontal"
      end if
      res2@cnLineLabelsOn         = False    ; Turn off line labels
      res2@cnInfoLabelOn          = False    ; Turn off info label
; "tiMainString" can be set to create a regular title).                 ;
;                                                                       ;
; Tick marks will be made to point outwards.                            ;
;                                                                       ;
;***********************************************************************;

function gsn_csm_contour(wks:graphic,data[*][*]:numeric,resources:logical)
    contour_object = gsn_contour(wks,datanew,res2)
main_zone, datanew, popgrid, contour_plot
begin

    getvalues contour_object
      "vpWidthF"             : width
      "vpHeightF"            : height

      "cnLevels"     : levels        ; Need this for creating
      "cnFillColors" : colors        ; a labelbar.

      "tiXAxisFontHeightF"   : xfontf
      "tiYAxisFontHeightF"   : yfontf

      "tmYLMajorLengthF"     : ylength
      "tmXBMajorLengthF"     : xlength
      "tmYLMinorLengthF"     : ymlength
      "tmXBMinorLengthF"     : xmlength
    end getvalues
    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    maxbb     = get_bb_res(res2)
; 
; Check if a POP grid is to be overlaid.
    major_length = min((/ylength,xlength/))    ; New length for major ticks.
    minor_length = min((/ymlength,xmlength/))  ; New length for minor ticks.

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

    major_length = scale * major_length
    minor_length = scale * minor_length
    set_right_subtitle(datanew,res2,res2)
    left_string   = new(1,logical)
    check_for_subtitles(res2,left_string,center_string,right_string)
    setvalues contour_object
      "tmXBMajorLengthF"        : major_length
      "tmXBMinorLengthF"        : minor_length
      "tmYLMajorLengthF"        : major_length
      "tmYLMinorLengthF"        : minor_length
      "tmXBMajorOutwardLengthF" : major_length
      "tmXBMinorOutwardLengthF" : minor_length
      "tmYLMajorOutwardLengthF" : major_length
      "tmYLMinorOutwardLengthF" : minor_length
      "tmXTMajorLengthF"        : major_length
      "tmXTMinorLengthF"        : minor_length
      "tmYRMajorLengthF"        : major_length
      "tmYRMinorLengthF"        : minor_length
      "tmXTMajorOutwardLengthF" : major_length
      "tmXTMinorOutwardLengthF" : minor_length
      "tmYRMajorOutwardLengthF" : major_length
      "tmYRMinorOutwardLengthF" : minor_length
    end setvalues
; Check if gsnShape set.

    if(check_attr(res2,"gsnShape",True,False))
      main_zone     = main_zone+1 ; Zone for main title
      if (lbar_orient.eq."vertical")
        lbar_width =  0.2 * width       ; width of labelbar
        lbar_height = height            ; height of labelbar
      else
        lbar_width =  width
        lbar_height = 0.2 * height
      end if

      lbres = True
      lbres@lbLabelFontHeightF = font_height
      lbres@lbOrientation      = lbar_orient
      lbres@vpHeightF          = lbar_height
      lbres@vpWidthF           = lbar_width

      lbres@amZone          = lbar_zone      ; Just outside plot area
      lbres@amSide          = lbar_side      ; Labelbar location
      lbres@amParallelPosF  = 0.5            ; 0.5 is center of plot
      lbres@amOrthogonalPosF= lbar_orth
      lbres@amJust          = lbar_just      ; Center it about the center
      lbres@amResizeNotify  = True           ; Resize labelbar if map resized.

      lbres = get_res_eq(res2,"lb") ; Check for labelbar resources.
      lbres = True

      add_labelbar(wks,contour_object,dimsizes(colors),colors,levels,lbres)
; Use coordinate variables for X and/or Y if they exist.

    check_for_coord_arrays(datanew,res2,"contour")

; Turn on a labelbar if fill is True and if the labelbar is not
    subres@txFontHeightF    = font_height
    add_subtitles(wks,contour_object,left_string,center_string,right_string, \
                  subres)
       (.not.isatt(res2,"lbLabelBarOn").or.\
        .not.check_attr(res2,"lbLabelBarOn",False,False)))
    draw_and_frame(wks,contour_object,calldraw,callframe)
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
; This function creates and draws a titled Hovmoller plot to the        ;
    cnres = get_res_ne(res2,(/"tx"/))
    contour_object = gsn_contour(wks,datanew,cnres)

; Get some information from contour plot that was created.

    contour_plot = check_class_name(contour_object,"contour")

; Add lat/lon labels to X/Y axes if appropriate coordinate arrays exist.

function gsn_csm_hov(wks:graphic,data:numeric,resources:logical)
local res, contour_object, res2, font_height, lbar_on, \
lft_str_object, ctr_str_object, rgt_str_object, calldraw, callframe, \
left_string, center_string, right_string, sub_zone, main_zone, datanew
      "vpWidthF"             : width
      "vpHeightF"            : height
  
    calldraw  = True
    callframe = True

    res2 = resources
      "tiYAxisFontHeightF"   : yfontf
    check_for_draw_and_frame(res2,calldraw,callframe)

    if((res2).and..not.any(ismissing(getvaratts(res2))))
    font_height = min((/xfontf,yfontf/))  ; Make subtitle label sizes a
                                          ; function of the size of the
                                          ; X/Y axis labels.

; If the plot is close to square in size, then make the 
    if(.not.ismissing(data!1).and.iscoord(data,data!1))
      set_attr(res2,"sfXArray",data&$data!1$)
    ratios     = (/0.5,0.75,1.0/)

    ratio = height/width
    if(ratio.gt.1) 
      ratio = 1./ratio
    end if
    index = ind(ratio.le.ratios)

    scale = font_scale(index(0))
    font_height  = scale * font_height
    if(.not.ismissing(data!1).and.iscoord(data,data!1).and.data!1.eq."lon")
    if(popgrid.ne."") then
      pop_latlon_grid(wks,contour_object,popgrid)  ; Add a POP grid.
    else
      getvalues contour_object
        "trXMinF"   : min_lon
        "trXMaxF"   : max_lon
      end getvalues

; Add longitude labels to X axis.

       add_lon_labels(contour_object,min_lon,max_lon)
    if (lbar_on)
      lbres = get_res_eq(res2,(/"lb","pm"/))
      add_labelbar(wks,contour_object,3,font_height,"contour",lbres)
    draw_and_frame(wks,contour_object,calldraw,callframe)
  
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
function gsn_csm_lat_time(wks:graphic,data:numeric,resources:logical)
local res, contour_object, res2, font_height, lbar_on, \
lft_str_object, ctr_str_object, rgt_str_object, calldraw, callframe, \
left_string, center_string, right_string, sub_zone, main_zone, datanew
; This function behaves differently from gsn_csm_contour in that it     ;
; draws a specially-labelled longitude X axis.                          ;
;                                                                       ;
    calldraw  = True
    callframe = True

    res2 = resources

    check_for_draw_and_frame(res2,calldraw,callframe)
; Initialize.
    res2         = get_resources(resources)

    calldraw     = get_res_value(res2,"gsnDraw", True)
    callframe    = get_res_value(res2,"gsnFrame",True)
    maxbb        = get_bb_res(res2)
    lon_spacing  = get_res_value(res2,"gsnMajorLonSpacing",0)
    if(iscoord(data,"lat"))
      set_attr(res2,"sfYArray",data&lat)
    else
      if(iscoord(data,"latitude"))
        set_attr(res2,"sfYArray",data&latitude)
      end if
    end if

    if(is_valid_lat_xcoord(data).or.is_valid_lon_xcoord(data))
      set_attr(res2,"tiXAxisString","")
    end if

; Use coordinate variable for X if it exists.

    if(is_valid_xcoord(data).and..not.(isatt(res2,"sfXCStartV").and. \
       isatt(res2,"sfXCEndV")))
    getvalues contour_object
      "trYMinF"   : min_lat
      "trYMaxF"   : max_lat
    end getvalues
; Use coordinate variable for Y if it exists.
; Add latitude labels to Y axis.

    add_lat_labels_yaxis(contour_object,min_lat,max_lat)


    draw_and_frame(wks,contour_object,calldraw,callframe)

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
function gsn_csm_pres_hgt(wks:graphic,data:numeric,resources:logical)
local res, contour_object, res2, xfontf, yfontf, font_height, lbar_on, \
lft_str_object, ctr_str_object, rgt_str_object, calldraw, callframe, \
left_string, center_string, right_string, sub_zone, main_zone, add_hgt
;***********************************************************************;
; Function : gsn_csm_time_lat                                           ;
;                     wks: workstation object                           ;
    calldraw      = True
    callframe     = True
    left_string   = False
    center_string = False
    right_string  = False
    log_y         = True  ; Default is to log the Y axis, unless trYLog = False
;                    data: data to be contoured                         ;
    pres_reverse =  True  ; Default is to assume pressure values
                          ; already reversed.
    res2 = resources
; workstation "wks" (the variable returned from a previous call to      ;
    check_for_draw_and_frame(res2,calldraw,callframe)

    if(isatt(data&$data!0$,"long_name"))

function gsn_csm_time_lat(wks:graphic,data[*][*]:numeric,resources:logical)
    if(.not.ismissing(data!0).and.iscoord(data,data!0).and.data!0.eq."lev")
    res2         = get_resources(resources)
    lat_spacing  = get_res_value(res2,"gsnMajorLatSpacing",15)
        pres = data&$data!0$(::-1)  ; reverse the values
        pres_reverse =  False       ; pres values weren't already reversed
    if((res2).and..not.any(ismissing(getvaratts(res2))))
        pres = data&$data!0$
    end if
      if(isatt(data&$data!0$,"units").and.(data&$data!0$@units.eq."Pa".or.\
                                           data&$data!0$@units.eq."Pascals"))
        pres = data&$data!0$ * 0.01  ; convert to mb
        pres@units = "mb"
    end getvalues

    add_lat_labels_xaxis(contour_plot,min_lat,max_lat,lat_spacing,\
                         mlat_spacing,res2)
      print("gsn_csm_pres_hgt: The first dimension of the input data must")
; Draw all this stuff: contour plot and subtitles.
      exit
; Return contour plot object.

    if(isatt(res2,"trYLog"))
      if(.not.res2@trYLog)
        log_y = False       ; Check if Y axis to stay linear.
      end if
      delete(res2@trYLog)
    end if

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
    getvalues contour_object
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
; This function behaves differently from gsn_csm_contour in that it     ;
    res2          = get_resources(resources)

    setvalues contour_object
      "tmYLLabelFontHeightF" : yaxis_label_height * 0.8
      "tmYRLabelFontHeightF" : yaxis_label_height * 0.8
      "tmXBLabelFontHeightF" : yaxis_label_height * 0.8
    end setvalues

    if(is_valid_ycoord(data).and.isatt(data&$data!0$,"long_name"))
      set_attr(res2,"tiYAxisString",data&$data!0$@long_name)
    if(.not.ismissing(data!1).and.iscoord(data,data!1).and.data!1.eq."lat")
       add_lat_labels_xaxis(contour_object,xmin,xmax)
; If this is a pressure/hgt plot, then do stuff to the pressure values.

;
; Add a right Y axis label.
; Check for pressure values.  If they are not in descending order, then
; reverse 'em.
;
      if(data&$data!0$(0).lt.data&$data!0$(npres-1))
        pres = 1.*data&$data!0$(::-1)  ; reverse the values
        pres_reverse =  False          ; pres values weren't already reversed
       else
        pres = 1.*data&$data!0$
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
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if
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
    contour_plot = check_class_name(contour_object,"contour")

    getvalues contour_plot
      "vpXF"                 : xvp
      "vpYF"                 : yvp
      "vpWidthF"             : widthvp
    draw_and_frame(wks,loglin_object,calldraw,callframe)
      "trXMinF"              : xmin
      "trXMaxF"              : xmax
      "trYMinF"              : ymin
      "trYMaxF"              : ymax
      "tiYAxisFontHeightF"   : yaxis_font_height
      "tmYLLabelFontHeightF" : yaxis_label_height
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

