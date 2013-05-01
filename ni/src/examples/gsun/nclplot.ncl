;
;      $Id$
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  2011                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       ncl_plot.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Mon Aug 15 10:52:29 MDT 2011
;;
;;  Description: This script is meant to be a "quick look" plotting
;;               utility, where you give it a filename and optionally
;;               a list of variables, and it will create plots of them.
;; 

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

;----------------------------------------------------------------------
; Return list of allowed names for a level dimemsion.
;----------------------------------------------------------------------
undef("get_allowed_level_names")
function get_allowed_level_names()
begin
  return((/"level","LEVEL","Level","lev","LEV","Lev",\
           "levels","LEVELS","Levels","bottom_top","bottom_top_stag"/))
end

;----------------------------------------------------------------------
; This function checks if the given variable has a time dimension.
; If so, it returns the dimension number (0...n-1). If not, it
; returns -1.
;----------------------------------------------------------------------
undef("get_time_dimension")
function get_time_dimension(f1,vname)
local dim_names, valid_time_names, nnames, i, ii
begin
  dim_names        = str_lower(getfilevardims(f1,vname))
  valid_time_names = (/"time","times"/)
  if(.not.any(ismissing(dim_names))) then
    ndims = dimsizes(dim_names)
    do i=0,ndims-1
      ii = ind(dim_names(i).eq.valid_time_names)
      if(.not.any(ismissing(ii))) then
        return(i)
      end if
      delete(ii)
    end do
  end if
  return(-1)
end


;----------------------------------------------------------------------
; This function checks if the given variable has a level dimension.
; If so, it returns the dimension number (0...n-1). If not, it
; returns -1.
;----------------------------------------------------------------------
undef("get_level_dimension")
function get_level_dimension(f1,vname)
local dim_names, valid_level_names, ndims, i, ii
begin
  dim_names         = getfilevardims(f1,vname)
  valid_level_names = get_allowed_level_names()
  if(.not.any(ismissing(dim_names))) then
    ndims = dimsizes(dim_names)
    do i=0,ndims-1
      ii = ind(dim_names(i).eq.valid_level_names)
      if(.not.any(ismissing(ii))) then
        return(i)
      end if
      delete(ii)
    end do
  end if
  return(-1)
end

;----------------------------------------------------------------------
; This function returns a 2D subset of an nD variable.
; It only checks for up to 6 dimensions.
; The assumption is that the dimensions are 
;   ... x [time] x [lev] x lat x lon
;----------------------------------------------------------------------
undef("get_2d_var")
function get_2d_var(f1:file,vname[1]:string,time,level)
local dsizes, rank, time2, level2, tdim, ldim, dim0, dim1, dim2
begin
  time2  = time    ; make a local copy
  level2 = time    ; make a local copy

  dsizes = getfilevardimsizes(f1,vname)
  rank   = dimsizes(dsizes)
  if(rank.lt.2) then
    print("get_2d_var: Fatal: '" + vname + "' only has " + \
           rank + " dimensions.")
    exit
  end if

;---Just in case we need to access the level and/or time dimensions
  tdim = get_time_dimension(f1,vname)
  ldim = get_level_dimension(f1,vname)
 
;---Simple error checking
  if(tdim.ne.-1.and.(time2.lt.0.or.time2.ge.dsizes(tdim))) then
    print("get_2d_var: Fatal: invalid time index specified.")
    exit
  end if

  if(ldim.ne.-1.and.(level2.lt.0.or.level2.ge.dsizes(ldim))) then
    print("get_2d_var: Fatal: invalid level index specified.")
    exit
  end if
;
; We can handle up to five dimensions. We assume the rightmost two
; dimensions are lat x lon, so try to guess at the remaining leftmost
; dimensions.
;
  if(tdim.eq.0) then
    dim0 = time2
  else if(ldim.eq.0) then
    dim0 = level2
  else
    dim0 = 0
  end if
  end if

  if(tdim.eq.1) then
    dim1 = time2
  else if(ldim.eq.1) then
    dim1 = level2
  else
    dim1 = 0
  end if
  end if

  if(tdim.eq.2) then
    dim2 = time2
  else if(ldim.eq.2) then
    dim2 = level2
  else
    dim2 = 0
  end if
  end if

  if(rank.eq.2) then
    x = f1->$vname$
  else if(rank.eq.3) then
    x = f1->$vname$(dim0,:,:)
  else if(rank.eq.4) then
    x = f1->$vname$(dim0,dim1,:,:)
  else if(rank.eq.5) then
    x = f1->$vname$(dim0,dim1,dim2,:,:)
  end if
  end if
  end if
  end if
  return(x)
end

;----------------------------------------------------------------------
; This function returns an x11 window
;----------------------------------------------------------------------
undef("open_wks")
function open_wks()
begin
  return(gsn_open_wks("x11","nclplot"))
end

;----------------------------------------------------------------------
; This function checks if a variable is a lat variable by
; checking the "units" attribute, if it exists.
;----------------------------------------------------------------------
undef("is_lat_var")
function is_lat_var(f1,vname)
local vatts
begin
  vatts = getfilevaratts(f1,vname)
  if(.not.any(ismissing(vatts)).and.any(vatts.eq."units").and.\
     any(f1->$vname$@units.eq.get_allowed_lat_units)) then
    return(True)
  else
    return(False)
  end if
end

;----------------------------------------------------------------------
; This function checks if a variable is a lon variable by
; checking the "units" attribute, if it exists.
;----------------------------------------------------------------------
undef("is_lon_var")
function is_lon_var(f1,vname)
local vatts
begin
  vatts = getfilevaratts(f1,vname)
  if(.not.any(ismissing(vatts)).and.any(vatts.eq."units").and.\
     any(f1->$vname$@units.eq.get_allowed_lon_units)) then
    return(True)
  else
    return(False)
  end if
end

;----------------------------------------------------------------------
; This function checks if a variable is a lat/lon variable.
;----------------------------------------------------------------------
undef("is_latlon_var")
function is_latlon_var(f1,vname)
begin
  if(is_lat_var(f1,vname).or.is_lon_var(f1,vname)) then
    return(True)
  else
    return(False)
  end if
end

;----------------------------------------------------------------------
; This procedure draws a contour plot or contour over map plot
;----------------------------------------------------------------------
undef("contour_plot")
procedure contour_plot(f:file,wks:graphic,data:numeric,cnres:logical)
local res, plot, vardims
begin
  vardims            = getvardims(data)
  res                = cnres     ; Make a copy
  res@gsnMaximize    = True
  res@cnFillOn       = True
  res@cnLinesOn      = False
  res@cnLineLabelsOn = False

;---1D coordinate arrays
  if(is_valid_latlon_coord(data,"x","lon",False).and.\
     is_valid_latlon_coord(data,"y","lat",False)) then
    res@mpMinLatF = min(data&$data!0$)
    res@mpMaxLatF = max(data&$data!0$)
    res@mpMinLonF = min(data&$data!1$)
    res@mpMaxLonF = max(data&$data!1$)
    res@mpCenterLonF = (res@mpMinLatF+res@mpMaxLonF)/2.

;---Determine cyclicness
    mx         = dimsizes(data&$data!1$)
    first_val  = data&$data!1$(0)
    second_val = data&$data!1$(1)
    last_val   = data&$data!1$(mx-1)
    delta      = second_val - first_val
    range      = last_val - first_val
    if(range.ge.360.) then
      res@gsnAddCyclic = False
    end if
    if(any((range+delta).lt.359.)) then
      res@gsnAddCyclic = False
    end if

    plot = gsn_csm_contour_map(wks,data,res)
;---Special lat2d/lon2d attributes
  else if (is_valid_latlon2d_attr(data,"lat2d").and. \
           is_valid_latlon2d_attr(data,"lon2d")) then
    res@mpMinLatF = min(data@lat2d)
    res@mpMaxLatF = max(data@lat2d)
    res@mpMinLonF = min(data@lon2d)
    res@mpMaxLonF = max(data@lon2d)
    res@mpCenterLonF = (res@mpMinLatF+res@mpMaxLonF)/2.
    res@gsnAddCyclic = False    ; For now, make this False always
;---Check for a WRF file.
  else if(.not.any(ismissing(vardims)).and.\
          any(vardims(0).eq.(/"south_north","south_north_stag"/)).and.\
          any(vardims(1).eq.(/"west_east","west_east_stag"/))) then
    res = wrf_map_resources(f,res) 
    res@tfDoNDCOverlay = get_res_value(res,"tfDoNDCOverlay",True)
    res@gsnAddCyclic   = False    ; For now, make this False always
    delete([/res@start_lat,res@start_lon,res@end_lat,res@end_lon,res@mpNestTime/])
    plot = gsn_csm_contour_map(wks,data,res)
;---"data" contains "coordinates" attribute
  else if(isatt(data,"coordinates")) then
    coord_names = str_split(data@coordinates," ")
    print(coord_names)
    do i=0,dimsizes(coord_names)-1
      if(isfilevar(f,coord_names(i))) then
         if(is_lat_var(f,coord_names(i))) then
           dims = dimsizes(f->$coord_names(i)$)
           if(dimsizes(dims).eq.2) then
             res@sfYArray          = f->$coord_names(i)$
           else if(dimsizes(dims).eq.3) then
             res@sfYArray          = f->$coord_names(i)$(0,:,:)
           end if
           end if
           res@gsnAddCyclic = False
           res@mpMinLatF    = min(res@sfYArray)
           res@mpMaxLatF    = max(res@sfYArray)
           delete(dims)
         end if
         if(is_lon_var(f,coord_names(i))) then
           dims = dimsizes(f->$coord_names(i)$)
           if(dimsizes(dims).eq.2) then
             res@sfXArray          = f->$coord_names(i)$
           else if(dimsizes(dims).eq.3) then
             res@sfXArray          = f->$coord_names(i)$(0,:,:)
           end if
           end if
           res@gsnAddCyclic = False
           res@mpMinLonF    = min(res@sfXArray)
           res@mpMaxLonF    = max(res@sfXArray)
           delete(dims)
         end if
      end if
    end do
    plot = gsn_csm_contour_map(wks,data,res)
  else
    plot = gsn_csm_contour(wks,data,res)
  end if
  end if
  end if
  end if
end

;----------------------------------------------------------------------
; This function checks a variable to see if it's
; one we can create a contour plot for.
;----------------------------------------------------------------------
undef("is_valid_var_to_contour")
function is_valid_var_to_contour(f1,vname,vartype)
local rank, valid_num_types
begin
;---Set valid types for plotting
  valid_num_types = (/"float","double","byte","ubyte","short","ushort",\
                      "integer","uint","long","ulong","int64","uint64"/)
  rank  = dimsizes(getfilevardimsizes(f1,vname))
  if(rank.ge.2.and.rank.le.5.and.any(vartype.eq.valid_num_types).and.\
     .not.is_latlon_var(f1,vname)) then
    return(True)
  end if
  return(False)
end

;----------------------------------------------------------------------
; Main code
;----------------------------------------------------------------------
begin
  dq = str_get_dq()

;----------------------------------------------------------------------
; Check for command line arguments.
;    filename - required
;    varname  - not required (default: all numeric variables)
;    varnames - not required (default: all numeric variables)
;    time     - not required (default: 0)
;    level    - not required (default: 0)
;    debug    - set True for debug output (default: False)
;----------------------------------------------------------------------
  if(.not.isvar("filename")) then
    print("Error: must input a filename.")
    print("       ncl 'filename=" + dq + "name_of_file" + dq + "' nclplot.ncl")
    exit
  else
;---Error checking
    if(.not.isfilepresent(filename)) then
      print("Error: '" + filename + "' either doesn't exist or is an invalid file.")
      exit
    end if
;---Open file
    f = addfile(filename,"r")
  end if

  if(isvar("varname")) then
    varnames = varname
  else if(.not.isvar("varnames")) then
    varnames = getfilevarnames(f)
    print("Warning: no variable name given. All numeric variables will be plotted.")
  end if
  end if
  nvars = dimsizes(varnames)
  
  if(.not.isvar("time")) then
    time = 0
  end if

  if(.not.isvar("level")) then
    level = 0
  end if

  if(.not.isvar("debug")) then
    DEBUG = False
  end if

  if(.not.isvar("raster")) then
    RASTER = False
  end if

;---Print some debug info
  if(DEBUG) then
    print("Filename: '" + filename + "'")
    print("time:      " + time)
    print("level:     " + level)
    print("debug:     " + DEBUG)
    print("Potential # of variables to plot: " + nvars)
    print("varnames:  " + varnames)
  end if

;---Open workstation to send graphics to.
  wks = open_wks()

;
; Loop through each variable and plot it if it meets
; certain qualifications.
;
  cnres = True
  if(RASTER) then
    cnres@cnFillMode = "RasterFill"
  end if
  vtypes = getfilevartypes(f,varnames)
  do i=0,nvars-1
    if(is_valid_var_to_contour(f,varnames(i),vtypes(i))) then
      print("Plotting variable '" + varnames(i) + "'")
      x2d = get_2d_var(f,varnames(i),time,level)
      if(dimsizes(dimsizes(x2d)).eq.2) then
        if(DEBUG) then
          print(str_join("" + dimsizes(x2d),","))
        end if
        cnres@tiMainString = varnames(i)
        contour_plot(f,wks,x2d,cnres)
      else
        print("Skipping variable '" + varnames(i) + "'")
        delete(x2d)
        continue
      end if
;---Clean up before next time through loop.
     delete(x2d)
    else
      print("Skipping variable '" + varnames(i) + "'")
      continue
    end if 
  end do
end
