;----------------------------------------------------------------------
; This script contains a bunch of "utility" functions, both private
; and public. It is loaded automatically by NCL.
;
; Some example public functions include:
;   totype
;   read_colormap_file
;   span_color_indexes
;----------------------------------------------------------------------

;----------------------------------------------------------------------
; This function converts input variable x to type specified by type.
; Wei Huang
; May 21, 2012
;----------------------------------------------------------------------
undef("totype")
function totype( varin, type:string )
local varout

begin
    ;printVarSummary(varin)
    ;print(type)

     ;Convert to float
     if(type .eq. "float") then
         varout = tofloat(varin)
         return(varout)
     end if

     ;Convert to double
     if(type .eq. "double") then
         varout = todouble(varin)
         return(varout)
     end if

     ;Convert to uint
     if(type .eq. "uint") then
         varout = touint(varin)
         return(varout)
     end if

    ;Convert to integer
     if(type .eq. "int" .or. type .eq. "integer") then
         varout = toint(varin)
         return(varout)
     end if

     ;Convert to char
     if(type .eq. "char" .or. type .eq. "character") then
         varout = tochar(varin)
         return(varout)
     end if

     ;Convert to byte
     if(type .eq. "byte") then
         varout = tobyte(varin)
         return(varout)
     end if

     ;Convert to short
     if(type .eq. "short") then
         varout = toshort(varin)
         return(varout)
     end if

     ;Convert to ushort
     if(type .eq. "ushort") then
         varout = toushort(varin)
         return(varout)
     end if

     ;Convert to long
     if(type .eq. "long") then
         varout = tolong(varin)
         return(varout)
     end if

     ;Convert to ulong
     if(type .eq. "ulong") then
         varout = toulong(varin)
         return(varout)
     end if

     ;Convert to int64
     if(type .eq. "int64") then
         varout = toint64(varin)
         return(varout)
     end if

     ;Convert to uint64
     if(type .eq. "uint64") then
         varout = touint64(varin)
         return(varout)
     end if

     ;Convert to string
     if(type .eq. "string") then
         varout = tostring(varin)
         return(varout)
     end if

     print("totype: warning: cannot convert input variable type <" + \
           typeof(varin) + "> to type: <" + type + ">")
     print("   The original type: <" + typeof(varin) + "> will be returned.")

     varout = varin
     return(varout)
end

;***********************************************************************;
; Function : lower_case                                                 ;
;           name : string                                               ;
;                                                                       ;
; Converts "name" to lowercase. This is an old alias for str_lower.     ;
;                                                                       ;
; Keep this for backwards compatibility purposes.  This function is not ;
; advertised, but users may still be using it.                          ;
;***********************************************************************;
undef("lower_case")
function lower_case(name:string)
begin
  return(str_lower(name))
end


;***********************************************************************;
; Function : enum_list                                                  ;
;                                                                       ;
; This function returns a list of predefined resource values and their  ;
; their equivalent enumerated integers as defined in various include    ;
; files in the C code.  This is a temporary solution until an internal  ;
; function is created that will automatically return these values.      ;
;***********************************************************************;
undef("enum_list")
function enum_list()
begin
  return(str_lower((/\ 
             (/"BoxCenters","0"/),\
             (/"InteriorEdges","1"/),\
             (/"ExternalEdges","2"/),\
             (/"IncludeOuterBoxes","0"/),\
             (/"IncludeMinMaxLabels","1"/),\
             (/"ExcludeOuterBoxes","2"/),\
             (/"NoCreate","-1"/),\
             (/"Never","0"/),\
             (/"Always","1"/),\
             (/"Conditional","2"/),\
             (/"ForceAlways","3"/),\
             (/"Horizontal","0"/),\
             (/"Vertical","1"/),\
             (/"PreDraw","0"/),\
             (/"Draw","1"/),\
             (/"PostDraw","2"/),\
             (/"RectangleEnds","0"/),\
             (/"TriangleLowEnd","1"/),\
             (/"TriangleHighEnd","2"/),\
             (/"TriangleBothEnds","3"/),\
             (/"Automatic","0"/),\
             (/"Manual","1"/),\
             (/"Explicit","2"/),\
             (/"AreaFill","0"/),\
             (/"RasterFill","1"/),\
             (/"CellFill","2"/),\
             (/"MeshFill","3"/),\
             (/"Top","0"/),\
             (/"Bottom","1"/),\
             (/"Right","2"/),\
             (/"Left","3"/),\
             (/"Center","4"/),\
             (/"AutomaticLevels","0"/),\
             (/"ManualLevels","1"/),\
             (/"ExplicitLevels","2"/),\
             (/"EqualSpacedLevels","3"/),\
             (/"LineArrow","0"/),\
             (/"FillArrow","1"/),\
             (/"WindBarb","2"/),\
             (/"CurlyVector","3"/),\
             (/"Map","0"/),\
             (/"LogLin","1"/),\
             (/"Irregular","2"/),\
             (/"Curvilinear","3"/),\
             (/"Spherical","4"/),\
             (/"TriangularMesh","5"/),\
             (/"MaximalArea","0"/),\
             (/"Latlon","1"/),\
             (/"Angles","2"/),\
             (/"NPC","3"/),\
             (/"NDC","4"/),\
             (/"Corners","5"/),\
             (/"Points","6"/),\
             (/"Window","7"/),\
             (/"Orthographic","0"/),\
             (/"Stereographic","1"/),\
             (/"LambertEqualArea","2"/),\
             (/"Gnomonic","3"/),\
             (/"AzimuthalEquidistant","4"/),\
             (/"Satellite","5"/),\
             (/"PseudoMollweide","6"/),\
             (/"Mercator","7"/),\
             (/"CylindricalEquidistant","8"/),\
             (/"LambertConformal","9"/),\
             (/"Robinson","20"/),\
             (/"CylindricalEqualArea","11"/),\
             (/"RotatedMercator","12"/),\
             (/"Aitoff","13"/),\
             (/"Hammer","14"/),\
             (/"Mollweide","15"/),\
             (/"WinkelTripel","16"/)\
             /)))
end

;***********************************************************************;
; Function : get_enum_value(enum_string[*]:string)                      ;
;                                                                       ;
; This function returns the enum value for the given predefined         ;
; resource vaule(s). This is useful if you are doing a getvalues on a   ;
; resource that is normally set with a predefined string, like:         ;
;                                                                       ;
;    res@lbLabelAlignment = "BoxCenters"                                ;
;                                                                       ;
; Since getvalues always returns the ENUM value and not the string      ;
; value, this function returns the enum value so you can check both the ;
; string and integer values of a resource. This function is used by the ;
; check_attr_enum_value function.                                       ;
;***********************************************************************;
undef("get_enum_value")
function get_enum_value(enum_strings[*]:string)
local values, nvals, ii, n
begin
  enum_values = enum_list()
  nvals   = dimsizes(enum_strings)
  ret_val = new(nvals,integer)
  do n=0,nvals-1
     ii := ind(str_lower(enum_strings(n)).eq.enum_values(:,0))
     if(.not.any(ismissing(ii))) then
       ret_val(n) = toint(enum_values(ii,1))
     else
       ret_val(n) = ret_val@_FillValue
     end if
  end do
  return(ret_val)
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
undef("check_attr")
function check_attr(res:logical,att_name:string,att_value, \
                    convert_lower:logical)
local res2, new_att_value, is_att_value_string, is_att_name_string
begin
  res2 = res
  if(res2.and.isatt(res2,att_name))
    if(typeof(att_value).eq."string") then
      is_att_value_string = True
    else
      is_att_value_string = False
    end if

    if(typeof(res@$att_name$).eq."string") then
      is_att_name_string = True
     else
      is_att_name_string = False
    end if
;
; If one value is a string and the other isn't, then we can't
; compare them, and we just have to return False.

    if(is_att_name_string.ne.is_att_value_string) then
      return(False)
    end if

    if(is_att_value_string.and.convert_lower)
      new_att_value   = str_lower(att_value)
      res2@$att_name$ = str_lower(res2@$att_name$)
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
; Function : check_attr_enum_value                                      ;
;                res     : logical                                      ;
;           att_name[1]  : string                                       ;
;          att_str_value : string                                       ;
;                                                                       ;
; This function is similar to check_attr, except it allows you to check ;
; if a resource is set, and if so, it can check both its predefined     ;
; string value and its enumerated value. For example, "lbOrientation"   ;
; can be set to "horizontal" or "vertical", or the equivalent integer   ;
; values, 0 or 1.                                                       ;
;***********************************************************************;
undef("check_attr_enum_value")
function check_attr_enum_value(res:logical,att_name[1]:string,att_str_value:string)
local att_num_value
begin
  if(.not.isatt(res,att_name).or.typeof(res@$att_name$).ne."string".and.\
                                 typeof(res@$att_name$).ne."integer") then
    return(False)
  end if
  if(typeof(res@$att_name$).eq."string") then
    return(check_attr(res,att_name,att_str_value,True))
  else
    att_num_value = get_enum_value(att_str_value)
    if(any(ismissing(att_num_value))) then
      print("check_attr_enum_value: warning: invalid predefined resource value: '" + att_str_value + "'.")
      return(False)
    else
      return(check_attr(res,att_name,att_num_value,False))
    end if
  end if
end

;***********************************************************************;
; Function : get_res_value                                              ;
;                res                                                    ;
;            resname[*]:string                                          ;
;        default_val                                                    ;
;                                                                       ;
; This function checks to see if any of the given resources have been   ;
; set, and if so, it returns its value and removes it from the resource ;
; list.                                                                 ;
;                                                                       ;
; Note: this function was updated in V6.3.1 to handle "resname" being   ;
; an array of strings.  This function will use whatever value is the    ;
; first one found in the list, but it will remove *all* resnames from   ;
; the resource list.
;                                                                       ;
; Otherwise, it returns the default value which is the last argument    ;
; passed in.                                                            ;
;***********************************************************************;
undef("get_res_value")
function get_res_value(res,resname[*]:string,default_val)
local return_val,nres,n,is_set
begin
  nres   = dimsizes(resname)
  is_set = False
  if(((typeof(res).eq."logical".and.res).or.(typeof(res).ne."logical")).and.\
     .not.any(ismissing(getvaratts(res)))) then
    do n=0,nres-1
      if(isatt(res,resname(n))) then
        if(.not.is_set) then 
          is_set     = True 
          return_val = res@$resname(n)$
        end if
        delete(res@$resname(n)$)
      end if
    end do
  end if
  if(.not.is_set) then
    return_val = default_val
  end if
  return(return_val)
end

;***********************************************************************;
; Function : get_res_value_keep                                         ;
;                res:logical                                            ;
;            resname[*]:string                                          ;
;        default_val                                                    ;
;                                                                       ;
; This function checks to see if any of the given resources have been   ;
; set, and if so, it returns its value and keeps it in the resource     ;
; list.                                                                 ;
;                                                                       ;
; Note: this function was updated in V6.3.1 to handle "resname" being   ;
; an array of strings.  This function will use whatever value is the    ;
; first one found in the list.                                          ;
;                                                                       ;
; Otherwise, it returns the default value which is the last argument    ;
; passed in.                                                            ;
;                                                                       ;
;***********************************************************************;
undef("get_res_value_keep")
function get_res_value_keep(res,resname[*]:string,default_val)
local return_val, nres, is_set
begin
  nres   = dimsizes(resname)
  is_set = False
  if(((typeof(res).eq."logical".and.res).or.(typeof(res).ne."logical")).and.\
     .not.any(ismissing(getvaratts(res)))) then
    do n=0,nres-1
      if(isatt(res,resname(n))) then
        if(.not.is_set) then 
          is_set     = True 
          return_val = res@$resname(n)$
          break
        end if 
      end if 
    end do
  end if
  if(.not.is_set) then
    return_val = default_val
  end if
  return(return_val)
end

;***********************************************************************
; is 'x' a scalar quantity: here 'scalar' means rank=1 & size=1
;***********************************************************************
undef("isscalar")
function isscalar(x)
local dimx, rankx
begin
  dimx  = dimsizes(x)
  rankx = dimsizes(dimx)
  if (rankx.eq.1 .and. dimx.eq.1) then
      return(True)
  else
      return(False)
  end if 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; is_string_numeric
; Carl Schreck (cjschrec@ncsu.edu)
; March 2016
; Added to NCL in V6.4.0; enhanced to handle missing values.
;
; Based on discussions with Carl, we decided to go with having
; this routine return missing values if the input was missing,
; instead of returning False.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

undef ("is_string_numeric")
function is_string_numeric( i_string : string )
local okChars, retVal, charArray, sDims, i, j, sDims_1D, istring_1D, retVal_1D
begin
  okChars = (/ "+", "-", ".", "0", "1", "2", "3", "4", "5", "6", "7", "8",  \
      "9", "E", "e" /)

  sDims = dimsizes(i_string)
  sRank = dimsizes(sDims)

  if(sRank.eq.1) then
    retVal = new( sDims, logical )
    do i = 0, sDims-1
      if(ismissing(i_string(i))) then
        continue
      end if
      charArray := stringtocharacter( i_string(i) )
      retVal(i) = True
      j = 0
      do while( retVal(i) .and. (j .le. (dimsizes(charArray)-2)))
        retVal(i) = any( charArray(j).eq.okChars )
        j = j + 1
      end do
    end do
    return(retVal)
  else
;
; Special case to handle multiple dimension arrays.
; This is done so we are not unnecessarily making a 
; copy of a 1D string array; strings in NCL can be
; expensive.
;
    i_string_1D = ndtooned(i_string)
    sDims_1D    = dimsizes(i_string_1D)
    retVal_1D   = new( sDims_1D, logical)
    do i = 0, sDims_1D-1
      if(ismissing(i_string_1D(i))) then
        continue
      end if
      charArray := stringtocharacter( i_string_1D(i) )
      retVal_1D(i) = True
      j = 0
      do while( retVal_1D(i) .and. (j .le. (dimsizes(charArray)-2)))
        retVal_1D(i) = any( charArray(j).eq.okChars )
        j = j + 1
      end do
    end do
    return(reshape(retVal_1D,sDims))
  end if
end ; is_string_numeric

;----------------------------------------------------------------------
; This function takes an array of anything and depending on "opt", 
; it does one of two things:
;
;   - Returns the unique values as a 1D array (opt=0)
;   - Returns the number of unique values     (opt=1)
;
; Some notes:
;  - THIS FUNCTION IS NOT INTENDED TO BE MADE PUBLIC.
;    See "get_unique_values" and "count_unique_values" below.
;
; - We did some timing tests and confirmed that sorting the values
;   first made for a faster algorithm, including with string arrays.
;
; - We decided to create a separate "count_unique_values" function,
;   instead of making the user do "dimsizes(get_unique_values(x))"
;   because the count function is less memory intensive.
;----------------------------------------------------------------------
undef("unique_values_opt")
function unique_values_opt(vals,opt)
local vals1d, i, ii, vals_nomsg, nvals, nuniq_vals, count_only
begin
  if(opt.eq.0) then
    count_only = False
  else if(opt.eq.1) then
    count_only = True
  else
    print("unique_values_opt: Error: Don't recognize opt=" + opt)
    print("This is an internal function and may change")
    exit
  end if
  end if

  if(isatt(vals,"_FillValue")) then
    vals1d = ndtooned(vals)
    ii     = ind(.not.ismissing(vals1d))  
    if(ismissing(ii(0))) then
; Input array is all missing
      if(count_only) then
        return(0)
      else
        return(new(1,typeof(vals))) 
      end if
    end if
    vals_nomsg = vals1d(ii)
    delete(vals_nomsg@_FillValue) ; This is important, otherwise a _FillValue 
                                  ; will get added to return value.
    delete([/vals1d,ii/])       
  else
    vals_nomsg = ndtooned(vals)
  end if

;---Sort the array first
  if(typeof(vals_nomsg).eq."string") then
    sqsort(vals_nomsg)
  else
    qsort(vals_nomsg)
  end if

  nvals = dimsizes(vals_nomsg)

;---This is the array to be returned, if returning unique values.
  if(.not.count_only) then
    vals_uniq    = new(nvals,typeof(vals_nomsg),"No_FillValue")
    vals_uniq(0) = vals_nomsg(0)   ; The first unique value
  end if
;
; Doing two different loops here, so we don't have an extra 
; "if" test inside the do loop.
;
  nuniq_vals  = 1
  if(count_only) then
    do i=1,nvals-1
      if(vals_nomsg(i).eq.vals_nomsg(i-1)) then
        continue
      end if
      nuniq_vals = nuniq_vals+1
    end do
    return(nuniq_vals)
  else
    do i=1,nvals-1
      if(vals_nomsg(i).eq.vals_nomsg(i-1)) then
        continue
      end if
      vals_uniq(nuniq_vals) = vals_nomsg(i)
      nuniq_vals            = nuniq_vals+1
    end do
    return(vals_uniq(0:nuniq_vals-1))
  end if
end

;----------------------------------------------------------------------
; This function takes an array of anything and returns the 
; unique values as a 1D array.
;----------------------------------------------------------------------
undef("get_unique_values")
function get_unique_values(vals)
begin
  return(unique_values_opt(vals,0))
end

;----------------------------------------------------------------------
; This function takes an array of anything and returns the 
; # of unique values.
;----------------------------------------------------------------------
undef("count_unique_values")
function count_unique_values(vals)
begin
  return(unique_values_opt(vals,1))
end

;----------------------------------------------------------------------
; This function takes an array of anything and returns the 
; # of unique values for the given dimension.
;----------------------------------------------------------------------
undef("count_unique_values_n")
function count_unique_values_n(vals,n[1])
local dims, rank, imsg, irgt, ilft, vals_1d, return_count_1d, \
      return_dims, size_rgt_dims, size_n_dim, size_lft_dims
begin
  dims = dimsizes(vals)
  rank = dimsizes(dims)
  imsg = new(1,integer)

  vals_1d = ndtooned(vals)

  if(n.lt.0.or.n.ge.rank) then
    print("count_unique_values_n: Error: invalid dimension specified, " +n)
    return(imsg)
  end if
  if(rank.eq.1) then
    return(unique_values_opt(vals,1))
  end if

;---Return array will have one fewer dimensions than input array
  return_dims  = new(rank-1,typeof(dims))

  size_n_dim     = dims(n)     ; Size of dimension that is being counted
  size_rgt_dims  = 1           ; Dimensions to right of "n" dimension
  size_lft_dims  = 1           ; Dimensions to left of "n" dimension
  do ilft=0,n-1
    return_dims(ilft) = dims(ilft)
    size_lft_dims     = size_lft_dims * dims(ilft)
  end do
  do irgt=n+1,rank-1
    return_dims(irgt-1) = dims(irgt)
    size_rgt_dims       = size_rgt_dims * dims(irgt)
  end do
  size_rgt_n_dims    = size_rgt_dims * size_n_dim
  size_rgt_n_m1_dims = size_rgt_dims * (size_n_dim-1)

  return_count_1d = new(product(return_dims),long,"No_FillValue")

  icount = 0
;---Check if "n" is a middle dimension
  if(rank.ge.3.and.n.gt.0.and.n.lt.(rank-1)) then
    do ilft=0,size_lft_dims-1
      lindex = size_rgt_n_dims * ilft
      do irgt=0,size_rgt_dims-1
        istrt = lindex+irgt
        iend  = istrt + size_rgt_dims * (size_n_dim-1)
        istp  = size_rgt_dims
        vals_subset = vals_1d(istrt:iend:istp)
        return_count_1d(icount) = count_unique_values(vals_subset)
        icount = icount + 1
      end do
    end do
;---Check if "n" is the leftmost dimension
  else if(n.eq.0) then
    do irgt=0,size_rgt_dims-1
      istrt = irgt
      iend  = istrt + size_rgt_n_m1_dims
      istp  = size_rgt_dims
      vals_subset = vals_1d(istrt:iend:istp)
      return_count_1d(icount) = count_unique_values(vals_subset)
      icount = icount + 1
    end do
;---Check if "n" is the rightmost dimension
  else   ; n.eq.(rank-1)
    do ilft=0,size_lft_dims-1
      istrt = ilft*size_n_dim
      iend  = istrt+size_n_dim-1
      vals_subset = vals_1d(istrt:iend)
      return_count_1d(icount) = count_unique_values(vals_subset)
      icount = icount + 1
    end do
  end if
  end if
  return(reshape(return_count_1d,return_dims))
end

;=============
undef("cla_sq")
function cla_sq(sleft[1]:string, sright[1]:string)
; Simple utility used for creating Command Line Assignments (CLAs) within a script
; 
; Return a string with a single quote (sq) at the
; beginning and end which enclose *nix sensitive characters.
;
; [1]  s  = cla_sq("PATH","./FIM.G5test.nc")  => 'PATH="./FIM.G5test.nc"'
;
; [2]
; asgn1 = cla_sq(  "f", "test.nc")             ; asgn1= 'f="test.nc"'
; asgn2 = cla_sq(  "p", "(/850,500,200/)")     ; asgn2= 'p=(/850,500,200/)'
; asgn3 = cla_sq("var", "(/"T","Q"/)"          ; asgn3= 'var=(/"T","Q"/)'
; cmd   = "ncl year=2015 "+asgn1+" "+asgn2+" "+asgn3+" foo.ncl"
; system(cmd)
;---
local sq
begin
   sq    = str_get_sq()      ; single quote character
   return(sq+ sleft +"="+sright +sq)
end

;--------------------------------------------------------------------------------
; Many of the color-based functions were moved from gsn_code.ncl to this
; script for the V6.3.1 release.
;--------------------------------------------------------------------------------


;***********************************************************************;
; Function : read_colormap_file                                         ;
;               colorMapName : either the name of an NCL-standard       ;
;                              colormap, or the filename of a           ;
;                              user-supplied colormap.                  ;
;                                                                       ;
; This function either reads an NCL-standard colormap, given is name,   ;
; or expects to read a colormap from a given file.  It supports reading ;
; either RGB-tuples or RGBA-tuples (or a mixture); it always returns a  ;
; colormap comprised of RGBA-tuples.                                    ;
;                                                                       ;
; This function was moved to utilities.ncl (from gsn_code.ncl) to make  ;
; it more accessible (say by functions in contributed.ncl).             ;
;***********************************************************************;
undef("read_colormap_file")
function read_colormap_file(colorMapName:string)
local pathname, lines, tokens, cmap, tmpCmap, i, numColors, \
      red, green, blue, alpha, maxValue, MAXCOLORS
begin
  MAXCOLORS = 256     ; symbolic constant, used below

  ; ----------------------------------------------------------
  ; Inner convenience function to test string as suitable for 
  ; conversion to numeric.
  undef("isNumerical")
  function isNumerical(s:string)
  local seenDecimal, charS, len, i
  begin
    seenDecimal = False
    charS = stringtocharacter(s)
    len = strlen(s)
    do i=0, len-1
      if (charS(i).eq.".") then
        if (seenDecimal) then
          return False
        else
          seenDecimal = True
        end if
      else
        if (charS(i).lt."0" .or. charS(i).gt."9") then
          return False
        end if
      end if
    end do
    return True
  end

  ; ------------------------------------------------------------
  ; Inner convenience function to find appropriate pathname for 
  ; the given filename.
  undef("getFilePath")
  function getFilePath(colorMapName:string)
  local suffixes, paths, path1, path2, i, j, tmp
  begin

    ; Is this one of our standard named colormaps? There are several well-defined
    ; locations and suffixes to try...
    tmp = getenv("NCARG_COLORMAPS")
    if (.not.ismissing(tmp)) then
        paths = str_split(tmp, ":")
    else 
        paths = (/ ncargpath("ncarg") + "/colormaps" /)
    end if

    suffixes = (/ ".rgb", ".gp", ".ncmap" /)

    ; loop over the product of possible paths and possible suffixes...
    do i=0, dimsizes(paths)-1
        path1 = paths(i) + "/" + colorMapName
        do j=0, dimsizes(suffixes)-1
            path2 = path1 + suffixes(j)
            if (fileexists(path2)) then
              return path2
            end if
          end do
    end do

    ; if still here, just return colorMapName literally; presumably is a 
    ; filename for a user-managed colormap...
    return colorMapName
  end

  ; get an appropriate pathname for the given colortable name and load it..
  pathname = getFilePath(colorMapName)
  lines = asciiread(pathname, -1, "string")
  lines = str_squeeze(lines)

  ; parse upto MAXCOLORS rgba tuples from the file just read...
  tmpCmap = new((/ MAXCOLORS, 4 /), "float")
  numColors = 0
  maxValue = -1.0
  i = 0
  do while (i.lt.dimsizes(lines) .and. numColors.lt.MAXCOLORS)
      if (ismissing(strlen(lines(i))) .or. strlen(lines(i)).eq.0) then  
          lines(i) = "#"  ; zero-lengthed lines cause us grief...
      end if 
      tokens = str_split(lines(i), " ,")
      if (dimsizes(tokens).ge.3) then
          red = -1.0
          green = -1.0
          blue = -1.0
          if (isNumerical(tokens(0))) then
              red = stringtofloat(tokens(0))
          end if
          if (isNumerical(tokens(1))) then
              green = stringtofloat(tokens(1))
          end if
          if (isNumerical(tokens(2))) then
              blue = stringtofloat(tokens(2))
          end if
          if (dimsizes(tokens).gt.3 .and. isNumerical(tokens(3))) then
              alpha = stringtofloat(tokens(3))
          else
              alpha = -1.0  ; used a marker, replaced appropriately below...
          end if

          ; were we able to get a rgba-tuple?
          ;
          if (red.ge.0 .and. green.ge.0 .and. blue.ge.0) then
              ; yes, add it to our colormap...
              tmpCmap(numColors,0) = red
              tmpCmap(numColors,1) = green
              tmpCmap(numColors,2) = blue
              tmpCmap(numColors,3) = alpha
              numColors = numColors + 1
              ; keep track of the magnitude of these values; used to rescale below...
              if (red.gt.maxValue) then
                  maxValue = red
              end if
              if (green.gt.maxValue) then
                  maxValue = green
              end if
              if (blue.gt.maxValue) then
                  maxValue = blue
              end if
          end if
      end if
      i = i + 1
      delete(tokens)
  end do

  ; copy tmpCmap into appropriately sized array
  cmap = new((/numColors, 4/), float)
  cmap = tmpCmap(0:numColors-1,:)

  ; normalize the values...(oh for true if-elseif!)
  ; this logical taken directly from HLU code in "Palette.c"
  if (maxValue.le.1) then
      cmap(:,3) = where(cmap(:,3).lt.0, 1., cmap(:,3))
  else if (maxValue.lt.256) then
      cmap(:,3) = where(cmap(:,3).lt.0, 255., cmap(:,3))
      cmap = cmap / 255.
  else if (maxValue.eq.256) then
      cmap(:,3) = where(cmap(:,3).lt.0, 256., cmap(:,3))
      cmap = cmap / 256.
  else if (maxValue.eq.65536) then
      cmap(:,3) = where(cmap(:,3).lt.0, 65535., cmap(:,3))
      cmap = cmap / 65535. 
  else if (maxValue.eq.65536) then
      cmap(:,3) = where(cmap(:,3).lt.0, 65536., cmap(:,3))
      cmap = cmap / 65536.
  else
      cmap(:,3) = where(cmap(:,3).lt.0, maxValue, cmap(:,3))
      cmap = cmap / maxValue
  end if
  end if 
  end if
  end if
  end if
        
  return cmap
end

;**********************************************************************
; I had to create a "non-metadata" version of rm_single_dims_no_meta 
; because rm_single_dims references copy_VarAtts which is only in 
; contributed.ncl and not seen by this script. Eventually we'll want to 
; reorder some of these 'utility' functions so they are more visible
; by other routines.
;
; Do NOT make this function public. It should be renamed first and
; potentially refactored so that "rm_single_dims" uses this routine.
;**********************************************************************
undef("rm_single_dims_no_meta")
function rm_single_dims_no_meta(x)
; Remove singleton (degenerate) dimensions. A singleton dimension is size 1.
local dimx, dimy, rankx 
begin
  dimx  = dimsizes(x)

  if (.not.any(dimx.eq.1)) then
      return(x)                     ; no degenerate dimensions
  end if

  rankx = dimsizes(dimx)

  if (all(dimx.eq.1)) then    
      dimy = 1                      ; all degenerate dimensions
  else
      dimy = dimx(ind(dimx.ne.1))
  end if
  y     = onedtond(ndtooned(x), dimy)

  return(y)
end

;***********************************************************************;
; This function converts an nD array to a 2D array. For example:        ;
;   - 4 x 5 x 3 becomes 20 x 3                                          ;
;   - 10 x 20 x 30 x 40 becomes 6000 x 40                               ;
;                                                                       ;
; Special case: a 1D array is converted to a 1 x N array.               ;
;                                                                       ;
; This function is useful if you need to treat all the leftmost         ;
; dimensions as a single dimension.                                     ;
;***********************************************************************;
undef("ndtotwod")
function ndtotwod(x)
local dims, rank, left_dims, right_dim, dims2d
begin
  dims = dimsizes(x)
  rank = dimsizes(dims)
  if(rank.eq.1) then
    return(reshape(x,(/1,dims/)))
  end if
  if(rank.eq.2) then
    return(x)
  end if

  left_dims = dims(0:rank-2)
  right_dim = dims(rank-1)
  dims2d    = (/product(left_dims),right_dim/) 
  return(reshape(x,dims2d))  
end

;***********************************************************************;
; Function : read_colormap_files                                        ;
;               colorMapNames : an array of names of an NCL-standard    ;
;                              colormap, or the filenames of            ;
;                              user-supplied colormaps.                 ;
;                                                                       ;
; This function behaves exactly like read_colormap_file, except it      ;
; handles an array of color maps rather than just a single color map.   ;
;***********************************************************************;
undef("read_colormap_files")
function read_colormap_files(colorMapNames[*]:string)
local ncmaps, ncolors, i, cmap, nc
begin
  ncmaps  = dimsizes(colorMapNames)
  ncolors = new(ncmaps,integer)

  do i=0,ncmaps-1
    cmap := read_colormap_file(colorMapNames(i))
    if(any(ismissing(cmap))) then
      print("read_colormap_files: Error: invalid color map name")
      return(new((/1,4/),float))
    end if
    ncolors(i) = dimsizes(cmap(:,0))
  end do

  rgba_array = new((/sum(ncolors),4/),float)
  nc = 0
  do i=0,ncmaps-1
   rgba_array(nc:nc+ncolors(i)-1,:) = read_colormap_file(colorMapNames(i))
   nc = nc + ncolors(i)
  end do

  return(rgba_array)
end

;***********************************************************************;
; Given a color map and the number of desired colors, this function 
; returns an array of color indexes that nicely span the full colormap.
;
; For a named colormap, the first two color values are not used,
; because these are the foreground/background colors.
;
; This function is very similar to the span_color_rgba function,
; which returns RGBA values. 
;
; The colormap can be a named colormap, like "rainbow", or an array
; of RGB (n,3) or RGBA (n,4).
;***********************************************************************
undef("span_color_indexes")
function span_color_indexes(cmapt,ncolors)
local ncols, fmin, fmax, fcols, icols, cmap
begin
  if(isstring(cmapt)) then
     cmap = read_colormap_file(cmapt)
  else if(isnumeric(cmapt)) then
    dims = dimsizes(cmapt)
   if(dimsizes(dims).ne.2.or.dims(0).lt.3.or.dims(0).gt.256.or.\
       .not.any(dims(1).ne.(/3,4/))) then
      print ("Error: span_color_indexes: cmap must be an n x 3 or n x 4 array of RGB or RGBA values, or a valid color map name")
      return(new(1,integer))   ; return missing
    end if
    cmap = cmapt
  else
    print ("Error: span_color_indexes: cmap must be an n x 3 or n x 4 array of RGB or RGBA values, or a valid color map name")
  end if
  end if

  ncols  = dimsizes(cmap(:,0))
;
; Start at index 0 and end at ncols-1 (the full range of the
; color map.
;
  minix = 0
  maxix = ncols-1

  fmin = new(1,float)    ; to make sure we get a missing value (?)
  fmax = new(1,float)
  fmin = minix
  fmax = maxix
  fcols = fspan(fmin,fmax,ncolors)
  icols = tointeger(fcols + 0.5)
  if(isstring(cmapt)) then
    return(icols+2)
  else
    return(icols)
  end if
end

;***********************************************************************;
; Given a color map and the number of desired colors, this function 
; returns an array of RGB[A] values that nicely span the full colormap.
;
; For a named colormap, the first two color values are not used,
; because these are the foreground/background colors.
;
; This function is very similar to the span_color_indexes function,
; except it returns RGBA values rather than index values. This 
; function actually uses span_color_indexes.
;
; The colormap can be a named colormap, like "rainbow", or an array
; of RGB (n,3) or RGBA (n,4).
;***********************************************************************
undef("span_color_rgba")
function span_color_rgba(cmapt,ncolors)
local icols, cmap, fmsg
begin
  icols = span_color_indexes(cmapt,ncolors)
  fmsg = new(4,float)          ; missing value
  if(any(ismissing(icols)))
    return(fmsg)
  end if

 if(isstring(cmapt)) then
   cmap = read_colormap_file(cmapt)
   icols = icols - 2                 ; read_colormap_file returns array 
                                     ; with indexes 0 and 1 dropped off
 else 
   cmap = cmapt
 end if

 return(cmap(icols,:))
end

;***********************************************************************
; Given an array of contour levels,  a color map, and a single
; value, this function returns an index value into the colormap
; to use for representing the single value
;
; This function is very similar to the get_color_rgb function,
; except it returns the index value into the color map, rather
; than an RGBA value.
;
; The colormap can be a named colormap, like "rainbow", or an array
; of RGB (n,3) or RGBA (n,4).
;
; This function replaces the deprecated GetFillColor.
;***********************************************************************
undef("get_color_index")
function get_color_index(cmapt,cnlvls[*]:numeric,value[1]:numeric)
local cmap, dims, ncn, nclr, color, n, col_indexes, ncoli
begin

 if(isstring(cmapt)) then
    cmap = read_colormap_file(cmapt)
 else if(isnumeric(cmapt)) then
   dims = dimsizes(cmapt)
   if(dimsizes(dims).ne.2.or.dims(0).lt.3.or.dims(0).gt.256.or.\
       .not.any(dims(1).ne.(/3,4/))) then
     print ("get_color_index: Error: cmap must be an n x 3 or n x 4 array of RGB or RGBA values, or a valid color map name")
     return(new(3,"float"))    ; return a missing value
   end if
   cmap = cmapt
 else
   print ("get_color_index: Error: cmap must be an n x 3 or n x 4 array of RGB or RGBA values, or a valid color map name")
 end if
 end if

 ncn  = dimsizes (cnlvls)
 nclr = dimsizes (cmap(:,0))
 imsg = new(1,integer)          ; missing value

 if (nclr .lt. ncn+1) then 
   print ("get_color_index: Warning: Not enough colors in colormap for number of requested levels")
   print ("         Colors will be repeated")
 end if
 if (ismissing(value)) then
   print ("get_color_index: Error: Input value is missing")
   return (imsg)
 end if
 if (any(ismissing(cnlvls))) then
   print ("get_color_index: Error: One or more input contour levels are missing")
   return (imsg)
 end if

;---Get nice span of indexes throughout the color map
 col_indexes = span_color_indexes(cmap,dimsizes(cnlvls)+1)
 ncoli       = dimsizes(col_indexes)    ; should be ncn+1

 do n = 0, ncn-1
   if (value .lt. cnlvls(n)) then
     break
   end if
 end do
 if(isstring(cmapt)) then
   return(col_indexes(n)+2)    ; Account for 0/1 index being dropped
  else
   return(col_indexes(n))
 end if
end

;***********************************************************************
; Given an array of contour levels,  a color map, and a single
; value, this function returns an RGB[A] value in the colormap
; to use for representing the single value
;
; This function uses get_color_index, and is very similar to this
; function except it returns the actual RGB[A] value, rather 
; than an index value.
;
; The colormap can be a named colormap, like "rainbow", or an array
; of RGB (n,3) or RGBA (n,4).
;***********************************************************************
undef("get_color_rgba")
function get_color_rgba(cmapt,cnlvls[*]:numeric,value[1]:numeric)
local fmsg, icol, cmap
begin
 fmsg = new(4,float)          ; missing value
 icol = get_color_index(cmapt,cnlvls,value)

 if (ismissing(icol)) then
   return (fmsg)
 end if

 if(isstring(cmapt)) then
   cmap = read_colormap_file(cmapt)
   return(cmap(icol-2,:))    ; Indexes start at 2
 else
   cmap = cmapt
   return(cmap(icol,:))
 end if
end


;***********************************************************************;
; This function returns the list of named colors in the rgb.txt file.
; This can be used to test for valid color names.
;***********************************************************************;
undef("get_named_color_list")
function get_named_color_list()
local db_dir, lines
begin
;---Read the rgb.txt file.
  db_dir = ncargpath("database")
  lines  = asciiread(db_dir + "/rgb.txt",-1,"string")
;
; Read list of named colors off the file.
;
; Need to use str_get_cols and not str_get_field,
; because some color names have spaces in them,
; and hence we can't use a space as a delimiter.
;
  named_colors = str_strip(str_get_cols(lines,12,-1))
  return(named_colors)
end

;***********************************************************************;
; This function checks the list of named colors to make sure they are
; all valid.  It will return a logical array of the same size as the 
; input array.
;***********************************************************************;
undef("is_valid_named_colors")
function is_valid_named_colors(colors:string)
local i, named_colors, valid_named_colors1d, ncolors, colors1d, dims
begin
  dims                 = dimsizes(colors)
  named_colors         = str_lower(get_named_color_list())   ; list of valid colors
  colors1d             = ndtooned(colors)
  ncolors              = dimsizes(colors1d)
  valid_named_colors1d = new(ncolors,logical)
  valid_named_colors1d = False    ; initialize to False
  do i=0,ncolors-1
    valid_named_colors1d(i) = any(str_lower(colors1d(i)).eq.named_colors)
  end do
  return(reshape(valid_named_colors1d,dims))
end

;***********************************************************************;
; This function checks the list of strings to see if they are any of    ;
; the the special "foreground", "background" or "transparent" strings.  ;
; It will return a logical array of the same size as the input array.   ;
;***********************************************************************;
undef("is_valid_special_colors")
function is_valid_special_colors(colors:string)
local i, special_names, dims, colors1d, ncolors
begin
  special_names     = (/"transparent","foreground","background"/)
  dims              = dimsizes(colors)
  colors1d          = ndtooned(colors)
  ncolors           = dimsizes(colors1d)
  special_colors_1d = new(ncolors,logical)
  special_colors_1d = False    ; initialize to False
  do i=0,ncolors-1
    special_colors_1d(i) = any(str_lower(colors1d(i)).eq.special_names)
  end do
  return(reshape(special_colors_1d,dims))
end

;***********************************************************************;
; This function returns the list of colormap names.
; This can be used to test for valid colormap names.
;***********************************************************************;
undef("get_colormap_names")
function get_colormap_names()
local cmap_dirs, nfiles, ndirs, cmaps, i, j, tmp_files, tmp_names
begin
;---Get the list of color map directories
  cmap_dirs = str_split(ncargpath("colormaps"),":")
  ndirs     = dimsizes(cmap_dirs)

  max_files = 500
  cmaps = new(max_files,string)
  nfiles = 0
  do i=0,ndirs-1
    tmp_files := systemfunc("ls -1 " + cmap_dirs(i))
    nt = dimsizes(tmp_files)
    do j=0,nt-1
      tmp_names     := str_split(tmp_files(j),".")
      cmaps(nfiles) = str_join(tmp_names(0:dimsizes(tmp_names)-2),".")
      nfiles = nfiles + 1
      if(nfiles.ge.max_files) then
        print("get_colormap_names: warning: reached maximum limit of color maps")
        print("Will only return " + max_files + " color map names.") 
        break
      end if
    end do
    if(nfiles.ge.max_files) then
      break
    end if 
  end do
  return(cmaps(0:nfiles-1))
end

;***********************************************************************;
; This function checks the list of color map names to see if they are
; all valid.  It will return a logical array of the same size as the 
; input array.
;***********************************************************************;
undef("is_valid_colormap_names")
function is_valid_colormap_names(colormap_names:string)
local i, valid_colormaps, ncmaps
begin
  cmaps  = get_colormap_names()
  colormap_names_1d = ndtooned(colormap_names)
  ncmaps = dimsizes(colormap_names_1d)
  valid_colormaps_1d = new(ncmaps,logical)
  valid_colormaps_1d = False    ; initialize to False
  do i=0,ncmaps-1
    valid_colormaps_1d(i) = any(colormap_names_1d(i).eq.cmaps)
  end do
  return(reshape(valid_colormaps_1d,dimsizes(colormap_names)))
end

;***********************************************************************;
; This function returns the type of color being used: 
;
;    color index           - values 0 to 255               "index"
;    absolute color index  - values 2^30 to (2^31)-1       "absolute"
;                           (1073741824 to 2147483647) 
;    named color           - "blue"                        "named"
;    special color         - "transparent","foreground"    "special"
;    named & special color - contains both named and 
;                            special colors                "mixed"
;    RGB triplet           - (/1,0,0.5/)                   "rgb"
;    RGBA quadruplet       - (/1,0,0.5,0.5/)               "rgba"
;    color map name        - "amwg"                        "colormap"
;
;  Note, if the user enters something like (/1,0,0/) or (/1,0,0,0/), this
;  will be seen as an rgb or rgba color type, and not an index color 
;  type with just 0s and 1s.
;
; "unknown" is returned if no valid type is detected.
;***********************************************************************;
undef("get_color_type")
function get_color_type(colors)
local dims_color, rank_color
begin
  dims_color = dimsizes(colors)
  rank_color = dimsizes(dims_color)

  if(typeof(colors).eq."string".and.\
     all(is_valid_colormap_names(colors))) then
    return("colormap")
  end if

  if(typeof(colors).eq."string".and.\
     all(is_valid_named_colors(colors))) then
    return("named")
  end if

  if(typeof(colors).eq."string".and.\
     all(is_valid_special_colors(colors))) then
    return("special")
  end if

;
; This test is different, because we have to make sure all
; of the colors are either a named color or a special color.
; If you have one bad one in the bunch, then we can't 
; claim it is "mixed".
;
  if(typeof(colors).eq."string".and.\
     (num(is_valid_named_colors(colors))+\
      num(is_valid_special_colors(colors))).eq.product(dims_color))
    return("mixed")
  end if

  if(dims_color(rank_color-1).eq.3.and.isnumeric(colors).and.\
     all(colors.ge.0.and.colors.le.1.0)) then
    return("rgb")
  end if

  if(dims_color(rank_color-1).eq.4.and.isnumeric(colors).and.\
     all(colors.ge.0.and.colors.le.1.0)) then
    return("rgba")
  end if

  if(isnumeric(colors)) then
    if(all(colors.ge.-1.and.colors.le.255)) then
      return("index")
    else if(all(colors.ge.(2^30).and.colors.le.((2^31)-1))) then
      return("absolute")
    end if
    end if
  end if

  return("unknown")
end

;***********************************************************************;
; This function returns True for every element of the input array that  ;
; represents a transparent color, and False otherwise.                  ;
; It only works for named, index, absolute, or RGBA colors.             ;
;                                                                       ;
; Note: if any of the input colors are named colors, but some of them   ;
; are invalid, then Missing is returned for all of them.                ;
; Since this is an internal function, we can change this behavior if    ;
; desired.                                                              ;
;***********************************************************************;
undef("is_color_transparent")
function is_color_transparent(colors)
local dims, rank, istrans, colors2d
begin
  dims = dimsizes(colors)
  rank = dimsizes(dims)
  type = get_color_type(colors)

  valid_types = (/"named","special","mixed","index","rgb","rgba","absolute"/)
  if(.not.any(type.eq.valid_types)) then
    print("is_color_transparent: Error: the input colors must be of type: " + \
          str_join(valid_types,","))
    print("   Will return all missing")
    istrans = new(dims,logical)
    return(istrans)
  end if
  if(type.eq."special".or.type.eq."mixed") then
    return(str_lower(colors).eq."transparent")
  end if
  if(type.eq."index") then
    return(colors.eq.-1)
  end if
  if(type.eq."absolute") then
    colors_rgba = ndtotwod(color_index_to_rgba(colors))
    return(reshape(colors_rgba(:,3).eq.0.0,dims))
  end if
  if(type.eq."rgba") then
    if(rank.eq.1) then
      return(colors(3).eq.0.0)
    else
      colors2d = ndtotwod(colors)
      return(reshape(colors2d(:,3).eq.0.0,dims(0:rank-2)))
    end if
  end if
  if(any(type.eq.(/"named","rgb"/))) then
    istrans = new(dims,logical)
    istrans = False
    return(istrans)
  end if
end

;***********************************************************************;
; Function : get_rgb_values                                             ;
;              named_colors: string array of named colors               ;
;                                                                       ;
; This is a deprecated function. Use namedcolor2rgb instead.            ;
;***********************************************************************;
undef("get_rgb_values")
function get_rgb_values(named_colors)
begin
  print("get_rgb_values: this function is deprecated.")
  print("   Use 'namedcolor2rgb' instead.")
  return(namedcolor2rgb(named_colors))
end


;***********************************************************************;
; This function returns the RGB triplets associated with a 
; given list of named colors.
; 
; A n x 3 array is returned, where "n" is the number of 
; named colors input.
;
; If a color is not found, missing values are returned for 
; that color.
;***********************************************************************;
undef("namedcolor2rgb")
function namedcolor2rgb(names:string)
local db_dir, lines, named_colors, i, ii, ncolors, \
lnames, rgb_array
begin
;---Read list of named colors off the file.
  named_colors = str_lower(get_named_color_list())
;
; Use str_sub_str to remove all tabs and spaces around
; and inside the color name.
;
  names1d      = ndtooned(names)
  ncolors      = dimsizes(names1d)
  lnames       = str_lower(names1d)
  lnames       = str_sub_str(lnames," ","")
  lnames       = str_sub_str(lnames,"	","")
  named_colors = str_sub_str(named_colors," ","")
  named_colors = str_sub_str(named_colors,"	","")

  rgb_array_2d = new((/ncolors,3/),float)

;---Read the rgb.txt file.
  db_dir = ncargpath("database")
  lines  = asciiread(db_dir + "/rgb.txt",-1,"string")

;---Loop through named colors and find the requested one(s).
  do i=0,ncolors-1
;---Get index into rgb.txt table. There can be more than one.
    ii := ind(lnames(i).eq.named_colors)

;---Get associated RGB triplet
    if(.not.any(ismissing(ii))) then
      rgb_array_2d(i,0) = tointeger(str_get_field(lines(ii(0)),1,\
                                   " 	"))/255.
      rgb_array_2d(i,1) = tointeger(str_get_field(lines(ii(0)),2,\
                                   " 	"))/255.
      rgb_array_2d(i,2) = tointeger(str_get_field(lines(ii(0)),3,\
                                   " 	"))/255.
    else
      print("namedcolor2rgb: Warning: '" + names1d(i)  + "' is not a valid named color.")
      print("Will return missing values for this color.")
    end if
  end do

  dims = dimsizes(names)
  rank = dimsizes(dims)
  new_dims = new(rank+1,typeof(dims))
  new_dims(0:rank-1) = dims
  new_dims(rank)     = 3
  return(reshape(rgb_array_2d,new_dims))
end

;***********************************************************************;
; This function returns the RGBA values given an RGB array. The
; "alpha" index is set to 1.0.
; 
; A N x 4 array is returned, where "N" represents the leftmost
; dimesions of "rgb".
;***********************************************************************;
undef("rgb2rgba")
function rgb2rgba(rgb:numeric)
local rggb_dims, rgb_rank, left_dims, rgba_dims, rgba2d, rgb2d
begin
  rgb_dims = dimsizes(rgb)
  rgb_rank = dimsizes(rgb_dims)

  if(rgb_dims(rgb_rank-1).ne.3) then
    print("rgb2rgba: Error: The input must be an RGB array with rightmost dimension of 3")
    print("   Returning missing.")
    return(new(4,float))
  end if

  if(rgb_rank.eq.1) then
    rgba      = new(4,typeof(rgb))
    rgba(0:2) = rgb
    rgba(3)   = 1
    return(rgba)
  end if

; rgb_rank >= 2
  left_dims     = rgb_dims(0:rgb_rank-2)
  rgba2d        = new((/product(left_dims),4/),typeof(rgb))
  rgba2d(:,0:2) = ndtotwod(rgb)
  rgba2d(:,3)   = 1
  if(rgb_rank.eq.2) then
    return(rgba2d)
  else
;---Reshape before we return
    rgba_dims = new(rgb_rank,typeof(rgb_dims))
    rgba_dims(0:rgb_rank-2) = rgb_dims(0:rgb_rank-2)
    rgba_dims(rgb_rank-1)   = 4
    return(reshape(rgba2d,rgba_dims))
  end if
end

;***********************************************************************;
; This function returns the RGBA triplets associated with a 
; given array of named colors.  The "A" value is always returned 
; as the value of 1.0. This function basically calls namedcolor2rgb and
; rgb2rgba.
;***********************************************************************;
undef("namedcolor2rgba")
function namedcolor2rgba(names:string)
begin
  return(rgb2rgba(namedcolor2rgb(names)))
end

;***********************************************************************;
; This function returns the RGBA quadruplets associated with a 
; given list of index colors.
; 
; A N x 4 array is returned, where "N" represents the input dimensions.
;
; If any input colors are invalid, then missing will be returned for 
; that single input.
;***********************************************************************;
undef("indexcolor2rgba")
function indexcolor2rgba(wks,color_indexes)
local dims, rank, msg,color_indexes_1d, rgba_cmap, rgb_cmap, ncolors, \
new_dims, rgba_2d
begin
  getvalues wks
      "wkColorMap" : rgb_cmap
  end getvalues
  ncolors = dimsizes(rgb_cmap(:,0))

  if(any(color_indexes.lt.-1.or.color_indexes.ge.ncolors)) then
    print("indexcolor2rgba: Error: one or more of the index colors")
    print("are invalid. Will return missing values for these values.")
  end if

  dims = dimsizes(color_indexes)
  rank = dimsizes(dims)
  new_dims           = new(rank+1,typeof(dims))
  new_dims(0:rank-1) = dims
  new_dims(rank)     = 4

  color_indexes_1d = ndtooned(color_indexes)
;
; We need to do things in 1D for indexing purposes, then
; we'll convert back to the original array shape upon
; return.
;
  nindexes = dimsizes(color_indexes_1d)
  rgba_cmap = rgb2rgba(rgb_cmap)
  rgba_2d = new((/dimsizes(color_indexes_1d),4/),typeof(rgba_cmap))
  do n=0,nindexes-1
    if(.not.ismissing(color_indexes_1d(n))) then
      if(color_indexes_1d(n).ge.0.and.color_indexes_1d(n).lt.ncolors) then
        rgba_2d(n,:) = rgba_cmap(color_indexes_1d(n),:)
      else if(color_indexes_1d(n).eq.-1) then
        rgba_2d(n,:) = (/0.,0.,0.,0./)
      end if
      end if
    end if
  end do
  return(reshape(rgba_2d,new_dims))
end

;***********************************************************************;
; This function returns the RGBA quadruplets associated with the 
; special colors: "background", "foreground", "transparent"
; 
; A N x 4 array is returned, where "N" represents the input dimensions.
;
; If any input colors are invalid, then missing will be returned for 
; that single input.
;***********************************************************************;
undef("specialcolor2rgba")
function specialcolor2rgba(wks,special_colors)
local special_names,rgb_cmap,ncolors,valid_special_colors,dims,rank,new_dims,\
foreground_rgb,foreground_rgba,background_rgb,background_rgba
begin
  getvalues wks
      "wkBackgroundColor" : background_rgb
      "wkForegroundColor" : foreground_rgb
  end getvalues
  background_rgba  = rgb2rgba(background_rgb)
  foreground_rgba  = rgb2rgba(foreground_rgb)
  transparent_rgba = (/0.,0.,0.,0./)

  special_colors_1d    = ndtooned(str_lower(special_colors))
  valid_special_colors = is_valid_special_colors(special_colors_1d)
  if(.not.all(valid_special_colors)) then
    print("specialcolor2rgba: Error: one or more of the special colors")
    print("are invalid. Will return missing values for these values.")
  end if

  dims = dimsizes(special_colors)
  rank = dimsizes(dims)
  new_dims           = new(rank+1,typeof(dims))
  new_dims(0:rank-1) = dims
  new_dims(rank)     = 4
;
; We need to do things in 1D for indexing purposes, then
; we'll convert back to the original array shape upon
; return.
;
  ncolors = dimsizes(special_colors_1d)
  rgba_2d = new((/dimsizes(special_colors_1d),4/),float)
  idxb = ind(special_colors_1d.eq."background")
  idxf = ind(special_colors_1d.eq."foreground")
  idxt = ind(special_colors_1d.eq."transparent")
  if(.not.any(ismissing(idxf))) then
    rgba_2d(idxf,:) = conform_dims((/dimsizes(idxf),4/),foreground_rgba,1)
  end if
  if(.not.any(ismissing(idxb))) then
    rgba_2d(idxb,:) = conform_dims((/dimsizes(idxb),4/),background_rgba,1)
  end if
  if(.not.any(ismissing(idxt))) then
    rgba_2d(idxt,:) = conform_dims((/dimsizes(idxt),4/),transparent_rgba,1)
  end if
  return(reshape(rgba_2d,new_dims))
end

;***********************************************************************;
; This function returns the RGBA quadruplets associated with either
; named colors or special colors "background", "foreground", and
; "transparent". If you know you have only named colors, use 
; "namedcolor2rgba". If you know you have only special colors, use 
; "specialcolor2rgba".
;
; While named colors and special colors seem similar, they are not.
; In order to retrieve the RGBA value for "background" and "foreground",
; you need to pass in "wks" so you can retrieve the wkForegroundColor
; and wkBackgroundColor resource values.
; 
; A N x 4 array is returned, where "N" represents the input dimensions.
;
; If any input colors are invalid, then missing will be returned for 
; that single input.
;***********************************************************************;
undef("mixedcolor2rgba")
function mixedcolor2rgba(wks,colors:string)
local special_names,rgb_cmap,ncolors,valid_special_colors,valid_named_colors,\
dims,rank,new_dims, idxn, idxs
begin
  colors_1d = ndtooned(str_lower(colors))
  valid_special_colors = is_valid_special_colors(colors_1d)
  valid_named_colors   = is_valid_named_colors(colors_1d)

  dims = dimsizes(colors)
  rank = dimsizes(dims)
  new_dims           = new(rank+1,typeof(dims))
  new_dims(0:rank-1) = dims
  new_dims(rank)     = 4
;
; We need to do things in 1D for indexing purposes, then
; we'll convert back to the original array shape upon
; return.
;
  ncolors = dimsizes(colors_1d)
  rgba_2d = new((/dimsizes(colors_1d),4/),float)
  idxn = ind(valid_named_colors)
  idxs = ind(valid_special_colors)
  if(.not.any(ismissing(idxn))) then
    rgba_2d(idxn,:) = namedcolor2rgba(colors_1d(idxn))
  end if
  if(.not.any(ismissing(idxs))) then
    rgba_2d(idxs,:) = specialcolor2rgba(wks,colors_1d(idxs))
  end if
  return(reshape(rgba_2d,new_dims))
end

;***********************************************************************;
; This is currently an unadvertised function, but it could be useful    ;
; to outside users.  The slightly annoying thing is that you have to    ;
; pass in the workstation to handle the case where the user is giving   ;
; color index values into a workstation color map.                      ;
;                                                                       ;
; Given a list of color maps, color indexes (workstation or absolute),  ;
; named colors, RGB, or RGBA colors, convert them to RGBA colors.       ;
;                                                                       ;
; I decided to always have this function return N x 4 even in the case  ;
; of N=1, so that other routines that use this function don't have to   ;
; test whether they have a 1D or 2D array. They can use                 ;
; rm_single_dims_no_meta if they need to get rid of a degenerate        ;
; dimension.                                                            ;
;***********************************************************************;
undef("convert_color_to_rgba")
function convert_color_to_rgba(wks,colors)
local color_type, dims, rank, new_dims
begin
  color_type = get_color_type(colors)
  if(color_type.eq."unknown")
    dims = dimsizes(colors)
    rank = dimsizes(dims)
    new_dims = new(rank+1,typeof(dims))
    new_dims(0:rank-1) = dims
    new_dims(rank)     = 4
    print("convert_color_to_rgba: Error: unknown color type.")
    print("   Returning all missing values.")
    return(new(new_dims,float))
  end if

  if(color_type.eq."rgba") then
    rgba_colors = colors
  else if(color_type.eq."index") then
    rgba_colors = indexcolor2rgba(wks,colors)
  else if(color_type.eq."absolute") then
    rgba_colors = color_index_to_rgba(colors)
  else if(color_type.eq."colormap") then
    rgba_colors = read_colormap_files(colors)
  else if(color_type.eq."named") then
    rgba_colors = namedcolor2rgba(colors)
  else if(color_type.eq."special") then
    rgba_colors = specialcolor2rgba(wks,colors)
  else if(color_type.eq."mixed") then
    rgba_colors = mixedcolor2rgba(wks,colors)
  else if(color_type.eq."rgb") then
    rgba_colors = rgb2rgba(colors)
  end if
  end if
  end if
  end if
  end if
  end if
  end if
  end if

  rank = dimsizes(dimsizes(rgba_colors))
  if(rank.eq.1.and.all(dimsizes(rgba_colors).eq.(/4/))) then
    return(reshape(rgba_colors,(/1,4/)))
  else
    return(rgba_colors)
  end if
end


;***********************************************************************;
; Given two named colors and the number of colors, this
; function creates a series of RGB triplets that spans
; between the two colors.
;***********************************************************************;
undef("span_two_named_colors")
function span_two_named_colors(named1[1]:string,named2[1]:string,ncolors,\
                               opt[1]:logical)
local rgb, msg, hsv1, hsv2, ii, hsv_array, debug, opt2, \
      beg_hue, end_hue, beg_sat, end_sat, beg_val, end_val
begin
  opt2 = opt

;---For debug prints
  debug = get_res_value_keep(opt2,"Debug",False)

  rgb = namedcolor2rgb((/named1,named2/))
  msg = new(3,float)
  if(any(ismissing(rgb))) then
    print("span_two_named_colors: Error: one or both of the named colors")
    print("are invalid. Returning missing.")
    return(msg)
  end if

;---Convert to HSV.
  hsv1 = rgbhsv(rgb(0,:))
  hsv2 = rgbhsv(rgb(1,:))

  if(debug)
    rgbstr1 = "(" + sprintf("%7.2f", rgb(0,0)) + ", " + \
                    sprintf("%7.2f", rgb(0,1)) + ", " + \
                    sprintf("%7.2f", rgb(0,2)) + ") "
    hsvstr1 = "(" + sprintf("%7.2f", hsv1(0)) + ", " + \
                    sprintf("%7.2f", hsv1(1)) + ", " + \
                    sprintf("%7.2f", hsv1(2)) + ") "
    rgbstr2 = "(" + sprintf("%7.2f", rgb(1,0)) + ", " + \
                    sprintf("%7.2f", rgb(1,1)) + ", " + \
                    sprintf("%7.2f", rgb(1,2)) + ") "
    hsvstr2 = "(" + sprintf("%7.2f", hsv2(0)) + ", " + \
                    sprintf("%7.2f", hsv2(1)) + ", " + \
                    sprintf("%7.2f", hsv2(2)) + ") "

    print(named1 + ", RGB: " + rgbstr1)
    print(named2 + ", RGB: " + rgbstr2)
    print(named1 + ", HSV: " + hsvstr1)
    print(named2 + ", HSV: " + hsvstr2)
  end if

  if(ncolors.le.0.or.ncolors.gt.256) then
    print("span_two_named_colors: Error: Invalid number of colors requested.")
    print("Defaulting to 256.")
    ncolors = 256
  end if

;---Create array to return
  hsv_array = new((/ncolors,3/),float)

;---Generate a span of colors from first color to second
  beg_hue = hsv1(0)    ; begin HUE value
  end_hue = hsv2(0)    ; end HUE value
  beg_sat = hsv1(1)    ; begin SAT value
  end_sat = hsv2(1)    ; end SAT value
  beg_val = hsv1(2)    ; begin VAL value
  end_val = hsv2(2)    ; end VAL value

  ii             = ispan(0,ncolors-1,1)
  hsv_array(:,0) = beg_hue + \
                   ii*((end_hue-beg_hue)/(ncolors-1))
  hsv_array(:,1) = beg_sat + \
                   ii*((end_sat-beg_sat)/(ncolors-1))
  hsv_array(:,2) = beg_val + \
                   ii*((end_val-beg_val)/(ncolors-1))

  return(hsvrgb(hsv_array))
end

;***********************************************************************;
; Given a list of named colors, this function creates a 
; series of RGB triplets that creates a span between each 
; set of colors. The first two colors will be set to white
; and black.
;
; By default, it will create a color map with 256 triplets.
; The "opt" variable can be used to set the following
; options:
;
;   opt@NumColorsInTable
;      Number of colors to put in the color table.
;      Must be <= 256 and > # of named colors specified.
;
;   opt@NumColorsInRange
;      Number of colors to span between each set of
;      named colors. For example, if you specify three 
;      colors (/"red", "green", "blue","purple"/), and
;      NumColorsInRange of (/5, 8, 4/), then you'll have a
;      color table with:
;
;   0        1        2         6         13      16
;  white   black     red......green......blue....purple
;***********************************************************************;
undef("span_named_colors")
function span_named_colors(named_colors,opt[1]:logical)
local i, ncolt, ncols_per_rng, def_ncols_per_rng, nnamed, \ 
      rgb_array, nbeg, nend, ncpg, debug, rgbtmp, opt2
begin
  opt2 = opt

;---For debug prints
  debug = get_res_value_keep(opt2,"Debug",False)


;---Get # of colors in color table.
  ncolt  = get_res_value_keep(opt2,"NumColorsInTable",256)
  if(ncolt.le.0) then
    print("span_named_colors: Error: # of colors must be > 0")
    return(new((/1,3/),float))
  end if

;---Background/foreground colors desired?
  skip_bf = get_res_value_keep(opt2,"SkipBackgroundForeground",False)
  if(ncolt.le.0) then
    print("span_named_colors: Error: # of colors must be > 0")
    return(new((/1,3/),float))
  end if

;---Check # of named colors.
  nnamed = dimsizes(named_colors)
  if(nnamed.le.1) then
    print("span_named_colors: Error: You must specify more than one named color.")
    return(new((/1,3/),float))
  end if

;
; Get number of colors between each set of named colors.
; The number in the range includes both end colors.
;
; Default is to use equally spaced colors.
;
  ncpg              = (ncolt-2)/(nnamed-1)
  def_ncols_per_rng = new(nnamed-1,integer)
  def_ncols_per_rng = ncpg
  if(opt2.and.isatt(opt2,"NumColorsInRange")) then
    ncols_per_rng = get_res_value_keep(opt2,"NumColorsInRange",-1)
    if(dimsizes(ncols_per_rng).ne.(nnamed-1)) then
      print("span_named_colors: Error: Invalid # of colors per range.")
      print("Using equally spaced colors.")
      delete(ncols_per_rng)
      ncols_per_rng = def_ncols_per_rng
    end if
  else
    ncols_per_rng = def_ncols_per_rng
  end if

;
; Calculate total number of colors and create RGB 
; array to populate.
;
; Since the inside colors are part of two ranges, but only
; counted once, you need to account for this. The "2" is
; for the background, foreground coors.
;
  ncolt     = sum(ncols_per_rng)+2-(nnamed-2)
  rgb_array = new((/ncolt,3/),float)

  if(debug) then
    print("# colors in table  = " + ncolt)
  end if

;---Background color defaults to white.
  rgb_array(0,:) = (/1.,1.,1./)      ; White
  if(opt2.and.isatt(opt2,"BackgroundColor")) then
    rgbtmp = namedcolor2rgb(opt2@BackgroundColor)
    if(any(ismissing(rgbtmp))) then
      print("span_named_colors: Error: Invalid background color.")
      print("Defaulting to white.")
    else
      rgb_array(0,:) = rgbtmp
    end if
  end if

;---Foreground color defaults to black.
  rgb_array(1,:) = (/0.,0.,0./)      ; Black
  if(opt2.and.isatt(opt2,"ForegroundColor")) then
    rgbtmp = namedcolor2rgb(opt2@ForegroundColor)
    if(any(ismissing(rgbtmp))) then
      print("span_named_colors: Error: Invalid foreground color.")
      print("Defaulting to black.")
    else
      rgb_array(1,:) = rgbtmp
    end if
  end if

  nbeg = 2
  nend = nbeg + ncols_per_rng(0) - 1
  do i=0,nnamed-2
    if(debug) then
      print("Indexes " + nbeg + " to " + nend + \
            ", "+named_colors(i)+" to "+named_colors(i+1)+\
            ", " + ncols_per_rng(i) + " colors per range")
    end if
    rgb_array(nbeg:nend,:) = span_two_named_colors(named_colors(i), \
                                                   named_colors(i+1), \
                                                   ncols_per_rng(i),opt2)
    if(i.lt.(nnamed-2)) then
      nbeg = nend     ; Start where we ended
      nend = nbeg + ncols_per_rng(i+1) -1
    end if
  end do
  
  if(skip_bf) then
    return(rgb_array(2:,:))
  else
    return(rgb_array)
  end if
end

;***********************************************************************;
; Procedure : draw_color_palette                                        ;
;                   wks[1] : graphic                                    ;
;                   colors :                                            ;
;                   opt[1] : logical                                    ;
;                                                                       ;
; This procedure draws the given colors as a series of boxes in the     ;
; same fashion as the old gsn_draw_colormap procedure.                  ;
;                                                                       ;
; "wks" is the workstation to draw the colors to.                       ; 
;                                                                       ;
; "colors" can be a color map name ("rainbow"), a list of named colors  ;
; (/"red","blue"/), an RGB array (n x 3), an RGBA array (n x 4), or a   ;
; list of color indexes (/2,5,3,8/).                                    ;
;                                                                       ;
;  "opt" If set to True, then you can optionally attach attributes to   ;
;        control behavior of this procedure.                            ;
;                                                                       ;
;***********************************************************************;
undef("draw_color_palette")
procedure draw_color_palette(wks,colors,opt)
local label_boxes, call_frame, labels_on, label_strings, across, \
color_type, rgba_colors, ncolors, nrows, ncols,width,height,\
canvas,gsid
begin
;---Retrieve values for attributes
  call_frame    = get_res_value_keep(opt,"Frame",True)
  labels_on     = get_res_value_keep(opt,"LabelsOn",True)
  if(opt.and.isatt(opt,"LabelStrings")) then
    set_label_strings = True
    label_strings     = opt@LabelStrings
  else
    set_label_strings = False
  end if
  font_height   = get_res_value_keep(opt,"LabelFontHeight",0.015)
  across        = get_res_value_keep(opt,"Across",True)

;---Check for valid colors
  color_type = get_color_type(colors)

  if(ismissing(color_type).or.color_type.eq."unknown")
    print("draw_color_palette: Error: invalid color specification.")
    return
  end if

  if(color_type.eq."rgba") then
    rgba_colors = colors
  end if
  if(color_type.eq."index") then
    rgba_colors = indexcolor2rgba(wks,colors)
  end if
  if(color_type.eq."absolute") then
    rgba_colors = color_index_to_rgba(colors)
  end if
  if(color_type.eq."colormap") then
    rgba_colors = read_colormap_files(colors)
  end if
  if(color_type.eq."named") then
    rgba_colors = namedcolor2rgba(colors)
  end if
  if(color_type.eq."rgb") then
    rgba_colors = rgb2rgba(colors)
  end if

  ncolors = dimsizes(rgba_colors(:,0))
  nrows   = toint(sqrt(ncolors))

;---Figure out ncols such that the columns will span across the page.
  ncols = floattoint(ncolors/nrows)
  if((ncols*nrows).lt.ncolors)
    ncols = ncols+1
  end if

  ntotal = nrows * ncols        ; # of colors per page.

;---If drawing labels, test that we have valid number of labels
  if(labels_on) then
    if(.not.set_label_strings) then
      label_strings = "" + ispan(0,ncolors-1,1)    
    else
      if(dimsizes(label_strings).ne.ncolors) then
        print("draw_color_palette: Error: invalid number of labels for boxes")
        return
      end if
    end if
  end if
;---Calculate X and Y positions of text and box in the view port.
  width  = 1./ncols
  height = 1./nrows

  if(ncols.gt.1) then
    if(across) then
      xpos = ndtooned(conform_dims((/nrows,ncols/),fspan(0,1-width,ncols),1))
    else
      xpos = ndtooned(conform_dims((/ncols,nrows/),fspan(0,1-width,ncols),0))
    end if
  else
    xpos = new(ntotal,float)
    xpos = 0.
  end if
  if(nrows.gt.1) then
    if(across) then
      ypos = ndtooned(conform_dims((/nrows,ncols/),fspan(1-height,0,nrows),0))
    else
      ypos = ndtooned(conform_dims((/ncols,nrows/),fspan(1-height,0,nrows),1))
    end if
  else
    ypos = new(ntotal,float)
    ypos = 1.-height
  end if

;---Calculate box coordinates.
  xbox = (/0,width, width,     0,0/)
  ybox = (/0,    0,height,height,0/)

  if(labels_on) then
    font_space                  = font_height/2.
  end if

  canvas = create "canvas" logLinPlotClass wks
    "vpXF"      : 0.0
    "vpYF"      : 1.0
    "vpWidthF"  : 1.0
    "vpHeightF" : 1.0
  end create
  gsid = create "graphic_style" graphicStyleClass wks 
    "gsLineColor" : "black"
  end create

;---ntotal colors per page.
  do i = 0,ncolors-1
;---Draw box and fill in the appropriate color.
    setvalues gsid
      "gsFillColor" : rgba_colors(i,:)
    end setvalues
    NhlNDCPolygon(canvas,gsid,xbox+xpos(i),ybox+ypos(i))

;---Outline box in black.
    NhlNDCPolyline(canvas,gsid,xbox+xpos(i),ybox+ypos(i))

;---Draw color label.
    if(labels_on) then
      txid = create "text_ndc"+i textItemClass wks
        "txString"              : label_strings(i)
        "txPosXF"               : font_space+xpos(i)
        "txPosYF"               : ypos(i)+font_space
        "txFontHeightF"         : font_height
        "txFont"                : "helvetica-bold"
        "txJust"                : "BottomLeft"
        "txPerimOn"             : True
        "txPerimColor"          : "black"
        "txFontColor"           : "black"
        "txBackgroundFillColor" : "white"
      end create
      draw(txid)
    end if
  end do
  if(call_frame) then
    frame(wks)   ; Advance the frame.
  end if
  return
end

;***********************************************************************;
; function : hsv2rgb                                                    ;
;                 h:float                                               ;
;                 s:float                                               ;
;                 v:float                                               ;
;                                                                       ;
; Note: after V4.3.1, the built-in function hsvrgb was added. This      ;
; should be used instead of this one.                                   ;
;                                                                       ;
; This function maps values from the HSV color model to the RGB color   ;
; model. HSV is a good model for generating smooth color maps. See      ;
; (Computer Graphics: Principles and Practice by Foley). The return     ;
; value is a 2 dimensional array of rgb color triplets. The return      ;
; value from this function can be directly assigned to the "wkColorMap" ;
; resource of a workstation object or to the second argument of         ;
; gsn_define_colormap.                                                  ;
;                                                                       ;
;***********************************************************************;
undef("hsv2rgb")
function hsv2rgb (h_old[*]:float,s_old[*]:float,v_old[*]:float)
begin
;
; Make a backup copy of the HSV values.
;
  h = h_old
  s = s_old
  v = v_old
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
    print("hsv2rgb: Warning: Some invalid HSV values were passed to hsv2rgb")
  end if
  return(r_g_b(cmap_len|:,rgb|:))
end


