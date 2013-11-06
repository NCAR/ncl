;--------------------------------------------------------------------------------
; This function convert input variable x to type specified by type.
; Wei Huang
; May 21, 2012
;--------------------------------------------------------------------------------

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

     print("")
     print("WARNING:")
     print("CANNOT convert input variable type: <" + typeof(varin) + "> to type: <" + type + ">")
     print("The original type: <" + typeof(varin) + "> is returned.")
     print("")

     varout = varin
     return(varout)
 end

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
            if (isfilepresent(path2)) then
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
      if (strlen(lines(i)).eq.0) then  
          lines(i) = "#"  ; zero-lengthed lines cause us grief...
      end if 
      tokens = str_split(lines(i), " ")
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
     print ("Error: get_color_index: cmap must be an n x 3 or n x 4 array of RGB or RGBA values, or a valid color map name")
     return(new(3,"float"))    ; return a missing value
   end if
   cmap = cmapt
 else
   print ("Error: get_color_index: cmap must be an n x 3 or n x 4 array of RGB or RGBA values, or a valid color map name")
 end if
 end if

 ncn  = dimsizes (cnlvls)
 nclr = dimsizes (cmap(:,0))
 imsg = new(1,integer)          ; missing value

 if (nclr-2 .lt. ncn+1) then 
   print ("Warning: get_color_index: Not enough colors in colormap for number of contour levels")
   print ("         Colors will be repeated")
 end if
 if (ismissing(value)) then
   print ("Error: get_color_index: Input value is missing")
   return (imsg)
 end if
 if (any(ismissing(cnlvls))) then
   print ("Error: get_color_index: One or more input contour levels are missing")
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


