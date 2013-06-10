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
  if (maxValue.lt.256) then
      cmap(:,3) = where(cmap(:,3).lt.0, 255., cmap(:,3))
      cmap = cmap / 255.
  else 
      if (maxValue.eq.256) then
          cmap(:,3) = where(cmap(:,3).lt.0, 256., cmap(:,3))
          cmap = cmap / 256.
      else 
          if (maxValue.eq.65536) then
              cmap(:,3) = where(cmap(:,3).lt.0, 65535., cmap(:,3))
              cmap = cmap / 65535. 
          else 
              if (maxValue.eq.65536) then
                  cmap(:,3) = where(cmap(:,3).lt.0, 65536., cmap(:,3))
                  cmap = cmap / 65536.
              else
                  cmap(:,3) = where(cmap(:,3).lt.0, maxValue, cmap(:,3))
                  cmap = cmap / maxValue
              end if
          end if 
      end if
  end if
        
  return cmap
end
