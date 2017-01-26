; 
;      $Id: contributed.ncl,v 1.273 2010-05-07 17:38:12 haley Exp $
;
; Contributed.ncl
;
; The codes in this script were contributed by various NCL'ers.  They are
; designed to be completely self contained so that if a user desires, the
; code can be extracted without loss of functionality.
;
;************************************************************
undef("get_pi")
function get_pi( p )
local pi, ptype
begin
  ptype = typeof(p)

  if (ptype.eq."double" .or.  \
     (ptype.eq."string" .and. (p.eq."double" .or. p.eq."d"))) then
      pi = 4d*atan(1d0)
  else
      pi = 4.0*atan(1.0)       ; default is to return float
  end if
  pi@long_name = "ratio of a circle's circumference to its diameter"
  return( pi )
end
;************************************************************
undef("get_d2r")                ; degrees to radians
function get_d2r( x )
local d2r
begin
  d2r = get_pi(x)/180
  d2r@long_name = "convert degrees to radians"
  return( d2r )
end
;************************************************************
undef("get_r2d")                ; radians to degrees
function get_r2d( x )
local r2d
begin
  r2d = 180/get_pi(x)
  r2d@long_name = "convert radians to degrees"
  return( r2d )
end
;************************************************************
; return number of elements of an array [scalar==> size=1]
;************************************************************
undef("size_array")
function size_array(x)
begin
  return( product(dimsizes(x)) )
end
;************************************************************
; D. Shea
; return True if "x" has one of several synonyms for "long_name"
;************************************************************

undef ("isatt_LongName")
function isatt_LongName(x)
local LongName, xAtts, nAtts, n
begin
  LongName = (/"long_name","description","standard_name" \
              ,"DESCRIPTION","DataFieldName" /)
  xAtts    = getvaratts(x)
  nAtts    = dimsizes(xAtts)
  do n=0,nAtts-1
     if (any(LongName.eq.xAtts(n))) then
         return( True )
     end if
  end do
  return( False )
end

;************************************************************
; D. Shea
; return the "long_name" attribute of a variable
; Check for various synonyms

undef ("getLongName")
function getLongName(x)
; return long_name: check for various synonyms
begin
  if (isatt(x,"long_name")) then      ; COARDS, CSM, CF
      return(x@long_name)
  end if

  if (isatt(x,"description")) then    ; WRF
      return(x@description)
  end if

  if (isatt(x,"DESCRIPTION")) then
      return(x@DESCRIPTION)
  end if

  if (isatt(x,"standard_name")) then  ; CF
      return(x@standard_name)
  end if

  if (isatt(x,"DataFieldName")) then  ; HDF [some]
      return(x@DataFieldName)
  end if
 ;return("")                          ; return 
  return(new(1,string))
end

; ******************************************************************
; D. Shea
; error check: called internally by a number of functions
;              make sure all dimension are named
; sample:   dNam = namDimCheck ("clmMonLLT", x)  

undef("namDimCheck")
function namDimCheck (name:string, x)
local rank, dNam, i
begin
  rank = dimsizes( dimsizes(x) )
  dNam = new ( rank, "string")    ; save input dim names
  do i=0,rank-1
     if (.not.ismissing(x!i)) then
         dNam(i) = x!i
     else
         print(name+": All dimensions should be named")
         print("       dimension "+i+" is missing"  )
        ;exit
         dNam(i) = "bogus_"+i   ; assign arbitrary name 
     end if
  end  do
  return (dNam)
end

;************************************************************
; D. Shea
; Copy all of the coordinate variables from one variable to another.  

undef("copy_VarCoords")
procedure copy_VarCoords(var_from,var_to)  
local dfrom, dto, rfrom, rto, i, dName
begin                                     
  dfrom = dimsizes(var_from)
  dto   = dimsizes(var_to)
  
  rfrom = dimsizes(dfrom)
  rto   = dimsizes(dto)
                                             ; coordinates must have names
  dName = getvardims(var_from)               ; Oct 18, 2005

  if (.not.all(ismissing(dName))) then

      if (all(dfrom(0:rto-1).eq.dto)) then
          do i = 0,rto-1
             if (.not.ismissing(dName(i))) then   ; Oct 18, 2005
                var_to!i = var_from!i
                if(iscoord(var_from,var_from!i))
               var_to&$var_to!i$ = var_from&$var_from!i$
                end if
             end if
          end  do
      else
          print("ERROR: copy_VarCoords: dimension sizes do not match")
          print(dto)
          print(dfrom)
      end if

  end if
end
;************************************************************
; D. Shea
; Copy the coordinate variables from one variable to another,
; except for last dimension.  
; Used internally

undef ("copy_VarCoords_1")
procedure copy_VarCoords_1(var_from,var_to)  
local dimt, dimf, rfrom, rto, i, dName
begin                      

  dimf  = dimsizes(var_from)            
  dimt  = dimsizes(var_to)
  rfrom = dimsizes(dimf)      ; rank of var_from
  rto   = dimsizes(dimt)      ; rank of var_to

  dName = getvardims(var_from)               ; Oct 18, 2005

  if (.not.all(ismissing(dName))) then
       ;if (rto.eq.(rfrom-1)) then
                do i = 0,rfrom-2            ; do not use last dimension
                   if (.not.ismissing(dName(i)).and.dimf(i).eq.dimt(i)) then  
                        var_to!i = var_from!i
                        if(iscoord(var_from,var_from!i))
                                var_to&$var_to!i$ = var_from&$var_from!i$
                        end if
                   end if
                end  do
       ;else
       ;        print("ERROR: copy_VarCoords_1: rank problem")
       ;end if
  end if
end
; *****************************************************************
; D. Shea
; Copy the coordinate variables from one variable to another,
; except for last two dimensions.  
; Used internally

undef ("copy_VarCoords_2")
procedure copy_VarCoords_2(var_from,var_to)  
local dimt, dimf, rfrom, rto, i, dName
begin                      
  dimf  = dimsizes(var_from)            
  rfrom = dimsizes(dimf)      ; rank of var_from

  if (rfrom.le.2) then
      return
  end if

  dName = getvardims(var_from)               ; Oct 18, 2005

  if (.not.all(ismissing(dName))) then
        if (rfrom.gt.2) then
            dimt  = dimsizes(var_to)
            rto   = dimsizes(dimt)      ; rank of var_to
            do i = 0,rfrom-3            ; do not use last two dimensions
               if (.not.ismissing(dName(i)) .and. dimf(i).eq.dimt(i)  ) then
                    var_to!i = var_from!i
                    if(iscoord(var_from,var_from!i))
                       var_to&$var_to!i$ = var_from&$var_from!i$
                    end if
                end if
            end  do
        end if
  end if
end

;************************************************************
; M. Haley
; Return a list of dimension indexes that correspond with
;  the dimension names.
;
; Used internally.

undef("dimnames_to_indexes")
function dimnames_to_indexes(var_from,dim_args[*])
local vdims_from, ndims, dims, i, imsg
begin
  ndims = dimsizes(dim_args)
  imsg = new(ndims,"integer")

  if(.not.any(typeof(dim_args).eq.(/"string","integer"/))) then
    print("dimnames_to_indexes: error: invalid type for last argument")
    return(imsg)
  end if

  if(typeof(dim_args).eq."string") then
    vdims_from = getvardims(var_from)
    dims = new(ndims,integer)
    do i=0,dimsizes(dim_args)-1
      ii = ind(dim_args(i).eq.vdims_from)
      if(.not.any(ismissing(ii))) then
        dims(i) = ii(0)
      else
        print("dimnames_to_indexes: error: invalid dimension name")
        return(imsg)
      end if
      delete(ii)
    end do
  else
    dims = dim_args
  end if
  return(dims)
end

;************************************************************
; M. Haley
;
; Dec 30 2001
; Copy the coordinate arrays of the given dimensions from
;    one array to another.  
;
; This function makes the assumption that the "dims"
;     dimensions represent dimensions in the "var_from"
;     array. They do not have to be monotonic.
;
; Note: in the special case where the rank of the two arrays are
;  the same, then the assumption that the dimensions to copy are
;  in the same dimension locations in both arrays. Otherwise,
;  coordinates are copied to the "var_to" variable starting
;  with the leftmost dimension and going to the next one, etc.
;
undef ("copy_VarCoords_n")
procedure copy_VarCoords_n(var_from,var_to,dims_to_copy)
local dimt, dimf, rfrom, rto, ifrom, ito, dName, ndims
begin                      
  dimf  = dimsizes(var_from)            
  dimt  = dimsizes(var_to)
  rfrom = dimsizes(dimf)      ; rank of var_from
  rto   = dimsizes(dimt)      ; rank of var_to
  ndims = dimsizes(dims_to_copy)

;---Error checking for "dims_to_copy"
  if(any(dims_to_copy.lt.0.or.dims_to_copy.gt.dimsizes(dimf))) then
    print("Error: copy_VarCoords_n: invalid dimensions to copy from")
    return
  end if

;---Error checking for rank
;;  if(rto.gt.rfrom) then
;;    print("Error: copy_VarCoords_n: arrays must be same rank, or second array must be subset in size of first array.")
;;    return
;;  end if

  dName = getvardims(var_from)

  ito = 0     ; leftmost dimension of var_to array
  if (.not.all(ismissing(dName))) then
    do i = 0,rfrom-1
      if(any(i.eq.dims_to_copy)) then
        ifrom = i
        if (.not.ismissing(dName(ifrom)).and.dimf(ifrom).eq.dimt(ito)) then  
          var_to!ito = var_from!ifrom
          if(iscoord(var_from,var_from!ifrom)) then
            if(dimf(ifrom).eq.dimt(ito)) then
              var_to&$var_to!ito$ = var_from&$var_from!ifrom$
            else
              print("Error: copy_VarCoords_n: dimension being copied must be the same size")
            end if
          end if
        end if
        ito = ito + 1
      else
        if(rfrom.eq.rto) then
          ito = ito + 1
        end if
      end if
    end  do
  end if
end

;************************************************************
; M. Haley
; Copy all but indicated coordinate variables from two arrays
;    of the same size, or from a larger array to a smaller 
;    array that is a subset in size. 
;
; Dec 30 2001:
;    This function used to be two different functions: 
;    "copy_VarCoords_n" and "copy_VarCoords_not_n". I
;    decided to combine them into one function called 
;    "copy_VarCoords_not_n".
;
; This function *used* to make the assumption that the
;    "dims_to_skip" dimensions were consecutive and 
;    increasing. This is no longer necessary.  The 
;    "dims_to_copy" must represent valid dimensions in 
;    the "var_from" array.
;
; Note: in the special case where the rank of the two arrays are
;  the same, then the assumption that the dimensions to skip are
;  in the same dimension locations in both arrays. Otherwise,
;  coordinates are copied to the "var_to" variable starting
;  with the leftmost dimension and going to the next one, etc.
;
undef ("copy_VarCoords_not_n")
procedure copy_VarCoords_not_n(var_from,var_to,dims_to_skip)
local dimt, dimf, rfrom, rto, i, ifrom, ito, dName, ndims, dims
begin                      
  dimf  = dimsizes(var_from)            
  dimt  = dimsizes(var_to)
  rfrom = dimsizes(dimf)      ; rank of var_from
  rto   = dimsizes(dimt)      ; rank of var_to
  ndims = dimsizes(dims_to_skip)

;---Error checking for "dims_to_skip"
  if(any(dims_to_skip.lt.0.or.dims_to_skip.gt.dimsizes(dimf))) then
    print("Error: copy_VarCoords_not_n: invalid dimensions to skip")
    return
  end if

;---Error checking for rank
  if((rfrom-ndims).gt.rto) then
    print("Error: copy_VarCoords_not_n: you are not skipping enough dimensions.")
    return
  end if

  dName = getvardims(var_from)

  ito = 0     ; leftmost dimension of var_to array
  if (.not.all(ismissing(dName))) then
    do i = 0,rfrom-1
      if(.not.any(i.eq.dims_to_skip)) then
        ifrom = i
        if (.not.ismissing(dName(ifrom)).and.dimf(ifrom).eq.dimt(ito)) then  
          var_to!ito = var_from!ifrom
          if(iscoord(var_from,var_from!ifrom))
            var_to&$var_to!ito$ = var_from&$var_from!ifrom$
          end if
        end if
        ito = ito + 1
      else
        if(rfrom.eq.rto) then
          ito = ito + 1
        end if
      end if
    end  do
  end if
end

;==============================
undef("copy_VarAtts")
procedure copy_VarAtts(var_from,var_to)    
local att_names, i, toType, fromFillValue, fromFillType
begin                                       
    att_names =getvaratts(var_from);

    if(.not.all(ismissing(att_names))) then

            ; copy all attributes except _FillValue and missing_value
            ; use := cuz shape/type may differ if var_to had previous attributes

       do i = 0,dimsizes(att_names)-1
          if (.not.(att_names(i).eq."_FillValue" .or. att_names(i).eq."missing_value")) then
              var_to@$att_names(i)$ := var_from@$att_names(i)$  
          end if
       end do

            ; handle _FillValue and missing_value

       toType        = typeof(var_to)

       if (any(att_names.eq."_FillValue")) then    ; associated with 'var_from'
           fromFillValue = var_from@_FillValue 
           fromFillType  = typeof(var_from@_FillValue) 

           if (fromFillType.eq.toType) then
               var_to@_FillValue = fromFillValue
           else
               var_to@_FillValue_original := fromFillValue 
               var_to@_FillValue := totype(fromFillValue, toType)
           end if
       end if             

       if (any(att_names.eq."missing_value")) then    ; associated with 'var_from'
           fromMissValue = var_from@missing_value
           fromMissType  = typeof(var_from@missing_value) 

           if (fromMissType.eq.toType) then
               var_to@missing_value = fromMissValue
           else
               var_to@missing_value_original := fromMissValue 
               var_to@missing_value := totype(fromMissValue, toType)
           end if
       end if             
    end if               
end
;======================================================================
; This is the modified version of copy_VarAtts originaly developed by
; Dennis Shea.
;
; Jan 12, 2012
; This function was originally developed for use in the ESMF software,
; and moved to contributed.ncl.
;======================================================================
undef("copy_VarAtts_except")
procedure copy_VarAtts_except(var_from,var_to, Except [*]:string)    
local att_names, i
begin                                       
    att_names = getvaratts(var_from);
    if(.not.all(ismissing(att_names)))
        do i = 0,dimsizes(att_names)-1
            if (.not.any(att_names(i).eq.Except)) then
                if (isatt(var_to,att_names(i))) then
                    delete(var_to@$att_names(i)$)  ; var_from att may be diff size/type
                end if
                var_to@$att_names(i)$ = var_from@$att_names(i)$
            end if
        end do
    end if
end     ; of copy_VarAtts_except

;***********************************************
; D. Shea
; delete one or more attributes of a variable
; This checks to see if the attribute exists and
; if it does, it deletes it.
;
; Sample usage:
;  x = 0
;  x@apple = 5
;  x@orange = 81.
;  x@peach  = (/ "a", "b"/)
;
;  delete_VarAtts(x, "apple") )
;  delete_VarAtts(x, (/ "apple", "peach"/) )
;
;  x@orange remains
;***********************************************
undef("delete_VarAtts")
procedure delete_VarAtts (x, ATTS)
local n, atts, nAtts
begin
  typeATTS = typeof(ATTS)
  if (.not.(typeATTS.eq."string" .or. typeATTS.eq."integer")) then
      print("delete_VarAtts: argument must be string or integer: type="+typeATTS)     
      print("delete_VarAtts: nothing deleted")     
      return
  end if

  if (typeATTS.eq."integer") then
      atts = getvaratts(x)
  end if

  if (typeof(ATTS).eq."string") then
      atts = ATTS
  end if

  nAtts = dimsizes(atts)
  do n=0,nAtts-1
     if (isatt(x, atts(n)) ) then
         delete(x@$atts(n)$)
     else
         print("delete_VarAtts: attribute="+atts(n)+" does not exist")
     end if
  end do
end

;****************************************
; D. Shea
; Basically a function version of copy_VarAtts which is a procedure
; copy attributes associated with "var_from" to "var_to" and
; return as a new variable.

; xNew = merge_VarAtts( x2, x1)
;        will result in attributes associated with x2 to be
;        added to those associated with x1. If duplictae
;        attributes exist those associated with x2 will
;        replace those associated with x1.

undef("merge_VarAtts")
function merge_VarAtts(var_from,var_to)    
local att_names, i, vNew

begin                                       
  vNew = var_to
  att_names =getvaratts(var_from);
  if(.not.all(ismissing(att_names)))
      do i = 0,dimsizes(att_names)-1
         if (isatt(vNew,att_names(i))) then
             delete(vNew@$att_names(i)$)  ; var_from att may be diff size/type
         end if
	 vNew@$att_names(i)$ = var_from@$att_names(i)$
      end do
  end if
  return(vNew)
end

;***************************************************************
; S. Murphy
; copyatt is very similar to the above two functions, except that
; the the variables do not have to be the same dimension. This can be
; used on variables that have been operated on by a dim_* function.
; It also copies both the attributes and the coordinate variables.

undef("copyatt")
procedure copyatt(var_to,var_from) 
local n, att_names, i, dName, rankto, natts 
begin

  dName = getvardims(var_from)               ; Oct 18, 2005
  rankto= dimsizes(dimsizes(var_to))
                                             ; coordinates must have names
  if (.not.all(ismissing(dName))) then
      do n=0,rankto-1   
         if (.not.ismissing(dName(n))) then   ; Oct 18, 2005
             var_to!n = var_from!n
             if (iscoord(var_from,var_from!n))then
                 var_to&$var_to!n$ = var_from&$var_from!n$
             end if
         end if 
      end do
  end if
;
; copy variable attributes
;

  copy_VarAtts(var_from,var_to)    ; DJS 20 Jan 2005

;;att_names = getvaratts(var_from) ; This was not quite right
;;natts     = dimsizes(att_names)
;;if(.not.all(ismissing(att_names)))
;;  do i=0,natts-1
;;     if(isatt(var_from,"_FillValue"))then
;;        if(typeof(var_from@_FillValue).eq."double"  .and.\
;;           typeof(var_to).eq."float")then
;;           var_to@_FillValue = doubletofloat(var_from@_FillValue)
;;        else
;;           if (isatt(var_to,att_names(i))) then  ; added 10 Mar 2003 [DJS]
;;               delete(var_to@$att_names(i)$)  
;;           end if
;;           var_to@$att_names(i)$ = var_from@$att_names(i)$
;;        end if
;;      end if
;;  end do
;;end if

end
;****************************************
; D. Shea
; New entry: better name than "copyatt" 
; also input arguments are the same as other routines
  undef ("copy_VarMeta" )
  procedure copy_VarMeta (var_from,var_to)
  begin
    copyatt(var_to, var_from)
  end
;************************************************************
; D. Shea
; called internally: add coordinate variables to a variable
; which has had an extra left dimension added.

undef("component_copy_VarCoords")
procedure component_copy_VarCoords (x, y)
local rankx, dimy, i, dName
begin
  rankx = dimsizes(dimsizes(x))
  dimy  = dimsizes(y)

  y!0 = "component"
  y&component = ispan(0, dimy(0)-1, 1)

  dName = getvardims( x )

  do i=0,rankx-1
     if (.not.ismissing(dName(i))) then
         y!(i+1) = x!i
         if(iscoord(x,x!i)) then
            y&$x!i$ = x&$x!i$
         end if
     end if
  end  do
end

;***************************************************************
; D. Shea   
; Determine the number of rows (ie, number of records, lines)
; in an ascii file
; This approach was suggested by: 
; From: "Lunde, Bruce N CIV NAVOCEANO, NP1" <bruce.lunde@navy.mil>
;
; Usage:   nrow = numAsciiRow ("/my/path/ascii_file") 

undef ("numAsciiRow")
function numAsciiRow (fNam:string)

local filString, nrow, nrow_s
begin
 ;filString = asciiread ( fNam, -1, "string")
 ;nrow      = dimsizes(filString)

  nrow_s    = systemfunc("'wc' -l " + fNam +" | awk '{print $1}'" )

  nrow      = tointeger( nrow_s )
  return (nrow)
end

;***************************************************************
; D. Shea   
; Determine the number of columns in an ascii file
; This assumes that all rows have the same number of columns
;      and the columns are separated by spaces.
;
; This approach was suggested by: 
; Date: Thu, 7 Dec 2006 11:39:37 -0800
; From: "Lunde, Bruce N CIV NAVOCEANO, NP1" <bruce.lunde@navy.mil>
;
; It replaced the original version which read the entire file.
;
; Usage:   ncol = numAsciiCol ("/my/path/ascii_file") 

undef ("numAsciiCol")
function numAsciiCol (fNam:string)
begin
  ncol = stringtointeger( systemfunc("head -1 "+fNam+" | wc -w") )
  return(ncol)
end
;*******************************************************************************
; HISTORY:
;   08-02-05 Bruce Lunde. 
;   08-02-06 Bruce Lunde. Generalized to awk versus gawk. Added option "start"
;     for the "every" option, to allow starting the selecting of every Nth
;     line starting with line 1 (opt@start=1). Otherwise, the first line
;     selected is the Nth line in the file.
; NOTES:
;   * Set opt=True and add the following attributes:
;       * opt@every=N ... To return every Nth line.
;       * opt@line1,opt@line2 ... To print a range of lines (opt@line2 optional,
;           defaults to End-of-File).
;       *opt@list ... To print a list of line numbers. First line of file is
;          numbered line 1. Input list is a 1D integer array.
;   * Option priority is (1) line1,line2 (2) every (3) list
;*******************************************************************************

undef("extractAsciiRows")
function extractAsciiRows(fName:string,OPT:logical)
local AOK, command, DBLQT, ii, ilist, numline, opt
begin

  if (OPT) then
      opt = OPT
  else
      return( asciiread(fName, -1, "string") )
  end if

  DBLQT = inttochar(34)
  AOK   = False
  if( opt )then
    if( isatt(opt,"list") )then
      AOK = True
;;; print(1)
      command = "awk -v LIST=" + DBLQT
      ilist = opt@list
      numline = dimsizes(ilist)
      do ii=0,numline-1
        command = command + ilist(ii) + " "
      end do
      command = command + DBLQT + " 'BEGIN{split(LIST,INDX); II=1}"
      command = command + "{if( NR == INDX[II] ){print $0; II=II+1}}' " 
      command = command + fName
    end if

    if( isatt(opt,"every") )then
      AOK = True
      START = "0"
      if( isatt(opt,"start") )then
        if( opt@start .eq. 1 )then
          START = "1"
        end if
      end if
;;; print(2)
      command = "awk '(NR % " + opt@every + ") == " + START + "' " + fName 
    end if

;.. NOTE: Should check for line1 <= line2
    if( isatt(opt,"line1") )then
      AOK = True
;;; print(3)
      if( isatt(opt,"line2") )then
        command = "awk 'NR==" + opt@line1 + ",NR==" + opt@line2
      else
        command = "awk 'NR>=" + opt@line1
      end if
      command = command + "' " + fName
    end if
;;; print("command = "+command)
  end if

  if( AOK )then
    return( systemfunc(command) )
  else
    return( "ERROR" )
  end if
end ; END extractAsciiRows

;***************************************************************
; D. Shea   
; There is no NCL built-in function to get the dimension names
; +++++++ THIS HAS CHANGED ++++++++++++
;         getvardims replaces this function
; +++++++++++++++++++++++++++++++++++++
; of a variable in memory. There is one for file variables 
; [getfilevardims] but not for a variable in memory. Dunno why! 
; Usage:   dimNames = getVarDimNames (x)

undef ("getVarDimNames")
function getVarDimNames (x)

;local dimx, rank, n, dimNames
begin
 ;dimx = dimsizes(x)
 ;rank = dimsizes(dimx)

 ;dimNames = new ( rank, "string", "missing")

 ;do n=0,rank-1
 ;   if (.not.ismissing(x!n)) then
 ;       dimNames(n) = x!n
 ;   end if
 ;end do

 ;return (dimNames)

  return (getvardims(x))
end

;*************************************************************************
; D. Shea
; print Min and Max of a numeric variable using long_name [if present]
; Haley
; Added units [if present] in 6.4.0
;
undef("printMinMax")
procedure printMinMax (x:numeric,optSpace:logical)  
;  Usage:   printMinMax (T,True)                   
begin                                             
          ; attribute names to check             
   vLongName = (/"long_name", "description", "standard_name" /)
   vUnits    = get_valid_units()
   long_name = ""        
   units     = ""
   do n=0,dimsizes(vLongName)-1            
      if (isatt(x,vLongName(n))) then     
          long_name = x@$vLongName(n)$   
          break
      end if
   end do
; Don't bother with units if long_name doesn't exist
   if(long_name.ne."") then
     do n=0,dimsizes(vUnits)-1            
        if (isatt(x,vUnits(n))) then     
            units = x@$vUnits(n)$   
            break
        end if
     end do
   end if
        
   if (optSpace) then
       print (" ")  
   end if          
                  
   if (long_name.ne."") then
      if(units.ne."") then 
        print (long_name+ " ("+units+")" + " : min="+min(x)+"   max="+max(x)) 
      else
        print (long_name+ " : min="+min(x)+"   max="+max(x)) 
      end if
   else 
       print ("min="+min(x)+"   max="+max(x))
   end if 
end

; *****************************************************************
; D. Shea
; Generate gaussian latitudes and meta data 
; nlat = 64
; lat  = latGau (nlat, "lat", "latitude", "degrees_north")
;
; set nlat@double = True if double precision is desired

undef ("latGau")
function latGau (nlat, dimName:string, longName:string, units:string)
local gau_info, lat
begin
  if (isatt(nlat,"double") .and. nlat@double) then
      gau_info  = gaus(nlat/2)         
  else
      gau_info  = doubletofloat(gaus(nlat/2))         
  end if

  lat           = gau_info(:,0)       ; lat values
  lat!0         = dimName             ; name the dimension
  lat@long_name = longName            
  lat@units     = units
  lat&$dimName$ = lat                 ; coordinate variable
  return (lat)
end

; *****************************************************************
; D. Shea
; Generate gaussian weights and meta data 
; nlat = 64
; gwt  = latGauWgt (nlat, "lat", "gaussian weights", "dimension_less")
; gwt  = latGauWgt (nlat, "lat", "gaussian weights", "")
;
; set nlat@double = True if double precision is desired

undef ("latGauWgt")
function latGauWgt(nlat,dimName:string,longName:string,units:string)
local gau_info, gwt
begin        
  if (isatt(nlat,"double") .and. nlat@double) then
      gau_info  = gaus(nlat/2)         
  else
      gau_info  = doubletofloat(gaus(nlat/2))         
  end if

  gwt           = gau_info(:,1)       ; gaussian wgt values
  gwt!0         = dimName             ; name the dimension
  gwt@long_name = longName            
  if (units.ne."")then
      gwt@units     = units
  end if
  gwt&$dimName$ = gau_info(:,0)       ; lat to named dimension
  return (gwt)
end

; *****************************************************************
; Mark Stevens
; normalize the cosine wgts so that the sum is 2.0
; just like gaussian wgts

undef("NormCosWgtGlobe")
function NormCosWgtGlobe (lat:numeric)
local deg_to_rad, wgt, tsum, nwgt
begin

 if (typeof(lat).eq."double") then
     one = 1.0d
     two = 2.0d
     con = 180.0d
 else
     one = 1.0
     two = 2.0
     con = 180.0
 end if

 deg_to_rad = acos(-one)/con
 wgt  = lat
 wgt  = cos(lat*deg_to_rad)
 tsum = sum(wgt)
 nwgt = wgt                ; copy coordinates
 nwgt = (/two*wgt/tsum/)    
 nwgt@long_name = "normalized cosine weights"
 nwgt@units     = "dimensionless"
 return(nwgt)             
end

; *****************************************************************
; D. Shea
; Generate longitudes for a Fixed global grid
; mlon = 128
; lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
; lon will run from 0->"whatever"
;
; If u want the initial lon to be -180., then upon return
;          lon = (/ lon - 180. /)  ; subtract 180 from all values 
;          lon&lon = lon           ; make coord
;
; set mlon@double = True if double precision is desired

undef ("lonGlobeF")
function lonGlobeF(mlon,dimName:string,longName:string,units:string)
local dlon, lon
begin
  if (isatt(mlon,"double") .and. mlon@double) then
      dlon      = new ( 1, "double")
  else
      dlon      = new ( 1, "float")
  end if
  delete (dlon@_FillValue)

  dlon          = 360./mlon           ; output lon
  lon           = ispan ( 0,mlon-1,1 )*dlon
  lon!0         = dimName
  lon@long_name = longName
  lon@units     = units
  lon&$dimName$ = lon
  return (lon)
end

; *****************************************************************
; D. Shea
; Generate longitudes for a Fixed-Offset global grid
; Example: lon = lonGlobeFo (72, "lon", "longitude", "degrees_east")
;          lon will run from -> 2.5 to 357.5 in the above example
;
; If u want the initial lon to be, say, -177.5,, then upon return
;          lon = (/ lon - 180. /)  ; subtract 180 from all values 
;          lon&lon = lon           ; make coord
;
; set mlon@double = True if double precision is desired

undef ("lonGlobeFo")
function lonGlobeFo(mlon,dimName:string,longName:string,units:string)
local dlon, offset, lon
begin
  if (isatt(mlon,"double") .and. mlon@double) then
      dlon      = new ( 1, "double")
  else
      dlon      = new ( 1, "float")
  end if
  delete (dlon@_FillValue)

  dlon          = 360./mlon           ; output lon
  offset        = dlon*0.5
  lon           = ispan ( 0,mlon-1,1 )*dlon + offset
  lon!0         = dimName
  lon@long_name = longName
  lon@units     = units
  lon&$dimName$ = lon
  return (lon)
end

; *****************************************************************
; D. Shea
; Internal: made for regridding routines with _Wrap  
;           Will cause 0=>360 to return -180-to-180 via
;           lon = (/ lon - 180. /)  ; subtract 180 from all values 
;           lon&lon = lon           ; make coord
; check the initial LON location: if lon(0) < 0 assume
; start is at Date Line
; note: x&lon is different from lonNew

undef ("lonGM2DateLine")
procedure lonGM2DateLine (xOld, lonNew) 
local namDim, dim_x, nDim 
begin
  dim_x = dimsizes(xOld)
  nDim  = dimsizes(dim_x) 
  if (.not.ismissing(xOld!(nDim-1)) .and. iscoord(xOld,xOld!(nDim-1))) then
      namDim = xOld!(nDim-1)
      if (xOld&$namDim$(0).lt.0.) then   ; is 1st value < 0.0
          lonNew = (/ lonNew-180. /)     ; start lonNew at Date Line
      end if
  end if
end
; *****************************************************************
; D.Shea
; Change case of each character in a string to the opposite case

; This function was deprecated in V5.1.1 and replaced with
; the built-in function "str_switch".

; input strings may be scalar [sample="An apple a day"]
;                   or 1D     [sample=new( 10, string) ]
;                              sample(0)="apple", (1)="ARTICHOKE", ...
; Usage:   sample = changeCaseChar (sample)  
;          sample = changeCaseChar ("apple")  ==> APPLE  
;          sample = changeCaseChar ("APPLE")  ==> apple  
;          sample = changeCaseChar ("ApplE")  ==> aPPLe  

undef ("changeCaseChar")
function changeCaseChar (x:string)

begin
;  print("changeCaseChar: this function has been deprecated.")
;  print("                Will use str_switch.")
  return(str_switch(x))
end
; *****************************************************************
; D.Shea
; Change case: (1) "low" [change all to lower case] 
;              (2) "up"  [change all to upper case]

; This function was deprecated in V5.1.1 and replaced with
; the built-in functions "str_lower" and "str_upper".

; input strings may be scalar [sample="An apple a day"]
;                   or 1D     [sample=new( 10, string) ]
;                              sample(0)="apple", (1)="ARTICHOKE", ...
; Usage:   sample = changeCase (sample, "up")   ; all upper case  
;          sample = changeCase (sample, "low")  ; all lower case

undef ("changeCase")
function changeCase (x:string, opt:string)

begin
  print("changeCase: this function has been deprecated.")
  print("            Will use str_lower or str_upper.")
  if (opt.eq."low") then
    return (str_lower(x))
  else
    return (str_upper(x))
  end if
end
; *****************************************************
; D. Shea
; trim trailing (rightmost) blanks from one or more strings

; This function was deprecated in V5.1.1 and replaced with
; the built-in function "str_right_strip".

;
undef("trimBlankRight")
function trimBlankRight (s[*]:string)
begin
  print("trimBlankRight: this function has been deprecated.")
  print("                Will use str_right_strip.")
  return(str_right_strip(s))
end
; *****************************************************************
; D. Shea
; Generate latitudes for a Fixed global grid
; nlat= 73
; lat = latGlobeF (nlat, "lat", "latitude", "degrees_north")
; lat will run from -90 -> +90
;
; set nlat@double = True if double precision is desired
;
undef ("latGlobeF")
function latGlobeF(nlat,dimName:string,longName:string,units:string)
local dlat, lat
begin
  if (isatt(nlat,"double") .and. nlat@double) then
      dlat      = new ( 1, "double" )
  else
      dlat      = new ( 1, "float" )
  end if
  delete (dlat@_FillValue)

  dlat          = 180./(nlat-1)              ; output lat
  lat           = ispan ( 0,nlat-1,1 )*dlat - 90.
  lat!0         = dimName
  lat@long_name = longName
  lat@units     = units
  lat&$dimName$ = lat
  return (lat)
end
; *****************************************************************
; D. Shea
; Generate latitudes for a Fixed-Offset global grid
; Example: nlat= 72
;          lat = latGlobeFo (nlat, "lat", "latitude", "degrees_north")
;          lat will run from -> -87.5 to 87.5 in the above example
;
; set nlat@double = True if double precision is desired
;
undef ("latGlobeFo")
function latGlobeFo(nlat,dimName:string,longName:string,units:string)
local dlat, offset, lat
begin
  if (isatt(nlat,"double") .and. nlat@double) then
      dlat      = new ( 1, "double" )
  else
      dlat      = new ( 1, "float" )
  end if
  delete (dlat@_FillValue)

  dlat          = 180./nlat           ; output lat
  offset        = dlat*0.5
  lat           = ispan ( 0,nlat-1,1 )*dlat - 90. + offset
  lat!0         = dimName
  lat@long_name = longName
  lat@units     = units
  lat&$dimName$ = lat
  return (lat)
end
; *******************************************************************
; D. Shea
; Assign all named dimensions to a variable along with the longname 
; and units.  It will not assigne coordinate variables.

undef("nameDim")
function nameDim(x,dimNames[*]:string,longName:string,units:string)

;usage:     z = nameDim (z,  (/a string name for each dim/), longName, units)
;usage: 4D: u = nameDim (u,  (/"time","lat","lev","lon"/), "zonal wind", "m/s")
;usage: 4D: u = nameDim (u,  (/"time","lev","lat","lon"/), "zonal wind", "m/s")
;usage: 3D: t = nameDim (x,  (/"time","lat","lon"/) , "temperature", "K")
;usage: 1D: lat = nameDim (lat,  "lat" , "latitude", "degrees_north")
;usage: 1D: gw  = nameDim ( gw,  "lat" , "gaussian weights", "")

local dimN, rank, n
begin
  dimN = dimsizes(dimNames)    ; number of names in dimNames
  rank = dimsizes(dimsizes(x)) ; number of dimension of "x"
  
  if (dimN.eq.rank) then
      do n=0,dimN-1
         x!n = dimNames(n)       ; eg: x!0 = "time"
      end do

      x@long_name = longName
      x@units     = units
  else
      print ("NCL: nameDim: #of dimension names (="+dimN+ \
             ") does not equal the rank of the input array ("+rank+")")
  end if
  
  return (x)
end

;********************************************************************
; D. Shea
; set a _FillValue "automatically" 
; If a variable has a "_FillValue" use it, if not "missing_value" etc.
; if none on file, then return "No_FillValue"  
; This function should be used within the "new" statement.
; example of inline use: var = new(dimsizes(x),typeof(x),getFillValue(x))

undef("getFillValue")
function getFillValue (x)
local FillValue
begin
  if (isatt(x,"_FillValue")) then
      FillValue = x@_FillValue
  else
      if (isatt(x,"missing_value")) then
           FillValue = x@missing_value
      else
           FillValue = "No_FillValue"
      end if
  end if

   return (FillValue)
end
;********************************************************************
; D. Shea
; set a numeric _FillValue "automatically" 
; If a variable has a "_FillValue" use it, if not "missing_value" etc.
; if none on file, then set default by variable type 
; example of inline use: var = new(dimsizes(x),typeof(x),getFillValue(x))

undef("getVarFillValue")
function getVarFillValue (x)
local FillValue
begin
  if (isatt(x,"_FillValue")) then
      return( x@_FillValue )
  else
      if (isatt(x,"missing_value")) then
           return( x@missing_value )
      else
           return( default_fillvalue(typeof(x)) )
      end if
  end if
end

; *****************************************************************
; Calculate wind speed
; *****************************************************************
undef ("wind_speed")
function wind_speed(u:numeric, v:numeric)
local wspd
begin
 if (.not.all(dimsizes(u).eq.dimsizes(v))) then
     print("wind_speed: dimension size mismatch")
     print("         u: "+dimsizes(u))
     print("         v: "+dimsizes(v))
     exit
 end if
  
  wspd = sqrt(u^2 + v^2)
  wspd@long_name = "wind speed"
  if (isatt(u,"units")) then
      wspd@units = u@units
  end if
  copy_VarCoords(u, wspd)
  return(wspd)
end
; *****************************************************************
; Calculate meteorological wind direction
; *****************************************************************
undef ("wind_direction")
function wind_direction(u:numeric, v:numeric, opt:integer)
; meteorological wind direction. 

; This is *** over-kill ***. 
; Only need: wdir=atan2(u,v)*radi + 180  [or 270-atan2(v,u)*radii) ]

local dimu, dimv, ranku, rankv, wdir, radi, con, wcrit, wmsg, zero, dimu, utype, uvtype
begin

  dimu  = dimsizes(u)
  ranku = dimsizes(dimu)
  dimv  = dimsizes(v)
  rankv = dimsizes(dimv)

  if (ranku.ne.ranku) then
      print("wind_direction: FATAL: rank mismatch: ranku="+ranku+" rankv="+rankv)
      exit
  end if

  if (.not.all(dimu.eq.dimv))
      print("wind_direction: FATAL: array size mismatch")
      print("          dimu="+dimu)
      print("          dimv="+dimv)
      exit
  end if

  utype = typeof(u)
  if (.not.(utype.eq."double" .or. utype.eq."float" .or. \
            utype.eq."integer" )) then
      print("wind_direction: illegal numeric type: "+utype)
      exit
  end if

  if (utype.eq."double") then
      uvtype = "double"
      zero   = 0.0d
    ;;con    = 270.0d
      con    = 180.0d
      wcrit  = 360d0
  else
      uvtype = "float"
      zero   = 0.0
    ;;con    = 270.0
      con    = 180.0
      wcrit  = 360.0
  end if

  if (isatt(u,"_FillValue")) then
      uvmsg = getFillValue(u)
  else 
      if (isatt(v,"_FillValue")) then
          uvmsg = getFillValue(v)
      else
          uvmsg = "No_FillValue"
      end if
  end if

  radi  = get_r2d(uvtype)                 ; radians to degrees
  wdir  = new (dimu, uvtype, uvmsg) 

;;wdir = (/ mod((con - atan2(v,u)*radi),360)  /)
  wdir = (/ con + atan2(u,v)*radi /)
  wdir = where(wdir.ge.wcrit, zero, wdir)  ; force 360 "north winds" to 0
                          
  if (typeof(opt).eq."integer" .and. opt.eq.0) then 
      cdir = zero                          ; calm 0.0
      wdir = where (u.eq.zero .and. v.eq.zero, cdir, wdir)
  end if
                          
  if (typeof(opt).eq."integer" .and. opt.eq.1) then 
      cdir = wdir@_FillValue               ; calm 0.0->_FillValue
      wdir = where (u.eq.zero .and. v.eq.zero, cdir, wdir)
  end if

  if (typeof(opt).eq."float" .or. typeof(opt).eq."double") then  
                                           ; calm 0.0->user_value
      wdir = where (u.eq.zero .and. v.eq.zero, opt, wdir)
  end if
  
  wdir@long_name = "wind direction (meteorological)"
  if (isatt(wdir,"units")) then
      delete(wdir@units)
  end if

  copy_VarCoords(u, wdir)

  return(wdir)
end
; *****************************************************************
; Calculate meteorological wind components
; *****************************************************************
undef ("wind_component")
function wind_component(wspd:numeric, wdir:numeric, opt:integer)
local rad, uvmsg, uveps, uvzero, dimw, rankw, dimd, rankd, dimuv, uv, uvtype
begin

  dimw = dimsizes(wspd)
  rankw= dimsizes(dimw)
  dimd = dimsizes(wdir)
  rankd= dimsizes(dimd)

  if (rankw.ne.rankd) then
      print("wind_component: FATAL: rank mismatch: rankw="+rankw+" rankd="+rankd)
      exit
  end if

  if (.not.all(dimw.eq.dimd))
      print("wind_component: FATAL: array size mismatch")
      print("          dimw="+dimw)                        
      print("          dimd="+dimd)                        
      exit
  end if

  if (typeof(wspd).eq."double" .or. typeof(wdir).eq."double" ) then  
      uvtype= "double"
      uveps = 1.0d-5
      uvzero= 0.0d0
  else
      uvtype= "float"
      uveps = 1.0e-5
      uvzero= 0.0
  end if
  rad   = get_d2r(uvtype)

  if (isatt(wspd,"_FillValue")) then
      uvmsg = getFillValue(wspd)
  else 
      if (isatt(wdir,"_FillValue")) then
          uvmsg = getFillValue(wdir)
      else
          uvmsg = "No_FillValue"
      end if
  end if

  dimuv= new ( rankw+1, "integer", "No_FillValue")
  dimuv(0)  = 2
  dimuv(1:) = dimw
  uv        = new (dimuv, uvtype, uvmsg) 

  if (rankw.eq.1 .and. dimw(0).eq.1) then
      uv(0,0) = -wspd*sin(wdir*rad)
      uv(1,0) = -wspd*cos(wdir*rad)
      uv      = where(abs(uv).le.uveps, uvzero, uv) ; make near zero => 0
      copy_VarMeta(wspd, uv)
      if (opt.ne.0) then
          return([/uv(0,0), uv(1,0)/])
      end if
  else if (rankw.eq.1) then
      uv(0,:) = -wspd*sin(wdir*rad)
      uv(1,:) = -wspd*cos(wdir*rad)
      uv      = where(abs(uv).le.uveps, uvzero, uv) ; make near zero => 0
      copy_VarMeta(wspd, uv(0,:))
      if (opt.ne.0) then
          return([/uv(0,:), uv(1,:)/])
      end if
  else if (rankw.eq.2) then
      uv(0,:,:) = -wspd*sin(wdir*rad)
      uv(1,:,:) = -wspd*cos(wdir*rad)
      uv        = where(abs(uv).le.uveps, uvzero, uv) ; make near zero => 0
      copy_VarMeta(wspd, uv(0,:,:))
      if (opt.ne.0) then
          return([/uv(0,:,:), uv(1,:,:)/])
      end if
  else if (rankw.eq.3) then
      uv(0,:,:,:) = -wspd*sin(wdir*rad)
      uv(1,:,:,:) = -wspd*cos(wdir*rad)
      uv          = where(abs(uv).le.uveps, uvzero, uv) ; make near zero => 0
      copy_VarMeta(wspd, uv(0,:,:,:))
      if (opt.ne.0) then
          return([/uv(0,:,:,:), uv(1,:,:,:)/])
      end if
  else if (rankw.eq.4) then
      uv(0,:,:,:,:) = -wspd*sin(wdir*rad)
      uv(1,:,:,:,:) = -wspd*cos(wdir*rad)
      uv            = where(abs(uv).le.uveps, uvzero, uv) ; make near zero => 0
      copy_VarMeta(wspd, uv(0,:,:,:,:))
      if (opt.ne.0) then
          return([/uv(0,:,:,:,:), uv(1,:,:,:,:)/])
      end if
  else if (rankw.eq.5) then
      uv(0,:,:,:,:,:) = -wspd*sin(wdir*rad)
      uv(1,:,:,:,:,:) = -wspd*cos(wdir*rad)
      uv              = where(abs(uv).le.uveps, uvzero, uv) ; make near zero => 0
      copy_VarMeta(wspd, uv(0,:,:,:,:,:))
      if (opt.ne.0) then
          return([/uv(0,:,:,:,:,:), uv(1,:,:,:,:,:)/])
      end if

  end if           ; 5
  end if           ; 4
  end if           ; 3
  end if           ; 2
  end if           ; 1
  end if           ; scalar

  uv!0 = "uv"
  uv@long_name = "zonal [0] and meridional [1] wind components"
  return(uv)
end
; *****************************************************************
; Basic wind related quantities and statistics
; *****************************************************************
undef("wind_stats")
function wind_stats(wspd:numeric, wdir:numeric, nDim[*]:integer, opt[1])
;
; Single pass *estimation* of the standard deviation of wind direction:
; ... Wind speed is a scalar
; ... Wind direction is circular quantity: [0..360); including 0 but excluding 360 and above
;     Think discontinuity at 360.
; ... Function 'wind_stats' uses Yamartino'a single-pass method. Yamartino's algorithm is 
;     an approximation within 2% of the theoretical distribution.
; ----
; The standard deviation of wind direction is a measure of lateral turbulence, 
; and can be used to estimate the Pasquill stability. 
; ----
; References:
; Yamartino (1984): Journal of Climate and Applied Meteorology , 23, 1362-1366 
; http://dx.doi.org/10.1175/1520-0450(1984)023<1362:ACOSPE>2.0.CO;2
;
; Turner, D.B. (1986): Comparison of Three Methods ...
; http://journals.ametsoc.org/doi/pdf/10.1175/1520-0450%281986%29025%3C0703%3ACOTMFC%3E2.0.CO%3B2

; Weber, R. (1997): Estimators for the Standard Deviation of Horizontal Wind Direction
; http://journals.ametsoc.org/doi/pdf/10.1175/1520-0450%281997%29036%3C1403%3AEFTSDO%3E2.0.CO%3B2
;
; Wikipedia;
; https://en.wikipedia.org/wiki/Yamartino_method
;
local wspd_avg, wspd_std, wdir_avg, wdir_std \
    ,  d2r, wrad, wsin, wcos, e, wsrd, uv
begin

; wind speed average and standard deviation + meta data

  wspd_avg = dim_avg_n_Wrap   (wspd, nDim) 
  wspd_std = dim_stddev_n_Wrap(wspd, nDim) 

  if (isatt(wspd,"long_name")) then                       ; add meta
      wspd_avg@long_name = "average: "+wspd@long_name
      wspd_avg@info_wspd = "average of wind speeds"
      wspd_std@long_name = "std dev: "+wspd@long_name
      wspd_std@info_wdir = "average of wind speeds"
  else
      wspd_avg@long_name = "average wind speed"
      wspd_std@long_name = "standard deviation of wind speed"
  end if

; wind direction: intermediate quantities (see Ymartino)

  d2r  = get_d2r(wdir)
  wrad = wdir*d2r
  wsin = dim_avg_n(sin(wrad), nDim)                ; avg sin(wdir) 
  wcos = dim_avg_n(cos(wrad), nDim)                ;     cos
  delete(wrad)                                     ; no longer needed

;;mean wind direction: Turner method p707 based on Yamartino

;;wdir_avg = atan2(wsin,wcos)*get_r2d(wspd)        ; average; Yamartino
;;if (wdir_avg.lt.0) then
;;    wdir_avg = wdir_avg + 360
;;end if
;;wdir_avg@long_name = "wind direction: mean"
;;wdir_avg@info      = "Yamartino single-pass method"

; mean wind via:  http://www.ndbc.noaa.gov/wndav.shtml
; (a) compute mean U (avgU) and mean V (avgV)
; (b) compute mean wind speed via mean components (a)
; (c) compute mean wind direction via mean components (a)

  uv   = wind_component(wspd, wdir, 1)             ; return list
                                                   ; explicitly extract: clarity
  u    = uv[0]                                     ; extract u
  v    = uv[1]                                     ;         v
  delete(uv)                                       ; no longer needed

  avgU = dim_avg_n(u, 0)                           ; average u
  avgV = dim_avg_n(v, 0)                           ;         v
  avgW = sqrt(avgU^2 + avgV^2)                     ; NOAA
  dirW = wind_direction(avgU, avgV, 0)             ; 'exact'

; std dev of wind direction (~ within 2% ) ... Yamartino (1984)
  
  e    = sqrt(1-(wsin*wsin+wcos*wcos))
  wdir_std = get_r2d(e)*asin(e)*(1+0.1547*e^3)
  delete(e)                                        ; no longer needed

  wdir_std@long_name = "wind direction: standard deviation"
  wdir_std@info      = "Yamartino single-pass method"
  copy_VarCoords(wspd_avg, wdir_std)

  return ([/wspd_avg, wspd_std,     dirW, wdir_std, avgU, avgV/])  
end
; **************************************************************
; D. Shea
; Loops thru a 1D variable [eg, time] and finds
; the indices which match up with the values of the cvWant 1D array
; e.g. time_want = (/1948, 1957, 1964, 1965, 1989/)
; indWant   = get1Dindex (time, time_want)
; note that the values of cvWant must EXIST in cv

undef("get1Dindex")
function get1Dindex (cv[*],  cvWant[*])
local nWant, indWant, n, indcv, nMult

begin
  nWant   = dimsizes (cvWant)
  indWant = new (nWant, "integer")
  nMult   = 0

  do n=0,nWant-1
     indcv     := ind( cv.eq.cvWant(n) ) ; could be multiple values
     indWant(n) = indcv(0)               ; return the 1st if multiple indcv
     if (dimsizes(indcv).gt.1) then
         nMult = nMult + 1
     end if
  end do

  indWant@index_info = "Out of nWant="+nWant+" : multiple index occurrences="+nMult
  indWant@nMultInd   = nMult

  return (indWant)
end 

; *******************************************************************
; u.utku.turuncoglu@be.itu.edu.tr
; The following procedure to sorts x,y coordinate pair based on
; selected one. The opt controls the behavior of the sort procedure. If the
; value is equal to 1 then, it sorts the x values and match the correct y
; with it. 

undef("sort_xy")
procedure sort_xy(x:numeric, y:numeric, eps:numeric, opt:numeric)

local x_old, y_old, x_ind, y_ind, x_eps, y_eps, i
begin
  ;--- sort x ---
  if (opt .eq. 1) then
    ;--- fix, the arguments of the get1Dindex must unique ---
    x_eps = new(dimsizes(x), typeof(x))
    do i = 0, dimsizes(x)-1
      x_eps(i) = (i+1)*eps
    end do

    ;--- add artifical small number to get unique array ---
    x = x+x_eps

    ;--- sort arrays ---
    x_old = x
    qsort(x)
    x_ind = get1Dindex(x_old, x)
    y = y(x_ind)
  ;--- sort y ---
  else
    ;--- fix, the arguments of the get1Dindex must unique ---
    y_eps = new(dimsizes(y), typeof(y))
    do i = 0, dimsizes(y)-1
      y_eps(i) = (i+1)*eps
    end do

    ;--- add artifical small number to get unique array ---
    y = y+y_eps

    ;--- sort arrays ---
    y_old = y
    qsort(y)
    y_ind = get1Dindex(y_old, y)
    x = x(y_ind)
  end if
end
; **************************************************************
; D. Shea
; Loops thru a 1D variable [eg, time] and finds
; the indices which do *not* match up with the values of the cvExclude

undef("get1Dindex_Exclude")
function get1Dindex_Exclude (cv[*],cvExclude[*])
local nCv, nExclude, cvLocal,n, indTemp
begin

  nExclude = dimsizes (cvExclude)
  nCv      = dimsizes (cv)

  cvLocal  = new( nCv, typeof(cv), getFillValue(cv) )
  cvLocal  = (/ cv /)

  do n=0,nExclude-1
     indTemp = ind( cvExclude(n).eq.cv)
     if (.not.any(ismissing(indTemp)) ) then
         cvLocal(indTemp) = cvLocal@_FillValue         
     end if
     delete(indTemp)
  end do

  return(ind(.not.ismissing(cvLocal)))
end
; **************************************************************
; D. Shea
; 19 Feb 2006
; This was wrong. For historical reasons keep the function but
; invoke get1Dindex_Exclude

undef("get1Dindex_Collapse")
function get1Dindex_Collapse (cv[*],cvWant[*])
begin
  return( get1Dindex_Exclude(cv, cvWant) )
end

;*************************************************************
; From: Dave Allured 
; Date: Thu, 15 Mar 2007 14:00:43 -0600
; "The following is much simpler and more robust" ... than 'closest_val' 
; DA's original name was 'closest_val_AnyOrder'
undef("closest_val")
function closest_val(xVal[1]:snumeric, x:snumeric)
local xAbsDif, xMinVal, iClose
begin
   xAbsDif = abs(xVal-x)
   iClose = minind(xAbsDif)
   return(iClose) ; original doc says "first occurence"
end 

;****************************************************************
; S. Murphy
; Finds the index of the array point closest to the desired value
; e.g. var2ck = 18.382
; values in array2ck = 17.0 17.5 18.0 18.5
; this function would return 3.

undef("closest_val_original")
function closest_val_original(var2ck:numeric,array2ck[*])
local size, narray,loc, test1,test2
begin
  size = dimsizes(array2ck)            ; get size of original array

; make sure array is monotonically increasing

  if (.not.all(array2ck(1:size-1).gt.array2ck(0:size-2)) ) then
      print("closest_val: input array is not monotonically increasing")
      exit
  end if
  

; first check to see if the value exists in the array

  if(any(var2ck.eq.array2ck))then
    return(ind(var2ck.eq.array2ck))
    print(var2ck +"equals a value in the array")
    exit
  end if

; value does not exist in array, so we insert it

  narray = new( (/size + 1/),typeof(array2ck)) ; create new array 

  narray(0:size-1) = array2ck(:)
  narray(size) = var2ck                ; add value to new array
  qsort(narray)                        ; sort the new array

  loc = ind(narray.eq.var2ck)          ; determine the index
;
; var2ck is last value
;
  if(loc.ge.size)then
    return(size)
    exit
  end if
;
; var2ck is first value
;
  if(loc.eq.0)then
    return(loc)
    exit
  end if
;
; somewhere in the middle
;
  test1 = narray(loc) - narray(loc-1)
  test2 = narray(loc+1) - narray(loc)
  if(test1.lt.test2)then
     return(loc-1)
  else
     return(loc) 
  end if
end
; **************************************************************
; D. Shea
; Convert a 1D array to a single string
; e.g. maxYrs = (/1960,1969,1980,1989/)  
; yrString    = oneDtostring (maxYrs)
; yrString    = "1960,1969,1980,1989"

undef("oneDtostring")
function oneDtostring (x[*])
local nx, n, newString
begin
  newString = new ( 1 , string)

  nx = dimsizes(x)
  if (nx.gt.1) then
      newString = x(0) + ","
      do n=1,nx-2
         newString = newString + x(n) + ","
      end do
      newString = newString + x(nx-1)
  else
      newString = x(0) 
  end if
  return (newString)
end
;********************************************************
; S. Murphy
; converts a list of comma deliminated variables passed from
; csh to NCL to an actual array of strings.
; e.g. set vars = ,T,U,V,TS,PRECC, (in csh)
;      setenv nclvars $vars        (to pass to NCL must be and env)
;      vs = getenv("nclvars")      (reads csh variable into NCL)
;      vars = cshstringtolist(vs)  (does conversion)
;      vars = (/"T","U","V","TS","PRECC"/)       

; note the string "list" is now an NCL keyword, so "list" has been
; shortened to "lst"

undef("cshstringtolist")
function cshstringtolist(cshlist:string)
local lst, breaks, nwords, str, i
begin

 lst=stringtochar(cshlist)            ;convert the string to characters
 breaks = ind(lst .eq. inttochar(44)) ;locate the deliminators (a space is 32)
 nwords=dimsizes(breaks)              ;count the deliminators

 str=new((/nwords-1/),string)
 do i=0,nwords-2                      ;pull out the separate strings
    str(i) = chartostring (lst(breaks(i)+1:breaks(i+1)-1))
 end do
 return(str)
end


;*************************************************************
; D. Shea
; use coersion for integer to float 
; get around *irritating* lack of conversion function

undef ("int2flt")
function int2flt(i:integer)
local dimi, fi
begin
   dimi = dimsizes(i)
   if (isatt(i,"_FillValue")) then
     fi = new( dimi, "float", i@_FillValue)
   else
     fi = new( dimi, "float","No_FillValue")
   end if

   fi   = i                    ; values + meta data
   return (fi)
end


;*************************************************************
; D. Shea
; use coersion for integer to float 
; get around *irritating* lack of conversion function

undef ("int2dble")
function int2dble(i:integer)
local dimi, di
begin
  dimi = dimsizes(i)
  di   = new( dimi, "double")
  di   = i                   ; values + meta data
  if (isatt(i,"_FillValue")) then
      di@_FillValue = i@_FillValue
  else
     delete(di@_FillValue)
  end if
  return (di)

end


; *************************************************************
; D. Shea
; use coersion for float to double
; but also make sure that _FillValue is assigned 
;     when missing_value is present
undef("flt2dble")
function flt2dble (xF:float) 
local xD
begin
 xD = new (dimsizes(xF), double, getFillValue(xF))
;if (.not.isatt(xF,"_FillValue")     .and. \
;    .not.isatt(xF,"missing_value")) then
;    delete(xD@_FillValue)     ; no _FillValue or missing_value on input
;end if
 xD = xF  ; variable-to-variable transfer [all meta data copied]

 if (isatt(xD,"missing_value") .and. \
     typeof(xD@missing_value).ne."double") then
     delete(xD@missing_value)         
     xD@missing_value = xD@_FillValue   ; add missing_value
 end if

 return(xD)
end

; *************************************************************
; D. Shea
; Wrapper for NCL function: doubletofloat
; This does the conversion and copies all the attributes
; and coordinate variables [CV] and attributes of the CV.
; This is more complicated than usual beacuse I want
; to convert any double attributes and CVs to float.

undef("dble2flt")
function dble2flt (xD)
local dimx, ndimx, xF, xD_atts, i, j, cv, cvD, cvF, cvD_atts
begin
 if (typeof(xD).eq."float") then
     if (isatt(xD,"long_name")) then
         print("dble2flt: input variable is already type float: "+xD@long_name)
     else
         print("dble2flt: input variable is already type float")
     end if
     return(xD)
 end if
     
 dimx = dimsizes(xD)

 if (isatt(xD,"_FillValue")) then
   xF = new (dimx, float, doubletofloat(xD@_FillValue ) )
 else
   if (isatt(xD,"missing_value")) then
      xF = new (dimx, float, doubletofloat(xD@missing_value) )
   end if
 end if

 xF = doubletofloat (xD)       ; convert values

 xD_atts = getvaratts(xD)      ; copy attributes of input variable
 if (.not.all(ismissing(xD_atts))) then
    do i=0,dimsizes(xD_atts)-1
       if (xD_atts(i).ne."_FillValue") then                   ; done above
           if(typeof(xD@$xD_atts(i)$) .ne. "double" ) then
              xF@$xD_atts(i)$ = xD@$xD_atts(i)$
           else
              xF@$xD_atts(i)$ = doubletofloat(xD@$xD_atts(i)$)
           end if
       end if
    end do
 end if
 delete (xD_atts)
                                  ; add info on operation performed
 xF@typeConversion_op_ncl = "double converted to float"

 ndimx = dimsizes(dimx)           ; number of dimensions
 do i=0,ndimx-1                   ; loop over all dimensions
    if (.not.ismissing(xD!i)) then
        xF!i = xD!i               ; copy dimension name
        if(iscoord(xD,xD!i)) then ; copy coordinate variable [if present]
           cvD = xD&$xD!i$        ; coordinate variable [for convenience]
           if (typeof(cvD).ne."double") then
               xF&$xF!i$ = cvD    ; straight copy
           else
               cvF       = doubletofloat(cvD) ; no attributes copied
               cvD_atts  = getvaratts(cvD)    ; coord var atts
               if (.not.all(ismissing(cvD_atts))) then
                   do j=0,dimsizes(cvD_atts)-1
                      if (typeof(cvD@$cvD_atts(j)$) .ne. "double" ) then
                          cvF@$cvD_atts(j)$  = cvD@$cvD_atts(j)$
                      else                    ; must be double
                          cvF@$cvD_atts(j)$ = doubletofloat( cvD@$cvD_atts(j)$)
                      end if
                   end do
               end if
               xF&$xF!i$ = cvF          ; assign float coord variable
               delete (cvF)
               delete (cvD_atts)
           end if                   
           delete (cvD)
        end if                     
    end if                        
 end do                          

 return (xF)
 
end
;******************************************************************
; D. Shea
; converts shorts to floats using the "scale" and "offset" attributes (if present) 
; Note: the CF and COARDS conventions require that 
; "if both scale_factor and add_offset
; attributes are present, the data are first scaled before the offset is added" 
; This follows these conventions.

undef("short2flt")
function short2flt (xS)
local xF, oNames, sNames, offset, scale, xAtts, nAtts, n
begin
 if (typeof(xS).eq."float") then
     if (isatt(xS,"long_name")) then
         print("short2flt: input variable is already type float: "+xS@long_name)
     else
         print("short2flt: input variable is already type float")
     end if
     print("short2flt: no conversion performed")
     return(xS)
 end if

 if (.not.(typeof(xS).eq."short" .or. typeof(xS).eq."ushort")) then
     if (isatt(xS,"long_name")) then
         print("short2flt: input variable is not type short or ushort: "+xS@long_name)
     else
         print("short2flt: input variable is not type short or ushort")
     end if
     exit
 end if

;xF = new ( dimsizes(xS), float)
 xF = new ( dimsizes(xS), float, 1.e20)  ; 20 Nov 2002
                                     ; added 19 Dec 2002
                                     ; the type stuff added Sept 2003
 if (isatt(xS,"missing_value") .and. .not.isatt(xS,"_FillValue")) then
     type_missing_value = typeof(xS@missing_value)
     if (typeof(xS).eq."short") then
         if (type_missing_value.eq."short") then
             xS@_FillValue = xS@missing_value
         end if
         if (type_missing_value.eq."ushort") then
             xS@_FillValue = toshort(xS@missing_value)
         end if
         if (type_missing_value.eq."integer") then
             xS@_FillValue = toshort(xS@missing_value)
         end if
     end if

     if (typeof(xS).eq."ushort") then
         if (type_missing_value.eq."ushort") then
             xS@_FillValue = xS@missing_value
         end if
         if (type_missing_value.eq."short") then
             xS@_FillValue = toushort(xS@missing_value)
         end if
         if (type_missing_value.eq."integer") then
             xS@_FillValue = toushort(xS@missing_value)
         end if
     end if
 end if   ; end of assigning _FillValue
 
 copy_VarAtts   (xS, xF)
 copy_VarCoords (xS, xF)

          ; should data be 'scaled' and/or 'offset' ?

          ; names to check
 oNames = (/"add_offset", "offset", "OFFSET", "Offset", "_offset" \
           ,"Intercept", "intercept", "add_off", "ADD_OFF"/)
 sNames = (/"scale", "SCALE", "Scale", "_scale", "scale_factor", "factor"    \
           ,"Scale_factor", "Slope" , "slope", "ScaleFactor", "Scale_Factor" \
           ,"SCALING_FACTOR" /)

 offset = 0.0   ; establish as type float
 scale  = 1.0

 xAtts  = getvaratts(xS)
 nAtts  = dimsizes(xAtts)
 
 do n=0,nAtts-1
    if (any(oNames.eq.xAtts(n))) then
        if (typeof(xS@$xAtts(n)$).eq."float") then
            offset = xS@$xAtts(n)$
        else
            if (typeof(xS@$xAtts(n)$).eq."double") then
                offset = doubletofloat(xS@$xAtts(n)$)
            end if
            if (typeof(xS@$xAtts(n)$).eq."string") then
                offset = stringtofloat(xS@$xAtts(n)$)
            end if
        end if
        delete(xF@$xAtts(n)$)  ; xF will no longer have offset att
        break
    end if
 end do
 
 do n=0,nAtts-1
    if (any(sNames.eq.xAtts(n))) then
        if (typeof(xS@$xAtts(n)$).eq."float") then
            scale  = xS@$xAtts(n)$
        else
            if (typeof(xS@$xAtts(n)$).eq."double") then
                scale  = doubletofloat(xS@$xAtts(n)$)
            end if
            if (typeof(xS@$xAtts(n)$).eq."string") then
                scale  = stringtofloat(xS@$xAtts(n)$)
            end if
        end if
        delete(xF@$xAtts(n)$)  ; xF will no longer have scale att
        break
    end if
 end do

 if (scale.eq.1.0 .and. offset.eq.0.) then
     xF = (/ xS /) 
 else
     xF = xS*scale + offset 
 end if

 if (isatt(xS,"valid_range") .and. typeof(xS@valid_range).eq."short") then
     vrS = xS@valid_range
     vrF = new ( dimsizes(vrS), float)
     vrF = vrS*scale + offset 
     delete(xF@valid_range)    ; delete the "short" valid_range
     xF@valid_range = vrF      ; recreate with float
 end if

 if (isatt(xF,"missing_value")) then
     delete(xF@missing_value)
     xF@missing_value = xF@_FillValue
 end if

 return (xF)

end
;******************************************************************
; D. Shea
; converts bytes to floats using the "scale" and "offset" attributes (if present) 
; Note: the CF and COARDS conventions require that 
; "if both scale_factor and add_offset
; attributes are present, the data are first scaled before the offset is added" 
; This follows these conventions.

undef("byte2flt")
function byte2flt (xB)
local xF, oNames, sNames, offset, scale, xAtts, nAtts, n
begin
 xF = new ( dimsizes(xB), float)
 
 copy_VarAtts   (xB, xF)
 copy_VarCoords (xB, xF)

          ; should data be 'scaled' and/or 'offset' ?

          ; names to check
 oNames = (/"add_offset", "offset", "OFFSET", "Offset", "_offset" \
           ,"Intercept", "intercept", "add_off", "ADD_OFF"/)
 sNames = (/"scale", "SCALE", "Scale", "_scale", "scale_factor" , "factor"   \
           ,"Scale_factor", "Slope" , "slope", "ScaleFactor", "Scale_Factor" \
           ,"SCALING_FACTOR" /)


 offset = 0.0   ; establish as type float
 scale  = 1.0

 xAtts  = getvaratts(xB)
 nAtts  = dimsizes(xAtts)
 
 do n=0,nAtts-1
    if (any(oNames.eq.xAtts(n))) then
        if (typeof(xB@$xAtts(n)$).eq."float") then
            offset = xB@$xAtts(n)$
        else
            if (typeof(xB@$xAtts(n)$).eq."double") then
                offset = doubletofloat(xB@$xAtts(n)$)
            end if
            if (typeof(xB@$xAtts(n)$).eq."string") then
                offset = stringtofloat(xB@$xAtts(n)$)
            end if
        end if
        delete(xF@$xAtts(n)$)  ; xF will no longer have offset att
        break
    end if
 end do
 
 do n=0,nAtts-1
    if (any(sNames.eq.xAtts(n))) then
        if (typeof(xB@$xAtts(n)$).eq."float") then
            scale  = xB@$xAtts(n)$
        else
            if (typeof(xB@$xAtts(n)$).eq."double") then
                scale  = doubletofloat(xB@$xAtts(n)$)
            end if
            if (typeof(xB@$xAtts(n)$).eq."string") then
                scale  = stringtofloat(xB@$xAtts(n)$)
            end if
        end if
        delete(xF@$xAtts(n)$)  ; xF will no longer have scale att
        break
    end if
 end do

 if (scale.eq.1.0 .and. offset.eq.0.) then
     xF = (/ xB /) 
 else
     xF = xB*scale + offset 
 end if

 if (isatt(xB,"valid_range") .and. \
    (typeof(xB@valid_range).eq."byte" .or. \
     typeof(xB@valid_range).eq."ubyte")    ) then
     vrB = xB@valid_range
     vrF = new ( dimsizes(vrB), float)
     vrF = vrB*scale + offset 
     delete(xF@valid_range)    ; delete the "byte" valid_range
     xF@valid_range = vrF      ; recreate with float
 end if

 return (xF)

end
;******************************************************************
; D. Shea
; converts shorts to floats using the "scale" and "offset" attributes (if present) 

; Note: Sometimes HDF usage require that 
; if both scale_factor and add_offset attributes are present, 
; the following implementation should be used:
;      result = scale_factor*(stored_integer-add_offset)
; This is different than the COARDS and CF Conventions.
; There is no automatic way to determine which form is to be used.
; Trial and error? 
;
; Usage: 
;      x = short2flt_hdf(xS)  where xS is a short
; also directly convert to float
;      x = short2flt_hdf(f->xS)  
;
undef("short2flt_hdf")
function short2flt_hdf (xS:short)
local xF, oNames, sNames, offset, scale, xAtts, nAtts, n
begin
;xF = new ( dimsizes(xS), float)
 xF = new ( dimsizes(xS), float, 1.e20)  ; 20 Nov 2002
 
 copy_VarAtts   (xS, xF)
 copy_VarCoords (xS, xF)
                                     ; added 19 Dec 2002
 if (isatt(xS,"missing_value") .and. .not.isatt(xS,"_FillValue")) then
           xS@_FillValue = xS@missing_value
 end if

          ; should data be 'scaled' and/or 'offset' ?

          ; names to check
 oNames = (/"add_offset", "offset", "OFFSET", "Offset", "_offset" \
           ,"Intercept", "intercept", "scalingIntercept", "INTERCEPT" \
           ,"add_off"                                             /)
 sNames = (/"scale", "SCALE", "Scale", "_scale", "scale_factor"   \
           ,"Scale_factor", "Slope" , "slope", "ScaleFactor"      \
           ,"Scale_Factor", "scalingSlope", "SCALING_FACTOR"      \
           ,"SCALE_FACTOR", "SLOPE"                             /)


 offset = 0.0   ; establish as type float
 scale  = 1.0

 xAtts  = getvaratts(xS)
 nAtts  = dimsizes(xAtts)
 
 do n=0,nAtts-1
    if (any(oNames.eq.xAtts(n))) then
        if (typeof(xS@$xAtts(n)$).eq."float") then
            offset = xS@$xAtts(n)$
        else
            if (typeof(xS@$xAtts(n)$).eq."double") then
                offset = doubletofloat(xS@$xAtts(n)$)
            end if
            if (typeof(xS@$xAtts(n)$).eq."string") then
                offset = stringtofloat(xS@$xAtts(n)$)
            end if
        end if
        delete(xF@$xAtts(n)$)  ; xF will no longer have offset att
        break
    end if
 end do
 
 do n=0,nAtts-1
    if (any(sNames.eq.xAtts(n))) then
        if (typeof(xS@$xAtts(n)$).eq."float") then
            scale  = xS@$xAtts(n)$
        else
            if (typeof(xS@$xAtts(n)$).eq."double") then
                scale  = doubletofloat(xS@$xAtts(n)$)
            end if
            if (typeof(xS@$xAtts(n)$).eq."string") then
                scale  = stringtofloat(xS@$xAtts(n)$)
            end if
        end if
        delete(xF@$xAtts(n)$)  ; xF will no longer have scale att
        break
    end if
 end do

 if (scale.eq.1.0 .and. offset.eq.0.) then
     xF = (/ xS /) 
 else
     xF = scale*(xS - offset) 
 end if

 if (isatt(xS,"valid_range") .and. typeof(xS@valid_range).eq."short") then
     vrS = xS@valid_range
     vrF = new ( dimsizes(vrS), float)
     vrF = vrS*scale + offset 
     delete(xF@valid_range)    ; delete the "short" valid_range
     xF@valid_range = vrF      ; recreate with float
 end if

 if (isatt(xF,"missing_value")) then
     delete(xF@missing_value)
     xF@missing_value = xF@_FillValue
 end if

 return (xF)

end
;******************************************************************
; D. Shea
; converts bytes to floats using the "scale" and "offset"
; attributes (if present) 

; Note: Conventional HDF usage [HDF Users Guide] require that 
; if both scale_factor and add_offset attributes are present, 
; the following implementation should be used:
;      Usage: x = byte2flt_hdf(xB)  where xB is a short

undef("byte2flt_hdf")
function byte2flt_hdf (xB:byte)
local xF, oNames, sNames, offset, scale, xAtts, nAtts, n
begin
 xF = new ( dimsizes(xB), float)
 
 copy_VarAtts   (xB, xF)
 copy_VarCoords (xB, xF)

          ; should data be 'scaled' and/or 'offset' ?

          ; names to check
 oNames = (/"add_offset", "offset", "OFFSET", "Offset", "_offset" \
           ,"Intercept", "intercept"/)
 sNames = (/"scale", "SCALE", "Scale", "_scale", "scale_factor" \
           ,"Scale_factor", "Slope" , "slope", "ScaleFactor"    \
           ,"Scale_Factor", "SCALING_FACTOR" /)


 offset = 0.0   ; establish as type float
 scale  = 1.0

 xAtts  = getvaratts(xB)
 nAtts  = dimsizes(xAtts)
 
 do n=0,nAtts-1
    if (any(oNames.eq.xAtts(n))) then
        if (typeof(xB@$xAtts(n)$).eq."float") then
            offset = xB@$xAtts(n)$
        else
            if (typeof(xB@$xAtts(n)$).eq."double") then
                offset = doubletofloat(xB@$xAtts(n)$)
            end if
            if (typeof(xB@$xAtts(n)$).eq."string") then
                offset = stringtofloat(xB@$xAtts(n)$)
            end if
        end if
        delete(xF@$xAtts(n)$)  ; xF will no longer have offset att
        break
    end if
 end do
 
 do n=0,nAtts-1
    if (any(sNames.eq.xAtts(n))) then
        if (typeof(xB@$xAtts(n)$).eq."float") then
            scale  = xB@$xAtts(n)$
        else
            if (typeof(xB@$xAtts(n)$).eq."double") then
                scale  = doubletofloat(xB@$xAtts(n)$)
            end if
            if (typeof(xB@$xAtts(n)$).eq."string") then
                scale  = stringtofloat(xB@$xAtts(n)$)
            end if
        end if
        delete(xF@$xAtts(n)$)  ; xF will no longer have scale att
        break
    end if
 end do

 if (scale.eq.1.0 .and. offset.eq.0.) then
     xF = (/ xB /) 
 else
     xF = scale*(xB - offset) 
 end if

 if (isatt(xB,"valid_range") .and. typeof(xB@valid_range).eq."byte") then
     vrB = xB@valid_range
     vrF = new ( dimsizes(vrB), float)
     vrF = vrB*scale + offset 
     delete(xF@valid_range)    ; delete the "byte" valid_range
     xF@valid_range = vrF      ; recreate with float
 end if

 return (xF)

end
; ********************************************************************
; D. Shea
; There is no built-in "floattostring"
; Convert float values to type string

undef("flt2string")
function flt2string ( x:float )
local x_str
begin
  ;x_str = x + ""              ; trick
  ;copy_VarMeta (x, x_str)     ; contributed.ncl

   x_str = new (dimsizes(x), "string")
   delete(x_str@_FillValue)
   x_str = x
   return(x_str)
end

; ********************************************************************
; D. Shea
; Convert *any* numeric type to integer with option for rounding

undef("numeric2int")
function numeric2int( x:numeric, opt[1]:integer)
local t, i
begin
  t = typeof( x )
  if (t.eq."integer") then
      return( x )
  end if
  if (t.eq."float") then
      if (opt.eq.0) then
          return( toint(x) )       ; truncate
      else
          return( round(x,3) )
      end if
  end if
  if (t.eq."double") then
      if (opt.eq.0) then
          return( toint(x) )  ; truncate
      else
          return( round(x,3) )
      end if
  end if
  if (t.eq."short" .or. t.eq."byte") then
      i = new( dimsizes(x), "integer", getFillValue(x))
      i = x               ; promote to integer
      return( i )
  end if
end
; ********************************************************************
; From contributed.ncl
; ********************************************************************
; D. Shea
; Truncate or Round "x" to the number of decimal places [nDec]
; This will [I hope] operate on any arbitrary array of float/double 
; numbers. However, originally it was designed to work on a scalar for
; plotting or printing purposes.

; ==> The NCL function "sprintf" has much of the same functionality
; ==> as this function. "decimalPlaces" is kept for backward
; ==> compatibility reasons.
;
; It was upgraded by D Shea in Dec 2009 to use "round" and to
; return metadata. It ronds to the higher absolute number.
;
; x     - float or double only
; nDec  - number of decimal places to the right of decimal point
; Round - round or truncate
;   
; usage: 
;   x = 12.345678: 
;   xRound = decimalPlaces( x, 2, True)   ;  12.35
;   xRound = decimalPlaces( x, 2, False)  ;  12.34
;   xRound = decimalPlaces(-x, 2, True)   ; -12.35
;   xRound = decimalPlaces(-x, 2, False)  ; -12.34
;   title  = "EOF: PerCent Variance="+decimalPlaces(pcvar(0), 2, True)

undef("decimalPlaces")
function decimalPlaces(x:numeric, nDec:integer, Round:logical)
local xType, xSign, shift, xTmp, _FillValue
begin
  xType = typeof(x)
  xSign = where(x.lt.0, -1, 1)

  if (xType.eq."float"  .or.  xType.eq."double") then
      if (xType.eq."double") then
          shift = 10.0d^nDec
      else
          shift = 10.0^nDec
      end if

      xTmp = x                                        ; meta data
      if (Round) then
          xTmp = (/ toint(round(abs(xTmp*shift),0))/shift /)  ; round
      else
          xTmp = toint(abs(xTmp*shift))/shift                 ; truncate 
      end if

      xTmp = xSign*xTmp
      return(xTmp)
  else
      print ("decimalPlaces: input is of type "+xType )
      print ("               only float or double is allowed")
      return (x)           
  end if
end
; *******************************************************************
; M. Haley
; Constructs a string with dim names to return with the _Wrap fcns
; str - name of function being used
; val - variable containing possible dimension names
; dims - dimensions whose dim names we want to use (if they exist)

undef("dimWrapString")
function dimWrapString(str[1]:string,val:numeric,dims[*]:integer)
local any_valid_dims, i, strcat, dnames
begin
 dnames        = getvardims(val)
 any_valid_dims = False
 if(.not.all(ismissing(dnames))) then
   strcat = str + " over dimension(s): "
   do i=0,dimsizes(dims)-1
     if(.not.ismissing(val!dims(i))) then   
       if(.not.any_valid_dims) then
         strcat = strcat + val!dims(i)
         any_valid_dims = True
       else
         strcat = strcat + "," + val!dims(i)
       end if
      end if
   end do
 end if
 if(any_valid_dims) then
   return(strcat)
 else
   return(str + " function was applied")
 end if
end

; *******************************************************************
; D. Shea
; Compute a dimension average of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 


undef ("dim_avg_Wrap")
function dim_avg_Wrap (x:numeric)     
local xave, dimx, Ndx, Ndx1
begin

 xave = dim_avg(x)          ; arithmetic ave [no meta data]
 
 copy_VarAtts (x, xave)     ; copy attributes

 dimx = dimsizes(x)         ; size of each dimension
 Ndx  = dimsizes(dimx)      ; number of dimensions
 copy_VarCoords_1 (x, xave) ; one less dimension 
                                       ; add an extra attribute
 Ndx1 = Ndx-1                          ; last dimension
 xave@average_op_ncl = dimWrapString("dim_avg",x,Ndx1)
 
 return (xave)
end
; *******************************************************************
; M. Haley
; Compute a dimension average of "x" [x may have many dimensions]
; return with one less dimension. Based on dim_avg_Wrap.
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do average across

undef ("dim_avg_n_Wrap")
function dim_avg_n_Wrap (x:numeric,dim_args[*])
local xave, dims
begin

 dims = dimnames_to_indexes(x,dim_args)
 xave = dim_avg_n(x,dims)   ; arithmetic ave [no meta data]
 
 copy_VarAtts (x, xave)     ; copy attributes
                            ; copy dim names and coord variables

 copy_VarCoords_not_n (x, xave, dims)
                                       ; add an extra attribute
 xave@average_op_ncl = dimWrapString("dim_avg_n",x,dims)

 return (xave)
end
; *******************************************************************
; D. Shea
; Compute a wgted average of the rightmost dimension of "x" 
; return with one less dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 

undef ("dim_avg_wgt_Wrap")
function dim_avg_wgt_Wrap (x:numeric, w[*]:numeric, opt[1]:integer)     
local xave, dimx, Ndx, Ndx1
begin

 xave = dim_avg_wgt(x,w,opt); weighted ave [no meta data]
 
 copy_VarAtts (x, xave)     ; copy attributes
                            ; copy dim  names and coord  variables
 dimx = dimsizes(x)         ; size of each dimension
 Ndx  = dimsizes(dimx)      ; number of dimensions
 copy_VarCoords_1 (x, xave) ; one less dimension 
                                       ; add an extra attribute
 Ndx1 = Ndx-1                          ; last dimension
 xave@average_op_ncl =  dimWrapString("dim_avg_wgt",x,Ndx1)
 
 return (xave)
end

; *******************************************************************
; M. Haley
; Compute a wgted average of the dims' dimensions of "x" 
; return with one less dimension. Based on dim_avg_wgt_Wrap.
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do wgted average across

undef ("dim_avg_wgt_n_Wrap")
function dim_avg_wgt_n_Wrap (x:numeric, w[*]:numeric, opt[1]:integer, \
                             dims[*]:integer)
local xave
begin

 xave = dim_avg_wgt_n(x,w,opt,dims); weighted ave [no meta data]
 
 copy_VarAtts (x, xave)     ; copy attributes
                            ; copy dim  names and coord  variables
 copy_VarCoords_not_n (x, xave, dims)
                                       ; add an extra attribute
 xave@average_op_ncl = dimWrapString("dim_avg_wgt_n",x,dims)

 return (xave)
end
; *******************************************************************
; D. Shea
; Compute a dimension variance of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 


undef ("dim_variance_Wrap")
function dim_variance_Wrap (x:numeric)     
local xvar, dimx, Ndx, Ndx1
begin

 xvar = dim_variance(x)     ; arithmetic ave [no meta data]
 
 copy_VarAtts (x, xvar)     ; copy attributes
                            ; copy dim  names and coord  variables
 dimx = dimsizes(x)         ; size of each dimension
 Ndx  = dimsizes(dimx)      ; number of dimensions
 copy_VarCoords_1 (x, xvar) ; one less dimension 
                                       ; add an extra attribute
 Ndx1 = Ndx-1                          ; last dimension
 xvar@variance_op_ncl = dimWrapString("dim_variance",x,Ndx1)
 
 return (xvar)
end
; *******************************************************************
; M. Haley
; Compute a dimension variance of "x" [x may have many dimensions]
; return with one less dimension. Based on dim_variance_Wrap
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do variance across

undef ("dim_variance_n_Wrap")
function dim_variance_n_Wrap (x:numeric, dims[*]:integer)
local xvar
begin

 xvar = dim_variance_n(x,dims)     ; arithmetic ave [no meta data]
 
 copy_VarAtts (x, xvar)     ; copy attributes
                            ; copy dim  names and coord  variables
 copy_VarCoords_not_n (x, xvar, dims)
                                       ; add an extra attribute
 xvar@variance_op_ncl = dimWrapString("dim_variance_n",x,dims)

 return (xvar)
end
; *******************************************************************
; D. Shea
; Compute a dimension standard dev of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 


undef ("dim_stddev_Wrap")
function dim_stddev_Wrap (x:numeric)     
local xstd, dimx, Ndx, Ndx1
begin

 xstd = dim_stddev(x)       ; arithmetic ave [no meta data]
 
 copy_VarAtts (x, xstd)     ; copy attributes
                            ; copy dim  names and coord  variables
 dimx = dimsizes(x)         ; size of each dimension
 Ndx  = dimsizes(dimx)      ; number of dimensions
 copy_VarCoords_1 (x, xstd) ; one less dimension 
                                       ; add an extra attribute
 Ndx1 = Ndx-1                          ; last dimension
 xstd@stddev_op_ncl = dimWrapString("dim_stddev",x,Ndx1)
 
 return (xstd)
end
; *******************************************************************
; M. Haley
; Compute a dimension standard dev of "x" [x may have many dimensions]
; return with one less dimension. Based on dim_stddev_Wrap
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do sum across

undef ("dim_stddev_n_Wrap")
function dim_stddev_n_Wrap (x:numeric,dims[*]:integer)
local xstd
begin

 xstd = dim_stddev_n(x,dims); arithmetic ave [no meta data]
 
 copy_VarAtts (x, xstd)     ; copy attributes
                            ; copy dim  names and coord  variables
 copy_VarCoords_not_n (x, xstd, dims) 
                                  ; add an extra attribute
 xstd@stddev_op_ncl = dimWrapString("dim_stddev_n",x,dims)
 
 return (xstd)
end
; *******************************************************************
; D. Shea
; Compute a dimension sum of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 


undef ("dim_sum_Wrap")
function dim_sum_Wrap (x:numeric)     
local xsum, dimx, Ndx, Ndx1
begin

 xsum = dim_sum(x)          ; arithmetic sum [no meta data]
 
 copy_VarAtts (x, xsum)     ; copy attributes
                            ; copy dim  names and coord  variables
 dimx = dimsizes(x)         ; size of each dimension
 Ndx  = dimsizes(dimx)      ; number of dimensions
 copy_VarCoords_1 (x, xsum) ; one less dimension 
                                       ; add an extra attribute
 Ndx1 = Ndx-1                          ; last dimension
 xsum@sum_op_ncl = dimWrapString("dim_sum",x,Ndx1)
 
 return (xsum)
end
; *******************************************************************
; M. Haley
; Compute a dimension sum of "x" [x may have many dimensions]
; return with one less dimension. Based on dim_sum_Wrap.
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do sum across

undef ("dim_sum_n_Wrap")
function dim_sum_n_Wrap (x:numeric,dims[*]:integer)
local xsum
begin

 xsum = dim_sum_n(x,dims)   ; arithmetic sum [no meta data]
 
 copy_VarAtts (x, xsum)     ; copy attributes
                            ; copy dim names and coord variables
 copy_VarCoords_not_n (x, xsum, dims)
                                       ; add an extra attribute
 xsum@sum_op_ncl = dimWrapString("dim_sum_n",x,dims)

 return (xsum)
end

; *******************************************************************
; M. Haley
; Compute a dimension min of "x" [x may have many dimensions]
; return with one less dimension. There is no dim_min_Wrap.
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do min across

undef ("dim_min_n_Wrap")
function dim_min_n_Wrap (x:numeric,dims[*]:integer)
local xmin
begin

 xmin = dim_min_n(x,dims)   ; min of data [no meta data]
 
 copy_VarAtts (x, xmin)     ; copy attributes
                            ; copy dim names and coord variables
 copy_VarCoords_not_n (x, xmin, dims)
                                       ; add an extra attribute
 xmin@min_op_ncl = dimWrapString("dim_min_n",x,dims)

 return (xmin)
end

; *******************************************************************
; M. Haley
; Compute a dimension max of "x" [x may have many dimensions]
; return with one less dimension. There is no dim_max_Wrap.
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do max across

undef ("dim_max_n_Wrap")
function dim_max_n_Wrap (x:numeric,dims[*]:integer)
local xmax
begin

 xmax = dim_max_n(x,dims)   ; max of data [no meta data]
 
 copy_VarAtts (x, xmax)     ; copy attributes
                            ; copy dim names and coord variables
 copy_VarCoords_not_n (x, xmax, dims)
                                       ; add an extra attribute
 xmax@max_op_ncl = dimWrapString("dim_max_n",x,dims)

 return (xmax)
end

; *******************************************************************
; D. Shea
; Compute a dimension sum of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 


undef ("dim_sum_wgt_Wrap")
function dim_sum_wgt_Wrap (x:numeric, w[*]:numeric, opt[1]:integer )  
local xsum, dimx, Ndx, Ndx1
begin

 xsum = dim_sum_wgt(x, w, opt) ; arithmetic weighted  sum 
 
 copy_VarAtts (x, xsum)     ; copy attributes
                            ; copy dim  names and coord  variables
 dimx = dimsizes(x)         ; size of each dimension
 Ndx  = dimsizes(dimx)      ; number of dimensions
 copy_VarCoords_1 (x, xsum) ; one less dimension 
                                       ; add an extra attribute
 Ndx1 = Ndx-1                          ; last dimension
 xsum@sum_op_ncl = dimWrapString("dim_sum_wgt",x,Ndx1)
 
 return (xsum)
end
; *******************************************************************
; M. Haley
; Compute a dimension wgted sum of "x" [x may have many dimensions]
; return with one less dimension. Based on dim_sum_wgt_Wrap
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do calculation on

undef ("dim_sum_wgt_n_Wrap")
function dim_sum_wgt_n_Wrap (x:numeric, w[*]:numeric, opt[1]:integer, \
                            dims[*]:numeric )  
local xsum
begin

 xsum = dim_sum_wgt_n(x, w, opt, dims) ; arithmetic weighted  sum 
 
 copy_VarAtts (x, xsum)     ; copy attributes
                            ; copy dim  names and coord  variables
 copy_VarCoords_not_n (x, xsum, dims) 
                                       ; add an extra attribute
 xsum@sum_op_ncl = dimWrapString("dim_sum_wgt_n",x,dims)
 
 return (xsum)
end
; *******************************************************************
; D. Shea
; Remove means of each rightmost dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
;
undef ("dim_rmvmean_Wrap")
function dim_rmvmean_Wrap (x:numeric)     
local xAnom, dimx, Ndx
begin

 xAnom = dim_rmvmean(x)     
 copy_VarAtts(x, xAnom)
 copy_VarCoords(x, xAnom)
 
 if (isatt(x,"long_name")) then
     xAnom@long_name = "Anomalies: "+getLongName(x)
 else
     xAnom@long_name = "Deviation from mean"
 end if
 
 dimx = dimsizes(x)
 Ndx  = dimsizes(dimx)      ; number of dimensions
 xAnom@rmvmean_op_NCL = dimWrapString("dim_rmvmean",x,Ndx-1)

 return (xAnom)
end
; *******************************************************************
; M. Haley
; Remove means of all but the dims' dimensions.
;      Based on dim_rmvmean_Wrap
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do calculation on

undef ("dim_rmvmean_n_Wrap")
function dim_rmvmean_n_Wrap (x:numeric,dims[*]:integer)
local xAnom
begin

 xAnom = dim_rmvmean_n(x,dims)
 copy_VarAtts(x, xAnom)
 copy_VarCoords(x, xAnom)
 
 if (isatt(x,"long_name")) then
     xAnom@long_name = "Anomalies: "+getLongName(x)
 else
     xAnom@long_name = "Deviation from mean"
 end if
   
 xAnom@rmvmean_op_NCL = dimWrapString("dim_rmvmean_n",x,dims)

 return (xAnom)
end
; *******************************************************************
; M. Haley
; Remove median of all but the dims' dimensions.
;      Based on dim_rmvmean_Wrap
; Remove median of each rightmost dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
;
undef ("dim_rmvmed_Wrap")
function dim_rmvmed_Wrap (x:numeric)     
local xRmvMed, dimx, Ndx
begin

 xRmvMed = x                   ; copy metadata
 xRmvMed = dim_rmvmed(x)     
 
 dimx = dimsizes(x)
 Ndx  = dimsizes(dimx)      ; number of dimensions
 xRmvMed@rmvmed_op_NCL = dimWrapString("dim_rmvmed",x,Ndx-1)

 return (xRmvMed)
end
; *******************************************************************
; M. Haley
; Remove median of all but the dims' dimensions.
;      Based on dim_rmvmed_Wrap
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do calculation on
undef ("dim_rmvmed_n_Wrap")
function dim_rmvmed_n_Wrap (x:numeric,dims[*]:integer)
local xRmvMed
begin

 xRmvMed = x                   ; copy metadata
 xRmvMed = dim_rmvmed_n(x,dims)
 
 xRmvMed@rmvmed_op_NCL = dimWrapString("dim_rmvmed_n",x,dims)

 return (xRmvMed)
end
; *******************************************************************
; D. Shea
; Standardize by the st. dev of each rightmost dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 


undef ("dim_standardize_Wrap")
function dim_standardize_Wrap (x:numeric, opt:integer)     
local xStd, dimx, Ndx, Ndx1
begin

 xStd = x                   ; copy metadata
 xStd = dim_standardize(x, opt)
 
 dimx = dimsizes(x)         ; size of each dimension
 Ndx  = dimsizes(dimx)      ; number of dimensions
 Ndx1 = Ndx-1               ; last dimension
 xStd@standardize_op_ncl = dimWrapString("dim_standardize",x,Ndx1)
 
 return (xStd)
end

; *******************************************************************
; M. Haley
; Standardize by the st. dev of each dims' dimensions
;    Based on dim_standardize_Wrap
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; dims - dimension(s) to do calculation on


undef ("dim_standardize_n_Wrap")
function dim_standardize_n_Wrap (x:numeric, opt:integer, dims[*]:integer)
local xStd
begin

 xStd = x                   ; copy metadata
 xStd = dim_standardize_n(x, opt, dims)
 
                            ; add an extra attribute
 xStd@standardize_op_ncl= dimWrapString("dim_standardize_n",x,dims)
 
 return (xStd)
end

;**********************************************************
; D. Shea
; Compute average root-mean-square-difference between "x" and "y" 
; return with one less dimension
; Copies over all the attributes and coordinate variables
; x,y    - 1D or multidimensional variables (must have same size) 

undef ("dim_rmsd_Wrap")
function dim_rmsd_Wrap (x:numeric, y:numeric)     
local xrmsd, dimx, dimy, Ndx, Ndx1
begin
 dimx  = dimsizes(x)
 dimy  = dimsizes(y)
 if (all(dimx.eq.dimy)) then

     xrmsd = dim_rmsd(x,y)          ; rmsd values only [no meta data]
     
     copy_VarAtts (x, xrmsd)    ; copy attributes
     if (isatt(xrmsd,"long_name")) then
         xrmsd@long_name = "RMSD: "+getLongName(x)
     end if
                                ; copy dim  names and coord  variables
     Ndx  = dimsizes(dimx)      ; number of dimensions
     copy_VarCoords_1 (x, xrmsd); one less dimension 
                                           ; add an extra attribute
     Ndx1 = Ndx-1                          ; last dimension
     xrmsd@rmsd_op_ncl = dimWrapString("dim_rmsd",x,Ndx1)

     return (xrmsd)
 else
     print ("------------------------------------------------------")
     print ("---> dim_rmsd_Wrap: error: x,y dimension mismatch <---")
     print ("------------------------------------------------------")
     exit
 end if
 
end
;**********************************************************
; M. Haley
; Compute average root-mean-square-difference between "x" and "y" 
;      Based on dim_rmsd_Wrap
; return with one less dimension
; Copies over all the attributes and coordinate variables
; x,y    - 1D or multidimensional variables (must have same size) 
; dims - dimension(s) to do calculation on

undef ("dim_rmsd_n_Wrap")
function dim_rmsd_n_Wrap (x:numeric, y:numeric, dims[*]:integer)
local xrmsd, dimx, dimy
begin
 dimx  = dimsizes(x)
 dimy  = dimsizes(y)
 if (all(dimx.eq.dimy)) then

     xrmsd = dim_rmsd_n(x,y,dims)     ; rmsd values only [no meta data]
     
     copy_VarAtts (x, xrmsd)    ; copy attributes
     if (isatt(xrmsd,"long_name")) then
         xrmsd@long_name = "RMSD: "+getLongName(x)
     end if
                                ; copy dim  names and coord  variables
     copy_VarCoords_not_n (x, xrmsd, dims)
                                           ; add an extra attribute
     xrmsd@rmsd_op_ncl = dimWrapString("dim_rmsd_n",x,dims)

     return (xrmsd)
 else
     print ("--------------------------------------------------------")
     print ("---> dim_rmsd_n_Wrap: error: x,y dimension mismatch <---")
     print ("--------------------------------------------------------")
     exit
 end if
 
end
; *******************************************************************
; D. Shea
; returns cumulative sum of each rightmost dimension
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; opt  - option argument, must be 0, 1, or 2
;
undef ("dim_cumsum_Wrap")
function dim_cumsum_Wrap (x:numeric,opt:integer)
local xCumSum
begin

 xCumSum = dim_cumsum(x,opt)
 copy_VarAtts(x, xCumSum)
 copy_VarCoords(x, xCumSum)
 
 if (isatt(x,"long_name")) then
     xCumSum@long_name = "Cumulative Sum: "+getLongName(x)
 else
     xCumSum@long_name = "Cumulative Sum"
 end if
 
 return (xCumSum)
end
; *******************************************************************
; M. Haley
; returns cumulative sum at the given  dimension
;    Based on dim_cumsum_Wrap 
; Copies over all the attributes and coordinate variables
; x    - multidimensional variable 
; opt  - option argument, must be 0, 1, or 2
; dims - dimensions to do calculations on

undef ("dim_cumsum_n_Wrap")
function dim_cumsum_n_Wrap (x:numeric,opt:integer,dims[1]:integer)
local xCumSum
begin

 xCumSum = dim_cumsum_n(x,opt,dims)
 copy_VarAtts(x, xCumSum)
 copy_VarCoords(x, xCumSum)
 
 if (isatt(x,"long_name")) then
     xCumSum@long_name = "Cumulative Sum: "+getLongName(x)
 else
     xCumSum@long_name = "Cumulative Sum"
 end if
 
 return (xCumSum)
end

;**********************************************************
;D. Shea
; Compute divergence on a fixed grid

undef("uv2dvF_Wrap")
function uv2dvF_Wrap (u:numeric, v:numeric)
local div
begin
 div = uv2dvF (u,v)
 copy_VarMeta (u, div)
 div@long_name = "divergence"
 div@units     = "1/s"        ; assume u,v are m/s
 return (div)
end

;**********************************************************
;D. Shea
; Compute divergence on a gaussian grid

undef("uv2dvG_Wrap")
function uv2dvG_Wrap (u:numeric, v:numeric)
local div
begin
 div = uv2dvG (u,v)
 copy_VarMeta (u, div)
 div@long_name = "divergence"
 div@units     = "1/s"        ; assume u,v are m/s
 return (div)
end

;**********************************************************
;D. Shea
; Compute relative vorticity on a fixed grid

undef("uv2vrF_Wrap")
function uv2vrF_Wrap (u:numeric, v:numeric)
local vrt
begin
 vrt = uv2vrF (u,v)
 copy_VarMeta (u, vrt)
 vrt@long_name = "vorticity"
 vrt@units     = "1/s"        ; assume u,v are m/s
 return (vrt)
end

;**********************************************************
;D. Shea
; Compute relative vorticity on a gaussian grid

undef("uv2vrG_Wrap")
function uv2vrG_Wrap (u:numeric, v:numeric)
local vrt
begin
 vrt = uv2vrG (u,v)
 copy_VarMeta (u, vrt)
 vrt@long_name = "vorticity"
 vrt@units     = "1/s"        ; assume u,v are m/s
 return (vrt)
end

;**********************************************************
;D. Shea
; Compute inverse laplacian on a gaussian grid

undef("ilapsG_Wrap")
function ilapsG_Wrap (zlap:numeric, zlmbda:numeric)
local answer
begin
 answer = ilapsG (zlap,zlmbda)
 copy_VarMeta (zlap, answer)
 answer@long_name = "inverse laplacian"
 answer@units     = ""        ; assume u,v are m/s
 return (answer)
end

;**********************************************************
;D. Shea
; Compute inverse laplacian on a fixed grid

undef("ilapsF_Wrap")
function ilapsF_Wrap (zlap:numeric, zlmbda:numeric)
local answer
begin
 answer = ilapsF (zlap,zlmbda)
 copy_VarMeta (zlap, answer)
 answer@long_name = "inverse laplacian"
 answer@units     = ""        ; assume u,v are m/s
 return (answer)
end

;************************************************************
; D. Shea
; Wrappers for the dv2uv[G/F] and vr2uv[G/F] functions  

undef("dv2uvG_Wrap")
function dv2uvG_Wrap (dv)
local uv
begin
  uv   = dv2uvG(dv)
  copy_VarAtts (dv, uv)
                       ; override long_name and units
  uv@long_name = "divergent zonal [0] and meridional [1] winds"
  uv@units     = "m/s"
                       ; coordinate variables
  component_copy_VarCoords (dv, uv)
  return(uv)
end

undef("dv2uvF_Wrap")
function dv2uvF_Wrap (dv)
local uv
begin
  uv   = dv2uvF(dv)
  copy_VarAtts (dv, uv)
                       ; override long_name and units
  uv@long_name = "divergent zonal [0] and meridional [1] winds"
  uv@units     = "m/s"
                       ; coordinate variables
  component_copy_VarCoords (dv, uv)
  return(uv)
end

undef("vr2uvG_Wrap")
function vr2uvG_Wrap (vr)
local uv
begin
  uv   = vr2uvG(vr)
  copy_VarAtts (vr, uv)
                       ; override long_name and units
  uv@long_name = "rotational zonal [0] and meridional [1] winds"
  uv@units     = "m/s"
                       ; coordinate variables
  component_copy_VarCoords (vr, uv)
  return(uv)
end

undef("vr2uvF_Wrap")
function vr2uvF_Wrap (vr)
local uv
begin
  uv   = vr2uvF(vr)
  copy_VarAtts (vr, uv)
                       ; override long_name and units
  uv@long_name = "rotational zonal [0] and meridional [1] winds"
  uv@units     = "m/s"
                       ; coordinate variables
  component_copy_VarCoords (vr, uv)
  return(uv)
end

;**********************************************************
;D. Shea
; Compute a zonal average of "x" [x may have many dimensions]
; return with one less dimension

undef("zonalAve")
function zonalAve (x:numeric)
local xzon
begin
 xzon = dim_avg_Wrap (x)
                        ; ----DJS personal---- over ride above 
 if (isatt(xzon,"long_name") .or. isatt(xzon,"description") .or. \
     isatt(xzon,"standard_name") ) then
     xzon@long_name  = "Zonal Ave ["+ getLongName(xzon)+"]" 
 else
     xzon@long_name  = "Zonal Average"
 end if

 if (isatt(xzon,"short_name")) then
     xzon@short_name = "Zonal Ave ["+ xzon@short_name+"]" 
 else
     xzon@short_name = "ZonAve"
 end if
                        ; ----end DJS personal
 return (xzon)
end


; **********************************************************
; D. Shea
; same arguments as NCL function smth9
; usually: p=0.50 and q={-0.25 [light], 0 [5-pt], 0.25[heavy]} 

; This is a "wrapper" for smth9. It copies all attributes
; coordinate dimensions etc to the smoothed variable.
; Also it adds an attribute or two.

; Basically it ensures that the return variable will have all the
; corrects meta info with it. 
; [eg:     xSmth = smth9_Wrap (x,p,q,False)  then xSmth will be "correct"]


undef("smth9_Wrap")
function smth9_Wrap(var:numeric, p[1]:numeric, q[1]:numeric, cyclic:logical)
local var_out
begin
  if (isatt(var,"missing_value") .and. .not.isatt(var,"_FillValue")) then
     var@_FillValue = var@missing_value
  end if
  var_out = var   ; transfer all coordinate variables, attributes etc
  var_out = smth9 (var, p, q, cyclic) ; add info on operation performed
  var_out@spatial_op_ncl = "smth9; nine-pt smoother applied; " + \
                             "p="+p+"  q="+q+"  cyclic="+cyclic
  return(var_out)
end
; ******************************************************************
; D. Shea
; error check: called internally by a number of functions
; sample:   modCheck ("clmMonLLT", ntim, nmos)   ; error check


undef("modCheck")
procedure modCheck (name:string, N, n)
local valid_types
begin
  valid_types = (/"integer","long"/)

  if(.not.any(typeof(N).eq.valid_types).or.\
     .not.any(typeof(n).eq.valid_types)) then
       print ("modCheck: n and N must be integers or longs")
       exit
  end if
  if ((N%n).ne.0) then
       print ("contributed.ncl: "+name+": dimension must be a multiple of "+n)
       exit 
 end if
end

; ******************************************************************
; D. Shea
; error check: called internally by a number of functions
; sample:   rankCheck ("routine_name", x, 3)   ; rank check

undef("rankCheck")
procedure rankCheck (name[1]:string, x, n[1]:integer)
local rank
begin
  rank = dimsizes(dimsizes(x))
  if (rank.ne.n) then
      print("contributed.ncl: rankCheck: "+name+": rank "+n+" required: rank="+rank)
      exit
  end if
end

; ******************************************************************
; D. Shea
; Calculate long term monthly means (monthly climatology)
;
; The time dimension must be a multiple of 12
;
;    x(lat,lon,time)  <==== INPUT DIMENSION ORDER
;    x!0 = "lat"
;    x!1 = "lon"
;    x!2 = "time"
;
; Usage:  moClm = clmMonLLT (x)

undef("clmMonLLT")
function clmMonLLT (x[*][*][*]:numeric)
local dimx, nlat, mlon, ntim, i, nmo, nmos, aveMonth
begin

   dimx  = dimsizes (x)
   nlat  = dimx(0)
   mlon  = dimx(1)
   ntim  = dimx(2)

   nmos  = 12
   modCheck ("clmMonLLT", ntim, nmos)   ; error check
;
; Compute all 12 monthly averages first. 
;
   aveMonth = new((/nlat,mlon,nmos/),typeof(x) \
                                    ,getFillValue(x))

   do nmo=0,nmos-1
      aveMonth(:,:,nmo) = dim_avg( x(:,:,nmo:ntim-1:nmos) )
   end do

; copy attributes and add a couple of informational attributes
; only need to do this if plotting or writing to netCDF file

   copy_VarAtts (x,aveMonth)
   aveMonth@time_op_ncl = "Climatology: "+ (ntim/nmos) +" years"
   aveMonth@info    = "function clmMonLLT: contributed.ncl"

; copy spatial (eg, lat/lon) coordinate variables

   do i=0,1
      if (.not.ismissing(x!i)) then
          aveMonth!i = x!i
          if (iscoord(x,x!i)) then
              aveMonth&$aveMonth!i$ = x&$x!i$
          end if
      end if
   end  do

   aveMonth!2     = "month"           ; create a "month" named dim
   aveMonth&month = ispan(0,nmos-1,1) ; create a month coord var

   return (aveMonth)
end

; ******************************************************************
; D. Shea
; Calculate standard deviations of monthly means 
;
; The time dimension must be a multiple of 12
;
;    x(lat,lon,time)  <==== INPUT DIMENSION ORDER
;    x!0 = "lat"   
;    x!1 = "lon"
;    x!2 = "time"
;
; Usage:  moStd = stdMonLLT (x)


undef("stdMonLLT")
function stdMonLLT (x[*][*][*]:numeric)
local dimx, nlat, mlon, ntim, i, nmo, nmos, stdMonth
begin

   dimx  = dimsizes (x)
   nlat  = dimx(0)
   mlon  = dimx(1)
   ntim  = dimx(2)

   nmos  = 12
   modCheck ("stdMonLLT", ntim, nmos)   ; error check
;
; Compute all 12 monthly standard deviations first. 
;
   stdMonth = new((/nlat,mlon,nmos/),typeof(x) \
                                    ,getFillValue(x))

   do nmo=0,nmos-1
      stdMonth(:,:,nmo) = dim_stddev( x(:,:,nmo:ntim-1:nmos) )
   end do

; copy attributes and add a couple of informational attributes
; only need to do this if plotting or writing to netCDF file

   copy_VarAtts (x,stdMonth)

   stdMonth@time_op_ncl = " Monthly Standard Deviation: "+ (ntim/nmos) +" years"
   stdMonth@info    = "function stdMonLLT: contributed.ncl"

; copy spatial (eg, lat/lon) coordinate variables

   do i=0,1
      if (.not.ismissing(x!i)) then
          stdMonth!i = x!i
          if (iscoord(x,x!i)) then
              stdMonth&$stdMonth!i$ = x&$x!i$
          end if
      end if
   end  do

   stdMonth!2     = "month"           ; create a "month" named dim
   stdMonth&month = ispan(0,nmos-1,1) ; create a month coord var

   return (stdMonth)
end

; ------------------------------------------------------------------
; D. Shea
; Calculate anomalies from climatology 
; returned array is same as from "rmMonthAnnualCycleLLT (x, yr1, yr2)
; Subtract the the long term means from each "month".
; On return x will consist of deviations from  each "month's" long term mean.
;
; The time dimension must be a multiple of 12
;
;     x(lat,lon,time)  <==== INPUT DIMENSION ORDER
;     x!0 = "lat"
;     x!1 = "lon"
;     x!2 = "time"
;
; Usage:  x     = calcMonAnomLLT (x,xAve)  ; overwrites "x"
;         xAnom = calcMonAnomLLT (x,xAve)  ; creates xAnom
; where   xAve  = clmMonLLT (x)            ; see previous function 


undef("calcMonAnomLLT")
function calcMonAnomLLT (x[*][*][*]:numeric, xAve[*][*][12]:numeric)
local dimx, ntim, yr, nmos, xAnom
begin
   dimx  = dimsizes (x)
   ntim  = dimx(2)

   nmos  = 12
   modCheck ("calcMonAnomLLT", ntim, nmos)   ; error check

; Now loop on every year and compute difference. 
; The [yr:yr+nmos-1] strips out 12 months for each year.

   xAnom = x          ; variable to variable copy [meta data]
   do yr=0,ntim-1,nmos
      xAnom(:,:,yr:yr+nmos-1) = (/ x(:,:,yr:yr+nmos-1)- xAve /)
   end do

; Create an informational attribute:  

   xAnom@anomaly_op_ncl  = "function calcMonAnomLLT: contributed.ncl"
   return (xAnom)
end


; ******************************************************************
; D. Shea
; Remove that remove Annual Cycle from "monthly" (nmos=12) data. 
; Subtract the the long term means from each "month".
; On return x will consist of deviations from  each "month's" long term mean.
;
; The time dimension must be a multiple of 12
;
;       x(lat,lon,time)  <==== INPUT DIMENSION ORDER
;       x!0 = "lat"
;       x!1 = "lon"
;       x!2 = "time"
;
; Usage:  x = rmMonAnnCyLLT (x)

undef("rmMonAnnCycLLT")
function rmMonAnnCycLLT (x[*][*][*]:numeric)
local dimx, ntim, nmos, xAve, xAnom
begin
   dimx  = dimsizes(x)
   ntim  = dimx(2)

   nmos  = 12
   modCheck ("rmMonAnnCycLLT", ntim, nmos)   ; error check

   xAve  = clmMonLLT (x)            ; Compute all 12 monthly averages first. 
   xAnom = calcMonAnomLLT (x,xAve)  ; Remove the mean from each year-month grid

; Create an informational attribute 

   xAnom@anomaly_op_ncl  = "Annual Cycle Removed:function rmMonAnnCycLLT:contributed.ncl" 
   xAnom@reference   = "function rmMonAnnCycLLT in contrib.ncl"
   return (xAnom)
end
; ******************************************************************
; D. Shea
; Calculate long term monthly means (monthly climatology)
; requires named dimensions
;
; The time dimension must be a multiple of 12
;
;    x(time,lat,lon)  <==== INPUT DIMENSION ORDER
;    x!1 = "time"  
;    x!2 = "lat"
;    x!3 = "lon"
;
; Usage:  moClm = clmMonTLL (x)


undef("clmMonTLL")
function clmMonTLL (x[*][*][*]:numeric)
local dimx, ntim, nlat, mlon, i, nmo, nmos, monAveLLT
begin
  dimx = dimsizes(x)
  ntim = dimx(0)

  nmos  = 12
  modCheck ("clmMonTLL", ntim, nmos)   ; error check
 ;rankCheck("clmMonTLL", x, 3)         ; not needed due to prototyping
      
  nlat = dimx(1)
  mlon = dimx(2)
;
; Compute all 12 monthly averages first. 
;
   aveMonth = new((/nmos,nlat,mlon/),typeof(x) \
                                    ,getFillValue(x))
   do nmo=0,nmos-1
      aveMonth(nmo,:,:) = dim_avg_n( x(nmo:ntim-1:nmos,:,:), 0 )
   end do

; copy attributes and add a couple of informational attributes
; only need to do this if plotting or writing to netCDF file

   copy_VarAtts (x,aveMonth)
   aveMonth@time_op_ncl = "Climatology: "+ (ntim/nmos) +" years"
   aveMonth@info    = "function clmMonTLL: contributed.ncl"

; copy spatial (eg, lat/lon) coordinate variables

   do i=1,2
      if (.not.ismissing(x!i)) then
          aveMonth!i = x!i
          if (iscoord(x,x!i)) then
              aveMonth&$aveMonth!i$ = x&$x!i$
          end if
      end if
   end  do

   aveMonth!0     = "month"           ; create a "month" named dim
   aveMonth&month = ispan(0,nmos-1,1) ; create a month coord var

   return (aveMonth)
end
; ******************************************************************
; D. Shea
; Calculate standard deviations of monthly means (interannual var)
;
; The time dimension must be a multiple of 12
;
;    x(time,lat,lon)  <==== INPUT DIMENSION ORDER
;    x!1 = "time"  
;    x!2 = "lat"
;    x!3 = "lon"
;
; Usage:  moStd = stdMonTLL (x)

undef("stdMonTLL")
function stdMonTLL (x[*][*][*]:numeric)
local dimx, ntim, nlat, mlon, i, nmo, nmos, stdMonth
begin
  dimx = dimsizes(x)
  ntim = dimx(0)

  nmos  = 12
  modCheck ("stdMonTLL", ntim, nmos)   ; error check
 ;rankCheck("stdMonTLL", x, 3)         ; not needed due to prototyping
      
  nlat = dimx(1)
  mlon = dimx(2)
;
; Compute all 12 std deviations. 
;
   stdMonth = new((/nmos,nlat,mlon/),typeof(x) \
                                    ,getFillValue(x))
   do nmo=0,nmos-1
      stdMonth(nmo,:,:) = dim_stddev_n( x(nmo:ntim-1:nmos,:,:), 0 )
   end do

; copy attributes and add a couple of informational attributes
; only need to do this if plotting or writing to netCDF file

   copy_VarAtts (x,stdMonth)
   stdMonth@time_op_ncl = "Monthly Standard Deviation: "+ (ntim/nmos) +" years"
   stdMonth@info    = "function stdMonTLL: contributed.ncl"

; copy spatial (eg, lat/lon) coordinate variables

   do i=1,2
      if (.not.ismissing(x!i)) then
          stdMonth!i = x!i
          if (iscoord(x,x!i)) then
              stdMonth&$stdMonth!i$ = x&$x!i$
          end if
      end if
   end  do

   stdMonth!0     = "month"           ; create a "month" named dim
   stdMonth&month = ispan(0,nmos-1,1) ; create a month coord var

   return (stdMonth)
end

;**********************************************************************
; D. Shea
; Calculate standardized anomalies from monthly data
;
; Subtract the the long term means from each Month.
; divide by the standard deviation for that month
; On return x will consist of anomalies from  each month's long term mean.
;
; opt 
;     opt=1     use population standard deviation to normalize. 
;     opt.ne.1, use sample standard deviation. 
;
; The time dimension must be a multiple of 12
;
;     x(time,lat,lon)  <==== INPUT DIMENSION ORDER
;     x!0 = "time"
;     x!1 = "lat"
;     x!2 = "lon"
;
; Usage:  x     = calcMonStandardizeAnomTLL (x, opt)  ; overwrites "x"
;         xStiz = calcMonStandardizeAnomTLL (x, opt)  ; creates xStiz as new variable

undef("calcMonStandardizeAnomTLL")
function calcMonStandardizeAnomTLL (x[*][*][*]:numeric, opt:integer)
local dimx, ntim, nmo, nmos, dNam, namd0, namd1, namd2, xStiz
begin
   dimx  = dimsizes (x)
   ntim  = dimx(0)

   nmos  = 12
   modCheck ("calcMonStandardizeAnomTLL", ntim, nmos)   ; error check
   dNam  = dimNamCheck ("calcMonStandardizeAnomTLL", x) ; error check

   nyrs  = ntim/nmos

   namd0 = dNam(0)
   namd1 = dNam(1)
   namd2 = dNam(2)   

   xStiz = x          ; variable to variable copy [meta data]
   do nmo=0,ntim-1,nmos
     ;xStiz(nmo:ntim-1:nmos,:,:) = dim_standardize(x(namd1|:,namd2|:,namd0|nmo:ntim-1:nmos), opt)
      xStiz(nmo:ntim-1:nmos,:,:) = dim_standardize_n(x(nmo:ntim-1:nmos,:,:), opt, 0 )  ; Nov 9, 2009
   end do
   if (isatt(x,"long_name") .or. isatt(x,"description") .or. \
       isatt(x,"standard_name") ) then
       xStiz@long_name = "Standardized Anomalies: "+getLongName(x)
   end if
   xStiz@units = "dimensionless"

; Create an informational attribute:  

   xStiz@standardized_op_ncl  = "calcMonStandardizeAnomTLL: contributed.ncl"
   
   return (xStiz)
end

;*******************************************************************
; Adam Phillips
; remove annual cycle from 1d monthly time series
;
; The time dimension must be a multiple of 12
; Usage:
;        xAnom = rmAnnCycle1D (x)
;        x     = rmAnnCycle1D (x)

undef ("rmAnnCycle1D")
function rmAnnCycle1D (x[*]:numeric)
local ntim, nmo, nmos, aveMonth, xAnom, yr
begin
  ntim = dimsizes(x)

  nmos = 12
  modCheck ("rmAnnCycle1D", ntim, nmos)   ; error check
;
; Compute all 12 monthly averages first. 
;
  aveMonth = new(nmos,typeof(x),getFillValue(x))
  do nmo=0,nmos-1
     aveMonth(nmo) = dim_avg( x(nmo:ntim-1:nmos) )
  end do
;
; remove the monthly means from each year
;
  xAnom = x          ; variable to variable copy [meta data]
  do yr=0,ntim-1,nmos
     xAnom(yr:yr+nmos-1) = (/ x(yr:yr+nmos-1)- aveMonth /)
  end do
  xAnom@anomaly_op_ncl  = "Annual Cycle Removed: rmAnnCycle1D: contributed.ncl" 
  return (xAnom)
end
; ******************************************************************
; D. Shea
; Calculate long term monthly means (monthly climatology)
;
; The time dimension must be a multiple of 12
;
;    x(lev,lat,lon,time)  <==== INPUT DIMENSION ORDER
;
; Usage:  moClm = clmMonLLLT (x)
;         moClm(lev,lat,lon,12)


undef("clmMonLLLT")
function clmMonLLLT (x[*][*][*][*]:numeric)
local dimx, klvl, nlat, mlon, ntim, i, nmo, nmos, aveMonth
begin

   dimx  = dimsizes (x)
   klvl  = dimx(0)
   nlat  = dimx(1)
   mlon  = dimx(2)
   ntim  = dimx(3)

   nmos  = 12
   modCheck ("clmMonLLLT", ntim, nmos)   ; error check
;
; Compute all 12 monthly averages first. 
;
   aveMonth = new((/klvl,nlat,mlon,nmos/),typeof(x) \
                                         ,getFillValue(x))

   do nmo=0,nmos-1
      aveMonth(:,:,:,nmo) = dim_avg( x(:,:,:,nmo:ntim-1:nmos) )
   end do

; copy attributes and add a couple of informational attributes
; only need to do this if plotting or writing to netCDF file

   copy_VarAtts (x,aveMonth)

   aveMonth@time_op_ncl = "Climatology: "+ (ntim/nmos) +" years"
   aveMonth@info    = "function clmMonLLLT: contributed.ncl"

; copy spatial (eg, lat/lon) coordinate variables

   do i=0,2
      if (.not.ismissing(x!i)) then
          aveMonth!i = x!i
          if (iscoord(x,x!i)) then
              aveMonth&$aveMonth!i$ = x&$x!i$
          end if
      end if
   end  do

   aveMonth!3     = "month"           ; create a "month" named dim
   aveMonth&month = ispan(0,nmos-1,1) ; create a month coord var

   return (aveMonth)
end

; ******************************************************************
; D. Shea
; Calculate standard deviations of monthly means 
;
; The time dimension must be a multiple of 12
;
;    x(lev,lat,lon,time)  <==== INPUT DIMENSION ORDER
;
; Usage:  moStd = stdMonLLLT (x)
;         moStd(lev,lat,lon,12)


undef("stdMonLLLT")
function stdMonLLLT (x[*][*][*][*]:numeric)
local dimx, klvl, nlat, mlon, ntim, i, nmo, nmos, stdMonth
begin

   dimx  = dimsizes (x)
   klvl  = dimx(0)
   nlat  = dimx(1)
   mlon  = dimx(2)
   ntim  = dimx(3)

   nmos  = 12
   modCheck ("stdMonLLLT", ntim, nmos)   ; error check
;
; Compute all 12 monthly standard deviations first. 
;
   stdMonth = new((/klvl,nlat,mlon,nmos/),typeof(x) \
                                         ,getFillValue(x))

   do nmo=0,nmos-1
      stdMonth(:,:,:,nmo) = dim_stddev( x(:,:,:,nmo:ntim-1:nmos) )
   end do

; copy attributes and add a couple of informational attributes
; only need to do this if plotting or writing to netCDF file

   copy_VarAtts (x,stdMonth)

   stdMonth@time_op_ncl = " Monthly Standard Deviation: "+ (ntim/nmos) +" years"
   stdMonth@info    = "function stdMonLLLT: contributed.ncl"

; copy spatial (eg, lat/lon) coordinate variables

   do i=0,2
      if (.not.ismissing(x!i)) then
          stdMonth!i = x!i
          if (iscoord(x,x!i)) then
              stdMonth&$stdMonth!i$ = x&$x!i$
          end if
      end if
   end  do

   stdMonth!3     = "month"           ; create a "month" named dim
   stdMonth&month = ispan(0,nmos-1,1) ; create a month coord var

   return (stdMonth)
end

; ------------------------------------------------------------------
; D. Shea
; Calculate anomalies from climatology 
; returned array is same as from "rmMonthAnnualCycleLLLT (x, yr1, yr2)
; Subtract the the long term means from each "month".
; On return x will consist of deviations from  each "month's" long term mean.
;
; The time dimension must be a multiple of 12
;
;     x(,lev,lat,lon,time)  <==== INPUT DIMENSION ORDER
;
; Usage:  x     = calcMonAnomLLLT (x,xAve)  ; overwrites "x"
;         xAnom = calcMonAnomLLLT (x,xAve)  ; creates xAnom
; where   xAve  = clmMonLLLT (x)            ; see previous function 


undef("calcMonAnomLLLT")
function calcMonAnomLLLT (x[*][*][*][*]:numeric, xAve[*][*][*][12]:numeric)
local dimx, ntim, yr, nmos, xAnom
begin

   dimx  = dimsizes (x)
   ntim  = dimx(3)

   nmos  = 12
   modCheck ("calcMonAnomLLLT", ntim, nmos)   ; error check

; Now loop on every year and compute difference. 
; The [yr:yr+nmos-1] strips out 12 months for each year.

   xAnom = x          ; variable to variable copy [meta data]
   do yr=0,ntim-1,nmos
      xAnom(:,:,:,yr:yr+nmos-1) = (/ x(:,:,:,yr:yr+nmos-1)- xAve /)
   end do

; Create an informational attribute:  

   xAnom@anomaly_op_ncl  = "function calcMonAnomLLLT: contributed.ncl"
   return (xAnom)
end


; ******************************************************************
; D. Shea
; Remove that Annual Cycle from "monthly" (nmos=12) data. 
; Subtract the the long term means from each "month".
; On return x will consist of deviations from  each "month's" long term mean.
;
; The time dimension must be a multiple of 12
;
;       x(lat,lon,time)  <==== INPUT DIMENSION ORDER
;
; Usage:  x = rmMonAnnCyLLLT (x)

undef("rmMonAnnCycLLLT")
function rmMonAnnCycLLLT (x[*][*][*][*]:numeric)
local dimx, ntim, xAve, xAnom, nmos
begin
   dimx = dimsizes(x)
   ntim = dimx(3)

   nmos  = 12
   modCheck ("rmMonAnnCycLLLT", ntim, nmos)   ; error check

   xAve  = clmMonLLLT (x)            ; Compute all 12 monthly averages first. 
   xAnom = calcMonAnomLLLT (x,xAve)  ; Remove the mean from each year-month grid

; Create an informational attribute 

   xAnom@anomaly_op_ncl  = "Annual Cycle Removed:function rmMonAnnCycLLLT:contributed.ncl" 
   xAnom@reference   = "function rmMonAnnCycLLLT in contrib.ncl"
   return (xAnom)
end
; ******************************************************************
; D. Shea
; Calculate long term monthly means (monthly climatology)
; requires named dimensions
;
; The time dimension must be a multiple of 12
;
;    x(time,lev,lat,lon)  <==== INPUT DIMENSION ORDER
;
; Usage:  moClm = clmMonTLLL (x)
; Output: moClm(12,lev,lat,lon)

undef("clmMonTLLL")
function clmMonTLLL (x[*][*][*][*]:numeric)
local dimx, klvl, ntim, nlat, mlon, i, nmo, nmos, monAveLLT
begin
  dimx = dimsizes(x)
  ntim = dimx(0)

  nmos  = 12
  modCheck ("clmMonTLLL", ntim, nmos)   ; error check
 ;rankCheck("clmMonTLLL", x, 3)         ; not needed due to prototyping
      
  klvl = dimx(1)
  nlat = dimx(2)
  mlon = dimx(3)
;
; Compute all 12 monthly averages first. 
;
   aveMonth = new((/nmos,klvl,nlat,mlon/),typeof(x) \
                                         ,getFillValue(x))
   do nmo=0,nmos-1
      aveMonth(nmo,:,:,:) = dim_avg_n( x(nmo:ntim-1:nmos,:,:,:), 0 )
   end do

; copy attributes and add a couple of informational attributes
; only need to do this if plotting or writing to netCDF file

   copy_VarAtts (x,aveMonth)
   aveMonth@time_op_ncl = "Climatology: "+ (ntim/nmos) +" years"
   aveMonth@info    = "function clmMonTLLL: contributed.ncl"

; copy spatial (eg, lat/lon) coordinate variables

   do i=1,3
      if (.not.ismissing(x!i)) then
          aveMonth!i = x!i
          if (iscoord(x,x!i)) then
              aveMonth&$aveMonth!i$ = x&$x!i$
          end if
      end if
   end  do

   aveMonth!0     = "month"           ; create a "month" named dim
   aveMonth&month = ispan(0,nmos-1,1) ; create a month coord var

   return (aveMonth)
end

; ******************************************************************
; D. Shea
; Calculate standard deviations of monthly means (interannual var)
;
; The time dimension must be a multiple of 12
;
;    x(time,klvl,lat,lon)  <==== INPUT DIMENSION ORDER
;
; Usage:  moStd = stdMonTLLL (x)
; Output: moStd(12,lev,lat,lon)

undef("stdMonTLLL")
function stdMonTLLL (x[*][*][*][*]:numeric)
local dimx, klvl, ntim, nlat, mlon, i, nmo, nmos, stdMonth
begin
  dimx = dimsizes(x)
  ntim = dimx(0)

  nmos  = 12
  modCheck ("stdMonTLLL", ntim, nmos)   ; error check
 ;rankCheck("stdMonTLLL", x, 3)         ; not needed due to prototyping
      
  klvl = dimx(1)
  nlat = dimx(2)
  mlon = dimx(3)
;
; Compute all 12 standard deviations first. 
;
   stdMonth = new((/nmos,klvl,nlat,mlon/),typeof(x) \
                                         ,getFillValue(x))
   do nmo=0,nmos-1
      stdMonth(nmo,:,:,:) = dim_stddev_n( x(nmo:ntim-1:nmos,:,:,:), 0 )
   end do

; copy attributes and add a couple of informational attributes
; only need to do this if plotting or writing to netCDF file

   copy_VarAtts (x,stdMonth)
   stdMonth@time_op_ncl = "Monthly Standard Deviation: "+ (ntim/nmos) +" years"
   stdMonth@info    = "function stdMonTLLL: contributed.ncl"

; copy spatial (eg, lat/lon) coordinate variables

   do i=1,3
      if (.not.ismissing(x!i)) then
          stdMonth!i = x!i
          if (iscoord(x,x!i)) then
              stdMonth&$stdMonth!i$ = x&$x!i$
          end if
      end if
   end  do

   stdMonth!0     = "month"           ; create a "month" named dim
   stdMonth&month = ispan(0,nmos-1,1) ; create a month coord var

   return (stdMonth)
end
;**********************************************************************
; D. Shea
; Calculate anomalies from climatology [Remove annual cycle]
;
; Subtract the the long term means from each "month".
; On return x will consist of deviations from  each "month's" long term mean.
;
; The time dimension must be a multiple of 12
;
;     x(time,lat,lon)  <==== INPUT DIMENSION ORDER
;     x!0 = "time"
;     x!1 = "lat"
;     x!2 = "lon"
;     xAve(12,lat,lon) <==== THE 12 is Jan, Feb, .., Dec
;
; Usage:  x     = calcMonAnomTLL (x,xAve)  ; overwrites "x"
;         xAnom = calcMonAnomTLL (x,xAve)  ; creates xAnom as new variable
; where   xAve  = clmMonTLL (x)           

undef("calcMonAnomTLL")
function calcMonAnomTLL (x[*][*][*]:numeric, xAve[12][*][*]:numeric)
local dimx, ntim, yr, nmos, xAnom
begin
   dimx  = dimsizes (x)
   ntim  = dimx(0)

   nmos  = 12
   modCheck ("calcMonAnomTLL", ntim, nmos)   ; error check
  ;rankCheck("calcMonAnomTLL", x, 3)         ; not needed due to prototyping

; Now loop on every year and compute difference. 
; The [yr:yr+nmos-1] strips out 12 months for each year. [array notation]

   xAnom = x          ; variable to variable copy [meta data]
   do yr=0,ntim-1,nmos
      xAnom(yr:yr+nmos-1,:,:) = (/ x(yr:yr+nmos-1,:,:)- xAve /)
   end do

; Create an informational attribute:  

   xAnom@anomaly_op_ncl  = "Anomalies from Annual Cycle: calcMonAnomTLL: contributed.ncl" 
   
   return (xAnom)
end
;**********************************************************************
; D. Shea
; Calculate anomalies from climatology [Remove annual cycle]
;
; Subtract the the long term means from each "month".
; On return x will consist of deviations from  each "month's" long term mean.
;
; The time dimension must be a multiple of 12
;
;     x(time,lev,lat,lon)  <==== INPUT DIMENSION ORDER
;     x!0 = "time"
;     x!1 = "lev"
;     x!2 = "lat"
;     x!3 = "lon"
;     xAve(12,lev,lat,lon) <==== THE 12 is Jan, Feb, .., Dec
;
; Usage:  x     = calcMonAnomTLLL (x,xAve)  ; overwrites "x"
;         xAnom = calcMonAnomTLLL (x,xAve)  ; creates xAnom as new variable
; where   xAve  = clmMonTLLL (x)           

undef("calcMonAnomTLLL")
function calcMonAnomTLLL (x[*][*][*][*]:numeric, xAve[12][*][*][*]:numeric)
local dimx, ntim, yr, nmos, xAnom
begin
   dimx  = dimsizes (x)
   ntim  = dimx(0)

   nmos  = 12
   modCheck ("calcMonAnomTLLL", ntim, nmos)   ; error check
  ;rankCheck("calcMonAnomTLLL", x, 3)         ; not needed due to prototyping

; Now loop on every year and compute difference. 
; The [yr:yr+nmos-1] strips out 12 months for each year. [array notation]

   xAnom = x          ; variable to variable copy [meta data]
   do yr=0,ntim-1,nmos
      xAnom(yr:yr+nmos-1,:,:,:) = (/ x(yr:yr+nmos-1,:,:,:)- xAve /)
   end do

; Create an informational attribute:  

   xAnom@anomaly_op_ncl  = "Anomalies from Annual Cycle: calcMonAnomTLLL: contributed.ncl" 
   
   return (xAnom)
end

; ******************************************************************
; D. Shea
; Remove the Annual Cycle from "monthly" (nmos=12) data. 
; Subtract the the long term means from each "month".
; On return x will consist of deviations from  each "month's" long term mean.
;
; The time dimension must be a multiple of 12
;
;       x(time,lat,lon)  <==== INPUT DIMENSION ORDER
;       x!0 = "time"
;       x!1 = "lat"
;       x!2 = "lon"
;
; Usage:  x = rmMonAnnCycTLL (x)

undef("rmMonAnnCycTLL")
function rmMonAnnCycTLL (x[*][*][*]:numeric)
local dimx, ntim, nmos, xAve, xAnom
begin
   dimx  = dimsizes (x)
   ntim  = dimx(0)

   nmos  = 12
   modCheck ("rmMonAnnCycTLL", ntim, nmos)   ; error check

   xAve  = clmMonTLL (x)  ; Compute all 12 monthly averages first. [12,lat,lon] 
   xAnom = calcMonAnomTLL (x,xAve)  ; Remove the mean from each year-month grid

; Create an informational attribute 

   xAnom@anomaly_op_ncl  = "Annual Cycle Removed: rmMonAnnCycTLL: contributed.ncl" 
   return (xAnom)
end

; =====================================
undef("clmDayTLL")
function clmDayTLL (x[*][*][*]:numeric, yyyyddd:integer)   
;
; calculate the mean Annual Cycle from daily data. 
; The return array will gave the raw climatology at each grid point
;
;              x(time,lat,lon)  <==== input dimension order
;              x!0 = "time"     <==== time is in days
;              x!1 = "lat"
;              x!2 = "lon"
;
;    non-Leap  yyyyddd
;              1905001 =>  Jan  1, 1905
;              1905032 =>  Feb  1, 1905
;              1905059 =>  Feb 28, 1905
;              1905060 =>  Mar  1, 1905
;              1905365 =>  Dec 31, 1905
;
;    Leap
;              1908001 =>  Jan  1, 1908]
;              1908032 =>  Feb  1, 1908]
;              1908059 =>  Feb 28, 1908]
;              1908060 =>  Feb 29, 1908]
;              1908061 =>  Mar  1, 1908]
;              1908366 =>  Dec 31, 1908]
;
; Usage:  xClmDay = clmDAY_TLL (x, yyyyddd)
; -------

local dimx, ntim, nlat, mlon, ndys, days, clmDay, ndy, indx, year_day, nFill
begin

   dimx  = dimsizes (x)

   ntim  = dimx(0)
   nlat  = dimx(1)
   mlon  = dimx(2)
   if (isatt(yyyyddd,"calendar")) then
       if (yyyyddd@calendar.eq."360_day" .or. yyyyddd@calendar.eq."360") then
           ndys = 360
       end if
       if (yyyyddd@calendar.eq."365_day" .or. yyyyddd@calendar.eq."365"      .or. \
           yyyyddd@calendar.eq."noleap"  .or. yyyyddd@calendar.eq."no_leap") then
           ndys  = 365
       end if
       if (yyyyddd@calendar.eq."366_day" .or. yyyyddd@calendar.eq."366"      .or. \
           yyyyddd@calendar.eq."allleap" .or. yyyyddd@calendar.eq."all_leap") then
           ndys  = 366
       end if
       if (yyyyddd@calendar.eq."standard" .or. yyyyddd@calendar.eq."gregorian") then
           ndys  = 366
       end if
   else
       ndys  = 366     ; default
   end if

   days  = yyyyddd - (yyyyddd/1000)*1000           ; strip year info [ddd]
   clmDay= new((/ndys,nlat,mlon/),typeof(x), getFillValue(x) ) ; daily climatology

;
; Compute averages for each sequential day of the year. 
;
   do ndy=0,ndys-1                                 
      indx = ind( days.eq.(ndy+1) )               
      if (.not.ismissing(indx(0))) then
          nindx = dimsizes(indx)
          if (nindx.eq.1) then                       ; force 3rd dimension
              clmDay(ndy,:,:) = dim_avg_n(x(indx:indx,:,:), 0) 
          else
              clmDay(ndy,:,:) = dim_avg_n(x(indx,:,:), 0) 
          end if
      end if
      delete(indx)                  
   end do

   if (.not.isatt(yyyyddd,"calendar") .or. \
       isatt(yyyyddd,"calendar") .and. yyyyddd@calendar.eq."standard" .or. \
                                       yyyyddd@calendar.eq."gregorian") then
                                                   ; nominal day 366 
                                                   ; ave(31 Dec + 1 Jan)=leap
       clmDay(ndys-1,:,:) = (clmDay(0,:,:) + clmDay(ndys-2,:,:))*0.5
   end if

   nFill = num(ismissing(clmDay))
   if (nFill.eq.0) then
       delete(clmDay@_FillValue)
   end if

   clmDay@long_name   = "Daily Climatology"
   if (isatt(x,"long_name")) then
       clmDay@long_name = clmDay@long_name +": "+x@long_name
   end if
   if (isatt(x,"units")) then
       clmDay@units     = x@units
   end if
   clmDay@information = "Raw daily averages across all years"
   clmDay@smoothing   = "None"

   year_day           = ispan(1,ndys,1)
   year_day@long_name = "day of year"
   year_day@units     = "ddd"

   clmDay!0           = "year_day"
   clmDay&year_day    =  year_day
   
   copy_VarCoords(x(0,:,:), clmDay(0,:,:))   ; trick
   if (isatt(clmDay,"year_day")) then
       delete(clmDay@year_day)               ; clean up
   end if

   if (isatt(yyyyddd,"calendar")) then
       clmDay@calendar = yyyyddd@calendar 
   end if
 
   return (clmDay)
end
; =====================================
undef("clmDayT")
; Too lazy to write an explicit function
function clmDayT (x[*]:numeric, yyyyddd:integer)   
local ntim, x3d, x3dClm, xClm
begin
    ntim   = dimsizes(x)
    x3d    = conform_dims( (/ntim,1,1/), x, 0)
    x3dClm = clmDayTLL(x3d, yyyyddd)
    xClm   = x3dClm(:,0,0)
    return(xClm)
end
; =====================================
undef("calcDayAnomT")
; Too lazy to write an explicit function
function calcDayAnomT (x[*]:numeric, yyyyddd[*]:integer, clmDay[*]:numeric)   
local ntim, days, xAnom
begin
   if (isatt(yyyyddd,"calendar") .and. isatt(clmDay,"calendar")) then
       if (yyyyddd@calendar .ne. clmDay@calendar) then
           print("calcDayAnomT: calendar mismatch")
           print("              yyyyddd@calendar = "+yyyyddd@calendar)
           print("               clmday@calendar = "+ clmDay@calendar)
           exit
       end if
   end if

   ntim  = dimsizes (x)
   ddd   = yyyyddd - (yyyyddd/1000)*1000      ; strip year info [yyyy]

; loop on every day and compute difference. 

   xAnom = (/ x  /)                           ; create xAnom
   do nt=0,ntim-1
      xAnom(nt) = x(nt) - clmDay(ddd(nt)-1)   ; -1 for 0-based subscript
   end do

   if (isatt(x,"long_name")) then
        xAnom@long_name = "Anomalies: "+x@long_name
    else
        xAnom@long_name = "Anomalies from Daily Climatology"
    end if
    if (isatt(x,"units")) then
        xAnom@units = x@units
    end if
    if (isatt(yyyyddd,"calendar")) then
        xAnom@calendar = yyyyddd@calendar
    end if

    copy_VarCoords(x, xAnom)
    return(xAnom)
end

; =====================================
undef("clmDayTLLL")
function clmDayTLLL (x[*][*][*][*]:numeric, yyyyddd:integer)   
;
; calculate the mean Annual Cycle from daily data. 
; The return array will gave the raw climatology at each grid point
;
;              x(time,lev,lat,lon)  <==== input dimension order
;              x!0 = "time"         <==== time is in days
;              x!1 = "lev"
;              x!2 = "lat"
;              x!3 = "lon"
;
;    non-Leap  yyyyddd
;              1905001 =>  Jan  1, 1905
;              1905032 =>  Feb  1, 1905
;              1905059 =>  Feb 28, 1905
;              1905060 =>  Mar  1, 1905
;              1905365 =>  Dec 31, 1905
;
;    Leap
;              1908001 =>  Jan  1, 1908]
;              1908032 =>  Feb  1, 1908]
;              1908059 =>  Feb 28, 1908]
;              1908060 =>  Feb 29, 1908]
;              1908061 =>  Mar  1, 1908]
;              1908366 =>  Dec 31, 1908]
;
; Usage:  xClmDay = clmDAYTLLL (x, yyyyddd)
; -------

local dimx, ntim, klev, nlat, mlon, ndys, days, clmDay, ndy, indx, year_day, nFill
begin

   dimx  = dimsizes (x)

   ntim  = dimx(0)
   klev  = dimx(1)
   nlat  = dimx(2)
   mlon  = dimx(3)
   if (isatt(yyyyddd,"calendar")) then
       if (yyyyddd@calendar.eq."360_day" .or. yyyyddd@calendar.eq."360") then
           ndys = 360
       end if
       if (yyyy@calendar.eq."365_day" .or. yyyy@calendar.eq."365"      .or. \
           yyyy@calendar.eq."noleap"  .or. yyyy@calendar.eq."no_leap") then
           ndys  = 365
       end if
       if (yyyy@calendar.eq."366_day" .or. yyyy@calendar.eq."366"      .or. \
           yyyy@calendar.eq."allleap" .or. yyyy@calendar.eq."all_leap") then
           ndys  = 366
       end if
       if (yyyy@calendar.eq."standard" .or. yyyy@calendar.eq."gregorian") then
           ndys  = 366
       end if
   else
       ndys  = 366     ; default
   end if

   days  = yyyyddd - (yyyyddd/1000)*1000           ; strip year info [yyyy]

   clmDay= new((/ndys,klev,nlat,mlon/),typeof(x), getFillValue(x) ) ; daily climatology
;
; Compute averages for each sequential day of the year. 
; This uses dimension swapping.
;
   do ndy=0,ndys-2                                 ; ndy=0->364 ==> day of year 1->365       
      indx = ind( days.eq.(ndy+1) )                ; indx:indx Mar 2012, handle 1 element
      if (.not.ismissing(indx(0))) then
          nindx = dimsizes(indx)
          if (nindx.eq.1) then                     ; force 3rd dimension
              clmDay(ndy,:,:,:) = dim_avg_n(x(indx:indx,:,:,:), 0)
          else
              clmDay(ndy,:,:,:) = dim_avg_n(x(indx,:,:,:), 0)
          end if
      end if
      delete(indx)                  
   end do

   if (.not.isatt(yyyyddd,"calendar") .or. \
       isatt(yyyyddd,"calendar") .and. yyyyddd@calendar.eq."standard" .or. \
                                       yyyyddd@calendar.eq."gregorian") then
                                              ; nominal day 366 
                                              ; ave(31 Dec + 1 Jan)=leap
      clmDay(ndys-1,:,:,:) = (clmDay(0,:,:,:) + clmDay(ndys-2,:,:,:))*0.5
   end if

   nFill = num(ismissing(clmDay))
   if (nFill.eq.0) then
       delete(clmDay@_FillValue)
   end if

   clmDay@long_name   = "Daily Climatology"
   if (isatt(x,"long_name")) then
       clmDay@long_name = clmDay@long_name +": "+x@long_name
   end if
   if (isatt(x,"units")) then
       clmDay@units     = x@units
   end if
   clmDay@information = "Raw daily averages across all years"
   clmDay@smoothing   = "None"

   year_day           = ispan(1,ndys,1)
   year_day@long_name = "day of year"
   year_day@units     = "ddd"

   clmDay!0           = "year_day"
   clmDay&year_day    =  year_day
   
   copy_VarCoords(x(0,:,:,:), clmDay(0,:,:,:))   ; trick
   delete(clmDay@year_day)                   ; clean up

   if (isatt(yyyyddd,"calendar")) then
       clmDay@calendar = yyyyddd@calendar 
   end if
 
   return (clmDay)
end
; =====================================
undef("smthClmDayT")
function smthClmDayT (clmDay[*]:numeric, nHarm:integer)
;
local cf, clmDaySmth 
begin
  
; The following was commented out (post 6.2.0) when people started using 
; this on daily SSTs which, of course, have _FillValue over land.
 
 ;if (isatt(clmDay, "_FillValue")) then
 ;    nFill = num(ismissing(clmDay))
 ;    if (nFill.gt.0) then
 ;        print("smthClmDayTLL: No missing values allowed: ezfftf does not allow")
 ;        print("smthClmDayTLL: nFill="+nFill)
 ;        exit  
 ;    end if
 ;end if

  cf = ezfftf_n( clmDay, 0)                 ; [2] x [nday/2]
                                            ; remember NCL is 0-based 
                                            ; cf(:,0:nHarm-1) are retained unaltered
  cf(:,nHarm   ) = 0.5*cf(:,nHarm)          ; mini-taper
  cf(:,nHarm+1:) = 0.0                      ; set all higher coef to 0.0

  clmDaySmth  = ezfftb_n( cf, cf@xbar, 1)   ; reconstructed series

  copy_VarMeta(clmDay, clmDaySmth)
  clmDaySmth@information = "smthClmDayT: Smoothed daily climatological averages"
  clmDaySmth@smoothing   = "FFT: "+nHarm+" harmonics were retained."
  return(clmDaySmth)
end
; =====================================
undef("smthClmDayTLL")
function smthClmDayTLL (clmDay[*][*][*]:numeric, nHarm:integer)
;
local nFill, dn, cf, clmDaySmth 
begin
  
; The following was commented out (post 6.2.0) when people started using 
; this on daily SSTs which, of course, have _FillValue over land.
 
 ;if (isatt(clmDay, "_FillValue")) then
 ;    nFill = num(ismissing(clmDay))
 ;    if (nFill.gt.0) then
 ;        print("smthClmDayTLL: No missing values allowed: ezfftf does not allow")
 ;        print("smthClmDayTLL: nFill="+nFill)
 ;        exit  
 ;    end if
 ;end if

; Historical stuff: Not needed
 ;dn = getvardims(clmDay)                   ; get dimension names
 ;if (dn(0).ne."year_day") then
 ;    print("smthClmDayTLL: Warning: Usually expect year_day to be the dimension name")
 ;end if

  cf = ezfftf_n( clmDay, 0)                 ; [2] x [nday/2] x [nlat] x [mlon]

                                            ; remember NCL is 0-based 
                                            ; cf(:,0:nHarm-1) are retained unaltered
  cf(:,nHarm,:,:   ) = 0.5*cf(:,nHarm,:,:)  ; mini-taper
  cf(:,nHarm+1:,:,:) = 0.0                  ; set all higher coef to 0.0

  clmDaySmth  = ezfftb_n( cf, cf@xbar, 1)   ; reconstructed series

  copy_VarMeta(clmDay, clmDaySmth)
  clmDaySmth@information = "Smoothed daily climatological averages"
  clmDaySmth@smoothing   = "FFT: "+nHarm+" harmonics were retained."

  return(clmDaySmth)
end
; =====================================
undef("smthClmDayTLLL")
function smthClmDayTLLL (clmDay[*][*][*][*]:numeric, nHarm:integer)
local nFill, dn, z, cf, clmDaySmth
begin
  
; The following was commented out (post 6.2.0) when people started using 
; this on daily ocean which, of course, have _FillValue over land.

 ;if (isatt(clmDay, "_FillValue")) then
 ;    nFill = num(ismissing(clmDay))
 ;    if (nFill.gt.0) then
 ;        print("smthClmDayTLLL: No missing values allowed: ezfftf does not allow")
 ;        print("smthClmDayTLLL: nFill="+nFill)
 ;        exit
 ;    end if
 ;end if

; Historical stuff: Not needed
 ;dn = getvardims(clmDay)                   ; get dimension names
 ;if (dn(0).ne."year_day") then
 ;    print("smthClmDayTLLL: Warning: Usually expect year_day to be the dimension name")
 ;end if

  cf = ezfftf_n( clmDay, 0 )                ; [2] x [nday/2] x [klev] x [nlat] x [mlon]
                                            ; remember NCL is 0-based
                                            ; cf(:,0:nHarm-1) are retained unaltered
  cf(:,nHarm,:,:,:   ) = 0.5*cf(:,nHarm,:,:,:); mini-taper
  cf(:,nHarm+1:,:,:,:) = 0.0                ; set all higher coef to 0.0

  clmDaySmth  = ezfftb_n( cf, cf@xbar, 1)                 ; reconstructed series

  copy_VarMeta(clmDay, clmDaySmth)
  clmDaySmth@information = "Smoothed daily climatological averages"
  clmDaySmth@smoothing   = "FFT: "+nHarm+" harmonics were retained."

  return(clmDaySmth)
end
; =====================================
;
; Calculate daily  temporal means.
; This is a more general form of clmDayTLL
; It works on any dimension array and allows for multiple 
; samples per day.
;
; It has not been extensively tested and it is not documented.
; Consider this unsupported at this time.
;
undef("clmDayHr")
function clmDayHr(z:numeric, yyyymmddhh[*]:integer)
begin
  yyyy = yyyymmddhh/1000000
  mdh  = yyyymmddhh%1000000
  mm   = mdh/10000
  dh   = mdh%10000
  dd   = dh/100
  hh   = dh-dd*100

  if (isatt(yyyymmddhh,"calendar")) then
      yyyy@calendar = yyyymmddhh@calendar     ; needed for 'day_of_year'
  end if

  NTIM   = dimsizes(yyyymmddhh)     ; all time steps

  dddhh  = day_of_year(yyyy, mm, dd)*100 + hh
  yyyydddhh = yyyy*100000 +  dddhh

  if (isatt(yyyy,"calendar")) then
      yyyydddhh@calendar = yyyy@calendar
  end if

  dhh    = hh(1)-hh(0)              ; assume constant 
  nsmp   = 24/dhh                   ; # samples/day                          

  if (.not.isatt(yyyy,"calendar") .or. \
      isatt(yyyy,"calendar") .and. yyyy@calendar.eq."standard" .or. \
                                   yyyy@calendar.eq."gregorian") then
      nday   = 365
      ndayx  = 366                  ; including leap year
  end if
  if (isatt(yyyy,"calendar")) then
      if (yyyy@calendar.eq."none") then
          print("clmDayHr: yyyy@calendar=none: I have no idea what to do")
          exit
      end if
      if (yyyy@calendar.eq."360_day" .or. yyyy@calendar.eq."360") then
          nday  = 360
          ndayx = 360
      end if
      if (yyyy@calendar.eq."365_day" .or. yyyy@calendar.eq."365"      .or. \
          yyyy@calendar.eq."noleap"  .or. yyyy@calendar.eq."no_leap") then
          nday  = 365
          ndayx = 365
      end if
      if (yyyy@calendar.eq."366_day" .or. yyyy@calendar.eq."366"      .or. \
          yyyy@calendar.eq."allleap" .or. yyyy@calendar.eq."all_leap") then
          nday  = 366
          ndayx = 366
      end if
  end if

  ntimx  = nsmp*ndayx

  dimz   = dimsizes(z)
  rankz  = dimsizes(dimz)

;---------------------------------------------------------------
; Create array to hold climatology and counts.
;---------------------------------------------------------------
  if (typeof(z).eq."double") then
      zType = "double"
      zFill =  1d20
  else
      zType = "float"
      zFill =  1e20
  end if

  if (rankz.eq.4) then
      klev   = dimz(1)
      nlat   = dimz(2)
      mlon   = dimz(3)
      zClm   = new ( (/ntimx,klev,nlat,mlon/), zType, zFill)
  end if
  if (rankz.eq.3) then
      nlat   = dimz(1)
      mlon   = dimz(2)
      zClm   = new ( (/ntimx,nlat,mlon/), zType, zFill)
  end if
  if (rankz.eq.2) then
      npts   = dimz(1)
      zClm   = new ( (/ntimx,npts/), zType, zFill)
  end if
  if (rankz.eq.1) then
      zClm   = new ( (/ntimx/), zType, zFill)
  end if

  yrStrt = yyyy(0)
  yrLast = yyyy(NTIM-1)
  nyrs   = yrLast-yrStrt+1 
;---------------------------------------------------------------
; Climatologies: Loop over each climate day and hour
;---------------------------------------------------------------
  nt = -1
  do ndy=1,nday
    do nhr=hh(0),hh(nsmp-1),dhh
       nt    = nt+1
       indx = ind(dddhh.eq.(ndy*100+nhr))  

       if (rankz.eq.4) then
           zClm(nt:nt,:,:,:) = dim_avg_n(z(indx,:,:,:), 0)
       end if
       if (rankz.eq.3) then
           zClm(nt:nt,:,:)   = dim_avg_n(z(indx,:,:), 0)
       end if
       if (rankz.eq.2) then
           zClm(nt:nt,:)     = dim_avg_n(z(indx,:), 0)
       end if
       if (rankz.eq.1) then
           zClm(nt:nt)       = dim_avg_n(z(indx), 0)
       end if

       delete(indx)
    end do
  end do

;---------------------------------------------------------------
; Fill in climatological day 366 with the average
; of Jan 1 (day 1) and 31 Dec (day 365)
; greorian, standard or no calendar attribute
;---------------------------------------------------------------
  if (nday.eq.365 .and. ndayx.eq.366) then
      do ns=0,nsmp-1
    
         if (rankz.eq.4) then
             zClm(ntim+ns,:,:,:) = 0.5*(zClm(ns,:,:,:)+zClm(ntim-(nsmp-ns),:,:,:))
         end if
         if (rankz.eq.3) then
             zClm(ntim+ns,:,:)   = 0.5*(zClm(ns,:,:)+zClm(ntim-(nsmp-ns),:,:))
         end if
         if (rankz.eq.2) then
             zClm(ntim+ns,:)     = 0.5*(zClm(ns,:)+zClm(ntim-(nsmp-ns),:))
         end if
         if (rankz.eq.1) then
             zClm(ntim+ns)       = 0.5*(zClm(ns)+zClm(ntim-(nsmp-ns)))
         end if
    
      end do
  end if

;---------------------------------------------------------------
; Meta data
;---------------------------------------------------------------
  dt        = 1.0/nsmp
  day       = fspan(1.00, ndayx+1-dt, ntimx)
  day@long_name = "day and fraction of day"
  day!0     = "day"
  day&day   =  day

  zClm!0    = "day"
  zClm&day  =  day 

  if (rankz.eq.4) then
      copy_VarCoords(z(0,:,:,:), zClm(0,:,:,:))
  end if
  if (rankz.eq.3) then
      copy_VarCoords(z(0,:,:), zClm(0,:,:))
  end if
  if (rankz.eq.2) then
      copy_VarCoords(z(0,:), zClm(0,:))
  end if

  if (isatt(z,"long_name")) then
      zClm@long_name = z@long_name
  end if
  if (isatt(z,"units")) then
      zClm@units     = z@units
  end if

  return(zClm)
end


; =====================================
undef("calcDayAnomTLL")
function calcDayAnomTLL (x[*][*][*]:numeric, yyyyddd:integer, clmDay[*][*][*]:numeric)   

; Remove the Annual Cycle from daily data. 
; On return x will consist of deviations from each day's long term mean.
;
;              x(time,lat,lon)  <==== input dimension order
;              x!0 = "time"     <==== time is in days
;              x!1 = "lat"
;              x!2 = "lon"
;
;
;    non-Leap  yyyyddd
;              1905001 =>  Jan  1, 1905
;              1905032 =>  Feb  1, 1905
;              1905059 =>  Feb 28, 1905
;              1905060 =>  Mar  1, 1905
;              1905365 =>  Dec 31, 1905
;
;    Leap
;              1908001 =>  Jan  1, 1908]
;              1908032 =>  Feb  1, 1908]
;              1908059 =>  Feb 28, 1908]
;              1908060 =>  Feb 29, 1908]
;              1908061 =>  Mar  1, 1908]
;              1908366 =>  Dec 31, 1908]

; Usage:  xAnom = calcDayAnomTLL (x, yyyyddd, clmDay)

local dimx, ntim, nlat, mlon, days, xAnom, nt
begin
   if (isatt(yyyyddd,"calendar") .and. isatt(clmDay,"calendar")) then  
       if (yyyyddd@calendar .ne. clmDay@calendar) then
           print("calcDayAnomTLL: calendar mismatch")
           print("                yyyyddd@calendar = "+yyyyddd@calendar)
           print("                 clmday@calendar = "+ clmDay@calendar)
           exit
       end if
   end if

   dimx  = dimsizes (x)

   ntim  = dimx(0)
   nlat  = dimx(1)
   mlon  = dimx(2)

   days  = yyyyddd - (yyyyddd/1000)*1000      ; strip year info [yyyy]

; loop on every day and compute difference. 

   xAnom = (/ x  /)                                 ; create xAnom
   do nt=0,ntim-1
      xAnom(nt,:,:) = x(nt,:,:) - clmDay(days(nt)-1,:,:)   ; -1 for 0-based subscript
   end do

   if (isatt(x,"long_name")) then
        xAnom@long_name = "Anomalies: "+x@long_name
    else
        xAnom@long_name = "Anomalies from Daily Climatology"
    end if
    if (isatt(x,"units")) then
        xAnom@units = x@units
    end if
    if (isatt(yyyyddd,"calendar")) then
        xAnom@calendar = yyyyddd@calendar
    end if
 
    copy_VarCoords(x, xAnom)
 
    return(xAnom)
 end
 
 ; *******************************************************************
 undef("calcDayAnomTLLL")
 function calcDayAnomTLLL (x[*][*][*][*]:numeric, yyyyddd:integer, clmDay[*][*][*][*]:numeric)   
 
 ; Remove the Annual Cycle from daily data. 
 ; On return x will consist of deviations from each day's long term mean.
 ;
 ;              x(time,lev,lat,lon)  <==== input dimension order
 ;              x!0 = "time"         <==== time is in days
 ;              x!1 = "lev"
 ;              x!2 = "lat"
 ;              x!3 = "lon"
 ;
 ;
 ;    non-Leap  yyyyddd
 ;              1905001 =>  Jan  1, 1905
 ;              1905032 =>  Feb  1, 1905
 ;              1905059 =>  Feb 28, 1905
 ;              1905060 =>  Mar  1, 1905
 ;              1905365 =>  Dec 31, 1905
 ;
 ;    Leap
 ;              1908001 =>  Jan  1, 1908]
 ;              1908032 =>  Feb  1, 1908]
 ;              1908059 =>  Feb 28, 1908]
 ;              1908060 =>  Feb 29, 1908]
 ;              1908061 =>  Mar  1, 1908]
 ;              1908366 =>  Dec 31, 1908]
 
 ; Usage:  xAnom = calcDayAnomTLLL (x, yyyyddd, clmDay)
 
 local dimx, ntim, klev,nlat, mlon, ndys, days, xAnom, nt
 begin
    if (isatt(yyyyddd,"calendar") .and. isatt(clmDay,"calendar")) then  
        if (yyyyddd@calendar .ne. clmDay@calendar) then
            print("calcDayAnomTLL: calendar mismatch")
            print("                yyyyddd@calendar = "+yyyyddd@calendar)
            print("                 clmday@calendar = "+ clmDay@calendar)
            exit
        end if
    end if
 
    dimx  = dimsizes (x)
 
    ntim  = dimx(0)
    klev  = dimx(1)
    nlat  = dimx(2)
    mlon  = dimx(3)
 
    days  = yyyyddd - (yyyyddd/1000)*1000      ; strip year info [yyyy]
 
 ; loop on every day and compute difference. 
 
    xAnom = (/ x  /)                                 ; create xAnom
    do nt=0,ntim-1
       xAnom(nt,:,:,:) = x(nt,:,:,:) - clmDay(days(nt)-1,:,:,:)   ; -1 for 0-based subscript
    end do
 
    if (isatt(x,"long_name")) then
        xAnom@long_name = "Anomalies: "+x@long_name
    else
        xAnom@long_name = "Anomalies from Daily Climatology"
    end if
    if (isatt(x,"units")) then
        xAnom@units = x@units
    end if
 
    if (isatt(yyyyddd,"calendar")) then
        xAnom@calendar = yyyyddd@calendar
    end if
 
    copy_VarCoords(x, xAnom)
    return(xAnom)
 end
 
 ; **********************************************************************
 ; D. Shea
 ; create different view of variable
 ; v6.2.0: calendar attribute recognized
 
undef("yyyymmdd_to_yyyyddd")
function yyyymmdd_to_yyyyddd (yyyymmdd[*]:integer)
local ntim, yyyy, mmdd, mm, dd, yyyyddd
begin
  ntim = dimsizes(yyyymmdd)

;;if (isatt(yyyymmdd,"calendar") .and. yyyymmdd@calendar.eq."proleptic_gregorian") then  
;;    print("yyyymmdd_to_yyyyddd:  proleptic_gregorian calendar not supported")
;;    yyyyddd = new(ntim, "integer", -9999)
;;    yyyyddd@long_name = "yyyymmdd_to_yyyyddd:  proleptic_gregorian calendar not supported"
;;    return(yyyyddd)
;;end if

  yyyy = yyyymmdd/10000
  mmdd = yyyymmdd - (yyyy*10000)
  mm   = mmdd/100
  dd   = mmdd - (mm*100)
 
  if (isatt(yyyymmdd,"calendar")) then     ; needed for day_of_year
      yyyy@calendar = yyyymmdd@calendar
  end if
 
  yyyyddd  = yyyy*1000 +day_of_year(yyyy, mm, dd)
  copy_VarMeta(yyyymmdd, yyyyddd)
  yyyyddd@long_name = "yyyy and day_of_year"
  yyyyddd@units = "yyyyddd"

  return(yyyyddd)
end

; **********************************************************************
; D. Shea
; v6.2.0: calendar attribute recognized

undef("yyyyddd_to_yyyymmdd")
function yyyyddd_to_yyyymmdd ( yyyyddd[*]:integer )
; convert yyyyddd (year|day_of_year) to yyyymmdd

local nTime, yyyy, ddd, days, yyyymmdd, nt, iyr \
    , ndyr, skip, nmo, ndy, ndymo, foo
begin
  nTime    = dimsizes( yyyyddd )

;;if (isatt(yyyyddd,"calendar") .and. yyyyddd@calendar.eq."proleptic_gregorian") then  
;;    print("yyyyddd_to_yyyymmdd:  proleptic_gregorian calendar not supported")
;;    yyyymmdd = new(nTime, "integer", -9999)
;;    yyyymmdd@long_name = "yyyyddd_to_yyyymmdd:  proleptic_gregorian calendar not supported"
;;    return(yyyymmdd)
;;end if

  yyyy     = yyyyddd/1000
  ddd      = yyyyddd - yyyy*1000
           ; non leap and leap [greorian, standard, or no 'calendar' attribute
  days     = (/ (/31,28,31,30,31,30,31,31,30,31,30,31/) \    ; [2] x [12]
              , (/31,29,31,30,31,30,31,31,30,31,30,31/) /)

  if (isatt(yyyyddd,"calendar")) then                        ; retrofit to existing code
      yyyy@calendar = yyyyddd@calendar                       ; needed for day_of_year

      if (yyyyddd@calendar.eq."365_day" .or. yyyyddd@calendar.eq."365" .or. \
          yyyyddd@calendar.eq."noleap"  .or. yyyyddd@calendar.eq."no_leap") then
          days(1,:) = days(0,:)         ; make all non-leap year 
      end if
      if (yyyyddd@calendar.eq."366_day" .or. yyyyddd@calendar.eq."366" .or. \
          yyyyddd@calendar.eq."allleap" .or. yyyyddd@calendar.eq."all_leap") then
          days(0,:) = days(1,:)         ; make all leap yeary
      end if
      if (yyyyddd@calendar.eq."360_day" .or. yyyyddd@calendar.eq."360") then
          days      = 30                ; make all 30
      end if
  end if

  yyyymmdd = new (nTime, integer)

  do nt=0,nTime-1
     iyr   = 0
     if (isleapyear(yyyy(nt))) then   ; only has effect for standard, gregorian
         iyr   = 1
     end if

     ndyr = 0            ; day of year
     skip = False        ; use to bounce out of loops

     do nmo=1,12
       do ndymo=1,days(iyr,(nmo-1))
          ndyr = ndyr+1      ; increment day of year
          if (ndyr.eq.ddd(nt)) then
              yyyymmdd(nt) = yyyy(nt)*10000 + nmo*100 + ndymo
              skip = True
              break      ; bounce out of ndy loop
          end if
       end do
        foo = 1
        if (skip) then
            break        ; bounce out of nmo loop
        end if
     end do

   end do

   if (.not.any(ismissing(yyyymmdd))) then
       delete(yyyymmdd@_FillValue)
   end if

   yyyymmdd!0 = "time"
   yyyymmdd@long_name = "current date"
   yyyymmdd@units     = "YYYYMMDD" 
   yyyymmdd@info      = "converted from YYYYDDD" 

   if (isatt(yyyyddd,"calendar")) then
       yyyymmdd@calendar = yyyyddd@calendar
   end if

   return(yyyymmdd) 
end
;==============================
undef("yyyyddd_to_yyyyfrac")
function yyyyddd_to_yyyyfrac( yyyyddd[*]:numeric, ddOffset[1]:numeric)
begin
   yyyy = yyyyddd/1000
   ddd  = yyyyddd - yyyy*1000 

   if (isatt(yyyyddd,"calendar")) then
       if (yyyyddd@calendar.eq."360_day" .or. yyyyddd@calendar.eq."360") then
           yyyyfrac = yyyy + ((ddd-1)/360d) 
       end if
       if (yyyyddd@calendar.eq."365_day" .or. yyyyddd@calendar.eq."365"      .or. \
           yyyyddd@calendar.eq."noleap"  .or. yyyyddd@calendar.eq."no_leap") then
           yyyyfrac = yyyy + ((ddd-1)/365d) 
       end if
       if (yyyyddd@calendar.eq."366_day" .or. yyyyddd@calendar.eq."366"      .or. \
           yyyyddd@calendar.eq."allleap" .or. yyyyddd@calendar.eq."all_leap") then
           yyyyfrac = yyyy + ((ddd-1)/366d) 
       end if
       if (yyyyddd@calendar.eq."standard" .or. yyyyddd@calendar.eq."gregorian" ) then
           yyyyfrac = yyyy + where(isleapyear(yyyy), ((ddd-1)/366d), ((ddd-1)/365d))
       end if
       yyyyfrac@calendar = yyyyddd@calendar
   else
       yyyyfrac = yyyy + where(isleapyear(yyyy), ((ddd-1)/366d), ((ddd-1)/365d))
       yyyyfrac@calendar = "standard"
   end if

   copy_VarCoords(yyyyddd, yyyyfrac)

   yyyyfrac@long_name = "current date"
   yyyyfrac@units     = "YYYY.fraction_of_current_year" 
   yyyyfrac@info      = "converted from YYYYDDD" 

   return(yyyyfrac)
end

; **********************************************************************
; D. Shea
; wrapper for NCL procedure "linint1"  that copies attributes and coordinate 
; vars.  It adds the longitude and latitude coordinates.

undef("linint1_Wrap")
function linint1_Wrap (xi:numeric, fi:numeric, wrapX:logical \
                      ,xo[*]:numeric, Opt)

; wrapper for NCL function "linint1"  that copies attributes and coordinate vars.
local fo, dimfi, nDim, n, nD
begin

  fo   = linint1 (xi,fi, wrapX, xo, Opt)        ; perform interpolation 
                                                ; shea_misc functions
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fo))                 ; number of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  copy_VarCoords_1 (fi, fo)                     ; copy coord variables  
                                                ; except for rightmost

  nD   = nDim-1                                 ; last dimension
                                                ; create a new coord for
  if (isdimnamed(xo,0)) then
      fo!nD = xo!0                              ; if present, use xo name
  else 
      if (isdimnamed(fi,nD)) then
          fo!nD = str_switch(fi!nD)             ; if present, use same name
      else                                      ; but change case [contributed]
          fo!nD = "X"                           ; default dimension name
      end if                
  end if
                                                ; assign coordinates
  fo&$fo!nD$ = xo                               ; rightmost dim
   
  return (fo)
end

; **********************************************************************
; M. Haley
; wrapper for NCL procedure "linint1_n" that copies attributes and coordinate 
; vars.  It adds the longitude and latitude coordinates.

undef("linint1_n_Wrap")
function linint1_n_Wrap (xi:numeric, fi:numeric, wrapX:logical \
                        ,xo[*]:numeric, Opt, dim[1]:integer)

; wrapper for NCL function "linint1_n" that copies attributes and coordinate vars.
local fo, dimfi, nDim, n, nD
begin

  fo   = linint1_n (xi,fi, wrapX, xo, Opt, dim) ; perform interpolation 
                                                ; shea_misc functions
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fo))                 ; number of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  copy_VarCoords_not_n (fi, fo, dim)            ; copy coord variables  
                                                ; except for dim-th

                                                ; create a new coord for
  nD   = dim                                    ; "dim"-th dimension
  if (isdimnamed(xo,0)) then
      fo!nD = xo!0                              ; if present, use xo name
  else 
      if (isdimnamed(fi,nD)) then
          fo!nD = str_switch(fi!nD)             ; if present, use same name
      else                                      ; but change case [contributed]
          fo!nD = "X"                           ; default dimension name
      end if                
  end if
                                                ; assign coordinates
  fo&$fo!nD$ = xo                               ; "dim"-th dimension
   
  return (fo)
end
; **********************************************************************
; D. Shea
; wrapper for NCL function "linint2"  that copies attributes and coordinate 
; vars.  It adds the longitude and latitude coordinates.

undef ("linint2_Wrap")
function linint2_Wrap (xi[*]:numeric,yi[*]:numeric, fi:numeric, wrapX:logical \
                      ,xo[*]:numeric,yo[*]:numeric, Opt)

; wrapper for NCL function "linint2"  that copies attributes and coordinate vars

local fo, dimfi, nDim, nD
begin
  fo   = linint2 (xi,yi,fi, wrapX, xo,yo, Opt)  ; perform interpolation 
                                                ; shea_misc functions
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fi))                 ; number of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  if (nDim.gt.2) then
      copy_VarCoords_2 (fi, fo)                 ; copy coord variables  
  end if

  fo!(nDim-2) = "Y"                             ; default named dimensions
  fo!(nDim-1) = "X"
                                                ; override if possible
  if (isdimnamed(xo,0) .and. isdimnamed(yo,0) ) then
      fo!(nDim-2) = yo!0                        ; if present, use xo name
      fo!(nDim-1) = xo!0                        ; if present, use xo name
  else 
      do nD=nDim-2,nDim-1                       ; two rightmost dimensions
         if (.not.ismissing(fi!nD)) then
             fo!nD = str_switch(fi!nD)          ; if present, use same name
         end if                                 ; but change case
      end do
  end if

  fo&$fo!(nDim-2)$ = yo                         ; create coordinate var 
  fo&$fo!(nDim-1)$ = xo                         ; two rightmost dimensions

  return (fo)
end


; **********************************************************************
; D. Shea
; wrapper for NCL function "linint2_points"  that copies attributes+coordinates
; vars.  It creates a "pts" coord variable and creates two 1D
; attributes that indicate the lat/lon associated with each point.
undef ("linint2_points_Wrap")
function linint2_points_Wrap \ 
                      (xi[*]:numeric,yi[*]:numeric, fi:numeric, wrapX:logical \
                      ,xo[*]:numeric,yo[*]:numeric, Opt)

; wrapper for NCL function "linint2_points"  that copies attributes and coordinate vars
local fo, dimfi, nDim, pts
begin
  fo   = linint2_points (xi,yi,fi, wrapX, xo,yo, Opt)  ; perform interpolation 
                                                ; shea_misc functions
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fi))                 ; number of dimensions

  copy_VarAtts     (fi, fo)                     ; copy variable attributes
  if (nDim.gt.2) then
      copy_VarCoords_2 (fi, fo)                 ; copy coord variables  
  end if
                                                ; except for 2 rightmost

  nDim = dimsizes(dimsizes(fo))                 ; # output  dimensions 
                                                
  pts           = ispan(0,dimsizes(xo)-1,1)     ; linear   
  pts@long_name = "Points"

  fo!(nDim-1)   = "pts"                         ; default named dimensions
  fo&pts        =  pts
  fo@xcoord     =  xo            ; x/longitude points
  fo@ycoord     =  yo            ; y/latitude  points
   
  return (fo)
end

; **********************************************************************
; D. Shea
; wrapper for NCL function "rcm2rgrid"  that copies attributes and coordinate
; vars.  It adds the longitude and latitude coordinates.

undef ("rcm2rgrid_Wrap")
function rcm2rgrid_Wrap (yi[*][*]:numeric,xi[*][*]:numeric, fi:numeric \
                        ,yo[*]:numeric,xo[*]:numeric, Opt)

; wrapper for NCL function "rcm2rgrid"  that copies attributes and coordinate vars

local fo, dimfi, nDim, nD
begin
  fo   = rcm2rgrid (yi,xi,fi, yo,xo, Opt)  ; perform interpolation

  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fi))            ; number of dimensions

  copy_VarAtts (fi, fo)                    ; copy variable attributes
  if (isatt(fo,"lat2d")) then
      delete(fo@lat2d)
  end if
  if (isatt(fo,"lon2d")) then
      delete(fo@lon2d)
  end if

  if (nDim.gt.2) then
      copy_VarCoords_2 (fi, fo)            ; copy coord variables
  end if

  fo!(nDim-2) = "Y"                        ; default named dimensions
  fo!(nDim-1) = "X"
                                           ; override if possible
  if (isdimnamed(xo,0) .and. isdimnamed(yo,0) ) then
      fo!(nDim-1) = xo!0                   ; if present, use xo name
      fo!(nDim-2) = yo!0                   ; if present, use xo name
  else
      do nD=nDim-2,nDim-1                  ; two rightmost dimensions
         if (.not.ismissing(fi!nD)) then
             fo!nD = str_switch(fi!nD)     ; if present, use same name
         end if                            ; but change case
      end do
  end if

  fo&$fo!(nDim-1)$ = xo                    ; create coordinate var
  fo&$fo!(nDim-2)$ = yo                    ; two rightmost dimensions

  fo@ncl = "rcm2rgrid used for interpolation"

  return (fo)
end
;-------------------------
undef ("rcm2points_Wrap")
function rcm2points_Wrap \ 
                      (yi[*][*]:numeric,xi[*][*]:numeric, fi:numeric \
                      ,yo[*]:numeric,xo[*]:numeric, Opt)

local fo, dimfi, nDim, pts
begin
  fo   = rcm2points (yi,xi,fi, yo,xo, Opt)  ; perform interpolation 
                                               
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fi))                 ; number of dimensions

  copy_VarAtts     (fi, fo)                     ; copy variable attributes
  if (nDim.gt.2) then
      copy_VarCoords_2 (fi, fo)                 ; copy coord variables  
  end if                                        ; except for 2 rightmost

  nDim = dimsizes(dimsizes(fo))                 ; # output  dimensions 
                                                
  pts           = ispan(0,dimsizes(xo)-1,1)     ; linear   
  pts@long_name = "Points"

  fo!(nDim-1)   = "pts"                         ; default named dimensions
  fo&pts        =  pts
  fo@xcoord     =  xo            ; x/longitude points
  fo@ycoord     =  yo            ; y/latitude  points
  fo@ncl = "rcm2points used for interpolation"

  return (fo)
end

; **********************************************************************
; D. Shea
; wrapper for NCL function "rgrid2rcm"  that copies attributes and coordinate 
; vars.  

undef ("rgrid2rcm_Wrap")
function rgrid2rcm_Wrap (yi[*]:numeric,xi[*]:numeric, fi:numeric \
                        ,yo[*][*]:numeric,xo[*][*]:numeric, Opt)

; wrapper for NCL function "rgrid2rcm"  that copies attributes and coordinate vars

local fo, dimfi, nDim, nD
begin
  fo   = rgrid2rcm (yi,xi,fi, yo,xo, Opt)  ; perform interpolation 
                       
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fi))                 ; number of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  if (nDim.gt.2) then
      copy_VarCoords_2 (fi, fo)                 ; copy coord variables  
  end if                                        ; except for 2 rightmost

  fo!(nDim-2) = "Y"                             ; default named dimensions
  fo!(nDim-1) = "X"   
                                                ; override if possible
  if (isdimnamed(xo,0) ) then
      fo!(nDim-2) = xo!0                        ; if present, use xo name
  end if
  if (isdimnamed(xo,1) ) then
      fo!(nDim-1) = xo!1                        ; if present, use xo name
  end if
  fo@ncl = "rgrid2rcm used for interpolation"
 ;fo@coordinates = fo!(nDim-2) +" "+fo!(nDim-1)

  return (fo)
end

;****************************************************
; D. Shea
; Take a monthly climatology and make a daily climatology
; Current for gregorian / standard year.
;
; Supported: leftmost dimension must be 12
;            x(12), x(12,N), x(12,N1,N2), x(12,N1,N2,N3)
; x must have named dimensions on entry
; opt - not used set to zero [0]
;
undef("clmMon2clmDay")
function clmMon2clmDay( x:numeric, retOrder:integer, opt:integer )
local dNames, dimx, rank, X, midMon, day
begin
  if (.not.(retOrder.eq.0 .or. retOrder.eq.1)) then
      print("clmMon2clmDay: retOrder must be 0 or 1, retOrder=" +retOrder)
      exit
  end if

  dNames = getvardims( x )
  if (any(ismissing(dNames))) then
      print("clmMon2clmDay: named dimensions required:" +dNames)
      exit
  end if

  dimx   = dimsizes(x)
  if (dimx(0).ne.12) then
      print("clmMon2clmDay: leftmost dimension must be size=12: SIZE="+dimx(0))
      exit
  end if

  rank   = dimsizes( dimx )
  if (rank.gt.4) then
      print("clmMon2clmDay: currently max of 4 dimension supported: rank="+rank)
      exit
  end if

 ;if (isatt(x,"_FillValue")) then
 ;    nFill = num(ismissing(x))
 ;    if (nFill.gt.0) then
 ;        print("clmMon2clmDay: input is assumed to have no missing values, nFill="+nFill)
 ;        exit
 ;    end if
 ;end if
                          ; transfer to work arrsy,if necessary, reorder array
  if (rank.eq.2) then
      X =  x
  end if
  if (rank.eq.2) then
      X =  x($dNames(1)$|:, $dNames(0)$|:)  
  end if
  if (rank.eq.3)
      X =  x($dNames(1)$|:, $dNames(2)$|:, $dNames(0)$|:) 
  end if
  if (rank.eq.4)
      X =  x($dNames(1)$|:, $dNames(2)$|:, $dNames(3)$|:, $dNames(0)$|:) 
  end if
                          ; mid day of each month
  if (isatt(opt,"midmon")) then
      if (dimsizes(opt@midMon).eq.12) then
          midMon = opt@midMon
      else
          print("clmMon2clmDay: midMon required to be size 12: size="+dimsizes(opt@midMon))
          exit
      end if
  else
      midMon = (/ 15.5, 45  , 74.5,105  ,135.5,166  \
                ,196.5,227.5,258  ,288.5,319  ,349.5/)
  end if
  midMon@long_name = "middle of month"

  day    = ispan(0,364,1)    ; use 0 => 364 for interpolation
  day!0  = "day"

  Z      = linint1_Wrap (midMon, X, True, day, 0)
  Z@info = "NCL: clmMon2clmDay"

  day    = ispan(1,365,1)    ; use 1 => 365 for coord variable
  day@long_name = "day of year: no leap"
  day@units     = "1=Jan 1, 32=Feb 1, ..., 365-Dec 31"
  Z!(rank-1) = "day"
  Z&day      =  day

  if (retOrder.eq.1) then
      return( Z )
  end if

  if (retOrder.eq.0) then
      if (rank.eq.1) then
          return(Z)
      end if
      if (rank.eq.2) then
          return( Z(day|:, $dNames(1)$|:) )
      end if
      if (rank.eq.3) then
          return( Z(day|:, $dNames(1)$|:, $dNames(2)$|:) )
      end if
      if (rank.eq.4) then
          return( Z(day|:, $dNames(1)$|:, $dNames(2)$|:, $dNames(3)$|:) )
      end if
  end if
end


;********************************************************************
; D. Shea
; wrapper for NCL function "cssgrid"  that copies attributes 
; It adds the longitude and latitude coordinates.

undef("cssgrid_Wrap")
function cssgrid_Wrap (lati[*]:numeric,loni[*]:numeric, fi:numeric  \
                      ,lato[*]:numeric,lono[*]:numeric)
local fo
begin
  fo   = cssgrid (lati,loni, fi, lato, lono)          ; perform interpolation 
  fo!0 = "lat"
  fo!1 = "lon"

  fo&lat = lato
  fo&lon = lono

  copy_VarAtts (fi, fo)     ; copy variable attributes
  fo@NCL_function = "cssgrid_Wrap" 

  if (isatt(fo,"time")) then
      delete (fo@time)   ; special case since this does not work
  end if                 ; on arrays

  return (fo)
end

;********************************************************************
; D. Shea
; wrapper for NCL function "g2gsh"  that copies attributes and coordinate vars.
; It adds the longitude and gaussian latitude coordinates.

undef("g2gsh_Wrap")
function g2gsh_Wrap (x:numeric, newDims, twave:numeric)
local nlat, mlon, xNew, lat, lon, nDim, gwt 
begin
  nlat = newDims(0)                             ; specify output grid
  mlon = newDims(1)
  if (typeof(x).eq."double") then
      nlat@double = True
      mlon@double = True
  end if

  xNew = g2gsh(x, (/nlat,mlon/), twave)         ; interpolate to new grid
                                                ; contributed functions
  copy_VarAtts (x, xNew)                        ; copy variable attributes
  copy_VarCoords_2 (x, xNew)   ; copy coord variables except lat and lon

  lat  = latGau    (nlat, "lat", "latitude" , "degrees_north")
  gwt  = latGauWgt (nlat, "lat", "gaussian weights", "")
  lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (x, lon)                       ; [lon(0)=-180] init location    

  nDim     = dimsizes(dimsizes(xNew))           ; number of dimensions
  xNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  xNew!(nDim-1)  = "lon"                        ; rightmost dimension
      
  xNew&lat = lat                                ; add new coord var
  xNew&lon = lon

  if (isatt(xNew,"gwt")) then
      delete(xNew@gwt)
  end if
  xNew@gwt = gwt                                ; attach as attribute

  return (xNew)
end

;******************************************************************
; D. Shea
; wrapper for NCL function "g2fsh"  that copies attributes and coordinate vars.
; It adds the longitude and gaussian latitude coordinates.

undef("g2fsh_Wrap")
function g2fsh_Wrap (x:numeric, newDims)
local nlat, mlon, xNew, lat, lon, nDim 
begin
  nlat = newDims(0)                             ; specify output grid
  mlon = newDims(1)
  if (typeof(x).eq."double") then
      nlat@double = True
      mlon@double = True
  end if

  xNew = g2fsh(x, (/nlat,mlon/) )               ; interpolate to new grid
                                                ; contributed functions
  copy_VarAtts (x, xNew)                        ; copy variable attributes
  copy_VarCoords_2 (x, xNew) ; copy coord variables except lat and lon

  lat  = latGlobeF (nlat, "lat", "latitude" , "degrees_north")
  lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (x, lon)                       ; [lon(0)=-180] init location    
   
  nDim     = dimsizes(dimsizes(xNew))           ; number of dimensions
  xNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  xNew!(nDim-1)  = "lon"                        ; rightmost dimension
      
  xNew&lat = lat                                ; add new coord var
  xNew&lon = lon

  return (xNew)
end


; **************************************************************
; D. Shea
; wrapper for NCL function "f2gsh" that copies attributes and coordinate vars.
; It adds the longitude and gaussian latitude coordinates.

undef("f2gsh_Wrap")
function f2gsh_Wrap (x:numeric, newDims, twave:numeric)
local nlat, mlon, xNew, lat, lon, nDim 
begin
  nlat = newDims(0)                             ; specify output grid
  mlon = newDims(1)
  if (typeof(x).eq."double") then
      nlat@double = True
      mlon@double = True
  end if

  xNew = f2gsh(x, newDims, twave)               ; interpolate to new grid
                                                ; contributed functions
  copy_VarAtts (x, xNew)                        ; copy variable attributes
  copy_VarCoords_2 (x, xNew)  ; copy coord variables  except lat and lon

  lat  = latGau    (nlat, "lat", "latitude" , "degrees_north")
  gwt  = latGauWgt (nlat, "lat", "gaussian weights", "")
  lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (x, lon)                       ; [lon(0)=-180] init location    
   
  nDim     = dimsizes(dimsizes(xNew))           ; number of dimensions
  xNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  xNew!(nDim-1)  = "lon"                        ; rightmost dimension
      
  xNew&lat = lat                                ; add new coord var
  xNew&lon = lon

  if (isatt(xNew,"gwt")) then
      delete(xNew@gwt)
  end if
  xNew@gwt = gwt                                ; attach gaussian weights

  return (xNew)
end



;********************************************************
; D. Shea
; wrapper for NCL function "f2fsh"  that copies attributes and coordinate vars.
; It adds the longitude and latitude coordinates.

undef("f2fsh_Wrap")
function f2fsh_Wrap (x:numeric, newDims)
local nlat, mlon, xNew, lat, lon, nDim 
begin
  nlat = newDims(0)                             ; specify output grid
  mlon = newDims(1)
  if (typeof(x).eq."double") then
      nlat@double = True
      mlon@double = True
  end if

  xNew = f2fsh(x, newDims )                     ; interpolate to new grid
                                                ; contributed functions
  copy_VarAtts (x, xNew)                        ; copy variable attributes
  copy_VarCoords_2 (x, xNew) ; copy coord variables except lat and lon

  lat  = latGlobeF (nlat, "lat", "latitude" , "degrees_north")
  lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (x, lon)                       ; [lon(0)=-180] init location    

  nDim     = dimsizes(dimsizes(xNew))           ; number of dimensions
  xNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  xNew!(nDim-1)  = "lon"                        ; rightmost dimension
      
  xNew&lat = lat                                ; add new coord var
  xNew&lon = lon

  return (xNew)
end


;****************************************************************
; D. Shea
; wrapper for NCL function "f2fosh"  that copies attributes and coordinate 
; vars. It adds the longitude and latitude coordinates.

undef("f2fosh_Wrap")
function f2fosh_Wrap (x:numeric)
local nlat, mlon, xNew, lat, lon, dimx, nDim, nlat1 
begin
  xNew = f2fosh(x)                              ; interpolate to new grid
                                                ; contributed functions
  copy_VarAtts (x, xNew)                        ; copy variable attributes
  copy_VarCoords_2 (x, xNew)                    ; copy coord variables  
                                                ; except lat and lon
  dimx = dimsizes (x)
  nDim = dimsizes (dimx)                        ; rank of matrix
  nlat = dimx(nDim-2)                           ; dim of INPUT grid
  mlon = dimx(nDim-1)    
  nlat1= nlat-1                                 ; fo has one less lat

  if (typeof(x).eq."double") then
      nlat@double = True
      nlat1@double= True
      mlon@double = True
  end if

  lat = latGlobeFo (nlat1, "lat", "latitude", "degrees_north")
  lon = lonGlobeFo (mlon , "lon", "longitude","degrees_east")
                                                ; possibly make near Date Line 
  lonGM2DateLine (x, lon)                       ; init location    
   
  nDim     = dimsizes(dimsizes(xNew))           ; number of dimensions
  xNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  xNew!(nDim-1)  = "lon"                        ; rightmost dimension
      
  xNew&lat = lat                                ; add new coord var
  xNew&lon = lon

  return (xNew)
end

;****************************************************************
; D. Shea
; wrapper for NCL function "fo2fsh"  that copies attributes and coordinate 
; vars. It adds the longitude and latitude coordinates.

undef("fo2fsh_Wrap")
function fo2fsh_Wrap (x:numeric)
local nlat, mlon, xNew, lat, lon, dimx, nDim, nlat1 
begin
  xNew = fo2fsh(x)                              ; interpolate to new grid
                                                ; contributed functions
  copy_VarAtts (x, xNew)                        ; copy variable attributes
  copy_VarCoords_2 (x, xNew)                    ; copy coord variables  
                                                ; except lat and lon
  dimx = dimsizes (x)
  nDim = dimsizes (dimx)                        ; rank of matrix
  nlat = dimx(nDim-2)                           ; dim of INPUT grid
  mlon = dimx(nDim-1)    
  nlat1= nlat+1                                 ; f has one additional lat

  if (typeof(x).eq."double") then
      nlat@double = True
      nlat1@double= True
      mlon@double = True
  end if

  lat = latGlobeF (nlat1, "lat", "latitude", "degrees_north")
  lon = lonGlobeF (mlon , "lon", "longitude","degrees_east")
                                                ; possibly make near Date Line 
  lonGM2DateLine (x, lon)                       ; init location    
   
  nDim     = dimsizes(dimsizes(xNew))           ; number of dimensions
  xNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  xNew!(nDim-1)  = "lon"                        ; rightmost dimension
      
  xNew&lat = lat                                ; add new coord var
  xNew&lon = lon

  return (xNew)
end
      

; **********************************************************
; D. Shea
; wrapper for NCL procedure "g2gshv"  that copies attributes and coordinate 
; vars. It adds the longitude and gaussian latitude coordinates.

undef("g2gshv_Wrap")
procedure g2gshv_Wrap (u:numeric, v:numeric, uNew:numeric, vNew:numeric,\
                       twave:numeric)
local dim_uNew, nDim, nlat, mlon, lat, lon, gwt 
begin
  g2gshv (u, v, uNew, vNew,twave)
                                                ; contributed functions
  copy_VarAtts (u, uNew)                        ; copy variable attributes
  copy_VarCoords_2 (u, uNew)                    ; copy coord variables  
                                                ; except lat and lon
  copy_VarAtts (v, vNew)                        ; copy variable attributes
  copy_VarCoords_2 (v, vNew)                    ; copy coord variables  

                                                ; except lat and lon
  dim_uNew= dimsizes(uNew)                      ; dim sizes of each dimension
  nDim    = dimsizes(dim_uNew)                  ; number of dimensions [rank]
  nlat    = dim_uNew(nDim-2)                    ; number of latitudes
  mlon    = dim_uNew(nDim-1)                    ; number of longitudes
  if (typeof(u).eq."double") then
      nlat@double = True
      mlon@double = True
  end if

  lat  = latGau    (nlat, "lat", "latitude" , "degrees_north")
  gwt  = latGauWgt (nlat, "lat", "gaussian weights", "")
  lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (u, lon)                       ; [lon(0)=-180] init location    

  uNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  uNew!(nDim-1)  = "lon"                        ; rightmost dimension
  uNew&lat       =  lat                         ; add new coord var
  uNew&lon       =  lon

  vNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  vNew!(nDim-1)  = "lon"                        ; rightmost dimension
  vNew&lat       =  lat                         ; add new coord var
  vNew&lon       =  lon

  if (isatt(uNew,"gwt")) then
      delete(uNew@gwt)                 
  end if
  uNew@gwt       = gwt                          ; attach gaussian weights

  if (isatt(vNew,"gwt")) then
      delete(vNew@gwt)
  end if
  vNew@gwt       = gwt                          ; attach gaussian weights
end

; ********************************************************************
; D. Shea
; wrapper for NCL procedure "g2fshv"  that copies attributes and coordinate 
; vars. It adds the longitude and gaussian latitude coordinates.

undef("g2fshv_Wrap")
procedure g2fshv_Wrap (u:numeric, v:numeric, uNew:numeric, vNew:numeric)
local dim_uNew, nDim, nlat, mlon, lon, lat
begin
  g2fshv (u, v, uNew, vNew)       
                                                ; contributed functions
  copy_VarAtts (u, uNew)                        ; copy variable attributes
  copy_VarCoords_2 (u, uNew)                    ; copy coord variables  
                                                ; except lat and lon
  copy_VarAtts (v, vNew)                        ; copy variable attributes
  copy_VarCoords_2 (v, vNew)                    ; copy coord variables  
                                                ; except lat and lon
  dim_uNew= dimsizes(uNew)                      ; dim sizes of each dimension
  nDim    = dimsizes(dim_uNew)                  ; number of dimensions [rank]
  nlat    = dim_uNew(nDim-2)                    ; number of latitudes
  mlon    = dim_uNew(nDim-1)                    ; number of longitudes

  if (typeof(u).eq."double") then
      nlat@double = True
      mlon@double = True
  end if

  lat  = latGlobeF (nlat, "lat", "latitude" , "degrees_north")
  lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (u, lon)                       ; [lon(0)=-180] init location    

  uNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  uNew!(nDim-1)  = "lon"                        ; rightmost dimension
  uNew&lat       = lat                          ; add new coord var
  uNew&lon       = lon

  vNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  vNew!(nDim-1)  = "lon"                        ; rightmost dimension
  vNew&lat       = lat                          ; add new coord var
  vNew&lon       = lon
end


; ***********************************************************************
; D. Shea
; wrapper for NCL procedure "f2gshv"  that copies attributes and coordinate 
; vars. It adds the longitude and gaussian latitude coordinates.

undef("f2gshv_Wrap")
procedure f2gshv_Wrap (u:numeric, v:numeric, uNew:numeric, vNew:numeric,\ 
                       twave:numeric)
local dim_uNew, nDim, nlat, mlon, lon, lat, gwt
begin
  f2gshv (u, v, uNew, vNew, twave)
                                                ; contributed functions
  copy_VarAtts (u, uNew)                        ; copy variable attributes
  copy_VarCoords_2 (u, uNew)                    ; copy coord variables  
                                                ; except lat and lon
  copy_VarAtts (v, vNew)                        ; copy variable attributes
  copy_VarCoords_2 (v, vNew)                    ; copy coord variables  
                                                ; except lat and lon
  dim_uNew= dimsizes(uNew)                      ; dim sizes of each dimension
  nDim    = dimsizes(dim_uNew)                  ; number of dimensions [rank]
  nlat    = dim_uNew(nDim-2)                    ; number of latitudes
  mlon    = dim_uNew(nDim-1)                    ; number of longitudes

  if (typeof(u).eq."double") then
      nlat@double = True
      mlon@double = True
  end if

  lat  = latGau    (nlat, "lat", "latitude" , "degrees_north")
  gwt  = latGauWgt (nlat, "lat", "gaussian weights", "")
  lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (u, lon)                       ; [lon(0)=-180] init location    

  uNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  uNew!(nDim-1)  = "lon"                        ; rightmost dimension
  uNew&lat       = lat                          ; add new coord var
  uNew&lon       = lon

  vNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  vNew!(nDim-1)  = "lon"                        ; rightmost dimension
  vNew&lat       = lat                          ; add new coord var
  vNew&lon       = lon

  if (isatt(uNew,"gwt")) then
      delete(uNew@gwt)
  end if
  uNew@gwt       = gwt                          ; attach gaussian weights

  if (isatt(vNew,"gwt")) then
      delete(vNew@gwt)
  end if
  vNew@gwt       = gwt                          ; attach gaussian weights
end

; *************************************************************************
; D. Shea
; wrapper for NCL procedure "f2fshv"  that copies attributes and coordinate 
; vars. It adds the longitude and gaussian latitude coordinates.

undef("f2fshv_Wrap")
procedure f2fshv_Wrap (u:numeric, v:numeric, uNew:numeric, vNew:numeric)
local dim_uNew, nDim, nlat, mlon, lon, lat
begin
  f2fshv (u, v, uNew, vNew)
                                                ; contributed functions
  copy_VarAtts (u, uNew)                        ; copy variable attributes
  copy_VarCoords_2 (u, uNew)                    ; copy coord variables  
                                                ; except lat and lon
  copy_VarAtts (v, vNew)                        ; copy variable attributes
  copy_VarCoords_2 (v, vNew)                    ; copy coord variables  
                                                ; except lat and lon
  dim_uNew= dimsizes(uNew)                      ; dim sizes of each dimension
  nDim    = dimsizes(dim_uNew)                  ; number of dimensions [rank]
  nlat    = dim_uNew(nDim-2)                    ; number of latitudes
  mlon    = dim_uNew(nDim-1)                    ; number of longitudes

  if (typeof(u).eq."double") then
      nlat@double = True
      mlon@double = True
  end if

  lat  = latGlobeF (nlat, "lat", "latitude" , "degrees_north")
  lon  = lonGlobeF (mlon, "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (u, lon)                       ; [lon(0)=-180] init location    

  uNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  uNew!(nDim-1)  = "lon"                        ; rightmost dimension
  uNew&lat       = lat                          ; add new coord var
  uNew&lon       = lon

  vNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  vNew!(nDim-1)  = "lon"                        ; rightmost dimension
  vNew&lat       = lat                          ; add new coord var
  vNew&lon       = lon
end



; ********************************************************************
; D. Shea
; wrapper for NCL procedure "f2fosh"  that copies attributes and coordinate 
; vars. It adds the longitude and latitude coordinates.

undef("f2foshv_Wrap")
procedure f2foshv_Wrap (u:numeric, v:numeric, uNew:numeric, vNew:numeric)
local dim_uNew, nDim, nlat, mlon, lon, lat
begin
  f2foshv(u, v, uNew, vNew)                    ; interpolate to new grid
                                               ; contributed functions
  copy_VarAtts (u, uNew)                       ; copy variable attributes
  copy_VarCoords_2 (u, uNew)                   ; copy coord variables  
                                               ; except lat and lon
  copy_VarAtts (v, vNew)                       ; copy variable attributes
  copy_VarCoords_2 (v, vNew)                   ; copy coord variables  
                                               ; except lat and lon
  dim_uNew= dimsizes(uNew)                     ; dim sizes of each dimension
  nDim    = dimsizes(dim_uNew)                 ; number of dimensions [rank]
  nlat    = dim_uNew(nDim-2)                   ; number of latitudes  [fo grid]
  mlon    = dim_uNew(nDim-1)                   ; number of longitudes [fo grid]
  nlat1= nlat

  if (typeof(u).eq."double") then
      nlat@double = True
      nlat1@double= True
      mlon@double = True
  end if

  lat = latGlobeFo (nlat1, "lat", "latitude", "degrees_north")
  lon = lonGlobeFo (mlon , "lon", "longitude","degrees_east")
                                                ; possibly make near Date Line 
  lonGM2DateLine (u, lon)                       ; init location    

  uNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  uNew!(nDim-1)  = "lon"                        ; rightmost dimension
  uNew&lat       = lat                          ; add new coord var
  uNew&lon       = lon

  vNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  vNew!(nDim-1)  = "lon"                        ; rightmost dimension
  vNew&lat       = lat                          ; add new coord var
  vNew&lon       = lon
end
; **********************************************************************
; D. Shea
; wrapper for NCL procedure "fo2fsh"  that copies attributes and coordinate 
; vars.  It adds the longitude and latitude coordinates.

undef("fo2fshv_Wrap")
procedure fo2fshv_Wrap (u:numeric, v:numeric, uNew:numeric, vNew:numeric)
local dim_uNew, nDim, nlat, mlon, lon, lat
begin
  fo2fshv(u, v, uNew, vNew)                    ; interpolate to new grid
                                               ; contributed functions
  copy_VarAtts (u, uNew)                       ; copy variable attributes
  copy_VarCoords_2 (u, uNew)                   ; copy coord variables  
                                               ; except lat and lon
  copy_VarAtts (v, vNew)                       ; copy variable attributes
  copy_VarCoords_2 (v, vNew)                   ; copy coord variables  
                                               ; except lat and lon
  dim_uNew= dimsizes(uNew)                     ; dim sizes of each dimension
  nDim    = dimsizes(dim_uNew)                 ; number of dimensions [rank]
  nlat    = dim_uNew(nDim-2)                   ; number of latitudes  [fo grid]
  mlon    = dim_uNew(nDim-1)                   ; number of longitudes [fo grid]
  nlat1   = nlat

  if (typeof(u).eq."double") then
      nlat@double = True
      nlat1@double= True
      mlon@double = True
  end if

  lat  = latGlobeF (nlat1, "lat", "latitude" , "degrees_north")
  lon  = lonGlobeF (mlon , "lon", "longitude", "degrees_east")
                                                ; possibly make Date Line 
  lonGM2DateLine (u, lon)                       ; [lon(0)=-180] init location    

  uNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  uNew!(nDim-1)  = "lon"                        ; rightmost dimension
  uNew&lat       = lat                          ; add new coord var
  uNew&lon       = lon

  vNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  vNew!(nDim-1)  = "lon"                        ; rightmost dimension
  vNew&lat       = lat                          ; add new coord var
  vNew&lon       = lon
end
 
;*********************************************************************
; Mark Stevens
; Read RGB file format of n rows by 3 columns (R,G,B)
; values are integers from 0 to 255
; first triplet is the background
; second triplet is the foreground
; normalize RGB values (=RGB/255) for cmap format

undef("RGBtoCmap")
function RGBtoCmap (fName:string)
local rgb, size, n, norm, cmap
begin
   rgb  = asciiread (fName, -1, "integer") 
   size = dimsizes(rgb)
   n    = size/3                    ; number of rows 
   norm = rgb/255.0                 ; divide all elements     
   cmap = onedtond (norm, (/n,3/))  ; back to triplets
   return (cmap)
end
;************************************************************
; Mark Stevens
; will choose a color to fill in a poly(line/gon/marker) based upon
; secondary scalar field.
;
; This function was updated 6/10/2013 to allow n x 3 or
; n x 4 (RGBA) arrays.
;
undef("GetFillColor")
function GetFillColor(cnlvls[*]:numeric,cmapt,data:numeric)
local dims, ncn, nclr, color, n
begin

 if(isstring(cmapt)) then
    cmap = read_colormap_file(cmapt)
 else if(isnumeric(cmapt)) then
   dims = dimsizes(cmapt)
   if(dims(0).lt.3.or.dims(0).gt.256.or..not.any(dims(1).ne.(/3,4/))) then
     print ("Error: GetFillColors: cmap must be an n x 3 or n x 4 array of RGB or RGBA values, or a valid color map name")
     return(new(3,"float"))    ; return a missing value
   end if
   cmap = cmapt
 else
   print ("Error: GetFillColors: cmap must be an n x 3 or n x 4 array of RGB or RGBA values, or a valid color map name")
 end if
 end if

 ncn = dimsizes (cnlvls)
 nclr = dimsizes (cmap(:,0))
 if (nclr-2 .lt. ncn+1) then 
   print ("Error: GetFillColors: Not enough colors in colormap for number of contour levels")
   return (new(3,float))   ; return missing
 end if

 if (data .le. cnlvls(0)) then
   color = cmap(2,:)
 else 
   if (data .gt. cnlvls(ncn-1)) then
     color = cmap(nclr-1,:)
   else 
     do n = 1, ncn-1
       if (data .le. cnlvls(n)) then
         color = cmap(n+2,:)
         break
       end if
     end do
   end if
 end if
 return (color)
end
;************************************************************
; Mark Stevens
; function returns the correct colormap index for the input data value
;
undef ("GetFillColorIndex")
function GetFillColorIndex(cnlvls[*]:numeric,indices[*]:integer,data:numeric)

; cnlvls  - input contour levels
; indices - input indices to colormap
; data    - input data value
local ncn, nclr, index, n
begin

 ncn = dimsizes (cnlvls)
 nclr = dimsizes (indices)

 if (nclr .lt. ncn+1) then 
   print ("Not enough colors in colormap for number of contour levels")
   return (default_fillvalue("integer"))
 end if

 if (data .le. cnlvls(0)) then
   index = indices(0) 
 else 
   if (data .gt. cnlvls(ncn-1)) then
     index = indices(nclr-1) 
   else 
     do n = 1, ncn-1
       if (data .le. cnlvls(n)) then
         index = indices(n) 
         break
       end if
     end do
   end if
 end if
 return (index)
end
;*****************************************************************
; S. Murphy
; goes and determines the appropriate value for the missing value from
; getFillValue, and then assigns the _FillValue and the missing_value
; to this number. This is useful when creating derivations and outputting
; data to netcdf, or dealing with netcdf data that has no attributes.

undef("assignFillValue")
procedure assignFillValue(var_from:numeric, var_to:numeric)
local value
begin

  if (isatt(var_from,"_FillValue")) then
      var_to@_FillValue    = var_from@_FillValue
      var_to@missing_value = var_from@_FillValue
  end if

end
;*****************************************************************
; A Phillips
; Sets a data point array to missing if a percentage of good points
; is not met.

; Time is assumed to be on the rightmost side of y
;
; dataperc is the percentage of data which is necessary to use
; in forthcoming calculations in your program. For instance, 
; if dataperc is .75, 75% of the data for a data point must
; be present. If not, all data values for that particular data
; point will be set to missing in this fxn. 
;
; USAGE:
;	y = rmInsufData(y,.75)   ;will replace "y" with filtered array
;	Y = rmInsufData(y,.75)   ;"Y" will be filtered, "y" will be unchanged

undef("rmInsufData")
function rmInsufData (y:numeric, dataperc:float)
local x, dims, numdims, i,j,k,l, dim0,dim1,dim2,dim3,dim4
begin
 x=y
 dims=dimsizes(x)
 numdims=dimsizes(dims)

 if (numdims.eq.1) then
   dim0=dims(0)
   if (((num(.not.ismissing(x(:)))*100)/dim0).lt.dataperc*100) then
      x = x@_FillValue
   end if
 end if

 if (numdims.eq.2) then
   dim0=dims(0)
   dim1=dims(1)
   do i=0,dim0-1
     if (((num(.not.ismissing(x(i,:)))*100)/dim1).lt.dataperc*100) then
       x(i,:) = x@_FillValue
     end if
   end do	   
 end if

 if (numdims.eq.3) then
   dim0=dims(0)
   dim1=dims(1)
   dim2=dims(2)
   do i=0,dim0-1
     do j=0,dim1-1
       if (((num(.not.ismissing(x(i,j,:)))*100)/dim2).lt.dataperc*100) then
	  x(i,j,:) = x@_FillValue
       end if
     end do	   
   end do
 end if	

 if (numdims.eq.4) then
    dim0=dims(0)
    dim1=dims(1)
    dim2=dims(2)
    dim3=dims(3)
    do i=0,dim0-1
      do j=0,dim1-1
	 do k=0,dim2-1
	   if(((num(.not.ismissing(x(i,j,k,:)))*100)/dim3).lt.dataperc*100)then
	      x(i,j,k,:) = x@_FillValue
	   end if
	 end do	   
      end do
    end do
 end if	

 if (numdims.eq.5) then
   dim0=dims(0)
   dim1=dims(1)
   dim2=dims(2)
   dim3=dims(3)
   dim4=dims(4)
   do i=0,dim0-1
      do j=0,dim1-1
	 do k=0,dim2-1
	    do l=0,dim3-1
	       if(((num(.not.ismissing(x(i,j,k,l,:)))*100)/dim4).lt.\
                    dataperc*100) then
	            x(i,j,k,l,:) = x@_FillValue
	       end if
	     end do	   
	  end do
      end do
   end do
 end if	
 return(x)
end
;; -------------------------------------------------------
undef("SqrtCosWeight")
function SqrtCosWeight (y:numeric)
;;	Created by Adam Phillips
;;
;;      The name of the latitude dimension is assumed to be "lat"
;;      The rightmost dimension is assumed to be longitude
;;
;;      Acceptable dimension orders:
;;      (lat,lon), (time,lat,lon), (time,???,lat,lon), (time,???,???,lat,lon)
;;
;;      This function will perform square-root of the cosine weighting on the 
;;      given array.
;;
;;                       
;; USAGE:
;;      y = SqrtCosWeight(y)   ;will replace "y" with weighted array
;;      Y = SqrtCosWeight(y)   ;"Y" will be weighted, "y" will be unchanged

local x, qwlat, dims, numdims, nlat, pi, rad, coslat, z, sqrtcos, a,b,c,d
begin
        x = y
     	if (typeof(x&lat).eq."double") then
	   qwlat = doubletofloat(x&lat)
	else
	   qwlat = x&lat
	end if   
        
        dims=dimsizes(x)
        numdims=dimsizes(dims)
        nlat   = dims(numdims-2)  
        pi     = 4.*atan(1.0)
        rad    = (pi/180.)
        coslat = cos(qwlat*rad)
        do z = 0,nlat-1
           if (coslat(z).lt.0) then
              coslat(z) = 0.            ;sets the cos(90) = 0
           end if               
        end do  
        sqrtcos = sqrt(coslat)

        if (numdims.eq.2) then
           do a = 0,nlat-1
              x(a,:) = x(a,:)*sqrtcos(a)
           end do  
        end if
        if (numdims.eq.3) then
           do b = 0,nlat-1
              x(:,b,:) = x(:,b,:)*sqrtcos(b)
           end do  
        end if
        if (numdims.eq.4) then
           do c = 0,nlat-1
              x(:,:,c,:) = x(:,:,c,:)*sqrtcos(c)
           end do  
        end if
        if (numdims.eq.5) then
           do d = 0,nlat-1
              x(:,:,:,d,:) = x(:,:,:,d,:)*sqrtcos(d)
           end do  
        end if
	if (numdims.ge.6) then
	   print("SqrtCosWeight accepts an array with 5 dimensions or less, array has "+numdims+" dimensions, exiting")
	   exit
	end if
	if (isatt(x,"long_name")) then
           x@long_name = x@long_name + " (sqrt cosine weighted)"
        end if
return(x)
end
;; -------------------------------------------------------
undef ("NewCosWeight")
function NewCosWeight (y:numeric)
;;	
;;	created by Adam Phillips
;;
;;      The name of the latitude dimension is assumed to be "lat"
;;      The rightmost dimension is assumed to be longitude
;;
;;      Acceptable dimension orders:
;;      (lat,lon), (time,lat,lon), (time,???,lat,lon), (time,???,???,lat,lon)
;;
;;      This function will perform cosine weighting on the given array.
;;
;; USAGE:
;;      y = NewCosWeight(y)   ;will replace "y" with weighted array
;;      Y = NewCosWeight(y)   ;"Y" will be weighted, "y" will be unchanged

local x, qwlat, dims, numdims, pi, rad, coslat, nlat, a,b,c,d 
begin
        x = y
	if (typeof(x&lat).eq."double") then
	   qwlat = doubletofloat(x&lat)
	else
	   qwlat = x&lat
	end if
	
        dims=dimsizes(x)
        numdims=dimsizes(dims)
        pi     = 4.*atan(1.0)
        rad    = (pi/180.)
        coslat = cos(qwlat*rad)
        nlat   = dims(numdims-2)  
        if (numdims.eq.2) then
           do a = 0,nlat-1
              x(a,:) = x(a,:)*coslat(a)
           end do  
        end if
        if (numdims.eq.3) then
           do b = 0,nlat-1
              x(:,b,:) = x(:,b,:)*coslat(b)
           end do  
        end if
        if (numdims.eq.4) then
           do c = 0,nlat-1
              x(:,:,c,:) = x(:,:,c,:)*coslat(c)
           end do  
        end if
        if (numdims.eq.5) then
           do d = 0,nlat-1
              x(:,:,:,d,:) = x(:,:,:,d,:)*coslat(d)
           end do  
        end if
	if (numdims.ge.6) then
	   print("NewCosWeight accepts an array with 5 dimensions or less, array has "+numdims+" dimensions, exiting")
	   exit
	end if
        if (isatt(x,"long_name")) then
           x@long_name = x@long_name + " (cosine weighted)"
        end if
return(x)
end
;*************************************************************************
; D. Shea

; Unfortunately, NCL's built-in function, "addfiles" does not act like
; the "addfile". It does not return any meta information.

; This function will return all the attributes and coordinate
; variables of a variable returned by "addfiles". This does make
; an assumption in the case of "join" that the leftmost dimension
; is the coordinate variable that must be treated 'specially'
;

; sample usage:
;        diri = "/fs/cgd/data0/shea/"
;        fili = "annual*.nc"
;        fils = systemfunc ("ls "+diri+fili)
;
;        f    = addfiles (fils, "r")
;
;        ListSetType (f, "cat")     ; default
;    or
;        ListSetType (f, "join")    ; [extra dimension]
;
;        T    = addfiles_GetVar (f, fils, "T" )
undef("addfiles_GetVar")
function addfiles_GetVar (f:list, fils[*]:string, varName:string)
local x, dimx, rankx, g, X, dimX, rankX, i
begin
  x     = f[:]->$varName$             ; returned variable [vlaues ONLY]
  dimx  = dimsizes(x)                  
  rankx = dimsizes(dimx)              ; # dimensions [rank]

  g     = addfile (fils(0), "r")      ; read in one variable
  X     = g->$varName$                ; with original atts + coord vars
  dimX  = dimsizes(X)                  
  rankX = dimsizes(dimX)              ; # dimensions [rank]

  copy_VarAtts (X,x)                  ; copy attributes
                                      ; copy/create coordinate variables
  if (rankx.eq.(rankX+1) ) then       ; must be "join"
     do i=0,rankX-1                    
        if (.not.ismissing(X!i)) then
            x!(i+1) = X!i             ; dimensions are offset by one
            if (iscoord(X,X!i) ) then 
	        x&$x!(i+1)$ = X&$X!i$
            end if
        end if
     end  do
                                      ; add the extra dim stuff
     x!0    = "case"                  ; arbitrary dimension name
     x&$x!0$ = ispan(0,dimx(0)-1,1)   ; sequential sequence

  else                                ; should be "cat"
 
     if (rankx.eq.rankX ) then               
         do i=0,rankX-1                    
            if (.not.ismissing(X!i)) then
                x!i = X!i             ; name all dimensions
	        if (iscoord(X,X!i) ) then 
	            if (i.eq.0 ) then 
                        x&$x!0$ = f[:]->$x!0$  ; leftmost dimension
	            else
	                x&$x!i$ = X&$X!i$      ; rightmost dimensions
                    end if
                end if
            end if
         end  do
      else 
         print ("function addfiles_GetVar: ERROR: dimension problem")
      end if
   end if

  return (x)
end
;************************************************************************
; M. Haley
; This procedure was copied from eofMeta in NCL V6.4.0 to handle 
; metadata for the new eofunc_n function, with an added "dim"
; argument to indicate the time dimension. 
;
; eofMeta (below) has been modified to call this routine with "dim"
; set to the rightost argument.
undef ("eofMeta_n")
procedure eofMeta_n (data:numeric, neval:integer, eof:numeric,dim[1])
local evn, dimd, dime, nDimd, nDime, i
begin
  if (isatt(data,"long_name") .or. isatt(data,"description") .or. \
      isatt(data,"standard_name") ) then
      eof@long_name = "EOF: "+getLongName(data)
  end if
  if (isatt(data,"lev") ) then
      eof@lev = data@lev
  end if

  evn  = ispan(1,neval,1)  ; built-in function     
  evn@long_name = "eigenvalue number"
  evn@units     = ""
  evn!0         = "evn"   ; name dimension
  evn&evn       =  evn    ; make coord variable
  
  eof!0     = "evn"        ; name eof leftmost dimension
  eof&evn   =  evn         ; assign coord var

  dimd = dimsizes(data)
  dime = dimsizes(eof)
  nDimd= dimsizes(dimsizes(data))               ; rank
  nDime= dimsizes(dimsizes(eof))                

; Copy dimension names and coord arrays in two sections,
; skipping over 'dim' dimension.
  do i=0,dim-1           ; do not use 'dim' dimension, and skip the 0th dimension of eof
     if (.not.ismissing(data!i)) then
         eof!(i+1) = data!i
         if (iscoord(data,data!i) ) then
	     eof&$eof!(i+1)$ = data&$data!i$
         end if
     end if
  end  do

  do i=dim+1,nDimd-1
     if (.not.ismissing(data!i)) then
         eof!i = data!i
         if (iscoord(data,data!i) ) then
	     eof&$eof!i$ = data&$data!i$
         end if
     end if
  end  do
end

;************************************************************************
; D. Shea
; called internally by eofcov_Wrap, eofcor_Wrap, eof_pcmsg_Wrap, 
; eof_pcmsg_Wrap, eofunc_Wrap. 
; Wrapper for NCL "eofxxxx" functions that copies coordinate variables
;
; M. Haley copied eofMeta to eofMeta_n (above) and added a "dim" argument
; in NCL V6.4.0. This procedure now just calls eofMeta_n with the 
; appropriate "dim" argument.
; 
undef ("eofMeta")
procedure eofMeta (data:numeric, neval:integer, eof:numeric)
local dims, rank
begin
  rank = dimsizes(dimsizes(data))
  eofMeta_n(data,neval,eof,rank-1)   ; pass the rgtmost dimension index
end

; **********************************************************************
; D. Shea
; wrappers for NCL functions "eofcov"/"eofcor"  that copies coord variables

; usage:  eof = eofcov_Wrap (data, neval)    
;         eof = eofcor_Wrap (data, neval)    

undef ("eofcov_Wrap")
function eofcov_Wrap (data:numeric, neval:integer) 
; wrapper for NCL function "eofcov"  that copies attributes and coordinate vars
local eof
begin
  eof = eofcov(data, neval)    ; invoke built-in function
  eofMeta (data, neval, eof)   ; add meta information
  eof@matrix = "covariance"
  return  (eof)                ; return
end
 
undef ("eofcor_Wrap")
function eofcor_Wrap (data:numeric, neval:integer) 
; wrapper for NCL function "eofcor"  that copies attributes and coordinate vars
local eof
begin
  eof  = eofcor(data, neval)
  eofMeta (data, neval, eof)
  eof@matrix = "correlation"
  return (eof)
end

undef ("eofcov_pcmsg_Wrap")
function eofcov_pcmsg_Wrap (data:numeric, neval:integer, pcrit:numeric) 
; wrapper for NCL function "eofcov_pcmsg"  that copies attributes and coordinate vars
local eof
begin
  eof = eofcov_pcmsg(data, neval, pcrit)    ; invoke built-in function
  eofMeta (data, neval, eof)   ; add meta information
  eof@matrix = "covariance"
  eof@pcrit  = pcrit
  return  (eof)                ; return
end
 
undef ("eofcor_pcmsg_Wrap")
function eofcor_pcmsg_Wrap (data:numeric, neval:integer, pcrit:numeric) 
; wrapper for NCL function "eofcor_pcmsg"  that copies attributes and coordinate vars
local eof
begin
  eof = eofcor_pcmsg(data, neval, pcrit)
  eofMeta (data, neval, eof)
  eof@matrix = "correlation"
  eof@pcrit  = pcrit
  return (eof)
end

; **********************************************************************
; M. Haley
; This procedure was copied from eofMeta in NCL V6.4.0 to handle 
; metadata for the new eofunc_n function, with an added "dim"
; argument to indicate the time dimension. 
;
; eofTsMeta (below) now calls this procedure with the last argument
; set to the rightmost dimension index.

undef ("eofTsMeta_n")
procedure eofTsMeta_n (data:numeric, eof:numeric, eofTs:numeric, dim[1])
local dimd, dime, nDimd, nDime, i, j
begin
  dimd = dimsizes(data)
  dime = dimsizes(eofTs)
  nDimd= dimsizes(dimsizes(data))               ; rank
  nDime= dimsizes(dimsizes(eofTs))                

  if (isatt(data,"long_name") .or. isatt(data,"description") .or. \
      isatt(data,"standard_name") ) then
      eofTs@long_name = "EOF: Amplitude: "+getLongName(data)
  end if

  if (.not.ismissing(eof!0)) then
      eofTs!0 = eof!0
      if (iscoord(eof,eof!0) ) then
          eofTs&$eofTs!0$ = eof&$eof!0$
      end if
  end if

  i   = dim                ; 'dim'th dimension of data
  j   = nDime-1            ; rightmost dimension of eofTs
  if (.not.ismissing(data!i)) then
      eofTs!j = data!i
      if (iscoord(data,data!i) ) then
          eofTs&$eofTs!j$ = data&$data!i$
      end if
  end if
  
end

; **********************************************************************
; D. Shea
; Called internally by eofcov_ts_Wrap, eofcor_ts_Wrap, eofunc_ts_Wrap.
;
; Wrappers for NCL functions "eofcov_ts" "eofcor_ts", "eofunc_ts" that
; copies coordinate variables.
;
; M. Haley copied eofTsMeta to eofTsMeta_n (above) and added a "dim"
; argument in NCL V6.4.0. This procedure now just calls eofTsMeta_n
; with the appropriate "dim" argument.

undef ("eofTsMeta")
procedure eofTsMeta (data:numeric, eof:numeric, eofTs:numeric)
local rank
begin
  rank = dimsizes(dimsizes(data))
  eofTsMeta_n(data,eof,eofTs,rank-1)  
end

; **********************************************************************
; D. Shea
; wrappers for NCL functions "eofcov_ts"/"eofcor_ts"  that copies coord variables

; usage:  eof = eofcov_ts_Wrap (data, eof)    
;         eof = eofcor_ts_Wrap (data, eof)    

undef ("eofcov_ts_Wrap")
function eofcov_ts_Wrap (data:numeric, eof:numeric)
local eofTS 
begin
  eofTs = eofcov_ts(data, eof)    ; invoke built-in function
  eofTsMeta (data, eof, eofTs)    ; add meta information
  return  (eofTs)                 ; return
end
 
undef ("eofcor_ts_Wrap")
function eofcor_ts_Wrap (data:numeric, eof:numeric)
local eofTS 
begin
  eofTs = eofcor_ts(data, eof)    ; invoke built-in function
  eofTsMeta (data, eof, eofTs)    ; add meta information
  return  (eofTs)                 ; return
end


; **********************************************************************
; D Shea
;
; Project the eofts onto the Data and return an 'eof' pattern.
; No normalization is performed.
;
; usage:   eof   = eofcov (x,neval)
;          eofts = eofcov_ts (x,eof)  ; where 'deof' is the data used
;          EOF   = eoftsData2eof(eof_ts, deof, False) 
;
; currently: the option argument is not used. 
;
undef("eoftsData2eof")
function eoftsData2eof (eof_ts[*][*], data:numeric, option:logical )
local dimd, rank, dimts, neval, ntim, npts, eof, nev \
    , np, opt, n, mx, MX, ny, NY
begin
  dimd  = dimsizes(data)
  rank  = dimsizes(dimd)
  if (rank.lt.2 .or. rank.gt.3) then
      print("-----")
      print("contributed: eoftsData2eof: only works with arrays of2 or 3")
      exit
  end if

  dimts = dimsizes(eof_ts)
  neval = dimts(0)
  ntim  = dimts(1)

  if (rank.eq.2) then
      npts = dimd(0)
      
      eof = new ( (/neval,npts/), typeof(data),getFillValue(data) ) 
      do nev=0,neval-1
        do np=0,npts-1
           eof(nev,np) = sum(eof_ts(nev,:)*data(np,:))
        end do
      end do 

      eof!0 = "evn"
      if (isdimnamed(data,0)) then
          eof!1 = data!0
          if (iscoord(data,data!0) ) then
              eof&$eof!0$ = data&$data!0$
          end if
      else
          eof!1 = "npts"
      end if
  end if

  if (rank.eq.3) then
      NY = dimd(0)
      MX = dimd(1)
      
      eof = new ( (/neval,NY,MX/), typeof(data),getFillValue(data) ) 
      do nev=0,neval-1
        do ny=0,NY-1
        do mx=0,MX-1
           eof(nev,ny,mx) = sum(eof_ts(nev,:)*data(ny,mx,:))
        end do
        end do
      end do 

      eof!0 = "evn"
      do n=1,2
         nm1 = n-1
         if (isdimnamed(data,n)) then
             eof!n = data!nm1
           if (iscoord(data,data!nm1) ) then
               eof&$eof!n$ = data&$data!nm1$
           end if
         end if
      end do
  end if

  return(eof)
end

undef ("eofunc_Wrap")
function eofunc_Wrap (data:numeric, neval:integer, optEOF:logical) 
; wrapper for NCL function "eofunc"  that copies attributes and coordinate vars
local eofx
begin
  eofx = eofunc(data, neval, optEOF)    ; invoke built-in function
  eofMeta (data, neval, eofx)           ; add meta information
  return  (eofx)                        ; return
end

undef ("eofunc_n_Wrap")
function eofunc_n_Wrap (data:numeric, neval:integer, optEOF:logical, dim[1]) 
; wrapper for NCL function "eofunc_n"  that copies attributes and coordinate vars
local eofx
begin
  eofx = eofunc_n(data, neval, optEOF,dim)  ; invoke built-in function
  eofMeta_n (data, neval, eofx, dim)        ; add meta information
  return  (eofx)                            ; return
end

undef ("eofunc_ts_Wrap")
function eofunc_ts_Wrap (data:numeric, eof:numeric, optETS:logical)
; wrapper for NCL function "eofunc_ts"  that copies attributes and coordinate vars
local eofTS 
begin
  eofTs = eofunc_ts(data, eof, optETS) ; invoke built-in function
  eofTsMeta(data, eof, eofTs)      ; add meta information
  return  (eofTs)                   ; return
end

undef ("eofunc_ts_n_Wrap")
function eofunc_ts_n_Wrap (data:numeric, eof:numeric, optETS:logical, dim[1])
; wrapper for NCL function "eofunc_ts_n"  that copies attributes and coordinate vars
local eofTS 
begin
  eofTs = eofunc_ts_n(data, eof, optETS,dim) ; invoke built-in function
  eofTsMeta_n(data, eof, eofTs,dim)      ; add meta information
  return  (eofTs)                   ; return
end

undef ("eofunc_varimax_Wrap")
function eofunc_varimax_Wrap (eof:numeric, optEVX:integer)
local eofEVX
begin
  eofEVX    = eofunc_varimax(eof, optEVX) ; invoke built-in function
  eofEVX@op = "Kaiser Varimax Rotation: opt="+optEVX
  copy_VarCoords(eof, eofEVX)

  return  (eofEVX)                        ; return
end

;*******************************************************************
; Returns the varimax rotated EOFs in descending order.
;*******************************************************************
undef("eof_varimax_reorder")
procedure eof_varimax_reorder ( eofr )
local dime, rank, neof, ip, EOFR
begin
  dime = dimsizes( eofr )
  rank = dimsizes( dime )
  if (rank.lt.2 .or. rank.gt.3) then
      print("eof_varimax_reorder: Currently eofr must be 2D or 3D.")
      print("eof_varimax_reorder: ***** Return original array ****")
  end if
  neofr= dime(0)
  
  ip   = dim_pqsort( eofr@pcvar_varimax, -1 )   ; descending order

  EOFR = eofr                                   ; temp and copt meta
  do ne=0,neofr-1
     if (rank.eq.2) then
         eofr(ne,:)   = (/ EOFR(ip(ne),:) /)
     end if
     if (rank.eq.3) then
         eofr(ne,:,:) = (/ EOFR(ip(ne),:,:) /)
     end if
     eofr@pcvar_varimax(ne)    = (/ EOFR@pcvar_varimax(ip(ne))    /)
     eofr@variance_varimax(ne) = (/ EOFR@variance_varimax(ip(ne)) /)
  end do
  
end
; **************************************************************
; D. Shea
; different entry name .... 
; **************************************************************

undef("eofunc_varimax_reorder")
procedure eofunc_varimax_reorder ( eofr )
begin
   eof_varimax_reorder( eofr )
end


; **************************************************************
; D. Shea
; reorder [flip] the longitude coordinate variable
; NOTE:
; (1) coordinate "lon"  is assumed to be the rightmost dim
; (2) the longitude is assume to be "global"
; (3) cyclic pt NOT allowed here

; change to the return variable:  Feb 9 2001
; instead of returning "x" I return "temp"

undef("lonFlip")
function lonFlip (x:numeric)   
local dimx, nDim, mon, mlon2, temp, i
begin

  dimx  = dimsizes(x)
  nDim  = dimsizes(dimx)
  if (nDim.gt.5) then
      print ("lonflip: too many dimensions: nDim="+nDim)
      return (x)
  end if

  mlon  = dimx(nDim-1)
  if (mlon%2 .ne. 0) then
      print ("lonflip: longitude dimension size must be even: mlon="+mlon)
      exit
  end if
  mlon2 = mlon/2

  if (mlon%2.ne.0) then
      print("=======================")
      print("Currently, lonFlip requires that the number")
      print("of longitudes be even. mlon="+mlon          )
      print("=======================")
      exit
  end if

  temp = x
  if (nDim.eq.1) then
      temp(0:mlon2-1) = (/ x(mlon2:)    /)
      temp(mlon2:)    = (/ x(0:mlon2-1) /)
  end if
  if (nDim.eq.2) then
      temp(:,0:mlon2-1) = (/ x(:,mlon2:)    /)
      temp(:,mlon2:)    = (/ x(:,0:mlon2-1) /)
  end if
  if (nDim.eq.3) then
      temp(:,:,0:mlon2-1) = (/ x(:,:,mlon2:)    /)
      temp(:,:,mlon2:)    = (/ x(:,:,0:mlon2-1) /)
  end if
  if (nDim.eq.4) then
      temp(:,:,:,0:mlon2-1) = (/ x(:,:,:,mlon2:)    /)
      temp(:,:,:,mlon2:)    = (/ x(:,:,:,0:mlon2-1) /)
  end if
  if (nDim.eq.5) then
      temp(:,:,:,:,0:mlon2-1) = (/ x(:,:,:,:,mlon2:)    /)
      temp(:,:,:,:,mlon2:)    = (/ x(:,:,:,:,0:mlon2-1) /)
  end if
            
  i = nDim-1                      ; last dimension
  if (.not.ismissing(x!i)) then
     if (iscoord(x,x!i)) then
         xlon = x&$x!i$           ; original coord variable
         tlon = (/ xlon /)
        
         xlon(0:mlon2-1) = (/ tlon(mlon2:) /)
         xlon(mlon2:)    = (/ tlon(0:mlon2-1)/)            

         if (tlon(0).ge.0.) then  ; (say) 0=>355
             xlon(0:mlon2-1) = (/ tlon(mlon2:) - 360 /)
             xlon(mlon2:)    = (/ tlon(0:mlon2-1)    /)            
         else                     ; (say) -180=>175
             xlon(0:mlon2-1) = (/ tlon(mlon2:) /)
             xlon(mlon2:)    = (/ tlon(0:mlon2-1) + 360  /)           
         end if
         
         temp&$x!i$ = xlon           ; new coord variable
     end if
  else
     print ("lonFlip: warning: last dimension is not named")
  end if

  temp@lonFlip = "longitude coordinate variable " + \
              "has been reordered via lonFlip"
  return (temp)
end
; ******************************************************************
; D. Shea
; pivot (flip) the contents of array "x" about some arbitrary
; user specified longitude. The effect is similar to "lonFlip"
; However, lonFlip will pivot about the mid point [whatever that is]
; while thus function allows the user to specify what lon to pivot about.

; grid must be "global" [no cyclic point] and it assumes that the
; rightmost dimension of "x" is a coordinate variable corresponding
; to longitude.

; change to the return variable:  Feb 9 2001

; usage    xNew = lonPivot (x, 20.)    ; pivot about 20E
;             x = lonPivot (x, 20.)    ; can overwrite
undef("lonPivot")
function lonPivot (x:numeric, pivotLon:numeric)
local dimx, nDim, lonName, temp, xlon, indP, mlon, indL, n \
    , temp, tlon, indt
begin
  dimx = dimsizes(x)
  nDim = dimsizes(dimx)
  if (nDim.gt.5) then
      print ("contributed.ncl: lonflip: too many dims: nDim="+nDim)
      return (x)
  end if

  if (.not.ismissing(x!(nDim-1)) ) then
      lonName = x!(nDim-1)
  else
      print ("contributed.ncl: lonPivot: lon coord var is msg")
      exit
  end if 

  temp     = x
  xlon     = x&$lonName$                ; original coord variable
  xlon!0   = "lon"
  xlon&lon = (/ xlon /)
  indP = ind(xlon.eq.xlon({pivotLon}))  ; must be exact
  if (ismissing(indP)) then
      print ("contributed.ncl: lonPivot: bad pivot value")
      exit
  end if

  mlon = dimx(nDim-1)    ; # of longitudes
  indL = mlon-1          ; last index
  n    = indL-indP

  if (nDim.eq.1) then
      temp(0:n)      = (/ x(indP:indL)/)
      temp(n+1:)     = (/ x(0:indP-1) /)
  end if
  if (nDim.eq.2) then
      temp(:,0:n)    = (/ x(:,indP:indL)/) 
      temp(:,n+1:)   = (/ x(:,0:indP-1) /)
  end if
  if (nDim.eq.3) then
      temp(:,:,0:n)  = (/ x(:,:,indP:indL)/)
      temp(:,:,n+1:) = (/ x(:,:,0:indP-1) /)
  end if
  if (nDim.eq.4) then
      temp(:,:,:,0:n)  = (/ x(:,:,:,indP:indL)/)
      temp(:,:,:,n+1:) = (/ x(:,:,:,0:indP-1) /)
  end if
  if (nDim.eq.5) then
      temp(:,:,:,:,0:n) = (/ x(:,:,:,:,indP:indL)/)
      temp(:,:,:,:,n+1:)= (/ x(:,:,:,:,0:indP-1) /)
  end if

  tlon       = new ( mlon, typeof(xlon) )
  tlon(0:n)  = (/ xlon(indP:indL) /)
  tlon(n+1:) = (/ xlon(0:indP-1)/)            
  delete (tlon@_FillValue)

  if (tlon(0).ge.0.) then  ; (say) 20,25,...,350,355,0,5,..
      indt = ind(tlon.lt.tlon(0))
      if (.not.all(ismissing(indt))) then
          tlon(indt) = (/ tlon(indt) + 360. /)
      end if
  end if
  if (tlon(0).ge.180. .or. tlon(0).eq.360.) then
      tlon = (/ tlon -360. /)
  end if
  copy_VarAtts   (xlon,tlon)
         
  temp&$lonName$ = tlon           ; new coord variable

  temp@lonPivot  = "reordered via lonPivot [NCL]: pivotLon="+pivotLon
  return (temp)
end

; ******************************************************************
; Dennis Shea

undef("natgrid_Wrap")
function natgrid_Wrap (xi[*]:numeric,yi[*]:numeric, fi:numeric \
                      ,xo[*]:numeric,yo[*]:numeric)

local fo, dimfi, nDimi, nDimo
begin
  fo = natgrid (xi,yi,fi, xo,yo)            ; perform interpolation 
                                             
  dimfi= dimsizes(fi)
  nDimi= dimsizes(dimsizes(fi))                 ; rank: # of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  if (nDimi.ge.2) then                          ; copy coord vars
      copy_VarCoords_1(fi,fo)                   ; except for rightmost dim
  end if

  nDimo = dimsizes(dimsizes(fo))

  dNam = getvardims( yo )
  if (.not.ismissing(dNam)) then
      fo!(nDimo-2) = dNam
      fo&$dNam$    = yo
  else
      fo!(nDimo-2) = "y"
      fo&y         = yo
  end if
  delete(dNam)

  dNam = getvardims( xo )
  if (.not.ismissing(dNam)) then
      fo!(nDimo-1) = dNam
      fo&$dNam$    = xo
  else
      fo!(nDimo-1) = "x"
      fo&x         = xo
  end if

  return (fo)
end

; ******************************************************************
; Dennis Shea

undef("obj_anal_ic_Wrap")
function obj_anal_ic_Wrap (xi[*]:numeric,yi[*]:numeric, fi:numeric \
                          ,xo[*]:numeric,yo[*]:numeric,rscan[*]:numeric \
                          ,opt[1]:logical)

local fo, dimfi, nDimi, nDimo
begin
  fo = obj_anal_ic (xi,yi,fi, xo,yo,rscan,opt)  ; perform interpolation 
                                             
  dimfi = dimsizes(fi)
  nDimi = dimsizes(dimsizes(fi))                ; rank: # of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  if (nDimi.ge.2) then                          ; copy coord vars
      copy_VarCoords_1(fi,fo)                   ; except for rightmost dim
  end if

  nDimo = dimsizes(dimsizes(fo))

  dNam = getvardims( yo )
  if (.not.ismissing(dNam)) then
      fo!(nDimo-2) = dNam
      fo&$dNam$    = yo
  else
      fo!(nDimo-2) = "y"
      fo&y         = yo
  end if
  delete(dNam)

  dNam = getvardims( xo )
  if (.not.ismissing(dNam)) then
      fo!(nDimo-1) = dNam
      fo&$dNam$    = xo
  else
      fo!(nDimo-1) = "x"
      fo&x         = xo
  end if

  return (fo)
end
; ******************************************************************
; Dennis Shea
; wrapper for NCL function runave

undef ("runave_Wrap")
function runave_Wrap( x:numeric, nave[1]:integer, kopt[1]:integer)
local xRunAve
begin
 ;xRunAve = x                               ; 10 Nov 2008
  xRunAve = runave (x, nave, kopt)   
  copy_VarMeta(x, xRunAve)                  ; 10 Nov 2008
  xRunAve@runave_op_ncl = "runave: nave="+nave 

  return(xRunAve)
end
; ******************************************************************
; Mary Haley
; wrapper for NCL function runave_n

undef ("runave_n_Wrap")
function runave_n_Wrap( x:numeric, nave[1]:integer, kopt[1]:integer, \
                        dim[1]:integer)
local xRunAve
begin
 ;xRunAve = x
  xRunAve = runave_n (x, nave, kopt, dim)   
  copy_VarMeta(x, xRunAve)
  xRunAve@runave_op_ncl = "runave_n: nave="+nave 

  return(xRunAve)
end
; ******************************************************************
; Dennis Shea
; wrapper for NCL function wgt_runave

undef ("wgt_runave_Wrap")
function wgt_runave_Wrap( x:numeric, wgt[*]:numeric, kopt[1]:integer)
local wRunAve
begin
 ;wRunAve = x                               ; 10 Nov 2008
  wRunAve = wgt_runave (x, wgt, kopt)   
  copy_VarMeta(x, wRunAve)                  ; 10 Nov 2008
  wRunAve@wgt_runave_op_ncl = "wgt_runave"

  return(wRunAve)
end
; ******************************************************************
; Mary Haley
; wrapper for NCL function wgt_runave_n

undef ("wgt_runave_n_Wrap")
function wgt_runave_n_Wrap( x:numeric, wgt[*]:numeric, kopt[1]:integer, \
                            dim[1]:integer)
local wRunAve
begin
 ;wRunAve = x
  wRunAve = wgt_runave_n (x, wgt, kopt, dim)   
  copy_VarMeta(x, wRunAve)
  wRunAve@wgt_runave_op_ncl = "wgt_runave_n"

  return(wRunAve)
end
; ******************************************************************
; Dennis Shea
; wrapper for NCL function taper

undef ("taper_Wrap")
function taper_Wrap( x:numeric, pct[1]:numeric, kopt[1]:numeric)
local xTaper 
begin
 ;xTaper = x            
  xTaper = taper (x, pct, kopt)   
  copy_VarMeta(x, xTaper)       
  xTaper@taper_op_ncl = "taper: pct="+sprintf("%4.2f", pct)

  return(xTaper)
end

; ******************************************************************
; Mary Haley
; wrapper for NCL function taper_n, added 11/1/2009
;
undef ("taper_n_Wrap")
function taper_n_Wrap( x:numeric, pct[1]:numeric, kopt[1]:numeric,  \
                       dim[1]:integer)
local xTaper 
begin
 ;xTaper = x            
  xTaper = taper_n (x, pct, kopt, dim)
  copy_VarMeta(x, xTaper)       
  xTaper@taper_op_ncl = "taper_n: pct="+sprintf("%4.2f", pct)

  return(xTaper)
end

; ******************************************************************
; Mary Haley
; wrapper for NCL function escorc
;
; This wrapper is unique because of the multiple
; dimensionality possibilities of x and y.

undef ("escorc_Wrap")
function escorc_Wrap(x:numeric, y:numeric)
local ccr, ndims_x, ndims_y, rank_x, rank_y, xnames, ynames
begin
  dsizes_x = dimsizes(x)
  dsizes_y = dimsizes(y)
  rank_x   = dimsizes(dsizes_x)
  rank_y   = dimsizes(dsizes_y)

  ccr = escorc (x, y)

  if(rank_x.gt.1.and.rank_x.eq.rank_y) then
    copy_VarCoords_not_n (x, ccr, rank_x-1)
  else
    ccr_n = 0   ; dimension counter for ccr 
    if(rank_x.gt.1) then
      xnames = getvardims(x)
      do n=0,rank_x-2
        if (ismissing(xnames(n))) then
          xnames(n) = "ncl_"+ccr_n
        end if
        ccr!ccr_n = xnames(n)
        if(iscoord(x,xnames(n))) then
          ccr&$ccr!ccr_n$ = x&$xnames(n)$
        end if
        ccr_n = ccr_n + 1
      end do
    end if
    if(rank_y.gt.1) then
      ynames = getvardims(y)
      do n=0,rank_y-2
        if (ismissing(ynames(n))) then
          ynames(n) = "ncl_"+ccr_n
        end if
        ccr!ccr_n = ynames(n)
        if(iscoord(y,ynames(n))) then
          ccr&$ccr!ccr_n$ = y&$ynames(n)$
        end if
      end do
    end if
  end if
  ccr@escorc_op_ncl = "escorc"
  return(ccr)
end

; ******************************************************************
; Mary Haley
; wrapper for NCL function escorc_n
;
; This wrapper is a pain because of the multiple
; dimensionality possibilities of x and y.

undef ("escorc_n_Wrap")
function escorc_n_Wrap(x:numeric,y:numeric,xdims[*]:integer,\
                       ydims[*]:integer)
local ccr, rank_x, rank_y, xnames, ynames, n, ccr_n
begin
  rank_x = dimsizes(dimsizes(x))
  rank_y = dimsizes(dimsizes(y))

  ccr = escorc_n (x, y, xdims, ydims)
; copy_VarAtts(x, ccr)

  if(rank_x.gt.1.and.rank_x.eq.rank_y) then
    copy_VarCoords_not_n (x, ccr, xdims)
  else
    ccr_n = 0   ; dimension counter for ccr 
    if(rank_x.gt.1) then
      xnames = getvardims(x)
      do n=0,rank_x-1
        if (.not.any(n.eq.xdims)) then
          if(ismissing(xnames(n))) then
            xnames(n) = "ncl_"+ccr_n
          end if
          ccr!ccr_n = xnames(n)
          if(iscoord(x,xnames(n))) then
            ccr&$ccr!ccr_n$ = x&$xnames(n)$
          end if
          ccr_n = ccr_n + 1
        end if
      end do
    end if
    if(rank_x.gt.1) then
      ynames = getvardims(y)
      do n=0,rank_y-1
        if (.not.any(n.eq.ydims)) then
          if (ismissing(ynames(n))) then
            ynames(n) = "ncl_"+ccr_n
          end if
          ccr!ccr_n = ynames(n)
          if(iscoord(y,ynames(n))) then
            ccr&$ccr!ccr_n$ = y&$ynames(n)$
          end if
          ccr_n = ccr_n + 1
        end if
      end do
    end if
  end if

  ccr@escorc_op_ncl = "escorc_n"

  return(ccr)
end

; ******************************************************************
; Dennis Shea
; wrapper for NCL function wgt_areaave

undef ("wgt_areaave_Wrap")
function wgt_areaave_Wrap (x:numeric, wgty[*]:numeric, wgtx[*]:numeric, opt:integer)
local dimx, rank, areaAve
begin
  dimx = dimsizes(x)
  rank = dimsizes(dimx)
  if (rank.lt.2) then
      print("wgt_areaave_Wrap: incorrect rank")
      exit
  end if

  areaAve  = wgt_areaave (x, wgty, wgtx, 0)   

  if (rank.gt.2) then
      copy_VarMeta (x, areaAve)          ; contributed.ncl (copy meta data)
  else
      copy_VarAtts (x, areaAve)
      if (isatt(areaAve,"gwt")) then
          delete(areaAve@gwt)
      end if
  end if

  areaAve@wgt_areaave_op_ncl = "Area Average"

  return(areaAve)
end
; ******************************************************************
undef("wgt_runave_leftdim")
function wgt_runave_leftdim(x:numeric, wgt[*]:numeric, opt[1]:integer)
;
; utility routine ... makes for cleaner code
; Reorder so time is rightmost; applies Lanczos weights, reorder back
;
; Updated 11/7/2009 to use wgt_runave_n_Wrap
;
begin
  return( wgt_runave_n_Wrap( x, wgt, opt, 0) )
end
; ******************************************************************
undef("taper_leftdim")
function taper_leftdim(x:numeric, pct[1]:numeric, opt[1]:integer)
;
; utility routine ... makes for cleaner code
; Reorder so time is rightmost; applies taper, reorder back
;
; Updated 11/1/2009 to use taper_n_Wrap. No reordering needed.
;
local n, dimx, rank, dNam
begin
  if (pct.le.0 .or. pct.gt.1) then
      print("**************************************************")
      print("taper_leftdim: no taper: bad input value: pct="+pct)
      print("**************************************************")
      return
  end if

  dimx = dimsizes(x)
  rank = dimsizes( dimx )

  if (rank.eq.1) then
      return( taper_Wrap( x, PCT, opt) )
  end if

  dNam = getvardims( x )
  do n=0,rank-1
     if (ismissing(dNam(n))) then
         dNam(n) = "ncl_"+n
         x!n     = dNam(n)
     end if
  end do

  return( taper_n_Wrap( x, pct, opt, 0) )
end
;**********************************************************
undef("dtrend_leftdim")
function dtrend_leftdim(x:numeric, opt[1]:logical)
;
; utility routine ... makes for cleaner code
;
; Pre V5.2.0: Reorder so time is rightmost; detrend the time series,
;     reorder back
;
; Post V5.2.0: Updated 11/1/09 to use dtrend_n.
;
local x_dtrend
begin
  x_dtrend = x                             ; retain meta data
  x_dtrend = dtrend_n( x, opt, 0) 
  return (x_dtrend)
end

; ******************************************************************
; Ethan Alpert
;
; This is for fortran SEQUENTIAL files that need to be byte swapped. This
; requires *all* records in the file to be the same type and dimension size.
; It also requires that the use provide the dimensionality of each record.
; The number of records is *not* necessary. If you input wrong dimensions
; for each record it *will* screw up the data.
;
; Output file will be sequential access file
;
;   [06 Feb 2011 - added support for "long" dims]

undef ("fbinseqSwap1")
procedure fbinseqSwap1(in_file[1]:string, outfile[1]:string \
                      ,type[1]:string,  dims[*])
local indata, tot, n, sz, i, done, tmp
begin
	indata = cbinread(in_file,-1,"byte")
	tot = tolong(dimsizes(indata))
       ;print(tot)
	n = tolong(product(dims))   ; Use longs to handle large sizes
       ;print(n)
	if(type.eq."long")
		print("fbinswap: won't convert longs due to fact that " + \
                      "they can be different sizes on different machines, " + \
                      "use int64/double for 64 bit ints and integer for 32 bit longs")
		return
	end if
	if(type.eq."short")
		sz = 2l
	end if
	if(any(type.eq.(/"integer","float"/)))
		sz = 4l
	end if
	if(any(type.eq.(/"int64","double"/)))
		sz = 8l
	end if
;
; Skip first control word
;
	i    = 4l
	done = False

	do while(.not.done)
		tmp = onedtond(indata(i:i+n*sz-1),(/n,sz/))
	 	fbinrecwrite(outfile,-1,tmp(:,::-1))
;
; Skip control word after current record and before next
;
		i = i + 8l + n*sz
		if(i.ge.tot)
			done = True
		end if
	end do
end	

; ******************************************************************
; Ethan Alpert
; more memory friendly version of fbinseqSwap1
; note: it requires an extra dimension
;
; Output file will be fortran sequential access file
;
;   [06 Feb 2011 - added support for "long" dims]

undef ("fbinseqSwap2")
procedure fbinseqSwap2(in_file[1]:string, outfile[1]:string, \
                       type[1]:string, nrec[1]:integer, dims[*])
local n, sz, i, indata, tmp
begin
	n = tolong(product(dims))
	if(type.eq."long")
		print("fbinswap: won't convert longs due to fact that " + \
                      "they can be different sizes on different machines, " + \
                      "use int64/double for 64 bit longs and integer for 32 bit longs")
		return
	end if
	if(type.eq."short")
		sz = 2l
	end if
	if(any(type.eq.(/"integer","float"/)))
		sz = 4l
	end if
	if(any(type.eq.(/"int64","double"/)))
		sz = 8l
	end if
	recsize = n*sz

	do rnum = 0,nrec,1
		indata = fbindirread(in_file,rnum,recsize+8,"byte")
		tmp = onedtond(indata(4:4+recsize-1),(/n,sz/))
	 	fbinrecwrite(outfile,-1,tmp(:,::-1))
	end do
end	
;******************
;
; This is for fortran DIRECT access files that need to be byte swapped.
; formerly called "reverseEndian"
;
; Output file will be direct access binary file
;

undef ("fbindirSwap")
procedure fbindirSwap (infile:string, dims \
                      ,inType:string, outFile:string)
                       
; procedure that reads byte reversed data 
; create a new output file which can then be read
; via fbindirread or cbinread

; Example:   fbindirSwap ("/shea/bigEnd", (/100,72,144/) \
;                        ,"float","/shea/littleEnd")
local nBytes, dimBytes, indata
begin
 ;print ("Start fbindirSwap: "+systemfunc("date"))

  nBytes = 4l     ; treat as longs just in case large dimensions
  if (inType.eq."double") then
      nBytes  = 8l
  end if
  dimBytes  = (/product(dims),nBytes/)
 ;print (dimBytes)
                                  ; read data as bytes
  indata = fbindirread(infile,0,dimBytes,"byte")
 ;printVarSummary (indata)
                                  ; write to temporary file
                                  ; and reverse byte order

  system ("/usr/bin/rm "+outFile) ; delete if it exists

  fbindirwrite(outFile,indata(:,::-1))
                                  
 ;print ("End   fbindirSwap: "+systemfunc("date"))
end
;***************************************************************
; D. Shea
; Calculates a base 2 logarithm.

  undef("LOG2")
  function LOG2(x:numeric)
  begin
	return(log10(x)/log10(2.))
  end
; **********************************************************************
; D. Shea
; wrapper for NCL function "zonal_mpsi"  that copies coord variables
; and adds attributes

; usage:  zmpsi = zonal_mpsi_Wrap (v, lat, p, ps)

undef ("zonal_mpsi_Wrap")
function zonal_mpsi_Wrap (v:numeric, lat[*]:numeric, p[*]:numeric, ps:numeric)
local zmpsi
begin
  zmpsi = zonal_mpsi (v, lat, p, ps)
  copy_VarCoords_1 (v, zmpsi)
  zmpsi@long_name = "Zonal Meridional Stream Function"
  zmpsi@units     = "kg/s"
  return (zmpsi)
end

; **********************************************************************
; D. Shea
; transpose a matrix: copies all attributes and coordinate variables

; usage:  xT = transpose (x)

undef ("transpose")
function transpose (x)
local dimx, N, N1, n, X, namedDim, xT
begin
  dimx  = dimsizes (x)
  N     = dimsizes( dimx )        ; rank of matrix
  N1    = N-1                     ; for convenience

  if (N.gt.6) then
      print ("transpose: currently set up for a max of 6 dimensions")
      exit
  end if
                                  ; is each dimension named?
  namedDim = getvardims(x)
  do n=0,N1     
     if (ismissing(namedDim(n))) then
         x!n = "dim"+n 
     end if
  end do

  if (N.eq.1) then
      xx = onedtond(x, (/1,dimx/) )
      xx!0 = "dumy"
      xx!1 = x!0
      xT = xx($xx!1$|:, $xx!0$|:)
      delete(xT!1)
      return(xT)
  end if

  if (N.eq.2) then
      xT = x($x!N1$|:, $x!(N1-1)$|:)
  end if

  if (N.eq.3) then
      xT = x($x!N1$|:, $x!(N1-1)$|:, $x!(N1-2)$|: )
  end if

  if (N.eq.4) then
      xT = x($x!N1$|:, $x!(N1-1)$|:, $x!(N1-2)$|:, $x!(N1-3)$|: )
  end if

  if (N.eq.5) then
      xT = x($x!N1$|:, $x!(N1-1)$|:, $x!(N1-2)$|:, $x!(N1-3)$|:, $x!(N1-4)$|: )
  end if

  if (N.eq.6) then
      xT = x($x!N1$|:, $x!(N1-1)$|:, $x!(N1-2)$|:, $x!(N1-3)$|:, $x!(N1-4)$|:, $x!(N1-5)$|: )
  end if
                                ; if temporary dim name
  do n=0,N1     
     if (ismissing(namedDim(n))) then
        delete(x!n)             ; delete temporary name 
        delete(xT!(N1-n))
     end if
  end do

  return (xT)
end

; ------------------------
 
; D Shea
; compute a user specified seasonal mean [all are three-month means]
; DJF,JFM,FMA,MAM,AMJ,MJJ,JJA,JAS,ASO,SON,OND,NDJ

; first (DJF=JF) /last (NDJ=ND) seasons are 2-month averages
;
; x(time,lat,lon),  x(time,lev,lat,lon)
;   ^^^^^^^^^^^^      ^^^^^^^^^^^^^^^^   
; must have named dim BUT can be ANY names
;
; The input "x" are assumed to contain monthly mean data
; The size of "time" MUST be divisible by 12.
; Also, it is assumed the "Jan" is the 1st month.
;
; xMon(time)  or  xMon(time,lat,lon)   or  xMon(time,lev,lat,lon)
; USAGE     xJJA = month_to_season (xMon, "JJA")
;
; RESULT    xJJA(time/12,lev,lat,lon)    xJJA(time/12,lat,lon)
;
; Note: this returns (7/2003) NMO as an attribute

undef ("month_to_season")
function month_to_season (xMon:numeric, SEASON:string)

local season,NMO,dimx,rank,ntim,nlat,mlon,nmos,nyrs,con \
    , nyrStrt,nyrLast,nyr,n,xSea, klev, dName,cv,xSea
begin
  season  =  (/"DJF","JFM","FMA","MAM","AMJ","MJJ" \
              ,"JJA","JAS","ASO","SON","OND","NDJ" /)

  NMO     = ind(season.eq.SEASON)  ; index corresponding to season
  if (ismissing(NMO)) then
      print ("contributed: month_to_season: bad season: SEASON="+SEASON)
      exit
  end if

  dimx    = dimsizes(xMon)
  rank    = dimsizes(dimx)
  if (rank.eq.2 .or. rank.ge.5) then
      print ("contributed: month_to_season: rank="+rank)
      print ("----- rank currently not handled -----")
  end if

  nmos    = 12
  ntim    = dimx(0)
  modCheck ("month_to_season", ntim, nmos)

  if (rank.ge.3) then
      nlat    = dimx(rank-2)
      mlon    = dimx(rank-1)
  end if
  nyrs    = ntim/nmos
  con     = 1./3.

  nyrStrt = 0
  nyrLast = nyrs-1
  if (NMO.eq.0) then
      nyrStrt = 1
  end if
  if (NMO.eq.nmos-1) then
      nyrLast = nyrs-2
  end if

  if (rank.eq.1) then
      xSea = new ( nyrs, typeof(xMon), getFillValue(xMon))
      do nyr=nyrStrt,nyrLast
         n = nyr*nmos + NMO
         xSea(nyr) = (xMon(n-1) + xMon(n) + xMon(n+1))*con
      end do
                                        ; special for beginning/end points
     if (NMO.eq.0) then
         n = 0
         xSea(0) = (xMon(n) + xMon(n+1))*0.5
     end if
     if (NMO.eq.nmos-1) then
         n = (nyrs-1)*nmos + NMO
         xSea(nyrs-1) = (xMon(n) + xMon(n-1))*0.5
     end if

  end if

  if (rank.eq.3) then
      xSea = new ( (/nyrs,nlat,mlon/), typeof(xMon), getFillValue(xMon))
      do nyr=nyrStrt,nyrLast
         n = nyr*nmos + NMO
         xSea(nyr,:,:) = (xMon(n-1,:,:) + xMon(n,:,:) + xMon(n+1,:,:))*con
      end do
                                        ; special for beginning/end points
     if (NMO.eq.0) then
         n = 0
         xSea(0,:,:) = (xMon(n,:,:) + xMon(n+1,:,:))*0.5
     end if
     if (NMO.eq.nmos-1) then
         n = (nyrs-1)*nmos + NMO
         xSea(nyrs-1,:,:) = (xMon(n,:,:) + xMon(n-1,:,:))*0.5
     end if

  end if

  if (rank.eq.4) then
      klev = dimx(1)
      xSea = new ( (/nyrs,klev,nlat,mlon/), typeof(xMon), getFillValue(xMon))
      do nyr=nyrStrt,nyrLast
         n = nyr*nmos + NMO
         xSea(nyr,:,:,:) = (xMon(n-1,:,:,:) + xMon( n ,:,:,:) \
                                            + xMon(n+1,:,:,:))*0.33333
      end do
   
     if (NMO.eq.0) then
         n = 0
         xSea(0,:,:,:) = (xMon(n,:,:,:) + xMon(n+1,:,:,:))*0.5
     end if
     if (NMO.eq.nmos-1) then
         n = (nyrs-1)*nmos + NMO
         xSea(nyrs-1,:,:,:) = (xMon(n,:,:,:) + xMon(n-1,:,:,:))*0.5
     end if
  end if

  copy_VarAtts (xMon, xSea)
  if (isatt(xMon,"long_name") .or. isatt(xMon,"description") .or. \
      isatt(xMon,"standard_name") ) then
      xSea@long_name = SEASON+": "+getLongName(xMon)
  end if

  do n=1,rank-1                  ; copy spatial coordinates
     if (.not.ismissing(xMon!n)) then
         xSea!n = xMon!n
        if(iscoord(xMon,xMon!n))
	   xSea&$xSea!n$ = xMon&$xMon!n$
        end if
     end if
  end  do

 ;n = 0                         ; special coordinate for time
 ;xSea!n = "year"
 ;if (iscoord(xMon,xMon!n))
 ;    xSea&$xSea!n$ = xMon&$xMon!n$(NMO:ntim-1:nmos)
 ;end if

  dName        = xMon!0
  xSea!0       = dName

  if(iscoord(xMon,dName)) then
      cv = xMon&$dName$(NMO:ntim-1:nmos) 
                                     ; possibly override
     ;if (isatt(cv,"units") .and. \
     ;         (cv@units.eq."YYYYMM" .or. cv@units.eq."YYMM")) then
     ;    cv = cv/100
     ;    cv@units = "YYYY"
     ;end if
     ;if (isatt(cv,"units") .and. cv@units.eq."YYYYMMDD") then
     ;    cv = cv/10000
     ;    cv@units = "YYYY"
     ;end if

      xSea&$dName$ = cv
  end if

  xSea@NMO = NMO   ; for possible use in subscripting 
                   ; eg: nStrt= xSea@NMO ; time(nStrt:ntim-1,12)
  return (xSea)
  end
; ------------------------
 
; D Shea
; Compute 12 seasonal (3-mo) averages using monthly data
;
; DJF,JFM,FMA,MAM,AMJ,MJJ,JJA,JAS,ASO,SON,OND,NDJ
;  0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10, 11 

; first (DJF=JF) /last (NDJ=ND) seasons are 2-month averages
;
; x(time),   x(time,lat,lon),  x(time,lev,lat,lon)
;              ^^^^^^^^^^^^      ^^^^^^^^^^^^^^^^   
; must have named dim BUT can be ANY names
;
; The input "x" are assumed to contain monthly mean data
; The size of "time" MUST be divisible by 12

; usage:   pSea = mon_to_season12 (pMon) 
; result   pSea(time,lat,lon) or pSea(time,lev,lat,lon) 

undef ("month_to_season12")
function month_to_season12 (xMon:numeric)
local season,dimx,rank,ntim,nlat,mlon,nmos,nyrs,dNam,i, xSea
begin
  season =  (/"DJF","JFM","FMA","MAM","AMJ","MJJ" \
             ,"JJA","JAS","ASO","SON","OND","NDJ" /)

  dimx   = dimsizes(xMon)
  rank   = dimsizes(dimx)
  if (rank.eq.2 .or. rank.ge.5) then
      print ("contributed: month_to_season12: rank="+rank)
      print ("----- rank currently not handled -----")
  end if

  nmos   = 12
  ntim   = dimx(0)
  modCheck ("month_to_season12", ntim, nmos)
  nyrs   = ntim/nmos

  if (rank.ge.3) then
      nlat   = dimx(rank-2)
      mlon   = dimx(rank-1)
  end if

  dNam = new ( rank, "string")    ; save input dim names
  do i=0,rank-1
     if (.not.ismissing(xMon!i)) then
         dNam(i) = xMon!i
     else
         print("mon_to_season12: All dimensions must be named")
         print("                 dimension "+i+" is missing"  )
         exit
     end if
  end  do

  if (rank.eq.1) then                ; (time)

      xSea = xMon              ; transfer meta and reorder
      xSea = runave (xSea ,3, 0 )              ; overwrite with seasonal means
      xSea(0)      = (xMon(0) + xMon(1) )*0.5
      xSea(ntim-1) = (xMon(ntim-2) + xMon(ntim-1) )*0.5

      xSea@long_name = "seasonal means: "+getLongName(xMon)
      xSea@season    = season 

      return (xSea)
  end if

  if (rank.eq.3) then                ; (time,lat,lon)

     ;xSea = xMon(lat|:,lon|:,time|:)   ; transfer meta and reorder
      xSea = xMon($dNam(1)$|:,$dNam(2)$|:,$dNam(0)$|:) ; transfer meta and reorder
      xSea = runave (xSea ,3, 0 )              ; overwrite with seasonal means
      xSea(:,:,0)      = (xMon(0,:,:) + xMon(1,:,:) )*0.5
      xSea(:,:,ntim-1) = (xMon(ntim-2,:,:) + xMon(ntim-1,:,:) )*0.5

      xSea@long_name = "seasonal means: "+getLongName(xMon)
      xSea@season    = season 

      return (xSea($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:))  ; reorder and return
     ;return (xSea(time|:,lat|:,lon|:))
  end if

  if (rank.eq.4) then                ; (time,lev,lat,lon)

     ;xSea = xMon(lev|:,lat|:,lon|:,time|:)    ; transfer meta and reorder
      xSea = xMon($dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:,$dNam(0)$|:) 
      xSea = runave (xSea ,3, 0 )              ; overwrite with seasonal means
      xSea(:,:,:,0)      = (xMon(0,:,:,:) + xMon(1,:,:,:) )*0.5
      xSea(:,:,:,ntim-1) = (xMon(ntim-2,:,:,:) + xMon(ntim-1,:,:,:) )*0.5

      xSea@long_name = "seasonal means: "+getLongName(xMon)
      xSea@season    = season

      return (xSea($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:)) 
     ;return (xSea(time|:,lev|:,lat|:,lon|:)) 
  end if
  
end
; -------------------------------------------------------
undef ("month_to_seasonN")
function month_to_seasonN (xMon:numeric, SEASON[*]:string)
 
; D Shea

; Compute the seasonal (3-mo) average for user specified seasons.
; These are the conventional seasons:
; DJF,JFM,FMA,MAM,AMJ,MJJ,JJA,JAS,ASO,SON,OND,NDJ
;
; xMon(time) or  xMon(time,lat,lon)    or   xMon(time,lev,lat,lon)
;
; The input "x" are assumed to contain monthly mean data
; The size of "time" MUST be divisible by 12.
; Also, it is assumed the "Jan" is the 1st month.
;
; first DJF season is a 2-month average (DJF=JF)
;
; USAGE:   xSea = month_to_seasonN (xMon, (/"DJF","MAM","JJA","SON"/))
;      :   xSea = month_to_seasonN (xMon, (/"DJF","JJA"/))
;      :   xSea = month_to_seasonN (xMon, (/"JJA","ASO","OND"/))
;
; RESULT   xSea(N,time/12,lat,lon) or xSea(N,time/12,lev,lat,lon)  
;          where N=dimsizes(SEASON)

;          The above would return:
;              xSea(4,time/12,lat,lon) or xSea(4,time/12,lev,lat,lon)
;              xSea(2,time/12,lat,lon) or xSea(2,time/12,lev,lat,lon)
;              xSea(3,time/12,lat,lon) or xSea(3,time/12,lev,lat,lon)
;
; NOTE: the "time" dimension may have to be altered to the user's desires.
;       it may correspond to those associated with the 1st month.
local season, N, n, dimx, rank, nmos, ntim, nyrs, xSea12, nlat, mlon \
    , NMO, NMO1, xSeaN, ns, dName, cv  
begin
  season  =  (/"DJF","JFM","FMA","MAM","AMJ","MJJ" \
              ,"JJA","JAS","ASO","SON","OND","NDJ" /)
  N      = dimsizes(SEASON)
  
  do n=0,N-1
     if (.not.any(season.eq.SEASON(n))) then
         print ("month_to_seasonN: You have at least one spelling error "+\
                "in your SEASON specification. "+SEASON(n)+" is not valid.")
         exit
     end if
  end do
                                     ; now subset the data
  dimx   = dimsizes(xMon)
  rank   = dimsizes(dimx)
  if (rank.eq.2 .or. rank.ge.5) then
      print ("contributed: month_to_seasonN: rank="+rank)
      print ("----- rank currently not handled -----")
  end if

  nmos   = 12
  ntim   = dimx(0)    
  modCheck ("month_to_seasonN", ntim, nmos)

  nyrs   = ntim/nmos

  xSea12 = month_to_season12 (xMon)  ; compute the 12 seasons

  if (rank.ge.3) then
      nlat   = dimx(rank-2)
      mlon   = dimx(rank-1)
  end if

  NMO1   = ind(SEASON(0).eq.season)
                                     ; error checking done
  if (rank.eq.1) then                ; (time,lat,lon)
      xSeaN = new ( (/N,nyrs/),  typeof(xSea12), getFillValue(xMon))
      do ns =0,N-1
         NMO= ind(SEASON(ns).eq.season)
         if (.not.ismissing(NMO)) then
             xSeaN(ns,:) = (/ xSea12(NMO:ntim-1:nmos) /)
         end if
      end do
  end if

  if (rank.eq.3) then                ; (time,lat,lon)
      xSeaN = new ( (/N,nyrs,dimx(1),dimx(2)/),  typeof(xSea12), \
                   getFillValue(xMon))
      do ns =0,N-1
         NMO= ind(SEASON(ns).eq.season)
         if (.not.ismissing(NMO)) then
             xSeaN(ns,:,:,:) = (/ xSea12(NMO:ntim-1:nmos,:,:) /)
         end if
      end do
  end if
               
  if (rank.eq.4) then                ; (time,lev,lat,lon)
      xSeaN = new ( (/N,nyrs,dimx(1),dimx(2),dimx(3)/),  typeof(xSea12), \
                   getFillValue(xMon))
      do ns =0,N-1
         NMO= ind(SEASON(ns).eq.season)
         if (.not.ismissing(NMO)) then
             xSeaN(ns,:,:,:,:) = (/ xSea12(NMO:ntim-1:nmos,:,:,:) /)
         end if
      end do
  end if
                                     ; copy attributes
  copy_VarAtts (xMon, xSeaN)
  if (isatt(xMon,"long_name") .or. isatt(xMon,"description") .or. \
      isatt(xMon,"standard_name") ) then
      xSeaN@long_name = "Seasonal Means: "+getLongName(xMon)
  end if
                                     ; copy dimension stuff
  xSeaN!0      = "season"
  xSeaN&season =  SEASON

  dName        = xSea12!0
  xSeaN!1      = dName

  if(iscoord(xSea12,dName)) then
      cv = xSea12&$dName$(NMO1:ntim-1:nmos) 
      xSeaN&$dName$ = cv
                                     ; possibly override
      if (isatt(cv,"units") .and. \
               (cv@units.eq."YYYYMM" .or. cv@units.eq."YYMM")) then
          cv = cv/100
          cv@units = "YYYY"
          xSeaN&$dName$ = cv
      end if
      if (isatt(cv,"units") .and. cv@units.eq."YYYYMMDD") then
          cv = cv/10000
          cv@units = "YYYY"
          xSeaN&$dName$ = cv
      end if
  end if

  if (rank.gt.1) then
      do i=1,rank-1                 ; copy spatial coords 
         dName       = xSea12!i
         xSeaN!(i+1) = dName
         if(iscoord(xSea12,dName)) then
            xSeaN&$dName$ = xSea12&$dName$
         end if
      end  do
  end if

  return (xSeaN)
end

; -------------------------------------------------------
undef ("wave_number_spc")
function wave_number_spc (x:numeric, gridType:string)

; D Shea

; Compute the total power spectrum as a function of wave number
; Currently "x" must have at least two dimensions and 
; may have up to four dimensions. The rightmost dimensions
; must be (lat,lon). 
;
; x   -  a numeric array of two to four dimensions (rightmost [lat,lon) ]
; gridType - "g" or "G" for x being a gaussian grid
;          - "f" or "F" for x being a regular (fixed) grid
;
; Usage:     power = wave_number_spc (T, "G") ; T is gaussian grid
;            power = wave_number_spc (X, "F") ; X is fixed grid
local dimx, rank, ab, spc, waveNumber 
begin
  dimx = dimsizes(x)       ; dimension sizes
  rank = dimsizes(dimx)    ; # of dimensions

  if (rank.lt.2 .or. rank.gt.4) then
      print ("wave_number_spc: rank="+rank+" error")
      exit
  end if
                           ; perform analysis
  if (gridType.eq."G" .or. gridType.eq."g") then 
      ab   = shagC (x)
  end if
  if (gridType.eq."F" .or. gridType.eq."f") then 
      ab   = shaeC (x)
  end if
                           ; compute power spectra
  if (rank.eq.2) then   
      spc = x(:,0)         ; spc is 1D
      spc = dim_sum( ab(0,:,:)^2 + ab(1,:,:)^2 )*0.5
  end if

  if (rank.eq.3) then
      spc = x(:,:,0)       ; spc is 2D
      spc = dim_sum( ab(0,:,:,:)^2 + ab(1,:,:,:)^2 )*0.5
  end if

  if (rank.eq.4) then
      spc = x(:,:,:,0)     ; spc is 3D
      spc = dim_sum( ab(0,:,:,:,:)^2 + ab(1,:,:,:,:)^2 )*0.5
  end if

  waveNumber   = ispan(1,dimx(rank-2),1)
  waveNumber!0 = "wave_number"
  waveNumber@long_name = "Wave Number"
  
  spc!(rank-2)       = waveNumber!0 
  spc&$waveNumber!0$ = waveNumber
  spc@long_name      = "Power"
  if (isatt(x,"units")) then
      spc@units = "(" + x@units + ")^2"
  end if

  return (spc)
end
;*************************************************************************
; D. Shea
; set all values X +/- eps of zero to 0
; X can be ANY dimensions
; note: "abs" is NOT generic like in fortran
;
; Usage: let X   = (/ 1,-2, -999., 1.e-05, -1.e-07, 10/)
;            X@_FillValue = -999.
;            eps = 1.e-04
;            epsZero (X, eps)
;     result X   = (/ 1,-2, -999., 0., 0., 10/)

undef ("epsZero")
procedure epsZero (X:numeric, eps:numeric)
local FillValue
begin
  if (isatt(X,"_FillValue")) then
      FillValue = X@_FillValue        ; save the _FillValue
      delete (X@_FillValue)           ; delete original 
  end if

  X@_FillValue = 0.0                  ; temporary [trick]

  if (typeof(X).eq."float" .or. typeof(X).eq."double") then
      X = mask(X,fabs(X).le.eps,False)
  else
      X = mask(X, abs(X).le.eps,False)
  end if

  delete (X@_FillValue)               ; delete temporary
  if (isvar("FillValue")) then
      X@_FillValue = FillValue        ; restore original _FillValue
  end if

end 

; M Haley
; you pass it your multi-dimensioned data array, and
; it will return the indices in that array where the 
; first maximum value occurs.  

; ****> superceded by NCL built-in function "ind_resolve"

undef ("maxind_ind")
function maxind_ind(data:numeric)
local i, dsizes, ndims, max_index, ind_product, data_indices, new_val
begin
  dsizes = dimsizes(data)      ; dimension sizes
  ndims  = dimsizes(dsizes)    ; number of dimensions
  if(ndims.eq.1)
    return(maxind(data))
  end if

  max_index = maxind(ndtooned(data))    ; Get index of first maximum value.

  if(ismissing(max_index))
    return(max_index)
  end if

  ind_product = new(ndims,integer)     ; This array is used to
  ind_product(ndims-1) = 1             ; calculate indices.

  do i = ndims-2,0,1
    ind_product(i) = dsizes(i+1) * ind_product(i+1)
  end do

  data_indices = new(ndims,integer)    ; Will hold the return indices.

  new_val = max_index
  do i = 0,ndims-1
    data_indices(i) = new_val/ind_product(i)   ; Calculate index.
    new_val = new_val % ind_product(i)         ; "%" is the modulus operator.
  end do

  delete(ind_product)       ; Clean up.
  delete(max_index)
  delete(dsizes)

  return(data_indices)      ; Return the indices.
end

;*************************************************************************
; D. Shea

; Bring one or more files from the MSS to a local directory
; The options are those for msrcp. The MSS files can be from different dir.
; Usage:   mssFiles   = (/ "/SHEA/sample/dummy1", ... /) ; a 1D array of MSS names
;          target_dir = "/ptmp/shea/whatever/"           ; a local directory
;          msrcp_mss2local (mssFiles, target_dir, "-n" )
undef ("msrcp_mss2local")
procedure msrcp_mss2local (source:string,target_dir:string,opts:string)
local dims, cmd, mssList, n
begin
  dims = dimsizes(source)
  if (dims(0).eq.1) then
      cmd  = "msrcp "+opts+" 'mss:"+source+" "+target_dir
  else
      mssList = ""  
      do n=0,dims(0)-1
         mssList = mssList + " mss:"+source(n)  
      end do
      cmd  = "msrcp "+opts + mssList +" "+ target_dir
  end if

  print ("msrcp_mss2local: "+dims(0)+" files: tStart= "+systemfunc("date"))
 ;print (cmd)
  system(cmd)
  print ("msrcp_mss2local: "+dims(0)+" files: tEnd  = "+systemfunc("date"))
end 
;*************************************************************************
; D. Shea
; old ... undocumented function: use "yyyymm_to_yyyyfrac"
; given, say, yyyymm = (/ 197901 , 198407, 200112 /)
;       yrFrac = yyyymm2yyyyFrac (yyyymm)
;       yrFrac ==> [1979.0 , 1979.5 , 2001.917]
;
; yyyymm       - scalar or array of dates
; 
undef("yyyymm2yyyyFrac")
function yyyymm2yyyyFrac (yyyymm:integer)
local year, mon, yrFrac
begin
  year = yyyymm/100 
  mon  = yyyymm-year*100  
  yrFrac = year + (mon-1.)/12.

  yrFrac@long_name = "Time"
  yrFrac@units     = "YYYY + fractional portion of year"
  yrFrac@info      = "derived using: function yyyymm2yyyyFrac"
     
  return(yrFrac)
end
;************************************************************************* 
; D. Shea                                                                 
;                                                                        
; given, say, yyyymm = (/ 197901 , 198407, 200112 /)                    
;       yrFrac = yyyymm_to_yyyyfrac (yyyymm, 0.0)                      
;       yrFrac ==> [1979.0 , 1979.5 , 2001.917]                       
;                                                                    
; yyyymm       - scalar or array of dates                           
; mm_offset    - 0.0 or 0.5
;                                                                  
undef("yyyymm_to_yyyyfrac")
function yyyymm_to_yyyyfrac (yyyymm[*]:numeric, mmOffset[1]:numeric)
local YYYYMM, year, mm, xmos, one, yrFrac      
begin                                                                                               
  if (typeof(yyyymm).eq."double" .or. typeof(mmOffset).eq."double") then 
      xmos = 12.d0                         
      one  = 1.0d0                        
      YYYYMM = toint(yyyymm)       ; numerical issue
  else                                   
      xmos = 12.0                       
      one  = 1.0                       
      if (typeof(yyyymm).eq."float") then
          YYYYMM = toint(yyyymm)    ; numerical issue
      else
          YYYYMM = yyyymm
      end if
  end if                              
                                     
  year   = YYYYMM/100               
  mm     = YYYYMM-year*100         
  yrFrac = year + ((mm+mmOffset)-one)/xmos  
                                           
  copy_VarMeta(yyyymm, yrFrac)            
                                         
  yrFrac@units     = "YYYY + fractional portion of year"  
  yrFrac@NCL       = "derived using function yyyymm_to_yyyyfrac"
                                                               
  return(yrFrac)                                              
end                                                          
;*************************************************************************
; D. Shea
; given, say, 19790719  where 0719 (mmdd)  is the 200th day of the year
;       yrFrac = yyyymmdd2yyyyFrac (19790719)
;       yrFrac = 1979.545     ( [200-1]/365.= 0.545..)
;
; yyyymmdd       - scalar or array of dates
; 
; 6.2.0: calendar attribute
;
undef ("yyyymmdd2yyyyFrac") 
function yyyymmdd2yyyyFrac (yyyymmdd:numeric, ddOffset[1]:numeric)
local ntim, year, mmdd, mon, day, dayYear, nDay, yrFrac, n, con, varType, YYYYMMDD
begin
     ntim = dimsizes(yyyymmdd)

     if (isatt(yyyymmdd,"calendar") .and. yyyymmdd@calendar.eq."proleptic_gregorian") then  
         print("yyyymmdd_to_yyyyfrac: yyyymmdd2yyyyFrac:  proleptic_gregorian calendar not supported")
         yrFrac = new( ntim, typeof(yyyymmdd))
         yrFrac@long_name = "yyyymmdd_to_yyyyfrac: yyyymmdd2yyyyFrac:  proleptic_gregorian calendar not supported"
         return(yrFrac)
     end if

     varType = typeof(yyyymmdd)

     if (varType.eq."integer") then
         YYYYMMDD = yyyymmdd
     else
         YYYYMMDD = toint(yyyymmdd)
     end if

     year = YYYYMMDD/10000
     mmdd = YYYYMMDD-year*10000
     delete(YYYYMMDD)

     mon  = mmdd/100
     day  = mmdd-mon*100
     delete (mmdd)

     if (isatt(yyyymmdd,"calendar")) then   ; retrofit code for calendar
         year@calendar = yyyymmdd@calendar  ; check for calendar
     end if
     
     dayYear = day_of_year(year, mon, day)  
     delete ( [/mon, day /] )
     
     nDay   = dimsizes(yyyymmdd)            ; all days 
     if (varType.eq."double") then
         yrFrac = new( nDay, "double")
         one    = 1d0
     else
         yrFrac = new( nDay, "float")
         one    = 1.0
     end if
     delete(yrFrac@_FillValue)

     if (isatt(yyyymmdd,"calendar")) then    ; retrofit to existing code
         yrFrac@calendar = yyyymmdd@calendar

         if (yyyymmdd@calendar.eq."360_day" .or. yyyymmdd@calendar.eq."360") then
             con = (/ 360, 360 /)*one
         end if
         if (yyyymmdd@calendar.eq."365_day" .or. yyyymmdd@calendar.eq."365"    .or. \
             yyyymmdd@calendar.eq."noleap"  .or. yyyymmdd@calendar.eq."no_leap") then
             con = (/ 365, 365 /)*one
         end if
         if (yyyymmdd@calendar.eq."366_day" .or. yyyymmdd@calendar.eq."366"    .or. \
             yyyymmdd@calendar.eq."allleap" .or. yyyymmdd@calendar.eq."all_leap") then
             con = (/ 366, 366 /)*one
         end if
         if (yyyymmdd@calendar.eq."gregorian" .or. yyyymmdd@calendar.eq."standard") then
             con = (/ 365, 366 /)*one 
         end if
     else
         con = (/ 365, 366 /)*one             ; default is gregorian/standard 
     end if
        
     do n=0,nDay-1
        if (isleapyear(year(n))) then
            yrFrac(n) = year(n) + ((dayYear(n)-1)+ddOffset)/con(1)
        else
            yrFrac(n) = year(n) + ((dayYear(n)-1)+ddOffset)/con(0)
        end if   
     end do
     
     yrFrac@long_name = "Time"
     yrFrac@units     = "YYYY + fractional portion of year"
     yrFrac@info      = "derived using: function yyyymmdd_to_yyyyFrac"
     
     return(yrFrac)
end
;*************************************************************************
; D. Shea
;      a function that invokes yyyymmdd2yyyyFrac
;      name consistent with yyyymm_to_yyyyfrac
undef("yyyymmdd_to_yyyyfrac")
function yyyymmdd_to_yyyyfrac (yyyymmdd:numeric, ddOffset:numeric)
local varType
begin
  varType = typeof(yyyymmdd)
  if (varType.eq."integer" .or. \
      varType.eq."float"   .or. \
      varType.eq."double"       ) then
      return (yyyymmdd2yyyyFrac (yyyymmdd, ddOffset))
  else
      print("yyyymmdd_to_yyyyfrac: variable type="+varType+" not supported")
      exit
  end if
end

;*************************************************************************
; D. Shea
;
; given, say, 1979071906  where 0719 is the 200th day of the year
;       yrFrac = yyyymmddhh2yyyyFrac (1979071906)
;       yrFrac = 1979.5458984375    ([200-1]*86400.+hh*3600.)/(86400*365)
;
; yyyymmddhh     - scalar or array of dates
;
; assumes Gregorian calendar
 
undef ("yyyymmddhh2yyyyFrac")
function yyyymmddhh2yyyyFrac (yyyymmddhh[*]:numeric)
local year, mmddhh, mon, ddhh, day, hour, dayYear, nTim, yrFrac \
    , n, ysec, dsec, varType, YEAR
begin
     varType = typeof(yyyymmddhh)
 
     if (varType.eq."integer") then
         YYYYMMDDHH = yyyymmddhh
     else
         YYYYMMDDHH = toint(yyyymmddhh)
     end if

     year   = YYYYMMDDHH/1000000
     mmddhh = YYYYMMDDHH-year*1000000
     delete(YYYYMMDDHH)

     mon  = mmddhh/10000
     ddhh = mmddhh-mon*10000
     delete (mmddhh)

     day  = ddhh/100
     hour = ddhh-day*100
     delete (ddhh)

     if (isatt(yyyymmddhh,"calendar")) then
         year@calendar = yyyymmddhh@calendar  ; check for calendar
     end if

     dayYear = day_of_year(year, mon, day)    ; Gregorian calendar
     delete ([/mon, day/])

     nTim   = dimsizes(yyyymmddhh)

     if (varType.eq."double") then
         yrFrac = new( nTim, "double", "No_FillValue")
         one    = 1d0
     else
         yrFrac = new( nTim, "float", "No_FillValue")
         one    = 1.0
     end if

     if (isatt(yyyymmddhh,"calendar")) then    ; retrofit to existing code
         yrFrac@calendar = yyyymmddhh@calendar

         if (yyyymmddhh@calendar.eq."360_day" .or. yyyymmddhh@calendar.eq."360") then
             con = (/ 360, 360, 86400, 3600 /)*one
         end if
         if (yyyymmddhh@calendar.eq."365_day" .or. yyyymmddhh@calendar.eq."365"    .or. \
             yyyymmddhh@calendar.eq."noleap"  .or. yyyymmddhh@calendar.eq."no_leap") then
             con = (/ 365, 365, 86400, 3600 /)*one
         end if
         if (yyyymmddhh@calendar.eq."366_day" .or. yyyymmddhh@calendar.eq."366"    .or. \
             yyyymmddhh@calendar.eq."allleap" .or. yyyymmddhh@calendar.eq."all_leap") then
             con = (/ 366, 366, 86400, 3600 /)*one
         end if
         if (yyyymmddhh@calendar.eq."gregorian" .or. yyyymmddhh@calendar.eq."standard") then
             con = (/ 365, 366, 86400, 3600 /)*one 
         end if
     else
         con = (/ 365, 366, 86400, 3600 /)*one   ; default is gregorian/standard
     end if

     do n=0,nTim-1
        if (isleapyear(year(n))) then
            ysec = con(2)*con(1)
        else
            ysec = con(2)*con(0)
         end if
         dsec = (dayYear(n)-1)*con(2)  + hour(n)*con(3)
         yrFrac(n) = year(n) + dsec/ysec
     end do

     yrFrac@long_name = "Time"
     yrFrac@units     = "YYYY + fractional portion of year"
     yrFrac@NCL       = "contributed.ncl: function yyyymmddhh_to_yyyyFrac"

     return(yrFrac)
end
;*************************************************************************
; D. Shea
;      a function that invokes yyyymmddhh2yyyyFrac
;      name consistent with yyyymm_to_yyyyfrac
;      name consistent with yyyymmdd_to_yyyyfrac
;
;      ignore hhOffset
;
undef("yyyymmddhh_to_yyyyfrac")
function yyyymmddhh_to_yyyyfrac (yyyymmddhh:numeric, hhOffset[1]:numeric)
local varType
begin
  varType = typeof(yyyymmddhh)

  if (varType.eq."integer" .or. \
      varType.eq."float"   .or. \
      varType.eq."double"       ) then
      return (yyyymmddhh2yyyyFrac (yyyymmddhh))
  else
      print("yyyymmddhh_to_yyyyfrac: variable type="+varType+" not supported")
      exit
  end if
end
;*************************************************************************
; D. Shea
;      convert initial_time (type string) to integer time
;
;      01/01/1997 (18:00)  <==> MM/DD/YYYY (HH:NN) {ignore NN}  
;      return will be integer of form YYYYMMDDHH
;
undef ("grib_stime2itime")
function grib_stime2itime(stime[*]:string)
local N, time, i, tmp_c
begin

        N      = dimsizes(stime)
        time   = new( N ,integer)
        delete(time@_FillValue)

        do i=0,N-1
             tmp_c   = stringtochar(stime(i))
             time(i) = stringtointeger((/tmp_c(6:9)/)) * 1000000 + \
                       stringtointeger((/tmp_c(0:1)/)) * 10000 + \
                       stringtointeger((/tmp_c(3:4)/)) * 100 + \
                       stringtointeger((/tmp_c(12:13)/))
        end do

        time!0 = "time"
        time@long_name  = "time"
        time@units      = "yyyymmddhh"

        time&time       =  time        ; make coordinate variable
        return (time)
end 
;*************************************************************************
; D. Shea
;      convert initial_time (type string) to COARDS time
;
;      01/01/1997 (18:00)  <==> MM/DD/YYYY (HH:NN) {ignore NN}  
;      return will be "double" 
; Usage:
;   sit0 = a->initial_time0
;   time = grib_stime2COARDStime(sit0, "days since 1801-01-01 00:00:0.0")
;                                      ^^^^ can be anything^^^^^^^^^^^^
;
undef ("grib_stime2COARDStime")
function grib_stime2COARDStime(stime[*]:string, tunits:string)
local N, time, tmp_c, year, month, day, hour, min, sec
begin

  N      = dimsizes(stime)
  time   = new( N ,"double")
  time@units = tunits
  time!0 = "time"
  time@long_name  = "time"

  tmp_c   = stringtochar(stime)
  year    = stringtointeger((/tmp_c(:,6:9)/))
  month   = stringtointeger((/tmp_c(:,0:1)/)) 
  day     = stringtointeger((/tmp_c(:,3:4)/))
  hour    = stringtointeger((/tmp_c(:,12:13)/))
  min     = stringtointeger((/tmp_c(:,15:16)/))
  sec     = new( N ,"float")
  sec     = 0.0

  time    = (/ cd_inv_calendar(year,month,day,hour,min,sec,tunits, 0) /)

  time&time       =  time        ; make coordinate variable

  if (isatt(time,"_FillValue")) then
      delete(time@_FillValue)
  end if
  return (time)
end 

;*************************************************************************
; D. Shea
; Concatenate 2 (or more) variables to create one variable
; All dimensions must match except for the leftmost (record) dimension
;
; Usage:   let xa(22,73,144) and xb(38,73,144)
;          X = cat2Var(xa,xb)   ; X(60,73,144)    
; Note: More than two variables can be concatenated by embedding the function
;          let xa(22,73,144), xb(38,73,144), xc(5,73,144), xd(25,73,144)
;          Xabc  = cat2Var( cat2Var(xa,xb), xc)  ; X(65,73,144)
;          Xabcd = cat2Var (cat2Var( cat2Var(xa,xb), xc), xd )  ; X(90,73,144)
;
undef ("cat2Var")
function cat2Var (xa, xb)
local dim_xa, dim_xb, rank_xa, rank_xb, rank, nr_xa, nr_xb, dim_X \
    , X, nab, i, ca, cb, cv
begin
  dim_xa = dimsizes(xa)
  dim_xb = dimsizes(xb)

  rank_xa = dimsizes(dim_xa)
  rank_xb = dimsizes(dim_xb)

  if (rank_xa.ne.rank_xb) then
      print ("contributed.ncl: cat2Var: rank mismatch")
      exit
  end if

  rank  = rank_xa                    ; all rank the same

  if (rank.gt.5) then
      print ("contributed.ncl: cat2Var: rank="+rank+" too big: change function")
      exit
  end if
 
  if (rank.gt.1) then
      if (.not.all(dim_xa(1:).eq.dim_xb(1:))) then
          print ("contributed.ncl: cat2Var:  non-record dim(s) must match")
          exit
      end if
  end if

  if (typeof(xa).ne.typeof(xb)) then
      print ("contributed.ncl: cat2Var: types must match (could be worked around) ")
      exit
  end if

  nr_xa = dim_xa(0)                  ; # of records in xa
  nr_xb = dim_xb(0)                  ;                 xb

  dim_X = dim_xa
  dim_X(0) = nr_xa + nr_xb

  X     = new (dim_X, typeof(xa), getFillValue(xa) ) 

  nab   = nr_xa+nr_xb-1              ; last subscript                  

  if (rank.eq.1) then
      X(0:nr_xa-1) = (/ xa /)
      X(nr_xa:nab) = (/ xb /)
  end if
  if (rank.eq.2) then
      X(0:nr_xa-1,:) = (/ xa /)
      X(nr_xa:nab,:) = (/ xb /)
  end if
  if (rank.eq.3) then
      X(0:nr_xa-1,:,:) = (/ xa /)
      X(nr_xa:nab,:,:) = (/ xb /)
  end if
  if (rank.eq.4) then
      X(0:nr_xa-1,:,:,:) = (/ xa /)
      X(nr_xa:nab,:,:,:) = (/ xb /)
  end if
  if (rank.eq.5) then
      X(0:nr_xa-1,:,:,:,:) = (/ xa /)
      X(nr_xa:nab,:,:,:,:) = (/ xb /)
  end if

  copy_VarAtts (xa, X)              ; contributed.ncl

  if (rank.gt.1) then
      do i=1,rank-1   ; copy all coords but rec dim
         if (.not.ismissing(xa!i)) then
             X!i = xa!i                 ; copy named dimension
             if(iscoord(xa,xa!i)) then  ; is there a coord var
                X&$X!i$ = xa&$xa!i$     ; copy coord variable
             end if
         end if
      end  do
  end if
 
  if (.not.ismissing(xa!0)) then
      X!0 = xa!0             ; copy named dimension
      if(iscoord(xa,xa!0) .and. iscoord(xb,xb!0)) then  ; is there a coord var
         ca = xa&$xa!0$
         cb = xb&$xb!0$
         cv = new ( dim_X(0), typeof(ca) )
         delete (cv@_FillValue)
         cv(0:nr_xa-1) = (/ ca /)
         cv(nr_xa:nab) = (/ cb /)
         X&$X!0$ = cv
      end if
  end if

  return (X)
end
;*************************************************************************
; D. Shea
; Basically, this takes a 2D {homo/hetero}geneous array
; and partitions it into a 3D array with lat/lon coordinate
; arrays. Attributes are also added.
;
; usage: 
; homlft = new((/nsvd,ncols/),typeof(x))
; hetlft = new((/nsvd,ncols/),typeof(x))
; homrgt = new((/nsvd,ncols/),typeof(x))
; hetrgt = new((/nsvd,ncols/),typeof(x))
; pcvar  = svdstd(x,y,nsvd,homlft,hetlft,homrgt,hetrgt)
;
; HOMLFT = svdHomHet2latlon(homlft,lat,lon,"homogeneous left"    ,"")
; HOMRGT = svdHomHet2latlon(homrgt,lat,lon,"homogeneous right"   ,"")
; HETLFT = svdHomHet2latlon(hetlft,lat,lon,"heterogeneous left"  ,"")
; HETRGT = svdHomHet2latlon(hetrgt,lat,lon,"heterogeneous right" ,"")
;
undef ("svdHomHet2latlon")
function svdHomHet2latlon (x[*][*]:numeric                \
                          ,lat[*]:numeric, lon[*]:numeric \
                          ,long_name:string, units:string )
local nlat, mlon, dimx, nsvd, ncols, X
begin
  nlat  = dimsizes(lat)
  mlon  = dimsizes(lon)

  dimx  = dimsizes(x)
  nsvd  = dimx(0)
  ncols = dimx(1)

  if ((nlat*mlon).ne.ncols) then
      print ("contributed: svdHomHet2latlon: size mismatch: nlat="+nlat \
            +"  mlon="+mlon+"  ncols="+ncols \
            +"  nlat*mlon="+(nlat*mlon) ) 
      exit
  end if

  X     = onedtond( ndtooned(x), (/nsvd,nlat,mlon/) )
  X!0   = "svd"
  X!1   = "lat"
  X!2   = "lon"
  X&svd = ispan(1,nsvd,1)
  X&lat =  lat
  X&lon =  lon

  X@long_name = long_name
  X@units     = units

  return (X)
end

;*************************************************************************
; D. Shea
; Basically, this takes a 1D expansion coef
; and partitions it into a 2D array with (svd,time)
; arrays. Attributes are also added.
;
; usage: 
; pcvar  = svdstd(x,y,nsvd,homlft,hetlft,homrgt,hetrgt)
;        or
; pcvar  = svdcoc(x,y,nsvd,homlft,hetlft,homrgt,hetrgt)
;
; ak     = svdAkBk2time (pcvar@ak, nsvd, time, "Exp Coef AK","")
; bk     = svdAkBk2time (pcvar@bk, nsvd, time, "Exp Coef BK","")
;
undef ("svdAkBk2time")
function svdAkBk2time (xk[*]:numeric, nsvd:integer, time[*]:numeric \
                      ,long_name:string, units:string )
local ntim, nxk, XK
begin
  ntim  = dimsizes(time)
  nxk   = dimsizes(xk)

  if ((nsvd*ntim).ne.nxk) then
      print ("contributed: svdAkBk2time: size mismatch: nsvd="+nsvd \
            +"  ntim="+ntim+"  nxk="+nxk \
            +"  nsvd*ntim="+(nsvd*ntim) ) 
      exit
  end if

  XK      = onedtond ( xk, (/nsvd,ntim/) )
  XK!0    = "svd"
  XK!1    = "time"
  XK&svd  = ispan(1,nsvd,1)
  XK&time =  time

  XK@long_name = long_name
  XK@units     = units

  return (XK)
end

;*************************************************************************
; D. Shea
undef ("timeCoads2YYYYMM")
function timeCoads2YYYYMM (yrStrt:integer, yrLast:integer, TYPE:string)
;
; This was a *terrible* name for a function.
; It was never explicitly documented.
; It was used in some "COADS" application examples.
; The function "yyyymm_time" is documented.
; It is just a different interface to this function.
;
; usage
;    yrLast = 1997   ; last year on this COADS file
;    yyyymm = timeCoads2YYYYMM (1800,yrLast,typeof(time))
local nmos, nyrs, ntim, time, n
begin
     nmos = 12
     nyrs = yrLast-yrStrt+1
     ntim = nmos*nyrs

     time = new ( ntim, TYPE)
     n = 0
     do yr=yrStrt,yrLast
        time(n:n+nmos-1) = yr*100 + ispan(1,nmos,1)
        n = n+nmos
     end do

     time@long_name = "time"
     time@units     = "YYYYMM"

     if (isatt(time,"_FillValue")) then
         delete(time@_FillValue)
     end if
     time!0    = "time"
     time&time =  time

     return (time)
end
;*************************************************************************
; D. Shea
undef ("yyyymm_time")
function yyyymm_time (yrStrt:integer, yrLast:integer, TYPE:string)
;
; documented interface to "timeCoads2YYYYMM"
; It is just a different interface to this function.
;
; usage
;    yyyymm = yyyymm_time (1800,2001, "integer")
begin
     return( timeCoads2YYYYMM (yrStrt, yrLast, TYPE) )     
end
;*************************************************************************
; Nadine Salzmann and Simon Scherrer

undef ("yyyymmdd_time")

function yyyymmdd_time (yrStrt:integer, yrLast:integer, TYPE:string)
local n, nmos, nyrs, ntim, year, nmo, ndy, tdum
begin
  nmos = 12
  nyrs = yrLast-yrStrt+1
  ntim = nyrs*nmos*31 ; make long enough array
  tdum = new ( ntim, TYPE, "No_FillValue" )

  year = 0
  if (isatt(yrStrt,"calendar")) then
      year@calendar = yrStrt@calendar
  end if

  n = -1 ; initialize day counter
  do year = yrStrt,yrLast
    do nmo=1,nmos
       YRM  = year*10000 + nmo*100
       nday = days_in_month(year, nmo)
      do ndy = 1,nday
         n = n+1
         tdum(n) = YRM + ndy
      end do
    end do
  end do

  time      = tdum(0:n) ; cut out only filled in times
  time@long_name = "time"
  time@units     = "YYYYMMDD"

  if (isatt(yrStrt,"calendar")) then
      time@calendar  = yrStrt@calendar
  end if

  time!0    = "time"
  time&time =  time
  return (time)
end
;*************************************************************************

undef ("yyyymmddhh_time")
function yyyymmddhh_time (yrStrt[1]:integer, yrLast[1]:integer, hrStep[1]:integer, TYPE:string)
local n, nmos, nyrs, ntim, year, nmo, ndy, tdum, nhr, nhrs
begin
  nmos = 12
  nyrs = yrLast-yrStrt+1
  nhrd = 24/hrStep               ; # of hours per day
  ntim = nyrs*nmos*nhrd*31       ; make long enough array
  tdum = new ( ntim, TYPE, "No_FillValue" )

  year = 0
  if (isatt(yrStrt,"calendar")) then
      year@calendar = yrStrt@calendar   ; needed for days in month
  end if

  n = -1 ; initialize counter index
  do year=yrStrt,yrLast
    do nmo=1,nmos
       YRM  = year*1000000 + nmo*10000
       nday = days_in_month(year, nmo)
      do ndy=1,nday
        do nhr=0,23,hrStep
           n = n+1
           tdum(n) = YRM + ndy*100 + nhr
        end do
      end do
    end do
  end do

  time      = tdum(0:n) ; cut out only filled in times
  time@long_name = "time"
  time@units     = "YYYYMMDDHH"

  if (isatt(yrStrt,"calendar")) then
      time@calendar  = yrStrt@calendar
  end if

  time!0    = "time"
  time&time =  time
  return (time)
end
;************************************************************************* 
; D. Shea 
; Parse (Partition) YYYYMM, YYYYMMDD, YYYYMMDDHH into a (:,6) array

undef("date_expand")
function date_expand(date[*]:numeric, opt:integer)
local ndate, DATE, utest, yyyy, mm, dd, hh, mn, sc
begin
  ndate = dimsizes(date)
  DATE  = new( (/ndate,6/), "integer", "No_FillValue")

  if (.not.isatt(date,"units")) then
      print(" ")
      print("date_expand: no units attribute: no expansion")
      print(" ")
      return(DATE)
  end if

  utest = str_lower(date@units)
  if (.not.any(utest.eq.(/"yyyymm","yyyymmdd","yyyymmddhh"/))) then
      print(" ")
      print("date_expand: units not recognized: units="+date@units)
      print("recognized units: yyyymm,yyyymmdd,yyyymmddhh or YYYYMM,YYYYMMDD,YYYYMMDDHH")
      print(" ")
      return(DATE)
  end if

  mn   = conform(date, 0, -1)
  sc   = conform(date, 0, -1)

  if (utest.eq."yyyymm") then
      yyyy = date/100
      mm   = date-(yyyy*100)
      dd   = conform(date, 0, -1)
      hh   = conform(date, 0, -1)
  end if

  if (utest.eq."yyyymmdd") then
      yyyy = date/10000
      mmdd = date-(yyyy*10000)
      mm   = mmdd/100
      dd   = mmdd-(mm*100)           
      hh   = conform(date, 0, -1)
  end if

  if (utest.eq."yyyymmddhh") then
      yyyy   = date/1000000
      mmddhh = date-(yyyy*1000000)
      mm     = mmddhh/10000
      mmdd   = mmddhh/100
      dd     = mmdd-(mm*100)           
      hh     = date-(yyyy*1000000+mm*10000+dd*100)
  end if

  DATE(:,0) = yyyy
  DATE(:,1) =   mm
  DATE(:,2) =   dd
  DATE(:,3) =   hh
  DATE(:,4) =   mn
  DATE(:,5) =   sc
  DATE@long_name = "time elements"
  DATE@units     = "yyyy,mm,dd,hh,m,sc"

  dName     = getvardims(date)
  if (.not.ismissing(dName)) then
      DATE!0 = dName
    if (iscoord(date,dName)) then
        DATE&$dName$ = date&$dName$
    end if
  end if

  if (isatt(date,"calendar")) then
      DATE@calendar  = date@calendar
  end if

  return(DATE)
end

;************************************************************************* 
; D. Shea 
; Read simple f90 namelist file as indicated by Ben Foster [HAO]
; An integer variable is returned. Upon return it will have 
; associated with it attributes that were created from the
; input namelist file.
;
; Usage:
; 
;     hao_user = namelist("haoUserNameList")
;
;     In some cases, these new attributes are used to modify an
;     existing (say, default) suite of attributes (say, haoDefault).
;
;     hao = merge_VarAtts(hao_user, haoDefault) 
;
undef ("namelist")
function namelist (fname:string)
local lines, nam, chr, blankc, eqc, quot1c, quot2c, semic    \
    , decimalc, slashc, commac, ampc, nullc, newlc, nLines   \
    , nl, cc, CC, nc, NC, nEq, iEq, iSemic, aName, iq, i, n  \
    , iComma, nComma, nDecimal, rhs, RHS 
begin
  lines  = asciiread (fname, -1, "string")
  nam    = 1   ; create variable to which 
               ; attributes may be attached

               ; special characters to check 
               ; for clarity use stringtochar 
  chr    = stringtochar(" ")  ; dimsizes(chr)=2
  blankc = chr(0)             ; blank space     (int 32)

  chr    = stringtochar("=")  ; dimsizes(chr)=2
  eqc    = chr(0)             ; equal           (int 61)

  chr    = stringtochar(";")  ; dimsizes(chr)=2
  semic  = chr(0)             ; comment         (int 59)

  chr    = stringtochar("\")  ; dimsizes(chr)=2
  slashc = chr(0)             ; line continuation (int 92)

  chr    = stringtochar(",")  ; dimsizes(chr)=2
  commac = chr(0)             ; value separator (int 44)

  chr    = stringtochar(".")  ; dimsizes(chr)=2
  decimalc= chr(0)            ; indicate float  (int 46)

  chr    = stringtochar("&")  ; dimsizes(chr)=2
  ampc   = chr(0)             ; ampersand       (int 38)

  chr    = stringtochar("'")  ; dimsizes(chr)=2
  quot1c = chr(0)             ; single quote    (int 39)

  quot2c= integertochar(34)   ; double quote "  (int 34)
  newlc = integertochar(10)   ; new line character
  nullc = integertochar(0)    ; null

  nLines = dimsizes(lines)    ; # of lines (strings)

  do nl=0,nLines-1                 ; loop over each line
     cc  = stringtochar(lines(nl)) ; convert to characters
     nc  = dimsizes(cc)            ; # characters
     nc  = nc - 1                  ; ignore last character

     nEq = num(cc.eq.eqc)          ; number of = signs
                                   ; eliminate (a) HAO's  &proc
                                   ; (b) any line without an =
     if (cc(0).eq.ampc .or. nEq.eq.0) then     
         delete (cc)               ; delete cuz size changes
         continue                  ; go to next iteration
     end if

     iSemic = ind(cc.eq.semic)     ; is simicolon (;) present [comment] 
     if (.not.ismissing(iSemic)) then
         nc = iSemic(0)-1          ; only look up to semi-colon
         if (nc.le.1) then         ; need at least 3 char [eg: i=0]
             delete (cc)
             delete (iSemic)
             continue              ; go to next iteration
         end if
     end if
     delete (iSemic)

     NC = -1                       ; remove blanks
     CC = new ( nc, "character")   ; cc after blanks removed
   do n=0,nc-1
      if (cc(n).ne.blankc .and. cc(n).ne.nullc .and. cc(n).ne.newlc) then 
          NC     = NC+1
          CC(NC) = cc(n)
      end if
   end do
   delete (cc)                     ; no longer needed

   if (NC.le.1) then               ; again need min 3 char
       delete (CC)                 ; size might change
       continue                    ; go to next iteration
   end if

   iEq     = ind(CC.eq.eqc)        ; = is separator; return index

                                   ; name of attribute (lhs)
   aName    = chartostring( (/CC(0:iEq-1)/) )

   nComma   = num(CC.eq.commac)    ; a comma (,) means more than one 
   RHS      = chartostring(CC(iEq+1:))       ; right hand side

                              ; does rhs have a ' or " [if so, string]
   iq = ind(CC.eq.quot1c .or. CC.eq.quot2c)  ; indices of quotes (',")

   if (any(.not.ismissing(iq))) then
       CC(iq) = quot2c        ; change ' to " [also "-to-" for convenience]

       if (nComma.gt.0) then              ; more than 1 element
           rhs         = new ( nComma+1, "string")
           delete (rhs@_FillValue)
                                          ; must parse CC(iEq+1:)
                                          ; put each element => array rhs
           iComma      = ind(CC.eq.commac); indices of commas
           rhs(0)      = chartostring( CC(iEq+1:iComma(0)-1) )
           rhs(nComma) = chartostring( CC(iComma(nComma-1)+1:nc-1) )
           if (nComma.gt.1) then
               do i=0,nComma-2
                  rhs(i+1) = chartostring(CC(iComma(i)+1:iComma(i+1)-1))
               end do
           end if

           nam@$aName$ = rhs
           delete (rhs)
           delete (iComma)
       else
           nam@$aName$ = RHS    ; single string
       end if
       
       delete (iq)
       delete (CC)
       continue                 ; go to next iteration
   end if
                                ; MUST be integer or real
   delete (iq)                  ; iq referred to index of ' or "

   nDecimal = num(CC.eq.decimalc)      ; number of decimal pts

   if (nComma.gt.0) then
       iComma          = ind(CC.eq.commac)  ; inices of ,
       if (nDecimal.eq.0) then
           rhs         = new ( nComma+1, "integer")
           rhs(0)      = stringtointeger( chartostring( CC(iEq+1:iComma(0)-1) ))
           rhs(nComma) = stringtointeger( chartostring( CC(iComma(nComma-1)+1:nc-1) ))
           if (nComma.gt.1) then
               do i=0,nComma-2
                  rhs(i+1) = stringtointeger(chartostring(CC(iComma(i)+1:iComma(i+1)-1)))
               end do
           end if
       else
           rhs         = new ( nComma+1, "float")
           rhs(0)      = stringtofloat( chartostring( CC(iEq+1:iComma(0)-1) ))
           rhs(nComma) = stringtofloat( chartostring( CC(iComma(nComma-1)+1:nc-1) ))
           if (nComma.gt.1) then
               do i=0,nComma-2
                  rhs(i+1) = stringtofloat( chartostring(CC(iComma(i)+1:iComma(i+1)-1)))
               end do
           end if
       end if
       delete (rhs@_FillValue)
       delete (iComma)
   else
       if (nDecimal.eq.0) then
           rhs = stringtointeger(RHS)
       else
           rhs = stringtofloat(RHS)
       end if
   end if

   nam@$aName$ = rhs     ; associate values with variable
   delete (rhs)

   delete (CC)
  end do

  return(nam)
end

;*************************************************************************
; D. Shea
;Within a string:  replace one character with another
; usage:   s = "apples are good"
;          replaceSingleChar(s, " ", "_")   ; ==> s="apples_are_good""

; This function was deprecated in V5.1.1 and replaced with
; the built-in function "str_sub_str".

undef("replaceSingleChar")
procedure replaceSingleChar (s[*]:string, oldStr[1]:string, newStr[1]:string)
begin
  print("replaceSingChar: this function has been deprecated.")
  print("                 Will use str_sub_str.")
  s = str_sub_str(s,oldStr,newStr)
end

;*************************************************************************
; D. Shea
; create symmetric min/max values for plots
; will add additional plot resources to the "res" variable
; 
; usage:    res = True
;          symMinMaxPlt(zData, 14, False, res)
;
undef("symMinMaxPlt")
procedure symMinMaxPlt (x:numeric, nCnLvl:integer, inOut:logical, res:logical)
local xMin, xMax, cmnmx, mnmxint
begin
  xMin  = min(x)
  xMax  = max(x)
  cmnmx = max( (/fabs(xMin), fabs(xMax)/) )  ; larger of two values

  mnmxint = nice_mnmxintvl( -cmnmx, cmnmx, nCnLvl, inOut)

  res@cnLevelSelectionMode = "ManualLevels"   
  res@cnMinLevelValF       = mnmxint(0)
  res@cnMaxLevelValF       = mnmxint(1)
  if (isnan_ieee(mnmxint(2))) then            ; extreme case
      res@cnLevelSpacingF  = 1.0
  else
      res@cnLevelSpacingF  = mnmxint(2)
  end if
end

undef("isStrSubset")
function isStrSubset(S[1]:string, s[1]:string)
; return True or False is "s" is a subset of "S" 
local SC, sc, nsc, nSC, n, sTF
begin

   SC  = stringtochar(S)     ; main
   sc  = stringtochar(s)     ; subset

   nSC = dimsizes( SC ) - 1  ; extra 'end of char' at end
   nsc = dimsizes( sc ) - 1

   sTF = False
   if (nsc.le.nSC) then      ; nsc must be <= nSC
       do n=0,nSC-nsc
          if (all(SC(n:n+nsc-1).eq.sc(0:nsc-1)) ) then
              sTF = True
              return (sTF)
          end if
       end do
   end if
   
   return (sTF)
end
; ----------------------
undef("indStrSubset")
function indStrSubset(S[1]:string, s[1]:string)
; return the indices of the characters
; of "S" of which "s" is a subset. 
local SC, sc, nsc, nSC, n, ii
begin

   if (ismissing(S) .or. ismissing(s)) then
       print("indStrSubset: missing string")
       imsg = default_fillvalue("integer")
       ii = (/imsg,imsg/)
       ii@_FillValue = imsg
       return(ii)
   end if

   SC  = stringtochar(S)     ; main
   sc  = stringtochar(s)     ; subset

   nSC = dimsizes( SC ) - 1  ; extra 'end of char' at end
   nsc = dimsizes( sc ) - 1

   if (nsc.le.nSC) then      ; nsc must be <= nSC
       do n=0,nSC-nsc
          if (all(SC(n:n+nsc-1).eq.sc(0:nsc-1)) ) then
              ii  = ispan(n,n+nsc-1,1)
              return( ii )
          end if
       end do
   end if

   ii = new ( 1, integer)
   return (ii)
end

undef ("getSubString")
function getSubString (s[*]:string, iStrt:integer, iLast:integer)
;*************************************************************************
; D. Shea
;     extract a sub-string from one or more strings 
;     a = "0850-0899"
;     a1= getSubString(a, 0, 3)  ; ==> a1 = "0850"
;     a2= getSubString(a, 5, 8)  ; ==> a2 = "0899"
;
;     A = (/ "vorticity", "flux", "divergence" /)
;     As= getSubString(A, 0, 4)  ; ==> As = (/"vort","flux","dive"/)

; This function was deprecated in V5.1.1 and replaced with
; the built-in function "str_get_cols".

begin
  return(str_get_cols(s,iStrt,iLast))
end

undef("wallClockElapseTime")
procedure wallClockElapseTime(wcStrt:string, title:string, opt:integer)
;
; compute *Wall Clock* elapsed time in seconds
; Usage:     wcStrt = systemfunc("date")
;                   :    ; one or more statements [usually a block of code]
;            wallClockElapseTime(wcStrt, "short_info", 0)
; opt not used right now
; this will not handle case where year or month changes
;
; Haley: updated Aug 2014 to return the date in the correct format.
;
local wcNow, wcStrt_c, tSTRT, wcNow_c, tNOW, sec, NLINE, dq, date_cmd
begin
  dq        = str_get_dq()
  date_cmd  = "date +" + dq + "%a %b %d %H:%M:%S %Z %Y" + dq
;;wcNow     = systemfunc("date")
  wcNow     = systemfunc(date_cmd)   ; current ("now") time; must be
  wcNow_c   = stringtochar(wcNow)    ;    be 'Wed Aug 27 08:38:33 MDT 2014' format
  wcStrt_c  = stringtochar(wcStrt)

  if (dimsizes(wcNow_c).eq.dimsizes(wcStrt_c) .and. \
      dimsizes(wcNow_c).eq.29) then  ; includes end-of-line character

      tSTRT = stringtointeger((/wcStrt_c( 8: 9)/))*86400 \
            + stringtointeger((/wcStrt_c(11:12)/))*3600  \
            + stringtointeger((/wcStrt_c(14:15)/))*60    \
            + stringtointeger((/wcStrt_c(17:18)/))

      tNOW  = stringtointeger((/wcNow_c( 8: 9)/))*86400  \
            + stringtointeger((/wcNow_c(11:12)/))*3600   \
            + stringtointeger((/wcNow_c(14:15)/))*60     \
            + stringtointeger((/wcNow_c(17:18)/))

      secElapseTime = tNOW-tSTRT

      NLINE     = inttochar(10)   ; new line character
      print (NLINE+ \
        "=====> Wall Clock Elapsed Time: "+title+": "+   \
         secElapseTime+" seconds <====="+NLINE)
  else
      print("wallClockElapseTime: something wrong: no printed value")
  end if

end

undef("partitionString")
function partitionString(s:string , fs:string)
; D Shea
; Subset a string into parts depending upon a string separator [fs].
; The fs argument can be only one character.
;
;    code = "apple:orange:cider:peach"     ; 4 components
;    a    = partitionString(code, ":")
; result will be a 1D array of length 4 containing
;    a    = (/"apple","orange","cider","peach"/)

local cs, cfs, i, nStr, str, n, iStrt, iLast
begin
  cs  = stringtochar(s)     ; string to individual characters   
  cfs = stringtochar(fs)    ; string separator as character
  if (dimsizes(cfs).ne.2) then
      print ("*****")
      print ("partitionString: fs can contain only one character, fs="+fs)
      print ("*****")
      return(s)
  end if
 
  i   = ind(cs .eq. cfs(0)) ; indices where fs occurs
  
  if (.not.any(ismissing(i)) ) then 
      nStr  = dimsizes(i) + 1
      str   = new ( nStr, "string")

      iStrt = 0
      do n=0,nStr-2
         iLast  = i(n)-1
         str(n) =  chartostring((/cs(iStrt:iLast)/)) 
         iStrt  = i(n)+1
      end do
      iLast = dimsizes(cs)-2
      str(nStr-1) =  chartostring((/cs(iStrt:iLast)/)) 
  else
      str = s     ; string separator found
  end if

  return(str)
end

;*************************************************************************
; D. Shea
; read an ascii file and return just the header as a 1D array of strings.
;
; The header is the non-numeric part. Note upon
; return this may have to be parsed individually for specific info. 
;
; opt - option
;       if type "integer" or "long" then it is the number of rows
;       (line) for the header. This is a 'fixed' number of lines.
;        [06 Feb 2011 - add support for "long" nrows]
;
;       if type "string" then it is a sequence of characters
;       that terminate a variable number of header rows.
;       Currently this just checks character sequences
;       starting in col 0. 
;
;       if type "float" and positive then it will read  all values as float. 
;       eg: f_head = readAsciiHead("foo.ascii", 3.) 
;       all numbers on the 1st three rows will be read as float.

undef("readAsciiHead")
function readAsciiHead( fName:string, opt)
local xs, xc, nr, nrow, noptc, nxc, f_head
begin
  xs   = asciiread(fName, -1, "string") ; all rows as strings
 
  if (any(typeof(opt).eq.(/"integer","long"/))) then
      if (opt.gt.0) then
          return( xs(0:opt-1) )       ; return just header rows
      else
          print ("contributed: readAsciiHead: opt must be >0")
          return( new(1,string) )
      end if
  end if

  if (typeof(opt).eq."string") then
      nrow = dimsizes(xs)
      do nr=0,nrow-1
         ii = str_index_of_substr(xs(nr),opt,1)  ; Find the 1st match
         if(.not.any(ismissing(ii)).and.ii.eq.0) then
             return( xs(0:nr) )
             exit
         end if
      end do
      return(new(1,string))
  end if

  if (typeof(opt).eq."float") then
      nrow = tolong(opt)    ; # of rows to be read as float

      tmpdir  = ncargpath("tmp") + "/"
      if(ismissing(tmpdir)) then
        tmpdir = "./"
      end if

      tmpfile = systemfunc("echo tmp$$")
      if (ismissing(tmpfile)) then
          tmpfile = "BoGuS_file"
      end if

      asciiwrite (tmpdir+tmpfile, xs(0:nrow-1))
      f_head = asciiread(tmpdir+tmpfile, -1, "float")
      system("/bin/rm "+tmpdir+tmpfile)

      return( f_head )
  end if

end

;*************************************************************************
; D. Shea
; read an ascii file and return the table data after the header portion
; opt - option
;       if type "integer" it can be a scalar or have size [2]
;       opt/opt(0) specifies the number of rows (lines) for the header. 
;       This is a 'fixed' number of lines.
;       if opt has 2 elements, the second integer value [ opt(1) ]
;       specifies the number of lines at the end of the 
;       file to be ignored.
;
;       if type "string" then it is a sequence of characters
;       that terminate a variable number of header rows.
;       Currently this just checks character sequences
;       starting in col 0. 
; ncol- number of columns in table
;
;   [06 Feb 2011 - added support for "long" ncol]

undef("readAsciiTable")
function readAsciiTable( fName:string, ncol, datatype:string, opt)
local head, nh, xs, nrow, table, nopt, valid_dim_types
begin
  valid_dim_types = (/"integer","long"/)
  if (.not.any(typeof(ncol).eq.valid_dim_types)) then
    print ("contributed: readAsciiTable: ncol must be int or long")
    return(new(1,string))
  end if
  nopt = dimsizes(opt)
  if (any(typeof(opt).eq.valid_dim_types) .and. nopt.eq.1 .and. \
      opt(0).eq.0) then
      nrow  = dimsizes(asciiread(fName, -1, "string"))  ; all rows as strings
      table = asciiread(fName, (/nrow,ncol/), datatype)
  else
    ;;if (opt(0).gt.0) then
      if ((nopt.eq.1 .and. typeof(opt).eq."string") .or. \
           opt(0).gt.0) then
        head = readAsciiHead( fName, opt(0))
        nh   = dimsizes(head)                 ; # header records
      else
        nh = 0                                ; no header to read
      end if
      xs   = asciiread(fName, -1, "string") ; all rows as strings
      nrow = dimsizes(xs)-nh                ; # rows after header
      if (nopt.gt.1) then
          nrow = nrow - opt(1) 
      end if

      tmpdir  = ncargpath("tmp") + "/"
      if(ismissing(tmpdir)) then
        tmpdir = "./"
      end if

    ;;tmpfile = systemfunc("echo tmp$$")   ; See JIRA: NCL-2174
      tmpnum = systemfunc("date +%N")      ; introduced 6.3.0
      if(tmpnum.eq."N")then
        delete(tmpnum)
        srand(toint(systemfunc("date +%s")))
        tmpnum = rand()
      end if
      tmpfile = "tmp"+tmpnum

      if (ismissing(tmpfile)) then
          tmpfile = "BoGuS_file"
      end if

      asciiwrite (tmpdir+tmpfile, xs(nh:nh+nrow-1))
      table = asciiread(tmpdir+tmpfile, (/nrow,ncol/), datatype)
      system("/bin/rm "+tmpdir+tmpfile)
  end if
  return(table)
end

;********************************************************************
; D. Shea
; wrapper for NCL function "hyi2hyo" that copies attributes and coordinate vars.
; It adds the new level coordinates.


undef("hyi2hyo_Wrap")
function hyi2hyo_Wrap(p0,hyai,hybi,psfc,xi,hyao,hybo,option)
;
; hyai, hybi - input  hybrid coordinates
; hyao, hybo - output hybrid coordinates
;
local xo, dName, nDim, P0, lev, n
begin
   xo = hyi2hyo(p0,hyai,hybi,psfc,xi,hyao,hybo,option)

   copy_VarAtts(xi,xo) 
   xo@info = "NCL function hyi2hyo used to interpolate" 

   dName = getvardims( xi )  ; get dim names of the input variable
   if (any(dName.eq."missing")) then
       return (xo)
   end if
   
   nDim = dimsizes( dimsizes(xo) )

   do n=0,nDim-1                     ; name the dimensions
      xo!n = dName(n)
     ;print("dName: "+n+": "+dName(n))
   end do

   if (isatt(p0,"units") .and. \
      (p0@units.eq."Pa" .or. p0@units.eq."Pascals") .or. \
      (p0.eq.100000.))  then
       P0  = p0/100.
       P0@units = "hPa"

       if (isStrSubset(hyai@long_name,"interfaces") .or. \
           isStrSubset(hyai@long_name,"midpoints" )) then
           lev                = (hyao+hybo)*P0
           lev@long_name      = "level"
           lev@units          = "hPa"

           if (isStrSubset(hyai@long_name,"interfaces")) then
               lev@long_name     = "hybrid level at interfaces (1000*(A+B))"
               lev@formula_terms = "a: hyai b: hybi p0: P0 ps: PS" 
               lev!0             = "ilev"
           end if
           if (isStrSubset(hyai@long_name,"midpoints")) then
               lev@long_name      = "hybrid level at midpoints (1000*(A+B))"
               lev@formula_terms  = "a: hyam b: hybm p0: P0 ps: PS" 
               lev!0             = "lev"
           end if
       else
           lev                = (hyao+hybo)*P0
           lev@long_name      = "level"
           lev@units          = "hPa"
           lev@long_name      = "hybrid level (1000*(A+B))"
           lev@formula_terms  = "a: hya b: hyb p0: P0 ps: PS" 
       end if
   end if
   lev@standard_name  = "atmosphere_hybrid_sigma_pressure_coordinate"
   lev@positive       = "down" 

   if (nDim.eq.3) then
       if (isvar("lev")) then
           xo&$dName(0)$ = lev
       end if

       do n=1,2
          if (iscoord(xi, dName(n)) ) then
              xo&$dName(n)$ = xi&$dName(n)$
          end if
       end do
   end if

   if (nDim.eq.4) then
       do n=0,3
          if (n.eq.1 .and. isvar("lev")) then
              xo&$dName(n)$ = lev
          else
              if (iscoord(xi, dName(n)) ) then
                  xo&$dName(n)$ = xi&$dName(n)$
              end if
          end if
       end do
   end if

   return (xo)
end
;**************************************************************
; D. Shea
; wrapper for NCL function "shsgc_R42"  that copies attributes and coordinate vars.
; It adds the longitude and gaussian latitude coordinates.

undef("shsgc_R42_Wrap")
function shsgc_R42_Wrap (a:numeric, b:numeric)
local xNew, lat, gwt, lon, nDim
begin
  xNew = shsgc_R42(a,b) 
  lat  = latGau    (108, "lat", "latitude" , "degrees_north")
  gwt  = latGauWgt (108, "lat", "gaussian weights", "")
  lon  = lonGlobeF (128, "lon", "longitude", "degrees_east")

  if (isatt(a,"long_name")) then
      xNew@long_name = a@long_name
  end if
  if (isatt(a,"units")) then
      xNew@units = a@units
  end if

  nDim     = dimsizes(dimsizes(xNew))           ; number of dimensions
  xNew!(nDim-2)  = "lat"                        ; 2nd rightmost dimension
  xNew!(nDim-1)  = "lon"                        ; rightmost dimension
      
  xNew&lat = lat(::-1)                          ; add new coord var
  xNew&lon = lon
  xNew@gwt = gwt                                ; attach as attribute
  return (xNew)
end
;********************************************************************
; D. Shea
; wrapper for NCL function "pres2hybrid" that copies attributes and coordinate vars.
; It adds the new level coordinates.


undef("pres2hybrid_Wrap")
function pres2hybrid_Wrap(p[*],psfc,p0,xi,hyao,hybo,intflg)
local xo, dName, nDim, P0, lev, n
begin
   xo = pres2hybrid(p,psfc,p0,xi,hyao,hybo,intflg)

   copy_VarAtts(xi,xo) 
   xo@info = "NCL function pres2hybrid used to interpolate" 

   dName = getvardims( xi )  ; get dim names of the input variable
   if (any(ismissing(dName))) then
       return (xo)
   end if
   
   nDim = dimsizes( dimsizes(xo) )
   if (nDim.ge.5) then
       print("pres2hybrid_Wrap: Too many dimensions: nDim="+nDim)
       exit
   end if
   if (nDim.lt.3) then
       print("pres2hybrid_Wrap: Too few  dimensions: nDim="+nDim)
       exit
   end if

   do n=0,nDim-1                     ; name the dimensions
      xo!n = dName(n)
   end do

   if (isatt(p0,"units") .and. \
      (p0@units.eq."Pa" .or. p0@units.eq."Pascals") .or. \
      (p0.eq.100000.))  then
       P0  = p0/100.
       P0@units = "hPa"
       lev      = (hyao+hybo)*P0
       lev@long_name     = "level"
       lev@units         = "hPa"
       lev@formula_terms = "a: hyam b: hybm p0: P0 ps: PS" 
       lev@positive      = "down" 
       lev@standard_name = "atmosphere_hybrid_sigma_pressure_coordinate"
   end if

   if (nDim.eq.3) then
       if (isvar("lev")) then
           xo&$dName(0)$ = lev
       end if

       do n=1,2
          if (iscoord(xi, dName(n)) ) then
              xo&$dName(n)$ = xi&$dName(n)$
          end if
       end do
   end if

   if (nDim.eq.4) then
       do n=0,3
          if (n.eq.1 .and. isvar("lev")) then
              xo&$dName(n)$ = lev
          else
              if (iscoord(xi, dName(n)) ) then
                  xo&$dName(n)$ = xi&$dName(n)$
              end if
          end if
       end do
   end if

   return (xo)
end
;
;**************************************************************
;
undef("albedo_ccm")
function albedo_ccm(FLUX1:numeric, FLUX2:numeric, formula[1]:integer)
;
; Calculate albedo from CLM/CAM-CESM model variables
; This is a ratio (ie, non-linear). The correct way is to compute each 
; time step and average. However, commonly the monthly means are used directly. 
;
; examples
; formula=0  albedo_broad_band  = albedo_ccm(FSDS, FSNS, 0)
;            albedo_broad_band <= (FLUX1-FLUX2/FLUX1 = (FSDS-FSNS)/FSDS   
;
; formula=1; aldedo_nir_band    = albedo_ccm(FSRND, FSDSND, 1)
;            aldedo_nir_band   <=  FLUX2/FLUX1       = FSRND/FSDSND 
;
; ptaylor (2/18/09; edited)
; The (FSDS-FSNS)/FSDS formula will give the broadband flux albedo. 
; However, in the radiation code of the CAM and CCSM4 different albedos are used: 
;     (a) Visible direct beam; (b) Visible diffuse bean; 
;     (c) Near-IR direct beam; (d) Near-IR diffuse beam. 
; The CLM output data includes the necessary spectral fluxes at the 
;    FSRND  (direct nir reflected solar radiation). 
;    The Near-IR albedo would be
;        ALBDIR= FLUX2/FLUX1= FSRND/FSDSND                ; formula 1
;        ALBDIR@long_name="Near-IR Direct Beam Albedo"
;
;    The surface broad band albedo would be
;        ALBSFC= (FLUX1-FLUX2/FLUX1= (FSDS-FSNS)/FSDS     ; formula 0
;        ALBSFC@long_name="Surface Albedo: Broad Band"
; 
; Any appropriate pair of variables:
; Surface: FLUX1: FSDS   - Downwelling solar flux at surface
;          FLUX2: FSNS   - Net solar flux at surface
;          ALBEDO_SFC    = albedo_ccm(FSDS, FSNS, 0)
; TOA      FLUX1: ?????? - ??? solar flux at top of atmosphere
;          FLUX2: FSNTOA - Net solar flux at top of atmosphere
;          ALBEDO_TOA    = albedo_ccm(???, FSNTOA, 0)
; TOM      FLUX1: ?????? - ??? solar flux at top of model
;          FLUX2: FSNT   - Net solar flux at top of model
;          ALBEDO_TOM    = albedo_ccm(???, FSNT  , 0)
; nir      FLUX1: FSRND  - direct nir reflected solar radiation"
;          FLUX2: FSDSND - direct nir incident solar radiation
;          ALBEDO_NIR    = albedo_ccm(FSRND, FSDSND, 1)

local albedo
begin
   if (formula.lt.0 .or. formula.gt.1) then
       print("albedo_ccm: FATAL:  formula argument must be 0 or 1: formula="+formula)
     ;;albedo = new (dimsizes(flux1), typeof(flux1))
     ;;return(albedo)
       exit
   end if

   if (any(FLUX2.gt.FLUX1)) then
       print("albedo_ccm: WARNING:  FLUX2 > FLUX1 should not occur")
   end if

; Avoid division by 0.0  [ use _FillValue

   if (formula.eq.0) then
       if (any(FLUX1.eq.0)) then
          if (isatt(FLUX1,"_FillValue")) then
              albedo = (FLUX1-FLUX2)/where(FLUX1.eq.0, FLUX1@_FillValue, FLUX1) 
          else
              albedo = (/ FLUX1 /) 
              albedo@_FillValue = 1e10
              albedo = (FLUX1-FLUX2)/where(FLUX1.eq.0, albedo@_FillValue, FLUX1) 
          end if
       else
           albedo = (FLUX1-FLUX2)/FLUX1              ; no FLUX1=0.0 
       end if
   end if

   if (formula.eq.1) then
       if (any(FLUX1.eq.0)) then
          if (isatt(FLUX1,"_FillValue")) then
              albedo = FLUX2/where(FLUX1.eq.0, FLUX1@_FillValue, FLUX1) 
          else
              albedo = (/ FLUX1 /) 
              albedo@_FillValue = 1e10
              albedo = FLUX2/where(FLUX1.eq.0, albedo@_FillValue, FLUX1) 
          end if
       else
          albedo = FLUX2/FLUX1                       ; no FLUX1=0.0
       end if
   end if
   
   albedo@long_name = "albedo"
   albedo@units     = "fraction"
   if (formula.eq.0) then
       albedo@formula   = "formula="+formula+": albedo = (flux1-flux2)/flux1"
   else
       albedo@formula   = "formula="+formula+": albedo =  flux2/flux1"
   end if
   copy_VarCoords(FLUX1, albedo)
   return(albedo)
end
;
;**************************************************************
; D. Shea
; Driver for NCL function "omega_ccm"  
; It calculates intermediate quantities needed for input.
;
undef("omega_ccm_driver")
function omega_ccm_driver(p0,psfc,u,v,hyam[*],hybm[*],hyai[*],hybi[*])

; calculate assorted intermediate quantities 
; prior to invoking the built-in function "omega_ccm"
;
;  p0     - Scalar numeric value equal to surface reference pressure in Pa.
;  psfc   - 2D or 3D array  ([time,]lat,lon) of surface pressures in Pa. 
;  u, v   - 3D or 4D arrays ([time,]lev,lat,lon) of zonal and meridional wind (m/s)
;  hyam   - 1D array containing the hybrid A coefficients. Must have the 
;           same dimension as the level dimension of u and v. 
;           The order must be top-to-bottom. 
;  hybm   - 1D array containing the hybrid B coefficients. Must have the 
;           same dimension as the level dimension of u and v. 
;           The order must be top-to-bottom. 
;  hyai   - 1D array containing the interface hybrid A coefficients. 
;           The order must be top-to-bottom. 
;  hybi   - 1D array containing the interface hybrid B coefficients. 
;           The order must be top-to-bottom. 


begin
  dimps  = dimsizes(psfc)
  rankps = dimsizes(dimps)
  dimu   = dimsizes(u)
  ranku  = dimsizes(dimu)
  if ((ranku .eq.rankps)             .or. \
      (ranku .le.2 .or.  ranku.ge.5) .or. \
      (rankps.le.1 .or. rankps.ge.4) .or. \
      (ranku .eq.4 .and.rankps.ne.3) .or. \
      (ranku .eq.3 .and.rankps.ne.2)) then
      print("omega_ccm_driver: expected ranku=3 or 4 and rankps=2 or 3")
      print("omega_ccm_driver: got      ranku="+ranku+"  and rankps="+rankps)
      exit
  end if
                          ; only klev is used below: ntim, nlat, mlon  are FYI only
  if (ranku.eq.3) then
      klev   = dimu(0)
      nlat   = dimu(1)
      mlon   = dimu(2)
  else
      ntim   = dimu(0)
      klev   = dimu(1)
      nlat   = dimu(2)
      mlon   = dimu(3)
  end if

  omega = u                                 ; create space
  omega@long_name = "Vertical pressure velocity"
  omega@units     = "Pa/s"

  lat   = omega&lat
                      ;  hybd  = new((/klev/),typeof(hyai))
                      ;  do k=0,klev-1
                      ;    hybd(k) = hybi(k+1)-hybi(k)
                      ;  end do
  klevi  = dimsizes(hybi)
  hybd   = hybi(1:) - hybi(0:klevi-2)
  nprlev = 0                                ; number of pure pressure levels
  do k=1,klev
    if (nprlev .eq. 0 .and. hybi(k) .ne. 0.0) then
      nprlev = k - 1 
    end if
  end do  

  pdel  = dpres_hybrid_ccm(psfc,p0,hyai,hybi)
  pmid  = pres_hybrid_ccm (psfc,p0,hyam,hybm)

  dpsl  = psfc                            ; create space for retrn variables
  dpsm  = psfc
  psln  = log(psfc)
  gradsg(psln,dpsl,dpsm)                  ; gradients of log(psfc) gaussian grid

  div   = uv2dvG(u,v)                     ; divergence on gaussian grid

  omega = omega_ccm(u       ,v       ,div     ,dpsl    \
                   ,dpsm    ,pmid    ,pdel             \
                   ,psfc    ,hybd    ,hybm    ,nprlev  )

  return(omega)
end
;************************************************************
; D. Shea 
;
; Converts OMEGA (Pa/s) to W (m/s)                          
; This is a 1st order approximation.

undef("omega_to_w")
function omega_to_w (omega, p, t)
;
; first order conversion: w = -omega/(density*gravity)
;
; omega: units = "Pa/sec"
; p    : units = "Pa"
; t    : units = "K"

local dims_omega, dims_t, dims_p, rank_omega, rank_t, rank_p, RGAS, GRAV, rho, w
begin
   dims_omega = dimsizes( omega )
   dims_t     = dimsizes( t )
   dims_p     = dimsizes( p )

   rank_omega = dimsizes( dims_omega )
   rank_t     = dimsizes( dims_t ) 
   rank_p     = dimsizes( dims_p ) 

   if (rank_omega.ne.rank_t .or. rank_omega.ne.rank_p .or. \
       .not.(all(dims_omega.eq.dims_t) .and. all(dims_omega.eq.dims_p)) ) then
       print("omega_to_w: omega, p, t must be the same rank & size")
       exit
   end if

   RGAS = 287.058            ; J/(kg-K) => m2/(s2 K)
   GRAV = 9.80665            ; m/s2
   rho  = p/(RGAS*t)         ; density => kg/m3
   w    = -omega/(rho*GRAV)  ; array operation

   w@long_name = "vertical velocity"
   w@units     = "m/s"
   w@info_tag  = "NCL: omega_to_w: -omega/(rho*g): approximation "
   copy_VarCoords( omega, w)
   return( w )
end 
;************************************************************
; D. Shea 
;
; Converts W (m/s) to OMEGA (Pa/s)                          
; This is a 1st order approximation.

undef("w_to_omega")
function w_to_omega (w, p, t)
;
; first order conversion: omega= -w*density*gravity
;
; w    : units = "m/sec"
; p    : units = "Pa"
; t    : units = "K"

local dims_w, dims_t, dims_p, rank_w, rank_t, rank_p, RGAS, GRAV, rho, omega
begin
   dims_w = dimsizes( w )
   dims_t = dimsizes( t )
   dims_p = dimsizes( p )

   rank_w = dimsizes( dims_w )
   rank_t = dimsizes( dims_t )
   rank_p = dimsizes( dims_p )

   if (rank_w.ne.rank_t .or. rank_w.ne.rank_p .or. \
       .not.(all(dims_w.eq.dims_t) .and. all(dims_w.eq.dims_p)) ) then
       print("w_to_omega: omega, p, t must be the same rank & size")
       exit
   end if

   RGAS = 287.058            ; J/(kg-K) => m2/(s2 K)
   GRAV = 9.80665            ; m/s2
   rho  = p/(RGAS*t)         ; density => kg/m3
   omega    = -w*rho*GRAV    ; array operation

   omega@long_name = "vertical velocity"
   omega@units     = "Pa/s"
   omega@info_tag  = "NCL: w_to_omega: omega= -w*rho*g: approximation "
   copy_VarCoords( w, omega)
   return( omega )
end

;************************************************************
; D. Shea and S. N. Hameed <U. Hawaii> 
;
; Determines the relative minima for a 1-dimensional array. 
; x      - A 1-dimensional float or double array. 
;          Missing data are not allowed.
; cyclic - Set to True if data are cyclic.  
;          Set to False if the data array is not cyclic in x.
; delta  - Tolerance level (negative). If values are within delta of 
;          surrounding values it will not be counted as a local min value. 
; opt    - indicates what is to be returned
;          0         ; return the minimum values
;          opt.ne.0  ; return the indices [subscripts] where 
;                    ; minimum values occured.
;
; usage:   qmin = local_min_1d(q, False, -0.25, 0)
;
;          imin = local_min_1d(q, False, -0.25, 1)
;          if (.not.ismissing(imax)) then
;              qmin = q(imin)
;              zmin = z(imin)   ; z where q is at a minimum
;          end if
;
; M.Haley, 5 Feb 2011 - updated to allow long dimension sizes

undef("local_min_1d")
function local_min_1d( X[*]:numeric  , cyclic:logical \
                     , del[1]:numeric, opt[1]:integer )
local NX, nx, x, iOffSet, nStrt, nLast, imn, i, type_nx
begin
  if (del.gt.0.0) then
      print("contributed: local_min_1d: del must be .le. 0.0")
      print("contributed: local_min_1d: del ="+del)
      exit 
  end if

  if (any(ismissing(X))) then
      print("contributed: local_min_1d: missing values not allowed")
      print("contributed: local_min_1d: "+num(ismissing(X)) \
                       +" missing values encountered")
      exit 
  end if

  NX  = dimsizes(X)

  if (cyclic) then
      nx        = NX+2
      x         = new (nx, typeof(X), getFillValue(X)) 
      x(0)      = (/ X(NX-1) /)
      x(nx-1)   = (/ X(0)    /)
      x(1:nx-2) = (/ X       /) 
      iOffSet   = -1
  else
      x         = (/ X /)
      iOffSet   = 0
  end if

  nx      = dimsizes(x)
  type_nx = typeof(nx)                     ; int or long
  imn     = new( nx, type_nx )
  nStrt   = new(  1, type_nx )
  nLast   = new(  1, type_nx )
  i       = new(  1, type_nx )

  nStrt = 1
  nLast = nx-2   
  i     = -1
  do n=nStrt,nLast
     if ((x(n)-x(n-1)).le.del .and. (x(n)-x(n+1)).le.del) then
         i = i+1
         imn(i) = n   ; indices of local minima
     end if
  end do
  i@_FillValue = -1             ; trick ... assign after loop 

  if (.not.ismissing(i)) then
      if (opt.eq.0) then
          return(x(imn(0:i)))   ; return actual minimum values
      else
          return(imn(0:i)+iOffSet)
      end if
  else                          ; must be no local minima
      if (opt.eq.0) then
          return(default_fillvalue(typeof(x)))
      else
          return(i)           ; default integer missing code
      end if
  end if
end
;************************************************************
; D. Shea and S. N. Hameed <U. Hawaii> 
;
; Determines the relative maxima for a 1-dimensional array. 
; x      - A 1-dimensional float or double array. 
;          Missing data are not allowed.
; cyclic - Set to True if data are cyclic.  
;          Set to False if the data array is not cyclic in x.
; delta  - Tolerance level (positive). If values are within delta 
;          of surrounding values it will not be counted as a local max value. 
; opt    = indicates what is to be returned
;          0         ; return the maximum values
;          opt.ne.0  ; return the indices [subscripts] where 
;                    ; maximum values occured.
;
; usage:   qmax = local_max_1d(q, False, 0.25, 0)
;
;          imax = local_max_1d(q, False, 0.25, 1)
;          if (.not.ismissing(imax)) then
;              qmax = q(imax)
;              zmax = z(imax)   ; z where q is at a maximum
;          end if

undef("local_max_1d")
function local_max_1d( X[*]:numeric  , cyclic:logical \
                     , del[1]:numeric, opt[1]:integer )
local NX, nx, x, iOffSet, nStrt, nLast, imx, i, type_nx
begin
  if (del.lt.0.0) then
      print("contributed: local_max_1d: del must be .ge. 0.0")
      print("contributed: local_max_1d: del ="+del)
      exit 
  end if

  if (any(ismissing(X))) then
      print("contributed: local_max_1d: missing values not allowed")
      print("contributed: local_max_1d: "+num(ismissing(X)) \
                       +" missing values encountered")
      exit 
  end if

  NX  = dimsizes(X)        
                          
  if (cyclic) then          
      nx        = NX+2                 
      x         = new (nx, typeof(X), getFillValue(X))    
      x(0)      = (/ X(NX-1) /)  
      x(nx-1)   = (/ X(0)    /)   
      x(1:nx-2) = (/ X       /)    
      iOffSet   = -1
  else                              
      x         = (/ X /)            
      iOffSet   = 0
  end if

  nx      = dimsizes(x)
  type_nx = typeof(nx)                     ; int or long
  imx     = new( nx, type_nx ) 
  nStrt   = new(  1, type_nx )
  nLast   = new(  1, type_nx )
  i       = new(  1, type_nx )

  nStrt = 1 
  nLast = nx-2   
  i     = -1
  do n=nStrt,nLast
     if ((x(n)-x(n-1)).ge.del .and. (x(n)-x(n+1)).ge.del) then
         i    = i+1
         imx (i) = n
     end if
  end do
  i@_FillValue = -1             ; trick ... assign after loop 

  if (.not.ismissing(i)) then
      if (opt.eq.0) then
          return(x(imx(0:i)))   ; return actual maximum values
      else
          return(imx(0:i)+iOffSet)      ; return index values
      end if
  else                          ; must be no local maxima
      if (opt.eq.0) then
          return(default_fillvalue(typeof(x)))
      else
          return(i)
      end if
  end if
end
;************************************************************
; Internal
; Copy all of the coordinate variables from one variable to another.  
; starting at dimension 1           

undef("copy_VarCoords_skipDim0")
procedure copy_VarCoords_skipDim0(var_from,var_to)  
local dfrom, dto, rfrom, rto, i, dName
begin                                     
  dfrom = dimsizes(var_from)
  dto   = dimsizes(var_to)
  
  rfrom = dimsizes(dfrom)
  rto   = dimsizes(dto)
                                             ; coordinates must have names
  dName = getvardims(var_from)               ; Oct 18, 2005

  if (.not.all(ismissing(dName))) then
          do i = 1,rto-1
             if (.not.ismissing(dName(i))) then   ; Oct 18, 2005
                var_to!i = var_from!i
                if(iscoord(var_from,var_from!i))
               var_to&$var_to!i$ = var_from&$var_from!i$
                end if
             end if
          end  do
  end if
end
; *****************************************************************
; D. Shea
; Turn monthly values [eg, precipitation] to annual values
; Two options: (a) sum of all 12 months (b) [sum of 12 values]/12
; Caveats: If there is more than one dimension, 
;            (a) all dimensions *must* be named.
;            (b) If there are no missing data [_FillValue], 
;                this function will work as expected.
; Nomenclature:
;   x          -  data array
;                 if rank is greater than one, then it is required that 
;                 the *leftmost* dimension be "time"
;                 eg: prc(time,lat,lon),  z(time,stations), q(time,lev,lat,lon)
;
;                 if 1D missing values [_FillValue] are allowed
;                 otherwise
;                 no missing values currently alloed
;
;   opt        - flag: opt=0 if annual total value [sum 12 values] is to be returned
;                      opt=1 if annual mean  value is to be returned [(annual total)/12]
;                    
;   Usage:
;                 PRC_annual_total = month_to_annual( prc, 0) 
;                 TMP_annual_mean  = month_to_annual( tmp, 1) 
;
; NOTE: there is some extra (non-sensical code) embedded due
; to a special request introduced in NCL 4.2.0.a033

undef("month_to_annual")
function month_to_annual(x:numeric, opt:integer)
local dimx, rankx, ntim, nyr, nyrs, nt, nx, ny, kz, nn, nmo \
    , dName, xAnnual, xTemp, n, m, k, nMsg, nmos, NMOS, nGood
begin
  if (opt.lt.0 .or. opt.gt.1) then
      print("month_to_annual: illegal value of opt: opt="+opt)
      exit
  end if

  NMOS  = 12                   ; fixed ... will not change
  nmos  = 12                   ; possible change in value
;;if (opt.gt.1) then           ; leftover from a 'special request'. No longer applicable.
;;    nmos = opt         
;;end if
  NMOS1 = NMOS-1               ; convenience

  dimx  = dimsizes(x)
  rankx = dimsizes(dimx)
  NTIM  = dimx(0)

  if (rankx .gt. 4) then
      print("*** FATAL  ***")
      print("month_to_annual: currently support up to 4D only")
      print("month_to_annual: rank="+rankx)
      exit
  end if

  if (NTIM%NMOS .ne. 0) then
      print("*** FATAL ***")
      print("month_to_annual: ntim="+NTIM+" not multiple of 12:  nyrs="+nyrs)
      exit
  end if

  nyrs = NTIM/NMOS
  ntim = nyrs*NMOS

  if (rankx.eq.1) then
      xAnnual = new ( nyrs, typeof(x), getFillValue(x)) ;  contributed.ncl

      nyr = -1
      do nt=0,ntim-1,NMOS
         nyr  = nyr+1

         nMsg = 0
         if (isatt(x,"_FillValue")) then
             nMsg = num (ismissing(x(nt:nt+NMOS1)) )
         end if
         nGood = NMOS-nMsg

         if (nMsg.eq.0 .and. opt.eq.0) then
             xAnnual(nyr)   = sum ( x(nt:nt+NMOS1) )
         end if
         if (nMsg.eq.0 .and. opt.ge.1) then
             xAnnual(nyr)   = avg ( x(nt:nt+NMOS1) )  ; always opt=1
         end if
;;       if (nMsg.gt.0 .and. opt.gt.1 .and. nGood.ge.nmos) then
;;           xAnnual(nyr)   = avg ( x(nt:nt+NMOS1) )  ; always opt=1
;;       end if
      end do

      copy_VarAtts (x, xAnnual)
      xAnnual@NCL  = "month_to_annual"
      xAnnual!0    = "year"
      return(xAnnual)
   end if

   dName  = getvardims(x)    ; get dim names
   if (any(ismissing(dName))) then
       print("*** FATAL  ***")
       print("month_to_annual: requires that all dimensions be named")
       exit
   end if
    
   if (rankx.eq.2) then
       nn     = dimx(1)  
       nyr    = -1
       xAnnual= new ( (/nyrs,nn/), typeof(x), getFillValue(x)) ;  contributed.ncl
       do nt=0,ntim-1,NMOS
          xTemp = x(nt:nt+NMOS1, :)       ; cleaner code only
          nyr   = nyr+1

          if (opt.eq.0) then
              xAnnual(nyr,:) = dim_sum_n ( xTemp, 0 )
          else
              xAnnual(nyr,:) = dim_avg_n ( xTemp, 0 )
          end if

          nMsg  = 0                             ; number of missing for current year (nyr)
          if (isatt(x,"_FillValue")) then
              nMsg = num (ismissing(xTemp) )    ; for all grid points
          end if

          if (nMsg.gt.0) then
              nGood = dim_num_n(.not.ismissing(xTemp), 0 )
              xAnnual(nyr,:) = mask( xAnnual(nyr,:), nGood.ge.nmos, True)  ; ?False
              print("month_to_annual: some points have missing data: nt="+nt \
                   +" nyr="+nyr+"  num(nGood)="+num(nGood.gt.0))
          end if

       end do

       copy_VarAtts (x, xAnnual)
       xAnnual@NCL  = "month_to_annual"
       xAnnual!0    = "year"
       copy_VarCoords_skipDim0 (x, xAnnual)
       return(xAnnual)
   end if

   if (rankx.eq.3) then
       ny     = dimx(1)      ; nlat
       mx     = dimx(2)      ; mlon
       nyr    = -1
       xAnnual= new ( (/nyrs,ny,mx/), typeof(x), getFillValue(x)) ;  contributed.ncl
       do nt=0,ntim-1,NMOS
          xTemp = x(nt:nt+NMOS1,:,:) ; cleaner code only
          nyr   = nyr+1

          if (opt.eq.0) then
              xAnnual(nyr,:,:) = dim_sum_n ( xTemp, 0 )
          else
              xAnnual(nyr,:,:) = dim_avg_n ( xTemp, 0 )
          end if

          nMsg  = 0                             ; number of missing for current year (nyr)
          if (isatt(x,"_FillValue")) then
              nMsg = num (ismissing(xTemp) )    ; for all grid points
          end if

          if (nMsg.gt.0) then
              nGood            = dim_num_n(.not.ismissing(xTemp), 0 )          ; (lat,lon)
              xAnnual(nyr,:,:) = mask( xAnnual(nyr,:,:), nGood.ge.nmos, True)  ; ?False
              print("month_to_annual: some grid points have missing data: nt="+nt \
                   +" nyr="+nyr+"  num(nGood)="+num(nGood.gt.0))
          end if
       end do

       copy_VarAtts (x, xAnnual)
       xAnnual@NCL  = "month_to_annual"
       xAnnual!0    = "year"
       copy_VarCoords_skipDim0 (x, xAnnual)
       return(xAnnual)
   end if

   if (rankx.eq.4) then
       kz     = dimx(1)      ; nlev
       ny     = dimx(2)      ; nlat
       mx     = dimx(3)      ; mlon
       nyr    = -1
       xAnnual= new ( (/nyrs,kz,ny,mx/), typeof(x), getFillValue(x)) ;  contributed.ncl
       do nt=0,ntim-1,NMOS
          xTemp = x(nt:nt+NMOS1,:,:,:) 
          nyr   = nyr+1

          if (opt.eq.0) then
                  xAnnual(nyr,:,:,:) = dim_sum_n ( xTemp, 0 )
          else
                  xAnnual(nyr,:,:,:) = dim_avg_n ( xTemp, 0 )
          end if

          nMsg  = 0                             ; number of missing for current year (nyr)
          if (isatt(x,"_FillValue")) then
              nMsg = num (ismissing(xTemp) )    ; for all grid points
          end if

          if (nMsg.gt.0) then
              nGood              = dim_num_n(.not.ismissing(xTemp), 0 )
              xAnnual(nyr,:,:,:) = mask( xAnnual(nyr,:,:,:), nGood.ge.nmos, True)
              print("month_to_annual: some grid points have missing data: nt="+nt \
                   +" nyr="+nyr+"  num(nGood)="+num(nGood.gt.0))
          end if
       end do

       copy_VarAtts (x, xAnnual)
       xAnnual@NCL  = "month_to_annual"
       xAnnual!0    = "year"
       copy_VarCoords_skipDim0 (x, xAnnual)
       return(xAnnual)
   end if
return
end

; *****************************************************************
; D. Shea

undef("month_to_annual_weighted")
function month_to_annual_weighted(yyyymm[*]:numeric, x:numeric, opt:integer)
local dimx, rankx, ntim, nyr, nyrs, nt, nx, ny, nz, nn, nmo \
    , dName, xAnnual, xTemp, nmos
begin
  yyyy  = yyyymm/100
  mm    = yyyymm - (yyyy*100)     ; mm=1,...,12
  nmos  = 12
  if (any(mm.lt.1 .or. mm.gt.12)) then
      print("month_to_annual_weighted: mm must be 1-to-12 inclusive")
      exit
  end if

  dimx  = dimsizes(x)
  rankx = dimsizes(dimx)
  NTIM  = dimx(0)

  if (NTIM.ne.dimsizes(yyyymm)) then
      print("month_to_annual_weighted: incompatible time dimesnions")
      print("                                  dimsizes(yyyymm) .ne. ntim  ")
      exit
  end if

  if (rankx .gt. 4) then
      print("*** FATAL  ***")
      print("month_to_annual_weighted: currently supports up to 4D only")
      print("month_to_annual_weighted: rank="+rankx)
      exit
  end if

  nyrs = NTIM/12
  ntim = nyrs*12

  if (NTIM%12 .ne. 0) then
      print("*** WARNING ***")
      print("month_to_annual_weighted: ntim="+NTIM+" not multiple of 12")
  end if

  if (isatt(yyyymm,"calendar")) then
      yyyy@calendar = yyyymm@calendar    ; needed for days_in_month
  end if

  dymon = days_in_month ( yyyy, mm)      ; accounts for calendar attribute

  if (typeof(x).eq."double") then
      one    = 1d0
  else
      one    = 1.0
  end if

  if (isatt(yyyymm,"calendar")) then    ; retrofit to existing code

      if (yyyymm@calendar.eq."360_day" .or. yyyymm@calendar.eq."360") then
          con = (/ 360, 360 /)*one
      end if
      if (yyyymm@calendar.eq."365_day" .or. yyyymm@calendar.eq."365"    .or. \
          yyyymm@calendar.eq."noleap"  .or. yyyymm@calendar.eq."no_leap") then
          con = (/ 365, 365 /)*one
      end if
      if (yyyymm@calendar.eq."366_day" .or. yyyymm@calendar.eq."366"    .or. \
          yyyymm@calendar.eq."allleap" .or. yyyymm@calendar.eq."all_leap") then
          con = (/ 366, 366 /)*one
      end if
      if (yyyymm@calendar.eq."gregorian" .or. yyyymm@calendar.eq."standard") then
          con = (/ 365, 366 /)*one 
      end if
  else
      con = (/ 365, 366 /)*one             ; default is gregorian/standard 
  end if

  wSum_1 = con(0)
  wSum_2 = con(1)

  if (rankx.eq.1) then
      xAnnual = new ( nyrs, typeof(x), getFillValue(x)) ;  contributed

      nyr = -1
      do nt=0,ntim-1,12
         nyr  = nyr+1

        ;nMsg = 0
        ;if (isatt(x,"_FillValue")) then
        ;    nMsg = num (ismissing(x(nt:nt+11)) )
        ;end if

        ;if (nMsg.eq.0) then
             wgt          = dymon(nt:nt+11)                ; days for each month
             xTemp        = x(nt:nt+11)*wgt                ; values(:)*wgt(:)
             xAnnual(nyr) = sum( xTemp )                   ; opt=0
             nAnnual      = num( .not.ismissing(xTemp) )
                                                          
             if (opt.eq.1) then                          
                 wgtSum = wSum_1
                 if (isleapyear(yyyy(nt))) then
                     wgtSum = wSum_2
                 end if
                 xAnnual(nyr) = xAnnual(nyr)/wgtSum        ; weighted average
             end if
             if (opt.eq.2) then
                 xAnnual(nyr) = xAnnual(nyr)/12            
             end if

             if (nAnnual.ne.12) then
                 xAnnual(nyr) = x@_FillValue
             end if
        ;end if
      end do

      xAnnual!0    = "year"
      xAnnual&year =  yyyy(0:ntim-1:12)
      copy_VarCoords_skipDim0 (x, xAnnual)

      if (isatt(x, "long_name")) then
          xAnnual@long_name = x@long_name
      end if
      if (isatt(x, "units")) then
          xAnnual@units     = x@units
      end if

      if (isatt(yyyymm,"calendar")) then
          xAnnual@calendar = yyyymm@calendar    
      end if

      return(xAnnual)
  end if

  if (rankx.eq.2) then
      dName  = getvardims(x)    ; get dim names
      xAnnual= new ( (/nyrs,dimx(1)/), typeof(x), getFillValue(x)) ;  contributed

      nyr    = -1
      do nt=0,ntim-1,12
         nyr     = nyr+1
         xTemp   = x(nt:nt+11,:)  

        ;nMsg = 0
        ;if (isatt(x,"_FillValue")) then
        ;    nMsg = num (ismissing(xTemp) ) 
        ;end if

        ;if (nMsg.eq.0) then
             wgt            = conform(xTemp, dymon(nt:nt+11), 0)
             xTemp          = xTemp*wgt                       ; values*wgt 
             xAnnual(nyr,:) = dim_sum_n( xTemp, 0 )                ; opt=3
             nAnnual        = dim_num_n(.not.ismissing(xTemp), 0 )  

             if (opt.eq.1) then                               
                 wgtSum = wSum_1
                 if (isleapyear(yyyy(nt))) then
                     wgtSum = wSum_2
                 end if
                 xAnnual(nyr,:) = xAnnual(nyr,:)/wgtSum
             end if
             if (opt.eq.2) then
                 xAnnual(nyr,:) = xAnnual(nyr,:)/12
             end if

             xAnnual(nyr,:) = mask( xAnnual(nyr,:), nAnnual.eq.12, True)
        ;end if
      end do

      xAnnual!0    = "year"
      xAnnual&year =  yyyy(0:ntim-1:12)
      copy_VarCoords_skipDim0 (x, xAnnual)

      if (isatt(x, "long_name")) then
          xAnnual@long_name = x@long_name
      end if
      if (isatt(x, "units")) then
          xAnnual@units     = x@units
      end if

      return(xAnnual)
  end if

  if (rankx.eq.3) then
      dName  = getvardims(x)
      ny     = dimx(1)      ; nlat
      mx     = dimx(2)      ; mlon
      xAnnual = new ( (/nyrs,ny,mx/), typeof(x), getFillValue(x)) ;  contributed

      nyr    = -1
      do nt=0,ntim-1,12
         xTemp = x(nt:nt+11,:,:) 

         nyr   = nyr+1

        ;nMsg = 0
        ;if (isatt(x,"_FillValue")) then
        ;    nMsg = num (ismissing(xTemp) )
        ;end if

        ;if (nMsg.eq.0) then
             wgt              = conform(xTemp, dymon(nt:nt+11), 0)
             xTemp            = xTemp*wgt
             xAnnual(nyr,:,:) = dim_sum_n ( xTemp, 0 )   ; opt=0
             nAnnual          = dim_num_n (.not.ismissing(xTemp), 0 )  

             if (opt.eq.1) then
                 wgtSum = wSum_1
                 if (isleapyear(yyyy(nt))) then
                     wgtSum = wSum_2
                 end if
                 xAnnual(nyr,:,:) = xAnnual(nyr,:,:)/wgtSum
             end if
             if (opt.eq.2) then
                 xAnnual(nyr,:,:) = xAnnual(nyr,:,:)/12
             end if

             xAnnual(nyr,:,:) = mask( xAnnual(nyr,:,:), nAnnual.eq.12, True)
        ;end if
      end do

      xAnnual!0    = "year"
      xAnnual&year =  yyyy(0:ntim-1:12)
      copy_VarCoords_skipDim0 (x, xAnnual)

      if (isatt(x, "long_name")) then
          xAnnual@long_name = x@long_name
      end if
      if (isatt(x, "units")) then
          xAnnual@units     = x@units
      end if

      return(xAnnual)
  end if

  if (rankx.eq.4) then
      dName   = getvardims(x)
      nz      = dimx(1)      ; nlev
      ny      = dimx(2)      ; nlat
      mx      = dimx(3)      ; mlon
      xAnnual = new ( (/nyrs,nz,ny,mx/), typeof(x), getFillValue(x)) ;  contributed

      nyr    = -1
      do nt=0,ntim-1,12
         xTemp = x(nt:nt+11,:,:,:) 

         nyr   = nyr+1

        ;nMsg = 0
        ;if (isatt(x,"_FillValue")) then
        ;    nMsg = num (ismissing(xTemp) )
        ;end if

        ;if (nMsg.eq.0) then
             wgt                = conform(xTemp, dymon(nt:nt+11),0)
             xTemp              = xTemp*wgt
             xAnnual(nyr,:,:,:) = dim_sum_n ( xTemp, 0 )   ; opt=0
             nAnnual            = dim_num_n (.not.ismissing(xTemp), 0 )  

             if (opt.eq.1) then
                 wgtSum = wSum_1
                 if (isleapyear(yyyy(nt))) then
                     wgtSum = wSum_2
                 end if
                 xAnnual(nyr,:,:,:) = xAnnual(nyr,:,:,:)/wgtSum
             end if
             if (opt.eq.2) then
                 xAnnual(nyr,:,:,:) = xAnnual(nyr,:,:,:)/12.
             end if

             xAnnual(nyr,:,:,:) = mask( xAnnual(nyr,:,:,:), nAnnual.eq.12, True)
        ;end if
      end do

      xAnnual!0    = "year"
      xAnnual&year =  yyyy(0:ntim-1:12)
      copy_VarCoords_skipDim0 (x, xAnnual)

      if (isatt(x, "long_name")) then
          xAnnual@long_name = x@long_name
      end if
      if (isatt(x, "units")) then
          xAnnual@units     = x@units
      end if

     if (isatt(yyyymm,"calendar")) then
         xAnnual@calendar = yyyymm@calendar    
     end if

      return(xAnnual)
   end if
end
; *****************************************************************
; D. Shea
; Convert monthly total values [eg, precipitation] to "per day" values.
; Each monthly total is divided by the number of days in the month.
; Leap years use 29 days.
;
; Nomenclature:
;   yyyymm     -  yearMonth   [eg  195602]
;                 Values correspond to the time dimension of "x"
;                 Must be the same size as time dimension of "x".
;   x          -  data array
;                 if rank is greater than one, then it is required that 
;                 the *leftmost* dimension be "time"
;                 eg: prc(time),  prc(time,lat,lon),  prc(time,stations)
;   opt        -  not used currently: set to zero [ 0 ]
;
;   Usage:  prc(time), prc(time,stations) or prc(time,lat,lon)
;           PRC = monthly_total_to_daily_mean( time, prc, 0) ; time is yyyymm
;

undef("monthly_total_to_daily_mean")
function monthly_total_to_daily_mean(yyyymm[*]:numeric, x:numeric, opt:integer)
local yyyy, mm, dymon, DYMON, dimx, rankx, xNew
begin

  dimx  = dimsizes(x)
  if (dimx(0).ne.dimsizes(yyyymm)) then
      print("monthly_total_to_daily_mean: yyyymm and x time dimension must be same size")
      exit
  end if 

  yyyy  = yyyymm/100
  mm    = yyyymm - (yyyy*100)     ; mm=1,...,12
  if (any(mm.lt.1 .or. mm.gt.12)) then
      print("monthly_total_to_daily_mean: mm must be 1-to-12 inclusive")
      exit
  end if

  rankx = dimsizes(dimx)                 ; number of dimensions

  if (isatt(yyyymm,"calendar")) then
      yyyy@calendar = yyyymm@calendar    ; needed for 'day_in_month'
  end if

  dymon = days_in_month ( yyyy, mm)      ; accounts for leap year

  if (rankx.eq.1) then
      xNew  = x/dymon                    ; per day
  else
      DYMON = conform(x, dymon, 0)       ; time is the left dim of x [0]
      xNew  = x/DYMON
  end if

  copy_VarMeta(x, xNew)
  if (isatt(x,"units")) then
      xNew@units = x@units + "/day"
  end if

  return(xNew)
end   
;**************************************************************************
; D Shea
;
; Standard "lossy" approach to compressing (packing) data.  
; The contributed.ncl functions "short2flt", "byte2flt"
; can be used to unpack the values.
;
; NOTE: THE ORIGINAL INPUT VALUES CAN NOT BE RECOVERED.
;
; The actual packing is pretty simple.
; Most of the code is to handle _FillValue and assorted permutations/options

undef("pack_values")
function pack_values(var:numeric, packType:string, opt:logical)  
local vType, one, two, pMax, sFill, vMin, vMax, vRange, vPack  \
    , scale_factor, add_offset, vMinVar, vMaxVar, msgFlag, msgVal, filFlag, filVal 
begin
        vType = typeof(var)
        if (.not.(vType.eq."float" .or. vType.eq."double")) then
            print("pack_values: FATAL:  input must be float or double: vType="+vType) 
            exit
        end if

        if (.not.(packType.eq."short" .or. packType.eq."byte")) then
            print("pack_values: FATAL:  packType must be short or byte: packType="+packType) 
            exit
        end if
                      ; user may input both scale_factor and add_offset
        if (opt .and. (isatt(opt,"scale_factor") .and. .not.isatt(opt,"add_offset"  ))  .or. \
                      (isatt(opt,"add_offset")   .and. .not.isatt(opt,"scale_factor"))) then
            print("pack_values: FATAL:  User must specify BOTH scale_factor and add_offset")
            exit
        end if
                                                  ; ensure that float/double handle correctly
        if (vType.eq."float") then
            one = 1.0
            two = 2.0
        else
            one = 1.0d 
            two = 2.0d
        end if

        if (packType.eq."short") then
            pMax = 2^15 - one
        else
            pMax = 2^7  - one            
        end if

        vMinVar = min(var)                        ; calculate [default]
        if (opt .and. isatt(opt,"min_value") ) then
            vMin = opt@min_value                  ; knowledgable user
            if (vMinVar.lt.vMin) then
                print("pack_values: FATAL: User specified opt@min_value is too high")
                print("pack_values: opt@min_value    = "+opt@min_value)
                print("pack_values: actual min value = "+vMinVar)
                exit
            end if
        else
            vMin = vMinVar
        end if

        vMaxVar = max(var)                       ; calculate [default]
        if (opt .and. isatt(opt,"max_value") ) then
            vMax = opt@max_value                 ; knowledgable user
            if (vMaxVar.gt.vMax) then
                print("pack_values: FATAL: User specified opt@max_value is too low")
                print("pack_values: opt@max_value    = "+opt@max_value)
                print("pack_values: actual max value = "+vMaxVar)
                exit
            end if
        else
            vMax = vMaxVar
        end if
                  
        if (.not.isatt(var,"_FillValue") .and. isatt(var,"missing_value")) then
            var@_FillValue = var@missing_value    ; NCL only understands _FillValue
        end if

        if (isatt(var,"_FillValue")) then
            if (opt .and. isatt(opt,"msgFill") ) then
                if (packType.eq."short") then
                    if (typeof(opt@msgFill).eq."integer") then
                        sFill = inttoshort(opt@msgFill)         ; user specified _FillValue 
                    else
                        sFill = opt@msgFill                     ; must be same as packType
                    end if
                else
                    if (typeof(opt@msgFill).eq."integer") then
                        sFill = inttobyte(opt@msgFill)          ; user specified _FillValue 
                    else
                        sFill = opt@msgFill                     ; must be same as packType
                    end if
                end if
            else
                if (packType.eq."short") then
                    if (vType.eq."float") then
                        sFill = floattoshort (pMax)             ; default _FillValue 
                    else 
                        sFill = doubletoshort(pMax)     
                    end if
                else
                    if (vType.eq."float") then
                        sFill = floattobyte (pMax)              ; default _FillValue
                    else 
                        sFill = doubletobyte(pMax)     
                    end if
                end if
            end if
            vPack = new ( dimsizes(var), packType, sFill)       ; pre-fill with _FillValue
        end if                                                  ; if _FillValue associated var
	
        vRange       = vMax-vMin
	scale_factor = vRange/pMax
	add_offset   = (vMax+vMin)/two
                      
        if (opt .and. isatt(opt,"scale_factor") .and. isatt(opt,"add_offset") ) then
            scale_factor = opt@scale_factor                     ; must be careful
            add_offset   = opt@add_offset                       ;  "   "     "
        end if

        if (packType.eq."short") then
            if (vType.eq."float") then
                vPack = floattoshort((var-add_offset)/scale_factor)  ; pack ... array syntax
            else
                vPack = doubletoshort((var-add_offset)/scale_factor) 
            end if
        else   ; byte                            
            if (vType.eq."float") then
                vPack = floattobyte((var-add_offset)/scale_factor)  ; pack ... array syntax
            else
                vPack = doubletobyte((var-add_offset)/scale_factor) 
            end if
        end if

	copy_VarCoords(var,vPack)              ; copy coordinates

        msgFlag = False
        if (isatt(var,"missing_value")) then
            msgVal = var@missing_value         ; float/double
            delete(var@missing_value)          ; so it will not be copied to new variable
            msgFlag = True
        end if

        filFlag = False
        if (isatt(var,"_FillValue")) then
            filVal = var@_FillValue            ; float/double
            delete(var@_FillValue)             ; so it will not be copied to new variable
            filFlag = True
        end if

	copy_VarAtts(var,vPack)                ; copy attributes but not original
                                               ; missing_value or _FillValue
        if (msgFlag) then
            var@missing_value = msgVal         ; reassign to input variable
            vPack@missing_value = sFill        ; explicitly add
        end if

        if (filFlag) then
            var@_FillValue = filVal            ; reassign to input variable
        end if

	vPack@add_offset   = add_offset
	vPack@scale_factor = scale_factor

        vPack@vMin_original_data   = vMinVar
        if (opt .and. isatt(opt,"min_value") ) then
            vPack@vMin_user_specified  = opt@min_value
        end if

        vPack@vMax_original_data   = vMaxVar
        if (opt .and. isatt(opt,"max_value") ) then
            vPack@vMax_user_specified  = opt@max_value
        end if

	vPack@vRange       = vRange 


	return(vPack)
end
;**************************************************************************
; D Shea
;
; Get the suffix associated with a file. Minor options.
; The attribute "fBase" is the file name without the suffix
;
; Usage:
;       fName  = "sample.1958-2005.nc.gz"
;       suffix = get_file_suffix(fName, 0)  ; ".gz"
;       if (suffix.eq.".gz") then
;           system("gzip -d "+fName) 
;           fileName = suffix@fBase            ; sample.1958-2005.nc
;       end if
;       f = addfile(fileName, "r")
;
;       fName  = "sample.1958-2005.nc.gz"
;       suffix = get_file_suffix(fName, 1)  ; ".1958-2005.gz"
;       fBase  = suffix@fBase               ; sample

undef("get_file_suffix")
function get_file_suffix (filName[1]:string, opt:integer)
local chr, ckey, cstr, N, filName_suffix, nStrt, nLast, n
begin
  chr    = stringtochar(".")  ; dimsizes(chr)=2
  ckey   = chr(0)             ; int 46

  cstr   = stringtochar(filName)
  N      = dimsizes(cstr)     ; one extra for end of char

  filName_suffix = new (1, "string")  ; _FillValue="missing"

  if (opt.eq.0) then
      nStrt = N-2
      nLast = 0
  else
      nStrt = 0
      nLast = N-2
  end if

  do n=nStrt,nLast,1
     if (cstr(n).eq.ckey) then
         filName_suffix        = chartostring(cstr(n:N-2))
         filName_suffix@fBase  = chartostring(cstr(0:n-1))
         break
     end if
  end do

  return(filName_suffix)
end

;**************************************************************************
; D Shea
;
undef("niceLatLon2D")
function niceLatLon2D(lat2d[*][*], lon2d[*][*])
; check map coordinates to see if they have a "nice" structure
;
; if True then 
; the data could be made accessible via classic
; netCDF coordinate array subscripting.
;
; lat      = lat2d(:,0)        
; lon      = lon2d(0,:)       
; lat@units= "degrees_north"  
; lon@units= "degrees_east"
; lat!0    = "lat"
; lon!0    = "lon"
; lat&lat  =  lat 
; lon&lon  =  lon 
;
; assign to a variable

local dimll, nLeft, nRght 
begin
   dimll = dimsizes(lat2d)       ; (south_north,west_east) 
   nLeft = dimll(0)
   nRght = dimll(1)

   if (all(lat2d(:,0).eq.lat2d(:,nRght/2)) .and.  \
       all(lat2d(:,0).eq.lat2d(:,nRght-1)) .and.  \
       all(lon2d(0,:).eq.lon2d(nLeft/2,:)) .and.  \
       all(lon2d(0,:).eq.lon2d(nLeft-1,:)) ) then
       return(True)
   else
       return(False)
   end if
end
;**************************************************************************
; D Shea

undef("isMonotonic")
function isMonotonic(x[*]:numeric)
; check for monoticity; bit of overkill her but that is ok
local nx, i, xx
begin
  if (.not.isatt(x,"_FillValue")) then
      nx   = dimsizes(x)
      if (nx.eq.1) then
          return(0)
      end if

      if (all(x(1:nx-1).gt.x(0:nx-2))) then
          return(1)
      end if
      if (all(x(1:nx-1).lt.x(0:nx-2))) then
          return(-1)
      end if
      return(0)
  end if
                         ; _FillValue attribute is present
  if (all(ismissing(x))) then
      return(-999)
  end if
                         ; at least one value must be present
  i  = ind(.not.ismissing(x))
  xx = x(i)

  nx = dimsizes(xx)
  if (nx.eq.1) then
      return(0)
  end if

  if (all(xx(1:nx-1).gt.xx(0:nx-2))) then
      return(1)
  end if
  if (all(xx(1:nx-1).lt.xx(0:nx-2))) then
      return(-1)
  end if
  return(0)

end
;**************************************************************************
; Contributed by Christine Shields, March 2006.
; Slight mods were made to allow input to be numeric.
; Bug fix: 23 Sept 2009

undef("rho_mwjf")
function rho_mwjf(t2d[*][*]:numeric,s2d[*][*]:numeric,depth:numeric) 

;-- based on Steve Yeager's rhoalphabeta
;-- which in turn is based on POP state_mod.F (ccsm3_0_beta22) for 'mwjf'
;=========================================================================


local dims,nx,ny,c1,c1p5,c2,c3,c4,c5,c10,c1000,p001,mwjfnp0s0t0,mwjfnp0s0t1,\
      mwjfnp0s0t2,mwjfnp0s0t3,mwjfnp0s1t0,mwjfnp0s1t1,mwjfnp0s2t0,mwjfnp1s0t0,\
      mwjfnp1s0t2,mwjfnp1s1t0,mwjfnp2s0t0,mwjfnp2s0t2,mwjfdp0s0t0,mwjfdp0s0t1,\
      mwjfdp0s0t2,mwjfdp0s0t3,mwjfdp0s0t4,mwjfdp0s1t0,mwjfdp0s1t1,mwjfdp0s1t3,\
      mwjfdp0sqt0,mwjfdp0sqt2,mwjfdp1s0t0,mwjfdp2s0t3,mwjfdp3s0t1,pressure,p,\
      sqr,work1,work2,rhofull
begin

;========= define rho
  rhoout = new(dimsizes(t2d),typeof(t2d))

;========== define constants
  c1    = 1.0
  c1p5  = 1.5
  c2    = 2.0
  c3    = 3.0
  c4    = 4.0
  c5    = 5.0
  c10   = 10.0
  c1000 = 1000.0
      ;*** these constants will be used to construct the numerator
      ;*** factor unit change (kg/m^3 -> g/cm^3) into numerator terms
  p001 = 0.001
  mwjfnp0s0t0 =   9.99843699e+2 * p001
  mwjfnp0s0t1 =   7.35212840e+0 * p001
  mwjfnp0s0t2 =  -5.45928211e-2 * p001
  mwjfnp0s0t3 =   3.98476704e-4 * p001
  mwjfnp0s1t0 =   2.96938239e+0 * p001
  mwjfnp0s1t1 =  -7.23268813e-3 * p001
  mwjfnp0s2t0 =   2.12382341e-3 * p001
  mwjfnp1s0t0 =   1.04004591e-2 * p001
  mwjfnp1s0t2 =   1.03970529e-7 * p001
  mwjfnp1s1t0 =   5.18761880e-6 * p001
  mwjfnp2s0t0 =  -3.24041825e-8 * p001
  mwjfnp2s0t2 =  -1.23869360e-11 * p001
      ;*** these constants will be used to construct the denominator
  mwjfdp0s0t0 =   1.0e+0
  mwjfdp0s0t1 =   7.28606739e-3
  mwjfdp0s0t2 =  -4.60835542e-5
  mwjfdp0s0t3 =   3.68390573e-7
  mwjfdp0s0t4 =   1.80809186e-10
  mwjfdp0s1t0 =   2.14691708e-3
  mwjfdp0s1t1 =  -9.27062484e-6
  mwjfdp0s1t3 =  -1.78343643e-10
  mwjfdp0sqt0 =   4.76534122e-6
  mwjfdp0sqt2 =   1.63410736e-9
  mwjfdp1s0t0 =   5.30848875e-6
  mwjfdp2s0t3 =  -3.03175128e-16
  mwjfdp3s0t1 =  -1.27934137e-17

;=====pressure calculaton
; taken from gokhan's idl pressure.pro for references
;     this function computes pressure in bars from depth in meters
;     by using a mean density derived from depth-dependent global
;     average temperatures and salinities from Levitus_94, and
;     integrating using hydrostatic balance.
;
;     references:
;        Levitus, S., R. Burgett, and T.P. Boyer, World Ocean Atlas
;          1994, Volume 3: Salinity, NOAA Atlas NESDIS 3, US Dept. of
;          Commerce, 1994.
;        Levitus, S. and T.P. Boyer, World Ocean Atlas 1994,
;          Volume 4: Temperature, NOAA Atlas NESDIS 4, US Dept. of
;          Commerce, 1994.
;        Dukowicz, J. K., 2000: Reduction of Pressure and Pressure
;          Gradient Errors in Ocean Simulations, J. Phys. Oceanogr.,
;          submitted.

  if(depth.ne.0) then
    pressure = 0.059808*(exp(-0.025*depth) - 1.0)  + 0.100766*depth + \
               2.28405e-7*(depth^2)  
  else
    pressure = 0.
  end if
 
  p = pressure 
; CAS corrected this; verified w/Steve Yeager 9/8/09
; p = pressure only works for potential density; depth/pressure = 0. 
  p = pressure*c10

;========= compute the numerator of the MWFJ density  [P_1(S,T,p)]  

  mwjfnums0t0 = mwjfnp0s0t0 + p*(mwjfnp1s0t0 + p*mwjfnp2s0t0)
  mwjfnums0t1 = mwjfnp0s0t1
  mwjfnums0t2 = mwjfnp0s0t2 + p*(mwjfnp1s0t2 + p*mwjfnp2s0t2)
  mwjfnums0t3 = mwjfnp0s0t3
  mwjfnums1t0 = mwjfnp0s1t0 + p*mwjfnp1s1t0
  mwjfnums1t1 = mwjfnp0s1t1
  mwjfnums2t0 = mwjfnp0s2t0

  work1 = t2d
  work1 = mwjfnums0t0 + t2d * (mwjfnums0t1 + t2d * (mwjfnums0t2 + \
  mwjfnums0t3 * t2d )) + s2d  * (mwjfnums1t0 + \
  mwjfnums1t1 * t2d + mwjfnums2t0 * s2d)

;============= compute the denominator of  MWJF density [P_2(S,T,p)]

  sqr = sqrt(s2d)

  mwjfdens0t0 = mwjfdp0s0t0 + p*mwjfdp1s0t0
  mwjfdens0t1 = mwjfdp0s0t1 + p^3 * mwjfdp3s0t1
  mwjfdens0t2 = mwjfdp0s0t2
  mwjfdens0t3 = mwjfdp0s0t3 + p^2 * mwjfdp2s0t3
  mwjfdens0t4 = mwjfdp0s0t4
  mwjfdens1t0 = mwjfdp0s1t0
  mwjfdens1t1 = mwjfdp0s1t1
  mwjfdens1t3 = mwjfdp0s1t3
  mwjfdensqt0 = mwjfdp0sqt0
  mwjfdensqt2 = mwjfdp0sqt2

  work2 = t2d 
  work2 = mwjfdens0t0 + t2d * (mwjfdens0t1 + t2d  * (mwjfdens0t2 +   \
          t2d * (mwjfdens0t3 + mwjfdens0t4 * t2d ))) + \
          s2d * (mwjfdens1t0 + t2d * (mwjfdens1t1 + t2d*t2d*mwjfdens1t3) + \
          sqr * (mwjfdensqt0 + t2d*t2d*mwjfdensqt2))    

  denomk = work2
  denomk = c1/work2
        
  rhofull = work1
  rhofull = work1*denomk

  rhoout = rhofull

;==== return density

  return (rhoout)   
end

; ******************************************************************
; D. Shea
; append [concatenate] arrays along record dimension 
undef("array_append_record")
function array_append_record (x1, x2, iopt:integer)
local dim_x1, dim_x2, rank_x1, rank_x2, n1, n2, dim_xNew, xNew \
    , errFlag, dimNames_x1, dimNames_x2, dimNames, n           \
    , recCoord, recFlag

begin
                                   ; get array shape/sizes
  dim_x1  = dimsizes(x1)
  dim_x2  = dimsizes(x2)

  rank_x1 = dimsizes(dim_x1)  
  rank_x2 = dimsizes(dim_x2)  

  errFlag = 0                      ; ERROR CHECKING
                                   ; ranks must be equal
  if (rank_x1.ne.rank_x2) then
      print("array_append_record: ranks not equal: rank_x1="+rank_x1+ \
                                                "  rank_x2="+rank_x2)
      errFlag = errFlag + 1
  end if
                                   ; current version only supports
  if (rank_x1.gt.5) then
      print ("array_append_record: currently will only append array of rank 5 or less")
      print ("                     rank="+rank_x1)                                     
      errFlag = errFlag + 1
  end if
                                   ; types must match
  if (typeof(x1).ne.typeof(x2)) then
      print ("array_append_record: arrays must be of the same type")
      print ("                     typeof(x1)="+typeof(x1))                                     
      print ("                     typeof(x2)="+typeof(x2))                                     
      errFlag = errFlag + 1
  end if

  if (rank_x1.gt.1 .and. .not.all(dim_x1(1:).eq.dim_x2(1:))) then
      print ("array_append_record: non-record dimensions must be the same size")
      errFlag = errFlag + 1
  end if
 
  if (errFlag.ne.0) then
      exit
  end if
                                   ; allocate space for new array
  n1          = dim_x1(0)
  n2          = dim_x2(0)
  dim_xNew    = dim_x1
  dim_xNew(0) = n1 + n2
  xNew        = new ( dim_xNew, typeof(x1), getFillValue(x1) )
                                   ; chk _FillValue stuff
  if (.not.isatt(x1,"_FillValue") ) then
      if (isatt(xNew,"_FillValue") ) then
          delete(xNew@_FillValue)
      end if
  end if

  if (isatt(x2,"_FillValue") ) then
      xNew@_FillValue = x2@_FillValue
  end if
                                   ; assign values
  if (rank_x1.eq.1) then
      xNew(0:n1-1)         = (/ x1 /)
      xNew(n1:   )         = (/ x2 /)
  end if

  if (rank_x1.eq.2) then
      xNew(0:n1-1,:)       = (/ x1 /)
      xNew(n1:   ,:)       = (/ x2 /)
  end if

  if (rank_x1.eq.3) then
      xNew(0:n1-1,:,:)     = (/ x1 /)
      xNew(n1:   ,:,:)     = (/ x2 /)
  end if

  if (rank_x1.eq.4) then
      xNew(0:n1-1,:,:,:)   = (/ x1 /)
      xNew(n1:   ,:,:,:)   = (/ x2 /)
  end if

  if (rank_x1.eq.5) then
      xNew(0:n1-1,:,:,:,:) = (/ x1 /)
      xNew(n1:   ,:,:,:,:) = (/ x2 /)
  end if
                                 ; meta data
  copy_VarAtts (x1, xNew)        ; copy attributes
  copy_VarAtts (x2, xNew)        ; may overwrite previous info

  dimNames_x1 = getvardims(x1)   ; dimension names
  dimNames_x2 = getvardims(x2) 
  dimNames    = dimNames_x1      ; default
                                 ; only go here if dimNames are not the same
                                 ; name all dimensions
  do n=0,rank_x1-1
     if (ismissing(dimNames_x1(n)) .and. \
         .not.ismissing(dimNames_x2(n))) then
         dimNames(n) = dimNames_x2(n)
         x1!n = dimNames(n)
     end if
     if (ismissing(dimNames_x1(n)) .and. \
         ismissing(dimNames_x2(n))) then
         dimNames(n) = "dim"+n
         x1!n = "dim"+n
         x2!n = "dim"+n
     end if
  end do

  if (iscoord(x1,dimNames_x1(0)) ) then 
      recCoord = new ( n1+n2, typeof(x1&$dimNames_x1(0)$) )
      if (iscoord(x1,dimNames_x1(0)) ) then 
          recCoord(0:n1-1) = x1&$dimNames_x1(0)$   
      end if
      if (iscoord(x2,dimNames_x2(0)) ) then 
          recCoord(n1:   ) = x2&$dimNames_x2(0)$   
      end if
  end if

  recFlag  = False
  if (isvar("recCoord") .and. .not.all(ismissing(recCoord))) then
      recFlag = True             ; must have coord
     ;delete(recCoord@_FillValue)
  end if
                                 ; assign coordinate variables [if present]
  do n=0,rank_x1-1                    
     xNew!n = dimNames(n)        ; name all dimensions
     if (n.eq.0 .and. recFlag) then 
         xNew&$dimNames(n)$ = recCoord
     else
         if (iscoord(x1,dimNames_x1(n)) ) then 
             xNew&$dimNames(n)$ = x1&$dimNames(n)$   ; right dimensions
         else
             if (iscoord(x1,dimNames_x2(n)) ) then
                 xNew&$dimNames(n)$ = x2&$dimNames(n)$
             end if
         end if
     end if
  end  do

  return (xNew)
end
; ******************************************************************
; D. Shea
; attaches/appends table data: ie (row,column) arrays ; add additional rows
; will add the coordinate variable of the right dimension
; but will not do so for the left dimension. The left dimension
; will be named but no coordinates will be associated with the name.

undef("table_attach_rows")
function table_attach_rows (t1[*][*], t2[*][*], iopt:integer)

local dim_t1, dim_t2, ncol1, ncol2, nrow1, nrow2    \
    , dimNames_t1, dimNames_t2, dimNames, n
begin
                                   ; get array shape/sizes
  dim_t1  = dimsizes(t1)
  dim_t2  = dimsizes(t2)

  ncol1   = dim_t1(1)
  ncol2   = dim_t2(1)

  if (ncol1.ne.ncol2) then
      print ("table_attach_rows: tables must have same number of columns")
      print ("                    ncol1="+ncol1)                                     
      print ("                    ncol2="+ncol2)                                     
      exit
  end if

  if (typeof(t1).ne.typeof(t2)) then
      print ("table_attach_rows: arrays must be of the same type")
      print ("                    typeof(t1)="+typeof(t1)) 
      print ("                    typeof(t2)="+typeof(t2)) 
      exit
  end if
                                   ; allocate space for new array
  nrow1       = dim_t1(0)
  nrow2       = dim_t2(0)

                                   ; chk _FillValue stuff
  if (isatt(t1,"_FillValue") ) then
      tNew = new ( (/nrow1+nrow2, ncol1/), typeof(t1), t1@_FillValue)
  else if (isatt(t2,"_FillValue") ) then
           tNew = new ( (/nrow1+nrow2, ncol1/), typeof(t2), t2@_FillValue)
       else 
           tNew = new ( (/nrow1+nrow2, ncol1/), typeof(t2), "No_FillValue")
       end if
  end if
                                 ; insert values
  tNew(0:nrow1-1,:)   = (/ t1 /)
  tNew(nrow1:   ,:)   = (/ t2 /)
                                 ; meta data
  tNew!0 = "row"                 ; *default* dim names
  tNew!1 = "col" 

  copy_VarAtts (t1, tNew)        ; copy attributes
  copy_VarAtts (t2, tNew)        ; may overwrite previous info

  dimNames_t1 = getvardims(t1)   ; dimension names
  dimNames_t2 = getvardims(t2) 

  do n=0,1                       ; override "row", "col"
     if (.not.ismissing(dimNames_t1(n))) then
         tNew!n = dimNames_t1(n) ; default ... use t1 dim names
     else if (.not.ismissing(dimNames_t2(n))) then
              tNew!n = dimNames_t2(n)    ; use t2 dim names if t1 not present
          end if
     end if
  end do
                                 ; coord for the right dim (n=1) only
                                 ; left dim (n=0) will be named but no coord
                                 ; too many hassles when testing      
  n = 1    
  if (iscoord(t1,dimNames_t1(n)) ) then 
      tNew&$dimNames_t1(n)$ = t1&$dimNames_t1(n)$   ; right dimensions
  else if (iscoord(t2,dimNames_t2(n)) ) then
           tNew&$dimNames_t2(n)$ = t2&$dimNames_t2(n)$
       end if
  end if

  return (tNew)
end

; ******************************************************************
; D. Shea
; appends table data: ie (row,column) arrays
; adds additional columns
;
undef("table_attach_columns")
function table_attach_columns (t1[*][*], t2[*][*], iopt:integer)
local dim_t1, dim_t2, nrow1, nrow2, trows    \
    , dimNames_t1, dimNames_t2, dimNames, n
begin
                                   ; get array shape/sizes
  dim_t1  = dimsizes(t1)
  dim_t2  = dimsizes(t2)

  nrow1   = dim_t1(0)
  nrow2   = dim_t2(0)

  dimNames_t1 = getvardims(t1)     ; dimension names
  dimNames_t2 = getvardims(t2) 

  if (nrow1.ne.nrow2) then
      print ("table_attach_columns: tables must have same number of rows")
      print ("                      nrow1="+nrow1)                                     
      print ("                      nrow2="+nrow2)                                     
      exit
  end if

  if (typeof(t1).ne.typeof(t2)) then
      print ("table_attach_columns: tables must be of the same type")
      print ("                      typeof(t1)="+typeof(t1))  
      print ("                      typeof(t2)="+typeof(t2)) 
      exit
  end if
                        
  if (any(ismissing(dimNames_t1))) then
      if (ismissing(dimNames_t1(0))) then
          t1!0 = "row_1"
          dimNames_t1(0) = t1!0
      end if
      if (ismissing(dimNames_t1(1))) then
          t1!1 = "col_1"
          dimNames_t1(1) = t1!1
      end if
  end if
                        
  if (any(ismissing(dimNames_t2))) then
      if (ismissing(dimNames_t2(0))) then
          t2!0 = "row_2"
          dimNames_t2(0) = t2!0
      end if
      if (ismissing(dimNames_t2(1))) then
          t2!1 = "col_2"
          dimNames_t2(1) = t2!1
      end if
  end if
                                   ; reverse order ... invoke row append
  trows = table_attach_rows(t1($dimNames_t1(1)$|:,$dimNames_t1(0)$|:) \
                           ,t2($dimNames_t2(1)$|:,$dimNames_t2(0)$|:) , 0 )
  dimNames = getvardims(trows)     ; dimension names
                                   ; revert back to original order
  return (trows($dimNames(1)$|:,$dimNames(0)$|:) )
end
;****************************************************************
; D Shea
; requires NCL version a034 or later
;
; Get the indices [subscripts] of the 2D lat/lon arrays
; closest to each LAT/LON coordinate pair.
;
undef("getind_latlon2d")
function getind_latlon2d(lat2d[*][*]:numeric,lon2d[*][*]:numeric \
                        ,LAT[*]:numeric, LON[*]:numeric)
local N, ij, lat1d, lon1d, dist, mndist, indx
begin
  N  = dimsizes( LAT )          
  ij = new ( (/N,2/) , typeof(N))

  lat1d  = ndtooned( lat2d )  
  lon1d  = ndtooned( lon2d )  
  n2d    = dimsizes( lat2d )    

  do n=0,N-1
     dist   = gc_latlon(LAT(n),LON(n),lat1d,lon1d, 2,2)
     mndist = min( dist )
     ind1d  = ind(dist.eq.mndist)
     if (.not.ismissing(ind1d(0))) then
         ij(n,:) = ind_resolve( ind1d(0), n2d )
     else
         print("getind_latlon2d: lat="+ LAT(n)+"  lon="+ LON(n)+" problem")
     end if

     delete(mndist)
     delete(ind1d)
  end do
  ij@long_name = "indices closest to specified LAT/LON coordinate pairs"

  if (.not.any(ismissing(ij))) then
      delete(ij@_FillValue)
  end if
     
  return( ij )
end

;****************************************************************
; D Shea
; Emulate the fortran "mod" function

undef ("mod")
function mod (r1:numeric, r2:numeric) 
; mod function like GNU fortran ; AS ALWAYS: BE CAREFUL MIXING NUMERIC TYPES
local rank_r1, rank_r2, type_r1, type_r2, tmp, R2
begin
  if (any(r2.eq.0)) then
      print("mod: contributed: r2 cannot be zero")
      exit
  end if

  rank_r1 = dimsizes(dimsizes(r1))
  rank_r2 = dimsizes(dimsizes(r2))
  if (rank_r1.ne.rank_r2 .and. rank_r2.gt.1) then
      print("mod: contributed: rank(r1).ne.rank(r2)")
      print("                  rank(r1)= "+rank_r1)
      print("                  rank(r2)= "+rank_r2)
      exit
  end if

  type_r1 = typeof(r1)
  type_r2 = typeof(r2)

  if (type_r1.eq."double") then
      return( r1 - (r2 * toint( r1/r2) ) ) 
  end if  

  if (type_r1.eq."float") then
      if (type_r2.eq."float" .or. type_r2.eq."integer") then
          return( r1 - (r2 * toint( r1/r2 ) ))
      end if     

      if (type_r2.eq."double") then
          tmp = doubletofloat( r1 - (r2 * toint( r1/r2 ) ))
          return( tmp )
      end if
  end if  

  if (type_r1.eq."integer") then
      if (type_r2.eq."integer") then
          return( r1 % r2 ) 
      end if
      if (type_r2.eq."float") then
          R2 = toint(r2)
          return( r1 - toint(R2 * (r1/R2)))
      end if
      if (type_r2.eq."double") then
          R2 = toint(r2)
          return( r1 - toint(R2 * (r1/R2)))
      end if
  end if

end

;-------------------------------------------------------------
; find indices corresponding to closest distance   
; to a coordinate array [ie: 1D mononic array]
;
undef("ind_nearest_coord")
function ind_nearest_coord ( z[*]:numeric, zgrid[*]:numeric, iopt:integer) 
local n, nz, iz, zz, mnzz, imn
begin
  nz  = dimsizes(z)
  iz  = new(nz, "integer", "No_FillValue")
  do n=0,nz-1                  ; loop over each value
     zz    = abs(z(n)-zgrid)   ; distances
     mnzz  = min( zz )         ; min distance
     imn   = ind(zz.eq.mnzz)   ; index on min distance: may be more than one 
     iz(n) = imn(0)            ; select only the 1st one
     delete(imn)
  end do

  return(iz)
end

;****************************************************************
; Christophe Cassou [CERFACS, Toulouse CEDEX France] and Dennis Shea
; Generate unique random subscript indices
;
; As of 6.0.0, N can be integer or long
;
undef("generate_unique_indices")
function generate_unique_indices( N )
local r
begin
  if(.not.any(typeof(N).eq.(/"integer","long"/))) then
    print("generate_unique_indices: N must be integer or long")
    exit
  end if
  r   = random_uniform(0,100,N)
  return( dim_pqsort(r, 1) )
end
; **********************************************************************
; D. Shea
;
undef("isEqualSpace")
function isEqualSpace(z[*]:numeric, epsz[1]:numeric)
local nz, dz, dz_low, dz_high, diff
begin
  nz = dimsizes(z)
  if (nz.le.1) then
      print("is_equal_space: input array  must be > 2")
      exit
  end if
  dz     = abs(z(1)-z(0))             ; error check 
  dz_low = dz-epsz
  dz_hgh = dz+epsz
  diff   = all(abs(z(1:nz-1)-z(0:nz-2)) .ge. dz_low) .and. \
           all(abs(z(1:nz-1)-z(0:nz-2)) .le. dz_hgh)      
  return (diff)
end
; ---
undef("isConstantSpace")
; old name
function isConstantSpace(z[*]:numeric, epsz[1]:numeric)
begin
  return(isEqualSpace(z,epsz))
end

; **********************************************************************
; D. Shea
; wrapper for NCL procedure "area_hi2lores": copies attributes and coordinate 
; vars.  It adds the longitude and latitude coordinates.
;
undef ("area_conserve_remap_Wrap")
function area_conserve_remap_Wrap (xi[*]:numeric,yi[*]:numeric, fi:numeric \
                                  ,xo[*]:numeric,yo[*]:numeric, opt)

; wrapper for NCL function "area_conserve_remap" 
; that copies attributes and coordinate vars

local fo, dimfi, nDim, nD
begin
  fo   = area_conserve_remap (xi,yi,fi, xo,yo, opt)  ; perform interp
                                                ; shea_misc functions
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fi))                 ; number of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  copy_VarCoords_2 (fi, fo)                     ; copy coord variables  

  fo!(nDim-2) = "Y"                             ; default named dimensions
  fo!(nDim-1) = "X"
                                                ; override if possible
  if (isdimnamed(xo,0) .and. isdimnamed(yo,0) ) then
      fo!(nDim-2) = yo!0                        ; if present, use xo name
      fo!(nDim-1) = xo!0                        ; if present, use xo name
  else 
      do nD=nDim-2,nDim-1                       ; two rightmost dimensions
         if (.not.ismissing(fi!nD)) then
            ;fo!nD = changeCaseChar(fi!nD)      ; if present, use same name
             fo!nD = str_switch(fi!nD)          ; if present, use same name
         end if                                 ; but change case
      end do
  end if

  fo&$fo!(nDim-2)$ = yo                         ; create coordinate var 
  fo&$fo!(nDim-1)$ = xo                         ; two rightmost dimensions

  return (fo)
end

; **********************************************************************
; D. Shea
; wrapper for NCL procedure "area_hi2lores": copies attributes and coordinate 
; vars.  It adds the longitude and latitude coordinates.

undef ("area_hi2lores_Wrap")
function area_hi2lores_Wrap (xi[*]:numeric,yi[*]:numeric, fi:numeric, wrapX:logical \
                      ,wy[*]:numeric,xo[*]:numeric,yo[*]:numeric, Opt)

; wrapper for NCL function "area_hi2lores"  that copies attributes and coordinate vars

local fo, dimfi, nDim, nD
begin
  fo   = area_hi2lores (xi,yi,fi, wrapX, wy, xo,yo, Opt)  ; perform interp
                                                ; shea_misc functions
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fi))                 ; number of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  copy_VarCoords_2 (fi, fo)                     ; copy coord variables  

  fo!(nDim-2) = "Y"                             ; default named dimensions
  fo!(nDim-1) = "X"
                                                ; override if possible
  if (isdimnamed(xo,0) .and. isdimnamed(yo,0) ) then
      fo!(nDim-2) = yo!0                        ; if present, use xo name
      fo!(nDim-1) = xo!0                        ; if present, use xo name
  else 
      do nD=nDim-2,nDim-1                       ; two rightmost dimensions
         if (.not.ismissing(fi!nD)) then
            ;fo!nD = changeCaseChar(fi!nD)      ; if present, use same name
             fo!nD = str_switch(fi!nD)          ; if present, use same name
         end if                                 ; but change case
      end do
  end if

  fo&$fo!(nDim-2)$ = yo                         ; create coordinate var 
  fo&$fo!(nDim-1)$ = xo                         ; two rightmost dimensions

  return (fo)
end
; **********************************************************************
; D. Shea
; Generate [sin(lat+dlat/2)-sin(lat-dlat/2)] weights 
; for equally spaced grids. Currently, only global grids are supported.
;
undef("latRegWgt")
function latRegWgt(lat[*]:numeric, nType[1]:string, opt[1]:integer)
; usage: wgt = latRegWgt(lat, "double", 0)
;        wgt = latRegWgt(lat, "float" , 0)
local nlat, pi, rad, err, eps, dlat, dlat_low, dlat_hgh, diff, nl, dNam
begin
  nlat = dimsizes(lat)
  pi   = 4.0d*atan(1.0d)
  rad  = pi/180.0d
  err  = 1d20
  eps  = 0.001                          ; arbitrary

  dlat = abs(lat(2)-lat(1))             ; error check
  dlat_low = dlat-eps
  dlat_hgh = dlat+eps
  diff     = all(abs(lat(1:nlat-1)-lat(0:nlat-2)) .ge. dlat_low) .and. \
             all(abs(lat(1:nlat-1)-lat(0:nlat-2)) .le. dlat_hgh)        
  if (.not.diff) then
      print("latRegWgt: Expecting equally spaced latitudes")
      if (nType.eq."double") then
          return(new(nlat,"double",err))
      else
          return(new(nlat,"float",doubletofloat(err)))
      end if
  end if
  delete(dlat)

  dlat = abs((lat(2)-lat(1))*rad)*0.5d

  w    = new (nlat, "double", "No_FillValue")

  do nl=0,nlat-1
     w(nl) = abs( sin(lat(nl)*rad+dlat) - sin(lat(nl)*rad-dlat))
  end do
                                        ; poles
  if (abs(lat(0)).gt.89.9999d) then
      nl = 0
    ;;w(nl) = abs( sin(lat(nl)*rad)- sin(lat(nl)*rad-dlat))
    ;;weight_pole = abs ( 1. - sin(pi/2 - (Delta_phi)/2 )  ; CJ
      w(nl) = abs ( 1d0 - sin(pi/2d0 - dlat))  ; CJ
  end if
  if (abs(lat(nlat-1)).gt.89.9999d) then
      nl = nlat-1
    ;;w(nl) = abs( sin(lat(nl)*rad)- sin(lat(nl)*rad-dlat))
    ;;weight_pole = abs ( 1. - sin(pi/2 - (Delta_phi)/2 )  ; CJ
      w(nl) = abs ( 1d0 - sin(pi/2d0 - dlat))  ; CJ
  end if

  dNam = getvardims( lat )
  if (.not.ismissing(dNam)) then
      w!0 = dNam
      if (iscoord(lat, dNam)) then
          w&$dNam$ = lat
      end if
  end if
  w@long_name = "latitude weight"

  if (nType.eq."double") then
      return( w )
  else
      return(dble2flt(w))
  end if
end

; **********************************************************************
; D. Shea
undef("quadroots")
function quadroots(a[1]:numeric, b[1]:numeric, c[1]:numeric)
; solve quadratic formula
local x, d, droot, dble, two, con, D
begin
  if (typeof(a).eq."double" .or. typeof(b).eq."double" \
                            .or. typeof(c).eq."double" ) then
      d    = 0.0d
      con  = 2.0d*a
      dble = True
  else
      d    = 0.0
      con  = 2.0*a
      dble = False
  end if

  d = b^2 - 4*a*c                      ; discriminant

  if (d.ge.0) then                     ; positive roots
      if (dble) then
          x    = (/0.0d,0.0d,0.0d0/)
      else
          x    = (/0.0 , 0.0, 0.0 /)
      end if

      if (d.gt.0) then                 ; two distinct real roots
          droot = sqrt(d)
          x(0)  = (-b + droot)/con
          x(1)  = (-b - droot)/con
      else              
          x     = -b/con               ; one distinct root  
      end if                           ; return as double root

      x@root         = "real"
      x@discriminant = d
     ;x@result1      = a*x(0)^2 + b*x(0) + c
     ;x@result2      = a*x(1)^2 + b*x(1) + c
      return (x)
  end if

  D      = sqrt(-d)/con                ; (4*a*c -b^2)/2a

  x = new ( 3, typeof(d), "No_FillValue")
  x(0) = -b/con                        ; real part
                                       ; imaginary parts
  x(1) =  D                            ; positive 
  x(2) = -D                            ; negative 

  x@root         = "complex"
  x@discriminant = d
  return (x)
end

; **********************************************************************
; Contributed by Carl J. Schreck, III, July 2008
;
; Converts a time variable from one units to another. The input
; variable must contain a "units" attribute of the correct form.
;
; Input variables:
;   dateFrom: the original date
;   unitsTo:  the NEW date units
; Return Value:
;   retVal:   the date converted to its new units
;***********************************************************************
undef("ut_convert")
function ut_convert( dateFrom:numeric, unitsTo:string )
local retVal, tempDate, utcDate
begin
 if(.not.isatt(dateFrom,"units")) then
   print("ut_convert: 'dateFrom' contains no 'units' attribute.")
   print("            Will return all missing values.")
  
   retVal = new(dimsizes(dateFrom),double)
   return(retVal)
 end if

 tempDate       = dateFrom
 tempDate@units = dateFrom@units

 utcDate = ut_calendar( tempDate, -5 )

 opt = 0
 if(isatt(dateFrom,"calendar")) then
   opt@calendar = dateFrom@calendar
 end if

 retVal = ut_inv_calendar( toint(utcDate(:,0)), toint(utcDate(:,1)), toint(utcDate(:,2)), \
                           toint(utcDate(:,3)), toint(utcDate(:,4)), utcDate(:,5), \
                           unitsTo, opt )
 return( retVal )
end

; **********************************************************************
; This is identical to ut_convert, except it calls a fixed 
; version of ut_calendar that is based on D. Pierce's Calcalcs routine.
;
; Converts a time variable from one units to another. The input
; variable must contain a "units" attribute of the correct form.
;
; Input variables:
;   dateFrom: the original date
;   unitsTo:  the NEW date units
; Return Value:
;   retVal:   the date converted to its new units
;***********************************************************************
undef("ut_convert_fix")
function ut_convert_fix( dateFrom:numeric, unitsTo:string )
local retVal, tempDate, utcDate
begin
 if(.not.isatt(dateFrom,"units")) then
   print("ut_convert_fix: 'dateFrom' contains no 'units' attribute.")
   print("            Will return all missing values.")
  
   retVal = new(dimsizes(dateFrom),double)
   return(retVal)
 end if

 tempDate       = dateFrom
 tempDate@units = dateFrom@units

 utcDate = ut_calendar_fix( tempDate, -5 )

 opt = 0
 if(isatt(dateFrom,"calendar")) then
   opt@calendar = dateFrom@calendar
 end if

 retVal = ut_inv_calendar_fix( toint(utcDate(:,0)), toint(utcDate(:,1)), toint(utcDate(:,2)), \
                               toint(utcDate(:,3)), toint(utcDate(:,4)), utcDate(:,5), \
                               unitsTo, opt )
 return( retVal )
end

; **********************************************************************
; This is identical to ut_convert, except it uses cd_calendar.
;***********************************************************************
undef("cd_convert")
function cd_convert( dateFrom:numeric, unitsTo:string )
local retVal, tempDate, utcDate
begin
 if(.not.isatt(dateFrom,"units")) then
   print("cd_convert: 'dateFrom' contains no 'units' attribute.")
   print("            Will return all missing values.")
  
   retVal = new(dimsizes(dateFrom),double)
   return(retVal)
 end if

 tempDate       = dateFrom
 tempDate@units = dateFrom@units

 utcDate = cd_calendar( tempDate, 0 )

 opt = 0
 if(isatt(dateFrom,"calendar")) then
   opt@calendar = dateFrom@calendar
 end if

 retVal = cd_inv_calendar( toint(utcDate(:,0)), toint(utcDate(:,1)), toint(utcDate(:,2)), \
                           toint(utcDate(:,3)), toint(utcDate(:,4)), utcDate(:,5), \
                           unitsTo, opt )
 return( retVal )
end

; **********************************************************************
; D. Shea
; wrapper for NCL function "triple2grid"  that copies attributes and coordinate
; vars.  It adds the longitude and latitude coordinates.

undef ("triple2grid_Wrap")
function triple2grid_Wrap (xi[*]:numeric,yi[*]:numeric, fi:numeric \
                          ,xo[*]:numeric,yo[*]:numeric, Opt)

; wrapper for NCL function "triple2grid"  that copies attributes and coordinate vars

local fo, dimfi, nDim, nD
begin
  fo    = triple2grid (xi,yi,fi, xo,yo, Opt)     ; builtin
  dimfi = dimsizes(fi)
  nDimi = dimsizes(dimsizes(fi))                 ; number of in dimensions [rank]
  nDimo = dimsizes(dimsizes(fo))

  copy_VarAtts (fi, fo)                         ; copy variable attributes

  fo!(nDimo-2) = "Y"                               ; default named dimensions
  fo!(nDimo-1) = "X"
                                             ; override if possible
  if (isdimnamed(yo,0) ) then
      fo!(nDimo-2)    = yo!0                   ; if present, use xo name
      fo&$fo!(nDimo-2)$ = yo                     ; create coordinate var
  end if
  if (isdimnamed(xo,0)) then
      fo!(nDimo-1)    = xo!0                   ; if present, use xo name
      fo&$fo!(nDimo-1)$ = xo                     ; two rightmost dimensions
  end if

  if (nDimo.ge.2) then
      copy_VarCoords_1 (fi,fo)
  end if

  return (fo)
end
;**************************************************************
; Calculate the PDF of an array
; Original source IDL code by Andrew Gettleman
; pdfx_v510 is to provide backward compatibility for the 5.1.0 version
;
undef("pdfx_v510")
function pdfx_v510(x:numeric, nbin[1]:integer, opt:logical)
local nGood, nbins, xMin, xMax, mnmxint, xSpace \
     ,bin, pdf, nTot
begin
  nGood = num(.not.ismissing(x))
  if (nGood.lt.3) then
      print("pdfx: nGood="+nGood+" : Need more non-missing points")
      exit
  end if

  if (nbin.le.2) then
      nbins = 50             ; default
  else
      nbins = nbin
  end if

  xMin = 0.0d     ; not required but done for test
  xMax = 0.0d

  if (opt .and. isatt(opt,"bin_min")) then  
      xMin = opt@bin_min                    ; user set
  else
      xMin = min(x)                         ; calculate
  end if

  if (opt .and. isatt(opt,"bin_max")) then  
      xMax = opt@bin_max                    ; user set
  else
      xMax = max(x)                         ; calculate
  end if

  if (opt .and. isatt(opt,"bin_nice")) then ; nice xMin, xMax
      outside = False
      if (isatt(opt,"bin_nice_outside")) then
          outside = opt@bin_nice_outside
      end if
      mnmxint = nice_mnmxintvl( min(x), max(x), nbins, outside)
      xMin    = mnmxint(0)
      xMax    = mnmxint(1)
      xSpace  = mnmxint(2)
      nbins   = round( (xMax-xMin)/xSpace , 3) 
  end if

  binBound    = fspan(xMin,xMax,nbins+1)
  pdf         = new( nbins, "double", getFillValue(x))
  binCenter   = (binBound(0:nbins-1) + binBound(1:nbins))*0.5d 
  pdf         = 0.0d

  do nb=0,nbins-2
     pdf(nb) = num( x.ge.binBound(nb) .and. x.lt.binBound(nb+1) )
  end do

  nTot = num(x.ge.xMin .and. x.le.xMax)   ; actual number used
  pdf  = pdf/nTot              ; frequency

  pdf@bin_center     = binCenter
  pdf@bin_bounds     = binBound
  pdf@bin_bound_min  = min(binBound)
  pdf@bin_bound_max  = max(binBound)
  pdf@bin_spacing    = binBound(2)-binBound(1)
  pdf@nbins          = nbins

  pdf@long_name      = "PDF"
  if (isatt(x,"long_name")) then
      pdf@long_name  = "PDF: "+x@long_name
  end if
  pdf@units          = "frequency"
  
  return( pdf )
end
;***********************************************
; binning consistent with the "pdfxy" function
; Feb 2012: replaced NCL loop with fortran loop
; in pdfx_bin. This is much faster.
;
undef("pdfx")
function pdfx(x:numeric, nbin[1]:integer, opt:logical)
local nGood, nbins, xMin, xMax, mnmxint, xSpace \
     ,bin, pdf, nTot, XMIN, XMAX, nLoOut, nHiOut
begin

  if (opt .and. isatt(opt, "v510") .and. opt@v510) then
      pdf = pdfx_v510(x, nbin, opt) ; different binning
      return(pdf)
  end if

  if (nbin.le.2) then
      nbins = 25             ; default 
  else
      nbins = nbin
  end if

  nGood = num(.not.ismissing(x))
  if (nGood.lt.3) then
      print("pdfx: nGood="+nGood+" : Need more non-missing points")
      pdf = new( nbins, "double", getFillValue(x))
      return( pdf )
  end if

  XMIN = min(x)*1d0   ; min/max for ENTIRE array             
  XMAX = max(x)*1d0   ; force "double" [*1d0]

  xMin = 0.0d         ; prototype as double
  xMax = 0.0d

  if (opt .and. isatt(opt,"bin_min")) then  
      xMin = opt@bin_min                    ; user set
  else
      xMin = XMIN                           ; calculate
  end if

  if (opt .and. isatt(opt,"bin_max")) then  
      xMax = opt@bin_max                    ; user set
  else
      xMax = XMAX                           ; calculate
  end if

  if (opt .and. isatt(opt,"bin_nice")) then ; nice xMin, xMax
      outside = False
      if (isatt(opt,"bin_nice_outside")) then
          outside = opt@bin_nice_outside
      end if
      mnmxint = nice_mnmxintvl( XMIN, XMAX, nbins, outside)
      xMin    = mnmxint(0)
      xMax    = mnmxint(1)
      xSpace  = mnmxint(2)
    ;;nbins   = round( (xMax-xMin)/xSpace , 3) 
  end if

;;dbin        = (xMax-xMin)/(nbins-1)          ; 5.2.0   
  dbin        = (xMax-xMin)/nbins   
  binBound    = xMin + ispan(0,nbins,1)*dbin
  binBound(nbins) = xMax                       ; avoid roundoff              
  binCenter   = (binBound(0:nbins-1) + binBound(1:nbins))*0.5d 

  binBoundMin = binBound(0)           
  binBoundMax = binBound(nbins)      

  pdf         = new( nbins, "double", getFillValue(x))
  pdf         = 0.0d
  
;;---- the following replaces the ;; NCL loop below ---
  popt          = True
  popt@fraction = True ; True mean total count (**DO NOT CHANGE**)
  pdf = pdfx_bin(x, binBound, popt) ; fortran

;;do nb=0,nbins-1      
;;   pdf(nb) = num( x.ge.binBound(nb) .and. x.lt.binBound(nb+1) )
;;   if (nb.eq.(nbins-1)) then                 ; last bin 
;;       pdf(nb) = pdf(nb) + num( x.eq.binBound(nb+1) )   ; include last bound
;;   end if
;;end do
;;----

  pdf!0    = "x"
  pdf&x    = binCenter
                                       ; max possible in data     
  nMax     = num(x.ge.XMIN .and. x.le.XMAX)
                                       ; actual number used
  nUse     = num(x.ge.binBoundMin .and. x.le.binBoundMax)

  nLoOut   = num(x.lt.binBoundMin)     ; number outliers
  nHiOut   = num(x.gt.binBoundMax)

  pdf      = 100d0*pdf/nMax            ; percent frequency

  pdf@bin_center     = binCenter
  pdf@bin_bounds     = binBound
  pdf@bin_bound_min  = binBoundMin
  pdf@bin_bound_max  = binBoundMax
  pdf@bin_spacing    = dbin            ; binBound(2)-binBound(1)
  pdf@nbins          = nbins
  pdf@nMax           = nMax
  pdf@nUse           = nUse
  if (nLoOut.gt.0 .or. nHiOut.gt.0) then
      pdf@nLoOut     = nLoOut       
      pdf@nHiOut     = nHiOut
  end if

  pdf@long_name      = "PDF"
  if (isatt(x,"long_name")) then
      pdf@long_name  = "PDF: "+x@long_name
  end if
  pdf@units          = "%"
  
  return( pdf )
end
;**************************************************************
; Calculate the "joint PDF [%]" of arrays "x" and "y"
;**************************************************************
undef("pdfxy")
function pdfxy(x:numeric, y:numeric, nbinx[1]:integer, nbiny[1]:integer, opt:logical)
local nGood_x, nbinsx, xMin, xMax, mnmxint, binx, binxBound, binxCenter, nbx \
     ,nGood_y, nbinsy, yMin, yMax, ySpace , biny, binyBound, binyCenter, nby \
     ,outside, mnmxint, pdf2, nTot, dimx, dimy, rankx, ranky, x1d, y1d, iy, warnFlag\
     ,xMIN, xMAX, yMIN, yMAX, binxBoundMin, binxBoundMax, binyBoundMin, binyBoundMax\
     ,nMax, nUse, nxLoOut, nxHiOut, nyLoOut, nyHiOut, popt, fmsg, epsx, epsy
begin

  fmsg = default_fillvalue("float")

  if (nbinx.le.2) then
      nbinsx = 25             ; default
  else
      nbinsx = nbinx          ; user sprecified
  end if

  if (nbiny.le.2) then
      nbinsy = 25             ; default
  else
      nbinsy = nbiny          ; user specified
  end if
                              ; error check
  warnFlag = True             ; default ... print warning messages
  if (opt .and. isatt(opt,"WarningMsg") .and. .not.opt@WarningMsg) then
      warnFlag = False
  end if

  if (warnFlag) then
      nGood_x = num(.not.ismissing(x))
      if (nGood_x.lt.3) then
          print("pdfxy: nGood(x)="+nGood_x+" : Need more non-missing points")
      end if
    
      nGood_y = num(.not.ismissing(y))
      if (nGood_y.lt.3) then
          print("pdfxy: nGood(y)="+nGood_y+" : Need more non-missing points")
      end if
    
      if (nGood_x.lt.3 .or. nGood_y.lt.3) then
          print("pdfxy: exit error: not enough points")
          pdf2 = new( (/nbinsy,nbinsx/), "double", 1d20)
          return( pdf2 )
      end if
  end if              ; warnFlag

  xMIN = min(x)*1d0   ; min/max for ENTIRE array             
  xMAX = max(x)*1d0   ; force "double" [*1d0] ... convenience only
  yMIN = min(y)*1d0   
  yMAX = max(y)*1d0  
                      ; max number possible (excludes _FillValue)
  nMax = num(x.ge.xMIN .and. x.le.xMAX .and. \
             y.ge.yMIN .and. y.le.yMAX) 

  if (nMax.lt.3) then                        
      print("pdfxy: exit error: nMax.lt.3")
      pdf2 = new( (/nbinsy,nbinsx/), "double", 1d20)
      return( pdf2 )
  end if

  xMin = 0.0d         ; prototype as "double"    
  xMax = 0.0d
  yMin = 0.0d     
  yMax = 0.0d
                      ; User may want different min/max used
  if (opt .and. isatt(opt,"binx_min")) then  
      xMin = opt@binx_min                   ; user
  else
      xMin = xMIN                           ; calculated
  end if

  if (opt .and. isatt(opt,"binx_max")) then  
      xMax = opt@binx_max                   ; user
  else
      xMax = xMAX                                         
  end if

  if (opt .and. isatt(opt,"biny_min")) then  
      yMin = opt@biny_min                   ; user
  else
      yMin = yMIN
  end if

  if (opt .and. isatt(opt,"biny_max")) then  
      yMax = opt@biny_max                   ; user
  else
      yMax = yMAX  
  end if
                                            
  if (opt.and.isatt(opt,"binx_nice")) then  ; nice xMin, xMax
      outside = False                       
      if (isatt(opt,"binx_nice_outside")) then
          outside = opt@binx_nice_outside
      end if
      mnmxint = nice_mnmxintvl( xMIN, xMAX, nbinsx, outside)
      xMin    = mnmxint(0)
      xMax    = mnmxint(1)
      xSpace  = mnmxint(2)
    ;;nbinsx  = round( (xMax-xMin)/xSpace , 3) 
      delete(mnmxint)
  end if

  if (opt .and. isatt(opt,"biny_nice")) then ; nice yMin, yMax
      outside = False
      if (isatt(opt,"biny_nice_outside")) then
          outside = opt@biny_nice_outside
      end if
      mnmxint = nice_mnmxintvl( yMIN, yMAX, nbinsy, outside)
      yMin    = mnmxint(0)
      yMax    = mnmxint(1)
      ySpace  = mnmxint(2)
    ;;nbinsy  = round( (yMax-yMin)/ySpace , 3) 
      delete(mnmxint)
  end if

;;dbinx       = (xMax-xMin)/(nbinsx-1)
  dbinx       = (xMax-xMin)/nbinsx
  binxBound   = xMin + ispan(0,nbinsx,1)*dbinx
  binxBound(nbinsx) = xMax                       ; avoid roundoff
  binxCenter  = (binxBound(0:nbinsx-1) + binxBound(1:nbinsx))*0.5d 

;;dbiny       = (yMax-yMin)/(nbinsy-1)
  dbiny       = (yMax-yMin)/nbinsy
  binyBound   = yMin + ispan(0,nbinsy,1)*dbiny
  binyBound(nbinsy) = yMax                       ; avoid roundoff
  binyCenter  = (binyBound(0:nbinsy-1) + binyBound(1:nbinsy))*0.5d 

  binxBoundMin= binxBound(0)          ; convenience
  binxBoundMax= binxBound(nbinsx)   
  binyBoundMin= binyBound(0)   
  binyBoundMax= binyBound(nbinsy)   

  pdf2        = new( (/nbinsy,nbinsx/), "double", 1d20)
  pdf2        = 0.0d                  ; initialize
  
  dimx        = dimsizes(x)
  dimy        = dimsizes(y)
  rankx       = dimsizes(dimx)
  ranky       = dimsizes(dimy)

  popt          = True
  popt@fraction = False     ; False means to return %
  
                      ; -------------------------------------------
                      ; epsx, epsy:  Nov 2013 for v6.2.0
		      ; get around:  LastLeftBin <= x,y < LastRightBin 
		      ; Add eps LastRightBin will *include* xMAX and yMAX
  if (opt .and. isatt(opt, "bin_max_epsx")) then
      epsx = opt@bin_max_epsx ; set to 0.0 for pre 6.2.0 results
  else
      epsx = 1d-6             ; default (arbitrary)
  end if
                      
  if (opt .and. isatt(opt, "bin_max_epsy")) then
      epsy = opt@bin_max_epsy
  else
      epsy = 1d-6         
  end if
                    
  binxBound(nbinsx)= binxBound(nbinsx) + epsx*binxBound(nbinsx)
  binyBound(nbinsy)= binyBound(nbinsy) + epsy*binyBound(nbinsy)
                      ; ------------------------------------------- 

  pdf2 = pdfxy_bin(x,y, binxBound,binyBound, popt)
                          ; all NCL [original code]
    ;;x1d  = ndtooned(x)  ; requires more memory
    ;;y1d  = ndtooned(y)

    ;;do nby=0,nbinsy-1
    ;;   iy = ind(y1d.ge.binyBound(nby) .and. y1d.lt.binyBound(nby+1))
    ;;   if (.not.ismissing(iy(0))) then
    ;;       do nbx=0,nbinsx-1
    ;;          pdf2(nby,nbx) = num(x1d(iy).ge.binxBound(nbx) .and. \
    ;;                              x1d(iy).lt.binxBound(nbx+1)     )
    ;;       end do
    ;;   end if
    ;;   delete(iy)      ; size may change
    ;;end do

  if (opt .and. isatt(opt,"fraction") .and. opt@fraction) then
      pdf2 = pdf2/1d2                ; fraction [0,1]
  end if

  pdf2!0   = "y"         ; arbitrary name
  pdf2!1   = "x"
  pdf2&x   =  binxCenter 
  pdf2&y   =  binyCenter
                         ; actual number used, excludes outliers
					     
  nUse     = num(x.ge.binxBoundMin .and. x.le.binxBoundMax .and. \
                 y.ge.binyBoundMin .and. y.le.binyBoundMax)

  if (nMax.eq.nUse) then
      nxLoOut  = 0
      nxHiOut  = 0
      nyLoOut  = 0
      nyHiOut  = 0
  else
      nxLoOut  = num(x.lt.binxBoundMin)
      nxHiOut  = num(x.ge.binxBoundMax)
      nyLoOut  = num(y.lt.binyBoundMin)
      nyHiOut  = num(y.ge.binyBoundMax)
  end if

  pdf2@nMax            = nMax
  pdf2@nUse            = nUse
  pdf2@nOut            = nxLoOut + nxHiOut + nyLoOut + nyHiOut
  if (nMax.gt.0) then
      pdf2@pcUse       = (1e2*nUse)/nMax         ; percent [pc]
      pdf2@pcOut       = (1e2*pdf2@nOut)/nMax
  else
      pdf2@pcUse       = fmsg
      pdf2@pcOut       = fmsg
  end if

  pdf2@xMIN            = xMIN 
  pdf2@xMAX            = xMAX 
  pdf2@binx_center     = binxCenter
  pdf2@binx_bounds     = binxBound
  pdf2@binx_bound_min  = binxBoundMin
  pdf2@binx_bound_max  = binxBoundMax
  pdf2@binx_spacing    = dbinx   ; binxBound(2)-binxBound(1)
  pdf2@nbinsx          = nbinsx
 ;pdf2@nxLoOut         = nxLoOut
 ;pdf2@nxHiOut         = nxHiOut
  
  if (nMax.gt.0) then
      pdf2@pcxLoOut    = (1e2*nxLoOut)/nMax         ; %
      pdf2@pcxHiOut    = (1e2*nxHiOut)/nMax
  else
      pdf2@pcxLoOut    = fmsg
      pdf2@pcxHiOut    = fmsg
  end if

  pdf2@yMIN            = yMIN 
  pdf2@yMAX            = yMAX 
  pdf2@biny_center     = binyCenter
  pdf2@biny_bounds     = binyBound
  pdf2@biny_bound_min  = binyBoundMin
  pdf2@biny_bound_max  = binyBoundMax
  pdf2@biny_spacing    = dbiny   ; binyBound(2)-binyBound(1)
  pdf2@nbinsy          = nbinsy
 ;pdf2@nyLoOut         = nyLoOut
 ;pdf2@nyHiOut         = nyHiOut
  if (nMax.gt.0) then
      pdf2@pcyLoOut        = (1e2*nyLoOut)/nMax         ; %
      pdf2@pcyHiOut        = (1e2*nyHiOut)/nMax
  else
      pdf2@pcyLoOut        = fmsg
      pdf2@pcyHiOut        = fmsg
  end if

 ;pdf2@long_name      = "Joint PDF"
 ;if (isatt(x,"long_name") .and. isatt(y,"long_name")) then
 ;    pdf2@long_name  = "Joint PDF: "+x@long_name+" | " \
 ;                                   +y@long_name
 ;end if
 ;pdf2@units          = "frequency"
  
  return( pdf2 )
end
; ----
undef("pdfxy_conform")
function pdfxy_conform(x,y,nbinx,nbiny,opt)  ; same arguments as pdfxy
;
; Work around to allow x and y to be different sizes
; Same arguments as pdfxy

local N, M, X, Y, XX, YY, pdf2
begin
    N = product(dimsizes(x))
    M = product(dimsizes(y))

    if (N.eq.M) then
        pdf2 = pdfxy(x,y,nbinx,nbiny,opt)
        return(pdf2)
    end if

    X = ndtooned(x)
    Y = ndtooned(y)

    if (N.gt.M) then
        YY   = new(N, typeof(Y), getVarFillValue(Y))
        YY(0:M-1) = (/ Y /)
        pdf2 = pdfxy(X,YY,nbinx,nbiny,opt)
    else
        XX   = new(M, typeof(X), getVarFillValue(X))
        XX(0:N-1) = (/ X /)
        pdf2 = pdfxy(XX,Y,nbinx,nbiny,opt)
    end if

    return(pdf2)
end 

; ----
undef ("genNormalDist")
function genNormalDist(xAve[1]:numeric, xStd[1]:numeric, opt:logical) 
; Usage:
;   xAve = 100
;   xStd =  10
;   xNor = createNormal(xAve, xStd, False)
local zero, one, pi, spn, N, con, xVar, x, nor
begin
  if (typeof(xAve).eq."double" .or. typeof(xAve).eq."double") then
      zero = 0.0d
      one  = 1.0d
      pi   = 4.0d*atan(1.0d)
      spn  = 3.0d                    ; 99.7% of normal
  else
      zero = 0.0 
      one  = 1.0
      pi   = 4.0*atan(1.0)
      spn  = 3.0
  end if

  if (xStd.eq.zero) then
      print("genNormalDist: xStd=0 is not allowed")
      exit
  end if

  if (opt .and. isatt(opt,"std_span")) then
      spn = opt@std_span
  end if
                ; fix an incorrect documention issue
                ; allow 'npts' or 'N'
  if (opt .and. (isatt(opt,"npts") .or. isatt(opt,"N"))) then
     if (isatt(opt,"N")) then
         N = opt@N
     else
         N = opt@npts
     end if
  else
      N = 101
  end if

  con  = one/(xStd*sqrt(2*pi))
  xVar = xStd^2

  x   = fspan( (xAve-spn*xStd), (xAve+spn*xStd), N)
  nor = con*exp(-((x-xAve)^2/(2*xVar)) )
  nor@long_name = "Normal Distribution"

  if (isatt(xAve,"units")) then
      x@units = xAve@units
  end if
  nor@x   = x
  nor@xsd = (x-xAve)/xStd
  return( nor )
end
; ------------
undef("exner_pressure")
function exner_pressure(p:numeric, punit[1]:integer)
; Exner pressure - non-dimensional pressure
local p0, exner_pres
begin
  if (punit.eq.0) then
      p0 = 100000      ; Pa    
  else
      p0 = 1000        ; hPa
  end if

  exner_pres = (p/p0)^(287.05/1005.0)
  exner_pres@long_name = "Exner Pressure"
 ;exner_pres@units     = ""        ; dimensionless
  copy_VarCoords(p, exner_pres)
  return(exner_pres) 
end
; ============
undef("exner_pressure_theta")
function exner_pressure_theta(p:numeric, theta:numeric, punit[1]:integer)
; Exner pressure - non-dimensional pressure
; theta - potential temperature (K)
local p0, exner_pres
begin
  if (punit.eq.0) then
      exner_pres = theta/p           ; Pa
  else
      exner_pres = theta/(p*100)     ; hPa
  end if
  exner_pres@long_name = "Exner Pressure"
 ;exner_pres@units     = ""        ; dimensionless
  copy_VarCoords(p, exner_pres)
  return(exner_pres) 
end
; ------------
undef("relhum_ttd")
function relhum_ttd (t:numeric, td:numeric, opt:integer)
;
; Calculate relative humidity given temperature (K)
; and dew point temperature (K)
;
; reference: John Dutton, Ceaseless Wind, 1976

local gc, lhv, rh 
begin
  rankt  = dimsizes( dimsizes(t ) )
  ranktd = dimsizes( dimsizes(td) )
  if (rankt.ne.ranktd) then
      print("relhum_ttd: rank mismatch: fatal")
      print("            rank(t )="+rankt )
      print("            rank(td)="+ranktd)
      exit
  end if
      
  gc  = 461.5             ; [j/{kg-k}]   gas constant water vapor
  gc  = gc/(1000.*4.186)  ; [cal/{g-k}]  change units
                                       ; lhv=latent heat vap
  lhv = ( 597.3-0.57*(t-273.) )        ; dutton top p273 [empirical]

  rh           = exp( (lhv/gc)*(1.0/t - 1.0/td) )
  rh@long_name = "relative humidity"
  rh@units     = "fraction"

  if (opt.eq.0) then
      rh       = rh*100.
      rh@units = "%"
  end if
  return (rh)
end

; ------------
undef ("relhum_water_ice")
function relhum_water_ice(tk:numeric,qw:numeric,p:numeric)
; ---
; computes rh (relative humidity) from p, t and qw (mixing ratio)
; ---
;  Calculate rel hum with respect to water (T>0C) or ice (T<=0)
;  Same constants as mixhum_ptrh  reversible calculation
; ---
; "Improved Magnus' Form Approx. of Saturation Vapor pressure"
;  Oleg A. Alduchov and Robert E. Eskridge
;  http://www.osti.gov/scitech/servlets/purl/548871/
;  One of Three best approx for +50 to -80 C max relative errors of (0.337/0.823) ?
; ---
; NCL version of f77 subroutines donated by: Alan Brammer, U. ALbany
; ---
; definition of mixing ratio
;    es  - is the saturation mixing ratio
;    ep  - is the ratio of the molecular weights of water vapor to dry air
;    p   - is the atmospheric pressure
;    rh  - is the relative humidity (given as a percent)

;    rh =  100*  q / ( (ep*es)/(p-es) )

; input-
;    P      - pressure (Pa)  ... this will be converted in the code to hPa (mb)
;             p      - pressure (hPa or mb)
;    tk     - temperature (k)
;    qw     - mixing ratio (kg/kg)

; output-
;    rh     - relative humidity as %

local t0,ep,onemep,es0w,aw,bw,es0i,ai,bi,est,qst,pa2mb
begin
; local: constants for water and ice

      t0     = 273.15d
      ep     = 0.622d
      onemep = 1-ep
      pa2mb  = 0.01d
          
; local: constants for water (W)

      es0w   = 6.11d
      aw     = 17.269d
      bw     = 35.86d
      
; local: constants for ice   (I)

      es0i   = 6.1128d
      ai     = 22.571d
      bi     = 273.71d

      est    = where(tk.gt.t0, es0w*exp((aw*(tk-t0)) / (tk-bw)) \
                             , es0i*exp((ai*(tk-t0)) / ((tk-t0)+bi)) )

      qst = (ep*est)/ ((p*pa2mb)-onemep*est)

      if (all(qst.gt.0)) then
          rh  =  100* (qw / qst)
      else                                           ; ? numerical issues ?
          if (isatt(tk,"_FillValue")) then
              qst@_FillValue = tk@_FillValue 
          else
              qst@_FillValue = default_fillvalue(typeof(qst))
          end if
          rh  =  100* (qw / where(qst.le.0, qst@_FillValue, qst) )
      end if

      if (typeof(tk).eq."float" .or. typeof(qw).eq."float" \
                                .or. typeof(p) .eq."float") then
          rh := tofloat(rh)
      end if

      rh@long_name = "relative humidity"
      rh@units     = "%"
      rh@tag       = "function: relhum_water_ice"
      copy_VarCoords(tk, rh)

      return (rh)
end
; ------------

undef("crossp3")
function crossp3(a[*][3]:numeric,b[*][3]:numeric)
; calculate a cross product:   c = a x b
begin

  if (typeof(a).eq."double" .or. typeof(b).eq."double") then
      if (typeof(a).eq."double") then
          c = new ( dimsizes(a), "double", getFillValue(a))
      else
          c = new ( dimsizes(b), "double", getFillValue(b))
      end if
  else
      if (typeof(a).eq."float" .or. typeof(b).eq."float") then
          if (typeof(a).eq."float") then
              c = new ( dimsizes(a), "float", getFillValue(a))
          else
              c = new ( dimsizes(b), "float", getFillValue(b))
          end if
      else
          c = new ( dimsizes(a), "integer", getFillValue(a))
      end if
  end if
  
  c(:,0) = a(:,1)*b(:,2)-a(:,2)*b(:,1)
  c(:,1) = a(:,2)*b(:,0)-a(:,0)*b(:,2)
  c(:,2) = a(:,0)*b(:,1)-a(:,1)*b(:,0)

  return(c)
end
; -----------------------------------------------------
undef("region_ind")
function region_ind(XLAT[*][*]:numeric, XLON[*][*]:numeric \
                   ,latS[1]:numeric, latN[1]:numeric \
                   ,lonW[1]:numeric, lonE[1]:numeric )
; extract subscript indicies corresponding to region
; described by curvilinear coordinates
; WRF, NARR, REGCM, etc
 
local  XLAT_1d, XLON_1d, nm_1d, nlml, ijsub
begin
 XLAT_1d  = ndtooned( XLAT )
 XLON_1d  = ndtooned( XLON )  

 nm_1d    = ind(XLAT_1d.ge.latS .and. XLAT_1d.le.latN .and. \
                XLON_1d.ge.lonW .and. XLON_1d.le.lonE)
 nlml     = ind_resolve(nm_1d, dimsizes(XLON))

 ijsub    = new( 4, typeof(nlml), "No_FillValue")

 ijsub(0) = min(nlml(:,0))   ; lat start index
 ijsub(1) = max(nlml(:,0))   ; lat last  index
 ijsub(2) = min(nlml(:,1))   ; lon start index
 ijsub(3) = max(nlml(:,1))   ; lon Last  index

 return(ijsub)
end
;-------------------------------------------------------------------------------
undef("icObjAnal_1d")
function icObjAnal_1d(x[*],y[*],z[*],lon[*],lat[*],dcrit[*]:numeric,opt:logical)
; This should *not* be invoked directly by the user.
; It is called by "function icObjAnal"
;
; Nomenclature
; x,y,z        - lon,lat,observation triplets
; lat          - lat of returned grid. Need not be equally spaced.
;                Should have the units attribute: lat@units="degrees_north"
; lon          - lon of returned grid. Need not be equally spaced
;                Should have the units attribute: lon@units="degrees_east"
; dcrit        - 1D array containing successive radii of influence.
;                Must be expressed in degrees latitude and should be
;                monotonically decreasing.  eg: dcrit = (/10, 5, 3/)
; opt          - variable to which optional attributes are attached
;                @guess = user supplied 2D guess array [default is no 1st guess]
;                         Must be same size and shape as grid defined by lat/lon
;                @zonal = True: use zonal average of z as 1st guess
;                @setmsg= True is default
;                @timing= True   ; print elapsed time per iteration (scan)
;                @count => Return number of observations used in each scan
;                           @nObs ==> (nscan,:,:)
;
local nlat, mlon, nScan, G, dimG, rankG, zonavg, i, j, ij, ns, nl, ml \
    , gcdist, diff, cf, nObs, dc2, nd, flag
begin
  nlat   = dimsizes(lat)
  mlon   = dimsizes(lon)

  nScan  = dimsizes(dcrit)
  nObs   = new( (/nScan,nlat,mlon/), "integer", "No_FillValue" ) 
  nObs   = 0

  if (opt .and. isatt(opt,"guess")) then

      G      = opt@guess                     ; 1st guess
      dimG   = dimsizes(G)
      rankG  = dimsizes(dimG)
      if (.not.(rankG.eq.2)) then
          print("icObjAnal_1d: rankG="+rankG+"  expecting 2D") 
          exit
      end if
      if (.not.(nlat*mlon.ne.prod(dimG))) then
          print("icObjAnal_1d: dimension sizes of G and nlat*mlon must match") 
          print("icObjAnal_1d: dimG="+dimG) 
          print("icObjAnal_1d: nlat="+nlat+"   mlon="+mlon) 
          exit
      end if
      flag = 2
  else

      G  = new( (/nlat,mlon/), typeof(z), getFillValue(z) )    ; 1st guess

      if (isatt(opt,"zonal") .and. opt@zonal) then       ; create zonal avg
          dlat  = max(abs(lat(1:nlat-1)-lat(0:nlat-2)) ) ; nominal
         ;dlat  = 2*dlat      ; expand to get more data for zonal average
                                                         ; bigger range
          zonavg = new( nlat, typeof(z), z@_FillValue)
          do nl=0,nlat-1
             i = ind(y.le.(lat(nl)+dlat) .and. y.ge.(lat(nl)-dlat))
             if (.not.all(ismissing(i))) then
                 zonavg(nl) = avg( z(i) )      ; zonal avg of all observations
             end if
             delete(i)
          end do

          if (any(ismissing(zonavg))) then
              zonavg = linmsg(zonavg, -1)      ; linearly interpolate
          end if
         print("icObjAnal_1d: lat="+lat+"   zonavg="+zonavg)
                                               ; arbitrary smooth
        ;;zonavg = wgt_runave(zonavg, (/0.25, 0.50, 0.25/), 1)  
          zonavg = wgt_runave(zonavg, filwgts_normal (7, 1.0, 0) , 1)  

          do nl=0,nlat-1
             G(nl,:) = zonavg(nl)
          end do

          delete(zonavg)
          flag = 1
      else
          G    = 0.0                         ; direct ... no 1st guess
          flag = 0
      end if
  end if

  G!0   = "lat"
  G!1   = "lon"
  G&lat =  lat
  G&lon =  lon

  wcStrt = systemfunc("date")

  do ns=0,nScan-1
     nsStrt = systemfunc("date")
     dc2    = dcrit(ns)^2

    do nl=0,nlat-1
       i = ind( abs(y-lat(nl)).le.dcrit(ns)) 
       if (.not.ismissing(i(0)) ) then

           do ml=0,mlon-1
              if (ns.eq.0 .or. (ns.gt.0 .and.  nObs(ns-1,nl,ml).gt.0)) then
                  gcdist = gc_latlon(lat(nl),lon(ml), y(i),x(i), 0,2)
                  nd     = num(gcdist.le.dcrit(ns))
                  nObs(ns,nl,ml) = nd     ; # observations within radius
     
                  if (nd.gt.0) then 
                      j      = ind(gcdist.le.dcrit(ns))
                      ij     = i(j)
                      diff   = z(ij)-G(nl,ml)  ; normally interpolate G to z but ..... 
                      wgt    = exp(-4*gcdist(j)^2/dc2)
                      cf     = sum(wgt*diff)/sum(wgt)  ; correction factor
                     ;print("ns="+ns+"   nl="+nl+"  ml="+ml+"  cf="+cf)
         
                      G(nl,ml) = G(nl,ml) + cf         ; update Guess
                      
                      delete(j)
                      delete(ij)
                      delete(cf)
                      delete(wgt)
                      delete(diff)
                  end if             ; nd
                  delete(gcdist)
              end if
           end do                    ; ml
       end if
       delete(i)
    end do                           ; nl
                                     ; default is to smooth
    if (.not.isatt(opt,"smooth") .or. opt@smooth) then
        if (ns.lt.(nScan-1)) then
            G = smth9(G, 0.50,-0.25, 0)  ; light local smoother
        else
            G = smth9(G, 0.50, 0.25, 0)  ; heavy local smoother
        end if
    end if
                                     ; set grid pts ouside of 
    if (ns.eq.0) then                ; max radius to _FillValue
        if (.not.isatt(opt,"setmsg") .or. opt@setmsg) then
            G = where(nObs(ns,:,:).eq.0, G@_FillValue, G)
        end if
    end if

    if (isatt(opt,"timing")) then
        wallClockElapseTime(nsStrt,"icObjAnal_1d: ns="+ns, 0)
    end if

  end do                             ; ns`

  if (isatt(opt,"timing")) then
      wallClockElapseTime(wcStrt,"Total time: icObjAnal_1d: nScan="+nScan , 0)
  end if

  if (opt .and. isatt(opt,"count") .and. opt@count) then
      G@nObs = nObs
  end if

  return(G)
end
;-----------------------------------------------------------------------------------
undef("icObjAnal_2d")
function icObjAnal_2d(x[*],y[*],z[*],lon2d[*][*],lat2d[*][*],dcrit[*]:numeric,opt:logical)
;
; This should not be invoked directly by the user.
; It is called by "function icObjAnal"
;
; Nomenclature
; x,y,z        - lon,lat,observation triplets
; lat2d        - lat of returned grid. 
; lon2d        - lon of returned grid
; dcrit        - 1D array containing successive radii of influence.
;                Must be expressed in degrees latitude and should be
;                monotonically de.  eg: dcrit = (/10, 5, 3/)
; opt          - variable to which optional attributes are attached
;                @guess = 2D guess array [default is no 1st guess]
;                @timing = True   ; print times
;                @count  => Return number of observations used in each scan
;                           @nObs ==> (nscan,:,:)
;
local nlat, mlon, nScan, G, dimG, rankG, zonavg, i, j, ij, ns, nl, ml \
    , gcdist, diff, cf, nObs, dc2, nd, dimlat, ranklt, LAT, LON, G1D
begin
  dimlat = dimsizes(lat2d)
  nlat   = dimlat(0)
  mlon   = dimlat(1)

  nScan  = dimsizes(dcrit)
  nObs   = new( (/nScan,nlat,mlon/), "integer", "No_FillValue" ) 
  nObs   = 0

  if (opt .and. isatt(opt,"guess")) then

      G      = opt@guess                     ; 1st guess
      dimG   = dimsizes(G)
      rankG  = dimsizes(dimG)
      ranklt = dimsizes(dimlat)
      if (.not.(rankG.eq.ranklt)) then
          print("icObjAnal_2d: rankG="+rankG+"  ranklt="+ranklt) 
          exit
      end if
      if (.not.all(dimlat.eq.dimG)) then
          print("icObjAnal_2d: all dimension sizes must be the same") 
          print("icObjAnal_2d: dimltt="+dimlat+"   dimG="+dimG) 
          exit
      end if

  else

      G  = new( dimlat, typeof(z), getFillValue(z) )    ; 1st guess

      if (isatt(opt,"zonal") .and. opt@zonal) then      ; create zonal avg
          mnlat = min(lat2d)
          mxlat = max(lat2d)
          lat1d = fspan(mnlat,mxlat,nlat)               ; nominal
          dlat  = (mxlat-mnlat)/(nlat-1)                ; nominal
          dlat  = 4*dlat      ; expand to get more data for zonal average
                                                         ; bigger range
          zonavg = new( nlat, typeof(z), z@_FillValue)
          do nl=0,nlat-1
             i = ind(y.le.(lat1d(nl)+dlat) .and. y.ge.(lat1d(nl)-dlat))
             if (.not.all(ismissing(i))) then
                 zonavg(nl) = avg( z(i) )      ; zonal avg of all observations
             end if
             delete(i)
          end do

          if (any(ismissing(zonavg))) then
              zonavg = linmsg(zonavg, -1)          ; linearly interpolate
          end if

        ;;zonavg = wgt_runave(zonavg, (/0.25, 0.50, 0.25/), 1)  ;smooth
          zonavg = wgt_runave(zonavg, filwgts_normal (7, 1.0, 0) , 1)  ;smooth

          LAT  = ndtooned(lat2d)
          G1D  = ndtooned(G) 

          do nl=0,nlat-1
             i = ind(LAT.le.(lat1d(nl)+dlat) .and. LAT.ge.(lat1d(nl)-dlat))
             G1D(i) = zonavg(nl)
             delete(i)
          end do

          delete(zonavg)
          delete(lat1d)
          delete(LAT)

          G = onedtond(G1D, dimlat)
          delete(G1D)
      else
          G = 0.0                         ; direct ... no 1st guess
      end if
  end if

  G@lat2d = lat2d
  G@lon2d = lon2d

  LAT    = ndtooned( lat2d )
  LON    = ndtooned( lon2d )

  wcStrt =  systemfunc("date")
  do ns=0,nScan-1
     nsStrt =  systemfunc("date")
     dc2 = dcrit(ns)^2

    do nl=0,nlat-1

      do ml=0,mlon-1
         if (ns.eq.0 .or. (ns.gt.0 .and. nObs(ns-1,nl,ml).gt.0)) then
             i = ind( abs(y-lat2d(nl,ml)).le.dcrit(ns) ) 
             if (.not.any(ismissing(i)) ) then
                 gcdist = gc_latlon(lat2d(nl,ml),lon2d(nl,ml), y(i),x(i), 0,2)
    
                 nd     = num(gcdist.le.dcrit(ns))
                 nObs(ns,nl,ml) = nd     ; # observations within radius
    
                 if (nd.gt.0) then 
                     j      = ind(gcdist.le.dcrit(ns))
                     ij     = i(j)
                     diff   = z(ij)-G(nl,ml)  ; normally interpolate G to z but .....            
                     wgt    = exp(-4*gcdist(j)^2/dc2)
                     cf     = sum(wgt*diff)/sum(wgt) 
                    ;print("ns="+ns+"   nl="+nl+"  ml="+ml+"  cf="+cf)
        
                     G(nl,ml) = G(nl,ml) + cf 
                     
                     delete(j)
                     delete(ij)
                     delete(cf)
                     delete(wgt)
                     delete(diff)
                 end if
                 delete(gcdist)
             end if
             delete(i)
         end if
      end do                         ; ml
    end do                           ; nl
                                     ; default is to smooth
    if (.not.isatt(opt,"smooth") .or. opt@smooth) then
        if (ns.lt.(nScan-1)) then
            G = smth9(G, 0.50,-0.25, 0)  ; light local smoother
        else
            G = smth9(G, 0.50, 0.25, 0)  ; heavy local smoother
        end if
    end if

    if (isatt(opt,"timing")) then
        wallClockElapseTime(nsStrt,"icObjAnal_2d: ns="+ns , 0)
    end if
  end do                             ; ns`

  if (isatt(opt,"count")) then
      G@nObs = nObs
  end if

  if (isatt(opt,"timing")) then
      wallClockElapseTime(wcStrt,"Total time: icObjAnal_2d: nScan="+nScan , 0)
  end if

  return(G)
end
;----------------------------------------------------------------------------
undef("obj_anal_ic_deprecated")
function obj_anal_ic_deprecated(X[*],Y[*],Z[*], lon:numeric,lat:numeric \
                        ,dcrit[*]:numeric,opt:logical)
; Perform Barnes [ Cressman ] type iterative correction objective analysis
;
; Nomenclature
; x,y,z        - lon,lat,observation triplets
; lat          - lat of returned grid. Need not be equally spaced
;                but must be monotonically increasing. Should have the
;                units attribute assigned: lat@units="degrees_north"
; lon          - lon of returned grid. Need not be equally spaced
;                but must be monotonically increasing. Should have the
;                units attribute assigned: lon@units="degrees_east"
; dcrit        - 1D array containing successive radii of influence.
;                Must be expressed in degrees latitude and should be
;                monotonically de.  eg: dcrit = (/10, 5, 3/)
;                dims = dimsizes(dcrit)   , nscan = dims(0) 
; opt          - variable to which optional attributes are attached
;                @guess = 2D guess array  [input]
;                @timing = print times    
;                @count  = number of observation used in each scan
;                          Return @nObs ==> (nscan,:,:)
;
local wcStrt, i, XX, YY, ZZ, k, x, y, z, dimLat, dimLon, rankLat, rankLon
begin
  wcStrt =  systemfunc("date")

  if (.not.isatt(Z,"_FillValue")) then
      Z@_FillVlaue = 1e20
  end if
                            ; eliminate missing values
  i   = ind(.not.ismissing(Z))
  if (ismissing(i(0))) then
      print("icObjAnal: all input data are missing")
  end if
  XX  = X(i)
  YY  = Y(i)
  ZZ  = Z(i)
  delete(i)
                           
 ;k   = dim_pqsort(YY, 1)   ; sort obs in ascending latitude order 
 ;x   = XX(k)               ; not used here ... too lazy to change code
 ;y   = YY(k)
 ;z   = ZZ(k)
 ;delete(k)

 ;delete(XX)
 ;delete(YY)
 ;delete(ZZ)

 ;print("icObjAnal:   z="+z+"  lon="+x+"  lat="+y )

  dimLat  = dimsizes(lat)
  dimLon  = dimsizes(lon)
  rankLat = dimsizes(dimLat)
  rankLon = dimsizes(dimLon)
  if (rankLat.ne.rankLon) then
      print("icObjAnal: ranks of lat and lon must match")
      print("icObjAnal: rankLat="+rankLat)
      print("icObjAnal: rankLon="+rankLon)
  end if
  if (rankLat.gt.2) then
      print("icObjAnal: ranks of lat and lon must be 1 or 2")
      print("icObjAnal: rankLat="+rankLat)
  end if

  if (rankLat.eq.1) then
     ;zGrid = icObjAnal_1d(X ,Y , Z,lon,lat,dcrit,opt)
      zGrid = icObjAnal_1d(XX,YY,ZZ,lon,lat,dcrit,opt)
  else
      zGrid = icObjAnal_2d(X ,Y , Z,lon,lat,dcrit,opt)
     ;zGrid = icObjAnal_2d(XX,YY,ZZ,lon,lat,dcrit,opt)
  end if
  if (isatt(opt,"timing")) then
      wallClockElapseTime(wcStrt,"icObjAnal", 0)
  end if

  if (isatt(Z,"long_name")) then
      zGrid@long_name = Z@long_name
  end if
  if (isatt(Z,"units")) then
      zGrid@units = Z@units
  end if
 
  return(zGrid)
end
;****************************************************************************
; Calculate a 'bunch' of dispersion statistics 
; This was originally developed for looking at satellite swath data
; which has many points but, often, many missing values and outliers.
; It can be used to ascertain the best values to use for
; contour resources: min/max/interval
; 
; *This can be slow for big 'x'*
;
undef("stat_dispersion")
function stat_dispersion (x:numeric, opt[1]:logical )
; Robust dispersion statistics
; 
local nStat, nx, sFillV, nFill, one, statx, work, nwork, x1d, ivalid, allFill, nValid
begin
   nx      = product( dimsizes(x) )
   nStat   = 30
   nFill   = 0
   allFill = False 

   if (typeof(x).eq."double") then
       sFillV = 1d20
   else
       sFillV = 1e20
   end if
  
   if (isatt(x,"_FillValue")) then  
       nFill  = num(ismissing(x))
       if (nFill.gt.0) then
           x1d    = ndtooned(x) 
           ivalid = ind(.not.ismissing(x1d))
           if (.not.ismissing(ivalid(0))) then
               work   = x1d(ivalid)
           else
               allFill= True
               work   = new ( 1, typeof(sFillV), sFillV)
           end if
           delete(x1d)
       else
           work = ndtooned(x) 
       end if
   else
       work  = ndtooned(x) 
   end if

   if (typeof(x).eq."double") then
       one    = 1.0d
       moment = (/0.0d, 0.0d ,0.0d ,0.0d /)
   else
       one    = 1.0
       moment = (/0.0 , 0.0  ,0.0  ,0.0  /)
   end if

   nValid   = 0l                    ; force 'long' int
   nValid   = nx-nFill

   nwork    = 0l                    ; force nwork as 'long' int
   nwork    = dimsizes(work)        

   statx    = new ( nStat, typeof(sFillV), sFillV) 
   if (nwork.gt.1) then
      ;statx(0) = avg(x)                ; mean
      ;statx(1) = stddev(x)             ; std. deviation
       moment   = dim_stat4( work ) 
       statx(0) = moment(0)             ; mean
       statx(1) = sqrt(moment(1))       ; sample std. deviation
                                        ; fixed 15 Oct 2014; had returned variance
       
       qsort( work )                    ; sort into ascending order
       statx(2) = work(0)               ; min ( work )

; Daniel Leuenberger (Meteo Swiss) suggested the small 
;      sample size granularity (10/2013)
                                            ; -1 ... NCL zero based 
       if (nwork.ge.10) then 
         statx(3) = work( nwork/10-1 )    ; lower dectile
         statx(13)= work((nwork*9)/10-1)  ; upper dectile
       end if

       if (nwork.ge.8) then
         statx(4) = work( nwork/8 -1 )    ; lower octtile
         statx(12)= work((nwork*7)/8 -1)  ; upper octtile
       end if

       if (nwork.ge.6) then
         statx(5) = work( nwork/6 -1 )    ; lower sextile
         statx(11)= work((nwork*5)/6 -1)  ; upper sextile
       end if

       if (nwork.ge.4) then
         statx(6) = work( nwork/4 -1 )    ; lower quartile
         statx(10)= work((nwork*3)/4 -1)  ; upper quartile
       end if
 
       if (nwork.ge.3) then
         statx(7) = work( nwork/3 -1 )    ; lower 3tile
         statx(9) = work((nwork*2)/3 -1)  ; upper 3tile
       end if

       if (nwork.ge.2) then
         if (nwork%2 .eq.0) then          ; median even
           statx(8) = (work(nwork/2-1)+work(nwork/2))*0.5  
         else
           statx(8) = work( nwork/2   ) ; median odd  
         end if
       end if 

       statx(14)= work(nwork-1)         ; max ( work )      
       statx(15)= statx(14)-statx(2)    ; range
       if (statx(1).gt.0) then
       if (.not.ismissing(statx(1)) .and. statx(1).gt.0) then
               statx(16)= statx(15)/statx(1); a measure of dispersion
           end if
           statx(17)= sqrt(sum((work-statx(0))^2)/nwork) ; root mean sq anomaly
       end if                               ; nwork.ge.10
   end if                                   ; .not.allFill
   statx(18)= nx
   statx(19)= nValid
   statx(20)= nFill
   statx(21)= 100*(one - (one*(nx-nFill))/nx)

   if (nwork.gt.1000) then
        ;JBuzan [Purdue] 
       statx(22) = work( max((/ 0, nwork/1000-1 /)) ) ; lower 99.9% 
       statx(23) = work( max((/ 0, nwork/100-1  /)) ) ; lower 99.%  
       statx(24) = work( max((/ 0, nwork/20-1   /)) ) ; lower 95%   
    
       statx(25) = work( min((/nwork-1, (nwork*19)/20-1    /)) ) ; upper 95%
       statx(26) = work( min((/nwork-1, (nwork*99)/100-1   /)) ) ; upper 99.%
       statx(27) = work( min((/nwork-1, (nwork*999)/1000-1 /)) ) ; upper 99.9%
        ;JBuzan
   end if

        ;Arne Melsom (Norwegian Meteorological Institute)
   statx(28)  = moment(2)                   ; skewness
   statx(29)  = moment(3)                   ; kurtosis
        ;Arne Melsom

   statx@long_name = "Robust Dispersion Statistics"
   if (isatt(x,"long_name")) then
       statx@long_name = statx@long_name +": "+ x@long_name
   else
       if (isatt(x,"hdfeos5_name")) then
           statx@long_name = statx@long_name +": "+ x@hdfeos5_name
       end if
   end if
   if (isatt(x,"units")) then
       statx@units     = x@units
   end if

   if (isatt(statx,"No_FillValue")) then
       if (typeof(x).eq."double") then
           statx@_FillValue = 1d20
       else
           statx@_FillValue = 1e20
       end if
   end if

   if (opt .and. (isatt(opt,"PrintStat") .and. opt@PrintStat)) then 
       print(" ")
       print("   ===> "+statx@long_name+" <===")
       print(" [0]            Mean="+statx(0)  ) 
       print(" [1]          StdDev="+statx(1)  ) 
       print(" [2]             Min="+statx(2)  ) 
       print(" [3]          LowDec="+statx(3)  ) 
       print(" [4]          LowOct="+statx(4)  ) 
       print(" [5]          LowSex="+statx(5)  ) 
       print(" [6]     LowQuartile="+statx(6)  ) 
       print(" [7]          LowTri="+statx(7)  ) 
       print(" [8]          Median="+statx(8)  ) 
       print(" [9]         HighTri="+statx(9)  ) 
       print(" [10]   HighQuartile="+statx(10) ) 
       print(" [11]        HighSex="+statx(11) ) 
       print(" [12]        HighOct="+statx(12) ) 
       print(" [13]        HighDec="+statx(13) ) 
       print(" [14]            Max="+statx(14) ) 
       print(" [15]          Range="+statx(15) ) 
       print(" [16]     Dispersion="+statx(16) ) 
       print(" [17]    RMS Anomaly="+statx(17) ) 
       print(" [18]      #   Total="+statx(18) ) 
       print(" [19]      #    Used="+statx(19) ) 
       print(" [20]      # Missing="+statx(20) ) 
       print(" [21]      % Missing="+statx(21) ) 
       print(" [22]     Lower 0.1%="+statx(22) ) 
       print(" [23]     Lower 1.0%="+statx(23) ) 
       print(" [24]     Lower 5.0%="+statx(24) ) 
       print(" [25]     Upper 5.0%="+statx(25) ) 
       print(" [26]     Upper 1.0%="+statx(26) ) 
       print(" [27]     Upper 0.1%="+statx(27) ) 
       print(" [28]       Skewness="+statx(28) ) 
       print(" [29]       Kurtosis="+statx(29) ) 
       print(" ")
   end if

   return(statx)
end
; **********************************************************************
; D. Shea
; wrapper for NCL procedure "int2p"  that copies attributes and coordinate 
; vars.  It adds the longitude and latitude coordinates.
;
; Modified by Seah 1/1/2010 to handle multi-d xo.

undef("int2p_Wrap")
function int2p_Wrap (xi:numeric, fi:numeric, xo:numeric, linlog[1]:integer)

; wrapper for NCL function "int2p"  that copies attributes and coordinate vars.
local fo, dimfi, nDim, n, nD
begin

  fo   = int2p (xi,fi, xo, linlog)              ; perform interpolation 
                                                ; shea_misc functions
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fo))                 ; number of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  copy_VarCoords_1 (fi, fo)                     ; copy coord variables  
                                                ; except for rightmost
  nD    = nDim-1                              ; last dimension (rightmost)
  rnkxo = dimsizes( dimsizes(xo) )

  if (rnkxo.eq.1) then
      if (isdimnamed(xo,0)) then                ; create a new coord for
          fo!nD      = xo!0                     ; if present, use xo name
      else 
          if (isdimnamed(fi,nD)) then
              fo!nD = str_switch(fi!nD)         ; if present, use same name
            ;;fo!nD = changeCaseChar(fi!nD)     ; if present, use same name
          else                                  ; but change case 
              fo!nD = "X"                       ; default dimension name
          end if                
      end if

      fo&$fo!nD$ = xo                           ; assign coords [1D only]

  else                                          ; must be multi-d
      if (isdimnamed(xo,nD)) then               ; is xo rightmost named?
         fo!nD   = xo!nD                        ; if present, use xo name
      else 
         if (isdimnamed(fi,nD)) then
             fo!nD = str_switch(fi!nD)          ; if present, use same name
           ;;fo!nD = changeCaseChar(fi!nD)      ; if present, use same name
         else                                   ; but change case 
             fo!nD = "X"                        ; default dimension name
         end if                
      end if
  end if
   
  return (fo)
end
; **********************************************************************
; M. Haley
; wrapper for NCL procedure "int2p_n" that copies attributes and
; coordinate  vars.  It adds the longitude and latitude coordinates.
;
; Based on int2p_Wrap.
; Added Oct 25, 2009

undef("int2p_n_Wrap")
function int2p_n_Wrap (xi:numeric, fi:numeric, xo:numeric, \
                       linlog[1]:integer, pdim[1]:integer)

; wrapper for NCL function "int2p_n"  that copies attributes and 
; coordinate vars.
local fo, dimfi, nDim, n, nD
begin

  fo   = int2p_n (xi,fi, xo, linlog, pdim)      ; perform interpolation 
                                                ; shea_misc functions
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fo))                 ; number of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  copy_VarCoords_not_n (fi, fo, pdim)           ; copy coord variables  
                                                ; except for pdim-th

                                                ; create a new coord for
                                                ; level dimension
  dimxo = dimsizes(xo)                          ; check if multi-d
  rnkxo = dimsizes(dimxo)
  if (rnkxo.eq.1.and.isdimnamed(xo,0)) then
    fo!pdim = xo!0                              ; if present, use xo name
  else
    if(rnkxo.gt.1.and.isdimnamed(xo,pdim)) then
      fo!pdim = xo!pdim                         ; if present, use xo name
    else 
      if (isdimnamed(fi,pdim)) then
        fo!pdim = str_switch(fi!pdim)           ; if present, use same name
        ;;fo!pdim = changeCaseChar(fi!pdim)     ; if present, use same name
      else                                      ; but change case 
        fo!pdim = "X"                           ; default dimension name
      end if                
    end if
  end if
                                                ; assign coordinates
  if (rnkxo.eq.1) then
    fo&$fo!pdim$ = xo
  end if
   
  return (fo)
end
;------------------------------------------------
undef("numBinOneRuns")
function numBinOneRuns ( a[*]:integer, nCrit:integer)
;
; Give a binary series [0s and 1s], calculate the
; number of 'runs' of 1s that meet lengths specified by 'nCrit'
; Nomenclature:
;      a[*]   => integer ... eg (/ 1,1,1,0,0,0,1,0,0,1,1,1,1 /)
;      nCrit  => integer ... scalar [eg, 3] or of length 2 [eg: (/4,6/) ]
;                scalar: count the number of runs .ge.3
;                   [2]: count the number of runs .ge.4 and .le. 6
;                      : if nCrit[0]=nCrit[1] ( eg : (/5,5/) ]
;                   [2]: count the number of runs .ge.5 and .le. 5
;                      : this would yield the number of runs of
;                      ; exactly length 5
;
; Primary source:
; David Allured: CU/CIRES Climate Diagnostics Center (CDC) 

local inds, ni, deltas, starts, ends, lengths
begin
   dim_nCrit  = dimsizes(nCrit)
   if (dim_nCrit.gt.2) then
       print("numBinOneRuns: nCrit size must be .le. 2: nCrit="+dim_nCrit)
       return(default_fillvalue("integer"))
   end if

   inds    = ind (a .eq. 1)
   ni      = dimsizes (inds)
   deltas  = new (ni+1, integer)
   deltas  = 0
   deltas(1:ni-1) = (inds(1:ni-1) - inds(0:ni-2))
   starts  = inds(ind (deltas(0:ni-1).ne.1 .and. deltas(1:ni).eq.1))
   ends    = inds(ind (deltas(0:ni-1).eq.1 .and. deltas(1:ni).ne.1))
   lengths = ends - starts + 1

   if (dim_nCrit.eq.1) then
       return(num (lengths.ge.nCrit) )
   else
       return(num (lengths.ge.nCrit(0) .and. lengths.le.nCrit(1)) )
   end if
end
;**********************************************************
undef("merge_levels_sfc")
function merge_levels_sfc (x, xsfc, levind[1]:integer)
; levind .ge. 0 top-to-bot ordering
; levind .lt. 0 bot-to-top ordering
;
; rank 1: x(lev),  xsfc(1)
; rank 2: x(time,lev),  xsfc(time)
; rank 3: x(lev,lat,lon),  xsfc(lat,lon)
; rank 4: x(time,lev,lat,lon),  xsfc(time,lat,lon)
; rank 5: x(case,time,lev,lat,lon),  xsfc(case,time,lat,lon)
;
local dimx, dimxs, rankx, rankxs, klev, k1, k2, mono, fudge
begin
  dimx   = dimsizes(x)
  rankx  = dimsizes(dimx)
  dimxs  = dimsizes(xsfc)
  rankxs = dimsizes(dimxs)

  if (rankx.gt.1 .and. .not.(rankx.eq.(rankxs+1)) ) then
      print("merge_levels_sfc: xsfc must have one less dimension than x")
      print("                  rank(x)="+rankx+"  rank(xs)="+rankxs )
      exit
  end if

  if (rankx.eq.1 .and. (rankxs.ne.1 .or. dimxs(0).gt.1) ) then
      print("merge_levels_sfc: For x[*], then xsfc must be a scalar")
      print("                  rank(x)="+rankx+"  rank(xs)="+rankxs+"  dimxs="+dimxs )
      exit
  end if

  momo  = 1
  fudge = 1.02    ; completely arbitrary > 1

;     rank 5: x(case,time,lev,lat,lon),  xsfc(case,time,lat,lon)
  if (rankx.eq.5 .and. rankxs.eq.4) then
      klev    = dimx(2)+1

      if (iscoord(x, x!2) ) then
          lv    = x&$x!2$
          mono  = isMonotonic(lv)
          lvNew = new (klev, typeof(lv))
      else
          lv    = ispan(0,klev-2,1)*1.0
          lvNew = ispan(0,klev-1,1)*1.0
      end if

      dimx(2) = klev
      xNew    = new( dimx, typeof(x), getFillValue(x) )
      if (levind.lt.0) then
          xNew(:,:, 1:  ,:,:) = (/ x /)
          xNew(:,:, 0:0 ,:,:) = (/ xsfc /)
          lvNew(1:)           = (/ lv /)
         ;lvNew(0 )           =  lv(0)*fudge  
          lvNew(0 )           =  2*lv(0) - lv(1)  
      else
          k1  = klev-1
          k2  = klev-2
          xNew(:,:, 0:k2,:,:) = (/ x /)
          xNew(:,:,k1:k1,:,:) = (/ xsfc /)
          lvNew(0:k2)         = (/ lv /)
         ;lvNew(k1)           =  lv(k2)*fudge  
          lvNew(k1)           =  2*lv(k2) - lv(k2-1)  
      end if
      copy_VarAtts(lv, lvNew)
       
      if (isdimnamed(x, 0)) then
          xNew!0  = x!0
      else
          x!0     = "case"
          xNew!0  = "case"
      end if
      xNew!1  = x!1
      xNew!2  = str_switch(x!2)   ; new name
      xNew!3  = x!3
      xNew!4  = x!4

      if (iscoord(x, x!0) ) then
          xNew&$xNew!0$ = x&$x!0$ ; case
      end if
      if (iscoord(x, x!1) ) then
          xNew&$xNew!1$ = x&$x!1$ ; time
      end if
      if (iscoord(x, x!2) ) then
          xNew&$xNew!2$ = lvNew   ; level of merged 
      end if
      if (iscoord(x, x!3) ) then
          xNew&$xNew!3$ = x&$x!3$ ; lat
      end if
      if (iscoord(x, x!4) ) then
          xNew&$xNew!4$ = x&$x!4$ ; lon
      end if
  end if

;     rank 4: x(time,lev,lat,lon),  xsfc(time,lat,lon)
  if (rankx.eq.4 .and. rankxs.eq.3) then
      klev    = dimx(1)+1
      if (iscoord(x, x!1) ) then
          lv    = x&$x!1$
          mono  = isMonotonic(lv)
          lvNew = new (klev, typeof(lv))
      else
          lv    = ispan(0,klev-2,1)
          lvNew = ispan(0,klev-1,1)
      end if

      dimx(1) = klev
      xNew    = new( dimx, typeof(x), getFillValue(x) )
      if (levind.lt.0) then
          xNew(:, 1:  ,:,:) = (/ x /)
          xNew(:, 0:0 ,:,:) = (/ xsfc /)
          lvNew(1:)         = (/ lv /)
         ;lvNew(0)          =  lv(0)*fudge  
          lvNew(0 )         =  2*lv(0) - lv(1)  
      else
          k1  = klev-1
          k2  = klev-2
          xNew(:, 0:k2,:,:) = (/ x /)
          xNew(:,k1:k1,:,:) = (/ xsfc /)
          lvNew(0:k2)       = (/ lv /)
         ;lvNew(k1)         =  lv(k2)*fudge  
          lvNew(k1)         =  2*lv(k2) - lv(k2-1)  
      end if
      copy_VarAtts(lv, lvNew)

      xNew!0  = x!0
      xNew!1  = str_switch(x!1)   ; new name
      xNew!2  = x!2
      xNew!3  = x!3
      if (iscoord(x, x!0) ) then
          xNew&$xNew!0$ = x&$x!0$ ; time
      end if
      if (iscoord(x, x!1) ) then
          xNew&$xNew!1$ = lvNew   ; level of merged 
      end if
      if (iscoord(x, x!2) ) then
          xNew&$xNew!2$ = x&$x!2$ ; lat
      end if
      if (iscoord(x, x!3) ) then
          xNew&$xNew!3$ = x&$x!3$ ; lon
      end if
  end if

;     rank 3: x(lev,lat,lon),  xsfc(lat,lon)
  if (rankx.eq.3 .and. rankxs.eq.2) then
      klev    = dimx(0)+1
      if (iscoord(x, x!0) ) then
          lv    = x&$x!0$
          mono  = isMonotonic(lv)
          lvNew = new (klev, typeof(lv))
      else
          lv    = ispan(0,klev-2,1)
          lvNew = ispan(0,klev-1,1)
      end if

      dimx(0) = klev
      xNew    = new( dimx, typeof(x), getFillValue(x) )
      if (levind.lt.0) then
          xNew( 1:  ,:,:) = (/ x /)
          xNew( 0:0 ,:,:) = (/ xsfc /)
          lvNew(1:)       = (/ lv /)
         ;lvNew(0)        =  lv(0)*fudge  
          lvNew(0 )       =  2*lv(0) - lv(1)  
      else
          k1  = klev-1
          k2  = klev-2
          xNew( 0:k2,:,:) = (/ x /)
          xNew(k1:k1,:,:) = (/ xsfc /)
          lvNew(0:k2)     = (/ lv /)
         ;lvNew(k1)       =  lv(k2)*fudge  
          lvNew(k1)       =  2*lv(k2) - lv(k2-1)  
      end if
      copy_VarAtts(lv, lvNew)

      xNew!0  = str_switch(x!0)   ; new name
      xNew!1  = x!1
      xNew!2  = x!2
      if (iscoord(x, x!0) ) then
          xNew&$xNew!0$ = lvNew   ; level of merged 
      end if
      if (iscoord(x, x!1) ) then
          xNew&$xNew!1$ = x&$x!1$ ; lat
      end if
      if (iscoord(x, x!2) ) then
          xNew&$xNew!2$ = x&$x!2$ ; lon
      end if
  end if

;     rank 1: x(lev),  xsfc(1)
  if (rankx.eq.1 .and. rankxs.eq.1) then
      klev    = dimx(0)+1
      if (iscoord(x, x!0) ) then
          lv    = x&$x!0$
          mono  = isMonotonic(lv)
          lvNew = new (klev, typeof(lv))
      else
          lv    = ispan(0,klev-2,1)
          lvNew = ispan(0,klev-1,1)
      end if

      dimx(0) = klev
      xNew    = new( dimx, typeof(x), getFillValue(x) )
      if (levind.lt.0) then
          xNew( 1:  ) = (/ x /)
          xNew( 0:0 ) = (/ xsfc /)
          lvNew(1:)   = (/ lv /)
         ;lvNew(0)    =  lv(0)*fudge  
          lvNew(0 )   =  2*lv(0) - lv(1)  
      else
          k1  = klev-1
          k2  = klev-2
          xNew( 0:k2) = (/ x /)
          xNew(k1:k1) = (/ xsfc /)
          lvNew(0:k2) = (/ lv /)
         ;lvNew(k1)   =  lv(k2)*fudge  
          lvNew(k1)   =  2*lv(k2) - lv(k2-1)  
      end if
      copy_VarAtts(lv, lvNew)

      xNew!0  = str_switch(x!0)   ; new name
      if (iscoord(x, x!0) ) then
          xNew&$xNew!0$ = lvNew   ; level of merged 
      end if
  end if

;     rank 2: x(time,lev),  xsfc(time)
  if (rankx.eq.2 .and. rankxs.eq.1) then
      klev    = dimx(1)+1
      if (iscoord(x, x!1) ) then
          lv    = x&$x!1$
          mono  = isMonotonic(lv)
          lvNew = new (klev, typeof(lv))
      else
          lv    = ispan(0,klev-2,1)
          lvNew = ispan(0,klev-1,1)
      end if

      dimx(1) = klev
      xNew    = new( dimx, typeof(x), getFillValue(x) )
      if (levind.lt.0) then
          xNew(:, 1:  ) = (/ x /)
          xNew(:, 0:0 ) = (/ xsfc /)
          lvNew(1:)     = (/ lv /)
         ;lvNew(0)      =  lv(0)*fudge  
          lvNew(0 )     =  2*lv(0) - lv(1)  
      else
          k1  = klev-1
          k2  = klev-2
          xNew(:, 0:k2) = (/ x /)
          xNew(:,k1:k1) = (/ xsfc /)
          lvNew(0:k2)   = (/ lv /)
         ;lvNew(k1)     =  lv(k2)*fudge  
          lvNew(k1)     =  2*lv(k2) - lv(k2-1)  
      end if

      xNew!0  = x!0
      xNew!1  = str_switch(x!1)   ; new name
      if (iscoord(x, x!0) ) then
          xNew&$xNew!0$ = x&$x!0$ ; time
      end if
      if (iscoord(x, x!1) ) then
          xNew&$xNew!1$ = lvNew   ; level of merged 
      end if
  end if

  copy_VarAtts(x, xNew)

  return(xNew)
end
; --------------------------
undef("time_to_newtime")
function time_to_newtime(time[*]:numeric, new_time_units[1]:string)
; change a "udunits" recognized time unit to a new (different)\
; "udunits" recognized time unit.
;
; Example:
;  TIME = time_to_newtime(time, "days since 1950-01-01 00:00" ) ; Time(time) 
;  time = time_to_newtime(time, "hours since 1800-01-01 00:00") ; time(time) 
begin
  ntim   = dimsizes(time)
  date   = cd_calendar( time, 0)
 ;print(date(:,0)+"  "+date(:,1)+"  "+date(:,2)+"  "+date(:,3))
  TIME   = cd_inv_calendar(toint(date(:,0)),toint(date(:,1)) \
                          ,toint(date(:,2)),toint(date(:,3)) \
                          ,toint(date(:,4)),todouble(date(:,5)),new_time_units, 0)

  if (isatt(time,"calendar")) then
      TIME@calendar = time@calendar
  end if

  TIME!0        = "time"
  TIME&time     =  TIME
                          ; delete attributes that may no longer be appropriate
  if (isatt(TIME,"actual_range")) then
     delete(TIME@actual_range)
  end if
  if (isatt(TIME,"delta_t")) then
     delete(TIME@delta_t)
  end if
  if (isatt(TIME,"avg_period")) then
     delete(TIME@avg_period)
  end if
  if (isatt(TIME,"prev_avg_period")) then
     delete(TIME@prev_avg_period)
  end if

  if (typeof(time).eq."float") then
      return(dble2flt(TIME))     ; retain meta data 
  else
      return(TIME)
  end if
end

; --------------------------
undef("time_to_newtime_fix")
function time_to_newtime_fix(time[*]:numeric, new_time_units[1]:string)
;
; Change a "udunits" recognized time unit to a new (different)
; "udunits" recognized time unit. This is identical to 
; time_to_newtime, except it uses a fixed version of ut_calendar
; that was added in NCL V6.4.0. This function is still being tested,
; so hence the "_fix" appended.
;
; Example:
;  TIME = time_to_newtime_fix(time, "days since 1950-01-01 00:00" ) ; Time(time) 
;  time = time_to_newtime_fix(time, "hours since 1800-01-01 00:00") ; time(time) 
begin
  ntim   = dimsizes(time)
  date   = cd_calendar( time, 0)
 ;print(date(:,0)+"  "+date(:,1)+"  "+date(:,2)+"  "+date(:,3))
  TIME   = ut_inv_calendar_fix(toint(date(:,0)),toint(date(:,1)) \
                              ,toint(date(:,2)),toint(date(:,3)) \
                              ,toint(date(:,4)),todouble(date(:,5)),new_time_units, 0)

  if (isatt(time,"calendar")) then
      TIME@calendar = time@calendar
  end if

  TIME!0        = "time"
  TIME&time     =  TIME
                          ; delete attributes that may no longer be appropriate
  if (isatt(TIME,"actual_range")) then
     delete(TIME@actual_range)
  end if
  if (isatt(TIME,"delta_t")) then
     delete(TIME@delta_t)
  end if
  if (isatt(TIME,"avg_period")) then
     delete(TIME@avg_period)
  end if
  if (isatt(TIME,"prev_avg_period")) then
     delete(TIME@prev_avg_period)
  end if

  if (typeof(time).eq."float") then
      return(dble2flt(TIME))     ; retain meta data 
  else
      return(TIME)
  end if
end

undef("sigma_interface")
function sigma_interface (sigma[*]:numeric, sigma_top[1]:numeric)
; -------------------------------------------------
; create "interface" sigma levels to mimic the
;  CAM models interface hybrid levels
; -------------------------------------------------
local ksig, SIGMA, sigmai, sigName
begin

  ksig  = dimsizes(sigma)
  if (sigma_top.gt.sigma(0)) then
      print("sigma_interface: sigma_top must be less than/equal to sigma(0)")
      print("                 sigma_top= "+sigma_top+"  ; sigma(0)="+sigma(0))
      exit
  end if
                         ; 'super' sigma .. include top and bottom
  SIGMA = new(ksig+2, typeof(sigma),"No_FillValue")
  SIGMA(0)      = sigma_top        ; anything less than sigma(0)
  SIGMA(1:ksig) = (/ sigma /)
  SIGMA(ksig+1) = 1.0
;;print(SIGMA)
                                   ; interface sigma levels
  sigmai = (SIGMA(1:) + SIGMA(0:ksig))*0.5
  sigmai(0)     = SIGMA(0)         ; force top for full thickness
  sigmai(ksig)  = 1.0              ; force bot
  copy_VarAtts(sigma, sigmai)
  sigmai@long_name = "Sigma at model interface layers"

  return(sigmai)
end

undef("dpres_sigma")
function dpres_sigma (ps:numeric, sigma[*]:numeric, sigma_top[1]:numeric)
; -------------------------------------------------
; use the  dpres_hybrid_ccm function to get layer thicknesses
; -------------------------------------------------

local sigmai, A, P0, dp_sigma, rank_ps, nd, psName, sigName
begin
  sigmai    = sigma_interface (sigma, sigma_top)
  A         = sigmai
  A         = 0.0
  P0        = 0.0

  dp_sigma  =  dpres_hybrid_ccm(ps,P0,A,sigmai)
  dp_sigma@long_name = "pressure thickness"
  if (isatt(ps, "units")) then
      dp_sigma@units = ps@units
  end if

  rank_ps    = dimsizes( dimsizes(ps) )
  if (rank_ps.eq.3) then
      psName     = getvardims(ps)      ; dimension names
      dp_sigma!0 = ps!0
      dp_sigma!2 = ps!1
      dp_sigma!3 = ps!2
      do nd=0,2
         if (.not.ismissing(psName(nd)) .and. \
             iscoord(ps,psName(nd))) then
             dp_sigma&$psName(nd)$ = ps&$psName(nd)$
         end if
      end do

      sigName   = getvardims(sigma)
      if (.not.ismissing(sigName) .and. iscoord(sigma,sigName)) then
          dp_sigma!1         = sigName
          dp_sigma&$sigName$ = sigma&$sigName$
      else
          dp_sigma!1 = "sigma"
          dp_sigma&sigma = sigma
      end if
  end if

  return(dp_sigma)
end
;---------------------------------------------------------------------------
undef("dpres_plevel_Wrap")
function dpres_plevel_Wrap(plev[*]:numeric, psfc:numeric, ptop[1]:numeric, opt:integer)
; -------------------------------------------------
; use the  dpres_plevel function to get layer thicknesses
; -------------------------------------------------

begin
  dp_plevel           = dpres_plevel(plev, psfc, ptop, 0) 

; add meta data

  dp_plevel@long_name = "pressure thickness"
  if (isatt(psfc, "units")) then
      dp_plevel@units = psfc@units
  end if

  plevName   = getvardims(plev)
  psfcName   = getvardims(psfc)      ; dimension names
  rank_psfc  = dimsizes( dimsizes(psfc) )

  if (rank_psfc.eq.1) then
      if (.not.ismissing(plevName)) then
          dp_plevel!0          = plevName
          if (iscoord(plev,plevName)) then
              dp_plevel&$plevName$ = plev&$plevName$
          end if
      else
          dp_plevel!0   = "plev"
          dp_plevel&plev=  plev
      end if
  end if

  if (rank_psfc.eq.2) then                           ; (lat,lon) => (0,1)
      do nd=0,1
         if (.not.ismissing(psfcName(nd))) then
             dp_plevel!(nd+1)   = psfcName(nd)       ; psfc!nd
             if (iscoord(psfc,psfcName(nd))) then
                 dp_plevel&$psfcName(nd)$ = psfc&$psfcName(nd)$
             end if
         end if
      end do

      if (.not.ismissing(plevName)) then
          dp_plevel!0          = plevName
          if (iscoord(plev,plevName)) then
              dp_plevel&$plevName$ = plev&$plevName$
          end if
      else
          dp_plevel!0   = "plev"
          dp_plevel&plev=  plev
      end if
  end if

  if (rank_psfc.eq.3) then                          ; (time,lat,lon) => (0,1,2)
      if (.not.ismissing(psfcName(0))) then
           dp_plevel!0   = psfcName(0)              ; psfc!0 
           if (iscoord(psfc,psfcName(0))) then
               dp_plevel&$psfcName(0)$ = psfc&$psfcName(0)$
           end if

           do nd=1,2
              if (.not.ismissing(psfcName(0))) then
                  dp_plevel!(nd+1)   = psfcName(nd)       ; psfc!nd
                  dp_plevel&$psfcName(nd)$ = psfc&$psfcName(nd)$
              end if
           end do

           if (.not.ismissing(plevName) .and. iscoord(plev,plevName)) then
               dp_plevel!1          = plevName
               dp_plevel&$plevName$ = plev&$plevName$
           else
               dp_plevel!1   = "plev"
               dp_plevel&plev=  plev
           end if
      end if
  end if

  return(dp_plevel)
end
;---------------------------------------------------
; Rob Nicholas [U. Washington] & D. Shea
undef("pattern_cor2")
function pattern_cor2 (x[*][*],y[*][*],w, opt:integer)
; This function should NOT be called directly by  users.
; Please use "pattern_cor"
; --
; Compute the pattern correlation between two fields
; (lat,lon)  .... wgt(lat)

local dimx, dimy, dimw, wgt, WGT, sumWGT, xAvgArea, xAnom, yAnom \
    , i, j, W1D, z1d, xyCov, xAnom2, yAnom2, r, rFill 
begin
  if (isatt(x,"_FillValue")) then
      if (all(ismissing(x))) then
          rFill = (/ x@_FillValue /)
          rFill@_FillValue = rFill
          return(rFill)
      end if
  end if
  if (isatt(y,"_FillValue")) then
      if (all(ismissing(y))) then
          rFill = (/ y@_FillValue /)
          rFill@_FillValue = rFill
          return(rFill)
      end if
  end if
                             ; x and y must have the same sizes
  dimx  = dimsizes(x)
  dimy  = dimsizes(y)
  if (.not.all(dimx.eq.dimy)) then
      print("pattern_cor: Fatal: x and y do not have the same dimension sizes")
      print("       dimx: "+dimx+"    dimy="+dimy)
      exit
  end if 

  dimw  = dimsizes(w)      
  rankw = dimsizes(dimw)
  if (rankw.gt.2) then
      print("pattern_cor: Fatal: w can be a scalar, w[*] or w[*][*]")
      print("      rankw: "+rankw)
      exit
  end if

  if (rankw.eq.2 .and. .not.all(dimx.eq.dimw)) then
      print("pattern_cor: Fatal: w[*][*] must have the same dimensions as x[*][*]")
      print("       dimx: "+dimx+"    dimw="+dimw)
      exit
  end if
                                     ; w can be w[1], w[*] or w[*][*]
  if (rankw.eq.1) then
      if (dimw.eq.1) then            ; if w is w(1) (scalar) set to 1.0 
          WGT = new(dimx, typeof(w),"No_FillValue")
          WGT = 1.0
      end if
      if (dimx(0).eq.dimw .and. .not.isvar("WGT")) then
          WGT = conform(x, w, 0)     ; broadcast dimensions to match x/y
      end if
  else                               ; must be 2D
      WGT     = w
  end if

; if x/y  has _Fillvalue attribute; set WGT=0.0 where x or y = _FillValue

  if (isatt(x,"_FillValue") .or. isatt(y,"_FillValue")) then
      W1D  = ndtooned(WGT)
      if (isatt(x,"_FillValue") .and. any(ismissing(x)) ) then
          z1d    = ndtooned(x)
          i      = ind(ismissing(z1d))
          W1D(i) = 0.0
      end if
      if (isatt(y,"_FillValue") .and. any(ismissing(y)) ) then
          z1d    = ndtooned(y)
          j      = ind(ismissing(z1d))
          W1D(j) = 0.0
      end if
      WGT  = onedtond(W1D, dimx)
  end if

  if (opt.eq.0) then                    ; centered correlation
      sumWGT   = sum(WGT)
      xAvgArea = sum(x*WGT)/sumWGT      ; weighted area average
      yAvgArea = sum(y*WGT)/sumWGT

      xAnom    = x - xAvgArea           ; anomalies
      yAnom    = y - yAvgArea

      xyCov    = sum(WGT*xAnom*yAnom)
      xAnom2   = sum(WGT*xAnom^2)
      yAnom2   = sum(WGT*yAnom^2)
  else 
      xyCov    = sum(WGT*x*y)
      xAnom2   = sum(WGT*x^2)
      yAnom2   = sum(WGT*y^2)
  end if


  if (xAnom2.gt.0.0 .and. yAnom2.gt.0.0) then
      r   = xyCov/(sqrt(xAnom2)*sqrt(yAnom2))
  else
      if (isatt(x,"_FillValue")) then
          r = x@_FillValue
      else
          if (isatt(y,"_FillValue")) then
              r = y@_FillValue
          else            
              r = -999.0 
          end if
      end if
  end if

  if (opt.eq.0) then    
      r@long_name = "pattern correlation (centered)"
  else
      r@long_name = "pattern correlation (uncentered)"
  end if
 ;r@units     = ""

  return(r)
end
;---------------------------------------------------
; Rob Nichols [U. Washington] & D. Shea
undef("pattern_cor")
function pattern_cor (x, y, w, opt:integer)
; Compute the centered or uncentered pattern correlation 
; between two fields
; (lat,lon)  .... wgt(1) or wgt(lat) or wgt(lat,lon)

local dimx, dimy, rankx, ranky, nt, kl, xc, rpc, rFill
begin
  if (isatt(x,"_FillValue")) then
      if (all(ismissing(x))) then
          rFill = (/ x@_FillValue /)
          rFill@_FillValue = rFill
          return(rFill)
      end if
  end if
  if (isatt(y,"_FillValue")) then
      if (all(ismissing(y))) then
          rFill = (/ y@_FillValue /)
          rFill@_FillValue = rFill
          return(rFill)
      end if
  end if
                             ; x and y must have the same sizes
  dimx  = dimsizes(x)
  dimy  = dimsizes(y)
  dimw  = dimsizes(w)         ; if w is scalar set to 1.0 

  rankx = dimsizes(dimx)
  ranky = dimsizes(dimy)
  rankw = dimsizes(dimw)

  if (rankx.ne.ranky) then
      print("pattern_cor: Fatal: x and y do not have the same rank")
      print("      rankx: "+rankx+"   ranky="+ranky)
      exit
  end if
  if (rankx.ge.5) then
      print("pattern_cor: Fatal: function only works with 2D, 3D or 4D")
      print("       rank: "+rankx+"D")
      exit
  end if
  if (.not.all(dimx.eq.dimy)) then
      print("pattern_cor: Fatal: x and y do not have the same dimension sizes")
      print("       dimx: "+dimx+"    dimy="+dimy)
      exit
  end if
  if (rankw.gt.2) then
      print("pattern_cor: Fatal: w can be a scalar, w[*] or w[*][*]")
      print("      rankw: "+rankw)
      exit
  end if

  if (rankx.eq.2) then
      return( pattern_cor2 (x,y,w,opt) )    ; scalar returned
  end if

  if (rankx.eq.3) then
      rpc = new ( dimx(0), typeof(x), "No_FillValue")
      do nt=0,dimx(0)-1
         rpc(nt) = pattern_cor2 (x(nt,:,:),y(nt,:,:),w,opt)
      end do
      xc = x(:,0,0)
      copy_VarCoords(xc, rpc)
      return( rpc )
  end if

  if (rankx.eq.4) then
      rpc = new ( (/dimx(0),dimx(1)/), typeof(x), "No_FillValue")
      do nt=0,dimx(0)-1
        do kl=0,dimx(1)-1
           rpc(nt,kl) = pattern_cor2 (x(nt,kl,:,:),y(nt,kl,:,:),w,opt)
        end do
      end do
      xc = x(:,:,0,0)
      copy_VarCoords(xc, rpc)
      return( rpc )
  end if
end
; ----------------------------------------------------
undef("rm_single_dims")
function rm_single_dims (x)
; remove singleton (degenerate) dimensions. A singleton dimension is size 1.
; NCL philosophy: do not 'lose' information.
local dimx, dimy, rankx, dnamx, y, m, n
begin
  dimx  = dimsizes(x)

  if (.not.any(dimx.eq.1)) then
      return(x)                     ; no degenerate dimensions
  end if

  rankx = dimsizes(dimx)
  dnamx = getvardims(x)             ; dim names

  if (all(dimx.eq.1)) then
      dimy = 1                      ; all degenerate dimensions
  else
      dimy = dimx(ind(dimx.ne.1))
  end if
  y     = onedtond(ndtooned(x), dimy)

  m     = -1
  do n=0,rankx-1
    if (.not.ismissing(dnamx(n))) then
         if (dimx(n).ne.1) then
              m   = m+1
              y!m = dnamx(n)
              if (iscoord(x, dnamx(n))) then
                 y&$dnamx(n)$ = x&$dnamx(n)$ ; attach coord
              end if
          else                          
              if (iscoord(x, dnamx(n))) then
                 y@$dnamx(n)$ = x&$dnamx(n)$ ; attach singleton coord as attribute @
              end if
          end if
      end if
  end do
  copy_VarAtts(x,y)

  return(y)
end
;------
; Thirteen pt (decadal)
undef("smth13_n")
function smth13_n (x:numeric, ndim)    ; smth13
local wgts, smX
begin
        wgts     = (/ 1,6,19,42,71,96,106,96,71,42,19,6,1 /)*1.0
        wgts     = wgts/sum(wgts)
        smX      = wgt_runave_n_Wrap(x, wgts, 1, ndim)   ; reflective end pts
        return(smX)
end

;------
; Names and abbreviations for months
undef("month_name")
function month_name (opt[1]:integer)
;  monName = month_name(-1)    ; return with extra January 
begin
 if (opt.eq.0) then
     return((/"Jan","Feb","Mar","Apr","May","Jun"\
             ,"Jul","Aug","Sep","Oct","Nov","Dec"/))         ; len=12
     else if (opt.eq.-1) then
        return((/"Jan","Feb","Mar","Apr","May","Jun"      \
                ,"Jul","Aug","Sep","Oct","Nov","Dec","Jan"/)); len=13
     else if (opt.eq.1) then
        return((/"January","February","March","April" \
                ,"May","June","July","August"         \
                ,"September","October","November","December"/)) ; len=12
     end if     ; opt= 1
     end if     ; opt=-1
  end if        ; opt= 0  
end

;------
; Names and abbreviations for seasons
undef("season_name")
function season_name (opt[1]:integer)
;  seaName = season_name(1)    ; 5-month season
begin
 if (opt.eq.0) then
       return((/"DJF","JFM","FMA","MAM","AMJ","MJJ" \
               ,"JJA","JAS","ASO","SON","OND","NDJ"/))       ; length 12
    else if (opt.eq.-1) then
       return((/"DJF","JFM","FMA","MAM","AMJ","MJJ" \
               ,"JJA","JAS","ASO","SON","OND","NDJ","DJF"/)) ; length 13
    else if (opt.eq.1) then
       return((/"NDJFM","DJFMA","JFMAM","FMAMJ","MAMJJ","AMJJA" \
               ,"MJJAS","JJASO","JASON","ASOND","SONDJ","ONDJF" /)) ; len 12
    else if (opt.eq.2) then
       return((/"DJF","MAM","JJA","SON"/))                   ; len 4
    else if (opt.eq.3) then
       return((/"Winter","Spring","Autumn","Summer"/))       ; len 4
    else if (opt.eq.4) then
       return((/"Summer","Autumn","Winter","Spring"/))       ; len4
    end if     ; opt= 4
    end if     ; opt= 3
    end if     ; opt= 2
    end if     ; opt= 1
    end if     ; opt=-1
  end if       ; opt= 0  
end
;---------------------------------------------------
undef("mon_fullyear")
function mon_fullyear(x:numeric, opt)
;
; Expand a variable containing monthly data to full (12 month) years
; The added months will be set to _FillValue.
;
; Example: Take a variable that may span (say)
; 190308 to 201105 and expand it to span 190301 to 201112.
; The extra values will be filled with _FillValue.
;
; This assumes that the input variable will have a
; time coordinate variable with units acceptable to cd_calendar
;
; generically: Only the 'time' dimension must be present.
;              It does not have to be named 'time'
;              All other meta data are preserved.
; x(time), x(time,J), x(time,N,M), x(time,K,N,M)
;
; If the input is already full years, the variable is returned unaltered.
;
; f = addfile(...)  
; x = mon_fullyear( f->X, 0)

begin
  dimx  = dimsizes(x)          ; dimsizes x
  rankx = dimsizes(dimx)       ; rank of x

  if (rankx.gt.4) then
      print("mon_fullyear_n: currently this function only works up to rank 4")
      exit
  end if

  dnamx = getvardims( x )      ; dimension names

  ntimx = dimx(0)              ; # input time steps
  ymdhms= cd_calendar(x&$dnamx(0)$, 0)

  if (isatt(x&$dnamx(0)$, "calendar")) then
      ymdhms@calendar = x&$dnamx(0)$@calendar
  end if
 
  yyyy  = toint( ymdhms(:,0) )
  mm    = toint( ymdhms(:,1) )

  nmos  = 12
  if ((ntimx%nmos).eq.0 .and. mm(0).eq.1 .and. mm(ntimx-1).eq.nmos) then
     ;x@year_mon = YYYY*100 + MM 
      return(x)                ; must be full years
  end if 
                               ; must be partial year
  dd    = toint( ymdhms(:,2) )
  hh    = toint( ymdhms(:,3) )
  mn    = toint( ymdhms(:,4) )
  sc    =        ymdhms(:,5)  

  tunits= x&$dnamx(0)$@units

  yrStrt= yyyy(0)
  yrLast= yyyy(ntimx-1)

  mmStrt= mm(0)
  mmLast= mm(ntimx-1)

  NYRS  = yrLast-yrStrt+1
  NTIM  = NYRS*nmos            ; full year-month
  
  YYYY  = new( NTIM, "integer" , "No_FillValue")
  MM    = new( NTIM, "integer" , "No_FillValue")
  DD    = new( NTIM, "integer" , "No_FillValue")
  HH    = new( NTIM, "integer" , "No_FillValue")
  MN    = new( NTIM, "integer" , "No_FillValue")
  SC    = new( NTIM, typeof(sc), "No_FillValue")

  nStrt = 0
  nLast = nmos-1
  do year=yrStrt,yrLast
     YYYY(nStrt:nLast) = year
     MM(nStrt:nLast)   = ispan(1,nmos,1) 
     DD(nStrt:nLast)   = 1 
     HH(nStrt:nLast)   = 0 
     MN(nStrt:nLast)   = 0
     SC(nStrt:nLast)   = 0

     nStrt = nStrt+nmos
     nLast = nLast+nmos
  end do
  
  TIME  =  (/ cd_inv_calendar(YYYY,MM,DD,HH,MN,SC,tunits, 0) /)
  TIME@units = tunits
  TIME!0     = dnamx(0)
  TIME&$dnamx(0)$ =  TIME        ; make coordinate variable

  if (isatt(x&$dnamx(0)$, "calendar")) then
      TIME@calendar = x&$dnamx(0)$@calendar
  end if

  DIMX    = dimx
  DIMX(0) = NTIM
  X     = new (DIMX, typeof(x), getFillValue(x))

  nStrt = ind(YYYY.eq.yrStrt .and. MM.eq.mmStrt)
  nLast = ind(YYYY.eq.yrLast .and. MM.eq.mmLast)
  
  if (rankx.eq.1) then
      X(nStrt:nLast) = (/ x /)
     else if (rankx.eq.2) then
         X(nStrt:nLast,:) = (/ x /)
         copy_VarCoords(x(0,:), X(0,:))
     else if (rankx.eq.3) then
         X(nStrt:nLast,:,:)   = (/ x /)
         copy_VarCoords(x(0,:,:), X(0,:,:))
     else if (rankx.eq.4) then
         X(nStrt:nLast,:,:,:) = (/ x /)
         copy_VarCoords(x(0,:,:,:), X(0,:,:,:))
     end if
     end if
     end if
  end if

  copy_VarAtts(x, X)
  X!0          = dnamx(0)
  X&$dnamx(0)$ = TIME

  return(X)
  
end
;----------------------------------------------------------
undef("stat_mode")
function stat_mode(A[*]:numeric)
;
; returns mode value(s) with attributes which indicate the
;                       number of modes (@nmode)
;                       count of mode occurences (@count)
;                       number of non-missing values in 'A' (@npts)
;---
; 'A' need not be in ascending order. The function will sort.
; Care should be used when A is type float or double.
; A value of 12.34567 is not the same as 12.34568 
; A suggestion is to preprocess the data. Say that 
; 2 decimal places is 'close enough'. 
;         scale = 100.0
;         A = toint(A*scale)/scale 
;       or
;         A = round(A*scale, 0)/scale
;---
; Examples: 
;
; a0     = (/ -1, 7, 7, 2, 2, 2, -1, 7, -3, -3 /)
; mode0  = (/2,7/), mode0@nmode=2, mode0@count=3, mode0@npts=10
;
; a1     = (/123, -1, 7, 7, 2, 2, 2, -1, 7, -3, -3, -456 /)
; mode1  = (/2,7/), mode1@nmode=2, mode1@count=3, mode1@npts=12
;
; a2     = (/  1, 1, 1, 1, 1, 0,  2, 2,  2,  2 /)
; mode2  = 1      , mode2@nmode=1, mode1@count=5, mode1@npts=10
;
; a3     = (/  0, 0,-1,-1, 9, 9,  3, 3,  7,  7 /)
; mode3  = (/-1, 0, 3, 7, 9/), mode3@nmode=5, mode3@count=2, mode3@npts=10
;
; a4     = ispan(1,10,1)
; mode4  = (/1,2,3,4,5,6,7,8,9,10/), mode4@nmode=10, mode4@count=1, mode4@npts=10
;
; a5     = (/1,2,3,4, 5,5, 7,8,9,10/)
; mode5  = 5, mode5@nmode=1, mode5@count=2, mode5@npts=10
;
local i, ni, a, na, freq, FREQ, n, nm, ii, nseq \
    , node, nmode, mode_count, val, ifmx
begin

  if (all(ismissing(A))) then
      mode       = -999
      mode@npts  = 0
      mode@nmode = -999
      mode@freq  = -999
      mode@_FillValue = -999
      return (mode)
  end if

  i    = ind(.not.ismissing(A))
  ni   = dimsizes(i)
  if (ni.eq.1) then
      mode       = A(i(0)) 
      mode@npts  = 1
      mode@nmode = 1
      mode@count = 1
      return (mode)
  end if

  a    = A(i)           ; local
  na   = ni             ; # of sorted non-missing value
  qsort( a )


  freq = new (na, "integer","No_FillValue")
  nm   = new (na, "integer","No_FillValue")
  val  = new (na, typeof(a),"No_FillValue")
  
  freq = 1      ; initialize (array syntax)
  nm   = 0      ; number of unique numbers
  ii   = 0      ; index
  

  do n=1,na-1
     if (a(n-1).eq.a(n)) then
         freq(ii) = freq(ii) + 1
         val(ii)  = a(n)
     else
         nm(ii) =  n-1
         ii     = ii+1
     end if
  end do

  nm(ii)     = n -1
  nseq       = ii+1

  FREQ       = freq(:ii)          ; convenience
  mode_cnt   = max(FREQ)
  ifmx       = ind(FREQ.eq.mode_cnt)       
  if (all(FREQ.eq.1)) then
      mode   = a
  else
     mode    = val(ifmx)          ; mode values
  end if
  mode@npts  = na                 ; number of pts used
  mode@nmode = dimsizes(mode)     ; num(FREQ.eq.mode_cnt)
  mode@count = mode_cnt
  
  return(mode)
end
;============================================
undef ("reg_multlin_stats")
function reg_multlin_stats(Y[*]:numeric, XP:numeric, opt) ; should be float or double
;
; Nomenclature:
; Y     - dependent variable (size NY)
;         missing values (_FillValue) are not allowed.
; XP    - one [*] or more [*][*] independent variables.
;         Missing values (_FillValue) are not allowed.
;         The size of the leftmost dimension of XP must be the
;         same size as Y[*]. The rightmost dimension for [*][*]
;         contains the independent variables.
; opt   - options [type logical: ie, True or False]
;         opt = False              .... no options               
;         opt = True
;         opr@print_data = True    .... print input Y and XP in table form
;         opr@print_anova= True    .... print ANOVA information
;
; See: 
; Wilks, D.S. (2006): Statistical Methods in the Atmospheric Sciences
;                     Academic Press (2nd Edition)
; Davis, J.C. (2002): Statistics and Data Analysis in Geology
;                     Wiley (3rd Edition)   ; pgs 462-470 
;
; Very readable discussions:
; http://www.ltrr.arizona.edu/~dmeko/notes_11.pdf
; http://reliawiki.org/index.php/Multiple_Linear_Regression_Analysis
; http://homepages.rpi.edu/~tealj2/stat03.pdf
; ---
;
local dimXP, N, NP, NY, M, T, Y, X, b, B, rankXP         \ ; local is
    , Yavg, Yvar, Ystd, Xavg, Xvar, Xstd, Yest, n, m     \ ; not necessary
    , Yres, SST, SSE, SSR, MST, MSE, MSR, SE, Frat       \
    , r2, ra, r, fuv, XXt, XXTi, varcovx, df, stderr, tval, pval \
    , opt_wm, fmt_XXt, fmt_XXti, fmt_covx, opt_pd, fmt_data      \
    , typeY, typeXP, typeYXP
begin

       dimXP   = dimsizes(XP)
       NX      = dimXP(0)              ; number of rows ('observations')    
       N       = dimsizes(Y)

       rankXP  = dimsizes(dimXP)

; error checks on input

       if (N.NE.NX) then              
           print("reg_multlin_stats: number of rows of Y and XP  must match")
           print("                   NY="+N +"  NXP="+NX           )
           exit
       end if

       if (rankXP.gt.2) then
           print("reg_multlin_stats: rank of XP > 2: rankXP="+rankXP)
           exit
       end if

       if (isatt(Y,"_FillValue")  .and. any(ismissing(Y))) then
           print("reg_multlin_stats: Y has missing values. Not allowed!")
           exit
       end if

       if (isatt(XP,"_FillValue") .and. any(ismissing(XP))) then
           print("reg_multlin_stats: XP has missing values. Not allowed!")
           exit
       end if
           
; input OK: onward

       if (rankXP.eq.2) then
           NP  = dimXP(1)              ; number predictor variables   
       else
           NP  = 1                     ; one predictor
       end if

       typeY   = typeof(Y) 
       typeXP  = typeof(XP) 
       if (typeY.eq."double" .or. typeXP.eq."double") then
           typeYXP = "double"
       else
           typeYXP = "float"           ; default 
       end if

       if (opt .and. isatt(opt,"print_data") .and. opt@print_data) then
           print(" ")
           opt_pd = True
           opt_pd@title  = "----- reg_multlin_stats: Y, XP -----"

           data   = new( (/N,NP+1/), typeYXP)

           data(:,0)  = (/ Y /)

           if (NP.eq.1) then
               data(:,1)  = (/ XP /)
           else
               data(:,1:) = (/ XP /)
           end if

           fmt_data   = "f11.2"
           if (isatt(opt,"fmt_data")) then
               fmt_data  = opt@fmt_data        ; eg: "f8.3", "e13.5",....
           end if
           write_matrix (data, (NP+1)+fmt_data , opt_pd)
           print(" ")
           delete(data)        ; not needed
       end if
       
; Create the necessary arrays. X is a "design matrix"  
;       http://en.wikipedia.org/wiki/Design_matrix
;
; Most multiple regression models include a constant term.
;   This ensures that the model will be "unbiased"
;   --i.e., the mean of the residuals will be exactly zero.

       M       = NP+1                     ; extra column for design matrix
                                          ; required ordering for reg_multlin 

       X   = new ( (/M,N/) , typeYXP, getFillValue(XP))

       X(0,:)  = 1.0

       if (NP.eq.1) then
           X(1,:)  = XP                   ; one predictor
       else
           do m=0,NP-1  
              X(m+1,:)  = XP(:,m)         ; m refers to predictor variables
           end do
       end if

; Calculate the partial regression coefficients: b->beta_hat
;      b = (X'X)^-1 X'y  (theoretically)
; http://www.ncl.ucar.edu/Document/Functions/Built-in/reg_multlin.shtml
                                         
       b       = reg_multlin(Y,X,False)   ; partial regression coef 
                                          ; unstandardized...depend on units
       delete(b@constant)                 ; extraneous attribute

; Assorted calculations

       Yavg    = avg(Y)
       Yvar    = variance(Y)         ; Unbiased estimate the variance 
       Ystd    = sqrt(Yvar)

       Xavg    = new(NP, typeof(b), "No_FillValue")  
       Xvar    = new(NP, typeof(b), "No_FillValue")
       Xstd    = new(NP, typeof(b), "No_FillValue")

       bstd    = new(M,  typeof(b), "No_FillValue") ; standardized regression coef
                                     ; describe what the partial regression coef. 
                                     ; would equal if all variables had the same 
                                     ; standard deviation.
       do n=0,NP-1                 
          Xavg(n)   = avg(X(n+1,:))
          Xvar(n)   = variance(X(n+1,:))
          Xstd(n)   = sqrt(Xvar(n))
          bstd(n+1) = b(n+1)*Xstd(n)/Ystd       ; don't do b(0)
       end do

; Calculate model estimates: Yest = b(0) + b(1)*   + b(2)* + .....

       Yest = new(N,typeYXP)
       do n=0,N-1
          Yest(n) = b(0) + sum(b(1:)*X(1:,n))   ; array syntax 
       end do

; ANOVA table info. (Eg, Wilks pg 185 & pg 197)
;       Sums of squares , degrees of freedom, mean squares, F statistic
;       Other statistics are also calculated.
;
; http://reliawiki.org/index.php/ANOVA_Calculations_in_Multiple_Linear_Regression
;        Mean squares are obtained by dividing the sum of squares 
;        with their associated degrees of freedom. 

       dof  = N-NP-1 

       Yres = Yest-Y               ; residuals (array operation)

                                   ; SS ==> Sum of Squares 
       SST  = sum((Y-Yavg)^2)      ; total SS      [dof=N-1]
       SSR  = sum((Yest-Yavg)^2)   ; regression SS [dof=NP] ; (aka: model explained SS=>SSM)
       SSE  = sum(Yres^2)          ; residual SS   [dof=N-NP-1]
 
       MST  = SST/(N-1)            ; Mean SST      [dof=N-1]
       MSR  = SSR/NP               ; Mean SSR      [dof=NP]
       MSE  = SSE/dof              ; Mean SSE      [dof=(N-NP-1)] 

       RSE  = sqrt(MSE)            ; residual standard error
                                   
       Frat = MSR/MSE              ; R dof=(M,N-NP-1) 
                                   ; F-Test is an overall (global) test of the model’s fit.
                                   ; If at least one variable has a significant coefficient,
                                   ; then the model fit should be significant.
                                   ; The F-statistic might be interpreted as the variation 
                                   ; explained by the regression relative to the variation
                                   ; not explained by the regression.
                                   
       r2   = SSR/SST              ; r2 = coefficient of determination. It is
                                   ;      the square of the Pearson correlation 
                                   ;      coefficient between the observed 
                                   ;      and modeled (predicted) data values
                                   ;      of the dependent variable. It is
                                   ;      another measure of 'goodness of fit.'
                                   ; biased high, particularly for small N

       r2a  = r2-(1-r2)*NP/tofloat(dof)   ; adjusted r2... better for small N
       r    = sqrt(r2)             ; multiple (overall) correlation
       fuv  = 1.0-r2               ; fraction of variance *unexplained* 

; Determine (a) standard error of coefficients; (b) t-values; (c)  p-values

       XXt     = X#transpose(X)    ; (M,M); opposite of text books 
       XXti    = inverse_matrix(XXt)
       varcovx = MSE*XXti          ; variance-covariance matrix
                                   ; of estimated regression coefficients

       if (opt .and. isatt(opt,"debug") .and. opt@debug) then ; used for debugging only
           print(" ")
           print("-----  DEBUG ---------")
           print(" ")
           opt_wm = True
           opt_wm@title  = "-----   XXt  ---------"
           fmt_XXt = "f11.2"                     
           if (isatt(opt,"fmt_XXt")) then
               fmt_XXt  = opt@fmt_XXt            ; "f8.2", "f12.4", "e13.5", ...
           end if
           write_matrix (XXt, M+fmt_XXt , opt_wm)

           opt_wm@title  = "-----   XXti ---------"
           fmt_XXti = "f11.2"
           if (isatt(opt,"fmt_XXti")) then
               fmt_XXti = opt@fmt_XXti
           end if
           write_matrix (XXti, M+fmt_XXti , opt_wm)

           opt_wm@title  = "----- varcovx --------"
           fmt_covx = "f11.2"
           if (isatt(opt,"fmt_covx")) then
               fmt_covx = opt@fmt_covx
           end if
           write_matrix (varcovx, M+fmt_covx , opt_wm)

           print(" ")
       end if
           
       stderr = new( M, typeYXP)
       tval   = new( M, typeYXP)
       pval   = new( M, typeYXP)

       df  = N-NP
       
       do m=0,M-1
          stderr(m) = sqrt(varcovx(m,m))  
          tval(m)   = b(m)/stderr(m)
          pval(m)   = betainc( df/(df+tval(m)^2), df/2.0, 0.5)
       end do

       if (opt .and. isatt(opt,"print_anova") .and. opt@print_anova) then
           print(" ")
           print("------- ANOVA information-------- ")
           print(" ")
           print("SST="+SST+"  SSR="+SSR+"  SSE="+SSE)
           print("MST="+MST+"  MSR="+MSR+"  MSE="+MSE+" RSE="+RSE)
           print("F-statistic="+Frat +" dof=("+NP+","+(N-NP-1)+")")
           print("          -------                 ")
         ;;print("r2="+r2+"  r="+r+"  ra="+ra+"  fuv="+fuv)  
         ;;print("          -------                 ")
         ;;print("stderr, tval, pval: "+stderr+"    "+tval+"    "+pval)
           print(" ")
       end if

; associate assorted information as attributes of 'b'

       b@long_name = "multiple regression coefficients"
       b@model     = "Yest = b(0) + b(1)*X1 + b(2)*X2 + ...+b(M)*XM"

       b@N      = N             ; # of 'observations'
       b@NP     = NP            ; # of predictors
       b@M      = M             ; design matrix size
       b@bstd   = bstd          ; standardized coefficients

       b@SST    = SST           ; [1]
       b@SSE    = SSE
       b@SSR    = SSR

       b@MST    = MST
       b@MSE    = MSE
       b@MSE_dof = dof 
       b@MSR    = MSR

       b@RSE    = RSE           ; [1]
       b@RSE_dof= N-M-1         ; [1]
       
       b@F      = Frat          ; [1]
       b@F_dof  = (/NP,dof/)    ; [2]

       df1      = b@F_dof(0)
       df2      = b@F_dof(1)
       b@F_pval = ftest(MSR, df1+1, MSE, df2+1, 0)*0.5   ; [1]

       b@r2     = r2            ; [1]
       b@r      = r             ; [1]
       b@r2a    = r2a           ; [1]

       b@fuv    = fuv

       b@Yest   = Yest          ; [NY]
       b@Yavg   = Yavg          ; [1]
       b@Ystd   = Ystd          ; [1]

       b@Xavg   = Xavg          ; [1]
       b@Xstd   = Xstd          ; [1]

       b@stderr = stderr        ; [M]
       b@tval   = tval          ; [M]
       b@pval   = pval          ; [M]

       return(b)
end
;============================================
undef("regline_stats")
function regline_stats(x[*]:numeric, y[*]:numeric)
;
; rc = regline_stats(x, y)
;
; Nomenclature:
; x     - independent variable.
; y     - dependent variable (same size as <em>x</em>)
;
; Missing values (_FillValue) are allowed. They are ignored.
;
; See: 
; Wilks, D.S. (2006): Statistical Methods in the Atmospheric Sciences
;                     Academic Press (2nd Edition)  ; pgs 180-204
; Davis, J.C. (2002): Statistics and Data Analysis in Geology
;                     Wiley (3rd Edition)   ; pgs 215-218 
; Example:
; http://www.stat.ucla.edu/~hqxu/stat105/pdf/ch11.pdf

local nx, ny, ii, nii, RC, rc, Sxx, Sxy, qslp, qint, zz, t025, t975, tnull
begin

; Get the lengths of the x and y arrays.
   nx  = dimsizes(x)
   ny  = dimsizes(y)

; Check if those lengths match.
   if (nx.ne.ny)then
       print("regline_stats: x and y must be the same length!")
       print("               nx="+nx+"   ny="+ny)               
       exit
   end if

; check
   if (all(x.eq.y)) then
       print("regline_stats: x and y are exactly the same: will lead to division by 0")
       rc = 1e10
       rc@_FillValue = rc
       return(rc)
   end if

   if (any(ismissing(x)) .or. any(ismissing(y))) then
       MSG = True
      ;print("nmsg_x="+num(ismissing(x))+" nmsg_y="+num(ismissing(x)) )

       ii  = ind(.not.ismissing(x) .and. .not.ismissing(y))
       nii = dimsizes(ii)
       if (nii.gt.2) then
           xii = x(ii)
           yii = y(ii)
           RC  = reg_multlin_stats(yii,xii,False)
       else
           RC  = new(1,typeof(y),getVarFillValue(y))
           RC@info = "Not enough non-missing values: nii="+nii
           print("regline_stats: Not enough non-missing values: nii="+nii)
           return(RC)
       end if
   else
       MSG = False                            ; mo msg data
       RC  = reg_multlin_stats(y,x,False)
   end if

   rc = RC(1)           ; transfers all attributes

   rc@long_name = "simple linear regression"
   rc@model     = "Yest = b(0) + b(1)*X" 

   rc@nptxy = rc@N      ; attributes for backward compatibility
   rc@xave  = rc@Xavg   ; with the original 'regline'
   rc@yave  = rc@Yavg
   rc@rstd  = rc@RSE
   rc@yintercept = (/ RC(0) /)               ; (/ ... /)  transfer values only
   rc@b     = (/ RC /)

;  -------- added 6.4.0 ----------> alpha=0.05; (1-alpha)=95%
   Sxy          = (rc@SST - rc@SSE)/rc       ; = sum((x-rc@Xavg)*(y-rc@Yavg))
                                             ; = sum(x*y)-((sum(x)*sum(y))/rc@N)
   Sxx          =  Sxy/rc                    ; = sum((x-rc@Xavg)^2)
                                             ; = sum(x^2)-((sum(x)^2)/rc@N)
   t025         = cdft_t(0.025, rc@MSE_dof)  ; -
   t975         = cdft_t(0.975, rc@MSE_dof)  ; +
   rc@tnull     = rc/sqrt(rc@MSE/Sxx)        ; test statistic for null hypothesis 

  ;print("Sxy="+Sxy+"  Sxx="+Sxx+"  t025="+t025+"  t975="+t975+"  tnull="+rc@tnull)  ; debug

                                             ; 95% slope & intercept confidence limits
   qslp         = sqrt(rc@MSE/Sxx)
   rc@b95       = (/ rc+t025*qslp, rc+t975*qslp /) ; slope 

   qint         = sqrt( rc@MSE*(1.0/rc@N + (rc@Xavg)^2/Sxx) )
   rc@y95       = (/ rc@yintercept +t025*qint  , rc@yintercept +t975*qint   /) ; y-int

                                             ; 95% mean response confidence intervals
   if (MSG) then
       zz       = sqrt(rc@MSE*(1.0/rc@N + (xii-rc@Xavg)^2/Sxx))          ; z[*] 
   else
       zz       = sqrt(rc@MSE*(1.0/rc@N + (  x-rc@Xavg)^2/Sxx))          ; z[*] 
   end if
   rc@YMR025    = rc@Yest + t025*zz           
   rc@YMR975    = rc@Yest + t975*zz           
                                             ; 95% prediction interval
   if (MSG) then
       zz       = sqrt(rc@MSE*(1.0 + 1.0/rc@N + (xii-rc@Xavg)^2/Sxx))    ; z[*]
   else
       zz       = sqrt(rc@MSE*(1.0 + 1.0/rc@N + (  x-rc@Xavg)^2/Sxx))    ; z[*]
   end if
   rc@YPI025    = rc@Yest + t025*zz           
   rc@YPI975    = rc@Yest + t975*zz           
;  -------------------------------

   return(rc)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print_clock.ncl
;; Carl Schreck (carl@cicsnc.org)
;; July 2011
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Print timestamp along with a string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
undef(   "print_clock" )
procedure print_clock( \\
  i_message \\ ; message to accompany timestamp
)
local None
begin
  
  maxCols = 44

  wallClock = systemfunc( "date +'  |  %F %T %Z'" )

  if( isstring( i_message ) ) then
    chMessage = stringtochar(i_message)

    if( dimsizes(chMessage).gt.maxCols ) then
      firstLine  = chartostring(chMessage(:maxCols-1))
      restOfMsg  = chartostring(chMessage(maxCols-1:))
      print( firstLine + wallClock )
      print( (/ restOfMsg /) )
    else
      blank     = stringtochar(" ")
      chPadding = new( maxCols +1 - dimsizes(chMessage), character )
      chPadding = blank(0)
      stPadding = chartostring(chPadding)
      firstLine = str_concat( (/ i_message, stPadding /) )
      print( firstLine + wallClock )
    end if

  else
    ;; Message is not a string
    print( i_message + wallClock )
  end if

end

;==========================
undef("sign_matlab")
function sign_matlab( X:numeric )
;
; Mimic Matlab 'sign' function: sign_matlab(X) returns the sign of each element of 'X'
; There are 3 possible return values for non _FillValue elements:  -1, 0, 1
; The return value type will be the same type as 'X'
;
local typex, zero, onePos, oneNeg, result
begin
  typex  = typeof(X)

  zero   = totype( 0, typex)
  onePos = totype( 1, typex)
  oneNeg = totype(-1, typex)

  result = X                       ; same size, shape and type as 'X'

  result = where(X.eq.zero,  zero  , result)
  result = where(X.gt.zero,  onePos, result)
  result = where(X.lt.zero,  oneNeg, result)

  return(result)
end
;==========================
undef("sign_f90")
function sign_f90( X:numeric, Y:numeric )
;
; X and Y must be the same shape and size. 
; They may be different numeric types (eg:, byte, short, int, float, double)
;
; Mimic the f90 'sign' function: sign(X,Y) returns the
;       absolute value of X times the sign of Y.
;       This is a transfer of sign from Y to X.
; Note: Fortran 90 does not allow a processor to distinguish between a 
;       positive and a negative zero, whereas Fortran 95 does.
;
; Note: NCL can not test for -0 .... this will test positive.
;
local typey, zero, result, dimX, dimY, rankX, rankY
begin
  dimX  = dimsizes(X)
  dimY  = dimsizes(Y)
  rankX = dimsizes(dimX)
  rankY = dimsizes(dimY)

  ier   = 0
  if (rankX.ne.rankY) then
      print("sign_f90: rank mismatch: rankX="+rankX+"; rankY="+rankY)
      ier = 1
  end if
  if (.not.all(dimX.eq.dimY)) then
      print("sign_f90: dimension size mismatch")
      print("          dimX="+dimX)
      print("          dimY="+dimY)
      ier = ier+10
  end if 
  if (ier.ne.0) then
      exit
  end if

  typeY  = typeof(Y)
  zero   = totype( 0, typeY) ; make 0 same numeric type as Y

  result = abs(X)            ; same size, shape and type as 'X'; all positive
  result = where(Y.lt.zero, -result, result)
  return(result)
end
; *****************************************************************
; Generate latitudes for a Fixed (Rectilinear) grid
; lat = latGridF (latStrt[1]:numeric, latLast[1]:numeric, nlat, "lat", "latitude", "degrees_north")
;
undef ("latGridF")
function latGridF(latStrt[1]:numeric, latLast[1]:numeric, nlat[1]:integer \
                 ,dimName[1]:string,longName[1]:string,units[1]:string)
local lat
begin
  lat           = fspan(latStrt, latLast, nlat)
  lat!0         = dimName
  lat@long_name = longName
  lat@units     = units
  lat&$dimName$ = lat
  return (lat)
end
; *****************************************************************
; Generate longitudes for a Fixed (Rectilinear) grid
; lon = lonGridF (lonStrt[1]:numeric, lonLast[1]:numeric, nlon, "lon", "longitude", "degrees_east")
;
undef ("lonGridF")
function lonGridF(lonStrt[1]:numeric, lonLast[1]:numeric, nlon[1]:integer \
                 ,dimName[1]:string,longName[1]:string,units[1]:string)
local lon
begin
  lon           = fspan(lonStrt, lonLast, nlon)
  lon!0         = dimName
  lon@long_name = longName
  lon@units     = units
  lon&$dimName$ = lon
  return (lon)
end
; *****************************************************************
; Generate index values that can be used for sampling
; NCL indices span 0 to (N-1)
;
undef("generate_sample_indices")
function generate_sample_indices(N[1], method[1]:integer)
;
; simple function to generate 0-based indices
;           N = sample size
;      method = 0   ==> sample *without* replacement; reshuffle the order 
;                       mean/std/... unchanged
;                       use when the order of sampling may be important
;
;      method = 1   ==> sample *with* replacement
;                       mean/std/... will change
local k, N1
begin
    if (method.lt.0 .or. method.gt.1) then
        print("generate_sample_indices: method="+method+": only 0 or 1 allowed")
        exit
    end if


    if (method.eq.0) then                  
        k  = generate_unique_indices( N )  
    else
        N1 = todouble(N-1)       ; largest valid index is (N-1)
       ;k  = toint(random_uniform(-0.499999999d0, N1+0.499999999d0, N))
        k  = toint(random_uniform(-0.500000000d0, N1+0.500000000d0, N))
    end if

    return(k)
end
;==========================
undef("getTimeFromBnds")
function getTimeFromBnds(f, opt[1]:logical)   ; f is of type 'list' or 'file'
local ftype, TIME, DATE, YEAR, nt
begin
  ftype = typeof(f)

  if (.not.(ftype.eq."file" .or. ftype.eq."list")) then 
      print("getTimeFromBnds: argument f must be type file or list: ftype="+ftype)
      exit
  end if

  if (ftype.eq."file" .and. isfilevar(f,"time_bnds")) then
      TIME          = f->time_bnds(:, 0)     ; address 'off-by-one' time    
      TIME@units    = f->time@units          ; appropriate for CESM
      TIME@calendar = f->time@calendar
  else if (isfilevar(f[0],"time_bnds")) then ; must be type 'list'
           TIME          = f[:]->time_bnds(:, 0)  ; address 'off-by-one' time    
           TIME@units    = f->time@units     ; appropriate for CESM
           TIME@calendar = f->time@calendar
       end if
  end if

  DATE   = cd_calendar(TIME, -2)             ; YYYYMMDD
  DATE@long_name = "current date (YYYYMMDD)"
  DATE!0 = "time"
 
  if (opt) then
      if (isatt(opt,"yrStrtEnd")) then
          YEAR = DATE
          YEAR = DATE/10000
          YEAR@long_name = "YYYY"
          nt   = ind(YEAR.ge.yrStrtEnd(0) .and. YEAR.le.yrStrtEnd(1))
          nt@long_name = "indices for Start/End period: "+yrStrtEnd(0)+"-"+yrStrtEnd(1)
          return( [/TIME(nt), DATE(nt), nt/] )
      end if
  else
      return([/TIME, DATE/])
  end if
    
end
;==========================
undef("pot_temp")
function pot_temp (p:numeric, t:numeric, npr[*]:integer, opt[1]:logical)
;
; Compute potential temperature
; Nomenclature
;     p       - pressure levels (default: Pa) 
;               if a 'units' attribute is present this may be altered
;     t       - temperature (K)   
;               Also: equivalent temperature (teqv) could be input
;                     teqv =  temperature of an air parcel from which all the water vapor 
;                             has been extracted by an adiabatic process.
;                     teqv = t + (Lv/cpd)*r  ; 
;                     cpd  = 1004. or 1005.7 ; specific heat dry air [J/kg/K]
;                     Lv   = 2.5104e6        ; [J/kg]=[m2/s2]  Latent Heat of Vaporization of Water
;                     r    = mixing ratio 
;     npr     - dimension number(s) of 'p' corresponding to temperature 
;               npr=-1 means that p and t have the same ranks and sizes
;                      In this case this argument is ignored.
;     opt     - options: Currently, not use, Set to False
;
local dimp, dimt, rankp, rankt, theta, p0
begin
  dimp   = dimsizes(p) 
  dimt   = dimsizes(t) 
  rankp  = dimsizes(dimp) 
  rankt  = dimsizes(dimt) 

  p0     = 100000.           ; default [units = Pa]
  if (isatt(p,"units") .and. (p@units.eq."mb"   .or. \
                              p@units.eq."MB"   .or. \
                              p@units.eq."millibar"    .or. \
                              p@units.eq."millibars"   .or. \
                              p@units.eq."hPa"  .or. \
                              p@units.eq."HPA"  .or. \
                              p@units.eq."hPA"  .or. \
                              p@units.eq."hpa"  )    ) then
      p0 = 1000.
 ;else
    ;print("pot_temp: WARNING:  units of pressure not recognized: p0="+p0+" Pa used")
  end if

  if (isatt(t,"units") .and. \
     .not.(t@units.eq."K" .or. t@units.eq."degK")) then
     print("pot_temp: WARNING:  units of temperature not recognized")
  end if

  if (rankp.eq.rankt) then
      if (all(dimp.eq.dimt)) then
          theta = t*(p0/p)^0.286  
      else
          print("pot_temp: FATAL error: dimension sizes do not match")
          print("          dimp="+dimp+"   dimt="+dimt) 
          exit
      end if
  else
      theta = t*(p0/conform(t,p,npr))^0.286  
  end if

  theta@long_name = "potential temperature"
  theta@units     = "K"
  copy_VarCoords(t, theta)

  return( theta )
end
;=========================
undef("pot_temp_equiv")
function pot_temp_equiv(p:numeric, t:numeric, h:numeric, dim:integer, humVarType[1]:string) ;
;
; Calculate Equivalent Potential Temperature 
; ...  https://en.wikipedia.org/wiki/Equivalent_potential_temperature
;
;  p : pressure (must be scalar, 1D or same size as t & q)
;  t : temperature (degK)
;  h : humidity variable
;      (a) specific humidity (kg/kg) : humVarType = "q"
;      (b) mixing ratio (kg/kg/)     : humVarType = "r"
;      (c) dew point (K)             : humVarType = "td"
;      (d) relative humidity (%)     : humVarType = "rh"
;  
;  p0: reference pressure: must be same units as p (Pa or hPa)
;
; contribution: Carl Schreck (NCSU)
;
local dimp, dimt, dimq, rankp, rankt, rankq, p0 \
    , Lv, cpd, R, kap, r, t_e, pExpand, humVarType_lc
begin 
  dimp   = dimsizes(p) 
  dimt   = dimsizes(t) 
  dimh   = dimsizes(h) 
  rankp  = dimsizes(dimp) 
  rankt  = dimsizes(dimt) 
  rankh  = dimsizes(dimh) 

  if (.not.(rankh.eq.rankt)) then
      print("pot_temp_equiv: FATAL error: h,t ranks do not match")
      exit
  end if

  if (.not.all(dimt.eq.dimh)) then
      print("pot_temp_equiv: FATAL error: h,t dimension sizes do not match")
      exit
  end if

  p0 = 100000.0                    ; Pa
  if (all(p.lt.1100)) then
      p0 = 1000.0                  ; hPa
  end if

  if (rankp.eq.rankt) then
      if (all(dimp.eq.dimt)) then
          pExpand = p
      else
          print("pot_temp_equiv: FATAL error: p,t rank ok but dimension sizes do not match")
          print("                dimp="+dimp+"   dimt="+dimt) 
          exit
      end if
  else                             
      pExpand = conform(t,p,dim)   ; conform p to t (expand, replicate, broadcast)
  end if


  Lv   = 2.51e6  ; latent heat of vaporization at the triple point [J/kg]; Bluestein, p203        
  cpd  = 1004.64 ; specific heat at constant pressure for air 
  R    = 287.04  ; specific gas constant for air [J/(Kg-K)]
  kap  = R/cpd    

  humVarType_lc = str_lower(humVarType)    ; ensure lower case for testing

  if( humVarType_lc.eq."q" ) then
    r = h/(1-h)                            ; convert spc humidity to mixing ratio
  else if( humVarType_lc.eq."r" )
    r = h                                  ; humidity is already mixing ratio
  else if( humVarType_lc.eq."td" )
    if (p0.eq.1000.0) then
      r = mixhum_ptd( pExpand*100., h, 1 ) ; convert dewpoint to mixing ratio
    else
      r = mixhum_ptd( pExpand, h, 1 )      ; convert dewpoint to mixing ratio
    end if
  else if( humVarType_lc.eq."rh" )
    if (p0.eq.1000.0) then
        r = mixhum_ptrh( pExpand, t, h, 1 )      ; convert relative humidity to mixing ratio
    else
        r = mixhum_ptrh( pExpand*0.01, t, h, 1 ) ; convert relative humidity to mixing ratio
    end if 
  else
    print("pot_temp_equiv: FATAL error: Invalid humidity variable" )
    print("                humVarType="+humVarType)
    exit
  end if
  end if
  end if
  end if

  t_e  = t + ( Lv / cpd ) * r    ;                 Lv     cpd         q
                                 ; Units check: ([J/kg]/[J/(kg-K)])[kg/kg]  => K
  t_e  = t_e * ( p0 / pExpand )^kap  ; p0 and pm must be same units
 
  t_e@long_name = "equivalent potential temperature"
  t_e@units     = "K"
  t_e@tag       = "NCL: pot_temp_equiv"
  copy_VarCoords( t, t_e ) ; assign coordinates
  return(t_e)
end   
;=========================
undef("static_stability") 
function static_stability (p:numeric, t:numeric, npr[1]:integer, sopt[1]:integer)
;
; Compute Static Stability
; Bluestein (1992): Synoptic-Dynamic Meteorology in Midlatitudes
;                   pg 197, eqn=4.3.8
;                   s = -T*d[log(theta)]/dp = -(T/theta)*d(theta)/dp
;
; Nomenclature
;     p       - pressure levels (Pa is default)  
;
;     t       - temperature (K)    
;               Also: equivalent temperature (teqv) could be input
;                     teqv = temperature of an air parcel from which all the water vapor 
;                            has been extracted by an adiabatic process.
;                     teqv = t + (Lv/cpd)*r  ; 
;                     cpd  = 1004. or 1005.7 ; specific heat dry air [J/kg/K]
;                     Lv   = 2.5104e6        ; [J/kg]=[m2/s2]  Latent Heat of Vaporization of Water
;                     r    = mixing ratio    ; (kg/kg) 
;
;     npr     - dimension number for pressure
;
;     sopt    - =0, Return static stability only
;             - =1, Return static stability, theta, dthdp as type list
;
; Note: In a statically stable atmosphere: d(theta)/dp < 0, hence s > 0       
;
local dimp, dimt, theta, rankt, rankp, dthdp, s
begin

  dimp   = dimsizes(p) 
  dimt   = dimsizes(t) 

  rankp  = dimsizes(dimp)
  rankt  = dimsizes(dimt)  

  theta  = pot_temp(p, t, npr, False)   ; rankp=rankt then npr is ignored

; dthdp = -T*d[log(theta)]/dp = -(T/theta)*d(theta)/dp

  if (rankp.eq.rankt) then
      dthdp  = center_finite_diff_n (theta,p,False,0, npr)
  else
      if (rankp.eq.1) then
          dthdp  = center_finite_diff_n (theta,conform(t,p,npr), False,0, npr)
      else
          print("static_stability: rank problem:  rankp="+rankp+"; rankt="+rankt)
          exit
      end if
  end if

  s = -(t/theta)*dthdp
  copy_VarCoords(t,s)

  s@long_name = "static stability"
  if (isatt(p,"units") .and. (p@units.eq."mb"   .or. \
                              p@units.eq."MB"   .or. \
                              p@units.eq."millibar"    .or. \
                              p@units.eq."millibars"   .or. \
                              p@units.eq."hPa"  .or. \
                              p@units.eq."HPA"  .or. \
                              p@units.eq."hPA"  .or. \
                              p@units.eq."hpa"  )    ) then
      s@units     = "K/hPa"   
  else
      s@units     = "K/Pa"       ; or "K-m-s2/kg"
  end if

  if (sopt.eq.0) then
      return(s)
  else
      dthdp@long_name = "vertical derivative of theta with pressure"
      dthdp@units     = s@units
      copy_VarCoords(t,dthdp)
      return( [/ s, theta, dthdp /] )
  end if
end
;=========================
undef("pot_vort_hybrid")
function pot_vort_hybrid\
            (p:numeric, u:numeric, v:numeric, t:numeric
            ,lat[*]:numeric, gridType[1]:integer, opt[1]:integer)
;
; Compute Isentropic Potential Vorticity on hybrid levels
; CCM Processor User's Guide: May 1994: page B-19
; Original source P Rasch and B Boville
;
; Nomenclature
;     p       - pressure levels (Pa)  [3D or 4D]
;     u       - zonal wind  (m/s)     [3D or 4D]          
;     v       - meridional wind (m/s) [3D or 4D]          
;     t       - temperature (K)       [3D or 4D]          
;     gridType- grid type
;               =0 means gaussian grid
;               =1 means regular or fixed grid
;     lat     - latitudes
;     opt     - options: not used. Set to 0
;
; Note: u,v,t,p MUST:
;    [1] be SOUTH-TO-NORTH 
;    [2] be GLOBAL because spherical harmonics are used
;    [3] have named dimensions because data are reordered
;   
; Usage:
;   f    = addfile ("foo.nc", "r")
;   U    = f->U    ; (time,lev,lat,lon) or (lev,lat,lon)
;   V    = f->V
;   T    = f->T
;   lat  = f->lat
;   hyam = f->hyam
;   hybm = f->hybm
;   p0   = f->P0  ; p0=100000
;   ps   = f->PS  ; Pa
;   p    = pres_hybrid_ccm (ps,p0,hyam,hybm)  ; Pa [kg/(m s2)]
;   copy_VarCoords(U,p)
;
;               ; Potential Vorticity
;   PV = pot_vort_hybrid(p,u,v,t,lat, 0,  0)
;
; Note: A nice basic discussion of PV may be found at:
; Mid-Latitude Atmospheric Dynamics: A First Course
; Jonathan E. Martin, Wiley 2006,   QC880.M36   , pp276-onward
; 
local ranku, rankv, rankt, rankp, npr, nlt, dthdp  \
    , dudp, dvdp ,theta, W, f, vr, dthdx, dthdy, G, W, rad, pv
begin
                                   ; ERROR CHECK
  ranku  = dimsizes(dimsizes(u)) 
  if (.not.(ranku.eq.3 .or. ranku.eq.4)) then
      print("pot_vort_hybrid: only 3D and 4D arrays allowed: rank="+ranku)
      exit
  end if
  rankv  = dimsizes(dimsizes(v)) 
  rankt  = dimsizes(dimsizes(t)) 
  rankp  = dimsizes(dimsizes(p)) 
  if (.not.(ranku.eq.rankv .and. ranku.eq.rankt .and. ranku.eq.rankp)) then
      print("pot_vort_hybrid: u, v, t, p must be the same rank: ranku=" \
            +ranku+"  rankv="+rankv+"  rankt="+rankt+"  rankp="+rankp)
      exit
  end if

  if (.not.(gridType.eq.0 .or. gridType.eq.1)) then
      print("pot_vort_hybrid: unrecognized gridType: only 0 and 1 allowed")
      print("               gridType="+gridType)
      ier = 1
  end if

  if ((lat(1)-lat(0)).le.0) then
      print("pot_vort_hybrid: data must be in S-N order")
      exit
  end if

  if (ranku.eq.3) then       ; (lev,lat,lon)  => (0,1,2) => (npr,nlt,2)
      npr = 0
      nlt = 1                 
  end if
  if (ranku.eq.4) then       ; (time,lev,lat,lon)  => (0,1,2,3) > (0,npr,nlt,2)
      npr = 1
      nlt = 2                  
  end if

  S           = static_stability(p,t,npr,1)   ; variable of type list

  s           = S[0]    ; [...] is list syntax    
  theta       = S[1]    ; theta = pot_tmp(p, t, npr, 0)
  dthdp       = S[2]    ; dthdp = center_finite_diff_n (theta,p,False,0,npr)
  s@_FillValue= 1e20
  delete(S)

 ;printVarSummary( s )  ; s not used for this formulation
 ;printMinMax( s, True )
 ;printVarSummary( theta )
 ;printMinMax( theta, True )
 ;printVarSummary( dthdp )
 ;printMinMax( dth2p, True )
                              ; compute VERTICAL (pressure) derivatives
  dudp  = center_finite_diff_n (    u,p,False,0,npr)
  dvdp  = center_finite_diff_n (    v,p,False,0,npr)
                              ; compute 
                              ; (1) lat/lon temperature gradients [K/m]
                              ; (2) relative vorticity            [1/s]
  dthdx   = theta             ; create E-W gradient array
  dthdy   = theta             ;        S-N        
  dthdx@long_name = "longitudinal gradient (derivative)"
  dthdy@long_name = "latitudinal gradient (derivative)"
  dthdx@units     = "K/m"
  dthdy@units     = "K/m"

  if (gridType.eq.0) then
      gradsg (theta, dthdx, dthdy) 
      vr = uv2vrG(u,v)
  end if
  if (gridType.eq.1) then
      gradsf (theta, dthdx, dthdy) 
      vr = uv2vrF(u,v)
  end if

  G   = 9.80665                    ; m/s2 ; gravity at 45 deg lat used by the WMO
  W   = 7.292e-5                   ; (1/s)     ; earth ang rotation
  rad = 4.*atan(1.)/180.
  if (typeof(lat).eq.typeof(vr)) then
      f = 2.*W*sin(lat*rad)          ; (1/s)     ; coriolis parameter
  else 
      f = 2.*W*sin(tofloat(lat)*rad)
  end if

  vr  = vr + conform(vr,f,nlt)     ; absolute vorticity   
                                   ; G used for commonly used units
  pv    = -G*(vr*dthdp - (dthdx*dvdp-dthdy*dudp) )  
  pv@long_name  = "potential vorticity"
  pv@short_name = "PV"
  pv@units      = "K m2/kg/s"      ; common units
  copy_VarCoords(t,pv)

  if (opt.eq.0) then
      return( pv )
  else
      return( [/pv, s, theta, dthdp, dudp, dvdp, dtdx, dtdy/] )
  end if
end

;----------------------------------------------------------------------------
undef("pot_vort_isobaric")
function pot_vort_isobaric\
            (p[*]:numeric, u:numeric, v:numeric, t:numeric
            ,lat[*]:numeric, gridType[1]:integer, opt:integer)
;
; Compute Potential Vorticity on constant pressure levels
; Bluestein: Synoptic-Dynamic Meteorology in Midlatitudes
;            Bug reported by Andy Show (Dec 2015)
;            pg 264  Eq 4.5.93 with [ (R/(s*p) ] replaced by [ 1/s ]) 
;            R/(s*p) = (R/p)(p/(R*T*d(theta)/dp) = 1/(T*d(theta)/dp ]= 1/s 
;
; Nomenclature
;     p       - pressure levels (Pa)  [1D]
;     u       - zonal wind  (m/s)     [3D or 4D]          
;     v       - meridional wind (m/s) [3D or 4D]          
;     t       - temperature (K)       [3D or 4D]          
;     lat     - latitudes
;     gridType- grid type
;               =0 means gaussian grid
;               =1 means regular or fixed grid
;     opt     - options: not used
;
; Note: u,v,t MUST be SOUTH-TO-NORTH 
;       and must be GLOBAL because spherical harmonics are used
;   
; Usage:
;   f   = addfile ("foo.nc", "r")
;   U   = f->U    ; (time,lev,lat,lon) or (lev,lat,lon)
;   V   = f->V
;   T   = f->T    ; K
;   lat = f->lat
;   lev = f->lev  
;   lev = lev*100
;   lev@units = "Pa"
;               ; Potential Vorticity
;   PV = pot_vort_isobaric(lev,u,v,t,lat, 0, 0)
;
; Note: A nice basic discussion of PV may be found at:
; Mid-Latitude Atmospheric Dynamics: A First Course
; Jonathan E. Martin, Wiley 2006,   QC880.M36   , pp276-onward
;
local ranku, rankv, rankt, npr, nlt, dthdp, dudp, dvdp \
     ,theta, S, s, con, R, W, f, vr, dtdx, dtdy, rad, pv
begin
                                   ; ERROR CHECK
  ranku  = dimsizes(dimsizes(u)) 
  if (.not.(ranku.eq.3 .or. ranku.eq.4)) then
      print("StaticStabilityP: only 3D and 4D arrays allowed: rank="+ranku)
      exit
  end if
  rankv  = dimsizes(dimsizes(v)) 
  rankt  = dimsizes(dimsizes(t)) 
  if (.not.(ranku.eq.rankv .and. ranku.eq.rankt)) then
      print("pot_vort_isobaric: u, v, t must be the same rank: ranku=" \
            +ranku+"  rankv="+rankv+"  rankt="+rankt)
      exit
  end if

  if (.not.(gridType.eq.0 .or. gridType.eq.1)) then
      print("pot_vort_isobaric: unrecognized gridType: only 0 and 1 allowed")
      print("           gridType="+gridType)
      ier = 1
  end if

  if ((lat(1)-lat(0)).le.0) then
      print("pot_vort_isobaric: data must be in S-N order")
      exit
  end if

  if (ranku.eq.3) then       ; (lev,lat,lon)  => (0,1,2) > (0,npr,nlt)
      npr = 0
      nlt = 1
  end if
  if (ranku.eq.4) then       ; (time,lev,lat,lon)  => (0,1,2,3) > (0,npr,nlt,2)
      npr = 1
      nlt = 2
  end if

  S           = static_stability(p,t,npr,1)   ; variable of type list

  s           = S[0]    ; [...] is list syntax    
  theta       = S[1]
  dthdp       = S[2]
  s@_FillValue= 1e20
  delete(S)

 ;printVarSummary( s )
 ;printMinMax( s, True )
 ;printVarSummary( theta )
 ;printMinMax( theta, True )
 ;printVarSummary( dthdp )
 ;printMinMax( dthdp, True )
                              ; compute VERTICAL (pressure) derivatives ; shear
  dudp        = center_finite_diff_n (    u,p,False,0,npr)
  dvdp        = center_finite_diff_n (    v,p,False,0,npr)

                              ; compute 
                              ; (1) lat/lon temperature gradients [K/m]
                              ; (2) relative vorticity            [1/s]

  dtdx   = t                  ; create E-W gradient array with coordinate info
  dtdy   = t                  ;        S-N        
  dtdx@long_name = "longitudinal gradient (derivative)"
  dtdy@long_name = "latitudinal gradient (derivative)"
  dtdx@units     = "K/m"
  dtdy@units     = "K/m"

  if (gridType.eq.0) then
      gradsg (t, dtdx, dtdy) 
      vr = uv2vrG(u,v)
  end if
  if (gridType.eq.1) then
      gradsf (t, dtdx, dtdy) 
      vr = uv2vrF(u,v)
  end if

  G   = 9.80665                    ; m/s2 ; gravity at 45 deg lat used by the WMO
  R   = 287.04                     ; m2/(s2-K) ; gas constant dry air
  W   = 7.292e-5                   ; (1/s)     ; earth ang rotation
  rad = 4.*atan(1.)/180.
  if (typeof(lat).eq.typeof(vr)) then
      f = 2.*W*sin(lat*rad)          ; (1/s)     ; coriolis parameter
  else
      f = 2.*W*sin(tofloat(lat)*rad)
  end if

  vr  = vr + conform(vr,f,nlt)     ; absolute vorticity   
  s   = where(s.eq.0, s@_FillValue, s)    ; safety
  con = 1/s
                                   ; G* added to get common pv units 
  pv  = -G*(vr + con*(dvdp*dtdx-dudp*dtdy))*dthdp  
  pv@long_name  = "potential vorticity"
  pv@short_name = "PV"
  pv@units      = "K m2/kg/s"      ; common units
  copy_VarCoords(t,pv)

  if (opt.eq.0) then
      return( pv )
  else
      return( [/pv, s, theta, dthdp, dudp, dvdp, dtdx, dtdy/] )
  end if
end
;====================
undef("advect_var")
function advect_var (u:numeric, v:numeric, x:numeric,gridType[1]:integer 
                    ,long_name:string, units:string, iopt[1]:integer)
;
; compute advection of a variable: UV.GRADIENT(X) ; U*(dX/dlon) + V*(dX/dlat)
;
; Requires:
;    (1) grid be global cuz spherical harmonics are used
;    (2) required dimension order ([time,[lev,]lat,lon)
;    (3) the input grids *must* be ordered South==>North
; 
; Nomenclature:
;   u, v      - zonal and meridional wind components [m/s]
;               rightmost dimensions must be ([...,]lat,lon)
;   x         - scalar quantity to be advected 
;               rightmost dimensions must be ([...,]lat,lon)
;               eg: T, Z, divergence, vorticity, latent energy,..., whatever
;   gridType  - type of grid: 0=gaussian, 1=fixed
;   long_name - descriptive name (eg: "Temperature Advection" )
;   units     - units of result  (eg: "m-K/s" )
;   iopt      - flag: =0 means return only the advect result          
;                     =1 means return the advect, longitidinal and latitudinal gradient
;                        as part of a list
; Usage:
;   f = addfile ("foo.nc", "r")
;   u = f->U    ; (time,lev,lat,lon)
;   v = f->V
;   T = f->T
;               ; linear advection of temperature
;   Tadv = advect_var(u,v,T,0,"linear advection of temperature","m-K/s",0)
;
local dimu, dimv, dimx, ranku, rankv, rankx, ier \
    , x_grad_lon, x_grad_lat, advect
begin
                             ; ERROR CHECKING
  dimu  = dimsizes(u)
  dimv  = dimsizes(v)
  dimx  = dimsizes(x)
  ranku = dimsizes(dimu)
  rankv = dimsizes(dimv)
  rankx = dimsizes(dimx)

  ier   = 0
  if (.not.(gridType.eq.0 .or. gridType.eq.1)) then
      print("advect_var: unrecognized gridType: only 0 and 1 allowed")
      print("            gridType="+gridType)
      ier = 1
  end if
  if (.not.(ranku.eq.rankv .and. ranku.eq.rankx)) then
      print("advect_var: all input arguments must have the same rank")
      print("            ranku="+ranku)
      print("            rankv="+rankv)
      print("            rankx="+rankx)
      ier = ier + 10
  end if
  if (.not.(all(dimu.eq.dimv) .and. all(dimu.eq.dimx))) then
      print("advect_var: all input arguments must have the same dimension sizes")
      print("            ranku="+ranku)
      print("            rankv="+rankv)
      print("            rankx="+rankx)
      ier = ier + 100
  end if
  if (rankx.lt.2) then
      print("advect_var: variable rank must be at least 2D: ([time,[lev,]lat,plon)")
      print("advect_var: rank="+rankx+"D")
      ier = ier + 1000
  end if

  dnam  = getvardims(x)             ; dimension names
  ynam  = dnam(rankx-2)             ; latitude dimension name
  if (.not.ismissing(ynam) .and. iscoord(x,ynam)) then
      yord  = isMonotonic(x&$ynam$)
      if (yord.le.0) then
          print("advect_var: grid is not in South-to-North order.")
          ier = ier + 10000
      end if
  else
      print("advect_var: Warning: No named dimensions. Can not check grid order.")
  end if

  if (ier.ne.0) then
      print("advect_var fatal error(s) encountered: ier="+ier)
      exit
  end if

  x_grad_lon = new( dimx, typeof(x), getFillValue(x) )
  x_grad_lat = new( dimx, typeof(x), getFillValue(x) )

  if (gridType.eq.0) then
      gradsg  (x, x_grad_lon, x_grad_lat)
  end if
  if (gridType.eq.1) then
      gradsf  (x, x_grad_lon, x_grad_lat)
  end if
                                        ; advection
  
  uxgrad_lon = u*x_grad_lon 
  vxgrad_lat = v*x_grad_lat
  advect     = uxgrad_lon + vxgrad_lat
                                        ; add meta data
  copy_VarCoords(x, advect)
  advect@long_name = long_name
  advect@units     = units

  if (iopt.eq.0) then
      return(advect)
  else
      copy_VarCoords(x, x_grad_lon)
      x_grad_lon@long_name = "longitudinal gradient"

      copy_VarCoords(x, x_grad_lat)
      x_grad_lat@long_name = "latitudinal gradient"

      return([/ advect, x_grad_lon, x_grad_lat /] )

     ;copy_VarCoords(x, ux_grad_lon)
     ;x_grad_lon@long_name = "zonal advection"

     ;copy_VarCoords(x, vx_grad_lat)
     ;x_grad_lat@long_name = "meridional advevtion"

     ;return([/ advect, x_grad_lon, x_grad_lat, uxgrad_lon, vxgrad_lat  /] )
  end if

end
;====================
undef("advect_variable")
function advect_variable (u:numeric, v:numeric, x:numeric,gridType[1]:integer 
                          ,long_name:string, units:string, iopt[1]:integer)
; advect_variable was the original function
begin
  return( advect_var(u, v, x, gridType, long_name, units, iopt) )
end
;---
undef("log1px")
function log1px(x:numeric)
; Common function in other languages 
local y, Fill
begin
  if (isatt(x,"_FillValue")) then
      Fill = x@_FillValue
  else
      if (typeof(x).eq."double") then
          Fill = 1d20
      else
          Fill = 1e20
      end if
  end if

  y = where((1+x).gt.0, log(1+x), Fill)   

  if (any(y.eq.Fill)) then
      y@_FillValue = Fill
  end if
  return (y)
end
;================
undef("pres_hybrid_ccm_se")
function pres_hybrid_ccm_se(ps:numeric,p0[1]:numeric,hya[*]:numeric,hyb[*]:numeric)
local dimNames, dim_ps, ntim, ncol, ps_dumy, pres_dumy, pres
begin

  dimNames = getvardims(ps) ; (time,ncol)
  if (dimNames(1).ne."ncol") then
      print("pres_hybrid_ccm_se: You are a MORON! This is not an SE grid!")
      exit
  end if

  dim_ps = dimsizes(ps)
  ntim = dim_ps(0)
  ncol = dim_ps(1)

  ps_dumy = conform_dims( (/ntim,1,ncol/), ps, (/0,2/))

  pres_dumy = pres_hybrid_ccm (ps_dumy,p0,hya,hyb) ; (ntim,klev,1,ncol)
  pres = pres_dumy(:,:,0,:) ; (ntim,klev,ncol)

; the following is not needed for builtin function

  pres@long_name = "vertical pressure"
  pres@units = "Pa" ; [kg/(m s2)]

  copy_VarCoords(ps, pres(:,0,:))
  pres!1 = "lev"

  return(pres)
end 
;================
undef("dpres_hybrid_ccm_se")
function dpres_hybrid_ccm_se(ps:numeric,p0[1]:numeric,hya[*]:numeric,hyb[*]:numeric)
local dimNames, dim_ps, ntim, ncol, ps_dumy, dpres_dumy, dpres
begin

  dimNames = getvardims(ps) ; (time,ncol)
  if (dimNames(1).ne."ncol") then
      print("dpres_hybrid_ccm_se: You are a MORON! This is not an SE grid!")
      exit
  end if

  dim_ps = dimsizes(ps)
  ntim = dim_ps(0)
  ncol = dim_ps(1)

  ps_dumy = conform_dims( (/ntim,1,ncol/), ps, (/0,2/))

  dpres_dumy = dpres_hybrid_ccm (ps_dumy,p0,hya,hyb) ; (ntim,klev,1,ncol)
  dpres = dpres_dumy(:,:,0,:) ; (ntim,klev,ncol)

; the following is not needed for builtin function

  dpres@long_name = "layer thickness"
  dpres@units = "Pa" ; [kg/(m s2)]

  copy_VarCoords(ps, dpres(:,0,:))
  dpres!1 = "lev"

  return(dpres)
end 
;===================
undef ("eofunc_north")
function eofunc_north(eval[*]:numeric, N[1]:integer, prinfo[1]:logical)
;
; North, G.R. et al (1982): Sampling Errors in the Estimation of Empirical Orthogonal Functions.
; Mon. Wea. Rev., 110, 699–706.
; doi: http://dx.doi.org/10.1175/1520-0493(1982)110<0699:SEITEO>2.0.CO;2
;
; Usage after 'eofunc'. Here ntim was used,  
;             prinfo = True
;             sig    = eval_north(eof@eval, ntim, prinfo)
;
local neval, dlam, low, high, sig, n
begin
  neval   = dimsizes(eval)
  if (neval.eq.1)
      print("eofunc_north: neval=1, no testing can be performed")
      sig = True 
      sig@long_name = "EOF separation is not testable N=1"
      sig@N         =  N
      return(sig)
  end if

  dlam    = eval * sqrt(2.0/N)   ; eq 24
  low     = eval-dlam
  high    = eval+dlam

  sig     = new(dimsizes(eval), logical)
  sig = False  ; default is not significantly separated

; first and last eigenvalues are special cales

  if (eval(0).gt.high(1)) then
      sig(0) = True
  end if
  if (eval(neval-1).lt.low(neval-2)) then
      sig(neval-1) = True
  end if

; loop over other eignvalues

  if (N.gt.2) then
      do n=1,neval-2
         if (eval(n).lt.low(n-1) .and. eval(n).gt.high(n+1)) then
             sig(n) = True
         end if
      end do
  end if

  if (prinfo) then
      print(dlam+"   "+low+"   "+eval+"   "+high+"  "+sig)
  end if

  sig@long_name = "EOF separation"
  sig@N         =  N
  return(sig)
end
;===================
undef ("eof_north") ; documentation snafu
function eof_north(eval[*]:numeric, N[1]:integer, prinfo[1]:logical)
begin
  sig = eofunc_north(eval, N, prinfo)
  return(sig)
end
;-----
undef("parse_globalatts_hdf5")
function parse_globalatts_hdf5( f, attName[1]:string )

; Do this *ONCE* for *EACH* global attribute

local header, cr, delim, strs, nstrs, n, info, hinfo
begin
 header= f@$attName$                      ; long string[1]

 cr    = str_get_nl()                     ; carriage return (new line)
 delim = cr + " ;"
 strs  = str_split(header, delim)
 nstrs = dimsizes(strs)
 
 hinfo = new( (/nstrs,2/), "string")
 do n=0,nstrs-1
    info := str_split(strs(n), "=")
    hinfo(n,0) = info(0)
    if (dimsizes(info) .eq. 2 ) then	
	hinfo(n,1) = info(1) 
    else
	hinfo(n,1) = ""
    end if
 end do 
 hinfo@hdf5_global_attribute_name = attName

 return(hinfo)
end
;---
undef("extract_globalatts_hdf5")
function extract_globalatts_hdf5( hinfo[*][2]:string, name_to_extract[1]:string)
; extract the value associated with the attribute which created 'hinfo'
; info is output from function getfilevaratts_hdf5
begin
  
  if (.not.isatt(hinfo,"hdf5_global_attribute_name")) then
      print("Warning:  hdf5_global_attribute_name is missing")
  end if
    
  idx = ind(hinfo(:,0) .eq. name_to_extract)
  if (ismissing(idx(0))) then
      return("extract_globalatts_hdf5: "+name_to_extract+" is missing")
  end if
  
  return(hinfo(idx(0),1))  ; to the right of = sign
end
;--------------
undef("coriolis_param")
function coriolis_param(lat:numeric)
; Coriolis Parameter 
local rad, omega
begin
  if (typeof(lat).eq."double") then
      omega = 7.292d-5                   ; (1/s)     ; earth ang rotation
      rad   = get_d2r("double")          ; rad=4d0*atan(1d0)/180d0
  else
      omega = 7.292e-5         
      rad   = get_d2r("float")           ; rad=4.0*atan(1.0)/180.0
  end if

  fcor  = 2*omega*sin(lat*rad)    
  fcor@long_name = "Coriolis parameter"
  fcor@units     = "1/s"
  copy_VarCoords(lat, fcor)

  return(fcor)
end
;--------------
undef("wgt_vertical_n")
function wgt_vertical_n (X:numeric, dp:numeric, iopt:integer, lev_dim[1]:integer)
; 
; Perform weighted vertical average (integral) or sum
; 
; Requirement: X must be in the following order: ([time,],lev,lat,lon)
;
; Nomenclature:
; X         - array to be integrated. No missing data allowed
; dp        - pressure thickness computed by "dpres_hybrid_ccm" or "dpres_plevel"
;             Must be the same size/shape as X
;             No missing data allowed.
; iopt      - =0 weighted vertical average
;             =1 weighted vertical sum
;             =2 weighted vertical sum, vertical avg
; lev_dim   - level dimension
;             lev_dim = 0                            ; (lev), (0); (time,lev),  (0,1) 
;                                                    ; (lev,lat,lon),  (0,1,2) 
;             lev_dim = 1                            ; (time,lev,lat,lon), (0,1,2,3)
;             lev_dim = 2                            ; (case,time,lev,lat,lon), (01,2,3,4)
;+++++++++++++++++++++++
; Usage for hybrid levels
;        f = addfile("....", "r")
;        hyai = f->hyai
;        hybi = f->hybi
;        p0   = f->P0                               ; p0=1000 or p0=100000
;        ps   = f->PS
;    
;        dp   = dpres_hybrid_ccm (ps,p0,hyai,hybi)  ; Pa [kg/(m s2)]
;--------------------
; Usage for pressure levels
;        f   = addfile("....", "r")
;        lev = f->lev  ; (/  1,  2,  3,  5,   7, 10, 20, 30, \   ; hPa 
;                           50, 70,100,150, 200,250,300,400, \
;                          500,600,700,775, 850,925,1000 /)
;                      ; uniys of lev and psfc must match
;        psfc= f->PS   ; PA         (time,lat,lon)
;        lev = lev*100 ; make PA to match psfc       
;        lev@units = "PA"
;
;        ptop= 0       ; integrate 0==>psfc at each grid point
;
;                      ; dp(klev,nlat,mlon) or dp(ntim,klev,nlat,mlon)
;        dp  = dpres_plevel(lev, psfc, ptop, 0)     ; Pa [kg/(m s2)]
;--------------------
; Use the 'dp' from above                           ; (  0 , 1 , 2 , 3 )
;        t    = f->T                                ; (time,lev,lat,lon)
;        xvi  = wgt_vert_n(t, dp,  0, 1)

local dimX, dimDP, rankX, Xdp, vsum, wsum, vavg
begin

  dimX  = dimsizes( X )
  dimDP = dimsizes( dp )
  rankX = dimsizes( dimX )

  if (.not.all(dimDP.eq.dimX) ) then  ; error check
      print("wgt_vertical_n: dimension sizes are not equal")
      print("wgt_vertical_n:  dimX="+dimX)
      print("wgt_vertical_n: dimDP="+dimsizes(dp) )
      exit
  end if

  if (isatt(X,"_FillValue") .and. any(ismissing(X)) ) then  ; error check
      print("wgt_vertical_n: No _FillValue allowed")
      print("wgt_vertical_n: X: nFill="+num(ismissing(X)))
      exit
  end if

;;if (isatt(dp,"_FillValue") .and. any(ismissing(dp)) ) then  ; error check
;;    print("wgt_vertical_n: No _FillValue allowed")
;;    print("wgt_vertical_n: dp: nFill="+num(ismissing(dp)))
;;    exit
;;end if

  Xdp  = X*dp                                ; [? kg/(m s2)]   (temporary variable)
  copy_VarCoords(X, Xdp)

  vsum = dim_sum_n_Wrap( Xdp, lev_dim )      ; sum vertically [ie integrate]
  if (iopt.eq.1) then
      return(vsum)
  end if

  wsum = dim_sum_n_Wrap( dp , lev_dim )      ;  "      " 
  if (any(wsum.eq.0)) then
      if (.not.isatt(wsum,"_FillValue")) then
          wsum@_FillValue = default_fillvalue(typeof(wsum))
      end if
      wsum = where(wsum.eq.0, wsum@_FillValue, wsum)   ; avoid division by 0
  end if
      
  vavg = vsum/wsum                           ; one less dimension (no vertical dim)
  copy_VarMeta(vsum, vavg) 
  vavg@NCL_op = "Weighted Vertical Average"

  vsum@NCL_op = "Weighted Vertical Sum"
  wsum@NCL_op = "Summed Weights"

  if (iopt.eq.0) then
      return(vavg)
  end if

  return([/ vavg, vsum, wsum /] )    ; iopt=2
end
;--------------------------------------------------------
undef("dim_maxind")
function dim_maxind(x, nDim[1]:integer)
;
; This is a modification of the original algorithmn posted to ncl-talk by
; Dave Allured (NOAA Affilate): April, 2016
;
local dimx, rankx, N, xmax, nmsgx, dim_xmax, t, tr, mask1
begin
  dimx  = dimsizes(x)
  rankx = dimsizes(dimx)

  if (nDim.ne.0) then
      print("dim_maxind: *** Currently nDim must be set to zero; nDim="+nDim)
      return( new(dimx(1:), integer, -99) )
  end if

  if (rankx.gt.4) then
      print("dim_maxind: *** Currently 1D, 2D, 3D, 4D are supported; rank="+rankx)
      print("                This corresponds to the leftmost dimension.")
      return( new(dimx(1:), integer, -99) )
  end if

  if (rankx.eq.1) then
      ind_xmax           = maxind(x)
      ind_xmax@long_name = "index of 1st  max value"
      ind_xmax@tag       = "dim_maxind"
      return(ind_xmax)
  end if

  N     = dimx(nDim)    ; size of the dimension corresponding to 'nDim'    

  xmax     = dim_max_n_Wrap (x, nDim)
  dim_xmax = dimsizes(xmax)
  ind_xmax = new (dim_xmax, integer, -99 )


  if (rankx.eq.2) then
      do t = 1, N
         tr = N-t
         mask1    = (x(tr,:) .ge. xmax)
         mask1    = where (ismissing(mask1), False, mask1)
         ind_xmax = where (mask1, tr, ind_xmax)
      end do
  end if

  if (rankx.eq.3) then
      do t = 1, N
         tr = N-t
         mask1    = (x(tr,:,:) .ge. xmax)
         mask1    = where (ismissing(mask1), False, mask1)
         ind_xmax = where (mask1, tr, ind_xmax)
      end do
  end if

  if (rankx.eq.4) then
      do t = 1, N
         tr = N-t
         mask1    = (x(tr,:,:,:) .ge. xmax)
         mask1    = where (ismissing(mask1), False, mask1)
         ind_xmax = where (mask1, tr, ind_xmax)
      end do
  end if

  copy_VarCoords(xmax, ind_xmax)
  ind_xmax@long_name = "index of 1st  max value"
  ind_xmax@tag       = "dim_maxind"

  return(ind_xmax)
end
;--------------------------------------------------
undef("dim_minind")
function dim_minind(x, nDim[1]:integer)
;
; It is a modification of the original algorithmn posted to ncl-talk  
; Dave Allured (NOAA Affilate): April, 2016
;
local dimx, rankx, N, xmin, dim_xmin, t, tr, mask1
begin
  dimx  = dimsizes(x)
  rankx = dimsizes(dimx)

  if (nDim.ne.0) then
      print("dim_minind: *** Currently nDim must be set to zero; nDim="+nDim)
      return( new(dimx(1:), integer, -99) )
  end if

  if (rankx.gt.4) then
      print("dim_minind: *** Currently 1D, 2D, 3D, 4D are supported; rank="+rankx)
      print("                This corresponds to the leftmost dimension.")
      return( new(dimx(1:), integer, -99) )
  end if

  if (rankx.eq.1) then
      ind_xmin           = minind(x)
      ind_xmin@long_name = "index of 1st min value"
      ind_xmin@tag       = "dim_minind"
      return(ind_xmin)
  end if

  N     = dimx(nDim)    ; size of the dimension corresponding to 'nDim'    

  xmin     = dim_min_n_Wrap (x, nDim)
  dim_xmin = dimsizes(xmin)
  ind_xmin = new (dim_xmin, integer, -99)

  if (rankx.eq.2) then
      do t = 1, N
         tr = N-t
         mask1    = (x(tr,:) .le. xmin)
         mask1    = where (ismissing(mask1), False, mask1)
         ind_xmin = where (mask1, tr, ind_xmin)
      end do
  end if

  if (rankx.eq.3) then
      do t = 1, N
         tr = N-t
         mask1    = (x(tr,:,:) .le. xmin)
         mask1    = where (ismissing(mask1), False, mask1)
         ind_xmin = where (mask1, tr, ind_xmin)
      end do
  end if

  if (rankx.eq.4) then
      do t = 1, N
         tr = N-t
         mask1    = (x(tr,:,:,:) .le. xmin)
         mask1    = where (ismissing(mask1), False, mask1)
         ind_xmin = where (mask1, tr, ind_xmin)
      end do
  end if

  copy_VarCoords(xmin, ind_xmin)
  ind_xmin@long_name = "index of 1st  min value"
  ind_xmin@tag       = "dim_minind"

  return(ind_xmin)
end
;--------------------------------------------------
undef("cohsq_c2p")
function cohsq_c2p(coher2:numeric, df:numeric)
;
; INPUT:
; coher2 - coherence squared (0-1)
; df     - degrees of freedom
;
; Dennis Hartmann
; http://www.atmos.washington.edu/~dennis/552_Notes_6c.pdf
;     See Table 6.2 , page 187 and the associated caption on page 186
;
; Paul Julian's paper
; http://journals.ametsoc.org/doi/abs/10.1175/1520-0469%281975%29032%3C0836%3ACOTDOS%3E2.0.CO%3B2
;
begin
                                                    ; df from FFT
;;prob = 1.0-(1.0-coher2)^((df/2.0)-1.0)            ; Paul Julian 
  prob = 1.0-(1.0-coher2)^( df     -1.0)            ; match Hartmann           

  prob@long_name = "coherence squared probability"
  prob@NCL_tag   = "cohsq_c2p: p=1-(1-coher_sq)^(df-1)"
  copy_VarCoords(coher2, prob)
  return(prob)
end
;--------------------------------------------------
undef("cohsq_p2c")
function cohsq_p2c(prob:numeric, df:numeric)
; 
; INPUT:
; prob  - probability (0-1)
; df    - degrees of freedom
;
; Dennis Hartmann
; http://www.atmos.washington.edu/~dennis/552_Notes_6c.pdf
;     See Table 6.2 , page 187 and the associated caption on page 186
;
; Paul Julian's paper
; http://journals.ametsoc.org/doi/abs/10.1175/1520-0469%281975%29032%3C0836%3ACOTDOS%3E2.0.CO%3B2
;
begin
                                                    ; df from FFT
;;c2  =  1.-(1.-prob)^(1./((df/2.)-1.))             ; Paul Julian 
  c2  =  1.-(1.-prob)^(1./( df    -1.))             ; match Hartmann

  c2@long_name = "coherence-squared confidence level"
  c2@NCL_tag   = "cohsq_p2c: coher_sq=1-(1-prob)^(1/(df-1))"
  copy_VarCoords(prob, c2)
  return(c2)
end
;--------------------------------------------------
;------------------------------
undef("brunt_vaisala_atm")
function brunt_vaisala_atm(th:numeric, z:numeric, opt[1]:integer, ndimz[1]:integer)
;
; Brunt-Vaisala: 
;   The frequency at which a displaced air parcel will oscillate when displaced 
;   vertically within a statically stable environment.
;
; AMS Glossary:  
; BruntV: http://glossary.ametsoc.org/wiki/Brunt-v%C3%A4is%C3%A4l%C3%A4_frequency
;
; Nomenclature:
;   th - potential temperature (degK); could also be virtual potential temperature
;   z  - geometric height (m)
;   opt- =0 return Brunt-Vaisala frequency only
;   opt- =1 return Brunt-Vaisala frequency and dthdz as part of a list variable
;
local dimth, dthdz , pmflag, g, buoy, brunt
begin
;;dimz    = dimsizes(z)
  dimth   = dimsizes(th)
;;rankz   = dimsizes(dimz)   
;;rankth  = dimsizes(dimth)

  dthdz   = center_finite_diff_n(th, z, False, 0, ndimz)

 ;copy_VarCoords(th, dthdz)
 ;dthdz@long_name = "Vertical Gradient of potential temperature"
 ;dthdz@units     = "degK/m"
 ;print("brunt_vaisala_atm: dthdz: min="+min(dthdz)+"   max="+max(dthdz))

  g      = 9.80665                ; m/s2 at 45N
  buoy   = (g/th)*dthdz           ; 'buoyancy' term (N^2): [m/s2][1/K][K/m]=[1/s2]     
                                  ; buoyancy can be negative if dthdz < 0
 ;buoy@long_name = "buoyancy"
 ;buoy@units     = "1/s^2"
 ;copy_VarCoords(th, buoy)
 ;print("buoyancy: min="+min(buoy)+"   max="+max(buoy))

  pmflag = new(dimth, "integer", -9999 )
  pmflag = where(buoy.lt.0, -1, 1)

  brunt  = sqrt(abs(buoy))     
  brunt  = brunt*pmflag           ; add negative flag
  delete(pmflag)

  copy_VarCoords(th, brunt)
  brunt@long_name = "Brunt-Vaisala frequency"
  brunt@units     = "1/s"
  brunt@info      = "http://glossary.ametsoc.org/wiki/Brunt-v%C3%A4is%C3%A4l%C3%A4_frequency"
  brunt@sign      = "brunt<0 means the buoyancy was <0"
  brunt@sign_1    = "User could use where(brunt.lt.0, 0, brunt) to create only positive values "

  if (opt.eq.0) then
      return( brunt )
  else
      return( [/ brunt, dthdz /] )
  end if
end
;-------------------------------
undef("eady_growth_rate")
function eady_growth_rate(th:numeric, u:numeric, z:numeric, lat:numeric,  opt[1]:integer, ndimz[1]:integer)
;
; Maximum Eady Growth Rate  
;
; Reference: 
;   R. S. Lindzen and Brian Farrell, 1980: 
;   A Simple Approximate Result for the Maximum Growth Rate of Baroclinic Instabilities. 
;   J. Atmos. Sci., 37, 1648–1654. 
;    http://dx.doi.org/10.1175/1520-0469(1980)037<1648:ASARFT>2.0.CO;2
;
;   Simmonds, I., and E.-P. Lim (2009): 
;   Biases in the calculation of Southern Hemisphere mean baroclinic eddy growth rate 
;   Geophys. Res. Lett., 36, L01707 
;   doi:10.1029/2008GL036320
;
;   Vallis, G.K. (2006) 
;   Atmospheric and Oceanic Dynamics: Fundamentals and Large-Scale Circulation
;   Cambridge Univ. Press, New York.
;    
; Nomenclature
; th   - potential temperature (K)
;        http://glossary.ametsoc.org/wiki/potential_temperature
;        https://www.ncl.ucar.edu/Document/Functions/Contributed/pot_temp.shtml
; u    - zonal wind components (m/s) ; same dimensionality as 'th'
; z    - height (m)
; lat  - latitude of each grid point ; same dimensionality as 'th'  
; opt  - =0 (egr only); 
; ndimz- dimension of 'th' for which vertical gradient is to be calculated
;        th(:), wspd(:), z(:) ..... ndimz=0
;        th(:,:,:)  , z(:) or z(:,:,:)   and the left is the vertical dim, ndimz=0
;        th(:,:,:,:), z(:) or z(:,:,:,:) and the left is the time dimension 
;                                        and the next is height,  ndimz=1 
;
;
local dimth, dimu, dimz, dimth, dimlat, rankth, ranku, rankz, rankth, ranklat \
    , u_shear, brunt, fcor, g, omega, con, rad, egr
begin
                                ; dimension checking
  dimu    = dimsizes(u)
  dimz    = dimsizes(z)
  dimth   = dimsizes(th)
  dimlat  = dimsizes(lat)

  ranku   = dimsizes(dimu)   
  rankz   = dimsizes(dimz)   
  rankth  = dimsizes(dimth)
  ranklat = dimsizes(dimlat)   

  if (.not.(rankth.eq.ranku .and. rankth.eq.rankz .and. rankth.eq.ranklat) \
      .and. all(dimth.eq.dimu) .and. all(dimth.eq.dimz) .and. all(dimth.eq.dimlat)) then
      print("eady_growth_rate: th, u, lat and must have the same rank & sizes")
      print("ranku="+ranku+"  rankth="+rankth+"  rankz="+rankz+"  ranklat="+ranklat)
      print(dimth)
      print(dimu) 
      print(dimz) 
      print(dimlat) 
      exit
  end if

  brunt   = brunt_vaisala_atm(th, z, 0, ndimz)

  dudz    = center_finite_diff_n(u , z, False, 0, ndimz) 

  copy_VarCoords(u, dudz)
  dudz@long_name  = "vertical gradient of the zonal wind (zonal wind shear): du/dz"
  dudz@units      = "1/s"
 ;print("eady_growth_rate: dudz: min="+min(dudz)+"   max="+max(dudz))

  g     = 9.80665                    ; m/s2 ; gravity at 45 deg lat used by the WMO
  con   = 0.3098
  fcor  = coriolis_param(lat)        ; (1/s)     ; coriolis parameter
                                     ; prevent 1/0
  if (any(brunt.eq.0)) then
      if (.not.isatt(brunt,"_FillValue")) then
          if (typeof(brunt).eq."double") then
              brunt@_FillValue = 1d20
          else
              brunt@_FillValue = 1e20
          end if
       end if
       brunt = where(brunt.eq.0, brunt@_FillValue, brunt)
  end if

  egr   = con*abs(fcor)*abs(dudz)/brunt
  egr@long_name = "maximum eady growth rate"
  egr@units     = ""
  copy_VarCoords(u, egr)

  if (opt.eq.0) then
      return (egr)
  else if (opt.eq.1) then
      return ( [/egr, dudz /] )
  else if (opt.eq.2) then
      return ( [/egr, dudz, brunt /] )
  end if
  end if
  end if

end
;-------------------------------
undef("rigrad_bruntv_atm")
function rigrad_bruntv_atm(thv:numeric, u:numeric, v:numeric, z:numeric, opt[1]:integer,  ndim[1]:integer)
;
; Gradient Richardson Number: RI  = buoyancy/shear_flow

;      Criterion for assessing the stability of stratified shear flow
;
;      A dimensionless ratio, Ri, related to the buoyant production or 
;      consumption of turbulence divided by the shear production of turbulence.
;      It is used to indicate dynamic stability and the formation of turbulence.
;      The critical Richardson number, Ric, is about 0.25 (although reported values have 
;      ranged from roughly 0.2 to 1.0), and flow is dynamically unstable and turbulent 
;      when Ri < Ric. Such turbulence happens either when the wind shear is great enough 
;      to overpower any stabilizing buoyant forces (numerator is positive), or when there 
;      is static instability (numerator is negative). 
;
;      If the Richardson number is much less than unity, buoyancy is unimportant in the flow. 
;      If it is much greater than unity, buoyancy is dominant (in the sense that there is 
;      insufficient kinetic energy to homogenize the fluids).
;
;      Large values of Ri indicate very stable conditions while low values *may*
;      indicate dynamic stability (subcritical region).
;
;      Note: Above indicates 0.25 as critical number but others indicate 1.0
;
;
; Brunt-Vaisala: 
;   The frequency at which a displaced air parcel will oscillate when displaced 
;   vertically within a statically stable environment.
;
; AMS Glossary:  
; BruntV: http://glossary.ametsoc.org/wiki/Brunt-v%C3%A4is%C3%A4l%C3%A4_frequency
; Ri_Num: http://glossary.ametsoc.org/wiki/Gradient_richardson_number
;         The latter uses Tv and not THETAv in denominator. ?Error?  Think so.
;         http://en.wikipedia.org/wiki/Richardson_number
;         Actually, in the boundary layer (near 1000hpa) Tv ~ thv
;         SEE:  http://radiometrics.com/data/uploads/2012/11/chan_iop08b.pdf
;
; Nomenclature
; thv  - *virtual* potential temperature (K): virtual_pot_temp = pot_temp*(1+0.61w)
;        http://glossary.ametsoc.org/wiki/Virtual_potential_temperature
;        NOte: 'regular' potential temperature is likely 'good enough'
;
; u,v  - wind components (m/s) ; same dimensionality as 'th' and 't'
; z    - height (m)
; opt  - option
;        opt=0   return gradient Richardson number only
;        opt=1   return Brunt-Vaisala frequency only    
;        opt=2   return gradient Richardson number & Brunt-Vaisala frequency    
;        opt=3   return gradient Richardson number , Brunt-Vaisala frequency    
;                       buoyancy, wind shear2
; ndim - dimension of 'th' for which vertical gradient is to be calculated
;        th(:), wspd(:), z(:) ..... ndim=0
;        th(:,:,:)  , z(:) or z(:,:,:)   and the left is the vertical dim, ndim=0
;        th(:,:,:,:), z(:) or z(:,:,:,:) and the left is the time dimension 
;                                        and the next is height,  ndim=1 
local dimthv, dimu, dimv, dimz, rankthv, ranku, rankv, rankz \
    , g, dthvdz, pmflag, buoy, dudz, dvdz, shear2, Ri
begin
                                ; dimension checking
  dimu    = dimsizes(u)
  dimv    = dimsizes(v)
  dimz    = dimsizes(z)
  dimthv  = dimsizes(thv)

  rankthv = dimsizes(dimthv)
  ranku   = dimsizes(dimu)   
  rankv   = dimsizes(dimv)   
  rankz   = dimsizes(dimz)   
                                
  if (.not.(rankthv.eq.ranku .and. all(dimthv.eq.dimz) .and. all(dimthv.eq.dimu))) then
      print("rigrad_bruntv_atm: thv, u, v must have the same rank & dimensionality")
      print(dimthv)
      print(dimu) 
      print(dimz) 
      exit
  end if

  dthvdz  = center_finite_diff_n(thv, z, False, 0, ndim) 

  copy_VarCoords(thv, dthvdz)
  dthvdz@long_name = "Vertical Gradient of potential temperature"
  dthvdz@units     = "degK/m"
 ;print("rigrad_bruntv_atm: dthvdz: min="+min(dthvdz)+"   max="+max(dthvdz))

  g      = 9.80665                ; m/s2
  buoy   = (g/thv)*dthvdz         ; 'buoyancy' term: [m/s2][1/K][K/m]=[1/s2]     

  copy_VarCoords(thv, buoy)
  buoy@long_name = "buoyancy"
  buoy@units     = "1/s^2"
 ;print("buoyancy: min="+min(buoy)+"   max="+max(buoy))

  pmflag = new(dimthv, "integer", -9999 )
  pmflag = where(buoy.lt.0, -1, 1)

  brunt  = sqrt(abs(buoy))     
  brunt  = brunt*pmflag
  delete(pmflag)

  copy_VarCoords(thv, brunt)
  brunt@long_name = "Brunt-Vaisala (buoyancy) frequency: atm"
  brunt@units     = "1/s"
  brunt@info      = "http://glossary.ametsoc.org/wiki/Brunt-v%C3%A4is%C3%A4l%C3%A4_frequency"
  brunt@sign      = "brunt<0 means the buoyancy was <0"
  brunt@sign_1    = "User could use where(brunt.lt.0, 0, brunt) to create only positive values "

  delete(dthvdz)
  
  dudz   = center_finite_diff_n(u , z, False, 0, ndim) 
  copy_VarCoords(u, dudz)
  dudz@long_name  = "vertical gradient of the zonal wind: du/dz"
  dudz@units      = "1/s"
 ;print("rigrad_bruntv_atm: dudz: min="+min(dudz)+"   max="+max(dudz))

  dvdz   = center_finite_diff_n(v , z, False, 0, ndim) 
  copy_VarCoords(v, dvdz)
  dvdz@long_name  = "vertical gradient of the meridional wind: dv/dz"
  dvdz@units      = "1/s"
 ;print("rigrad_bruntv_atm: dvdz: min="+min(dvdz)+"   max="+max(dvdz))

  shear2 = (dudz^2 + dvdz^2) 
  copy_VarCoords(u, shear2)
  shear2@long_name  = "wind shear squared"
  shear2@units      = "1/s^2"
 ;print("rigrad_bruntv_atm: shear2: min="+min(shear)+"   max="+max(shear))
  delete([/dudz,dvdz/])

  Ri     = buoy/shear2               ; [1/s2] / [1/s2] => dimensionless
  copy_VarCoords(u, Ri)
  Ri@long_name   = "Gradient Richardson Number"
  Ri@info = "http://glossary.ametsoc.org/wiki/Gradient_richardson_number"

  if (opt.eq.0) then
      return(Ri)
  end if

  if (opt.eq.1) then
      return([/Ri, brunt/])
  end if
  
  return([/ Ri, brunt, buoy, shear2 /])
end
;=============
undef("dz_pop")
; compute layer thicknesses for POP: z_t ot z_w  (ALL layers)
function dz_pop(z[*])
local kz, dz
begin
  kz           = dimsizes(z)
  dz           = new( kz, typeof(z), "No_FillValue")
  dz(0)        = 0.5*(z(0)+z(1))                       ; top layer has sfc
  dz(1:kz-2)   = 0.5*(z(2:kz-1)-z(0:kz-3))
  dz(  kz-1)   = z(kz-1) - 0.5*(z(kz-1)+z(kz-2))       ; bottom layer
  copy_VarCoords(z, dz)
  dz@long_name = "depth layer thickness"
  dz@units     = z@units
  dz@sum_dz    = sum(dz)   ; should match the bottom layer [ eg: z_t(kz-1) ]

  return(dz)
end
;========================
undef("fire_index_haines")
function fire_index_haines(p[*]:numeric,t[*]:numeric, td[*]:numeric, opt[1]:logical)
;
; nomenclature:
;   p  - pressure levels (hPa)    ; must be a coordinate variable
;   t  - temperature (C)          ; must have a pressure coordinate variable
;   td - dew point temperature (C); must have a pressure coordinate variable
;  opt - not used; set to False
;  
; http://www.nwas.org/digest/papers/1988/Vol13-Issue2-May1988/Pg23-Haines.pdf
; The Haines_Index is technically the "Low Atmosphere Stability Index" ==> LASI
;
; It consists of two parts: 
;    (i) Stability Term: (Tpl - Tp2) 
;   (ii) Moisture Term : (Tp1 - TDp1)

; The Haines Index can range between 2 and 6. 
; The drier and more unstable the lower atmosphere the higher the index.
; ROS means 'Risk Of Spread'
; A Haines Index of 2-3= a very low ROS, 4= low ROS, 5=moderate ROS, and 6= high ROS.
; 
; The function calculates a "low" (0), "mid" (1) and "high" (2) elevation index
; -----------------------------------------
; The following classifications are based upon: 
;    http://www.erh.noaa.gov/cae/haines.htm
;
; The HI boundaries are those used for the above web site.
; The low-level Haines boundary values for the Coastal and Midlands zones, and 
; the mid-level Haines bounadries for the Upstate zones.

local HI, st, mt, klvl, stLow, mtLow, stMid, mtMid, stHigh, mtHigh
begin

  if (.not.(isdimnamed(p,0))) then
      print("fire_index_haines: p argument must  have a named dimension")
      exit
  else
      pName = getvardims(p)
      if (.not.(iscoord(t,pName) .and. iscoord(td,pName))) then 
          print("fire_index_haines: t, td arguments must ciirdinate variables")
          exit
      end if
  end if

  klvl = 3                                ; 3 level HI

; calculate individual stability and moisture terms
; There is no need for the 'st' and 'mt' arrays.
; The computations could be done without them. 
; However, I think they make things a bit clearer. Further, it makes it
;          possible to return them as separate variables via a 'list' variable 

  st      = new ( klvl, "float", "No_FillValue")
  st(0)   = t({950}) - t({850})     ; low elevaltion
  st(1)   = t({850}) - t({700})     ; mid
  st(2)   = t({700}) - t({500})     ; high
  st!0    = "elevation"
  st@long_name = "HI: Stability Term"
  st@units     = "degC"
 ;printVarSummary(st)

  mt      = new ( klvl, "float", "No_FillValue")
  mt(0)   = t({850}) - td({850})    ; low elevaltion
  mt(1)   = t({850}) - td({850})    ; mid
  mt(2)   = t({700}) - td({700})    ; high
  mt!0    = "elevation"
  mt@long_name = "HI: Moisture Term"
  mt@units     = "degC"
 ;printVarSummary(mt)

; Classification (HI -> Haines Index)

  HI      = new ( klvl, "float", "No_FillValue")
  HI      = 0                   ; initialize (not necessary here)
                                ; just doing it to be explicit
; Low (index 0)

  stLow   = (/ 3.5, 7.5 /)      
  mtLow   = (/ 5.5, 9.5 /)

  if (st(0).le.stLow(0)) then   ; stability
      HI(0) = 1 
  else if (st(0).gt.stLow(0) .and. st(0).le.stLow(1)) then
      HI(0) = 2 
  else if (st(0).gt.stLow(1)) then
      HI(0) = 3 
     end if
   end if
  end if

  if (mt(0).le.mtLow(0)) then   ; moisture (combine with stability)
      HI(0) = HI(0)+ 1 
  else if (mt(0).gt.mtLow(0) .and. mt(0).le.mtLow(1)) then
      HI(0) = HI(0)+ 2  
  else if (mt(0).gt.mtLow(1)) then
      HI(0) = HI(0)+ 3
     end if
   end if
  end if

; Mid (index 1)

  stMid   = (/ 5.5, 9.5 /)      
  mtMid   = (/ 5.5,11.5 /)

  if (st(1).le.stMid(0)) then    ; stability
      HI(1) = 1 
  else if (st(1).gt.stMid(0) .and. st(1).le.stMid(1)) then
      HI(1) = 2 
  else if (st(1).gt.stMid(1)) then
      HI(1) = 3 
     end if
   end if
  end if

  if (mt(1).le.mtMid(0)) then   ; moisture (combine with stability)
      HI(1) = HI(1)+ 1 
  else if (mt(1).gt.mtMid(0) .and. mt(1).le.mtMid(1)) then
      HI(1) = HI(1)+ 2  
  else if (mt(1).gt.mtMid(1)) then
      HI(1) = HI(1)+ 3 
     end if
   end if
  end if

; High (index 2)

  stHigh  = (/ 17.0, 20.5 /)      
  mtHigh  = (/ 14.0, 20.0 /)

  if (st(2).le.stHigh(0)) then    ; stability
      HI(2) = 1 
  else if (st(2).gt.stHigh(0) .and. st(2).le.stHigh(1)) then
      HI(2) = 2 
  else if (st(2).gt.stHigh(1)) then
      HI(2) = 3 
     end if
   end if
  end if

  if (mt(2).le.mtHigh(0)) then   ; moisture (combine with stability)
      HI(2) = HI(2)+ 1 
  else if (mt(2).gt.mtHigh(0) .and. mt(2).le.mtHigh(1)) then
      HI(2) = HI(2)+ 2 
  else if (mt(2).gt.mtHigh(1)) then
      HI(2) = HI(2)+ 3 
     end if
   end if
  end if

  HI@long_name = "Haines Index"
  HI@reference = "http://www.nwas.org/digest/papers/1988/Vol13-Issue2-May1988/Pg23-Haines.pdf" 
  HI!0         = "elevation"
  HI@info      = "Low Atmosphere Stability Index (LASI)" 
  HI@elevation = (/"Low", "Medium", "High"/)
  HI@details   = "http://www.erh.noaa.gov/cae/haines.htm"

  return (HI)
;;return( (/HI, st, mt/) )  ; return 3 variables
end
;=================================================
undef("time_reassign")
function time_reassign(f, timeName[1]:string)
;
; utility to center the time using the 'time_bounds' argument
;
; double time(time) ;
;        time:units = "days since 2011-01-01 00:00:00" ;
;        time:calendar = "noleap" ;
;        time:bounds = "time_bounds"   ; attribute which points to bounds variable
;
;     f1   = addfile ("foo.nc", "r")   ; f1 is type file
;     time = time_reassign(f, "time")
; or
;     fils = systemfunc("ls foo*nc")
;     f2   = addfiles(fils, "r")       ; f2 is type list
; create a 'correct' (center of mass) time
;     time = time_reassign(f2, "time")
; 
; ----------------------------------------------------
; Oddity: some files have 'time' as type float & 'time_bnd' as double
;         I speculate the reverse is also true.                       
; ----------------------------------------------------
local time, tbnd
begin
  if (.not.(typeof(f).eq."file" .or. typeof(f).eq."list")) then
      print("time_reassign: FATAL: input argument f must be of type list or file: type is "+typeof(f)) 
      exit
  end if

  if (typeof(f).eq."file") then
      time = f->$timeName$
      if (isatt(time,"bounds") .and. isfilevar(f,time@bounds) ) then
          tbnd = f->$time@bounds$      ; (ntim,2)
      else
          print("time_reassign: WARNING: No time bounds attribute available; return original time")
          return(time)
      end if
  else                              ; must be type list
      time = f[:]->$timeName$       ; (ntim)
      if (isatt(time,"bounds") .and. isfilevar(f[0],time@bounds) ) then
          tbnd = f[:]->$time@bounds$    ; (ntim,2)
      else
          print("time_reassign: WARNING: No time bounds attribute  available; return original time") 
          return(time)
      end if
  end if
           ; the following was needed cuz some files had 'time' as float & 'time_bnds' as double
  time     = (/ totype( (tbnd(:,0)+tbnd(:,1))*0.5 , typeof(time)) /)  ; center of 'time' bnds
  time&$timeName$ = time
  time@NCL = "function time_reassign used to reassign time to mid-value of bounds"
  return(time)
end
;----
undef("time_reassign_cv2var")
function time_reassign_cv2var(f, timeName[1]:string, varName[1]:string)
;
;   f1   = addfile ("foo.nc","r")
;   temp = time_reassign_cv2var(f1, "time", "T")
; or
;   f2   = addfiles(  fils   ,"r")
;   temp = time_reassign_cv2var(f2, "time", "T")
begin
  if (.not.(typeof(f).eq."file" .or. typeof(f).eq."list")) then
      print("getVar_time_reassign: FATAL: input argument f must be of type list or file: type is "+typeof(f)) 
      exit
  end if

  time = time_reassign(f, timeName)   ; new time coordinate

  if (typeof(f).eq."file") then
      if (isfilevar(f,varName)) then
          var = f->$varName$
      else
          print("time_reassign_cv2var: FATAL: variable "+varName+" on file") 
          exit
      end if
  else                                ; must be type list
      if (isfilevar(f[0],varName)) then
          var = f->$varName$
      else
          print("time_reassign_cv2var: FATAL: variable "+varName+" on file") 
          exit
      end if
  end if

  var&$timeName$ = time               ; rassign time coordinate variable (cv)
  var@NCL = "function time_reassign_cv2var used to reassign time coordinate variable to mid-value of bounds"
  return(var)
end
;----
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
undef("demod_cmplx")
function demod_cmplx(y:numeric, frqdem[1]:numeric, frqcut[1]:numeric, nwt[1]:integer,  ndim[1]:integer,  opt[1]:logical)
;
; Simple complex demodulation 
; Extract approximately periodic components (ie, slowly varying) from a time series
; The data Y(t) is taken to be a nearly-periodic signal plus everything else, Z(t).
; The amplitude (A) and phase of the periodic signal are allowed to be time-dependent 
; but assumed to vary slowly compared to the frequency 'omega'.
;
; Source: http://www.uni-muenster.de/ZIV.BennoSueselbeck/s-html/helpfiles/demod.html
;   Complex demodulation is a technique for analyzing nonstationary time series by estimating 
;   the instantaneous amplitude and phase of a given harmonic component. To better understand 
;   the results of complex demodulation several lowpass filters should be tried: the smaller 
;   the pass band, the less instantaneous in time but more specific in frequency is the result.
;
; Nice summary:
; http://faculty.washington.edu/kessler/generals/complex-demodulation.pdf
;
; This technique fails when the frequency of oscillation differs from the modulation 
; frequency by more than the cutoff frequency of the low-pass filter. 
;
local dimy, ranky, pi, zero, one, frqdem25, frqdem50, ny, arg, ARG, yr, ys \
    , delf, ihp, nsigma, fcb, wgt, amp, pha
begin
   if (typeof(y).eq."double" .or. typeof(frqdem).eq."double" \
                             .or. typeof(frqcut).eq."double" ) then
       pi       = 4d0*atan(1d0)
       zero     = 0.0d
       one      = 1.0d
       frqdem25 = 0.25d
       frqdem50 = 0.50d
   else
       pi       = 4.0*atan(1.0)
       zero     = 0.0 
       one      = 1.0 
       frqdem25 = 0.25 
       frqdem50 = 0.50 
   end if
   pi2 = 2*pi

   dimy  = dimsizes(y)
   ranky = dimsizes(dimy) 
   ny    = dimy(ndim) 
   delf  = one/ny

; error check

   if (frqcut.le.delf .or. frqcut.gt.frqdem50 .or. \
       frqdem.le.delf .or. frqdem.gt.frqdem50      ) then
       print("demod_cmplx: illegal value(s): frqcut="+frqcut+" : frqdem="+frqdem)
       exit
   end if

; demodulate (primary): Bloomfield, 1976: page 148

   arg = ispan(0,ny-1,1)*frqdem*2*pi
   ARG = conform(y, arg, ndim)     ; all dimensions
                                   ; complex demodulate 
   yr  =  y*cos(ARG)*2             ; 'real' series
   yi  = -y*sin(ARG)*2             ; 'imaginary'       

;---Create weights for Lanczos low pass filter
;---Apply filter weights to raw demodulated series (nwt/2 point 'lost; at start/end)

   if (opt .and. isatt(opt,"NoFilter") .and. opt@NoFilter) then
      print("demod_cmplx: no low pass filter applied")

   else                            ; default is low pass filter
      ihp    = 0                  
      nsigma = one                       
      if (opt .and. isatt(opt, "nsigma")) then
          nsigma := totype(opt@nsigma, typeof(pi))
      end if 
   
      fcb    = totype(-999, typeof(pi))
      wgt    = filwgts_lanczos (nwt, ihp, frqcut, fcb, nsigma)  
   
      yr     = wgt_runave_n_Wrap ( yr, wgt, 0, 0) 
      yi     = wgt_runave_n_Wrap ( yi, wgt, 0, 0) 
   end if
  
;---Calculate the amplitudes
   
   amp = sqrt(yr^2 + yi^2)
   pha = where(amp.eq.zero, zero, atan2(yi, yr)) 

   amp@long_name = "amplitude"
   pha@long_name = "phase"         
   if (isatt(y,"units")) then
       amp@units = y@units
   end if

   copy_VarCoords(y, amp)
   copy_VarCoords(y, pha)

   return([/ amp, pha /])           ; return as a list
end
;=================================================
undef("epflux4")
function epflux4(U[*][*][*][*]:numeric, V[*][*][*][*]:numeric \
                ,T[*][*][*][*]:numeric, plvl[*]:numeric, lat[*]:numeric \
                ,opt[1]:logical)

;*******************************************************
; Purpose:
; NCL Script to calculate near-realtime Eliassen-Palm flux from NCEP Reanalysis Data
; This version produces Quasi-geostrophic Eliassen-Palm Fluxes in spherical coordinates
; Plots the EP Flux vectors in a latitude-log(pressure) coordinate
; Optionally plot the divergence of EP-Flux
;*******************************************************
; History
; Original code written (2009) by J. Barsugli [NOAA/ESRL PSD]
; Adapted by C. Smith [NOAA/ESRL PSD]a.gov
; Modified by Joe Barsugli to add contours of EP-Flux divergence June 2010
; Modified by Joe Barsugli to redo scaling of arrows in the vertical June 2010
; Original code available here:  
;         http://www.esrl.noaa.gov/psd/data/epflux/epflux.2010.ncl
;********************************************************
; Modified by Dennis Shea to NCL 6.0.0 release June 2012 (hidden) 
; Made into a supported function by Dennis Shea Sept. 2015 for 6.3.1 inclusion
;********************************************************
; Reference:
; Edmon, H.J., B.J. Hoskins,and M.E. McIntyre,1980:
; Eliassen-Palm cross sections for the troposphere.
; J. Atmos. Sci., 37:2600–2616
;********************************************************
; local not required
local PLVL, P0, LAT, a, PI, phi, cphi, acphi, asphi, omega, latfac   \
    , THETA, THEATp, THETAptm, THETAza, Uza, Vza, UV, UVzm, UVzmtm   \
    , Fphi, Fp, dudt, EPdiv, EPdiv1, EPdiv2, EPdiv \
    , rhofac, strat1, stratmask, print_var_info
begin
  print_var_info = False
  if (opt .and. isatt(opt,"print_var_info")) then
      print_var_info = True
  end if

;---Dimension sizes

;;dimUVT = dimsizes(U)
;;ntim   = dimUVT(0)
;;klvl   = dimUVT(1)
;;nlat   = dimUVT(2)
;;mlon   = dimUVT(3)

;---PLVL is a local version of 'plvl' in Pa
  
  PLVL = plvl
  if (max(plvl).lt.2000) then                 ; must be hPa
      PLVL = PLVL*100                         ; make local be Pa
  end if
  logPLVL = log(PLVL)                         ; used below
  P0      = 100000                            ; "Pa", reference

;---LAT is a local version of 'lat'. 
;   Get around any pole singularity: ie., avoid dividing by cos(90)=0.0 
;   Set abs(LAT)=90 to _FillValue

  LAT = lat
  LAT@_FillValue = totype(1e20,typeof(LAT))   ; any out-of-range value will do
  LAT = where(abs(LAT).eq.90, LAT@_FillValue, LAT) 
  
;---Constants & quantities needed
 
  a     = 6.37122e06       ; radius of the earth (m)
  PI    = get_pi(U)        ; same type as 'U'
  omega = 7.2921e-5   

  phi   = LAT*PI/180.0     ; latitude in radians
  cphi  = cos(phi)
  acphi = a*cphi             
  asphi = a*sin(phi)       ; a*sin latitude for use in calculating the divergence.
  f     = 2*omega*sin(phi) ; coriolis parameter
  
  latfac= acphi*cphi       ; scale factor includes extra cos(phi) 
                           ; for graphical display of arrows 
                           ; see Edmon et al, 1980
;---Compute theta (degK)

 ;THETA   = T*(conform(T,PLVL,1)/P0)^(-0.286) ; (time,lev,lat,lon); (0,1,2,3)
  THETA   = T*(P0/conform(T,PLVL,1))^( 0.286) ; (time,lev,lat,lon)
  THETA@long_name = "potential temperature"
  THETA@units     = "degK"
  copy_VarCoords(T, THETA)

  if (print_var_info) then
      printVarSummary(THETA)
      printMinMax(THETA,0)
      print("~~~")
  end if


;---Zonal mean (zm) THEATA

  THETAzm = dim_avg_n_Wrap(THETA, 3)          ; (time,lev,lat) => (0,1,2)
  THETAzm@long_name = "zonal mean potential temperature"

  if (print_var_info) then
      printVarSummary(THETAzm)
      printMinMax(THETAzm,0)
      print("~~~")
  end if
  
;---d(THETAzm)/d(log(p))                      ; p<==>plvl

  THETAp   = center_finite_diff_n (THETAzm,logPLVL,False,0,1) ; (time,lev,lat)
  THETAp@long_name = "d(THETAzm)/d[log(p)]"
  THETAp@units     = "degK"
  copy_VarCoords(THETAzm, THETAp)

  if (print_var_info) then
      printVarSummary(THETAp)
      printMinMax(THETAp,0)
      print("~~~")
  end if
                                             
;---Compute (1/p)*THETAp = THETAp/p (overwrite variable) 

  THETAp   = THETAp/conform(THETAp,PLVL,1)    ; broadcast PLVL to all other dimensions
  THETAp@long_name = "THETAp/plvl"
  THETAp@units     = "degK/Pa"

  if (print_var_info) then
      printVarSummary(THETAp)
      printMinMax(THETAp,0)
      print("~~~")
  end if

;---Compute time average of THETAp

  THETAptm = dim_avg_n_Wrap(THETAp,0)         ; (lev,lat,lon)

  if (print_var_info) then
      printVarSummary(THETAptm)
      printMinMax(THETAptm,0)
      print("~~~")
  end if

;---Zonal mean wind component anomalies (za)
  
  Uza     = dim_rmvmean_n_Wrap(U,3)           ; (time,lev,lat,lon)
  Vza     = dim_rmvmean_n_Wrap(V,3)
  THETAza = dim_rmvmean_n_Wrap(THETA,3)
  THETAza@long_name = "THETA zonal anomaly"
  Vza@long_name     = "V meridional anomaly"                    
  Uza@long_name     = "U zonal anomaly U"                    

  if (print_var_info) then
      printVarSummary(Uza)
      printMinMax(Uza,0)
      print("~~~")
      printVarSummary(Vza)
      printMinMax(Vza,0)
      print("~~~")
      printVarSummary(THETAza)
      printMinMax(THETAza,0)
      print("~~~")
  end if

;---Anomaly products
  
  UV       = Uza*Vza                          ; (time,lev,lat,lon)
  UV@long_name = "Uza*Vza: U'V'"
  UV@units = "m2/s2"
  copy_VarCoords(U,UV)

  if (print_var_info) then
      printVarSummary(UV)
      printMinMax(UV,0)
      print("~~~")
  end if

  VTHETA  = Vza*THETAza                       ; (time,lev,lat,lon)
  VTHETA@long_name = "Vza*THETAza"
  VTHETA@units     = "degK-m/s"
  copy_VarCoords(V,VTHETA)

  if (print_var_info) then
      printVarSummary(VTHETA)
      printMinMax(VTHETA,0)
      print("~~~")
  end if

;---Zonal (zm) & time (tm) means of UV & VTHETA 

  UVzm    = dim_avg_n_Wrap(UV  ,3)            ; (time,lev,lat)
  UVzm@long_name   = "zonal mean UV"

  if (print_var_info) then
      printVarSummary(UVzm)
      printMinMax(UVzm,0)
      print("~~~")
  end if

  UVzmtm  = dim_avg_n_Wrap(UVzm,0)            ; (lev,lat) 
  UVzmtm@long_name = "time mean of zonal mean UV"

  if (print_var_info) then
      printVarSummary(UVzmtm)
      printMinMax(UVzmtm,0)
      print("~~~")
  end if
  
  VTHETAzm   = dim_avg_n_Wrap(VTHETA  ,3)     ; (time,lev,lat)
  VTHETAzm@long_name   = "zonal mean UV"

  if (print_var_info) then
      printVarSummary(VTHETAzm)
      printMinMax(VTHETAzm,0)
      print("~~~")
  end if

  VTHETAzmtm = dim_avg_n_Wrap(VTHETAzm,0)     ; (lev,lat)
  VTHETAzmtm@long_name = "time mean of zonal mean UV"

  if (print_var_info) then
      printVarSummary(VTHETAzmtm)
      printMinMax(VTHETAzmtm,0)
      print("~~~")
  end if

;---EP flux meridional and vertical compoments
  
  Fphi  = -UVzmtm*conform(UVzmtm,latfac,1)                     ; (lev,lat)
  Fp    = (VTHETAzmtm/THETAptm)*conform(VTHETAzmtm,f*acphi,1)  ; (lev,lat)
  Fphi@long_name = "UVzmtm: meridional component of EP flux"
  Fp@long_name   = "(VTHETAzmtm/THETAptm): vertical component of EP flux"
  copy_VarCoords(UVzmtm    ,Fphi)
  copy_VarCoords(VTHETAzmtm,Fp)

  if (print_var_info) then
      printVarSummary(Fphi)
      printMinMax(Fphi,0)
      print("~~~")
      printVarSummary(Fp)
      printMinMax(Fp,0)
      print("~~~")
  end if

;---EP Flux Divergence
;   Derivative w.r.t latitude  using  1/[a cos(phi)]  d/dphi [cos(phi)*X] =  d/d [asin(phi)] Fphi
;   Note: Fphi already has the extra factor of cos(phi)

  EPdiv1 = center_finite_diff_n(Fphi,asphi,False,0,1)  
  EPdiv1@long_name = "Meridional divergence"

  EPdiv2 = center_finite_diff_n(Fp,PLVL,False,0,0)    
  EPdiv2@long_name = "Vertical divergence"

  EPdiv  = EPdiv1 + EPdiv2                             ; Add components
  EPdiv@long_name   = "EP flux divergence"
  copy_VarCoords(Fp, EPdiv)

  if (print_var_info) then
      printMinMax(EPdiv1,0)
      print("~~~")
      printMinMax(EPdiv2,0)
      print("~~~")
      printVarSummary(EPdiv)
      printMinMax(EPdiv,0)
      print("~~~")
  end if
  delete( [/EPdiv1, EPdiv2 /])
  
;---Compute acceleration from div(F):   
;   Rate of change in angular momentum per unit mass and divide by a*cos(phi).
  
  dudt = 86400*EPdiv/conform(EPdiv,acphi,1) ; 
  dudt@long_name = "acceleration of zonal wind"
  dudt@units     = "m/s2"
  copy_VarCoords(EPdiv,dudt)

  if (print_var_info) then
      printVarSummary(dudt)
      printMinMax(dudt,0)
      print("~~~")
  end if

;---Optional return (opt@raw=True)

  if (opt .and. isatt(opt, "raw") .and. opt@raw) then
      return ([/ Fphi, Fp, EPdiv, dudt /] )   ; 'raw' 
  end if

;---Scale the vectors for plot 
;   First scale according to Edmon et al. for pressure coordinates
;   (even though I am using log-p display -- not entirely consistent as the arrows 
;    may "look" divergent when they are not, but better visibility in practice)

  Fp   = Fp*conform(Fp,cphi,1)   
  Fphi = Fphi/a
  
;---Next scale by the relative ranges of the two axes of the plot(3.14 radians by 10^5 Pa)

  Fp   = Fp/P0                      ; Fp/1.0e5
  Fphi = Fphi/PI

;---Option: scale by sqrt(pressure)

  if (opt .and. isatt(opt, "scale_sqrt_p") .and. .not.opt@scale_sqrt_p) then
      scale_by_sqrt_p = 0               ; not used
  else   
      scale_by_sqrt_p = 1               ; default
      rhofac   = sqrt(P0/PLVL)

      Fp   = Fp*conform(Fp,rhofac,0)
      Fphi = Fphi*conform(Fphi,rhofac,0)

      Fp@long_name   = "Vertical EP Flux: scaled by sqrt(P0/plvl)" 
      Fphi@long_name = "Meridional EP Flux: scaled by sqrt(P0/plvl)" 
  end if
  
;--Scale by a magnification factor above 100 hPa (10000 Pa). 

  if (opt .and. isatt(opt,"magf")) then
      strat1    = where(PLVL.lt.10000, opt@magf, 1)
      stratmask = conform(Fp,strat1,0)
      Fp        = Fp*stratmask
      Fphi      = Fphi*stratmask
      Fp@tag    = "magnification factor at <= 100 hPa: magf="+opt@magf
      Fphi@tag  = "magnification factor at <= 100 hPa: magf="+opt@magf
  end if

  return ([/ Fphi, Fp, EPdiv, dudt /] )  ; standard 'scaled' values
end
;---------------------------------------------------------------------------------
; generic function interface ... promote 3D to 4D ... if applicable
;---------------------------------------------------------------------------------
undef("epflux")
function epflux(U:numeric, V:numeric, T:numeric, plvl[*]:numeric, lat[*]:numeric, opt[1]:logical)
local dimUVT, rankUVT, klvl, nlat, mlon, ntim, epflx, U4, V4, T4
begin
  dimUVT  = dimsizes(U)
  rankUVT = dimsizes(dimUVT)

  if (rankUVT.lt.3 .or.rankUVT.gt.4) then
      print("epflux: U/V/T rank must be 3 or 4; rank="+rankUVT)
      print("epflux: dimsizes(U)="+dimUVT)
      exit
  end if

  if (rankUVT.eq.4) then
      epflx  = epflux4(U,V,T,plvl,lat,opt)

  else                    ; create a temporary 4D array; the 'time' timension is bogus
      klvl = dimUVT(0)
      nlat = dimUVT(1)
      mlon = dimUVT(2)
      ntim = 1            ; place holder

      U4   = new( (/ntim,klvl,nlat,mlon/), typeof(U), getFillValue(U)) 
      V4   = new( (/ntim,klvl,nlat,mlon/), typeof(V), getFillValue(V)) 
      T4   = new( (/ntim,klvl,nlat,mlon/), typeof(T), getFillValue(T)) 
      U4(0:0,:,:,:) = U 
      V4(0:0,:,:,:) = V 
      T4(0:0,:,:,:) = T 
                          ; the following is *not* necessary but ....
      U4!0 = "time_dumy"    
      V4!0 = "time_dumy"
      T4!0 = "time_dumy"
      U4&time_dumy = 0
      V4&time_dumy = 0
      T4&time_dumy = 0

      epflx = epflux4(U4,V4,T4,plvl,lat,opt)
  end if

  return(epflx)
end
;---------
undef("heat_index_nws_eqns")
function heat_index_nws_eqns(t:numeric, rh:numeric, crit[3]:numeric, c[9]:numeric \
                            ,eqnType[1]:integer, opt[1]:logical)
; 'heat_index_nws' driver; input t is **degF**
local HI, A, t2, rh2, trh
begin
                                                           ; NWS practice
  HI = (0.5*(t+61.0+((t-68.0)*1.2)+(rh*0.094)) + t)*0.5    ; avg (Steadman and t)
  HI = where(t.le.40, t, HI)                               ; http://ehp.niehs.nih.gov/1206273/

 ;A  = -10.3 + 1.1*t + 0.047*HI                            ; ehp.1206273.g003.tif
 ;if (A.ge.40 .and. A.lt.lt.crit(0)) then
 ;    HI = A 
 ;end if
 ;delete(A)

  if (all(t.lt.crit(0))) then
      eqnType = 0
  else
      HI = where(HI.ge.crit(0)  \
                ,c(0)+ c(1)*t + c(2)*rh + c(3)*t*rh  + c(4)*t^2              \
                +c(5)*rh^2 + c(6)*t^2*rh  + c(7)*t*rh^2 + c(8)*(t^2)*(rh^2)  \
                ,HI)                 
    
      HI = where(rh.lt.13 .and. (t.gt.80 .and. t.lt.112) \
                ,HI-((13-rh)/4)*sqrt((17-abs(t-95.))/17), HI)
    
      HI = where(rh.gt.85 .and. (t.gt.80 .and. t.lt.87)  \
                ,HI+((rh-85)/10)*((87-t)/5), HI)

      eqnType = 1
  end if

  return(HI)
end
; ------
undef("heat_index_nws")
function heat_index_nws(t:numeric, rh:numeric, iounit[2]:integer, opt[1]:logical)
;
; http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
; https://en.wikipedia.org/wiki/Heat_index
; Reference:
; R. G. Steadman, 1979: 
;    The Assessment of Sultriness. Part I: A Temperature-Humidity Index Based on Human Physiology and Clothing Science.
;    J. Appl. Meteor., 18, 861–873.
;    doi: http://dx.doi.org/10.1175/1520-0450(1979)018<0861:TAOSPI>2.0.CO;2 
;
; Lans P. Rothfusz (1990): NWS Technical Attachment (SR 90-23)
;
; The ‘Heat Index’ is a measure of how hot weather "feels" to the body.
; The combination of temperature an humidity produce an "apparent temperature" 
; or the temperature the body "feels". The returned values are for shady locations only. 
; Exposure to full sunshine can increase heat index values by up to 15°F. 
; Also, strong winds, particularly with very hot, dry air, can be extremely 
; hazardous as the wind adds heat to the body
;
; The computation of the heat index is a refinement of a result obtained by multiple 
; regression analysis carried out by Lans P. Rothfusz and described in a 
; 1990 National Weather Service (NWS) Technical Attachment (SR 90-23).  
;
; In practice, the Steadman formula is computed first and the result averaged 
; with the temperature. If this heat index value is 80 degrees F or higher, 
; the full regression equation along with any adjustment as described above is applied. 
;
local HI, T, Tcrit, c, eqnType, units
begin

  if (iounit(0).lt.0 .or. iounit(0).gt.2) then
      print("heat_index_nws: invalid iounit(0): invalid(0)="+iounit(0))
      exit
  end if
  if (iounit(1).lt.0 .or. iounit(1).gt.2) then
      print("heat_index_nws: invalid iounit(1): invalid(1)="+iounit(1))
      exit
  end if
  if (all(rh.lt.1)) then
      print("heat_index_nws: rh must be % not fractional; All rh are < 1")
      exit
  end if

       ; Default coef are for .ge.80F and 40-100% humidity 
  coef  = (/-42.379, 2.04901523, 10.14333127, -0.22475541   \
           ,-0.00683783, -0.05481717, 0.00122874, 0.00085282, -0.00000199 /)
  crit  = (/ 80, 40, 100/)    ; (T_low (F),  RH_low,  RH_High/)

       ; Optional coef are for 70F-115F and humidities between 0 and 80% 
       ; Within 3F of default coef
  if (opt .and. isatt(opt,"coef") .and. opt@coef.eq.2) then
      coef := (/ 0.363445176, 0.988622465, 4.777114035, -0.114037667  \
               ,-0.000850208,-0.020716198, 0.000687678,  0.000274954, 0.0 /)
      crit := (/ 70, 0, 80/)   ; F
  end if

  eqnType = -1
  if (iounit(0).eq.2) then                          ; t must be degF
      HI = heat_index_nws_eqns(t, rh, crit, coef, eqnType, opt) ; use input (t) directly
  else
      if (iounit(0).eq.0) then
           T = 1.8*t + 32                           ; degC => degF
      else
           T = 1.8*t - 459.67                       ; degK => degF
      end if
      HI = heat_index_nws_eqns(T, rh, crit, coef, eqnType, opt) ; use local T
  end if

  if (iounit(1).eq.2) then                 
      units = "degF"
  else
      if (iounit(1).eq.0) then
          HI = (HI-32)*0.55555
          units = "degC"
      else
          HI = (HI+459.67)*0.55555
          units = "degK"
      end if
  end if

  HI@long_name = "heat index: NWS"
  HI@units     = units
  HI@www       = "http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml"
  HI@info      = "appropriate for shady locations with no wind"
  if (eqnType.eq.0) then
      HI@tag   = "NCL: heat_index_nws; (Steadman+t)*0.5"
  else
      HI@tag   = "NCL: heat_index_nws; (Steadman+t)*0.5 and Rothfusz"            
  end if
  copy_VarCoords(t, HI)

  return(HI)
end
;----------------------------------------------------------
undef("latent_heat_water")
function latent_heat_water(t:numeric, iounit[2]:integer, key[1]:integer, opt[1]:logical)
;
;   Baker, Schlatter  17-may-1982     original version.
;
;   This function returns the latent heat of
;               evaporation/condensation         for key=1
;               melting/freezing                 for key=2
;               sublimation/deposition           for key=3
;   for water. The latent heat heatl (joules per kilogram) is a
;   function of temperature t. The formulas are polynomial
;   approximations to the values in table 92, p. 343 of the Smithsonian
;   Meteorological Tables, sixth revised edition, 1963 by Roland List.
;   The approximations were developed by Eric Smith at Colorado State University.
;   polynomial coefficients

;   iounit(0)=0 means the input are in degrees C (degC);
;   iounit(0)=1 means the input are in degrees K (degK);
;   iounit(0)=2 means the input are in degrees F. 

;   iounit(1)=0 means the output are in J/kg
;   iounit(1)=1 means the output are in J/g
;   iounit(1)=2 means the output are in cal/g

;   1 J/g = 0.238846 cal/g; 1 cal/g = 4.1868 J/g

local ttype, t0, con, cun, a, tk, heatl, lname
begin
  ttype = typeof(t)

  if (ttype.eq."double") then
      t0  = 273.15d
      con = 5.0d/9.0d
      cun = (/1.0d, 0.001d, 0.238846d /)
  else
      t0  = 273.15
      con = 5.0/9.0
      cun = (/1.0, 0.001, 0.238846 /)
  end if

; Polynomial Coefficients

  if (key.eq.1) then
      if (ttype.eq."double") then
          a = (/ 3337118.5d, -3642.8583d,  2.1263947d /)
      else
          a = (/ 3337118.5 , -3642.8583 ,  2.1263947  /)
      end if
      lname = "Latent Heat: evaporation/condensation"
  else if (key.eq.2) then
      if (ttype.eq."double") then
          a = (/-1161004.0d,  9002.2648d, -12.931292d /)
      else
          a = (/-1161004.0 ,  9002.2648 , -12.931292  /)
      end if
      lname = "Latent Heat: melting/freezing"
  else if (key.eq.3) then
      if (ttype.eq."double") then
          a = (/ 2632536.8d,  1726.9659d, -3.6248111d /)
      else
          a = (/ 2632536.8 ,  1726.9659 , -3.6248111  /)
      end if
      lname = "Latent Heat: sublimation/deposition"
  else
      print("latent_heat_water: illegal key value: key="+key)
      exit
  end if   ; key=3
  end if   ; key=2
  end if   ; key=1

  if (iounit(0).eq.0) then        ; input in C; convert to K 
      heatl = a(0)+ a(1)*(t+t0) + a(2)*(t+t0)^2
  else if (iounit(0).eq.1) then   ; input in K; no need convert units
      heatl = a(0)+ a(1)*t + a(2)*t*t
  else if (iounit(0).eq.2) then   ; input in F; convert to K 
      tk    = (t-32)*con + t0
      heatl = a(0)+ a(1)*tk + a(2)*tk^2
  end if   ; iounit(0)=2
  end if   ; iounit(0)=1
  end if   ; iounit(0)=0

 ;print("latent_heat_water: key="+key+"  heatl="+heatl)

; meta data

  heatl@long_name = lname

  if (iounit(1).eq.0) then
      heatl@units = "J/kg"         ; joules per kilogram 
  else if (iounit(1).eq.1) then
      heatl = heatl*cun(1) 
      heatl@units = "J/g"          ; joules per gram 
  else if (iounit(1).eq.2) then
      heatl = (heatl*cun(1))*cun(2)
      heatl@units = "cal/g"        ; calories (IT) per gram 
  end if   ; iounit(0)=2
  end if   ; iounit(0)=1
  end if   ; iounit(0)=0

  copy_VarCoords(t, heatl)         ; copy coords
  return(heatl)
end     ; latent_heat_water
;----------------------------------------------------------
undef("calculate_monthly_values")
function calculate_monthly_values (x:numeric, arith:string, nDim[1]:integer, opt[1]:logical)
;
; calculate monthly values [avg, sum, min, max]
; x: numeric array of 5D or less [eg: time,lev,lat,lon]
;    *must* have time coordinate recognized by cd_calendar
;    if 5D [case,time,lev,lat,lon]
; arith: "avg" [also, "ave"], "sum","min","max"     others may be added later
; nDim : scalar integer that specifies the 'time' dimension [generally, 0]
; opt  : only used to eliminate a warning message
;        opt= 0 ... default ... print Warning
;        opt=-1 ... do not print Warning
;
; Sample usage:  x(time,lat,lon) where time are n-hrly or daily values.
;        xMonthAvg = calculate_monthly_values(x, "avg", 0, False)
;        xMonthSum = calculate_monthly_values(x, "sum", 0, False)
;        xMonthMin = calculate_monthly_values(x, "min", 0, False)
;        xMonthMax = calculate_monthly_values(x, "max", 0, False)
;        xMonthVar = calculate_monthly_values(x, "var", 0, False)
;        xMonthStd = calculate_monthly_values(x, "std", 0, False)
;                  ************
; Note that NCL's 'dimension reduction' which, in general, I view as a feature,
;      introduces some processing issues in the 'corner case' of one value.
;      This requires extra (nuisance) steps and leads to 'code bloat'.  :-(
;                  ************
; If this function is slow, consider using the Climate Data Operator (CDO)
;      cdo monmean foo_hourly_or_daily.nc  foo_monthly_mean.nc
;      cdo monmin  foo_hourly_or_daily.nc  foo_monthly_min.nc
;      cdo monmax  foo_hourly_or_daily.nc  foo_monthly_max.nc
;      cdo monsum  foo_hourly_or_daily.nc  foo_monthly_sum.nc
;                  ************
;
local dnamx, TNAME, TIME, dimx, rankx, NTIM, utc_date, year, month, ntim, work
     ,yyyy, mm, nt, it, nmos, xStat, nval_crit, yrStrt, yrLast
begin
    if (.not.(arith.eq."ave" .or. arith.eq."avg" .or. arith.eq."sum"  \
                             .or. arith.eq."min" .or. arith.eq."max"  \     
                             .or. arith.eq."var" .or. arith.eq."std") ) then
        print("calculate_monthly_values: unrecognizezed 'arith' argument="+arith)
        exit
    end if

; a monthly mean will be calculated only if the # of values is >= nval_crit

    nval_crit = 1                                  ; default
    if (opt .and. isatt(opt,"nval_crit")) then
        nval_crit = toint(opt@nval_crit)
    end if

    dnamx    = getvardims(x)                       ; dimension names
    TNAME    = dnamx(nDim)                         ; typically TNAME="time"
 
    dimx     = dimsizes(x)                         ; dimensions sizes
    rankx    = dimsizes(dimx)                      ; # of dimensions
    ntim     = dimx(nDim)                          ; size of input time dimension

;--- Create current yyyy/mm
;   'cd_calendar' recognizes any 'calendar' attribute associated with ' x&$TNAME$ '.
;    Hence, the elements below (yyyy, month, ..) reflect that calendar.
;    No calendar attributes defaults to gregorian [standard].

    utc_date = cd_calendar(x&$TNAME$, 0)           ; (x&$TNAME$, 0) ; typically   x&time
    year     = floattointeger(utc_date(:,0))
    month    = floattointeger(utc_date(:,1))
    day      = floattointeger(utc_date(:,2))
   ;hour     = floattointeger(utc_date(:,3))
   ;minute   = floattointeger(utc_date(:,4))
   ;second   = utc_date(:,5)

;--- Create monthly coordinate variable. Size must be determined because
;    each month *may* have different number of time steps. 
;    An example might be a field program.

    nt       =  -1                                 ; number of individual days
    work     = new (ntim, typeof(x&$TNAME$), getVarFillValue(x&$TNAME$)) 

    yrStrt   = min(year)
    yrLast   = max(year)

    do yyyy=yrStrt,yrLast
      do mm=min(month),max(month)
         it := ind(yyyy.eq.year .and. mm.eq.month)
         if (.not.all(ismissing(it))) then
             nt   = nt+1
             work(nt) = (/ x&$TNAME$(it(0)) /)     ; use 1st encountered time  
             nit  = dimsizes(it)                   ; for print only
           ;;print("yyyy="+yyyy+"  mm="+mm +"  nit="+nit+"  it(0)="+it(0)+" nt="+nt)
         end if    ; .not.all(ismissing(it))
      end do       ; mm
    end do         ; yyyy

    if (nt.lt.0) then
        print("WARNING: calculate_monthly_values: no values for period: "+ \
               yrStrt+"-"+yrLast)
        xStat      = getVarFillValue(x)
        xStat!0    = "time"
        xStat&time = getVarFillValue(x)
        xStat@long_name = "All Missing Values"
        return(xStat)
    end if
    NTIM = nt+1           ; number of months
  ;;print("NTIM="+NTIM)

    TIME   = work(0:nt)
    TIME!0 = "TIME"
    if (isatt(x&$TNAME$,"long_name")) then
        TIME@long_name = x&$TNAME$@long_name 
    else
        TIME@long_name = "time corresponding to 1st time of current month"
       TIME@long_name = "time corresponding to 1st encountered time of current month"
 
    end if
    if (isatt(x&$TNAME$,"units")) then
        TIME@units     = x&$TNAME$@units 
    end if
    if (isatt(x&$TNAME$,"calendar")) then
        TIME@calendar  = x&$TNAME$@calendar 
    end if

    TIME&TIME = TIME               ; create coordinate variable 
    delete(work)                   ; no longer needed

    nt   = -1
    nmos = 12

    if (rankx.eq.1) then
        xStat  = new ( (/NTIM/), typeof(x), getVarFillValue(x))

        do yyyy=yrStrt,yrLast
          do mm=min(month),max(month)
             it  := ind(yyyy.eq.year .and. mm.eq.month) 
             if (.not.all(ismissing(it))) then
                 nit  = dimsizes(it)
                 nt = nt+1
                 if (arith.eq."avg" .or. arith.eq."ave") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt)  = dim_avg_n( x(it), nDim)
                     else               ; one 'it'
                         xStat(nt)  = x(it(0))
                     end if
                 end if
                 if (arith.eq."sum") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt)  = dim_sum_n( x(it), nDim)
                     else               ; one 'it'
                         xStat(nt)  = x(it(0))
                     end if
                 end if
                 if (arith.eq."min") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt)  = dim_min_n( x(it), nDim)
                     else               ; one 'it'
                         xStat(nt)  = x(it(0))
                     end if
                 end if
                 if (arith.eq."max") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt)  = dim_max_n( x(it), nDim)
                     else               ; one 'it'
                         xStat(nt)  = x(it(0))
                     end if
                 end if
                 if (arith.eq."var") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt)  = dim_variance_n( x(it), nDim)
                     else               ; one 'it'; variance set to zero
                         xStat(nt)  = 0.0       
                     end if
                 end if
                 if (arith.eq."std") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt)  = dim_stddev_n( x(it), nDim)
                     else               ; one 'it'; std. dev set to zero
                         xStat(nt)  = 0.0       
                     end if
                 end if
                         
                 if (nval_crit.gt.1) then
                     nit_valid = dim_num_n(.not.ismissing(x(it)),0)
                     xStat(nt) = where(nit_valid.ge.nval_crit,  xStat(nt),  xStat@_FillValue)
                 end if ; nval_crit

             end if     ; .not.all(ismissing(it))
          end do        ; mm
        end do          ; yyyy

        copy_VarAtts(x, xStat)
    end if              ; rankx.eq.1 

    if (rankx.eq.2) then
        xStat  = new ( (/NTIM,dimx(1)/), typeof(x), getVarFillValue(x))

        do yyyy=yrStrt,yrLast
          do mm=min(month),max(month)
             it  := ind(yyyy.eq.year .and. mm.eq.month) 
             if (.not.all(ismissing(it))) then
                 nit = dimsizes(it)
                 nt  = nt+1
                 if (arith.eq."avg" .or. arith.eq."ave") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:)  = dim_avg_n( x(it,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:)  = x(it(0),:)
                     end if
                 end if
                 if (arith.eq."sum") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:)  = dim_sum_n( x(it,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:)  = x(it(0),:)
                     end if
                 end if
                 if (arith.eq."min") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:)  = dim_min_n( x(it,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:)  = x(it(0),:)
                     end if
                 end if
                 if (arith.eq."max") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:)  = dim_max_n( x(it,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:)  = x(it(0),:)
                     end if
                 end if
                 if (arith.eq."var") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:)  = dim_variance_n( x(it,:), nDim)
                     else               ; one 'it'; variance set to zero
                         xStat(nt,:)  = 0.0       
                     end if
                 end if
                 if (arith.eq."std") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:)  = dim_stddev_n( x(it,:), nDim)
                     else               ; one 'it'; std. dev set to zero
                         xStat(nt,:)  = 0.0       
                     end if
                 end if
                         
                 if (nval_crit.gt.1) then
                     nit_valid   = dim_num_n(.not.ismissing(x(it,:)),0)
                     xStat(nt,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:),  xStat@_FillValue)
                 end if  ; nval_crit

             end if    ; .not.all(ismissing(it))
          end do       ; mm
        end do         ; yyyy

        copy_VarMeta(x(0,:), xStat(0,:))
    end if             ; rankx.eq.2 

    if (rankx.eq.3) then
        xStat  = new ( (/NTIM,dimx(1),dimx(2)/), typeof(x), getVarFillValue(x))

        do yyyy=yrStrt,yrLast
          do mm=min(month),max(month)
             it  := ind(yyyy.eq.year .and. mm.eq.month) 
             if (.not.all(ismissing(it))) then
                 nt  = nt+1
                 nit = dimsizes(it)
                 if (arith.eq."avg" .or. arith.eq."ave") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:)  = dim_avg_n( x(it,:,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:,:)  = x(it(0),:,:)
                     end if
                 end if
                 if (arith.eq."sum") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:)  = dim_sum_n( x(it,:,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:,:)  = x(it(0),:,:)
                     end if
                 end if
                 if (arith.eq."min") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:)  = dim_min_n( x(it,:,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:,:)  = x(it(0),:,:)
                     end if
                 end if
                 if (arith.eq."max") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:)  = dim_max_n( x(it,:,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:,:)  = x(it(0),:,:)
                     end if
                 end if
                 if (arith.eq."var") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:)  = dim_variance_n( x(it,:,:), nDim)
                     else               ; one 'it'; variance set to zero
                         xStat(nt,:,:)  = 0.0       
                     end if
                 end if
                 if (arith.eq."std") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:)  = dim_stddev_n( x(it,:,:), nDim)
                     else               ; one 'it'; std. dev set to zero
                         xStat(nt,:,:)  = 0.0       
                     end if
                 end if
                         
                 if (nval_crit.gt.1) then
                     nit_valid     = dim_num_n(.not.ismissing(x(it,:,:)),0)
                     xStat(nt,:,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:,:),  xStat@_FillValue)
                 end if  ; nval_crit

             end if    ; .not.all(ismissing(it))
          end do       ; mm
        end do         ; yyyy

        copy_VarMeta(x(0,:,:), xStat(0,:,:))
    end if             ; rankx.eq.3 

    if (rankx.eq.4) then
        xStat  = new ( (/NTIM,dimx(1),dimx(2),dimx(3)/), typeof(x), getVarFillValue(x))

        do yyyy=yrStrt,yrLast
          do mm=min(month),max(month)
             it  := ind(yyyy.eq.year .and. mm.eq.month) 
             if (.not.all(ismissing(it))) then
                 nit = dimsizes(it)
                 nt  = nt+1
                 if (arith.eq."avg" .or. arith.eq."ave") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:,:)  = dim_avg_n( x(it,:,:,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:,:,:)  = x(it(0),:,:,:)
                     end if
                 end if
                 if (arith.eq."sum") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:,:)  = dim_sum_n( x(it,:,:,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:,:,:)  = x(it(0),:,:,:)
                     end if
                 end if
                 if (arith.eq."min") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:,:)  = dim_min_n( x(it,:,:,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:,:,:)  = x(it(0),:,:,:)
                     end if
                 end if
                 if (arith.eq."max") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:,:)  = dim_max_n( x(it,:,:,:), nDim)
                     else               ; one 'it'
                         xStat(nt,:,:,:)  = x(it(0),:,:,:)
                     end if
                 end if
                 if (arith.eq."var") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:,:)  = dim_variance_n( x(it,:,:,:), nDim)
                     else               ; one 'it'; variance set to zero
                         xStat(nt,:,:,:)  = 0.0       
                     end if
                 end if
                 if (arith.eq."std") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(nt,:,:,:)  = dim_stddev_n( x(it,:,:,:), nDim)
                     else               ; one 'it'; std. dev set to zero
                         xStat(nt,:,:,:)  = 0.0       
                     end if
                 end if
                         
                 if (nval_crit.gt.1) then
                     nit_valid       = dim_num_n(.not.ismissing(x(it,:,:,:)),0)
                     xStat(nt,:,:,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:,:,:),  xStat@_FillValue)
                 end if  ; nval_crit

             end if    ; .not.all(ismissing(it))
          end do       ; mm
        end do         ; yyyy

        copy_VarMeta(x(0,:,:,:), xStat(0,:,:,:))
    end if             ; rankx.eq.4 

    if (rankx.eq.5) then
        xStat  = new ( (/dimx(0),NTIM,dimx(2),dimx(3),dimx(4)/), typeof(x), getVarFillValue(x))

        do yyyy=yrStrt,yrLast
          do mm=min(month),max(month)
             it  := ind(yyyy.eq.year .and. mm.eq.month) 
             if (.not.all(ismissing(it))) then
                 nit  = dimsizes(it)
                 nt = nt+1
                 if (arith.eq."avg" .or. arith.eq."ave") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(:,nt,:,:,:)  = dim_avg_n( x(:,it,:,:,:), nDim)
                      else               ; one 'it'
                         xStat(:nt,:,:,:)  = x(:,it(0),:,:,:)
                     end if
                 end if
                 if (arith.eq."sum") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(:,nt,:,:,:)  = dim_sum_n( x(:,it,:,:,:), nDim)
                     else               ; one 'it'
                         xStat(:,nt,:,:,:)  = x(:,it(0),:,:,:)
                     end if
                 end if
                 if (arith.eq."min") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(:,nt,:,:,:)  = dim_min_n( x(:,it,:,:,:), nDim)
                     else               ; one 'it'
                         xStat(:,nt,:,:,:)  = x(:,it(0),:,:,:)
                     end if
                 end if
                 if (arith.eq."max") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(:,nt,:,:,:)  = dim_max_n( x(:,it,:,:,:), nDim)
                     else               ; one 'it'
                         xStat(:,nt,:,:,:)  = x(:,it(0),:,:,:)
                     end if
                 end if
                 if (arith.eq."var") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(:,nt,:,:,:)  = dim_variance_n( x(:,it,:,:,:), nDim)
                     else               ; one 'it'; set variance to zero
                         xStat(:,nt,:,:,:)  = 0.0
                     end if
                 end if
                 if (arith.eq."std") then
                     if (nit.gt.1) then ; work around for NCL's dimension reduction
                         xStat(:,nt,:,:,:)  = dim_stddev_n( x(:,it,:,:,:), nDim)
                     else               ; one 'it'; set set. dev. to zero
                         xStat(:,nt,:,:,:)  = 0.0
                     end if
                 end if
                         
                 if (nval_crit.gt.1) then
                     nit_valid         = dim_num_n(.not.ismissing(x(it,:,:,:,:)),0)
                     xStat(nt,:,:,:,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:,:,:,:),  xStat@_FillValue)
                 end if  ; nval_crit

             end if    ; .not.all(ismissing(it))
          end do       ; mm
        end do         ; yyyy
    end if             ; rankx.eq.5 

    xStat!nDim    = TNAME 
    xStat&$TNAME$ = TIME
    xStat@NCL_tag = "calculate_monthly_values: arith="+arith
    return(xStat)
end
;----------------------------------------------------------
undef("calculate_daily_values")
function calculate_daily_values (x:numeric, arith:string, nDim[1]:integer, opt[1]:logical)
;                  ************
; Note that NCL's 'dimension reduction' which, in general, I view as a feature,
;      introduces some processing issues in the 'corner case' of one value.
;      This requires extra (nuisance) steps and leads to 'code bloat'.  :-(
;                  ************
; If this function is slow, consider using the Climate Data Operator (CDO)
;      cdo daymean foo_hourly.nc  foo_daily_mean.nc
;      cdo daymin  foo_hourly.nc  foo_daily_min.nc
;      cdo daymax  foo_hourly.nc  foo_daily_max.nc
;      cdo daysum  foo_hourly.nc  foo_daily_sum.nc
;                  ************
local dnamx, TIME, TNAME, dimx, rankx, NTIM, utc_date, year, month, day \
     ,ntim, work, yyyy, mm, dd, dymon, it, nt, nmos, xSta, nval_crit, yrStrt, yrLast
begin
    if (.not.(arith.eq."ave" .or. arith.eq."avg" .or. arith.eq."sum"  \
                             .or. arith.eq."min" .or. arith.eq."max") ) then
        print("calculate_daily_values: unrecognizezed 'arith' argument="+arith)
        exit
    end if

; a daily mean will be calculated only if the # of values is >= nval_crit

    if (opt .and. isatt(opt,"nval_crit") .and. typeof(opt@nval_crit).eq."integer") then
        nval_crit = opt@nval_crit
    else
        nval_crit = 1
    end if

    dnamx    = getvardims(x)                       ; dimension names
    TNAME    = dnamx(nDim)                         ; typically TNAME="time"
 
    dimx     = dimsizes(x)
    rankx    = dimsizes(dimx)                      ; # of dimensions
    ntim     = dimx(nDim)                          ; size of input time dimension

;--- Create current yyyy/mm/dd
;   'cd_calendar' recognizes any 'calendar' attribute associated with ' x&$TNAME$ '.
;    Hence, the elements below (yyyy, month, ..) reflect that calendar.
;    No calendar attribute defaults to gregorian [standard].

    utc_date = cd_calendar(x&$TNAME$, 0)           ; (x&$TNAME$, 0) ; typically   x&time
    year     = floattointeger(utc_date(:,0))
    month    = floattointeger(utc_date(:,1))
    day      = floattointeger(utc_date(:,2))
    hour     = floattointeger(utc_date(:,3))
   ;minute   = floattointeger(utc_date(:,4))
   ;second   = utc_date(:,5)

    yrStrt   = min(year)
    yrLast   = max(year)

;--- Create daily coordinate variable. Size must be determined because
;    each day *may* have different number of time steps. 
;    An example might be a field program.

    nt       =  -1                                 ; number of individual days
    work     = new (ntim, typeof(x&$TNAME$), getVarFillValue(x&$TNAME$)) 
    do yyyy=yrStrt,yrLast
      do mm=min(month),max(month)
         dymon  = days_in_month ( yyyy, mm) 
        do dd=1,dymon          
           it  := ind(yyyy.eq.year .and. mm.eq.month .and. dd.eq.day) 
           nit  = dimsizes(it)
           if (.not.ismissing(it(0)) .and. nit.ge.nval_crit) then
               nt = nt+1
               work(nt) = (/ x&$TNAME$(it(0)) /)   ; use 1st time of current day  
           end if
        end do     ; dd
      end do       ; mm
    end do         ; yyyy
    NTIM = nt+1    ; get 1-based total

    TIME   = work(0:nt)
    TIME!0 = "TIME"
    if (isatt(x&$TNAME$,"long_name")) then
        TIME@long_name = x&$TNAME$@long_name 
    else
        TIME@long_name = "time corresponding to 1st time of current day"
    end if
    if (isatt(x&$TNAME$,"units")) then
        TIME@units     = x&$TNAME$@units 
    end if
    if (isatt(x&$TNAME$,"calendar")) then
        TIME@calendar  = x&$TNAME$@calendar
    end if

    TIME&TIME = TIME               ; create coordinate variable 
    delete(work)                   ; no longer needed

    nt   = -1
    nmos = 12

    if (rankx.eq.1) then
        xStat  = new ( (/NTIM/), typeof(x), getVarFillValue(x))

        do yyyy=min(year),max(year)
          do mm=min(month),max(month)
             dymon = days_in_month ( yyyy, mm) 
            do dd=1,dymon          ; min(day),max(day)
               it := ind(yyyy.eq.year .and. mm.eq.month .and. dd.eq.day) 
               nit = dimsizes(it)
               if (.not.ismissing(it(0)) .and. nit.ge.nval_crit) then
                   nt = nt+1
                   if (arith.eq."avg" .or. arith.eq."ave") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt)  = dim_avg_n( x(it), nDim)
                       else               ; one 'it'
                           xStat(nt)  = x(it(0))
                       end if
                   end if
                   if (arith.eq."sum") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt)  = dim_sum_n( x(it), nDim)
                       else               ; one 'it'
                           xStat(nt)  = x(it(0))
                       end if
                   end if
                   if (arith.eq."min") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt)  = dim_min_n( x(it), nDim)
                       else               ; one 'it'
                           xStat(nt)  = x(it(0))
                       end if
                   end if
                   if (arith.eq."max") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt)  = dim_max_n( x(it), nDim)
                       else               ; one 'it'
                           xStat(nt)  = x(it(0))
                       end if
                   end if
                         
                   if (nval_crit.gt.1) then
                       nit_valid  = dim_num_n(.not.ismissing(x(it)),0)
                       xStat(nt)  = where( nit_valid.ge.nval_crit,  xStat(nt),  xStat@_FillValue)
                   end if  ; nval_crit

               end if  ; .not.ismissing(it(0))
            end do     ; dd
          end do       ; mm
        end do         ; yyyy

        copy_VarAtts(x, xStat)
    end if             ; rankx.eq.1 

    if (rankx.eq.2) then
        xStat  = new ( (/NTIM,dimx(1)/), typeof(x), getVarFillValue(x))

        do yyyy=min(year),max(year)
          do mm=min(month),max(month)
             dymon = days_in_month ( yyyy, mm) 
            do dd=1,dymon          ; min(day),max(day)
               it := ind(yyyy.eq.year .and. mm.eq.month .and. dd.eq.day) 
               nit = dimsizes(it)
               if (.not.ismissing(it(0)) .and. nit.ge.nval_crit) then
                   nt = nt+1
                   if (arith.eq."avg" .or. arith.eq."ave") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:)  = dim_avg_n( x(it,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:)  = x(it(0),:)
                       end if
                   end if
                   if (arith.eq."sum") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:)  = dim_sum_n( x(it,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:)  = x(it(0),:)
                       end if
                   end if
                   if (arith.eq."min") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:)  = dim_min_n( x(it,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:)  = x(it(0),:)
                       end if
                   end if
                   if (arith.eq."max") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:)  = dim_max_n( x(it,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:)  = x(it(0),:)
                       end if
                   end if
                        
                   if (nval_crit.gt.1) then
                       nit_valid   = dim_num_n(.not.ismissing(x(it,:)),0)
                       xStat(nt,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:),  xStat@_FillValue)
                   end if  ; nval_crit

               end if  ; .not.ismissing(it(0))
            end do     ; dd
          end do       ; mm
        end do         ; yyyy

        copy_VarMeta(x(0,:), xStat(0,:))
    end if             ; rankx.eq.2 

    if (rankx.eq.3) then
        xStat  = new ( (/NTIM,dimx(1),dimx(2)/), typeof(x), getVarFillValue(x))

        do yyyy=min(year),max(year)
          do mm=min(month),max(month)
             dymon = days_in_month ( yyyy, mm) 
            do dd=min(day),max(day)
               it := ind(yyyy.eq.year .and. mm.eq.month .and. dd.eq.day) 
               nit = dimsizes(it)
               if (.not.ismissing(it(0)) .and. nit.ge.nval_crit) then
                   nt = nt+1
                   if (arith.eq."avg" .or. arith.eq."ave") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:)  = dim_avg_n( x(it,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:)  = x(it(0),:,:)
                       end if
                   end if
                   if (arith.eq."sum") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:)  = dim_sum_n( x(it,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:)  = x(it(0),:,:)
                       end if
                   end if
                   if (arith.eq."min") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:)  = dim_min_n( x(it,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:)  = x(it(0),:,:)
                       end if
                   end if
                   if (arith.eq."max") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:)  = dim_max_n( x(it,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:)  = x(it(0),:,:)
                       end if
                   end if
                         
                   if (nval_crit.gt.1) then
                       nit_valid     = dim_num_n(.not.ismissing(x(it,:,:)),0)
                       xStat(nt,:,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:,:),  xStat@_FillValue)
                   end if  ; nval_crit

               end if  ; .not.ismissing(it(0))
            end do     ; dd
          end do       ; mm
        end do         ; yyyy

        copy_VarMeta(x(0,:,:), xStat(0,:,:))
    end if             ; rankx.eq.3 

    if (rankx.eq.4) then
        xStat  = new ( (/NTIM,dimx(1),dimx(2),dimx(3)/), typeof(x), getVarFillValue(x))

        do yyyy=min(year),max(year)
          do mm=min(month),max(month)
             dymon = days_in_month ( yyyy, mm) 
            do dd=min(day),max(day)
               it := ind(yyyy.eq.year .and. mm.eq.month .and. dd.eq.day) 
               nit = dimsizes(it)
               if (.not.ismissing(it(0)) .and. nit.ge.nval_crit) then
                   nt = nt+1
                   if (arith.eq."avg" .or. arith.eq."ave") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:,:)  = dim_avg_n( x(it,:,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:,:)  = x(it(0),:,:,:)
                       end if
                   end if
                   if (arith.eq."sum") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:,:)  = dim_sum_n( x(it,:,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:,:)  = x(it(0),:,:,:)
                       end if
                   end if
                   if (arith.eq."min") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:,:)  = dim_min_n( x(it,:,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:,:)  = x(it(0),:,:,:)
                       end if
                   end if
                   if (arith.eq."max") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:,:)  = dim_max_n( x(it,:,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:,:)  = x(it(0),:,:,:)
                       end if
                   end if
                        
                   if (nval_crit.gt.1) then
                       nit_valid       = dim_num_n(.not.ismissing(x(it,:,:,:)),0)
                       xStat(nt,:,:,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:,:,:),  xStat@_FillValue)
                   end if  ; nval_crit


               end if  ; .not.ismissing(it(0))
            end do     ; dd
          end do       ; mm
        end do         ; yyyy

        copy_VarMeta(x(0,:,:,:), xStat(0,:,:,:))
    end if             ; rankx.eq.4 

    if (rankx.eq.5) then
        xStat  = new ( (/dimx(0),NTIM,dimx(2),dimx(3),dimx(4)/), typeof(x), getVarFillValue(x))
        do yyyy=min(year),max(year)
          do mm=min(month),max(month)
             dymon = days_in_month ( yyyy, mm) 
            do dd=min(day),max(day)
               it := ind(yyyy.eq.year .and. mm.eq.month .and. dd.eq.day) 
               nit = dimsizes(it)
               if (.not.ismissing(it(0)) .and. nit.ge.nval_crit) then
                   nt = nt+1
                   if (arith.eq."avg" .or. arith.eq."ave") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(:,nt,:,:,:)  = dim_avg_n( x(:,it,:,:,:), nDim)
                       else               ; one 'it'
                           xStat(:,nt,:,:,:)  = x(:,it(0),:,:,:)
                       end if
                   end if
                   if (arith.eq."sum") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(:,nt,:,:,:)  = dim_sum_n( x(:,it,:,:,:), nDim)
                       else               ; one 'it'
                           xStat(:,nt,:,:,:)  = x(:,it(0),:,:,:)
                       end if
                   end if
                   if (arith.eq."min") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(:,nt,:,:,:)  = dim_min_n( x(:,it,:,:,:), nDim)
                       else               ; one 'it'
                           xStat(:,nt,:,:,:)  = x(:,it(0),:,:,:)
                       end if
                   end if
                   if (arith.eq."max") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(:,nt,:,:,:)  = dim_max_n( x(:,it,:,:,:), nDim)
                       else               ; one 'it'
                           xStat(:,nt,:,:,:)  = x(:,it(0),:,:,:)
                       end if
                   end if
                         
                   if (nval_crit.gt.1) then
                       nit_valid         = dim_num_n(.not.ismissing(x(it,:,:,:,:)),0)
                       xStat(nt,:,:,:,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:,:,:,:),  xStat@_FillValue)
                   end if  ; nval_crit

               end if  ; .not.ismissing(it(0))
            end do     ; dd
          end do       ; mm
        end do         ; yyyy

        copy_VarMeta(x(:,0,:,:,:), xStat(:,0,:,:,:))
    end if             ; rankx.eq.5 

    xStat!nDim    = TNAME 
    xStat&$TNAME$ = TIME

    xStat@NCL_tag = "calculate_daily_values: arith="+arith
    return(xStat)
end
;---------------------------------------------------------------------
undef("calculate_segment_values")
function calculate_segment_values (x:numeric, arith:string, nDim[1]:integer, opt[1]:logical)
;                  ************
; Note that NCL's 'dimension reduction' which, in general, I view as a feature,
;      introduces some processing issues in the 'corner case' of one value.
;      This requires extra (nuisance) steps and leads to 'code bloat'.  :-(
;                  ************
local dnamx, TIME, TNAME, dimx, rankx, NTIM, utc_date, year, month, day \
     ,ntim, work, yyyy, mm, dd, dymon, it, nt, nmos, xSta, nval_crit, lseg, nseg
begin
    if (.not.(arith.eq."ave" .or. arith.eq."avg" .or. arith.eq."sum"  \
                             .or. arith.eq."min" .or. arith.eq."max") ) then
        print("calculate_segment_values: unrecognizezed 'arith' argument="+arith)
        exit
    end if

; define segment length in terms of 'day length' ... default is 7

    lseg = 7                                       ; default [7 days]
    if (opt .and. isatt(opt,"segment_length")) then
        lseg = toint(opt@segment_length)
    end if
    lseg1 = lseg-1

; a segment mean will be calculated only if the # of values is >= nval_crit

    nval_crit = 1                                  ; default [1 day]
    if (opt .and. isatt(opt,"nval_crit")) then
        nval_crit = toint(opt@nval_crit)
    end if

    dnamx    = getvardims(x)                       ; dimension names
    TNAME    = dnamx(nDim)                         ; typically TNAME="time"
    TIME     = x&$TNAME$                           ; extract for convenience

; cd_convert has bug recognizing calendar attribute
; This function currently works with the 'standard' and 'gregorian' calendars. 
; No calendar attribute defaults to 'gregorian.'

    if (isatt(TIME,"calendar") .and. \
        .not.(TIME@calendar.eq."standard" .or. TIME@calendar.eq."gregorian")) then
        print("calculate_segment_values: does not work with calendar="+TIME@calendar)
        exit
    end if 
 
    dimx     = dimsizes(x)
    rankx    = dimsizes(dimx)                      ; # of dimensions
    ntim     = dimx(nDim)                          ; size of input time dimension

;--- Create current yyyy/mm/dd

    utc_date = cd_calendar(TIME, 0)                ; (x&$TNAME$, 0) ; typically   x&time
    year     = floattointeger(utc_date(:,0))
    month    = floattointeger(utc_date(:,1))
    day      = floattointeger(utc_date(:,2))
    hour     = floattointeger(utc_date(:,3))
   ;minute   = floattointeger(utc_date(:,4))
   ;second   = utc_date(:,5)

;--- Trick: create a 'time' variable in units of 'days since' the start of the input array.

    timeDay  = cd_convert( TIME, "days since "+year(0)+"-"+month(0)+"-"+day(0)+" 00:00" )
    timeDay@info = "indicates the start time of the current segment"
  ;;print(timeDay)  ; for (non-standard; non-gregorian) this will have episodic 1 day gaps

    nday_span= round((timeDay(ntim-1)-timeDay(0)), 3)  ; last_day - start_day; start_day=0  
    nseg     = nday_span/lseg                          ; eg: lseg=7;  nseg= # weeks

    nt       = -1
    ndyStrt  = 0
    ndyLast  = lseg1
    timeSeg  = new( nseg, typeof(timeDay)) 

   ;print(year+"  "+month+"  "+day+"  "+timeDay)
   ;print("nday_span="+nday_span+"; nseg="+nseg)
   ;print("rankx="+rankx+"  ndyStrt="+ndyStrt+"  ndyLast="+ndyLast)

    if (rankx.eq.1) then
        xStat  = new ( (/nseg/), typeof(x), getVarFillValue(x))

        do ns=0,nseg-1
           it := ind(timeDay.ge.ndyStrt .and. timeDay.le.ndyLast)
           if (.not.all(ismissing(it))) then
               nit = dimsizes(it)
               nt  = nt+1

               if (.not.ismissing(it(0))) then
                   timeSeg(nt) = timeDay(it(0))
    
                   if (arith.eq."avg" .or. arith.eq."ave") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt)  = dim_avg_n( x(it), nDim)
                       else               ; one 'it'
                           xStat(nt)  = x(it(0))
                       end if
                   end if
                   if (arith.eq."sum") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt)  = dim_sum_n( x(it), nDim)
                       else               ; one 'it'
                           xStat(nt)  = x(it(0))
                       end if
                   end if
                   if (arith.eq."min") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt)  = dim_min_n( x(it), nDim)
                       else               ; one 'it'
                           xStat(nt)  = x(it(0))
                       end if
                   end if
                   if (arith.eq."max") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt)  = dim_max_n( x(it), nDim)
                       else               ; one 'it'
                           xStat(nt)  = x(it(0))
                       end if
                   end if

                   if (nval_crit.gt.1) then
                       nit_valid = dim_num_n(.not.ismissing(x(it)),0)   
                       xStat(nt) = where(nit_valid.ge.nval_crit,  xStat(nt),  xStat@_FillValue)
                   end if             ;  nval_crit
               end if                 ; .not.ismissing(it(0))
           end if                     ; .not.all(ismissing(it))

           ndyStrt = ndyLast+1          ; update segment bounds
           ndyLast = ndyStrt+lseg1
        end do         ; ns (segment loop)         

        copy_VarAtts(x, xStat)
    end if             ; rankx.eq.1 

    if (rankx.eq.2) then
        xStat  = new ( (/nseg,dimx(1)/), typeof(x), getVarFillValue(x))

        do ns=0,nseg-1
           it  := ind(timeDay.ge.ndyStrt .and. timeDay.le.ndyLast)
           if (.not.all(ismissing(it))) then
               nit  = dimsizes(it)
               nt   = nt+1

               if (.not.ismissing(it(0))) then
                   timeSeg(nt) = timeDay(it(0))
    
                   if (arith.eq."avg" .or. arith.eq."ave") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:)  = dim_avg_n( x(it,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:)  = x(it(0),:)
                       end if
                   end if
                   if (arith.eq."sum") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:)  = dim_sum_n( x(it,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:)  = x(it(0),:)
                       end if
                   end if
                   if (arith.eq."min") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:)  = dim_min_n( x(it,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:)  = x(it(0),:)
                       end if
                   end if
                   if (arith.eq."max") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:)  = dim_max_n( x(it,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:)  = x(it(0),:)
                       end if
                   end if
    
                            
                   if (nval_crit.gt.1) then
                       nit_valid   = dim_num_n(.not.ismissing(x(it,:)),0)
                       xStat(nt,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:),  xStat@_FillValue)
                   end if                 ; nval_crit
               end if                     ; .not.ismissing(it(0))
           end if                         ; .not.all(ismissing(it))

           ndyStrt = ndyLast+1            ; update segment bounds
           ndyLast = ndyStrt+lseg1
        end do                            ; ns (segment loop)         

        copy_VarMeta(x(0,:), xStat(0,:))
    end if             ; rankx.eq.2 

    if (rankx.eq.3) then
        xStat  = new ( (/nseg,dimx(1),dimx(2)/), typeof(x), getVarFillValue(x))

        do ns=0,nseg-1
           it  := ind(timeDay.ge.ndyStrt .and. timeDay.le.ndyLast)
           if (.not.all(ismissing(it))) then
                nit  = dimsizes(it)
                nt   = nt+1

               if (.not.ismissing(it(0))) then
                   timeSeg(nt) = timeDay(it(0))
 
                   if (arith.eq."avg" .or. arith.eq."ave") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:)  = dim_avg_n( x(it,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:)  = x(it(0),:,:)
                       end if
                   end if
                   if (arith.eq."sum") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:)  = dim_sum_n( x(it,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:)  = x(it(0),:,:)
                       end if
                   end if
                   if (arith.eq."min") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:)  = dim_min_n( x(it,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:)  = x(it(0),:,:)
                       end if
                   end if
                   if (arith.eq."max") then
                       if (nit.gt.1) then ; work around for NCL's dimension reduction
                           xStat(nt,:,:)  = dim_max_n( x(it,:,:), nDim)
                       else               ; one 'it'
                           xStat(nt,:,:)  = x(it(0),:,:)
                       end if
                   end if
                             
                   if (nval_crit.gt.1) then
                       nit_valid     = dim_num_n(.not.ismissing(x(it,:,:)),0)
                       xStat(nt,:,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:,:),  xStat@_FillValue)
                   end if               ; nval_crit
               end if                   ; .not.ismissing(it(0)))
           end if                       ; .not.all(ismissing(it))

           ndyStrt = ndyLast+1          ; update segment bounds
           ndyLast = ndyStrt+lseg1
        end do         ; ns (segment loop)         

        copy_VarMeta(x(0,:,:), xStat(0,:,:))
    end if             ; rankx.eq.3 

    if (rankx.eq.4) then
        xStat  = new ( (/nseg,dimx(1),dimx(2),dimx(3)/), typeof(x), getVarFillValue(x))

        do ns=0,nseg-1
           it  := ind(timeDay.ge.ndyStrt .and. timeDay.le.ndyLast)
           if (.not.all(ismissing(it))) then
                nit  = dimsizes(it)
                nt   = nt+1

                if (.not.ismissing(it(0))) then
                    timeSeg(nt) = timeDay(it(0))
     
                    if (arith.eq."avg" .or. arith.eq."ave") then
                        if (nit.gt.1) then ; work around for NCL's dimension reduction
                            xStat(nt,:,:,:)  = dim_avg_n( x(it,:,:,:), nDim)
                        else               ; one 'it'
                            xStat(nt,:,:,:)  = x(it(0),:,:,:)
                        end if
                    end if
                    if (arith.eq."sum") then
                        if (nit.gt.1) then ; work around for NCL's dimension reduction
                            xStat(nt,:,:,:)  = dim_sum_n( x(it,:,:,:), nDim)
                        else               ; one 'it'
                            xStat(nt,:,:,:)  = x(it(0),:,:,:)
                        end if
                    end if
                    if (arith.eq."min") then
                        if (nit.gt.1) then ; work around for NCL's dimension reduction
                            xStat(nt,:,:,:)  = dim_min_n( x(it,:,:,:), nDim)
                        else               ; one 'it'
                            xStat(nt,:,:,:)  = x(it(0),:,:,:)
                        end if
                    end if
                    if (arith.eq."max") then
                        if (nit.gt.1) then ; work around for NCL's dimension reduction
                            xStat(nt,:,:,:)  = dim_max_n( x(it,:,:,:), nDim)
                        else               ; one 'it'
                            xStat(nt,:,:,:)  = x(it(0),:,:,:)
                        end if
                    end if
     
                    if (nval_crit.gt.1) then
                        nit_valid       = dim_num_n(.not.ismissing(x(it,:,:,:)),0)
                        xStat(nt,:,:,:) = where( nit_valid.ge.nval_crit,  xStat(nt,:,:,:),  xStat@_FillValue)
                    end if              ; nval_crit
                end if                  ; .not.ismissing(it(0))
           end if                       ; .not.all(ismissing(it))

           ndyStrt = ndyLast+1          ; update segment bounds
           ndyLast = ndyStrt+lseg1
        end do         ; ns (segment loop)         

        copy_VarMeta(x(0,:,:,:), xStat(0,:,:,:))
    end if             ; rankx.eq.4 

    xStat!nDim    = TNAME 
    xStat&$TNAME$ = timeSeg

    xStat@NCL_tag = "calculate_segment_values: lseg="+lseg+" days: arith="+arith
    return(xStat)
end
;---------------------------------------------------------------------
; The UNION of two sets is the set of elements which are in either set.
; For example: let A = (1,2,3) and let B = (3,4,5). 
; Now the UNION of A and B, written A union B = (1,2,3,4,5). 
; There is no need to list the 3 twice.
;---------------------------------------------------------------------
undef("venn2_union")
function venn2_union( a, b )
local AB, ab, ranka, rankb
begin
   if (typeof(a).ne.typeof(b)) then
       print("venn2_union: arguments a and b must be the same type")
       print("             typeof(a)="+typeof(a))
       print("             typeof(b)="+typeof(b))
       exit
   end if

   ranka = dimsizes(dimsizes(a))
   rankb = dimsizes(dimsizes(b))
                          
   if (ranka.eq.1 .and. rankb.eq.1) then
       AB = array_append_record(a,b, 0)
   else 
       if (ranka.eq.1) then
           AB = array_append_record(a,ndtooned(b), 0)
       else if (rankb.eq.1) then         
                AB = array_append_record(ndtooned(a), b, 0)
            else
                AB = array_append_record(ndtooned(a)   \
                                        ,ndtooned(b), 0)
            end if
       end if
   end if

   ab     = get_unique_values(AB)    ; 'get_unique_values' sorts the values
                                     ; into a one-dimenaional array
   ab@tag = "NCL: venn2_union"
   return(ab)
end

;---------------------------------------------------------------------
; The INTERSECTION of two sets is the set of elements which are in both sets.
; For example: let A = (1,2,3) and B = (3,4,5). 
; The INTERSECTION of A and B, written A intersection B = (3).
;---------------------------------------------------------------------
undef("venn2_intersection")   
function venn2_intersection( a, b )
local atype, btype, au, bu, nau, nbu, abi, k, n, m
begin
   atype = typeof(a)
   btype = typeof(b)
   if (atype.ne.btype) then
       print("venn2_intersection: arguments a and b must be the same type")
       print("                    typeof(a)="+atype)
       print("                    typeof(b)="+btype)
       exit
   end if
                                ; 'get_unique_values' sorts the elements
                                ;  and returns a one-dimensional arry
   au  = get_unique_values(a)   ; avoid duplicate 'a' entries
   bu  = get_unique_values(b)   ; avoid duplicate 'b' entries

   nau = dimsizes(au)
   nbu = dimsizes(bu)
                                ; reserve space
   abi = new (min((/nau,nbu/)), typeof(a), default_fillvalue(atype))
   k   = -1

   na  = 0                                 ; MH suggested loop
   nb  = 0
   do while(na.lt.nau.and.nb.lt.nbu)
     if (bu(nb).gt.au(na)) then
         na = na + 1
     else 
         if (au(na).gt.bu(nb)) then
             nb = nb + 1
         else                              ; they are equal         
             k = k+1
             abi(k) = au(na)
             nb = nb + 1
             na = na + 1
         end if
     end if
   end do

   if (k.eq.-1) then           
       abi    := new (1, typeof(a), default_fillvalue(atype))
       abi@tag = "venn2_intersection; NULL set; no intersection pairs"
       return(abi)
   end if

   abi@tag = "NCL: venn2_intersection"
   return(abi(0:k))                   
end
;
;---------------------------------------------------------------------
; The DIFFERENCE of two sets is the set of elements which are unique to each set.
; For example: let A = (1,2,3) and B = (3,4,5). 
; The DIFFERENCE of A and B, written A difference B = (1,2,4,5).
;---------------------------------------------------------------------
undef("venn2_difference")   
function venn2_difference( a, b )
local atype, btype, abi, nabi, au, bu, abd, AB, nAB, n
begin
   atype = typeof(a)
   btype = typeof(b)
   if (atype.ne.btype) then
       print("venn2_difference: arguments a and b must be the same type")
       print("                  typeof(a)="+atype)
       print("                  typeof(b)="+btype)
       exit
   end if
                                          ; 'get_unique_values' sorts the elements
                                          ;  and returns a one-dimensional arry
   abi  = venn2_intersection(a, b)   ; intersect (common) values
   if (ismissing(abi(0))) then            ; NO intersect
                                          ; 'get_unique_values' sorts the elements
       au  = get_unique_values(a)         ; unique 'a' [au]
       bu  = get_unique_values(b)         ; unique 'b' [bu]
       abd = array_append_record(au,bu, 0); all unique valuesl [d] difference
       delete( [/au, bu /] )
       if (atype.eq."string") then        ; must sort again cuz 'ab' may not be in order
           sqsort(abd)
       else
           qsort(abd)
       end if
       abd@tag = "NCL: venn2_difference; arrays are DISJOINT; no intersection"
       return(abd)
   end if
   nabi = dimsizes(abi)

   au   = get_unique_values(a)   ; 'a' [u]nique entries
   bu   = get_unique_values(b)   ; 'b' [u]nique entries
   nau  = dimsizes(au)
   nbu  = dimsizes(bu)
                                 ; reserve space
   abd  = new ( nau+nbu,  typeof(a), default_fillvalue(atype))
   ABu  = array_append_record(au,bu,0)    ; unique AB
   delete( [/au, bu /] )
   nABu = dimsizes(ABu)

   abd  = new ( nABu, typeof(a), default_fillvalue(atype))
   k    = -1
   do n=0,nABu-1
      if (.not.any(abi.eq.ABu(n))) then
          k = k+1
          abd(k) = ABu(n)
      end if
   end do

   if (atype.eq."string") then        ; must sort again cuz 'ab' may not be in order
       sqsort(abd(0:k))
   else
       qsort(abd(0:k))
   end if
   abd@tag = "NCL: venn2_difference has created the result"
   return(abd(0:k))
end
;------------------------------------------------------
undef("satvpr_water_bolton")
function satvpr_water_bolton(t:numeric, iounit[2]:integer)
;
; Saturation vapor pressure over liquid water given a temperature
; 
; Reference:
;   Bolton, David, 1980: 
;   The computation of equivalent potential temperature 
;   Monthly Weather Review, vol. 108, no. 7 (july),  p. 1047, eq.(10) 
;   http://dx.doi.org/10.1175/1520-0493(1980)108<1046:TCOEPT>2.0.CO;2
;
;   The quoted accuracy is 0.3% or better for -35 < t < 35c.

local tk0, es0, es, tdc
begin
  tk0  = 273.15
  es0  = 6.1121 
  warn = False

  if (iounit(0).eq.0) then                  ; input C
      es  = es0*exp(17.67*t/(t+243.5))
      if (any(abs(t).gt.35)) then
          warn = True
      end if
  else if (iounit(0).eq.1) then             ; input K
      tc  = t-tk0
      es  = es0*exp(17.67*tc/(tc+243.5))
      if (any(abs(tc).gt.35)) then
          warn = True
      end if
  else if (iounit(0).eq.2) then             ; input F
      tc  = 0.555556*(t-32)
      es  = es0*exp(17.67*tc/(tc+243.5))
      if (any(abs(tc).gt.35)) then
          warn = True
      end if
  end if   ; F
  end if   ; K
  end if   ; C

  es@long_name = "saturation vapor pressure over liquid water"

  if (iounit(1).eq.0) then
      es@units     = "hPa"   ; "mb"
  else if (iounit(1).eq.1) then
      es           = 100*es
      es@units     = "Pa"  
  else if (iounit(1).eq.2) then
      es           = 0.1*es
      es@units     = "kPa"   

  end if           ; hPa=>Pa
  end if           ; hPa=>kPa
  end if           ; hPa     

  es@NCL       = "satvpr_water_bolton"
  es@doi       = "http://dx.doi.org/10.1175/1520-0493(1980)108<1046:TCOEPT>2.0.CO;2"
  es@info      = "Equation 10 of reference"
  if (warn) then
      es@warning = "one or more temperatures outside of valid range: -35C to +35C"
  end if

  copy_VarCoords(t, es)
  return(es)
end
;------------------------------------------------------
undef("satvpr_water_stipanuk")
function satvpr_water_stipanuk(t:numeric, iounit[2]:integer)
;
; Saturation vapor pressure over liquid water given a temperature
; 
; Reference:  g.s. stipanuk     1973            original version.
;            "algorithms for generating a skew-t, log p diagram
;             and computing selected meteorological quantities"
;             atmospheric sciences laboratory
;             U.S. army electronics command
;             white sands missile range, new mexico 88002
;
local tk0, tk, p1, p2, c1, esat
begin
  tk0  = 273.15

  if (iounit(0).eq.0) then                  ; input C
      tk   = t + tk0
      p1   = 11.344-0.0303998*tk
      p2   = 3.49149-1302.8844/tk
      c1   = 23.832241-5.02808*log10(tk)
      es   = 10.^(c1-1.3816e-7*10.^p1+8.1328e-3*10.^p2-2949.076/tk)
  else if (iounit(0).eq.1) then             ; input K
      p1   = 11.344-0.0303998*t 
      p2   = 3.49149-1302.8844/t 
      c1   = 23.832241-5.02808*log10(t)
      es   = 10.^(c1-1.3816e-7*10.^p1+8.1328e-3*10.^p2-2949.076/t)
  else if (iounit(0).eq.2) then             ; input F
      tk  = 0.555556*(t-32) + tk0
      p1   = 11.344-0.0303998*tk
      p2   = 3.49149-1302.8844/tk
      c1   = 23.832241-5.02808*log10(tk)
      es   = 10.^(c1-1.3816e-7*10.^p1+8.1328e-3*10.^p2-2949.076/tk)
  end if   ; F
  end if   ; K
  end if   ; C

  es@long_name = "saturation vapor pressure over liquid water"

  if (iounit(1).eq.0) then
      es@units     = "hPa"   ; "mb"
  else if (iounit(1).eq.1) then
      es           = 100*es
      es@units     = "Pa"  
  else if (iounit(1).eq.2) then
      es           = 0.1*es
      es@units     = "kPa"   
  end if           ; hPa=>Pa
  end if           ; hPa=>kPa
  end if           ; hPa     

  es@NCL       = "satvpr_water_stipanuk"

  copy_VarCoords(t, es)
  return(es)
end
;------------------------------------------------------
undef("grad_latlon_cfd")
function grad_latlon_cfd(z:numeric, lat[*]:numeric, lon[*]:numeric, rCyclic[1]:logical, opt[1]:logical)
local dimz, rankz, nlat, mlon, rad, re, con, dlon, dx, cgx, cgy, gradLatLon, dlon_re, lon_re, lonx
begin
  dimz  = dimsizes(z)
  rankz = dimsizes(dimz)

  if (rankz.lt.2 .or. rankz.gt.4) then
      print("grad_latlon_cfd: illegal rank: rankz="+rankz)
      exit
  end if

  mlon  = dimz(rankz-1)  
  nlat  = dimz(rankz-2) 
;************************************************
; Miscellaneous
;************************************************
  rad   = 4.0*atan(1.0)/180.0
  re    = 6.37122e6  
  con   = re*rad                 ; one deg lat = 111198.8 meters

;************************************************
; Use 'center_finite_diff_n for meridional (Y) gradient
; .    the 'lat' may be unequally spaced (eg: gaussian latitudes)
;************************************************

  cgy  = center_finite_diff_n( z, lat, False ,0,rankz-2) 
  cgy  = cgy/con
  copy_VarCoords( z, cgy)                         ; add mets 
  cgy@long_name = "cfd: meridional gradient"
  cgy@units     = "?/m"

;************************************************
; Use cfd for zonal (X) gradients
; Generally, these are much smaller than the meridional (Y) gradients
; This assumes that the longitudes are equally spaced
; Pre-allocate space for gradients
;************************************************

  dlon_re = re*(lon(2)-lon(1))*rad    ; meters at 're' radius 
  lon_re  = ispan(0,mlon-1,1)*dlon_re ; lon(mlon)

  cgx  = new( dimz, typeof(z), getFillValue(z) )  ; lon=>X

  if (rankz.eq.2) then
      do nl=0,nlat-1                              ; loop over each latitude (y)
          lonx          = lon_re*cos(lat(nl)*rad) ; longitude spacing (x) function(latitude)
          cgx(nl,:)     = totype( center_finite_diff_n (z(nl,:)    , lonx, rCyclic,0,0), typeof(cgx))
      end do
  else if (rankz.eq.3) then
      do nl=0,nlat-1                              ; loop over each latitude (y)
          lonx          = lon_re*cos(lat(nl)*rad) ; longitude spacing (x) function(latitude)
          cgx(:,nl,:)   = totype( center_finite_diff_n (z(:,nl,:)  , lonx, rCyclic,0,1), typeof(cgx))
      end do
  else if (rankz.eq.4) then
      do nl=0,nlat-1                              ; loop over each latitude (y)
          lonx          = lon_re*cos(lat(nl)*rad) ; longitude spacing (x) function(latitude)
          cgx(:,:,nl,:) = totype( center_finite_diff_n (z(:,:,nl,:), lonx, rCyclic,0,2), typeof(cgx))
      end do
    end if      ; rankz=4
   end if       ; rankz=3
  end if        ; rankz=2

  copy_VarCoords( z, cgx)            
  cgx@long_name = "cfd: zonal gradient"
  cgx@units     = "?/m"

  gradLatLon = [/ cgy, cgx /]   ; return two variables as a type 'list'
  return( gradLatLon )
end
