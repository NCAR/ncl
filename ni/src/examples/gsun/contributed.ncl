;
;      $Id: contributed.ncl,v 1.28 2004-01-25 23:00:01 shea Exp $
;
; -------------- added Feb 14 2002
; copy_VarMeta
; cssgrid_Wrap
; epzZero
; flt2dble
; getVarDimNames
; numAsciiRow
; numAsciiCol
; month_to_season 
; month_to_seasonN
; month_to_season12
; wave_number_spc
; uv2dvG_Wrap
; uv2vrG_Wrap
; uv2dvF_Wrap
; uv2vrF_Wrap
; dv2uvG_Wrap
; vr2uvG_Wrap
; dv2uvF_Wrap
; vr2uvF_Wrap
; ilapsG_Wrap
; ilapsF_Wrap
; dim_standardize_Wrap
; msrcp_mss2local

; -------------- changed 13 May 2002
; eofMeta:  changed a dimension name from "eval" to "evn"
; -------------- added   13 May 2002
; eofcor_pcmsg_Wrap
; eofcov_pcmsg_Wrap

; -------------- changed June 2002
; short2flt:   added extra names
;
; -------------- replaced June 2002
; NewCosWeight
; SqrtCosWeight
;
; -------------- added July  3 2002
; GetFillColorIndex
; yyyymmdd2yyyyFrac
;
; -------------- bug fix 30 Aug 2002
; closest_val:   mult "closest" values Murphy
;
; -------------- added 13 Sept 2002
; grib_stime2itime:  convert initial_time (string) to time (integer)
; wgt_areaave_Wrap
;
; -------------- 19 Sept 2002
; NormCosWgtGlobe: make lat wgts sum to 2.0 like gaussian whts 
; namDimCheck    : makes sure all dimensions are named
; calcMonStandardizeAnomTLL : calculated standardized anomalies for each month
;
; --------------    Oct  2002
; cat2var: Concatenate 2 (or more) variables to create one variable
; byte2float
; svdHomHet2LatLon: convert SVD output arrays to lat/lon for plotting
; svdAkBk2time    : convert SVD output attributes to timefor plotting
; trimBlankRight  ; trim trailing (rightmost) blanks from strings
;
; --------------    Nov  2002
; All routines had local variables explicitly declared.
; All 'undef' statements were activated.
; Numerous regridding routines had changes made to them. [lat/lon stuff]
;
; -------------     Feb-Mar 2003
; added clmMonTLLL, stdMonTLLL
; namelist
; merge_VarAtts
; timeCoads2YYYYMM
; -------------   
; Now using CVS .... no longer will I manually maintain the change log
; -------------
;
; Contributed.ncl
;
; The codes in this script were contributed by various NCL'ers.  They are
; designed to be completely self contained so that if a user desires, the
; code can be extracted without loss of functionality.
;

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
; Both variables must be the same size, and have the exact same 
; coordinate variables.

undef("copy_VarCoords")
procedure copy_VarCoords(var_from,var_to)  
local rfrom, rto, i
begin                                     
  rfrom = dimsizes(dimsizes(var_from))
  rto   = dimsizes(dimsizes(var_to))

  if((rto.eq.rfrom).and.(all(dimsizes(var_from).eq.dimsizes(var_to))))
      do i = 0,rfrom-1
         if (.not.ismissing(var_from!i)) then
            var_to!i = var_from!i
            if(iscoord(var_from,var_from!i))
	       var_to&$var_to!i$ = var_from&$var_from!i$
            end if
         end if
      end  do
  else
      print("ERROR: copy_VarCoords: number of dimensions do not match")
  end if
end
;************************************************************
; D. Shea
; Copy the coordinate variables from one variable to another,
; except for last dimension.  
; Used internally

undef ("copy_VarCoords_1")
procedure copy_VarCoords_1(var_from,var_to)  
local dimt, dimf, rfrom, rto, i
begin                      
        dimf  = dimsizes(var_from)            
        dimt  = dimsizes(var_to)
        rfrom = dimsizes(dimf)      ; rank of var_from
        rto   = dimsizes(dimt)      ; rank of var_to
       ;if (rto.eq.(rfrom-1)) then
                do i = 0,rfrom-2            ; do not use last dimension
                   if (.not.ismissing(var_from!i).and.dimf(i).eq.dimt(i))then
                        var_to!i = var_from!i
			if(iscoord(var_from,var_from!i))
				var_to&$var_to!i$ = var_from&$var_from!i$
			end if
                   end if
                end  do
       ;else
       ;        print("ERROR: copy_VarCoords_1: rank problem")
       ;end if
end
; *****************************************************************
; D. Shea
; Copy the coordinate variables from one variable to another,
; except for last two dimensions.  
; Used internally

undef ("copy_VarCoords_2")
procedure copy_VarCoords_2(var_from,var_to)  
local dimt, dimf, rfrom, rto, i
begin                      
        dimf  = dimsizes(var_from)            
        rfrom = dimsizes(dimf)      ; rank of var_from
        if (rfrom.gt.2) then
            dimt  = dimsizes(var_to)
            rto   = dimsizes(dimt)      ; rank of var_to
            do i = 0,rfrom-3            ; do not use last two dimensions
               if (.not.ismissing(var_from!i) .and. dimf(i).eq.dimt(i)  ) then
                    var_to!i = var_from!i
	            if(iscoord(var_from,var_from!i))
		       var_to&$var_to!i$ = var_from&$var_from!i$
                    end if
                end if
            end  do
        end if
end
;****************************************
; D. Shea
; Copies all of a variables attributes (e.g. long_name) from
; one variable to another

undef("copy_VarAtts")
procedure copy_VarAtts(var_from,var_to)    
local att_names, i
begin                                       
  att_names =getvaratts(var_from);
    if(.not.all(ismissing(att_names)))
      do i = 0,dimsizes(att_names)-1
         if (isatt(var_to,att_names(i))) then
             delete(var_to@$att_names(i)$)  ; var_from att may be diff size/type
         end if
	 var_to@$att_names(i)$ = var_from@$att_names(i)$
      end do
  end if
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

;undef("copyatt")
procedure copyatt(var_to,var_from) 
local n, att_names, i 
begin
  do n = 0, dimsizes(dimsizes(var_to))-1   
    var_to!n=var_from!n
    if(iscoord(var_from,var_from!n))then
      var_to&$var_to!n$ = var_from&$var_from!n$
    end if 
  end do
;
; copy variable attributes
;
  att_names = getvaratts(var_from);
  if(.not.all(ismissing(att_names)))
    do i = 0, dimsizes(att_names)-1
       if(isatt(var_from,"_FillValue"))then
          if(typeof(var_from@_FillValue).eq."double"  .and.\
             typeof(var_to).eq."float")then
	     var_to@_FillValue = doubletofloat(var_from@_FillValue)
          else
             if (isatt(var_to,att_names(i))) then  ; added 10 Mar 2003 [DJS]
                 delete(var_to@$att_names(i)$)  
             end if
             var_to@$att_names(i)$ = var_from@$att_names(i)$
          end if
        end if
    end do
  end if
  delete(att_names)
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

procedure component_copy_VarCoords (x, y)
local rankx, dimy, i
begin
  rankx = dimsizes(dimsizes(x))
  dimy  = dimsizes(y)

  y!0 = "component"
  y&component = ispan(0, dimy(0)-1, 1)

  do i=0,rankx-1
     if (.not.ismissing(x!i)) then
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
; Usage:   nrow = numAsciiRow ("/my/path/ascii_file") 

undef ("numAsciiRow")
function numAsciiRow (fNam:string)

local filString, nrow
begin
  filString = asciiread ( fNam, -1, "string")
  nrow      = dimsizes(filString)
  delete (filString)  ; not required but to be explicit

  return (nrow)
end

;***************************************************************
; D. Shea   
; Determine the number of columns in an ascii file
; This assumes that all rows have the same number of columns
;
; Usage:   ncol = numAsciiCol ("/my/path/ascii_file") 

undef ("numAsciiCol")
function numAsciiCol (fNam:string)

local nrow, filChr, ncol
begin
  nrow      = numAsciiRow (fNam)
  filChr    = asciiread ( fNam, -1, "character") 
  ncol      = dimsizes(filChr)/nrow - 1    ; ignore end of line
  delete (filChr)             ; not needed but to be explicit

  return (ncol)
end


;***************************************************************
; D. Shea   
; There is no NCL built-in function to get the dimension names
; of a variable in memory. There is one for file variables 
; [getfilevardims] but not for a variable in memory. Dunno why! 
; Usage:   dimNames = getVarDimNames (x)

undef ("getVarDimNames")
function getVarDimNames (x)

local dimx, rank, n, dimNames
begin
  dimx = dimsizes(x)
  rank = dimsizes(dimx)

  dimNames = new ( rank, "string", "missing")

  do n=0,rank-1
     if (.not.ismissing(x!n)) then
         dimNames(n) = x!n
     end if
  end do

  return (dimNames)
end

; *****************************************************************
; D. Shea
; Generate gaussian latitudes and meta data 
; nlat = 64
; lat  = latGau (nlat, "lat", "latitude", "degrees_north")
;
; set nlat@double = True if double precision is desired

undef ("latGau")
function latGau (nlat:integer, dimName:string, longName:string, units:string)
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
function latGauWgt(nlat:integer,dimName:string,longName:string,units:string)
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
 deg_to_rad = acos(-1.0)/180.0
 wgt  = lat
 wgt  = cos(lat*deg_to_rad)
 tsum = sum(wgt)
 nwgt = wgt                ; copy coordinates
 nwgt = (/2.*wgt/tsum/)    
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
function lonGlobeF(mlon:integer,dimName:string,longName:string,units:string)
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
function lonGlobeFo(mlon:integer,dimName:string,longName:string,units:string)
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

; input strings may be scalar [sample="An apple a day"]
;                   or 1D     [sample=new( 10, string) ]
;                              sample(0)="apple", (1)="ARTICHOKE", ...
; Usage:   sample = changeCaseChar (sample)  
;          sample = changeCaseChar ("apple")  ==> APPLE  
;          sample = changeCaseChar ("APPLE")  ==> apple  
;          sample = changeCaseChar ("ApplE")  ==> aPPLe  

undef ("changeCaseChar")
function changeCaseChar (x:string)

local low, up, xc, lowc, upc, dimxc, ndim, i, j 
begin
  low  = (/"a","b","c","d","e","f","g","h","i","j","k","l","m" \
          ,"n","o","p","q","r","s","t","u","v","w","x","y","z" /)
  up   = (/"A","B","C","D","E","F","G","H","I","J","K","L","M" \
          ,"N","O","P","Q","R","S","T","U","V","W","X","Y","Z" /)

  xc    = stringtochar(x)
  dimxc = dimsizes (xc)
  ndim  = dimsizes (dimxc)

  lowc = stringtochar (low) 
  upc  = stringtochar (up) 

  if (ndim.eq.1) then
      do i=0,dimxc-2         ; ignore end-of-string character
         if (any(lowc(:,0).eq.xc(i))) then
             xc(i) = upc(ind(lowc(:,0).eq.xc(i)),0)
         else
             if (any(upc(:,0).eq.xc(i))) then
                 xc(i) = lowc(ind(upc(:,0).eq.xc(i)),0)
             end if
         end if
      end do
  end if

  if (ndim.eq.2) then
      do i=0,dimxc(0)-1      ; loop thru each string
       do j=0,dimxc(1)-1     ; loop thry each char in string
          if (any(lowc(:,0).eq.xc(i,j))) then
              xc(i,j) = upc(ind(lowc(:,0).eq.xc(i,j)),0)
          else
              if (any(upc(:,0).eq.xc(i,j))) then
                  xc(i,j) = lowc(ind(upc(:,0).eq.xc(i,j)),0)
              end if
          end if
       end do
      end do
  end if

  return (chartostring(xc))
end
; *****************************************************************
; D.Shea
; Change case: (1) "low" [change all to lower case] 
;              (2) "up"  [change all to upper case]

; input strings may be scalar [sample="An apple a day"]
;                   or 1D     [sample=new( 10, string) ]
;                              sample(0)="apple", (1)="ARTICHOKE", ...
; Usage:   sample = changeCase (sample, "up")   ; all upper case  
;          sample = changeCase (sample, "low")  ; all lower case

undef ("changeCase")
function changeCase (x:string, opt:string)

local low, up, xc, dimxc, ndim, oldc, newc, i, j 
begin
  low  = (/"a","b","c","d","e","f","g","h","i","j","k","l","m" \
          ,"n","o","p","q","r","s","t","u","v","w","x","y","z" /)
  up   = (/"A","B","C","D","E","F","G","H","I","J","K","L","M" \
          ,"N","O","P","Q","R","S","T","U","V","W","X","Y","Z" /)

  xc   = stringtochar(x)
  dimxc= dimsizes (xc)
  ndim = dimsizes (dimxc)


  if (opt.eq."low") then
      oldc = stringtochar (up)
      newc = stringtochar (low)
  else     ; must be upper-to-lower
      oldc = stringtochar (low) 
      newc = stringtochar (up) 
  end if

  if (ndim.eq.1) then
      do i=0,dimxc-2          ; ignore end-of-string character
         if (any(oldc(:,0).eq.xc(i))) then
             xc(i) = newc(ind(oldc(:,0).eq.xc(i)),0)
         end if
      end do
  end if

  if (ndim.eq.2) then
      do i=0,dimxc(0)-1       ; loop thru each string
        do j=0,dimxc(1)-1     ; loop thry each char in string
           if (any(oldc(:,0).eq.xc(i,j))) then
               xc(i,j) = newc(ind(oldc(:,0).eq.xc(i,j)),0)
           end if
        end do
      end do
  end if

  return (chartostring(xc))
end
; *****************************************************
; D. Shea
; trim trailing (rightmost) blanks from one or more strings
;
undef("trimBlankRight")
function trimBlankRight (s[*]:string)
local nRow, str, cBlank, n, kChar
begin
  nRow = dimsizes(s)            ; # rows/strings
  str  = new (nRow, "string")   ; return string(s)

  cBlank = inttochar(32)        ; character blank
  
  do n=0,nRow-1
     chr   = stringtochar(s(n))
     kChar = dimsizes(chr)-2    ; ignore end-of-line
     if (kChar.ge.0) then
         do k=kChar,0,1
           ;print ("k="+k+"  chr(k)="+chr(k)+"$ c2i="+chartoint(chr(k)))
            if (chr(k).ne.cBlank) then
                str(n) = chartostring(chr(0:k))
                break
            end if
         end do
     end if
  end do
  return(str)
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
function latGlobeF(nlat:integer,dimName:string,longName:string,units:string)
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
function latGlobeFo(nlat:integer,dimName:string,longName:string,units:string)
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


; **************************************************************
; D. Shea
; Loops thru a 1D variable [eg, time] and finds
; the indices which match up with the values of the cvWant 1D array
; e.g. time_want = (/1948, 1957, 1964, 1965, 1989/)
; indWant   = get1Dindex (time, time_want)
; note that the values of cvWant must EXIST in cv

undef("get1Dindex")
function get1Dindex (cv[*]:numeric,  cvWant[*]:numeric)
local nWant, indWant, n
begin
  nWant   = dimsizes (cvWant)
  indWant = new (nWant, integer)

  do n=0,nWant-1
     indWant(n) = ind( cv.eq.cvWant(n) )
  end do

  return (indWant)        
end

; **************************************************************
; D. Shea
; Loops thru a 1D variable [eg, time] and finds
; the indices which match up with the values of the cvWant 1D array
; e.g. time_want = (/1948, 1957, 1964, 1965, 1989/)
; indWant   = get1Dindex_Collapse (time, time_want)
; This will eliminate any places where there was no match
; ie:  values of cvWant need not exist in cv

undef("get1Dindex_Collapse")

function get1Dindex_Collapse (cv[*]:numeric,cvWant[*]:numeric)
local nWant, indWant, n, k, indTemp
begin
  nWant   = dimsizes (cvWant)
  indWant = new (nWant, integer, -999)

  k  = -1
  do n=0,nWant-1
     indTemp = ind( cv.eq.cvWant(n) )
     if (.not.ismissing(indTemp) ) then
         k = k+1
         indWant(k) = indTemp
     end if
  end do

  if (k.ge.0) then
      return ( indWant(0:k) )        
  else
      return ( indWant )        
  end if
end


;****************************************************************
; S. Murphy
; Finds the index of the array point closest to the desired value
; e.g. var2ck = 18.382
; values in array2ck = 17.0 17.5 18.0 18.5
; this function would return 3.

undef("closest_val")
function closest_val(var2ck:numeric,array2ck[*])
local size, narray,loc, test1,test2
begin

; first check to see if the value exists in the array

  if(any(var2ck.eq.array2ck))then
    return(ind(var2ck.eq.array2ck))
    print(var2ck +"equals a value in the array")
    exit
  end if

; value does not exist in array, so we insert it

  size = dimsizes(array2ck)            ; get size of original array
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

;********************************************************************
; D. Shea
; set a _FillValue "automatically" 
; If a variable has a "_FillValue" use it, if not "missing_value" etc.
; if none on file, then set default by variable type 
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
           if (typeof(x).eq."float") then
               FillValue = -999.
           end if
           if (typeof(x).eq."double") then
               FillValue = -9999.
           end if
           if (typeof(x).eq."integer") then
               FillValue = -99
           end if
           if (typeof(x).eq."logical") then
               FillValue = -1
           end if
           if (typeof(x).eq."byte") then
               FillValue = 0377
           end if
           if (typeof(x).eq."short") then
               FillValue = -99
           end if
           if (typeof(x).eq."graphic") then
               FillValue = -9999
           end if
           if (typeof(x).eq."file") then
               FillValue = -9999
           end if
           if (typeof(x).eq."list") then
               FillValue = -9999
           end if
           if (typeof(x).eq."character") then
               FillValue = "\0"
           end if
           if (typeof(x).eq."string") then
               FillValue = "missing"
           end if
      end if
  end if

   return (FillValue)
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
 if (.not.isatt(xF,"_FillValue")     .and. \
     .not.isatt(xF,"missing_value")) then
     delete(xD@_FillValue)     ; no _FillValue or missing_value on input
 end if
 xD = xF  ; variable-to-variable transfer [all meta data copied]

 if (isatt(xD,"missing_value") .and. \
     typeof(xD@missing_value).ne."double") then
     delete(xD@missing_value)         
     xD@missing_value = xD@_FillValue   ; add missing_value
 end if

 return(xD)
end

undef("dble2flt")
; *************************************************************
; D. Shea
; Wrapper for NCL function: doubletofloat
; This does the conversion and copies all the attributes
; and coordinate variables [CV] and attributes of the CV.
; This is more complicated than usual beacuse I want
; to convert any double attributes and CVs to float.

undef("dble2flt")
function dble2flt (xD:double)
local dimx, ndimx, xF, xD_atts, i, j, cv, cvD, cvF, cvD_atts
begin
 dimx = dimsizes(xD)

 if (isatt(xD,"_FillValue")) then
   xF = new (dimx, float, doubletofloat(xD@_FillValue ) )
 else
   if (isatt(xD,"missing_value")) then
      xF = new (dimx(xD), float, doubletofloat(xD@missing_value) )
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
function short2flt (xS:short)
local xF, oNames, sNames, offset, scale, xAtts, nAtts, n
begin
;xF = new ( dimsizes(xS), float)
 xF = new ( dimsizes(xS), float, 1.e20)  ; 20 Nov 2002
 
 copy_VarAtts   (xS, xF)
 copy_VarCoords (xS, xF)
                                     ; added 19 Dec 2002
                                     ; the type stuff added Sept 2003
 if (isatt(xS,"missing_value") .and. .not.isatt(xS,"_FillValue")) then
     type_missing_value = typeof(xS@missing_value)
     if (type_missing_value.eq."short") then
         xS@_FillValue = xS@missing_value
     end if
     if (type_missing_value.eq."integer") then
         xS@_FillValue = integertoshort(xS@missing_value)
     end if
 end if

          ; should data be 'scaled' and/or 'offset' ?

          ; names to check
 oNames = (/"add_offset", "offset", "OFFSET", "Offset", "_offset" \
           ,"Intercept", "intercept"/)
 sNames = (/"scale", "SCALE", "Scale", "_scale", "scale_factor" \
           ,"Scale_factor", "Slope" , "slope" /)


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
function byte2flt (xB:byte)
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
           ,"Scale_factor", "Slope" , "slope" /)


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

 if (isatt(xB,"valid_range") .and. typeof(xB@valid_range).eq."short") then
     vrB = xB@valid_range
     vrF = new ( dimsizes(vrB), float)
     vrF = vrB*scale + offset 
     delete(xF@valid_range)    ; delete the "short" valid_range
     xF@valid_range = vrF      ; recreate with float
 end if

 return (xF)

end
;******************************************************************
; D. Shea
; converts shorts to floats using the "scale" and "offset" attributes (if present) 

; Note: Conventional HDF usage require that 
; if both scale_factor and add_offset attributes are present, 
; the following implementation should be used:
;      result = scale_factor*(stored_integer-add_offset)
; This is different than the COARDS and CF Conventions.
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
           ,"Intercept", "intercept"/)
 sNames = (/"scale", "SCALE", "Scale", "_scale", "scale_factor" \
           ,"Scale_factor", "Slope" , "slope" /)


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
           ,"Scale_factor", "Slope" , "slope" /)


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

 if (isatt(xB,"valid_range") .and. typeof(xB@valid_range).eq."short") then
     vrB = xB@valid_range
     vrF = new ( dimsizes(vrB), float)
     vrF = vrB*scale + offset 
     delete(xF@valid_range)    ; delete the "short" valid_range
     xF@valid_range = vrF      ; recreate with float
 end if

 return (xF)

end
; ********************************************************************
; D. Shea
; Truncate or Round "x" to the number of decimal places [nDec]
; This will [I hope] operate on any arbitrary array of float/double 
; numbers. However, originally it was designed to work on a scalar for
; plotting or printing purposes.

; ==> The NCL function "sprintf" has much of the same functionality
; ==> as this function. "decimalPlaces" is kept for backward
; ==> compatibility reasons.

; x     - float or double only
; nDec  - number of decimal places to the right of decimal point
; Round - round or truncate
;   
; usage: 
;   x is a scalar
;   xRound = decimalPlaces(x, 2, True)  ; x=12.345678 => 12.35
;   xRound = decimalPlaces(x, 2, False) ; x=12.345678 => 12.34
;   title  = "EOF: PerCent Variance="+decimalPlaces(pcvar(0), 2, True)


undef("decimalPlaces")
function decimalPlaces (x:numeric, nDec:integer, Round:logical)
local xType, shift, xTmp, iTmp
begin
  xType = typeof(x)
  if (xType.eq."float"  .or.  xType.eq."double") then
      shift = 10.^nDec                  ; type float
      if (Round) then
          xTmp = (x + 0.5/shift)*shift  ; xTmp same type as x
      else
          xTmp = x*shift
      end if

      if (xType.eq."float") then
          iTmp  = floattointeger(xTmp)  ; truncate temporary variable
      else
          iTmp  = doubletointeger(xTmp) ; truncate temporary variable
      end if

      xTmp  = iTmp/shift                ; return original type
      return (xTmp)
  else
      print ("decimalPlaces: input is of type "+xType )
      return (x)                        ; must not be float or double
  end if
end
; *******************************************************************
; D. Shea
; Compute a dimension average of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attribures and coordinate variables
; x    - multidimensional variable 


undef ("dim_avg_Wrap")
function dim_avg_Wrap (x:numeric)     
local xave, dimx, Ndx, Ndx1
begin

 xave = dim_avg(x)          ; arithmetic ave [no meta data]
 
 copy_VarAtts (x, xave)     ; copy attributes
                            ; copy dim  names and coord  variables
 dimx = dimsizes(x)         ; size of each dimension
 Ndx  = dimsizes(dimx)      ; number of dimensions
 copy_VarCoords_1 (x, xave) ; one less dimension 
                                       ; add an extra attribute
 Ndx1 = Ndx-1                          ; last dimension
 if (.not.ismissing(x!Ndx1)) then   
      xave@average_op_ncl= "dim_avg over dimension: "+x!Ndx1
 else
      xave@average_op_ncl= "dim_avg function was applied"
 end if
 
 return (xave)
end
; *******************************************************************
; D. Shea
; Compute a dimension variance of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attribures and coordinate variables
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
 if (.not.ismissing(x!Ndx1)) then   
      xvar@variance_op_ncl= "dim_variance over dimension: "+x!Ndx1
 else
      xvar@variance_op_ncl= "dim_variance function was applied"
 end if
 
 return (xvar)
end
; *******************************************************************
; D. Shea
; Compute a dimension standard dev of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attribures and coordinate variables
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
 if (.not.ismissing(x!Ndx1)) then   
      xstd@stddev_op_ncl= "dim_stddev over dimension: "+x!Ndx1
 else
      xstd@stddev_op_ncl= "dim_stddev function was applied"
 end if
 
 return (xstd)
end
; *******************************************************************
; D. Shea
; Compute a dimension sum of "x" [x may have many dimensions]
; return with one less dimension
; Copies over all the attribures and coordinate variables
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
 if (.not.ismissing(x!Ndx1)) then   
      xsum@sum_op_ncl= "dim_sum over dimension: "+x!Ndx1
 else
      xsum@sum_op_ncl= "dim_sum function was applied"
 end if
 
 return (xsum)
end
; *******************************************************************
; D. Shea
; Standardize by the st. dev of each rightmost dimension
; Copies over all the attribures and coordinate variables
; x    - multidimensional variable 


undef ("dim_standardize_Wrap")
function dim_standardize_Wrap (x:numeric, opt:integer)     
local xStd, dimx, Ndx
begin

 xStd = x
 xStd = dim_standardize(x, opt)     
 
 dimx = dimsizes(x)
 Ndx  = dimsizes(dimx)      ; number of dimensions
 if (.not.ismissing(x!(Ndx-1))) then   
      xStd@standardize_op_ncl= "dim_standardize over dimension: "+x!(Ndx-1)
 else
      xStd@standardize_op_ncl= "dim_standardize function was applied"
 end if
 
 return (xStd)
end

;**********************************************************
; D. Shea
; Compute average root-mean-square-difference between "x" and "y" 
; return with one less dimension
; Copies over all the attribures and coordinate variables
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
         xrmsd@long_name = "RMSD: "+x@long_name
     end if
                                ; copy dim  names and coord  variables
     Ndx  = dimsizes(dimx)      ; number of dimensions
     copy_VarCoords_1 (x, xrmsd); one less dimension 
                                           ; add an extra attribute
     Ndx1 = Ndx-1                          ; last dimension
     if (.not.ismissing(x!Ndx1)) then   
          xrmsd@rmsd_op_ncl= "dim_rmsd applied to dimension: "+x!Ndx1
     else
          xrmsd@rmsd_op_ncl= "dim_rmsd function was applied "+Ndx1
     end if

     return (xrmsd)
 else
     print ("------------------------------------------------------")
     print ("---> dim_rmsd_Wrap: error: x,y dimension mismatch <---")
     print ("------------------------------------------------------")
     exit
 end if
 
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
 answer = ilapsF (zlap,zlmda)
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
 if (isatt(xzon,"long_name")) then
     xzon@long_name  = "Zonal Ave ["+ xzon@long_name+"]" 
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
function smth9_Wrap(var:float, p[1]:float, q[1]:float, cyclic:logical)
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
procedure modCheck (name:string, N:integer, n:integer)
begin
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
procedure rankCheck (name:string, x, n:integer)
begin
  rank = dimsizes(dimsizes(x))
  if (rank.ne.n) then
      print("contributed.ncl: "+name+":: rank +n+ required: rank="+rank)
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
local dimx, ntim, i, nmos, monAveLLT
begin
  dimx = dimsizes(x)
  ntim = dimx(0)

  nmos  = 12
  modCheck ("clmMonTLL", ntim, nmos)   ; error check
 ;rankCheck("clmMonTLL", x, 3)         ; not needed due to prototyping
      
  do i=0,2
     if (ismissing(x!i)) then
         print("contributed.ncl: clmMonTLL: all dimensions must be named")
         exit
     end if
  end do

  monAveLLT = clmMonLLT( x($x!1$|:,$x!2$|:,$x!0$|:) )
  return ( monAveLLT(month|:,$x!1$|:,$x!2$|:) )   ; return (month,lat,lon)
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
local dimx, ntim, i, nmos, monStdLLT
begin
  dimx = dimsizes(x)
  ntim = dimx(0)

  nmos  = 12
  modCheck ("stdMonTLL", ntim, nmos)   ; error check
 ;rankCheck("stdMonTLL", x, 3)         ; not needed due to prototyping

  do i=0,2
     if (ismissing(x!i)) then
         print("contributed.ncl: stdMonTLL: all dimensions must be named")
         exit
     end if
  end do

  monStdLLT = stdMonLLT( x($x!1$|:,$x!2$|:,$x!0$|:) )
  return ( monStdLLT(month|:,$x!1$|:,$x!2$|:) )      ; return (month,lat,lon)
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
      xStiz(nmo:ntim-1:nmos,:,:) = dim_standardize(x(namd1|:,namd2|:,namd0|nmo:ntim-1:nmos), opt)
   end do
   if (isatt(x,"long_name")) then
       xStiz@long_name = "Standardized Anomalies: "+x@long_name
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
  xAnom@long_name  = x@long_name + ": Anomalies"
  xAnom@anomaly_op_ncl = "function rmAnnCyc1D "

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
local dimx, ntim, i, nmos, monAveLLLT
begin
  dimx = dimsizes(x)
  ntim = dimx(0)

  nmos  = 12
  modCheck ("clmMonTLLL", ntim, nmos)   ; error check
 ;rankCheck("clmMonTLLL", x, 4)         ; not needed due to prototyping
      
  do i=0,3
     if (ismissing(x!i)) then
         print("contributed.ncl: clmMonTLLL: all dimensions must be named")
         exit
     end if
  end do

  monAveLLLT = clmMonLLLT( x($x!1$|:,$x!2$|:,$x!3$|:,$x!0$|:) )
  return ( monAveLLLT(month|:,$x!1$|:,$x!2$|:,$x!3$|:) )   ; return (month,lev,lat,lon)
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
local dimx, ntim, i, nmos, monStdLLLT
begin
  dimx = dimsizes(x)
  ntim = dimx(0)

  nmos  = 12
  modCheck ("stdMonTLLL", ntim, nmos)   ; error check
 ;rankCheck("stdMonTLLL", x, 4)         ; not needed due to prototyping

  do i=0,3
     if (ismissing(x!i)) then
         print("contributed.ncl: stdMonTLLL: all dimensions must be named")
         exit
     end if
  end do

  monStdLLLT = stdMonLLLT( x($x!1$|:,$x!2$|:,$x!3$|:,$x!0$|:) )
  return ( monStdLLLT(month|:,$x!1$|:,$x!2$|:,$x!3$|:) ) ; return (month,lev,lat,lon)
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
function g2gsh_Wrap (x:numeric, newDims:integer, twave:numeric)
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

  xNew@gwt = gwt                                ; attach as attribute

  return (xNew)
end

;******************************************************************
; D. Shea
; wrapper for NCL function "g2fsh"  that copies attributes and coordinate vars.
; It adds the longitude and gaussian latitude coordinates.

undef("g2fsh_Wrap")
function g2fsh_Wrap (x:numeric, newDims:integer)
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
function f2gsh_Wrap (x:numeric, newDims:integer, twave:numeric)
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
  xNew@gwt = gwt                                ; attach gaussian weights

  return (xNew)
end



;********************************************************
; D. Shea
; wrapper for NCL function "f2fsh"  that copies attributes and coordinate vars.
; It adds the longitude and latitude coordinates.

undef("f2fsh_Wrap")
function f2fsh_Wrap (x:numeric, newDims:integer)
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

  uNew@gwt       = gwt                          ; attach gaussian weights
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

  uNew@gwt       = gwt                          ; attach gaussian weights
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
  nlat1= nlat-1                                ; fo has one less lat

  if (typeof(x).eq."double") then
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
  nlat1   = nlat+1                             ; f has one more lat

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
; **********************************************************************
; D. Shea
; wrapper for NCL procedure "linint1"  that copies attributes and coordinate 
; vars.  It adds the longitude and latitude coordinates.

undef("linint1_Wrap")
function linint1_Wrap (xi[*]:numeric, fi:numeric, wrapX:logical \
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
  if (.not.ismissing(xo!0)) then
      fo!nD = xo!0                              ; if present, use xo name
  else 
      if (.not.ismissing(fi!nD)) then
          fo!nD = changeCaseChar(fi!nD)         ; if present, use same name
      else                                      ; but change case [contributed]
          fo!nD = "X"                           ; default dimension name
      end if                
  end if
                                                ; assign coordinates
  fo&$fo!nD$ = xo                               ; rightmost dim
   
  return (fo)
end
; **********************************************************************
; D. Shea
; wrapper for NCL procedure "linint1"  that copies attributes and coordinate 
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
  copy_VarCoords_2 (fi, fo)                     ; copy coord variables  
                                                ; except for 2 rightmost

  fo!(nDim-2) = "Y"                             ; default named dimensions
  fo!(nDim-1) = "X"
                                                ; override if possible
  if (.not.ismissing(xo!0) .and. .not.ismissing(yo!0)) then
      fo!(nDim-2) = yo!0                        ; if present, use xo name
      fo!(nDim-1) = xo!0                        ; if present, use xo name
  else 
      do nD=nDim-2,nDim-1                       ; two rightmost dimensions
         if (.not.ismissing(fi!nD)) then
             fo!nD = changeCaseChar(fi!nD)      ; if present, use same name
         end if                                 ; but change case
      end do
  end if

  fo&$fo!(nDim-2)$ = yo                         ; create coordinate var 
  fo&$fo!(nDim-1)$ = xo                         ; two rightmost dimensions

  return (fo)
end

; **********************************************************************
; D. Shea
; wrapper for NCL function "linint2_points"  that copies attributes and coordinate 
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
  copy_VarCoords_2 (fi, fo)                     ; copy coord variables  
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

undef("GetFillColor")
function GetFillColor(cnlvls[*]:numeric,cmap[*][3]:numeric,data:numeric)
local ncn, nclr, color, n
begin

 ncn = dimsizes (cnlvls)
 nclr = dimsizes (cmap(:,0))
 color = new (3,"float",-1.0)

 if (nclr-2 .lt. ncn+1) then 
   print ("Not enough colors in colormap for number of contour levels")
   return (color)
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
   index = -999
   return (index)
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
procedure assignFillValue(var_from:numeric,var_to:numeric)
local value
begin

  value = getFillValue(var_from)

  var_to@_FillValue    = value
  var_to@missing_value = value

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
        x@long_name = x@long_name + " (sqrt cosine weighted)"
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
        x@long_name = x@long_name + " (cosine weighted)"
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
; D. Shea
; called internally by eofcov_Wrap, eofcor_Wrap, eof_pcmsg_Wrap, eof_pcmsg_Wrap 
; wrapper for NCL function "eofcov"  that copies coordiante variables

undef ("eofMeta")
  
procedure eofMeta (data:numeric, neval:integer, eof:numeric)
local evn, dimd, dime, nDimd, nDime, i
begin
  if (isatt(data,"long_name") ) then
      eof@long_name = "EOF: "+data@long_name
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

  do i=0,nDimd-2           ; do not use last dimension
     if (.not.ismissing(data!i)) then
         eof!(i+1) = data!i
         if (iscoord(data,data!i) ) then
	     eof&$eof!(i+1)$ = data&$data!i$
         end if
     end if
  end  do
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
; D. Shea
; called internally by eofcov_ts_Wrap and eofcor_ts_Wrap 
; wrapper for NCL functions "eofcov_ts" "eofcor_ts"  
; that copies coordiante variables
  
undef ("eofTsMeta")
procedure eofTsMeta (data:numeric, eof:numeric, eofTs:numeric)
local dimd, dime, nDimd, nDime, i, j
begin
  dimd = dimsizes(data)
  dime = dimsizes(eofTs)
  nDimd= dimsizes(dimsizes(data))               ; rank
  nDime= dimsizes(dimsizes(eofTs))                

  if (isatt(data,"long_name") ) then
      eofTs@long_name = "EOF: Amplitude: "+data@long_name
  end if

  if (.not.ismissing(eof!0)) then
      eofTs!0 = eof!0
      if (iscoord(eof,eof!0) ) then
          eofTs&$eofTs!0$ = eof&$eof!0$
      end if
  end if

  i   = nDimd-1            ; rightmost dimension of data
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
; This does a varimax rotation on the time series returned
; from eofcov_ts. This is called Q-mode varimax rotation.
;
;  eof   = eofcov (x,neval)
;  eofts = eofcov_ts (x,eof)  ; where 'deof' is the data used
;  eofts_varimax = varimax_eofts_Qmode(eofts, eof@eval, eof@pcvar )
;
;  This also returns the % variance explained by:
;       (a) each rotated component (attribute: Q_pcvar)
;       (b) each rotated component relative to the 
;           original variance (attribute: Q_pcvar_original)
;
undef ("varimax_eofts")
function varimax_eofts (eof_ts[*][*]:numeric \
                       ,eval[*]:numeric, pcvar[*]:numeric )

local neval, nev, Q_eof_ts, Q_eof_ts2, opt, tot_pcvar \
    , x, Q_tot_var, Q_pcvar

begin
   neval = dimsizes(eval)
   tot_pcvar = sum(pcvar)       ; total % variance of input modes

   Q_eof_ts = eof_ts            ; normalize the variance for each mode
   do nev=0,neval-1
      Q_eof_ts(nev,:) = eof_ts(nev,:)/sqrt(eval(nev))
   end do
                                        ; invoke built-in function
   Q_eof_ts = (/ eof_varimax(Q_eof_ts) /)
   Q_eof_ts@long_name = "Q-mode Varimax Rotation"

                                        ; get the % variance
   Q_eof_ts2  = Q_eof_ts
   Q_eof_ts2  = Q_eof_ts^2              ; square all values
   Q_mode_var = dim_variance(Q_eof_ts2) ; variance of each Q mode
   Q_tot_var  = sum(Q_mode_var)         ; sum ... should be neval
   Q_pcvar    = (dim_variance(Q_eof_ts2)/Q_tot_var)*100.  ; % each Q mode
   Q_pcvar_original = Q_pcvar*tot_pcvar*0.01  ; % of original variance explained
   delete (Q_eof_ts2)
                                ; reorder into ascending order
   Q_ts  = Q_eof_ts             ; save
   Q_pc  = Q_pcvar
   qsort(Q_pc)                  ; this sorts in ascending order

   indx = new ( neval, "integer")
   do n=0,neval-1
      indx(n) = ind(Q_pcvar.eq.Q_pc(n))
   end do
   indx = indx(::-1)            ; make descending order

   Q_pcvar  = Q_pcvar(indx)
   Q_pcvar_original = Q_pcvar_original(indx)
   do n=0,neval-1
      Q_eof_ts(n,:) = Q_ts(indx(n),:)
   end do

   Q_eof_ts@varimax_pcvar          = Q_pcvar     ; should sum to 100%
   Q_eof_ts@varimax_pcvar_original = Q_pcvar_original

   return  (Q_eof_ts)                ; return
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
;          eofts_varimax = varimax_eofts_Qmode(eofts, eof@eval, eof@pcvar )
;      and/or
;          eof_opt = True    ; currently not used
;          EOF_Q = eoftsData2eof(eofts_varimax, deof, False) 
;
; currently: the option argument is not used. 
;
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
  mlon2 = mlon/2

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
  if (tlon(0).eq.180. .or. tlon(0).eq.360.) then
      tlon = (/ tlon -360. /)
  end if
  copy_VarAtts   (xlon,tlon)
         
  temp&$lonName$ = tlon           ; new coord variable

  temp@lonPivot  = "reordered via lonPivot [NCL]: pivotLon="+pivotLon
  return (temp)
end

; ******************************************************************
; Dennis Shea

; wrapper for NCL functions "natgrds" and "natgrdd"
; Note: The natgrd[s/d] functions are two of the few
;       that require that all types be the same.
; Note: because the (xi,yi) and/or (xo,yo) are no
;       required to be monotonic, they may or may
;       nor be coordinate variables.

undef("natgrid_Wrap")
function natgrid_Wrap (xi[*]:numeric,yi[*]:numeric, fi:numeric \
                      ,xo[*]:numeric,yo[*]:numeric)

local fo, dimfi, nDim
begin
  fo = natgrid (xi,yi,fi, xo,yo)            ; perform interpolation 
                                             
  dimfi= dimsizes(fi)
  nDim = dimsizes(dimsizes(fi))                 ; rank: # of dimensions

  copy_VarAtts (fi, fo)                         ; copy variable attributes
  if (nDim.ge.2) then                           ; copy coord vars
      copy_VarCoords_1(fi,fo)                   ; except for rightmost dim
  end if

  return (fo)
end
; ******************************************************************
; Dennis Shea

; wrapper for NCL function runave

undef ("runave_Wrap")
function runave_Wrap( x:numeric, nave:integer, kopt[1]:integer)
local xRunAve
begin
  xRunAve = x
  xRunAve = runave (x, nave, kopt)   
  xRunAve@runave_op_ncl = "runave: nave="+nave 

  return(xRunAve)
end
; ******************************************************************
; Dennis Shea

; wrapper for NCL function wgt_runave

undef ("wgt_runave_Wrap")
function wgt_runave_Wrap( x:numeric, wgt[*]:numeric, kopt[1]:integer)
local wRunAve
begin
  wRunAve = x
  wRunAve = wgt_runave (x, wgt, kopt)   
  wRunAve@wgt_runave_op_ncl = "wgt_runave"

  return(wRunAve)
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

  copy_VarMeta (x, areaAve)          ; contributed.ncl (copy meta data)
  areaAve@wgt_areaave_op_ncl = "Area Average"

  return(areaAve)
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

undef ("fbinseqSwap1")
procedure fbinseqSwap1(in_file[1]:string, outfile[1]:string \
                      ,type[1]:string,  dims[*]:integer)
local indata, tot, n, sz, i, done, tmp
begin
	indata = cbinread(in_file,-1,"byte")
	tot = dimsizes(indata)
       ;print(tot)
	n = floattointeger(product(dims))
       ;print(n)
	if(type.eq."long")
		print("fbinswap: won't convert longs due to fact that " + \
                      "they can be different sizes on different machines, " + \
                      "use double for 64 bit longs and integer for 32 bit longs")
		return
	end if
	if(type.eq."short")
		sz = 2
	end if
	if(any(type.eq.(/"integer","float"/)))
		sz = 4
	end if
	if(type.eq."double")
		sz = 8
	end if
;
; Skip first control word
;
	i    = 4
	done = False

	do while(.not.done)
		tmp = onedtond(indata(i:i+n*sz-1),(/n,sz/))
	 	fbinrecwrite(outfile,-1,tmp(:,::-1))
;
; Skip control word after current record and before next
;
		i = i + 8 + n*sz
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

undef ("fbinseqSwap2")
procedure fbinseqSwap2(in_file[1]:string, outfile[1]:string, \
                       type[1]:string, nrec[1]:integer, dims[*]:integer)
local n, sz, i, indata, tmp
begin
	n = floattointeger(product(dims))
	if(type.eq."long")
		print("fbinswap: won't convert longs due to fact that " + \
                      "they can be different sizes on different machines, " + \
                      "use double for 64 bit longs and integer for 32 bit longs")
		return
	end if
	if(type.eq."short")
		sz = 2
	end if
	if(any(type.eq.(/"integer","float"/)))
		sz = 4
	end if
	if(type.eq."double")
		sz = 8
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
procedure fbindirSwap (infile:string, dims:integer \
                      ,inType:string, outFile:string)
                       
; procedure that reads byte reversed data 
; create a new output file which can then be read
; via fbindirread or cbinread

; Example:   fbindirSwap ("/shea/bigEnd", (/100,72,144/) \
;                        ,"float","/shea/littleEnd")
local nBytes, dimBytes, indata
begin
 ;print ("Start fbindirSwap: "+systemfunc("date"))

  nBytes = 4
  if (inType.eq."double") then
      nBytes  = 8
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
  end if
                                  ; is each dimension named?
  namedDim = new ( N, logical) 
  namedDim = True                 ; assume all dimensions are named
  do n=0,N1     
    if (ismissing(x!n)) then
        namedDim(n) = False
        x!n = "dim"+n             ; add temporary name 
    end if
  end do

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
     if( .not.namedDim(n)) then
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
  modCheck ("month_to_season12", ntim, nmos)

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
      NYRStrt = nyrs-2
  end if

  if (rank.eq.1) then
      xSea = new ( nyrs, typeof(xMon))
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
      xSea = new ( (/nyrs,nlat,mlon/), typeof(xMon))
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
      xSea = new ( (/nyrs,klev,nlat,mlon/), typeof(xMon))
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
  if (isatt(xMon, "long_name")) then
      xSea@long_name = SEASON+": "+xMon@long_name
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

      xSea@long_name = "seasonal means: "+xMon@long_name
      xSea@season    = season 

      return (xSea)
  end if

  if (rank.eq.3) then                ; (time,lat,lon)

     ;xSea = xMon(lat|:,lon|:,time|:)   ; transfer meta and reorder
      xSea = xMon($dNam(1)$|:,$dNam(2)$|:,$dNam(0)$|:) ; transfer meta and reorder
      xSea = runave (xSea ,3, 0 )              ; overwrite with seasonal means
      xSea(:,:,0)      = (xMon(0,:,:) + xMon(1,:,:) )*0.5
      xSea(:,:,ntim-1) = (xMon(ntim-2,:,:) + xMon(ntim-1,:,:) )*0.5

      xSea@long_name = "seasonal means: "+xMon@long_name
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

      xSea@long_name = "seasonal means: "+xMon@long_name
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
      xSeaN = new ( (/N,nyrs/),  typeof(xSea12))
      do ns =0,N-1
         NMO= ind(SEASON(ns).eq.season)
         if (.not.ismissing(NMO)) then
             xSeaN(ns,:) = (/ xSea12(NMO:ntim-1:nmos) /)
         end if
      end do
  end if

  if (rank.eq.3) then                ; (time,lat,lon)
      xSeaN = new ( (/N,nyrs,dimx(1),dimx(2)/),  typeof(xSea12))
      do ns =0,N-1
         NMO= ind(SEASON(ns).eq.season)
         if (.not.ismissing(NMO)) then
             xSeaN(ns,:,:,:) = (/ xSea12(NMO:ntim-1:nmos,:,:) /)
         end if
      end do
  end if
               
  if (rank.eq.4) then                ; (time,lev,lat,lon)
      xSeaN = new ( (/N,nyrs,dimx(1),dimx(2),dimx(3)/),  typeof(xSea12))
      do ns =0,N-1
         NMO= ind(SEASON(ns).eq.season)
         if (.not.ismissing(NMO)) then
             xSeaN(ns,:,:,:,:) = (/ xSea12(NMO:ntim-1:nmos,:,:,:) /)
         end if
      end do
  end if
                                     ; copy attributes
  copy_VarAtts (xMon, xSeaN)
  if (isatt(xMon, "long_name")) then
      xSeaN@long_name = "Seasonal Means: "+xMon@long_name
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
; given, say, 19790719  where 0719 is the 200th day of the year
;       yrFrac = yyyymmdd2yyyyFrac (19790719)
;       yrFrac = 1979.545     ( [200-1]/365.= 0.545..)
;
; yyyymmdd       - scalar or array of dates
; 
; assumes Gregorian calendar
undef ("yyyymmdd2yyyyFrac")
function yyyymmdd2yyyyFrac (yyyymmdd:integer)
local year, mmdd, mon, day, dayYear, nDay, yrFrac, n
begin
     year = yyyymmdd/10000
     mmdd = yyyymmdd-year*10000
     mon  = mmdd/100
     day  = mmdd-mon*100
     delete (mmdd)
     
     dayYear = day_of_year(year, mon, day)  ; Gregorian calendar
     delete (mon)
     delete (day)
     
     nDay   = dimsizes(yyyymmdd)
     yrFrac = new( nDay, "float")
     delete(yrFrac@_FillValue)
        
     do n=0,nDay-1
        if (isleapyear(year(n))) then
            yrFrac(n) = year(n) + (dayYear(n)-1)/366.
        else
            yrFrac(n) = year(n) + (dayYear(n)-1)/365.
         end if   
     end do
     
     yrFrac@long_name = "Time"
     yrFrac@units     = "YYYY + fractional portion of year"
     yrFrac@info      = "derived using: function yyyymmdd2yyyyFrac"
     
     return(yrFrac)
end
;*************************************************************************
; D. Shea
;      convert initial_time (type string) to integer
;
;      01/01/1997 (18:00)  <==> MM/DD/YYYY (HH:NN) {ignore NN}  
;      return will be integer of form YYYYMMDDHH
;
undef ("grib_stime2itime")
function grib_stime2itime(stime[*]:string)
local N, time, i, tmp_c
begin

        N      = dimsizes(stime)
        time   = new( N ,integer,-99)
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

undef ("timeCoads2YYYYMM")
function timeCoads2YYYYMM (yrStrt:integer, yrLast:integer, TYPE:string)
; usage
;    yrLast = 1997   ; last year on this COADS file
;    yyyymm = timeCoads2YYYYMM (1800,yrLast,typeof(time))
local nmos, nyrs, ntim, time, n
begin
     nmos = 12
     nyrs = yrLast-yrStrt+1
     ntim = nmos*nyrs

     time = new ( ntim, TYPE)
     delete(time@_FillValue)
     n = 0
     do yr=yrStrt,yrLast
        time(n:n+nmos-1) = yr*100 + ispan(1,nmos,1)
        n = n+nmos
     end do

     time@long_name = "time"
     time@units     = "yyyymm"
       
     time!0    = "time"
     time&time = time

     return (time)
end
;*************************************************************************
; D. Shea
; Read simple f90 namelist file as indicated by Ben Foster
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
; Read simple f90 namelist file as indicated by Ben Foster
; An integer variable is returned. Upon return it will have 
; associated with it attributes that were created from the
; input namelist file.
;
; create symmetric min/max values for plots
; will add additional plot resources to the "res" variable
; 
; usage:    res = True
;          symMinMaxPlt(zData, 14, False, res)
;
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
  res@cnLevelSpacingF      = mnmxint(2)
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

local N, sChr, sSub, n
begin
  N   = dimsizes(s)
  if (N.eq.1) then
      sChr = stringtochar(s)
      return( chartostring((/sChr(iStrt:iLast)/)) ) 
  else
      sSub = new ( N, "string")
      delete(sSub@_FillValue)
      do n=0,N-1
         sChr = stringtochar(s(n))
         sSub(n) = stringtochar( (/sChr(iStrt:iLast)/) )
         delete(sChr)   ; size may change
      end do
      return (sSub)
  end if
end

undef ("wallClockElapseTime")
procedure wallClockElapseTime(wcStrt:string, title:string, opt:integer)
;
; compute *Wall Clock* elapsed time in seconds
; Usage:     wcStrt = systemfunc("date")
;                   :    ; one or more statements [usually a block of code]
;            wallClockElapseTime(wcStrt, "short_info", 0)
; opt not used right now
; this will not handle case where year or month changes
local wcNow, wcStrt_c, tSTRT, wcNow_c, tNOW, sec, NLINE
begin
  wcNow     = systemfunc("date")   ; current ("now") time
 
  wcStrt_c  = stringtochar(wcStrt)
  tSTRT     = stringtointeger((/wcStrt_c( 8: 9)/))*86400 \
            + stringtointeger((/wcStrt_c(11:12)/))*3600  \
            + stringtointeger((/wcStrt_c(14:15)/))*60    \
            + stringtointeger((/wcStrt_c(17:18)/))
 
  wcNow_c   = stringtochar(wcNow)
  tNOW      = stringtointeger((/wcNow_c( 8: 9)/))*86400  \
            + stringtointeger((/wcNow_c(11:12)/))*3600   \
            + stringtointeger((/wcNow_c(14:15)/))*60     \
            + stringtointeger((/wcNow_c(17:18)/))

  secElapseTime = tNOW-tSTRT    

  NLINE     = inttochar(10)   ; new line character
  print (NLINE+ \
        "=====> Wall Clock Elapsed Time: "+title+": "+   \
         secElapseTime+" seconds <====="+NLINE)

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

undef ("yyyymmddhh2yyyyFrac")
function yyyymmddhh2yyyyFrac (yyyymmddhh[*]:integer)
;
; given, say, 1979071906  where 0719 is the 200th day of the year
;       yrFrac = yyyymmddhh2yyyyFrac (1979071906)
;       yrFrac = 1979.5458984375    ([200-1]*86400.+hh*3600.)/(86400*365)
;
; yyyymmddhh     - scalar or array of dates
;
; assumes Gregorian calendar
 
local year, mmddhh, mon, ddhh, day, hour, dayYear, nTim, yrFrac, n, ysec, dsec
begin
     year   = yyyymmddhh/1000000
     mmddhh = yyyymmddhh-year*1000000
     mon  = mmddhh/10000
     ddhh = mmddhh-mon*10000
     day  = ddhh/100
     hour = ddhh-day*100
     delete (mmddhh)
     delete (ddhh)


     dayYear = day_of_year(year, mon, day)  ; Gregorian calendar
     delete (mon)
     delete (day)

     nTim   = dimsizes(yyyymmddhh)
     yrFrac = new( nTim, "double")
     delete(yrFrac@_FillValue)

     do n=0,nTim-1
        if (isleapyear(year(n))) then
            ysec = 86400.*366.
        else
            ysec = 86400.*365.
         end if
         dsec = (dayYear(n)-1)*86400. + hour(n)*3600.
         yrFrac(n) = year(n) + dsec/ysec
     end do

     yrFrac@long_name = "Time"
     yrFrac@units     = "YYYY + fractional portion of year"
     yrFrac@NCL       = "contributed.ncl: function yyyymmddhh2yyyyFrac"

     return(yrFrac)
end

;*************************************************************************
; D. Shea
; read an ascii file and return just the header as a 1D array of strings.
;
; The header is the non-numeric part. Note upon
; return this may have to be parsed individually for specific info. 
;
; opt - option
;       if type "integer" then it is the number of rows (line)
;       for the header. This is a 'fixed' number of lines.
;
;       if type "string" then it is a sequence of characters
;       that terminate a variable number of header rows.
;       Currently this just checks character sequences
;       starting in col 0. 
;
;       if type "float" and positive then it will read  all values as float. 
;       eg: f_head = readAsciiHead("foo.ascii", 3.) 
;       all numbers on the 1st three rows will be read as float.

function readAsciiHead( fName:string, opt)
local xs, xc, nr, nrow, noptc, nxc, f_head
begin
  xs   = asciiread(fName, -1, "string") ; all rows as strings
 
  if (typeof(opt).eq."integer") then
      if (opt.gt.0) then
          return( xs(0:opt-1) )       ; return just header rows
      else
          print ("contributed: readAsciiHead: opt must be >0")
          return( "missing" )
      end if
  end if

  if (typeof(opt).eq."string") then
      nrow = dimsizes(xs)
      noptc = dimsizes(stringtochar(opt))-1
    ;;print ("nrow="+nrow+"  noptc="+noptc)
      
      do nr=0,nrow-1
         xc  = stringtochar(xs(nr))
         nxc = dimsizes(xc)
         if (nxc.ge.noptc .and. opt.eq.chartostring(xc(0:noptc))) then
             return( xs(0:nr) )
             exit
         end if
         delete (xc)
      end do
      return( "missing" )           ; return just header
  end if

  if (typeof(opt).eq."float") then
      nrow = floattointeger(opt)    ; # of rows to be read as float
      asciiwrite ("HeAdBoGuSfile", xs(0:nrow-1))
      f_head = asciiread("HeAdBoGuSfile", -1, "float")
      system("/bin/rm HeAdBoGuSfile")
      return(f_head)
  end if

end

;*************************************************************************
; D. Shea
; read an ascii file and return the table data after the header portion
; opt - option
;       if type "integer" then it is the number of rows (line)
;       for the header. This is a 'fixed' number of lines.
;
;       if type "string" then it is a sequence of characters
;       that terminate a variable number of header rows.
;       Currently this just checks character sequences
;       starting in col 0. 
; ncol- number of columns in table


function readAsciiTable( fName:string, ncol:integer, datatype:string, opt)
local head, nh, xs, nrow, table
begin
  head = readAsciiHead( fName, opt)
  nh   = dimsizes(head)
; ********************************************************
; read each data row: trick
;      (1) write strings as a tempoarary file;
;      (2) read temporary file back in as 2D 
; ********************************************************
  xs   = asciiread(fName, -1, "string") ; all rows as strings
  nrow = dimsizes(xs)-nh
  asciiwrite ("tempBOGUSfile", xs(nh:))
  table = asciiread("tempBOGUSfile", (/nrow,ncol/), datatype)
  system("/bin/rm tempBOGUSfile")
  return(table)
end

