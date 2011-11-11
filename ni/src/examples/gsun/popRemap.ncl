;
;      $Id: popRemap.ncl,v 1.15 2009-09-29 18:38:12 shea Exp $
;
;===========================================================
; D. Shea
; NCL function to perform selected grid-to-grid transformations
; **Within NCAR** the remapping files reside inseveral directories
; outside  firewall
;    CSM convention:    /fs/cgd/csm/mapping/maps/validated
;                       [replaces /fs/cgd/csm/mapping/maps 14 June 2004]
;    SCRIP convention:  /fs/cgd/data0/shea/pop
; behind firewall
;    one official master copy: /fis/cgd/cseg/csm/mapping/maps
;    unofficial:        /cgd/cas/shea/POP_REMAP/
; 
; July 1, 2004
; We can no longer automatically access certain directories.
; I [Dennis Shea] have decided to have the code look in the
; directories specified by the environment variables:
;     NCL_POP_REMAP    
; and
;     NCL_POP_REMAPV    [ contains file with roation angles]
; or th current directory 

; CCSM uses gx1v2

; Pls look at the file: /fs/cgd/csm/mapping/

;- gx1   : Greenland pole x1 (320x384) pop grid  [POPX1]
;- gx1v2 : Greenland pole x1 (320x384) pop grid, version 2 (only
;          different from gx1 in terms of land/ocn mask and kmt)
;- gx3   : Greenland pole x3 (100x116) pop grid  [POPX3]
;- hx1   : Hudson's Bay pole x1 (384x320) pop grid [pcm2 ?]
;- r05   : 1/2 degree lat/lon grid, for runoff model 
;- T21   : gaussian ccm grid
;- T31   : gaussian ccm grid
;- T42   : gaussian ccm grid
;- T62   : gaussian grid
;- T63   : gaussian ccm grid
;- T85   : gaussian ccm grid
;- T170  : gaussian ccm grid
;- 1x1d  : 1 degree lat/lon grid used by Levitus
;- 1.9x2.5 : finite volume

; --------------------------------------------------------------------
undef ("create_rmpPopFileName")
function create_rmpPopFileName (gridSrc:string, gridDst:string \
                               ,method:string, areaType:string \
                               ,Date:string)

; invoked internally by PopLatLon and PopLatLonV
; creates the name of the file containing the remap information
; also performs various consistency/error checks
; determies directory containing the file with the remap weights

; Map names will be of the form
;        map_[src grd]_to_[dst grd]_[method]_[type].nc
;     or
;        map_[src grd]_to_[dst grd]_[method]_[type]_Date.nc
; where [method] is bilin or aave and 
; [type] is da, for destarea, or fa, for fracarea.

local ier, rmpPOP, gridPOP, dirENV, dirPOP, indP
begin
  ier = 0
  if (.not.(areaType.eq."da" .or. areaType.eq."fa") ) then
      ier = ier + 1
      print ("create_rmpPopFileName: areaType="+areaType \
            +" Only da or fa recognized")
  end if
  if (.not.(method.eq."bilin" .or. method.eq."aave") ) then
      ier = ier + 1
      print ("create_rmpPopFileName: method="+method \
            +" Only bilin or aave recognized")
  end if
  if (ier.ne.0) then
      print ("create_rmpPopFileName: exit")
      exit
  end if
                                            ; concatenate
  if (Date.eq."" .or. Date.eq." ") then
      rmpPOP  = "map_"+gridSrc+"_to_"+gridDst \
                      +"_"+method+"_"+areaType+".nc"   
  else
      rmpPOP  = "map_"+gridSrc+"_to_"+gridDst \
                      +"_"+method+"_"+areaType+"_"+Date+".nc"   
  end if

 ;dirENV  = "/fis/cgd/cseg/csm/mapping/maps"

  dirENV  = getenv("NCL_POP_REMAP")
  if (ismissing(dirENV)) then
      dirPOP  = (/ "./" /)             ; local only
  else
      dirPOP  = (/ dirENV, "." /) + "/"
  end if

 ;print (dirPOP)

  do nd=0,dimsizes(dirPOP)-1
    ;print ("nd="+nd+"  dirPOP="+dirPOP(nd))
     gridPOP = systemfunc("cd "+dirPOP(nd)+" ; ls map_*")
    ;print (gridPOP)

     indP    = ind(gridPOP.eq.rmpPOP)
     if (.not.ismissing(indP)) then   ; ?supported transformation?
         print ("create_rmpPopFileName: "+dirPOP(nd)+rmpPOP)
         return (dirPOP(nd)+rmpPOP)
     end if
     delete (gridPOP)
  end do
                                                                     
  print ("create_rmpPopFileName: file not recognized/found: " +rmpPOP)
  print ("create_rmpPopFileName: dir searched: " +dirPOP)
  exit

end
; ----------------------------------------------------------
undef ("create_rmpPopFileName2")
function create_rmpPopFileName2(gridSrc:string, gridDst:string \
                               ,method:string, areaType:string )

; invoked internally by PopLatLon and PopLatLonV
; creates the name of the file containing the remap information
; also performs various consistency/error checks
; determies directory containing the file with the remap weights

; Map names will be of the form
;        map_[src grd]_to_[dst grd]_[method]_[type].nc
;     or
;        map_[src grd]_to_[dst grd]_[method]_[type]_Date.nc
; where [method] is bilin or aave and 
; [type] is da, for destarea, or fa, for fracarea.

local ier, rmpPOP, gridPOP, dirENV, dirPOP, indP
begin
  ier = 0
  if (.not.(areaType.eq."da" .or. areaType.eq."fa") ) then
      ier = ier + 1
      print ("create_rmpPopFileName2: areaType="+areaType \
            +" Only da or fa recognized")
  end if
  if (.not.(method.eq."bilin" .or. method.eq."aave") ) then
      ier = ier + 1
      print ("create_rmpPopFileName2: method="+method \
            +" Only bilin or aave recognized")
  end if
  if (ier.ne.0) then
      print ("create_rmpPopFileName2: exit")
      exit
  end if
                                            ; concatenate
  rmpPOP  = "map_"+gridSrc+"_to_"+gridDst \
                  +"_"+method+"_"+areaType+"_"+"*.nc"   

 ;dirENV  = "/fis/cgd/cseg/csm/mapping/maps"

  dirENV  = getenv("NCL_POP_REMAP")
  if (ismissing(dirENV)) then
      dirPOP  = (/ "./" /)             ; local only
  else
      dirPOP  = (/ dirENV, "." /) + "/"
  end if

 ;print (dirPOP)

  do nd=0,dimsizes(dirPOP)-1
    ;print ("nd="+nd+"  dirPOP="+dirPOP(nd))
     gridPOP = systemfunc("cd "+dirPOP(nd)+" ; ls "+rmpPOP)
    ;print (gridPOP)

     if (.not.ismissing(gridPOP)) then   ; ?supported transformation?
         print ("create_rmpPopFileName2: "+dirPOP(nd)+gridPOP)
         return (dirPOP(nd)+gridPOP)
     end if
     delete (gridPOP)
  end do
                                                                     
  print ("create_rmpPopFileName2: file not recognized/found: " +rmpPOP)
  print ("create_rmpPopFileName2: dir searched: " +dirPOP)
  exit

end
; ----------------------------------------------------------
undef ("create_rotPopFileName")
function create_rotPopFileName (gridSrc:string, gridDst:string) 

; invoked internally by PopLatLonV
; creates the name of the file containing the rotation angles
; all files containing the appropriate rotation angles begin with "rot_"

local dirENV, dirPOPV, rotPOPV, indRot, gridROT
begin

 ;print ("create_rotPopFileName: gridSrc="+gridSrc+"  gridDst="+gridDst)

                                        ; names with rot angles
  rotPOP  = (/ "POPX3","POP23" , "POP43", "gx1v2", "gx1v3" \
             , "gx1v4", "gx1v5", "gx1v6"                   \
             , "gx3v3", "gx3v4", "gx3v5", "gx3v6", "gx3v7" \
             , "gx3Uum", "gx3uum"  \
             , "gx3", "gx3p", "hx1", "crx3", "tx0.1v2" /)

  indRot  = ind(gridSrc.eq.rotPOP)
  if (ismissing(indRot)) then
      indRot = ind(gridDst.eq.rotPOP)   ; try other grid type
      if (ismissing(indRot)) then
          print ("create_rotPopFileName: file not recognized/supported: " \
                +gridSrc+" or "+gridDst)
          exit
      end if
  end if

 ;dirENV  = "/cgd/cas/shea/POP_REMAP"   ; dir with transform files

  dirENV  = getenv("NCL_POP_REMAPV")
  if (ismissing(dirENV)) then
      dirPOPV  = (/ "./" /)             ; local only
  else
      dirPOPV  = (/ dirENV, "." /) + "/"
  end if

  rotPOPV = "rot_"+gridSrc+".nc"

  do nd=0,dimsizes(dirPOPV)-1
     gridROT = systemfunc("cd "+dirPOPV(nd)+" ; ls rot_*")
     indP    = ind(gridROT.eq.rotPOPV)
    ;print ("nd="+nd+"  indP="+indP)
     if (.not.ismissing(indP)) then   ; ?supported transformation?
         print ("create_rotPopFileName: "+dirPOPV(nd)+rotPOPV)
         return (dirPOPV(nd)+rotPOPV)
     end if
     delete (gridROT)
  end do

  print ("create_rotPopFileName: file not recognized/found: " +rotPOPV)
  print ("create_rotPopFileName: dir searched: " +dirPOPV)
  exit
end
; -----------------------------------------------------
undef("copy_VA_pop")
procedure copy_VA_pop(var_from,var_to)    
; invoked internally
; Copies all attributes (e.g. long_name) from one variable to another
local att_names, i
begin                                       
  att_names =getvaratts(var_from);
    if(.not.all(ismissing(att_names)))
      do i = 0,dimsizes(att_names)-1
         var_to@$att_names(i)$ = var_from@$att_names(i)$
      end do
  end if
end

; -----------------------------------------------------
; internal function
; Copy the coordinate variables from one variable to another,
; except for last two dimensions.  
; The "iOffSet" is for the vector case where there is one 
; extra dimension.
undef ("copy_CV_2_pop")
procedure copy_CV_2_pop(var_from,var_to, iOffSet:integer)  
local dimt, dimf, rfrom, rto, i
begin                      
        dimf  = dimsizes(var_from)            
        rfrom = dimsizes(dimf)      ; rank of var_from
        if (rfrom.gt.2) then
            dimt  = dimsizes(var_to)
            rto   = dimsizes(dimt)      ; rank of var_to
            do i = 0,rfrom-3            ; do not use last two dimensions
               if (.not.ismissing(var_from!i) .and. dimf(i).eq.dimt(i)  ) then
                    var_to!(i+iOffSet) = var_from!i
                    if(iscoord(var_from,var_from!i))
                       var_to&$var_to!i$ = var_from&$var_from!i$
                    end if
                end if
            end  do
        end if
end
; -----------------------------------------------------
undef ("PopCoord")
procedure PopCoord (x:numeric, gDst:string 
                   ,latStart:numeric,lonStart:numeric)

; invoked internally by PopLatLon and PopLatLonV
; ------this could be cleaned up a bit----------

; AFTER PopLatLon or PopLatLonV has generated 
;       one of the supported "destination" [Dst]
;       lat/lon grids (HlfD, OneD, TwoD, 1x1d),
;       have this routine generate the named dimensions
;       and coordinates arrays for the two rightmost dimensions

local dimx, nDx, nlat, mlon, dlon, nadd, dlat, lat, lon
begin
  dimx = dimsizes (x)                    ; dimension sizes of x
  nDx  = dimsizes(dimx)                  ; # of dimensions 
  nlat = dimx(nDx-2)                     ; # latitudes  [for clarity]
  mlon = dimx(nDx-1)                     ; # longitudes [for clarity]
  dlon = 360./mlon
  nadd = nlat%2
  dlat = 180./(nlat-nadd)

  if (gDst.eq."T170".or. gDst.eq."T85" .or. \
      gDst.eq."T63" .or. gDst.eq."T62" .or. \
      gDst.eq."T42" .or. gDst.eq."T21" .or. \
      gDst.eq."TwoD" .or.gDst.eq."OneD".or. \
      gDst.eq."HlfD" .or.gDst.eq."1x1d".or. \
      gDst.eq."1x1125" .or. gDst.eq."2x25d".or. \
      gDst.eq."T31"  .or.gDst.eq."1.9x2.5" .or. \
      gDst.eq."fv0.9x1.25" .or. gDst.eq."fv1.9x2.5" .or. \
      gDst.eq."0.1x0.1") then

      if (gDst.eq."T170".or. gDst.eq."T85" .or. \
          gDst.eq."T63" .or. gDst.eq."T62" .or. \
          gDst.eq."T42" .or. gDst.eq."T21" .or. \
          gDst.eq."T31") then
          if (typeof(x).eq."double") then
              gau_info= gaus(nlat/2)             ; gaussian latitudes and wts
          else
              gau_info= doubletofloat(gaus(nlat/2)) 
          end if
          lat     = gau_info(:,0)                ; gaussian lats (1st dim of gau_info)
      end if

     ;print (gDst \
     ;      +"   nlat="+nlat+"   mlon="+mlon \
     ;      +"   latStart="+latStart+"   lonStart="+lonStart)

      if (lonStart.ge.358.) then                 ; kinda funny
          lonStart = lonStart - 360.             ; T63 and TwoD
      end if
                                                 ; SPECIAL OVERRIDE
      if (gDst.eq."OneD" .or. gDst.eq."1x1d") then
          latStart = -89.5
      end if
      if (gDst.eq."HlfD" ) then
          latStart = -89.75
      end if
      if (gDst.eq."0.1x0.1" ) then
          latStart = -89.95
      end if
      if (gDst.eq."2x25d" ) then
          latStart = 90.    
      end if
                                                ; END OVERRIDE
      if (gDst.eq."HlfD"   .or. gDst.eq."OneD" .or. \
          gDst.eq."TwoD"   .or. gDst.eq."1x1d" .or.  \
          gDst.eq."1x1125" .or. gDst.eq."0.1x0.1" .or. gDst.eq."2x25d") then
          lat =  latStart + fspan (0,(nlat-1)*dlat, nlat); generate latitudes
      end if

      lon  = lonStart + fspan (0,(mlon-1)*dlon, mlon); generate longitudes
     ;print(lon)
      if (gDst.eq."2x25d" ) then
          lat(0) = -89.45              ; stranger than fiction
          lat(nlat-1) = 89.45   
      end if

      if (isvar("lat")) then
          lat@long_name = "latitude"   
          lat@units     = "degrees_north"
      end if
      if (isvar("lon")) then
          lon@long_name = "longitude"   
          lon@units     = "degrees_east"
      end if
;                                    ; MAJOR kluge 3/16/2008, 3/2/2011
      if (gDst.eq."1.9x2.5" .or. gDst.eq."fv1.9x2.5" .or.  \  
          gDst.eq."0.9x1.25".or. gDst.eq."fv0.9x1.25") then 
          if (isvar("lat")) then
              delete(lat)                     ; DJS stuck this in 3/16/2008
          end if
          if (isvar("lon")) then
              delete(lon)
          end if
          if (gDst.eq."1.9x2.5" .or. gDst.eq."fv1.9x2.5") then
              nlat = 96
              mlon = 144
          end if
          if (gDst.eq."0.9x1.25".or. gDst.eq."fv0.9x1.25") then
              nlat = 192
              mlon = 288
          end if
          lat  = latGlobeF(nlat, "lat", "latitude", "degrees_north")
          lon  = lonGlobeF(mlon, "lon", "longitude", "degrees_east")
      end if

      x!(nDx-2) = "lat"                   ; name dimensions
      x!(nDx-1) = "lon"
      x&lat     = lat                     ; add coordinate arrays
      x&lon     = lon 

      if (lonStart.gt.0.) then
          lon@modulo_lon = 360.
      end if
  else                                   ; on POP grid
      Y         = ispan(1,nlat,1)        ; consistent with SCRIP
      Y@long_name = "Latitude index"
      Y!0       = "Y"
      Y&Y       = Y

      X         = ispan(1,mlon,1)        ; consistent with SCRIP
      X@long_name = "Longitude index"
      X!0       = "X"
      X&X       = X

      x!(nDx-2) = "Y"
      x!(nDx-1) = "X"
      x&Y       = Y                      ; attach index coord vars
      x&X       = X 
      if (isatt(x,"coordinate_caution")) then
          delete (x@coordinate_caution)
      end if
  end if

; get rid of 2D graphical "coordinate attributes"
; these should not be associated with the return variable
  if (isatt(x,"lat2d")) then
      delete(x@lat2d)
  end if
  if (isatt(x,"lon2d")) then
      delete(x@lon2d)
  end if

 ;x@coordinate_caution = "Coordinates are educated guesses. "+ \
 ;                       "No info available."
 ;return (x)
end
; ----------------------------------------------------------
undef ("PopLatLon2D")
function PopLatLon2D (x[*][*]:numeric, ny:integer, mx:integer \
                     ,map_wts[*][*]:numeric, dst_add[*]:numeric \
                     ,src_add[*]:numeric \
                     ,nlink:integer, nwt:integer)                  

; invoked internally by PopLatLon
; extra arguments are used when testing fortran .so code
; also, when testing .so type must be changed to float

local in_1D, out_1D
begin	
; note: pop_rmap needs input and output grids to be 1D 
  in_1D     = ndtooned ( x )                
  out_1D    = new (ny*mx, typeof(x), getFillValue(x) )

; f77 interface ; used for test
     ;POP::remap_pop(xNew_1D , map_wts, dst_add ,src_add, x_1D \ 
     ;              ,dimsizes(xNew_1D),nlink,nwt,dimsizes(x_1D)\ 
     ;              ,x_1D@_FillValue)

;      print ("========== PopLatLon2D ==========") 
;      print ("dimsizes(dst_add)="+dimsizes(dst_add))
;      print ("dimsizes(src_add)="+dimsizes(src_add))
;      print ("dimsizes(map_wts)="+dimsizes(map_wts))
;      print ("dimsizes(xNew_1D)="+dimsizes(xNew_1D))
;      print ("dimsizes(x_1D)="+dimsizes(x_1D))
;
;******************************************************
; conduct remapping and convert 1D array to full grid
;******************************************************
  pop_remap(out_1D,map_wts,dst_add,src_add,in_1D) ; NCL function

  return(onedtond(out_1D, (/ny,mx/)))    
end
; ----------------------------------------------------------
undef ("PopLatLon")
function PopLatLon (x:numeric, gridSrc:string, gridDst:string \
                             , method:string, areaType:string \
                             , Date:string)

; Regrid a variable from one grid to the other
;
; EG: xOut =  PopLatLon(x, "gx1v2" , "T62", "bilin", "da") 
; EG: xOut => xOut( 94,192)[lat,lon]
; EG: xPOP =  PopLatLon(xOut  , "T62", "gx3", "aave", "da" ) 
; EG: xPOP => xPOP( ny, nx)

local rmpFile, fP, dim_x, dim_src, dst_add\
    , src_ad, map_wts, dim_wts, nlink, nwt, dim_dst, mlon, nlat \
    , latStart, lonStart, x_atts, i, xmsg, n, nDx, rmpType
begin
  print (" ")
;*****************************************************************
; take user input for original and destination grid and determine
; what weight file should be used.
;*****************************************************************
  rmpFile = create_rmpPopFileName (gridSrc, gridDst, method, areaType, Date )
 ;print ("PopLatLon: rmpFile="+rmpFile)
  fP      = addfile(rmpFile,"r")
;*****************************************************************
; check to see if weight file for source and actual data source
; have the same dimensions.
;*****************************************************************
  dim_src = fP->src_grid_dims           ; size source grid 

  dim_x   = dimsizes( x )               ; consistency check
  nDx     = dimsizes( dim_x )           ; rank [# dimensions] of data

  if (dim_x(nDx-2).ne.dim_src(1) .or. dim_x(nDx-1).ne.dim_src(0)) then
      print ("PopLatLon: Input and Tranform do not match")
      print ("PopLatLon: -----Dimension mismatch-------")
      print (dim_src)
      print ("PopLatLon: dim_x(nDx-2)="+dim_x(nDx-2) )
      print ("PopLatLon: dim_x(nDx-1)="+dim_x(nDx-1) )
      print ("PopLatLon: dim_src(1)="+dim_src(1) )
      print ("PopLatLon: dim_src(0)="+dim_src(0) )
      exit
  end if
;*******************************************************
; determine which type of convention the weight file was
; written out in.
; for NCAR CSM models we want CSM, for PCM models we expect
; SCRIP
;*******************************************************
  if (isfilevar(fP, "row")) then
    convention = "CSM"
  else
    if (isfilevar(fP, "remap_matrix")) then
      convention = "SCRIP"
    else
      print ("PopLatLon: don't know which convention")
      print ("PopLatLon: weight file="+rmpFile)
      exit
    end if
  end if
;*********************************************************
  if (convention.eq."CSM") then
;     print ("PopLatLon: convention="+convention)
      dst_add = fP->row                 ; destination grid [linear] 
      src_add = fP->col                 ; source grid [linear] 
      s       = fP->S                   ; wts [fortran indexing] 
      dims    = dimsizes(s)             ; size of each dimension
      ranks   = dimsizes(dims)          ; rank [# dimensions]
;      printVarSummary (s)
;      print ("dimensions of weight file is ="+dims)
;      print ("ranks="+ranks)

;**********************************************************
; create local weights array. pop_remap requires weights to
; have two dimensions. If they come in as only 1D, then create
; a dummy dimension
;**********************************************************
      if (ranks.eq.1) then
;          print("PopLatLon: in_wts are 1D, making 2D")
;          map_wts = new ( (/ 1,dims(0)/), typeof(s))
;          map_wts(0,:) = s 

          map_wts = new ( (/ dims(0),1/), typeof(s))
          map_wts(:,0) = s 
      else
          map_wts = new ( (/dims(1)+1,dims(0)/), typeof(s))
          map_wts = s 
      end if
      delete (s)

      yc_b0 = fP->yc_b(0)
      xc_b0 = fP->xc_b(0)
      if (.not.isatt(yc_b0, "units")) then 
          latStart= doubletofloat( yc_b0 )*57.29578 
          lonStart= doubletofloat( xc_b0 )*57.29578
      else
          latStart= doubletofloat( yc_b0 )
          lonStart= doubletofloat( xc_b0 )
      end if
  else
;************************************************************
; SCRIP weight files
;************************************************************
;     print ("PopLatLon: convention=SCRIP")
      dst_add = fP->dst_address         ; destination grid [linear] 
      src_add = fP->src_address         ; source grid [linear] 
      map_wts = fP->remap_matrix        ; wts 

      latStart= doubletofloat( fP->dst_grid_center_lat(0) )*57.29578 
      lonStart= doubletofloat( fP->dst_grid_center_lon(0) )*57.29578
  end if
;     print ("PopLatLon: lonStart="+lonStart)
;     print ("PopLatLon: latStart="+latStart)

  dim_wts = dimsizes(map_wts)
  nlink   = dim_wts(0)
  nwt     = dim_wts(1)
;*********************************************************
; now deal with the destination grid
;*********************************************************
  dim_dst = fP->dst_grid_dims            ; size destination grid
  mlon    = dim_dst(0)                   ; eg: 192 for T63 
  nlat    = dim_dst(1)                   ; eg:  94 for T63
;*******************************************************
; Attach missing_value/_FillValue if necessary
;*******************************************************
  if (.not.(isatt(x,"missing_value") .and. isatt(x,"_FillValue")) ) then
      if (isatt(x,"missing_value") .and. .not.isatt(x,"_FillValue")) then
          x@_FillValue = x@missing_value             
      else
          if (isatt(x,"_FillValue") .and. .not.isatt(x,"missing_value")) then
              x@missing_value = x@_FillValue
          else
              xmsg = new ( 1, typeof(x) )
              xmsg = 1.e30                  ; create msg val
              x@missing_value = xmsg
              x@_FillValue    = xmsg
          end if
      end if
  end if

  xmsg = x@_FillValue
;*******************************************************
; Actual remapping is done here
;*******************************************************
  if (nDx.eq.2) then                                ; lat,lon grid
;      print("input grid is [lat,lon]")
;      print ("dimsizes(dst_add)="+dimsizes(dst_add))
;      print ("dimsizes(src_add)="+dimsizes(src_add))
;      print ("dimsizes(map_wts)=:")
;      print (dimsizes(map_wts))
      xOut = PopLatLon2D(x, nlat, mlon, map_wts, dst_add, src_add \
                          , nlink, nwt)  
  end if                              

  if (nDx.eq.3) then
      xOut = new ( (/dim_x(0),nlat,mlon/), typeof(x), getFillValue(x) )
      do n=0,dim_x(0)-1
         xOut(n,:,:) = PopLatLon2D(x(n,:,:), nlat, mlon, map_wts \
                                  ,dst_add, src_add, nlink, nwt)
      end do
                                 
      if (.not.ismissing(x!0)) then ; if coord present copy
          xOut!0 = x!0
          if (iscoord(x,x!0)) then
              xOut&$x!0$ = x&$x!0$
          end if
      end if
  end if                                 

  if (nDx.eq.4) then
      xOut = new ( (/dim_x(0), dim_x(1),nlat,mlon/), typeof(x), getFillValue(x) )
      do n=0,dim_x(0)-1
        do i=0,dim_x(1)-1
           xOut(n,i,:,:) = PopLatLon2D(x(n,i,:,:), nlat, mlon, map_wts \
                                      ,dst_add, src_add, nlink, nwt)
        end do
      end do
                                 
      do n=0,1
         if (.not.ismissing(x!n)) then ; if coord present copy
             xOut!n = x!n
             if (iscoord(x,x!n)) then
                 xOut&$x!n$ = x&$x!n$
             end if
         end if
      end do
  end if                                 
                                         
;*****************************************************************
; take care of attributes
;*****************************************************************

  copy_VA_pop(x,xOut)
  xOut@_FillValue    = xmsg              ; force common msg val
  if (isatt(xOut,"missing_value")) then
      delete(xOut@missing_value)         
  end if
  xOut@missing_value = xmsg
                              
  if (isatt(fP,"map_method")) then
      xOut@spatial_op = fP@map_method + ": 1st order" 
  else
      xOut@spatial_op = "Unknown remapping"
  end if

  if (isatt(fP,"normalization")) then
      xOut@spatial_op = xOut@spatial_op +": "+fP@normalization
  end if

  xOut@spatial_op = xOut@spatial_op +": NCL: "+rmpFile
;*************************************************
; add lat/lon [grid point] coord variables
;*************************************************
  PopCoord (xOut, gridDst, latStart, lonStart) 
;*************************************************
; output result
;*************************************************
  print (" ")
  return(xOut)  ; return as 2D array

end
;----------------------------------------------------------------------
undef ("PopLatLon2")
function PopLatLon2(x:numeric, gridSrc:string, gridDst:string \
                             , method:string, areaType:string )

; Regrid a variable from one grid to the other
;
; EG: xOut =  PopLatLon(x, "gx1v2" , "T62", "bilin", "da") 
; EG: xOut => xOut( 94,192)[lat,lon]
; EG: xPOP =  PopLatLon(xOut  , "T62", "gx3", "aave", "da" ) 
; EG: xPOP => xPOP( ny, nx)

local rmpFile, fP, dim_x, dim_src, dst_add\
    , src_ad, map_wts, dim_wts, nlink, nwt, dim_dst, mlon, nlat \
    , latStart, lonStart, x_atts, i, xmsg, n, nDx, rmpType
begin
  print (" ")
;*****************************************************************
; take user input for original and destination grid and determine
; what weight file should be used.
;*****************************************************************
  rmpFile = create_rmpPopFileName2(gridSrc, gridDst, method, areaType )
 ;print ("PopLatLon2: rmpFile="+rmpFile)
  fP      = addfile(rmpFile,"r")
;*****************************************************************
; check to see if weight file for source and actual data source
; have the same dimensions.
;*****************************************************************
  dim_src = fP->src_grid_dims           ; size source grid 

  dim_x   = dimsizes( x )               ; consistency check
  nDx     = dimsizes( dim_x )           ; rank [# dimensions] of data

  if (dim_x(nDx-2).ne.dim_src(1) .or. dim_x(nDx-1).ne.dim_src(0)) then
      print ("PopLatLon2: Input and Tranform do not match")
      print ("PopLatLon2: -----Dimension mismatch-------")
      print (dim_src)
      print ("PopLatLon2: dim_x(nDx-2)="+dim_x(nDx-2) )
      print ("PopLatLon2: dim_x(nDx-1)="+dim_x(nDx-1) )
      print ("PopLatLon2: dim_src(1)="+dim_src(1) )
      print ("PopLatLon2: dim_src(0)="+dim_src(0) )
      exit
  end if
;*******************************************************
; determine which type of convention the weight file was
; written out in.
; for NCAR CSM models we want CSM, for PCM models we expect
; SCRIP
;*******************************************************
  if (isfilevar(fP, "row")) then
    convention = "CSM"
  else
    if (isfilevar(fP, "remap_matrix")) then
      convention = "SCRIP"
    else
      print ("PopLatLon2: don't know which convention")
      print ("PopLatLon2: weight file="+rmpFile)
      exit
    end if
  end if
;*********************************************************
  if (convention.eq."CSM") then
;     print ("PopLatLon2: convention="+convention)
      dst_add = fP->row                 ; destination grid [linear] 
      src_add = fP->col                 ; source grid [linear] 
      s       = fP->S                   ; wts [fortran indexing] 
      dims    = dimsizes(s)             ; size of each dimension
      ranks   = dimsizes(dims)          ; rank [# dimensions]
;      printVarSummary (s)
;      print ("dimensions of weight file is ="+dims)
;      print ("ranks="+ranks)

;**********************************************************
; create local weights array. pop_remap requires weights to
; have two dimensions. If they come in as only 1D, then create
; a dummy dimension
;**********************************************************
      if (ranks.eq.1) then
;          print("PopLatLon2: in_wts are 1D, making 2D")
;          map_wts = new ( (/ 1,dims(0)/), typeof(s))
;          map_wts(0,:) = s 

          map_wts = new ( (/ dims(0),1/), typeof(s))
          map_wts(:,0) = s 
      else
          map_wts = new ( (/dims(1)+1,dims(0)/), typeof(s))
          map_wts = s 
      end if
      delete (s)

      yc_b0 = fP->yc_b(0)
      xc_b0 = fP->xc_b(0)
      if (.not.isatt(yc_b0, "units")) then 
          latStart= doubletofloat( yc_b0 )*57.29578 
          lonStart= doubletofloat( xc_b0 )*57.29578
      else
          latStart= doubletofloat( yc_b0 )
          lonStart= doubletofloat( xc_b0 )
      end if
  else
;************************************************************
; SCRIP weight files
;************************************************************
;     print ("PopLatLon2: convention=SCRIP")
      dst_add = fP->dst_address         ; destination grid [linear] 
      src_add = fP->src_address         ; source grid [linear] 
      map_wts = fP->remap_matrix        ; wts 

      latStart= doubletofloat( fP->dst_grid_center_lat(0) )*57.29578 
      lonStart= doubletofloat( fP->dst_grid_center_lon(0) )*57.29578
  end if
;     print ("PopLatLon2: lonStart="+lonStart)
;     print ("PopLatLon2: latStart="+latStart)

  dim_wts = dimsizes(map_wts)
  nlink   = dim_wts(0)
  nwt     = dim_wts(1)
;*********************************************************
; now deal with the destination grid
;*********************************************************
  dim_dst = fP->dst_grid_dims            ; size destination grid
  mlon    = dim_dst(0)                   ; eg: 192 for T63 
  nlat    = dim_dst(1)                   ; eg:  94 for T63
;*******************************************************
; Attach missing_value/_FillValue if necessary
;*******************************************************
  if (.not.(isatt(x,"missing_value") .and. isatt(x,"_FillValue")) ) then
      if (isatt(x,"missing_value") .and. .not.isatt(x,"_FillValue")) then
          x@_FillValue = x@missing_value             
      else
          if (isatt(x,"_FillValue") .and. .not.isatt(x,"missing_value")) then
              x@missing_value = x@_FillValue
          else
              xmsg = new ( 1, typeof(x) )
              xmsg = 1.e30                  ; create msg val
              x@missing_value = xmsg
              x@_FillValue    = xmsg
          end if
      end if
  end if

  xmsg = x@_FillValue
;*******************************************************
; Actual remapping is done here
;*******************************************************
  if (nDx.eq.2) then                                ; lat,lon grid
;      print("input grid is [lat,lon]")
;      print ("dimsizes(dst_add)="+dimsizes(dst_add))
;      print ("dimsizes(src_add)="+dimsizes(src_add))
;      print ("dimsizes(map_wts)=:")
;      print (dimsizes(map_wts))
      xOut = PopLatLon2D(x, nlat, mlon, map_wts, dst_add, src_add \
                          , nlink, nwt)  
  end if                              

  if (nDx.eq.3) then
      xOut = new ( (/dim_x(0),nlat,mlon/), typeof(x), getFillValue(x) )
      do n=0,dim_x(0)-1
         xOut(n,:,:) = PopLatLon2D(x(n,:,:), nlat, mlon, map_wts \
                                  ,dst_add, src_add, nlink, nwt)
      end do
                                 
      if (.not.ismissing(x!0)) then ; if coord present copy
          xOut!0 = x!0
          if (iscoord(x,x!0)) then
              xOut&$x!0$ = x&$x!0$
          end if
      end if
  end if                                 

  if (nDx.eq.4) then
      xOut = new ( (/dim_x(0), dim_x(1),nlat,mlon/), typeof(x), getFillValue(x) )
      do n=0,dim_x(0)-1
        do i=0,dim_x(1)-1
           xOut(n,i,:,:) = PopLatLon2D(x(n,i,:,:), nlat, mlon, map_wts \
                                      ,dst_add, src_add, nlink, nwt)
        end do
      end do
                                 
      do n=0,1
         if (.not.ismissing(x!n)) then ; if coord present copy
             xOut!n = x!n
             if (iscoord(x,x!n)) then
                 xOut&$x!n$ = x&$x!n$
             end if
         end if
      end do
  end if                                 
                                         
;*****************************************************************
; take care of attributes
;*****************************************************************

  copy_VA_pop(x,xOut)
  xOut@_FillValue    = xmsg              ; force common msg val
  if (isatt(xOut,"missing_value")) then
      delete(xOut@missing_value)         
  end if
  xOut@missing_value = xmsg
                              
  if (isatt(fP,"map_method")) then
      xOut@spatial_op = fP@map_method + ": 1st order" 
  else
      xOut@spatial_op = "Unknown remapping"
  end if

  if (isatt(fP,"normalization")) then
      xOut@spatial_op = xOut@spatial_op +": "+fP@normalization
  end if

  xOut@spatial_op = xOut@spatial_op +": NCL: "+rmpFile
;*************************************************
; add lat/lon [grid point] coord variables
;*************************************************
  PopCoord (xOut, gridDst, latStart, lonStart) 
;*************************************************
; output result
;*************************************************
  print (" ")
  return(xOut)  ; return as 2D array

end
;----------------------------------------------------------------------
undef("preRotate_PopLatLon2D")
function preRotate_PopLatLon2D \
                     (u[*][*]:numeric, v[*][*]:numeric , rot[*]:numeric \
                     ,ny:integer, mx:integer ,map_wts[*][*]:numeric     \
                     ,dst_add[*]:numeric, src_add[*]:numeric, uvmsg:numeric) 

; invoked internally by PopLatLonV

local u_1D, v_1D, uWRK_1D, vWRK_1D, uNew_1D, vNew_1D
begin
 ;print("this grid is pre-rotated")

  u_1D    = ndtooned ( u )              ; required for remap
  v_1D    = ndtooned ( v )              
                                        ; rotate axis PRIOR to remap
  uWRK_1D = u_1D*cos(rot) - v_1D*sin(rot) ; still on POP grid
  vWRK_1D = u_1D*sin(rot) + v_1D*cos(rot)
  delete (u_1D)
  delete (v_1D)

  uNew_1D = new ( mx*ny, typeof(u) , uvmsg) ; preallocate for the
  vNew_1D = new ( mx*ny, typeof(v) , uvmsg) ; returned arrays
                                        ; 1st order remap
  pop_remap(uNew_1D, map_wts, dst_add ,src_add, uWRK_1D )
  pop_remap(vNew_1D, map_wts, dst_add ,src_add, vWRK_1D )
                                        ; create return array
  uvNew   = new ((/2,ny,mx/), typeof(u) , uvmsg)
  uvNew(0,:,:) = onedtond ( uNew_1D, (/ny,mx/)) ; now in lat/lon space
  uvNew(1,:,:) = onedtond ( vNew_1D, (/ny,mx/))

  return (uvNew)
end
;----------------------------------------------------------------------
undef("postRotate_PopLatLon2D")
function postRotate_PopLatLon2D \
                     (u[*][*]:numeric, v[*][*]:numeric , rot[*]:numeric \
                     ,ny:integer, mx:integer ,map_wts[*][*]:numeric     \
                     ,dst_add[*]:numeric, src_add[*]:numeric, uvmsg:numeric)  

begin

 ;print("this grid is post rotated")
  uWRK_1D = new ( mx*ny, typeof(u) , uvmsg)
  vWRK_1D = new ( mx*ny, typeof(v) , uvmsg)
                                        ;1st order remap
  pop_remap(uWRK_1D, map_wts, dst_add ,src_add, ndtooned (u)  )
  pop_remap(vWRK_1D, map_wts, dst_add ,src_add, ndtooned (v) )
                                        ; rotate axis AFTER remap

  uNew_1D =  uWRK_1D*cos(rot) + vWRK_1D*sin(rot)
  vNew_1D = -uWRK_1D*sin(rot) + vWRK_1D*cos(rot)

  delete (uWRK_1D)
  delete (vWRK_1D)
                                        
  uvNew   = new ((/2,ny,mx/), typeof(u) , uvmsg)
  uvNew(0,:,:) = onedtond ( uNew_1D, (/ny,mx/))
  uvNew(1,:,:) = onedtond ( vNew_1D, (/ny,mx/))

  return (uvNew)
end
; ----------------------------------------------------------
undef ("PopLatLonV")
function PopLatLonV (u:numeric , v:numeric          \
                    ,gridSrc:string, gridDst:string \
                    ,method:string, areaType:string \
                    ,Date:string)

; Regrid a vector from one grid to the other
; EG: xOut =  PopLatLonV(xPOP23, "gx1v2" , "T62", "bilin", "da", "")) 
; EG: xOut => xOut( 94,192)[lat,lon]
; EG: xPOP =  PopLatLonV(xOut  , "T62", "gx3", "aave", "da", "010808" ) 
; EG: xPOP => xPOP( ny, nx)

local rmpFile, rotFile, fP, gP, dim_u, dim_src, dst_add \
    , src_add, map_wts, dim_wts, nlink, nwt, dim_dst, mlon, nlat \
    , latStart, lonStart, u_1D, v_1D, gP, rot, gridChar, gridSrcTyp \
    , uWRK_1D, vWRK_1D, uNew_1D, vNew_1D, uvNew, u_atts, i

begin
  print (" ")
  rmpFile = create_rmpPopFileName (gridSrc, gridDst \
                                  ,method, areaType , Date )
  fP      = addfile(rmpFile,"r")

  dim_u   = dimsizes( u )               ; consistency check
  nDu     = dimsizes( dim_u )           ; rank [# dimensions]
  dim_src = fP->src_grid_dims            ; size source grid
  if (dim_u(nDu-2).ne.dim_src(1) .or. dim_u(nDu-1).ne.dim_src(0)) then
      print ("PopLatLonV: Input and Tranform do not match")
      print ("PopLatLonV: -----Dimension mismatch--------")
      print ("PopLatLonV: dim_x(nDx-2)="+dim_x(nDx-2) )
      print ("PopLatLonV: dim_x(nDx-1)="+dim_x(nDx-1) )
      print ("PopLatLonV: dim_src(1)="+dim_src(1) )
      print ("PopLatLonV: dim_src(0)="+dim_src(0) )
      exit
  end if


;*******************************************************
; determine which type of convention the weight file was
; written out in.
; for NCAR CSM models we want CSM, for PCM models we expect
; SCRIP
;*******************************************************
  if (isfilevar(fP, "row")) then
    convention = "CSM"
  else
    if (isfilevar(fP, "remap_matrix")) then
      convention = "SCRIP"
    else
      print ("PopLatLon: don't know which convention")
      print ("PopLatLon: weight file="+rmpFile)
      exit
    end if
  end if

  if (convention.eq."CSM") then
      dst_add = fP->row                 ; destination grid [linear] 
      src_add = fP->col                 ; source grid [linear] 
      s       = fP->S                   ; wts [fortran indexing] 
      dims    = dimsizes(s)             ; size of each dimension
      ranks   = dimsizes(dims)          ; rank [# dimensions]
      if (ranks.eq.1) then
          map_wts = new ( (/ dims(0),1/), typeof(s))
          map_wts(:,0) = s 
      else
          map_wts = new ( (/dims(1)+1,dims(0)/), typeof(s))
          map_wts = s 
      end if
      delete (s)

      yc_b0 = fP->yc_b(0)
      xc_b0 = fP->xc_b(0)
      if (.not.isatt(yc_b0, "units")) then 
          latStart= doubletofloat( yc_b0 )*57.29578 
          lonStart= doubletofloat( xc_b0 )*57.29578
      else
          latStart= doubletofloat( yc_b0 )
          lonStart= doubletofloat( xc_b0 )
      end if

  else ; for SCRIP convention

      dst_add = fP->dst_address         ; destination grid [linear] 
      src_add = fP->src_address         ; source grid [linear] 
      map_wts = fP->remap_matrix        ; wts 

      latStart= doubletofloat( fP->dst_grid_center_lat(0) )*57.29578 
      lonStart= doubletofloat( fP->dst_grid_center_lon(0) )*57.29578
      
  end if

  dim_wts = dimsizes(map_wts)
  nlink   = dim_wts(0)
  nwt     = dim_wts(1)

  dim_dst = fP->dst_grid_dims            ; size destination grid
  mlon    = dim_dst(0)                   ; eg: 192 for T63 
  nlat    = dim_dst(1)                   ; eg:  94 for T63

;********************************************************************
; read the correct rotation angles
;********************************************************************
  rotFile = create_rotPopFileName (gridSrc, gridDst )
 ;print (rotFile)

  gP      = addfile(rotFile,"r")
  rotaName = (/"ANGLE", "angle", "UTAN", "utan" \
               ,"ROTA", "rota"  , "ROTANG", "rotang" /)
  do i=0,dimsizes(rotaName)-1
     if (isfilevar(gP,rotaName(i))) then
         rot = ndtooned( gP->$rotaName(i)$)
         break                       ; exit the loop
     end if
  end do

  if (isatt(u,"missing_value") .and. .not. isatt(u,"_FillValue")) then
      uvmsg = u@missing_value
  else
      if (isatt(u,"_FillValue")) then
          uvmsg = u@_FillValue
      else
          uvmsg = new ( 1, typeof(u) )
          uvmsg = 1.e30                  ; create msg val
      end if
  end if
  if (.not.isatt(u,"_FillValue")) then   ; force common msg val
      u@_FillValue = uvmsg         ; NCL looks for _FillValue
      v@_FillValue = uvmsg         ; NCL looks for _FillValue
  end if

  gridChar = stringtochar (gridSrc)
  gridSrcType= gridSrc
;  gridSrcType= chartostring(gridChar(0:2)); ex: "POP

                                         ; REMAP
                                         ; uvNew will have extra dim

  types = (/"POP","gx3","gx3p","gx1v2","gx1v3", "gx1v4", "gx1v5", "gx1v6",\
            "gx3v3",  "gx3v4", "gx3v5", "gx3v6", "gx3v7", "crx3", "tx0.1v2"/)
 ;print(gridSrcType)
 ;print(types)
  if (nDu.eq.2) then
      uvNew = new ( (/2, nlat,mlon/), typeof(u), uvmsg )
      
      if (any(gridSrcType .eq. types)) then
          uvNew = preRotate_PopLatLon2D \
                       (u,v,rot,nlat, mlon, map_wts, dst_add, src_add, uvmsg)  
      else
          uvNew = postRotate_PopLatLon2D \
                       (u,v,rot,nlat, mlon, map_wts, dst_add, src_add, uvmsg)
 ;        printVarSummary(uvNew)  
      end if
  end if                    ; nDu=2  
        

  if (nDu.eq.3) then
      uvNew = new ( (/2, dim_u(0), nlat,mlon/), typeof(u), uvmsg )
      do n=0,dim_u(0)-1
         if (any(gridSrcType.eq.types)) then
             uvNew(:,n,:,:) = preRotate_PopLatLon2D \
                             (u(n,:,:),v(n,:,:),rot,nlat, mlon \
                             ,map_wts, dst_add, src_add, uvmsg)  
         else
             uvNew(:,n,:,:) = postRotate_PopLatLon2D \
                             (u(n,:,:),v(n,:,:),rot,nlat, mlon \
                             ,map_wts, dst_add, src_add, uvmsg)  
         end if
      end do
                                 
      if (.not.ismissing(u!0)) then ; if coord present copy
          uvNew!1 = u!0
          if (iscoord(u,u!0)) then
              uvNew&$u!0$ = u&$u!0$
          end if
      end if
  end if                    ; nDu=3

  if (nDu.eq.4) then
      uvNew = new ( (/2, dim_u(0), dim_u(1) ,nlat,mlon/), typeof(u), uvmsg )
      do n=0,dim_u(0)-1
        do i=0,dim_u(1)-1
           if (any(gridSrcType.eq.types)) then
               uvNew(:,n,i,:,:) = preRotate_PopLatLon2D \
                               (u(n,i,:,:),v(n,i,:,:),rot,nlat, mlon \
                               ,map_wts, dst_add, src_add, uvmsg)  
           else
               uvNew(:,n,i,:,:) = postRotate_PopLatLon2D \
                               (u(n,i,:,:),v(n,i,:,:),rot,nlat, mlon \
                               ,map_wts, dst_add, src_add, uvmsg)  
            end if
        end do
      end do
                                 
      do n=0,1
         if (.not.ismissing(u!n)) then ; if coord present copy
             uvNew!(n+1) = u!n
             if (iscoord(u,u!n)) then
                 uvNew&$u!n$ = u&$u!n$
             end if
         end if
      end do
  end if                     ; nDu=4                   

  uvNew!0 = "component"                   ; generic names
  uvNew&component = (/0,1/)               ; index coord dim (not needed)

  copy_VA_pop (u, uvNew)                  ; copy atttributes
  uvNew@_FillValue    = uvmsg
  if (isatt(uvNew,"missing_value")) then
      delete(uvNew@missing_value)         
  end if
  uvNew@missing_value = uvmsg

  uvNew@long_name = "velocity components" ; generic name
                              ; add info on operation performed
  if (isatt(fP,"map_method")) then
      uvNew@spatial_op = fP@map_method + "; 1st order" 
  else
      uvNew@spatial_op = "Unknown remapping"
  end if

  if (isatt(fP,"normalization")) then
      uvNew@spatial_op = uvNew@spatial_op +"; "+fP@normalization
  end if
  uvNew@spatial_op = uvNew@spatial_op +"; NCL; "+rmpFile

  PopCoord (uvNew, gridDst, latStart, lonStart) ; add lat/lon [grid point] coord variables

  print (" ")
  return( uvNew)  ; return as array with one additional dimension
end
;-------------------------------------------------------
undef ("PopLatLonV2")
function PopLatLonV2(u:numeric , v:numeric, rotin:numeric \
                    ,gridSrc:string, gridDst:string \
                    ,method:string, areaType:string )

; Regrid a vector from one grid to the other
; EG: xOut =  PopLatLonV(xPOP23, "gx1v2" , "T62", "bilin", "da", "")) 
; EG: xOut => xOut( 94,192)[lat,lon]
; EG: xPOP =  PopLatLonV(xOut  , "T62", "gx3", "aave", "da", "010808" ) 
; EG: xPOP => xPOP( ny, nx)

local rmpFile, fP, gP, dim_u, dim_src, dst_add \
    , src_add, map_wts, dim_wts, nlink, nwt, dim_dst, mlon, nlat \
    , latStart, lonStart, u_1D, v_1D, gP, rot, gridChar, gridSrcTyp \
    , uWRK_1D, vWRK_1D, uNew_1D, vNew_1D, uvNew, u_atts, i

begin
  print (" ")
  rmpFile = create_rmpPopFileName2(gridSrc, gridDst, method, areaType )
  fP      = addfile(rmpFile,"r")

  dim_u   = dimsizes( u )               ; consistency check
  nDu     = dimsizes( dim_u )           ; rank [# dimensions]
  dim_src = fP->src_grid_dims            ; size source grid
  if (dim_u(nDu-2).ne.dim_src(1) .or. dim_u(nDu-1).ne.dim_src(0)) then
      print ("PopLatLonV2: Input and Tranform do not match")
      print ("PopLatLonV2: -----Dimension mismatch--------")
      print ("PopLatLonV2: dim_x(nDx-2)="+dim_x(nDx-2) )
      print ("PopLatLonV2: dim_x(nDx-1)="+dim_x(nDx-1) )
      print ("PopLatLonV: dim_src(1)="+dim_src(1) )
      print ("PopLatLonV2: dim_src(0)="+dim_src(0) )
      exit
  end if


;*******************************************************
; determine which type of convention the weight file was
; written out in.
; for NCAR CSM models we want CSM, for PCM models we expect
; SCRIP
;*******************************************************
  if (isfilevar(fP, "row")) then
    convention = "CSM"
  else
    if (isfilevar(fP, "remap_matrix")) then
      convention = "SCRIP"
    else
      print ("PopLatLon2: don't know which convention")
      print ("PopLatLon2: weight file="+rmpFile)
      exit
    end if
  end if

  if (convention.eq."CSM") then
      dst_add = fP->row                 ; destination grid [linear] 
      src_add = fP->col                 ; source grid [linear] 
      s       = fP->S                   ; wts [fortran indexing] 
      dims    = dimsizes(s)             ; size of each dimension
      ranks   = dimsizes(dims)          ; rank [# dimensions]
      if (ranks.eq.1) then
          map_wts = new ( (/ dims(0),1/), typeof(s))
          map_wts(:,0) = s 
      else
          map_wts = new ( (/dims(1)+1,dims(0)/), typeof(s))
          map_wts = s 
      end if
      delete (s)

      yc_b0 = fP->yc_b(0)
      xc_b0 = fP->xc_b(0)
      if (.not.isatt(yc_b0, "units")) then 
          latStart= doubletofloat( yc_b0 )*57.29578 
          lonStart= doubletofloat( xc_b0 )*57.29578
      else
          latStart= doubletofloat( yc_b0 )
          lonStart= doubletofloat( xc_b0 )
      end if

  else ; for SCRIP convention

      dst_add = fP->dst_address         ; destination grid [linear] 
      src_add = fP->src_address         ; source grid [linear] 
      map_wts = fP->remap_matrix        ; wts 

      latStart= doubletofloat( fP->dst_grid_center_lat(0) )*57.29578 
      lonStart= doubletofloat( fP->dst_grid_center_lon(0) )*57.29578
      
  end if

  dim_wts = dimsizes(map_wts)
  nlink   = dim_wts(0)
  nwt     = dim_wts(1)

  dim_dst = fP->dst_grid_dims            ; size destination grid
  mlon    = dim_dst(0)                   ; eg: 192 for T63 
  nlat    = dim_dst(1)                   ; eg:  94 for T63

  rot = ndtooned(rotin)

  if (isatt(u,"missing_value") .and. .not. isatt(u,"_FillValue")) then
      uvmsg = u@missing_value
  else
      if (isatt(u,"_FillValue")) then
          uvmsg = u@_FillValue
      else
          uvmsg = new ( 1, typeof(u) )
          uvmsg = 1.e30                  ; create msg val
      end if
  end if
  if (.not.isatt(u,"_FillValue")) then   ; force common msg val
      u@_FillValue = uvmsg         ; NCL looks for _FillValue
      v@_FillValue = uvmsg         ; NCL looks for _FillValue
  end if

  gridChar = stringtochar (gridSrc)
  gridSrcType= gridSrc
;  gridSrcType= chartostring(gridChar(0:2)); ex: "POP

                                         ; REMAP
                                         ; uvNew will have extra dim

  types = (/"POP","gx3","gx3p","gx1v2","gx1v3", "gx1v4", "gx1v5", "gx1v6",\
            "gx3v3",  "gx3v4", "gx3v5", "gx3v6", "gx3v7", "crx3", "tx0.1v2"/)
 ;print(gridSrcType)
 ;print(types)
  if (nDu.eq.2) then
      uvNew = new ( (/2, nlat,mlon/), typeof(u), uvmsg )
      
      if (any(gridSrcType .eq. types)) then
          uvNew = preRotate_PopLatLon2D \
                       (u,v,rot,nlat, mlon, map_wts, dst_add, src_add, uvmsg)  
      else
          uvNew = postRotate_PopLatLon2D \
                       (u,v,rot,nlat, mlon, map_wts, dst_add, src_add, uvmsg)
 ;        printVarSummary(uvNew)  
      end if
  end if                    ; nDu=2  
        

  if (nDu.eq.3) then
      uvNew = new ( (/2, dim_u(0), nlat,mlon/), typeof(u), uvmsg )
      do n=0,dim_u(0)-1
         if (any(gridSrcType.eq.types)) then
             uvNew(:,n,:,:) = preRotate_PopLatLon2D \
                             (u(n,:,:),v(n,:,:),rot,nlat, mlon \
                             ,map_wts, dst_add, src_add, uvmsg)  
         else
             uvNew(:,n,:,:) = postRotate_PopLatLon2D \
                             (u(n,:,:),v(n,:,:),rot,nlat, mlon \
                             ,map_wts, dst_add, src_add, uvmsg)  
         end if
      end do
                                 
      if (.not.ismissing(u!0)) then ; if coord present copy
          uvNew!1 = u!0
          if (iscoord(u,u!0)) then
              uvNew&$u!0$ = u&$u!0$
          end if
      end if
  end if                    ; nDu=3

  if (nDu.eq.4) then
      uvNew = new ( (/2, dim_u(0), dim_u(1) ,nlat,mlon/), typeof(u), uvmsg )
      do n=0,dim_u(0)-1
        do i=0,dim_u(1)-1
           if (any(gridSrcType.eq.types)) then
               uvNew(:,n,i,:,:) = preRotate_PopLatLon2D \
                               (u(n,i,:,:),v(n,i,:,:),rot,nlat, mlon \
                               ,map_wts, dst_add, src_add, uvmsg)  
           else
               uvNew(:,n,i,:,:) = postRotate_PopLatLon2D \
                               (u(n,i,:,:),v(n,i,:,:),rot,nlat, mlon \
                               ,map_wts, dst_add, src_add, uvmsg)  
            end if
        end do
      end do
                                 
      do n=0,1
         if (.not.ismissing(u!n)) then ; if coord present copy
             uvNew!(n+1) = u!n
             if (iscoord(u,u!n)) then
                 uvNew&$u!n$ = u&$u!n$
             end if
         end if
      end do
  end if                     ; nDu=4                   

  uvNew!0 = "component"                   ; generic names
  uvNew&component = (/0,1/)               ; index coord dim (not needed)

  copy_VA_pop (u, uvNew)                  ; copy atttributes
  uvNew@_FillValue    = uvmsg
  if (isatt(uvNew,"missing_value")) then
      delete(uvNew@missing_value)         
  end if
  uvNew@missing_value = uvmsg

  uvNew@long_name = "velocity components" ; generic name
                              ; add info on operation performed
  if (isatt(fP,"map_method")) then
      uvNew@spatial_op = fP@map_method + "; 1st order" 
  else
      uvNew@spatial_op = "Unknown remapping"
  end if

  if (isatt(fP,"normalization")) then
      uvNew@spatial_op = uvNew@spatial_op +"; "+fP@normalization
  end if
  uvNew@spatial_op = uvNew@spatial_op +"; NCL; "+rmpFile

  PopCoord (uvNew, gridDst, latStart, lonStart) ; add lat/lon [grid point] coord variables

  print (" ")
  return( uvNew)  ; return as array with one additional dimension
end
;----------------------------------------------------------------------
undef("POPlonReorderGM")
function POPlonReorderGM (x:numeric)   

; force reorder so that the longitudes run from GM eastward 

local lon, ind360, ind0, indGM, nInd, indLast, n, nEnd, flag, temp, nDim
begin

  lon    = (/ x&lon /)          ; local lon
                                ; chk to see if reordering is necessary
  if (any(lon.ge.360) .or. any(lon.lt.0) ) then 
      temp = x                       ; variable-to-variable transfer
      nDim = dimsizes(dimsizes(x))   ; rank of x

      if (nDim.gt.5) then
          print ("popRemap: POPlonReorderGM: too many dimensions")
          exit
      end if

      if (any(lon.ge.360)) then
          flag    = 360
          ind360  = ind(lon.ge.360.) ; indices >= 360
          nInd    = dimsizes(ind360) ; # elements >= 360
          indGM   = ind360(0)        ; nominal GM (1st index)
          indLast = ind360(nInd-1)   ; last index
          nEnd    = indLast    
      else
          flag    = 0
          ind0    = ind(lon.lt.0) 
          nInd    = dimsizes(ind0)  ; # elements < 0
          indGM   = nInd            ; index nominal GM
          indLast = dimsizes(lon)-1 ; last index
          nEnd    = indGM-1
      end if
      n = indLast-indGM
          
      if (nDim.eq.2) then
          temp(:,0:n)          = (/ x(:,indGM:indLast) /)
          temp(:,n+1:nEnd)     = (/ x(:,0:indGM-1) /)
      end if
      
      if (nDim.eq.3) then
          temp(:,:,0:n)        = (/ x(:,:,indGM:indLast) /)
          temp(:,:,n+1:nEnd)   = (/ x(:,:,0:indGM-1) /)
      end if
      
      if (nDim.eq.4) then
          temp(:,:,:,0:n)      = (/ x(:,:,:,indGM:indLast) /)
          temp(:,:,:,n+1:nEnd) = (/ x(:,:,:,0:indGM-1) /)
      end if
      
      if (nDim.eq.5) then
          temp(:,:,:,:,0:n)      = (/ x(:,:,:,:,indGM:indLast) /)
          temp(:,:,:,:,n+1:nEnd) = (/ x(:,:,:,:,0:indGM-1) /)
      end if

      if (flag.eq.360) then
          temp&lon(0:n)        = (/ lon(indGM:indLast)-360. /)
          temp&lon(n+1:nEnd)   = (/ lon(0:indGM-1) /)
      else
          temp&lon(0:n)        = (/ lon(indGM:indLast)   /)
          temp&lon(n+1:nEnd)   = (/ lon(0:indGM-1)+360. /)
      end if

      return(temp)   ; return reordered array + coord var
  end if             ; "any" >=360  or  <0

  return (x)         ; nothing changed

end
;-------------------------------------------------------
undef("latLon2Pop_LinearIntS")
function latLon2Pop_LinearIntS (x:numeric, dst_type:string)   

; use bilinear interpolation to interpolate from a
; conventional lat/lon grid to a POP X1 or X3 grid.
; use only for scalars

; usage    assume "s" is (time,depth,lat,lon)
;   s_X1 = latLon2Pop_LinearIntS ( s, "POPX1")    ==> (time,depth,320,384)
;   t_X3 = latLon2Pop_LinearIntS ( temp, "POPX3") ==> (time,depth,116,100)

local dimx, nDim, lon, lat, f, lon_pop, lat_pop, ny, nx, \
      x_tmp, x_pop, i, x_atts, dirENV, dirPOP
begin
  dimx = dimsizes(x)
  nDim = dimsizes(dimx)

  lon  = x&lon              ; should be degrees
  lat  = x&lat
  if (isatt(lon,"units") .and. lon@units.ne."degrees_east") then
      if (lon@units.eq."radians") then
          lon = lon*57.29578
          lon@units = "degrees_east"
      else
           print ("popRemap: latLon2Pop_LinearIntS: lon must be degrees_east")
           exit
      end if
  end if
  if (isatt(lat,"units") .and. lat@units.ne."degrees_north") then
      if (lat@units.eq."radians") then
          lat = lat*57.29578
          lat@units = "degrees_north"
      else
          print ("popRemap: latLon2Pop_LinearIntS: lat must be degrees_north")
          exit
      end if
  end if


  dirENV  = getenv("NCL_POP_REMAP")
  if (ismissing(dirENV)) then
      dirPOP  = "./"             ; local only
  else
      dirPOP  = dirENV + "/"
  end if

; get two-dimensional lat and lon coordinates from a X1 or X3  file


  if (dst_type.eq."POPX1") then
     ;f = addfile ("/fs/cgd/data0/shea/pop/rmp_OneD_to_POPX1_C.nc","r")
      f = addfile (dirPOP+"rmp_OneD_to_POPX1_C.nc","r")
      lon_pop = f->dst_grid_center_lon    ; this is one-dim
      lat_pop = f->dst_grid_center_lat
      ny = 320
      nx = 384
  end if

  if (dst_type.eq."POPX3") then
     ;f = addfile ("/fs/cgd/data0/shea/pop/rmp_OneD_to_POPX3_B_CSM.nc","r")
      f = addfile (dirPOP+"rmp_OneD_to_POPX3_B_CSM.nc","r")
      lon_pop = f->xc_b    ; this is one-dim
      lat_pop = f->yc_b
      ny = 116
      nx = 100
  end if

  if (isatt(lon_pop,"units") .and. lon_pop@units.eq."radians") then
      lon_pop = lon_pop*57.29578
      lat_pop = lat_pop*57.29578
      lon_pop@units = "degrees_east"
      lat_pop@units = "degrees_north"
  end if

; perform the interpolation and reshape the grid

  x_tmp = linint2_points (lon,lat, x ,True, lon_pop, lat_pop, 0)
  dimx(nDim-2) = ny
  dimx(nDim-1) = nx
  x_pop = onedtond( x_tmp , dimx )
  delete (x_tmp)                    ; no longer needed

; if more than two dimensions copy other dimension related info

  if (nDim.gt.2) then
      do i=0,nDim-3                 ; do not use last two dimensions
         if (.not.ismissing(x!i) ) then
             x_pop!i = x!i
             if (iscoord(x,x!i))
                 x_pop&$x_pop!i$ = x&$x!i$
             end if
         end if
      end  do
  end if
                                          ; name the last two dimensions
  x_pop!(nDim-2) = "lati"                ; arbitrary
  x_pop!(nDim-1) = "loni"                ; dimension names

  lati = ispan(0,ny-1,1)
  lati@long_name = "latitude index"
  x_pop&lati = lati

  loni = ispan(0,nx-1,1)
  loni@long_name = "longitude index"
  x_pop&loni = loni

  x_atts = getvaratts(x)      ; copy attributes of input variable
  if (.not.all(ismissing(x_atts))) then
      do i=0,dimsizes(x_atts)-1
         x_pop@$x_atts(i)$ = x@$x_atts(i)$
      end do
  end if
  x_pop@info = "popRemap: latLon2Pop_LinearIntS used for interpolation"

  return (x_pop)
end
