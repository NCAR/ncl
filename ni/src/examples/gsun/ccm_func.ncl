;
;      $Id: ccm_func.ncl,v 1.3 2006-07-19 02:12:20 haley Exp $
;
; ccm_func.ncl
;
; this script contains ncl codes that are model specific.
; you MUST load /fs/cgd/data0/shea/contributed.ncl before you
; load this script.
;
;****************************************************
; S. Murphy
; returns local angular momentum  out[nlat,nlon] for the wgne diagnostics.
; True angular momentum is a full 3D integral and is available from the
; NCL function angmom_atm.  
;
; The value returned by this function is weighted 
; averaged, and then multiplied by 4*pi*radius_of_earth^2


; nomenclature:
; u       - u-velocity
; dp      - delta pressure (compute using  

undef("aam_local")
function aam_local(u[*][*][*][*]:numeric,dp[*][*][*][*],fin:file)

begin
;****************************
; parameters
;****************************
 g  = new (1, double)
 g  = 9.81                     ; m/s^2 
 a  = new (1, double)
 a  = 6.37122e+06              ; radius of earth in meters
 rad = new (1, double)
 rad = 4.0*atan(1.0)/180.  ; convert degrees to radians
;****************************
; integrate in the vertical
;****************************
 ru = u(time|:,lat|:,lon|:,lev|:)
 sum_vert = dim_sum(ru * dp(time|:,lat|:,lon|:,lev|:))
;****************************
; multiply by cos of lat
;****************************
 lat = fin->lat
 out = new( \
 (/filevardimsizes(fin,"time"),dimsizes(lat),filevardimsizes(fin,"lon")/),\
            double )

 do j = 0,dimsizes(lat)-1
    out(:,j,:) = sum_vert(:,j,:)*cos(lat(j)*rad)
 end do
 out=out*(a/g)
;****************************
; assign attributes
;****************************
 copyatt(out,ru)
 out@derive_op = "(a/g) cos(lat) sum_k U DP"
 out@units     = "kg/s"
 out@long_name  = "local atmos angular momentum"
;****************************
; determine type to return
;****************************
 if(typeof(u).eq."float")then
; we loose coordinate variables
; reassign in main program to avoid double to float issue with missing
; value transfer
    return(doubletofloat(out))         
 else
    return(out)
 end if
end
;****************************************************************************
; D. Shea
; This assumes that the input data are in COORDS, CSM  or CCMHT dimension order
; eg: COORDS: t(time,lev,lat,lon) if 4D  or t(lev,lat,lon) if 3D  
; eg: CCMHT : t(time,lat,lev,lon) if 4D  or t(lat,lev,lon) if 3D  
; usage:  theta = thetaHybrid (t,ps,P0,hyam,hybm,"COORDS") 
;         theta = thetaHybrid (t,ps,P0,hyam,hybm,"CSM") 
;         theta = thetaHybrid (t,ps,P0,hyam,hybm,"CCMHT") 
; nomenclature:
; t      - temperature on sigma levels. 
; ps     - sfc pressure (Pa). An array of size [nlat,mlon]
; P0     - base pressure(Pa)
; hyam   - hybrid coefs for base pressure 
; hybm   - hybrid coefs for sfc  pressure 

; history- 7  Aug 2000: added CSM string and "history" attribute
  
undef("thetaHybrid")
function thetaHybrid (t:numeric,ps:numeric, P0:numeric \
                     ,HYAM[*]:numeric, HYBM[*]:numeric, order:string)


begin

 klvl  = dimsizes (HYAM)  
 nDim  = dimsizes(dimsizes(t))  ; rank or number of dim for "t"

 pres  = t                      ; trick: create array to hold pres
                                ; pres = pressure on sigma levels

 if (typeof(t).eq."float" .and. typeof(HYAM).eq."double") then
     hyam = doubletofloat(HYAM)
     hybm = doubletofloat(HYBM)
 else
     hyam = HYAM
     hybm = HYBM
 end if

 if (order.eq."CSM" .or. order.eq."COORDS") then
     if (nDim.eq.4) then
         do k=0,klvl-1  
            pres(:,k,:,:)  = P0*hyam(k) + hybm(k)*ps
         end do
     else                       ; handle 3D case
         do k=0,klvl-1  
            pres(k,:,:)  = P0*hyam(k) + hybm(k)*ps
         end do
     end if
 else                           ; must be CCMHT
     if (nDim.eq.4) then
         do k=0,klvl-1  
            pres(:,:,k,:)  = P0*hyam(k) + hybm(k)*ps
         end do
     else                       ; handle 3D case
         do k=0,klvl-1  
            pres(:,k,:)  = P0*hyam(k) + hybm(k)*ps
         end do
     end if
 end if
                                ; not required but for clarity
 pres@long_name  = "Pressure on Hybrid Surface"
 pres@short_name = "Pres"
 pres@units      = "Pa"

 theta = t                      ; trick: create array to hold theta 
 rcp   = 0.286                  ; r/c_p  [=287/1004]
 theta = t*(P0/pres)^rcp

 theta@long_name  = "Potential Temperature"
 theta@short_name = "THETA"
 theta@units      = "K"
 theta@history    = "Derived using thetaHybrid [contributed.ncl]"

 return (theta)
end
;*********************************************************************
; Mark Stevens

; creates mask file for the pacific, atlantic and indian ocean basins
; oro: orography data array (lat,lon) 
; assumes that lat and lon are attached to oro as coordinates, 
; and oro has values ocean: 0, land: 1, seaice: 2
; used internally to all the transport functions in ccm_func.ncl

undef("ocean_mask")
function ocean_mask (oro[*][*]:numeric)
begin
 
 lat = oro&lat
 lon = oro&lon
 nlat = dimsizes(lat)
 nlon = dimsizes(lon)

; make 2D mask array for ocean grid points
 basins_mask = oro
 assignFillValue(basins_mask,basins_mask)   ; since oro has no _FillValue
 basins_mask = basins_mask@_FillValue 
 basins_mask@long_name = "(1)pacific (2)atlantic (3)indian"

; Pacific ocean basin
 do j = 0, nlat-1
   do i = 0, nlon-1
     if (oro(j,i).eq.0) then
       if ((lon(i).gt.100.0 .and. lon(i).lt.260.0 .and. \
          lat(j).lt. 65.0 .and. lat(j).gt. 15.0) .or. \
         (lon(i).gt.100.0 .and. lon(i).lt.275.0 .and. \
          lat(j).le. 15.0 .and. lat(j).gt. 10.0) .or. \
         (lon(i).gt.100.0 .and. lon(i).lt.290.0 .and. \
          lat(j).le. 10.0 .and. lat(j).gt. -5.0) .or. \
         (lon(i).ge.130.0 .and. lon(i).le.290.0 .and. \
          lat(j).le. -5.0)) then
         basins_mask(j,i) = 1       ; pacific
       end if
     end if
   end do
 end do

; Atlantic ocean basin 
 do j = 0, nlat-1
   do i = 0, nlon-1
     if (oro(j,i).eq.0) then
       if ((lon(i).gt.290.0 .and. lon(i).lt.360.0 .and. \
          lat(j).le. 65.0 .and. lat(j).gt. 45.0) .or. \
         (lon(i).ge.  0.0 .and. lon(i).lt. 10.0 .and. \
          lat(j).le. 65.0 .and. lat(j).gt. 45.0) .or. \
         (lon(i).gt.260.0 .and. lon(i).lt.360.0 .and. \
          lat(j).le. 45.0 .and. lat(j).gt. 40.0) .or. \
         (lon(i).gt.260.0 .and. lon(i).lt.355.0 .and. \
          lat(j).le. 40.0 .and. lat(j).gt. 15.0) .or. \
         (lon(i).gt.275.0 .and. lon(i).lt.360.0 .and. \
          lat(j).le. 15.0 .and. lat(j).gt. 10.0) .or. \
         (lon(i).ge.  0.0 .and. lon(i).lt. 25.0 .and. \
          lat(j).le. 15.0 .and. lat(j).gt. 10.0) .or. \
         (lon(i).gt.290.0 .and. lon(i).lt.360.0 .and. \
          lat(j).le. 10.0) .or. \
         (lon(i).ge.  0.0 .and. lon(i).lt. 25.0 .and. \
          lat(j).le. 10.0)) then
         basins_mask(j,i) = 2      ; atlantic
       end if
     end if
   end do
 end do

; Indian ocean basin
 do j = 0, nlat-1
   do i = 0, nlon-1
     if (oro(j,i).eq.0) then
       if ((lon(i).gt.60.0 .and. lon(i).lt.100.0 .and. \
          lat(j).lt. 25.0 .and. lat(j).gt. 20.0) .or. \
         (lon(i).gt. 45.0 .and. lon(i).lt.100.0 .and. \
          lat(j).le. 20.0 .and. lat(j).gt.  0.0) .or. \
         (lon(i).ge. 25.0 .and. lon(i).lt.100.0 .and. \
          lat(j).le.  0.0 .and. lat(j).gt. -5.0) .or. \
         (lon(i).ge. 25.0 .and. lon(i).le.130.0 .and. \
          lat(j).le. -5.0)) then
         basins_mask(j,i) = 3     ; indian
       end if
     end if
   end do
 end do

 return (basins_mask)     ; returns 2D mask array (lat,lon)
end
;*****************************************************************************
; Mark Stevens

; calculate the ocean heat transport for models
; gwt : gaussian weights (lat)
; oro : orography data array (lat,lon)
; requires the lat and lon are attached coordinates of oro 
; and that oro and the following variables are 2D arrays (lat,lon).
; fsns: net shortwave solar flux at surface
; flns: net longwave solar flux at surface
; shfl: sensible heat flux at surface 
; lhfl: latent heat flux at surface 

undef("oht_model")
function oht_model (gwt[*]:numeric,oro[*][*]:numeric,fsns[*][*]:numeric, \
                    flns[*][*]:numeric,shfl[*][*]:numeric,lhfl[*][*]:numeric)
begin

 if (typeof(gwt).eq."double") then
   gw = dble2flt(gwt)
 else
   gw = gwt
 end if

; constants
 pi = 3.14159265
 re = 6.371e6            ; radius of earth
 coef = re^2/1.e15       ; scaled by PW
 heat_storage = 0.3      ; W/m^2 adjustment for ocean heat storage 

 nlat = dimsizes(oro(:,0))
 nlon = dimsizes(oro(0,:))
 dlon = 2.*pi/nlon       ; dlon in radians
 lat = oro&lat 
 lat&lat = lat
 i65n = ind(lat.eq.lat({65}))
 i65s = ind(lat.eq.lat({-65}))

; get the mask for the ocean basins
 
 basins_mask = ocean_mask(oro)    ; returns 2D array(lat,lon) 

; compute net surface energy flux
 netflux = fsns
 netflux = (/fsns-flns-shfl-lhfl-heat_storage/)

; compute the net flux for the basins
 netflux_basin = new((/3,nlat,nlon/),float) 
 netflux_basin(0,:,:) = mask(netflux,basins_mask,1)  ; pacific
 netflux_basin(1,:,:) = mask(netflux,basins_mask,2)  ; atlantic
 netflux_basin(2,:,:) = mask(netflux,basins_mask,3)  ; indian

; sum flux over the longitudes in each basin
 heatflux = new((/3,nlat/),float) 
 heatflux = dim_sum(netflux_basin)  

; compute implied heat transport in each basin
 oft = new((/4,nlat/),float)
 oft!0 = "basin number"   ; 0:pacific, 1:atlantic, 2:indian, 3:total
 oft!1 = "lat"
 oft&lat = lat

 do n = 0, 2
   do j = i65n, i65s, 1      ;start sum at most northern point 
     oft(n,j) = -coef*dlon*sum(heatflux(n,j:i65n)*gw(j:i65n))
   end do
 end do

; compute total implied ocean heat transport at each latitude
; as the sum over the basins at that latitude
 do j = i65s, i65n
   oft(3,j) = sum(oft(:,j))
 end do

 return(oft)     ; 2D array(4,lat)
end

;**************************************************************************
; Mark Stevens
; calculate the heat transport for the entire surface
; gwt : gaussian weights (lat)
; oro : orography
; fsns: net shortwave solar flux at surface
; flns: net longwave solar flux at surface
; shfl: sensible heat flux at surface 
; lhfl: latent heat flux at surface 
; adjust: logical switch for applying adjustment

undef("ht_surface")
function ht_surface (gwt[*]:numeric,oro[*][*]:numeric,fsns[*][*]:numeric, \
   flns[*][*]:numeric,shfl[*][*]:numeric,lhfl[*][*]:numeric,adjust:logical)

begin

 if (typeof(gwt).eq."double") then
   gw = dble2flt(gwt)
 else
   gw = gwt
 end if

; constants
 pi = 3.14159265
 re = 6.371e6            ; radius of earth
 coef = re^2/1.e15       ; scaled by PW
 heat_storage = 0.3      ; W/m^2 adjustment for ocean heat storage 

 nlat = dimsizes(oro(:,0))
 nlon = dimsizes(oro(0,:))
 dlon = 2.*pi/nlon       ; dlon in radians
 lat = oro&lat 
 lat&lat = lat

; compute net surface energy flux
 tmp = fsns
 tmp = (/fsns-flns-shfl-lhfl/)   ; (lat,lon) 

; zonally average entire surface 
 heatflux = dim_avg(tmp)         ; (lat) 

; global mean
 if (adjust) then
   gbl = sum(heatflux*gw)/sum(gw)
 end if

 ht = new(nlat,float)
 ht!0 = "lat"
 ht&lat = lat

 do j = nlat-1,0, 1      ;start sum at most northern point 
   if (adjust) then
     ht(j) = -coef*2.*pi*sum((heatflux(j:nlat-1)-gbl)*gw(j:nlat-1))
   else
     ht(j) = -coef*2.*pi*sum(heatflux(j:nlat-1)*gw(j:nlat-1))
   end if
 end do

 return(ht)     ; 1D array(lat)
end

;***************************************************************************
; Mark Stevens
; calculate the ocean freshwater transport for models
; gwt : gaussian weights (lat)
; oro : orography data array (lat,lon)
; requires the lat and lon are attached coordinates of oro 
; and that oro and the following variables are 2D arrays (lat,lon).
; precc : convective precipitation (m/s)
; precl : large-scale precipitation (m/s)
; qflx  : surface water flux (kg/s)


undef("oft_model")
function oft_model (gwt[*]:numeric,oro[*][*]:numeric,precc[*][*]:numeric, \
                    precl[*][*]:numeric,qflx[*][*]:numeric)
begin

 if (typeof(gwt).eq."double") then
   gw = dble2flt(gwt)
 else
   gw = gwt
 end if

; constants
 pi = 3.14159265
 re = 6.371e6            ; radius of earth
 coef = re^2/1.e6        ; scaled for Sverdrups 

 nlat = dimsizes(oro(:,0))
 nlon = dimsizes(oro(0,:))
 dlon = 2.*pi/nlon       ; dlon in radians
 lat = oro&lat 
 lat&lat = lat
 i65n = ind(lat.eq.lat({65}))
 i65s = ind(lat.eq.lat({-65}))

; get the mask for the ocean basins
 
 basins_mask = ocean_mask(oro)    ; returns 2D array(lat,lon) 

; compute net surface freshwater flux
 netflux = precc
 netflux = (/(precc+precl)-qflx/1000./)   ; units of m^3/s

; compute the net flux for the basins
 netflux_basin = new((/3,nlat,nlon/),float) 
 netflux_basin(0,:,:) = mask(netflux,basins_mask,1)  ; pacific
 netflux_basin(1,:,:) = mask(netflux,basins_mask,2)  ; atlantic
 netflux_basin(2,:,:) = mask(netflux,basins_mask,3)  ; indian

; sum flux over the longitudes in each basin
 heatflux = new((/3,nlat/),float) 
 heatflux = dim_sum(netflux_basin)  

; compute implied freshwater transport in each basin
 oft = new((/4,nlat/),float)
 oft!0 = "basin number"   ; 0:pacific, 1:atlantic, 2:indian, 3:total
 oft!1 = "lat"
 oft&lat = lat

 do n = 0, 2
   do j = i65n, i65s, 1      ;start sum at most northern point 
     oft(n,j) = -coef*dlon*sum(heatflux(n,j:i65n)*gw(j:i65n))
   end do
 end do

; compute total implied ocean freshwater transport at each latitude
; as the sum over the basins at that latitude
 do j = i65s, i65n
   oft(3,j) = sum(oft(:,j))
 end do

 return(oft)     ; 2D array(4,lat)
end


;***************************************************************************
; Mark Stevens

; calculate the required heat transport from model data at the top of
; the atmosphere (TOA)
; gwt : gaussian weights (lat)
; restoa : residual energy at TOA = fsntoa-flut  


undef("rht_model")
function rht_model (gwt[*]:numeric,restoa[*][*]:numeric)
begin

 if (typeof(gwt).eq."double") then
   gw = dble2flt(gwt)
 else
   gw = gwt
 end if

; constants
 pi = 3.14159265
 re = 6.371e6            ; radius of earth
 coef = re^2/1.e15       ; scaled for PW 

 nlat = dimsizes(restoa(:,0))
 nlon = dimsizes(restoa(0,:))
 dlon = 2.*pi/nlon       ; dlon in radians
 lat = restoa&lat 
 lat&lat = lat

; sum flux over the longitudes 
 heatflux = dim_sum(restoa)  

; compute required heat transport 
 rht = new(nlat,float)
 rht!0 = "lat"
 rht&lat = lat

 do j = nlat-1, 0, 1      ;start sum at most northern point 
   rht(j) = -coef*dlon*sum(heatflux(j:nlat-1)*gw(j:nlat-1))
 end do

 return(rht)     ; 1D array(nlat)

end



