load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
;=========================================================================================
; Template for function or procedure
;=========================================================================================
;undef("FuncProc_name")
;function/procedure FuncProc_name(varlist)
;local localvarlist
;begin
;end

;=========================================================================================
; get_coordinate_system_string
;=========================================================================================
undef("get_coordinate_system_string")
function get_coordinate_system_string(Opt[1]:logical)
local ProjStr, Proj, ProjName
begin
	ProjStr="";
	Proj=(/	"aea", "aeqd", "airy", "aitoff", \
					"alsk", "apian", "august", "bacon", "bipc", "boggs", "bonne", "cass", "cc", \ 
					"cea", "chamb", "collg", "crast", "denoy", "eck1", "eck2", "eck3", "eck4", \
					"eck5", "eck6", "eqc", "eqdc", "euler", "etmerc", "fahey", "fouc", "fouc_s", \
					"gall", "geos", "gins8", "gn_sinu", "gnom", "goode", "gs48", "gs50", "hammer", \
					"hatano", "healpix", "rhealpix", "igh", "imw_p", "isea", "kav5", "kav7", \
					"krovak", "labrd", "laea", "lagrng", "larr", "lask", "lonlat", "latlon", "lcc", \
					"lcca", "leac", "lee_os", "loxim", "lsat", "mbt_s", "mbt_fps", "mbtfpp", \
					"mbtfpq", "mbtfps", "merc", "mil_os", "mill", "moll", "murd1", "murd2", "murd3", \
					"natearth", "nell", "nell_h", "nicol", "nsper", "nzmg", "ob_tran", "ocea", "oea", \
					"omerc", "ortel", "ortho", "pconic", "poly", "putp1", "putp2", "putp3", "putp3p",\
					"putp4p", "putp5", "putp5p", "putp6", "putp6p", "qua_aut", "robin", "rouss", \
					"rpoly", "sinu", "somerc", "stere", "sterea", "gstmerc", "tcc", "tcea", "tissot",\
					"tmerc", "tpeqd", "tpers", "ups", "urm5", "urmfps", "utm", "vandg", "vandg2", \
					"vandg3", "vandg4", "vitk1", "wag1", "wag2","wag3", "wag4", "wag5", "wag6", \
					"wag7", "weren", "wink1", "wink2", "wintri" /)
	ProjName=(/ "Albers Equal Area", "Azimuthal Equidistant", "Airy", "Aitoff", \
	 						"Mod. Stererographics of Alaska", "Apian Globular I", "August Epicycloidal",\
	 						"Bacon Globular", "Bipolar conic of western hemisphere", "Boggs Eumorphic",\
	 						"Bonne (Werner lat_1=90)", "Cassini", "Central Cylindrical", \
	 						"Equal Area Cylindrical", "Chamberlin Trimetric", "Collignon", \
	 						"Craster Parabolic (Putnins P4)", "Denoyer Semi-Elliptical", "Eckert I", \
	 						"Eckert II", "Eckert III", "Eckert IV", "Eckert V", "Eckert VI", \
	 						"Equidistant Cylindrical (Plate Caree)", "Equidistant Conic", "Euler", \
	 						"Extended Transverse Mercator", "Fahey", "Foucaut", "Foucaut Sinusoidal", \
	 						"Gall (Gall Stereographic)", "Geostationary Satellite View", \
	 						"Ginsburg VIII (TsNIIGAiK)", "General Sinusoidal Series", "Gnomonic", \
	 						"Goode Homolosine", "Mod. Stererographics of 48 U.S.", \
	 						"Mod. Stererographics of 50 U.S.", "Hammer & Eckert-Greifendorff", \
	 						"Hatano Asymmetrical Equal Area", "HEALPix", "rHEALPix", \
	 						"Interrupted Goode Homolosine", "International Map of the World Polyconic", \
	 						"Icosahedral Snyder Equal Area", "Kavraisky V", "Kavraisky VII", "Krovak", \
	 						"Laborde", "Lambert Azimuthal Equal Area", "Lagrange", "Larrivee", \
	 						"Laskowski", "Lat/long (Geodetic)", "Lat/long (Geodetic alias)", \
	 						"Lambert Conformal Conic", "Lambert Conformal Conic Alternative", \
	 						"Lambert Equal Area Conic", "Lee Oblated Stereographic", "Loximuthal", \
	 						"Space oblique for LANDSAT", "McBryde-Thomas Flat-Polar Sine (No. 1)", \
	 						"McBryde-Thomas Flat-Pole Sine (No. 2)", \
	 						"McBride-Thomas Flat-Polar Parabolic", "McBryde-Thomas Flat-Polar Quartic",\
	 						"McBryde-Thomas Flat-Polar Sinusoidal", "Mercator", \
	 						"Miller Oblated Stereographic", "Miller Cylindrical", "Mollweide", \
	 						"Murdoch I", "Murdoch II", "Murdoch III", "Natural Earth", "Nell", \
	 						"Nell-Hammer", "Nicolosi Globular", "Near-sided perspective", \
	 						"New Zealand Map Grid", "General Oblique Transformation", \
	 						"Oblique Cylindrical Equal Area", "Oblated Equal Area", \
	 						"Oblique Mercator", "Ortelius Oval", "Orthographic", \
	 						"Perspective Conic", "Polyconic (American)", "Putnins P1", \
	 						"Putnins P2", "Putnins P3", "Putnins P3'", "Putnins P4'", "Putnins P5", \
	 						"Putnins P5'", "Putnins P6", "Putnins P6'", "Quartic Authalic", \
	 						"Robinson", "Roussilhe Stereographic", "Rectangular Polyconic", \
	 						"Sinusoidal (Sanson-Flamsteed)", "Swiss. Obl. Mercator", "Stereographic", \
	 						"Oblique Stereographic Alternative", \
	 						"Gauss-Schreiber Transverse Mercator (aka Gauss-Laborde Reunion)", \
	 						"Transverse Central Cylindrical", "Transverse Cylindrical Equal Area", \
	 						"Tissot", "Transverse Mercator", "Two Point Equidistant", \
	 						"Tilted perspective", "Universal Polar Stereographic", "Urmaev V", \
	 						"Urmaev Flat-Polar Sinusoidal", "Universal Transverse Mercator (UTM)", \
	 						"van der Grinten (I)", "van der Grinten II", "van der Grinten III", \
	 						"van der Grinten IV", "Vitkovsky I", "Wagner I (Kavraisky VI)", \
	 						"Wagner II", "Wagner III", "Wagner IV", "Wagner V", "Wagner VI", \
	 						"Wagner VII", "Werenskiold I", "Winkel I", "Winkel II", "Winkel Tripel" /)
	 						
	ellps=(/ "MERIT", "SGS85", "GRS80", "IAU76", "airy", "APL4.9", "NWL9D", "mod_airy", \
	         "andrae", "aust_SA", "GRS67", "bessel", "bess_nam", "clrk66", "clrk80", "CPM",\
	         "delmbr", "engelis", "evrst30", "evrst48", "evrst56", "evrst69", "evrstSS", \
	         "fschr60", "fschr60m", "fschr68", "helmert", "hough", "intl", "krass", \
	         "kaula", "lerch", "mprts", "new_intl", "plessis", "SEasia", "walbeck", \
	         "WGS60", "WGS66", "WGS72", "WGS84", "sphere" /)
	         
	datum=(/ "WGS84", "GGRS87", "Greek_Geodetic_Reference_System_1987", "NAD83", \
	         "North_American_Datum_1983", "NAD27", "North_American_Datum_1927", \
	         "potsdam", "Potsdam", "carthage", "Carthage", "bessel", "Hermannskogel", \
	         "ire65", "Ireland", "nzgd49", "New", "OSGB36", "Airy" /)
	         
	; Adding Projection Name (Required)
	if (isatt(Opt,"projection")) then
		if(all(Opt@projection.ne.Proj)) then
			print(Opt@projection+" is not a supported projection.")
			print("Supported projections are:")
			print("- "+Proj)
			exit
		else
			ProjStr=" +proj="+Opt@projection
		end if
	else
		print("Opt@projection is missing.")
		exit
	end if
	
	; Adding Ellipsoid (required)
	if (isatt(Opt,"ellipsoid")) then
		if(all(Opt@ellipsoid.ne.ellps)) then
			print(Opt@ellipsoid+" is not a supported projection.")
			print("Supported ellipsoids are:")
			print("- "+ellps);
			exit
		else
			ProjStr=ProjStr + " +ellps="+Opt@ellipsoid
		end if
	else
		print("Opt@ellipsoid is missing.")
		exit
	end if

	; Optional
	; Semimajor radius of the ellipsoid axis
	if (isatt(Opt,"a")) then
			ProjStr=ProjStr + " +a="+Opt@a
	end if

	;Used with Oblique Mercator and possibly a few others
	if (isatt(Opt,"alpha")) then
			ProjStr=ProjStr + " +alpha="+Opt@alpha
	end if

	;Axis orientation
	if (isatt(Opt,"axis")) then
			ProjStr=ProjStr + " +axis="+Opt@axis
	end if
	
	;Semiminor radius of the ellipsoid axis
	if (isatt(Opt,"b")) then
			ProjStr=ProjStr + " +b="+Opt@b
	end if

	;Eccentricity of the ellipsoid = sqrt(1 - b^2/a^2) = sqrt( f*(2-f) )
	if (isatt(Opt,"e")) then
			ProjStr=ProjStr + " +e="+Opt@e
	end if
	
	;Eccentricity of the ellipsoid squared
	if (isatt(Opt,"es")) then
			ProjStr=ProjStr + " +es="+Opt@es
	end if
	
	;Flattening of the ellipsoid = 1-sqrt(1-e^2) (often presented as an inverse, e.g. 1/298)
	if (isatt(Opt,"f")) then
			ProjStr=ProjStr + " +f="+Opt@f
	end if
	
	; Datum name
	if (isatt(Opt,"datum")) then
		if(all(Opt@datum.ne.datum)) then
			print(Opt@datum+" is not a supported projection.")
			print("Supported datums are:")
			print("- "+datum);
			exit
		else
			ProjStr=ProjStr + " +datum="+Opt@datum
		end if
	end if

	;Scaling factor (old name)
	if (isatt(Opt,"k")) then
			ProjStr=ProjStr + " +k="+Opt@k
	end if

	;Scaling factor (new name)
	if (isatt(Opt,"k_0")) then
			ProjStr=ProjStr + " +k_0="+Opt@k_0
	end if

	;Latitude of origin
	if (isatt(Opt,"lat_0")) then
			ProjStr=ProjStr + " +lat_0="+Opt@lat_0
	end if

	;Latitude of first standard parallel
	if (isatt(Opt,"lat_1")) then
			ProjStr=ProjStr + " +lat_1="+Opt@lat_1
	end if
	
	;Latitude of second standard parallel
	if (isatt(Opt,"lat_2")) then
			ProjStr=ProjStr + " +lat_2="+Opt@lat_2
	end if
	
	;Latitude of true scale
	if (isatt(Opt,"lat_ts")) then
			ProjStr=ProjStr + " +lat_ts="+Opt@lat_ts
	end if
	
	;Central meridian
	if (isatt(Opt,"lon_0")) then
			ProjStr=ProjStr + " +lon_0="+Opt@lon_0
	end if	
	
	;Longitude used with Oblique Mercator and possibly a few others
	if (isatt(Opt,"lonc")) then
			ProjStr=ProjStr + " +lonc="+Opt@lonc
	end if		
	
	;Center longitude to use for wrapping
	if (isatt(Opt,"lon_wrap")) then
			ProjStr=ProjStr + " +lon_wrap="+Opt@lon_wrap
	end if		

	;Filename of NTv2 grid file to use for datum transforms
	if (isatt(Opt,"nadgrids")) then
			ProjStr=ProjStr + " +nadgrids="+Opt@nadgrids
	end if		

	if (isatt(Opt,"no_defs")) then
			ProjStr=ProjStr + " +no_defs="+Opt@no_defs
	end if

	;Allow longitude output outside -180 to 180 range, disables wrapping
	if (isatt(Opt,"over")) then
			ProjStr=ProjStr + " +over="+Opt@over
	end if		

	;Alternate prime meridian (typically a city name)
	if (isatt(Opt,"pm")) then
			ProjStr=ProjStr + " +pm="+Opt@pm
	end if

	;Denotes southern hemisphere UTM zone
	if (isatt(Opt,"south")) then
			ProjStr=ProjStr + " +south="+Opt@south
	end if

	;Multiplier to convert map units to 1.0m
	if (isatt(Opt,"to_meter")) then
			ProjStr=ProjStr + " +to_meter="+Opt@to_meter
	end if

	;3 or 7 term datum transform parameters
  if (isatt(Opt,"towgs84")) then
			ProjStr=ProjStr + " +towgs84="+Opt@towgs84
	end if
	
	;meters, US survey feet, etc.
  if (isatt(Opt,"units")) then
			ProjStr=ProjStr + " +units="+Opt@units
	end if
	
	;vertical conversion to meters.
  if (isatt(Opt,"vto_meter")) then
			ProjStr=ProjStr + " +vto_meter="+Opt@vto_meter
	end if	
	
	;vertical units
  if (isatt(Opt,"vunits")) then
			ProjStr=ProjStr + " +vunits="+Opt@vunits
	end if
	
	;False easting
  if (isatt(Opt,"x_0")) then
			ProjStr=ProjStr + " +x_0="+Opt@x_0
	end if
	
	;False northing
  if (isatt(Opt,"y_0")) then
			ProjStr=ProjStr + " +y_0="+Opt@y_0
	end if
	
	;UTM zone
  if (isatt(Opt,"zone")) then
			ProjStr=ProjStr + " +zone="+Opt@zone
	end if
					
	return(ProjStr)
end

;=========================================================================================
; GetAltModeNumber: The integer number are based on the enum type in c
; implementation.
;=========================================================================================
undef("GetAltModeNumber")
function GetAltModeNumber(AltMode[1]:string)
begin
    if (AltMode.eq."absolute") then
      return(touint(0));
    end if
    if (AltMode.eq."clampToGround") then
      return(touint(1));
    end if
    if (AltMode.eq."clampToSeaFloor") then
      return(touint(2));
    end if
    if (AltMode.eq."relativeToGround") then
      return(touint(3));
    end if
    if (AltMode.eq."relativeToSeaFloor") then
      return(touint(4));
    end if
end
    
;=========================================================================================
; readICT: Reads ICARTT file format which is designed for airborne
; measurements. The Variables, their units, and the entire header is
; assigned to the variable as an attribute.
;=========================================================================================
undef("read_ict")
function read_ict(FileName[1]:string)
local tmp, nLines, nHeader, nVar, Values, MissingValues
begin
  ; Reading the ict file
  tmp=asciiread(FileName,-1,"string")

  ; Getting number of lines in the file
  nLines=dimsizes(tmp)
  
  ; Getting number of header Lines;
  nHeader=tointeger(str_get_field(tmp(0),1," "))
  
  ; Getting number of variables
  nVar=tointeger(tmp(9))+1;
  
  ; Getting the Values
  asciiwrite("tmp.txt",tmp(nHeader:nLines-1))
  Values=asciiread("tmp.txt",(/nLines-nHeader,nVar/),"double")
  system("rm -rf tmp.txt")
  
  ; Assigning the Variable Names and Units
  Values@VariableNames=new(nVar,"string")
  Values@VariableNames(0)=str_get_field(tmp(8),1,",")
  Values@VariableUnits=new(nVar,"string")
  Values@VariableUnits(0)=str_get_field(tmp(8),2,",")
  Values@VariableNames(1:nVar-1)=str_get_field(tmp(12:12+nVar-2),1,",");
  Values@VariableUnits(1:nVar-1)=str_get_field(tmp(12:12+nVar-2),2,",");
  
  ; Getting the NoData Values and assigning the missing values
  Values@_FillValue=-9999.0d
  asciiwrite("tmp.txt",tmp(11))
  MissingValues=asciiread("tmp.txt",nVar-1,"double")
  system("rm -rf tmp.txt")
  do i=1,nVar-1
    Values(:,i)=where(Values(:,i).eq.MissingValues(i-1),Values@_FillValue,Values(:,i))
  end do
  
  ; Storing the header lines
  Values@Header=tmp(0:nHeader-1)
  
  Values@Description="Variable names and their units are stored at Var@VariableNames and Var@VariableUnits. For full headers refer to Var@Header."  
  return(Values);
end

;=========================================================================================
; squeeze: Removes those dimensions with one element.
;=========================================================================================
undef("squeeze")
function squeeze(inVar)
local inVardimsizes, newdimsizes, outVar, counter
begin
    inVardimsizes=dimsizes(inVar)
    newdimsizes=inVardimsizes(ind(inVardimsizes.ne.1))
    outVar=reshape(inVar,newdimsizes)
    
    ;copying the dimension names.
    counter=0;
    do i=0,dimsizes(inVardimsizes)-1
        if (inVardimsizes(i).ne.1) then
            if (isdefined(inVar!i)) then
                tmpName=inVar!i
                outVar!counter=tmpName
                outVar&$tmpName$=inVar&$tmpName$
            end if
            counter=counter+1
        end if
    end do
    
    return(outVar)
end

;=========================================================================================
; addTime: given a dd-hh-mm-ss and a time interval as hh-mm-ss, calculates
; the new dd-hh-mm-ss. Note that dd can become negative.
;=========================================================================================
undef("addTime")
procedure addTime(dd[1]:integer,hh[1]:integer,mm[1]:integer,ss[1]:integer, \
                 hhI[1]:integer, mmI[1]:integer, ssI[1]:integer)
local BeginTime, TimeInterval, EndTime, Counter, EndTime
begin
  BeginTime=dd*24.0d*3600.0d + hh*3600.0d + mm*60.0d + ss;
  TimeInterval=hhI*3600.0d + mmI*60.0d + ssI;
  EndTime=BeginTime+TimeInterval
  
  Counter=0
  do while(EndTime.lt.0.0d)
    EndTime=EndTime+24.0d*3600.0d
    Counter=Counter-1
  end do
  
  dd=Counter
  dd=dd+tointeger(EndTime/(24.0d*3600.0d))
  EndTime=todouble( tointeger(EndTime)%(24*3600) )
  hh=tointeger(EndTime/3600.d)
  EndTime=todouble( tointeger(EndTime)%(3600) )
  mm=tointeger(EndTime/60.d)
  EndTime=todouble( tointeger(EndTime)%(60) )
  ss=tointeger(EndTime)
end

;=========================================================================================
; get_KML_IconHref: returns a URL to standard icons of Google Earth.
; if the IconName is not found within the standard names, the IconName
; is returned. This behavior can be used to return a weblink to non-standard
; icons.
;=========================================================================================
undef("get_KML_IconHref")
function get_KML_IconHref(IconName[1]:string)
begin
  if (IconName.eq."ylw-pushpin") then
    return("http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png")
  end if
  if (IconName.eq."blue-pushpin") then
    return("http://maps.google.com/mapfiles/kml/pushpin/blue-pushpin.png")
  end if
  if (IconName.eq."grn-pushpin") then
    return("http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png")
  end if
  if (IconName.eq."ltblu-pushpin") then
    return("http://maps.google.com/mapfiles/kml/pushpin/ltblu-pushpin.png")
  end if
  if (IconName.eq."pink-pushpin") then
    return("http://maps.google.com/mapfiles/kml/pushpin/pink-pushpin.png")
  end if
  if (IconName.eq."purple-pushpin") then
    return("http://maps.google.com/mapfiles/kml/pushpin/purple-pushpin.png")
  end if
  if (IconName.eq."red-pushpin") then
    return("http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png")
  end if
  if (IconName.eq."wht-pushpin") then
    return("http://maps.google.com/mapfiles/kml/pushpin/wht-pushpin.png")
  end if
  if (IconName.eq."A") then
    return("http://maps.google.com/mapfiles/kml/paddle/A.png")
  end if
  if (IconName.eq."B") then
    return("http://maps.google.com/mapfiles/kml/paddle/B.png")
  end if
  if (IconName.eq."C") then
    return("http://maps.google.com/mapfiles/kml/paddle/C.png")
  end if
  if (IconName.eq."D") then
    return("http://maps.google.com/mapfiles/kml/paddle/D.png")
  end if
  if (IconName.eq."E") then
    return("http://maps.google.com/mapfiles/kml/paddle/E.png")
  end if
  if (IconName.eq."F") then
    return("http://maps.google.com/mapfiles/kml/paddle/F.png")
  end if
  if (IconName.eq."G") then
    return("http://maps.google.com/mapfiles/kml/paddle/G.png")
  end if
  if (IconName.eq."H") then
    return("http://maps.google.com/mapfiles/kml/paddle/H.png")
  end if
  if (IconName.eq."I") then
    return("http://maps.google.com/mapfiles/kml/paddle/I.png")
  end if
  if (IconName.eq."J") then
    return("http://maps.google.com/mapfiles/kml/paddle/J.png")
  end if
  if (IconName.eq."A") then
    return("http://maps.google.com/mapfiles/kml/paddle/A.png")
  end if
  if (IconName.eq."B") then
    return("http://maps.google.com/mapfiles/kml/paddle/B.png")
  end if
  if (IconName.eq."C") then
    return("http://maps.google.com/mapfiles/kml/paddle/C.png")
  end if
  if (IconName.eq."D") then
    return("http://maps.google.com/mapfiles/kml/paddle/D.png")
  end if
  if (IconName.eq."E") then
    return("http://maps.google.com/mapfiles/kml/paddle/E.png")
  end if
  if (IconName.eq."F") then
    return("http://maps.google.com/mapfiles/kml/paddle/F.png")
  end if
  if (IconName.eq."G") then
    return("http://maps.google.com/mapfiles/kml/paddle/G.png")
  end if
  if (IconName.eq."H") then
    return("http://maps.google.com/mapfiles/kml/paddle/H.png")
  end if
  if (IconName.eq."I") then
    return("http://maps.google.com/mapfiles/kml/paddle/I.png")
  end if
  if (IconName.eq."J") then
    return("http://maps.google.com/mapfiles/kml/paddle/J.png")
  end if
  if (IconName.eq."K") then
    return("http://maps.google.com/mapfiles/kml/paddle/K.png")
  end if
  if (IconName.eq."L") then
    return("http://maps.google.com/mapfiles/kml/paddle/L.png")
  end if
  if (IconName.eq."M") then
    return("http://maps.google.com/mapfiles/kml/paddle/M.png")
  end if
  if (IconName.eq."N") then
    return("http://maps.google.com/mapfiles/kml/paddle/N.png")
  end if
  if (IconName.eq."O") then
    return("http://maps.google.com/mapfiles/kml/paddle/O.png")
  end if
  if (IconName.eq."P") then
    return("http://maps.google.com/mapfiles/kml/paddle/P.png")
  end if
  if (IconName.eq."Q") then
    return("http://maps.google.com/mapfiles/kml/paddle/Q.png")
  end if
  if (IconName.eq."R") then
    return("http://maps.google.com/mapfiles/kml/paddle/R.png")
  end if
  if (IconName.eq."S") then
    return("http://maps.google.com/mapfiles/kml/paddle/S.png")
  end if
  if (IconName.eq."T") then
    return("http://maps.google.com/mapfiles/kml/paddle/T.png")
  end if
  if (IconName.eq."K") then
    return("http://maps.google.com/mapfiles/kml/paddle/K.png")
  end if
  if (IconName.eq."L") then
    return("http://maps.google.com/mapfiles/kml/paddle/L.png")
  end if
  if (IconName.eq."M") then
    return("http://maps.google.com/mapfiles/kml/paddle/M.png")
  end if
  if (IconName.eq."N") then
    return("http://maps.google.com/mapfiles/kml/paddle/N.png")
  end if
  if (IconName.eq."O") then
    return("http://maps.google.com/mapfiles/kml/paddle/O.png")
  end if
  if (IconName.eq."P") then
    return("http://maps.google.com/mapfiles/kml/paddle/P.png")
  end if
  if (IconName.eq."Q") then
    return("http://maps.google.com/mapfiles/kml/paddle/Q.png")
  end if
  if (IconName.eq."R") then
    return("http://maps.google.com/mapfiles/kml/paddle/R.png")
  end if
  if (IconName.eq."S") then
    return("http://maps.google.com/mapfiles/kml/paddle/S.png")
  end if
  if (IconName.eq."T") then
    return("http://maps.google.com/mapfiles/kml/paddle/T.png")
  end if
  if (IconName.eq."U") then
    return("http://maps.google.com/mapfiles/kml/paddle/U.png")
  end if
  if (IconName.eq."V") then
    return("http://maps.google.com/mapfiles/kml/paddle/V.png")
  end if
  if (IconName.eq."W") then
    return("http://maps.google.com/mapfiles/kml/paddle/W.png")
  end if
  if (IconName.eq."X") then
    return("http://maps.google.com/mapfiles/kml/paddle/X.png")
  end if
  if (IconName.eq."Y") then
    return("http://maps.google.com/mapfiles/kml/paddle/Y.png")
  end if
  if (IconName.eq."Z") then
    return("http://maps.google.com/mapfiles/kml/paddle/Z.png")
  end if
  if (IconName.eq."U") then
    return("http://maps.google.com/mapfiles/kml/paddle/U.png")
  end if
  if (IconName.eq."V") then
    return("http://maps.google.com/mapfiles/kml/paddle/V.png")
  end if
  if (IconName.eq."W") then
    return("http://maps.google.com/mapfiles/kml/paddle/W.png")
  end if
  if (IconName.eq."X") then
    return("http://maps.google.com/mapfiles/kml/paddle/X.png")
  end if
  if (IconName.eq."Y") then
    return("http://maps.google.com/mapfiles/kml/paddle/Y.png")
  end if
  if (IconName.eq."Z") then
    return("http://maps.google.com/mapfiles/kml/paddle/Z.png")
  end if
  if (IconName.eq."1") then
    return("http://maps.google.com/mapfiles/kml/paddle/1.png")
  end if
  if (IconName.eq."2") then
    return("http://maps.google.com/mapfiles/kml/paddle/2.png")
  end if
  if (IconName.eq."3") then
    return("http://maps.google.com/mapfiles/kml/paddle/3.png")
  end if
  if (IconName.eq."4") then
    return("http://maps.google.com/mapfiles/kml/paddle/4.png")
  end if
  if (IconName.eq."5") then
    return("http://maps.google.com/mapfiles/kml/paddle/5.png")
  end if
  if (IconName.eq."6") then
    return("http://maps.google.com/mapfiles/kml/paddle/6.png")
  end if
  if (IconName.eq."7") then
    return("http://maps.google.com/mapfiles/kml/paddle/7.png")
  end if
  if (IconName.eq."8") then
    return("http://maps.google.com/mapfiles/kml/paddle/8.png")
  end if
  if (IconName.eq."9") then
    return("http://maps.google.com/mapfiles/kml/paddle/9.png")
  end if
  if (IconName.eq."10") then
    return("http://maps.google.com/mapfiles/kml/paddle/10.png")
  end if
  if (IconName.eq."1") then
    return("http://maps.google.com/mapfiles/kml/paddle/1.png")
  end if
  if (IconName.eq."2") then
    return("http://maps.google.com/mapfiles/kml/paddle/2.png")
  end if
  if (IconName.eq."3") then
    return("http://maps.google.com/mapfiles/kml/paddle/3.png")
  end if
  if (IconName.eq."4") then
    return("http://maps.google.com/mapfiles/kml/paddle/4.png")
  end if
  if (IconName.eq."5") then
    return("http://maps.google.com/mapfiles/kml/paddle/5.png")
  end if
  if (IconName.eq."6") then
    return("http://maps.google.com/mapfiles/kml/paddle/6.png")
  end if
  if (IconName.eq."7") then
    return("http://maps.google.com/mapfiles/kml/paddle/7.png")
  end if
  if (IconName.eq."8") then
    return("http://maps.google.com/mapfiles/kml/paddle/8.png")
  end if
  if (IconName.eq."9") then
    return("http://maps.google.com/mapfiles/kml/paddle/9.png")
  end if
  if (IconName.eq."10") then
    return("http://maps.google.com/mapfiles/kml/paddle/10.png")
  end if
  if (IconName.eq."blu-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-blank.png")
  end if
  if (IconName.eq."blu-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-blank.png")
  end if
  if (IconName.eq."blu-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-diamond.png")
  end if
  if (IconName.eq."blu-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-diamond.png")
  end if
  if (IconName.eq."blu-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-circle.png")
  end if
  if (IconName.eq."blu-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-circle.png")
  end if
  if (IconName.eq."blu-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-square.png")
  end if
  if (IconName.eq."blu-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-square.png")
  end if
  if (IconName.eq."blu-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-stars.png")
  end if
  if (IconName.eq."blu-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/blu-stars.png")
  end if
  if (IconName.eq."grn-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-blank.png")
  end if
  if (IconName.eq."grn-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-blank.png")
  end if
  if (IconName.eq."grn-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-diamond.png")
  end if
  if (IconName.eq."grn-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-diamond.png")
  end if
  if (IconName.eq."grn-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-circle.png")
  end if
  if (IconName.eq."grn-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-circle.png")
  end if
  if (IconName.eq."grn-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-square.png")
  end if
  if (IconName.eq."grn-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-square.png")
  end if
  if (IconName.eq."grn-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-stars.png")
  end if
  if (IconName.eq."grn-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/grn-stars.png")
  end if
  if (IconName.eq."ltblu-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-blank.png")
  end if
  if (IconName.eq."ltblu-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-blank.png")
  end if
  if (IconName.eq."ltblu-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-diamond.png")
  end if
  if (IconName.eq."ltblu-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-diamond.png")
  end if
  if (IconName.eq."ltblu-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-circle.png")
  end if
  if (IconName.eq."ltblu-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-circle.png")
  end if
  if (IconName.eq."ltblu-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-square.png")
  end if
  if (IconName.eq."ltblu-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-square.png")
  end if
  if (IconName.eq."ltblu-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-stars.png")
  end if
  if (IconName.eq."ltblu-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/ltblu-stars.png")
  end if
  if (IconName.eq."pink-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-blank.png")
  end if
  if (IconName.eq."pink-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-blank.png")
  end if
  if (IconName.eq."pink-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-diamond.png")
  end if
  if (IconName.eq."pink-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-diamond.png")
  end if
  if (IconName.eq."pink-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-circle.png")
  end if
  if (IconName.eq."pink-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-circle.png")
  end if
  if (IconName.eq."pink-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-square.png")
  end if
  if (IconName.eq."pink-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-square.png")
  end if
  if (IconName.eq."pink-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-stars.png")
  end if
  if (IconName.eq."pink-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/pink-stars.png")
  end if
  if (IconName.eq."ylw-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/ylw-diamond.png")
  end if
  if (IconName.eq."ylw-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/ylw-diamond.png")
  end if
  if (IconName.eq."ylw-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/ylw-circle.png")
  end if
  if (IconName.eq."ylw-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/ylw-circle.png")
  end if
  if (IconName.eq."ylw-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/ylw-square.png")
  end if
  if (IconName.eq."ylw-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/ylw-square.png")
  end if
  if (IconName.eq."ylw-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/ylw-stars.png")
  end if
  if (IconName.eq."ylw-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/ylw-stars.png")
  end if
  if (IconName.eq."wht-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-blank.png")
  end if
  if (IconName.eq."wht-blank") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-blank.png")
  end if
  if (IconName.eq."wht-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-diamond.png")
  end if
  if (IconName.eq."wht-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-diamond.png")
  end if
  if (IconName.eq."wht-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-circle.png")
  end if
  if (IconName.eq."wht-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-circle.png")
  end if
  if (IconName.eq."wht-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-square.png")
  end if
  if (IconName.eq."wht-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-square.png")
  end if
  if (IconName.eq."wht-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-stars.png")
  end if
  if (IconName.eq."wht-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/wht-stars.png")
  end if
  if (IconName.eq."red-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/red-diamond.png")
  end if
  if (IconName.eq."red-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/red-diamond.png")
  end if
  if (IconName.eq."red-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/red-circle.png")
  end if
  if (IconName.eq."red-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/red-circle.png")
  end if
  if (IconName.eq."red-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/red-square.png")
  end if
  if (IconName.eq."red-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/red-square.png")
  end if
  if (IconName.eq."red-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/red-stars.png")
  end if
  if (IconName.eq."red-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/red-stars.png")
  end if
  if (IconName.eq."purple-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/purple-diamond.png")
  end if
  if (IconName.eq."purple-diamond") then
    return("http://maps.google.com/mapfiles/kml/paddle/purple-diamond.png")
  end if
  if (IconName.eq."purple-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/purple-circle.png")
  end if
  if (IconName.eq."purple-circle") then
    return("http://maps.google.com/mapfiles/kml/paddle/purple-circle.png")
  end if
  if (IconName.eq."purple-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/purple-square.png")
  end if
  if (IconName.eq."purple-square") then
    return("http://maps.google.com/mapfiles/kml/paddle/purple-square.png")
  end if
  if (IconName.eq."purple-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/purple-stars.png")
  end if
  if (IconName.eq."purple-stars") then
    return("http://maps.google.com/mapfiles/kml/paddle/purple-stars.png")
  end if
  if (IconName.eq."arrow-reverse") then
    return("http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png")
  end if
  if (IconName.eq."arrow") then
    return("http://maps.google.com/mapfiles/kml/shapes/arrow.png")
  end if
  if (IconName.eq."track") then
    return("http://maps.google.com/mapfiles/kml/shapes/track.png")
  end if
  if (IconName.eq."donut") then
    return("http://maps.google.com/mapfiles/kml/shapes/donut.png")
  end if
  if (IconName.eq."forbidden") then
    return("http://maps.google.com/mapfiles/kml/shapes/forbidden.png")
  end if
  if (IconName.eq."info-i") then
    return("http://maps.google.com/mapfiles/kml/shapes/info-i.png")
  end if
  if (IconName.eq."polygon") then
    return("http://maps.google.com/mapfiles/kml/shapes/polygon.png")
  end if
  if (IconName.eq."open-diamond") then
    return("http://maps.google.com/mapfiles/kml/shapes/open-diamond.png")
  end if
  if (IconName.eq."square") then
    return("http://maps.google.com/mapfiles/kml/shapes/square.png")
  end if
  if (IconName.eq."star") then
    return("http://maps.google.com/mapfiles/kml/shapes/star.png")
  end if
  if (IconName.eq."target") then
    return("http://maps.google.com/mapfiles/kml/shapes/target.png")
  end if
  if (IconName.eq."triangle") then
    return("http://maps.google.com/mapfiles/kml/shapes/triangle.png")
  end if
  if (IconName.eq."cross-hairs") then
    return("http://maps.google.com/mapfiles/kml/shapes/cross-hairs.png")
  end if
  if (IconName.eq."placemark_square") then
    return("http://maps.google.com/mapfiles/kml/shapes/placemark_square.png")
  end if
  if (IconName.eq."placemark_circle") then
    return("http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png")
  end if
  if (IconName.eq."shaded_dot") then
    return("http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png")
  end if
  if (IconName.eq."dining") then
    return("http://maps.google.com/mapfiles/kml/shapes/dining.png")
  end if
  if (IconName.eq."coffee") then
    return("http://maps.google.com/mapfiles/kml/shapes/coffee.png")
  end if
  if (IconName.eq."bars") then
    return("http://maps.google.com/mapfiles/kml/shapes/bars.png")
  end if
  if (IconName.eq."snack_bar") then
    return("http://maps.google.com/mapfiles/kml/shapes/snack_bar.png")
  end if
  if (IconName.eq."man") then
    return("http://maps.google.com/mapfiles/kml/shapes/man.png")
  end if
  if (IconName.eq."woman") then
    return("http://maps.google.com/mapfiles/kml/shapes/woman.png")
  end if
  if (IconName.eq."wheel_chair_accessible") then
    return("http://maps.google.com/mapfiles/kml/shapes/wheel_chair_accessible.png")
  end if
  if (IconName.eq."parking_lot") then
    return("http://maps.google.com/mapfiles/kml/shapes/parking_lot.png")
  end if
  if (IconName.eq."cabs") then
    return("http://maps.google.com/mapfiles/kml/shapes/cabs.png")
  end if
  if (IconName.eq."bus") then
    return("http://maps.google.com/mapfiles/kml/shapes/bus.png")
  end if
  if (IconName.eq."truck") then
    return("http://maps.google.com/mapfiles/kml/shapes/truck.png")
  end if
  if (IconName.eq."rail") then
    return("http://maps.google.com/mapfiles/kml/shapes/rail.png")
  end if
  if (IconName.eq."airports") then
    return("http://maps.google.com/mapfiles/kml/shapes/airports.png")
  end if
  if (IconName.eq."ferry") then
    return("http://maps.google.com/mapfiles/kml/shapes/ferry.png")
  end if
  if (IconName.eq."heliport") then
    return("http://maps.google.com/mapfiles/kml/shapes/heliport.png")
  end if
  if (IconName.eq."subway") then
    return("http://maps.google.com/mapfiles/kml/shapes/subway.png")
  end if
  if (IconName.eq."tram") then
    return("http://maps.google.com/mapfiles/kml/shapes/tram.png")
  end if
  if (IconName.eq."info") then
    return("http://maps.google.com/mapfiles/kml/shapes/info.png")
  end if
  if (IconName.eq."info_circle") then
    return("http://maps.google.com/mapfiles/kml/shapes/info_circle.png")
  end if
  if (IconName.eq."flag") then
    return("http://maps.google.com/mapfiles/kml/shapes/flag.png")
  end if
  if (IconName.eq."rainy") then
    return("http://maps.google.com/mapfiles/kml/shapes/rainy.png")
  end if
  if (IconName.eq."water") then
    return("http://maps.google.com/mapfiles/kml/shapes/water.png")
  end if
  if (IconName.eq."snowflake_simple") then
    return("http://maps.google.com/mapfiles/kml/shapes/snowflake_simple.png")
  end if
  if (IconName.eq."marina") then
    return("http://maps.google.com/mapfiles/kml/shapes/marina.png")
  end if
  if (IconName.eq."fishing") then
    return("http://maps.google.com/mapfiles/kml/shapes/fishing.png")
  end if
  if (IconName.eq."sailing") then
    return("http://maps.google.com/mapfiles/kml/shapes/sailing.png")
  end if
  if (IconName.eq."swimming") then
    return("http://maps.google.com/mapfiles/kml/shapes/swimming.png")
  end if
  if (IconName.eq."ski") then
    return("http://maps.google.com/mapfiles/kml/shapes/ski.png")
  end if
  if (IconName.eq."parks") then
    return("http://maps.google.com/mapfiles/kml/shapes/parks.png")
  end if
  if (IconName.eq."campfire") then
    return("http://maps.google.com/mapfiles/kml/shapes/campfire.png")
  end if
  if (IconName.eq."picnic") then
    return("http://maps.google.com/mapfiles/kml/shapes/picnic.png")
  end if
  if (IconName.eq."campground") then
    return("http://maps.google.com/mapfiles/kml/shapes/campground.png")
  end if
  if (IconName.eq."ranger_station") then
    return("http://maps.google.com/mapfiles/kml/shapes/ranger_station.png")
  end if
  if (IconName.eq."toilets") then
    return("http://maps.google.com/mapfiles/kml/shapes/toilets.png")
  end if
  if (IconName.eq."poi") then
    return("http://maps.google.com/mapfiles/kml/shapes/poi.png")
  end if
  if (IconName.eq."hiker") then
    return("http://maps.google.com/mapfiles/kml/shapes/hiker.png")
  end if
  if (IconName.eq."cycling") then
    return("http://maps.google.com/mapfiles/kml/shapes/cycling.png")
  end if
  if (IconName.eq."motorcycling") then
    return("http://maps.google.com/mapfiles/kml/shapes/motorcycling.png")
  end if
  if (IconName.eq."horsebackriding") then
    return("http://maps.google.com/mapfiles/kml/shapes/horsebackriding.png")
  end if
  if (IconName.eq."play") then
    return("http://maps.google.com/mapfiles/kml/shapes/play.png")
  end if
  if (IconName.eq."golf") then
    return("http://maps.google.com/mapfiles/kml/shapes/golf.png")
  end if
  if (IconName.eq."trail") then
    return("http://maps.google.com/mapfiles/kml/shapes/trail.png")
  end if
  if (IconName.eq."shopping") then
    return("http://maps.google.com/mapfiles/kml/shapes/shopping.png")
  end if
  if (IconName.eq."movies") then
    return("http://maps.google.com/mapfiles/kml/shapes/movies.png")
  end if
  if (IconName.eq."convenience") then
    return("http://maps.google.com/mapfiles/kml/shapes/convenience.png")
  end if
  if (IconName.eq."grocery") then
    return("http://maps.google.com/mapfiles/kml/shapes/grocery.png")
  end if
  if (IconName.eq."arts") then
    return("http://maps.google.com/mapfiles/kml/shapes/arts.png")
  end if
  if (IconName.eq."homegardenbusiness") then
    return("http://maps.google.com/mapfiles/kml/shapes/homegardenbusiness.png")
  end if
  if (IconName.eq."electronics") then
    return("http://maps.google.com/mapfiles/kml/shapes/electronics.png")
  end if
  if (IconName.eq."mechanic") then
    return("http://maps.google.com/mapfiles/kml/shapes/mechanic.png")
  end if
  if (IconName.eq."gas_stations") then
    return("http://maps.google.com/mapfiles/kml/shapes/gas_stations.png")
  end if
  if (IconName.eq."realestate") then
    return("http://maps.google.com/mapfiles/kml/shapes/realestate.png")
  end if
  if (IconName.eq."salon") then
    return("http://maps.google.com/mapfiles/kml/shapes/salon.png")
  end if
  if (IconName.eq."dollar") then
    return("http://maps.google.com/mapfiles/kml/shapes/dollar.png")
  end if
  if (IconName.eq."euro") then
    return("http://maps.google.com/mapfiles/kml/shapes/euro.png")
  end if
  if (IconName.eq."yen") then
    return("http://maps.google.com/mapfiles/kml/shapes/yen.png")
  end if
  if (IconName.eq."firedept") then
    return("http://maps.google.com/mapfiles/kml/shapes/firedept.png")
  end if
  if (IconName.eq."hospitals") then
    return("http://maps.google.com/mapfiles/kml/shapes/hospitals.png")
  end if
  if (IconName.eq."lodging") then
    return("http://maps.google.com/mapfiles/kml/shapes/lodging.png")
  end if
  if (IconName.eq."phone") then
    return("http://maps.google.com/mapfiles/kml/shapes/phone.png")
  end if
  if (IconName.eq."caution") then
    return("http://maps.google.com/mapfiles/kml/shapes/caution.png")
  end if
  if (IconName.eq."earthquake") then
    return("http://maps.google.com/mapfiles/kml/shapes/earthquake.png")
  end if
  if (IconName.eq."falling_rocks") then
    return("http://maps.google.com/mapfiles/kml/shapes/falling_rocks.png")
  end if
  if (IconName.eq."post_office") then
    return("http://maps.google.com/mapfiles/kml/shapes/post_office.png")
  end if
  if (IconName.eq."police") then
    return("http://maps.google.com/mapfiles/kml/shapes/police.png")
  end if
  if (IconName.eq."sunny") then
    return("http://maps.google.com/mapfiles/kml/shapes/sunny.png")
  end if
  if (IconName.eq."partly_cloudy") then
    return("http://maps.google.com/mapfiles/kml/shapes/partly_cloudy.png")
  end if
  if (IconName.eq."volcano") then
    return("http://maps.google.com/mapfiles/kml/shapes/volcano.png")
  end if
  if (IconName.eq."camera") then
    return("http://maps.google.com/mapfiles/kml/shapes/camera.png")
  end if
  if (IconName.eq."webcam") then
    return("http://maps.google.com/mapfiles/kml/shapes/webcam.png")
  end if

  print("The IconName is not recognized.")
  return(IconName);
end

;=========================================================================================
; ones: returns a variable of type VarType and dimension specified in
; VarDims with all entries set to 1
;=========================================================================================
undef("ones")
function ones(VarDims[*]:integer,VarType[1]:string)
local tmp
begin
  tmp=new(VarDims,VarType)
  tmp=totypeof(1.0,VarType)
  return(tmp)
end

;=========================================================================================
; zeros: returns a variable of type VarType and dimension specified in
; VarDims with all entries set to 0
;=========================================================================================
undef("zeros")
function zeros(VarDims[*]:integer,VarType[1]:string)
local tmp
begin
  tmp=new(VarDims,VarType)
  tmp=totypeof(0.0,VarType)
  return(tmp)
end

;=========================================================================================
; dim_dimsizes: returns the length on the given dimension. The dimension
; numbers starts from 1. If the requested dimension is zero, negative, or 
; non-existance, -1 is returned.
;=========================================================================================
undef("dim_dimsizes")
function dim_dimsizes(inData,dim[1]:integer)
local tmpDims
begin
  tmpDims=dimsizes(inData);
  if (dim.gt.dimsizes(tmpDims) .or. dim.le.0) then
    return(-1);
  else
    return(tmpDims(dim-1));
  end if
end

;=========================================================================================
; Gray2RGBA: converts a gray scale image into Red,Green,Blue,Alpha Map
; The transparency for the missing data is set to full transparent.
;=========================================================================================
undef("Gray2RGBA")
function Gray2RGBA(inData[*]:double,inOpt[1]:logical)
local Opt, cmap, ncolors, tmpcmap, cIndex, nDataPoints, RGBA
begin
  Opt=inOpt;
  ; Setting the default values for options
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@ColorMap=get_att_value(Opt,"ColorMap","WhiteBlueGreenYellowRed");
  Opt@MinData=get_att_value(Opt,"MinData",min(inData));
  Opt@MaxData=get_att_value(Opt,"MaxData",max(inData));
  Opt@ReverseColorMap=get_att_value(Opt,"ReverseColorMap",False);
  Opt@UseLogScale=get_att_value(Opt,"UseLogScale",False);
  
  if (Opt@Verbose) then
    print("Reading the color map: "+Opt@ColorMap)
  end if
  cmap = read_colormap_file(Opt@ColorMap)
  ncolors=dim_dimsizes(cmap,1)
  
  ; reversing the color map if requested.
  if (Opt@ReverseColorMap) then
    if (Opt@Verbose) then
      print("Reversing the colormap ...")
    end if
    tmpcmap=new((/ncolors,4/),typeof(cmap))
    do i=0,ncolors-1
      tmpcmap(i,:)=cmap(ncolors-1-i,:)
    end do
    cmap=tmpcmap;
    delete(tmpcmap);
  end if
  
  if (Opt@Verbose) then
    print("MinData: "+Opt@MinData+", MaxData: "+Opt@MaxData)
    print("Number of colors in the color map: " + ncolors)
  end if

  ; converting the data to color index
  if (Opt@UseLogScale) then
    cIndex=where(ismissing(inData),0,round((log(inData)-log(Opt@MinData))/(log(Opt@MaxData)-log(Opt@MinData))*(ncolors-1),3))
  else
    cIndex=where(ismissing(inData),0,round((inData-Opt@MinData)/(Opt@MaxData-Opt@MinData)*(ncolors-1),3))
  end if
  cIndex=where(cIndex.lt.0,0,cIndex);
  cIndex=where(cIndex.ge.ncolors,ncolors-1,cIndex);

  ; retrieving the RGBA fields
  nDataPoints=dimsizes(inData);
  RGBA=new((/4,nDataPoints/),"integer")
  do i=0,3
    RGBA(i,:)=round(cmap(cIndex,i)*255.0,3)
  end do
  
  ; Making out of bound data and the missing data completely transparent
  RGBA(3,:)=where((inData.lt.Opt@MinData).or.(inData.gt.Opt@MaxData).or.ismissing(inData),0,RGBA(3,:));

  ; Making NoData values completely transparent if requested
  if (isatt(Opt,"NoData")) then
  	RGBA(3,:)=where(inData.eq.Opt@NoData,0,RGBA(3,:));
  end if

  return(RGBA)
end

;=========================================================================================
; Gray2cIndex: converts a gray scale image into a colormap index.
; NOTE: if there is any missing data, -1 is returned as color index.
;=========================================================================================
undef("Gray2cIndex")
function Gray2cIndex(inData[*]:double,inOpt[1]:logical)
local Opt, cmap, ncolors, cIndex
begin
  Opt=inOpt;
  ; Setting the default values for options
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@ColorMap=get_att_value(Opt,"ColorMap","WhiteBlueGreenYellowRed");
  Opt@MinData=get_att_value(Opt,"MinData",min(inData));
  Opt@MaxData=get_att_value(Opt,"MaxData",max(inData));
  
  if (Opt@Verbose) then
    print("Reading the color map: "+Opt@ColorMap)
  end if
  cmap = read_colormap_file(Opt@ColorMap)
  ncolors=dim_dimsizes(cmap,1)
  if (Opt@Verbose) then
    print("Number of colors in the color map: " + ncolors)
  end if

  ; converting the data to color index
  cIndex=round((inData-Opt@MinData)/(Opt@MaxData-Opt@MinData)*(ncolors-1),3);
  cIndex=where(cIndex.lt.0,0,cIndex);
  cIndex=where(cIndex.ge.ncolors,ncolors-1,cIndex);
  cIndex=where(ismissing(inData),-1,cIndex);

  ; Identifying NoData values if requested
  if (isatt(Opt,"NoData")) then
  	cIndex=where(inData.eq.Opt@NoData,-1,cIndex);
  end if
  
  return(cIndex)
end

;=========================================================================================
; ToHex: converts a number into hexasystem
;=========================================================================================
undef("ToHex")
function ToHex(inData[1]:numeric)
local HexDigits, Value, OutData
begin
  HexDigits=tochar("0123456789abcdef")
  Value=todouble(inData)
  OutData="";
  do while (Value.ge.16.0)
    OutData=tostring(HexDigits(tointeger(Value)%16))+OutData
    Value=floor(Value/16.0d)
  end do
  OutData=tostring(HexDigits(tointeger(Value)%16))+OutData
  return(OutData)
end

;*****************************************************************************************
; Low Level KML functions/Procedures
; These functions/procedures can be used to add a required KML field to a file.
; Then can be used to generate various KML files.
;*****************************************************************************************

;=========================================================================================
; add_KML_Description
;=========================================================================================
undef("add_KML_Description")
procedure add_KML_Description(FileName[1]:string, DescStr[1]:string)
begin
  system("echo " + str_get_dq() +"<description> " + \
                                  DescStr + \
                                 " </description>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Address
;=========================================================================================
undef("add_KML_Address")
procedure add_KML_Address(FileName[1]:string, AddressStr[1]:string)
begin
  system("echo " + str_get_dq() +"<address> " + \
                                  AddressStr + \
                                 " </address>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Range
;=========================================================================================
undef("add_KML_Range")
procedure add_KML_Range(FileName[1]:string, Range[1]:numeric)
begin
  system("echo " + str_get_dq() +"<range> " + \
                                  Range + \
                                 " </range>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Name will
;=========================================================================================
undef("add_KML_Name")
procedure add_KML_Name(FileName[1]:string, NameString[1]:string)
begin
  system("echo " + str_get_dq() +"<name> " + \
                                  NameString + \
                                 " </name>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Coordinates
;=========================================================================================
undef("add_KML_Coordinates")
procedure add_KML_Coordinates(FileName[1]:string, \
                        lat[*]:numeric, \
                        lon[*]:numeric, \
                        h[*]:numeric)
local CoordsStr
begin
  system("echo " + str_get_dq() +"<coordinates>" + str_get_dq() + " >> " + FileName)
  CoordsStr=tostring(lon)+","+tostring(lat)+","+tostring(h)
  asciiwrite("KML_Coordinates_tmp",CoordsStr)
  system("cat KML_Coordinates_tmp >> " + FileName)
  system("rm -rf KML_Coordinates_tmp")
  system("echo " + str_get_dq() +"</coordinates>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Width
;=========================================================================================
undef("add_KML_Width")
procedure add_KML_Width(FileName[1]:string, WidthValue[1]:numeric)
begin
  system("echo " + str_get_dq() +"<width> " + \
                                  WidthValue + \
                                 " </width>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Open
;=========================================================================================
undef("add_KML_Open")
procedure add_KML_Open(FileName[1]:string, Yes[1]:logical)
begin
  if (Yes) then
   system("echo " + str_get_dq() + "<open>1</open>" + str_get_dq() + " >> " + FileName)
  else 
   system("echo " + str_get_dq() + "<open>0</open>" + str_get_dq() + " >> " + FileName)
  end if
end

;=========================================================================================
; add_KML_FlyToView
;=========================================================================================
undef("add_KML_FlyToView")
procedure add_KML_FlyToView(FileName[1]:string, Yes[1]:logical)
begin
  if (Yes) then
   system("echo " + str_get_dq() + "<flyToView>1</flyToView>" + str_get_dq() + " >> " + FileName)
  else 
   system("echo " + str_get_dq() + "<flyToView>0</flyToView>" + str_get_dq() + " >> " + FileName)
  end if
end

;=========================================================================================
; add_KML_Extrude
;=========================================================================================
undef("add_KML_Extrude")
procedure add_KML_Extrude(FileName[1]:string, Yes[1]:logical)
begin
  if (Yes) then
   system("echo " + str_get_dq() + "<extrude>1</extrude>" + str_get_dq() + " >> " + FileName)
  else 
   system("echo " + str_get_dq() + "<extrude>0</extrude>" + str_get_dq() + " >> " + FileName)
  end if
end

;=========================================================================================
; add_KML_Visibility
;=========================================================================================
undef("add_KML_Visibility")
procedure add_KML_Visibility(FileName[1]:string, Yes[1]:logical)
begin
  if (Yes) then
   system("echo " + str_get_dq() + "<visibility>1</visibility>" + str_get_dq() + " >> " + FileName)
  else 
   system("echo " + str_get_dq() + "<visibility>0</visibility>" + str_get_dq() + " >> " + FileName)
  end if
end

;=========================================================================================
; add_KML_Tessellate
;=========================================================================================
undef("add_KML_Tessellate")
procedure add_KML_Tessellate(FileName[1]:string, Yes[1]:logical)
begin
  if (Yes) then
   system("echo " + str_get_dq() + "<tessellate>1</tessellate>" + str_get_dq() + " >> " + FileName)
  else 
   system("echo " + str_get_dq() + "<tessellate>0</tessellate>" + str_get_dq() + " >> " + FileName)
  end if
end

;=========================================================================================
; add_KML_Fill
;=========================================================================================
undef("add_KML_Fill")
procedure add_KML_Fill(FileName[1]:string, Yes[1]:logical)
begin
  if (Yes) then
   system("echo " + str_get_dq() + "<fill>1</fill>" + str_get_dq() + " >> " + FileName)
  else 
   system("echo " + str_get_dq() + "<fill>0</fill>" + str_get_dq() + " >> " + FileName)
  end if
end

;=========================================================================================
; add_KML_Outline
;=========================================================================================
undef("add_KML_Outline")
procedure add_KML_Outline(FileName[1]:string, Yes[1]:logical)
begin
  if (Yes) then
   system("echo " + str_get_dq() + "<outline>1</outline>" + str_get_dq() + " >> " + FileName)
  else 
   system("echo " + str_get_dq() + "<outline>0</outline>" + str_get_dq() + " >> " + FileName)
  end if
end

;=========================================================================================
; add_KML_Altitude
;=========================================================================================
undef("add_KML_Altitude")
procedure add_KML_Altitude(FileName[1]:string, Altitude[1]:numeric)
begin
  system("echo " + str_get_dq() +"<altitude> " + \
                                  Altitude + \
                                 " </altitude>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_AltMode
;=========================================================================================
undef("add_KML_AltMode")
procedure add_KML_AltMode(FileName[1]:string, AltMode[1]:string)
begin
  if (AltMode.eq."absolute") then
   system("echo " + str_get_dq() + "<altitudeMode>absolute</altitudeMode>" + str_get_dq() + " >> " + FileName)
  else if (AltMode.eq."clampToGround") then
   system("echo " + str_get_dq() + "<altitudeMode>clampToGround</altitudeMode>" + str_get_dq() + " >> " + FileName)
  else if (AltMode.eq."clampToSeaFloor") then
   system("echo " + str_get_dq() + "<altitudeMode>clampToSeaFloor</altitudeMode>" + str_get_dq() + " >> " + FileName)
  else if (AltMode.eq."relativeToGround") then
   system("echo " + str_get_dq() + "<altitudeMode>relativeToGround</altitudeMode>" + str_get_dq() + " >> " + FileName)
  else if (AltMode.eq."relativeToSeaFloor") then
   system("echo " + str_get_dq() + "<altitudeMode>relativeToSeaFloor</altitudeMode>" + str_get_dq() + " >> " + FileName)
  else
    print("Requested Altitude Mode: " + AltMode)
    print("Altitude Mode not Supported")
    exit
  end if
  end if
  end if
  end if
  end if
end

;=========================================================================================
; add_KML_Color
;=========================================================================================
undef("add_KML_Color")
procedure add_KML_Color(FileName[1]:string, \
                        Opacity[1]:integer, \
                        RedColor[1]:integer, \
                        BlueColor[1]:integer, \
                        GreenColor[1]:integer)
local rr, gg, bb, aa, bOpacity
begin
  if ( Opacity.ne.-1 .and. .not.(Opacity.ge.0 .and. Opacity.le.100) ) then
    print("Opacity: "+Opacity)
    print("ERROR: Opacity must be between 0 and 100")
    exit
  end if
  if (RedColor.gt.255 .or. RedColor.lt.0) then
    print("ERROR: RedColor must be between 0 and 255")
    exit
  end if
  if (BlueColor.gt.255 .or. BlueColor.lt.0) then
    print("ERROR: BlueColor must be between 0 and 255")
    exit
  end if
  if (GreenColor.gt.255 .or. GreenColor.lt.0) then
    print("ERROR: GreenColor must be between 0 and 255")
    exit
  end if
  
  if (RedColor.gt.15) then
    rr=ToHex(RedColor)
  else
    rr="0"+ToHex(RedColor)
  end if

  if (GreenColor.gt.15) then
    gg=ToHex(GreenColor)
  else
    gg="0"+ToHex(GreenColor)
  end if
  
  if (BlueColor.gt.15) then
    bb=ToHex(BlueColor)
  else
    bb="0"+ToHex(BlueColor)
  end if
    
  if (Opacity.eq.-1) then
    bOpacity=255;
  else
    bOpacity=round(Opacity*255.0/100.0,3)
  end if
  if (bOpacity.gt.15) then
    aa=ToHex(bOpacity)
  else
    aa="0"+ToHex(bOpacity)
  end if
  
  system("echo " + str_get_dq() +"<color> " + \
                                  aa + bb + gg + rr + \
                                 " </color>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_North
;=========================================================================================
undef("add_KML_North")
procedure add_KML_North(FileName[1]:string, North[1]:numeric)
begin
  system("echo " + str_get_dq() +"<north> " + \
                                  North + \
                                 " </north>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_South
;=========================================================================================
undef("add_KML_South")
procedure add_KML_South(FileName[1]:string, South[1]:numeric)
begin
  system("echo " + str_get_dq() +"<south> " + \
                                  South + \
                                 " </south>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_East
;=========================================================================================
undef("add_KML_East")
procedure add_KML_East(FileName[1]:string, East[1]:numeric)
begin
  system("echo " + str_get_dq() +"<east> " + \
                                  East + \
                                 " </east>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_West
;=========================================================================================
undef("add_KML_West")
procedure add_KML_West(FileName[1]:string, West[1]:numeric)
begin
  system("echo " + str_get_dq() +"<west> " + \
                                  West + \
                                 " </west>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Rotation
;=========================================================================================
undef("add_KML_Rotation")
procedure add_KML_Rotation(FileName[1]:string, Rotation[1]:numeric)
begin
  system("echo " + str_get_dq() +"<rotation> " + \
                                  Rotation + \
                                 " </rotation>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_StyleUrl
;=========================================================================================
undef("add_KML_StyleUrl")
procedure add_KML_StyleUrl(FileName[1]:string, StyleID[1]:string)
begin
  system("echo " + str_get_dq() +"<styleUrl> " + \
                                  StyleID + \
                                 " </styleUrl>" + str_get_dq() + " >> " + FileName)
end
    
;=========================================================================================
; add_KML_HideChildrenStyle
;=========================================================================================
undef("add_KML_HideChildrenStyle")
procedure add_KML_HideChildrenStyle(FileName[1]:string)
begin
  system("echo " + str_get_dq() + \
         "<Style id=\"+str_get_dq()+"check-hide-children\"+str_get_dq()+ \
         "><ListStyle><listItemType>checkHideChildren</listItemType></ListStyle></Style>" \
         + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; hide_KML_Children
;=========================================================================================
undef("hide_KML_Children")
procedure hide_KML_Children(FileName[1]:string)
begin
  add_KML_StyleUrl(FileName,"#check-hide-children")
end

;=========================================================================================
; add_KML_Href will add required codes for a Href in a KML file
;=========================================================================================
undef("add_KML_Href")
procedure add_KML_Href(FileName[1]:string, hrefLink[1]:string)
begin
  system("echo " + str_get_dq() +"<href> " + \
                                  hrefLink + \
                                 " </href>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Icon will add required codes for a Icon in a KML file
;=========================================================================================
undef("add_KML_Icon")
procedure add_KML_Icon(FileName[1]:string, hrefLink[1]:string)
begin
  system("echo " + str_get_dq() +"<Icon>" + str_get_dq() + " >> " + FileName)
  add_KML_Href(FileName, hrefLink)
  system("echo " + str_get_dq() +"</Icon>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Heading will add required codes for a Icon in a KML file
;=========================================================================================
undef("add_KML_Heading")
procedure add_KML_Heading(FileName[1]:string, Heading[1]:numeric)
begin
  system("echo " + str_get_dq() +"<heading> " + \
                                  Heading + \
                                 " </heading>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_TimeSpan
;=========================================================================================
undef("add_KML_TimeSpan")
procedure add_KML_TimeSpan(FileName[1]:string, \
                           Begin_TimeTagStr[1]:string, \
                           End_TimeTagStr[1]:string)
begin
  system("echo " + str_get_dq() +"<TimeSpan><begin>" + \
                                  Begin_TimeTagStr + \
                                 "</begin><end>" + \
                                  End_TimeTagStr + \
                                 "</end></TimeSpan>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_TimeStamp_When will add required codes for a 
; add_KML_TimeStamp_When in a KML file
;=========================================================================================
undef("add_KML_TimeStamp_When")
procedure add_KML_TimeStamp_When(FileName[1]:string, TimeTagStr[1]:string)
begin
  system("echo " + str_get_dq() +"<TimeStamp><when>" + \
                                  TimeTagStr + \
                                 "</when></TimeStamp>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_LineStyle
;=========================================================================================
undef("add_KML_LineStyle")
procedure add_KML_LineStyle(FileName[1]:string, \
                            inOpt[1]:logical)
local Opt
begin
  Opt=inOpt;
  Opt@Opacity=get_att_value(Opt,"Opacity",100);
  Opt@Red=get_att_value(Opt,"Red",255);
  Opt@Blue=get_att_value(Opt,"Blue",0);
  Opt@Green=get_att_value(Opt,"Green",255);
  Opt@Width=get_att_value(Opt,"Width",1);

  system("echo " + str_get_dq() +"<LineStyle>" + str_get_dq() + " >> " + FileName)
  add_KML_Color(FileName, Opt@Opacity, Opt@Red, Opt@Blue,Opt@Green)
  add_KML_Width(FileName, Opt@Width)
  system("echo " + str_get_dq() +"</LineStyle>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_IconStyle
;=========================================================================================
undef("add_KML_IconStyle")
procedure add_KML_IconStyle(FileName[1]:string, \
                            inOpt[1]:logical)
local Opt
begin
  Opt=inOpt
  Opt@Opacity=get_att_value(Opt,"Opacity",100);
  Opt@Red=get_att_value(Opt,"Red",255);
  Opt@Blue=get_att_value(Opt,"Blue",0);
  Opt@Green=get_att_value(Opt,"Green",255);
  Opt@Width=get_att_value(Opt,"Width",1);
  Opt@hrefLink=get_att_value(Opt,"hrefLink","ylw-pushpin");
  system("echo " + str_get_dq() +"<IconStyle>" + str_get_dq() + " >> " + FileName)
  add_KML_Color(FileName, Opt@Opacity, Opt@Red, Opt@Blue,Opt@Green)
  if (isatt(Opt,"Heading")) then
  	add_KML_Heading(FileName,Opt@Heading);
  end if
  add_KML_Icon(FileName, Opt@hrefLink);
  system("echo " + str_get_dq() +"</IconStyle>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_PolyStyle
;=========================================================================================
undef("add_KML_PolyStyle")
procedure add_KML_PolyStyle(FileName[1]:string, \
                            inOpt[1]:logical)
local Opt
begin
  Opt=inOpt
  Opt@Opacity=get_att_value(Opt,"Opacity",100);
  Opt@Red=get_att_value(Opt,"Red",255);
  Opt@Blue=get_att_value(Opt,"Blue",0);
  Opt@Green=get_att_value(Opt,"Green",255);
  Opt@Fill=get_att_value(Opt,"Fill",True);
  Opt@ShowOutline=get_att_value(Opt,"ShowOutline",True);
  
  system("echo " + str_get_dq() +"<PolyStyle>" + str_get_dq() + " >> " + FileName)
  add_KML_Color(FileName, Opt@Opacity, Opt@Red, Opt@Blue,Opt@Green)
  add_KML_Fill(FileName, Opt@Fill)
  add_KML_Outline(FileName, Opt@ShowOutline)
  system("echo " + str_get_dq() +"</PolyStyle>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_LineString
;=========================================================================================
undef("add_KML_LineString")
procedure add_KML_LineString(FileName[1]:string, \
                           Lat[*]:numeric, \
                           Lon[*]:numeric, \
                           h[*]:numeric, \
                           AltMode[1]:string, \
                           Extrude[1]:logical, \
                           Tesselate[1]:logical)
begin
  system("echo " + str_get_dq() +"<LineString>" + str_get_dq() + " >> " + FileName)
  add_KML_Extrude(FileName, Extrude)
  add_KML_Tessellate(FileName, Tesselate)
  add_KML_AltMode(FileName,AltMode)
  add_KML_Coordinates(FileName,Lat,Lon,h)
  system("echo " + str_get_dq() +"</LineString>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_LatLonBox will add required codes for a LatLonBox in a KML 
; file
;=========================================================================================
undef("add_KML_LatLonBox")
procedure add_KML_LatLonBox(FileName[1]:string, \
                           North[1]:numeric, \
                           South[1]:numeric, \
                           West[1]:numeric, \
                           East[1]:numeric, \
                           Rotation[1]:numeric)
begin
  system("echo " + str_get_dq() +"<LatLonBox>" + str_get_dq() + " >> " + FileName)
  add_KML_North(FileName, North)
  add_KML_South(FileName, South)
  add_KML_West(FileName, West)
  add_KML_East(FileName, East)
  add_KML_Rotation(FileName, Rotation)
  system("echo " + str_get_dq() +"</LatLonBox>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_LinearRing
;=========================================================================================
undef("add_KML_LinearRing")
procedure add_KML_LinearRing(FileName[1]:string,lat[*]:numeric, lon[*]:numeric, h[*]:numeric)
begin
  system("echo " + str_get_dq() +"<LinearRing>" + str_get_dq() + " >> " + FileName)
  add_KML_Coordinates(FileName,lat,lon,h)
  system("echo " + str_get_dq() +"</LinearRing>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_OuterBoundaryIs
;=========================================================================================
undef("add_KML_OuterBoundaryIs")
procedure add_KML_OuterBoundaryIs(FileName[1]:string, \
                                  lat[*]:numeric, lon[*]:numeric, h[*]:numeric)
begin
  system("echo " + str_get_dq() +"<outerBoundaryIs>" + str_get_dq() + " >> " + FileName)
  add_KML_LinearRing(FileName,lat, lon, h)
  system("echo " + str_get_dq() +"</outerBoundaryIs>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_InnerBoundaryIs
;=========================================================================================
undef("add_KML_InnerBoundaryIs")
procedure add_KML_InnerBoundaryIs(FileName[1]:string, \
                                  lat[*]:numeric, lon[*]:numeric, h[*]:numeric)
begin
  system("echo " + str_get_dq() +"<innerBoundaryIs>" + str_get_dq() + " >> " + FileName)
  add_KML_LinearRing(FileName,lat, lon, h)
  system("echo " + str_get_dq() +"</innerBoundaryIs>" + str_get_dq() + " >> " + FileName)
end
  
;=========================================================================================
; add_KML_GOverlay will add required codes for a Ground Overlay in 
; a KML file
;=========================================================================================
undef("add_KML_GOverlay")
procedure add_KML_GOverlay(FileName[1]:string, \
                           hrefLink[1]:string, \
                           inOpt[1]:logical)
local Opt
begin
  Opt=inOpt
  system("echo " + str_get_dq() +"<GroundOverlay>" + str_get_dq() + " >> " + FileName)
  Opt@Opacity=get_att_value(Opt,"Opacity",100);
  
  add_KML_Color(FileName, Opt@Opacity, 255, 255, 255)
  
  ; Setting the ground overlay figure
  add_KML_Icon(FileName, hrefLink)
  
  ; Setting the altitude and its mode
  Opt@Altitude=get_att_value(Opt,"Altitude",0);
  if (Opt@Altitude.eq.0) then
    add_KML_AltMode(FileName,"clampToGround");
  else
    if (isatt(Opt,"AltMode")) then
      add_KML_AltMode(FileName,Opt@AltMode);
    else
      add_KML_AltMode(FileName,"absolute");
    end if
    add_KML_Altitude(FileName,Opt@Altitude);
  end if
  
  ; Adding the bounding box of the figure.
  add_KML_LatLonBox(FileName, Opt@North, Opt@South, Opt@West, Opt@East, Opt@Rotation)
  
  ; Adding time stamp or span
  ; but first checking if it is consistent.
  if (isatt(Opt,"TimeTagStr") .and. \
      (isatt(Opt,"Begin_TimeTagStr") .or. \
      isatt(Opt,"End_TimeTagStr")) )then
      print("Either TimeTagStr must be provided or Begin/End_TimeTagStr, but not both")
      exit
  end if
  if (isatt(Opt,"TimeTagStr") )then
    add_KML_Name(FileName,Opt@TimeTagStr)
    add_KML_TimeStamp_When(FileName, Opt@TimeTagStr)
  end if
  if (isatt(Opt,"Begin_TimeTagStr").and.isatt(Opt,"End_TimeTagStr") ) then
    add_KML_Name(FileName,Opt@Begin_TimeTagStr)
    add_KML_TimeSpan(FileName, Opt@Begin_TimeTagStr, Opt@End_TimeTagStr)
  end if
  
  ; finalizing the ground overlay
  system("echo " + str_get_dq() +"</GroundOverlay>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_Polygon
;=========================================================================================
undef("add_KML_Polygon")
procedure add_KML_Polygon(FileName[1]:string, \
                        lat[*]:numeric, lon[*]:numeric, h[*]:numeric, \
                        inOpt[1]:logical)
local Opt
begin
  Opt=inOpt
  Opt@Extrude=get_att_value(Opt,"Extrude",1);
  Opt@AltMode=get_att_value(Opt,"AltMode","absolute");
  Opt@Tesselate=get_att_value(Opt,"Tesselate",1);
  system("echo " + str_get_dq() +"<Polygon>" + str_get_dq() + " >> " + FileName)
  add_KML_Extrude(FileName,Opt@Extrude)
  add_KML_AltMode(FileName,Opt@AltMode)
  add_KML_Tessellate(FileName, Opt@Tesselate)
  add_KML_OuterBoundaryIs(FileName,lat, lon, h)
  system("echo " + str_get_dq() +"</Polygon>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_nSidedPolygon
;=========================================================================================
undef("add_KML_nSidedPolygon")
procedure add_KML_nSidedPolygon(FileName[1]:string, \
                                lat[1]:numeric, lon[1]:numeric, h[1]:numeric, \
                                nSide[1]:integer, Radius[1]:numeric,
                                inOpt[1]:logical)
local Opt, pLat, pLon, tmpLat, tmpLon, Theta
begin
  Opt=inOpt
  if (nSide.lt.3) then
    print("nSide must be bigger than or equal to three.")
    exit
  end if
  ; Getting Default Options
  Opt@Extrude=get_att_value(Opt,"Extrude",0);
  Opt@AltMode=get_att_value(Opt,"AltMode","relativeToGround");
  Opt@Tesselate=get_att_value(Opt,"Tesselate",1);
  Opt@Rotation=get_att_value(Opt,"Rotation",0.0d);
  
  ; Forming the corners of the polygon
  pLat=zeros(nSide+1,"double")
  pLon=zeros(nSide+1,"double")
  tmpLat=0.0d;
  tmpLon=0.0d;
  Theta=todouble(fspan(0.0,360.0,nSide+1)+Opt@Rotation)
  do i=0,nSide
    directVincenty(todouble(lat),todouble(lon),Theta(i),todouble(Radius), \
                   tmpLat,tmpLon,0.0d,0.0d);
		; NOTE: for some reason if I pass pLat(i) and pLon(i) to above function it wont work.
		; therefore, I am using the intermediate variables, tmpLat and tmpLon.
    pLat(i)=tmpLat
    pLon(i)=tmpLon
  end do
  
  system("echo " + str_get_dq() +"<Polygon>" + str_get_dq() + " >> " + FileName)
  add_KML_Extrude(FileName,Opt@Extrude)
  add_KML_AltMode(FileName,Opt@AltMode)
  add_KML_Tessellate(FileName, Opt@Tesselate)
  add_KML_OuterBoundaryIs(FileName,pLat, pLon, h*ones(nSide+1,"double"))
  system("echo " + str_get_dq() +"</Polygon>" + str_get_dq() + " >> " + FileName)
end


;=========================================================================================
; add_KML_Point
;=========================================================================================
undef("add_KML_Point")
procedure add_KML_Point(FileName[1]:string, \
                        lat[1]:numeric, lon[1]:numeric, \
                        inOpt[1]:logical)
local Opt
begin
  Opt=inOpt
  Opt@Extrude=get_att_value(Opt,"Extrude",1);
  Opt@AltMode=get_att_value(Opt,"AltMode","clampToGround");
  Opt@Height=get_att_value(Opt,"Height",0.0);
  system("echo " + str_get_dq() +"<Point>" + str_get_dq() + " >> " + FileName)
  add_KML_Extrude(FileName,Opt@Extrude)
  add_KML_AltMode(FileName,Opt@AltMode)
  add_KML_Coordinates(FileName,lat,lon,Opt@Height)
  system("echo " + str_get_dq() +"</Point>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Open_KML_MultiGeometry will add required codes to open a Folder in a KML file
;=========================================================================================
undef("Open_KML_MultiGeometry")
procedure Open_KML_MultiGeometry(FileName[1]:string)
begin
  system("echo " + str_get_dq() +"<MultiGeometry>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Close_KML_MultiGeometry will add required codes to open a Folder in a KML file
;=========================================================================================
undef("Close_KML_MultiGeometry")
procedure Close_KML_MultiGeometry(FileName[1]:string)
begin
  system("echo " + str_get_dq() +"</MultiGeometry>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Open_KML_Link 
;=========================================================================================
undef("Open_KML_Link")
procedure Open_KML_Link(FileName[1]:string)
begin
    system("echo " + str_get_dq() +"<Link>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Close_KML_Link 
;=========================================================================================
undef("Close_KML_Link")
procedure Close_KML_Link(FileName[1]:string)
begin
    system("echo " + str_get_dq() +"</Link>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Open_KML_NetworkLink 
;=========================================================================================
undef("Open_KML_NetworkLink")
procedure Open_KML_NetworkLink(FileName[1]:string, IDStr[1]:string)
begin
    system("echo " + str_get_dq() +"<NetworkLink id=\"+str_get_dq()+IDStr+"\"+str_get_dq()+ ">"+str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Close_KML_NetworkLink 
;=========================================================================================
undef("Close_KML_NetworkLink")
procedure Close_KML_NetworkLink(FileName[1]:string)
begin
    system("echo " + str_get_dq() +"</NetworkLink>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Open_KML_Folder
;=========================================================================================
undef("Open_KML_Folder")
procedure Open_KML_Folder(FileName[1]:string, NameString[1]:string)
begin
  system("echo " + str_get_dq() +"<Folder>" + str_get_dq() + " >> " + FileName)
  add_KML_Name(FileName, NameString)
end

;=========================================================================================
; Close_KML_Folder
;=========================================================================================
undef("Close_KML_Folder")
procedure Close_KML_Folder(FileName[1]:string)
begin
  system("echo " + str_get_dq() +"</Folder>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Open_KML_Placemark
;=========================================================================================
undef("Open_KML_Placemark")
procedure Open_KML_Placemark(FileName[1]:string, NameStr[1]:string)
begin
  system("echo " + str_get_dq() +"<Placemark>" + str_get_dq() + " >> " + FileName)
  add_KML_Name(FileName,NameStr)
end

;=========================================================================================
; Close_KML_Placemark
;=========================================================================================
undef("Close_KML_Placemark")
procedure Close_KML_Placemark(FileName[1]:string)
begin
  system("echo " + str_get_dq() +"</Placemark>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Open_KML_Style
;=========================================================================================
undef("Open_KML_Style")
procedure Open_KML_Style(FileName[1]:string, ISStr[1]:string)
begin
  system("echo " + str_get_dq() +"<Style id=\"+str_get_dq()+ISStr+"\"+str_get_dq()+ ">"+str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Close_KML_Style
;=========================================================================================
undef("Close_KML_Style")
procedure Close_KML_Style(FileName[1]:string)
begin
  system("echo " + str_get_dq() +"</Style>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Open_KML_Document
;=========================================================================================
undef("Open_KML_Document")
procedure Open_KML_Document(FileName[1]:string)
begin
  system("echo " + str_get_dq() +"<Document>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; Close_KML_Document
;=========================================================================================
undef("Close_KML_Document")
procedure Close_KML_Document(FileName[1]:string)
begin
  system("echo " + str_get_dq() +"</Document>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; OpenKML will prepare a text file for KML
;=========================================================================================
undef("OpenKML")
procedure OpenKML(FileName[1]:string)
local h1,h2
begin
  ; h1 = <?xml version="1.0" encoding="UTF-8"?>
  ; h2 = <kml xmlns="http://www.opengis.net/kml/2.2">
  h1="<?xml version=\"+str_get_dq()+"1.0\"+str_get_dq()+" encoding=\"+str_get_dq()+"UTF-8\"+str_get_dq()+"?>"
  h2="<kml xmlns=\"+str_get_dq()+"http://www.opengis.net/kml/2.2\"+str_get_dq()+">"
  system("echo "+str_get_dq()+h1+str_get_dq()+" > "+FileName)
  system("echo "+str_get_dq()+h2+str_get_dq()+" >> "+FileName)
  Open_KML_Document(FileName)
  add_KML_Open(FileName,True)
end

;=========================================================================================
; CloseKML
;=========================================================================================
undef("CloseKML")
procedure CloseKML(FileName[1]:string)
begin
  Close_KML_Document(FileName)
  system("echo " + str_get_dq() +"</kml>" + str_get_dq() + " >> " + FileName)
end

;=========================================================================================
; add_KML_ColorMapStyles closes a KML file
;=========================================================================================
undef("add_KML_ColorMapStyles")
function add_KML_ColorMapStyles(FileName[1]:string, inOpt[1]:logical)
local ncolors, StyleIDStr, StyleOpt, cmap
begin
  Opt=inOpt;
  if ( isatt(Opt,"UseConstColor").and.Opt@UseConstColor ) then
    ncolors=2;
    StyleIDStr=new(ncolors,"string")
    StyleIDStr(0)="ColorMapLineStyleID"+0;

    StyleOpt=True
    StyleOpt@Red=get_att_value(Opt,"Red",255);
    StyleOpt@Green=get_att_value(Opt,"Green",255);
    StyleOpt@Blue=get_att_value(Opt,"Blue",0);
    StyleOpt@Opacity=get_att_value(Opt,"Opacity",100);
    StyleOpt@Width=get_att_value(Opt,"Width",1.0);

    Open_KML_Style(FileName,StyleIDStr(0))
    add_KML_LineStyle(FileName,StyleOpt)
    Close_KML_Style(FileName)

    ; Anything uses this styleID is not going to be shown.
    ; Good for missing values
    StyleIDStr(1)="ColorMapLineStyleID1";
    StyleOpt@Red=255
    StyleOpt@Green=255
    StyleOpt@Blue=255
    StyleOpt@Opacity=0;
    Open_KML_Style(FileName,StyleIDStr(1))
    add_KML_LineStyle(FileName,StyleOpt)
    Close_KML_Style(FileName)

  else
    cmap = read_colormap_file(get_att_value(Opt,"ColorMap","WhiteBlueGreenYellowRed"))
    ncolors=dim_dimsizes(cmap,1)
    StyleIDStr=new(ncolors+1,"string")
    StyleOpt=True;
    do i=0,ncolors-1
      StyleIDStr(i)="ColorMapStyleID"+i;
      StyleOpt@Red=round(cmap(i,0)*255,3)
      StyleOpt@Green=round(cmap(i,1)*255,3)
      StyleOpt@Blue=round(cmap(i,2)*255,3)
      StyleOpt@Opacity=round(cmap(i,3)*100,3)
      StyleOpt@Width=Opt@LineWidth;
      Open_KML_Style(FileName,StyleIDStr(i))
      add_KML_LineStyle(FileName,StyleOpt)
      Close_KML_Style(FileName)
    end do
    
    ; Anything uses this styleID is not going to be shown.
    ; Good for missing value
    StyleIDStr(ncolors)="FullTransparentStyleID";
    StyleOpt@Red=255
    StyleOpt@Green=255
    StyleOpt@Blue=255
    StyleOpt@Opacity=0;
    Open_KML_Style(FileName,StyleIDStr(ncolors))
    add_KML_LineStyle(FileName,StyleOpt)
    Close_KML_Style(FileName)
  end if
  
  return(StyleIDStr)
end ; end of add_KML_ColorMapStyles

;*****************************************************************************************
; High Level KML functions/Procedures
; These functions/procedures can be used to export certain types of data onto Google Earth
;*****************************************************************************************

;=========================================================================================
; RasterTimeSeries2KML recieves a time series of structured raster data set and
; exports it to KML file or KMZ file format, to be shown on Google Earth
;=========================================================================================
undef("RasterTimeSeries2KML")
procedure RasterTimeSeries2KML(FileName[1]:string,\
                               RasterData[*][*][*]:numeric, \
                               xCoords[*][*]:numeric, \
                               yCoords[*][*]:numeric, \
                               TimeTag[*]:string, \
                               inOpt[1]:logical)
local Opt, DstCoord, DstCoordStr, data_lon, data_lat, nframe, nlat, nlon, kml_nlat, kml_nlon, \
      PNGOpt, RGBA, kml_FileName, GOverlayOpt, i, hrefLink, timetags_s, \
      timetags_e, dd, hh, mm, ss, StyleIDStr
begin

  ; ***** Checking Opt and setting the default values if needed *****
  Opt=inOpt ; Opt is always effective regardless of being set to true or false
  Opt=True

  Opt@StoragePath=get_att_value(Opt,"StoragePath","./");
  Opt@DataSetTitle=get_att_value(Opt,"DataSetTitle","--");
  Opt@PixelRatio=get_att_value(Opt,"PixelRatio",1.0);
  Opt@CoordUnits=get_att_value(Opt,"CoordUnits","Degrees");
  Opt@InterpWeightFile=get_att_value(Opt,"InterpWeightFile",FileName+"_wgt.nc");
  Opt@KeepInterpWeightFile=get_att_value(Opt,"KeepInterpWeightFile",True);
  Opt@InterpMethod=get_att_value(Opt,"InterpMethod","bilinear");
  Opt@SrcRegional=get_att_value(Opt,"SrcRegional",True);
  Opt@DstRegional=get_att_value(Opt,"DstRegional",True);
  Opt@Rotation=get_att_value(Opt,"Rotation",0);
  Opt@ConstOpacity=get_att_value(Opt,"ConstOpacity",-1);
  Opt@Altitude=get_att_value(Opt,"Altitude",0);
  Opt@IncludeGrid=get_att_value(Opt,"IncludeGrid",False);
  Opt@GridAltitude=get_att_value(Opt,"GridAltitude",Opt@Altitude+3000);
  Opt@GridVisibility=get_att_value(Opt,"GridVisibility",False);
  Opt@AltMode=get_att_value(Opt,"AltMode","relativeToGround");
  Opt@Extrude=get_att_value(Opt,"Extrude",True);
  Opt@Tesselate=get_att_value(Opt,"Tesselate",True);
  Opt@LineOpacity=get_att_value(Opt,"LineOpacity",100);
  Opt@LineRedColor=get_att_value(Opt,"LineRedColor",255);
  Opt@LineBlueColor=get_att_value(Opt,"LineBlueColor",0);
  Opt@LineGreenColor=get_att_value(Opt,"LineGreenColor",255);
  Opt@LineWidth =get_att_value(Opt,"LineWidth",1);
  Opt@Overwrite=get_att_value(Opt,"Overwrite",False);
  Opt@ForceOverwrite=get_att_value(Opt,"ForceOverwrite",False);
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@Debug=get_att_value(Opt,"Debug",False);
  
  ; Checking the Opacity range
  if (Opt@ConstOpacity.ne.-1 .and. \
      .not.(Opt@ConstOpacity.ge.0 .and. Opt@ConstOpacity.le.100) ) then
    print("Opt@ConstOpacity must be a number between 0 to 100, or it must be set to -1 to be ingnored.")
    exit
  end if
  
  ; Checking the coordinates
  if (Opt@CoordUnits.ne."Degrees")
  	; we need to transform the coordinates onto lat/lon
  	if (isatt(Opt,"CoordSystem")) then
  		; setting the destination grid coordsystem.
			DstCoord=True;
			DstCoord@projection="latlon"
			DstCoord@ellipsoid="WGS84"
			DstCoordStr=get_coordinate_system_string(SrcCoord);
			
			data_lon=todouble(xCoords);
			data_lat=todouble(yCoords);
			transform_coordinate(Opt@CoordSystem,DstCoordStr, \
			                     data_lon,data_lat,zeros(dimsizes(xCoords),"double"))
			delete(DstCoord);
			delete(DstCoordStr);
  	else
  		print("Opt@CoordSystem is needed to transform the data onto Lat/Lon WGS84 coordinate system.")
  	end if
  else
    data_lon=xCoords;
    data_lat=yCoords;
  end if
  Opt@North=get_att_value(Opt,"North",tofloat(max(data_lat)));
  Opt@South=get_att_value(Opt,"South",tofloat(min(data_lat)));
  Opt@East=get_att_value(Opt,"East",tofloat(max(data_lon)));
  Opt@West=get_att_value(Opt,"West",tofloat(min(data_lon)));
  
  Opt@Verbose=get_att_value(Opt,"Verbose",False);

  ; Printing the options if user requested the verbose mode 
  if (Opt@Verbose) then
    print(" ")
    print("Exporting Data to KML with the following Options:")
    print("KML Base file Name: " + FileName)
    print("-- Opt: " + Opt)
    print("-- Opt@StoragePath: " + Opt@StoragePath)
    print("-- Opt@DataSetTitle: " + Opt@DataSetTitle)
    print("-- Opt@PixelRatio: " + Opt@PixelRatio)
    print("-- Opt@CoordUnits: " + Opt@CoordUnits)
    print("-- Opt@InterpWeightFile: " + Opt@InterpWeightFile)
    print("-- Opt@KeepInterpWeightFile: " + Opt@KeepInterpWeightFile)
    print("-- Opt@InterpMethod: " + Opt@InterpMethod)
    print("-- Opt@SrcRegional: " + Opt@SrcRegional)
    print("-- Opt@DstRegional: " + Opt@DstRegional)
    print("-- Opt@North: " + Opt@North)
    print("-- Opt@South: " + Opt@South)
    print("-- Opt@East: " + Opt@East)
    print("-- Opt@West: " + Opt@West)
    print("-- Opt@Rotation: " + Opt@Rotation)
    print("-- Opt@ConstOpacity: " + Opt@ConstOpacity)
    print("-- Opt@Altitude: " +   Opt@Altitude)
    print("-- Opt@IncludeGrid: " +  Opt@IncludeGrid)
    print("-- Opt@GridAltitude: " +   Opt@GridAltitude)
    print("-- Opt@GridVisibility: " + Opt@GridVisibility)
    print("-- Opt@AltMode: " +  Opt@AltMode)
    print("-- Opt@Extrude: " +  Opt@Extrude)
    print("-- Opt@Tesselate: " +  Opt@Tesselate)
    print("-- Opt@LineOpacity: " + Opt@LineOpacity)
    print("-- Opt@LineRedColor: " + Opt@LineRedColor)
    print("-- Opt@LineBlueColor: " + Opt@LineBlueColor)
    print("-- Opt@LineGreenColor: " + Opt@LineGreenColor)
    print("-- Opt@LineWidth: " + Opt@LineWidth)
    print("-- Opt@Overwrite: " + Opt@Overwrite)
    print("-- Opt@ForceOverwrite: " + Opt@ForceOverwrite)
  end if

  ; ***** Getting some size *****
  if (Opt@Verbose) then
    print(" ")
    print(" Getting Data sizes ...")
  end if
  nframe=dim_dimsizes(RasterData,1);
  nlat=dim_dimsizes(RasterData,2);
  nlon=dim_dimsizes(RasterData,3);
  
  kml_nlat=tointeger(nlat*Opt@PixelRatio)
  kml_nlon=tointeger(nlon*Opt@PixelRatio)
  if (Opt@Verbose) then
    print("Number of frames:" + nframe)
    print("Frame Size: " + nlat + "x" + nlon)
    print("KML Image Size: " + kml_nlat + "x" + kml_nlon)
  end if
  
  ; ***** Preparing the Interpolation Weights *****
  if (isfilepresent(Opt@StoragePath+"/"+Opt@InterpWeightFile)) then
    if (Opt@Verbose) then
      print("Interpolation Weights were found")
    end if
  else
    if (Opt@Verbose) then
      print("Interpolation Weights were NOT found. Creating one ...")
    end if

    ; Creating the destination grid suitable for GE.
    kml_lat=conform_dims((/kml_nlat, kml_nlon/),fspan(Opt@North,Opt@South,kml_nlat),0)
    kml_lon=conform_dims((/kml_nlat, kml_nlon/),fspan(Opt@West,Opt@East,kml_nlon),1)

    ; Converting coordinates to SCRIP format
    curvilinear_to_SCRIP("tmpSrcGrid.nc",data_lat,data_lon,Opt)
    curvilinear_to_SCRIP("tmpDstGrid.nc",kml_lat,kml_lon,Opt)

    ; Generating the interpolation weights
    ESMF_regrid_gen_weights("tmpSrcGrid.nc","tmpDstGrid.nc",Opt@StoragePath + "/" + Opt@InterpWeightFile,Opt);
    system("rm -rf tmpSrcGrid.nc")
    system("rm -rf tmpDstGrid.nc")
  end if

  ; ***** Interpolating *****
  if (Opt@Verbose) then
    print("Interpolating data on grid suitable for Google Earth (GE)")
  end if
  Data_Interped=ESMF_regrid_with_weights(RasterData,Opt@StoragePath + "/" + Opt@InterpWeightFile,Opt)

  ; ***** Preparing KML Ground Overlay images *****
  if (Opt@Verbose) then
    print(" ")
    print("Preparing KML Ground Overlay images")
  end if
  
  ; I changed the code so only PNG files are generated. (No need to have GDAL anymore)
  ; converting the colormap
  PNGOpt=True;
  PNGOpt@ColorMap=get_att_value(Opt,"ColorMap","WhiteBlueGreenYellowRed");
  PNGOpt@MinData=get_att_value(Opt,"MinData",min(RasterData));
  PNGOpt@MaxData=get_att_value(Opt,"MaxData",max(RasterData));
  PNGOpt@ReverseColorMap=get_att_value(Opt,"ReverseColorMap",False);
  if (isatt(Data_Interped,"_MissingValue")) then
 		PNGOpt@NoData=Data_Interped@_MissingValue;
 	end if
  RGBA=Gray2RGBA(ndtooned(Data_Interped),PNGOpt)
  RGBA(3,:)=where(ismissing(ndtooned(Data_Interped)),0,RGBA(3,:));

  ; Adjusting the Alpha/Transparency
  if(isatt(Opt,"MinDataOpacity").and.isatt(Opt,"MaxDataOpacity")) then
    RGBA(3,:)=round( ((ndtooned(Data_Interped)-PNGOpt@MinData)/(PNGOpt@MaxData-PNGOpt@MinData)*(Opt@MaxDataOpacity-Opt@MinDataOpacity)+Opt@MinDataOpacity)*2.55,3)
    RGBA(3,:)=where(ndtooned(Data_Interped).lt.PNGOpt@MinData,round(Opt@MinDataOpacity*2.55,3),RGBA(3,:));
    RGBA(3,:)=where(ndtooned(Data_Interped).gt.PNGOpt@MaxData,round(Opt@MaxDataOpacity*2.55,3),RGBA(3,:));
    RGBA(3,:)=where(ismissing(ndtooned(Data_Interped)),0,RGBA(3,:));
  else if (Opt@ConstOpacity.ne.-1) then
    RGBA(3,:)=where(ismissing(ndtooned(Data_Interped)),0,round(Opt@ConstOpacity*255.0/100.0,3));
  end if
  end if

  ; Storing the PNG Files.
  system("mkdir -p "+Opt@StoragePath + "/" + FileName + "_images")
  rgba2png(Opt@StoragePath + "/" + FileName + "_images/Frame", \
           touint(RGBA(0,:)),touint(RGBA(1,:)),touint(RGBA(2,:)),touint(RGBA(3,:)), \
           touint(dim_dimsizes(Data_Interped,3)),touint(dim_dimsizes(Data_Interped,2)),touint(dim_dimsizes(Data_Interped,1)))
  delete(Data_Interped)

  
  ; ***** Preparing KML file *****
  kml_FileName=Opt@StoragePath + "/"+FileName+".kml"
  OpenKML(kml_FileName)
  Open_KML_Folder(kml_FileName, Opt@DataSetTitle)
  GOverlayOpt=True
  GOverlayOpt@North=Opt@North
  GOverlayOpt@South=Opt@South
  GOverlayOpt@West=Opt@West
  GOverlayOpt@East=Opt@East
  GOverlayOpt@Rotation=Opt@Rotation
  GOverlayOpt@Opacity=Opt@ConstOpacity
  GOverlayOpt@Altitude=Opt@Altitude
  GOverlayOpt@AltMode=Opt@AltMode
  do i=1,nframe
    hrefLink = FileName + "_images/Frame" + sprinti("%0.4i", i-1) + ".png"

    timetags_s=tochar(TimeTag(i-1)+"Z");
    timetags_s(10)=tochar("T")

    timetags_e=tochar(TimeTag(min( (/i,nframe-1/) ))+"Z");
    dd=tointeger(tostring(timetags_e(8:9)))
    hh=tointeger(tostring(timetags_e(11:12)))
    mm=tointeger(tostring(timetags_e(14:15)))
    ss=tointeger(tostring(timetags_e(17:18)))
    if (i.ne.nframe) then
      addTime(dd,hh,mm,ss,0,0,-1);
    end if
    timetags_e(8:9)=tochar(sprinti("%0.2i",dd))
    timetags_e(11:12)=tochar(sprinti("%0.2i",hh))
    timetags_e(14:15)=tochar(sprinti("%0.2i",mm))
    timetags_e(17:18)=tochar(sprinti("%0.2i",ss))
    timetags_e(10)=tochar("T")
    
    if (i.ne.nframe) then
      if (isatt(GOverlayOpt,"TimeTagStr")) then
        delete(GOverlayOpt@TimeTagStr)
      end if
      GOverlayOpt@Begin_TimeTagStr=tostring(timetags_s);
      GOverlayOpt@End_TimeTagStr=tostring(timetags_e);
    else
      if (isatt(GOverlayOpt,"Begin_TimeTagStr").and.isatt(GOverlayOpt,"End_TimeTagStr")) then
        delete(GOverlayOpt@Begin_TimeTagStr)
        delete(GOverlayOpt@End_TimeTagStr)
      end if
      GOverlayOpt@TimeTagStr=tostring(timetags_s)
    end if
    add_KML_GOverlay(kml_FileName, hrefLink, GOverlayOpt)
  end do
  Close_KML_Folder(kml_FileName)
  
  if (Opt@IncludeGrid) then
    if (Opt@Verbose) then
      print(" ")
      print("Adding the grid extend to KML.")
      print("Depending on your grid size this may take a while. Please wait ...")
    end if
    
    ; adding line style for coloring
    StyleIDStr="ID_"+FileName+"_"+Opt@DataSetTitle
    Open_KML_Style(kml_FileName,StyleIDStr)
    LineStyleOpt=True
    LineStyleOpt@Opacity=Opt@LineOpacity
    LineStyleOpt@Red=Opt@LineRedColor
    LineStyleOpt@Blue=Opt@LineBlueColor
    LineStyleOpt@Green=Opt@LineGreenColor
    LineStyleOpt@Width=Opt@LineWidth
    add_KML_LineStyle(kml_FileName,LineStyleOpt)
    Close_KML_Style(kml_FileName)
    Open_KML_Folder(kml_FileName, "Grid")
    add_KML_Visibility(kml_FileName,Opt@GridVisibility)

    add_NCL_KML_2DGrid(kml_FileName,data_lat,data_lon,Opt@GridAltitude*ones((/nlat,nlon/),"float"), \
                       StyleIDStr,GetAltModeNumber(Opt@AltMode), \
                       Opt@Extrude, Opt@Tesselate, Opt@GridVisibility)
    Close_KML_Folder(kml_FileName)
  end if
  
  CloseKML(kml_FileName)

  if (Opt@Verbose) then
    print("All done!")
  end if  
end ; of RasterTimeSeries2KML

;=========================================================================================
; SHP2KML converts single points to KML format
;=========================================================================================
undef("SHP2KML")
procedure SHP2KML(SHPFileName[1]:string, FileName[1]:string, inOpt[1]:logical)
local Opt, fid, vNames, geom, seg, lon, lat, nFeatures, nSegments, nPoints, h, \
      tmpLessThanMin, tmpMoreThanMax, ScaleFactor, Offset, RGBA, geomType, \
      kml_FileName, FeatureOpt, i, StyleIDStr, startSeg, startPoint, endPoint, zValue, \
      subFeature
begin
  Opt=inOpt;  Making a local copy, so the original copy is not changed.
  Opt=True;
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@Name=get_att_value(Opt,"Name","SHP");
  Opt@Extrude=get_att_value(Opt,"Extrude",True);
  Opt@AltMode=get_att_value(Opt,"AltMode","absolute");
  Opt@Tesselate=get_att_value(Opt,"Tesselate",True);
  Opt@ColorMap=get_att_value(Opt,"ColorMap","WhiteBlueGreenYellowRed");
  Opt@ReverseColorMap=get_att_value(Opt,"ReverseColorMap",False);
  Opt@VarName=get_att_value(Opt,"VarName","z");
  Opt@StoragePath=get_att_value(Opt,"StoragePath","./");
  Opt@NoData=get_att_value(Opt,"NoData",-9999);
  Opt@constHeight=get_att_value(Opt,"constHeight",3000.0);
  Opt@UseConstOpacity=get_att_value(Opt,"UseConstOpacity",False);
  Opt@ConstOpacity=get_att_value(Opt,"ConstOpacity",50);
  Opt@MakeLogScale=get_att_value(Opt,"MakeLogScale",False);
  
  if (Opt@Verbose) then
    print("- Opt@Name: " + Opt@Name)
    print("- Opt@Extrude: " + Opt@Extrude)
    print("- Opt@AltMode: " + Opt@AltMode)
    print("- Opt@Tesselate: " + Opt@Tesselate)
    print("- Opt@ColorMap: " + Opt@ColorMap)
    print("- Opt@ReverseColorMap: " + Opt@ReverseColorMap)
    print("- Opt@VarName: " + Opt@VarName)
    print("- Opt@StoragePath: " + Opt@StoragePath)
    print("- Opt@NoData: " + Opt@NoData)
    print("- Opt@constHeight: " + Opt@constHeight)
    print("- Opt@UseConstOpacity: " + Opt@UseConstOpacity)
    print("- Opt@ConstOpacity: " + Opt@ConstOpacity)
    print("- Opt@MakeLogScale: " + Opt@MakeLogScale)
  end if
  
  ; Opening the Shapefile
  if (Opt@Verbose) then
    print("Opening the shapefile ... ")
  end if
  fid=addfile(SHPFileName,"r")

  ;Reading segments
  if (Opt@Verbose) then
    print("Reading Data ... ")
  end if

  geom=fid->geometry;
  seg=fid->segments;
  lon=fid->x
  lat=fid->y
  
  ; getting number of features, segments, and points
  nFeatures=dim_dimsizes(geom,1);
  nSegments=dim_dimsizes(seg,1);
  nPoints=dimsizes(lon);  
    
  ; Checking if the variable is present
  vNames=getfilevarnames(fid);
  Opt@VarExists=any(vNames.eq.Opt@VarName)
  
  if (Opt@VarExists) then
    if (Opt@Verbose) then
      print("Data field were found. Opt@constHeight is ignored")
    end if
    h=fid->$Opt@VarName$
    h@_FillValue=Opt@NoData
    if (Opt@MakeLogScale) then
      if (Opt@Verbose) then
        print("Taking the log of the data ...")
      end if
      h=log(h);
      if (isatt(Opt,"MinData")) then
        Opt@MinData=log(Opt@MinData)
      end if
      if (isatt(Opt,"MaxData")) then
        Opt@MaxData=log(Opt@MaxData)
      end if
    end if
    if (isatt(Opt,"MinH").and.isatt(Opt,"MaxH")) then
      Opt@MinData=get_att_value(Opt,"MinData",min(h));
      Opt@MaxData=get_att_value(Opt,"MaxData",max(h));
      tmpLessThanMin=ind(h.lt.Opt@MinData);
      tmpMoreThanMax=ind(h.gt.Opt@MaxData);
      if (Opt@Verbose) then
        print("Rescaling data as requested ...")
      end if
      ScaleFactor=(Opt@MaxH-Opt@MinH)/(Opt@MaxData-Opt@MinData)
      Offset=-ScaleFactor*Opt@MinData+Opt@MinH
      h=ScaleFactor*h+Offset
      if (.not.any(ismissing(tmpLessThanMin))) then
        h(tmpLessThanMin)=Opt@MinH;
      end if
      if (.not.any(ismissing(tmpMoreThanMax))) then
        h(tmpMoreThanMax)=Opt@MaxH
      end if
    end if
  else
    print("No data variable were found. Showing polygons at height " + Opt@constHeight)
    h=ones(nFeatures,"float")*Opt@constHeight ; if the variable is not there it is assumed that the height is 3000m.
    Opt@AltMode="absolute"
  end if
  
  ; Getting color based on height
  Opt@MinData=min(h)
  Opt@MaxData=max(h)
  RGBA=Gray2RGBA(h,Opt)

  if (Opt@UseConstOpacity) then
    if (Opt@Verbose) then
      print("Ignoring the colormap opacity, and using the constant opacity of " \
              + Opt@ConstOpacity + "%")
    end if
    RGBA(3,:)=round(todouble(Opt@ConstOpacity)/100.0*255.0,3)
  end if

  ;Getting Geometry type
  geomType=fid@geometry_type
  if (Opt@Verbose) then
    print("Geometry type: " + geomType)
    print("Number of Features: " + nFeatures)
  end if

  if (Opt@Verbose) then
    print("Adding features from the shape file to the KML file ...")
    print("This may take some time. please wait ...")
  end if
  kml_FileName=Opt@StoragePath+FileName+".kml"
  FeatureOpt=True
  FeatureOpt@Extrude=Opt@Extrude
  FeatureOpt@AltMode=Opt@AltMode
  OpenKML(kml_FileName)
  Open_KML_Folder(kml_FileName,"Polygons")
  add_KML_Open(kml_FileName,False)

  do i=0,nFeatures-1
    if (Opt@Verbose) then
      print("Adding feature#: "+i)
    end if
    if (geomType.eq."polyline")
      FeatureOpt@Red=RGBA(0,i)
      FeatureOpt@Green=RGBA(1,i)
      FeatureOpt@Blue=RGBA(2,i)
      FeatureOpt@Opacity=round(todouble(RGBA(3,i))/255.0*100.0,3)
      FeatureOpt@Width=get_att_value(Opt,"LineWidth",1);
      ; Adding the Style
      StyleIDStr="ID_"+FileName+"_"+Opt@VarName+"_Polyline_"+i
      Open_KML_Style(kml_FileName,StyleIDStr)
      ; adding line style for coloring
      add_KML_LineStyle(kml_FileName,FeatureOpt)
      Close_KML_Style(kml_FileName)
      
      ; adding the place mark
      Open_KML_Placemark(kml_FileName, Opt@Name+" - Polyline# "+i)
      add_KML_StyleUrl(kml_FileName,StyleIDStr)
      Open_KML_MultiGeometry(kml_FileName)

      startSeg=geom(i,0)
      endSeg=startSeg+geom(i,1)-1
      do subFeature=startSeg,endSeg
        startPoint=seg(subFeature,0)
        endPoint=startPoint+seg(subFeature,1)-1
        if (Opt@Verbose) then
          print("-- Adding segment#: "+subFeature)
        end if

        zValue=h(i)*ones(endPoint-startPoint+1,"float");
        add_KML_LineString(kml_FileName, \
                          lat(startPoint:endPoint), \
                          lon(startPoint:endPoint), \
                          zValue, \
                          Opt@AltMode, Opt@Extrude, Opt@Tesselate)
        delete(zValue)
      end do
      Close_KML_MultiGeometry(kml_FileName)
      Close_KML_Placemark(kml_FileName)
    end if

    if (geomType.eq."polygon")
      FeatureOpt@Red=RGBA(0,i)
      FeatureOpt@Green=RGBA(1,i)
      FeatureOpt@Blue=RGBA(2,i)
      FeatureOpt@Opacity=round(todouble(RGBA(3,i))/255.0*100.0,3)
            
      ; Adding the Style
      StyleIDStr="ID_"+FileName+"_"+Opt@VarName+"_Poly_"+i
      Open_KML_Style(kml_FileName,StyleIDStr)
      ; adding line style for coloring
      add_KML_LineStyle(kml_FileName,FeatureOpt)
      add_KML_PolyStyle(kml_FileName,FeatureOpt)
      Close_KML_Style(kml_FileName)
      
      ; adding the place mark
      Open_KML_Placemark(kml_FileName, Opt@Name+" - Poly# "+i)
      add_KML_StyleUrl(kml_FileName,StyleIDStr)
      Open_KML_MultiGeometry(kml_FileName)

      startSeg=geom(i,0)
      endSeg=startSeg+geom(i,1)-1
      do subFeature=startSeg,endSeg
        startPoint=seg(subFeature,0)
        endPoint=startPoint+seg(subFeature,1)-1
        if (Opt@Verbose) then
          print("-- Adding segment#: "+subFeature)
        end if
        
        zValue=h(i)*ones(endPoint-startPoint+1,"float");
        add_KML_Polygon(kml_FileName, \
                        lat(startPoint:endPoint), \
                        lon(startPoint:endPoint), \
                        zValue, \
                        FeatureOpt)
        delete(zValue)
      end do
      Close_KML_MultiGeometry(kml_FileName)
      Close_KML_Placemark(kml_FileName)
    end if
    if (geomType.eq."point") then
    	print("Point data in shapefile is NOT SUPPORTED YET.")
    	exit
    end if
  end do
  Close_KML_Folder(kml_FileName)
  CloseKML(kml_FileName)
end ; of SHP2KML

;=========================================================================================
; Point2KML converts single points to KML format
;=========================================================================================
undef("Point2KML")
procedure Point2KML(FileName[1]:string, inOpt[1]:logical)
local Opt, kml_FileName
begin
  Opt=inOpt
  Opt@Name=get_att_value(Opt,"Name","Default Name");
  Opt@Visibility=get_att_value(Opt,"Visibility",True);
  Opt@Description=get_att_value(Opt,"Description","No Description");
  Opt@StoragePath=get_att_value(Opt,"StoragePath","./");
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  
  if (Opt@Verbose) then
    print("Chosen Options:")
    print("- Opt@Name: " + Opt@Name)
    print("- Opt@Visibility: " + Opt@Visibility)
    print("- Opt@Description: " + Opt@Description)
    if (isatt(Opt,"Address")) then
      print("- Opt@Address: "+Opt@Address)
    end if
    if (isatt(Opt,"Lat").and.isatt(Opt,"Lon")) then
      print("- Opt@Lat: " + Opt@Lat)
      print("- Opt@Lon: " + Opt@Lon)
    end if
    print("- Opt@StoragePath: " + Opt@StoragePath)
    print("- Opt@Verbose: " + Opt@Verbose)
  end if
  
  ;
  kml_FileName=Opt@StoragePath+FileName+".kml"
  OpenKML(kml_FileName)
  Open_KML_Placemark(kml_FileName, Opt@Name)
  add_KML_Description(kml_FileName,Opt@Description)
  if (isatt(Opt,"Address")) then
    if (Opt@Verbose) then
      print("Adding the address field ...")
    end if
    add_KML_Address(kml_FileName,Opt@Address)
  end if
  if (isatt(Opt,"Lat").and.isatt(Opt,"Lon")) then
    if (Opt@Verbose) then
      print("Adding the lat/lon coordinates ...")
    end if
    add_KML_Point(kml_FileName,Opt@Lat,Opt@Lon,Opt)
  end if
  Close_KML_Placemark(kml_FileName)
  CloseKML(kml_FileName)
  
  if (Opt@Verbose) then
    print("All done!")
  end if
end ; end of Point2KML

;=========================================================================================
; Velocity2KML 
;=========================================================================================
undef("Velocity2KML")
procedure Velocity2KML(FileName[1]:string, \
                       u[*][*][*]:float, \
                       v[*][*][*]:float, \
                       lat[*][*]:float, \
                       lon[*][*]:float, \
                       TimeTag[*]:string, \
                       inOpt[1]:logical)
local   Opt, nFrame, nlat, nlon, nData, kml_FileName, StyleIDStr, cIndex, i, \
        timetags_s, timetags_e, dd, hh, mm, ss
begin
  Opt=inOpt;
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@Name=get_att_value(Opt,"Name","Default Name");
  Opt@StoragePath=get_att_value(Opt,"StoragePath","./");
  Opt@Height=get_att_value(Opt,"Height",3000.0);
  Opt@UseConstColor=get_att_value(Opt,"UseConstColor",False);
  Opt@AltMode=get_att_value(Opt,"AltMode","relativeToGround");
  Opt@Extrude=get_att_value(Opt,"Extrude",False);
  Opt@Tesselate=get_att_value(Opt,"Tesselate",True);
  Opt@LineOpacity=get_att_value(Opt,"LineOpacity",100);
  Opt@LineRedColor=get_att_value(Opt,"LineRedColor",255);
  Opt@LineBlueColor=get_att_value(Opt,"LineBlueColor",0);
  Opt@LineGreenColor=get_att_value(Opt,"LineGreenColor",255);
  Opt@LineWidth =get_att_value(Opt,"LineWidth",1);
  Opt@ColorMap=get_att_value(Opt,"ColorMap","WhiteBlueGreenYellowRed");
  Opt@IStride=get_att_value(Opt,"IStride",1);
  Opt@JStride=get_att_value(Opt,"JStride",1);
  
  if (Opt@Verbose) then
    print("Options:")
    print("- Opt@Name: " + Opt@Name)
    print("- Opt@StoragePath: " + Opt@StoragePath)
    print("- Opt@Height: " + Opt@Height)
    print("- Opt@UseConstColor: " + Opt@UseConstColor)
    print("- Opt@AltMode: " + Opt@AltMode)
    print("- Opt@Extrude: " + Opt@Extrude)
    print("- Opt@Tesselate: " + Opt@Tesselate)
    print("- Opt@LineOpacity: " + Opt@LineOpacity)
    print("- Opt@LineRedColor: " + Opt@LineRedColor)
    print("- Opt@LineBlueColor: " + Opt@LineBlueColor)
    print("- Opt@LineGreenColor: " + Opt@LineGreenColor)
    print("- Opt@LineWidth: " + Opt@LineWidth)
    print("- Opt@ColorMap: " + Opt@ColorMap)
    print("- Opt@IStride: " + Opt@IStride)
    print("- Opt@JStride: " + Opt@JStride)
  end if

  nFrame=dim_dimsizes(u,1);
  nlat=dim_dimsizes(u,2);
  nlon=dim_dimsizes(u,3);
  nData=nlat*nlon;

  if (Opt@Verbose) then
    print("nFrames: "+nFrame)
    print("nlat: "+nlat)
    print("nlon: "+nlon)
  end if  

  kml_FileName=Opt@StoragePath+FileName+".kml"
  
  OpenKML(kml_FileName);
  
  ; Reading and adding the colormap style
  if (Opt@Verbose) then
    print(" getting the color map ...")
  end if
  StyleIDStr=add_KML_ColorMapStyles(kml_FileName, Opt)

  ; Calculating the color Index
  if ( Opt@UseConstColor ) then
    cIndex=zeros(nFrame*nlat*nlon,"integer");
  else
    cIndex=Gray2cIndex(ndtooned(u*u+v*v),Opt)
  end if

  ; adding the vectors
  do i=0,nFrame-1
    if (Opt@Verbose) then
      print("Adding frame "+i)
    end if
    timetags_s=tochar(TimeTag(i)+"Z");
    timetags_s(10)=tochar("T")
    
    timetags_e=tochar(TimeTag(min( (/i+1,nFrame-1/) ))+"Z");
    dd=tointeger(tostring(timetags_e(8:9)))
    hh=tointeger(tostring(timetags_e(11:12)))
    mm=tointeger(tostring(timetags_e(14:15)))
    ss=tointeger(tostring(timetags_e(17:18)))
    if (i.ne.nFrame-1) then
      addTime(dd,hh,mm,ss,0,0,-1);
    end if
    timetags_e(8:9)=tochar(sprinti("%0.2i",dd))
    timetags_e(11:12)=tochar(sprinti("%0.2i",hh))
    timetags_e(14:15)=tochar(sprinti("%0.2i",mm))
    timetags_e(17:18)=tochar(sprinti("%0.2i",ss))
    timetags_e(10)=tochar("T")
    
    Open_KML_Folder(kml_FileName,tostring(timetags_s))
    add_KML_Open(kml_FileName,False)
    if (i.ne.0) then
      add_KML_Visibility(kml_FileName,False)
    end if

  add_NCL_KML_Arrow(kml_FileName,Opt@IStride,Opt@JStride, \
                    (lat), (lon), \
                    (u(i,:,:)),(v(i,:,:)), ones((/nlat,nlon/),"float")*Opt@Height, \
                    onedtond(touint(cIndex(i*nlat*nlon:i*nlat*nlon+(nlat-1)*nlon+nlon-1)),(/nlat,nlon/)), \
                    tostring(timetags_s),tostring(timetags_e), \
                    Opt@ArrowScale,0.0,0.0,GetAltModeNumber(Opt@AltMode),Opt@Extrude, Opt@Tesselate, False);

    Close_KML_Folder(kml_FileName)
  end do ; frame loop
  
  CloseKML(kml_FileName);

    
end ; end of Velocity2KML


;=========================================================================================
; Points2KML converts multiple points to KML format
;=========================================================================================
undef("Points2KML")
procedure Points2KML(FileName[1]:string, NameString[1]:string)
begin
  print("to do")
end


;=========================================================================================
; Collect_KMLs collects multiple KML files into one
;=========================================================================================
undef("Collect_KMLs")
procedure Collect_KMLs(outFileName[1]:string, inFileList[*]:string, inOpt[1]:logical)
local Opt, nFiles, kml_FileName, i 
begin
  Opt=inOpt;
  nFiles=dimsizes(inFileList);
  print("Number of KML Files: "+nFiles);
  
  if (isatt(Opt,"NameList")) then
    if (dimsizes(Opt@NameList).eq.nFiles) then
      isNameListProvided=True;
    else
      isNameListProvided=False
    end if
  else
    isNameListProvided=False
  end if
  
  kml_FileName=outFileName+".kml";
  OpenKML(kml_FileName);
  do i=0,nFiles-1
    print("adding "+inFileList(i)+" ...")
    Open_KML_NetworkLink(kml_FileName, "NetLinkID:"+i)
    if (isNameListProvided) then
      add_KML_Name(kml_FileName, Opt@NameList(i))
    else
      add_KML_Name(kml_FileName, inFileList(i))
    end if
    add_KML_FlyToView(kml_FileName,True);
    add_KML_Open(kml_FileName,False);
    Open_KML_Link(kml_FileName);
    add_KML_Href(kml_FileName,inFileList(i));
    Close_KML_Link(kml_FileName);
    Close_KML_NetworkLink(kml_FileName);
  end do
  CloseKML(kml_FileName);
end ; end of Collect_KMLs

;=========================================================================================
; Path2KML 
;=========================================================================================
undef("Path2KML")
procedure Path2KML(FileName[1]:string, \
                   lat[*]:numeric,lon[*]:numeric,Height[*]:numeric,\
                   TimeTagStr[*]:string,inOpt[1]:logical)
local Opt, kml_FileName, StyleOpt, PointOpt, pi, r2d, nPoint, \
      HeadingX, HeadingY, timetags_s, timetags_e, dd, hh, mm, ss
begin
  Opt=inOpt;
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@StoragePath=get_att_value(Opt,"StoragePath","./");
  Opt@PathName=get_att_value(Opt,"PathName"," ");
  Opt@IconName=get_att_value(Opt,"IconName","ylw-pushpin");
  Opt@LineOpacity=get_att_value(Opt,"LineOpacity",100);
  Opt@LineRedColor=get_att_value(Opt,"LineRedColor",255);
  Opt@LineGreenColor=get_att_value(Opt,"LineGreenColor",255);
  Opt@LineBlueColor=get_att_value(Opt,"LineBlueColor",0);
  Opt@LineWidth =get_att_value(Opt,"LineWidth",1);
  Opt@PointRedColor=get_att_value(Opt,"PointRedColor",255);
  Opt@PointGreenColor=get_att_value(Opt,"PointGreenColor",255);
  Opt@PointBlueColor=get_att_value(Opt,"PointBlueColor",255);
  Opt@PointOpacity=get_att_value(Opt,"PointOpacity",100);
  Opt@AdjustHeading=get_att_value(Opt,"AdjustHeading",False);
  Opt@AltMode=get_att_value(Opt,"AltMode","relativeToGround");
  Opt@Extrude=get_att_value(Opt,"Extrude",False);
  Opt@Tesselate=get_att_value(Opt,"Tesselate",True);
  
  kml_FileName=Opt@StoragePath+FileName+".kml"
  
  OpenKML(kml_FileName);
  ; adding the Line Style
  StyleOpt=True
  StyleOpt@Red=Opt@LineRedColor
  StyleOpt@Green=Opt@LineGreenColor
  StyleOpt@Blue=Opt@LineBlueColor
  StyleOpt@Opacity=Opt@LineOpacity
  StyleOpt@Width=Opt@LineWidth
  
  Open_KML_Style(kml_FileName,"PathStyleID")
  add_KML_LineStyle(kml_FileName,StyleOpt)
  Close_KML_Style(kml_FileName)
  
  add_KML_HideChildrenStyle(kml_FileName);

  ; adding the path as a line string
  if (Opt@Verbose) then
    print("Adding the path ...")
  end if
  Open_KML_Folder(kml_FileName,Opt@PathName)
  Open_KML_Placemark(kml_FileName, Opt@PathName)
  add_KML_LineString(kml_FileName,lat,lon,Height,\
                     Opt@AltMode,Opt@Extrude,Opt@Tesselate);
  add_KML_StyleUrl(kml_FileName, "PathStyleID");
  Close_KML_Placemark(kml_FileName)
  Close_KML_Folder(kml_FileName)

  ; Adding individual records as points
  if (Opt@Verbose) then
    print("Adding points on path ...")
  end if
  Open_KML_Folder(kml_FileName,"Points")
  hide_KML_Children(kml_FileName)
  add_KML_Visibility(kml_FileName,False); 
  PointOpt=True
  PointOpt@Extrude=Opt@Extrude;
  PointOpt@AltMode=Opt@AltMode

  StyleOpt@Red=Opt@PointRedColor
  StyleOpt@Green=Opt@PointGreenColor
  StyleOpt@Blue=Opt@PointBlueColor
  StyleOpt@Opacity=Opt@PointOpacity
  StyleOpt@hrefLink=get_KML_IconHref(Opt@IconName)
  StyleOpt@Heading=0.0d;
  
  pi=atan(1)*4.0;
  r2d=180.0/pi;
  nPoint=dimsizes(lat);
  do i=0,nPoint-1
    if (Opt@Verbose) then
      print("Adding record #"+i)
    end if
    
    if (Opt@AdjustHeading) then
      if (i.ne.nPoint-1)
        HeadingX=lon(i+1)-lon(i);
        HeadingY=lat(i+1)-lat(i);
        StyleOpt@Heading=90.0-atan2(HeadingY,HeadingX)*r2d;
        if (StyleOpt@Heading.lt.0) then
          StyleOpt@Heading=StyleOpt@Heading+360
        end if
      end if
    end if
    
    Open_KML_Style(kml_FileName,"PointOnPathStyleID"+i)
    add_KML_IconStyle(kml_FileName,StyleOpt)
    Close_KML_Style(kml_FileName)
    timetags_s=tochar(TimeTagStr(i)+"Z");
    timetags_s(10)=tochar("T")
    
    timetags_e=tochar(TimeTagStr(min( (/i+1,nPoint-1/) ))+"Z");
    dd=tointeger(tostring(timetags_e(8:9)))
    hh=tointeger(tostring(timetags_e(11:12)))
    mm=tointeger(tostring(timetags_e(14:15)))
    ss=tointeger(tostring(timetags_e(17:18)))
    if (i.ne.nPoint-1) then
      addTime(dd,hh,mm,ss,0,0,-1);
    end if
    timetags_e(8:9)=tochar(sprinti("%0.2i",dd))
    timetags_e(11:12)=tochar(sprinti("%0.2i",hh))
    timetags_e(14:15)=tochar(sprinti("%0.2i",mm))
    timetags_e(17:18)=tochar(sprinti("%0.2i",ss))
    timetags_e(10)=tochar("T")
  
    Open_KML_Placemark(kml_FileName, Opt@PathName)
    add_KML_StyleUrl(kml_FileName, "PointOnPathStyleID"+i);
    add_KML_TimeSpan(kml_FileName,tostring(timetags_s),tostring(timetags_e));
    PointOpt@Height=Height(i);
    add_KML_Point(kml_FileName,lat(i),lon(i),PointOpt)
    Close_KML_Placemark(kml_FileName)
  end do
  Close_KML_Folder(kml_FileName)
  
  
  CloseKML(kml_FileName);
end ; end of Path2KML

;=========================================================================================
; Measurements2KML 
;=========================================================================================
undef("Measurements2KML")
procedure Measurements2KML(FileName[1]:string, \
                   lat[*]:numeric,lon[*]:numeric,Height[*]:numeric,\
                   inData[*]:numeric, \
                   TimeTagStr[*]:string,inOpt[1]:logical)
local Opt, RGBA, Radius, kml_FileName, nPoint, FeatureOpt, i, timetags_s, timetags_e, dd, \
      hh, mm, ss, StyleIDStr
begin
  Opt=inOpt;
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@StoragePath=get_att_value(Opt,"StoragePath",".");
  Opt@DataSetName=get_att_value(Opt,"DataSetName"," ");

  Opt@NSidePolygon=get_att_value(Opt,"NSidePolygon",20);
  Opt@ColorMap=get_att_value(Opt,"ColorMap","WhiteBlueGreenYellowRed");
  Opt@DataScale=get_att_value(Opt,"DataScale",20000.0);
  Opt@MinData=get_att_value(Opt,"MinData",min(inData));
  Opt@MaxData=get_att_value(Opt,"MaxData",max(inData));
  Opt@ConstOpacity=get_att_value(Opt,"ConstOpacity",-1);
  Opt@UseConstSize=get_att_value(Opt,"UseConstSize",False);
  
  Opt@ShowOutline=get_att_value(Opt,"ShowOutline",False);
  Opt@Fill=get_att_value(Opt,"Fill",True);
  Opt@AltMode=get_att_value(Opt,"AltMode","relativeToGround");
  Opt@Extrude=get_att_value(Opt,"Extrude",False);
  Opt@Tesselate=get_att_value(Opt,"Tesselate",True);
  
  ; coloring the data based on the color map.
  RGBA=Gray2RGBA(inData,Opt)
  RGBA(3,:)=round((100.0d/255.0d)*RGBA(3,:),3);
  RGBA(3,:)=where(ismissing(inData),0,RGBA(3,:));
  
  ; Adjusting the Opacity if requested
  if (Opt@ConstOpacity.ne.-1) then
    RGBA(3,:)=where(ismissing(inData),0,tointeger(Opt@ConstOpacity));
  end if
  
  ; Getting Radius based on the scale
  if (Opt@UseConstSize) then
    Radius=ones(dimsizes(inData),"double")*Opt@DataScale;
  else
    Radius=todouble(abs(inData)*Opt@DataScale);
  end if
  
  kml_FileName=Opt@StoragePath+"/"+FileName+".kml"
  
  OpenKML(kml_FileName)
  Open_KML_Folder(kml_FileName, Opt@DataSetName)
  
  nPoint=dimsizes(lat)
  FeatureOpt=True;
  do i=0,nPoint-1
    
    ; Getting time strings
    timetags_s=tochar(TimeTagStr(i)+"Z");
    timetags_s(10)=tochar("T")
    
    timetags_e=tochar(TimeTagStr(min( (/i+1,nPoint-1/) ))+"Z");
    dd=tointeger(tostring(timetags_e(8:9)))
    hh=tointeger(tostring(timetags_e(11:12)))
    mm=tointeger(tostring(timetags_e(14:15)))
    ss=tointeger(tostring(timetags_e(17:18)))
    if (i.ne.nPoint-1) then
      addTime(dd,hh,mm,ss,0,0,-1);
    end if
    timetags_e(8:9)=tochar(sprinti("%0.2i",dd))
    timetags_e(11:12)=tochar(sprinti("%0.2i",hh))
    timetags_e(14:15)=tochar(sprinti("%0.2i",mm))
    timetags_e(17:18)=tochar(sprinti("%0.2i",ss))
    timetags_e(10)=tochar("T")
    
    ; Adding the Style
    StyleIDStr="ID_Poly_"+i
    Open_KML_Style(kml_FileName,StyleIDStr)
    ; adding line/polygon style for coloring
    FeatureOpt@Red=RGBA(0,i)
    FeatureOpt@Green=RGBA(1,i)
    FeatureOpt@Blue=RGBA(2,i)
    FeatureOpt@Opacity=RGBA(3,i)
    FeatureOpt@Fill=Opt@Fill
    FeatureOpt@ShowOutline=Opt@ShowOutline
    add_KML_LineStyle(kml_FileName,FeatureOpt)
    add_KML_PolyStyle(kml_FileName,FeatureOpt)
    Close_KML_Style(kml_FileName)
  
    if (Opt@Verbose) then
      print("adding measurement # "+i)
    end if
    Open_KML_Placemark(kml_FileName, "Measurement #"+i+": "+inData(i))
    add_KML_TimeSpan(kml_FileName,tostring(timetags_s),tostring(timetags_e));
    add_KML_StyleUrl(kml_FileName,StyleIDStr)
    add_KML_nSidedPolygon(kml_FileName, \
                          todouble(lat(i)),todouble(lon(i)),Height(i), \
                          Opt@NSidePolygon,Radius(i),Opt)
    Close_KML_Placemark(kml_FileName)
  end do
  
  Close_KML_Folder(kml_FileName)
  CloseKML(kml_FileName)
end ; end of Measurements2KML


;=========================================================================================
; UnstructGrid2KML
;=========================================================================================
undef("UnstructGrid2KML")
procedure UnstructGrid2KML(FileName[1]:string, \
                           verticesOnEdge[*][2]:uint, \
                           latVertex[*]:float, lonVertex[*]:float, \
                           inOpt[1]:logical)
local Opt, kml_FileName, StyleIDStr, LineStyleOpt
begin
  Opt=inOpt;
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@StoragePath=get_att_value(Opt,"StoragePath",".");
  Opt@GridName=get_att_value(Opt,"GridName","GRID");
  Opt@Height=get_att_value(Opt,"Height",5000.0);

  Opt@Opacity=get_att_value(Opt,"Opacity",100);
  Opt@Red=get_att_value(Opt,"Red",255);
  Opt@Green=get_att_value(Opt,"Green",255);
  Opt@Blue=get_att_value(Opt,"Blue",0);
  Opt@Width=get_att_value(Opt,"Width",1);
  Opt@Visible=get_att_value(Opt,"Visible",True);
  Opt@AltMode=get_att_value(Opt,"AltMode","absolute");
  Opt@Extrude=get_att_value(Opt,"Extrude",False);
  Opt@Tesselate=get_att_value(Opt,"Tesselate",False);
  
  
  kml_FileName=Opt@StoragePath+"/"+FileName+".kml"
  OpenKML(kml_FileName)
  Open_KML_Folder(kml_FileName, Opt@GridName)
  add_KML_Visibility(kml_FileName,Opt@Visible)  
  
  ; adding line style for coloring
  StyleIDStr="ID_GRID_"+Opt@GridName
  Open_KML_Style(kml_FileName,StyleIDStr)
  LineStyleOpt=True
  LineStyleOpt@Opacity=Opt@Opacity
  LineStyleOpt@Red=Opt@Red
  LineStyleOpt@Green=Opt@Green
  LineStyleOpt@Blue=Opt@Blue
  LineStyleOpt@Width=Opt@Width
  add_KML_LineStyle(kml_FileName,LineStyleOpt)
  Close_KML_Style(kml_FileName)
    
  add_NCL_KML_UnstructGrid(kml_FileName, verticesOnEdge, \
                           latVertex, lonVertex, Opt@Height*ones(dimsizes(latVertex),"float"), \
                           StyleIDStr, \
                           GetAltModeNumber(Opt@AltMode), \
                           Opt@Extrude, Opt@Tesselate, Opt@Visible)

  Close_KML_Folder(kml_FileName)
  CloseKML(kml_FileName);
  
end ; end of UnstructGrid2KML

;=========================================================================================
; UnstructData2KML
;=========================================================================================
undef("UnstructData2KML")
procedure UnstructData2KML(FileName[1]:string, \
                           inData[*][*]:numeric, \
                           lat[*]:float, lon[*]:float, \
                           TimeTag[*]:string, \
                           inOpt[1]:logical)
local Opt, ESMF_Opt, kml_lat, kml_lon, Data_Interped, nFrames, RGBA, kml_FileName, \
      GOverlayOpt, i, hrefLink, timetags_s, timetags_e, dd, hh, mm, ss, ExReg_Ind
begin
  Opt=inOpt;
  Opt@Verbose=get_att_value(Opt,"Verbose",False);
  Opt@StoragePath=get_att_value(Opt,"StoragePath",".");
  Opt@DataSetTitle=get_att_value(Opt,"DataSetTitle","Unstructued Data");
  Opt@North=get_att_value(Opt,"North",max(lat));
  Opt@South=get_att_value(Opt,"South",min(lat));
  Opt@West=get_att_value(Opt,"West",min(lon));
  Opt@East=get_att_value(Opt,"East",max(lon));
  Opt@Rotation=get_att_value(Opt,"Rotation",0.0);
  Opt@Height=get_att_value(Opt,"Height",0.0);
  Opt@AltMode=get_att_value(Opt,"AltMode","clampToGround");
  
  Opt@NPixelWidth=get_att_value(Opt,"NPixelWidth",480);
  Opt@NPixelHeight=get_att_value(Opt,"NPixelHeight",320);
  
  Opt@ExcludeRegion=get_att_value(Opt,"ExcludeRegion",False);
  if (Opt@ExcludeRegion .and. \
      (.not.isatt(Opt,"ExcludedRegionNorth").or. \
       .not.isatt(Opt,"ExcludedRegionSouth").or. \
       .not.isatt(Opt,"ExcludedRegionWest").or. \
       .not.isatt(Opt,"ExcludedRegionEast"))) then
    print("There should be a value provided for Opt@ExclidedRegion_North/South/West/East, Once Opt@ExcludedRegion is set to true.")
    exit
  end if
  
  Opt@InterpWeightFile=get_att_value(Opt,"InterpWeightFile",FileName+"_wgt.nc");
  Opt@KeepInterpWeightFile=get_att_value(Opt,"KeepInterpWeightFile",True);
  
  Opt@ColorMap=get_att_value(Opt,"ColorMap","WhiteBlueGreenYellowRed");
  Opt@ReverseColorMap=get_att_value(Opt,"ReverseColorMap",False);
  Opt@UseLogScale=get_att_value(Opt,"UseLogScale",False);
  Opt@MinData=get_att_value(Opt,"MinData",min(inData));
  Opt@MaxData=get_att_value(Opt,"MaxData",max(inData));
  Opt@ConstOpacity=get_att_value(Opt,"ConstOpacity",-1);
  Opt@MinDataOpacity=get_att_value(Opt,"MinDataOpacity",-1);
  Opt@MaxDataOpacity=get_att_value(Opt,"MaxDataOpacity",-1);
  
  if (Opt@Verbose) then
    print("Opt@")
    print("----StoragePath:"+Opt@StoragePath)
    print("----DataSetTitle:"+Opt@DataSetTitle)
    print("----North:"+Opt@North)
    print("----South:"+Opt@South)
    print("----West:"+Opt@West)
    print("----East:"+Opt@East)
    print("----Rotation:"+Opt@Rotation);
    print("----Height:"+Opt@Height)
    print("----AltMode:"+Opt@AltMode)
    print("----NPixelWidth:"+Opt@NPixelWidth)
    print("----NPixelHeight:"+Opt@NPixelHeight)
    print("----ExcludeRegion:"+Opt@ExcludeRegion)
    if (Opt@ExcludeRegion) then
      print("----ExcludedRegionNorth:"+Opt@ExcludedRegionNorth)
      print("----ExcludedRegionSouth:"+Opt@ExcludedRegionSouth)
      print("----ExcludedRegionWest:"+Opt@ExcludedRegionWest)
      print("----ExcludedRegionEast:"+Opt@ExcludedRegionEast)
    end if
    print("----InterpWeightFile:"+Opt@InterpWeightFile)
    print("----KeepInterpWeightFile:"+Opt@KeepInterpWeightFile)
    print("----ColorMap:"+Opt@ColorMap)
    print("----ReverseColorMap:"+Opt@ReverseColorMap)
    print("----UseLogScale:"+Opt@UseLogScale)
    print("----MinData:"+Opt@MinData)
    print("----MaxData:"+Opt@MaxData)
    print("----ConstOpacity:"+Opt@ConstOpacity)
    print("----MinDataOpacity:"+Opt@MinDataOpacity)
    print("----MaxDataOpacity:"+Opt@MaxDataOpacity)
  end if
  
  
  ; Preparing options for ESMF Interpolation
  ESMF_Opt=Opt
  ESMF_Opt=True;
  ESMF_Opt@SrcESMF=True;
  ESMF_Opt@DstESMF=False;
  ; Creating Destination Grid, suitable for GE
  kml_lat=conform_dims((/Opt@NPixelHeight, Opt@NPixelWidth/),fspan(Opt@North,Opt@South,Opt@NPixelHeight),0)
  kml_lon=conform_dims((/Opt@NPixelHeight, Opt@NPixelWidth/),fspan(Opt@West,Opt@East,Opt@NPixelWidth),1)
  if (isfilepresent(Opt@StoragePath+"/"+Opt@InterpWeightFile)) then
    if (Opt@Verbose) then
      print("Interpolation Weights were found.")
    end if
  else
    if (Opt@Verbose) then
      print("Interpolation Weights werenot found. Creating one ...")
      print("preparing destination grid ...")
    end if
    
    ; Converting coordinates to SCRIP format
    curvilinear_to_SCRIP("tmpDstGrid.nc",kml_lat,kml_lon,Opt)
  
    if (Opt@Verbose) then
      print("preparing source grid ...")
    end if
    ; Creating the source grid.
    unstructured_to_ESMF("tmpSrcGrid.nc",lat,lon,ESMF_Opt);

    if (Opt@Verbose) then
      print("preparing weight file ...")
    end if
    ; Generating the interpolation weight file.
    ESMF_regrid_gen_weights("tmpSrcGrid.nc","tmpDstGrid.nc",Opt@StoragePath + "/" + Opt@InterpWeightFile,ESMF_Opt);
    system("rm -rf tmpSrcGrid.nc")
    system("rm -rf tmpDstGrid.nc")
  end if

  ; ***** Interpolating *****
  if (Opt@Verbose) then
    print("Interpolating data on grid suitable for Google Earth (GE)")
  end if
  Data_Interped=ESMF_regrid_with_weights(inData,Opt@StoragePath + "/" + Opt@InterpWeightFile,ESMF_Opt)
  
  if (.not.Opt@KeepInterpWeightFile) then
    system("rm "+Opt@StoragePath+"/"+Opt@InterpWeightFile);
  end if

  nFrames=dim_dimsizes(inData,1);
  if (Opt@Verbose) then
    print("Number of data frames: "+nFrames)
  end if
  
  RGBA=Gray2RGBA(ndtooned(Data_Interped),Opt);
  RGBA(3,:)=where(ismissing(ndtooned(Data_Interped)),0,RGBA(3,:));

  ; Adjusting the Alpha/Transparency
  if((Opt@MinDataOpacity.ne.-1).and.(Opt@MaxDataOpacity.ne.-1)) then
    RGBA(3,:)=round( ((ndtooned(Data_Interped)-Opt@MinData)/(Opt@MaxData-Opt@MinData)*(Opt@MaxDataOpacity-Opt@MinDataOpacity)+Opt@MinDataOpacity)*2.55,3)
    RGBA(3,:)=where(ndtooned(Data_Interped).lt.Opt@MinData,round(Opt@MinDataOpacity*2.55,3),RGBA(3,:));
    RGBA(3,:)=where(ndtooned(Data_Interped).gt.Opt@MaxData,round(Opt@MaxDataOpacity*2.55,3),RGBA(3,:));
    RGBA(3,:)=where(ismissing(ndtooned(Data_Interped)),0,RGBA(3,:));
  else if (Opt@ConstOpacity.ne.-1) then
    RGBA(3,:)=where(ismissing(ndtooned(Data_Interped)),0,round(Opt@ConstOpacity*255.0/100.0,3));
  end if
  end if
  
  if (Opt@ExcludeRegion) then
  	if (Opt@Verbose) then
  		print("Making excluded region full transparent ...")
  	end if
  	ExReg_Ind=ind(ndtooned(kml_lat.ge.Opt@ExcludedRegionSouth) .and. \
  	              ndtooned(kml_lat.le.Opt@ExcludedRegionNorth) .and. \
  	              ndtooned(kml_lon.ge.Opt@ExcludedRegionWest) .and. \
  	              ndtooned(kml_lon.le.Opt@ExcludedRegionEast))
		do i=0,nFrames-1
			RGBA(3,ExReg_Ind+i*Opt@NPixelWidth*Opt@NPixelHeight)=0;
		end do
    
  end if
  
  ; Exporting to PNG
  if (Opt@Verbose) then
    print("Exporting PNG files ...")
  end if
  system("mkdir -p "+Opt@StoragePath + "/" + FileName + "_images");
  rgba2png(Opt@StoragePath + "/" + FileName + "_images/Frame", \
           touint(RGBA(0,:)),touint(RGBA(1,:)),touint(RGBA(2,:)),touint(RGBA(3,:)), \
           touint(dim_dimsizes(Data_Interped,3)),touint(dim_dimsizes(Data_Interped,2)),touint(dim_dimsizes(Data_Interped,1)))

; ***** Preparing KML file *****
  kml_FileName=Opt@StoragePath + "/"+FileName+".kml"
  OpenKML(kml_FileName)
  Open_KML_Folder(kml_FileName, Opt@DataSetTitle)
  GOverlayOpt=Opt
  GOverlayOpt@North=Opt@North
  GOverlayOpt@South=Opt@South
  GOverlayOpt@West=Opt@West
  GOverlayOpt@East=Opt@East
  GOverlayOpt@Rotation=Opt@Rotation
  GOverlayOpt@Opacity=Opt@ConstOpacity
  GOverlayOpt@Altitude=Opt@Height
  GOverlayOpt@AltMode=Opt@AltMode
  do i=1,nFrames
    if (Opt@Verbose) then
      print("Exporting data frame #"+i+" to KML file ...")
    end if
    hrefLink = FileName + "_images/Frame" + sprinti("%0.4i", i-1) + ".png"

    ; Preparing the time tag
    timetags_s=tochar(TimeTag(i-1)+"Z");
    timetags_s(10)=tochar("T")
    if (i.ne.nFrames) then
      if (isatt(GOverlayOpt,"TimeTagStr")) then
        delete(GOverlayOpt@TimeTagStr)
      end if
      timetags_e=tochar(TimeTag(min( (/i,nFrames-1/) ))+"Z");
      dd=tointeger(tostring(timetags_e(8:9)))
      hh=tointeger(tostring(timetags_e(11:12)))
      mm=tointeger(tostring(timetags_e(14:15)))
      ss=tointeger(tostring(timetags_e(17:18)))
      if (i.ne.nFrames) then
        addTime(dd,hh,mm,ss,0,0,-1);
      end if
      timetags_e(8:9)=tochar(sprinti("%0.2i",dd))
      timetags_e(11:12)=tochar(sprinti("%0.2i",hh))
      timetags_e(14:15)=tochar(sprinti("%0.2i",mm))
      timetags_e(17:18)=tochar(sprinti("%0.2i",ss))
      timetags_e(10)=tochar("T")
    
      GOverlayOpt@Begin_TimeTagStr=tostring(timetags_s);
      GOverlayOpt@End_TimeTagStr=tostring(timetags_e);
    else
      if (isatt(GOverlayOpt,"Begin_TimeTagStr")) then
        delete(GOverlayOpt@Begin_TimeTagStr)
      end if
      if (isatt(GOverlayOpt,"End_TimeTagStr")) then
        delete(GOverlayOpt@End_TimeTagStr)
      end if
      GOverlayOpt@TimeTagStr=tostring(timetags_s)
    end if
    add_KML_GOverlay(kml_FileName, hrefLink, GOverlayOpt)
  end do
  Close_KML_Folder(kml_FileName)

  CloseKML(kml_FileName);
  
  if (Opt@Verbose) then
    print("All done!")
  end if
end ; end of UnstructData2KML

;=========================================================================================
; UnstructVelocity2KML
;=========================================================================================
undef("UnstructVelocity2KML")
procedure UnstructVelocity2KML(FileName[1]:string, \
                           verticesOnEdge[*][2]:uint, \
                           latVertex[*]:float, lonVertex[*]:float, \
                           inOpt[1]:logical)
begin
end ; end of UnstructVelocity2KML




















































