/* Table 0 Part 1 */
int centers_index[] = { 7, 8, 9, 34, 52, 54, 57, 58, 59, 60, 74, 85, 97, 98, 99 };
char *centers[] = {
			"US Weather Service - National Met. Center",	/*07*/
			"US Weather Service - NWS Telecomms Gateway",	/*08*/
			"US Weather Service - Field Stations",		/*09*/
			"Japanese Meteorological Agency - Tokyo",	/*34*/
			"National Hurricane Center, Miami",	/*52*/
			"Canadian Meteorological Service - Montreal",	/*54*/
			"US Air Force = Global Weather Center",	/*57*/
			"US Navy - Fleet Numerical Oceanography Center",	/*58*/
			"NOAA Forecast Systems Lab, Boulder, CO",	/*59*/
			"National Center for Atmospheric Research (NCAR), Boulder, CO",	/*60*/
			"U.K. Met Office - Bracknell",	/*74*/
			"French Weather Service - Toulouse",	/*85*/
			"European Space Agency",	/*97*/
			"European Center for Medium-Range Weather Forecasts - Reading",	/*98*/
			"DeBilt, Netherlands"	/*99*/
};

/* Table 0 Part 2 */
int sub_centers_index[] = { 1, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 170 };
char *sub_centers[] = {
			"NMC Re-Analysis Project", /*01*/
			"ABRFC - Arkansas-Red River RFC, Tulsa OK", /*150*/
			"Alaska RFC, Anchorage, AK", /*151*/
			"CBRFC - Colorado Basin RFC, Salt Lake City, UT", /*152*/
			"CNRFC - California-Nevada RFC, Sacramento, CA", /*153*/
			"LMRFC - Lower Mississippi RFC, Slidel, LA", /*154*/
			"MARFC - Middle Atlantic RFC, State College, PA", /*155*/
			"MBRFC - Missouri Basin RFC, Kansas City, MO", /*156*/
			"NCRFC - North Central RFC, Minneapolic, MN", /*157*/
			"NERFC - Northeast RFC, Hartford, CT", /*158*/
			"NWRFC - Northwest RFC, Portland, OR", /*159*/
			"OHRFC - Ohio Basin RFC, Cincinnati, OH", /*160*/
			"SERFC - Southeast RFC, Atlanta, GA", /*161*/
			"WGRFC - West Gulf RFC, Fort Worth, TX", /*162*/
			"OUN - Norman OK WFO" /*170*/
};

/* Table A */
int model_index[] = { 2, 5, 10, 19, 25, 39, 42, 43, 44, 49, 52, 53, 64, 68, 69, 70, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 150, 151, 152, 153};
char* model[] = {
		"Ultra Violet Index Model", /*02*/
		"Satellite Derived Precipitation and temperatures, from IR", /*05*/
		"Global Wind-Wave Forecast Model", /*10*/
		"Limited-area Fine Mesh (LFM) analysis", /*19*/
		"Snow Cover Analysis", /*25*/
		"Nested Grid forecast Model (NGM)", /*39*/
		"Global Optimum Interpolation Analysis (GOI) from \"Aviation\" run", /*42*/
		"Global Optimum Interpolation Analysis (GOI) from \"Final\" run", /*43*/
		"Sea Surface Temperature Analysis", /*44*/
		"Ozone Analysis from TIROS Observations", /*49*/
		"Ozone Analysis from Nimbus 7 Observations", /*52*/
		"LFM-Fourth Order Forecast Model", /*53*/
		"Regional Optimum Interpolation Analysis (ROI)", /*64*/
		"80 wave triangular, 18-layer Spectral model from \"Aviation\" run", /*68*/
		"80 wave triangular, 18 layer Spectral model from \"Medium Range Forecast\" run", /*69*/
		"Quasi-Lagrangian Hurricane Model (QLM)", /*70*/
		"Fog Forecast model - Ocean Prod. Center", /*73*/
		"Gulf of Mexico Wind/Wave", /*74*/
		"Gulf of Alaska Wind/Wave", /*75*/
		"Bias corrected Medium Range Forecast", /*76*/
		"126 wave triangular, 28 layer Spectral model from \"Aviation\" run", /*77*/
		"126 wave triangular, 28 layer Spectral model from \"Medium Range Forecast\" run", /*78*/
		"Backup from the previous run", /*79*/
		"62 wave triangular, 28 layer Spectral model from \"Medium Range Forecast\" run", /*80*/
		"Spectral Statistical Interpolation (SSI) analysis from \"Aviation\" run.", /*81*/
		"Spectral Statistical Interpolation (SSI) analysis from \"Final\" run.", /*82*/
		"ETA Model - 80 km version", /*83*/
		"ETA Model - 40 km version", /*84*/
		"ETA Model - 30 km version", /*85*/
		"RUC/MAPS Model, from Forecast Systems Lab (Isentropic; scale: 60km at 40N)", /*86*/
		"CAC Ensemble Forecasts from Spectral (ENSMB)", /*87*/
		"Ocean Wave model with additional physics (PWAV)", /*88*/
		"NWS River Forecast System (NWSRFS)", /*150*/
		"NWS Flash Flood Guidance System (NWSFFGS)", /*151*/
		"WSR-88D Stage II Precipitation Analysis", /*152*/
		"WSR-88D Stage III Precipitation Analysis" /*153*/
};


/* TABLE 2*/
int params_index[] = {1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 
31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 
63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 
96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 
123, 124, 125, 126, 127 };

int params_nwsnmc_index[] = {
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 
154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 172, 173, 174, 175, 176, 177, 181, 182, 183, 184,
189,190,191,192,193,194,195,196,197 ,201, 204, 205, 206, 207, 208, 209, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 
223, 226, 227, 228, 229, 231, 232, 233, 234, 235, 237, 238, 239, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 
254 
};

TBLE2 params[] = {
{"001", "Pressure", "Pa", "PRES"},
{"002", "Pressure reduced to MSL", "Pa", "PRMSL"},
{"003", "Pressure tendency", "Pa/s", "PTEND"},
{"005", "ICAO Standard Atmosphere Reference Height", "M", "ICAHT"},
{"006", "Geopotential", "m^2/s^2", "GP"},
{"007", "Geopotential height", "gpm", "HGT"},
{"008", "Geometric height", "m", "DIST"},
{"009", "Standard deviation of height", "m", "HSTDV"},
{"010", "Total ozone", "Dobson", "TOZONE"},
{"011", "Temperature", "K", "TMP"},
{"012", "Virtual temperature", "K", "VTMP"},
{"013", "Potential temperature", "K", "POT"},
{"014", "Pseudo-adiabatic potential temperature", "K", "EPOT"},
{"015", "Maximum temperature", "K", "T MAX"},
{"016", "Minimum temperature", "K", "T MIN"},
{"017", "Dew point temperature", "K", "DPT"},
{"018", "Dew point depression (or deficit)", "K", "DEPR"},
{"019", "Lapse rate", "K/m", "LAPR"},
{"020", "Visibility", "m", "VIS"},
{"021", "Radar Spectra (1)", "-", "RDSP1"},
{"022", "Radar Spectra (2)", "-", "RDSP2"},
{"023", "Radar Spectra (3)", "-", "RDSP3"},
{"024", "Parcel lifted index (to 500hPa)",  "K", "PLI"},
{"025", "Temperature anomaly", "K", "TMP_A"},
{"026", "Pressure anomaly", "Pa", "PRESA"},
{"027", "Geopotential height anomaly", "gpm", "GP_A"},
{"028", "Wave Spectra (1)", "-", "WVSP1"},
{"029", "Wave Spectra (2)", "-", "WVSP2"},
{"030", "Wave Spectra (3)", "-", "WVSP3"},
{"031", "Wind direction (from which blowing)", "deg true", "WDIR"},
{"032", "Wind speed", "m/s", "WIND"},
{"033", "u-component of wind", "m/s", "U_GRD"},
{"034", "v-component of wind", "m/s", "V_GRD"},
{"035", "Stream function", "m^2/s", "STRM"},
{"036", "Velocity potential", "m^2/s", "V_POT"},
{"037", "Montgomery stream function", "m^2/s^2", "MNTSF"},
{"038", "Sigma coord. vertical velocity", "/s", "SGCVV"},
{"039", "Pressure Vertical velocity", "Pa/s", "V_VEL"},
{"040", "Geometric Vertical velocity", "m/s", "DZDT"},
{"041", "Absolute vorticity", "1/s", "ABS_V"},
{"042", "Absolute divergence", "1/s", "ABS_D"},
{"043", "Relative vorticity", "1/s", "REL_V"},
{"044", "Relative divergence", "1/s", "REL_D"},
{"045", "Vertical u-component shear", "1/s", "VUCSH"},
{"046", "Vertical v-component shear", "1/s", "VVCSH"},
{"047", "Direction of current", "deg true", "DIR_C"},
{"048", "Speed of current", "m/s", "SP_C"},
{"049", "u-component of current", "m/s", "UOGRD"},
{"050", "v-component of current", "m/s", "VOGRD"},
{"051", "Specific humidity", "kg/kg", "SPF_H"},
{"052", "Relative humidity", "%", "R_H"},
{"053", "Humidity mixing ratio", "kg/kg", "MIXR"},
{"054", "Precipitable water", "kg/m^2", "P_WAT"},
{"055", "Vapor pressure", "Pa", "VAPP"},
{"056", "Saturation deficit", "Pa", "SAT_D"},
{"057", "Evaporation", "kg/m^2", "EVP"},
{"058", "Cloud Ice", "kg/m^2", "C_ICE"},
{"059", "Precipitation rate", "kg/m^2/s", "PRATE"},
{"060", "Thunderstorm probability", "%", "TSTM"},
{"061", "Total precipitation", "kg/m^2", "A_PCP"},
{"062", "Large scale precipitation (non-conv.)", "kg/m^2", "NCPCP"},
{"063", "Convective precipitation", "kg/m^2", "ACPCP"},
{"064", "Snowfall rate water equivalent", "kg/m^2/s", "SRWEQ"},
{"065", "Water equiv. of accum. snow depth", "kg/m^2", "WEASD"},
{"066", "Snow depth", "m", "SNO_D"},
{"067", "Mixed layer depth", "m", "MIXHT"},
{"068", "Transient thermocline depth", "m", "TTHDP"},
{"069", "Main thermocline depth", "m", "MTHD"},
{"070", "Main thermocline anomaly", "m", "MTH_A"},
{"071", "Total cloud cover", "%", "T_CDC"},
{"072", "Convective cloud cover", "%", "CDCON"},
{"073", "Low cloud cover", "%", "L_CDC"},
{"074", "Medium cloud cover", "%", "M_CDC"},
{"075", "High cloud cover", "%", "H_CDC"},
{"076", "Cloud water", "kg/m^2", "C_WAT"},
{"077", "Best lifted index (to 500hPa)", "K", "BLI"},
{"078", "Convective snow", "kg/m^2", "SNO_C"},
{"079", "Large scale snow", "kg/m^2", "SNO_L"},
{"080", "Water Temperature", "K", "WTMP"},
{"081", "Land-sea mask (land=1;sea=0)", "fraction", "LAND"},
{"082", "Deviation of sea level from mean", "m", "DSL_M"},
{"083", "Surface roughness", "m", "SFC_R"},
{"084", "Albedo", "%", "ALBDO"},
{"085", "Soil temperature", "K", "TSOIL"},
{"086", "Soil moisture content", "kg/m^2", "SOIL_M"},
{"087", "Vegetation", "%", "VEG"},
{"088", "Salinity", "kg/kg", "SALTY"},
{"089", "Density", "kg/m^3", "DEN"},
{"090", "Water runoff", "kg/m^2", "WATR"},
{"091", "Ice concentration (ice=1;no ice=0)", "fraction", "ICE_C"},
{"092", "Ice thickness", "m", "ICETK"},
{"093", "Direction of ice drift", "deg. true", "DICED"},
{"094", "Speed of ice drift", "m/s", "SICED"},
{"095", "u-component of ice drift", "m/s", "U_ICE"},
{"096", "v-component of ice drift", "m/s", "V_ICE"},
{"097", "Ice growth rate", "m/s", "ICE_G"},
{"098", "Ice divergence", "1/s", "ICE_D"},
{"099", "Snow melt", "kg/m^2", "SNO_M"},
{"100", "Significant height of combined wind", "m", "HTSGW"},
{"101", "Direction of wind waves (from which)", "deg true", "WVDIR"},
{"102", "Significant height of wind waves", "m", "WVHGT"},
{"103", "Mean period of wind waves", "s", "WVPER"},
{"104", "Direction of swell waves", "deg true", "SWDIR"},
{"105", "Significant height of swell waves", "m", "SWELL"},
{"106", "Mean period of swell waves", "s", "SWPER"},
{"107", "Primary wave direction", "deg true", "DIRPW"},
{"108", "Primary wave mean period", "s", "PERPW"},
{"109", "Secondary wave direction", "deg true", "DIRSW"},
{"110", "Secondary wave mean period", "s", "PERSW"},
{"111", "Net short-wave radiation (surface)", "W/m^2", "NSWRS"},
{"112", "Net long wave radiation (surface)", "W/m^2", "NLWRS"},
{"113", "Net short-wave radiation (top of atmos.)", "W/m^2", "NSWRT"},
{"114", "Net long wave radiation (top of atmos.),", "W/m^2", "NLWRT"},
{"115", "Long wave radiation", "W/m^2", "LWAVR"},
{"116", "Short wave radiation", "W/m^2", "SWAVR"},
{"117", "Global radiation", "W/m^2", "G_RAD"},
{"118", "Brightness temperature", "K", "BRTMP"},
{"119", "Long-wave radiation", "W/srm^2", "LWRAD"},
{"120", "Short-wave radiation", "W/srm^2", "SWRAD"},
{"121", "Latent heat net flux", "W/m^2", "LHTFL"},
{"122", "Sensible heat net flux", "W/m^2", "SHTFL"},
{"123", "Boundary layer dissipation", "W/m^2", "BLYDP"},
{"124", "Momentum flux, u component", "N/m^2", "U_FLX"},
{"125", "Momentum flux, v component", "N/m^2", "V_FLX"},
{"126", "Wind mixing energy", "J", "WMIXE"},
{"127", "Image data", "-", "IMG D"}
};

TBLE2 params_nwsnmc[] = {
{"128", "Mean Sea Level Pressure", "Pa", "MSLSA"},
{"129", "Mean Sea Level Pressure", "Pa", "MSLMA"},
{"130", "Mean Sea Level Pressure", "Pa", "MSLET"},
{"131", "Surface lifted index", "K", "LFT_X"},
{"132", "Best (4 layer)", "K", "4LFTX"},
{"133", "K index", "K", "K_X"},
{"134", "Sweat index", "K", "S_X"},
{"135", "Horizontal moisture divergence", "kg/kg/s", "MCONV"},
{"136", "Vertical speed shear", "1/s", "VW_SH"},
{"137", "3-hr pressure tendency", "Pa/s", "TSLSA"},
{"138", "Brunt-Vaisala frequency (squared)", "1/s^2", "BVF_2"},
{"139", "Potential vorticity (density weighted)", "1/s/m", "PV_MW"},
{"140", "Categorical rain  (yes=1; no=0)", "non-dim", "CRAIN"},
{"141", "Categorical freezing rain  (yes=1; no=0)", "non-dim", "CFRZRN"},
{"142", "Categorical ice pellets  (yes=1; no=0)", "non-dim", "CICEPL"},
{"143", "Categorical snow  (yes=1; no=0)", "non-dim", "CSNOW"},
{"144", "Volumetric soil moisture content", "fraction", "SOILW"},
{"145", "Potential evaporation rate", "W/m^2", "PEVPR"},
{"146", "Cloud workfunction", "J/kg", "CWORK"},
{"147", "Zonal flux of gravity wave stress", "N/m**2", "U_GWD"},
{"148", "Meridional flux of gravity wave stress", "N/m**2", "V_GWD"},
{"149", "Potential vorticity", "m**2/s/kg", "PV"},
{"150", "Covariance between meridional", "m^2/s^2", "COVMZ"},
{"151", "Covariance between temperature", "K*m/s", "COVTZ"},
{"152", "Covariance between temperature", "K*m/s", "COVTM"},
{"153", "Cloud water", "Kg/kg", "CLWMR"},
{"154", "Ozone mixing ratio", "Kg/kg", "O3MR"},
{"155", "Ground Heat Flux", "W/m^2", "GFLUX"},
{"156", "Convective inhibition", "J/kg", "CIN"},
{"157", "Convective Available Potential Energy", "J/kg", "CAPE"},
{"158", "Turbulent Kinetic Energy", "J/kg", "TKE"},
{"159", "Condensation pressure of parcel", "Pa", "CONDP"},
{"160", "Clear Sky Upward Solar Flux", "W/m^2", "CSUSF"},
{"161", "Clear Sky Downward Solar Flux", "W/m^2", "CSDSF"},
{"162", "Clear Sky upward long wave flux", "W/m^2", "CSULF"},
{"163", "Clear Sky downward long wave flux", "W/m^2", "CSDLF"},
{"164", "Cloud forcing net solar flux", "W/m^2", "CFNSF"},
{"165", "Cloud forcing net long wave flux", "W/m^2", "CFNLF"},
{"166", "Visible beam downward solar flux", "W/m^2", "VBDSF"},
{"167", "Visible diffuse downward solar flux", "W/m^2", "VDDSF"},
{"168", "Near IR beam downward solar flux", "W/m^2", "NBDSF"},
{"169", "Near IR diffuse downward solar flux", "W/m^2", "NDDSF"},
{"172", "Momentum flux", "N/m^2", "M_FLX"},
{"173", "Mass point model surface", "non-dim", "LMH"},
{"174", "Velocity point model surface", "non-dim", "LMV"},
{"175", "Model layer number (from bottom up)", "non-dim", "MLYNO"},
{"176", "latitude (-90 to +90)", "deg", "NLAT"},
{"177", "east longitude (0-360)", "deg", "ELON"},
{"181", "x-gradient of log pressure", "1/m", "LPS_X"},
{"182", "y-gradient of log pressure", "1/m", "LPS_Y"},
{"183", "x-gradient of height", "m/m", "HGT_X"},
{"184", "y-gradient of height", "m/m", "HGT_Y"},
{"189", "Virtual potential temperature", "K", "VPTMP"},
{"190", "Storm relative helicity", "m^2/s^2", "HLCY"},
{"191", "Probability from ensemble", "numeric", "PROB"},
{"192", "Probability from ensemble normalized with respect to climate expectancy", "numeric", "PROBN"},
{"193", "Probability of precipitation", "%", "POP"},
{"194", "Probability of frozen precipitation", "%", "CPOFP"},
{"195", "Probability of freezing precipitation", "%", "CPOZP"},
{"196", "u-component of storm motion", "m/s", "USTM"},
{"197", "v-component of storm motion", "m/s", "VSTM"},
{"201", "Ice-free water surface", "%", "ICWAT"},
{"204", "downward short wave rad. flux", "W/m^2", "DSWRF"},
{"205", "downward long wave rad. flux", "W/m^2", "DLWRF"},
{"206", "Ultra violet index (1 hour integration centered at solar noon)", "J/m^2", "UVI"},
{"207", "Moisture availability", "%", "MSTAV"},
{"208", "Exchange coefficient", "kg/m^3(m/s)", "SFEXC"},
{"209", "No. of mixed layers next to surface", "integer", "MIXLY"},
{"211", "upward short wave rad. flux", "W/m^2", "USWRF"},
{"212", "upward long wave rad. flux", "W/m^2", "ULWRF"},
{"213", "Amount of non-convective cloud", "%", "CDLYR"},
{"214", "Convective Precipitation rate", "kg/m^2/s", "CPRAT"},
{"215", "Temperature tendency by all physics", "K/s", "TTDIA"},
{"216", "Temperature tendency by all radiation", "K/s", "TTRAD"},
{"217", "Temperature tendency by non-radiation physics", "K/s", "TTPHY"},
{"218", "precip.index (0.0-1.00)", "fraction", "PREIX"},
{"219", "Std. dev. of IR T over 1x1 deg area", "K", "TSD1D"},
{"220", "Natural log of surface pressure", "ln(kPa)", "NLGSP"},
{"220", "Planetary boundary layer height", "m", "HPBL"},
{"222", "5-wave geopotential height", "gpm", "5WAVH"},
{"223", "Plant canopy surface water", "kg/m^2", "C_WAT"},
{"226", "Blackadar's mixing length scale", "m", "BMIXL"},
{"227", "Asymptotic mixing length scale", "m", "AMIXL"},
{"228", "Potential evaporation", "kg/m^2", "PEVAP"},
{"229", "Snow phase-change heat flux", "W/m^2", "SNOHF"},
{"231", "Convective cloud mas flux", "Pa/s", "MFLUX"},
{"232", "Downward total radiation flux", "W/m^2", "DTRF"},
{"233", "Upward total radiation flux", "W/m^2", "UTRF"},
{"234", "Baseflow-groundwater runoff", "kg/m^2", "BGRUN"},
{"235", "Storm surface runoff", "kg/m^2", "SSRUN"},
{"237", "Total ozone", "Kg/m^2", "03TOT"},
{"238", "Snow cover", "percent", "SNO_C"},
{"239", "Snow temperature", "K", "SNO_T"},
{"241", "Large scale condensat. heat rate", "K/s", "LRGHR"},
{"242", "Deep convective heating rate", "K/s", "CNVHR"},
{"243", "Deep convective moistening rate", "kg/kg/s", "CNVMR"},
{"244", "Shallow convective heating rate", "K/s", "SHAHR"},
{"245", "Shallow convective moistening rate", "kg/kg/s", "SHAMR"},
{"246", "Vertical diffusion heating rate", "K/s", "VDFHR"},
{"247", "Vertical diffusion zonal acceleration", "m/s^2", "VDFUA"},
{"248", "Vertical diffusion meridional accel", "m/s^2", "VDFVA"},
{"249", "Vertical diffusion moistening rate", "kg/kg/s", "VDFMR"},
{"250", "Solar radiative heating rate", "K/s", "SWHR"},
{"251", "long wave radiative heating rate", "K/s", "LWHR"},
{"252", "Drag coefficient", "non-dim", "CD"},
{"253", "Friction velocity", "m/s", "FRICV"},
{"254", "Richardson number", "non-dim.", "RI"}
};
int params_ecmwf_index[] = { 127, 128, 129, 130, 131, 132, 133, 134, 135,136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 151, 152, 155, 156, 157, 158, 164, 165, 166, 167, 168, 170, 171, 172, 173, 174, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185,
186,187,188,198,235,236,237
};
TBLE2 params_ecmwf[] = {
{"127","Atmospheric Tide","-","ATMOS_TIDE"},
{"128","Budget Values","-","BUDG_V"},
{"129","Geopotential","m^2/s","GP"},
{"130","Temperature","K","TMP"},
{"131","U-component of Wind","m/s","U_GRD"},
{"132","V-component of Wind","m/s","V_GRD"},
{"133","Specific Humidity","kg/kg","SPF_H"},
{"134","Surface Pressure","Pa","PRES"},
{"135","Vertical Velocity","Pa/s","V_VEL"},
{"136","Total Column Water","kg/m^2","TCW"},
{"137","Precipitable Water Content","m (of water)","P_WAT"},
{"138","Vorticity","1/s","VORT"},
{"139","Soil Temperature level 1","K","STMP"},
{"140","Soil Moisture level 1","m (of water)","SOIL_M"},
{"141","Snow Depth","m","SNO_D"},
{"142","Large Scale Precipitation","m","NCPCP"},
{"143","Convective Precipitation","m","ACPCP"},
{"144","Snow Fall","m","SNO"},
{"145","Boundary Layer Dissipation","w/m^2","BLYDP"},
{"146","Surface Flux of Sensible Heat","w/m^2","SSHTFL"},
{"147","Surface Flux of Latent Heat","w/m^2","SLHTFL"},
{"151","Mean Sea Level (MSL) Pressure","Pa","MSLP"},
{"152","Log Surface Pressure","-","LOGP"},
{"155","Divergence","1/s","ABS_D"},
{"156","Height (Geopotential)","m'","HGT"},
{"157","Relative Humidity","%","RH"},
{"158","Tendency of Surface Pressure","Pa/s","PTEND"},
{"164","Total Cloud Cover","(0 - 1)","T_CDC"},
{"165","U-wind at 10 m","m/s","U_GRD_10M"},
{"166","V-wind at 10 m","m/s","V_GRD_10M"},
{"167","Temperature at 2 m","K","TMP_2M"},
{"168","Dewpoint at 2 m","K","DPT_2M"},
{"170","Soil Temperature level 2","K","STMP2"},
{"171","Soil Wetness level 2","m (of water)","SOIL_M2"},
{"172","Land-Sea Mask","(0, 1)","LAND"},
{"173","Surface Roughness","m","SFC_R"},
{"174","Albedo","-","ALBDO"},
{"176","Net Shortwave Radiation (surface)","W/m^2","NSWRS"},
{"177","Net Longwave Radiation (surface)","W/m^2","NLWRS"},
{"178","Net Shortwave Radiation (top of atmosphere)","W/m^2","NSWRT"},
{"179","Net Longwave Radiation (top of atmosphere)","W/m^2","NLWRT"},
{"180","U-component of Surface Wind Stress","N/m^2","U_GWD"},
{"181","V-component of Surface Wind Stress","N/m^2","V_GWD"},
{"182","Evaporation","m (of water)","EVAP"},
{"183","Soil Temperature level 3","K","STMP3"},
{"184","Soil Wetness level 3","m (of water)","SOIL_M3"},
{"185","Cloud Cover","(0 - 1)","T_CDC"},
{"186","Low Cloud Cover","(0 - 1)","L_CDC"},
{"187","Medium Cloud Cover","(0 - 1)","M_CDC"},
{"188","High Cloud Cover","(0 - 1)","H_CDC"},
{"198","Skin reservoir content","m","SKRESC"},
{"235","Skin temperature","K","SKTMP"},
{"236","Soil temperature level 4","K","STMP4"},
{"237","Soil moisture level 4","m","SOIL_M4"},
};

int params_fsl_index[] = { 129,149,157,158,171,172,173,179,186,187,188,189,191,193,194,195,204,205,211,212,229,234,235,252};
TBLE2 params_fsl[] = {
{"129","MAPS mean sea level pressure","Pa","MMSP"},
{"149","potential vorticity","m**2/s/kg","PV"},
{"157","convective available potential energy","J/kg","CAPE"},
{"158","turbulent kinetic energy","J/kg","TKE"},
{"171","montgomery stream function / gravity","m","MSG"},
{"172","virtual potential temperature","deg K","VPT"},
{"173","condensation pressure","mb","CPR"},
{"179","convective inhibition","J/kg","CIN"},
{"186","cloud water mixing ratio","kg/kg","QC"},
{"187","rain water mixing ratio","kg/kg","QR"},
{"188","ice mixing ratio","kg/kg","QI"},
{"189","snow mixing ratio","kg/kg","QS"},
{"191","snow accumulation","cm","SAC"},
{"193","graupel mixing ratio","kg/kg","QQ"},
{"194","number concentration for ice particles","1/m**3","QIN"},
{"195","altimeter setting","Pa","ALT"},
{"204","time averaged downward short wave rad. flux","W/m**2","DSWRF"},
{"205","time averaged downward long wave rad. flux","W/m**2","DLWRF"},
{"211","time averaged upward short wave rad. flux","W/m**2","USWRF"},
{"212","time averaged upward long wave rad. flux","W/m**2","ULWRF"},
{"229","time averaged snow phase change heat flux","W/m**2","SHOHF"},
{"234","sub-surface runoff","kg/m**2","RNF"},
{"235","surface runoff","kg/m**2","SFCRNF"},
{"252","surface drag coefficient","non-dim","SDRCO"},
};
int level_index[] = {1,2,3,4,5,6,7,8,9,20,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,125,128,141,160,200,201};

char *level_str[] = {
	"SFC",		/*1*/
	"CBL",		/*2*/
	"CTL",		/*3*/
	"0DEG",		/*4*/
	"ADCL",		/*5*/
	"MWSL",		/*6*/
	"TRO",		/*7*/
	"NTAT",		/*8*/
	"SEAB",		/*9*/
	"TMPL",		/*20*/
        "ISBL", 	/*100*/
        "ISBY", 	/*101*/
        "MSL", 	/*102*/
        "GPML",	/*103*/
        "GPMY",	/*104*/
        "HTGL",	/*105*/
        "HTGY",/*106*/
	"SIGL",		/*107*/
	"SIGY",	/*108*/
	"HYBL",		/*109*/
	"HYBY",/*110*/
	"DBLL",	/*111*/
	"DBLY", /*112*/
	"THEL",	/*113*/
	"THEY", /*114*/
	"SPDL",	/*115*/
	"SPDY", /*116*/
	"PVL", /*117*/
	"ETAL", /*119*/
	"ETAY", /*120*/
	"IBYH", 	/*121*/
	"HGLH",	/*125*/
	"SGYH", /*128*/
	"IBYM", /*141*/
	"DBSL",	/*160*/
	"EATM",	/*200*/
	"EOCN",		/*201*/
};

char *level_str_long_name[] = {
	"surface of the earth including sea surface", /*1*/
	"cloud base level",	/*2*/
	"cloud top level",	/*3*/
	"0 deg (C) isotherm level (lowest)", /*4*/
	"adiabatic condensation level (parcel lifted from surface)", /*5*/
	"maximum wind speed level", /*6*/
	"tropopause level",	/*7*/
	"nominal top of atmosphere", /*8*/
	"sea bottom", /*9*/
	"temperature in 1/100 K", /*20*/
	"isobaric level",	/*100*/
	"layer between tow isobaric levels",	/*101*/
	"mean sea level",	/*102*/
	"fixed height level",	/*103*/
	"layer between two height levels above msl",	/*104*/
	"fixed height above ground",	/*105*/
	"layer between two height levels above ground",	/*106*/
	"sigma level",	/*107*/
	"layer between two sigma levels",	/*108*/
	"Hybrid level",	/*109*/
	"layer between two hybrid levels",	/*110*/
	"depth below land surface",	/*111*/
	"layer between two depths below land surface",	/*112*/
	"isentropic (theta) level",	/*113*/
	"layer between two isentropic levels",	/*114*/
	"level at specified pressure difference from ground to level",	/*115*/
	"layer between two levels at specified pressure difference from ground to level",	/*116*/
	"potential vorticity (pv) level",	/*117*/
	"ETA level",	/*119*/
	"layer between two ETA levels",	/*120*/
	"layer between two isobaric surfaces (high precision)",	/*121*/
	"height level above ground (high precision)",	/*125*/
	"layer between two sigma levels (high precision)",	/*128*/
	"layer between two isobaric surfaces (mixed precision)",	/*141*/
	"depth below sea level",	/*160*/
	"entire atmosphere considered as a single layer",	/*200*/
	"entire ocean considered as a single layer"	/*201*/
};
	
