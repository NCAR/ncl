typedef struct _GribTable {
	int index;
	char *name;
} GribTable;

GribTable centers[] = {
	{1, "Melbourne (WMC)"},
	{2, "Melbourne (WMC)"},
	{4, "Moscow (WMC)"},
	{5, "Moscow (WMC)"},
	{7, "US National Weather Service - NCEP (WMC)"},
	{8, "US National Weather Service - NWSTG (WMC)"},
	{9, "US National Weather Service - Other (WMC)"},
	{10, "Cairo (RSMC/RAFC)"},
	{12, "Dakar (RSMC/RAFC)"},
	{14, "Nairobi (RSMC/RAFC)"},
	{16, "Atananarivo (RSMC)"},
	{18, "Tunis-Casablanca (RSMC)"},
	{20, "Las Palmas (RAFC)"},
	{21, "Algiers (RSMC)"},
	{22, "Lagos (RSMC)"},
	{26, "Khabarovsk (RSMC)"},
	{28, "New Delhi (RSMC/RAFC)"},
	{30, "Novosibirsk (RSMC)"},
	{32, "Tashkent (RSMC)"},
	{33, "Jeddah (RSMC)"},
	{34, "Japanese Meteorological Agency - Tokyo (RSMC)"},
	{36, "Bankok"},
	{37, "Ulan Bator"},
	{38, "Beijing (RSMC)"},
	{40, "Seoul"},
	{41, "Buenos Aires (RSMC/RAFC)"},
	{43, "Brasilia (RSMC/RAFC)"},
	{45, "Santiago"},
	{46, "Brasilian Space Agency - INPE"},
	{51, "Miami (RSMC/RAFC)"},
	{52, "National Hurricane Center, Miami"},
	{53, "Canadian Meteorological Service - Montreal (RSMC)"},
	{54, "Canadian Meteorological Service - Montreal (RSMC)"},
	{55, "San Francisco"},
	{57, "U.S. Air Force - Global Weather Center"},
	{58, "US Navy  - Fleet Numerical Oceanography Center"},
	{59, "NOAA Forecast Systems Lab, Boulder CO"},
	{60, "National Center for Atmospheric Research (NCAR), Boulder, CO"},
	{64, "Honolulu"},
	{65, "Darwin (RSMC)"},
	{67, "Melbourne (RSMC)"},
	{69, "Wellington (RSMC/RAFC)"},
	{74, "U.K. Met Office - Bracknell"},
	{76, "Moscow (RSMC/RAFC)"},
	{78, "Offenbach (RSMC)"},
	{80, "Rome (RSMC)"},
	{82, "Norrkoping"},
	{85, "French Weather Service - Toulouse"},
	{86, "Helsinki"},
	{87, "Belgrade"},
	{88, "Oslo"},
	{89, "Prague"},
	{90, "Episkopi"},
	{91, "Ankara"},
	{92, "Frankfurt/Main (RAFC)"},
	{93, "London (WAFC)"},
	{94, "Copenhagen"},
	{95, "Rota"},
	{96, "Athens"},
	{97, "European Space Agency (ESA)"},
	{98, "European Center for Medium-Range Weather Forecasts - Reading"},
	{99, "DeBilt, Netherlands"},
};


/* Table 0 Part 2 */

GribTable sub_centers[] = {
        {1, "NCEP Re-Analysis Project"},
        {2, "NCEP Ensemble Products"},
        {3, "NCEP Central Operations"},
        {4, "Environmental Modeling Center"},
        {5, "Hydrometeorological Prediction Center"},
        {6, "Marine Prediction Center"},
        {7, "Climate Prediction Center"},
        {8, "Aviation Weather Center"},
        {9, "Storm Prediction Center"},
        {10, "Tropical Prediction Center"},
        {11, "NWS Techniques Development Laboratory"},
        {12, "NESDIS Office of Research and Applications"},
        {13, "FAA"},
        {14, "NWS Meteorological Development Laboratory"},
        {15, "The North American Regional Reanalysis (NARR) Project"},
	{150, "ABRFC - Arkansas-Red River RFC, Tulsa OK"},
	{151, "Alaska RFC, Anchorage, AK"}, 
	{152, "CBRFC - Colorado Basin RFC, Salt Lake City, UT"},
	{153, "CNRFC - California-Nevada RFC, Sacramento, CA"}, 
	{154, "LMRFC - Lower Mississippi RFC, Slidel, LA"},
	{155, "MARFC - Middle Atlantic RFC, State College, PA"},
	{156, "MBRFC - Missouri Basin RFC, Kansas City, MO"},
	{157, "NCRFC - North Central RFC, Minneapolic, MN"},
	{158, "NERFC - Northeast RFC, Hartford, CT"},
	{159, "NWRFC - Northwest RFC, Portland, OR"},
	{160, "OHRFC - Ohio Basin RFC, Cincinnati, OH"},
	{161, "SERFC - Southeast RFC, Atlanta, GA"},
	{162, "WGRFC - West Gulf RFC, Fort Worth, TX"},
	{170, "OUN - Norman OK WFO"} 
};


/* Table A */
GribTable models[] = {
	{2, "Ultra Violet Index Model"},
	{3, "NCEP/ARL Transport and Dispersion Model"},
	{5, "Satellite Derived Precipitation and temperatures, from IR"},
	{10, "Global Wind-Wave Forecast Model"},
	{19, "Limited-area Fine Mesh (LFM) analysis"},
	{25, "Snow Cover Analysis"},
	{30, "Forecaster generated field"},
	{31, "Value added post processed field"},
	{39, "Nested Grid forecast Model (NGM)"},
	{42, "Global Optimum Interpolation Analysis (GOI) from \"Aviation\" run"},
	{43, "Global Optimum Interpolation Analysis (GOI) from \"Final\" run"},
	{44, "Sea Surface Temperature Analysis"},
	{45, "Coastal Ocean Circulation Model"},
	{49, "Ozone Analysis from TIROS Observations"},
	{52, "Ozone Analysis from Nimbus 7 Observations"},
	{53, "LFM-Fourth Order Forecast Model"},
	{64, "Regional Optimum Interpolation Analysis (ROI)"},
	{68, "80 wave triangular, 18-layer Spectral model from \"Aviation\" run"},
	{69, "80 wave triangular, 18 layer Spectral model from \"Medium Range Forecast\" run"},
	{70, "Quasi-Lagrangian Hurricane Model (QLM)"},
	{73, "Fog Forecast model - Ocean Prod. Center"},
	{74, "Gulf of Mexico Wind/Wave"},
	{75, "Gulf of Alaska Wind/Wave"},
	{76, "Bias corrected Medium Range Forecast"},
	{77, "126 wave triangular, 28 layer Spectral model from \"Aviation\" run"},
	{79, "Backup from the previous run"},
	{80, "62 wave triangular, 28 layer Spectral model from \"Medium Range Forecast\" run"},
	{81, "Spectral Statistical Interpolation (SSI) analysis from \"Aviation\" run."},
	{82, "Spectral Statistical Interpolation (SSI) analysis from \"Final\" run."},
	{83, "ETA Model - 80 km version"},
	{84, "MESO ETA Model"},
	{85, "ETA Model - 30 km version"},
	{86, "RUC Model, from Forecast Systems Lab (isentropic; scale: 60km at 40N)"},
	{87, "CAC Ensemble Forecasts from Spectral (ENSMB)"},
	{88, "NOAA Wave Watch III (NWW3) Ocean Wave Model"},
	{92, "62 wave triangular, 28 layer spectral model run from the \"Medium Range Forecast\" final analysis"},
	{94, "T170/L42 Global Spectral Model from MRF run"},
	{95, "T126/L42 Global Spectral Model from MRF run"},
	{96, "Global Forecast System Model (formerly known as the Aviation), T254 - Forecast hours 00-84, T170 - Forecast hours 87-180, T126 - Forecast hours 192 - 384"},
	{100, "RUC Surface Analysis (scale: 60km at 40N)"},
	{101, "RUC Surface Analysis (scale: 40km at 40N)"},
	{105, "RUC Model from FSL (isentropic; scale: 20km at 40N)"},
	{110, "ETA Model - 15km version"},
	{120, "Ice Concentration Analysis"},
	{121, "Western North Atlantic Regional Wave Model"},
	{122, "Alaska Waters Regional Wave Model"},
	{123, "North Atlantic Hurricane Wave Model"},
	{124, "Merge of fields from the RUC, Eta, and Spectral Model"},
	{140, "North American Regional Reanalysis (NARR)"},
	{141, "Land Data Assimilation and Forecast System"},
	{150, "NWS River Forecast System (NWSRFS)"},
	{151, "NWS Flash Flood Guidance System (NWSFFGS)"},
	{152, "WSR-88D Stage II Precipitation Analysis"},
	{153, "WSR-88D Stage III Precipitation Analysis"},
	{180, "Quantitative Precipitation Forecast generated by NCEP"},
	{181, "River Forecast Center Quantitative Precipitation Forecast mosaic generated by NCEP"},
	{182, "River Forecast Center Quantitative Precipitation estimate mosaic generated by NCEP"},
	{183, "NDFD product generated by NCEP/HPC"},
	{190, "National Convective Weather Diagnostic generated by NCEP/AWC"},
	{191, "CPC Manual Forecast Product"}
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
	"HYBL",		/*109*/ /* If you change this you better change _Get109A in NclGRIB.c */
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
char *level_units_str[] = {
	"none" /*"SFC",	1*/,
	"none" /*"CBL",	2*/,
	"none" /*"CTL",	3*/,
	"none" /*"0DEG",	4*/,
	"none" /*"ADCL",	5*/,
	"none" /*"MWSL",	6*/,
	"none" /*"TRO",	7*/,
	"none" /*"NTAT",	8*/,
	"none" /*"SEAB",	9*/,
	"(1/100) K"/*"TMPL",	20*/,
        "hPa" /*"ISBL", 	100*/,
        "hPa" /*"ISBY", 	101*/,
        "hPa" /*"MSL", 	102*/,
        "m" /*"GPML",	103*/,
        "hm" /*"GPMY",	104*/,
        "m" /*"HTGL",	105*/,
        "hm" /*"HTGY",106*/,
	"sigma" /*"SIGL",		107*/,
	"sigma" /*"SIGY",	108*/,
	"number" /*"HYBL",		109*/ /* If you change this you better change _Get109A in NclGRIB.c */,
	"number" /*"HYBY", 110*/,
	"cm" /*"DBLL",	111*/,
	"cm" /*"DBLY", 112*/,
	"K" /*"THEL",	113*/,
	"K" /*"THEY", 114*/,
	"hPa" /*"SPDL",	115*/,
	"hPa" /*"SPDY", 116*/,
	"10^-6Km^2/kgs" /*"PVL", 117*/,
	"ETA value" /*"ETAL", 119*/,
	"ETA value" /*"ETAY", 120*/,
	"hPa" /*"IBYH", 	121*/,
	"cm" /*"HGLH",	125*/,
	"1.1 - sigma" /*"SGYH", 128*/,
	"hPa" /*"IBYM", 141*/,
	"m" /*"DBSL",	160*/,
	"none" /*"EATM",	200*/,
	"none" /*"EOCN",		201*/
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
	
