/* Table 0 Part 1 */
int centers_index[] = { 7, 8, 9, 34, 46, 52, 54, 57, 58, 59, 60, 74, 78, 85, 97, 98, 99, 128, 129 };
char *centers[] = {
			"US Weather Service - National Met. Center",	/*07*/
			"US Weather Service - NWS Telecomms Gateway",	/*08*/
			"US Weather Service - Field Stations",		/*09*/
			"Japanese Meteorological Agency - Tokyo",	/*34*/
			"Brasilian Space Agency - INPE/CPTEC", /*46*/
			"National Hurricane Center, Miami",	/*52*/
			"Canadian Meteorological Service - Montreal",	/*54*/
			"US Air Force - Global Weather Center",	/*57*/
			"US Navy - Fleet Numerical Meteorology and Oceanography Center",	/*58*/
			"NOAA Forecast Systems Lab, Boulder, CO",	/*59*/
			"National Center for Atmospheric Research (NCAR), Boulder, CO",	/*60*/
			"U.K. Met Office - Bracknell",	/*74*/
			"Offenbach (DWD)", /*78*/
			"French Weather Service - Toulouse",	/*85*/
			"European Space Agency",	/*97*/
			"European Center for Medium-Range Weather Forecasts - Reading",	/*98*/
			"DeBilt, Netherlands",	/*99*/
			"Naval Research Laboratory - Monterey, CA", /*128*/
			"Center for Air/Sea Technology" /*129**/
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
	
