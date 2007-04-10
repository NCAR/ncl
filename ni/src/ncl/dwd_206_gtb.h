/*
 * Deutscher Wetterdienst (DWD): Offenbach
 * Center: 78
 * Subcenter: 0
 * Parameter table version: 206
 */

TBLE2 dwd_206_params[] = {
{2,   "pressure reduced to MSL (smoothed)",  "Pa", "PRESSURE_S"},
{8,   "geometrical height (smoothed)",  "m", "GEOMET_H_S"},
{11,  "temperature (smoothed)", "K", "TEMPERAT_S"},
{15,  "maximum temperature (smoothed)",  "K", "MAX_TEMP_S"},
{16,  "minimum temperature (smoothed)",  "K", "MIN_TEMP_S"},
{17,  "dew-point temperature (smoothed)",  "K", "D_P_TEMP_S"},
{20,  "visibility (smoothed)",  "m", "VISIBILI_S"},
{33,  "u-component of wind (smoothed)",  "m/s", "WIND_U_S"},
{34,  "v-component of wind (smoothed)",  "m/s", "WIND_V_S"},
{61,  "total precipitation (smoothed)",  "kg/(m**2)", "TOT_PREC_S"},
{65,  "water equivalent of accum. snow depth (smoothed)",    "kg/(m**2)", "SN_DEPWE_S"},
{71,  "total cloud cover (smoothed)",  "%", "CL_COV_S"},
{73,  "low cloud cover (smoothed)",  "%", "CL_COVCL_S"},
{74,  "medium cloud cover (smoothed)",  "%", "CL_COVCM_S"},
{75,  "high cloud cover (smoothed)",  "%", "CL_COVCH_S"},
{79,  "large scale snow (smoothed)", "kg/(m**2)", "LS_SNOW_S"},
{81,  "land cover (1=land, 0=sea) (smoothed)",  "proportion", "LAND_COV_S"},
{84,  "albedo (smoothed) ",  "%", "ALBEDO_S"},
{85,  "soil temperature (smoothed)",  "K", "SOIL_TE_S"},
{111, "net short-wave radiation flux (surface)(smoothed)",  "W/(m**2)", "NETSWRAD_S"},
{112, "net long-wave radiation flux (surface)(smoothed)",  "W/(m**2)", "NETLWRAD_S"},
{117, "global radiation flux (smoothed)",  "W/(m**2)", "GLOB_RAD_S"},
{118, "brightness temperature (smoothed)",  "K", "BRIGHT_T_S"},
{187, "maximum wind speed (smoothed)",  "m/s", "MAXWIND_S"},
};
