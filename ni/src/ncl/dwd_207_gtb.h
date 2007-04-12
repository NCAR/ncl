/*
 * Deutscher Wetterdienst (DWD): Offenbach
 * Center: 78
 * Subcenter: 0
 * Parameter table version: 207
 */

TBLE2 dwd_207_params[] = {
{2,   "pressure reduced to MSL (calibrated)", "Pa", "PRESSURE_C"},
{8,   "geometrical height (calibrated)", "m", "GEOMET_H_C"},
{11,  "temperature (calibrated)",    "K", "TEMPERAT_C"},
{15,  "maximum temperature (calibrated)",    "K", "MAX_TEMP_C"},
{16,  "minimum temperature (calibrated)",    "K", "MIN_TEMP_C"},
{17,  "dew-point temperature (calibrated)",  "K", "D_P_TEMP"},
{20,  "visibility (calibrated)", "m", "VISIBILI_C"},
{33,  "u-component of wind (calibrated)", "m/s", "WIND_U_C"},
{34,  "v-component of wind (calibrated)", "m/s", "WIND_V_C"},
{61,  "total precipitation (calibrated)",    "kg/(m**2)", "TOT_PREC_C"},
{65,  "water equivalent of accum. snow depth (calibrated)",  "kg/(m**2)", "SN_DEPWE_C"},
{71,  "total cloud cover (calibrated)",  "%", "CL_COV_C"},
{73,  "low cloud cover (calibrated)",    "%", "CL_COVCL_C"},
{74,  "medium cloud cover (calibrated)", "%", "CL_COVCM_C"},
{75,  "high cloud cover (calibrated)",   "%", "CL_COVCH_C"},
{79,  "large scale snow (calibrated)",   "kg/(m**2)", "LS_SNOW_C"},
{81,  "land cover (1=land, 0=sea) (calibrated)", "proportion", "LAND_COV_C"},
{84,  "albedo (calibrated)", "%", "ALBEDO_C"},
{85,  "soil temperature (calibrated)",   "K", "SOIL_TE_C"},
{111, "net short-wave radiation flux (surf.)(calibrated)",   "W/(m**2)", "NETSWRAD_C"},
{112, "net long-wave radiation flux (surface)(calibrated)",  "W/(m**2)", "NETLWRAD_C"},
{117, "global radiation flux (calibrated)",  "W/(m**2)", "GLOB_RAD_C"},
{118, "brightness temperature (calibrated)", "K", "BRIGHT_T_C"},
{187, "maximum wind speed (smoothed)",   "m/s", "MAXWIND_C"},
};
