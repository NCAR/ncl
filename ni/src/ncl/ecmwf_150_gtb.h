/* 
 * European Centre for Medium-Range Weather Forecasts: Reading
 * Center: 98
 * Subcenter: 0
 * Parameter table version: 150
 * Usage:Ocean - preliminary 
 */ 

TBLE2 ecmwf_150_params[] = {
{129, "Ocean potential temperature", "deg C", "POT_OC"},
{130, "Ocean salinity", "psu", "SALTY_OC"},
{131, "Ocean potential density(reference = surface)", "kg m**-3 -1000", "PDEN_OC"},
{133, "Ocean u velocity", "m s**-1", "UVEL_OC"},
{134, "Ocean v velocity", "m s**-1", "VVEL_OC"},
{135, "Ocean w velocity", "m s**-1", "WVEL_OC"},
{137, "Richardson number", "-", "RI"},
{139, "u*v product", "m s**-2", "UV"},
{140, "u*T product", "m s**-1 deg C", "UT"},
{141, "v*T product", "m s**-1 deg C", "VT"},
{142, "u*u product", "m s**-2", "UU"},
{143, "v*v product", "m s**-2", "VV"},
{144, "uv - u~v~ (u~ is time-mean of u)", "m s**-2", "UV_TMEAN"},
{145, "uT - u~T~", "m s**-1 deg C", "UT_TMEAN"},
{146, "vT - v~T~", "m s**-1 deg C", "VT_TMEAN"},
{147, "uu - u~u~", "m s**-2", "UU_TMEAN"},
{148, "vv - v~v~", "m s**-2", "VV_TMEAN"},
{152, "Sea level (departure from geoid tides removed)", "-", "SL_DEP"},
{153, "Barotropic stream function", "-", "BSTRM"},
{154, "Mixed layer depth (Tcr=0.5 C for HOPE model)", "m", "MIXHT"},
{155, "Depth (eg of isothermal surface)", "m", "DEPTH"},
{168, "U-stress", "Pa", "USTR"},
{169, "V-stress", "Pa", "VSTR"},
{170, "Turbulent Kinetic Energy input", "-", "TKE"},
{171, "Net surface heat flux (+ve = down)", "-", "NSHFLX"},
{172, "Surface solar radiation", "-", "SSRAD"},
{173, "P-E", "-", "PE"},
{180, "Diagnosed SST eror", "deg C", "SST_ERR"},
{181, "Heat flux correction", "W m**-2", "HFLX_COR"},
{182, "Observed SST deg", "C", "SST_OB"},
{183, "Observed heat flux", "W m**-2", "HFLX_OB"},
};
