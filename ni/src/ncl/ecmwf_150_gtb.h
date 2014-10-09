/* 
 * European Centre for Medium-Range Weather Forecasts: Reading
 * Center: 98
 * Subcenter: 0
 * Parameter table version: 150
 * Usage:Ocean - preliminary 
 */ 

TBLE2 ecmwf_150_params[] = {
{129, "Ocean potential temperature", "deg C", "OCPT"},
{130, "Ocean salinity", "psu", "OCS"},
{131, "Ocean potential density(reference = surface)", "kg m**-3 -1000", "OCPD"},
{133, "Ocean u velocity", "m s**-1", "OCU"},
{134, "Ocean v velocity", "m s**-1", "OCV"},
{135, "Ocean w velocity", "m s**-1", "OC@"},
{137, "Richardson number", "-", "RN"},
{139, "u*v product", "m s**-2", "UV"},
{140, "u*T product", "m s**-1 deg C", "UT"},
{141, "v*T product", "m s**-1 deg C", "VT"},
{142, "u*u product", "m s**-2", "UU"},
{143, "v*v product", "m s**-2", "VV"},
{144, "UV - U", "m s**-2", "UVMU"},
{145, "UT - U", "m s**-1 deg C", "UTMU"},
{146, "VT - V", "m s**-1 deg C", "VTMV"},
{147, "UU - U", "m s**-2", "UUMU"},
{148, "VV - V", "m s**-2", "VVMV"},
{152, "Sea level", "m", "SL"},
{153, "Barotropic stream function", "-", "BSTRM"},
{154, "Mixed layer depth", "m", "MLD"},
{155, "Depth", "m", "DEPTH"},
{168, "U-stress", "Pa", "USTR"},
{169, "V-stress", "Pa", "VSTR"},
{170, "Turbulent Kinetic Energy input", "-", "TKE"},
{171, "Net surface heat flux", "-", "NSF"},
{172, "Surface solar radiation", "-", "SSRAD"},
{173, "P-E", "-", "PE"},
{180, "Diagnosed sea surface temperature  eror", "deg C", "SST_ERR"},
{181, "Heat flux correction", "W m**-2", "HFLX_COR"},
{182, "Observed sea surface temperature", "deg C", "SST_OB"},
{183, "Observed heat flux", "W m**-2", "HFLX_OB"},
};
