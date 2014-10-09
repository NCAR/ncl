/*
 *
 * European Centre for Medium-Range Weather Forecasts: Reading
 * Non-ECMWF seasonal forecast model
 *
 * Center: 98
 * Subcenter: 0
 * Parameter table version: 174
 */

TBLE2 ecmwf_174_params[] = {
{6, "Total soil moisture", "m", ""},
{8, "Surface runoff", "kg m**-2", "SRO"},
{9, "Sub-surface runoff", "kg m**-2", "SSRO"},
{10, "Clear-sky", "W m**-2", "SSWCSDOWN"},
{13, "Clear-sky", "W m**-2", "SSWCSUP"},
{25, "Visibility at 1", "m", "VIS15"},
{31, "Fraction of sea-ice in sea", "(0 - 1)", ""},
{34, "Open-sea surface temperature", "K", ""},
{39, "Volumetric soil water layer 1", "m**3 m**-3", ""},
{40, "Volumetric soil water layer 2", "m**3 m**-3", ""},
{41, "Volumetric soil water layer 3", "m**3 m**-3", ""},
{42, "Volumetric soil water layer 4", "m**3 m**-3", ""},
{49, "10 metre wind gust over last 24 hours", "m s**-1", ""},
{50, "Minimum temperature at 1.5m since previous post-processing", "K", "MN15T"},
{51, "Maximum temperature at 1.5m since previous post-processing", "K", "MX15T"},
{52, "Relative humidity at 1.5m", "kg kg**-1", "RHUM"},
{55, "1.5m temperature - mean over last 24 hours", "K", ""},
{83, "Net primary productivity", "kg C m**-2 s**-1", ""},
{85, "10m U wind over land", "m s**-1", ""},
{86, "10m V wind over land", "m s**-1", ""},
{87, "1.5m temperature over land", "K", ""},
{88, "1.5m dewpoint temperature over land", "K", ""},
{89, "Top incoming solar radiation", "W m**-2 s", ""},
{90, "Top outgoing solar radiation", "W m**-2 s", ""},
{94, "Mean sea surface temperature", "K", ""},
{95, "1.5m specific humidity", "kg kg**-1", ""},
{98, "Sea-ice thickness", "m", ""},
{99, "Liquid water potential temperature", "K", ""},
{110, "Ocean ice concentration", "(0 - 1)", ""},
{111, "Ocean mean ice depth", "m", ""},
{116, "Short wave radiation flux at surface", "J m**-2", "SWRSURF"},
{117, "Short wave radiation flux at top of atmosphere", "J m**-2", "SWRTOP"},
{137, "Total column water vapour", "kg m**-2", "TCWVAP"},
{139, "Soil temperature layer 1", "K", ""},
{142, "Large scale rainfall rate", "kg m**-2 s**-1", "LSRRATE"},
{143, "Convective rainfall rate", "kg m**-2 s**-1", "CRFRATE"},
{164, "Average potential temperature in upper 293.4m", "degrees C", ""},
{167, "1.5m temperature", "K", ""},
{168, "1.5m dewpoint temperature", "K", ""},
{170, "Soil temperature layer 2", "K", ""},
{172, "Land-sea mask", "(0 - 1)", "LSM"},
{175, "Average salinity in upper 293.4m", "psu", ""},
{183, "Soil temperature layer 3", "K", ""},
{186, "Very low cloud amount", "(0 - 1)", "VLCA"},
{201, "1.5m temperature - maximum over last 24 hours", "K", ""},
{202, "1.5m temperature - minimum over last 24 hours", "K", ""},
{236, "Soil temperature layer 4", "K", ""},
{239, "Convective snowfall rate", "kg m**-2 s**-1", "CSFRATE"},
{240, "Large scale snowfall rate", "kg m**-2 s**-1", "LSFRATE"},
{248, "Total cloud amount - random overlap", "(0 - 1)", "TCCRO"},
{249, "Total cloud amount in lw radiation", "(0 - 1)", "TCCLWR"},
};
