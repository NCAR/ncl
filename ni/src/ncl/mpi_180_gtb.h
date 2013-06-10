/*
 * Center: 98
 * Subcenter: 232 
 * Parameter table version: 180
 */

TBLE2 mpi_180_params[] = {
{10, "Potential Natural Land Cover Fraction", "-", "cover_fract_pot"},
{12, "Land Cover Fraction", "-", "cover_fract"},
{14, "Surface Albedo in the Visible Range", "-", "albedo_vis"},
{15, "Surface Albedo in the NIR", "-", "albedo_nir"},
{20, "Maximum Vegetation Fraction", "-", "veg_ratio_max"},
{21, "Surface Downwelling Solar Radiation", "W m-2", "swdown_acc"},
{22, "Surface Upwelling Solar Radiation", "W m-2", "swdown_reflect_acc"},
{24, "vegetated fraction of grid box", "-", "box_veg_ratio"},
{40, "Ground Heat Flux (avg)", "W/m^2", "ground_heat_flux"},
{44, "Evapotranspiration (avg)", "kg/m^2s", "evapotranspiration"},
{55, "Water Content of the Skin Reservoir", "m", "skin_reservoir"},
{56, "Relative Humidity", "-", "relative_humidity_air_inst"},
{60, "Snow Cover Fraction", "-", "snow_fract"},
{67, "Glacier Runoff", "kg/m^2s", "glacier_runoff"},
{68, "Soil Temperature", "K", "soil_temperature"},
{76, "Transpiration (avg)", "kg/m^2s", "transpiration"},
{77, "Non-dimensional Snow Age", "-", "snow_age"},
{107, "Leaf Area Index", "-", "lai"},
{107, "Leaf Area Index", "-", "lai"},
{109, "Snow Depth on Canopy", "m", "snow_depth_canopy"},
{109, "Snow Depth on Canopy", "m", "snow_depth_canopy"},
{110, "Snow Fraction on Canopy", "-", "snow_fract_canopy"},
{110, "Snow Fraction on Canopy", "-", "snow_fract_canopy"},
{116, "Correction factor for cover fraction 1-exp(-LAI_max/2)", "-", "veg_fract_correction"},
{120, "Canopy Conductance", "m/s", "canopy_conductance"},
{124, "Canopy Net Carbon Assimilation (avg.)", "mol(CO2) m-2(grid box) s-1", "net_assimilation"},
{124, "Canopy Net Carbon Assimilation", "mol(CO2) m-2(canopy) s-1", "net_assimilation_acc"},
{126, "Gross Assimilation (avg.)", "mol(CO2) m-2(grid box) s-1", "gross_assimilation"},
{126, "Gross Assimilation (avg.)", "mol(CO2) m-2(canopy) s-1", "gross_assimilation_acc"},
{148, "PAR Absorbed by Canopy (avg.)", "mol(PHOTONS) m-2(canopy) s-1", "apar_acc"},
{148, "PAR absorbed by Canopy (avg.)", "mol(PHOTONS) m-2(canopy) s-1", "apar_acc"},
{149, "incoming PAR (avg.)", "mol(PHOTONS) m-2(canopy) s-1", "par_acc"},
{160, "Net CO2 flux to atmosphere", "mol(CO2) m-2(grid box) s-1", "zCO2_flux_net"},
{161, "Grazing CO2 flux to atmosphere", "mol(CO2) m-2(grid box) s-1", "zCO2_flux_herbivory"},
{162, "Land-cover change CO2 flux to the atmosphere", "mol(CO2) m-2(grid box) s-1", "zCO2_emission_landcover_change"},
{163, "Harvest CO2 flux to atmosphere", "mol(CO2) m-2(grid box) s-1", "zCO2_emission_harvest"},
{164, "Disturbance CO2 flux to atmosphere", "mol(CO2) m-2(grid box) s-1", "zCO2_flux_dynveg"}
};
