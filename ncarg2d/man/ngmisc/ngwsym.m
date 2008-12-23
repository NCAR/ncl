.TH NGWSYM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGWSYM - Draws a symbol from the standard WMO/NOAA meteorological
fonts by reference to the font name and symbol number within that
font.
.SH SYNOPSIS
CALL NGWSYM(FTYPE,NUM,X,Y,SIZE,ICOLOR,IALT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngwsym(char *ftype, int num, float x, float y, 
.br
float size,  
int icolor, int ialt)
.SH DESCRIPTION 
.IP FTYPE 12
(an input parameter of type CHARACTER) specifying the desired font.  Legal
values are:
.RS
.IP "'WW'" 
\-  Present weather.
.IP "'C'" 
\-  Cloud types.
.IP "'CL'" 
\-  Low clouds.
.IP "'CM'" 
\-  Medium clouds.
.IP "'CH'" 
\-  High clouds.
.IP "'W'" 
\-  Past weather.
.IP "'N'" 
\-  Sky cover.
.IP "'a'" 
\-  Pressure tendency.
.RE
.IP NUM 12
(an input parameter of type INTEGER) specifying the number of the
desired symbol within the specified font.
.IP X 12
(an input parameter of type REAL) specifying the X coordinate 
position, in world coordinates, where the symbol is to be positioned.  
This X position marks the horizontal center of the symbol.
.IP Y 12
(an input parameter of type REAL) specifying the Y coordinate 
position, in world coordinates, where the symbol is to be positioned.  
This Y position marks the vertical center of the symbol.
.IP SIZE 12
(an input parameter of type REAL) the value of which is the height, in 
world coordinates, of the symbol.
.IP ICOLOR 12
(an input parameter of type INTEGER) the value of which is the GKS 
color index specifying what color the symbol will be.
.IP IALT 12
(an input parameter of type INTEGER) indicating whether an alternate
representation for the specified symbol is to be used.   If IALT=1, then 
the alternate symbol for the one specified is drawn.  This applies 
only to a few symbols such as numbers 7, 93, 94, 95, 97 in the WW font 
and number 3 in the W font.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This function simply looks up the appropriate symbol in either
font 36 or 37 of Plotchar and uses Plotchar to draw the symbol.
.sp
To produce a table of all the weather symbols broken down by font and symbol 
number, execute "ncargex fngwsym".
.sp
For the WW (present weather) font, the descriptions for the symbols are:
.sp
.IP "  0" 6
Cloud development NOT observed or NOT observable during past hour
.IP "  1" 6
Clouds generally dissolving or becoming less developed during past hour
.IP "  2" 6
State of sky on the whole unchanged during past hour
.IP "  3" 6
Clouds generally forming or developing during past hour
.IP "  4" 6
Visibility reduced by smoke
.IP "  5" 6
Haze
.IP "  6" 6
Widespread dust in suspension in the air, NOT raised by wind, 
at time of observation
.IP "  7" 6
Dust or sand raised by wind at time of observation
.IP "  8" 6
Well-developed dust whirl(s) within past hour
.IP "  9" 6
Dust storm or sandstorm within sight of or at station during past hour
.IP " 10" 6
Light fog (mist)
.IP " 11" 6
Patches of shallow fog at station, NOT deeper than 6 feet on land
.IP " 12" 6
More or less continuous shallow fog at station, NOT deeper than 6 feet
on land
.IP " 13" 6
Lightning visible, no thunder heard
.IP " 14" 6
Precipitation within sight, but NOT reaching the ground
.IP " 15" 6
Precipitation within sight, reaching the ground but distant from station
.IP " 16" 6
Precipitation within sight, reaching the ground, near to but NOT at
station
.IP " 17" 6
Thunderstorm, but no precipitation at the station
.IP " 18" 6
Squall(s) within sight during past hour or at time of observation
.IP " 19" 6
Funnel cloud(s) within sight of station at time of observation
.IP " 20" 6
Drizzle (NOT freezing) or snow grains (NOT falling as showers) during
past hour, but NOT at time of observation
.IP " 21" 6
Rain (NOT freezing and not falling as showers) during past hour, but
NOT at time of observation
.IP " 22" 6
Snow (NOT falling as showers) during past hour, but NOT at time of
observation
.IP " 23" 6
Rain and snow or ice pellets (NOT falling as showers) during past
hour, but NOT at time of observation
.IP " 24" 6
Freezing drizzle or freezing rain (NOT falling as showers) during
past hour, but NOT at time of observation
.IP " 25" 6
Showers of rain during past hour, but NOT at time of observation
.IP " 26" 6
Showers of snow or of rain and snow, during past hour, but NOT 
at time of observation
.IP " 27" 6
Showers of hail or of hail and rain, during past hour, but NOT 
at time of observation
.IP " 28" 6
Fog during past hour, but NOT at time of observation
.IP " 29" 6
Thunderstorm (with or without precipitation) during past hour, 
but NOT at time of observation
.IP " 30" 6
Slight or moderate dust storm or sandstorm, has decreased during past hour
.IP " 31" 6
Slight or moderate dust storm or sandstorm, no appreciable change 
during past hour
.IP " 32" 6
Slight or moderate dust storm or sandstorm has begun or increased during
past hour
.IP " 33" 6
Severe dust storm or sandstorm, has decreased during past hour
.IP " 34" 6
Severe dust storm or sandstorm, no appreciable change during past hour
.IP " 35" 6
Severe dust storm or sandstorm has begun or increased during past hour
.IP " 36" 6
Slight or moderate drifting snow, generally low (less than 6 feet)
.IP " 37" 6
Heavy drifting snow, generally low
.IP " 38" 6
Slight or moderate blowing snow, generally high (more than 6 feet)
.IP " 39" 6
Heavy blowing snow, generally high
.IP " 40" 6
For or ice fog at distance at time of observation, but NOT at station
during past hour
.IP " 41" 6
Fog or ice fog in patches
.IP " 42" 6
Fog or ice fog, sky discernible, has become thinner during past hour
.IP " 43" 6
Fog or ice fog, sky NOT discernible, has become thinner during past hour
.IP " 44" 6
Fog or ice fog, sky discernible, no appreciable change during past hour
.IP " 45" 6
Fog or ice fog, sky NOT discernible, no appreciable change during past hour
.IP " 46" 6
Fog or ice fog, sky discernible, has begun or become thicker during past hour
.IP " 47" 6
Fog or ice fog, sky NOT discernible, has begun or become thicker 
during past hour
.IP " 48" 6
Fog depositing rime, sky discernible
.IP " 49" 6
Fog depositing rime, sky NOT discernible
.IP " 50" 6
Intermittent drizzle (NOT freezing), slight at time of observation
.IP " 51" 6
Continuous drizzle (NOT freezing), slight at time of observation
.IP " 52" 6
Intermittent drizzle (NOT freezing), moderate at time of observation
.IP " 53" 6
Continuous drizzle (NOT freezing), moderate at time of observation
.IP " 54" 6
Intermittent drizzle (NOT freezing), heavy at time of observation
.IP " 55" 6
Continuous drizzle (NOT freezing), heavy at time of observation
.IP " 56" 6
Slight freezing drizzle
.IP " 57" 6
Moderate or heavy freezing drizzle
.IP " 58" 6
Drizzle and rain, slight
.IP " 59" 6
Drizzle and rain, moderate or heavy
.IP " 60" 6
Intermittent rain (NOT freezing), slight at time of observation
.IP " 61" 6
Continuous rain (NOT freezing), slight at time of observation
.IP " 62" 6
Intermittent rain (NOT freezing), moderate at time of observation
.IP " 63" 6
Continuous rain (NOT freezing), moderate at time of observation
.IP " 64" 6
Intermittent rain (NOT freezing), heavy at time of observation
.IP " 65" 6
Continuous rain (NOT freezing), heavy at time of observation
.IP " 66" 6
Slight freezing rain
.IP " 67" 6
Moderate or heavy freezing rain
.IP " 68" 6
Rain or drizzle and snow, slight
.IP " 69" 6
Rain or drizzle and snow, moderate or heavy
.IP " 70" 6
Intermittent fall of snowflakes, slight at time of observation
.IP " 71" 6
Continuous fall of snowflakes, slight at time of observation
.IP " 72" 6
Intermittent fall of snowflakes, moderate at time of observation
.IP " 73" 6
Continuous fall of snowflakes, moderate at time of observation
.IP " 74" 6
Intermittent fall of snowflakes, heavy at time of observation
.IP " 75" 6
Continuous fall of snowflakes, heavy at time of observation
.IP " 76" 6
Ice prisms (with or without fog)
.IP " 77" 6
Snow grains (with or without fog)
.IP " 78" 6
Isolated starlike snow crystals (with or without fog)
.IP " 79" 6
Ice pellets or snow pellets
.IP " 80" 6
Slight rain shower(s)
.IP " 81" 6
Moderate or heavy rain shower(s)
.IP " 82" 6
Violent rain shower(s)
.IP " 83" 6
Slight shower(s) of rain and snow mixed
.IP " 84" 6
Moderate or heavy shower(s) of rain and snow mixed
.IP " 85" 6
Slight snow shower(s)
.IP " 86" 6
Moderate or heavy snow shower(s)
.IP " 87" 6
Slight shower(s) of snow pellets, or ice pellets with or without rain,
or rain and snow mixed
.IP " 88" 6
Moderate or heavy shower(s) of snow pellets, or ice pellets, or ice
pellets with or without rain or rain and snow mixed
.IP " 89" 6
Slight shower(s) of hail, with or without rain or rain and snow mixed,
not associated with thunder
.IP " 90" 6
Moderate or heavy shower(s) of hail, with or without rain, or rain and 
snow mixed, not associated with thunder
.IP " 91" 6
Slight rain at time of observation; thunderstorm during past hour, 
but NOT at time of observation
.IP " 92" 6
Moderate or heavy rain at time of observation; thunderstorm during past hour,
but NOT at time of observation
.IP " 93" 6
Slight snow, or rain and snow mixed, or hail at time of observation;
thunderstorm during past hour, but NOT at time of observation
.IP " 94" 6
Moderate or heavy snow, or rain and snow mixed, or hail at time of
observation; thunderstorm during past hour, but NOT at time of observation
.IP " 95" 6
Slight or moderate thunderstorm without hail, but with rain and/or snow
at time of observation
.IP " 96" 6
Slight or moderate thunderstorm, with hail at time of observation
.IP " 97" 6
Heavy thunderstorm, without hail, but with rain and/or snow at time 
of observation
.IP " 98" 6
Thunderstorm combined with dust storm or sandstorm at time of observation
.IP " 99" 6
Heavy thunderstorm with hail at time of observation
.sp
.sp
.IP "For the C (cloud types) font:" 0
.IP "  0" 6
Ci - Cirrus
.IP "  1" 6
Cc - Cirrocumulus
.IP "  2" 6
Cs - Cirrostratus
.IP "  3" 6
Ac - Altocumulus
.IP "  4" 6
As - Altostratus
.IP "  5" 6
Ns - Nimbostratus
.IP "  6" 6
Sc - Stratocumulus
.IP "  7" 6
St - Stratus
.IP "  8" 6
Cu - Cumulus
.IP "  9" 6
Cb - Cumulonimbus
.sp
.sp
.IP "For the CL (low clouds) font:" 0
.IP "  0" 6
not defined
.IP "  1" 6
Cu of fair weather, little vertical development and seemingly flattened
.IP "  2" 6
Cu of considerable development, generally towering, with or without other
Cu or Sc bases all at same level
.IP "  3" 6
Cb with tops lacking clear-cut outlines, but distinctly not cirriform or
anvil-shaped; with or without Cu, Sc, St
.IP "  4" 6
Sc formed by spreading out of Cu; Cu often present also
.IP "  5" 6
Sc not formed by spreading out of Cu
.IP "  6" 6
St or StFra (stratus fractus), but no StFra of bad weather
.IP "  7" 6
StFra and/or CuFra of bad weather (scud)
.IP "  8" 6
Cu and Sc (not formed by spreading out of Cu) with bases at 
different levels
.IP "  9" 6
Cb having clearly fibrous (cirriform) top, often anvil-shaped, with or
without Cu, Sc, St, or scud
.sp
.sp
.IP "For the CM (medium clouds) font:" 0
.IP "  0" 6
not defined
.IP "  1" 6
Thin As (most of cloud layer semitransparent)
.IP "  2" 6
Thick As, greater part sufficiently dense to hide sun (or moon), or Ns
.IP "  3" 6
Thin Ac, mostly semitransparent: cloud elements not changing much and at
a single level
.IP "  4" 6
Thin Ac in patches; cloud elements continually changing and/or occurring
at more than one level
.IP "  5" 6
Thin Ac in bands or in a layer gradually spreading over sky and usually
thickening as a whole
.IP "  6" 6
Ac formed by the spreading out of Cu or Cb
.IP "  7" 6
Double-layered Ac, or a thick layer of Ac, not increasing; or Ac with
As and/or Ns
.IP "  8" 6
Ac in the form of Cu-shaped tufts or Ac with turrets
.IP "  9" 6
Ac of a chaotic sky, usually at different levels; patches of dense
Ci are usually present also
.sp
.sp
.IP "For the CH (high clouds) font:" 0
.IP "  0" 6
not defined
.IP "  1" 6
Filaments of Ci, or "mares tails," scattered and not increasing
.IP "  2" 6
Dense Ci in patches or twisted sheaves, usually not increasing,
sometimes like remains of Cb; or towers or tufts
.IP "  3" 6
Dense Ci, often anvil-shaped, derived from or associated with Cb
.IP "  4" 6
Ci, often hook-shaped, gradually spreading over the sky and usually thickening
as a whole
.IP "  5" 6
Ci and Cs, often in converging bands, or Cs alone; generally overspreading
and growing denser; the continuous layer not reaching 45 degree altitude
.IP "  6" 6
Ci and Cs, often in converging bands, or Cs alone; generally overspreading
and growing denser; the continuous layer exceeding 45 degree altitude
.IP "  7" 6
Veil of Cs covering the entire sky
.IP "  8" 6
Cs not increasing and not covering entire sky
.IP "  9" 6
Cc alone or Cc with some Ci or Cs, but the Cc being the main cirriform cloud
.sp
.sp
.IP "For the W (past weather) font:" 0
.IP "  0" 6
Clear or few clouds [no associated symbol]
.IP "  1" 6
Partly cloudy (scattered) or variable sky [no associated symbol]
.IP "  2" 6
Cloudy (broken) or overcast [no associated symbol]
.IP "  3" 6
Sandstorm or dust storm, or drifting or blowing snow
.IP "  4" 6
Fog, ice fog, thick haze or thick smoke
.IP "  5" 6
Drizzle
.IP "  6" 6
Rain
.IP "  7" 6
Snow, or rain and snow mixed, or ice pellets
.IP "  8" 6
Shower(s)
.IP "  9" 6
Thunderstorm, with or without precipitation
.sp
.sp
.IP "For the N (sky cover) font:" 0
.IP "  0" 6
No clouds
.IP "  1" 6
One-tenth or less
.IP "  2" 6
Two-tenths or three-tenths
.IP "  3" 6
Four-tenths
.IP "  4" 6
Five-tenths
.IP "  5" 6
Six-tenths
.IP "  6" 6
Seven-tenths or eight tenths
.IP "  7" 6
Nine-tenths or overcast with openings
.IP "  8" 6
Completely overcast (ten-tenths)
.IP "  9" 6
Sky obscured
.sp
.sp
.IP "For the a (pressure tendency) font:" 0
.IP "  0" 6
Rising, then falling; same as or higher than 3 hours ago
.IP "  1" 6
Rising, then steady; or rising, then rising more slowly
.IP "  2" 6
Rising steadily, or unsteadily
.IP "  3" 6
Falling or steady, then rising; or rising, then rising more rapidly
.IP "  4" 6
Steady; same as 3 hours ago
.IP "  5" 6
Falling, then rising; same as or lower than 3 hours ago
.IP "  6" 6
Falling, then steady; or falling, then falling more slowly
.IP "  7" 6
Falling steadily, or unsteadily
.IP "  8" 6
Steady or rising, then falling; or falling, then falling more rapidly
.IP "  9" 6
not defined
.SH EXAMPLES
.IP "CALL NGWSYM('N',5,.5,.5,.25,1,0)"
Plots the symbol for six-tenths cloud cover
at position (.5,.5) and height .25 in the foreground color.
.IP "CALL NGWSYM('a',6,.2,.8,.3,1,0)"
Plots the symbol for barometric pressure that is falling then steady.
.IP "CALL NGWSYM('WW',95,.5,.5,.2,1,1)"
Plots the alternate symbol for slight or moderate 
thunderstorm without hail.
.sp
Use the ncargex command to see the following relevant
example: 
fngwsym.
.SH ACCESS
To use NGWSYM or c_ngwsym, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
If an illegal font, or symbol number within a font, is requested,
then a warning is issued.
.SH SEE ALSO
Online:
plotchar(3NCARG),
ncarg_cbind(3NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
