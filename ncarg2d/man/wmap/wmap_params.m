.TH Wmap_params 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Wmap_params - This document briefly describes all the
internal parameters of Wmap.
.SH DESCRIPTION 
The following shows all of the internal parameters
that affect the behavior of Wmap. Each entry includes the
name of a parameter, its Fortran type, its default value, and
a short description of the parameter.
.IP "\'ALO\'   -   Integer   -    0"
A Flag to indicate whether a weather front is at the surface 
or aloft. ALO=0 is surface; ALO=1 is aloft.
.IP "\'AOC\'   -   Integer   -   -1"
Color index for the outlines of arrows (outlines drawn 
only if AOC is non-negative).
.IP "\'ARC\'   -   Real   -   0."
Length of current weather front line (retrieval only).
.IP "\'ARD\'   -   Real   -   0."
Direction of arrows, expressed in degrees.
.IP "\'ARL\'   -   Real   -   1."
Scale factor for the length of an arrow's tail, independent 
of the arrow's size.
.IP "\'ARS\'   -   Real   -   0.035"
Size of arrows, expressed as a fraction of the maximum 
screen height.
.IP "\'ASC\'   -   Integer   -   -1"
Color index for the shadows of arrows (shadows are 
drawn only if ASC is non-negative).
.IP "\'AWC\'   -   Integer   -   1"
Color index for the interior of arrows.
.IP "\'BEG\'   -   Real   -   0.015"
Space, expressed in world coordinates, to leave along a 
front line before the first symbol is drawn.
.IP "\'BET\'   -   Real   -   0.045"
Space, expressed in world coordinates, to leave along a 
front line before the first symbol is drawn.
.IP "\'CBC\'   -   Integer   -   1"
The color index to be used for backgrounds of city and 
daily high/low labels.
.IP "\'CC1\'   -   Integer   -   2"
Color index for the interior of a cloud symbol.
.IP "\'CC2\'   -   Integer   -   1"
Color index for the outline of the cloud symbol.
.IP "\'CC3\'   -   Integer   -   1"
Color index for the shadow of the cloud symbol.
.IP "\'CFC\'   -   Integer   -   1"
Color index to use for cold front symbols.
.IP "\'CHT\'   -   Real   -   0.0105"
Height of characters, expressed as a fraction of the maximum 
screen width, of the city labels and daily high/low 
temperatures.
.IP "\'CMG\'   -   Real   -   0.002"
Size, expressed as a fraction of the maximum screen 
height, of the margins used for the city labels.
.IP "\'COL\'   -   Integer   -   1"
Color index to use for all objects that require only a single color setting.
.IP "\'CS1\'   -   Real   -   N/A"
Slope of the left edge of a front line as calculated internally as 
measured in degrees from the X-axis (used when 
SLF=0,2, or 3). This parameter is for retrieval only.
.IP "\'CS2\'   -   Real   -   N/A"
Slope of the right edge of a front line as calculated internally 
as measured in degrees from the X-axis (used when 
SLF=0,1, or 3). This parameter is for retrieval only.
.IP "\'DBC\'   -   Integer   -   0"
The color index to be used for the background shadow 
for the dots marking the city locations.
.IP "\'DTC\'   -   Integer   -   1"
The color index to use for the dots marking the city locations.
.IP "\'DTS\'   -   Real   -   0.006"
Size, expressed as a fraction of the maximum screen 
height, of the dots used to mark cities.
.IP "\'DWD\'   -   Real   -   2.0"
Line widths for front lines that do not have symbols 
along them (like tropical fronts and convergence lines).
.IP "\'END\'   -   Real   -   0.015"
Space, expressed in world coordinates, to leave along a 
front line after the last symbol has been drawn.
.IP "\'FRO\'   -   Character   -   'WARM'"
 Front type (one of `WARM', `COLD', `OCCLUDED', 
`STATIONARY', `SQUALL', `TROPICAL', or `CONVERGENCE').
.IP "\'HIB\'   -   Integer   -   0"
Background color index for the "H" drawn for the high 
pressure symbols.
.IP "\'HIC\'   -   Integer   -   1"
Color index of the circumscribed circle for the "H" 
drawn for the high pressure symbols.
.IP "\'HIF\'   -   Integer   -   1"
Color index for the "H" drawn for the high pressure 
symbols.
.IP "\'HIS\'   -   Integer   -   1"
Color index for the shadow of the high pressure symbols
.IP "\'LC1\'   -   Integer   -   2"
Color index for the interior of the lightening bolt symbol
.IP "\'LC2\'   -   Integer   -   1"
Color index for the outline of the lightening bolt symbol.
.IP "\'LC3\'   -   Integer   -   1"
Color index for the shadow of the lightening bolt symbol
.IP "\'LIN\'   -   Real   -   8."
Line widths, expressed as a fraction of the maximum 
screen width, for fronts having symbols along them.
.IP "\'LOB\'   -   Integer   -   1"
Color index for the background of the "L" drawn for the 
low pressure symbols.
.IP "\'LOF\'   -   Integer   -   0"
Color index for the "L" drawn for the low pressure symbols.
.IP "\'LOS\'   -   Integer   -   0"
Color index for the shadow of the low pressure symbols.
.IP "\'LWD\'   -   Real   -   0.00275"
Line width used when parameter WTY=1 (see below)
.IP "\'MXS\'   -   Integer   -   300"
Maximum number of symbols allowed along a weather 
front line.
.IP "\'NBZ\'   -   Integer   -   51"
The number of points to use in the Bezier curves for the 
symbols along the warm fronts.
.IP "\'NMS\'   -   Integer   -   internally calculated"
Specifies precisely the number of symbols to appear 
along a weather front line (if this parameter has not been 
set by the user, then it is calculated internally).
.IP "\'PAI\'   -   Integer   -   1"
Current parameter array index used in specifying internal 
parameters that are arrays.
.IP "\'RBS\'   -   Integer   -   -1"
The color index to use for the background of the regional 
temperature labels (plotted only if RBS is non-negative).
.IP "\'RC1\'   -   Integer   -   1"
Color index for the outline of the boxes drawn for the 
regional weather labels.
.IP "\'RC2\'   -   Integer   -   0"
Color index for the backgrounds of the boxes used for 
the regional weather labels.
.IP "\'RC3\'   -   Integer   -   1"
Color index for the shadow color of the boxes used for 
the regional weather labels.
.IP "\'RC4\'   -   Integer   -   1"
Color index for the text string used for regional weather 
labels.
.IP "\'RC5\'   -   Integer   -   1"
Color for the outlines of the characters in the text strings 
used for regional weather labels (plotted only if RC5 is 
non-negative).
.IP "\'REV\'   -   Integer   -   N/A"
Reverses the current setting for the direction symbols 
will be drawn along front lines.
.IP "\'RFC\'   -   Integer   -   1"
The color index to be used for the foreground of regional 
temperature labels and cities.
.IP "\'RHT\'   -   Real   -   0.008"
Height, expressed as a fraction of the maximum screen 
width, of the characters used for the regional weather 
patterns (like rain, snow, etc.).
.IP "\'RLS\'   -   Integer   -   1"
The color index to use for shadows of regional temperature labels.
.IP "\'RMG\'   -   Real   -   0.001"
Size, expressed as a fraction of the maximum screen 
height, of the margins of the regional temperature labels.
.IP "\'ROS\'   -   Integer   -   -1"
The color index to use for the outlines of the regional 
temperature labels (plotted only if ROS is non-negative).
.IP "\'SC1\'   -   Integer   -   2"
Color index to be used for the center of the sun symbol.
.IP "\'SC2\'   -   Integer   -   3"
Color index for the points of the sun symbol.
.IP "\'SC3\'   -   Integer   -   1"
Color index for the outline of the sun symbol.
.IP "\'SC4\'   -   Integer   -   1"
Color index for the shadow of the sun symbol.
.IP "\'SHT\'   -   Real   -   0.02"
Height of symbols, expressed as a fraction of the maximum 
screen width, for all the special symbols.
.IP "\'SL1\'   -   Real   -   0.0"
The slope of the beginning of a front line measured in 
degrees from the X-axis. This parameter is used in conjunction
with the parameter SLF.
.IP "\'SL2\'   -   Real   -   0.0"
The slope of the end of a front line measured in degrees 
from the X-axis. This parameter is used in conjunction 
with the parameter SLF.
.IP "\'SLF\'   -   Integer   -   3"
Flag for indicating how the slopes at the end of a front 
line should be handled (0=use SL1 & SL2; 1=use SL1 
only; 2=use SL2 only; 3=use neither SL1 or SL2). When 
either SL1 or SL2 is not used, it is calculated internally.
.IP "\'STY\'   -   Integer Array   -   all 2s"
An array for precisely specifying whether a warm front 
or cold front symbol is to be drawn at the specified position along 
a front line (1=cold; 2=warm). Use the internal parameter 
PAI for defining this array.
.IP "\'SWI\'   -   Real   -   0.0325"
Width of a symbol along a weather front, expressed as a 
fraction of the maximum screen width.
.IP "\'T1C\'   -   Integer   -   1"
One color to use for the alternating colors of the dashes 
in the tropical fronts.
.IP "\'T2C\'   -   Integer   -   1"
A second color to use for the alternating colors of the 
dashes in the tropical fronts.
.IP "\'THT\'   -   Real   -   0.0165"
Height of characters, expressed as a fraction of the maximum 
screen width, for the regional temperature labels.
.IP "\'UNT\'   -   Integer   -   0"
Flags whether imperial units (the default) or metric units
are used.
.IP "\'VVC\'   -   Integer   -   0"
Flags whether the raw SYNOP codes are plotted for surface visibility (default is not to).
.IP "\'WBA\'   -   Real   -   62."
Angle (in degrees) that the wind barb ticks make with the 
wind barb shafts.
.IP "\'WBC\'   -   Real   -   0.3"
Diameter of sky cover circle at base of wind barb, 
expressed as a fraction of the shaft length.
.IP "\'WBD\'   -   Real   -   0.1"
Spacing between tick marks along a wind barb expressed 
as a fraction of the wind barb shaft length.
.IP "\'WBF\'   -   Integer   -   0"
Flag indicating whether the base of a wind barb should 
be drawn to allow for the sky cover circle at its base 
(WBF=1 means yes; WBF=0 means no).
.IP "\'WBL\'   -   Real   -   0.17"
Size of the text labels in the station model display, 
expressed as a fraction of the shaft length.
.IP "\'WBR\'   -   Real   -   0.25"
Radius of the larger circle drawn for calm, as a fraction 
of the wind barb shaft length.
.IP "\'WBS\'   -   Real   -   0.035"
The size, expressed as a fraction of the maximum screen 
height, of a wind barb shaft.
.IP "\'WBT\'   -   Real   -   0.33"
Length of wind barb full tic as a fraction of its shaft 
length.
.IP "\'WFC\'   -   Integer   -   1"
Color index to use for warm front symbols.
.IP "\'WHT\'   -   Real   -   0.014"
Height of characters, expressed as a fraction of the maximum 
screen width, of the characters used for regional 
weather labels (plotted with WMLABW or c_wmlabw).
.IP "\'WTY\'   -   Integer   -   0"
Flag indicating whether linewidths are to be implemented via 
GKS (WTY=0), or simulated internally (WTY=1).
.SH SEE ALSO
Online:
wmap_params,
wmbarb, 
wmdflt, 
wmdrft, 
wmdrrg, 
wmgetc, 
wmgeti, 
wmgetr, 
wmlabc, 
wmlabs,
wmlabt, 
wmlabw,
wmlgnd, 
wmsetc, 
wmseti, 
wmsetr, 
wmstnm,
ncarg_cbind.
.sp
Hardcopy:
WMAP - A Package for Producing Daily Weather Maps and Plotting Station
Model Data
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
