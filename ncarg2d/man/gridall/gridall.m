.TH Gridall 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Gridall - 
Allows one to draw backgrounds for
X/Y plots. Included are routines for drawing grids,
perimeters, and pairs of axes. Internal parameters of
Gridall control such things as the color indices and line
width scale factors used to draw various parts of the
background, the formats to be used for labels, the sizes of
the characters to be used for labels, the amount by which
labels are to be offset from the axes, and the routine to
be called to draw the labels (the SPPS routine WTSTR or the
Plotchar routine PLCHHQ).
.SH SYNOPSIS
Each of the routines GRID, GRIDL, HALFAX, PERIM, PERIML,
and GRIDAL draws a background of some sort within the part
of the plotter frame occupied by the current GKS viewport,
as follows:
.IP \(bu
GRID draws an unlabeled grid.
.IP \(bu
GRIDL draws a labeled grid.
.IP \(bu
HALFAX draws a pair of intersecting axes.
.IP \(bu
PERIM draws an unlabeled perimeter.
.IP \(bu
PERIML draws a labeled perimeter.
.IP \(bu
GRIDAL draws any of the above.
.SH ""
The positioning of ticks and grid lines is determined
partly by the arguments of these routines and partly by the
nature of the current mapping from the "user" coordinate
system to the "fractional" coordinate system, as determined
by the current definitions of the GKS viewport and window
and by the values of two internal parameters of the package
SPPS: \'LS\' and \'MI\'. Numeric labels reflect values in the
"user" coordinate system.
.sp
Note: Calling the SPPS routine SET defines the GKS viewport
and window and the values of the parameters \'LS\' and \'MI\'.
The parameter \'LS\' determines whether the mappings of user
coordinates to fractional coordinates are linear or
logarithmic and the parameter \'MI\' determines whether the
mappings are mirror-imaged or not. 
.sp
Each of the internal parameters of Gridall has a three-character
mnemonic name of the form \'xxx\'. Each of the
routines GAGETC, GAGETI, GAGETR, GASETC, GASETI, and GASETR
is intended either to retrieve the value of a specific
internal parameter of Gridall or to give a new value to it,
as follows:
.IP \(bu
GAGETC is called with the name of an internal parameter of
type CHARACTER and a character variable in which the value
of the parameter is to be returned.
.IP \(bu
GAGETI is called with the name of an internal parameter of
type INTEGER or REAL and an integer variable in which the
integer equivalent of the value of the parameter is to be
returned.
.IP \(bu
GAGETR is called with the name of an internal parameter of
type INTEGER or REAL and a real variable in which the real
equivalent of the value of the parameter is to be returned.
.IP \(bu
GASETC is called with the name of an internal parameter of
type CHARACTER and a character expression that is to become
the new value of the parameter.
.IP \(bu
GASETI is called with the name of an internal parameter of
type INTEGER or REAL and an integer expression that is to
become the new value of the parameter.
.IP \(bu
GASETR is called with the name of an internal parameter of
type INTEGER or REAL and a real expression that is to
become the new value of the parameter.
.SH ""
These six routines allow one to access all of the internal
parameters of Gridall in a manner consistent with other NCAR
Graphics utilities and are intended to replace some older
routines, each of which allows access to a limited subset
of the internal parameters. However, the older routines are
still available and will remain so for the foreseeable
future:
.IP \(bu
GACOLR sets the value of internal parameters that control
the color of various parts of the background.
.IP \(bu
LABMOD sets the value of internal parameters that affect
the appearance of labels.
.IP \(bu
TICKS and TICK4 set the value of internal parameters that
determine the length and direction of ticks.
.SH C-BINDING SYNOPSIS
c_grid
.br
c_gridl
.br
c_halfax
.br
c_perim
.br
c_periml
.br
c_gridal
.br
c_gagetc
.br
c_gageti
.br
c_gagetr
.br
c_gasetc
.br
c_gaseti
.br
c_gasetr
.br
c_gacolr
.br
c_labmod
.br
c_ticks
.br
c_tick4
.SH USER-MODIFIABLE INTERNAL ROUTINES
None.
.SH ACCESS 
To use Gridall, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use the C bindings, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
Various error conditions can occur in Gridall.  Each of these results in
a call to the error-handling routine SETER, with a final argument indicating
that the error is recoverable; by default, an error message is printed and
execution is terminated, but, if you turn on error recovery
(as described in the "man" page for "error_handling"), you
can get control back.
.sp
The error messages are as follows; all should be
more or less self-explanatory.
.sp
.in +5
GACOLR - UNCLEARED PRIOR ERROR
.sp
GAGETC - UNCLEARED PRIOR ERROR
.sp
GAGETC - UNRECOGNIZED PARAMETER NAME
.sp
GAGETI - UNCLEARED PRIOR ERROR
.sp
GAGETR - UNCLEARED PRIOR ERROR
.sp
GAGETR - UNRECOGNIZED PARAMETER NAME
.sp
GASETC - UNCLEARED PRIOR ERROR
.sp
GASETC - UNRECOGNIZED PARAMETER NAME
.sp
GASETI - UNCLEARED PRIOR ERROR
.sp
GASETR - UNCLEARED PRIOR ERROR
.sp
GASETR - UNRECOGNIZED PARAMETER NAME
.sp
GRID - UNCLEARED PRIOR ERROR
.sp
GRIDAL - ERROR EXIT FROM GQCLIP
.sp
GRIDAL - ERROR EXIT FROM GQLWSC
.sp
GRIDAL - ERROR EXIT FROM GQPLCI
.sp
GRIDAL - ERROR EXIT FROM GQTXCI
.sp
GRIDAL - UNCLEARED PRIOR ERROR
.sp
GRIDL - UNCLEARED PRIOR ERROR
.sp
HALFAX - UNCLEARED PRIOR ERROR
.sp
LABMOD - UNCLEARED PRIOR ERROR
.sp
PERIM - UNCLEARED PRIOR ERROR
.sp
PERIML - UNCLEARED PRIOR ERROR
.sp
TICK4 - UNCLEARED PRIOR ERROR
.sp
TICKS - UNCLEARED PRIOR ERROR
.in -5
.SH SEE ALSO
Online:
gridall_params,
gacolr,
gagetc,
gageti,
gagetr,
gasetc,
gaseti,
gasetr,
grid,
gridal,
gridl,
halfax,
labmod,
perim,
periml,
tick4,
ticks,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
