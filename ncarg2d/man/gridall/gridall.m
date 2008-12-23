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
To use Gridall C or Fortran routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_c, preferably in that order.  
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
.br
GAGETC - UNCLEARED PRIOR ERROR
.br
GAGETC - UNRECOGNIZED PARAMETER NAME
.br
GAGETI - UNCLEARED PRIOR ERROR
.br
GAGETR - UNCLEARED PRIOR ERROR
.br
GAGETR - UNRECOGNIZED PARAMETER NAME
.br
GASETC - UNCLEARED PRIOR ERROR
.br
GASETC - UNRECOGNIZED PARAMETER NAME
.br
GASETI - UNCLEARED PRIOR ERROR
.br
GASETR - UNCLEARED PRIOR ERROR
.br
GASETR - UNRECOGNIZED PARAMETER NAME
.br
GRID - UNCLEARED PRIOR ERROR
.br
GRIDAL - ERROR EXIT FROM GQCLIP
.br
GRIDAL - ERROR EXIT FROM GQLWSC
.br
GRIDAL - ERROR EXIT FROM GQPLCI
.br
GRIDAL - ERROR EXIT FROM GQTXCI
.br
GRIDAL - UNCLEARED PRIOR ERROR
.br
GRIDL - UNCLEARED PRIOR ERROR
.br
HALFAX - UNCLEARED PRIOR ERROR
.br
LABMOD - UNCLEARED PRIOR ERROR
.br
PERIM - UNCLEARED PRIOR ERROR
.br
PERIML - UNCLEARED PRIOR ERROR
.br
TICK4 - UNCLEARED PRIOR ERROR
.br
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
