.\"
.\"	$Id: graphcap.m,v 1.22 2008-12-23 00:03:52 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH GRAPHCAP 5NCARG "March 1994" NCAR "NCAR GRAPHICS"
.SH NAME
graphcap \- A list of supported NCAR Graphics device definition files for use with the CGM interpreters ctrans and ictrans.
.SH DESCRIPTION
.TP 15
.B a60
Abekas a60 raster image format.
.TP 15
.B adm5
ADM5 with the DEC RG1500 graphics board.
.TP 15
.B aed.a
AED512 in ASCII mode.
.TP 15
.B aed.b
AED512 in binary mode.
.TP 15
.B avs
Application Visualization System raster image format.
.TP 15
.B balsml
HI DMP-29 in small chart mode.
.TP 15
.B ditroff
Device-independent troff.
.TP 15
.B form
A blank form for user-defined graphcap definitions.
.TP 15
.B CTXT
Produces a human-readable ASCII dump of the input.
.TP 15
.B hdf
NCSA's HDF (Hierarchical Data Format) formatted raster file.
.TP 15
.B hp2648a
HP2648A graphics terminal.
.TP 15
.B hp150
HP150 personal computer.
.TP 15
.B hp7475a
HP7475a six pen plotter.
.TP 15
.B hp7510a
HP7510a color film recorder.
.TP 15
.B hpgl
Basic support for the HP-GL language in its most elementary form.  This
graphcap is provided for older printers/plotters that do not support
the full HP-GL/2 language as standardized by HP.
.TP 15
.B hpgl2
Support for the HP-GL/2 language as standardized by HP.  Use this graphcap
for devices that support HP-GL/2 but do not support either the dual context
extensions (device accepts languages in addition to HP-GL/2) or the
palette extensions (allows for user-defined colors).
.TP 15
.B hpgl2.dual
Support for the HP-GL/2 language as standardized by HP running with the
dual context extensions (device accepts languages in addition to HP-GL/2).
If the device also supports the palette extensions, use the hpgl2pe.dual
graphcap.
.TP 15
.B hpgl2pe
Support for the HP-GL/2 language as standardized by HP running with the
palette extensions (allows for user-defined colors).  If the device also
supports the dual context extensions (device accepts languages in addition 
to HP-GL/2), use the hpgl2pe.dual graphcap.
.TP 15
.B hpgl2pe.dual
Support for the HP-GL/2 language as standardized by HP running with the
palette extensions (allows for user-defined colors) and the dual context
extensions (device accepts languages in addition to HP-GL/2).
.TP 15
.B hppcl
Produces files for the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.); hppcl
is meant to replace the now obsolete hpljxxx graphcaps.  The ctrans
"-dpi" and "-landscape" options can be used for various resolutions
and picture positioning.
.TP 15
.B imagen
Graphcap for the IMAGEN 8/300 laser printer in graphics landscape mode.
.TP 15
.B imagen.port
Graphcap for the IMAGEN 8/300 laser printer in graphics portrait mode.
.TP 15
.B nrif
NCAR Raster Interchange Format formatted raster file.
.TP 15
.B pc.mono
This graphcap is used to drive PCPLOT on IBM and compatible PC's.
.TP 15
.B ps.color
Color PostScript graphcap (portrait mode).
.TP 15
.B ps.land.color
Color PostScript graphcap (landscape mode).
.TP 15
.B ps.land.mono
Black and white PostScript (landscape mode).
.TP 15
.B ps.mono
Black and white PostScript (portrait mode).
.TP 15
.B qms800
QMS800 laser printer.
.TP 15
.B r6211
RAMTEK 6211 in TEKTRONIX compatible mode.
.TP 15
.B s100
SELINAR HiREZ100 graphics terminal.
.TP 15
.B sgi
Silicon Graphics raster image format.
.TP 15
.B sun
Sun formatted raster file.
.TP 15
.B t4010
Tektronix 4010 and 4012.
.TP 15
.B t4025
TEKTRONIX 4025 graphics terminal.
.TP 15
.B t4105
TEKTRONIX 4105 color graphics terminal.
.TP 15
.B t4107
TEKTRONIX 4107 color graphics terminal.
.TP 15
.B t4107.seg
TEKTRONIX 4107 color graphics terminal.
.TP 15
.B t4115
TEKTRONIX 4115 color graphics terminal.
.TP 15
.B t4115.seg
TEKTRONIX 4115 color graphics terminal.
.TP 15
.B tal1590
Talaris 1590 plotter.
.TP 15
.B tekalike
TEKALIKE program on the APPLE MACINTOSH.
.TP 15
.B versaterm
VERSATERM program on the APPLE MACINTOSH.
.TP 15
.B vt100
VT100 with Digital Engeering VT640 retrofit.
.TP 15
.B vt125
VT125 graphics terminal.
.TP 15
.B vt220
VT220 with Selinar SG220 graphics retrofit board.
.TP 15
.B vt330
VT330 graphics terminal.
.TP 15
.B vt340
VT340 graphics terminal (returns with white foreground and black background).
.TP 15
.B vt340w
VT340 graphics terminal (returns with black foreground and white background).
.TP 15
.B X11
X Window System interface.
.TP 15
.B xwd
X11 xwd formatted raster file.
.SH SEE ALSO
Online:
ctrans(1NCARG), ras_formats(1NCARG), graphc(5NCARG), ncarg_env(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
CGM, Graphcap, and Fontcap Supplement
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
