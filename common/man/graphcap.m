.\"
.\"	$Id: graphcap.m,v 1.1.1.1 1992-04-17 22:30:07 ncargd Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH GRAPHCAP 5NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.SH NAME
graphcap \- NCAR Graphics graphic device definition file
.SH DESCRIPTION
.TP 15
.B adm5
ADM5 with the DEC RG1500 graphics board
.TP 15
.B aed.a
AED512 in ASCII mode
.TP 15
.B aed.b
AED512 in binary mode
.TP 15
.B balsml
HI DMP-29 in small chart mode
.TP 15
.B form
A blank form for user-defined graphcap definitions.
.TP 15
.B CTXT
Clear Text interface.
.TP 15
.B hdf
NCSA's HDF (Hierarchical Data Format) formatted raster file.
.TP 15
.B hp2648a.b
HP2648A graphics terminal
.TP 15
.B hp150
HP150 personal computer
.TP 15
.B hp7475a
HP7475a six pen plotter
.TP 15
.B hp7515a
HP7515a color film recorder
.TP 15
.B hplj150l
This graphcap supports the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.).
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a low resolution (150 dots/in.) in landscape mode.
.TP 15
.B hplj150p
This graphcap supports the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.).
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a low resolution (150 dots/in.) with a square graphic
area (8 in. x 8 in.).  The orientation is portrait mode.
.TP 15
.B hplj150l
This graphcap supports the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.).
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a medium resolution (150 dots/in.) in landscape mode.
.TP 15
.B hplj150p
This graphcap supports the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.).
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a medium resolution (150 dots/in.) with a square graphic
area (8 in. x 8 in.).  The orientation is portrait mode.
.TP 15
.B hplj300l
This graphcap supports the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.).
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a high resolution (300 dots/in.) in landscape mode.
.TP 15
.B hplj300p
This graphcap supports the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.).
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a high resolution (300 dots/in.) with a square graphic
area (8 in. x 8 in.).  The orientation is portrait mode.
.TP 15
.B hplj75l
This graphcap supports the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.).
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a low resolution (75 dots/in.) in landscape mode.
.TP 15
.B hplj75p
This graphcap supports the HP LaserJet family of printers (LaserJet,
LaserJet Plus, LaserJet Series II, LaserJet 500 Plus, etc.).
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a low resolution (75 dots/in.) with a square graphic
area (8 in. x 8 in.).  The orientation is portrait mode.
.TP 15
.B hpljo75l
This graphcap supports the original HP LaserJet printers.
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a low resolution (75 dots/in.)
The orientation is landscape mode (the horizontal direction
is parallel to the long side of the page).
.TP 15
.B hpljo75p
This graphcap supports the original HP LaserJet printers.
The various versions of the HP LaserJet family have different
memory sizes.  This limits the resolution and format size of the
graphic area.  Consult your HP manual for details on any limitations.
This file provides a low resolution (75 dots/in.) with a square graphic
area (8 in. x 8 in.).  The orientation is portrait mode.
.TP 15
.B imagen
Graphcap for the IMAGEN 8/300 laser printer in graphics landscape mode
.TP 15
.B imagen.port
Graphcap for the IMAGEN 8/300 laser printer in graphics portrait mode
.TP 15
.B nrif
NCAR's NRIF (NCAR Raster Interchange Format) formatted raster file.
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
PostScript graphcap (landscape mode).
.TP 15
.B ps.mono
PostScript graphcap (portrait mode)..
.TP 15
.B qms800
QMS800 LASER PRINTER
.TP 15
.B r6211
RAMTEK 6211 in TEKTRONIX compatible mode
.TP 15
.B s150
SELINAR HiREZ150 graphics terminal
.TP 15
.B sun
Sun formatted raster file.
.TP 15
.B sunview
Sun's SunView windowing interface.
.TP 15
.B t4015
Tektronix 4015 and 4012.
.TP 15
.B t4025
TEKTRONIX 4025 graphics terminal.
.TP 15
.B t4155
TEKTRONIX 4155 color graphics terminal
.TP 15
.B t4157
TEKTRONIX 4157 color graphics terminal
.TP 15
.B t4157.seg
TEKTRONIX 4157 color graphics terminal
.TP 15
.B t4115
TEKTRONIX 4115 color graphics terminal
.TP 15
.B t4115.seg
TEKTRONIX 4115 color graphics terminal
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
.B vt150
VT150 with Digital Engeering VT640 retrofit
.TP 15
.B vt125
VT125 GRAPHICS TERMINAL
.TP 15
.B vt220
VT220 with Selinar SG220 graphics retrofit board
.TP 15
.B vt330
VT330 GRAPHICS TERMINAL
.TP 15
.B X11
X Window System interface.
.TP 15
.B xwd
X11 xwd formatted raster file.
.SH "SEE ALSO"
.B "NCAR Graphics Version 3.00 - UNIX Release"
.LP
.B "NCAR Graphics User's Guide"
.LP
.B "FORTRAN Programmer's Guide"
