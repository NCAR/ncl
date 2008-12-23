.\"
.\"	$Id: ras_palette.m,v 1.18 2008-12-23 00:04:16 haley Exp $
.\"
.TH RAS_PALETTE 5NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH \fBNAME\fP
ras_palette \- format descriptions for palette files for NCAR View
.SH \fBSYNOPSIS\fP
.nf
palette_file.pal
palette_file.txt
.fi
.SH \fBDESCRIPTION\fP
.LP
NCAR View supports a binary as well as a textual format for color
palettes.
.LP
The binary format is compatible with NCSA HDF and must have a file
extension of ".pal". The palette has a red, a green, and a blue
table, each with 256 single unsigned byte entries. A 0 represents
no intensity for a given color, and a 255 full intensity. In the file,
the palette is stored as:
.LP
.in +1.0i
.nf
\fIR0 R1 ... R255 G0 G1 ... G255 B0 B1 ... B255\fP
.fi
.in -1.0i
.LP
for a total of 768 bytes.
.LP
The textual format can be created by a program or manually with
a text editor. It provides for 256 RGB entries but also allows
for a sparse color table. In the latter case, missing values are
calculated from those provided using linear interpolation. If unspecified,
the 0 entry defaults to black and the 255 entry to white.  Each
color palette entry has the following format:
.LP
.in +1.0i
.nf
\fIindex_number red green blue\fP
.fi
.in -1.0i
.LP
where \fIindex_number\fP, \fIred\fP, \fIgreen\fP, and \fIblue\fP are
all integers in the range 0 to 255.
.SH \fBEXAMPLES\fP
.LP
All of the examples below deal with textual color palettes.
.TP
Setting Entries
.IP
Set color index 0 to black.
.in +1.0i
.nf
0 0 0 0
.fi
.in -1.0i
.IP
Set color index 255 to white
.in +1.0i
.nf
255 255 255 255
.fi
.in -1.0i
.TP
Grayscale Ramp
.IP
Let's suppose you have a file named "my.txt" and it has just two
lines in it: the black entry and the white entry from above. This
will produce a grayscale color palette, where the table starts
at black and ramps up to white; since only entries 0 and 255
are specified, the remaining indicies are interpolated.
.in +1.0i
.nf
0 0 0 0
255 255 255 255
.fi
.in -1.0i
.IP
(\fINote\fP: An empty palette file (e.g. "empty.txt") is legal and,
since the 0 index defaults to black and the 255 index to white,
produces a grayscale ramp just as in the above example.)
.TP
Temperature Color Scale
.IP
Let's suppose you want entry 0 to be black, and the remaining
indices to start at 1 with a fully-saturated blue and shift linearly to
255 with a fully-saturated red. Only three entries are required in your
file, "temp.txt".
.in +1.0i
.nf
0 0 0 0
1 0 0 255
255 255 0 0
.fi
.in -1.0i
.SH "\fBSEE ALSO\fP"
\fBrasview\fP(1NCARG),\fBrascat\fP(1NCARG),\fBrasgetpal\fP(1NCARG),
\fBrassplit\fP(1NCARG),\fBrasls\fP(1NCARG),\fBras_formats\fP(5NCARG)
.SH \fBBUGS/CAVEATS\fP
.LP
Color palettes are only useful with indexed-color imagery.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
