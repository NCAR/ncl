.TH NCARGINTRO 5NCARG "March 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargintro \- NCAR Graphics Overview
.SH SYNOPSIS
\fBncargintro\fP 
.SH DESCRIPTION
NCAR Graphics is a collection of graphics libraries that supports the
display of scientific data. A level 0a GKS package that generates an
NCGM (NCAR Graphics Computer Graphics Metafile) is also included along with
NCGM translators and accompanying device drivers.
.SH ENVIRONMENT VARIABLES
Every user of NCAR Graphics will either need to set the environment
variable NCARG_ROOT or the environment variables NCARG_BIN, NCARG_LIB,
and NCARG_INCLUDE depending on how NCAR Graphics was installed on your
system.
.sp
If the NCAR Graphics binaries, libraries, and include files were installed 
under one root directory, say for example /usr/local/bin, /usr/local/lib, 
and /usr/local/include respectively, then you'll need to do the following:
.sp
.in .5i
setenv NCARG_ROOT /usr/local
.in -.5i
.fi
.sp
If the binaries, libraries, and include files were NOT installed under
one root directory, say for example /usr/local/ncarg/bin, /usr/local/ncarg/lib,
and /usr/local/include, then you'll need to do the following:
.nf
.sp
.in .5i
setenv NCARG_BIN /usr/local/ncarg/bin
setenv NCARG_LIB /usr/local/ncarg/lib
setenv NCARG_INCLUDE /usr/local/include
.sp
.in -.5i
.fi
The environment variable NCARG_GKS_OUTPUT may be used to direct the metafile
output from the NCAR Graphics GKS package to a file (use just the file name)
or to pipe it to a translator (give the name of the translator, preceded by
a "|").
.sp
The environment variable NCARG_GKS_PSOUTPUT may be used to direct the
PostScript output from the NCAR Graphics GKS package to a file (stdout
is allowed).
.sp
The environment variables GRAPHCAP and FONTCAP may be used to tell the 
NCGM translators "ctrans", "ictrans" and "idt" what default 
"graphcap" and "fontcap" to use.  See the man pages for ctrans(1NCARG), 
ictrans(1NCARG), and idt(1NCARG) for more information.
.sp
The environment variable DISPLAY is not actually used by the translators; if
you are using X Windows, it determines where the translator output is to be
displayed.
.SH SEE ALSO
Online:
.sp
Commands which facilitate compiling and running with NCAR Graphics:
.sp
.BR ncargf77(1NCARG),
.BR ncargcc(1NCARG),
.BR ncargrun(1NCARG)
.sp
FORTRAN utilities in NCAR Graphics:
.sp
.BR areas(3NCARG),
.BR autograph(3NCARG),
.BR bivar(3NCARG),
.BR colconv(3NCARG),
.BR conpack(3NCARG),
.BR dashline(3NCARG),
.BR ezmap(3NCARG),
.BR gflash(3NCARG),
.BR gks(3NCARG),
.BR gridall(3NCARG),
.BR histogram(3NCARG),
.BR isosurface(3NCARG),
.BR labelbar(3NCARG),
.BR ngmisc(3NCARG),
.BR plotchar(3NCARG),
.BR softfill(3NCARG),
.BR surface(3NCARG),
.BR scrolled_title(3NCARG),
.BR streamlines(3NCARG),
.BR threed(3NCARG),
.BR vectors(3NCARG)
.sp
NCGM translators:
.sp
.BR ctrans(1NCARG),
.BR ictrans(1NCARG),
.BR idt(1NCARG),
.BR med(1NCARG)
.sp
Raster formats and applications:
.sp
.BR ras_formats(1NCARG),
.BR ras_palette(1NCARG),
.BR rascat(1NCARG),
.BR rasview(1NCARG),
.BR rasls(1NCARG),
.BR rasgetpal(1NCARG)
.sp
NCAR Graphics C-binding:
.sp
.BR ncarg_cbind(3NCARG)
.BR ncarg_gks_cbind(3NCARG)
.sp
Descriptions of fontcaps and graphcaps:
.sp
.BR fcaps(1NCARG),
.BR fontcap(5NCARG),
.BR gcaps(1NCARG),
.BR graphcap(5NCARG)
.sp
Metafile filters:
.sp
.BR pre2ncgm(1NCARG),
.BR psblack(1NCARG),
.BR pswhite(1NCARG),
.BR ncgm2cgm(1NCARG),
.BR cgm2ncgm(1NCARG)
.sp
A command which tells you what version of NCAR Graphics
you are using:
.sp
.BR ncargversion(1NCARG)
.sp
A command which tells you what version of NCAR View
you are using:
.sp
.BR ncarvversion(1NCARG)
.sp
Utilities for generating NCAR Graphics Fortran and C examples:
.sp
.BR ncargex(1NCARG),
.BR ncargcex(1NCARG)
.sp
Utilities for giving you access to special NCAR Graphics files and/or tables.
.sp
.BR ncargfile(1NCARG)
.sp
Hardcopy:
.sp
NCAR Graphics Fundamentals;
NCAR Graphics Contouring and Mapping Tutorial;
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Installer's Guide
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
