.TH NCARGINTRO 5NCARG "June 1995" NCAR "NCAR GRAPHICS"
.SH NAME
ncargintro \- NCAR Graphics Overview
.SH SYNOPSIS
\fBncargintro\fP 
.SH DESCRIPTION
NCAR Graphics is a collection of graphics libraries that supports the
display of scientific data. Several interfaces are available for
visualizing your data:
.sp
1. The low-level utilities (LLUs) are the traditional C and Fortran
interfaces for contouring, mapping, drawing field flows, drawing
surfaces, drawing histograms, drawing X/Y plots, labeling, and more.
.sp
2. The high-level utilities (HLUs) are a high level programmatic
interface with both C and Fortran bindings. The range of functionality
of the HLUs is exercised by setting option switches, called
resources. Resources can be set in a text file, so a plot can be
redrawn without a recompile.
.sp
3. The NCAR Command Language (NCL) is a full programming language
including looping and conditionals for data selection, manipulation,
and display. NCL commands can be executed one at a time from the
command line, or can be gathered into a series of commands, called a
script, and executed in a batch mode.
.sp
4. The prototype GUI interface is an X11-based "point and click"
interface. The prototype qualifier means that this version of the GUI
will not be supported, but is there if you can use it.
.sp
Output from these interfaces can be directed in a combination of three
ways: an NCGM (NCAR Graphics Computer Graphics Metafile), an X11
window, or one of many PostScript outputs (regular, Encapsulated, or
Interchange PostScript format).  Other formats are available through
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
setenv NCARG_ROOT /usr/local
.sp
If the binaries, libraries, and include files were NOT installed under
one root directory, say for example /usr/local/ncarg/bin, /usr/local/ncarg/lib,
and /usr/local/include, then you'll need to do the following:
.sp
setenv NCARG_BIN /usr/local/ncarg/bin
.sp
setenv NCARG_LIB /usr/local/ncarg/lib
.sp
setenv NCARG_INCLUDE /usr/local/include
.sp
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
.BR nhlf77(1NCARG),
.BR nhlcc(1NCARG),
.BR ncargf77(1NCARG),
.BR ncargcc(1NCARG),
.BR ncargpath(1NCARG),
.BR ncargrun(1NCARG)
.sp
Fortran utilities in NCAR Graphics:
.sp
.BR areas(3NCARG),
.BR autograph(3NCARG),
.BR bivar(3NCARG),
.BR colconv(3NCARG),
.BR conpack(3NCARG),
.BR dashline(3NCARG),
.BR dashpack(3NCARG),
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
.BR vectors(3NCARG),
.BR wmap(3NCARG)
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
LLU NCAR Graphics C-binding:
.sp
.BR ncarg_cbind(3NCARG)
.BR ncarg_gks_cbind(3NCARG)
.sp
Descriptions of environment variables:
.sp
.BR ncarg_env(5NCARG),
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
.BR cgm2ncgm(1NCARG)
.BR ncgm2cgm(1NCARG),
.BR pre2ncgm(1NCARG),
.BR psblack(1NCARG),
.BR pswhite(1NCARG),
.sp
A command which tells you what version of NCAR Graphics
you are using:
.sp
.BR ncargversion(1NCARG)
.sp
Utilities for generating NCAR Graphics Fortran, C, and NCL examples:
.sp
.BR ng4ex(1NCARG)
.BR ncargex(1NCARG),
.sp
Utilities for giving you access to special NCAR Graphics files and/or tables.
.sp
.BR ncargfile(1NCARG)
.sp
Programs for generating sample NCGMs:
.BR ncargworld, tgks0a
.sp
Online documentation:
.sp
NCAR Graphics User Guide,
NCAR Graphics Reference Manual,
NCAR Graphics Quick Start Guide,
NCAR Graphics Contouring and Mapping Tutorial,
NCAR Graphics Fundamentals,
User's Guide for NCAR GKS-0A Graphics,
NCAR Graphics Release Notes
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993, 1994, 1995 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
