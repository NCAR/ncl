.\"
.\"	$Id: ncargcex.m,v 1.8 1994-11-03 17:39:16 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGCEX 1NCARG "October 1994" NCAR "NCAR GRAPHICS"
.SH NAME
ncargcex \- NCAR Graphics C Examples
.SH SYNOPSIS
\fBncargcex\fP 
[\fB\-A\fR]
[\fB\-W workstation_type\fR]
[\f\B\-autograph\fR]
[\f\B\-bivar\fR]
[\f\B\-conpack\fR]
[\f\B\-ezmap\fR]
[\f\B\-gks\fR]
[\f\B\-labelbar\fR]
[\f\B\-plotchar\fR]
[\f\B\-scrolled_title\fR]
[\f\B\-softfill\fR]
[\fB\-clean\fR]
[\fB\-n\fR]
[\fB\-noX11\fR]
[\fB\-onebyone\fR]
\fBexample_name ...\fR
.SH DESCRIPTION
.I ncargcex
provides the user with access to a few complete example NCAR
Graphics C source codes.
\fIncargcex\fP copies the source code for the specified
example(s) into the current directory and then compiles, links, and
executes the example.  Depending on the type of workstation specified
on the command line, the output may either be an NCGM (NCAR Graphics
Metafile) file, one of many types of PostScript files, or a text
dump.  It is also possible for no output to be produced if you select
the "x11" workstation, in which case each frame is displayed directly
to a separate X window after it is generated.  If no workstation is
specified on the command line, then it defaults to an "NCGM", unless
the example is a special one which is discussed below.

.sp
If you select one of the workstation types that produces an output
file, then the file name will have the same name as the example and
ending with an appropriate suffix: ".ncgm", ".txt", ".ps", etc.

.sp
Certain examples were created to demonstrate a particular function,
like how to rename your metafile from within the program, how to use
the full page when going to PostScript output, how to use the X11
driver and produce a graphic file at the same time, etc.  If the
example that you ask for is one of these, a message will be printed to
this effect to alert you.

.sp
In order to run \fIncargcex\fP, you must have your NCARG_ROOT
environment variable set to the parent directory where the NCAR
Graphics libraries, binaries, and include files were installed.  If this
environment variable is not set, \fIncargcex\fP will attempt to set it
for you.  If the NCAR Graphics libraries, binaries, and include files
were not installed under one parent directory, then you will need to set
the environment variables NCARG_LIB, NCARG_BIN, and NCARG_INCLUDE
instead.  Please see "man ncargintro" for more information.

.sp
.SH OPTIONS
.sp
.IP \-W workstation_type " " ""
Specify the workstation type.  This argument can be specified as a number
or as a string, where the number has the same value you would use in a call
to GOPWK.  The following numbers are valid:
.IP "             1" 18
-  NCGM
.IP "             8" 18
-  X11 window.
.IP "            10" 18
-  text dump of graphics output.
.IP "            20" 18
-  color PostScript in portrait mode.
.IP "            21" 18
-  color Encapsulated PostScript (EPS) in portrait mode.
.IP "            22" 18
-  color Encapsulated PostScript Interchange format (EPSI) in portrait mode.
.IP "            23" 18
-  monochrome PostScript in portrait mode.
.IP "            24" 18
-  monochrome Encapsulated PostScript (EPS) in portrait mode.
.IP "            25" 18
-  monochrome Encapsulated PostScript Interchange format (EPSI) in portrait mode.
.IP "            26" 18
-  color PostScript in landscape mode.
.IP "            27" 18
-  color Encapsulated PostScript (EPS) in landscape mode.
.IP "            28" 18
-  color Encapsulated PostScript Interchange format (EPSI) in landscape mode.
.IP "            29" 18
-  monochrome PostScript in landscape mode.
.IP "            30" 18
-  monochrome Encapsulated PostScript (EPS) in landscape mode.
.IP "            31" 18
-  monochrome Encapsulated PostScript Interchange format (EPSI) in landscape mode.

.IP "" 0
If you want to specify the workstation as a string, then there are a
few ways this can be done.  For example, the following workstation types
are valid:
.IP "             ncgm" 18
-  NCGM
.IP "             x11" 18
-  X11 window
.IP "            text" 18
-  text dump of graphics output
.IP "" 0
For PostScript output, there are more attributes: the type of
PostScript you file want ("ps", "eps", or "epsi"), whether you want
color or monochrome ("color" or "mono"), and whether you want portrait
or landscape mode ("port" or "land").  The orientation and the color
preference can be omitted (as they will will default to "port" and
"color" respectively), but you must specify the PostScript file type.
Each attribute can be entered in any order, separated by periods.  The
following are examples of valid PostScript workstation types: 
.IP "ps.color" 18
- color PostScript in portrait mode.  
.IP "land.eps.mono" 18
- monochrome Encapsulated PostScript format (EPS) in
landscape mode.
.IP " epsi" 18
- color Encapsulated PostScript Interchange format (EPSI) in portrait mode.
.IP "port.mono.ps" 18
-  monochrome PostScript in portrait mode.
.sp
Any combination of these three types of attributes can be used, as long
as one of them is the PostScript file type.
.sp
.IP \-A " " ""
Generate all available examples, tests, programmer doc, fundamental and
tutorial examples.
.sp
.IP \-autograph " " ""
Generate all autograph examples.
.sp
.IP \-bivar " " ""
Generate all bivar examples.
.sp
.IP \-conpack " " ""
Generate all conpack examples.
.sp
.IP \-ezmap " " ""
Generate all ezmap examples.
.sp
.IP \-gks " " ""
Generate all gks examples.
.sp
.IP \-labelbar " " ""
Generate all labelbar examples.
.sp
.IP \-plotchar " " ""
Generate all plotchar examples.
.sp
.IP \-scrolled_title " " ""
Generate all scrolled examples.
.sp
.IP \-softfill " " ""
Generate all softfill examples.
.sp
.IP \-clean " " ""
Remove everything but the ".ncgm" file.
.sp
.IP \-n " " ""
Specifies that the example should not be compiled, linked, or run, but
just copied into your local directory.
.sp
.IP \-noX11 " " ""
Do not link in the X library when linking the selected examples and/or
tests.  A stub will be linked instead.
.sp
.IP \-onebyone " " ""
Specifies that the selected examples and/or tests should be generated one
at a time and viewed as they are generated.  This is intended for use during
testing of new releases at NCAR.
.sp
Below is a list of all the available \fIncargcex\fP examples.  They are
listed according to which utility they belong with.
.sp
.SH EXAMPLES AVAILABLE
.sp
.I "AUTOGRAPH Examples:"
.sp
c_agex07
.sp
.I "BIVAR Examples:"
.sp
c_cbex01
.sp
.I "CONPACK Examples:"
.sp
c_colcon
.sp
.I "EZMAP Examples:"
.sp
c_mpex05 c_eezmpa
.sp
.I "GKS Examples:"
.sp
c_gtxpac
.sp
.I "LABELBAR Examples:"
.sp
c_elblba
.sp
.I "PLOTCHAR Examples:"
.sp
c_epltch
.sp
.I "SCROLLED TITLE Examples:"
.sp
c_slex01
.sp
.I "SOFTFILL Examples:"
.sp
c_sfex02
.sp
.I "Miscellaneous Examples:"
.sp
c_xwndws
.sp
.fi
.SH SEE ALSO
Online:
.BR ncargcc(1NCARG),
.BR ncargex(1NCARG),
.BR ncargfile(1NCARG),
.BR ncargintro(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993, 1994, 1995 University Corporation
for Atmospheric Research
.br
All Rights Reserved
