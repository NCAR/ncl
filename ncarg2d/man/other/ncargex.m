.\"
.\"	$Id: ncargex.m,v 1.28 1995-10-13 15:01:18 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGEX 1NCARG "March 1995" NCAR "NCAR GRAPHICS"
.SH NAME
ncargex \- NCAR Graphics Fortran and C Low-Level Utility Examples
.SH SYNOPSIS
\fBncargex\fP 
[\fB\-A\fR]
[\fB\-E\fR]
[\fB\-F\fR]
[\fB\-P\fR]
[\fB\-T\fR]
[\fB\-U\fR]
[\fB\-C\fR]
[\fB\-Fortran\fR]
[\fB\-class\fR]
[\fB\-ps\fR]
[\fB\-x11\fR]
[\fB\-W workstation_type\fR]
[\f\B\-areas]
[\f\B\-autograph\fR]
[\f\B\-bivar\fR]
[\f\B\-colconv\fR]
[\f\B\-conpack\fR]
[\f\B\-conran_family\fR]
[\f\B\-conrec_family\fR]
[\f\B\-dashline\fR]
[\f\B\-dashpack\fR]
[\f\B\-ezmap\fR]
[\f\B\-field_flow\fR]
[\f\B\-gflash\fR]
[\f\B\-gks\fR]
[\f\B\-gridall\fR]
[\f\B\-halftone\fR]
[\f\B\-histogram\fR]
[\f\B\-isosrfhr\fR]
[\f\B\-isosurface\fR]
[\f\B\-labelbar\fR]
[\f\B\-ngmisc\fR]
[\f\B\-plotchar\fR]
[\f\B\-polypack\fR]
[\f\B\-pwrite_family\fR]
[\f\B\-scrolled_title\fR]
[\f\B\-seter\fR]
[\f\B\-softfill\fR]
[\f\B\-spps\fR]
[\f\B\-streamlines\fR]
[\f\B\-surface\fR]
[\f\B\-threed\fR]
[\f\B\-vectors\fR]
[\f\B\-wmap\fR]
[\f\B\-misc\fR]
[\fB\-clean\fR]
[\fB\-n\fR]
[\fB\-noX11\fR]
[\fB\-onebyone\fR]
\fBexample_name ...\fR
.SH DESCRIPTION
.I ncargex
provides the user with access to over 300 complete example NCAR
Graphics Fortran and C source codes, including the examples in the
NCAR Graphics Tutorial. \fIncargex\fP copies the source code for the
specified example(s) into the current directory and then compiles,
links, and executes the example.  Depending on the type of workstation
specified on the command line, the output may either be an NCGM (NCAR
Graphics Metafile) file, one of many types of PostScript files, or a
text dump.  It is also possible for no output to be produced if you
select the "x11" workstation, in which case each frame is displayed
directly to a separate X window after it is generated.  If no
workstation is specified on the command line, then it defaults to an
"NCGM", unless the example is a special one which is discussed below.
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
In order to run \fIncargex\fP, you must have your NCARG_ROOT
environment variable set to the parent directory where the NCAR
Graphics libraries, binaries, and include files were installed.  If this
environment variable is not set, \fIncargex\fP will attempt to set it
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
.IP "epsi" 18
- color Encapsulated PostScript Interchange format (EPSI) in portrait mode.
.IP "port.mono.ps" 18
-  monochrome PostScript in portrait mode.
.sp
Any combination of these three types of attributes can be used, as long
as one of them is the PostScript file type.
.sp
.IP \-A " " ""
Generate all available C/Fortran examples, tests, programmer doc, fundamental
and tutorial examples.
.sp
.IP \-E " " ""
Generate all available C/Fortran examples.
.sp
.IP \-F " " ""
Generate all available fundamental C/Fortran examples.
.sp
.IP \-P " " ""
Generate all available programmer doc C/Fortran examples.
.sp
.IP \-T " " ""
Generate all available tests.
.sp
.IP \-U " " ""
Generate all available tutorial C/Fortran examples.
.sp
.IP \-C " " ""
Generate all available C examples.
.sp
.IP \-Fortran " " ""
Generate all available Fortran examples.
.sp
.IP \-class " " ""
Generate all available class C/Fortran examples.
.sp
.IP \-ps " " ""
Generate all C/Fortran examples that use the PostScript driver.
.sp
.IP \-x11 " " ""
Generate all C/Fortran examples that use the X11 driver.
.sp
.IP \-areas " " ""
Generate all areas C/Fortran examples.
.sp
.IP \-autograph " " ""
Generate all autograph C/Fortran examples.
.sp
.IP \-bivar " " ""
Generate all bivar C/Fortran examples.
.sp
.IP \-colconv " " ""
Generate all colconv C/Fortran examples.
.sp
.IP \-conpack " " ""
Generate all conpack C/Fortran examples.
.sp
.IP \-conran_family " " ""
Generate all conran Fortran examples.
.sp
.IP \-conrec_family " " ""
Generate all conrec Fortran examples.
.sp
.IP \-dashline " " ""
Generate all dashline C/Fortran examples.
.sp
.IP \-dashpack " " ""
Generate all dashpack C/Fortran examples.
.sp
.IP \-ezmap " " ""
Generate all ezmap C/Fortran examples.
.sp
.IP \-field_flow " " ""
Generate all field_flow C/Fortran examples.  Includes streamlines and vectors 
examples.
.sp
.IP \-gflash " " ""
Generate all gflash C/Fortran examples.
.sp
.IP \-gks " " ""
Generate all gks C/Fortran examples.
.sp
.IP \-gridall " " ""
Generate all gridall C/Fortran examples.
.sp
.IP \-halftone " " ""
Generate all halftone Fortran examples.
.sp
.IP \-histogram " " ""
Generate all histogram C/Fortran examples.
.sp
.IP \-isosrfhr " " ""
Generate all isosrfhr Fortran examples.
.sp
.IP \-isosurface " " ""
Generate all isosurface C/Fortran examples.
.sp
.IP \-labelbar " " ""
Generate all labelbar C/Fortran examples.
.sp
.IP \-ngmisc " " ""
Generate all ngmisc C/Fortran examples.
.sp
.IP \-plotchar " " ""
Generate all plotchar C/Fortran examples.
.sp
.IP \-polypack " " ""
Generate all polypack C/Fortran examples.
.sp
.IP \-pwritx_family " " ""
Generate all pwrite Fortran examples.
.sp
.IP \-scrolled_title " " ""
Generate all scrolled C/Fortran examples.
.sp
.IP \-seter " " ""
Generate all seter C/Fortran examples.
.sp
.IP \-softfill " " ""
Generate all softfill C/Fortran examples.
.sp
.IP \-spps " " ""
Generate all spps C/Fortran examples.
.sp
.IP \-streamlines " " ""
Generate all streamlines C/Fortran examples.
.sp
.IP \-surface " " ""
Generate all surface C/Fortran examples.
.sp
.IP \-threed " " ""
Generate all threed C/Fortran examples.
.sp
.IP \-vectors " " ""
Generate all vectors C/Fortran examples.
.sp
.IP \-wmap " " ""
Generate all weather map C/Fortran examples.
.sp
.IP \-misc " " ""
Generate all miscellaneous C/Fortran examples.
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
Below is a list of all the available \fIncargex\fP examples.  They are
listed according to which utility they belong with.
.sp
.SH EXAMPLES AVAILABLE
.sp
.I "AREAS Examples:"
.sp
arex01 arex02 arex03 cardb1 cardb2 caredg carfill carline carmap tareas c_tareas
.sp
.I "AUTOGRAPH Examples:"
.sp
agex01 agex02 agex03 agex04 agex05 agex06 agex07 agex08
agex09 agex10 agex11 agex12 agex13 fagaxclr fagaxlbl
fagaxmax fagcuclr fagcudsh fagezmxy fagezmy fagezxy
fagezy fagilclr fagovrvw tagupw tautog c_agex07
.sp
.I "BIVAR Examples:"
.sp
cbex01 cidsfft c_cbex01
.sp
.I "COLCONV Examples:"
.sp
coex01 coex02 coex03 fcce01 fcce02 tcolcv c_coex02
.sp
.I "CONPACK Examples:"
.sp
cbex01 ccpback ccpcff ccpcfx ccpcica ccpcir ccpcis ccpcit
ccpclc ccpcld ccpcldm ccpcldr ccpcll ccpclu ccpcnrc
ccpdflt ccpezct ccpfil ccpga ccphand ccphcf ccphl ccphlt
ccpila ccpils ccpilt ccpklb ccplbam ccplbdr ccpline ccpllb
ccpllc ccplll ccpllo ccpllp ccpllt ccpllw ccpmap ccpmovi
ccpmpxy ccpncls ccpnet ccpnof ccpnsd ccppc ccppc1 ccppc2
ccppc3 ccppc4 ccppkcl ccppole ccprc ccprect ccprwc ccprwu
ccpscam ccpset ccpsps1 ccpsps2 ccpspv ccpt2d ccptitle
ccpvp ccpvs cidsfft colcon cpex01 cpex02 cpex03 cpex04
cpex05 cpex06 cpex07 cpex08 cpex09 cpex10 cpex11 cpex12 cpex13
tconpa c_cbex01 c_colcon
.sp
.I "CONRAN FAMILY Examples:"
.sp
tconan tconaq tconas
.sp
.I "CONREC FAMILY Examples:"
.sp
tcnqck tcnsmt tcnsup tconre
.sp
.I "DASHLINE Examples:"
.sp
fdlcurvd fdldashc fdldashd fdlsmth tdashc tdashl tdashp
tdashs c_fdldashc
.sp
.I "DASHPACK Examples:"
.sp
tdshpk c_tdshpk
.sp
.I "EZMAP Examples:"
.sp
cezmap1 cezmap2 cezmap3 cmpclr cmpdd cmpdrw cmpel cmpfil
cmpgci cmpgrd cmpgrp cmpita cmpitm cmplab cmplbl cmplot
cmpmsk cmpou cmppos cmpsat cmpsup cmptit cmptra cmpusr
eezmpa mpex01 mpex02 mpex03 mpex04 mpex05 mpex06 mpex07
mpex08 mpex09 mpex10 mpexfi tezmap tezmpa c_mpex05 c_eezmpa
.sp
.I "FIELD FLOW Examples:"
.sp
ffex00 ffex01 ffex02 ffex03 ffex04 ffex05 fcover fstream
stex01 stex02 stex03 vvex01 vvex02 vvex03 c_ffex03 c_vvex03
.sp
.I "GFLASH Example:"
.sp
tgflas c_tgflas
.sp
.I "GKS Examples:"
.sp
fcell fcell0 fgke01 fgke02 fgke03 fgke04 fgkgpl fgkgpm
fgkgtx fgklnclr fgklnwth fcirc fgpm01 pgkex01 pgkex02
pgkex03 pgkex04 pgkex05 pgkex06 pgkex07 pgkex08 pgkex09
pgkex10 pgkex11 pgkex12 pgkex13 pgkex14 pgkex15 pgkex16
pgkex17 pgkex18 pgkex19 pgkex20 pgkex21 c_gtxpac c_pgkex21
.sp
.I "GRIDALL Example:"
.sp
tgrida c_tgrida
.sp
.I "HALFTONE Example:"
.sp
thafto
.sp
.I "HISTOGRAM Examples:"
.sp
thstgr thstmv c_thstmv
.sp
.I "ISOSRFHR Example:"
.sp
tisohr
.sp
.I "ISOSURFACE Examples:"
.sp
fisissrf fispwrzi tisosr tpwrzi c_tisosr
.sp
.I "LABELBAR Examples:"
.sp
elblba tlblba clbfil clbbar clblbr c_elblba
.sp
.I "NGMISC Examples:"
.sp
fngngdts fngwsym c_fngwsym
.sp
.I "PLOTCHAR Examples:"
.sp
epltch fpchiqu fpcloqu fpcfonts tpltch c_epltch
.sp
.I "POLYPACK Examples:"
.sp
ppex01 tppack c_ppex01
.sp
.I "PWRITE FAMILY Examples:"
.sp
tpwrtx tpwry
.sp
.I "SCROLLED TITLE Examples:"
.sp
fslfont slex01 slex02 tstitl c_slex01
.sp
.I "SETER Examples:"
.sp
tseter
.sp
.I "SOFTFILL Examples:"
.sp
fsfsgfa fsfwrld sfex01 sfex02 tsoftf c_sfex02
.sp
.I "SPPS Examples:"
.sp
fcoord fcoord1 fcoord2 fspcurve fspline fsppoint
fspponts splogy sprevx c_fcoord2
.sp
.I "STREAMLINES Examples:"
.sp
fstream ffex00 ffex01 ffex03 ffex04 stex01 stex02
stex03 tstrml c_ffex03
.sp
.I "SURFACE Examples:"
.sp
fsrezsrf fsrpwrzs fsrsrfac srex01 tsrfac tpwrzs c_srex01
.sp
.I "THREED Examples:"
.sp
fthex01 fthex02 fthex03 fthex04 fthex05 tthree tpwrzt c_fthex01
.sp
.I "VECTORS Examples:"
.sp
ffex00 ffex01 ffex02 ffex05 fcover tvelvc vvex01 vvex02
vvex03 c_vvex03
.sp
.I "WEATHER MAP Examples:"
.sp
wmex01 wmex02 wmex03 wmex04 wmex05 wmex06 wmex07
wmex08 wmex09 wmex10 wmex11 wmex12 wmex13 wmex14
c_wmex09
.sp
.I "Miscellaneous Examples:"
.sp
bnchmk example ncargworld
.sp
.I "X11 Examples:"
.sp
fgke01 fgke04 c_xwndws
.sp
.fi
.SH SEE ALSO
Online:
.BR ncargf77(1NCARG),
.BR ncargcc(1NCARG),
.BR ncargfile(1NCARG),
.BR ng4ex(1NCARG),
.BR ncarg_cbind(5NCARG),
.BR ncargintro(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-1995
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
