.\"
.\"	$Id: ncargcex_local.m,v 1.1 1994-10-26 14:50:25 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGCEX_LOCAL 1NCARG "October 1994" NCAR "NCAR GRAPHICS"
.SH NAME
ncargcex_local \- NCAR Graphics C Examples
.SH SYNOPSIS
\fBncargcex_local\fP 
[\fB\-A\fR]
[\fB\-E\fR]
[\fB\-F\fR]
[\fB\-P\fR]
[\fB\-T\fR]
[\fB\-U\fR]
[\fB\-class\fR]
[\fB\-ps\fR]
[\fB\-x11\fR]
[\fB\-W workstation_type\fR]
[\f\B\-areas]
[\f\B\-autograph\fR]
[\f\B\-bivar\fR]
[\f\B\-colconv\fR]
[\f\B\-conpack\fR]
[\f\B\-dashline\fR]
[\f\B\-dashpack\fR]
[\f\B\-ezmap\fR]
[\f\B\-field_flow\fR]
[\f\B\-gflash\fR]
[\f\B\-gks\fR]
[\f\B\-gridall\fR]
[\f\B\-histogram\fR]
[\f\B\-isosurface\fR]
[\f\B\-labelbar\fR]
[\f\B\-ngmisc\fR]
[\f\B\-plotchar\fR]
[\f\B\-polypack\fR]
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
.I ncargcex_local
provides the user with access to almost 300 complete example NCAR
Graphics C source codes, including the examples in the NCAR Graphics
Tutorial. \fIncargcex_local\fP copies the source code for the specified
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
In order to run \fIncargcex_local\fP, you must have your NCARG_ROOT
environment variable set to the parent directory where the NCAR
Graphics libraries, binaries, and include files were installed.  If this
environment variable is not set, \fIncargcex_local\fP will attempt to set it
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

Any combination of these three types of attributes can be used, as long
as one of them is the PostScript file type.
.sp
.IP \-A " " ""
Generate all available examples, tests, programmer doc, fundamental and
tutorial examples.
.sp
.IP \-E " " ""
Generate all available examples.
.sp
.IP \-F " " ""
Generate all available fundamental examples.
.sp
.IP \-P " " ""
Generate all available programmer doc examples.
.sp
.IP \-T " " ""
Generate all available tests.
.sp
.IP \-U " " ""
Generate all available tutorial examples.
.sp
.IP \-class " " ""
Generate all available class examples.
.sp
.IP \-ps " " ""
Generate all examples that use the PostScript driver.
.sp
.IP \-x11 " " ""
Generate all examples that use the X11 driver.
.sp
.IP \-areas " " ""
Generate all areas examples.
.sp
.IP \-autograph " " ""
Generate all autograph examples.
.sp
.IP \-bivar " " ""
Generate all bivar examples.
.sp
.IP \-colconv " " ""
Generate all colconv examples.
.sp
.IP \-conpack " " ""
Generate all conpack examples.
.sp
.IP \-dashline " " ""
Generate all dashline examples.
.sp
.IP \-dashpack " " ""
Generate all dashpack examples.
.sp
.IP \-ezmap " " ""
Generate all ezmap examples.
.sp
.IP \-field_flow " " ""
Generate all field_flow examples.  Includes streamlines and vectors examples.
.sp
.IP \-gflash " " ""
Generate all gflash examples.
.sp
.IP \-gks " " ""
Generate all gks examples.
.sp
.IP \-gridall " " ""
Generate all gridall examples.
.sp
.IP \-histogram " " ""
Generate all histogram examples.
.sp
.IP \-isosurface " " ""
Generate all isosurface examples.
.sp
.IP \-labelbar " " ""
Generate all labelbar examples.
.sp
.IP \-ngmisc " " ""
Generate all ngmisc examples.
.sp
.IP \-plotchar " " ""
Generate all plotchar examples.
.sp
.IP \-polypack " " ""
Generate all polypack examples.
.sp
.IP \-scrolled_title " " ""
Generate all scrolled examples.
.sp
.IP \-seter " " ""
Generate all seter examples.
.sp
.IP \-softfill " " ""
Generate all softfill examples.
.sp
.IP \-spps " " ""
Generate all spps examples.
.sp
.IP \-streamlines " " ""
Generate all streamlines examples.
.sp
.IP \-surface " " ""
Generate all surface examples.
.sp
.IP \-threed " " ""
Generate all threed examples.
.sp
.IP \-vectors " " ""
Generate all vectors examples.
.sp
.IP \-wmap " " ""
Generate all weather map examples.
.sp
.IP \-misc " " ""
Generate all miscellaneous examples.
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
Below is a list of all the available \fIncargcex_local\fP examples.  They are
listed according to which utility they belong with.
.sp
.SH EXAMPLES AVAILABLE
.sp
.I "AREAS Examples:"
.sp
c_arex01 c_arex02 c_cardb1 c_cardb2 c_caredg c_carfill c_carline c_carmap
c_tareas
.sp
.I "AUTOGRAPH Examples:"
.sp
c_agex01 c_agex02 c_agex03 c_agex04 c_agex05 c_agex06 c_agex07 c_agex08
c_agex09 c_agex10 c_agex11 c_agex12 c_agex13 c_fagaxclr c_fagaxlbl
c_fagaxmax c_fagcuclr c_fagcudsh c_fagezmxy c_fagezmy c_fagezxy
c_fagezy c_fagilclr c_fagovrvw c_tagupw c_tautog
.sp
.I "BIVAR Examples:"
.sp
c_cbex01 c_cidsfft
.sp
.I "COLCONV Examples:"
.sp
c_coex01 c_coex02 c_coex03 c_fcce01 c_fcce02 c_tcolcv
.sp
.I "CONPACK Examples:"
.sp
c_cbex01 c_ccpback c_ccpcff c_ccpcfx c_ccpcica c_ccpcir c_ccpcis c_ccpcit
c_ccpclc c_ccpcld c_ccpcldm c_ccpcldr c_ccpcll c_ccpclu c_ccpcnrc
c_ccpdflt c_ccpezct c_ccpfil c_ccpga c_ccphand c_ccphcf c_ccphl c_ccphlt
c_ccpila c_ccpils c_ccpilt c_ccpklb c_ccplbam c_ccplbdr c_ccpline c_ccpllb
c_ccpllc c_ccplll c_ccpllo c_ccpllp c_ccpllt c_ccpllw c_ccpmap c_ccpmovi
c_ccpmpxy c_ccpncls c_ccpnet c_ccpnof c_ccpnsd c_ccppc c_ccppc1 c_ccppc2
c_ccppc3 c_ccppc4 c_ccppkcl c_ccppole c_ccprc c_ccprect c_ccprwc c_ccprwu
c_ccpscam c_ccpset c_ccpsps1 c_ccpsps2 c_ccpspv c_ccpt2d c_ccptitle
c_ccpvp c_ccpvs c_cidsfft c_colcon c_cpex01 c_cpex02 c_cpex03 c_cpex04
c_cpex05 c_cpex06 c_cpex07 c_cpex08 c_cpex09 c_cpex10 c_cpex11 c_cpex12
c_tconpa
.sp
.I "DASHLINE Examples:"
.sp
c_fdlcurvd c_fdldashc c_fdldashd c_fdlsmth c_tdashc c_tdashl c_tdashp
c_tdashs
.sp
.I "DASHPACK Examples:"
.sp
c_tdshpk
.sp
.I "EZMAP Examples:"
.sp
c_cezmap1 c_cezmap2 c_cezmap3 c_cmpclr c_cmpdd c_cmpdrw c_cmpel c_cmpfil
c_cmpgci c_cmpgrd c_cmpgrp c_cmpita c_cmpitm c_cmplab c_cmplbl c_cmplot
c_cmpmsk c_cmpou c_cmppos c_cmpsat c_cmpsup c_cmptit c_cmptra c_cmpusr
c_eezmpa c_mpex01 c_mpex02 c_mpex03 c_mpex04 c_mpex05 c_mpex06 c_mpex07
c_mpex08 c_mpex09 c_mpex10 c_mpexfi c_tezmap c_tezmpa
.sp
.I "FIELD FLOW Examples:"
.sp
c_ffex00 c_ffex01 c_ffex02 c_ffex03 c_ffex04 c_ffex05 c_fcover c_fstream
c_stex01 c_stex02 c_stex03 c_vvex01 c_vvex02 c_vvex03
.sp
.I "GFLASH Example:"
.sp
c_tgflas
.sp
.I "GKS Examples:"
.sp
c_fcell c_fcell0 c_fgke01 c_fgke02 c_fgke03 c_fgke04 c_fgkgpl c_fgkgpm
c_fgkgtx c_fgklnclr c_fgklnwth c_fcirc c_fgpm01 c_pgkex01 c_pgkex02
c_pgkex03 c_pgkex04 c_pgkex05 c_pgkex06 c_pgkex07 c_pgkex08 c_pgkex09
c_pgkex10 c_pgkex11 c_pgkex12 c_pgkex13 c_pgkex14 c_pgkex15 c_pgkex16
c_pgkex17 c_pgkex18 c_pgkex19 c_pgkex20 c_pgkex21 c_gtxpac c_test09 c_tgesc
.sp
.I "GRIDALL Example:"
.sp
c_tgrida
.sp
.I "HISTOGRAM Examples:"
.sp
c_thstgr c_thstmv
.sp
.I "ISOSURFACE Examples:"
.sp
c_fisissrf c_fispwrzi c_tisosr c_tpwrzi
.sp
.I "LABELBAR Examples:"
.sp
c_elblba c_tlblba c_clbfil c_clbbar c_clblbr
.sp
.I "NGMISC Examples:"
.sp
c_fngngdts c_fngwsym
.sp
.I "PLOTCHAR Examples:"
.sp
c_epltch c_fpchiqu c_fpcloqu c_fpcfonts c_tpltch
.sp
.I "POLYPACK Examples:"
.sp
c_ppex01 c_tppack
.sp
.I "SCROLLED TITLE Examples:"
.sp
c_fslfont c_slex01 c_tstitl
.sp
.I "SETER Examples:"
.sp
c_tseter
.sp
.I "SOFTFILL Examples:"
.sp
c_fsfsgfa c_fsfwrld c_sfex01 c_sfex02 c_tsoftf
.sp
.I "SPPS Examples:"
.sp
c_fcoord c_fcoord1 c_fcoord2 c_fspcurve c_fspline c_fsppoint
c_fspponts c_splogy c_sprevx
.sp
.I "STREAMLINES Examples:"
.sp
c_fstream c_ffex00 c_ffex01 c_ffex03 c_ffex04 c_stex01 c_stex02
c_stex03 c_tstrml
.sp
.I "SURFACE Examples:"
.sp
c_fsrezsrf c_fsrpwrzs c_fsrsrfac c_srex01 c_tsrfac c_tpwrzs
.sp
.I "THREED Examples:"
.sp
c_fthex01 c_fthex02 c_fthex03 c_fthex04 c_fthex05 c_tthree c_tpwrzt
.sp
.I "VECTORS Examples:"
.sp
c_ffex00 c_ffex01 c_ffex02 c_ffex05 c_fcover c_tvelvc c_vvex01 c_vvex02
c_vvex03
.sp
.I "WEATHER MAP Examples:"
.sp
c_wmex01 c_wmex02 c_wmex03 c_wmex04
.sp
.I "Miscellaneous Examples:"
.sp
c_animat c_bnchmk c_example c_plcmnt c_xwndws
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
