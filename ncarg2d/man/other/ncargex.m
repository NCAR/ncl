.\"
.\"	$Id: ncargex.m,v 1.16 1994-04-18 22:24:40 kennison Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGEX 1NCARG "March 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargex \- NCAR Graphics Fortran Examples
.SH SYNOPSIS
\fBncargex\fP 
[\fB\-all, -A\fR]
[\fB\-allexamples, -E\fR]
[\fB\-allfundamental, -F\fR]
[\fB\-allpdocexamples, -P\fR]
[\fB\-alltests, -T\fR]
[\fB\-alltutorial, -U\fR]
[\f\B\-areas]
[\f\B\-autograph\fR]
[\f\B\-bivar\fR]
[\f\B\-colconv\fR]
[\f\B\-conpack\fR]
[\f\B\-conran_family\fR]
[\f\B\-conrec_family\fR]
[\f\B\-dashline\fR]
[\f\B\-ezmap\fR]
[\f\B\-field_flow\fR]
[\f\B\-gflash\fR]
[\f\B\-gridall\fR]
[\f\B\-halftone\fR]
[\f\B\-histogram\fR]
[\f\B\-isosrfhr\fR]
[\f\B\-isosurface\fR]
[\f\B\-labelbar\fR]
[\f\B\-ngmisc\fR]
[\f\B\-plotchar\fR]
[\f\B\-pwritx\fR]
[\f\B\-pwrity\fR]
[\f\B\-scrolled_title\fR]
[\f\B\-seter\fR]
[\f\B\-softfill\fR]
[\f\B\-spps\fR]
[\f\B\-streamlines\fR]
[\f\B\-surface\fR]
[\f\B\-threed\fR]
[\f\B\-vectors\fR]
[\f\B\-gks\fR]
[\f\B\-misc\fR]
[\fB\-clean\fR]
[\fB\-n\fR]
[\fB\-noX11\fR]
[\fB\-onebyone\fR]
\fBname ...\fR
.SH DESCRIPTION
.I ncargex
provides the user with access to example source code as well as the
tutorial code for NCAR Graphics. \fIncargex\fP copies the source code
for the specified example(s) into the current directory and then
compiles, links, and executes the example, leaving an NCGM file with
the same name as the example, suffixed with ".ncgm".  Exceptions to
this are examples that don't generate NCGM files (a message will be
printed to this effect) and the "fgke03" example, which generates two
metafiles with the names "META01" and "META02".
.sp
In order to run \fIncargex\fP, you must have your NCARG_ROOT
environment variable set to the directory pathname where the NCAR
Graphics libraries, binaries, and include files were installed.  If
you are not sure what NCARG_ROOT should be set to, please check with
your system administrator or the site representative for NCAR
Graphics.  If the NCAR Graphics libraries, binaries, and include files
were not installed under one root directory, then you will need to set
the environment variables NCARG_LIB, NCARG_BIN, and NCARG_INCLUDE
instead.  Please see "man ncargintro" for more information.
.sp
There are two interactive examples that require you to be running X
and to have your DISPLAY environment variable set correctly in order
to execute them, because they pop up an X window.  These examples are
"fgke01" and "fgke04".  The "-gks" and "-F" options will not generate
these examples; you must explicitly list them on the \fIncargex\fP
command line.
.sp
The argument \fIname\fP may be selected from the lists that appear below.
.sp
.I OPTIONS
.IP "\-all, \-A"
Generate all available examples, tests, and tutorial examples.
.sp
.IP "\-allexamples, \-E"
Generate all available examples.
.sp
.IP "\-allfundamental, \-F"
Generate all available fundamental examples.
.sp
.IP "\-allpdocexamples, \-P"
Generate all available programmer doc examples.
.sp
.IP "\-alltests, \-T"
Generate all available tests.
.sp
.IP "\-alltutorial, \-U"
Generate all available tutorial examples.
.sp
.IP "\-areas"
Generate all areas examples.
.sp
.IP "\-autograph\fR"
Generate all autograph examples.
.sp
.IP "\-bivar\fR"
Generate all bivar examples.
.sp
.IP "\-colconv\fR"
Generate all colconv examples.
.sp
.IP "\-conpack\fR"
Generate all conpack examples.
.sp
.IP "\-conran_family\fR"
Generate all conran examples.
.sp
.IP "\-conrec_family\fR"
Generate all conrec examples.
.sp
.IP "\-dashline\fR"
Generate all dashline examples.
.sp
.IP "\-ezmap\fR"
Generate all ezmap examples.
.sp
.IP "\-field_flow\fR"
Generate all field_flow examples.  Includes streamlines and vectors examples.
.sp
.IP "\-gflash\fR"
Generate all gflash examples.
.sp
.IP "\-gridall\fR"
Generate all gridall examples.
.sp
.IP "\-halftone\fR"
Generate all halftone examples.
.sp
.IP "\-histogram\fR"
Generate all histogram examples.
.sp
.IP "\-isosrfhr\fR"
Generate all isosrfhr examples.
.sp
.IP "\-isosurface\fR"
Generate all isosurface examples.
.sp
.IP "\-labelbar\fR"
Generate all labelbar examples.
.sp
.IP "\-ngmisc\fR"
Generate all ngmisc examples.
.sp
.IP "\-plotchar\fR"
Generate all plotchar examples.
.sp
.IP "\-pwritx\fR"
Generate all pwritx examples.
.sp
.IP "\-pwrity\fR"
Generate all pwrity examples.
.sp
.IP "\-scrolled_title\fR"
Generate all scrolled examples.
.sp
.IP "\-seter\fR"
Generate all seter examples.
.sp
.IP "\-softfill\fR"
Generate all softfill examples.
.sp
.IP "\-spps\fR"
Generate all spps examples.
.sp
.IP "\-streamlines\fR"
Generate all streamlines examples.
.sp
.IP "\-surface\fR"
Generate all surface examples.
.sp
.IP "\-threed\fR"
Generate all threed examples.
.sp
.IP "\-vectors\fR"
Generate all vectors examples.
.sp
.IP "\-gks\fR"
Generate all gks examples.
.sp
.IP "\-misc\fR"
Generate all miscellaneous examples.
.sp
.IP \-clean " " ""
Remove everything but the ".ncgm" file.
.sp
.IP \-n " " ""
Specifies that the example should not be compiled, linked, or run.
.sp
.IP \-noX11 " " ""
Do not link in the X library when linking the selected examples and/or
tests.
.sp
.IP \-onebyone " " ""
Specifies that the selected examples and/or tests should be generated one
at a time and viewed as they are generated.  This is intended for use during
testing of new releases at NCAR.
.sp
Below is a list of all the available \fIncargex\fP examples.  They are
listed according to which utility they belong with:
.nf
.sp
.I "EXAMPLES AVAILABLE"
.sp
.I "AREAS Examples:"
	arex01 arex02 cardb1 cardb2 caredg carfill carline carmap tareas
.sp
.I "AUTOGRAPH Examples:"
	agex01 agex02 agex03 agex04 agex05 agex06 agex07 agex08
	agex09 agex10 agex11 agex12 agex13 fagaxclr fagaxlbl
	fagaxmax fagcuclr fagcudsh fagezmxy fagezmy fagezxy
	fagezy fagilclr fagovrvw tagupw tautog
.sp
.I "BIVAR Examples:"
	cbex01 cidsfft
.sp
.I "COLCONV Examples:"
	coex01 coex02 coex03 fcce01 fcce02 tcolcv
.sp
.I "CONPACK Examples:"
	cbex01 ccpback ccpcff ccpcfx ccpcica ccpcir ccpcis ccpcit
	ccpclc ccpcld ccpcldm ccpcldr ccpcll ccpclu ccpcnrc
	ccpdflt ccpezct ccpfil ccpga ccphand ccphcf ccphl ccphlt
	ccpila ccpils ccpilt ccpklb ccplbam ccplbdr ccpline ccpllb
	ccpllc ccplll ccpllo ccpllp ccpllt ccpllw ccpmap ccpmovi
	ccpmpxy ccpncls ccpnet ccpnof ccpnsd ccppc ccppc1 ccppc2
	ccppc3 ccppc4 ccppkcl ccppole ccprc ccprect ccprwc ccprwu
	ccpscam ccpset ccpsps1 ccpsps2 ccpspv ccpt2d ccptitle
	ccpvp ccpvs cidsfft colcon cpex01 cpex02 cpex03 cpex04
	cpex05 cpex06 cpex07 cpex08 cpex09 cpex10 tconpa
.sp
.I "CONRAN FAMILY Examples:"
    tconan tconaq tconas
.sp
.I "CONREC FAMILY Examples:"
    tcnqck tcnsmt tcnsup tconre
.sp
.I "DASHLINE Examples:"
	fdlcurvd fdldashc fdldashd fdlsmth tdashc tdashl tdashp
	tdashs
.sp
.I "EZMAP Examples:"
	cezmap1 cezmap2 cezmap3 cmpclr cmpdd cmpdrw cmpel cmpfil
	cmpgci cmpgrd cmpgrp cmpita cmpitm cmplab cmplbl cmplot
	cmpmsk cmpou cmppos cmpsat cmpsup cmptit cmptra cmpusr
	eezmpa mpex01 mpex02 mpex03 mpex04 mpex05 mpex06 mpex07
	mpex08 mpex09 mpex10 mpexfi tezmap tezmpa
.sp
.I "FIELD FLOW Examples:"
	ffex00 ffex01 ffex02 ffex03 ffex04 ffex05 fcover fstream
	stex01 stex02 stex03 vvex01 vvex02 vvex03
.sp
.I "GFLASH Example:"
    tgflas
.sp
.I "GKS Examples:"
	fcell fcell0 fgke01 fgke02 fgke03 fgke04 fgkgpl fgkgpm
	fgkgtx fgklnclr fgklnwth fcirc fgpm01 pgkex01 pgkex02
	pgkex03 pgkex04 pgkex05 pgkex06 pgkex07 pgkex08 pgkex09
	pgkex10 pgkex11 pgkex12 pgkex13 pgkex14 pgkex15 pgkex16
	pgkex17 pgkex18 pgkex19 pgkex20 pgkex21
.sp
.I "GRIDALL Example:"
    tgrida
.sp
.I "HALFTONE Example:"
    thafto
.sp
.I "HISTOGRAM Examples:"
    thstgr thstmv
.sp
.I "ISOSRFHR Example:"
    tisohr
.sp
.I "ISOSURFACE Examples:"
	fisissrf fispwrzi tisosr tpwrzi
.sp
.I "LABELBAR Examples:"
	elblba tlblba
.sp
.I "NGMISC Examples:"
	fngngdts fngwsym
.sp
.I "PLOTCHAR Examples:"
	epltch fpchiqu fpcloqu fpcfonts tpltch
.sp
.I "PWRITE FAMILY Examples:"
    tpwrtx tpwry
.sp
.I "SCROLLED TITLE Examples:"
	fslfont slex01 tstitl
.sp
.I "SETER Examples:"
	tseter
.sp
.I "SOFTFILL Examples:"
	fsfsgfa fsfwrld sfex01 sfex02 tsoftf
.sp
.I "SPPS Examples:"
	fcoord fcoord1 fcoord2 fspcurve fspline fsppoint
	fspponts splogy sprevx
.sp
.I "STREAMLINES Examples:"
	fstream ffex00 ffex01 ffex03 ffex04 stex01 stex02
	stex03 tstrml 
.sp
.I "SURFACE Examples:"
	fsrezsrf fsrpwrzs fsrsrfac srex01 tsrfac tpwrzs
.sp
.I "THREED Examples:"
	fthex01 fthex02 fthex03 fthex04 fthex05 tthree tpwrzt
.sp
.I "VECTORS Examples:"
	ffex00 ffex01 ffex02 ffex05 fcover tvelvc vvex01 vvex02
	vvex03
.sp
.I "Miscellaneous Examples:"
	bnchmk example
.sp
.fi
.SH SEE ALSO
Online:
.BR ncargf77(1NCARG),
.BR ncargcex(1NCARG),
.BR ncargfile(1NCARG),
.BR ncargintro(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
