.\"
.\"	$Id: ncargex.m,v 1.3 1993-03-29 23:16:40 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGEX 1NCARG "March 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargex \- NCAR Graphics Fortran Examples and Tests
.SH SYNOPSIS
\fBncargex\fP 
[\fB\-all, -A\fR]
[\fB\-allexamples, -E\fR]
[\fB\-allfundamental, -F\fR]
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
provides the user with access to example source code as
well as the tutorial code for NCAR
Graphics. \fIncargex\fP
copies the source code for the specified example(s)
into the current directory and then compiles, links,
and executes the example, leaving an NCGM file
with the same name as the example, suffixed with
".ncgm". An option allows you to request that
only the source code be copied to your directory,
without compilation, linking, or execution.
Another option allows you to request that only the
NCGM file be left in your directory and that all other files
created by \fIncargex\fP be deleted.
The argument \fIname\fP may be
selected from the lists that appear below.
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
.I "EXAMPLES AND TESTS AVAILABLE"
.sp
.I "AREAS Examples:"
.nf
	arex01
.sp
.I "AUTOGRAPH Examples:"
.nf
	agex01 agex02 agex03 agex04 agex05
	agex06 agex07 agex08 agex09 agex10
	agex11 agex12 agex13
.sp
.I "BIVAR Examples:"
.nf
	cbex01
.sp
.I "CONPACK Examples:"
.nf
	cpex01 cpex02 cpex03 cpex04 cpex05
	cpex06 cpex07 cpex08 cpex09
.sp
.I "EZMAP Examples:"
.nf
	mpex01 mpex02 mpex03 mpex04 mpex05
	mpex06 mpex07 mpex08 mpex09 mpex10
	mpexfi eezmpa
.sp
.I "LABELBAR Examples:"
.nf
	elblba
.sp
.I "PLOTCHAR Examples:"
.nf
	epltch
.sp
.I "SCROLLED_TITLE Examples:"
.nf
	slex01
.sp
.I "SOFTFILL Examples:"
.nf
	sfex01 sfex02
.sp
.I "SPPS Examples:"
.nf
	splogy sprevx spset1 spset2
.sp
.I "STRMLN Examples:"
.nf
	stex01 stex02 stex03
.sp
.I "SURFACE Examples:"
.nf
	srex01
.sp
.I "VELVCT Examples:"
.nf
	vvex01 vvex02
.sp
.I "Miscellaneous Examples:"
.nf
	coex01 coex02 coex03 bnchmk
.sp
.I "Tutorial AREAS Examples:"
.nf
	cardb1 caredg carline cardb2 carfill carmap
.sp
.I "Tutorial CONPACK Examples:"
.nf
	ccpback ccpcff ccpcfx ccpcir ccpcis ccpcit ccpclc ccpcld
	ccpcldm ccpcldr ccpcll ccpclu ccpcnrc ccpdflt ccpezct ccpfil 
	ccpga ccphand ccphl ccphlt ccpila ccpils ccpilt ccpklb ccplbam 
	ccplbdr ccpline ccpllb ccpllc ccplll ccpllo ccpllp ccpllt 
	ccpllw ccpmap ccpmpxy ccpncls ccpnet ccpnof ccpnsd ccppc ccppc1 
	ccppc2 ccppc3 ccppc4 ccppkcl ccprc ccprect ccprwc ccprwu ccpscam 
	ccpset ccpsps1 ccpsps2 ccpspv ccptitle ccpvp ccpvs cidsfft colcon
.sp
.I "Tutorial EZMAP Examples:"
.nf
	cezmap1 cezmap2 cezmap3 cmpclr cmpdd cmpdrw cmpel cmpfil
	cmpgci cmpgrd cmpgrp cmpita cmpitm cmplab cmplbl cmplot 
	cmpmsk cmpou cmppos cmpsat cmpsup cmptit cmptra cmpusr
.sp
.I "Fundamental DASHLINE Examples:"
	fdlcurvd fdldashc fdldashd fdlsmth
.sp
.I "Fundamental GKS Examples:"
	fgkgpl fgkgpm fgkgtx fgklnclr fgklnwth
.sp
.I "Fundamental ISOSURFACE Examples:"
	fiseziso fisissrf fispwrzi
.sp
.I "Fundamental NGMISC Examples:"
	fngngdts fngwsym
.sp
.I "Fundamental PLOTCHAR Examples:"
	fpchiqu fpcloqu
.sp
.I "Fundamental SOFTFILL Examples:"
	fsfwrld fsfsgfa
.sp
.I "Fundamental SPPS  Examples:"
	fspcurve fspline fsppoint fspponts
.sp
.I "Fundamental SURFACE Examples:"
	fsrezsrf fsrpwrzs fsrsrfac
.sp
.I "AREAS Test Programs:"
    tareas
.sp
.I "AUTOGRAPH Test Programs:"
    tagupw tautog
.sp
.I "COLCONV Test Programs:"
    tcolcv
.sp
.I "CONPACK Test Programs:"
    tconpa
.sp
.I "CONRAN Test Programs:"
    tconan tconaq tconas
.sp
.I "CONREC Test Programs:"
    tconre tcnqck tcnsmt tcnsup
.sp
.I "CONREC Test Programs:"
    tdashc tdashl tdashp tdashs
.sp
.I "EZMAP Test Programs:"
    tezmap tezmpa
.sp
.I "GFLASH Test Programs:"
    tgflas
.sp
.I "GRIDALL Test Programs:"
    tgrida
.sp
.I "HALFTONE Test Programs:"
    thafto
.sp
.I "HISTOGRAM Test Programs:"
    thstgr
.sp
.I "ISOSRFHR Test Programs:"
    tisohr
.sp
.I "ISOSURFACE Test Programs:"
    tisosr tpwrzi
.sp
.I "LABELBAR Test Programs:"
    tlblba
.sp
.I "PLOTCHAR Test Programs:"
    tpltch
.sp
.I "PWRITX Test Programs:"
    tpwrtx
.sp
.I "PWRITY Test Programs:"
    tpwry
.sp
.I "SCROLLED_TITLE Test Programs:"
    tstitl
.sp
.I "SOFTFILL Test Programs:"
    tsoftf
.sp
.I "STREAMLINES Test Programs:"
    tstrml
.sp
.I "SURFACE Test Programs:"
    tsrfac tpwrzs
.sp
.I "THREED Test Programs:"
    tthree tpwrzt
.sp
.I "VECTORS Test Programs:"
    tvelvc
.sp
.fi
.SH SEE ALSO
Online:
ncargf77(1NCARG), ncargcex(1NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
