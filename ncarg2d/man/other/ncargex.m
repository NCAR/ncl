.\"
.\"	$Id: ncargex.m,v 1.66 2008-12-15 22:36:53 kennison Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGEX 1NCARG "June 1998" NCAR "NCAR GRAPHICS"
.SH NAME
ncargex \- NCAR Graphics Fortran and C Low-Level Utility Examples
.SH SYNOPSIS
.nf
\fBncargex\fP [\fB-A\fR] [\fB-E\fR] [\fB-F\fR] [\fB-P\fR] [\fB-T\fR]
[\fB-U\fR] [\fB-C\fR] [\fB-Fortran\fR] [\fB-class\fR] [\fB-ps\fR]
[\fB-x11\fR] [\fB-W workstation_type\fR] [\fB-areas]
[\fB-autograph\fR] [\fB-bivar\fR] [\fB-colconv\fR] [\fB-conpack\fR]
[\fB-conpackt\fR] [\fB-conran_family\fR] [\fB-conrec_family\fR]
[\fB-csagrid\fR] [\fB-cssgrid\fR] [\fB-dashline\fR] [\fB-dashpack\fR]
[\fB-dsgrid\fR] [\fB-ezmap\fR] [\fB-field_flow\fR] [\fB-fitgrid\fR]
[\fB-gflash\fR] [\fB-gks\fR] [\fB-gridall\fR] [\fB-halftone\fR]
[\fB-histogram\fR] [\fB-isosrfhr\fR] [\fB-isosurface\fR] [\fB-labelbar\fR]
[\fB-natgrid\fR] [\fB-ngmath\fR] [\fB-ngmisc\fR] [\fB-plotchar\fR]
[\fB-polypack\fR] [\fB-pwrite_family\fR] [\fB-scrolled_title\fR]
[\fB-seter\fR] [\fB-shgrid\fR] [\fB-softfill\fR] [\fB-spps\fR]
[\fB-streamlines\fR] [\fB-surface\fR] [\fB-tdpack\fR] [\fB-threed\fR]
[\fB-vaspackt\fR] [\fB-vectors\fR] [\fB-wmap\fR] [\fB-misc\fR]
[\fB-clean\fR] [\fB-n\fR] [\fB-ncarbd\fR] [\fB-noX11\fR] [\fB-onebyone\fR]
\fBexample_name ...\fR
.fi
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
Generate all Areas C/Fortran examples.
.sp
.IP \-autograph " " ""
Generate all Autograph C/Fortran examples.
.sp
.IP \-bivar " " ""
Generate all bivar C/Fortran examples.
.sp
.IP \-colconv " " ""
Generate all Colconv C/Fortran examples.
.sp
.IP \-conpack " " ""
Generate all Conpack C/Fortran examples.
.sp
.IP \-conpackt " " ""
Generate all Conpackt C/Fortran examples.
.sp
.IP \-conran_family " " ""
Generate all Conran Fortran examples.
.sp
.IP \-conrec_family " " ""
Generate all Conrec Fortran examples.
.sp
.IP \-csagrid " " ""
Generate all Ngmath Csagrid C/Fortran examples.
.sp
.IP \-cssgrid " " ""
Generate all Ngmath Cssgrid C/Fortran examples.
.sp
.IP \-dashline " " ""
Generate all Dashline C/Fortran examples.
.sp
.IP \-dashpack " " ""
Generate all Dashpack C/Fortran examples.
.sp
.IP \-dsgrid " " ""
Generate all Ngmath Dsgrid C/Fortran examples.
.sp
.IP \-ezmap " " ""
Generate all Ezmap C/Fortran examples.
.sp
.IP \-field_flow " " ""
Generate all Field_flow C/Fortran examples.  Includes streamlines and vectors 
examples.
.sp
.IP \-fitgrid " " ""
Generate all Ngmath Fitgrid C/Fortran examples.
.sp
.IP \-gflash " " ""
Generate all Gflash C/Fortran examples.
.sp
.IP \-gks " " ""
Generate all GKS C/Fortran examples.
.sp
.IP \-gridall " " ""
Generate all Gridall C/Fortran examples.
.sp
.IP \-halftone " " ""
Generate all Halftone Fortran examples.
.sp
.IP \-histogram " " ""
Generate all Histogram C/Fortran examples.
.sp
.IP \-isosrfhr " " ""
Generate all Isosrfhr Fortran examples.
.sp
.IP \-isosurface " " ""
Generate all Isosurface C/Fortran examples.
.sp
.IP \-labelbar " " ""
Generate all Labelbar C/Fortran examples.
.sp
.IP \-natgrid " " ""
Generate all Ngmath Natgrid C/Fortran examples.
.sp
.IP \-ngmath " " ""
Generate all Ngmath C/Fortran examples.
.sp
.IP \-ngmisc " " ""
Generate all Ngmisc C/Fortran examples.
.sp
.IP \-plotchar " " ""
Generate all Plotchar C/Fortran examples.
.sp
.IP \-polypack " " ""
Generate all Polypack C/Fortran examples.
.sp
.IP \-pwritx_family " " ""
Generate all Pwrite Fortran examples.
.sp
.IP \-scrolled_title " " ""
Generate all Scrolled title C/Fortran examples.
.sp
.IP \-seter " " ""
Generate all Seter C/Fortran examples.
.sp
.IP \-shgrid " " ""
Generate all Ngmath Shgrid C/Fortran examples.
.sp
.IP \-softfill " " ""
Generate all Softfill C/Fortran examples.
.sp
.IP \-spps " " ""
Generate all SPPS C/Fortran examples.
.sp
.IP \-streamlines " " ""
Generate all Streamlines C/Fortran examples.
.sp
.IP \-surface " " ""
Generate all Surface C/Fortran examples.
.sp
.IP \-tdpack " " ""
Generate all Tdpack C/Fortran examples.
.sp
.IP \-threed " " ""
Generate all Threed C/Fortran examples.
.sp
.IP \-vaspackt " " ""
Generate all Vaspackt C/Fortran examples.
.sp
.IP \-vectors " " ""
Generate all Vectors C/Fortran examples.
.sp
.IP \-wmap " " ""
Generate all Weather map C/Fortran examples.
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
.IP "\-ncarbd"
Use this option for compilers that appear to be having trouble
initializing blockdata variables. It will cause one or two small
subroutines to be linked in that helps force the loading of
blockdata initialization routines.
.sp
.IP "\-ngmathbd"
Use this option for compilers that appear to be having trouble
initializing blockdata variables. It will cause a small subroutine to
be linked in that helps force the loading of blockdata initialization
routines.
.sp
Note: this option doesn't need to be specified separately if you are
already including the \fB\-ncarbd\fR  and \fB\-ngmath\fR options.
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
agdp01 agex01 agex02 agex03 agex04 agex05 agex06 agex07
agex08 agex09 agex10 agex11 agex12 agex13 fagaxclr fagaxlbl
fagaxmax fagcuclr fagcudsh fagezmxy fagezmy fagezxy fagezy
fagilclr fagovrvw tagupw tautog c_agex07
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
cpex14 cpex15 cpex16 tconpa c_cbex01 c_colcon
.sp
.I "CONPACKT Examples:"
.sp
ctcbay ctex01 ctex02 ctfite ctgaus ctgc23 ctgeo1 ctgeo2 ctgeo3 ctiscp ctisc2
ctllg1 ctllg2 ctllg3 ctnccl ctorca ctpopg ctswth ctterr ctwng1 ctwng2 cttd01
cttd02 c_ctllg3
.sp
.I "CONRAN FAMILY Examples:"
.sp
tconan tconaq tconas
.sp
.I "CONREC FAMILY Examples:"
.sp
tcnqck tcnsmt tcnsup tconre
.sp
.I "CSAGRID Examples:"
.sp
csex01 csex02 csex03 csex04 csex05 csex06 csex07 c_csex01 c_csex02
c_csex03 c_csex04 c_csex05 c_csex06 c_csex07
.sp
.I "CSSGRID Examples:"
.sp
cssex01 cssex02 cssex03 c_cssex01 c_cssex02 c_cssex03
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
.I "DSGRID Examples:"
.sp
dsex01 dsex01d dsex02 dsex03 dsex04 dsex05 dsex06 c_dsex01 c_dsex01d
c_dsex02 c_dsex03 c_dsex04 c_dsex05 c_dsex06
.sp
.I "EZMAP Examples:"
.sp
cezmap1 cezmap2 cezmap3 cmpclr cmpdd cmpdrw cmpel cmpfil
cmpgci cmpgrd cmpgrp cmpita cmpitm cmplab cmplbl cmplot
cmpmsk cmpou cmppos cmpsat cmpsup cmptit cmptra cmpusr
eezmpa mpex01 mpex02 mpex03 mpex04 mpex05 mpex06 mpex07
mpex08 mpex09 mpex10 mpex11 mpex12 mpex13 mpex14 mpex15
mpexfi tezmap tezmpa tezmpb
c_mpex05 c_eezmpa c_eezmpb
.sp
.I "FIELD FLOW Examples:"
.sp
ffex00 ffex01 ffex02 ffex03 ffex04 ffex05 fcover fstream
stex01 stex02 stex03 vvex01 vvex02 vvex03 c_ffex03 c_vvex03
.sp
.I "FITGRID Examples:"
.sp
ftex01 ftex02 ftex03 ftex04 ftex05 ftex06 c_ftex01 c_ftex02 c_ftex03
c_ftex04 c_ftex05 c_ftex06
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
pgkex17 pgkex18 pgkex19 pgkex20 pgkex21 pgkex22 pgkex23
pgkex24 pgkex25 pgkex26 pgkex27 c_gtxpac c_pgkex21
.sp
.I "GRIDALL Example:"
.sp
ccpga tgrida c_tgrida
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
.I "NATGRID Examples:"
.sp
nnex01 nnex01d nnex02 nnex03 nnex04 nnex05 nnex06 nnex07 nnex08 nnex09
c_nnex01 c_nnex01d c_nnex02 c_nnex03 c_nnex06
.sp
.I "NGMATH Examples:"
.sp
csex01 csex02 csex03 csex04 csex05 csex06 csex07 cssex01 cssex02
cssex03 dsex01 dsex01d dsex02 dsex03 dsex04 dsex05 dsex06 ftex01
ftex02 ftex03 ftex04 ftex05 ftex06 nnex01 nnex01d nnex02 nnex03 nnex04
nnex05 nnex06 nnex07 nnex08 nnex09 shex01 shex02 shex03 c_csex01
c_csex02 c_csex03 c_csex04 c_csex05 c_csex06 c_csex07 c_cssex01
c_cssex02 c_cssex03 c_dsex01 c_dsex01d c_dsex02 c_dsex03 c_dsex04
c_dsex05 c_dsex06 c_ftex01 c_ftex02 c_ftex03 c_ftex04 c_ftex05
c_ftex06 c_nnex01 c_nnex01d c_nnex02 c_nnex03 c_nnex06 c_shex01
c_shex02 c_shex03
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
.I "SHGRID Examples:"
.sp
shex01 shex02 shex03 c_shex01 c_shex02 c_shex03
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
.I "TDPACK Examples:"
.sp
tdex01 tdex02 tdex03 tdex04 tdex05 tdex06 tdex07 tdex08 c_tdex01 c_tdex03
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
wmex01 wmex02 wmex03 wmex04 wmex05 wmex06 wmex07 wmex08
wmex09 wmex10 wmex11 wmex12 wmex13 wmex14 wmex15
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
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
