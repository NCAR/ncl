.\"
.\"	$Id: ncargex.m,v 1.2 1993-01-13 21:49:18 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGEX 1NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.SH NAME
ncargex \- NCAR Graphics Examples and Tests
.SH SYNOPSIS
\fBncargex\fP 
[\fB\-all, -A\fR]
[\fB\-allexamples, -E\fR]
[\fB\-alltests, -T\fR]
[\fB\-alltutorial, -U\fR]
[\fB\-clean\fR]
[\fB\-n\fR]
[\fB\-onebyone\fR]
\fBname ...\fR
.SH DESCRIPTION
.LP
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
.LP
.I OPTIONS
.LP
.IP "\-all, \-A"
Generate all available examples, tests, and tutorial examples.
.LP
.IP "\-allexamples, \-E"
Generate all available examples.
.LP
.IP "\-alltests, \-T"
Generate all available tests.
.LP
.IP "\-alltutorial, \-U"
Generate all available tutorial examples.
.LP
.IP \-clean " " ""
Remove everything but the ".ncgm" file.
.LP
.IP \-n " " ""
Specifies that the example should not be compiled, linked, or run.
.LP
.IP \-onebyone " " ""
Specifies that the selected examples and/or tests should be generated one
at a time and viewed as they are generated.  This is intended for use during
testing of new releases at NCAR.
.LP
.I "EXAMPLES AND TESTS AVAILABLE"
.LP
.I "AUTOGRAPH Examples:"
.nf
	agex01 agex02 agex03 agex04 agex05
	agex06 agex07 agex08 agex09 agex10
	agex11 agex12 agex13
.fi
.LP
.I "EZMAP Examples:"
.nf
	mpex01 mpex02 mpex03 mpex04 mpex05
	mpex06 mpex07 mpex08 mpex09 mpex10
	mpexfi
.fi
.LP
.I "EZMAPA Examples:"
.nf
	eezmpa
.fi
.LP
.I "CONPACK Examples:"
.nf
	cpex01 cpex02 cpex03 cpex04 cpex05
	cpex06 cpex07 cpex08 cpex09
.fi
.LP
.I "LABELBAR Examples:"
.nf
	elblba
.fi
.LP
.I "SOFTFILL Examples:"
.nf
	sfex01 sfex02
.fi
.LP
.I "SURFACE Examples:"
.nf
	srex01
.fi
.LP
.I "PLOTCHAR Examples:"
.nf
	epltch
.fi
.LP
.I "Miscellaneous Examples:"
.nf
	arex01 cbex01 coex01 coex02 coex03 stex01
.fi
.LP
.I "Tutorial Areas Examples:"
.nf
	cardb1 caredg carline cardb2 carfill carmap
.fi
.LP
.I "Tutorial Conpack Examples:"
.nf
	ccpback ccpcff ccpcfx ccpcir ccpcis ccpcit ccpclc ccpcld
	ccpcldm ccpcldr ccpcll ccpclu ccpcnrc ccpdflt ccpezct ccpfil 
	ccpga ccphand ccphl ccphlt ccpila ccpils ccpilt ccpklb ccplbam 
	ccplbdr ccpline ccpllb ccpllc ccplll ccpllo ccpllp ccpllt 
	ccpllw ccpmap ccpmpxy ccpncls ccpnet ccpnof ccpnsd ccppc ccppc1 
	ccppc2 ccppc3 ccppc4 ccppkcl ccprc ccprect ccprwc ccprwu ccpscam 
	ccpset ccpsps1 ccpsps2 ccpspv ccptitle ccpvp ccpvs cidsfft colcon
.fi
.LP
.I "Tutorial Ezmap Examples:"
.nf
	cezmap1 cezmap2 cezmap3 cmpclr cmpdd cmpdrw cmpel cmpfil
	cmpgci cmpgrd cmpgrp cmpita cmpitm cmplab cmplbl cmplot 
	cmpmsk cmpou cmppos cmpsat cmpsup cmptit cmptra cmpusr
.fi
.LP
.I "Tutorial Softfill Examples:"
.nf
	csfwrld csfsgfa
.fi
.LP
.I "Test Programs:"
.LP
.nf
tagupw |  aguprwtx - AUTOGRAPH with PWRITX
tareas | areas     - AREAS
tautog | autograph - AUTOGRAPH
tcolcv | colconv   - COLCONV (color conversion program)
tconre | conrec    - CONREC (standard version)
tcnqck | conrecq   - CONREC (quick version)
tcnsmt | conrecs   - CONREC (smooth version)
tcnsup | conrecsup - CONREC (super Version)
tconan | conran    - CONRAN (standard version)
tconaq | conranq   - CONRAN (quick version)
tconas | conransup - CONRAN (super version)
tconpa | conpack   - CONPACK
tdashc | dashchar  - DASHCHAR (standard version)
tdashl | dashline  - DASHCHAR (quick version)
tdashp | dashsup   - DASHCHAR (super version)
tdashs | dashsmth  - DASHCHAR (smooth version)
tezmap | ezmap     - EZMAP
tezmpa | ezmapa    - EZMAPA (ezmap with areas)
tgflas | gflash    - GFLASH
tgrida | gridal    - GRIDAL
thafto | hafton    - HAFTONE
thstgr | histgr    - HISTGR
tisosr | isosrf    - ISOSRF
tisohr | isosrfhr  - ISOSRFHR
tlblba | labelbar  - LABELBAR
tpltch | plotchar  - PLOTCHAR
tpwrtx | pwritx    - PWRITX
tpwry  | pwrity    - PWRITY
tpwrzi | pwrzi     - PWRZI
tpwrzs | pwrzs     - PWRZS
tpwrzt | pwrzt     - PWRZT
tsoftf | softfill  - SOFTFILL
tsrfac | srface    - SRFACE
tstitl | stitle    - STITLE
tstrml | strmln    - STRMLN
tthree | threed    - THREED
tvelvc | velvct    - VELVCT
.fi
.SH "SEE ALSO"
.LP
\fIUsing NCAR Graphics in a UNIX Environment\fP
.LP
\fINCAR Graphics User's Guide\fP
