.\"
.\"	$Id: ncargintro.m,v 1.1.1.1 1992-04-17 22:30:32 ncargd Exp $
.\"
.TH NCARGINTRO 1NCARG "JUNE 1988" NCAR "NCAR GRAPHICS"
.SH NAME
ncargintro \- NCAR Graphics Overview
.SH SYNOPSIS
\fBncargintro\fP 
.SH DESCRIPTION
.LP
NCAR Graphics is a collection of graphics libraries that supports the
display of scientific data. A level 0a GKS package that generates a
CGM (Computer Graphics Metafile) is also included along with
CGM translators and accompanying device drivers.
.SH "ENVIRONMENT VARIABLES"
The environment variable NCARG_GKS_OUTPUT may be used to direct the metafile
output from the NCAR Graphics GKS package to a file (use just the file name)
or to pipe it to a translator (give the name of the translator, preceded by
a "|").
.LP
The environment variables GRAPHCAP and FONTCAP may be used to tell the CGM
translators "ftrans", "ctrans", "ictrans" and "idt" what default 
"graphcap" and "fontcap" to
use.  See the "man" pages for "ftrans", "ctrans", "ictrans" and "idt"
for more information.
.LP
The environment variable DISPLAY is not actually used by the translators; if
you are using X Windows, it determines where the translator output is to be
displayed.
.SH "SEE ALSO"
.LP
.I "Using NCAR Graphics in a UNIX Environment"
.LP
.I "NCAR Graphics User's Guide"
.LP
.I "NCAR View - A CGM Translation and Manipulation Package"
.LP
.I "NCAR Graphics - Unix Release (system administrators)"
.LP
.I "NCAR Graphics Installer's Guide (system administrators)"
.LP
A Unix command to retrieve NCAR Graphics examples:  ncargex(1).
.LP
Unix commands which facilitate compiling and running with
NCAR Graphics:  ncargf77(1), ncargcc(1), ncargrun(1).
.LP
FORTRAN utilities in NCAR Graphics:  areas(1),
autograph(1),
colconv(1),
conpack(1),
conran(1),
conraq(1),
conras(1),
conrcq(1),
conrcspr(1),
conrec(1),
dashchar(1),
dashline(1),
dashsmth(1),
dashsupr(1),
ezmap(1),
ezmapa(1),
findg(1),
gflash(1),
gridal(1),
hafton(1),
histgr(1),
isosrf(1),
isosrfhr(1),
labelbar(1),
plotchar(1),
pwritx(1),
pwrity(1),
pwrzi(1),
pwrzs(1),
pwrzt(1),
softfill(1),
srface(1),
stitle(1),
strmln(1),
threed(1),
velvct(1).
.LP
CGM translators:  ctrans(1), ictrans(1), idt(1), ftrans(1), cgmtrans(1), plt(1).
.LP
Descriptions of "fontcaps" and "graphcaps":  fcaps(1), fontcap(5), gcaps(1),
graphcap(5).
.LP
Metafile filters:  pre2ncgm(1), psblack(1), pswhite(1), ncgm2cgm(1), 
cgm2ncgm(1).
.LP
A Unix command which tells you what version of NCAR Graphics
you are using:  ncargversion(1).
.LP
A Unix command which tells you what version of NCAR View
you are using:  ncarvversion(1).
.LP
Descriptions of EZMAP examples:  ezmap.exam(1).
