.\"
.\"	$Id: ncargsrc.m,v 1.3 1994-08-11 17:54:26 haley Exp $
.\"
.TH NCARGSRC 1NCARG "OCTOBER 1990" NCAR "NCAR GRAPHICS"
.de sf          \"start constant spacing
.ps 10
.vs 12
.nf
.ft L
..
.de ef          \"resume variable spacing & prev. size & font
.ft
.fi
.ps
.vs
..
.SH NAME
ncargsrc \- to retrieve NCAR Graphics source.
.SH SYNOPSIS
\fBncargsrc\fP
[\fB\...args...\fR]
.sp 1
\fIncargsrc\fP is a script that allows the user to retrieve NCAR Graphics
source files.  These files are copyrighted; redistribution is not permitted
except with written permission from UCAR (the University Corporation for
Atmospheric Research).
.sp 1
Note:  For various reasons, files retrieved by "ncargsrc" are temporarily put
in a subdirectory called "NCARGSOURCE" and then moved to another subdirectory
called "ncargsource".  Among other things, this prevents overwriting user
files accidentally.
.SH DESCRIPTION
The argument list is processed from left to right.  Arguments which do not
begin with a dash are the names of source files to be retrieved; those with
a suffixed ".f", ".c", or ".h" are names of specific routines, while those
which have no such suffix refer to collections of routines comprising a
complete utility.  For example, the command
.sp 1
.in +4
.B ncargsrc
areas
.in -4
.sp 1
will retrieve, in a file called "areas", the entire collection of routines
comprising the utility called AREAS, while the command
.sp 1
.in +4
.B ncargsrc
argeti.f
.in -4
.sp 1
will retrieve, in a file called "argeti.f", one member of that collection,
called ARGETI.
.sp 1
Arguments which begin with a dash select options to be applied from then on.
.sp 1
The argument "-index" directs that what should be retrieved for an argument
like "areas" is not the code for the utility called "AREAS", but a file
called "areas.index" containing the names of the routines which make up that
utility.  You can execute a command like
.sp 1
.in +4
.B ncargsrc
-index areas
.in -4
.sp 1
and then, after examining the contents of the file "areas.index", execute
a command like
.sp 1
.in +4
.B ncargsrc
arscam.f
.in -4
.sp 1
to retrieve a specific selected routine from "areas".
.sp 1
The command
.sp 1
.in +4
.B ncargsrc
-index
.in -4
.sp 1
(in which the sole argument is "-index") produces a file called "INDEX" that
contains a listing of the contents of all the source libraries.
.sp 1
The following three source libraries are normally searched first:
.sp 1
.sf
    srcncarg.a     - NCAR Graphics utilities (including
                     default members of CONRAN, CONREC,
                     and DASH families).
    srcncarg_gks.a - NCAR Graphics GKS package.
.ef
.sp 1
The following eight source libraries are normally searched after the first
three:
.sp 1
.sf
    srcagupwrtx.a  - a version of the AUTOGRAPH routine
                     AGPWRT that uses PWRITX.
    srcconraq.a    - the "quick" member of the CONRAN family.
    srcconras.a    - the "super" member of the CONRAN family.
    srcconrcqck.a  - the "quick" member of the CONREC family.
    srcconrcspr.a  - the "super" member of the CONREC family.
    srcdashline.a  - the "quick" member of the DASH family.
    srcdashsmth.a  - the "smooth" member of the DASH family.
    srcdashsupr.a  - the "super" member of the DASH family.
.ef
.sp 1
The following command-line arguments may be used to move libraries to the
beginning of the search list:
.sp 1
.sf
    Argument       Libraries moved to beginning of list
    -------------  -----------------------------------------
    -agupwrtx      srcagupwrtx.a
    -quick         srcconrcqck.a, srcconraq.a, srcdashline.a
    -smooth        srcdashsmth.a
    -super         srcconrcspr.a, srcconras.a, srcdashsupr.a
    -conrecquick   srcconrcqck.a
    -conrecsmooth  srcdashsmth.a
    -conrecsuper   srcconrcspr.a, srcdashsupr.a
    -conranquick   srcconraq.a
    -conransmooth  srcdashsmth
    -conransuper   srcconras.a, srcdashsupr.a
    -dashline      srcdashline.a
    -dashquick     srcdashline.a
    -dashsmooth    srcdashsmth.a
    -dashsuper     srcdashsupr.a
.ef
.sp 1
Moving a library to the beginning of the search list has a simple effect on
the retrieval of a specific file.  The command
.sp 1
.in +4
.B ncargsrc
conrec.f
.in -4
.sp 1
retrieves the default version of "conrec.f", while the command
.sp 1
.in +4
.B ncargsrc
-quick conrec.f
.in -4
.sp 1
retrieves the "quick" version.
.sp 1
The effect (of moving a library to the beginning of the search list) on the
retrieval of an entire utility is not quite so simple.  The command
.sp 1
.in +4
.B ncargsrc
conrec
.in -4
.sp 1
retrieves, in a single file called "conrec", all the routines of the default
version of the utility CONREC, while the command
.sp 1
.in +4
.B ncargsrc
-quick conrec
.in -4
.sp 1
retrieves, in a single file called "conrec", all the routines of the default
version of the utility CONREC, replaces those routines for which there are
"quick" versions with those versions, and then appends all other routines
from the "-quick" libraries to the end of the file "conrec".  The intent of
this is to provide the user with all of the source code required by the
version of the utility implied by the argument list, but it does not always
work quite as one might like.  Some recommendations:
.sp 1
.sf
    To get             Use the following command
    ---------------    ---------------------------------------
    "normal" CONRAN    ncargsrc conran conterp concom
    "quick" CONRAN     ncargsrc conraq conterp concom
    "smooth" CONRAN    ncargsrc conran conterp concom dashsmth
    "super" CONRAN     ncargsrc conras conterp concom dashsupr
    "quick" CONREC     ncargsrc -conrecquick conrec
    "smooth" CONREC    ncargsrc -conrecsmooth conrec
    "super" CONREC     ncargsrc -conrecsuper conrec
    "normal" DASH      ncargsrc dashchar
    "quick" DASH       ncargsrc dashline
    "smooth" DASH      ncargsrc dashsmth
    "super" DASH       ncargsrc dashsupr
.ef
.sp 1
The following table lists all of the named collections of routines in the
source libraries:
.sp 1
.sf
    srcncarg.a      -  areas   autogrph colconv  common
                       concom  conpack  conran   conrec
                       conterp dashchar ezmap    ezmapa
                       gflash  gridal   hafton   histgr
                       isosrf  isosrfhr labelbar plotchar
                       pwritx  pwrity   pwrzi    pwrzs
                       pwrzt   softfill spps     srface
                       stitle  strmln   support  threed
                       velvct
    srcncarg_gks.a  -  awi bwi
    srcagupwrtx.a   -  agupwrtx
    srcconraq.a     -  conraq
    srcconras.a     -  conras
    srcconrcqck.a   -  conrcqck
    srcconrcspr.a   -  conrcspr
    srcdashline.a   -  dashline
    srcdashsmth.a   -  dashsmth
    srcdashsupr.a   -  dashsupr
.ef
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
