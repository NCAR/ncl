.TH NCARG_ENV 5NCARG "May 1995" NCARG "NCAR GRAPHICS"
.SH NAME
NCAR Graphics environment variables
.SH SYNOPSIS
.sp
What follows is an alphabetical list of all the NCAR Graphics
environment variables and what they mean.
.sp
\fBNCARG_BIN\fP, if set, indicates where the NCAR Graphics binaries
are installed.  This environment variable takes precedence over
NCARG_ROOT and doesn't need to be set unless you want to override the
default ($NCARG_ROOT/bin).
.sp
\fBNCARG_CONFIG\fP, if set, indicates where the NCAR Graphics
configuration files are installed (these are needed by site installers
only).  This environment variable takes precedence over NCARG_ROOT and
doesn't need to be set unless you want to override the default
($NCARG_ROOT/lib/ncarg/config).
.sp
\fBNCARG_DATA\fP, if set, indicates where the NCAR Graphics example
data files are installed.  This environment variable takes precedence
over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/data).
.sp
\fBNCARG_DATABASE\fP, if set, indicates where the NCAR Graphics 
database files are installed.  This environment variable takes precedence
over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/database).
.sp
\fBNCARG_DOC\fP, if set, indicates where the NCAR Graphics PostScript
documentation is installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless you want
to override the default ($NCARG_ROOT/lib/ncarg/doc).
.sp
\fBNCARG_EXAMPLES\fP, if set, indicates where some of the NCAR
Graphics examples are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/examples).
.sp
\fBNCARG_FONTCAP\fP, if set, indicates the default NCAR Graphics
fontcap to be used.
.sp
\fBNCARG_FONTCAPS\fP, if set, indicates where the NCAR Graphics
fontcaps are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/fontcaps).
.sp
\fBNCARG_GKS_BUFSIZE\fP provides you with control over how CGM
output is buffered.
.sp
\fBNCARG_GKS_DEBUG\fP, if set, will result in debugging information
about the GKS I/O subsystem being output to stdout.
.sp
\fBNCARG_GKS_GENCGM\fP allows you to request that the NCAR GKS
package generate binary encoded CGM rather than NCGM (the NCAR 
private encoding of binary encoded CGM).
.sp
\fBNCARG_GKS_OUTPUT\fP allows you to specify what file or process
CGM output is directed to.
.sp
\fBNCARG_GKS_PSOUTPUT\fP allows you to specify what file any PostScript
output from NCAR GKS is directed to (stdout is allowed).
.sp
\fBNCARG_GKS_PDFOUTPUT\fP allows you to specify what file any PDF
output from NCAR GKS is directed to (stdout is allowed).
.sp
\fBNCARG_GRAPHCAP\fP, if set, indicates the default NCAR Graphics
graphcap to be used.
.sp
\fBNCARG_GRAPHCAPS\fP, if set, indicates where the NCAR Graphics
graphcaps are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/graphcaps).
.sp
\fBNCARG_HLUEX\fP, if set, indicates where the NCAR Graphics
HLU examples are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/hluex).
.sp
\fBNCARG_INCLUDE\fP, if set, indicates where the NCAR Graphics include
files are installed.  This environment variable takes precedence over
NCARG_ROOT and doesn't need to be set unless you want to override the
default ($NCARG_ROOT/include).
.sp
\fBNCARG_LIB\fP, if set, indicates where the NCAR Graphics libraries are
installed.  This environment variable takes precedence over NCARG_ROOT
and doesn't need to be set unless you want to override the default
($NCARG_ROOT/lib).
.sp
\fBNCARG_MAN\fP, if set, indicates where the NCAR Graphics man pages are
installed.  This environment variable takes precedence over NCARG_ROOT
and doesn't need to be set unless you want to override the default
($NCARG_ROOT/man).
.sp
\fBNCARG_NCARG\fP, if set, indicates where the NCAR Graphics examples,
databases, resource files, etc. are installed.  This environment
variable takes precedence over NCARG_ROOT and doesn't need to be set
unless you want to override the default ($NCARG_ROOT/lib/ncarg).
.sp
\fBNCARG_NCLEX\fP, if set, indicates where the NCAR Graphics
NCL examples are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/nclex).
.sp
\fBNCARG_NGURL\fP, if set, indicates where the file that
contains the URL to the NCAR Graphics documentation is installed.
This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/ngwww/ngurl).
.sp
\fBNCARG_NGWWW\fP, if set, indicates where the NCAR Graphics HTML
documentation is installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless you want
to override the default ($NCARG_ROOT/lib/ncarg/ngwww).
.sp
\fBNCARG_RESFILES\fP, if set, indicates where the NCAR Graphics
resource files are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/resfiles).
.sp
\fBNCARG_ROOT\fP, indicates the parent directory where all of the NCAR
Graphics binaries, libraries, include files, man pages, examples, etc. are
installed.  This environment variable must be set by anybody who wants
to run NCAR Graphics (or you can optionally set NCARG_LIB, NCARG_BIN, and
NCARG_INCLUDE).
.sp
\fBNCARG_SYSRESFILE\fP, if set, indicates where the NCAR Graphics
system resource file is installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/sysappres).
.sp
\fBNCARG_TESTS\fP, if set, indicates where the NCAR Graphics
test examples are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/tests).
.sp
\fBNCARG_TMP\fP, if set, indicates where NCAR Graphics
temporary files should be written.
.sp
\fBNCARG_TUTORIAL\fP, if set, indicates where the NCAR Graphics
tutorial examples are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/tutorial).
.sp
\fBNCARG_USRRESFILE\fP, if set, indicates where the NCAR Graphics
user resource file is installed.  This environment variable doesn't
need to be set unless you want to override the default (~/.hluresfile).
.sp
\fBNCARG_XAPP\fP, if set, indicates where the NCAR Graphics
X application default files are installed.  This environment variable takes
precedence over NCARG_ROOT and doesn't need to be set unless
you want to override the default ($NCARG_ROOT/lib/ncarg/xapp).
.sp
.SH "SEE ALSO"
.BR ncarg_gks(1NCARG),
.BR ncargintro(5NCARG)
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
