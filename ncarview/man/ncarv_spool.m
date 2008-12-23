.\"
.\"	$Id: ncarv_spool.m,v 1.22 2008-12-23 00:04:16 haley Exp $
.\"
.\" ncarv_spool 3.01 90/06/22 
.TH NCARV_SPOOL 5NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
ncarv_spool \- spooled device configuration table for interactive 
metafile translator
.B ictrans
.SH SYNOPSIS
.B $NCARG_ROOT/lib/ncarg/ncarv_spool
.br
.B ~/.ncarv_spool
.SH DESCRIPTION
The
.B ncarv_spool
file contains definitions for spooled devices to be used by 
.BR ictrans .
Each definition describes a rule used for sending a set of metafile
frames to a spooled device. 
.LP
Each entry consists of a line of the form:
.IP
.IB "alias_name " : " translator_args " : " filter_chain"
.sp
.TP 20
.I "alias_name"
an identifier unique to the file. It is the alias by which the definition
may be referenced.
.TP 
.I "translator_args"
.I "translator_args"
is a list of valid 
.B ctrans
command line arguments to be passed to 
.BR ctrans .
Metafile frames are translated by 
.B ctrans
and the output of this translation is made available for further processing
as specified by the remaining spooler definition. 
.TP
.I "filter_chain"
is a set of simple commands separated by the UNIX pipe symbol
.BR | .
.I "filter_chain"
may be terminated by the UNIX file redirection symbols
.B > 
or 
.B >>
.IR filename .
Output from 
.B ctrans
is made available to the processes defined in 
.IR "filter_chain" .
.LP
A pound-sign
.RB ( # )
as the first character indicates a comment line which
is ignored by routines that read this file.
The order of entries in
.B ncarv_spool
is important. If an alias name conflict arises the last definition encountered
is given precedence. Any previous definition for the same alias name is
discarded. 
.SH EXAMPLES
The following is an example 
.B ncarv_spool
file. The first entry describes a device named
.BR postscript .
When 
.B postscript
is named as the spooling device, metafile frames are translated by 
.B ctrans
into the format described by the 
.B ps.mono
graphcap. The font
.B font1
is used for CGM text elements. 
The result of this translation will be piped to 
.BR lpr .
The second entry is named 
.BR dicomed .
The 
.B dicomed 
entry specifies that metafile frames will be rasterized at a 1857 by
1183 pixel resolution and output in X11 xwd format. The result of this 
rasterization is piped to a program 
.B x11tonrif
which in turn sends its output to a program called
.BR rasttg .
In the third example the final output is directed to a file called
.BR sunfile .
.sp
.RS
.ft B
.nf
#
# example ncarv_spool file
#
postscript : -d ps.mono -f font1 : | lpr -Pprinter0
dicomed : -d xwd -geom 1857x1183 : | x11tonrif | rasttg -tc
sunraster : -d xwd -geom 1152x900 : | x11tosun > sunfile
.ft R
.fi
.RE
.SH ENVIRONMENT
.TP
.B NCARG_ROOT
Path to root of NCAR Graphics installation.
.TP
.B NCARG_LIB
If set this variable contains the path to the installed NCAR Graphics 
libraries. 
.B NCARG_LIB
overrides 
.BR NCARG_ROOT .
.SH FILES
.TP 40
$NCARG_ROOT/lib/ncarg/ncarv_spool
- System spooler configuration file
.TP 40
$HOME/.ncarv_spool
- User spooler configuration file
.SH SEE ALSO
.BR ctrans(1NCARG) ,
.BR ictrans(1NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
