.\"
.\"	$Id: ftrans.m,v 1.3 1993-04-15 16:11:09 haley Exp $
.\"
.TH FTRANS 1NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.SH NAME
ftrans \- NCAR \fICGM\fR translator - will become obsolete in version
4 of NCAR Graphics, so please try using ctrans instead.
.SH SYNOPSIS
.B ftrans
[\ \fB\-d\ \fIdevice\fR\ ]
[\ \fB\-f\ \fIfontcap\fR\ ]
[\ \fB\-r\ \fIrecord number\fR\ ]
.I metafilename
.PP
.SH DESCRIPTION
.B ftrans
is NCAR's Fortran-based metafile translator which translates
.I metafilename
starting at
.I record_number 
(note that this is not frame-number).  If
.I metafilename
is "-", stdin is used and
.I record_number
is ignored. If
.I record_number
is not specified, it defaults to zero.
.PP
The \fIdevice\fP may be specified on the command line.
If not specified on the command line, ftrans looks for
the \fIGRAPHCAP\fP environment variable.
If the device name starts with a "/" or a ".", it is assumed
to be an absolute or relative pathname. This allows an
individual to have a personal graphcap for purposes of
development or testing.  All other graphcap names
are assumed to be in the default installed graphcap library.
Since there is not a default device type, the device
must be specified either on the command line or in
the environment, or an error will result.
.PP
ftrans may determine the font type using the \fIFONTCAP\fP
environment variable, but this defaults to \fIfont1\fP.
If the font name starts with a "/" or a ".", it is assumed
to be an absolute or relative pathname.  As in the case of
graphcaps, this allows one to have a personal fontcap for purposes
of development or testing.  All other fontcap names
are assumed to be in the default installed fontcap library.
.PP
ftrans places device-dependent output on stdout, thus allowing
the use of either a hardcopy spooler or terminal.
.PP
ftrans supports old NCAR metacode and CGM binary-encoded metacode.
An NCAR metafile is organized as a series of 1440 byte records
each of which has a 4 byte preamble. This preamble contains
the useable length of the record in bytes, along with
information regarding frame boundaries and metacode type.
The frame boundary information is used by higher-level 
interfaces to provide random access to metafile frames.
.SH SEE ALSO
.BR cgmtrans(1NCARG),
.BR ctrans(1NCARG),
.BR graphcap(5NCARG),
.BR ncargintro(5NCARG)
