.\"
.\"	$Id: cgmtrans.m,v 1.1.1.1 1992-04-17 22:30:20 ncargd Exp $
.\"
.TH CGMTRANS 1NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.SH NAME
cgmtrans \- NCAR \fICGM\fR translator
.SH SYNOPSIS
.B cgmtrans
.I metafilename
[ \fIrecord_number\fR\ ]
.PP
.SH DESCRIPTION
.B cgmtrans
is NCAR's Fortran-based metafile translator which translates
.I metafilename
starting at
.I record_number.
If
.I metafilename
is "-", stdin is used and
.I record_number
is ignored. If
.I record_number
is not specified, it defaults to zero.
.PP
cgmtrans determines the device type using the value of the 
.I GRAPHCAP
environment variable. There are no defaults.
If the device name starts with a "/" or a ".", it is assumed
to be an absolute or relative pathname. This allows an
individual to have a personal graphcap for purposes of
development or testing.  All other graphcap names
are assumed to be in the default installed graphcap library.
.PP
cgmtrans may determine the font type using the
.I FONTCAP
environment variable, but this defaults to
.I font0.
If the font name starts with a "/" or a ".", it is assumed
to be an absolute or relative pathname.  As in the case of
graphcaps, this allows one to have a personal fontcap for purposes
of development or testing.  All other fontcap names
are assumed to be in the default installed fontcap library.
.PP
cgmtrans places device-dependent output on stdout, thus allowing
the use of either a hardcopy spooler or terminal.
.PP
cgmtrans supports old NCAR metacode and CGM binary-encoded metacode.
An NCAR metafile is organized as a series of 1440 byte records
each of which has a 4 byte preamble. This preamble contains
the useable length of the record in bytes, along with
information regarding frame boundaries and metacode type.
The frame boundary information is used by higher-level 
interfaces to provide random access to metafile frames.
.SH SEE ALSO
ctrans(1), ftrans(1), plt(1), graphcap(5)
