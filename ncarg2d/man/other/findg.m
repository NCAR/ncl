.\"
.\"	$Id: findg.m,v 1.1 1993-03-11 15:22:43 haley Exp $
.\"
.\"	findg.l 3.00 11/15/89 NCAR Graphics
.TH FINDG lNCARG "NOVEMBER 1989" NCAR "NCAR Graphics"
.SH NAME
findg \- locates calls to all entries in pre-GKS NCAR Graphics
.SH SYNOPSIS
\fBfindg <fortran-file\fP
.PP
.SH DESCRIPTION
\fBfindg\fP reads an input FORTRAN file, \fIfortran-file\fP,
from the standard input and prints out
those lines (along with line numbers)
which contain calls to old plotting
entries from the pre-GKS NCAR plot package.
In conversions to GKS, the user should
examine these calls for possible changes.
Many entries will require no change.
For details on needed changes, see the
"Conversion Guide" section of the Version 2.00 User's Guide.
.sp
If the single character $ is read in
column 1 of any input line, then a complete
list of the entry points being searched for
is printed, and execution is terminated.
.PP
.SH SEE ALSO
.nf
\fINCAR Graphics User's Guide, Version 2.00\fR (Oct. 1987)
\fINCAR Generic Package Installer's Guide\fR, Version 3.00 (Nov. 1989)
.fi

