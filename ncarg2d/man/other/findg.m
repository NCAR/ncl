.\"
.\"	$Id: findg.m,v 1.8 2000-07-11 23:03:57 haley Exp $
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
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
