.\"
.\"	$Id: ncargpar.m,v 1.1.1.1 1992-04-17 22:30:08 ncargd Exp $
.\"
.TH NCARGPAR 1NCARG "JUNE 1991" NCAR "NCAR GRAPHICS"
.SH NAME
ncargpar \- Retrieve a value from the NCAR Graphics parameter file.
.SH SYNOPSIS
.B ncargpar
parameter_name
.PP
.SH DESCRIPTION
.B ncargpar
echoes to standard output the value of the parameter named by the argument.
The value is retrieved from the NCAR Graphics parameter file, which, in
versions of NCAR Graphics at revision level 3.1.2 and later, is used to
keep track of where the various portions of NCAR Graphics have been installed.
(For example, when EZMAP needs to read the continental outline data, it
retrieves, from the parameter file, the value of the parameter named "DBDIR",
which says in which directory databases have been stored.)  This scheme makes
it possible to move the pieces of NCAR Graphics around.  See also the command
"ncargmv".
