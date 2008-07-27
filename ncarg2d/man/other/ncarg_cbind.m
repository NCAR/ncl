.\"
.\"	$Id: ncarg_cbind.m,v 1.18 2008-07-27 03:34:10 haley Exp $
.\"
.TH NCARG_CBIND 3NCARG "February 1993" NCAR "NCAR GRAPHICS"
.SH NAME
NCAR Graphics C-binding - Description of how to use the NCAR Graphics
C-binding.
.SH SYNOPSIS
The NCAR Graphics C-binding consists of a collection of C routines
that allow you to call NCAR Graphics Fortran routines from a C program
without having to worry about the C to Fortran interface yourself.
For detailed information about the C-binding for a particular routine,
please see the man page for the corresponding Fortran routine.
.sp
A C-binding has been created for every user entry point in the NCAR
Graphics utilities with the exception of some of the obsolete
utilities listed below.  A GKS level 0A C-binding has also been
created which adheres to the ISO/IEC standard.  
.sp
From here on, a \fIutility\fP C-binding refers to a C-binding for the 
NCAR Graphics utilities (AREAS, CONPACK, etc.).
.SH NAMING CONVENTION
For all of the utility user entry points, the name of the C-binding
function is the same as the Fortran routine name with a "c_"
prepended.  For example, the name of the C-binding for the CONPACK
routine \fBCPBACK\fP is \fBc_cpback\fP.  This naming convention does
not apply to the GKS C-bindings.  Instead, the C-binding names are
more descriptive, like \fBgset_fill_colr_ind\fP which is
equivalent to the Fortran routine \fBGSFACI\fP.  Please see the man
page for ncarg_gks_cbind(3NCARG) the GKS Fortran routine to get the name 
of the corresponding GKS C-binding name.
.sp
The C programming language is case sensitive, so for
convenience the utility C-binding names and their arguments are all in
lower case.  In this man page, the Fortran routines and their arguments 
will be referred to in upper-case.
.SH ARGUMENT LISTS
The argument list of each utility C-binding corresponds with the
argument list of the corresponding Fortran routine except in some 
cases where multi-dimensioned arrays and/or character strings are
involved.  These exceptions will be described in separate
sections.
.sp
The C-binding argument list being similar to the Fortran argument
list makes using the C-bindings easier for people who already know 
the calling sequence of a particular Fortran routine.  For example, 
the NCAR Graphics routine \fBPLCHHQ\fP has the following argument list:
.sp
.nf
PLCHHQ(REAL X, REAL Y, CHARACTER CHRS, REAL SZ, REAL ANG, REAL CNTR)
.fi
.sp
and the corresponding C-binding \fBc_plchhq\fP has the same type of
argument list:
.sp
.nf
c_plchhq(float x, float y, char *chrs, float sz, float ang, float cntr)
.fi
.sp
.SH ARRAYS
One of the exceptions to the utility C-binding argument lists has to do 
with multi-dimensioned arrays.  In Fortran, arrays are stored in
column-major order, while in C they are stored in row-major order.
This means that the subscripts of a multi-dimensioned array need to 
be switched before passing the array to a Fortran routine from C.
.sp
As an example, the \fBCONPACK\fP routine \fBCPRECT\fP takes as one of
its arguments a two-dimensional array, \fIZDAT\fP.  If the Fortran
dimensions of \fIZDAT\fP are to be 25 x 14, then in the C program you
must dimension \fIZDAT\fP to be 14 x 25 before calling the C-binding
\fBc_cprect\fP.  The next three arguments of \fBCPRECT\fP describe the
array in question.  The first of these three arguments, \fIKZDT\fP, is
the first dimension of the array \fIZDAT\fP as it is declared in the
Fortran calling program.  For \fBCPRECT\fP this value would be 25.  For
the C-binding \fBc_cprect\fP, however, this argument is the second 
dimension of the array as declared in the C program.  This value would 
be 25 as well since the subscripts had to be switched in the C program.
The second and third of these three arguments (\fIMZDT\fP and 
\fINZDT\fP) specify the number of elements in each row and column 
(respectively) to be contoured.  If you only want to contour 20 x 10 
of the array \fIZDAT\fP, then \fIMZDT\fP and \fINZDT\fP should be 20 
and 10 respectively in \fBCPRECT\fP.  For \fBc_cprect\fP, these variables
specify the columns and rows, so again, the values would be 20 and 10 
(because the subscripts have to be switched).
.SH CHARACTER STRINGS
Another exception to the argument lists for the utility C-bindings has
to do with routines that return character strings.  The NCAR Graphics
routines that return strings through the parameter list do not have a
string length as part of their argument lists, so you must pass an
extra argument to the C-binding which specifies the maximum length of
the string.  Also, all input strings passed to the C-bindings must
be null-terminated!
.sp
For example, in the routine \fBPCGETC\fP, you pass a parameter name
and it returns the value of that parameter which in this case is a
string.  The two arguments to \fBPCGETC\fP are \fIWHCH\fP and
\fICVAL\fP, where \fIWHCH\fP is the parameter name and \fICVAL\fP is
the string to be returned.  Since the C-binding \fBc_pcgetc\fP needs
to know the length of \fIcval\fP, an extra argument of type "int" must
be passed.  Thus, the arguments for \fBc_pcgetc\fP would be
\fIwhch\fP, \fIcval\fP, and \fIlen\fP, where \fIlen\fP is the length
of \fIcval\fP as it is declared in the C program.  In any case of
having to add an extra argument for the string length, the extra
argument will always be the last one in the list.  If more than one
string length argument needs to be added, then each one should be added 
at the end of the argument list in the order that their corresponding
strings appear.
.sp
There are some routines like \fBAGDSHN\fP which are defined as character
strings themselves.  In this case, the user does not need to pass
a string length since it is already defined.  But, the string
that is returned is declared statically, thus it will go away once
you call the routine again.  If you need to save these character
strings, be sure to copy them to your own local variable.
.SH FUNCTION PROTOTYPES
The C-bindings are intended to be ANSI C compliant.  To get the
correct function prototypes for the utility C-bindings, you can
include <ncarg/ncargC.h>.  For the GKS C-bindings, include
<ncarg/gks.h>.  In some cases, it may be necessary to typecast the
arguments in the utility C-bindings to get the prototypes correct.
For example, the C-bindings do not distinguish between singly and
multiply dimensioned arrays, so if you are passing a multiply dimensioned
float array, you may need to typecast it as a (float *) if this is not how
it is declared in the main program.
.SH COMPILING YOUR PROGRAM
To compile your NCAR Graphics C program with the C-bindings, use the
NCARG application \fBncargcc\fP.  \fBncargcc\fP will take care of loading in
the necessary C/Fortran interface libraries as well as the NCAR
Graphics C and Fortran libraries.  You will either need to set the 
NCARG_ROOT or the NCARG_BIN, NCARG_LIB, and NCARG_INCLUDE environment 
variables in order to run \fBncargcc\fP.  See "man ncargintro" for more 
information.
.sp
If you do not wish to use \fBncargcc\fP, then you can just run it with
no arguments to see what the necessary libraries are, and then put this
information in your Makefile or whatever else you are using to compile
and link your program.  Note:  if you have an ANSI C compiler, it is
important that you define the macro \fBNeedFuncProto\fP on the compile line
so that function prototyping is included.
.SH EXAMPLES
A few examples of C programs that call the NCAR Graphics C-bindings
have been provided for your convenience.  You may be familiar with the
output from these C programs as they were modeled after the Fortran
programs that you can get with the \fBncargex\fP command.  To copy
any one of these C programs in your directory, and then compile, link, 
and run it, type:
.sp
\fBncargex\fP \fIxxxx\fP
.sp
where \fIxxxx\fP is the name of the example you wish to generate.
To see a list of all the C examples, type:
.in .5i
.sp
\fBncargex -C -list\fP
.in -.5i
.sp
\fBncargex\fP uses \fBncargcc\fP to compile and link the C-binding
example programs.
.sp
.in -.5i
.fi
.SH OBSOLETE ROUTINES
C-bindings have not been provided for user entry points in the 
following obsolete utilities:
.sp
.in .5i
Conran_family, Conrec_family, Halftone, Isosrfhr, and Pwrite_family.
.in -.5i
.SH SEE ALSO
Online: 
.BR ncargcc(1NCARG),
.BR ncargex(1NCARG),
.BR ncarg_gks_cbind(3NCARG), 
.BR ncargintro(5NCARG).
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version; User's Guide for 
NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
