.TH Softfill 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Softfill - Fills a polygonal subset of the plotter frame.
.SH SYNOPSIS
SFSGFA - Can be made to fill polygons by calling the GKS
routine GFA or by calling the lower-level Softfill routines 
SFWRLD and SFNORM.
.sp
SFWRLD - Fills polygons with parallel lines or with dots,
polymarkers, or selected characters arrayed in a regular
rectangular pattern.
.sp
SFNORM - Fills polygons with parallel lines or with dots,
polymarkers, or selected characters arrayed in a regular
rectangular pattern.
.sp
SFGETC - Retrieves the current character value of a specified
internal parameter. 
.sp
SFGETI - Retrieves the current integer value of a specified
internal parameter.
.sp
SFGETP - Retrieves the current value of the dot pattern.
.sp
SFGETR - Retrieves the current real value of a specified
internal parameter.
.sp
SFSETC - Sets the new character value of a specified internal
parameter.
.sp
SFSETI - Sets the new integer value of a specified internal
parameter.
.sp
SFSETP - Sets the new value of the dot pattern.
.sp
SFSETR - Sets the new real value of a specified internal
parameter.
.SH C-BINDING SYNOPSIS
c_sfsgfa
.br
c_sfwrld
.br
c_sfnorm
.br
c_sfgetc
.br
c_sfgeti
.br
c_sfgetp
.br
c_sfgetr
.br
c_sfsetc
.br
c_sfseti
.br
c_sfsetp
.br
c_sfsetr
.SH USER-MODIFIABLE INTERNAL ROUTINES
None
.SH MESSAGES
The following error messages may result from calls to routines
in the SOFTFILL package. All are treated as fatal errors.
.sp
SFSGFA - ERROR EXIT FROM GQFACI 
.sp
SFSGFA - ERROR EXIT FROM GQPLCI 
.sp
SFNORM - COORDINATE ARRAYS TOO SMALL 
.sp
SFNORM - ARRAY DST IS TOO SMALL 
.sp
SFNORM - ARRAY IND IS TOO SMALL 
.sp
SFNORM - LOGIC ERROR - SEE SPECIALIST 
.sp
SFGETC - PARAMETER NAME TOO SHORT - x 
.sp
SFGETC - PARAMETER NAME NOT KNOWN - xx 
.sp
SFGETI OR SFGETR - PARAMETER NAME TOO SHORT - x 
.sp
SFGETI OR SFGETR - PARAMETER NAME NOT KNOWN - xx 
.sp
SFSETC - PARAMETER NAME TOO SHORT - x 
.sp
SFSETC - PARAMETER NAME NOT KNOWN - xx 
.sp
SFSETI OR SFSETR - PARAMETER NAME TOO SHORT - x 
.sp
SFSETI OR SFSETR - PARAMETER NAME NOT KNOWN - xx
.sp
Most of these should be self-explanatory. The "LOGIC ERROR" in
SFNORM should never occur; if it does, it indicates a compiler
problem or some sort of tampering with the code of the package.
Where "x" or "xx" appears, it will be replaced by the offending
parameter name.
.SH SEE ALSO
Online:
softfill_params, sfgetc, sfgeti, sfgetp, sfgetr, sfsetc, sfseti,
sfsetp, sfsetr, sfsgfa, sfwrld, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
