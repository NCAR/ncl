.\"
.\"	$Id: plchhq.m,v 1.1 1993-03-11 16:29:28 haley Exp $
.\"
.TH PLCHHQ 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
PLCHHQ - Plots high-quality characters from either of two
564-character databases and provides various text manipulation
capabilities. To see all the characters accessed by PLCHHQ,
execute "ncargex plotchar".
.SH SYNOPSIS
CALL PLCHHQ (XPOS, YPOS, CHRS, SIZE, ANGD, CNTR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_plchhq (float xpos, float ypos, char *chrs, float size, float angd, float cntr)
.SH DESCRIPTION 
.IP "XPOS, YPOS" 12
(Real, Input) - 
Specifies the X and Y coordinate positions for the characters
to be plotted. These are world coordinates. See the
description of CNTR below.
.IP "CHRS" 12
(Character, Input) - 
A string specifying the characters to be plotted. The
number of characters in CHRS is taken to be LEN(CHRS);
to use characters m through n from a character variable
CHRS (where m and n are integer expressions), use the
FORTRAN 77 substring notation CHRS(m:n).  
.sp
CHRS may contain embedded function codes, which are
used to reset internal variables affecting the
selection of characters from the database and the way
in which those characters are plotted.  See the manual
for further information on changing fonts, etc.
<<<Question: what manual? should we delete this line?<<<
.sp
Caution: Do not use the characters caret (^),
underscore (_), or single left quotation mark (`).
Although these three are ASCII characters, they do not
exist in the PLCHHQ complex or duplex character sets.
If you accidentally use one of these, your output will
be a five-pointed star, which indicates an unrecognized
character.
.IP "SIZE" 12
(Real, Input) -  
Defines a multiplier for character size.  Set the value
of SIZE to a real number in one of the following ranges:
.RS
.IP Range 12
Description
.IP "\(<= 0." 12
The absolute value specifies the size as a
multiple of the digitized size.
.IP "0.<SIZE<1." 
Specifies the desired width of a
character as a fraction of the distance across
the plotter frame.
.IP "\(>= 1." 12
Specifies the desired width of a character in
plotter coordinates, as defined by default or
by a user call to the SPPS routine SETI.
.RE
.IP "ANGD" 12
(Real, Input) - 
The angle, 
in degrees counterclockwise from the positive X axis,
at which the character string is to be plotted.
.sp
If the internal parameter 'TE' is non-zero ('TE' is
zero by default) and if ANGD is exactly 360., then 
PLCHHQ will not plot any characters. In this case, ANGD
just computes the distances, in the fractional
coordinate system, from the point (XPOS,YPOS) to the
left edge, the right edge, the top edge, and the bottom
edge of a box enclosing the string.  These are stored
as the values of the internal parameters 'DL', 'DR',
\&'DT', and 'DB' and may be retrieved by calls to
PCGETR.
.IP "CNTR" 12
(Real, Input) - 
Specifies the centering option. Set the value of CNTR to a real
number as follows:
.RS
.IP Value 12
Description
.IP -1. 12
(XPOS,YPOS) is the center of the top edge or
the left edge of the first character (depending
on whether that character was plotted down or
across).
.IP 1. 12
(XPOS,YPOS) is the center of the bottom edge or
the right edge of the last character (depending
on whether the plotting direction was left as
down or across).
.IP 0. 12
(XPOS,YPOS) is the midpoint of the line joining
the center of the top edge or the left edge of
the first character (depending on whether that
character was plotted down or across) to the
center of the bottom edge or the right edge of
the last character (depending on whether the
plotting direction was left as down or
across).
.IP s 12
Any real number can be used as s.  (XPOS,YPOS)
is a point obtained by linear interpolation
along the line joining the point associated
with the value -1. and the point associated
with the value +1.
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use PLCHHQ, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_plchhq, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online: 
pcgetc, pcgeti, pcgetr, pcsetc, pcseti, pcsetr, plchhq, plchlq,
plchmq, plotchar, ncarg_cbind
.sp
Hardcopy: "NCAR Graphics User's Guide," Version 2.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
