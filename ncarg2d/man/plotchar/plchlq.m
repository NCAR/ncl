.\"
.\"	$Id: plchlq.m,v 1.1 1993-03-11 16:29:32 haley Exp $
.\"
.TH PLCHLQ 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
PLCHLQ - Plots characters of low quality by calling the GKS
character plotting routines.  Using PLCHLQ to plot a given
string of characters will create a smaller metafile than if you
use PLCHHQ or PLCHMQ.  However, the appearance of the output
may not be satisfactory at your site; it depends on
capabilities of the output device.
.SH SYNOPSIS
CALL PLCHLQ (XPOS, YPOS, CHRS, SIZE, ANGD, CNTR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_plchlq (float xpos, float ypos, char *chrs, float size, float angd, float cntr)
.SH DESCRIPTION 
.IP "XPOS, YPOS" 12
The X and Y coordinate positions for the characters
to be plotted. These are world coordinates.
.IP CHRS 12
A string consisting of the characters to be plotted.
The string may include any of the following 95
characters: A-Z a-z 0-9 ! " # $ % & ' ( ) * + - / : ; <
= > ? @ ` [ \ ] ^ _ { | } tilde , .  blank
.sp
Only one font is available with PLCHLQ.
.sp
The number of characters in CHRS is taken to be
LEN(CHRS); to use characters m through n from a
character variable CHRS (where m and n are integer
expressions), use the FORTRAN 77 substring notation
CHRS(m:n).
.IP SIZE 12
Defines a multiplier for character size, in one of
three ways, as listed below. SIZE is defined to be more
or less consistent with SIZE in PLCHHQ. Set the value
of SIZE to a real number in one of the following
ranges:
.RS
.IP Range 12
Description
.IP "<= 0." 12
The absolute value specifies the size as a
multiple of a default digitized size on a
1024x1024 grid on which blanks are 16 units
wide.
.IP "0.<SIZE<1." 12
Specifies the desired width of a blank as a
fraction of the distance across the plotter
frame.
.IP ">= 1." 12
Specifies the desired width of a blank in
plotter coordinates, as defined by default or
by a user call to the SPPS routine SETI.
.RE
.IP ANGD 12
The angle, 
in degrees counterclockwise from the positive X axis,
at which the character string is to be plotted.
.IP CNTR 12
Specifies the centering option. Set the value of CNTR to a real
number as follows:
.RS
.IP Value 12
Description
.IP -1. 12
(XPOS,YPOS) is the center of the left edge of
the first character.
.IP 1. 12
(XPOS,YPOS) is the center of the right edge of
the last character.
.IP 0. 12
(XPOS,YPOS) is the midpoint of the line joining
the center of the left edge of the first
character to the center of the right edge of
the last character.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use PLCHLQ, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_plchlq, load 
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
