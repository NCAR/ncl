.\"
.\"	$Id: ncargmac3.m,v 1.1.1.1 1992-04-17 22:30:46 ncargd Exp $
.\"
.\"============================================================================
.\"
.\"      File:  ncargmac3.m
.\"
.\"   Purpose:  Macros for creating man pages from NCAR Graphics Version 3
.\"		utility documentation files (originally "nroff -mm" format).
.\"
.\"    Author:  rob@ncar  (from kleber@ncar's ncargmac.m)
.\"
.\"============================================================================
.\"
.de *i
.\".nr *I \\w'\\$1'
.\".in \\n(.lu-\\n(*Iu
.\".rm *I
..
.\"----------------------------------------------------------------------------
.\"
.de L1      \"  Level 1 heading
.in 0       \"  Usage:  .L1 "Introduction"  
.ft B       \"  embolden 
.sp
~~ \\$1
.sp 2
.in +.5i
.S 12
.ft R
.fi
..
.\"----------------------------------------------------------------------------
.\"
.de L2      \"  Level 2 heading, mixed case    
.sp 2 
.ne 6
.in 0
.ft B           
.S 14
\\$1
.S 12
.sp
.in +.5i
.ft R
.fi
..
.\"----------------------------------------------------------------------------
.\"
.de L3       \"  Level 3 heading
.br
.ne 3
.sp
.ft B           
\\$1           
.ft R         
.sp .5
.fi
..
.\"----------------------------------------------------------------------------
.\"
.de L4       \"  Level 4 heading
.br             
.ne 3          
.sp 
.ft B          
\\$1
.ft R
.br
.fi
..
.\"----------------------------------------------------------------------------
.\"
.de L5       \"  Level 5 heading
.sp
.ne 2         
.ft B        
\\$1       \"  NOTE: Heading is followed by a colon you must enter
.ft R
.fi
..
.\"----------------------------------------------------------------------------
.\"
.de rt  \" disable return up screen, produces nroff garbage
.sp
..
.\"----------------------------------------------------------------------------
.\"
.de BL   \" set up for * LI's
.VL 6mm
..
.\"----------------------------------------------------------------------------
.\"
.de AL   \" set up for * LI's
.VL 6mm
..
.\"----------------------------------------------------------------------------
.\"
.de ne
..
.\"----------------------------------------------------------------------------
.\"
.de sp             \" redefine blank line feed due to neg. spacing

..
.\"----------------------------------------------------------------------------
.\"
.de }F    \" ignore footer breaks 
..
.\"----------------------------------------------------------------------------
.\"
.de bp    \" ignore page breaks
.sp
..
.\"----------------------------------------------------------------------------
.\"
.de S          \" catch util name and synopsis
.if\\$1>98 \{\  
.N             \" call to print NAME, only once
.ND
.SY
\}
./".if\\$1=11 \{\
./".SY             \" call to print synopsis
./"\}
..
.\"----------------------------------------------------------------------------
.\"
.de N         \" print name at header	
.SH NAME
.rm N
..
.\"----------------------------------------------------------------------------
.\"
.de SY        \" print synopsis on page 1
.SH SYNOPSIS
.nf
.tr ~
.if \\n(sN-0 \\*(S1  \" list all subroutines
.if \\n(sN-1 \\*(S2
.if \\n(sN-2 \\*(S3
.if \\n(sN-3 \\*(S4
.if \\n(sN-4 \\*(S5
.if \\n(sN-5 \\*(S6
.if \\n(sN-6 \\*(S7
.if \\n(sN-7 \\*(S8
.if \\n(sN-8 \\*(S9
.if \\n(sN-9 \\*(s1
.if \\n(sN-10 \\*(s2
.if \\n(sN-11 \\*(s3
.if \\n(sN-12 \\*(s4
.if \\n(sN-13 \\*(s5
.if \\n(sN-14 \\*(s6
.if \\n(sN-15 \\*(s7
.if \\n(sN-16 \\*(s8
.if \\n(sN-17 \\*(s9
.if \\n(sN-18 \\*(N1
.if \\n(sN-19 \\*(N2
.if \\n(sN-20 \\*(N3
.if \\n(sN-21 \\*(N4
.if \\n(sN-22 \\*(N5
.if \\n(sN-23 \\*(N6
.if \\n(sN-24 \\*(N7
.if \\n(sN-25 \\*(N8
.if \\n(sN-26 \\*(N9
.if \\n(sN-27 \\*(N0
.D
.rm SY        \" done only once
..
.\"----------------------------------------------------------------------------
.\"
.de ND        \" print  descript. catted onto util as NA
.nf
\\*(NA
.rm ND        \" done only once
..
.\"----------------------------------------------------------------------------
.\"
.de VL        \" Vertical Line spacing
.ns
.nr.1 \\$1u         \" new indent bump passed and read
.if \\n(.1u=3 \{\    \" some were without units
.nr.1 3mm
\}
.nrf2 \\n(:f        \" f2 holds old front tab for return
.nr:f \\n(.iu       \" f holds new front tab
.nr:b \\n(.iu+\\n(.1u  \" b holds new indent
.nr:b \\n(:bu+0.2i      \" bump b to fit labels better
.fi
.in \\n(:bu             \" set new indent
.ti \\n(:bu            
..
.\"----------------------------------------------------------------------------
.\"
.de P   \" ignore P whatever it was
..
.\"----------------------------------------------------------------------------
.\"
.de pn  \" ignore page numbering
..
.\"----------------------------------------------------------------------------
.\"
.de R   \" ignore return to Regular font since nroff has no font
..
.\"----------------------------------------------------------------------------
.\"
.de LI  \" Labeled pharagraph
.br
.sp                \" print blank
.if \\n(.$ .dsL6 \\$1     \" read label
.if !\\n(.$ .dsL6 "   *  \" set label if none
.in \\n(:bu     \" set indent
.ti 0
.ta \\n(:fu \\n(.iu   \"set tabs
\t\\*(L6\t\c           \" print label, then text
..
.\"----------------------------------------------------------------------------
.\"
.de LE        \" close LI environment
.nr:b \\n(:f  \" reset indent holder to previous
.nr:f \\n(f2  \" reset front tab to previous
.in \\n(:bu   \" reset indent
..
.\"----------------------------------------------------------------------------
.\"
.de H     \" header labels
.D        \" print DESCRIPTION before first header
.rm D      \" and only first
.}X         \" set up page, man commd.
.ti 0.33i    
\&\\$2 \|\\$3 \|\\$4 \|\\$5 \|\\$6 \|\\$7    \" print label
.br
.nrf2 0  \" reset front tab to 0
..
.\"----------------------------------------------------------------------------
.\"
.de D
.br
.sp
.ti -1i
DESCRIPTION
.VL 0.0i
..
.\"----------------------------------------------------------------------------
