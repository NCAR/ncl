.\"
.\"	$Id: gflash.m,v 1.1.1.1 1992-04-17 22:30:34 ncargd Exp $
.\"
.TH GFLASH 3NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE3
.dsNA " GFLASH - Capture and insert specified portions of graphics ~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~ instructions in subsequent frames
.dsS1 " CALL GFLAS1 (INAME) Starts storage of plotting instructions
.dsS2 " CALL GFLAS2 Stops storage of plotting instructions
.dsS3 " CALL GFLAS3 (INAME) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Inserts saved plotting instructions into the output stream
.dsS4 " CALL GFLAS4 (ID,FNAME) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Associates plotting instructions stored in disk file with ~~~~~~~~~~~~~~~~~~~~~ id for GFLAS3
.nrsN 4
.ds f. man/gflash.l
./" revised 9/27/89 w/new headers, footers, L1 heads 
./" revised 6/13/89 to include tab macro .tA
./" revised 6/27/89 to include correct headers & footers
./" revised 6/29/89 to include macro for right-adjusted headings (cg)
.de *X 		.\" Make an Index Entry
.if \\nJ .tm .IE\tENTRY\t\\$1\t\\$2\t\\$3\t\\$4\t\\$5\t\\$6\t\\n% 
..
.tr ~
.PH ""
.PF ""
.EF "@\s126-%  \s9Movies\fR@@NCAR Graphics Guide to New Utilities@"
.OF "@\s9Version 3.00, October 1989@@\s9Movies~~\s126-%\s9@"
./"  Headers & footers: replace chapter name, utility, and digit before the %
.ll 6.5i
.\"   heading macros for Graphics, Version 3.00, written by Nancy Dawson, 5/89
.EQ         \"  Makes all equations be 12 point, Roman font
.S 99
.EN
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
.de L5       \"  Level 5 heading
.sp
.ne 2         
.ft B        
\\$1       \"  NOTE: Heading is followed by a colon you must enter
.ft R
.fi
..
.de tA
.sp
.ta 2i-1n   \"  tab setup for Type:  Default: line
Type:  \\$1\tDefault:  \\$2
.sp
..
.de >>       \"  display for indented lines
.sp
.nf
..           \"  subsequent lines are indented
.de <<       \"  end of display for indented lines
.fi
.sp
..           \"  subsequent lines are not indented
.de sF       \"  begin FORTRAN (constant spacing font)
.ps 10
.vs 12
.nf
.ft L
..
.de eF       \"  end FORTRAN (resume previous font & prev. size & font)
.ft
.fi
.ps
.vs
..
.ft B
.ps 18  
.*i "MOVIES"
.sp 4
.L1 "Chapter Overview" 
NCAR Graphics Version 3.00 has an enhanced GKS package to accommodate
two higher-level utilities that are useful tools for making
movies, but were not included in 
Version 2.00.  If you have made movies with
the NCAR System Plot Package (NSPP), 
the forerunner of NCAR Graphics Version 2.00, you are
familiar with the NSPP FLASH and SCROLL utilities.  Now 
NCAR Graphics Version 3.00 provides the
functionality of FLASH and SCROLL as new 
GKS-based utilities, GFLASH and STITLE.
.sp
.L2 "What GFLASH and STITLE Do"
The GFLASH routines store and insert picture segments that appear repeatedly 
in a movie.  Such tasks were previously
handled by FLASH in NSPP, and not at all in NCAR Graphics
Version 2.00.  This was due to the necessity of
a higher GKS level than was present in NCAR Graphics Version
2.00.  The only
alternative to obtaining the functionality of the NSPP FLASH
package with NCAR Graphics Version 2.00 was to use a non-NCAR GKS
package that was at level 2A or higher.
.sp
The STITLE routines answer the void created when 
SCROLL was dropped in the conversion from NSPP to NCAR
Graphics Version 2.00.  Movie titles, either scrolled or stationary, are
created with the STITLE utility.  STITLE also adds color as
well as the capability to fade text 
in and out, none of which SCROLL could do.  
.L2 "Making Movies at Your Site"
Production facilities vary from location to location.  To
assure that you have an understanding of the hardware and
software configurations at your site, contact your NCAR
Graphics site representative or local system administrator
to request the latest
available documentation.  
.SK 1  
\&
.ps 16
.ft B
.*i "GFLASH"
.ft R
.sp 3.5
.ps 12 
.L1 "GFLASH Introduction"
GFLASH is a package of Fortran subroutines that provides a limited
picture segmentation capability.  With GFLASH, various parts of a picture may be stored independently in segments and reinserted into any subsequent picture, 
making it unnecessary to regenerate the instructions.  A classic use of these capabilities is in making movies, where,
for example, a background that will be used for many pictures can be saved and 
inserted into all desired pictures in your movie 
without having to regenerate the background for every picture.
.L2 "GKS Requirements" 
GFLASH can be used with any level 2A GKS package, and the
NCAR GKS package has been enhanced to support the level 2
GKS functions necessary to support GFLASH.  However, fully
implemented GKS packages at level 2A provide more complete
picture segmentation capabilities than those offered by
GFLASH, and users of such packages will most likely want to
use those capabilities rather than using GFLASH.
.L2 "GFLASH Replaces FLASH"
GFLASH duplicates the FLASH capabilities of the NCAR System 
Plot Package (NSPP), the pre-GKS graphics package still in 
use at many sites.  The GFLASH subroutines GFLAS1, GFLAS2, 
GFLAS3, and GFLAS4 offer essentially the same functions as 
FLASH1, FLASH2, FLASH3, and FLASH4 provided in FLASH.  
The entries in GFLASH are simpler to use than the old FLASH 
entries, but there is an additional requirement that the 
package must be opened before use and closed after use.  
Also, GFLASH uses disk writes instead of writing to memory, 
as NSPP did, for reasons of portability and consistency with 
the I/O done by NCAR GKS.
.EH "@\s9GFLASH@@@"
.OH "@@@\s9GFLASH@"
.L1 "Using GFLASH with the NCAR GKS Package"
If you will be using GFLASH with a GKS package other than NCAR's,  skip this section and read "Using GFLASH with a Non-NCAR GKS Package" instead.
.L2 "Opening GFLASH"
It is assumed that the metafile workstation is open and
active when a GFLASH entry is called.  (See OPNGKS
documentation in the SPPS section of the \fINCAR Graphics
User's Guide, Version 2.00\fR.)  Additionally, you must call the
GKS entry GOPWK before calling any GFLASH entry.
An error will be reported if this required call has not
been made before attempting to use any of the GFLASH
entries.
.sp
\fB\s11CALL GOPWK(ID, IC, 3)\fR\s12
.VL .60i 
.LI "\fBID\fR"
Any non-negative integer used as a workstation identifier.
This same workstation identifier 
is required by the close workstation call (see below).  
.LI "\fBIC\fR" 
A valid Fortran logical unit number used to write the plotting instructions to disk.  A valid unit number is one not currently being used and not units 5 or 6.  The NCAR GKS package uses unit 2 for its output if OPNGKS
has been used to open GKS, and uses the logical unit number
specified in the user's GOPWK call to open GKS otherwise.  The NCAR GKS package opens all files itself, and no file preconnections are required. 
.LI "\fB3\fR"
A workstation type. "3" indicates 
Workstation Independent Segment Storage (WISS).  WISS is available disk space implemented on disk storage in the NCAR GKS package.
.LE  
.L2 "Closing GFLASH"
A call to the GKS entry GCLWK is required to close GFLASH.  GFLASH must be closed before deactivating and closing the metafile output workstation (before calling CLSGKS if you are using that SPPS entry to close GKS, or
before the calls to GDAWK and GCLKS otherwise). 
.sp
\fB\s11CALL GCLWK(ID)\fR\s12
.VL .60i 
.LI "\fBID\fR"
The same identifier as was used in the call to open GFLASH.
.LE
.L1 "GFLASH Calls" 
.sp
.ne 6
The following GFLASH calls assume NCAR GKS support is
present.  If you are making movies with a non-NCAR GKS
package, refer to the section, "Using GFLASH with a Non-NCAR
GKS Package."
.in 0
.sp 
.ft B  
.S 14
GFLAS1
.S 12
.L2 Purpose
A call to GFLAS1 initiates storage of plotting instructions into
a disk file.  Instructions subsequent to GFLAS1, but prior to a
GFLAS2 call, will be stored on disk rather than inserted
into the output metafile.  
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL GFLAS1\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~(IB)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 224 file man/gflash.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL GFLAS1\fR\s12\h'|\n(41u'~~~~(IB)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.VL 1.2i
.LI "\fBIB\fR" 
Can be any integer between 0 and 99 inclusive, thus making it possible 
to define 100 different storage buffers in a single job step.
.sp 
GFLAS1 automatically assigns the name GNFB\fIxx\fR (GKS New 
Flash Buffer) to the file that will receive subsequent plotting 
instructions, where \fIxx\fR is the integer value of IB.  
For example, if GFLAS1 is called with an argument of 9, 
then subsequent plotting instructions will be stored in file GNFB09.  
You may need to know the name of the disk file where the plotting 
instructions are stored if and when you use GFLAS4.  
The GNFB\fIxx\fR files are all created using Fortran logical 
unit IC as specified in the GOPWK call.
.ne 6
.in 0
.ft B  
.S 14
GFLAS2
.S 12
.L2 Purpose
A call to GFLAS2 terminates putting plotting instructions to disk
and resumes putting plotting instructions to the metafile output.  A
call to GFLAS2 can only be made after a previous call to GFLAS1.  
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL GFLAS2\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~None
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 258 file man/gflash.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL GFLAS2\fR\s12\h'|\n(41u'~~~~None\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.in 0
.ft B  
.S 14
GFLAS3
.S 12
.L2 Purpose 
A call to GFLAS3 inserts the instructions saved on disk with a
previous GFLAS1 identifier IB into the output metafile.  GFLAS3 can be called only after a previous GFLAS1 and GFLAS2 sequence or after a call to GFLAS4.  GFLAS3 also uses Fortran logical unit IC for its reads.
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL GFLAS3\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~(IB)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput 
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 276 file man/gflash.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL GFLAS3\fR\s12\h'|\n(41u'~~~~~(IB)\h'|\n(42u'Integer\h'|\n(43u'Input \h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.VL 1.2i 
.LI "\fBIB\fR"
The same identifying integer used for the (IB) argument in GFLAS1.
.LE
.sp
.ne 6
.in 0
.ft B  
.S 14
GFLAS4
.S 12
.L2 Purpose
A call to GFLAS4 allows you to access a disk file of plotting instructions generated with a GFLAS1 and GFLAS2 sequence in a previous job for use in a GFLAS3 call. 
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL GFLAS4\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~(IB,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~FNAME)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 300 file man/gflash.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL GFLAS4\fR\s12\h'|\n(41u'~~~~(IB,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~FNAME)\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBIB\fR"
This argument to GFLAS4 is the identifier to be used for subsequent GFLAS3 calls, and needs to be between 0 and 99 inclusive just as with the argument IB to GFLAS1.
.LI "\fBFNAME\fR"
The name of the file in which the plotting instructions are stored; FNAME is a Fortran character variable specifying the file name of the disk file.
.LE
.L1 "Using GFLASH with a Non-NCAR GKS Package" 
With the exception of GFLAS4, GFLASH routines can be used with any GKS
package that is level 2A or higher.  GFLAS4 will report an error
message if it is called in a non-NCAR GKS environment.
.sp
The GFLASH entries call the appropriate GKS segmentation functions to provide the desired functionality.   GFLASH requires that WISS (Workstation Independent Segment Storage) be opened before use and closed after use.  
.L2 "Opening GFLASH"
.hw at-tempt-ing
A call to the GKS entry GOPWK is required before calling any
GFLASH entry.  
An error will be reported if the required GOPWK call has not
been made before attempting to use any of the GFLASH entries. 
.sp 
\fB\s11CALL GOPWK(ID, IC, IW)\fR\s12
.sp .5
.VL .60i 
.LI "\fBID\fR"
Any nonnegative integer identifier.
.LI "\fBIC\fR"
A valid Fortran logical unit number for reads.
.LI "\fBIW\fR"
The workstation type for WISS.  IW is implementation-dependent; you can obtain the correct value for it from the documentation for the GKS package being used.
.LE
.L2 "Closing GFLASH"
A call to the GKS entry GCLWK is required to close WISS.  This step is necessary in order to complete usage of GFLASH.
.sp
\fB\s11CALL GCLWK(ID)\fR\s12
.sp .5
.VL .60i
.LI "\fBID\fR" 
The same identifier used in the call to open WISS.
.LE
.sp
.in 0
.in .5i
To avoid possible errors, it is important to know how GFLASH works in
a generic GKS environment.  
.BL2
.LI
A call to GFLAS1 deactivates all active workstations except WISS,
which it activates.  This results in all subsequent plotting
instructions being saved in WISS, not getting sent to any other output
workstation.
.LI
A call to GFLAS2 will deactivate WISS and reactivate all workstations
that were active when GFLAS1 was called.
.LI
A call to GFLAS3 will send all instructions saved by the associated
GFLAS1 and GFLAS2 sequence to all currently active workstations.
.LE
.in 0
.L1 "GFLASH Usage Notes"
The following points may help in your usage of GFLAS1, GFLAS2, GFLAS3,
and GFLAS4.
.BL2
.LI 
It is not legal to call FRAME (or to clear workstations) between a
call to GFLAS1 and GFLAS2.
.LI
The plotting instructions captured by a GFLAS1 and GFLAS2 
sequence reflect the exact environment in effect when 
the instructions were created, including the clipping rectangle and
all attribute settings.  A call to GFLAS3 will insert the requested
instructions just as they were created, without regard to attribute
settings in effect at the time that GFLAS3 was called.  For example,
if you call GFLAS1 and a pink line is drawn and then call GFLAS2,
the instructions to draw a pink line are saved.  If you later 
call GFLAS3 at a time when the current line color is blue, the 
pink line will still be drawn as it was when created in the GFLAS1 
and GFLAS2 sequence.  However, a line drawn after the GFLAS3 call has 
finished executing would be blue.
.LI
Instructions created by a GFLAS1 and GFLAS2 sequence are scaled
and clipped in accordance with the window and viewport settings
at the time the instructions were created.  When the instructions
are inserted into the output stream with a GFLAS3 call, they will
appear as they were created and will not be affected by new
window or viewport settings.  GFLAS3 will have no affect on the
current window and viewport setting, nor will it be affected by
the current window and viewport setting.  The current attribute 
settings at any given point in a job are those that would apply if 
all GFLAS\fIn\fR  calls were removed.
.LI
As described above, when you use the NCAR GKS package, a file 
name of the form GNFB\fIxx\fR is created for each separate 
GFLAS1 and GFLAS2 sequence, and the appropriate plotting 
instructions are saved in those files.  What is done with 
these files is implementation dependent.  On the NCAR Cray 
computers, these files are local datasets and will disappear 
at job termination unless saved as permanent datasets.  
On most other hosts, however, the GNFB\fIxx\fR files will 
remain after job termination, and you will have to purposely 
delete them.  The GNFB\fIxx\fR files have the same file 
attributes as the default metafile GMETA.  However, the 
GNFB\fIxx\fR files are not complete metafiles and cannot 
be plotted independently without error.
.LI
FLASH buffers can be reused within the same executing program; 
that is, the same index can be used more than once in a GFLAS1, 
GFLAS2 sequence.  One common use for this feature is to 
duplicate frames for a movie sequence \(em frame number \fIn\fR
can be put into FLASH buffer 1 and put into the output file 
two or more times using GFLAS3; then frame number \fIn\fR+1 
can be put into FLASH buffer 1 and put to the output file two 
or more times, and so forth.
.LI
No plotting results from a GFLAS4 call.  GFLAS4 simply does the
bookkeeping necessary to associate the plotting instructions stored in
file FNAME with the identifier IB.  A call to GFLAS3 is required to
actually put the instructions in FNAME into the current output
metafile.  There is no restriction on the filename except that it be a
legal name on the current host.
.LI
The disk file specified by FNAME has all the same attributes as a
standard NCAR metafile, and it may be sent across Ethernet or via File
Transfer Protocol (FTP) just like an NCAR metafile.
.LI
The GFLAS4 subroutine is an option only if you are using the NCAR GKS
package.  GFLAS4 uses a specially defined GKS ESCAPE function in the
NCAR GKS package that is not defined in any other GKS package;
consequently, if you try to use GFLAS4 with a non-NCAR GKS package,
you will get an error message to the effect that no ESCAPE function
with function id of -1392 exists.
.LE
./"
./".bp
./".bp
./".*X GFLASH "examples, output" "" "" PAGE START
./".L1 "GFLASH Examples \(em Output"
./".sp -2
./".L2 "Example 1-1"
./".sp
./".MF gflash.meta 1 6.0
./".bp
./".sp
./".L2 "Example 1-2"
./".sp 2 
./".MF gflash.meta 2 6.0
./".bp
./".sp
./".L2 "Example 1-3"
./".sp 2
./".MF gflash.meta 3 6.0
./".SK 1
./".*X GFLASH "examples, output" "" "" PAGE END
./".L1 "GFLASH Examples \(em Code"
./".*X GFLASH "examples, code" "" "" PAGE START
./".sp -2
.L2 "Examples 1-1 through 1-3"
.in -.5i
Note:  The user will have to provide a driver similar to DGFLAS below in
order to utilize test subroutine TGFLAS, which is on the distribution tape.
.sp
.sF
.nf
      PROGRAM DGFLAS
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL TGFLAS (IER)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END


=========================================================================
=========================================================================
 
      SUBROUTINE TGFLAS (IERROR)
C
C PURPOSE                To provide a simple demonstration of the
C                        GFLASH package.
C
C USAGE                  CALL TGFLAS (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                            = 0  If there is a normal exit from GFLASH,
C                            = 1  Otherwise
C
C I/O                    If there is a normal exit from GFLASH,
C                        the message
C
C                          GFLASH TEST SUCCESSFUL . . . SEE PLOTS TO
C                          VERIFY PERFORMANCE
C
C                        is written on unit 6
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       GFLASH
C FILES
C
C LANGUAGE               FORTRAN
C
C HISTORY                Written  by members of the
C                        Scientific Computing Division of NCAR,
C                        Boulder Colorado
C
C PORTABILITY            FORTRAN 77
C
C NOTE                   The call to GOPWK will have to be modified
C                        when using a non-NCAR GKS package.  The third
C                        argument must be the workstation type for WISS.
C
C
C  Data for the graphical objects.
C
      DATA    TWOPI/6.283185/
      DATA    X0C, Y0C, RC, NPTC/ 0.325, 0.5, 0.10 , 210/
      DATA    X0P, Y0P, RP, NPTP/ 0.500, 0.5, 0.35 ,  16/
      DATA    X0S, Y0S, RS      / 0.675, 0.5, 0.10      /
C
C Establish the viewport and window.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C  Initialize the GFLASH package.  If using a non-NCAR GKS package
C  the final argument in the following call should be replaced with
C  the workstation type for WISS.
C
      CALL GOPWK(9,1,3)
C
C  Put a circle in a buffer with identifier 1.
C
      CALL GFLAS1(1)
      DTHETA = TWOPI/NPTC
      CALL FRSTPT(X0C+RC,Y0C)
      DO 20 I=1,NPTC
      ANG = DTHETA*REAL(I)
      XC = RC*COS(ANG)
      YC = RC*SIN(ANG)
      CALL VECTOR(X0C+XC,Y0C+YC)
   20 CONTINUE
      CALL GFLAS2
C
C  Put a polygonal fan in a buffer with identifier 2.
C
      CALL GFLAS1(2)
      DTHETA = TWOPI/NPTP
      DO 10 I=1,NPTP
      ANG = DTHETA*REAL(I)
      XC = RP*COS(ANG)
      YC = RP*SIN(ANG)
      CALL LINE(X0P,Y0P,X0P+XC,Y0P+YC)
   10 CONTINUE
      CALL GFLAS2
C
C  Put a square in a buffer with identifier 3.
C
      CALL GFLAS1(3)
      CALL FRSTPT(X0S+RS,Y0S+RS)
      CALL VECTOR(X0S-RS,Y0S+RS)
      CALL VECTOR(X0S-RS,Y0S-RS)
      CALL VECTOR(X0S+RS,Y0S-RS)
      CALL VECTOR(X0S+RS,Y0S+RS)
      CALL GFLAS2
C
C  Put a background perimeter in a buffer with identifier 4.
C
      CALL GFLAS1(4)
      CALL FRSTPT( 0.0, 0.0)
      CALL VECTOR( 1.0, 0.0)
      CALL VECTOR( 1.0, 1.0)
      CALL VECTOR( 0.0, 1.0)
      CALL VECTOR( 0.0, 0.0)
      CALL GFLAS2
C
C  Create plots.
C
C  Frame 1 -- title, circle, and fan.
C
      CALL WTSTR(0.5,0.91,'FRAME 1',25,0,0)
      CALL GFLAS3(1)
      CALL GFLAS3(2)
      CALL GFLAS3(4)
      CALL FRAME
C
C  Frame 2 -- fan, title, and square.
C
      CALL GFLAS3(2)
      CALL WTSTR(0.5,0.91,'FRAME 2',25,0,0)
      CALL GFLAS3(3)
      CALL GFLAS3(4)
      CALL FRAME
C
C  Frame 3 -- circle, square, fan, and title (note that the change
C             in window affects the WTSTR call, but not the elements
C             in the buffers -- this illustrates the independent
C             nature of the FLASH buffers).
C
      CALL SET (0.,1.,0.,1.,0.,10.,0.,10.,1)
      CALL GFLAS3(1)
      CALL GFLAS3(3)
      CALL GFLAS3(2)
      CALL WTSTR(5.0,9.1,'FRAME 3',25,0,0)
      CALL GFLAS3(4)
      CALL FRAME
C
C  Close the GFLASH package.
C
      CALL GCLWK(9)
C
      END
.eF
.fi
.ad
.L1 "GFLASH Errors"  
GFLASH uses the subroutine SETER in the error handling
package ERPRT77 to generate error messages.  (See Appendix A
for more information about the NCAR Graphics Error Handling
Package.)  After SETER is called, the execution of
your program is terminated unless you have called ENTSR to
set your recovery mode to "recover."  Error messages that you may
encounter while using GFLASH are listed below.  The messages
are grouped according to the subroutines with which they are associated. 
.L2 "GFLAS1 Error Messages"
.VL .25i
.LI "GFLAS1 CALLED CONSECUTIVELY WITHOUT AN INTERVENING "
.LI "GFLAS2 CALL "
.br
You must terminate the usage of one FLASH buffer before
reusing it.
.sp
.LI "THE NON-NCAR GKS PACKAGE IS NOT LEVEL 2"
.br
GFLASH requires GKS level 2A or higher in order to function
in a non-NCAR GKS environment.  
.sp
.LI "WISS MUST BE OPEN BEFORE CALL TO GFLAS1"
.br
GFLASH entries must have access to the appropriate GKS segmentation
functions in order to work.  This is accomplished by making a call to
the GKS entry GOPWK to open WISS.
.sp
.LI "MAXIMUM NUMBER OF OPEN WORKSTATIONS ALLOWED IS 100"
.br
You have called GFLAS1 with more than 100 different
identifiers.
.sp 
.LI "BUFFER IDENTIFIER .LT. 0, OR .GT. 99"
.br
The argument to GFLASH is out of the described range.
.sp
.LI "GKS MUST BE OPEN BEFORE A CALL TO GFLAS1"
.br
You have tried calling GFLAS1 without opening GKS.  A call to the GKS
entry GOPWK is required before calling any GFLASH entry.
.LE
.L2 "GFLAS2 Error Messages"
.VL .25i
.LI "GFLAS2 CALLED WITHOUT CALLING GFLAS1"
.br
GFLAS2 terminates the GFLAS1 job of writing plotting instructions to disk.  GFLAS2 has no functionality if not called in the legal sequence.
.sp
.LI "WISS NOT OPEN AT GFLAS2 TIME"
.br
GFLASH entries must have access to the appropriate GKS
segmentation functions in order to work.  This is accomplished by making a call to the GKS entry GOPWK to open WISS. 
.LE
.L2 "GFLAS3 Error Message"
.VL .25i
.LI "GFLAS3 CALLED WITHOUT CALLING GFLAS2 OR GFLAS4"
.br
GFLAS3 has no access to the plotting instructions that it is
to insert into the output metafile unless a call to either
GFLAS2 or GFLAS4 is made first.
.LE
.L2 "GFLAS4 Error Messages"
.VL .25i
.LI "THE NON-NCAR GKS PACKAGE IS NOT LEVEL 2"
.br 
GFLASH requires GKS level 2A or higher in order to function
in a non-NCAR GKS environment.  
.sp
.LI "WISS MUST BE OPEN BEFORE A CALL TO GFLAS4"
.br 
GFLASH entries must have access to the appropriate GKS
segmentation functions in order to work.  This is accomplished by making a
call to the GKS entry GOPWK to open WISS.  
.sp
.LI "MAXIMUM NUMBER OF WORKSTATIONS ALLOWED IS 100"
.br
You have called GFLAS1 with more than 100 different
identifiers.
.sp
.LI "GKS MUST BE OPEN BEFORE A CALL TO GFLAS4"
.br
You have tried calling GFLAS4 without opening the NCAR GKS
package.  (The GFLAS4 subroutine is an option only if you are using the NCAR GKS package.) 
.sp
.LI "CANNOT BE CALLED WHILE GFLAS1 IS STILL OPEN"
.br
Call GFLAS2 to complete the GFLAS1/GFLAS2 sequence before attempting to call GFLAS4.   
.LE
.L1 "GFLASH Reference Material"
.L2 "Notes for NCAR Users"
On the NCAR CRAY X-MP/48, the default maximum size for a
disk file is 40,000 512-word blocks.  This can be increased
to 500,000 blocks by setting LM equal to the desired number
of blocks on an ASSIGN statement. 
