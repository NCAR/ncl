.\"
.\"	$Id: plotchar.m,v 1.1.1.1 1992-04-17 22:30:33 ncargd Exp $
.\"
.TH PLOTCHAR 3NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE3
.dsNA " PLOTCHAR - Plot text using one of three quality levels
.dsS1 " CALL PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Plots characters of high quality -- using Hershey set
.dsS2 " CALL PLCHMQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Plots characters of medium quality -- Roman font available
.dsS3 " CALL PLCHLQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Plots characters of low quality -- uses GKS text routines
.dsS4 " CALL PCGETC (WHCH,CVAL) Retrieves current character values
.dsS5 " CALL PCGETI (WHCH,IVAL) Retrieves current integer values
.dsS6 " CALL PCGETR (WHCH,RVAL) Retrieves current real values
.dsS7 " CALL PCSETC (WHCH,CVAL) Sets character values
.dsS8 " CALL PCSETI (WHCH,IVAL) Sets integer values
.dsS9 " CALL PCSETR (WHCH,RVAL) Sets real values
.nrsN 9
.ds f. man/plotchar.l
./" revised 9/20/89 to add index macro definition
./" revised 9/18/89 w/new headers, footers, L1 heads, *i
./" revised 6/13/89 to include tab macro .tA
./" revised 6/27/89 to include correct headers & footers
./" revised 6/29/89 to include macro for right-adjusted headings (cg)
.\" Index Entry Macro Definition
.de *X
.if \\nJ .tm .IE\tENTRY\t\\$1\t\\$2\t\\$3\t\\$4\t\\$5\t\\$6\t\\n%
..
.PH ""
.PF ""
.EF "@\s125-%  \s9Text, Labels, and Legends\fR@@NCAR Graphics Guide to New Utilities@"
.OF "@\s9Version 3.00, October 1989@@\s9Text, Labels, and Legends  \s125-%\s9@"
.EH "@\s9PLOTCHAR@@@"
./"  Headers & footers: replace chapter name, utility, and digit before the %
./"  Put odd header command after first page of text so it won't print on pg1;
./"  Odd header:  .OH "@@@\s9TITLE@"
.ll 6.5i
.\"   heading macros for Graphics, Version 3.00, written by Nancy Dawson, 5/89
.EQ         \"  Makes all equations be 12 point, Roman font
.S 99
.EN
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
.*i "TEXT, LABELS, AND LEGENDS"
.sp 4
.L1 "Chapter Overview" 
This release of NCAR Graphics 
introduces two new utilities:
PLOTCHAR, which plots text, and LABELBAR, which creates a key (or legend) to
accompany your plot.  PLOTCHAR and LABELBAR are documented in this chapter.
.sp
PLOTCHAR plots characters of high, medium, or low quality.  The
Hershey character set is used at the highest quality level.  PLOTCHAR
allows you to control the text size, angle, and position on the
plotter frame.  Superscripting, subscripting, and many special
characters are available.  PLOTCHAR duplicates the functions of the
older utilities PWRITX and PWRITY and the SPPS subroutines PWRIT and
WTSTR, which were part of Version 2.00 of NCAR Graphics.  In addition,
PLOTCHAR provides many additional features, such as tools for
retrieving the size of character strings to be written, increased
control over aspect ratio, subscripting and superscripting even with
lower-quality characters, a medium-quality character set including
lowercase letters, optional constant spacing, increased control over
text angles, and more.  Because of this added flexibility, we
recommend that you use PLOTCHAR unless you are using previously
created files that already contain calls to PWRIT and WTSTR.
.sp
LABELBAR lets you create a labeled, filled, rectangular bar to serve
as a key for a filled plot.  Labels can be positioned above, below, or
on both sides of a horizontal bar, or to the right, left, or on both
sides of a vertical bar.  Fill may be in color or black-and-white, and
it may be solid or patterned.
.L2 "Additional Methods to Plot Text"
Besides using PLOTCHAR, there are other ways to plot text with NCAR
Graphics.  With the release of Version 3.00, a set of fonts and many
special characters are available at the GKS level.  This set of fonts
adheres to the GKS text attribute standards and is accessed through
the GKS routine GTX.  Because these fonts require a lot of disk space,
some sites may choose not to install them.  Check with the NCAR
Graphics package installer or your NCAR Graphics site representative
to see if you have access to these fonts at your site.  These fonts,
known as the Hershey fonts, are in NCAR Graphics; they are not
necessarily included in other GKS implementations.
Three utilities are available for plotting text in projections 
of three-dimensional planes:  PWRZI, PWRZS, and PWRZT.  PWRZI is used 
with the utility ISOSRF;  
PWRZS is used with the utility SRFACE; and PWRZT is  
used with the utility THREED.  The  
utilities PWRZI, PWRZS, PWRZT,
ISOSRF, SRFACE, and THREED
are documented in
the \fINCAR Graphics User's Guide, Version 2.00.\fR
.ps 16
.B
.*i "PLOTCHAR"
.R
.sp 3.5
.ps 12
.EH "@\s9PLOTCHAR@@@"
.OH "@@@\s9PLOTCHAR@"
.L1 "PLOTCHAR Introduction"
PLOTCHAR plots characters of high, medium, or low quality,
depending on which subroutine you call.  PLOTCHAR contains
the user-callable subroutines
PLCHHQ, PLCHMQ, PLCHLQ, 
PCSETC, PCSETI, PCSETR,
PCGETC, PCGETI, and PCGETR.
These subroutines fall into three categories:  subroutines for plotting
text, subroutines for setting internal parameter values, and
subroutines for retrieving internal parameter values.
.L2 "Subroutines for Plotting Text"
PLCHHQ, PLCHMQ, and PLCHLQ plot text of high, medium, or low quality,
respectively.  PLCHHQ produces the highest quality characters and
gives you the most control over the appearance and position of the
text string, but it also produces the largest metafile and takes the
most computer time.
.sp
The argument lists of all three subroutines are essentially identical.
The arguments provide control over the position of the text on the
plotter frame, the text size, and the angle at which it is written.
In addition, PLCHHQ and PLCHMQ have internal parameters that provide
control over other aspects of text plotting.  You can set the values
of internal parameters by calling the parameter access subroutines.
The three subroutines for plotting text are as follows:
.sp
PLCHHQ provides variable-width characters in both Roman and Greek
fonts, many special characters, and several text-manipulation
capabilities.  With PLCHHQ, you can choose a serif or sans serif
character set for the Roman alphabet, uppercase and lowercase letters,
superscripting and subscripting, and direction of the plotted text.
These additional capabilities are controlled by \fIfunction codes\fR,
which are included in the Fortran character string.
.sp
PLCHMQ provides both uppercase and lowercase Roman characters
as well as many other characters
from the standard set of printable ASCII characters.  PLCHMQ 
requires less computing time than PLCHHQ, and the metafile is smaller.
.sp
PLCHLQ plots characters 
of the lowest (more accurately, the least predictable)
quality.  It calls GKS primitives, 
thus creating the smallest metafile and requiring
the least computing time of the three character-plotting subroutines.
However, the appearance of PLCHLQ output is
dependent on the capabilities of the translator at your site.
.L2 "Subroutines for Setting Internal Parameter Values"
The behavior of PLOTCHAR can be controlled by internal parameters in
internal common blocks to which two of the PLOTCHAR subroutines have access.
PLCHHQ can access 20 of the internal parameters, and PLCHMQ can access 1 
internal
parameter.
You can use the subroutines with names of the form PCSET\fIx\fR to
give new values to internal parameters.  For example, you can call
PCSETI to set the internal parameter \&'CD', which specifies either the
complex or duplex font.
.L2 "Subroutines for Retrieving Internal Parameter Values"
The subroutines with names of the form PCGET\fIx\fR retrieve the
current values of PLOTCHAR's internal parameters.  This capability is
furnished for consistency with other utilities in the NCAR Graphics
package and also for programmers who build interfaces to PLOTCHAR and
need to save and restore the values of its internal parameters.
.L2 "Strategies for Using PLOTCHAR"
Since PLCHHQ requires the most time and the most computing resources
to plot characters, there are some alternative ways to use the
lower-quality subroutines.  The following criteria should help you
decide which subroutine to use:
.sp
.AL
.LI
If you need the special characters provided by PLCHHQ (the Greek
font, for example), then you must use that routine.
.br
.ne 4
.LI
If you need to use function codes for subscripting or superscripting,
for varying size and case within a string, or for writing characters
down, rather than across, the frame, then you must use PLCHHQ.
.LI
If you want the more detailed characters provided by PLCHHQ, and are
willing to wait for them to be plotted, use PLCHHQ.
.LI
If none of the above apply, call PLCHMQ or PLCHLQ directly.
.LI
If PLCHHQ is used, but no special characters are required,
then you may wish to set the quality flag \&'QU' to make PLCHHQ use the
lower-quality characters.  This still gives you the capabilities of 
subscripting and
superscripting, varying case and size, and writing down, rather than
across, the frame, but it saves time because the characters don't take
so long to plot.
.sp
You may want to use \&'QU' to save time while checking out other aspects
of your code and then go back to high-quality characters for the
final production run.
.LE
.L1 "PLOTCHAR Calls"
.ne 6
.in 0
.ft B           
.S 14
PLCHHQ
.S 12
.L2 Purpose
PLCHHQ plots high-quality characters 
from either of two 564-character databases
and provides various text-manipulation
capabilities.  To see all the characters accessed by PLCHHQ, turn to the 
"PLOTCHAR Examples \(em Output" section at the end of the PLOTCHAR
portion of this chapter.
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
.nr 38 \w\fB\s11CALL PLCHHQ\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB   Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w   (XPOS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    YPOS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    CHRS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    SIZE,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    ANGD,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    CNTR)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
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
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
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
.if t .if \n(TW>\n(.li .tm Table at line 230 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB   Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PLCHHQ\fR\s12\h'|\n(41u'   (XPOS,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    YPOS,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    CHRS,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    SIZE,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    ANGD,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    CNTR)\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBXPOS"
.LI "YPOS\fR"
The X and Y coordinate positions for the characters to be plotted.
These are world coordinates.  See the description of CNTR, below.
.LI "\fBCHRS\fR"
A string specifying the characters to be plotted.
The number of
characters in CHRS is taken to be LEN(CHRS); to use characters \fIm\fR
through \fIn\fR from a character variable CHRS (where \fIm\fR and \fIn\fR are integer
expressions), use the FORTRAN 77 substring notation CHRS(\fIm\fR:\fIn\fR).
CHRS may contain embedded \fIfunction codes\fR, which are used to reset
internal variables affecting the selection of characters from the
database and the way in which those characters are plotted.
.sp
CHRS is scanned, character by character, from left to right.  The
scanner has two states:  one in which characters are being selected
for plotting (which is the initial state), and another in which
function codes are being processed.  The function-code delimiter
specified by the internal parameter \&'FC' (by default, a
colon) is used in CHRS to cause a switch from one state to the other.
Thus, function codes may be thought of as being enclosed in pairs of
such characters.  (Note that the character chosen for \&'FC' is thereby
made unavailable for any other use in CHRS.)
.sp
What characters may be used in CHRS depends on what state the scanner
is in when each of those characters is encountered.  (Of course, the
character specified by \&'FC' may always be used and always has the
effect of changing the state of the scanner.)  When characters are
being selected for plotting, any character from one of the following
two groups may be used:
.>>
Group 1:  A-Z 0-9 + - * / ( ) $ = blank , .
.<<
Each of the 47 characters in group 1 is a standard Fortran character.
What character is plotted in response to each of these depends on the
function codes that have preceded it.  (In the absence of any such
function codes, the obvious character is plotted.)  It is possible
to access all 564 characters in the database by using only these
characters together with the appropriate function codes.  The way in
which this is done is discussed below, under the heading "Character
Access Scheme."
.>>
Group 2:  a-z ! " # % & : ; < > ? @ [ \\ ] { | } tilde \&'
.<<
Each of the characters in group 2 selects for plotting that character
in the database which most closely resembles it.  Preceding function
codes have no effect on the character selected.  Except for the colon
and the apostrophe, characters in group 2 are not standard Fortran
characters, and the Fortran standard says that the effect of using them is
"processor dependent."  In practice, use of these characters in a
character string is unlikely to cause a problem on any current
computing system and it is certainly more convenient to use them.
.sp
When function codes are being processed, the following characters may
be used:
.>>
A B C D E G H I K L N P Q R S U V 0-9 blank ,
.<<
Not all combinations of these characters are correct; the permissible
function codes are described below, under the heading "Function
Codes for Use with PLCHHQ."
.L3 "Character Access Scheme"
By default, PLCHHQ uses what is called the \fIcomplex\fR character set.
PLCHHQ may be made to use another dataset, called the \fIduplex\fR set, by
giving the internal parameter \&'CD' the value 1.  Since the number of
characters in each of these datasets considerably exceeds the number
of standard Fortran characters, there cannot be a one-to-one mapping
between the characters in CHRS and the characters plotted.  Function
codes make it possible to access all of the 564 characters in one of
these databases while using only standard Fortran characters in CHRS.
.sp
The 564 characters in each database are divided into 12 groups of
47 characters apiece.  Each of the 12 groups is characterized by
a font (Roman or Greek), a size (principal, indexical, or
cartographic, where principal is the largest and 
cartographic is the smallest),
and a case (upper or lower).  Within each of the
12 groups, each character is associated with one of the following
47 Fortran characters:
.sp
    A-Z 0-9 + - * / ( ) $ = blank , .
.sp
To specify a particular character in the database, it suffices to
specify the font, the size, the case, and one of the 47 Fortran
characters.  The font, size, and case are specified by
internal variables (with initial values of Roman, principal, and upper,
respectively).  These are subject to change by means of function
codes.  The selector character simply appears in CHRS.
.L3 "Examples of Character Access"
CHRS = \&'A' plots an upper case Roman "A" of principal size.
.sp
CHRS = \&':L:A' plots a lower case Roman "a" of principal size.
.sp
CHRS = \&':IGL:AB' plots an indexical-size, lower case Greek alpha,
followed by an indexical-size, lower case Greek beta.
.L3 "Positioning of Characters"
The positioning of characters drawn by PLCHHQ is controlled by four
internal variables specifying the current X coordinate, the current
Y coordinate, the level (normal, superscript, or subscript)
and the direction (across or down).  Initially, as the scan of
CHRS begins, the X and Y coordinates are as required by the values
of the arguments XPOS, YPOS, and CNTR, the level is normal, and the
direction is across.  As the string is scanned and characters are
plotted, the values of the four internal variables change.  The X and
Y coordinates change automatically to reflect the position of the
last character drawn; in addition, each of the four variables is
subject to change as a result of function codes that appear in CHRS.
These function codes are described below in the section called
"Function Codes for Use with PLCHHQ."
.L5 "Caution:"
Do not use the characters caret (^), underscore (_), or single left
quotation mark (`).  Although these three are ASCII characters, they
do not exist in the PLCHHQ complex or duplex character sets.  If you
accidentally use one of these, your output will be a five-pointed
star, which indicates an unrecognized character.
.br
.ne 8
.sp
.LI "\fBSIZE\fR"
SIZE defines a multiplier for character size.  Set the value of SIZE
to a real number in one of
the following ranges:
.sp
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The absolute value specifies
the size as a multiple of the digitized size.  
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Specifies the desired width of a 
principal-size blank as a fraction 
of the distance across the plotter frame.  
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Specifies the desired width
of a principal-size blank in 
plotter coordinates, as defined by default
or by a user call to the SPPS routine SETI.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.<SIZE<1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGE 1.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 379 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBRange\h'|\n(41u'Description\fR
.sp
./"LE 0.;T{
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.<SIZE<1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GE 1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-23
.sp
See the sections "PLOTCHAR Parameter Access Subroutines" and "PLOTCHAR Internal Parameter Descriptions" for more detailed information.
.sp
.LI "\fBANGD\fR"
The angle at which the character string is to be plotted,
in degrees counterclockwise from the positive X axis.
.sp
If the internal parameter \&'TE' is non-zero ('TE' is zero by default) 
and if ANGD is exactly 360., then no characters
are plotted by PLCHHQ.  In this case, ANGD just computes the distances,
in the fractional coordinate system, from the point (XPOS,YPOS) to the
left edge, the right edge, the top edge, and the bottom edge of a box
enclosing the string.  These are stored as the values of the internal parameters
\&'DL', \&'DR', \&'DT', and \&'DB' and may be retrieved by calls to PCGETR.
See the section "PLOTCHAR Parameter Access Subroutines" for complete details.
.LI "\fBCNTR\fR"
The centering option.  Set the value of CNTR to a real number as follows:
.sp
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the center of the top edge or the
left edge of the first character (depending on whether that character was
plotted down or across).
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the center of the bottom edge or
the right edge of the last character (depending on whether the plotting
direction was left as down or across).
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the midpoint of the line joining the
center of the top edge or the left edge of the first character 
(depending on whether that character was plotted down or across)
to the center of the bottom edge or the right edge of the last character
(depending on whether the plotting direction was left as down or across).
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 81
.br
.di d+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Any real number can be used as \fIs\fR.  (XPOS,YPOS) is a point 
obtained by linear interpolation along the line joining the point 
associated with the value -1. and the point associated with the value +1.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIs\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(d-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 429 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne 8
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fIs\fR\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.d+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-31
.LE
.in -.5i
.EQ
.EN
.in -1i
.L2 "Function Codes for Use with PLCHHQ"
You can include function codes in the character string CHRS to
change internal variables controlling the font, size, case,
level, and plotting direction.
The defaults are as follows:
.in +1.2i
.sp
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
.rm 80 81
.nr 80 0
.nr 38 \w\fBFunction
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wFont
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSize
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCase
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLevel
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wDirection
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDefault\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wRoman
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPrincipal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wUpper
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNormal 
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAcross
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 451 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBFunction\h'|\n(41u'Default\fR
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Font\h'|\n(41u'Roman
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Size\h'|\n(41u'Principal
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Case\h'|\n(41u'Upper
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Level\h'|\n(41u'Normal 
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Direction\h'|\n(41u'Across
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-9
.in -1.2i
.sp
Function codes are enclosed in
pairs of function-code delimiters,
which are colons by default.
(Apostrophes are a poor choice for the function-code delimiters.
If you must use apostrophes, be aware of the FORTRAN 77 conventions for
defining strings containing apostrophes.)
No punctuation is needed between function codes except for a comma or a blank between
adjacent numbers; however, extra commas and blanks may be used between
function codes to improve readability.
.sp
All the legal function codes are described below.  Any other characters in a
function-code string will be ignored and an error message will be
printed.
.L3 "Function Code Example"
CHRS = \&':PRU:A:S:2:N:+B:S:2:N:'  will
be plotted as "A squared + B squared"
.sp
Starting with the first colon, the pairs of colons enclose the function codes
PRU, S, N, S, and N.
All other characters will be plotted.
.L3 "Font Definitions"
.VL 1.2i
.LI "\fBR\fR"
Selects the Roman font.
.LI "\fBG\fR"
Selects the Greek font.
.sp .5
Default:  R
.LE
.br
.ne 9
.L3 "Size Definitions"
.VL 1.2i
.LI "\fBP\fR"
Selects characters of principal size.
.LI "\fBI\fR"
Selects characters of indexical size.
.LI "\fBK\fR"
Selects characters of cartographic size.
.sp .5
Default:  P
.LE
.L3 "Case Definitions"
.VL 1.2i
.LI "\fBU"
.LI "U\fIn\fR"
Selects uppercase letters.  If U is followed by a number \fIn\fR (with no 
intervening comma), then \fIn\fR
characters will be plotted in upper case; subsequent characters will
be in lower case.  The U\fIn\fR option is particularly useful for
capitalizing sentences.
.br
.ne 5
.LI "\fBL"
.LI "L\fIn\fR"
Selects lowercase letters.  If L is followed by a number \fIn\fR, then \fIn\fR characters will be plotted in
lower case; subsequent characters will be in upper case.
.sp .5
Default:  U
.LE
.L3 "Level Definitions"
.VL 1.2i
.LI "\fBB"
.LI "B\fIn\fR"
Selects subscript level.  If B is followed by a number
\fIn\fR, then \fIn\fR characters will
be plotted at the subscript level.
After the indicated subscripting,
the internal variables controlling size, case, and position are automatically
restored to the values they had after the base character was plotted.
.LI "\fBS"
.LI "S\fIn\fR"
Selects superscript level.  If S is followed by a number
\fIn\fR, then \fIn\fR characters will be plotted at the superscript level.
After the indicated superscripting, 
the internal variables controlling size, case, and position are automatically
restored to the values they had after the base character was plotted.
.sp
If a character count is given, do not change level again before that count is
satisfied.
(For example, do not use \&':S3:12:B:3'.)
If you accidentally do this, the effect will be the same as if the
character count had been omitted (in the example above, as if you had used
\&':S:12:B:3').
.LI "\fBE\fR"
Ends subscripting or superscripting, returns to original position.
The function code E terminates the current level of subscripting
or superscripting and restores internal variables controlling
the size, case, and position to the values
they had after the base character was plotted.
This is useful when you need to place a superscript directly over a
subscript.
.LI "\fBN\fR"
Ends subscripting or superscripting, returns to normal position.
The function code N terminates the current level of subscripting
or superscripting and restores the internal variables controlling
the size and case to the values they had after the base
character was plotted.
The function code N also advances the position to the spot
required to continue plotting at the
level of the base character without overwriting the subscripts or
superscripts.
.sp .5
Default:  N
.L4 "Changes in Character Size"
When subscripting or superscripting, the character size will
change depending on the base character.  Principal base characters
are subscripted or superscripted with indexical characters, with a
shift of \&'PS' digitization units up or down (10, by default).
Indexical or cartographic base characters are subscripted or
superscripted with cartographic characters, with a shift of \&'IS'
units up or down (7, by default).
.L4 "Changes in Case"
The case of the subscripting and superscripting 
characters will generally be the same as
that of the base character.  An exception is that a lowercase
indexical base will be subscripted or superscripted with uppercase 
cartographic, as the cartographic size has no lowercase
alphabetic or numeric characters available.
.sp
Automatic size and case changes can, of course, be overridden.
For example, the string \&'A:SPU:B' would be used to plot 
"A to the B",
with both A and B being of principal size and in uppercase.
.L4 "Examples of Level Changes"
\&'10:S:10:S:100' will be plotted as "10 to the 10th to the 100th"
.br
Up to five levels are allowed.
.sp
.ne 4
\&\fR'X:B1:2:S1:3' will be plotted as "X sub 2 cubed"
.br
When the character count in the B1 is satisfied, the
original base character is reinstated so that you can add
another subscript or superscript to it.
.sp
\&\fR'X:B1:2:S:3:N:Y:S:3' will be plotted as "X sub 2 cubed Y cubed"
.br
The function code N returns to the normal level and advances the
position in such a way that the character Y does not overwrite the 2
and the 3.
.sp
\&\fR'X:S:A:B:1:NN:...'  will be plotted as "X to the A sub 1"
.br
Note that two function code
N's are used here \(em  one to terminate plotting the subscript 1 and
another to terminate plotting the superscript $A sub 1$ and
leave the current position such that what follows won't be plotted on
top of the $A sub 1$ superscript.
.LE
.EQ 
.EN
.L3 "Coordinate Definitions"
.hw units
.VL 1.2i
.LI "\fBH\fIn\fR"
.LI "\fBH\fIn\fBQ\fR"
Horizontal move.  Increments the current position in the direction
specified by ANGD.  H\fIn\fR shifts the position by \fIn\fR
digitization units; \fIn\fR can be negative.  H\fIn\fRQ shifts the
position by \fIn\fR blank widths; if \fIn\fR is equal to zero, it is
taken to be 1.
.br
.ne 6
.LI "\fBV\fIn\fR"
.LI "\fBV\fIn\fBQ\fR"
Vertical move.
Increments the current position in the direction specified by
ANGD+90.  V\fIn\fR shifts the position by \fIn\fR digitization units; \fIn\fR 
can be negative.  V\fIn\fRQ shifts 
the position by \fIn\fR blank heights; if \fIn\fR is
equal to zero, it is taken to be 1.
.LI "\fBC\fR"
Carriage return.
A carriage return will be done before the next character is
plotted.
.LE
.br
.ne 5
.L3 "Direction Definitions"
.VL 1.2i
.LI "\fBD\fR"
.LI "\fBD\fIn\fR"
Plots down, rather than across, the frame.
If D appears without a following integer \fIn\fR or if \fIn\fR is zero,
characters will be plotted down
until an A
is encountered.  If
\fIn\fR appears, \fIn\fR characters will be plotted down and subsequent
characters will be plotted across the frame.
.LI "\fBA\fR"
Write across the frame.
Stop plotting down.
.sp .5
Default:  A
.LE
.ne 14
.L3 "Direct Character Access"
.VL 1.2i
.LI "\fInnnn\fR"
Numeric character.
The character with octal index \fInnnn\fR in the database
will be plotted.  The octal number for a given character is the
sum of the following:
.in -.2
.BL
.LI
A font index (0 = Roman, 600 = Greek).
.LI
A size index
(0 = principal, 200 = indexical, and 400 = cartographic).
.LI
A case index (0 = uppercase, 100 = lowercase).
.LI
The octal
equivalent of the character (1-32 = A through Z, 33-44 = 0
through 9, and 45-57 = the individual characters +  -  *  /
(  )  $  =  blank, comma, and period).
.LE
.in +.2
.L4 "Example of Direct Character Access"
\&':GIU:*' is the same as \&':1047:'
.br
(600+200+0+47=1047, octal)  
.sp
To know what character PLCHHQ will produce in response to a given octal
code, see the output and code for PLOTCHAR Examples 1-1
through 1-10.
.LE
.ne 6
.in 0
.ft B           
.S 14
PLCHMQ
.S 12
.L2 Purpose
PLCHMQ plots characters of medium quality.  It provides a single font.  The font contains uppercase and lowercase letters of the Roman alphabet and some special characters.  All characters are constant-spaced. 
.sp
The characters produced by PLCHMQ have less detail than those produced by PLCHHQ, and you cannot use function codes with PLCHMQ.  As a result of these differences, PLCHMQ produces smaller metafiles than PLCHHQ.  See the "PLOTCHAR 
Examples \(em Output" section for a comparison of the characters plotted with PLCHHQ and PLCHMQ.  
.sp
At some sites, the output from PLCHLQ (plot characters, low quality)
is almost the same as the output from PLCHMQ.  Since PLCHLQ produces
metafiles that are smaller than metafiles produced with PLCHMQ, you
may find that using PLCHLQ instead of PLCHMQ is satisfactory.  It will
depend on the capabilities of the translator at your site.  You may
need to use PLCHMQ rather than PLCHLQ to produce satisfactory output.
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
.nr 38 \w\fB\s11CALL PLCHMQ\fR\s12   
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fBArgument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(XPOS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w YPOS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w CHRS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w SIZE,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w ANGD,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w CNTR)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput 
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
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
.nr 38 \w 
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
.if t .if \n(TW>\n(.li .tm Table at line 714 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fBArgument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PLCHMQ\fR\s12   \h'|\n(41u'(XPOS,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' YPOS,\h'|\n(42u'Real\h'|\n(43u'Input \h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' CHRS,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u' 
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' SIZE,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' ANGD,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' CNTR)\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBXPOS\fR"
.LI "\fBYPOS\fR"
The X and Y coordinate positions for the characters to be plotted.
These are world coordinates.  
.LI "\fBCHRS\fR"
A string consisting of the characters to be plotted.
The
string may include any of the following 95 characters:  A-Z  a-z  0-9
!  "  #  $  %  &  \&'  (  )  *  +  -  /  :  ;  <  =  >  ?
@  `  [  \&\\  ]  ^  _   {  |  }  \&tilde  ,  .  blank
.sp
Function codes
may not be used \(em they are available in PLCHHQ only.
.sp
The number of characters in CHRS is taken to be LEN(CHRS); to use
characters \fIm\fR through \fIn\fR from a character variable CHRS
(where \fIm\fR and \fIn\fR are integer expressions), use the FORTRAN 77
substring notation CHRS(\fIm\fR:\fIn\fR).
.sp
.LI "\fBSIZE\fR"
SIZE defines a multiplier for character size, in one of three ways, as
listed below.  SIZE is defined to be more or 
less consistent with SIZE in PLCHHQ.  Set  
the value of SIZE to a real number in one of the following ranges:
.sp
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The absolute value specifies the
size as a multiple of a 
default digitized size on a 1024x1024 grid, on
which blanks are 16 units wide.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Specifies the desired
width of a blank as a fraction of the distance across the plotter frame.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Specifies the desired width
of a blank in plotter coordinates, as defined by default or by a user
call to the SPPS routine SETI.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.<SIZE<1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGE 1.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 764 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBRange\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.<SIZE<1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GE 1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.sp
Blanks are 16 units wide.  Capital letters are 4/6 as wide as a blank
and, by default, 7/4 as high as they are wide.  The distance from the
center of each character to the center of the next is constant and is
equal to one blank width.  You can change the aspect ratio of
characters by changing the internal parameter \&'HW', which is
described in the section, "PLOTCHAR Internal Parameter Descriptions."
.sp
If you change a CALL PLCHHQ to a CALL PLCHMQ (assuming that no function
codes are used in the PLCHHQ
string), the output of the two will be much the same.
However, the characters produced by PLCHMQ will be slightly shorter 
and the length of the string will be
different.  Because the high-quality characters use variable spacing and
because, on the average, capital letters are slightly wider
than a blank, the PLCHHQ string will be longer than the
PLCHMQ string.
If SIZE in the PLCHMQ call is reduced to 8/10 of that in the
PLCHHQ call, the strings will appear more nearly the same.
The last example in this chapter shows all the PLCHMQ characters in
various aspect ratios.
.sp
.LI "\fBANGD\fR"
The angle at which the character string is to be plotted,
in degrees counterclockwise from the positive X axis.
.sp
.ne 9
.LI "\fBCNTR\fR"
The centering option.  Set the value of CNTR to a real number as follows:
.sp
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the center of the left edge of the
first character.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the center of the right edge of the
last character.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the midpoint of the line joining the
center of the left edge of the first character
to
the center of the right edge of the last character.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 81
.br
.di d+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Any real number can be used as \fIs\fR.   (XPOS,YPOS) is
a point obtained by linear
interpolation along the line joining the 
point associated with the value -1. and the point associated with the value
+1.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIs\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(d-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 824 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fIs\fR\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.d+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-29
.in 0
.ne 6
.in 0
.ft B           
.S 14
PLCHLQ
.S 12
.L2 Purpose
PLCHLQ plots characters of low quality by calling the GKS character-plotting
routines.  
.sp
Using PLCHLQ to plot a given string of characters will create a
smaller metafile than if you use PLCHHQ or PLCHMQ.  However, the
appearance of the output may not be satisfactory at your site; it
depends on capabilities of the translator.
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
.nr 38 \w\fB\s11CALL PLCHLQ\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB    Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    (XPOS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w     YPOS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w     CHRS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w     SIZE,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w     ANGD,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w     CNTR)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
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
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
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
.if t .if \n(TW>\n(.li .tm Table at line 853 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB    Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PLCHLQ\fR\s12\h'|\n(41u'    (XPOS,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'     YPOS,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'     CHRS,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'     SIZE,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'     ANGD,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'     CNTR)\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBXPOS\fR"
.LI "\fBYPOS\fR"
The X and Y coordinate positions for the characters to be plotted.
These are world coordinates.  
.LI "\fBCHRS\fR"
A string consisting of the characters to be plotted.
The
string may include any of the following 95 characters:  A-Z  a-z  0-9
!  "  #  $  %  &  \&'  (  )  *  +  -  /  :  ;  <  =  >  ?
@  `  [  \&\\  ]  ^  _   {  |  }  \&tilde  ,  .  blank
.sp
Function codes
may not be used \(em they are available in PLCHHQ only.
.sp
The number of characters in CHRS is taken to be LEN(CHRS); to use
characters \fIm\fR through \fIn\fR from a character variable CHRS
(where \fIm\fR and \fIn\fR are integer expressions), use the FORTRAN 77
substring notation CHRS(\fIm\fR:\fIn\fR).
.sp
.LI "\fBSIZE\fR"
SIZE defines a multiplier for character size, in one of three ways, as
listed below.
SIZE is defined to be more or less consistent with SIZE in PLCHHQ.  Set
the value of SIZE to a real number in one of the following ranges:
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The absolute value specifies the
size as a multiple of a 
default digitized size on a 1024x1024 grid, on
which blanks are 16 units wide.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Specifies the desired
width of a blank as a fraction of the distance across the plotter frame.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Specifies the desired width
of a blank in plotter coordinates, as defined by default or by a user
call to the SPPS routine SETI.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.<SIZE<1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGE 1.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 902 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBRange\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.<SIZE<1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GE 1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.sp
.LI "\fBANGD\fR"
The angle at which the character string is to be plotted,
in degrees counterclockwise from the positive X axis.
.sp
.ne 9
.LI "\fBCNTR\fR"
The centering option.  Set the value of CNTR to a real number as follows:
.sp
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the center of the left edge of the
first character.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the center of the right edge of the
last character.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(XPOS,YPOS) is the midpoint of the line joining the
center of the left edge of the first character
to
the center of the right edge of the last character.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 934 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne 10
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.LE
.L1 "PLOTCHAR Parameter Access Subroutines"
Internal parameters of PLOTCHAR provide you with further control over
text plotting.  These parameters reside in a labeled common block in
the package and maintain their values between calls to subroutines in
the package.  The parameters have many uses.  Some specify the actual
values of physical quantities (like character sizes).  Others act as
switches, changing the flow of execution of a routine in the package
and therefore the nature of its output.  Others, during the execution
of a routine in the package, are given values that the user program
needs to retrieve.
.sp
There are three subroutines to set the values of internal parameters
\(em one each for types character, integer, and real.  Likewise, there
are three subroutines to retrieve the current values of internal
parameters.  These six subroutines are described here.  See the
section "PLOTCHAR Internal Parameter Descriptions" for the details
of using each internal parameter.
.L2 "Setting Internal Parameter Values"
Use the following subroutines to reset the current values of internal
parameters.  Only the first two characters of the internal parameter
name are examined by these subroutines.  It is recommended that the
rest of the character string be used to improve the readability of
your code.  For example, instead of just \&'QU', use \&'QU - QUALITY
OF CHARACTERS'.
.sp
Change the value of an internal parameter before the call to
PLOTCHAR that you want to be affected.  An internal
parameter retains a value that it has been given until
another call to one of these routines changes its value
again.
.sp
The subroutine PCSETI is passed an integer value \fIi\fR.
If you are setting an integer parameter, it receives the
value \fIi\fR.  If you are setting a real parameter, it
receives the value REAL(\fIi\fR).
.sp
The subroutine PCSETR is passed a real value \fIr\fR.  If
you are setting a real parameter, it receives the value
\fIr\fR.  If you are setting an integer parameter, it
receives the value INT(\fIr\fR).  Note that the Fortran
intrinsic INT does truncation rather than rounding.
.sp
Thus, the subroutine PCSETR is the more general routine; it provides access to
all real and integer parameters.
PCSETI allows for more natural access to integer parameters
and to those real parameters whose values have no fractional part.
.sp
.ne 7
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
.nr 38 \w\fB\s11CALL PCSETC\fR\s12    
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fBArgument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w CVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
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
.if t .if \n(TW>\n(.li .tm Table at line 992 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fBArgument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PCSETC\fR\s12    \h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' CVAL)\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character 
(for example, \&'FC').  
.LI "\fBCVAL\fR"
The character value you select for the parameter.
.LE
.sp 2
.ne 7
.in 0 
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
.nr 38 \w\fB\s11CALL PCSETI\fR\s12     
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fBArgument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
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
.if t .if \n(TW>\n(.li .tm Table at line 1011 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fBArgument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PCSETI\fR\s12     \h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' IVAL)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'CD'). 
.LI "\fBIVAL\fR"
The integer value you select for the parameter.
.LE
.sp 2
.in 0 
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
.nr 38 \w\fB\s11CALL PCSETR\fR\s12    
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fBArgument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w RVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
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
.if t .if \n(TW>\n(.li .tm Table at line 1028 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fBArgument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PCSETR\fR\s12    \h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' RVAL)\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'CH').
.LI "\fBRVAL\fR"
The real value you select for the parameter.
.LE
.in 0
.L2 "Retrieving Current Internal Parameter Values"
If you need to recover the current value for
one of the internal parameters, use one of the following
calls.  Only the first two characters of the internal
parameter name are examined by these subroutines.  It is
recommended that the rest of the character string be used to
improve the readability of your code.  For example, instead
of just \&'FC', use \&'FC  - FUNCTION CODE DELIMITER'.
.sp
The subroutine PCGETI returns an integer value.  If you are getting
the value of an integer parameter, PCGETI returns that integer value.
If you are getting the value of a real parameter with value \fIr\fR,
then the value returned is INT(\fIr\fR).  Note that the Fortran
intrinsic INT does truncation rather than rounding.
.sp
The subroutine PCGETR returns a real value.  If you are
getting the value of a real parameter, PCGETR returns that
value.  If you are getting the value of an integer parameter
with value \fIi\fR, then the value returned is REAL(\fIi\fR).
.sp
Thus, the subroutine PCGETR is the more general routine; it
provides access to all real and integer parameters.  PCGETI
allows more natural access to integer parameters and to
those real parameters whose values have no fractional part.
.in 0
.sp  
.ne 8
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
.nr 38 \w\fB\s11CALL PCGETC\fR\s12   
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fBArgument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w CVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
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
.nr 38 \wOutput
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
.if t .if \n(TW>\n(.li .tm Table at line 1071 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fBArgument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PCGETC\fR\s12   \h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' CVAL)\h'|\n(42u'Character\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'FC').
.LI "\fBCVAL\fR"
A character variable in which the current value of the
parameter is to be returned.
.LE
.sp 2
.in 0
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
.nr 38 \w\fB\s11CALL PCGETI\fR\s12    
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fBArgument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
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
.nr 38 \wOutput
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
.if t .if \n(TW>\n(.li .tm Table at line 1089 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fBArgument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PCGETI\fR\s12    \h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' IVAL)\h'|\n(42u'Integer\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'CD').
.LI "\fBIVAL\fR"
An integer variable in which the current value of the
parameter is to be returned.
.LE
.sp 2
.in 0
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
.nr 38 \w\fB\s11CALL PCGETR\fR\s12   
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fBArgument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w RVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
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
.if t .if \n(TW>\n(.li .tm Table at line 1107 file man/plotchar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fBArgument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL PCGETR\fR\s12   \h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u' RVAL)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'CH').
.LI "\fBRVAL\fR"
A real variable in which the current value of the parameter
is to be returned.
.LE
.L1 "PLOTCHAR Internal Parameters \(em Overview"
Internal parameters of PLOTCHAR are variables with scopes limited to
the subroutines in the PLOTCHAR utility.
All of the PLOTCHAR internal parameters except one are for
use with PLCHHQ.
The exception is \&'HW', which is the only internal parameter used
by PLCHMQ.
No internal parameters are used by PLCHLQ.
.sp
To set or retrieve the value of an internal parameter, use a call to
the appropriate subroutine, as discussed under "PLOTCHAR Parameter Access
Subroutines," earlier in this chapter.
For example,
.sF
.>>
CALL PCSETI ('QU', 1)
.<<
.eF
sets the integer value of \&'QU' to request
medium-quality characters, and
.sF
.>>
CALL PCGETC ('FC', IFCC)
.<<
.eF
retrieves the current value of the
character parameter \&'FC', where IFCC is of type 
CHARACTER *1.
.sp
The detailed descriptions of all the internal parameters are
in alphabetical order in "PLOTCHAR Internal Parameter Descriptions," 
which immediately follows this section.
The internal parameters are arranged by function in the 
table that follows the parameter descriptions.
.sp
There are two categories of internal parameters that perform
specific, related tasks.
Together, these two groups account for 17 of the 21
internal parameters.
These two groups are described below.
.L2 "Internal Parameters to Adjust Aspect Ratio and Spacing"
Twelve of the internal parameters affect either the \fIaspect ratio\fR
or the spacing of characters.  The \fIaspect ratio\fR of a set of
characters is the ratio of the height of the tallest character in the
set to the width of the widest character in the set.  You can use the
relevant internal parameters to change the aspect ratio of various
subsets of the complex and duplex sets.  You can also change the
vertical spacing between lines of text and the superscripting and
subscripting offset for principal and indexical size characters
through the use of internal parameters.  The internal parameters in
this group are \&'CH', \&'CV', \&'CW', \&'HW', \&'IH', \&'IS', \&'IV',
\&'IW', \&'PH', \&'PS', \&'PV', and \&'PW'.
.br
.L2 "Internal Parameters That Affect Text-extent Vectors"
Five internal parameters have to do with the \fItext-extent
vectors\fR.  \fIText-extent vectors\fR are the four vectors from
(XPOS,YPOS) to the four edges of a box that would enclose a text
string and just touch the text string on all sides.  You can recover
the magnitude of the text-extent vectors to find out how large a
string plotted was, or how large it will be.  After a call to PLCHHQ,
calls to PCGETR may be used to retrieve the text-extent values for the
string defined by that call.  The internal parameters in this group
are \&'DB', \&'DL', \&'DR', \&'DT', and \&'TE'.
.L1 "PLOTCHAR Internal Parameter Descriptions" 
The internal parameter descriptions follow, in alphabetical order.
All but \&'HW' are used by PLCHHQ; PLCHMQ uses \&'HW' only; and
PLCHLQ uses no internal parameters.
.VL 1.2i
.br
.ne 20
.LI "\fB'CD'\fR"
Accesses either the complex or duplex character set.  The complex
character set has serifs and some thick and thin variation in the
strokes that make up the characters.  It is similar to a Times font,
such as is used in the text you are now reading.  The duplex character
set does not have serifs, and the strokes that form the characters are
of uniform thickness.  It is similar to a Helvetica font.  The
"PLOTCHAR Examples \(em Output" section shows all characters in both
character sets.  Only the alphanumeric characters differ in the
complex and duplex character sets; all other characters are identical.
Be aware that each switch from one character set to the other requires
reading a binary file, which increases CPU and I/O time.  Set the
value of \&'CD' to an integer as follows:
.sp
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
.rm 80 81
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSelects the complex character set.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSelects the duplex character set.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 1208 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'Selects the complex character set.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'Selects the duplex character set.
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-8
.tA "Integer" "0"
.LI "\fB'CH'\fR"
Digitized height of cartographic capital letters.
.tA "Real" "9."
.LI "\fB'CS'\fR"
Used to turn on constant spacing of characters.  Set the value of \&'CS'
to a real number in one of the following ranges:
.sp
.ne 4
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Selects constant spacing.
The absolute value specifies the spacing in digitization units.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Selects constant spacing.
The absolute value is used as a multiplier of the current width of a
blank. 
The value 1.25 gives pleasing results.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSelects variable spacing of characters.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 1235 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBRange\h'|\n(41u'Description\fR
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.\h'|\n(41u'Selects variable spacing of characters.
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'<0.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'>0.\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-17
.tA "Real" "0."  
.LI "\fB'CV'\fR"
Digitized vertical spacing of cartographic capital letters.
.tA "Real" "14."
.LI "\fB'CW'\fR"
Digitized width of cartographic capital letters.
.tA "Real" "8."
.LI "\fB'DB'\fR"
Distance to the bottom edge of the text-extent box for
the string, in the fractional coordinate system.
For retrieval only.
.tA "Real" "none"
.LI "\fB'DL'\fR"
Distance to the left edge of the text-extent box for the
string, in the fractional coordinate system.
For retrieval only.
.tA "Real" "none"
.LI "\fB'DR'\fR"
Distance to the right edge of the text-extent box for the string,
in the fractional coordinate system.
For retrieval only.
.tA "Real" "none"
.LI "\fB'DT'\fR"
Distance to the top edge of the text-extent box for the
string, in the fractional coordinate system.
For retrieval only.
.tA "Real" "none"
.LI "\fB'FC'\fR"
The function code delimiter character.
.tA "Character" ": (colon)"
.LI "\fB'HW'\fR"
Used only by PLCHMQ.  Specifies the desired ratio of the character
height (the height of a capital letter) to the character width
(excluding white space).  Negative values of \&'HW' may be used.  The
absolute value will be used as the ratio.  In addition, PLCHHQ will be
prohibited from changing \&'HW' (which it otherwise does when the
quality flag \&'QU' is set to 1, forcing it to call PLCHMQ).  The
default value of \&'HW' is 1.75, reflecting the fact that the capital
letters are digitized to be 7 units high and 4 units wide.  Therefore,
1.75 is the natural aspect ratio.
.tA "Real" "1.75"
.LI "\fB'IH'\fR"
Digitized height of indexical capital letters.
.tA "Real" "13."
.LI "\fB'IS'\fR"
Vertical offset used for subscripting and superscripting a character of
indexical size, in digitization units.
.tA "Real" "7."
.LI "\fB'IV'\fR"
Digitized vertical spacing of indexical capital letters.
.tA "Real" "20."
.LI "\fB'IW'\fR"
Digitized width of indexical capital letters.
.tA "Real" "12."
.LI "\fB'PH'\fR"
Digitized height of principal capital letters.
.tA "Real" "21."
.LI "\fB'PS'\fR"
Vertical offset used for subscripting and superscripting a character of
principal size, in digitization units.
.tA "Real" "10."
.LI "\fB'PV'\fR"
Digitized vertical spacing of principal capital letters.
.tA "Real" "32."
.LI "\fB'PW'\fR"
Digitized width of principal capital letters.
.tA "Real" "16."
.ne 15  
.LI "\fB'QU'\fR"
The quality flag allows you to force PLCHHQ to use medium-quality or
low-quality characters, by calling either the routine PLCHMQ or
PLCHLQ.  The advantage of this is that subscripting and superscripting
can still be done easily through the use of function codes, but
computing time and metafile size are reduced since lower quality
characters are plotted.  The Greek characters and the non-ASCII
symbols in the PLCHHQ set are not available.  However, function codes,
including those to change size and case and to invoke subscripting and
superscripting, are still available.  (If you're not using any
function codes, you might as well call PLCHMQ or PLCHLQ directly.)
Set the value of \&'QU' to an integer as follows:
.sp
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The high-quality characters of PLCHHQ
will be
plotted.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The medium-quality characters of PLCHMQ will
be plotted.
PLCHMQ
uses a 95-character digitized set,
which has less complicated characters than the PLCHHQ set, so
the resulting metafile will be about half as large.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The low-quality characters of PLCHLQ will be
plotted.  PLCHLQ plots characters using GKS primitives.
The resulting metafile is considerably 
.br
shortened because only characters are sent to it, instead of all the
pen moves required to plot the characters.  The disadvantage is that
the appearance of your output becomes dependent on the translator used
at your site, which may or may not have the necessary capabilities.
At NCAR, PLCHLQ is intended to produce characters as good as those
produced by PLCHMQ, but this may not be the case at your site.  
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 1349 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-32
.tA "Integer" "0"
.ne 20
.LI "\fB'TE'\fR"
Turns on text-extent computation.  Set the value of \&'TE' to an integer
in one of the following ranges:
.sp
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
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Specifies that text-extent quantities will not be computed.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Specifies that text-extent quantities will be computed.
If \&'TE' is non-zero and if ANGD is exactly 360., then no characters
are plotted by PLCHHQ.  In this case, it just computes the distances,
in the fractional coordinate system, from the point (XPOS,YPOS) to the
left edge, the right edge, the top edge, and the bottom edge of a box
enclosing the string.  These are stored as the values of the parameters
\&'DL', \&'DR', \&'DB', and \&'DT' and may be retrieved by calls to PCGETR.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE  0
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 1374 file man/plotchar.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBRange\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NE  0\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.TE    
.if \n-(b.=0 .nr c. \n(.c-\n(d.-18
.tA "Integer" "0"
.LE
.in 0
.ce
.ft B
.S 12
Functional Listing of Internal Parameters
.sp
.S 12
.ft R
This table lists all the internal parameters 
available for PLOTCHAR, grouped according to functionality.  Note that all 
but \&'HW' are specific to PLCHHQ.  
.sp 2
./"------------------------------------------------------
./"      repace tbl table with hand made one
.nf
Internal
Parameter       Type            Default         Brief Description

\'QU\'            Integer         0               Quality of characters
\'CD\'            Integer         0               Complex/duplex character sets
\'CS\'            Real            0.              Constant spacing
\'FC\'            Character       \':\'             Function-code character

Aspect Ratio and Spacing

\'CH\'            Real            9.              Cartographic height
\'CW\'            Real            8.              Cartographic width
\'CV\'            Real            14.             Cartographic vertical spacing
\'PH\'            Real            21.             Principal height
\'PW\'            Real            16.             Principal width
\'PV\'            Real            32.             Principal vertical spacing
\'PS\'            Real            10.             Principal offset used when
                                                 subscripting or superscripting
\'IH\'            Real            13.             Indexical height
\'IW\'            Real            12.             Indexical width
\'IV\'            Real            20.             Indexical vertical spacing
\'IS\'            Real            7.              Indexical offset used when
                                                 subscripting or superscripting
\'HW\'            Real            None            (PLCHMQ only) Desired ratio
                                                 of character height to
                                                 character width

Text-extent Vectors

\'TE\'            Integer         0               Text-extent computation
\'DL\'            Real            None            Distance to left edge of
                                                 text-extent box
\'DR\'            Real            None            Distance to right edge of
                                                 text-extent box
\'DB\'            Real            None            Distance to bottom edge of
                                                 text-extent box
\'DT\'            Real            None            Distance to top edge of
                                                 text-extent box
.fi

./".TS
./"tab(;);
./"l l l l
./"l l n l.
./"\fBInternal;;;
./"\fBParameter  ;Type;Default     ;Brief Description\fR
./"_
./".sp .5
./"\&'QU';Integer;0;Quality of characters
./"\&'CD';Integer;0;Complex/duplex character sets
./"\&'CS';Real;0.;Constant spacing
./"\&'FC';Character;':';Function-code character
./".sp .5
./".B
./"Aspect Ratio and Spacing
./".R
./".sp .5
./"\&'CH';Real;9.;Cartographic height
./"\&'CW';Real;8.;Cartographic width
./"\&'CV';Real;14.;Cartographic vertical spacing
./".sp .5
./"\&'PH';Real;21.;Principal height
./"\&'PW';Real;16.;Principal width
./"\&'PV';Real;32.;Principal vertical spacing
./"\&'PS';Real;10.;T{
./"Principal offset used when subscripting or superscripting
./"T}
./".sp .5
./"\&'IH';Real;13.;Indexical height
./"\&'IW';Real;12.;Indexical width
./"\&'IV';Real;20.;Indexical vertical spacing
./"\&'IS';Real;7.;T{
./"Indexical offset used when subscripting or superscripting
./"T}
./".sp .5
./"\&'HW';Real;None;T{
./"(PLCHMQ only) Desired ratio of character height
./"to character width
./"T}
./".sp .5
./".B
./"Text-extent Vectors
./".R
./".sp .5
./"\&'TE';Integer;0;Text-extent computation
./"\&'DL';Real;None;T{
./"Distance to left edge of text- extent box
./"T}
./"\&'DR';Real;None;T{
./"Distance to right edge of text-extent box
./"T}
./"\&'DB';Real;None;T{
./"Distance to bottom edge of text-extent box
./"T}
./"\&'DT';Real;None;T{
./"Distance to top edge of text- extent box
./"T}
./".sp .5
./".TE
./"------------------------------------------------------
./".bp
./"\&
./".bp
./".L1 "PLOTCHAR Examples \(em Output"
./".sp -1  
./".L2 "Example 1-1"
./".MF plotchar.examples.metacode 1 6.0
./".bp
./".L2 "Example 1-2"
./".sp3
./".MF plotchar.examples.metacode 2 6.0
./".bp
./".L2 "Example 1-3"
./".sp3
./".MF plotchar.examples.metacode 3 6.0
./".bp
./".L2 "Example 1-4"
./".sp3
./".MF plotchar.examples.metacode 4 6.0
./".bp
./".L2 "Example 1-5"
./".sp3
./".MF plotchar.examples.metacode 5 6.0
./".bp
./".L2 "Example 1-6"
./".sp3
./".MF plotchar.examples.metacode 6 6.0
