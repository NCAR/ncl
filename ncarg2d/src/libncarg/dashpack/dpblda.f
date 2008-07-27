C
C $Id: dpblda.f,v 1.8 2008-07-27 00:16:58 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA DPBLDAX
C
C This is the BLOCK DATA routine for DASHPACK; it provides initial
C values for all the common variables that control the behavior of
C the package.
C
C Declare the character common block.
C
        COMMON /DPCMCH/ CHDP,CHRB,CHRG,CHRS
          CHARACTER*256 CHDP
          CHARACTER*1 CHRB,CHRG,CHRS
        SAVE   /DPCMCH/
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C ANGF is the internal parameter 'SAF' (for "String Angle Flag").  When
C 'SAF' has the value zero, strings are written along a curve in the
C direction in which the curve is being drawn.  If 'SAF' is negative,
C all angles are written in the direction ABS('SAF') degrees, but this
C is done only if 'LTL' is non-zero; otherwise, the code behaves as if
C 'SAF' were zero.  When 'SAF' is greater than zero, strings are written
C along the curve, but the angle is adjusted by adding multiples of 180
C degrees so that the resulting angle lies in the range from ANGF-90 to
C ANGF+90 degrees.  (Generally, when ANGF is different from zero, it is
C either -360 or +360, which ensures that labels are as nearly upright
C as possible on the frame).  Note that, if embedded character strings
C in the dash pattern are broken (either because there are "breakpoint"
C characters in the strings or because 'SCF' is non-zero), using 'SAF'
C non-zero could give undesirable results: some characters in such a
C string might be rotated and others might not be.  Therefore, in such
C cases, a non-zero value of 'SAF' is treated as zero.
C
        DATA ANGF / 360. /
C
C CHDP is the internal parameter 'DPT' (for "Dash PaTtern"), defining
C the character dash pattern.  It is accessed by DPGETC/DPSETC.  (When
C 'DPT' is accessed by DPGETI/DPSETI, the internal variable INDP is
C meant.)  When 'DPS' is greater than or equal to zero, it says that
C CHDP specifies the dash pattern.  CHDP may contain occurrences of the
C character 'CRS' to specify a solid part of the line and the character
C 'CRG' to specify a gap.  Other characters form strings that are to be
C written along the line; within such strings, the character 'CRB' may
C be used to specify places where the string may be broken (which has
C the effect of making the characters follow the curve instead of being
C written in a straight line).
C
        DATA CHDP / '$$$$$$$$$$$$$$$$' /
C
C CHRB is the internal parameter 'CRB' (for "Character Representing
C Break") - the character that, when used in a character dash pattern,
C represents a "breakpoint" - a position at which an embedded string
C of characters may be broken.  Example: if 'DPT' has the value
C
C   '$_$_$_1|3|.|6|2|:L1:4|1|0:S:14:N:_',
C
C and other internal parameters have their default values, the dollar
C signs represent solid parts of the line and the underscores represent
C gaps; the rest of the characters form a string to be written along
C the line.  Each of the substrings '1', '3', '.', '6', '2', ':L1:4'
C (which represents the "times" sign), '1', and '0:S:14:N:' (which
C represents "zero to the power 14") will be written using a separate
C call to PLCHHQ.  The effect of this will be to make the whole label
C follow the bends of the curve, while ensuring that each call to
C PLCHHQ contains the appropriate "function codes" to make the call
C write the desired characters.
C
        DATA CHRB / '|' /
C
C CHRG is the internal parameter 'CRG' (for "Character Representing
C Gap") - the character that, when used in a character dash pattern,
C represents a gap in the line.
C
        DATA CHRG / '_' /
C
C CHRS is the internal parameter 'CRS' (for "Character Representing
C Solid")  - the character that, when used in a character dash pattern,
C represents a solid part of the line.
C
        DATA CHRS / '$' /
C
C DBPI is the internal parameter 'SSL' (for "Smoothed Segment Length"),
C which defines the distance between points interpolated along smooth
C curves.  It is given as a fraction of the distance across the plotter
C frame.
C
        DATA DBPI / .01 /
C
C EPSI is the internal parameter 'EPS' (for "EPSilon"), which specifies
C how far apart two points have to be to be considered distinct.  It is
C given as a fraction of the distance across the plotter frame.
C
        DATA EPSI / .000001 /
C
C IDPI is a flag that says whether or not quantities derived from the
C dash pattern have been computed or not.  It is initially zero and is
C set back to zero whenever anything that would affect the dash pattern
C changes.
C
        DATA IDPI / 0 /
C
C IDPS is the internal parameter 'DPS' (for "Dash Pattern Selector") -
C a dash pattern selector switch.  When IDPS is negative and equal to
C "-n", it says to use the "n" low-order bits of the current integer
C dash pattern.  When IDPS is zero, it says to use all the characters
C of the current character dash pattern.  When IDPS is positive and
C equal to "+n", it says to use the first "n" characters of the current
C character dash pattern.
C
        DATA IDPS / 0 /
C
C ILTL is the internal parameter 'LTL' (for "Lines Through Labels"),
C which may be set non-zero to cause curve lines to be drawn through
C the strings that serve as line labels.  By default, 'LTL' is zero,
C in which case each such string is written within its own gap in
C the line.
C
        DATA ILTL / 0 /
C
C INDP is the internal parameter 'DPT' (for "Dash PaTtern"), defining
C the integer dash pattern.  It is accessed by DPGETI/DPSETI.  (When
C 'DPT' is accessed by DPGETC/DPSETC, the internal variable CHDP is
C meant.)  When 'DPS' is less than zero, it says how many low-order
C bits of INDP are to be used.  Each zero bit represents a gap and each
C one bit represents a solid part of the line.
C
        DATA INDP / 65535 /
C
C IPCF is the internal parameter 'PCF' (for "PlotChar Flag"), which
C says which PLOTCHAR routine is to be called to write out character
C strings.  The value 0 says to call PLCHHQ, the value 1 says to call
C PLCHMQ, and the value 2 says to call PLCHLQ.
C
        DATA IPCF / 0 /
C
C ISBF is the internal parameter 'SBF' (for "String Buffering Flag");
C when set non-zero, it turns on a buffering mechanism for character
C strings to be written as part of a dash pattern.  Turning buffering
C on ensures that, if the end of a curve occurs anywhere within a
C contiguous group of strings, none of those strings is written;
C instead, that part of the curve is drawn using only the gap and
C solid elements of the dash pattern.  If buffering is turned off,
C there may be gaps at the ends of curves; if, in addition, 'SCF'
C is non-zero or breakpoints are specified in the embedded character
C strings, the result may be to write only part of a character string
C at the end of a curve.
C
        DATA ISBF / 1 /
C
C ISCF is the internal parameter 'SCF' (for "Single-Character Flag");
C when set non-zero, it says that character strings embedded in dash
C patterns are to be broken into single characters, each of which is
C written separately.  This has the effect of making the characters
C appear to follow the curve being drawn.  (Note, however, that it is
C not appropriate for 'SCF' to be non-zero when PLCHHQ is being used
C and an embedded character string contains function codes that are
C meaningful to that routine; in that case, one should leave 'SCF' = 0
C and use the "break character" 'CRB' to tell DASHPACK where to break
C the string.)
C
        DATA ISCF / 0 /
C
C LCDP is the internal parameter 'DPL' (for "Dash Pattern Length").
C Its value is set whenever a call to DPSETC sets 'DPT' - to the length
C of the character string that the call sets 'DPT' to - but 'DPL' may
C also be set independently by a subsequent call to DPSETI, specifying
C the use of fewer characters of the string previously given.
C
        DATA LCDP / 16 /
C
C RLS1 and RLS2 are the internal parameters 'LS1' and 'LS2' (for "Label
C Spacing 1 and 2").  The first of these specifies the widths of extra
C gaps to be placed before and after a label.  The second specifies the
C amount of additional gap space that will be provided for each label
C substring written by a single call to PLCHHQ/MQ/LQ (in addition to
C the length of the string itself).  Both are stated as multipliers of
C the current character width.
C
        DATA RLS1,RLS2 / .5 , 0. /
C
C RMFS is the internal parameter 'MFS' (for "Multiplier for First
C Solid").  If the first element of a dash pattern specifies a solid
C chunk of line, its length is multiplied by the current value of RMFS.
C This can be used to help ensure that labels along adjacent, nearly
C identical, curves (as, for example, when drawing contour lines) do
C not overlap each other.  (This feature of the old dash package is
C duplicated here for what use it may have in the new one.)
C
        DATA RMFS / 1. /
C
C TENS is the internal parameter 'TCS' (for "Tension on Cubic Splines");
C if it is set non-negative, it turns on smoothing of the lines drawn;
C the value zero says to use cubic splines; values greater than zero
C say to use splines under tension and specify what tension to use.
C Values greater than about 15 should be avoided, as they can cause
C overflows in the smoothing routines.  Note that DPSMTH always smooths;
C it treats a negative value of TENS as zero and uses cubic splines.
C
        DATA TENS / -1. /
C
C WCHR is the internal parameter 'WOC' (for "Width Of Character"), which
C specifies the width of the characters to be used in writing character
C strings, given as a fraction of the width of the plotter frame.
C
        DATA WCHR / .01 /
C
C WGAP is the internal parameter 'WOG' (for "Width of Gap"), which
C specifies the width of a gap in a dashed line, given as a fraction
C of the width of the plotter frame.
C
        DATA WGAP / .005 /
C
C WSLD is the internal parameter 'WOS' (for "Width of Solid"), which
C specifies the width of a solid in a dashed line, given as a fraction
C of the width of the plotter frame.
C
        DATA WSLD / .005 /
C
      END
