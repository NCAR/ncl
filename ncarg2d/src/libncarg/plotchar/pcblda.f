C
C $Id: pcblda.f,v 1.17 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA PCBLDAX
C
C Specify default values of internal parameters of PLCHHQ, PLCHMQ, and
C the routine PCCFFC, which is called to retrieve a character from a
C fontcap.
C
C COMMON block declarations.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),IBNU,
     +                IBXC(3),IBXF,ICEN,IORD,IOUC,IOUF,IPCC,IQUF,
     +                ISHC,ISHF,ITEF,JCOD,LSCI(16),NFCC,NODF,RBXL,
     +                RBXM,RBXX,RBXY,ROLW,RPLW,RSLW,SHDX,SHDY,SIZA,
     +                SSIC,SSPR,SUBS,VPIC(3),WPIC(3),XBEG,XCEN,XEND,
     +                XMUL(3),YBEG,YCEN,YEND,YMUL(3),ZINX,ZINY,ZINZ
      SAVE   /PCPRMS/
C
      COMMON /PCSVEM/ ICOD,IDDA(8625),IDDL,RDGU(7000),IDPC(256),IERU,
     +                INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
      COMMON /PCPFLQ/ IMAP,OORV,RHTW
      SAVE   /PCPFLQ/
C
      COMMON /PCFNNO/ LFNO(43),LFNL
      SAVE   /PCFNNO/
C
      COMMON /PCFNNM/ LFNM(43,2)
      CHARACTER*18 LFNM
      SAVE   /PCFNNM/
C
      COMMON /PCSTCM/ XVPL,XVPR,YVPB,YVPT
      SAVE   /PCSTCM/
C
      COMMON /PCMP04/ PANG,PLAT,PLON
      SAVE   /PCMP04/
C
C Define the add-space flag, which allows the user to specify
C additional spacing between characters along the line.
C
      DATA ADDS / 0. /
C
C Define the constant-spacing flag, which allows the user to position
C characters a constant distance apart along the line.
C
      DATA CONS / 0. /
C
C Define the height of characters of the various sizes (on a 1024x1024
C grid).
C
      DATA HPIC / 21.,13., 9. /
C
C Define the number of the unit from which the binary file of data may
C be read.
C
      DATA IBNU / 3 /
C
C IBXC is an array of color indices to be used for various parts of a
C box drawn around a character string.  Element 1 is for the outline of
C the box, element 2 for fill of the box, and element 3 for fill of the
C box shadow.
C
      DATA IBXC / 3*-1 /
C
C IBXF is a box flag, set non-zero to cause to be drawn various parts
C of a box around a character string.  It contains three bits, one of
C which controls filling of the shadow, one of which controls filling
C of the box itself, and one of which controls drawing of the outline.
C
      DATA IBXF / 0 /
C
C ICEN is the internal parameter 'CE'; it determines the type of
C centering to be done by PLCHHQ.  When ICEN is zero, centering is
C controlled by the final argument of PLCHHQ, CNTR.  When ICEN is
C non-zero, the value of CNTR is ignored and the string written is
C centered exactly on the point (XPOS,YPOS).  This will mostly be
C useful when when writing a single character which is intended to
C mark a particular point.
C
      DATA ICEN / 0 /
C
C IMAP is the internal parameter 'MA'; it says how X/Y coordinates
C are to be mapped.
C
      DATA IMAP / 0 /
C
C INIT is an initialization flag for PLCHHQ.
C
      DATA INIT / 0 /
C
C IORD is a flag that says in what order characters are to be drawn by
C PLCHHQ.  IORD is the value of the parameter 'DO'. The parameter
C setting for 'DO' can be used in combination with the setting for
C the 'SS' parameter to achieve stacking effects.   See frame 8
C of the ncargex example 'epltch' to observe these effects.
C If its value is positive, characters are to be drawn in the
C order in which they occur in an input string (from left to right);
C otherwise, they are to be drawn in the reverse order (from right to
C left).  The absolute value of IORD should be either a 1 or a 2; the
C value 1 says that PLCHHQ should draw shadows for all the characters,
C then principal bodies for all the characters and then outlines for all
C the characters.  The value 2 says that it should draw the shadow, the
C principal body, and the outline for character 1, then the shadow, the
C principal body, and the outline for character 2, and so on.  Note
C that, if the input string defines more than NCSO characters, they are
C dealt with in batches of NCSO at a time; for some values of IORD, this
c is a problem.
C
      DATA IORD / 1 /
C
C IOUC is the outline color index, to be used only when IOUF is non-
C zero.
C
      DATA IOUC / 1 /
C
C IOUF is the "outline" flag.  When it is set non-zero, characters
C from the filled fonts are outlined in a contrasting color.
C
      DATA IOUF / 0 /
C
C IPCC is the principal character color.  When it is set less than 0,
C no setting of color indices takes place before drawing a character.
C Otherwise, it specifies the principal color index to use.
C
      DATA IPCC / -1 /
C
C IQUF is the "quality" flag.  When it is zero, high-quality characters
C are used.  Otherwise, lower-quality characters are produced by calling
C PCMQLQ, which in turn calls either PLCHMQ (if IQUF = 1) or PLCHLQ (if
C IQUF = 2).
C
      DATA IQUF / 0 /
C
C ISHC is the shadow color index.  When ISHF is set greater than or
C equal to zero, characters drawn by PLCHHQ and PLCHMQ have shadows
C drawn in the color specified by ISHC.
C
      DATA ISHC / 0 /
C
C ISHF is the "shadow" flag.  When it is set non-zero, characters drawn
C by PLCHHQ and PLCHMQ are "shadowed" by first being drawn, with an
C an offset of SHDX and SHDY, in the color specified by ISHC.
C
      DATA ISHF / 0 /
C
C ITEF is the "compute-text-extent-vectors" flag.  When it is set, calls
C to PLCHHQ with ANGD = 360. plot nothing; they cause the parameters
C DSTL, DSTR, DSTB, and DSTT (the magnitudes of the text extent vectors)
C to be computed for recovery by the calling program.
C
      DATA ITEF / 0 /
C
C The first time a given dataset is read, it is checked for correctness.
C The flags IVCO and IVDU are used to prevent this from being done
C thereafter.
C
      DATA IVCO,IVDU / 0,0 /
C
C JCOD may be set to 0 to request use of the complex dataset or to 1 to
C request use of the duplex dataset.  Its value is compared with the
C value of ICOD every time PLCHHQ is called, and, if the two differ,
C the dataset is reloaded.
C
      DATA JCOD / 0 /
C
C The list of font numbers, LFNO, and the list of font names, LFNM, each
C of which is of length LFNL, are used by PCGETC and PCSETC.
C
      DATA LFNL / 43 /
C
      DATA LFNO( 1),LFNM( 1,1),LFNM( 1,2) /   0 , 'PWRITX DATABASE   ' ,
     +                                            'pwritx database   ' /
      DATA LFNO( 2),LFNM( 2,1),LFNM( 2,2) /   1 , 'DEFAULT           ' ,
     +                                            'default           ' /
      DATA LFNO( 3),LFNM( 3,1),LFNM( 3,2) /   2 , 'CARTOGRAPHIC_ROMAN' ,
     +                                            'cartographic_roman' /
      DATA LFNO( 4),LFNM( 4,1),LFNM( 4,2) /   3 , 'CARTOGRAPHIC_GREEK' ,
     +                                            'cartographic_greek' /
      DATA LFNO( 5),LFNM( 5,1),LFNM( 5,2) /   4 , 'SIMPLEX_ROMAN     ' ,
     +                                            'simplex_roman     ' /
      DATA LFNO( 6),LFNM( 6,1),LFNM( 6,2) /   5 , 'SIMPLEX_GREEK     ' ,
     +                                            'simplex_greek     ' /
      DATA LFNO( 7),LFNM( 7,1),LFNM( 7,2) /   6 , 'SIMPLEX_SCRIPT    ' ,
     +                                            'simplex_script    ' /
      DATA LFNO( 8),LFNM( 8,1),LFNM( 8,2) /   7 , 'COMPLEX_ROMAN     ' ,
     +                                            'complex_roman     ' /
      DATA LFNO( 9),LFNM( 9,1),LFNM( 9,2) /   8 , 'COMPLEX_GREEK     ' ,
     +                                            'complex_greek     ' /
      DATA LFNO(10),LFNM(10,1),LFNM(10,2) /   9 , 'COMPLEX_SCRIPT    ' ,
     +                                            'complex_script    ' /
      DATA LFNO(11),LFNM(11,1),LFNM(11,2) /  10 , 'COMPLEX_ITALIC    ' ,
     +                                            'complex_italic    ' /
      DATA LFNO(12),LFNM(12,1),LFNM(12,2) /  11 , 'COMPLEX_CYRILLIC  ' ,
     +                                            'complex_cyrillic  ' /
      DATA LFNO(13),LFNM(13,1),LFNM(13,2) /  12 , 'DUPLEX_ROMAN      ' ,
     +                                            'duplex_roman      ' /
      DATA LFNO(14),LFNM(14,1),LFNM(14,2) /  13 , 'TRIPLEX_ROMAN     ' ,
     +                                            'triplex_roman     ' /
      DATA LFNO(15),LFNM(15,1),LFNM(15,2) /  14 , 'TRIPLEX_ITALIC    ' ,
     +                                            'triplex_italic    ' /
      DATA LFNO(16),LFNM(16,1),LFNM(16,2) /  15 , 'GOTHIC_GERMAN     ' ,
     +                                            'gothic_german     ' /
      DATA LFNO(17),LFNM(17,1),LFNM(17,2) /  16 , 'GOTHIC_ENGLISH    ' ,
     +                                            'gothic_english    ' /
      DATA LFNO(18),LFNM(18,1),LFNM(18,2) /  17 , 'GOTHIC_ITALIAN    ' ,
     +                                            'gothic_italian    ' /
      DATA LFNO(19),LFNM(19,1),LFNM(19,2) /  18 , 'MATH_SYMBOLS      ' ,
     +                                            'math_symbols      ' /
      DATA LFNO(20),LFNM(20,1),LFNM(20,2) /  19 , 'SYMBOL_SET1       ' ,
     +                                            'symbol_set1       ' /
      DATA LFNO(21),LFNM(21,1),LFNM(21,2) /  20 , 'SYMBOL_SET2       ' ,
     +                                            'symbol_set2       ' /
      DATA LFNO(22),LFNM(22,1),LFNM(22,2) /  21 , 'HELVETICA         ' ,
     +                                            'helvetica         ' /
      DATA LFNO(23),LFNM(23,1),LFNM(23,2) /  22 , 'HELVETICA-BOLD    ' ,
     +                                            'helvetica-bold    ' /
      DATA LFNO(24),LFNM(24,1),LFNM(24,2) /  25 , 'TIMES-ROMAN       ' ,
     +                                            'times-roman       ' /
      DATA LFNO(25),LFNM(25,1),LFNM(25,2) /  26 , 'TIMES-BOLD        ' ,
     +                                            'times-bold        ' /
      DATA LFNO(26),LFNM(26,1),LFNM(26,2) /  29 , 'COURIER           ' ,
     +                                            'courier           ' /
      DATA LFNO(27),LFNM(27,1),LFNM(27,2) /  30 , 'COURIER-BOLD      ' ,
     +                                            'courier-bold      ' /
      DATA LFNO(28),LFNM(28,1),LFNM(28,2) /  33 , 'GREEK             ' ,
     +                                            'greek             ' /
      DATA LFNO(29),LFNM(29,1),LFNM(29,2) /  34 , 'MATH-SYMBOLS      ' ,
     +                                            'math-symbols      ' /
      DATA LFNO(30),LFNM(30,1),LFNM(30,2) /  35 , 'TEXT-SYMBOLS      ' ,
     +                                            'text-symbols      ' /
      DATA LFNO(31),LFNM(31,1),LFNM(31,2) /  36 , 'WEATHER1          ' ,
     +                                            'weather1          ' /
      DATA LFNO(32),LFNM(32,1),LFNM(32,2) /  37 , 'WEATHER2          ' ,
     +                                            'weather2          ' /
      DATA LFNO(33),LFNM(33,1),LFNM(33,2) / 121 , 'O_HELVETICA       ' ,
     +                                            'o_helvetica       ' /
      DATA LFNO(34),LFNM(34,1),LFNM(34,2) / 122 , 'O_HELVETICA-BOLD  ' ,
     +                                            'o_helvetica-bold  ' /
      DATA LFNO(35),LFNM(35,1),LFNM(35,2) / 125 , 'O_TIMES-ROMAN     ' ,
     +                                            'o_times-roman     ' /
      DATA LFNO(36),LFNM(36,1),LFNM(36,2) / 126 , 'O_TIMES-BOLD      ' ,
     +                                            'o_times-bold      ' /
      DATA LFNO(37),LFNM(37,1),LFNM(37,2) / 129 , 'O_COURIER         ' ,
     +                                            'o_courier         ' /
      DATA LFNO(38),LFNM(38,1),LFNM(38,2) / 130 , 'O_COURIER-BOLD    ' ,
     +                                            'o_courier-bold    ' /
      DATA LFNO(39),LFNM(39,1),LFNM(39,2) / 133 , 'O_GREEK           ' ,
     +                                            'o_greek           ' /
      DATA LFNO(40),LFNM(40,1),LFNM(40,2) / 134 , 'O_MATH-SYMBOLS    ' ,
     +                                            'o_math-symbols    ' /
      DATA LFNO(41),LFNM(41,1),LFNM(41,2) / 135 , 'O_TEXT-SYMBOLS    ' ,
     +                                            'o_text-symbols    ' /
      DATA LFNO(42),LFNM(42,1),LFNM(42,2) / 136 , 'O_WEATHER1        ' ,
     +                                            'o_weather1        ' /
      DATA LFNO(43),LFNM(43,1),LFNM(43,2) / 137 , 'O_WEATHER2        ' ,
     +                                            'o_weather2        ' /
C
C LSCI is a list of special color indices.  The definitions of some
C of the characters in the filled fonts include the specification of
C particular colors to be used.  If one of these definitions says to
C use color number I, what will actually be used is the color whose
C color index is specified by LSCI(I).  By default, all elements of
C LSCI have the value "-1", which is equivalent to "undefined".  A
C user call to the routine PCDLSC ("PlotChar, Define List of Special
C Colors") will define a user-specified set of color indices to have
C the proper colors and then set the contents of LSCI appropriately.
C Alternatively, the user may define his/her own set of color indices
C and set the values of LSCI elements as he/she wishes.
C
      DATA LSCI / 16*-1 /
C
C NFCC is the position of the function code character in the collating
C sequence - the default, a colon, is set during initialization.
C
      DATA NFCC / -1 /
C
C NODF, if non-zero, selects one of the fonts defined by fontcaps,
C characters from which are used in place of the "built-in" high
C quality characters.  One may change font in the middle of a string,
C using the "function code" F, followed by the number of the desired
C font.
C
      DATA NODF / 0 /
C
C OORV, if non-zero, is the out-of-range value that is returned by
C PCMPXY to indicate that the input X and Y coordinates are not
C mappable.
C
      DATA OORV / 0. /
C
C Define default values of the quantities that are used by PCMPXY
C when 'MAP' = 4 (to prevent problems if the user forgets to set
C them).
C
      DATA PANG,PLAT,PLON / 3*0. /
C
C RBXL, if greater than zero, is the line width to be used while drawing
C the various parts of a box around a character string.  A value less
C than or equal to zero indicates that line width is to be unchanged
C while drawing the box.
C
      DATA RBXL / 0. /
C
C RBXM gives the width of the margin of a box around a character string,
C as a fraction of the character height.
C
      DATA RBXM / .15 /
C
C RBXX and RBXY give the X and Y offsets for the box shadow; both are
C given as fractions of the principal character height.
C
      DATA RBXX,RBXY / -.05,-.05 /
C
C Define the default value of the ratio of character height to width.
C
      DATA RHTW / 1.75 /
C
C ROLW, if non-zero, is the desired outline line width.
C
      DATA ROLW / 0. /
C
C RPLW, if non-zero, is the desired principal line width.
C
      DATA RPLW / 0. /
C
C RSLW, if non-zero, is the desired shadow line width.
C
      DATA RSLW / 0. /
C
C SIZA is the internal parameter 'SA', which serves as a multiplier
C for the sizes of the characters written by PLCHHQ.  Its default
C value is (16/21)*(7/6)=(112/126)=.888888888888888, which serves
C to make the characters written by a call to PLCHHQ more nearly
C the same size as those written by a call to PLCHMQ.  (They are
C exactly the same height, but not necessarily of the same width.)
C This parameter is being introduced in October, 1992, for inclusion
C in release 3.2 of NCAR Graphics.  Those users who want PLCHHQ to
C produce the same size characters it used to should set 'SA' to 1.
C
      DATA SIZA / .888888888888888 /
C
C Define the subtract-space flag, which allows the user to specify
C reduced spacing between characters along the line.
C
      DATA SUBS / 0. /
C
C Define the extent of the shifts for super- or subscripting (in terms
C of plotter address units on a 1024x1024 grid).
C
      DATA SSIC,SSPR / 7.,10. /
C
C SHDX and SHDY specify the X and Y offsets to be used in drawing
C character shadows.  These are stated as fractions of the font height.
C
      DATA SHDX,SHDY / -.05,-.05 /
C
C Define the vertical spacing for characters of the various sizes (on
C a 1024x1024 grid).
C
      DATA VPIC / 32.,20.,14. /
C
C Define the width of characters of the various sizes (on a 1024x1024
C grid).
C
      DATA WPIC / 16.,12., 8. /
C
C Define default values for x/y positioning information retrievable by
C the user, just in case.
C
      DATA XBEG,XCEN,XEND / 0.,0.,0. /
      DATA YBEG,YCEN,YEND / 0.,0.,0. /
C
C Define multipliers for the x and y components of the digitized
C characters to make them come out the right size.
C
      DATA XMUL / 1.,1.,1. /
      DATA YMUL / 1.,1.,1. /
C
C Define dummy values for the viewport-edge parameters that will be
C passed by STITLE to PCMPXY.  This just keeps the code from blowing
C up in certain error situations.
C
      DATA XVPL,XVPR,YVPB,YVPT / 4*0. /
C
C ZINX, ZINY, and ZINZ are default zoom factors (internal parameters
C 'ZX', 'ZY', and 'ZZ', respectively).
C
      DATA ZINX,ZINY,ZINZ / 3*1. /
C
      END
