C
C	$Id: fftbkd.f,v 1.5 2008-07-27 12:23:42 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FFTBKD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA FFTBKDX
C
      include 'fntcom.h'
C
C  Data for the fontcap preprocessing for the filled fonts.
C
C  Error numbers.
C      EEOF   -- Unexpected EOF encountered.
C      ERED   -- Error reading file.
C      EINM   -- Invalid font name.
C      EORD   -- Invalid fontcap ordering.
C      EXWD   -- X coordinates too large to encode.
C      EYWD   -- Y coordinates too large to encode.
C      ESWD   -- Special value flags too large to encode.
C
      DATA EEOF,ERED,EINM,EORD,EXWD,EYWD,ESWD/1,2,3,4,5,6,7/
C
C  List all possible fontcap keywords in sequence.
C
      DATA KEYLST/'FONT_NAME           ',
     +            'CHARACTER_START     ',
     +            'CHARACTER_END       ',
     +            'FONT_RIGHT          ',
     +            'FONT_TOP            ',
     +            'FONT_CAP_OVERSHOOT  ',
     +            'FONT_CAP            ',
     +            'FONT_X-HEIGHT_OVERSH',
     +            'FONT_X-HEIGHT       ',
     +            'FONT_HALF           ',
     +            'FONT_BASE           ',
     +            'FONT_BOTTOM         ',
     +            'FONT_CAP_HORIZONTAL_',
     +            'FONT_CAP_VERTICAL_ST',
     +            'FONT_BOUNDING_BOX   ', 
     +            'FONT_EXTENDED_BOUNDI'/
C
C  Legal font names.
C
      DATA FNTNMS/'NCAR:HELVETICA                          ',
     +            'NCAR:HELVETICA-BOLD                     ',
     +            'NCAR:TIMES-ROMAN                        ',
     +            'NCAR:TIMES-BOLD                         ',
     +            'NCAR:COURIER                            ',
     +            'NCAR:COURIER-BOLD                       ',
     +            'NCAR:GREEK                              ',
     +            'NCAR:MATH-SYMBOLS                       ',
     +            'NCAR:TEXT-SYMBOLS                       ',
     +            'NCAR:WEATHER1                           ',
     +            'NCAR:WEATHER2                           ',
     +            'NCAR:TEST                               '/
C
C  Specify the Fortran logical unit number for I/O.
C
      DATA UNIT/1/
C
C  Blank line.
C
      DATA BLNKL/'                                                     
     +                          '/
C
C  Special value flags (SVNUM is the number of special value flags).
C
      DATA SVNUM/7/
      DATA COORD, ENDR, BEGINH, ENDH, BEZIER, ENDC, BEGINC, ENDL
     +    /    0,    1,      2,    3,      4,    5,      6,    7/       
C
C  Verbose flag.
C
      DATA VERBOS/0/
      END
