C
C	$Id: fntcom.h,v 1.5 2008-07-27 12:23:42 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C  The binary encoding for the filled fonts is stored in BUFFER.
C  The fontcap is logically separated into three segments: a section
C  giving font metrics and various technical information on the
C  subsequent encoding; a table of indices (one for each digitized
C  character) that are byte pointers into the character digitizations;
C  and the character digitizations.  The first two segments contain
C  16-bit quantities exclusively.  The last segment contains only
C  "packets" (described below).  In the initial section of the 
C  fontcap the integers are stored in a 16-bit 2's complement format.
C  This is the same format as the CGM 16-bit integer at 16-bit
C  precision.
C
C  Here is a precise detailing of the encoding:
C
C  Byte #   Contents            Notes
C  -------  --------            -----
C    0-  1  TYPE_FLAG           This 16-bit quantity is used to flag
C                               whether the fontcap if for font 
C                               outlines or for strokes.  If the
C                               quantity is all 1's, then the fontcap
C                               is for outlines and the documentation
C                               here pertains.  Otherwise the fontcap
C                               is for strokes - the details on those
C                               fontcaps is in file cfaamn.f.sed.
C    2- 41  FONT_NAME           An ASCII string (blank filled to the
C                               right) specifying the font name.
C   42- 43  CHARACTER_START     An ASCII decimal equivalent specifying
C                               the beginning character in the ASCII
C                               collating sequence.
C   44- 45  CHARACTER_END       An ASCII decimal equivalnet specifying
C                               the end character in the digitizations
C                               below.  Any character not digitized
C                               should be a blank.
C   46- 47  FONT_RIGHT          The value from the ASCII fontcap.
C   48- 49  FONT_TOP            The value from the ASCII fontcap.
C   50- 51  FONT_CAP_OVERSHOOT  The value from the ASCII fontcap.
C   52- 53  FONT_CAP            The value from the ASCII fontcap.
C   54- 55  FONT_X_HEIGHT_OVERSHOOT  The value from the ASCII fontcap.
C   56- 57  FONT_X_HEIGHT       The value from the ASCII fontcap.
C   58- 59  FONT_HALF           The value from the ASCII fontcap.
C   60- 61  FONT_BASE           The value from the ASCII fontcap.
C   62- 63  FONT_BOTTOM         The value from the ASCII fontcap.
C   64- 65  FONT_CAP_HORIZONTAL_STEM_WIDTH  The value from the ASCII fontcap.
C   66- 67  FONT_CAP_VERTICAL_STEM_WIDTH    The value from the ASCII fontcap.
C   68- 69  FONT_LLX            The lower left X coordinate of the font
C                               bounding box (in the font coordinate system).
C   70- 71  FONT_LLY            The lower left Y coordinate of the font
C                               bounding box (in the font coordinate system).
C   72- 73  FONT_URX            The upper right X coordinate of the font
C                               bounding box (in the font coordinate system).
C   74- 75  FONT_URY            The upper right Y coordinate of the font
C                               bounding box (in the font coordinate system).
C   76- 77  TABLE_POINTER       A byte address for the start of the table
C                               of indices that point into the digitizations
C                               of the characters.  This address should
C                               fall on a word boundary.  The address 
C                               start at byte 0 in this file.
C   78- 79  X_BIT_WIDTH         The number of bits that are required
C                               to store each X coordinate in the
C                               digitizations.  This will normally be
C                               the maximum number needed to accommodate
C                               the largest X coordinate.
C   80- 81  Y_BIT_WIDTH         The number of bits that are required
C                               to store each Y coordinate in the
C                               digitizations.  This will normally be
C                               the maximum number needed to accommodate
C                               the largest Y coordinate.
C   82- 83  X_BIAS              The bias for the X coordinates in the
C                               digitizations.  The bias will be 
C                               subtracted off of each X coordinate 
C                               to get the correct X value in the font
C                               coordinate system.
C   84- 85  Y_BIAS              The bias for the Y coordinates in the
C                               digitizations.  The bias will be 
C                               subtracted off of each XYcoordinate 
C                               to get the correct Y value in the font
C                               coordinate system.
C   86- 87  PACKET_FLAG_WIDTH   Each coordinate pair in the digitizations
C                               is encoded as a packet that consists of
C                               three things: some bits of special
C                               information, a biased X coordinate and
C                               a biased Y coordinate.  The value for
C                               PACKET_FLAG_WIDTH specifies how many
C                               bits at the beginning of a coordinate
C                               packet are reserved for the special
C                               information (see the description of the
C                               character digitizations below for details).
C  88- 89  LAST_POINTER         The byte address of the final byte of 
C                               final character.
C NT1-NT2  POINTER_TABLE        The value for NT1 is given by TABLE_POINTER
C                               above.  NT2 will equal 
C                               NT1+2*(CHARACTER_END-CHARACTER_START+1)-1,
C                               i.e. there is one 16-bit quantity for
C                               each digitized character.  This is the
C                               byte address where the digitization for
C                               given character starts.  Each such address
C                               should fall on a word boundary.
C ST1-end  OUTLINES             This section contains the information for 
C                               the font outlines.  The information for
C                               each character begins on a word boundary
C                               that is specified in the POINTER_TABLE
C                               above.  To understand the OUTLINES see:
C
C    http://ncarg.ucar.edu/supplements/cgm_supp/cgm_supp.html#HDR19
C
C                               Each character in the OUTLINES section
C                               consists of a sequence of packets as
C                               described above.  The values contained 
C                               in the special information bits of each
C                               packet are:
C
C                                 0 -- The X and Y values are coordinates.
C                                 1 -- End the current region (this implies
C                                      beginning a new region unless the
C                                      character is complete).  The X and
C                                      Y values are not significant.
C                                 2 -- Begin the definition of a hole.
C                                      The X and Y values are not significant.
C                                 3 -- End the difinition of a hole.  The
C                                      X and Y values are not significant.
C                                 4 -- Flags that the next three packets
C                                      (together with the most recent point)
C                                      contain the four Bezier control
C                                      points (in the font coordinate
C                                      system) for a curve.
C                                 5 -- End of outline definitions for this
C                                      character.  The X and Y values are
C                                      not significant.
C                                 6 -- Begin the outlines definitions for
C                                      a character.  The X value of the
C                                      packet is the ASCII decimal equivalent
C                                      and the Y value contains the character
C                                      width (in the font coordinate system).
C                                      These are unbiased numbers.
C
      PARAMETER (IBDIM=15000)
      COMMON /FCAP/BUFFER(IBDIM),BSIZE
C
      COMMON /ERNO/EEOF,ERED,EINM,EORD,EXWD,EYWD,ESWD
      INTEGER      EEOF,ERED,EINM,EORD,EXWD,EYWD,ESWD
C
      COMMON /KVALC/FNAME
      CHARACTER*40 FNAME
C
      PARAMETER (NUMKYS=16)
      COMMON /KEYS/KEYLST
      CHARACTER*20 KEYLST(NUMKYS)
C
      PARAMETER (NUMFTS=12)
      COMMON /FONTNS/FNTNMS
      CHARACTER*40 FNTNMS(NUMFTS)
C
      COMMON /CLINE/LINE
      CHARACTER*80 LINE
C
      COMMON /FNTIO/UNIT  , BLNKL , NBPERI, NBYPWD, PKWID , BITPNT,
     +              VERBOS, LSTPNT, NWRDS , LSIZE
      INTEGER       UNIT  , NBPERI, NBYPWD, PKWID , BITPNT, VERBOS,
     +              LSTPNT, NWRDS
      CHARACTER*80 BLNKL
C
      PARAMETER (NUMNTR=28)
      COMMON /INTRNL/TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV, 
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  , 
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  , 
     +               FLLEX , FLLEY , FUREX , FUREY ,
     +               TABPNT, XBITWD, YBITWD, XBIAS , YBIAS , PKFLWD
      INTEGER        TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV, 
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  , 
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  , 
     +               FLLEX , FLLEY , FUREX , FUREY ,
     +               TABPNT, XBITWD, YBITWD, XBIAS , YBIAS , PKFLWD 
      INTEGER INTARR(NUMNTR)
      EQUIVALENCE (INTARR,TYPFLG)
C
      COMMON /SFLAGS/SVNUM ,
     +               COORD , ENDR  , BEGINH, ENDH  , BEZIER, ENDC  ,
     +               BEGINC, ENDL
      INTEGER        SVNUM ,
     +               COORD , ENDR  , BEGINH, ENDH  , BEZIER, ENDC  ,
     +               BEGINC, ENDL
