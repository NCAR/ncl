C
C	$Id: capchg.f.sed,v 1.3 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      PROGRAM CAPCHG
C
C  This program converts the ASCII graphcap files into binary
C  graphcap files which are readable by the CGM translator.
C  Execution of this program will create a request for the
C  name of the ASCII graphcap file and a request for the name
C  of the binary output file.  If the program is executed in
C  a batch environment, appropriate changes must be made to
C  ensure the proper file names are supplied and OPENed.
C
C  This program contains the following labeled common blocks:
C
C    CAPASC
C    CAPASI
C    CAPBND
C    CAPCOL
C    CAPDEV
C    CAPERR
C    CAPIO2
C    CAPIOB
C    CAPLIN
C    CAPMAR
C    CAPPLG
C    CAPSCN
C    CAPSKP
C    CAPSPC
C    CAPTXT
C    CAPUSR
C    PARTB1
C    PARTB2
C
C  We now describe the variables in each of these commons.
C  After each variable name there is a pair of values in
C  brackets; the first value indicates the type of the
C  common variable, and the second value indicates its
C  default initial value.  The possibilities for the first
C  entry in the bracketed pair are:
C
C      I -- Integer
C     IA -- Integer array
C      R -- Real
C     RA -- Real array
C      C -- Character
C     CA -- Character array
C      L -- Logical
C
C    CAPASC
C    ------
C      ASCTB1 [CA, - ]  Table containing all 3-character sequences
C                       which have special meaning to the translator.
C                       This includes the special ASCII characters
C                       with ADE values between 0 and 31 (decimal).
C      ASCTB2 [CA, - ]  Table of all ASCII characters with ADE values
C                       between 33 and 126 (decimal).
C
C    CAPASI
C    ------
C      ASCVL1 [IA, - ]  A table containing decimal equivalents to the
C                       characters in ASCTB1.  Special negative values
C                       are assigned to the strings which are significant
C                       to the translator, but are not part of the ASCII
C                       character set.
C      ASCVL2 [IA, - ]  A table containing decimal equivalents for the
C                       ASCII characters in table ASCTB2.
C
C
C
C    CAPBND (Common for the bundle tables.)
C    ------
C      PLBTEC [ I, -1]  Number of POLYLINE bundle tables.
C      PLIBTB [IA, - ]  Array of indices for POLYLINE bundle tables.
C      PLTBTB [IA, - ]  Array of POLYLINE LINETYPE bundle table entries.
C      PLWBTB [RA, - ]  Array of POLYLINE LINEWIDTH bundle table entries.
C      PLCBTB [IA, - ]  Array of POLYLINE COLOR INDEX bundle table entries.
C      PMBTEC [ I, -1]  Number of POLYMARKER bundle tables.
C      PMIBTB [IA, - ]  Array of indices for POLYMARKER bundle tables.
C      PMTBTB [IA, - ]  Array of POLYMARKER MARKERTYPE bundle table entries.
C      PMSBTB [RA, - ]  Array of POLYMARKER MARKERSIZE bundle table entries.
C      PMCBTB [IA, - ]  Array of POLYMARKER COLOR INDEX bundle tbl. entries.
C      TXBTEC [ I, -1]  Number of TEXT bundle tables.
C      TXIBTB [IA, - ]  Array of indices for TEXT bundle tables.
C      TXFBTB [IA, - ]  Array of TEXT FONT bundle table entries.
C      TXPBTB [IA, - ]  Array of TEXT PRECISION bundle table entries.
C      TCXBTB [RA, - ]  Array of CHARACTER EXPANSION FACTOR table entries.
C      TCSBTB [RA, - ]  Array of CHARACTER SPACING bundle table entries.
C      TXCBTB [IA, - ]  Array of TEXT COLOR INDEX bundle table entries.
C      FABTEC [ I, -1]  Number of FILL AREA bundle tables.
C      FAIBTB [IA, - ]  Array of indices for FILL AREA bundle tables.
C      FISBTB [IA, - ]  Array of FILL AREA INTERIOR STYLE bundle table entries.
C      FASBTB [IA, - ]  Array of FILL AREA STYLE INDEX bundle table entries.
C      FACBTB [IA, - ]  Array of FILL AREA COLOR INDEX bundle table entries.
C
C
C    CAPCOL (Contains device color information.)
C    ------
C      COLINT [IA,  0]  Contains the DEVICE_MAP_INIT table of color intensities
C                       (COLINT is dimensioned for 3*(max number of colors)).
C      COLIDX [IA, - ]  Array of indices associated with the table of color
C                       intensities defined in COLINT above.
C      IDXCUR [ I,  0]  DEVICE_MAP_INDEX_RANGE_DEFINED
C      VDMINT [ -, - ]  Unused.
C      DMPAVL [ L,.FALSE.] DEVICE_MAP_AVAILABLE
C      COLFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       COLMAX x 4) containing the DEVICE_COLOR_INDEX_FORMAT
C      COLFIN [IA,  0]  Array dimensioned for 8 describing the
C                       color indices as follows:
C                       COLFIN(1) -- DEVICE_COLOR_INDEX_ENCODING
C                       COLFIN(2) -- Number of 4-entry lines defined
C                                    in COLFMT.
C                       COLFIN(3) -- Maximum number of lines allowed
C                                    in COLFMT.
C                       COLFIN(4) -- Number of entries in each
C                                    line of COLFMT.
C
C                       If COLFIN(1) is not equal to 5, then
C                       COLFIN(5) is IFIX(ALOG10(FLOAT(IDXMAX+1))+1).
C
C                       The INTEGER array COLFIN is equivalenced to
C                       the REAL array COLRIN, and
C                       if COLFIN(1) equals 5, then
C                       COLRIN(5) -- Minimum data value input to encoder
C                       COLRIN(6) -- Maximum data value input to encoder
C                       COLRIN(7) -- Minimum data value output from
C                                    encoder.
C                       COLRIN(8) -- Maximum data value output from
C                                    encoder.
C                       obtained from DEVICE_COLOR_INDEX_FLOATING_INFO
C      IDXMAX [ I, -1]  DEVICE_MAP_INDEX_RANGE_MAX
C      MSTSTR [IA,  0]  Contains the string DEVICE_MAP_INSTRUCTION_START
C      MSTSIZ [ I,  0]  Number of characters in MSTSTR.
C      MTRSTR [IA,  0]  Contains the string DEVICE_MAP_INSTRUCTION_TERMINATOR
C      DMPMDL [ I,  1]  DEVICE_MAP_MODEL
C      MTRSIZ [ I,  0]  Number of characters in MTRSTR.
C      DMPIDV [ L,.FALSE.]  DEVICE_MAP_INDIVIDUAL
C      DMPFIN [IA,  0]  Array dimensioned for 8 describing the
C                       device map intensities as follows:
C                       DMPFIN(1) -- DEVICE_MAP_INTENSITY_ENCODING
C                       DMPFIN(2) -- Number of 4-entry lines defined
C                                    in DMPFMT.
C                       DMPFIN(3) -- Maximum number of lines allowed
C                                    in DMPFMT.
C                       DMPFIN(4) -- Number of entries in each
C                                    line of DMPFMT.
C
C                       If DMPFIN(1) equals 5, then
C                       DMPFIN(5) is IFIX(ALOG10(FLOAT(INTMAX+1))+1),
C                       where INTMAX is the maximum value in the
C                       COLINT array.
C
C                       The INTEGER array DMPFIN is equivalenced to
C                       the REAL array DMPRIN, and
C                       if DMPFIN(1) equals 5, then
C                       DMPRIN(5) -- Minimum data value input to encoder
C                       DMPRIN(6) -- Maximum data value input to encoder
C                       DMPRIN(7) -- Minimum data value output from
C                                    encoder.
C                       DMPRIN(8) -- Maximum data value output from
C                                    encoder.
C                       as obtained from
C                       DEVICE_MAP_INTENSITY_FLOATING_INFO
C      DMPFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       DMPMAX x 4) containing the DEVICE_MAP_INTENSITY_FORMAT
C
C
C    CAPDEV (Contains all device class data.)
C    ------
C      DGISTR [IA, - ]  Array containing the DEVICE_GRAPHIC_INIT string.
C      DGISIZ [ I, - ]  Number of defined elements in DGISTR.
C      DGESTR [IA, - ]  Array containing the DEVICE_ERASE string.
C      DGESIZ [ I, - ]  Number of defined elements in DGESIZ.
C      DTISTR [IA, - ]  Array containing the DEVICE_TEXT_INIT string.
C      DTISIZ [ I, - ]  Number of defined elements in DTISTR.
C      DCDLLX [ I,  0]  DEVICE_COORD_LOWER_LEFT_X
C      DCDLLY [ I,  0]  DEVICE_COORD_LOWER_LEFT_Y
C      DCDURX [ I,  0]  DEVICE_COORD_UPPER_RIGHT_X
C      DCDURY [ I,  0]  DEVICE_COORD_UPPER_RIGHT_Y
C      DCOAVL [ L,.FALSE.]  DEVICE_COLOR_AVAILABLE
C      CORFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       DCFTMX x 4) containing the DEVICE_COORD_FORMAT.
C      CORFIN [IA,  0]  Array dimensioned for 8 describing the
C                       device coordinates as follows:
C                       CORFIN(1) -- DEVICE_COORD_ENCODING
C                       CORFIN(2) -- Number of lines defined
C                                    in CORFMT.
C                       CORFIN(3) -- Maximum number of lines allowed
C                                    in CORFMT.
C                       CORFIN(4) -- Number of entries in each
C                                    line of CORFMT.
C
C                       The INTEGER array CORFIN is equivalenced to
C                       the REAL array CORRIN, and
C                       if encoding is ASCII real, then
C                       CORRIN(5) -- Minimum data value input to encoder
C                       CORRIN(6) -- Maximum data value input to encoder
C                       CORRIN(7) -- Minimum data value output from
C                                    encoder.
C                       CORRIN(8) -- Maximum data value output from
C                                    encoder.
C                       as obtained from DEVICE_COORD_FLOATING_INFO
C      BATCH  [ L,.FALSE.] DEVICE_BATCH
C      DHCSIZ [ I, - ]  Number of defined characters in DEVICE_CURSOR_HOME.
C      DHCSTR [ I, - ]  Array containing the DEVICE_CURSOR_HOME string.
C      CORXOF [ I,  0]  DEVICE_COORD_XOFFSET
C      CORYOF [ I,  0]  DEVICE_COORD_YOFFSET
C      DASBIT [ I,100]  DASH_BIT_LENGTH
C      CORXSC [ R,1.0]  DEVICE_COORD_XSCALE
C      CORYSC [ R,1.0]  DEVICE_COORD_YSCALE
C      VDWLLX [ I,  0]  DEVICE_WINDOW_LOWER_LEFT_X
C      VDWLLY [ I,  0]  DEVICE_WINDOW_LOWER_LEFT_Y
C      VDWURX [ I,32767]  DEVICE_WINDOW_UPPER_RIGHT_X
C      VDWURY [ I,32767]  DEVICE_WINDOW_UPPER_RIGHT_X
C
C
C    CAPERR  (Error flags)
C    ------
C      ALLOK  [ I, 0]  No error condition obtains.
C      EOFFL  [ I, 1]  End-of-file encountered.
C      INTERR [ I, 2]  Error decoding an integer value.
C      MAXINT [ I, 3]  Integer exceeds the maximum number of digits
C                      allowed.
C      PLBERR [ I, 4]  Error in defining the polyline bundle tables
C                      indicating that not all of the tables in the
C                      class are of the same length.
C      PMBERR [ I, 5]  Error in defining the polymarker bundle tables
C                      indicating that not all of the tables in the
C                      class are of the same length.
C      FABERR [ I, 6]  Error in defining the fill area bundle tables
C                      indicating that not all of the tables in the
C                      class are of the same length.
C      TXBERR [ I, 7]  Error in defining the text bundle tables
C                      indicating that not all of the tables in the
C                      class are of the same length.
C      FLTERR [ I, 8]  Error in decoding a floating point value.
C      MAXFLT [ I, 9]  Floating point value exceeds the maximum
C                      number of digits allowed.
C      NOTINT [ I,10]  An integer field is being decoded, but the
C                      value in the input graphcap file is not integer.
C      SIZERR [ I,11]  The string buffer is not big enough to hold
C                      the input string.
C      UNDASC [ I,12]  A value was encountered which is not in the
C                      local ASCII tables.
C      DEVERR [ I,13]  The device coordinate space is null.
C      DOCERR [ I,14]  Not used.
C      TBLERR [ I,15]  Not used.
C      STSZER [ I,16]  Not used.
C      ENTERR [ I,17]  Not used.
C      TABERR [ I,18]  Not used.
C      TABSER [ I,19]  Not used.
C      PRSFIL [ I,20]  Not used.
C
C
C    CAPIO2
C    ------
C      LINE  [CA, - ]  Array containing the current line buffer.
C
C
C    CAPIOB
C    ------
C      UNIT  [ I, - ]  Used for the unit number for reading the
C                      input file, and for writing the output file.
C      IPTR  [ I, - ]  Points to the current position in the LINE buffer.
C      LSIZE [ I, - ]  The number of useable characters in LINE.
C
C    CAPLIN  (Contains the POLYLINE information.)
C    ------
C      PLAVBL  [ L,.FALSE.]  LINE_DRAW_POLY_FLAG
C      LDSSTR  [IA, - ]  LINE_DRAW_INSTRUCTION_START
C      LDSSIZ  [ I, - ]  Number of characters in  LDSSTR.
C      LDTSTR  [IA, - ]  LINE_DRAW_INSTRUCTION_TERMINATOR
C      LDTSIZ  [ I, - ]  Number of characters in  LDTSTR.
C      LMSSTR  [IA, - ]  LINE_MOVE_INSTRUCTION_START
C      LMSSIZ  [ I, - ]  Number of characters in LMSSTR.
C      LMTSTR  [IA, - ]  LINE_MOVE_INSTRUCTION_TERMINATOR
C      LMTSIZ  [ I, - ]  Number of characters in  LMTSTR.
C      LCSSTR  [IA, - ]  LINE_COLOR_INSTRUCTION_START
C      LCSSIZ  [ I, - ]  Number of characters in  LCSSTR.
C      LCTSTR  [IA, - ]  LINE_COLOR_INSTRUCTION_TERMINATOR
C      LCTSIZ  [ I, - ]  Number of characters in  LCTSTR.
C      LINFIN [IA,  0]  Array dimensioned for 8 describing the
C                       device vector counts as follows:
C                       LINFIN(1) -- DEVICE_VECTOR_COUNT_ENCODING
C                       LINFIN(2) -- Number of lines defined
C                                    in LINFMT.
C                       LINFIN(3) -- Maximum number of lines allowed
C                                    in LINFMT.
C                       LINFIN(4) -- Number of entries in each
C                                    line of LINFMT.
C
C                       The INTEGER array LINFIN is equivalenced to
C                       the REAL array LINRIN, and
C                       if encoding is ASCII real, then
C                       LINRIN(5) -- Minimum data value input to encoder
C                       LINRIN(6) -- Maximum data value input to encoder
C                       LINRIN(7) -- Minimum data value output from
C                                    encoder.
C                       LINRIN(8) -- Maximum data value output from
C                                    encoder.
C                       obtained from DEVICE_VECTOR_COUNT_FLOATING_INFO
C
C                       if encoding is not ASCII real, then
C                       LINFIN(5) equals 5 .
C      LINFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       LVCFMX x 4) containing the
C                       DEVICE_VECTOR_COUNT_FORMAT
C      LWSSTR [IA, - ]  LINE_WIDTH_INSTRUCTION_START
C      LWSSIZ [ I, - ]  Number of characters in LWSSTR.
C      LWTSTR [IA, - ]  LINE_WIDTH_INSTRUCTION_TERMINATOR
C      LWTSIZ [ I, - ]  Number of characters in LWTSTR.
C      LWTFIN [IA,  0]  Array dimensioned for 8 describing the
C                       line widths as follows:
C                       LWTFIN(1) -- LINE_WIDTH_ENCODING
C                       LWTFIN(2) -- Number of lines defined
C                                    in LWTFMT.
C                       LWTFIN(3) -- Maximum number of lines allowed
C                                    in LWTFMT.
C                       LWTFIN(4) -- Number of entries in each
C                                    line of LWTFMT.
C
C                       The INTEGER array LWTFIN is equivalenced to
C                       the REAL array LWTRIN, and
C                       if encoding is ASCII real, then
C                       LWTRIN(5) -- Minimum data value input to encoder
C                       LWTRIN(6) -- Maximum data value input to encoder
C                       LWTRIN(7) -- Minimum data value output from
C                                    encoder.
C                       LWTRIN(8) -- Maximum data value output from
C                                    encoder.
C                       obtained from LINE_WIDTH_FLOATING_INFO
C
C                       if encoding is not ASCII real, then
C                       LWTFIN(5) equals 5 .
C      LWTFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       LWTFMX x 4) containing the
C                       LINE_WIDTH_FORMAT
C      LWTRNG [IA, - ]  Two values specifying the LINE_WIDTH_RANGE.
C      LWTSCF [ R,1.0]  LINE_WIDTH_SCALE
C      LBSSTR [IA, - ]  LINE_BACKGROUND_COLOR_INSTRUCTION_START
C      LBSSIZ [ I, - ]  Number of characters in LBSSTR.
C      LBTSTR [IA, - ]  LINE_BACKGROUND_COLOR_INSTRUCTION_TERMINATOR
C      LBTSIZ [ I, - ]  Number of characters in LBTSTR.
C      LPSSTR [IA, - ]  LINE_POINT_START
C      LPSSIZ [ I, - ]  Number of characters in LPSSTR.
C      LPTSTR [IA, - ]  LINE_POINT_TERMINATOR
C      LPTSIZ [ I, - ]  Number of characters in LPTSTR.
C
C
C    CAPMAR  (Contains POLYMARKER information.)
C    ------
C      MCSSTR [IA, - ]  MARKER_COLOR_INSTRUCTION_START
C      MCSSIZ [ I, - ]  Number of characters in  MCSSTR.
C      MCTSTR [IA, - ]  MARKER_COLOR_INSTRUCTION_TERMINATOR
C      MCTSIZ [ I, - ]  Number of characters in MCTSTR.
C      MARFIN [IA,  0]  Array dimensioned for 5 describing the
C                       marker vector counts as follows:
C                       MARFIN(1) -- MARKER_VECTOR_COUNT_ENCODING
C                       MARFIN(2) -- Number of lines defined
C                                    in MARFMT.
C                       MARFIN(3) -- Currently undefined.
C                       MARFIN(4) -- Currently undefined.
C                       MARFIN(5) -- Currently undefined.
C      MARFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       MVCFMX x 4) containing the
C                       MARKER_VECTOR_COUNT_FORMAT
C      MRSSTR [IA, - ]  MARKER_INSTRUCTION_START
C      MRSSIZ [ I, - ]  Number of characters in MRSSTR.
C      MRTSTR [IA, - ]  MARKER_INSTRUCTION_TERMINATOR
C      MRTSIZ [ I, - ]  Number of charactgers in MRTSTR.
C      MDOTSZ [ I, 0 ]  MARKER_DOT_SIZE
C
C
C    CAPPLG  (Polygon class information.)
C    ------
C      PCSSTR [IA, - ]  POLYGON_COLOR_INSTRUCTION_START
C      PCSSIZ [ I, - ]  Number of characters in PCSSTR.
C      PCTSTR [IA, - ]  POLYGON_COLOR_INSTRUCTION_TERMINATOR
C      PCTSIZ [ I, - ]  Number of characters in PCTSTR.
C      PLSSTR [IA, - ]  POLYGON_INSTRUCTION_START
C      PLSSIZ [ I, - ]  Number of characters in PLSSTR.
C      PLTSTR [IA, - ]  POLYGON_INSTRUCTION_TERMINATOR
C      PLTSIZ [ I, - ]  Number of characters in PLTSTR.
C      PBSSTR [IA, - ]  POLYGON_BACKGROUND_COLOR_INSTRUCTION_START
C      PBSSIZ [ I, - ]  Number of characters in PBSSIZ.
C      PBTSTR [IA, - ]  POLYGON_BACKGROUND_COLOR_INSTRUCTION_TERMINATOR
C      PBTSIZ [ I, - ]  Number of characters in PBTSIZ.
C      PHATSP [ I,300]  POLYGON_HATCH_SPACING
C      PMAXPT [I,32767] POLYGON_MAXIMUM_POINTS
C      PPSSTR [IA, - ]  POLYGON_POINT_START
C      PPSSIZ [ I, - ]  Number of characters in PPSSIZ.
C      PPTSTR [IA, - ]  POLYGON_POINT_TERMINATOR
C      PPTSIZ [ I, - ]  Number of characters in PPTSIZ.
C      PLGSIM [ L,.TRUE.] POLYGON_SIMULATE
C      PSIMSP [ I, * ]  POLYGON_SIMULATION_SPACING --
C                         default depends on device coordinate space.
C      PSIMTR [ I, 0 ]  POLYGON_SIMULATION_TRUNCATION
C
C
C    CAPSCN  (RASTER class information.)
C    ------
C      SCSSTR [IA, - ]  RASTER_HORIZONTAL_INSTRUCTION_START
C      SCSSIZ [ I, - ]  Number of characters in SCSSTR.
C      SCTSTR [IA, - ]  RASTER_HORIZONTAL_INSTRUCTION_TERMINATOR
C      SCTSIZ [ I, - ]  Number of characters in SCTSTR.
C      SCNLLX [ I, - ]  RASTER_COORD_LOWER_LEFT_X
C      SCNLLY [ I, - ]  RASTER_COORD_LOWER_LEFT_Y
C      SCNURX [ I, - ]  RASTER_COORD_UPPER_RIGHT_X
C      SCNURY [ I, - ]  RASTER_COORD_UPPER_RIGHT_Y
C      SCNXOF [ I, - ]  RASTER_COORD_XOFF
C      SCNYOF [ I, - ]  RASTER_COORD_YOFF
C      SCNXSC [ R, - ]  RASTER_COORD_XSCALE
C      SCNYSC [ R, - ]  RASTER_COORD_YSCALE
C      SCNFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       SFMFMX x 4) containing the
C                       RASTER_DATA_FORMAT
C      SCNFIN [IA,  0]  Array dimensioned for 8 describing the
C                       raster data format as follows:
C                       SCNFIN(1) -- RASTER_DATA_ENCODING
C                       SCNFIN(2) -- Number of lines defined
C                                    in SCNFMT.
C                       SCNFIN(3) -- Maximum number of lines allowed
C                                    in SCNFMT.
C                       SCNFIN(4) -- Number of entries in each
C                                    line of SCNFMT.
C
C                       The INTEGER array SCNFIN is equivalenced to
C                       the REAL array SCNRIN, and
C                       if encoding is ASCII real, then
C                       SCNRIN(5) -- Minimum data value input to encoder
C                       SCNRIN(6) -- Maximum data value input to encoder
C                       SCNRIN(7) -- Minimum data value output from
C                                    encoder.
C                       SCNRIN(8) -- Maximum data value output from
C                                    encoder.
C                       obtained from RASTER_DATA_FLOATING_INFO
C      SCVFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       SCVFMX x 4) containing the
C                       RASTER_VECTOR_COUNT_FORMAT
C      SCVFIN [IA,  0]  Array dimensioned for 8 describing the
C                       raster vector count format as follows:
C                       SCVFIN(1) -- RASTER_VECTOR_COUNT_ENCODING
C                       SCVFIN(2) -- Number of lines defined
C                                    in SCVFMT.
C                       SCVFIN(3) -- Maximum number of lines allowed
C                                    in SCVFMT.
C                       SCVFIN(4) -- Number of entries in each
C                                    line of SCVFMT.
C
C                       The INTEGER array SCVFIN is equivalenced to
C                       the REAL array SCVRIN, and
C                       if encoding is ASCII real, then
C                       SCVRIN(5) -- Minimum data value input to encoder
C                       SCVRIN(6) -- Maximum data value input to encoder
C                       SCVRIN(7) -- Minimum data value output from
C                                    encoder.
C                       SCVRIN(8) -- Maximum data value output from
C                                    encoder.
C                       obtained from RASTER_VECTOR_COUNT_FLOATING_INFO
C      SCNSIM [ L,.FALSE.]  RASTER_SIMULATE
C
C
C    CAPSKP (Controls printing of the input lines.)
C    ------
C      SKIPIT [ L,.TRUE.]  Controls printing of the input lines, if
C                          SKIPIT is .TRUE., then the input lines are
C                          not printed; if SKIPIT is .FALSE., then
C                          the input lines are printed.
C
C
C    CAPSPC (Dummy graphcap space for future temporary expansions)
C    ------
C      DUMSPC  [ I, - ]     SCRATCH SPACE (DIMENSIONED FOR DUMSIZ)
C      ENDDSP  [ I, - ]     Dummy variable to flag the end of graphcap
C                           variables in the binary graphcap.
C
C
C    CAPTXT (TEXT class information.)
C    ------
C      TCSSTR [IA, - ]  TEXT_COLOR_INSTRUCTION_START
C      TCSSIZ [ I, - ]  Number of characters in TCSSTR.
C      TCTSTR [IA, - ]  TEXT_COLOR_INSTRUCTION_TERMINATOR
C      TCTSIZ [ I, - ]  Number of characters in TCTSTR.
C      TXTFMT [IA,  0]  A two-dimensional array (dimensioned for
C                       TVCFMX x 4) containing the
C                       TEXT_VECTOR_COUNT_FORMAT
C      TXTFIN [IA,  0]  Array dimensioned for 5 describing the
C                       text vector count format as follows:
C                       TXTFIN(1) -- TEXT_VECTOR_COUNT_ENCODING
C                       TXTFIN(2) -- Number of lines defined
C                                    in TXTFMT.
C                       TXTFIN(3) -- Currently not used.
C                       TXTFIN(4) -- Currently not used.
C      TXSSTR [IA, - ]  TEXT_INSTRUCTION_START
C      TXSSIZ [ I, - ]  Number of characters in TXSSTR.
C      TXTSTR [IA, - ]  TEXT_INSTRUCTION_TERMINATOR
C      TXTSIZ [ I, - ]  Number of characters in TXTSIZ.
C
C
C    CAPUSR
C    ------
C      UPRSTR [IA, - ]  USER_PROMPT
C      UPRSIZ [ I, - ]  Number of characters in UPRSTR.
C
C
C    PARTB1
C    ------
C      PART1  [CA, - ]  Contains all character sequences
C                       which are pertinent to parsing then
C                       input graphcap file.  PART1 is
C                       initialized in the BLOCKDATA.
C      KEYSEP [ C,'_']  A CHARACTER*1 variable containing
C                       the keyword separator.
C      KEYTER [ C,' ']  A CHARACTER*1 variable containing the
C                       keyword terminator.
C      FRCOM  [CA,'/','*']  A CHARACTER*1 array of dimension 2
C                       containing the two initial characters
C                       which will cause a line in the input
C                       grpaphcap to be regarded as a comment line.
C
C    PARTB2
C    ------
C      PART2 [IA, - ]  An array of pointers indicating the
C                      the next jump in the parse tree.  See
C                      the example after the description of
C                      PART5 to see how the parse tables
C                      interact.For
C
C                      we should go to the 24th section of
C                      keywords in PART1 to find our next
C                      keyword.
C                      PART4(24) = 786 indicating we should
C
C                      indicates we should go to PART2(88)
C                      to see where the next row of keywords
C                      is.  PART2(88) = 24, and PART3(24) =
C                      5
C      PART3 [IA, - ]  Contains pointers into PART4.
C      PART4 [IA, - ]  Contains pointers into PART1.  These pointers
C                      indicate where the first character
C                      of sections of keywords are located in PART1.
C      PART5 [IA, - ]  Pairs of integers indicating how many keywords
C                      are in a given section in PART1, and how
C                      many characters in length each keyword
C                      in that section is.  Currently there are
C                      24 different keyword sections in PART1, i.e.
C                      PART1 is segmented into 24 different sections.
C
C                      DISCUSSION of the parse tables:
C                       The keywords necessary to the parsing are
C                       stored in PART1 in groups.  Within each
C                       group, all keywords have the same length
C                       (some may have to be blank padded to satisfy
C                       this.)  The values in PART4 indicate where
C                       in PART1 groups of keywords begin.  The
C                       values in PART5 indicate, for each group of
C                       keywords in PART1, how many keywords are
C                       in that group, and the length of each word
C                       in the group.  The values in PART3 are pointers
C                       to the various keyword groups.
C
C                      EXAMPLE:  Let us consider an example of how
C                       the various parse tables interact.  Suppose
C                       the input keyword string being parsed is
C                       RASTER_HORIZONTAL_INSTRUCTION_TERMINATOR.
C                       The keyword RASTER is the 9th entry
C                       in the initial group of  keywords in PART1
C                       (the 9 entries being: DEVICE, LINE, USER,
C                       BUNDLE, TEXT, MARKER, POLYGON, DASH,
C                       and RASTER.)  PART2 is used to determine
C                       which group of keywords to search next after
C                       RASTER has been found.  If PART2(9) = N,
C                       and PART3(N) = M, then the next group of
C                       keywords to be searched is the Mth group
C                       which starts at PART4(M).  If the keyword
C                       matched within the current keyword group is
C                       number L, then PART2(N+L-1) provides the
C                       pointer into PART3 which will produce the
C                       number of the next keyword group to be
C                       searched.  This procedure is continued
C                       until the entry in PART2 is 0, indicating
C                       the final keyword has been found.
C      CTSTR [ I, - ]  Currently unused.
C      CTLOC [ I, - ]  Currently unused.
C
C
C
C
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
      COMMON /CAPIOB/ UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPIO2/ LINE, GRNM
      INTEGER LNMAX
      PARAMETER (LNMAX=80)
      CHARACTER*80 LINE
      CHARACTER*80 GRNM
      INTEGER UNIT, IPTR, LSIZE, NFST, NFLG
C
C  CONTROL THE PRINTING OF INPUT LINES
C
      COMMON /CAPSKP/ SKIPIT
      LOGICAL SKIPIT
      COMMON /CAPPLG/ PCSSTR, PCSSIZ, PCTSTR, PCTSIZ, PLSSTR,
     1                PLSSIZ, PLTSTR, PLTSIZ, PBSSTR, PBSSIZ,
     2                PBTSTR, PBTSIZ, PHATSP, PMAXPT, PPSSTR,
     3                PPSSIZ, PPTSTR, PPTSIZ, PLGSIM, PSIMSP,
     4                PSIMTR
      INTEGER         PCSMAX, PCTMAX, PLSMAX, PLTMAX, PBSMAX,
     1                PBTMAX, PPSMAX, PPTMAX
      PARAMETER   (PCSMAX=20, PCTMAX=15, PLSMAX=40, PLTMAX=20)
      PARAMETER   (PBSMAX=30, PBTMAX=15, PPSMAX=20, PPTMAX=20)
      INTEGER         PCSSTR(PCSMAX), PCSSIZ, PCTSTR(PCTMAX), PCTSIZ,
     1                PLSSTR(PLSMAX), PLSSIZ, PLTSTR(PLTMAX), PLTSIZ,
     2                PBSSTR(PBSMAX), PBSSIZ, PBTSTR(PBTMAX), PBTSIZ,
     3                PHATSP, PMAXPT, PPSSTR(PPSMAX), PPSSIZ,
     4                PPTSTR(PPTMAX), PPTSIZ, PSIMSP, PSIMTR
      LOGICAL         PLGSIM
      INTEGER         LENPLG
      PARAMETER   (LENPLG=PCSMAX+1+PCTMAX+1+PLSMAX+1+PLTMAX+1+
     1                    PBSMAX+1+PBTMAX+1+1+1+PPSMAX+1+PPTMAX+
     2                    1+1+1+1)
      COMMON /CAPDEV/ DGISTR, DGISIZ, DGESTR, DGESIZ, DTISTR,
     1                DTISIZ, DCDLLX, DCDLLY, DCDURX, DCDURY,
     3                DCOAVL, CORFMT, CORFIN, BATCH , DHCSIZ,
     4                DHCSTR, CORXOF, CORYOF, DASBIT, CORXSC,
     5                CORYSC, VDWLLX, VDWLLY, VDWURX, VDWURY
      INTEGER         DGIMAX, DGEMAX, DTIMAX, DCFTMX, DHCMAX
      PARAMETER   (DGIMAX=300, DGEMAX=150, DTIMAX=100)
      PARAMETER   (DCFTMX=30 , DHCMAX=50)
      INTEGER         DGISTR(DGIMAX), DGISIZ, DGESTR(DGEMAX),
     1                DGESIZ, DTISTR(DTIMAX), DTISIZ, DCDLLX,
     2                DCDLLY, DCDURX, DCDURY, CORFMT(DCFTMX,4),
     3                CORFIN(8)     , DHCSIZ, DHCSTR(DHCMAX),
     4                CORXOF, CORYOF, DASBIT, VDWLLX, VDWLLY,
     5                VDWURX, VDWURY
      REAL            CORXSC, CORYSC, CORRIN(8)
      LOGICAL         DCOAVL, BATCH
C  Size of the COMMON
      INTEGER         LENDEV
      PARAMETER   (LENDEV=DGIMAX+1+DGEMAX+1+DTIMAX+1+4+1+4*DCFTMX+
     1                  8+2+DHCMAX+9)
      EQUIVALENCE (CORFIN,CORRIN)
C
      INTEGER  IOS, STATUS, II
      CHARACTER*80 DFLNAM
C
C Modification for UNIX Version
C
      CHARACTER*80 ARG1, ARG2
C
C End of modification for UNIX Version
C
C Do calls forcing BLOCKDATAs to be loaded from a binary library.
C
      CALL CAPAST
      CALL CAPDAT
C
C  Initialize STATUS to ALLOK.
C
 110  CONTINUE
      STATUS = ALLOK
C
C  Get the file name of the ASCII input graphcap.
C
      DO 10 II = 1,80
  10  DFLNAM(II:II) = ' '
C Modification for UNIX Version

#ifdef	hpux
      CALL IGETARG(1, ARG1, 80)
#else
      CALL GETARG(1, ARG1)
#endif
      DO 15 II = 1,80
	DFLNAM(II:II) = ARG1(II:II)
   15 CONTINUE

C End of modification for UNIX Version
      UNIT = 1
C
C  Open the input graphcap for reading.
C
      CALL CHROPN(UNIT,DFLNAM,IOS,STATUS)
      IF (STATUS .NE. ALLOK ) THEN
C
C  Error opening the input file.
C
        WRITE(6,40)IOS
 40     FORMAT(' ERROR OPENING THE CHARACTER GRAPHCAP FILE, IOS='
     1          ,I7)
        STOP
      END IF
C
C  Parse the character graphcap file after initializing COMMON
C  variables.
C
      CALL GRINIT
      CALL CAPPAR(IOS,STATUS)
      IF (STATUS .NE. ALLOK .AND. STATUS .NE. EOFFL) THEN
C
C  Error reading the file.
C
        WRITE(6,50)STATUS,IOS
 50     FORMAT(' ERROR READING THE CHARACTER GRAPHCAP FILE',
     1          ', STATUS=',I7,' IOS=',I7)
        CALL CHRCLS(UNIT,IOS,STATUS)
        STOP
      END IF
C
C  Default the fill line spacing if it has not been defined.
C
      IF (DCDURY .EQ. DCDLLY) THEN
        WRITE(6,100)
  100   FORMAT(' Device coordinate space is null')
        STOP
      ENDIF
      IF (PSIMSP .EQ. 0) THEN
        ITMP   = INT(0.9*32767./REAL((IABS(DCDURY-DCDLLY))))
        PSIMSP = MAX0(1,ITMP)
      ENDIF
C
C  Close the input graphcap file.
C
      CALL CHRCLS(UNIT,IOS,STATUS)
C
C  Get the file name for the binary output file.
C
      DO 60 II = 1,80
 60   DFLNAM(II:II) = ' '
C Modification for UNIX Version

#ifdef	hpux
      CALL IGETARG(2, ARG2, 80)
#else
      CALL GETARG(2, ARG2)
#endif
      DO 65 II = 1,80
	DFLNAM(II:II) = ARG2(II:II)
   65 CONTINUE

C End of modification for UNIX Version
      UNIT = 1
C
C  Open the binary graphcap output file.
C
      CALL BINOPN(UNIT,DFLNAM,IOS,STATUS)
      IF (STATUS.NE.0) GO TO 1000
C
C  Write the output file.
C
      CALL BINPUT(IOS,STATUS)
      IF (STATUS .NE. 0) THEN
C
C  Error writing the file.
C
        WRITE(6,80)STATUS,IOS
 80     FORMAT(' ERROR WRITING THE BINARY GRAPHCAP FILE',
     1          ', STATUS=',I7,' IOS=',I7)
        CALL BINCLS(UNIT,IOS,STATUS)
        STOP
      END IF
C
C  Close the output file.
C
      CALL BINCLS(UNIT,IOS,STATUS)
C
C  More graphcaps?
C
C Modification for UNIX Version

C Removed prompt for more graphcaps

C End of modification for UNIX Version
C
      STOP
 1000 CONTINUE
      WRITE(6,1010)
 1010 FORMAT(' CANNOT OPEN THE BINARY OUTPUT FILE')
      END
