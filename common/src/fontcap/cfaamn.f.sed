C
C	$Id: cfaamn.f.sed,v 1.4 2008-07-27 12:23:41 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM CFAAMN
C
C  This program converts the tabular form of a fontcap, as described
C  in "NCAR Graphics Installer's GUIDE," or a fontcap describing an
C  NCAR filled font into a compact binary representation that is read 
C  directly by ctrans or PLOTCHAR.  
C
C  This program was created by merging two independent processors
C  (one for the stroked fonts and the other for the filled fonts).
C  The forms of the fontcaps are quite different in the case of 
C  stroked or filled fonts and the logic in this code that handles 
C  the two cases is different.  Subroutines in this code that are
C  common to the processing of both forms of the fontcap begin with
C  the letters "CF"; subroutines that apply to the processing of
C  the stroked fonts begin with "SF"; subroutines that apply to the
C  processing of the filled fonts begin with "FF".
C
C  Exclusive of the character stroke tables, all fontcap entries
C  are written to the output file as integers.  The character
C  stroke tables are packed into integer words in accordance 
C  with the COORD field descriptors.  Each line of a stroke
C  table entry for each character is packed into one integer.
C
C  Upon abnormal termination, a status number will be printed.  The 
C  meaning of these numbers is:
C
C       STATUS  --  Integer-valued error status:
C                              =  0 -- No errors
C                              =  1 -- EOF encountered.
C                              =  2 -- Error decoding an integer value.
C                              =  3 -- Integer exceeds max. number of
C                                      digits allowed.
C                              =  8 -- Error decoding floating-point value.
C                              =  9 -- Floating value exceeds maximum
C                                      number of digits allowed.
C                              = 10 -- Not an integer field.
C                              = 11 -- String buffer not big enough
C                                      to hold the input string.
C                              = 12 -- Non-ASCII value encountered.
C                              = 13 -- The keyword FONT_TYPE has not
C                                      been defined.
C
C  This code contains common blocks CAPFNT, FNTTB1, FNTTB2,
C  FNTERR, FNTIOB, FNTIO2.  The contents of these common
C  blocks are described below.
C
C
C  CAPFNT
C  ------
C
C  The binary output file for stroked fonts is stored in common CAPFNT.  
C  (The binary output for the filled fonts is stored in BUFFER, see
C  the comments in fntcom.h for details on that encoding.)
C  There are two important arrays in common CAPFNT:  CPNTRS dimensioned
C  for CHRSM1 (set in a data statement), and  CSTRKS dimensioned
C  for CHRSM2 (set in a data statement.)  
C
C  The binary output file contains:
C
C   Word #   Variable  Description
C   ------   --------  -----------
C
C        1   CHSTRT    CHARACTER_START
C        2   CHSEND    CHARACTER_END
C        3   CHRWDT    CHARACTER_WIDTH
C   4-  14   CHRSCH    Scratch array for possible future expansion of
C                      the CHARACTER class.
C       15   FNTRHT    FONT_RIGHT
C       16   FNTTOP    FONT_TOP
C       17   FNTCAP    FONT_CAP
C       18   FNTHLF    FONT_HALF
C       19   FNTBAS    FONT_BASE
C       20   FNTBOT    FONT_BOTTOM
C       21   FNTTYP    FONT_TYPE
C  22-  30   FNTSCH    Scratch array for possible future expansion of
C                      the FONT class.
C       31   CORXST    COORD_X_START
C       32   CORXLN    COORD_X_LEN
C       33   CORYST    COORD_Y_START
C       34   CORYLN    COORD_Y_LEN
C       35   CORPST    COORD_PEN_START
C       36   CORPLN    COORD_PEN_LEN
C       37   PBEGST    PAINT_BEGIN_START
C       38   PBEGLN    PAINT_BEGIN_LEN
C       39   PENDST    PAINT_END_START
C       40   PENDLN    PAINT_END_LEN
C  41-  50   CORSCH    Scratch array for possible future expansion of
C                      the COORDINATE or PAINT classes.
C  51- 350   NEWCLS    Scratch array for the possible future addition
C                      of new classes.
C 351- 478   CPNTRS    One integer for each defined character
C                      which points into the stroke sequence
C                      in array CSTRKS, defined below, indicating where
C                      the stroke sequence for the defined
C                      character begins.  CPNTRS is dimensioned
C                      for CHRSM1.
C      479   CPNLST    Pointer to the final stroke of the final character.
C 480-5600   CSTRKS    Packed stroke sequences for the characters;
C                      each integer contains one line packed from 
C                      the stroke tables in the fontcap table.
C                      CSTRKS is dimensioned for CHRSM2.
C
C
C  FNTTB1
C  ------
C 
C        PART1  -- Parser Table 1 is a CHARACTER*1 array
C                  which contains all character sequences
C                  pertinent to parsing the fontcap form.
C                  First to appear in PART1 is a list of
C                  all the major classes (CHARACTER, COORD,
C                  PAINT, CHAR, FONT), and then comes a list
C                  of the keywords pertinent to each class.
C                  PART1 is completely specified in the BLOCKDATA.
C        KEYSEP -- Is a CHARACTER*1 variable containing
C                  the keyword separator.
C        KEYTER -- Is a CHARACTER*1 variable containing the
C                  keyword terminator.
C        FRMCOM -- Is a CHARACTER*1 array of dimension 2
C                  containing the two initial characters
C                  which will cause a line in the fontcap
C                  form to be regarded as a comment line.
C
C  FNTTB2
C  ------
C
C         PART2 -- The fontcap entries are processed in
C                  the order: CHARACTER, COORD, PAINT, CHAR,
C                  FONT.  The elements of PART2 point to
C                  the virtual words in PART1 containing
C                  the keywords for the individual classes.
C                  For example, PART2(2) points to word 10
C                  which is X_START, the first keyword 
C                  appropriate to the COORD class.  PART2(4)
C                  is 0 since there are no keywords appropriate
C                  to the class CHAR.
C         PART3 -- Contains pointers into the table descriptors
C                  defined in PART4 and PART5.
C         PART4 -- Contains pointers into PART1 indicating
C                  at which character the keywords pertinent
C                  to a given class begin.
C         PART5 -- An array of pointers into PART1.  For each
C                  table, there are two entries in PART5,
C                  the first specifying how many keywords
C                  there are in a given class, and the second
C                  specifying how many characters long each
C                  keyword in that class is.
C         PTHBTS - Number of bits to hold internal flags to
C                  indicate what keywords are being processed.
C
C
C  FNTERR
C  ------
C         
C         ALLOK  --  Error number 0 to indicate everything is OK.
C         EOFFL  --  Set to 1, indicates EOF encountered.
C         INTERR --  Error number 2 indicating an error in trying
C                    to convert an integer fontcap entry to character.
C         MAXINT --  Error number 3 indicating the maximum number
C                    of characters per line has been exceeded.
C
C
C  FNTIOB
C  ------
C
C         UNIT   --  FORTRAN logical unit used for the input 
C                    fontacp file.
C         IPTR   --  Pointer to the column number of the current
C                    character being processed.
C         LSIZE  --  Size (in characters) of the current input line.
C                    
C
C  FNTIO2
C  ------
C
C         LINE  --  Current line being processed.
C
C  Other internal variables of interest:
C
C         PD011 -- Number of keywords in the highest class.
C         PD012 -- Number of characters per keyword in highest class.
C         PD021 -- Number of keywords in the CHARACTER class.
C         PD022 -- Number of characters per keyword in the CHARACTER class.
C         PD031 -- Number of keywords in the COORD class.
C         PD032 -- Number of characters per keyword in the COORD class.
C         PD041 -- Number of keywords in the PAINT class.
C         PD042 -- Number of characters per keyword in the PAINT class.
C         PD051 -- Number of keywords in the FONT class.
C         PD052 -- Number of characters per keyword in the FONT class.
C
      include 'fnttab.h'
      include 'fnterr.h'
      include 'fntcom.h'
C
      INTEGER  IOS, STATUS, II
      CHARACTER*80  DFLNAM
      CHARACTER*80 ARG1, ARG2, ARG3
C
C Do calls forcing BLOCKDATAs to be loaded from a binary library.
C
      CALL FFTBKD
      CALL SFTBKD
C
C  Set the status field to ALLOK and inititalize the array to 
C  receive the name of the character fontcap file.
C
      STATUS = ALLOK
      DO 10 II = 1,80
  10  DFLNAM(II:II) = ' '
C
C  Check if the verbose mode flag is there.
C
#ifdef  hpux
      CALL IGETARG(3, ARG3, 80)
#else
      CALL GETARG(3, ARG3)
#endif
      VERBOS = 0
      IF (ARG3(1:2) .EQ. '-v') VERBOS =1
C
C
C  Get file name of the input fontcap file, and OPEN the file.
C
#ifdef  hpux
      CALL IGETARG(1, ARG1, 80)
#else
      CALL GETARG(1, ARG1)
#endif
C
      DO 15 II = 1,80
        DFLNAM(II:II) = ARG1(II:II)
   15 CONTINUE
C
      UNIT = 1
      CALL CHROPN(UNIT,DFLNAM,IOS,STATUS)
      IF (STATUS .NE. ALLOK ) THEN
C
C       Error opening the file.
C
        WRITE(6,40)IOS
 40     FORMAT(' ERROR OPENING THE CHARACTER FONTCAP FILE, IOS='
     1          ,I7)
        STOP
      END IF
C
C  Parse the character fontcap file.
C
      CALL SFPRCF(ITYP,IOS,STATUS)
      IF (STATUS.NE.ALLOK .AND. STATUS.NE.EOFFL) THEN
C
C       Error reading the file.
C
        WRITE(6,50)STATUS,IOS
 50     FORMAT(' ERROR READING THE CHARACTER FONTCAP FILE',
     1          ', STATUS=',I7,' IOS=',I7)
        CALL CHRCLS(UNIT,IOS,STATUS)
        STOP
      END IF
C
C  Close the fontcap file.
C
      CALL CHRCLS(UNIT,IOS,STATUS)
C
C  Open the fontcap binary file for writing.
C
      DO 60 II = 1,80
 60   DFLNAM(II:II) = ' '
C
#ifdef  hpux
      CALL IGETARG(2, ARG2, 80)
#else
      CALL GETARG(2, ARG2)
#endif
      DO 65 II = 1,80
        DFLNAM(II:II) = ARG2(II:II)
   65 CONTINUE
C
      UNIT = 1
      CALL BINOPN(UNIT,DFLNAM,IOS,STATUS)
      IF (STATUS.NE.0) GO TO 1000
C
      CALL CFWRIT(UNIT,ITYP,IOS,STATUS)
      IF (STATUS .NE. 0) THEN
C
C  Error writing the file.
C
        WRITE(6,80)STATUS,IOS
 80     FORMAT(' ERROR WRITING THE BINARY FONTCAP FILE',
     +       ', STATUS=',I7,' IOS=',I7)
        CALL BINCLS(UNIT,IOS,STATUS)
        STOP
      END IF
C
C  Close the binary fontcap file.
C
      CALL BINCLS(UNIT,IOS,STATUS)
C
      STOP
 1000 CONTINUE
      WRITE(6,1010)
 1010 FORMAT(' CANNOT OPEN THE BINARY FILE')
      END
