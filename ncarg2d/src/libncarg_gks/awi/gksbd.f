C
C	$Id: gksbd.f,v 1.38 2010-04-02 16:38:00 brownrig Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GKSBD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA GKSBDX
C
      include 'gkscom.h'
C
C
C     DESCRIPTION OF ALL GKS COMMON BLOCKS
C
C-----------------------------------------------------------------------
C
C     GKINTR:  GKS INTERNAL VARIABLES
C
C       NOPWK   -- NUMBER OF CURRENTLY OPEN WORKSTATIONS
C       NACWK   -- NUMBER OF CURRENTLY ACTIVE WORKSTATIONS
C       WCONID  -- CONNECTION IDENTIFIER FOR WISS
C       NUMSEG  -- NUMBER OF SEGMENTS CURRENTLY IN USE
C       SEGS    -- SET OF SEGMENT NAMES CURRENTLY IN USE
C       CURSEG  -- NAME OF CURRENT SEGMENT
C       SEGLEN  -- LENGTH (IN NUMBER OF RECORDS) OF THE ASSOCIATED
C                  SEGMENTS.
C       MXSREC  -- THE NUMBER OF RECORDS IN THE SEGMENT TO PROCESS
C                  IN A COPY TO A WORKSTATION
C       SEGT    -- AN ARRAY OF SEGMENT TRANSFORMATION MATRICES
C       CURTM   -- THE CURRENT SEGMET TRANSFORMATION WHEN COPYING A
C                  SEGMENT
C       SEGDEL  -- FLAG TO INDICATE WHETHER ALL SEGMENTS SHOULD BE
C                  REMOVED AT CLOSE GKS TIME (0 = NO; 1 = YES)
C       RWKSP   -- REAL WORKSPACE ARRAY
C       GKSCLP  -- FLAG TO INDICATE WHETHER GKS CLIPPING IS ON
C                  (0 = NO; 1 = YES)
C-----------------------------------------------------------------------
C
C     GKOPDT:  OPERATING STATE AND DESCRIPTION TABLE VARIABLES
C
C       OPS    --  THE GKS OPERATING STATE
C       KSLEV  --  LEVEL OF GKS
C       WK     --  NUMBER OF AVAILABLE WORKSTATION TYPES
C       LSWK   --  LIST OF AVAILABLE WORKSTATION TYPES
C       MOPWK  --  MAXIMUM NUMBER OF SIMULTANEOUSLY OPEN WORKSTATIONS
C       MACWK  --  MAXIMUM NUMBER OF SIMULTANEOUSLY ACTIVE WORKSTATIONS
C       MNT    --  MAXIMUM NORMALIZATION TRANSFORMATION NUMBER
C-----------------------------------------------------------------------
C
C     GKSTAT: GKS STATE LIST VARIABLES--
C       SOPWK  -- SET OF OPEN WORKSTATIONS
C       SACWK  -- SET OF ACTIVE WORKSTATIONS
C       CPLI   -- CURRENT POLYLINE INDEX
C       CLN    -- CURRENT LINETYPE
C       CLWSC  -- CURRENT LINEWIDTH SCALE FACTOR
C       CPLCI  -- CURRENT POLYLINE COLOR INDEX
C       CLNA   -- CURRENT LINETYPE ASF
C       CLWSCA -- CURRENT LINEWIDTH SCALE FACTOR ASF
C       CPLCIA -- CURRENT POLYLINE COLOR INDEX ASF
C       CPMI   -- CURRENT POLYMARKER INDEX
C       CMK    -- CURRENT MARKER TYPE
C       CMKS   -- CURRENT MARKER SIZE SCALE FACTOR
C       CPMCI  -- CURRENT POLYMARKER COLOR INDEX
C       CMKA   -- CURRENT MARKER TYPE ASPECT SOURCE FLAG
C       CMKSA  -- CURRENT MARKER SIZE SCALE FACTOR ASF
C       CPMCIA -- CURRENT POLYMARKER COLOR INDEX ASF
C       CTXI   -- CURRENT TEXT INDEX
C       CTXFP  -- CURRENT TEXT FONT AND PRECISION
C       CCHXP  -- CURRENT CHARACTER EXPANSION FACTOR
C       CCHSP  -- CURRENT CHARACTER SPACING
C       CTXCI  -- CURRENT TEXT COLOR INDEX
C       CTXFPA -- CURRENT TEXT FONT AND PRECISION ASF
C       CCHXPA -- CURRENT CHARACTER EXPANSION FACTOR ASF
C       CCHSPA -- CURRENT CHARACTER SPACING ASF
C       CTXCIA -- CURRENT TEXT COLOR INDEX ASF
C       CCHH   -- CURRENT CHARACTER HEIGHT
C       CCHUP  -- CURRENT CHARACTER UP VECTOR
C       CTXP   -- CURRENT TEXT PATH
C       CTXAL  -- CURRENT TEXT ALIGNMENT
C       CFAI   -- CURRENT FILL AREA INDEX
C       CFAIS  -- CURRENT FILL AREA INTERIOR STYLE
C       CFASI  -- CURRENT FILL AREA STYLE INDEX
C       CFACI  -- CURRENT FILL AREA COLOR INDEX
C       CFAISA -- CURRENT FILL AREA INTERIOR STYLE ASF
C       CFASIA -- CURRENT FILL AREA STYLE INDEX ASF
C       CFACIA -- CURRENT FILL AREA COLOR INDEX ASF
C       CPA    -- CURRENT PATTERN SIZE
C       CPARF  -- CURRENT PATTERN REFERENCE POINT
C       CNT    -- CURRENT NORMALIZATION TRANSFORMATION NUMBER
C       LSNT   -- LIST OF NORMALIZATION TRANSFORMATIONS (ORDERED
C                 BY VIEWPORT INPUT PRIORITY)
C       NTWN   -- NORMALIZATION TRANSFORMATION WINDOWS
C       NTVP   -- NORMALIZATION TRANSFORMATION VIEWPORTS
C       CCLIP  -- CURRENT CLIPPING INDICATOR
C       SWKTP  -- SET OF WORKSTATION TYPES ASSOCIATED WITH THE
C                 OPEN WORKSTATIONS
C       NOPICT -- FLAG TO INDICATE NO PICTURE ELEMENTS HAVE BEEN
C                 ISSUED FOR THE CURRENT PICTURE
C       NWKTP  -- WORKSTATION TYPE
C       LXWKID -- LOCAL X WINDOW WKID RETRIEVED FROM THE X DRIVER
C                 AT OPEN WORKSTATION TIME
C       ECONID -- THE WINDOW ID FOR WORKSTATIONS OF TYPE GXWE (EXISTING
C                 X WINDOW).  THIS VALUE IS SUPPLIED VIA THE CONNECTION
C                 ID AT OPEN WORKSTATION TIME.
C       CLLX   -- LOWER LEFT X COORDINATE FOR POSITIONING PICTURE ON PAGE
C       CLLY   -- LOWER LEFT Y COORDINATE FOR POSITIONING PICTURE ON PAGE
C       CURX   -- UPPER RIGHT X COORDINATE FOR POSITIONING PICTURE ON PAGE
C       CURY   -- UPPER RIGHT Y COORDINATE FOR POSITIONING PICTURE ON PAGE
C       CPSCL  -- SCALE FACTOR FOR POSTSCRIPT WORKSTATIONS
C       CCMDL  -- Flag for CMYK color model or RGB color model
C       CSUPR  -- Flag for suppressing background color and/or bounding
C                 box for PS output.
C       CPTLD  -- Flag for portrait PS mode (=0), or landscape (non-zero)
C       COLMOD -- Color Model for X color allocation
C       PDFHGT -- Height of PDF to be specified in the MediaBox
C       PDFWTH -- Width of PDF to be specified in the MediaBox
C       PSHGT  -- Height of PS paper to be specified in the setpagedevice
C       PSWTH  -- Width of PS paper to be specified in the setpagedevice
C-----------------------------------------------------------------------
C
C     GKEROR:  GKS ERROR STATE LIST
C       ERS    --  ERROR STATE
C       ERF    --  ERROR FILE
C       CUFLAG --  A UTILITY FLAG THAT IS USED TO MARK THAT A PARTICULAR
C                  WORKSTATION IS BEING ADDRESSED IN THE INTERFACE.  IF
C                  CUFLAG IS POSITIVE, THEN IT IS EQUAL TO THE
C                  WORKSTATION ID OF THE PARTICULAR WORKSTATION FOR
C                  WHICH INSTRUCTIONS ARE TARGETED; IF CUFLAG = -1, THEN
C                  INSTRUCTIONS SHOULD GO TO ALL APPROPRIAT WORKSTATIONS.
C       MXERMG --  MAXIMUM NUMBER OF ERROR MESSAGES TO ISSUE BEFORE ABORT
C
C-----------------------------------------------------------------------
C
C     GKETBI:
C       IERNMS --  AN ARRAY CONTAINING THE NCAR GKS ERROR NUMERS
C     GKETBC:
C       ERMSGS --  AN ARRAY CONTAINING THE NCAR GKS ERROR MESSAGE STRINGS
C-----------------------------------------------------------------------
C
C     GKENUM: GKS ENUMERATION TYPE VARIABLES
C       GBUNDL -- BUNDLED
C       GINDIV -- INDIVIDUAL
C       GGKCL  -- GKS CLOSED
C       GGKOP  -- GKS OPEN
C       GWSOP  -- WORKSTATION OPEN
C       GWSAC  -- WORKSTATION ACTIVE
C       GSGOP  -- SEGMENT OPEN
C       GOUTPT -- OUTPUT WORKSTATION
C       GINPUT -- INPUT WORKSTATION
C       GOUTIN -- OUTPUT/INPUT WORKSTATION
C       GWISS  -- WORKSTATION INDEPENDENT SEGMENT STORAGE
C       GMO    -- METAFILE OUTPUT WORKSTATION
C       GMI    -- METAFILE INPUT WORKSTATION
C       GCGM   -- WORKSTATION TYPE CGM
C       GCROMIN -- MINIMUM TYPE FOR THE cairo DRIVERS
C       GCROMAX -- MAXIMUM TYPE FOR THE cairo DRIVERS
C       GWSS   -- WORKSTATION TYPE WISS
C       GXWE   -- WORKSTATION TYPE EXISTING COLOUR X WINDOW
C       GXWC   -- WORKSTATION TYPE COLOUR X WINDOW
C       GPIX   -- Workstation type for PIX map.
C       GPSMIN -- MINIMUM TYPE FOR THE POSTSCRIPT DRIVERS
C       GPSMAX -- MAXIMUM TYPE FOR THE POSTSCRIPT DRIVERS
C       GPDFP  -- Workstation type for PDF portrait.
C       GPDFL  -- Workstation type for PDF landscape.
C
C-----------------------------------------------------------------------
C
C     GKSNAM: NAMES
C
C       GNAM   -- ARRAY OF GKS FUNCTION NAMES IS AS PER THE BINDING
C       SEGNAM -- FILE NAMES ASSOCIATED WITH THE SEGMENT NUMBERS IN
C                 VARIABLE SEGS
C       GFNAME -- FILENAME FOR MO WORKSTATIONS
C       GSEGRT -- ROOT NAME FOR SEGMENT FILE NAMES
C
C-----------------------------------------------------------------------
C
C     GKSIN1 & GKSIN2: WORKSTATION INTERFACE COMMON BLOCKS
C
C       FCODE  -- FUNCTION CODE FOR THE CURRENT INSTRUCTION
C       CONT   -- CONTINUATION FLAG (1 MEANS MORE TO COME; 0 MEANS LAST)
C       IL1    -- TOTAL NUMBER OF ELEMENTS TO BE PASSED IN THE ID
C                 ARRAY FOR THE CURRENT INSTRUCTION
C       IL2    -- NUMBER OF ELEMENTS IN THE ID ARRAY FOR THE GIVEN
C                 WORKSTATION INTERFACE INVOCATION
C       ID     -- ARRAY FOR PASSING INTEGERS
C       IC1    -- TOTAL NUMBER OF ELEMENTS TO BE PASSED IN THE IC
C                 ARRAY FOR THE CURRENT INSTRUCTION
C       IC2    -- NUMBER OF ELEMENTS IN THE IC ARRAY FOR THE GIVEN
C                 WORKSTATION INTERFACE INVOCATION
C       IC     -- ARRAY FOR PASSING COLOR INDICES
C       RL1    -- TOTAL NUMBER OF ELEMENTS TO BE PASSED IN THE RX AND
C                 RY ARRAYS FOR THE CURRENT INSTRUCTION
C       RL2    -- NUMBER OF ELEMENTS IN THE RX AND RY ARRAYS FOR THE
C                 GIVEN WORKSTATION INTERFACE INVOCATION
C       RX     -- ARRAY FOR PASSING REAL X COORDINATE VALUES
C       RY     -- ARRAY FOR PASSING REAL Y COORDINATE VALUES
C       STRL1  -- TOTAL NUMBER OF CHARACTERS TO BE PASSED IN THE
C                 CHARACTER VARIABLE STR FOR THE CURRENT INSTRUCTION
C       STRL2  -- NUMBER OF CHARACTERS IN THE CHARACTER VARIABLE STR
C                 FOR THE CURRENT INVOCATION OF THE WORKSTATION
C                 INTERFACE
C       RERR   -- RETURN VARIABLE FOR ERROR INDICATOR
C       STR    -- CHARACTER VARIABLE FOR PASSING CHARACTERS
C
C-----------------------------------------------------------------------
      DATA KSLEV,WK/0, 28/
C**************************************************************************
C******** Be sure and change the dimension of LSWK in gkscom.h when    ****
C******** adding a new workstation type (as well as changing the value ****
C******** of WK above to the same number).                              ****
C**************************************************************************
      DATA LSWK/1,3,7,8,9,10,11,12,20,21,22,23,24,25,26,27,28,29,30,31,
     +          40,41,42,43,44,45,46,47/
C**************************************************************************
      DATA MOPWK,MACWK,MNT
     +    /   15,   15,  1/
      DATA OPS/0/
      DATA ERS,ERF,CUFLAG,MXERMG/0,6,-1,10/
      DATA GBUNDL,GINDIV/0,1/
      DATA GGKCL,GGKOP,GWSOP,GWSAC,GSGOP/0,1,2,3,4/
      DATA GOUTPT,GINPUT,GOUTIN,GWISS,GMO,GMI/0,1,2,3,4,5/
      DATA GCGM,GWSS,GXWE,GXWC,GPIX, GDMP,GPDFP,GPDFL,GPSMIN,GPSMAX
     +    /   1,   3,   7,   8,   9,   10,   11,   12,    20,    31/
      DATA GCROMIN, GCROMAX
     +    /     40,      50/
      DATA NOPWK,NACWK,NUMSEG,CURSEG,SEGDEL,GKSCLP/0,0,0,-1,1,1/
      DATA NOPICT/-1/
      DATA GFNAME/'DEFAULT'/
      DATA GSEGRT/'GSEG'/
      DATA CLLX,CLLY,CURX,CURY,CPSCL/-9999,-9999,-9999,-9999,-1/
      DATA CCMDL/1/
      DATA CSUPR/0/
      DATA CPTLD/1/
      DATA COLMOD/-1/
      DATA PDFHGT/792/
      DATA PDFWTH/612/
      DATA PSHGT/792/
      DATA PSWTH/612/
C
      DATA IERNMS/    1,    2,    3,    4,    5,    6,    7,    8,
     +               20,   21,   22,   23,   24,   25,   26,   27,
     +               28,   29,   30,   32,   33,   34,   35,   36,
     +               38,   50,   51,   52,   53,   54,   60,   63,
     +               65,   66,   69,   72,   75,   77,   78,   79,
     +               80,   84,   85,   87,   91,   92,   93,   96,
     +              100,  101,  102,  103,  120,  121,  122,  124,
     +              160,  161,  162,  163,  165,  166,  167,  168,
     +              180,  182,  300,  302,  303,  304,  305,  306,
     +              307,  308, 2000, 2001, 2002, 2003, -100, -101,
     +             -102, -103, -105, -106, -107, -108, -109, -110,
     +             -111, -112, -200, -201, -202, -203, -204, -205,
     +             -206, -207, -208, -209, -210, -211, -212, -213,
     +             -214, -215, -216, -300, -301, -302, -303, -217,
     +             -218, -219, -220, -221, 2200, 2201, 2202, 2203,
     +             2204,   90, -113, -400, -401, -402, -403, -404,
     +             -405, -406, -350, -351, -352, -353, -450, -451,
     +             -452, -453, -354, -501, -502, -355, -356, -503,
     *             -504/
C  Error 1
      DATA ERMSGS(  1)/' --GKS NOT IN PROPER STATE: GKS SHALL BE IN STAT
     +E GKCL'/
C  Error 2
      DATA ERMSGS(  2)/' --GKS NOT IN PROPER STATE: GKS SHALL BE IN STAT
     +E GKOP'/
C  Error 3
      DATA ERMSGS(  3)/' --GKS NOT IN PROPER STATE: GKS SHALL BE IN STAT
     +E WSAC'/
C  Error 4
      DATA ERMSGS(  4)/' --GKS NOT IN PROPER STATE: GKS SHALL BE IN STAT
     +E SGOP'/
C  Error 5
      DATA ERMSGS(  5)/' --GKS NOT IN PROPER STATE: GKS SHALL BE EITHER
     +IN THE STATE WSAC OR IN THE STATE SGOP'/
C  Error 6
      DATA ERMSGS(  6)/' --GKS NOT IN PROPER STATE: GKS SHALL BE EITHER
     +IN THE STATE WSOP OR IN THE STATE WSAC'/
C  Error 7
      DATA ERMSGS(  7)/' --GKS NOT IN PROPER STATE: GKS SHALL BE IN ONE
     +OF THE STATES  WSOP, WSAC, OR SGOP'/
C  Error 8
      DATA ERMSGS(  8)/' --GKS NOT IN PROPER STATE: GKS SHALL BE IN ONE
     +OF THE STATES  GKOP, WSOP, WSAC, OR SGOP'/
C  Error 20
      DATA ERMSGS(  9)/' --SPECIFIED WORKSTATION IDENTIFIER IS INVALID
     +'/
C  Error 21
      DATA ERMSGS( 10)/' --SPECIFIED CONNECTION IDENTIFIER IS INVALID'/
C  Error 22
      DATA ERMSGS( 11)/' --SPECIFIED WORKSTATION TYPE IS INVALID'/
C  Error 23
      DATA ERMSGS( 12)/' --SPECIFIED WORKSTATION TYPE DOES NOT EXIST'/
C  Error 24
      DATA ERMSGS( 13)/' --SPECIFIED WORKSTATION IS OPEN'/
C  Error 25
      DATA ERMSGS( 14)/' --SPECIFIED WORKSTATION IS NOT OPEN'/
C  Error 26
      DATA ERMSGS( 15)/' --SPECIFIED WORKSTATION CANNOT BE OPENED'/
C  Error 27
      DATA ERMSGS( 16)/' --WORKSTATION INDEPENDENT SEGMENT STORAGE IS NO
     +T OPEN'/
C  Error 28
      DATA ERMSGS( 17)/' --WORKSTATION INDEPENDENT SEGMENT STORAGE IS AL
     +READY OPEN'/
C  Error 29
      DATA ERMSGS( 18)/' --SPECIFIED WORKSTATION IS ACTIVE'/
C  Error 30
      DATA ERMSGS( 19)/' --SPECIFIED WORKSTATION IS NOT ACTIVE'/
C  Error 32
      DATA ERMSGS( 20)/' --SPECIFIED WORKSTATION IS NOT OF CATEGORY MO'/
C  Error 33
      DATA ERMSGS( 21)/' --SPECIFIED WORKSTATION IS OF CATEGORY MI'/
C  Error 34
      DATA ERMSGS( 22)/' --SPECIFIED WORKSTATION IS NOT OF CATEGORY MI'/
C  Error 35
      DATA ERMSGS( 23)/' --SPECIFIED WORKSTATION IS OF CATEGORY INPUT'/
C  Error 36
      DATA ERMSGS( 24)/' --SPECIFIED WORKSTATION IS WORKSTATION INDEPEND
     +ENT SEGMENT STORAGE'/
C  Error 38
      DATA ERMSGS( 25)/' --SPECIFIED WORKSTATION IS NEITHER OF CATEGORY
     +INPUT NOR OF CATEGORY OUTIN'/
C  Error 50
      DATA ERMSGS( 26)/' --TRANSFORMATION NUMBER IS INVALID'/
C  Error 51
      DATA ERMSGS( 27)/' --RECTANGLE DEFINITION IS INVALID'/
C  Error 52
      DATA ERMSGS( 28)/' --VIEWPORT IS NOT WITHIN THE NORMALIZED DEVICE
     +COORDINATE UNIT SQUARE'/
C  Error 53
      DATA ERMSGS( 29)/' --WORKSTATION WINDOW IS NOT WITHIN THE NORMALIZ
     +ED DEVICE COORDINATE UNIT SQUARE'/
C  Error 54
      DATA ERMSGS( 30)/' --WORKSTATION VIEWPORT IS NOT WITHIN THE DISPLA
     +Y SPACE'/
C  Error 60
      DATA ERMSGS( 31)/' --POLYLINE INDEX IS INVALID'/
C  Error 63
      DATA ERMSGS( 32)/' --LINETYPE IS LESS THAN OR EQUAL TO ZERO'/
C  Error 65
      DATA ERMSGS( 33)/' --LINEWIDTH SCALE FACTOR IS LESS THAN ZERO'/
C  Error 66
      DATA ERMSGS( 34)/' --POLYMARKER INDEX IS INVALID'/
C  Error 69
      DATA ERMSGS( 35)/' --MARKER TYPE IS LESS THAN OR EQUAL TO ZERO'/
C  Error 72
      DATA ERMSGS( 36)/' --TEXT INDEX IS INVALID'/
C  Error 75
      DATA ERMSGS( 37)/' --TEXT FONT IS EQUAL TO ZERO'/
C  Error 77
      DATA ERMSGS( 38)/' --CHARACTER EXPANSION FACTOR IS LESS THAN OR EQ
     +UAL TO ZERO'/
C  Error 78
      DATA ERMSGS( 39)/' --CHARACTER HEIGHT IS LESS THAN OR EQUAL TO ZER
     +O'/
C  Error 79
      DATA ERMSGS( 40)/' --LENGTH OF CHARACTER UP VECTOR IS ZERO'/
C  Error 80
      DATA ERMSGS( 41)/' --FILL AREA INDEX IS INVALID'/
C  Error 84
      DATA ERMSGS( 42)/' --STYLE (PATTERN OR HATCH) INDEX IS LESS THAN O
     +R EQUAL TO ZERO'/
C  Error 85
      DATA ERMSGS( 43)/' --SPECIFIED PATTERN INDEX IS INVALID'/
C  Error 87
      DATA ERMSGS( 44)/' --PATTERN SIZE VALUE IS NOT POSITIVE'/
C  Error 91
      DATA ERMSGS( 45)/' --DIMENSIONS OF COLOR ARRAY ARE INVALID'/
C  Error 92
      DATA ERMSGS( 46)/' --COLOR INDEX IS LESS THAN ZERO'/
C  Error 93
      DATA ERMSGS( 47)/' --COLOR INDEX IS INVALID'/
C  Error 96
      DATA ERMSGS( 48)/' --COLOR IS OUTSIDE RANGE ZERO TO ONE INCLUSIVE'
     +/
C  Error 100
      DATA ERMSGS( 49)/' --NUMBER OF POINTS IS INVALID'/
C  Error 101
      DATA ERMSGS( 50)/' --INVALID CODE IN STRING'/
C  Error 102
      DATA ERMSGS( 51)/' --GENERALIZED DRAWING PRIMITIVE IDENTIFIER IS I
     +NVALID'/
C  Error 103
      DATA ERMSGS( 52)/' --CONTENT OF GENERALIZED DRAWING PRIMITIVE DATA
     + RECORD IS INVALID'/
C  Error 120
      DATA ERMSGS( 53)/' --SPECIFIED SEGMENT NAME IS INVALID'/
C  Error 121
      DATA ERMSGS( 54)/' --SPECIFIED SEGMENT NAME IS ALREADY IN USE'/
C  Error 122
      DATA ERMSGS( 55)/' --SPECIFIED SEGMENT DOES NOT EXIST'/
C  Error 124
      DATA ERMSGS( 56)/' --SPECIFIED SEGMENT DOES NOT EXIST ON WORKSTATI
     +ON INDEPENDENT SEGMENT STORAGE'/
C  Error 160
      DATA ERMSGS( 57)/' --ITEM TYPE IS NOT ALLOWED FOR USER ITEMS'/
C  Error 161
      DATA ERMSGS( 58)/' --ITEM LENGTH IS INVALID'/
C  Error 162
      DATA ERMSGS( 59)/' --NO ITEM IS LEFT IN GKS METAFILE INPUT'/
C  Error 163
      DATA ERMSGS( 60)/' --METAFILE ITEM IS INVALID'/
C  Error 165
      DATA ERMSGS( 61)/' --CONTENT OF ITEM DATA RECORD IS INVALID FOR TH
     +E SPECIFIED ITEM TYPE'/
C  Error 166
      DATA ERMSGS( 62)/' --MAXIMUM ITEM DATA RECORD LENGTH IS INVALID'/
C  Error 167
      DATA ERMSGS( 63)/' --USER ITEM CANNOT BE INTERPRETED'/
C  Error 168
      DATA ERMSGS( 64)/' --SPECIFIED FUNCTION IS NOT SUPPORTED IN THIS L
     +EVEL OF GKS'/
C  Error 180
      DATA ERMSGS( 65)/' --SPECIFIED ESCAPE FUNCTION IS NOT SUPPORTED'/
C  Error 182
      DATA ERMSGS( 66)/' --CONTENTS OF ESCAPE DATA RECORD ARE INVALID'/
C  Error 300
      DATA ERMSGS( 67)/' --STORAGE OVERFLOW HAS OCCURRED IN GKS'/
C  Error 302
      DATA ERMSGS( 68)/' --INPUT/OUTPUT ERROR HAS OCCURRED WHILE READING
     +'/
C  Error 303
      DATA ERMSGS( 69)/' --INPUT/OUTPUT ERROR HAS OCCURRED WHILE WRITING
     +'/
C  Error 304
      DATA ERMSGS( 70)/' --INPUT/OUTPUT ERROR HAS OCCURRED WHILE SENDING
     + DATA TO A WORKSTATION'/
C  Error 305
      DATA ERMSGS( 71)/' --INPUT/OUTPUT ERROR HAS OCCURRED WHILE RECEIVI
     +NG DATA FROM A WORKSTATION'/
C  Error 306
      DATA ERMSGS( 72)/' --INPUT/OUTPUT ERROR HAS OCCURRED DURING PROGRA
     +M LIBRARY MANAGEMENT'/
C  Error 307
      DATA ERMSGS( 73)/' --INPUT/OUTPUT ERROR HAS OCCURRED WHILE READING
     + WORKSTATION DESCRIPTION TABLE'/
C  Error 308
      DATA ERMSGS( 74)/' --ARITHMETIC ERROR HAS OCCURRED'/
C  Error 2000
      DATA ERMSGS( 75)/' --ENUMERATION TYPE OUT OF RANGE'/
C  Error 2001
      DATA ERMSGS( 76)/' --OUTPUT PARAMETER SIZE INSUFFICIENT'/
C  Error 2002
      DATA ERMSGS( 77)/' --LIST OR SET ELEMENT NOT AVAILABLE'/
C  Error 2003
      DATA ERMSGS( 78)/' --INVALID DATA RECORD'/
C  Error -100
      DATA ERMSGS( 79)/' --UNKNOWN ERROR CODE'/
C  Error -101
      DATA ERMSGS( 80)/' --NO ADDITIONAL WORKSTATIONS MAY BE ACTIVATED'/
C  Error -102
      DATA ERMSGS( 81)/' --GKS SYSTEM ERROR--IMPROPER CONTINUATION SEQUE
     +NCE'/
C  Error -103
      DATA ERMSGS( 82)/' --THE NCAR GKS IMPLEMENTATION REQUIRES THAT WIS
     +S BE ACTIVE BEFORE A SEGMENT IS CREATED'/
C  Error -105
      DATA ERMSGS( 83)/' --ERROR OPENING DISK FILE'/
C  Error -106
      DATA ERMSGS( 84)/' --LOGICAL UNIT NUMBER FOR SEGMENT STORAGE CANNO
     +T BE THE SAME AS THAT FOR METAFILE OUTPUT'/
C  Error -107
      DATA ERMSGS( 85)/' --MAXIMUM NUMBER OF ERROR MESSAGES EXCEEDED'/
C  Error -108
      DATA ERMSGS( 86)/' --ILLEGAL PLACEMENT OF A CALL TO ASSIGN A PICTU
     +RE NAME'/
C  Error -109
      DATA ERMSGS( 87)/' --THIS FUNCTION NOT YET IMPLEMENTED FOR WORKSTA
     +TIONS OF CATEGORY OUTPUT'/
C  Error -110
      DATA ERMSGS( 88)/' --CHARACTER STRING TOO LARGE'/
C  Error -111
      DATA ERMSGS( 89)/' --SYSTEM ERROR'/
C  Error -112
      DATA ERMSGS( 90)/' --ONLY ONE METAFILE WORKSTATION CAN BE OPEN AT
     +A TIME'/
C  Error -200
      DATA ERMSGS( 91)/' --X driver error: memory allocation in processi
     +ng a character from a fontcap'/
C  Error -201
      DATA ERMSGS( 92)/' --X driver error: error opening fontcap'/
C  Error -202
      DATA ERMSGS( 93)/' --X driver error: error reading fontcap'/
C  Error -203
      DATA ERMSGS( 94)/' --X driver warning: invalid font index, using t
     +he default font'/
C  Error -204
      DATA ERMSGS( 95)/' --X driver error: memory allocation error in cr
     +eating device table'/
C  Error -205
      DATA ERMSGS( 96)/' --X driver error: too many open devices'/
C  Error -206
      DATA ERMSGS( 97)/' --X driver error: error in internal memory mana
     +gement'/
C  Error -207
      DATA ERMSGS( 98)/' --X driver error: error in allocating memory fo
     +r device dependent table'/
C  Error -208
      DATA ERMSGS( 99)/' --X driver error: DISPLAY environment variable
     +not set'/
C  Error -209
      DATA ERMSGS(100)/' --X driver error: error opening display'/
C  Error -210
      DATA ERMSGS(101)/' --X driver error: error getting window attribut
     +es'/
C  Error -211
      DATA ERMSGS(102)/' --X driver error: error creating pixmap'/
C  Error -212
      DATA ERMSGS(103)/' --X driver error: cell array has zero width or
     +height'/
C  Error -213
      DATA ERMSGS(104)/' --X driver error: memory allocation error in pr
     +ocessing cell array'/
C  Error -214
      DATA ERMSGS(105)/' --X driver error: error creating X image'/
C  Error -215
      DATA ERMSGS(106)/' --X driver error: memory allocation error in cr
     +eating X image'/
C  Error -216
      DATA ERMSGS(107)/' --X driver error: pixel size must be byte multi
     +ple'/
C  Error -300
      DATA ERMSGS(108)/' --PostScript error: Encapsulated PostScript can
     + have only one page'/
C  Error -301
      DATA ERMSGS(109)/' --PostScript error: error in allocating memory
     +for device dependent table'/
C  Error -302
      DATA ERMSGS(110)/' --PostScript error: error opening output file'/
C  Error -303
      DATA ERMSGS(111)/' --PostScript warning: requested character not a
     +vailable, asterisk plotted, use Plotchar'/
C  Error -217
      DATA ERMSGS(112)/' --X driver error: error in retrieving fontcap n
     +ame'/
C  Error -218
      DATA ERMSGS(113)/' --X driver error: invalid index into GKS workst
     +ation identifiers'/
C  Error -219
      DATA ERMSGS(114)/' --X driver error: color index exceeds maximum a
     +llowed'/
C  Error -220
      DATA ERMSGS(115)/' --X driver warning: color allocated is differen
     +t from the color requested'/
C  Error -221
      DATA ERMSGS(116)/' --X driver warning: unable to change color mode
     +l'/
C  Error 2200
      DATA ERMSGS(117)/' --C-binding specific error: buffer overflow in
     +input or inquiry function'/
C  Error 2201
      DATA ERMSGS(118)/' --C-binding specific error: start index out of
     +range'/
C  Error 2202
      DATA ERMSGS(119)/' --C-binding specific error: enumeration type ou
     +t of range'/
C  Error 2203
      DATA ERMSGS(120)/' --C-binding specific error: length of list is n
     +egative'/
C  Error 2204
      DATA ERMSGS(121)/' --C-binding specific error: cannot allocate mem
     +ory'/
C  Error 90
      DATA ERMSGS(122)/' --INTERIOR STYLE PATTERN IS NOT SUPPORTED ON TH
     +IS WORKSTATION'/
C  Error -113
      DATA ERMSGS(123)/' --Warning:  XMIN > XMAX or YMIN > YMAX in windo
     +w (non-standard NCAR extension of GKS)'/
C  Error -400
      DATA ERMSGS(124)/' --temporary file termination applies only to me
     +tafile workstations'/
C  Error -401
      DATA ERMSGS(125)/' --Invalid workstation identifier in metafile re
     +open'/
C  Error -402
      DATA ERMSGS(126)/' --Invalid connection identifier in metafile reo
     +pen'/
C  Error -403
      DATA ERMSGS(127)/' --Invalid workstation type in metafile reopen'/
C  Error -404
      DATA ERMSGS(128)/' --Workstation identifier already open in metafi
     +le reopen'/
C  Error -405
      DATA ERMSGS(129)/' --Maximum number of open workstations exceeded
     +in metafile reopen'/
C  Error -406
      DATA ERMSGS(130)/' --cannot have more than one NCAR CGM open at a
     +given time'/
C  Error -350
      DATA ERMSGS(131)/' --PDF error: error in allocating memory for dev
     +ice dependent table'/
C  Error -351
      DATA ERMSGS(132)/' --PDF error: error opening output file'/
C  Error -352
      DATA ERMSGS(133)/' --PDF warning: requested character not availab
     +le, asterisk plotted, use Plotchar'/
C  Error -353
      DATA ERMSGS(134)/' -- Only one PDF workstation can be open at a ti
     +me'/
C  Error -450
      DATA ERMSGS(135)/' -- Error opening xwd output file'/
C  Error -451
      DATA ERMSGS(136)/' -- Error opening PNG output file'/
C  Error -452
      DATA ERMSGS(137)/' -- Error reading pixmap for PNG output'/
C  Error -453
      DATA ERMSGS(138)/' -- Unsupported pixmap depth for PNG output'/
C  Error -354
      DATA ERMSGS(139)/' -- A dummy version of the interface function gg
     +kwdr has been called, most likely as a result of requesting output
     + other than an NCGM while having specified the -noX11 option.'/
C  Error -501
      DATA ERMSGS(140)/' -- cairo driver error: error in allocating memo
     +ry for device dependent table'/
C  Error -502
      DATA ERMSGS(141)/' -- cairo driver error: error opening output fil
     +e'/
C  Error -355
      DATA ERMSGS(142)/' -- PDF driver error, incorrect paper width speci
     +fication.'/
C  Error -356
      DATA ERMSGS(143)/' -- PDF driver error, incorrect paper height spec
     +ification.'/
C  Error -503
      DATA ERMSGS(144)/' -- cairo driver error, null or invalid argument
     + passed to function.'/
C  Error -504
      DATA ERMSGS(145)/' -- cairo driver error, unable to rename output
     *file.'/
C
      DATA GNAM(001),GNAM(002),GNAM(003)/'GOPKS' ,'GCLKS' ,'GOPWK' /
      DATA GNAM(004),GNAM(005),GNAM(006)/'GCLWK' ,'GACWK' ,'GDAWK' /
      DATA GNAM(007),GNAM(008),GNAM(009)/'GCLRWK','GRSGWK','GUWK'  /
      DATA GNAM(010),GNAM(011),GNAM(012)/'GSDS'  ,'GMSG'  ,'GESC'  /
      DATA GNAM(013),GNAM(014),GNAM(015)/'GPL'   ,'GPM'   ,'GTX'   /
      DATA GNAM(016),GNAM(017),GNAM(018)/'GFA'   ,'GCA'   ,'GGDP'  /
      DATA GNAM(019),GNAM(020),GNAM(021)/'GSPLI' ,'GSLN'  ,'GLSWSC'/
      DATA GNAM(022),GNAM(023),GNAM(024)/'GSPLCI','GSPMI' ,'GSMK'  /
      DATA GNAM(025),GNAM(026),GNAM(027)/'GSMKSC','GSPMCI','GSTXI' /
      DATA GNAM(028),GNAM(029),GNAM(030)/'GSTXFP','GSCHXP','GSCHSP'/
      DATA GNAM(031),GNAM(032),GNAM(033)/'GSTXCI','GSCHH' ,'GSCHUP'/
      DATA GNAM(034),GNAM(035),GNAM(036)/'GSTXP' ,'GSTXAL','GSFAI' /
      DATA GNAM(037),GNAM(038),GNAM(039)/'GSFAIS','GSFASI','GSFACI'/
      DATA GNAM(040),GNAM(041),GNAM(042)/'GSPA'  ,'GSPARF','GSASF' /
      DATA GNAM(043),GNAM(044),GNAM(045)/'GSPKID','GSPLR' ,'GSPMR' /
      DATA GNAM(046),GNAM(047),GNAM(048)/'GSTXR' ,'GSFAR' ,'GSPAR' /
      DATA GNAM(049),GNAM(050),GNAM(051)/'GSCR'  ,'GSWN'  ,'GSVP'  /
      DATA GNAM(052),GNAM(053),GNAM(054)/'GSVPIP','GSELNT','GSCLIP'/
      DATA GNAM(055),GNAM(056),GNAM(057)/'GSWKWN','GSWKVP','GCRSG' /
      DATA GNAM(058),GNAM(059),GNAM(060)/'GCLSG' ,'GRENSG','GDSG'  /
      DATA GNAM(061),GNAM(062),GNAM(063)/'GDSGWK','GASGWK','GCSGWK'/
      DATA GNAM(064),GNAM(065),GNAM(066)/'GINSG' ,'GSSGT' ,'GSVIS' /
      DATA GNAM(067),GNAM(068),GNAM(069)/'GSHLIT','GSSGP' ,'GSDTEC'/
      DATA GNAM(070),GNAM(071),GNAM(072)/'GINLC' ,'GINSK' ,'GINVL' /
      DATA GNAM(073),GNAM(074),GNAM(075)/'GINCH' ,'GINPK' ,'GINST' /
      DATA GNAM(076),GNAM(077),GNAM(078)/'GSLCM' ,'GSSKM' ,'GSVLM' /
      DATA GNAM(079),GNAM(080),GNAM(081)/'GSCHM' ,'GSPKM' ,'GSSTM' /
      DATA GNAM(082),GNAM(083),GNAM(084)/'GRQLC' ,'GRQSK' ,'GRQVL' /
      DATA GNAM(085),GNAM(086),GNAM(087)/'GRQCH' ,'GRQPK' ,'GRQST' /
      DATA GNAM(088),GNAM(089),GNAM(090)/'GSMLC' ,'GSMSK' ,'GSMVL' /
      DATA GNAM(091),GNAM(092),GNAM(093)/'GSMCH' ,'GSMPK' ,'GSMST' /
      DATA GNAM(094),GNAM(095),GNAM(096)/'GWAIT' ,'GFLUSH','GGTLC' /
      DATA GNAM(097),GNAM(098),GNAM(099)/'GGTSK' ,'GGTVL' ,'GGTCH' /
      DATA GNAM(100),GNAM(101),GNAM(102)/'GGTPK' ,'GGTST' ,'GWITM' /
      DATA GNAM(103),GNAM(104),GNAM(105)/'GGTITM','GRDITM','GIITM' /
      DATA GNAM(106),GNAM(107),GNAM(108)/'GEVTM' ,'GACTM' ,'GPREC' /
      DATA GNAM(109)                    /'GUREC'                   /
      END
