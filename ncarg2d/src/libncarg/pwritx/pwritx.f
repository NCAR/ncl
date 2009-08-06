C
C	$Id: pwritx.f,v 1.7 2009-08-06 20:55:14 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PWRITX(X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR)
C
C SUBROUTINE PWRITX (X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR)
C
C LATEST REVISION        October, 1988
C
C PURPOSE                PWRITX is a character plotting routine.  It
C                        produces high quality characters for annotating
C                        graphs, making movie titles, etc.
C
C USAGE                  CALL PWRITX (X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR)
C
C ARGUMENTS
C
C COMMON BLOCK PUSER     By default PWRITX uses the complex character
C                        set.  A complete list of the characters
C                        contained in this font appears as the
C                        output from the PWRITX demo driver in
C                        the Examples section of the current
C                        graphics manual.  PWRITX can be made to
C                        access one other font, called the duplex
C                        character set.  The characters in the
C                        duplex font are somewhat simpler in
C                        appearance (no serifs, fewer curves, etc.),
C                        but are still of high-quality.  Only the
C                        alphanumeric characters in the duplex
C                        font are different from the alphanumeric
C                        characters in the complex font.  All other
C                        characters remain the same.  To make
C                        PWRITX access the duplex character set, the user
C                        has to define in his main program a common block
C                        PUSER containing 1 integer variable. If this
C                        variable is initialized to 1 before the first
C                        call to PWRITX, the duplex character set is
C                        used by PWRITX.
C
C                        For example:
C
C                               .
C                               .
C                               .
C
C                        COMMON /PUSER/ MODE
C
C                               .
C                               .
C                               .
C
C                        MODE = 1
C
C                               .
C                               .
C                               .
C
C                        CALL PWRITX(.................)
C
C                               .
C                               .
C                               .
C
C                        NOTES
C                          The character set cannot be changed after
C                          the first call to PWRITX.  To produce
C                          examples of the duplex character set,
C                          run the demo driver for pwritx (as supplied)
C                          with MODE set to 1 as described above.
C                          The demo driver for PWRITX also serves
C                          for examples of PWRITX calls (particularly
C                          the final plot.)
C
C ON INPUT               X,Y
C                          Positioning coordinates for the characters to
C                          be drawn.  These are world coordinates.  See
C                          JCTR.
C
C                        IDPC
C                          Characters to be drawn and FUNCTION CODES
C                          (see below.)   IDPC is TYPE CHARACTER.
C
C                        NCHAR
C                          The number of characters in IDPC, including
C                          characters to be drawn and function codes.
C
C                        JSIZE
C                          Size of the character.
C                          . If between 0 and 3, it is 1., 1.5, 2.,
C                            or 3. times digitized character width.
C                            (See  FUNCTION CODES  below for these
C                             sizes.)
C                          . If greater than 3 it is the desired plotter
C                            address units for principal character
C                            height, i.e. principal characters will be
C                            JSIZE plotter address units high, and
C                            indexical and cartographic characters will
C                            be scaled proportionally, such that
C
C                             Indexical = (13/21)*JSIZE  PLA units high
C                          Cartographic = (9/21)*JSIZE  PLA units high
C
C                        JOR
C                          Character string orientation in degrees
C                          counter-clockwise from the positive X axis.
C
C                        JCTR
C                          Centering option.
C                          = 0  (X,Y) is the center of the entire
C                                     string.
C                          = -1 (X,Y) is the center of the left edge of
C                                     the first character.
C                          = 1  (X,Y) is the center of the right edge of
C                                     the last character.
C
C ON OUTPUT              All arguments are unchanged.
C
C FUNCTION CODES         Function codes may be included in the character
C                        string IDPC to change font, case, etc. within a
C                        string of text.  All function instructions must
C                        be enclosed in apostrophes.
C                        No punctuation is needed between functions
C                        except for a comma between adjacent numbers;
C                        however, commas may be used between functions
C                        to improve readability.  The following are the
C                        only legal function codes.  Any other
C                        characters in a function string will be ignored
C                        except that an error message will be printed
C                        and, if more than 10 errors occur within a
C                        string, control will be returned to the main
C                        program. At the first call to PWRITX, size,type
C                        and case are Principal, Roman and Upper.
C
C                        PLA = plotter address  (for resolution 10)
C                        UPA = user plotter address  (resolution as
C                              defined by user )
C
C                        A.  FONT DEFINITIONS
C                            R  Roman type characters
C                            G  Greek type characters
C
C                        B.  SIZE DEFINITIONS
C                            P  Principal size, digitized to be 21 PLA
C                               units high.  The total character
C                               including white space is 32 PLA units
C                               high.  A carriage return or a Y
C                               increment will space down 32 PLA units.
C                               A blank or an X increment will space
C                               across 16 PLA units.  Note:  Characters
C                               vary in width.
C                            I  Indexical size, digitized to be 13 PLA
C                               units high (20 PLA units high
C                               including white space).  A carriage
C                               return or a Y increment is 20 PLA units.
C                               Blanks or X increments are 12 PLA units.
C                            K  Cartographic size, digitized to be 9 PLA
C                               units high (14 PLA units high
C                               including white space).  Carriage return
C                               or Y increments are 14 PLA units. Blanks
C                               or X increments are 8 PLA units.
C
C                        C.  CASE DEFINITIONS
C                            U or Un.  Upper case
C                               If U is followed by a number n (not
C                               separated by a comma) then n characters
C                               will be drawn in upper case, subsequent
C                               characters will be in lower case.  (The
C                               Un option is particularly useful for
C                               capitalizing sentences.)
C                            L or Ln.  Lower case
C                               If L is followed by a number n, then n
C                               characters will be drawn in lower case
C                               and subsequent characters will be in
C                               upper case.
C
C                        D.  LEVEL DEFINITIONS
C                            S or Sn.  Superscript level.
C                            B or Bn.  Subscript level.
C                            N or Nn.  Normal level.
C
C                               When super or subscripting, the
C                               character size will change depending on
C                               the previous character drawn.  Principal
C                               base characters will be subscripted or
C                               superscripted with indexical characters,
C                               with a 10 PLA unit shift (scaled to
C                               JSIZE) up or down.  Indexical and
C                               cartographic base characters will be sub
C                               or superscripted with cartographic
C                               characters with a 7 PLA unit shift.
C
C                               The case of the indexing characters will
C                               generally be the same as that of the bas
C                               character unless otherwise specified.
C                               Exception: a lower case indexical base
C                               will be super or subscripted with upper
C                               case cartographic, as the cartographic
C                               type has no lower case alphabetic or
C                               numeric characters available.
C
C                               If S,B or N is followed by a number n,
C                               then n characters will be drawn as
C                               specified above, after which character
C                               size, case and position will be reset to
C                               that of the base character.
C                               If n is negative, its absolute value
C                               will be used instead (n cannot be 0.)
C                               Do not overlap level definitions given
C                               for a specified number of characters.
C                               The N option returns character case and
C                               size to that of the base but maintains
C                               the current character position.
C
C                               Example:  'U1'T'S1'EST
C                                                          E
C                                         Will be written TST
C                                         'U1'T'S'E'N'ST
C                                                          E
C                                         Will be written T ST
C
C                        E.  COORDINATE DEFINITIONS (descriptions assume
C                                                    normal UPA unit
C                                                    space.)
C                            H,Hn,HnQ.  Increment in the X direction.
C                               If this option appears without a number
C                               n, n will be taken to be 1.  Hn will
C                               shift the present X position n UPA
C                               units.  If n is positive the shift is to
C                               the right, if n is negative the shift is
C                               to the left.  If Hn is followed by a Q,
C                               the X position will be incremented by n
C                               character widths (i.e., n blanks) either
C                               right or left.
C                            V,Vn,VnQ.  Increment in the Y direction.
C                               If this option appears without a number
C                               n, n will be taken to be 1.  Vn will
C                               shift the present Y position n UPA
C                               units.  If n is positive the shift is
C                               up, if n is negative the shift is down.
C                               If Vn is followed by a Q, the Y position
C                               will be incremented by n lines up or
C                               down.
C                            X,Xn.  Set X.
C                               If X appears without a number n, this
C                               will act as a do-nothing statement.
C                               Otherwise, the character position in the
C                               X direction will be set to the UPA
C                               coordinate n, so that the next character
C                               drawn will be centered on n and
C                               subsequent characters will be drawn from
C                               this position.
C                            Y,Yn.  Set Y.
C                               This works the same as set X.
C                            C  Carriage return.
C                               A carriage return will be done before
C                               the next character is plotted.
C
C                        F.  DIRECTION DEFINITIONS
C                            D,Dn.  Write down, rather than across the
C                                   frame.
C                               If D appears without an n or if n=0, all
C                               characters will be written down, until
C                               an 'A' function is encountered.  If D is
C                               followed by a number n, n characters
C                               will be written down and subsequent
C                               characters will be written across the
C                               frame.
C                               If n is negative, the absolute value of
C                               n is used instead.
C                            A  Write across.
C                               Escape from the D option.
C
C                        G.  DIRECT CHARACTER ACCESS
C                               NNN.  Numeric character.
C                               Character number NNN will be drawn.
C                               NNN is base 8.
C
C NOTE                   .  All characters in a given call are drawn in
C                           the same intensity. If JSIZE .LE. 2,
C                           characters are in low intensity, otherwise
C                           they are in high intensity.  Return to the
C                           main program is always in high intensity.
C                        .  On other than the first entry to PWRITX,
C                           font, case, etc. are in the state they
C                           were in when the program exited from the
C                           the last PWRITX call.
C                        .  Font, size, and case are reset to previous
C                           values if they were only set for a specified
C                           number of characters in the previous call.
C                        .  The previous case is always reset to upper
C                           for each new call to PWRITX.
C                        .  The direction is always reset to across
C                           for each new call to PWRITX.
C                        .  Numbers for direct character access must
C                           not be signed. All other numbers can be
C                           signed.
C
C PORTABILITY            FORTRAN 77
C
C REQUIRED RESIDENT      SQRT, SIN, COS.
C ROUTINES
C
C REQUIRED GKS LEVEL     0A
C
C ENTRY POINTS           PWRITX, GTNUM, GTNUMB, XTCH, PWRXBD, GTSIGN,
C                        GTDGTS, HTABLE, GETHOL, CCHECK, DCECK, PWRX
C                        MKMSK, PWRXBDX
C
C COMMON BLOCKS          PWRSV1, PSAV1, PSAV3, PUSER, PINIT, PINIT1,
C                        HOLTAB, PWRC0, PWRC1, PWRC2
C
C REQUIRED LIBRARY       The ERPRT77 package and the SPPS.
C
C I/O                    Plots characters.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C HISTORY               -Originally implemented to make the Hershey
C                        character sets more accessable.
C                       -Made portable in 1978.
C                        for use on all computer systems which support
C                        plotters with up to 15 bit resolution.
C                       -Made to be FORTRAN 77 and GKS compatible,
C                        August, 1984.
C
C INTERNAL PARAMETERS    NAME   DEFAULT  FUNCTION
C                        ----   -------  --------
C
C                        All values below are for plotter address units.
C
C                        SPRIH  32.      Height of principal characters
C                        SPRIW  16.      Width of principal characters
C                        SINDH  20.      Height of indexical chars.
C                        SINDW  12.      Width of indexical chars.
C                        SCARH  14.      Height of cartographic chars.
C                        SCARW  8.       Width of cartographic chars.
C                        SSPR   10.      Shift in case of super or
C                                        subscripting for principal
C                                        characters.
C                        SSIC   7.       Shift in case of super or
C                                        subscripting for indexical
C                                        characters.
C
C **********************************************************************
C
C IMPLEMENTATION INSTRUCTIONS
C
C 1.  Create the PWRITX binary database from the card-image files
C     PWRITXC1, PWRITXC2, PWRITXD1, PWRITXD2 supplied.  To do this,
C     read the instructions in file PWRITXNT and execute that program.
C 2.  Supply 3 machine dependent subprograms for bit manipulations,
C     namely ISHIFT(I1,I2), IOR(I1,I2), and IAND(I1,I2).
C     These subprograms can be the same routines used to implement the
C     NCAR GKS plot package.
C 3.  Set up a binary input file from which character data may be read
C     (user input) and supply its number below.
C 4.  Set the binary input file where PWRITX may read its data.
C **********************************************************************
C
C
C
C
C
C
C   D E C L A R A T I O N S
C
      CHARACTER       IDPC*(*)
C
C   IDPC contains the characters to be drawn, and the function codes.
C
C   IDPC must be of type character on input.
C
C
C  SAVE DATA VALUES FOR LATER PASSES
C
      COMMON/PWRSV1/NBWD,NUM15,INDLEN,IDDLEN,NUNIT
C
C
C NOTE THE SIZE OF IDD AND IND MAY BE MODIFED TO CONTAIN THE
C  NUMBER OF ELEMENTS EQUAL TO THE VALUE OF IDDLEN AND INDLEN
C  COMPUTED BELOW.
C
      COMMON/PWRC0/IDD(8625),IND(789)
C
C PWRC0 AND PWRC1 ARE FOR COMMUNICATION WITH SUBROUTINE XTCH.
C PWRC0 IS TO PASS VALUES TO XTCH AND PWRC1 IS TO PASS VALUES BACK.
      COMMON /PWRC1/ LC(150)
C PWRC2 IS FOR COMMUNICATION WITH SUBROUTINE XTCH AND BLOCKDATA PWRXBDX.
      COMMON /PWRC2/ INDZER
C VARIABLES WHICH HAVE TO BE SAVED FOR SUBSEQUENT CALLS TO PWRITX AND
C HAVE TO BE INITIALIZED.
C PSAV1 IS ALSO CONTAINED IN PWRXBDX.
      COMMON /PSAV1/ FINIT,IFNT,IC,IT,ITO,RLX,RLY,RLOX,RLOY
C VARIABLES WHICH HAVE TO BE SAVED BUT NOT INITIALIZED.
C PSAV3 IS NOT CONTAINED IN ANY OTHER ROUTINE.
      COMMON /PSAV3/ IAPOST, IQU
C
C COMMUNICATION WITH THE USER.
      COMMON /PUSER/ MODE
C
      LOGICAL NFLAG
      LOGICAL FINIT
C
C  Character constants
C
      CHARACTER*1 IAPOST,IQU
C
C  Storage for current character of string
C
      CHARACTER*1 NC
C
C  Value of the character in the character-to-hollerith table
C
      INTEGER HOLIND
C
C   C O N S T A N T   D E F I N I T I O N S
C
C INFORMATION ABOUT THE SIZE OF THE FILES OF INTEGERS SUPPLIED BY NCAR
C TOGETHER WITH PWRITX.
C THESE CONSTANTS ARE NEVER USED IN THE CODE BUT REFERRED TO IN COMMENTS
C
C NUMBER OF CARD IMAGES NEEDED TO REPRESENT ARRAY IND.
      DATA ICNUM1 /49/
C NUMBER OF CARD IMAGES NEEDED TO REPRESENT ARRAY IDD.
      DATA ICNUM2 /575/
C
C DEFINE INDICES FOR FONT, SIZE, AND CASE DEFINITIONS.
C
C THESE INDICES ARE USED AS DISPLACEMENTS INTO THE ARRAY IND AND THEY
C DEFINE UNIQUELY FOR EACH CHARACTER TOGETHER WITH ITS REPRESENTATION
C IN DPC AN ENTRY IN THE ARRAY IND. THIS ENTRY CONTAINS A POINTER
C WHICH POINTS TO THE DIGITIZATION OF THE SYMBOL AS DEFINED BY FONT,
C SIZE, CASE, AND CHARACTER.
C
C DEFINE INDEX FOR ROMAN AND GREEK FONT.
      DATA INDROM, INDGRE /0,384/
C DEFINE INDEX FOR PRINCIPAL, INDEXICAL, AND CARTOGRAPHIC SIZE.
      DATA INDPRI, INDIND, INDCAR /0,128,256/
C DEFINE INDEX FOR UPPER AND LOWER CASE.
      DATA INDUPP, INDLOW /0,64/
C
C DEFINE CHARACTER SIZES IN PLOTTER ADDRESS UNITS (FOR RESOLUTION 10)
C
C FOR PRINCIPAL CHARACTER SIZE - HEIGHT,WIDTH
      DATA SPRIH, SPRIW /32.,16./
C FOR INDEXICAL SIZE - HEIGTH,WIDTH
      DATA SINDH,SINDW /20.,12./
C FOR CARTOGRAPHIC SIZE - HEIGTH,WIDTH
      DATA SCARH, SCARW /14.,8./
C
C SHIFTING FOR SUPER OR SUBSCRIPTING.
C
C NUMBER OF PLOTTER ADDRESS UNITS SHIFTED FOR PRINCIPAL CHARACTERS.
      DATA SSPR /10./
C NUMBER OF PLOTTER ADDRESS UNITS SHIFTED FOR INDEXICAL OR CARTOGRAPHIC
C CHARACTERS.
      DATA SSIC /7./
C
C CONSTANT USED TO CHANGE DEGREES INTO RADIANS.
C DEGRAD = 2*3.14/360
      DATA DEGRAD /0.01745329/
C T2 = PI/2
      DATA  T2 /1.5707963/
C
C
C **********************************************************************
C
C IMPLEMENTATION-DEPENDENT CONSTANTS
C
C
C THE UNIT NUMBER WHERE THE BINARY FILE SUPPLIED TO PWRITX CAN BE READ.
C
C     DATA INBIN /0/
      DATA INBIN /3/
C
C
C END OF IMPLEMENTATION-DEPENDENT CONSTANTS
C
C
C **********************************************************************
C
C
C
C
C VARIABLES AND CONSTANTS USED IN PWRX:
C
C IND - POINTERS INTO ARRAY IDD
C IDD - DIGITIZED CHARACTERS
C LC - CONTAINS DIGITIZATION OF 1 CHARACTER, 1 DIGITIZATION UNIT PER
C      WORD.
C DISPLACEMENTS INTO THE ARRAY IND
C   INDROM - FOR ROMAN FONT
C   INDGRE - FOR GREEK FONT
C   INDPRI - FOR PRINCIPAL CHARACTER SIZE
C   INDIND - FOR INDEXICAL CHARACTER SIZE
C   INDCAR - FOR CARTOGRAPHIC CHARACTER SIZE
C   INDUPP - FOR UPPER CASE
C   INDLOW - FOR LOWER CASE
C CHARACTER SIZE AS DEFINED BY FUNCTION CODE (PLOTTER ADDRESS UNITS)
C   SPRIH - HEIGHT OF PRINCIPAL CHARACTERS
C   SPRIW - WIDTH OF PRINCIPAL CHARACTERS
C   SINDH - HEIGHT OF INDEXICAL CHARACTERS
C   SINDW - WIDTH OF INDEXICAL CHARACTERS
C   SCARH - HEIGHT OF CARTOGRAPHIC CHARACTERS
C   SCARW - WIDTH OF CARTOGRAPHIC CHARACTERS
C SHIFTING FOR SUPER OR SUBSCRIPTING (PLOTTER ADDRESS UNITS)
C   SSPR - SHIFT FOR PRINCIPAL CHARACTERS
C   SSIC - SHIFT FOR INDEXICAL OR CARTOGRAPHIC CHARACTERS
C II  - THE POSITION OF THE CHARACTER CURRENTLY BEING PROCESSED
C NC  - THE CHARACTER CURRENTLY BEING PROCESSED, RIGHT JUSTIFIED,
C       ZERO FILLED, ASCII REPRESENTATION.
C NCHOL - CHARACTER NC IN HOLLERITH REPRESENTATION.
C INDPOI - THE POSITION OF THE POINTER IN IND WHICH POINTS TO THE
C          DIGITIZATION OF CHARACTER NC IN THE ARRAY IDD.
C NUM - SIGNED INTEGER AS EXTRACTED FROM CHARACTER STRING IDPC
C IPASS = 1 : PASS TO CENTER THE STRING
C       = 2 : PASS TO DRAW OUT THE CHARACTERS
C L1  - DISTANCE FROM THE CENTER OF A CHARACTER TO ITS LEFT END
C       (AS A NEGATIVE NUMBER)
C L2  - DISTANCE FROM THE CENTER OF A CHARACTER TO ITS RIGHT END
C       (AS A POSITIVE NUMBER)
C IERR- ERROR COUNT
C NFLAG = .FALSE.  NORMAL
C       = .TRUE.  DIRECT CHARACTER ACCESS
C IT  - REPRESENTS CHARACTER SIZE
C       0   = PRICIPAL  128 = INDEXICAL   256 = CARTOGRAPHIC
C ITO - REPRESENTS PREVIOUS CHARACTER SIZE
C IC  - REPRESENTS LOWER CASE , UPPER CASE
C       =0 UPPER CASE
C       =64 LOWER CASE
C ICO - REPRESENTS PREVIOUS CASE
C IFNT = 0  ROMAN
C      = 384  GREEK
C IFLG = 0  NORMAL
C      = 1  DISTANCE BETWEEN CHARACTERS IS NOT TAKEN FROM DIGITIZATION
C           POSITION INSTEAD DEFINED BY HIGHER PRIORITY FUNCTION CODE
C           (C,Y,X)
C N   - NUMBER OF CHARACTERS IN INPUT STRING
C IDFLG = 0  WRITE ACROSS THE FRAME
C       = 1  WRITE DOWN THE FRAME
C FINIT = .TRUE. ONE TIME INITIALIZATION HAS BEEN DONE
C       = .FALSE. ONE TIME INITIALIZATION HAS NOT BEEN DONE YET
C RLX - CHARACTER HEIGHT ACCORDING TO FUNCTION CODE AND SIZE PARAMETER
C RLY - CHARACTER WIDTH ACCORDING TO FUNCTION CODE AND SIZE PARAMETER
C RLOX - PREVIOUS CHARACTER HEIGHT
C RLOY - PREVIOUS CHARACTER WIDTH
C N4  = 0  NO CHARACTERS TO BE WRITTEN DOWN
C     OTHERWISE  NUMBER OF CHARACTERS TO BE WRITTEN DOWN
C IN4 - NUMBER OF CHARACTER CURRENTLY BEING WRITTEN DOWN
C N3  - NUMBER AS COMPUTED BY GTNUM FOR COORDINATE DEFINITIONS OR
C       NUMBER AS COMPUTED BY GTNUMB FOR NUMERIC CHARACTER DEFINITIONS.
C N3T1 - TEMPORARY STORAGE FOR VARIABLE N3. ONLY USED IN SECTION FOR
C        FUNCTION CODE H.
C N3T2 - LIKE N3T1, BUT FOR FUNCTION CODE V.
C N2  - NUMBER OF CHARACTERS TO BE WRITTEN AS SUPERSCRIPT OR SUBSCRIPT
C IN2 - NUMBER OF CHARACTER CURRENTLY BEING WRITTEN AS SUPERSCRIPT OR
C       SUBSCRIPT
C N1  - NUMBER OF CHARACTERS TO BE WRITTEN IN LOWER CASE OR UPPER CASE
C IN1 - NUMBER OF CHARACTER CURRENTLY BEING WRITTEN IN LOWER CASE OR
C       UPPER CASE
C NF  =1  NORMAL LEVEL
C     =0  FLAG FOR SUPERSCRIPTING OR SUBSCRIPTING
C NIX,NIY - IN PASS 1 THE PARAMETER COORDINATES X AND Y IN METACODE
C           ADDRESS UNITS. IN PASS 2 THE ACTUAL COORDINATES OF THE FIRST
C           CHARACTER IN THE STRING IN METACODE ADDRESS UNITS.
C IX,IY - SAME AS NIX,NIY AS LONG AS NO CARRIAGE RETURN IS ENCOUNTERED.
C         AFTER EACH CARRIAGE RETURN THEY CONTAIN THE COORDINATES OF
C         THE FIRST CHARACTER IN THE NEW LINE.
C NXX,NYY - THE PARAMETER COORDINATES X AND Y IN METACODE ADDRESS UNITS.
C           NIX AND NIY CAN BE USED INSTEAD.
C XX,YY - THE CENTER OF THE CHARACTER TO BE DRAWN (CONSIDERED) NEXT
C XXO,YYO - THE CENTER OF THE LAST CHARACTER DRAWN (CONSIDERED)
C           BEFORE SUPER OR SUBSCRIPTING STARTED
C MX  - THE DISTANCE BETWEEN THE CHARACTER CURRENTLY BEING PROCESSED AND
C       THE CENTER OF THE NEXT CHARACTER TO BE PROCESSED,
C       IN DIGITIZATION UNITS
C MXO  LAST DISTANCE COMPUTED BEFORE SUPER OR SUBSCRIPTING STARTED
C NN  - INDEX VARIABLE IN THE LOOP FOR STRIKING OUT CHARACTERS
C JX,JY - CONTAIN COORDINATES WHEN CHARACTERS ARE BEING STROKED OUT
C MXEND,MYEND - TEMPORARY STORAGE FOR COORDINATES IN TERMINAL PORTION
C IAPOST - THE CHARACTER APOSTROPHE '
C IQU - THE CHARACTER Q
C NCCG - ONLY USED FOR COMPUTED GO TO IN FUNCTION MODE CONTROL SECTION.
C NUMDUN - THE NUMBER OF DIGITIZATION UNITS CONTAINED IN ARRAY LC
C          =0  NO DIGITIZATION FOUND
C INDZER - AN INDICATION FOR AN ALL-0-BITS DIGITIZATION UNIT. INDICATES
C          THAT NEXT MOVE IS PEN-UP.
C
C
C
C          I N I T I A L I Z A T I O N
C
C
C INITIALIZATION  -  ONCE PER LOADING OF ROUTINE
C
C
C CHECK IF INITIALIZATION IS ALREADY DONE.
      IF (FINIT) GOTO 100
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL PWRXBD
C Modification for UNIX Version.
      CALL OPENPW(3)
C End of modification for UNIX Version.
C Modification for UNIX Version.
      CALL OPENPW(3)
C End of modification for UNIX Version.
C Modification for UNIX Version.
      CALL OPENPW(3)
C End of modification for UNIX Version.
C
C  FIND THE NUMBER OF BITS PER WORD
C
      NBWD = I1MACH(5)
C
C  COMPUTE THE NUMBER OF 15-BIT PARCELS PER WORD
C
      NUM15 = NBWD/15
C
C  CALCULATE THE AMOUNT OF ARRAY IND REQUIRED ON THIS MACHINE
C
      INDLEN = (ICNUM1*16-1)/NUM15+1
C
C  CALCULATE THE AMOUNT OF ARRAY IDD REQUIRED ON THIS MACHINE
C
      IDDLEN = (ICNUM2*16*15-1)/NBWD+1
C
C CHARACTERS USED FOR COMPARISONS.
C
      IAPOST = ''''
      IQU    = 'Q'
C
C Define the error file logical unit number
C
      NUNIT = I1MACH(4)
C
C Initialize the hash table for obtaining the hollerith code of a
C character.
C
      CALL HTABLE
C
C READ IN POINTER ARRAY AND CHARACTER DIGITIZATIONS
C
      IF (MODE .EQ. 1) GOTO 98
C
C READ IN COMPLEX SET.
      REWIND INBIN
      READ(INBIN) (IND(I),I=1,INDLEN)
      READ(INBIN) (IDD(I),I=1,IDDLEN)
      REWIND INBIN
      GOTO 97
C
   98 CONTINUE
C
C READ IN DUPLEX SET.
      REWIND INBIN
      READ(INBIN) DUMREA
      READ(INBIN) DUMREA
      READ(INBIN) (IND(I),I=1,INDLEN)
      READ(INBIN) (IDD(I),I=1,IDDLEN)
      REWIND INBIN
   97 CONTINUE
C
C SHORT TEST IF ARRAYS IDD AND IND ARE LOADED CORRECTLY.
      IF (MODE .EQ. 1) CALL DCHECK(LCHERR)
      IF (MODE .NE. 1) CALL CCHECK(LCHERR)
      IF (LCHERR .NE. 0) THEN
           CALL SETER
     +     ('PWRITX - ARRAY IND OR IDD NOT LOADED CORRECTLY',LCHERR,2)
      ENDIF
   99 CONTINUE
C
C SET FLAG THAT INITIALIZATION IS DONE.
      FINIT = .TRUE.
C
  100 CONTINUE
C
C
C INITIALIZATION  -  ONCE PER CALL TO ROUTINE
C
C
C RETRIEVE RESOLUTION.
C
      CALL GETUSV('XF',LXSAVE)
      CALL GETUSV('YF',LYSAVE)
C
C DEFINE FACTOR TO CHANGE PLOTTER ADDRESS UNITS INTO METACODE UNITS.
C
      METPLA = ISHIFT(1,15-10)
C
C DEFINE FACTOR TO CHANGE USER PLOTTER ADDRESS UNITS INTO METACODE UNITS
C
      METUPA = ISHIFT(1,15-LXSAVE)
C
C COPY ARGUMENTS INTO LOCAL VARIABLES
C
      ICTR = JCTR
      IOR = JOR
      ISIZE = JSIZE
      N = NCHAR
C INSIDE PWRITX WE WORK ONLY WITH METACODE ADDRESS UNITS
      CALL FL2INT (X,Y,NIX,NIY)
C
C SAVE THE FOLLOWING CONSTANTS FOR PASS 2
C
      ID1 = IFNT
      ID2 = IC
      ID3 = IT
      ID4 = ITO
      R1 = RLX
      R2 = RLY
      R3 = RLOX
      R4 = RLOY
C
      IPASS = 1
C ONLY 1 PASS REQUIRED IF THE CHARACTER STRING IS CENTERED TO THE LEFT
      IF (ICTR .EQ. -1) IPASS = 2
C
C INITIALIZATION  -  ONCE PER PASS THROUGH ROUTINE
C
  102 CONTINUE
C POSITION PEN
      CALL PLOTIT(NIX,NIY,0)
C STORE CURRENT POSITION OF PEN
C IX AND IY ARE USED FOR CARRIAGE RETURNS. THEY DEFINE THE BEGINNING
C OF THE LINE CURRENTLY BEING WRITTEN.
      IX = NIX
      IY = NIY
      RX = NIX
      RY = NIY
      XXO = NIX
      YYO = NIY
      XX = NIX
      YY = NIY
C  Z  MULTIPLICATION OF DIGITIZED SIZE
      Z = 1. + REAL(ISIZE)/2.
      IF (ISIZE .EQ. 3) Z = 3.
      IF (ISIZE .GT. 3) Z = REAL(ISIZE)/21.
C TRANSLATE SIZE FROM PLOTTER ADDRESS UNITS TO METACODE ADDRESS UNITS.
      Z = Z*REAL(METPLA)
C SET INTENSITY
      INT = 0
      IF (Z .GT. 64.) INT = 1
C  T - ORIENTATION IN RADIANS
      T = REAL(IOR)*DEGRAD
C COMPUTE VALUES USED TO POSITION THE SUBSEQUENT CHARACTER DEPENDING
C ON ANGLE AND FUNCTION CODE.
C NORMAL CASE.
      SIT = SIN(T)
      COT = COS(T)
C USED FOR SUBSCRIPT, CARRIAGE RETURN, Y INCREMENT
      SIM = SIN(T-T2)
      COM = COS(T-T2)
C USED FOR SUPERSCRIPT AND Y INCREMENT
      SIP = SIN(T+T2)
      COP = COS(T+T2)
C MAKE POSITIONING ALSO DEPENDENT ON SIZE PARAMETER
      CT = Z*COT
      ST = Z*SIT
C ADJUST CHARACTER HEIGHT AND WIDTH ACCORDING TO SIZE PARAMETER
      RLX = RLX*Z
      RLY = RLY*Z
C
      MX = 0
C START PROCESSING FROM BEGINNING OF STRING
      II = 0
C FUNCTION CODES DEFINED ONLY FOR A SPECIFIED NUMBER OF CHARACTERS ARE
C NOT VALID ANY MORE. UNRESTRICTED FUNCTION CODES SET IN PREVIOUS CALLS
C TO PWRX ARE STILL VALID.
C CASE.
      N1 = 0
      IN1 = 0
C LEVEL.
      N2 = 0
      IN2 = 0
C DIRECTION.
      N4 = 0
      IN4 = 0
C THE FOLLOWING FUNCTIONS ARE RESET FOR EACH CALL TO PWRX.
C RESET PREVIOUS CASE TO UPPER.
      ICO = 0
C WRITE ACROSS FRAME.
      IDFLG = 0
C THE FOLLOWING INTERNAL FLAGS ARE RESET FOR EACH CALL TO PWRX.
C PROCEED NORMALLY IN X-DIRECTION
      IFLG = 0
C NO DIRECT CHARACTER ACCESS
      NFLAG = .FALSE.
      NF = 0
C
C
C  END INITIALIZATION
C
C-----------------------------------------------------------------------
C
C
C  S T A R T   T O   P R O C E S S   D A T A   S T R I N G
C
C IF IN DIRECT CHARACTER ACCESS MODE, GO TO FUNCTION PROCESSOR SECTION
  104 IF (NFLAG) GO TO 125
C LET II POINT TO THE NEXT CHARACTER IN IDPC
      II = II + 1
C CHECK IF NO CHARACTER LEFT IN STRING.
      IF (II .GT. N) GOTO 120
C RETRIEVE CHARACTER IN NC
      NC = IDPC(II:II)
C IF CHARACTER INDICATES BEGINNING OF FUNCTION CODE, GO TO FUNCTION
C PROCESSOR SECTION.
      IF (NC .EQ. IAPOST) GO TO 125
C FIND ENTRY IN POINTER TABLE POINTING TO DIGITIZATION OF CHARACTER NC
      CALL GETHOL (NC,NCHOL,IER2)
C ERROR IF NC IS NOT A FORTRAN CHARACTER OR AN APOSTROPHE
      IF (IER2 .NE. 0) GOTO 153
      INDPOI = NCHOL + IFNT + IC + IT
C CHECK IF THIS CASE IS ONLY FOR SPECIFIED NUMBER OF CHARACTERS
      IF (N1 .EQ. 0) GO TO 106
C INCREMENT COUNTER FOR CHARACTERS WRITTEN IN THIS CASE
      IN1 = IN1+1
C CHECK IF THIS CHARACTER IS TO BE WRITTEN DOWN
  106 IF (N4 .EQ. 0) GO TO 109
C        ***** PROCESS WRITING DOWN *****
C INCREMENT COUNTER FOR CHARACTERS WRITTEN DOWN
      IN4 = IN4+1
C CHECK IF ALL CHARACTERS SPECIFIED HAVE BEEN WRITTEN DOWN
      IF (IN4 .GT. N4) GO TO 108
C
      RX = XX+RAD*COM
      RY = YY+RAD*SIM
C SET FLAG TO NOT PROCEED IN X DIRECTION
  107 IFLG = 1
C
      MX = 0
      GO TO 109
C SET FLAG THAT ALL CHARACTERS SPECIFIED HAVE BEEN WRITTEN DOWN
  108 N4 = 0
C        ***** END OF WRITING DOWN PROCESSING *****
C
C  PROCESS ONE CHARACTER
C  ---------------------
C
C RETURN DIGITIZATION OF CHARACTER IN ARRAY LC
  109 CONTINUE
      CALL XTCH (INDPOI,IPASS,NUMDUN)
C IF NO DIGITIZATION WAS FOUND, JUST GET NEXT CHARACTER.
      IF (NUMDUN .EQ. 0) GOTO 104
C DEFINE DISTANCE FROM CENTER TO LEFT END OF CHARACTER.
      L1 = LC(1)
C DEFINE DISTANCE FROM CENTER TO RIGHT END OF CHARACTER.
      L2 = LC(2)
C IN THE NORMAL CASE, JUMP
      IF (IFLG .EQ. 0) GO TO 111
C FOR FUNCTION CODES C, Y, AND X NO BLANK SPACE IS LEFT
      L1 = 0
C RESET FLAG TO NORMAL
      IFLG = 0
  111 CONTINUE
C FIND THE CENTER OF THE NEXT CHARACTER TO BE DRAWN.
      MX = MX - L1
      XX = RX+REAL(MX)*CT
      YY = RY+REAL(MX)*ST
C DO NOT STROKE OUT CHARACTERS IN PASS 1.
      IF (IPASS .EQ. 1) GO TO 117
C
C STROKE OUT CHARACTER
C
      NN = 1
      LC(1) = INDZER
      DO 113 NN = 3,NUMDUN,2
C CHECK FOR INDICATION OF PEN-UP MOVE
      IF (LC(NN) .EQ. INDZER) GOTO 113
C FIND COORDINATES FOR NEXT PEN MOVE
      JX = XX + (REAL(LC(NN))*CT - REAL(LC(NN+1))*ST + .5)
      JY = YY + (REAL(LC(NN))*ST + REAL(LC(NN+1))*CT + .5)
C CHECK FOR PEN-UP OR PEN-DOWN
      IF (LC(NN-2) .NE. INDZER) GOTO 116
C PEN-UP MOVE.
      CALL PLOTIT (JX,JY,0)
      GOTO 113
C PEN-DOWN MOVE.
  116 CALL PLOTIT (JX,JY,1)
C
  113 CONTINUE
C
C RESET VARIABLES AFTER ONE CHARACTER HAS BEEN PROCESSED
C
C
  117 CONTINUE
C FIND THE RIGHT END OF THE LAST CHARACTER DRAWN.
      MX = MX + L2
C      ***** FIND CASE DEFINTION OF NEXT CHARACTER *****
C CHECK IF THE CURRENT CASE IS ONLY FOR A SPECIFIED NUMBER.
      IF (N1 .EQ. 0) GO TO 118
C CHECK IF SPECIFIED NUMBER OF CHARACTERS HAS ALREADY BEEN DRAWN.
      IF (IN1 .LT. N1) GO TO 118
C RESET INDICATORS FOR CASE SPECIFICATION.
      N1 = 0
      IN1 = 0
C SET CASE TO PREVIOUS CASE.
      IC = ICO
C CHECK IF THE CURRENT LEVEL IS ONLY FOR SPECIFIED NUMBER OF CHARACTERS.
  118 IF (N2 .EQ. 0) GO TO 104
C CHECK IF SPECIFIED NUMBER OF CHARACTERS HAS ALREADY BEEN DRAWN.
      IN2 = IN2+1
      IF (IN2 .LT. N2) GO TO 104
C SET LEVEL TO PREVIOUS LEVEL.
      IT = ITO
C
      RX = RNX
      RY = RNY
C SET CHARACTER HEIGHT AND WIDTH TO PREVIOUS VALUES
      RLX = RLOX
      RLY = RLOY
C STOP SUPER OR SUBSCRIPTING.
      N2 = 0
C CHECK IF WE WERE IN SUPER OR SUBSCRIPTING MODE BEFORE.
      IF (NF .EQ. 1) GO TO 119
C YES WE WERE. RETURN TO PREVIOUS CASE.
      IC = ICO
C
      XX = XXO
      YY = YYO
C
  119 MX = MXO
C RESET FLAG FOR SUPER OR SUBSCRIPTING TO NORMAL.
      NF = 0
C
      L2 = L2O
C
      GO TO 104
C
C TERMINAL PORTION AFTER WHOLE STRING HAS BEEN PROCESSED
C ------------------------------------------------------
C
  120 CONTINUE
C RESET CHARACTER HEIGHT AND WIDTH
      RLX = RLX/Z
      RLY = RLY/Z
      IF (IPASS .EQ. 2) THEN
          CALL PLOTIT(0,0,0)
          RETURN
      ENDIF
C
C GET READY FOR PASS 2
C
      IPASS = 2
C GET COORDINATES PASSED TO PWRX IN METACODE ADDRESS UNITS
      CALL FL2INT (X,Y,NXX,NYY)
C THE COORDINATES FOR THE END OF THE CHARACTER STRING IF THE FIRST
C CHARACTER HAD BEEN POSITIONED AT THE COORDINATES PASSED TO PWRX.
      MXEND = XX + REAL(L2)*CT
      MYEND = YY + REAL(L2)*ST
C COMPUTE LENGTH OF CHARACTER STRING TO BE DRAWN
      R     = SQRT(REAL(MXEND-NXX)**2 + REAL(MYEND-NYY)**2)
C CONSIDER CENTERING OPTION
      WFAC = -1.
      IF (ICTR .EQ. 0) WFAC = -0.5
C COMPUTE DISPLACEMENT FROM COORDINATES PASSED TO PWRX
      RCT = WFAC*R*COT
      RST = WFAC*R*SIT
C CHECK FOR WRITING DOWN OR ACROSS FRAME
      IF (IDFLG .EQ. 1) GO TO 122
C GET COORDINATES WHERE FIRST CHARACTER IN STRING HAS TO BE CENTERED
      XDIF = MXEND - NXX
      NIX = REAL(NXX) + XDIF*WFAC
      YDIF = MYEND - NYY
      NIY = REAL(NYY) + YDIF*WFAC
      GO TO 123
C GET COORDINATES WHERE FIRST CHARACTER IN STRING HAS TO BE CENTERED
  122 NIX = REAL(NXX) + RST
      NIY = REAL(NYY) - RCT
C
C REINITIALIZE VARIABLES FOR PASS 2
C SET ROMAN,UPPER CASE, PRINCIPAL
C SET PREVIOUS CHARACTER SIZE ALSO TO PRINCIPAL
  123 IFNT = ID1
      IC = ID2
      IT = ID3
      ITO = ID4
      RLX = R1
      RLY = R2
      RLOX = R3
      RLOY = R4
      GO TO 102
C
C-----------------------------------------------------------------------
C
C   F U N C T I O N   P R O C E S S O R   S E C T I O N
C
C
C
C
C
C
C
C
C
C
C  FUNCTION MODE CONTROL SECTION
C  -----------------------------
C
  125 IERR = 0
      NFLAG = .FALSE.
  126 CONTINUE
      II = II + 1
      IF (II .GT. N) GOTO 120
      NC = IDPC(II:II)
C                        Check for an invalid function character, and pr
C                        an error message if one is found.
      CALL GETHOL(NC, HOLIND, NERR)
      IF
     +   (( HOLIND .EQ. 0) .OR. (HOLIND .EQ. 47) .OR.
     +   (( HOLIND .GT. 34 ) .AND. ( HOLIND .LT. 46)) .OR.
     +    ( HOLIND .GT. 52) )
     +THEN
           IERR = IERR + 1
           WRITE( NUNIT, 1001) II, IERR
           IF (IERR .GT. 9) GOTO 120
           GOTO 126
      ENDIF
C                        Check to see if the character is an octal digit
      IF (( NC .GE. '0') .AND. (NC .LE. '7')) THEN
C
C NUMERIC CHARACTER DEFINITION
C ----------------------------
C
C SET FLAG FOR NUMERIC CHARACTER DEFINITION
           NFLAG = .TRUE.
C RETRIEVE OCTAL NUMBER IN N3
           CALL GTNUMB (IDPC,N,II,N3)
C IF END OF CHARACTER STRING REACHED GO TO TERMINAL PORTION
           IF (II .GT. N) GO TO 120
C CHECK IF THIS CASE IS ONLY FOR A SPECIFIED NUMBER OF CHARACTERS.
           IF (N1 .NE. 0) THEN
                IN1 = IN1+1
                IF (IN1 .GT. N1) THEN
                     N1 = 0
C RETURN TO PREVIOUS CASE.
                     IC = ICO
                ENDIF
           ENDIF
C
C SEE IF THE TERMINATING CHARACTER WAS AN APOSTROPHE OR NOT.  IF SO,
C SET 'NFLAG' FALSE TO SUSPEND FUNCTION-CODE SCANNING.
C
           NC = IDPC(II:II)
           IF (NC .EQ. IAPOST) NFLAG = .FALSE.
           INDPOI = N3
           GOTO 106
      ENDIF
C
      IF (NC .EQ. IAPOST) GOTO 104
      IF (NC .EQ. ',') GOTO 126
C
C                           Goto the appropriate section to process the
C                           alphabetic function code characters
C
      GOTO
C        A , B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,
     + (145,140,146,144,127,127,129,147,131,127,132,134,127,
C        N,  O,  P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z
     +  141,127,130,126,128,135,127,133,148,127,149,150,127)
     + HOLIND
C
C                           Error handling section for invalid alphabeti
C                           function code.
C
 127  IERR = IERR + 1
      WRITE( NUNIT, 1001) II, IERR
      IF (IERR .GT. 9) GOTO 120
      GOTO 126
C
C FONT DEFINITION
C ---------------
C
C  ROMAN  R  22B
C
  128 CONTINUE
C DEFINE INDEX INTO POINTER TABLE
      IFNT = INDROM
C GO TO GET NEXT FUNCTION CODE
      GO TO 126
C
C  GREEK  G  07B
C
C EQUIVALENT TO ROMAN
  129 CONTINUE
      IFNT = INDGRE
      GO TO 126
C
C SIZE DEFINITION
C ---------------
C
C  PRINCIPAL SIZE  P  20B
C
  130 CONTINUE
C DEFINE INDEX INTO POINTER TABLE
      IT = INDPRI
C SET CHARACTER WIDTH
      RLX = SPRIW*Z
C SET CHARACTER HEIGHT
      RLY = SPRIH*Z
C GO TO GET NEXT FUNCTION CODE
      GO TO 126
C
C  INDEXICAL SIZE  I  11B
C
C EQUIVALENT TO PRINCIPAL SIZE DEFINITION
  131 CONTINUE
      IT = INDIND
      RLX = SINDW*Z
      RLY = SINDH*Z
      GO TO 126
C
C  CARTOGRAPHIC SIZE  K  13B
C
C EQUIVALENT TO PRINCIPAL SIZE DEFINITION
  132 CONTINUE
      IT = INDCAR
      RLX = SCARW*Z
      RLY = SCARH*Z
      GO TO 126
C
C CASE DEFINITION
C ---------------
C
C  UPPER CASE  U  25B
C
  133 CONTINUE
C Set previous case to lower, current case to
C upper, no characters drawn in this case yet.
      ICO = INDLOW
      IC  = INDUPP
      IN1 = 0
C Set the number of characters to be drawn in upper case.
C If no number is found, then all remaining characters
C should be in upper case.
      CALL GTNUM (IDPC,N,II,N1)
      IF (N1 .EQ. 0) N1 = N + 1 - II
      GO TO 126
C
C  LOWER CASE  L  14B
C
  134 CONTINUE
C Set previous case to upper, current case to
C lower, no characters drawn in this case yet.
      ICO = INDUPP
      IC  = INDLOW
      IN1 = 0
C Set the number of characters to be drawn in upper case.
C If no number is found, then all remaining characters
C should be in lower case.
      CALL GTNUM (IDPC,N,II,N1)
      IF (N1 .EQ. 0) N1 = N + 1 - II
      GO TO 126
C
C LEVEL DEFINITION
C ----------------
C
C  SUPERSCRIPT  S  23B
C
  135 CONTINUE
C DEFINE ANGLE FROM BASE TO FIRST SUPERSCRIPTED CHARACTER.
      TS = SIP
      TC = COP
      GOTO 136
C
C SUBSCRIPT  B  02B
C
  140 CONTINUE
C DEFINE ANGLE FROM BASE TO FIRST SUBSCRIPTED CHARACTER.
      TS = SIM
      TC = COM
C
C FOR SUPERSCRIPT AND SUBSCRIPT.
C
  136 CONTINUE
C DEFINE DISTANCE FROM BASE TO FIRST SUPER OR SUBSCRIPTED CHARACTER.
      RAD = SSPR*Z
      IF (IT .NE. INDPRI) RAD = SSIC*Z
C REMEMBER POSITION OF BASE CHARACTER.
      XXO = XX
      YYO = YY
C
C FOR SUPERSCRIPT, SUBSCRIPT, AND NORMAL.
C
  137 CONTINUE
C REMEMBER POSITION OF LAST CHARACTER BEFORE LEVEL CHANGE.
      RNX = RX
      RNY = RY
C FIND THE POSITION OF THE FIRST CHARACTER AFTER LEVEL CHANGE.
      RX = XX+RAD*TC
      RY = YY+RAD*TS
C RETRIEVE FOR HOW MANY CHARACTERS THE LEVEL HAS TO BE CHANGED.
      IN2 = 0
      CALL GTNUM (IDPC,N,II,N2)
      N2 = ABS(N2)
C REMEMBER SIZE OF CHARACTERS IN PREVIOUS LEVEL.
      RLOX = RLX
      RLOY = RLY
C
      IF (TS .EQ. SIT) GO TO 142
      MXO = MX
C
      MX = L2
      L2O = L2
      NF = 0
C REMEMBER CASE DEFINITION OF BASE CHARACTER.
      ICO = IC
C REMEMBER SIZE DEFINITION OF BASE CHARACTER
      ITO = IT
C ***** FIND CHARACTER SIZE AFTER LEVEL CHANGE *****
C CHECK IF BASE CHARACTER HAS PRINCIPAL SIZE DEFINITION.
      IF (IT .NE. INDPRI) GO TO 139
C BASE CHARACTER HAS PRINCIPAL SIZE. THEN SUPER OR SUBSCRIPT HAS
C INDEXICAL SIZE
  138 CONTINUE
      IT = INDIND
      RLX = SINDW*Z
      RLY = SINDH*Z
      GO TO 126
C BASE CHARACTER HAS INDEXICAL OR CARTOGRAPHIC SIZE. THEN SUPER OR
C SUBSCRIPT HAS CARTOGRAPHIC SIZE.
  139 CONTINUE
      IT = INDCAR
      RLX = SCARW*Z
      RLY = SCARH*Z
C CASE IS SET TO UPPER SINCE CARTOGRAPHIC DOES NOT HAVE LOWER CASE
C CHARACTERS.
      IC = INDUPP
      GO TO 126
C ***** CHARACTER SIZE FOR AFTER LEVEL CHANGE DEFINED *****
C
C  NORMAL  N  16B
C
  141 CONTINUE
C ANGLE BETWEEN LAST SUPER OR SUBSCRIPTED CHARACTER AND FIRST NORMAL
C CHARACTER.
      TS = SIT
      TC = COT
C DISTANCE BETWEEN LAST SUPER OR SUBSCRIPTED CHARACTER AND FIRST
C NORMAL CHARACTER.
      RAD = 0
      IF (IT .EQ. INDIND) RAD = SSPR*Z
      IF (IT .EQ. INDCAR) RAD = SSIC*Z
C GET THE CASE OF THE BASE CHARACTER.
      IC = ICO
C GET THE POSITION OF THE BASE CHARACTER.
      XX = XXO
      YY = YYO
      NF = 1
      GO TO 137
C
  142 CONTINUE
C ***** RESET CHARACTER SIZE WHEN RETURNING TO NORMAL LEVEL *****
      IF (IT .EQ. INDPRI) GO TO 126
C
      IF (IT .EQ. INDCAR .AND. ITO .EQ. INDCAR) GO TO 126
C
      IF (IT .EQ. INDIND) GO TO 143
C
      ITO = IT
      GO TO 138
C
  143 CONTINUE
C RESET CHARACTER SIZE TO PRINCIPAL.
      ITO = IT
      IT = INDPRI
      RLX = SPRIW*Z
      RLY = SPRIH*Z
C *****CHARACTER SIZE IS RESET TO NORMAL *****
      GO TO 126
C
C DIRECTION DEFINITION
C --------------------
C
C    WRITE DOWN  D  04B
C
  144 RAD = RLY
C NO CHARACTER IS CURRENTLY BEING WRITTEN DOWN
      IN4 = 0
C SET FLAG FOR WRITING DOWN
      IDFLG = 1
C GET NUMBER OF CHARACTERS TO BE WRITTEN DOWN
      CALL GTNUM (IDPC,N,II,N4)
C IF N APPEARS WITHOUT AN N OR IF N=0 , WRITE ALL CHARACTERS DOWN
C UNTIL AN 'A' IS ENCOUNTERED OR PWRX RETURNS.
      IF (N4 .EQ. 0) N4 = N
C IF N IS NEGATIVE USE ITS ABSOLUTE VALUE INSTEAD.
      N4 = ABS(N4)
C GO TO INTERPRET CHARACTER NC (ALREADY FETCHED)
      GO TO 126
C
C  ESCAPE FROM DOWN  A  01B
C
  145 CONTINUE
C 0 CHARACTERS TO BE WRITTEN DOWN
      N4 = 0
C *****
C RESET FLAG TO WRITING ACROSS.
      IDFLG = 0
C *****
      GO TO 126
C
C COORDINATE DEFINITION
C ---------------------
C
C  CARRIAGE RETURN  C  03B
C
  146 CONTINUE
C DEFINE DISTANCE TO NEXT LINE.
      RAD = RLY
      MX = 0
C DEFINE POSITION OF FIRST CHARACTER IN NEXT LINE.
      RX = REAL(IX)+RAD*COM
      RY = REAL(IY)+RAD*SIM
      XX = RX
      YY = RY
C DEFINE THE BEGINNING OF THE LINE CURRENTLY BEING WRITTEN.
      IX = RX
      IY = RY
      GO TO 126
C
C  INCREMENT X  H OR H Q  10B
C
  147 CONTINUE
C RETRIEVE NUMBER AFTER H IN N3
      CALL GTNUM (IDPC,N,II,N3)
C IF NO NUMBER IS PROVIDED IT WILL BE TAKEN TO BE 1.
      IF (N3 .EQ. 0) N3 = 1
      N3T1 = N3
C TRANSFORM UPA UNITS INTO METACODE UNITS.
      N3 = N3*METUPA
C IF HN IS FOLLOWED BY A Q THE SHIFT IS N CHARACTER WIDTHS
      NC = IDPC(II:II)
      IF (NC .EQ. IQU) N3 = RLX*REAL(N3T1)
C
      RX = RX + REAL(N3)
C
      XX = RX+REAL(MX)*CT
      YY = RY+REAL(MX)*ST
      GO TO 126
C
C  INCREMENT Y  V OR V Q  26B
C
  148 CONTINUE
C RETRIEVE NUMBER AFTER V IN N3
      CALL GTNUM (IDPC,N,II,N3)
C IF NO NUMBER IS PROVIDED IT WILL BE TAKEN TO BE 1.
      IF (N3 .EQ. 0) N3 = 1
      N3T2 = N3
C TRANSFORM USER PLOTTER ADDRESS UNITS INTO METACODE ADDRESS UNITS.
      N3 = N3*METUPA
C IF VN IS FOLLOWED BY A Q THE SHIFT IS N CHARACTER HEIGHTS
      NC = IDPC(II:II)
      IF (NC .EQ. IQU) N3 = RLY*REAL(N3T2)
C
      RY = RY + REAL(N3)
      XX = RX + REAL(MX)*CT
      YY = RY + REAL(MX)*ST
      GO TO 126
C
C  SET X  (SPECIFY CRT UNIT)  X  30B
C
  149 CONTINUE
C RETRIEVE NUMBER AFTER X IN N3
      CALL GTNUM (IDPC,N,II,N3)
C TRANSLATE USER PLOTTER ADDRESS UNITS INTO METACODE ADDRESS UNITS.
      N3 = N3*METUPA
C
      RX = N3
      XX = RX
      YY = RY + REAL(MX)*ST
C PREPARE TO NOT PROCEED IN X DIRECTION FOR NEXT CHARACTER DRAWN.
      MX = 0
      IFLG = 1
C
      GO TO 126
C
C  SET Y  (SPECIFY CRT UNIT)  Y  31B
C
  150 CONTINUE
C RETRIEVE NUMBER AFTER Y IN N3
      CALL GTNUM (IDPC,N,II,N3)
C TRANSLATE USER PLOTTER ADDRESS UNITS INTO METACODE ADDRESS UNITS.
      N3 = N3*METUPA
      RY = N3
      XX = RX+REAL(MX)*CT
      YY = RY
      GO TO 126
C
C END FUNCTION MODE PROCESSING
C
C-----------------------------------------------------------------------
C
C     ESCAPE CHARACTER ERROR
C
  153 CONTINUE
      WRITE(NUNIT,1002)II
      GO TO 104
C
 1001 FORMAT(' THE ',I5,'TH CHARACTER IN IDPC IS THE ',I3,
     +           'TH ILLEGAL FUNCTION CODE.')
 1002 FORMAT(' THE ',I5,'TH CHARACTER IN IDPC IS ILLEGAL')
C
      END
