C
C	$Id: plchhq.F,v 1.1.1.1 1992-04-17 22:32:17 ncargd Exp $
C
C***********************************************************************
C P A C K A G E   P L O T C H A R   -   I N T R O D U C T I O N
C***********************************************************************
C
C This file contains implementation instructions, a write-up, and the
C code for the package PLOTCHAR.  Banners like the one above delimit the
C major sections of the file.  The code itself is separated into four
C sections:  character-writing routines, parameter-access routines,
C internal routines, and the block data routines which determine the
C default values of internal parameters.
C
C***********************************************************************
C P A C K A G E   P L O T C H A R   -   I M P L E M E N T A T I O N
C***********************************************************************
C
C The PLOTCHAR package is written in FORTRAN-77 and should be quite
C easy to implement.  To implement the full package requires four
C steps:
C
C 1.  Supply 3 machine-dependent functions for bit manipulations -
C     ISHIFT(I1,I2), IOR(I1,I2), and IAND(I1,I2) - and one function
C     to provide machine-dependent quantities - I1MACH(I).  These
C     functions are the same ones used to implement the NCAR GKS plot
C     package.
C
C 2.  Create the PWRITX binary database from the card-image files
C     PWRITXC1, PWRITXC2, PWRITXD1, and PWRITXD2.  To do this, read
C     the instructions in the file PWRITXNT and execute the program
C     given there.
C
C 3.  Create the binary fontcaps that are used by the translators,
C     in a manner described in "NCAR Graphics Generic Package
C     Installer's Guide".  This will require running the program
C     FONTC once per fontcap.
C
C 4.  Supply system-dependent versions of three routines which are
C     used to access both the PWRITX binary database and the binary
C     fontcaps.  These routines are as follows:
C
C     SUBROUTINE PCFOPN (IBNU,NFNT)
C     SUBROUTINE PCFRED (IBNU,NFNT,IBUF,LBUF)
C     SUBROUTINE PCFCLS (IBNU,NFNT)
C
C     PCFOPN is called to open a particular binary file, PCFRED is
C     called to read the next binary record from a file, and PCFCLS
C     is called to close the file.  IBNU is the number of a unit to
C     be used (if necessary) in manipulating the files.  The default
C     value of IBNU is 3, which is defined in the BLOCK DATA routine
C     PCBLDA; it may be necessary to change this value.  NFNT is the
C     number of the desired font: 0 selects the PWRITX database and
C     a non-zero value selects one of the other fonts.  IBUF is an
C     integer array, of length LBUF, into which the next record of
C     data is to be read.  On a Unix system, the following versions
C     may be used (BOFRED, BINRED, and BCLRED are support routines for
C     the FORTRAN version of the metafile translator "cgmtrans"):
C
C     SUBROUTINE PCFOPN (IBNU,NFNT)
C     CHARACTER*128 PARANM,FILENM
C     DATA PARANM / 'DBDIR' /
C       IF (NFNT.EQ.0) THEN
C         CALL GETNGP (PARANM,FILENM)
C         DO 101 I=1,119
C           IF (FILENM(I:I).EQ.' ') THEN
C             FILENM(I:I+9)='/pwritdata'
C             GO TO 102
C           END IF
C 101     CONTINUE
C         GO TO 103
C 102     OPEN (UNIT=IBNU,FILE=FILENM,STATUS='OLD',
C    +                  FORM='UNFORMATTED',ERR=103)
C         REWIND IBNU
C       ELSE IF (NFNT.GE.1.AND.NFNT.LE.20) THEN
C         CALL BOFRED (IBNU,NFNT,IOST,ISTA)
C         IF (ISTA.NE.0) THEN
C           PRINT * , 'PCFOPN - ERROR OPENING FONT ',NFNT
C           STOP
C         END IF
C       ELSE
C         PRINT * , 'PCFOPN - REQUEST FOR UNAVAILBLE FONT ',NFNT
C         STOP
C       END IF
C       RETURN
C 103   PRINT * , 'PCFOPN - ERROR OPENING PWRITX DATA FILE ',FILENM
C       STOP
C     END
C
C     SUBROUTINE PCFRED (IBNU,NFNT,IBUF,LBUF)
C       DIMENSION IBUF(LBUF)
C       IF (NFNT.EQ.0) THEN
C         READ (IBNU,ERR=101) (IBUF(I),I=1,LBUF)
C       ELSE
C         CALL BINRED (IBNU,LBUF,IBUF,IOST,ISTA)
C         IF (ISTA.NE.0) THEN
C           PRINT * , 'PCFRED - ERROR READING FONT ',NFNT
C           STOP
C         END IF
C       END IF
C       RETURN
C 101   PRINT * , 'PCFRED - ERROR READING PWRITX DATABASE'
C       STOP
C     END
C
C     SUBROUTINE PCFCLS (IBNU,NFNT)
C       IF (NFNT.EQ.0) THEN
C         CLOSE (UNIT=IBNU,STATUS='KEEP',ERR=101)
C       ELSE
C         CALL BCLRED (IBNU,IOST,ISTA)
C         IF (ISTA.NE.0) THEN
C           PRINT * , 'PCFCLS - ERROR CLOSING FONT ',NFNT
C           STOP
C         END IF
C       END IF
C       RETURN
C 101   PRINT * , 'PCFCLS - ERROR CLOSING PWRITX DATABASE'
C       STOP
C     END
C
C It is not necessary to implement the full package in order to get a
C great deal of use out of it.  You can leave out steps 2, 3, and 4
C and change the default value of the internal parameter IQUF, in the
C BLOCK DATA routine PCBLDA, below, from a 0 to a 1.  This will leave
C the package still usable in "medium-quality" mode, as described in
C the write-up.
C
C***********************************************************************
C P A C K A G E   P L O T C H A R   -   W R I T E - U P
C***********************************************************************
C
C LATEST REVISION        June, 1990
C
C PURPOSE                To plot characters of high, medium, or low
C                        quality.
C
C USAGE                  CALL PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C                        CALL PLCHMQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C                        CALL PLCHLQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C                        Each of these calls draws the character string
C                        specified by the character variable CHRS at the
C                        position (XPOS,YPOS).  The size of characters
C                        to be used, the angle at which they are to be
C                        written, and the way in which the whole string
C                        is to be positioned relative to (XPOS,YPOS)
C                        are specified by the arguments SIZE, ANGD, and
C                        CNTR.
C
C                        PLCHHQ plots high-quality characters selected
C                        from the PWRITX database or from any of the
C                        fontcap-defined databases which are used by
C                        the translators.  Characters from one or more
C                        of these databases may be mixed together in
C                        the graphic output resulting from a single
C                        call to PLCHHQ.  The PWRITX database includes
C                        variable-width Roman and Greek characters in
C                        three different sizes and in upper and lower
C                        case; many special characters are available.
C                        The fontcap databases include Roman, Greek,
C                        Cyrillic, and Gothic characters, italics, math
C                        symbols, music symbols, astronomical symbols,
C                        and miscellaneous other symbols.
C
C                        When PLCHHQ is called, subscripting and
C                        superscripting may be specified.  Text-extent
C                        vector magnitudes can be computed, and you can
C                        recover them to find out how large the string
C                        drawn was (or how large it will be).  Internal
C                        parameters and "function codes" in CHRS give
C                        precise control over the aspect ratio of the
C                        characters and their positioning with respect
C                        to one another.  An internal parameter is
C                        provided allowing you to force the drawing of
C                        characters by calling PLCHMQ or PLCHLQ; the
C                        advantage of this is that subscripting and
C                        superscripting can still be done easily, but
C                        CPU time and metafile size are reduced.
C
C                        PLCHMQ plots characters of medium quality.  It
C                        offers upper and lower case Roman characters
C                        in a single size, as well as a number of other
C                        characters from the standard set of printable
C                        ASCII characters.  A single internal parameter
C                        is provided, allowing you to change the aspect
C                        ratio of the characters.
C
C                        PLCHLQ plots characters of the lowest (or,
C                        perhaps, the least predictable) quality. It
C                        calls GKS primitives, thus creating less
C                        metacode, but making you dependent on the
C                        capabilities of the translator.
C
C                        CALL PCGETC (WHCH,CVAL)
C                        CALL PCGETI (WHCH,IVAL)
C                        CALL PCGETR (WHCH,RVAL)
C
C                        CALL PCSETC (WHCH,CVAL)
C                        CALL PCSETI (WHCH,IVAL)
C                        CALL PCSETR (WHCH,RVAL)
C
C                        Each of the above calls, depending on the name,
C                        either gets the value of an internal parameter
C                        or sets an internal parameter.  The parameter
C                        may be of type Character, Integer, or Real.
C
C                        See the write-ups of individual routines,
C                        below.
C
C ENTRY POINTS           PCBDMQ, PCBLDA, PCCCHK, PCDCHK, PCEXCD, PCGETC,
C                        PCGETI, PCGETR, PCGPTR, PCGTDI, PCGTPI, PCMQLQ,
C                        PCSETC, PCSETI, PCSETR, PCSORT, PLCHHQ, PLCHLQ,
C                        PLCHMQ
C
C COMMON BLOCKS          PCPFMQ, PCPRMS, PCSVEM.
C
C REQUIRED LIBRARY       The ERPRT77 package and SPPS.
C ROUTINES
C
C I/O                    Graphical output is generated.  PLCHHQ reads
C                        data from one or more binary files.
C
C ERROR CONDITIONS       An unrecognized character may result in the
C                        plotting of a blank or a five-pointed star
C                        (depending on which routine is called).
C                        PLCHHQ may call the error routine SETER, if
C                        the PWRITX database has somehow been corrupted.
C
C PRECISION              Single.
C
C LANGUAGE               FORTRAN.
C
C HISTORY                Created in February and March of 1988, by
C                        massively revising existing routines.  PLCHHQ
C                        may be viewed as a much improved and expanded
C                        version of PWRITX, PLCHMQ as an improved
C                        version of PWRITY, and PLCHLQ as a modified
C                        version of the SPPS routine WTSTR.
C
C                        Access to fontcap-defined fonts was added in
C                        May and June of 1990.  "Zoom" function codes,
C                        inter-character spacing controls, and a few
C                        other details were added at the same time.
C
C
C TIMING                 Difficult to quantify.  A test of PLCHHQ using
C                        lots of function codes and producing 18 rather
C                        complicated frames ran .7232 seconds on our
C                        Cray X-MP and produced 90,720 metacode bytes.
C                        Setting the character quality flag to 1 to
C                        force the use of PLCHMQ reduced the time to
C                        .2393 seconds and produced 43,200 metacode
C                        bytes.  Setting the character quality flag to
C                        2 to force the use of PLCHLQ reduced the time
C                        to .1320 seconds and produced 28,800 metacode
C                        bytes.
C
C PORTABILITY            The code is written in FORTRAN-77 and should
C                        be very portable.  Binary files containing
C                        data for PLCHHQ must be generated and read
C                        routines must be supplied.  See implementation
C                        instructions at the beginning of this file.
C
C REQUIRED GKS LEVEL     0A
C
C-----------------------------------------------------------------------
C S U B R O U T I N E   P L C H H Q   -   D E S C R I P T I O N
C-----------------------------------------------------------------------
C
C SUBROUTINE PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C PURPOSE                PLCHHQ is a character plotting routine.  It
C                        plots high quality characters for annotating
C                        graphs, making movie titles, etc.  PLCHHQ uses
C                        the same database as PWRITX, but it can also
C                        use characters from any of the fontcap-defined
C                        databases, it has an improved interface, and
C                        it has many more capabilities.
C
C USAGE                  CALL PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C                        writes a string of characters specified by the
C                        FORTRAN character string CHRS at the position
C                        (XPOS,YPOS).  The size of the characters, the
C                        angle at which they are written, and the way
C                        in which the string is positioned relative to
C                        (XPOS,YPOS) are specified by the arguments
C                        SIZE, ANGD, and CNTR.
C
C                        The routines PCSETC, PCSETI, and PCSETR may be
C                        used to set the values of internal parameters
C                        affecting the behavior of PLCHHQ.  The routines
C                        PCGETC, PCGETI, and PCGETR may be used to get
C                        the current values of internal parameters.
C                        See the section "INTERNAL PARAMETERS", below.
C
C                        By default, PLCHHQ plots characters from a
C                        subset of the PWRITX database called the
C                        "complex" character set, which contains 564
C                        different characters.  Obviously, since the
C                        printable ASCII and EBCDIC character sets
C                        are not that large and the set of standard
C                        FORTRAN characters is even smaller, there
C                        cannot be a one-to-one mapping between the
C                        characters in CHRS and the characters plotted.
C                        To access them all, some scheme is needed for
C                        making the characters of CHRS do double duty;
C                        this scheme is described in the next paragraph.
C
C                        The 564 characters of the complex set are
C                        divided into 12 groups of 47 characters apiece.
C                        (These characters are produced by the PLCHHQ
C                        demo driver and appear in the Examples section
C                        of the graphics manual.)  Each of the 12 groups
C                        is characterized by font (Roman or Greek), size
C                        (principal, indexical, or cartographic), and
C                        case (upper or lower).  Within each group, each
C                        character is associated with one of the 47
C                        characters A-Z, 0-9, +, -, *, /, (, ), $, =,
C                        blank, comma, and period.  Initially, as the
C                        scan of the character string CHRS starts, the
C                        font, size, and case are assumed to be Roman,
C                        principal, and upper, respectively.  Thus, if
C                        CHRS = 'A', a Roman A, of principal size, in
C                        upper case, will be written.  As the scan
C                        proceeds from left to right, the font, size,
C                        and case may be changed by the occurrence of
C                        "function codes", which are just characters
C                        enclosed by pairs of function-code signal
C                        characters (by default, a colon, but the user
C                        has the option of changing to some other
C                        character).  For example, if CHRS = ':L:A', a
C                        Roman A, of principal size, but in lower case,
C                        will be written, and, if CHRS = ':IGL:A', an
C                        indexical-size, lower-case Greek alpha will be
C                        written.
C
C                        Using function codes allows all of the 564
C                        characters in the complex set to be accessed
C                        using just the 48 standard FORTRAN characters.
C                        Forty-five other characters, including the 26
C                        lower-case alphabetic characters and the 19
C                        special characters exclamation point, double
C                        quote, pound sign, percent sign, ampersand,
C                        colon, semicolon, less than, greater than,
C                        question mark, at sign, left square bracket,
C                        backward slash, right square bracket, left
C                        curly bracket, virgule, right curly bracket,
C                        tilde, and apostrophe may be used, as well.
C                        Each of these results in the selection of the
C                        correct character from the complex dataset.
C                        Note that using these 45 characters must be
C                        viewed with some suspicion, since the FORTRAN
C                        77 standard indicates that the effect of using
C                        them in a character string is "processor
C                        dependent"; however, I would be very surprised
C                        to find a system on which using them caused
C                        any problems.  Three other ASCII characters -
C                        the hat, the underscore, and the backwards
C                        apostrophe - are recognized by PLCHHQ, but,
C                        since these characters do not exist in the
C                        complex set, each will cause the drawing of a
C                        five-pointed star.  The same five-pointed star
C                        will be drawn for unrecognized characters.
C
C                        PLCHHQ can be made to use a different 564-
C                        character subset of the PWRITX database,
C                        called the "duplex" character set.  Duplex
C                        characters are simpler in appearance (no
C                        serifs, fewer curves, etc.), but are still
C                        of high quality.  Only the alphanumeric
C                        characters in the two sets are different;
C                        all other characters are the same.  To make
C                        PLCHHQ access the duplex set, simply reset
C                        the internal parameter 'CD' to 1 instead of
C                        its default value, 0.  To return to the
C                        complex set, just reset 'CD' to 0 again.
C                        Be aware that each switch from one set to the
C                        other requires reading a binary file.
C
C                        The internal parameter 'FN' (for "Fontcap
C                        Number") has the default value 0.  If it is
C                        given a non-zero value "n", characters from
C                        fontcap database "n" are used.  Each of these
C                        defines at most 128 characters (and usually
C                        no more than 97).  Each desired character is
C                        selected by using the proper ASCII character
C                        in CHRS; function codes which alter the font,
C                        size, and case for PWRITX database access
C                        have no effect when 'FN' is non-zero.  (See
C                        the demo driver for examples of the characters
C                        defined by the fontcap databases.)  It is also
C                        possible to switch from one fontcap database
C                        to another (or to resume the use of characters
C                        from the PWRITX database) by means of "F"
C                        function codes in CHRS.  For example, if
C                        CHRS = ':F1:A:F2:B:F0:C', the effect will be
C                        to select the character in fontcap database
C                        1 which is associated with an "A", followed by
C                        the character from fontcap database 2 which is
C                        associated with a "B", followed by an upper
C                        case Roman "C" of principal size from the
C                        PWRITX database.  Again, be aware that each
C                        "F" function code may result in reading a
C                        binary file.
C
C                        Function codes may be used for other purposes,
C                        as well.  Four of them are used to change the
C                        "level" on which characters are written:
C                        superscript level, subscript level, or normal
C                        level.  Subscripting and superscripting may be
C                        carried to as many as five levels (as in "A to
C                        the B to the C ...").  Changing level may also
C                        change font, case, and size, but all such
C                        changes may be overridden.  Function codes
C                        may be used to take complete control of the
C                        positioning of characters relative to one
C                        another, and to write down, instead of across,
C                        the page.  Strings of octal digits may be used
C                        to specify individual characters in the
C                        PWRITX database directly.  Characters may be
C                        "zoomed" in either width or height, or both.
C
C                        See the section "FUNCTION CODES", below, for
C                        complete descriptions of all function codes.
C
C                        Another internal parameter of PLCHHQ is the
C                        "quality flag" 'QU', whose default value is
C                        zero.  If 'QU' is set to one, characters are
C                        written by calling the medium-quality character
C                        generator PLCHMQ (which see).  PLCHMQ uses a
C                        95-character digitized set which is not as
C                        fancy as the PLCHHQ set, so the resulting
C                        metacode will be about half as large.  Greek
C                        characters and the non-ASCII symbols in the
C                        PLCHHQ set will be unavailable, but other
C                        function codes, including those to change size
C                        and case and to do sub- and superscripting,
C                        may be used.  (If you're not using any function
C                        codes, you might as well call PLCHMQ directly,
C                        of course.)  If the quality flag 'QU' is set
C                        to two, the characters are written by calling
C                        the low-quality character generator PLCHLQ
C                        (which see).  PLCHLQ draws characters using GKS
C                        primitives.  The resulting metacode file is
C                        considerably shortened because only characters
C                        are sent to it, instead of all the pen moves
C                        required to stroke out the characters.  The
C                        disadvantage is that one then becomes dependent
C                        on capabilities of the translator which may or
C                        may not be present.  At NCAR, PLCHLQ is
C                        intended to produce characters as good as
C                        those produced by PLCHMQ, but this situation
C                        cannot be guaranteed elsewhere.
C
C                        Other internal parameters are provided which
C                        allow one to change the effective digitized
C                        sizes of the characters, to force constant
C                        spacing of the characters and to add extra
C                        space between them, to turn on and off the
C                        computation of "text extent vectors", and
C                        to retrieve the magnitudes of the text
C                        extent vectors (distances, in the fractional
C                        coordinate system, from the point (XPOS,YPOS)
C                        to the left, right, bottom, and top edges of
C                        the string just written).
C
C ARGUMENTS
C
C ON INPUT               XPOS,YPOS
C
C                          Positioning coordinates for the characters to
C                          be drawn.  These are user coordinates.  See
C                          CNTR.
C
C                        CHRS
C
C                          Characters to be drawn and "function codes".
C                          CHRS is of type CHARACTER.  The number of
C                          characters in CHRS is taken to be LEN(CHRS);
C                          to use characters 1 through NCHS from a
C                          character variable CHRS, use the FORTRAN-77
C                          substring notation "CHRS(1:NCHS)".
C
C                        SIZE
C
C                          Defines a multiplier for character size, in
C                          one of three ways:
C
C                          If SIZE is less than or equal to zero, its
C                          absolute value specifies the size as a
C                          multiple of the digitized size.  (On a
C                          1024x1024 grid, principal-size capitals are
C                          about 'PW' units wide by 'PH' units high
C                          and the vertical spacing between lines is
C                          'PV' units (by default, 'PW', 'PH', and 'PV'
C                          are 16, 21, and 32).  Indexical-size capitals
C                          are about 'IW' units wide by 'IH' units high;
C                          the vertical spacing between lines is 'IV'
C                          units (by default, 'IW', 'IH', and 'IV' are
C                          12, 13, and 20).  Cartographic-size capitals
C                          are about 'CW' units wide by 'CH' units high;
C                          the vertical spacing between lines is 'CV'
C                          units (by default, 'CW', 'CH', and 'CV' are
C                          8, 9, and 14).
C
C                          If SIZE is greater than zero, but less than
C                          one, it specifies the desired width of a
C                          principal-size capital as a fraction of the
C                          distance across the plotter frame.
C
C                          If SIZE is greater than or equal to one, it
C                          specifies the desired width of a principal-
C                          size capital in plotter coordinates, as
C                          defined by default or by a user call to the
C                          SPPS routine SETI.
C
C                        ANGD
C
C                          The angle, in degrees counterclockwise from
C                          the positive X axis, at which the character
C                          string is to be written.  If the internal
C                          parameter 'TE' is non-zero (by default, it
C                          is zero) and if ANGD is exactly 360., then
C                          no characters are plotted by PLCHHQ; it just
C                          computes the distances, in the fractional
C                          coordinate system, from the point (XPOS,YPOS)
C                          to the left edge, the right edge, the bottom
C                          edge, and the top edge of a box enclosing
C                          the string.  These are stored as the values
C                          of the parameters 'DL', 'DR', 'DB', and
C                          'DT' and may be retrieved by calls to PCGETR.
C
C                        CNTR
C
C                          Centering option.  If the internal parameter
C                          'CE' is zero (the default), then if CNTR
C
C                          = -1. (XPOS,YPOS) is the center of the top
C                                edge or the left edge of the first
C                                character (depending on whether that
C                                character was written down or across,
C                                respectively).
C
C                          =  1. (XPOS,YPOS) is the center of the
C                                bottom edge or the right edge of the
C                                last character (depending on whether
C                                the writing direction was left as
C                                "down" or "across", respectively).
C
C                          =  s  (XPOS,YPOS) is a point obtained by
C                                linear interpolation along the line
C                                joining the two points mentioned above
C                                (the first point being associated with
C                                the value -1 and the second with the
C                                value +1).  The point associated with
C                                "s" is used.  The value "0." gives the
C                                midpoint of the line.
C
C                          If 'CE' is non-zero, then the value of CNTR
C                          is ignored.  Text-extent quantities are
C                          computed and used to exactly center the
C                          output on the point (XPOS, YPOS).  This is
C                          useful, among other things, for labelling
C                          each of a number of points with a single
C                          character.
C
C ON OUTPUT              All arguments are unchanged.
C
C FUNCTION CODES         Function codes may be included in the character
C                        string CHRS to change size, font, case, etc.,
C                        within a string of text.  Function codes are
C                        enclosed by pairs of function-code control
C                        characters (colons, by default).
C
C                        NOTE:  Apostrophes are a poor choice for the
C                        function-code control characters.  If you must
C                        use apostrophes, be aware of the FORTRAN 77
C                        conventions for defining strings containing
C                        apostrophes.
C
C                        An example of a string containing function
C                        codes is as follows:  ':PRU:A:S:2:N:+B:S:2:N:',
C                        which writes the equivalent of "A squared plus
C                        B squared".  Because of the way the colons are
C                        paired, the characters P, R, U, S, N, S, and
C                        N are function codes; all other characters are
C                        characters to be written.
C
C                        No punctuation is needed between function codes
C                        except for a comma between adjacent numbers;
C                        however, extra commas may be used to improve
C                        readability.  The following are the only legal
C                        function codes.  Any other characters in a
C                        function-code string will be ignored except
C                        that an error message will be printed.
C
C                        A.  DATABASE SELECTION
C
C                            F selects the database specified by the
C                              internal parameter 'FN' (by default,
C                              'FN' = 0, selecting the PWRITX database).
C                              The default at the beginning of a string
C                              is to use the database implied by 'FN'.
C
C                            F0 selects the PWRITX database.
C
C                            Fn selects the database defined by fontcap
C                              "n", where, currently, n is between 1
C                              and 20, inclusive.
C
C                        B.  FONT DEFINITIONS
C
C                            These only affect the selection of
C                            characters from the PWRITX database:
C
C                            R  Selects the Roman font.  This is the
C                               default at the beginning of a string.
C
C                            G  Selects the Greek font.
C
C                        C.  SIZE DEFINITIONS
C
C                            These only affect the selection of
C                            characters from the PWRITX database:
C
C                            P  Selects characters of principal size.
C                               This is the default at the beginning
C                               of a string.
C
C                            I  Selects characters of indexical size.
C
C                            K  Selects characters of cartographic size.
C
C                        D.  CASE DEFINITIONS
C
C                            These only affect the selection of
C                            characters from the PWRITX database:
C
C                            U or Un.  Selects upper case characters.
C                               This is the default at the beginning of
C                               a string.
C
C                               If U is followed by a number n (with no
C                               intervening comma), then n characters
C                               will be drawn in upper case; subsequent
C                               characters will be in lower case.  (The
C                               Un option is particularly useful for
C                               capitalizing sentences.)
C
C                            L or Ln.  Selects lower case.
C
C                               If L is followed by a number n, then n
C                               characters will be drawn in lower case;
C                               subsequent characters will be in upper
C                               case.
C
C                        E.  LEVEL DEFINITIONS
C
C                            B or Bn.  Selects subscript level.
C                            S or Sn.  Selects superscript level.
C                            E.        End of sub- or superscripting.
C                            N.        Return to normal level.
C
C                               When subscripting or superscripting
C                               using characters from the PWRITX
C                               database, character size will change
C                               depending on the base character.
C                               Principal base characters are sub- or
C                               superscripted with indexical characters,
C                               with a shift of 'PS' digitization units
C                               up or down (10, by default).  Indexical
C                               or cartographic base characters are
C                               sub- or superscripted with cartographic
C                               characters, with a shift of 'IS' units
C                               up or down (7, by default).  The case
C                               of the indexing characters will
C                               generally be the same as that of the
C                               base character; an exception is that
C                               a lower case indexical base will be
C                               sub- or superscripted with upper case
C                               cartographic, as the cartographic size
C                               has no lower case alphabetic or
C                               numeric characters available.  When
C                               subscripting or superscripting using
C                               characters defined by the fontcap
C                               databases, a similar size change will
C                               be forced, but case will be unaffected.
C
C                               Automatic size and case changes can, of
C                               course, be overridden.  For example,
C                               the string 'A:SPU:B' would be used to
C                               write "A to the B", with both A and B
C                               being of principal size and in upper
C                               case.
C
C                               If S or B is followed by a positive
C                               integer n, then n characters will be
C                               drawn as specified above, after which
C                               size, case and position will be reset
C                               to that of the base character.  Do not
C                               overlap level definitions given for a
C                               specified number of characters.
C
C                               The function code E terminates the
C                               current level of sub- or superscripting,
C                               restoring the size, case, and position
C                               to that of the base character.
C
C                               The function code N terminates the
C                               current level of sub- or superscripting,
C                               restoring the size and case to that of
C                               the base character, but the position
C                               as required to continue writing at
C                               the level of the base character without
C                               writing over the sub- or superscripts.
C
C                               Examples:
C
C                               1.  To write "10 to the 10 to the 100",
C                               use the string '10:S:10:S:100'.  Up to
C                               five levels are allowed.
C
C                               2.  To write "X sub 2, cubed", use the
C                               string 'X:B1:2:S1:3'.  Note that when
C                               the character count in the "B1" is
C                               satisfied, the original base character
C                               is reinstated, so that we can add
C                               another subscript or superscript to it.
C
C                               3.  To write "X sub 2 cubed Y cubed, use
C                               the string 'X:B1:2:S:3:N:Y:S:3'.  The
C                               function code N returns to normal level
C                               in such a way that the character Y does
C                               not overwrite the 2 and the 3.
C
C                               4.  To write "X to the power a sub 1",
C                               followed by something else, use the
C                               string 'X:S:A:B:1:NN:...'  Note that
C                               two function code N's are used, one to
C                               terminate writing the subscript 1 and
C                               another to terminate writing the
C                               superscript "a sub 1" and leave the
C                               current position such that what follows
C                               won't go on top of the "a sub 1".
C
C                        F.  COORDINATE DEFINITIONS
C
C                            Hn,HnQ.  Increment the current position
C                               in the direction specified by ANGD.  Hn
C                               shifts the position by n digitization
C                               units; n can be negative.  HnQ shifts
C                               the position by n blank widths; if n is
C                               less than or equal to zero, it is taken
C                               to be one.
C
C                            Vn,VnQ.  Increment the current position in
C                               the direction specified by ANGD+90.  Vn
C                               shifts the position by n digitization
C                               units; n can be negative.  VnQ shifts
C                               the position by n blank heights; if n is
C                               less than or equal to zero, it is taken
C                               to be one.
C
C                            C  Carriage return.  A "carriage return"
C                               will be done before the next character
C                               is plotted.
C
C                        G.  DIRECTION DEFINITIONS
C
C                            D,Dn.  Write down, rather than across, the
C                               frame.  If D appears without a following
C                               integer n or if n is zero, characters
C                               will be written down until an "A" is
C                               encountered.  If n appears, n characters
C                               will be written down and subsequent
C                               characters will be written across the
C                               frame.
C
C                            A  Write across.  Stop writing down.  At
C                               the beginning of a string, this is the
C                               default.
C
C                        H.  DIRECT CHARACTER ACCESS
C
C                            nnnn.  The character whose octal number
C                               in the PWRITX database is "nnnn" will
C                               be drawn.  The octal number for a given
C                               character is the sum of a font index (0
C                               for Roman, 600 for Greek), a size index
C                               (0 for Principal, 200 for Indexical,
C                               and 400 for Cartographic), a case index
C                               (0 for Upper, 100 for Lower), and the
C                               DPC equivalent of the character (1-32
C                               for A through Z, 33-44 for 0 through 9,
C                               and 45-57 for the individual characters
C                               +, -, *, /, (, ), $, =, blank, comma,
C                               and period).  For example, the string
C                               ':GIU:*' is the same as the string
C                               ':1047:'.  (600+200+0+47=1047, octal.)
C                               To know what character PLCHHQ will
C                               produce in response, refer to the
C                               example in the graphics manual which
C                               shows all of the characters.
C
C                        I.  CHARACTER ZOOMING
C
C                            Xn,XnQ.  Subsequent characters will be made
C                               n/100 times as wide (in the direction
C                               ANGD).  A suffixed Q is ignored.  If "n"
C                               is omitted or zero, 100 is assumed.
C
C                            Yn,YnQ.  Subsequent characters will be made
C                               n/100 times as high (in the direction
C                               ANGD+90).  A suffixed Q will cause a
C                               shift of character position in the
C                               direction ANGD+90 sufficient to keep
C                               the bases of the characters properly
C                               aligned.  If "n" is omitted or zero, 100
C                               is assumed.
C
C                            Zn,ZnQ.  Subsequent characters will be made
C                               n/100 times as large in both of the
C                               directions ANGD and ANGD+90.  A suffixed
C                               Q will cause a shift in the direction
C                               ANGD+90 sufficient to keep the bases of
C                               the characters properly aligned.  If "n"
C                               is omitted or zero, 100 is assumed.
C
C INTERNAL PARAMETERS    A number of internal parameters may be used
C                        to modify the behavior of PLCHHQ.  Parameter
C                        access routines are provided.  To get the
C                        current value of a parameter, do one of the
C                        following calls:
C
C                          CALL PCGETC (WHCH,CVAL)
C                          CALL PCGETI (WHCH,IVAL)
C                          CALL PCGETR (WHCH,RVAL)
C
C                        In each case, the argument WHCH is a character
C                        string, the first two characters of which
C                        identify a particular parameter, and the other
C                        argument is a variable in which the value of
C                        the parameter is to be returned.  PCGETC is
C                        used to get character values, PCGETI to get
C                        integer values, and PCGETR to get real values.
C
C                        Similarly, to set the value of a parameter, do
C                        one of the following calls:
C
C                          CALL PCSETC (WHCH,CVAL)
C                          CALL PCSETI (WHCH,IVAL)
C                          CALL PCSETR (WHCH,RVAL)
C
C                        Again, the argument WHCH is a character string,
C                        the first two characters of which identify a
C                        particular parameter, and the other argument
C                        is a value to be assigned to the parameter.
C
C                        Following is a table of all the parameters.
C                        The first column gives the name, the second
C                        gives its basic type, the third gives its
C                        default value, and the fourth gives a short
C                        description of it.
C
C                          WHCH  T  Def  Description
C                          ----  -  ---  ------------------------------
C
C                          'AS'  R   0.  Add-space flag.  When this
C                                        parameter is non-zero, it
C                                        forces extra space between
C                                        characters.  If a negative
C                                        value is used, it is
C                                        interpreted in digitization
C                                        units.  If a positive value is
C                                        used, it is interpreted as a
C                                        multiple of the current blank
C                                        width.  The lower-quality
C                                        characters are not affected
C                                        by this flag.
C
C                          'CD'  I   0   0 selects the complex subset
C                                        of the PWRITX database, and
C                                        1 selects the duplex subset.
C
C                          'CE'  I   0   0 selects centering of strings
C                                        as specified by the argument
C                                        CNTR, and 1 selects exact
C                                        centering, using text-extent
C                                        quantities.
C
C                          'CH'  R   9.  Digitized cartographic height.
C
C                          'CS'  R   0.  Constant-spacing flag.  The
C                                        high-quality character sets
C                                        have variable spacing.  When
C                                        this parameter is non-zero,
C                                        it forces the characters to
C                                        be spaced a constant distance
C                                        apart.  If a negative value
C                                        is used, its absolute value
C                                        specifies the spacing in
C                                        digitization units.  If a
C                                        positive value is used, it is
C                                        interpreted as a multiplier
C                                        of the current blank width.
C                                        The lower-quality characters
C                                        are not affected by this flag.
C
C                          'CV'  R  14.  Digitized cartographic vertical
C                                        spacing.
C
C                          'CW'  R   8.  Digitized cartographic width.
C
C                          'DB'  R  NA   Output only - distance to the
C                                        bottom edge of the text-extent
C                                        box for the last string drawn,
C                                        in the fractional system.
C
C                          'DL'  R  NA   Output only - distance to the
C                                        left edge of the text-extent
C                                        box for the last string drawn,
C                                        in the fractional system.
C
C                          'DR'  R  NA   Output only - distance to the
C                                        right edge of the text-extent
C                                        box for the last string drawn,
C                                        in the fractional system.
C
C                          'DT'  R  NA   Output only - distance to the
C                                        top edge of the text-extent
C                                        box for the last string drawn,
C                                        in the fractional system.
C
C                          'FC'  C  ':'  Function-code character.
C
C                          'FN'  I   0   Number of the fontcap database
C                                        to be used.  "F" function codes
C                                        in CHRS do not change the value
C                                        of 'FN'; they only affect the
C                                        change the desired fontcap
C                                        database number temporarily.
C
C                          'IH'  R  13.  Digitized indexical height.
C
C                          'IS'  R   7.  Offset to be used when sub-
C                                        or superscripting a character
C                                        of indexical size, in
C                                        digitization units.
C
C                          'IV'  R  20.  Digitized indexical vertical
C                                        spacing.
C
C                          'IW'  R  12.  Digitized indexical width.
C
C                          'PH'  R  21.  Digitized principal height.
C
C                          'PS'  R  10.  Offset to be used when sub-
C                                        or superscripting a character
C                                        of principal size, in
C                                        digitization units.
C
C                          'PV'  R  32.  Digitized principal vertical
C                                        spacing.
C
C                          'PW'  R  16.  Digitized principal width.
C
C                          'QU'  I   0   Quality flag.  0 means use the
C                                        high-quality PWRITX characters.
C                                        1 means use the medium-quality
C                                        characters of PLCHMQ, and 2
C                                        means use the "low-quality"
C                                        characters of PLCHLQ.
C
C                          'SS'  R   0.  Subtract-space flag.  When
C                                        this parameter is non-zero,
C                                        it reduces the space between
C                                        high-quality characters.  If
C                                        a negative value is used, it
C                                        is interpreted in digitization
C                                        units.  If a positive value is
C                                        used, it is interpreted as a
C                                        multiple of the current blank
C                                        width.  The lower-quality
C                                        characters are not affected
C                                        by this flag.
C
C                          'TE'  I   0   The text-extent computation
C                                        flag.  Zero means do not
C                                        compute text-extent quantities,
C                                        non-zero means do compute them.
C                                        Even when 'TE' is zero, the
C                                        text-extent quantities may be
C                                        computed, if 'CE' is non-zero.
C
C                          'XB'  R   0.  For retrieval only.  The X
C                                        position at the beginning of
C                                        the last string written by
C                                        PLCHHQ (fractional system).
C
C                          'XC'  R   0.  For retrieval only.  The X
C                                        position at the center of the
C                                        last character written by
C                                        PLCHHQ (fractional system).
C
C                          'XE'  R   0.  For retrieval only.  The X
C                                        position at the end of the
C                                        last string written by PLCHHQ
C                                        (fractional system).
C
C                          'YB'  R   0.  For retrieval only.  The Y
C                                        position at the beginning of
C                                        the last string written by
C                                        PLCHHQ (fractional system).
C
C                          'YC'  R   0.  For retrieval only.  The Y
C                                        position at the center of the
C                                        last character written by
C                                        PLCHHQ (fractional system).
C
C                          'YE'  R   0.  For retrieval only.  The Y
C                                        position at the end of the
C                                        last string written by PLCHHQ
C                                        (fractional system).
C
C HISTORY                Originally implemented to make the Hershey
C                        character sets more accessible, with the
C                        name PWRX.
C
C                        Made portable in 1978 and given the name
C                        PWRITX.
C
C                        Made to be FORTRAN 77 and GKS compatible,
C                        August, 1984.
C
C                        Massively rewritten in February-March, 1988;
C                        renamed PLCHHQ.
C
C                        Overhauled in June, 1990, principally to
C                        provide access to fontcap databases.
C
C-----------------------------------------------------------------------
C S U B R O U T I N E   P L C H M Q   -   D E S C R I P T I O N
C-----------------------------------------------------------------------
C
C SUBROUTINE PLCHMQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C PURPOSE                PLCHMQ is a routine which plots characters of
C                        "medium quality".  It plots them by drawing
C                        lines, as PLCHHQ does, but it does not produce
C                        quite such fancy characters.  Using PLCHMQ to
C                        plot a given string of characters will create
C                        more metacode than if PLCHLQ were used, which
C                        may be a disadvantage.  However, it may also
C                        be more dependable, in that it does not depend
C                        on capabilities which the translator may or
C                        may not have.
C
C USAGE                  CALL PLCHMQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C ARGUMENTS
C
C ON INPUT               XPOS,YPOS
C
C                          Positioning coordinates for the characters to
C                          be drawn.  These are user coordinates.  See
C                          CNTR.
C
C                        CHRS
C
C                          Character string to be drawn.  The string
C                          may include any of the 95 characters !, ", #,
C                          $, %, &, ', (, ), *, +, comma, -, ., /, 0-9,
C                          :, ;, <, =, >, ?, @, A-Z, [, \, ], ^, _, `,
C                          a-z, {, |, }, ~, and blank.  Function codes,
C                          like those in calls to PLCHHQ, may not be
C                          used.
C
C                        SIZE
C
C                          Defines a multiplier for character size, in
C                          one of three ways:
C
C                          If SIZE is less than or equal to zero, its
C                          absolute value specifies the size as a
C                          multiple of a default digitized size on a
C                          1024x1024 grid, on which blanks are 16 units
C                          wide.
C
C                          If SIZE is greater than zero, but less than
C                          one, it specifies the desired width of a
C                          blank as a fraction of the distance across
C                          the plotter frame.
C
C                          If SIZE is greater than or equal to one, it
C                          specifies the desired width of a blank in
C                          plotter coordinates, as defined by default
C                          or by a user's call to the SPPS routine SETI.
C
C                          Capital letters are 4/6 as wide as a blank
C                          and, by default, 7/4 as high as they are
C                          wide (the internal parameter 'HW' may be
C                          used to change the latter value).  The
C                          distance from the center of each character
C                          to the center of the next is constant and
C                          is equal to one blank width.
C
C                          SIZE is defined in such a way as to be more
C                          or less consistent with PLCHHQ.  If you
C                          change a "CALL PLCHHQ" to a "CALL PLCHMQ"
C                          (assuming that no function codes are used
C                          in the string, of course), the results will
C                          be much the same; the characters will be
C                          slightly shorter and the length of the
C                          string will be different.  If you use SIZE
C                          less than zero and you have reset the PLCHHQ
C                          internal parameter 'PW' to something other
C                          than 16, the difference in the output of
C                          the two routines will be greater.
C
C                        ANGD
C
C                          The angle, in degrees counterclockwise from
C                          the positive X axis, at which the character
C                          string is to be written.
C
C                        CNTR
C
C                          Centering option.
C
C                          = -1. (XPOS,YPOS) is the center of the left
C                                edge of the first character.
C
C                          =  1. (XPOS,YPOS) is the center of the right
C                                edge of the last character.
C
C                          =  s  (XPOS,YPOS) is a point obtained by
C                                linear interpolation along the line
C                                joining the two points mentioned above
C                                (the first point being associated with
C                                the value -1 and the second with the
C                                value +1).  The point associated with
C                                "s" is used.  The value "0." gives the
C                                midpoint of the line.
C
C ON OUTPUT              All arguments are unchanged.
C
C HISTORY                Implemented for use by DASHCHAR with the name
C                        PWRY.
C
C                        Made portable in January, 1977, and given the
C                        name PWRITY.
C
C                        Converted to FORTRAN 77 and GKS in July, 1984.
C
C                        Massively rewritten in February-March, 1988;
C                        renamed PLCHMQ.  Many ASCII characters added
C                        to the dataset.
C
C ALGORITHM              Digitizations of the characters are defined by
C                        data statements, mapped as implied by values
C                        of XPOS, YPOS, SIZE, ANGD, and CNTR, and
C                        plotted.
C
C INTERNAL PARAMETERS    PLCHMQ has a single internal parameter, named
C                        'HW'; it specifies the desired ratio of the
C                        character height (the height of a capital) to
C                        the character width (excluding white space).
C                        To set 'HW', execute the statement
C
C                          CALL PCSETR ('HW',RVAL)
C
C                        where "RVAL" is the desired value of 'HW'.  To
C                        get the current value of 'HW', execute the
C                        statement
C
C                          CALL PCGETR ('HW',RVAL)
C
C                        where RVAL is a variable in which the value of
C                        'HW' is to be placed.
C
C                        The default value of 'HW' is 1.75, reflecting
C                        the fact that the capitals are digitized to
C                        be 7 units high and 4 units wide and 1.75 is
C                        therefore their natural aspect ratio.
C
C                        Negative values of 'HW' may be used.  The
C                        absolute value will be used as the ratio, but,
C                        in addition, PLCHHQ will be prohibited from
C                        changing 'HW' (which it otherwise does when
C                        the quality flag 'QU' is set to 1).
C
C-----------------------------------------------------------------------
C S U B R O U T I N E   P L C H L Q   -   D E S C R I P T I O N
C-----------------------------------------------------------------------
C
C SUBROUTINE PLCHLQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C PURPOSE                PLCHLQ is a routine which plots characters of
C                        "low quality".  It plots them by using the GKS
C                        character-drawing routines.  Using PLCHLQ to
C                        plot a given string of characters will create
C                        less metacode than if PLCHHQ or PLCHMQ were
C                        used; the results will depend on capabilities
C                        of the translator.
C
C USAGE                  CALL PLCHLQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C ARGUMENTS
C
C ON INPUT               XPOS,YPOS
C
C                          Positioning coordinates for the characters to
C                          be drawn.  These are user coordinates.  See
C                          CNTR.
C
C                        CHRS
C
C                          Character string to be drawn.  The string
C                          may include any of the 95 characters !, ", #,
C                          $, %, &, ', (, ), *, +, comma, -, ., /, 0-9,
C                          :, ;, <, =, >, ?, @, A-Z, [, \, ], ^, _, `,
C                          a-z, {, |, }, ~, and blank.  Function codes,
C                          like those in calls to PLCHHQ, may not be
C                          used.
C
C                        SIZE
C
C                          Defines a multiplier for character size, in
C                          one of three ways:
C
C                          If SIZE is less than or equal to zero, its
C                          absolute value specifies the size as a
C                          multiple of a default digitized size on a
C                          1024x1024 grid, on which blanks are 16 units
C                          wide.
C
C                          If SIZE is greater than zero, but less than
C                          one, it specifies the desired width of a
C                          blank as a fraction of the distance across
C                          the plotter frame.
C
C                          If SIZE is greater than or equal to one, it
C                          specifies the desired width of a blank in
C                          plotter coordinates, as defined by default
C                          or by a user's call to the SPPS routine SETI.
C
C                          Note that SIZE is defined in such a way as
C                          to be consistent with PLCHHQ.
C
C                        ANGD
C
C                          The angle, in degrees counterclockwise from
C                          the positive X axis, at which the character
C                          string is to be written.
C
C                        CNTR
C
C                          Centering option.
C
C                          < 0.  (XPOS,YPOS) is the center of the left
C                                edge of the first character.
C
C                          > 0.  (XPOS,YPOS) is the center of the right
C                                edge of the last character.
C
C                          = 0.  (XPOS,YPOS) is the midpoint of the
C                                string.
C
C                          Note that this argument is not quite the
C                          same as that for PLCHHQ (though it does
C                          include the three most useful cases).
C
C ON OUTPUT              All arguments are unchanged.
C
C HISTORY                Written in March, 1988, as third member of the
C                        PLCHxQ family.  Based on WTSTR, from SPPS.
C
C ALGORITHM              GKS character-writing routines are used.
C
C-----------------------------------------------------------------------
C S U B R O U T I N E   P C G E T x   -   D E S C R I P T I O N
C-----------------------------------------------------------------------
C
C PURPOSE                To get the values of internal parameters.
C
C USAGE                  CALL PCGETC (WHCH,CVAL)
C                        CALL PCGETI (WHCH,IVAL)
C                        CALL PCGETR (WHCH,RVAL)
C
C ARGUMENTS
C
C ON INPUT               WHCH is a character string specifying the
C                        parameter to get.
C
C                        CVAL, IVAL, or RVAL is a variable to receive
C                        the value of the parameter specified by WHCH -
C                        of type character, integer, or real,
C                        respectively.
C
C                        See "INTERNAL PARAMETERS", in the write-ups
C                        of PLCHHQ and PLCHMQ.
C
C ON OUTPUT              All arguments are unchanged.
C
C-----------------------------------------------------------------------
C S U B R O U T I N E   P C S E T x   -   D E S C R I P T I O N
C-----------------------------------------------------------------------
C
C PURPOSE                To set the values of internal parameters.
C
C USAGE                  CALL PCSETC (WHCH,CVAL)
C                        CALL PCSETI (WHCH,IVAL)
C                        CALL PCSETR (WHCH,RVAL)
C
C ARGUMENTS
C
C ON INPUT               WHCH is a character string specifying the
C                        parameter to set.
C
C                        CVAL, IVAL, or RVAL is the value to be assigned
C                        to the parameter specified by WHCH - of type
C                        character, integer, or real, respectively.
C
C                        See "INTERNAL PARAMETERS", in the write-ups
C                        of PLCHHQ and PLCHMQ.
C
C ON OUTPUT              All arguments are unchanged.
C
C***********************************************************************
C C O D E   -   C H A R A C T E R - W R I T I N G   R O U T I N E S
C***********************************************************************
C
      SUBROUTINE PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This is the high-quality character-drawing routine.
C
C
C D E C L A R A T I O N S
C
C
      CHARACTER CHRS*(*)
C
C COMMON block declarations.  PCPRMS holds user-accessible internal
C parameters.  PCSVEM holds other variables which are either used in
C more than one routine or which need to be saved from one call to the
C next.  PCPFMQ holds internal parameters for the medium-quality
C character generator PLCHMQ.  PCPFFC holds internal parameters for the
C new routine (PCCFFC) which access the fonts defined by font caps.
C
C Note that the sizes of IDDA and INDA may be reduced to match the
C values of IDDL and INDL computed below.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),ICEN,IQUF,
     +                ISCR,ITEF,JCOD,NFCC,SSIC,SSPR,SUBS,VPIC(3),
     +                WPIC(3),XBEG,XCEN,XEND,XMUL(3),YBEG,YCEN,YEND,
     +                YMUL(3)
      SAVE   /PCPRMS/
C
      COMMON /PCSVEM/ IBNU,ICOD,IDDA(8625),IDDL,RDGU(300),IDPC(256),
     +                IERU,INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
      COMMON /PCPFMQ/ RHTW
      SAVE   /PCPFMQ/
C
      COMMON /PCPFFC/ NFNT
      SAVE   /PCPFFC/
C
C Declare the BLOCK DATA routine external to force it to load.
C
      EXTERNAL PCBLDA
C
C Define a dummy array for use in skipping records on IBNU.
C
      DIMENSION IDUM(1)
C
C Define arrays in which to put multipliers representing the combined
C effects of changing 'PH', 'PW', 'IH', 'IW', 'CH', 'CW', and the
C zoom factor.
C
      DIMENSION XMZM(3),YMZM(3)
C
C Define arrays implementing the subscript/superscript stack.
C
      DIMENSION NLEV(5),XCNO(5),YCNO(5),XRGO(5),YRGO(5),ICSO(5),ISZO(5),
     +          CSTO(5),SNTO(5)
C
C Define an array to hold the codes associated with 19 "special" ASCII
C characters which might occur in CHRS and for which we will attempt
C to provide a proper response.
C
      DIMENSION ISPC(19)
C
C Define a character variable in which to collect characters to be
C written by PLCHMQ or PLCHLQ if lower quality is selected.
C
      CHARACTER*100 CHBF
C
C Define a character array in which to define the characters to be
C associated with values of NDPC from 1 to 95.
C
      CHARACTER*1 CDPC(95)
C
C Define an array in which to define the ASCII decimal equivalents of
C the characters in CDPC.
C
      DIMENSION IASC(95)
C
C Define the characters to be associated with values of NDPC from 1
C to 95.
C
      DATA CDPC / 'A','B','C','D','E','F','G','H','I','J',
     +            'K','L','M','N','O','P','Q','R','S','T',
     +            'U','V','W','X','Y','Z','0','1','2','3',
     +            '4','5','6','7','8','9','+','-','*','/',
     +            '(',')','$','=',' ',',','.','a','b','c',
     +            'd','e','f','g','h','i','j','k','l','m',
     +            'n','o','p','q','r','s','t','u','v','w',
     +            'x','y','z','!','"','#','%','&',':',';',
#if defined(u370) && defined(unix)
     +            '<','>','?','@','[','\',']','{','|','}',
#else
     +            '<','>','?','@','[','\\',']','{','|','}',
#endif
#if defined(mips) && defined(ultrix)
     +            '~',"'",'^','_','`'                   /
#else
     +            '~','''','^','_','`'                   /
#endif
C
C Define the ASCII decimal equivalents of the characters in CDPC.
C
      DATA IASC /  65, 66, 67, 68, 69, 70, 71, 72, 73, 74,
     +             75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
     +             85, 86, 87, 88, 89, 90, 48, 49, 50, 51,
     +             52, 53, 54, 55, 56, 57, 43, 45, 42, 47,
     +             40, 41, 36, 61, 32, 44, 46, 97, 98, 99,
     +            100,101,102,103,104,105,106,107,108,109,
     +            110,111,112,113,114,115,116,117,118,119,
     +            120,121,122, 33, 34, 35, 37, 38, 58, 59,
     +             60, 62, 63, 64, 91, 92, 93,123,124,125,
     +            126, 39, 94, 95, 96                    /
C
C Define the number of card images used to represent the contents of
C the arrays INDA and IDDA.  These are used to compute the number of
C binary words to be read into these arrays.
C
      DATA ICND,ICDD / 49,575 /
C
C Define indices for font, size, and case definitions.  These indices
C are used as displacements into the array INDA.  The sum of a font
C index (Roman or Greek), a size index (principal, indexical, or
C cartographic), a case index (upper or lower), and the DPC code for
C a particular character uniquely defines the index of an entry in
C INDA pointing to the proper digitization in the array IDDA.
C
      DATA IFRO,IFGR      / 0,384     /
      DATA ISZP,ISZI,ISZC / 0,128,256 /
      DATA ICSU,ICSL      / 0, 64     /
C
C Define the codes associated with 19 "special" ASCII characters which
C might possibly appear in CHRS.
C
      DATA ISPC / 93,484,485,111,421,91,92,99,100,94,321,105,337,106,
     +            745,336,746,431,483 /
C
C
C I N I T I A L I Z A T I O N
C
C
C If it hasn't already been done, do that part of the initialization
C which is done only once per loading of the routine.
C
      IF (INIT.EQ.0) THEN
C
        INIT=1
C
C NBPW is the number of bits per word and NPPW is the number of 15-bit
C parcels per word.  INDL and IDDL are the number of words to be read
C into the arrays INDA and IDDA, respectively.
C
        NBPW=I1MACH(5)
        NPPW=NBPW/15
        INDL=(ICND*16-1)/NPPW+1
        IDDL=(ICDD*16*15-1)/NBPW+1
C
C Define the error file logical unit number.
C
        IERU=I1MACH(4)
C
C If it hasn't already been done, initialize the numeric equivalent of
C the function code character.
C
        IF (NFCC.EQ.0) NFCC=ICHAR(':')
C
C Initialize the table IDPC.  For a given character "c", IDPC(ICHAR(c))
C is the integer value of the equivalent of "c" in an augmented DPC
C character set.  (DPC was the set of six-bit character codes used on
C the CDC 7600 (the machine on which the predecessor of PLCHHQ was
C first implemented) and the digitization arrays are arranged in DPC
C order.  First, all undefined entries are zeroed.
C
        DO 101 I=1,256
          IDPC(I)=0
  101   CONTINUE
C
C Then, the entries are defined, one at a time.
C
        DO 102 NDPC=1,95
          NCOL=ICHAR(CDPC(NDPC))
          IF (NCOL.GE.1.AND.NCOL.LE.256) THEN
            IF (IDPC(NCOL).EQ.0) IDPC(NCOL)=NDPC
          END IF
  102   CONTINUE
C
C Set the flag ICOD to force loading of the dataset implied by the
C user-set or default value of JCOD.
C
        ICOD=-1
C
      END IF
C
C Get the fractional-system coordinates of the string position.
C
      XFRA=CUFX(XPOS)
      YFRA=CUFY(YPOS)
C
C Determine the resolution of the plotter, as declared by default or
C by the user.
C
      CALL GETUSV ('XF',IRSX)
      RSLN=2.**IRSX-1.
C
C Determine a multiplier for the digitized size which will make the
C characters have the size requested by the user.
C
      IF (SIZE.LE.0.) THEN
        SIZM=ABS(SIZE)/1023.
      ELSE IF (SIZE.LT.1.) THEN
        SIZM=SIZE/WPIC(1)
      ELSE
        SIZM=(SIZE/RSLN)/WPIC(1)
      END IF
C
C If high-quality characters are to be used, load the high-quality
C character dataset.
C
      IF (IQUF.EQ.0) THEN
C
C If no dataset has been read, or if the user has requested the other
C one of the two, read the proper one.  The first time a given dataset
C is read, verify that it has been done correctly.
C
        IF (ICOD.NE.JCOD) THEN
C
          ICOD=JCOD
C
          CALL PCFOPN (IBNU,0)
C
          IF (ICOD.NE.0) THEN
            CALL PCFRED (IBNU,0,IDUM,1)
            CALL PCFRED (IBNU,0,IDUM,1)
          END IF
C
          CALL PCFRED (IBNU,0,INDA,INDL)
          CALL PCFRED (IBNU,0,IDDA,IDDL)
C
          CALL PCFCLS (IBNU,0)
C
          IERR=0
          IF (ICOD.EQ.0.AND.IVCO.EQ.0) THEN
            CALL PCCCHK (IERR)
            IVCO=1
          END IF
          IF (ICOD.NE.0.AND.IVDU.EQ.0) THEN
            CALL PCDCHK (IERR)
            IVDU=1
          END IF
          IF (IERR.NE.0) CALL SETER
     +                  ('PLCHHQ - DATASET NOT LOADED CORRECTLY',IERR,2)
        END IF
C
C If lower-quality characters are to be used, save the PLCHMQ parameter
C specifying the ratio of character height to character width.
C
      ELSE
C
        RHWO=RHTW
C
      END IF
C
C Set the orientation angle, in radians, and determine the components
C of various vectors in the direction  of the string and at right
C angles to it.
C
      ANGR=.017453292519943*ANGD
C
      SINO=SIN(ANGR)
      COSO=COS(ANGR)
      STSO=SIZM*SINO
      STCO=SIZM*COSO
      SINM=SIN(ANGR-1.57079632679489)
      COSM=COS(ANGR-1.57079632679489)
      SINP=SIN(ANGR+1.57079632679489)
      COSP=COS(ANGR+1.57079632679489)
C
C Decide whether to do pass 1 and pass 2 or just pass 2.  (If the string
C is positioned relative to its starting point, we need only do pass 2.)
C
      IPSS=1
      IF (ICEN.EQ.0.AND.CNTR.EQ.-1.) IPSS=2
C
C For this pass, begin the string at the position of the image of the
C user's (XPOS,YPOS) in the fractional system.
C
      XBEG=XFRA
      YBEG=YFRA
C
C Save the current value of NFNT for later restoration.
C
      NFNO=NFNT
C
C Start of pass.  Make three copies of the string position for various
C purposes.  (XBOL,YBOL) is the point at the beginning of the current
C line, (XCEN,YCEN) is the point at the center of the last character,
C and (XRGT,YRGT) is the point at the right end of the last character.
C
  103 XBOL=XBEG
      YBOL=YBEG
      XCEN=XBEG
      YCEN=YBEG
      XRGT=XBEG
      YRGT=YBEG
C
C Initialize the character index and the count of characters in the
C string.
C
      ICHR=0
      NCHR=LEN(CHRS)
C
C Initialize the index offsets for font, size and case to Roman,
C principal, and upper, respectively.
C
      IFNT=IFRO
      ISZE=ISZP
      ICSE=ICSU
C
C Initialize the index which says what size the current character is.
C
      IPIC=1
C
C If lower-quality characters are being used and we are starting pass
C 2, initialize the substring position.  It is assumed that the whole
C substring needs to be shifted by 1/6 character width to the right.
C This is as required for PLCHMQ.  Also initialize the substring
C character count and the character size.
C
      IF (IQUF.NE.0.AND.IPSS.EQ.2) THEN
        XSUB=CFUX(XBEG+WPIC(1)*STCO/6.)
        YSUB=CFUY(YBEG+WPIC(1)*STSO/6.)
        NCBF=0
        SIZS=SIZM*WPIC(1)
        IF (RHTW.GE.0.) RHTW=1.5*HPIC(1)/WPIC(1)
      END IF
C
C Zero the character counts for "writing down" and for case change.
C
      NDWN=0
      NCSE=0
C
C Zero the variable defining the vertical space used by the previous
C character, which is used for vertical spacing when "writing down".
C
      VEPC=0.
C
C Zero variables which affect the spacing between characters.
C
      ADDP=0.
      SUBP=0.
C
C Set the current subscript/superscript level.
C
      NSSL=0
C
C Set the flag to indicate that we're not currently processing function
C codes.
C
      IPFC=0
C
C Initialize the multipliers representing the combined effects of
C changing 'PH', 'PW', 'IH', 'IW', 'CH', 'CW', and the zoom factor.
C
      XMZM(1)=XMUL(1)
      XMZM(2)=XMUL(2)
      XMZM(3)=XMUL(3)
      YMZM(1)=YMUL(1)
      YMZM(2)=YMUL(2)
      YMZM(3)=YMUL(3)
C
C If the distances to the edges of the string are to be computed,
C initialize the needed quantities.
C
      IF ((IPSS.EQ.1.AND.ICEN.NE.0).OR.
     +    (IPSS.EQ.2.AND.ITEF.NE.0)) THEN
        DSTL=-1.E6
        DSTR=-1.E6
        DSTB=-1.E6
        DSTT=-1.E6
      END IF
C
C
C  P R O C E S S   T H E   N E X T   C H A R A C T E R
C
C
C If there are no more characters in the string, jump to end-of-string
C processor.
C
  104 IF (ICHR.GE.NCHR) GO TO 108
C
C Get the next character from the string.  NCOL is the number of the
C character in the character collating sequence on the machine on
C which PLCHHQ is being run.
C
      ICHR=ICHR+1
      NCOL=ICHAR(CHRS(ICHR:ICHR))
C
C If the function-code signal character is encountered, flip the value
C of the function-code processing flag and go get the next character.
C
      IF (NCOL.EQ.NFCC) THEN
        IPFC=1-IPFC
        GO TO 104
      END IF
C
C NDPC is the number of the character in the DPC collating sequence.
C
      NDPC=IDPC(NCOL)
C
C If function codes are being processed, jump to the function code
C processor.
C
      IF (IPFC.NE.0) GO TO 109
C
C If the character is not one of the "standard" ones, take special
C action.  Lower-case alphabetic characters behave like upper-case
C alphabetic characters, but force the case to "lower".  "Special"
C characters yield a particular index into the PWRITX database or, in
C three cases, a blank.  As a last resort, a star is used.
C
      IF (NDPC.LT.1.OR.NDPC.GT.47) THEN
        IF (NDPC.GE.48.AND.NDPC.LE.73) THEN
          INDP=IFNT+ISZE+ICSL+NDPC-47
        ELSE IF (NDPC.GE.74.AND.NDPC.LE.92) THEN
          INDP=ISPC(NDPC-73)
        ELSE IF (NDPC.GE.93.AND.NDPC.LE.95) THEN
          INDP=45
        ELSE
          INDP=358
        END IF
        GO TO 105
      END IF
C
C Compute the index of the entry in INDA which points to the proper
C digitization in IDDA.
C
      INDP=IFNT+ISZE+ICSE+NDPC
C
C
C   D R A W   A   C H A R A C T E R
C
C
C If high-quality characters are being used, get the digitization of
C the character in the array RDGU; if no digitization is found, try a
C star instead; if that can't be found, there's a more serious error.
C In any case, define the distances from the center of the character to
C its left and right ends.
C
  105 IF (IQUF.EQ.0) THEN
        IF (NFNT.EQ.0.OR.NDPC.LT.1.OR.NDPC.GT.95.OR.INDP.EQ.95) THEN
          CALL PCEXCD (INDP,MIN(2,IPSS+ICEN),NDGU)
        ELSE
          IF (ICSE.NE.ICSL.OR.NDPC.LT.1.OR.NDPC.GT.26) THEN
            NASC=IASC(NDPC)
          ELSE
            NASC=IASC(NDPC+47)
          END IF
          CALL PCCFFC (MIN(2,IPSS+ICEN),IBNU,NFNT,NASC,IPIC,RDGU,300,
     +                                                             NDGU)
          IF (NDGU.EQ.0) THEN
            CALL PCCFFC (MIN(2,IPSS+ICEN),IBNU,1,NASC,IPIC,RDGU,300,
     +                                                             NDGU)
            IF (NDGU.EQ.0) CALL PCEXCD (INDP,MIN(2,IPSS+ICEN),NDGU)
          END IF
        END IF
        IF (NDGU.EQ.0) THEN
          CALL PCEXCD (358,MIN(2,IPSS+ICEN),NDGU)
          IF (NDGU.EQ.0) CALL SETER
     +            ('PLCHHQ - INTERNAL LOGIC ERROR - SEE CONSULTANT',1,2)
        END IF
        IF (CONS.EQ.0.) THEN
          DTLE=-XMZM(IPIC)*RDGU(1)
          DTRE= XMZM(IPIC)*RDGU(2)
        ELSE IF (CONS.LT.0.) THEN
          DTLE=-CONS
          DTRE=-CONS
        ELSE
          DTLE=CONS*WPIC(IPIC)
          DTRE=CONS*WPIC(IPIC)
        END IF
      ELSE
        DTLE=.5*WPIC(IPIC)
        DTRE=.5*WPIC(IPIC)
      END IF
C
C If this character is to be "written down" (that is to say, beneath
C the previous character), arrange for that.  Otherwise, just zero the
C "last character written down" flag.  Note that, while subscripting
C or superscripting is in progress, "writing down" is suspended.
C
      IF (NDWN.GT.0.AND.NSSL.EQ.0) THEN
        LCWD=1
        XCEN=XCEN+.5*SIZM*(VEPC+VPIC(IPIC))*COSM
        YCEN=YCEN+.5*SIZM*(VEPC+VPIC(IPIC))*SINM
        IF (ADDP.NE.0.) THEN
          IF (ADDS.LT.0.) THEN
            XCEN=XCEN+SIZM*ADDP*COSM
            YCEN=YCEN+SIZM*ADDP*SINM
          ELSE
            XCEN=XCEN+SIZM*.5*(ADDP+ADDS*WPIC(IPIC))*COSM
            YCEN=YCEN+SIZM*.5*(ADDP+ADDS*WPIC(IPIC))*SINM
          END IF
        END IF
        IF (SUBP.NE.0.) THEN
          IF (SUBS.LT.0.) THEN
            XCEN=XCEN-SIZM*SUBP*COSM
            YCEN=YCEN-SIZM*SUBP*SINM
          ELSE
            XCEN=XCEN-SIZM*.5*(SUBP+SUBS*WPIC(IPIC))*COSM
            YCEN=YCEN-SIZM*.5*(SUBP+SUBS*WPIC(IPIC))*SINM
          END IF
        END IF
        NDWN=NDWN-1
        IF (IQUF.NE.0.AND.IPSS.EQ.2) THEN
          IF (NCBF.NE.0) THEN
            CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
            NCBF=0
          END IF
          XSUB=CFUX(XCEN-(DTLE-WPIC(IPIC)/6.)*STCO)
          YSUB=CFUY(YCEN-(DTLE-WPIC(IPIC)/6.)*STSO)
        END IF
      ELSE
        LCWD=0
      END IF
C
C Find the coordinates of the center and the right end of the new
C character.
C
      IF (LCWD.EQ.0) THEN
        XCEN=XRGT+DTLE*STCO
        YCEN=YRGT+DTLE*STSO
        IF (ADDP.NE.0.) THEN
          IF (ADDS.LT.0.) THEN
            XCEN=XCEN+ADDP*STCO
            YCEN=YCEN+ADDP*STSO
          ELSE
            XCEN=XCEN+.5*(ADDP+ADDS*WPIC(IPIC))*STCO
            YCEN=YCEN+.5*(ADDP+ADDS*WPIC(IPIC))*STSO
          END IF
        END IF
        IF (SUBP.NE.0.) THEN
          IF (SUBS.LT.0.) THEN
            XCEN=XCEN-SUBP*STCO
            YCEN=YCEN-SUBP*STSO
          ELSE
            XCEN=XCEN-.5*(SUBP+SUBS*WPIC(IPIC))*STCO
            YCEN=YCEN-.5*(SUBP+SUBS*WPIC(IPIC))*STSO
          END IF
        END IF
        IF (IQUF.NE.0.AND.IPSS.EQ.2.AND.(ADDP.NE.0..OR.SUBP.NE.0.)) THEN
          IF (NCBF.NE.0) THEN
            CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
            NCBF=0
          END IF
          XSUB=CFUX(XCEN-(DTLE-WPIC(IPIC)/6.)*STCO)
          YSUB=CFUY(YCEN-(DTLE-WPIC(IPIC)/6.)*STSO)
        END IF
      END IF
C
      XRGT=XCEN+DTRE*STCO
      YRGT=YCEN+DTRE*STSO
C
C Draw the character and/or update the quantities from which the
C magnitudes of the text-extent vectors will be computed.
C
      IF (IQUF.EQ.0) THEN
C
        RDGU(1)=-2048.
C
        IF (IPSS.EQ.2.AND.(ITEF.EQ.0.OR.ANGD.NE.360.)) THEN
C
          DO 106 I=3,NDGU,2
            IF (RDGU(I).NE.-2048.) THEN
              XTMP=XCEN+XMZM(IPIC)*RDGU(I  )*STCO
     +                 -YMZM(IPIC)*RDGU(I+1)*STSO
              YTMP=YCEN+XMZM(IPIC)*RDGU(I  )*STSO
     +                 +YMZM(IPIC)*RDGU(I+1)*STCO
              IF (RDGU(I-2).EQ.-2048.) THEN
                IF (ISCR.EQ.0) THEN
                  CALL PLOTIF (XTMP,YTMP,0)
                ELSE
                  CALL SCPLTW (INT(XTMP*32767.),INT(YTMP*32767.),0)
                END IF
              ELSE
                IF (ISCR.EQ.0) THEN
                  CALL PLOTIF (XTMP,YTMP,1)
                ELSE
                  CALL SCPLTW (INT(XTMP*32767.),INT(YTMP*32767.),1)
                END IF
              END IF
            END IF
  106     CONTINUE
C
        END IF
C
        IF ((IPSS.EQ.1.AND.ICEN.NE.0).OR.
     +      (IPSS.EQ.2.AND.ITEF.NE.0)) THEN
C
          UCEN=+(XCEN-XFRA)*COSO+(YCEN-YFRA)*SINO
          VCEN=-(XCEN-XFRA)*SINO+(YCEN-YFRA)*COSO
C
          DO 107 I=3,NDGU,2
            IF (RDGU(I).NE.-2048.) THEN
              DSTL=MAX(DSTL,-UCEN-SIZM*XMZM(IPIC)*RDGU(I  ))
              DSTR=MAX(DSTR,+UCEN+SIZM*XMZM(IPIC)*RDGU(I  ))
              DSTB=MAX(DSTB,-VCEN-SIZM*YMZM(IPIC)*RDGU(I+1))
              DSTT=MAX(DSTT,+VCEN+SIZM*YMZM(IPIC)*RDGU(I+1))
            END IF
  107     CONTINUE
C
        END IF
C
      ELSE
C
        IF (IPSS.EQ.2.AND.(ITEF.EQ.0.OR.ANGD.NE.360.)) THEN
          IF (NCBF.EQ.100) THEN
            CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
            XSUB=CFUX(CUFX(XSUB)+REAL(NCBF)*WPIC(IPIC)*STCO)
            YSUB=CFUY(CUFY(YSUB)+REAL(NCBF)*WPIC(IPIC)*STSO)
            NCBF=0
          END IF
          NCBF=NCBF+1
          IF (INDP.EQ.95) THEN
            CHBF(NCBF:NCBF)='x'
          ELSE IF (INDP.EQ.358) THEN
            CHBF(NCBF:NCBF)=' '
          ELSE IF (ICSE.EQ.ICSL.AND.NDPC.GE.1.AND.NDPC.LE.26) THEN
            CHBF(NCBF:NCBF)=CDPC(NDPC+47)
          ELSE
            CHBF(NCBF:NCBF)=CDPC(NDPC)
          END IF
        END IF
C
        IF ((IPSS.EQ.1.AND.ICEN.NE.0).OR.
     +      (IPSS.EQ.2.AND.ITEF.NE.0)) THEN
          UCEN=+(XCEN-XFRA)*COSO+(YCEN-YFRA)*SINO
          VCEN=-(XCEN-XFRA)*SINO+(YCEN-YFRA)*COSO
          DSTL=MAX(DSTL,-UCEN-SIZM*WPIC(IPIC)/3.,
     +                  -UCEN+SIZM*WPIC(IPIC)/3.)
          DSTR=MAX(DSTR,+UCEN-SIZM*WPIC(IPIC)/3.,
     +                  +UCEN+SIZM*WPIC(IPIC)/3.)
          DSTB=MAX(DSTB,-VCEN-SIZM*ABS(RHTW)*WPIC(IPIC)/3.,
     +                  -VCEN+SIZM*ABS(RHTW)*WPIC(IPIC)/3.)
          DSTT=MAX(DSTT,+VCEN-SIZM*ABS(RHTW)*WPIC(IPIC)/3.,
     +                  +VCEN+SIZM*ABS(RHTW)*WPIC(IPIC)/3.)
        END IF
C
      END IF
C
C Save the vertical space occupied by the current character as that of
C the previous character.
C
      VEPC=VPIC(IPIC)
C
C Set variables affecting the amount of space between characters.
C
      IF (ADDS.LT.0.) THEN
        ADDP=-ADDS
      ELSE IF (ADDS.GT.0.) THEN
        ADDP=ADDS*WPIC(IPIC)
      END IF
C
      IF (SUBS.LT.0.) THEN
        SUBP=-SUBS
      ELSE IF (SUBS.GT.0.) THEN
        SUBP=SUBS*WPIC(IPIC)
      END IF
C
C If case was set for a specified number of characters, see if all of
C them have been done and, if so, reset the case index.
C
      IF (NCSE.GT.0) THEN
        NCSE=NCSE-1
        IF (NCSE.EQ.0) ICSE=ICSP
      END IF
C
C If we are subscripting or superscripting for a specified number of
C characters, see if all of them have been done and, if so, reset all
C the required variables.
C
      IF (NSSL.NE.0) THEN
        IF (NLEV(NSSL).GT.0) THEN
          NLEV(NSSL)=NLEV(NSSL)-1
          IF (NLEV(NSSL).EQ.0) THEN
            XCEN=XCNO(NSSL)
            YCEN=YCNO(NSSL)
            XRGT=XRGO(NSSL)
            YRGT=YRGO(NSSL)
            ICSE=ICSO(NSSL)
            ISZE=ISZO(NSSL)
            IPIC=ISZE/128+1
            NSSL=NSSL-1
            IF (IQUF.NE.0.AND.IPSS.EQ.2) THEN
              IF (NCBF.NE.0) THEN
                CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
                NCBF=0
              END IF
              XSUB=CFUX(XRGT+WPIC(IPIC)*STCO/6.)
              YSUB=CFUY(YRGT+WPIC(IPIC)*STSO/6.)
              SIZS=SIZM*WPIC(IPIC)
              IF (RHTW.GE.0.) RHTW=1.5*HPIC(IPIC)/WPIC(IPIC)
            END IF
          END IF
        END IF
      END IF
C
C Go get the next character.
C
      GO TO 104
C
C
C E N D   O F   S T R I N G   E N C O U N T E R E D
C
C
C Restore the original value of NFNT.
C
  108 NFNT=NFNO
C
C Figure out where the end of the character string fell.  This depends
C on whether the last character was "written down" or "written across".
C In the former case, the bottom of the character box is used; in the
C latter case, the center of the right edge of the character is used.
C
      IF (LCWD.NE.0) THEN
        XEND=XCEN+.5*VEPC*STSO
        YEND=YCEN-.5*VEPC*STCO
      ELSE
        XEND=XRGT
        YEND=YRGT
      END IF
C
C If pass 2 was just completed, wrap it up and quit.
C
      IF (IPSS.EQ.2) THEN
C
        IF (ITEF.EQ.0.OR.ANGD.NE.360.) THEN
          IF (IQUF.EQ.0) THEN
            CALL PLOTIF (0.,0.,2)
          ELSE
            IF (NCBF.NE.0) THEN
              CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
            END IF
          END IF
        END IF
C
C Restore the PLCHMQ parameter specifying the ratio of character height
C to width.
C
        IF (IQUF.NE.0) RHTW=RHWO
C
C Done.
C
        RETURN
C
      ENDIF
C
C Otherwise, prepare for pass 2.
C
      IPSS=2
C
C Adjust the beginning coordinates so as to achieve the desired
C centering.
C
      IF (ICEN.EQ.0) THEN
        XBEG=XBEG-.5*(CNTR+1.)*(XEND-XBEG)
        YBEG=YBEG-.5*(CNTR+1.)*(YEND-YBEG)
      ELSE
        XBEG=XBEG-.5*(DSTR-DSTL)*COSO+.5*(DSTT-DSTB)*SINO
        YBEG=YBEG-.5*(DSTR-DSTL)*SINO-.5*(DSTT-DSTB)*COSO
      END IF
C
C Go back to do pass 2.
C
      GO TO 103
C
C
C   F U N C T I O N   C O D E   P R O C E S S I N G
C
C
C If the current character is invalid, issue an error message and go
C back for the next one.
C
  109 IF (NDPC.LE.0.OR.(NDPC.GE.35.AND.NDPC.NE.45.AND.NDPC.NE.46)) THEN
        IF (IPSS.EQ.2) WRITE (IERU,1001) ICHR,CHRS(ICHR:ICHR)
        GO TO 104
      ENDIF
C
C If the character is an octal digit, retrieve an octal number in INDP
C and jump back to process the character it points to.  NDPC is set to
C a blank so as to avoid problems when low-quality characters are used.
C
      IF (NDPC.GE.27.AND.NDPC.LE.34) THEN
        CALL PCGTPI (CHRS,NCHR,ICHR,8,INDP)
        ICHR=ICHR-1
        NDPC=45
        GO TO 105
      ENDIF
C
C Blanks and commas are just separators; ignore them.
C
      IF (NDPC.EQ.45.OR.NDPC.EQ.46) GO TO 104
C
C Jump to the appropriate section to process an alphabetic character.
C
C             A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,
C             N,  O,  P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z
C
      GO TO (124,118,125,123,121,128,112,126,114,110,115,117,110,
     +       122,110,113,104,111,119,110,116,127,110,129,130,131) , NDPC
C
C Log an error if it's not one of the legal ones.
C
  110 IF (IPSS.EQ.2) WRITE (IERU,1001) ICHR,CHRS(ICHR:ICHR)
      GO TO 104
C
C FONT DEFINITION
C ---------------
C
C Define index offset into INDA.
C
C R is for Roman font.
C
  111 IFNT=IFRO
      GO TO 104
C
C G is for Greek font.
C
  112 IFNT=IFGR
      GO TO 104
C
C SIZE DEFINITION
C ---------------
C
C Define index offset into INDA and set current character height and
C width.
C
C P is for Principal size.
C
  113 ISZE=ISZP
      IPIC=1
      GO TO 132
C
C I is for Indexical size.
C
  114 ISZE=ISZI
      IPIC=2
      GO TO 132
C
C K is for Cartographic size.
C
  115 ISZE=ISZC
      IPIC=3
      GO TO 132
C
C CASE DEFINITION
C ---------------
C
C Set old and current index offsets into INDA and look for character
C count.
C
C  U is for Upper case.
C
  116 ICSP=ICSL
      ICSE=ICSU
      CALL PCGTDI (CHRS,NCHR,ICHR,NCSE)
      GO TO 104
C
C L is for Lower case.
C
  117 ICSP=ICSU
      ICSE=ICSL
      CALL PCGTDI (CHRS,NCHR,ICHR,NCSE)
      GO TO 104
C
C LEVEL DEFINITION
C ----------------
C
C B is for suBscript.  Set the sine and cosine of the offset angle and
C jump to join the superscript code.
C
  118 SINT=SINM
      COST=COSM
      GO TO 120
C
C S is for Superscript.  Set the sine and cosine of the offset angle.
C
  119 SINT=SINP
      COST=COSP
C
C Increment the subscript/superscript level, but to no more than five.
C
  120 NSSL=MIN(NSSL+1,5)
C
C Retrieve the character count, if any, following the B or S.
C
      CALL PCGTDI (CHRS,NCHR,ICHR,NLEV(NSSL))
C
C Compute the size of the offset to the superscript or subscript and
C use that to modify the sine and cosine factors.
C
      OFFS=SSPR*SIZM
      IF (ISZE.NE.ISZP) OFFS=SSIC*SIZM
      COST=OFFS*COST
      SINT=OFFS*SINT
C
C Save all the variables which will be needed to undo the offset on a
C stack.
C
      XCNO(NSSL)=XCEN
      YCNO(NSSL)=YCEN
      XRGO(NSSL)=XRGT
      YRGO(NSSL)=YRGT
      ICSO(NSSL)=ICSE
      ISZO(NSSL)=ISZE
      SNTO(NSSL)=SINT
      CSTO(NSSL)=COST
C
C Reset the coordinates of the right end of the last character.
C
      XRGT=XRGT+COST
      YRGT=YRGT+SINT
C
C Reset the character case and size to values appropriate for subscripts
C and superscripts.
C
      IF (ISZE.EQ.ISZP) THEN
        ISZE=ISZI
        IPIC=2
      ELSE
        ISZE=ISZC
        IPIC=3
        ICSE=ICSU
      END IF
C
        GO TO 132
C
C E is for End of subscripting or superscripting, which is illegal if
C we're not in that mode.
C
  121 IF (NSSL.EQ.0) GO TO 110
C
C Return everything to what it was after drawing the base character.
C
      XCEN=XCNO(NSSL)
      YCEN=YCNO(NSSL)
      XRGT=XRGO(NSSL)
      YRGT=YRGO(NSSL)
      ICSE=ICSO(NSSL)
      ISZE=ISZO(NSSL)
      IPIC=ISZE/128+1
      NSSL=NSSL-1
C
      GO TO 132
C
C N is for Normal, which is illegal if we're not subscripting or
C superscripting.
C
  122 IF (NSSL.EQ.0) GO TO 110
C
C Return everything to what it was after drawing the base character
C except the position of the right edge, in which we just reverse the
C original offset.
C
      XCEN=XCNO(NSSL)
      YCEN=YCNO(NSSL)
      XRGT=XRGT-CSTO(NSSL)
      YRGT=YRGT-SNTO(NSSL)
      ICSE=ICSO(NSSL)
      ISZE=ISZO(NSSL)
      IPIC=ISZE/128+1
      NSSL=NSSL-1
C
      GO TO 132
C
C DIRECTION DEFINITION
C --------------------
C
C D is for Down.  Set the character count to the following decimal
C integer, if there is one, or to a number exceeding the number of
C characters left in the string, otherwise.  Set the flag indicating
C that the last character was "written down".
C
  123 CALL PCGTDI (CHRS,NCHR,ICHR,NDWN)
      IF (NDWN.LE.0) NDWN=NCHR
      LCWD=1
      GO TO 104
C
C A is for Across.  Clear the "down" flags.
C
  124 NDWN=0
      LCWD=0
      GO TO 104
C
C COORDINATE DEFINITION
C ---------------------
C
C C is for Carriage return.  Reset the beginning-of-line position and
C the positions of the center and right edge of the last character.
C Suppress spacing manipulations at the beginning of the new line.
C
  125 XBOL=XBOL+SIZM*VPIC(IPIC)*COSM
      YBOL=YBOL+SIZM*VPIC(IPIC)*SINM
      XCEN=XBOL
      YCEN=YBOL
      XRGT=XBOL
      YRGT=YBOL
      ADDP=0.
      SUBP=0.
      GO TO 132
C
C H is for Horizontal.
C
C Retrieve the integer which follows the H and use it to modify all of
C the current character-position variables.
C
  126 CALL PCGTDI (CHRS,NCHR,ICHR,NUPA)
      DELX=REAL(NUPA)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          IF (NUPA.EQ.0) NUPA=1
          DELX=REAL(NUPA)*WPIC(IPIC)
        END IF
      END IF
      XCEN=XCEN+DELX*STCO
      YCEN=YCEN+DELX*STSO
      XRGT=XRGT+DELX*STCO
      YRGT=YRGT+DELX*STSO
      GO TO 132
C
C V is for Vertical.
C
C Retrieve the integer which follows the V and use it to modify all of
C the current character-position variables.
C
  127 CALL PCGTDI (CHRS,NCHR,ICHR,NUPA)
      DELY=REAL(NUPA)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          IF (NUPA.EQ.0) NUPA=1
          DELY=REAL(NUPA)*VPIC(IPIC)
        END IF
      END IF
      XCEN=XCEN-DELY*STSO
      YCEN=YCEN+DELY*STCO
      XRGT=XRGT-DELY*STSO
      YRGT=YRGT+DELY*STCO
      GO TO 132
C
C FONT CHANGE REQUEST
C -------------------
C
C F is for Font.  The intent is to request use of one of the Hershey
C fonts which are defined by fontcaps, stroking out the characters,
C rather than leaving it to be done by the translator.
C
C Retrieve the integer which follows the F; it determines which font to
C use.  If there is no such integer, go back to the font that was in
C use when PLCHHQ was called.
C
  128 ICHS=ICHR
      CALL PCGTDI (CHRS,NCHR,ICHR,NFNT)
      IF (ICHR.EQ.ICHS) NFNT=NFNO
      GO TO 132
C
C ZOOM REQUEST
C ------------
C
C X is for X zoom, Y is for Y zoom, and Z is for Zoom.  The intent is
C to temporarily change the width, the height, or both dimensions, of
C the characters to a given percentage of normal.
C
C The integer which follows the X, Y, or Z determines the percent
C of normal to be used.  If a "Q" follows the number, it requests a
C vertical adjustment of the current center and right positions to
C make the characters look as if they were all written on the same
C base line.  This has no meaning for "X" and is just ignored.
C
C Process an "X".
C
  129 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      XMZM(1)=ZOOM*XMUL(1)
      XMZM(2)=ZOOM*XMUL(2)
      XMZM(3)=ZOOM*XMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') ICHR=ICHR+1
      END IF
      GO TO 132
C
C Process a "Y".
C
  130 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOLD=YMZM(1)/YMUL(1)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      YMZM(1)=ZOOM*YMUL(1)
      YMZM(2)=ZOOM*YMUL(2)
      YMZM(3)=ZOOM*YMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (IQUF.EQ.0.AND.CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          DELY=.428571428571429*HPIC(IPIC)*(ZOOM-ZOLD)
          XCEN=XCEN-DELY*STSO
          YCEN=YCEN+DELY*STCO
          XRGT=XRGT-DELY*STSO
          YRGT=YRGT+DELY*STCO
        END IF
      END IF
      GO TO 132
C
C Process a "Z".
C
  131 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOLD=YMZM(1)/YMUL(1)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      XMZM(1)=ZOOM*XMUL(1)
      XMZM(2)=ZOOM*XMUL(2)
      XMZM(3)=ZOOM*XMUL(3)
      YMZM(1)=ZOOM*YMUL(1)
      YMZM(2)=ZOOM*YMUL(2)
      YMZM(3)=ZOOM*YMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (IQUF.EQ.0.AND.CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          DELY=.428571428571429*HPIC(IPIC)*(ZOOM-ZOLD)
          XCEN=XCEN-DELY*STSO
          YCEN=YCEN+DELY*STCO
          XRGT=XRGT-DELY*STSO
          YRGT=YRGT+DELY*STCO
        END IF
      END IF
      GO TO 132
C
C A function code has just occurred which changes the size of the
C characters being written or implies that the next character will
C not simply be written after the previous one.  If lower-quality
C characters are in use, this implies that characters collected
C in the buffer should be dumped before going back for the next
C character in the input string.
C
  132 IF (IQUF.NE.0.AND.IPSS.EQ.2) THEN
        IF (NCBF.NE.0)CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
        XSUB=CFUX(XRGT+WPIC(IPIC)*STCO/6.)
        YSUB=CFUY(YRGT+WPIC(IPIC)*STSO/6.)
        NCBF=0
        SIZS=SIZM*WPIC(IPIC)
        IF (RHTW.GE.0.) RHTW=1.5*HPIC(IPIC)/WPIC(IPIC)
      END IF
C
      GO TO 104
C
C Formats.
C
 1001 FORMAT(' PLCHHQ - CHARACTER NUMBER',I3,' (',A1,') IS NOT A',
     +       ' LEGAL FUNCTION CODE')
C
      END
