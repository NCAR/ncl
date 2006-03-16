
      PROGRAM TPWRTX
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL TPWRTX1(IERR)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE TPWRTX1(IERROR)
C
C PURPOSE                To provide a demonstration of PWRITX
C                        and to test PWRITX with an example.
C
C USAGE                  CALL TPWRTX1 (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test is successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C               PWRITX TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is written on unit 6.
C
C                        In addition, four frames are produced.  The
C                        first three frames contain complete
C                        character plots, and the fourth frame
C                        tests various settings of the function
C                        codes.  To determine if the test is
C                        successful, it is necessary to examine these
C                        plots.
C
C PRECISION              Single
C
C REQUIRED ROUTINES      PWRITX
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN
C
C ALGORITHM              TPWRTX calls the software character drawing
C                        subroutine PWRITX once for twelve different
C                        function codes for 46 separate characters.
C                        This produces a total of 552 characters on
C                        four separate plots.  Each plot contains a
C                        grid of characters with the principle Roman
C                        characters in the first column and their
C                        other representations, produced with different
C                        function codes, across each row.  Each function
C                        code has a mnemonic interpretation (e.g.,
C                        PRU - Principle Roman Upper,  IGL - Indexical
C                        Greek Lower).  In the first four plots, each
C                        column is labelled with its function code.
C                        The fifth plot invokes PWRITX with various
C                        function codes.
C
      CHARACTER*1  DAT(48)
C
C DAT contains the standard character set
C
      DATA  DAT(1),DAT(2),DAT(3),DAT(4),DAT(5),DAT(6),DAT(7),DAT(8),
     1      DAT( 9),DAT(10),DAT(11),DAT(12),DAT(13),DAT(14),DAT(15),
     2      DAT(16),DAT(17),DAT(18),DAT(19),DAT(20),DAT(21),DAT(22),
     3      DAT(23),DAT(24),DAT(25),DAT(26),DAT(27),DAT(28),DAT(29),
     4      DAT(30),DAT(31),DAT(32),DAT(33),DAT(34),DAT(35),DAT(36),
     5      DAT(37),DAT(38),DAT(39),DAT(40),DAT(41),DAT(42),DAT(43),
     6      DAT(44),DAT(45),DAT(46),DAT(47),DAT(48) /
     7       'A',    'B',    'C',    'D',    'E',    'F',    'G',
     8       'H',    'I',    'J',    'K',    'L',    'M',    'N',
     9       'O',    'P',    'Q',    'R',    'S',    'T',    'U',
     +       'V',    'W',    'X',    'Y',    'Z',    '0',    '1',
     1       '2',    '3',    '4',    '5',    '6',    '7',    '8',
     2       '9',    '+',    '-',    '*',    '/',    '(',    ')',
     3       '$',    '=',    ' ',    ',',    '.',    ' '  /
C
C Use normalization transformation 0
C
      CALL GSELNT (0)
C
C A separate frame is produced for each iteration through this loop
C
      DO 160 K=1,4
C
C  Label the column and change the function code
C
         DO 150 J=1,12
            XPOS = REAL(J*80-39) / 1024.
            GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1             110,120),J
   10       CALL PWRITX (XPOS,.9375,'PRU',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''PRU''',5,1,0,0)
            GO TO 130
   20       CALL PWRITX (XPOS,.9375,'PRL',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''PRL''',5,1,0,0)
            GO TO 130
   30       CALL PWRITX (XPOS,.9375,'IRU',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''IRU''',5,1,0,0)
            GO TO 130
   40       CALL PWRITX (XPOS,.9375,'IRL',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''IRL''',5,1,0,0)
            GO TO 130
   50       CALL PWRITX (XPOS,.9375,'KRU',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''KRU''',5,1,0,0)
            GO TO 130
   60       CALL PWRITX (XPOS,.9375,'KRL',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''KRL''',5,1,0,0)
            GO TO 130
   70       CALL PWRITX (XPOS,.9375,'PGU',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''PGU''',5,1,0,0)
            GO TO 130
   80       CALL PWRITX (XPOS,.9375,'PGL',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''PGL''',5,1,0,0)
            GO TO 130
   90       CALL PWRITX (XPOS,.9375,'IGU',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''IGU''',5,1,0,0)
            GO TO 130
  100       CALL PWRITX (XPOS,.9375,'IGL',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''IGL''',5,1,0,0)
            GO TO 130
  110       CALL PWRITX (XPOS,.9375,'KGU',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''KGU''',5,1,0,0)
            GO TO 130
  120       CALL PWRITX (XPOS,.9375,'KGL',3,16,0,0)
            CALL PWRITX (1./1024.,1./1024.,'''KGL''',5,1,0,0)
C
  130       CONTINUE
C
C           Draw twelve characters with the same function code
C
            DO 140 I=1,12
               YPOS = REAL( 980-I*80 ) / 1024.
               IF = I+(K-1)*12
               CALL PWRITX (XPOS,YPOS,DAT(IF),1,1,0,0)
  140       CONTINUE
C
C           Return function to Principle Roman Upper to label column
C
            CALL PWRITX (1./1024.,1./1024.,'''PRU''',5,1,0,0)
  150    CONTINUE
C
C        Label frame
C
         CALL PWRITX(.5,1000./1024.,
     +               'DEMONSTRATION PLOT FOR PWRITX',29,1,0,0)
         CALL FRAME
  160 CONTINUE
C
C  Test more function codes.
C
C  Tests:
C    Upper and lower case function codes
C    Sub- and Super-scripting and Normal function codes
C    Down and Across function codes
C    Direct Character access
C    X and Y coordinate control, Carriage control function codes
C    Orientation of string (argument to PWRITX)
C
C
C  Test direct character access and string orientation
C
      CALL PWRITX(.5,.5,'''546''',5,3,0,0)
      CALL PWRITX(.5,.5,'''H9L''ANGLE OF ''U''30',19,18,30,-1)
      CALL PWRITX(.5,.5,'''H-9L''ANGLE OF ''U''190',21,18,190,-1)
C
C  Upper and Lower case
C
      CALL PWRITX(.65,.25,'2 ''L2''LOWER, 3 ''U3''UPPER',24,1,0,0)
C
C  Level definitions (sub and superscripting)
C
      CALL PWRITX(.95,.15,'THIS IS ''U1''S''S''UPERSCRIPTING',29,0,0,1)
      CALL PWRITX(.95,.1,'''N''THIS IS ''L1''S''B''UBSCRIPTING',
     -            30,0,0,1)
      CALL PWRITX(.95,.05,'''N''SHOW ''U1''U''S''SE OF''NU''NORMAL',
     -            31,0,0,1)
C
C  Direction definitions
C
      CALL PWRITX(.05,.5,'DO''D''WNA''A''CROSS',16,0,0,-1)
C
C  Coordinate definitions
C
      CALL PWRITX(.3,.85,
     - '''L''U''V7''S''V7''E''V7'' ''V7U1''V''V7'' FOR VERTICAL STEPS',
     -            49,0,0,0)
      CALL PWRITX(.25,.6,
     -            '''U''SHIFT''H11''.''H11''.''H11''.''H11''.RIGHT',
     -            37,14,90,-1)
      CALL PWRITX(.45,.6,
     -            'SHIFT''H-30''.''H-11''.''H-11''.''H-11''.LEFT',
     -            37,14,90,-1)
      CALL PWRITX(.8,.8,'''L3''USE C''CL''FOR''C''CARRIAGE''C''RETURNS',
     -            37,16,0,0)
      CALL PWRITX(.1,.1,'''UX50Y50''( X50, Y50 )''X99Y99''( X99, Y99 )',
     -            41,14,0,-1)
C
C        Label frame
C
         CALL PWRITX(.5,1000./1024.,
     +               'DEMONSTRATION PLOT FOR PWRITX',29,1,0,0)
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' PWRITX TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
