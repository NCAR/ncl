C
C $Id: epltch.f,v 1.2 1992-11-11 01:55:27 kennison Exp $
C
      PROGRAM EXMPLS
C
C ---------------------------------------------------------------------
C
C Define arrays for column labels and row labels for the plots showing
C the complex and duplex character sets.
C
      CHARACTER*9 CLBL(12)
      CHARACTER*5 RLBL(48)
C
C Define a couple of temporary variables of type CHARACTER.
C
      CHARACTER*6 CTMP
      CHARACTER*7 CHRS
C
C Define the column and row labels.  The character string ':c:r', where
C "c" is the first three characters of a column label and "r" is the
C first character of a row label, is used to select the character to
C be written in that column of that row.
C
      DATA CLBL /'PRU(0000)','PRL(0100)','IRU(0200)','IRL(0300)',
     +           'KRU(0400)','KRL(0500)','PGU(0600)','PGL(0700)',
     +           'IGU(1000)','IGL(1100)','KGU(1200)','KGL(1300)'/
C
      DATA RLBL /'A(01)','B(02)','C(03)','D(04)','E(05)','F(06)',
     +           'G(07)','H(10)','I(11)','J(12)','K(13)','L(14)',
     +           'M(15)','N(16)','O(17)','P(20)','Q(21)','R(22)',
     +           'S(23)','T(24)','U(25)','V(26)','W(27)','X(30)',
     +           'Y(31)','Z(32)','0(33)','1(34)','2(35)','3(36)',
     +           '4(37)','5(40)','6(41)','7(42)','8(43)','9(44)',
     +           '+(45)','-(46)','*(47)','/(50)','((51)',')(52)',
     +           '$(53)','=(54)',' (55)',',(56)','.(57)','     '/
C
C Define character arrays needed for example 1-10.  These are used
C to sidestep problems on various machines with backslashes in
C FORTRAN code.
C
      CHARACTER*1  SPCH(32)
      CHARACTER*32 SOSC
C
C Define a flag which says, if 0, that the first eight plots are to
C occupy eight separate frames and, if 1, that those plots are to be
C compressed onto two frames.
C
      DATA ICMP / 1 /
C
C Define the special characters needed in example 1-10.
C
      DATA SPCH / '!', '"', '#', '$', '%', '&', '''','(', ')', '*',
     +            '+', ',', '-', '.', '/', ':', ';', '<', '=', '>',
     +            '?', '@', '[', '\\',']', '^', '_', '`', '{', '|',
     +            '}', '~'                                        /
C
C ---------------------------------------------------------------------
C
C Open GKS.
C
      CALL OPNGKS
C
C Do a call to SET which allows us to use fractional coordinates.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Examples 1-1 through 1-8. -------------------------------------------
C
C Produce examples of the complex and duplex character sets.
C
C Compute a character-size multiplier, depending on whether the first
C eight plots are being put on eight frames or two.
C
      CSMU=REAL(2-ICMP)
C
C Each pass through the loop on I produces four plots - the first four
C for the complex set, and the second four for the duplex set.
C
      DO 105 I=1,2
C
C Change to the appropriate character set.
C
        CALL PCSETI ('CD',I-1)
C
C Each pass through the following loop produces a single plot.
C
        DO 104 J=1,4
C
C If the first eight plots are to be compressed, re-do the SET call to
C put the plot in a particular quadrant of the frame.
C
          IF (ICMP.NE.0)
     +      CALL SET (.5*REAL(MOD(J-1,2)),.5*REAL(MOD(J-1,2))+.5,
     +                .5-.5*REAL((J-1)/2),   1.-.5*REAL((J-1)/2),
     +                                             0.,1.,0.,1.,1)
C
C Put labels at the top of the frame and along the left edge.
C
          IF (I.EQ.1) THEN
            CALL PLCHHQ (.5,.98,'PLCHHQ - COMPLEX CHARACTER SET',
     +                                                   CSMU*.01,0.,0.)
          ELSE
            CALL PLCHHQ (.5,.98,'PLCHHQ - DUPLEX CHARACTER SET',
     +                                                   CSMU*.01,0.,0.)
          END IF
C
          CALL PLCHHQ (.58,.9267,
     +                 'FUNCTION CODES SPECIFYING SIZE, FONT, AND CASE',
     +                                                CSMU*.00735,0.,0.)
C
          CALL PLCHHQ (.035,.445,':D:STANDARD FORTRAN CHARACTERS',
     +                                                CSMU*.00735,0.,0.)
C
C Force constant spacing of the characters used for the column and row
C labels, so that they will line up better with each other.
C
          CALL PCSETR ('CS',1.25)
C
C Label the columns.
C
          DO 101 K=1,12
            XPOS=.125+.07*REAL(K)
            CALL PLCHHQ (XPOS,.90,CLBL(K)(1:3),CSMU*.006,0.,0.)
            CALL PLCHHQ (XPOS,.88,CLBL(K)(4:9),CSMU*.004,0.,0.)
  101     CONTINUE
C
C Each pass through the following loop produces a single row.
C
          DO 103 K=1,12
C
C Compute the Y coordinate of the row.
C
            YPOS=.9-.07*REAL(K)
C
C Label the row.
C
            CALL PLCHHQ (.085,YPOS,RLBL(12*(J-1)+K)(1:1),CSMU*.006,0.,
     +                                                              -1.)
            CALL PLCHHQ (.105,YPOS,RLBL(12*(J-1)+K)(2:5),CSMU*.004,0.,
     +                                                              -1.)
C
C Each pass through the following loop produces a single character.
C
            DO 102 L=1,12
              XPOS=.125+.07*REAL(L)
              CTMP=':'//CLBL(L)(1:3)//':'//RLBL(12*(J-1)+K)(1:1)
              CALL PLCHHQ (XPOS,YPOS,CTMP,CSMU*.01,0.,0.)
  102       CONTINUE
C
  103     CONTINUE
C
C Return to variable spacing.
C
          CALL PCSETR ('CS',0.)
C
C If eight frames are being produced, advance the frame here.
C
        IF (ICMP.EQ.0) CALL FRAME
C
  104   CONTINUE
C
C If two frames are being produced, advance the frame here.
C
        IF (ICMP.NE.0) CALL FRAME
C
  105 CONTINUE
C
C Return to the complex character set.
C
      CALL PCSETI ('CD',0)
C
C If two frames were produced, re-do the call to SET which allows us to
C use fractional coordinates.
C
      IF (ICMP.NE.0) CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Example 1-9. --------------------------------------------------------
C
C Do a single frame showing various capabilities of PLCHHQ.
C
C Put labels at the top of the plot.
C
      CALL PLCHHQ (.5,.98,'PLCHHQ - VARIOUS CAPABILITIES',.02,0.,0.)
C
C First, write characters at various sizes.
C
      CALL PLCHHQ (.225,.900,'SIZE is -1.0',-1.0,0.,0.)
      CALL PLCHHQ (.225,.873,'SIZE is -.75',-.75,0.,0.)
      CALL PLCHHQ (.225,.846,'SIZE is .015',.015,0.,0.)
      CALL PLCHHQ (.225,.811,'SIZE is .020',.020,0.,0.)
      CALL PLCHHQ (.225,.776,'SIZE is 15.0',15.0,0.,0.)
      CALL PLCHHQ (.225,.742,'SIZE is 20.0',20.0,0.,0.)
C
C Next, write characters at various angles.
C
      CALL PLCHHQ (.225,.453,'   ANGD is   0.',.012,  0.,-1.)
      CALL PLCHHQ (.225,.453,'   ANGD is  45.',.012, 45.,-1.)
      CALL PLCHHQ (.225,.453,'   ANGD is  90.',.012, 90.,-1.)
      CALL PLCHHQ (.225,.453,'   ANGD is 135.',.012,135.,-1.)
      CALL PLCHHQ (.225,.453,'   ANGD is 180.',.012,180.,-1.)
      CALL PLCHHQ (.225,.453,'   ANGD is 225.',.012,225.,-1.)
      CALL PLCHHQ (.225,.453,'   ANGD is 270.',.012,270.,-1.)
      CALL PLCHHQ (.225,.453,'   ANGD is 315.',.012,315.,-1.)
C
C Next, use various values of the centering option.
C
      CALL PLCHHQ (.225,.164,'CNTR is -1.5',.012,0.,-1.5)
      CALL PLCHHQ (.225,.140,'CNTR is -1.0',.012,0.,-1.0)
      CALL PLCHHQ (.225,.116,'CNTR is -0.5',.012,0.,-0.5)
      CALL PLCHHQ (.225,.092,'CNTR is  0.0',.012,0., 0.0)
      CALL PLCHHQ (.225,.068,'CNTR is +0.5',.012,0.,+0.5)
      CALL PLCHHQ (.225,.044,'CNTR is +1.0',.012,0.,+1.0)
      CALL PLCHHQ (.225,.020,'CNTR is +1.5',.012,0.,+1.5)
C
C Turn on the computation of text-extent-vector magnitudes and use
C them to draw a box around a label.  (DRAWBX is not part of PLOTCHAR;
C the code for it appears at the end of this example.)
C
      CALL PCSETI ('TE - TEXT EXTENT FLAG',1)
C
      CALL PLCHHQ (.130,.140,'TEXT EXTENT BOX',.012,33.,0.)
      CALL DRAWBX (.130,.140,33.,.01)
C
      CALL PCSETI ('TE - TEXT EXTENT FLAG',0)
C
C On the right side of the frame, create examples of the various kinds
C of function codes.  First, do them using high-quality characters.
C
      CALL PLCHHQ (.715,.900,'HIGH-QUALITY CHARACTERS USED BELOW',
     +                                                       .012,0.,0.)
C
      CALL PCSETC ('FC','$')
      CALL PLCHHQ (.625,.870,'INPUT STRING',.012,0.,0.)
      CALL PLCHHQ (.625,.840,'------------',.012,0.,0.)
      CALL PLCHHQ (.625,.810,':L:A',.012,0.,0.)
      CALL PLCHHQ (.625,.780,':IGL:A',.012,0.,0.)
      CALL PLCHHQ (.625,.750,'A:S:2:N:+B:S:2:N:',.012,0.,0.)
      CALL PLCHHQ (.625,.720,'A:S:B',.012,0.,0.)
      CALL PLCHHQ (.625,.690,'A:SPU:B',.012,0.,0.)
      CALL PLCHHQ (.625,.660,':GIU:+',.012,0.,0.)
      CALL PLCHHQ (.625,.630,':1045:',.012,0.,0.)
      CALL PLCHHQ (.625,.600,'10:S:10:S:100',.012,0.,0.)
      CALL PLCHHQ (.625,.570,'X:B1:2:S1:3',.012,0.,0.)
      CALL PLCHHQ (.625,.540,'X:B1:2:S:3:N:Y:S:2',.012,0.,0.)
      CALL PLCHHQ (.625,.510,'X:S:A:B:1:NN:ABC',.012,0.,0.)
      CALL PLCHHQ (.625,.480,'1.3648:L1:410:S:-13',.012,0.,0.)
C
      CALL PCSETC ('FC',':')
      CALL PLCHHQ (.875,.870,'RESULT',.012,0.,0.)
      CALL PLCHHQ (.875,.840,'------',.012,0.,0.)
      CALL PLCHHQ (.875,.810,':L:A',.012,0.,0.)
      CALL PLCHHQ (.875,.780,':IGL:A',.012,0.,0.)
      CALL PLCHHQ (.875,.750,'A:S:2:N:+B:S:2:N:',.012,0.,0.)
      CALL PLCHHQ (.875,.720,'A:S:B',.012,0.,0.)
      CALL PLCHHQ (.875,.690,'A:SPU:B',.012,0.,0.)
      CALL PLCHHQ (.875,.660,':GIU:+',.012,0.,0.)
      CALL PLCHHQ (.875,.630,':1045:',.012,0.,0.)
      CALL PLCHHQ (.875,.600,'10:S:10:S:100',.012,0.,0.)
      CALL PLCHHQ (.875,.570,'X:B1:2:S1:3',.012,0.,0.)
      CALL PLCHHQ (.875,.540,'X:B1:2:S:3:N:Y:S:2',.012,0.,0.)
      CALL PLCHHQ (.875,.510,'X:S:A:B:1:NN:ABC',.012,0.,0.)
      CALL PLCHHQ (.875,.480,'1.3648:L1:410:S:-13',.012,0.,0.)
C
C Now, do the same examples using medium-quality characters.
C
      CALL PLCHHQ (.715,.440,'MEDIUM-QUALITY CHARACTERS USED BELOW',
     +                                                       .012,0.,0.)
C
      CALL PCSETI ('QU',1)
C
      CALL PCSETC ('FC','$')
      CALL PLCHHQ (.625,.410,'INPUT STRING',.012,0.,0.)
      CALL PLCHHQ (.625,.380,'------------',.012,0.,0.)
      CALL PLCHHQ (.625,.350,':L:A',.012,0.,0.)
      CALL PLCHHQ (.625,.320,':IGL:A',.012,0.,0.)
      CALL PLCHHQ (.625,.290,'A:S:2:N:+B:S:2:N:',.012,0.,0.)
      CALL PLCHHQ (.625,.260,'A:S:B',.012,0.,0.)
      CALL PLCHHQ (.625,.230,'A:SPU:B',.012,0.,0.)
      CALL PLCHHQ (.625,.200,':GIU:+',.012,0.,0.)
      CALL PLCHHQ (.625,.170,':1045:',.012,0.,0.)
      CALL PLCHHQ (.625,.140,'10:S:10:S:100',.012,0.,0.)
      CALL PLCHHQ (.625,.110,'X:B1:2:S1:3',.012,0.,0.)
      CALL PLCHHQ (.625,.080,'X:B1:2:S:3:N:Y:S:2',.012,0.,0.)
      CALL PLCHHQ (.625,.050,'X:S:A:B:1:NN:ABC',.012,0.,0.)
      CALL PLCHHQ (.625,.020,'1.3648:L1:410:S:-13',.012,0.,0.)
C
      CALL PCSETC ('FC',':')
      CALL PLCHHQ (.875,.410,'RESULT',.012,0.,0.)
      CALL PLCHHQ (.875,.380,'------',.012,0.,0.)
      CALL PLCHHQ (.875,.350,':L:A',.012,0.,0.)
      CALL PLCHHQ (.875,.320,':IGL:A',.012,0.,0.)
      CALL PLCHHQ (.875,.290,'A:S:2:N:+B:S:2:N:',.012,0.,0.)
      CALL PLCHHQ (.875,.260,'A:S:B',.012,0.,0.)
      CALL PLCHHQ (.875,.230,'A:SPU:B',.012,0.,0.)
      CALL PLCHHQ (.875,.200,':GIU:+',.012,0.,0.)
      CALL PLCHHQ (.875,.170,':1045:',.012,0.,0.)
      CALL PLCHHQ (.875,.140,'10:S:10:S:100',.012,0.,0.)
      CALL PLCHHQ (.875,.110,'X:B1:2:S1:3',.012,0.,0.)
      CALL PLCHHQ (.875,.080,'X:B1:2:S:3:N:Y:S:2',.012,0.,0.)
      CALL PLCHHQ (.875,.050,'X:S:A:B:1:NN:ABC',.012,0.,0.)
      CALL PLCHHQ (.875,.020,'1.3648:L1:410:S:-13',.012,0.,0.)
C
      CALL PCSETI ('QU',0)
C
C Advance the frame.
C
      CALL FRAME
C
C Example 1-10. -------------------------------------------------------
C
C Do a single frame showing the medium-quality characters with various
C aspect ratios.
C
C Put labels at the top of the plot.
C
      CALL PLCHMQ (.5,.98,
     +             'PLCHMQ - ALL CHARACTERS - VARIOUS ASPECT RATIOS',
     +                                                        .02,0.,0.)
C
      CALL PLCHMQ (.5,.95,'(Ratio of height to width varies from 2 in th
     +e top group down to .5 in the bottom group.)',.01,0.,0.)
C
C Produce five groups of characters.
C
      DO 111 I=1,32
        SOSC(I:I)=SPCH(I)
  111 CONTINUE
C
      DO 106 I=1,5
        YPOS=1.-.18*REAL(I)
        CALL PCSETR ('HW',2.-1.5*REAL(I-1)/4.)
        CALL PLCHMQ (.5,YPOS+.04,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',
     +                                                        .02,0.,0.)
        CALL PLCHMQ (.5,YPOS    ,'abcdefghijklmnopqrstuvwxyz0123456789',
     +                                                        .02,0.,0.)
        CALL PLCHMQ (.5,YPOS-.04,SOSC(1:32),.02,0.,0.)
  106 CONTINUE
C
C Advance the frame.
C
      CALL FRAME
C
C Example 1-11. -------------------------------------------------------
C
C Do a single frame showing all the characters in the fontcap databases,
C access to which was added in June of 1990.
C
C Double the line width.
C
      CALL GSLWSC (2.)
C
C Do a single frame showing all the characters in the fontcap databases,
C access to which was added in June of 1990.
C
C Put a label at the top of the plot.
C
      CALL PLCHHQ (.5,.98,'PLCHHQ - FONTCAP DATABASES ADDED 6/90',
     +                                                        .02,0.,0.)
C
C Temporarily use the slash as a function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER','/')
C
C Put an explanatory note on the plot.
C
      CALL PLCHHQ (.5,.945,':F1:c selects the ASCII character "c", as sh
     +own in the first two lines.',.01,0.,0.)
C
      CALL PLCHHQ (.5,.925,':Fn:c (2/F18/K/F0/n/F18/K/F0/20) selects the
     + corresponding character from font n.',.01,0.,0.)
C
C Return to a colon as the function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER',':')
C
C Loop through all the available fonts.
C
      DO 108 IFNT=1,20
C
        YCEN=.945-.045*REAL(IFNT)
C
        WRITE (CHRS,'(I7)') IFNT
        CHRS(1:4)='FONT'
        CALL PCSETI ('FN - FONTCAP NUMBER',7)
        CALL PLCHHQ (.005,YCEN,CHRS,.012,0.,-1.)
C
        CALL PCSETI ('FN - FONTCAP NUMBER',IFNT)
C
C Draw all the meaningful characters from the font.
C
        DO 107 ICHR=33,126
          IF (ICHR.LE.79) THEN
            XCEN=.125+.0183*REAL(ICHR-32)
          ELSE
            XCEN=.125+.0183*REAL(ICHR-79)
          END IF
          IF (ICHR.EQ.80) YCEN=YCEN-.0225
          IF (CHAR(ICHR).EQ.':') CALL PCSETC ('FC','!')
          CALL PLCHHQ (XCEN,YCEN,CHAR(ICHR),.01,0.,0.)
          IF (CHAR(ICHR).EQ.':') CALL PCSETC ('FC',':')
  107   CONTINUE
C
C End of loop through fonts.
C
  108 CONTINUE
C
C Restore the fontcap number to 0 to select the PWRITX database.
C
      CALL PCSETI ('FN - FONTCAP NUMBER',0)
C
C Go back to normal line width.
C
      CALL PLOTIF (0.,0.,2)
      CALL GSLWSC (1.)
C
C Advance the frame.
C
      CALL FRAME
C
C Example 1-12. -------------------------------------------------------
C
C Do a single frame showing the use of fontcap databases and some of
C new features added in June of 1990.
C
C Double the line width.
C
      CALL GSLWSC (2.)
C
C Put a label at the top of the plot.
C
      CALL PLCHHQ (.5,.98,'PLCHHQ - FEATURES ADDED 6/90',.02,0.,0.)
C
C Temporarily use the slash as a function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER','/')
C
C Combine characters from several different fonts to produce a single
C line.
C
      CALL PLCHHQ (.5,.910,'/F13/A line containing characters from sever
     +al fonts:  /F8/P/BF13/0/N/=/F5/g/SF13/2/N/+/F5/j/SF13/2/N/',
     +                                                       .012,0.,0.)
C
C Reset the internal parameter 'FN' to 4 and write a line illustrating
C the effect of function codes "Fn", "F", and "F0".  Then reset 'FN'
C to 0.
C
      CALL PCSETI ('FN - FONT NUMBER',4)
      CALL PLCHHQ (.5,.844,'Set ''FN'' (Font Number) to 4 and write a li
     +ne using "F" function codes:',.012,0.,0.)
      CALL PLCHHQ (.5,.820,'Before an F10 - /F10/after an F10 - /F/after
     + an F - /F0/after an F0.',.012,0.,0.)
      CALL PCSETI ('FN - FONT NUMBER',0)
C
C Write lines illustrating various kinds of zooming.
C
      CALL PLCHHQ (.5,.754,'/F13/Unzoomed characters from font 13.',.012
     +,0.,0.)
      CALL PLCHHQ (.5,.730,'/F13X150Q/Characters zoomed in width, using
     +X150Q.',.012,0.,0.)
      CALL PLCHHQ (.5,.700,'/F13Y150Q/Characters zoomed in height, using
     + Y150Q.',.012,0.,0.)
      CALL PLCHHQ (.5,.664,'/F13Z150Q/Characters zoomed both ways, using
     + Z150Q.',.012,0.,0.)
C
C Write a line illustrating non-aligned zooming in height.
C
      CALL PLCHHQ (.5,.630,'/F13/Unaligned zoom of selected characters:
     + /F16Y200/S/Y/cientific /Y200/V/Y/isualization /Y200/G/Y/roup',
     +                                                       .012,0.,0.)
C
C Write lines illustrating the use of 'AS' and 'SS'.
C
      CALL PCSETR ('AS - ADD SPACE BETWEEN CHARACTERS     ',.125)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',  0.)
      CALL PLCHHQ (.5,.564,'/F14/Line written with ''AS'' = .125 and ''S
     +S'' = 0.',.012,0.,0.)
      CALL PCSETR ('AS - ADD SPACE BETWEEN CHARACTERS     ',  0.)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',  0.)
      CALL PLCHHQ (.5,.540, '/F14/Line written with ''AS'' = 0. and ''SS
     +'' = 0.',.012,0.,0.)
      CALL PCSETR ('AS - ADD SPACE BETWEEN CHARACTERS     ',  0.)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',.125)
      CALL PLCHHQ (.5,.516, '/F14/Line written with ''AS'' = 0. and ''SS
     +'' = .125',.012,0.,0.)
      CALL PCSETR ('AS - ADD SPACE BETWEEN CHARACTERS     ',  0.)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',  0.)
C
C Illustrate the difference between inexact centering and exact
C centering of a single character.
C
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',.128)
C
      CALL PLCHHQ (.1,.455,'/F7/This "g" is centered on the cross using
     + CNTR = 0. and ''CE'' = 0:',.012,0.,-1.)
      CALL LINE (.880,.455,.920,.455)
      CALL LINE (.900,.435,.900,.475)
      CALL PCSETI ('CE - CENTERING OPTION',0)
      CALL PLCHHQ (.9,.455,'/F7/g',.025,0.,0.)
      CALL PCSETI ('CE - CENTERING OPTION',0)
C
      CALL PLCHHQ (.1,.405,'/F7/This "g" is centered on the cross using
     + CNTR = 0. and ''CE'' = 1:',.012,0.,-1.)
      CALL LINE (.880,.405,.920,.405)
      CALL LINE (.900,.385,.900,.425)
      CALL PCSETI ('CE - CENTERING OPTION',1)
      CALL PLCHHQ (.9,.405,'/F7/g',.025,0.,0.)
      CALL PCSETI ('CE - CENTERING OPTION',0)
C
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',0.)
C
C Put some large characters on a grid to show the digitization.
C
      CALL PLCHHQ (.5,.312,'Large characters on digitization grid.  X''s
     + mark edge points of the characters.',.01,0.,0.)
C
      WDTH=.150
      XLFT=.500-48.*(WDTH/16.)
      XRGT=.500+48.*(WDTH/16.)
      YBOT=.150-11.*(WDTH/16.)
      YTOP=.150+14.*(WDTH/16.)
C
      CALL PLOTIF (0.,0.,2)
      CALL GSLWSC (1.)
C
      DO 109 I=-48,48
        XCRD=.500+REAL(I)*(WDTH/16.)
        CALL LINE (XCRD,YBOT,XCRD,YTOP)
  109 CONTINUE
C
      DO 110 J=-11,14
        YCRD=.150+REAL(J)*(WDTH/16.)
        CALL LINE (XLFT,YCRD,XRGT,YCRD)
  110 CONTINUE
C
      CALL PLOTIF (0.,0.,2)
      CALL GSLWSC (2.)
C
      XCRD=.500-45.*(WDTH/16.)
      YCRD=.150
      CALL LINE (XCRD-WDTH/32.,YCRD-WDTH/32.,
     +           XCRD+WDTH/32.,YCRD+WDTH/32.)
      CALL LINE (XCRD-WDTH/32.,YCRD+WDTH/32.,
     +           XCRD+WDTH/32.,YCRD-WDTH/32.)
      CALL PLCHHQ (XCRD,YCRD,'/F9/A',WDTH,0.,-1.)
      CALL PCGETR ('XE - X COORDINATE AT END OF STRING',XCRD)
      CALL LINE (XCRD-WDTH/32.,YCRD-WDTH/32.,
     +           XCRD+WDTH/32.,YCRD+WDTH/32.)
      CALL LINE (XCRD-WDTH/32.,YCRD+WDTH/32.,
     +           XCRD+WDTH/32.,YCRD-WDTH/32.)
      CALL PLCHHQ (XCRD,YCRD,'/F9/B',WDTH,0.,-1.)
      CALL PCGETR ('XE - X COORDINATE AT END OF STRING',XCRD)
      CALL LINE (XCRD-WDTH/32.,YCRD-WDTH/32.,
     +           XCRD+WDTH/32.,YCRD+WDTH/32.)
      CALL LINE (XCRD-WDTH/32.,YCRD+WDTH/32.,
     +           XCRD+WDTH/32.,YCRD-WDTH/32.)
      CALL PLCHHQ (XCRD,YCRD,'/F9/C',WDTH,0.,-1.)
      CALL PCGETR ('XE - X COORDINATE AT END OF STRING',XCRD)
      CALL LINE (XCRD-WDTH/32.,YCRD-WDTH/32.,
     +           XCRD+WDTH/32.,YCRD+WDTH/32.)
      CALL LINE (XCRD-WDTH/32.,YCRD+WDTH/32.,
     +           XCRD+WDTH/32.,YCRD-WDTH/32.)
      CALL PLCHHQ (XCRD,YCRD,'/F9/D',WDTH,0.,-1.)
      CALL PCGETR ('XE - X COORDINATE AT END OF STRING',XCRD)
      CALL LINE (XCRD-WDTH/32.,YCRD-WDTH/32.,
     +           XCRD+WDTH/32.,YCRD+WDTH/32.)
      CALL LINE (XCRD-WDTH/32.,YCRD+WDTH/32.,
     +           XCRD+WDTH/32.,YCRD-WDTH/32.)
C
C Return to a colon as the function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER',':')
C
C Go back to normal line width.
C
      CALL PLOTIF (0.,0.,2)
      CALL GSLWSC (1.)
C
C Advance the frame.
C
      CALL FRAME
C
C ---------------------------------------------------------------------
C
C Close GKS.
C
      CALL CLSGKS
C
C Done.
C
      STOP
C
C ---------------------------------------------------------------------
C
      END
      SUBROUTINE DRAWBX (XCEN,YCEN,ANGD,XTRA)
      CALL PCGETR ('DL - DISTANCE LEFT  ',DSTL)
      CALL PCGETR ('DR - DISTANCE RIGHT ',DSTR)
      CALL PCGETR ('DB - DISTANCE BOTTOM',DSTB)
      CALL PCGETR ('DT - DISTANCE TOP   ',DSTT)
      ANGR=.017453292519943*ANGD
      SINA=SIN(ANGR)
      COSA=COS(ANGR)
      XFRA=CUFX(XCEN)
      YFRA=CUFY(YCEN)
      XALB=XFRA-(DSTL+XTRA)*COSA+(DSTB+XTRA)*SINA
      YALB=YFRA-(DSTL+XTRA)*SINA-(DSTB+XTRA)*COSA
      XARB=XFRA+(DSTR+XTRA)*COSA+(DSTB+XTRA)*SINA
      YARB=YFRA+(DSTR+XTRA)*SINA-(DSTB+XTRA)*COSA
      XART=XFRA+(DSTR+XTRA)*COSA-(DSTT+XTRA)*SINA
      YART=YFRA+(DSTR+XTRA)*SINA+(DSTT+XTRA)*COSA
      XALT=XFRA-(DSTL+XTRA)*COSA-(DSTT+XTRA)*SINA
      YALT=YFRA-(DSTL+XTRA)*SINA+(DSTT+XTRA)*COSA
      CALL PLOTIF (XALB,YALB,0)
      CALL PLOTIF (XARB,YARB,1)
      CALL PLOTIF (XART,YART,1)
      CALL PLOTIF (XALT,YALT,1)
      CALL PLOTIF (XALB,YALB,1)
      CALL PLOTIF (0.,0.,2)
      RETURN
      END
