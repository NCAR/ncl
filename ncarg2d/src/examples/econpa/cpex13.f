
        PROGRAM CPEX13
C
C This program produces a detailed picture of the contents of a typical
C CONPACK-produced area map.  It then shows three different situations
C that tend to cause problems.  This example is intended to be viewed
C while reading the text from the programmer document for CONPACK that
C describes each of the four frames.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Define various local parameters.
C
        PARAMETER (LRWK=5000,LIWK=5000,LAMA=20000,NCLV=4)
C
C Declare the basic data array, a real workspace array, and an integer
C workspace array.
C
        DIMENSION ZDAT(7,7),RWRK(LRWK),IWRK(LIWK)
C
C Declare a bigger data array to use for rho/theta data.
C
        DIMENSION RHTH(10,25)
C
C Declare an area-map array.
C
        DIMENSION IAMA(LAMA)
C
C Declare an array to hold contour levels.
C
        DIMENSION CLEV(NCLV)
C
C Define the contour levels at which contours are to be generated.
C
        DATA CLEV / .25,.52,.90,1.40 /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Set internal parameters of AREAS that affect the appearance of the
C debug plots produced by ARDBPX.
C
        CALL ARSETR ('ID - IDENTIFIER DISTANCE',.008)
        CALL ARSETR ('IS - IDENTIFIER SIZE',.008)
        CALL ARSETR ('AL - ARROWHEAD LENGTH',0.)
        CALL ARSETR ('AW - ARROWHEAD WIDTH',0.)
C
C Tell the dash package to use alternating solids and gaps.  This
C pattern will be used for the circles on frame 3.
C
        CALL DPSETC ('DPT - DASH PATTERN','$_')
C
C Tell PLOTCHAR to use font number 25 (a filled font) and to outline
C each character.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
C
C Tell PLOTCHAR to tell the Bezier package to reproduce the curves
C outlining the characters with a little less fidelity.  This cuts
C down on the size of the metafile.
C
        CALL PCSETR ('FB - FIDELITY OF BEZIER CURVES',.00015)
C
C
C ***** FIRST FRAME BEGINS ********************************************
C
C Put a label at the top of the first frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.98),
     +          'A DETAILED VIEW OF A CONPACK AREA MAP (EDGE GROUP 3)',
     +                                                       .015,0.,0.)
C
C Put informative labels at the top and bottom of the frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.95),'This is a simple case that demo
     +nstrates all the essential features of a CONPACK area map.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.926),'All the edge segments in group
     + 3 are shown, each with its own left and right area identifiers.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.062),'See the CONPACK programmer doc
     +ument for a complete description of this area map.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.038),'Also see the frames that follo
     +w for examples of situations in which problems can arise.',
     +                                                       .012,0.,0.)
C
C Define the mapping from the "user" system to the fractional system.
C
        CALL SET    (.05,.95,.05,.95,-.1,1.1,-.1,1.1,1)
C
C Tell CONPACK not to call SET.
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
C
C Tell CONPACK to map output coordinates.  Included below is a special
C version of CPMPXY that, when 'MAP' = 1, does an identity mapping, but
C generates some out-of-range values.
C
        CALL CPSETI ('MAP - MAPPING FLAG',1)
C
C Tell CONPACK what to expect as an out-of-range value in the output
C from CPMPXY.
C
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Tell CONPACK not to select contour levels.  We'll do it.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION METHOD',0)
C
C Tell CONPACK how many contour levels to use and exactly what those
C levels are.
C
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)

        DO 103 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPSETR ('CLV - CONTOUR LEVEL',CLEV(ICLV))
          CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',3)
  103   CONTINUE
C
C Tell CONPACK to position line labels using the regular scheme and
C modify a few parameters so as to get labels in particular places.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',2)
C
        CALL CPSETR ('RC1 - REGULAR SCHEME CONSTANT 1',.55)
        CALL CPSETR ('RC2 - REGULAR SCHEME CONSTANT 2',.85)
        CALL CPSETR ('RC3 - REGULAR SCHEME CONSTANT 3',0.)
C
        CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',3.)
C
C Get rid of the informational label.
C
        CALL CPSETC ('ILT - INFORMATIONAL LABEL TEXT',' ')
C
C Tell CONPACK how to map the data grid into coordinates to be
C delivered to CPMPXY.
C
        CALL CPSETR ('XC1 - X COORDINATE FOR I = 1',0.)
        CALL CPSETR ('XCM - X COORDINATE FOR I = M',1.)
        CALL CPSETR ('YC1 - Y COORDINATE FOR J = 1',0.)
        CALL CPSETR ('YCN - Y COORDINATE FOR J = N',1.)
C
C Tell CONPACK what value is used in the data as a special value.
C
        CALL CPSETR ('SPV - SPECIAL VALUE',1.E36)
C
C Generate a simple two-dimensional data field.
C
        DO 102 I=1,7
          XCRD=REAL(I-1)/6.
          DO 101 J=1,7
            YCRD=REAL(J-1)/6.
            ZDAT(I,J)=XCRD**2+YCRD**2
  101     CONTINUE
  102   CONTINUE
C
C Put some special values in the lower left corner of the data field.
C
        ZDAT(1,1)=1.E36
        ZDAT(1,2)=1.E36
        ZDAT(2,1)=1.E36
        ZDAT(2,2)=1.E36
C
C Tell CONPACK the dimensions of its data array, the real workspace
C array, and the integer workspace array, so that it can initialize
C itself to work with those arrays.
C
        CALL CPRECT (ZDAT,7,7,7,RWRK,LRWK,IWRK,LIWK)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Put into the area map the viewport perimeter, the boundary of the
C "invisible" area (the area in which CPMPXY returns the value 'ORV'),
C the edge of the grid, the edges of the special-value areas, and the
C contour lines.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Put the label boxes into the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Call a modified version of the AREAS routine ARDBPA to draw the
C contents of the area map.
C
        CALL ARDBPX (IAMA,3)
C
C Label features of interest on the plot.
C
        CALL PLCHHQ (CFUX(.90),CFUY(.11),'EDGE OF PLOTTER FRAME  ',
     +                                                  .008,0.,1.)
C
        CALL PLCHHQ (CFUX(.86),CFUY(.15),'EDGE OF VIEWPORT  ',
     +                                             .008,0.,1.)
C
        CALL PLCHHQ (CFUX(.80),CFUY(.32),'EDGE',
     +                               .008,0.,0.)
        CALL PLCHHQ (CFUX(.80),CFUY(.30),'OF',
     +                             .008,0.,0.)
        CALL PLCHHQ (CFUX(.80),CFUY(.28),'GRID',
     +                               .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.30),CFUY(.34),'THIS IS A',
     +                                    .008,0.,0.)
        CALL PLCHHQ (CFUX(.30),CFUY(.32),'SPECIAL-VALUE',
     +                                        .008,0.,0.)
        CALL PLCHHQ (CFUX(.30),CFUY(.30),'AREA, IN WHICH ALL',
     +                                             .008,0.,0.)
        CALL PLCHHQ (CFUX(.30),CFUY(.28),'DATA VALUES ARE ',
     +                                           .008,0.,0.)
        CALL PLCHHQ (CFUX(.30),CFUY(.26),'EQUAL TO ''SPV''',
     +                                           .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.77),CFUY(.83),'IN THIS AREA,',
     +                                        .008,0.,0.)
        CALL PLCHHQ (CFUX(.77),CFUY(.81),'CPMPXY RETURNS',
     +                                         .008,0.,0.)
        CALL PLCHHQ (CFUX(.77),CFUY(.79),'COORDINATE VALUES',
     +                                            .008,0.,0.)
        CALL PLCHHQ (CFUX(.77),CFUY(.77),'EQUAL TO ''ORV''; AREA',
     +                                                 .008,0.,0.)
        CALL PLCHHQ (CFUX(.77),CFUY(.75),'IS INVISIBLE UNDER',
     +                                             .008,0.,0.)
        CALL PLCHHQ (CFUX(.77),CFUY(.73),'MAPPING',
     +                                  .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.505),CFUY(.61),'HERE ARE',
     +                                    .008,0.,0.)
        CALL PLCHHQ (CFUX(.505),CFUY(.59),'TWO',
     +                               .008,0.,0.)
        CALL PLCHHQ (CFUX(.505),CFUY(.57),'LABEL BOXES',
     +                                       .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.28),CFUY(.43),'CONTOUR BAND 1',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.30),CFUY(.53),'CONTOUR BAND 2',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.36),CFUY(.67),'CONTOUR BAND 3',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.52),CFUY(.75),'CONTOUR BAND 4',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.48),CFUY(.30),'CONTOUR',
     +                                  .008,0.,0.)
        CALL PLCHHQ (CFUX(.48),CFUY(.28),'(LEVEL 1)',
     +                                    .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.62),CFUY(.31),'CONTOUR',
     +                                  .008,0.,0.)
        CALL PLCHHQ (CFUX(.62),CFUY(.29),'(LEVEL 2)',
     +                                    .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.73),CFUY(.40),'CONTOUR',
     +                                  .008,0.,0.)
        CALL PLCHHQ (CFUX(.73),CFUY(.38),'(LEVEL 3)',
     +                                    .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.745),CFUY(.615),'CONTOUR',
     +                                    .008,0.,0.)
        CALL PLCHHQ (CFUX(.745),CFUY(.596),'(LEVEL 4)',
     +                                      .008,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C
C ***** SECOND FRAME BEGINS *******************************************
C
C Put a label at the top of the second frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.98),'CONPACK AREA MAPS - FRAME 2',
     +                                                       .015,0.,0.)
C
C Put informative labels at the top and bottom of the frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.95),'This frame shows what happens w
     +hen CPMPXY can''t do inverses (or incorrectly says it can''t).',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.926),'The algorithm that generates t
     +he edge of the invisible area doesn''t work so well then.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.062),'In this case, the effects aren
     +''t too bad; more serious effects are sometimes seen.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.038),'See the CONPACK programmer doc
     +ument for a complete discussion of this problem.',
     +                                                       .012,0.,0.)
C
C Change to mapping function 2 to illustrate the problem with a CPMPXY
C that doesn't do inverses.  'MAP' = 2 is just like 'MAP' = 1 except
C that it doesn't do inverses.
C
        CALL CPSETI ('MAP - MAPPING FLAG',2)
C
C Tell CONPACK the dimensions of its data array, the real workspace
C array, and the integer workspace array, so that it can initialize
C itself to work with those arrays.
C
        CALL CPRECT (ZDAT,7,7,7,RWRK,LRWK,IWRK,LIWK)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Put into the area map the viewport perimeter, the boundary of the
C "invisible" area (the area in which CPMPXY returns the value 'ORV'),
C the edge of the grid, the edges of the special-value areas, and the
C contour lines.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Put the label boxes into the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Call a modified version of the AREAS routine ARDBPA to draw the
C contents of the area map.
C
        CALL ARDBPX (IAMA,3)
C
C Label features of interest on the plot.
C
        CALL PLCHHQ (CFUX(.90),CFUY(.11),'EDGE OF PLOTTER FRAME  ',
     +                                                  .008,0.,1.)
C
        CALL PLCHHQ (CFUX(.86),CFUY(.15),'EDGE OF VIEWPORT  ',
     +                                             .008,0.,1.)
C
        CALL PLCHHQ (CFUX(.80),CFUY(.32),'EDGE',
     +                               .008,0.,0.)
        CALL PLCHHQ (CFUX(.80),CFUY(.30),'OF',
     +                             .008,0.,0.)
        CALL PLCHHQ (CFUX(.80),CFUY(.28),'GRID',
     +                               .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.30),CFUY(.31),'SPECIAL-VALUE',
     +                                        .008,0.,0.)
        CALL PLCHHQ (CFUX(.30),CFUY(.29),'AREA',
     +                               .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.77),CFUY(.78),'INVISIBLE AREA',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.505),CFUY(.59),'LABEL BOXES',
     +                                       .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.28),CFUY(.43),'CONTOUR BAND 1',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.30),CFUY(.53),'CONTOUR BAND 2',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.36),CFUY(.67),'CONTOUR BAND 3',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.52),CFUY(.75),'CONTOUR BAND 4',
     +                                         .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.48),CFUY(.30),'CONTOUR',
     +                                  .008,0.,0.)
        CALL PLCHHQ (CFUX(.48),CFUY(.28),'AT LEVEL 1',
     +                                     .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.62),CFUY(.31),'CONTOUR',
     +                                  .008,0.,0.)
        CALL PLCHHQ (CFUX(.62),CFUY(.29),'AT LEVEL 2',
     +                                     .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.73),CFUY(.40),'CONTOUR',
     +                                  .008,0.,0.)
        CALL PLCHHQ (CFUX(.73),CFUY(.38),'AT LEVEL 3',
     +                                     .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.745),CFUY(.615),'CONTOUR',
     +                                    .008,0.,0.)
        CALL PLCHHQ (CFUX(.745),CFUY(.596),'AT LEVEL 4',
     +                                       .008,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C
C ***** THIRD FRAME BEGINS ********************************************
C
C Put a label at the top of the third frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.98),'CONPACK AREA MAPS - FRAME 3',
     +                                                       .015,0.,0.)
C
C Put informative labels at the top and bottom of the frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.95),'Sometimes a segment of a contou
     +r line is parallel to and just barely inside the edge of the grid.
     +',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.926),'The dashed circles in the area
     + map below show the locations of two such contour-line segments.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.074), 'Prior to version 4, the outer
     + area identifier for such a segment could "leak" outside the grid.
     +',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.050),'Now, conflicting information f
     +rom near-coincident segments is resolved in such a way as to avoid
     + problems.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.026),'See the CONPACK programmer doc
     +ument for a complete discussion of this problem.',
     +                                                       .012,0.,0.)
C
C Change the mapping back to what it was on the first frame.
C
        CALL CPSETI ('MAP - MAPPING FLAG',1)
C
C Generate data. The value "4.984615" has been carefully chosen to
C create the problem.
C
        DO 105 I=1,7
          XCRD=REAL(I-1)/6.
          DO 104 J=1,7
            YCRD=REAL(J-1)/6.
            ZDAT(I,J)=4.984615*((XCRD-.58333333)**2+(YCRD-.58333333)**2)
  104     CONTINUE
  105   CONTINUE
C
C Salt in some special values.
C
        ZDAT(1,1)=1.E36
        ZDAT(1,2)=1.E36
        ZDAT(2,1)=1.E36
        ZDAT(2,2)=1.E36
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Tell CONPACK the dimensions of its data array, the real workspace
C array, and the integer workspace array, so that it can initialize
C itself to work with those arrays.
C
        CALL CPRECT (ZDAT,7,7,7,RWRK,LRWK,IWRK,LIWK)
C
C Put into the area map the viewport perimeter, the boundary of the
C "invisible" area (the area in which CPMPXY returns the value 'ORV'),
C the edge of the grid, the edges of the special-value areas, and the
C contour lines.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Call a modified version of the AREAS routine ARDBPA to draw the
C contents of the area map.
C
        CALL ARDBPX (IAMA,3)
C
C Draw a couple of circles around the problem areas.
C
        CALL CIRCLE (.1+.8*CUFX(.58333333),.1+.8*CUFY(1.),.065)
        CALL CIRCLE (.1+.8*CUFX(1.),.1+.8*CUFY(.58333333),.065)
C
C Label features of interest on the plot.
C
        CALL PLCHHQ (CFUX(.90),CFUY(.11),'EDGE OF PLOTTER FRAME  ',
     +                                                  .008,0.,1.)
C
        CALL PLCHHQ (CFUX(.86),CFUY(.15),'EDGE OF VIEWPORT  ',
     +                                             .008,0.,1.)
C
        CALL PLCHHQ (CFUX(.80),CFUY(.31),'EDGE',
     +                               .008,0.,0.)
        CALL PLCHHQ (CFUX(.80),CFUY(.29),'OF',
     +                             .008,0.,0.)
        CALL PLCHHQ (CFUX(.80),CFUY(.27),'GRID',
     +                               .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.30),CFUY(.31),'SPECIAL-VALUE',
     +                                        .008,0.,0.)
        CALL PLCHHQ (CFUX(.30),CFUY(.29),'AREA',
     +                               .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.77),CFUY(.78),'INVISIBLE AREA',
     +                                         .008,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C
C ***** FOURTH FRAME BEGINS *******************************************
C
C Put a label at the top of the fourth frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.98),'CONPACK AREA MAPS - FRAME 4',
     +                                                       .015,0.,0.)
C
C Put informative labels at the top and bottom of the frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.95),'Some mappings transform two dif
     +ferent parts of the grid to the same place in user coordinate spac
     +e.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.926),'The :F33:r/q:F: mapping used h
     +ere maps the grid into a doughnut; left and right edges map to the
     + same line.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.074),'Similarly, EZMAP frequently ma
     +ps the left and right edges of the grid into the same great circle
     +.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.050), 'This can sometimes cause area
     + identifiers for the outside of the grid to appear to apply to the
     + inside.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.026),'See the CONPACK programmer doc
     +ument for a complete discussion of this problem.',
     +                                                       .012,0.,0.)
C
C Redefine the mapping from the user system to the fractional system.
C
        CALL SET    (.05,.95,.05,.95,-1.1,1.1,-1.1,1.1,1)
C
C Change the mapping function to be used.  For 'MAP' = 3, we get a
C standard rho/theta mapping.
C
        CALL CPSETI ('MAP - MAPPING FLAG',3)
C
C Tell CONPACK that no out-of-range values will be returned.
C
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',0.)
C
C Change the X and Y values assumed to correspond to the edges of the
C data grid.
C
        CALL CPSETR ('XC1 - X COORDINATE FOR I = 1',.3)
        CALL CPSETR ('XCM - X COORDINATE FOR I = M',1.)
        CALL CPSETR ('YC1 - Y COORDINATE FOR J = 1',  0.)
        CALL CPSETR ('YCN - Y COORDINATE FOR J = N',360.)
C
C Generate rho/theta data.
C
        DO 107 I=1,10
          RHO=.3+.07*REAL(I)
          DO 106 J=1,25
            THETA=.017453292519943*REAL(15*J-15)
            RHTH(I,J)=2.*RHO*COS(THETA)**2+RHO*SIN(THETA)**2
  106     CONTINUE
  107   CONTINUE
C
C Tell CONPACK the dimensions of its data array, the real workspace
C array, and the integer workspace array, so that it can initialize
C itself to work with those arrays.
C
        CALL CPRECT (RHTH,10,10,25,RWRK,LRWK,IWRK,LIWK)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Put into the area map the viewport perimeter, the  edge of the grid,
C the edges of the special-value areas, and the contour lines.
C
        CALL CPCLAM (RHTH,RWRK,IWRK,IAMA)
C
C Call a modified version of the AREAS routine ARDBPA to draw the
C contents of the area map.
C
        CALL ARDBPX (IAMA,3)
C
C Label features of interest on the plot.
C
        CALL PLCHHQ (CFUX(.90),CFUY(.11),'EDGE OF PLOTTER FRAME  ',
     +                                                  .008,0.,1.)
C
        CALL PLCHHQ (CFUX(.86),CFUY(.15),'EDGE OF VIEWPORT  ',
     +                                             .008,0.,1.)
C
        CALL PLCHHQ (CFUX(.24),CFUY(.83),'UPPER EDGE OF',
     +                                        .008,0.,0.)
        CALL PLCHHQ (CFUX(.24),CFUY(.81),'DATA GRID MAPS',
     +                                         .008,0.,0.)
        CALL PLCHHQ (CFUX(.24),CFUY(.79),'TO OUTER CIRCLE',
     +                                          .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.50),CFUY(.52),'LOWER EDGE OF',
     +                                        .008,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.50),'DATA GRID MAPS',
     +                                         .008,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.48),'TO INNER CIRCLE',
     +                                          .008,0.,0.)
C
        CALL PLCHHQ (CFUX(.76),CFUY(.83),'LEFT AND RIGHT EDGES',
     +                                               .008,0.,0.)
        CALL PLCHHQ (CFUX(.76),CFUY(.81),'OF DATA GRID MAP TO',
     +                                              .008,0.,0.)
        CALL PLCHHQ (CFUX(.76),CFUY(.79),'HORIZONTAL LINE',
     +                                          .008,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END



      SUBROUTINE CPMPXY (IMAP,XINP,YINP,XOTP,YOTP)
C
C When 'MAP' = 1, this version of CPMPXY just does the identity mapping
C and its inverse except in the upper right corner, in which it returns
C the out-of-range value (for illustrative purposes).
C
C Using 'MAP' = 2 is the same except that the routine says that it can't
C do the inverse mapping (IMAP = 0 and XINP = 1 gives YINP = 1 instead
C of 3).  This is used to show the adverse effects on the generation of
C the edge of the area invisible under the mapping.
C
C Using 'MAP' = 3 gives the polar coordinate transformation and its
C inverse.  No out-of-range values can be returned.
C
        IF (IMAP.EQ.0) THEN
          IF (XINP.EQ.1.) THEN
            YINP=3.
          ELSE IF (XINP.EQ.2.) THEN
            YINP=1.
          ELSE IF (XINP.EQ.3.) THEN
            YINP=3.
          ELSE
            YINP=3.
          END IF
        ELSE IF (ABS(IMAP).EQ.1.OR.ABS(IMAP).EQ.2) THEN
          IF ((XINP-1.)**2+(YINP-1.)**2.GT..0625) THEN
            XOTP=XINP
            YOTP=YINP
          ELSE
            XOTP=1.E12
            YOTP=1.E12
          END IF
        ELSE IF (ABS(IMAP).EQ.3) THEN
          IF (IMAP.GT.0) THEN
            XOTP=XINP*COS(.017453292519943*YINP)
            YOTP=XINP*SIN(.017453292519943*YINP)
          ELSE
            XOTP=SQRT(XINP*XINP+YINP*YINP)
            YOTP=57.2957795130823*ATAN2(YINP,XINP)
          END IF
        ELSE
          XOTP=XINP
          YOTP=YINP
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE ARDBPX (IAMA,IGIP)
C
        DIMENSION IAMA(*)
C
C The routine ARDBPX produces a picture of that part of the contents of
C the area map IAMA that belongs to the group IGIP; if IGIP is zero or
C negative, all groups of edges are shown.  This is a modified version
C of the AREAS routine ARDBPA.  No label is written at the top.  All
C color-setting and error-recovery code has been removed.  The code
C computing RXCN and RYCN has been changed to force the picture into
C a smaller square than the whole plotter frame (so that labels near
C the edge are readable and there is room for additional labels at top
C and bottom).
C
C The common block ARCOM1 is used to communicate with the arrow-drawing
C routine ARDBDA.
C
        COMMON /ARCOM1/ DT
C
C Bump the line width by a factor of two.
C
        CALL GSLWSC (2.)
C
C Extract the length of the area map.
C
        LAMA=IAMA(1)
C
C Save the current state of the SET call and switch to the fractional
C coordinate system.
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        CALL    SET (  0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.,   1)
C
C Trace the edges in the area map, drawing arrows as we go.
C
        DT=0.
        INDX=8
        RXCN=.5
        RYCN=.5
C
  101   RXCO=RXCN
        RYCO=RYCN
C
        RXCN=.1+.8*REAL(IAMA(INDX+1))/1000000.
        RYCN=.1+.8*REAL(IAMA(INDX+2))/1000000.
C
        IF (IAMA(INDX+7).NE.0) THEN
          IGID=ABS(IAMA(INDX+7))
          IF (IGID.LT.IAMA(6)) THEN
            IGID=IAMA(IAMA(1)-IGID)/2
          ELSE
            IGID=IAMA(IGID)/2
          END IF
          IF (IGIP.LE.0.OR.IGID.EQ.IGIP) THEN
            IAIL=IAMA(INDX+8)
            IF (IAIL.GT.0) IAIL=IAMA(IAIL)/2
            IAIR=IAMA(INDX+9)
            IF (IAIR.GT.0) IAIR=IAMA(IAIR)/2
            CALL ARDBDA (RXCO,RYCO,RXCN,RYCN,IAIL,IAIR,IGIP,IGID)
          END IF
        ELSE
          DT=0.
        END IF
C
        IF (IAMA(INDX+3).NE.0) THEN
          INDX=IAMA(INDX+3)
          GO TO 101
        END IF
C
C Restore the original SET call.
C
        CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
C Set the line width back to normal.
C
        CALL GSLWSC (1.)
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE CIRCLE (XCEN,YCEN,RADC)
C
C This routine draws a circle with center (XCEN,YCEN) and radius RADC.
C All input variables are stated in the fractional system.
C
        CALL DPDRAW (XCEN+RADC,YCEN,0)
C
        DO 101 I=1,90
          ANGR=.017453292519943*REAL(4*I)
          CALL DPDRAW (XCEN+RADC*COS(ANGR),YCEN+RADC*SIN(ANGR),1)
  101   CONTINUE
C
        CALL DPDRAW (0.,0.,2)
C
C Done.
C
        RETURN
C
      END
