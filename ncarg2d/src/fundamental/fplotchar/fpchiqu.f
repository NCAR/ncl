
      PROGRAM FPCHIQU
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
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set the "fill area interior style" to "solid".
C
      CALL GSFAIS (1)
C
C Do a call to SET which allows us to use fractional coordinates.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Define some colors to use.
C
      CALL GSCR (IWKID,0,1.,1.,1.)
      CALL GSCR (IWKID,1,0.,.0,.0)
      CALL GSCR (IWKID,2,0.,.5,.5)
      CALL GSCR (IWKID,3,.9,.9,0.)
      CALL GSCR (IWKID,4,1.,.3,.3)
      CALL GSCR (IWKID,5,0.,0.,1.)
      CALL GSCR (IWKID,6,.2,.2,.2)
      CALL GSCR (IWKID,7,.8,.8,.8)
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
C Show various other features like lines with several fonts.
C
      CALL PLCHHQ (.715,.440,'OTHER FEATURES',.012,0.,0.)
C

C Show the use of fontcap databases and some of
C the new features added in June of 1990.
      CALL PLOTIF (0.,0.,2)
C
C Temporarily use the slash as a function code character.
C
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER','/')
C
C Combine characters from several different fonts to produce a single
C line.
C
      CALL PLCHHQ (.990,.410,'/F13/A line with characters from several f
     +onts:  /F8/P/BF13/0/N/=/F5/g/SF13/2/N/+/F5/j/SF13/2/N/',
     +                                                       .012,0.,1.)
C
C Reset the internal parameter 'FN' to 4 and write a line illustrating
C the effect of function codes "Fn", "F", and "F0".  Then reset 'FN'
C to 0.
C
      CALL PCSETI ('FN - FONT NUMBER',4)
      CALL PLCHHQ (.990,.380,'Set ''FN'' (Font Number) to 4 and then use
     +"F" function codes:',.012,0.,1.)
      CALL PLCHHQ (.990,.350,'Before F10 - /F10/after F10 - /F/after F
     +- /F0/after F0.',.012,0.,1.)
      CALL PCSETI ('FN - FONT NUMBER',0)
C
C Write lines illustrating various kinds of zooming.
C
      CALL PLCHHQ (.990,.320,'/F13/Unzoomed characters from font 13.',
     +.012,0.,1.)
      CALL PLCHHQ (.990,.290,'/F13X130Q/Characters zoomed in width, usin
     +g X130Q.',.012,0.,1.)
      CALL PLCHHQ (.990,.260,'/F13Y130Q/Characters zoomed in height, usi
     +ng Y130Q.',.012,0.,1.)
      CALL PLCHHQ (.99000,.230,'/F13Z130Q/Characters zoomed both ways, us
     +ing Z130Q.',.012,0.,1.)
C
C Write a line illustrating non-aligned zooming in height.
C
      CALL PLCHHQ (.990,.200,'/F13/Unaligned zoom of characters: /F16Y20
     +0/S/Y/cientific /Y200/V/Y/isualization /Y200/G/Y/roup', .012,0.,
     +1.)
C
C Write lines illustrating the use of 'AS' and 'SS'.
C
      CALL PCSETR ('AS - ADD SPACE BETWEEN CHARACTERS     ',.125)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',  0.)
      CALL PLCHHQ (.990,.170,'/F14/Line with ''AS'' = .125 and ''SS = 0.
     +',.012,0.,1.)
      CALL PCSETR ('AS - ADD SPACE BETWEEN CHARACTERS     ',  0.)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',  0.)
      CALL PLCHHQ (.990,.140,'/F14/Line with ''AS'' = 0. and ''SS'' = 0.
     +',.012,0.,1.)
      CALL PCSETR ('AS - ADD SPACE BETWEEN CHARACTERS     ',  0.)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',.125)
      CALL PLCHHQ (.990,.110, '/F14/Line with ''AS'' = 0. and ''SS'' = .
     +125',.012,0.,1.)
      CALL PCSETR ('AS - ADD SPACE BETWEEN CHARACTERS     ',  0.)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',  0.)
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
C Draw a bounding box
C
      CALL LINE(0.,0.,1.,0.)
      CALL LINE(1.,0.,1.,1.)
      CALL LINE(1.,1.,0.,1.)
      CALL LINE(0.,1.,0.,0.)


C
C Advance the frame.
C
      CALL FRAME
C
C Do a single frame showing some of the new features added in December
C of 1992.
C
C Put a label at the top of the plot and, below that, an explanatory
C note.
C
      CALL PLCHHQ (.5,.975,':F25:PLCHHQ - FEATURES ADDED 12/92',
     +                                                       .025,0.,0.)
C
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',.275)
      CALL PLCHHQ (.5,.938,':F13:(Use idt''s ''zoom'' to view some of th
     +is in detail, especially stacking.)',
     +                                                       .017,0.,0.)
      CALL PCSETR ('SS - SUBTRACT SPACE BETWEEN CHARACTERS',0.)
C
C Illustrate the use of filled fonts with shadows and outlines.
C
C Write a line.
C
      CALL PLCHHQ (.5,.900,':F26:By default, the current foreground colo
     +r is used.',.024,0.,0.)
C
C Define the principal color to be used for characters.
C
      CALL PCSETI ('CC - CHARACTER COLOR',4)
C
C Write another line.
C
      CALL PLCHHQ (.5,.850,':F26:A non-negative ''CC'' requests a differ
     +ent color.', .026,0.,0.)
C
C Turn on character shadows and define various characteristics of the
C shadow.
C
      CALL PCSETI ('SF - SHADOW FLAG',1)
      CALL PCSETR ('SX - SHADOW OFFSET IN X',-.15)
      CALL PCSETR ('SY - SHADOW OFFSET IN Y',-.15)
      CALL PCSETI ('SC - SHADOW COLOR',1)
C
C Write another line.
C
      CALL PLCHHQ (.5,.796,':F26:''SF'', ''SC'', ''SX'', and ''SY'' crea
     +te shadows.',
     +                                                       .028,0.,0.)
C
C Turn on character outlines and define the color of the outline.
C
      CALL PCSETI ('SF - SHADOW FLAG',0)
      CALL PCSETI ('OF - OUTLINE FLAG',1)
      CALL PCSETI ('OC - OUTLINE COLOR',1)
      CALL PCSETI ('OL - OUTLINE LINE WIDTH',1)
C
C Write another line.
C
      CALL PLCHHQ (.5,.738,':F26:''OF'', ''OC'', and ''OL'' add outlines
     +.',
     +                                                       .030,0.,0.)
C
C Turn on the drawing of boxes and define characteristics of them.
C
      CALL PCSETI ('BF - BOX FLAG',7)
      CALL PCSETI ('BL - BOX LINE WIDTH',2)
      CALL PCSETR ('BM - BOX MARGIN',.15)
      CALL PCSETR ('BX - BOX SHADOW X OFFSET',-.1)
      CALL PCSETR ('BY - BOX SHADOW Y OFFSET',-.1)
      CALL PCSETI ('BC(1) - BOX COLOR - BOX OUTLINE    ',5)
      CALL PCSETI ('BC(2) - BOX COLOR - BOX FILL       ',7)
      CALL PCSETI ('BC(3) - BOX COLOR - BOX SHADOW FILL',1)
C
C Write another line.
C
      CALL PLCHHQ (.5,.672,':F26:''BF'', ''BC'', ''BL'', ''BM'', ''BX'',
     + and ''BY'' add a box.',
     +                                                       .026,0.,0.)
C
C Get rid of the box shadow, which doesn't add much.
C
      CALL PCSETI ('BF - BOX FLAG',3)
C
C Write another line.
C
      CALL PCSETC ('FC - FUNCTION-CODE CHARACTER','/')
      CALL PLCHHQ (.5,.592,'/F26/''MA'' and ''OR'' are used for mapping:
     +',
     +                                                       .030,0.,0.)
      CALL PCSETC ('FC - FUNCTION CODE CHARACTER',':')
C
C Write a couple of headers for the plots that follow.
C
      CALL PLCHHQ (.28,.528,':F25:(EZMAP)',.024,0.,0.)
      CALL PLCHHQ (.72,.528,':F33:(r:F25: and :F33:q)',.024,0.,0.)
C
C Initialize EZMAP and draw a background.
C
      CALL MAPSTC ('OU','CO')
      CALL MAPSTI ('GR',5)
      CALL MAPPOS (.065,.495,.065,.495)
      CALL MAPSTR ('SA',8.5)
      CALL MAPROJ ('SV',0.,-25.,0.)
      CALL MAPINT
      CALL MAPLOT
      CALL MAPGRD
C
C Tell PLOTCHAR to map characters through EZMAP.
C
      CALL PCSETI ('MA - MAPPING FLAG',1)
      CALL PCSETR ('OR - OUT-OF-RANGE FLAG',1.E12)
C
C Write a line across the surface of the globe.
C
      CALL PLCHHQ (-25.,0.,':F25Y200:NCAR GRAPHICS',8.,30.,0.)
C
C Do an appropriate SET call for a rho-theta mapping.
C
      CALL SET    (.505,.935,.065,.495,-27.5,27.5,-27.5,27.5,1)
C
C Tell PLOTCHAR to use a rho-theta mapping.
C
      CALL PCSETI ('MA - MAPPING FLAG',2)
      CALL PCSETR ('OR - OUT-OF-RANGE FLAG',0.)
C
C Write three lines in rho/theta space, orienting them so they come out
C in a circle after mapping.
C
      CALL PLCHHQ (20., 90.,':F25Y125:NCAR GRAPHICS',8.,-90.,0.)
      CALL PLCHHQ (20.,210.,':F25Y125:NCAR GRAPHICS',8.,-90.,0.)
      CALL PLCHHQ (20.,-30.,':F25Y125:NCAR GRAPHICS',8.,-90.,0.)
C
C Turn off mapping and recall SET to allow fractional coordinates again.
C
      CALL PCSETI ('MA - MAPPING FLAG',0)
C
      CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Change the drawing order to allow for "stacking" characters from
C right to left.
C
      CALL PCSETI ('DO - DRAWING ORDER',-2)
C
C Reduce the space between characters so the "stacking" is visible.
C
      CALL PCSETR ('SS - SUBTRACT-SPACE FLAG',.3)
C
C Turn off the box.  Make the shadows black and position them so they
C help make the stacked characters readable.
C
      CALL PCSETI ('BF - BOX FLAG',0)
      CALL PCSETI ('SC - SHADOW COLOR',0)
      CALL PCSETR ('SX - SHADOW OFFSET IN X',.1)
      CALL PCSETR ('SY - SHADOW OFFSET IN Y',0.)
C
C Write a final line demonstrating "stacking".
C
      CALL PLCHHQ (.5,.030,':F26:Use    ''DO''    and    ''SS''    to   +
     + "stack"    characters    in    either    direction.',
     +                                                       .026,0.,0.)
C
C Draw a bounding box
C
      CALL LINE(0.,0.,1.,0.)
      CALL LINE(1.,0.,1.,1.)
      CALL LINE(1.,1.,0.,1.)
      CALL LINE(0.,1.,0.,0.)
C
C Advance the frame.
C
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
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
