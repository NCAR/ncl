
        PROGRAM CBEX01
C
C LATEST REVISION        September, 1989
C
C PURPOSE                To provide a simple demonstration of the
C                        use of BIVAR and CONPACK together as a
C                        temporary replacement for CONRAN.
C
C PRECISION              Single.
C
C REQUIRED LIBRARY       BIVAR, CONPACK.
C FILES
C
C LANGUAGE               FORTRAN.
C
C HISTORY                Written September, 1989, by Dave Kennison.
C
C ALGORITHM              At each of nine points scattered "at random"
C                        in a portion of the x/y plane, a mathematical
C                        function is evaluated to obtain a value of z.
C                        The resulting triplets approximately describe
C                        the surface which is defined exactly by the
C                        function.  The routine BIVAR is then called to
C                        obtain an array of interpolated values of z on
C                        a regular grid, approximating the same surface,
C                        and this data is used as input to CONPACK to
C                        draw two different contour plots.
C
C                        On the first plot, contours are shown in a
C                        rectangular area containing the x/y positions
C                        of the original nine data points and those
C                        positions are marked on the plot.
C
C                        On the second plot, capabilities of CONPACK
C                        and AREAS are used to limit the drawing of
C                        contours to the convex hull of the original
C                        nine x/y positions.
C
C PORTABILITY            ANSI standard.
C
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
C Define arrays to be used below.  XRAN, YRAN, and ZRAN are used for
C the "random data".  XCNV and YCNV are used to define the convex hull
C of the x/y positions of this data.  XDAT, YDAT, and ZDAT are used for
C the regular grid of data returned by BIVAR.  IWRK and RWRK are used
C as integer and real workspace arrays, both in calls to BIVAR and in
C calls to CONPACK.
C
        DIMENSION XRAN(9),YRAN(9),ZRAN(9),XCNV(7),YCNV(7)
        DIMENSION XDAT(11),YDAT(12),ZDAT(11,12)
        DIMENSION IWRK(1000),RWRK(1000)
C
C Define, in a labelled common block, quantities which must be available
C to the routines CPCHHL and CPCHLL.  The flag ICLL is used to turn
C on and off the culling of labels which these routines do (it is off
C while drawing the first plot and on while drawing the second).  The
C array IAMA is the area map array.
C
        COMMON /AMAPRA/ ICLL,IAMA(10000)
C
C Define a temporary character variable for use below.
C
        CHARACTER*1 ICHR
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL CPDRPL
C
C Specify the X and Y input data values.
C
        DATA XRAN / 0.4, 2.0, 2.8, 5.0, 4.8, 8.8, 5.1, 6.2, 9.1 /
        DATA YRAN / 5.4, 1.1, 3.7, 6.8, 9.5, 9.8, 0.7, 2.7, 3.0 /
C
C Specify the Z input data values.
C
        DO 101 I=1,9
C         ZRAN(I)=0.2+0.4*XRAN(I)*XRAN(I)+.6*YRAN(I)*YRAN(I)
          ZRAN(I)=EXP(-(XRAN(I)-3.)**2/9.-(YRAN(I)-5.)**2/25.)-
     +            EXP(-(XRAN(I)-6.)**2/9.-(YRAN(I)-5.)**2/25.)
  101   CONTINUE
C
C Specify the points defining the convex hull.
C
        XCNV(1)=XRAN(1)
        YCNV(1)=YRAN(1)
        XCNV(2)=XRAN(2)
        YCNV(2)=YRAN(2)
        XCNV(3)=XRAN(7)
        YCNV(3)=YRAN(7)
        XCNV(4)=XRAN(9)
        YCNV(4)=YRAN(9)
        XCNV(5)=XRAN(6)
        YCNV(5)=YRAN(6)
        XCNV(6)=XRAN(5)
        YCNV(6)=YRAN(5)
        XCNV(7)=XRAN(1)
        YCNV(7)=YRAN(1)
C
C Specify the X and Y coordinates of the points on the regular grid.
C
        DO 102 I=1,11
          XDAT(I)=REAL(I-1)
  102   CONTINUE
C
        DO 103 J=1,12
          YDAT(J)=REAL(J-1)
  103   CONTINUE
C
C Call IDSFFT to obtain a regular grid of values on the fitted surface.
C
        CALL IDSFFT (1,9,XRAN,YRAN,ZRAN,11,12,11,XDAT,YDAT,ZDAT,IWRK,
     +                                                          RWRK)
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off clipping.
C
        CALL GSCLIP (0)
C
C Define a set of colors to use.
C
        CALL DFCLRS(IWKID)
C
C Tell CONPACK to position labels using the "regular" scheme.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',2)
C
C Tell CONPACK to put the informational label in a different place.
C
        CALL CPSETI ('ILP - INFORMATIONAL LABEL POSITIONING',-2)
        CALL CPSETR ('ILX - INFORMATIONAL LABEL X COORDINATE',.98)
        CALL CPSETR ('ILY - INFORMATIONAL LABEL Y COORDINATE',.02)
C
C Provide a little more room below the viewport; otherwise, the labels
C on AUTOGRAPH's X axis get squashed.
C
        CALL CPSETR ('VPB - VIEWPORT BOTTOM EDGE',.08)
C
C Tell CONPACK in what ranges to generate X and Y coordinates.
C
        CALL CPSETR ('XC1 - X COORDINATE AT I=1',0.)
        CALL CPSETR ('XCM - X COORDINATE AT I=M',10.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J=1',0.)
        CALL CPSETR ('YCN - Y COORDINATE AT J=N',11.)
C
C Turn off the culling of labels by CPCHHL and CPCHLL.
C
        ICLL=0
C
C Dump the polyline buffer and change the polyline color to white.
C Change the text color to orange.  CONPACK will use these colors.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (1)
        CALL GSTXCI (12)
C
C Initialize the drawing of the first contour plot.
C
        CALL CPRECT (ZDAT,11,11,12,RWRK,1000,IWRK,1000)
C
C Initialize the area map which will be used to keep contour lines from
C passing through labels.
C
        CALL ARINAM (IAMA,10000)
C
C Put label boxes in the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Draw the contour lines, masked by the area map.
C
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,CPDRPL)
C
C Draw all the labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Dump the polyline buffer and change the polyline color to orange.
C Change the text color to orange, too, so that the AUTOGRAPH background
C will come out entirely in that color.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (12)
        CALL GSTXCI (12)
C
C Use AUTOGRAPH to produce a background for the contour plot, forcing
C it to pick up appropriate values from CONPACK's SET call.
C
        CALL AGSETI ('SET.',4)
        CALL AGSTUP (DUMI,1,1,1,1,DUMI,1,1,1,1)
        CALL AGBACK
C
C Dump the polyline buffer and change the polyline color to green.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (9)
C
C Change the aspect ratio of the characters drawn by PLCHMQ to make
C them approximately square.
C
        CALL PCSETR ('HW',1.)
C
C At each of the "random" data positions, put the index of the point
C and a starburst to set it off.
C
        DO 104 I=1,9
          ICHR=CHAR(ICHAR('0')+I)
          CALL PLCHMQ (XRAN(I),YRAN(I),ICHR,.0175,0.,0.)
          CALL LINE (CFUX(CUFX(XRAN(I))-.02),CFUY(CUFY(YRAN(I))-.02),
     +               CFUX(CUFX(XRAN(I))-.01),CFUY(CUFY(YRAN(I))-.01))
          CALL LINE (CFUX(CUFX(XRAN(I))+.01),CFUY(CUFY(YRAN(I))+.01),
     +               CFUX(CUFX(XRAN(I))+.02),CFUY(CUFY(YRAN(I))+.02))
          CALL LINE (CFUX(CUFX(XRAN(I))-.02),CFUY(CUFY(YRAN(I))+.02),
     +               CFUX(CUFX(XRAN(I))-.01),CFUY(CUFY(YRAN(I))+.01))
          CALL LINE (CFUX(CUFX(XRAN(I))+.01),CFUY(CUFY(YRAN(I))-.01),
     +               CFUX(CUFX(XRAN(I))+.02),CFUY(CUFY(YRAN(I))-.02))
          CALL LINE (CFUX(CUFX(XRAN(I))-.02828),CFUY(CUFY(YRAN(I))),
     +               CFUX(CUFX(XRAN(I))-.01414),CFUY(CUFY(YRAN(I))))
          CALL LINE (CFUX(CUFX(XRAN(I))+.01414),CFUY(CUFY(YRAN(I))),
     +               CFUX(CUFX(XRAN(I))+.02828),CFUY(CUFY(YRAN(I))))
          CALL LINE (CFUX(CUFX(XRAN(I))),CFUY(CUFY(YRAN(I))-.02828),
     +               CFUX(CUFX(XRAN(I))),CFUY(CUFY(YRAN(I))-.01414))
          CALL LINE (CFUX(CUFX(XRAN(I))),CFUY(CUFY(YRAN(I))+.01414),
     +               CFUX(CUFX(XRAN(I))),CFUY(CUFY(YRAN(I))+.02828))
  104   CONTINUE
C
C Dump the polyline buffer and switch the polyline color to yellow.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (11)
C
C Put a label at the top of the plot.
C
        CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL PLCHHQ (.5,.975,
     +               'DEMONSTRATING THE USE OF BIVAR AND CONPACK',
     +               .012,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Do another frame.  First, turn on the culling of labels by the
C routines CPCHHL and CPCHLL (which see, below).
C
        ICLL=1
C
C Dump the polyline buffer and switch the polyline color back to white.
C Force the text color index to orange.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (1)
        CALL GSTXCI (12)
C
C Initialize the drawing of the second contour plot.
C
        CALL CPRECT (ZDAT,11,11,12,RWRK,1000,IWRK,1000)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,10000)
C
C Put the convex hull of the "random" data in the area map, using group
C identifier 4, an area identifier of 0 inside the hull, and an area
C identifier of -1 outside the hull.
C
        CALL AREDAM (IAMA,XCNV,YCNV,7,4,0,-1)
C
C Put label boxes in the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Draw contour lines, masked by the area map.

        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,CPDRPL)
C
C Draw labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Dump the polyline buffer and switch the polyline color to orange.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (12)
C
C Use AUTOGRAPH to draw a background.
C
        CALL AGSETI ('SET.',4)
        CALL AGSTUP (DUMI,1,1,1,1,DUMI,1,1,1,1)
        CALL AGBACK
C
C Dump the polyline buffer and switch the polyline color to yellow.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (11)
C
C Draw a label at the top of the plot.
C
        CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL PLCHHQ (.5,.975,
     +               'DEMONSTRATING THE USE OF BIVAR AND CONPACK',
     +               .012,0.,0.)
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
      SUBROUTINE DFCLRS(IWKID)
C
C Define a set of RGB color triples for colors 1 through 15 on
C workstation 1.
C
        DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
        DATA RGBV / 1.00 , 1.00 , 1.00 ,
     +              0.70 , 0.70 , 0.70 ,
     +              0.75 , 0.50 , 1.00 ,
     +              0.50 , 0.00 , 1.00 ,
     +              0.00 , 0.00 , 1.00 ,
     +              0.00 , 0.50 , 1.00 ,
     +              0.00 , 1.00 , 1.00 ,
     +              0.00 , 1.00 , 0.60 ,
     +              0.00 , 1.00 , 0.00 ,
     +              0.70 , 1.00 , 0.00 ,
     +              1.00 , 1.00 , 0.00 ,
     +              1.00 , 0.75 , 0.00 ,
     +              1.00 , 0.38 , 0.38 ,
     +              1.00 , 0.00 , 0.38 ,
     +              1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
        CALL GSCR (IWKID,0,0.,0.,0.)
C
        DO 101 I=1,15
          CALL GSCR (IWKID,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
      SUBROUTINE CPCHHL (IFLG)
C
C This version of CPCHHL, if and only if ICLL is non-zero, examines a
C high/low label which is about to be drawn.  If that label would fall
C in an area outside the convex hull defined by edge group 4, the text
C of the label is changed to a blank, so that the label is effectively
C deleted.
C
        COMMON /AMAPRA/ ICLL,IAMA(10000)
        DIMENSION IAAI(10),IAGI(10)
        IF (ICLL.EQ.0) RETURN
        IF ((IFLG.GE.2.AND.IFLG.LE.4).OR.
     +      (IFLG.GE.6.AND.IFLG.LE.8)) THEN
          CALL CPGETR ('LBX',XPOS)
          CALL CPGETR ('LBY',YPOS)
          CALL ARGTAI (IAMA,XPOS,YPOS,IAAI,IAGI,10,NAIR,1)
          IVIS=1
          DO 101 I=1,NAIR
            IF (IAGI(I).EQ.4.AND.IAAI(I).LT.0) IVIS=0
  101     CONTINUE
          IF (IVIS.EQ.0) THEN
            CALL CPSETC ('CTM',' ')
          END IF
        END IF
        RETURN
      END
      SUBROUTINE CPCHLL (IFLG)
C
C This version of CPCHLL, if and only if ICLL is non-zero, examines a
C contour-line label which is about to be drawn.  If that label would
C fall in an area outside the convex hull defined by edge group 4, the
C text of the label is changed to a blank, so that it is effectively
C deleted.
C
        COMMON /AMAPRA/ ICLL,IAMA(10000)
        DIMENSION IAAI(10),IAGI(10)
        IF (ICLL.EQ.0) RETURN
        IF (IFLG.GE.2.AND.IFLG.LE.4) THEN
          CALL CPGETR ('LBX',XPOS)
          CALL CPGETR ('LBY',YPOS)
          CALL ARGTAI (IAMA,XPOS,YPOS,IAAI,IAGI,10,NAIR,1)
          IVIS=1
          DO 101 I=1,NAIR
            IF (IAGI(I).EQ.4.AND.IAAI(I).LT.0) IVIS=0
  101     CONTINUE
          IF (IVIS.EQ.0) THEN
            CALL CPSETC ('CTM',' ')
          END IF
        END IF
        RETURN
      END
