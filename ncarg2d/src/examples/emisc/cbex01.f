C
C	$Id: cbex01.f,v 1.1.1.1 1992-04-17 22:33:20 ncargd Exp $
C
      PROGRAM CPBIVR
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
        CALL OPNGKS
C
C Turn off clipping.
C
        CALL GSCLIP (0)
C
C Define a set of colors to use.
C
        CALL DFCLRS
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
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END
      SUBROUTINE DFCLRS
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
        CALL GSCR (1,0,0.,0.,0.)
C
        DO 101 I=1,15
          CALL GSCR (1,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
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
C
C PACKAGE BIVAR          NOTE:  DOCUMENTATION FOR INDIVIDUAL ROUTINES
C                        FOLLOWS THE GENERAL PACKAGE INFORMATION.
C
C LATEST REVISION        JANUARY, 1985
C
C PURPOSE                TO PROVIDE BIVARIATE INTERPOLATION AND SMOOTH
C                        SURFACE FITTING FOR VALUES GIVEN AT IRREGULARLY
C                        DISTRIBUTED POINTS.
C
C                        THE RESULTING INTERPOLATING FUNCTION AND
C                        ITS FIRST-ORDER PARTIAL DERIVATIVES ARE
C                        CONTINUOUS.
C
C                        THE METHOD EMPLOYED IS LOCAL, I.E. A CHANGE
C                        IN THE DATA IN ONE AREA OF THE PLANE DOES NOT
C                        AFFECT THE INTERPOLATING FUNCTION EXCEPT IN
C                        THAT LOCAL AREA.  THIS IS ADVANTAGEOUS OVER
C                        GLOBAL INTERPOLATION METHODS.
C
C                        ALSO, THE METHOD GIVES EXACT RESULTS WHEN ALL
C                        POINTS LIE IN A PLANE. THIS IS ADVANTAGEOUS
C                        OVER OTHER METHODS SUCH AS TWO-DIMENSIONAL
C                        FOURIER SERIES INTERPOLATION.
C
C USAGE                  THIS PACKAGE CONTAINS TWO USER ENTRIES,
C                        IDBVIP AND IDSFFT, BOTH REQUIRING INPUT
C                        DATA TO BE GIVEN AT POINTS
C                        ( X(I) , Y(I) ), I=1,...,N.
C
C                        IF THE USER DESIRES THE INTERPOLATED DATA
C                        TO BE OUTPUT AT GRID POINTS, I.E. AT POINTS
C                        ( XI(I) , YI(J) ), I=1,...,NX, J=1,...,NYI,
C                        ROUTINE IDSFFT SHOULD BE USED.  THIS IS
C                        USEFUL FOR GENERATING AN INTERPOLATING
C                        SURFACE.
C
C                        THE OTHER USER ENTRY POINT, IDBVIP, WILL
C                        PRODUCE INTERPOLATED VALUES AT POINTS
C                        ( XI(I) , YI(I) ), I=1,...,NIP.  THIS IS
C                        USEFUL FOR FILLING IN MISSING DATA POINTS
C                        ON A GRID.
C
C I/O                    NONE, EXCEPT ERROR MESSAGES PRINTED VIA
C                        ROUTINE ULIBER.
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       ULIBER ON ULIB, WHICH IS AUTOMATICALLY
C FILES                  LOADED ON NCAR'S CRAY MACHINES.
C
C LANGUAGE               FORTRAN
C
C HISTORY                THE ORIGINAL VERSION OF BIVAR WAS WRITTEN BY
C                        HIROSHI AKIMA IN AUGUST 1975 AND REWRITTEN BY
C                        HIM IN LATE 1976.  IT WAS INCORPORATED INTO
C                        NCAR'S PUBLIC SOFTWARE LIBRARIES IN JANUARY
C                        1977.  IN AUGUST 1984 A NEW VERSION OF BIVAR,
C                        INCORPORATING CHANGES DESCRIBED IN THE ROCKY
C                        MOUNTAIN JOURNAL OF MATHEMATICS ARTICLE CITED
C                        BELOW, WAS OBTAINED FROM DR. AKIMA BY MICHAEL
C                        PERNICE OF NCAR'S SCIENTIFIC COMPUTING
C                        DIVISION, WHO EVALUATED IT AND MADE IT
C                        AVAILABLE IN FEBRUARY, 1985.
C
C PORTABILITY            FORTRAN 66
C
C ACCURACY               ACCURATE TO MACHINE PRECISION ON THE INPUT
C                        DATA POINTS.  ACCURACY AT OTHER POINTS
C                        GREATLY DEPENDS ON THE INPUT DATA.
C
C REFERENCES             THE ORIGINAL METHOD IS DESCRIBED IN
C
C                        AKIMA, HIROSHI, 1978:  A METHOD OF BIVARIATE
C                             INTERPOLATION AND SMOOTH SURFACE FITTING
C                             FOR VALUES GIVEN AT IRREGULARLY
C                             DISTRIBUTED POINTS,
C                             ACM-TOMS, VOL. 4, NO. 2, JUNE 1978.
C
C                        AN IMPROVEMENT TO THE ORIGINAL METHOD
C                        WAS PRESENTED IN
C
C                        AKIMA, HIROSHI, 1984:  ON ESTIMATING PARTIAL
C                             DERIVATIVES FOR BIVARIATE INTERPOLATION
C                             OF SCATTERED DATA,
C                             ROCKY MOUNTAIN JOURNAL OF MATHEMATICS,
C                             VOL. 14, NO. 1, WINTER 1984.
C
C METHOD                 THE X-Y PLANE IS DIVIDED INTO TRIANGULAR
C                        CELLS, EACH CELL HAVING PROJECTIONS OF THREE
C                        DATA POINTS IN THE PLANE AS ITS VERTICES, AND
C                        A BIVARIATE QUINTIC POLYNOMIAL IN X AND Y IS
C                        FITTED TO EACH TRIANGULAR CELL.
C
C                        THE COEFFICIENTS IN THE FITTED QUINTIC
C                        POLYNOMIALS ARE DETERMINED BY CONTINUITY
C                        REQUIREMENTS AND BY ESTIMATES OF PARTIAL
C                        DERIVATIVES AT THE VERTICES AND ALONG THE
C                        EDGES OF THE TRIANGLES.  THE METHOD DESCRIBED
C                        IN THE ROCKY MOUNTAIN JOURNAL REFERENCE
C                        GUARANTEES THAT THE GENERATED SURFACE
C                        DEPENDS CONTINUOUSLY ON THE TRIANGULATION.
C
C                        THE RESULTING INTERPOLATING FUNCTION IS
C                        INVARIANT UNDER THE FOLLOWING TYPES OF LINEAR
C                        COORDINATE TRANSFORMATIONS:
C                             1) A ROTATION OF X-Y COORDINATE SYSTEM
C                             2) LINEAR SCALE TRANSFORMATION OF Z-AXIS
C                             3) TILTING OF THE X-Y PLANE, I.E. NEW
C                                COORDINATES (U,V,W) GIVEN BY
C                                       U = X
C                                       V = Y
C                                       W = Z + A*X + B*Y
C                                WHERE A, B ARE ARBITRARY CONSTANTS.
C
C                         COMPLETE DETAILS OF THE METHOD ARE GIVEN
C                         IN THE REFERENCE PUBLICATIONS.
C
C ********************************************************************
C
C INDIVIDUAL USER ENTRY POINT DOCUMENTATION FOLLOWS.
C
C ********************************************************************
C
C     SUBROUTINE IDBVIP (MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,IWK,WK)
C
C DIMENSION OF            XD(NDP), YD(NDP), ZD(NDP), XI(NIP), YI(NIP)
C ARGUMENTS               ZI(NIP), IWK(31*NDP+NIP),  WK(8*NDP)
C
C PURPOSE                 TO PERFORM BIVARIATE INTERPOLATION WHEN THE
C                         PROJECTIONS OF THE DATA POINTS IN THE X-Y
C                         PLANE ARE IRREGULARLY DISTRIBUTED.
C
C USAGE                   CALL IDBVIP (MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,
C                                      IWK,WK)
C
C ARGUMENTS
C
C ON INPUT                MD
C                           MODE OF COMPUTATION (MUST BE 1, 2, OR 3,
C                           ELSE AN ERROR RETURN OCCURS.)
C                           = 1 IF THIS IS THE FIRST CALL TO THIS
C                               SUBROUTINE, OR IF THE VALUE OF NDP
C                               HAS BEEN CHANGED FROM THE PREVIOUS
C                               CALL, OR IF THE CONTENTS OF THE XD
C                               OR YD ARRAYS HAVE BEEN CHANGED FROM
C                               THE PREVIOUS CALL.
C                           = 2 IF THE VALUES OF NDP AND THE XD AND
C                               YD ARRAYS ARE UNCHANGED FROM THE
C                               PREVIOUS CALL, BUT NEW VALUES FOR
C                               XI, YI ARE BEING USED.  IF MD = 2
C                               AND NDP HAS BEEN CHANGED SINCE THE
C                               PREVIOUS CALL TO IDBVIP, AN ERROR
C                               RETURN OCCURS.
C                           = 3 IF THE VALUES OF  NDP, NIP, XD,
C                               YD, XI, YI ARE UNCHANGED FROM THE
C                               PREVIOUS CALL, I.E. IF THE ONLY
C                               CHANGE ON INPUT TO IDBVIP IS IN THE
C                               ZD ARRAY.  IF MD=3 AND NDP OR NIP HAS
C                               BEEN CHANGED SINCE THE PREVIOUS CALL
C                               TO IDBVIP, AN ERROR RETURN OCCURS.
C
C                           BETWEEN THE CALL WITH MD=2 OR MD=3 AND
C                           THE PRECEDING CALL, THE IWK AND WK WORK
C                           ARRAYS SHOULD NOT BE DISTURBED.
C
C                        NDP
C                          NUMBER OF DATA POINTS (MUST BE 4 OR
C                          GREATER, ELSE AN ERROR RETURN OCCURS).
C
C                        XD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE
C                          X COORDINATES OF THE DATA POINTS.
C
C                        YD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE
C                          Y COORDINATES OF THE DATA POINTS.
C
C                        ZD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE
C                          Z COORDINATES OF THE DATA POINTS.
C
C                        NIP
C                          THE NUMBER OF OUTPUT POINTS AT WHICH
C                          INTERPOLATION IS TO BE PERFORMED (MUST BE
C                          1 OR GREATER, ELSE AN ERROR RETURN OCCURS).
C
C                        XI
C                          ARRAY OF DIMENSION  NIP  CONTAINING THE X
C                          COORDINATES OF THE OUTPUT POINTS.
C
C                        YI
C                          ARRAY OF DIMENSION  NIP  CONTAINING THE Y
C                          COORDINATES OF THE OUTPUT POINTS.
C
C                        IWK
C                          INTEGER WORK ARRAY OF DIMENSION AT LEAST
C                          31*NDP + NIP
C
C                        WK
C                          REAL WORK ARRAY OF DIMENSION AT LEAST 8*NDP
C
C ON OUTPUT               ZI
C                           ARRAY OF DIMENSION NIP WHERE INTERPOLATED
C                           Z  VALUES ARE TO BE STORED.
C
C SPECIAL CONDITIONS     INADEQUATE WORK SPACE IWK AND WK MAY
C                        MAY CAUSE INCORRECT RESULTS.
C
C                        THE DATA POINTS MUST BE DISTINCT AND THEIR
C                        PROJECTIONS IN THE X-Y PLANE MUST NOT BE
C                        COLLINEAR, OTHERWISE AN ERROR RETURN OCCURS.
C ********************************************************************
C
C      SUBROUTINE IDSFFT (MD,NDP,XD,YD,ZD,NXI,NYI,NZI,XI,YI,ZI,IWK,WK)
C
C DIMENSION OF           XD(NDP), YD(NDP),     ZD(NDP),   XI(NXI),
C ARGUMENTS              YI(NYI), ZI(NZI,NYI), WK(6*NDP),
C                        IWK(31*NDP + NXI*NYI)
C
C PURPOSE                THIS SUBROUTINE PERFORMS SMOOTH SURFACE
C                        FITTING WHEN THE PROJECTIONS OF THE DATA
C                        POINTS IN THE X-Y PLANE ARE IRREGULARLY
C                        DISTRIBUTED IN THE PLANE.
C
C USAGE                  CALL IDSFFT (MD,NDP,XD,YD,ZD,NXI,NYI,NZI,
C                                    XI,YI,ZI,IWK,WK)
C
C ARGUMENTS
C
C ON INPUT               MD
C                          MODE OF COMPUTATION (MUST BE 1, 2, OR 3,
C                          ELSE AN ERROR RETURN WILL OCCUR).
C                           = 1 IF THIS IS THE FIRST CALL TO THIS
C                               SUBROUTINE, OR IF THE VALUE OF NDP
C                               HAS BEEN CHANGED FROM THE PREVIOUS
C                               CALL, OR IF THE CONTENTS OF THE XD
C                               OR YD ARRAYS HAVE BEEN CHANGED FROM
C                               THE PREVIOUS CALL.
C                           = 2 IF THE VALUES OF NDP AND THE XD,
C                               YD ARRAYS ARE UNCHANGED FROM THE
C                               PREVIOUS CALL, BUT NEW VALUES FOR
C                               XI, YI ARE BEING USED.  IF MD = 2
C                               AND NDP HAS BEEN CHANGED SINCE THE
C                               PREVIOUS CALL TO IDSFFT, AN ERROR
C                               RETURN OCCURS.
C                           = 3 IF THE VALUES OF NDP, NXI, NYI, XD,
C                               YD, XI, YI ARE UNCHANGED FROM THE
C                               PREVIOUS CALL, I.E. IF THE ONLY CHANGE
C                               ON INPUT TO IDSFFT IS IN THE ZD ARRAY.
C                               IF MD = 3 AND NDP, NXI OR NYI HAS BEEN
C                               CHANGED SINCE THE PREVIOUS CALL TO
C                               IDSFFT, AN ERROR RETURN OCCURS.
C
C                           BETWEEN THE CALL WITH MD=2 OR MD=3 AND
C                           THE PRECEDING CALL, THE IWK AND WK WORK
C                           ARRAYS SHOULD NOT BE DISTURBED.
C
C                        NDP
C                          NUMBER OF DATA POINTS (MUST BE 4 OR
C                          GREATER, ELSE AN ERROR RETURN WILL OCCUR).
C
C                        XD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE X
C                          COORDINATES OF THE DATA POINTS.
C
C                        YD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE Y
C                          COORDINATES OF THE DATA POINTS.
C
C                        ZD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE Z
C                          COORDINATES OF THE DATA POINTS.
C
C                        NXI
C                          NUMBER OF OUTPUT GRID POINTS IN THE X-
C                          DIRECTION (MUST BE 1 OR GREATER, ELSE
C                          AN ERROR RETURN WILL OCCUR).
C
C                        NYI
C                          NUMBER OF OUTPUT GRID POINTS IN THE Y-
C                          DIRECTION (MUST BE 1 OR GREATER, ELSE
C                          AN ERROR RETURN WILL OCCUR).
C
C                        NZI
C                          FIRST DIMENSION OF ZI AS DECLARED IN THE
C                          CALLING PROGRAM.  NZI MUST BE GREATER THAN
C                          OR EQUAL TO NXI, ELSE AN ERROR RETURN WILL
C                          OCCUR.
C
C                        XI
C                          ARRAY OF DIMENSION  NXI  CONTAINING THE
C                          X COORDINATES OF THE OUTPUT GRID POINTS.
C
C                        YI
C                         ARRAY OF DIMENSION  NYI  CONTAINING THE
C                         Y COORDINATES OF THE OUTPUT GRID POINTS.
C
C                        IWK
C                          INTEGER WORK ARRAY OF DIMENSION AT
C                          LEAST 31*NDP + NXI*NYI
C
C                        WK
C                          REAL WORK ARRAY OF DIMENSION AT LEAST 6*NDP
C
C ON OUTPUT              ZI
C                           REAL, TWO-DIMENSIONAL ARRAY OF DIMENSION
C                           (NZI,NYI), STORING THE INTERPOLATED Z
C                           VALUES AT THE OUTPUT GRID POINTS.
C
C SPECIAL CONDITIONS     INADEQUATE WORK SPACE IWK AND WK MAY
C                        MAY CAUSE INCORRECT RESULTS.
C
C                        THE DATA POINTS MUST BE DISTINCT AND THEIR
C                        PROJECTIONS IN THE X-Y PLANE MUST NOT BE
C                        COLLINEAR, OTHERWISE AN ERROR RETURN OCCURS.
C ********************************************************************
      SUBROUTINE  IDBVIP(MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,
     1                   IWK,WK)
C THIS SUBROUTINE CALLS THE IDLCTN, IDPDRV, IDPTIP, AND IDTANG
C SUBROUTINES.
C DECLARATION STATEMENTS
      DIMENSION XD(NDP), YD(NDP), ZD(NDP),           XI(NIP),
     1          YI(NIP), ZI(NIP), IWK(31*NDP + NIP), WK(8*NDP)
      COMMON/IDLC/ITIPV,DMMY1(13)
      COMMON/IDPT/ITPV,DMMY(27)
C
C  THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
C SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES.
C (FOR MD=1,2,3)
   10 MD0=MD
      NDP0=NDP
      NIP0=NIP
C ERROR CHECK.  (FOR MD=1,2,3)
   20 IF (MD0.LT.1.OR.MD0.GT.3) THEN
         CALL ULIBER (32,
     1' IDBVIP (BIVAR) - INPUT PARAMETER MD OUT OF RANGE',49)
         STOP 'ULIBER32'
      ENDIF
      IF (NDP0.LT.4) THEN
         CALL ULIBER (33,
     1' IDBVIP (BIVAR) - INPUT PARAMETER NDP OUT OF RANGE',50)
         STOP 'ULIBER33'
      ENDIF
      IF (NIP0.LT.1) THEN
         CALL ULIBER (34,
     1' IDBVIP (BIVAR) - INPUT PARAMETER NIP OUT OF RANGE',50)
         STOP 'ULIBER34'
      ENDIF
      IF(MD0.GT.1)        GO TO 21
      IWK(1)=NDP0
      GO TO 22
   21 NDPPV=IWK(1)
      IF (NDP0.NE.NDPPV) THEN
         CALL ULIBER (50,
     1' IDBVIP (BIVAR) - MD=2 OR 3 BUT NDP WAS CHANGED SINCE LAST CALL',
     2                63)
         STOP 'ULIBER50'
      ENDIF
   22 IF(MD0.GT.2)        GO TO 23
      IWK(3)=NIP
      GO TO 30
   23 NIPPV=IWK(3)
      IF (NIP0.LT.NIPPV) THEN
         CALL ULIBER (51,
     1' IDBVIP (BIVAR) - MD=3 BUT NIP WAS CHANGED SINCE LAST CALL',
     2                58)
         STOP 'ULIBER51'
      ENDIF
C ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3)
   30 JWIPT=16
      JWIWL=6*NDP0+1
      JWIWK=JWIWL
      JWIPL=24*NDP0+1
      JWIWP=30*NDP0+1
      JWIT0=31*NDP0
      JWWPD=5*NDP0+1
C TRIANGULATES THE X-Y PLANE.  (FOR MD=1)
   40 IF(MD0.GT.1)   GO TO 41
      CALL IDTANG(NDP0,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1            IWK(JWIWL),IWK(JWIWP),WK)
      IWK(5)=NT
      IWK(6)=NL
      IF(NT.EQ.0)    RETURN
      GO TO 50
   41 NT=IWK(5)
      NL=IWK(6)
C LOCATES ALL POINTS AT WHICH INTERPOLATION IS TO BE PERFORMED.
C (FOR MD=1,2)
   50 IF(MD0.GT.2)   GO TO 60
      ITIPV=0
      JWIT=JWIT0
      DO 51  IIP=1,NIP0
        JWIT=JWIT+1
        CALL IDLCTN(NDP0,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1            XI(IIP),YI(IIP),IWK(JWIT),IWK(JWIWK),WK)
   51 CONTINUE
C ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS.
C (FOR MD=1,2,3)
   60 CALL IDPDRV(NDP0,XD,YD,ZD,NT,IWK(JWIPT),WK,WK(JWWPD))
C INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3)
   70 ITPV=0
      JWIT=JWIT0
      DO 71  IIP=1,NIP0
        JWIT=JWIT+1
        CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1              IWK(JWIT),XI(IIP),YI(IIP),ZI(IIP))
   71 CONTINUE
      RETURN
      END
      SUBROUTINE  IDGRID(XD,YD,NT,IPT,NL,IPL,NXI,NYI,XI,YI,
     1                   NGP,IGP)
C THIS SUBROUTINE ORGANIZES GRID POINTS FOR SURFACE FITTING BY
C SORTING THEM IN ASCENDING ORDER OF TRIANGLE NUMBERS AND OF THE
C BORDER LINE SEGMENT NUMBER.
C THE INPUT PARAMETERS ARE
C     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y
C           COORDINATES OF THE DATA POINTS, WHERE NDP IS THE
C           NUMBER OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE
C           POINT NUMBERS OF THE END POINTS OF THE BORDER
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE
C           NUMBERS,
C     NXI = NUMBER OF GRID POINTS IN THE X COORDINATE,
C     NYI = NUMBER OF GRID POINTS IN THE Y COORDINATE,
C     XI,YI = ARRAYS OF DIMENSION NXI AND NYI CONTAINING
C           THE X AND Y COORDINATES OF THE GRID POINTS,
C           RESPECTIVELY.
C THE OUTPUT PARAMETERS ARE
C     NGP = INTEGER ARRAY OF DIMENSION 2*(NT+2*NL) WHERE THE
C           NUMBER OF GRID POINTS THAT BELONG TO EACH OF THE
C           TRIANGLES OR OF THE BORDER LINE SEGMENTS ARE TO
C           BE STORED,
C     IGP = INTEGER ARRAY OF DIMENSION NXI*NYI WHERE THE
C           GRID POINT NUMBERS ARE TO BE STORED IN ASCENDING
C           ORDER OF THE TRIANGLE NUMBER AND THE BORDER LINE
C           SEGMENT NUMBER.
C DECLARATION STATEMENTS
      DIMENSION XD(1), YD(1), IPT(1), IPL(1), XI(1), YI(1), NGP(1),
     1          IGP(1)
C STATEMENT FUNCTIONS
      SPDT(U1,V1,U2,V2,U3,V3)=(U1-U2)*(U3-U2)+(V1-V2)*(V3-V2)
      VPDT(U1,V1,U2,V2,U3,V3)=(U1-U3)*(V2-V3)-(V1-V3)*(U2-U3)
C PRELIMINARY PROCESSING
   10 NT0=NT
      NL0=NL
      NXI0=NXI
      NYI0=NYI
      NXINYI=NXI0*NYI0
      XIMN=AMIN1(XI(1),XI(NXI0))
      XIMX=AMAX1(XI(1),XI(NXI0))
      YIMN=AMIN1(YI(1),YI(NYI0))
      YIMX=AMAX1(YI(1),YI(NYI0))
C DETERMINES GRID POINTS INSIDE THE DATA AREA.
   20 JNGP0=0
      JNGP1=2*(NT0+2*NL0)+1
      JIGP0=0
      JIGP1=NXINYI+1
      DO 39  IT0=1,NT0
        NGP0=0
        NGP1=0
        IT0T3=IT0*3
        IP1=IPT(IT0T3-2)
        IP2=IPT(IT0T3-1)
        IP3=IPT(IT0T3)
        X1=XD(IP1)
        Y1=YD(IP1)
        X2=XD(IP2)
        Y2=YD(IP2)
        X3=XD(IP3)
        Y3=YD(IP3)
        XMN=AMIN1(X1,X2,X3)
        XMX=AMAX1(X1,X2,X3)
        YMN=AMIN1(Y1,Y2,Y3)
        YMX=AMAX1(Y1,Y2,Y3)
        INSD=0
        DO 22  IXI=1,NXI0
          IF(XI(IXI).GE.XMN.AND.XI(IXI).LE.XMX)    GO TO 21
          IF(INSD.EQ.0)   GO TO 22
          IXIMX=IXI-1
          GO TO 23
   21     IF(INSD.EQ.1)   GO TO 22
          INSD=1
          IXIMN=IXI
   22   CONTINUE
        IF(INSD.EQ.0)     GO TO 38
        IXIMX=NXI0
   23   DO 37  IYI=1,NYI0
          YII=YI(IYI)
          IF(YII.LT.YMN.OR.YII.GT.YMX)        GO TO 37
          DO 36  IXI=IXIMN,IXIMX
            XII=XI(IXI)
            L=0
            IF(VPDT(X1,Y1,X2,Y2,XII,YII))     36,25,26
   25       L=1
   26       IF(VPDT(X2,Y2,X3,Y3,XII,YII))     36,27,28
   27       L=1
   28       IF(VPDT(X3,Y3,X1,Y1,XII,YII))     36,29,30
   29       L=1
   30       IZI=NXI0*(IYI-1)+IXI
            IF(L.EQ.1)    GO TO 31
            NGP0=NGP0+1
            JIGP0=JIGP0+1
            IGP(JIGP0)=IZI
            GO TO 36
   31       IF(JIGP1.GT.NXINYI)     GO TO 33
            DO 32  JIGP1I=JIGP1,NXINYI
              IF(IZI.EQ.IGP(JIGP1I))     GO TO 36
   32       CONTINUE
   33       NGP1=NGP1+1
            JIGP1=JIGP1-1
            IGP(JIGP1)=IZI
   36     CONTINUE
   37   CONTINUE
   38   JNGP0=JNGP0+1
        NGP(JNGP0)=NGP0
        JNGP1=JNGP1-1
        NGP(JNGP1)=NGP1
   39 CONTINUE
C DETERMINES GRID POINTS OUTSIDE THE DATA AREA.
C - IN SEMI-INFINITE RECTANGULAR AREA.
   40 DO 79  IL0=1,NL0
        NGP0=0
        NGP1=0
        IL0T3=IL0*3
        IP1=IPL(IL0T3-2)
        IP2=IPL(IL0T3-1)
        X1=XD(IP1)
        Y1=YD(IP1)
        X2=XD(IP2)
        Y2=YD(IP2)
        XMN=XIMN
        XMX=XIMX
        YMN=YIMN
        YMX=YIMX
        IF(Y2.GE.Y1)      XMN=AMIN1(X1,X2)
        IF(Y2.LE.Y1)      XMX=AMAX1(X1,X2)
        IF(X2.LE.X1)      YMN=AMIN1(Y1,Y2)
        IF(X2.GE.X1)      YMX=AMAX1(Y1,Y2)
        INSD=0
        DO 42  IXI=1,NXI0
          IF(XI(IXI).GE.XMN.AND.XI(IXI).LE.XMX)    GO TO 41
          IF(INSD.EQ.0)   GO TO 42
          IXIMX=IXI-1
          GO TO 43
   41     IF(INSD.EQ.1)   GO TO 42
          INSD=1
          IXIMN=IXI
   42   CONTINUE
        IF(INSD.EQ.0)     GO TO 58
        IXIMX=NXI0
   43   DO 57  IYI=1,NYI0
          YII=YI(IYI)
          IF(YII.LT.YMN.OR.YII.GT.YMX)        GO TO 57
          DO 56  IXI=IXIMN,IXIMX
            XII=XI(IXI)
            L=0
            IF(VPDT(X1,Y1,X2,Y2,XII,YII))     46,45,56
   45       L=1
   46       IF(SPDT(X2,Y2,X1,Y1,XII,YII))     56,47,48
   47       L=1
   48       IF(SPDT(X1,Y1,X2,Y2,XII,YII))     56,49,50
   49       L=1
   50       IZI=NXI0*(IYI-1)+IXI
            IF(L.EQ.1)    GO TO 51
            NGP0=NGP0+1
            JIGP0=JIGP0+1
            IGP(JIGP0)=IZI
            GO TO 56
   51       IF(JIGP1.GT.NXINYI)     GO TO 53
            DO 52  JIGP1I=JIGP1,NXINYI
              IF(IZI.EQ.IGP(JIGP1I))     GO TO 56
   52       CONTINUE
   53       NGP1=NGP1+1
            JIGP1=JIGP1-1
            IGP(JIGP1)=IZI
   56     CONTINUE
   57   CONTINUE
   58   JNGP0=JNGP0+1
        NGP(JNGP0)=NGP0
        JNGP1=JNGP1-1
        NGP(JNGP1)=NGP1
C - IN SEMI-INFINITE TRIANGULAR AREA.
   60   NGP0=0
        NGP1=0
        ILP1=MOD(IL0,NL0)+1
        ILP1T3=ILP1*3
        IP3=IPL(ILP1T3-1)
        X3=XD(IP3)
        Y3=YD(IP3)
        XMN=XIMN
        XMX=XIMX
        YMN=YIMN
        YMX=YIMX
        IF(Y3.GE.Y2.AND.Y2.GE.Y1)   XMN=X2
        IF(Y3.LE.Y2.AND.Y2.LE.Y1)   XMX=X2
        IF(X3.LE.X2.AND.X2.LE.X1)   YMN=Y2
        IF(X3.GE.X2.AND.X2.GE.X1)   YMX=Y2
        INSD=0
        DO 62  IXI=1,NXI0
          IF(XI(IXI).GE.XMN.AND.XI(IXI).LE.XMX)    GO TO 61
          IF(INSD.EQ.0)   GO TO 62
          IXIMX=IXI-1
          GO TO 63
   61     IF(INSD.EQ.1)   GO TO 62
          INSD=1
          IXIMN=IXI
   62   CONTINUE
        IF(INSD.EQ.0)     GO TO 78
        IXIMX=NXI0
   63   DO 77  IYI=1,NYI0
          YII=YI(IYI)
          IF(YII.LT.YMN.OR.YII.GT.YMX)        GO TO 77
          DO 76  IXI=IXIMN,IXIMX
            XII=XI(IXI)
            L=0
            IF(SPDT(X1,Y1,X2,Y2,XII,YII))     66,65,76
   65       L=1
   66       IF(SPDT(X3,Y3,X2,Y2,XII,YII))     70,67,76
   67       L=1
   70       IZI=NXI0*(IYI-1)+IXI
            IF(L.EQ.1)    GO TO 71
            NGP0=NGP0+1
            JIGP0=JIGP0+1
            IGP(JIGP0)=IZI
            GO TO 76
   71       IF(JIGP1.GT.NXINYI)     GO TO 73
            DO 72  JIGP1I=JIGP1,NXINYI
              IF(IZI.EQ.IGP(JIGP1I))     GO TO 76
   72       CONTINUE
   73       NGP1=NGP1+1
            JIGP1=JIGP1-1
            IGP(JIGP1)=IZI
   76     CONTINUE
   77   CONTINUE
   78   JNGP0=JNGP0+1
        NGP(JNGP0)=NGP0
        JNGP1=JNGP1-1
        NGP(JNGP1)=NGP1
   79 CONTINUE
      RETURN
      END
      SUBROUTINE  IDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,XII,YII,ITI,
     1                   IWK,WK)
C THIS SUBROUTINE LOCATES A POINT, I.E., DETERMINES TO WHAT TRI-
C ANGLE A GIVEN POINT (XII,YII) BELONGS.  WHEN THE GIVEN POINT
C DOES NOT LIE INSIDE THE DATA AREA, THIS SUBROUTINE DETERMINES
C THE BORDER LINE SEGMENT WHEN THE POINT LIES IN AN OUTSIDE
C RECTANGULAR AREA, AND TWO BORDER LINE SEGMENTS WHEN THE POINT
C LIES IN AN OUTSIDE TRIANGULAR AREA.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y
C           COORDINATES OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE
C           POINT NUMBERS OF THE END POINTS OF THE BORDER
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE
C           NUMBERS,
C     XII,YII = X AND Y COORDINATES OF THE POINT TO BE
C           LOCATED.
C THE OUTPUT PARAMETER IS
C     ITI = TRIANGLE NUMBER, WHEN THE POINT IS INSIDE THE
C           DATA AREA, OR
C           TWO BORDER LINE SEGMENT NUMBERS, IL1 AND IL2,
C           CODED TO IL1*(NT+NL)+IL2, WHEN THE POINT IS
C           OUTSIDE THE DATA AREA.
C THE OTHER PARAMETERS ARE
C     IWK = INTEGER ARRAY OF DIMENSION 18*NDP USED INTER-
C           NALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION 8*NDP USED INTERNALLY AS A
C           WORK AREA.
C DECLARATION STATEMENTS
      DIMENSION XD(NDP), YD(NDP), IPT(3*NT), IPL(3*NL), IWK(18*NDP),
     1          WK(8*NDP)
      DIMENSION   IDSC(9)
      COMMON/IDLC/ITIPV,XS1,XS2,YS1,YS2,NTSC(9)
C STATEMENT FUNCTIONS
      SPDT(U1,V1,U2,V2,U3,V3)=(U1-U2)*(U3-U2)+(V1-V2)*(V3-V2)
      VPDT(U1,V1,U2,V2,U3,V3)=(U1-U3)*(V2-V3)-(V1-V3)*(U2-U3)
C PRELIMINARY PROCESSING
   10 NDP0=NDP
      NT0=NT
      NL0=NL
      NTL=NT0+NL0
      X0=XII
      Y0=YII
C PROCESSING FOR A NEW SET OF DATA POINTS
   20 IF(ITIPV.NE.0)      GO TO 30
C - DIVIDES THE X-Y PLANE INTO NINE RECTANGULAR SECTIONS.
      XMN=XD(1)
      XMX=XMN
      YMN=YD(1)
      YMX=YMN
      DO 21  IDP=2,NDP0
        XI=XD(IDP)
        YI=YD(IDP)
        XMN=AMIN1(XI,XMN)
        XMX=AMAX1(XI,XMX)
        YMN=AMIN1(YI,YMN)
        YMX=AMAX1(YI,YMX)
   21 CONTINUE
      XS1=(XMN+XMN+XMX)/3.0
      XS2=(XMN+XMX+XMX)/3.0
      YS1=(YMN+YMN+YMX)/3.0
      YS2=(YMN+YMX+YMX)/3.0
C - DETERMINES AND STORES IN THE IWK ARRAY TRIANGLE NUMBERS OF
C - THE TRIANGLES ASSOCIATED WITH EACH OF THE NINE SECTIONS.
      DO 22  ISC=1,9
        NTSC(ISC)=0
        IDSC(ISC)=0
   22 CONTINUE
      IT0T3=0
      JWK=0
      DO 27  IT0=1,NT0
        IT0T3=IT0T3+3
        I1=IPT(IT0T3-2)
        I2=IPT(IT0T3-1)
        I3=IPT(IT0T3)
        XMN=AMIN1(XD(I1),XD(I2),XD(I3))
        XMX=AMAX1(XD(I1),XD(I2),XD(I3))
        YMN=AMIN1(YD(I1),YD(I2),YD(I3))
        YMX=AMAX1(YD(I1),YD(I2),YD(I3))
        IF(YMN.GT.YS1)                   GO TO 23
        IF(XMN.LE.XS1)                   IDSC(1)=1
        IF(XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(2)=1
        IF(XMX.GE.XS2)                   IDSC(3)=1
   23   IF(YMX.LT.YS1.OR.YMN.GT.YS2)     GO TO 24
        IF(XMN.LE.XS1)                   IDSC(4)=1
        IF(XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(5)=1
        IF(XMX.GE.XS2)                   IDSC(6)=1
   24   IF(YMX.LT.YS2)                   GO TO 25
        IF(XMN.LE.XS1)                   IDSC(7)=1
        IF(XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(8)=1
        IF(XMX.GE.XS2)                   IDSC(9)=1
   25   DO 26  ISC=1,9
          IF(IDSC(ISC).EQ.0)   GO TO 26
          JIWK=9*NTSC(ISC)+ISC
          IWK(JIWK)=IT0
          NTSC(ISC)=NTSC(ISC)+1
          IDSC(ISC)=0
   26   CONTINUE
C - STORES IN THE WK ARRAY THE MINIMUM AND MAXIMUM OF THE X AND
C - Y COORDINATE VALUES FOR EACH OF THE TRIANGLE.
        JWK=JWK+4
        WK(JWK-3)=XMN
        WK(JWK-2)=XMX
        WK(JWK-1)=YMN
        WK(JWK)  =YMX
   27 CONTINUE
      GO TO 60
C CHECKS IF IN THE SAME TRIANGLE AS PREVIOUS.
   30 IT0=ITIPV
      IF(IT0.GT.NT0)      GO TO 40
      IT0T3=IT0*3
      IP1=IPT(IT0T3-2)
      X1=XD(IP1)
      Y1=YD(IP1)
      IP2=IPT(IT0T3-1)
      X2=XD(IP2)
      Y2=YD(IP2)
      IF(VPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.0)      GO TO 60
      IP3=IPT(IT0T3)
      X3=XD(IP3)
      Y3=YD(IP3)
      IF(VPDT(X2,Y2,X3,Y3,X0,Y0).LT.0.0)      GO TO 60
      IF(VPDT(X3,Y3,X1,Y1,X0,Y0).LT.0.0)      GO TO 60
      GO TO 80
C CHECKS IF ON THE SAME BORDER LINE SEGMENT.
   40 IL1=IT0/NTL
      IL2=IT0-IL1*NTL
      IL1T3=IL1*3
      IP1=IPL(IL1T3-2)
      X1=XD(IP1)
      Y1=YD(IP1)
      IP2=IPL(IL1T3-1)
      X2=XD(IP2)
      Y2=YD(IP2)
      IF(IL2.NE.IL1)      GO TO 50
      IF(SPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.0)      GO TO 60
      IF(SPDT(X2,Y2,X1,Y1,X0,Y0).LT.0.0)      GO TO 60
      IF(VPDT(X1,Y1,X2,Y2,X0,Y0).GT.0.0)      GO TO 60
      GO TO 80
C CHECKS IF BETWEEN THE SAME TWO BORDER LINE SEGMENTS.
   50 IF(SPDT(X1,Y1,X2,Y2,X0,Y0).GT.0.0)      GO TO 60
      IP3=IPL(3*IL2-1)
      X3=XD(IP3)
      Y3=YD(IP3)
      IF(SPDT(X3,Y3,X2,Y2,X0,Y0).LE.0.0)      GO TO 80
C LOCATES INSIDE THE DATA AREA.
C - DETERMINES THE SECTION IN WHICH THE POINT IN QUESTION LIES.
   60 ISC=1
      IF(X0.GE.XS1)       ISC=ISC+1
      IF(X0.GE.XS2)       ISC=ISC+1
      IF(Y0.GE.YS1)       ISC=ISC+3
      IF(Y0.GE.YS2)       ISC=ISC+3
C - SEARCHES THROUGH THE TRIANGLES ASSOCIATED WITH THE SECTION.
      NTSCI=NTSC(ISC)
      IF(NTSCI.LE.0)      GO TO 70
      JIWK=-9+ISC
      DO 61  ITSC=1,NTSCI
        JIWK=JIWK+9
        IT0=IWK(JIWK)
        JWK=IT0*4
        IF(X0.LT.WK(JWK-3))    GO TO 61
        IF(X0.GT.WK(JWK-2))    GO TO 61
        IF(Y0.LT.WK(JWK-1))    GO TO 61
        IF(Y0.GT.WK(JWK))      GO TO 61
        IT0T3=IT0*3
        IP1=IPT(IT0T3-2)
        X1=XD(IP1)
        Y1=YD(IP1)
        IP2=IPT(IT0T3-1)
        X2=XD(IP2)
        Y2=YD(IP2)
        IF(VPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.0)    GO TO 61
        IP3=IPT(IT0T3)
        X3=XD(IP3)
        Y3=YD(IP3)
        IF(VPDT(X2,Y2,X3,Y3,X0,Y0).LT.0.0)    GO TO 61
        IF(VPDT(X3,Y3,X1,Y1,X0,Y0).LT.0.0)    GO TO 61
        GO TO 80
   61 CONTINUE
C LOCATES OUTSIDE THE DATA AREA.
   70 DO 72  IL1=1,NL0
        IL1T3=IL1*3
        IP1=IPL(IL1T3-2)
        X1=XD(IP1)
        Y1=YD(IP1)
        IP2=IPL(IL1T3-1)
        X2=XD(IP2)
        Y2=YD(IP2)
        IF(SPDT(X2,Y2,X1,Y1,X0,Y0).LT.0.0)    GO TO 72
        IF(SPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.0)    GO TO 71
        IF(VPDT(X1,Y1,X2,Y2,X0,Y0).GT.0.0)    GO TO 72
        IL2=IL1
        GO TO 75
   71   IL2=MOD(IL1,NL0)+1
        IP3=IPL(3*IL2-1)
        X3=XD(IP3)
        Y3=YD(IP3)
        IF(SPDT(X3,Y3,X2,Y2,X0,Y0).LE.0.0)    GO TO 75
   72 CONTINUE
      IT0=1
      GO TO 80
   75 IT0=IL1*NTL+IL2
C NORMAL EXIT
   80 ITI=IT0
      ITIPV=IT0
      RETURN
      END
      SUBROUTINE  IDPDRV(NDP,XD,YD,ZD,NT,IPT,PD,WK)
C THIS SUBROUTINE ESTIMATES PARTIAL DERIVATIVES OF THE FIRST AND
C SECOND ORDER AT THE DATA POINTS.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X,
C           Y, AND Z COORDINATES OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES.
C THE OUTPUT PARAMETER IS
C     PD  = ARRAY OF DIMENSION 5*NDP, WHERE THE ESTIMATED
C           ZX, ZY, ZXX, ZXY, AND ZYY VALUES AT THE ITH
C           DATA POINT ARE TO BE STORED AS  THE (5*I-4)TH,
C           (5*I-3)RD, (5*I-2)ND, (5*I-1)ST AND (5*I)TH
C           ELEMENTS, RESPECTIVELY, WHERE I = 1, 2, ...,
C           NDP.
C THE OTHER PARAMETER IS
C     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A
C           WORK AREA.
C DECLARATION STATEMENTS
      DIMENSION XD(NDP), YD(NDP), ZD(NDP), IPT(3*NT), PD(5*NDP), WK(NDP)
      DIMENSION   IPTI(3),XV(3),YV(3),ZV(3),ZXV(3),ZYV(3),
     1            W1(3),W2(3)
      DATA  EPSLN/1.0E-6/
C PRELIMINARY PROCESSING
   10 NDP0=NDP
      NT0=NT
C CLEARS THE PD ARRAY.
   20 JPDMX=5*NDP0
      DO 21  JPD=1,JPDMX
        PD(JPD)=0.0
   21 CONTINUE
      DO 22  IDP=1,NDP
        WK(IDP)=0.0
   22 CONTINUE
C ESTIMATES ZX AND ZY.
   30 DO 34  IT=1,NT0
        JPT0=3*(IT-1)
        DO 31  IV=1,3
          JPT=JPT0+IV
          IDP=IPT(JPT)
          IPTI(IV)=IDP
          XV(IV)=XD(IDP)
          YV(IV)=YD(IDP)
          ZV(IV)=ZD(IDP)
   31   CONTINUE
        DX1=XV(2)-XV(1)
        DY1=YV(2)-YV(1)
        DZ1=ZV(2)-ZV(1)
        DX2=XV(3)-XV(1)
        DY2=YV(3)-YV(1)
        DZ2=ZV(3)-ZV(1)
        VPX=DY1*DZ2-DZ1*DY2
        VPY=DZ1*DX2-DX1*DZ2
        VPZ=DX1*DY2-DY1*DX2
        VPZMN=ABS(DX1*DX2+DY1*DY2)*EPSLN
        IF(ABS(VPZ).LE.VPZMN)     GO TO 34
        D12=SQRT((XV(2)-XV(1))**2+(YV(2)-YV(1))**2)
        D23=SQRT((XV(3)-XV(2))**2+(YV(3)-YV(2))**2)
        D31=SQRT((XV(1)-XV(3))**2+(YV(1)-YV(3))**2)
        W1(1)=1.0/(D31*D12)
        W1(2)=1.0/(D12*D23)
        W1(3)=1.0/(D23*D31)
        W2(1)=VPZ*W1(1)
        W2(2)=VPZ*W1(2)
        W2(3)=VPZ*W1(3)
   32   DO 33  IV=1,3
          IDP=IPTI(IV)
          JPD0=5*(IDP-1)
          WI=(W1(IV)**2)*W2(IV)
          PD(JPD0+1)=PD(JPD0+1)+VPX*WI
          PD(JPD0+2)=PD(JPD0+2)+VPY*WI
          WK(IDP)=WK(IDP)+VPZ*WI
   33   CONTINUE
   34 CONTINUE
      DO 36  IDP=1,NDP0
        JPD0=5*(IDP-1)
        PD(JPD0+1)=-PD(JPD0+1)/WK(IDP)
        PD(JPD0+2)=-PD(JPD0+2)/WK(IDP)
   36 CONTINUE
C ESTIMATES ZXX, ZXY, AND ZYY.
   40 DO 44  IT=1,NT0
        JPT0=3*(IT-1)
        DO 41  IV=1,3
          JPT=JPT0+IV
          IDP=IPT(JPT)
          IPTI(IV)=IDP
          XV(IV)=XD(IDP)
          YV(IV)=YD(IDP)
          JPD0=5*(IDP-1)
          ZXV(IV)=PD(JPD0+1)
          ZYV(IV)=PD(JPD0+2)
   41   CONTINUE
        DX1=XV(2)-XV(1)
        DY1=YV(2)-YV(1)
        DZX1=ZXV(2)-ZXV(1)
        DZY1=ZYV(2)-ZYV(1)
        DX2=XV(3)-XV(1)
        DY2=YV(3)-YV(1)
        DZX2=ZXV(3)-ZXV(1)
        DZY2=ZYV(3)-ZYV(1)
        VPXX=DY1*DZX2-DZX1*DY2
        VPXY=DZX1*DX2-DX1*DZX2
        VPYX=DY1*DZY2-DZY1*DY2
        VPYY=DZY1*DX2-DX1*DZY2
        VPZ=DX1*DY2-DY1*DX2
        VPZMN=ABS(DX1*DX2+DY1*DY2)*EPSLN
        IF(ABS(VPZ).LE.VPZMN)     GO TO 44
        D12=SQRT((XV(2)-XV(1))**2+(YV(2)-YV(1))**2)
        D23=SQRT((XV(3)-XV(2))**2+(YV(3)-YV(2))**2)
        D31=SQRT((XV(1)-XV(3))**2+(YV(1)-YV(3))**2)
        W1(1)=1.0/(D31*D12)
        W1(2)=1.0/(D12*D23)
        W1(3)=1.0/(D23*D31)
        W2(1)=VPZ*W1(1)
        W2(2)=VPZ*W1(2)
        W2(3)=VPZ*W1(3)
   42   DO 43  IV=1,3
          IDP=IPTI(IV)
          JPD0=5*(IDP-1)
          WI=(W1(IV)**2)*W2(IV)
          PD(JPD0+3)=PD(JPD0+3)+VPXX*WI
          PD(JPD0+4)=PD(JPD0+4)+(VPXY+VPYX)*WI
          PD(JPD0+5)=PD(JPD0+5)+VPYY*WI
   43   CONTINUE
   44 CONTINUE
      DO 46  IDP=1,NDP0
        JPD0=5*(IDP-1)
        PD(JPD0+3)=-PD(JPD0+3)/WK(IDP)
        PD(JPD0+4)=-PD(JPD0+4)/(2.0*WK(IDP))
        PD(JPD0+5)=-PD(JPD0+5)/WK(IDP)
   46 CONTINUE
      RETURN
      END
      SUBROUTINE  IDPTIP(XD,YD,ZD,NT,IPT,NL,IPL,PDD,ITI,XII,YII,
     1                   ZII)
C THIS SUBROUTINE PERFORMS PUNCTUAL INTERPOLATION OR EXTRAPOLA-
C TION, I.E., DETERMINES THE Z VALUE AT A POINT.
C THE INPUT PARAMETERS ARE
C     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X,
C           Y, AND Z COORDINATES OF THE DATA POINTS, WHERE
C           NDP IS THE NUMBER OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE
C           POINT NUMBERS OF THE END POINTS OF THE BORDER
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE
C           NUMBERS,
C     PDD = ARRAY OF DIMENSION 5*NDP CONTAINING THE PARTIAL
C           DERIVATIVES AT THE DATA POINTS,
C     ITI = TRIANGLE NUMBER OF THE TRIANGLE IN WHICH LIES
C           THE POINT FOR WHICH INTERPOLATION IS TO BE
C           PERFORMED,
C     XII,YII = X AND Y COORDINATES OF THE POINT FOR WHICH
C           INTERPOLATION IS TO BE PERFORMED.
C THE OUTPUT PARAMETER IS
C     ZII = INTERPOLATED Z VALUE.
C DECLARATION STATEMENTS
      DIMENSION XD(1), YD(1), ZD(1), IPT(1), IPL(1), PDD(1)
      COMMON/IDPT/ITPV,X0,Y0,AP,BP,CP,DP,
     1            P00,P10,P20,P30,P40,P50,P01,P11,P21,P31,P41,
     2            P02,P12,P22,P32,P03,P13,P23,P04,P14,P05
      DIMENSION   X(3),Y(3),Z(3),PD(15),
     1            ZU(3),ZV(3),ZUU(3),ZUV(3),ZVV(3)
      REAL        LU,LV
      EQUIVALENCE (P5,P50)
C PRELIMINARY PROCESSING
   10 IT0=ITI
      NTL=NT+NL
      IF(IT0.LE.NTL)      GO TO 20
      IL1=IT0/NTL
      IL2=IT0-IL1*NTL
      IF(IL1.EQ.IL2)      GO TO 40
      GO TO 60
C CALCULATION OF ZII BY INTERPOLATION.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
   20 IF(IT0.EQ.ITPV)     GO TO 30
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE
C VERTEXES.
   21 JIPT=3*(IT0-1)
      JPD=0
      DO 23  I=1,3
        JIPT=JIPT+1
        IDP=IPT(JIPT)
        X(I)=XD(IDP)
        Y(I)=YD(IDP)
        Z(I)=ZD(IDP)
        JPDD=5*(IDP-1)
        DO 22  KPD=1,5
          JPD=JPD+1
          JPDD=JPDD+1
          PD(JPD)=PDD(JPDD)
   22   CONTINUE
   23 CONTINUE
C DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM
C TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM
C AND VICE VERSA.
   24 X0=X(1)
      Y0=Y(1)
      A=X(2)-X0
      B=X(3)-X0
      C=Y(2)-Y0
      D=Y(3)-Y0
      AD=A*D
      BC=B*C
      DLT=AD-BC
      AP= D/DLT
      BP=-B/DLT
      CP=-C/DLT
      DP= A/DLT
C CONVERTS THE PARTIAL DERIVATIVES AT THE VERTEXES OF THE
C TRIANGLE FOR THE U-V COORDINATE SYSTEM.
   25 AA=A*A
      ACT2=2.0*A*C
      CC=C*C
      AB=A*B
      ADBC=AD+BC
      CD=C*D
      BB=B*B
      BDT2=2.0*B*D
      DD=D*D
      DO 26  I=1,3
        JPD=5*I
        ZU(I)=A*PD(JPD-4)+C*PD(JPD-3)
        ZV(I)=B*PD(JPD-4)+D*PD(JPD-3)
        ZUU(I)=AA*PD(JPD-2)+ACT2*PD(JPD-1)+CC*PD(JPD)
        ZUV(I)=AB*PD(JPD-2)+ADBC*PD(JPD-1)+CD*PD(JPD)
        ZVV(I)=BB*PD(JPD-2)+BDT2*PD(JPD-1)+DD*PD(JPD)
   26 CONTINUE
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
   27 P00=Z(1)
      P10=ZU(1)
      P01=ZV(1)
      P20=0.5*ZUU(1)
      P11=ZUV(1)
      P02=0.5*ZVV(1)
      H1=Z(2)-P00-P10-P20
      H2=ZU(2)-P10-ZUU(1)
      H3=ZUU(2)-ZUU(1)
      P30= 10.0*H1-4.0*H2+0.5*H3
      P40=-15.0*H1+7.0*H2    -H3
      P50=  6.0*H1-3.0*H2+0.5*H3
      H1=Z(3)-P00-P01-P02
      H2=ZV(3)-P01-ZVV(1)
      H3=ZVV(3)-ZVV(1)
      P03= 10.0*H1-4.0*H2+0.5*H3
      P04=-15.0*H1+7.0*H2    -H3
      P05=  6.0*H1-3.0*H2+0.5*H3
      LU=SQRT(AA+CC)
      LV=SQRT(BB+DD)
      THXU=ATAN2(C,A)
      THUV=ATAN2(D,B)-THXU
      CSUV=COS(THUV)
      P41=5.0*LV*CSUV/LU*P50
      P14=5.0*LU*CSUV/LV*P05
      H1=ZV(2)-P01-P11-P41
      H2=ZUV(2)-P11-4.0*P41
      P21= 3.0*H1-H2
      P31=-2.0*H1+H2
      H1=ZU(3)-P10-P11-P14
      H2=ZUV(3)-P11-4.0*P14
      P12= 3.0*H1-H2
      P13=-2.0*H1+H2
      THUS=ATAN2(D-C,B-A)-THXU
      THSV=THUV-THUS
      AA= SIN(THSV)/LU
      BB=-COS(THSV)/LU
      CC= SIN(THUS)/LV
      DD= COS(THUS)/LV
      AC=AA*CC
      AD=AA*DD
      BC=BB*CC
      G1=AA*AC*(3.0*BC+2.0*AD)
      G2=CC*AC*(3.0*AD+2.0*BC)
      H1=-AA*AA*AA*(5.0*AA*BB*P50+(4.0*BC+AD)*P41)
     1   -CC*CC*CC*(5.0*CC*DD*P05+(4.0*AD+BC)*P14)
      H2=0.5*ZVV(2)-P02-P12
      H3=0.5*ZUU(3)-P20-P21
      P22=(G1*H2+G2*H3-H1)/(G1+G2)
      P32=H2-P22
      P23=H3-P22
      ITPV=IT0
C CONVERTS XII AND YII TO U-V SYSTEM.
   30 DX=XII-X0
      DY=YII-Y0
      U=AP*DX+BP*DY
      V=CP*DX+DP*DY
C EVALUATES THE POLYNOMIAL.
   31 P0=P00+V*(P01+V*(P02+V*(P03+V*(P04+V*P05))))
      P1=P10+V*(P11+V*(P12+V*(P13+V*P14)))
      P2=P20+V*(P21+V*(P22+V*P23))
      P3=P30+V*(P31+V*P32)
      P4=P40+V*P41
      ZII=P0+U*(P1+U*(P2+U*(P3+U*(P4+U*P5))))
      RETURN
C CALCULATION OF ZII BY EXTRAPOLATION IN THE RECTANGLE.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
   40 IF(IT0.EQ.ITPV)     GO TO 50
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE END
C POINTS OF THE BORDER LINE SEGMENT.
   41 JIPL=3*(IL1-1)
      JPD=0
      DO 43  I=1,2
        JIPL=JIPL+1
        IDP=IPL(JIPL)
        X(I)=XD(IDP)
        Y(I)=YD(IDP)
        Z(I)=ZD(IDP)
        JPDD=5*(IDP-1)
        DO 42  KPD=1,5
          JPD=JPD+1
          JPDD=JPDD+1
          PD(JPD)=PDD(JPDD)
   42   CONTINUE
   43 CONTINUE
C DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM
C TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM
C AND VICE VERSA.
   44 X0=X(1)
      Y0=Y(1)
      A=Y(2)-Y(1)
      B=X(2)-X(1)
      C=-B
      D=A
      AD=A*D
      BC=B*C
      DLT=AD-BC
      AP= D/DLT
      BP=-B/DLT
      CP=-BP
      DP= AP
C CONVERTS THE PARTIAL DERIVATIVES AT THE END POINTS OF THE
C BORDER LINE SEGMENT FOR THE U-V COORDINATE SYSTEM.
   45 AA=A*A
      ACT2=2.0*A*C
      CC=C*C
      AB=A*B
      ADBC=AD+BC
      CD=C*D
      BB=B*B
      BDT2=2.0*B*D
      DD=D*D
      DO 46  I=1,2
        JPD=5*I
        ZU(I)=A*PD(JPD-4)+C*PD(JPD-3)
        ZV(I)=B*PD(JPD-4)+D*PD(JPD-3)
        ZUU(I)=AA*PD(JPD-2)+ACT2*PD(JPD-1)+CC*PD(JPD)
        ZUV(I)=AB*PD(JPD-2)+ADBC*PD(JPD-1)+CD*PD(JPD)
        ZVV(I)=BB*PD(JPD-2)+BDT2*PD(JPD-1)+DD*PD(JPD)
   46 CONTINUE
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
   47 P00=Z(1)
      P10=ZU(1)
      P01=ZV(1)
      P20=0.5*ZUU(1)
      P11=ZUV(1)
      P02=0.5*ZVV(1)
      H1=Z(2)-P00-P01-P02
      H2=ZV(2)-P01-ZVV(1)
      H3=ZVV(2)-ZVV(1)
      P03= 10.0*H1-4.0*H2+0.5*H3
      P04=-15.0*H1+7.0*H2    -H3
      P05=  6.0*H1-3.0*H2+0.5*H3
      H1=ZU(2)-P10-P11
      H2=ZUV(2)-P11
      P12= 3.0*H1-H2
      P13=-2.0*H1+H2
      P21=0.0
      P23=-ZUU(2)+ZUU(1)
      P22=-1.5*P23
      ITPV=IT0
C CONVERTS XII AND YII TO U-V SYSTEM.
   50 DX=XII-X0
      DY=YII-Y0
      U=AP*DX+BP*DY
      V=CP*DX+DP*DY
C EVALUATES THE POLYNOMIAL.
   51 P0=P00+V*(P01+V*(P02+V*(P03+V*(P04+V*P05))))
      P1=P10+V*(P11+V*(P12+V*P13))
      P2=P20+V*(P21+V*(P22+V*P23))
      ZII=P0+U*(P1+U*P2)
      RETURN
C CALCULATION OF ZII BY EXTRAPOLATION IN THE TRIANGLE.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
   60 IF(IT0.EQ.ITPV)     GO TO 70
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE VERTEX
C OF THE TRIANGLE.
   61 JIPL=3*IL2-2
      IDP=IPL(JIPL)
      X0=XD(IDP)
      Y0=YD(IDP)
      Z0=ZD(IDP)
      JPDD=5*(IDP-1)
      DO 62  KPD=1,5
        JPDD=JPDD+1
        PD(KPD)=PDD(JPDD)
   62 CONTINUE
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
   67 P00=Z0
      P10=PD(1)
      P01=PD(2)
      P20=0.5*PD(3)
      P11=PD(4)
      P02=0.5*PD(5)
      ITPV=IT0
C CONVERTS XII AND YII TO U-V SYSTEM.
   70 U=XII-X0
      V=YII-Y0
C EVALUATES THE POLYNOMIAL.
   71 P0=P00+V*(P01+V*P02)
      P1=P10+V*P11
      ZII=P0+U*(P1+U*P20)
      RETURN
      END
      SUBROUTINE IDSFFT (MD,NDP,XD,YD,ZD,NXI,NYI,NZI,XI,YI,ZI,
     1                   IWK,WK)
C THIS SUBROUTINE CALLS THE IDGRID, IDPDRV, IDPTIP, AND IDTANG
C SUBROUTINES.
C DECLARATION STATEMENTS
      DIMENSION XD(NDP), YD(NDP),     ZD(NDP),               XI(NXI),
     1          YI(NYI), ZI(NZI,NYI), IWK(31*NDP + NXI*NYI), WK(6*NDP)
      COMMON/IDPT/ITPV,DMMY(27)
C
C  THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
C SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES.
C (FOR MD=1,2,3)
   10 MD0=MD
      NDP0=NDP
      NXI0=NXI
      NYI0=NYI
C ERROR CHECK.  (FOR MD=1,2,3)
   20 IF (MD0.LT.1.OR.MD0.GT.3) THEN
         CALL ULIBER (39,
     1' IDSFFT (BIVAR) - INPUT PARAMETER MD OUT OF RANGE',49)
         STOP 'ULIBER39'
      ENDIF
      IF (NDP0.LT.4) THEN
         CALL ULIBER (40,
     1' IDSFFT (BIVAR) - INPUT PARAMETER NDP OUT OF RANGE',50)
         STOP 'ULIBER40'
      ENDIF
      IF (NXI0.LT.1.OR.NYI0.LT.1) THEN
         CALL ULIBER (41,
     1' IDSFFT (BIVAR) - INPUT PARAMETER NXI OR NYI OUT OF RANGE',57)
         STOP 'ULIBER41'
      ENDIF
      IF (NXI0.GT.NZI) THEN
         CALL ULIBER (42,
     1' IDSFFT (BIVAR) - INPUT PARAMETER NZI IS LESS THAN NXI',54)
         STOP 'ULIBER42'
      ENDIF
      IF(MD0.GT.1)        GO TO 21
      IWK(1)=NDP0
      GO TO 22
   21 NDPPV=IWK(1)
      IF (NDP0.NE.NDPPV) THEN
         CALL ULIBER (43,
     1' IDSFFT (BIVAR) - MD=2 OR 3 BUT NDP WAS CHANGED SINCE LAST CALL',
     2                63)
         STOP 'ULIBER43'
      ENDIF
   22 IF(MD0.GT.2)        GO TO 23
      IWK(3)=NXI0
      IWK(4)=NYI0
      GO TO 30
   23 NXIPV=IWK(3)
      NYIPV=IWK(4)
      IF (NXI0.NE.NXIPV) THEN
         CALL ULIBER (45,
     1' IDSFFT (BIVAR) - MD=3 BUT NXI WAS CHANGED SINCE LAST CALL',
     2                58)
         STOP 'ULIBER45'
      ENDIF
      IF (NYI0.NE.NYIPV) THEN
         CALL ULIBER (46,
     1' IDSFFT (BIVAR) - MD=3 BUT NYI WAS CHANGED SINCE LAST CALL',
     2                58)
         STOP 'ULIBER46'
      ENDIF
C ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3)
   30 JWIPT=16
      JWIWL=6*NDP0+1
      JWNGP0=JWIWL-1
      JWIPL=24*NDP0+1
      JWIWP=30*NDP0+1
      JWIGP0=31*NDP0
      JWWPD=5*NDP0+1
C TRIANGULATES THE X-Y PLANE.  (FOR MD=1)
   40 IF(MD0.GT.1)   GO TO 41
      CALL IDTANG(NDP0,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1            IWK(JWIWL),IWK(JWIWP),WK)
      IWK(5)=NT
      IWK(6)=NL
      IF(NT.EQ.0)    RETURN
      GO TO 50
   41 NT=IWK(5)
      NL=IWK(6)
C SORTS OUTPUT GRID POINTS IN ASCENDING ORDER OF THE TRIANGLE
C NUMBER AND THE BORDER LINE SEGMENT NUMBER.  (FOR MD=1,2)
   50 IF(MD0.GT.2)   GO TO 60
      CALL IDGRID(XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),NXI0,NYI0,
     1            XI,YI,IWK(JWNGP0+1),IWK(JWIGP0+1))
C ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS.
C (FOR MD=1,2,3)
   60 CALL IDPDRV(NDP0,XD,YD,ZD,NT,IWK(JWIPT),WK,WK(JWWPD))
C INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3)
   70 ITPV=0
      JIG0MX=0
      JIG1MN=NXI0*NYI0+1
      NNGP=NT+2*NL
      DO 79  JNGP=1,NNGP
        ITI=JNGP
        IF(JNGP.LE.NT)    GO TO 71
        IL1=(JNGP-NT+1)/2
        IL2=(JNGP-NT+2)/2
        IF(IL2.GT.NL)     IL2=1
        ITI=IL1*(NT+NL)+IL2
   71   JWNGP=JWNGP0+JNGP
        NGP0=IWK(JWNGP)
        IF(NGP0.EQ.0)     GO TO 76
        JIG0MN=JIG0MX+1
        JIG0MX=JIG0MX+NGP0
        DO 72  JIGP=JIG0MN,JIG0MX
          JWIGP=JWIGP0+JIGP
          IZI=IWK(JWIGP)
          IYI=(IZI-1)/NXI0+1
          IXI=IZI-NXI0*(IYI-1)
          CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1               ITI,XI(IXI),YI(IYI),ZI(IXI,IYI))
   72   CONTINUE
   76   JWNGP=JWNGP0+2*NNGP+1-JNGP
        NGP1=IWK(JWNGP)
        IF(NGP1.EQ.0)     GO TO 79
        JIG1MX=JIG1MN-1
        JIG1MN=JIG1MN-NGP1
        DO 77  JIGP=JIG1MN,JIG1MX
          JWIGP=JWIGP0+JIGP
          IZI=IWK(JWIGP)
          IYI=(IZI-1)/NXI0+1
          IXI=IZI-NXI0*(IYI-1)
          CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1               ITI,XI(IXI),YI(IYI),ZI(IXI,IYI))
   77   CONTINUE
   79 CONTINUE
      RETURN
      END
      SUBROUTINE  IDTANG(NDP,XD,YD,NT,IPT,NL,IPL,IWL,IWP,WK)
C THIS SUBROUTINE PERFORMS TRIANGULATION.  IT DIVIDES THE X-Y
C PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA
C POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE
C BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS
C CORRESPONDING TO THE BORDER LINE SEGMENTS.
C AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE
C ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS
C OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE,
C LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE.
C THIS SUBROUTINE CALLS THE IDXCHG FUNCTION.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           X COORDINATES OF THE DATA POINTS,
C     YD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           Y COORDINATES OF THE DATA POINTS.
C THE OUTPUT PARAMETERS ARE
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 6*NDP-15, WHERE THE
C           POINT NUMBERS OF THE VERTEXES OF THE (IT)TH
C           TRIANGLE ARE TO BE STORED AS THE (3*IT-2)ND,
C           (3*IT-1)ST, AND (3*IT)TH ELEMENTS,
C           IT=1,2,...,NT,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 6*NDP, WHERE THE
C           POINT NUMBERS OF THE END POINTS OF THE (IL)TH
C           BORDER LINE SEGMENT AND ITS RESPECTIVE TRIANGLE
C           NUMBER ARE TO BE STORED AS THE (3*IL-2)ND,
C           (3*IL-1)ST, AND (3*IL)TH ELEMENTS,
C           IL=1,2,..., NL.
C THE OTHER PARAMETERS ARE
C     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED
C           INTERNALLY AS A WORK AREA,
C     IWP = INTEGER ARRAY OF DIMENSION NDP USED
C           INTERNALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A
C           WORK AREA.
C DECLARATION STATEMENTS
      DIMENSION XD(NDP),     YD(NDP),  IPT(6*NDP - 15), IPL(6*NDP),
     1          IWL(18*NDP), IWP(NDP), WK(NDP)
      DIMENSION   ITF(2)
      DATA  EPSLN/1.0E-6/, NREP/100/
C STATEMENT FUNCTIONS
      DSQF(U1,V1,U2,V2)=(U2-U1)**2+(V2-V1)**2
      SPDT(U1,V1,U2,V2,U3,V3)=(U2-U1)*(U3-U1)+(V2-V1)*(V3-V1)
      VPDT(U1,V1,U2,V2,U3,V3)=(V3-V1)*(U2-U1)-(U3-U1)*(V2-V1)
C PRELIMINARY PROCESSING
   10 NDP0=NDP
      NDPM1=NDP0-1
      IF (NDP0.LT.4) THEN
         CALL ULIBER (47,
     1' IDTANG (BIVAR) - INPUT PARAMETER NDP OUT OF RANGE',50)
         STOP 'ULIBER47'
      ENDIF
C DETERMINES THE CLOSEST PAIR OF DATA POINTS AND THEIR MIDPOINT.
   20 DSQMN=DSQF(XD(1),YD(1),XD(2),YD(2))
      IPMN1=1
      IPMN2=2
      DO 22  IP1=1,NDPM1
        X1=XD(IP1)
        Y1=YD(IP1)
        IP1P1=IP1+1
        DO 21  IP2=IP1P1,NDP0
          DSQI=DSQF(X1,Y1,XD(IP2),YD(IP2))
      IF (DSQI.EQ.0.) THEN
         CALL ULIBER (48,
     1' IDTANG (BIVAR) - TWO OF THE INPUT DATA POINTS ARE IDENTICAL',60)
         STOP 'ULIBER48'
      ENDIF
          IF(DSQI.GE.DSQMN)    GO TO 21
          DSQMN=DSQI
          IPMN1=IP1
          IPMN2=IP2
   21   CONTINUE
   22 CONTINUE
      XDMP=(XD(IPMN1)+XD(IPMN2))/2.0
      YDMP=(YD(IPMN1)+YD(IPMN2))/2.0
C SORTS THE OTHER (NDP-2) DATA POINTS IN ASCENDING ORDER OF
C DISTANCE FROM THE MIDPOINT AND STORES THE SORTED DATA POINT
C NUMBERS IN THE IWP ARRAY.
   30 JP1=2
      DO 31  IP1=1,NDP0
        IF(IP1.EQ.IPMN1.OR.IP1.EQ.IPMN2)      GO TO 31
        JP1=JP1+1
        IWP(JP1)=IP1
        WK(JP1)=DSQF(XDMP,YDMP,XD(IP1),YD(IP1))
   31 CONTINUE
      DO 33  JP1=3,NDPM1
        DSQMN=WK(JP1)
        JPMN=JP1
        DO 32  JP2=JP1,NDP0
          IF(WK(JP2).GE.DSQMN)      GO TO 32
          DSQMN=WK(JP2)
          JPMN=JP2
   32   CONTINUE
        ITS=IWP(JP1)
        IWP(JP1)=IWP(JPMN)
        IWP(JPMN)=ITS
        WK(JPMN)=WK(JP1)
   33 CONTINUE
C IF NECESSARY, MODIFIES THE ORDERING IN SUCH A WAY THAT THE
C FIRST THREE DATA POINTS ARE NOT COLLINEAR.
   35 X1=XD(IPMN1)
      Y1=YD(IPMN1)
      X2=XD(IPMN2)
      Y2=YD(IPMN2)
      DO 36  JP=3,NDP0
        IP=IWP(JP)
        SP=SPDT(XD(IP),YD(IP),X1,Y1,X2,Y2)
        VP=VPDT(XD(IP),YD(IP),X1,Y1,X2,Y2)
        IF(ABS(VP).GT.(ABS(SP)*EPSLN))   GO TO 37
   36 CONTINUE
      CALL ULIBER (49,' IDTANG (BIVAR) - ALL COLLINEAR DATA POINTS',43)
      STOP 'ULIBER49'
   37 IF(JP.EQ.3)    GO TO 40
      JPMX=JP
      DO 38  JPC=4,JPMX
        JP=JPMX+4-JPC
        IWP(JP)=IWP(JP-1)
   38 CONTINUE
      IWP(3)=IP
C FORMS THE FIRST TRIANGLE.  STORES POINT NUMBERS OF THE VER-
C TEXES OF THE TRIANGLE IN THE IPT ARRAY, AND STORES POINT NUM-
C BERS OF THE BORDER LINE SEGMENTS AND THE TRIANGLE NUMBER IN
C THE IPL ARRAY.
   40 IP1=IPMN1
      IP2=IPMN2
      IP3=IWP(3)
      IF(VPDT(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3))
     1     .GE.0.0)       GO TO 41
      IP1=IPMN2
      IP2=IPMN1
   41 NT0=1
      NTT3=3
      IPT(1)=IP1
      IPT(2)=IP2
      IPT(3)=IP3
      NL0=3
      NLT3=9
      IPL(1)=IP1
      IPL(2)=IP2
      IPL(3)=1
      IPL(4)=IP2
      IPL(5)=IP3
      IPL(6)=1
      IPL(7)=IP3
      IPL(8)=IP1
      IPL(9)=1
C ADDS THE REMAINING (NDP-3) DATA POINTS, ONE BY ONE.
   50 DO 79  JP1=4,NDP0
        IP1=IWP(JP1)
        X1=XD(IP1)
        Y1=YD(IP1)
C - DETERMINES THE FIRST INVISIBLE AND VISIBLE BORDER LINE SEG-
C - MENTS, ILIV AND ILVS.
        DO 53  IL=1,NL0
          IP2=IPL(3*IL-2)
          IP3=IPL(3*IL-1)
          X2=XD(IP2)
          Y2=YD(IP2)
          X3=XD(IP3)
          Y3=YD(IP3)
          SP=SPDT(X1,Y1,X2,Y2,X3,Y3)
          VP=VPDT(X1,Y1,X2,Y2,X3,Y3)
          IF(IL.NE.1)     GO TO 51
          IXVS=0
          IF(VP.LE.(ABS(SP)*(-EPSLN)))   IXVS=1
          ILIV=1
          ILVS=1
          GO TO 53
   51     IXVSPV=IXVS
          IF(VP.GT.(ABS(SP)*(-EPSLN)))   GO TO 52
          IXVS=1
          IF(IXVSPV.EQ.1)      GO TO 53
          ILVS=IL
          IF(ILIV.NE.1)        GO TO 54
          GO TO 53
   52     IXVS=0
          IF(IXVSPV.EQ.0)      GO TO 53
          ILIV=IL
          IF(ILVS.NE.1)        GO TO 54
   53   CONTINUE
        IF(ILIV.EQ.1.AND.ILVS.EQ.1)  ILVS=NL0
   54   IF(ILVS.LT.ILIV)  ILVS=ILVS+NL0
C - SHIFTS (ROTATES) THE IPL ARRAY TO HAVE THE INVISIBLE BORDER
C - LINE SEGMENTS CONTAINED IN THE FIRST PART OF THE IPL ARRAY.
   55   IF(ILIV.EQ.1)     GO TO 60
        NLSH=ILIV-1
        NLSHT3=NLSH*3
        DO 56  JL1=1,NLSHT3
          JL2=JL1+NLT3
          IPL(JL2)=IPL(JL1)
   56   CONTINUE
        DO 57  JL1=1,NLT3
          JL2=JL1+NLSHT3
          IPL(JL1)=IPL(JL2)
   57   CONTINUE
        ILVS=ILVS-NLSH
C - ADDS TRIANGLES TO THE IPT ARRAY, UPDATES BORDER LINE
C - SEGMENTS IN THE IPL ARRAY, AND SETS FLAGS FOR THE BORDER
C - LINE SEGMENTS TO BE REEXAMINED IN THE IWL ARRAY.
   60   JWL=0
        DO 64  IL=ILVS,NL0
          ILT3=IL*3
          IPL1=IPL(ILT3-2)
          IPL2=IPL(ILT3-1)
          IT  =IPL(ILT3)
C - - ADDS A TRIANGLE TO THE IPT ARRAY.
          NT0=NT0+1
          NTT3=NTT3+3
          IPT(NTT3-2)=IPL2
          IPT(NTT3-1)=IPL1
          IPT(NTT3)  =IP1
C - - UPDATES BORDER LINE SEGMENTS IN THE IPL ARRAY.
          IF(IL.NE.ILVS)  GO TO 61
          IPL(ILT3-1)=IP1
          IPL(ILT3)  =NT0
   61     IF(IL.NE.NL0)   GO TO 62
          NLN=ILVS+1
          NLNT3=NLN*3
          IPL(NLNT3-2)=IP1
          IPL(NLNT3-1)=IPL(1)
          IPL(NLNT3)  =NT0
C - - DETERMINES THE VERTEX THAT DOES NOT LIE ON THE BORDER
C - - LINE SEGMENTS.
   62     ITT3=IT*3
          IPTI=IPT(ITT3-2)
          IF(IPTI.NE.IPL1.AND.IPTI.NE.IPL2)   GO TO 63
          IPTI=IPT(ITT3-1)
          IF(IPTI.NE.IPL1.AND.IPTI.NE.IPL2)   GO TO 63
          IPTI=IPT(ITT3)
C - - CHECKS IF THE EXCHANGE IS NECESSARY.
   63     IF(IDXCHG(XD,YD,IP1,IPTI,IPL1,IPL2).EQ.0)     GO TO 64
C - - MODIFIES THE IPT ARRAY WHEN NECESSARY.
          IPT(ITT3-2)=IPTI
          IPT(ITT3-1)=IPL1
          IPT(ITT3)  =IP1
          IPT(NTT3-1)=IPTI
          IF(IL.EQ.ILVS)  IPL(ILT3)=IT
          IF(IL.EQ.NL0.AND.IPL(3).EQ.IT)      IPL(3)=NT0
C - - SETS FLAGS IN THE IWL ARRAY.
          JWL=JWL+4
          IWL(JWL-3)=IPL1
          IWL(JWL-2)=IPTI
          IWL(JWL-1)=IPTI
          IWL(JWL)  =IPL2
   64   CONTINUE
        NL0=NLN
        NLT3=NLNT3
        NLF=JWL/2
        IF(NLF.EQ.0)      GO TO 79
C - IMPROVES TRIANGULATION.
   70   NTT3P3=NTT3+3
        DO 78  IREP=1,NREP
          DO 76  ILF=1,NLF
            IPL1=IWL(2*ILF-1)
            IPL2=IWL(2*ILF)
C - - LOCATES IN THE IPT ARRAY TWO TRIANGLES ON BOTH SIDES OF
C - - THE FLAGGED LINE SEGMENT.
            NTF=0
            DO 71  ITT3R=3,NTT3,3
              ITT3=NTT3P3-ITT3R
              IPT1=IPT(ITT3-2)
              IPT2=IPT(ITT3-1)
              IPT3=IPT(ITT3)
              IF(IPL1.NE.IPT1.AND.IPL1.NE.IPT2.AND.
     1           IPL1.NE.IPT3)      GO TO 71
              IF(IPL2.NE.IPT1.AND.IPL2.NE.IPT2.AND.
     1           IPL2.NE.IPT3)      GO TO 71
              NTF=NTF+1
              ITF(NTF)=ITT3/3
              IF(NTF.EQ.2)     GO TO 72
   71       CONTINUE
            IF(NTF.LT.2)       GO TO 76
C - - DETERMINES THE VERTEXES OF THE TRIANGLES THAT DO NOT LIE
C - - ON THE LINE SEGMENT.
   72       IT1T3=ITF(1)*3
            IPTI1=IPT(IT1T3-2)
            IF(IPTI1.NE.IPL1.AND.IPTI1.NE.IPL2)    GO TO 73
            IPTI1=IPT(IT1T3-1)
            IF(IPTI1.NE.IPL1.AND.IPTI1.NE.IPL2)    GO TO 73
            IPTI1=IPT(IT1T3)
   73       IT2T3=ITF(2)*3
            IPTI2=IPT(IT2T3-2)
            IF(IPTI2.NE.IPL1.AND.IPTI2.NE.IPL2)    GO TO 74
            IPTI2=IPT(IT2T3-1)
            IF(IPTI2.NE.IPL1.AND.IPTI2.NE.IPL2)    GO TO 74
            IPTI2=IPT(IT2T3)
C - - CHECKS IF THE EXCHANGE IS NECESSARY.
   74       IF(IDXCHG(XD,YD,IPTI1,IPTI2,IPL1,IPL2).EQ.0)
     1         GO TO 76
C - - MODIFIES THE IPT ARRAY WHEN NECESSARY.
            IPT(IT1T3-2)=IPTI1
            IPT(IT1T3-1)=IPTI2
            IPT(IT1T3)  =IPL1
            IPT(IT2T3-2)=IPTI2
            IPT(IT2T3-1)=IPTI1
            IPT(IT2T3)  =IPL2
C - - SETS NEW FLAGS.
            JWL=JWL+8
            IWL(JWL-7)=IPL1
            IWL(JWL-6)=IPTI1
            IWL(JWL-5)=IPTI1
            IWL(JWL-4)=IPL2
            IWL(JWL-3)=IPL2
            IWL(JWL-2)=IPTI2
            IWL(JWL-1)=IPTI2
            IWL(JWL)  =IPL1
            DO 75  JLT3=3,NLT3,3
              IPLJ1=IPL(JLT3-2)
              IPLJ2=IPL(JLT3-1)
              IF((IPLJ1.EQ.IPL1.AND.IPLJ2.EQ.IPTI2).OR.
     1           (IPLJ2.EQ.IPL1.AND.IPLJ1.EQ.IPTI2))
     2                         IPL(JLT3)=ITF(1)
              IF((IPLJ1.EQ.IPL2.AND.IPLJ2.EQ.IPTI1).OR.
     1           (IPLJ2.EQ.IPL2.AND.IPLJ1.EQ.IPTI1))
     2                         IPL(JLT3)=ITF(2)
   75       CONTINUE
   76     CONTINUE
          NLFC=NLF
          NLF=JWL/2
          IF(NLF.EQ.NLFC)      GO TO 79
C - - RESETS THE IWL ARRAY FOR THE NEXT ROUND.
          JWL1MN=2*NLFC+1
          NLFT2=NLF*2
          DO 77  JWL1=JWL1MN,NLFT2
            JWL=JWL1+1-JWL1MN
            IWL(JWL)=IWL(JWL1)
   77     CONTINUE
          NLF=JWL/2
   78   CONTINUE
   79 CONTINUE
C REARRANGES THE IPT ARRAY SO THAT THE VERTEXES OF EACH TRIANGLE
C ARE LISTED COUNTER-CLOCKWISE.
   80 DO 81  ITT3=3,NTT3,3
        IP1=IPT(ITT3-2)
        IP2=IPT(ITT3-1)
        IP3=IPT(ITT3)
        IF(VPDT(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3))
     1       .GE.0.0)     GO TO 81
        IPT(ITT3-2)=IP2
        IPT(ITT3-1)=IP1
   81 CONTINUE
      NT=NT0
      NL=NL0
      RETURN
      END
      FUNCTION  IDXCHG(X,Y,I1,I2,I3,I4)
C THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO
C TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION
C BY C. L. LAWSON.
C THE INPUT PARAMETERS ARE
C     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA
C           POINTS,
C     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2,
C           P3, AND P4 THAT FORM A QUADRILATERAL WITH P3
C           AND P4 CONNECTED DIAGONALLY.
C THIS FUNCTION RETURNS AN INTEGER VALUE 1 (ONE) WHEN AN EX-
C CHANGE IS NECESSARY, AND 0 (ZERO) OTHERWISE.
C DECLARATION STATEMENTS
      DIMENSION X(1), Y(1)
      EQUIVALENCE (C2SQ,C1SQ),(A3SQ,B2SQ),(B3SQ,A1SQ),
     1            (A4SQ,B1SQ),(B4SQ,A2SQ),(C4SQ,C3SQ)
      DATA  EPSLN/1.0E-6/
C PRELIMINARY PROCESSING
   10 X1=X(I1)
      Y1=Y(I1)
      X2=X(I2)
      Y2=Y(I2)
      X3=X(I3)
      Y3=Y(I3)
      X4=X(I4)
      Y4=Y(I4)
C CALCULATION
   20 IDX=0
      U3=(Y2-Y3)*(X1-X3)-(X2-X3)*(Y1-Y3)
      U4=(Y1-Y4)*(X2-X4)-(X1-X4)*(Y2-Y4)
      IF(U3*U4.LE.0.0)    GO TO 30
      U1=(Y3-Y1)*(X4-X1)-(X3-X1)*(Y4-Y1)
      U2=(Y4-Y2)*(X3-X2)-(X4-X2)*(Y3-Y2)
      A1SQ=(X1-X3)**2+(Y1-Y3)**2
      B1SQ=(X4-X1)**2+(Y4-Y1)**2
      C1SQ=(X3-X4)**2+(Y3-Y4)**2
      A2SQ=(X2-X4)**2+(Y2-Y4)**2
      B2SQ=(X3-X2)**2+(Y3-Y2)**2
      C3SQ=(X2-X1)**2+(Y2-Y1)**2
      S1SQ=U1*U1/(C1SQ*AMAX1(A1SQ,B1SQ))
      S2SQ=U2*U2/(C2SQ*AMAX1(A2SQ,B2SQ))
      S3SQ=U3*U3/(C3SQ*AMAX1(A3SQ,B3SQ))
      S4SQ=U4*U4/(C4SQ*AMAX1(A4SQ,B4SQ))
      IF((AMIN1(S3SQ,S4SQ)-AMIN1(S1SQ,S2SQ)).GT.EPSLN)
     1  IDX=1
   30 IDXCHG=IDX
      RETURN
      END
      SUBROUTINE ULIBER (IERR,MESS,LMESS)
C SUBROUTINE ULIBER (IERR,MESS,LMESS)
C
C PURPOSE                TO PRINT AN ERROR NUMBER AND AN ERROR MESSAGE
C                        OR JUST AN ERROR MESSAGE.
C
C USAGE                  CALL ULIBER (IERR,MESS,LMESS)
C
C ARGUMENTS
C ON INPUT               IERR
C                          THE ERROR NUMBER (PRINTED ONLY IF NON-ZERO).
C
C                        MESS
C                          MESSAGE TO BE PRINTED.
C
C                        LMESS
C                          NUMBER OF CHARACTERS IN MESS (.LE. 130).
C
C ARGUMENTS
C ON OUTPUT              NONE
C
C I/O                    THE MESSAGE IS WRITEN TO UNIT 101.
C ******************************************************************
C
      REAL MESS(1)
C
      IF (IERR.NE.0) WRITE (101,1001) IERR
      NWORDS=(LMESS+7)/8
      WRITE (101,1002) (MESS(I),I=1,NWORDS)
      RETURN
C
 1001 FORMAT (6H0IERR=,I5)
 1002 FORMAT (16A8,A2)
C
      END
