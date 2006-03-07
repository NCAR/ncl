
        PROGRAM MPEX11
C
C The object of this EZMAP example is to show off some capabilities of
C new code in EZMAPB (created in April, 1998).  Three different parts of
C the earth are shown at various "levels"; level 1 includes just land
C and water, level 2 includes continents, level 3 includes countries,
C level 4 includes states within the US, and level 5 includes counties
C within the states.
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
C LAMA is the length of an area map A for geographical boundaries;
C in general, this value needs to be a bit larger than would have been
C required for the old 'CO', 'PO', 'PS', and 'US' datasets.  The
C geographical boundaries go into edge group 1, but we also put into
C area map A a set of lines carving up the frame into vertical strips
C and some circles carving up the frame into areas within each of which
C we wish to display geographic information differently; the vertical
C strips go into edge group 2 and the circles into edge group 5.
C
        PARAMETER (LAMA=2000000)
C
C LAMB is the length of an area map B into which will be put just the
C circles mentioned above.  This area map will be used to determine
C characteristics of the lines drawn by the call to MPLNDM.
C
        PARAMETER (LAMB=  80000)
C
C MCRA is the required length of the scratch arrays to be used by
C ARSCAM for X/Y coordinates.
C
        PARAMETER (MCRA=  40000)
C
C Declare the area map arrays.
C
        DIMENSION IAMA(LAMA),IAMB(LAMB)
C
C Declare the X/Y scratch arrays and area-identifier-information arrays
C to be used by ARSCAM.
C
        DIMENSION XCRA(MCRA),YCRA(MCRA),IAAI(5),IAGI(5)
C
C Declare the names of some area- and line-processing routines EXTERNAL
C to keep the compiler from interpreting them as REAL variables.
C
        EXTERNAL COLORA,COLORL,COLORS
C
C Declare some arrays in which to put values defining some portions of
C the globe to be looked at.
C
        DIMENSION CLON(4),SLAT(4),SLON(4),BLAT(4),BLON(4)
        CHARACTER*64 LABL(4)
C
C Define the portions of the globe to be looked at.
C
        DATA CLON /  -90. ,  10. , 55. , 110. /
C
        DATA SLAT /   -9. ,  34. , 20. , -16. /
        DATA SLON / -145. , -10. , 20. ,  80. /
        DATA BLAT /   70. ,  65. , 60. ,  40. /
        DATA BLON /  -45. ,  40. , 90. , 140. /
C
        DATA LABL /
     +       'North America (Custer County, Nebraska, in Pink)',
     +       'Northern Europe (Slovakia in Pink)              ',
     +       'Eurasia (Uzbekistan in Pink)'                    ,
     +       'Southeast Asia (Cambodia in Pink)'               /
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
C Set the GKS "fill area interior style" to "solid".
C
        CALL GSFAIS (1)
C
C Define some color indices.  (DFCLRS is part of this example; it is
C not an NCAR Graphics library routine.)
C
        CALL DFCLRS (IWKID)
C
C Select PLOTCHAR font number 25, turn on the outlining of filled fonts,
C and turn off the setting of the outline color.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
        CALL PCSETR ('OC - OUTLINE LINE COLOR',-1.)
C
C Turn on "vertical stripping" by EZMAP.  What this means is that a call
C to MPLNAM will put into an area map not only the geographical boundary
C lines (in edge group 1), but also lines (in edge group 2) defining
C some vertical strips (4, in this case).  This helps to break up the
C areas defined by the geographical boundaries in such a way as to
C reduce the total number of points required for any particular area.
C
        CALL MPSETI ('VS',4)
C
C Loop to depict three different portions of the globe.
C
        DO 105 IVEW=1,4
C
C Tell EZMAP to use a Mercator projection.  CLON(IVEW) is the center
C longitude.
C
          CALL MAPROJ ('ME',0.,CLON(IVEW),0.)
C
C Tell EZMAP to set up the map limits in such a way as to include all
C of a region from some smallest latitude and longitude to some biggest
C latitude and longitude.
C
          CALL MAPSET ('GR',SLAT(IVEW),SLON(IVEW),BLAT(IVEW),BLON(IVEW))
C
C Initialize EZMAP.
C
          CALL MAPINT
C
C Find out what SET call was done by EZMAP, reset the map limits in
C such a way as to make the map square, and then reinitialize.
C
          CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
          XCEN=.5*(XWDL+XWDR)
          YCEN=.5*(YWDB+YWDT)
          HWTH=.5*MAX(XWDR-XWDL,YWDT-YWDB)
          CALL MAPSET ('LI',XCEN-HWTH,XCEN+HWTH,
     +                      YCEN-HWTH,YCEN+HWTH)
          CALL MAPINT
C
C Initialize both of area maps A and B.
C
          CALL ARINAM (IAMA,LAMA)
          CALL ARINAM (IAMB,LAMB)
C
C Put four concentric circles into both of area maps A and B (in edge
C group 5).  The interior of the smallest circle has area identifier 5,
C the ring around it has area identifier 4, the ring around that has
C area identifier 3, the ring around that has area identifier 2, and
C the remainder has area identifier 1.  These area identifiers will be
C used to determine the "level" at which the geographical information
C is to be displayed; level 5 means "counties", level 4 means "states",
C level 3 means "countries", level 2 means "continents", and level 1
C means just "land/water".
C
          DO 102 ICIR=1,4
            DO 101 IANG=1,361
              ANGL=.017453292519943*REAL(MOD(IANG-1,360))
              XCRA(IANG)=CFUX(.5+(.05+.13*REAL(ICIR-1))*COS(ANGL))
              YCRA(IANG)=CFUY(.5+(.05+.13*REAL(ICIR-1))*SIN(ANGL))
  101       CONTINUE
            CALL AREDAM (IAMA,XCRA,YCRA,361,5,6-ICIR,5-ICIR)
            CALL AREDAM (IAMB,XCRA,YCRA,361,5,6-ICIR,5-ICIR)
  102     CONTINUE
C
C Put all the EZMAP boundary lines from the named dataset (down to
C level 5) into area map A.
C
          CALL MPLNAM ('Earth..2',5,IAMA)
C
C Color the map as implied by the contents of area map A.  See the
C "user callback" routine COLORA (elsewhere in this file) to see how
C the area identifiers from groups 1 (geographic), 2 (vertical strips),
C and 5 (circles) are all used to produce the desired effect.
C
          CALL ARSCAM (IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,5,COLORA)
C
C Print some numbers reflecting the amount of space actually used in
C the area map arrays.
C
          PRINT * , ' '
          PRINT * , 'Length of area map A: ',LAMA-IAMA(6)+IAMA(5)+1
          PRINT * , 'Length of area map B: ',LAMB-IAMB(6)+IAMB(5)+1
C
C Draw the EZMAP boundary lines masked by area map B; the lines are
C drawn differently inside each of the areas created by the concentric
C circles.
C
          CALL MPLNDM ('Earth..2',5,IAMB,XCRA,YCRA,MCRA,
     +                                         IAAI,IAGI,5,COLORL)
C
C Draw the concentric circles themselves.
C
          CALL GSLWSC (2.)
          CALL GSPLCI (2)
C
          DO 104 ICIR=1,4
            DO 103 IANG=1,361
              ANGL=.017453292519943*REAL(MOD(IANG-1,360))
              XCRA(IANG)=CFUX(.5+(.05+.13*REAL(ICIR-1))*COS(ANGL))
              YCRA(IANG)=CFUY(.5+(.05+.13*REAL(ICIR-1))*SIN(ANGL))
  103       CONTINUE
            CALL CURVE (XCRA,YCRA,361)
  104     CONTINUE
C
C Put some labels on the plot.
C
          CALL GSLWSC (1.)
          CALL GSPLCI (2)
          CALL GSFACI (2)
C
          CALL PLCHHQ (CFUX(.500),CFUY(.975),
     +                      'The Database "Earth..2" at Various Levels',
     +                                                       .018,0.,0.)
C
          CALL PLCHHQ (CFUX(.500),CFUY(.025),
     +                                 LABL(IVEW)(1:MPILNB(LABL(IVEW))),
     +                                                       .018,0.,0.)
C
          CALL PLCHHQ (CFUX(.142911),CFUY(.142911),'1',.02,0.,0.)
          CALL PLCHHQ (CFUX(.234835),CFUY(.234835),'2',.02,0.,0.)
          CALL PLCHHQ (CFUX(.326759),CFUY(.326759),'3',.02,0.,0.)
          CALL PLCHHQ (CFUX(.418683),CFUY(.418683),'4',.02,0.,0.)
          CALL PLCHHQ (CFUX(.500000),CFUY(.500000),'5',.02,0.,0.)
C
          CALL GSPLCI (1)
          CALL GSFACI (1)
C
C Advance the frame.
C
          CALL FRAME
C
C End of view loop.
C
  105   CONTINUE
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



      SUBROUTINE DFCLRS (IWKS)
C
C Define some color indices for use in the example "mpex13" (0 = black,
C the background color; 1 = white, the foreground color; 2 = yellow,
C for some labels; 11 = white, for water/land edges; 12 = gray, for
C edges of continents; 13 = gray, for edges of countries; 14 = gray,
C for edges of states; 15 = gray, for edges of counties; 16 = pink,
C for highlighted areas; 101-107 = area colors 1-7).
C
        CALL GSCR (IWKS,  0,0.,0.,0.)
        CALL GSCR (IWKS,  1,1.,1.,1.)
        CALL GSCR (IWKS,  2,1.,1.,0.)
        CALL GSCR (IWKS, 11,1.,1.,1.)
        CALL GSCR (IWKS, 12,1.,1.,1.)
        CALL GSCR (IWKS, 13,1.,1.,1.)
        CALL GSCR (IWKS, 14,1.,1.,1.)
        CALL GSCR (IWKS, 15,.1,.1,.1)
        CALL GSCR (IWKS, 16,1.,.6,.6)
        CALL GSCR (IWKS,101,.2,.2,.8)
        CALL GSCR (IWKS,102,.2,.4,.6)
        CALL GSCR (IWKS,103,.2,.6,.4)
        CALL GSCR (IWKS,104,.2,.8,.2)
        CALL GSCR (IWKS,105,.4,.6,.2)
        CALL GSCR (IWKS,106,.6,.4,.2)
        CALL GSCR (IWKS,107,.6,.6,.6)
C
        RETURN
C
      END



      SUBROUTINE MPCHLN (IFLG,ILTY,IOAL,IOAR,NPTS,PNTS)
C
        DIMENSION PNTS(*)
C
C This version of the "user callback" routine MPCHLN determines some
C characteristics of lines of different types drawn by calls to MPLNDM
C and MPLNDR, as follows:
C
C     Level 1 (land/water boundaries): double thickness, color 11
C     Level 2 (continental boundaries): double thickness, color 12
C     Level 3 (country boundaries): double thickness, color 13
C     Level 4 (state boundaries): single thickness, color 14
C     Level 5 (county boundaries): single thickness, color 15
C
C Flush SPPS pen-move buffers.
C
        CALL PLOTIF (0.,0.,2)
C
C If IFLG is greater than one, a line of type ILTY is about to be
C drawn; set up the desired characteristics.
C
        IF (IFLG.GT.1) THEN
C
          IF      (ILTY.EQ.1) THEN
            CALL GSLWSC (2.)
            CALL GSPLCI (11)
          ELSE IF (ILTY.EQ.2) THEN
            CALL GSLWSC (2.)
            CALL GSPLCI (12)
          ELSE IF (ILTY.EQ.3) THEN
            CALL GSLWSC (2.)
            CALL GSPLCI (13)
          ELSE IF (ILTY.EQ.4) THEN
            CALL GSLWSC (1.)
            CALL GSPLCI (14)
          ELSE IF (ILTY.EQ.5) THEN
            CALL GSLWSC (1.)
            CALL GSPLCI (15)
          END IF
C
C If, on the other hand, IFLG is less than minus one, a line was just
C drawn; reset line characteristics to default values.
C
        ELSE IF (IFLG.LT.-1) THEN
C
          CALL GSLWSC (1.)
          CALL GSPLCI (1)
C
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE COLORA (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C In the example "mpex13", the routine COLORA is called by the AREAS
C routine ARSCAM to fill the areas created by area map A.
C
        CHARACTER*128 MPFNME
C
C Extract the area identifiers of the area relative to groups 1
C (geographic), 2 (vertical stripping), and 5 (concentric circles).
C
        IAI1=-1
        IAI2=-1
        IAI5=-1
C
        DO 101 I=1,NGPS
          IF (IAGI(I).EQ.1) IAI1=IAAI(I)
          IF (IAGI(I).EQ.2) IAI2=IAAI(I)
          IF (IAGI(I).EQ.5) IAI5=IAAI(I)
  101   CONTINUE
C
C If all the area identifiers have valid values, choose a color for the
C area and fill it.  If the full name of it is "Nebraska - Custer",
C "Slovakia", "Uzbekistan", or "Cambodia", it is filled using color 16
C (pink); otherwise, we use the suggested color for the area at the
C level implied by the group-5 area identifier.
C
        IF (IAI1.GE.1) THEN
          IF (IAI2.GE.0) THEN
            IF (IAI5.GE.1) THEN
              IF (MPFNME(IAI1,4).EQ.'Nebraska - Custer'.OR.
     +            MPFNME(IAI1,4).EQ.'Slovakia'.OR.
     +            MPFNME(IAI1,4).EQ.'Uzbekistan'.OR.
     +            MPFNME(IAI1,4).EQ.'Cambodia') THEN
                CALL GSFACI (16)
              ELSE
                CALL GSFACI (100+MPISCI(MPIOSA(IAI1,IAI5)))
              END IF
              CALL GFA (NCRA-1,XCRA,YCRA)
            END IF
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE COLORS (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C In the example "mpex11", the routine COLORS is used to color the areas
C on the zoom-in frames.  Only group-1 and group-2 area identifiers are
C examined and the colors suggested by the contents of the EZMAP dataset
C are used.
C
        IAI1=-1
        IAI2=-1
C
        DO 101 I=1,NGPS
          IF (IAGI(I).EQ.1) IAI1=IAAI(I)
          IF (IAGI(I).EQ.2) IAI2=IAAI(I)
  101   CONTINUE
C
        IF (IAI1.GE.1) THEN
          IF (IAI2.GE.0) THEN
            CALL GSFACI (100+MPISCI(IAI1))
            CALL GFA (NCRA-1,XCRA,YCRA)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE COLORL (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C In the example "mpex13", the routine COLORL is called by the EZMAPB
C routine MPLNDM to draw lines masked by the contents of area map B.
C
C Get the value of the line type for the line being drawn.
C
        CALL MPGLTY (ILTY)
C
C Find the area identifier relative to group 5 (the circles).
C
        IAI5=-1
C
        DO 101 I=1,NGPS
          IF (IAGI(I).EQ.5) IAI5=IAAI(I)
  101   CONTINUE
C
C If the group-5 area identifier is valid, draw the line if and only
C if its type is less than or equal to the group-5 area identifier.
C What this means is that all boundary lines (down to the county level)
C are drawn in the inner circle, county lines are omitted in the ring
C surrounding that, state lines are omitted in the ring surrounding
C that, country lines are omitted in the ring surrounding that, and
C continental boundary lines are omitted elsewhere.
C
        IF (IAI5.GE.1) THEN
          IF (ILTY.LE.IAI5) THEN
            CALL GPL (NCRA,XCRA,YCRA)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
