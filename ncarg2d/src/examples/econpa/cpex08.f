
        PROGRAM CPEX08
C
C This example shows a color-filled contour plot on the surface of the
C earth, in orthographic projection, with a color bar to show what the
C colors mean.  It is assumed that the field being contoured has meaning
C only over the African continent and, accordingly, contours are only
C shown there.  Lines of latitude and longitude are drawn only over the
C ocean areas.
C
C Note: As of 10/17/2005, this example has been modified to produce two
C frames.  The first makes use of the old EZMAP database 'CO' and the
C second makes use of the newer EZMAP database 'Earth..2'.
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
C Declare a dummy data array.
C
        DIMENSION ZDAT(40,40)
C
C Declare workspace arrays for CONPACK.
C
        PARAMETER (LRWK=1000,LIWK=1000)
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C Declare arrays needed to do color fill.
C
        PARAMETER (LAMA=1000000,NCRA=LAMA/10,NGPS=5)
C
        DIMENSION IAMA(LAMA)
        DIMENSION XCRA(NCRA),YCRA(NCRA)
        DIMENSION IAIA(NGPS),IGIA(NGPS)
C
C Declare arrays to hold the list of indices and the list of labels
C required by the label-bar routine.
C
        DIMENSION LIND(14)
        CHARACTER*10 LLBS(14)
C
C Declare routines to color the areas represented by the area map for
C frames 1 and 2.
C
        EXTERNAL COLRAM,COLRA2
C
C Declare routines to draw contour lines over land only for frames 1
C and 2.
C
        EXTERNAL COLRCL,COLRC2
C
C Declare routines to draw lat/lon lines over ocean only for frames 1
C and 2.
C
        EXTERNAL COLRLL,COLRL2
C
C Define the list of indices required by the label-bar routine.
C
        DATA LIND / 7,2,3,4,5,6,8,9,10,11,12,13,14,15 /
C
C Define the list of labels required by the label-bar routine.
C
        DATA LLBS / 'OCEAN  ' , 'LAND   ' , '< 0    ' , '0-10   '  ,
     +              '10-20  ' , '20-30  ' , '30-40  ' , '40-50  '  ,
     +              '50-60  ' , '60-70  ' , '70-80  ' , '80-90  '  ,
     +              '90-100 ' , '> 100  ' /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Define color indices.
C
        CALL DFCLRS(IWKID)
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,40,40,40,15,15,-10.,110.)
C
C   F R A M E   1   -   O L D   E Z M A P   D A T A B A S E
C   - - - - -   -   -   - - -   - - - - -   - - - - - - - -
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Use EZMAP and EZMAPA to create a background.
C
        CALL MAPPOS (.01,.74,.01,.99)
        CALL MAPROJ ('OR - ORTHOGRAPHIC PROJECTION',15.,15.,0.)
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
        CALL MAPSTI ('EL - ELLIPTICAL BOUNDARY',1)
        CALL MAPSTC ('OU - OUTLINE DATASET','CO')
        CALL MAPSTI ('VS - VERTICAL STRIPPING',0)
        CALL MAPINT
        CALL MAPBLA (IAMA)
C
C Add a line segment to the area map to separate Africa from Eurasia.
C Because this line segment is added last, it will determine the area
C identifier for all of Africa (223).  Also, add a line cutting
C Madagascar in two, with the same area identifier on both sides, so
C that it will be treated as a part of Africa.
C
        CALL MAPITA (26.0,35.0,0,IAMA,1,223,0)
        CALL MAPITA (28.8,33.0,1,IAMA,1,223,0)
        CALL MAPITA (33.0,30.0,1,IAMA,1,223,0)
        CALL MAPIQA (IAMA,1,223,0)
C
        CALL MAPITA (-20.0,42.5,0,IAMA,1,223,223)
        CALL MAPITA (-20.0,50.0,1,IAMA,1,223,223)
        CALL MAPIQA (IAMA,1,223,223)
C
C Tell CONPACK not to do the SET call (since it's already been done),
C to use mapping function 1 (EZMAP background), and what range of X and
C Y coordinates to send into the mapping function.  The X coordinates
C will be treated as latitudes and will range from 40 degrees west of
C Greenwich to 55 degrees east of Greenwich, and the Y coordinates will
C be treated as latitudes and will range from 45 degrees south of the
C equator to 45 degrees north of the equator.
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('XC1 - X COORDINATE AT I=1',-18.)
        CALL CPSETR ('XCM - X COORDINATE AT I=M',+52.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J=1',-35.)
        CALL CPSETR ('YCN - Y COORDINATE AT J=N',+38.)
C
C Tell CONPACK exactly what contour levels to use.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',1)
        CALL CPSETR ('CMN - CONTOUR LEVEL MINIMUM',0.)
        CALL CPSETR ('CMX - CONTOUR LEVEL MAXIMUM',100.)
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',10.)
C
C Tell CONPACK what value EZMAP uses to signal that a projected point
C has disappeared around the limb.  Strictly speaking, this call is
C not necessary here; it has been inserted for the benefit of users
C who modify the example to use global data.
C
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,40,40,40,RWRK,LRWK,IWRK,LIWK)
C
C Add contour lines to the area map.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Color the map.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAIA,IGIA,NGPS,COLRAM)
C
C Outline the continents in black, put black contour lines over the
C color map, and put gray lines of latitude and longitude over the
C ocean.
C
        CALL GSPLCI (0)
        CALL MAPLOT
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,COLRCL)
        CALL GSPLCI (2)
        CALL MAPGRM (IAMA,XCRA,YCRA,NCRA,IAIA,IGIA,NGPS,COLRLL)
        CALL GSPLCI (1)
C
C Draw a label bar for the plot, relating colors to values.
C
        CALL LBSETI ('CBL - COLOR OF BOX LINES',0)
        CALL LBLBAR (1,.76,.99,.13,.87,14,.5,1.,LIND,0,LLBS,14,1)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line at the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 8',IAMA,LAMA)
        CALL LABTOP ('EXAMPLE 8',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C   F R A M E   2   -   N E W   E Z M A P   D A T A B A S E
C   - - - - -   -   -   - - -   - - - - -   - - - - - - - -
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Use EZMAP and EZMAPB to create a background.  Note that I am using
C the database 'Earth..2' at level 3.  This is to make "Madagascar" a
C recognized area.  If I did not wish to show contours over Madagascar,
C I could use the database at level 2, saving a little space in the
C area map and in the metafile.
C
        CALL MAPPOS (.01,.74,.01,.99)
        CALL MAPROJ ('OR - ORTHOGRAPHIC PROJECTION',15.,15.,0.)
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
        CALL MAPSTI ('EL - ELLIPTICAL BOUNDARY',1)
        CALL MAPSTI ('VS - VERTICAL STRIPPING',0)
        CALL MAPINT
        CALL MDLNAM ('Earth..2',3,IAMA)
C
C Tell CONPACK not to do the SET call (since it's already been done),
C to use mapping function 1 (EZMAP background), and what range of X and
C Y coordinates to send into the mapping function.  The X coordinates
C will be treated as latitudes and will range from 40 degrees west of
C Greenwich to 55 degrees east of Greenwich, and the Y coordinates will
C be treated as latitudes and will range from 45 degrees south of the
C equator to 45 degrees north of the equator.
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('XC1 - X COORDINATE AT I=1',-18.)
        CALL CPSETR ('XCM - X COORDINATE AT I=M',+52.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J=1',-35.)
        CALL CPSETR ('YCN - Y COORDINATE AT J=N',+38.)
C
C Tell CONPACK exactly what contour levels to use.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',1)
        CALL CPSETR ('CMN - CONTOUR LEVEL MINIMUM',0.)
        CALL CPSETR ('CMX - CONTOUR LEVEL MAXIMUM',100.)
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',10.)
C
C Tell CONPACK what value EZMAP uses to signal that a projected point
C has disappeared around the limb.  Strictly speaking, this call is
C not necessary here; it has been inserted for the benefit of users
C who modify the example to use global data.
C
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,40,40,40,RWRK,LRWK,IWRK,LIWK)
C
C Add contour lines to the area map.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Color the map.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAIA,IGIA,NGPS,COLRA2)
C
C Outline the continents in black, put black contour lines over the
C color map, and put gray lines of latitude and longitude over the
C ocean.
C
        CALL GSPLCI (0)
        CALL MDLNDR ('Earth..2',2)
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,COLRC2)
        CALL GSPLCI (2)
        CALL MAPGRM (IAMA,XCRA,YCRA,NCRA,IAIA,IGIA,NGPS,COLRL2)
        CALL GSPLCI (1)
C
C Draw a label bar for the plot, relating colors to values.
C
        CALL LBSETI ('CBL - COLOR OF BOX LINES',0)
        CALL LBLBAR (1,.76,.99,.13,.87,14,.5,1.,LIND,0,LLBS,14,1)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line at the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 8',IAMA,LAMA)
        CALL LABTOP ('EXAMPLE 8',.017)
        CALL BNDARY
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


      SUBROUTINE COLRAM (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*),IOCI(12)
C
C This routine is called to color an area from an area map.  Its
C coordinates are given by the NCRA coordinates in the arrays XCRA and
C YCRA.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
C the area relative to the group whose group identifier is IGIA(I).
C
C Define an array of color indices associated with area identifiers.
C
        DATA IOCI / 3,4,5,6,8,9,10,11,12,13,14,15 /
C
C Find the area identifier for the area relative to groups 1 and 3.
C The first of these tells us whether the area is over land or water,
C and the second tells us what contour band the area is in.
C
        IAI1=-1
        IAI3=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1) IAI1=IAIA(I)
          IF (IGIA(I).EQ.3) IAI3=IAIA(I)
  101   CONTINUE
C
C Color-fill the area, using blue for any area over water, gray for
C any area over land which is not over Africa (including Madagascar)
C or is outside the contour plot, and a color depending on the contour
C level elsewhere.
C
        IF (IAI1.GT.0) THEN
          IF (MAPACI(IAI1).EQ.1) THEN
            CALL GSFACI (7)
            CALL GFA (NCRA-1,XCRA,YCRA)
          ELSE
            IF (IAI1.NE.223.OR.IAI3.LE.0) THEN
              CALL GSFACI (2)
              CALL GFA (NCRA-1,XCRA,YCRA)
            ELSE
              CALL GSFACI (IOCI(IAI3))
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


      SUBROUTINE COLRCL (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C This routine is called to draw a portion of a contour line which is
C wholly contained in some area of an area map.  Its coordinates are
C given by the NCRA coordinates in the arrays XCRA and YCRA.  For each
C I from 1 to NAGI, IAIA(I) is the area identifier of the area relative
C to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to groups 1 and 3.
C The first of these tells us whether the area is over land or water,
C and the second tells us what contour band the area is in.
C
        IAI1=-1
        IAI3=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1) IAI1=IAIA(I)
          IF (IGIA(I).EQ.3) IAI3=IAIA(I)
  101   CONTINUE
C
C Draw the line only if the area it is in is over Africa (including
C Madagascar) and within the boundary of the contour plot.
C
        IF (IAI1.EQ.223.AND.IAI3.GT.0) CALL GPL (NCRA,XCRA,YCRA)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE COLRLL (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C This routine is called to draw a portion of a line of latitude or
C longitude which is wholly contained in some area of an area map.  Its
C coordinates are given by the NCRA coordinates in the arrays XCRA and
C YCRA.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
C the area relative to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to group 1, which will
C tell us whether the area is over land or water.
C
        IAI1=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1.AND.IAIA(I).GT.0) IAI1=IAIA(I)
  101   CONTINUE
C
C Draw the line only if it is over water.
C
        IF (IAI1.GT.0.AND.MAPACI(IAI1).EQ.1) CALL GPL (NCRA,XCRA,YCRA)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE COLRA2 (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*),IOCI(12)
C
C This routine is called to color an area from an area map.  Its
C coordinates are given by the NCRA coordinates in the arrays XCRA and
C YCRA.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
C the area relative to the group whose group identifier is IGIA(I).
C
C Define an array of color indices associated with area identifiers.
C
        DATA IOCI / 3,4,5,6,8,9,10,11,12,13,14,15 /
C
C Find the area identifier for the area relative to groups 1 and 3.
C The first of these tells us whether the area is over land or water,
C and the second tells us what contour band the area is in.
C
        IAI1=-1
        IAI3=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1) IAI1=IAIA(I)
          IF (IGIA(I).EQ.3) IAI3=IAIA(I)
  101   CONTINUE
C
C Color-fill the area, using blue for any area over water, gray for any
C area over land which is not over Africa (including Madagascar) or is
C outside the contour plot, and a color depending on the contour level
C elsewhere.
C
        IF (IAI1.GT.0) THEN
          IF (MDIPAN(IAI1,'Water').NE.0) THEN
            CALL GSFACI (7)
            CALL GFA (NCRA-1,XCRA,YCRA)
          ELSE
            IF ((MDIPAN(IAI1,'Africa').EQ.0.AND.
     +           MDIPAN(IAI1,'Madagascar').EQ.0).OR.IAI3.LE.0) THEN
              CALL GSFACI (2)
              CALL GFA (NCRA-1,XCRA,YCRA)
            ELSE
              CALL GSFACI (IOCI(IAI3))
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


      SUBROUTINE COLRC2 (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C This routine is called to draw a portion of a contour line which is
C wholly contained in some area of an area map.  Its coordinates are
C given by the NCRA coordinates in the arrays XCRA and YCRA.  For each
C I from 1 to NAGI, IAIA(I) is the area identifier of the area relative
C to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to groups 1 and 3.
C The first of these tells us whether the area is over land or water,
C and the second tells us what contour band the area is in.
C
        IAI1=-1
        IAI3=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1) IAI1=IAIA(I)
          IF (IGIA(I).EQ.3) IAI3=IAIA(I)
  101   CONTINUE
C
C Draw the line only if the area it is in is over Africa and within
C the boundary of the contour plot.
C
        IF (IAI1.GT.0.AND.
     +      (MDIPAN(IAI1,'Africa').NE.0.OR.
     +       MDIPAN(IAI1,'Madagascar').NE.0).AND.IAI3.GT.0) THEN
          CALL GPL (NCRA,XCRA,YCRA)
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE COLRL2 (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C This routine is called to draw a portion of a line of latitude or
C longitude which is wholly contained in some area of an area map.  Its
C coordinates are given by the NCRA coordinates in the arrays XCRA and
C YCRA.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
C the area relative to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to group 1, which will
C tell us whether the area is over land or water.
C
        IAI1=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1.AND.IAIA(I).GT.0) IAI1=IAIA(I)
  101   CONTINUE
C
C Draw the line only if it is over water.
C
        IF (IAI1.GT.0.AND.MDIPAN(IAI1,'Water').NE.0) THEN
          CALL GPL (NCRA,XCRA,YCRA)
        END IF
C
C Done.
C
        RETURN
C
      END
