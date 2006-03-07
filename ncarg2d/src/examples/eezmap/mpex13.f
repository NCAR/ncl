
        PROGRAM MPEX13
C
C This program shows how to use EZMAP routines providing access to
C the RANGS database ("Regionally Accessible Nested Global Shorelines")
C created by Reiner Feistel, which make use of the GSHHS database
C ("Global Self-consistent Heirarchical High-resolution Shorelines")
C created by Paul Wessel and Walter H. F. Smith.  The original data were
C from the World Vector Shoreline and World Databank II databases.
C
C It also demonstrates the use of routines for labelling latitudes or
C longitudes along a specified line on a map.
C
C Note that, as it stands, this program will fail to produce any plots
C using the RANGS/GSHHS data, because that data will not be available
C to it.  The user must add a routine called MDRGDI, which looks like
C this:
C
C     SUBROUTINE MDRGDI (DINM)
C
C This is a user-replaceable routine that returns the name of the
C directory in which the RANGS/GSHHS data files have been placed.
C
C       CHARACTER*(*) DINM
C
C Return the name of the directory where the RANGS/GSHHS data reside.
C
C       DINM='/fs/scd/home1/kennison/Mapping/RANGSData'
C
C Done.
C
C       RETURN
C
C     END
C
C The specified directory must contain the following files:
C
C   gshhs(0).rim 89496428 bytes
C   gshhs(1).rim 20775224 bytes
C   gshhs(2).rim  5099196 bytes
C   gshhs(3).rim  1109192 bytes
C   gshhs(4).rim   170948 bytes
C   rangs(0).cat   259200 bytes
C   rangs(0).cel  6518664 bytes
C   rangs(1).cat   259200 bytes
C   rangs(1).cel  5945492 bytes
C   rangs(2).cat   259200 bytes
C   rangs(2).cel  4062215 bytes
C   rangs(3).cat   259200 bytes
C   rangs(3).cel  3398642 bytes
C   rangs(4).cat   259200 bytes
C   rangs(4).cel  3045848 bytes
C
C These files may be obtained from Rainer Feistel's Web site:
C
C   http://www.io-warnemuende.de/homepages/rfeistel/index.html
C
C A description of the files is available here:
C
C   http://www.agu.org/eos_elec/99063e.html
C
C Hopefully, these files will also be available from the NCAR Graphics
C Web site.
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
C Declare arrays to hold center-point positions, angular half-widths,
C grid spacings, resolutions, and titles for four different maps to
C be drawn.  For each of the four, the map is drawn three different
C times, once using the old 'CO' database, once using the database
C "Earth..2", and again using the RANGS/GSHHS database.
C
        DOUBLE PRECISION PLAT(4),PLON(4),ANGL(4),GRSP(4)
        INTEGER IRGL(4)
        CHARACTER*16 TITL(4)
C
C Declare arrays to hold color indices to be passed to MDRGSC, defining
C the colors to be used on maps drawn from the RANGS/GSHHS database
C
        DIMENSION ICOL(5),ICSF(5)
C
C Declare a character-size variable.
C
        REAL CHSZ
C
C Declare stuff required to do color fill.
C
        PARAMETER (LAMA=200000,NCRA=10000,NGPS=10,LRWK=2*NCRA)
C
        DIMENSION IAMA(LAMA),XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
        DIMENSION RWRK(LRWK)
        EQUIVALENCE (RWRK(1),XCRA(1)),(RWRK(NCRA+1),YCRA(1))
C
        EXTERNAL COLRCO,COLRE2
C
C Define the center-point positions and angular half-width for each of
C the four plots.
C
        DATA PLAT(1),PLON(1),ANGL(1) /  23.0D0 , -76.0D0 , 5.0D0 /
        DATA PLAT(2),PLON(2),ANGL(2) / -53.5D0 , -70.0D0 , 3.0D0 /
        DATA PLAT(3),PLON(3),ANGL(3) /  54.5D0 ,  -2.0D0 , 4.5D0 /
        DATA PLAT(4),PLON(4),ANGL(4) /  37.0D0 , 126.5D0 , 4.0D0 /
C
C Define the resolution of the grid and the level of the RANGS data
C to be used on each of the four plots.
C
        DATA GRSP(1),IRGL(1) / 1.D0 , 0 /
        DATA GRSP(2),IRGL(2) / 1.D0 , 0 /
        DATA GRSP(3),IRGL(3) / 1.D0 , 0 /
        DATA GRSP(4),IRGL(4) / 1.D0 , 0 /
C
C Define titles for the tops of the plots, describing the place mapped.
C
        DATA TITL(1) / 'Caribbean' /
        DATA TITL(2) / 'Tierra del Fuego' /
        DATA TITL(3) / 'Great Britain' /
        DATA TITL(4) / 'Korea' /
C
C Define the character size to be used for lat/lon labels.
C
        DATA CHSZ / .014 /
C
C Tell the user what he needs to do to get this to run.
C
        PRINT * , ' '
        PRINT * , 'This example will not run properly until you have'
        PRINT * , 'downloaded the RANGS/GSHHS data, supplied a routine'
        PRINT * , 'for EZMAP to call to find out which directory the'
        PRINT * , 'data are in, and removed these PRINTs and the STOP'
        PRINT * , 'which follows them.  See code for full information.'
        PRINT * , ' '
        STOP
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off clipping by GKS (so labels don't get clipped).
C
        CALL GSCLIP (0)
C
C Define some colors to use.  0 and 1 are black and white, the default
C background and foreground colors.  2 through 6 are used for coloring
C RANGS/GSHHS areas "ocean", "land", "lake", "island in lake", and
C "pond on island in lake", respectively.  7 is for debug use.
C
        CALL GSCR (1,0,0.,0.,0.)
        CALL GSCR (1,1,1.,1.,1.)
C
        CALL GSCR (1,2,0.,0.,1.)
        CALL GSCR (1,3,0.,1.,0.)
        CALL GSCR (1,4,0.,1.,1.)
        CALL GSCR (1,5,0.,1.,0.)
        CALL GSCR (1,6,0.,1.,1.)
C
        CALL GSCR (1,7,1.,0.,0.)
C
C Tell EZMAP to draw the map in a bit smaller square, leaving room for
C lat/lon labels around the edge.
C
        CALL MAPPOS (.08,.92,.08,.92)
C
C Tell EZMAP what colors to use for the outlines and filled areas
C produced from the RANGS/GSHHS data.
C
        ICOL(1)=7
        ICOL(2)=7
        ICOL(3)=7
        ICOL(4)=7
        ICOL(5)=7
C
        ICSF(1)=2
        ICSF(2)=3
        ICSF(3)=4
        ICSF(4)=5
        ICSF(5)=6
C
        CALL MDRGSC (ICOL,ICSF)
C
C Increase the number of points to be interpolated along the edges of
C the 1-degree cells (to get rid of some edge effects that otherwise
C occur).
C
        CALL MPSETI ('II',64064)
C       CALL MPSETD ('GD',.1D0)
C
C Set the PLOTCHAR font, character color, and outlining.
C
        CALL PCSETI ('FN',26)
        CALL PCSETI ('CC', 1)
        CALL PCSETI ('OF', 1)
        CALL PCSETI ('OC', 1)
C
C Loop to do views of four different spots on the globe.
C
        DO 101 ISPT=1,4
C
C Print the loop index, so the user will know something is happening.
C (This program runs pretty slowly.)
C
          PRINT * , ' '
          PRINT * , 'POSITION NUMBER ',ISPT,'    ',TITL(ISPT)
C
C Define a satellite altitude (for use if 'SV' is selected below).
C
          CALL MAPSTD ('SA',2.5D0)
C
C Define the projection to be used.  (If only a small portion of the
C projection plane is being shown, it doesn't make too much difference
C which projection is chosen.)
C
C         CALL MDPROJ ('ST',PLAT(ISPT),PLON(ISPT),0.D0)
          CALL MDPROJ ('OR',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPROJ ('CE',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPROJ ('MO',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPROJ ('ME',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPROJ ('RO',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPROJ ('AE',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPROJ ('LE',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPROJ ('GN',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPROJ ('SV',PLAT(ISPT),PLON(ISPT),0.D0)
C
C Define the limits of the map.
C
          CALL MDPSET ('AN',ANGL(ISPT),ANGL(ISPT),ANGL(ISPT),ANGL(ISPT))
C
C Here's an interesting combination:
C
C         CALL MAPSTD ('SA',1.03125D0)
C         CALL MDPROJ ('SV',PLAT(ISPT),PLON(ISPT),0.D0)
C         CALL MDPSET ('AN',     65.D0,     65.D0,     65.D0,     65.D0)
C
C Tell EZMAP what spacing to use on the grid.
C
          CALL MAPSTD ('GR',GRSP(ISPT))
C
C Draw a default EZMAP map, using the outline database 'CO', and then
C add a grid, lat/lon labels, and informational labels.
C
          PRINT * , '  USING DATABASE "CO"'
          CALL ARINAM (IAMA,LAMA)
          CALL MDPBLA (IAMA)
          CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLRCO)
          CALL GSPLCI (7)
          CALL MDPLOT
          CALL GSPLCI (1)
          CALL MDPGRD
          CALL MDLBLT (.08,.08,.08,.92,-CHSZ,    0.,CHSZ,0.,+1.)
          CALL MDLBLT (.92,.08,.92,.92, CHSZ,    0.,CHSZ,0.,-1.)
          CALL MDLBLN (.08,.08,.92,.08,0.,-1.5*CHSZ,CHSZ,0., 0.)
          CALL MDLBLN (.08,.92,.92,.92,0.,+1.5*CHSZ,CHSZ,0., 0.)
          CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +                 TITL(ISPT)(1:MDILNB(TITL(ISPT))),.015,0.,0.)
          CALL PLCHHQ (CFUX(.5),CFUY(.025),'Old Database "CO"',
     +                                              .015,0.,0.)
          CALL FRAME
C
C Draw a map using outlines from the database "Earth..2", and then
C add a grid, lat/lon labels, and informational labels.
C
          PRINT * , '  USING DATABASE "Earth..2"'
          CALL ARINAM (IAMA,LAMA)
          CALL MDLNAM ('Earth..2',1,IAMA)
          CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLRE2)
          CALL GSPLCI (7)
          CALL MDLNDR ('Earth..2',1)
          CALL GSPLCI (1)
          CALL MDPGRD
          CALL MDLBLT (.08,.08,.08,.92,-CHSZ,    0.,CHSZ,0.,+1.)
          CALL MDLBLT (.92,.08,.92,.92, CHSZ,    0.,CHSZ,0.,-1.)
          CALL MDLBLN (.08,.08,.92,.08,0.,-1.5*CHSZ,CHSZ,0., 0.)
          CALL MDLBLN (.08,.92,.92,.92,0.,+1.5*CHSZ,CHSZ,0., 0.)
          CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +                 TITL(ISPT)(1:MDILNB(TITL(ISPT))),.015,0.,0.)
          CALL PLCHHQ (CFUX(.5),CFUY(.025),'Earth..2',.015,0.,0.)
          CALL FRAME
C
C Now, draw a solid-filled map using the RANGS/GSHHS data.
C
          PRINT * , '  USING RANGS/GSHHS DATABASE'
C
          CALL MDRGSF (IRGL(ISPT),RWRK,LRWK,IAMA,LAMA)
C
C Add outlines using the RANGS/GSHHS data.
C
          CALL MDRGOL (IRGL(ISPT),RWRK,LRWK)
C
C Overlay a lat/lon grid.
C
          CALL MDPGRD
C
C Add latitude and longitude labels.
C
          CALL MDLBLT (.08,.08,.08,.92,-CHSZ,    0.,CHSZ,0.,+1.)
          CALL MDLBLT (.92,.08,.92,.92, CHSZ,    0.,CHSZ,0.,-1.)
          CALL MDLBLN (.08,.08,.92,.08,0.,-1.5*CHSZ,CHSZ,0., 0.)
          CALL MDLBLN (.08,.92,.92,.92,0.,+1.5*CHSZ,CHSZ,0., 0.)
C
C Put a label at the top of the plot.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +                 TITL(ISPT)(1:MDILNB(TITL(ISPT))),.015,0.,0.)
C
C Put a label at the bottom of the plot.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.025),'RANGS/GSHHS',.015,0.,0.)
C
C Advance the plotter frame.
C
          CALL FRAME
C
C End of loop.
C
  101   CONTINUE
C
C Reset the PLOTCHAR font, character color, and outlining.
C
        CALL PCSETI ('FN', 0)
        CALL PCSETI ('CC',-1)
        CALL PCSETI ('OF', 0)
        CALL PCSETI ('OC', 1)
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


      SUBROUTINE COLRCO (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
        IAI1=-1
        DO 101 IGRP=1,NGPS
          IF (IAGI(IGRP).EQ.1) IAI1=IAAI(IGRP)
  101   CONTINUE
        IF (IAI1.GT.0) THEN
          IF (MDPACI(IAI1).EQ.1) THEN
C           OCEAN
            CALL GSFACI (2)
          ELSE
C           LAND
            CALL GSFACI (3)
          END IF
          CALL GFA    (NCRA-1,XCRA,YCRA)
        END IF
        RETURN
      END


      SUBROUTINE COLRE2 (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
        IAI1=-1
        DO 101 IGRP=1,NGPS
          IF (IAGI(IGRP).EQ.1) IAI1=IAAI(IGRP)
  101   CONTINUE
        IF (IAI1.GT.0) THEN
          IF (MDIPAN(IAI1,'Water').NE.0) THEN
C           OCEAN
            CALL GSFACI (2)
          ELSE
C           LAND
            CALL GSFACI (3)
          END IF
          CALL GFA    (NCRA-1,XCRA,YCRA)
        END IF
        RETURN
      END
