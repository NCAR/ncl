
        PROGRAM CPEX09
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
C Define an array for the data.
C
        DIMENSION ZDAT(40,40)
C
C Generate dummy data.
C
        CALL GENDAT (ZDAT,40,40,40,14,14,-145.6,389.7)
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
C Force the use of medium-quality characters by the package PLOTCHAR.
C
        CALL PCSETI ('QU - QUALITY OF CHARACTERS',1)
C
C Put a label at the top of the first plot.  (The SET call is not
C needed for CPEZCT, but for the labelling routine.)
C
        CALL SET (.05,.95,.05,.95,0.,1.,0.,1.,1)
        CALL LABTOP ('EXAMPLE 9-1',.017)
C
C Put a boundary line at the edge of the plotter frame.
C
        CALL BNDARY
C
C Contour the data, using the EZCNTR simulator.
C
        CALL CPEZCT (ZDAT,40,40)
C
C Contour a subset of the data, forcing a contour interval of 20, using
C the CONREC simulator.
C
        CALL CPCNRC (ZDAT(7,9),40,32,24,0.,0.,20.,3,0,-682)
C
C Put a boundary line at the edge of the plotter frame, label the plot,
C and advance the frame.
C
        CALL BNDARY
        CALL LABTOP ('EXAMPLE 9-2',.017)
        CALL FRAME
C
C Switch to the "penalty scheme" for positioning contour-line labels
C and change one of the constants which are used by it.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
        CALL CPSETR ('PC1 - PENALTY SCHEME CONSTANT 1',1.5)
C
C Turn on the smoother, with a relatively high tension.
C
        CALL CPSETI ('T2D - TENSION ON THE 2D SMOOTHER',4)
C
C Force the labelling of every other contour line.  (This will only
C work when the contour interval is forced, as it will be in the next
C call to CPCNRC.)
C
        CALL CPSETI ('LIS - LABEL INTERVAL SPECIFIER',2)
C
C Repeat the last plot, forcing a contour interval of 10.
C
        CALL CPCNRC (ZDAT(7,9),40,32,24,0.,0.,10.,3,0,-682)
C
C Put a boundary line at the edge of the plotter frame, label the plot,
C and advance the frame.
C
        CALL BNDARY
        CALL LABTOP ('EXAMPLE 9-3',.017)
        CALL FRAME
C
C Create an EZMAP background.
C
        CALL MAPSTI ('PE - PERIMETER',0)
        CALL MAPSTI ('GR - GRID',0)
        CALL MAPSTC ('OU - OUTLINE DATASET','PS')
        CALL MAPSTI ('DO - DOTTING OF OUTLINES',1)
        CALL MAPSTR ('SA - SATELLITE HEIGHT',1.13)
        CALL MAPROJ ('SV - SATELLITE-VIEW',40.,-95.,0.)
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
        CALL MAPDRW
C
C Arrange for output from CPCNRC to be placed on the EZMAP background.
C
        CALL CPSETR ('XC1 - X COORDINATE AT I = 1',-130.)
        CALL CPSETR ('XCM - X COORDINATE AT I = M',-60.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',10.)
        CALL CPSETR ('YCN - Y COORDINATE AT J = N',70.)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Define some special values and arrange for the special-value area to
C be outlined.
C
        ZDAT(15,13)=1.E36
        ZDAT(16,13)=1.E36
        ZDAT(15,14)=1.E36
        CALL CPSETR ('SPV - OUT-OF-RANGE VALUE',1.E36)
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
C
C Specify that high/low labels are to be written at an angle of 30
C degrees, using relatively small characters.  (These parameters will
C really be used for point-value labels.)
C
        CALL CPSETR ('HLA - HIGH/LOW LABEL ANGLE',30.)
        CALL CPSETR ('HLS - HIGH/LOW LABEL CHARACTER SIZE',.008)
C
C Turn off line labelling.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',0)
C
C Use the CONREC simulator to plot a subset of the data on the EZMAP
C background, labelling each data point.
C
        CALL CPCNRC (ZDAT(9,7),40,20,20,0.,0.,10.,4,1,-682)
C
C Put a boundary line at the edge of the plotter frame, label the plot,
C and advance the frame.
C
        CALL BNDARY
        CALL LABTOP ('EXAMPLE 9-4',.017)
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
