
        PROGRAM MPEX10
C
C Assume you have a FORTRAN function that, given the position of a
C point on the surface of the earth, returns the real value of some
C physical quantity there.  You would like to split the full range of
C values of the quantity into intervals, associate a different color
C with each interval, and then draw a colored map of the resulting
C globe.  One way to do this is to use the contouring package CONPACK;
C another way to do it is given below.
C
C This program constructs a rectangular cell array covering the part
C of the plotter frame occupied by a selected map of the globe.  Each
C element of the cell array occupies a small rectangular portion of
C the plotter frame.  The EZMAP routine MAPTRI, which does the inverse
C transformations, is used to find the values of latitude and longitude
C associated with each cell; these can be used to obtain the value of
C the physical quantity and therefore the color index associated with
C the cell.  When the cell array is complete, it is drawn by a call to
C the GKS routine GCA.
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
C Define an integer array in which to build the cell array.
C
        DIMENSION ICRA(1000,1000)
C
C Declare some required variables.
C
        CHARACTER*2 PROJ,JLTS
        DIMENSION PLM1(2),PLM2(2),PLM3(2),PLM4(2)
C
C NCLS specifies the number of cells along each edge of the cell array.
C Use a positive value less than or equal to 1000.
C
        DATA NCLS / 300 /
C
C NCLR specifies the number of different colors to be used.
C
        DATA NCLR / 64 /
C
C PROJ is the desired projection type.  Use one of 'LC', 'ST', 'OR',
C 'LE', 'GN', 'AE', 'SV', 'CE', 'ME', or 'MO'.
C
        DATA PROJ / 'OR' /
C
C PLAT and PLON are the desired latitude and longitude of the center of
C the projection, in degrees.
C
        DATA PLAT,PLON / 40. , -105. /
C
C ROTA is the desired final rotation of the map, in degrees.
C
        DATA ROTA / 0. /
C
C SALT, ALFA, and BETA are the desired values of the parameters 'SA',
C 'S1', and 'S2', which are only used with a satellite-view projection.
C SALT is the distance of the satellite from the center of the earth,
C in units of earth radii.  ALFA is the angle, in degrees, between the
C line of sight and the line to the center of the earth.  BETA is used
C only when ALFA is non-zero; it is the angle, in degrees, measured
C counterclockwise, from the plane passing through the satellite, the
C center of the earth, and the point which is due east on the horizon
C to the plane in which the line of sight and the line to the center
C of the earth both lie.
C
        DATA SALT,ALFA,BETA / 1.25 , 15. , 90. /
C
C JLTS, PLM1, PLM2, PLM3, and PLM4 are the required arguments of the
C EZMAP routine MAPSET, which determines the boundaries of the map.
C
        DATA JLTS / 'MA' /
        DATA PLM1(1),PLM2(1),PLM3(1),PLM4(1) / 0. , 0. , 0. , 0. /
        DATA PLM1(2),PLM2(2),PLM3(2),PLM4(2) / 0. , 0. , 0. , 0. /
C
C IGRD is the spacing, in degrees, of the EZMAP grid of latitudes and
C longitudes.
C
        DATA IGRD / 15 /
C
C Define the constant used to convert from degrees to radians.
C
        DATA DTOR / .017453292519943 /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Define the color indices required.  0 and 1 are used for black and
C white (as is customary); the next NCLR values are distributed between
C pure blue (color 2) and pure red (color NCLR+1).
C
        CALL GSCR (IWKID,0,0.,0.,0.)
        CALL GSCR (IWKID,1,1.,1.,1.)
C
        DO 101 ICLR=1,NCLR
          CALL GSCR (IWKID,1+ICLR,REAL(ICLR-1)/REAL(NCLR-1),0.,
     +                        REAL(NCLR-ICLR)/REAL(NCLR-1))
  101   CONTINUE
C
C Set the EZMAP projection parameters.
C
        CALL MAPROJ (PROJ,PLAT,PLON,ROTA)
        IF (PROJ.EQ.'SV') THEN
          CALL MAPSTR ('SA',SALT)
          CALL MAPSTR ('S1',ALFA)
          CALL MAPSTR ('S2',BETA)
        END IF
C
C Set the limits of the map.
C
        CALL MAPSET (JLTS,PLM1,PLM2,PLM3,PLM4)
C
C Set the grid spacing.
C
        CALL MAPSTI ('GR - GRID SPACING',IGRD)
C
C Initialize EZMAP, so that calls to MAPTRI will work properly.
C
        CALL MAPINT
C
C Fill the cell array.  The data generator is rigged to create
C values between 0 and 1, so as to make it easy to interpolate to
C get a color index to be used.  Obviously, the statement setting
C DVAL can be replaced by one that yields a value of some real data
C field of interest (normalized to the range from 0 to 1).
C
        DO 103 I=1,NCLS
          X=CFUX(.05+.90*(REAL(I-1)+.5)/REAL(NCLS))
          DO 102 J=1,NCLS
            Y=CFUY(.05+.90*(REAL(J-1)+.5)/REAL(NCLS))
            CALL MAPTRI (X,Y,RLAT,RLON)
            IF (RLAT.NE.1.E12) THEN
              DVAL=.25*(1.+COS(DTOR*10.*RLAT))+
     +             .25*(1.+SIN(DTOR*10.*RLON))*COS(DTOR*RLAT)
              ICRA(I,J)=MAX(2,MIN(NCLR+1,2+INT(DVAL*REAL(NCLR))))
            ELSE
              ICRA(I,J)=0
            END IF
  102     CONTINUE
  103   CONTINUE
C
C Draw the cell array.
C
        CALL GCA (CFUX(.05),CFUY(.05),CFUX(.95),CFUY(.95),1000,1000,
     +                                           1,1,NCLS,NCLS,ICRA)
C
C Draw a map on top of the cell array.
C
        CALL MAPDRW
C
C Put a label at the top of the plot.
C
        CALL SET   (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL WTSTR (.5,.975,'EXAMPLE 10',2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
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
