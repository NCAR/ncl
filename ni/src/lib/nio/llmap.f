C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPBD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA MAPBDX
C
        PARAMETER (MNAI=6000)
C
C The common block MAPCM0 contains mathematical constants.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
C
C The common block MAPCM1 contains transformation constants.
C
        COMMON /MAPCM1/  COSO,COSR,PHOC,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,PHOC,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
C The common block MAPCM2 contains area-specification variables.
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
C The common block MAPCM3 contains parameters having to do with reading
C the data for outlines.
C
        COMMON /MAPCM3/  ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,
     +                   SLOG,PNTS(200),IDOS(4)
        INTEGER          ITPN,NOUT,NPTS,IGID,IDLS,IDRS,IDOS
        REAL             BLAG,SLAG,BLOG,SLOG,PNTS
        SAVE   /MAPCM3/
C
C The common block MAPCM4 contains most of the input parameters.
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
C The common block MAPCM5 contains various lists ("dictionaries") of
C two-character codes required by EZMAP for parameter-setting.
C
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(12),
     +                   PDCL(12)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
C The common block MAPCM6 contains quantities needed by the routines
C MDPTRA and MDPTRI.
C
        COMMON /MAPCM6/  UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM,ELPM
        DOUBLE PRECISION UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM
        LOGICAL ELPM
        SAVE   /MAPCM6/
C
C The common block MAPCM7 contains parameters describing the portion of
C the plotter frame being used.
C
        COMMON /MAPCM7/  ULOW,UROW,VBOW,VTOW
        DOUBLE PRECISION ULOW,UROW,VBOW,VTOW
        SAVE   /MAPCM7/
C
C The common block MAPCM8 contains parameters set by MDPTRN and used by
C MAPIT in handling "cross-over" problems.
C
        COMMON /MAPCM8/  P,Q,R
        DOUBLE PRECISION P,Q,R
        SAVE   /MAPCM8/
C
C The common block MAPCMA contains values which are used to position
C dots along dotted outlines and to avoid drawing vectors which are
C too short.
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
C The common block MAPCMC contains variables for MDPBLA.
C
        COMMON /MAPCMC/  IGI1,IGI2,NCRA,NOVS,XCRA(100),YCRA(100)
        INTEGER          IGI1,IGI2,NCRA,NOVS
        REAL             XCRA,YCRA
        SAVE   /MAPCMC/
C
C The common block MAPCMP contains the buffers in which the X and Y
C coordinates of points are collected for an eventual call to POINTS.
C
        COMMON /MAPCMP/  NPTB,XPTB(50),YPTB(50)
        INTEGER          NPTB
        REAL             XPTB,YPTB
        SAVE   /MAPCMP/
C
C The common block MAPCMQ contains quantities specifying the color
C indices to be used for various portions of the plot.
C
        COMMON /MAPCMQ/  ICIN(8)
        INTEGER          ICIN
        SAVE   /MAPCMQ/
C
C The common blocks MAPCMX and MAPCMY contain variables used by the new
C routines in EZMAPB (implementing access to the new dataset created in
C early 1998).  MAPCMX contains integer variables and MAPCMY contains
C character variables.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/  NAME(MNAI),FLNS
        CHARACTER*64     NAME,FLNS
        SAVE   /MAPCMY/
C
C The common block MAPRGD contains two arrays of color indices and
C a couple of point-interpolation values used by the routines that plot
C data from the RANGS/GSHHS dataset.
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
C The common block MAPSAT contains parameters for the satellite-view
C projection.
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Below are descriptions of the variables in each of the common blocks,
C together with data statements giving default values to those variables
C which need default variables.
C
C Variables in MAPCM0:
C
        DATA COS1 / .999847695156390D0 /  !  COSINE OF 1 DEGREE
        DATA DTOR / .017453292519943D0 /  !  DEGREES TO RADIANS
        DATA DTRH / .008726646259971D0 /  !  DEGREES TO RADIANS, HALVED
        DATA OOPI / .318309886183790D0 /  !  ONE OVER PI
        DATA PI   / 3.14159265358979D0 /  !  PI
        DATA PIOT / 1.57079632679489D0 /  !  PI OVER TWO
        DATA RTDD / 114.591559026165D0 /  !  RADIANS TO DEGREES, DOUBLED
        DATA RTOD / 57.2957795130823D0 /  !  RADIANS TO DEGREES
        DATA SIN1 / .017452406437283D0 /  !  SINE OF 1 DEGREE
        DATA TOPI / .636619772367581D0 /  !  TWO OVER PI
C
C Variables in MAPCM1:
C
C IPRJ is an integer between 0 and 14, specifying what projection is
C currently in use.  The values 11, 12, 13, and 14 specify fast-path
C versions of the values 7, 8, 9, and 10, respectively.  The meanings
C of the various possible values of IPRJ are as follows:
C
C     IPRJ    Projection Selected                    Type
C     ----    -----------------------------------    -----------
C       0     USGS transformations                   Various
C       1     Lambert Conformal                      Conical
C       2     Stereographic                          Azimuthal
C       3     Orthographic (or Satellite)            Azimuthal
C       4     Lambert Equal-Area                     Azimuthal
C       5     Gnomonic                               Azimuthal
C       6     Azimuthal Equidistant                  Azimuthal
C       7     Cylindrical Equidistant (arbitrary)    Cylindrical
C       8     Mercator (arbitrary)                   Cylindrical
C       9     Mollweide (arbitrary)                  Cylindrical
C      10     Robinson (arbitrary)                   Cylindrical
C      11     Cylindrical Equidistant (fast-path)    Cylindrical
C      12     Mercator (fast-path)                   Cylindrical
C      13     Mollweide (fast-path)                  Cylindrical
C      14     Robinson (fast-path)                   Cylindrical
C
C PHOC is just a copy of PHIO, from the common block MAPCM4.  IROD is
C a flag which, if non-zero, says that we have to use double precision
C at selected points.  SINO, COSO, SINR, and COSR are projection
C variables computed by MDPINT for use by MDPTRN.
C
      DATA IROD / 0 /
C
C Variables in MAPCM2:
C
C UMIN, UMAX, VMIN, and VMAX specify the limits of the rectangle to be
C drawn, in projection space.  UCEN, VCEN, URNG, and VRNG are computed
C by MDPINT for use when the map perimeter is made elliptical (by
C setting the flag ELPF).  BLAM, SLAM, BLOM, and SLOM are respectively
C the biggest latitude, the smallest latitude, the biggest longitude,
C and the smallest longitude on the map.  They are used in MDPGRD and
C in MDPLOT to make the drawing of grids and outlines more efficient.
C ISSL is a flag set by MDPINT to indicate the success (ISSL=1) or
C failure (ISSL=0) of the code setting BLAM, SLAM, BLOM, and SLOM;
C failure indicates that default values were used and that it is not
C safe to take the efficient paths in MDPGRD.  PEPS is set by MDPINT
C for use in MAPIT in testing for cross-over problems.  UMIN and UMAX
C are given default values to prevent code in MDSETI and MDSETR from
C blowing up when PLTR is set prior to the first call to MDPINT.
C (08/07/2001) UOFF and VOFF are being added to this common block to
C support the use of U/V coordinates from which an offset has been
C subtracted to avoid loss of resolution when zooming in on a small
C portion of a projection.
C
      DATA UMIN,UMAX / 0.D0,1.D0 /
C
C Variables in MAPCM3:
C
C ITPN is the unit number of the "tape" from which outline data is to
C be read.  NOUT is the number of the outline to be used; the values 0
C through 5 imply 'NO', 'CO', 'US', 'PS', and 'PO', respectively; thus,
C if NOUT is zero, no outlines are to be used, and, if it is non-zero,
C it is the number of the "file" to be read from unit ITPN.  NPTS, just
C after a read, is the number of elements read into PNTS; it is then
C divided by 2 to become the number of points defined by the group just
C read.  IGID is an identifier for the group, so that, for example, one
C can distinguish a group belonging to a international boundary from
C one belonging to a U.S. state boundary.  IDLS and IDRS are integers
C identifying the areas to the left and right of the edge defined by
C the group of points just read.  BLAG, SLAG, BLOG, and SLOG specify
C the biggest and smallest latitude and the biggest and smallest
C longitude of the points in the group, so that, in some cases at least,
C one can decide quickly not to bother with the group.  PNTS contains
C NPTS coordinate pairs, each consisting of a latitude and a longitude,
C in degrees.  IDOS is an array of area-identifier offsets; for a given
C EZMAP dataset with index NOUT, the area identifiers in the dataset
C are biased by the addition of IDOS(NOUT) in order to generate a
C unique value.
C
      DATA ITPN,NOUT / 1,1 /
C
      DATA IDOS / 0,222,439,1003 /
C
C Variables in MAPCM4:
C
C INTF is a flag whose value at any given time indicates whether the
C package EZMAP is in need of initialization (.TRUE.) or not (.FALSE.).
C JPRJ is an integer between 0 and 10 indicating the type of projection
C currently in use.  PHIA, PHIO, and ROTA are the pole latitude and
C longitude and the rotation angle specified by the last user call to
C MAPROJ.  ILTS is an integer between 1 and 6, specifying how the limits
C of the map are to be chosen.  PLA1-4 and PLB1-4 are the values given
C by the user for PLM1(1), PLM2(1), ..., PLM1(2), PLM2(2), ..., in the
C last call to MAPSET.  PLTR is the plotter resolution - effectively,
C the number of addressable points in the x direction.  GRID, if zero,
C turns off the drawing of the lat/lon grid; if non-zero, it specifies
C the desired default spacing between grid lines, in degrees.  IDSH is
C the desired dash pattern (16-bit binary) for grid lines.  IDOT is a
C flag selecting solid outlines (0) or dotted outlines (1).  LBLF is
C a logical flag indicating whether the international date line, the
C equator, the Greenwich meridian, and the poles are to be labelled or
C not.  PRMF is a logical flag indicating whether or not a perimeter
C is to be drawn.  ELPF is a logical flag indicating whether the map
C perimeter is to be rectangular (.FALSE.) or elliptical (.TRUE.).
C XLOW, XROW, YBOW, and YTOW are fractions between 0. and 1. specifying
C the position of area of the plotter frame in which the map is to be
C put; the map is centered in this area and made as large as possible.
C IDTL is a flag specifying that MAPIT should draw solid outlines (0)
C or dotted outlines (1).  GRDR and SRCH are measured in degrees and
C lie in the range from .001 to 10.  GRDR specifies the resolution with
C which the grid is to be drawn and SRCH the accuracy with which the
C latitude/longitude limits of the map are to be found.  ILCW is the
C character width for characters in the label, as required for use in a
C call to PWRIT.  GRLA and GRLO specify the desired spacings of latitude
C and longitude grid lines, respectively (in degrees); if either is less
C than or equal to zero, the value of GRID is used instead.  GRPO
C specifies the way in which the lat/lon grid is to be modified near
C the poles on projections which map the poles into single points; it
C has a value of the form 1000*GRP1+GRP2, where GRP1 is an integer
C between 0 and 90 and GRP2 is a positive real between 0 and 360.  If
C GRP1 is zero, all the latitude lines of the grid are drawn; if GRP1
C is non-zero, latitude lines at latitudes from GRP1 to 90, inclusive
C (South or North) are omitted.   If GRP2 is zero, no longitude lines
C are drawn near the poles; if GRP2 is non-zero, only longitude lines
C of the grid at multiples of GRP2 are drawn near the poles.  Examples:
C GRPO=0 says "draw all the latitude lines of the grid; omit longitude
C lines of the grid near the poles."  GRPO=1 says "draw entire grid."
C GRPO=75045 says "suppress latitude lines of the grid above 75N and
C below 75S; near the poles, draw longitude lines of the grid only at
C multiples of 45 degrees."  The default is GRPO=90, which says "draw
C all latitude lines of the grid; near the poles, omit longitude lines
C of the grid except those at multiples of 90 degrees".  OTOL is an
C "offset tolerance" (new in 08/08/2001): when the absolute value of the
C difference of the U values at the left and right edges of a map is
C less than OTOL times the absolute value of their sum, or the absolute
C value of the difference of the V values at the bottom and top edges of
C a map is less than OTOL times the absolute value of their sum, MDPINT
C will initialize the projection to use offset values of U and V, thus
C avoiding a loss of precision.
C
      DATA INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,PLB1,PLB2 /
     1   .TRUE.,   7,0.D0,0.D0,0.D0,   1,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0 /
C
      DATA PLB3,PLB4,    PLTR, GRID, IDSH,IDOT, LBLF , PRMF ,  ELPF  /
     1     0.D0,0.D0,32768.D0,10.D0,21845,   0,.TRUE.,.TRUE.,.FALSE. /
C
      DATA IDTL,  OTOL /
     1        0,.001D0 /
C
      DATA XLOW,XROW,YBOW,YTOW / .05D0,.95D0,.05D0,.95D0 /
C
      DATA GRDR,SRCH / 1.D0,1.D0 /
C
      DATA ILCW / 1 /
C
      DATA GRLA,GRLO,GRPO / 0.D0,0.D0,90.D0 /
C
C Variables in MAPCM5:
C
C DDCT is the dictionary of available datasets, LDCT the dictionary of
C map limit definition types, and PDCT the dictionary of map projection
C names.  DDCL, LDCL, and PDCL are lower-case equivalents.
C
      DATA DDCT
     +        / 'NO','CO','US','PS','PO' /
      DATA DDCL
     +        / 'no','co','us','ps','po' /
C
      DATA LDCT
     +        / 'MA','CO','PO','AN','LI','GR' /
      DATA LDCL
     +        / 'ma','co','po','an','li','gr' /
C
      DATA PDCT
     +   / 'UT','LC','ST','OR','LE','GN','AE','CE','ME','MO','RO','SV' /
      DATA PDCL
     +   / 'ut','lc','st','or','le','gn','ae','ce','me','mo','ro','sv' /
C
C Variables in MAPCM6:
C
C All of the variables in this common block are basically copies of
C variables in other common blocks (perhaps somewhat modified) for use
C by transformation routines.  ELPM is a copy of the logical variable
C ELPF, which indicates whether the map perimeter is to be rectangular
C or elliptical.  UMNM, UMXM, UMXM, and VMNM specify a rectangular
C perimeter just a little outside the one specified by UMIN, UMAX, VMIN,
C and VMAX.  UCNM, VCNM, URNM, and VRNM are just copies of the values
C UCEN, VCEN, URNG, and VRNG, respectively; they are used when the map
C perimeter is elliptical.  None of these parameters needs a default
C value.
C
C Variables in MAPCM7:
C
C ULOW, UROW, VBOW, and VTOW define the fraction of the plotter frame
C to be occupied by the map - they may be thought of as the first four
C arguments of the SET call or, in the GKS scheme, as the viewport.
C They are computed by MDPINT.  ULOW and UROW are given default values
C to prevent code in MDSETI and MDSETR from blowing up when PLTR is
C set prior to the first call to MDPINT.
C
      DATA ULOW,UROW / 0.D0,1.D0 /
C
C Variables in MAPCM8:
C
C P, Q, and R are set by MDPTRN each time it maps (RLAT,RLON) to (U,V).
C Q is always equal to V, but P is not always equal to U.  Instead, it
C is a value of U from an intermediate step in the projection process.
C For the Lambert conformal conic, P is the distance, in longitude, from
C the central meridian.  For the cylindrical projections, P is a value
C of U prior to multiplication by a function of V shrinking the map
C toward a vertical bisector.  They are all used by MAPIT, while drawing
C lines from point to point, to detect "cross-over" (a jump from one
C side of the map to the other, caused by the projection's having slit
C the globe along some half of a great circle and laid it open with the
C two sides of the slit at opposite ends of the map).
C
C Variables in MAPCMA:
C
C DPLT is the mimimum vector length; MAPIT requires two points to be at
C least DPLT plotter units apart before it will join them with a vector.
C DDTS is the desired distance in plotter units between dots in a dotted
C outline.  These values are relative to the "plotter resolution" PLTR;
C DPLT/PLTR is a fraction of the plotter frame.  DSCA is the ratio of
C the length of a vector, measured in plotter units, to the length of
C the same vector, measured in the u/v plane.  Thus, given a vector of
C length D in the u/v plane, D*DSCA is its length in plotter units.
C DPSQ and DSSQ are the squares of DPLT and DSCA, respectively.  DBTD
C is the distance, in the u/v plane, between two dots DDTS plotter
C units apart.  DPLT and DDTS have the values given below and are not
C reset by the code; DSCA, DPSQ, DSSQ, and DBTD are computed by MDPINT.
C DSCA is given a default value only to keep the routines MDSETI and
C MDSETR from blowing up when DDTS is set prior to any call to MDPINT.
C DATL is used by MAPIT and MDPVP to keep track of where the next point
C along a curve should go.
C
      DATA DPLT,DDTS,DSCA / 1.D0,96.D0,1.D0 /
C
C Variables in MAPCMC:
C
C IGI1 is the group identifier for the principal edge group - the one
C containing all the real map boundaries.  IGI2 is the group identifier
C for the dummy group that splits the map into vertical strips.  NOVS
C is the number of vertical strips to be used.  XCRA and YCRA hold X
C and Y coordinates for delivery to AREDAM.  NCRA is the number of
C coordinates in XCRA and YCRA.
C
      DATA IGI1,IGI2,NOVS,NCRA / 1,2,1,0 /
C
C Variables in MAPCMP:
C
C NPTB is the number of points whose coordinates have been collected in
C the arrays XPTB and YPTB for eventual output by a call to POINTS.
C
      DATA NPTB / 0 /
C
C Variables in MAPCMQ:
C
C The array ICIN specifies color indices to be used for the perimeter,
C for the grid, for labelling, for limbs, for the continental outlines,
C for the U.S. state outlines, and for international political outlines.
C See the routine MDPCHI.  The default values are all negative, to say
C that color should not be set.
C
      DATA ICIN / -1,-1,-1,-1,-1,-1,-1,-1 /
C
C Variables in MAPCMX and MAPCMY:
C
C For each I from 1 to MNAI, IATY(I), if non-zero, is the type of the
C area with area identifier I, ISCI(I) is a suggested color index for
C that area, IPAR(I) is the index of the parent area, and NAME(I) is
C the name of the area.  FLNS is the full name of the file from which
C the data in IATY, ISCI, IPAR, and NAME were read; it allows us to
C avoid re-reading the data when that isn't necessary.
C
      DATA (IATY(I),I=1,MNAI) / MNAI*0   /
      DATA (ISCI(I),I=1,MNAI) / MNAI*0   /
      DATA (IPAR(I),I=1,MNAI) / MNAI*0   /
      DATA (NAME(I),I=1,MNAI) / MNAI*' ' /
C
      DATA FLNS / ' ' /
C
C Variables in MAPRGD:
C
C The array ICOL contains five color indices to be used in drawing
C the outlines of polygons of the five different types defined by the
C RANGS/GSHHS data, and the array ICSF contains five color indices to
C be used in filling the polygons.  The routine MDRGSC may be called
C by the user to set these colors and the routine MDRGGC may be called
C to retrieve them.  The indices of each of these arrays are assigned
C to features as follows: 1 => ocean; 2 => land; 3 => lake; 4 =>
C island in lake; and 5 => pond on island.  NILN and NILT determine
C how many points are to be interpolated per degree of longitude or
C degree of latitude, respectively, along those parts of the filled
C polygons lying along the edges of 1-degree cells.  The object of this
C interpolation is to reduce the probability of having little gaps
C along the seams between adjacent cells.  (If it is not done and a
C projection is used that maps the 1-degree cells into patches whose
C edges are neither horizontal nor vertical, the gaps can be quite
C noticeable.)  OLAT and OLON are temporary variables in which some
C values are passed from MPRGSX to MPRGFA; they do not need to be
C initialized.  IDPF is a flag indicating how the RANGS/GSHHS data
C to be processed.  IDPF = 0 means that the data are to be used in
C are unedited form (MDRGSQ is called).  IDPF = 1 means that the data
C are to be edited and that polygon fills are to be done using direct
C calls to GFA, without using an area map (MDRGSX is called).  IDPF =
C 2 means that the data are to be edited and that fills are to be done
C using an area map (MDRGSX is called).
C
      DATA ICOL / 1 , 1 , 1 , 1 , 1 /
      DATA ICSF / 0 , 1 , 0 , 1 , 0 /
C
      DATA IDPF / 1 /
C
      DATA NILN,NILT / 50 , 50 /
C
C Variables in MAPSAT:
C
C The absolute value of SALT, if greater than 1, serves as a flag that
C a satellite-view projection is to be used in place of an orthographic
C projection; its value is the distance of the satellite from the center
C of the earth, in units of earth radii.  In this case, SSMO is the
C square of SALT minus 1 and SRSS is the square root of SSMO.  If ALFA
C is zero, the projection shows the view seen by a satellite looking
C straight at the center of the earth; call this the basic satellite
C view.  If ALFA is non-zero, it and BETA are angles, in degrees,
C determining where the line of sight of the projection is.  If E is
C at the center of the earth, S is at the satellite, and P is a point
C along the line of sight, then ALFA measures the angle ESP.  If O is
C the point at the origin of the basic satellite view and P is the
C projection of the line of sight, then BETA measures the angular
C distance from the positive u axis to the line OP, positive if
C measured counter-clockwise.  DSNA, DCSA, DSNB, and DCSB are sines
C and cosines of ALFA and BETA.  The sign of SALT says whether a normal
C projection (positive) or an extended projection (negative) is to be
C used.  The latter makes it easier to overlay CONREC output on one of
C these projections, by projecting points out of sight around the limb
C to point just outside the limb on the projected view.
C
      DATA SALT,ALFA,BETA / 0.D0,0.D0,0.D0 /
C
      DATA DSNA,DCSA,DSNB,DCSB / 0.D0,1.D0,0.D0,1.D0 /
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPPOS (ARG1,ARG2,ARG3,ARG4)
        REAL ARG1,ARG2,ARG3,ARG4
        IF (ICFELL('MAPPOS - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPPOS (DBLE(ARG1),DBLE(ARG2),DBLE(ARG3),DBLE(ARG4))
        IF (ICFELL('MAPPOS',2).NE.0) RETURN
        RETURN
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPROJ (ARG1,ARG2,ARG3,ARG4)
        CHARACTER*(*) ARG1
        REAL          ARG2,ARG3,ARG4
        IF (ICFELL('MAPROJ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPROJ (ARG1,DBLE(ARG2),DBLE(ARG3),DBLE(ARG4))
        IF (ICFELL('MAPROJ',2).NE.0) RETURN
        RETURN
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPSET (ARG1,ARG2,ARG3,ARG4,ARG5)
        CHARACTER*(*) ARG1
        REAL          ARG2(2),ARG3(2),ARG4(2),ARG5(2)
        DOUBLE PRECISION DRG2(2),DRG3(2),DRG4(2),DRG5(2)
        DRG2(1)=DBLE(ARG2(1))
        DRG2(2)=DBLE(ARG2(2))
        DRG3(1)=DBLE(ARG3(1))
        DRG3(2)=DBLE(ARG3(2))
        DRG4(1)=DBLE(ARG4(1))
        DRG4(2)=DBLE(ARG4(2))
        DRG5(1)=DBLE(ARG5(1))
        DRG5(2)=DBLE(ARG5(2))
        CALL MDPSET (ARG1,DRG2,DRG3,DRG4,DRG5)
        IF (ICFELL('MAPSET',2).NE.0) RETURN
        RETURN
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPTRA (RLAT,RLON,UVAL,VVAL)
        REAL   RLAT,RLON,UVAL,VVAL
        DOUBLE PRECISION DUVL,DVVL
        IF (ICFELL('MAPTRA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPTRA (DBLE(RLAT),DBLE(RLON),DUVL,DVVL)
        IF (ICFELL('MAPTRA',2).NE.0) RETURN
        IF (DUVL.NE.1.D12) THEN
          UVAL=REAL(DUVL)
          VVAL=REAL(DVVL)
        ELSE
          UVAL=1.E12
          VVAL=1.E12
        END IF
        RETURN
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPTRI (UVAL,VVAL,RLAT,RLON)
        REAL   UVAL,VVAL,RLAT,RLON
        DOUBLE PRECISION DLAT,DLON
        IF (ICFELL('MAPTRI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPTRI (DBLE(UVAL),DBLE(VVAL),DLAT,DLON)
        IF (ICFELL('MAPTRI',2).NE.0) RETURN
        IF (DLAT.NE.1.D12) THEN
          RLAT=REAL(DLAT)
          RLON=REAL(DLON)
        ELSE
          RLAT=1.E12
          RLON=1.E12
        END IF
        RETURN
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPTRN (RLAT,RLON,UVAL,VVAL)
        REAL   RLAT,RLON,UVAL,VVAL
        DOUBLE PRECISION DUVL,DVVL
        IF (ICFELL('MAPTRN - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPTRN (DBLE(RLAT),DBLE(RLON),DUVL,DVVL)
        IF (ICFELL('MAPTRN',2).NE.0) RETURN
        IF (DUVL.NE.1.D12) THEN
          UVAL=REAL(DUVL)
          VVAL=REAL(DVVL)
        ELSE
          UVAL=1.E12
          VVAL=1.E12
        END IF
        RETURN
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPCEM (IEM1,IEM2,IERR,IFLG)
C
        CHARACTER*(*) IEM1,IEM2
        INTEGER       IERR,IFLG
C
C MDPCEM is called to do a call to SETER when the error message to be
C printed is in two parts which need to be concatenated.  FORTRAN-77
C rules make it necessary to concatenate the two parts of the message
C into a local character variable.
C
        CHARACTER*100 IEMC
C
        IEMC=IEM1//IEM2
        CALL SETER (IEMC,IERR,IFLG)
C
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPIN1
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,PHOC,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,PHOC,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Set up alternate names for a couple of variables in common.
C
        DOUBLE PRECISION FLT1,FLT2
        EQUIVALENCE      (PHIA,FLT1),(ROTA,FLT2)
C
C Declare local variables.
C
        DOUBLE PRECISION CHI1,CHI2,COST,SINT,TMP1,TMP2
        REAL             TST1,TST2,TST3
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL MAPBD
C
C Decide whether MAPTRN should use real or double-precision arithmetic.
C The subroutine call is necessary to fool some compilers into storing
C TST1, TST2, and TST3; otherwise, real precision may be used on
C machines on which double precision is necessary.  NOTE: As of 07/2001,
C double precision is being used everywhere except in the USGS routines,
C so the flag IROD is not as important as it was prior to that time.
C
        CALL MDPIN2 (TST1,TST2,TST3)
C
        IF (TST1.NE.TST2.AND.TST2.NE.TST3) THEN
          IROD=0
        ELSE
          IROD=1
        END IF
C
C IPRJ equals JPRJ until we find out if fast-path projections are to be
C used.  PHOC is just a copy of PHIO.
C
        IPRJ=JPRJ
        PHOC=PHIO
C
        IF (IPRJ.EQ.1) THEN
C
C Compute quantities required for the Lambert conformal conic.
C
          SINO=SIGN(1.D0,.5D0*(FLT1+FLT2))
          CHI1=(90.D0-SINO*FLT1)*DTOR
C
          IF (FLT1.EQ.FLT2) THEN
            COSO=COS(CHI1)
          ELSE
            CHI2=(90.D0-SINO*FLT2)*DTOR
            COSO=LOG(SIN(CHI1)/SIN(CHI2))/
     +           LOG(TAN(.5D0*CHI1)/TAN(.5D0*CHI2))
          END IF
C
        ELSE
C
C Compute quantities required for all the other projections.
C
          TMP1=ROTA*DTOR
          TMP2=PHIA*DTOR
          SINR=SIN(TMP1)
          COSR=COS(TMP1)
          SINO=SIN(TMP2)
          COSO=COS(TMP2)
C
C Compute constants required only by the cylindrical projections.
C
          IF (IPRJ.GE.7) THEN
C
C See if fast-path transformations can be used (PLAT=0, ROTA=0 or 180).
C
            IF (ABS(PHIA).GE..000001D0.OR.(ABS(ROTA).GE..000001D0.AND.
     +                                  ABS(ROTA).LE.179.999999D0)) THEN
C
C No.  Compute constants for the ordinary cylindrical projections.
C
              SINT=COSO*COSR
              COST=SQRT(1.D0-SINT**2)
              TMP1=SINR/COST
              TMP2=SINO/COST
              PHOC=PHIO-ATAN2(TMP1,-COSR*TMP2)*RTOD
              SINR=TMP1*COSO
              COSR=-TMP2
              SINO=SINT
              COSO=COST
C
            ELSE
C
C Yes.  The fast paths are implemented as three additional projections.
C
              IPRJ=IPRJ+4
C
            END IF
C
          END IF
C
        END IF
C
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPIN2 (TST1,TST2,TST3)
        REAL TST1,TST2,TST3
        TST1=1.
        TST2=TST1+1.E-10
        TST3=TST2+1.E-10
        RETURN
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPINT
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,PHOC,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,PHOC,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM6/  UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM,ELPM
        DOUBLE PRECISION UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM
        LOGICAL ELPM
        SAVE   /MAPCM6/
C
        COMMON /MAPCM7/  ULOW,UROW,VBOW,VTOW
        DOUBLE PRECISION ULOW,UROW,VBOW,VTOW
        SAVE   /MAPCM7/
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER          IPRF
        SAVE   /USGSC1/
C
C Set up alternate names for some of the variables in common.
C
        EQUIVALENCE      (PLA1,AUMN),(PLA2,AUMX),
     +                   (PLA3,AVMN),(PLA4,AVMX)
C
C Declare local variables.
C
        DOUBLE PRECISION A,AUMN,AUMX,AVMN,AVMX,B,C,CLAT,CLON,CUMA,CUMI,
     +                   CVMA,CVMI,D,DLAT,DLON,DU,DV,E,F,R,RESL,RLAT,
     +                   RLN1,RLN2,RLON,RLT1,RLT2,SASQ,SUMA,SUMI,SVMA,
     +                   SVMI,TEM1,TEM2,TEM3,TEM4,TEM5,TEM6,TEM7,TEM8,
     +                   U,V,XMAX,XMIN,XPOS,YMAX,YMIN,YPOS
        INTEGER          I,J,NLNS,NLTS
C
C Declare function types.
C
        DOUBLE PRECISION RBGDFE
C
C Define a necessary constant.
C
        DATA RESL / 10.D0 /
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPINT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for an error in the projection specifier.
C
        IF (JPRJ.LT.0.OR.JPRJ.GE.11) GO TO 901
C
C Jump to a subroutine that is responsible for pre-computing constants
C required by MDPTRN.  The same subroutine may, on occasion, need to be
C called by MDPTRN itself.  (MDPTRN may not call MDPINT because MDPINT
C calls MDPTRN.)
C
        CALL MDPIN1
C
C Zero UOFF and VOFF to turn off the offset feature while we're setting
C up the projection.
C
        UOFF=0.D0
        VOFF=0.D0
C
C Set UMIN, UMAX, VMIN, and VMAX to correspond to the maximum useful
C area produced by the projection.
C
C Projection: US  LC  ST  OR  LE  GN  AE  CE  ME  MO  RO
C
        GO TO (100,101,102,101,102,102,103,104,103,105,106,
     +                                     104,103,105,106) , IPRJ+1
C
C USGS transformations.
C
  100   UMIN=UUMN  !  ???
        UMAX=UUMX  !  ???
        VMIN=UVMN  !  ???
        VMAX=UVMX  !  ???
C
        GO TO 107
C
C Lambert conformal conic and orthographic.  The quantity "R" which is
C used below is the largest acceptable ratio of the lengths of the major
C and minor axes of the elliptical limb of the satellite-view projection
C for which we will show the entire ellipse.  For larger values of the
C angle "alpha", we just use a "camera" with a 120-degree field of view;
C it is debatable whether there is any better course of action.
C
  101   IF (IPRJ.NE.3.OR.ABS(SALT).LE.1.D0.OR.ALFA.EQ.0.D0) THEN
          UMIN=-1.D0
          UMAX=+1.D0
          VMIN=-1.D0
          VMAX=+1.D0
        ELSE
          R=4.D0
          IF (ALFA.LT.RTOD*
     +        ACOS(SQRT((SALT*SALT+R*R-1.D0)/(R*R*SALT*SALT)))) THEN
            SASQ=SALT*SALT
            A=(1.D0-DSNA*DSNA*DCSB*DCSB)*SASQ-1.D0
            B=(1.D0-DSNA*DSNA*DSNB*DSNB)*SASQ-1.D0
            C=-2.D0*SASQ*DSNA*DSNA*DSNB*DCSB
            D= 2.D0*SASQ*SRSS*DSNA*DCSA*DCSB
            E= 2.D0*SASQ*SRSS*DSNA*DCSA*DSNB
            F=SSMO*(SSMO*DSNA*DSNA-DCSA*DCSA)
            TEM1=SQRT((2.D0*C*E-4.D0*B*D)**2-
     +                 4.D0*(C*C-4.D0*A*B)*(E*E-4.D0*B*F))
            TEM2=(4.D0*B*D-2.D0*C*E+TEM1)/(2.D0*(C*C-4.D0*A*B))
            TEM3=(4.D0*B*D-2.D0*C*E-TEM1)/(2.D0*(C*C-4.D0*A*B))
            UMIN=MIN(TEM2,TEM3)
            UMAX=MAX(TEM2,TEM3)
            TEM1=SQRT((2.D0*C*D-4.D0*A*E)**2-
     +                 4.D0*(C*C-4.D0*A*B)*(D*D-4.D0*A*F))
            TEM2=(4.D0*A*E-2.D0*C*D+TEM1)/(2.D0*(C*C-4.D0*A*B))
            TEM3=(4.D0*A*E-2.D0*C*D-TEM1)/(2.D0*(C*C-4.D0*A*B))
            VMIN=MIN(TEM2,TEM3)
            VMAX=MAX(TEM2,TEM3)
          ELSE
            UMIN=-SRSS*TAN(DTOR*60.D0)
            UMAX=+SRSS*TAN(DTOR*60.D0)
            VMIN=-SRSS*TAN(DTOR*60.D0)
            VMAX=+SRSS*TAN(DTOR*60.D0)
          END IF
        END IF
C
        GO TO 107
C
C Stereographic, Lambert equal area, and Gnomonic.
C
  102   UMIN=-2.D0
        UMAX=+2.D0
        VMIN=-2.D0
        VMAX=+2.D0
        GO TO 107
C
C Azimuthal equidistant and Mercator.
C
  103   UMIN=-PI
        UMAX=+PI
        VMIN=-PI
        VMAX=+PI
        GO TO 107
C
C Cylindrical equidistant.
C
  104   UMIN=-180.D0
        UMAX=+180.D0
        VMIN= -90.D0
        VMAX= +90.D0
        GO TO 107
C
C Mollweide.
C
  105   UMIN=-2.D0
        UMAX=+2.D0
        VMIN=-1.D0
        VMAX=+1.D0
        GO TO 107
C
C Robinson.
C
  106   UMIN=-1.0000D0
        UMAX=+1.0000D0
        VMIN= -.5072D0
        VMAX= +.5072D0
C
C Compute the quantity used by MAPIT in checking for crossover.  The
C USGS and conical projections are oddballs.
C
  107   IF (IPRJ.EQ.0) THEN
          IF (IPRF.EQ. 3.OR.IPRF.EQ. 4.OR.IPRF.EQ. 5.OR.IPRF.EQ. 7.OR.
     +        IPRF.EQ. 8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
     +        IPRF.EQ.19.OR.IPRF.EQ.21) THEN
            PEPS=270.D0
          ELSE IF (IPRF.EQ.9) THEN
            PEPS=.75D0*(VMAX-VMIN)
          ELSE IF (IPRF.EQ.20) THEN
            PEPS=.75D0*(UMAX-UMIN)
          ELSE IF (IPRF.EQ.22) THEN
            PEPS=.25D0*(UMAX-UMIN)
          ELSE
            PEPS=.75D0*(UMAX-UMIN)
          END IF
        ELSE IF (IPRJ.EQ.1) THEN
          PEPS=270.D0
        ELSE
          PEPS=.75D0*(UMAX-UMIN)
        END IF
C
C Turn off the initialization-required flag; this is done here, rather
C than at the end, because, otherwise, each of the following calls to
C MDPTRN will result in an unnecessary call to MDPIN1.
C
        INTF=.FALSE.
C
C Jump to the appropriate limit-setting code.
C
        GO TO (600,200,300,400,500,550) , ILTS
C
C ILTS=2    Points (PL1,PL2) and (PL3,PL4) are on opposite corners
C ------    of the plot.
C
  200   E=0.D0
  201   CALL MDPTRN (PLA1,PLA2+E,TEM1,TEM3)
        IF (ICFELL('MDPINT',2).NE.0) GO TO 999
        CALL MDPTRN (PLA3,PLA4-E,TEM2,TEM4)
        IF (ICFELL('MDPINT',3).NE.0) GO TO 999
        IF (IPRJ.GE.7.AND.TEM1.GE.TEM2.AND.E.EQ.0.D0) THEN
          E=.000001D0
          GO TO 201
        END IF
        UMIN=MIN(TEM1,TEM2)
        UMAX=MAX(TEM1,TEM2)
        VMIN=MIN(TEM3,TEM4)
        VMAX=MAX(TEM3,TEM4)
        IF (UMAX.GE.1.D12) GO TO 904
        GO TO 600
C
C ILTS=3    Four edge points are given.
C ------
C
  300   E=0.D0
  301   CALL MDPTRN (PLA1,PLB1+E,TEM1,TEM5)
        IF (ICFELL('MDPINT',4).NE.0) GO TO 999
        CALL MDPTRN (PLA2,PLB2-E,TEM2,TEM6)
        IF (ICFELL('MDPINT',5).NE.0) GO TO 999
        IF (IPRJ.GE.7.AND.TEM1.GE.TEM2.AND.E.EQ.0.D0) THEN
          E=.000001D0
          GO TO 301
        END IF
        CALL MDPTRN (PLA3,PLB3,TEM3,TEM7)
        IF (ICFELL('MDPINT',6).NE.0) GO TO 999
        CALL MDPTRN (PLA4,PLB4,TEM4,TEM8)
        IF (ICFELL('MDPINT',7).NE.0) GO TO 999
        UMIN=MIN(TEM1,TEM2,TEM3,TEM4)
        UMAX=MAX(TEM1,TEM2,TEM3,TEM4)
        VMIN=MIN(TEM5,TEM6,TEM7,TEM8)
        VMAX=MAX(TEM5,TEM6,TEM7,TEM8)
        IF (UMAX.GE.1.D12) GO TO 904
        GO TO 600
C
C ILTS=4    Angular distances are given.
C ------
C
  400   CUMI=COS(AUMN*DTOR)
        SUMI=SIN(AUMN*DTOR)
        CUMA=COS(AUMX*DTOR)
        SUMA=SIN(AUMX*DTOR)
        CVMI=COS(AVMN*DTOR)
        SVMI=SIN(AVMN*DTOR)
        CVMA=COS(AVMX*DTOR)
        SVMA=SIN(AVMX*DTOR)
C
C Projection: US  LC  ST  OR  LE  GN  AE  CE  ME  MO  RO
C
        GO TO (410,904,401,402,403,404,405,406,407,408,409,
     +                                     406,407,408,409) , IPRJ+1
C
C USGS transformations.
C
  410   UMIN=UUMN  !  ???
        UMAX=UUMX  !  ???
        VMIN=UVMN  !  ???
        VMAX=UVMX  !  ???
C
        GO TO 600
C
C Stereographic.
C
  401   IF (SUMI.LT..000001D0) THEN
          IF (CUMI.GT.0.D0) UMIN=0.D0
        ELSE
          UMIN=-(1.D0-CUMI)/SUMI
        END IF
        IF (SUMA.LT..000001D0) THEN
          IF (CUMA.GT.0.D0) UMAX=0.D0
        ELSE
          UMAX=(1.D0-CUMA)/SUMA
        END IF
        IF (SVMI.LT..000001D0) THEN
          IF (CVMI.GT.0.D0) VMIN=0.D0
        ELSE
          VMIN=-(1.D0-CVMI)/SVMI
        END IF
        IF (SVMA.LT..000001D0) THEN
          IF (CVMA.GT.0.D0) VMAX=0.D0
        ELSE
          VMAX=(1.D0-CVMA)/SVMA
        END IF
        GO TO 600
C
C Orthographic or satellite-view.
C
  402   IF (ABS(SALT).LE.1.D0) THEN
          IF (MAX(AUMN,AUMX,AVMN,AVMX).GT.90.D0) GO TO 902
          UMIN=-SUMI
          UMAX=+SUMA
          VMIN=-SVMI
          VMAX=+SVMA
        ELSE
          IF (MAX(AUMN,AUMX,AVMN,AVMX).GE.90.D0) GO TO 902
          UMIN=-SRSS*SUMI/CUMI
          UMAX=+SRSS*SUMA/CUMA
          VMIN=-SRSS*SVMI/CVMI
          VMAX=+SRSS*SVMA/CVMA
        END IF
        GO TO 600
C
C Lambert equal area.
C
  403   IF (SUMI.LT..000001D0) THEN
          IF (CUMI.GT.0.D0) UMIN=0.D0
        ELSE
          UMIN=-2.D0/SQRT(1.D0+((1.D0+CUMI)/SUMI)**2)
        END IF
        IF (SUMA.LT..000001D0) THEN
          IF (CUMA.GT.0.D0) UMAX=0.D0
        ELSE
          UMAX=2.D0/SQRT(1.D0+((1.D0+CUMA)/SUMA)**2)
        END IF
        IF (SVMI.LT..000001D0) THEN
          IF (CVMI.GT.0.D0) VMIN=0.D0
        ELSE
          VMIN=-2.D0/SQRT(1.D0+((1.D0+CVMI)/SVMI)**2)
        END IF
        IF (SVMA.LT..000001D0) THEN
          IF (CVMA.GT.0.D0) VMAX=0.D0
        ELSE
          VMAX=2.D0/SQRT(1.D0+((1.D0+CVMA)/SVMA)**2)
        END IF
        GO TO 600
C
C Gnomonic.
C
  404   IF (MAX(AUMN,AUMX,AVMN,AVMX).GE.89.999999D0) GO TO 902
        UMIN=-SUMI/CUMI
        UMAX=+SUMA/CUMA
        VMIN=-SVMI/CVMI
        VMAX=+SVMA/CVMA
        GO TO 600
C
C Azimuthal equidistant.
C
  405   UMIN=-AUMN*DTOR
        UMAX=+AUMX*DTOR
        VMIN=-AVMN*DTOR
        VMAX=+AVMX*DTOR
        GO TO 600
C
C Cylindrical equidistant.
C
  406   UMIN=-AUMN
        UMAX=+AUMX
        VMIN=-AVMN
        VMAX=+AVMX
        GO TO 600
C
C Mercator.
C
  407   IF (MAX(AVMN,AVMX).GE.89.999999D0) GO TO 902
        UMIN=-AUMN*DTOR
        UMAX=+AUMX*DTOR
        VMIN=-LOG((1.D0+SVMI)/CVMI)
        VMAX=+LOG((1.D0+SVMA)/CVMA)
        GO TO 600
C
C Mollweide.
C
  408   UMIN=-AUMN/90.D0
        UMAX=+AUMX/90.D0
        VMIN=-SVMI
        VMAX=+SVMA
        GO TO 600
C
C Robinson.
C
  409   UMIN=-AUMN/180.D0
        UMAX=+AUMX/180.D0
        VMIN=-RBGDFE(MAX(-90.D0,MIN(+90.D0,AVMN)))
        VMAX=+RBGDFE(MAX(-90.D0,MIN(+90.D0,AVMX)))
        GO TO 600
C
C ILTS=5    Values in the u/v plane are given.
C ------
C
  500   UMIN=PLA1
        UMAX=PLA2
        VMIN=PLA3
        VMAX=PLA4
        GO TO 600
C
C ILTS=6    Ranges of latitudes and longitudes are given, defining a
C ------    region of interest, all of which is to be shown.
C
  550   RLT1=MAX(-90.D0,MIN(90.D0,PLA1))
        RLT2=MAX(-90.D0,MIN(90.D0,PLA3))
        IF (RLT1.GT.RLT2) THEN
          RLAT=RLT1
          RLT1=RLT2
          RLT2=RLAT
        END IF
        RLN1=MOD(PLA2+3600.D0,360.D0)
        RLN2=MOD(PLA4+3600.D0,360.D0)
        IF (RLN2.LE.RLN1) RLN2=RLN2+360.D0
        NLTS=MAX(5,INT((RLT2-RLT1)/GRDR))+2
        NLNS=MAX(5,INT((RLN2-RLN1)/GRDR))+2
        XMIN=+1.D12
        XMAX=-1.D12
        YMIN=+1.D12
        YMAX=-1.D12
        DO 552 J=1,NLTS
          RLAT=RLT1+DBLE(J-1)*(RLT2-RLT1)/DBLE(NLTS-1)
          DO 551 I=1,NLNS
            RLON=RLN1+DBLE(I-1)*(RLN2-RLN1)/DBLE(NLNS-1)
            CALL MDPTRN (RLAT,RLON,XPOS,YPOS)
            IF (XPOS.NE.1.D12) THEN
              XMIN=MIN(XMIN,XPOS)
              XMAX=MAX(XMAX,XPOS)
              YMIN=MIN(YMIN,YPOS)
              YMAX=MAX(YMAX,YPOS)
            END IF
  551     CONTINUE
  552   CONTINUE
        IF (XMIN.NE.1.D12) THEN
          UMIN=XMIN
          UMAX=XMAX
          VMIN=YMIN
          VMAX=YMAX
        END IF
        GO TO 600
C
C Compute the width and height of the plot.
C
  600   DU=UMAX-UMIN
        DV=VMAX-VMIN
C
C Error if map has zero area.
C
        IF (DU.LE.0.D0.OR.DV.LE.0.D0) GO TO 903
C
C Position the map on the plotter frame.
C
        IF (DU/DV.LT.(XROW-XLOW)/(YTOW-YBOW)) THEN
          ULOW=.5D0*(XLOW+XROW)-.5D0*(DU/DV)*(YTOW-YBOW)
          UROW=.5D0*(XLOW+XROW)+.5D0*(DU/DV)*(YTOW-YBOW)
          VBOW=YBOW
          VTOW=YTOW
        ELSE
          ULOW=XLOW
          UROW=XROW
          VBOW=.5D0*(YBOW+YTOW)-.5D0*(DV/DU)*(XROW-XLOW)
          VTOW=.5D0*(YBOW+YTOW)+.5D0*(DV/DU)*(XROW-XLOW)
        END IF
C
C Error if map has essentially zero area.
C
        IF (MIN(UROW-ULOW,VTOW-VBOW)*PLTR.LT.RESL) GO TO 903
C
C Compute the quantities used by MAPIT to see if points are far enough
C apart to draw the line between them and the quantities used by MDPVP
C to determine the number of dots to interpolate between two points.
C
        DSCA=(UROW-ULOW)*PLTR/DU
        DPSQ=DPLT*DPLT
        DSSQ=DSCA*DSCA
        DBTD=DDTS/DSCA
C
C Set parameters required if an elliptical perimeter is being used.
C
        UCEN=.5D0*(UMIN+UMAX)
        VCEN=.5D0*(VMIN+VMAX)
        URNG=.5D0*(UMAX-UMIN)
        VRNG=.5D0*(VMAX-VMIN)
C
C Now, compute the latitude/longitude limits which will be required by
C various other routines.
C
C At first, assume the whole globe will be projected.
C
        SLAM=-90.D0
        BLAM=+90.D0
        SLOM=PHOC-180.D0
        BLOM=PHOC+180.D0
C
C Jump if it's obvious that really is the case.
C
        IF (JPRJ.EQ.0) GO TO 701  !  ???
C
        IF (ILTS.EQ.1.AND.(JPRJ.EQ.4.OR.JPRJ.EQ.6.OR.JPRJ.EQ.7.OR.
     +                               JPRJ.EQ.9.OR.JPRJ.EQ.10)) GO TO 701
C
C Otherwise, the whole globe is not being projected.  The first thing
C to do is to find a point (CLAT,CLON) whose projection is known to be
C on the map.  First, try the pole of the projection.
C
        CLAT=PHIA
        CLON=PHOC
        CALL MDPTRN (CLAT,CLON,U,V)
        IF (ICFELL('MDPINT',8).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +        (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0))
     +                                                         GO TO 611
C
C If that didn't work, try a point based on the limits specifier.
C
        IF (ILTS.EQ.2.OR.ILTS.EQ.6) THEN
          CLAT=.5D0*(PLA1+PLA3)
          CLON=.5D0*(PLA2+PLA4)
        ELSE IF (ILTS.EQ.3) THEN
          TEM1=MIN(PLA1,PLA2,PLA3,PLA4)
          TEM2=MAX(PLA1,PLA2,PLA3,PLA4)
          TEM3=MIN(PLB1,PLB2,PLB3,PLB4)
          TEM4=MAX(PLB1,PLB2,PLB3,PLB4)
          CLAT=.5D0*(TEM1+TEM2)
          CLON=.5D0*(TEM3+TEM4)
        ELSE
          GO TO 700
        END IF
        CALL MDPTRN (CLAT,CLON,U,V)
        IF (ICFELL('MDPINT',9).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +        (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0))
     +                                                         GO TO 611
        GO TO 700
C
C Once we have the latitudes and longitudes of a point on the map, we
C find the minimum and maximum latitude and the minimum and maximum
C longitude by running a search point about on a fine lat/lon grid.
C
C Find the minimum latitude.
C
  611   RLAT=CLAT
        RLON=CLON
        DLON=SRCH
  612   RLAT=RLAT-SRCH
        IF (RLAT.LE.-90.D0) GO TO 621
  613   CALL MDPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MDPINT',10).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +    (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0)) THEN
          DLON=SRCH
          GO TO 612
        END IF
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180.D0.AND.RLON.LT.CLON+180.D0) GO TO 613
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180.D0.AND.RLON.LT.CLON+180.D0) GO TO 613
        SLAM=RLAT
C
C Find the maximum latitude.
C
  621   RLAT=CLAT
        RLON=CLON
        DLON=SRCH
  622   RLAT=RLAT+SRCH
        IF (RLAT.GT.90.D0) GO TO 631
  623   CALL MDPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MDPINT',11).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +    (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0)) THEN
          DLON=SRCH
          GO TO 622
        END IF
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180.D0.AND.RLON.LT.CLON+180.D0) GO TO 623
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180.D0.AND.RLON.LT.CLON+180.D0) GO TO 623
        BLAM=RLAT
C
C Find the minimum longitude.
C
  631   RLAT=CLAT
        RLON=CLON
        DLAT=SRCH
  632   RLON=RLON-SRCH
        IF (RLON.LE.CLON-360.D0) GO TO 651
  633   CALL MDPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MDPINT',12).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +    (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0)) THEN
          DLAT=SRCH
          GO TO 632
        END IF
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90.D0.AND.RLAT.LT.90.D0) GO TO 633
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90.D0.AND.RLAT.LT.90.D0) GO TO 633
        SLOM=RLON+(-SIGN(180.D0,RLON+180.D0)+SIGN(180.D0,180.D0-RLON))
C
C Find the maximum longitude.
C
  641   RLAT=CLAT
        RLON=CLON
        DLAT=SRCH
  642   RLON=RLON+SRCH
        IF (RLON.GE.CLON+360.D0) GO TO 651
  643   CALL MDPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MDPINT',13).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +           (ELPF.AND.((U-UCEN)/URNG)**2+
     +                      ((V-VCEN)/VRNG)**2.LE.1.D0)) THEN
          DLAT=SRCH
          GO TO 642
        END IF
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90.D0.AND.RLAT.LT.90.D0) GO TO 643
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90.D0.AND.RLAT.LT.90.D0) GO TO 643
        BLOM=RLON+(-SIGN(180.D0,RLON+180.D0)+SIGN(180.D0,180.D0-RLON))
        IF (BLOM.LE.SLOM) BLOM=BLOM+360.D0
        GO TO 701
C
  651   SLOM=PHOC-180.D0
        BLOM=PHOC+180.D0
        GO TO 701
C
C Control comes here if we didn't succeed in setting limits properly.
C
  700   ISSL=0
        GO TO 702
C
C Control comes here if we did succeed in setting limits properly.
C
  701   ISSL=1
C
C Set the offset values and reset the minimum and maximum values.
C
  702   IF (ABS(UMIN-UMAX).LT.OTOL*ABS(UMIN+UMAX).OR.
     +      ABS(VMIN-VMAX).LT.OTOL*ABS(VMIN+VMAX)) THEN
          UOFF=UMIN
          VOFF=VMIN
          UMIN=0.
          VMIN=0.
          UMAX=UMAX-UOFF
          VMAX=VMAX-VOFF
        END IF
C
C Do the required SET call, being careful to use real numbers which are
C within the ranges specified by the double-precision numbers involved.
C
c       CALL SET (RDPNUW(ULOW),RDPNDW(UROW),RDPNUW(VBOW),RDPNDW(VTOW),
c    +            RDPNUW(UMIN),RDPNDW(UMAX),RDPNUW(VMIN),RDPNDW(VMAX),1)
c       IF (ICFELL('MDPINT',14).NE.0) GO TO 999
C
C Set all the variables in the common block passed to MDPTRA.
C
        ELPM=ELPF
        UMNM=UMIN-.000001D0*(UMAX-UMIN)
        UMXM=UMAX+.000001D0*(UMAX-UMIN)
        VMNM=VMIN-.000001D0*(VMAX-VMIN)
        VMXM=VMAX+.000001D0*(VMAX-VMIN)
        UCNM=UCEN
        VCNM=VCEN
        URNM=URNG
        VRNM=VRNG
C
C Done.
C
        RETURN
C
C Error returns.
C
  901   CALL SETER ('MDPINT - ATTEMPT TO USE NON-EXISTENT PROJECTION',
     +                                                           15,1)
        GO TO 999
C
  902   CALL SETER ('MDPINT - ANGULAR LIMITS TOO GREAT',16,1)
        GO TO 999
C
  903   CALL SETER ('MDPINT - MAP HAS ZERO AREA',17,1)
        GO TO 999
C
  904   CALL SETER ('MDPINT - MAP LIMITS INAPPROPRIATE',18,1)
        GO TO 999
C
  999   INTF=.TRUE.
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPPOS (ARG1,ARG2,ARG3,ARG4)
C
        DOUBLE PRECISION ARG1,ARG2,ARG3,ARG4
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPPOS - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check the arguments for errors.
C
        IF (ARG1.LT.0.D0.OR.ARG1.GE.ARG2.OR.ARG2.GT.1.D0) GO TO 901
        IF (ARG3.LT.0.D0.OR.ARG3.GE.ARG4.OR.ARG4.GT.1.D0) GO TO 901
C
C Transfer in the values.
C
        XLOW=ARG1
        XROW=ARG2
        YBOW=ARG3
        YTOW=ARG4
C
C Set the flag to indicate that initialization is now required.
C
        INTF=.TRUE.
C
C Done.
C
        RETURN
C
C Error exit.
C
  901   CALL SETER ('MDPPOS - ARGUMENTS ARE INCORRECT',2,1)
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPROJ (ARG1,ARG2,ARG3,ARG4)
C
        CHARACTER*(*)    ARG1
        DOUBLE PRECISION ARG2,ARG3,ARG4
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(12),
     +                   PDCL(12)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Declare local variables.
C
        INTEGER          I
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPROJ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the parameters defining the projection.
C
        I=IDICTL(ARG1,PDCT,12)
        IF (I.EQ.0) I=IDICTL(ARG1,PDCL,12)
        IF (I.EQ.0) GO TO 901
C
        JPRJ=I-1
C
        IF (JPRJ.EQ.3) THEN
          CALL MDSETD ('SA',0.D0)
          IF (ICFELL('MDPROJ',2).NE.0) RETURN
        ELSE IF (JPRJ.EQ.11) THEN
          JPRJ=3
          IF (ABS(SALT).LE.1.D0) THEN
            CALL MDSETD ('SA',6.631D0)
            IF (ICFELL('MDPROJ',3).NE.0) RETURN
          END IF
        END IF
C
        PHIA=MAX(-90.D0,MIN(90.D0,ARG2))
        PHIO=ARG3+(-SIGN(180.D0,ARG3+180.D0)+SIGN(180.D0,180.D0-ARG3))
        ROTA=ARG4+(-SIGN(180.D0,ARG4+180.D0)+SIGN(180.D0,180.D0-ARG4))
C
C Set the flag to indicate that initialization is now required.
C
        INTF=.TRUE.
C
C Done.
C
        RETURN
C
C Error exit.
C
  901   CALL MDPCEM ('MDPROJ - UNKNOWN PROJECTION NAME ',ARG1,4,1)
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPSET (ARG1,ARG2,ARG3,ARG4,ARG5)
C
        CHARACTER*(*)    ARG1
        DOUBLE PRECISION ARG2(2),ARG3(2),ARG4(2),ARG5(2)
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(12),
     +                   PDCL(12)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
C Declare local variables.
C
        INTEGER          I
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the parameters defining the map limits.
C
        I=IDICTL(ARG1,LDCT,6)
        IF (I.EQ.0) I=IDICTL(ARG1,LDCL,6)
        IF (I.EQ.0) GO TO 901
        ILTS=I
C
        PLA1=ARG2(1)
        PLA2=ARG3(1)
        PLA3=ARG4(1)
        PLA4=ARG5(1)
C
        IF (I.EQ.3) THEN
          PLB1=ARG2(2)
          PLB2=ARG3(2)
          PLB3=ARG4(2)
          PLB4=ARG5(2)
        END IF
C
C Set the flag to indicate that initialization is now required.
C
        INTF=.TRUE.
C
C Done.
C
        RETURN
C
C Error exit.
C
  901   CALL MDPCEM ('MDPSET - UNKNOWN MAP AREA SPECIFIER ',ARG1,2,1)
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPTRI (UVAL,VVAL,RLAT,RLON)
C
        DOUBLE PRECISION UVAL,VVAL,RLAT,RLON
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,PHOC,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,PHOC,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM6/  UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM,ELPM
        DOUBLE PRECISION UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM
        LOGICAL ELPM
        SAVE   /MAPCM6/
C
        COMMON /MAPCM8/  P,Q,R
        DOUBLE PRECISION P,Q,R
        SAVE   /MAPCM8/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Declare local variables.
C
        DOUBLE PRECISION ANGA,ANGU,RCOSA,RCOSB,RCOSLA,RCOSPH,RCOSU,
     +                   RSINA,RSINB,RSINLA,RSINPH,RSINU,TMP1,TMP2,
     +                   UTMP,UTM1,UTM2,UTM3,VTMP,VTM1,VTM2,VTM3,VVTM
C
        REAL             SLAT,SLON
C
C Declare function types.
C
        DOUBLE PRECISION RBGLEN,RBIDFE
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPTRI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPTRI',2).NE.0) RETURN
        END IF
C
C Check for a point outside the perimeter.  Return 1.D12's for such
C points.
C
        IF (ELPM) THEN
          IF (((UVAL-UCNM)/URNM)**2+
     +        ((VVAL-VCNM)/VRNM)**2.GT.1.000002D0) GO TO 301
        ELSE
          IF (UVAL.LT.UMNM.OR.UVAL.GT.UMXM.OR.
     +        VVAL.LT.VMNM.OR.VVAL.GT.VMXM) GO TO 301
        END IF
C
C The point is inside the perimeter.  Compute its actual coordinates.
C
        UTMP=UVAL+UOFF
        VTMP=VVAL+VOFF
C
C Jump to the proper piece of code, depending on the projection type.
C
C Projection: US  LC  ST  OR  LE  GN  AE  CE  ME  MO  RO
C
        GO TO (100,101,102,103,104,105,106,107,108,109,110,
     +                                     111,112,113,114) , IPRJ+1
C
C USGS transformations
C.
  100   RLAT=1.D12
        RLON=1.D12
        RETURN
C
c  100   IF (IROD.EQ.0) THEN
c          CALL MDUTIS (REAL(UTMP),REAL(VTMP),SLAT,SLON)
c          IF (SLAT.NE.1.E12) THEN
c            RLAT=DBLE(SLAT)
c            RLON=DBLE(SLON)
c          ELSE
c            RLAT=1.D12
c             RLON=1.D12
c          END IF
c        ELSE
c          CALL MDUTID (UTMP,VTMP,RLAT,RLON)
c        END IF
c        IF (RLAT.NE.1.D12) GO TO 201
c         RETURN
C     
C Lambert conformal conic.
C
  101   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) THEN
          RLAT=SINO*90.D0
          TMP1=0.D0
          TMP2=1.D0
        ELSE
          RLAT=SINO*(90.D0-RTDD*ATAN(R**(1.D0/COSO)))
          TMP1=UTMP/R
          TMP2=-SINO*VTMP/R
        END IF
        RLON=PHOC+RTOD*ATAN2(TMP1,TMP2)/COSO
        IF (ABS(RLON-PHOC).GT.180.D0) GO TO 301
        GO TO 201
C
C Stereographic.
C
  102   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        RSINB=(UTMP*COSR-VTMP*SINR)/R
        RCOSB=(UTMP*SINR+VTMP*COSR)/R
        RSINA=2.D0*R/(1.D0+R*R)
        RCOSA=(1.D0-R*R)/(1.D0+R*R)
        GO TO 199
C
C Orthographic or satellite-view (depending on the value of SALT).
C
  103   IF (ABS(SALT).LE.1.D0) THEN
          R=SQRT(UTMP*UTMP+VTMP*VTMP)
          IF (R.LT..000001D0) GO TO 198
          RSINB=(UTMP*COSR-VTMP*SINR)/R
          RCOSB=(UTMP*SINR+VTMP*COSR)/R
          IF (R.LE.1.D0) THEN
            RSINA=R
            RCOSA=SQRT(1.D0-RSINA*RSINA)
          ELSE
            IF (SALT.GE.0.D0.OR.R.GT.2.D0) GO TO 301
            RSINA=2.D0-R
            RCOSA=-SQRT(1.D0-RSINA*RSINA)
          END IF
          GO TO 199
        ELSE
          IF (ALFA.EQ.0.D0) THEN
            UTM1=UTMP
            VTM1=VTMP
          ELSE
            UTM3=UTMP*DCSB+VTMP*DSNB
            VTM3=VTMP*DCSB-UTMP*DSNB
            UTM2=SRSS*(SRSS*DSNA+UTM3*DCSA)/(SRSS*DCSA-UTM3*DSNA)
            IF ((SRSS*DCSA+UTM2*DSNA)/SRSS.LT..0001D0) GO TO 301
            VTM2=SRSS*VTM3/(SRSS*DCSA-UTM3*DSNA)
            UTM1=UTM2*DCSB-VTM2*DSNB
            VTM1=UTM2*DSNB+VTM2*DCSB
          END IF
          R=SQRT(UTM1*UTM1+VTM1*VTM1)
          IF (R.LT..000001D0) GO TO 198
          RSINB=(UTM1*COSR-VTM1*SINR)/R
          RCOSB=(UTM1*SINR+VTM1*COSR)/R
          IF (R.LE.1.D0) THEN
            RCOSA=(R*R*ABS(SALT)+SSMO*SQRT(1.D0-R*R))/(R*R+SSMO)
          ELSE
            IF (SALT.GE.0.D0.OR.R.GT.2.D0) GO TO 301
            R=2.D0-R
            RCOSA=(R*R*ABS(SALT)-SSMO*SQRT(1.D0-R*R))/(R*R+SSMO)
          END IF
          RCOSA=MAX(-1.D0,MIN(+1.D0,RCOSA))
          RSINA=SQRT(1.D0-RCOSA*RCOSA)
          GO TO 199
        END IF
C
C Lambert equal-area.
C
  104   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        IF (R.GT.2.D0) GO TO 301
        RSINB=(UTMP*COSR-VTMP*SINR)/R
        RCOSB=(UTMP*SINR+VTMP*COSR)/R
        RCOSA=MAX(-1.D0,MIN(+1.D0,1.D0-R*R/2.D0))
        RSINA=SQRT(1.D0-RCOSA*RCOSA)
        GO TO 199
C
C Gnomonic.
C
  105   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        RSINB=(UTMP*COSR-VTMP*SINR)/R
        RCOSB=(UTMP*SINR+VTMP*COSR)/R
        RCOSA=MAX(-1.D0,MIN(+1.D0,1.D0/SQRT(1.D0+R*R)))
        RSINA=SQRT(1.D0-RCOSA*RCOSA)
        GO TO 199
C
C Azimuthal equidistant.
C
  106   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        IF (R.GT.PI) GO TO 301
        RSINB=(UTMP*COSR-VTMP*SINR)/R
        RCOSB=(UTMP*SINR+VTMP*COSR)/R
        RCOSA=COS(R)
        RSINA=SQRT(1.D0-RCOSA*RCOSA)
        GO TO 199
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  107   IF (ABS(UTMP).GT.180.D0.OR.ABS(VTMP).GT.90.D0) GO TO 301
        ANGA=DTOR*(90.D0-VTMP)
        RSINA=SIN(ANGA)
        RCOSA=COS(ANGA)
        ANGU=DTOR*UTMP
        RSINU=SIN(ANGU)
        RCOSU=COS(ANGU)
        RSINB=RSINU*COSR+RCOSU*SINR
        RCOSB=RSINU*SINR-RCOSU*COSR
        GO TO 199
C
C Mercator, arbitrary pole and orientation.
C
  108   IF (ABS(UTMP).GT.PI) GO TO 301
        RSINA=SIN(PI-2.D0*ATAN(EXP(VTMP)))
        RCOSA=COS(PI-2.D0*ATAN(EXP(VTMP)))
        RSINU=SIN(UTMP)
        RCOSU=COS(UTMP)
        RSINB=RSINU*COSR+RCOSU*SINR
        RCOSB=RSINU*SINR-RCOSU*COSR
        GO TO 199
C
C Mollweide, arbitrary pole and orientation.
C
  109   IF (ABS(VTMP).GT.1.D0) GO TO 301
        RCOSA=VTMP
        RSINA=SQRT(1.D0-RCOSA*RCOSA)
        IF (RSINA.NE.0.D0) THEN
          IF (ABS(UTMP/RSINA).GT.2.D0) GO TO 301
          ANGU=PIOT*UTMP/RSINA
          RSINU=SIN(ANGU)
          RCOSU=COS(ANGU)
        ELSE
          IF (UTMP.NE.0.D0) GO TO 301
          RSINU=0.D0
          RCOSU=1.D0
        END IF
        RSINB=RSINU*COSR+RCOSU*SINR
        RCOSB=RSINU*SINR-RCOSU*COSR
        GO TO 199
C
C Robinson, arbitrary pole and orientation.
C
  110   IF (ABS(VTMP).GT..5072D0) GO TO 301
        VVTM=RBIDFE(VTMP)
        IF (ABS(UTMP).GT.RBGLEN(VVTM)) GO TO 301
        ANGA=PIOT-DTOR*VVTM
        RSINA=SIN(ANGA)
        RCOSA=COS(ANGA)
        ANGU=PI*UTMP/RBGLEN(VVTM)
        RSINU=SIN(ANGU)
        RCOSU=COS(ANGU)
        RSINB=RSINU*COSR+RCOSU*SINR
        RCOSB=RSINU*SINR-RCOSU*COSR
        GO TO 199
C
C Cylindrical equidistant, fast path.
C
  111   IF (ABS(UTMP).GT.180.D0.OR.ABS(VTMP).GT.90.D0) GO TO 301
        RLAT=VTMP
        RLON=PHOC+UTMP
        GO TO 200
C
C Mercator, fast path.
C
  112   IF (ABS(UTMP).GT.PI) GO TO 301
        RLAT=RTDD*ATAN(EXP(VTMP))-90.D0
        RLON=PHOC+RTOD*UTMP
        GO TO 200
C
C Mollweide, fast path.
C
  113   IF (ABS(VTMP).GT.1.D0) GO TO 301
        RLAT=ASIN(VTMP)*RTOD
        IF (1.D0-VTMP*VTMP.NE.0.D0) THEN
          RLON=PHOC+90.D0*UTMP/SQRT(1.D0-VTMP*VTMP)
          IF (ABS(RLON-PHOC).GT.180.D0) GO TO 301
        ELSE
          IF (UTMP.NE.0.D0) GO TO 301
          RLON=PHOC
        END IF
        GO TO 200
C
C Robinson, fast path.
C
  114   IF (ABS(VTMP).GT..5072D0) GO TO 301
        VVTM=RBIDFE(VTMP)
        IF (ABS(UTMP).GT.RBGLEN(VVTM)) GO TO 301
        RLAT=VVTM
        RLON=PHOC+180.D0*UTMP/RBGLEN(VVTM)
        GO TO 200
C
C The following code is common to all of the azimuthal projections when
C the "radius" R is within epsilon of zero.
C
  198   RSINB=0.D0
        RCOSB=1.D0
        RSINA=0.D0
        RCOSA=1.D0
C
C The following code is common to all of the azimuthal projections.
C
  199   RSINPH=RSINA*RSINB
        RCOSPH=RCOSA*COSO-RSINA*SINO*RCOSB
        RCOSLA=SQRT(RSINPH*RSINPH+RCOSPH*RCOSPH)
C
        IF (RCOSLA.NE.0.D0) THEN
          RSINPH=RSINPH/RCOSLA
          RCOSPH=RCOSPH/RCOSLA
        END IF
        IF (ABS(SINO).GT..000001D0) THEN
          RSINLA=(RCOSA-RCOSLA*RCOSPH*COSO)/SINO
        ELSE
          RSINLA=RSINA*RCOSB
        END IF
C
        IF (RSINLA.NE.0.D0.OR.RCOSLA.NE.0.D0) THEN
          RLAT=RTOD*ATAN2(RSINLA,RCOSLA)
        ELSE
          RLAT=0.D0
        END IF
        IF (RSINA*RSINB.NE.0.D0.OR.
     +      RCOSA*COSO-RSINA*SINO*RCOSB.NE.0.D0) THEN
          RLON=PHOC+RTOD*ATAN2(RSINA*RSINB,RCOSA*COSO-RSINA*SINO*RCOSB)
        ELSE
          RLON=0.D0
        END IF
C
        GO TO 201
C
C The following code is common to all the fast-path projections.  If the
C rotation angle is 180, negate the output values of RLAT and RLON.
C
  200   IF (ABS(ROTA).GT.179.999999D0) THEN
          RLAT=-RLAT
          RLON=-RLON
        END IF
C
        GO TO 201
C
C Done.
C
  201   IF (ABS(RLON).GT.180.D0) RLON=RLON-SIGN(360.D0,RLON)
C
        RETURN
C
C Inverse is not defined; return the values that signal that.
C
  301   RLAT=1.D12
        RLON=1.D12
C
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPTRN (RLAT,RLON,U,V)
C
        DOUBLE PRECISION RLAT,RLON,U,V
C
C Given the latitude, RLAT, and the longitude, RLON, of a point on the
C earth, this routine returns the U and V coordinates of the projection
C of that point on the map.
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,PHOC,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,PHOC,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM8/  P,Q,R
        DOUBLE PRECISION P,Q,R
        SAVE   /MAPCM8/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER          IPRF
        SAVE   /USGSC1/
C
C Declare local variables.
C
        DOUBLE PRECISION CHI,COSA,COSB,COSLA,COSPH,SINA,SINB,SINLA,
     +                   SINPH,TCOS,TEMP,TMP1,TMP2,UTM1,UTM2,UTM3,
     +                   VTM1,VTM2,VTM3
C
        REAL             USNG,VSNG
C
C Declare function types.
C
        DOUBLE PRECISION MDGDDP,RBGDFE,RBGLEN
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPTRN - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, there's a problem: MDPTRN can't call
C MDPINT, because MDPINT calls MDPTRN; instead, it calls the internal
C routine MDPIN1 to initialize just the constants that it needs.
C
        IF (INTF) CALL MDPIN1
C
C Set up U and V for the fast paths.  U is a longitude, in degrees,
C between -180. and +180., inclusive, and V is a latitude, in degrees.
C
        TEMP=RLON-PHOC
        U=TEMP+(-SIGN(180.D0,TEMP+180.D0)+SIGN(180.D0,180.D0-TEMP))
        V=MAX(-90.D0,MIN(90.D0,RLAT))
C
C If a fast-path projection is in use and the rotation angle is 180,
C adjust U and V.
C
        IF (IPRJ.GE.11.AND.ABS(ROTA).GT.179.999999D0) THEN
          U=-U
          V=-V
        END IF
C
C Take fast paths for simple cylindrical projections.
C
        IF (IPRJ-11) 100,197,113
C
C No fast path.  Sort out the USGS transformations and the Lambert
C conformal conic from the rest.
C
  100   IF (IPRJ-1) 101,102,103
C
C USGS projections.
C
c  101   IF (IPRF.EQ.0) GO TO 901
  101   GO TO 901
C
c        IF (IROD.EQ.0) THEN
c          CALL MDUTFS (REAL(RLAT),REAL(RLON),USNG,VSNG)
c          IF (USNG.NE.1.E12) THEN
c            U=DBLE(USNG)
c            V=DBLE(VSNG)
c          ELSE
c            U=1.D12
c            V=1.D12
c          END IF
c        ELSE
c          CALL MDUTFD (RLAT,RLON,U,V)
c        END IF
cC
c        IF (IPRF.EQ. 3.OR.IPRF.EQ. 4.OR.IPRF.EQ. 5.OR.IPRF.EQ. 7.OR.
c     +      IPRF.EQ. 8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
c     +      IPRF.EQ.19.OR.IPRF.EQ.21) THEN
c          TMP1=RLON-UTPA(5)
c          P=TMP1+(-SIGN(180.D0,TMP1+180.D0)+SIGN(180.D0,180.D0-TMP1))
c          IF ((IPRF.EQ.3.OR.IPRF.EQ.8).AND.ABS(RLAT).GT.89.999999D0)
c     +                                                         GO TO 200
c        ELSE IF (IPRF.EQ.9) THEN
c          P=V
c        ELSE IF (IPRF.EQ.11.OR.IPRF.EQ.12) THEN
c          IF (MDGDDP(RLAT,RLON,UTPA(6),UTPA(5)).GT.179.99D0) GO TO 200
c        ELSE IF (IPRF.EQ.20) THEN
c          P=U*SIN(DTOR*UTPA(4))+V*COS(DTOR*UTPA(4))
c        ELSE IF (IPRF.EQ.22) THEN
c          P=U*SIN(DTOR*UTPA(4))+V*COS(DTOR*UTPA(4))
c        END IF
cC
c        GO TO 199
C
C Lambert conformal conic.
C
  102   P=U
        CHI=90.D0-SINO*V
        IF (CHI.GT.179.999999D0) GO TO 200
        R=TAN(DTRH*CHI)**COSO
        U=U*COSO*DTOR
        V=-R*SINO*COS(U)
        U=R*SIN(U)
        GO TO 198
C
C Not Lambert conformal conic.  Calculate constants common to most of
C the other projections.
C
  103   TMP1=U*DTOR
        TMP2=V*DTOR
        SINPH=SIN(TMP1)
        SINLA=SIN(TMP2)
        COSPH=COS(TMP1)
        COSLA=COS(TMP2)
        TCOS=COSLA*COSPH
        COSA=MAX(-1.D0,MIN(+1.D0,SINLA*SINO+TCOS*COSO))
        SINA=SQRT(1.D0-COSA*COSA)
        IF (SINA.LT..000001D0) THEN
          SINA=0.D0
          IF (IPRJ.EQ.3.AND.ABS(SALT).GT.1.D0) THEN
            SINB=0.D0
            COSB=1.D0
            GO TO 105
          END IF
          IF (IPRJ.GE.7.OR.COSA.LT.0.D0) GO TO 200
          U=0.D0
          V=0.D0
          GO TO 197
        END IF
        SINB=COSLA*SINPH/SINA
        COSB=(SINLA*COSO-TCOS*SINO)/SINA
C
C Jump to code appropriate for the chosen projection.
C
C Projection: ST  OR  LE  GN  AE  CE  ME  MO  RO
C
        GO TO (104,105,106,107,108,109,110,111,112) , IPRJ-1
C
C Stereographic.
C
  104   IF (ABS(SINA).LT..000001D0) THEN
          R=SINA/2.D0
        ELSE
          R=(1.D0-COSA)/SINA
        END IF
        GO TO 196
C
C Orthographic or satellite-view, depending on the value of SALT.
C
  105   IF (ABS(SALT).LE.1.D0) THEN
          IF (COSA.GT.0.D0) THEN
            R=SINA
          ELSE
            IF (SALT.GE.0.D0) GO TO 200
            R=2.D0-SINA
          END IF
          GO TO 196
        ELSE
          IF (COSA.GT.1.D0/ABS(SALT)) THEN
            R=SRSS*SINA/(ABS(SALT)-COSA)
          ELSE
            IF (SALT.GE.0.D0) GO TO 200
            R=2.D0-SRSS*SINA/(ABS(SALT)-COSA)
          END IF
          IF (ALFA.EQ.0.D0) GO TO 196
          UTM1=R*(SINB*COSR+COSB*SINR)
          VTM1=R*(COSB*COSR-SINB*SINR)
          UTM2=UTM1*DCSB+VTM1*DSNB
          VTM2=VTM1*DCSB-UTM1*DSNB
          IF ((SRSS*DCSA+UTM2*DSNA)/SRSS.LT..0001D0) GO TO 200
          UTM3=SRSS*(UTM2*DCSA-SRSS*DSNA)/
     +                    (UTM2*DSNA+SRSS*DCSA)
          VTM3=SRSS*VTM2/(UTM2*DSNA+SRSS*DCSA)
          U=UTM3*DCSB-VTM3*DSNB
          V=VTM3*DCSB+UTM3*DSNB
          GO TO 197
        END IF
C
C Lambert equal area.
C
  106   IF (ABS(COSA+1.D0).LT..000001D0) GO TO 200
        R=(1.D0+COSA)/SINA
        R=2.D0/SQRT(1.D0+R*R)
        GO TO 196
C
C Gnomonic.
C
  107   IF (COSA.LE..000001D0) GO TO 200
        R=SINA/COSA
        GO TO 196
C
C Azimuthal equidistant.
C
  108   IF (ABS(COSA+1.D0).LT..000001D0) GO TO 200
        R=ACOS(COSA)
        GO TO 196
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  109   U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)*RTOD
        V=90.D0-ACOS(COSA)*RTOD
        GO TO 197
C
C Mercator, arbitrary pole and orientation.
C
  110   U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)
        V=LOG((1.D0+COSA)/SINA)
        GO TO 197
C
C Mollweide, arbitrary pole and orientation.
C
  111   P=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)*TOPI
        U=P*SINA
        V=COSA
        GO TO 198
C
C Robinson, arbitrary pole and orientation.
C
  112   P=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)*OOPI
        U=P*RBGLEN(90.D0-ACOS(COSA)*RTOD)
        V=RBGDFE(90.D0-ACOS(COSA)*RTOD)
        GO TO 198
C
C Fast-path cylindrical projections (with PLAT=0, ROTA=0 or 180).
C
  113   IF (IPRJ-13) 114,115,116
C
C Fast-path Mercator.
C
  114   IF (ABS(V).GT.89.999999D0) GO TO 200
        U=U*DTOR
        V=LOG(TAN((V+90.D0)*DTRH))
        GO TO 197
C
C Fast-path Mollweide.
C
  115   P=U/90.D0
        V=SIN(V*DTOR)
        U=P*SQRT(1.D0-V*V)
        GO TO 198
C
C Fast-path Robinson.
C
  116   P=U/180.D0
        U=P*RBGLEN(V)
        V=RBGDFE(V)
        GO TO 198
C
C Common terminal code for certain projections.
C
  196   U=R*(SINB*COSR+COSB*SINR)
        V=R*(COSB*COSR-SINB*SINR)
C
  197   P=U
C
  198   Q=V
C
C Normal exit.
C
  199   U=U-UOFF
        V=V-VOFF
C
        RETURN
C
C Projection of point is invisible or undefined.
C
  200   U=1.D12
        P=U
C
        RETURN
C
C Error exit.
C
  901   CALL SETER ('MDPTRN - USGS PROJECTION WAS NOT INITIALIZED',2,1)
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDSETD (WHCH,DVAL)
C
        CHARACTER*(*)    WHCH
        DOUBLE PRECISION DVAL
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM7/  ULOW,UROW,VBOW,VTOW
        DOUBLE PRECISION ULOW,UROW,VBOW,VTOW
        SAVE   /MAPCM7/
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        IF (ICFELL('MDSETD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        IF      (WHCH(1:2).EQ.'DD'.OR.WHCH(1:2).EQ.'dd') THEN
          DDTS=DVAL
          DBTD=DDTS/DSCA
        ELSE IF (WHCH(1:2).EQ.'GD'.OR.WHCH(1:2).EQ.'gd') THEN
          GRDR=MAX(.001D0,MIN(10.D0,DVAL))
        ELSE IF (WHCH(1:2).EQ.'GN'.OR.WHCH(1:2).EQ.'gn') THEN
          GRLO=DVAL
        ELSE IF (WHCH(1:2).EQ.'GP'.OR.WHCH(1:2).EQ.'gp') THEN
          GRPO=MAX(0.D0,MIN(90360.D0,DVAL))
        ELSE IF (WHCH(1:2).EQ.'GR'.OR.WHCH(1:2).EQ.'gr') THEN
          GRID=DVAL
        ELSE IF (WHCH(1:2).EQ.'GT'.OR.WHCH(1:2).EQ.'gt') THEN
          GRLA=DVAL
        ELSE IF (WHCH(1:2).EQ.'MV'.OR.WHCH(1:2).EQ.'mv') THEN
          DPLT=DVAL
          DPSQ=DPLT*DPLT
        ELSE IF (WHCH(1:2).EQ.'OT'.OR.WHCH(1:2).EQ.'ot') THEN
          OTOL=MAX(0.D0,DVAL)
        ELSE IF (WHCH(1:2).EQ.'RE'.OR.WHCH(1:2).EQ.'re') THEN
          PLTR=DVAL
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
          SALT=DVAL
          IF (ABS(SALT).GT.1.D0) THEN
            SSMO=SALT*SALT-1.D0
            SRSS=SQRT(SSMO)
          END IF
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'S1'.OR.WHCH(1:2).EQ.'s1') THEN
          ALFA=ABS(DVAL)
          DSNA=SIN(DTOR*ALFA)
          DCSA=COS(DTOR*ALFA)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'S2'.OR.WHCH(1:2).EQ.'s2') THEN
          BETA=DVAL
          DSNB=SIN(DTOR*BETA)
          DCSB=COS(DTOR*BETA)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'SR'.OR.WHCH(1:2).EQ.'sr') THEN
          SRCH=MAX(.001D0,MIN(10.D0,DVAL))
          INTF=.TRUE.
        ELSE
          GO TO 901
        END IF
C
C Done.
C
        RETURN
C
C Error exits.
C
  901   CALL MDPCEM ('MDSETD - UNKNOWN PARAMETER NAME ',WHCH,2,1)
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION RBGDFE (RLAT)
C
        DOUBLE PRECISION RLAT
C
C The value of RBGDFE(RLAT) is the signed distance, from the equator,
C of the parallel at latitude RLAT; its magnitude is a fraction of the
C length of the equator.
C
C Declare local variables.
C
        DOUBLE PRECISION FLAT,PDFE(19)
C
        INTEGER          ILAT
C
C Define the contents of PDFE per the definition of the Robinson
C projection.
C
        DATA PDFE( 1) / 0.0000D0 /  !   0N
        DATA PDFE( 2) / 0.0620D0 /  !   5N
        DATA PDFE( 3) / 0.1240D0 /  !  10N
        DATA PDFE( 4) / 0.1860D0 /  !  15N
        DATA PDFE( 5) / 0.2480D0 /  !  20N
        DATA PDFE( 6) / 0.3100D0 /  !  25N
        DATA PDFE( 7) / 0.3720D0 /  !  30N
        DATA PDFE( 8) / 0.4340D0 /  !  35N
        DATA PDFE( 9) / 0.4958D0 /  !  40N
        DATA PDFE(10) / 0.5571D0 /  !  45N
        DATA PDFE(11) / 0.6176D0 /  !  50N
        DATA PDFE(12) / 0.6769D0 /  !  55N
        DATA PDFE(13) / 0.7346D0 /  !  60N
        DATA PDFE(14) / 0.7903D0 /  !  65N
        DATA PDFE(15) / 0.8435D0 /  !  70N
        DATA PDFE(16) / 0.8936D0 /  !  75N
        DATA PDFE(17) / 0.9394D0 /  !  80N
        DATA PDFE(18) / 0.9761D0 /  !  85N
        DATA PDFE(19) / 1.0000D0 /  !  90N
C
C Determine where the parallel of interest lies relative to the ones
C represented in the tables (between the ones associated with elements
C ILAT and ILAT+1 and a fractional distance FLAT from the former to the
C latter).
C
        ILAT=MAX(1,MIN(18,INT(1.D0+ABS(RLAT)/5.D0)))
C
        FLAT=1.D0+ABS(RLAT)/5.D0-DBLE(ILAT)
C
C Return the desired value.
C
        RBGDFE=.5072D0*SIGN((1.D0-FLAT)*PDFE(ILAT)+
     +                             FLAT*PDFE(ILAT+1),RLAT)
C
C Done.
C
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION RBGLEN (RLAT)
C
        DOUBLE PRECISION RLAT
C
C The value of RBGLEN(RLAT) is the length of the parallel at latitude
C RLAT, stated as a fraction of the length of the equator.
C
C Declare local variables.
C
        DOUBLE PRECISION FLAT,PLEN(19)
C
        INTEGER          ILAT
C
C Define the contents of PLEN per the definition of the Robinson
C projection.
C
        DATA PLEN( 1) / 1.0000D0 /  !   0N
        DATA PLEN( 2) / 0.9986D0 /  !   5N
        DATA PLEN( 3) / 0.9954D0 /  !  10N
        DATA PLEN( 4) / 0.9900D0 /  !  15N
        DATA PLEN( 5) / 0.9822D0 /  !  20N
        DATA PLEN( 6) / 0.9730D0 /  !  25N
        DATA PLEN( 7) / 0.9600D0 /  !  30N
        DATA PLEN( 8) / 0.9427D0 /  !  35N
        DATA PLEN( 9) / 0.9216D0 /  !  40N
        DATA PLEN(10) / 0.8962D0 /  !  45N
        DATA PLEN(11) / 0.8679D0 /  !  50N
        DATA PLEN(12) / 0.8350D0 /  !  55N
        DATA PLEN(13) / 0.7986D0 /  !  60N
        DATA PLEN(14) / 0.7597D0 /  !  65N
        DATA PLEN(15) / 0.7186D0 /  !  70N
        DATA PLEN(16) / 0.6732D0 /  !  75N
        DATA PLEN(17) / 0.6213D0 /  !  80N
        DATA PLEN(18) / 0.5722D0 /  !  85N
        DATA PLEN(19) / 0.5322D0 /  !  90N
C
C Determine where the parallel of interest lies relative to the ones
C represented in the tables (between the ones associated with elements
C ILAT and ILAT+1 and a fractional distance FLAT from the former to the
C latter).
C
        ILAT=MAX(1,MIN(18,INT(1.D0+ABS(RLAT)/5.D0)))
C
        FLAT=1.D0+ABS(RLAT)/5.D0-DBLE(ILAT)
C
C Return the desired value.
C
        RBGLEN=(1.D0-FLAT)*PLEN(ILAT)+FLAT*PLEN(ILAT+1)
C
C Done.
C
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION RBIDFE (QDFE)
C
        DOUBLE PRECISION QDFE
C
C The value of RBIDFE(QDFE) is the latitude for which the distance of
C the parallel of the Robinson projection from the equator is QDFE.
C
C Declare local variables.
C
        DOUBLE PRECISION PDFE(19),XDFE
C
        INTEGER          ITST,IBEG,IEND
C
C Define the contents of PLEN per the definition of the Robinson
C projection.
C
C
        DATA PDFE( 1) / 0.0000D0 /  !   0N
        DATA PDFE( 2) / 0.0620D0 /  !   5N
        DATA PDFE( 3) / 0.1240D0 /  !  10N
        DATA PDFE( 4) / 0.1860D0 /  !  15N
        DATA PDFE( 5) / 0.2480D0 /  !  20N
        DATA PDFE( 6) / 0.3100D0 /  !  25N
        DATA PDFE( 7) / 0.3720D0 /  !  30N
        DATA PDFE( 8) / 0.4340D0 /  !  35N
        DATA PDFE( 9) / 0.4958D0 /  !  40N
        DATA PDFE(10) / 0.5571D0 /  !  45N
        DATA PDFE(11) / 0.6176D0 /  !  50N
        DATA PDFE(12) / 0.6769D0 /  !  55N
        DATA PDFE(13) / 0.7346D0 /  !  60N
        DATA PDFE(14) / 0.7903D0 /  !  65N
        DATA PDFE(15) / 0.8435D0 /  !  70N
        DATA PDFE(16) / 0.8936D0 /  !  75N
        DATA PDFE(17) / 0.9394D0 /  !  80N
        DATA PDFE(18) / 0.9761D0 /  !  85N
        DATA PDFE(19) / 1.0000D0 /  !  90N
C
C XDFE is the magnitude of QDFE, limited to the range of values in the
C table.
C
        XDFE=MAX(0.D0,MIN(1.D0,ABS(QDFE)/.5072D0))
C
C Find the indices of the values in the table between which XDFE lies.
C
        IBEG=1
        IEND=19
C
  101   ITST=(IBEG+IEND)/2
C
        IF (PDFE(ITST).LE.XDFE) THEN
          IBEG=ITST
        ELSE
          IEND=ITST
        END IF
C
        IF (IEND-IBEG.GT.1) GO TO 101
C
C Now, just interpolate to find the desired latitude.
C
        RBIDFE=SIGN(5.D0*(DBLE(IBEG-1)+(      XDFE-PDFE(IBEG))/
     +                                 (PDFE(IEND)-PDFE(IBEG))),QDFE)
C
C Done.
C
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION IDICTL (ISTR,IDCT,NDCT)
C
        CHARACTER*(*) ISTR
        CHARACTER*2   IDCT(NDCT)
        INTEGER       NDCT
C
C The value of this function is the index in the NDCT-element dictionary
C IDCT of the string ISTR.  Only the first two characters of ISTR and
C IDCT(I) are compared.  If ISTR is not found in the dictionary, the
C function value is zero.
C
C Declare local variables
C
        INTEGER       I
C
        DO 101 I=1,NDCT
          IF (ISTR(1:2).EQ.IDCT(I)) THEN
            IDICTL=I
            RETURN
          END IF
  101   CONTINUE
C
C Not found.  Return a zero.
C
        IDICTL=0
        RETURN
C
      END
C
C $Id: llmap.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPTRA (RLAT,RLON,UVAL,VVAL)
C
        DOUBLE PRECISION RLAT,RLON,UVAL,VVAL
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM6/  UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM,ELPM
        DOUBLE PRECISION UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM
        LOGICAL ELPM
        SAVE   /MAPCM6/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPTRA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPTRA',2).NE.0) RETURN
        END IF
C
C The call to MDPTRA is simply passed on to MDPTRN, but the values
C returned are checked to see if the point lies outside the perimeter;
C if so, the value 1.D12 is substituted for UVAL.
C
        CALL MDPTRN (RLAT,RLON,UVAL,VVAL)
        IF (ICFELL('MDPTRA',3).NE.0) RETURN
C
        IF (ELPM) THEN
          IF (((UVAL-UCNM)/URNM)**2+
     +        ((VVAL-VCNM)/VRNM)**2.GT.1.000002D0) THEN
            UVAL=1.D12
            VVAL=1.D12
          END IF
        ELSE
          IF (UVAL.LT.UMNM.OR.UVAL.GT.UMXM.OR.
     +        VVAL.LT.VMNM.OR.VVAL.GT.VMXM) THEN
            UVAL=1.D12
            VVAL=1.D12
          END IF
        END IF
C
        RETURN
C
      END
