C
C $Id: mapbd.f,v 1.31 2008-09-18 12:31:28 kennison Exp $
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
        PARAMETER (MNAI=8000)
C
C The common block MAPCM0 contains mathematical constants.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
C The common block MAPCM1 contains transformation constants.
C
        COMMON /MAPCM1/  COSO,COSR,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,SINO,SINR
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
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
C The common block MAPCM5 contains various lists ("dictionaries") of
C two-character codes required by EZMAP for parameter-setting.
C
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(19),
     +                   PDCL(19)
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
C The common block MAPCMW contains variables affecting the definitions
C of the equirectangular, cylindrical equal-area, and Winkel tripel
C projections.
C
        COMMON /MAPCMW/  CSLS,CSLT,SLTD,ISLT
        DOUBLE PRECISION CSLS,CSLT,SLTD
        INTEGER ISLT
        SAVE  /MAPCMW/
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
        CHARACTER*64     NAME
        CHARACTER*512    FLNS
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
C COS1 is the cosine of 1 degree; DTOR is a conversion constant to go
C from degrees to radians; DTRH is a conversion constant to go from
C degrees to radians, halved; OOPI is one over pi; PI is pi; PIOT is
C pi over two; PIOF is pi over four; RTDD is a conversion constant to go
C from radians to degrees, doubled; RTOD is a conversion constant to go
C from radians to degrees; SIN1 is the sine of 1 degree; SROT is the
C square root of two; TOPI is two over pi; and TSRT is two times the
C square root of two.
C
        DATA COS1 / .999847695156390D0 /
        DATA DTOR / .017453292519943D0 /
        DATA DTRH / .008726646259971D0 /
        DATA OOPI / .318309886183790D0 /
        DATA PI   / 3.14159265358979D0 /
        DATA PIOT / 1.57079632679489D0 /
        DATA PIOF / .785398163397445D0 /
        DATA RTDD / 114.591559026165D0 /
        DATA RTOD / 57.2957795130823D0 /
        DATA SIN1 / .017452406437283D0 /
        DATA SROT / 1.41421356237310D0 /
        DATA TOPI / .636619772367581D0 /
        DATA TSRT / 2.82842712474619D0 /
C
C Variables in MAPCM1:
C
C IPRJ is an integer between 0 and 25, specifying what projection is
C currently in use.  The following table defines the meanings of the
C various possible values of IPRJ.  In the table, the word "arbitrary"
C is used to indicate the the projection may be centered at any point
C on the globe and the word "fast-path" is used to indicate that the
C projection is centered at (0,0) on the globe (which usually speeds
C up the evaluation of the equations).  Projections 16 through 24 are
C fast-path versions of projections 7 through 15.
C
C     IPRJ    Projection Selected                         Type
C     ----    ------------------------------------        -----------
C       0     (UT) USGS transformations                   Various 
C       1     (LC) Lambert Conformal                      Conical 
C       2     (ST) Stereographic                          Azimuthal
C       3     (OR or SA) Orthographic or Satellite        Azimuthal
C       4     (LE) Lambert Equal-Area                     Azimuthal
C       5     (GN) Gnomonic                               Azimuthal
C       6     (AE) Azimuthal Equidistant                  Azimuthal
C       7     (CE) Cylindrical Equidistant (arbitrary)    Cylindrical
C       8     (ME) Mercator (arbitrary)                   Cylindrical
C       9     (MT) Mollweide-type (arbitrary)             Cylindrical
C      10     (RO) Robinson (arbitrary)                   Cylindrical
C      11     (EA) Cylindrical Equal-Area (arbitrary)     Cylindrical
C      12     (AI) Aitoff (arbitrary)                     Mixed
C      13     (HA) Hammer (arbitrary)                     Mixed
C      14     (TM) True Mollweide (arbitrary)             Mixed
C      15     (WT) Winkel tripel (arbitrary)              Mixed
C      16     (CE) Cylindrical Equidistant (fast-path)    Cylindrical
C      17     (ME) Mercator (fast-path)                   Cylindrical
C      18     (MT) Mollweide-type (fast-path)             Cylindrical
C      19     (RO) Robinson (fast-path)                   Cylindrical
C      20     (EA) Cylindrical Equal-Area (fast-path)     Cylindrical
C      21     (AI) Aitoff (fast-path)                     Mixed
C      22     (HA) Hammer (fast-path)                     Mixed
C      23     (MO) True Mollweide (fast-path)             Mixed
C      24     (WT) Winkel tripel (fast-path)              Mixed
C      25     (RM) Rotated Mercator (fast-path)           Cylindrical
C
C IROD is a flag which, if non-zero, says that we have to use double
C precision at selected points.  SINO, COSO, SINR, and COSR are
C projection variables computed by MDPINT for use by MDPTRN.
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
C blowing up when PDRE is set prior to the first call to MDPINT.
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
C JPRJ is an integer between 0 and 13 indicating the type of projection
C currently in use.  PLTO, PLNO, and ROTA are the pole latitude and
C longitude and the rotation angle specified by the last user call to
C MAPROJ.  ILTS is an integer between 1 and 6, specifying how the limits
C of the map are to be chosen.  PLA1-4 and PLB1-4 are the values given
C by the user for PLM1(1), PLM2(1), ..., PLM1(2), PLM2(2), ..., in the
C last call to MAPSET.  PDRE is the plotter resolution - effectively,
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
      DATA INTF,JPRJ,PLTO,PLNO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,PLB1,PLB2 /
     1   .TRUE.,   7,0.D0,0.D0,0.D0,   1,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0 /
C
      DATA PLB3,PLB4,    PDRE, GRID, IDSH,IDOT, LBLF , PRMF ,  ELPF  /
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
     +        / 'UT','LC','ST','OR','LE','GN','AE','CE','ME','MT',
     +          'RO','EA','AI','HA','MO','WT','SV','ER','RM' /
      DATA PDCL
     +        / 'ut','lc','st','or','le','gn','ae','ce','me','mt',
     +          'ro','ea','ai','ha','mo','wt','sv','er','rm' /
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
C to prevent code in MDSETI and MDSETR from blowing up when PDRE is
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
C outline.  These values are relative to the "plotter resolution" PDRE;
C DPLT/PDRE is a fraction of the plotter frame.  DSCA is the ratio of
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
C Variables in MAPCMW:
C
C SLTD is a "standard latitude" parameter affecting the behavior of
C three projections: the equirectangular, the cylindrical equal-area,
C and the Winkel tripel.  CSLT is the cosine of SLTD and CSLS is the
C square of CSLT.  When IPRJ is 7, ISLT is 0 if the cylindrical
C equidistant projection is to be used or 1 if the equirectangular
C projection is to be used.
C
      DATA CSLS,CSLT,SLTD,ISLT / 1.D0,1.D0,-1.D0,0 /
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
C to points just outside the limb on the projected view.
C
      DATA SALT,ALFA,BETA / 0.D0,0.D0,0.D0 /
C
      DATA DSNA,DCSA,DSNB,DCSB / 0.D0,1.D0,0.D0,1.D0 /
C
      END
CNOSPLIT
      BLOCKDATA MAPBDQ
C
C The common block MAQCMN contains only those variables that are needed
C for MDQTRA, MDQTRI, and MDQTRN to carry out the transformation that
C was in effect at the time MDQINI was called.  All of them are copies
C of variables in other common blocks and are described above, in the
C commenting for BLOCKDATA MAPBDX.  They are given default values here
C mostly to protect the transformation routines from blowing up if they
C are erroneously called prior to a call to MDQINI.
C
        COMMON /MAQCMN/  ALFA,COSO,COSR,CSLS,CSLT,DCSA,DCSB,DSNA,DSNB,
     +                   DTOR,DTRH,OOPI,PLNO,  PI,PIOT,ROTA,RTDD,RTOD,
     +                   SALT,SINO,SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,
     +                   UOFF,URNM,VCNM,VMNM,VMXM,VOFF,VRNM,UTPA,IPRF,
     +                   IPRJ,IROD,ELPM
        DOUBLE PRECISION ALFA,COSO,COSR,CSLS,CSLT,DCSA,DCSB,DSNA,DSNB,
     +                   DTOR,DTRH,OOPI,PLNO,  PI,PIOT,ROTA,RTDD,RTOD,
     +                   SALT,SINO,SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,
     +                   UOFF,URNM,VCNM,VMNM,VMXM,VOFF,VRNM,UTPA(15)
C
        INTEGER IPRF,IPRJ,IROD
C
        LOGICAL ELPM
C
        SAVE   /MAQCMN/
C
        DATA ALFA / 0.D0 /
        DATA COSO / 1.D0 /
        DATA COSR / 1.D0 /
        DATA CSLS / 1.D0 /
        DATA CSLT / 1.D0 /
        DATA DCSA / 1.D0 /
        DATA DCSB / 1.D0 /
        DATA DSNA / 0.D0 /
        DATA DSNB / 0.D0 /
        DATA DTOR / .017453292519943D0 /
        DATA DTRH / .008726646259971D0 /
        DATA ELPM / .FALSE. /
        DATA IPRJ / 16 /
        DATA IROD / 1 /
        DATA OOPI / .318309886183790D0 /
        DATA PLNO / 0.D0 /
        DATA PI   / 3.14159265358979D0 /
        DATA PIOT / 1.57079632679489D0 /
        DATA ROTA / 0.D0 /
        DATA RTDD / 114.591559026165D0 /
        DATA RTOD / 57.2957795130823D0 /
        DATA SALT / 0.D0 /
        DATA SINO / 0.D0 /
        DATA SINR / 0.D0 /
        DATA SRSS / 0.D0 /
        DATA SSMO / 0.D0 /
        DATA TOPI / .636619772367581D0 /
        DATA UCNM / 0.D0 /
        DATA UMNM / -180.000360D0 /
        DATA UMXM / +180.000360D0 /
        DATA UOFF / 0.D0 /
        DATA URNM / 0.D0 /
        DATA VCNM / 0.D0 /
        DATA VMNM / -90.000180D0 /
        DATA VMXM / +90.000180D0 /
        DATA VOFF / 0.D0 /
        DATA VRNM / 0.D0 /
C
        DATA IPRF / 0 /
C
        DATA UTPA / 15*0.D0 /
C
      END
