C
C	$Id: arblda.f,v 1.1.1.1 1992-04-17 22:32:10 ncargd Exp $
C
C ---------------------------------------------------------------------
C I N T R O D U C T I O N
C ---------------------------------------------------------------------
C
C This file contains a write-up, implementation instructions, and the
C code for a graphics package called AREAS.  Given a set of "edges"
C dividing the plane into separate areas, the package allows one to
C recover the polygons defining all of those areas.  The polygons may
C then be color-filled, shaded, or used as "shields" in the drawing
C of other objects.
C
C ---------------------------------------------------------------------
C W R I T E - U P
C ---------------------------------------------------------------------
C
C The basic notion of AREAS is very simple.  One has one or more groups
C of "edges".  Each group divides the user plane into areas.  Each edge
C is defined by an ordered set of points, a left identifier, which says
C what area is to the left of the edge, and a right identifier, which
C says what area is to the right of the edge.  One transmits the edges,
C one at a time, to AREAS, which digests all of the edge information and
C constructs from it what I have chosen to call an "area map".  Various
C things can then be done with the area map; for example, one may, on
C demand, retrieve the definitions of all the areas defined by it, and
C color-fill them.
C
C To use AREAS, one first executes the FORTRAN statement
C
C       CALL ARINAM (IAM,LAM)
C
C where IAM is an integer array, dimensioned at least LAM, in which the
C area map is to be constructed.  Then, for each edge, one executes the
C FORTRAN statement
C
C       CALL AREDAM (IAM,XCA,YCA,LCA,IGI,IDL,IDR)
C
C where IAM is the area-map array, XCA and YCA are real arrays holding
C the X and Y coordinates of the points defining the edge, LCA is the
C number of such coordinates, IGI is the identifier of the group to
C which the edge belongs, IDL is the identifier of the area which lies
C to the left of the edge, and IDR is the identifier of the area which
C lies to the right of the edge.  (Left and right are defined in the
C obvious way, given the ordering of the points defining the edge.)
C
C The X and Y coordinates are supplied in the current "user" coordinate
C system, as defined by the last call to the SPPS routine SET or by GKS
C calls defining the current window.  (Internally, AREAS uses integer
C coordinates in the range from 0 to 'LC', where 'LC' is an internal
C parameter; the default value is 1000000, but a user may choose to
C use some other value.)
C
C Group identifiers should be small positive integers.  Edges are
C organized into groups for a couple of reasons.  For example:  Group
C 1 might contain edges derived from an EZMAP projection of political
C boundaries and group 2 edges derived by putting contour lines through
C the same EZMAP projection; one can tell, for each area defined by the
C edges, to what political entity it belongs and between which pair of
C contour lines it lies.  Another example:  On some devices, there is
C an upper limit on the number of points that can be used to define a
C polygon to be color-filled.  If one's areas are intrinsically more
C complicated than this, one may overlay them with a group of edges
C defining, for example, a set of vertical strips.  This will have the
C effect of reducing the number of points defining each area.  (It also
C increases the run time of AREAS and the time required to draw the
C resulting plot, unfortunately.)
C
C The left and right area identifiers should also be small integers.
C Values may be assigned in any convenient way.  For example, the
C routine MAPLAM, which can be used to color EZMAP output, uses for
C each edge left and right area identifiers which are indices into a
C comprehensive list of global areas.  The contouring package CONPACK
C uses integers from which contour levels may be deduced.
C
C Negative and zero area identifiers have a special use.  An area with
C a negative area identifier is considered to be outside the region of
C interest.  Zero area identifiers are used when the real value is
C unknown or unimportant.  (For example, when using the technique
C mentioned above to reduce the largest number of points used to define
C a color-filled polygon, the area identifiers for the vertical strips
C can be zeroes.)
C
C For each group of edges, a perimeter is automatically supplied; it
C consists of the five points defining the largest possible rectangle,
C the points being given in clockwise order, with a left area identifier
C of -1 and a right area identifier of 0.
C
C Once all the edges have been inserted in the area map, the area map
C must be preprocessed.  This is done by means of the following FORTRAN
C statement:
C
C       CALL ARPRAM (IAM,IF1,IF2,IF3)
C
C where IAM is the area-map array.  IF1, IF2, and IF3 are flags saying
C what shortcuts are to be allowed in the preprocessing step (see the
C following paragraphs).  If such a call is not done by the user, it
C will be done by the package itself, but the values of the shortcut
C flags will be set in such a way as to take no shortcuts whatsoever,
C which may result in its running considerably less efficiently.
C
C ARPRAM first shortens overly long edge segments (those which are more
C than twice as long as the average) by interpolating points along
C their lengths; this leads to greater efficiency in executing other
C parts of the algorithm.
C
C Next, ARPRAM finds all intersections of edges with each other, which
C can be a rather time-consuming business.  If IF1 is set non-zero, a
C pair of edge segments is examined only if one of the pair has a left
C or right area identifier which is zero or negative.  This would be
C appropriate, for example, for contour lines, which are known not to
C intersect each other, but only to intersect the perimeter.  When
C intersections are found, points are interpolated along the edge
C segments which intersect.
C
C Next, ARPRAM searches for and removes "dangling" edges (those which
C do not contribute to enclosing any area); such edges are removed.
C If IF2 is non-zero, this process is skipped.  Again, this would be
C appropriate for contour lines, which are known not to have any such
C "dangling" edges, but not for EZMAP boundary lines.  (The EZMAP
C dataset contains, for example, islands which are formed from simple
C unclosed curves.)
C
C Next, ARPRAM adjusts area-identifier information in the area map.  It
C examines all the edge segments of each area in each group to see what
C area identifier should be assigned to the area.  If a -1 is found
C anywhere, the area is considered to have area identifier -1.  If only
C zeroes are found, the area is considered to have area identifier 0.
C If only non-negative area identifiers are found, the positive value
C most recently inserted in the area map is used.  In any case, all of
C the area identifiers for a given area are reset to the same value,
C so that all the information is consistent.  If IF3 is set non-zero,
C this process is greatly speeded up by omitting the consideration of
C "holes" in the areas examined.  Again, this is appropriate for contour
C lines, but not for EZMAP boundary lines.
C
C In every case, if an internal problem is detected which appears to
C indicate that a shortcut was taken inappropriately, ARPRAM will
C attempt to fix the problem by re-executing the code with shortcutting
C turned off.
C
C Once ARPRAM has been executed, either as a result of the user's call
C or as a result of AREAS detecting that it has not been done yet, the
C area map is ready to be used to generate graphical output.  One way
C to do this is to execute the FORTRAN statement
C
C       CALL ARSCAM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,APR)
C
C where IAM is the area-map array, XCS and YCS are real arrays of size
C MCS, IAI and IAG are integer arrays of size MAI, and APR is an area-
C processing routine supplied by the user; the name of APR must appear
C in an EXTERNAL statement in the routine which calls ARSCAM.  ARSCAM
C scans the area map from left to right, picking off, one by one, the
C areas defined by it and, for each one, calling the routine APR to
C process that area.  The routine APR must be supplied by the user and
C must have the following structure:
C
C       SUBROUTINE APR (XCS,YCS,NCS,IAI,IAG,NAI)
C         DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C         (CODE TO PROCESS THE AREA DEFINED BY XCS, YCS, IAI, AND IAG)
C
C         RETURN
C       END
C
C The arrays XCS and YCS will contain NCS normalized device coordinates
C (in the range from 0. to 1., inclusive) of the points defining the
C area.  The last point will be a duplicate of the first.  Holes in an
C area will be traced in such a way as to maximize the probability of
C hardware fill working properly (at least on the devices that I know
C about), using vertical lines to get to and from the holes and tracing
C them in the proper direction.  For each I from 1 to NAI, IAI(I) will
C be the area identifier for the area with respect to the group IAG(I).
C
C Before executing the first call to APR, the routine ARSCAM calls
C GETSET to retrieve the current user-system mapping parameters and
C then executes the statement
C
C       CALL SET (VPL,VPR,VPB,VPT,VPL,VPR,VPB,VPT,1)
C
C where VPL, VPR, VPB, and VPT are the parameters defining the current
C viewport, as defined by the last call to SET.  This ensures that, if
C the normalized device coordinates in XCS and YCS are used by APR in
C calls to such routines as GFA and FILL, the results will be correct.
C Of course, the routine APR may do its own SET call to achieve some
C other effect.  Before returning control to the caller, ARSCAM
C re-calls SET to restore the original mapping parameters.
C
C In the user-defined routine APR, the statement
C
C       CALL ARGETI ('DI - DIRECTION',IDI)
C
C will set IDI to 1 if the edge of the area is given in counterclockwise
C order (with the interior to the left) or to 2 if the edge of the area
C is given in clockwise order (with the interior to the right).  This
C information may be needed for some purposes.
C
C The routine ARGTAI may be used to get the area identifiers associated
C with a given point.  It is called using the following statement:
C
C       CALL ARGTAI (IAM,XCD,YCD,IAI,IAG,MAI,NAI,ICF)
C
C where IAM is the area-map array.  XCD and YCD are X and Y coordinates,
C in the current user coordinate system, of a point at which information
C is desired, and IAI and IAG are integer arrays, each dimensioned MAI,
C in which the information is to be returned.  Upon return from the
C call, NAI will have been set to the number of groups of areas in the
C area map and, for each I from 1 to NAI, IAI(I) will be an area
C identifier and IAG(I) the associated group identifier.  ICF is a flag
C set non-zero by the user to indicate that the definition of the user
C coordinate system has been changed since the last call to ARGTAI, in
C which case calls to GETSET must be executed by ARGTAI; if the flag is
C zero, it will be assumed that the information retrieved previously is
C still correct and that calls to GETSET may be skipped.
C
C The routine ARDRLN, given a polyline and an area map, breaks the
C polyline into pieces in such a way that each piece lies within one
C area.  For each such piece of the polyline, a user-specified routine
C is called.  It may draw (or not draw) each piece.  This may be used,
C for example, to limit the drawing of lines of latitude and longitude
C to areas over the ocean.  In the package CONPACK, it will be used to
C keep contour lines from passing through contour labels.  ARDRLN is
C called using the following statement:
C
C       CALL ARDRLN (IAM,XCD,YCD,NCD,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
C where IAM is the area-map array.  XCD and YCD are coordinate arrays,
C in the current user coordinate system, of the polyline.  NCD is the
C number of points defining the polyline.  XCS and YCS are coordinate
C arrays for ARDRLN to use in calls to LPR; each is dimensioned MCS.
C Similarly, IAI and IAG are integer arrays, each dimensioned MAI,
C in which the area identifiers and group identifiers are to be passed
C to LPR.  LPR is a user-supplied line-processing routine; it must be
C declared EXTERNAL in the routine calling ARDRLN.  For each piece of
C the polyline, LPR will be called by ARDRLN, using a statement like
C
C       CALL LPR (XCS,YCS,NCS,IAI,IAG,NAI)
C
C where XCS and YCS hold the normalized device coordinates of NCS points
C defining the piece of the original polyline and IAI and IAG hold NAI
C area-identifier/group-identifier pairs for the area within which that
C piece of the polyline lies.  The routine LPR, just like the routine
C APR mentioned above in connection with ARSCAM, may assume that a call
C to SET has been done to make the normalized device coordinates usable.
C
C AREAS has two internal parameters whose values may be set - 'LC' (for
C "Largest Coordinate") and 'AT' (for "Arithmetic Type").  (A third
C internal parameter, called 'DI' [for "DIrection"], has already been
C mentioned above in the discussion of the routine ARSCAM and is not
C to be set - its value is for retrieval only.)
C
C To reset the values of 'LC' and 'AT', use the calls
C
C       CALL ARSETI ('LC - LARGEST COORDINATE',desired_value_of_'LC')
C       CALL ARSETI ('AT - ARITHMETIC TYPE',desired_value_of_'AT')
C
C Only the first two characters of the first argument are examined; use
C of the remaining characters is recommended to make the code easier to
C read.
C
C X and Y coordinates in an area map are represented by integers in
C the range from 0 and 'LC', inclusive; the default value of 'LC' is
C 1000000.  Attempting to give 'LC' a value less than 1000 will give it
C the value 1000.  Obviously, the value of 'LC' must not be greater
C than the largest integer on the machine on which AREAS is running;
C its value must also be exactly representable as a real number on
C that machine.
C
C The value of 'AT' specifies what sort of arithmetic is to be used by
C AREAS, as follows:  'AT' less than or equal to 0 (the default value)
C allows AREAS to decide what sort of arithmetic to use.  'AT' = 1
C implies that real arithmetic is to be used.  'AT' = 2 implies that
C double precision arithmetic is to be used.  'AT' = 3 or greater
C implies that multiple precision integer arithmetic is to be used,
C in which case, if the given value is exactly equal to 3, AREAS will
C choose the base value to be used; if a greater value is used, it
C specifies the base value.  (For example, the value 100 would specify
C the use of base-100 integer arithmetic.)
C
C All calls to AREAS which operate on a particular area map must be
C done with the same values of 'LC' and 'AT'.  (Typically, if you
C change the values, you will do it once, at the beginning of your
C program, and will leave them unchanged thereafter.)  The value of
C 'LC' must be exactly representable as a real number on the machine
C on which AREAS is being run.  (In fact, it is a good idea to ensure
C that 10.*'LC' is exactly representable as a real number.)
C
C Using a non-zero value of 'AT' requires knowledge of the internals
C of AREAS and, for the moment, should only be done on recommendation
C of the author.  Roughly speaking, real arithmetic can be used if the
C number 'LC'**2 is exactly representable as a real on the machine on
C which AREAS is being run and double precision arithmetic can be used
C if that number is exactly representable as a double on the machine.
C Multiple precision integer arithmetic is quite a bit slower than real
C arithmetic and should only be used as a last resort (if, for example,
C your compiler will not handle double precision correctly).
C
C If you specify the values of 'LC' and/or 'AT' to be used, the given
C values will not be checked for correctness; it will be assumed that
C you know what you are doing.
C
C To recover the current values of internal parameters, use calls like
C
C       CALL ARGETI ('LC - LARGEST COORDINATE',ILC)
C       CALL ARGETI ('AT - ARITHMETIC TYPE',IAT)
C       CALL ARGETI ('DI - DIRECTION',IDI)
C
C In each case, the second argument is an integer variable which is
C to receive the current value of the internal parameter specified by
C the first two characters of the first argument.  Using 'AT' returns
C a value indicating what type of arithmetic is actually being used
C by AREAS.
C
C (08/25/91) I have added some debugging tools to AREAS.  First, there
C is a parameter 'DB', the default value of which is zero.  If it is set
C to a non-zero group identifier, it makes ARPRAM produce, at each of
C seven breakpoints, a plot showing all the edges in the area map that
C belong to group 'DB'.  Second, the routine that is called to produce
C the plots, which is called ARDBPA (for "AReas, DeBug, Plot Area map")
C may be called by a user program; the FORTRAN statement
C
C     CALL ARDBPA (IAM,IGI,LAB)
C
C produces a plot showing all the edges in the area map array IAM which
C belong to group IGI.  The character variable LAB specifies a label for
C the plot.
C
C (02/19/92) I have added a new parameter, called 'DC', for "Debug Color
C index".  The routine ARDBPA uses color indices 'DC' + 1 through 'DC'
C + 5.  The default value of 'DC' is 100, so that color indices 101
C through 105 are reset and used.  If this conflicts with your use of
C the color indices, you can change 'DC' to some other value.
C
C
C ---------------------------------------------------------------------
C I M P L E M E N T A T I O N   I N S T R U C T I O N S
C ---------------------------------------------------------------------
C
C How you go about implementing AREAS depends on which version of this
C file you have.  Look at the first two lines of the file.  If they are
C
C    .OP LS=10001 LI=1 CB RT ES=< ET=> OC UC=0 IF=2
C    .EL I
C
C you have an IFTRAN version of the file; otherwise, you have a FORTRAN
C version.
C
C The IFTRAN version of AREAS is more readable and, because of extensive
C parameterization, easier to change; unfortunately, one must have an
C IFTRAN preprocessor available in order to generate usable FORTRAN code
C from the IFTRAN code.
C
C To implement the IFTRAN version.
C -- --------- --- ------ -------
C
C To implement the IFTRAN version, review the IFTRAN commands in the
C rest of this section, making such changes as seem necessary for the
C target machine, and then run the resulting file through IFTRAN to
C obtain a FORTRAN file.
C
C To implement the FORTRAN version.
C -- --------- --- ------- -------
C
C The FORTRAN version should be portable FORTRAN 77 code and require no
C modifications to run.
C
C In the FORTRAN version, the section "IMPLEMENTATION INSTRUCTIONS"
C ends here; in the IFTRAN version, various preprocessor-time variables
C are set following this point.
C
C
C ---------------------------------------------------------------------
C C O D E
C ---------------------------------------------------------------------
C
C The BLOCK DATA routine ARBLDA
C --- ----- ---- ------- ------
C
      BLOCK DATA ARBLDA
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI
      SAVE   /ARCOMN/
C
C Below are descriptions of all the common variables and default values
C for those which require defaults.
C
C IAD is the type of arithmetic desired by the user, as follows:
C
C IAD=0 allows AREAS to decide what type of arithmetic to use.
C IAD=1 forces the use of real arithmetic.
C IAD=2 forces the use of double-precision arithmetic.
C IAD=3 forces the use of multiple-precision arithmetic.
C
      DATA IAD / 0 /
C
C IAU is the type of arithmetic actually chosen for use, either by the
C user or by AREAS itself, as follows:
C
C IAU=0 says that no choice has been made yet.
C IAU=1 specifies the use of real arithmetic.
C IAU=2 specifies the use of double-precision arithmetic.
C IAU=3 specifies the use of multiple-precision arithmetic.
C
      DATA IAU / 0 /
C
C ILC is the largest coordinate value to be used.  ARINIT sets RLC
C equal to REAL(ILC).
C
      DATA ILC / 1000000 /
C
C ILM is equal to ILC-1, RLM is equal to ILM, ILP is equal to ILC+1,
C and RLP is equal to ILP.  All of these values are set by ARINIT.
C
C IBS is the base for the multiple-precision arithmetic, when that type
C of arithmetic is selected.  Its value is set by ARINIT.  RBS is made
C equal to REAL(IBS) and DBS is made equal to DBLE(IBS).
C
      DATA IBS / 0 /
C
C IDB is the internal parameter 'IDB', which may be set non-zero by a
C user program to turn on the production of debug plots.
C
      DATA IDB / 0 /
C
C IDC is the internal parameter 'DC', which may be set by a user
C program to change the range of color indices used by ARDBPA.
C
      DATA IDC / 100 /
C
C IDI is the internal parameter 'DI', which may be retrieved in a user
C version of the routine "APR".  Its value will be 1 if the polygon to
C be processed is traced counter-clockwise (interior to the left), or a
C 2 if the polygon is traced clockwise (interior to the right).
C
      DATA IDI / 0 /
C
      END
