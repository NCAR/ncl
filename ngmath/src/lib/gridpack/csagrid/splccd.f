C PACKAGE SPLPAK         Documentation for user entries follows
C                        the general package information.
C
C LATEST REVISION        August, 1998
C
C PURPOSE                This package contains routines for fitting
C                        (least squares) a multidimensional cubic spline
C                        to arbitrarily located data.  It also contains
C                        routines for evaluating this spline (or its
C                        partial derivatives) at any point.
C
C                        Coefficient calculation is performed in
C                        subroutines SPLCC or SPLCW and evaluation is
C                        performed by functions SPLFE or SPLDE.
C
C USAGE                  Package SPLPAK contains four user entries --
C                        SPLCC, SPLCW, SPLFE, AND SPLDE.
C
C                        The user first calls SPLCC by
C
C                          CALL SPLCC (NDIM,XDATA,L1XDAT,YDATA,NDATA,
C                                      XMIN,XMAX,NODES,XTRAP,COEF,NCF,
C                                      WORK,NWRK,IERROR)
C
C                        or SPLCW by
C
C                          CALL SPLCW (NDIM,XDATA,L1XDATA,YDATA,WDATA,
C                                      NDATA,XMIN,XMAX,NODES,XTRAP,
C                                      COEF,NCF,WORK,NWRK,IERROR)
C
C                        The parameter NDATA in the call to SPLCW
C                        enables the user to weight some of the data
C                        points more heavily than others.  Both
C                        routines return a set of coefficients in the
C                        array COEF.  These coefficients are
C                        subsequently used in the computation of
C                        function values and partial derivatives.
C                        To compute values on the spline approximation
C                        the user then calls SPLFE or SPLDE any
C                        number of times in any order provided that
C                        the values of the inputs, NDIM, COEF, XMIN,
C                        XMAX, and NODES, are preserved between calls.
C
C*PL*ERROR* Comment line too long
C                        SPLFE and SPLDE are called in the following way:
C
C                          F = SPLFE (NDIM,X,COEF,XMIN,XMAX,NODES,
C                                     IERROR)
C
C                        or
C
C                          F = SPLDE (NDIM,X,NDERIV,COEF,XMIN,XMAX,
C                                     NODES,IERROR)
C
C                        The routine SPLFE returns an interpolated
C                        value at the point defined by the array X.
C                        SPLDE affords the user the additional
C                        capability of calculating an interpolated
C                        value for one of several partial derivatives
C                        specified by the array NDERIV.
C
C I/O                    None, except for error messages printed by
C                        calls to CFSARR, if an error is detected.
C
C PRECISION              Single
C
C REQUIRED LIBRARY       SUPRLS, CFAERR
C FILES
C
C LANGUAGE               FORTRAN
C
C HISTORY                Developed in 1972-73 by NCAR's
C                        Scientific Computing Division.
C
C                        Cleaned up and added to the Ngmath library in
C                        1998.
C
C PORTABILITY            FORTRAN 77
C
C***********************************************************************
C
C SUBROUTINE SPLCCD(NDIM,XDATA,L1XDAT,YDATA,NDATA,XMIN,XMAX,NODES,
C                   XTRAP,COEF,NCF,WORK,NWRK,IERROR)
C
C DIMENSION OF           XDATA(NDATA,L1XDAT),YDATA(NDATA),XMIN(NDIM),
C ARGUMENTS              XMAX(NDIM),NODES(NDIM),COEF(NCF),WORK(NWRK)
C
C PURPOSE                N-dimensional cubic spline coefficient
C                        calculation by least squares.
C
C USAGE                  The usage and arguments of this routine are
C                        identical to those for SPLCW except for the
C                        omission of the array of weights, WDATA.  See
C                        entry SPLCW description immediately below for a
C                        complete description.
C
C                        CALL SPLCCD(NDIM,XDATA,L1XDAT,YDATA,NDATA,XMIN,
C                                    XMAX,NODES,XTRAP,COEF,NCF,WORK,
C                                    NWRK,IERROR)
C
C***********************************************************************
C
C SUBROUTINE SPLCWD(NDIM,XDATA,L1XDAT,YDATA,WDATA,NDATA,XMIN,XMAX,
C                   NODES,XTRAP,COEF,NCF,WORK,NWRK,IERROR)
C
C
C DIMENSION OF           XDATA(L1XDAT,NDATA),YDATA(NDATA),WDATA(NDATA),
C ARGUMENTS              XMIN(NDIM),XMAX(NDIM),NODES(NDIM),COEF(NCF),
C                        WORK(NWRK)
C
C PURPOSE                N-dimensional cubic spline coefficient
C                        calculation by weighted least squares on
C                        arbitrarily located data.
C
C                        A grid of evenly spaced nodes in NDIM space is
C                        defined by the arguments XMIN, XMAX and NODES.
C                        A linear basis for the class of natural splines
C                        on these nodes is formed, and a set of
C                        corresponding coefficients is computed in the
C                        array COEF.  These coefficients are chosen to
C                        minimize the weighted sum of squared errors
C                        between the spline and the arbitrarily located
C                        data values described by the arguments XDATA,
C                        YDATA and NDATA.  The smoothness of the spline
C                        in data sparse areas is controlled by the
C                        argument XTRAP.
C
C NOTE                   In order to understand the arguments of this
C                        routine, one should realize that the node grid
C                        need not bear any particular relation to the
C                        data points.  In the theory of exact-fit
C                        interpolatory splines, the nodes would in fact
C                        be data locations, but in this case they serve
C                        only to define the class of splines from which
C                        the approximating function is chosen.  This
C                        node grid is a rectangular arrangement of
C                        points in NDIM space, with the restriction that
C                        along any coordinate direction the nodes are
C                        equally spaced.  The class of natural splines
C                        on this grid of nodes (NDIM-cubic splines whose
C                        2nd derivatives normal to the boundaries are 0)
C                        has as many degrees of freedom as the grid has
C                        nodes.  Thus the smoothness or flexibility of
C                        the splines is determined by the choice of the
C                        node grid.
C
C USAGE                  CALL SPLCW (NDIM,XDATA,L1XDAT,YDATA,WDATA,
C                                    NDATA,XMIN,XMAX,NODES,XTRAP,COEF,
C                                    NCF,WORK,NWRK,IERROR)
C
C                        The spline (or its derivatives) may then be
C                        evaluated by using function SPLFE (or SPLDE).
C
C ARGUMENTS
C
C ON INPUT               NDIM
C                          The dimensionality of the problem.  The
C                          spline is a function of NDIM variables or
C                          coordinates and thus a point in the
C                          independent variable space is an NDIM vector.
C                          NDIM must be in the range 1 .LE. NDIM .LE. 4.
C
C                        XDATA
C                          A collection of locations for the data
C                          values, i.e., points from the independent
C                          variable space.  This collection is a
C                          2-dimensional array whose 1st dimension
C                          indexes the NDIM coordinates of a given point
C                          and whose 2nd dimension labels the data
C                          point.  For example, the data point with
C                          label IDATA is located at the point
C                          (XDATA(1,IDATA),...,XDATA(NDIM,IDATA)) where
C                          the elements of this vector are the values of
C                          the NDIM coordinates.  The location, number
C                          and ordering of the data points is arbitrary.
C                          The dimension of XDATA is assumed to be
C                          XDATA(L1XDAT,NDATA).
C
C                        L1XDAT
C                          The length of the 1st dimension of XDATA in
C                          the calling program.  L1XDAT must be .GE.
C                          NDIM.
C
C                               NOTE:  For 1-dimensional problems L1XDAT
C                                      is usually 1.
C
C                        YDATA
C                          A collection of data values corresponding to
C                          the points in XDATA.  YDATA(IDATA) is the
C                          data value associated with the point
C                          (XDATA(1,IDATA),...,XDATA(NDIM,IDATA)) in the
C                          independent variable space.  The spline whose
C                          coefficients are computed by this routine
C                          approximates these data values in the least
C*PL*ERROR* Comment line too long
C                          squares sense.  The dimension is assumed to be
C                          YDATA(NDATA).
C
C                        WDATA
C                          A collection of weights.  WDATA(IDATA) is a
C                          weight associated with the data point
C                          labelled IDATA.  It should be non-negative,
C                          but may be of any magnitude.  The weights
C                          have the effect of forcing greater or lesser
C                          accuracy at a given point as follows: this
C                          routine chooses coefficients to minimize the
C                          sum over all data points of the quantity
C
C                            (WDATA(IDATA)*(YDATA(IDATA) - spline value
C                            at XDATA(IDATA)))**2.
C
C                          Thus, if the reliability
C                          of a data point is known to be low, the
C                          corresponding weight may be made small
C                          (relative to the other weights) so that the
C                          sum over all data points is affected less by
C                          discrepencies at the unreliable point.  Data
C                          points with zero weight are completely
C                          ignored.
C
C                               NOTE:  If WDATA(1) is .LT. 0, the other
C                                      elements of WDATA are not
C                                      referenced, and all weights are
C                                      assumed to be unity.
C
C                          The dimension is assumed to be WDATA(NDATA)
C                          unless WDATA(1) .LT. 0., in which case the
C                          dimension is assumed to be 1.
C
C                        NDATA
C                          The number of data points mentioned in the
C                          above arguments.
C
C                        XMIN
C                          A vector describing the lower extreme corner
C                          of the node grid.  A set of evenly spaced
C                          nodes is formed along each coordinate axis
C                          and XMIN(IDIM) is the location of the first
C                          node along the IDIM axis.  The dimension is
C                          assumed to be XMIN(NDIM).
C
C                        XMAX
C                          A vector describing the upper extreme corner
C                          of the node grid.  A set of evenly spaced
C                          nodes is formed along each coordinate axis
C                          and XMAX(IDIM) is the location of the last
C                          node along the IDIM axis.  The dimension is
C                          assumed to be XMAX(NDIM).
C
C                        NODES
C                          A vector of integers describing the number of
C                          nodes along each axis.  NODES(IDIM) is the
C                          number of nodes (counting endpoints) along
C                          the IDIM axis and determines the flexibility
C                          of the spline in that coordinate direction.
C                          NODES(IDIM) must be .GE. 4, but may be as
C                          large as the arrays COEF and WORK allow.
C                          The dimension is assumed to be NODES(NDIM).
C
C                          NOTE:  The node grid is completely defined by
C                                 the arguments XMIN, XMAX and NODES.
C                                 The spacing of this grid in the IDIM
C                                 coordinate direction is:
C
C                                   DX(IDIM) = (XMAX(IDIM)-XMIN(IDIM)) /
C                                              (NODES(IDIM)-1).
C
C                                 A node in this grid may be indexed by
C                                 an NDIM vector of integers
C                                 (IN(1),...,IN(NDIM)) where
C                                 1 .LE. IN(IDIM) .LE. NODES(IDIM).
C                                 The location of such a node may be
C                                 represented by an NDIM vector
C                                 (X(1),...,X(NDIM)) where
C                                 X(IDIM) = XMIN(IDIM) + (IN(IDIM)-1) *
C                                                         DX(IDIM).
C
C                        XTRAP
C                          A parameter to control extrapolation to data
C                          sparse areas.  The region described by XMIN
C                          and XMAX is divided into rectangles, the
C                          number of which is determined by NODES, and
C                          any rectangle containing a disproportionately
C                          small number of data points is considered to
C                          be data sparse (rectangle is used here to
C                          mean NDIM-dimensional rectangle).  If XTRAP
C                          is nonzero the least squares problem is
C                          augmented with derivative constraints in the
C                          data sparse areas to prevent the matrix from
C                          becoming poorly conditioned.  XTRAP serves as
C                          a weight for these constraints, and thus may
C                          be used to control smoothness in data sparse
C                          areas.  Experience indicates that unity is a
C                          good first guess for this parameter.
C
C                               NOTE:  If XTRAP is zero, substantial
C                                      portions of the routine will be
C                                      skipped, but a singular matrix
C                                      can result if large portions of
C                                      the region are without data.
C
C                        NCF
C                          The length of the array COEF in the calling
C                          program.  If NCF is .LT.
C                          NODES(1)*...*NODES(NDIM), a fatal error is
C                          diagnosed.
C
C                        WORK
C                          A workspace array for solving the least
C                          squares matrix generated by this routine.
C                          Its required size is a function of the total
C                          number of nodes in the node grid.  This
C                          total, NCOL = NODES(1)*...*NODES(NDIM), is
C                          also the number of columns in the least
C                          squares matrix.  The length of the array WORK
C                          must equal or exceed NCOL*(NCOL+1).
C
C                        NWRK
C                          The length of the array WORK in the calling
C                          program.  If
C                          NCOL = NODES(1)*...*NODES(NDIM) is the total
C                          number of nodes, then a fatal error is
C                          diagnosed if NWRK is less than
C                          NCOL*(NCOL+1).
C
C ON OUTPUT              COEF
C                          The array of coefficients computed by this
C                          routine.  Each coefficient corresponds to a
C                          particular basis function which in turn
C                          corresponds to a node in the node grid.  This
C                          correspondence between the node grid and the
C                          array COEF is as if COEF were an
C                          NDIM-dimensional Fortran array with
C                          dimensions NODES(1),...,NODES(NDIM), i.e., to
C                          store the array linearly, the leftmost
C                          indices are incremented most frequently.
C                          Hence the length of the COEF array must equal
C                          or exceed the total number of nodes, which is
C                          NODES(1)*...*NODES(NDIM).  The computed array
C                          COEF may be used with function SPLFE
C                          (or SPLDE) to evaluate the spline (or its
C                          derivatives) at an arbitrary point in NDIM
C*PL*ERROR* Comment line too long
C                          space.  The dimension is assumed to be COEF(NCF).
C
C                        WORK
C                          The workspace containing intermediate
C                          calculations.  It need not be saved.
C
C                        IERROR
C                          An error flag with the following meanings:
C                              0  No error.
C                            101  NDIM is .LT. 1 or is .GT. 4.
C                            102  NODES(IDIM) is .LT. 4 fOR some IDIM.
C                            103  XMIN(IDIM) = XMAX(IDIM) for some IDIM.
C                            104  NCF (size of COEF) is
C                                 .LT. NODES(1)*...*NODES(NDIM).
C                            105  NDATA is .LT. 1.
C                            106  NWRK (size of WORK) is too small.
C                            107  SUPRLS failure (usually insufficient
C                                 data) -- ordinarily occurs only if
C                                 XTRAP is zero or WDATA contains all
C                                 zeros.
C
C ALGORITHM              An overdetermined system of linear equations
C                        is formed -- one equation for each data point
C                        plus equations for derivative constraints.
C                        This system is solved using subroutine SUPRLSD.
C
C ACCURACY               If there is exactly one data point in the
C                        near vicinity of each node and no extra data,
C                        the resulting spline will agree with the
C                        data values to machine accuracy.  However, if
C                        the problem is overdetermined or the sparse
C                        data option is utilized, the accuracy is hard
C                        to predict.  Basically, smooth functions
C                        require fewer nodes than rough ones for the
C                        same accuracy.
C
C TIMING                 The execution time is roughly proportional
C                        to NDATA*NCOF**2 where NCOF = NODES(1)*...*
C                        NODES(NDIM).
C
C***********************************************************************
C
      SUBROUTINE SPLCCD(NDIM,XDATA,L1XDAT,YDATA,NDATA,XMIN,XMAX,NODES,
     +                  XTRAP,COEF,NCF,WORK,NWRK,IERROR)
      DOUBLE PRECISION XDATA
      DOUBLE PRECISION YDATA
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION XMAX
      DOUBLE PRECISION XTRAP
      DOUBLE PRECISION COEF
      DOUBLE PRECISION WORK
      DOUBLE PRECISION W
      DIMENSION XDATA(L1XDAT,NDATA),YDATA(NDATA),XMIN(NDIM),XMAX(NDIM),
     +          NODES(NDIM),COEF(NCF),WORK(NWRK)
      DIMENSION W(1)
      SAVE
C
      W(1) = -1.D0
      CALL SPLCWD(NDIM,XDATA,L1XDAT,YDATA,W,NDATA,XMIN,XMAX,NODES,XTRAP,
     +            COEF,NCF,WORK,NWRK,IERROR)
C
      RETURN
      END
