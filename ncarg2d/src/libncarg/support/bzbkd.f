C
C	$Id: bzbkd.f,v 1.2 1992-11-17 19:09:57 fred Exp $
C
      BLOCKDATA BZBKD
C
C  Data for the Bezier curve package.
C
      include 'bzcom.h'
C
C  FRATIO  --  The tolerance limit for the recursive subdivision
C              algorithm.  This limit specifies how close the
C              interpolated curve must be to the actual Bezier
C              curve before subdivision ceases.  This ratio is 
C              specified as a ratio of the maximum screen height
C              (the maximum Y extent in user space that can be mapped 
C              onto the unit interval in NDC space).  It is applied 
C              to the current user space to get a value in user 
C              coordinates.  As implemented the subdivision will 
C              cease after eight levels under any circumstance.
C  NPPC    --  A flag to indicate whether the recursive subdivision
C              algorithm will be overridden.  If NPPC .LT. 2 (the
C              default), then recursive subdivision will be utilized; 
C              if NPPC.GE.2 and NPPC.LE.128, then that many points 
C              will be returned along the curve at equally spaced 
C              values for the parameter in the parametric definition 
C              of the curve.
C      
      DATA FRATIO/.00003/
C
C  Flag for the number of points on a curve.
C
      DATA NPPC/0/
C
      END
