C
C	$Id: csblda.f,v 1.3 2000-03-25 00:53:28 fred Exp $
C
      BLOCKDATA CSBLDA
C
C  Initialize data.
C
      include 'cscomn.h'
C
C  Number of nodes to use in least squares fit.
C
      DATA NUMLS/10/
C
C  Value for constant SIGMA.
C
      DATA USSIG/1.0D0/
C
C  Tolerance to use in calculating gradient differences to terminate
C  iteration sequence when computing global gradients.
C
      DATA TOLIC/0.01D0/
C
C  Tolerance to use in determining how close each SIGMA element 
C  should be to its optimum value in GETSIG.
C
      DATA TOLSG/0.01D0/
C
C  Fill value to use with NCL functions that can return a missing
C  value.
C
      DATA RMVAL/-8.D0/
C
C  Maximum number of iterations to use in computing SIGMA array.
C
      DATA NUMIT/10/
C
C  Flag to revert to calculating a SIGMA array rather than
C  use a constant SIGMA.  Using a constant SIGMA is effected
C  by setting a value for SIGME using CSSETR.  If ICSIG is 0,
C  then a SIGMA array is used, otherwise a constant SIGMA is used.
C
      DATA ICSIG/0/
C
C  Flag indicating global/local gradients.
C
      DATA IGFLG/1/
C
      END
