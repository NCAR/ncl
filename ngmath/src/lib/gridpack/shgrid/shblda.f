C
C	$Id: shblda.f,v 1.1 1999-08-19 21:18:09 fred Exp $
C
      BLOCKDATA SHBLDA
C
C  Initialize data.
C
      COMMON /SHCOMI/ NMLSTQ, NMINFL, NMCELS
C
C  Number of data points used in least squares fit must be initialized
C  at run time - set flag.
C
      DATA NMLSTQ/-1/
C
C  Number of nodes within the radius of influence must be initialized
C  at run time - set flag.
C
      DATA NMINFL/-1/
C
C  Number of rows, columns, planes in the cell grid must be initialized
C  at run time - set flag.
C
      DATA NMCELS/-1/
C
      END
