C
C	$Id: shcomn.h,v 1.1 1999-08-19 21:18:09 fred Exp $
C
C  SHCOMI contains the values for all settable INTEGER parameters:
C
C    Var.    Name  Def.
C    ----    ----  ----------------------------------------------------------
C    NMLSTQ  NLS   Number of data points used in least squares fit.
C    NMINFL  NFL   Number of nodes within the radius of influence.
C    NMCELS  NCL   Number of rows, columns, planes in the cell grid.
C
      COMMON /SHCOMI/ NMLSTQ, NMINFL, NMCELS
