C
C	$Id: csserr.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSERR (ISUB,IERR)                               
C
C SUBROUTINE CSSERR (ISUB,IERR)
C                                                                       
C PURPOSE        To print an error number and an error message.
C                                                                       
C USAGE          CALL CSSERR (ISUB,IERR)
C                                                                       
C ARGUMENTS                                                             
C ON INPUT       ISUB
C                  The subroutine producing the error.
C                IERR                                           
C                  The error number.
C                                                                       
C ARGUMENTS                                                             
C ON OUTPUT      None                                           
C                                                                       
C I/O            The message is writen to unit 6.             
C
C ******************************************************************    
C                                                                       
      CHARACTER *(*) ISUB
C
      IF (IERR .EQ. 1) THEN
        WRITE(6,100) ISUB
  100   FORMAT(A7,' - error 1, invalid number of input points (must be g
     +reater than 3).')
      ELSE IF (IERR .EQ. 2) THEN
        WRITE(6,110) ISUB
  110   FORMAT(A7,' - error 2, invalid dimension for latitudes in the ou
     +tput grid.')
      ELSE IF (IERR .EQ. 3) THEN
        WRITE(6,120) ISUB
  120   FORMAT(A7,' - error 3, invalid dimension for longitudes in the o
     +utput grid.')
      ELSE IF (IERR .EQ. 4) THEN
        WRITE(6,130) ISUB
  130   FORMAT(A7,' - error 4, first three nodes are collinear.')
      ELSE IF (IERR .EQ. 5) THEN
        WRITE(6,140) ISUB
  140   FORMAT(A7,' - error 5, extrapolation failed due to the uniform g
     +rid extending too far beyond the triangulation boundary.')
      ELSE IF (IERR .EQ. 6) THEN
        WRITE(6,150) ISUB
  150   FORMAT(A7,' - error 6, internal algorithm error - please report 
     +this.')
      ELSE IF (IERR .EQ. 7) THEN
        WRITE(6,160) ISUB
  160   FORMAT(A7,' - error 7, vertex of a triangle containing an interp
     +olation point is outside its valid range.')
      ELSE IF (IERR .EQ. 8) THEN
        WRITE(6,170) ISUB
  170   FORMAT(A7,' - error 8, the angular distance between an interpola
     +ted point and the nearest point of the triangulation is at least 9
     +0 degrees.')
      ELSE IF (IERR .EQ. 9) THEN
        WRITE(6,180) ISUB
  180   FORMAT(A7,' - error 9, not enough input points to calculate a gr
     +adient.')
      ELSE IF (IERR .EQ. 10) THEN
        WRITE(6,190) ISUB
  190   FORMAT(A7,' - error 10, insufficient space for the triangulation
     + (must be >= number of boundary nodes minus 2).')
      ELSE IF (IERR .EQ. 11) THEN
        WRITE(6,200) ISUB
  200   FORMAT(A7,' - error 11, degenerate triangle (two vertices lie on
     + same geodesic).')
      ELSE IF (IERR .EQ. 12) THEN
        WRITE(6,210) ISUB
  210   FORMAT(A7,' - error 12, duplicate input points.')
      ELSE IF (IERR .EQ. 13) THEN
        WRITE(6,220) ISUB
  220   FORMAT(A7,' - error 13, parameter TOL or NSG out of range.')
      ELSE IF (IERR .LT. 0) THEN
        NERR = -IERR
        WRITE(6,300) ISUB,IERR,NERR,NERR
  300   FORMAT(A7,' - error ',I5,', coordinates ',I5,' and M coincide fo
     +r some M  > ',I5,' >= 1 (coordinate numbering starting at 1).')
      ENDIF
C                                                                       
      RETURN                                                            
      END                                                               
