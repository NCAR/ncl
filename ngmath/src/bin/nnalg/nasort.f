C
C $Id: nasort.f,v 1.4 2008-07-27 03:11:53 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NASORT(XA,IP,N)                             
C
C  Given an array of REAL numbers of dimension N in XA,
C  this routine returns a permutation vector IP such that
C
C      XA(IP(I)) .LE. XA(IP(J))  
C           for all I,J such that  1. LE. I .LE. J .LE. N .
C
C  This is an implementation of the shellsort (the details of this
C  algorithm can be found in the book "Algorithms" by Robert Sedgewick).
C                                                                       
      INTEGER  H,I,N,IP(N),IP1,IP2,IPH,J,M
      REAL     XA(N)
C                                                                       
C  Check on the input argument N.
C                                                                       
      IF (N .LT. 0) THEN
        CALL SETER(' ASCSRT -- N TOO SMALL',1,1)
      ELSE IF (N .LE. 1) THEN
        IP(1) = 1
        RETURN
      ENDIF
C
C  Calculate the largest H less than N using the recursive definition 
C               H(L+1) = 3*H(L)+1
C
      H = 1
   10 CONTINUE
      H = 3*H+1
      IF (H .LT. N) GO TO 10
      H = (H-1)/3     
C                                                                       
C  Initialize permutation vector.
C                                                                       
      DO 20 I=1,N                                                    
      IP(I) = I                                                     
   20 CONTINUE
C                                                                       
C  Main loop.
C                                                                       
   30 CONTINUE
      M = N-H                                                       
      DO 50 J=1,M
      I = J                                                         
   40 CONTINUE
      IPH = I+H                                                  
      IP1 = IP(I)                                                
      IP2 = IP(IPH)                                              
      IF (XA(IP1) .LE. XA(IP2)) GO TO 50                      
C                                                                       
C  Interchange.
C                                                                       
      IP(I)   = IP2
      IP(IPH) = IP1
C                                                                       
C  Move interchanged elements up the line.
C                                                                       
      I = I-H                                                   
      IF (I .GE. 1) GO TO 40                                     
   50 CONTINUE
C                                                                       
C  Decrease H, continue sort as long as H is greater than 0.
C
      H = (H-1)/3                                               
      IF (H .LT. 1) RETURN                                          
      GO TO 30                                                         
      END                                                               
