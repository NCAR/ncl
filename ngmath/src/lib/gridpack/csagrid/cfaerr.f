C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
C SUBROUTINE CFAERR (IERR,MESS,LMESS)                                   
C                                                                       
C PURPOSE        To print an error number and an error message  
C                or just an error message.                      
C                                                                       
C USAGE          CALL CFAERR (IERR,MESS,LMESS)                  
C                                                                       
C ARGUMENTS                                                             
C ON INPUT       IERR                                           
C                  The error number (printed only if non-zero). 
C                                                               
C                MESS                                           
C                  Message to be printed.                       
C                                                               
C                LMESS                                          
C                  Number of characters in mess (.LE. 130).     
C                                                                       
C ARGUMENTS                                                             
C ON OUTPUT      None                                           
C                                                                       
C I/O            The message is writen to unit 6.             
C
C ******************************************************************    
C
      SUBROUTINE CFAERR (IERR,MESS,LMESS)                               
C                                                                       
      CHARACTER *(*) MESS
C                                                                       
      IF (IERR .NE. 0) WRITE (6,'(A,I5)') ' IERR=', IERR
      WRITE (6,'(A)') MESS(1:LMESS)
C
      RETURN                                                            
      END                                                               
