C
C	$Id: r1mach.F,v 1.1 1994-08-11 16:29:57 haley Exp $
C
      REAL FUNCTION R1MACH (I)  
C
C***BEGIN PROLOGUE  R1MACH      
C***PURPOSE  Return floating point machine dependent constants. 
C***LIBRARY   SLATEC    
C***CATEGORY  R1
C***TYPE      SINGLE PRECISION (R1MACH-S, D1MACH-D)     
C***KEYWORDS  MACHINE CONSTANTS 
C***AUTHOR  Fox, P. A., (Bell Labs)     
C           Hall, A. D., (Bell Labs)    
C           Schryer, N. L., (Bell Labs) 
C***DESCRIPTION 
C       
C   R1MACH can be used to obtain machine-dependent parameters for the   
C   local machine environment.  It is a function subprogram with one    
C   (input) argument, and can be referenced as follows: 
C       
C        A = R1MACH(I)  
C       
C   where I=1,...,5.  The (output) value of A above is determined by    
C   the (input) value of I.  The results for various values of I are    
C   discussed below.    
C       
C   R1MACH(1) = B**(EMIN-1), the smallest positive magnitude.   
C   R1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.   
C   R1MACH(3) = B**(-T), the smallest relative spacing. 
C   R1MACH(4) = B**(1-T), the largest relative spacing. 
C   R1MACH(5) = LOG10(B)
C       
C   Assume single precision numbers are represented in the T-digit,     
C   base-B form 
C       
C              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )     
C       
C   where 0 .LE. X(I) .LT. B for I=1,...,T, 0 .LT. X(1), and    
C   EMIN .LE. E .LE. EMAX.      
C       
C   The values of B, T, EMIN and EMAX are provided in I1MACH as 
C   follows:    
C   I1MACH(10) = B, the base.   
C   I1MACH(11) = T, the number of base-B digits.
C   I1MACH(12) = EMIN, the smallest exponent E. 
C   I1MACH(13) = EMAX, the largest exponent E.  
C       
C   To alter this function for a particular environment, the desired    
C   set of DATA statements should be activated by removing the C from   
C   column 1.  Also, the values of R1MACH(1) - R1MACH(4) should be      
C   checked for consistency with the local operating system.    
C       
C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for  
C                 a portable library, ACM Transactions on Mathematical  
C                 Software 4, 2 (June 1978), pp. 177-188.       
C       
      INTEGER SMALL
      INTEGER LARGE
      INTEGER RIGHT
      INTEGER DIVER
      INTEGER LOG10
C       
      REAL RMACH(5)     
      SAVE RMACH
C       
      EQUIVALENCE (RMACH(1),SMALL)   
      EQUIVALENCE (RMACH(2),LARGE)   
      EQUIVALENCE (RMACH(3),RIGHT)   
      EQUIVALENCE (RMACH(4),DIVER)   
      EQUIVALENCE (RMACH(5),LOG10)   
C       
C     MACHINE CONSTANTS FOR THE AMIGA   
C     ABSOFT FORTRAN COMPILER USING THE 68020/68881 COMPILER OPTION     
C       
C     DATA SMALL / Z'00800000' /     
C     DATA LARGE / Z'7F7FFFFF' /     
C     DATA RIGHT / Z'33800000' /     
C     DATA DIVER / Z'34000000' /     
C     DATA LOG10 / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE AMIGA   
C     ABSOFT FORTRAN COMPILER USING SOFTWARE FLOATING POINT     
C       
C     DATA SMALL / Z'00800000' /     
C     DATA LARGE / Z'7EFFFFFF' /     
C     DATA RIGHT / Z'33800000' /     
C     DATA DIVER / Z'34000000' /     
C     DATA LOG10 / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE APOLLO  
C       
C     DATA SMALL / 16#00800000 /     
C     DATA LARGE / 16#7FFFFFFF /     
C     DATA RIGHT / 16#33800000 /     
C     DATA DIVER / 16#34000000 /     
C     DATA LOG10 / 16#3E9A209B /     
C       
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM   
C       
C     DATA RMACH(1) / Z400800000 /      
C     DATA RMACH(2) / Z5FFFFFFFF /      
C     DATA RMACH(3) / Z4E9800000 /      
C     DATA RMACH(4) / Z4EA800000 /      
C     DATA RMACH(5) / Z500E730E8 /      
C       
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS
C       
C     DATA RMACH(1) / O1771000000000000 /       
C     DATA RMACH(2) / O0777777777777777 /       
C     DATA RMACH(3) / O1311000000000000 /       
C     DATA RMACH(4) / O1301000000000000 /       
C     DATA RMACH(5) / O1157163034761675 /       
C       
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE 
C       
C     DATA RMACH(1) / Z"3001800000000000" /     
C     DATA RMACH(2) / Z"4FFEFFFFFFFFFFFE" /     
C     DATA RMACH(3) / Z"3FD2800000000000" /     
C     DATA RMACH(4) / Z"3FD3800000000000" /     
C     DATA RMACH(5) / Z"3FFF9A209A84FBCF" /     
C       
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES    
C       
C     DATA RMACH(1) / 00564000000000000000B /   
C     DATA RMACH(2) / 37767777777777777776B /   
C     DATA RMACH(3) / 16414000000000000000B /   
C     DATA RMACH(4) / 16424000000000000000B /   
C     DATA RMACH(5) / 17164642023241175720B /   
C       
C     MACHINE CONSTANTS FOR THE CELERITY C1260  
C       
C     DATA SMALL / Z'00800000' /     
C     DATA LARGE / Z'7F7FFFFF' /     
C     DATA RIGHT / Z'33800000' /     
C     DATA DIVER / Z'34000000' /     
C     DATA LOG10 / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE CONVEX  
C     USING THE -fn COMPILER OPTION     
C       
C     DATA RMACH(1) / Z'00800000' /     
C     DATA RMACH(2) / Z'7FFFFFFF' /     
C     DATA RMACH(3) / Z'34800000' /     
C     DATA RMACH(4) / Z'35000000' /     
C     DATA RMACH(5) / Z'3F9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE CONVEX  
C     USING THE -fi COMPILER OPTION     
C       
C     DATA RMACH(1) / Z'00800000' /     
C     DATA RMACH(2) / Z'7F7FFFFF' /     
C     DATA RMACH(3) / Z'33800000' /     
C     DATA RMACH(4) / Z'34000000' /     
C     DATA RMACH(5) / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE CONVEX  
C     USING THE -p8 OR -pd8 COMPILER OPTION     
C       
C     DATA RMACH(1) / Z'0010000000000000' /     
C     DATA RMACH(2) / Z'7FFFFFFFFFFFFFFF' /     
C     DATA RMACH(3) / Z'3CC0000000000000' /     
C     DATA RMACH(4) / Z'3CD0000000000000' /     
C     DATA RMACH(5) / Z'3FF34413509F79FF' /     
C       
C     MACHINE CONSTANTS FOR THE CRAY    
C       
C     DATA RMACH(1) / 200034000000000000000B /  
C     DATA RMACH(2) / 577767777777777777776B /  
C     DATA RMACH(3) / 377224000000000000000B /  
C     DATA RMACH(4) / 377234000000000000000B /  
C     DATA RMACH(5) / 377774642023241175720B /  
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200      
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -      
C     STATIC RMACH(5)   
C       
C     DATA SMALL /    20K,       0 /    
C     DATA LARGE / 77777K, 177777K /    
C     DATA RIGHT / 35420K,       0 /    
C     DATA DIVER / 36020K,       0 /    
C     DATA LOG10 / 40423K,  42023K /    
C       
C     MACHINE CONSTANTS FOR THE DEC ALPHA       
C     USING G_FLOAT     
C       
C     DATA RMACH(1) / '00000080'X /     
C     DATA RMACH(2) / 'FFFF7FFF'X /     
C     DATA RMACH(3) / '00003480'X /     
C     DATA RMACH(4) / '00003500'X /     
C     DATA RMACH(5) / '209B3F9A'X /     
C       
C     MACHINE CONSTANTS FOR THE DEC ALPHA       
C     USING IEEE_FLOAT  
C       
C     DATA RMACH(1) / '00800000'X /     
C     DATA RMACH(2) / '7F7FFFFF'X /     
C     DATA RMACH(3) / '33800000'X /     
C     DATA RMACH(4) / '34000000'X /     
C     DATA RMACH(5) / '3E9A209B'X /     
C       
C     MACHINE CONSTANTS FOR THE DEC RISC
C       
C     DATA RMACH(1) / Z'00800000' /     
C     DATA RMACH(2) / Z'7F7FFFFF' /     
C     DATA RMACH(3) / Z'33800000' /     
C     DATA RMACH(4) / Z'34000000' /     
C     DATA RMACH(5) / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE DEC VAX 
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)    
C     THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS 
C     THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS  
C       
C     DATA SMALL /       128 /       
C     DATA LARGE /    -32769 /       
C     DATA RIGHT /     13440 /       
C     DATA DIVER /     13568 /       
C     DATA LOG10 / 547045274 /       
C       
C     DATA SMALL / Z00000080 /       
C     DATA LARGE / ZFFFF7FFF /       
C     DATA RIGHT / Z00003480 /       
C     DATA DIVER / Z00003500 /       
C     DATA LOG10 / Z209B3F9A /       
C       
C     MACHINE CONSTANTS FOR THE ELXSI 6400      
C     (ASSUMING REAL*4 IS THE DEFAULT REAL)     
C       
C     DATA SMALL / '00800000'X /     
C     DATA LARGE / '7F7FFFFF'X /     
C     DATA RIGHT / '33800000'X /     
C     DATA DIVER / '34000000'X /     
C     DATA LOG10 / '3E9A209B'X /     
C       
C     MACHINE CONSTANTS FOR THE HARRIS 220      
C       
C     DATA SMALL, SMALL(2) / '20000000, '00000201 /  
C     DATA LARGE, LARGE(2) / '37777777, '00000177 /  
C     DATA RIGHT, RIGHT(2) / '20000000, '00000352 /  
C     DATA DIVER, DIVER(2) / '20000000, '00000353 /  
C     DATA LOG10, LOG10(2) / '23210115, '00000377 /  
C       
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES       
C       
C     DATA RMACH(1) / O402400000000 /   
C     DATA RMACH(2) / O376777777777 /   
C     DATA RMACH(3) / O714400000000 /   
C     DATA RMACH(4) / O716400000000 /   
C     DATA RMACH(5) / O776464202324 /   
C       
C     MACHINE CONSTANTS FOR THE HP 730  
C       
C     DATA RMACH(1) / Z'00800000' /     
C     DATA RMACH(2) / Z'7F7FFFFF' /     
C     DATA RMACH(3) / Z'33800000' /     
C     DATA RMACH(4) / Z'34000000' /     
C     DATA RMACH(5) / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE HP 2100 
C     3 WORD DOUBLE PRECISION WITH FTN4 
C       
C     DATA SMALL, SMALL(2) / 40000B,       1 /       
C     DATA LARGE, LARGE(2) / 77777B, 177776B /       
C     DATA RIGHT, RIGHT(2) / 40000B,    325B /       
C     DATA DIVER, DIVER(2) / 40000B,    327B /       
C     DATA LOG10, LOG10(2) / 46420B,  46777B /       
C       
C     MACHINE CONSTANTS FOR THE HP 2100 
C     4 WORD DOUBLE PRECISION WITH FTN4 
C       
C     DATA SMALL, SMALL(2) / 40000B,       1 /       
C     DATA LARGE, LARGE(2) / 77777B, 177776B /       
C     DATA RIGHT, RIGHT(2) / 40000B,    325B /       
C     DATA DIVER, DIVER(2) / 40000B,    327B /       
C     DATA LOG10, LOG10(2) / 46420B,  46777B /       
C       
C     MACHINE CONSTANTS FOR THE HP 9000 
C       
C     DATA SMALL / 00004000000B /    
C     DATA LARGE / 17677777777B /    
C     DATA RIGHT / 06340000000B /    
C     DATA DIVER / 06400000000B /    
C     DATA LOG10 / 07646420233B /    
C       
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,     
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86  AND 
C     THE PERKIN ELMER (INTERDATA) 7/32.
C       
C     DATA RMACH(1) / Z00100000 /       
C     DATA RMACH(2) / Z7FFFFFFF /       
C     DATA RMACH(3) / Z3B100000 /       
C     DATA RMACH(4) / Z3C100000 /       
C     DATA RMACH(5) / Z41134413 /       
C       
C     MACHINE CONSTANTS FOR THE IBM PC  
C       
C     DATA SMALL / 1.18E-38      /   
C     DATA LARGE / 3.40E+38      /   
C     DATA RIGHT / 0.595E-07     /   
C     DATA DIVER / 1.19E-07      /   
C     DATA LOG10 / 0.30102999566 /   
C       
C     MACHINE CONSTANTS FOR THE IBM RS 6000     
C       
C     DATA RMACH(1) / Z'00800000' /     
C     DATA RMACH(2) / Z'7F7FFFFF' /     
C     DATA RMACH(3) / Z'33800000' /     
C     DATA RMACH(4) / Z'34000000' /     
C     DATA RMACH(5) / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE INTEL i860      
C       
C     DATA RMACH(1) / Z'00800000' /     
C     DATA RMACH(2) / Z'7F7FFFFF' /     
C     DATA RMACH(3) / Z'33800000' /     
C     DATA RMACH(4) / Z'34000000' /     
C     DATA RMACH(5) / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING   
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL). 
C       
C     DATA SMALL /    8388608 /      
C     DATA LARGE / 2147483647 /      
C     DATA RIGHT /  880803840 /      
C     DATA DIVER /  889192448 /      
C     DATA LOG10 / 1067065499 /      
C       
C     DATA RMACH(1) / O00040000000 /    
C     DATA RMACH(2) / O17777777777 /    
C     DATA RMACH(3) / O06440000000 /    
C     DATA RMACH(4) / O06500000000 /    
C     DATA RMACH(5) / O07746420233 /    
C       
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING   
C     16-BIT INTEGERS  (EXPRESSED IN INTEGER AND OCTAL).
C       
C     DATA SMALL, SMALL(2) /   128,     0 /  
C     DATA LARGE, LARGE(2) / 32767,    -1 /  
C     DATA RIGHT, RIGHT(2) / 13440,     0 /  
C     DATA DIVER, DIVER(2) / 13568,     0 /  
C     DATA LOG10, LOG10(2) / 16282,  8347 /  
C       
C     DATA SMALL, SMALL(2) / O000200, O000000 /      
C     DATA LARGE, LARGE(2) / O077777, O177777 /      
C     DATA RIGHT, RIGHT(2) / O032200, O000000 /      
C     DATA DIVER, DIVER(2) / O032400, O000000 /      
C     DATA LOG10, LOG10(2) / O037632, O020233 /      
C       
C     MACHINE CONSTANTS FOR THE SGI-IRIS
C       
C     DATA RMACH(1) / Z'00800000' /     
C     DATA RMACH(2) / Z'7F7FFFFF' /     
C     DATA RMACH(3) / Z'33800000' /     
C     DATA RMACH(4) / Z'34000000' /     
C     DATA RMACH(5) / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE SUN     
C       
C     DATA RMACH(1) / Z'00800000' /     
C     DATA RMACH(2) / Z'7F7FFFFF' /     
C     DATA RMACH(3) / Z'33800000' /     
C     DATA RMACH(4) / Z'34000000' /     
C     DATA RMACH(5) / Z'3E9A209B' /     
C       
C     MACHINE CONSTANTS FOR THE SUN     
C     USING THE -r8 COMPILER OPTION     
C       
C     DATA RMACH(1) / Z'0010000000000000' /     
C     DATA RMACH(2) / Z'7FEFFFFFFFFFFFFF' /     
C     DATA RMACH(3) / Z'3CA0000000000000' /     
C     DATA RMACH(4) / Z'3CB0000000000000' /     
C     DATA RMACH(5) / Z'3FD34413509F79FF' /     
C       
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES      
C       
C     DATA RMACH(1) / O000400000000 /   
C     DATA RMACH(2) / O377777777777 /   
C     DATA RMACH(3) / O146400000000 /   
C     DATA RMACH(4) / O147400000000 /   
C     DATA RMACH(5) / O177464202324 /   
C       
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR      
C       
C     DATA SMALL, SMALL(2) /     0,    256/  
C     DATA LARGE, LARGE(2) /    -1,   -129/  
C     DATA RIGHT, RIGHT(2) /     0,  26880/  
C     DATA DIVER, DIVER(2) /     0,  27136/  
C     DATA LOG10, LOG10(2) /  8347,  32538/  
C       
#if defined(CRAY)
      DATA RMACH(1) / 200034000000000000000B /  
      DATA RMACH(2) / 577767777777777777776B /  
      DATA RMACH(3) / 377224000000000000000B /  
      DATA RMACH(4) / 377234000000000000000B /  
      DATA RMACH(5) / 377774642023241175720B /  
#else
#if defined(__alpha) && defined(__osf__)
      DATA RMACH(1) / '00800000'X /
      DATA RMACH(2) / '7F7FFFFF'X /
      DATA RMACH(3) / '33800000'X /
      DATA RMACH(4) / '34000000'X /
      DATA RMACH(5) / '3E9A209B'X /
#else
      DATA SMALL / X'00800000' /
      DATA LARGE / X'7F7FFFFF' /
      DATA RIGHT / X'33800000' /
      DATA DIVER / X'34000000' /
      DATA LOG10 / X'3E9A209B' /
#endif       
#endif       
      IF (I .LT. 1 .OR. I .GT. 5) WRITE(6,500)
  500 FORMAT(' ERROR IN R1MACH - I OUT OF BOUNDS')       
C       
      R1MACH = RMACH(I) 
      RETURN    
C       
      END       

