c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK3.0                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file hrfft.f
c
c     this file contains a multiple fft package for spherepack3.0.
c     it includes code and documentation for performing fast fourier
c     transforms (see subroutines hrffti,hrfftf and hrfftb)
c
c **********************************************************************
c
c     subroutine hrffti(n,wsave)
c
c     subroutine hrffti initializes the array wsave which is used in
c     both hrfftf and hrfftb. the prime factorization of n together 
c     with a tabulation of the trigonometric functions are computed and
c     stored in wsave.
c
c     input parameter
c
c     n       the length of the sequence to be transformed.
c
c     output parameter
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             the same work array can be used for both hrfftf and 
c             hrfftb as long as n remains unchanged. different wsave 
c             arrays are required for different values of n. the 
c             contents of wsave must not be changed between calls 
c             of hrfftf or hrfftb.
c
c **********************************************************************
c
c     subroutine hrfftf(m,n,r,mdimr,wsave,work)
c
c     subroutine hrfftf computes the fourier coefficients of m real
c     perodic sequences (fourier analysis); i.e. hrfftf computes the
c     real fft of m sequences each with length n. the transform is 
c     defined below at output parameter r.
c
c     input parameters
c
c     m       the number of sequences.
c
c     n       the length of all m sequences.  the method is most
c             efficient when n is a product of small primes. n may
c             change as long as different work arrays are provided
c
c     r       r(m,n) is a two dimensional real array that contains m
c             sequences each with length n.
c
c     mdimr   the first dimension of the r array as it appears
c             in the program that calls hrfftf. mdimr must be
c             greater than or equal to m.
c
c
c     wsave   a work array with at least least 2*n+15 locations
c             in the program that calls hrfftf. the wsave array must be
c             initialized by calling subroutine hrffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by hrfftf and hrfftb.
c
c     work    a real work array with m*n locations.
c
c
c     output parameters
c
c     r      for all j=1,...,m
c  
c             r(j,1) = the sum from i=1 to i=n of r(j,i)
c
c             if n is even set l =n/2   , if n is odd set l = (n+1)/2
c
c               then for k = 2,...,l
c
c                  r(j,2*k-2) = the sum from i = 1 to i = n of
c
c                       r(j,i)*cos((k-1)*(i-1)*2*pi/n)
c
c                  r(j,2*k-1) = the sum from i = 1 to i = n of
c
c                      -r(j,i)*sin((k-1)*(i-1)*2*pi/n)
c
c             if n is even
c
c                  r(j,n) = the sum from i = 1 to i = n of
c
c                       (-1)**(i-1)*r(j,i)
c
c      *****  note
c                  this transform is unnormalized since a call of hrfftf
c                  followed by a call of hrfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of hrfftf or hrfftb.
c
c     work    a real work array with m*n locations that does
c             not have to be saved.
c
c **********************************************************************
c
c     subroutine hrfftb(m,n,r,mdimr,wsave,work)
c
c     subroutine hrfftb computes the real perodic sequence of m
c     sequences from their fourier coefficients (fourier synthesis). 
c     the transform is defined below at output parameter r.
c
c     input parameters
c
c     m       the number of sequences.
c
c     n       the length of all m sequences.  the method is most
c             efficient when n is a product of small primes. n may
c             change as long as different work arrays are provided
c
c     r       r(m,n) is a two dimensional real array that contains
c             the fourier coefficients of m sequences each with 
c             length n.
c
c     mdimr   the first dimension of the r array as it appears
c             in the program that calls hrfftb. mdimr must be
c             greater than or equal to m.
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             in the program that calls hrfftb. the wsave array must be
c             initialized by calling subroutine hrffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by hrfftf and hrfftb.
c
c     work    a real work array with m*n locations.
c
c
c     output parameters
c
c     r      for all j=1,...,m
c  
c             for n even and for i = 1,...,n
c
c                  r(j,i) = r(j,1)+(-1)**(i-1)*r(j,n)
c
c                       plus the sum from k=2 to k=n/2 of
c
c                        2.*r(j,2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                       -2.*r(j,2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c             for n odd and for i = 1,...,n
c
c                  r(j,i) = r(j,1) plus the sum from k=2 to k=(n+1)/2 of
c
c                       2.*r(j,2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                      -2.*r(j,2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c      *****  note
c                  this transform is unnormalized since a call of hrfftf
c                  followed by a call of hrfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of hrfftb or hrfftf.
c
c     work    a real work array with m*n locations that does not
c             have to be saved
c
c **********************************************************************
c
c
c
      subroutine hrffti (n,wsave)
      dimension       wsave(n+15)                                              
      common /hrf/ tfft
      tfft = 0.
      if (n .eq. 1) return                                                     
      call hrfti1 (n,wsave(1),wsave(n+1))
      return                                                                   
      end                                                                      
      subroutine hrfti1 (n,wa,fac)
c                                                                              
c     a multiple fft package for spherepack
c                                                                              
      dimension       wa(n)      ,fac(15)    ,ntryh(4)                         
      double precision tpi,argh,argld,arg
      data ntryh(1),ntryh(2),ntryh(3),ntryh(4)/4,2,3,5/                        
      nl = n                                                                   
      nf = 0           
      j = 0            
  101 j = j+1          
      if (j-4) 102,102,103                
  102 ntry = ntryh(j)                     
      go to 104        
  103 ntry = ntry+2    
  104 nq = nl/ntry     
      nr = nl-ntry*nq                     
      if (nr) 101,105,101                 
  105 nf = nf+1        
      fac(nf+2) = ntry                    
      nl = nq          
      if (ntry .ne. 2) go to 107          
      if (nf .eq. 1) go to 107            
      do 106 i=2,nf    
         ib = nf-i+2   
         fac(ib+2) = fac(ib+1)            
  106 continue         
      fac(3) = 2       
  107 if (nl .ne. 1) go to 104            
      fac(1) = n       
      fac(2) = nf      
      tpi = 8.d0*datan(1.d0)
      argh = tpi/float(n)                 
      is = 0           
      nfm1 = nf-1      
      l1 = 1           
      if (nfm1 .eq. 0) return             
      do 110 k1=1,nfm1                    
         ip = fac(k1+2)                   
         ld = 0        
         l2 = l1*ip    
         ido = n/l2    
         ipm = ip-1    
         do 109 j=1,ipm                   
            ld = ld+l1                    
            i = is     
            argld = float(ld)*argh        
            fi = 0.    
            do 108 ii=3,ido,2             
               i = i+2                    
               fi = fi+1.                 
               arg = fi*argld             
	       wa(i-1) = dcos(arg)
	       wa(i) = dsin(arg)
  108       continue   
            is = is+ido                   
  109    continue      
         l1 = l2       
  110 continue         
      return           
      end              
      subroutine hrfftf (m,n,r,mdimr,whrfft,work)
c                      
c     a multiple fft package for spherepack
c                      
      dimension       r(mdimr,n)  ,work(1)    ,whrfft(n+15)
      common /hrf/ tfft
      if (n .eq. 1) return                
c     tstart = second(dum)
      call hrftf1 (m,n,r,mdimr,work,whrfft,whrfft(n+1))
c     tfft = tfft+second(dum)-tstart
      return           
      end              
      subroutine hrftf1 (m,n,c,mdimc,ch,wa,fac)
c                      
c     a multiple fft package for spherepack
c                      
      dimension       ch(m,n) ,c(mdimc,n)  ,wa(n)   ,fac(15)
      nf = fac(2)      
      na = 1           
      l2 = n           
      iw = n           
      do 111 k1=1,nf   
         kh = nf-k1    
         ip = fac(kh+3)                   
         l1 = l2/ip    
         ido = n/l2    
         idl1 = ido*l1                    
         iw = iw-(ip-1)*ido               
         na = 1-na     
         if (ip .ne. 4) go to 102         
         ix2 = iw+ido                     
         ix3 = ix2+ido                    
         if (na .ne. 0) go to 101         
	 call hradf4 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2),wa(ix3))
         go to 110     
  101    call hradf4 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2),wa(ix3))
         go to 110     
  102    if (ip .ne. 2) go to 104         
         if (na .ne. 0) go to 103         
	 call hradf2 (m,ido,l1,c,mdimc,ch,m,wa(iw))
         go to 110     
  103    call hradf2 (m,ido,l1,ch,m,c,mdimc,wa(iw))
         go to 110     
  104    if (ip .ne. 3) go to 106         
         ix2 = iw+ido                     
         if (na .ne. 0) go to 105         
	 call hradf3 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2))
         go to 110     
  105    call hradf3 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2))
         go to 110     
  106    if (ip .ne. 5) go to 108         
         ix2 = iw+ido                     
         ix3 = ix2+ido                    
         ix4 = ix3+ido                    
         if (na .ne. 0) go to 107         
      call hradf5(m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 110     
  107 call hradf5(m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 110     
  108    if (ido .eq. 1) na = 1-na        
         if (na .ne. 0) go to 109         
	 call hradfg (m,ido,ip,l1,idl1,c,c,c,mdimc,ch,ch,m,wa(iw))
         na = 1        
         go to 110     
  109    call hradfg (m,ido,ip,l1,idl1,ch,ch,ch,m,c,c,mdimc,wa(iw))
         na = 0        
  110    l2 = l1       
  111 continue         
      if (na .eq. 1) return
      do 112 j=1,n     
      do 112 i=1,m     
	 c(i,j) = ch(i,j)
  112 continue         
      return           
      end              
      subroutine hradf4 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1,wa2,wa3)
c                      
c     a multiple fft package for spherepack
c                      
      dimension    cc(mdimcc,ido,l1,4)   ,ch(mdimch,ido,4,l1)     ,
     1             wa1(ido)     ,wa2(ido)     ,wa3(ido)
      hsqt2=sqrt(2.)/2.                   
      do 101 k=1,l1    
         do 1001 m=1,mp                   
         ch(m,1,1,k) = (cc(m,1,k,2)+cc(m,1,k,4))             
     1      +(cc(m,1,k,1)+cc(m,1,k,3))    
         ch(m,ido,4,k) = (cc(m,1,k,1)+cc(m,1,k,3))           
     1      -(cc(m,1,k,2)+cc(m,1,k,4))    
         ch(m,ido,2,k) = cc(m,1,k,1)-cc(m,1,k,3)             
         ch(m,1,3,k) = cc(m,1,k,4)-cc(m,1,k,2)               
 1001    continue      
  101 continue         
      if (ido-2) 107,105,102              
  102 idp2 = ido+2     
      do 104 k=1,l1    
         do 103 i=3,ido,2                 
            ic = idp2-i                   
            do 1003 m=1,mp                
            ch(m,i-1,1,k) = ((wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*                 
     1       cc(m,i,k,2))+(wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*  
     1       cc(m,i,k,4)))+(cc(m,i-1,k,1)+(wa2(i-2)*cc(m,i-1,k,3)+             
     1       wa2(i-1)*cc(m,i,k,3)))       
            ch(m,ic-1,4,k) = (cc(m,i-1,k,1)+(wa2(i-2)*cc(m,i-1,k,3)+           
     1       wa2(i-1)*cc(m,i,k,3)))-((wa1(i-2)*cc(m,i-1,k,2)+                  
     1       wa1(i-1)*cc(m,i,k,2))+(wa3(i-2)*cc(m,i-1,k,4)+  
     1       wa3(i-1)*cc(m,i,k,4)))       
            ch(m,i,1,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*   
     1       cc(m,i-1,k,2))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))+(cc(m,i,k,1)+(wa2(i-2)*cc(m,i,k,3)-               
     1       wa2(i-1)*cc(m,i-1,k,3)))     
            ch(m,ic,4,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*  
     1       cc(m,i-1,k,2))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))-(cc(m,i,k,1)+(wa2(i-2)*cc(m,i,k,3)-               
     1       wa2(i-1)*cc(m,i-1,k,3)))     
            ch(m,i-1,3,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)* 
     1       cc(m,i-1,k,2))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))+(cc(m,i-1,k,1)-(wa2(i-2)*cc(m,i-1,k,3)+           
     1       wa2(i-1)*cc(m,i,k,3)))       
            ch(m,ic-1,2,k) = (cc(m,i-1,k,1)-(wa2(i-2)*cc(m,i-1,k,3)+           
     1       wa2(i-1)*cc(m,i,k,3)))-((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*           
     1       cc(m,i-1,k,2))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))              
            ch(m,i,3,k) = ((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)* 
     1       cc(m,i,k,4))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2)))+(cc(m,i,k,1)-(wa2(i-2)*cc(m,i,k,3)-                 
     1       wa2(i-1)*cc(m,i-1,k,3)))     
            ch(m,ic,2,k) = ((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*                  
     1       cc(m,i,k,4))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2)))-(cc(m,i,k,1)-(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*        
     1       cc(m,i-1,k,3)))              
 1003       continue   
  103    continue      
  104 continue         
      if (mod(ido,2) .eq. 1) return       
  105 continue         
      do 106 k=1,l1    
         do 1006 m=1,mp                   
            ch(m,ido,1,k) = (hsqt2*(cc(m,ido,k,2)-cc(m,ido,k,4)))+              
     1       cc(m,ido,k,1)                
            ch(m,ido,3,k) = cc(m,ido,k,1)-(hsqt2*(cc(m,ido,k,2)-                
     1       cc(m,ido,k,4)))              
            ch(m,1,2,k) = (-hsqt2*(cc(m,ido,k,2)+cc(m,ido,k,4)))-               
     1       cc(m,ido,k,3)                
            ch(m,1,4,k) = (-hsqt2*(cc(m,ido,k,2)+cc(m,ido,k,4)))+               
     1       cc(m,ido,k,3)                
 1006    continue      
  106 continue         
  107 return           
      end              
      subroutine hradf2 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1)
c                      
c     a multiple fft package for spherepack
c                      
      dimension   ch(mdimch,ido,2,l1)  ,cc(mdimcc,ido,l1,2)     ,
     1                wa1(ido)            
      do 101 k=1,l1    
         do 1001 m=1,mp                   
         ch(m,1,1,k) = cc(m,1,k,1)+cc(m,1,k,2)               
         ch(m,ido,2,k) = cc(m,1,k,1)-cc(m,1,k,2)             
 1001    continue      
  101 continue         
      if (ido-2) 107,105,102              
  102 idp2 = ido+2     
      do 104 k=1,l1    
         do 103 i=3,ido,2                 
            ic = idp2-i                   
            do 1003 m=1,mp                
            ch(m,i,1,k) = cc(m,i,k,1)+(wa1(i-2)*cc(m,i,k,2)- 
     1       wa1(i-1)*cc(m,i-1,k,2))      
            ch(m,ic,2,k) = (wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*   
     1       cc(m,i-1,k,2))-cc(m,i,k,1)   
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+(wa1(i-2)*cc(m,i-1,k,2)+              
     1       wa1(i-1)*cc(m,i,k,2))        
            ch(m,ic-1,2,k) = cc(m,i-1,k,1)-(wa1(i-2)*cc(m,i-1,k,2)+             
     1       wa1(i-1)*cc(m,i,k,2))        
 1003       continue   
  103    continue      
  104 continue         
      if (mod(ido,2) .eq. 1) return       
  105 do 106 k=1,l1    
         do 1006 m=1,mp                   
         ch(m,1,2,k) = -cc(m,ido,k,2)     
         ch(m,ido,1,k) = cc(m,ido,k,1)    
 1006    continue      
  106 continue         
  107 return           
      end              
      subroutine hradf3 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1,wa2)
c                      
c     a multiple fft package for spherepack
c                      
      dimension   ch(mdimch,ido,3,l1)  ,cc(mdimcc,ido,l1,3)     ,
     1                wa1(ido)     ,wa2(ido)                 
      arg=2.*pimach()/3.                  
      taur=cos(arg)    
      taui=sin(arg)    
      do 101 k=1,l1    
         do 1001 m=1,mp                   
         ch(m,1,1,k) = cc(m,1,k,1)+(cc(m,1,k,2)+cc(m,1,k,3)) 
         ch(m,1,3,k) = taui*(cc(m,1,k,3)-cc(m,1,k,2))        
         ch(m,ido,2,k) = cc(m,1,k,1)+taur*                   
     1      (cc(m,1,k,2)+cc(m,1,k,3))     
 1001    continue      
  101 continue         
      if (ido .eq. 1) return              
      idp2 = ido+2     
      do 103 k=1,l1    
         do 102 i=3,ido,2                 
            ic = idp2-i                   
            do 1002 m=1,mp                
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+((wa1(i-2)*cc(m,i-1,k,2)+             
     1       wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*            
     1       cc(m,i,k,3)))                
            ch(m,i,1,k) = cc(m,i,k,1)+((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*          
     1       cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*  
     1       cc(m,i-1,k,3)))              
            ch(m,i-1,3,k) = (cc(m,i-1,k,1)+taur*((wa1(i-2)*  
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*  
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))))+(taui*((wa1(i-2)*            
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa2(i-2)*  
     1       cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3))))           
            ch(m,ic-1,2,k) = (cc(m,i-1,k,1)+taur*((wa1(i-2)* 
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*  
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))))-(taui*((wa1(i-2)*            
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa2(i-2)*  
     1       cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3))))           
            ch(m,i,3,k) = (cc(m,i,k,1)+taur*((wa1(i-2)*cc(m,i,k,2)-             
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*            
     1       cc(m,i-1,k,3))))+(taui*((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*          
     1       cc(m,i,k,3))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2))))               
            ch(m,ic,2,k) = (taui*((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*             
     1       cc(m,i,k,3))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2))))-(cc(m,i,k,1)+taur*((wa1(i-2)*cc(m,i,k,2)-           
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*            
     1       cc(m,i-1,k,3))))             
 1002       continue   
  102    continue      
  103 continue         
      return           
      end              
      subroutine hradf5 (mp,ido,l1,cc,mdimcc,ch,mdimch,
     1                   wa1,wa2,wa3,wa4)
c                      
c     a multiple fft package for spherepack
c                      
      dimension  cc(mdimcc,ido,l1,5)    ,ch(mdimch,ido,5,l1)     ,
     1           wa1(ido)     ,wa2(ido)     ,wa3(ido)     ,wa4(ido)             
      arg=2.*pimach()/5.                  
      tr11=cos(arg)    
      ti11=sin(arg)    
      tr12=cos(2.*arg)                    
      ti12=sin(2.*arg)                    
      do 101 k=1,l1    
         do 1001 m=1,mp                   
         ch(m,1,1,k) = cc(m,1,k,1)+(cc(m,1,k,5)+cc(m,1,k,2))+                   
     1    (cc(m,1,k,4)+cc(m,1,k,3))       
         ch(m,ido,2,k) = cc(m,1,k,1)+tr11*(cc(m,1,k,5)+cc(m,1,k,2))+            
     1    tr12*(cc(m,1,k,4)+cc(m,1,k,3))  
         ch(m,1,3,k) = ti11*(cc(m,1,k,5)-cc(m,1,k,2))+ti12*  
     1    (cc(m,1,k,4)-cc(m,1,k,3))       
         ch(m,ido,4,k) = cc(m,1,k,1)+tr12*(cc(m,1,k,5)+cc(m,1,k,2))+            
     1    tr11*(cc(m,1,k,4)+cc(m,1,k,3))  
         ch(m,1,5,k) = ti12*(cc(m,1,k,5)-cc(m,1,k,2))-ti11*  
     1    (cc(m,1,k,4)-cc(m,1,k,3))       
 1001    continue      
  101 continue         
      if (ido .eq. 1) return              
      idp2 = ido+2     
      do 103 k=1,l1    
         do 102 i=3,ido,2                 
            ic = idp2-i                   
            do 1002 m=1,mp                
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+((wa1(i-2)*cc(m,i-1,k,2)+             
     1       wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*            
     1       cc(m,i,k,5)))+((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*                   
     1       cc(m,i,k,3))+(wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4)))        
            ch(m,i,1,k) = cc(m,i,k,1)+((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*          
     1       cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*  
     1       cc(m,i-1,k,5)))+((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*                   
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))              
            ch(m,i-1,3,k) = cc(m,i-1,k,1)+tr11*              
     1      ( wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2)    
     1       +wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5))+tr12*                
     1      ( wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3)    
     1       +wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))+ti11*                
     1      ( wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2)    
     1       -(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*cc(m,i-1,k,5)))+ti12*              
     1      ( wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3)    
     1       -(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*cc(m,i-1,k,4))) 
            ch(m,ic-1,2,k) = cc(m,i-1,k,1)+tr11*             
     1      ( wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2)    
     1       +wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5))+tr12*                
     1     ( wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3)     
     1      +wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))-(ti11*                
     1      ( wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2)    
     1       -(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*cc(m,i-1,k,5)))+ti12*              
     1      ( wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3)    
     1       -(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*cc(m,i-1,k,4))))                   
            ch(m,i,3,k) = (cc(m,i,k,1)+tr11*((wa1(i-2)*cc(m,i,k,2)-             
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*            
     1       cc(m,i-1,k,5)))+tr12*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*              
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4))))+(ti11*((wa4(i-2)*cc(m,i-1,k,5)+                   
     1       wa4(i-1)*cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*            
     1       cc(m,i,k,2)))+ti12*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*              
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*  
     1       cc(m,i,k,3))))               
            ch(m,ic,2,k) = (ti11*((wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*             
     1       cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2)))+ti12*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*              
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*  
     1       cc(m,i,k,3))))-(cc(m,i,k,1)+tr11*((wa1(i-2)*cc(m,i,k,2)-           
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*            
     1       cc(m,i-1,k,5)))+tr12*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*              
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4))))             
            ch(m,i-1,5,k) = (cc(m,i-1,k,1)+tr12*((wa1(i-2)*  
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*  
     1       cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5)))+tr11*((wa2(i-2)*              
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))+(wa3(i-2)*  
     1       cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))))+(ti12*((wa1(i-2)*            
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa4(i-2)*cc(m,i,k,5)-         
     1       wa4(i-1)*cc(m,i-1,k,5)))-ti11*((wa2(i-2)*cc(m,i,k,3)-              
     1       wa2(i-1)*cc(m,i-1,k,3))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*            
     1       cc(m,i-1,k,4))))             
            ch(m,ic-1,4,k) = (cc(m,i-1,k,1)+tr12*((wa1(i-2)* 
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*  
     1       cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5)))+tr11*((wa2(i-2)*              
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))+(wa3(i-2)*  
     1       cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))))-(ti12*((wa1(i-2)*            
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa4(i-2)*cc(m,i,k,5)-         
     1       wa4(i-1)*cc(m,i-1,k,5)))-ti11*((wa2(i-2)*cc(m,i,k,3)-              
     1       wa2(i-1)*cc(m,i-1,k,3))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*            
     1       cc(m,i-1,k,4))))             
            ch(m,i,5,k) = (cc(m,i,k,1)+tr12*((wa1(i-2)*cc(m,i,k,2)-             
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*            
     1       cc(m,i-1,k,5)))+tr11*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*              
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4))))+(ti12*((wa4(i-2)*cc(m,i-1,k,5)+                   
     1       wa4(i-1)*cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*            
     1       cc(m,i,k,2)))-ti11*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*              
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*  
     1       cc(m,i,k,3))))               
            ch(m,ic,4,k) = (ti12*((wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*             
     1       cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2)))-ti11*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*              
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*  
     1       cc(m,i,k,3))))-(cc(m,i,k,1)+tr12*((wa1(i-2)*cc(m,i,k,2)-           
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*            
     1       cc(m,i-1,k,5)))+tr11*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*              
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4))))             
 1002       continue   
  102    continue      
  103 continue         
      return           
      end              
      subroutine hradfg (mp,ido,ip,l1,idl1,cc,c1,c2,mdimcc,
     1              ch,ch2,mdimch,wa)
c                      
c     a multiple fft package for spherepack
c                      
      dimension     ch(mdimch,ido,l1,ip)   ,cc(mdimcc,ido,ip,l1)  ,
     1            c1(mdimcc,ido,l1,ip)    ,c2(mdimcc,idl1,ip),
     2                ch2(mdimch,idl1,ip)           ,wa(ido)
      tpi=2.*pimach()                     
      arg = tpi/float(ip)                 
      dcp = cos(arg)   
      dsp = sin(arg)   
      ipph = (ip+1)/2                     
      ipp2 = ip+2      
      idp2 = ido+2     
      nbd = (ido-1)/2                     
      if (ido .eq. 1) go to 119           
      do 101 ik=1,idl1                    
         do 1001 m=1,mp                   
         ch2(m,ik,1) = c2(m,ik,1)         
 1001    continue      
  101 continue         
      do 103 j=2,ip    
         do 102 k=1,l1                    
            do 1002 m=1,mp                
            ch(m,1,k,j) = c1(m,1,k,j)     
 1002       continue   
  102    continue      
  103 continue         
      if (nbd .gt. l1) go to 107          
      is = -ido        
      do 106 j=2,ip    
         is = is+ido   
         idij = is     
         do 105 i=3,ido,2                 
            idij = idij+2                 
            do 104 k=1,l1                 
               do 1004 m=1,mp             
               ch(m,i-1,k,j) = wa(idij-1)*c1(m,i-1,k,j)+wa(idij)                
     1           *c1(m,i,k,j)             
               ch(m,i,k,j) = wa(idij-1)*c1(m,i,k,j)-wa(idij) 
     1           *c1(m,i-1,k,j)           
 1004          continue                   
  104       continue   
  105    continue      
  106 continue         
      go to 111        
  107 is = -ido        
      do 110 j=2,ip    
         is = is+ido   
         do 109 k=1,l1                    
            idij = is                     
            do 108 i=3,ido,2              
               idij = idij+2              
               do 1008 m=1,mp             
               ch(m,i-1,k,j) = wa(idij-1)*c1(m,i-1,k,j)+wa(idij)                
     1           *c1(m,i,k,j)             
               ch(m,i,k,j) = wa(idij-1)*c1(m,i,k,j)-wa(idij) 
     1           *c1(m,i-1,k,j)           
 1008          continue                   
  108       continue   
  109    continue      
  110 continue         
  111 if (nbd .lt. l1) go to 115          
      do 114 j=2,ipph                     
         jc = ipp2-j   
         do 113 k=1,l1                    
            do 112 i=3,ido,2              
               do 1012 m=1,mp             
               c1(m,i-1,k,j) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)  
               c1(m,i-1,k,jc) = ch(m,i,k,j)-ch(m,i,k,jc)     
               c1(m,i,k,j) = ch(m,i,k,j)+ch(m,i,k,jc)        
               c1(m,i,k,jc) = ch(m,i-1,k,jc)-ch(m,i-1,k,j)   
 1012          continue                   
  112       continue   
  113    continue      
  114 continue         
      go to 121        
  115 do 118 j=2,ipph                     
         jc = ipp2-j   
         do 117 i=3,ido,2                 
            do 116 k=1,l1                 
               do 1016 m=1,mp             
               c1(m,i-1,k,j) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)  
               c1(m,i-1,k,jc) = ch(m,i,k,j)-ch(m,i,k,jc)     
               c1(m,i,k,j) = ch(m,i,k,j)+ch(m,i,k,jc)        
               c1(m,i,k,jc) = ch(m,i-1,k,jc)-ch(m,i-1,k,j)   
 1016          continue                   
  116       continue   
  117    continue      
  118 continue         
      go to 121        
  119 do 120 ik=1,idl1                    
         do 1020 m=1,mp                   
         c2(m,ik,1) = ch2(m,ik,1)         
 1020    continue      
  120 continue         
  121 do 123 j=2,ipph                     
         jc = ipp2-j   
         do 122 k=1,l1                    
            do 1022 m=1,mp                
            c1(m,1,k,j) = ch(m,1,k,j)+ch(m,1,k,jc)           
            c1(m,1,k,jc) = ch(m,1,k,jc)-ch(m,1,k,j)          
 1022       continue   
  122    continue      
  123 continue         
c                      
      ar1 = 1.         
      ai1 = 0.         
      do 127 l=2,ipph                     
         lc = ipp2-l   
         ar1h = dcp*ar1-dsp*ai1           
         ai1 = dcp*ai1+dsp*ar1            
         ar1 = ar1h    
         do 124 ik=1,idl1                 
            do 1024 m=1,mp                
            ch2(m,ik,l) = c2(m,ik,1)+ar1*c2(m,ik,2)          
            ch2(m,ik,lc) = ai1*c2(m,ik,ip)                   
 1024       continue   
  124    continue      
         dc2 = ar1     
         ds2 = ai1     
         ar2 = ar1     
         ai2 = ai1     
         do 126 j=3,ipph                  
            jc = ipp2-j                   
            ar2h = dc2*ar2-ds2*ai2        
            ai2 = dc2*ai2+ds2*ar2         
            ar2 = ar2h                    
            do 125 ik=1,idl1              
               do 1025 m=1,mp             
               ch2(m,ik,l) = ch2(m,ik,l)+ar2*c2(m,ik,j)      
               ch2(m,ik,lc) = ch2(m,ik,lc)+ai2*c2(m,ik,jc)   
 1025          continue                   
  125       continue   
  126    continue      
  127 continue         
      do 129 j=2,ipph                     
         do 128 ik=1,idl1                 
            do 1028 m=1,mp                
            ch2(m,ik,1) = ch2(m,ik,1)+c2(m,ik,j)             
 1028       continue   
  128    continue      
  129 continue         
c                      
      if (ido .lt. l1) go to 132          
      do 131 k=1,l1    
         do 130 i=1,ido                   
            do 1030 m=1,mp                
            cc(m,i,1,k) = ch(m,i,k,1)     
 1030       continue   
  130    continue      
  131 continue         
      go to 135        
  132 do 134 i=1,ido   
         do 133 k=1,l1                    
            do 1033 m=1,mp                
            cc(m,i,1,k) = ch(m,i,k,1)     
 1033       continue   
  133    continue      
  134 continue         
  135 do 137 j=2,ipph                     
         jc = ipp2-j   
         j2 = j+j      
         do 136 k=1,l1                    
            do 1036 m=1,mp                
            cc(m,ido,j2-2,k) = ch(m,1,k,j)                   
            cc(m,1,j2-1,k) = ch(m,1,k,jc)                    
 1036       continue   
  136    continue      
  137 continue         
      if (ido .eq. 1) return              
      if (nbd .lt. l1) go to 141          
      do 140 j=2,ipph                     
         jc = ipp2-j   
         j2 = j+j      
         do 139 k=1,l1                    
            do 138 i=3,ido,2              
               ic = idp2-i                
               do 1038 m=1,mp             
               cc(m,i-1,j2-1,k) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)                  
               cc(m,ic-1,j2-2,k) = ch(m,i-1,k,j)-ch(m,i-1,k,jc)                 
               cc(m,i,j2-1,k) = ch(m,i,k,j)+ch(m,i,k,jc)     
               cc(m,ic,j2-2,k) = ch(m,i,k,jc)-ch(m,i,k,j)    
 1038          continue                   
  138       continue   
  139    continue      
  140 continue         
      return           
  141 do 144 j=2,ipph                     
         jc = ipp2-j   
         j2 = j+j      
         do 143 i=3,ido,2                 
            ic = idp2-i                   
            do 142 k=1,l1                 
               do 1042 m=1,mp             
               cc(m,i-1,j2-1,k) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)                  
               cc(m,ic-1,j2-2,k) = ch(m,i-1,k,j)-ch(m,i-1,k,jc)                 
               cc(m,i,j2-1,k) = ch(m,i,k,j)+ch(m,i,k,jc)     
               cc(m,ic,j2-2,k) = ch(m,i,k,jc)-ch(m,i,k,j)    
 1042          continue                   
  142       continue   
  143    continue      
  144 continue         
      return           
      end              
      function pimach()                   
      pimach=3.14159265358979             
      return           
      end              
      subroutine hrfftb(m,n,r,mdimr,whrfft,work)
c                      
c     a multiple fft package for spherepack
c                      
      dimension     r(mdimr,n)  ,work(1)    ,whrfft(n+15)
      common /hrf/ tfft
      if (n .eq. 1) return                
c     tstart = second(dum)
      call hrftb1 (m,n,r,mdimr,work,whrfft,whrfft(n+1))
c     tfft = tfft+second(dum)-tstart
      return           
      end              
      subroutine hrftb1 (m,n,c,mdimc,ch,wa,fac)
c                      
c     a multiple fft package for spherepack
c                      
      dimension       ch(m,n), c(mdimc,n), wa(n) ,fac(15)
      nf = fac(2)      
      na = 0           
      l1 = 1           
      iw = 1           
      do 116 k1=1,nf   
         ip = fac(k1+2)                   
         l2 = ip*l1    
         ido = n/l2    
         idl1 = ido*l1                    
         if (ip .ne. 4) go to 103         
         ix2 = iw+ido                     
         ix3 = ix2+ido                    
         if (na .ne. 0) go to 101         
	 call hradb4 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2),wa(ix3))
         go to 102     
  101    call hradb4 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2),wa(ix3))
  102    na = 1-na     
         go to 115     
  103    if (ip .ne. 2) go to 106         
         if (na .ne. 0) go to 104         
	 call hradb2 (m,ido,l1,c,mdimc,ch,m,wa(iw))
         go to 105     
  104    call hradb2 (m,ido,l1,ch,m,c,mdimc,wa(iw))
  105    na = 1-na     
         go to 115     
  106    if (ip .ne. 3) go to 109         
         ix2 = iw+ido                     
         if (na .ne. 0) go to 107         
	 call hradb3 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2))
         go to 108     
  107    call hradb3 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2))
  108    na = 1-na     
         go to 115     
  109    if (ip .ne. 5) go to 112         
         ix2 = iw+ido                     
         ix3 = ix2+ido                    
         ix4 = ix3+ido                    
         if (na .ne. 0) go to 110         
      call hradb5 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 111     
  110 call hradb5 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2),wa(ix3),wa(ix4))
  111    na = 1-na     
         go to 115     
  112    if (na .ne. 0) go to 113         
	 call hradbg (m,ido,ip,l1,idl1,c,c,c,mdimc,ch,ch,m,wa(iw))
         go to 114     
  113    call hradbg (m,ido,ip,l1,idl1,ch,ch,ch,m,c,c,mdimc,wa(iw))
  114    if (ido .eq. 1) na = 1-na        
  115    l1 = l2       
         iw = iw+(ip-1)*ido               
  116 continue         
      if (na .eq. 0) return
      do 117 j=1,n     
      do 117 i=1,m     
	 c(i,j) = ch(i,j)
  117 continue         
      return           
      end              
      subroutine hradbg (mp,ido,ip,l1,idl1,cc,c1,c2,mdimcc,
     1          ch,ch2,mdimch,wa)
c                      
c     a multiple fft package for spherepack
c                      
      dimension    ch(mdimch,ido,l1,ip)    ,cc(mdimcc,ido,ip,l1) ,
     1           c1(mdimcc,ido,l1,ip)     ,c2(mdimcc,idl1,ip),
     2                ch2(mdimch,idl1,ip)       ,wa(ido)
      tpi=2.*pimach()                     
      arg = tpi/float(ip)                 
      dcp = cos(arg)   
      dsp = sin(arg)   
      idp2 = ido+2     
      nbd = (ido-1)/2                     
      ipp2 = ip+2      
      ipph = (ip+1)/2                     
      if (ido .lt. l1) go to 103          
      do 102 k=1,l1    
         do 101 i=1,ido                   
            do 1001 m=1,mp                
            ch(m,i,k,1) = cc(m,i,1,k)     
 1001       continue   
  101    continue      
  102 continue         
      go to 106        
  103 do 105 i=1,ido   
         do 104 k=1,l1                    
            do 1004 m=1,mp                
            ch(m,i,k,1) = cc(m,i,1,k)     
 1004       continue   
  104    continue      
  105 continue         
  106 do 108 j=2,ipph                     
         jc = ipp2-j   
         j2 = j+j      
         do 107 k=1,l1                    
            do 1007 m=1,mp                
            ch(m,1,k,j) = cc(m,ido,j2-2,k)+cc(m,ido,j2-2,k)  
            ch(m,1,k,jc) = cc(m,1,j2-1,k)+cc(m,1,j2-1,k)     
 1007       continue   
  107    continue      
  108 continue         
      if (ido .eq. 1) go to 116           
      if (nbd .lt. l1) go to 112          
      do 111 j=2,ipph                     
         jc = ipp2-j   
         do 110 k=1,l1                    
            do 109 i=3,ido,2              
               ic = idp2-i                
               do 1009 m=1,mp             
               ch(m,i-1,k,j) = cc(m,i-1,2*j-1,k)+cc(m,ic-1,2*j-2,k)             
               ch(m,i-1,k,jc) = cc(m,i-1,2*j-1,k)-cc(m,ic-1,2*j-2,k)            
               ch(m,i,k,j) = cc(m,i,2*j-1,k)-cc(m,ic,2*j-2,k)                   
               ch(m,i,k,jc) = cc(m,i,2*j-1,k)+cc(m,ic,2*j-2,k)                  
 1009          continue                   
  109       continue   
  110    continue      
  111 continue         
      go to 116        
  112 do 115 j=2,ipph                     
         jc = ipp2-j   
         do 114 i=3,ido,2                 
            ic = idp2-i                   
            do 113 k=1,l1                 
               do 1013 m=1,mp             
               ch(m,i-1,k,j) = cc(m,i-1,2*j-1,k)+cc(m,ic-1,2*j-2,k)             
               ch(m,i-1,k,jc) = cc(m,i-1,2*j-1,k)-cc(m,ic-1,2*j-2,k)            
               ch(m,i,k,j) = cc(m,i,2*j-1,k)-cc(m,ic,2*j-2,k)                   
               ch(m,i,k,jc) = cc(m,i,2*j-1,k)+cc(m,ic,2*j-2,k)                  
 1013          continue                   
  113       continue   
  114    continue      
  115 continue         
  116 ar1 = 1.         
      ai1 = 0.         
      do 120 l=2,ipph                     
         lc = ipp2-l   
         ar1h = dcp*ar1-dsp*ai1           
         ai1 = dcp*ai1+dsp*ar1            
         ar1 = ar1h    
         do 117 ik=1,idl1                 
            do 1017 m=1,mp                
            c2(m,ik,l) = ch2(m,ik,1)+ar1*ch2(m,ik,2)         
            c2(m,ik,lc) = ai1*ch2(m,ik,ip)                   
 1017       continue   
  117    continue      
         dc2 = ar1     
         ds2 = ai1     
         ar2 = ar1     
         ai2 = ai1     
         do 119 j=3,ipph                  
            jc = ipp2-j                   
            ar2h = dc2*ar2-ds2*ai2        
            ai2 = dc2*ai2+ds2*ar2         
            ar2 = ar2h                    
            do 118 ik=1,idl1              
               do 1018 m=1,mp             
               c2(m,ik,l) = c2(m,ik,l)+ar2*ch2(m,ik,j)       
               c2(m,ik,lc) = c2(m,ik,lc)+ai2*ch2(m,ik,jc)    
 1018          continue                   
  118       continue   
  119    continue      
  120 continue         
      do 122 j=2,ipph                     
         do 121 ik=1,idl1                 
            do 1021 m=1,mp                
            ch2(m,ik,1) = ch2(m,ik,1)+ch2(m,ik,j)            
 1021       continue   
  121    continue      
  122 continue         
      do 124 j=2,ipph                     
         jc = ipp2-j   
         do 123 k=1,l1                    
            do 1023 m=1,mp                
            ch(m,1,k,j) = c1(m,1,k,j)-c1(m,1,k,jc)           
            ch(m,1,k,jc) = c1(m,1,k,j)+c1(m,1,k,jc)          
 1023       continue   
  123    continue      
  124 continue         
      if (ido .eq. 1) go to 132           
      if (nbd .lt. l1) go to 128          
      do 127 j=2,ipph                     
         jc = ipp2-j   
         do 126 k=1,l1                    
            do 125 i=3,ido,2              
               do 1025 m=1,mp             
               ch(m,i-1,k,j) = c1(m,i-1,k,j)-c1(m,i,k,jc)    
               ch(m,i-1,k,jc) = c1(m,i-1,k,j)+c1(m,i,k,jc)   
               ch(m,i,k,j) = c1(m,i,k,j)+c1(m,i-1,k,jc)      
               ch(m,i,k,jc) = c1(m,i,k,j)-c1(m,i-1,k,jc)     
 1025          continue                   
  125       continue   
  126    continue      
  127 continue         
      go to 132        
  128 do 131 j=2,ipph                     
         jc = ipp2-j   
         do 130 i=3,ido,2                 
            do 129 k=1,l1                 
               do 1029 m=1,mp             
               ch(m,i-1,k,j) = c1(m,i-1,k,j)-c1(m,i,k,jc)    
               ch(m,i-1,k,jc) = c1(m,i-1,k,j)+c1(m,i,k,jc)   
               ch(m,i,k,j) = c1(m,i,k,j)+c1(m,i-1,k,jc)      
               ch(m,i,k,jc) = c1(m,i,k,j)-c1(m,i-1,k,jc)     
 1029          continue                   
  129       continue   
  130    continue      
  131 continue         
  132 continue         
      if (ido .eq. 1) return              
      do 133 ik=1,idl1                    
         do 1033 m=1,mp                   
         c2(m,ik,1) = ch2(m,ik,1)         
 1033    continue      
  133 continue         
      do 135 j=2,ip    
         do 134 k=1,l1                    
            do 1034 m=1,mp                
            c1(m,1,k,j) = ch(m,1,k,j)     
 1034       continue   
  134    continue      
  135 continue         
      if (nbd .gt. l1) go to 139          
      is = -ido        
      do 138 j=2,ip    
         is = is+ido   
         idij = is     
         do 137 i=3,ido,2                 
            idij = idij+2                 
            do 136 k=1,l1                 
               do 1036 m=1,mp             
               c1(m,i-1,k,j) = wa(idij-1)*ch(m,i-1,k,j)-wa(idij)*               
     1          ch(m,i,k,j)               
               c1(m,i,k,j) = wa(idij-1)*ch(m,i,k,j)+wa(idij)*                   
     1          ch(m,i-1,k,j)             
 1036          continue                   
  136       continue   
  137    continue      
  138 continue         
      go to 143        
  139 is = -ido        
      do 142 j=2,ip    
         is = is+ido   
         do 141 k=1,l1                    
            idij = is                     
            do 140 i=3,ido,2              
               idij = idij+2              
               do 1040 m=1,mp             
               c1(m,i-1,k,j) = wa(idij-1)*ch(m,i-1,k,j)-wa(idij)*               
     1          ch(m,i,k,j)               
               c1(m,i,k,j) = wa(idij-1)*ch(m,i,k,j)+wa(idij)*                   
     1          ch(m,i-1,k,j)             
 1040          continue                   
  140       continue   
  141    continue      
  142 continue         
  143 return           
      end              
      subroutine hradb4 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1,wa2,wa3)
c                      
c     a multiple fft package for spherepack
c                      
      dimension  cc(mdimcc,ido,4,l1)  ,ch(mdimch,ido,l1,4)    ,
     1                wa1(ido)  ,wa2(ido)  ,wa3(ido)         
      sqrt2=sqrt(2.)   
      do 101 k=1,l1    
          do 1001 m=1,mp                  
         ch(m,1,k,3) = (cc(m,1,1,k)+cc(m,ido,4,k))           
     1   -(cc(m,ido,2,k)+cc(m,ido,2,k))   
         ch(m,1,k,1) = (cc(m,1,1,k)+cc(m,ido,4,k))           
     1   +(cc(m,ido,2,k)+cc(m,ido,2,k))   
         ch(m,1,k,4) = (cc(m,1,1,k)-cc(m,ido,4,k))           
     1   +(cc(m,1,3,k)+cc(m,1,3,k))       
         ch(m,1,k,2) = (cc(m,1,1,k)-cc(m,ido,4,k))           
     1   -(cc(m,1,3,k)+cc(m,1,3,k))       
 1001     continue     
  101 continue         
      if (ido-2) 107,105,102              
  102 idp2 = ido+2     
      do 104 k=1,l1    
         do 103 i=3,ido,2                 
            ic = idp2-i                   
               do 1002 m=1,mp             
            ch(m,i-1,k,1) = (cc(m,i-1,1,k)+cc(m,ic-1,4,k))   
     1      +(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                  
            ch(m,i,k,1) = (cc(m,i,1,k)-cc(m,ic,4,k))         
     1      +(cc(m,i,3,k)-cc(m,ic,2,k))   
            ch(m,i-1,k,2)=wa1(i-2)*((cc(m,i-1,1,k)-cc(m,ic-1,4,k))              
     1      -(cc(m,i,3,k)+cc(m,ic,2,k)))-wa1(i-1)            
     1      *((cc(m,i,1,k)+cc(m,ic,4,k))+(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))        
            ch(m,i,k,2)=wa1(i-2)*((cc(m,i,1,k)+cc(m,ic,4,k)) 
     1      +(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))+wa1(i-1)        
     1      *((cc(m,i-1,1,k)-cc(m,ic-1,4,k))-(cc(m,i,3,k)+cc(m,ic,2,k)))        
            ch(m,i-1,k,3)=wa2(i-2)*((cc(m,i-1,1,k)+cc(m,ic-1,4,k))              
     1      -(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))-wa2(i-1)        
     1      *((cc(m,i,1,k)-cc(m,ic,4,k))-(cc(m,i,3,k)-cc(m,ic,2,k)))            
            ch(m,i,k,3)=wa2(i-2)*((cc(m,i,1,k)-cc(m,ic,4,k)) 
     1      -(cc(m,i,3,k)-cc(m,ic,2,k)))+wa2(i-1)            
     1      *((cc(m,i-1,1,k)+cc(m,ic-1,4,k))-(cc(m,i-1,3,k)  
     1      +cc(m,ic-1,2,k)))             
            ch(m,i-1,k,4)=wa3(i-2)*((cc(m,i-1,1,k)-cc(m,ic-1,4,k))              
     1      +(cc(m,i,3,k)+cc(m,ic,2,k)))-wa3(i-1)            
     1     *((cc(m,i,1,k)+cc(m,ic,4,k))-(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))         
            ch(m,i,k,4)=wa3(i-2)*((cc(m,i,1,k)+cc(m,ic,4,k)) 
     1      -(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))+wa3(i-1)        
     1      *((cc(m,i-1,1,k)-cc(m,ic-1,4,k))+(cc(m,i,3,k)+cc(m,ic,2,k)))        
 1002          continue                   
  103    continue      
  104 continue         
      if (mod(ido,2) .eq. 1) return       
  105 continue         
      do 106 k=1,l1    
               do 1003 m=1,mp             
         ch(m,ido,k,1) = (cc(m,ido,1,k)+cc(m,ido,3,k))       
     1   +(cc(m,ido,1,k)+cc(m,ido,3,k))   
         ch(m,ido,k,2) = sqrt2*((cc(m,ido,1,k)-cc(m,ido,3,k))                   
     1   -(cc(m,1,2,k)+cc(m,1,4,k)))      
         ch(m,ido,k,3) = (cc(m,1,4,k)-cc(m,1,2,k))           
     1   +(cc(m,1,4,k)-cc(m,1,2,k))       
         ch(m,ido,k,4) = -sqrt2*((cc(m,ido,1,k)-cc(m,ido,3,k))                  
     1   +(cc(m,1,2,k)+cc(m,1,4,k)))      
 1003          continue                   
  106 continue         
  107 return           
      end              
      subroutine hradb2 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1)
c                      
c     a multiple fft package for spherepack
c                      
      dimension  cc(mdimcc,ido,2,l1)    ,ch(mdimch,ido,l1,2),
     1                wa1(ido)            
      do 101 k=1,l1    
          do 1001 m=1,mp                  
         ch(m,1,k,1) = cc(m,1,1,k)+cc(m,ido,2,k)             
         ch(m,1,k,2) = cc(m,1,1,k)-cc(m,ido,2,k)             
 1001     continue     
  101 continue         
      if (ido-2) 107,105,102              
  102 idp2 = ido+2     
      do 104 k=1,l1    
         do 103 i=3,ido,2                 
            ic = idp2-i                   
               do 1002 m=1,mp             
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+cc(m,ic-1,2,k)     
            ch(m,i,k,1) = cc(m,i,1,k)-cc(m,ic,2,k)           
            ch(m,i-1,k,2) = wa1(i-2)*(cc(m,i-1,1,k)-cc(m,ic-1,2,k))             
     1      -wa1(i-1)*(cc(m,i,1,k)+cc(m,ic,2,k))             
            ch(m,i,k,2) = wa1(i-2)*(cc(m,i,1,k)+cc(m,ic,2,k))+wa1(i-1)          
     1      *(cc(m,i-1,1,k)-cc(m,ic-1,2,k))                  
 1002          continue                   
  103    continue      
  104 continue         
      if (mod(ido,2) .eq. 1) return       
  105 do 106 k=1,l1    
          do 1003 m=1,mp                  
         ch(m,ido,k,1) = cc(m,ido,1,k)+cc(m,ido,1,k)         
         ch(m,ido,k,2) = -(cc(m,1,2,k)+cc(m,1,2,k))          
 1003     continue     
  106 continue         
  107 return           
      end              
      subroutine hradb3 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1,wa2)
c                      
c     a multiple fft package for spherepack
c                      
      dimension  cc(mdimcc,ido,3,l1)    ,ch(mdimch,ido,l1,3),
     1                wa1(ido)   ,wa2(ido)                   
      arg=2.*pimach()/3.                  
      taur=cos(arg)    
      taui=sin(arg)    
      do 101 k=1,l1    
          do 1001 m=1,mp                  
         ch(m,1,k,1) = cc(m,1,1,k)+2.*cc(m,ido,2,k)          
         ch(m,1,k,2) = cc(m,1,1,k)+(2.*taur)*cc(m,ido,2,k)   
     1   -(2.*taui)*cc(m,1,3,k)           
         ch(m,1,k,3) = cc(m,1,1,k)+(2.*taur)*cc(m,ido,2,k)   
     1   +2.*taui*cc(m,1,3,k)             
 1001     continue     
  101 continue         
      if (ido .eq. 1) return              
      idp2 = ido+2     
      do 103 k=1,l1    
         do 102 i=3,ido,2                 
            ic = idp2-i                   
               do 1002 m=1,mp             
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+(cc(m,i-1,3,k)+cc(m,ic-1,2,k))        
            ch(m,i,k,1) = cc(m,i,1,k)+(cc(m,i,3,k)-cc(m,ic,2,k))                
            ch(m,i-1,k,2) = wa1(i-2)*     
     1 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))- 
     * (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))                    
     2                   -wa1(i-1)*       
     3 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))+       
     * (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))                
            ch(m,i,k,2) = wa1(i-2)*       
     4 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))+       
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))                
     5                  +wa1(i-1)*        
     6 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))- 
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))                    
              ch(m,i-1,k,3) = wa2(i-2)*   
     7 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))+ 
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))                    
     8   -wa2(i-1)*    
     9 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))-       
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))                
            ch(m,i,k,3) = wa2(i-2)*       
     1 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))-       
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))                
     2                 +wa2(i-1)*         
     3 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))+ 
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))                    
 1002          continue                   
  102    continue      
  103 continue         
      return           
      end              
      subroutine hradb5 (mp,ido,l1,cc,mdimcc,ch,mdimch,
     1       wa1,wa2,wa3,wa4)
c                      
c     a multiple fft package for spherepack
c                      
      dimension  cc(mdimcc,ido,5,l1)    ,ch(mdimch,ido,l1,5),
     1             wa1(ido)     ,wa2(ido)     ,wa3(ido)     ,wa4(ido)           
      arg=2.*pimach()/5.                  
      tr11=cos(arg)    
      ti11=sin(arg)    
      tr12=cos(2.*arg)                    
      ti12=sin(2.*arg)                    
      do 101 k=1,l1    
      do 1001 m=1,mp   
         ch(m,1,k,1) = cc(m,1,1,k)+2.*cc(m,ido,2,k)+2.*cc(m,ido,4,k)            
         ch(m,1,k,2) = (cc(m,1,1,k)+tr11*2.*cc(m,ido,2,k)    
     1   +tr12*2.*cc(m,ido,4,k))-(ti11*2.*cc(m,1,3,k)        
     1   +ti12*2.*cc(m,1,5,k))            
         ch(m,1,k,3) = (cc(m,1,1,k)+tr12*2.*cc(m,ido,2,k)    
     1   +tr11*2.*cc(m,ido,4,k))-(ti12*2.*cc(m,1,3,k)        
     1   -ti11*2.*cc(m,1,5,k))            
         ch(m,1,k,4) = (cc(m,1,1,k)+tr12*2.*cc(m,ido,2,k)    
     1   +tr11*2.*cc(m,ido,4,k))+(ti12*2.*cc(m,1,3,k)        
     1   -ti11*2.*cc(m,1,5,k))            
         ch(m,1,k,5) = (cc(m,1,1,k)+tr11*2.*cc(m,ido,2,k)    
     1   +tr12*2.*cc(m,ido,4,k))+(ti11*2.*cc(m,1,3,k)        
     1   +ti12*2.*cc(m,1,5,k))            
 1001          continue                   
  101 continue         
      if (ido .eq. 1) return              
      idp2 = ido+2     
      do 103 k=1,l1    
         do 102 i=3,ido,2                 
            ic = idp2-i                   
      do 1002 m=1,mp   
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+(cc(m,i-1,3,k)+cc(m,ic-1,2,k))        
     1      +(cc(m,i-1,5,k)+cc(m,ic-1,4,k))                  
            ch(m,i,k,1) = cc(m,i,1,k)+(cc(m,i,3,k)-cc(m,ic,2,k))                
     1      +(cc(m,i,5,k)-cc(m,ic,4,k))   
            ch(m,i-1,k,2) = wa1(i-2)*((cc(m,i-1,1,k)+tr11*   
     1      (cc(m,i-1,3,k)+cc(m,ic-1,2,k))+tr12              
     1      *(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti11*(cc(m,i,3,k)                 
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
     1      -wa1(i-1)*((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))             
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))+(ti11*(cc(m,i-1,3,k)              
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))              
            ch(m,i,k,2) = wa1(i-2)*((cc(m,i,1,k)+tr11*(cc(m,i,3,k)              
     1      -cc(m,ic,2,k))+tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))  
     1      +(ti11*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))+ti12       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))+wa1(i-1)       
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)             
     1      +cc(m,ic-1,2,k))+tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))               
     1      -(ti11*(cc(m,i,3,k)+cc(m,ic,2,k))+ti12           
     1      *(cc(m,i,5,k)+cc(m,ic,4,k))))                    
            ch(m,i-1,k,3) = wa2(i-2)      
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti12*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
     1     -wa2(i-1)   
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-                 
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))   
     1      +(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))                
            ch(m,i,k,3) = wa2(i-2)        
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-                 
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))   
     1      +(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))                
     1      +wa2(i-1)                     
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti12*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
            ch(m,i-1,k,4) = wa3(i-2)      
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti12*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
     1      -wa3(i-1)                     
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-                 
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))   
     1      -(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))                
            ch(m,i,k,4) = wa3(i-2)        
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-                 
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))   
     1      -(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))                
     1      +wa3(i-1)                     
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti12*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
            ch(m,i-1,k,5) = wa4(i-2)      
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti11*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
     1      -wa4(i-1)                     
     1      *((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))   
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))-(ti11*(cc(m,i-1,3,k)              
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))              
            ch(m,i,k,5) = wa4(i-2)        
     1      *((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))   
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))-(ti11*(cc(m,i-1,3,k)              
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))              
     1      +wa4(i-1)                     
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti11*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
 1002          continue                   
  102    continue      
  103 continue         
      return           
      end              
