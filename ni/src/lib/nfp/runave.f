C NCLFORTSTART
      subroutine runave (x,npts,nave,kopt,xmsg,work,lwork,ier)

C NCL function: x = runave (x,nave,kopt)

      integer npts, nave,kopt,ier
      real x(npts), xmsg 
C NCLEND

      integer lwork
      real work(lwork)

      nhalf = nave/2
      lwork = npts+2*nhalf

      call runavx77 (x,npts,nave,kopt,xmsg,work,lwork,ier)       

      return   ! NCL return (runave[x])
      end
C ---------------------------------------------------------
      subroutine runavx77 (x,npts,nave,kopt,xmsg,work,lw,ier)       

c same as runavx: slightly faster but larger memory for work
                                                                
c this routine will perform a running average on x             
c .   the end points may be treated in three different ways   
                                                                                
c nomenclature:                                                                 
c .   x         - on input the series to be smoothed                            
c .               on output the smoothed series                                 
c .   npts      - no of pts in x                                                
c .   nave     - no. of pts to be utilized in the running average              
c .   kopt      - end-point option [prefer an odd number]
c .               kopt < 0 : utilize cyclic conditions             
c .                          e.g.,  nave=3 at n=1 and n=npts     
c .                          x(1)    = (x(npts)+x(1)+x(2)) / 3.  
c .                          x(npts) = (x(npts-1)+x(npts)+x(1)) / 3.
c .                          e.g.,  nave=4 at n=1 and n=npts     
c .                          x(1)    = (x(npts)+x(1)+x(2)+x(3)) / 4.  
c .                          x(npts) = (x(npts-2)+x(npts-1)+x(npts)+x(1)) / 4.
c .               kopt = 0 : set unsmoothed beginning and end pts to xmsg       
c .                          e.g.,  nave=3 , x(1) = xmsg and x(npts) = xmsg    
c .                          e.g.,  nave=4 , x(1) = xmsg and x(2) = xmsg and 
c .                                          x(npts) = xmsg    
c .               kopt > 0 : utilize reflective (symmetric) conditions
c .                          e.g.,  nave=3 at n=1 and n=npts        
c .                          x(1)    = (x(2)+x(1)+x(2)) / 3.        
c .                          x(npts) = (x(npts-1)+x(npts)+x(npts-1)) /3.
c .                          e.g.,  nave=4 at n=1 and n=npts        
c .                          x(1)    = (x(3)+x(2)+x(1)+x(2)) / 4.        
c .                          x(npts) = (x(npts-2)+x(npts-1)+x(npts)+x(npts-1)) /4.
c .   xmsg      - missing/special value
c .               also; if kopt=0 set end points to this special value     
c .   work      - work vector 
c .   lw        - length of work (=npts+2*N where N=nave/2)
c .   ier       - error flag                          
c .               ier = 0 : no errors                
c .               ier = -11 : npts @ 0              
c .               ier = -12 : npts < nave         
c .               ier = -13 : nave is not odd    
                                                 
      real x(1:npts) , work(lw)          ! do not need work this large
                                                
      ier = 0                                 
      if (npts.lt.1) ier = -11               
      if (nave.gt.npts) ier = -12          
      if (ier.lt.0) return                 
  
      if (nave.le.1) then
          do n=1,npts
             work(n) = x(n)    ! historical reasons
          end do
          return              
      end if
 
      nav   = nave          
      nav2  = nave/2        
      wgt   = 1./real(nav)        
      lwork = npts+2*nav2
      noe   = 0                           ! odd-even offset used below
      if (mod(nave,2).eq.0) noe = 1

      do n=1,lwork
         work(n) = xmsg                   ! preset to xmsg
      enddo
      
      do n=1,npts     
         work(nav2+n) = x(n)              ! stuff x into middle of work 
      enddo
                                          ! pad work with appropriate values
      if (kopt.gt.0) then
          do n=1,nav2
c c c        print *,"(nav2+1-n), (npts+nav2+n)=",
c c c+                (nav2+1-n), (npts+nav2+n)
             work(nav2+1-n)    = x(n+1)
             work(npts+nav2+n) = x(npts-n)
          enddo
      elseif (kopt.lt.0) then
          do n=1,nav2
c c c        print *,"(nav2+1-n), (npts+nav2+n)=",
c c c+                (nav2+1-n), (npts+nav2+n)
             work(nav2+1-n)    = x(npts+1-n)
             work(npts+nav2+n) = x(n)
          enddo
      endif

      if (mod(nave,2).eq.0) then
c          print *," "
c          do n=1,lwork
c             write (*,"(' runavx77: ',3i4,3f8.2))") kopt,nav2,n,work(n) 
c          end do
      end if
      
      do n=1,npts  
         kmsg  = 0 
         sum   = 0.                      
         nmid  = n+nav2+noe
         mstrt = nmid-nav2
         mlast = mstrt+nav-1
        do m=mstrt,mlast
           if (work(m).ne.xmsg) then 
               sum = sum+work(m) 
c c c          if (mod(nave,2).eq.0) then
c c c          print *,' DEBUG: kopt,n,nmid,mstrt,mlast,m,work(m)='
c c c+                ,         kopt,n,nmid,mstrt,mlast,m,work(m)
c c c          end if
           else
               kmsg = kmsg+1
           endif  
        enddo       

         if (kmsg.eq.0) then
             x(n) = sum*wgt    ! all values 
         else
             x(n) = xmsg        ! msg values encountered
         endif
      enddo        

CDIR$ IVDEP
c     do n=1,npts               ! return x in work(1 => npts)
c        work(n) = work(nav2+n) ! for historical reasons 
c     enddo
 
      return  
      end    
