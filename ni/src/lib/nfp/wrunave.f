C ---------------------------------------------------------
C NCLFORTSTART
      subroutine wgtrunave (x,npts,wgt,nwgt,kopt,xmsg,work,lwork,ier)
 
C NCL function: x = wgtrunwgt (x,wgt,kopt)

      integer npts, nwgt,kopt,ier
      real x(npts), wgt(nwgt), xmsg 
C NCLEND
      integer lwork
      real work(lwork)

      nhalf = nwgt/2
      lwork = npts+2*nhalf

      call wrunavx77 (x,npts,wgt,nwgt,kopt,xmsg,work,lwork,ier)       

      return   ! NCL return (wrunwgt[x])
      end
c ---------------------------------------------------------
      subroutine wrunavx77 (x,npts,wgt,nwgt,kopt,xmsg,work,lw,ier)       

c same as wrunavx: slightly faster but slightly larger memory for work
                                                                
c this routine will perform a weighted  running average on x             
c .   the end points may be treated in three different ways   
                                                                                
c nomenclature:                                                                 
c .   x         - on input the series to be smoothed                            
c .               on output the smoothed series                                 
c .   npts      - no of pts in x                                                
c .   wgt       - vector containing weights [length=nwgt]
c .   nwgt      - no. of pts to be utilized in the wgted running average              
c .   kopt      - end-point option [prefer an odd number]
c .               kopt < 0 : utilize cyclic conditions             
c .                          e.g.,  nwgt=3 at n=1 and n=npts and wsum=SUM{wgt}     
c .                          x(1)    = w(1)*x(npts)+w(2)*x(1)+w(3)*x(2) 
c .                          x(npts) = w(1)*x(npts-1)+w(2)*x(npts)+w(3)*x(1)
c .                          e.g.,  nwgt=4 at n=1 and n=npts     
c .                          x(1)    = w(1)*x(npts-1)+w(2)*x(npts)+w(3)*x(1)+w(4)*x(2) 
c .                          x(npts) = w(1)*x(npts-2)+w(2)*x(npts-1)
c .                                   +w(3)*x(npts)  +w(4)*x(1) 
c .               kopt = 0 : set unsmoothed beginning and end pts to xmsg       
c .                          e.g.,  nwgt=3 , x(1) = xmsg and x(npts) = xmsg    
c .                                        , x(2) = w(1)*x(1)+w(2)*x(2)+w(3)*x(3)
c .                          e.g.,  nwgt=4 , x(1) = xmsg and x(2) = xmsg 
c .                                        , x(3) = w(1)*x(1)+w(2)*x(2)
c .                                                +w(3)*x(3)+w(4)*x(4)
c .                                     x(npts-1) = xmsg, x(npts) = xmsg    
c .               kopt > 0 : utilize reflective (symmetric) conditions
c .                          e.g.,  nwgt=3 at n=1 and n=npts        
c .                          x(1)    = w(1)*x(2)+w(2)*x(1)+w(3)*x(2)
c .                          x(npts) = w(1)*x(npts-1)+w(2)*x(npts)+w(3)*x(npts-1) 
c .                          e.g.,  nwgt=4 at n=1 and n=npts        
c .                          x(1)    = w(1)*x(3)+w(2)*x(2)+w(3)*x(1)+w(4)*x(2)
c .                          x(npts) = w(1)*x(npts-2)+w(2)*x(npts-1)
c .                                   +w(3)*x(npts)  +w(4)*x(npts-1)
c .   xmsg      - missing/special value
c .               also; if kopt=0 set end points to this special value     
c .   work      - work vector 
c .   lw        - length of work (=npts+2*N where N=nwgt/2)
c .   ier       - error flag                          
c .               ier = 0 : no errors                
c .               ier = -11 : npts @ 0              
c .               ier = -12 : npts < nwgt         
c .               ier = -13 : nwgt is not odd    
                                                 
      real x(1:npts) , work(lw), wgt(nwgt)
                                                
      ier = 0                                 
      if (npts.lt.1) ier = -11               
      if (nwgt.gt.npts) ier = -12          
      if (ier.lt.0) return                 
  
      if (nwgt.le.1) then
          do n=1,npts
             work(n) = x(n)    ! historical reasons
          end do
          return              
      end if
 
      nav   = nwgt          
      nav2  = nav/2        
      lwork = npts+2*nav2
      noe   = 0
      if (mod(npts,2).eq.0) noe = 1

      do n=1,lwork
         work(n) = xmsg                   ! preset to xmsg
      enddo
      
      do n=1,npts     
         work(nav2+n) = x(n)              ! stuff x into middle of work 
      enddo

      wsum = 0.0
      do n=1,nwgt
         wsum = wsum+wgt(n)   
      end do
      if (wsum.gt.1.0) then
          wsum = 1./wsum
      else
          wsum = 1.0                      ! This is a "do nothing" op
      end if
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

c c c if (mod(nwgt,2).eq.0) then
c          print *," "
c          do n=1,lwork
c             write (*,"('wrunavx77: ',3i4,3f8.2))") kopt,nav2,n,work(n) 
c          end do
c c c end if
      
      do n=1,npts  
         kmsg  = 0 
         sum   = 0.                      
         nmid  = n+nav2
         mstrt = nmid-nav2
         mlast = mstrt+nav-1
        do m=mstrt,mlast
           if (work(m).ne.xmsg) then 
               sum = sum+work(m)*wgt(m-mstrt+1) 
c c c          if (mod(nwgt,2).eq.0) then
c c c          print *,' DEBUG: kopt,n,nmid,mstrt,mlast,m,work(m)'
c c c+                ,',wgt(m-mstrt+1),work(m)*wgt(m-mstrt+1),sum='
c c c+                ,         kopt,n,nmid,mstrt,mlast,m,work(m)
c c c+                ,  wgt(m-mstrt+1),work(m)*wgt(m-mstrt+1),sum
c c c          end if

           else
               kmsg = kmsg+1
           endif  
        enddo       

         if (kmsg.eq.0) then
             x(n) = sum*wsum    ! all values 
         else
             x(n) = xmsg        ! msg values encountered
         endif
      enddo        

      return  
      end    
