C NCLFORTSTART

      subroutine print2df(fname,ncol,nrow,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: print2df(fname, x, fmtx, title, titsp, iopt)

      character*(*) title, fname, fmtx

      integer titsp
      character*2 titspc

      integer       ncol,nrow,iopt
      real          x(nrow,ncol)

C NCLEND

      integer       nr, nc
      character*96  newfmt, titfmt

      if (titsp.gt.0) then
          write(titspc,"(i2)") titsp
          titfmt = "(" // titspc // "x, a)" 
      else
          titfmt = "(x, a)" 
      end if

      if (fname.ne."*") then 
c write to a file
          open  (31,file=fname,form="formatted")
          if (.not.(title.eq."" .or. title.eq." ")) then
              write (31,titfmt) title
          end if

          newfmt = "(" // fmtx // ")" 

          write(31, titfmt) " "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")" 
              do nr=1,nrow
                 write (31,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "2X" // fmtx // ")" 
              do nr=1,nrow
                 write (31,newfmt) nr, (x(nr,nc),nc=1,ncol)
              end do
          end if
          close (31) 
      else
c write to standard out
          if (.not.(title.eq."" .or. title.eq." ")) then
              write (*,titfmt) title
          end if
          
          print *," "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")" 
              do nr=1,nrow
                 write (*,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "2X" // fmtx // ")" 
              do nr=1,nrow
                 write (*,newfmt) nr, (x(nr,nc),nc=1,ncol)
              end do
          end if
          print *," "
      end if

      return
      end


C NCLFORTSTART

      subroutine print2di(fname,ncol,nrow,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: print2df(fname, x, fmtx, title, titsp, iopt)

      character*(*) title, fname, fmtx
      integer       ncol,nrow,iopt,titsp
      integer       x(nrow,ncol)

C NCLEND

      integer       nr, nc
      character*96  newfmt, titfmt
      character*2   titspc

      if (titsp.gt.0) then
          write(titspc,"(i2)") titsp
          titfmt = "(" // titspc // "x, a)"
      else
          titfmt = "(x, a)"
      end if

      if (fname.ne."*") then
c write to a file
          open  (31,file=fname,form="formatted")
          if (.not.(title.eq."" .or. title.eq." ")) then
              write (31,titfmt) title
          end if

          newfmt = "(" // fmtx // ")"

          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) nr, (x(nr,nc),nc=1,ncol)
              end do
          end if
          close (31)
      else
c write to standard out
          if (.not.(title.eq."" .or. title.eq." ")) then
              write (*,titfmt) title
          end if

          print *," "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) nr, (x(nr,nc),nc=1,ncol)
              end do
          end if
          print *," "
      end if

      return
      end


C NCLFORTSTART

      subroutine print2dd(fname,ncol,nrow,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: print2df(fname, x, fmtx, title, titsp, iopt)

      character*(*) title, fname, fmtx
      integer       ncol,nrow,iopt,titsp
      double precision       x(nrow,ncol)

C NCLEND
      integer       nr, nc
      character*96  newfmt, titfmt
      character*2   titspc

      if (titsp.gt.0) then
          write(titspc,"(i2)") titsp
          titfmt = "(" // titspc // "x, a)"
      else
          titfmt = " "
      end if

      if (fname.ne."*") then

c write to a file
          open  (31,file=fname,form="formatted")
          if (.not.(title.eq."" .or. title.eq." ")) then
              write (31,titfmt) title
          end if

          newfmt = "(" // fmtx // ")"

          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) nr, (x(nr,nc),nc=1,ncol)
              end do
          end if
          close (31)
      else
c write to standard out
          if (.not.(title.eq."" .or. title.eq." ")) then
              write (*,titfmt) title
          end if

          print *," "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) nr, (x(nr,nc),nc=1,ncol)
              end do
          end if
          print *," "
      end if

      return
      end
