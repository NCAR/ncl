C NCLFORTSTART

      subroutine writematrixi(fname,ncol,nrow,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: writematrixi(fname, x, fmtx, title, titsp, iopt)

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
          if (.not.(title.eq." ")) then
              write (31,titfmt) title
          end if

          newfmt = "(" // fmtx // ")"

          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) nr-1, (x(nr,nc),nc=1,ncol)
              end do
          end if
          close (31)
      else
c write to standard out
          if (.not.(title.eq." ")) then
              write (*,titfmt) title
          end if

          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) nr-1, (x(nr,nc),nc=1,ncol)
              end do
          end if
          print *," "
      end if

      return
      end

C NCLFORTSTART

      subroutine writematrixf(fname,ncol,nrow,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: writematrixf(fname, x, fmtx, title, titsp, iopt)

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
          if (.not.(title.eq." ")) then
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
              newfmt = "(i5," // "1X," // fmtx // ")" 
              do nr=1,nrow
                 write (31,newfmt) nr-1, (x(nr,nc),nc=1,ncol)
              end do
          end if
          close (31) 
      else
c write to standard out
          if (.not.(title.eq." ")) then
              write (*,titfmt) title
          end if
          
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")" 
              do nr=1,nrow
                 write (*,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")" 
              do nr=1,nrow
                 write (*,newfmt) nr-1, (x(nr,nc),nc=1,ncol)
              end do
          end if
          print *," "
      end if

      return
      end



C NCLFORTSTART

      subroutine writematrixd(fname,ncol,nrow,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: writematrixd(fname, x, fmtx, title, titsp, iopt)

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
          if (.not.(title.eq." ")) then
              write (31,titfmt) title
          end if

          newfmt = "(" // fmtx // ")"

          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) nr-1, (x(nr,nc),nc=1,ncol)
              end do
          end if
          close (31)
      else
c write to standard out
          if (.not.(title.eq." ")) then
              write (*,titfmt) title
          end if

          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) (x(nr,nc),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) nr-1, (x(nr,nc),nc=1,ncol)
              end do
          end if
          print *," "
      end if

      return
      end
