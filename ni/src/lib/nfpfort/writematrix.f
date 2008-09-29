C NCLFORTSTART

      subroutine writematrixi(fname,nrow,ncol,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: writematrixi(fname, x, fmtx, title, titsp, iopt)

      character*(*) title, fname, fmtx
      integer       ncol,nrow,iopt,titsp
      integer       x(ncol,nrow)

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
                 write (31,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          close (31)
      else
c write to standard out
          if (.not.(title.eq." ")) then
              write (*,titfmt) title
          end if

          print *, " "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          print *," "
          call flush(6)
      end if

      return
      end

C NCLFORTSTART

      subroutine writematrixf(fname,nrow,ncol,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: writematrixf(fname, x, fmtx, title, titsp, iopt)

      character*(*) title, fname, fmtx

      integer titsp
      character*2 titspc

      integer       ncol,nrow,iopt
      real          x(ncol,nrow)

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

          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")" 
              do nr=1,nrow
                 write (31,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")" 
              do nr=1,nrow
                 write (31,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          close (31) 
      else
c write to standard out
          if (.not.(title.eq." ")) then
              write (*,titfmt) title
          end if
          
          print *, " "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")" 
              do nr=1,nrow
                 write (*,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")" 
              do nr=1,nrow
                 write (*,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          print *," "
          call flush(6)
      end if

      return
      end



C NCLFORTSTART

      subroutine writematrixd(fname,nrow,ncol,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: writematrixd(fname, x, fmtx, title, titsp, iopt)

      character*(*) title, fname, fmtx
      character*2   titspc
      integer       ncol,nrow,iopt,titsp
      double precision       x(ncol,nrow)

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

          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          close (31)
      else
c write to standard out
          if (.not.(title.eq." ")) then
              write (*,titfmt) title
          end if

          print *, " "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          print *," "
          call flush(6)
      end if

      return
      end

C NCLFORTSTART
      subroutine writematrixb(fname,nrow,ncol,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: writematrixb(fname, x, fmtx, title, titsp, iopt)

      character*(*) title, fname, fmtx
      integer       ncol,nrow,iopt,titsp
      integer*1     x(ncol,nrow)

C NCLEND
c ------------------------------------------------------
c NOTE: integer*1  is non-standard fortran
c       The f77 standard does not support byte or short
c       However, these are common extensions and are supported
c       by g77.
c ------------------------------------------------------

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
                 write (31,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          close (31)
      else
c write to standard out
          if (.not.(title.eq." ")) then
              write (*,titfmt) title
          end if

          print *, " "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          print *," "
          call flush(6)
      end if

      return
      end

C NCLFORTSTART

      subroutine writematrixs(fname,nrow,ncol,x,fmtx,title,titsp,iopt)
      implicit      none

C NCL procedure: writematrixi(fname, x, fmtx, title, titsp, iopt)

      character*(*) title, fname, fmtx
      integer       ncol,nrow,iopt,titsp
      integer*2     x(ncol,nrow)

C NCLEND
c ------------------------------------------------------
c NOTE: integer*2  is non-standard fortran
c       The f77 standard does not support byte or short
c       However, these are common extensions and are supported
c       by g77.
c ------------------------------------------------------

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
                 write (31,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (31,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          close (31)
      else
c write to standard out
          if (.not.(title.eq." ")) then
              write (*,titfmt) title
          end if

          print *, " "
          if (iopt.eq.0) then
              newfmt = "(" // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) (x(nc,nr),nc=1,ncol)
              end do
          else
              newfmt = "(i5," // "1X," // fmtx // ")"
              do nr=1,nrow
                 write (*,newfmt) nr-1, (x(nc,nr),nc=1,ncol)
              end do
          end if
          print *," "
          call flush(6)
      end if

      return
      end
