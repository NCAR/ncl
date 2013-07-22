subroutine plotfmt_open(fname,istatus)

  implicit none

  integer :: istatus, funit
  character(*) :: fname

  integer :: io_status
  logical :: is_used

  istatus = 0

  ! Open intermediate file
  funit = 10
  open(funit,file=fname,status='old',form='unformatted',convert='big_endian',iostat=io_status)

  if(io_status > 0) istatus = 1

  return

end subroutine plotfmt_open
