subroutine red2reg(nred,red,nlat,jlon,nlon,reg,ierror)
implicit none
 
integer, intent(in) :: nred
integer, intent(in) :: nlat
integer, intent(in) :: nlon

real(kind=8), intent(in)  :: red(nred)
integer, intent(in)       :: jlon(nlat)
real(kind=8), intent(out) :: reg(nlat,nlon)
integer, intent(out)      :: ierror

! Local Variables
      
real(kind=8), dimension(:,:), allocatable :: atmp
real(kind=8), dimension(:), allocatable   :: work
real(kind=8), dimension(:), allocatable   :: wsave

integer :: jlat
integer :: jptr

! ==========================================
! Error tests go here
! ==========================================
      
ierror=0
      
! ==========================================
! Forward FFT (real-> complex) the reduced 
! grid data_values and store in the 
! regular grid array
! ==========================================

reg(:,:) = 0.0_8
jptr=1
do jlat=1,nlat

   ! recompute FFT work arrays only when longitude changes
   
   if (jlat==1) then
      allocate(wsave(2*jlon(jlat)+15))
      allocate(work(jlon(jlat)))
      allocate(atmp(1,jlon(jlat)))
            
      call hrffti(jlon(jlat),wsave)
   else if (jlon(jlat) .ne. jlon(jlat-1)) then

      deallocate(wsave)
      deallocate(work)
      deallocate(atmp)

      allocate(wsave(2*jlon(jlat)+15))
      allocate(work(jlon(jlat)))
      allocate(atmp(1,jlon(jlat)))

      call hrffti(jlon(jlat),wsave)
   end if
   atmp(1,1:jlon(jlat)) = red(jptr:jlon(jlat)+jptr-1)
   call hrfftf(1,jlon(jlat),atmp,1,wsave,work)
   reg(jlat,1:jlon(jlat)) = atmp(1,1:jlon(jlat))/jlon(jlat) ! rescale wave numbers
   jptr=jptr+jlon(jlat)
end do
      
deallocate(wsave)
deallocate(work)
deallocate(atmp)

! =============================
! Inverse FFT (complex to real)
! =============================
              
allocate(wsave(2*nlon+15))
allocate(work(nlat*nlon))

call hrffti(nlon,wsave)
call hrfftb(nlat,nlon,reg,nlat,wsave,work)
      
deallocate(wsave)
deallocate(work)

end subroutine red2reg


subroutine reg2red(nlat,jlon,nlon,reg,nred,red,ierror)
implicit none

integer, intent(in) :: nlat
integer, intent(in) :: nlon
integer, intent(in) :: nred
      
integer, intent(in)                 :: jlon(nlat)
real(kind=8), intent(in)   ::   reg(nlat,nlon)
real(kind=8), intent(out)  :: red(nred)

integer, intent(out)                :: ierror

! Local Variables
      
real(kind=8), dimension(:,:), allocatable :: atmp, reg1
real(kind=8), dimension(:), allocatable :: work
real(kind=8), dimension(:), allocatable :: wsave

integer :: jlat, i
integer :: jptr
      
allocate(reg1(nlat,nlon))
reg1 = reg
! ===================================================
! Error tests go here.
! ===================================================
      
ierror=0
      
! ===================================================
! Forward FFT (complex to real) the regular grid data
! ===================================================
              
allocate(wsave(2*nlon+15))
allocate(work(nlat*nlon))

call hrffti(nlon,wsave)
call hrfftf(nlat,nlon,reg1,nlat,wsave,work)
      
deallocate(wsave)
deallocate(work)
      
! ==========================================
! Forward FFT (real-> complex) the reduced 
! grid data_values and store in the 
! regular grid array
! ==========================================

red(:) = 0.0_8
jptr=1
do jlat=1,nlat

   ! recompute FFT work arrays only when longitude changes
   
   if (jlat==1) then
      allocate(wsave(2*jlon(jlat)+15))
      allocate(work(jlon(jlat)))
      allocate(atmp(1,jlon(jlat)))
            
      call hrffti(jlon(jlat),wsave)
   else if (jlon(jlat) .ne. jlon(jlat-1)) then

      deallocate(wsave)
      deallocate(work)
      deallocate(atmp)

      allocate(wsave(2*jlon(jlat)+15))
      allocate(work(jlon(jlat)))
      allocate(atmp(1,jlon(jlat)))

      call hrffti(jlon(jlat),wsave)
   end if
         
   atmp(1,1:jlon(jlat)) = reg1(jlat,1:jlon(jlat))/nlon  ! rescale wave numbers
   call hrfftb(1,jlon(jlat),atmp,1,wsave,work)
   red(jptr:jlon(jlat)+jptr-1) = atmp(1,1:jlon(jlat))
         
   jptr=jptr+jlon(jlat)
end do
      
deallocate(wsave)
deallocate(work)
deallocate(atmp)
deallocate(reg1)

end subroutine reg2red


subroutine find_diff(nlats,ngpts,ngpts_perlats,u,un, &
          v,vn,ue,ve,iue,ive,ulat,ulon,vlat,vlon)
implicit none
integer, intent(in) :: nlats,ngpts,ngpts_perlats(nlats)
real(8), dimension(ngpts), intent(in) :: u,un,v,vn
real(8), intent(out) :: ue,ve
integer, intent(out) :: iue, ive, ulat,ulon,vlat,vlon
real(8) :: udf, vdf, uet, vet
integer :: gp, jlat, klon
ue = 0.d0
ve = 0.d0
gp = 0
do jlat = 1, nlats
  do klon = 1, ngpts_perlats(jlat)
    gp = gp + 1
    udf = u(gp) - un(gp)
    uet = udf/u(gp)
    if(abs(uet).ge.abs(ue)) then
       ue = uet
       iue = gp
       ulat = jlat
       ulon = klon
    endif
    vdf = v(gp) - vn(gp)
    vet = vdf/v(gp)
    if(abs(vet).ge.abs(ve)) then
       ve = vet
       ive = gp
       vlat = jlat
       vlon = klon
    endif
  end do
end do
end subroutine find_diff
