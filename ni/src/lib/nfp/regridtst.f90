program read_redn80_uv

implicit none

integer, parameter                     :: nlats = 160
integer, parameter                     :: ngpts = 35718
integer, dimension(nlats)              :: ngpts_perlats
!integer, dimension(nlats)              :: lats
real(8), dimension(nlats)              :: lats
real(8), dimension(ngpts)              :: u, un
real(8), dimension(ngpts)              :: v, vn
integer                                :: gp
integer                                :: jlat
integer                                :: klon, nlons, ierror
real(8), dimension(:,:), allocatable :: ur, vr, br, bi, cr, ci
real(8), dimension(:), allocatable :: wvhagc, work
real(8)  :: ue, ve, uet, vet
integer :: iue,ive,ulat,ulon,vlat,vlon,mdab,ndab,l1,l2,lvhagc, &
           lwork, i

ngpts_perlats = (/18,25,36,40,45,54,60,64,72,72,80,90,96,100,108,              &
                   120,120,128,135,144,144,150,160,160,180,180,180,192,192,200, &
                   200,216,216,216,225,225,240,240,240,256,256,256,256,288,288, &
                   288,288,288,288,288,288,288,300,300,300,300,320,320,320,320, &
                   320,320,320,320,320,320,320,320,320,320,320,320,320,320,320, &
                   320,320,320,320,320,320,320,320,320,320,320,320,320,320,320, &
                   320,320,320,320,320,320,320,320,320,320,320,320,320,320,300, &
                   300,300,300,288,288,288,288,288,288,288,288,288,256,256,256, &
                   256,240,240,240,225,225,216,216,216,200,200,192,192,180,180, &
                   180,160,160,150,144,144,135,128,120,120,108,100,96,90,80,    &
                   72,72,64,60,54,45,40,36,25,18/)

lats = (/ 89.1416,  88.0294,  86.9108,  85.7906,  84.6699,  83.5489,  &
            82.4278,  81.3066,  80.1853,  79.0640,  77.9426,  76.8212,  &
            75.6998,  74.5784,  73.4570,  72.3356,  71.2141,  70.0927,  &
            68.9712,  67.8498,  66.7283,  65.6069,  64.4854,  63.3639,  &
            62.2425,  61.1210,  59.9995,  58.8780,  57.7566,  56.6351,  &
            55.5136,  54.3921,  53.2707,  52.1492,  51.0277,  49.9062,  &
            48.7847,  47.6632,  46.5418,  45.4203,  44.2988,  43.1773,  &
            42.0558,  40.9343,  39.8129,  38.6914,  37.5699,  36.4484,  &
            35.3269,  34.2054,  33.0839,  31.9624,  30.8410,  29.7195,  &
            28.5980,  27.4765,  26.3550,  25.2335,  24.1120,  22.9905,  &
            21.8690,  20.7476,  19.6261,  18.5046,  17.3831,  16.2616,  &
            15.1401,  14.0186,  12.8971,  11.7756,  10.6542,   9.5327,  &
            8.4112,   7.2897,   6.1682,   5.0467,   3.9252,   2.8037,   &
            1.6822,   0.5607,                                           &
            -0.5607, -1.6822,                                           &
            -2.8037, -3.9252, -5.0467, -6.1682, -7.2897, -8.4112,       & 
            -9.5327, -10.6542, -11.7756, -12.8971, -14.0186, -15.1401,  &
            -16.2616, -17.3831, -18.5046, -19.6261, -20.7476, -21.8690, &
            -22.9905, -24.1120, -25.2335, -26.3550, -27.4765, -28.5980, &
            -29.7195, -30.8410, -31.9624, -33.0839, -34.2054, -35.3269, &
            -36.4484, -37.5699, -38.6914, -39.8129, -40.9343, -42.0558, &
            -43.1773, -44.2988, -45.4203, -46.5418, -47.6632, -48.7847, &
            -49.9062, -51.0277, -52.1492, -53.2707, -54.3921, -55.5136, &
            -56.6351, -57.7566, -58.8780, -59.9995, -61.1210, -62.2425, &
            -63.3639, -64.4854, -65.6069, -66.7283, -67.8498, -68.9712, &
            -70.0927, -71.2141, -72.3356, -73.4570, -74.5784, -75.6998, &
            -76.8212, -77.9426, -79.0640, -80.1853, -81.3066, -82.4278, &
            -83.5489, -84.6699, -85.7906, -86.9108, -88.0294, -89.1416 /)

open(unit=10,file='UV_redn80_500mb_19570900.asc',access='sequential',form='formatted')

! Read in u:
gp = 0
do jlat = 1, nlats
  do klon = 1, ngpts_perlats(jlat)
    gp = gp + 1
    read(10,'(f10.5)') u(gp)
  end do
end do

! Read in v:
gp = 0
do jlat = 1, nlats
  do klon = 1, ngpts_perlats(jlat)
    gp = gp + 1
    read(10,'(f10.5)') v(gp)
  end do
end do

close(10)

nlons = 0
do jlat = 1, nlats
  nlons = max( nlons, ngpts_perlats(jlat))
end do
allocate( ur(nlats, nlons), vr(nlats,nlons) )

!-- Use Rich's routine to convert reduced to regular 
call red2reg(ngpts,u,nlats,ngpts_perlats,nlons,ur,ierror)
call red2reg(ngpts,v,nlats,ngpts_perlats,nlons,vr,ierror)

!-- Use Rich's routine to convert regular to reduced 
call reg2red(nlats,ngpts_perlats,nlons,ur,ngpts,un,ierror)
call reg2red(nlats,ngpts_perlats,nlons,vr,ngpts,vn,ierror)

!-- Compare the difference in this roundtrip and print
call find_diff(nlats,ngpts,ngpts_perlats,u,un,v,vn, &
     ue,ve,iue,ive,ulat,ulon,vlat,vlon)
print *,'Error after going from reduced to regular and '
print *,'back to reduced grid using routines written by Rich'
print *,'applied on data provided by Dave'
print *,'Max rel error in u ',ue,' for the value ',u(iue),' at (lat,lon)',ulat,ulon
print *,'Max rel error in v ',ve,' for the value ',v(ive),' at (lat,lon)',vlat,vlon

!-- Now let us analyse and sythensize this regular grid to
!-- make sure there is no gotchas somewhere else.
if ( mod(nlons,2) .eq. 0 ) then
  mdab = min(nlats,nlons/2)
  l1 = min(nlats,nlons/2)
else
  mdab = min(nlats,(nlons+1)/2)
  l1 = min(nlats,(nlons+1)/2)
endif

if ( mod(nlats,2) .eq. 0 ) then
  l2 = nlats/2
else
  l2 = (nlats+1)/2
endif

ndab = nlats

allocate(br(mdab,ndab),bi(mdab,ndab),cr(mdab,ndab),ci(mdab,ndab))

lvhagc = 4*nlats*l2+3*max(l1-2,0)*(2*nlats-l1-1)+nlons+l2+15
lwork = max(2*nlats*(2*nlons+3*l2), nlats*(2*nlons+max(6*l2,nlons)))
allocate( wvhagc(lvhagc), work(lwork) )

!-- Analyse
call vhagci(nlats,nlons,wvhagc,lvhagc,work,lwork,ierror)
call vhagc(nlats,nlons,0,1,ur,vr,nlats,nlons,br,bi,cr,ci, &
                 mdab,ndab,wvhagc,lvhagc,work,lwork,ierror)

!-- Synthesize to regular
call vhsgci(nlats,nlons,wvhagc,lvhagc,work,lwork,ierror)
call vhsgc(nlats,nlons,0,1,ur,vr,nlats,nlons,br,bi,cr,ci, &
                 mdab,ndab,wvhagc,lvhagc,work,lwork,ierror)

!-- Once again use Rich's routine to convert regular to reduced 
call reg2red(nlats,ngpts_perlats,nlons,ur,ngpts,un,ierror)
call reg2red(nlats,ngpts_perlats,nlons,vr,ngpts,vn,ierror)

!-- Compare the difference in this roundtrip and print
call find_diff(nlats,ngpts,ngpts_perlats,u,un,v,vn, &
     ue,ve,iue,ive,ulat,ulon,vlat,vlon)
print *,'Error after going from reduced to regular and '
print *,'analysis and followed by a synthesis and then'
print *,'back to reduced grid using routines written by Rich'
print *,'applied on data provided by Dave'
print *,'Max rel error in u ',ue,' for the value ',u(iue),' at (lat,lon)',ulat,ulon
print *,'Max rel error in v ',ve,' for the value ',v(ive),' at (lat,lon)',vlat,vlon

end program read_redn80_uv



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
