
C NCLFORTSTART
      subroutine deof11( d, nx, nt, nmodes, icovcor, dmsg,
     &		         eigenvalues, eigenvectors, variance,
     &                   princomp)
c minimal argument list
        implicit none

	integer	nx, nt, nmodes, icovcor
	double precision d(nx,nt), eigenvalues(nmodes), 
     &		eigenvectors(nx,nmodes), variance(nmodes)
        double precision princomp(nt,nmodes), dmsg
C NCLEND
        integer iminnxnt, imaxnxnt
        integer n

c need  iminnxnt and imaxnxnt for automatic allocation

        iminnxnt = min(nx,nt)
        imaxnxnt = max(nx,nt)
        
	call deof22(d, nx, nt, nmodes, icovcor, dmsg,
     &		    eigenvalues, eigenvectors, variance,
     &		    iminnxnt, imaxnxnt, princomp )

        return
        end
c ----------------------------------------------------------------
      subroutine deof22(d, nx, nt, nmodes, icovcor, dmsg,
     &		         eigenvalues, eigenvectors, variance,
     &		         iminnxnt, imaxnxnt, princomp)
        implicit none

        integer nx, nt, nmodes, icovcor, iminnxnt, imaxnxnt
        double precision d(nx,nt), eigenvalues(nmodes),                                          
     &          eigenvectors(nx,nmodes), variance(nmodes)
        double precision princomp(nt,nmodes), dmsg

c local and  automatic arrays
        double precision cumvariance(nmodes),
     &          spacked(iminnxnt*(iminnxnt+1)/2), evals(iminnxnt),                                  
     &          evecs(iminnxnt,nmodes), rlawork(8*iminnxnt),                                        
     &          tentpcs(imaxnxnt,nmodes)
        double precision  sqrootweights(nx)                                         
        integer ilawork(5*iminnxnt), ifail(iminnxnt)    
        integer n

c NCL expects the user will do weighting externally

        do n=1,nx
           sqrootweights(n) = 1.d0
        end do

        call deof( d, nx, nt, nmodes, icovcor, dmsg,
     &		eigenvalues, eigenvectors, princomp, variance, cumvariance,
     &		iminnxnt, imaxnxnt, spacked, evals, evecs, rlawork, 
     &		ilawork, ifail, tentpcs, sqrootweights )
        return
        end
c ----------------------------------------------------------------
c --- Original Scripps Code: modified for missing values [dmsg]
c ---                        Dennis Shea [Aug 2005]
c ----------------------------------------------------------------
      subroutine deof( data, nx, nt, nmodes, icovcor, dmsg,
     &		eigenvalues, eigenvectors, princomp, variance, cumvariance,
     &		iminnxnt, imaxnxnt, spacked, evals, evecs, rlawork, 
     &		ilawork, ifail, tentpcs, sqrootweights )

c	------------------------------------------------------------------
c	This routine generates the Empirical Orthogonal Functions (EOFs)
c	for a given time-space data set.  (Note that although the routine
c	is set up as if the data is exactly two dimensional (X,T), that
c	you can compute the EOFs of 3-D (X,Y,T) fields simply by concat-
c	enating the data along the X,Y axes into one big 2-D array and
c	then computing the EOFs of that 2-D array).  The "typical" 
c	normalization is applied, i.e., the magnitude of the eigenvectors
c	is scaled to be equal to 1.
c
c	David W. Pierce
c	dpierce@ucsd.edu
c	Scripps Institution of Oceanography
c	Climate Research Division, 0224
c	Jan 29, 1996	
c	
c	Inputs:
c
c		data(nx,nt): data to compute the EOFs of.  THIS MUST
c			BE ANOMALIES.
c
c		nx, nt: number of space (nx) and time (nt) points in the
c			input data array.
c
c		nmodes: number of modes (EOFs) to compute.
c		
c		icovcor: if =0, then compute using covariance matrix;
c			 if =1, then compute using correlation matrix.
c			See, for example, discussion in Daniel S. Wilks
c			1995, "Statistical Methods in the Atmospheric
c			Sciences", p. 383 for a discussion.
c
c		iminnxnt: the SMALLER of 'nx' and 'nt'.
c
c		imaxnxnt: the LARGER of 'nx' and 'nt'.
c
c		spacked(iminnxnt*(iminnxnt+1)/2): workspace.  This is used to 
c			store the packed covariance or correlation matrix.
c
c		evals(iminnxnt): workspace.  This is used to store the
c			complete series of eigenvalues.  I suppose you
c			could access this if you wanted to, remembering
c			that a) it's more modes than you asked for; b)
c			they are in ASCENDING order rather than descending.
c
c		evecs(iminnxnt,nmodes): workspace.  This is used to store
c			the (possibly switched) eigenvectors, which are
c			in ascending order.
c
c		rlawork(8*iminnxnt): Real LAPACK workspace.
c
c		ilawork(5*iminnxnt): Integer LAPACK workspace.
c
c		ifail(iminnxnt): Integer LAPACK workspace.  This is used
c			to indicate which eigenvectors didn't converge,
c			if that happened.
c
c		tentpcs(imaxnxnt,nmodes): Real workspace.  This is 
c			used to store the 'tentative' principal components.
c
c		sqrootweights(nx) : NOT USED IN NCL IMPLEMENTATION [set to 1.0]
c                       the SQUARE ROOT of the areal weighting to
c			use.  Just set this to all 1.0's if you don't care
c			about areal weighting.  I think you can set places
c			which should be ignored to zero to have them not
c			participate in the calculation, thus implementing a
c			crude form of data masking.
c
c               dmsg    missing code
c
c	Outputs:
c
c		eigenvalues(nmodes): the computed eigenvalues of
c			the data, the largest returned first.
c
c		eigenvectors(nx,nmodes): the computed eigenvectors
c			of the data, the most important returned first.
c
c		princomp(nt,nmodes): the principal components of 
c			the data.
c
c		variance(nmodes): the percent of variance explained
c			by each mode.
c
c		cumvariance(nmodes): the cumulative percent of 
c			variance explained by each mode.  This is
c			a bit redundant -- it's just the sum
c			of "variance".
c			
c	Method:
c
c	EOFs are simply the eigenvectors of the covariance (or correlation)
c	matrix.  So form the proper matrix from the data and pass it to
c	a LAPACK routine which calculates eigenvectors/eigenvalues.  Then
c	calculate the principal components explicitly using the fact that
c	the principal component for mode M at time T is just the projection 
c	of the data at time T onto eigenvector M.  
c
c	There is a slight complication for efficiency's sake.  That is, 
c	we often have nx >> nt, i.e., there are many more spatial than
c	temporal points.  The traditional correlation/covariance matrix
c	is size (nx,nx), which can be huge.  Think, for example, of a 3-D
c	field of size (100,100,120) which has been concatenated into a
c	2-D field of size (10000,120).  The traditional covariance/correlation
c	matrix in this case is size (10000,10000)!  That's way too big to 
c	easily work with.  So, following the discussion in Preisendorfer
c	(1988, "Principal Component Analysis in Meteorology and Oceanography",
c	p. 64) we work, in such cases, in the "dual" of the space.  All
c	that means is that we logically switch X and T; the 
c	covariance/correlation matrix for the example given above is then
c	size (120,120), which is much easier and more efficient to work
c	with.  If you do this kind of switch, the basic idea is that
c	the eigenvectors of the switched matrix are the principal components
c	of the original matrix, and the principal components of the switched
c	matrix are the eigenvectors of the original matrix.  Which is to
c	say, if you switch T and X to begin with, the final result is that
c	the T dependence is in the X part and the X dependence is in the
c	T part.  There is also a normalization which has to be applied--
c	see Preisendorfer for details.
c	------------------------------------------------------------------

	implicit none

	integer	covariance, correlation
	parameter( covariance=0, correlation=1 )  ! possible values of 'icovcor'
	
c	-----------------
c	Passed parameters
c	-----------------
	integer	nx, nt, nmodes, icovcor, iminnxnt, imaxnxnt
        integer ifail(iminnxnt)
	double precision data(nx,nt), eigenvalues(nmodes), 
     &		eigenvectors(nx,nmodes), princomp(nt,nmodes), 
     &		variance(nmodes), cumvariance(nmodes),
     &		spacked(iminnxnt*(iminnxnt+1)/2), evals(iminnxnt), 
     &		evecs(iminnxnt,nmodes), rlawork(8*iminnxnt), 
     &		tentpcs(imaxnxnt,nmodes), sqrootweights(nx)
	double precision dmsg, deps
	integer ilawork(5*iminnxnt)

c	---------------
c	Local variables
c	---------------
	logical		doswitched
	integer		orderofs, i, j, jascending, jdescending
	double precision sum, fact, totvar
	character*1	jobz, range, uplo	! for LAPACK routine
	integer		m, n, il, iu, ldz, info	! for LAPACK routine
	double precision vl, vu, abstol		! for LAPACK routine
	integer 	lwork, mode

        logical DEBUG
        DEBUG = .false.

        if (DEBUG) then
            print *, 'entering deof with nx, nt, nmodes=',nx,nt,nmodes
c c c       print *, 'sqrootweights:',sqrootweights(1:10)
        end if

c	----------------------------------------------
c	Weight the data by the square root of the area
c	----------------------------------------------
        do j=1, nt
          do i=1, nx
             if (data(i,j).ne.dmsg) then
                 data(i,j) = data(i,j) * sqrootweights(i)
             end if
          end do
        end do
	
c	---------------------------------------------------------
c	Figure out whether we should do the 'regular' EOF or the
c	one with switched X and T axes.  The correlation matrix
c	for the regular method is size (nx,nx) and for the 
c	switched method it is size (nt,nt); choose based on which
c	of these is smaller.
c	---------------------------------------------------------
        if (DEBUG) then
            print *, 'figuring switched or not'
        end if

	doswitched = (nx .gt. nt)
	if( doswitched ) then
		orderofs = nt
		if (DEBUG) print *, 'deof: Working in switched mode'
	else
		orderofs = nx
		if (DEBUG) print *, 'deof: Working in unswitched mode'
	endif
	if( orderofs .gt. iminnxnt ) then
		print * , 'Error!  EOF routine must be supplied '
		print * , 'with enough workspace; passed parameter'
		print * , 'iminnxnt must be at least ', orderofs
		print * , 'Passed value was iminnxnt=',iminnxnt
		stop 'eof '
	endif
	if( nmodes .gt. orderofs ) then
		print * , 'Error! EOF routine called requesting more'
		print * , 'modes than exist!  Request=',nmodes,' exist=',
     &		           orderofs
		stop 'eof '
	endif

c	-------------------------------------------------
c	Form the covariance or correlation matrix, put it 
c	into 's'.  Note that 's' is always symmetric --
c	the correlation between X and Y is the same as 
c	between Y and X -- so use packed storage for 's'.
c	The packed storage scheme we use is the same as
c	LAPACK uses so we can pass 's' directly to the
c	solver routine: the matrix is lower triangular,
c	and s(i,j) = spacked(i+(j-1)*(2*n-j)/2).
c	-------------------------------------------------
        call deofcovcor( data, nx, nt, icovcor, covariance,
     &		correlation, spacked, doswitched, iminnxnt, dmsg )
c	------------------------------------------------------
c	Now call the LAPACK solver to get the eigenvalues
c	and eigenvectors.  The eigenvalues express the
c	amount of variance explained by the various modes,
c	so choose to return those 'nmodes' modes which 
c	explain the most variance.
c	Dims of arrays:
c		evals  (n)	  ! The calculated eigenvalues
c		evecs  (n,nmodes) ! The calculated eigenvectors
c		rlawork(8*n)
c		ilawork(5*n)
c		ifail  (n)
c	Remember that the calculated eigenvectors may not be
c	the ones we really want if we are doing switched
c	X and T axes.  However the eigen*values* are the same
c	either way, according to Preisendorfer.
c	*NOTE* that the LAPACK routine returns the eigenvalues
c	(and corresponding eigenvectors) in ASCENDING order,
c	but this routine returns them in DESCENDING order; 
c	this will be switched in the final assembly phase,
c	below.
c	------------------------------------------------------
	jobz  = 'V' 	! Both eigenvalues and eigenvectors
	range = 'I'	! Specify range of eigenvalues to get.
	uplo  = 'L'	! 'spacked' has lower triangular part of s
	n     = orderofs
	il    = n - nmodes + 1	! Smallest eigenvalue to get
	iu    = n		! Largest eigenvalue to get
	abstol= 0.0d0		! See LAPACK documentation
	ldz   = n

	lwork = 8*iminnxnt

	do j=1, nmodes
           evals(j) = 0.0d0
          do i=1, iminnxnt
             evecs(i,j) = 0.0d0
	  end do
	end do

        if (DEBUG) then
c c c       print *,'about to call dspevx, spacked=', spacked(1:10)
c c c       print *,'about to call dspevx, spacked=',(spacked(i),i=1,10)   
            do i=1,10
               print *,"spacked: n=",n,"  ",spacked(i)
            end do
        end if

	call dspevx( jobz, range, uplo, n, spacked,
     &		vl, vu, il, iu, abstol, m, evals, evecs, ldz,
     &		rlawork, ilawork, ifail, info )

c	------------------------------
c	Check for LAPACK routine error
c	------------------------------
	if( info .ne. 0 ) then
		if( info .lt. 0 ) then
			print *, 'LAPACK error: argument ',
     &				-info, ' had illegal value'
			stop 'eof'
		else
			print *, 'LAPACK error: ', info, 
     &				'eigenvectors failed to converge!'
			print *, 'Consult the LAPACK docs!'
			stop 'eof'
		endif
	endif

c	------------------------------------------------
c	Make sure that no eigenvalues <= zero.  Besides
c	being mathematically forbidden, this would cause
c	a divide by zero or negative sqrt error later on.
c
c       In the original code, it would exit if there was
c       an eigenvalue less than 0. For our purposes, however,
c       we'll just set the eigenvalue to 0 and let it continue.
c	------------------------------------------------
        DEPS = 1d-6
	do i=1, nmodes
          if(abs(evals(i)) .le. DEPS ) then
c	    print *, 'Error! LAPACK routine returned'
c	    print *, 'eigenvalue <= 0!! ', i, evals(i)
	    print *, 'Warning! LAPACK routine returned eigenvalue <= 0.'
	    print *, 'Setting it to zero...'
            evals(i) = 0.0d0
c	    do j=1, nmodes
c	      print *, j, evals(j)
c  	    end do
c	    print *, 'Note1: This means you may be asking'
c	    print *, 'for more modes than the data supports.'
c	    print *, 'Try reducing the number of requested'
c	    print *, 'modes.'
c  	    print *, 'Note2: missing values can'
c	    print *, 'result in a covariance matrix that'
c	    print *, 'is not positive definite.'  
c	    stop 'eofunc'
          endif
	end do

c	------------------------------------------------
c	Compute the tentative principal components; they
c	are 'tentative' because they might be the PCs 
c	of the switched data.  Put them into 'tentpcs',
c	which is of size (nt,nmodes) if we are doing
c	regular EOFs and (nx,nmodes) if we are doing 
c	switched EOFs.  These PCs come out in order
c	corresponding to the order of 'evecs', which is
c	in ASCENDING order.
c	------------------------------------------------
	call deofpcs( data, nx, nt, nmodes, iminnxnt, 
     &		imaxnxnt, evecs, doswitched, tentpcs, dmsg )

c	--------------------------------------------------
c	Now we have all the pieces to assemble our final 
c	result.  How we actually assemble them depends on
c	whether we are doing switched or unswitched EOFs
c	(except for the eigenVALUES, which are the same
c	either way).
c	--------------------------------------------------
	if( doswitched ) then 
c		------------------------------------------
c		In this case we must switch the principal
c		components and the eigenvectors, applying
c		the proper normalization. 
c		First get the unswitched eigenvectors, 
c		which are the switched (tentative) principal
c		components divided by the square root of the
c		appropriate eigenvalue.  Recall that the
c		LAPACK values are in ASCENDING order while
c		we want them in DESCENDING order; do the
c		switch in this loop.
c		--------------------------------------------
		do jascending=1, nmodes
			jdescending = nmodes - jascending + 1
                        if(abs(evals(jascending)) .le. DEPS ) then
                           fact        = 0.0d0
                        else
                           fact        = 1.0d0/sqrt(evals(jascending))
                        end if
			do i=1, nx
				eigenvectors(i,jdescending) = 
     &			 	  	tentpcs(i,jascending)*fact
			end do
		end do

c		-----------------------------------------------
c		Next get unswitched principal components, which 
c		are the switched eigenvectors multiplied by
c		the appropriate eigenvalues.
c		-----------------------------------------------
		do jascending=1, nmodes
			jdescending = nmodes - jascending + 1
			fact        = sqrt(evals(jascending))
			do i=1, nt
				princomp(i,jdescending) = 
     &					evecs(i,jascending)*fact
			end do
		end do
	else
c		-------------------------------------------------
c		This is the unswitched case, and so it is easier.
c		All we have to do is return things in DESCENDING
c		order despite the fact that LAPACK returns them
c		in ASCENDING order.
c		Do the eigenvectors first...
c		-------------------------------------------------
		do jascending=1, nmodes
			jdescending = nmodes - jascending + 1
			do i=1, nx
				eigenvectors(i,jdescending) =
     &					evecs(i,jascending)
			end do
		end do

c		--------------------------------
c		...then the principal components
c		--------------------------------
		do jascending=1, nmodes
			jdescending = nmodes - jascending + 1
			do i=1, nt
				princomp(i,jdescending) =
     &					tentpcs(i,jascending)
			end do
		end do

			
	endif 

c	--------------------------------------------
c	Do the second half of the areal weighting...
c	--------------------------------------------
	do mode=1, nmodes
	do i=1, nx
		if( sqrootweights(i) .eq. 0.0d0 ) then
			eigenvectors(i,mode) = 0.0d0
		else
			eigenvectors(i,mode) = 
     &				eigenvectors(i,mode) / sqrootweights(i)
		endif
	end do
	end do

c	------------------------------------------------
c	Scale the eigenvectors to have a magnitude of 1;
c	scale the corresponding principal components to
c	reproduce the original data.
c	------------------------------------------------
	do mode=1, nmodes
c		----------------------------
c		Get the normalization factor
c		----------------------------
		sum = 0.0d0
		do i=1, nx
			sum = sum + eigenvectors(i,mode)*eigenvectors(i,mode)
		end do
		fact = sqrt(sum)
c		--------------------------
c		Normalize the eigenvectors
c		--------------------------
                if (fact.eq.0.0d0) then
                   do i=1, nx
			eigenvectors(i,mode) = 0.0d0
                   end do
                else
                   do i=1, nx
			eigenvectors(i,mode) = eigenvectors(i,mode)/fact
                   end do
                end if
c		----------------------------------
c		Normalize the principal components
c		----------------------------------
		do i=1, nt
			princomp(i,mode) = princomp(i,mode)*fact
		end do
	end do

c	-------------------------------------------------
c	Copy over just the requested number of
c	eigenvalues, and calculate the cumulative percent
c	variance explained.  Start by getting the total
c	variance in the field, so we can normalize by 
c	that.
c	-------------------------------------------------
	call deoftotvar( data, nx, nt, totvar, doswitched,
     &		icovcor, covariance, correlation, dmsg )
        if (DEBUG) then
            print *," "
            print *,"totvar=",totvar
            print *," "
        end if

	sum = 0.0d0
	do jascending=nmodes, 1, -1
		jdescending = nmodes - jascending + 1
		eigenvalues(jdescending) = evals(jascending)
		variance(jdescending)    = 
     &			eigenvalues(jdescending)/totvar*100.0d0
		sum = sum + variance(jdescending)
		cumvariance(jdescending) = sum
	end do

	return
	end
c --------------------------------------------------------

	subroutine deofcovcor( data, nx, nt, icovcor, covariance,
     &		correlation, spacked, doswitched, iminnxnt, dmsg )

c	-------------------------------------------------
c	Form the covariance or correlation matrix, put it 
c	into 's'.  Note that 's' is always symmetric --
c	the correlation between X and Y is the same as 
c	between Y and X -- so use packed storage for 's'.
c	The packed storage scheme we use is the same as
c	LAPACK uses so we can pass 's' directly to the
c	solver routine: the matrix is lower triangular,
c	and s(i,j) = spacked(i+(j-1)*(1*n-j)/2).
c
c	Inputs:
c		data(nx,nt): The basic data array.  THIS MUST
c			BE ANOMALIES.
c
c		nx, nt: size of data array
c			[INTEGER]
c
c		icovcor: if .eq. covariance, then calculate
c			the covariance array;
c			 if .eq. correlation, then calculate
c			the correlation array.
c			[INTEGER]
c
c		covariance, correlation: integer values to
c			indicate each of these options.
c			[INTEGER]
c
c		doswitched: if .TRUE., then calculate the
c			'switched' array (which is of size
c			(nt,nt)); if .FALSE., then calculate
c			the normal array of size (nx,nx).
c			[LOGICAL]
c
c		iminnxnt: min(nt,nx).  Used to dimension
c			'spacked'.
c
c	Outputs:
c
c		spacked(iminnxnt*(iminnxnt+1)/2): the covariance 
c			or correlation array.  This is in packed 
c			form corresponding to LAPACK's lower
c			triangular form.  
c
c	David Pierce
c	Scripps Institution of Oceanography
c	Climate Research Division
c	dpierce@ucsd.edu
c	Jan 29, 1996
c	-------------------------------------------------

	implicit none

c	-----------------
c	Passed parameters
c	-----------------
	integer	nx, nt, icovcor, covariance, correlation, iminnxnt
	double precision data(nx,nt), spacked(iminnxnt*(iminnxnt+1)/2)
	double precision dmsg
	logical doswitched

c	---------------
c	Local variables
c	---------------
	integer	i, j, k, npts
	double precision sum, sum2, sum3, fact

	if( nx .le. 1 ) then
		print *, 'covariance: error: nx too small!! nx=', nx
c c c           call exit(-1)
                stop
	endif
	if( doswitched ) then
		do j=1, nt
		do i=j, nt
			sum  = 0.0d0
			sum2 = 0.0d0
			sum3 = 0.0d0
                        npts = 0
			do k=1, nx
                           if (data(k,i).ne.dmsg .and. 
     &                         data(k,j).ne.dmsg) then
				sum  = sum  + data(k,i)*data(k,j)
				sum2 = sum2 + data(k,i)*data(k,i)
				sum3 = sum3 + data(k,j)*data(k,j)
                                npts = npts + 1
                           end if
			enddo
			if( icovcor .eq. covariance ) then
c c c                       fact = 1.0d0/float(nx-1)
                            fact = 1.0d0/float(npts-1)
			else
                            fact = 1.0d0/(sqrt(sum2)*sqrt(sum3))
			endif
                        spacked(i+(j-1)*(2*nt-j)/2) = sum*fact
		enddo
		enddo
	else
		do j=1, nx
		do i=j, nx
			sum  = 0.0d0
			sum2 = 0.0d0
			sum3 = 0.0d0
                        npts = 0
			do k=1, nt
                           if (data(i,k).ne.dmsg .and.
     &                         data(j,k).ne.dmsg) then
				sum  = sum  + data(j,k)*data(i,k)
				sum2 = sum2 + data(i,k)*data(i,k)
				sum3 = sum3 + data(j,k)*data(j,k)
                                npts = npts + 1
                           end if
			enddo
			if( icovcor .eq. covariance ) then
c c c                       fact = 1.0d0/float(nt-1)
                            fact = 1.0d0/float(npts-1)
			else
                            fact = 1.0d0/(sqrt(sum2)*sqrt(sum3))
			endif
			spacked(i+(j-1)*(2*nx-j)/2) = sum*fact
		enddo
		enddo
	endif
			
	return
	end

c --------------------------------------------------------

	subroutine deofpcs( data, nx, nt, nmodes, iminnxnt,
     &		imaxnxnt, evecs, doswitched, tentpcs, dmsg )

c       ------------------------------------------------
c       Compute the tentative principal components; they
c       are 'tentative' because they might be the PCs
c       of the switched data.  Put them into 'tentpcs',
c       which is of size (nt,nmodes) if we are doing
c       regular EOFs and (nx,nmodes) if we are doing
c       switched EOFs.  These PCs come out in order
c       corresponding to the order of 'evecs', which is
c       in ASCENDING order.
c
c	Inputs:
c
c		data(nx,nt): The input data.  THESE MUST
c			BE ANOMALIES.
c
c		nmodes: # of modes to calculate.
c
c		iminnxnt: min(nx,nt)
c
c		evecs(iminnxnt,nmodes): the eigenvectors 
c			(which might be switched).
c
c		doswitched: if .TRUE., then we are doing
c			switched (space,time) calculation;
c			otherwise, regular (time,space)
c			calculation.
c
c	Outputs:
c
c		tentpcs(imaxnxnt,nmodes): the tentative
c			(possibly switched) principal
c			components.
c
c	David W. Pierce
c	Scripps Institution of Oceanography
c	Climate Research Division
c	dpierce@ucsd.edu
c	Jan 29, 1996
c       ------------------------------------------------

	implicit none

c	-----------------
c	Passed parameters
c	-----------------
	integer	nx, nt, nmodes, iminnxnt, imaxnxnt
	double precision data(nx,nt), evecs(iminnxnt,nmodes),
     &		tentpcs(imaxnxnt,nmodes), dmsg
	logical	doswitched

c	---------------
c	Local variables
c	---------------
	integer	i, j, k
	double precision	sum

	if( doswitched ) then
		do j=1, nmodes
		do i=1, nx
			sum = 0.0d0
			do k=1, nt
                           if (data(i,k).ne.dmsg) then
				sum = sum + data(i,k)*evecs(k,j)
                           end if
			enddo
			tentpcs(i,j) = sum
		enddo
		enddo
	else
		do j=1, nt
		do i=1, nmodes
			sum = 0.0d0
			do k=1, nx
                           if (data(k,j).ne.dmsg) then
				sum = sum + data(k,j)*evecs(k,i)
                           end if
			enddo
			tentpcs(j,i) = sum
		enddo
		enddo
	endif

	return
	end

c ------------------------------------------

	subroutine deoftotvar( data, nx, nt, totvar, doswitched,
     &		icovcor, covariance, correlation, dmsg )

c	-------------------------------------------------
c	Returns the total variance in the field so we can
c	normalize by it.
c
c	Inputs:
c
c		data(nx,nt): data to calculate upon
c
c		nx, nt:	size of 'data'.
c
c		doswitched: if .TRUE., then we are working
c			in switched (nx,nt) space, false 
c			otherwise.
c
c		icovcor: if (icovcor .eq. covariance), then
c			we are using the covariance array;
c			if( icovcor .eq. correlation) then
c			we are using the correlation array.
c
c	Outputs:
c
c		totvar: estimate of total variance
c
c	-------------------------------------------------

	implicit none

	integer	nx, nt, icovcor, covariance, correlation
	double precision data(nx,nt), totvar, sum, fact, dmsg
	logical	doswitched
	integer	i, j, npts

	if( icovcor .eq. correlation ) then
		if( doswitched ) then
			totvar = nt 
		else
			totvar = nx
		endif
		return
	endif

	totvar = 0.0d0
        if( doswitched ) then
            fact = 1.0d0/(nx-1.0d0)
        else
            fact = 1.0d0/(nt-1.0d0)
        endif
	do j=1, nt
		sum  = 0.0d0
		npts = 0
		do i=1, nx
                   if (data(i,j).ne.dmsg) then
			sum  = sum  + data(i,j)*data(i,j)
                        npts = npts + 1
                   end if
		enddo
                fact = 1.0d0/(npts-1.0d0)
		totvar = totvar + sum*fact
	enddo

	return
	end

