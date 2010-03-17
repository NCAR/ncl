;******************************************************************************
; kf_filter.ncl
; Carl Schreck (carl@atmos.albany.edu)
; January 2009
;******************************************************************************

undef("kf_filter")

function kf_filter( inData[*][*]:float, obsPerDay:numeric, \\
                    tMin:numeric, tMax:numeric, kMin:numeric, kMax:numeric, \\
                    hMin:numeric, hMax:numeric, waveName:string )
;******************************************************************************
; Apply zonal and temporal filtering for the WK99 Kelvin filter.  Set one or 
; both of hMin and hMax to missing to ignore the dispersion curve in that
; direction.
;
; Input Variables:
;   inData: the data to be filtered.  time should be 1st coordinate
;           lon should be 2nd coordinate.
;   obsPerDay: number of time steps per 24 hour period
;   tMin, tMax: range of cut-off periods (must be positive)
;   kMin, kMax: range of wavenumbers, can be pos (eastward) or neg (westward)
;   hMin, hMax: range of equivalent depths to use.
;   waveName: name of dispersion curve to use: 
;             "ER", "Kelvin", "MRG", "IG0", "IG1", IG2" (IG0 and MRG are same)
; Return Value:
;   retVal[*][*]: the filtered data
;******************************************************************************

local lonDim, timeDim, wrapFlag, tempData, fftData, kDim, freqDim, \\
      jMin, jMax, iMin,iMax, PI, beta, c, spc, i, k, freq, \\
      jMinWave, jMaxWave, retVal

begin
; Find out if we need to worry about wrapping
  lonDim   = dimsizes(inData&lon)
  timeDim  = dimsizes(inData&time)
  wrapFlag = ( ( inData&lon(0) + 360 ).eq.inData&lon(lonDim-1) )
  if( wrapFlag ) then
    tempData = inData(lon|1:lonDim-1,time|:)
  else
    tempData = inData(lon|:,time|:)
  end if

; detrend and taper the data
; tempData = dtrend_msg( tempData&time,tempData, False, False )
  tempData = dtrend( tempData, False )

  tempData = taper( tempData, 0.05, 0 )

; perform the 2-d fourier transform
  fftData  = fft2df( tempData )

; Find the indeces for the period cut-offs
  kDim = dimsizes( fftData(0,:,0) )
  freqDim = dimsizes( fftData(0,0,:) )
  jMin = round( ( timeDim * 1. / ( tMax * obsPerDay ) ), 3 )
  jMax = round( ( timeDim * 1. / ( tMin * obsPerDay ) ), 3 )
  jMax = min( (/ jMax, freqDim /) )

; Find the indeces for the wavenumber cut-offs
; This is more complicated because east and west are separate
  if( kMin.lt.0 ) then
    iMin = round( ( kDim + kMin ), 3 )
    iMin = max( (/ iMin, ( kDim / 2 ) /) )
  else
    iMin = round( kMin, 3 )
    iMin = min( (/ iMin, ( kDim / 2 ) /) )
  end if
  if( kMax.lt.0 ) then
    iMax = round( ( kDim + kMax ), 3 )
    iMax = max( (/ iMax, ( kDim / 2 ) /) )
  else
    iMax = round( kMax, 3 )
    iMax = min( (/ iMax, ( kDim / 2 ) /) )
  end if

; set the appropriate coefficients to zero
  if( jMin.gt.0 ) then
    fftData( :, :, :jMin-1 ) = 0
  end if
  if( jMax.lt.( freqDim - 1 ) ) then
    fftData( :, :, jMax+1: ) = 0
  end if

  if( iMin.lt.iMax ) then
    ; Set things outside the range to zero, this is more normal
    if( iMin.gt.0 ) then
      fftData( :, :iMin-1, : ) = 0
    end if
    if( iMax.lt.( kDim - 1 ) ) then
      fftData( :, iMax+1:, : ) = 0
    end if
  else
; Set things inside the range to zero, this should be somewhat unusual
    fftData( :, iMax+1:iMin-1, : ) = 0
  end if

; Find constants
  PI = acos( -1 )
  beta = 2.28e-11
;  cMin = ( 9.8 * hMin )^0.5
;  cMax = ( 9.8 * hMax )^0.5
  c = ( 9.8 * (/ hMin, hMax /) )^0.5
  spc = 24 * 3600. / ( 2 * PI * obsPerDay ) ; seconds per cycle

; Now set things to zero that are outside the Kelvin dispersion
  do i = 0, kDim-1
      ; find nondimensional wavenumber
    if( i.gt.( kDim / 2 ) ) then
      ; k is negative
      k = ( i - kDim  ) * 1. / (6.37e6); adjusting for circumfrence of earth
    else
      ; k is positive
      k = i * 1. / (6.37e6) ; adjusting for circumfrence of earth
    end if

    freq = (/ 0, freqDim /) * 1. / spc
    jMinWave = 0
    jMaxWave = freqDim

    if( ( waveName.eq."Kelvin" ).or.( waveName.eq."kelvin" ) \\
         .or.( waveName.eq."KELVIN" ) ) then
      freq = k * c
    end if
 
    if( ( waveName.eq."ER" ).or.( waveName.eq."er" ) ) then
      freq = -beta * k / ( k^2 + 3. * beta / c )
    end if

    if( ( waveName.eq."MRG" ).or.( waveName.eq."IG0" ) \\
         .or. ( waveName.eq."mrg" ).or.( waveName.eq."ig0" ) ) then
      if( k.eq.0 ) then
        freq = ( beta * c )^0.5
      else
        if( k.gt.0) then
          freq = k * c \\
               * ( 0.5 + 0.5 * ( 1 + 4 * beta / ( k^2 * c ) )^0.5 )
        else
          freq = k * c \\
               * ( 0.5 - 0.5 * ( 1 + 4 * beta / ( k^2 * c ) )^0.5 )
        end if
      end if
    end if

    if( ( waveName.eq."IG1" ).or.( waveName.eq."ig1" ) ) then
      freq = ( 3 * beta * c + k^2 * c^2 )^0.5
    end if

    if( ( waveName.eq."IG2" ).or.( waveName.eq."ig2" ) ) then
      freq = ( 5 * beta * c + k^2 * c^2 )^0.5
    end if

    if( ismissing( hMin ) ) then
      jMinWave = 0
    else
      jMinWave = floattointeger( floor( freq(0) * spc * timeDim ) )
    end if

    if( ismissing( hMax ) ) then
      jMaxWave = freqDim
    else
      jMaxWave = floattointeger( ceil( freq(1) * spc * timeDim ) )
    end if

    jMaxWave = max( (/ jMaxWave, 0 /) )
    jMinWave = min( (/ jMinWave, freqDim /) )

; set the appropriate coefficients to zero
    if( jMinWave.gt.0 ) then
      fftData( :, i, :jMinWave-1 ) = 0
    end if
    if( jMaxWave.lt.( freqDim - 1 ) ) then
      fftData( :, i, jMaxWave+1: ) = 0
    end if
  end do

; perform the inverse transform to reconstruct the data
  retVal = inData
  tempData = fft2db( fftData )
  if( wrapFlag ) then
    retVal(time|:,lon|1:lonDim-1) = tempData(time|:,lon|:)
    retVal(time|:,lon|0) = (/ retVal(time|:,lon|lonDim-1) /)
  else
    retVal(time|:,lon|:) = tempData(time|:,lon|:)
  end if

; Attach some metadata about the filtering
  retVal@wavenumber = (/ kMin, kMax /)
  retVal@period     = (/ tMin, tMax /)
  retVal@depth      = (/ hMin, hMax /)
  retVal@waveName   = waveName
; return the filtered data
  return( retVal )
end
