;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1993                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       mp03n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Translated to NCL by Ed Stautler
;
;   Date:       Fri Oct 14 11:42:41 MDT 1994
;
;   Description:    Demonstrates MapPlot masking; loosely emulates the
;           LLU example 'colcon'
;

rseq =(/ .749, .973, .666, .804, .081, .483, .919, .903, .951, .960,       \
   .039, .269, .270, .756, .222, .478, .621, .063, .550, .798, .027, .569, \
   .149, .697, .451, .738, .508, .041, .266, .249, .019, .191, .266, .625, \
   .492, .940, .508, .406, .972, .311, .757, .378, .299, .536, .619, .844, \
   .342, .295, .447, .499, .688, .193, .225, .520, .954, .749, .997, .693, \
   .217, .273, .961, .948, .902, .104, .495, .257, .524, .100, .492, .347, \
   .981, .019, .225, .806, .678, .710, .235, .600, .994, .758, .682, .373, \
   .009, .469, .203, .730, .588, .603, .213, .495, .884, .032, .185, .127, \
   .010, .180, .689, .354, .372, .429 /)

iseq = 0

;
; Random number generating function
;
function fran()
begin
        iseq = (iseq % 100) + 1
        return(rseq(iseq-1))
end


;
; Maximum function
;
function max(
        x: float,
        y: float
)
begin
        if (x .gt. y) then
                return(x)
        else
                return(y)
        end if
end


;
; Minimum function
;
function min(
        x: float,
        y: float
)
begin
        if (x .lt. y) then
                return(x)
        else
                return(y)
        end if
end


;
; Power of 2
;
function pow2(
        x : float
)
begin
        return (x*x)
end


;
; Exponentiation
;
function exp(
        x : float
)
begin
        return (2.7182818^x)
end


procedure gendat (
        data: float,
        idim: integer,
        m : integer,
        n : integer,
        mlow: integer,
        mhgh: integer,
        dlow: float,
        dhgh: float
)
begin
;
; This is a routine to generate test data for two-dimensional graphics
; routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
; the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
; of data having approximately "MLOW" lows and "MHGH" highs, a minimum
; value of exactly "DLOW" and a maximum value of exactly "DHGH".
;
; "MLOW" and "MHGH" are each forced to be greater than or equal to 1
; and less than or equal to 25.
;
; The function used is a sum of exponentials.
;
        ccnt = new((/3,50/),float)

    fovm=9./m
        fovn=9./n

        nlow=min(25.0,mlow*1.0)
        nlow=max(1.0,nlow)
        nhgh=min(25.0,mhgh*1.0)
        nhgh=max(1.0,nhgh)
        ncnt=nlow+nhgh

        do k=1,ncnt
            ccnt(0,k-1)=1.+(m-1.)*fran()
            ccnt(1,k-1)=1.+(n-1.)*fran()
            if (k .le. nlow)
                    ccnt(2,k-1)= -1.
            else
                    ccnt(2,k-1) = 1.
            end if
        end do

        dmin =  1.e36
        dmax = -1.e36
        do j=1,n
            do i=1,m
                    data(j-1,i-1)=.5*(dlow+dhgh)
                    do k=1,ncnt
                        temp = -(pow2((fovm*((i)-ccnt(0,k-1))))+ \
                                pow2(fovn*((j)-ccnt(1,k-1))))
                        if (temp .ge. -20.)
                                data(j-1,i-1)=data(j-1,i-1)+     \
                                .5*(dhgh-dlow)*ccnt(2,k-1)*exp(temp)
                        end if
                    end do
                    dmin=min(dmin,data(j-1,i-1))
                    dmax=max(dmax,data(j-1,i-1))
            end do
        end do

        do j=0,n-1
            do i=0,m-1
                data(j,i)=(data(j,i)-dmin)/(dmax-dmin)*(dhgh-dlow)+dlow;
            end do
        end do
end


begin

    Z = new((/50,50/),float)
    M = 50
        N = 50
        mlow = 13
        mhigh = 18
        dlow = 13.0
        dhigh = 18.0
    len_dims = new((/2/),integer)

    mask_specs = (/ "oceans" /)
;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; The resource file sets most of the Contour resources that remain fixed
; throughout the life of the Contour object.
;
    appid = create "mp03" appLayerClass noparent
        "appUsrDir" : "./"
        "appDefaultParent" : "True"
    end create
;
; Default is to display output to an X workstation
;
    NCGM=0

    if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
        wid = create "mp03Work" ncgmWorkstationLayerClass noparent
           "wkMetaName" : "./mp03n.ncgm"
        end create
    else
;
; Create an XWorkstation object.
;
        wid = create "mp03Work" xWorkstationLayerClass defaultapp
            "wkPause" : "True"
        end create
    end if

;
; Call the routine 'gendat' to create the first array of contour
; data. Create a ScalarField data object and hand it the data created by
; 'gendat'. Define the extent of the data coordinates as the whole globe 
; 
    len_dims(0) = N
    len_dims(1) = M
        gendat(Z,M,M,N,mlow,mhigh,dlow,dhigh)
        dataid = create "Gendat" scalarFieldLayerClass appid
            "sfDataArray" : Z
        "sfXCStartV" : -180
            "sfXCEndV" : 180
        "sfYCStartV" : -90
            "sfYCEndV" : 90
    end create
;
; Create a Contour object, supplying the ScalarField object as data,
; and setting the size of the viewport.
;
    cnid = create "Contour1" contourPlotLayerClass wid
        "cnScalarFieldData" : dataid
        "cnLabelDrawOrder" : "postdraw"
    end create
;
; Create a MapPlot object, setting the fill to draw over the main draw,
; and masking out the oceans.
;
    mapid = create "Map1" mapPlotLayerClass wid
        "mpFillOn" : "true"
        "ovTitleDisplayMode" : "always"
        "tiMainString" : "mp03n"
        "mpFillDrawOrder" : "postdraw"
        "mpAreaMaskingOn" : "true"
        "mpMaskAreaSpecifiers" : mask_specs
    end create
;
; Overlay the Contour object on the MapPlot object
;
    overlay(mapid, cnid)
    
    draw(mapid)
    frame(wid)
;
; Destroy the objects created, close the HLU library and exit.
;
    delete(mapid)
    delete(wid)
end
