;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn04n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Converted to NCL by Ed Stautler
;
;   Date:       Wed Oct 12 13:18:13 MDT 1994
;
;   Description:    Emulates the output of the Conpack example 'cpex02.f'
;           using the HLU library.
;


;
; This program emulates the output of cpex02 with a few differences:
; 1. Because the information label is implemented as an HLU Annotation
;    object, Conpack is unaware of its existence, much less its location.
;    Therefore it is not possible to have Conpack remove the high/low
;    labels that occupy the same space as the info label.
; 2. Line labels do not appear in the same positions.
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

M = 33
N = 33

z = new((/N,M/),float)
mlow = 20
mhigh = 20
dlow = .000025
dhigh = .000075
len_dims = new((/2/),integer)

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; The resource file sets most of the Contour resources that remain fixed
; throughout the life of the Contour object.
;
appid = create "cn04" appLayerClass noparent
    "appUsrDir" : "./"
    "appDefaultParent" : "True"
end create

;
; Default is to display output to an X workstation
;
NCGM=0

if (NCGM.eq.1) then
;
; Create an NCGM workstation.
;
  wid = create "cn04Work" ncgmWorkstationLayerClass noparent
    "wkMetaName" : "./cn04n.ncgm"
  end create
else
;
; Create an X workstation.
;
  wid = create "cn04Work" xWorkstationLayerClass noparent
    "wkPause" : "True"
  end create
end if

;
; Call the Fortran routine 'GENDAT' to create the first array of contour
; data. Create a ScalarField data object and hand it the data created by
; 'GENDAT'.
; 
len_dims(0) = N
len_dims(1) = M
gendat(z,M,M,N,mlow,mhigh,dlow,dhigh)

dataid = create "Gendat" scalarFieldLayerClass appid
        "sfDataArray" : z
end create

;
; Create a Contour object, supplying the ScalarField object as data,
; and setting the size of the viewport.
;
cnid = create "Contour1" contourPlotLayerClass wid
    "cnScalarFieldData" : dataid
    "tiMainString" : "EXAMPLE 2-1"
    "vpWidthF" : 0.4625
    "vpHeightF" : 0.4625
end create

;
; In order to set the contour array resources of interest, you must 
; allocate memory for the arrays and fill in the correct value for each
; element. But by calling GetValues for the arrays the Contour object 
; allocates the space and fills in the current values for you. Then all
; that is necessary is to modify the values that you want to change. 
; Remember, however, that you are responsible for freeing the memory
; after you are done with it. Note also that a GetValues resource list is
; different that a SetValues list.
;
getvalues cnid
    "cnLevelFlags" : lvlflags
    "cnFillPatterns" : pats
    "cnLevels" : levels
    "cnLineThicknesses" : thicknesses
end getvalues

; 
; Depending on the level flag for each contour line, widen the line if
; there is a label on the line. Also set the fill style to pattern #6
; if the level is between certain values. Note that there is always one
; more element in the fill resource arrays than there are Contour line 
; levels: the first element of these arrays specifies the attributes
; of areas less than the minimum contour level and the last element 
; specifies attributes of areas greater than the maximum contour level. 
;
do i=0,dimsizes(levels)-1 
    if (lvlflags(i) .eq. 3)
                thicknesses(i) = 2.0
    end if
        if (levels(i) .ge. .000045 .and. levels(i) .lt. .000055)
                pats(i) = 6
        else 
            pats(i) = -1
    end if
end do
pats(dimsizes(pats)-1) = -1

;
; Now that the arrays are correctly filled in set the arrays that have
; been modified. Also set the position of the first Contour plot and
; the label scaling mode.
;
setvalues cnid
    "cnFillPatterns" : pats
    "cnLineThicknesses" : thicknesses
    "vpXF" : 0.0250
    "vpYF" : 0.9750
        "cnLabelScalingMode" : "ConfineToRange"
        "cnLabelScaleValueF" : 10.0
end setvalues

draw(cnid)

;
; Plot 2 - Set the Scalar Field object with a newly generated data set;
; Set the Contour object with a new title, position, and a new label
; scaling mode.
;
gendat(z,M,M,N,mlow,mhigh,dlow,dhigh)
setvalues dataid
        "sfDataArray" : z
end setvalues

setvalues cnid
    "tiMainString" : "EXAMPLE 2-2"
    "vpXF" : 0.5125
    "cnScalarFieldData" : dataid
        "cnLabelScalingMode" : "MaxSigDigitsLeft"
end setvalues

draw(cnid)

;
; Plot 3 - Set the Scalar Field object with a newly generated data set;
; Set the Contour object with a new title, position, and a new label
; scaling mode.
;
gendat(z,M,M,N,mlow,mhigh,dlow,dhigh)
setvalues dataid
        "sfDataArray" : z
end setvalues

setvalues cnid
    "tiMainString" : "EXAMPLE 2-3"
    "vpXF" : 0.0250
    "vpYF" : 0.4875
    "cnScalarFieldData" : dataid
        "cnLabelScalingMode" : "TrimZeros"
end setvalues

draw(cnid)

;
; Plot 4 - Set the Scalar Field object with a newly generated data set;
; Set the Contour object with a new title, position, and a new label
; scaling mode.
;
gendat(z,M,M,N,mlow,mhigh,dlow,dhigh)
setvalues dataid
        "sfDataArray" : z
end setvalues

setvalues cnid
    "tiMainString" : "EXAMPLE 2-4"
    "vpXF" : 0.5125
    "cnScalarFieldData" : dataid
        "cnLabelScalingMode" : "IntegerLineLabels"
end setvalues

draw(cnid)

;
; Label the plot as an emulation
;
txid = create "TextItem1" textItemLayerClass wid
       end create

draw(txid)
frame(wid)

;
; Free dynamic arrays, destroy the objects created, 
; close the HLU library and exit.
;
delete(dataid)
delete(cnid)
delete(wid)
delete(appid)

end
