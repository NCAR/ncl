;----------------------------------------------------------------------
; Many of the following functions and procedures were written by:
;        Mohammad Abouali, maboualiedu@gmail.com
;              May 22nd, July 30th, 2011
;
;        http://sites.google.com/site/maboualihome/
;
; Many functions were updated, fixed, or added by Mary Haley.
; See notes below.
;
; A debt of gratitude is owed to the ESMF development team, and 
; especially Robert Oehmke, for help in using the ESMF software.
;
; This script was added to the NCL trunk on 13 Oct 2011.
;
;    Important modifications:
;       22 Dec 2016:
;           Added check to ensure ESMF_RegridWeightGen can be executed.
;           Added version detection for ESMF_RegridWeightGen.
;           Added --ignore_degenerate flag to ESMF_RegridWeightGen for ESMF
;           >=7.0.0 to match functionality with older versions.
;           Added --src_loc and --dst_loc flags to ESMF_RegridWeightGen to
;           further ensure compatibility with old and new versions of ESMF.
;       27 Apr 2015:
;           Added unadvertised ElementVertices, so that user can input own 
;           element vertices mesh when regridding unstructured data.
;           Currently, the code wants a triangular mesh or a quadrilateral
;           mesh, and "csstri" will be used if neither is provided. "csstri"
;           is notoriously slow.
;       14 March 2015:
;           Decided to add a SrcSCRIP/DstSCRIP which a user can set to
;           True to force an unstructured description file to be created
;           as a SCRIP file instead of an ESMF file  This is currently
;           unadvertised, and only affects unstructrued files.  See
;           "unstructured_to_SCRIP" below.
;
;       20 Feb 2015:
;         Created new "Src/DstGridArea" attribute to
;          allow user to enter the "grid_area" quantity 
;          required for SCRIP and "conserve" interpolation.
;          I haven't tested this yet!
;
;       14 Feb 2015:
;           Various cleanup for ESMF regridding:
;              Created new "Src/DstGridMask" attribute to
;              replace the misleading "Src/DstMask2D".
;              The old one will still work.
;
;       10 Feb 2015:
;           Added an (as yet unadvertised) "unstructured_to_SCRIP" function
;           that writes the grid description file to a SCRIP file instead
;           of an ESMF file, if SrcESMF or DstESMF is set to False. This
;           is currently being tested.  (March 2015 followup: I decided
;           to call this Src/DstSCRIP instead).
;
;       04 Feb 2015:                                                                                   
;           Fixed the "remap" attribute that gets attached to regridded variable                       
;           to be the ESMF_regrid_method attribute that's on the weights file,                         
;           and not the "map_method" attribute, which doesn't always have the                          
;           right interpolation method.                                                                
;
;       01 Sep 2014:
;           Renamed print_elapsed_time to print_elapsed_esmf_time
;
;       17 Aug 2014:
;         Added --norm_type fracarea for the "conserve" method.
;           This replaces the old "ApplyFracB" code in 
;           ESMF_regrid_with_weights.  You can turn this off
;           by setting NormalizeWeights = False. The old
;           code can still be invoked by setting WgtApplyFracB
;           when calling ESMF_regrid_with_weights.
;
;         Added a "Check" option to tell the regridder to
;         include "--check" on the regridder line. This
;         will add some extra error reporting on the output
;         results.
;
;       24 Jul 2014:
;         Added a "NoPETLog" option to tell the regridder not
;         to create the PET log file.  This was added as the
;         "--no_log" option added in 6.3.0rp1.
;         We'll keep the RemovePETLog option for people who 
;         want to remove an existing PET file.
;
;       8 Jul 2014:
;           Added a WgtNetCDFType option for source/destination files
;           and weight files. ESMF_RegridWeightGen uses the F90
;           interface to read the source/destination files, which
;           is a problem because this interface doesn't support
;           the NetCDF-4 "string" type.  We have to write file
;           string attributes (like "units") as character arrays.
;
;       18 Dec 2013:
;          Added some code to get ready for automatically setting
;          SrcMask2D.
;
;       23 Oct 2013:
;         Added RemovePETLog option to ESMF_regrid so user can
;         ask to have the PET0.RegridWeightGen.Log file removed.
;
;       14 Aug 2013:
;           Added TriangularMesh, so that user can input own 
;           triangular mesh to unstruct_to_ESMF code. This
;           is to get around the issue of csstri being slow,
;           and potentially chewing up memory (need to verify the
;           memory hog claim).
;
;       25 Oct 2012:
;           The LargeFile option is now True by default.
;
;       17 Oct 2012:
;           Attributes attached to regridded variable were
;           cleaned up. A new "remap" attribute will be attached
;           to the regridded variable.
;
;       16 Oct 2012:
;           ESMF_regrid and ESMF_regrid_with_weights will now
;              return float if a float is input. Double is 
;              returned otherwise. If you still want a double 
;              returned, set ReturnDouble to True.
;              This was changed AFTER 6.1.0-beta.
;
;       31 Jul 2012:
;           Added a check_grid_type function to check for valid
;              grid types ("rectilinear","5deg","G64", etc) and
;              return necessary information.
;       29 Jul 2012:
;            Fixed the code for copying coordinates to use the
;            values on the weights file instead of the description
;            file. ESMF_copy_varcoords is the new procedure.
;            This change pretty much renders all of the 
;            retrieve_SCRIP_xxx, retrieve_ESMF_xxx, 
;            retrieve_dstGrid_xxx, and retrieve_srcGrid_xxx
;            obsolete.
;
;            ESMF_regrid_with_weights now copies attributes and
;            coordinates by default.
;            (CopyVarCoords and CopyVarAtts now default True.)
;
;            Added new CopyVarMeta attribute, which is True by default.
;            Setting this to False overrides CopyVarCoords and CopyVarAtts. 
;
;       20 Jun 2012:
;            Fix the section that calculates the corner grid cells
;            so that the appropriate algorithm is used depending
;            on whether the lat values go to -90/90.
;            
;       6 May 2012:
;            Clean up error statements.
;            Renamed RemoveDstGridFile, RemoveSrcGridFile, and
;              RemoveGridFiles to RemoveDstFile, RemoveSrcFile,
;              and RemoveFiles.
;            Renamed ForceOverWrite and OverWrite to 
;              ForceOverwrite and Overwrite (lowercase 'w').
;            Added check for duplicate options being set 
;                 (like SrcOverwrite and Overwrite).
;            Renamed DstGridFileName / SrcGridFileName to
;                just DstFileName / SrcFileName.
;
;       2 May 2012:
;            Changed default names of description files to
;               source_grid_file.nc and 
;               destination_grid_file.nc 
;
;       27 April 2012:
;            Renamed CornerLat and CornerLon to GridCornerLat and
;            GridCornerLon to make it more clear what these are.
;
;       31 Jul 2012:
;           Added a check_grid_type function to check for valid
;              grid types ("rectilinear","5deg","G64", etc) and
;              return necessary information.
;       29 Jul 2012:
;            Fixed the code for copying coordinates to use the
;            values on the weights file instead of the description
;            file. ESMF_copy_varcoords is the new procedure.
;            This change pretty much renders all of the 
;            retrieve_SCRIP_xxx, retrieve_ESMF_xxx, 
;            retrieve_dstGrid_xxx, and retrieve_srcGrid_xxx
;            obsolete.
;
;            ESMF_regrid_with_weights now copies attributes and
;            coordinates by default.
;            (CopyVarCoords and CopyVarAtts now default True.)
;
;            Added new CopyVarMeta attribute, which is True by default.
;            Setting this to False overrides CopyVarCoords and CopyVarAtts. 
;
;       20 Jun 2012:
;            Fix the section that calculates the corner grid cells
;            so that the appropriate algorithm is used depending
;            on whether the lat values go to -90/90.
;            
;       6 May 2012:
;            Clean up error statements.
;            Renamed RemoveDstGridFile, RemoveSrcGridFile, and
;              RemoveGridFiles to RemoveDstFile, RemoveSrcFile,
;              and RemoveFiles.
;            Renamed ForceOverWrite and OverWrite to 
;              ForceOverwrite and Overwrite (lowercase 'w').
;            Added check for duplicate options being set 
;                 (like SrcOverwrite and Overwrite).
;            Renamed DstGridFileName / SrcGridFileName to
;                just DstFileName / SrcFileName.
;
;       2 May 2012:
;            Changed default names of description files to
;               source_grid_file.nc and 
;               destination_grid_file.nc 
;
;       27 April 2012:
;            Renamed CornerLat and CornerLon to GridCornerLat and
;            GridCornerLon to make it more clear what these are.
;
;       26 April 2012:
;            For "conserve" method, be sure to divide results by
;            "frac_b". This also meant I had to modify "reshape"
;            so I could use it to reshape *and* conform the frac_b 
;            array to the destination grid size.
;
;       22 April 2012:
;            Renamed "method" and "pole" attributes to "InterpMethod"
;            and "Pole" for consistency with other attributes. "method"
;            and "pole" will still work.
;            Added RemoveGridFiles, RemoveSrcGridFile, RemoveDstGridFile
;            to ESMF_regrid_gen_weights.
;
;       13 April 2012:
;            Removed ForcedCorners attribute. The user
;            can just set CornerLat and CornerLon.
;
;       08 March 2012:
;            Renamed this script to ESMF_regridding.ncl
;
;       06 March 2012:
;            Renamed ESMF_gen_weights to ESMF_regrid_gen_weights
;            Renamed ESMF_regrid      to ESMF_regrid_with_weights
;            Renamed ESMF_all         to ESMF_regrid
;
;       08 Feb 2012:
;            Added retrieve_ESMF_lat and retrieve_ESMF_lon.
;            Added ESMF_all
;            Added write_grid_description_file.
;            Removed isbothvaratt_logical_true and made isatt_logical_true
;               behave like it.
;            Added isatt_logical_false.
;            Set IgnoreMappedPoints to True by default.
;            Set pole="none" if InterpMethod="conserve" (and pole not 
;              explicitly set by user).
;
;       23 Jan 2012:
;            Removed BothRegional and BothESMF, since they might
;            confuse users. Use Src/DstRegional and Src/DstESMF.
;
;       13 Jan 2012:
;            Renamed this script to ESMFRegriddingTools.ncl
;
;       12 Jan 2012:
;            Moved copy_VarAtts_Except to contributed.ncl and 
;            renamed to copy_VarAtts_except.
;
;       10 Jan 2012:
;            Overhauled latlon_to_SCRIP to accept a grid type
;            as input, like "1x1", "0.25", "G64", etc. 
;            Overhauled rectilinear_to_SCRIP, curvilinear_to_SCRIP
;            latlon_to_SCRIP, and unstructured_to_ESMF to turn some
;            of the input arguments (like a title) into "opt"
;            attributes. Also removed "mask2d" as an input argument,
;            and made it an "opt" attribute.
;
;       09 Jan 2012:
;            Renamed cartesian_to_SCRIP to latlon_to_SCRIP
;               Renamed some of the attributes in this function.
;            Added PrintTimings to the xxxx_to_SCRIP, xxxx_to_ESMF,
;            and ESMF_gen_weights functions.
;
;       01 Jan 2012:
;            Changed behavior of ForceOverWrite. No longer
;            requires OverWrite to be set. Removed
;            ForceUnstructured since it's not really needed.
;            Added get_start_time and print_elapsed_time.
;
;       22 Dec 2011:
;            Renamed some more functions: changed "esmf" to "ESMF"
;            and "scrip" to "SCRIP".
;            Changed esmf_remap_weights to ESMF_gen_weights.
;            Changed esmf_remap to ESMF_regrid
;            Converted ESMF_gen_weights to a procedure.
;            Other changes to clean up error statements, remove
;               extraneous print statements, add more comments.
;
;       20 Dec 2011:
;            Combined GenBox_with_LLURCorner and GenBox_with_Center_Dim
;            into one script called "cartesian_to_SCRIP".
;
;       18 Dec 2011: 
;            Cleaned up Auto2SCRIP:
;               - changed to procedure
;               - renamed to auto_to_SCRIP
;               - changed values for "coordType"
;               - cleaned up "if-else-endif" statements
;
;       18 Dec 2011: 
;            Cleaned up this script to:
;               - rename some functions and/or change to procedures
;               - use better functions where appropriate
;               - align code to make it easier to read
;               - add routine names to error prints
;
;       15 Dec 2011: 
;            Added the "-i" flag to the ESMF_RegridWeightGen
;            command to properly handle regional grids like WRF. 
;            The WRF regridding example wasn't working otherwise.
;
;       09 Nov 2011: 
;            Decided to completely remove "remove outlier" code.
;            Cleaned up the sparse_matrix_mult code to correctly
;            handle the case of the source grid being 1D.
;
;       06 Nov 2011: 
;            Made code faster by adding option
;            ("RemoveOutliers", False by default) to not
;            remove outliers. It turns out this code was
;            added to fix bug in original SMM code in
;            handling missing values. This code should no
;            longer be needed.
;
;            Added PrintTimings to time the sparse matrix multiply
;            and the removing outliers code.
;
;       02 Nov 2011: 
;            Replaced the SMM call with the new sparse_matrix_mult
;            function.
;
;       01 Nov 2011: 
;            Created a built-in version of "reshape" which doesn't
;            copy any metadata. Removed the "reshape" that was in
;            this file.
;
;       31 Oct 2011: 
;            Replaced "return(tointeger(1))" with just "return(1)"
;            Removed SMM and squeeze, since they don't seemed to be
;            used by anybody.
;
;       14 Oct 2011:
;            Removed references to $ESMFBINDIR. This should be on 
;            user's search path. Also, executable had wrong name,
;            changed to "ESMF_RegridWeightGen".

;======================================================================
; This function returns the start time, either in CPU or wall
; clock seconds, depending on your version of NCL. Use this function
; with print_elapsed_esmf_time, below.
;======================================================================
undef("get_start_time")
function get_start_time()
local s
begin
  s = str_split(get_ncl_version(),".")        
  if(s(0).eq."5".or.(s(0).eq."6".and.s(1).eq."0")) then
    return(systemfunc("date"))
  else
    return(get_cpu_time())
  end if
end

;======================================================================
; This procedure prints the elapsed time, either in CPU or wall
; clock seconds, depending on your version of NCL. Use this procedure
; with get_start_time, above.
;======================================================================
undef("print_elapsed_esmf_time")
procedure print_elapsed_esmf_time(stime,title)
local s 
begin
  s = str_split(get_ncl_version(),".")        
  if(s(0).eq."5".or.(s(0).eq."6".and.s(1).eq."0")) then
    wallClockElapseTime(stime, title, 0)
  else
    diff_time = get_cpu_time() - stime
    print("=====> CPU Elapsed Time: " + title + ": " + diff_time + " seconds <=====")
  end if
end

;======================================================================
; This function receives a set of variable names and checks 
; if their units attribute contains north or east
; If it contains:
;           north, it returns "latitude"
;           east, it returns "longitude"
; otherwise it returns "unknown"
;======================================================================
undef("isLatorLon")
function isLatorLon(fid,VarNames[*]:string)
local NumNames, OutPut, i
begin
    NumNames = dimsizes(VarNames)
    OutPut   = new(NumNames,"string","No_FillValue")
    OutPut   = "unknown"
    do i=0,NumNames-1
        if (isfilevar(fid,VarNames(i)).and. \
            isfilevaratt(fid,VarNames(i),"units")) then
            if( isStrSubset(str_lower(fid->$VarNames(i)$@units),"north") ) then
                OutPut(i)="latitude"
            else if ( isStrSubset(str_lower(fid->$VarNames(i)$@units),"east") ) then
                OutPut(i)="longitude"
            end if
            end if
        end if
    end do
    return(OutPut)
end

;***********************************************************************;
; Function : get_att_value(                                             ;
;              opt[1]  : logical                                        ;
;              attname : string                                         ;
;              default_val                                              ;
;                                                                       ;
; This function checks to see if the given attribute has been set, and  ;
; if so, it returns its value. Otherwise, it returns the default value  ;
; which is the 3rd argument.                                            ;
;***********************************************************************;
undef("get_att_value")
function get_att_value(opt,attname:string,default_val)
local return_val
begin
  if(islogical(opt).and.opt.and..not.any(ismissing(getvaratts(opt))).and.\
    isatt(opt,attname)) then
    return_val = opt@$attname$
  else
    return_val = default_val
  end if
  return(return_val)
end


;======================================================================
; This function will return True if Opt is True and the AttName 
; attribute is a logical attribute that is set to True
;======================================================================
undef("isatt_logical_true")
function isatt_logical_true(Opt[1]:logical,AttName[1]:string)
begin
    if (isatt(Opt,AttName).and.islogical(Opt@$AttName$)) then
        return(Opt@$AttName$)
    end if
    return(False)
end     ; of isatt_logical_true(...)

;======================================================================
; This function will return True if Opt is True and the AttName 
; attribute is a logical attribute that is set to False
;======================================================================
undef("isatt_logical_false")
function isatt_logical_false(Opt[1]:logical,AttName[1]:string)
begin
    if (isatt(Opt,AttName).and.islogical(Opt@$AttName$).and. \
        .not.Opt@$AttName$) then
        return(True)
    end if
    return(False)
end     ; of isatt_logical_false(...)

;======================================================================
; This procedure checks if a file already exists, and removes if 
; certain options are set.
;
; If opt=True and opt@Overwrite = True, then user will be prompted
; whether to remove file.
;
; Otherwise, if opt=True and opt@ForceOverwrite = True, then file
; will be removed with no prompting.
;======================================================================
undef("check_for_file")
procedure check_for_file(fname,opt)
local DEBUG
begin
    DEBUG = isatt_logical_true(opt,"Debug")
    if (isfilepresent(fname)) then
        if(isatt_logical_true(opt,"Overwrite")) then
            system("rm -rfi "+fname)
        else if (isatt_logical_true(opt,"ForceOverwrite")) then
            system("rm -rf "+fname)
        else
            print("check_for_file: the file '" + fname + "' already exists.")
            print("                Set opt@Overwrite      = True")
            print("                 or opt@ForceOverwrite = True")
            print("                 to override.")
            exit
        end if
        end if
    end if
end

;======================================================================
; This procedure checks if two attributes are set, and set to 
; different values. If so, they will be set to the given value,
; and a warning printed.
;======================================================================
undef("check_both_atts")
procedure check_both_atts(opt,att1,att2,attval)
begin
  if (isatt(opt,att1).and.isatt(opt,att2).and.\
      opt@$att1$.ne.opt@$att2$) then
    print("check_both_atts: you have set " + att1 + " and")
    print("      " + att2 + " to different values.")
    print("      Setting them both to " + attval + " to be safe.")
    opt@$att1$ = attval
    opt@$att2$ = attval
  end if
end

;======================================================================
; This function coerces the type of a variable to the given type.
; It will also copy the attributes.
;======================================================================
undef("totypeof")
function totypeof(inVar,outType)
begin
    Err="OK"
    if (outType.eq."double") then
        outVar=todouble(inVar)
    else if (outType.eq."float") then
        outVar=tofloat(inVar)
    else if (outType.eq."integer") then
        outVar=tointeger(inVar)
    else if (outType.eq."int64") then
        outVar=toint64(inVar)
    else if (outType.eq."uint64") then
        outVar=touint64(inVar)
    else if (outType.eq."long") then
        outVar=tolong(inVar)
    else if (outType.eq."ulong") then
        outVar=toulong(inVar)
    else if (outType.eq."uint") then
        outVar=touint(inVar)
    else if (outType.eq."short") then
        outVar=toshort(inVar)
    else if (outType.eq."ushort") then
        outVar=toushort(inVar)
    else if (outType.eq."byte") then
        outVar=tobyte(inVar)
    else if (outType.eq."ubyte") then
        outVar=toubyte(inVar)
    else if (outType.eq."string") then
        outVar=tostring(inVar)
    else if (outType.eq."character") then
        outVar=tocharacter(inVar)
    else
        print("totypeof: Error: conversion to the provided type is not supported.")
        exit
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    copy_VarAtts_except(inVar,outVar,"_FillValue")
    return(outVar)
end     ; of totypeof(...)

;======================================================================
; This function returns the grid mask name, if any, being provided
; by the user.  This is to handle the old case of "Mask2D" being
; used (pre NCL V6.3.0), and the new "GridMask" being used.
;======================================================================
undef("get_mask_name")
function get_mask_name(opt)
local valid_mask_names, i
begin
    if(.not.opt) then
      return("")
    end if
    valid_mask_names = (/"GridMask","Mask2D"/)
    do i=0,dimsizes(valid_mask_names)-1
      if(isatt(opt,valid_mask_names(i))) then
        return(valid_mask_names(i))
      end if
    end do
    return("")
end

;======================================================================
; This procedure rotates the given lat/lon grid, given a rotation angle.
; It operates directly on the input lat/lon.
;======================================================================
undef("rotate_latlon")
procedure rotate_latlon(lat:snumeric,lon:snumeric,Rot[1]:numeric)
local d2r, tmpPoints, latlon_dims
begin
    if ( any(dimsizes(lat).ne.dimsizes(lon) ) ) then
        print("rotate_latlon: lat and lon must have the same dimensions.")
        exit
    end if
    
    latlon_dims    = dimsizes(lat)
    d2r            = atan(1)/45.0;
    RotateMat      = (/(/cos(Rot*d2r), -sin(Rot*d2r)/), \
                       (/sin(Rot*d2r),  cos(Rot*d2r)/) /)

    tmpPoints      = new( (/2,product(latlon_dims)/),typeof(lat) )
    tmpPoints(0,:) = ndtooned(lat)
    tmpPoints(1,:) = ndtooned(lon)
    tmpPoints      = RotateMat#tmpPoints
    lat            = onedtond(tmpPoints(1,:),latlon_dims)
    lon            = onedtond(tmpPoints(0,:),latlon_dims)
end

;======================================================================
; This functions calculates the mirror of p1 with respect to po.
;======================================================================
undef("mirrorP2P")
function mirrorP2P(p1,po)
local dVec
begin
    dVec    = p1-po
    MirrorP = po-dVec
    return(MirrorP)
end     ; of mirrorP2P(...)

;======================================================================
; This functions checks the given string for a valid grid type:
;    - "rectilinear"
;    - "curvilinear"
;    - "unstructred"
;    - "1x1", "0.25x0.25", etc. 
;    - "1deg", "0.25deg", etc.
;    - "G64", "G128", tec.
;
; If a valid type is found, then it will be returned as a string:
;  - "rectilinear" - returns "rectilinear"
;  - "curvilinear" - returns "curvilinear"
;  - "unstructured" - returns "unstructured"
;
; These grid types will return additional attributes:
;  - "1x1", "0.25x0.25", etc - returns "deg" and nlat,nlon as attributes
;  - "1deg", "0.25deg", etc  - returns "deg" and nlat,nlon as attributes
;  - "G64", "G128", etc      - returns "gaussian" and the nlon number
;
; If a valid type is not found, then "none" is returned. 
;======================================================================
undef("check_grid_type")
function check_grid_type(grid_type[1]:string)
local str, dlat, dlon, nlon, ret
begin
  ret     = grid_type     ; this may change
  bad_ret = "none"
  if(any(grid_type.eq.(/"rectilinear","curvilinear","unstructured"/))) then
    return(ret)
  end if

;---Check for "1x1", "2x3", "0.25x0.25" format
  if(isStrSubset(grid_type,"x")) then
    str = str_split(grid_type,"x")
    if(dimsizes(str).ne.2) then
      print("check_grid_type: invalid format for NxM type of grid.")
      return(bad_ret)
    end if
    dlat = tofloat(str(0))
    dlon = tofloat(str(1))
    if(ismissing(dlat).or.ismissing(dlon)) then
      print("check_grid_type: invalid format for NxM type of grid.")
      return(bad_ret)
    end if
    ret      = "degree"
    ret@dlat = dlat
    ret@dlon = dlon
    return(ret)
  end if

;---Check for "1deg", "0.25 deg" format
  if(isStrSubset(grid_type,"deg")) then
    str = str_split(grid_type,"deg")
    if(dimsizes(str).ne.1) then
      print("check_grid_type: invalid format for Ndeg type of grid.")
      return(bad_ret)
    end if
    dlat = tofloat(str(0))
    if(ismissing(dlat)) then
      print("check_grid_type: invalid format for Ndeg type of grid.")
      return(bad_ret)
    end if
    ret      = "degree"
    ret@dlat = dlat
    ret@dlon = dlat    ; same value for both
    return(ret)
  end if

;---Check for "G64", "G 128" format
  if(isStrSubset(grid_type,"G")) then
    str = str_split(grid_type,"G")
    if(dimsizes(str).ne.1) then
      print("check_grid_type: invalid format for gaussian grid.")
      return(bad_ret)
    end if
    nlon = tointeger(str(0))
    if(ismissing(nlon)) then
      print("check_grid_type: invalid format for gaussian grid.")
      return(bad_ret)
    end if
    if((nlon%2).ne.0) then
      print("check_grid_type: invalid number for gaussian grid.")
      return(bad_ret)
    end if
    ret = "gaussian"
    ret@nlon = nlon
    ret@nlat = nlon/2
    return(ret)
  end if
  print("check_grid_type: invalid grid type")
  return(bad_ret)
end 

;======================================================================
; This procedure is meant as a replacement for 
; "calc_SCRIP_corners_noboundaries" (previously called 
; "find_SCRIP_corners"). It takes the same arguments, but uses a 
; different algorithm that was suggested by Bob Oehmke.
;
; After 6.1.0-beta, I realized that this algorithm really should only
; be used if any of the lat points are at the boundary. At this point,
; I  decided to rename this function from "calc_SCRIP_corners" to 
; "calc_SCRIP_corners_boundaries", and go back to using 
; "calc_SCRIP_corners_noboundaries" as the regular algorithm.
;======================================================================
undef("calc_SCRIP_corners_boundaries")
procedure calc_SCRIP_corners_boundaries(lat2d[*][*],lon2d[*][*], \
                                        grid_corner_lat, grid_corner_lon,\
                                        opt)
local DEBUG, latlon_dims, grid_corner_lat2d,  grid_corner_lon2d, \
      nlat, nlon, nlatp1, nlonp1
begin
    DEBUG = isatt_logical_true(opt,"Debug")

;---Check input dimension sizes.
    if (any(dimsizes(lat2d).ne.dimsizes(lon2d))) then
        print("calc_SCRIP_corners_boundaries: lat and lon must have the same dimensions.")
        exit
    end if

    if (any(dimsizes(grid_corner_lat).ne.dimsizes(grid_corner_lon))) then
        print("calc_SCRIP_corners_boundaries: lat and lon must have the same dimensions.")
        exit
    end if

;---Get dimension sizes
    latlon_dims     = dimsizes(lat2d)
    cnr_latlon_dims = dimsizes(grid_corner_lat)
    cnr_rank        = dimsizes(cnr_latlon_dims)
    nlat            = latlon_dims(0)
    nlon            = latlon_dims(1)
    nlatp1          = nlat+1
    nlonp1          = nlon+1

    if(cnr_rank.ne.2.or.cnr_latlon_dims(0).ne.(nlat*nlon).or. \
       cnr_latlon_dims(1).ne.4) then
      print("calc_SCRIP_corners_boundaries: grid_corner_lat/grid_corner_lon must be (nlat*nlon) x 4")
      exit
    end if

    if(DEBUG) then
      print("calc_SCRIP_corners_boundaries")
      print("     min/max original lat: " + min(lat2d) + "/" + max(lat2d))
      print("     min/max original lon: " + min(lon2d) + "/" + max(lon2d))
    end if

;---Create array to hold a 2D array of lat/lon corners
    grid_corner_lat2d = new((/nlatp1,nlonp1/),typeof(lat2d))
    grid_corner_lon2d = new((/nlatp1,nlonp1/),typeof(lon2d))

;---Calculate interior locations of corner grid
    do i=1,nlat-1
      do j=1,nlon-1
        grid_corner_lat2d(i,j) = avg((/lat2d(i-1,j-1),lat2d(i,j-1),\
                                       lat2d(i-1,j),  lat2d(i,j)/))
        grid_corner_lon2d(i,j) = avg((/lon2d(i-1,j-1),lon2d(i,j-1),\
                                       lon2d(i-1,j),  lon2d(i,j)/))
      end do
    end do

;---Calculate bottom locations of corner grid
    do j=1,nlon-1
      grid_corner_lat2d(0,j) = avg((/lat2d(0,j),lat2d(0,j-1)/))
      grid_corner_lon2d(0,j) = avg((/lon2d(0,j),lon2d(0,j-1)/))
    end do

;---Calculate top locations of corner grid
    do j=1,nlon-1
      grid_corner_lat2d(nlat,j) = avg((/lat2d(nlat-1,j), \
                                        lat2d(nlat-1,j-1)/))
      grid_corner_lon2d(nlat,j) = avg((/lon2d(nlat-1,j), \
                                        lon2d(nlat-1,j-1)/))
    end do

;---Calculate left locations of corner grid
    do i=1,nlat-1
      grid_corner_lat2d(i,0) = avg((/lat2d(i,0),lat2d(i-1,0)/))
      grid_corner_lon2d(i,0) = avg((/lon2d(i,0),lon2d(i-1,0)/))
    end do

;---Calculate right locations of corner grid
    do i=1,nlat-1
      grid_corner_lat2d(i,nlon) = avg((/lat2d(i,nlon-1), \
                                        lat2d(i-1,nlon-1)/))
      grid_corner_lon2d(i,nlon) = avg((/lon2d(i,nlon-1), \
                                        lon2d(i-1,nlon-1)/))
    end do

;---Set four corners of corner grid
    grid_corner_lat2d(0,0)       = lat2d(0,0)
    grid_corner_lon2d(0,0)       = lon2d(0,0)
    grid_corner_lat2d(0,nlon)    = lat2d(0,nlon-1)
    grid_corner_lon2d(0,nlon)    = lon2d(0,nlon-1)
    grid_corner_lat2d(nlat,nlon) = lat2d(nlat-1,nlon-1)
    grid_corner_lon2d(nlat,nlon) = lon2d(nlat-1,nlon-1)
    grid_corner_lat2d(nlat,0)    = lat2d(nlat-1,0)
    grid_corner_lon2d(nlat,0)    = lon2d(nlat-1,0)

    if(DEBUG) then
      print("calc_SCRIP_corners_boundaries")
      print("     min/max grid_corner_lat2d: " + \
                  min(grid_corner_lat2d) + "/" + max(grid_corner_lat2d))
      print("     min/max grid_corner_lon2d: " + \
                  min(grid_corner_lon2d) + "/" + max(grid_corner_lon2d))
    end if

    n = 0
    do i=0,nlat-1
      do j=0,nlon-1
        grid_corner_lat(n,0) = grid_corner_lat2d(i,  j)
        grid_corner_lat(n,1) = grid_corner_lat2d(i,  j+1)
        grid_corner_lat(n,2) = grid_corner_lat2d(i+1,j+1)
        grid_corner_lat(n,3) = grid_corner_lat2d(i+1,j)
        grid_corner_lon(n,0) = grid_corner_lon2d(i,  j)
        grid_corner_lon(n,1) = grid_corner_lon2d(i,  j+1)
        grid_corner_lon(n,2) = grid_corner_lon2d(i+1,j+1)
        grid_corner_lon(n,3) = grid_corner_lon2d(i+1,j)
        n=n+1
      end do
    end do

    if(DEBUG) then
      print("calc_SCRIP_corners_boundaries")
      print("     min/max grid_corner_lat: " + min(grid_corner_lat) + \
                                         "/" + max(grid_corner_lat))
      print("     min/max grid_corner_lon: " + min(grid_corner_lon) + \
                                         "/" + max(grid_corner_lon))
    end if

    delete([/grid_corner_lat2d,grid_corner_lon2d/])

end     ; of calc_SCRIP_corners_boundaries(...)

;======================================================================
; This procedure is meant as a test to see if calling 
; calc_SCRIP_corners_boundaries once for lat and once for lon is 
; better than doing both lat/lon in one call. The reason for this
; is doing both can be unnecessarily memory intensive.
;======================================================================
undef("calc_SCRIP_corners_boundaries_2")
procedure calc_SCRIP_corners_boundaries_2(ll2d[*][*],grid_corner_ll,\
                                        ll_dim_name,opt)
local DEBUG, latlon_dims, grid_corner_ll2d, nlat, nlon, nlatp1, nlonp1
begin
    DEBUG = isatt_logical_true(opt,"Debug")

;---Get dimension sizes
    latlon_dims     = dimsizes(ll2d)
    cnr_latlon_dims = dimsizes(grid_corner_ll)
    cnr_rank        = dimsizes(cnr_latlon_dims)
    nlat            = latlon_dims(0)
    nlon            = latlon_dims(1)
    nlatp1          = nlat+1
    nlonp1          = nlon+1

    if(cnr_rank.ne.2.or.cnr_latlon_dims(0).ne.(nlat*nlon).or. \
       cnr_latlon_dims(1).ne.4) then
      print("calc_SCRIP_corners_boundaries_2: grid_corner_lat/grid_corner_lon must be (nlat*nlon) x 4")
      exit
    end if

    if(DEBUG) then
      print("calc_SCRIP_corners_boundaries_2")
      print("     min/max original " + ll_dim_nam + " : " + min(ll2d) + "/" + max(ll2d))
    end if

;---Create array to hold a 2D array of lat/lon corners
    grid_corner_ll2d = new((/nlatp1,nlonp1/),typeof(ll2d))

;---Calculate interior locations of corner grid
    do i=1,nlat-1
      do j=1,nlon-1
        grid_corner_ll2d(i,j) = avg((/ll2d(i-1,j-1),ll2d(i,j-1),\
                                      ll2d(i-1,j),  ll2d(i,j)/))
      end do
    end do

;---Calculate bottom locations of corner grid
    do j=1,nlon-1
      grid_corner_ll2d(0,j) = avg((/ll2d(0,j),ll2d(0,j-1)/))
    end do

;---Calculate top locations of corner grid
    do j=1,nlon-1
      grid_corner_ll2d(nlat,j) = avg((/ll2d(nlat-1,j), \
                                       ll2d(nlat-1,j-1)/))
    end do

;---Calculate left locations of corner grid
    do i=1,nlat-1
      grid_corner_ll2d(i,0) = avg((/ll2d(i,0),ll2d(i-1,0)/))
    end do

;---Calculate right locations of corner grid
    do i=1,nlat-1
      grid_corner_ll2d(i,nlon) = avg((/ll2d(i,nlon-1), \
                                       ll2d(i-1,nlon-1)/))
    end do

;---Set four corners of corner grid
    grid_corner_ll2d(0,0)       = ll2d(0,0)
    grid_corner_ll2d(0,nlon)    = ll2d(0,nlon-1)
    grid_corner_ll2d(nlat,nlon) = ll2d(nlat-1,nlon-1)
    grid_corner_ll2d(nlat,0)    = ll2d(nlat-1,0)

    if(DEBUG) then
      print("calc_SCRIP_corners_boundaries_2")
      print("     min/max grid_corner_" + ll_dim_name + ": " + \
                  min(grid_corner_ll2d) + "/" + max(grid_corner_ll2d))
    end if

    n = 0
    do i=0,nlat-1
      do j=0,nlon-1
        grid_corner_ll(n,0) = grid_corner_ll2d(i,  j)
        grid_corner_ll(n,1) = grid_corner_ll2d(i,  j+1)
        grid_corner_ll(n,2) = grid_corner_ll2d(i+1,j+1)
        grid_corner_ll(n,3) = grid_corner_ll2d(i+1,j)
        n=n+1
      end do
    end do

    if(DEBUG) then
      print("calc_SCRIP_corners_boundaries_2")
      print("     min/max grid_corner_" + ll_dim_name + ": " + min(grid_corner_ll) + \
                                         "/" + max(grid_corner_ll))
    end if

    delete(grid_corner_ll2d)

end     ; of calc_SCRIP_corners_boundaries_2(...)

;======================================================================
; This procedure is used within the curvilinear_to_SCRIP function.
; It used to be called "find_SCRIP_corners".
;
; Given a structured grid, it looks for the four corners surrounding
; each node. It returns the center of the cells for the corners.
;
; You have to be careful with this routine. It doesn't check if you
; are at or near the edge of the globe (i.e. lat=90, or lon=-180),
; and hence it might give you corners that fall outside -90/90 for lat
; and/or 0/360 for lon.
;
; Really, if the user has a special grid, he/she should input the
; corners via the GridCornerLat and GridCornerLon resources.
;======================================================================
undef("calc_SCRIP_corners_noboundaries")
procedure calc_SCRIP_corners_noboundaries(lat2d[*][*],lon2d[*][*], \
                             grid_corner_lat,grid_corner_lon,opt)
local latlon_dims, nlat, nlon, Extlon2d, Extlat2d, ExtGridCenter_lat, \
      nm, found_mask, valid_mask_names, ExtGridCenter_lon, tmp, ii, jj, DEBUG
begin
    DEBUG = isatt_logical_true(opt,"Debug")
    delta = get_att_value(opt,"GridCornerDelta",0.25)

    if ( any(dimsizes(lat2d).ne.dimsizes(lon2d) ) ) then
        print("calc_SCRIP_corners_noboundaries: lat and lon must have the same dimensions.")
        exit
    end if

    latlon_dims = dimsizes(lat2d)
    nlat        = latlon_dims(0)
    nlon        = latlon_dims(1)
    nlatp1      = nlat+1
    nlonp1      = nlon+1
    nlatp2      = nlat+2
    nlonp2      = nlon+2
    if(DEBUG) then
      print("calc_SCRIP_corners_noboundaries")
      print("     min/max original lat: " + min(lat2d) + "/" + max(lat2d))
      print("     min/max original lon: " + min(lon2d) + "/" + max(lon2d))
    end if
;
; Extend the lat/lon grid (needed to calculate the
; corners at the boundaries).
;
    Extlat2d = new( (/nlatp2, nlonp2/),typeof(lat2d))
    Extlon2d = new( (/nlatp2, nlonp2/),typeof(lon2d))

;---The middle grid is exactly the original lat2d/lon2d arrays
    Extlat2d(1:nlat,1:nlon) = (/lat2d/)
    Extlon2d(1:nlat,1:nlon) = (/lon2d/)
    
;---bottom row, minus corners
    Extlat2d(0,1:nlon)      = mirrorP2P(lat2d(1,:),lat2d(0,:))
    Extlon2d(0,1:nlon)      = mirrorP2P(lon2d(1,:),lon2d(0,:))

;---top, minus corners
    Extlat2d(nlatp1,1:nlon) = mirrorP2P(lat2d(nlat-2,:),lat2d(nlat-1,:))
    Extlon2d(nlatp1,1:nlon) = mirrorP2P(lon2d(nlat-2,:),lon2d(nlat-1,:))

;---left, minus corners
    Extlat2d(1:nlat,0)      = mirrorP2P(lat2d(:,1),lat2d(:,0))
    Extlon2d(1:nlat,0)      = mirrorP2P(lon2d(:,1),lon2d(:,0))

;---right, minus corners
    Extlat2d(1:nlat,nlonp1) = mirrorP2P(lat2d(:,nlon-2),lat2d(:,nlon-1)) 
    Extlon2d(1:nlat,nlonp1) = mirrorP2P(lon2d(:,nlon-2),lon2d(:,nlon-1))

;---lower left corner
    Extlat2d(0,0)           = mirrorP2P(lat2d(1,1),lat2d(0,0))
    Extlon2d(0,0)           = mirrorP2P(lon2d(1,1),lon2d(0,0))

;---upper right corner
    Extlat2d(nlatp1,nlonp1) = mirrorP2P(lat2d(nlat-2,nlon-2), \
                                        lat2d(nlat-1,nlon-1))
    Extlon2d(nlatp1,nlonp1) = mirrorP2P(lon2d(nlat-2,nlon-2),\
                                        lon2d(nlat-1,nlon-1))
;---lower right corner
    Extlat2d(0,nlonp1)      = mirrorP2P(lat2d(1,nlon-2),lat2d(0,nlon-1))
    Extlon2d(0,nlonp1)      = mirrorP2P(lon2d(1,nlon-2),lon2d(0,nlon-1))

;---upper left corner
    Extlat2d(nlatp1,0)      = mirrorP2P(lat2d(nlat-2,1),lat2d(nlat-1,0))
    Extlon2d(nlatp1,0)      = mirrorP2P(lon2d(nlat-2,1),lon2d(nlat-1,0))
    
;
; This kludge is commented out for now, because it's not a good one.
; We need a better way to fix the boundary corners if they go over.
;
;;    if(DEBUG) then
;;      print("calc_SCRIP_corners_noboundaries")
;;      print("      Before kludge to fix lat/lon corners...")
;;      print("      min/max Extlat2d: " + min(Extlat2d) + "/" + max(Extlat2d))
;;      print("      min/max Extlon2d: " + min(Extlon2d) + "/" + max(Extlon2d))
;;    end if
;;
;;;---Kludge to cap the latitudes at -90/90
;;    Extlat2d = where(Extlat2d.lt. -90, -90,Extlat2d)
;;    Extlat2d = where(Extlat2d.gt.  90,  90,Extlat2d)
;;    Extlon2d = where(Extlon2d.lt.   0,   0,Extlon2d)
;;    Extlon2d = where(Extlon2d.gt. 360, 360,Extlon2d)
;;
;;    if(DEBUG) then
;;      print("calc_SCRIP_corners_noboundaries")
;;      print("     After kludge to fix lat/lon corners...")
;;      print("     min/max Extlat2d: " + min(Extlat2d) + "/" + max(Extlat2d))
;;      print("     min/max Extlon2d: " + min(Extlon2d) + "/" + max(Extlon2d))
;;    end if

    if(DEBUG) then
      print("calc_SCRIP_corners_noboundaries")
      print("     min/max Extlat2d: " + min(Extlat2d) + "/" + max(Extlat2d))
      print("     min/max Extlon2d: " + min(Extlon2d) + "/" + max(Extlon2d))
    end if
;
; Calculate the cell center of the extended grid, which 
; would be the corner coordinates for the original grid.
;
    tmp = Extlat2d(:,1:nlonp1)+Extlat2d(:,0:nlon)
    ExtGridCenter_lat       = ndtooned((tmp(1:nlatp1,:)+tmp(0:nlat,:))*delta)

    tmp = Extlon2d(:,1:nlonp1)+Extlon2d(:,0:nlon)
    ExtGridCenter_lon       = ndtooned((tmp(1:nlatp1,:)+tmp(0:nlat,:))*delta)
    delete(tmp)

    if(DEBUG) then
      print("calc_SCRIP_corners_noboundaries")
      print("     min/max ExtGridCenter_lat: " + \
                  min(ExtGridCenter_lat) + "/" + max(ExtGridCenter_lat))
      print("     min/max ExtGridCenter_lon: " + \
                  min(ExtGridCenter_lon) + "/" + max(ExtGridCenter_lon))
    end if
;---Extract the grid cell corners
    ii = ndtooned(conform_dims((/nlat,nlon/),ispan(0,nlon-1,1),1))
    jj = ndtooned(conform_dims((/nlat,nlon/),ispan(0,nlat-1,1),0))

    grid_corner_lat(:,0) = ExtGridCenter_lat(jj*(nlonp1)+ii)
    grid_corner_lat(:,1) = ExtGridCenter_lat(jj*(nlonp1)+(ii+1))
    grid_corner_lat(:,2) = ExtGridCenter_lat((jj+1)*(nlonp1)+(ii+1))
    grid_corner_lat(:,3) = ExtGridCenter_lat((jj+1)*(nlonp1)+ii)
    
    grid_corner_lon(:,0) = ExtGridCenter_lon(jj*(nlonp1)+ii)
    grid_corner_lon(:,1) = ExtGridCenter_lon(jj*(nlonp1)+(ii+1))
    grid_corner_lon(:,2) = ExtGridCenter_lon((jj+1)*(nlonp1)+(ii+1))
    grid_corner_lon(:,3) = ExtGridCenter_lon((jj+1)*(nlonp1)+ii)

;--Clean up
    delete([/ExtGridCenter_lat,ExtGridCenter_lon,Extlat2d,Extlon2d/])

end     ; of calc_SCRIP_corners_noboundaries(...)

;======================================================================
; This procedure generates a SCRIP file for an unstructured grid.
; This is still being tested as of Feb 17, 2015.
;======================================================================
undef("unstructured_to_SCRIP")
procedure unstructured_to_SCRIP(FName[1]:string,inLat,inLon,Opt[1]:logical)
begin
;---Check for options
    PrintTimings = isatt_logical_true(Opt,"PrintTimings")
    if(PrintTimings) then
      start_time = get_start_time()
    end if

    DEBUG = isatt_logical_true(Opt,"Debug")

;---Check if the file already exists
    check_for_file(FName,Opt)

;---Do we need to create a large file?
    if (.not.isatt_logical_false(Opt,"LargeFile")) then
      setfileoption("nc","Format","LargeFile") 
    end if

;---Do we need to create a netcdf-4 file?
    if (isatt(Opt,"NetCDFType").and.\
        str_lower(Opt@NetCDFType).eq."netcdf4") then
      setfileoption("nc","Format","netcdf4") 
      nc_file_type = "netcdf4"
    else
      nc_file_type = "netcdf3"
    end if

    lat_dims = dimsizes(inLat)
    lon_dims = dimsizes(inLon)
    lat_rank = dimsizes(lat_dims)
    lon_rank = dimsizes(lon_dims)

    if (lat_rank.ne.1.or.lon_rank.ne.1.or.lat_dims.ne.lon_dims) then
        print("unstructured_to_SCRIP: latitude and longitude must be one-dimensional and the same length.")
        exit
    end if
    grid_size = lat_dims
    grid_rank = lat_rank  ; should be 1

;---Were the grid corners provided?
    if (isatt(Opt,"GridCornerLat").and.isatt(Opt,"GridCornerLon")) then
        if(DEBUG) then
          print("unstructured_to_SCRIP: using grid corners provided by user...")
        end if
        if(.not.any(typeof(Opt@GridCornerLat).eq.(/"float","double"/))) then
          GridCornerLat = tofloat(Opt@GridCornerLat)
        else
          GridCornerLat = Opt@GridCornerLat
        end if
        if(.not.any(typeof(Opt@GridCornerLon).eq.(/"float","double"/))) then
          GridCornerLon = tofloat(Opt@GridCornerLon)
        else
          GridCornerLon = Opt@GridCornerLon
        end if
        corner_lat_dims = dimsizes(GridCornerLat)
        corner_lon_dims = dimsizes(GridCornerLon)
        corner_lat_rank = dimsizes(corner_lat_dims)
        corner_lon_rank = dimsizes(corner_lon_dims)
        if(corner_lat_rank.ne.2.or.corner_lon_rank.ne.2.or.\
           any(corner_lat_dims.ne.corner_lon_dims).or.corner_lat_dims(0).ne.grid_size) then
          print("unstructured_to_SCRIP: the grid corners provided are not the correct dimensionality.")
          print("Was expecting a " + lat_dims + " x n array, where 'n' represents the number of corners.")
          exit
        end if
        grid_corners    = corner_lat_dims(1)
        grid_corner_lat = reshape( GridCornerLat,(/ grid_size, grid_corners /))
        grid_corner_lon = reshape( GridCornerLon,(/ grid_size, grid_corners /))
    else
      print("unstructured_to_SCRIP: no grid corners ([Src]/[Dst]GridCorner[Lat]/[Lon]) provided. Can't continue")
      exit
    end if

;---Was a mask provided?
    grid_mask_name = get_mask_name(Opt)
    if(grid_mask_name.ne."") then
      mask_dims = dimsizes(Opt@$grid_mask_name$)
      mask_rank = dimsizes(mask_dims)
      if(mask_rank.ne.1.or.mask_dims.ne.grid_size) then
        print("unstructured_to_SCRIP: Opt@" + grid_mask_name + \
                  " is not the correct dimensionality")
        exit
      end if   
      grid_mask = Opt@$grid_mask_name$
    else
      grid_mask = new(grid_size, "integer","No_FillValue")
      grid_mask = 1
    end if
    
;---Was a grid area provided? (Required for conserve regridding)
    if(Opt.and.isatt(Opt,"GridArea")) then
      grid_area = todouble(Opt@GridArea)
      if (dimsizes(dimsizes(grid_area).ne.1.and.grid_size.ne.dimsizes(grid_area)))
        print("unstructured_to_SCRIP: Area array is not the appropriate size.")
        exit
      end if
      grid_area_provided = True
    else
      grid_area_provided = False
    end if

    if(Opt.and.isatt(Opt,"InputFileName")) then
      inputFName = Opt@InputFileName
    else
      inputFName = "none provided"
    end if

    if(Opt.and.isatt(Opt,"Title")) then
      FTitle = Opt@Title
    else
      FTitle = "unstructured_to_SCRIP (" + grid_size + ")"
    end if

;---Create the file
    fid = addfile(FName,"c")
    setfileoption(fid,"DefineMode",True)

;---Define the file attributes
    FileAtt              = True
    FileAtt@title        = FTitle
    FileAtt@Conventions  = "SCRIP"
    FileAtt@Createdby    = "ESMF_regridding.ncl"
    FileAtt@date_created = systemfunc("date")
    FileAtt@inputFile     = inputFName 
    fileattdef(fid,FileAtt)
    
;---Define the SCRIP dimensions on the file
    FDimNames = (/ "grid_size","grid_corners","grid_rank" /)
    FDimSizes = (/ grid_size,grid_corners,grid_rank /)
    FDimUnlim = (/ False,False,False /)
	
    filedimdef(fid,FDimNames,FDimSizes,FDimUnlim)
	
;---Define Variables
    filevardef(fid,"grid_dims","integer","grid_rank")
    filevardef(fid,"grid_center_lat","double","grid_size")
    filevardef(fid,"grid_center_lon","double","grid_size")
    filevardef(fid,"grid_corner_lat","double",(/ "grid_size", "grid_corners" /) )
    filevardef(fid,"grid_corner_lon","double",(/ "grid_size", "grid_corners" /) )
    filevardef(fid,"grid_imask","integer","grid_size")
    
    if(grid_area_provided) then
      filevardef(fid,"grid_area","double","grid_size")
    end if
    
;---Define the variables unit attribute
    DummyAtt1       = 0
    DummyAtt2       = 0
;
; The conversion to character arrays for NetCDF-4 files is a 
; work-around.  ESMF_RegridWeightGen uses the F90 interface to 
; read NetCDF-4 files, which doesn't yet have support for 
; NetCDF-4 "string" types.
; 
    if(nc_file_type.eq."netcdf3") then
      DummyAtt1@units = "degrees"
      DummyAtt2@units = "unitless"
    else
      DummyAtt1@units = tochar("degrees")
      DummyAtt2@units = tochar("unitless")
    end if
    filevarattdef(fid,"grid_center_lat",DummyAtt1)
    filevarattdef(fid,"grid_center_lon",DummyAtt1)
    filevarattdef(fid,"grid_imask",DummyAtt2)
    filevarattdef(fid,"grid_corner_lat",DummyAtt1)
    filevarattdef(fid,"grid_corner_lon",DummyAtt1)

    if(grid_area_provided) then
      DummyAtt3 = 0
      if(nc_file_type.eq."netcdf3") then
        DummyAtt3@units     = "radians^2"
        DummyAtt3@long_name = "area weights"
      else
        DummyAtt3@units     = tochar("radians^2")
        DummyAtt3@long_name = tochar("area weights")
      end if
      filevarattdef(fid,"grid_area",DummyAtt3)
      delete(DummyAtt3)
    end if

    delete(DummyAtt1)
    delete(DummyAtt2)
    
;---Prepare the file to store the values
    setfileoption(fid,"DefineMode",False)
    
;---Storing Grid Dims
    fid->grid_dims = (/grid_size/)
    
;---Store Cell Center Lat/Lon
    fid->grid_center_lat = (/inLat/)
    fid->grid_center_lon = (/inLon/)

;---Store Cell Corner Lat/Lon
    fid->grid_corner_lat = (/grid_corner_lat/)
    fid->grid_corner_lon = (/grid_corner_lon/)

;---Store grid mask
    fid->grid_imask = (/grid_mask/)

;---Store grid area
    if(grid_area_provided) then
      fid->grid_area = (/grid_area/)
    end if

;---Store grid mask
    fid->grid_imask = (/grid_mask/)

    if(PrintTimings) then
      print_elapsed_esmf_time(start_time,"unstructured_to_SCRIP")
    end if
end     ; of unstructured_to_SCRIP(...)

;======================================================================
; This procedure generates an ESMF file for an unstructured grid.
; It is sometimes better to even store logically rectangular and 
; structured grids as unstructured grids. (Refer to TriPolar - TGrid)
;======================================================================
undef("unstructured_to_ESMF")
procedure unstructured_to_ESMF(FName[1]:string,inLat,inLon,Opt[1]:logical)
local DEBUG, lat, lon, NodeMask, ElementVertices, title, grid_mask_name
begin
;---Check for options
    PrintTimings = isatt_logical_true(Opt,"PrintTimings")
    if(PrintTimings) then
      start_time = get_start_time()
    end if

    DEBUG = isatt_logical_true(Opt,"Debug")

;---Check if the file already exists
    check_for_file(FName,Opt)

;---Do we need to create a large file?
    if (.not.isatt_logical_false(Opt,"LargeFile")) then
      setfileoption("nc","Format","LargeFile") 
    end if

;---Do we need to create a netcdf-4 file?
    if (isatt(Opt,"NetCDFType").and.\
        str_lower(Opt@NetCDFType).eq."netcdf4") then
      setfileoption("nc","Format","netcdf4") 
      nc_file_type = "netcdf4"
    else
      nc_file_type = "netcdf3"
    end if

    lat = ndtooned(inLat)
    lon = ndtooned(inLon)

    if (dimsizes(lat).ne.dimsizes(lon)) then
        print("unstructured_to_ESMF: latitude and longitude must have the same number of elements.")
        exit
    end if
;---Was a mask provided?
    grid_mask_name = get_mask_name(Opt)
    if(grid_mask_name.ne."") then
      if(.not.all(product(dimsizes(Opt@$grid_mask_name$)).eq.dimsizes(lat))) then
        print("unstructured_to_ESMF: Opt@" + grid_mask_name + \ 
              " is not the correct dimensionality")
        exit
      else
        NodeMask = ndtooned(Opt@$grid_mask_name$)
      end if
    else
;---No masking
      NodeMask = new(dimsizes(lat), "integer","No_FillValue")
      NodeMask = 1
    end if
    
    if(Opt.and.isatt(Opt,"InputFileName")) then
      inputFName = Opt@InputFileName
    else
      inputFName = "none provided"
    end if

;---Were ElementVertices provided?
    if(Opt.and.isatt(Opt,"ElementVertices")) then
      if(DEBUG) then
        print("unstructured_to_ESMF: using element vertices mesh provided by the user")
      end if
      if(dimsizes(dimsizes(Opt@ElementVertices)).ne.2) then
        print("unstructured_to_ESMF: ElementVertices must be an ncells x nedges array")
        exit
      end if
      ElementVertices = Opt@ElementVertices
;---Was a triangular mesh provided?
    else if(Opt.and.isatt(Opt,"TriangularMesh")) then
      dims = dimsizes(Opt@TriangularMesh)
      if(dims(1).ne.3) then
        print("unstructured_to_ESMF: TriangularMesh must be an ncells x 3 array")
        exit
      end if
      if(DEBUG) then
        print("unstructured_to_ESMF: using triangular mesh provided by the user")
      end if
      ElementVertices = Opt@TriangularMesh
;---Was a quadrilateral mesh provided?
    else if(Opt.and.isatt(Opt,"QuadrilateralMesh")) then
      dims = dimsizes(Opt@QuadrilateralMesh)
      if(dims(1).ne.4) then
        print("unstructured_to_ESMF: QuadrilateralMesh must be an ncells x 4 array")
        exit
      end if
      if(DEBUG) then
        print("unstructured_to_ESMF: using quadrilateral mesh provided by the user")
      end if
      ElementVertices = Opt@QuadrilateralMesh
    else
;---Create a triangular mesh if no mesh provided
      if(DEBUG) then
        print("unstructured_to_ESMF: triangulating the data to create the edges for your unstructured data.")
        print("                      This can be VERY slow if you have a large array.")
        print("                      To speed up code, provide your own edge vertices via the") 
        print("                      'ElementVertices' option.")
      end if
      ElementVertices = csstri(lat,lon)       ; Triangulate
    end if
    end if
    end if
    if(DEBUG) then
      print("min/max ElementVertices = " + min(ElementVertices) + "/" + \
                                           max(ElementVertices))
    end if
    ElementVertices_Dim = dimsizes(ElementVertices)

    if(DEBUG) then
      print("unstructured_to_ESMF: total number of elements created: "+ \
               ElementVertices_Dim(0))
    end if

;---Create the file
    fid = addfile(FName,"c")
    setfileoption(fid,"DefineMode",True)

;---Define the file attributes
    FileAtt               = True
    FileAtt@gridType      = "unstructured"
    FileAtt@Conventions   = "ESMF"
    FileAtt@Createdby     = "ESMF_regridding.ncl"
    FileAtt@version       = "0.9"
    FileAtt@inputFile     = inputFName 
    FileAtt@timeGenerated = systemfunc("date")
    FileAtt@date_created  = FileAtt@timeGenerated
    fileattdef(fid,FileAtt)
    
;---Define the ESMF dimensions
    nodeCount       = dimsizes(lat)
    elementCount    = ElementVertices_Dim(0)
    maxNodePElement = ElementVertices_Dim(1)
    coordDim        = 2
;                    0            1               2               3
    FDimNames=(/ "nodeCount","elementCount","maxNodePElement","coordDim" /)
    FDimSizes=(/  nodeCount , elementCount , maxNodePElement , coordDim  /)
    FDimUnlim=(/    False   ,    False     ,      False      ,   False   /)
	
    filedimdef(fid,FDimNames,FDimSizes,FDimUnlim)    

;---Define variables
    filevardef(fid,"nodeCoords",    "double", (/ FDimNames(0), FDimNames(3) /))
    filevardef(fid,"elementConn",   "integer",(/ FDimNames(1), FDimNames(2) /))
    filevardef(fid,"numElementConn","byte",   (/ FDimNames(1) /))
    filevardef(fid,"centerCoords",  "double", (/ FDimNames(1), FDimNames(3) /))
    filevardef(fid,"elementArea",   "double", (/ FDimNames(1) /))
    filevardef(fid,"elementMask",   "integer",(/ FDimNames(1) /))
    
;---Define the variables unit attribute
    AttNodeCoords               = 0
    AttElementConn              = 0
    AttNumElementConn           = 0
    AttCenterCoords             = 0
    AttElementArea              = 0
;
; The conversion to character arrays for NetCDF-4 files is a 
; work-around.  ESMF_RegridWeightGen uses the F90 interface to 
; read NetCDF-4 files, which doesn't yet have support for 
; NetCDF-4 "string" types.
; 
    if(nc_file_type.eq."netcdf3") then
      AttNodeCoords@units         = "degrees"
      AttElementConn@long_name    = "Node Indices that define the element connectivity"
      AttNumElementConn@long_name = "Number of nodes per element"
      AttCenterCoords@units       = "degrees"
      AttElementArea@units        = "radians^2"
      AttElementArea@long_name    = "area weights"
    else
      AttNodeCoords@units         = tochar("degrees")
      AttElementConn@long_name    = tochar("Node Indices that define the element connectivity")
      AttNumElementConn@long_name = tochar("Number of nodes per element")
      AttCenterCoords@units       = tochar("degrees")
      AttElementArea@units        = tochar("radians^2")
      AttElementArea@long_name    = tochar("area weights")
    end if
    
    AttElementConn@_FillValue   = -1
    
    filevarattdef(fid,"nodeCoords",    AttNodeCoords)
    filevarattdef(fid,"elementConn",   AttElementConn)
    filevarattdef(fid,"numElementConn",AttNumElementConn)
    filevarattdef(fid,"centerCoords",  AttCenterCoords)
    filevarattdef(fid,"elementArea",   AttElementArea)
    
;---Prepare the file to store the values
    setfileoption(fid,"DefineMode",False)
    
;---Store Node Coordinates
    nodeCoords      = new((/nodeCount,coordDim/),"double","No_FillValue")
    nodeCoords(:,0) = lon
    nodeCoords(:,1) = lat
    fid->nodeCoords = (/nodeCoords/)
    
;---Store Element Connectivity
    fid->elementConn = (/ElementVertices+1/)
    
;---Store numElementConn
    numElementConn      = new((/elementCount/),"byte","No_FillValue")
    numElementConn      = tobyte(3)
    fid->numElementConn = (/numElementConn/)

;---Store center of each element, but first calculating the centeroids.
    centerCoords      = new((/elementCount,coordDim/),"double","No_FillValue")
    centerCoords(:,0) = dim_avg(reshape(lon(ndtooned(ElementVertices)), \
                                       (/elementCount,3/)))
    centerCoords(:,1) = dim_avg(reshape(lat(ndtooned(ElementVertices)), \
                                        (/elementCount,3/)))
    fid->centerCoords = (/centerCoords/)
    
;---Store element area
    d2r = atan(1)/45.0;
;;  elementArea  = new(elementCount,"double","No_FillValue")
;; Redundant? Maybe needed to remove _FillValue?
;;  elementalLat = new((/elementCount,3/),"double","No_FillValue")
;;  elementalLon = new((/elementCount,3/),"double","No_FillValue")
    elementalLat = todouble(reshape(lat(ndtooned(ElementVertices)),(/elementCount,3/)))
    elementalLon = todouble(reshape(lon(ndtooned(ElementVertices)),(/elementCount,3/)))
    elementArea  = todouble(gc_tarea(elementalLat,elementalLon))
    delete(elementalLat)
    delete(elementalLon)
    if(DEBUG) then
      print("unstructured_to_ESMF: Element Area: min:" + min(elementArea) + \
                                               " max:" + max(elementArea))
    end if
    fid->elementArea = (/elementArea/)

;---Store the element Mask
    elementMask = dim_sum(reshape(NodeMask(ndtooned(ElementVertices)), \
                                  (/elementCount,3/)))
    elementMask      =  where(elementMask.eq.3,1,0)
    fid->elementMask = (/elementMask/)

    if(PrintTimings) then
      print_elapsed_esmf_time(start_time,"unstructured_to_ESMF")
    end if
end     ; of unstructured_to_ESMF(...)
;======================================================================
; This function receives a 2D curvilinear grid and mask and stores
; it in FName NetCDF file based on SCRIP standard.
; lat2d and lon2d must have (nlat,nlot) dimension.
; Opt controls the behavior of the function
; current attribute in Opt are:
; (1) Overwrite [Logical] if True, and if the out file already exists
;     it will erase the file.
; (2) ForceOverwrite [Logical] If set to True, the user is not asked
;     for removing an existing file. If set to false the user permission
;     is required to remove an existing file. This is ineffective if
;     Overwrite is set to False.
; (3) If GridCornerLat and GridCornerLon are set, then these will be used
;     instead of calcuation the corner points for the lat/lon cells.
;     The corner points are needed for the "conserve" method.
;======================================================================
undef("curvilinear_to_SCRIP")
procedure curvilinear_to_SCRIP(FName[1]:string,lat2d[*][*]:numeric,\
                               lon2d[*][*]:numeric,Opt[1]:logical)
local latlon_dims, nlat, nlon, fid, FileAtt, grid_siz, grid_corners, \
grid_rank, FDimNames, FDimSizes, FDimUnlim, DummyAtt1, DummyAtt2, \
GridCornerLat, GridCornerLon, grid_corner_lat, grid_corner_lon, grid_mask, \
lat_type, lon_type, DEBUG, nc_file_type
begin
;---Check for options
    PrintTimings = isatt_logical_true(Opt,"PrintTimings")
    if(PrintTimings) then
      start_time = get_start_time()
    end if

    DEBUG = isatt_logical_true(Opt,"Debug")
    TESTIT = isatt_logical_true(Opt,"Testit")

;---Check if the file already exists
    check_for_file(FName,Opt)

;---Do we need to create a large file?
    if (.not.isatt_logical_false(Opt,"LargeFile")) then
      setfileoption("nc","Format","LargeFile") 
    end if

;---Do we need to create a netcdf-4 file?
    if (isatt(Opt,"NetCDFType").and.\
        str_lower(Opt@NetCDFType).eq."netcdf4") then
      setfileoption("nc","Format","netcdf4") 
      nc_file_type = "netcdf4"
    else
      nc_file_type = "netcdf3"
    end if

    if ( any(dimsizes(lat2d).ne.dimsizes(lon2d)) ) then
        print("curvilinear_to_SCRIP: latitude and longitude must have the same number of elements.")
        exit
    end if

    latlon_dims = dimsizes(lat2d)
    nlat        = latlon_dims(0)
    nlon        = latlon_dims(1)

    if(Opt.and.isatt(Opt,"Title")) then
      FTitle = Opt@Title
    else
      FTitle = "curvilinear_to_SCRIP (" + nlat + "," + nlon + ")"
    end if

;---Was a mask provided?
    grid_mask_name = get_mask_name(Opt)
    if(grid_mask_name.ne."") then
      if(.not.all(dimsizes(Opt@$grid_mask_name$).eq.latlon_dims)) then
        print("curvilinear_to_SCRIP: Opt@" + grid_mask_name + \
              " is not the correct dimensionality")
        exit
      else
        grid_mask = Opt@$grid_mask_name$
      end if
    else
;---No masking
      grid_mask = new(latlon_dims, "integer","No_FillValue")
      grid_mask = 1
    end if

;---Create the file
    fid = addfile(FName,"c")
    setfileoption(fid,"DefineMode",True)

;---Define the file attributes
    FileAtt              = True
    FileAtt@title        = FTitle
    FileAtt@Conventions  = "SCRIP"
    FileAtt@Createdby    = "ESMF_regridding.ncl"
    FileAtt@date_created = systemfunc("date")
    fileattdef(fid,FileAtt)
    
;---Define the SCRIP dimensions
    grid_size    = nlat*nlon   ; This is number of data points (grid nodes)
    grid_corners = 4	
    grid_rank    = 2
	
    FDimNames = (/ "grid_size","grid_corners","grid_rank" /)
    FDimSizes = (/ grid_size,grid_corners,grid_rank /)
    FDimUnlim = (/ False,False,False /)
	
    filedimdef(fid,FDimNames,FDimSizes,FDimUnlim)
	
;---Define Variables
    filevardef(fid,"grid_dims","integer","grid_rank")
    filevardef(fid,"grid_center_lat","double","grid_size")
    filevardef(fid,"grid_center_lon","double","grid_size")
    filevardef(fid,"grid_imask","integer","grid_size")
    filevardef(fid,"grid_corner_lat","double",(/ "grid_size", "grid_corners" /) )
    filevardef(fid,"grid_corner_lon","double",(/ "grid_size", "grid_corners" /) )

;---Define the variables unit attribute
    DummyAtt1       = 0
    DummyAtt2       = 0
;
; The conversion to character arrays for NetCDF-4 files is a 
; work-around.  ESMF_RegridWeightGen uses the F90 interface to 
; read NetCDF-4 files, which doesn't yet have support for 
; NetCDF-4 "string" types.
; 
    if(nc_file_type.eq."netcdf3") then
      DummyAtt1@units = "degrees"
      DummyAtt2@units = "unitless"
    else
      DummyAtt1@units = tochar("degrees")
      DummyAtt2@units = tochar("unitless")
    end if
    filevarattdef(fid,"grid_center_lat",DummyAtt1)
    filevarattdef(fid,"grid_center_lon",DummyAtt1)
    filevarattdef(fid,"grid_imask",DummyAtt2)
    filevarattdef(fid,"grid_corner_lat",DummyAtt1)
    filevarattdef(fid,"grid_corner_lon",DummyAtt1)
    delete(DummyAtt1)
    delete(DummyAtt2)
    
;---Prepare the file to store the values
    setfileoption(fid,"DefineMode",False)
    
;---Storing Grid Dims
    fid->grid_dims = (/ nlon, nlat /)   ; SCRIP is FORTRAN-based.
                                        ; (nlat,nlon) in NCL is equivalent to
                                        ; (nlon,nlat) in FORTRAN
    
;---Store Cell Center Lat/Lon
    fid->grid_center_lat = (/ndtooned(lat2d)/)
    fid->grid_center_lon = (/ndtooned(lon2d)/)
	
;---Store Cell Masks
    if (grid_size.ne.dimsizes(ndtooned(grid_mask))) then
        print("curvilinear_to_SCRIP: Mask array is not the appropriate size.")
        exit
    else
        fid->grid_imask=(/ tointeger(ndtooned(grid_mask)) /)
    end if
	
;---Get the grid lat/lon corners
    if (isatt(Opt,"GridCornerLat").and.isatt(Opt,"GridCornerLon")) then
        if(DEBUG) then
          print("curvilinear_to_SCRIP: using grid corners provided by user...")
        end if
        if(.not.any(typeof(Opt@GridCornerLat).eq.(/"float","double"/))) then
          GridCornerLat = tofloat(Opt@GridCornerLat)
        else
          GridCornerLat = Opt@GridCornerLat
        end if
        if(.not.any(typeof(Opt@GridCornerLon).eq.(/"float","double"/))) then
          GridCornerLon = tofloat(Opt@GridCornerLon)
        else
          GridCornerLon = Opt@GridCornerLon
        end if
        grid_corner_lat = reshape( GridCornerLat,(/ grid_size, grid_corners /))
        grid_corner_lon = reshape( GridCornerLon,(/ grid_size, grid_corners /))
    else
;---Estimate the grid cell corners; it's better if the user provides them.
        if(DEBUG) then
          print("curvilinear_to_SCRIP: calculating grid corners...")
        end if
        lat_type = typeof(lat2d)
        lon_type = typeof(lon2d)
        if(.not.any(lat_type.eq.(/"float","double"/))) then
          lat_type = "float"
        end if
        if(.not.any(lon_type.eq.(/"float","double"/))) then
          lon_type = "float"
        end if
        grid_corner_lat = new( (/ grid_size, grid_corners /), lat_type)
        grid_corner_lon = new( (/ grid_size, grid_corners /), lon_type)
        if(any(fabs(lat2d).eq.90)) then
          if(DEBUG) then
            print("curvilinear_to_SCRIP: one or more lat values are at the")
            print("       poles, so calculating grid corners using")
            print("       calc_SCRIP_corners_boundaries...")
          end if
          if(TESTIT) then
            calc_SCRIP_corners_boundaries_2(lat2d,grid_corner_lat,"lat",Opt)
            calc_SCRIP_corners_boundaries_2(lon2d,grid_corner_lon,"lon",Opt)
          else 
            calc_SCRIP_corners_boundaries(lat2d,lon2d,grid_corner_lat,grid_corner_lon,Opt)
          end if
        else
          if(DEBUG) then
            print("curvilinear_to_SCRIP: no lat values are at the poles, so")
            print("       calculating grid corners using")
            print("       calc_SCRIP_corners_noboundaries...")
          end if
          calc_SCRIP_corners_noboundaries(lat2d,lon2d,grid_corner_lat,grid_corner_lon,Opt)
         if(any(fabs(grid_corner_lat).gt.90)) then
           if(DEBUG) then
             print("curvilinear_to_SCRIP: calc_SCRIP_corners_noboundaries")
             print("       produced out-of-range latitude values.")
             print("       Trying calc_SCRIP_corners_boundaries...")
           end if
            if(TESTIT) then
              calc_SCRIP_corners_boundaries_2(lat2d,grid_corner_lat,"lat",Opt)
              calc_SCRIP_corners_boundaries_2(lon2d,grid_corner_lon,"lon",Opt)
            else 
              calc_SCRIP_corners_boundaries(lat2d,lon2d,grid_corner_lat,grid_corner_lon,Opt)
            end if
          end if
        end if
    end if
   
;---Store the cell corners
    if (isatt(grid_corner_lat,"_FillValue")) then
        delete(grid_corner_lat@_FillValue)
    end if
    if (isatt(grid_corner_lon,"_FillValue")) then
        delete(grid_corner_lon@_FillValue)
    end if
    fid->grid_corner_lat = (/ todouble(grid_corner_lat) /)
    fid->grid_corner_lon = (/ todouble(grid_corner_lon) /)

    if(PrintTimings) then
      print_elapsed_esmf_time(start_time,"curvilinear_to_SCRIP")
    end if
end     ; of curvilinear_to_SCRIP(...)

;======================================================================
; This function accept an array of latitudes and longitudes, creates
; a rectilinear grid based on the given input and then stores it as a
; SCRIP file. This procedure calls curvilinear_to_SCRIP.
;======================================================================
undef("rectilinear_to_SCRIP")
procedure rectilinear_to_SCRIP(FName[1]:string,lat[*]:numeric,\
                               lon[*]:numeric, Opt[1]:logical)
local nlat, nlon, grid_center_lat, grid_center_lon, Opt2, FTitle
begin
   Opt2 = Opt

;---Check for options
    PrintTimings = isatt_logical_true(Opt2,"PrintTimings")
    if(PrintTimings) then
      start_time = get_start_time()
      delete(Opt2@PrintTimings)
    end if

    if(.not.isatt(Opt2,"Title")) then
      Opt2@Title = "rectilinear_to_SCRIP (" + dimsizes(lat) + "," + \
                                              dimsizes(lon) + ")"
    end if

    nlat = dimsizes(lat)
    nlon = dimsizes(lon)
    
;---Conform the lat/lon to 2D arrays
    grid_center_lat = conform_dims((/nlat, nlon/),lat,0)
    grid_center_lon = conform_dims((/nlat, nlon/),lon,1)

;---Generate the mask
    grid_mask_name = get_mask_name(Opt2)
    if(grid_mask_name.eq."") then
      Opt2@GridMask = onedtond(1,(/nlat,nlon/))
    end if
    
;---Convert to SCRIP
    curvilinear_to_SCRIP(FName,grid_center_lat,grid_center_lon,Opt2)

    if(PrintTimings) then
      print_elapsed_esmf_time(start_time,"rectilinear_to_SCRIP")
    end if
end     ; of rectilinear_to_SCRIP

;======================================================================
; This procedure automatically retrieves the grid information from the
; file and converts the grid to a SCRIP file.
;
; The variable must have one of the following:
;   - an attribute called "coordinates" which defines
;      where the coordinate information are stored.
;   - 1D coordinate arrays called "lat" and "lon"
;   - 1D coordinate arrays with units that contain "east" and "north"
;   - attributes with units that contain "east" and "north"
;======================================================================
undef("auto_to_SCRIP")
procedure auto_to_SCRIP(srcFile[1]:string,VarName[1]:string, \
                        SCRIPFName[1]:string,FTitle[1]:string,Opt[1]:logical)
local i, j, fid, nDim, nlat, nlon, VarData, coordType, \
  grid_center_lon, grid_center_lat, CoordDim, LonData, LatData, \
  LonAttName, LatAttName, tmpAtt, LonDimName, LatDimName, tmpunits, \
  tmpdimname, LonVarName, LatVarName, LonVarInd, LatVarInd, \
  ResultIsLatorLon, VarNamesInCoordAtt, nCoords, CoordAtt, NumAtt, attNames, \
  isLonFound, isLatFound
begin
;---Open the file if it exists
    if (.not.isfilepresent(srcFile)) then
        print("auto_to_SCRIP: cannot open '" + srcFile + "'.")
        exit
    else
    end if

    fid = addfile(srcFile,"r")
    
;---Check if the variable is present in the file
    if (.not.isfilevar(fid,VarName)) then
        print("auto_to_SCRIP: the '" + VarName + \
              "' variable doesn't exist on the '" + srcFile + "' file.")
        exit
    end if

    VarData = fid->$VarName$
    
;
; Check if the lat/lon coordinates are defined for the variable.
; The coordinates can be defined using one of the following methods:
;
; coordType = "coordinate":
;     The variable has either:
;      - coordinate arrays called "lat" and "lon".
;      - coordinate arrays whose units attributes contain 
;        "north" and "east"
;
; coordType = "variable":
;     The coordinates are stored in a separate variable, with the 
;     name of those variables defined in the "coordinates"
;     attribute for the variable.
;
; coordType = "attribute"
;     The variable has either:
;      - attributes called "lat2d" and "lon2d"
;      - attributes whose units attributes contain "north" and "east".
;
    coordType = "unknown"
    if( all(iscoord(VarData,(/"lat","lon"/))) ) then
        coordType  = "coordinate"
        LonDimName = "lon"
        LatDimName = "lat"
    end if

;---Check if the coordinate information can be found in the attributes.
    if (coordType.eq."unknown") then
        isLatFound = False
        isLonFound = False        
        attNames   = getvaratts(VarData)
        NumAtt     = dimsizes(attNames)
        do i=0,NumAtt-1
;---Check if the "coordinate" attribute is set
            if( isStrSubset(str_lower(attNames(i)),"coordinate") ) then
                coordType = "variable"
                CoordAtt  = VarData@$attNames(i)$
                nCoords   = str_fields_count(CoordAtt," ")
                if (nCoords.lt.2) then
                    print("auto_to_SCRIP: at least two coordinates must be present")
                    exit
                else
                    VarNamesInCoordAtt = new(nCoords,"string","No_FillValue")
                    do j=0,nCoords-1
                        VarNamesInCoordAtt(j)=str_get_field(CoordAtt,j+1," ")                        
                    end do
                    ResultIsLatorLon = isLatorLon(fid,VarNamesInCoordAtt)
                    LatVarInd = ind(ResultIsLatorLon.eq."latitude")
                    LonVarInd = ind(ResultIsLatorLon.eq."longitude")
                    if (.not.ismissing(LatVarInd)) then
                        isLatFound = True
                        LatVarName = VarNamesInCoordAtt(LatVarInd)
                    end if
                    if (.not.ismissing(LonVarInd)) then
                        isLonFound = True
                        LonVarName = VarNamesInCoordAtt(LonVarInd)
                    end if
                    if(.not.(isLonFound.and.isLatFound)) then
                        print("auto_to_SCRIP: cannot locate lat/lon coordinates for ")
                        print("               variable " + VarName + \
                              " on file " + srcFile)
                        exit
                    end if
                end if
            end if
        end do  ; end of checking if the coordinate information can be found in the attributes.
    end if
        
;---Check if there is any coordinate dimension
    if (coordType.eq."unknown") then
        isLatFound = False
        isLonFound = False  
        nDim = dimsizes(dimsizes(VarData))
        do i=0,nDim-1
            if(isdimnamed(VarData,i)) then
                tmpdimname = VarData!i
                if (iscoord(VarData,tmpdimname)) then
                    if(isatt(VarData&$tmpdimname$,"units")) then
                        tmpunits = VarData&$tmpdimname$@units
                        if(isStrSubset(tmpunits,"north")) then
                            isLatFound = True
                            LatDimName = tmpdimname
                        end if
                        if(isStrSubset(tmpunits,"east")) then
                            isLonFound = True
                            LonDimName = tmpdimname
                        end if
                    end if
                end if
            end if
        end do
        if(isLonFound.and.isLatFound) then
            coordType = "coordinate"
        end if            
    end if      ; end of checking if there is any coordinate dimension
;
; Check if there is any attributes containing the coordinates.
; It won't work if the attributes are stored as a 1D array.
;
    if (coordType.eq."unknown") then
        isLatFound = False
        isLonFound = False  
        do i=0,NumAtt-1
            tmpAtt = VarData@$attNames(i)$
            if(isatt(tmpAtt,"units")) then
                tmpunits = tmpAtt@units
                if(isStrSubset(tmpunits,"north")) then
                    isLatFound = True
                    LatAttName = attNames(i)
                end if
                if(isStrSubset(tmpunits,"east")) then
                    isLonFound = True
                    LonAttName = attNames(i)
                end if                
            end if
        end do
        if(isLonFound.and.isLatFound) then
            coordType = "attribute"
        end if 
    end if      ; end of checking if there is any attributes containing the coordinates.
    if(.not.(isLonFound.and.isLatFound)) then
        print("auto_to_SCRIP: cannot locate lat/lon coordinates for ")
        print("               variable " + VarName + " on file " + srcFile)
        exit
    end if

;---Get the coordinate variable names and coordinate data
    if (coordType.eq."variable") then
        if(isfilevar(fid,LatVarName)) then
            LatData = fid->$LatVarName$
        else
            print("auto_to_SCRIP: can't read latitude information.")
            exit
        end if
        if(isfilevar(fid,LonVarName)) then
            LonData = fid->$LonVarName$
        else
            print("auto_to_SCRIP: can't read longitude information.")
            exit
        end if
    else if (coordType.eq."coordinate") then
        nCoords = dimsizes(VarData)
        LonData = VarData&$LonDimName$
        LatData = VarData&$LatDimName$
    else ; coordType.eq."attribute"
        LonData = VarData@$LonAttName$
        LatData = VarData@$LatAttName$
        if ((dimsizes(dimsizes(LonData)).ne.2).or.\
            (dimsizes(dimsizes(LatData)))) then
            print("auto_to_SCRIP: " + LonAttName + " and " + \
                                      LatAttName + " must be 2D.")
            exit
        end if
    end if
    end if  ; end of getting coordinate variables names and data
            ; at this point lat/lon data is stored in LatData and LonData
        
    if (.not.all(dimsizes(LonData).eq.dimsizes(LatData))) then
        print("auto_to_SCRIP: latitude and longitude must have the same number of dimensions.")
        exit
    end if
    
    CoordDim = dimsizes(dimsizes(LonData))
    if ((CoordDim.eq.1).and.(coordType.eq."coordinate")) then
        nlat = dimsizes(LatData)
        nlon = dimsizes(LonData)
;---Generate the grid centers
    	grid_center_lat = conform_dims((/nlat, nlon/),LatData,0)
        grid_center_lon = conform_dims((/nlat, nlon/),LonData,1)
    else if ((CoordDim.eq.2).and.((coordType.eq."attribute").or. \
                                  (coordType.eq."variable"))) then
        grid_center_lat = LatData
        grid_center_lon = LonData
        delete(LatData)
        delete(LonData)
    else 
        print("auto_to_SCRIP: Can't help with recognizing the coordinates.")
        exit
    end if
    end if
    
;---Get mask
    if(get_mask_name(Opt).eq."") then
      Opt@GridMask = new(dimsizes(grid_center_lat),"integer","No_FillValue")
      Opt@GridMask = 1
    end if
    Opt@Title = FTitle

    curvilinear_to_SCRIP(SCRIPFName,grid_center_lat,grid_center_lon,Opt)
end     ; of auto_to_SCRIP

;======================================================================
; This procedure generates a regularly-spaced lat/lon grid based on 
; the corners of a lat/lon box, or a box center and the width
; and height of the box and the spacing or the number of values.
;
; It was overhauled on Jan 10 2012 to allow for input of a grid type
; rather than inputing nlat and nlon.
;
; GridType can be one of the following types of input:
;
;    "1x1", "2x3", "0.25x0.25", etc
;    "1deg", "0.25deg" "0.25 deg" "0.25" (which means "0.25deg")
;    "G64", "G128"
;
; Recognized attributes for "Opt":
;   LLCorner  - defaults to (-90,-180)
;   URCorner  - defaults to ( 90, 180)
;   Rotation  - rotation angle
;
; It stores the grid in a NetCDF file based on SCRIP standard.
;
; This procedure calls curvilinear_to_SCRIP.
;======================================================================
undef("latlon_to_SCRIP")
procedure latlon_to_SCRIP(FName[1]:string,GridType[1]:string,Opt[1]:logical)
local BoxDiag, Rot, nlat, nlon, grid_type, dlat, dlon,\
      lat, lon, gau_info, grid_center_lat, grid_center_lon, FTitle, \
      PrintTimings, set_pt, orig_pt
begin
  PrintTimings = isatt_logical_true(Opt,"PrintTimings")
  if(PrintTimings) then
    start_time = get_start_time()
  end if

;---Check for "1x1", "2x3", "0.25x0.25" format
  grid_type = check_grid_type(GridType)
  if(grid_type.eq."none") then
    print("latlon_to_SCRIP: invalid format for grid type")
    exit
  end if

  if(grid_type.eq."degree") then
    dlat = grid_type@dlat
    dlon = grid_type@dlon
  else if(grid_type.eq."gaussian") then
    nlat = grid_type@nlat
    nlon = grid_type@nlon
  end if
  end if

;---Check for Opt attributes
  if(Opt.and.isatt(Opt,"Title")) then
    FTitle = Opt@Title
  else
    FTitle = GridType + " grid"
  end if

  if (Opt.and.(isatt(Opt,"Rotation"))) then
      Rot = Opt@Rotation
  else
      Rot = 0.0
  end if

  if(.not.isvar("LLCorner")) then
      if(isatt(Opt,"LLCorner")) then
          LLCorner = Opt@LLCorner
      else
          LLCorner = (/-90,-180/)
      end if
  end if

  if(.not.isvar("URCorner")) then
      if(isatt(Opt,"URCorner")) then
          URCorner = Opt@URCorner
      else
          URCorner = (/90,180/)
      end if
  end if

;---Check the LLCorner and URCorner variables.
  if ( (abs(LLCorner(0)).gt.90).or.(abs(URCorner(0)).gt.90) ) then
      print("latlon_to_SCRIP: lat corners must be within -90 to 90")
      exit
  end if

  BoxDiag = URCorner-LLCorner
  if (Opt.and.(isatt(Opt,"Rotation"))) then
    Rot = Opt@Rotation
    if (Rot.ne.0.0) then
;
; Align the box diagonal to make box sides parallel/perpendicular
; to equator.
;
      rotate_latlon(BoxDiag(0),BoxDiag(1),-1.0*Rot)
    end if
  else
    Rot = 0.0
  end if
        
  if ( BoxDiag(0).le.0.0 ) then
    print("latlon_to_SCRIP: The corner latitudes do not meet the criteria of a bounding box.")
    exit
  end if
  if ( BoxDiag(1).le.0.0 ) then
    print("latlon_to_SCRIP: The corner longitudes do not meet the criteria of a bounding box.")
    exit
  end if
        
  if (Rot.ne.0.0) then
    print("latlon_to_SCRIP: The box is rotated. Spacing may be different than what you meant.")
  end if
  if(grid_type.eq."degree") then
    nlat = tointeger(BoxDiag(0)/dlat+1)
    nlon = tointeger(BoxDiag(1)/dlon+1)

    lat = fspan(0.0,BoxDiag(0),nlat)
    lon = fspan(0.0,BoxDiag(1),nlon)
  else if(grid_type.eq."gaussian") then
    gau_info = gaus(nlat/2)
    lat      = gau_info(:,0)  ; gaussian latitudes
    lon      = todouble((/lonGlobeFo(nlon,"","","")/))
  end if
  end if

;---Generate the cell center Lat/Lon
  grid_center_lat = conform_dims((/nlat, nlon/),lat,0)
  grid_center_lon = conform_dims((/nlat, nlon/),lon,1)
	
  if(get_mask_name(Opt).eq."") then
    Opt@GridMask = onedtond(1,(/nlat,nlon/))
  end if

;---Rotate back the box, if needed.
  if (Rot.ne.(0.0)) then
      rotate_latlon(grid_center_lat,grid_center_lon,Rot)
  end if

;---Now that the rotation was successful translating it back.
  if(grid_type.ne."gaussian") then
    grid_center_lat = grid_center_lat + LLCorner(0)
    grid_center_lon = grid_center_lon + LLCorner(1)
  end if

  Opt@Title = FTitle

;---Make sure curvilinear_to_SCRIP doesn't print timings.
  set_pt = False
  if(isatt(Opt,"PrintTimings")) then
    set_pt = True
    orig_pt = Opt@PrintTimings
    Opt@PrintTimings = False
  end if
  curvilinear_to_SCRIP(FName,grid_center_lat,grid_center_lon,Opt)

  if(set_pt) then
    Opt@PrintTimings = orig_pt
  end if

  if(PrintTimings) then
    print_elapsed_esmf_time(start_time,"latlon_to_SCRIP")
  end if
end     ; of latlon_to_SCRIP(...)

;======================================================================
; Checks if the required dimensions in the SCRIP file are present
;======================================================================
undef("check_SCRIP_dims")
function check_SCRIP_dims(fid[1]:file,verbose[1]:logical)
local i, file_dims, scrip_dim_names
begin
  file_dims       = getvardims(fid) 
  scrip_dim_names = (/"grid_rank","grid_size","grid_corners"/)
  do i=0,dimsizes(scrip_dim_names)-1
    if (.not.(any(file_dims.eq.scrip_dim_names(i))))
      if(verbose) then
        print("check_SCRIP_dims: " + scrip_dim_names(i) + " not defined.")
      end if
      return(False)
    end if
  end do
  return(True)
end     ; of check_SCRIP_dims(...)

;======================================================================
; Checks if the required variables in SCRIP file are present
;======================================================================
undef("check_SCRIP_vars")
function check_SCRIP_vars(fid[1]:file,verbose[1]:logical)
local i, scrip_vars
begin
  scrip_vars = (/"grid_dims",  "grid_center_lat", "grid_center_lon",\
                 "grid_imask", "grid_corner_lat", "grid_corner_lon"/)
  do i=0,dimsizes(scrip_vars)-1
    if(.not.(isfilevar(fid,scrip_vars(i))))
      if(verbose) then
        print("check_SCRIP_vars: " + scrip_vars(i) + " not defined.")
      end if
      return(False)
    end if
  end do
  return(True)
end     ; check_SCRIP_vars(...)

;======================================================================
; Checks if the input filename is following SCRIP standard
;======================================================================
undef("is_SCRIP")
function is_SCRIP(FileName[1]:string,verbose[1]:logical)
local fid
begin
    if (.not.isfilepresent(FileName)) then
      if(verbose) then
        print("is_SCRIP: '" + FileName + "' doesn't exist.")
      end if
      return(False)
    end if
    fid = addfile(FileName,"r")
    return check_SCRIP_dims(fid,verbose).and.check_SCRIP_vars(fid,verbose)

end     ; of is_SCRIP(...)

;======================================================================
; Checks if the required dimensions in the ESMF file are present
;======================================================================
undef("check_ESMF_dims")
function check_ESMF_dims(fid[1]:file,verbose[1]:logical)
local i, esmf_dim_names, file_dims
begin
  file_dims      = getvardims(fid) 
  esmf_dim_names = (/"nodeCount", "elementCount", \
                     "maxNodePElement","coordDim"/)
   
  do i=0,dimsizes(esmf_dim_names)-1
    if (.not.(any(file_dims.eq.esmf_dim_names(i))))
      if(verbose) then
        print("check_ESMF_dims: " + esmf_dim_names(i) + " not defined.")
      end if
      return(False)
    end if
  end do
  return(True)
end     ; of check_ESMF_dims(...)

;======================================================================
; Checks if the required variables in ESMF file are present
;======================================================================
undef("check_ESMF_vars")
function check_ESMF_vars(fid[1]:file,verbose[1]:logical)
local i, esmf_vars
begin
  esmf_vars = (/"nodeCoords","elementConn","numElementConn",\
                "centerCoords","elementArea","elementMask"/)

  do i=0,dimsizes(esmf_vars)-1
    if(.not.(isfilevar(fid,esmf_vars(i))))
      if(verbose) then
        print("check_ESMF_vars: " + esmf_vars(i) + " not defined.")
      end if
      return(False)
    end if
  end do
  return(True)
end     ; check_ESMF_vars(...)

;======================================================================
; Checks if the input filename is following ESMF standard
;======================================================================
undef("is_ESMF")
function is_ESMF(FileName[1]:string,verbose[1]:logical)
local fid
begin
    if (.not.isfilepresent(FileName)) then
      if(verbose) then
        print("is_ESMF: '" + FileName + "' doesn't exist.")
      end if
      return(False)
    end if
    fid = addfile(FileName,"r")
    return check_ESMF_dims(fid,verbose).and.check_ESMF_vars(fid,verbose)

end     ; of is_ESMF(...)

;======================================================================
; This function will retrieve the latitude coordinate of a grid
; from a SCRIP formatted file.
;
; This routine is obsolete in V6.1.0 (not in V6.1.0-beta). It's
; replaced by ESMF_copy_varcoords.
;======================================================================
undef("retrieve_SCRIP_lat")
function retrieve_SCRIP_lat(fileName[1]:string)
local fid, grid_dims, grid_center_lat
begin
  if (.not.isfilepresent(fileName)) then
    print("retrieve_SCRIP_lat: cannot open '" + fileName + "'.") 
    exit
  end if
    
  fid             = addfile(fileName,"r")
  grid_dims       = fid->grid_dims
  grid_center_lat = reshape( fid->grid_center_lat , grid_dims(::-1))
  grid_center_lat@units="degrees_north"

  return(grid_center_lat)
end     ; of retrieve_SCRIP_lat

;======================================================================
; This function will retrieve the longitude coordinate of a grid
; from a SCRIP file.
;
; This routine is obsolete in V6.1.0 (not in V6.1.0-beta). It's
; replaced by ESMF_copy_varcoords.
;======================================================================
undef("retrieve_SCRIP_lon")
function retrieve_SCRIP_lon(fileName[1]:string)
local fid, grid_dims, grid_center_lon
begin
  if (.not.isfilepresent(fileName)) then
    print("retrieve_SCRIP_lon: cannot open '" + fileName + "'.")
    exit
  end if

  fid             = addfile(fileName,"r")
  grid_dims       = fid->grid_dims;
  grid_center_lon = reshape( fid->grid_center_lon , grid_dims(::-1))
  grid_center_lon@units="degrees_east"

  return(grid_center_lon)
end     ; of retrieve_SCRIP_lon

;======================================================================
; This function will retrieve the latitude values from an ESMF 
; formatted file.
;
; This routine is obsolete in V6.1.0 (not in V6.1.0-beta). It's
; replaced by ESMF_copy_varcoords.
;======================================================================
undef("retrieve_ESMF_lat")
function retrieve_ESMF_lat(fileName[1]:string)
local fid, grid_center_lon
begin
  if (.not.isfilepresent(fileName)) then
    print("retrieve_ESMF_lat: cannot open '"+ fileName + "'.")
    exit
  end if
    
  fid              = addfile(fileName,"r")
  grid_center_lat  = fid->centerCoords(:,1)
  grid_center_lat@units="degrees_north"

  return(grid_center_lat)
end     ; of retrieve_ESMF_lat

;======================================================================
; This function will retrieve the longitude values from an ESMF 
; formatted file.
;
; This routine is obsolete in V6.1.0 (not in V6.1.0-beta). It's
; replaced by ESMF_copy_varcoords.
;======================================================================
undef("retrieve_ESMF_lon")
function retrieve_ESMF_lon(fileName[1]:string)
local fid, grid_center_lon
begin
  if (.not.isfilepresent(fileName)) then
      print("retrieve_ESMF_lon: cannot open '" + fileName + "'.")
      exit
  end if

  fid             = addfile(fileName,"r")
  grid_center_lon = fid->centerCoords(:,0)
  grid_center_lon@units="degrees_east"

  return(grid_center_lon)
end     ; of retrieve_ESMF_lon

;======================================================================
; This procedure will retrieve the destination lat/lon values from a
; weights file generated by ESMF_RegridWeightGen and attach them
; to the given variable. Since rectilinear lat/lon grids are written
; as 2D arrays, you can't tell if it's rectilinear or curvilinear,
; unless you test for repetitive values.
; 
; To force this routine to not check for "rectilinear-ness", you
; can set opt@DstGridType to "rectilinear".
;
; There's no standard way to attached unstructured lat/lon data to
; a variable, so we do it by "lat1d" and "lon1d" undef.
;======================================================================
undef("ESMF_copy_varcoords")
procedure ESMF_copy_varcoords(sd:numeric,sd_regrid:numeric,\
                              wgt_file_name[1]:string,opt[1]:logical)
local fwgt, src_dims, src_ranks, dst_dims, dst_rank, mlon, nlat, i, ii, \
      dst_grid_dims, dst_grid_rank, lat, lon, nleftmost, nleftdims, DEBUG
begin
  DEBUG = isatt_logical_true(opt,"Debug")

;---Error checking
  if (.not.isfilepresent(wgt_file_name)) then
    print("Warning: ESMF_copy_varcoords: cannot open '" + wgt_file_name + "'.") 
    print("         No coordinate information will be added.")
    return
  end if

;---Get size of source and destination grids from weight file
  fwgt          = addfile(wgt_file_name, "r")
  src_grid_dims = fwgt->src_grid_dims(::-1)    ; dimensions are Fortran based
  dst_grid_dims = fwgt->dst_grid_dims(::-1)
  src_grid_rank = dimsizes(src_grid_dims)
  dst_grid_rank = dimsizes(dst_grid_dims)

;---More error checking
  if(dst_grid_rank.ne.1.and.dst_grid_rank.ne.2) then
    print("Warning: ESMF_copy_varcoords: the rank of the lat/lon values")
    print("         on the weights file must be 1 or 2.")
    print("         No coordinate information will be added.")
    return
  end if

  if(isatt(opt,"DstGridType")) then
    if(check_grid_type(opt@DstGridType).eq."none") then
      print("Warning: ESMF_copy_varcoords: don't recognize the grid type")
      print("         specified: '" + opt@DstGridType + "'.")
      print("         No coordinate information will be added.")
      return
    end if
    if(opt@DstGridType.eq."unstructured".and.dst_grid_rank.ne.1) then
      print("Warning: ESMF_copy_varcoords: you specified an unstructured")
      print("         grid, but your lat/lon coordinates appear to be") 
      print("         curvilinear or rectilinear.")
      print("         No coordinate information will be added.")
      return
    end if
    if(any(opt@DstGridType.eq.(/"curvilinear","rectilinear"/)).and.\
       dst_grid_rank.ne.2) then
      print("Warning: ESMF_copy_varcoords: you specified a rectilinear")
      print("         or curvilinear grid, but your lat/lon coordinates")
      print("         appear to be unstructured.")
      print("         No coordinate information will be added.")
      return
    end if
  end if

;---Get size of original and regridded variables
  src_dims = dimsizes(sd)
  src_rank = dimsizes(src_dims)
  dst_dims = dimsizes(sd_regrid)
  dst_rank = dimsizes(dst_dims)

;---Both grids must have the same leftmost dimemsions
  if((src_rank-src_grid_rank).ne.(dst_rank-dst_grid_rank)) then
    print("Warning: ESMF_copy_varcoords: the dimension sizes of your")
    print("         source and destination grids don't match up.")
    print("         No coordinate information will be added.")
    return
  end if
;---Make sure leftmost dimensions are same size.
  nleftdims = src_rank-src_grid_rank
  do i=0,nleftdims-1
    if(src_dims(i).ne.dst_dims(i)) then
      print("Warning: ESMF_copy_varcoords: the dimension sizes of your")
      print("         source and destination grids don't match up.")
      print("         No coordinate information will be added.")
      return
    end if
  end do

;---Done with error checking. Copy the leftmost coordinates, if any.
  if(nleftdims.gt.0) then
    ii = ispan(0,nleftdims-1,1)
    copy_VarCoords_n(sd,sd_regrid,ii)
  end if
;
; Test for what kind of coordinate information to add.
; 
  if (dst_grid_rank.eq.1) then
    sd_regrid@lat1d = fwgt->yc_b      ; I don't like this, but how else to 
    sd_regrid@lon1d = fwgt->xc_b      ; attach?
    return
  end if

;---dst_grid_rank must be 2
  nlat = dst_grid_dims(0)
  mlon = dst_grid_dims(1)

  if(isatt(opt,"DstGridType").and.opt@DstGridType.eq."curvilinear") then
    sd_regrid@lat2d = reshape(fwgt->yc_b,(/nlat,mlon/))
    sd_regrid@lon2d = reshape(fwgt->xc_b,(/nlat,mlon/))
    return
  end if
;
; If DstGridType not specified, then we have to check if these are
; rectilinear coordinates. This can be an expensive operation.
; I'm not sure if this should be the default.
;
  is_rectilinear = isatt(opt,"DstGridType").and.\
                   opt@DstGridType.eq."rectilinear"

  lat = fwgt->yc_b(0::mlon)
  lon = fwgt->xc_b(0:mlon-1)

  if(.not.is_rectilinear) then
     tmplat    = fwgt->yc_b(nlat/2::mlon)
     tmplon    = fwgt->xc_b(mlon:2*mlon-1)
     nlat_test = dimsizes(lat)
     nlon_test = dimsizes(lon)
     nlat_tmp  = dimsizes(tmplat)
     nlon_tmp  = dimsizes(tmplon)
;---Test for "rectilinear-ness"
     if (nlat_test.eq.nlat_tmp.and.nlon_test.eq.nlon_tmp.and.\
         all(lat.eq.tmplat).and.all(lon.eq.tmplon)) then
       if(DEBUG) then
         print("ESMF_copy_varcoords: detected a rectilinear grid.")
         dq = str_get_dq()
         print("      If you know your grid is rectilinear, then set")
         print("      opt@DstGridType = " + dq + "rectilinear" + dq + " to")
         print("      prevent this potentially slow test.")
       end if
       is_rectilinear = True
     else
       sd_regrid@lat2d = reshape(fwgt->yc_b,(/nlat,mlon/))
       sd_regrid@lon2d = reshape(fwgt->xc_b,(/nlat,mlon/))
       return
     end if
     delete([/tmplat,tmplon/])
  end if

;---Must be rectilinear!
  lat@long_name = "latitude"
  lat@units     = "degrees_north"
  lat!0         = "lat"
  lat&lat       =  lat

  lon  = fwgt->xc_b(0:mlon-1)
  lon@long_name = "longitude"
  lon@units     = "degrees_east"
  lon!0         = "lon"
  lon&lon       =  lon
    
  dst_dims = dimsizes(sd_regrid)
  dst_rank = dimsizes(dst_dims)
  sd_regrid!(dst_rank-2) = "lat"
  sd_regrid!(dst_rank-1) = "lon"
  sd_regrid&lat         = lat
  sd_regrid&lon         = lon
  return
end


;======================================================================
; This procedure copies attributes from the original variable to
; the regridded variable, and attaches coordinate information, if
; available. It will not copy lat1d/lon1d or lat2d/lon2d attributes.
;
; The coordinate information is retrieved from the weights file.
;
; I think this procedure needs to have added to it the ability to
; have a "coordinates" attribute for the case of curvilinear grids.
;======================================================================
undef("ESMF_copy_varmeta")
procedure ESMF_copy_varmeta(sd,sd_regrid,wgt_file_name[1]:string, \
                            opt[1]:logical)
local var_dims, var_rank, dstlat, dstlon, atts_to_skip, natts
begin
  var_dims = dimsizes(sd_regrid)
  var_rank = dimsizes(var_dims)

;---Copy variable attributes, unless requested otherwise (True by default)
  if(.not.isatt_logical_false(opt,"CopyVarAtts").and.\
     .not.isatt_logical_false(opt,"CopyVarMeta")) then
    atts_to_skip = (/"lat1d","lon1d","lat2d","lon2d","coordinates"/)
    if(isatt(opt,"CopyVarAttsExcept")) then
      natts            = dimsizes(atts_to_skip)
      new_atts_to_skip = new(natts+dimsizes(opt@CopyVarAttsExcept),string)
      new_atts_to_skip(0:natts-1) = atts_to_skip      
      new_atts_to_skip(natts:)    = opt@CopyVarAttsExcept
      copy_VarAtts_except(sd,sd_regrid,new_atts_to_skip)
    else
      copy_VarAtts_except(sd,sd_regrid,atts_to_skip)
    end if
    if(.not.any("_FillValue".eq.atts_to_skip).and.isatt(sd,"_FillValue").and. \
       .not.isatt(sd_regrid,"_FillValue")) then
      sd_regrid@_FillValue = sd@_FillValue
    end if
  end if

;---Create variable coords, unless requested otherwise (True by default)
  if(.not.isatt_logical_false(opt,"CopyVarCoords").and.\
     .not.isatt_logical_false(opt,"CopyVarMeta")) then
    ESMF_copy_varcoords(sd,sd_regrid,wgt_file_name,opt)
  end if
end

;======================================================================
; This procedure calls the UNIX command "ESMF_RegridWeightGen".
;
;  srcGridFile - the name of the NetCDF source grid file (ESMF or SCRIP)
;  dstGridFile - the name of the NetCDF destination grid file (ESMF or SCRIP)
;  wgtFile     - the name of the NetCDF weight file to create.
;  opt         - Set to True to indicate optional attributes are attached.
;
;  The following attributes are allowed with "opt":
;        Debug - Turns on debug print statements
;           True or False (default=False)
;        SrcESMF - whether only the src grid file is an ESMF file
;           True or False (default=False)
;        DstESMF - whether only the dst grid file is an ESMF file
;           True or False (default=False)
;        SrcRegional - whether only the src grid file is regional
;           True or False (default=False)
;        DstRegional - whether only the dst grid file is regional
;           True or False (default=False)
;        IgnoreUnmappedPoints - whether to ignore unmapped points
;           True or False (default=True)
;        InterpMethod ("method" will work too)
;           "bilinear" (default), "patch" (slow), "conserve"
;        Pole ("pole" will work too)
;           "all" (default, unless InterpMethod=conserve), "none"
;        RemoveSrcFile
;           True or False (default=False)
;           Whether to remove srcGridFile after the
;             weight generation is done.
;        RemoveDstFile
;           True or False (default=False)
;           Whether to remove dstGridFile after the
;             weight generation is done.
;        RemoveFiles
;           True or False (default=False)
;           Whether to remove both src and dst grid files
;             after the weight generation is done.
;
; The ESMFBINDIR environment variable can be set  prior to calling 
; this procedure, in order to indicate the location of this binary.
;
; The NumProc env var can be set to indicate the number of
; processors, if the ESMF software was built with MPI turned on.
;
;======================================================================
undef("ESMF_regrid_gen_weights")
procedure ESMF_regrid_gen_weights(srcGridFile[1]:string, dstGridFile[1]:string, \
                           wgtFile[1]:string, opt[1]:logical)
local DEBUG, NumProc, Executer, ESMFCMD, ESMF_exec, interp_method, pole, opt, \
      path_to_ESMF, ESMF_exec_exists, SRC_LOC, DST_LOC
begin
  valid_interp_methods = (/"bilinear","patch","conserve", \
                           "nearestdtos","neareststod"/)
;
; Some attribute options cannot both be set if they have
; different values.  Check for them here.
;
  opt2 = opt
  check_both_atts(opt2,"RemoveFiles","RemoveDstFile",False)
  check_both_atts(opt2,"RemoveFiles","RemoveSrcFile",False)
  check_both_atts(opt2,"WgtForceOverwrite","ForceOverwrite",False)
  check_both_atts(opt2,"WgtOverwrite","Overwrite",False)
  check_both_atts(opt2,"WgtNetCDFType","NetCDFType","netcdf3")

  if(isatt_logical_true(opt2,"RemoveDstFile"))
    print("ESMF_regrid_gen_weights: Warning: RemoveDstFile was set to True.")
    print("  This file might be needed later to generate coordinates.")
  end if

  if(isatt(opt2,"WgtNetCDFType")) then
    opt2@NetCDFType = opt2@WgtNetCDFType
  end if

  if(isatt_logical_true(opt2,"WgtOverwrite")) then
    opt2@Overwrite = True
  end if

  if(isatt_logical_true(opt2,"WgtForceOverwrite")) then
    opt2@ForceOverwrite = True
  end if

;---Name of ESMF offline regridder
  ESMF_exec = "ESMF_RegridWeightGen"

;---Check ESMFBINDIR
  if (ismissing(getenv("ESMFBINDIR"))) then
    path_to_ESMF = ESMF_exec
  else
    path_to_ESMF = getenv("ESMFBINDIR") + "/" + ESMF_exec
  end if

;---Check if ESMF_exec exists and can be executed
  ESMF_exec_exists = systemfunc(path_to_ESMF + " --help >/dev/null 2>&1 && echo True || echo False")
  if (ESMF_exec_exists.eq."False") then
    print("ESMF_regrid_gen_weights: could not find " + ESMF_exec + " executable.")
    exit
  end if

;---Check ESMF_RegridWeightGen version.
  ESMF_MAJOR_VERSION = toint(systemfunc(path_to_ESMF + " --version | grep MAJOR | awk '{print $2}'"))

;---Check for options
  PrintTimings   = isatt_logical_true(opt2,"PrintTimings")
  if(PrintTimings) then
    start_time = get_start_time()
  end if

  DEBUG          = isatt_logical_true(opt2,"Debug")
  CHECK          = isatt_logical_true(opt2,"Check")
  REMOVE_PET_LOG = isatt_logical_true(opt2,"RemovePETLog")

;---Check if the source grid file is a SCRIP file.
  if (.not.(isatt_logical_true(opt2,"SrcESMF")).and. \
      .not.(is_SCRIP(srcGridFile,True))) then
    print("ESMF_regrid_gen_weights: invalid or missing SCRIP source grid file.")
    exit
  end if

;---Check if the source grid file is an ESMF file.
  SRC_LOC = ""
  if (isatt_logical_true(opt2,"SrcESMF")) then
    if (is_ESMF(srcGridFile,True)) then
      if (ESMF_MAJOR_VERSION.ge.7) then
        ;---Necessary flag for ESMF >=7.0.0
        SRC_LOC = "--src_loc corner "
      end if
    else
      print("ESMF_regrid_gen_weights: invalid or missing ESMF source grid file.")
      exit
    end if
  end if

;---Check if the destination grid file is a SCRIP file.
  if (.not.(isatt_logical_true(opt2,"DstESMF")).and. \
      .not.(is_SCRIP(dstGridFile,True))) then
    print("ESMF_regrid_gen_weights: invalid or missing SCRIP destination grid file.")
    exit
  end if

;---Check if the destination grid file is a ESMF file.
  DST_LOC = ""
  if (isatt_logical_true(opt2,"DstESMF")) then
    if (is_ESMF(dstGridFile,True)) then
      if (ESMF_MAJOR_VERSION.ge.7) then
        ;---Necessary flag for ESMF >=7.0.0
        DST_LOC = "--dst_loc corner "
      end if
    else
      print("ESMF_regrid_gen_weights: invalid or missing ESMF destination grid file.")
      exit
    end if
  end if

;
; All seems ok. Run the ESMF regrid weight generator.
;
; Check the number of processor supported by the system.
;
; The user must have compiled ESMF with parallel features.
;
  if(ismissing(getenv("NumProc"))) then  
    NumProc  = 1
    Executer = ""
  else 
    NumProc = getenv("NumProc")
    Executer = "mpirun -n " +NumProc+" "
  end if
  if(DEBUG) then
    print("ESMF_regrid_gen_weights: number of processors used: " + NumProc)
  end if

;---Check if the weight file already exists.
  check_for_file(wgtFile,opt2)

;---Add necessary --ignore_degenerate flag for ESMF >=7.0.0
  if(ESMF_MAJOR_VERSION.ge.7) then
    ESMF_IGNORE_DEGENERATE = " --ignore_degenerate "
  else
    ESMF_IGNORE_DEGENERATE = ""
  end if

;---Build the command (First Pass - non-optional features)
  ESMFCMD = Executer + path_to_ESMF \
            + ESMF_IGNORE_DEGENERATE \
            + SRC_LOC + DST_LOC \
            + " --source " + srcGridFile \
            + " --destination " + dstGridFile \
            + " --weight "+ wgtFile

;---Check for options
  if (opt2) then
;
; Check if the user has requested a certain method.
; Otherwise, the default will be used.
;
    if (isatt(opt2,"InterpMethod")) then
      interp_method = str_lower(opt2@InterpMethod)
    end if
    if (isatt(opt2,"method")) then
      interp_method = str_lower(opt2@method)
    end if
    if (isvar("interp_method")) then
      if(.not.any(interp_method.eq.valid_interp_methods)) then
        print("ESMF_regrid_gen_weights: InterpMethod must be one of " + \
              str_join(valid_interp_methods,", "))
        exit
      end if
      ESMFCMD = ESMFCMD + " --method " + interp_method
    end if
;
; Check if the user has specified how to handle the poles
; Otherwise, the default will be used.
;
    if (isatt(opt2,"Pole")) then
      pole = str_lower(opt2@Pole)
    end if
    if (isatt(opt2,"pole")) then
      pole = str_lower(opt2@pole)
    end if
    if(isvar("interp_method").and.isvar("pole").and.pole.eq."all".and.\
       any(interp_method.eq.(/"conserve","neareststod","nearestdtos"/))) then
      print("ESMF_regrid_gen_weights: warning: 'Pole' cannot be set to 'all' when using") 
      print("              'conserve' or 'nearest neighbor' methods. Defaulting to 'none'.")
      pole = "none"      
    end if

    if (isvar("pole")) then
      ESMFCMD = ESMFCMD + " --pole " + pole
    end if

;---Check if the user specifies that source grid is regional
    if (isatt_logical_true(opt2,"SrcRegional")) then
      ESMFCMD = ESMFCMD + " --src_regional"
    end if

;---Check if the user specifies that destination grid is regional
    if (isatt_logical_true(opt2,"DstRegional")) then
      ESMFCMD = ESMFCMD + " --dst_regional"
    end if

;---Checking if the user specifies that source grid is ESMF
    if (isatt_logical_true(opt2,"SrcESMF")) then
        ESMFCMD = ESMFCMD + " --src_type ESMF"
    end if

;---Check if the user specifies that destination grid is ESMF
    if (isatt_logical_true(opt2,"DstESMF")) then
      ESMFCMD = ESMFCMD + " --dst_type ESMF"
    end if

;---This flag is necessary for V5.2.0r1 and grids like WRF.
    if (.not.isatt_logical_false(opt2,"IgnoreUnmappedPoints")) then
        ESMFCMD = ESMFCMD + " -i"
    end if

;---This flag is necessary for writing vars > 2 GB
    if (isatt_logical_true(opt2,"LargeFile")) then
        ESMFCMD = ESMFCMD + " --64bit_offset"
    end if

;---This flag is necessary for writing vars > 4 GB
    if (isatt(opt2,"NetCDFType").and.\
        str_lower(opt2@NetCDFType).eq."netcdf4") then
        ESMFCMD = ESMFCMD + " --netcdf4"
    end if
;
; Must normalize weights on destinatioin grid if using "conserve"
; This was added in V6.2.1. in V6.2.0 or earlier, the weights were
; normalized *after* the weights file was created
; (see ESMF_regrid_with_weights).
;
    if((isvar("interp_method").and.interp_method.eq."conserve".and.\
        .not.isatt_logical_false(opt2,"NormalizeWeights")).or.\
        isatt_logical_true(opt2,"NormalizeWeights")) then
      ESMFCMD = ESMFCMD + " --norm_type fracarea"
    end if

;---Check if the user has requested to print out the full command to be run.
  end if  ; end of building the command (Second Pass - optional features)
		
  if (isatt_logical_true(opt2,"NoPETLog")) then
      ESMFCMD = ESMFCMD + " --no_log"
  end if

  if (CHECK) then
      ESMFCMD = ESMFCMD + " --check"
  end if

  if (DEBUG)
    print("--------------------------------------------------")
    print("ESMF_regrid_gen_weights: the following command is about to be executed on the system:")
    print("'"+ESMFCMD+"'")
    print("--------------------------------------------------")
  end if

;---Execute the regridding command
  RegridOut = systemfunc(ESMFCMD)
        
;
; This removes the PET log file. You can set NoPETLog=True to
; have it not generated at all.
;
  if(REMOVE_PET_LOG) then
    system("rm -rf PET0.RegridWeightGen.Log")
  end if

  if(DEBUG.or.CHECK) then
    print("ESMF_regrid_gen_weights: output from '" + ESMF_exec + "':")
    print("     " + RegridOut)
    print("--------------------------------------------------")
  end if
;
; The binary tool performs more sophisticated tests.
; Here it is checked that everything went ok.
;
  if (.not.(any(str_squeeze(RegridOut).eq. \
            "Completed weight generation successfully."))) then
    print("ESMF_regrid_gen_weights: '" + ESMF_exec + "' was not successful.")
    exit
  end if
  if(DEBUG) then
    print("ESMF_regrid_gen_weights: '" + ESMF_exec + "' was successful.")
  end if    

;
; Clean up, if desired. We don't allow for the removal of the weight
; file here because it might be needed later to generate coordinate
; information. We might decide to change this.
;
  if (isatt_logical_true(opt2,"RemoveFiles").or. \
      isatt_logical_true(opt2,"RemoveSrcFile")) then
    if(DEBUG) then
      print("ESMF_regrid_gen_weights: By request, removing '" + srcGridFile + "'")
    end if    
    system("rm -rf " + srcGridFile)
  end if

  if (isatt_logical_true(opt2,"RemoveFiles").or. \
      isatt_logical_true(opt2,"RemoveDstFile")) then
    if(DEBUG) then
      print("ESMF_regrid_gen_weights: By request, removing '" + dstGridFile + "'")
    end if    
    system("rm -rf " + dstGridFile)
  end if

  if(PrintTimings) then
    print_elapsed_esmf_time(start_time,"ESMF_regrid_gen_weights")
  end if

end     ; of ESMF_regrid_gen_weights(...)

;======================================================================
; Using the provided weight file, remaps the data 
; from source grid into destination grid
;
; I've been tempted to add options for removing the source, destination,
; and weight files after this routine is done. But, ESMF_regrid can
; do this, so it's easier to let this routine "do it all" than to
; have the individual functions do it.
;======================================================================
undef("ESMF_regrid_with_weights")
function ESMF_regrid_with_weights(srcData:numeric,wgtFile[1]:string, opt[1]:logical)
local DEBUG, fmsg, fid, srclat, srclon, dstlat, dstlon, src_grid_dims, \
      src_grid_rank, srcData_dims, srcData_rank, srcData_rgt_dims, \
      srcData_lft_dims, has_leftmost_dims, dst_grid_dims, dst_grid_rank, \
      dst_grid_dims_in, row, col, v, x, minDataValue, maxDataValue, \
      RemapIndexes, Indexes, IndexesDims, dstData_tmp2d
begin
  fmsg  = new(1,float)                         ; For error return

;---Check for options
  PrintTimings = isatt_logical_true(opt,"PrintTimings")
  DEBUG        = isatt_logical_true(opt,"Debug")
;
; If the input type is float, and the user hasn't explicitly set
; ReturnDouble to True, then floats will be returned.
;
  if(typeof(srcData).ne."double".and.\
     .not.isatt_logical_true(opt,"ReturnDouble")) then
    ReturnFloat = True
  else
    ReturnFloat = False
  end if

  if(PrintTimings) then
    start_time = get_start_time()
  end if

;
; Check if the indexes on the file actually represent indexes
; into a larger 2D grid. If so, we have get the Indexes and
; the dimension sizes of the larger 2D grid, for remapping
; later.
;
  RemapIndexes = isatt_logical_true(opt,"RemapIndexes")
  if(RemapIndexes) then
    if(isatt(opt,"Indexes")) then
      Indexes = opt@Indexes
    else
      print("ESMF_regrid_with_weights: error: you set RemapIndexes=True, but didn't provide 'Indexes'.")
      exit
    end if
    if(isatt(opt,"IndexesDims")) then
      IndexesDims = opt@IndexesDims
    else
      print("ESMF_regrid_with_weights: error: you set RemapIndexes=True, but didn't provide the ")
      print("                          dimension sizes of the nD array (IndexesDims")
      exit
    end if
  end if

  if(DEBUG) then
    print("ESMF_regrid_with_weights: regridding using interpolation weights ...")
  end if

;---Make sure the weight file is accessible
  map_method = "unknown"
  if (.not.(isfilepresent(wgtFile))) then
    print("ESMF_regrid_with_weights: cannot open weight file '" + \
          wgtFile + "'.")
    return(fmsg)
  else
    fid = addfile(wgtFile,"r")
    if(isatt(fid,"map_method")) then
      map_method = fid@map_method
    end if
;---Error checking
    ns = getfilevardimsizes(fid,"S")
    if(all(ismissing(ns)).or.all(ns.eq.0)) then
      print("ESMF_regrid_with_weights: error: there are no weights on this file.")
      print("    This likely means there was a problem with the mapping")
      print("    from the source grid to the destination grid.")
      print("    Check that both grids are valid.")
      return(fmsg)
    end if
  end if

;
; Apply frac_b only for conservative interpolation. We may
; change this later to let the user decide when to apply.
; The ESMF folks said that it was only needed for the
; "conserve" case, and they reconfirmed this Oct 2012.
;
  if(map_method.eq."Conservative remapping") then
    ApplyFrac_B = isatt_logical_true(opt,"WgtApplyFracB")
  else
    ApplyFrac_B = False
  end if

  if(ApplyFrac_B) then
    frac_b = fid->frac_b
    frac_b@_FillValue = default_fillvalue(typeof(frac_b))
    frac_b = where(frac_b.eq.0,frac_b@_FillValue,frac_b)
  end if

;---Get the lat/lon of source and destination grids
  srclat = fid->yc_a
  srclon = fid->xc_a
  dstlat = fid->yc_b
  dstlon = fid->xc_b
	
;---Check that the source grid "covers" the destination grid 
  if ( DEBUG.and..not.( \
       (min(dstlat).ge.min(srclat)).and.  \
       (max(dstlat).le.max(srclat)).and.  \
       (min(dstlon).ge.min(srclon)).and.  \
       (max(dstlon).le.max(srclon))   ) ) then
      print("ESMF_regrid_with_weights: warning: destination grid is not")
      print("    completely covered by the source grid. This is not an error.")
      print("    It just means your destination grid covers a larger area")
      print("    than your source grid.")
  end if
	
;---Get the dimensions and rank of the source grid and input grid.
  src_grid_dims = fid->src_grid_dims(::-1)    ; dimensions are Fortran-based
  src_grid_rank = dimsizes(src_grid_dims)
  is_src_grid_scalar = src_grid_rank.eq.1.and.src_grid_dims(0).eq.1

  srcData_dims = dimsizes(srcData)
  srcData_rank = dimsizes(srcData_dims)

;---Get the dimensions and rank of the destination grid
  dst_grid_dims = fid->dst_grid_dims(::-1)    ; dimensions are Fortran based
  dst_grid_rank = dimsizes(dst_grid_dims)	

;---Check dimensions of srcData
  if(src_grid_rank.gt.srcData_rank) then
    print("ESMF_regrid_with_weights: error: the rank of the source dimensions on the")
    print("                          weight file is more than the rank of the data")
    print("                          to be regridded. Can't continue.")
    return(fmsg)
  end if

  if(DEBUG) then
;---Source grid info
    print("ESMF_regrid_with_weights: Source Grid:")
    print("                          rank:      " + src_grid_rank)
    str = "                          dimensions:"
    do i=0,src_grid_rank-1
      str = str + " " + src_grid_dims(i)
    end do
    print("" + str)
    print("                          original source rank: " + srcData_rank)
    print("                          latitude min/max: " + min(srclat) + "/" + max(srclat))
    print("                          longitude min/max:" + min(srclon) + "/" + max(srclon))
;---Destination grid info
    print("ESMF_regrid_with_weights: Destination Grid:")
    str = "                          dimensions:"
    do i=0,dst_grid_rank-1
      str = str + " " + dst_grid_dims(i)
    end do
    print("" + str)
    print("                          latitude min/max: " + min(dstlat) + "/" + max(dstlat))
    print("                          longitude min/max:" + min(dstlon) + "/" + max(dstlon))
  end if

;
; The source dimensions on the description file will not 
; necessarily be the same as the original source dimensions
; of the grid you want to regrid from. You have to take
; that into account in this whole procedure, and make sure
; you use the correct dimensions depending on what you're
; doing.
;
  if(.not.is_src_grid_scalar.and.srcData_rank.gt.src_grid_rank) then
    srcData_lft_ndims = srcData_rank - src_grid_rank
    srcData_rgt_dims  = srcData_dims(srcData_lft_ndims:)
    srcData_lft_dims  = srcData_dims(0:srcData_lft_ndims-1)
  else
    srcData_rgt_dims  = srcData_dims
    srcData_lft_ndims = 0
  end if

;
; Retrieve the interpolation weights. They are stored as a 
; sparse matrix in a coordinate list format
;
  if(DEBUG) then
    print("ESMF_regrid_with_weights: retrieving interpolation weights ...")
  end if

  row = fid->row
  col = fid->col
  v   = fid->S
  if(isatt(fid,"ESMF_regrid_method")) then
    esmf_regrid_method = fid@ESMF_regrid_method
  else
    esmf_regrid_method = ""
  end if

;---Check that the column indexes will map into src_grid_dims
;;  if ( max(col).gt.product(srcData_rgt_dims) ) then
;;    print("ESMF_regrid_with_weights: error: source data on the description")
;;    print("     file does not have proper dimensions.")
;;    return(fmsg)
;;  end if
    
;
; We have to convert the src grid to 1D no matter what.
; However, we have to determine if there are any leftmost 
; dimensions to account for.
;
  if(.not.is_src_grid_scalar.and.srcData_rank.gt.src_grid_rank) then
    src_grid_dims_tmp                        = new(srcData_lft_ndims+1,long)
    src_grid_dims_tmp(0:srcData_lft_ndims-1) = srcData_lft_dims
    src_grid_dims_tmp(srcData_lft_ndims)     = product(srcData_rgt_dims)
    x = reshape(srcData,src_grid_dims_tmp)
  else
    x = reshape(srcData,product(srcData_dims))
  end if
;
; Note from Mohammad:
;   Currently there is a problem with ESMF files.
;   once it is fixed by ESMF people This can be reinstated.
;  if( max(row).gt.product(dst_grid_dims) ) then  
;    print("ESMF_regrid_with_weights: error: weight file has internal mistmatched data or is corrupted.")
;    return(fmsg)
;  end if

; 01 Sep 2014: I don't think the above is an issue any more,
; so will not use "max(row)" below.
;
  if(DEBUG)
    print("ESMF_regrid_with_weights: calling sparse_matrix_mult to apply weights...")
  end if

;
; We have to always treat the output grid as a 1D array
; of length N because ESMF gives us the indices into a
; 1D array. 
;
;; NOTE: 01 Sep 2014: removed the N=max(row) below, because
;; it causes a problem if you have a destination mask and
;; a curvilinear or unstructured grid. Don't think this
;; has been needed for awhile.
;;
;; If dst_grid_dims is 1, then we have to set N=max(row).
;;
;;  if(dst_grid_rank.eq.1) then
;;    dst_grid_dims_in = max(row)
;;  else
;;    dst_grid_dims_in = product(dst_grid_dims)
;;  end if

  dst_grid_dims_in = product(dst_grid_dims)

;
; We will need to reshape the return (M) x N grid from
; sparse_matrix_mult if the output dimensions are not 1D.
;
; Pre V6.2.1, we also needed to divide by frac_b if using
; the "conserve" method. Post V6.2.0, we can use the 
; "--norm_type fracarea" option when generating the weights.
; So, frac_b stuff only applies if the user sets the
; *unadvertised* WgtApplyFracB.
;
; All the "if" tests below are to avoid unnecessary copying
; of the return array.
;
; Return array has to be reshaped and converted to float
;   Case 1: Apply frac_b
;   Case 2: Don't apply frac_b
; Return array has to be reshaped, but leave as double
;   Case 3: Apply frac_b
;   Case 4: Don't apply frac_b
; Return array has to be converted to float (no reshaping)
;   Case 5: Apply frac_b
;   Case 6: Don't apply frac_b
; Return array left as double (no reshaping)
;   Case 7: Apply frac_b, convert to float
;   Case 8: Convert to float
;
  if(ApplyFrac_B.and.DEBUG) then 
    print("ESMF_regrid_with_weights: dividing results by frac_b...")
    print("            (currently for conserve interpolation only)")
  end if

  if(dst_grid_rank.gt.1) then
    dst_grid_dims_out = new(srcData_lft_ndims+dst_grid_rank,long)
    if(.not.is_src_grid_scalar.and.srcData_rank.gt.src_grid_rank) then
      dst_grid_dims_out(0:srcData_lft_ndims-1) = srcData_lft_dims
    end if
    dst_grid_dims_out(srcData_lft_ndims:) = dst_grid_dims
    if(ReturnFloat) then
;---Case 1: Apply frac_b, reshape, convert to float
      if(ApplyFrac_B) then
        dstData = tofloat(reshape(sparse_matrix_mult(row-1,col-1,v,x,\
                  dst_grid_dims_in),dst_grid_dims_out) / \
                  reshape(frac_b,dst_grid_dims_out))
      else
;---Case 2: Reshape, convert to float
        dstData = tofloat(reshape(sparse_matrix_mult(row-1,col-1,v,x,\
                  dst_grid_dims_in),dst_grid_dims_out))
      end if
    else
;---Case 3: Apply frac_b, reshape, leave as double
      if(ApplyFrac_B) then
        dstData = reshape(sparse_matrix_mult(row-1,col-1,v,x,\
                  dst_grid_dims_in),dst_grid_dims_out) / \
                  reshape(frac_b,dst_grid_dims_out)
      else
;---Case 4: Reshape, leave as double
        dstData = reshape(sparse_matrix_mult(row-1,col-1,v,x,\
                  dst_grid_dims_in),dst_grid_dims_out)
      end if
    end if
  else
    if(ReturnFloat) then
;---Case 5: Apply frac_b, convert to float (no reshape)
      if(ApplyFrac_B) then
        dstData = tofloat(sparse_matrix_mult(row-1,col-1,v,x,\
                  dst_grid_dims_in)/frac_b)
      else
;---Case 6: Convert to float (no reshape)
        dstData = tofloat(sparse_matrix_mult(row-1,col-1,v,x,dst_grid_dims_in))
      end if
    else
;---Case 7: Apply frac_b, leave as double (no reshape)
      if(ApplyFrac_B) then
        dstData = sparse_matrix_mult(row-1,col-1,v,x,dst_grid_dims_in)/frac_b
      else
;---Case 8: Leave as double (no reshape)
        dstData = sparse_matrix_mult(row-1,col-1,v,x,dst_grid_dims_in)
      end if
    end if
  end if

;---Clean up to save memory
  if(ApplyFrac_B) then
    delete(frac_b)
  end if

;---Make sure dstData has a missing value
  if(isatt(srcData,"_FillValue")) then
    dstData@_FillValue = totypeof(srcData@_FillValue,typeof(dstData))
  else
    dstData@_FillValue = default_fillvalue(typeof(dstData))
  end if

;---Check if we need to remap the values onto a larger 2D grid.
  if(RemapIndexes) then
    if(DEBUG)
      print("ESMF_regrid_with_weights: putting interpolated values back onto larger 2D grid...")
    end if
    dstData_tmp2d = reshape_ind(dstData,Indexes,IndexesDims)
    delete(dstData)
    dstData = dstData_tmp2d
  end if

  if(DEBUG) then
    print("ESMF_regrid_with_weights: dstData")
    str = "                          Dimensions:"
    nd = dimsizes(dstData)
    do i=0,dimsizes(nd)-1
      str = str + " " + nd(i)
    end do
    print("" + str)
    print("                          minSrcData: " + min(srcData))
    print("                          maxSrcData: " + max(srcData))
    print("                          minDstData: " + min(dstData))
    print("                          maxDstData: " + max(dstData))
  end if

;---Copy variable attributes and coordinate arrays if requested.
  delete(fid)    ; close weight file before it gets opened again
  ESMF_copy_varmeta(srcData,dstData,wgtFile,opt)

;----------------------------------------------------------------------
; Clean up, add, and remove attributes.
;
; Note that due to standards for the weights file, the map_method
; attribute will say "Bilinear remapping" for bilinear, patch,
; neareststod, and nearestdtos methods. In newer versions of
; the offline regridder, there's a ESMF_regrid_method attribute.  Use this
; attribute if available; otherwise use the old fixed map_method.
; The ESMF_regrid routine will set FixMapMethod to the correct
; value so this routine can fix the "remap" attribute. Otherwise if 
; ESMF_regrid_with_weights is called directly, there's no way to 
; tell which interpolation method is used.
;----------------------------------------------------------------------
  if (.not.isatt(dstData,"remap")) then
    if(esmf_regrid_method.ne."") then
      dstData@remap = "remapped via ESMF_regrid_with_weights: " + \
                      esmf_regrid_method
    else
      if(isatt(opt,"FixMapMethod")) then
        dstData@remap = "remapped via ESMF_regrid_with_weights: " + \
                         str_sub_str(map_method,"Bilinear",opt@FixMapMethod)
      else
        dstData@remap = "remapped via ESMF_regrid_with_weights: " + \
                        map_method
      end if
    end if
  end if

;---Check for missing value attributes and fix them if necessary
  if(.not.isatt(dstData,"missing_value")) then
    dstData@missing_value = dstData@_FillValue
  else
    if(typeof(dstData@missing_value).ne.typeof(dstData)) then
      tmp_msg = totypeof(dstData@missing_value,typeof(dstData))
      delete(dstData@missing_value)
      dstData@missing_value = tmp_msg
    end if
  end if

  if(isatt(dstData,"_FillValue_original")) then
    delete(dstData@_FillValue_original)
  end if
  if(isatt(dstData,"missing_value_original")) then
    delete(dstData@missing_value_original)
  end if

  if(PrintTimings) then
    print_elapsed_esmf_time(start_time,"ESMF_regrid_with_weights")
  end if

  return(dstData)
end     ; of ESMF_regrid_with_weights(...)

;----------------------------------------------------------------------
; Note: this routine should be combined with "get_dst_grid_info" to
; make just one "get_grid_info" routine. For now, any changes made to
; this routine should potentially be done to "get_dst_grid_info".
;
; Given a source data variable, this function returns information 
; about the source grid, and the lat/lon values, if appropriate.
;
; The lat/lon values can be provided via:
;
;   - SrcGridLat/SrcGridLon
;
;   - Coordinate arrays [rectilinear grid] 
;
;   - lat2d/lon2d attributes [curvilinear grid] 
;
;   - lat1d/lon1d attributes [unstructured grid] 
;
;   - By a grid type of "5deg", "1x1", "2x3", etc.
;----------------------------------------------------------------------
undef("get_src_grid_info")
function get_src_grid_info(opt,data)
local opt2, grid_type_name, grid_lat_name, grid_lon_name, islatlon, \
      grid_string, lat_vals, lon_vals, lat_dims, lon_dims, slat_dims, slon_dims, \
      lat_rank, lon_rank, var_dims, var_rank, left_dims, grid_long_string
begin
  opt2             = opt
  grid_string      = "Src"
  grid_long_string = "source"

  DEBUG = isatt_logical_true(opt2,"Debug")

  var_dims = dimsizes(data)
  var_rank = dimsizes(var_dims)

;---List of some of the possible attributes attached to "opt".
  grid_type_name = grid_string + "GridType"
  grid_lat_name  = grid_string + "GridLat"
  grid_lon_name  = grid_string + "GridLon"

  grid_type = str_lower(get_att_value(opt2,grid_type_name,"unknown"))

;---Check if SrcGridLat/SrcGridLon were specified.
  if(isatt(opt2,grid_lat_name).and.isatt(opt2,grid_lon_name)) then
    islatlon = True
    lat_vals = opt2@$grid_lat_name$
    lon_vals = opt2@$grid_lon_name$

;---Curvilinear grid?
  else if(isatt(data,"lat2d").and.isatt(data,"lon2d")) then
    if(.not.any(grid_type.eq.(/"unknown","curvilinear"/))) then
      print("get_src_grid_info: the lat/lon values provided don't match")
      print("                   the grid type (" + grid_type + ") specified.")
      exit
    end if
    
    grid_type = "curvilinear"
    islatlon  = True
    lat_vals  = data@lat2d
    lon_vals  = data@lon2d
    
;---Rectilinear grid?
  else if(var_rank.ge.2.and. \
          isdimnamed(data,var_rank-1).and. \
          isdimnamed(data,var_rank-2).and. \
          iscoord(data,data!(var_rank-1)).and. \
          iscoord(data,data!(var_rank-2))) then

    if(.not.any(grid_type.eq.(/"unknown","rectilinear"/))) then
      print("get_src_grid_info: the lat/lon values provided don't match")
      print("               the grid type (" + grid_type + ")")
      exit
    end if

    grid_type = "rectilinear"
    islatlon  = True
    lat_vals  = data&$data!(var_rank-2)$
    lon_vals  = data&$data!(var_rank-1)$
;---Unstructured grid?
  else if(isatt(data,"lat1d").and.isatt(data,"lon1d")) then
    if(.not.any(grid_type.eq.(/"unknown","unstructured"/))) then
      print("get_src_grid_info: the lat/lon values provided don't match")
      print("                        the grid type (" + grid_type + \
                                      ") specified.")
      exit
    end if
    
    grid_type = "unstructured"
    islatlon  = True
    lat_vals  = data@lat1d
    lon_vals  = data@lon1d

;---Better be a grid like "5deg" or "2x3"!
  else
    islatlon  = False      ; No lat/lon values specified
  end if
  end if
  end if
  end if

  if(grid_type.eq."unknown".and..not.islatlon) then
    print("get_src_grid_info: can't determine what type of " + \
          grid_long_string + " grid you have.")
    exit
  end if

;---rectilinear, curvilinear, and unstructured require lat/lon values.
  if(any(grid_type.eq.(/"rectilinear","curvilinear","unstructured"/)).and.\
     .not.islatlon) then
    print("get_src_grid_info: no lat/lon values provided for type of grid specified (" + grid_type + ").")
    exit
  end if

  if(grid_type.eq."unknown".and..not.islatlon) then
    print("get_src_grid_info: can't determine type of grid you have.")
    exit
  end if
  if(islatlon) then
    lat_dims  = dimsizes(lat_vals)
    lon_dims  = dimsizes(lon_vals)
    lat_rank  = dimsizes(lat_dims)
    lon_rank  = dimsizes(lon_dims)
    slat_dims = tostring(lat_dims)
    slon_dims = tostring(lon_dims)

    if(DEBUG) then
      print("get_src_grid_info: " + grid_long_string + \
            " lat dims = (" + str_join(slat_dims,",") + ")")
      print("get_src_grid_info: " + grid_long_string + \
            " lon dims = (" + str_join(slon_dims,",") + ")")
    end if

    if(lat_rank.gt.2.or.lon_rank.gt.2.or.lat_rank.ne.lon_rank) then
      print("get_src_grid_info: invalid " + grid_long_string + \
            " lat/lon given.")
      exit
    end if

;---If grid_type still unknown at this point, try to guess at it.
    if(grid_type.eq."unknown") then
;---Test for rectilinear
      if(lat_rank.eq.1.and.var_rank.gt.1.and.\
         var_dims(var_rank-2).eq.lat_dims.and.\
         var_dims(var_rank-1).eq.lon_dims) then
        grid_type = "rectilinear"
;---Test for curvilinear
      else if(lat_rank.eq.2.and.lon_rank.eq.2.and.\
              all(lat_dims.eq.lon_dims)) then
        grid_type = "curvilinear"
;
; Test for unstructured (what a mess). The extra long test is to 
; make sure this is not possibly a rectilinear grid.
;
      else if(lat_rank.eq.1.and.lon_rank.eq.1.and.lat_dims.eq.lon_dims.and.\
              ((var_rank.eq.1.and.var_dims.eq.lat_dims).or.\
               (var_rank.gt.1.and.var_dims(var_rank-1).eq.lat_dims.and.\
                var_dims(var_rank-2).ne.var_dims(var_rank-1)))) then
        grid_type = "unstructured"
      end if
      end if
      end if
    end if
  end if

;---If grid_type is still "unknown", then we can't continue.
  if(grid_type.eq."unknown") then
    print("get_src_grid_info: " + grid_type_name + " and/or " + \
          grid_lat_name + "/" + grid_lon_name + " were not set.")
    print("          Cannot determine the " + grid_long_string + \
          " grid type.")
    exit
  end if

  if(DEBUG) then
    print("get_src_grid_info: " + grid_long_string + \
           " grid type is '" + grid_type + "'")
  end if

  if(grid_type.eq."rectilinear") then
    latlon_dims = (/lat_dims,lon_dims/)
    left_rank   = var_rank-2
  else if(grid_type.eq."curvilinear") then
    latlon_dims = lat_dims
    left_rank   = var_rank-2
  else if(grid_type.eq."unstructured") then
    latlon_dims = lat_dims
    left_rank   = var_rank-1
  else
    return([/grid_type/])
  end if
  end if
  end if

  if(left_rank.ge.1)
    left_dims = var_dims(0:left_rank-1)
  else
    left_dims = -1
  end if    

  return([/grid_type,latlon_dims,lat_vals,lon_vals,left_dims/])
end

;----------------------------------------------------------------------
; Note: this routine should be combined with "get_src_grid_info" to
; make just one "get_grid_info" routine. For now, any changes made to
; this routine should potentially be done to "get_src_grid_info".
;
; Given an option variable, this function returns information about 
; the destination grid and the lat/lon values, if appropriate.
;
; The lat/lon values can be provided via:
;
;   - DstGridLat/DstGridLon
;
;   - By a grid type of "5deg", "1x1", "2x3", etc.
;----------------------------------------------------------------------
undef("get_dst_grid_info")
function get_dst_grid_info(opt)
local opt2, grid_type_name, grid_lat_name, grid_lon_name, islatlon, \
      grid_string, lat_dims, lon_dims, slat_dims, slon_dims, \
      lat_rank, lon_rank, grid_long_string
begin
  opt2             = opt
  grid_string      = "Dst"
  grid_long_string = "destination"

  DEBUG = isatt_logical_true(opt2,"Debug")

;---List of some of the possible attributes attached to "opt".
  grid_type_name = grid_string + "GridType"
  grid_lat_name  = grid_string + "GridLat"
  grid_lon_name  = grid_string + "GridLon"

  grid_type = str_lower(get_att_value(opt2,grid_type_name,"unknown"))

;---Check if DstGridLat/DstGridLon were specified.
  if(isatt(opt2,grid_lat_name).and.isatt(opt2,grid_lon_name)) then
    islatlon = True
    lat_vals = opt2@$grid_lat_name$
    lon_vals = opt2@$grid_lon_name$
;---Better be a grid like "5deg" or "2x3"!
  else
    islatlon  = False      ; No lat/lon values specified
  end if

  if(grid_type.eq."unknown".and..not.islatlon) then
    print("get_dst_grid_info: can't determine what type of " + \
          grid_long_string + " grid you have.")
    exit
  end if

;---rectilinear, curvilinear, and unstructured require lat/lon values.
  if(any(grid_type.eq.(/"rectilinear","curvilinear","unstructured"/)).and.\
     .not.islatlon) then
    print("get_dst_grid_info: no lat/lon values provided for type of grid specified (" + grid_type + ").")
    exit
  end if

  if(grid_type.eq."unknown".and..not.islatlon) then
    print("get_dst_grid_info: can't determine type of grid you have.")
    exit
  end if
  if(islatlon) then
    lat_dims  = dimsizes(lat_vals)
    lon_dims  = dimsizes(lon_vals)
    lat_rank  = dimsizes(lat_dims)
    lon_rank  = dimsizes(lon_dims)
    slat_dims = tostring(lat_dims)
    slon_dims = tostring(lon_dims)

    if(DEBUG) then
      print("get_dst_grid_info: " + grid_long_string + \
            " lat dims = (" + str_join(slat_dims,",") + ")")
      print("get_dst_grid_info: " + grid_long_string + \
            " lon dims = (" + str_join(slon_dims,",") + ")")
    end if

    if(lat_rank.gt.2.or.lon_rank.gt.2.or.lat_rank.ne.lon_rank) then
      print("get_dst_grid_info: invalid " + grid_long_string + \
            " lat/lon given.")
      exit
    end if

;
; If grid_type still unknown at this point, try to guess at it.
;
; Note: if lat_dims and lon_dims are both 1D and equal in length,
; this can be either an unstructured or rectilinear grid. In
; this case, the user *must* set "grid_type" to either "rectilinear"
; or "unstructured" so that it's clear which one this is.
;
    if(grid_type.eq."unknown") then
      if(lat_rank.eq.1.and.lon_rank.eq.1.and.lat_dims.ne.lon_dims) then
        grid_type = "rectilinear"
      else if(lat_rank.eq.2.and.lon_rank.eq.2.and.\
              all(lat_dims.eq.lon_dims)) then
        grid_type = "curvilinear"
      end if
      end if
    end if
  end if

;---If grid_type is still "unknown", then we can't continue.
  if(grid_type.eq."unknown") then
    if(lat_rank.eq.1.and.lon_rank.eq.1.and.lat_dims.eq.lon_dims) then
      print("get_dst_grid_info: Error: the lat/lon arrays are both 1D and the same length.")
      print("   This could be a rectilinear or unstructured grid.") 
      print("   Set 'DstGridType' to 'rectilinear' or 'unstructured'.")
    else   
      print("get_dst_grid_info: " + grid_type_name + " and/or " + \
            grid_lat_name + "/" + grid_lon_name + " were not set.")
      print("          Cannot determine the " + grid_long_string + \
            " grid type.")
    end if
    exit
  end if

  if(grid_type.eq."rectilinear") then
    latlon_dims = (/lat_dims,lon_dims/)
  else if(grid_type.eq."curvilinear".or.grid_type.eq."unstructured") then
    latlon_dims = lat_dims
  end if
  end if

  if(islatlon) then
    return([/grid_type,latlon_dims,lat_vals,lon_vals/])
  else
    return([/grid_type/])
  end if
end

;======================================================================
; This procedure is a generic one that writes the description of a 
; source or destination grid to a NetCDF file. It distinguishes 
; between the source and data grids via the "which_grid" option,
; and also via the "opt" attributes which can start with "Src" or
; "Dst".
;
; Arguments:
;    srcData    - variable to be regridded
;    which_grid - must be "Src" or "Dst" (or "source", "destination")
;    opt        - logical variable with attributes
;======================================================================
undef("write_grid_description_file")
procedure write_grid_description_file(srcData,which_grid,opt)
local opt_new, opt2, grid_type, lat_dims, lon_dims, \
      lat_rank, lon_rank, grid_file_name, grid_lat_name, DEBUG, \
      grid_string, grid_lon_name, grid_type_name, spc_atts, \
      lat_vals, lon_vals, gen_atts, tmp_att, i, grid_type
begin
  opt_new = True  ; For constructing new options list
  opt2    = opt   ; Make a copy so we can modify it

  DEBUG = isatt_logical_true(opt2,"Debug")

;---Test which_grid
  valid_src_names = (/"src","source"/)
  valid_dst_names = (/"dst","destination"/)
  if(any(str_lower(which_grid).eq.valid_src_names)) then
    grid_string      = "Src"
    grid_long_string = "source"
  else if(any(str_lower(which_grid).eq.valid_dst_names)) then
    grid_string      = "Dst"
    grid_long_string = "destination"
  else
    grid_string = ""
  end if
  end if
  if(grid_string.eq."") then
    print("write_grid_description_file: don't recognize value for second " + \
          "argument: '" + which_grid + "'")
    exit
  end if  

;
; Some attribute options cannot both be set if they have
; different values.  Check for them here.
;
  check_both_atts(opt2,grid_string+"Overwrite","Overwrite",False)
  check_both_atts(opt2,grid_string+"ForceOverwrite","ForceOverwrite",False)

;---List of some of the possible attributes attached to "opt".
  grid_file_name = grid_string + "FileName"
  grid_type_name = grid_string + "GridType"

;
; Check for generic attributes to pass through; that is,
; attributes like "Debug".
;
  gen_atts = (/"NetCDFType","GridCornerDelta","PrintTimings",\
               "Overwrite","ForceOverwrite","Debug"/)
  do i=0,dimsizes(gen_atts)-1
    if(isatt(opt2,gen_atts(i))) then
      opt_new@$gen_atts(i)$ = opt2@$gen_atts(i)$
    end if
  end do
;
; Check for specific attributes to pass through; that is,
; attributes that start with "Src" or "Dst".
;
; If the generic form of the attribute has already been
; set, like "opt@Overwrite", then ignore the specific
; attribute, like "opt@SrcOverwrite".
;
  spc_atts = (/"GridMask","Mask2D","GridArea","TriangularMesh","ElementVertices",\
               "LargeFile","ESMF","SCRIP","NetCDFType","Title","LLCorner",\
               "URCorner","Rotation","GridCornerLat","GridCornerLon",\
               "InputFileName","Overwrite","ForceOverwrite"/)
  do i=0,dimsizes(spc_atts)-1
    tmp_att = grid_string + spc_atts(i)
    if(isatt(opt2,tmp_att)) then
      opt_new@$spc_atts(i)$ = opt2@$tmp_att$
    end if
  end do
;
; Error checking added in V6.1.1 to make sure user doesn't 
; accidentally specify SrcFileName or DstFileName to be
; the original input file names.
;
  if(isatt(opt2,grid_file_name).and.isfilepresent(opt2@$grid_file_name$).and.\
     .not.is_SCRIP(opt2@$grid_file_name$,False).and.\
     .not.is_ESMF(opt2@$grid_file_name$,False)) then
     print("ESMF_regrid: error: the file name given for the output " + grid_long_string)
     print("    grid file (via the special option '" + grid_string + "FileName') already exists")
     print("    ('" + opt2@$grid_file_name$ + "'), but doesn't appear to be a SCRIP")
     print("    file. Make sure you are not overwriting any original data files!")
     print("")
     print("    Remember: '" + grid_string + "FileName' should be set to the name of the")
     print("    *output* " + grid_long_string + " grid description file that will be")
     print("    created, and not the name of the original input file that")
     print("    contains your data or destination grid. Use '" + grid_string + "InputFileName'")
     print("    for that purpose.")
     exit
  end if

  if(grid_string.eq."Src") then
    grid_info_list = get_src_grid_info(opt2,srcData)
  else
    grid_info_list = get_dst_grid_info(opt2)
  end if
  nlist     = ListCount(grid_info_list)
  grid_type = grid_info_list[0]
  if(nlist.gt.1) then
    latlon_dims = grid_info_list[1]
    lat_vals    = grid_info_list[2]
    lon_vals    = grid_info_list[3]
  end if

;---Check if user set a filename for the description file
  grid_file = get_att_value(opt2,grid_file_name,grid_long_string + \
                                               "_grid_file.nc")
  if(grid_type.eq."rectilinear") then
    rectilinear_to_SCRIP(grid_file,lat_vals,lon_vals,opt_new)
  else if(grid_type.eq."curvilinear") then
    curvilinear_to_SCRIP(grid_file,lat_vals,lon_vals,opt_new)
  else if(grid_type.eq."unstructured") then
;
; This extra code for writing a SCRIP unstructured file
; file was added Feb 10, 2015. I'm not sure it's robust
; yet.  It is currently unadvertised, but can be 
; done by the user if they set opt@SrcSCRIP = True or
; opt@DstSCRIP = True. The default is to write an
; ESMF unstructured file.
;
    if(.not.isatt_logical_true(opt_new,"SCRIP")) then
      unstructured_to_ESMF(grid_file,lat_vals,lon_vals,opt_new)
      tmp_att = grid_string + "ESMF"
      if(DEBUG) then
        print("write_grid_description_file: writing unstructured '" + grid_string + \
        "' grid to an ESMF description file")
      end if
    else
      unstructured_to_SCRIP(grid_file,lat_vals,lon_vals,opt_new)
;---Make sure SrcSCRIP or DstSCRIP is set (the default is to assume SCRIP)
      tmp_att = grid_string + "SCRIP"
      if(DEBUG) then
        print("write_grid_description_file: writing unstructured '" + grid_string + \
        "' grid to a SCRIP description file")
      end if
    end if
    opt@$tmp_att$ = True
  else
    latlon_to_SCRIP(grid_file,grid_type,opt_new)
    opt@$grid_type_name$ = "rectilinear"
  end if
  end if
  end if

;---Save the name of the description file and grid type
  if(.not.isatt(opt,grid_file_name)) then
    opt@$grid_file_name$ = grid_file
  end if
  if(.not.isatt(opt,grid_type_name)) then
    opt@$grid_type_name$ = grid_type
  end if
end

;======================================================================
; This function combines all the regridding steps:
;   1) Describing the source and destination grids
;   2) Generating the weights
;   3) Apply the weights to a given variable
;
; This function recognizes the following options
;
; Recognized attributes for "Opt":
;   SrcLLCorner / SrcURCorner - defaults to (-90,-180) / ( 90, 180)
;   DstLLCorner / DstURCorner - defaults to (-90,-180) / ( 90, 180)
;   SrcRotation / DstRotation - rotation angle
;
; SrcGridCornerLat,SrcGridCornerLon / DstGridCornerLat,DstGridCornerLon
;  - if set, then these values are used rather than calculating the 
;    corner points for the cells 
;
;  SrcFileName / DstFileName
;  SrcGridType / SrcGridLat / SrcGridLon
;  DstGridType / DstGridLat / DstGridLon
;  SrcGridMask / DstGridMask    (new names for mask as of NCL V6.3.0)
;  SrcGridArea / DstGridArea
;  SrcMask2D   / DstMask2D      (old names, use Src/DstGridMask)
;  SrcLargeFile / DstLargeFile
;  SrcNetCDFType / DstNetCDFType
;  WgtFileName
;  SkipSrcGrid / SkipDstGrid / SkipWgtGen
;  Debug
;  PrintTimings
;  ForceOverwrite
;  Overwrite
;======================================================================
undef("ESMF_regrid")
function ESMF_regrid(srcData:numeric, opt[1]:logical)
local opt2, src_dims, src_rank, src_grid_type, src_grid_name, \
      src_lat_dims, src_lon_dims, src_lat_rank, src_lon_rank, \
      dst_dims, dst_rank, dst_grid_type, dst_grid_name, \
      dst_lat_dims, dst_lon_dims, dst_lat_rank, dst_lon_rank, \
      dstlat, dstlon, wgt_file_name, remove_dst_file, \
      remove_src_file, remove_wgt_file, DEBUG
begin
;---Check for certain options that must be set.
  if(.not.opt) then
    print("ESMF_regrid: opt was set to False. Can't continue.")
    exit
  end if

  DEBUG = isatt_logical_true(opt,"Debug")

;---Make a copy so we don't modify original opt.
  opt2 = opt

;---Check for incompatible attributes set by user
  check_both_atts(opt2,"RemoveFiles","RemoveSrcFile",False)
  check_both_atts(opt2,"RemoveFiles","RemoveDstFile",False)
  check_both_atts(opt2,"RemoveFiles","RemoveWgtFile",False)

;
; Check if any of the source, grid, or weight files are
; to be removed after this function is done. It's easier
; to let this routine handle it, than ESMF_regrid_gen_weights,
; so just do it.
;
  remove_src_file = False
  remove_dst_file = False
  remove_wgt_file = False
  if(isatt_logical_true(opt2,"RemoveFiles")) then
    delete(opt2@RemoveFiles)
    remove_src_file = True
    remove_dst_file = True
    remove_wgt_file = True
  end if
  if(isatt_logical_true(opt2,"RemoveSrcFile")) then
    delete(opt2@RemoveSrcFile)
    remove_src_file = True
  end if
  if(isatt_logical_true(opt2,"RemoveDstFile")) then
    delete(opt2@RemoveDstFile)
    remove_dst_file = True
  end if
  if(isatt_logical_true(opt2,"RemoveWgtFile")) then
    delete(opt2@RemoveWgtFile)
    remove_wgt_file = True
  end if

;---Write source grid description file
  if(.not.isatt_logical_true(opt2,"SkipSrcGrid")) then
    write_grid_description_file(srcData,"Src",opt2)
  else
    if(DEBUG) then
      print("ESMF_regrid: skipping generation of source grid.")
    end if
  end if
    
;---Write destination grid description file
  if(.not.isatt_logical_true(opt2,"SkipDstGrid")) then
    write_grid_description_file(srcData,"Dst",opt2)
  else
    if(DEBUG) then
      print("ESMF_regrid: skipping generation of destination grid.")
      print("             You may need to set DstGridType.")
    end if
  end if

;--------------------------------------------------
; Section to generate the weights
;--------------------------------------------------
  wgt_file_name         = get_att_value(opt2,"WgtFileName","weights_file.nc")
  if(.not.isatt_logical_true(opt2,"SkipWgtGen")) then
;
; If skipped the source and/or destination grid generation phase, then
; the user must supply the names of the grid files explicitly.
;
    if(isatt_logical_true(opt,"SkipSrcGrid").and.isatt(opt,"SrcFileName")) then
      src_grid_file = opt@SrcFileName
    else if(.not.isatt_logical_true(opt,"SkipSrcGrid").and.isatt(opt2,"SrcFileName")) then
      src_grid_file = opt2@SrcFileName
    else
      print("ESMF_regrid: need the name of the source grid file (SrcFileName)")
      exit
    end if
    end if
    if(isatt_logical_true(opt,"SkipDstGrid").and.isatt(opt,"DstFileName")) then
      dst_grid_file = opt@DstFileName
    else if(.not.isatt_logical_true(opt,"SkipDstGrid").and.isatt(opt2,"DstFileName")) then
      dst_grid_file = opt2@DstFileName
    else
      print("ESMF_regrid: need the name of the destination grid file (DstFileName)")
      exit
    end if
    end if
    ESMF_regrid_gen_weights(src_grid_file, dst_grid_file, wgt_file_name, opt2)
  else
    if(DEBUG) then
      print("ESMF_regrid: skipping generation of weights file.")
    end if
  end if

;
; If patch method used, "sneak" this knowledge into the weights gen
; routine so that we can get the correct "map_method" for the
; "remap" attribute.
;
  if(opt2.and.isatt(opt2,"InterpMethod").and. \
     .not.any(str_lower(opt2@InterpMethod).eq.(/"bilinear","conserve"/)))
   opt2@FixMapMethod = str_capital(opt2@InterpMethod)
  end if

;---Regrid the input variable
  srcData_regrid = ESMF_regrid_with_weights(srcData,wgt_file_name,opt2)

;---Copy variable attributes and coordinate arrays if requested.
  ESMF_copy_varmeta(srcData,srcData_regrid,wgt_file_name,opt2)

;---Clean up, if desired
  if (remove_src_file) then
    if(DEBUG) then
      print("ESMF_regrid: By request, removing '" + src_grid_file + "'")
    end if    
    system("rm -rf " + src_grid_file)
  end if

  if (remove_dst_file) then
    if(DEBUG) then
      print("ESMF_regrid: By request, removing '" + dst_grid_file + "'")
    end if    
    system("rm -rf " + dst_grid_file)
  end if

  if (remove_wgt_file) then
    if(DEBUG) then
      print("ESMF_regrid: By request, removing '" + wgt_file_name + "'")
    end if    
    system("rm -rf " + wgt_file_name)
  end if

  return(srcData_regrid)
end

;======================================================================
; This routine is obsolete in V6.1.0 (not in V6.1.0-beta). It's
; replaced by ESMF_copy_varcoords.  I'm not sure it was
; ever used...
;======================================================================
undef("retrieve_dstGrid_lat")
function retrieve_dstGrid_lat(fileName[1]:string)
local fid, dst_grid_dims
begin
    if (.not.isfilepresent(fileName)) then
        print("retrieve_dstGrid_lat: cannot open '" + fileName + "'.")
        exit
    end if

    fid           = addfile(fileName,"r")
    dst_grid_dims = fid->dst_grid_dims
    lat           = reshape( fid->yc_b , dst_grid_dims(::-1))
    return(lat)
end     ; of retrieve_dstGrid_lat

;======================================================================
; This routine is obsolete in V6.1.0 (not in V6.1.0-beta). It's
; replaced by ESMF_copy_varcoords.  I'm not sure it was
; ever used...
;======================================================================
undef("retrieve_dstGrid_lon")
function retrieve_dstGrid_lon(fileName[1]:string)
local fid, dst_grid_dims
begin
    if (.not.isfilepresent(fileName)) then
        print("retrieve_dstGrid_lon: cannot open '" + fileName + "'.")
        exit
    end if

    fid           = addfile(fileName,"r")
    dst_grid_dims = fid->dst_grid_dims
    lon           = reshape( fid->xc_b , dst_grid_dims(::-1))
    return(lon)
    
end     ; of retrieve_dstGrid_lon

;======================================================================
; This routine is obsolete in V6.1.0 (not in V6.1.0-beta). It's
; replaced by ESMF_copy_varcoords.  I'm not sure it was
; ever used...
;======================================================================
undef("retrieve_srcGrid_lat")
function retrieve_srcGrid_lat(fileName[1]:string)
local fid, src_grid_dims
begin
    if (.not.isfilepresent(fileName)) then
        print("retrieve_srcGrid_lat: cannot open '" + fileName + "'.")
        exit
    end if

    fid           = addfile(fileName,"r")
    src_grid_dims = fid->src_grid_dims
    lat           = reshape( fid->yc_a , src_grid_dims(::-1))
    return(lat)
end     ; of retrieve_srcGrid_lat

;======================================================================
; This routine is obsolete in V6.1.0 (not in V6.1.0-beta). It's
; replaced by ESMF_copy_varcoords.  I'm not sure it was
; ever used...
;======================================================================
undef("retrieve_srcGrid_lon")
function retrieve_srcGrid_lon(fileName[1]:string)
local fid, src_grid_dims
begin
    if (.not.isfilepresent(fileName)) then
        print("retrieve_srcGrid_lon: cannot open '" + fileName + "'.")
        exit
    end if

    fid           = addfile(fileName,"r")
    src_grid_dims = fid->src_grid_dims
    lon           = reshape( fid->xc_a , src_grid_dims(::-1))
    return(lon)
end     ; of retrieve_srcGrid_lon
