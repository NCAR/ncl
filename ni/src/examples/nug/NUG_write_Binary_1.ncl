;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_write_Binary_1.ncl
;
;  KMF
;-----------------------------------------------------------------
begin 

;-- set path and file name
  diri     = "./"
  fili     = "rectilinear_grid_2D.nc"
  file_out = "example.bin"

  if (.not. fileexists(diri+fili)) then
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     exit
  end if
  
;-- read netCDF file and variable
  fi = addfile(diri+fili,"r")
  t  = fi->tsurf

  if (isfilepresent(file_out)) then
    system("rm -rf "+fileout)      ;-- make sure that file does not exist
  end if
  
;*************************************************************
; note the -1 indicates to just add on to the end of the file
; the (/.../) syntax means output the values only with no meta
; data
;*************************************************************

;-- output latitudes
   fbinrecwrite (file_out, -1, (/ fi->lat /))
   
;-- output longitude  
   fbinrecwrite (file_out, -1, (/ fi->lon /))

;-- output data subsection: 1st time step, 2nd latitude
   fbinrecwrite (file_out, -1, (/ t(0,1,:) /))  

end   
