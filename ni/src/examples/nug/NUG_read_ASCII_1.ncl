;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_read_ASCII_1.ncl
;
;  KMF
;-----------------------------------------------------------------
begin
; Read data into a one-dimensional int array of length 14:

  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "asc1.txt"
  
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
  
  data = asciiread(diri+fili,14,"integer")

  npts = dimsizes(data)   ; should be 14
  print("Number of values: "+npts)
  print(data)             ; print the values

end
