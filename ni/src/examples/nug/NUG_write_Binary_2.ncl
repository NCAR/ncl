;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_write_Binary_2.ncl
;
;  KMF
;-----------------------------------------------------------------
begin
;-- Create some dummy arrays.
  nlev = 10
  nlat = 64
  nlon = 128

  t1 = random_uniform(0,100,(/nlev,nlat,nlon/))
  t2 = random_uniform(0,100,(/nlev,nlat,nlon/))
  t3 = random_uniform(0,100,(/nlev,nlat,nlon/))

;-- Remove file just in case
  filename = "dummy_file.bin"
  system("rm -f " + filename)

;-- Write first variable to file
  fbindirwrite(filename,t1)
  system("ls -l " + filename)

;-- Append second variable to file
  fbindirwrite(filename,t2)
  system("ls -l " + filename)

;-- Append third variable to file
  fbindirwrite(filename,t3)
  system("ls -l " + filename)

;-- Read data back in and compare the diffs. Should be equal to 0.
  t1r = fbindirread(filename,0,(/nlev,nlat,nlon/),"float")
  t2r = fbindirread(filename,1,(/nlev,nlat,nlon/),"float")
  t3r = fbindirread(filename,2,(/nlev,nlat,nlon/),"float")

  print("")
  print("If the result below is 0/0 then everything is fine!")
  print(min(t1r-t1) + "/" + max(t1r-t1))
  print(min(t2r-t2) + "/" + max(t2r-t2))
  print(min(t3r-t3) + "/" + max(t3r-t3))
  
end

