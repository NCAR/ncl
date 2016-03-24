;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_write_ASCII_4.ncl
;
;  KMF
;-----------------------------------------------------------------
begin
;-- Generate a dummy 3D array
  nx   = 200    ; # of blocks
  ny   = 100    ; # of rows
  nz   = 10     ; # of columns
  data = random_uniform(-5,5,(/nx,ny,nz/))
;
; Use this to create "nice" numbers for debug purposes. 
; This makes it easier to see how the data file is being written.
;
;  data = reshape(conform_dims((/200,ny*nz/),ispan(1,ny*nz,1),1),\
;          (/nx,ny,nz/)) + conform_dims((/nx,ny,nz/),ispan(1,nx,1),0)\
;           /1000.

;-- Remove file just in case
  filename = "file4.txt"
  system("rm -f " + filename)

;-- Write a header to the file
  header = "This ASCII file contains " + nx + " blocks of " + ny + \
           " x " + nz + " arrays"
  write_table(filename, "w", [/header/], "%s") ; Use "w" to create file

;-- Create row format string. It will have "%7.3f" repeated nz times
  fmt_str = "%s" + str_concat(conform_dims(nz,"%8.3f",-1))
;
; Loop through each column of each block and write the
; column of data to a List object.  We can then use
; write_table to append a whole block of formatted data
; to an ASCII file.
;
  row_labels = "Row " + sprinti("%3i",ispan(1,ny,1))

  dtmp = True   ; Variable to hold temporary attributes
  do i=0,nx-1

;-- Write out the block number. Use "a" to append to existing file.
    slist = [/"Block " + (i+1) + " of " + nx/]
    write_table(filename, "a", slist, "%s")

;-- Create a new List object for this block of data
    dlist = NewList("lifo")

;-- Loop in reverse order so items are written in correct order
    do j=nz-1,0,1
      ListPush(dlist,(/data(i,:,j)/))
    end do  

;-- Push array of row headers onto list object
    str = unique_string("test")
    dtmp@$str$ = row_labels
    ListPush(dlist,dtmp@$str$)

;-- Append this List of data to file.
    write_table(filename, "a", dlist, fmt_str)
  end do  

end
