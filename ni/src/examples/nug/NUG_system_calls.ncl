;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_system_calls.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
;======================================
;== define function my_pi()
;======================================
undef("my_pi")
function my_pi()
local lpi
begin
  lpi = 4*atan(1)
  return(lpi)
end

;=========================================================================
;==   MAIN script
;=========================================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  print("")
  print("")
  print("--------------------------------------")

  print("echo $PWD")
  system("echo $PWD")
  print("date")
  system("date")
  print("--------------------------------------")

  print("Environment variable NCARG_COLORMAPS:  " + getenv("NCARG_COLORMAPS"))
  print("--------------------------------------")

  ret = isbigendian()
  if(ret.eq.True) then
     print("Big endian machine:   True")
  else
     print("Big endian machine:   False")
  end if
  print("--------------------------------------")

  print("Script name:  " + get_script_name())
  print("Script name:  " + get_script_prefix_name())
  print("--------------------------------------")

  file_list = systemfunc("ls *.ncl")
  print("ls *.ncl")
  print(file_list)
  print("--------------------------------------")

  print("Total CPU time: " + get_cpu_time() + "s")
  print("--------------------------------------")

  print("Directory of NCL database files:  " + ncargpath("database"))
  print("Directory of NCL documents:       " + ncargpath("bin"))
  print("--------------------------------------")

  print("Sleep 1 seconds")
  sleep(1)
  print("--------------------------------------")

  print("Call function my_pi()")
  x = my_pi()
  print(x)
  print("--------------------------------------")

end
