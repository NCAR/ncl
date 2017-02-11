;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_system_calls.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
;============================================
;== define function my_pi()
;== note: can use "get_pi" added in V6.4.0
;============================================
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
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
  start_time = get_cpu_time
  print("--------------------------------------")
  print("Start CPU time: " + start_time + "s")

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

  print("Directory of NCL database files:  " + ncargpath("database"))
  print("Directory of NCL binaries:        " + ncargpath("bin"))
  print("--------------------------------------")

  print("Sleep 1 seconds")
  sleep(1)
  print("--------------------------------------")

  print("Call function my_pi()")
  x = my_pi()
  print(x)
  print("--------------------------------------")

  end_time = get_cpu_time
  print("--------------------------------------")
  print("End CPU time:   " + end_time + "s")
  print("Total CPU time: " + (end_time-start_time) + "s")
  print("--------------------------------------")

end
