;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_strings.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
   qc    = str_get_dq()                          ; the quote character

   title = "Strings"
   fili  = "tas_AFR-44_MPI-ESM-LR_historical_r1i1p1_CCLM_4-8-17_mon_1961-1990.nc"

;-- get number of field elements of a given string and delimiter
   print("---")
   split_char = "_"
   print("File name: " + fili)
   print("Number of fields split by '" + split_char + "' character: " + str_fields_count(fili,split_char))
   print("---")

;-- get field elements of given string
   var         =  str_get_field(fili,1,"_")
   domain      =  str_get_field(fili,2,"_")
   gcm         =  str_get_field(fili,3,"_")
   exp_id      =  str_get_field(fili,4,"_")
   exp_vs      =  str_get_field(fili,5,"_")
   rcm         =  str_get_field(fili,6,"_")
   rcm_vs      =  str_get_field(fili,7,"_")
   freq        =  str_get_field(fili,8,"_")
   time_int    =  str_get_field(fili,9,"_")
   start_year  =  toint(str_get_field(time_int,1,"-"))
   end_year    =  toint(str_get_field(time_int,2,"-"))
   file_format =  str_get_field(fili,2,".")

   print("Variable name:       "+var)
   print("Domain name:         "+domain)
   print("GCM name:            "+gcm)
   print("Experiment id:       "+exp_id)
   print("Experiment version:  "+exp_vs)
   print("RCM name:            "+rcm)
   print("RCM version:         "+rcm_vs)
   print("Frequency:           "+freq)
   print("Start year:          "+start_year)
   print("Endyear:             "+end_year)
   print("File extension:      "+file_format)
   print("---")

;-- get quote character itself"
   dq = str_get_dq()
   print("< " + dq + " >")
   print("---")

;-- write with newline to stdout
   nl = str_get_nl()
   print(nl +"Now you can write text in a new line to stdout" + nl)

;-- delete leading and ending spaces and TABs
   str = "     abc        "
   print("<" + str + ">" + nl)
   new_string = str_left_strip(str)
   print("<" + new_string + ">" + nl)
   str = "     abc        "
   new_string = str_right_strip(str)
   print("<" + new_string + ">" + nl)
   str = "     abc        "
   new_string = str_strip(str)
   print("<" + new_string + ">" + nl)

;-- change to upper case or lower case
    str_up  = "HELLO WORLD"
    str_low = "good morning"

    print("Original string:  " + str_up + "   converted to   " + str_lower(str_up))
    print("Original string:  " + str_low + "  converted to   " + str_upper(str_low) + nl)

;-- capitalize all elemts of a string

    str_line = "hello world - this is a test for string manipulations"
    print("Capitalize the following text:  " + str_line)
    print("                                " + str_capital(str_line))

;-- concatenation of strings

    str_array = (/"Temperature", "Precipitation", "None", "Soil", "Cloud Cover", "None", "Wind speed"/)
    str_array@_FillValue = "None"
    print("Original string array:      " + str_join(str_array,","))
    print("Concatenated string array:  " + str_concat(str_array))
    print("Concatenated string array w/'None' replaced with a space:" + str_get_nl() + \
          "                                    " + str_concat(where(ismissing(str_array)," ",str_array)))
  
    str     = "Using NCL makes a lot of fun"
    str_new = str_split(str, " ")
    print("Original single string      = " + qc + str + qc)
    print("New strings split by spaces = " + qc + str_new + qc)

    str     = "20130101,000000,53.33,10.0,278.32,t2m,,,Near-Surface Air Temperature"
    str_new := str_split_csv(str, ",", 0)
    print("Original single string      = " + qc + str + qc)
    print("New strings split by commas = " + qc+ str_new + qc) 

end
