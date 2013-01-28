;--------------------------------------------------------------------------------
; This function convert input variable x to type specified by type.
; Wei Huang
; May 21, 2012
;--------------------------------------------------------------------------------

 undef("totype")
 function totype( varin, type:string )
 local varout

 begin
    ;printVarSummary(varin)
    ;print(type)

     ;Convert to float
     if(type .eq. "float") then
         varout = tofloat(varin)
         return(varout)
     end if

     ;Convert to double
     if(type .eq. "double") then
         varout = todouble(varin)
         return(varout)
     end if

     ;Convert to uint
     if(type .eq. "uint") then
         varout = touint(varin)
         return(varout)
     end if

    ;Convert to integer
     if(type .eq. "int" .or. type .eq. "integer") then
         varout = toint(varin)
         return(varout)
     end if

     ;Convert to char
     if(type .eq. "char" .or. type .eq. "character") then
         varout = tochar(varin)
         return(varout)
     end if

     ;Convert to byte
     if(type .eq. "byte") then
         varout = tobyte(varin)
         return(varout)
     end if

     ;Convert to short
     if(type .eq. "short") then
         varout = toshort(varin)
         return(varout)
     end if

     ;Convert to ushort
     if(type .eq. "ushort") then
         varout = toushort(varin)
         return(varout)
     end if

     ;Convert to long
     if(type .eq. "long") then
         varout = tolong(varin)
         return(varout)
     end if

     ;Convert to ulong
     if(type .eq. "ulong") then
         varout = toulong(varin)
         return(varout)
     end if

     ;Convert to int64
     if(type .eq. "int64") then
         varout = toint64(varin)
         return(varout)
     end if

     ;Convert to uint64
     if(type .eq. "uint64") then
         varout = touint64(varin)
         return(varout)
     end if

     ;Convert to string
     if(type .eq. "string") then
         varout = tostring(varin)
         return(varout)
     end if

     print("")
     print("WARNING:")
     print("CANNOT convert input variable type: <" + typeof(varin) + "> to type: <" + type + ">")
     print("The original type: <" + typeof(varin) + "> is returned.")
     print("")

     varout = varin
     return(varout)
 end

