;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print_clock.ncl
;; Carl Schreck (carl@cicsnc.org)
;; July 2011
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Print timestamp along with a string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

undef(   "print_clock" )
procedure print_clock( \\
  i_message \\ ; message to accompany timestamp
)
local None
begin
  
  maxCols = 44

  wallClock = systemfunc( "date +'  |  %F %T %Z'" )

  if( isstring( i_message ) ) then
    chMessage = stringtochar(i_message)

    if( dimsizes(chMessage).ge.maxCols ) then
      firstLine  = chartostring(chMessage(:maxCols-1))
      restOfMsg  = chartostring(chMessage(maxCols-1:))
      print( firstLine + wallClock )
      print( (/ restOfMsg /) )
    else
      blank     = stringtochar(" ")
      chPadding = new( maxCols +1 - dimsizes(chMessage), character )
      chPadding = blank(0)
      stPadding = chartostring(chPadding)
      firstLine = str_concat( (/ i_message, stPadding /) )
      print( firstLine + wallClock )
    end if

  else
    ;; Message is not a string
    print( i_message + wallClock )
  end if

end
