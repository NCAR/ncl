;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nchoosek function
; Gabriel Abrahao: gabriel.abrahao@ufv.br
; Adapted from the original Martin Broadhurst's C code, that can be found at:
; http://www.martinbroadhurst.com/
;
; Prototype:
; function nchoosek (
;	n	:integer
;	k	:integer
; )
;
; return_val [n!/(k!(n-k)!),k] 
;
; Returns all the k-combinations taken from the n-set {0:n-1}
;
; Uses: nextcombination (below)
;
; The algorithm is explained at : http://www.martinbroadhurst.com/combinatorial-algorithms.html#combinations
; In short:
; Begin with the combination containing the the numbers from 0 to k - 1, and at each step: 
; -Find the rightmost element ar(i) that is less than the maximum value it can have (which is (n - 1) - (k - 1) - i) 
; -Increment it 
; -Turn the elements after it into a linear sequence continuing from ar(i)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nextcombination function
; Gabriel Abrahao: gabriel.abrahao@ufv.br
; Adapted from the original Martin Broadhurst's C code, that can be found at:
; http://www.martinbroadhurst.com/source/combination.c.html
;
; Takes a strictly increasing vector of k integers and finds the next k-combination in the lexicographical order,
; from the set {0:n-1}
;
; The algorithm is explained at : http://www.martinbroadhurst.com/combinatorial-algorithms.html#combinations
; In short, it does the step in the algorithm to find all combinations:
; Begin with the combination containing the the numbers from 0 to k - 1, and at each step: 
; -Find the rightmost element ar(i) that is less than the maximum value it can have (which is (n - 1) - (k - 1) - i) 
; -Increment it 
; -Turn the elements after it into a linear sequence continuing from ar(i)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

undef("nextcombination")
function nextcombination(n,k,ar)
begin

;n=5
;k=3
;ar=(/2,3,4/)
;ar=(/0,2,3/)

finished=0
changed=0

;do i=k-1,0,-1
i=k-1
do while(i .ge. 0)
if (finished .ne. 1 .and. changed .ne. 1) then
        if ( ar(i) .lt. (n-1)-(k-1)+i ) then
        ;Increment ar(i)
        ar(i)=ar(i)+1

                if ( i .lt. k-1 ) then
                        ;Turn elements after it into a linear sequence
                        do j=i+1,k-1,1
                                ar(j) = ar(j-1) + 1
                        end do
                end if
        changed=1
        end if
else
break
end if
i=i-1
end do

return(ar)
end


undef("nchoosek")
function nchoosek(n,k)
begin

;Initializes from 0 to k-1
ini=ispan(0,k-1,1)

;Computes the total number of combinations using the gamma function for computing factorials (gamma(x)=(x-1)!)
ncomb = gamma(n+1)/(gamma(k+1)*gamma(n-k+1))

;Preallocates the matrix
combmat=new((/round(ncomb,3),k/),integer)
;Fills the matrix using nextcombination
combmat(0,:) = ini
do i=1,round(ncomb,3)-1
; The obvious line here would be:
; combmat(i,:) = nextcombination(n,k,combmat(i-1,:))
; but it doesn't work for some reason, so a dummy was used
dummy=combmat(i-1,:)
combmat(i,:) = nextcombination(n,k,dummy)


end do

return(combmat)
end

