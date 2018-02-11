(defun natp (n)
  "Checks that the input is a natural number"
  (and (integerp n) (> n 0))
)

(defun 1songp (song)
  "Checks that the song is well formed (correct number and type of information)"
  (and (listp song) ; parameter is a list
       (= (length song) 6) ; list has correct number of atoms
       (natp (first song)) ; first atom (ID) is a natural number
       (stringp (second song)) ; second atom (song name) is a string
       (stringp (third song)) ; third atom (artist) is a string
       (natp (fourth song)) ; fourth atom (year released) is a natural number...
       (>= (fourth song) 1900) ; ... above 1900
       (natp (fifth song)) ; fifth atom (song length) is a natural number
       (rationalp (sixth song)) ; sixth atom (rating) is a rational number...
       (>= (sixth song) 0) (<= (sixth song) 1) ; ... between 0 and 1
  )
)

(defun songp (song)
  (assert (listp song) (song)
	  "~s is not a list." song)
  (assert (= (length song) 6) (length song)
	  "MP3 database requires 6 pieces of song info; ~s provided." (length song))
  (assert (natp (first song)) (first song)
	  "~s invalid: ID must be a natural number." (first song))
  (assert (stringp (second song)) (second song)
	  "~s invalid: song name must be a string." (second song))
  (assert (stringp (third song)) (third song)
	  "~s invalid: artist must be a string." (third song))
  (assert (and (natp (fourth song)) 
	       (>= (fourth song) 1900)) (fourth song)
	  "~s invalid: year released must be a natural number above 1900" (fourth song))
  t
)

(defun my-divide (numerator denominator)
  (assert (not (zerop denominator)) (numerator denominator)
	  "You can't divide ~D by ~D." numerator denominator)
  (/ numerator denominator))

