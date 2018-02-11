(defun natp (n)
  "Checks that the input is a natural number"
  (and (integerp n) (> n 0))
)

(defun songp (song)
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

(defun assertSongP (song)
  "Checks that the song provided is well formed (correct number and type of information)"
  (assert (listp song) (song)
	  "~s is not a list." song)
  (assert (= (length song) 6) (length song)
	  "MP3 database requires 6 pieces of song info; ~s provided." (length song))
  (assert (natp (first song)) (first song)
	  "~s invalid: ID must be a natural number." (first song))
  (assert (stringp (second song)) (second song)
	  "~s invalid: song name must be a string." (second song))
  (assert (stringp (third song)) (third song)
	  "~s invalid (song: ~s): artist must be a string." (third song) (second song))
  (assert (and (natp (fourth song)) 
	       (>= (fourth song) 1900)) (fourth song)
	  "~s invalid (song: ~s): year released must be a natural number above 1900" (fourth song) (second song))
  (assert (natp (fifth song)) (fifth song)
	  "~s invalid (song: ~s): song length must be a natural number." (fifth song) (second song))
  (assert (and (rationalp (sixth song))
	       (>= (sixth song) 0)
	       (<= (sixth song) 1)) (sixth song)
	       "~s invalid (song: ~s): rating must be between 0/5 and 5/5" (sixth song) (second song))
  t
)

(defun mp3databasep (mp3database)
  "Checks that the database provided is a list of well-formed songs"
  (assert (listp mp3database) (mp3database)
	  "~s invalid: input must be a list of songs" mp3database)
  
  (mp3databaseRec mp3database)
)

(defun mp3databaseRec (mp3database)
  "Recursively checks if the list is a database of well-formed songs"
  (if (endp (first mp3database)) ; end of mp3 database
      t
    (if (songp (first mp3database)) ;  valid song
      (mp3databaseRec (rest mp3database)) ; check next list atom
    nil))
)

(defun idInDbP (id mp3database)
  "Checks if the given ID is a natural number and the database given is well-formed, then begins to search to find the ID in the database"
  (if (and (natp id) (mp3databasep mp3database))
      (idInDbRec id mp3database))
)

(defun idInDbRec (id mp3database)
  "Recursively checks the database for the given ID"
  (if (endp mp3database) ; if at the end of the database
      nil ; id not found
    (if (= id (first (first mp3database))) ; check ID to first song's ID
	(first mp3database) ; ID is in database
      (idInDbRec id (rest mp3database)))) ; not matched, recurse to next atoms in the list
)

(defun insertsong (newSong mp3database)
  "Adds a well-formed song to the end of a database of well-formed songs"
  (if (and (songp newSong) (mp3databasep mp3database)) ; valid new song & song database
      (if (not (idInDbP (first newSong) mp3database)) ; check if ID is already in the database
	  (append mp3database (list newSong)))) ; if not, add new song to end of database
)

;(defun removesongRec (id mp3database)
;  (if (=id (first mp3database))
;      (remove 
;)

(defun removesong (id mp3database)
  "Checks if the song is in the database and removes it"
  (if (idInDbP id mp3database)
      (remove (idInDbRec id mp3database) mp3database)
    nil)
)

(defun updaterate (id newRate mp3database)
  "Updates the rating of a song, if it is in the database"
  (if (idInDbP id mp3database)
      (setf (nth 5 (idInDbp id mp3database)) newRate))

      ;(nth (position (idInDbP id mp3database) mp3database)))
      ;(position (idInDbP id mp3database) mp3database))

)

(defun sortyear (mp3database)
  (if (mp3databasep mp3database)
      (sort mp3database #'< :key #'fourth))
)

(defun sortartist (mp3database)
  (if (mp3databasep mp3database)
      (sort mp3database #'string< :key #'third))
)

(defun sortlength (mp3database)
  (if (mp3databasep mp3database)
      (sort mp3database #'< :key #'fifth))
)

(defun sortrating (mp3database)
  (if (mp3databasep mp3database)
      (sort mp3database #'> :key #'sixth))
)

(defun longestsong (mp3database)
  ;(if (mp3databasep mp3database)
  
)

(defun maxOfList (list)
  (cond ((= (length list) 1) (first list))
	((< (first list) (second list)) (maxOfList (rest list)))
	(t (maxOfList (cons (first list) (rest (rest list))))))
)

;'((1 "Somebody to love" "Queen" 1976 296 5/5)
;(2 "A day in the life" "The Beatles" 1967 335 4/5)
;(3 "Hey Brother" "Avicii" 2013 254 3/5))
