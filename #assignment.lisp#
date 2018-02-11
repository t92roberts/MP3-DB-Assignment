;; Tom Roberts

(defun natP (n)
  "Checks that the input is a natural number"
  (assert (integerp n) (n)
	  "~s invalid: must be an integer" n)
  (assert (> n 0) (n)
	  "~s invalid: must be greater than 0" n)
  t
)

(defun songP (song)
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

(defun mp3databaseP (mp3database)
  "Checks that the database provided is a list of well-formed songs"
  (assert (listp mp3database) (mp3database)
	  "~s invalid: input must be a list of songs" mp3database)
  
  (mp3databaseRec mp3database)
)

(defun mp3databaseRec (mp3database)
  "Recursively checks if the list is a database of well-formed songs"
  (if (endp mp3database) ; end of mp3 database
      t
    (if (songp (first mp3database)) ;  valid song
      (mp3databaseRec (rest mp3database)) ; check next list atom
    nil))
)

(defun idInDbP (id mp3database)
  "Checks if the given ID is a natural number and the database given is well-formed, then begins to search to find the ID"
  (if (and (natP id) (mp3databaseP mp3database))
      (idInDBRec id 0 mp3database))
)

(defun idInDbRec (id pos mp3database)
  "Recursively checks the database for the given ID"
  (if (endp mp3database)
      nil
    (if (= id (first (first mp3database))) ;; found id in mp3 db
	pos ;; return position in list
      (idInDbRec id (+ pos 1) (rest mp3database)))) ;; move to next atom
)

(defun insertSong (newSong mp3database)
  "Adds a well-formed song to the end of a database of well-formed songs"
  (if (and (songp newSong) (mp3databasep mp3database)) ; valid new song & song database
      (if (not (idInDbP (first newSong) mp3database)) ; check if ID is already in the database
	  (append mp3database (list newSong)))) ; if not, add new song to end of database
)

(defun removeSong (id mp3database)
  "Checks if the song is in the database and removes it"
  (if (idInDbP id mp3database)
      (removeSongRec (idInDbP id mp3database) mp3database)
    nil)
)

(defun removeSongRec (pos mp3database)
  "Recursively moves through the database of MP3s and removes to song in the specified position"
  (cond ((endp mp3database) nil) ;; reached end of mp3 db
	((= pos 0) (rest mp3database)) ;; at position of song being deleted

	;; construct a list of elements not being removed to be returned at the end
	(t (cons (car mp3database)
		 (removeSongRec (- pos 1) (rest  mp3database))))
	)
)

(defun updateRate (id newRate mp3database)
  "Check valid inputs and start recursion"
  (if (idInDbP id mp3database) ;; check if ID is in mp3 db
      (updateRateRec (idInDbP id mp3database) newRate mp3database)
    )
)

(defun updateRateRec (pos newRate mp3database)
  "Recursively moves through the database of MP3s constructing a new list with the edited atom"
  (cond ((endp mp3database) nil) ;; reached end of mp3 db

	;; at list element being updated
	;; construct a list of the edited song and the rest of the mp3 db
	((= pos 0) (cons (replaceNth 5 newRate (first mp3database)) (rest mp3database)))

	;; else construct a list of the unchanged db so far to be returned
	(t (cons (car mp3database)
		 (updateRateRec (- pos 1) newRate (rest mp3database)))))
)

(defun replaceNth (pos newAtom list)
  "Recursively move through atoms in a list and replace the atom in the specified position with the new data"
  (cond ((endp list) nil) ;; reached end of list
	((= pos 0) (cons newAtom (rest list))) ;; at the atom being changed

	;; else construct a list of the other unchanged atoms
	(t (cons (car list)
		 (replaceNth (- pos 1) newAtom (rest list)))))
)

(defun sortyear (mp3database)
  "Sorts a list of songs by their release year (ascending)"
  (if (mp3databasep mp3database)
      (sort mp3database #'< :key #'fourth))
)

(defun sortartist (mp3database)
  "Sorts a list of songs alphabetically by their artist (ascending)"
  (if (mp3databasep mp3database)
      (sort mp3database #'string< :key #'third))
)

(defun sortlength (mp3database)
  "Sorts a list of songs by the track length (ascending)"
  (if (mp3databasep mp3database)
      (sort mp3database #'< :key #'fifth))
)

(defun sortrating (mp3database)
  "Sorts a list of songs by their rating (descending)"
  (if (mp3databasep mp3database)
      (sort mp3database #'> :key #'sixth))
)

(defun longestSong (mp3database)
  "Checks valid database is given and begins recursion"
  (if (mp3databasep mp3database)
      (longestSongRec mp3database))
)

(defun longestSongRec (mp3database)
  "Recursively checks each song length to the next song's and retains the longest length song at the start of the list, until it reaches the end of the database"
  (cond ((= (length mp3database) 1) (first mp3database)) ;; only 1 song in list
	((< (fifth (first mp3database)) (fifth (second mp3database))) ;; 1st song's length < 2nd song's...
	 (longestSong (rest mp3database))) ;; ignore 1st song, continue with rest of mp3 db

	;; else 1st song's length > 2nd song's, ignore 2nd song and continue searching
	(t (longestSong (cons (first mp3database) (rest (rest mp3database))))))
)

(defun allArtistsSongs (artist mp3database)
  "Returns a list of all songs in the mp3 db by the given artist"
  (assert (stringp artist) (artist)
	  "~s must be a string" artist)

  (if (mp3databaseP mp3database)
      (allArtistsSongsRec artist mp3database)
    nil)
)

(defun allArtistsSongsRec (artist mp3database)
  "Recursively checks if each song in the db matches the artist given and constructs a list of matches"
  (if (endp mp3database) ;; reached end of mp3 db
      nil
    (if (string= (third (first mp3database)) artist) ;; if song artist matches the one specified
	;; add song to list of matched songs, continue through mp3 db
	(cons (first mp3database) (allArtistsSongsRec artist (rest mp3database)))
      (allArtistsSongsRec artist (rest mp3database)))) ;; no match, continue through mp3 db
)

(defun songsDateRange (startYear endYear mp3database)
  "Checks valid inputs then finds songs between the given dates"
  (assert (natp startYear) (startYear)
	  "~s invalid: start year must be a natural number" startYear)
  (assert (>= startYear 1900) (startYear)
	  "~s invalid: start year must be greater than/equal to 1900" startYear)
  (assert (natp endYear) (endYear)
	  "~s invalid: end year must be a natural number" endYear)
  (assert (>= endYear 1900) (endYear)
	  "~s invalid: end year must be greater than/equal to 1900" endYear)
  (assert (mp3databaseP mp3database) (mp3database)
	  "database must be a list of well-formed songs")

  (songsDateRangeRec startYear endYear mp3database)
)

(defun songsDateRangeRec (startYear endYear mp3database)
  "Recursively checks if each song in the db was released between the two given dates and constructs a list of matches"
  (if (endp mp3database) ;; reached end of mp3 db
      nil
    ;; if song date is between given dates....
    (if (and (> (fourth (first mp3database)) startYear) (< (fourth (first mp3database)) endYear))
	;; add song to list of matches, continue through mp3 db
	(cons (first mp3database) (songsDateRangeRec startYear endYear (rest mp3database)))
      ;; else continue through mp3 db
      (songsDateRangeRec startYear endYear (rest mp3database))))
)

(defun maxSongRating (mp3database)
  "Checks valid database and starts the recursion"
  (if (mp3databasep mp3database)
      (maxSongRatingRec mp3database))
)

(defun maxSongRatingRec (mp3database)
  "Recursively builds a list of songs with the maximum rating"
  (if (endp mp3database) ;; reached end of the mp3 db
      nil
    (if (= (sixth (first mp3database)) 1) ;; song has the max rating
	;; add song to list of matches, continue through mp3 db
	(cons (first mp3database) (maxSongRatingRec (rest mp3database)))
      ;; else continue through mp3 db
      (maxSongRatingRec (rest mp3database))))
)

;'((1 "Somebody to love" "Queen" 1976 296 5/5)
;(2 "A day in the life" "The Beatles" 1967 335 4/5)
;(3 "Hey Brother" "Avicii" 2013 254 3/5)
;(4 "I want to break free" "Queen" 1984 200 5/5))
