module MyLibrary

#light


//
// explode a string into a list of characters.
// Example: "cat" -> ['c'; 'a'; 't']
//
let explode(s:string) =
  [ for c in s -> c ]


//
// implode a list L of characters back into a string.
// Example: implode ['c'; 'a'; 't'] -> "cat"
//
let implode L =
  let sb = System.Text.StringBuilder()
  let ignore = List.map (fun c -> sb.Append (c:char)) L
  sb.ToString()


//
// Initialize:
//
// This function is called ONCE at program startup to initialize any
// data structures in the library.  We use this function to input the
// Scrabble dictionary and build a list of legal Scrabble words.
//
let mutable WordList = []

let Initialize folderPath =
  let alphabetical = System.IO.Path.Combine(folderPath, "alphabetical.txt")
  WordList <- [ for line in System.IO.File.ReadAllLines(alphabetical) -> line ]
  printfn "%A" (List.length WordList)


//
// possibleWords:
//
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters.  The words are returned as a list
// in alphabetical order.
//
// Example:  letters = "tca" returns the list
//   ["act"; "at"; "cat"; "ta"]
//

let rec _letterRemove letter word = 
    match word with
    | []      ->  []
    | hd::tl  when (hd = letter) -> tl
    | hd::tl  -> hd :: _letterRemove letter tl

let letterRemove letters dictionaryWord = 
    let explodedLetters = explode letters
    let dictWord = explode dictionaryWord
    
    let rec wordLoop word dictWord = 
        match word with
        | []        -> dictWord
        | hd::[]    -> _letterRemove hd dictWord
        | hd::tl    -> wordLoop tl (_letterRemove hd dictWord)
    
    let newDictWord = wordLoop explodedLetters dictWord
    (List.length newDictWord) = 0

let possibleWords letters = 
  List.filter(fun f -> letterRemove letters f) WordList

  
//
// wordsWithScores:
//
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters.  The words are then "scored"
// based on the value of each letter, and the results returned as
// a list of tuples in the form (word, score).  The list is ordered
// in descending order by score; if 2 words have the same score,
// they are ordered in alphabetical order.
//
// Example:  letters = "tca" returns the list
//   [("act",5); ("cat",5); ("at",2); ("ta",2)]

let getScore letter = 
    match letter with
    | 'a' -> 1
    | 'b' -> 3
    | 'c' -> 3
    | 'd' -> 2
    | 'e' -> 1
    | 'f' -> 4
    | 'g' -> 2
    | 'h' -> 4
    | 'i' -> 1
    | 'j' -> 8
    | 'k' -> 5
    | 'l' -> 1
    | 'm' -> 3
    | 'n' -> 1
    | 'o' -> 1
    | 'p' -> 3
    | 'q' -> 10
    | 'r' -> 1
    | 's' -> 1
    | 't' -> 1
    | 'u' -> 1
    | 'v' -> 4
    | 'w' -> 4
    | 'x' -> 8
    | 'y' -> 4
    | 'z' -> 10



let rec findScore word = 
    let newWord = explode word

    let rec _findScore newWord score = 
        match newWord with
        | []     -> score
        | hd::tl -> _findScore tl (score + (getScore hd))

    _findScore newWord 0


let wordsWithScores letters =
  let list = possibleWords letters
  let tupleList = List.map (fun x -> (x, findScore x) ) list 
  List.sortBy(fun (word, y) -> (-y, word)) tupleList

//w
// wordsThatFitPattern:
//
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters + the letters in the pattern, such
// that those words all fit into the pattern.  The results are 
// returned as a list of tuples (word, score), in descending order by
// score (with secondary sort on the word in alphabetical order).
//
// Example:  letters = "tca" and pattern = "e**h" returns the list
//   [("each",9); ("etch",9); ("eath",7)]
//
let rec contains x L = 
  match L with
  | []     -> false
  | hd::tl -> if hd = x then
                true
              else
                contains x tl


let comparePattern letters pattern dictionaryWord = 
  let list = explode letters
  let listpattern = explode pattern
  let dictWord = explode dictionaryWord
  
  let rec _compare list listpattern dictWord = 
    match dictWord, listpattern with
    | [], []           -> true
    | h1::t1, h2::t2   when h2 = '*' -> if (contains h1 list) then _compare (_letterRemove h1 list) t2 t1
                                        else false
    | h1::t1, h2::t2   -> if (h1 = h2) then _compare list t2 t1 else false 
    | _, _             -> false

  _compare list listpattern dictWord



let wordsThatFitPattern letters pattern = 
  let words = List.filter(fun f -> comparePattern letters pattern f) WordList
  let tupleList = List.map (fun x -> (x, findScore x) ) words 
  List.sortBy(fun (word, y) -> (-y, word)) tupleList
