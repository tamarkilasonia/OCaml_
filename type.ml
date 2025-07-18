type person = {
  given : string
  sur : string
  age : int
}
(*This says:
 A person has:
given → a string (first name)
sur → a string (last name)
age → an integer*)

let paul = { given = "Paul"; sur = "Meier"; age = 24 }

let hans = { sur = "kohl"; age = 23; given = "hans" }


type color = Diamonds | Hearts | Gras | Clubs

type value = Seven | Eight | Nine | Jack | Queen | King |
 Ten | Ace