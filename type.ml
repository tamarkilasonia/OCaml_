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

let Tamro = { given = "Tamro"; sur = "Kilas"; age = 19 }

let Saba = { sur = "Hm"; age = 19; given = "Saba" }


type color = Diamonds | Hearts | Gras | Clubs

type value = Seven | Eight | Nine | Jack | Queen | King |
 Ten | Ace
