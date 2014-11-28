#use "/course/cs017/src/ocaml/CS17setup.ml";;

type regexp_element =
    | Lit of string
    | One
    | Any;;
Lit "example";;
One;;
Any;;

type response_element =
    | Text of string
    | Place of int;;
Text "example";;
Place 5;;

type rule =
    | Rule of regexp_element list*response_element list;;
Rule ([Lit ("I") ; Any ; Lit ("my") ; Any],
      [ Text ( " Why do you " ); Place (1) ; Text ("your") ; Place (2) ; Text ("?")]);;
Rule ([Lit ("I") ; Any ; Lit ("hate") ; Lit ("you")],
      [Text ("Why do you") ; Place(1) ; Text("hate me?")]);;
Rule ([Any],
      [Text ("What's on your mind?")]);;


let eliza_rules = [Rule([Lit ("I") ; Lit ("like") ; Any],
                        [Text ("I also like") ; Place (1) ; Text ("! Tell me about") ; Place (1)]) ;
                   Rule([Lit ("I") ; Lit ("miss") ; Any],
                        [Text ("Describe") ; Place (1) ; Text ("in one word.")]) ;
                   Rule([Lit ("I'm") ; Any  ; Lit ("at") ; One],
                        [Text ("Why are you") ; Place (1) ; Text ("at") ; Place (2)]) ;
                   Rule([Any ; Lit("aunt") ; Any],
                        [Text("Don't talk about your aunt like that")]) ;
                   Rule([Lit("I'm") ; One],
                        [Text("Who gave you the right to be so") ; Place (1) ; Text("?")]) ;
                   Rule([ One ; One ; One ; Any],
                        [ Place (3) ; Place (1) ; Place (4) ; Place (2)]) ;
                   Rule([Lit("What?")],
                        [Text("How do you not understand? You go to Brown!")]) ;
                   Rule([One],
                        [Text ("Are you sure?")]) ;
                   Rule([Any],
                        [Text ("Oh?  Tell me more about yourself ;)")])];;


(* Inputs: slist, a list of strings
 *         reglist, a list of regexp_elements*)
(* Output: true if it is possible to match slist to the list of regexp_elements using the given rules, false otherwise*)
let rec regexp_match (slist : string list) (reglist : regexp_element list) : bool =
  match slist, reglist with
    | [], [] ->  true
    | [], head::tail ->
        (match head with
          |(One|Lit _) -> false
          |Any -> regexp_match [] tail)
    | head::tail, [] -> false
    | head::tail, head2::tail2 ->
        (match head, head2 with
          | _, One -> regexp_match tail tail2
          | thing, Lit word ->
              if (thing = word)
              then regexp_match tail tail2
              else false
          | _, Any ->
              regexp_match slist tail2 || regexp_match tail reglist);;

(*check_expect (regexp_match [] []) true;;
  check_expect (regexp_match [] [One ; Lit("17")]) false;;
  check_expect (regexp_match [] [Any ; Any ; Any ; Any ; Any]) true;;
  check_expect (regexp_match ["cs" ; "17" ; "rocks"] []) false;;
  check_expect (regexp_match ["cs" ; "17" ; "rocks"] [Lit("cs") ; Lit("17")])
  false;;
  check_expect (regexp_match ["cs" ; "17" ; "rocks"] [Lit("cs") ; Lit("17") ; One])
  true;;
  check_expect (regexp_match ["cs" ; "17" ; "rocks" ; "my" ; "socks";"off"] [Lit("cs") ; Lit("17") ; Any]) true;;
  check_expect (regexp_match ["I" ; "hate" ; "to" ; "hate" ; "you"]
  [Lit("I") ; Any ; Lit("hate") ; Lit("you")]) true;;
*)

(* Inputs: slist, a list of strings
 *         reglist, a list of regexp_elements*)
(* Output: a list of string lists, each string list cooresponding to a regexp_elements in reglist, in order, following the given rules*)
let rec extract (slist : string list) (reglist : regexp_element list) : string list list =
  match slist, reglist with
    | _, [] -> []
    | [], head::tail ->
        (match head with
          |(One|Lit _) -> failwith "error: extract should only be called on a list of strings that matches the regexp!"
          |Any -> []:: (extract [] tail))
    | head::tail, head2::tail2 ->
        (match head2 with
          | (One|Lit _) ->
              if regexp_match (head::[]) (head2::[])
              then 
                (match head2 with
                  | One -> [head]::(extract tail tail2)
                  | Lit _ -> extract tail tail2)
              else failwith "error: extract should only be called on a list of strings that matches the regexp!"
          | Any ->
              (match tail2 with
                | [] -> [slist]
                | hd::tl ->
                    match hd with
                      | Any -> []::(extract slist tail2)
                      | (One|Lit _) ->
                          if regexp_match slist tail2
                          then [] :: extract slist tail2
                          else ([head] @ (List.hd (extract tail reglist))) :: (List.tl (extract tail reglist))));;


(*check_expect (extract ["cs" ; "17" ; "is" ; "really" ; "fun"] []) [];;

  check_expect (extract ["cs" ; "17" ; "is" ; "really" ; "fun"]
  [Lit("cs") ; One ; Lit("is") ; One ; Lit("fun")])
  [["17"]; ["really"]] ;;

  check_expect (extract ["The" ; "right" ; "answer" ; "is" ; "right"]
  [Any ; Lit("right") ; Any])
  [["The"]; ["answer"; "is"; "right"]];;

  check_error (fun x -> (extract ["I"; "am" ; "not" ; "me" ; "today"]
  [Lit("I") ; Any ; Lit("not") ; One])) "error: extract should only be called on a list of strings that matches the regexp!";;

  check_error (fun x -> (extract []
  [Lit("I") ; Any ; Lit("not") ; One])) "error: extract should only be called on a list of strings that matches the regexp!";;

  check_expect (extract ["I" ; "hate" ; "to" ; "hate" ; "you"]
  [Lit("I") ; Any ; Lit("hate") ; Lit("you")])
  [["hate"; "to"]];;

  check_expect (extract ["I" ; "hate" ; "to" ; "hate" ; "you"]
  [Lit("I") ; Any ; One ; Lit("you")])
  [["hate"; "to"] ; ["hate"]];;

  check_expect (extract ["I" ; "hate" ; "to" ; "hate" ; "you"]
  [Lit("I") ; Any ; Any ; Lit("you")])
  [[] ; ["hate"; "to" ; "hate"]];;*)

(* Inputs: slistlist, a list of lists of strings
 *         template, a list of response_elements*)
(* Output: a list of strings, built using the response_elements in template and referring to slistlist to fill in Places*)
let rec make_response (slistlist : string list list) (template : response_element list) : string list =
  match template with
    | [] -> []
    | head::tail ->
        (match head with
          | Text words -> words::(make_response slistlist tail)
          | Place n -> (List.nth slistlist (n-1))@(make_response slistlist tail));;

(*check_expect (make_response [["Eric"] ; ["Alex"]]
  [Text("Why was") ; Place(2) ; Text("hit by") ; Place(1) ; Text("?")])
  ["Why was"; "Alex"; "hit by"; "Eric"; "?"];;

  check_expect (make_response [["cookies" ; "and" ; "cakes"]]
  [Text("What do you like about") ; Place(1) ; Text("?")])
  ["What do you like about"; "cookies"; "and"; "cakes"; "?"];;

  check_expect (make_response [["Eric"] ; ["Alex"]] []) [];;*)



(* Inputs: slist, a list of strings
 *         rulelist, a list of rules*)
(* Output: looks through rulelist until it finds a rule that matches slist, and then extracts the correct strings and uses make_response*)
let rec eliza_respond (slist : string list) (rulelist : rule list) : string list =
  match rulelist with
    | [] -> failwith "error: need to input a non-empty rulelist"
    | Rule (regexps, template)::tail -> 
        if regexp_match slist regexps
        then make_response (extract slist regexps) template
        else eliza_respond slist tail;;



(*check_expect (eliza_respond ["I" ; "like" ; "my" ; "cat"] eliza_rules)
  ["I also like"; "my"; "cat"; "! Tell me about"; "my"; "cat"] ;;
  check_expect (eliza_respond ["I" ; "like" ; "blueberry" ; "pie"] eliza_rules)
  ["I also like"; "blueberry"; "pie"; "! Tell me about"; "blueberry"; "pie"];;
  check_expect (eliza_respond ["I" ; "miss" ; "blueberry" ; "pie"] eliza_rules)
  ["Describe"; "blueberry"; "pie"; "in one word."];;
  check_expect (eliza_respond ["Blue"] eliza_rules)
  ["Are you sure?"];;
  check_expect (eliza_respond a["I'm" ; "really" ; "bad" ; "at" ; "sports"] eliza_rules) ["Why are you"; "really"; "bad"; "at"; "sports"];;
  check_expect (eliza_respond ["I" ; "love" ; "my" ; "aunt"] eliza_rules)
  ["Don't talk about your aunt like that"];;
  check_expect (eliza_respond ["I'm" ; "happy"] eliza_rules)
  ["Who gave you the right to be so"; "happy"; "?"];;
  check_expect (eliza_respond ["Hello, " ; "my" ; "name" ; "is" ; "Fred"] eliza_rules) ["name"; "Hello, "; "is"; "Fred"; "my"];;
  check_expect (eliza_respond ["What?"] eliza_rules)
  ["How do you not understand? You go to Brown!"];;
  check_expect (eliza_respond ["I'm" ; "hungry"] eliza_rules)
  ["Who gave you the right to be so"; "hungry"; "?"];;
  check_error (fun x -> eliza_respond ["I" ; "like" ; "my" ; "cat"] []) "error: need to input a non-empty rulelist";;
*)


#use "/course/cs017/src/eliza/eliza_interactions.ml";;
