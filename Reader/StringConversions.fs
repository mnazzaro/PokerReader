module StringConversions

open Types

let StringToRank (strRank: char) = 
    match strRank with
    | '2' -> Rank.Two
    | '3' -> Rank.Three
    | '4' -> Rank.Four
    | '5' -> Rank.Five
    | '6' -> Rank.Six
    | '7' -> Rank.Seven
    | '8' -> Rank.Eight
    | '9' -> Rank.Nine
    | 'T' -> Rank.Ten
    | 'J' -> Rank.Jack
    | 'Q' -> Rank.Queen
    | 'K' -> Rank.King
    | 'A' -> Rank.Ace
    | _ -> failwith "Undefined card rank string"

let StringToSuit (strSuit: char) = 
    match strSuit with
    | 'h' -> Hearts
    | 'd' -> Diamonds
    | 'c' -> Clubs
    | 's' -> Spades
    | _ -> failwith "Undefined card suit string"

let StringToCard (strCard: string) = 
    if strCard.Length = 2 then Card ((StringToRank strCard.[0]), (StringToSuit strCard.[1]))
    else failwith "Undefined card string"