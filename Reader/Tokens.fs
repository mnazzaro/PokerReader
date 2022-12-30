module Tokens

open System.Text.RegularExpressions

open Types

open StringConversions

type Token =
| Const of decimal
| Card of Card
| Format of FormatType
| Limit of LimitType
| Bet
| Hand
| Join
| Leave
| New
| Deal
| Name of string
| Fold
| Blinds
| Ante

let (|ConstR|_|) i = if Regex(@"^\d+(\.\d+)?$", RegexOptions.Compiled).IsMatch(i) then Some(i) else None
let (|CardR|_|) i = if Regex(@"^((A|K|Q|J|T|[2-9])(c|s|d|h)|q)$", RegexOptions.Compiled).IsMatch(i) then Some(i) else None
let (|FormatR|_|) i = if Regex(@"^(omaha|holdem)$", RegexOptions.Compiled).IsMatch(i) then Some(i) else None
let (|LimitR|_|) i = if Regex(@"^(pl|nl)$", RegexOptions.Compiled).IsMatch(i) then Some(i) else None
let (|BetR|_|) i = if Regex(@"^bet$", RegexOptions.Compiled).IsMatch(i) then Some() else None
let (|HandR|_|) i = if Regex(@"^hand", RegexOptions.Compiled).IsMatch(i) then Some() else None
let (|JoinR|_|) i = if Regex(@"^join$", RegexOptions.Compiled).IsMatch(i) then Some() else None
let (|LeaveR|_|) i = if Regex(@"^leave$", RegexOptions.Compiled).IsMatch(i) then Some() else None
let (|NewR|_|) i = if Regex(@"^new$", RegexOptions.Compiled).IsMatch(i) then Some() else None
let (|DealR|_|) i = if Regex(@"^deal$", RegexOptions.Compiled).IsMatch(i) then Some() else None
let (|NameR|_|) i = if Regex(@"^\$(.+)\$$", RegexOptions.Compiled).IsMatch(i) then Some(i) else None
let (|FoldR|_|) i = if Regex(@"^f$", RegexOptions.Compiled).IsMatch(i) then Some() else None
let (|BlindsR|_|) i = if Regex(@"^blinds$", RegexOptions.Compiled).IsMatch(i) then Some() else None
let (|AnteR|_|) i = if Regex(@"^ante$", RegexOptions.Compiled).IsMatch(i) then Some() else None


let GetNameStr (i: string) = i.[1..(i.Length - 2)]
let GetFormatType (i: string) = match i with | "omaha" -> Omaha | "holdem" -> Holdem | _ -> failwithf "Not a valid format type: %s" i
let GetLimitType (i: string) = match i with | "pl" -> LimitType.PotLimit | "nl" -> NoLimit | _ -> failwithf "Not a valid limit type: %s" i

let Tokenize i = 
    match i with
    | ConstR a -> Const(a |> decimal)
    | CardR a -> Card(a |> StringToCard)
    | FormatR a -> Format(a |> GetFormatType)
    | LimitR a -> Limit(a |> GetLimitType)
    | BetR -> Bet
    | HandR -> Hand
    | JoinR -> Join
    | LeaveR -> Leave
    | NewR -> New
    | DealR -> Deal
    | NameR a -> Name(a |> GetNameStr)
    | FoldR -> Fold
    | BlindsR -> Blinds
    | AnteR -> Ante
    | _ -> failwithf "Undefined input sequence %s" i



