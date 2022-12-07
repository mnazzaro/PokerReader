module Lexer

open Tokens
open System.IO
open System.Text.RegularExpressions

let rec Lexer (inSeq: string list) (outSeq: Token list) = 
    match inSeq with
    | [] -> outSeq |> List.rev
    | h::t -> Lexer t ((Tokenize(h)::outSeq))

let Read (fpath: string) = 
    use stream = new StreamReader(fpath)
    let wordSeq = Regex(@"\s+").Split((stream.ReadToEnd ()).Trim()) |> Seq.toList
    printfn "%A" wordSeq
    Lexer wordSeq []
    