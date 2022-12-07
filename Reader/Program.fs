// Learn more about F# at http://fsharp.org

open System
open Lexer
open Types
open HandParser
[<EntryPoint>]
let main argv =
    let fpath = @"C:\Users\markn\source\repos\Poker\Reader\example1.pkr"
    let a = Read fpath
    let hands = parse a GameState.Empty []
    printf "%A" (hands.Head |> string)
    0 // return an integer exit code
