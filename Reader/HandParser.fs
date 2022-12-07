module HandParser

open Types
open Tokens

let (|NewSeq|_|) toks = match toks with | New::Format(ft)::Limit(lt)::Const(sb)::Const(bb)::t -> Some(ft, lt, sb, bb) | _ -> None
let (|BetSeq|_|) toks = match toks with 
                        | Bet::Name(player)::Const(amount)::t -> Some(player, Bet.Bet(amount)) 
                        | Bet::Name(player)::Fold::t -> Some(player, Bet.Fold) 
                        | _ -> None
let (|JoinSeq|_|) toks = match toks with | Join::Name(player)::Name(playerLeft)::Const(startingStack)::t -> Some(player, playerLeft, startingStack) | _ -> None
let (|LeaveSeq|_|) toks = match toks with | Leave::Name(player)::t -> Some(player) | _ -> None
let (|BlindsSeq|_|) toks = match toks with | Blinds::Const(sb)::Const(bb)::t -> Some(sb, bb) | _ -> None


let lookahead (toks: Token list) (n: int) = toks.[0..(n-1)]

let rec parse (toks: Token list) (gs: GameState) (h: Hand list) = 
    match toks with 
    | [] -> h
    | NewSeq(ft, lt, sb, bb) -> parseNewPlayers (toks |> List.skip 5) gs h ft lt sb bb [] Map.empty
    | Format(ft)::t -> parse t { gs with GameType = (let (f, l) = gs.GameType in (ft, l)) } h
    | Limit(lt)::t -> parse t { gs with GameType = (let (f, l) = gs.GameType in (f, lt)) } h
    | Hand::t -> parseHand t gs h (new HandBuilder(gs, None, None, None, Bet.Bet(0m)))
    | JoinSeq(p, pl, ss) -> parse (toks |> List.skip 4) (gs.Join(p, pl, ss)) h
    | LeaveSeq(p) -> parse (toks |> List.skip 2) (gs.Leave(p)) h
    | BlindsSeq(sb, bb) -> parse (toks |> List.skip 3) { gs with Blinds = SmallBig (sb, bb) } h
    | _ -> failwithf "Unexpected token %A in parse environment" toks.Head
and parseNew toks gs h (ft: FormatType) (lt: LimitType) (sb: decimal) (bb: decimal) (ps: string list) (ss: Map<string, decimal>) = 
    parse toks { GameType = (ft, lt); Blinds=SmallBig(sb, bb); Players=ps; Stacks=ss } h
and parseNewPlayers toks gs h ft lt sb bb ps ss = 
    match toks with
    | (Name(p))::(Const(f))::t -> parseNewPlayers t gs h ft lt sb bb (p::ps) (Map.add p f ss)
    | _ -> parseNew toks gs h ft lt sb bb (List.rev ps) ss
and parseHand toks gs h (hb: HandBuilder) = 
    match toks with
    | Name(p)::t -> parseHandCard t gs h p (hb.AddPlayer(p))
    | _ -> failwithf "Unexpected token %A in parseHand environment" toks.Head
and parseHandCard toks gs h (p: string) (hb: HandBuilder) =
    match toks with
    | Card(c)::t -> parseHandCard t gs h p (hb.AddHoleCard(c))
    | Name(_)::t -> parseHand toks gs h hb
    | BetSeq(p, a) -> parseBet toks gs h (hb.AddBettingRound())
    | _ -> failwithf "Unexpected token %A in parseHand environment" toks.Head
and parseBet toks gs h hb  = 
    match toks with
    | [] -> parse toks (gs.MoveButtton()) (hb.Build()::h)
    | BetSeq(p, a) -> parseBet (toks |> List.skip 3) gs h (hb.Bet(p, a))
    | Deal::t -> parseDeal t gs h (hb.AddBettingRound()) []
    | _ -> parse toks (gs.MoveButtton()) (hb.Build()::h)
and parseDeal toks gs h hb (cards: Card list) = 
    match toks with
    | Card(c)::t -> parseDeal t gs h hb (c::cards)
    | Bet::t -> parseBet toks gs h (hb.Deal(cards))
    | _ -> failwithf "Unexpected token %A in parseDeal expression" toks.Head
