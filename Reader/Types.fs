module Types

type FormatType = 
    | Omaha
    | Holdem

type LimitType = 
    | PotLimit
    | NoLimit

type Rank = 
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6
    | Seven = 7
    | Eight = 8
    | Nine = 9
    | Ten = 10
    | Jack = 11
    | Queen = 12
    | King = 13
    | Ace = 14

type Suit = 
    | Hearts
    | Diamonds
    | Clubs
    | Spades

type Bet =
    | Bet of decimal
    | Fold
    | Check
    | Call of decimal
    | Raise of decimal

type Card = 
    | Known of Rank * Suit
    | Unknown

type GameType = FormatType * LimitType

type Deal = Card list

type Hole = Card list

type Blinds = decimal * decimal

type Ante = decimal

type GameState  = 
    { GameType: GameType 
      Ante: Ante
      Blinds: Blinds
      Players: string list 
      Stacks: Map<string, decimal> }
    static member Empty = { GameType=(Holdem, NoLimit); Ante = 0m; Blinds=Blinds(0.5m, 1m); Players=[]; Stacks=Map.empty; }
    member this.MoveButtton() = { this with Players = match this.Players with | h::t -> t @ [h] | _ -> failwith "There are less than two people in the game" }
    member this.Bet(player: string, amount: decimal) = 
        { this with Stacks = Map.change player (fun x -> match x with | Some(f) -> Some(f + amount) | None -> failwithf "Player %s is not seated at the table" player) this.Stacks }
    member this.Join(player: string, playerToLeft: string, stack: decimal) = 
        let rec addPlayerInOrder (po: string list) (pn: string list) = 
            match po with
            | h::[] when h = playerToLeft -> (pn |> List.rev)@[h;player]
            | h::t when h = playerToLeft -> (pn |> List.rev)@(h::(player::t))
            | h::t -> addPlayerInOrder t (h::pn)
            | _ -> failwithf "Player %s is not seated at the table" playerToLeft
        in 
        { GameType = this.GameType; Ante = this.Ante; Blinds = this.Blinds; Players = addPlayerInOrder this.Players []; Stacks = Map.add player stack this.Stacks; }
    member this.Leave(player: string) =
        let rec removePlayer (po: string list) (pn: string list) = 
            match po with 
            | h::[] when h = player -> pn |> List.rev
            | h::t when h = player -> (pn |> List.rev)@t
            | h::t -> removePlayer t (h::pn)
            | _ -> failwithf "Unexpected leave: %s" player
        in
        { GameType = this.GameType; Ante = this.Ante; Blinds = this.Blinds; Players = removePlayer this.Players []; Stacks = Map.remove player this.Stacks; }
    override this.ToString() =
        let (format, limit) = this.GameType
        let (small, big) = this.Blinds
        let header = sprintf "\nFormat: %A\nLimit: %A\nAnte: %M\nBlinds: %M, %M\nPlayers Clockwise From Dealer: " format limit this.Ante small big
        let rec printPlayers out players =
            match players with
            | [] -> out
            | h::[] -> printPlayers (out + h + "(" + (this.Stacks.[h] |> string) + ")\n") []
            | h::t -> printPlayers (out + h + "(" + (this.Stacks.[h] |> string) + "), ") t
        printPlayers header this.Players


type BettingRound = 
    { BettingOrder: string list
      Bets: Map<string, Bet list> }

      override this.ToString() =
        let updateMapFunc x = 
            match x with 
            | Some([]) -> None
            | Some(h::[]) -> None
            | Some(h::t) -> Some(t)
            | None -> failwith "Malformed BettingRound"
        let updateMap m k = m |> Map.change k updateMapFunc
        let rec displayBets (out: string) (l: string list) (m: Map<string, Bet list>) = 
            match l with
            | [] -> out
            | h::t -> if m |> Map.containsKey h then 
                        let hBets = m.[h] in
                        printfn "%A" hBets
                        match hBets with
                        | Fold::[] -> displayBets (out + h + " folds\n") t (updateMap m h)
                        | Check::[] -> displayBets (out + h + " checks\n") t (updateMap m h)
                        | (Call a)::[] -> displayBets (out + h + " calls " + (a |> string) + "\n") t (updateMap m h)
                        | (Raise a)::[] -> displayBets (out + h + " raises to " + (a |> string) + "\n") t (updateMap m h)
                        | Check::tt -> displayBets (out + h + " checks\n") (t@[h]) (updateMap m h)
                        | (Call a)::tt -> displayBets (out + h + " calls " + (a |> string) + "\n") (t@[h]) (updateMap m h)
                        | (Raise a)::tt -> displayBets (out + h + " raises to " + (a |> string) + "\n") (t@[h]) (updateMap m h)
                        | _ -> failwith "Cannot display bet of type Bet"
                      else displayBets out t m
        in displayBets "" (this.BettingOrder.Tail@[this.BettingOrder.Head]) this.Bets


type Hand (state: GameState, holes: Card list list, board: Card list list, bettingRounds: BettingRound list) =
    member this.state = state
    member this.board = board
    member this.holes = holes
    member this.bettingRounds = bettingRounds

    override this.ToString() =
        let rec displayList l out = 
            match l with
            | [] -> out
            | h::t -> displayList t (out + (h |> string) + " ")
        let rec displayHoles (l: (string * Card list) list) (out: string) =
            match l with
            | [] -> out
            | (player, holes)::[] -> displayHoles [] (sprintf "%s %s: %s\n\n" out player (displayList holes ""))
            | (player, holes)::t -> displayHoles t (sprintf "%s %s: %s\n" out player (displayList holes ""))
        let rec displayBetting (l: (Card list * BettingRound) list) out = 
            match l with
            | [] -> out
            | (deal, bets)::t -> displayBetting t (out + (displayList deal "") + "\n" + (bets |> string))
        let addBackwards a b = b + a in
        this.state
        |> string
        |> displayHoles ((Seq.zip this.state.Players this.holes) |> Seq.toList)
        |> addBackwards (this.bettingRounds.Head |> string)
        |> displayBetting ((Seq.zip this.board this.bettingRounds.Tail) |> Seq.toList)

type HandBuilder (gs: GameState, holes: (Card list list) option, board: (Card list list) option, bettingRounds: (BettingRound list) option, prevBet: Bet) = 
    member this.gs = gs
    member this.holes = holes
    member this.board = board
    member this.bettingRounds = bettingRounds
    member this.prevBet = prevBet;

    member this.AddPlayer(player) = 
        match this.holes with
        | Some h -> new HandBuilder(this.gs, Some([]::h), this.board, this.bettingRounds, this.prevBet)
        | None -> new HandBuilder(this.gs, Some([[]]), this.board, this.bettingRounds, this.prevBet)
    member this.AddHoleCard(card) = 
        match this.holes with
        | Some (h::t) -> new HandBuilder(this.gs, Some((card::h)::t), this.board, this.bettingRounds, this.prevBet)
        | _ -> failwith "Can't add hole cards - there are no players"
    member this.Deal(cards) = 
        match this.board with
        | Some b -> new HandBuilder(this.gs, holes, Some(cards::b), this.bettingRounds, this.prevBet)
        | None -> new HandBuilder(this.gs, holes, Some([cards]), this.bettingRounds, this.prevBet)
    member this.Bet(player, bet) = 
        let (newBet, newPrevBet) = match (bet, prevBet) with
                                   | (Bet(a), Bet(b)) when b = 0m -> if a = 0m then (Check, Bet(a)) else (Raise(a), Raise(a))
                                   | (Bet(a), Raise(b)) -> if a = b then (Call(a), Raise(b)) else (Raise(a), Raise(a)) // TODO: Check for invalid bets
                                   | (Fold, _) -> (bet, prevBet)
                                   | _ -> failwith "Unsupported betting mode detected"
        match this.bettingRounds with
        | Some(b::t) -> if Map.containsKey player b.Bets then
                            let newBetsMap = (b.Bets |> Map.change player (function | Some(x) -> Some(x@[newBet]) | None -> failwithf "No player with name %s" player))
                            let newBets = { b with Bets = newBetsMap }
                            new HandBuilder(this.gs, this.holes, this.board, Some(newBets::t), newPrevBet) 
                        else
                            let newBetsMap = b.Bets |> Map.add player [newBet]
                            let newBets = { b with Bets = newBetsMap }
                            in new HandBuilder(this.gs, this.holes, this.board, Some(newBets::t), newPrevBet)
        | _ -> failwith "No betting rounds have been initiated"
    member this.AddBettingRound() = 
        let newBets = match this.bettingRounds with
                      | Some(h::t) -> { BettingOrder = this.gs.Players; Bets = Map.empty }::(h::t)
                      | Some([]) | None -> { BettingOrder = this.gs.Players; Bets = Map.empty }::[]
        new HandBuilder(this.gs, this.holes, this.board, Some(newBets), Bet(0m))
    member this.Build() = 
        let unwrap = (function | Some (x) -> x | _ -> failwith "Cannot build with None")
        let hs = this.holes |> unwrap |> List.rev
        let bd = this.board |> unwrap |> List.rev
        let br = this.bettingRounds |> unwrap |> List.rev
        new Hand(this.gs, hs, bd, br)


