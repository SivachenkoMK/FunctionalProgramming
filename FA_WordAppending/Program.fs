open System.IO
open System

type State = int
type Automaton = {
    StartState: State
    FinalStates: Set<State>
    Transitions: Map<State, Map<char, State>>
}

let rec depthFirstSearch (visitedStates : Map<char,State>) currentState (previousSymbols : char[]) automaton =
    if Set.contains currentState automaton.FinalStates then
        Some ([currentState], previousSymbols)
    else
        let nextStates = Map.find currentState automaton.Transitions
        let unvisitedStates = nextStates |> Map.filter (fun _ value -> not (Map.exists (fun _ visitedValue -> value = visitedValue) visitedStates))

        if Set.isEmpty (unvisitedStates |> Map.values |> Set.ofSeq) then
            None
        else
            let newState = Map.minKeyValue unvisitedStates
            let currentSymbol, currentState = newState
            let currentSymbols = Array.append previousSymbols [| currentSymbol |]
            match depthFirstSearch (Map.add currentSymbol currentState visitedStates) currentState currentSymbols automaton with
            | Some (path, symbols) -> Some (currentState :: path, symbols)
            | None -> None

let findPath w automaton : (State list * char array) option =
    try
        let finalState =
            Seq.fold (fun state symbol -> 
                match Map.tryFind state automaton.Transitions with
                | Some transitions -> 
                    match Map.tryFind symbol transitions with
                    | Some nextState -> nextState
                    | None -> failwith "Symbol not found in transitions"
                | None -> failwith "State not found in transitions"
            ) automaton.StartState w
        depthFirstSearch Map.empty finalState (Array.create 0 'a') automaton
    with
    | :? System.Collections.Generic.KeyNotFoundException as ex -> printfn $"Key not found: %s{ex.Message}"; None
    | :? Exception as ex -> printfn $"An error occurred: %s{ex.Message}"; None

let readAutomaton filePath =
    let lines = File.ReadAllLines filePath
    let startState = Int32.Parse lines[0]
    let finalStates = lines[1].Split ' ' |> Array.map Int32.Parse |> Set.ofArray
    let transitions =
        lines[2..]
        |> Array.map (fun line ->
            let parts = line.Split ' '
            let state = Int32.Parse parts[0]
            let symbol = parts[1][0]
            let nextState = Int32.Parse parts[2]
            state, symbol, nextState)
        |> Array.groupBy (fun (state, _, _) -> state)
        |> Array.map (fun (state, transitions) ->
            let symbolMap =
                transitions
                |> Array.map (fun (_, symbol, nextState) -> symbol, nextState)
                |> Map.ofArray
            state, symbolMap)
        |> Map.ofArray
    { StartState = startState; FinalStates = finalStates; Transitions = transitions }

let main (argv : string[]) =
    let automaton = readAutomaton argv[0]
    let w = File.ReadAllText argv[1]
    match findPath w automaton with
    | Some (_, chars) -> 
        let chars = Array.append (w.ToCharArray()) chars
        let str = String(chars)
        printfn $"Possible word: %s{str}"
    | None -> printfn "No path found"

main [| "automation.txt"; "word.txt" |]


