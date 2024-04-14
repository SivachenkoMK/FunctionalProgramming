open System.IO

type Grammar = Map<string, string list list>

let epsilon = "ε"

let rec first grammar (symbol: string) =
    let key = symbol[0].ToString()
    match Map.tryFind key grammar with
    | Some rules ->
        rules
        |> List.collect (fun rule ->
            match rule with
            | [] -> [epsilon]
            | head :: _ ->
                if head = epsilon then [epsilon]
                else if Map.containsKey (head[0].ToString()) grammar then first grammar head
                else [head]
        )
    | None -> [symbol]

let firstSets grammar =
    grammar
    |> Map.map (fun key _ -> first grammar key)

let removeEpsilon (str : string) =
    let epsilonRow = str.Trim('ε');
    if (epsilonRow="") then
        "ε"
    else
    if str = epsilon then epsilon
    else str.Replace(epsilon, "")

let parseRule (rule: string) =
    rule.Split(' ')
    |> Array.map removeEpsilon
    |> Array.toList

let parseGrammarLine (line : string) =
    let parts = line.Split("->")
    let nonTerminal = parts.[0].Trim()
    let rules = parts.[1].Split('|')
                     |> Array.map (fun rule -> parseRule (rule.Trim()))
                     |> Array.toList
    (nonTerminal, rules)


let readGrammarFromFile filename =
    File.ReadLines(filename)
    |> Seq.map parseGrammarLine
    |> Map.ofSeq

let printFirstSets firsts =
    firsts
    |> Map.map (fun key value -> 
        value 
        |> List.map (fun str -> (str : string) .[0].ToString()) 
        |> List.distinct
    )
    |> Map.iter (printfn "First(%s): %A")

let grammar = readGrammarFromFile "grammar.txt"
let firsts = firstSets grammar

printFirstSets firsts