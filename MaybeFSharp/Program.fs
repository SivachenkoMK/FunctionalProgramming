// 4; 7; 2

// 1 u(x)

let u1 x =
    if x + 32.0 >= 0.0 then Some (1.0 / sqrt (x + 32.0))
    else None

let u2 x =
    let value = x * x - 32.0
    if value > 0.0 && value <> 1.0 then Some (1.0 / log10 value)
    else None

let u3 x =
    if x > 0.0 then
        let logX = log10 x
        let denominator = 32.0 * x + logX
        if denominator <> 0.0 then Some (1.0 / denominator)
        else None
    else None

printfn "%A" "Unary Maybe-functions:"
printfn "%A" (u1 4.0)
printfn "%A" (u2 7.0)
printfn "%A" (u3 1)

// 2 u1(u2(u3(x)))

let composite x =
    match u3 x with
    | Some result3 ->
        match u2 result3 with
        | Some result2 ->
            match u1 result2 with
            | Some result1 -> Some result1
            | None -> None
        | None -> None
    | None -> None

printfn "%A" "Without do (|>) notation:"
printfn "%A" (composite 10)

let compositeChain x =
    u3 x
    |> Option.bind u2
    |> Option.bind u1

printfn "%A" "With do (|>) notation:"
printfn "%A" (compositeChain 10)
printfn "%A" (compositeChain 1.0)

// 3 v(x, n)

let v x n =
    if x > 0.0 then
        let logX = log10 x
        let denominator = n * x + logX
        if denominator <> 0.0 then Some (1.0 / denominator)
        else None
    else None

printfn "%A" "Binary Maybe-function:"
printfn "%A" (v 10.0 3.2)
printfn "%A" (v 0.0 1.0)
printfn "%A" (v 10.0 -1.0)
printfn "%A" (v 1.0 (-1.0 / log10 1.0))

// 4 v(u1(x), u2(x))

let superposition x =
    match u1 x with
    | Some u1Result ->
        match u2 x with
        | Some u2Result -> v u1Result u2Result
        | None -> None
    | None -> None

printfn "%A" "Without do (Options) notation:"
printfn "%A" (superposition 100.0)
printfn "%A" (superposition 5.0)

let superpositionChain x =
    Option.bind (fun u1Result ->
        Option.bind (fun u2Result ->
            v u1Result u2Result
        ) (u2 x)
    ) (u1 x)

printfn "%A" "With do (Options) notation:"
printfn "%A" (superpositionChain 100.0)
printfn "%A" (superpositionChain 5.0)