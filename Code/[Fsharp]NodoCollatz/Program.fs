open System
open System.Collections.Generic
open System.Diagnostics
open System.Text
open System.Numerics
open System.IO
open File1

let collatz (n: BigInteger) =
    if n % 2I = 0I then (n / 2I, "R")
    else (3I * n + 1I, "L")

let detectPattern (values: BigInteger list) =
    match values with
    | v0 :: v1 :: _ ->
        let jump = v1 - v0
        let rec check (list: BigInteger list) =
            match list with
            | a :: b :: rest -> if b - a = jump then check (b :: rest) else false
            | _ -> true
        if check values then Some(v0, jump) else None
    | _ -> None

let verifyAndPrint (vEE: BigInteger) (vEO: BigInteger) (vOE: BigInteger) (vOO: BigInteger) (vEE2: BigInteger) (vEO2: BigInteger) (vOE2: BigInteger) (vOO2: BigInteger) (currentExponent: int) =
    let total = vEE + vEO + vOE + vOO + vEE2 + vEO2 + vOE2 + vOO2
    let currentThreshold = BigInteger.Pow(BigInteger(2), currentExponent)
    if total >= currentThreshold then
        printfn "2^%d EE=%A EO=%-6A OE=%-6A OO=%-6A EE2=%-6A EO2=%-6A OE2=%-6A OO2=%-6A | Total=%A" currentExponent vEE vEO vOE vOO vEE2 vEO2 vOE2 vOO2 total
        currentExponent + 1
    else
        currentExponent

let Print (vEE: BigInteger) (vEO: BigInteger) (vOE: BigInteger) (vOO: BigInteger) (vEE2: BigInteger) (vEO2: BigInteger) (vOE2: BigInteger) (vOO2: BigInteger) =
    let total = vEE + vEO + vOE + vOO + vEE2 + vEO2 + vOE2 + vOO2
    printfn "EE=%s + %s EO=%s + %s OE=%s + %s OO=%s + %s Total=%A" 
        (vEE.ToString().PadRight(5)) (vEE2.ToString().PadRight(8))
        (vEO.ToString().PadRight(5)) (vEO2.ToString().PadRight(8))
        (vOE.ToString().PadRight(0)) (vOE2.ToString().PadRight(0))
        (vOO.ToString().PadRight(5)) (vOO2.ToString().PadRight(8))
        total

let generateTree (startNode: NodoCollatz) (levels: int) (seedsCount: int) (secondMode: bool) =
    
    let tree = List<Dictionary<string, NodoCollatz>>()
    tree.Add(Dictionary<string, NodoCollatz>(dict [("N", startNode)]))
    
    let patternHistory = HashSet<BigInteger * BigInteger>()
    let resultsHistory = Dictionary<BigInteger * BigInteger, (BigInteger * BigInteger) option * (BigInteger * BigInteger) option>()
    let mutable vEE = 0I // Even-Even
    let mutable vOO = 0I // Odd-Odd
    let mutable vEO = 0I // Even-Odd
    let mutable vOE = 0I // Odd-Even
    let mutable vEE2 = 0I
    let mutable vOO2 = 0I
    let mutable vEO2 = 0I
    let mutable vOE2 = 0I
    let mutable targetExponent = 1
    
    for q1 in 0 .. levels - 1 do
        let current = tree.[q1]
        let next = Dictionary<string, NodoCollatz>()
        Print vEE vEO vOE vOO vEE2 vEO2 vOE2 vOO2

        for kvp in current do
            let path, node = kvp.Key, kvp.Value
            let key = (node.Desplazamiento, node.Progresion)

            let processFunc (res: (BigInteger * BigInteger) option) (letter: string) =
                match res with
                | Some (d, p) ->
                    match (d % 2I = 0I, p % 2I = 0I) with
                    | (true, true)   -> vEE2 <- vEE2 + 1I
                    | (true, false)  -> vEO2 <- vEO2 + 1I
                    | (false, true)  -> vOE2 <- vOE2 + 1I
                    | (false, false) -> vOO2 <- vOO2 + 1I
                    
                    let isDup = patternHistory.Contains((d, p))
                    if not isDup then patternHistory.Add((d, p)) |> ignore
                    next.[path + letter] <- { Desplazamiento = d; Progresion = p; Ruta = path + letter; Nivel = q1 + 1; EsDuplicado = isDup }
                | None -> ()

            if secondMode then
                let d, p = key
                match resultsHistory.TryGetValue(key) with
                | true, (resR, resL) ->
                    processFunc resR "R"
                    processFunc resL "L"
                | _ ->
                    match (d % 2I = 0I, p % 2I = 0I) with
                    | (true, true)   ->
                        let newDR = d / 2I
                        let newPR = p / 2I
                        let resultR = Some(newDR, newPR)
                        processFunc resultR "R"
                        let resultL = None
                        resultsHistory.[key] <- (resultR, resultL)
                        vEE <- vEE + 1I
                    | (true, false)  ->
                        let newDR = d / 2I
                        let newPR = p
                        let resultR = Some(newDR, newPR)
                        processFunc resultR "R"
                        let newDL = (d * 3I) + (p * 3I) + 1I
                        let newPL = (p * 3I) * 2I
                        let resultL = Some(newDL, newPL)
                        processFunc resultL "L"
                        resultsHistory.[key] <- (resultR, resultL)
                        vEO <- vEO + 1I
                    | (false, true)  ->
                        printfn "OE"
                        vOE <- vOE + 1I
                    | (false, false) ->
                        let newDR = (d + p) / 2I
                        let newPR = p
                        let resultR = Some(newDR, newPR)
                        processFunc resultR "R"
                        let newDL = (d * 3I) + 1I
                        let newPL = (p * 3I) * 2I
                        let resultL = Some(newDL, newPL)
                        processFunc resultL "L"
                        resultsHistory.[key] <- (resultR, resultL)
                        vOO <- vOO + 1I
            else
                match resultsHistory.TryGetValue(key) with
                | true, (resR, resL) ->
                    processFunc resR "R"
                    processFunc resL "L"
                | _ ->
                    let mutable rVals = []
                    let mutable lVals = []
                    for i in 0 .. seedsCount - 1 do
                        let s = node.Progresion * BigInteger(i) + node.Desplazamiento
                        let v, b = collatz s
                        if b = "R" then rVals <- v :: rVals else lVals <- v :: lVals
                
                    let resR = detectPattern (List.rev rVals)
                    let resL = detectPattern (List.rev lVals)
                    resultsHistory.[key] <- (resR, resL)
                    processFunc resR "R"
                    processFunc resL "L"
        tree.Add(next)
    tree

let calculateDyadicSurreal (path: string) =
    let clean = path.Replace("N", "")
    if String.IsNullOrEmpty(clean) then "0 / 2^0"
    else
        let n = clean.Length
        let denominator = bigint.Pow(2I, n - 1)
        let mutable numerator = 0I
        if clean.[0] = 'L' then numerator <- -denominator
        else numerator <- denominator
        for i in 1 .. n - 1 do
            let weight = bigint.Pow(2I, n - 1 - i)
            if clean.[i] = 'L' then
                numerator <- numerator - weight
            else
                numerator <- numerator + weight
        let exp = n - 1
        let decimalValue = 
            if exp = 0 then 0.0 
            else float numerator / float denominator
        sprintf "%A / 2^%d %-16A" numerator (n - 1) decimalValue

[<EntryPoint>]
let main argv =
    let targetLevels = 8
    let seeds = 32
    let initialNode = { Desplazamiento = 1I; Progresion = 1I; Ruta = "N"; Nivel = 0; EsDuplicado = false }

    let sw = Stopwatch.StartNew()
    let finalRes = generateTree initialNode targetLevels seeds true
    sw.Stop()
    printfn "%d %f" targetLevels sw.Elapsed.TotalSeconds

    let sb = StringBuilder()
    let symmetrySummary = List<string>()

    for i in 0 .. finalRes.Count - 1 do
        sb.AppendLine(sprintf "\nLEVEL %d %s" i (String('-', 50))) |> ignore
        let level = finalRes.[i]
        let mutable pathsNR = 0
        let mutable pathsNLRRR = 0

        let sortedKeys = level.Keys |> Seq.sort
        let mutable duplicateCountL = 0
        for path in sortedKeys do
            let node = level.[path]
            let dyadicFraction = calculateDyadicSurreal path
            let nodeLine = sprintf "%-16s \t%s" (node.ToString()) dyadicFraction
            if path.StartsWith("NR") then
                pathsNR <- pathsNR + 1
            elif path.StartsWith("NLRRR") then
                pathsNLRRR <- pathsNLRRR + 1
            else
                if (node.EsDuplicado) then duplicateCountL <- duplicateCountL + 1
                sb.AppendLine(nodeLine) |> ignore
        
        if pathsNLRRR > 0 then
            let msg = sprintf "LEVEL %d-2 (NLRRR Symmetry) Paths: %d" i pathsNLRRR
            sb.AppendLine(msg) |> ignore
        if pathsNR > 0 then
            let msg = sprintf "LEVEL %d-1 (NR Symmetry) Paths: %d" i pathsNR
            sb.AppendLine(msg) |> ignore
        symmetrySummary.Add(sprintf "LEVEL %-4d NR: %-8d NLRRR: %-8d NL*: %-8d Total: %-8d" i pathsNR pathsNLRRR duplicateCountL (duplicateCountL + pathsNR + pathsNLRRR))
    
    printfn "%A" sb
    sb.AppendLine("\n" + String('=', 60)) |> ignore
    sb.AppendLine("SYMMETRY SUMMARY:") |> ignore
    for line in symmetrySummary do
        sb.AppendLine(line) |> ignore
    
    for line in symmetrySummary do printfn "%s" line
    0
