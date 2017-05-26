// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


open System
open System.IO
open System.Diagnostics
open System.Collections.Generic


type RootedTree<'T> = 
    | Leaf of 'T
    | RootedTree of 'T * seq<RootedTree<'T>>
    | None

let BuildRootedTree (labels: array<int>) =     
    if (Array.isEmpty labels) then None
    else
        let rec BuildTree serialOrder (serials: array<int>) = 
            let N = serials.Length     
            let nodeLabel = serials.[serialOrder]
            let children = seq {for i in 0 .. (N - 1) do 
                                    if serials.[i] = serialOrder then yield (BuildTree i serials)}
            if Seq.isEmpty children then Leaf nodeLabel
            else RootedTree(nodeLabel, children)     
        
        let index = Array.tryFindIndex (fun x -> x = -1) labels
        if index.IsNone then None
        else BuildTree index.Value labels

let Layout (t: RootedTree<int>) = 
    let rec preorderGRD (d: int) (t: RootedTree<int>) =
        seq {                                   
            match t with
            | Leaf _ -> yield d
            | RootedTree (_, children) ->
                 yield d
                 for child in children do
                     yield! preorderGRD (d + 1) child
            | None -> ()
        }

    preorderGRD 0 t

let IsEachElemNumEqual max (l1: array<int>) (l2: array<int>) =
    let rec compare i1 = 
        if i1 >= max then true
        else
            let arrayi1 = l1 |> Array.filter(fun x -> x = l1.[i1])
            let arrayi2 = l2 |> Array.filter(fun x -> x = l2.[i1])

            if arrayi1.Length <> arrayi2.Length then false
            else compare (i1 + 1)
    compare 0

let incr x = 
    x := !x + 1
    !x

let IsLeaf t =
    match t with
    | Leaf _ -> true
    | _ -> false

let IsEmpty t =
    match t with
    | None -> true
    | _ -> false

let getChildren (t: RootedTree<int>) =
    match t with
    | None | Leaf _ -> seq []
    | RootedTree(_, children) -> children

let rec IsLayoutEqual (t1: RootedTree<int>) (t2: RootedTree<int>) =    
    let l1, l2 = Seq.toArray (Layout t1), Seq.toArray (Layout t2)
    let N1, N2 = l1.Length, l2.Length
    let max1, max2 = Array.max l1, Array.max l2
    
    if N1 <> N2 then false
    elif max1 <> max2 then false
    elif not (IsEachElemNumEqual max1 l1 l2) then false
    else
        let children1 = getChildren t1
        let children2 = getChildren t2
        let cN1 = Seq.length children1
        let cN2 = Seq.length children2
        if cN1 <> cN2 then false
        else
            let compareWithSeq tSeq t = 
                let results = Seq.map (IsLayoutEqual t) tSeq
                if Seq.exists (fun x -> x = true) results then true
                else false
            let seqResult = Seq.map (compareWithSeq children2) children1 
            if Seq.exists (fun x -> x = false) seqResult then false
            else true


[<EntryPoint>]
let main argv = 
    try
        //printfn "%A" argv 
        let rec readInputData treesNum scenarioCount =
            //printfn "treeNum = %d" treesNum
            if treesNum = 0 then ()
                //printfn "terminated"
            else
                let treeLabelSerials = 
                    [| for i in 0 .. treesNum - 1 ->
                        let line = stdin.ReadLine().Trim()
                        line.Split(Array.empty<string>, 
                                    StringSplitOptions.RemoveEmptyEntries)
                        |> Array.map Int32.Parse                    
                    |]
                let nodeNums = treeLabelSerials |> Array.map Array.head
                let trees = treeLabelSerials |> Array.map Array.tail
                                             |> Array.map BuildRootedTree
            
                //trees |> Array.iter (printfn "tree = %A")
            
                stdout.Write(sprintf "%d: " scenarioCount)
                let treeClasses = Array.zeroCreate<int>(treesNum)
                let treeClassMax = ref treeClasses.[0]
                stdout.Write(sprintf "%d " treeClasses.[0])
                for j in 1 .. treesNum - 1 do
                    let index = Array.sub trees 0 j |> Array.tryFindIndex (IsLayoutEqual trees.[j])
                    if not index.IsNone then
                        stdout.Write(sprintf "%d " treeClasses.[index.Value])
                        treeClasses.[j] <- treeClasses.[index.Value]
                    else
                        treeClasses.[j] <- incr treeClassMax
                        stdout.Write(sprintf "%d " treeClasses.[j])        
                stdout.WriteLine()

                readInputData (stdin.ReadLine().Trim() |> Int32.Parse) (scenarioCount + 1)
    
        readInputData (stdin.ReadLine().Trim() |> Int32.Parse) 0

        0 // return an integer exit code
    with
    | ex -> 
        Console.Error.WriteLine (sprintf "*** %s" ex.Message)
        1
