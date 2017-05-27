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
        let rec BuildTree serialNum (serials: array<int>) = 
            let N = serials.Length     
            let nodeLabel = serials.[serialNum]
            let children = seq {for i in 0 .. (N - 1) do 
                                    if serials.[i] = serialNum then yield (BuildTree i serials)}
            if Seq.isEmpty children then Leaf nodeLabel
            else RootedTree(nodeLabel, children)     
        
        BuildTree 0 labels

let labels = [|-1; 3; 3; 0; 3; 0; 0|]
let labels' = [|-1; 0; 0; 6; 6; 6; 0|]
let labels'' = [|-1; 0; 0; 6; 6; 6; 0; 4; 4; 4; 7; 7|]
let labelstest1 = [|-1|]
let labelstest2 = [|-1; 0|]
let aTree = BuildRootedTree labels
let aTree' = BuildRootedTree labels' 
let aTree'' = BuildRootedTree labels''
let treetest1 = BuildRootedTree labelstest1
let treetest2 = BuildRootedTree labelstest2

printfn "aTree = %A" aTree
printfn "aTree' = %A" aTree'
printfn "aTree'' = %A" aTree''
printfn "treetest1 = %A" treetest1
printfn "treetest2 = %A" treetest2

let incr x = 
    x := !x + 1
    !x

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

printfn "layout of aTree is: %A" (Seq.toArray (Layout aTree))
printfn "layout of aTree' is: %A" (Seq.toArray (Layout aTree'))
printfn "layout of aTree'' is: %A" (Seq.toArray (Layout aTree''))
printfn "layout of treetest1 is: %A" (Seq.toArray (Layout treetest1))
printfn "layout of treetest2 is: %A" (Seq.toArray (Layout treetest2))

let IsEachElemNumEqual max (l1: array<int>) (l2: array<int>) =
    let rec compare i1 = 
        if i1 >= max then true
        else
            let arrayi1 = l1 |> Array.filter(fun x -> x = l1.[i1])
            let arrayi2 = l2 |> Array.filter(fun x -> x = l2.[i1])

            if arrayi1.Length <> arrayi2.Length then false
            else compare (i1 + 1)
    compare 0

printfn "element equal: %A" (IsEachElemNumEqual 2 (Seq.toArray (Layout aTree)) (Seq.toArray (Layout aTree')))

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

printfn "the 2 trees are layout equal: %A" (IsLayoutEqual aTree aTree')
 
let readTreesNumber = string("3") |> Int32.Parse        

