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
let aTree = BuildRootedTree labels
let aTree' = BuildRootedTree labels' 
let aTree'' = BuildRootedTree labels''

printfn "aTree = %A" aTree
printfn "aTree' = %A" aTree'
printfn "aTree'' = %A" aTree''

let incr x = 
    x := !x + 1
    !x

let Layout N (t: RootedTree<int>) = 
    let l = Array.zeroCreate<int>(N)
    let order = ref -1
    let rec preorderGRD (d: int) (t: RootedTree<int>) =                
        //printfn "%d,%d" !order d
        match t with
        | None -> ()
        | Leaf _ -> 
            l.[incr order] <- d
        | RootedTree (_, children) ->
            l.[incr order] <- d
            for child in children do
                preorderGRD (d + 1) child
    
    preorderGRD 0 t
    l

printfn "layout of aTree is: %A" (Layout 7 aTree)
printfn "layout of aTree' is: %A" (Layout 7 aTree')

let IsEachElemNumEqual max (l1: array<int>) (l2: array<int>) =
    let rec compare i1 = 
        if i1 >= max then true
        else
            let arrayi1 = l1 |> Array.filter(fun x -> x = l1.[i1])
            let arrayi2 = l2 |> Array.filter(fun x -> x = l2.[i1])

            if arrayi1.Length <> arrayi2.Length then false
            else compare (i1 + 1)
    compare 0

let IsLayoutEqual (l1: array<int>) (l2: array<int>) =
    let N1, N2 = l1.Length, l2.Length
    let max1, max2 = Array.max l1, Array.max l2
    if N1 <> N2 then false
    elif max1 <> max2 then false
    elif 
        

