open System
open System.IO
open System.Diagnostics
open System.Collections.Generic

type RootedTree<'T> = 
    |Leaf of 'T
    |RootedTree of 'T * seq<RootedTree<'T>>
    |None

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
let aTree = BuildRootedTree labels
let aTree' = BuildRootedTree labels'

printfn "aTree = %A" aTree
printfn "aTree' = %A" aTree'


