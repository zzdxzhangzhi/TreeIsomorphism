// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


open System
open System.IO
open System.Diagnostics
open System.Collections.Generic


[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    let readTreesNumber = stdin.ReadLine() |> Int32.Parse
    while not (readTreesNumber = 0) do
        printfn "readTreesNumber = %d" readTreesNumber

    0 // return an integer exit code
