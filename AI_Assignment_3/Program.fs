// Learn more about F# at http://fsharp.org

//System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ //here is how we set the current directory for F# interactive to where this file is (run without slashes ALT-ENTER)
namespace AI
module program = 
    open System
    open Utility

    [<EntryPoint>]
    let main argv =
        // run the mazes and get their results,
        let resultsTest4x4 = Utility.computeAverageScoreForSize 100 4
        let resultsTest5x5 = Utility.computeAverageScoreForSize 100 5
        let resultsTest8x8 = Utility.computeAverageScoreForSize 100 8
        let resultsTest10x10 = Utility.computeAverageScoreForSize 100 10
        printfn "4x4 avg (Score,Visited Cells):   %A \n5x5 avg (Score,Visited Cells):   %A \n8x8 avg (Score,Visited Cells):   %A \n10x10 avg (Score,Visited Cells): %A" (resultsTest4x4 |> fst) (resultsTest5x5 |> fst) (resultsTest8x8 |> fst) (resultsTest10x10 |> fst)
        0 // return an integer exit code
