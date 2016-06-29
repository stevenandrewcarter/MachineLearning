#I @"..\packages\"
#r @"FSharp.Data.2.3.1\lib\net40\FSharp.Data.dll"

open FSharp.Data

type Data = CsvProvider<"..\Data\day.csv">
let dataset = Data.Load("..\Data\day.csv")
let data = dataset.Rows

#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx"
open FSharp.Charting

let all = Chart.Line [ for obs in data -> obs.Cnt ]

let windowedExample = 
    [1 .. 10 ]
    |> Seq.windowed 3
    |> Seq.toList

let ma n (series:float seq) = 
    series
    |> Seq.windowed n
    |> Seq.map (fun xs -> xs |> Seq.average)
    |> Seq.toList

let count = 
    data |> Seq.map (fun x -> float x.Cnt)

Chart.Combine [
    Chart.Line count
    Chart.Line (ma 7 count)
    Chart.Line (ma 30 count)
]

let baseline =
    let avg = data |> Seq.averageBy (fun x -> float x.Cnt)
    data |> Seq.averageBy (fun x -> abs (float x.Cnt - avg))

type Obs = Data.Row

let model (theta0, theta1) (obs:Obs) =
    theta0 + theta1 * (float obs.Instant)

let model0 = model (4504., 0.)
let model1 = model (6000., -4.5)

Chart.Combine [
    Chart.Line count
    Chart.Line [ for obs in data -> model0 obs]
    Chart.Line [ for obs in data -> model1 obs]
]

type Model = Obs -> float

let cost (data:Obs seq) (m:Model) =
    data
    |> Seq.sumBy (fun x -> pown (float x.Cnt - m x) 2)
    |> sqrt

let overallCost = cost data
overallCost model0 |> printfn "Cost model0: %.0f"
overallCost model1 |> printfn "Cost model1: %.0f"