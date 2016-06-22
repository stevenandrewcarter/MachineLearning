open System.IO
type Observation = { Label:string; Pixels: int[] }

let toObservation (csvData:string) = 
  let columns = csvData.Split(',')
  let label = columns.[0]
  let pixels = columns.[1..] |> Array.map int 
  { Label = label; Pixels = pixels }

let reader path = 
    let data = File.ReadAllLines path
    data.[1..] |> Array.map toObservation

let trainingPath = __SOURCE_DIRECTORY__ + @"..\..\Data\trainingsample.csv"
let training = reader trainingPath

type Distance = int[] * int[] -> int 
let manhattanDistance (pixels1,pixels2) = 
    Array.zip pixels1 pixels2
    |> Array.map (fun (x, y) -> abs(x - y))
    |> Array.sum

let euclideneDistance (pixels1,pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x, y) -> pown(x - y) 2)
    |> Array.sum

let train (trainingset:Observation[]) (dist:Distance) =
    let classify (pixels:int[]) =
        trainingset
        |> Array.minBy (fun x -> dist (x.Pixels, pixels))
        |> fun x -> x.Label
    classify

let manhattanClassifier = train training manhattanDistance
let euclideneClassifier = train training euclideneDistance

let validationPath = __SOURCE_DIRECTORY__ + @"..\..\Data\validationsample.csv"
let validationData = reader validationPath

let evaluate validationSet classifier = 
    validationSet
    |> Array.averageBy (fun x -> if classifier x.Pixels = x.Label then 1. else 0.)
    |> printfn "Correct: %.3f"

printfn "Manhattan"
evaluate validationData manhattanClassifier
printfn "Euclidene"
evaluate validationData euclideneClassifier