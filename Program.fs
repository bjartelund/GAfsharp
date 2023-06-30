// For more information see https://aka.ms/fsharp-console-apps
open System

type Weight = int
type Price = int
type Quantity = int
type Thing = {Weight: int; Price: int ; Quantity: int}
type Banana = Thing
type Orange = Thing
type Apple = Thing
type Strawberry = Thing
let Empty : Thing = {Weight=0; Price=0; Quantity=0}

let CalculatePriceTotal ( things) : int =
    let total = [
        for thing in things do
            yield thing.Price * thing.Quantity
        ]
    total |> List.sum 

let CalculateWeightTotal ( things) : int =
    let total = [
        for thing in things do
            yield thing.Weight* thing.Quantity
    ]
    total |> List.sum

let RandomizeQuantity ( things )  =
    let rnd = new Random()
    let newBag = things |> List.map (fun (thing) ->  {thing with Quantity = rnd.Next(0,10)})
    newBag

let changeQuantity likelihood thing  =
    let rnd = new Random()
    if (rnd.Next(0,100) < likelihood) 
        then 
            {thing with Quantity = rnd.Next(0,10)}
    else
        thing

let RandomizeSomeQuantities likelihood things  =
    let changer = changeQuantity likelihood
    things |> List.map changer

let shuffledUnzip (array : (('a * 'b) list)) = 
    let len = List.length array
    let res1 = Array.create len Empty
    let res2 = Array.create len Empty
    let rnd = new Random()
    let elements = len - 1
    for i = 0 to elements  do
        let x,y = array.[i]
        let random = rnd.Next(0,2)
        if random = 1 
            then
                res1.[i] <- x
                res2.[i] <- y 
        else
            res2.[i] <- x
            res1.[i] <- y
    res1,res2

let Shuffle bagA bagB =
    let a,b = List.zip bagA bagB |> shuffledUnzip 
    a

let Evaluate things =
    let priceTotal = CalculatePriceTotal things
    let weightTotal = CalculateWeightTotal things
    priceTotal, weightTotal

let PrintEvaluation things =
    let totalPrice, totalWeight = Evaluate things
    printfn "Total price is %d" totalPrice
    printfn "Total weight is %d" totalWeight


let banan : Banana = { Weight = 3; Price = 5; Quantity = 1}
let apple : Apple = { Weight = 2; Price = 4; Quantity = 1}
let orange: Orange = { Weight = 2; Price = 3; Quantity = 1}
let strawberry: Strawberry = { Weight = 1; Price = 6; Quantity = 1}

let items = [banan;apple;orange;strawberry]
PrintEvaluation items

printfn "Shake things up"
let newBag = RandomizeQuantity items
PrintEvaluation newBag

printfn "Shuffle"
let (shuffledStart)  = Shuffle items newBag
PrintEvaluation shuffledStart

printfn "Start for real"

let ShuffleFromList collection =
    let rnd = new Random()
    let length = List.length collection
    let sequence = seq 
                    { for _ in 1..50 do
                        Shuffle collection[rnd.Next length] collection[rnd.Next length]
                    }
    sequence |> Seq.toList|> List.map Array.toList

let Evolve collection = 
    let best = collection |> List.sortBy (fun things -> -CalculatePriceTotal things) |> Seq.truncate 10 |> Seq.toList
    let shuffled = ShuffleFromList best
    let randomizer = RandomizeSomeQuantities 5
    let randomized = shuffled@best |> List.map randomizer
    let collected = best@shuffled@randomized
    collected |> List.filter (fun things -> CalculateWeightTotal things < 20 )

let EvolveSteps steps collection =
    let mutable newCollection = collection
    for i in 1..steps do
        newCollection <- Evolve newCollection
        let maxPrize = CalculatePriceTotal (newCollection |> List.maxBy (fun things -> CalculatePriceTotal things) )
        printfn "iteration %d: %d"  i maxPrize 
    newCollection
let collection = List.replicate 100 items |> List.map RandomizeQuantity 
let filtered = collection |> List.filter (fun things -> CalculateWeightTotal things < 20 )
let best = filtered |> List.sortBy (fun things -> -CalculatePriceTotal things)
best[0..4] |> List.map PrintEvaluation |> ignore

printfn "shuffle and randomize 1000x "
let moreEvolution = EvolveSteps 1000 best |> List.sortBy (fun things -> -CalculatePriceTotal things) |> Seq.take 5 |> Seq.toList 
moreEvolution|> List.map PrintEvaluation |> ignore
