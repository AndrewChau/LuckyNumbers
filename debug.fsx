let printSeq theSeq =
    theSeq |> Seq.iter (printf "%d ")
    printfn("")

let rec removeGaps desiredNumber currentList index =
    let gap = Seq.item index currentList
    let currentListIndexed = currentList |> Seq.indexed

    let newList = Seq.filter (fun (i, v) -> not((i + 1) % gap = 0)) currentListIndexed
    let newListValues = Seq.map (fun (i, v) -> v) newList

    if Seq.exists (fun v -> v = desiredNumber) newListValues then
        let desiredNumberIndex = Seq.findIndex (fun v -> v = desiredNumber) newListValues
        if desiredNumberIndex < gap then
            true
        else
            printfn "index = %d, gap = %d, desiredNumberIndex = %d" index gap desiredNumberIndex
            // printSeq currentList
            removeGaps desiredNumber newListValues (index + 1)
    else
        printSeq currentList
        false

let isLuckyNumber desiredNumber =
    if desiredNumber = 1 then
        true
    else
        let lastNumber = desiredNumber * 2
        let numbersToCheck = seq { 1 ..2.. lastNumber }
        removeGaps desiredNumber numbersToCheck 1


let desiredNumber = match fsi.CommandLineArgs.Length with
                        | args when args > 1 -> (Array.get fsi.CommandLineArgs 1) |> int
                        | _ -> 1

printfn "Is %d a lucky number? %b" desiredNumber (isLuckyNumber desiredNumber)