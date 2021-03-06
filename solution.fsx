/// <summary>
/// Recursively remove gaps for the next number in the list until either the number we are checking
/// is smaller than the gap or not in the list anymore.
/// </summary>
/// <param name="desiredNumber">The number being tested</param>
/// <param name="currentList">The remaining list of numbers</param>
/// <param name="index">The index of the next value to become the gap</param>
/// <returns>False if the desired number is no longer in the list after removing the gaps</returns>
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
            removeGaps desiredNumber newListValues (index + 1)
    else
        false

/// <summary>
/// Check whether the desired number is a lucky number.
/// </summary>
/// <param name="desiredNumber">The number to check</param>
/// <returns>True if the desired number is a lucky number</returns>
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