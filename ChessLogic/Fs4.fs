//namespace Fs4

module MyList

    open System.Collections.Generic

    // Can replace with List.contains in F# 4.0
    let contains item = List.exists (fun i -> i = item)
    // Can replace with List.except in F# 4.0
    let except (itemsToExclude: 'T list) list =
        match box itemsToExclude with
        | null -> nullArg "itemsToExclude"
        | _ -> ()

        match list with
        | [] -> list
        | _ ->
            let cached = new HashSet<'T>(itemsToExclude, HashIdentity.Structural)
            list |> List.filter cached.Add
