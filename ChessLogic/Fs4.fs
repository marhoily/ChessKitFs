module List

open System.Collections.Generic

// Can replace with List.contains in F# 4.0
let internal contains item = List.exists (fun i -> i = item)

// Can replace with List.except in F# 4.0
let internal except (itemsToExclude : 'T list) list = 
    let cached = new HashSet<'T>(itemsToExclude, HashIdentity.Structural)
    list |> List.filter cached.Add
