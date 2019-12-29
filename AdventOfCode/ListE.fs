module ListE

let isEmpty (xs: 'a list) =
    match xs with
    | [] -> true
    | _ -> false

let nonEmpty (xs: 'a list) =
    not(isEmpty xs)

