﻿module Exercise

    (* Notes

        The following exercise is split into two parts. The first has you reimplementing several library functions. The second deals with
        If it is necessary to make a function recursive, add change the declaration from let functionname to let rec functionname

        For any function that is marked with TODO, they have been given default values to reduce the numbers of errors in the incompelte state. 

        If you are having issues with producing the correct result, take a look at the signature file Exercise.fsi to see what your function signature is meant to look like 
        (Or hover over the function)

        When you have completed all of the functions, press f5 to run the full program!

        IF you have any questions, please send them over to Jason!

        Testing functions. Example: 
        Highlight the full definition of PrintList, right click, then press run in interactive.
        Now type:
        PrintList ["a"; "b"] ;; 
        to test
    *)

    //Part one: Reimplementing standard library functions

    // This is a hint and example that uses recursion, a cons pattern and a list pattern. Example from: Pattern Matching - https://msdn.microsoft.com/en-us/library/dd547125.aspx
    // Notice that no time information is provided for what is contained in the list!
    let rec printList list =
        match list with
        | head :: tail -> printfn "%A " head; printList tail
        | [] -> printfn ""

    //Note: Try not to look at other implementations and do from first principles! 
    //If you need examples to test, take a look at the msdn page


    // Reimplement List.map -> https://msdn.microsoft.com/en-us/library/ee370378.aspx
    // Map takes a function and a list, applies the function to each element in the list, and returns a new list of the results. 
    let rec mapList mapping list = 
        match list with
        | x::xs -> mapping x :: mapList mapping xs
        | [] -> []
    
    // Reimplement List.filter -> https://msdn.microsoft.com/en-us/library/ee370294.aspx
    // Fold takes a function to test each list element (function returns a boolean) and a list. Returns a new list containly only the elements for which the predicate returned true. 
    let rec filterList predicate list =
        match list with
        | x::xs ->
            if (predicate x) then x :: filterList predicate xs
            else filterList predicate xs
        | [] -> []

    // Reimplement List.fold -> https://msdn.microsoft.com/en-us/library/ee353894.aspx
    // Takes a function to update the accumulated values given the input element, the initial value, and the list. returns the final accumulated value
    let rec foldList folder acc list =
        match list with
        | x::xs ->
            let newAcc = folder acc x
            foldList folder newAcc xs
        | [] -> acc

    // Reimplement List.reduce -> https://msdn.microsoft.com/en-us/library/ee370239.aspx
    // Takes a function to update the accumulated values given the input element and the list. returns the final accumulated value
    // Throws an ArgumentException if the list is empty (Use invalidArg function -> https://msdn.microsoft.com/en-us/library/dd233178.aspx ) 
    let rec reduceList reducer list =
        match list with
        | x::xs ->
            match xs with
            | y::ys ->
                let reduced = reducer x y
                let newList = reduced::ys
                reduceList reducer newList
            | [] -> x
        | [] -> invalidArg "list" "cannot reduce empty list"
