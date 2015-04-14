module Test

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

open FsUnit
open FsCheck
open NUnit.Framework

open Exercise

// mapList tests

[<Test>]
let ``Map empty list returns empty list``() =
    mapList (fun x -> x * 2) []
    |> should equal []

[<Test>]
let ``Map of doubling doubles all elements in the list``() =
    mapList (fun x -> x * 2) [1; 3; 5]
    |> should equal [2; 6; 10]

[<Test>]
let ``Map of single element applies map once``() =
    mapList (fun x -> -x) [4]
    |> should equal [-4]

// filterList tests

[<Test>]
let ``Filter empty list returns empty list``() =
    filterList (fun x -> true) []
    |> should equal []

[<Test>]
let ``Filter with false predicate returns empty list``() =
    filterList (fun x -> false) [1; 2; 3; 4]
    |> should equal []

[<Test>]
let ``Filter only returns matching elements``() =
    filterList (fun x -> x % 2 = 0) [1; 2; 3; 4]
    |> should equal [2; 4]

// foldList tests

[<Test>]
let ``Fold on empty list returns empty list``() =
    foldList (fun acc (x,y) -> acc + y) 0 []
    |> should equal 0;

[<Test>]
let ``Fold on single element returns that element folding result``() =
    foldList (fun acc (x,y) -> acc + y) 0 [("Cats", 4)]
    |> should equal 4;

[<Test>]
let ``Fold on multiple elements returns the result of applying folder to all items``() =
    foldList (fun acc (x,y) -> acc + y) 0 [("Cats", 4); ("Dogs", 5); ("Mice", 3); ("Elephants", 2)]
    |> should equal 14;

// reduceList tests

[<Test>]
[<ExpectedException("System.ArgumentException")>]
let ``Reduce on empty list is invalid argument``() =
    reduceList (fun x y -> x + y) [] |> should equal null

[<Test>]
let ``Reduce on single element returns the element``() =
    reduceList (fun x y -> x + y) [5] |> should equal 5

[<Test>]
let ``Reduce on multiple elements applies the reducer to all of them``() =
    reduceList (fun x y -> x + y) [5; 11; 23; 1; 3;] |> should equal 43