module Record

//open Fuchu
//open Swensen.Unquote

//open Sandline

//[<Tests>]
//let specs = 
//    testList "Purity checking for records" [
//        testCase "A record with no mutable fields is pure" <| fun _ ->
//            let filepath = saveCode """
//            module MyLibrary

//            type R = {
//                X : int
//                Y : string
//            }

//            let a = { X = 2; Y = "foo" }
//            """
//            test <@ checkPurity filepath = Pure @>
//        testCase "A record with a mutable field is impure" <| fun _ ->
//            let filepath = saveCode """
//            module MyLibrary

//            type R = {
//                mutable X : int
//                Y : string
//            }

//            let a = { X = 2; Y = "foo" }
//            """
//            test <@ checkPurity filepath = Impure("R.X", UsesMutability) @>
//        testCase "An instance of a record with no mutable fields but with an impure value is impure" <| fun _ ->
//            let filepath = saveCode """
//            module MyLibrary

//            type R = {
//                X : int
//                Y : string
//            }

//            let a b = { X = !b; Y = "foo" }
//            """
//            test <@ checkPurity filepath = Impure("MyLibrary.a", CallsImpureCode (bang, UsesMutability)) @>
//    ]
