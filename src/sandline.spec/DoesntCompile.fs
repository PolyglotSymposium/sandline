module DoesntCompile

//open Fuchu

//open Sandline

//[<Tests>]
//let specs = 
//    testList "Purity checking for records" [
//        testCase "A record with no mutable fields is pure" <| fun _ ->
//            let filepath = saveCode """
//            module MyLibrary

//            let a = { X : 2; Y = "foo" }
//            """
//            Assert.Raise("Code does not compile", typeof<System.Exception>, fun _ -> checkPurity filepath |> ignore)
//    ]
