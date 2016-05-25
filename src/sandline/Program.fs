// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main _ = 
    Sandline.test()
    printfn "Your code is pure: %b" <| Sandline.checkPurity()
    0 // return an integer exit code
