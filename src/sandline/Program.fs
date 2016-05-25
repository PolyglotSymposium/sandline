// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main _ = 
    Sandline.test()
    printfn "Your first code is %A" <| Sandline.checkPurity Sandline.input0
    printfn "Your second code is %A" <| Sandline.checkPurity Sandline.input1
    0 // return an integer exit code
