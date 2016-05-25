// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main _ = 
    Sandline.test()
    printfn "Your first code is %A" <| Sandline.checkPurity Sandline.input0
    printfn "Your second code is %A" <| Sandline.checkPurity Sandline.input1
    //printfn "Your third code is %A" <| Sandline.checkPurity Sandline.input2
    printfn "Your fourth code is %A" <| Sandline.checkPurity Sandline.input3
    printfn "Your fifth code is %A" <| Sandline.checkPurity Sandline.input4
    printfn "Your sixth code is %A" <| Sandline.checkPurity Sandline.input5
    0 // return an integer exit code
