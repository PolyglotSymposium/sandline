#r @"packages\FAKE\tools\FakeLib.dll"

open Fake

let buildDir = "output"

let clean() =
    CleanDir buildDir

let compileSrcSln () =
    !! "src/*.sln"
    |> MSBuildRelease buildDir "Build"
    |> Log "Compile-Output: "

let runFuchuTests() =
    let errorCode =
        !! (buildDir </> "*.spec.exe")
        |> Seq.map (fun p -> if not isMono then p,null else "mono",p)
        |> Seq.map (fun (p,a) -> asyncShellExec { defaultParams with Program = p; CommandLine = a })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.sum
    if errorCode <> 0
    then failwith "Unit tests failed"

Target "clean" clean
Target "compile" compileSrcSln
Target "test" runFuchuTests
Target "rebuild" DoNothing

"clean" ==> "rebuild"
"clean" ?=> "compile"
"compile" ==> "rebuild"
"compile" ?=> "test"
"test" ==> "rebuild"

RunTargetOrDefault "rebuild"
