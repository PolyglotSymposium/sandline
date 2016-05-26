module Sandline

open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parseAndCheckSingleFile file = 
    // Get context representing a stand-alone (script) file
    let projOptions = 
        checker.GetProjectOptionsFromScript(file, File.ReadAllText file)
        |> Async.RunSynchronously

    (checker.ParseAndCheckProject(projOptions) 
    |> Async.RunSynchronously).AssemblyContents.ImplementationFiles.[0]

let rec visitExpr f (e:FSharpExpr) = 
    f e
    match e with 
    | BasicPatterns.AddressOf(lvalueExpr) -> 
        visitExpr f lvalueExpr
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
        visitExpr f lvalueExpr; visitExpr f rvalueExpr
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) -> 
        visitExpr f funcExpr; visitExprs f argExprs
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> 
        visitObjArg f objExprOpt; visitExprs f argExprs
    | BasicPatterns.Coerce(targetType, inpExpr) -> 
        visitExpr f inpExpr
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) -> 
        visitExpr f startExpr; visitExpr f limitExpr; visitExpr f consumeExpr
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> 
        visitObjArg f objExprOpt
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> 
        visitObjArg f objExprOpt
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
        visitExpr f guardExpr; visitExpr f thenExpr; visitExpr f elseExpr
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) -> 
        visitExpr f bodyExpr
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) -> 
        visitExpr f bindingExpr; visitExpr f bodyExpr
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> 
        List.iter (snd >> visitExpr f) recursiveBindings; visitExpr f bodyExpr
    | BasicPatterns.NewArray(arrayType, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) -> 
        visitExpr f delegateBodyExpr
    | BasicPatterns.NewObject(objType, typeArgs, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.NewRecord(recordType, argExprs) ->  
        visitExprs f argExprs
    | BasicPatterns.NewTuple(tupleType, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.Quote(quotedExpr) -> 
        visitExpr f quotedExpr
    | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> 
        visitObjArg f objExprOpt
    | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> 
        visitObjArg f objExprOpt; visitExpr f argExpr
    | BasicPatterns.Sequential(firstExpr, secondExpr) -> 
        visitExpr f firstExpr; visitExpr f secondExpr
    | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) -> 
        visitExpr f bodyExpr; visitExpr f finalizeExpr
    | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) -> 
        visitExpr f bodyExpr; visitExpr f catchExpr
    | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> 
        visitExpr f tupleExpr
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) -> 
        visitExpr f decisionExpr; List.iter (snd >> visitExpr f) decisionTargets
    | BasicPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> 
        visitExprs f decisionTargetExprs
    | BasicPatterns.TypeLambda(genericParam, bodyExpr) -> 
        visitExpr f bodyExpr
    | BasicPatterns.TypeTest(ty, inpExpr) -> 
        visitExpr f inpExpr
    | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> 
        visitExpr f unionExpr; visitExpr f valueExpr
    | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> 
        visitExpr f unionExpr
    | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> 
        visitExpr f unionExpr
    | BasicPatterns.UnionCaseTag(unionExpr, unionType) -> 
        visitExpr f unionExpr
    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> 
        visitExpr f baseCallExpr
        List.iter (visitObjMember f) overrides
        List.iter (snd >> List.iter (visitObjMember f)) interfaceImplementations
    | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.ValueSet(valToSet, valueExpr) -> 
        visitExpr f valueExpr
    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) -> 
        visitExpr f guardExpr; visitExpr f bodyExpr
    | BasicPatterns.BaseValue baseType -> ()
    | BasicPatterns.DefaultValue defaultType -> ()
    | BasicPatterns.ThisValue thisType -> ()
    | BasicPatterns.Const(constValueObj, constType) -> ()
    | BasicPatterns.Value(valueToGet) -> ()
    | _ -> failwith (sprintf "unrecognized %+A" e)

and visitExprs f exprs = 
    List.iter (visitExpr f) exprs

and visitObjArg f objOpt = 
    Option.iter (visitExpr f) objOpt

and visitObjMember f memb = 
    visitExpr f memb.Body

let rec printDecl prefix d = 
    match d with 
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> 
        printfn "%sEntity %s was declared and contains %d sub-declarations" prefix e.CompiledName subDecls.Length
        for subDecl in subDecls do 
            printDecl (prefix+"    ") subDecl
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> 
        printfn "%sMember or value %s was declared" prefix  v.CompiledName
    | FSharpImplementationFileDeclaration.InitAction(e) -> 
        printfn "%sA top-level expression was declared" prefix 

let input0 = """
module MyLibrary

let foo = System.DateTime.Now
"""

let input1 = """
module MyLibrary

let foo = 0
"""

let input2 = """
module MyLibrary 

open System

let foo(x, y) = 
    let msg = String.Concat("Hello", " ", "world")
    if msg.Length > 10 then 
        10 
    else 
        20

type MyClass() = 
    member x.MyMethod() = 1
      """

let input3 = """
module MyLibrary

let id x = x
"""

let input4 = """
module MyLibrary

let mutable foo = 5
"""

let input5 = """
module MyLibrary

let foo = ref 5
"""

type Purity =
    | Pure
    | Impure
    | Unknown of string

let (&&&&) p1 p2 =
    match p1, p2 with
    | Pure, Pure -> Pure
    | Impure, _ -> Impure
    | _, Impure -> Impure
    | Unknown a, _ -> Unknown a
    | _, Unknown b -> Unknown b

let rec checkExprPurity (expr : FSharpExpr) = 
    match expr with 
    | BasicPatterns.AddressOf(lvalueExpr) -> Unknown "AddressOf"
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) -> Unknown "AddressSet"
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) -> Unknown "Application"
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> Unknown "Call"
    | BasicPatterns.Coerce(targetType, inpExpr) -> Unknown "Coerce"
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) -> Unknown "FastIntegerForLoop"
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) -> Unknown "ILAsm"
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> Unknown "ILFieldGet"
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> Unknown "IlFieldSet"
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> Unknown "IfThenElse"
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) -> Unknown "Lambda"
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) -> Unknown "Let"
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> Unknown "LetRec"
    | BasicPatterns.NewArray(arrayType, argExprs) -> Unknown "NewArray"
    | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) -> Unknown "NewDelegate"
    | BasicPatterns.NewObject(objType, typeArgs, argExprs) -> Unknown "NewObject"
    | BasicPatterns.NewRecord(recordType, argExprs) -> Unknown "NewRecord"
    | BasicPatterns.NewTuple(tupleType, argExprs) -> Unknown "NewTuple"
    | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) -> Unknown "NewUnionCase"
    | BasicPatterns.Quote(quotedExpr) -> Unknown "Quote"
    | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> Unknown "FSharpFieldGet"
    | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> Unknown "FSharpFieldSet"
    | BasicPatterns.Sequential(firstExpr, secondExpr) -> Unknown "Sequential"
    | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) -> Unknown "TryFinally"
    | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) -> Unknown "TryWith"
    | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> Unknown "TupleGet"
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) -> Unknown "DecisionTree"
    | BasicPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> Unknown "DecisionTreeSuccess"
    | BasicPatterns.TypeLambda(genericParam, bodyExpr) -> Unknown "TypeLambda"
    | BasicPatterns.TypeTest(ty, inpExpr) -> Unknown "TypeTest"
    | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> Unknown "UnionCaseSet"
    | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> Unknown "UnionCaseGet"
    | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> Unknown "UnionCaseTest"
    | BasicPatterns.UnionCaseTag(unionExpr, unionType) -> Unknown "UnionCaseTag"
    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> Unknown "ObjectExpr"
    | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argExprs) -> Unknown "TraitCall"
    | BasicPatterns.ValueSet(valToSet, valueExpr) -> Unknown "ValueSet"
    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) -> Unknown "WhileLoop"
    | BasicPatterns.BaseValue baseType -> Unknown "BaseValue"
    | BasicPatterns.DefaultValue defaultType -> Unknown "DefaultValue"
    | BasicPatterns.ThisValue thisType -> Unknown "ThisValue"
    | BasicPatterns.Const(constValueObj, constType) -> Pure
    | BasicPatterns.Value valueToGet -> Pure
    | _ -> Unknown "don't even know what this is"

let rec checkDeclPurity d = 
    match d with 
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> 
        checkDeclsPurity subDecls
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, expr) -> 
        if v.IsMutable
        then Impure
        else checkExprPurity expr
    | FSharpImplementationFileDeclaration.InitAction expr -> 
        checkExprPurity expr
and checkDeclsPurity decls =
    Seq.map checkDeclPurity decls
    |> Seq.reduce (&&&&)

let checkPurity input =
    checkDeclsPurity (parseAndCheckSingleFile input).Declarations

let test'() =
    let checkedFile = parseAndCheckSingleFile input1

    for d in checkedFile.Declarations do 
       printDecl "" d

    let myLibraryEntity, myLibraryDecls =    
       match checkedFile.Declarations.[0] with 
       | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> (e, subDecls)
       | _ -> failwith "unexpected"

    printfn "myLibraryEntity is F# module: %b" myLibraryEntity.IsFSharpModule

    let (fooSymbol, fooArgs, fooExpression) = 
        match myLibraryDecls.[0] with 
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> (v, vs, e)
        | _ -> failwith "unexpected"

    printfn "foo Expression Type: %A" fooExpression.Type  // shows that the return type of the body expression is 'int'

    fooExpression |> visitExpr (printfn "Visiting %A")
