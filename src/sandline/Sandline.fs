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

let input0 = """
module MyLibrary

let foo = System.DateTime.Now
"""

type ImpurityEvidence =
    | UsesMutability of FSharpSymbol

type Purity =
    | Pure
    | Impure of ImpurityEvidence
    | Unknown of string

let (&&&&) p1 p2 =
    match p1, p2 with
    | Pure, Pure -> Pure
    | Impure a, _ -> Impure a
    | _, Impure a -> Impure a
    | Unknown a, _ -> Unknown a
    | _, Unknown b -> Unknown b

let impureFunctions = [
    "Microsoft.FSharp.Core.Operators.ref"
    "Microsoft.FSharp.Core.Operators.( ! )"
    "Microsoft.FSharp.Core.Operators.( := )"
]

let mapPurity f =
    Seq.map f
    >> Seq.reduce (&&&&)

let rec checkExprPurity (expr : FSharpExpr) = 
    match expr with 
    | BasicPatterns.AddressOf(lvalueExpr) -> Unknown "AddressOf"
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) -> Unknown "AddressSet"
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) -> Unknown "Application"
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
        if Seq.exists ((=) memberOrFunc.FullName) impureFunctions
        then Impure <| UsesMutability memberOrFunc
        else
            mapPurity checkExprPurity argExprs
    | BasicPatterns.Coerce(targetType, inpExpr) -> Unknown "Coerce"
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) -> Unknown "FastIntegerForLoop"
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) -> Unknown "ILAsm"
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> Unknown "ILFieldGet"
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> Unknown "IlFieldSet"
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        mapPurity checkExprPurity [guardExpr; thenExpr; elseExpr]
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
        mapPurity checkDeclPurity subDecls
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(mOrFOrV, vs, expr) -> 
        if mOrFOrV.IsMutable
        then Impure <| UsesMutability mOrFOrV
        else checkExprPurity expr
    | FSharpImplementationFileDeclaration.InitAction expr -> 
        checkExprPurity expr

let checkPurity input =
    mapPurity checkDeclPurity (parseAndCheckSingleFile input).Declarations

