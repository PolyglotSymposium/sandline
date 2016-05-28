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

type EntityId = string

type ImpurityEvidence =
    | UsesMutability
    | UsesExceptions
    | CallsImpureCode of EntityId * ImpurityEvidence

let rec evidenceToString =
    function
    | UsesMutability -> "uses mutability"
    | UsesExceptions -> "uses exceptions"
    | CallsImpureCode(id, evidence) ->
        sprintf "calls impure %s, which is impure because it %s" id <| evidenceToString evidence

type Purity =
    | Pure
    | Impure of EntityId * ImpurityEvidence
    | Unknown of string

let purityAccumulator state next =
    match state, next with
    | Pure, Pure -> Pure
    | Impure(a, b), _
    | _, Impure (a, b) -> Impure (a, b)
    | Unknown a, _
    | Pure, Unknown a -> Unknown a

let nestingPurityAccumulator id state next =
    match state, next with
    | Pure, Pure -> Pure
    | Impure(a, b), _ -> Impure (a, b)
    | _, Impure (a, b) -> Impure(id, CallsImpureCode(a, b))
    | Unknown a, _ -> Unknown a
    | Pure, Unknown b -> Unknown b

let usesMutability = [
    "Microsoft.FSharp.Core.Operators.ref"
    "Microsoft.FSharp.Core.Operators.( ! )"
    "Microsoft.FSharp.Core.Operators.( := )"
]

let usesExceptions = [
    "Microsoft.FSharp.Core.Operators.raise"
]

let private mapPurity' acculumator f childPurities =
    if Seq.isEmpty childPurities
    then Pure
    else
        Seq.map f childPurities
        |> Seq.reduce acculumator

let mapPurityOfArgs =
    mapPurity' purityAccumulator

let mapPurity id =
    mapPurity' (nestingPurityAccumulator id)

let rec checkExprPurity (expr : FSharpExpr) = 
    match expr with 
    | BasicPatterns.AddressOf(lvalueExpr) -> Unknown "AddressOf"
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) -> Unknown "AddressSet"
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) -> Unknown "Application"
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
        if Seq.exists ((=) memberOrFunc.FullName) usesMutability
        then Impure(memberOrFunc.FullName, UsesMutability)
        else if Seq.exists ((=) memberOrFunc.FullName) usesExceptions
        then Impure(memberOrFunc.FullName, UsesExceptions)
        else
            mapPurityOfArgs checkExprPurity argExprs
    | BasicPatterns.Coerce(targetType, inpExpr) -> Unknown "Coerce"
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) -> Unknown "FastIntegerForLoop"
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) -> Unknown "ILAsm"
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> Unknown "ILFieldGet"
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> Unknown "IlFieldSet"
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        mapPurityOfArgs checkExprPurity [guardExpr; thenExpr; elseExpr]
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) ->
        checkExprPurity bodyExpr
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) -> Unknown "Let"
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> Unknown "LetRec"
    | BasicPatterns.NewArray(arrayType, argExprs) -> Unknown "NewArray"
    | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) -> Unknown "NewDelegate"
    | BasicPatterns.NewObject(objType, typeArgs, argExprs) ->
        sprintf "NewObject: (Type: %s, Type Args: %A, Arg Exprs: %A)" objType.FullName typeArgs argExprs
        |> Unknown
    | BasicPatterns.NewRecord(recordType, argExprs) -> Unknown "NewRecord"
    | BasicPatterns.NewTuple(tupleType, argExprs) -> Unknown "NewTuple"
    | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) -> Unknown "NewUnionCase"
    | BasicPatterns.Quote(quotedExpr) -> Unknown "Quote"
    | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> Unknown "FSharpFieldGet"
    | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> Unknown "FSharpFieldSet"
    | BasicPatterns.Sequential(firstExpr, secondExpr) -> Unknown "Sequential"
    | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) -> Unknown "TryFinally"
    | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) -> Impure("try...with", UsesExceptions)
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
    | FSharpImplementationFileDeclaration.Entity (entity, subDecls) -> 
        mapPurity entity.FullName checkDeclPurity subDecls
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(mOrFOrV, vs, expr) -> 
        let id = mOrFOrV.FullName
        if mOrFOrV.IsMutable
        then Impure(id, UsesMutability)
        else nestingPurityAccumulator id Pure <| checkExprPurity expr
    | FSharpImplementationFileDeclaration.InitAction expr -> 
        checkExprPurity expr

let checkPurity input =
    let checkedFile = parseAndCheckSingleFile input
    mapPurity checkedFile.QualifiedName checkDeclPurity checkedFile.Declarations

let formatPurityResult =
    function
    | Pure -> "Your code is pure!"
    | Impure(id, evidence) -> sprintf "%s is impure because it %s" id (evidenceToString evidence)
    | Unknown debugInfo -> "Could not determine if your code was pure. Debug info: " + debugInfo
