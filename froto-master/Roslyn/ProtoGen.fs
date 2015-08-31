
module Froto.Roslyn.ProtoGen

open System
open System.IO
open Froto.Parser
open Froto.Parser.ProtoAst
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open System.IO
open System.Globalization

// aliases
module NS = NamespaceDeclarationSyntax
module CU = CompilationUnitSyntax
module CD = ClassDeclarationSyntax
module ED = EnumDeclarationSyntax
module Cmp = Compilation
module MD = MethodDeclarationSyntax
module PD = PropertyDeclarationSyntax
module CTOR = ConstructorDeclarationSyntax

let internal doesContain (value:string) (list:string array) = List.ofArray list |> List.exists (fun elem -> elem = value)

let createCompilation path rootTpName root =

    let culture =  new CultureInfo("en-US",false)

    // https://developers.google.com/protocol-buffers/docs/proto#scalar
    let scalarToSyntaxKind =
        [   "double", TypeSyntax.double;
            "float", TypeSyntax.float;
            "int32", TypeSyntax.int;
            "int64", TypeSyntax.int64;
            "uint32", TypeSyntax.uint;
            "uint64", TypeSyntax.uint64;
            "sint32", TypeSyntax.int;
            "sint64", TypeSyntax.int64;
            "fixed32", TypeSyntax.uint;
            "fixed64", TypeSyntax.uint64;
            "sfixed32", TypeSyntax.int;
            "sfixed64", TypeSyntax.int64;
            "bool", TypeSyntax.bool;
            "string", TypeSyntax.string;
            "bytes", TypeSyntax.byteArray;

            //nullable types
            "double?", TypeSyntax.nullableDouble;
            "int32?", TypeSyntax.nullableInt32;
            "float?", TypeSyntax.nullableFloat;
            "int64?", TypeSyntax.nullableInt64;
            "uint32?", TypeSyntax.nullableUint;
            "uint64?", TypeSyntax.nullableuInt64;
            "sint32?", TypeSyntax.nullableInt32;
            "sint64?", TypeSyntax.nullableInt64;
            "fixed32?", TypeSyntax.nullableInt32;
            "fixed64?", TypeSyntax.nullableInt64;
            "sfixed32?", TypeSyntax.nullableInt32;
            "sfixed64?", TypeSyntax.nullableInt64;
            "bool?", TypeSyntax.nullableBool;
            //optional string just returns a string (default to be nullable)
            "string?", TypeSyntax.string;
            "bytes?", TypeSyntax.byteArray;
        ]
        |> Map.ofList

    let listType = "List"

    let proto = ProtoParser.parseProtoFile path
        
    let rec listNestedEnums (enums:ProtoEnum  list) (messages:ProtoMessage list) =
        [   yield! enums
            for message in messages do
                yield! listNestedEnums message.Enums message.Messages 
        ]

    let listEnums (protoFile:ProtoFile) =
        [ 
            yield! protoFile.Enums 
        ]

    let removeSubDirectory (dir:string) = 
        let arr = dir.Split('/')
        if(arr.Length > 1) then arr.[arr.Length-1] else arr.[0]

    let rec findImports (imports:string list) rootPath =
        [
            for import in imports do
                yield! Directory.EnumerateFiles(rootPath, removeSubDirectory import)
                
            for path in Directory.EnumerateDirectories rootPath do
                yield! findImports imports path
        ]

    //get the list of packages that need to be imported as "usings"
    let getPackages imports = 
        [
            for import in imports do
               yield! ProtoParser.parseProtoFile(import).Packages
        ]

    //func to return the top level enums.   We only care about the top level enums b/c the nested ones are only accessible by the type
    //they are nested within
    let getPackageImportEnums imports = 
        [
            for import in imports do
               yield! ProtoParser.parseProtoFile(import).Enums
        ]

    //get the unique list of packages for the usings of each type 
    let packageNames = findImports proto.Imports root |> getPackages |> Seq.distinctBy (fun elem -> elem) 
    let packageEnums = findImports proto.Imports root |> getPackageImportEnums |> Seq.distinctBy (fun elem -> elem) 

    //get the top level enums first
    let allEnums = listNestedEnums [] proto.Messages @ listEnums proto @ List.ofSeq packageEnums

    //get the enum names
    let enumToString (all:ProtoEnum list)  = 
        [
            for enum in all do
                yield enum.Name
        ]

    let capName (name:string) = Char.ToUpper(name.[0]).ToString() + name.Substring(1) 

    let createPropertyName prop typeName  = 
        let capProp = capName prop
        if typeName.Equals capProp then capProp + "_" else capProp

    let getTypeSyntax (f:ProtoField) (enums:string list) =
                let Type = if( f.Type.StartsWith(".")) then f.Type.Substring(1, f.Type.Length-1) else f.Type 
                let tList = Type.Split('.')
                let typeSansQN = if tList.Length > 1 then tList.[tList.Length - 1] else tList.[0]
                
                let tp =
                    //option and primitive needs to be set to nullable
                    if  f.Rule = ProtoFieldRule.Optional && scalarToSyntaxKind.ContainsKey Type then
                        scalarToSyntaxKind.[Type+"?"]
                    //repeated and primative needs to be a list
                    elif f.Rule = ProtoFieldRule.Repeated && scalarToSyntaxKind.ContainsKey Type then
                        TypeSyntax.create (listType + "<" + scalarToSyntaxKind.[Type].ToString() + ">")
                    //not optional or list, but primitive
                    elif scalarToSyntaxKind.ContainsKey Type then
                        scalarToSyntaxKind.[Type]
                    //list nonprimative
                    elif f.Rule = ProtoFieldRule.Repeated && (scalarToSyntaxKind.ContainsKey Type <> true) then
                        TypeSyntax.create (listType + "<" + f.Type.ToString() + ">")
                    
                    elif f.Rule = ProtoFieldRule.Optional && List.exists (fun elem -> elem = typeSansQN) enums then
                        TypeSyntax.create (Type + "?")
                    else
                        TypeSyntax.create Type
                tp
    
    //func to generate properties per class
    let getCds (fields:ProtoField list) (allEnums:string list) (messageType:string) (msgEnums:string list) = fields |> List.map (fun f ->
                let tp = getTypeSyntax f allEnums
                
                //handle the case when the property name is the same as the type name

                let name = createPropertyName f.Name messageType
                let enumArr = Array.ofList msgEnums
                                
                let safeName = if doesContain name enumArr  then name + "_" else name
                    
                PD.create safeName tp  
                |> PD.addModifier Keyword.Public
                //create the protoMember attribute and use the position passed in from the proto parser
                |> PD.addAttribute (
                    AttributeSyntax.create "ProtoMember" 
                    |> AttributeSyntax.addArgument (SyntaxFactory.AttributeArgument(SyntaxFactory.ParseName (f.Position.ToString())))
                )   
                :> MemberDeclarationSyntax
                )

    //bit of a cheat here, but we need to make sure we create a default constructor that also creates the lists 
    //otherwise when we use them we'll need to make sure we always create a list prior to use
    //protobuf-net does create the list, but thats only on the read, when we write out a proto, we hit this issue
    let createCtorBody typeName (fields:ProtoField list) (enums:string list) =  
        [
            for field in fields do
                if field.Rule = ProtoFieldRule.Repeated 
                then 
                    let typeSyntax = getTypeSyntax field enums
                    yield createPropertyName field.Name typeName + " = new " + typeSyntax.ToString() + "()"
        ]

    let allEnumNames = enumToString allEnums
    let ctor (name:string) (fields:ProtoField list) = 
        CTOR.createCtor name
        |> CTOR.addModifier Keyword.Public
        |> CTOR.addExpressionToBody (createCtorBody name fields allEnumNames)
        :> MemberDeclarationSyntax        


    let enums (protoEnums:ProtoEnum list) = protoEnums |> List.map (fun enum ->
        ED.create enum.Name
        |> ED.addAttribute (
            AttributeSyntax.create "ProtoContract"        )
        |> ED.addModifier Keyword.Public
        |> ED.addMembers (enum.Items |> List.map (fun item -> EnumMemberDeclarationSyntax.create item.Name item.Value))
        :> MemberDeclarationSyntax
    ) 

    // recursive func to generate a class per message
    let rec cds (msgs:ProtoMessage list) = msgs |> List.map (fun msg ->
        let enumList = enumToString msg.Enums
        CD.create msg.Name
        |> CD.addAttribute (
            AttributeSyntax.create "ProtoContract"        )
        |> CD.addModifier Keyword.Public
        |> CD.addModifier Keyword.Sealed
        |> CD.addMembers ( getCds msg.Fields allEnumNames msg.Name enumList )
        |> CD.addMembers ( msg.Messages |> cds )
        |> CD.addMembers (enums msg.Enums)
        |> CD.addMember (ctor msg.Name msg.Fields)
        :> MemberDeclarationSyntax
    )
    
    let nsName = proto.Packages.[0]
    let fullMessages = cds proto.Messages

    let ns =
        NS.create nsName
        |> NS.addMembers (enums proto.Enums)
        |> NS.addMembers fullMessages

    let rootNs =
        NS.create nsName |> NS.addUsing nsName
            
    let packageNameArray = Seq.toArray packageNames
    
    let st =
        CU.Empty 
        |> CU.addUsing "ProtoBuf"
        |> CU.addUsing "System.IO"
        |> CU.addUsing  "System.Collections.Generic"
        |> CU.addUsingArray packageNameArray
        |> CU.addMember ns
        // uncomment to add the strongly typed serializers
        //|> CU.addMember rootNs
        |> CU.formatDefault
        |> CU.createSyntaxTree

    Cmp.createDll rootTpName
    |> Cmp.addReference typeof<Object> // mscorlib
    |> Cmp.addReference typeof<Attribute> // System.Runtime
    |> Cmp.addReference typeof<ProtoBuf.Serializer> // protobuf-net
    |> Cmp.addReference typeof<Xml.XmlNode>
    |> Cmp.addSyntaxTree st



let generate (root:string) (outputPath:string) (ignoreList:string array) =

    let rec findProtos (protos:string list) rootPath =
        [
            yield! Directory.EnumerateFiles(rootPath) 

            for path in Directory.EnumerateDirectories rootPath do
                if (doesContain path ignoreList) <> true then yield! findProtos protos path
        ]

    let protoList = findProtos [] root

    let generateEachFile (pList:string list) = pList |> List.map(fun path ->
        let protoNameArray = path.Split [|'\\'|]
        let protoName  = protoNameArray.[protoNameArray.Length-1]

        let cleanProtoName = protoName.Replace(".proto","")

        let newPath = outputPath + cleanProtoName + ".cs"

        use sw = new StreamWriter(newPath, false, Text.Encoding.UTF8)

        createCompilation path cleanProtoName root
        |> Cmp.syntaxTrees |> Seq.iter (fun st ->
            sprintf "%A" st |> sw.WriteLine
        ))

    generateEachFile protoList