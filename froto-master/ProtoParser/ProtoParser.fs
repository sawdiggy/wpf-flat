
module Froto.Parser.ProtoParser

open System
open System.IO
open System.Text
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error
open ProtoAst

let internal pWord : Parser<string,unit> = many1Chars (asciiLetter <|> digit <|> anyOf "_.")
let internal pWordQuotes : Parser<string,unit> = pchar '"' >>. many1Chars (asciiLetter <|> digit <|> anyOf "_./! ") .>> pchar '"'
let internal pWordParens = pchar '(' >>. pWord .>> pchar ')'

let internal fieldRuleMap =
    [
    "required", Required;
    "optional", Optional;
    "repeated", Repeated;
    ]
    |> Map.ofList

let internal toFieldRule s = fieldRuleMap.[s]

let pFieldRule =
    (pstring "required" <|> pstring "optional" <|> pstring "repeated" ) |>> toFieldRule

let pOption  =
    let pCustom = pWordParens .>>. opt pWord |>> fun (prefix, name) -> Some prefix, name
    let pNotCustom = pWord |>> fun name -> None, Some name
    (pCustom <|> pNotCustom) .>> spaces .>> pchar '=' .>> spaces .>>. (pWordQuotes <|> pWord) .>> spaces
    |>> fun ((prefix, name), value) -> ProtoOption(prefix, name, value)

let pFieldOptions =
    pchar '[' >>. spaces >>. sepBy pOption (pchar ',' .>> spaces) .>> (pchar ']') .>> spaces

let pField =
 pFieldRule .>> spaces1 .>>. pWord .>> spaces1 .>>. pWord .>> spaces .>> pchar '=' .>> spaces .>>. pint32 .>> spaces .>>. (opt pFieldOptions) .>> pchar ';' .>> spaces
    |>> fun ((((rule, tp), name), position), options) -> ProtoField(rule, tp, name, position, options)

    //extensions can end in a "max" or a number, so we give the optional long or string at the end with no spaces between
let pExtension = 
    pstring "extensions" .>> spaces1 .>>. pint64 .>> spaces1 .>>. pstring "to" .>> spaces1 .>> (opt pint64) .>> spaces .>> (opt pWord) .>> pchar ';' .>> spaces
    |>> fun ((ext, start) , max) -> ProtoExtension(start, max)
   

let pEnumItem =
    pWord .>> spaces .>> pchar '=' .>> spaces .>>. pint32 .>> spaces .>> pchar ';' .>> spaces
    |>> fun (name, value) -> ProtoEnumItem(name, value)

let pEnum =
    pstring "enum" >>. (spaces1 >>. pWord .>> spaces .>> pchar '{' .>> spaces .>>. many1 pEnumItem .>> many1 (pchar '}' <|> pchar ';') .>> spaces
    |>> fun (name, items) -> ProtoEnum(name, items))

let pImport  =
    pstring "import" >>. (spaces >>. pWordQuotes .>> spaces .>> pchar ';' .>> spaces)

let pMessageRec, pMessageRecRef = createParserForwardedToRef()

let pOptionLine =
    pstring "option" >>. spaces >>. pOption .>> spaces .>> pchar ';' .>> spaces

let pMessage : Parser<ProtoMessage,unit> =
    let pMessageBox = pMessageRec |>> fun (msg:ProtoMessage) -> 
        let pmp = if msg.IsExtend then ProtoMessagePart.Extend else ProtoMessagePart.Message
        pmp, box msg
    let pEnumBox = pEnum |>> fun enum -> ProtoMessagePart.Enum, box enum
    let pFieldBox = pField |>> fun field -> ProtoMessagePart.Field, box field
    let pExtensionBox = pExtension |>> fun ext -> ProtoMessagePart.Extensions, box ext
    let pOptionBox = pOptionLine |>> fun option -> ProtoMessagePart.Option, box option
    (pstring "message" <|> pstring "extend") .>> spaces1 .>>. pWord .>> spaces .>> pchar '{'
    .>> spaces .>>. many (pMessageBox <|> pEnumBox <|> pFieldBox <|> pOptionBox <|> pExtensionBox)
    .>> many1 (pchar '}' <|> pchar ';') .>> spaces
    |>> fun ((tp, name), parts) -> ProtoMessage(name, parts, (tp = "extend"))

do pMessageRecRef := pMessage

let pRpc =
    pstring "rpc" >>. spaces >>. pWord .>> spaces .>>. pWordParens .>> spaces .>> pstring "returns" .>> spaces .>>. pWordParens .>> spaces .>> pchar ';' .>> spaces
    |>> fun ((name, requestType), responseType) -> ProtoRpc(name, requestType, responseType)

let pService =
    let pRpcBox = pRpc |>> fun rpc -> ProtoServicePart.Rpc, box rpc
    pstring "service" >>. spaces >>. pWord .>> spaces .>> pchar '{' .>> spaces .>>.
    many1 pRpcBox
    .>> spaces .>> pchar '}' .>> spaces
    |>> fun (name, parts) -> ProtoService(name, parts) 

let pPackage =
    pstring "package" >>. (spaces1 >>. pWord .>> spaces .>> pchar ';' .>> spaces)

let pProto =
    let pPackageBox = pPackage |>> fun package -> ProtoSection.Package, box package
    let pMessageBox = pMessage |>> fun msg ->
        let ps = if msg.IsExtend then ProtoSection.Extend else ProtoSection.Message
        ps, box msg
    let pOptionBox = pOptionLine |>> fun option -> ProtoSection.Option, box option
    let pImportBox = pImport |>> fun import -> ProtoSection.Import, box import
    let pEnumBox = pEnum |>> fun enum -> ProtoSection.Enum, box enum
    let pServiceBox = pService |>> fun service -> ProtoSection.Service, box service
    spaces >>. many (pPackageBox <|> pOptionBox <|> pImportBox <|> pMessageBox <|> pServiceBox <|> pEnumBox)
    |>> fun sections -> ProtoFile(sections)

let internal resultOrFail parserResult =
    match parserResult with
    | Success (result, _, _) -> result
    | Failure (errMsg, _, _) -> failwith errMsg

let parseString parser str =
    runParserOnString (parser .>> eof) () String.Empty str
    |> resultOrFail

let parseFile parser path =
    runParserOnFile (parser .>> eof) () path Text.Encoding.UTF8
    |> resultOrFail

let parseStream parser stream =
    runParserOnStream (parser .>> eof) () String.Empty stream Encoding.UTF8
    |> resultOrFail

let pSingleLineComments =
    let pNotSlash : Parser<string,unit> = manyChars (noneOf "/\r\n")
    let pComment = attempt (pNotSlash .>> pstring "//" .>> restOfLine true) <|> restOfLine true
    many1Till pComment eof

let pMultiLineComments =
    let pBefore : Parser<string,unit> = manyCharsTill anyChar ((skipString "/*") <|> eof)
    let pIn = manyCharsTill anyChar (skipString "*/")
    let pAfter = manyCharsTill anyChar eof
    many (attempt (pBefore .>> pIn)) .>>. pAfter .>> eof

let internal createMemoryStream (fWrite : StreamWriter -> unit) =
    let ms = new MemoryStream()
    use sw = new StreamWriter(ms, Encoding.UTF8, 4096, true) // leave stream open
    fWrite sw
    sw.Flush()
    ms.Position <- 0L
    ms

let internal parseProto lines =
    use sSingle = createMemoryStream (fun sw ->
        lines |> Seq.iter (fun (line:string) -> sw.WriteLine line)
    )
    let texts, last = parseStream pMultiLineComments sSingle
    use sMulti = createMemoryStream (fun sw ->
        texts |> Seq.iter (fun text-> sw.Write text)
        sw.Write last

    )
    parseStream pProto sMulti

let parseProtoFile path =
    parseFile pSingleLineComments path |> parseProto

let parseProtoStream stream =
    parseStream pSingleLineComments stream |> parseProto