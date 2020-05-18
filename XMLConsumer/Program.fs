// Learn more about F# at http://fsharp.org

open System
open System.IO
open XMLUtil.Parsing
open XMLUtil.Converting
open XMLUtil.Commands

//TODO: create file format for replacing names and data types

//TODO: file parser for replacing format

let getFiles (location:string) = 

    let mutable files:(string*string) list = []

    use sr = new StreamReader(location)

    let mutable currLine = ""

    while currLine <> null do
        currLine <- sr.ReadLine()
        if currLine <> null
            then
                let separators = [|" > "|]
                let split = currLine.Split(separators, StringSplitOptions.None)
                let _from = split.[0]
                let _to = split.[1]
                files <- (_from, _to)::files

    files
    
let parseXMLSchema (inDir:string)(outDir:string) = 

    //printSchema(xmlPath)

    let root = inDir |> getRoot

    let rows = root |> Option.map (fun x -> x|> readSchema)

    //try to convert rows to marshalled row data if they exist

    //create the XElement row field predicates
    let predXName = {Namespace = "urn:schemas-microsoft-com:rowset"; LocalName = "name"}
    let predXType = {Namespace = "uuid:C2F41010-65B3-11d1-A29F-00AA00C14882"; LocalName = "type"}

    //create predicates for marshalled row name and types to be replaced
    let predsMName= [
        {Target = ":"; Replacing = "_"}
        {Target = "ü"; Replacing = "ue"}
        {Target = "ä"; Replacing = "ae"}
        {Target = " "; Replacing = "_"}
    ]

    let predsMType = [
        {Target = "i4"; Replacing = "id_String"}
        {Target = "float"; Replacing = "id_Float"} //id_Float
        {Target = "string"; Replacing = "id_String"}
        {Target = "datetime"; Replacing ="id_String"} //id_Date
        {Target = "boolean"; Replacing = "id_String"} //id_Boolean
        {Target = "variant";Replacing = "id_String"}
        {Target = "String"; Replacing = "id_String"}
    ]

    match rows with
    | Some(rs) -> 
        let infos = getRowInfos rs (predXName, predXType)

        //the marshalled row data where content has been filtered
        let marshd = replaceRowData infos (predsMName, predsMType)

        //get a sequence of talend xml column elements
        let converted = seq{ for m in marshd -> m |> t_column}

        let schema = converted |> Seq.toList |> t_schema

        let document = schema |> t_schemaWrapper

        document.Save(outDir)

        ()

    | None -> ()

[<EntryPoint>]
let main argv =

    //the commands (input and output directories, later on also string replacements)
    //are read from a commandList text file
    let files = "D:\Work\MOGI\04_Projects\RTATV\Parser\Input\commandList.txt" |> getFiles

    for f in files do
        
        let (_i, _o) = f

        parseXMLSchema _i _o

        ()

    
    0 // return an integer exit code