namespace XMLUtil

open System.IO
open System.Xml.Linq
open System.Globalization

type MarshalRow = {
    Name: string
    Type: string
}

module Parsing =

    //this function demonstrates the evaluatable strucutre of an xml document
    let printSchema(path:string)=

        if File.Exists(path)
            then
                let root = XElement.Load(path)

                let descendants (element:XElement) =
                    query{
                        for desc in element.Descendants() do
                            select desc
                    }

                let test (element:XElement)= 
                    query{
                        for desc in element.Descendants() do
                        where(desc.Name.LocalName.Length < 4)
                    }

                let nodes (element:XElement) =
                    query{
                        for node in element.Nodes() do
                            select node
                    }

                let attributes (element:XElement) =
                    query{
                        for attr in element.Attributes() do
                                    select attr
                    }
                
                //f# query expressions
                //  https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/query-expressions

                //linq to xml overview
                //  https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/linq-to-xml-overview

                //f# implicit conversion
                //  https://stackoverflow.com/questions/10719770/is-there-anyway-to-use-c-sharp-implicit-operators-from-f

                let inline (!>) (x: ^a) :^b =
                    ((^a or ^b):(static member implicit : ^a -> ^b) x)

                

                let schemaElements = descendants root

                schemaElements |> Seq.iter(
                    fun element -> 
                        element.Name |> printfn "%A"

                        descendants element |> Seq.iter(
                            fun desc -> 

                                printfn "Descendant:"

                                desc.Name.NamespaceName |> printfn "\tNamespace: %A"
                                desc.Name.LocalName |> printfn "\tName: %A"

                                attributes desc |> Seq.iter(
                                    fun attr -> 
                                        printfn "\t\tAttribute:"
                                        attr.Name.NamespaceName |> printfn "\t\t\tNamespace: %A"
                                        attr.Name.LocalName |> printfn "\t\t\tName: %A"
                                        attr.Value |> printfn "\t\t\tValue: %A"
                                )
                                

                        )
                )
                
                //learn: each element has attributes.
                //dynamically extract all elements and filter for specific ones (e.g. name and data type)
        ()

    let getRoot (path:string):XElement option =
            if File.Exists(path)
                then 
                    Some (XElement.Load(path))
                else 
                    None

    type ImpureRow = {
        mutable Name:XElement option
        mutable Type:XElement option
    }

    type PureRow = {
        Name: XElement
        Type: XElement
    }
    
    type XNamePredicate = {
        Namespace:string
        LocalName:string
    }



    //allows to replace datatypes and substrings in names
    type NamePredicate = {
        Target:string
        Replacing:string
    }

    //to declare a defaut xml element and get rid of option
    let private  defXel = Unchecked.defaultof<XElement>

    let private extract (row:ImpureRow) = 
    
        let mutable n = defXel
        let mutable t = defXel

        let mutable valid = true

        match row.Name with
        | Some(_n) -> n <- _n
        | None -> valid <- false

        if valid = false
            then None
            else
                match row.Type with
                | Some(_t) -> 
                        t <- _t
                        Some({PureRow.Name = n; PureRow.Type = t})
                | None -> None   
        
    let readSchema (root:XElement) =


        let schema = 
            query{
                for desc in root.Descendants() do
                    if desc.Name.LocalName = "Schema"
                        then select desc
            }

        let isRowInfo (element:XElement) =
            match element.Name.LocalName with
            | "AttributeType" -> true
            | "datatype" -> true
            | _ -> false

        let attributeTypes (schema:XElement) =
                query{
                    for desc in schema.Descendants() do
                        if desc |> isRowInfo
                            then select desc

                }

        let mutable rows: PureRow list = []

       
        let buildSchemaRepresentation schema =

            (*
                This function assumes that the entered schema already only contains
                the needed attributes where the local name is 
                "Attributetype" or "datatype"
            *)


            let buildRow (row:ImpureRow) (attr:XElement):ImpureRow =
                match attr.Name.LocalName with
                | "AttributeType" -> 
                    {Name = Some(attr); Type = None} //on AttrybuteType a new row is created
                | "datatype" -> 
                   {row with Type = Some attr} //on datatype the accumulator is updated
                | _ -> row //this case would not happen

            //scan is used because the intermediate accumulations are needed
            let unvalidatedBuild = Seq.scan buildRow {Name = None; Type = None} schema


            //this filters the intermediate accumulations for the used rows where
            let validateSchemaRepresentation schema =
                let Validation (row:ImpureRow) =
                     match row with
                     | {Name = Some(x); Type = Some(y)} -> true
                     | _ -> false

                Seq.filter Validation schema

            validateSchemaRepresentation unvalidatedBuild


        schema |> Seq.iter(

            let mutable currRow:ImpureRow = {Name = None; Type = None}

            fun sch -> 

                let schemaBuild = buildSchemaRepresentation (sch|>attributeTypes)

                List.ofSeq schemaBuild |> printfn "%A"

                sch |> attributeTypes |> Seq.iter(
                    fun attr ->

                        match attr.Name.LocalName with
                        | "AttributeType" -> (currRow <- {Name = Some attr; Type=None}) |> ignore
                        | "datatype" -> currRow<-{currRow with Type = Some attr} //currRow.Type <- Some attr
                        | _ -> ()

                        let extr = currRow |> extract

                        match extr with
                        | Some(r) -> rows <- List.append rows [r]
                        | None -> ()
                    
                )
        )
        rows


    //converts a single pure row to a row data marshalled object
    let private convertToMarshalled (row:PureRow) (matchesName: XAttribute -> bool, matchesType: XAttribute -> bool) =

        let attributes (element:XElement) =
             query{
                 for attr in element.Attributes() do
                             select attr
             }
            
            

        let mutable n = ""
        let mutable t = ""

        //get row name via attribute
        row.Name |> attributes |> Seq.iter(
            fun attr -> 
                if attr |> matchesName 
                    then n <- attr.Value
        )

        //get row type via attribute
        row.Type |> attributes |> Seq.iter(
            fun attr ->
                if attr |> matchesType
                    then t <- attr.Value
        )
              
        {MarshalRow.Name = n; MarshalRow.Type = t}
    

    //converts a list of rows to a list of marshalled row objects
    //no filters are applied to the marshalled row's fields
    let getRowInfos (rows:PureRow list)(namePredicate: XNamePredicate, typePredicate:XNamePredicate) =

        let mutable marshalled: MarshalRow list = []

        let createMatcher (pred:XNamePredicate) =

            fun (attribute:XAttribute) -> 
            
                let mutable isMatch = true

                if pred.Namespace.Length > 0 && attribute.Name.NamespaceName <> pred.Namespace
                    then isMatch <- false
                    else
                        if attribute.Name.LocalName <> pred.LocalName
                            then
                                isMatch <- false

                isMatch


        rows |> Seq.iter(
            fun r ->
                
                //perform a conversion from pure row data to marshalled row data
                //this is done by passing in predicates both for the name and for the type field of a row
                let m = convertToMarshalled r ((namePredicate |> createMatcher), (typePredicate |> createMatcher))

                marshalled <- m::marshalled
        )

        marshalled


    let private replaceSequences (row:MarshalRow)(predName:NamePredicate, predType:NamePredicate) =
               
        let rn = row.Name.Replace(predName.Target, predName.Replacing)

        let rt =
           if row.Type = predType.Target
            then predType.Replacing
            else row.Type

        {MarshalRow.Name = rn; MarshalRow.Type = rt}

    let replaceRowData (rows:MarshalRow list)(predsName:NamePredicate list, predsType:NamePredicate list) =
        
        let mutable replaced:MarshalRow list = []

        rows |> Seq.iter(
            fun r -> 

                let mutable curr = r
                
                for pn in predsName do
                    for pt in predsType do
                        curr <- replaceSequences curr (pn,pt)

                replaced <- curr::replaced
        )

        replaced


module Converting =

    type NameSet = 
    | LocalName of string 
    | GlobalName of (string * string)
    | NamespacedName of (XNamespace * string)

    type CosmeticAttribute = {
        Name:NameSet
        Value:obj
    }
    type RowTemplate = {
        ElementName: NameSet
        NameAttribute: NameSet
        TypeAttribute: NameSet
    }

    type xn = XName
    
    let private XName (from:NameSet) = 
        
        match from with
        | LocalName n -> xn.Get(n)
        | GlobalName (ns, ln) -> xn.Get(ns, ln)
        | NamespacedName (ns, n) -> (ns + n)


    let private contains (s1:string)(s2:string) =

        let culture = CultureInfo.InvariantCulture

        let dist = culture.CompareInfo.IndexOf(s1, s2, CompareOptions.IgnoreCase)

        if dist > 0 
            then true
            else false

    let private createAttribute (name:NameSet)(value:obj) =

        let xattr = XAttribute( name |> XName , value)

        xattr
    
    let convertRow (row:MarshalRow) (template:RowTemplate)(preceding: CosmeticAttribute list option) =

        let mutable elem = XElement(template.ElementName |> XName)

        let attrn = XAttribute(template.NameAttribute |> XName , row.Name)

        let attrt = XAttribute(template.TypeAttribute |> XName, row.Type)

        //create and add preceeding cosmetic attributes (cas) if required
        match preceding with
        |Some(cas) ->

            let attrPrec = seq{
                for c in cas -> 
                    let attr = createAttribute c.Name c.Value
                    attr
            }

            attrPrec |> Seq.iter elem.Add
                
            

        |None -> ()

        elem.Add(attrn, attrt)

        elem

    type TalendGenericSchema = {
        ConnectionId:string
        PackageId:string
    }

    let t_featureRow (row:MarshalRow) =

        let xsi = XNamespace.Get("http://www.w3.org/2001/XMLSchema-instance")
        let xmi = XNamespace.Get("http://www.omg.org/XMI")
        
        let ft =  XElement(LocalName "feature" |> XName, 
            createAttribute (NamespacedName (xsi, "type")) "TalendMetadata:MetadataColumn",
            createAttribute (NamespacedName (xmi, "id")) "some-unique-file-occurring id",
            createAttribute (LocalName "name") row.Name,
            createAttribute (LocalName "label") row.Name,
            createAttribute (LocalName "length") "-1",
            createAttribute (LocalName "talendType") row.Type,
            createAttribute (LocalName "nullable") "false"
        )

        //add pattern attribute if type is date
        if contains row.Type "date"
            then
                ft.Add(
                    createAttribute (LocalName "pattern") "\"yyyy-MM-dd\""
                )

        ft

    //provides ability to create a generic schema connection by package id and connection id
    let t_genericSchemaConnection (schemadata:TalendGenericSchema) = 

        let xmi = XNamespace.Get("http://www.omg.org/XMI")
        let talendMetadata = XNamespace.Get("http://www.talend.org/metadata/connection/2010")


        let gs = XElement(NamespacedName (talendMetadata, "SchemaConnectinon") |> XName,
            createAttribute (NamespacedName(xmi, "id")) schemadata.ConnectionId,
            createAttribute (LocalName "name") "repository.metadataGenericSchema",
            createAttribute (LocalName "dataPackage") schemadata.PackageId
        )

        gs

    //provides ability to create a generic package by package id and connection id
    let t_genericPackage (schemadata:TalendGenericSchema) =

        let xmi = XNamespace.Get("http://www.omg.org/XMI")
        let talendMetadata = XNamespace.Get("http://www.talend.org/metadata/connection/2010")
  
            
        let gp = XElement(NamespacedName (talendMetadata, "GenericSchema") |> XName, 
            createAttribute (NamespacedName (xmi, "id")) schemadata.PackageId,
            createAttribute (LocalName "dataManager") schemadata.ConnectionId,
            createAttribute (LocalName "name") "repository.metadataGenericSchema"
        )

        gp


    let t_ownedElement (rows:XElement list) =

        let xsi = XNamespace.Get("http://www.w3.org/2001/XMLSchema-instance")
        let xmi = XNamespace.Get("http://www.omg.org/XMI")

        let mutable oe = XElement(LocalName "ownedElement" |> XName, 
            createAttribute (NamespacedName (xsi, "type")) "TalendMetadata:MetadataTable",
            createAttribute (NamespacedName (xmi, "id")) "xmi-id",
            createAttribute (LocalName "id") "local id",
            createAttribute (LocalName "comment") "Created by F-Sharp Code!",
            createAttribute (LocalName "label") "Codegenerated_Schema"
        )

        for row in rows do
            oe.Add(row)

        oe

    //creates complete document wrapper (works as expected)
    let createWrapper =
        
        let xsi = XNamespace.Get("http://www.w3.org/2001/XMLSchema-instance")
        let xmi = XNamespace.Get("http://www.omg.org/XMI")
        let talendMetadata = XNamespace.Get("http://www.talend.org/metadata/connection/2010")

        let wrapper = XDocument(
            XElement(xmi + "XMI",
                XAttribute(XNamespace.Xmlns + "xmi", xmi),
                XAttribute(XNamespace.Xmlns + "xsi", xsi),
                XAttribute(XNamespace.Xmlns + "TalendMetadata", talendMetadata),
                createAttribute (NamespacedName (xmi, "version")) "2.0"
            )
        )

        wrapper

    //apparently, talend can also import only the schema columns from an xml, so there`s no need to create the whole generic schema
    let t_column (row:MarshalRow)= 
        
        let col = XElement( LocalName "column" |> XName,
            createAttribute (LocalName "comment") "",
            createAttribute (LocalName "default") "",
            createAttribute (LocalName "key") "false", //indicates a talend options whether or not the column is a db key
            createAttribute (LocalName "label") row.Name,
            createAttribute (LocalName "length") "-1", //dynamic field length is -1
            createAttribute (LocalName "nullable") "true",
            createAttribute (LocalName "originalDbColumnName") row.Name,
            createAttribute (LocalName "precision") "0", //idk what this does, but all columns have it
            createAttribute (LocalName "talendType") row.Type
        )

        let mutable pattern = ""

        //add pattern attribute if type is date
        if contains row.Type "date"
           then 
            pattern <- "\"yyyy-MM-dd\""


        col.Add(
            createAttribute (LocalName "pattern") pattern
        )

        col


    let t_schema (rows:XElement list) = 
        
        let sch = XElement(LocalName "schema" |> XName)
        
        //rows are somehow added reversed
        
        let reversed = rows |> List.rev

        for r in reversed do
            sch.Add(r)

        sch

    let t_schemaWrapper (schema:XElement)=
        
        let doc = XDocument(schema)

        doc

module Commands =

    type FileTask = {
        InputTarget: DirectoryInfo
        OutputTarger: DirectoryInfo
    }

    type SemanticTask = {
        TargetSequence:string
        ReplacingSequence:string
    }

    type TaskList = {
        Files: FileTask list
        Semantics: SemanticTask list
    }