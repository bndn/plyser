/// Copyright (C) 2016 The Authors.
module Plyser

type Scalar = Char | UChar | Short | UShort | Int | UInt | Float | Double

type Value = Int of int | Float of float

type Token =
    // Header tokens.
    | Ply | Format | Element | Property | EndHeader
    // Scalar type tokens.
    | Scalar of Scalar | List
    // Scalar value tokens.
    | Value of Value
    // String value tokens.
    | Name of string | Comment of string
    // Miscellaneous tokens.
    | Break

type PropertyDefinition =
    | Scalar of Scalar * string
    | List of Scalar * Scalar * string

type ElementDefinition = ElementDefinition of PropertyDefinition list * int * string

type Property =
    | Char of sbyte
    | UChar of byte
    | Short of int16
    | UShort of uint16
    | Int of int
    | UInt of uint32
    | Float of single
    | Double of double
    | List of Property list

[<NoComparison>]
type Element = Element of string * Map<string, Property>

/// <summary>
/// Raised in case of an exception during tokenisation.
/// </summary>
exception TokenisationException

/// <summary>
/// Get the name of an element.
/// </summary>
/// <param name=e>The element whose name to get.</param>
/// <returns>The name of the element.</returns>
let getName (Element(n, _)) = n

/// <summary>
/// Get the value of a char property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getChar p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(Char p) -> Some p
    | _ -> None

/// <summary>
/// Get the value of a char list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getCharList p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(List ps) ->
        let f p ps =
            match p with
            | Char p -> p :: ps
            | _ -> ps

        Some(List.foldBack f ps [])
    | _ -> None

/// <summary>
/// Get the value of an unsigned char property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getUChar p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(UChar p) -> Some p
    | _ -> None

/// <summary>
/// Get the value of an unsigned char list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getUCharList p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(List ps) ->
        let f p ps =
            match p with
            | UChar p -> p :: ps
            | _ -> ps

        Some(List.foldBack f ps [])
    | _ -> None

/// <summary>
/// Get the value of a short property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getShort p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(Short p) -> Some p
    | _ -> None

/// <summary>
/// Get the value of a short list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getShortList p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(List ps) ->
        let f p ps =
            match p with
            | Short p -> p :: ps
            | _ -> ps

        Some(List.foldBack f ps [])
    | _ -> None

/// <summary>
/// Get the value of an unsigned short property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getUShort p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(UShort p) -> Some p
    | _ -> None

/// <summary>
/// Get the value of an unsigned short list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getUShortList p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(List ps) ->
        let f p ps =
            match p with
            | UShort p -> p :: ps
            | _ -> ps

        Some(List.foldBack f ps [])
    | _ -> None

/// <summary>
/// Get the value of an int property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getInt p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(Int p) -> Some p
    | _ -> None

/// <summary>
/// Get the value of an int list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getIntList p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(List ps) ->
        let f p ps =
            match p with
            | Int p -> p :: ps
            | _ -> ps

        Some(List.foldBack f ps [])
    | _ -> None

/// <summary>
/// Get the value of an unsigned int property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getUInt p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(UInt p) -> Some p
    | _ -> None

/// <summary>
/// Get the value of an unsigned int list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getUIntList p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(List ps) ->
        let f p ps =
            match p with
            | UInt p -> p :: ps
            | _ -> ps

        Some(List.foldBack f ps [])
    | _ -> None

/// <summary>
/// Get the value of a float property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getFloat p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(Float p) -> Some p
    | _ -> None

/// <summary>
/// Get the value of a float list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getFloatList p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(List ps) ->
        let f p ps =
            match p with
            | Float p -> p :: ps
            | _ -> ps

        Some(List.foldBack f ps [])
    | _ -> None

/// <summary>
/// Get the value of a double property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getDouble p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(Double p) -> Some p
    | _ -> None

/// <summary>
/// Get the value of a double list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
let getDoubleList p (Element(_, m)) =
    match Map.tryFind p m with
    | Some(List ps) ->
        let f p ps =
            match p with
            | Double p -> p :: ps
            | _ -> ps

        Some(List.foldBack f ps [])
    | _ -> None

/// <summary>
/// Check if a characters is a newline.
/// </summary>
let isNewline c = c = '\n'

/// <summary>
/// Check if a character is blank, i.e. whitespace.
/// </summary>
let isBlank c = System.Char.IsWhiteSpace c

/// <summary>
/// Check if a character is a letter.
/// </summary>
let isLetter c = System.Char.IsLetter c

/// <summary>
/// Check if a characters is punctuation.
/// </summary>
let isPunctuation c = System.Char.IsPunctuation c

/// <summary>
/// Check if a character is a digit.
/// </summary>
let isDigit c  = System.Char.IsDigit c

/// <summary>
/// Convert a character to its integer version.
/// </summary>
let value (c:char) = int c - int '0'

/// <summary>
/// Take a number of elements from a list.
/// </summary>
let rec take n xs = function
    | x :: xr when not (n = 0) -> take (n - 1) (x :: xs) xr
    | xr -> (List.rev xs, xr)

/// <summary>
/// Scan a character sequence and construct a numerical token.
/// </summary>
/// <param name=n>The accumulating numerical value.</param>
/// <param name=cs>The character sequence to scan.</param>
/// <returns>The remaining characters and the value accumulated so far.</returns>
let rec tokeniseNumber (n, s, cs) =
    match cs with
    | '.' :: c :: cr when isDigit c -> tokeniseDecimal (float n, float s, 0.1, c :: cr)
    |  c       :: cr when isDigit c -> tokeniseNumber (10 * n + value c, s, cr)
    | 'e' :: '-' :: cr -> tokeniseScientific (float n, float s, -1, cr)
    | 'e'        :: cr -> tokeniseScientific (float n, float s, 0, cr)
    | _ -> Value(Value.Int (n * s)), cs

/// <summary>
/// Scan a character sequence and construct a floating point token.
/// </summary>
/// <param name=n>The accumulating decimal value.</param>
/// <param name=pv>The place value of the decimal value.</param>
/// <param name=cs>The character sequence to scan.</param>
/// <returns>The remaining characters and the value accumulated so far.</returns>
and tokeniseDecimal (n, s, pv, cs) =
    match cs with
    |  c :: cr when isDigit c -> tokeniseDecimal (n + pv * float (value c), s, pv / 10.0, cr)
    | 'e' :: '-' :: cr -> tokeniseScientific (n, s, -1, cr)
    | 'e'        :: cr -> tokeniseScientific (n, s, 0, cr)
    | _ -> (Value(Value.Float (n * s)), cs)

/// <summary>
/// Scan a character sequence and construct a floating point token in scientific notation.
/// </summary>
/// <param name=n>The accumulating decimal value.</param>
/// <param name=e>The accumulating exponent value.</param>
/// <param name=cs>The character sequence to scan.</param>
/// <returns>The remaining characters and the value accumulated so far.</returns>
and tokeniseScientific (n, s, e, cs) =
    match cs with
    | c :: cr when isDigit c ->
        let v = value c
        if (e = 0 || e = -1) && v = 0
        then tokeniseScientific (n, s, e, cr)
        else tokeniseScientific (n, s, 10 * e + value c, cr)
    | _ -> (Value(Value.Float (n * (10.0 ** float e) * s)), cs)

/// <summary>
/// Scan a characters sequence and construct a name token.
/// </summary>
let rec tokeniseName = function
    // Continue building the name until whitespace is encountered.
    | w, c :: cr when not (isBlank c) -> tokeniseName (w + string c, cr)
    // Check if we've encountered a keyword.
    | "ply", cs        -> Token.Ply, cs
    | "end_header", cs -> Token.EndHeader, cs
    | "format", cs     -> Token.Format, cs
    | "element", cs    -> Token.Element, cs
    | "property", cs   -> Token.Property, cs
    | "list", cs       -> Token.List, cs
    | "char", cs       -> Token.Scalar Scalar.Char, cs
    | "uchar", cs      -> Token.Scalar Scalar.UChar, cs
    | "short", cs      -> Token.Scalar Scalar.Short, cs
    | "ushort", cs     -> Token.Scalar Scalar.UShort, cs
    | "int", cs        -> Token.Scalar Scalar.Int, cs
    | "int32", cs      -> Token.Scalar Scalar.Int, cs
    | "uint", cs       -> Token.Scalar Scalar.UInt, cs
    | "uint8", cs      -> Token.Scalar Scalar.UInt, cs
    | "float", cs      -> Token.Scalar Scalar.Float, cs
    | "float32", cs    -> Token.Scalar Scalar.Float, cs
    | "double", cs     -> Token.Scalar Scalar.Double, cs
    | "comment", cs | ("obj_info", cs) ->
        match cs with
        | c :: cr when isBlank c -> let l, cr = tokeniseLine ("", cr) in Token.Comment l, cr
        | _ -> Token.Comment "", cs
    // Otherwise, we've encountered a generic name.
    | w, cs -> Name w, cs

/// <summary>
/// Scan a character sequence and construct a line token.
/// </summary>
and tokeniseLine = function
    | l, c :: cr when not (isNewline c) -> tokeniseLine (l + string c, cr)
    | l, cs -> l, cs

/// <summary>
/// Scan a character sequence and construct a list of tokens.
/// </summary>
/// <param name=s>The character sequence to scan.</param>
/// <returns>The constructed token list.</returns>
let tokenise s =
    let rec scan ts = function
    |        c :: cr when isNewline c -> scan (Break :: ts) cr
    |        c :: cr when isBlank   c -> scan ts cr
    |        c :: cr when isLetter  c -> let t, cr = tokeniseName (string c, cr)      in scan (t :: ts) cr
    |        c :: cr when isDigit   c -> let t, cr = tokeniseNumber (value c, 1, cr)  in scan (t :: ts) cr
    | '-' :: c :: cr when isDigit   c -> let t, cr = tokeniseNumber (value c, -1, cr) in scan (t :: ts) cr
    | [] -> List.rev ts
    | _  -> raise TokenisationException

    let rec normalise acc = function
    | Break :: Break :: tr -> normalise acc (Break :: tr)
    | t              :: tr -> normalise (t :: acc) tr
    | []                   -> List.rev acc

    normalise [] (scan [] [for e in s -> e])

/// <summary>
/// The entry point to the PLY parsing process.
/// </summary>
/// <remarks>
///     <code>
///     ply = eol ply | "ply" eol format header body
///     </code>
/// </remarks>
let rec ply = function
    | Token.Break :: tr -> ply tr
    | Token.Ply   :: tr ->
        let tr = (eol >> format) tr
        let es, tr = header [] tr
        let is, tr = body [] tr

        match tr with
        | [] -> es, is
        | tr -> failwith (sprintf "Unexpected token %A" tr)
    | tr -> failwith (sprintf "Expected a PLY header, got %A" tr)

/// <summary>
/// Parse a name.
/// </summary>
and name = function
    | Token.Name n :: tr -> n, tr
    | tr -> failwith (sprintf "Expected a name, got %A" tr)

/// <summary>
/// Parse a float.
/// </summary>
and float' = function
    | Token.Value(Value.Float f) :: tr -> f, tr
    | tr -> failwith (sprintf "Expected a float, got %A" tr)

/// <summary>
/// Parse an integer.
/// </summary>
and int' = function
    | Token.Value(Value.Int i) :: tr -> i, tr
    | tr -> failwith (sprintf "Expected an integer, got %A" tr)

/// <summary>
/// Parse a scalar type.
/// </summary>
/// <remarks>
///     <code>
///     scalar = "char" | "uchar" | "short" | "ushort" | "int" | "uint" | "float" | "double"
///     </code>
/// </remarks>
and scalar = function
    | Token.Scalar s :: tr -> s, tr
    | tr -> failwith (sprintf "Expected a scalar, got %A" tr)

/// <summary>
/// Parse a line or file ending.
/// </summary>
/// <remarks>
///     <code>
///     eol = "\n"
///     </code>
/// </remarks>
and eol = function
    | Token.Break :: tr -> tr
    | tr -> failwith (sprintf "Expected a line break, got %A" tr)

/// <summary>
/// Parse a comment.
/// </summary>
/// <remarks>
///     <code>
///     comment = string eol
///     </code>
/// </remarks>
and comment = function
    | Token.Comment _ :: tr -> eol tr
    | tr -> failwith (sprintf "Expected a comment, got %A" tr)

/// <summary>
/// Parse a format definition.
/// </summary>
/// <remarks>
///     <code>
///     format = "format" name float eol
///     </code>
/// </remarks>
and format = function
    | Token.Format :: tr -> (name >> snd >> float' >> snd >> eol) tr
    | tr -> failwith (sprintf "Expected a format, got %A" tr)

/// <summary>
/// Parse a header.
/// </summary>
/// <remarks>
///     <code>
///     header = element header | comment header | "end_header" eol
///     </code>
/// </remarks>
and header es = function
    | Token.Element   :: _ as ts -> let e, tr = element ts in header (e :: es) tr
    | Token.Comment _ :: _ as ts -> (comment >> header es) ts
    | Token.EndHeader ::      tr -> let tr = eol tr in (List.rev es, tr)
    | tr -> failwith (sprintf "Expected an element or a header end, got %A" tr)

/// <summary>
/// Parse an element.
/// </summary>
/// <remarks>
///     <code>
///     element = "element" name int eol properties
///     </code>
/// </remarks>
and element = function
    | Token.Element :: tr ->
        let n, tr = name tr
        let i, tr = int' tr
        let p, tr = properties [] (eol tr)
        (ElementDefinition(p, i, n), tr)
    | tr -> failwith (sprintf "Expected an element, got %A" tr)

/// <summary>
/// Parse a list of properties.
/// </summary>
/// <remarks>
///     <code>
///     properties = property | property properties
///     </code>
/// </remarks>
and properties ps = function
    | Token.Property :: _ as ts ->
        let p, tr = property ts
        match tr with
        | Token.Property :: _ -> properties (p :: ps) tr
        | _ -> (List.rev (p :: ps), tr)
    | tr -> failwith (sprintf "Expected a property, got %A" tr)

/// <summary>
/// Parse a property.
/// </summary>
/// <remarks>
///     <code>
///     property = "property" scalar name eol | "property" "list" scalar scalar name eol
///     </code>
/// </remarks>
and property = function
    | Token.Property :: tr ->
        match tr with
        | Token.Scalar _ :: _  ->
            let s, tr = scalar tr
            let n, tr = name tr
            (PropertyDefinition.Scalar(s, n), eol tr)
        | Token.List :: tr ->
            let s1, tr = scalar tr
            let s2, tr = scalar tr
            let n, tr = name tr
            (PropertyDefinition.List(s1, s2, n), eol tr)
        | tr -> failwith (sprintf "Expected a scalar or a List, got %A" tr)
    | tr -> failwith (sprintf "Expected a property, got %A" tr)

/// <summary>
/// Parse an item.
/// </summary>
/// <remarks>
///     <code>
///     item = int item | int | float item | float
///     </code>
/// </remarks>
and item vs = function
    | Token.Value(v) :: tr -> match tr with
                              | Value _ :: _ -> item (v :: vs) tr
                              | _ -> List.rev (v :: vs), tr
    | tr -> failwith (sprintf "Expected an item, got %A" tr)

/// <summary>
/// Parse a body.
/// </summary>
/// <remarks>
///     <code>
///     body = item . | item eol body | .
///     </code>
/// </remarks>
and body is = function
    | Value _ :: _ as ts ->
        let vs, tr = item [] ts

        match tr with
        | Break :: _ as tr -> let tr = eol tr in body (vs :: is) tr
        | [] -> List.rev (vs :: is), []
        | tr -> failwith (sprintf "Unexpected token %A" tr)
    | [] -> List.rev is, []
    | tr -> failwith (sprintf "Expected an item, got %A" tr)

/// <summary>
/// Given a value and scalar tuple, construct a property.
/// </summary>
let toProperty v s =
    match (v, s) with
    | (Value.Int   i, Scalar.Char)   -> Property.Char(sbyte i)
    | (Value.Int   i, Scalar.UChar)  -> Property.UChar(byte i)
    | (Value.Int   i, Scalar.Short)  -> Property.Short(int16 i)
    | (Value.Int   i, Scalar.UShort) -> Property.UShort(uint16 i)
    | (Value.Int   i, Scalar.Int)    -> Property.Int i
    | (Value.Int   i, Scalar.UInt)   -> Property.UInt(uint32 i)
    | (Value.Int   i, Scalar.Float)  -> Property.Float(single i)
    | (Value.Int   i, Scalar.Double) -> Property.Double(double i)
    | (Value.Float f, Scalar.Float)  -> Property.Float(single f)
    | (Value.Float f, Scalar.Double) -> Property.Double(double f)
    | (v, s) -> failwith (sprintf "Type declaration \"%A\" does not match value \"%A\"" s v)

/// <summary>
/// Given a value list and scalar tuple, construct a property list.
/// </summary>
let toPropertyList vs s =
    let ps = List.fold (fun ps v -> (toProperty v s) :: ps) [] vs
    Property.List (List.rev ps)

/// <summary>
/// Given a name and property map tuple, construct an element.
/// </summary>
let toElement n pm = Element(n, pm)

/// <summary>
/// Construct properties from a list of property definitions and parsed items.
/// </summary>
let rec defineProperties (pm, vs) pds =
    match vs, pds with
    | v :: vr, PropertyDefinition.Scalar(s, n) :: pds ->
        defineProperties (Map.add n (toProperty v s) pm, vr) pds
    | v :: vr, PropertyDefinition.List(_, s, n) :: pds ->
        let i = match v with
                | Value.Int   i -> i
                | Value.Float f -> int f

        let vr, vrr = take i [] vr

        let j = List.length vr

        if not (j = i)
        then failwith (sprintf "Expected %i values in list, got %i" i j)

        defineProperties (Map.add n (toPropertyList vr s) pm, vrr) pds
    | ([], []) -> pm
    | (_, []) -> failwith "Expected fewer properties"
    | ([], _) -> failwith "Expected more properties"

/// <summary>
/// Construct elements from a list of elements definitions and parsed items.
/// </summary>
let rec defineElements (es, vss) = function
    | ElementDefinition(_,  i, _) :: eds when i = 0 -> defineElements (es, vss) eds
    | ElementDefinition(ps, i, n) :: eds ->
        match vss with
        | vs :: vss ->
            let pm = defineProperties (Map.empty, vs) ps
            let e = toElement n pm
            let ed = ElementDefinition(ps, i - 1, n)
            defineElements (e :: es, vss) (ed :: eds)
        | _ -> failwith "Expected more elements"
    | [] ->
        if not (List.isEmpty vss)
        then failwith "Expected fewer elements"
        List.rev es

/// <summary>
/// Parse a PLY-formatted characters sequence to a list of elements.
/// </summary>
/// <param name=s>The character sequence to parse.</param>
/// <returns>The list of parsed elements.</returns>
let parse s = let eds, vss = ply (tokenise s) in defineElements ([], vss) eds
