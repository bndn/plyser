/// Copyright (C) 2016 The Authors.
module Plyser

type Element

/// <summary>
/// Get the name of an element.
/// </summary>
/// <param name=e>The element whose name to get.</param>
/// <returns>The name of the element.</returns>
val getName : e:Element -> string

/// <summary>
/// Get the value of a char property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getChar : p:string -> e:Element -> sbyte option

/// <summary>
/// Get the value of a char list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getCharList : p:string -> e:Element -> sbyte list option

/// <summary>
/// Get the value of an unsigned char property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getUChar : p:string -> e:Element -> byte option

/// <summary>
/// Get the value of an unsigned char list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getUCharList : p:string -> e:Element -> byte list option

/// <summary>
/// Get the value of a short property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getShort : p:string -> e:Element -> int16 option

/// <summary>
/// Get the value of a short list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getShortList : p:string -> e:Element -> int16 list option

/// <summary>
/// Get the value of an unsigned short property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getUShort : p:string -> e:Element -> uint16 option

/// <summary>
/// Get the value of an unsigned short list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getUShortList : p:string -> e:Element -> uint16 list option

/// <summary>
/// Get the value of an int property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getInt : p:string -> e:Element -> int option

/// <summary>
/// Get the value of an int list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getIntList : p:string -> e:Element -> int list option

/// <summary>
/// Get the value of an unsigned int property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getUInt : p:string -> e:Element -> uint32 option

/// <summary>
/// Get the value of an unsigned int list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getUIntList : p:string -> e:Element -> uint32 list option

/// <summary>
/// Get the value of a float property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getFloat : p:string -> e:Element -> single option

/// <summary>
/// Get the value of a float list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getFloatList : p:string -> e:Element -> single list option

/// <summary>
/// Get the value of a double property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getDouble : p:string -> e:Element -> double option

/// <summary>
/// Get the value of a double list property from an element.
/// </summary>
/// <param name=e>The element whose property to get.</param>
/// <returns>The value of the property, if found.</returns>
val getDoubleList : p:string -> e:Element -> double list option

/// <summary>
/// Parse a PLY-formatted characters sequence to a list of elements.
/// </summary>
/// <param name=s>The character sequence to parse.</param>
/// <returns>The list of parsed elements.</returns>
val parse : s:char seq -> Element list
