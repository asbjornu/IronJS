namespace IronJS

  #nowarn "9"

  module Types =

    open IronJS
    open IronJS.Aliases

    open System
    open System.Reflection
    open System.Reflection.Emit
    open System.Runtime.InteropServices

    //-------------------------------------------------------------------------
    // Aliases for .NET core types to give them more convenient names
    type ClrType        = System.Type //So we have "ClrType" and "JsType"
    type ClrObject      = System.Object
    type StrongBox<'a>  = System.Runtime.CompilerServices.StrongBox<'a>

    //-------------------------------------------------------------------------
    // Enumeration of all javascript types and valid combinations
    type JsType
      = Nothing   = 0   // NOT null
      | Closure   = 1
      | Number    = 2
      | Boolean   = 4
      | String    = 8

      | Object    = 16
      | Function  = 32
      | Array     = 64

      | Undefined = 128
      | Null      = 256
      | Dynamic   = 512
      | Clr       = 1024
      | ClrNull   = 1280 // Clr | Null
  
      // Special combined types to allow us to keep 
      // strong typing if the only non-typed value is null
      | StringNull    = 264 // String | Null
      | ObjectNull    = 272 // Object | Null
      | FunctionNull  = 288 // Function | Null
      | ArrayNull     = 320 // Array | Null
      | UndefinedNull = 384 // Undefined | Null

      // Special combined types for JavaScript objects
      // to allow strong typing as Runtime.Object
      | ObjFuncArr      = 112 // Object | Function | Array
      | ObjFuncArrNull  = 368 // Object | Function | Array | Null
      | ObjArr          = 80  // Object | Array
      | ObjArrNull      = 336 // Object | Array | Null
      | ObjFunc         = 48  // Object | Function
      | ObjFuncNull     = 304 // Object | Function | Null
      | ArrFunc         = 96  // Array | Function
      | ArrFuncNull     = 352 // Array | Function | Null

    //-------------------------------------------------------------------------
    // Struct used to box dynamic values during runtime
    type [<StructLayout(LayoutKind.Explicit)>] Box =
      struct
        [<FieldOffset(0)>]  val mutable Clr    : obj 
        [<FieldOffset(8)>]  val mutable Bool   : bool
        [<FieldOffset(8)>]  val mutable Double : double
        [<FieldOffset(16)>] val mutable Type   : JsType

        #if FAST_CAST
        [<FieldOffset(0)>] val mutable Object : Object
        [<FieldOffset(0)>] val mutable Func   : Function
        [<FieldOffset(0)>] val mutable String : string
        #endif
      end
    
    //-------------------------------------------------------------------------
    // Type representing javascript 'undefined'
    type Undefined() =
      static let instance = new Undefined()
      static member Instance = instance
      static member InstanceExpr = Dlr.constant instance
      static member Boxed = 
        let mutable box = new Box()
        box.Type <- JsType.Undefined
        box.Clr  <- Undefined.Instance
        box
      
    //-------------------------------------------------------------------------
    // Base class for all objects
    type [<AllowNullLiteral>] Object = 
      val mutable Properties : Map<string, Box>

      new() = {
        Properties = Map.empty
      }

      member x.Set (name:string, value: Box) =
        x.Properties <- Map.add name value x.Properties
        value

      member x.Get (name:string) =
        match Map.tryFind name x.Properties with
        | None -> Undefined.Boxed
        | Some(value) -> value
      
    //-------------------------------------------------------------------------
    // Base class for all functions
    and [<AllowNullLiteral>] Function =
      inherit Object

      val mutable Closure : Closure
      val mutable Compile : Func<ClrType, Delegate>

      new(closure, compile) = {
        inherit Object()
        Closure = closure
        Compile = compile
      }

      member x.CompileAs<'a when 'a :> Delegate>() =
        x.Compile.Invoke(typeof<'a>) :?> 'a

    //-------------------------------------------------------------------------
    // Base class for all closures
    and Closure = 
      val mutable Env : Environment
      val mutable Scopes : Scope array

      new (env, scopes, nextScope) = {
        Env = env
        Scopes = Dlr.ArrayUtils.Append(scopes, nextScope)
      }

      new (env, scopes) = {
        Env = env
        Scopes = scopes
      }

      new (env) = {
        Env = env
        Scopes = [||]
      }
      
    //-------------------------------------------------------------------------
    // 
    and Scope =
      val mutable Names : Map<string, int>
      val mutable Values : Box array

      new (values, names) = {
        Names = names
        Values = Array.zeroCreate<Box> values
      }
      
    //-------------------------------------------------------------------------
    // Environment object containing global scope, etc.
    and [<AllowNullLiteral>] Environment =
      val mutable Globals : Object
      
      new () = {
        Globals = new Object()
      }
      
    //-------------------------------------------------------------------------
    // Converts a ClrType object to a JsType enum value
    let clrToJs (type':ClrType) =
      if   type' = typeof<double>     then JsType.Number
      elif type' = typeof<string>     then JsType.String
      elif type' = typeof<bool>       then JsType.Boolean
      elif type' = typeof<Object>     then JsType.Object
      elif type' = typeof<Function>   then JsType.Function
      elif type' = typeof<Undefined>  then JsType.Undefined
      elif type' = typeof<ClrObject>  then JsType.Clr
      elif type' = typeof<Closure>    then JsType.Closure
      elif type' = typeof<Box>        then JsType.Dynamic
      else failwithf "Invalid type '%s'" type'.Name
      
    //-------------------------------------------------------------------------
    // Normalizes a JsType enum value to one of the non-combined values
    let normalizeJsType type' =
      match type' with
      | JsType.Closure   -> JsType.Closure
      | JsType.Number    -> JsType.Number
      | JsType.Boolean   -> JsType.Boolean

      | JsType.StringNull 
      | JsType.String    -> JsType.String

      | JsType.ArrayNull
      | JsType.Array     -> JsType.Array

      | JsType.FunctionNull
      | JsType.Function  -> JsType.Function

      | JsType.UndefinedNull
      | JsType.Undefined -> JsType.Undefined

      | JsType.Null
      | JsType.Clr
      | JsType.ClrNull   -> JsType.Clr

      | JsType.Object
      | JsType.ObjectNull
      | JsType.ArrFunc
      | JsType.ArrFuncNull
      | JsType.ObjArr
      | JsType.ObjArrNull
      | JsType.ObjFunc
      | JsType.ObjFuncNull
      | JsType.ObjFuncArr
      | JsType.ObjFuncArrNull -> JsType.Object

      | _ -> JsType.Dynamic
      
    //-------------------------------------------------------------------------
    // Converts a JsType enum value to a ClrType object
    let rec jsToClr typ =
      match typ with
      | JsType.Closure    -> typeof<Closure>
      | JsType.Number     -> typeof<double>
      | JsType.Boolean    -> typeof<bool>
      | JsType.String     -> typeof<string>
      | JsType.Object     -> typeof<Object>
      | JsType.Function   -> typeof<Function>
      | JsType.Undefined  -> typeof<Undefined>
      | JsType.Dynamic    -> typeof<Box>
      | JsType.Null       
      | JsType.Clr        -> typeof<ClrObject>
      | _                 -> jsToClr (normalizeJsType typ)

    //-------------------------------------------------------------------------
    // Utility functions and values for dealing with all IronJS-related types
    module Utils =
      //-------------------------------------------------------------------------
      // Functions and values for working with Object objects
      module Object = 
        let isObject (type':ClrType) = type' = typeof<Object> || type'.IsSubclassOf(typeof<Object>)

      //-------------------------------------------------------------------------
      // Functions and values for working with Function objects
      module Function = 
        let isFunction (type':ClrType) = type' = typeof<Function>

      //-------------------------------------------------------------------------
      // Functions and values for working with Box structs
      module Box = 
        let isBox (type':ClrType) = type' = typeof<Box>

        let typeToFieldName (type':JsType) =
          match type' with
          | JsType.Boolean  -> "Bool"
          | JsType.Number   -> "Double"
          | _               -> "Clr"

        let boxValue (value:obj) =
          let type' = clrToJs (value.GetType())
          let mutable box = new Box() 

          match type' with
          | JsType.Boolean -> box.Bool <- (value :?> bool)
          | JsType.Number -> box.Double <- (value :?> double)
          | _ -> box.Clr <- value
          
          box.Type <- type'
          box
      //-------------------------------------------------------------------------
      // Functions and values for working with StrongBox<'a> objects
      module StrongBox =
        let typeDef = typedefof<System.Runtime.CompilerServices.StrongBox<_>>
        let make type' = typeDef.MakeGenericType([|type'|])
        let makeT<'a> = make typeof<'a>
        let isStrongBox (type':System.Type) = type'.IsGenericType && type'.GetGenericTypeDefinition() = typeDef

      //-------------------------------------------------------------------------
      // Functions and values for working with Closure objects
      module Closure =
        //-------------------------------------------------------------------------
        // This function and bindings are used to create closure classes with 
        // generic type parameters for closures, e.g: Closure<T0, T1, ...>. 
        // Created classes are cached and re-used with new type parameters.
        let private _assemblyName = new AssemblyName("IronJS.DynamicAssembly")
        let private _dynamicAssembly = AppDomain.CurrentDomain.DefineDynamicAssembly(_assemblyName, AssemblyBuilderAccess.RunAndCollect)
        let private _dynamicModule = _dynamicAssembly.DefineDynamicModule("Module", "IronJS.DynamicAssembly.dll")
        let private _closureCache = new Collections.Concurrent.ConcurrentDictionary<int, ClrType>()
        let private _closureFieldAttributes = FieldAttributes.Public
        let private _closureTypeAttributes = TypeAttributes.Public ||| TypeAttributes.AutoLayout 
                                             ||| TypeAttributes.Class ||| TypeAttributes.Sealed

        let rec createType (types:ClrType array) =
          if types.Length = 0 then
            typeof<Closure>
          else
            let typeCount = types.Length
            let success, type' = _closureCache.TryGetValue typeCount
            if success then type'.MakeGenericType(types)
            else
              let newType' = _dynamicModule.DefineType(sprintf "Closure%i" typeCount, _closureTypeAttributes, typeof<Closure>);
              let genericParams = newType'.DefineGenericParameters(Array.init typeCount (fun i -> sprintf "T%i" i))

              Array.iteri (fun i type' -> newType'.DefineField(sprintf "Item%i" i, type', _closureFieldAttributes) |> ignore) genericParams

              let concreteType = newType'.CreateType()

              let type' = 
                if _closureCache.TryAdd(typeCount, concreteType) 
                  then concreteType
                  else createType types

              type'.MakeGenericType(types)
      
      //-------------------------------------------------------------------------
      // Function + cache that creates delegates for IronJS functions, delegates
      // are cached because calling Dlr.delegateType with >16 types will generate
      // incomptabile delegates for the same arguments each time it's called.
      // E.g: Func<Closure, int, string, Box>
      let private _delegateCache = 
        new Collections.Concurrent.ConcurrentDictionary<System.RuntimeTypeHandle list, ClrType>()

      let createDelegateType (types:ClrType seq) =
        let key = Seq.fold (fun s (t:ClrType) -> t.TypeHandle :: s) [] types

        let rec createDelegateType' types =
          let success, func = _delegateCache.TryGetValue key
          if success then func
          else
            let funcType = Dlr.delegateType types
            if _delegateCache.TryAdd(key, funcType) 
              then funcType
              else createDelegateType' types

        createDelegateType' types