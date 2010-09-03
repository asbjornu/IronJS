namespace IronJS

  module Types =

    open IronJS
    open System
    open System.Reflection
    open System.Reflection.Emit

    type ClrType = System.Type

    (*
    *)
    type JsType
      = Nothing   = 0   // NOT null
      | Void      = 1
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

    (*
    *)
    let private _delegateCache = 
      new System.Collections.Concurrent.ConcurrentDictionary<
            System.RuntimeTypeHandle list, 
            ClrType
          >()

    let createDelegateType (types:ClrType seq) =
      let key = Seq.fold (fun s (t:ClrType) -> t.TypeHandle :: s) [] types

      let rec createDelegateType' types =
        let success, func = _delegateCache.TryGetValue key
        if success then func
        else
          let funcType = Dlr.Expr.delegateType types
          if _delegateCache.TryAdd(key, funcType) 
            then funcType
            else createDelegateType' types

      createDelegateType' types

    (*
    *)
    let private _assemblyName = new AssemblyName("IronJS.DynamicAssembly");
    let private _dynamicAssembly = AppDomain.CurrentDomain.DefineDynamicAssembly(_assemblyName, AssemblyBuilderAccess.Run);
    let private _dynamicModule = _dynamicAssembly.DefineDynamicModule("Module", "IronJS.DynamicAssembly.dll");
    let private _typeCache = new Collections.Concurrent.ConcurrentDictionary<int, ClrType>()
    let private _fieldAttributes = FieldAttributes.Public;
    let private _typeAttributes = TypeAttributes.Public ||| TypeAttributes.AutoLayout 
                                  ||| TypeAttributes.Class ||| TypeAttributes.Sealed

    let rec createClosureType (genericCount:int) =
      let success, type' = _typeCache.TryGetValue genericCount
      if success then type'
      else
        let newType' = _dynamicModule.DefineType(sprintf "Closure%i" genericCount, _typeAttributes);
        let genericParams = newType'.DefineGenericParameters(Array.init genericCount (fun i -> sprintf "T%i" i))

        Array.iteri (fun i gp -> newType'.DefineField(sprintf "Item%i" i, gp, _fieldAttributes) |> ignore) genericParams

        let concreteType = newType'.CreateType()

        if _typeCache.TryAdd(genericCount, concreteType) 
          then concreteType
          else createClosureType genericCount