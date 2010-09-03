namespace IronJS

  module Types =

    open IronJS
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

    let private _delegateCache = 
      new System.Collections.Concurrent.ConcurrentDictionary<
            System.RuntimeTypeHandle list, 
            ClrType
          >()

    let createDelegateType (types:ClrType seq) =
      let key = Seq.fold (fun s (t:ClrType) -> t.TypeHandle :: s) [] types

      let rec getFor' types =
        let success, func = _delegateCache.TryGetValue key
        if success then func
        else
          let funcType = Dlr.Expr.delegateType types
          if _delegateCache.TryAdd(key, funcType) 
            then funcType
            else getFor' types

      getFor' types