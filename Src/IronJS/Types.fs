namespace IronJS

  #nowarn "9"

  module Types =

    open IronJS
    open IronJS.Aliases

    open System
    open System.Reflection
    open System.Reflection.Emit
    open System.Runtime.InteropServices

    (*
    Alias for System.Type so we have "ClrType" and "JsTypes"
    *)
    type ClrType        = System.Type
    type StrongBox<'a>  = System.Runtime.CompilerServices.StrongBox<'a>

    (*
    Enumeration of all javascript types and combinations
    *)
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

    (*
    Struct used to box dynamic values during runtime
    *)
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

    let strongBoxTypeDef = typedefof<System.Runtime.CompilerServices.StrongBox<_>>
    let makeStrongBox type' = strongBoxTypeDef.MakeGenericType([|type'|])
    let makeStrongBoxT<'a> = makeStrongBox typeof<'a>

    type Undefined() =
      static let instance = new Undefined()
      static member Instance = instance
      static member InstanceExpr = Dlr.constant instance

    type Object = 
      
      val mutable ClassId: int64

      new (classId) = {
        ClassId = classId
      }

    and Function =

      val mutable Closure : Closure

      new(closure) = {
        Closure = closure
      }

    (*
    Base type for all closures
    *)
    and Closure = 

      val mutable Object : obj
      
      new(obj) = {
        Object = obj
      }

    (*
    *)
    let clrToJs (type':ClrType) =
      if   type' = typeof<double>             then JsType.Number
      elif type' = typeof<string>             then JsType.String
      elif type' = typeof<bool>               then JsType.Boolean
      elif type' = typeof<Object>             then JsType.Object
      elif type' = typeof<Function>           then JsType.Function
      elif type' = typeof<Undefined>          then JsType.Undefined
      elif type' = typeof<ClrObject>          then JsType.Clr
      elif type' = typeof<Closure>            then JsType.Closure
      elif type' = typeof<Box>                then JsType.Dynamic
      else failwithf "Invalid type '%s'" type'.Name

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

      | JsType.Dynamic 
      | JsType.Nothing -> JsType.Dynamic

      | _ -> failwithf "Unknown type %A" type'

    (*Converts a JsType enum to ClrType object*)
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

    (*
    *)
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

    (*
    *)
    let private _assemblyName = new AssemblyName("IronJS.DynamicAssembly");
    let private _dynamicAssembly = AppDomain.CurrentDomain.DefineDynamicAssembly(_assemblyName, AssemblyBuilderAccess.RunAndCollect);
    let private _dynamicModule = _dynamicAssembly.DefineDynamicModule("Module", "IronJS.DynamicAssembly.dll");
    let private _typeCache = new Collections.Concurrent.ConcurrentDictionary<int, ClrType>()
    let private _fieldAttributes = FieldAttributes.Public;
    let private _typeAttributes = TypeAttributes.Public ||| TypeAttributes.AutoLayout 
                                  ||| TypeAttributes.Class ||| TypeAttributes.Sealed

    let rec createClosureType (genericCount:int) =
      let success, type' = _typeCache.TryGetValue genericCount
      if success then type'
      else
        let newType' = _dynamicModule.DefineType(sprintf "Closure%i" genericCount, _typeAttributes, typeof<Closure>);
        let genericParams = newType'.DefineGenericParameters(Array.init genericCount (fun i -> sprintf "T%i" i))

        Array.iteri (fun i gp -> newType'.DefineField(sprintf "Item%i" i, gp, _fieldAttributes) |> ignore) genericParams

        let concreteType = newType'.CreateType()

        if _typeCache.TryAdd(genericCount, concreteType) 
          then concreteType
          else createClosureType genericCount