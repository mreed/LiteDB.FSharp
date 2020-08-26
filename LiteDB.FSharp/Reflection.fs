namespace LiteDB.FSharp
open LiteDB
open System
open System.Collections.Generic
open System.Linq.Expressions
open Newtonsoft.Json

module Reflection =
    open System
    let isCollection (collectionType:Type) =
        let typeNames = ["FSharpList`1";"IEnumerable`1";"List`"; "List`1"; "IList`1"; "FSharpOption`1"]
        let typeName = collectionType.Name
        if List.contains typeName typeNames then
            true
        elif collectionType.IsArray then
            true
        else 
            false
    let getCollectionElementType (collectionType:Type)=
        if isCollection collectionType then
            if collectionType.IsArray then collectionType.GetElementType()
            else collectionType.GetGenericArguments().[0]
        else failwithf "Could not extract element type from collection of type %s"  collectionType.FullName    
    
    let generateGetter dataType (memberInfo : System.Reflection.PropertyInfo) =
        let obj = Expression.Parameter(typeof<Object>,"0")
        let accessor = Expression.MakeMemberAccess(Expression.Convert(obj, memberInfo.DeclaringType), memberInfo)
        Expression.Lambda<GenericGetter>((Expression.Convert(accessor, typeof<Object>)),obj).Compile()
        
           

