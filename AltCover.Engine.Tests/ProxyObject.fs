namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword
open System

[<NUnit.Framework.IncludeExcludeAttribute>]
type ProxyObject() =
  inherit MarshalByRefObject()