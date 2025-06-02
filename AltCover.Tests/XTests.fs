namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

#nowarn "25"
#if !NET472
module AltCoverXTests =

  let runnerInit () = AltCover.Runner.init ()
  let mainInit () = AltCover.Main.init ()
#endif