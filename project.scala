//> using scala 3.9.0

//> using dep com.halotukozak::commons::0.3.0

//> using test.dep org.scalameta::munit::1.3.6
//> using test.dep org.scala-lang::scala3-compiler:3.9.0

//> using options -deprecation -feature -new-syntax -unchecked
//> using options -language:noAutoTupling
//> using options -Xcheck-macros -Ycheck:macros
//> using options -Ycheck:all
//> using options -Yexplicit-nulls
//> using options -Wsafe-init -Werror
// methods past the compiler's hardcoded 3000-node coverage-instrumentation threshold (not configurable)
// are left uninstrumented; CI's -Wall reports that, as an info so -Werror doesn't fail on it but it stays visible
//> using options "-Wconf:msg=Skipping coverage instrumentation.*:i"
//> using options "-Wconf:msg=Missing symbol position.*:s"
// -Wall's warnings (except -Wsafe-init, set above) are added in CI only: shared ci.yml in halotukozak-com/.github
// compiler debugging flags, to switch back on while chasing a compiler problem:
////> using options -Vprofile -Xprint-inline -Ydebug-flags -Ydebug-missing-refs
////> using options -Yexplain-lowlevel -Yshow-suppressed-errors -Yshow-var-bounds
////> using options -Yprofile-enabled" -Yprofile-trace:debug/compile-trace.json"

//> using publish.organization com.halotukozak
//> using publish.name made
//> using publish.computeVersion git:tag
//> using publish.description "M&DE - Mirror Annotations & Default Extraction"
//> using publish.url https://github.com/halotukozak/made
//> using publish.license MIT
//> using publish.vcs github:halotukozak/made
//> using publish.repository central
//> using publish.developer "halotukozak|Bartłomiej Kozak|https://github.com/halotukozak"
