namespace Sample25

module Say =
  let hello name = printfn "Hello %s" name

#if IDEMPOTENT_INSTRUMENT
[<AltCover.Recorder.InstrumentationAttribute(Assembly = "YkSBYKm1rLNNQAObr3F/7E5EWmGzn6y25sn4NR/8gc0=",
                                             Configuration = "L8ab9IPrbO8xD1JTWD0jOMmMVY7zH/hVbZcw6gbKrWM=")>]
()
#endif

(*
Decompiles as

.custom instance void [AltCover.Recorder]AltCover.Recorder.InstrumentationAttribute::.ctor() = (
01 00 02 00 54 0e 08 41 73 73 65 6d 62 6c 79 2c
59 6b 53 42 59 4b 6d 31 72 4c 4e 4e 51 41 4f 62
72 33 46 2f 37 45 35 45 57 6d 47 7a 6e 36 79 32
35 73 6e 34 4e 52 2f 38 67 63 30 3d 54 0e 0d 43
6f 6e 66 69 67 75 72 61 74 69 6f 6e 2c 4c 38 61
62 39 49 50 72 62 4f 38 78 44 31 4a 54 57 44 30
6a 4f 4d 6d 4d 56 59 37 7a 48 2f 68 56 62 5a 63
77 36 67 62 4b 72 57 4d 3d
)

01 00 02 00
54 0e
08 41 73 73 65 6d 62 6c 79 <= pascal string Assembly

2c 59 6b 53 42 59 4b 6d 31 72 4c 4e 4e 51 41 4f <= pascal string Assembly value
62 72 33 46 2f 37 45 35 45 57 6d 47 7a 6e 36 79
32 35 73 6e 34 4e 52 2f 38 67 63 30 3d

54 0e
0d 43 6f 6e 66 69 67 75 72 61 74 69 6f 6e <= pascal string Configuration

2c 4c 38 61 62 39 49 50 72 62 4f 38 78 44 31 4a <= pascal string Configuration value
54 57 44 30 6a 4f 4d 6d 4d 56 59 37 7a 48 2f 68
56 62 5a 63 77 36 67 62 4b 72 57 4d 3d

*)