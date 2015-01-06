import Arbre.Expressions
import Arbre.Box
import Arbre.Compile
import Arbre.Short
import Arbre.Native

testLiteral = modul [
    def "main" [] (string "hello world")
  ]

testSymref = modul [
    Def "x" (numv 486),
    def "main" [] (self "x")
  ]

testWrappedNative = modul [
    def "main" [] (Call (num 1) "Add" [num 2])
  ]

testCall = modul [
    def "addone" ["x"] (Call (num 1) "Add" [local "x"]),
    def "main" [] (Call selff "addone" [num 2])
  ]

{-

testAdd = modul [
    def "main" [] (Call (env "Add") [(num 137), (num 349)])
  ]

testAdd2 = modul [
    def "main" [] (Call (env "Equals") [
        (Call (env "Add") [(num 137), (num 349)]),
        (num 486)
      ]
    )
  ]

testMult = modul [
    def "main" [] (Call (env "Multiply") [
        (num 137),
        (num 349)
      ]
    )
  ]
    
testNested = modul [
    def "main" [] (Call (env "Equals") [
        (Call (env "Multiply") [
            (Call (env "Add") [
                (num 2),
                (Call (env "Multiply") [
                    (num 4),
                    (num 6)
                  ])
              ]),
            (Call (env "Add") [
                (Call (env "Add") [
                    (num 3),
                    (num 5)
                  ]),
                (num 7)
              ])
          ]),
          (num 390)
      ]
    )
  ]
    
testSquare = modul [
    def "square" ["x"] (
        Call (env "Multiply") [
            (local "x"),
            (local "x")
          ]
      ),
    def "main" [] (Call (env "Equals") [
        (Call (self "square") [(num 21)]),
        (num 441)
      ]
    )
  ]
    
testSumOfSquares = modul [
    def "square" ["x"] (
        Call (env "Multiply") [
            (local "x"),
            (local "x")
          ]
      ),
    def "sumOfSquares" ["i", "j"] (
        Call (env "Add") [
            (Call (self "square") [local "i"]),
            (Call (self "square") [local "j"])
          ]
      ),
    def "main" [] (Call (env "Equals") [
        (Call (self "sumOfSquares") [num 4, num 3]),
        (num 25)
      ]
    )
  ]
    
testAbs = modul [
    def "abs" ["x"] (
        Call (env "choose") [
            (Call (env "GreaterThan") [local "x", num 0]),
            (block [] $ env "x"),
            (block [] $ Call (env "Multiply") [env "x", num (-1)])
          ]
     ),
    def "main" [] (Call (env "Equals") [
        (Call (self "abs") [num 5]),
        (num 5)
      ]
     )
  ]
    
testAbs2 = modul [
    def "abs" ["x"] (
        Call (env "choose") [
            (Call (env "GreaterThan") [local "x", num 0]),
            (block [] $ env "x"),
            (block [] $ Call (env "Multiply") [env "x", num (-1)])
          ]
     ),
    def "main" [] (Call (env "Equals") [
        (Call (self "abs") [num (-5)]),
        (num 5)
      ]
     )
  ]
    
testIf = modul [
    Def "truth" truev,
    def "main" [] (Call (env "Equals") [
        Call (env "choose") [
            (self "truth"),
            (block [] $ num 5),
            (block [] $ num 0)
          ],
        (num 5)
      ]
     )
  ]
  
testIf2 = modul [
    Def "truth" falsev,
    def "main" [] (Call (env "Equals") [
        Call (env "choose") [
            (self "truth"),
            (block [] $ num 5),
            (block [] $ num 0)
          ],
        (num 0)
      ]
     )
  ]

testAnd = modul [
    def "main" [] (Call (env "and") [true, false])
  ]
testAnd2 = modul [
    def "main" [] (Call (env "and") [true, true])
  ]
testAnd3 = modul [
    def "main" [] (Call (env "and") [false, false])
  ]

testOr = modul [
    def "main" [] (Call (env "or") [true, false])
  ]
testOr2 = modul [
    def "main" [] (Call (env "or") [true, true])
  ]
testOr3 = modul [
    def "main" [] (Call (env "or") [false, false])
  ]

testNot = modul [
    def "main" [] (Call (env "not") [true])
  ]
testNot2 = modul [
    def "main" [] (Call (env "not") [false])
  ]

testSqrt = modul [
    def "square" ["victim"] (
        Call (env "Multiply") [
            (local "victim"),
            (local "victim")
          ]
      ),
    def "abs" ["number"] (
        Call (env "choose") [
            (Call (env ">f") [local "number", float 0]),
            (block [] $ env "number"),
            (block [] $ Call (env "Multiply") [env "number", float (-1)])
          ]
     ),
    def "goodenough" ["guess3", "match"] (
        Call (env ">f") [
            (float 0.001),
            (Call (self "abs") [
                (Call (env "-f") [
                    (Call (self "square") [
                        (local "guess3")
                    ]),
                    (local "match")
                ])
           ])
        ]
    ),
    def "average" ["i", "j"]
        (Call (env "Divide") [
            (Call (env "Add") [
                (local "i"),
                (local "j")
            ]),
            (float 2)
        ]),
    def "improve" ["guess2", "desired"]
        (Call (self "average") [
            (local "guess2"),
            (Call (env "Divide") [
                (local "desired"),
                (local "guess2")
            ])
           ]),
    def "sqrtiter" ["guess1", "goal"]
        (Call (env "choose") [
            (Call (self "goodenough") [local "guess1", local "goal"]),
            (block [] $ env "guess1"),
            (block [] $ (Call (self "sqrtiter") [
                    (Call (self "improve") [env "guess1", env "goal"]),
                    (env "goal")
                ]))
           ]
       ),
    def "sqrt" ["target"] (Call (self "sqrtiter") [float 1, local "target"]),
    def "main" [] (Call (self "sqrt") [float 9])
  ]

testSqrt2 = modul [
    def "square" ["x"] (
        Call (env "Multiply") [
            (local "x"),
            (local "x")
          ]
      ),
    def "abs" ["x"] (
        Call (env "choose") [
            (Call (env ">f") [local "x", float 0]),
            (block [] $ env "x"),
            (block [] $ Call (env "Multiply") [env "x", float (-1)])
          ]
     ),
    def "goodenough" ["guess", "x"] (
        Call (env ">f") [
            (float 0.001),
            (Call (self "abs") [
                (Call (env "-f") [
                    (Call (self "square") [
                        (local "guess")
                    ]),
                    (local "x")
                ])
           ])
        ]
    ),
    def "average" ["x", "y"]
        (Call (env "Divide") [
            (Call (env "Add") [
                (local "x"),
                (local "y")
            ]),
            (float 2)
        ]),
    def "improve" ["guess", "x"]
        (Call (self "average") [
            (local "guess"),
            (Call (env "Divide") [
                (local "x"),
                (local "guess")
            ])
           ]),
    def "sqrtiter" ["guess", "x"]
        (Call (env "choose") [
            (Call (self "goodenough") [local "guess", local "x"]),
            (block [] $ env "guess"),
            (block [] $ (Call (self "sqrtiter") [
                    (Call (self "improve") [env "guess", env "x"]),
                    (env "x")
                ]))
           ]
       ),
    def "sqrt" ["x"] (Call (self "sqrtiter") [float 1, local "x"]),
    def "main" [] (Call (self "sqrt") [float 9])
  ]

  -}

{-
testSqrtIter = modul [
    Def "square" (block ["x"] (
        Call (env "Multiply") [
            (local "x"),
            (local "x")
          ]
      )),
    Def "abs" (block ["x"] (
        Call (env "choose") [
            (Call (env ">f") [local "x", float 0]),
            (block [] $ env "x"),
            (block [] $ Call (env "Multiply") [env "x", float (-1)])
          ]
     )),
    Def "goodenough" (block ["x"] (
        Call (env ">f") [
            (float 0.001),
            (Call (self "abs") [
                (Call (env "-f") [
                    (Call (self "square") [
                        (dyn "guess")
                    ]),
                    (local "x")
                ])
           ])
        ]
    )),
    Def "average" (block ["x", "y"]
        (Call (env "Divide") [
            (Call (env "Add") [
                (local "x"),
                (local "y")
            ]),
            (float 2)
        ])
       ),
    Def "improve" (block ["x"]
        (Call (self "average") [
            (dyn "guess"),
            (Call (env "Divide") [
                (local "x"),
                (dyn "guess")
            ])
           ])
        ),
    Def "sqrt" (block ["x"]
        (Call (env "choose") [
            (Call (self "goodenough") [local "x"]),
            (block [] $ dyn "guess"),
            (block [] $ set "guess" (Call (self "improve") [env "x"]))
           ])
       ),
    Def "start" (define "guess" (float 1)),
    Def "step" (Call (self "sqrt") [float 1234567891011121])
  ]
-}

{-
testFac = modul [
    def "fac" ["n"]
        (Call (env "choose") [
            (Call (env "Equals") [local "n", num 1]),
            (block [] $ num 1),
            (block [] $ (Call (env "Multiply") [
                env "n",
                (Call (self "fac") [
                    (Call (env "Subtract") [
                        env "n",
                        num 1
                      ])
                  ])
              ]))
          ]),
    def "main" [] (Call (self "fac") [num 1])
  ]

testFac2 = modul [
    def "fac" ["n"]
        (Call (env "choose") [
            (Call (env "Equals") [local "n", num 1]),
            (block [] $ num 1),
            (block [] $ (Call (env "Multiply") [
                env "n",
                (Call (self "fac") [
                    (Call (env "Subtract") [
                        env "n",
                        num 1
                      ])
                  ])
              ]))
          ]),
    def "main" [] (Call (self "fac") [num 10])
  ]

testFib = modul [
    def "fib" ["n"]
        (Call (env "choose") [
            (Call (env "Equals") [local "n", num 0]),
            (block [] $ num 0),
            (block [] $ (Call (env "choose") [
                (Call (env "Equals") [env "n", num 1]),
                (block [] $ num 1),
                (block [] $ (Call (env "Add") [
                    (Call (self "fib") [
                        (Call (env "Subtract") [
                            env "n",
                            num 1
                          ])
                      ]),
                    (Call (self "fib") [
                        (Call (env "Subtract") [
                            env "n",
                            num 2
                          ])
                      ])
                    ]))
              ]))
          ]),
    def "main" [] (Call (self "fib") [num 1])
  ]

testFib2 = modul [
    def "fib" ["n"]
        (Call (env "choose") [
            (Call (env "Equals") [local "n", num 0]),
            (block [] $ num 0),
            (block [] $ (Call (env "choose") [
                (Call (env "Equals") [env "n", num 1]),
                (block [] $ num 1),
                (block [] $ (Call (env "Add") [
                    (Call (self "fib") [
                        (Call (env "Subtract") [
                            env "n",
                            num 1
                          ])
                      ]),
                    (Call (self "fib") [
                        (Call (env "Subtract") [
                            env "n",
                            num 2
                          ])
                      ])
                    ]))
              ]))
          ]),
    def "main" [] (Call (self "fib") [num 7])
  ]

testPiSum = modul [
    def "sum" ["term", "a", "next", "b"]
      (Call (env "choose") [
          Call (env "GreaterThan") [local "a", local "b"],
          block [] $ float 0,
          block [] $ Call (env "Add") [
              Call (env "term") [env "a"],
              Call (self "sum") [
                  env "term",
                  Call (env "next") [env "a"],
                  env "next",
                  env "b"
                ]
            ]
        ]),
    def "piterm" ["x"]
      (Call (env "Divide") [
          float 1,
          Call (env "Multiply") [
              local "x",
              Call (env "Add") [
                  local "x",
                  float 2
                ]
            ]
        ]),
    def "pinext" ["x"] (Call (env "Add") [local "x", float 4]),
    def "pisum" ["a", "b"]
        (Call (env "Multiply") [
            float 8,
            Call (self "sum") [self "piterm", local "a", self "pinext", local "b"]
          ]),
    def "inc" ["x"] (Call (env "Add") [local "x", float 1]),
    def "identity" ["x"] (local "x"),
--    def "main" [] (Call (self "pisum") [float 1, float 1000])
    def "main" [] (Call (self "pisum") [float 1, float 90])
  ]
-}

{-
testPrint = modul [
    def "print1" [] (num 1),
    def "main" [] (Call (self "print1") [])
  ]

testPrint2 = modul [
    def "print1" [] (prntAnd (num 1) (block [] (Call (self "print1") []))),
    def "main" [] (Call (self "print1") [])
  ]
-}

{-

testPrintIntegers = modul [
    Def "printn" (block [] $ Event Print (dyn "n")),
    Def "incn" (block [] $ set "n" (Call (env "Add") [dyn "n", num 1])),
    Def "start" (define "n" (num 1)),
    Def "step" (Combine (Call (self "printn") []) (Call (self "incn") []))
  ]


testPrintFib = modul [
    Def "printa" (block [] $ Event Print (dyn "a")),
    Def "fibit" (block [] $ Combine (set "a" (dyn "b")) (set "b" (
        Call (env "Add") [dyn "a", dyn "b"]
      ))),
    Def "start" (Combine (define "a" (num 1)) (define "b" (num 1))),
    Def "step" (Combine (Call (self "printa") []) (Call (self "fibit") []))
  ]

testPrintPi = modul [
    Def "printDigit" (block ["d"] $ Event Print (local "d")),
    Def "pi" (block [] $ (Call (env "choose") [
        Call (env "<") [ -- 4*q+r-t < n*t
          Call (env "Subtract") [
            Call (env "Add") [
              Call (env "Multiply") [num 4, dyn "q"],
              dyn "r"
            ],
            dyn "t"
          ],
          Call (env "Multiply") [dyn "n", dyn "t"]
        ],
        block [] $
        combine [
          Call (self "printDigit") [dyn "n"],
          superset [
            ("q", Call (env "Multiply") [num 10, dyn "q"]),
            ("r", Call (env "Multiply") [
              num 10,
              Call (env "Subtract") [
                dyn "r",
                Call (env "Multiply") [dyn "n", dyn "t"]
              ]
            ]),
            ("n", Call (env "Subtract") [
              Call (env "/") [
                Call (env "Multiply") [
                  num 10,
                  Call (env "Add") [
                    dyn "r",
                    Call (env "Multiply") [num 3, dyn "q"]
                  ]
                ],
                dyn "t"
              ],
              Call (env "Multiply") [num 10, dyn "n"]
            ])
          ]
        ],
        block [] $
        superset [
            ("q", Call (env "Multiply") [dyn "q", dyn "k"]),
            ("r", Call (env "Multiply") [
              dyn "l",
              Call (env "Add") [
                dyn "r",
                Call (env "Multiply") [num 2, dyn "q"]
              ]
            ]),
            ("t", Call (env "Multiply") [dyn "t", dyn "l"]),
            ("k", Call (env "Add") [dyn "k", num 1]),
            ("n", Call (env "/") [
              Call (env "Add") [
                Call (env "Multiply") [
                  dyn "q",
                  Call (env "Add") [
                    num 2,
                    Call (env "Multiply") [num 7, dyn "k"]
                  ]
                ],
                Call (env "Multiply") [dyn "r", dyn "l"]
              ],
              Call (env "Multiply") [dyn "t", dyn "l"]
            ]),
            ("l", Call (env "Add") [dyn "l", num 2])
        ]
      ])),
    Def "start" (superdefine [("q",1),("r",0),("t",1),("k",1),("n",3),("l",3)]),
    Def "step" (Call (self "pi") [])
  ]

testEchoStdin = modul [
    Def "main" (Receiver Stdin (block ["input"] $ Event Print (local "input")))
  ]

testReadline = modul [
    Def "getline" (block ["buffer", "char"] (
     Call (env "choose") [
        (Call (env "==s") [local "char", string "\n"]),
        block [] (env "buffer"),
        block [] $ (Receiver Stdin $ block ["input"] (
          Call (self "getline") [
            Call (env "append") [env "buffer", env "char"],
            local "input"
           ]
         ))
      ]
     )),
     Def "readline" (block [] (
       Receiver Stdin (block ["input"] $ (
         Call (self "getline") [string "", local "input"]
        ))
      )),
     def "main" [] (Call (self "readline") [])
  ]


testReadline2 = modul [
    Def "getline" (block ["buffer", "char"] (
     Call (env "choose") [
        (Call (env "==s") [local "char", string "\n"]),
        block [] (env "buffer"),
        block [] $ (Receiver Stdin $ block ["input"] (
          Call (self "getline") [
            Call (env "append") [env "buffer", env "char"],
            local "input"
           ]
         ))
      ]
     )),
     Def "readline" (block [] (
       Receiver Stdin (block ["input"] $ (
         Call (self "getline") [string "", local "input"]
        ))
      )),
     def "main" [] (Call (self "readline") [])
  ]

testReadline3 = modul [
    Def "getline" (block ["buffer", "char"] (
     Call (env "choose") [
        (Call (env "==s") [local "char", string "\n"]),
        block [] (env "char"),
        block [] $ (Combine (env "char")
         (stdin (Call (self "getline") [
             Call (env "append") [env "buffer", env "char"],
             local "input"
            ]))
         )
      ]
     )),
     Def "readline" (block [] $ stdin $ Call (self "getline") [
       string "", local "input"
      ]),
     def "main" [] (Call (self "readline") [])
  ]

testHelloWorld = modul [
    Def "getline" (block ["char"] (
     Call (env "choose") [
        (Call (env "==s") [local "char", string "\n"]),
        block [] $ (Call (local "sayHello") []),
        block [] $ (Combine
           (set "buffer" (Call (env "append") [dyn "buffer", env "char"]))
           (stdin $ block ["input"] (Call (self "getline") [local "input"]))
         )
      ]
     )),
     Def "sayHello" (block ["name"] (
       Call (env "append") [
         string "Hello, ",
         local "name"
        ]
      )),
     Def "helloworld" (block [] $ Combine
       (string "What is your name?")
       (stdin $ block ["input"] (Call (self "getline") [local "input"]))
      ),
     Def "start" (define "buffer" (boxString "")),
     def "main" [] (Call (self "helloworld") [])
  ]
-}
  
main = do
  prepare

  compile "literal" testLiteral
  compile "symref" testSymref
  compile "wrappednative" testWrappedNative
  compile "call" testCall
{-
  compile "add" testAdd
  compile "add2" testAdd2
  compile "mult" testMult
  compile "nested" testNested
  compile "square" testSquare
  compile "sumsquares" testSumOfSquares
  compile "abs" testAbs
  compile "abs2" testAbs2
  compile "choose" testIf
  compile "choose2" testIf2
  compile "and" testAnd
  compile "and2" testAnd2
  compile "and3" testAnd3
  compile "or" testOr
  compile "or2" testOr2
  compile "or3" testOr3
  compile "not" testNot
  compile "not2" testNot2
  compile "sqrt" testSqrt
  compile "sqrt2" testSqrt2
  compile "fac" testFac
  compile "fac2" testFac2
  compile "fib" testFib
  compile "fib2" testFib2
  compile "pisum" testPiSum
-}
