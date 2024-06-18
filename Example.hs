{-# LANGUAGE TypeOperators #-}
module Example where

import Lang
import Free
import CBNeed_Translation (denoteCBNeed, emptyEnvCBNeed)
import CBName_Translation (denoteCBName, emptyEnvCBName)
import CBVal (denoteCBVal, emptyEnvCBVal, Value)
import Thunk (Pack)



-- print "Hello1";
-- let x be thunk (
--      print "Hello2"; 
--      return True;
--     )
-- print "Hello3";
-- print x;
-- x;

examplePrint1 :: Lang
examplePrint1 = Print (Stringl "Hello1") (Let "x" (Print (Stringl "Hello2") Truel) (Print (Stringl "Hello3") (Print (Var "x") (Var "x"))))


-- print "Hello1";
-- let x be thunk (
--      print "Hello2"; 
--      return True;
--     )
-- print "Hello3";
-- print x;
-- apply x to
--     lambda y. (
--         print "Hello4";
--         y
--     )

examplePrint2 :: Lang
examplePrint2 = Print (Stringl "Hello1") (
        Let "x" (
            Print (Stringl "Hello2") Truel
        ) (
            Print (Stringl "Hello3") (
                Print (Var "x") (
                    App (Var "x") (
                        Lambda "y" (
                            Print (Stringl "Hello4") (
                                Var "y"
                            )
                        )
                    )
                )
            )
        )
    )


printCBNeed :: String
printCBNeed = show (handle_ hPrint (handle_ hState (denoteCBNeed emptyEnvCBNeed examplePrint2) []) "")

printCBName :: String
printCBName = show (handle_ hPrint (denoteCBName emptyEnvCBName examplePrint2) "")

printCBVal :: String
printCBVal = show (handle_ hPrint (denoteCBVal emptyEnvCBVal examplePrint2) "")