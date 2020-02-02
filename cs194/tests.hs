data FailableDouble = Failure
                    | OK Double
                    deriving Show


safediv :: Double -> Double -> FailableDouble
safediv _ 0 = Failure
safediv a b = OK (a / b)