module Types
    (
    )
  where



data CoreType = TInt16
              | TInt32
              | TInt
              | TFloat
              | TDouble
              | TList CoreType

