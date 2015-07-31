module Model
where

data Stock = Stock { input_flow   :: Quantity
                    ,quantity     :: Quantity
                    ,output_flow  :: Quantity }
    deriving (Eq,Show)

type Quantity = Double

initial :: Stock
initial = Stock 0 0 0

input :: Stock -> Quantity -> Stock
input (Stock i q o) n = Stock n (q+n) o

output :: Stock -> Quantity -> Stock
output (Stock i q o) l = Stock i q' (q-q')
    where q' = max (q-l) 0

