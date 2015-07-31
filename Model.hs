module Model
where

data Stock = Stock Quantity Quantity Quantity
           | Merge Stock Stock
    deriving (Eq,Show)

type Quantity = Double

quantity :: Stock -> Quantity
quantity (Stock _ q _) = q
quantity (Merge s s')  = quantity s + quantity s' 

input_flow :: Stock -> Quantity
input_flow (Stock i _ _) = i
input_flow (Merge s s') = input_flow s + input_flow s'

output_flow :: Stock -> Quantity
output_flow (Stock _ _ o) = o
output_flow (Merge s s') = output_flow s + output_flow s'

initial :: Stock
initial = Stock 0 0 0

input :: Stock -> Quantity -> Stock
input (Stock i q o) n = Stock n (q+n) o

output :: Stock -> Quantity -> Stock
output (Stock i q o) l = Stock i q' (q-q')
    where q' = max (q-l) 0

throughput :: Stock -> (Stock -> Quantity) -> Stock
throughput s f = output s (f s) 
