module Model
where
import Data.List (deleteBy)

type Label = String
type Quantity = Double
type Model = [(Label,Stock)]
data Stock = Stock Quantity

initial :: Model
initial = []

stock :: Model -> Label -> Stock
stock m l = case lookup l m of
    Just s -> s
    Nothing -> error $ "unknown stock:" ++ l 
    

addStock :: Label -> Quantity -> Model -> Model
addStock l q m = (l, Stock q) : m

deleteStock :: Label -> Model -> Model
deleteStock s m = filter (\(l,_) -> l /= s) m

quantity :: Stock -> Quantity
quantity (Stock q) = q

update :: Label -> (Model -> Quantity) -> Model -> Model
update l f m = addStock l (f m) (deleteStock l m)

increase :: Label -> (Model -> Quantity) -> Model -> Model
increase l f m = addStock l (q + f m) (deleteStock l m)
    where q = quantity (stock m l)

decrease :: Label -> (Model -> Quantity) -> Model -> Model
decrease l f m = addStock l (q - f m) (deleteStock l m)
    where q = quantity (stock m l)




