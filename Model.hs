module Model
where

type Model = [(Label,Stock)]

data Stock = Stock { 
    label :: Label,
    quantity :: Quantity,
    input    :: Quantity,
    output   :: Quantity
    }
    deriving (Eq, Show)

type Label = String
type Quantity = Double

addStock :: Stock -> Model -> Model
addStock s m = (label s, s) : m

initial :: Model
initial = []

stock :: Model -> Label -> Stock
stock m l = case lookup l m of
    (Just s) -> s
    Nothing -> error $ "unknown stock: "++l

update :: Label -> Stock -> Model -> Model
update l s m = addStock s $
    filter (\(lbl,_) -> lbl /= l) m 

runOutput :: (Stock -> Quantity) -> Stock -> Stock
runOutput f s = s { quantity = q, output = o}
    where o = f s
          q = quantity s - o

runInput :: (Stock -> Quantity) -> Stock -> Stock
runInput f s = s { quantity = q, input = i}
    where i = f s
          q = quantity s + i
