type Quantity = Double
type Label = String
type Model = [(Label, Stock)]
type Output = (Label,Rule)
type Rule = Model -> Stock -> Quantity
data Stock = Stock Quantity Quantity Quantity [Output]
                                
stock :: Model -> String -> Stock
stock l m = case lookup m l of
    Nothing -> error "unknown stock"
    Just s  -> s

initial :: Stock
initial = Stock 0.0 0.0 0.0 []

quantity :: Stock -> Quantity
quantity (Stock _ q _ _) = q

input :: Stock -> Quantity
input (Stock i _ _ _) = i

output :: Stock -> Quantity 
output (Stock _ _ o _) = o

figures :: Stock -> [Quantity]
figures (Stock i q o _) = [i,q,o]

flowTo :: Stock -> Output -> Stock
flowTo (Stock i q o outs) out = Stock i q o (out:outs) 

task = flowTo (flowTo initial ("FEAT", \m s -> output s * 0.8)) ("BUGS", \m s -> output s * 0.2)

myModel = [("TASK",task),("FEAT",initial),("BUGS",initial)]



myStock :: String -> Stock
myStock s = stock myModel s

main = do
    putStrLn $ show $ figures $ myStock "TASK"
    putStrLn $ show $ figures $ myStock "BING"





