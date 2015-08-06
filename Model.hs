module Model
where

type Model = ([Limit],Value,Value,Value)

type Limit = (Label, Value)
type Label = String
type Value = Double

initial :: Model
initial = ([], 0.0, 0.0, 0.0)

addLimit :: Limit -> Model -> Model
addLimit l (ls,i,q,o) = (l:ls,i,q,o)

inputValue :: Model -> Value
inputValue (_,i,_,_) = i

outputValue :: Model -> Value
outputValue (_,_,_,o) = o

quantity :: Model -> Value
quantity (_,_,q,_) = q

input :: Value -> Model -> Model 
input n (ls,_,q,o) = (ls,n,q+n,o)

output :: Model -> Model
output (ls,i,q,o) = (ls, i, q, o)
