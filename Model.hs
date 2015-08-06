module Model
where

data Model = Model { featureRequests   :: Value,
                     codingTasks       :: Value,
                     deliveredFeatures :: Value,
                     teamCapacity      :: Value }
type Value = Double

setTeamCapacity :: Model -> Value -> Model
setTeamCapacity m n = m { teamCapacity = n }

initial :: Model
initial = Model 0.0 0.0 0.0 0.0

input :: Model -> Value -> Model
input m n = m { featureRequests = n, codingTasks = (codingTasks m) + n }

output :: Model -> Model
output m = let out = min (teamCapacity m) (codingTasks m)
        in m { codingTasks = (codingTasks m) - out, deliveredFeatures = out }


