-- Import a function from the C library
import Math exposing (sqrt)

type alias Input = { value : Float }
type alias Output = { result : Float }

process : Input -> Output
process env =
    { result = sqrt(env.value) }
