type alias Input = { temperature : Float, humidity : Float }

type alias Output = { fanSpeed : Int }

process : Input -> Output
process env =
    if env.temperature > 30 then
        { fanSpeed = 100 }
    else
        { fanSpeed = 50 }
