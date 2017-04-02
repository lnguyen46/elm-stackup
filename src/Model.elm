module Model exposing (..)

import Window


-- MODEL
type GameState = Play | Pause | GameOver
type BlockState = ToRight | ToLeft | Falling
type BuildingState = OnGround | OnSky

type alias Model =
    { size: Window.Size
    , x: Float
    , y: Float
    , blockState: BlockState
    , gameState: GameState
    , buildingState: BuildingState
    , building: List (Float, Float)
    , offBuilding: Int
    , floors: Int
    , score: Int
    , compliment: String
    }

model: Model
model =
    { size = Window.Size 0 0
    , x = 0
    , y = 0
    , blockState = ToRight
    , gameState = Play
    , buildingState = OnGround
    , building = []
    , offBuilding = 0
    , floors = 0
    , score = 0
    , compliment = ""
    }

        -- screen =
        --     { wallLeft = toFloat (negate model.size.width // 4)
        --     , wallRight = toFloat (model.size.width // 4)
        --     , floor = toFloat (negate model.size.height // 2)
        --     , ceil = toFloat (model.size.height // 2)
        --     }

