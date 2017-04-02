port module Update exposing (..)

import Model exposing (..)

import Window
import Time exposing (Time)



-- PORT
port sendBuilding: List (Float, Float) -> Cmd msg

                   

type alias Screen =
    { wallLeft: Float
    , wallRight: Float
    , floor: Float
    , ceil: Float
    }

type Msg
    = Keydown Int
    | MyWindowSize Window.Size
    | GoRight Time
    | GoLeft Time
    | FreeFall Time
      

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        
        MyWindowSize size ->
            setAppSize model size
                
        Keydown keyCode ->
            handleKey model keyCode
                    
        GoRight _ ->
            model
                |> updateGameState
                |> updateBuildingState
                |> goRight
            
        GoLeft _ ->
            goLeft model
                
        FreeFall _ ->
            freeFall model

                
    
updateGameState: Model -> Model
updateGameState model =
    if model.offBuilding == 3 then
        { model |
              gameState = GameOver
        }
    else
        model

updateBuildingState: Model -> Model
updateBuildingState model =
    if model.floors < 8 then
        { model | buildingState = OnGround }
    else if model.floors == 8 then
        { model |
              buildingState = OnSky,
              building = onlyGetRoof model
        }
    else
        beyondTheSky model


beyondTheSky: Model -> Model
beyondTheSky model =
    let
        flats = List.length model.building
    in
        if flats == 9 then
            { model |
                  building = onlyGetRoof model
            }
        else
            model

        
onlyGetRoof: Model -> List (Float, Float)
onlyGetRoof model =
    let
        bottomLine = toFloat <| negate <| model.size.height // 2
        roof = List.take 2 model.building
    in
        case roof of
            [] ->
                [ (0,0) ]
                    
            (x, y) :: [] ->
                [ (x, bottomLine) ]
                    
            (x1, y1) :: (x2, y2) :: [] ->
                [ (x1, bottomLine + 70), (x2, bottomLine) ]
                    
            _ :: _ :: _ ->
                [ (0, 0) ]

        
goRight: Model -> (Model, Cmd Msg)
goRight model =
    let
        wallRight = toFloat <| model.size.width // 4
    in
        if model.x < wallRight then
            { model | x = model.x + 5 } ! []
        else
            { model | blockState = ToLeft } ! [sendBuilding(model.building)]


goLeft: Model -> (Model, Cmd Msg)
goLeft model =
    let
        wallLeft = toFloat <| negate <| model.size.width // 4
    in
        if model.x > wallLeft then
            { model | x = model.x - 5 } ! []
        else
            { model | blockState = ToRight } ! []


freeFall: Model -> (Model, Cmd Msg)
freeFall model =
    let
        bottomLine = toFloat <| negate <| model.size.height // 2
        buildingHeight = toFloat <| (List.length model.building) * 70
        actualHeight = bottomLine + 84 +  buildingHeight
    in
        case model.buildingState of
            OnGround ->
                if model.y > actualHeight then
                    { model | y = model.y - 5 } ! []
                else
                    updateBuilding model

            OnSky ->
                if model.y > bottomLine + buildingHeight then
                    { model | y = model.y - 5 } ! []
                else
                    updateBuildingSky model

                    

updateBuilding: Model -> (Model, Cmd Msg)                
updateBuilding model =
        case model.building of
            [] ->
                model
                    |> addFirstBlock
                    |> backToStartPoint
            
            head :: _ ->
                let
                    (baseX, baseY) = head
                    min = baseX - 70
                    max = baseX + 70
                          
                    fitRange = Basics.abs (baseX - model.x)

                    bottomLine = toFloat <| negate <| model.size.height // 2

                    ground = bottomLine + 84
                in
                    if model.x > min && model.x < max then
                        model
                            |> addBlock baseY
                            |> updateScore fitRange
                            |> backToStartPoint
                    else
                        if model.y > ground then
                            { model | y = model.y - 5 } ! []
                        else
                            model
                                |> handleFailedBlock
                                |> backToStartPoint



updateBuildingSky: Model -> (Model, Cmd Msg)
updateBuildingSky model =
    let
        window_h = toFloat <| negate <| model.size.height // 2

        (topX, topY) = getTopBlock model.building
        min = topX - 70
        max = topX + 70

        fitRange = Basics.abs (topX - model.x)
    in
        if model.x > min && model.x < max then
            model
                |> addBlock topY
                |> updateScore fitRange
                |> backToStartPoint
        else
            if model.y > window_h then
                { model | y = model.y - 5 } ! []
            else
                model
                    |> handleFailedBlock 
                    |> backToStartPoint

    
        
addFirstBlock: Model -> Model
addFirstBlock model =
    let
        window_h = toFloat model.size.height
        onTheGround = 84 - window_h/2
    in
        { model |
              building = (model.x, onTheGround) :: [],
              score = 70,
              compliment = "Good Start!",
              floors = 1
        }

        
addBlock: Float -> Model -> Model
addBlock base model =
    { model |
          building = (model.x, base + 70) :: model.building,
          floors = model.floors + 1
    }

        
handleFailedBlock: Model -> Model
handleFailedBlock model =
    { model |
          offBuilding = model.offBuilding + 1,
          compliment = "Be careful!"
    } 

        
backToStartPoint: Model -> (Model, Cmd Msg)
backToStartPoint model =
    let
        wallLeft = toFloat <| negate <| model.size.width // 4
        ceil = toFloat <| model.size.height // 2
    in
        { model |
              blockState = ToRight,
              x = wallLeft,
              y = ceil - 70
        } ! []
      
        
updateScore: Float -> Model -> Model
updateScore range model =
    let
        perfect = 70
        great = 50
        good = 30
        cool = 20
        bad = 10
    in
        if range == 0 then
            { model |
                  score = model.score + perfect,
                  compliment = "Perfect"
            } 
        else if range > 0 && range < 10 then
            { model |
                  score = model.score + great,
                  compliment = "Great"
            } 
        else if range >= 10 && range < 30 then
            { model |
                  score = model.score + good,
                  compliment = "Good"
            } 
        else if range >= 30 && range < 50 then
            { model |
                  score = model.score + cool,
                  compliment = "Cool"
            } 
        else
            { model |
                  score = model.score + bad,
                  compliment = "Bad"
            } 

            

getTopBlock: List (Float, Float) -> (Float, Float)
getTopBlock building =
    case List.head building of
        Nothing ->
            (0, 0)
        Just (x, y) ->
            (x, y)

            
restart: Model -> Model
restart model =
    { model |
          blockState = ToRight,
          building = [],
          offBuilding = 0,
          floors = 0,
          score = 0,
          gameState = Play,
          buildingState = OnGround,
          compliment = ""
    } 


handleKey: Model -> Int -> (Model, Cmd Msg)
handleKey model keyCode =
    case keyCode of
        32 ->
            case model.gameState of
                Play ->
                    { model | blockState = Falling } ! []

                Pause ->
                    model ! []

                GameOver ->
                    (restart model) ! []

        13 ->
            case model.gameState of
                Play ->
                    { model | gameState = Pause } ! []

                Pause ->
                    { model | gameState = Play } ! []

                GameOver ->
                    model ! []

        _ ->
            model ! []



setAppSize: Model -> Window.Size -> (Model, Cmd Msg)
setAppSize model size =
    let
        startPoint =
            { x = toFloat <| negate <| size.width // 4
            , y = toFloat <| (size.height // 2) - 70
            }
    in
        { model |
              size = size,
              x = startPoint.x,
              y = startPoint.y
        } ! []
                
