module View exposing (..)

import Model exposing (..)
import Update exposing (..)

import Html exposing (..)
import Collage
import Text exposing (Text)
import Element
import Color

view: Model -> Html Msg
view model =
    let
        (window_w, window_h) = (model.size.width, model.size.height)
        (w, h) = (toFloat window_w, toFloat window_h)

        background =
            renderBackground model w h

        backgroundGO =
            Collage.alpha 0.7 background
                
        block =
            Collage.rect 70 70
                |> Collage.filled Color.brown
                |> Collage.move (model.x, model.y)

        building =
            List.map createBlock model.building

        info =
            renderInfo model

        gameOver =
            renderGameOver model

        pause =
            renderPause model
    in
        case model.gameState of
            Play ->
                background :: block :: info :: building
                    |> Collage.collage window_w window_h
                    |> Element.toHtml

            Pause ->
                backgroundGO :: info :: pause
                    |> Collage.collage window_w window_h
                    |> Element.toHtml

            GameOver ->
                backgroundGO :: gameOver
                    |> Collage.collage window_w window_h
                    |> Element.toHtml



renderBackground: Model -> Float -> Float -> Collage.Form
renderBackground model w h =
    let
        sky = Color.linear
              (0, 0)
              (w, h)
              [ (0, (Color.rgb 224 234 252))
              , (1, (Color.rgb 207 222 243))
              ]

    in
        case model.buildingState of
            OnGround ->
                Collage.group
                    [ Collage.rect w h -- sky
                          |> Collage.gradient sky
                       
                    , Collage.rect w 50 -- land
                          |> Collage.filled Color.green
                          |> Collage.move (0, 24 - h/2)
                    ]

            OnSky ->
                Collage.rect w h
                    |> Collage.gradient sky
            

                           
createBlock: (Float, Float) -> Collage.Form
createBlock (x, y) =
    Collage.rect 70 70
        |> Collage.filled Color.brown
        |> Collage.move (x, y)


renderInfo: Model -> Collage.Form           
renderInfo model =
    let
        infoX = toFloat <| negate <| (model.size.width // 3  + 120)
                
        life =
            case 3 - model.offBuilding of
                1 -> 
                    "⭐"
                2 ->
                    "⭐ ⭐"
                3 ->
                    "⭐ ⭐ ⭐"
                _ ->
                    ""
                        
        floors = model.floors
                         
        (recentX, recentY) = Update.getTopBlock model.building

    in
            Collage.group
                [  life
                      |> renderText infoX 0
                      
                , "Score: " ++ (toString model.score)
                      |> renderText infoX -20
                         
                , "Floors: " ++ (toString floors)
                      |> renderText infoX -40

                , renderCompliment model
                      |> Text.typeface ["American Typewriter"]
                      |> Text.bold
                      |> Text.height 15
                      |> Collage.text
                      |> Collage.move (recentX + 100, recentY)
                ]


renderText: Float -> Float -> String -> Collage.Form
renderText x y text =
    let
        textStyle =
            { typeface = [ "Courier"]
            , height   = Just 16
            , color    = Color.red
            , bold     = True
            , italic   = False
            , line     = Just Text.Under
            }
    in
        text
            |> Text.fromString
            |> Text.style textStyle
            |> Collage.text
            |> Collage.move (x, y)
           

renderCompliment: Model -> Text
renderCompliment model =
    let
        alert = Text.fromString model.compliment
        maroon = Color.rgb 133 20 75
        teal = Color.rgb 57 204 204
    in
        if model.gameState == Pause then
            Text.fromString ""
        else
            alert
                |>
              case model.compliment of
                  "Good Start!" ->
                      Text.color Color.red 

                  "Perfect" ->
                      Text.color Color.purple 

                  "Great" ->
                      Text.color teal

                  "Good" ->
                      Text.color Color.lightOrange

                  "Cool" ->
                      Text.color Color.darkBlue 

                  "Bad" ->
                      Text.color maroon
                          
                  otherwise ->
                      Text.color Color.red
                    

renderPause: Model -> List (Collage.Form)
renderPause model =
        [ Collage.rect 150 100
              |> Collage.filled (Color.yellow)
              
        , Collage.polygon
            [ (-20, 25), (-20, -25), (25, 0) ]
              |> Collage.filled Color.red
            
        ]

                          
renderGameOver: Model -> List (Collage.Form)
renderGameOver model =
    let
        myStyle = {
            typeface = [ "Zapfino" ],
            height   = Just 20,
            color    = Color.red,
            bold     = True,
            italic   = True,
            line     = Just Text.Under
        }

        littleStyle = {
             typeface = [ "Zapfino" ],
             height   = Just 16,
             color    = Color.red,
             bold     = False,
             italic   = False,
             line     = Just Text.Under
        }

    in
        [ Collage.rect 400 250
              |> Collage.filled (Color.rgb 255 255 204)
                      
        , "Game Over"
              |> Text.fromString
              |> Text.style myStyle
              |> Collage.text
              |> Collage.move (0, 40)

        , "Score: " ++ (toString model.score)
              |> Text.fromString
              |> Text.style littleStyle
              |> Collage.text
              |> Collage.move (-80, -40)

        , "Floors: " ++ (toString model.floors)
              |> Text.fromString
              |> Text.style littleStyle
              |> Collage.text
              |> Collage.move (80, -40)

        ]
