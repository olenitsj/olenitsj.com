module Main exposing (..)

import Html exposing (Html)
import Browser
import Element exposing (Element, el, text, row, column, alignRight, fill, fillPortion, width, rgb255, spacing, centerX, centerX, padding, maximum, minimum)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Dict exposing (Dict)

type alias Page = {pageName:PageName
    , inMenu:Bool  
    , isActive:Bool}

type PageName = Home
    | Contact
    | Social
    | Blog

type alias Model = {windowWidth: Int
    , windowHeight: Int
    , menuPages : List (Page)}

type Msg
    = SwitchPage PageName

main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

init : String -> (Model, Cmd Msg)
init flags = ({windowWidth = 3000 , windowHeight = 100, menuPages = [{pageName = Home, inMenu = True, isActive = True}, {pageName = Social, inMenu = True, isActive = False}, {pageName = Blog, inMenu = True, isActive = False}, {pageName = Contact, inMenu = True, isActive = False}]}, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SwitchPage pageName -> 
            switchToPage pageName model
switchToPage: PageName -> Model -> (Model, Cmd Msg)
switchToPage pageName model = 
    ({windowWidth = model.windowWidth
    , windowHeight = model.windowHeight
    , menuPages = List.map (updateActivePageInMenu pageName) model.menuPages}, Cmd.none)

updateActivePageInMenu : PageName -> Page -> Page
updateActivePageInMenu a b = if a == b.pageName 
    then {pageName = b.pageName, inMenu = b.inMenu, isActive = True}
    else {pageName = b.pageName, inMenu = b.inMenu, isActive = False}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Element.layout []
        (baseLayout model)
baseLayout : Model -> Element Msg
baseLayout model =
    column [width fill ]
        [ header model.menuPages
        , createPage model (extractFirstPageNameFromPageList model.menuPages)
        ]

extractFirstPageNameFromPageList : List (Page) -> PageName
extractFirstPageNameFromPageList menuPages = case List.head 
    (List.filter isActive menuPages 
    |> List.map (\n -> n.pageName)) of 
     Just a -> a
     Nothing -> Home


isActive : Page -> Bool
isActive n = n.isActive

createPage : Model -> PageName -> Element Msg
createPage model pageName = 
    case pageName of
        Home -> Element.image [width (fill
            |> maximum model.windowWidth)] 
            {src = "programming.png", description = "bla"}
        Blog -> row[spacing 10]
            [
            text (pageString pageName)
            , text ("windowHeight: " ++ String.fromInt model.windowHeight)
            ]
        Social -> row[]
            [
            text (pageString pageName)
            , text "blablabla"
            ]
        Contact -> row[]
            [
            text (pageString pageName)
            , text "blablabla"
            ]

header : List (Page) -> Element Msg
header menuPages =
    row [ Background.color (rgb255 0 0 0)
    , width fill 
    , padding 30
    , spacing 50]
    [ logo
    , menu menuPages
    ]


logo : Element Msg
logo =
    el
        [Font.color (rgb255 255 255 255)]
        (text "Igor Olenitsj")

menu : List (Page) -> Element Msg
menu menuPages = 
  row 
  [
    Element.spacingXY 10 10 
  , Font.color (rgb255 255 255 255) 
  ] (List.map createMenuButton menuPages)


createMenuButton : Page -> Element Msg
createMenuButton page = Input.button 
        [ Background.color (rgb255 0 0 0)
            , padding 10
            , Element.mouseOver 
            [ if not page.isActive then Border.shadow 
                { 
                    offset = ( 0, 2 )
                    , size = 0
                    , blur = 0
                    , color = rgb255 1 255 156
                }
                else Border.shadow 
                { 
                    offset = ( 0, 0 )
                    , size = 0
                    , blur = 0
                    , color = rgb255 1 255 156
                }
            ], if page.isActive then Border.shadow 
                { 
                    offset = ( 0, 5 )
                    , size = 0
                    , blur = 0
                    , color = rgb255 1 100 156
                } 
                else Border.shadow 
                { 
                    offset = ( 0, 0 )
                    , size = 0
                    , blur = 0
                    , color = rgb255 1 100 156
                }
        ] { onPress = Just (SwitchPage page.pageName), label = text (pageString page.pageName)}

pageString : PageName -> String
pageString pageName  = case pageName of 
    Home -> "Home"
    Contact -> "Contact"
    Social -> "Social"
    Blog -> "Blog"

