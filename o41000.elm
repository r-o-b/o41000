port module Main exposing (..)

import Html exposing (Html, beginnerProgram, program, main_, div, span, button, text, h1, h2, h3, ul, li, br, footer, a, nav, img, button, node)
import Html.Attributes exposing (style, href, class, classList, target, rel, src, type_, attribute, id, alt, title)
import Set
import Tuple
import Json.Decode as Decode
import Debug
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Navbar as Navbar
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Http
import Color


-- Helpers --


port title : String -> Cmd a


unique : List comparable -> List comparable
unique list =
    Set.fromList list
        |> Set.toList



-- Types --


type Page
    = Home
    | About
    | Normal GroupOption FilterOption
    | NotFound


type DataState
    = Loading
    | Loaded
    | Failed


type FilterOption
    = All
    | Main
    | Contemporary


type GroupOption
    = Title
    | Artist
    | Source


type alias Model =
    { filteredMp3s : List Mp3
    , fullMp3List : List Mp3
    , groupedMp3s : List Mp3Group
    , groupBy : GroupOption
    , filterBy : FilterOption
    , page : Page
    , hash : String
    , navbarState : Navbar.State
    , dataState : DataState
    }


type alias Mp3 =
    { hymnTitle : String
    , artist : String
    , source : String
    , url : String
    , sourceUrl : String
    , musicStyle : String
    , mainList : Bool
    , contemporary : Bool
    }


type alias Mp3Group =
    { heading : String
    , mp3s : List Mp3
    }


type Msg
    = UrlChange Location
    | NavbarMsg Navbar.State
    | DatasHere (Result Http.Error (List Mp3))



-- Main Program --


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Subscriptions --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Init --


initialNavState : Navbar.State
initialNavState =
    Tuple.first (Navbar.initialState NavbarMsg)


initialNavCmd : Cmd Msg
initialNavCmd =
    Tuple.second (Navbar.initialState NavbarMsg)


initialDataCmd : Cmd Msg
initialDataCmd =
    getData


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( model, urlCmd ) =
            urlUpdate location { initialModel | page = Home }
    in
        ( model, Cmd.batch [ urlCmd, initialNavCmd, initialDataCmd ] )


initialModel : Model
initialModel =
    { filteredMp3s = []
    , fullMp3List = []
    , groupedMp3s = (groupMp3sByOption Title [])
    , groupBy = Title
    , filterBy = All
    , page = Home
    , hash = ""
    , navbarState = initialNavState
    , dataState = Loading
    }



-- Routing (used in both Init and Update) --


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        newModel : Model
        newModel =
            { model | hash = location.hash }

        pageForLocation : Page
        pageForLocation =
            case decode location of
                Nothing ->
                    NotFound

                Just page ->
                    page

        pageNameForLocation : String
        pageNameForLocation =
            pageName pageForLocation
    in
        case pageForLocation of
            NotFound ->
                ( { newModel | page = pageForLocation }, title pageNameForLocation )

            About ->
                ( { newModel | page = pageForLocation }, title pageNameForLocation )

            Home ->
                ( { newModel | page = pageForLocation, groupBy = Title, filterBy = All }, title pageNameForLocation )

            Normal groupOption filterOption ->
                ( { newModel | page = pageForLocation, groupBy = groupOption, filterBy = filterOption }, title pageNameForLocation )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map About (UrlParser.s "about")
        , UrlParser.map (Normal Title All) (UrlParser.s "title" </> UrlParser.s "all")
        , UrlParser.map (Normal Title Main) (UrlParser.s "title" </> UrlParser.s "main")
        , UrlParser.map (Normal Title Contemporary) (UrlParser.s "title" </> UrlParser.s "contemporary")
        , UrlParser.map (Normal Artist All) (UrlParser.s "artist" </> UrlParser.s "all")
        , UrlParser.map (Normal Artist Main) (UrlParser.s "artist" </> UrlParser.s "main")
        , UrlParser.map (Normal Artist Contemporary) (UrlParser.s "artist" </> UrlParser.s "contemporary")
        , UrlParser.map (Normal Source All) (UrlParser.s "source" </> UrlParser.s "all")
        , UrlParser.map (Normal Source Main) (UrlParser.s "source" </> UrlParser.s "main")
        , UrlParser.map (Normal Source Contemporary) (UrlParser.s "source" </> UrlParser.s "contemporary")
        ]



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        UrlChange location ->
            urlUpdate location model

        DatasHere (Ok data) ->
            ( { model | fullMp3List = data, dataState = Loaded }, Cmd.none )

        DatasHere (Err err) ->
            Debug.log ("Error getting data: " ++ (toString err)) ( { model | dataState = Failed }, Cmd.none )



-- -- View -- --


view : Model -> Html Msg
view model =
    let
        mainContent =
            case model.dataState of
                Loading ->
                    pageDataLoading

                Failed ->
                    pageDataFailed

                Loaded ->
                    case model.page of
                        Home ->
                            pageHome model

                        About ->
                            pageAbout

                        Normal _ _ ->
                            pageNormal model

                        NotFound ->
                            pageNotFound
    in
        Grid.container [] <|
            [ navbar model
            , h1 [] [ text (pageName model.page) ]
            , mainContent
            ]


pageName : Page -> String
pageName page =
    case page of
        Home ->
            "Home"

        About ->
            "About"

        NotFound ->
            "404"

        Normal groupOption filterOption ->
            (toString filterOption) ++ " by " ++ (toString groupOption)


pageDataLoading : Html Msg
pageDataLoading =
    main_ [] [ text "Loading data..." ]


pageDataFailed : Html Msg
pageDataFailed =
    main_ [] [ text "Sorry, there was an error loading the data." ]


pageHome : Model -> Html Msg
pageHome model =
    div []
        [ div
            [ class "welcome" ]
            [ text "Welcome. This site lists recordings of Charles Wesley hymns available online. Use the links above or the buttons below to Group By a characteristic other than Title or to Filter By a characteristic instead of showing All." ]
        , pageNormal model
        ]


pageNormal : Model -> Html Msg
pageNormal model =
    let
        filteredMp3s =
            filterMp3s model.fullMp3List model.filterBy

        groupedMp3s =
            groupMp3sByOption model.groupBy filteredMp3s
    in
        div []
            [ customLinks model
            , hymnGroupEls groupedMp3s
            , statusLine groupedMp3s
            ]


filterMp3s : List Mp3 -> FilterOption -> List Mp3
filterMp3s fullMp3List filter =
    case filter of
        All ->
            fullMp3List

        Main ->
            List.filter (\m -> m.mainList == True) fullMp3List

        Contemporary ->
            List.filter (\m -> m.contemporary == True) fullMp3List


pageNotFound : Html Msg
pageNotFound =
    h2 [] [ text "Page not found." ]


pageAbout : Html Msg
pageAbout =
    main_
        [ class "about" ]
        [ (text "This site is a list of Charles Wesley hymns with audio on the web, based on the data in ")
        , a
            [ (href "https://docs.google.com/spreadsheets/d/1Alrt7wM0lWV0Ionva_a2dM767KcrKwbSsE4RiF2Hp7Q"), (target "_blank") ]
            [ text "this spreadsheet" ]
        , (text ". I haven't included instrumental versions, since Wesley only wrote the words. Please email O41000@zoho.com with any questions, suggestions, etc. ")
        ]


mp3UniqueBy : (Mp3 -> String) -> List Mp3 -> List String
mp3UniqueBy funForProp mp3List =
    List.map funForProp mp3List
        |> unique


groupMp3sByOption : GroupOption -> List Mp3 -> List Mp3Group
groupMp3sByOption groupOption mp3List =
    let
        funcForProp : Mp3 -> String
        funcForProp =
            case groupOption of
                Title ->
                    .hymnTitle

                Artist ->
                    .artist

                Source ->
                    .source

        listOfPropVals : List String
        listOfPropVals =
            mp3UniqueBy funcForProp mp3List

        filterForOnePropVal : String -> List Mp3
        filterForOnePropVal onePropVal =
            List.filter (\mp3 -> (funcForProp mp3) == onePropVal) mp3List
    in
        listOfPropVals
            |> List.map (\onePropVal -> Mp3Group onePropVal (filterForOnePropVal onePropVal))


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.darkCustom Color.darkBlue
        |> Navbar.brand
            [ href "#" ]
            [ img
                [ src "o41000_icon_30x30.jpg"
                , alt ""
                , class "d-inline-block align-top"
                , style [ ( "width", "30px" ) ]
                ]
                []
            , text " O41000"
            ]
        |> Navbar.items
            [ Navbar.itemLink
                [ href "#title/all", classList [ ( "active", (model.hash == "#title/all" || model.hash == "") ) ] ]
                [ text "All by Title" ]
            , Navbar.itemLink
                [ (href "#title/main"), (classList [ ( "active", (model.hash == "#title/main") ) ]) ]
                [ text "Main by Title" ]
            , Navbar.itemLink
                [ href "#source/all", (classList [ ( "active", (model.hash == "#source/all") ) ]) ]
                [ text "All by Source" ]
            , Navbar.itemLink
                [ href "#about", (classList [ ( "active", (model.hash == "#about") ) ]) ]
                [ text "About" ]
            , Navbar.itemLink
                [ (href "http://o41000.herokuapp.com/"), (target "_blank") ]
                [ text "Old Site" ]
            ]
        |> Navbar.view model.navbarState


customLinks : Model -> Html Msg
customLinks model =
    div []
        [ div [ class "btn-toolbar" ]
            [ linkButtonWrapper "Group by:"
                -- Keep Filter, change Group
                [ linkButton Title model.filterBy (model.groupBy == Title) "Title"
                , linkButton Artist model.filterBy (model.groupBy == Artist) "Artist"
                , linkButton Source model.filterBy (model.groupBy == Source) "Source"
                ]
            , linkButtonWrapper "Filter by:"
                -- Keep Group, change Filter
                [ linkButton model.groupBy All (model.filterBy == All) "All"
                , linkButton model.groupBy Main (model.filterBy == Main) "Main"
                , linkButton model.groupBy Contemporary (model.filterBy == Contemporary) "Contemporary"
                ]
            ]
        ]


linkButtonWrapper : String -> List (Html Msg) -> Html Msg
linkButtonWrapper label linkButtons =
    Form.formInline [ (class "mr-2 form-group") ]
        --classes for right and bottom margins
        [ Form.label [] [ text label ]
        , div [ class "btn-group" ] linkButtons
        ]


linkButton : GroupOption -> FilterOption -> Bool -> String -> Html Msg
linkButton group filter isActive label =
    let
        hash =
            "#" ++ (urlPartForGroup group) ++ "/" ++ (urlPartForFilter filter)

        atts =
            if isActive then
                [ (href hash), (class "active") ]
            else
                [ (href hash) ]
    in
        Button.linkButton [ Button.secondary, Button.attrs atts ] [ text label ]


urlPartForGroup : GroupOption -> String
urlPartForGroup group =
    case group of
        Title ->
            "title"

        Artist ->
            "artist"

        Source ->
            "source"


urlPartForFilter : FilterOption -> String
urlPartForFilter filter =
    case filter of
        Main ->
            "main"

        All ->
            "all"

        Contemporary ->
            "contemporary"


statusLine : List Mp3Group -> Html Msg
statusLine listOfGroups =
    let
        listOfLists : List (List Mp3)
        listOfLists =
            List.map .mp3s listOfGroups

        numGroups : String
        numGroups =
            listOfGroups |> List.length |> toString

        numMp3s : String
        numMp3s =
            listOfLists |> List.concat |> List.length |> toString
    in
        footer
            [ style [ ( "text-align", "right" ), ( "font-style", "italic" ) ] ]
            [ text ("Displaying " ++ numMp3s ++ " recordings in " ++ numGroups ++ " groups") ]


hymnGroupEls : List Mp3Group -> Html Msg
hymnGroupEls groupsOfMp3s =
    main_ []
        (List.map hymnEls groupsOfMp3s)


hymnEls : Mp3Group -> Html Msg
hymnEls mp3Group =
    Card.config []
        |> Card.headerH5 [] [ text mp3Group.heading ]
        |> Card.listGroup (List.map hymnLi mp3Group.mp3s)
        |> Card.view


hymnLi : Mp3 -> ListGroup.Item Msg
hymnLi mp3 =
    ListGroup.li []
        [ span
            []
            [ a [ href mp3.url, target "_blank" ] [ (text mp3.hymnTitle) ]
            , text (" (" ++ (String.toLower mp3.musicStyle) ++ ")")
            , text (" by " ++ mp3.artist ++ " at ")
            , a [ href mp3.sourceUrl, target "_blank" ] [ (text mp3.source) ]
            ]
        ]



-- Data (called by Init, used in Update; JSON comes in all at once as program loads) --


getData : Cmd Msg
getData =
    Http.get "o41000.json" mp3ListDecoder
        |> Http.send DatasHere


mp3ListDecoder : Decode.Decoder (List Mp3)
mp3ListDecoder =
    Decode.list mp3Decoder


mp3Decoder : Decode.Decoder Mp3
mp3Decoder =
    Decode.map8 Mp3
        (Decode.field "hymnTitle" Decode.string)
        (Decode.field "artist" Decode.string)
        (Decode.field "source" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "sourceUrl" Decode.string)
        (Decode.field "musicStyle" Decode.string)
        (Decode.field "mainList" Decode.bool)
        (Decode.field "contemporary" Decode.bool)
