module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import List.Extra as LX
import Random exposing (generate)
import Random.Extra as RX
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- TYPES


type alias Tables =
    { atmosphere : List String
    , biosphere : List String
    , population : List String
    , tags : List String
    , tech : List String
    , temperature : List String
    , trade : List String
    , origin : List String
    , relationship : List String
    , contact : List String
    , prefix : List String
    , suffix : List String
    }


type alias Planet =
    { namePrefix : Maybe String
    , nameSuffix : Maybe String
    , atmosphere : Maybe String
    , biosphere : Maybe String
    , population : Maybe String
    , isSecondary : Bool
    , firstWorldTag : Maybe String
    , secondWorldTag : Maybe String
    , tradeTag : Maybe String
    , tech : Maybe String
    , temperature : Maybe String
    , trade : Maybe String
    , origin : Maybe String
    , relationship : Maybe String
    , contact : Maybe String
    }



-- DECODERS


decodeTableData : JD.Decoder Tables
decodeTableData =
    JD.succeed Tables
        |> JDP.required "atmosphere" (JD.list JD.string)
        |> JDP.required "biosphere" (JD.list JD.string)
        |> JDP.required "population" (JD.list JD.string)
        |> JDP.required "tags" (JD.list JD.string)
        |> JDP.required "tech" (JD.list JD.string)
        |> JDP.required "temperature" (JD.list JD.string)
        |> JDP.required "trade" (JD.list JD.string)
        |> JDP.required "origin" (JD.list JD.string)
        |> JDP.required "relationship" (JD.list JD.string)
        |> JDP.required "contact" (JD.list JD.string)
        |> JDP.required "prefix" (JD.list JD.string)
        |> JDP.required "suffix" (JD.list JD.string)



-- MODEL


type alias Model =
    { planet : Planet
    , tables : Tables
    , rolls : List Int
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    update GetAllData initModel


initModel : Model
initModel =
    { planet =
        { namePrefix = Nothing
        , nameSuffix = Nothing
        , atmosphere = Nothing
        , biosphere = Nothing
        , population = Nothing
        , isSecondary = False
        , firstWorldTag = Nothing
        , secondWorldTag = Nothing
        , tradeTag = Nothing
        , tech = Nothing
        , temperature = Nothing
        , trade = Nothing
        , origin = Nothing
        , relationship = Nothing
        , contact = Nothing
        }
    , tables = Tables [] [] [] [] [] [] [] [] [] [] [] []
    , rolls = []
    }


getAllData : (Result Http.Error Tables -> Msg) -> Cmd Msg
getAllData cmd =
    Http.get
        { url = "data.json"
        , expect = Http.expectJson cmd decodeTableData
        }



-- UPDATE


type Msg
    = GetAllData
    | GotTables (Result Http.Error Tables)
    | PickFromList (List String) (Maybe String -> Msg)
    | RollDice Int Int (Cmd Msg)
    | SetRoll (Cmd Msg) (List Int)
    | RollPlanet
    | SetSecondary Bool
    | GetFirstWorldTag (Maybe String)
    | GetSecondWorldTag (Maybe String)
    | GetTradeTag (Maybe String)
    | GetAtmosphere (Maybe String)
    | GetTemperature (Maybe String)
    | GetBiosphere (Maybe String)
    | GetPopulation (Maybe String)
    | GetTechLevel (Maybe String)
    | GetOrigin (Maybe String)
    | GetRelationship (Maybe String)
    | GetContactPoint (Maybe String)
    | GetFullName
    | GetNamePrefix (Maybe String)
    | GetNameSuffix (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAllData ->
            ( model, getAllData GotTables )

        GotTables result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok data ->
                    ( { model | tables = data }, Cmd.none )

        PickFromList traitList callback ->
            ( model, generate callback (RX.sample traitList) )

        RollDice dice faces callback ->
            ( model, generate (SetRoll callback) (rollMultiDice dice faces) )

        SetRoll callback results ->
            ( { model | rolls = results }, callback )

        RollPlanet ->
            let
                msgs =
                    [ PickFromList model.tables.tags GetFirstWorldTag
                    , PickFromList model.tables.tags GetSecondWorldTag
                    , PickFromList model.tables.atmosphere GetAtmosphere
                    , PickFromList model.tables.temperature GetTemperature
                    , PickFromList model.tables.biosphere GetBiosphere
                    , PickFromList model.tables.population GetPopulation
                    , PickFromList model.tables.tech GetTechLevel
                    , PickFromList model.tables.trade GetTradeTag
                    , PickFromList model.tables.origin GetOrigin
                    , PickFromList model.tables.relationship GetRelationship
                    , PickFromList model.tables.contact GetContactPoint
                    , PickFromList model.tables.prefix GetNamePrefix
                    , PickFromList model.tables.suffix GetNameSuffix
                    ]
            in
            ( model, Cmd.batch (msgs |> List.map asCmdMsg) )

        SetSecondary b ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | isSecondary = b } }, Cmd.none )

        GetFirstWorldTag worldTag ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | firstWorldTag = worldTag } }, Cmd.none )

        GetSecondWorldTag worldTag ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | secondWorldTag = worldTag } }, Cmd.none )

        GetTradeTag tradeTag ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | tradeTag = tradeTag } }, Cmd.none )

        GetAtmosphere atmo ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | atmosphere = atmo } }, Cmd.none )

        GetTemperature temp ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | temperature = temp } }, Cmd.none )

        GetBiosphere bio ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | biosphere = bio } }, Cmd.none )

        GetPopulation pop ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | population = pop } }, Cmd.none )

        GetTechLevel tech ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | tech = tech } }, Cmd.none )

        GetOrigin origin ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | origin = origin } }, Cmd.none )

        GetRelationship rel ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | relationship = rel } }, Cmd.none )

        GetContactPoint contact ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | contact = contact } }, Cmd.none )

        GetFullName ->
            let
                msgs =
                    [ PickFromList model.tables.prefix GetNamePrefix
                    , PickFromList model.tables.suffix GetNameSuffix
                    ]
            in
            ( model, Cmd.batch (msgs |> List.map asCmdMsg) )

        GetNamePrefix pre ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | namePrefix = pre } }, Cmd.none )

        GetNameSuffix suf ->
            let
                oldPlanet =
                    model.planet
            in
            ( { model | planet = { oldPlanet | nameSuffix = suf } }, Cmd.none )



-- UPDATE FUNCTIONS


rollDice : Int -> Random.Generator Int
rollDice faces =
    Random.int 0 faces


rollMultiDice : Int -> Int -> Random.Generator (List Int)
rollMultiDice dice faces =
    Random.list dice (Random.int 1 faces)


rollWithSumOn : List String -> Model -> Maybe String
rollWithSumOn table model =
    table
        |> LX.getAt (List.sum model.rolls)


asCmdMsg : Msg -> Cmd Msg
asCmdMsg msg =
    Task.succeed identity |> Task.perform (\_ -> msg)



-- VIEW


clickFn : List String -> (Maybe String -> Msg) -> Msg
clickFn traitList msg =
    PickFromList traitList msg


view : Model -> Html Msg
view model =
    let
        planetName =
            case ( model.planet.namePrefix, model.planet.nameSuffix ) of
                ( Just pre, Just suf ) ->
                    pre ++ suf

                _ ->
                    "ROLL"

        tagsWithoutCurrent =
            model.tables.tags
                |> LX.dropWhile
                    (\t ->
                        [ model.planet.firstWorldTag |> Maybe.withDefault ""
                        , model.planet.secondWorldTag |> Maybe.withDefault ""
                        ]
                            |> List.member t
                    )
    in
    div [ class "outer" ]
        [ div []
            [ button [ class "half", onClick RollPlanet ] [ text "Roll Full Planet" ] ]
        , div [ class "container half" ]
            [ div []
                [ h2 [] [ text "Name" ]
                , button
                    [ onClick GetFullName ]
                    [ text planetName ]
                ]
            , div []
                [ h2 [] [ text "World Tags" ]
                , button
                    [ onClick (PickFromList tagsWithoutCurrent GetFirstWorldTag) ]
                    [ text (model.planet.firstWorldTag |> Maybe.withDefault "ROLL") ]
                , br [] []
                , button
                    [ onClick (PickFromList tagsWithoutCurrent GetSecondWorldTag) ]
                    [ text (model.planet.secondWorldTag |> Maybe.withDefault "ROLL") ]
                ]
            , div []
                [ h2 [] [ text "Trade Tag" ]
                , button
                    [ onClick (PickFromList model.tables.trade GetTradeTag) ]
                    [ text (model.planet.tradeTag |> Maybe.withDefault "ROLL") ]
                ]
            , div []
                [ h2 [] [ text "Atmosphere" ]
                , button
                    [ onClick (PickFromList model.tables.atmosphere GetAtmosphere) ]
                    [ text (model.planet.atmosphere |> Maybe.withDefault "ROLL") ]
                ]
            , div []
                [ h2 [] [ text "Temperature" ]
                , button
                    [ onClick (PickFromList model.tables.temperature GetTemperature) ]
                    [ text (model.planet.temperature |> Maybe.withDefault "ROLL") ]
                ]
            , div []
                [ h2 [] [ text "Biosphere" ]
                , button
                    [ onClick (PickFromList model.tables.biosphere GetBiosphere) ]
                    [ text (model.planet.biosphere |> Maybe.withDefault "ROLL") ]
                ]
            , div []
                [ h2 [] [ text "Population" ]
                , button
                    [ onClick (PickFromList model.tables.population GetPopulation) ]
                    [ text (model.planet.population |> Maybe.withDefault "ROLL") ]
                ]
            , div []
                [ h2 [] [ text "Tech Level" ]
                , button
                    [ onClick (PickFromList model.tables.tech GetTechLevel) ]
                    [ text (model.planet.tech |> Maybe.withDefault "ROLL") ]
                ]
            ]
        , hr [ class "divider" ] []
        , div [] [ h2 [ class "inline" ] [ text "For Secondary World:" ] ]
        , div []
            [ div [ class "origin-wrap center-text" ]
                [ h2 [] [ text "Origin" ]
                , button
                    [ onClick (PickFromList model.tables.origin GetOrigin) ]
                    [ text (model.planet.origin |> Maybe.withDefault "ROLL") ]
                ]
            , div []
                [ h2 [] [ text "Relationship w/ Primary" ]
                , button
                    [ onClick (PickFromList model.tables.relationship GetRelationship) ]
                    [ text (model.planet.relationship |> Maybe.withDefault "ROLL") ]
                ]
            , div []
                [ h2 [] [ text "Contact Point w/ Primary" ]
                , button
                    [ onClick (PickFromList model.tables.contact GetContactPoint) ]
                    [ text (model.planet.contact |> Maybe.withDefault "ROLL") ]
                ]
            ]
        ]
