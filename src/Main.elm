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
    { namePrefix : String
    , nameSuffix : String
    , atmosphere : String
    , biosphere : String
    , population : String
    , isSecondary : Bool
    , firstWorldTag : String
    , secondWorldTag : String
    , tradeTag : String
    , tech : String
    , temperature : String
    , trade : String
    , origin : String
    , relationship : String
    , contact : String
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
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    update GetAllData initModel


initModel : Model
initModel =
    { planet =
        { namePrefix = ""
        , nameSuffix = ""
        , atmosphere = "ROLL"
        , biosphere = "ROLL"
        , population = "ROLL"
        , isSecondary = False
        , firstWorldTag = "ROLL"
        , secondWorldTag = "ROLL"
        , tradeTag = "ROLL"
        , tech = "ROLL"
        , temperature = "ROLL"
        , trade = "ROLL"
        , origin = "ROLL"
        , relationship = "ROLL"
        , contact = "ROLL"
        }
    , tables = Tables [] [] [] [] [] [] [] [] [] [] [] []
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
    | PickFromList (List String) (String -> Msg)
    | RollPlanet
    | SetSecondary Bool
    | GetFirstWorldTag String
    | GetSecondWorldTag String
    | GetTradeTag String
    | GetAtmosphere String
    | GetTemperature String
    | GetBiosphere String
    | GetPopulation String
    | GetTechLevel String
    | GetOrigin String
    | GetRelationship String
    | GetContactPoint String
    | GetFullName
    | GetNamePrefix String
    | GetNameSuffix String


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
            ( model, generate (\x -> x |> Maybe.withDefault "" |> callback) (traitList |> RX.sample) )

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


asCmdMsg : Msg -> Cmd Msg
asCmdMsg msg =
    Task.succeed identity |> Task.perform (\_ -> msg)



-- VIEW


view : Model -> Html Msg
view model =
    let
        planetName =
            if model.planet.namePrefix == "" || model.planet.nameSuffix == "" then
                "ROLL"

            else
                model.planet.namePrefix ++ model.planet.nameSuffix

        tagsWithoutCurrent =
            model.tables.tags
                |> LX.dropWhile
                    (\t ->
                        [ model.planet.firstWorldTag
                        , model.planet.secondWorldTag
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
                    [ text model.planet.firstWorldTag ]
                , br [] []
                , button
                    [ onClick (PickFromList tagsWithoutCurrent GetSecondWorldTag) ]
                    [ text model.planet.secondWorldTag ]
                ]
            , div []
                [ h2 [] [ text "Trade Tag" ]
                , button
                    [ onClick (PickFromList model.tables.trade GetTradeTag) ]
                    [ text model.planet.tradeTag ]
                ]
            , div []
                [ h2 [] [ text "Atmosphere" ]
                , button
                    [ onClick (PickFromList model.tables.atmosphere GetAtmosphere) ]
                    [ text model.planet.atmosphere ]
                ]
            , div []
                [ h2 [] [ text "Temperature" ]
                , button
                    [ onClick (PickFromList model.tables.temperature GetTemperature) ]
                    [ text model.planet.temperature ]
                ]
            , div []
                [ h2 [] [ text "Biosphere" ]
                , button
                    [ onClick (PickFromList model.tables.biosphere GetBiosphere) ]
                    [ text model.planet.biosphere ]
                ]
            , div []
                [ h2 [] [ text "Population" ]
                , button
                    [ onClick (PickFromList model.tables.population GetPopulation) ]
                    [ text model.planet.population ]
                ]
            , div []
                [ h2 [] [ text "Tech Level" ]
                , button
                    [ onClick (PickFromList model.tables.tech GetTechLevel) ]
                    [ text model.planet.tech ]
                ]
            ]
        , hr [ class "divider" ] []
        , div [] [ h2 [ class "inline" ] [ text "For Secondary World:" ] ]
        , div []
            [ div [ class "origin-wrap center-text" ]
                [ h2 [] [ text "Origin" ]
                , button
                    [ onClick (PickFromList model.tables.origin GetOrigin) ]
                    [ text model.planet.origin ]
                ]
            , div []
                [ h2 [] [ text "Relationship w/ Primary" ]
                , button
                    [ onClick (PickFromList model.tables.relationship GetRelationship) ]
                    [ text model.planet.relationship ]
                ]
            , div []
                [ h2 [] [ text "Contact Point w/ Primary" ]
                , button
                    [ onClick (PickFromList model.tables.contact GetContactPoint) ]
                    [ text model.planet.contact ]
                ]
            ]
        ]
