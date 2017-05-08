import Html exposing (Html, div, button, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- MODEL

type alias Model = 
    {name : String
    , oceanMap : List (List Int) -> List Int -> Int -> List (List Int)
    , skillPoints : Int
    , currentLine : List
    , sailingSkill : Int
    , navigationSkill : Int
    , leadershipSkill : Int
    , population : Int
    , maxSpeed : Int
    , boatDirection : Int --Numbers 1-8 determine directon. 1 is NW, 2 is N, and so on.
    , boatXAxis : Int
    , boatYAxis : Int
    , windSpeed : Int
    , windDirection : Int
    , withWind : Bool}



-- UPDATE


createOceanLine : List Int -> Int -> List Int
createOceanLine list x =
    if List.length list < 50 then 
        createOceanLine (x::list) (x + 7)

    else
        list

createFullOcean : List (List Int) -> List Int -> Int -> List (List Int)
createFullOcean bigList list x = 
    if List.length bigList < 50 then
        createFullOcean ((createOceanLine list x) :: bigList) ([]) (x + 20)
    else
        bigList

oceanMapYSearchHelper : List(List Int) -> List Int -> Int -> List Int --Searches through the Y-Axis without chaning BoatYAxis
oceanMapYSearchHelper bigList list y =
    oceanMapYSearch bigList list y

oceanMapYSearch : List(List Int) -> List Int -> Int -> List Int
oceanMapYSearch bigList list y = 
    if y == 0 then
        (List.head bigList)
    else
        (oceanMapYSearch (List.tail bigList) (list) (y - 1))

oceanMapXSearchHelper : List Int -> Int -> Int
oceanMapXSearchHelper list x =
    oceanMapXSearch list x

oceanMapXSearch : List Int -> Int -> Int
oceanMapXSearch list x =
    if x == 0 then
        (List.head list)
    else
        oceanMapXSearch (List.tail list) (x - 1)


--sailMove : Int -> Int-> Bool -> Int
--sailMove sailingSkill windSpeed withWind =



type Msg 
    = Name String
    |   OceanMap List
    |   CurrentLine List
    |   SkillPointSpend Int
    |   SailingSkill Int
    |   NavigationSkill Int
    |   LeadershipSkill Int
    |   Population Int
    |   MaxSpeed Int
    |   BoatDirection Int
    |   WindSpeed Int
    |   WindDirection Int
    |   BoatXAxis Int
    |   BoatYAxis Int
    |   HasSkills Bool

update : Msg -> Model -> Model
update msg model =
  case msg of 
    Name name ->
        { model | name = name }
    OceanMap newOceanMap->
        { model | oceanMap = createFullOcean }
    CurrentLine newCurrentLine ->
        { model | currentLine = newCurrentLine}
    SkillPointSpend skillPoints ->
        { model | skillPoints = skillPoints - 1 }
    SailingSkill sailingSkill ->
        { model | sailingSkill = sailingSkill + 1 }
    NavigationSkill navigationSkill ->
        { model | navigationSkill = navigationSkill + 1}
    LeadershipSkill leadershipSkill ->
        { model | leadershipSkill = leadershipSkill + 1}
    Population newPopulation ->
        { model | population = newPopulation}
    MaxSpeed newSpeed ->
        { model | maxSpeed = newSpeed}
    BoatDirection newDirection ->
        { model | boatDirection = newDirection}
    WindSpeed newWindSpeed ->
        { model | windSpeed = newWindSpeed}
    WindDirection newWindDirection ->
        { model | windDirection = newWindDirection}
    BoatXAxis newX ->
        { model | boatXAxis = newX}
    BoatYAxis newY ->
        { model | boatYAxis = newY}


-- VIEW


view model =  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []


    ]

