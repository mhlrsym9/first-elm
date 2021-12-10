module LanguageSelect exposing (getChosenDisplayLanguage, getChosenContentCodeString, init, Model, Msg(..), setAvailableLanguages, update, view)

import Dropdown
import Element exposing (clip, Element, fill, height, maximum, padding, paddingXY, px, rgb255, scrollbarY, spacing, width)
import Element.Background as Background
import Element.Border as Border
import LanguageHelpers exposing (ContentCode, Language, Languages)
import Task exposing (Task)

-- MODEL

type alias Model =
    { chosenLanguage : Maybe Language
    , displayedLanguages : Languages
    , dropdownState : Dropdown.State Language
    , languageModel : LanguageHelpers.Model
    }

-- INIT

initialData : LanguageHelpers.Model -> Model
initialData ( { languages } as model ) =
    let
        chosenLanguage =
            case languages of
                f::_ ->
                    Just f
                [] ->
                    Nothing
    in
    { chosenLanguage = chosenLanguage
    , displayedLanguages = languages
    , dropdownState = Dropdown.init "dropdown"
    , languageModel = model
    }

init : LanguageHelpers.Model -> Model
init languageModel =
    initialData languageModel

getChosenDisplayLanguage : Model -> String
getChosenDisplayLanguage { chosenLanguage }  =
    case chosenLanguage of
        Just { displayName } ->
            displayName
        Nothing ->
            ""

getChosenContentCodeString : Model -> String
getChosenContentCodeString { chosenLanguage } =
    case chosenLanguage of
        Just language ->
            LanguageHelpers.contentCodeStringFromLanguage language

        Nothing ->
            ""

setAvailableLanguages : List String -> Model -> Cmd Msg
setAvailableLanguages ccs { languageModel } =
    Task.perform UpdateLanguages
        (Task.succeed
            (LanguageHelpers.emptyLanguage :: List.sortBy
                (\l -> LanguageHelpers.displayNameFromLanguage l)
                (LanguageHelpers.contentCodesToLanguages languageModel ccs)))

-- UPDATE

type Msg
    = DropdownMsg (Dropdown.Msg Language)
    | UpdateLanguage (Maybe Language)
    | UpdateLanguages Languages

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of
        ( DropdownMsg subMsg, _ ) ->
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model model.dropdownState
            in
            ( { model | dropdownState = state }, cmd )

        ( UpdateLanguage language, _ ) ->
            ( { model | chosenLanguage = language }, Cmd.none )

        ( UpdateLanguages languages, _ ) ->
            ( { model | displayedLanguages = languages }
            , Cmd.none
            )

dropdownConfig : Dropdown.Config Language Msg Model
dropdownConfig =
    let
        containerAttrs =
            [ width ( px 300 ) ]

        selectAttrs =
            [ Border.width 1
            , Border.rounded 5
            , paddingXY 16 8
            , spacing 10
            , width fill
            ]

        searchAttrs =
            [ Border.width 0
            , padding 0
            , width fill
            ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , width fill
            , clip
            , scrollbarY
            , height (fill |> maximum 200)
            ]

        itemToPrompt language =
            Element.text (LanguageHelpers.displayNameFromLanguage language)

        itemToText language =
            LanguageHelpers.displayNameFromLanguage language

        itemToElement selected highlighted language =
            let
                bgColor =
                    if highlighted then
                        rgb255 128 128 128

                    else if selected then
                        rgb255 100 100 100

                    else
                        rgb255 255 255 255
            in
            Element.el
                [ Background.color bgColor
                , padding 8
                , spacing 10
                , width fill
                ]
                ( Element.text (LanguageHelpers.displayNameFromLanguage language) )
    in
    Dropdown.filterable
        { itemsFromModel = .displayedLanguages
        , selectionFromModel = .chosenLanguage
        , dropdownMsg = DropdownMsg
        , onSelectMsg = UpdateLanguage
        , itemToPrompt = itemToPrompt
        , itemToElement = itemToElement
        , itemToText = itemToText
        }
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withPromptElement (Element.el [ ] (Element.text "Select language") )
        |> Dropdown.withFilterPlaceholder "Type for option"
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs

view : Model -> Element Msg
view ( { chosenLanguage, dropdownState } as model ) =
    case chosenLanguage of
        Nothing ->
            Element.text "Failure!"

        Just { contentCode } ->
            Dropdown.view dropdownConfig model dropdownState
                |> Element.el [ ]

