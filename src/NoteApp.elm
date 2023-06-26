module NoteApp exposing (main)

import Browser
import Category exposing (Category)
import Html exposing (Html, br, button, div, span, text)
import Html.Events exposing (onClick)
import Http
import Note exposing (Note)
import Platform.Cmd as Cmd


type RequestState
    = None
    | Pending
    | Success
    | Failure


type alias Model =
    { notes : List Note
    , categories : List Category
    , selectedCategory : Category
    , requestState : RequestState
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { notes = []
    , categories = []
    , selectedCategory = { id = 0, name = "" }
    , requestState = None
    , error = Nothing
    }


type Msg
    = FetchNotesStart
    | FetchCategoriesStart
    | FetchNotesEnd (Result Http.Error (List Note))
    | FetchCategoriesEnd (Result Http.Error (List Category))
    | SelectCategory Category


fetchNotes : Cmd Msg
fetchNotes =
    Http.get
        { url = "http://localhost:3001/notes"
        , expect =
            Http.expectJson
                FetchNotesEnd
                Note.collectionDecoder
        }


fetchCategories : Cmd Msg
fetchCategories =
    Http.get
        { url = "http://localhost:3001/categories"
        , expect =
            Http.expectJson
                FetchCategoriesEnd
                Category.collectionDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchNotesStart ->
            ( { model | requestState = Pending }
            , fetchNotes
            )

        FetchNotesEnd result ->
            case result of
                Ok notes ->
                    ( { model
                        | requestState = Success
                        , notes = notes
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | requestState = Failure
                        , error = Just "Error"
                      }
                    , Cmd.none
                    )

        SelectCategory selectedCategory ->
            ( { model | selectedCategory = selectedCategory }
            , Cmd.none
            )

        FetchCategoriesStart ->
            ( { model | requestState = Pending }
            , fetchCategories
            )

        FetchCategoriesEnd result ->
            case result of
                Ok newList ->
                    ( { model
                        | requestState = Success
                        , categories = newList
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | requestState = Failure
                        , error = Just "Error"
                      }
                    , Cmd.none
                    )


renderPostItem : Note -> Html Msg
renderPostItem note =
    div []
        [ div [] [ text note.title ]
        , div [] [ text note.content ]
        , br [] []
        ]


renderBoard : Model -> Html Msg
renderBoard model =
    case model.requestState of
        None ->
            div [] []

        Pending ->
            div [] [ text "Loading..." ]

        Success ->
            div []
                (model.notes
                    |> List.filter
                        (\p ->
                            p.categoryID
                                == model.selectedCategory.id
                        )
                    |> List.map renderPostItem
                )

        Failure ->
            case model.error of
                Just error ->
                    span [] [ text error ]

                Nothing ->
                    span [] []


renderCategoryItem : Category -> Category -> Html Msg
renderCategoryItem item selectedItem =
    if item.id == selectedItem.id then
        div []
            [ span [] [ text item.name ]
            ]

    else
        div []
            [ button
                [ onClick (SelectCategory item) ]
                [ text item.name ]
            ]


renderCategoryList : Model -> Html Msg
renderCategoryList model =
    case model.requestState of
        None ->
            div [] []

        Pending ->
            div [] [ text "Loading" ]

        Success ->
            div []
                (model.categories
                    |> List.map (\item -> renderCategoryItem item model.selectedCategory)
                )

        Failure ->
            case model.error of
                Just error ->
                    span [] [ text error ]

                Nothing ->
                    span [] []


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick FetchNotesStart ]
            [ text "Fetch All Notes" ]
        , button
            [ onClick FetchCategoriesStart ]
            [ text "Fetch All Categories" ]
        , renderCategoryList model
        , renderBoard model
        ]


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ fetchCategories, fetchNotes ] )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
