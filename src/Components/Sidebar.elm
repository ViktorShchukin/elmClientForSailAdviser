module Components.Sidebar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import View exposing (View)


view :
    { title : String
    , body : List (Html msg)
    }
    -> View msg
view props =
    { title = props.title
    , body =
        [ div [ class "layout" ]
            [ nav []
                [ ul [] [ li [] [text "Sale Adviser"]]
                , ul [] [ li [] [ a [ href "/" ] [ text "Home" ]]
                        , li [] [a [ href "/search-product"] [ text "search product"]]
                        , li [] [a [href "/group"] [ text "product group"]]
                        ]
                ]
            , div [ class "page" ] props.body
            ]
        ]
    }


--custom tag which used in pico.css for sidebar
nav: List (Attribute msg) -> List (Html msg) -> Html msg
nav =
    node "nav"
