module Test.Logs exposing (..)

import Expect exposing (Expectation)
import Frontend
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Time
import Types exposing (..)


suite : Test
suite =
    describe "logs"
        [ describe "keep their order on insert"
            [ test "when inserting after" <|
                \() ->
                    let
                        before =
                            [ { timestamp = Time.millisToPosix 1
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 1
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                              , logHistory = []
                              , sha = "test_sha_1"
                              }
                            , { timestamp = Time.millisToPosix 0
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                              , logHistory = []
                              , sha = "test_sha_0"
                              }
                            ]

                        newLog =
                            { date = Time.millisToPosix 2
                            , title = "Title 2"
                            , content = "Content 2"
                            }
                    in
                    Frontend.insertLog "test_sha_2"
                        newLog
                        ( before
                        , []
                        )
                        |> Expect.equal
                            ( { timestamp = Time.millisToPosix 2
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 2
                                        , title = Unmodified "Title 2"
                                        , content = Unmodified "Content 2"
                                        }
                              , logHistory = []
                              , sha = "test_sha_2"
                              }
                                :: before
                            , []
                            )
            , test "when inserting in the middle" <|
                \() ->
                    let
                        before =
                            [ { timestamp = Time.millisToPosix 2
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 2
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                              , logHistory = []
                              , sha = "test_sha_1"
                              }
                            , { timestamp = Time.millisToPosix 0
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                              , logHistory = []
                              , sha = "test_sha_0"
                              }
                            ]

                        newLog =
                            { date = Time.millisToPosix 1
                            , title = "Title 2"
                            , content = "Content 2"
                            }
                    in
                    Frontend.insertLog "test_sha_2"
                        newLog
                        ( before
                        , []
                        )
                        |> Expect.equal
                            ( [ { timestamp = Time.millisToPosix 2
                                , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 2
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                                , logHistory = []
                                , sha = "test_sha_1"
                                }
                              , { timestamp = Time.millisToPosix 1
                                , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 1
                                        , title = Unmodified "Title 2"
                                        , content = Unmodified "Content 2"
                                        }
                                , logHistory = []
                                , sha = "test_sha_2"
                                }
                              , { timestamp = Time.millisToPosix 0
                                , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                                , logHistory = []
                                , sha = "test_sha_0"
                                }
                              ]
                            , []
                            )
            , test "when inserting before" <|
                \() ->
                    let
                        before =
                            [ { timestamp = Time.millisToPosix 1
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 1
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                              , logHistory = []
                              , sha = "test_sha_1"
                              }
                            , { timestamp = Time.millisToPosix 0
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                              , logHistory = []
                              , sha = "test_sha_0"
                              }
                            ]

                        newLog =
                            { date = Time.millisToPosix -1
                            , title = "Title 2"
                            , content = "Content 2"
                            }
                    in
                    Frontend.insertLog "test_sha_2"
                        newLog
                        ( before
                        , []
                        )
                        |> Expect.equal
                            ( before
                                ++ [ { timestamp = Time.millisToPosix -1
                                     , currentLog =
                                        Fresh
                                            { date = Time.millisToPosix -1
                                            , title = Unmodified "Title 2"
                                            , content = Unmodified "Content 2"
                                            }
                                     , logHistory = []
                                     , sha = "test_sha_2"
                                     }
                                   ]
                            , []
                            )
            ]
        , describe "keep their order on upsert"
            [ test "when upserting after" <|
                \() ->
                    let
                        before =
                            [ { timestamp = Time.millisToPosix 1
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 1
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                              , logHistory = []
                              , sha = "test_sha_1"
                              }
                            , { timestamp = Time.millisToPosix 0
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                              , logHistory = []
                              , sha = "test_sha_0"
                              }
                            ]

                        newLog =
                            { timestamp = Time.millisToPosix 2
                            , currentLog =
                                Fresh
                                    { date = Time.millisToPosix 2
                                    , title = Unmodified "Title 2"
                                    , content = Unmodified "Content 2"
                                    }
                            , logHistory = []
                            , sha = "test_sha_2"
                            }
                    in
                    Frontend.upsertLog
                        newLog
                        ( before
                        , []
                        )
                        |> Expect.equal
                            ( newLog
                                :: before
                            , []
                            )
            , test "when upserting in the middle" <|
                \() ->
                    let
                        before =
                            [ { timestamp = Time.millisToPosix 2
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 2
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                              , logHistory = []
                              , sha = "test_sha_1"
                              }
                            , { timestamp = Time.millisToPosix 0
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                              , logHistory = []
                              , sha = "test_sha_0"
                              }
                            ]

                        newLog =
                            { timestamp = Time.millisToPosix 1
                            , currentLog =
                                Fresh
                                    { date = Time.millisToPosix 1
                                    , title = Unmodified "Title 2"
                                    , content = Unmodified "Content 2"
                                    }
                            , logHistory = []
                            , sha = "test_sha_2"
                            }
                    in
                    Frontend.upsertLog
                        newLog
                        ( before
                        , []
                        )
                        |> Expect.equal
                            ( [ { timestamp = Time.millisToPosix 2
                                , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 2
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                                , logHistory = []
                                , sha = "test_sha_1"
                                }
                              , newLog
                              , { timestamp = Time.millisToPosix 0
                                , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                                , logHistory = []
                                , sha = "test_sha_0"
                                }
                              ]
                            , []
                            )
            , test "when upserting before" <|
                \() ->
                    let
                        before =
                            [ { timestamp = Time.millisToPosix 1
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 1
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                              , logHistory = []
                              , sha = "test_sha_1"
                              }
                            , { timestamp = Time.millisToPosix 0
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                              , logHistory = []
                              , sha = "test_sha_0"
                              }
                            ]

                        newLog =
                            { timestamp = Time.millisToPosix -1
                            , currentLog =
                                Fresh
                                    { date = Time.millisToPosix -1
                                    , title = Unmodified "Title 2"
                                    , content = Unmodified "Content 2"
                                    }
                            , logHistory = []
                            , sha = "test_sha_2"
                            }
                    in
                    Frontend.upsertLog
                        newLog
                        ( before
                        , []
                        )
                        |> Expect.equal
                            ( before
                                ++ [ newLog
                                   ]
                            , []
                            )
            , test "when upserting on" <|
                \() ->
                    let
                        before =
                            [ { timestamp = Time.millisToPosix 1
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 1
                                        , title = Unmodified "Title 1"
                                        , content = Unmodified "Content 1"
                                        }
                              , logHistory = []
                              , sha = "test_sha_1"
                              }
                            , { timestamp = Time.millisToPosix 0
                              , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                              , logHistory = []
                              , sha = "test_sha_0"
                              }
                            ]

                        newLog =
                            { timestamp = Time.millisToPosix 1
                            , currentLog =
                                Fresh
                                    { date = Time.millisToPosix 1
                                    , title = Unmodified "Title 2"
                                    , content = Unmodified "Content 2"
                                    }
                            , logHistory = []
                            , sha = "test_sha_2"
                            }
                    in
                    Frontend.upsertLog
                        newLog
                        ( before
                        , []
                        )
                        |> Expect.equal
                            ( [ newLog
                              , { timestamp = Time.millisToPosix 0
                                , currentLog =
                                    Fresh
                                        { date = Time.millisToPosix 0
                                        , title = Unmodified "Title 0"
                                        , content = Unmodified "Content 0"
                                        }
                                , logHistory = []
                                , sha = "test_sha_0"
                                }
                              ]
                            , []
                            )
            ]
        ]
