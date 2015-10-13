:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert))
:- [run].

% les routes
% :- http_handler('/', getParam, []).
:- http_handler('/init', initAction, []).


server(Port) :-
        http_server(http_dispatch, [port(Port)]).


run_game(Request) :-
    run().

initAction(Request) :-
    init(),
    cors_enable,
    format('Content-type: text/plain~n~n'),
    format('Jeu initialisé').


getParam(Request) :-
    http_parameters(Request,
                    [ name(Name, []),
                      sex(Sex, [oneof([male,female])]),
                      birth_year(BY, [between(1850,10000)])
                    ]),
    % register_user(Name, Sex, BY),
    say_perdu(Name, Sex, BY).


say_perdu(Name, Sex, BY) :-
        format('Content-type: text/plain~n~n'),
        format('Alors on s apelle ~w ?~n On est ~w ?~n On est est en ~w?', [Name, Sex, BY]).