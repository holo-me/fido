:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- multifile o/3.

% Consulting all sub-modules.
:- ['common.pl', 'domain.pl', 'user.pl', 'uxui.pl'].

% Predicate used by type/5 to add styling properties, mapped by "uxui.pl".
style(TYPE, LEVEL, PROPS) :-
    tag(TYPE, TAG),
    font(LEVEL, FONT),
    !,
    PROPS = [tag=TAG|FONT].

% Predicate used by get/2 (alterniating with follow/5) for recognizing types and handling them appropriately.
% In the case that the node has no type, processing continues.
type(PATH, NODE, CRITERIA, LEVEL, RESULT) :-
    \+ o(NODE, hasType, _),
    !,
    follow(PATH, NODE, CRITERIA, LEVEL, RESULT).
% The special case where the node is a leaf in a particular relationship with the current topic.
type(_, NODE, _, LEVEL, json([REL=TOPIC|PROPS])) :-
    o(NODE, REL, topic),
    !,
    o(thisApp, isAbout, TOPIC),
    o(NODE, hasType, TYPE),
    style(TYPE, LEVEL, PROPS).
% The case where the node has a type and children, so we are dealing with a complex type.
type(PATH, NODE, CRITERIA, LEVEL, json([children=CHILDREN|PROPS])) :-
    o(NODE, hasOne, _),
    o(NODE, hasType, TYPE),
    !,
    NEXT is LEVEL + 1,
    style(TYPE, LEVEL, PROPS),
    findall(CHILD, follow(PATH, NODE, CRITERIA, NEXT, CHILD), CHILDREN).
% Otherwise, the node is a simple type leaf. Styling properties are added and the path in the data structure is specified.
type(PATH, NODE, _, LEVEL, json([path=FINAL_PATH|PROPS])) :-
    o(NODE, hasType, TYPE),
    style(TYPE, LEVEL, PROPS),
    reverse([NODE|PATH], FINAL_PATH).

% Predicate used by follow/5 for checking if the criteria for including a node are met.
o(NODE, meets, CRITERIA) :-
    [PURPOSE,SCOPE] = CRITERIA,
    o(NODE, hasPurpose, PURPOSE),
    \+ o(NODE, hasWiderScopeThat, SCOPE).

% Predicate used by get/2 (alterniating with type/5) for traversing the realtionship tree.
% In this clause the predicate checks the children one by one and if they meet criteria continues the traversing (DFS).
follow(PATH, NODE, CRITERIA, LEVEL, RESULT) :-
    o(NODE, hasOne, CHILD),
    o(CHILD, meets, CRITERIA),
    NEXT is LEVEL + 1,
    type([NODE|PATH], CHILD, CRITERIA, NEXT, RESULT).
% When node is a member of a class, it inherits its properties (the level remains unchanged).
follow(PATH, NODE, CRITERIA, LEVEL, RESULT) :-
    o(NODE, isa, CLASS),
    o(CLASS, meets, CRITERIA),
    type(PATH, CLASS, CRITERIA, LEVEL, RESULT).
% The special case where the node has a collection of simple types.
follow(PATH, NODE, CRITERIA, _, json([tag='Collection',path=FINAL_PATH])) :-
    o(NODE, hasMany, CHILDREN),
    o(CHILDREN, meets, CRITERIA),
    reverse([CHILDREN,NODE|PATH], FINAL_PATH).

% The main predicate for traversing and processing the relationship tree.
% The initial case.
get(main, RESPONSE) :-
    get(thisApp, RESPONSE).
% The case of processing the tree starting with the root given by "ITEM".
get(ITEM, RESPONSE) :-
    o(thisApp, hasPurpose, PURPOSE),
    o(thisApp, hasScope, SCOPE),
    type([], ITEM, [PURPOSE,SCOPE], 3, RESPONSE).

% The request handling.
handle(REQUEST, _) :-
    get(REQUEST, RESPONSE),
    reply_json_dict(RESPONSE).

% Prolog server initialization and simple routing
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- http_handler(root(REQUEST), handle(REQUEST), []).

:- initialization(server(3002)).
