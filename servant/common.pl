% The common part contains abstractions adopted by consensus. This part is maintained by the custodian.
% Note that although the code is declarative and self-describing, the comments contain definitions
% taken from the dictionary. As the ontology grows, all aspects should be accounted for as facts of the Prolog.

:- multifile o/3.

% The oversimplified fact that if something does not have a specific purpose it has general purpose.
o(THING, hasPurpose, general) :-
    \+ o(THING, isFor, _).

% Predicate returns "true" if something has a wider spatial scope than that specified in the "SCOPE".
o(THING, hasWiderScopeThat, SCOPE) :-
    SCOPES = [continent, country, province, city, street],
    o(THING, hasScope, T_SCOPE),
    nth0(N, SCOPES, T_SCOPE),
    nth0(M, SCOPES, SCOPE),
    M >= N.

% infoApp - a simple application for general information purposes.
o(infoApp, hasType, container).
o(infoApp, hasOne, simpleHeader).
    o(simpleHeader, hasType, titleBar).
    o(simpleHeader, text, topic).
o(infoApp, hasOne, infoBody).
    o(infoBody, hasType, selector).
    o(infoBody, reference, topic).

% Name - a language unit by which a person or thing is known.
o(name, hasType, stringType).

% Code - a coding system used for transmitting messages requiring brevity or secrecy.
o(code, isFor, brevity).
o(code, hasType, regexType).

% Address - the place where a person or company can be located or contacted with by post.
o(address, hasType, flatText).
o(address, hasOne, country).
    o(country, hasScope, country).
    o(country, hasOne, countryName).
        o(countryName, isa, name).
    o(country, hasOne, countryCode).
        o(countryCode, isa, code).
o(address, hasOne, city).
    o(city, hasScope, city).
    o(city, hasOne, cityName).
        o(cityName, isa, name).
    o(city, hasOne, postalCode).
        o(postalCode, isa, code).
o(address, hasOne, street).
    o(street, hasScope, street).
    o(street, hasOne, streetName).
        o(streetName, isa, name).
    o(street, hasOne, streetNumber).
        o(streetNumber, hasType, stringType).
    o(street, hasOne, streetFlat).
        o(streetFlat, hasType, stringType).

% Phone - the number is used in calling a particular telephone.
o(phone, hasOne, phonePrefix).
    o(phonePrefix, hasType, regexType).
    o(phonePrefix, hasScope, country).
o(phone, hasOne, phoneNumber).
    o(phoneNumber, hasType, regexType).
