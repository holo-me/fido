const { fact, rule, ask, are, atoms, variables, jokers } = require('./adapter');

const [john, mary, bob, isa, is, beauty, girl, wine, likes] = are(atoms);
const [X, Y] = are(variables);
const [_] = are(jokers);

john.value = 'John';
bob.value = 'Robert';
mary.value = 'Mary';
wine.value = 'Chardonnay';

fact(bob, likes, wine);
fact(mary, isa, girl);
fact(mary, is, beauty);

rule(john, likes, X)
    .if(X, isa, girl).and(X, is, beauty)
    .or(X, likes, wine)
    .save();

ask(X, likes, Y).then(console.log);

return;