/**
 * This is an alternative way to implement logic programming using an adapter.
 * This module only supports the "fact" predicate, simple rules, and queries.
 * It doesn't support cut, negation, lists, built-in predicates and many others.
 * It should cover files "domain.pl" and "user.pl", that is the browser part,
 * the rest should be defined in the Prolog file - "base.pl", intended to be retrieved
 * from the server. Go to the "example.js" file to see how it works.
 */

const exec = require('child-process-promise').exec;

/**
 * Collections for atoms, variables and wildcards.
 */
const [atoms, variables, jokers] = [[], [], []];

/**
 * Mapping the set name into a prefix
 */
const map = set => set === atoms ? 'a' : set === variables ? 'A' : '_';

/**
 * The generator for atoms, variables and wildcards, that can encapsulate other objects.
 */
function* are(set, i = 0) {
    while(++i) yield set[set.push({id: map(set) + i, value: i}) - 1];
}

/**
 * We will expand this variable with further clauses.
 * */
let clauses = '';

/**
 * Basic function for wrapping terms.
 */
const expand = (pre, terms, post = '') =>
    terms.reduce((clause, term, i) => clause + (i ? ',' : '') + term.id, pre) + post;

/**
 * This predicate gather subject-predicate-object into an RDF triple.
 */
const fact = (...terms) => clauses += expand('assert(o(', terms, ')),');

/**
 * This is the way of expressing Prolog rules in the JavaScript.
 * Note that the "if" method follows directly the head
 * and the entire clause should end with the "save" method.
 */
const rule = (...terms) => {
    const head = expand('assert((o(', terms, '):-o(');
    let chain = {
        if: (...terms) => (clauses += expand(head, terms)) && chain,
        and: (...terms) => (clauses += expand('),o(', terms)) && chain,
        or: (...terms) => (clauses += expand('))),' + head, terms)) && chain,
        save: () => clauses += '))),',
    }
    return chain;
}

/**
 * This is how we query the knowledge base.
 * Returns an array with sets of substituted variables or in fact the values they contain.
 * True-false queries are not supported.
 */
const ask = async (...terms) => {
    const vars = terms.filter(term => variables.includes(term));
    let goal = clauses + expand('findall([', vars, '],');
    goal += expand('o(', terms, '),X),write(X),halt.');
    const { stdout } = await exec('swipl -f base.pl -g "' + goal + '"');
    const sets = stdout.replace(' ','').replace('[[','').replace(']]','').split('],[');
    return sets.map(set => set.split(',')
        .map(name => atoms.find(atom => atom.id === name).value))
};

module.exports = { fact, rule, ask, are, atoms, variables, jokers };
