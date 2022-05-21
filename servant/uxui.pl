% This is a subset of the common space that contains information about specific frameworks.

:- multifile o/3.

% Material UI tag mapping and styling.
font(3, [variant='h2', color='text.primary']).
font(4, [variant='h3', color='text.primary']).
font(5, [variant='h4', color='text.primary']).
font(6, [variant='h5', color='text.secondary']).
font(_, [variant='h6', color='text.secondary']).
tag(container, 'Container').
tag(titleBar, 'Bar').
tag(selector, 'Selector').
tag(card, 'CompanyCard').
tag(stringType, 'Typography').
tag(regexType, 'Typography').
tag(dateType, 'DatePicker').
tag(currencyType, 'ComplexText').
tag(flatText, 'ComplexText').
tag(row, 'InLineText').
tag(string, 'Typography').
tag(regex, 'Typography').
