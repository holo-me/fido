% Here developer describe the model and the problem domain.

:- multifile o/3.

% thisApp is a general information application about companies used only in the particular country.
o(thisApp, isa, infoApp).
o(thisApp, hasPurpose, general).
o(thisApp, hasScope, country).
o(thisApp, isAbout, company).
    o(company, hasType, card).
    o(company, hasOne, companyName).
        o(companyName, isa, name).
    o(company, hasOne, contactDetails).
        o(contactDetails, hasOne, companyAddress).
            o(companyAddress, isa, address).
        o(contactDetails, hasOne, contactPhone).
            o(contactPhone, isa, phone).
        o(contactDetails, hasOne, wwwPage).
            o(wwwPage, hasType, regexType).
    o(company, hasMany, branch).
        o(branch, hasOne, branchName).
        o(branchName, isa, name).
    o(company, hasOne, registerDetails).
        o(registerDetails, isFor, register).
        o(registerDetails, hasOne, registerDate).
            o(registerDate, hasType, dateType).
        o(registerDetails, hasOne, nip).
            o(nip, isa, identifier).
        o(registerDetails, hasOne, regon).
            o(regon, isa, identifier).
        o(registerDetails, hasOne, krs).
            o(krs, isa, identifier).
        o(registerDetails, hasOne, shareCapital).
            o(shareCapital, hasType, currencyType).
