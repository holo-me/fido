# fido

## Overview

Here is a small platform that is intended to generate **GUI** with [ReactJS](https://reactjs.org/), using a simple domain ontology created by a developer. To describe such a domain the developer uses the [Prolog](https://www.swi-prolog.org/) language, mostly the `o/3` predicate which collects subject-predicate-object into an [RDF](https://www.w3.org/RDF/) triple.

Take a look at the example of how it works:
```prolog
o(thisApp, isAbout, person).
o(thisApp, hasType, card).
  o(person, hasOne, name).
    o(name, hasType, string).
  o(person, hasOne, address).
    o(address, hasType, row).
    o(address, hasOne, city).
      o(city, hasType, string).
    o(address, hasOne, street).
      o(street, hasType, string).
  o(person, hasOne, phone).
    o(phone, hasType, regex).
```
Based on the above schema and some rules & facts predefined in a knowledge base, the **Prolog server** responds with a **JSON** file:
```json
{
  "tag":"Card",
  "variant":"h2",
  "color":"text.primary",
  "children": [
    {
      "tag":"Typography",
      "variant":"h4",
      "color":"text.primary",
      "path": ["person", "name"]
    },
    {
      "tag":"Row",
      "variant":"h4",
      "color":"text.primary",
      "children": [
        {
          "tag":"Typography",
          "variant":"h6",
          "color":"text.secondary",
          "path": ["person", "address", "city"]
        },
        {
          "tag":"Typography",
          "variant":"h6",
          "color":"text.secondary",
          "path": ["person", "address", "street"]
        }
      ]
    },
    {
      "tag":"Typography",
      "variant":"h4",
      "color":"text.primary",
      "path": ["person", "phone"]
    }
  ]
}
```
that is parsed by the frontend part and rendered by the **ReactJS**.

## Purpose

The purpose of this platform is to show how **Prolog** and **RDF** can be incorporated into **web application** development. **Prolog** is an amazing language that has the ability to predict facts that are not directly expressed. **RDF** on the other hand is a highly declarative form of expressing our knowledge about users, interfaces, common concepts etc.

This is not ready-to-use generator, but some sketch, an idea that I hope will inspire someone.

## Project structure

On the backend side we have a *servant.js* file and a few **Prolog** files:
- *domain.pl* - here the developer describes the purpose and properties of the app, including the schema
- *common.pl* - assuming there will be more apps, some concepts may be moved into a common space and shared
- *uxui.pl* - a subdomain of the common space, containing facts about UX, UIs and frameworks, in our case [Material UI](https://mui.com/)
- *user.pl* - a few facts that describe a specific user - an analog of browser **cookies**
- *servant.pl* - an engine that composes a *JSON* file based on the ontology and serves it for the *server.js*

The [node.js](https://nodejs.org/en/) file *servant.js* sends data fetched from, for example, [MongoDB](https://www.mongodb.com/) (mocked with a **data.json** in our case) and metadata via [socket.io](https://socket.io/) to the frontend part, where they are processed.

On the frontend side, there are only a few initial **ReactJS** files left for presentation purposes. *App.js* is the main file where parsing takes place. Tags are mapped to ReactJS components, both those imported from the **Material UI** library and those defined by the developer in the *Components.js* file.

## Installation

To run this demo you need to install [SWI-Prolog](https://www.swi-prolog.org/Download.html). Don't forget to add the `swipl` path to the `PATH` environment variable.

Open your console in the project directory and type:
- `npm run prolog` to run the **Prolog** engine on port 3002
- `npm run servant` to run the **node.js** backend server on port 3001
- `npm start` to run the **ReactJS** frontend on port 3000

Visit http://localhost:3000 to see the effect.

## TODOs

The platform is under development. There are still many things to do:
- data validation, logging and error handling
- describe the semantics of most components, their purpose and context dependent appearance
- approach to different roles, adjustment the view to the goals and capabilities of different kind of users
- dealing with changes in the application state, that is, actions that can be performed by the user (buttons, menus etc.)
- adding fuzzy logic, relevance and certainty of facts
- an adapter that allows to write some logic code in **JavaScript**  
and much more...

## Further work

So it is basically a small step towards the broader concept of the semantic browser & search engine:
```
┌─────────────────────────────────────────────────────────────────────┐
│ custodian                   ┌─────────────────────────────────────┐ │
│                             │                common               │ │
│                             └─────────────────────────────────────┘ │
│ ┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────┐ │
│ │  uxui   │   │ search  │   │ domain1 │   │ domain2 │   │ domain3 │ │
│ └─────────┘   └─────────┘   └────┬────┘   └────┬────┘   └────┬────┘ │
└────────────────────┬─────────────┼─────────────┼─────────────┼──────┘
┌────────────────────┼──────┐      │             │             │
│ ┌─────────┐   ┌────┴────┐ │ ┌────┴────┐        │             │
│ │   GUI   ├───┤ servant ├···┤ server1 │        │             │
│ └─────────┘   └────┬────┘ │ └─────────┘   ┌────┴────┐        │
│               ┌────┴────┐ │          ·····┤ server2 │        │
│               │  user   │ │               └─────────┘   ┌────┴────┐
│ browser       └─────────┘ │                        ·····┤ server3 │
└───────────────────────────┘                             └─────────┘
```
In this concept, we have different servers that share only raw data with a browser. The view is generated by the browser on the basis of metadata from the custodian and knowledge about the application user. The server owners, in addition to making the content available for browsers, are responsible for presenting an initial **manifesto** to the custodian, and the custodian is responsible for integrating the received domain with other domains and verifying compliance of served data with the **manifesto**. The custodian also conduct an abstraction of concepts.

Let's make the **JavaScript** more semantic!


