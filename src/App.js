import io from 'socket.io-client';
import React, { Component, Fragment } from "react";
import { Typography } from "@mui/material";
import { Container, Bar, Selector, CompanyCard, ComplexText, Collection } from './Components';

/**
 * "tags" is responsible for mapping names to ReactJS components, both those imported
 * from the Material UI library and those defined by the developer in the "Components.js" file.
 */
const tags = { Fragment, Typography, Container, Bar, Selector, CompanyCard, ComplexText, Collection };

/**
 * Serial number generator for components' children.
 */
const auto = (function* generator(i) { while(true) yield i++; })(0);

export default class App extends Component {
    constructor(props) {
        super(props);

        /**
         * "this.state.gui" stores ready-to-parse fragments of GUI,
         * while "this.state.data" stores JSON objects with data, both lazy retrieved from "servant.js".
         */
        this.state = {data: {}, gui: {}};

        this.attach = this.attach.bind(this);
        this.link = this.link.bind(this);
        this.parse = this.parse.bind(this);
    }

    componentDidMount() {
        this.servant = io('http://localhost:3001');

        /**
         * Handling a message from "servant.js" with a piece of data and metadata (GUI fragment).
         * Updates the application state in the correct order.
         */
        this.servant.on('fragment', ({reference, data, gui}) => {
            if(!this.state.data[reference]) {
                this.setState({data: {...this.state.data, [reference]: data}}, () => {
                    this.setState({gui: {...this.state.gui, [reference]: gui}})
                });
            }
        });

        /**
         * Starting communication with "servant.js" after the component has been successfully mounted.
         */
        this.servant.emit('getFragment', 'main');
    }

    componentWillUnmount() {
        this.servant.disconnect();
    }

    /**
     * Method that extends path from an array of nodes and then attach it to the "root".
     */
    attach = (path, root) => path.reduce((tree, node) => tree[node] || tree, root);

    /**
     * Method called when a reference is encountered.
     * Queries "servant.js" for new portion of data and inserts a node pending for data into the DOM tree.
     */
    link(fragment) {
        const {tag, reference} = fragment;
        this.servant.emit('getFragment', reference);
        return React.createElement(tags[tag],
            {reference, data: this.state.data, gui: this.state.gui, parse: this.parse, key: auto.next().value});
    }

    /**
     * This is the main method for parsing the GUI fragment represented by a JSON object.
     * The method recursively traverse such an object and creates ReactJS elements.
     * Leaves with references to other fragments, with paths to data atoms or containing only text, exits recursion.
     */
    parse(fragment, root = this.state.data) {
        const {tag, children, path, reference, text, ...props} = fragment;
        if(reference) return this.link(fragment);
        const content = children && children.map(child => this.parse(child, root)) || path && this.attach(path, root) || text;
        return React.createElement(tags[tag], {...props, key: auto.next().value}, content);
    };

    render = () =>
        <div className="App">
            {this.state.gui.main ? this.parse(this.state.gui.main) : <Typography>Please wait...</Typography>}
        </div>

}
