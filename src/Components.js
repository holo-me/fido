import { Component } from "react";
import { Typography, Card, CardContent, AppBar, Toolbar, FormControl,
    Select, MenuItem, CardActions, Button, Grid, Chip } from "@mui/material";

class Wrap extends Component {
    render() {
        const style = {margin: 20, display: 'flex', justifyContent: 'center'};
        return <div style={style}>{this.props.children}</div>
    }
}

class Container extends Component {
    render() {
        return (
            <Wrap>
                <Card sx={{minWidth: 375}}>
                    <CardContent>{this.props.children}</CardContent>
                </Card>
            </Wrap>
        )
    }
}

class Bar extends Component {
    render() {
        return (
            <AppBar position="static">
                <Toolbar variant="dense">
                    <Typography variant="h6" color="inherit" component="div">
                        {this.props.children}
                    </Typography>
                </Toolbar>
            </AppBar>
        )
    }
}

class Selector extends Component {
    constructor(props) {
        super(props);
        this.state = {selected: 0};
    }

    render() {
        const {reference, data, gui, parse} = this.props;
        return (!data[reference] || !gui[reference]) ? <Typography>Please wait...</Typography> :
            <FormControl fullWidth>
                <Select value={this.state.selected} onChange={e => this.setState({selected: e.target.value})}>
                    {data[reference].map((el, i) => <MenuItem key={i} value={i}>{el[reference].name}</MenuItem>)}
                </Select>
                {parse(gui[reference], data[reference][this.state.selected])}
            </FormControl>
    }
}

class CompanyCard extends Component {
    render() {
        return (
            <Wrap>
                <Card sx={{minWidth: 375}}>
                    <CardContent>{this.props.children}</CardContent>
                    <CardActions>
                        <Button size='small'>
                            Learn More
                        </Button>
                    </CardActions>
                </Card>
            </Wrap>
        )
    }
}

class ComplexText extends Component {
    render() {
        return (
            <Grid container spacing={1} justifyContent="center" alignItems="center">
                {this.props.children.map((child, i) =>
                    <Grid item key={i}>
                        {child}
                    </Grid>
                )}
            </Grid>
        )
    }
}

class Collection extends Component {
    render() {
        return (
            <div style={{display: 'flex', justifyContent: 'center'}}>
                {this.props.children.map((element, i) =>
                    <Chip key={i} label={element} />
                )}
            </div>
        )
    }
}

export { Container, Bar, Selector, CompanyCard, ComplexText, Collection };