import React, {useEffect} from 'react';
import './App.css';
import { AppBar, createStyles, IconButton, makeStyles, Theme, Toolbar, Typography } from '@material-ui/core';
import MenuIcon from '@material-ui/icons/Menu';

import Editor from './Editor'

const useStyles = makeStyles((theme: Theme) => {
    return createStyles({
        root: {
            flexGrow: 1,
        },
        menuButton: {
            marginRight: theme.spacing(2),
        },
        title: {
            flexGrow: 1,
        },
        button: {
            margin: theme.spacing(1),
        },
        header: {
            marginBottom: "10px",
        }
    });
},
);


const App: React.FC = () => {
    useEffect(() => {
        document.title = "GSD: Gradually Structured Data"
    }, []);

    return (
        <div className="App">
            <AppHeader />
            <Editor />
        </div>
    );
};

function AppHeader(): JSX.Element {
    const classes = useStyles();
    return (
        <AppBar position="static">
            <Toolbar className={classes.header}>
                <Typography variant="h6" className={classes.title}>
                    GSD: Gradually Structured Data
                    </Typography>
            </Toolbar>
        </AppBar>
    );
}


export default App;
