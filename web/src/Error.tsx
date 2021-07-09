import React from 'react';
import { createStyles, makeStyles, Theme } from '@material-ui/core';
import { Alert, AlertTitle, Color } from '@material-ui/lab';
import TeX from '@matejmazur/react-katex';

const useStyles = makeStyles((theme: Theme) => {
    return createStyles({
        errorMsg: {
            fontFamily: "mono",
            whiteSpace: "pre-wrap",
            textAlign : "left"
        },
    });
},
);

const ErrorMsg: React.FunctionComponent<{
    callbackClose: () => void;
    title: string;
    msg: string;
    level: number;
}> = (props): JSX.Element | null => {

    var severity: Color = "success";
    var tag: JSX.Element = <p> </p>;
    const classes = useStyles();

    switch (props.level) {
        case 0:
            return null;

        case 1:
            severity = "warning";
            tag = <TeX math = { `\\text{ ${props.msg} }` } />;
            break;

        case 2:
            severity = "error";
            tag = <TeX math={ `\\text{ ${props.msg} }` } />;
            break;

        case 3:
            severity = "error";
            tag = <span> {props.msg} </span>;
            break;

        default:
            severity = "error";
            tag = <span> {props.msg} </span>;
    }
    return <Alert severity={severity} onClose={props.callbackClose} className={classes.errorMsg}>
        <AlertTitle>{props.title}</AlertTitle>
        {tag}
    </Alert>
}

export default ErrorMsg;
