import React, { useEffect, useState } from 'react';
import { Card, CardContent, Button, Container, Theme, FormControl, InputLabel, Select, MenuItem, FormLabel, FormControlLabel, FormGroup, Switch, Grid } from '@material-ui/core';
import { createStyles, makeStyles } from '@material-ui/core/styles';

import 'codemirror/lib/codemirror.css';
import './spacemacs.css';
import { UnControlled as CodeMirror } from 'react-codemirror2';
import { PlayArrow, Delete, CheckCircle } from '@material-ui/icons';
import gdtMode from './gdt';
import './fonts/SourceCodePro-SemiBold.ttf';
import examples from './examples';

import 'katex/dist/katex.min.css';

import ErrorMsg from './Error'
import Trace, { Judgment } from './Trace';
import TeX from '@matejmazur/react-katex';

const useStyles = makeStyles((theme: Theme) => {
    return createStyles({
        button: {
            margin: theme.spacing(1),
        },
        formControl: {
            margin: theme.spacing(1),
            minWidth: 200,
        },
        main: {
            justifyContent: "center",
            textAlign: "left",
            position: "relative",
            top: 25,
        },
        typeSep: {
            width: "5em",
        }
    });
},
);

const Editor: React.FC = (): JSX.Element => {

//    const emptyJudgment: Judgment = { expression: "", premises: [] };

    const [errorLevel, setErrorLevel] = useState(0);
    const [errorTitle, setErrorTitle] = useState("Error Title!");
    const [errorMsg, setErrorMsg] = useState("Error Msg!");
    const [text, setText] = useState("");
    const [textAlt, setTextAlt] = useState("");
    const [value, setValue] = useState("");
    const [type, setType] = useState("");
    const [steps, setSteps] = useState([] as string[]);
    const [valid, setValid] = useState("Sound");
    const [example, setExample] = useState(0);
    const [showTrace, setShowTrace] = useState(false);
    const [showEvidence, setShowEvidence] = useState(true);
    const [showType, setShowType] = useState(true);

    const macros = (): object => {
        return {
            '\\ascParens': ((showEvidence && showType) ? '(#1)' : '#1'),
            '\\ascription': ((showEvidence ? '{#1} ~' : '\\vphantom{#1}') + '{#2}' + (showType ? '~ : ~ {#3}' : '\\vphantom{#3}')),
        }
    };

    const callbackCloseErrorMsg = () => {
        setErrorLevel(0);
    };

    const onReset = () => {
        setTextAlt("");
        setErrorLevel(0);
        setValue("");
        setType("");
        setSteps([] as string[]);
    }

    const handleExample = (event: React.ChangeEvent<{ value: unknown }>) => {
        const i = event.target.value as number;
        setExample(i);
        setTextAlt(examples[i].body);
    };

    const handleValid = (event: React.ChangeEvent<{ value: unknown }>) => {
        setValid(event.target.value as string);
    };

    const handleShowEvidence = (_event: React.ChangeEvent<HTMLInputElement>, checked: boolean) => {
        setShowEvidence(checked);
    };

    const handleShowType = (_event: React.ChangeEvent<HTMLInputElement>, checked: boolean) => {
        setShowType(checked);
    };

    const handleShowTrace = (_event: React.ChangeEvent<HTMLInputElement>, checked: boolean) => {
        setShowTrace(checked);
    };

    const showError = (data: { title: string, msg: string, steps: string[] }, level: number) => {
        setErrorTitle(data.title);
        setErrorMsg(data.msg);
        setErrorLevel(level);
        setSteps(data.steps);
        setValue("");
        setType("");
    }

    const onRun = (isEval: boolean) => {
        const requestOptions = {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ "sourceCode": text, "valid": valid, "trace": showTrace })
        };

//        fetch("https://pleiad.cl/gsd/api/" + (isEval ? "eval" : "check"), requestOptions)
//        fetch("https://gsd.cybre.dev:8900/gsd/api/" + (isEval ? "eval" : "check"), requestOptions)
        fetch("http://localhost:8001/gsd/api/" + (isEval ? "eval" : "check"), requestOptions)
            .then(async res => {
                const data = await res.json();

                if (!res.ok) {
                    const error = (data && data.message) || res.status;
                    return Promise.reject(error);
                }

                switch (data.tag) {
                    case "Ok":
                        setValue(data.val);
                        setType(data.typ);
                        setSteps(data.steps);
                        setErrorLevel(0);
                        break;
                    case "Warn":
                        showError(data, 1);
                        break;
                    case "Err":
                        showError(data, 2);
                        break;
                    case "PErr":
                        showError(data, 3);
                        break;
                    default:
                        showError({ title: "Unknown error!", msg: "", steps: [] }, 4);
                        break;
                }
            }
            )
            .catch(_error => {
                showError({ title: "Server is down!", msg: "", steps: [] }, 4);
            });
    }

    const resultMsg = () => {
        return `${value} \\quad \\mathbf{:} \\quad ${type}`
    }

    const examplesItems = examples.map((example, index) => {
        return (<MenuItem value={index}> {example.title} </MenuItem>);
    })

    const classes = useStyles();
    return (
        <Container className={classes.main}>
            <Grid container spacing={2}>
                <Grid item xs>
                    <CodeMirror
                        value={textAlt}
                        defineMode={{ name: "gdt", fn: gdtMode }}
                        options={{
                            mode: "gdt",
                            theme: "spacemacs",
                            lineNumbers: true,
                            autoFocus: true,
                        }}
                        onChange={(_editor, _data, value) => {
                            setText(value);
                        }}
                    />
                </Grid>
                <Grid item xs={3}>
                    <FormControl className={classes.formControl}>
                        <FormLabel component="legend"> Examples </FormLabel>
                        <FormGroup>
                            <Select
                                id="example-select"
                                onChange={handleExample}
                            >
                                <MenuItem value="" disabled>
                                    Examples
                                </MenuItem>
                                {examplesItems}
                            </Select>
                        </FormGroup>
                    </FormControl>
                    <FormControl className={classes.formControl}>
                        <FormLabel component="legend"> Options </FormLabel>
                        <FormGroup>
                            <FormControlLabel
                                control={<Switch checked={showTrace}
                                    onChange={handleShowTrace}
                                    name="showTrace" />}
                                label="Show reduction steps" />

                            <FormControlLabel
                                control={<Switch checked={showEvidence}
                                    onChange={handleShowEvidence}
                                    name="showEvidence" />}
                                label="Show evidences" />
                            <FormControlLabel
                                control={<Switch checked={showType}
                                    onChange={handleShowType}
                                    name="showType" />}
                                label="Show types in ascriptions" />
                        </FormGroup>
                    </FormControl>
                    <FormControl className={classes.formControl}>
                        <FormGroup>
                            <InputLabel id="valid-select-label">Matching strategy</InputLabel>
                            <Select
                                labelId="valid-select-label"
                                id="valid-select"
                                value={valid}
                                onChange={handleValid}
                            >
                                <MenuItem value="Sound">Sound</MenuItem>
                                <MenuItem value="Exact">Exact</MenuItem>
                                <MenuItem value="Complete">Complete</MenuItem>
                            </Select>
                        </FormGroup>
                    </FormControl>
                    <div className="AppCodeButtons">
                        <Button
                            variant="contained"
                            color="primary"
                            className={classes.button}
                            endIcon={<CheckCircle />}
                            onClick={() => onRun(false)}
                        >
                            Typecheck
                        </Button>
                        <Button
                            variant="contained"
                            color="primary"
                            className={classes.button}
                            endIcon={<PlayArrow />}
                            onClick={() => onRun(true)}
                        >
                            Run
                        </Button>
                        <Button
                            variant="contained"
                            color="secondary"
                            className={classes.button}
                            startIcon={<Delete />}
                            onClick={onReset}
                        >
                            Reset
                        </Button>
                    </div>
                </Grid>
            </Grid>
            <ErrorMsg callbackClose={callbackCloseErrorMsg}
                title={errorTitle}
                msg={errorMsg}
                level={errorLevel}
            />
            <Card className="expression-card">
                <CardContent>
                    <TeX settings={{ macros: macros() }} block>
                        {resultMsg()}
                    </TeX>
                </CardContent>
            </Card>
            <Trace macros={macros} trace={showTrace ? steps : []} />
        </Container>
    );
}


export default Editor;
