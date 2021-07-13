import React, { useState } from 'react';
import {Card, CardContent} from '@material-ui/core';
import TeX from '@matejmazur/react-katex';
import { Pagination } from '@material-ui/lab';

export interface Judgment {
    expression: string,
    premises: Judgment[]

};

const Trace: React.FunctionComponent<{
    macros: () => object;
    trace: string[];
}> = (props): JSX.Element => {

/**    const [currentPage, setCurrentPage] = useState(0); */
    const [minIndex, setMinIndex] = useState(0);
    const [maxIndex, setMaxIndex] = useState(0);

    const pagesize: number = 10;
    const numPages: number = Math.floor(props.trace.length / pagesize);

    function updatePage(_: any, page: number): void {
/**        setCurrentPage(page); */
        setMinIndex(page * pagesize);
        setMaxIndex((page + 1) * pagesize);
    }


    const renderCards = () => {
        return props.trace.map((step, index) => {
            if (minIndex <= index && index < maxIndex) {
                return (
                    <Card className="expression-card" key={`traceCard${index}`}>
                        <CardContent>
                            <TeX settings={{ macros: props.macros() }} block>
                                {step}
                            </TeX>
                        </CardContent>
                    </Card>
                )
            } else {
                return null;
            };
        });
    };

    return (
        <div>
            <Pagination count={numPages} onChange={updatePage} />
        {renderCards()}
        </div>
    );
}

export default Trace;
