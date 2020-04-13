import {Basic} from "./Basic/Basic";
import * as React from "react";

export const getItems = () => {
    return [
        {
            links: [
                {
                    name: 'Basic',
                    url: '/',
                    key: 'home',
                    exact: true,
                    main: () => <Basic/>
                },
                {
                    name: 'Notebook',
                    url: '/notebook/',
                    key: 'key',
                    disabled: true,
                    exact: true,
                    main: () => {}
                },
            ]
        }
    ];
};
