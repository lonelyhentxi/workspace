import React from 'react';
import {initializeIcons} from '@uifabric/icons';
import {Layout} from "./Layout/Layout";
import styled from "styled-components";
import {getItems} from "./NavItems";

initializeIcons();

const PlainApp: React.FC<{className:string}> = ({className}) => {
    return (
        <div className={className}>
            <Layout sidebarItems={getItems()} className="default-layout"/>
        </div>
    );
};

const App = styled(PlainApp)`
  min-height: 100%;
  height: 100%;
`;

export default App;
