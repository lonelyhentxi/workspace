import React from 'react';
import {LayoutHeader} from "./LayoutHeader";
import styled from "styled-components";
import {LayoutContainer} from "./LayoutContainer";
import {INavLinkGroup} from "office-ui-fabric-react";

export const PlainLayout: React.FC<{ className: string, sidebarItems: INavLinkGroup[] }> = ({className, sidebarItems}) => {
    return (
        <div className={className}>
            <LayoutHeader className="layout-header"/>
            <LayoutContainer sidebarItems={sidebarItems} className="layout-container"/>
        </div>
    );
};

export const Layout = styled(PlainLayout)`
   min-height: 100%;  
   width: 100%;
   height: available;
   min-width: 1024px;
   background-image: linear-gradient(to right, #fff 0%,#fff 19.5%, #f1f1f1 19.5%, #f7f7f7 20%,#f7f7f7 100%);
`;
