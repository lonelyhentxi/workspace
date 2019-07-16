import React from "react";
import styled from "styled-components";
import {Nav, INavLink, INavLinkGroup} from "office-ui-fabric-react/lib";
import {Route,BrowserRouter} from 'react-router-dom';

export const PlainLayoutContainer: React.FC<({ className: string, sidebarItems: INavLinkGroup[] })> = ({className, sidebarItems}) => {
    return (
        <BrowserRouter>
            <div className={className}>
                <div style={{
                    width: '20%',
                }}>
                    <Nav
                        selectedKey="sidebar"
                        styles={{
                            root: {
                                margin: '40px 0',
                                float: 'right',
                                maxWidth: "200px",
                                boxSizing: 'border-box',
                                overflowY: 'auto'
                            }
                        }}
                        groups={sidebarItems}
                    />
                </div>
                <div
                    style={{
                        width: '80%',
                        margin: '40px'
                    }}
                >
                    {sidebarItems[0].links.map((route) => (
                        <Route key={route.key}
                               path={route.url}
                               exact={route.exact}
                               component={route.main}
                        />
                    ))}
                </div>
            </div>
        </BrowserRouter>
    );
};

export const LayoutContainer = styled(PlainLayoutContainer)`
  width: 100%;
  min-height: calc(100vh - 50px);
  display: flex;
  flex-direction: row;
  align-items: stretch;
`;
