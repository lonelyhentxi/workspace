import React from "react";
import { Separator } from 'office-ui-fabric-react/lib/Separator';
import { Stack } from 'office-ui-fabric-react/lib/Stack';
import { Text } from 'office-ui-fabric-react/lib/Text';
import styled from "styled-components";

const LogoText = styled(Text)`
  margin: auto 24px;
  font-family: Consolas, serif;
  font-weight: bolder;
  font-size: 1.2em;
  color: #3b3a39;
  line-height: 44px;
`;

export const PlainLayoutHeader: React.FC<{ className: string}> = ({className}) => {
    return (
        <div className={className}>
            <Stack horizontal horizontalAlign="start"
                style={{
                    marginLeft: '5%',
                    minWidth: '90%',
                }}
            >
                <Stack horizontalAlign="center">
                    <LogoText>Interactive CG</LogoText>
                </Stack>
                <Stack horizontalAlign="center"
                    style={{
                        width: 'fit-content',
                        margin: 'auto 0',
                    }}>
                    <Separator vertical />
                </Stack>
            </Stack>
        </div>
    );
};

export const LayoutHeader = styled(PlainLayoutHeader)`
    width: 100%;
    height: 44px;
    box-shadow: 2px 0 2px 2px rgba(0,0,0,0.1);
    background: #fff;
`;
