import React from 'react';
import {
    BrowserRouter as Router,
    Switch,
    Route,
    NavLink,
    Redirect
} from 'react-router-dom';
import logo from './logo.svg';
import './App.css';
import LearnCanvas from './learn_canvas/LearnCanvas';
import LearnWebGL from './learn_webgl/LearnWebGL';
import LearnThreeJS from "./learn_threejs/LearnThreeJS";

function App() {
    return (
        <Router>
            <div className="App">
                <header className="App-header">
                    <img src={logo} className="App-logo" alt="logo"/>
                </header>
                <aside className="App-menu">
                    <NavLink
                        className="App-menu-link"
                        activeClassName="App-menu-link-active"
                        to="/learn-canvas">
                        Learn Canvas
                    </NavLink>
                    <NavLink
                        className="App-menu-link"
                        activeClassName="App-menu-link-active"
                        to="/learn-webgl">
                        Learn WebGL
                    </NavLink>
                    <NavLink
                        className="App-menu-link"
                        activeClassName="App-menu-link-active"
                        to="/learn-threejs">
                        Learn ThreeJS
                    </NavLink>
                </aside>
                <main className="App-main">
                    <Switch>
                        <Route exact path="/">
                            <Redirect to="/learn-canvas"/>
                        </Route>
                        <Route path='/learn-canvas'>
                            <LearnCanvas/>
                        </Route>
                        <Route path="/learn-webgl">
                            <LearnWebGL/>
                        </Route>
                        <Route path="/learn-threejs">
                            <LearnThreeJS/>
                        </Route>
                    </Switch>
                </main>
            </div>
        </Router>
    );
}

export default App;
