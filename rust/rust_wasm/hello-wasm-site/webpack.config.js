const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    entry: "./src/index.js",
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: "[name].bundle.js"
    },
    mode: "development",
    devServer: {
        contentBase: path.resolve(__dirname, "dist"),
        compress: false,
        port: 4200,
        open: true
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: "hello-wasm",
            template: "./src/index.html",
            filename: "index.html"
        })
    ]
};
