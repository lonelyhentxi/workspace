# NODEJS CPP EXTENSIONS

## Tips

### install tips

My enviroment: windows 10 (1803) 15063+, visual studio 2017 15.7, node 10.7.0, node-gyp 3.7.0

- switch npm mirror to none
- open proxy
    1. start proxy
    2. admin cmd: netsh winhttp import proxy source=ie
    3. set http_proxy = http://127.0.0.1:1080
    4. set https_proxy = http://127.0.0.1:1080
- npm install -g --production windows-build-tools (msbuild in vs2015)
- npm install -g node-gyp
- optional: if use vs2017
    1. npm config set msvs_version 2015
    2. open visual studio installer: change config => select windows sdk 14393

## tutorial

[from XadillaX/nyaa-nodejs-demo: node.js 来一打 C++ 拓展](https://github.com/XadillaX/nyaa-nodejs-demo/blob/master/README.md)