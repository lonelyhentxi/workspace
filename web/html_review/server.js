const http = require('http').createServer();
const fs = require('fs');
const path = require('path');
const cheerio = require('cheerio');
const io = require('socket.io')(http);
const { Subject, Observable } = require('rxjs');
const { tap, throttleTime } = require('rxjs/operators');

const baseDir = path.join(__dirname, 'src');
const subject = new Subject();
new Observable((observer) => {
    fs.watch(baseDir, (_, filename) => {
        observer.next(filename);
    })
}).pipe(
    throttleTime(1000),
    tap((filename) => console.info(`[reload]: file ${filename} changed`)),
).subscribe(subject);

io.on('connection', (socket) => {
    console.info('[connection]: a user connected')
    const subscription = subject.subscribe((filename) => {
        io.emit('reload', filename);
    })
    socket.on('disconnect', () => {
        console.info('[disconnection]: a user disconnected');
        subscription.unsubscribe();
    })
})

http.on('request', (req, res) => {
    const filename = req.url.replace(/^\/|\/$/g, '');
    if (filename.startsWith('socket.io')) {
        if (filename === 'socket.io.js') {
            console.info(`[request]: access ${filename}`);
            res.writeHead(200, { 'Content-Type': 'application/x-javascript' });
            res.write(fs.readFileSync(
                path.join(__dirname, 'node_modules', 'socket.io-client', 'dist', 'socket.io.js'),
                {
                    encoding: 'utf-8'
                }
            ));
            res.end();
        }
    } else {
        console.info(`[request]: access ${filename}`);
        if (filename.indexOf(/[\/\\]/g) !== -1) {
            res.writeHead(400, { 'Content-Type': 'text/plain' });
            res.write('url should be simple string');
            res.end();
        } else {
            const filepath = path.join(baseDir, filename);
            if (fs.existsSync(filepath) && fs.lstatSync(filepath).isFile()) {
                const html = fs.readFileSync(filepath, {
                    encoding: 'utf-8'
                });
                const $ = cheerio.load(html);
                $('head').append('<script src="./socket.io.js"></script>')
                $('head').append(
                    `<script>
                    const socket = io();
                    socket.on('reload',()=>{
                        window.location.reload();
                    })
                </script>`)
                res.writeHead(200, { 'Content-Type': 'text/html' });
                res.write($.html());
                res.end();
            } else {
                res.writeHead(404, { 'Content-Type': 'text/plain' });
                res.write('not found');
                res.end();
            }
        }
    }
});

http.listen(8080);