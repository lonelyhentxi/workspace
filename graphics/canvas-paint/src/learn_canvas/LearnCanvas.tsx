import React, { useEffect, useRef } from 'react';
import './LearnCanvas.css';

function drawBasic(ctx: CanvasRenderingContext2D) {
    ctx.fillStyle = "rgb(200, 0, 0)";
    ctx.fillRect(10, 10, 55, 50);
    ctx.fillStyle = "rgb(0, 0, 200, 0.5)";
    ctx.fillRect(30, 30, 55, 50);
}

function drawRectangle(ctx: CanvasRenderingContext2D) {
    ctx.fillRect(25, 25, 100, 100);
    ctx.clearRect(45, 45, 60, 60);
    ctx.strokeRect(50, 50, 50, 50);
}

function drawPath(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();
    ctx.moveTo(75, 50);
    ctx.lineTo(100, 75);
    ctx.lineTo(100, 25);
    // fill will close path automatically
    ctx.fill();
}

function drawMoveTo(ctx: CanvasRenderingContext2D) {
    ctx.beginPath()
    ctx.arc(75, 75, 50, 0, Math.PI * 2, true);
    ctx.moveTo(110, 75);
    ctx.arc(75, 75, 35, 0, Math.PI, false);
    ctx.moveTo(65, 65);
    ctx.arc(60, 65, 5, 0, Math.PI * 2, true);
    ctx.moveTo(95, 65);
    ctx.arc(90, 65, 5, 0, Math.PI * 2, true);
    ctx.stroke();
}

function drawLineTo(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();
    ctx.moveTo(25, 25);
    ctx.lineTo(105, 25);
    ctx.lineTo(25, 105);
    ctx.fill();

    ctx.beginPath()
    ctx.moveTo(125, 125);
    ctx.lineTo(45, 125);
    ctx.lineTo(125, 45);
    ctx.closePath();
    // stroke line will not auto close path
    ctx.stroke();
}

function drawArc(ctx: CanvasRenderingContext2D) {
    for (let i = 0; i < 3; i++) {
        for (let j = 0; j < 3; j++) {
            ctx.beginPath();
            const x = 25 + j * 50;
            const y = 25 + i * 50;
            const radius = 20;
            const startAngle = 0;
            const endAngle = Math.PI + (Math.PI * j) / 2;
            const anticlockwise = i % 2 === 0 ? false : true;
            ctx.arc(x, y, radius, startAngle, endAngle, anticlockwise);
            if (i > 1) {
                ctx.fill();
            } else {
                ctx.stroke();
            }
        }
    }
}

function drawQuadraticCurve(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();
    ctx.moveTo(75, 25);
    ctx.quadraticCurveTo(25, 25, 25, 62.5);
    ctx.quadraticCurveTo(25, 100, 50, 100);
    ctx.quadraticCurveTo(50, 120, 30, 125);
    ctx.quadraticCurveTo(60, 120, 65, 100);
    ctx.quadraticCurveTo(125, 100, 125, 62.5);
    ctx.quadraticCurveTo(125, 25, 75, 25);
    ctx.stroke();
}

function drawBezierCurve(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();
    ctx.moveTo(75, 40);
    ctx.bezierCurveTo(75, 37, 70, 25, 50, 25);
    ctx.bezierCurveTo(20, 25, 20, 62.5, 20, 62.5);
    ctx.bezierCurveTo(20, 80, 40, 102, 75, 120);
    ctx.bezierCurveTo(110, 102, 130, 80, 130, 62.5);
    ctx.bezierCurveTo(130, 62.5, 130, 25, 100, 25);
    ctx.bezierCurveTo(85, 25, 75, 37, 75, 40);
    ctx.fill();
}

function drawRoundedRect(ctx: CanvasRenderingContext2D,
    x: number, y: number, width: number, height: number, radius: number) {
    ctx.beginPath();
    ctx.moveTo(x, y + radius);
    ctx.lineTo(x, y + height - radius);
    ctx.quadraticCurveTo(x, y + height, x + radius, y + height);
    ctx.lineTo(x + width - radius, y + height);
    ctx.quadraticCurveTo(x + width, y + height, x + width, y + height - radius);
    ctx.lineTo(x + width, y + radius);
    ctx.quadraticCurveTo(x + width, y, x + width - radius, y);
    ctx.lineTo(x + radius, y);
    ctx.quadraticCurveTo(x, y, x, y + radius);
    ctx.stroke();
}

function drawPacman(ctx: CanvasRenderingContext2D) {
    drawRoundedRect(ctx, 12, 12, 150, 150, 15);
    drawRoundedRect(ctx, 19, 19, 150, 150, 9);
    drawRoundedRect(ctx, 53, 53, 49, 33, 10);
    drawRoundedRect(ctx, 53, 119, 49, 16, 6);
    drawRoundedRect(ctx, 135, 53, 49, 33, 10);
    drawRoundedRect(ctx, 135, 119, 25, 49, 10);
    for (var i = 0; i < 8; i++) {
        ctx.fillRect(51 + i * 16, 35, 4, 4);
    }

    for (i = 0; i < 6; i++) {
        ctx.fillRect(115, 51 + i * 16, 4, 4);
    }

    for (i = 0; i < 8; i++) {
        ctx.fillRect(51 + i * 16, 99, 4, 4);
    }

    ctx.beginPath();
    ctx.moveTo(83, 116);
    ctx.lineTo(83, 102);
    ctx.bezierCurveTo(83, 94, 89, 88, 97, 88);
    ctx.bezierCurveTo(105, 88, 111, 94, 111, 102);
    ctx.lineTo(111, 116);
    ctx.lineTo(106.333, 111.333);
    ctx.lineTo(101.666, 116);
    ctx.lineTo(97, 111.333);
    ctx.lineTo(92.333, 116);
    ctx.lineTo(87.666, 111.333);
    ctx.lineTo(83, 116);
    ctx.fill();

    ctx.fillStyle = "white";
    ctx.beginPath();
    ctx.moveTo(91, 96);
    ctx.bezierCurveTo(88, 96, 87, 99, 87, 101);
    ctx.bezierCurveTo(87, 103, 88, 106, 91, 106);
    ctx.bezierCurveTo(94, 106, 95, 103, 95, 101);
    ctx.bezierCurveTo(95, 99, 94, 96, 91, 96);
    ctx.moveTo(103, 96);
    ctx.bezierCurveTo(100, 96, 99, 99, 99, 101);
    ctx.bezierCurveTo(99, 103, 100, 106, 103, 106);
    ctx.bezierCurveTo(106, 106, 107, 103, 107, 101);
    ctx.bezierCurveTo(107, 99, 106, 96, 103, 96);
    ctx.fill();

    ctx.fillStyle = "black";
    ctx.beginPath();
    ctx.arc(101, 102, 2, 0, Math.PI * 2, true);
    ctx.fill();

    ctx.beginPath();
    ctx.arc(89, 102, 2, 0, Math.PI * 2, true);
    ctx.fill();

    ctx.beginPath();
    ctx.arc(37, 37, 13, Math.PI / 7, -Math.PI / 7, false);
    ctx.lineTo(31, 37);
    ctx.fill();
}

function drawPath2D(ctx: CanvasRenderingContext2D) {
    const rectangle = new Path2D();
    rectangle.rect(10, 10, 50, 50);
    const circle = new Path2D();
    circle.moveTo(125, 35);
    circle.arc(100, 35, 25, 0, 2 * Math.PI);

    ctx.stroke(rectangle);
    ctx.fill(circle);
}

function drawSVGPaths(ctx: CanvasRenderingContext2D) {
    const p = new Path2D("M10 10 h 80 v 80 h -80 Z");
    ctx.stroke(p);
}

function drawFillStyle(ctx: CanvasRenderingContext2D) {
    for (let i = 0; i < 5; i++) {
        for (let j = 0; j < 5; j++) {
            ctx.fillStyle =
                `rgb(${Math.floor(255 - 50 * i)},${Math.floor(255 - 50 * j)},0)`;
            ctx.fillRect(j * 30, i * 30, 30, 30);
        }
    }
}

function drawStrokeStyle(ctx: CanvasRenderingContext2D) {
    for (let i = 0; i < 5; i++) {
        for (let j = 0; j < 5; j++) {
            ctx.strokeStyle = `rgba(0, ${Math.floor(255 - 50 * i)},${Math.floor(255 - 50 * j)},${(1 + i) * (1 + j) * 0.04})`;
            ctx.beginPath();
            ctx.arc(15 + j * 30, 15 + i * 30, 10, 0, Math.PI * 2, true);
            ctx.stroke();
        }
    }
}

function drawGlobalAlpha(ctx: CanvasRenderingContext2D) {
    ctx.fillStyle = '#FD0';
    ctx.fillRect(0, 0, 75, 75);
    ctx.fillStyle = '#6C0';
    ctx.fillRect(75, 0, 75, 75);
    ctx.fillStyle = '#09F';
    ctx.fillRect(0, 75, 75, 75);
    ctx.fillStyle = '#F30';
    ctx.fillRect(75, 75, 75, 75);
    ctx.fillStyle = '#FFF';
    ctx.globalAlpha = 0.2;
    for (var i = 0; i < 7; i++) {
        ctx.beginPath();
        ctx.arc(75, 75, 10 + 10 * i, 0, Math.PI * 2, true);
        ctx.fill();
    }
}

function drawLineWidth(ctx: CanvasRenderingContext2D) {
    for (let i = 0; i < 10; i++) {
        ctx.lineWidth = 1 + i;
        ctx.beginPath();
        ctx.moveTo(5 + i * 14, 5);
        ctx.lineTo(5 + i * 14, 140);
        ctx.stroke();
    }
}

function drawLineCap(ctx: CanvasRenderingContext2D) {
    // 
    const lineCaps: CanvasLineCap[] = ['butt', 'round', 'square'];
    ctx.strokeStyle = '#09f';
    ctx.beginPath();
    ctx.moveTo(10, 10);
    ctx.lineTo(140, 10);
    ctx.moveTo(10, 140);
    ctx.lineTo(140, 140);
    ctx.stroke();
    ctx.strokeStyle = 'black';
    for (var i = 0; i < lineCaps.length; i++) {
        ctx.lineWidth = 15;
        ctx.lineCap = lineCaps[i];
        ctx.beginPath();
        ctx.moveTo(25 + i * 50, 10);
        ctx.lineTo(25 + i * 50, 140);
        ctx.stroke();
    }
}

function drawLineJoin(ctx: CanvasRenderingContext2D) {
    // bevel 倒角；miter 斜接
    const lineJoin: CanvasLineJoin[] = ['round', 'bevel', 'miter'];
    ctx.lineWidth = 10;
    // there is a miterlimit
    for (let i = 0; i < lineJoin.length; i++) {
        ctx.lineJoin = lineJoin[i];
        ctx.beginPath();
        ctx.lineTo(35, 45 + i * 40);
        ctx.lineTo(75, 5 + i * 40);
        ctx.lineTo(115, 45 + i * 40);
        ctx.lineTo(155, 5 + i * 40);
        ctx.stroke();
    }
}

function drawLineDash(ctx: CanvasRenderingContext2D) {
    let offset = 0;
    const draw = () => {
        ctx.clearRect(10, 10, 130, 130);
        // fill 4 empty 2
        ctx.setLineDash([4, 2]);
        ctx.lineDashOffset = -offset;
        ctx.strokeRect(20, 20, 110, 110);
    };

    const march = () => {
        offset++;
        if (offset > 16) {
            offset = 0;
        }
        draw();
        setTimeout(march, 20);
    };
    march();
}

function drawLinearGradients(ctx: CanvasRenderingContext2D) {
    const lineargradient = ctx.createLinearGradient(0, 0, 0, 150);
    lineargradient.addColorStop(0, '#00ABEB');
    lineargradient.addColorStop(0.5, '#fff');
    lineargradient.addColorStop(0.5, '#26C000');
    lineargradient.addColorStop(1, '#fff');
    ctx.fillStyle = lineargradient;
    ctx.fillRect(10, 10, 130, 130);
    const lineargradient2 = ctx.createLinearGradient(0, 50, 0, 95);
    lineargradient2.addColorStop(0.5, '#000');
    lineargradient2.addColorStop(1, 'rgba(0,0,0,0)');
    ctx.strokeStyle = lineargradient2;
    ctx.strokeRect(50, 50, 50, 50);
}

function drawRadialGradients(ctx: CanvasRenderingContext2D) {
    const radgrad = ctx.createRadialGradient(45, 45, 10, 52, 50, 30);
    radgrad.addColorStop(0, '#A7D30C');
    radgrad.addColorStop(0.9, '#019F62');
    radgrad.addColorStop(1, 'rgba(1,159,98,0)');

    const radgrad2 = ctx.createRadialGradient(105, 105, 20, 112, 120, 50);
    radgrad2.addColorStop(0, '#FF5F98');
    radgrad2.addColorStop(0.75, '#FF0188');
    radgrad2.addColorStop(1, 'rgba(255,1,136,0)');

    const radgrad3 = ctx.createRadialGradient(95, 15, 15, 102, 20, 40);
    radgrad3.addColorStop(0, '#00C9FF');
    radgrad3.addColorStop(0.8, '#00B5E2');
    radgrad3.addColorStop(1, 'rgba(0,201,255,0)');

    const radgrad4 = ctx.createRadialGradient(0, 150, 50, 0, 140, 90);
    radgrad4.addColorStop(0, '#F4F201');
    radgrad4.addColorStop(0.8, '#E4C700');
    radgrad4.addColorStop(1, 'rgba(228,199,0,0)');

    ctx.fillStyle = radgrad4;
    ctx.fillRect(0, 0, 150, 150);
    ctx.fillStyle = radgrad3;
    ctx.fillRect(0, 0, 150, 150);
    ctx.fillStyle = radgrad2;
    ctx.fillRect(0, 0, 150, 150);
    ctx.fillStyle = radgrad;
    ctx.fillRect(0, 0, 150, 150);
}

function drawPattern(ctx: CanvasRenderingContext2D) {
    const img = new Image();
    img.src = "https://mdn.mozillademos.org/files/222/Canvas_createpattern.png";
    img.onload = function () {
        const pattern = ctx.createPattern(img, 'repeat');
        if (pattern != null) {
            ctx.fillStyle = pattern;
            ctx.fillRect(0, 0, 150, 150);
        }
    }
}

function drawShadow(ctx: CanvasRenderingContext2D) {
    ctx.shadowOffsetX = 2;
    ctx.shadowOffsetY = 2;
    ctx.shadowBlur = 2;
    ctx.shadowColor = "rgba(0, 0, 0, 0.5)";
    ctx.font = "20px Times New Roman";
    ctx.fillStyle = "Black";
    ctx.fillText("Sample String", 5, 30);
}

function drawFillRules(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();
    ctx.arc(50, 50, 30, 0, Math.PI * 2, true);
    ctx.arc(50, 50, 15, 0, Math.PI * 2, true);
    ctx.fill("evenodd"); // or nonzero
}

function drawFillText(ctx: CanvasRenderingContext2D) {
    ctx.font = "15px serif";
    ctx.fillText("Hello world", 10, 50);
}

function drawStrokeText(ctx: CanvasRenderingContext2D) {
    ctx.font = "15px serif";
    ctx.textBaseline = "hanging";
    const text = ctx.measureText("Hello World");
    ctx.strokeText(`Hello World: ${text.width}`, 10, 50);
}

function drawImage(ctx: CanvasRenderingContext2D) {
    const img = new Image();
    img.src = "https://mdn.mozillademos.org/files/222/Canvas_createpattern.png";
    img.onload = () => {
        ctx.drawImage(img, 0, 0);
    }
}

function drawDataUrl(ctx: CanvasRenderingContext2D) {
    const img = new Image();
    img.onload = () => {
        ctx.drawImage(img, 0, 0, 150, 150);
    };
    img.src = 'data:image/gif;base64,R0lGODlhCwALAIAAAAAA3pn/ZiH5BAEAAAEALAAAAAALAAsAAAIUhA+hkcuO4lmNVindo7qyrIXiGBYAOw==';
}

function drawVideoFrame(ctx: CanvasRenderingContext2D) {
    const video = document.createElement('VIDEO') as HTMLVideoElement;
    video.setAttribute("controls", "controls");
    video.setAttribute("src","https://www.w3schools.com/html/mov_bbb.mp4");
    video.onloadeddata = () => {
        ctx.drawImage(video, 0, 0, 320, 176, 0, 0, 150, 150);
    };
}

// canvas will sync draw
function drawCanvasScreenshot(ctx: CanvasRenderingContext2D) {
    const items = document
        .getElementsByClassName("LearnCanvas-canvas");
    const targetCanvas = items.item(items.length-1) as HTMLCanvasElement;
    ctx.drawImage(targetCanvas, 0, 0, 150, 150);
}

function drawSaveRestore(ctx: CanvasRenderingContext2D) {
    ctx.fillRect(0, 0, 150, 150);
    // push to stack
    ctx.save();   
    ctx.fillStyle = '#09F'
    ctx.fillRect(15,15,120,120);
    ctx.save();  
    ctx.fillStyle = '#FFF'   
    ctx.globalAlpha = 0.5;  
    ctx.fillRect(30,30,90,90); 
    // pop from stack
    ctx.restore();    
    ctx.fillRect(45,45,60,60);    
    ctx.restore();    
    ctx.fillRect(60,60,30,30);
}

function drawTranslate(ctx: CanvasRenderingContext2D) {
    for (var i = 0; i < 3; i++) {
        for (var j = 0; j < 3; j++) {
          ctx.save();
          ctx.fillStyle = 'rgb(' + (51 * i) + ', ' + (255 - 51 * i) + ', 255)';
          ctx.translate(10 + j * 50, 10 + i * 50);
          ctx.fillRect(0, 0, 25, 25);
          ctx.restore();
        }
    }
}

function drawRotate(ctx: CanvasRenderingContext2D) {
    ctx.translate(75, 75);
    for(let i=0; i<6;i++) {
        ctx.save();
        ctx.fillStyle = `rgb(${51*i},${255-51*i},255)`;
        for(let j=0;j<i*6;j++) {
            // rotate x, not rotate to x
            ctx.rotate(Math.PI*2/(i*6));
            ctx.beginPath();
            ctx.arc(0, i*12.5, 5, 0, Math.PI * 2, true);
            ctx.fill();
        }
        ctx.restore();
    }
}

function drawScale(ctx: CanvasRenderingContext2D) {
    ctx.save();
    ctx.scale(10, 3);
    ctx.fillRect(1, 10, 10, 10);
    ctx.restore();
    ctx.scale(-1, 1);
    ctx.font = "48px serif";
    ctx.fillText("MDN", -135, 120);
}

function drawTransform(ctx: CanvasRenderingContext2D) {
    const sin = Math.sin(Math.PI/6);
    const cos = Math.cos(Math.PI/6);
    ctx.translate(75,75);
    let c = 0;
    for(let i=0; i<=12; i++) {
        c = Math.floor(255/12*i);
        ctx.fillStyle = `rgb(${c},${c},${c})`;
        ctx.fillRect(0, 0, 75, 10);
        ctx.transform(cos, sin, -sin, cos, 0, 0);
    }
    // reset transform matrix to unit transform
    // then set transform
    ctx.setTransform(-1, 0, 0, 1, 75, 75);
    ctx.fillStyle = `rgba(255, 128, 255, 0.5)`;
    ctx.fillRect(0, 25, 75, 75);
}

// Compositing see [Compositing](https://developer.mozilla.org/zh-CN/docs/Web/API/Canvas_API/Tutorial/Compositing/Example)

function drawStar(ctx: CanvasRenderingContext2D, r: number) {
    ctx.save();
    ctx.beginPath();
    ctx.moveTo(r, 0);
    for(let i=0;i<9;i++) {
        ctx.rotate(Math.PI/5);
        if(i%2===0) {
            ctx.lineTo((r/0.525731)*0.200811,0);
        } else {
            ctx.lineTo(r, 0);
        }
    }
    ctx.closePath();
    ctx.fill();
    ctx.restore();
}

function drawClip(ctx: CanvasRenderingContext2D) {
    ctx.fillRect(0, 0, 150, 150);
    ctx.translate(75, 75);
    ctx.beginPath();
    ctx.arc(0, 0, 60, 0, Math.PI * 2, true);
    ctx.clip();
    const lingrad = ctx.createLinearGradient(0, -75, 0, 75);
    lingrad.addColorStop(0, '#232256');
    lingrad.addColorStop(1, '#143778');
    ctx.fillStyle = lingrad;
    ctx.fillRect(-75, -75, 150, 150);
    for (var j=1;j<50;j++){
        ctx.save();
        ctx.fillStyle = '#fff';
        ctx.translate(75-Math.floor(Math.random()*150),
                      75-Math.floor(Math.random()*150));
        drawStar(ctx,Math.floor(Math.random()*4)+2);
        ctx.restore();
    }
}

function drawScheduledAnimation(ctx: CanvasRenderingContext2D) {
    const sun = new Image();
    const moon = new Image();
    const earth = new Image();
    const loadImage = (image: HTMLImageElement, src: string) => {
        return new Promise((resolve, reject) => {
            image.onload = () => {
                resolve();
            }
            image.src = src;
            setTimeout(reject, 10000);
        });
    };
    const draw = () => {
        ctx.globalCompositeOperation = "destination-over";
        ctx.clearRect(0,0,150, 150);
        ctx.fillStyle = 'rgba(0,0,0,0.4)';
        ctx.strokeStyle = 'rgba(0,153,255,0.4)';
        ctx.save();
        ctx.translate(75,75);
        const time = new Date();
        ctx.rotate( ((2*Math.PI)/60)*time.getSeconds() + ((2*Math.PI)/60000)*time.getMilliseconds() );
        ctx.translate(52.5,0);
        ctx.fillRect(0, -6, 25, 12);
        ctx.drawImage(earth, -6, -6);
        ctx.save();
        ctx.rotate( ((2*Math.PI)/6)*time.getSeconds() + ((2*Math.PI)/6000)*time.getMilliseconds() );
        ctx.translate(0,14.25);
        ctx.drawImage(moon,-1.75,-1.75);
        ctx.restore();
        ctx.restore();
        ctx.beginPath();
        ctx.arc(75,75, 52.5, 0, Math.PI * 2, true);
        ctx.stroke();
        ctx.drawImage(sun, 0, 0, 150, 150);
        window.requestAnimationFrame(draw);
    };
    Promise.all(
        [
            [sun, 'https://mdn.mozillademos.org/files/1456/Canvas_sun.png'],
            [moon, 'https://mdn.mozillademos.org/files/1443/Canvas_moon.png'],
            [earth, 'https://mdn.mozillademos.org/files/1429/Canvas_earth.png']
        ].map(pair=>loadImage(pair[0] as HTMLImageElement, pair[1] as string))
    ).then(()=>{
        draw();
    }, ()=> {
        ctx.fillText("Error in load image", 0, 0);
    });
}

class Ball {
    x: number = 50;
    y: number = 50;
    radius: number = 12.5;
    color: string = 'blue';
    vx: number = 4;
    vy: number = 1;
    ctx: CanvasRenderingContext2D;
    constructor(ctx: CanvasRenderingContext2D) {
        this.ctx = ctx;
    }

    draw() {
        const ctx = this.ctx;
        ctx.beginPath();
        ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2, true);
        ctx.closePath();
        ctx.fillStyle = this.color;
        ctx.fill();
    }
}

function drawAdvancedAnimation(ctx: CanvasRenderingContext2D, canvas: HTMLCanvasElement) {
    const ball = new Ball(ctx);
    let raf: number;
    let running = false;
    const clear = () => {
        ctx.fillStyle = 'rgba(255,255,255,0.3)';
        ctx.fillRect(0,0,canvas.width,canvas.height);
    };
    const draw = () => {
        clear();
        ball.draw();
        if(ball.y + ball.vy > canvas.height || ball.y + ball.vy < 0) {
            ball.vy = -ball.vy;
        }
        if(ball.x+ball.vx > canvas.width || ball.x+ball.vx < 0) {
            ball.vx = -ball.vx;
        }
        ball.x += ball.vx;
        ball.y += ball.vy;
        ball.vy *= .99;
        ball.vy += .25;
        raf = window.requestAnimationFrame(draw);
    };
    canvas.addEventListener('mousemove', (e)=>{
        if(!running) {
            clear();
            ball.x = e.clientX - canvas.getBoundingClientRect().left;
            ball.y = e.clientY -  canvas.getBoundingClientRect().top;
            ball.draw();
        }
    });

    canvas.addEventListener('click',function(e){
        if (!running) {
          raf = window.requestAnimationFrame(draw);
          running = true;
          ball.vx = 4;
          ball.vy = 1;
        }
      });
      
      canvas.addEventListener('mouseout', function(e){
        window.cancelAnimationFrame(raf);
        running = false;
      });
      
      ball.draw();
}

function drawGetImageData(ctx: CanvasRenderingContext2D, canvas: HTMLCanvasElement) {
    const lingrad = ctx.createLinearGradient(0, 0, 150, 100);
    lingrad.addColorStop(0, '#000');
    lingrad.addColorStop(1, '#fff');
    ctx.fillStyle = lingrad;
    ctx.fillRect(0, 0, 150, 100);
    canvas.addEventListener('click', (event) => {
        const x = event.clientX - canvas.getBoundingClientRect().left;
        const y = event.clientY - canvas.getBoundingClientRect().top;
        if(x>=0 && y>=0 && x<=150 && y<=100 ) {
            const pixel = ctx.getImageData(x, y, 1, 1);
            const data = pixel.data;
            const rgba = `rgba(${data[0]},${data[1]},${data[2]},${data[3]/255})`;
            ctx.clearRect(0, 100, 150, 50);
            ctx.fillStyle = rgba;
            ctx.fillRect(0, 100, 150, 50);
            ctx.textBaseline = "top";
            ctx.fillStyle = `rgba(${255-data[0]},${255-data[1]},${255-data[2]},${data[3]/255})`;
            ctx.fillText(rgba, 0, 100);
        }
    });
}

function drawScaleAndSmooth(ctx: CanvasRenderingContext2D, canvas: HTMLCanvasElement) {
    const img = new Image();
    const draw = (img: HTMLImageElement) => {
        ctx.drawImage(img, 0, 0, 150, 100);
        img.style.display = "none";
        ctx.textBaseline = "top";
        ctx.font = "20 Serifs"
        ctx.fillText("ZOOM", 0, 100);
    };
    const zoom = (event: MouseEvent) => {
        const x = event.clientX - canvas.getBoundingClientRect().left;
        const y = event.clientY - canvas.getBoundingClientRect().top;
        ctx.imageSmoothingEnabled = true;
        ctx.drawImage(canvas, x-5, y-5, 10, 10, 100, 100, 50, 50);
    };
    img.onload = () => {
        draw(img);
        canvas.addEventListener('mousemove', zoom);
    };
    img.src = "https://mdn.mozillademos.org/files/5397/rhino.jpg";
}

// use canvas toDataURL('image/png') to png
// use canvas toDataURL('image/jpeg', quality) to jpeg
// use canvas.toBlob(callback, type, encoderOptions) to Blob

// use aria-pressed and aria-label to support aria access

/*
function drawOffscreenContext(ctx: CanvasRenderingContext2D, canvas: HTMLCanvasElement) {
    const offscreenCanvas = document.createElement("canvas") as HTMLCanvasElement;
    offscreenCanvas.width = 150;
    offscreenCanvas.height = 150;
    const offscreenCtx = offscreenCanvas.getContext("2d", {alpha: false})!;
    offscreenCtx.arc(75, 75, 10, 0, Math.PI * 2, true);
    offscreenCtx.fillStyle = "red";
    offscreenCtx.fill();
    
}*/

function LearnCanvas() {
    const canvasPainters = [
        drawBasic, drawRectangle, drawPath, drawMoveTo, drawLineTo,
        drawArc, drawQuadraticCurve, drawBezierCurve, drawPacman, drawPath2D,
        drawSVGPaths, drawFillStyle, drawStrokeStyle, drawGlobalAlpha, drawLineWidth, 
        drawLineCap, drawLineJoin,drawLineDash, drawLinearGradients, drawRadialGradients,
        drawPattern, drawShadow, drawFillRules, drawFillText, drawStrokeText,
        drawImage, drawDataUrl, drawVideoFrame, drawCanvasScreenshot, drawSaveRestore,
        drawTranslate, drawRotate, drawScale, drawTransform, drawClip,
        drawScheduledAnimation,drawAdvancedAnimation, drawGetImageData,drawScaleAndSmooth,
    ];
    const canvasSizes = new Array(canvasPainters.length).fill([150, 150]);
    canvasSizes[36] = [300, 150];
    canvasPainters.reverse();
    canvasSizes.reverse();
    const canvasRefs = useRef<HTMLCanvasElement[]>(new Array(canvasPainters.length));
    useEffect(() => {
        const tasks = canvasPainters.map((p, i) => 
        () => { 
            const canvas = canvasRefs.current[i];
            return p(canvas.getContext('2d')!, canvas); 
        });
        tasks.reverse();
        tasks.forEach(t=>t());
    });
    return (
        <div className="LearnCanvas">
            <section className="LearnCanvas-gallery">
                {
                    canvasPainters.map((_, i) => (
                        <canvas
                        key={i}
                        ref={el => canvasRefs.current[i] = el!}
                        className="LearnCanvas-canvas"
                        height={canvasSizes[i][1]}
                        width={canvasSizes[i][0]}>
                        current canvas size: {canvasSizes[i][0]} * {canvasSizes[i][1]}
                        </canvas>
                    ))
                }
            </section>
        </div>
    );
}

export default LearnCanvas;