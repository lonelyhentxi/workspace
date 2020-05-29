import React, {useRef,useEffect} from 'react';
import './LearnThreeJS.css';

function LearnThreeJS() {
    const webGLPainters = [
        renderBasic, renderCube
    ];
    const webGLCanvasRefs = useRef<HTMLCanvasElement[]>(new Array(webGLPainters.length));
    const webGLPainterSizes = new Array(webGLPainters.length).fill([640, 480]);
    webGLPainters.reverse();
    webGLPainterSizes.reverse();
    useEffect(()=>{
        const tasks = webGLPainters.map((p,i)=>()=>{
            const canvas = webGLCanvasRefs.current[i]!;
            return p(canvas.getContext("webgl")!, canvas);
        });
        tasks.reverse();
        tasks.forEach(t=>t());
    });
    return (
        <div className="LearnWebGL">
            <section  className="LearnWebGL-gallery">
                {
                    webGLPainters.map((_,i)=>(
                        <canvas className="LearnWebGL-canvas"
                            key={i}
                            ref={el=>webGLCanvasRefs.current[i]=el!}
                            width = {webGLPainterSizes[i][0]}
                            height = {webGLPainterSizes[i][1]}
                        >
                            canvas size: {webGLPainterSizes[i][0]} x {webGLPainterSizes[i][1]}
                        </canvas>
                    ))
                }
            </section>
        </div>
    );
}

export default LearnThreeJS;
