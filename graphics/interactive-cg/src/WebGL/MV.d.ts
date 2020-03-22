export declare function radians(degrees: number): number;

export declare interface DArray {
}

export declare interface DVec extends DArray {
}

export declare interface DMat extends DArray {
}

export declare type Vec2 = [number, number] & DVec;
export declare type Vec3 = [number, number, number] & DVec;
export declare type Vec4 = [number, number, number] & DVec;
export declare type Mat2 = [Vec2, Vec2] & { matrix: true } & DMat;
export declare type Mat3 = [Vec3, Vec3, Vec3] & { matrix: true } & DMat;
export declare type Mat4 = [Vec4, Vec4, Vec4, Vec4] & { matrix: true } & DMat;

export declare function vec2(first?: number, second?: number): Vec2;

export declare function vec3(first?: number, second?: number, third?: number): Vec3;

export declare function vec4(first?: number, second?: number, third?: number, forth?: number): Vec4;

export declare function mat2(identity?: number): Mat2;

export declare function mat3(identity?: number): Mat3;

export declare function mat4(identity?: number): Mat4;

export declare function equal(u: DArray, v: DArray): boolean;

export declare function add<T extends DArray>(u: T, v: T): T;

export declare function subtract<T extends DArray>(u: T, v: T): T;

export declare function mult<T extends DArray>(u: T, v: T): T;

export declare function translate(x: number, y: number, z: number): Mat4;
export declare function translate(input: Vec3): Mat4;

export declare function rotate(angle: number, axisX: number, axisY: number, axisZ: number): Mat4;
export declare function rotate(angle: number, axis: Vec3): Mat4;

export declare function rotateX(theta: number): Mat4;

export declare function rotateY(theta: number): Mat4;

export declare function rotateZ(theta: number): Mat4;

export declare function scalem(x: number, y: number, z: number): Mat4;
export declare function scalem(input: Vec3): Mat4;

export declare function scale<T extends DVec>(s: number, input: T): T;

export declare function lookAt(eye: Vec3, at: Vec3, up: Vec3): Mat4;

export declare function ortho(left: number, right: number, bottom: number, top: number, near: number, far: number): Mat4;

export declare function perspective(fovy: number, aspect: number, near: number, far: number): Mat4;

export declare function transpose<T extends DMat>(m: T): T;

export declare function dot<T extends DVec>(u: T, v: T): number;

export declare function negate<T extends DVec>(u: T): T;

export declare function cross(u: Vec3, v: Vec3): Vec3;

export declare function length(u: DVec): number;

export declare function normalize<T extends DVec>(u: T, excludeLastComponent: boolean): T;

export declare function mix<T extends DVec>(u: T, v: T, s: number): T;

export declare function flatten(u: DArray): Float32Array;

export declare function printm(m: DMat);

export declare function det2(m: Mat2): number;

export declare function det3(m: Mat3): number;

export declare function det4(m: Mat4): number;

export declare function det(m: DMat): number;

export declare function inverse2(m: Mat2): Mat2;

export declare function inverse3(m: Mat3): Mat3;

export declare function inverse4(m: Mat4): Mat4;

export declare function inverse<T extends DMat>(m: T): T;

export declare function normalMatrix(m: DMat, flag: boolean): Mat4;
