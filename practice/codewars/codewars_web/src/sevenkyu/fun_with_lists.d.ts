export declare class FunWithListsNode
{
    data: any;
    next: FunWithListsNode|null;
    constructor(data: any,next?: FunWithListsNode|null);
}

export declare function nodeEqual(lhs: FunWithListsNode|null, rhs: FunWithListsNode|null): boolean;

export declare function listFromArray(array: any[]): FunWithListsNode | null;

export declare function indexOf(head: FunWithListsNode|null, value: any): number;

export declare function forEach(head: FunWithListsNode|null, callback: (item: any, index: number)=>any|undefined): any|undefined;

export declare function length(head: FunWithListsNode|null): number;

export declare function lastIndexOf(head: FunWithListsNode|null, value: any): number;

export declare function countIf(head: FunWithListsNode|null, p: (item: any)=>boolean): number;


export declare function anyMatch(head: FunWithListsNode|null, p: (item: any)=>boolean): boolean;

export declare function allMatch(head: FunWithListsNode|null, p: (item: any)=>boolean): boolean;

export declare function filter(head: FunWithListsNode|null, p: (item: any)=>boolean): FunWithListsNode|null;

export declare function map(head: FunWithListsNode|null, p: (item: any)=>any): FunWithListsNode|null;

export declare function reduce(head: FunWithListsNode|null, f: (acc: any, curr: any)=>any, init: any): any;