export declare class FunWithListsNode
{
    data: any;
    next: FunWithListsNode|null;
    constructor(data: any,next: FunWithListsNode|null);
}

export declare function indexOf(head: FunWithListsNode|null, value: any): number;

export declare function forEach(head: FunWithListsNode|null, callback: (item: any, index: number)=>any|undefined): any|undefined;

export declare function length(head: FunWithListsNode|null): number;

export declare function lastIndexOf(head: FunWithListsNode|null, value: any): number;

export declare function countIf(head: FunWithListsNode|null, p: (item: any)=>boolean): number;