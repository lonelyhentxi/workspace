import {BigNumber} from 'bignumber.js';

const simpleEqual = function<T>(source:T,target:T,
                             sstart:number, tstart:number,
                             length:number):boolean {
  for(let i=0;i<length;i++){
    if(source[sstart+i]!==target[tstart+i]) {
      return false;
    }
  }
  return true;
};

export const naiveSearch = function(source:string,sub:string):boolean{
  const n = source.length;
  const m = sub.length;
  for(let i=0;i<=n-m;i++) {
    if(simpleEqual(source,sub,i,0,m)){
      return true;
    }
  }
  return false;
};

export const rabinKarpSearch = function(source: number[], sub: number[]):boolean {
  const n = source.length;
  const m = sub.length;
  const prime = generatePrime(m,1)[0];
  const c  = new BigNumber(`10e${m-1}`).mod(prime).toNumber();
  let fp:number = 0;
  let ft:number = 0;
  for(let i =0;i<m;i++) {
    fp = (fp*10+sub[i])%prime;
    ft = (ft*10+source[i])%prime;
  }
  for(let i=0;i<n-m+1;i++) {
    if(fp===ft) {
      if(simpleEqual(source,sub,i,0,m)){
        return true;
      }
    }
    ft = ((ft-source[i]*c)*10 + source[i+m])%prime;
  }
  return false;
};

export const computePrefixTable = function(target:string):number[]{
  const m = target.length;
  const prefixTable:number[] = new Array<number>(m);
  prefixTable[0]=-1;
  let k=-1;
  for(let i=1;i<m;i++) {
    while(k>-1&&target.charCodeAt(k+1)!==target.charCodeAt(i)){
      k=prefixTable[k];
    }
    if(target[k+1]===target[i]) {
      k=k+1;
    }
    prefixTable[i] = k;
  }
  return prefixTable;
};

export const knuthMorrisPratt = function(source,sub) {
  const n = source.length;
  const m = sub.length;

};