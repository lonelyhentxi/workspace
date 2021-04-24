import { interval, from, merge, NEVER, EMPTY, of } from "rxjs";

import { exhaustMap, scan, tap } from "rxjs/operators";

const values = [false, false, false, true, false];

const fn = async (i) => {
  return new Promise((resolve) => {
    setTimeout(() => {
      console.log(Date.now(), values[i], i);
      resolve({ success: values[i] });
    }, 500);
  });
};

interval(100)
  .pipe(
    scan((acc, _) => (acc + 1) % 5, 0),
    exhaustMap((i) =>
      from(fn(i)).pipe(
        exhaustMap((res) => merge(res.success ? NEVER : EMPTY, of(res)))
      )
    ),
    tap((v) => console.log(v))
  )
  .subscribe(() => {});
