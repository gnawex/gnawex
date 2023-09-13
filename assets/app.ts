let order_cost = document.querySelector("#order-cost") as HTMLInputElement;
let order_quantity = document.querySelector("#order-quantity") as HTMLInputElement;

order_cost?.addEventListener("keyup", _event => {
  let costPerUnit = parseFloat(order_cost?.value);
  let computation = compute(costPerUnit);
  let incrementAsStr = `${computation[0]}`;

  order_quantity?.setAttribute("step", incrementAsStr);
  order_quantity?.setAttribute("min", `${computation[0]}`);
  order_quantity?.setAttribute("value", incrementAsStr);
});

function compute(cost: number): [number, number] {
  let bundleCost = cost * 100;
  let increment = 100;
  let x = 1;

  if (bundleCost < increment) {
    x = gcd(bundleCost, increment);
  } else {
    x = gcd(increment, bundleCost);
  }

  return [increment / x, bundleCost / x];
}

function gcd(a: number, b: number): number {
  while(a != 0) {
    let oldA = a;

    a = b % a;
    b = oldA;
  }

  return b;
}
