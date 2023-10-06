let order_cost = document.querySelector("#order-cost") as HTMLInputElement;
let order_quantity = document.querySelector("#order-quantity") as HTMLInputElement;

// TODO: Clean up this mess

order_cost?.addEventListener("blur", _event => populateQuantity());

order_quantity?.addEventListener("blur", _event => {
  let costPerUnit = parseFloat(order_cost?.value);
  let bundleCost = parseInt((costPerUnit * 100).toFixed(2));
  let computation = compute(bundleCost);
  let currentQuantity = parseInt(order_quantity?.value);

  if(currentQuantity % computation[0] != 0) {
    order_quantity.value = `${computation[0]}`;
  }
})

function populateQuantity() {
  let costPerUnit = parseFloat(order_cost?.value);
  let bundleCost = parseInt((costPerUnit * 100).toFixed(2));
  let computation = compute(bundleCost);
  let incrementAsStr = `${computation[0]}`;

  order_quantity?.setAttribute("step", incrementAsStr);
  order_quantity?.setAttribute("min", `${computation[0]}`);
  order_cost.value = `${bundleCost / 100}`;
  order_quantity.value = incrementAsStr;
}

// Computes the increments in quantity needed.
function compute(bundleCost: number): [number, number] {
  let x = 1;
  let increment = 100;

  if (bundleCost < increment) {
    x = gcd(bundleCost, increment);
  } else {
    x = gcd(increment, bundleCost);
  }

  return [increment / x, bundleCost / x];
}

function gcd(a: number, b: number): number {
  while (a != 0) {
    let oldA = a;

    a = b % a;
    b = oldA;
  }

  return b;
}
