
// A simple function to request that the browser is in
// the foreground before performing this request.
function requestForeground(callback) {
  requestAnimationFrame(() => {
    callback();
  });
}

export {
  requestForeground
}