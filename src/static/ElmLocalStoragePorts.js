const subscribe = app => {
  const receivePort = app.ports["receiveItem"];

  app.ports["getItem"].subscribe(key => {
    let value;
    try {
      value = JSON.parse(localStorage.getItem(key));
    } catch (e) {
      value = null;
    }
    receivePort.send({ key, value });
  });

  app.ports["setItem"].subscribe(kv => {
    const key = kv[0];
    const json = kv[1];
    if (json === null) {
      localStorage.removeItem(key);
    } else {
      localStorage.setItem(key, JSON.stringify(json));
    }
  });
};

export default subscribe;
