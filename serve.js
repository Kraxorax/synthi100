const Bundler = require('parcel-bundler');
const express = require('express');
const proxy = require('http-proxy-middleware');

const app = express();

app.use('/api', proxy({
  target: 'http://localhost:8081/'
}));

app.use('/uploads', proxy({
  target: 'http://localhost:8081/'
}));

const bundler = new Bundler('web/index.html');
app.use(bundler.middleware());

app.listen(Number(process.env.PORT || 8082));
