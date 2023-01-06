const path = require('path');

module.exports = {
  entry: {
    app: [
      './index.js',
    ],
  },

  output: {
    path: path.resolve(`${__dirname}/public`),
    filename: '[name].js',
  },

  module: {
    loaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: 'babel-loader',
        query: {
          presets: ['es2015'],
        },
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]',
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader?debug=false',
      }
    ],

    noParse: /\.elm$/,
  },
};
