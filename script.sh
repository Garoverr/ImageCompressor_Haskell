#!/bin/bash

# stop the scrypt if an error occurs
set -e

stack build

make

echo "Installing react dependencies..."
npm install

echo "Installing dependencies for the server in Node.js..."
npm install express multer cors

node src/server.js &

npm start
