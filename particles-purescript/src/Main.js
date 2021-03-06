/* global exports, console */
"use strict";

// module Main

exports.alert = function (s) {
  return function () {
    alert('' + s);
    return {};
  };
};

exports.createCanvas = function(width) {
  return function(height) {
    return function() {
      var canvas = document.createElement("canvas");
      canvas.width = width;
      canvas.height = height;
      return canvas;
    };
  };
}

exports.addCanvasToBody = function(canvas) {
  return function() {
    document.body.appendChild(canvas);
    return {};
  };
}

exports.js_now = function() {
  return new Date().getTime();
}

exports.js_alertNumber = function(num) {
  return function() {
    alert('Measured milliseconds: ' + num);
  }
}

exports.js_requestAnimationFrame = function(action) {
  return function() {
    window.requestAnimationFrame(action);
  };
}

exports.js_getNumBarriers = function() {
  return global_num_barriers;
}

exports.js_getNumParticles = function() {
  return global_num_particles;
}

exports.js_getNumFrames = function() {
  return global_num_frames;
}
