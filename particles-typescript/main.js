var Barrier = (function () {
    function Barrier(radius, x, y) {
        this.radius = radius;
        this.x = x;
        this.y = y;
    }
    Barrier.prototype.getRadius = function () {
        return this.radius;
    };
    Barrier.prototype.getX = function () {
        return this.x;
    };
    Barrier.prototype.getY = function () {
        return this.y;
    };
    return Barrier;
})();
var Particle = (function () {
    function Particle(radius, color, x, y, vx, vy) {
        this.radius = radius;
        this.color = color;
        this.posX = x;
        this.posY = y;
        this.oldX = x - vx;
        this.oldY = y - vy;
    }
    Particle.prototype.getRadius = function () {
        return this.radius;
    };
    Particle.prototype.getX = function () {
        return this.posX;
    };
    Particle.prototype.getY = function () {
        return this.posY;
    };
    Particle.prototype.getColor = function () {
        return this.color;
    };
    Particle.prototype.step = function (env, barriers) {
        this.verlet(env);
        var length = barriers.length;
        for (var i = 0; i < length; ++i) {
            this.barrierCollide(barriers[i]);
        }
    };
    Particle.prototype.verlet = function (env) {
        var newX = (((env.dampingA1 * this.posX) / env.dampingA2) | 0) - (((env.dampingB1 * this.oldX) / env.dampingB2) | 0);
        var newY = (((env.dampingA1 * this.posY) / env.dampingA2) | 0) - (((env.dampingB1 * this.oldY) / env.dampingB2) | 0) + env.gravity;
        this.oldX = this.posX;
        this.oldY = this.posY;
        this.posX = newX;
        this.posY = newY;
    };
    Particle.prototype.barrierCollide = function (b) {
        var dx = this.posX - b.getX();
        var dy = this.posY - b.getY();
        var distanceSquared = (dx * dx) + (dy * dy);
        var minDistance = b.getRadius() + this.radius;
        if (distanceSquared < minDistance * minDistance) {
            var distance = Math.sqrt(distanceSquared);
            this.posX = b.getX() + (((dx * minDistance) / distance) | 0);
            this.posY = b.getY() + (((dy * minDistance) / distance) | 0);
        }
    };
    return Particle;
})();
var Environment = (function () {
    function Environment() {
    }
    return Environment;
})();
var PseudoRandom = (function () {
    function PseudoRandom() {
        this.x = 123456789;
        this.y = 362436069;
        this.z = 521288629;
        this.w = 88675123;
    }
    PseudoRandom.prototype.next = function () {
        var t = this.x ^ (this.x << 11);
        var x2 = this.y;
        var y2 = this.z;
        var z2 = this.w;
        var w2 = this.w ^ (this.w >> 19) ^ (t ^ (t >> 8));
        this.x = x2;
        this.y = y2;
        this.z = z2;
        this.w = w2;
        return w2;
    };
    PseudoRandom.prototype.nextRange = function (low, high) {
        var r = this.next();
        return low + (r % (high - low));
    };
    return PseudoRandom;
})();
var World = (function () {
    function World(numBarriers, numParticles) {
        var w = 640000;
        var h = 480000;
        this.env = new Environment();
        this.env.width = w;
        this.env.height = h;
        this.env.gravity = 80;
        this.env.dampingA1 = 199;
        this.env.dampingA2 = 100;
        this.env.dampingB1 = 99;
        this.env.dampingB2 = 100;
        var rg = new PseudoRandom();
        this.barriers = new Array();
        for (var i = 0; i < numBarriers; ++i) {
            this.barriers.push(randomBarrier(rg, w, h));
        }
        this.particles = new Array();
        for (var i = 0; i < numParticles; ++i) {
            this.particles.push(randomParticle(rg));
        }
    }
    World.prototype.getParticles = function () {
        return this.particles;
    };
    World.prototype.getBarriers = function () {
        return this.barriers;
    };
    World.prototype.step = function () {
        var length = this.particles.length;
        for (var i = 0; i < length; ++i) {
            this.particles[i].step(this.env, this.barriers);
        }
    };
    return World;
})();
function randomParticle(rg) {
    var r = rg.nextRange(2000, 5000);
    var c = rg.nextRange(128, 255);
    var x = rg.nextRange(0, 640000);
    var y = rg.nextRange(0, 100000);
    var vx = rg.nextRange(-2000, 2000);
    var vy = rg.nextRange(-2000, 0);
    return new Particle(r, c, x, y, vx, vy);
}
function randomBarrier(rg, w, h) {
    var r = rg.nextRange(10000, 50000);
    var x = rg.nextRange(0, w);
    var y = rg.nextRange(100000, h);
    return new Barrier(r, x, y);
}
function main() {
    var canvas = document.createElement('canvas');
    canvas.width = 640;
    canvas.height = 480;
    document.body.appendChild(canvas);
    var ctx = canvas.getContext('2d');
    var world = new World(global_num_barriers, global_num_particles);
    var startTime = new Date().getTime();
    var framesLeft = global_num_frames;
    function animate() {
        renderWorld(ctx, world);
        world.step();
        framesLeft--;
        if (framesLeft < 0) {
            var endTime = new Date().getTime();
            alert('Measured milliseconds: ' + (endTime - startTime));
        }
        else {
            window.requestAnimationFrame(animate);
        }
    }
    animate();
}
function renderWorld(ctx, world) {
    function renderParticle(p) {
        ctx.beginPath();
        ctx.arc(p.getX() / 1000, p.getY() / 1000, p.getRadius() / 1000, 0, 2 * Math.PI, false);
        ctx.fillStyle = 'rgba(' + 0 + ',' + p.getColor() + ',' + 0 + ',' + 1 + ')';
        ctx.fill();
    }
    function renderBarrier(b) {
        ctx.beginPath();
        ctx.arc(b.getX() / 1000, b.getY() / 1000, b.getRadius() / 1000, 0, 2 * Math.PI, false);
        ctx.fillStyle = 'rgba(' + 128 + ',' + 128 + ',' + 128 + ',' + 1 + ')';
        ctx.fill();
    }
    ctx.clearRect(0, 0, 640, 480);
    world.getBarriers().forEach(renderBarrier);
    world.getParticles().forEach(renderParticle);
}
// Needed for Closure Compiler
window["main"] = main;
