class Barrier {
    private radius: number;
    private x: number;
    private y: number;

    public constructor(radius: number, x: number, y: number) {
        this.radius = radius;
        this.x = x;
        this.y = y;
    }

    public getRadius(): number {
        return this.radius;
    }

    public getX(): number {
        return this.x;
    }

    public getY(): number {
        return this.y;
    }
}

class Particle {
    private radius: number;
    private color: number;
    private posX: number;
    private posY: number;
    private oldX: number;
    private oldY: number;

    public constructor(radius: number, color: number, x: number, y: number, vx: number, vy: number) {
        this.radius = radius;
        this.color = color;
        this.posX = x;
        this.posY = y;
        this.oldX = x - vx;
        this.oldY = y - vy;
    }

    public getRadius(): number {
        return this.radius;
    }

    public getX(): number {
        return this.posX;
    }

    public getY(): number {
        return this.posY;
    }

    public getColor(): number {
        return this.color;
    }

    public step(env:Environment, barriers:Array<Barrier>): void {
        this.verlet(env);
        var length: number = barriers.length;
        for (var i: number = 0; i < length; ++i) {
            this.barrierCollide(barriers[i]);
        }
    }

    private verlet(env:Environment) {
        var newX: number = (((env.dampingA1 * this.posX) / env.dampingA2)|0)  -  (((env.dampingB1 * this.oldX) / env.dampingB2)|0);
        var newY: number = (((env.dampingA1 * this.posY) / env.dampingA2)|0)  -  (((env.dampingB1 * this.oldY) / env.dampingB2)|0) + env.gravity;
        this.oldX = this.posX;
        this.oldY = this.posY;
        this.posX = newX;
        this.posY = newY;
    }

    private barrierCollide(b:Barrier) {
        var dx:number = this.posX - b.getX();
        var dy:number = this.posY - b.getY();
        var distanceSquared:number = (dx * dx) + (dy * dy);
        var minDistance:number = b.getRadius() + this.radius;
        if (distanceSquared < minDistance * minDistance) {
            var distance:number = Math.sqrt(distanceSquared);
            this.posX = b.getX() + (((dx * minDistance) / distance)|0)
            this.posY = b.getY() + (((dy * minDistance) / distance)|0)
        }
    }
}

class Environment {
    public width: number;
    public height: number;
    public gravity: number;
    public dampingA1: number;
    public dampingA2: number;
    public dampingB1: number;
    public dampingB2: number;
}

class PseudoRandom {
    private x: number;
    private y: number;
    private z: number;
    private w: number;

    public constructor() {
        this.x = 123456789;
        this.y = 362436069;
        this.z = 521288629;
        this.w = 88675123;
    }

    public next(): number {
        var t: number = this.x ^ (this.x << 11);
        var x2: number = this.y;
        var y2: number = this.z;
        var z2: number = this.w;
        var w2: number = this.w ^ (this.w >> 19) ^ (t ^ (t >> 8));

        this.x = x2;
        this.y = y2;
        this.z = z2;
        this.w = w2;

        return w2;
    }

    nextRange(low: number, high: number): number {
        var r: number = this.next();
        return low + (r % (high - low));
    }
}

class World {
    private env: Environment;
    private particles: Array<Particle>;
    private barriers: Array<Barrier>;

    public constructor(numBarriers:number, numParticles:number) {
        var w:number = 640000;
        var h:number = 480000;

        this.env = new Environment();
        this.env.width = w;
        this.env.height = h;
        this.env.gravity = 80;
        this.env.dampingA1 = 199;
        this.env.dampingA2 = 100;
        this.env.dampingB1 = 99;
        this.env.dampingB2 = 100;

        var rg:PseudoRandom = new PseudoRandom();

        this.barriers = new Array<Barrier>();
        for (var i:number = 0; i < numBarriers; ++i) {
            this.barriers.push(randomBarrier(rg, w, h));
        }

        this.particles = new Array<Particle>();
        for (var i:number = 0; i < numParticles; ++i) {
            this.particles.push(randomParticle(rg));
        }
    }

    public getParticles(): Array<Particle> {
        return this.particles;
    }

    public getBarriers(): Array<Barrier> {
        return this.barriers;
    }

    public step(): void {
        var length: number = this.particles.length;
        for (var i: number = 0; i < length; ++i) {
            this.particles[i].step(this.env, this.barriers);
        }
    }
}

function randomParticle(rg:PseudoRandom) {
    var r: number = rg.nextRange(2000, 5000);
    var c: number = rg.nextRange(128, 255);
    var x: number = rg.nextRange(0, 640000);
    var y: number = rg.nextRange(0, 100000);
    var vx: number = rg.nextRange(-2000, 2000);
    var vy: number = rg.nextRange(-2000, 0);
    return new Particle(r, c, x, y, vx, vy);
}

function randomBarrier(rg:PseudoRandom, w:number, h:number) {
    var r: number = rg.nextRange(10000, 50000);
    var x: number = rg.nextRange(0, w);
    var y: number = rg.nextRange(100000, h);
    return new Barrier(r, x, y);
}

declare var global_num_barriers:number;
declare var global_num_particles:number;
declare var global_num_frames:number;

function main() {
    var canvas = document.createElement('canvas');
    canvas.width = 640;
    canvas.height = 480;
    document.body.appendChild(canvas);

    var ctx = canvas.getContext('2d');

    var world: World = new World(global_num_barriers, global_num_particles);

    var startTime = new Date().getTime();

    var framesLeft:number = global_num_frames;

    function animate() {
        renderWorld(ctx, world);
        world.step();

        framesLeft--;
        if (framesLeft < 0) {
            var endTime:number = new Date().getTime();
            alert('' + (endTime - startTime));
        } else {
            window.requestAnimationFrame(animate);
        }
    }

    animate();
}

function renderWorld(ctx:CanvasRenderingContext2D, world:World) {
    function renderParticle(p:Particle) {
        ctx.beginPath();
        ctx.arc(p.getX() / 1000, p.getY() / 1000, p.getRadius() / 1000, 0, 2 * Math.PI, false);
        ctx.fillStyle = 'rgba(' + 0 + ',' + p.getColor() + ',' + 0 + ',' + 1 + ')';
        ctx.fill();
    }

    function renderBarrier(b:Barrier) {
        ctx.beginPath();
        ctx.arc(b.getX() / 1000, b.getY() / 1000, b.getRadius() / 1000, 0, 2 * Math.PI, false);
        ctx.fillStyle = 'rgba(' + 128 + ',' + 128 + ',' + 128 + ',' + 1 + ')';
        ctx.fill();
    }

    ctx.clearRect(0, 0, 640, 480);

    world.getBarriers().forEach(renderBarrier);
    world.getParticles().forEach(renderParticle);
}
