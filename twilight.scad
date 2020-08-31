layer_height = 0.2;
extrusion_width = 0.45;
extrusion_overlap = layer_height * (1 - PI/4);
extrusion_spacing = extrusion_width - extrusion_overlap;

// convert between path counts and spacing, qspace to quantize
function xspace(n=1) = n*extrusion_spacing;
function nspace(x=xspace()) = x/extrusion_spacing;
function qspace(x=xspace()) = xspace(round(nspace(x)));
function cspace(x=xspace()) = xspace(ceil(nspace(x)));
function fspace(x=xspace()) = xspace(floor(nspace(x)));

// convert between path counts and width, qspace to quantize
function xwall(n=1) = xspace(n) + (0<n ? extrusion_overlap : 0);
function nwall(x=xwall()) =  // first path gets full extrusion width
    x < 0 ? nspace(x) :
    x < extrusion_overlap ? 0 :
    nspace(x - extrusion_overlap);
function qwall(x=xwall()) = xwall(round(nwall(x)));
function cwall(x=xwall()) = xwall(ceil(nwall(x)));
function fwall(x=xwall()) = xwall(floor(nwall(x)));

// quantize thin walls only (less than n paths wide, default for 2 perimeters)
function qthin(x=xwall(), n=4.5) = x < xwall(n) ? qwall(x) : x;
function cthin(x=xwall(), n=4.5) = x < xwall(n) ? cwall(x) : x;
function fthin(x=xwall(), n=4.5) = x < xwall(n) ? fwall(x) : x;

tolerance = 0.001;
border = 1;

$fa = 15;
$fs = min(layer_height/2, xspace(1)/2);

inch = 25.4;
card = [2.5*inch, 3.5*inch];  // standard playing card dimensions

sand_sleeve = [81, 122];  // Dixit
orange_sleeve = [73, 122];  // Tarot
magenta_sleeve = [72, 112];  // Scythe
brown_sleeve = [67, 103];  // 7 Wonders
lime_sleeve = [82, 82];  // Big Square
blue_sleeve = [73, 73];  // Square
dark_blue_sleeve = [53, 53];  // Mini Square
gray_sleeve = [66, 91];  // Standard Card
purple_sleeve = [62, 94];  // Standard European
ruby_sleeve = [46, 71];  // Mini European
green_sleeve = [59, 91];  // Standard American
yellow_sleeve = [44, 67];  // Mini American
catan_sleeve = [56, 82];  // Catan (English)

no_sleeve = 0.35;  // common unsleeved card thickness (UG assumes 0.325)
thin_sleeve = 0.1;  // 50 micron sleeves
thick_sleeve = 0.2;  // 100 micron sleeves
double_sleeve = thick_sleeve + thin_sleeve;
function double_sleeve_count(d) = floor(d / (no_sleeve + double_sleeve));
function thick_sleeve_count(d) = floor(d / (no_sleeve + thick_sleeve));
function thin_sleeve_count(d) = floor(d / (no_sleeve + thin_sleeve));
function unsleeved_count(d) = floor(d / no_sleeve);
function vdeck(n=1, sleeve=double_sleeve, card=yellow_sleeve) =
    [card[0], card[1], n*(no_sleeve+sleeve)];

function unit_axis(n) = [for (i=[0:1:2]) i==n ? 1 : 0];

// common sleeve dimensions
FFG = [66.5, 0.2, 94];

wall0 = xwall(3);
gap0 = 0.1;
thick0 = 2*wall0 + gap0;
join0 = 10;
seam0 = 1/3;
rise0 = 1/3;

module beveler(size, flats=1, walls=1, center=false) {
    function unit_axis(n) = [for (i=[0:1:2]) i==n ? 1 : 0];
    module bevel(w, n=0) {
        a = unit_axis(n);
        v = [1, 1, 1] - a;
        aj = (n-1+3) % 3;
        ak = (n+1+3) % 3;
        for (j=[1,-1]) for (k=[1,-1]) {
            dj = j*unit_axis(aj)*size[aj]/2;
            dk = k*unit_axis(ak)*size[ak]/2;
            translate(origin+dj+dk)
                rotate(45*a) cube(a*(size[n]+10) + v*sqrt(2)*w, center=true);
        }
    }
    xy = sqrt(2) * flats;
    z = sqrt(2) * walls;
    origin = center ? [0, 0, 0] : size/2;
    bevel(flats, 0);
    bevel(flats, 1);
    bevel(walls, 2);
}

module beveled_cube(size, flats=1, walls=1, center=false) {
    difference() {
        cube(size, center);
        beveler(size=size, flats=flats, walls=walls, center=center);
    }
}

module rounded_cube(size, r=thick0, center=false) {
    origin = [0, 0, center ? 0 : size[2]/2];
    translate(origin) hull() {
        for (s0=[1,-1]) for (s1=[1,-1]) {
            translate([s0*(size[0]/2-r), s1*(size[1]/2-r), 0])
                cylinder(h=size[2], r=r, center=true);
        }
        cube([size[0]-2*r, size[1], size[2]], center=true);
        cube([size[0], size[1]-2*r, size[2]], center=true);
    }
}

module triple_rounded_cube(size, thick=thick0, walls=wall0, center=false) {
    R = max(thick, walls);
    B = min(thick, walls);
    L = B*sqrt(2)/2;
    S = R - sqrt(R*R - L*L);
    O = (R+L-S)*sqrt(2)/2;  // (R-S)*sqrt(2)/2;
    $fa = 6;
    module axis(x, y, z, a=[0, 0, 0]) {
        rotate(a)
        for (s0=[1,-1]) for (s1=[1,-1]) {
            // 1/2-step rotation aligns all the spheres & cylinders
            translate([s0*(x-O), s1*(y-O), 0])
                rotate($fa/2) cylinder(r=R, h=2*(z-O), center=true);
            if (a == [0, 0, 0])  // corners
                for (s2=[1,-1]) translate([s0*(x-O), s1*(y-O), s2*(z-O)])
                    rotate($fa/2) sphere(r=R);
        }
    }
    origin = center ? [0, 0, 0] : size/2;
    translate(origin) hull() intersection() {
        cube(size, center=true);
        union() {
            axis(size[0]/2, size[1]/2, size[2]/2);
            axis(size[2]/2, size[1]/2, size[0]/2, [0, 90, 0]);
            axis(size[0]/2, size[2]/2, size[1]/2, [90, 0, 0]);
        }
    }
}

// TODO: dividers
module faction(out=undef, in=undef, wall=wall0, gap=gap0, join=join0,
               seam=seam0, rise=rise0, rounded=true, lid=false, ghost=undef) {
    module side(w, d0, d1, h0, h1) {
        shape = [[0, 0], [0, h0+h1], [d0, h0+h1], [d0+d1, h0], [d0+d1, 0]];
        if (0<h1) rotate([90, 0, 90]) linear_extrude(w) polygon(shape);
        if (0<h0) cube([w, d0+d1+d0, h0]);
    }
    module snap(d, a=0) {
        jfit = join * cos(atan(z1/run));
        snaph = xspace(2);
        snapd = min(d, jfit-2*vgap-2*snaph);
        rotate([90, 0, a])
            cylinder(d1=snapd+2*snaph, d2=snapd, h=snaph, center=true);
    }
    module box(wall, join=0, inset=0) {
        space = join && inset ? vgap : 0;
        translate([inset, inset, 0])
            cube([box[0]-2*inset, wall, z0+z1+join-space]);  // back
        translate([inset, box[1]-wall-inset, 0])
            cube([box[0]-2*inset, wall, z0+join-space]);  // front
        difference() {
            for (x=[inset, box[0]-wall-inset]) translate([x, inset, 0])
                side(wall, thick-inset, run, z0+join-space, z1);
            if (join) {  // snap
                y = thick + 4/5*run;
                z = z0 + 1/5*z1 + join/2;
                translate([inset?inset:wall, y, z])
                    snap(join/4, inset?90:-90);
                translate([box[0]-(inset?inset:wall), y, z]) mirror([1, 0, 0])
                    snap(join/4, inset?90:-90);
            }
        }
        if (join) {  // snap
            y = thick + 1/5*run;
            z = z0 + 4/5*z1 + join/2;
            translate([inset?inset:wall, y, z]) mirror([1, 0, 0])
                snap(join/4, inset?90:-90);
            translate([box[0]-(inset?inset:wall), y, z])
                snap(join/4, inset?90:-90);
        }
    }

    vgap = max(gap, layer_height);
    flat = round(wall/layer_height) * layer_height;
    thick = 2*wall + gap;
    in0 = [thick, thick, flat];
    box = is_undef(out) ? in + 2*in0 : out;
    run = box[1]-2*thick;
    z1 = rise * (box[2] - join);
    z0 = (lid ? 1 - rise - seam : seam) * (box[2] - join);
    echo("exterior", box);
    echo("interior", box - 2*in0);
    echo("lid angle", z1, run, atan(z1/run));
    echo("double sleeve", double_sleeve_count(run));
    echo("thick sleeve", thick_sleeve_count(run));
    echo("thin sleeve", thin_sleeve_count(run));
    echo("unsleeved", unsleeved_count(run));

    // reference objects
    %if (ghost) translate([0, box[1], box[2]]) rotate([0, 180, 0])
        faction(out, in, wall, gap, join, seam, rise, lid=!lid, ghost=false);
    %if (ghost!=false) translate(in0) cube(box-2*in0);

    difference() {
        intersection() {
            if (lid) {
                cube([box[0], box[1], flat]);  // floor
                box(thick);  // wall
                box(wall, join);  // joint
            }
            else {
                inset = thick - wall;
                translate([inset, inset, 0])
                    cube([box[0]-2*inset, box[1]-2*inset, flat]);  // floor
                box(thick);  // wall
                box(wall, join, inset);  // joint
            }
            if (rounded) triple_rounded_cube(box, thick, wall);
            else beveled_cube(box, thick, xspace(2));
        }
        translate([box[0]/2, 0, (z0+z1+(lid?join:0))/2])
            rotate([90, lid?180:0, 0])
            for (d=[0,1])
                linear_extrude(xspace(2-d), center=true)
                    offset(r=d*layer_height) children();
    }
}

module set(out=undef, in=undef, wall=wall0, gap=gap0, join=join0,
           seam=seam0, rise=rise0, rounded=true, ghost=undef) {
    flat = round(wall/layer_height) * layer_height;
    thick = 2*wall + gap;
    in0 = [thick, thick, flat];
    box = is_undef(out) ? in + 2*in0 : out;
    echo(box);
    echo(box-2*in0);
    translate([box[0]+10, 0, 0])
        faction(out=out, in=in, wall=wall, gap=gap, join=join,
                seam=seam, rise=rise, rounded=rounded, ghost=ghost) children();
    faction(out=out, in=in, wall=wall, gap=gap, join=join, seam=seam,
            rise=rise, rounded=rounded, lid=true, ghost=ghost) children();
}

module bevel_test(out, wall=wall0, gap=gap0, rounded=true) {
    box = [out[0], out[1], 2*out[2]];
    flat = round(wall/layer_height) * layer_height;
    difference() {
        faction(out=box, wall=wall, gap=gap, join=out[2]-flat,
                rounded=rounded, lid=true, ghost=false);
        translate([-1/2, -1/2, out[2]]) cube(box+[1,1,1]);
    }
}


// box dimensions
boxlid = [442, 300, 136];
boxbase = [434, 292, 133];
interior = [427, 287, 130];
sector = [(interior[0]-3)/4, (interior[1]-5)/6, (interior[2]-10)/2];

// hex dimensions
stock = 1.75;  // tile stock thickness
Dhex = 116;  // long diameter across hex
dhex = 101;  // short diameter across hex
hex = [Dhex, dhex, stock];  // bounding box of hex tile

module hex(n=1, h=stock, D=undef, d=undef, center=false) {
    // TODO: make it easier to select minimum bounds (D=116)
    Dx = is_undef(D) ? Dhex : D;  // 116 default
    dx = is_undef(d) ? dhex : d;  // 101 default
    R = max(Dx/2, dx/cos(30)/2);
    r = max(Dx*cos(30)/2, dx/2);
    linear_extrude(n*h, center=center)
        polygon([[R, 0], [R/2, r], [-R/2, r], [-R, 0], [-R/2, -r], [R/2, -r]]);
}

leg_head = [10.2, 1.5];
leg_post = [4.9, 38];
leg_washer = [8, 1];
leg_thread = 4;
leg_rise = leg_head[1] + leg_post[1] + leg_washer[1];
leg_height = 2*leg_head[1] + leg_post[1] + leg_washer[1];
table_size = [105, 94, 3];
stand_height = leg_height + table_size[2];

module leg(gap=gap0, center=false) {
    post = leg_post;
    head = leg_head;
    washer = leg_washer;
    thread = [leg_thread, gap];
    h = 2*head[1] + post[1] + thread[1] + washer[1];
    origin = [0, 0, center ? 0 : h/2];
    translate(origin) {
        color("dimgray") {
            cylinder(h=h, d=thread[0], center=true);
            translate([0, 0, (post[1]+head[1]+washer[1]-h)/2])
                cylinder(h=post[1]+head[1]+washer[1], d=post[0], center=true);
            for (s=[1,-1]) translate([0, 0, s*(h-head[1])/2])
                cylinder(h=head[1], d=head[0], center=true);
        }
        translate([0, 0, (h-washer[1])/2-head[1]-thread[1]])
            color("white") cylinder(h=washer[1], d=washer[0], center=true);
    }
}

module rounded_hex(h=table_size[2], d=table_size[1], Dr=table_size[0],
                   center=false) {
    r = d/2;       // indiameter
    R = r/cos(30);  // circumdiameter
    Rr = Dr/2;     // circumdiameter after rounding
    Rc = (R-Rr)*sin(60)/(1-sin(60));  // corner radius
    Er = R - Rc/sin(60);  // edge after rounding
    origin = [0, 0, center ? 0 : h/2];
    translate(origin) hull() for (a=[0:120:359]) rotate(a) {
        // position the rounded corners to create the total Dr distance
        for (s=[1,-1]) translate([s*(Rr-Rc), 0, 0])
            cylinder(h=h, r=Rc, center=true);
        // connect the corners with solid sides
        rotate(30) cube([d, Er, h], center=true);
    }
}

module stand(n=1, h=table_size[2], d=table_size[1], Dr=table_size[0],
             Rl=leg_thread/2, Xl=6.5, legs=false, center=false) {
    H = n*h;
    origin = [0, 0, center ? 0 : (H + (legs? leg_height : 0)) / 2];
    rise = legs ? leg_rise - leg_height/2 : 0;
    translate(origin) {
        translate([0, 0, rise]) color("aquamarine", 1/4) difference() {
            rounded_hex(h=H, d=d, Dr=Dr, center=true);
            // subtract the leg holes
            // holes are 2mm radius, set 6.5mm from rounded corner
            for (a=[0:120:359]) rotate(a) {
                translate([Dr/2-Xl, 0, 0]) {
                    cylinder(h=2*H, r=Rl, center=true);
                }
            }
        }
        // draw legs
        if (legs) for (a=[0:120:359]) rotate(a) {
            translate([Dr/2-Xl, 0, 0]) leg(gap=H, center=true);
        }
    }
}

// index: 0 = base, 1 = expansion 1, etc.
deck_faction = [6, 6];
deck_public = [10, 10];
deck_secret = [20, 20];
deck_objective = 2*deck_public + deck_secret;
deck_action = [80, 20];
deck_agenda = [50, 13];
deck_planet = [32, 22];  // expansion planet cards TBD, assume 1/tile
deck_explore = [0, 84];
deck_common = deck_objective + deck_action + deck_agenda + deck_planet +
              deck_explore;

function sum(v) = [for (p=v) 1] * v;

module deck(n=1, sleeve=double_sleeve, card=yellow_sleeve, center=false) {
    cards = is_list(n) ? sum(n) : n;
    v = vdeck(cards, sleeve, card);
    origin = [0, 0, center ? 0 : v[2]/2];
    translate(origin) cube(v, center=true);
    echo(cards, v);
}

tiles = 32 + 22 + 9 + 1;
flights = 16;  // 8 fit in player boxes
*hex(tiles);
*translate([0, 0, tiles*stock]) stand(flights);

module boxbase(center=false) {
    origin = [0, 0, center ? 0 : boxbase[2]/2];
    translate(origin) difference() {
        cube(boxbase, center=true);
        translate([0, 0, boxbase[2]-interior[2]])
            cube([interior[0], interior[1], boxbase[2]], center=true);
    }
}

module theater(center=false) {
    origin = [0, 0, center ? 0 : interior[2]/2];
    color("navy", 0.5) translate(origin) difference() {
        cube(boxlid, center=true);
        cube([interior[0], interior[1], interior[2]], center=true);
        for (x=[-1.5:1:1.5]) for (y=[-2.5:1:2.5])
            translate([x*(sector[0]+1), y*(sector[1]+1), 0]) {
            cube([sector[0], sector[1], 2*boxlid[2]], center=true);
            cube([sector[0], 2*boxlid[1], interior[2]], center=true);
            cube([2*boxlid[0], sector[1], interior[2]], center=true);
        }
    }
}

*rotate([90, 0, 90]) translate([0, interior[1]/2, (interior[2]-boxbase[2])/2])
    boxbase(center=true);

zz = tiles*stock + flights*3;
*translate([-65, 25, zz+34.5]) rotate([90, 0, 90])
    deck(deck_action/2);
*translate([-65, -25, zz+34.5]) rotate([90, 0, 90])
    deck(deck_action/2);
*translate([-65+33.5, 25, zz+34.5]) rotate([90, 0, 90])
    deck(deck_secret);
*translate([-65+33.5, -25, zz+34.5]) rotate([90, 0, 90])
    deck(2*deck_public);
*translate([-65+33.5+27, 25, zz+34.5]) rotate([90, 0, 90])
    deck(deck_explore/4);
*translate([-65+33.5+27, -25, zz+34.5]) rotate([90, 0, 90])
    deck(deck_explore/4);
*translate([-65+33.5+27+14.65, 25, zz+34.5]) rotate([90, 0, 90])
    deck(deck_explore/4);
*translate([-65+33.5+27+14.65, -25, zz+34.5]) rotate([90, 0, 90])
    deck(deck_explore/4);
*translate([-65+33.5, 25, zz+1+68]) rotate(90) deck(deck_agenda);
*translate([-65+33.5, -25, zz+1+68]) rotate(90) deck(deck_planet);
*rotate(90) deck(deck_common);

*faction([75, 30, 98.5]);
*faction([75, 60, 98.5]);
*faction([75, 75, 98.5]);

*for (x=[-1.5:1:1.5]) for (y=[-1:1:1]) for (z=[0:1:1])
    translate([x*(sector[0]+1), y*(sector[1]+1), z*stand_height])
        rotate(z*180) stand(legs=true);

module leg_stack(w=3, h=1, gap=gap0, center=false) {
    dr = leg_head[0] - leg_post[0]/2 + gap;
    dy = leg_head[1] + leg_washer[1] + gap;
    dW = dr*(w-1);
    dH = dr*(h-1);
    W = leg_head[0] + dW;
    H = leg_head[0] + dH;
    D = leg_height + gap + dy;
    echo("leg stack", w, h, [W, D, H]);

    origin = [-dW/2, 0, center ? -dH/2 : leg_head[0]/2];
    translate(origin) for (x=[0:1:w-1]) for (z=[0:1:h-1]) {
        flip = (x+z) % 2;
        translate([dr*x, dy*(flip-0.5), dr*z]) rotate([90-180*flip, 0, 0])
            leg(gap=gap, center=true);
    }
}

module stand_tray(n=8, wall=wall0, thick=thick0, gap=gap0, preview=true,
                  center=false) {
    flat = round(wall/layer_height) * layer_height;
    xflat = round(thick/layer_height) * layer_height;
    echo(flat, xflat);
    size = [sector[0], 3*sector[1]+2, flat+xflat+n*table_size[2]];
    w = size[0];
    d = size[1];
    h = size[2];
    echo("stand tray", size);
    origin = [0, 0, center ? -h/2: 0];
    translate(origin) {
        // tray
        // TODO: do something interesting with hex corners
        color("slategray") difference() {
            translate([0, 0, h/2]) rounded_cube(size, center=true);
            dhex = 2*sector[1]+1;
            Dhex = dhex/cos(30);
            translate([0, -(sector[1]+1)/2, xflat+h/2]) {  // thicker
                hex(h=h, d=dhex, D=Dhex, center=true);
                translate([0, 0, h/2]) {
                    // align & round tile cutouts
                    cut = 2*thick;
                    cut30 = 2*(cut+(Dhex-w)/2)*cos(30)/sin(30);
                    cut60 = 2*cut*cos(60)/sin(60);
                    rotate([0, 90, 0])
                        rounded_cube([2*h, cut30, 2*w], cut30/3, center=true);
                    translate([0, thick/2-dhex/2, 0]) rotate([90, 0, 0])
                        rounded_cube([Dhex/2+cut60, 2*h, 2*cut],
                                     cut60/2, center=true);
                    // finger holds
                    translate([0, 0, h/2-xflat]) for (s=[1,-1]) {
                        x0 = s*(w/2-thick);
                        y0p = dhex/2;
                        y0n = dhex/2-thick;
                        y1 = cut30/2+thick;
                        x1 = s*(Dhex/2+thick/cos(30)-y1*tan(30));
                        x2p = x1-s*(y0p-y1)*tan(30);
                        x2n = s*(Dhex/4+cut60/2+thick);
                        y1n = y1-s*(x2n-x1)*tan(60);
                        linear_extrude(2*h, center=true) {
                            polygon([[x0,y0p], [x0,y1], [x1,y1], [x2p,y0p]]);
                            polygon([[x0,-y0n], [x0,-y1], [x1,-y1], [x2n,-y1n],
                                     [x2n,-y0n]]);
                        }
                    }
                }
            }
            // scoop inside corners
            vlegs = [sector[0]-2*thick, sector[1]+1-2*thick, h];
            scoop = leg_head[0]/2;
            translate([0, sector[1]+1/2, h+flat]) rotate([0, 90, 0])
                rounded_cube([2*vlegs[2], vlegs[1], vlegs[0]], scoop,
                             center=true);
            echo("leg compartment", vlegs);
        }
        // contents
        if (preview) {
            translate([0, -(sector[1]+1)/2, xflat])
                stand(n, center=false);
            for (s=[1,-1])
                translate([s*(sector[0]-leg_head[0])/4, sector[1]+1/2, flat])
                rotate(90) leg_stack(w=5, h=ceil(3*n/10), center=false);
        }
    }
}

// TODO: module to arrange stuff more easily?

*theater(center=false);
for (s=[1,-1]) translate([0.5*(sector[0]+1), s*1.5*(sector[1]+1), 0])
    rotate(90+s*90) stand_tray(preview=true);

*stand_tray(preview=false);
