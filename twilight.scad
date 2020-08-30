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

module rounded_cube(size, thick=thick0, walls=wall0, center=false) {
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
            if (rounded) rounded_cube(box, thick, wall);
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

sectormm = [106, 143, 62];
sectorin = [4+3/16, 5+5/8, 2+1/2]*inch;
// echo(sectormm, sectorin);

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

module flight(n=1, h=3, d=94, Dr=105, Rh=2, Xh=-6.5, center=false) {
    // TODO: model legs
    H = n*h;
    r = d/2;       // indiameter
    R = r/cos(30);  // circumdiameter
    Rr = Dr/2;     // circumdiameter after rounding
    Rc = (R-Rr)*sin(60)/(1-sin(60));  // corner radius
    Er = R - Rc/sin(60);  // edge after rounding
    translate([0, 0, center ? 0 : H/2]) difference() {
        hull() for (a=[0:120:359]) rotate(a) {
            // position the rounded corners to create the total Dr distance
            for (s=[1,-1]) translate([s*(Rr-Rc), 0, 0])
                cylinder(h=H, r=Rc, center=true);
            // connect the corners with solid sides
            rotate(30) cube([d, Er, H], center=true);
        }
        // subtract the leg holes
        for (a=[0:120:359]) rotate(a) {
            translate([Rr+Xh, 0, 0]) cylinder(h=2*H, r=Rh, center=true);
        }
    }
    // holes are 2mm radius, set 6.5mm from rounded corner
}

// index: 0 = base, 1 = expansion 1, etc.
deck_faction = [6, 6];
deck_public = [10, 10];
deck_secret = [20, 20];
deck_objective = 2*deck_public + deck_secret;
deck_action = [80, 20];
deck_agenda = [50, 13];
deck_planet = [32, 32];  // expansion planet cards TBD
deck_explore = [0, 84];
deck_common = deck_objective + deck_action + deck_agenda + deck_planet +
              deck_explore;
echo(deck_common);

function sum(v) = [for (p=v) 1] * v;

module deck(n=1, sleeve=double_sleeve, card=yellow_sleeve, center=false) {
    cards = is_list(n) ? sum(n) : n;
    v = vdeck(cards, sleeve, card);
    origin = [0, 0, center ? 0 : v[2]/2];
    translate(origin) cube(v, center=true);
    echo(cards, v);
}

%hex();
%flight();

deck(deck_objective);

*faction([75, 30, 98.5]);
*faction([75, 60, 98.5]);
*faction([75, 75, 98.5]);
