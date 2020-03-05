translate([-1.7, 0, 0])
union() {
    color("Black") intersection(){
        translate([-11.895, -7.8]) cube([23.79, 15.6, 4.8]);
        difference() {
            union() {           
                cylinder(h=4.8, d=22);
                translate([8, -6.5, 0]) cube([4.79, 13, 4.8]);
            }
                translate([0, 0, 1]) cylinder(h=3.8, d=20);    
        }
    }
    color("Gold") {
        translate([-10.5, -0.9, 0]) cube([4, 1.8, 0.1]);
     translate([22.79/2, -1.4, 0]) cube([2, 2.8, 0.1]);   
    }
}