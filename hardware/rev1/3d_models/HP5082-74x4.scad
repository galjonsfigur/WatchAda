rotate([0,0,90])
translate([-6.35, -3.81 ,1.78/2])
union() {
    color("Red") {
    cube([15.37, 6.35, 1.78], center=true);
    translate([0, 0, 1.78])
    cube([15.3, 6.3, 2], center=true);
    intersection(){
    for(i = [0:3]) {
        translate([-15.37/2+1.92125+i*1.92125*2, 0, 2.03])
            color("Red") sphere(r=2.8, $fn=50);
    }
    cube([15.3, 6.3, 15], center=true);
    }
}
    translate([-5.08 - 2.54, 0, -0.9])
    for (i = [0:5]) {
    color("Silver") translate([i*2.54,-3.5,0]) {
    	translate([1.252 - 0.5, -0.45, 1.5])
    	    cube([1, 5, 0.3]);
    	translate([1.252-0.5, 2.55, 1.5])
    	    cube([1, 5, 0.3]);
    	translate([1.252-0.3, -0.45, -4.2])
            cube([0.6, 0.3, 6]);
    	translate([1.252 - 0.3, 7.25,-4.2])
            cube([0.6, 0.3, 6]);
    	translate([1.252 - 0.5, -0.45, -0.4])
            cube([1, 0.3, 2]);
    	translate([1.252 - 0.5, 7.25, -0.4])
            cube([1, 0.3, 2]);
    }
}
 
}