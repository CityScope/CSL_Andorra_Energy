/**
* Name: SolarBlockChain
* Author: Patrick Taillandier, Luis Alonso,  Tri Nguyen Huu and Arnaud Grignard
* Description: 
* Tags: Tag1, Tag2, TagN
*/

model SocialInfluence

/* Insert your model definition here */

global{
	
	float buying_scale_factor <- 40000.0;
	float selling_scale_factor <- 25000.0;
	
	string cityScopeCity <-"Andorra" among: ["Taipei", "Shanghai", "Lyon_PlaceBellecour", "Andorra", "Hamburg", "Lima", "Rabat","Marrakech"];
	// GIS FILE //	
	file buildings_shapefile <- file("./../includes/City/"+cityScopeCity+"/Buildings.shp");
	file roads_shapefile <- file("./../includes/City/"+cityScopeCity+"/Roads.shp");
	geometry shape <- envelope(buildings_shapefile);
	int maxProd;
	int minProd;
	int maxCon;
	int minCon;
	map<string,int> class_map<- ["OL"::2, "OM"::3,  "OS"::3,  "RL"::4, "RM"::5,  "RS"::6, "PL"::7, "PM"::8,  "PS"::9];
	map<string,rgb> class_color_map<- ["OL"::rgb(12,30,51), "OM"::rgb(31,76,128),  "OS"::rgb(53,131,219),  "RL"::rgb(143,71,12), "RM"::rgb(219,146,25),  "RS"::rgb(219,198,53), "PL"::rgb(110,46,100), "PM"::rgb(127,53,116),  "PS"::rgb(179,75,163), "Park"::rgb(142,183,31)];
		
	float step <- 1#mn;
	date starting_date <- date([2019,1,1]);
		
	
	graph road_network;
	
	//parking parameters
	float PARKING_SPACING <- 5.0#m;
	float PARKING_WIDTH <- 2.0#m;
	float nb_change_per_hour <- 0.5;
	float change_probability <- step/1#hour*nb_change_per_hour;
	
	
	init{
		create road from: roads_shapefile{
			int segments_number <- length(shape.points)-1;
			loop i from: 0 to: segments_number-1{
				float segment_x <- shape.points[i+1].x - shape.points[i].x;
				float segment_y <- shape.points[i+1].y - shape.points[i].y;
				float segment_length <- sqrt(segment_x^2 + segment_y^2);
				parking_number <- max([0,int(segment_length/PARKING_SPACING)-1]);		
				
				float pk_angle <- acos(segment_x/segment_length);
		 		pk_angle <- segment_y<0 ? - pk_angle : pk_angle; 
		 		point shift <-  {0.8 * PARKING_WIDTH*cos(pk_angle+90), 0.8 * PARKING_WIDTH*sin(pk_angle+90)};
		 		loop j from:0 to: parking_number-1 step:1 {
			 		point parking_location <- {shape.points[i].x + segment_x/segment_length * (j+1) * PARKING_SPACING, shape.points[i].y + segment_y/segment_length * (j+1) * PARKING_SPACING};
					create parking{
						location <- parking_location+shift;
						angle <- pk_angle;
						capacity <- 1;
						occupancy <- rnd(1);
					}
					create parking{
						location <- parking_location-shift;
						angle <- pk_angle;
						capacity <- 1;
						occupancy <- rnd(1);
						add self to: myself.parking_list;
					}	
				}
				
			}
		}
		
		create pev number: 150;
		
		road_network <- as_edge_graph(road);
		create building from: buildings_shapefile with: 
		[usage::string(read ("Usage")),scale::string(read ("Scale")),category::string(read ("Category")), nbFloors::1+float(read ("Floors"))]{		
			area <-shape.area;
		}
		ask building  {
			create people number: 1  {
				my_home <- myself;
				working_place <- one_of(building where (each.category = "O"));
				location <- any_location_in(my_home);
				myself.inhabitants << self;
//				if flip(0.8){
//					has_car <- true;
//					parking_location <- parking where(each.occupancy < each.capacity) closest_to(self);
//					parking_location.occupancy <- parking_location.occupancy+1;
//				}
			}
		}
		
		write "There are "+length(people)+" people in "+cityScopeCity+" and "+length(parking)+" parking places.";
		

		
	}
	
	

		
	reflex simulation{
	
		ask building {
//			if not(self.category = "Park"){
//				do update_status ;
//			}
		}
		
		if (every(1#day)){
//		 	ask building {
//		 		do choose_produce_electricty;	
//		 	}
			
		}
		
		
	}	
}

species pev skills:[moving]{
	
	reflex wander {
		do wander on: road_network speed: speed;	
	}

	aspect default {
		draw triangle(3.0) color: #green; 
	}

}


species people skills:[moving]{
	building my_home;
	building working_place;
	float environmental_value <- rnd(1.0);
	float financial_value <- rnd(1.0);
	bool electric_car <- false;
	int working_hour <- rnd(7,10);
	int going_home_hour <- rnd(16,20);
//	bool commuting <- false;
	float pro_environment <- rnd(0.5) min:0.0 max: 1.0;
	//point target;
	//bool has_car <- false;
	parking parking_location <- nil;
	float speed <- 3#km/#hour;
//	bool change_mode <- false;
	bool use_car <- false;
	bool parking_car <- false;
	
	reflex wander when: not parking_car{
		do wander on: road_network speed: speed;	
	}
	
	reflex changing_mode when: flip(change_probability) {
		if use_car{
			parking_location <- (road(current_edge).parking_list where (each.occupancy < each.capacity)) closest_to self;
			if parking_location != nil{
				parking_location.occupancy <- parking_location.occupancy + 1;
				use_car <- false;
				speed <- 3#km/#hour;
			}
		}else{
			parking_location <- (road(current_edge).parking_list where (each.occupancy >= 1)) closest_to self;
			if parking_location != nil and parking_location distance_to self < 10#m{
				parking_location.occupancy <- parking_location.occupancy - 1;
				use_car <- true;
				speed <- 45#km/#hour;	
			}
		}
	}
	
	
	
//	reflex park_car when: parking_car{
//		do goto target: parking_location on: current_edge speed: 5#km/#hour;
//		if self distance_to parking_location < 1#m {
//			change_mode <- false;
//			parking_car <- false;
//			parking_location.occupancy <- parking_location.occupancy + 1;
//			use_car <- false;
//			speed <- 3#km/#hour;
//		}
//		
//	}
//	
//	reflex changing_mode when: change_mode and not parking_car{
//		if use_car{
//			parking_location <- (road(current_edge).parking_list where (each.occupancy < each.capacity)) closest_to self;
//			if parking_location != nil{//} and parking_location distance_to self < 10#m{
//				parking_car <- true;
//			}
//		}else{
//			parking_location <- (road(current_edge).parking_list where (each.occupancy >= 1)) closest_to self;
//			if parking_location != nil and parking_location distance_to self < 10#m{
//				change_mode <- false;
//				parking_location.occupancy <- parking_location.occupancy - 1;
//				use_car <- true;
//				speed <- 45#km/#hour;	
//			}
//		}
//	}
//	
//	reflex update_mode when: not change_mode and flip(change_probability){
//		change_mode <- true;
//		if use_car{
//			speed <- 20#km/#hour;
//		}
//	}
		
//	reflex move when: commuting{
//	do wander on: road_network;
//		if self distance_to target < 2#m{
//			commuting <- false;
//		}else{
//			do goto target: target on: road_network recompute_path: false;	
//		}

//	}
	
//	reflex go_to_work when:not commuting and (current_date.hour = working_hour) {
//		target <- any_location_in(working_place);
//		commuting <- true;
//	}	
//	
//	
//	reflex go_home when: not commuting and (current_date.hour = going_home_hour) {
//		target <- any_location_in(my_home);
//		commuting <- true;
//	}
	
	
	aspect default {
		float val <- 255 * pro_environment;
		if use_car{
			draw square(3.0) color: parking_car?#green:#red; //rgb(255 - val, val,0.0); 
		}else{
			draw circle(3.0) color: #blue; //rgb(255 - val, val,0.0);
		}
	}
}



species building {
	rgb color;
	float area;
	float production <-0.0;
	float consumption <-0.0;
	string usage; 
	string scale;
	float nbFloors;
	string category;
	list<people> inhabitants;


	

	

	
	aspect base {	
		if category="Park"{
			draw shape color: class_color_map["Park"];
		}
		else{
			draw shape color: class_color_map[usage+scale];
		}
	}
	
	
}

species road {
	int parking_number <- 0;
	list<parking> parking_list <- [];
	
	aspect default {
		draw shape color: #black;
	}
}

species parking{
	int capacity;
	int occupancy;
	float angle <- 0.0; // useful to keep parking place aligned with roads
	geometry shape <- rectangle(0.8*PARKING_SPACING,2);
	
	
	aspect {
		float level <- 150 - 120*occupancy/capacity;
		draw shape color: rgb(level, level, level) rotate: angle;
	}
	
}

experiment start type: gui {
	output {
		
		display view1  type:opengl  {
			species building aspect:base;	
			species parking;
			species road;
			species people;
			species pev;
		}
	//	display chartprod 
	//	{
//			chart 'prod' axes:rgb(125,125,125) size:{1.0,0.5} type:histogram style:stack //white
//			{
//				data 'production' value:sum(building collect each.production) accumulate_values:true color:rgb(169,25,37) marker:false thickness:2.0; //red
//				data 'consumption' value:-sum(building collect each.consumption)  accumulate_values:true color:rgb(71,168,243) marker:false thickness:2.0; //blue
//			}
			
			
	//	}
	}
}


//experiment multi_sim_city type: gui {
//	init {
//	
//		create simulation with: [cityScopeCity::"Lyon_PlaceBellecour"];
//		create simulation with: [cityScopeCity::"Taipei"];
//		create simulation with: [cityScopeCity::"Shanghai"];
//		
//	}
//	output {
//		
//		
//		display view1  type:opengl  {	
//			species building aspect:is_producer;	
//			species road;
//			species people;
//		}
//		
//	}
//}




