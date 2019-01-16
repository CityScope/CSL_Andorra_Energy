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
	
	string cityScopeCity <-"Andorra" among: ["Taipei", "Shanghai", "Lyon_PlaceBellecour", "Andorra"];
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
		
	
	float step <- 1#hour;
	date starting_date <- date([2019,1,1]);
		
	
	graph road_network;
	
	//parking parameters
	float PARKING_SPACING <- 5.0#m;
	float PARKING_WIDTH <- 2.0#m;

	
	
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
					}				
				}
				
			}
		}
		
		
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




species people {
	building my_home;
	building working_place;
	float environmental_value <- rnd(1.0);
	float financial_value <- rnd(1.0);
	bool electric_car <- false;
	int working_hour <- rnd(7,10);
	int going_home_hour <- rnd(16,20);
	bool goto_work <- false;
	float pro_environment <- rnd(0.5) min:0.0 max: 1.0;
	
	
	reflex go_to_work when:not goto_work and (current_date.hour = working_hour) {
		goto_work <- true;
		location <- any_location_in(working_place);
	}	
	
	
	reflex go_to_home when: goto_work and (current_date.hour = going_home_hour) {
		goto_work <- false;
		location <- any_location_in(my_home);
	}
	
	
	aspect default {
		float val <- 255 * pro_environment;
		draw circle(5.0) color: rgb(255 - val, val,0.0) border: #black;
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
			species road;
			species people;
			species parking;
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




