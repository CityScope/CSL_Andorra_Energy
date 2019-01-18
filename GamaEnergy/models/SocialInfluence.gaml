/**
* Name: SolarBlockChain
* Author: Arnaud Grignard, Tri Nguyen , Patrick Taillandier, Luis Alonso  
* Description: Started in Andorra in the 2019 Workshop. 
* From the SolarBlockChain model a reinforcement learning strategy has been explored introducing people and governement species
* Patrick and Pev have also been introduced 
*
* Tags: Tag1, Tag2, TagN
*/

model SocialInfluence

/* Insert your model definition here */

global{
	
	list<list<float>> heat_map <- [[30,146,254],[86,149,242],[144,201,254],[180,231,252],[223,255,216],[254,255,113],[248,209,69],[243,129,40],[235,46,26],[109,23,8]]; 
	
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
	map<string,int> energy_price_map<- ["OL"::10, "OM"::7,  "OS"::6,  "RL"::3, "RM"::2,  "RS"::1];
	matrix consumption_matrix ;
	map<string,rgb> class_color_map<- ["OL"::rgb(12,30,51), "OM"::rgb(31,76,128),  "OS"::rgb(53,131,219),  "RL"::rgb(143,71,12), "RM"::rgb(219,146,25),  "RS"::rgb(219,198,53), "PL"::rgb(110,46,100), "PM"::rgb(127,53,116),  "PS"::rgb(179,75,163), "Park"::rgb(142,183,31)];
		
	file consumption_csv_file <- csv_file("./../includes/Energy/171203_Energy_Consumption_CSV.csv",",");
	file production_csv_file <- csv_file("./../includes/Energy/171203_Energy_Production_CSV.csv",",");
	float average_surface;
	float max_surface;
	float max_energy;
	matrix production_matrix;
	float max_produce_energy;
	
	float step <- 1#mn;
	date starting_date <- date([2019,1,1]);
	
	float influence_factor <- 0.1;
	
	bool governmentAction parameter: "Government Action" category: "Government" <- false;
    string P_strategy parameter:"Intervention type" category: "Government" <- "random"  among: ["random", "pro", "cons"];
	int intervention_frequency parameter: "Intervention frequeny" category: "Government" min:1 max:100 <- 50;
	int nbPromotion parameter: "Number of people prmoted" category: "Government" min:1 max:1000 <- 100;
	bool teleTransportation  <- false;
	float interactionDistance parameter: "interaction Distance" min:1.0 max:100.0 <- 20.0;
	bool heatmap parameter: "Show HeatMap" category:"Visualization"  <- false;
	
	list<float> histo;
	int histo_cat_number <- 10;
	
	graph road_network;
	
	//parking parameters
	float PARKING_SPACING <- 8.0#m;
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
				int parking_number <- max([0,int(segment_length/PARKING_SPACING)-1]);		
				
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

	   		price <-float(energy_price_map[usage+scale]);			
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
		
		max_surface <-max (building collect (each.area));
		average_surface<-mean (building collect (each.area));
	
		consumption_matrix <- matrix(consumption_csv_file); 
		max_energy <- float (max (consumption_matrix))*max_surface;
		write 'Max consumed energy: '+max_energy;	
	
		production_matrix <- matrix(production_csv_file); 
		max_produce_energy <- float (max (production_matrix))*max_surface;
		write 'Max produced energy: '+max_produce_energy;	
		
		ask building {
		 		do choose_produce_electricty;	
		 	}
		 	
		 create gouvernment with: [strategy::P_strategy];
	}
	
	
	user_command promote_environment {
			ask gouvernment {
				do promote_environment;
			}
		}
		
	reflex reinit_pollution when:heatmap{
		ask cell {
			level <- 0.0;
		}
	}

		
	reflex simulation{
		if (governmentAction and cycle mod intervention_frequency = 0){
			ask gouvernment {
				do promote_environment;
			}
		}
	
		ask building {
			if not(self.category = "Park"){
				do calculate_consumption;
				do calculate_production ;
				do update_status ;
			}
		}
		
		if (every(1#hour)){
		 	ask building {
		 		do choose_produce_electricty;	
		 	}	
		}
		
		
	}
	
	reflex compute_histogram{
		histo <- list_with(histo_cat_number,0.0);
		loop tmp over: people{
			int cat <- min([int(histo_cat_number*tmp.pro_environment), histo_cat_number-1]);
			histo[cat] <- histo[cat]+1; 
		}
		loop i from:0 to: histo_cat_number-1{
			histo[i] <- histo[i]/length(people);
		}
	}	
}


species gouvernment {
	string strategy <- "random";
	action promote_environment {
		switch strategy {
			match "random" {
				ask nbPromotion among people {
					pro_environment <- pro_environment + rnd(1.0); 
				}	
			}
			match "pro" {
				ask nbPromotion first (people sort_by - each.pro_environment) {
					pro_environment <- pro_environment + 0.25; 
				}	
			}
			match "cons" {
				ask nbPromotion first (people sort_by each.pro_environment) {
					pro_environment <- pro_environment + 0.25; 
				}	
			}
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
	bool goto_work <- false;
	float pro_environment <- rnd(1.0) min:0.0 max: 1.0;
	parking parking_location <- nil;
	float speed <- 3#km/#hour;
	bool use_car <- false;
	float pollution_scale <- 1.0;
	
	reflex wander when:!teleTransportation{
		do wander on:road_network speed: speed;
	}
	
	reflex pollute{
		cell tmp <- cell at self.location;
		if tmp != nil{
			tmp.level <- tmp.level+pollution_scale*(use_car?1:0.1);
			ask tmp.neighbours
			{
				level <- level+myself.pollution_scale*(myself.use_car?0.4:0.01);
			}  
		}
	}

	
	reflex changing_mode when: flip(change_probability) and !teleTransportation {
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
	

	
	reflex go_to_work when:not goto_work and (current_date.hour = working_hour) and teleTransportation{
		goto_work <- true;
		location <- any_location_in(working_place);
	}	
	
	
	reflex go_to_home when: goto_work and (current_date.hour = going_home_hour) and teleTransportation{
		goto_work <- false;
		location <- any_location_in(my_home);
	}
	
	
	reflex influence_by_other when: flip(0.1) {
		list<people> neighbors <- people at_distance interactionDistance;
		if(length(neighbors)>0){
			pro_environment <- pro_environment + (influence_factor * ((neighbors mean_of each.pro_environment) - pro_environment)) -0.01;
		}
		
	}
	
	
	aspect default {
		float val <- 255 * pro_environment;
		if use_car{
			draw square(5.0) color: rgb(255 - val, val,0.0); 
		}else{
			draw circle(3.0) color: rgb(255 - val, val,0.0);
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
	string status <- "idle"; //among "buying", "selling", "finished_buying", "finished_selling","idle"
	float energyPrice;
	float price;
	bool produce_electricty <- false;
	list<people> inhabitants;
	
	reflex pollute{
		float pollution <- sum(inhabitants collect (1 - each.pro_environment));
		list<cell> cells <- cell overlapping self;
		loop tmp over: cells{
			tmp.level <- tmp.level+pollution;
			ask tmp.neighbours
			{
				level <- level+pollution/10;
			}  
		}
	
	}

	action choose_produce_electricty {	
		produce_electricty <- (inhabitants mean_of (each.pro_environment)) > 0.5;
	}
	action calculate_consumption {
		consumption<-float(consumption_matrix[current_date.hour,class_map[usage+scale]])*((1/2)*(1+sqrt(average_surface/(area+1))))*(area*nbFloors);
	}
	
	action calculate_production {
		if (produce_electricty) {
			production<-float(production_matrix[current_date.hour,1])*((1/2)*(1+sqrt(average_surface/(area+1))))*area;	
		} else {
			production <- 0.0;
		}
		
	}
	
	action update_status{
		status <- "idle";
		if(production - consumption>0){
			status<- "selling";
		}
		if(production - consumption<0){
			status<-"buying";
		}
	}
	
	
	aspect is_producer {
		draw shape color:(produce_electricty ? #green : #red);
	}
	
	
}

species road {
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


grid cell width: 100 height: 50 {
	float level <- 0.0;
	list neighbours of: cell <- (self neighbors_at 1) of_species cell;  
	rgb color <- rgb(rnd(255),rnd(255),rnd(255));   
	
	reflex update_color when:heatmap{
		float level2 <-(min([1,max([0,level/25])]))^(0.5);
		float tmp <- level2*(length(heat_map)-1);
		color <- rgb(heat_map[int(tmp)]);
	}
	
	aspect default{
		if(heatmap){
		  draw shape color:color;	
		}
	}
}

experiment start type: gui {
	output{
		display view1  type:opengl  {	
			species building aspect:is_producer;
			species parking;	
			species road;
			species people;
			species pev;
			species cell transparency: 0.5;
		}
		display chartprod
		{
			chart 'prod' axes:rgb(125,125,125) size:{0.5,0.5} type:histogram style:stack //white
			{
				data 'production' value:sum(building collect each.production) accumulate_values:true color:rgb(169,25,37) marker:false thickness:2.0; //red
				data 'consumption' value:-sum(building collect each.consumption)  accumulate_values:true color:rgb(71,168,243) marker:false thickness:2.0; //blue
			}
			
			chart 'building producing' axes:rgb(125,125,125) size:{0.5,0.5} position:{0.0,0.5}
			{
				data 'nb building producing' value:building count (each.produce_electricty) color: #green marker:false thickness:2.0;  //red
				data 'nb building not producing' value:building count not(each.produce_electricty) color: #red marker:false thickness:2.0;  //red
		
			}
			
			chart 'people opinion' axes:rgb(125,125,125) size:{0.25,0.25} type:histogram style:stack position:{0.5,0.0}//white
			{
				data 'pro environment > 0.5' value:length(people where (each.pro_environment>0.5)) accumulate_values:true color:rgb(169,25,37) marker:false thickness:2.0; //red
			}
			
			chart 'opinion distribution' axes:rgb(125,125,125) size:{0.25,0.25}  position:{0.75,0.0} y_range: [0,1.0] type:histogram style:stack //white
			{
				data 'pro environment > 0.5' value: histo  color:rgb(169,25,37)   marker:false thickness:2.0; //red
			}
			chart 'green building' axes:rgb(125,125,125) size:{0.5,0.5} type:histogram style:stack position:{0.5,0.5}//white
			{
				data 'green building ' value:length(building where (each.produce_electricty=true)) accumulate_values:true color:#green marker:false thickness:2.0; //red
			}
			
		}
	}
}


experiment multi_sim_strategy type: gui {
	init {
		create simulation with: [P_strategy::"cons"];
		create simulation with: [P_strategy::"pro"];
	}
	output {
		
		
		display view1  type:opengl  {	
			species building aspect:is_producer;	
			species road;
			species people;
			species  cell transparency: 0.5;// lines: #white 
		}	
	}
}





