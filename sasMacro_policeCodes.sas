/* STA 402 Term Project CLEAN MACRO */

/* 
Arguments for crimes_macro(crime=, folder=):

crime=
One of the following aggregated categories of crime:
	Violent_Behavior - General voilent or threatening behavior. Including possession 
						of a weapon (without using it), fighting, aggression, etc.
	Noise - Noise complatint for dogs barking, loud alarms, people yelling, etc.
	Serious_Violent_Crime - Felonies including arson, shootings, robberies, etc.
	Vehicle - Anything involving a car, bicycle, or other vehicle. This includes 
			   traffic stops, car chases, etc.
	Drugs/Alcohol - Charges related to the consumption or posession of drugs, 
					 alchohol, or other illegal substances.
	Civil - A civil dispute or disturbance in the community. Landlord disputes, 
			general complaints, stolen packages, mental wellbeing checks, etc.
	Nonviolent_Crime - A less serious crime including panhandling, vandalism,
						petty theft, solicitation, etc.
	Juvenile - Any crime either involving a child such as kidnapping, or where a 
				juvinille is the offender.
	Existing/Assisting - Reports regarding already existing cases such as a restraining 
						   order or parole violation or assisting other poliece forces
						   such as the UCPD or the fire department.


folder=
The filpath to an existing folder on your machine. The folder should have the 
file 'Police_Department_Calls_for_Service.csv' already in it. Only put the filepath, 
do not put any quotes around the folder name. A RTF file will be output with the
results from the macro and saved in the user specified folder.


A few examples:
%crimes_macro(crime=Juvenile, folder=M:\STA402\Homework\Project);
%crimes_macro(crime=Drugs/Alcohol, folder=M:\STA402\Homework\Project);
%crimes_macro(crime=Nonviolent_Crime, folder=M:\STA402\Homework\Project);
*/

%macro crimes_macro(crime=, folder=);
/* Create SAS library */
	libname proj "&folder";

/* DATA-step: Read poliece calls data */
	data proj.calls;
		infile "&folder\Police_Department_Calls_for_Service.csv"
			firstobs = 2
			dsd;
		/* Adding formatting to help read in data properly */
		input crime_id crime_type :$32. report_date :mmddyy10. call_date :mmddyy10. offense_date :mmddyy10. call_time :time.
			  call_date_time :mdyampm19. disposition :$3. address :$50. city :$20. state :$2. agency_id address_type :$15.
			  common_location :$15.
			  ;
		format report_date mmddyy10. call_date mmddyy10. offense_date mmddyy10. call_time time.; 

		/* Extract day of the week from date, extracted as number, converted to word */
		Day = put(call_date, downame3.);

		/* Extract year from date */
		Year = year(call_date);

		/* String processing */
		crime_type = lowcase(crime_type);  * everything to lowercase;
		crime_type = compress(crime_type);

		/* Don't want all vars */
		keep crime_type Day Year;
	run;

/* PROC SORT: Order by crime type */
	proc sort data=proj.calls;
		by crime_type;
	run;

/* DATA-step: Pull out and catagorize significant codes

		This DATA step creates multiple arrays - one for each category of crime.
		The arrays are then looped through with a DO loop. If an observation in 
		the datacontains a substring of a given array, the observation is replaced 
		with the respective category of crime. 

*/
	data proj.small_crimes;
		set proj.calls;

		/* Set up all arrays first */

		/* Array for violence */
		array violence_arr [45] $ ('abuse', 'assault', 'atrisk', 'attem', 'bbgun', 'bleed', 'bullet', 
									'cut', 'elderly', 'dw', 'dv', 'dog', 'gang', 'gun', 'harass', 'hammer', 
									'heated', 'injur', 'kick', 'knife', 'machet', 'pepper', 'pistol', 'bitbull', 
									'punch', 'razor', 'scis', 'taser', 'weapon', 'voilent', 'brandish', 'broken', 
									'crowbar', 'escal', 'fight', 'hatche', 'hostile', 'agit', 'aggress', '410', 
									'519', '520', '532', '595', '917'
		);
		/* Array for noise */
		array noise_arr [33] $ ('amp', 'bang', 'boom', 'drum', 'const', 'honk', 'loud', 'music', 'noise', 
								'party', 'radio', 'rant', 'rap', 'rave', 'rowdy', 'sax', 'scream', 'shout', 
								'sing', 'siren', 'stereo', 'talk', 'trumpet', 'whistl', 'yell', 'crying', 
								'const', 'alarm', 'ringingdoorbell', '405', '905', '916', '417'
		);
		/* Array for serious violent crimes */
		array sv_crimes_arr [22] $ ('arson', 'burglery', 'death', 'felony', 'hatecrime', 'holdup', 'shot', 
									'honeenv', 'homein', 'strongarmrobbery', '187', '211', '217', '240', '245', 
									'261', '368', '487', '535', '529', '530', '1000'
		);
		/* Array for vehicle crimes */
		array vehicle_arr [57] $ ('auto', 'bicycle', 'bike', 'camper', 'car', 'chp', 'crosswalk', 'driv', 
								  'hitandrun', 'kar', 'licenceplate', 'lyft', 'motor', 'obstruct', 'reck', 
								  'redzone', 'revv', 'roadrage', 'skateboard', 'taxi', 'tire', 'traf', 
								  'truck', 'uber', 'veh', 'facingthew', 'facingw', 'curb', 'caltrain', 'cab', 
								  'bicy', 'eng', 'park', 'racing', 'scooter', 'yz', '600', '601', '602', '603', 
								  '799', '800', '801', '802', '805', '807', '809', '819', '914', '22500', '22502',
								  '22507', '2250', '2251', '22550', '22651', 'grandtheft'
		);
		/* Array for drugs and alcohol */
		array drug_alc_arr [24] $ ('beliger', 'brew', 'crack', 'drug', 'deal', 'drink', 'fentanyl', 'heroin', 
									'meth', 'overdose', 'needle', 'tweak', 'bottle', 'user', 'beer', 'brisbane', 
									'hallcinant', 'pipe', 'poss', '211', '420', '527', '528', '806'
		);
		/* Array for civil disturbance or disbutes */
		array civil_arr [73] $ ('block', 'blk', 'credit', 'damage', 'defact', 'debris', 'electric', 'doorway', 'dine', 
								'disable', 'dispu', 'fail', 'face', 'garage', 'hack', 'has', 'hydrant', 'jump', 'lang', 
								'live', 'medical', 'package', 'opport', 'passport', 'physic', 'peigon', 'postal', 'psych', 
								'recycle', 'roomat', 'secure', 'security', 'sleeper', 'static', 'has', 'tarasof', 'tres', 
								'water', 'wellbeing', 'window', 'bbq', 'climbing', 'civil', 'complaint', 'comput', 'covid', 
								'famil', 'fire', 'gas', 'grab', 'mentallydisturbed', 'muniinspection','100', '400', '407', 
								'408', '409', '415', '518', '594', '902', '904', '909', '912', '913', '51-50', '1030', '10-30', 
								'500', '5150', '647', '915', '418'
		);
		/* Array for nonviolent crimes */
		array nonvio_crimes_arr [69] $ ('cw', 'egg', 'dump', 'embez', 'encamp', 'evict', 'expos', 'follow', 'fraud', 'fh', 
										'fw', 'gambl', 'graffiti', 'identity', 'idling', 'illegalvend', 'susp', 'loadin', 
										'panh', 'loiter', 'missing', 'naked', 'panhandl', 'handl', 'harass', 'ped', 'peep', 
										'pickpocket', 'pkg', 'purse', 'racial', 'racist', 'scam', 'squat', 'steal', 'tamper', 
										'threat', 'tipjar', 'urinat', 'vandl', 'burg', 'encamp', 'feces', 'grop', 'solicit',
										'pettytheft', 'prostitute/solicite', 'vandal', '152', '212', '219', '222', '311', '404', 
										'406', '416', '419', '421', '459', '470', '488', '585', '586', '587', '588', '596', '646B', 
										'650', 'stolenproperty'
		);
		/* Array for crimes involving children */
		array child_arr [15] $ ('child', 'cps', 'custody', 'kidnap', 'runaway', 'juv', 'nucb', 'j/o', '207', '216', 
								'288', 'jo', 'boy', 'girl', 'baby'
		);
		/* Array for existing suspect or assisting other departments */
		array exist_assist_arr [37] $ ('dcpd', 'federal', 'sanmateo', 'state', 'ucpd', 'ucfs', '811', '851', '910', '911', '919', 
										'646', '852', '852', '901', '9-1-1', '920', 'awol', 'case', 'casing', 'cell', 'city', 'cj', 
										'cold', 'collect', 'epo', 'eval', 'vict', 'meetw/citizen', 'arrestmade', 'prisonertransport', 
										'sw', 'ygc/jailbreak','148', '213', '496', '918'
		);

		/* DO loops to replace obs with crime categories */
		/* DO loop for voilence: change the entry if the ob has any substring in array */
		do i = 1 to dim(violence_arr);
		        if index(crime_type, trim(violence_arr[i])) > 0 then do;
		           crime_type = 'Violent_Behavior'; 
				end;
		    end;
		/* DO loop for noise: change the entry if the ob has any substring in array */
		do i = 1 to dim(noise_arr);
		        if index(crime_type, trim(noise_arr[i])) > 0 then do;
		           crime_type = 'Noise'; 
				end;
		    end;
		/* DO loop for serious violent crimes: change the entry if the ob has any substring in array */
		do i = 1 to dim(sv_crimes_arr);
		        if index(crime_type, trim(sv_crimes_arr[i])) > 0 then do;
		           crime_type = 'Serious_Violent_Crime'; 
				end;
		    end;
		/* DO loop for vehicle: change the entry if the ob has any substring in array */
		do i = 1 to dim(vehicle_arr);
		        if index(crime_type, trim(vehicle_arr[i])) > 0 then do;
		           crime_type = 'Vehicle'; 
				end;
		    end;
		/* DO loop for drugs and alcohol: change the entry if the ob has any substring in array */
		do i = 1 to dim(drug_alc_arr);
		        if index(crime_type, trim(drug_alc_arr[i])) > 0 then do;
		           crime_type = 'Drugs/Alcohol'; 
				end;
		    end;
		/* DO loop for civil disturbance or disputes: change the entry if the ob has any substring in array */
		do i = 1 to dim(civil_arr);
		        if index(crime_type, trim(civil_arr[i])) > 0 then do;
		           crime_type = 'Civil'; 
				end;
		    end;
		/* DO loop for nonvoilent crimes: change the entry if the ob has any substring in array */
		do i = 1 to dim(nonvio_crimes_arr);
		        if index(crime_type, trim(nonvio_crimes_arr[i])) > 0 then do;
		           crime_type = 'Nonvoilent_Crime'; 
				end;
		    end;
		/* DO loop for crimes involving children: change the entry if the ob has any substring in array */
		do i = 1 to dim(child_arr);
		        if index(crime_type, trim(child_arr[i])) > 0 then do;
		           crime_type = 'Juvenile'; 
				end;
		    end;
		/* DO loop for other existing or assisting: change the entry if the ob has any substring in array */
		do i = 1 to dim(exist_assist_arr);
		        if index(crime_type, trim(exist_assist_arr[i])) > 0 then do;
		           crime_type = 'Existing/Assisting'; 
				end;
		    end;
	
	/* Only keep the crime category, day, and year */
		keep crime_type Day Year;
	run;

/* Free up space */
	proc delete data=proj.calls; run;

/* DATA-step: Filter for category of crime the user inputs*/
	data proj.crimes_filtered;
		set proj.small_crimes; /* categorized df */
		if crime_type = "&crime"; /* macro varibale */
	run;

/* PROC DELETE: Free up space */
	proc delete data=proj.small_crimes;
	run;

/* Output to RTF */
	ods rtf bodytitle file="&folder/Castel_project.rtf"; /* macro folder location */

/* PROC FREQ: Data in correct format to graph */
	title 'Frequency by weekday and year';
	proc freq data=proj.crimes_filtered noprint; /* don't print this */
		tables day * year / out=proj.crimes_summary; /* new df */
	run;

/* PROC SGPLOT: Bar chart showing distribution by weekday and year */
	title 'Frequency by weekday and year'; 
	proc sgplot data=proj.crimes_summary;
		vbar day / group=year groupdisplay=cluster response=count;
	run;

/* Show frequency of crime by year */ 
	title 'Frequency by year';
	proc freq data=proj.crimes_filtered;
		tables year / nocum nopercent norow nocol; * only show counts by year;
	run;

/* Show frequency of crime by day of the week */
	ods rtf startpage=no; /* stay on the same page */
	title 'Frequency by weekday';
	proc freq data=proj.crimes_filtered;
  		tables day / nocum nopercent norow nocol; * only show counts by day;
	run;

/* Close RTF */
	ods rtf close;

%mend;


