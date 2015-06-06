#!/usr/bin/python

from ua_parser import user_agent_parser
import sys

archivo = sys.argv[1]

with open(archivo) as f:

    for line in f:

        result_dict = user_agent_parser.Parse(line)

	ua = result_dict['user_agent']['family']
	os = result_dict['os']['family']
	dev = result_dict['device']['family']
	
	result_string = ""

	if ua == "Other":
		result_string += "NA"
	else:
		result_string += ua

	if os == "Other":
		result_string += "," + "NA"
	else:
		result_string += "," + os

	if dev == "Other":
		result_string += "," +  "NA"
	else:
		result_string += "," +  dev
	
	result_file = open("ua_mod.csv", "a")
	result_file.write(result_string + "\n")
