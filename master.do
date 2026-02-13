//	AUTHOR:			GIUSEPPE FAÈ
//	CREATED:		APRIL 2, 2025
//	MODIFIED: 		
//	DESCRIPTION:	THIS FILE DEFINES ALL GLOBAL MACROS

// Set Stata version
version 18.0

// Clear memory
clear all

// Font type
graph set window fontface "Times New Roman"

********************************************************************************
// SET GLOBAL MACROS
********************************************************************************
global root       	 /Users/Ueppe/Desktop/PLD
global data			 ${root}/data
global data_raw      ${data}/raw
global funke 		 ${data_raw}/funke
global un			 ${data_raw}/un
global data_clean 	 ${data}/clean
global codes		 ${root}/codes
global cleaning		 ${codes}/cleaning
global analysis      ${codes}/analysis
global output 		 ${root}/output
global figures		 ${output}/figures
global tables		 ${output}/tables
cd ${root}

********************************************************************************
// PERFORM CODES
********************************************************************************
do ${cleaning}/dataprep.do
do ${analysis}/preliminaries.do
do ${analysis}/LP_fert.do
do ${analysis}/LP_old.do
do ${analysis}/LP_mig.do
do ${analysis}/IPWRA-LPs_fert.do
do ${analysis}/IPWRA-LPs_old.do
do ${analysis}/IPWRA-LPs_mig.do
do ${analysis}/scm_fert.do
do ${analysis}/scm_old.do
do ${analysis}/scm_mig.do
